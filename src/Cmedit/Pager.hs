-- | The paged read-only view: how to look at a file that is too big to hold.
--
-- A file over 'Cmedit.EditorState.maxOpenBytes' cannot be decoded into a
-- 'Cmedit.TextBuffer.Buffer' — millions of lines of 'Text' would dwarf the
-- machine — so the editor used to refuse it outright. The common case behind
-- that refusal is benign: a log, a database dump, a CSV export. The user wants
-- to *look*, not to edit.
--
-- This module is the pure half of doing that with memory that does not depend
-- on the file's size:
--
--   * a **sparse index** of byte offsets, one entry every 'pgStride' lines
--     (built once by a streaming pass — 'buildPagerIndex'), so
--     a 2 GB file costs a few tens of KB of index;
--   * a **window** of decoded lines around the viewport, refilled from disk as
--     it scrolls and never larger than a few screens.
--
-- Everything above the file-access section at the bottom is pure; the two IO
-- functions ('buildPagerIndex', 'readPagerWindow') live here rather than in the
-- driver because they own the on-disk format the index describes, exactly as
-- "Cmedit.TextBuffer" owns load\/save. The driver calls them and hands the
-- result back through 'pagerFilled'.
module Cmedit.Pager
  ( PagerDoc(..)
  , mkPagerDoc
  , pagerStride
  , maxPagerLine
    -- * Reading the view
  , pagerLine
  , pagerHasLine
  , pagerNeedsFill
  , pagerFilled
  , offsetOfLine
    -- * Movement (all clamped; the view is read-only)
  , pagerScroll
  , pagerMoveTo
  , pagerMoveBy
  , pagerTop
  , pagerBottom
  , pagerEnsureVisible
    -- * Presentation
  , pagerStatus
  , humanBytes
    -- * File access (the only IO here; mirrors TextBuffer's load\/save)
  , buildPagerIndex
  , readPagerWindow
  ) where

import Data.Array.Unboxed (UArray, listArray, bounds, (!))
import Data.Int (Int64)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

import Control.Exception (SomeException, handle)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.IO (IOMode(ReadMode), SeekMode(AbsoluteSeek), hIsEOF, hSeek, withBinaryFile)

import Cmedit.TextBuffer (Encoding(..), LineEnding(..), stripBom)

-- | Lines between index entries. 1000 keeps the index tiny (a 2 GB, 20-million
-- line file indexes in ~160 KB) while bounding the forward scan needed to reach
-- any line to at most 1000 lines from the nearest entry.
pagerStride :: Int
pagerStride = 1000

-- | Longest line the paged view will materialise, in bytes.
--
-- Without this the view is bounded per *window* but not per *line*, and a file
-- that is one enormous line — a minified bundle, a base64 blob, a database dump
-- with no newlines — puts all of it in memory at once: a 120 MB single-line
-- file measured 255 MB resident, which is precisely what this view exists to
-- avoid. Only the first 'maxPagerLine' bytes of such a line are shown; the
-- terminal can display a few hundred columns of it in any case.
maxPagerLine :: Int
maxPagerLine = 64 * 1024

-- | A file being viewed page-by-page. Read-only by construction: there is no
-- buffer to edit, and nothing here can produce one.
data PagerDoc = PagerDoc
  { pgPath    :: !FilePath
  -- ^ NB: 'pgEol' is not decoration — it selects the byte the index and the
  -- window reader split on, so a classic CR-only file is paged correctly
  -- rather than appearing as one enormous line.
  , pgSize    :: !Integer            -- ^ File size in bytes, for the status line.
  , pgLineCount :: !Int              -- ^ Total lines, counted by the index pass.
  , pgIndex   :: !(UArray Int Int64) -- ^ Byte offset of line @i * pagerStride@.
  , pgEol     :: !LineEnding
  , pgEnc     :: !Encoding
  , pgTop     :: !Int                -- ^ First visible line (0-based).
  , pgCursor  :: !Int                -- ^ Cursor line; the view has no column.
  , pgLeft    :: !Int                -- ^ Horizontal scroll, in display columns.
  , pgWinFrom :: !Int                -- ^ First line held in 'pgWindow'.
  , pgWindow  :: !(Seq Text)         -- ^ Decoded lines starting at 'pgWinFrom'.
  } deriving (Show)

-- | Build the view from a completed index pass.
mkPagerDoc :: FilePath -> Integer -> Int -> [Int64] -> LineEnding -> Encoding -> PagerDoc
mkPagerDoc path size nLines offsets eol enc = PagerDoc
  { pgPath = path
  , pgSize = size
  , pgLineCount = max 1 nLines
  , pgIndex = listArray (0, max 0 (length offsets - 1)) (if null offsets then [0] else offsets)
  , pgEol = eol
  , pgEnc = enc
  , pgTop = 0
  , pgCursor = 0
  , pgLeft = 0
  , pgWinFrom = 0
  , pgWindow = Seq.empty
  }

-- | Byte offset to start reading from to reach line @n@: the nearest index
-- entry at or before it, plus how many lines must be skipped from there.
offsetOfLine :: PagerDoc -> Int -> (Int64, Int)
offsetOfLine pg n =
  let i = max 0 (min hi (n `div` pagerStride))
      (_, hi) = bounds (pgIndex pg)
  in (pgIndex pg ! i, n - i * pagerStride)

-- | A line of the file, or 'Nothing' when it is outside the loaded window.
pagerLine :: PagerDoc -> Int -> Maybe Text
pagerLine pg n
  | n < pgWinFrom pg = Nothing
  | otherwise        = Seq.lookup (n - pgWinFrom pg) (pgWindow pg)

pagerHasLine :: PagerDoc -> Int -> Bool
pagerHasLine pg n = maybe False (const True) (pagerLine pg n)

-- | Does the window need refilling to show @height@ lines from the top, and if
-- so from which line and how many?
--
-- The requested range is padded by a screen on each side and snapped to the
-- index stride, so scrolling a line at a time does not re-read constantly, and
-- so a refill always starts where a seek can land exactly.
pagerNeedsFill :: Int -> PagerDoc -> Maybe (Int, Int)
pagerNeedsFill height pg
  | covered  = Nothing
  | otherwise = Just (from, count)
  where
    h        = max 1 height
    lastWant = min (pgLineCount pg - 1) (pgTop pg + h - 1)
    covered  = pagerHasLine pg (pgTop pg)
                 && (lastWant <= pgWinFrom pg + Seq.length (pgWindow pg) - 1)
    padded   = max 0 (pgTop pg - h)
    from     = (padded `div` pagerStride) * pagerStride
    count    = min (pgLineCount pg - from) (3 * h + 2 * pagerStride)

-- | Install a freshly read window (driver callback).
pagerFilled :: Int -> Seq Text -> PagerDoc -> PagerDoc
pagerFilled from lns pg = pg { pgWinFrom = from, pgWindow = lns }

------------------------------------------------------------------------------
-- Movement. Everything clamps to the file; there is no cursor column, because
-- there is no editing — the cursor is a highlighted line.

clampLine :: PagerDoc -> Int -> Int
clampLine pg n = max 0 (min (pgLineCount pg - 1) n)

-- | Scroll the viewport without moving the cursor line off screen.
pagerScroll :: Int -> Int -> PagerDoc -> PagerDoc
pagerScroll height delta pg =
  let top' = max 0 (min (max 0 (pgLineCount pg - 1)) (pgTop pg + delta))
      cur' = max top' (min (top' + max 1 height - 1) (pgCursor pg))
  in pg { pgTop = top', pgCursor = clampLine pg cur' }

-- | Move the cursor to a line, scrolling to keep it visible.
pagerMoveTo :: Int -> Int -> PagerDoc -> PagerDoc
pagerMoveTo height n pg = pagerEnsureVisible height pg { pgCursor = clampLine pg n }

pagerMoveBy :: Int -> Int -> PagerDoc -> PagerDoc
pagerMoveBy height d pg = pagerMoveTo height (pgCursor pg + d) pg

pagerTop :: Int -> PagerDoc -> PagerDoc
pagerTop height = pagerMoveTo height 0

pagerBottom :: Int -> PagerDoc -> PagerDoc
pagerBottom height pg = pagerMoveTo height (pgLineCount pg - 1) pg

pagerEnsureVisible :: Int -> PagerDoc -> PagerDoc
pagerEnsureVisible height pg =
  let h = max 1 height
      c = pgCursor pg
      top | c < pgTop pg           = c
          | c >= pgTop pg + h      = c - h + 1
          | otherwise              = pgTop pg
  in pg { pgTop = max 0 top }

------------------------------------------------------------------------------

-- | The right-hand status text for the paged view.
pagerStatus :: PagerDoc -> String
pagerStatus pg =
  "Ln " ++ show (pgCursor pg + 1) ++ " of " ++ show (pgLineCount pg)
    ++ "   " ++ humanBytes (pgSize pg) ++ "   PAGED "

-- | A compact byte size (the same style the open-refusal message used).
humanBytes :: Integer -> String
humanBytes n
  | n >= 1024 * 1024 * 1024 = showUnit (1024 * 1024 * 1024) "GB"
  | n >= 1024 * 1024        = showUnit (1024 * 1024) "MB"
  | n >= 1024               = showUnit 1024 "KB"
  | otherwise               = show n ++ " B"
  where
    showUnit u name =
      let whole = n `div` u
          tenth = (n * 10 `div` u) `mod` 10
      in show whole ++ (if whole < 10 then "." ++ show tenth else "") ++ " " ++ name

------------------------------------------------------------------------------
-- File access

-- | One streaming pass over a huge file: count its lines and record the byte
-- offset of every 'pagerStride'-th one.
--
-- Reads in fixed blocks and never holds more than one, so a 2 GB file costs a
-- 64 KiB buffer and an index of a few tens of KB — the whole point of the paged
-- view. Line endings and the BOM are sniffed from the first block, exactly as
-- 'loadFromBytes' would.
buildPagerIndex :: FilePath -> Integer -> IO (Either String PagerDoc)
buildPagerIndex path size = handle onErr $ withBinaryFile path ReadMode $ \h -> do
  first <- BS.hGet h chunkSize
  let (enc, _) = stripBom first
      bomLen   = if enc == Utf8Bom then 3 else 0
      eol | BS.isInfixOf (BS.pack [13, 10]) first = CRLF
          | BS.elem 13 first                      = CR
          | otherwise                             = LF
      -- Offsets are of the *start* of each indexed line, so a reader can seek
      -- straight to one; line 0 starts after any BOM.
      sepByte = if eol == CR then 13 else 10   -- CRLF still ends with LF
      go !bs !base !nl !acc !next
        | BS.null bs = pure (nl, acc, next)
        | otherwise = case BS.elemIndex sepByte bs of
            Nothing -> pure (nl, acc, next)
            Just i ->
              let lineNo = nl + 1
                  off    = base + fromIntegral i + 1
                  (acc', next') = if lineNo == next
                                    then (off : acc, next + pagerStride)
                                    else (acc, next)
              in go (BS.drop (i + 1) bs) off lineNo acc' next'
      -- The carry between blocks is the current partial line. A file with no
      -- newlines would otherwise grow it to the whole file; cap it, since the
      -- offsets are all that matter here (the window reader re-reads content).
      loop !bs0 !base !nl !acc !next = do
        let bs = if BS.length bs0 > 2 * chunkSize then BS.drop (BS.length bs0 - chunkSize) bs0 else bs0
        (nl', acc', next') <- go bs base nl acc next
        -- Carry the unterminated tail forward with its absolute offset.
        let consumed = case BS.elemIndexEnd sepByte bs of
                         Just j  -> j + 1
                         Nothing -> 0
            tailLen  = BS.length bs - consumed
            base'    = base + fromIntegral consumed
        more <- BS.hGet h chunkSize
        if BS.null more
          then pure (nl' + (if tailLen > 0 then 1 else 0), reverse acc')
          else loop (BS.drop consumed bs <> more) base' nl' acc' next'
  (nLines, offs) <- loop (BS.drop bomLen first) (fromIntegral bomLen) 0
                         [fromIntegral bomLen] pagerStride
  pure (Right (mkPagerDoc path size (max 1 nLines) offs eol enc))
  where
    chunkSize = 64 * 1024
    onErr :: SomeException -> IO (Either String PagerDoc)
    onErr e = pure (Left (show e))

-- | Read @count@ lines starting at line @from@ of a paged file, using the
-- sparse index to seek. Splits on the file's own record separator ('pgEol').
--
-- Bounded in three ways, all of them necessary: at most a stride of lines is
-- skipped from the nearest index entry; at most @count@ lines are decoded; and
-- each line is capped at 'maxPagerLine' bytes. The last one is not a nicety —
-- a file with no separators at all (one 120 MB line) otherwise makes the reader
-- concatenate the entire file, and it stops early once the final line it needs
-- has reached the cap, so such a file costs one block read rather than a scan.
readPagerWindow :: PagerDoc -> Int -> Int -> IO (Seq Text)
readPagerWindow pg from count = handle onErr $
  withBinaryFile (pgPath pg) ReadMode $ \h -> do
    let sepByte = if pgEol pg == CR then 13 else 10
        (off, skip) = offsetOfLine pg from
        need = skip + max 0 count
    hSeek h AbsoluteSeek (fromIntegral off)
    -- Splitting on the separator byte is safe for UTF-8: neither 0x0A nor 0x0D
    -- occurs inside a multi-byte sequence, so no character is ever cut in half.
    let readMore !acc !n !cur
          | n >= need = pure (reverse acc)
          -- The line we are on is the last one wanted and is already at the
          -- cap: nothing further can change what is displayed.
          | n + 1 >= need && BS.length cur >= maxPagerLine = pure (reverse (cur : acc))
          | otherwise = do
              bs <- BS.hGet h blockSize
              if BS.null bs
                then pure (reverse (if BS.null cur && not (null acc) then acc else cur : acc))
                else consume acc n cur bs
        consume !acc !n !cur bs = case BS.elemIndex sepByte bs of
          Nothing ->
            let cur' = if BS.length cur >= maxPagerLine
                         then cur
                         else BS.take maxPagerLine (cur <> bs)
            in readMore acc n cur'
          Just i ->
            let line = BS.take maxPagerLine (cur <> BS.take i bs)
                acc' = line : acc
                n'   = n + 1
            in if n' >= need then pure (reverse acc')
                             else consume acc' n' BS.empty (BS.drop (i + 1) bs)
    raw <- readMore [] 0 BS.empty
    let decoded = map (TE.decodeUtf8With TEE.lenientDecode . stripCR) (drop skip raw)
        -- An empty file is one empty line, matching how the buffer loader sees
        -- it (and what 'pgLineCount' reports), so the view is never blank in a
        -- way that looks like a failed read.
        out = take count decoded
    pure (Seq.fromList (if null out && from == 0 then [T.empty] else out))
  where
    blockSize = 64 * 1024
    stripCR b = if not (BS.null b) && BS.last b == 13 then BS.init b else b
    onErr :: SomeException -> IO (Seq Text)
    onErr _ = pure Seq.empty
