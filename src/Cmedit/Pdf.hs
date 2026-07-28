{-# LANGUAGE BangPatterns #-}
-- | The PDF reading view: a page-description file shown as a document.
--
-- PDF is binary, so unlike the RTF view ("Cmedit.Rtf") this is not a
-- projection of a text buffer that Alt+T can toggle away from — there is no
-- buffer at all. It is structured like the image view instead: sniffed by
-- magic bytes, decoded from raw bytes into a read-only per-document model,
-- and never written back. Nothing here touches IO.
--
-- Three things make a text view of PDF tractable without a rasteriser:
--
--   * __Objects can be found without the cross-reference table.__ Every
--     indirect object in the file is introduced by @N G obj@, so a single
--     scan recovers the object graph. That is deliberately not "parse
--     @startxref@ and follow the chain": broken, patched and
--     linearised-then-appended files are common in the wild, and the scan
--     reads them all. Incremental updates fall out for free — a later
--     definition of an object number wins, which is exactly the semantics an
--     appended update has. Objects hidden inside compressed object streams
--     (@\/ObjStm@, universal in PDF 1.5+) are the one thing a scan cannot
--     see, so those are expanded afterwards, ordered by the same rule.
--
--   * __The content stream is a tiny stack machine.__ Of its ~70 operators
--     only the text ones and the two matrix ones carry text position, so the
--     interpreter here is small; everything else is skipped by arity-free
--     operand clearing, the same way an RTF reader skips markup it does not
--     model.
--
--   * __Reading order is recoverable, but only heuristically.__ A PDF has no
--     paragraphs, no lines and no reading order — just glyphs at coordinates.
--     'assemblePage' puts them back: columns from a vertical gap in the
--     x-occupancy, lines from y-clustering, spaces from the gap between one
--     glyph run's end and the next one's start measured in ems, and
--     paragraphs from lines that reach the right margin under a consistent
--     leading. These are judgement calls, and they are all in one place so
--     they can be tuned against real files.
--
-- What is deliberately absent: encrypted files (reported, not decrypted),
-- embedded images, vector graphics and any attempt at page fidelity. The
-- view reflows to the terminal width, which is the point — it is for reading
-- a document, not for reproducing its typesetting.
module Cmedit.Pdf
  ( -- * The document model
    PdfDoc(..)
  , PdfPage(..)
  , PdfPar(..)
  , PdfSpan(..)
  , PdfFmt(..)
  , PdfKind(..)
  , PdfAlign(..)
  , defaultPdfFmt
    -- * Parsing
  , parsePdf
  , sniffPdf
  , maxPdfBytes
  , maxPdfPages
    -- * The view
  , pdfRelayout
  , pdfLines
  , pdfLineCount
  , pdfPageCount
  , pdfPageLine
    -- * Laid-out lines (what the renderer draws)
  , PdfLine(..)
  , layoutPdf
    -- * Selection (read-only: it exists to be copied, never edited)
  , pdfSelection
  , pdfSelText
  , pdfSelectRange
  , pdfSelectAll
  , pdfClearSel
  , pdfSetCaret
  , pdfExtendTo
  , pdfLineText
  , pdfClampPos
  , pdfPosAtCell
  , pdfCellOfPos
  , pdfWordRange
  , pdfLineRange
  , pdfCaretLeft
  , pdfCaretRight
  , pdfCaretUp
  , pdfCaretDown
  , pdfCaretHome
  , pdfCaretEnd
  , pdfCaretTop
  , pdfCaretBottom
  , pdfScrollToCaret
    -- * Movement (all clamped; the view is read-only)
  , pdfScroll
  , pdfGoTop
  , pdfGoBottom
  , pdfClamp
  , pdfGoToPage
  , pdfNextPage
  , pdfPrevPage
  , pdfCurrentPage
    -- * Presentation
  , pdfStatus
  , pdfPlainText
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, isDigit, isHexDigit, digitToInt, ord, toLower)
import Data.Foldable (toList)
import Data.List (foldl', sortOn, isInfixOf)
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

import Cmedit.Inflate (inflateDyn)
import Cmedit.Types (Pos(..), origin)
import Cmedit.Width (colToDisplay, displayToCol, lineDisplayWidth, wrapLine)

------------------------------------------------------------------------------
-- The document model

-- | Character formatting, as much of it as a terminal can show. Bold and
-- italic are inferred from the font (its name and descriptor flags), not
-- declared: PDF has no notion of "the bold version of this run", only a
-- different font resource.
data PdfFmt = PdfFmt
  { pfBold   :: !Bool
  , pfItalic :: !Bool
  , pfMono   :: !Bool
  , pfSize   :: !Double  -- ^ Effective font size in points, after the text and current transformation matrices.
  } deriving (Eq, Show)

defaultPdfFmt :: PdfFmt
defaultPdfFmt = PdfFmt False False False 0

data PdfAlign = PAlignLeft | PAlignCenter | PAlignRight
  deriving (Eq, Show)

-- | What a reconstructed block of text is.
data PdfKind
  = PKFlow    -- ^ Ordinary prose: re-wrapped to the terminal width.
  | PKFixed   -- ^ A line whose horizontal positions carry meaning (a table row, a code listing): placed by column, never re-wrapped.
  | PKBlank   -- ^ Vertical space between blocks.
  | PKRule    -- ^ A page boundary, drawn as a rule.
  deriving (Eq, Show)

-- | A run of text in one format. @psX@ is where it started on the page, as a
-- fraction of its column's width — meaningful for 'PKFixed' lines (which are
-- laid out by it) and for the first run of a paragraph (which sets the
-- indent); ignored otherwise.
data PdfSpan = PdfSpan
  { psX    :: !Double
  , psText :: !Text
  , psFmt  :: !PdfFmt
  } deriving (Eq, Show)

-- | One reconstructed block: a paragraph, a table row, a blank, or a rule.
data PdfPar = PdfPar
  { ppKind   :: !PdfKind
  , ppAlign  :: !PdfAlign
  , ppMargin :: !Double    -- ^ Left margin as a fraction of the column width.
  , ppFirst  :: !Double    -- ^ Extra first-line indent, as a fraction (negative for a hanging indent).
  , ppRuns   :: ![PdfSpan]
  } deriving (Eq, Show)

data PdfPage = PdfPage
  { pgNumber :: !Int
  , pgPars   :: ![PdfPar]
  } deriving (Eq, Show)

-- | One laid-out line. Same shape as "Cmedit.Rtf"'s 'Cmedit.Rtf.RtfLine' —
-- leading pad, text, and character ranges carrying the formatting — because
-- the renderer feeds both to the same @expandLineCellsFrom@ the plain text
-- view uses. @plPage@ is what the status bar and page navigation read.
data PdfLine = PdfLine
  { plPad   :: !Int
  , plText  :: !Text
  , plSpans :: ![(Int, Int, PdfFmt)]
  , plRule  :: !Bool
  , plPage  :: !Int
  } deriving (Eq, Show)

-- | The PDF view of a document: the extracted pages, the scroll position, and
-- the laid-out lines cached against the width they were laid out for.
--
-- Extraction happens once, when the file is opened (it is the expensive part:
-- inflating every content stream and interpreting it). Layout re-runs on a
-- resize only, and scrolling never re-lays out — the same bargain 'Cmedit.Rtf'
-- and the image view make.
data PdfDoc = PdfDoc
  { pdPages  :: !(Seq PdfPage)
  , pdTop    :: !Int    -- ^ First visible laid-out line.
  , pdCache  :: !(Maybe (Int, Int, Seq PdfLine, Seq Int))
      -- ^ (width, tab width, lines, first line index of each page).
  , pdCaret  :: !Pos
      -- ^ The end of the selection that moves, in laid-out (line, character)
      -- coordinates. Shown as the terminal cursor only while 'pdAnchor' is
      -- set: with no selection this view has no position worth pointing at.
  , pdAnchor :: !(Maybe Pos)
      -- ^ The fixed end of the selection; 'Nothing' when there is none.
      --
      -- Both are indices into the *laid-out* lines, so they mean nothing after
      -- a re-wrap — 'pdfRelayout' drops the selection when the width changes,
      -- which is also what a reader expects from reflowing the document.
  , pdTitle  :: !Text   -- ^ @\/Info \/Title@, when the file names one.
  , pdVer    :: !Text   -- ^ The @%PDF-1.x@ header version.
  , pdNote   :: !Text   -- ^ A warning to show on opening (truncation, no extractable text), or empty.
  } deriving (Show)

------------------------------------------------------------------------------
-- Limits.
--
-- Every one of these bounds work that a *malformed or hostile* file could
-- otherwise make unbounded; none of them is reached by an ordinary document.

-- | Largest PDF this view will parse. The object scan and the extracted text
-- both cost memory linear in the file.
maxPdfBytes :: Int
maxPdfBytes = 128 * 1024 * 1024

-- | Most pages extracted; beyond this the view says it stopped.
maxPdfPages :: Int
maxPdfPages = 2000

-- | Most characters extracted across the whole document.
maxPdfChars :: Int
maxPdfChars = 4 * 1024 * 1024

-- | Largest single decompressed stream.
maxStreamBytes :: Int
maxStreamBytes = 64 * 1024 * 1024

-- | Content-stream operators executed per page, and form-XObject nesting.
-- A form that draws itself would otherwise not terminate.
maxOpsPerPage, maxFormDepth :: Int
maxOpsPerPage = 400000
maxFormDepth  = 8

-- | Does this file start like a PDF? Producers are allowed to put junk before
-- the header, so the marker is looked for in the first kilobyte rather than at
-- offset zero — and that tolerance is also what makes this safe to run before
-- the binary-file refusal, which a PDF would otherwise always fail.
sniffPdf :: BS.ByteString -> Bool
sniffPdf bs = BS.isInfixOf (BC.pack "%PDF-") (BS.take 1024 bs)

------------------------------------------------------------------------------
-- Objects

type Dict = M.Map BS.ByteString Obj

data Obj
  = ONull
  | OBool !Bool
  | ONum !Double
  | OStr !BS.ByteString
  | OName !BS.ByteString
  | OArr [Obj]
  | ODict !Dict
  | OStream !Dict !BS.ByteString   -- ^ Dictionary plus the raw, still-encoded bytes.
  | ORef !Int                      -- ^ Generation numbers are dropped: the scan already resolves duplicates by position.
  deriving (Eq, Show)

-- | A parsed file: every object found, and the trailer that names the catalog.
data PdfFile = PdfFile
  { pfObjs    :: !(IM.IntMap Obj)
  , pfTrailer :: !Dict
  }

isWsB :: Word8 -> Bool
isWsB c = c == 32 || c == 10 || c == 13 || c == 9 || c == 12 || c == 0

isDelimB :: Word8 -> Bool
isDelimB c = c `elem` [40, 41, 60, 62, 91, 93, 123, 125, 47, 37]  -- ()<>[]{}/%

isRegB :: Word8 -> Bool
isRegB c = not (isWsB c) && not (isDelimB c)

byteAt :: BS.ByteString -> Int -> Word8
byteAt bs i = if i >= 0 && i < BS.length bs then BS.index bs i else 0

-- | Advance past whitespace and @%@ comments.
skipWs :: BS.ByteString -> Int -> Int
skipWs bs = go
  where
    n = BS.length bs
    go !i
      | i >= n = i
      | isWsB (BS.index bs i) = go (i + 1)
      | BS.index bs i == 37 = go (eol i)   -- '%'
      | otherwise = i
    eol !i | i >= n = i
           | BS.index bs i == 10 || BS.index bs i == 13 = i
           | otherwise = eol (i + 1)

-- | Parse one object at @i@, returning it and the position just after.
-- Never fails destructively: an unrecognised token is consumed as 'ONull', so
-- a malformed region costs the objects inside it and nothing more.
parseObj :: BS.ByteString -> Int -> Maybe (Obj, Int)
parseObj bs i0
  | i >= n = Nothing
  | c == 47 = let (nm, j) = readName bs (i + 1) in Just (OName nm, j)
  | c == 40 = let (s, j) = readLitStr bs (i + 1) in Just (OStr s, j)
  | c == 60 && byteAt bs (i + 1) == 60 = parseDictAt bs (i + 2)
  | c == 60 = let (s, j) = readHexStr bs (i + 1) in Just (OStr s, j)
  | c == 91 = parseArrAt bs (i + 1) []
  | c == 93 || c == 62 = Nothing            -- a closer: the caller handles it
  | isNumStart c = parseNumAt bs i
  | otherwise =
      let (kw, j) = readRegular bs i
      in if BS.null kw
           then Just (ONull, i + 1)         -- an unexpected delimiter: step over it
           else Just (keyword kw, j)
  where
    n = BS.length bs
    i = skipWs bs i0
    c = byteAt bs i
    isNumStart b = (b >= 48 && b <= 57) || b == 43 || b == 45 || b == 46
    keyword k
      | k == BC.pack "true"  = OBool True
      | k == BC.pack "false" = OBool False
      | otherwise            = ONull

readRegular :: BS.ByteString -> Int -> (BS.ByteString, Int)
readRegular bs i = (BS.take len (BS.drop i bs), i + len)
  where len = go 0
        n = BS.length bs
        go !k | i + k < n && isRegB (BS.index bs (i + k)) = go (k + 1)
              | otherwise = k

-- | A name, with @#xx@ escapes decoded.
readName :: BS.ByteString -> Int -> (BS.ByteString, Int)
readName bs i =
  let (raw, j) = readRegular bs i
  in (if BS.elem 35 raw then BS.pack (unhash (BS.unpack raw)) else raw, j)
  where
    unhash (35 : a : b : rest)
      | isHexDigit (w2c a) && isHexDigit (w2c b) =
          fromIntegral (16 * digitToInt (w2c a) + digitToInt (w2c b)) : unhash rest
    unhash (x : rest) = x : unhash rest
    unhash [] = []

w2c :: Word8 -> Char
w2c = chr . fromIntegral

-- | A @(...)@ string: balanced parentheses, backslash escapes, octal escapes,
-- and a backslash-newline line continuation.
readLitStr :: BS.ByteString -> Int -> (BS.ByteString, Int)
readLitStr bs i0 = go i0 (1 :: Int) []
  where
    n = BS.length bs
    go !i !depth acc
      | i >= n = (BS.pack (reverse acc), i)
      | otherwise = case BS.index bs i of
          92 -> esc (i + 1) depth acc                     -- backslash
          40 -> go (i + 1) (depth + 1) (40 : acc)
          41 | depth <= 1 -> (BS.pack (reverse acc), i + 1)
             | otherwise  -> go (i + 1) (depth - 1) (41 : acc)
          b  -> go (i + 1) depth (b : acc)
    esc !i !depth acc
      | i >= n = (BS.pack (reverse acc), i)
      | otherwise = case BS.index bs i of
          110 -> go (i + 1) depth (10 : acc)   -- n
          114 -> go (i + 1) depth (13 : acc)   -- r
          116 -> go (i + 1) depth (9 : acc)    -- t
          98  -> go (i + 1) depth (8 : acc)    -- b
          102 -> go (i + 1) depth (12 : acc)   -- f
          10  -> go (i + 1) depth acc          -- line continuation
          13  -> go (if byteAt bs (i + 1) == 10 then i + 2 else i + 1) depth acc
          b | b >= 48 && b <= 55 ->
                let ds = takeWhile (\k -> let x = byteAt bs (i + k)
                                          in k < 3 && x >= 48 && x <= 55) [0, 1, 2]
                    v = foldl' (\a k -> a * 8 + fromIntegral (byteAt bs (i + k)) - 48) 0 ds
                in go (i + length ds) depth (fromIntegral (v .&. 0xff :: Int) : acc)
            | otherwise -> go (i + 1) depth (b : acc)

-- | A @<...>@ hex string; an odd final digit is padded with zero, per spec.
readHexStr :: BS.ByteString -> Int -> (BS.ByteString, Int)
readHexStr bs i0 = go i0 []
  where
    n = BS.length bs
    go !i acc
      | i >= n = (pack' acc, i)
      | BS.index bs i == 62 = (pack' acc, i + 1)
      | isHexDigit (w2c (BS.index bs i)) = go (i + 1) (digitToInt (w2c (BS.index bs i)) : acc)
      | otherwise = go (i + 1) acc
    pack' acc =
      let ds = reverse acc
          ds' = if odd (length ds) then ds ++ [0] else ds
      in BS.pack (pairs ds')
    pairs (a : b : rest) = fromIntegral (a * 16 + b) : pairs rest
    pairs _ = []

-- | A number — or an indirect reference, which is three tokens (@int int R@)
-- and so can only be recognised here, by lookahead.
parseNumAt :: BS.ByteString -> Int -> Maybe (Obj, Int)
parseNumAt bs i =
  let (tok, j) = readRegular bs i
      v = readNumber tok
  in case asInt tok of
       Just num | num >= 0 ->
         let k = skipWs bs j
             (gtok, k2) = readRegular bs k
             k3 = skipWs bs k2
         in case asInt gtok of
              Just _ | byteAt bs k3 == 82                       -- 'R'
                     , not (isRegB (byteAt bs (k3 + 1))) -> Just (ORef num, k3 + 1)
              _ -> Just (ONum v, j)
       _ -> Just (ONum v, j)

asInt :: BS.ByteString -> Maybe Int
asInt s
  | BS.null s = Nothing
  | BS.all (\c -> c >= 48 && c <= 57) s && BS.length s <= 10 =
      Just (BS.foldl' (\a c -> a * 10 + fromIntegral c - 48) 0 s)
  | otherwise = Nothing

-- | Tolerant decimal read: PDF reals have no exponent, but broken producers
-- emit things like @--3@ or @4.@, and a viewer should not stop for those.
readNumber :: BS.ByteString -> Double
readNumber s0 =
  let str = BC.unpack s0
      (sign, str1) = case span (`elem` ("+-" :: String)) str of
                       (ss, r) -> (if odd (length (filter (== '-') ss)) then -1 else 1, r)
      (ip, rest) = span isDigit str1
      fp = case rest of ('.' : r) -> takeWhile isDigit r; _ -> ""
      whole = foldl' (\a d -> a * 10 + fromIntegral (digitToInt d)) 0 (take 12 ip) :: Double
      frac = foldr (\d a -> (a + fromIntegral (digitToInt d)) / 10) 0 (take 10 fp) :: Double
  in sign * (whole + frac)

parseArrAt :: BS.ByteString -> Int -> [Obj] -> Maybe (Obj, Int)
parseArrAt bs i acc =
  let j = skipWs bs i
  in if j >= BS.length bs then Just (OArr (reverse acc), j)
     else if byteAt bs j == 93 then Just (OArr (reverse acc), j + 1)
     else case parseObj bs j of
            Just (o, k) | k > j -> parseArrAt bs k (o : acc)
            _ -> Just (OArr (reverse acc), j + 1)

-- | A dictionary, and — if @stream@ follows it — the stream it heads.
parseDictAt :: BS.ByteString -> Int -> Maybe (Obj, Int)
parseDictAt bs i0 = go i0 M.empty
  where
    n = BS.length bs
    go !i !acc
      | j >= n = Just (ODict acc, j)
      | byteAt bs j == 62 && byteAt bs (j + 1) == 62 = afterDict acc (j + 2)
      | byteAt bs j == 47 =
          let (k, j1) = readName bs (j + 1)
          in case parseObj bs j1 of
               Just (v, j2) -> go j2 (M.insert k v acc)
               Nothing      -> Just (ODict acc, j1)
      -- Junk where a key belongs: step over it rather than abandoning the
      -- dictionary, which may still hold the /Type or /Font we need.
      | otherwise = go (j + 1) acc
      where j = skipWs bs i

    afterDict d j
      | BS.isPrefixOf (BC.pack "stream") (BS.drop j' bs) = stream d (j' + 6)
      | otherwise = Just (ODict d, j)
      where j' = skipSpaceOnly j
    -- Only spaces may separate ">>" from "stream"; a comment cannot.
    skipSpaceOnly !k | k < n && (BS.index bs k == 32 || BS.index bs k == 13
                                 || BS.index bs k == 10 || BS.index bs k == 9) = skipSpaceOnly (k + 1)
                     | otherwise = k

    stream d j =
      let dataStart = case (byteAt bs j, byteAt bs (j + 1)) of
                        (13, 10) -> j + 2
                        (10, _)  -> j + 1
                        (13, _)  -> j + 1
                        _        -> j
          declared = case M.lookup (BC.pack "Length") d of
                       Just (ONum v) | v >= 0 -> Just (truncate v)
                       _                      -> Nothing
          okDeclared len =
            let e = skipWs bs (dataStart + len)
            in dataStart + len <= n && BS.isPrefixOf (BC.pack "endstream") (BS.drop e bs)
          -- /Length is often an indirect reference (unresolvable here) or
          -- simply wrong; searching for the terminator always works, so the
          -- declared length is used only when it agrees with one.
          len = case declared of
                  Just l | okDeclared l -> l
                  _ -> searchEnd
          searchEnd =
            let hay = BS.drop dataStart bs
                (pre, post) = BS.breakSubstring (BC.pack "endstream") hay
            in if BS.null post then BS.length hay else trimEol pre
          trimEol p
            | BS.length p >= 2, BS.index p (BS.length p - 2) == 13
                              , BS.index p (BS.length p - 1) == 10 = BS.length p - 2
            | BS.length p >= 1, let c = BS.index p (BS.length p - 1)
                              , c == 10 || c == 13 = BS.length p - 1
            | otherwise = BS.length p
          raw = BS.take len (BS.drop dataStart bs)
          after = let e = skipWs bs (dataStart + len)
                  in if BS.isPrefixOf (BC.pack "endstream") (BS.drop e bs) then e + 9 else dataStart + len
      in Just (OStream d raw, after)

------------------------------------------------------------------------------
-- Finding every object in the file

-- | Scan for @N G obj@ and parse what follows each one.
--
-- Objects are keyed by number and carry the offset they were defined at, so a
-- later definition wins — which is what an incrementally-updated file means.
-- Scanning resumes after the object just parsed, so an @obj@ that happens to
-- occur inside a compressed stream's bytes is never mistaken for a definition.
scanObjects :: BS.ByteString -> IM.IntMap (Int, Obj)
scanObjects bs = go 0 IM.empty
  where
    n = BS.length bs
    marker = BC.pack "obj"
    go !i !acc
      | i >= n = acc
      | otherwise =
          let hay = BS.drop i bs
              (pre, post) = BS.breakSubstring marker hay
          in if BS.null post then acc
             else
               let at = i + BS.length pre
               in if isRegB (byteAt bs (at + 3))
                    then go (at + 3) acc          -- "object", "objects", ...
                    else case objHeader bs at of
                      Nothing -> go (at + 3) acc
                      Just (num, start) -> case parseObj bs (at + 3) of
                        Just (o, j) | j > at ->
                          go (skipEndobj j) (IM.insertWith later num (start, o) acc)
                        _ -> go (at + 3) acc
    later new old = if fst new >= fst old then new else old
    skipEndobj j =
      let k = skipWs bs j
      in if BS.isPrefixOf (BC.pack "endobj") (BS.drop k bs) then k + 6 else max j 0

-- | Read the @N G@ that must precede an @obj@ keyword, walking backwards.
-- Returns the object number and the offset its definition starts at.
objHeader :: BS.ByteString -> Int -> Maybe (Int, Int)
objHeader bs at = do
  g1 <- backWs (at - 1)
  (_, g0) <- backDigits g1
  n1 <- backWs (g0 - 1)
  (num, n0) <- backDigits n1
  if n0 == 0 || not (isRegB (byteAt bs (n0 - 1))) then Just (num, n0) else Nothing
  where
    -- Skip whitespace backwards; at least one byte of it is required.
    backWs i | i >= 0 && isWsB (byteAt bs i) = Just (skipBack i)
             | otherwise = Nothing
    skipBack i | i >= 0 && isWsB (byteAt bs i) = skipBack (i - 1)
               | otherwise = i
    -- Read a decimal number backwards, returning it and its first byte.
    backDigits i
      | i < 0 = Nothing
      | not (isDig (byteAt bs i)) = Nothing
      | otherwise =
          let s = start i
              len = i - s + 1
          in if len > 10 then Nothing
             else Just (fromMaybe 0 (asInt (BS.take len (BS.drop s bs))), s)
      where start k | k > 0 && isDig (byteAt bs (k - 1)) = start (k - 1)
                    | otherwise = k
    isDig c = c >= 48 && c <= 57

-- | Objects that live inside compressed object streams, which the scan cannot
-- see. Expanded after it, and folded in by the same later-definition-wins
-- rule (an @\/ObjStm@'s own offset stands for the objects inside it).
expandObjStms :: BS.ByteString -> IM.IntMap (Int, Obj) -> IM.IntMap (Int, Obj)
expandObjStms _bs found = foldl' step found (IM.toList found)
  where
    base = PdfFile (IM.map snd found) M.empty
    step acc (_, (off, OStream d raw))
      | M.lookup (BC.pack "Type") d == Just (OName (BC.pack "ObjStm"))
      , Right dat <- decodeStream base d raw =
          let cnt = intOf base (M.lookup (BC.pack "N") d)
              first = intOf base (M.lookup (BC.pack "First") d)
              hdr = readPairs dat 0 (min cnt 100000)
          in foldl' (\a (num, rel) -> case parseObj dat (first + rel) of
                        Just (o, _) -> IM.insertWith later num (off, o) a
                        Nothing     -> a)
                    acc hdr
    step acc _ = acc
    later new old = if fst new >= fst old then new else old
    -- The header is 2N integers: object number, then offset from /First.
    readPairs dat = goPairs
      where
        goPairs !i !k
          | k <= 0 = []
          | otherwise =
              let i1 = skipWs dat i
                  (a, i2) = readRegular dat i1
                  i3 = skipWs dat i2
                  (b, i4) = readRegular dat i3
              in case (asInt a, asInt b) of
                   (Just x, Just y) -> (x, y) : goPairs i4 (k - 1)
                   _ -> []

intOf :: PdfFile -> Maybe Obj -> Int
intOf f o = truncate (numOf f o)

numOf :: PdfFile -> Maybe Obj -> Double
numOf f o = case resolve f (fromMaybe ONull o) of
  ONum v -> v
  _      -> 0

-- | Follow indirect references, with a depth limit so a self-referential file
-- cannot loop.
resolve :: PdfFile -> Obj -> Obj
resolve f = go (16 :: Int)
  where
    go 0 o = o
    go k (ORef r) = go (k - 1) (IM.findWithDefault ONull r (pfObjs f))
    go _ o = o

dictOf :: PdfFile -> Obj -> Dict
dictOf f o = case resolve f o of
  ODict d     -> d
  OStream d _ -> d
  _           -> M.empty

lookD :: PdfFile -> Dict -> String -> Obj
lookD f d k = resolve f (M.findWithDefault ONull (BC.pack k) d)

arrOf :: PdfFile -> Obj -> [Obj]
arrOf f o = case resolve f o of
  OArr xs -> xs
  ONull   -> []
  x       -> [x]

nameOf :: Obj -> BS.ByteString
nameOf (OName n) = n
nameOf _ = BS.empty

------------------------------------------------------------------------------
-- Stream filters

-- | Decode a stream through its @\/Filter@ chain. Image codecs are reported
-- rather than attempted: the caller (content-stream loading) simply skips
-- what it cannot read.
decodeStream :: PdfFile -> Dict -> BS.ByteString -> Either String BS.ByteString
decodeStream f d raw = foldl' step (Right raw) (zip filters parms)
  where
    filters = map nameOf (arrOf f (lookD f d "Filter"))
    parmList = case lookD f d "DecodeParms" of
                 OArr xs -> map (dictOf f) xs
                 ONull   -> []
                 o       -> [dictOf f o]
    parms = parmList ++ repeat M.empty
    step (Left e) _ = Left e
    step (Right dat) (nm, pd) = do
      out <- applyFilter (BC.unpack nm) dat
      pure (applyPredictor f pd out)

applyFilter :: String -> BS.ByteString -> Either String BS.ByteString
applyFilter nm dat = case nm of
  "FlateDecode"     -> flate
  "Fl"              -> flate
  "LZWDecode"       -> Right (lzwDecode dat)
  "LZW"             -> Right (lzwDecode dat)
  "ASCIIHexDecode"  -> Right (hexDecode dat)
  "AHx"             -> Right (hexDecode dat)
  "ASCII85Decode"   -> Right (a85Decode dat)
  "A85"             -> Right (a85Decode dat)
  "RunLengthDecode" -> Right (rleDecode dat)
  "RL"              -> Right (rleDecode dat)
  "Crypt"           -> Right dat        -- the Identity crypt filter
  ""                -> Right dat
  other             -> Left (other ++ ": not a text filter")
  where
    -- A zlib wrapper is the norm; a few producers emit raw DEFLATE, so a
    -- failed header check retries from byte zero rather than giving up.
    flate =
      let hdr2 = BS.length dat >= 2
                   && (fromIntegral (BS.index dat 0) .&. (0x0f :: Int)) == 8
                   && ((fromIntegral (BS.index dat 0) * 256
                        + fromIntegral (BS.index dat 1)) `mod` (31 :: Int)) == 0
      in case inflateDyn dat (if hdr2 then 2 else 0) maxStreamBytes of
           Right out -> Right out
           Left _ | hdr2 -> inflateDyn dat 0 maxStreamBytes
           Left e -> Left e

-- | Undo a PNG or TIFF predictor. Used by cross-reference streams and by any
-- stream whose producer thought it would compress better; harmless (identity)
-- when @\/Predictor@ is absent or 1.
applyPredictor :: PdfFile -> Dict -> BS.ByteString -> BS.ByteString
applyPredictor f pd dat
  | pred' < 2 = dat
  | pred' == 2 = tiff
  | otherwise = png
  where
    pred'   = intOf f (M.lookup (BC.pack "Predictor") pd)
    colors  = maxI 1 (intOf f (M.lookup (BC.pack "Colors") pd))
    bpc     = maxI 1 (intOf f (M.lookup (BC.pack "BitsPerComponent") pd))
    columns = maxI 1 (intOf f (M.lookup (BC.pack "Columns") pd))
    maxI a b = if b <= 0 then a else b
    bpp = max 1 ((colors * bpc + 7) `div` 8)
    rowLen = max 1 ((colors * bpc * columns + 7) `div` 8)
    -- The left neighbour comes out of the reversed accumulator rather than by
    -- indexing the output, which would make each row quadratic; @bpp@ is at
    -- most a handful of bytes, so the walk back is a constant.
    leftOf acc = case drop (bpp - 1) acc of (x : _) -> x; [] -> 0
    headOr0 xs = case xs of (x : _) -> x; [] -> 0

    -- TIFF predictor 2 at 8 bits: each component is a delta from the one
    -- @bpp@ bytes earlier in the same row.
    tiff | bpc /= 8 = dat
         | otherwise = BS.pack (concatMap row (chunks rowLen (BS.unpack dat)))
      where
        row r = go r []
          where go [] acc = reverse acc
                go (v : vs) acc = go vs ((v + leftOf acc) : acc)

    png = BS.pack (concat (goRows (BS.unpack dat) (replicate rowLen 0)))
      where
        goRows [] _ = []
        goRows (ft : rest) prev =
          let (row, more) = splitAt rowLen rest
              out = unfilterRow (fromIntegral ft) row prev
          in out : (if null more then [] else goRows more out)
        -- Three lists walk in step: the filtered row, the row above (@b@) and
        -- the row above shifted by @bpp@ (@c@).
        unfilterRow ft row prev = go row prev (replicate bpp 0 ++ prev) []
          where
            go [] _ _ acc = reverse acc
            go (v : vs) ps cs acc =
              let a = leftOf acc
                  b = headOr0 ps
                  c = headOr0 cs
                  x = case ft :: Int of
                        0 -> v
                        1 -> v + a
                        2 -> v + b
                        3 -> v + fromIntegral ((fromIntegral a + fromIntegral b :: Int) `div` 2)
                        4 -> v + paeth a b c
                        _ -> v
              in go vs (drop 1 ps) (drop 1 cs) (x : acc)
        paeth a b c =
          let p = fromIntegral a + fromIntegral b - fromIntegral c :: Int
              pa = abs (p - fromIntegral a); pb = abs (p - fromIntegral b); pc = abs (p - fromIntegral c)
          in if pa <= pb && pa <= pc then a else if pb <= pc then b else c

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks k xs = let (a, b) = splitAt k xs in a : chunks k b

hexDecode :: BS.ByteString -> BS.ByteString
hexDecode = fst . readHexStr'
  where readHexStr' bs = readHexStr (BS.snoc bs 62) 0

a85Decode :: BS.ByteString -> BS.ByteString
a85Decode bs0 = BS.pack (go (filter (not . isWsB) (BS.unpack body)) )
  where
    -- Some producers include the "<~" introducer even though PDF's variant
    -- does not use it.
    body = let b = if BS.isPrefixOf (BC.pack "<~") bs0 then BS.drop 2 bs0 else bs0
               (pre, _) = BS.breakSubstring (BC.pack "~>") b
           in pre
    go [] = []
    go (122 : rest) = [0, 0, 0, 0] ++ go rest      -- 'z'
    go cs =
      let (grp, rest) = splitAt 5 cs
          k = length grp
      in if k < 2 then []
         else
           let padded = grp ++ replicate (5 - k) 117   -- 'u'
               v = foldl' (\a c -> a * 85 + (fromIntegral c - 33)) (0 :: Int) padded
               bytes = [ fromIntegral ((v `shiftR` s) .&. 0xff) | s <- [24, 16, 8, 0] ]
           in take (k - 1) bytes ++ go rest

rleDecode :: BS.ByteString -> BS.ByteString
rleDecode bs = BS.pack (go 0)
  where
    n = BS.length bs
    go !i
      | i >= n = []
      | l == 128 = []
      | l < 128 = BS.unpack (BS.take (l + 1) (BS.drop (i + 1) bs)) ++ go (i + l + 2)
      | otherwise = replicate (257 - l) (byteAt bs (i + 1)) ++ go (i + 2)
      where l = fromIntegral (BS.index bs i) :: Int

-- | PDF's LZW: MSB-first codes, and the code width grows one entry *early*
-- (the @\/EarlyChange@ default), which is the one place it differs from GIF's
-- — hence a separate implementation from the one in "Cmedit.Image".
lzwDecode :: BS.ByteString -> BS.ByteString
lzwDecode bs = BS.concat (reverse (go 0 9 initTable Nothing 0 []))
  where
    nbits = 8 * BS.length bs
    initTable = IM.fromList [ (i, BS.singleton (fromIntegral i)) | i <- [0 .. 255] ]
    getCode p w
      | p + w > nbits = Nothing
      | otherwise = Just (foldl' (\a k ->
          let bit = (fromIntegral (byteAt bs ((p + k) `shiftR` 3)) `shiftR` (7 - ((p + k) .&. 7))) .&. 1
          in (a `shiftL` 1) .|. bit) (0 :: Int) [0 .. w - 1])
    -- @out@ is the running output length. Measuring it by concatenating the
    -- accumulator would make the whole decode quadratic in its own output.
    go !p !w !table prev !out acc
      | out > maxStreamBytes = acc
      | otherwise = case getCode p w of
          Nothing -> acc
          Just 257 -> acc
          Just 256 -> go (p + w) 9 initTable Nothing out acc
          Just code ->
            let next = IM.size table + 2
                entry = case IM.lookup code table of
                  Just e  -> e
                  -- The one self-referential case: a code for an entry that
                  -- this very step is about to define.
                  Nothing -> case prev of
                               Just pv -> pv `BS.snoc` BS.head pv
                               Nothing -> BS.empty
                table' = case prev of
                  -- 4096 codes is all a 12-bit stream has; past that the
                  -- encoder owes us a clear code, and adding entries anyway
                  -- would only desynchronise the two tables.
                  Just pv | not (BS.null entry), next < 4096 ->
                              IM.insert next (pv `BS.snoc` BS.head entry) table
                  _ -> table
                sz = IM.size table' + 2
                -- PDF's default /EarlyChange widens the code one entry before
                -- the table actually needs it.
                w' | sz >= 2047 = 12
                   | sz >= 1023 = 11
                   | sz >= 511  = 10
                   | otherwise  = 9
            in if BS.null entry then acc
               else go (p + w) w' table' (Just entry) (out + BS.length entry) (entry : acc)

------------------------------------------------------------------------------
-- The document: trailer, catalog, page tree

-- | Find the trailer dictionary — the one naming @\/Root@.
--
-- Both spellings are looked for: the classic @trailer \<\<...\>\>@ keyword and
-- the cross-reference *stream* whose own dictionary is the trailer (PDF 1.5+).
-- Failing both, any object that calls itself a catalog will do; a file whose
-- trailer was truncated away still has its page tree.
findTrailer :: BS.ByteString -> IM.IntMap Obj -> Dict
findTrailer bs objs
  | not (null withRoot) = last withRoot
  | not (null xrefDicts) = last xrefDicts
  | otherwise = case [ num | (num, o) <- IM.toList objs
                     , M.lookup (BC.pack "Type") (plainDict o) == Just (OName (BC.pack "Catalog")) ] of
      (num : _) -> M.singleton (BC.pack "Root") (ORef num)
      []        -> M.empty
  where
    plainDict (ODict d) = d
    plainDict (OStream d _) = d
    plainDict _ = M.empty
    hasRoot d = M.member (BC.pack "Root") d
    withRoot = filter hasRoot (trailerKeywords ++ xrefDicts)
    xrefDicts = [ d | (_, o) <- IM.toList objs, let d = plainDict o
                , M.lookup (BC.pack "Type") d == Just (OName (BC.pack "XRef")) ]
    trailerKeywords = go 0
      where
        go i =
          let hay = BS.drop i bs
              (pre, post) = BS.breakSubstring (BC.pack "trailer") hay
          in if BS.null post then []
             else let at = i + BS.length pre + 7
                  in case parseObj bs at of
                       Just (ODict d, _) -> d : go at
                       _                 -> go at

-- | Walk the page tree, carrying the attributes a page inherits from its
-- ancestors. Cycles (a malformed tree pointing back at itself) are cut by the
-- visited set; a file with no usable tree falls back to every object that
-- calls itself a page, in object-number order.
collectPages :: PdfFile -> [Dict]
collectPages f
  | not (null viaTree) = viaTree
  | otherwise = [ d | (_, o) <- IM.toAscList (pfObjs f), let d = dictOf f o
                , M.lookup (BC.pack "Type") d == Just (OName (BC.pack "Page")) ]
  where
    root = dictOf f (M.findWithDefault ONull (BC.pack "Root") (pfTrailer f))
    pagesRef = M.findWithDefault ONull (BC.pack "Pages") root
    viaTree = take maxPdfPages (walk [] 0 M.empty pagesRef)
    inheritable = map BC.pack ["Resources", "MediaBox", "CropBox", "Rotate"]
    walk seen depth inh node
      | depth > 64 = []
      | ORef r <- node, r `elem` seen = []
      | otherwise =
          let seen' = case node of ORef r -> r : seen; _ -> seen
              d = dictOf f node
              inh' = foldl' (\a k -> case M.lookup k d of
                                       Just v  -> M.insert k v a
                                       Nothing -> a) inh inheritable
              ty = M.lookup (BC.pack "Type") d
              kids = arrOf f (M.findWithDefault ONull (BC.pack "Kids") d)
          in if ty == Just (OName (BC.pack "Page")) || (null kids && M.member (BC.pack "Contents") d)
               then [M.union d inh']
               else concatMap (walk seen' (depth + 1) inh') kids

------------------------------------------------------------------------------
-- Fonts
--
-- Everything here exists to answer two questions per glyph code: what
-- character is this, and how wide is it. The first decides what the view
-- shows; the second decides where words break, since PDF does not record
-- spaces between positioned runs — they have to be inferred from the gap.

data FontInfo = FontInfo
  { fiBold    :: !Bool
  , fiItalic  :: !Bool
  , fiMono    :: !Bool
  , fiTwoByte :: !Bool                 -- ^ Composite font with 2-byte codes (Identity-H and friends).
  , fiToUni   :: !(IM.IntMap Text)     -- ^ From a @\/ToUnicode@ CMap: authoritative when present.
  , fiEnc     :: !(IM.IntMap Char)     -- ^ Simple-font encoding, base plus @\/Differences@.
  , fiWidths  :: !(IM.IntMap Double)   -- ^ Glyph widths, in the units 'fiWScale' converts.
  , fiDefW    :: !Double
  , fiWScale  :: !Double
      -- ^ What one unit of 'fiWidths' is worth in text space. A thousandth of
      -- an em for every font except Type3, whose widths are in its own glyph
      -- space and must go through its @\/FontMatrix@ — miss that and a
      -- Type3 document's advances are wrong by orders of magnitude, which
      -- shows up as text with every space missing (the gaps this view infers
      -- word breaks from all collapse to zero).
  }

emptyFont :: FontInfo
emptyFont = FontInfo False False False False IM.empty winAnsiMap IM.empty 500 0.001

loadFont :: PdfFile -> Dict -> FontInfo
loadFont f d = FontInfo
  { fiBold = bold, fiItalic = italic, fiMono = mono
  , fiTwoByte = twoByte
  , fiToUni = toUni
  , fiEnc = encMap
  , fiWidths = widths
  , fiDefW = defW
  , fiWScale = wScale
  }
  where
    subtype = nameOf (lookD f d "Subtype")
    isType3 = subtype == BC.pack "Type3"
    wScale
      | isType3 = case map (numOf f . Just) (arrOf f (lookD f d "FontMatrix")) of
                    (a : _) | a /= 0 -> a
                    _ -> 0.001
      | otherwise = 0.001
    baseFont = BC.unpack (nameOf (lookD f d "BaseFont"))
    -- A subset-embedded font is named "ABCDEF+Real-Name"; the tag says
    -- nothing about the face.
    faceName = map toLower (drop 1 (dropWhile (/= '+') baseFont ++ "+" ++ baseFont))
    isType0 = subtype == BC.pack "Type0"
    descendant = case arrOf f (lookD f d "DescendantFonts") of
                   (x : _) -> dictOf f x
                   []      -> M.empty
    fdSource = if isType0 then descendant else d
    descr = dictOf f (lookD f fdSource "FontDescriptor")
    flags = intOf f (M.lookup (BC.pack "Flags") descr)
    italicAngle = numOf f (M.lookup (BC.pack "ItalicAngle") descr)
    stemV = numOf f (M.lookup (BC.pack "StemV") descr)
    has s = s `isInfixOf` faceName
    bold = has "bold" || has "black" || has "heavy" || has "semib"
             || (flags .&. 0x40000) /= 0 || stemV >= 120
    italic = has "italic" || has "oblique" || (flags .&. 0x40) /= 0 || italicAngle /= 0
    mono = has "mono" || has "courier" || has "consol" || (flags .&. 0x1) /= 0

    toUni = case lookD f d "ToUnicode" of
      OStream sd raw -> either (const IM.empty) parseToUnicode (decodeStream f sd raw)
      _              -> IM.empty

    -- Composite fonts are 2-byte in every encoding this view meets; a
    -- one-byte CMap is legal but essentially unused outside CJK legacy files.
    twoByte = isType0

    -- Simple-font encoding: a named base, or a dictionary carrying a base and
    -- a /Differences list that renames individual codes.
    encMap
      | isType0 = IM.empty
      | otherwise = applyDifferences f (lookD f d "Encoding") baseMap
    baseMap = case lookD f d "Encoding" of
      OName n -> namedEncoding (BC.unpack n)
      o@(ODict _) -> case lookD f (dictOf f o) "BaseEncoding" of
                       OName n -> namedEncoding (BC.unpack n)
                       _       -> defaultBase
      _ -> defaultBase
    -- Symbolic fonts with no encoding of their own are anyone's guess; the
    -- WinAnsi assumption is right far more often than it is wrong, and a
    -- /ToUnicode map (which most such fonts carry) overrides it anyway.
    defaultBase = winAnsiMap

    widths
      | isType0 = cidWidths f (lookD f descendant "W")
      | otherwise =
          let first = intOf f (M.lookup (BC.pack "FirstChar") d)
              ws = [ numOf f (Just o) | o <- arrOf f (lookD f d "Widths") ]
          in if null ws
               -- Only the standard 14 have metrics to fall back on; a Type3
               -- font's glyphs are procedures we do not run, so an unlisted
               -- code simply does not advance.
               then (if isType3 then IM.empty else builtinWidths faceName)
               else IM.fromList (zip [first ..] ws)
    defW
      | isType0 = case lookD f descendant "DW" of
                    ONum v | v > 0 -> v
                    _              -> 1000
      | isType3 = 0
      | otherwise = case lookD f descr "MissingWidth" of
                      ONum v | v > 0 -> v
                      _ | mono       -> 600
                        | otherwise  -> 500

-- | @\/W@ for a CID font: @[c [w …] cFirst cLast w …]@, mixing both forms.
cidWidths :: PdfFile -> Obj -> IM.IntMap Double
cidWidths f o = go (arrOf f o) IM.empty
  where
    go (a : rest) acc = case (resolve f a, rest) of
      (ONum c, (b : more)) -> case resolve f b of
        OArr ws -> go more (foldl' (\m (i, v) -> IM.insert i (numOf f (Just v)) m)
                                   acc (zip [truncate c ..] ws))
        ONum c2 -> case more of
          (wv : more') -> go more' (foldl' (\m i -> IM.insert i (numOf f (Just wv)) m)
                                           acc [truncate c .. min (truncate c2) (truncate c + 65535)])
          [] -> acc
        _ -> go more acc
      _ -> acc
    go [] acc = acc

-- | Parse a @\/ToUnicode@ CMap: the @bfchar@ and @bfrange@ sections, which map
-- glyph codes to UTF-16BE strings. The rest of the CMap language (codespace
-- ranges, usecmap) is not needed to read one.
parseToUnicode :: BS.ByteString -> IM.IntMap Text
parseToUnicode bs = IM.union (chars) (ranges)
  where
    chars = sections "beginbfchar" "endbfchar" bfChar
    ranges = sections "beginbfrange" "endbfrange" bfRange
    sections open close h = foldl' IM.union IM.empty (map h (slices open close))
    slices open close = go 0
      where
        go i =
          let hay = BS.drop i bs
              (pre, post) = BS.breakSubstring (BC.pack open) hay
          in if BS.null post then []
             else
               let s = i + BS.length pre + length open
                   (body, after) = BS.breakSubstring (BC.pack close) (BS.drop s bs)
               in body : (if BS.null after then [] else go (s + BS.length body + length close))
    bfChar body = IM.fromList
      [ (codeOf a, utf16 b) | [a, b] <- pairsOf (objsIn body) ]
    bfRange body = IM.fromList (concatMap one (triplesOf (objsIn body)))
      where
        one [OStr a, OStr b, OStr dst] =
          let lo = beInt a; hi = min (beInt b) (lo + 65535)
              base = utf16 (OStr dst)
          in [ (c, bumpLast base (c - lo)) | c <- [lo .. hi] ]
        one [OStr a, OStr _, OArr ds] =
          [ (beInt a + k, utf16 v) | (k, v) <- zip [0 ..] ds ]
        one _ = []
    codeOf o = case o of OStr s -> beInt s; ONum v -> truncate v; _ -> 0
    beInt = BS.foldl' (\a c -> a * 256 + fromIntegral c) 0
    -- A bfrange's destination increments in its *last* code unit.
    bumpLast t k
      | T.null t = t
      | otherwise = T.init t `T.snoc` safeChr (fromEnum (T.last t) + k)
    safeChr n = if n >= 0 && n <= 0x10ffff && not (n >= 0xd800 && n <= 0xdfff)
                  then chr n else '\xfffd'
    utf16 (OStr s) = decodeUtf16BE s
    utf16 _ = T.empty
    objsIn body = unfoldObjs body 0
    unfoldObjs b i = case parseObj b i of
      Just (o, j) | j > i -> o : unfoldObjs b j
      _ -> []
    pairsOf (a : b : r) = [a, b] : pairsOf r
    pairsOf _ = []
    triplesOf (a : b : c : r) = [a, b, c] : triplesOf r
    triplesOf _ = []

decodeUtf16BE :: BS.ByteString -> Text
decodeUtf16BE s = T.pack (go (BS.unpack s))
  where
    go (a : b : rest) =
      let u = fromIntegral a * 256 + fromIntegral b :: Int
      in if u >= 0xd800 && u <= 0xdbff
           then case rest of
             (c : d : rest') ->
               let lo = fromIntegral c * 256 + fromIntegral d :: Int
               in if lo >= 0xdc00 && lo <= 0xdfff
                    then chr (0x10000 + ((u - 0xd800) `shiftL` 10) + (lo - 0xdc00)) : go rest'
                    else go rest'
             _ -> []
           else if u >= 0xdc00 && u <= 0xdfff then go rest
           else chr u : go rest
    go _ = []

-- | Apply a @\/Differences@ array (@code \/name code \/name …@) over a base
-- encoding.
applyDifferences :: PdfFile -> Obj -> IM.IntMap Char -> IM.IntMap Char
applyDifferences f enc base = case enc of
  ODict d -> go (arrOf f (M.findWithDefault ONull (BC.pack "Differences") d)) 0 base
  _       -> base
  where
    go (o : rest) !cur acc = case resolve f o of
      ONum v  -> go rest (truncate v) acc
      OName n -> go rest (cur + 1) (case glyphChar (BC.unpack n) of
                                      Just c  -> IM.insert cur c acc
                                      Nothing -> acc)
      _       -> go rest cur acc
    go [] _ acc = acc

-- | A glyph name to the character it draws. The Adobe glyph list proper runs
-- to four thousand names; this covers the Latin text set plus the @uniXXXX@
-- convention, which between them is what a Differences array in a text
-- document actually uses.
glyphChar :: String -> Maybe Char
glyphChar nm = case nm of
  [c] -> Just c
  ('u' : 'n' : 'i' : hx) | length hx >= 4, all isHexDigit (take 4 hx) ->
    Just (chr (foldl' (\a c -> a * 16 + digitToInt c) 0 (take 4 hx)))
  ('u' : hx) | length hx >= 4, all isHexDigit hx ->
    Just (chr (foldl' (\a c -> a * 16 + digitToInt c) 0 (take 6 hx)))
  _ -> lookup nm glyphNames

glyphNames :: [(String, Char)]
glyphNames =
  [ ("space", ' '), ("exclam", '!'), ("quotedbl", '"'), ("numbersign", '#')
  , ("dollar", '$'), ("percent", '%'), ("ampersand", '&'), ("quotesingle", '\'')
  , ("quoteright", '\x2019'), ("quoteleft", '\x2018'), ("parenleft", '(')
  , ("parenright", ')'), ("asterisk", '*'), ("plus", '+'), ("comma", ',')
  , ("hyphen", '-'), ("period", '.'), ("slash", '/'), ("zero", '0'), ("one", '1')
  , ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6')
  , ("seven", '7'), ("eight", '8'), ("nine", '9'), ("colon", ':')
  , ("semicolon", ';'), ("less", '<'), ("equal", '='), ("greater", '>')
  , ("question", '?'), ("at", '@'), ("bracketleft", '['), ("backslash", '\\')
  , ("bracketright", ']'), ("asciicircum", '^'), ("underscore", '_')
  , ("grave", '`'), ("braceleft", '{'), ("bar", '|'), ("braceright", '}')
  , ("asciitilde", '~'), ("quotedblleft", '\x201c'), ("quotedblright", '\x201d')
  , ("quotedblbase", '\x201e'), ("quotesinglbase", '\x201a'), ("bullet", '\x2022')
  , ("endash", '\x2013'), ("emdash", '\x2014'), ("fi", '\xfb01'), ("fl", '\xfb02')
  , ("ff", '\xfb00'), ("ffi", '\xfb03'), ("ffl", '\xfb04')
  , ("dagger", '\x2020'), ("daggerdbl", '\x2021'), ("ellipsis", '\x2026')
  , ("perthousand", '\x2030'), ("guilsinglleft", '\x2039'), ("guilsinglright", '\x203a')
  , ("guillemotleft", '\xab'), ("guillemotright", '\xbb'), ("trademark", '\x2122')
  , ("minus", '\x2212'), ("fraction", '\x2044'), ("florin", '\x192')
  , ("dotlessi", '\x131'), ("circumflex", '\x2c6'), ("tilde", '\x2dc')
  , ("macron", '\xaf'), ("breve", '\x2d8'), ("dotaccent", '\x2d9')
  , ("dieresis", '\xa8'), ("ring", '\x2da'), ("cedilla", '\xb8')
  , ("hungarumlaut", '\x2dd'), ("ogonek", '\x2db'), ("caron", '\x2c7')
  , ("exclamdown", '\xa1'), ("cent", '\xa2'), ("sterling", '\xa3')
  , ("currency", '\xa4'), ("yen", '\xa5'), ("brokenbar", '\xa6'), ("section", '\xa7')
  , ("copyright", '\xa9'), ("ordfeminine", '\xaa'), ("logicalnot", '\xac')
  , ("registered", '\xae'), ("degree", '\xb0'), ("plusminus", '\xb1')
  , ("mu", '\xb5'), ("paragraph", '\xb6'), ("periodcentered", '\xb7')
  , ("onesuperior", '\xb9'), ("ordmasculine", '\xba'), ("onequarter", '\xbc')
  , ("onehalf", '\xbd'), ("threequarters", '\xbe'), ("questiondown", '\xbf')
  , ("Agrave", '\xc0'), ("Aacute", '\xc1'), ("Acircumflex", '\xc2')
  , ("Atilde", '\xc3'), ("Adieresis", '\xc4'), ("Aring", '\xc5'), ("AE", '\xc6')
  , ("Ccedilla", '\xc7'), ("Egrave", '\xc8'), ("Eacute", '\xc9')
  , ("Ecircumflex", '\xca'), ("Edieresis", '\xcb'), ("Igrave", '\xcc')
  , ("Iacute", '\xcd'), ("Icircumflex", '\xce'), ("Idieresis", '\xcf')
  , ("Eth", '\xd0'), ("Ntilde", '\xd1'), ("Ograve", '\xd2'), ("Oacute", '\xd3')
  , ("Ocircumflex", '\xd4'), ("Otilde", '\xd5'), ("Odieresis", '\xd6')
  , ("multiply", '\xd7'), ("Oslash", '\xd8'), ("Ugrave", '\xd9'), ("Uacute", '\xda')
  , ("Ucircumflex", '\xdb'), ("Udieresis", '\xdc'), ("Yacute", '\xdd')
  , ("Thorn", '\xde'), ("germandbls", '\xdf'), ("agrave", '\xe0'), ("aacute", '\xe1')
  , ("acircumflex", '\xe2'), ("atilde", '\xe3'), ("adieresis", '\xe4')
  , ("aring", '\xe5'), ("ae", '\xe6'), ("ccedilla", '\xe7'), ("egrave", '\xe8')
  , ("eacute", '\xe9'), ("ecircumflex", '\xea'), ("edieresis", '\xeb')
  , ("igrave", '\xec'), ("iacute", '\xed'), ("icircumflex", '\xee')
  , ("idieresis", '\xef'), ("eth", '\xf0'), ("ntilde", '\xf1'), ("ograve", '\xf2')
  , ("oacute", '\xf3'), ("ocircumflex", '\xf4'), ("otilde", '\xf5')
  , ("odieresis", '\xf6'), ("divide", '\xf7'), ("oslash", '\xf8')
  , ("ugrave", '\xf9'), ("uacute", '\xfa'), ("ucircumflex", '\xfb')
  , ("udieresis", '\xfc'), ("yacute", '\xfd'), ("thorn", '\xfe')
  , ("ydieresis", '\xff'), ("Euro", '\x20ac'), ("Scaron", '\x160')
  , ("scaron", '\x161'), ("Zcaron", '\x17d'), ("zcaron", '\x17e')
  , ("Ydieresis", '\x178'), ("OE", '\x152'), ("oe", '\x153')
  ]

namedEncoding :: String -> IM.IntMap Char
namedEncoding n = case n of
  "WinAnsiEncoding"  -> winAnsiMap
  "MacRomanEncoding" -> macRomanMap
  _                  -> winAnsiMap

-- | WinAnsi is Windows-1252: Latin-1 apart from 0x80–0x9F, which is where the
-- curly quotes and dashes live — the bytes that decide whether a document's
-- punctuation reads correctly.
winAnsiMap :: IM.IntMap Char
winAnsiMap = IM.fromList $
  [ (i, chr i) | i <- [32 .. 126] ]
  ++ zip [128 ..] cp1252High
  ++ [ (i, chr i) | i <- [160 .. 255] ]

cp1252High :: String
cp1252High =
  "\x20ac\xfffd\x201a\x0192\x201e\x2026\x2020\x2021\
  \\x02c6\x2030\x0160\x2039\x0152\xfffd\x017d\xfffd\
  \\xfffd\x2018\x2019\x201c\x201d\x2022\x2013\x2014\
  \\x02dc\x2122\x0161\x203a\x0153\xfffd\x017e\x0178"

macRomanMap :: IM.IntMap Char
macRomanMap = IM.fromList $
  [ (i, chr i) | i <- [32 .. 126] ] ++ zip [128 ..] macRomanHigh

macRomanHigh :: String
macRomanHigh =
  "\xc4\xc5\xc7\xc9\xd1\xd6\xdc\xe1\xe0\xe2\xe4\xe3\xe5\xe7\xe9\xe8\
  \\xea\xeb\xed\xec\xee\xef\xf1\xf3\xf2\xf4\xf6\xf5\xfa\xf9\xfb\xfc\
  \\x2020\xb0\xa2\xa3\xa7\x2022\xb6\xdf\xae\xa9\x2122\xb4\xa8\x2260\xc6\xd8\
  \\x221e\xb1\x2264\x2265\xa5\xb5\x2202\x2211\x220f\x3c0\x222b\xaa\xba\x3a9\xe6\xf8\
  \\xbf\xa1\xac\x221a\x192\x2248\x2206\xab\xbb\x2026\xa0\xc0\xc3\xd5\x152\x153\
  \\x2013\x2014\x201c\x201d\x2018\x2019\xf7\x25ca\xff\x178\x2044\x20ac\x2039\x203a\xfb01\xfb02\
  \\x2021\xb7\x201a\x201e\x2030\xc2\xca\xc1\xcb\xc8\xcd\xce\xcf\xcc\xd3\xd4\
  \\xf8ff\xd2\xda\xdb\xd9\x131\x2c6\x2dc\xaf\x2d8\x2d9\x2da\xb8\x2dd\x2db\x2c7"

-- | Approximate metrics for the standard 14 fonts, which a file may use
-- without supplying @\/Widths@ at all. Getting these roughly right matters
-- more than it sounds: word breaks in this view are inferred from the gap
-- between one run's computed end and the next run's start, so a font whose
-- advance is guessed badly produces text with the spaces in the wrong places.
builtinWidths :: String -> IM.IntMap Double
builtinWidths face
  | "courier" `isInfixOf` face = IM.fromList [ (i, 600) | i <- [32 .. 255] ]
  | "times" `isInfixOf` face || "roman" `isInfixOf` face || "serif" `isInfixOf` face =
      IM.fromList (zip [32 ..] timesWidths)
  | otherwise = IM.fromList (zip [32 ..] helvWidths)

-- Advance widths for ASCII 32..126, in 1/1000 em (Adobe's AFM values).
helvWidths :: [Double]
helvWidths =
  [ 278,278,355,556,556,889,667,191,333,333,389,584,278,333,278,278
  , 556,556,556,556,556,556,556,556,556,556,278,278,584,584,584,556
  , 1015,667,667,722,722,667,611,778,722,278,500,667,556,833,722,778
  , 667,778,722,667,611,722,667,944,667,667,611,278,278,278,469,556
  , 333,556,556,500,556,556,278,556,556,222,222,500,222,833,556,556
  , 556,556,333,500,278,556,500,722,500,500,500,334,260,334,584 ]

timesWidths :: [Double]
timesWidths =
  [ 250,333,408,500,500,833,778,180,333,333,500,564,250,333,250,278
  , 500,500,500,500,500,500,500,500,500,500,278,278,564,564,564,444
  , 921,722,667,667,722,611,556,722,722,333,389,722,611,889,722,722
  , 556,722,667,556,611,722,722,944,722,722,611,333,278,333,469,500
  , 333,444,500,444,500,444,333,500,500,278,278,500,278,778,500,500
  , 500,500,333,389,278,500,500,722,500,500,444,480,200,480,541 ]

-- | Decode one PDF string into glyph codes paired with the text they draw.
decodeGlyphs :: FontInfo -> BS.ByteString -> [(Int, Text)]
decodeGlyphs fi s
  | fiTwoByte fi = map two (pairsOf (BS.unpack s))
  | otherwise = map one (BS.unpack s)
  where
    pairsOf (a : b : r) = (fromIntegral a * 256 + fromIntegral b) : pairsOf r
    pairsOf [a] = [fromIntegral a * 256]
    pairsOf [] = []
    two c = (c, expandLigatures (glyphText c))
    one b = let c = fromIntegral b in (c, expandLigatures (glyphText c))
    glyphText c = case IM.lookup c (fiToUni fi) of
      Just t  -> t
      Nothing -> case IM.lookup c (fiEnc fi) of
        Just ch -> T.singleton ch
        -- A composite font with no /ToUnicode is unreadable by construction
        -- (its codes are glyph indices in an embedded font programme); an
        -- unmapped simple code below 256 is far more likely to be Latin-1.
        Nothing | fiTwoByte fi -> T.empty
                | c >= 32 && c < 256 -> T.singleton (chr c)
                | otherwise -> T.empty

glyphWidth :: FontInfo -> Int -> Double
glyphWidth fi c = IM.findWithDefault (fiDefW fi) c (fiWidths fi)

-- | Spell the Latin typographic ligatures out.
--
-- A typeset document is full of them and they are what a @\/ToUnicode@ map
-- honestly reports, but they are also outside every terminal font's usual
-- coverage — leaving them in means a reader sees @rst@ with a hole in it
-- rather than @first@. The letters are what the document says; the ligature
-- was a typesetting decision for a page width this view is not using anyway.
expandLigatures :: Text -> Text
expandLigatures t
  | T.all (< '\xfb00') t = t
  | otherwise = T.concatMap one t
  where
    one c = case c of
      '\xfb00' -> "ff"
      '\xfb01' -> "fi"
      '\xfb02' -> "fl"
      '\xfb03' -> "ffi"
      '\xfb04' -> "ffl"
      '\xfb05' -> "st"
      '\xfb06' -> "st"
      _        -> T.singleton c

------------------------------------------------------------------------------
-- The content-stream interpreter

-- | @[a b c d e f]@, the affine matrix PDF writes as a six-element array.
type Mat = (Double, Double, Double, Double, Double, Double)

identityMat :: Mat
identityMat = (1, 0, 0, 1, 0, 0)

matMul :: Mat -> Mat -> Mat
matMul (a, b, c, d, e, f) (a', b', c', d', e', f') =
  ( a * a' + b * c'
  , a * b' + b * d'
  , c * a' + d * c'
  , c * b' + d * d'
  , e * a' + f * c' + e'
  , e * b' + f * d' + f' )

-- | A positioned run of text.
--
-- Both ends are kept, not a start and a width, because the advance direction
-- is whatever the text matrix says: a landscape page usually draws its text
-- rotated, and a run that advances along @y@ has no horizontal extent at all
-- to measure. 'rotateRun' maps both points into display space and normalises
-- them; everything downstream reads @rX0 <= rX1@ along the line and @rY@
-- growing downward.
data Run = Run
  { rX0   :: !Double
  , rY    :: !Double
  , rX1   :: !Double
  , rY1   :: !Double
  , rSize :: !Double
  , rTxt  :: !Text
  , rFmt  :: !PdfFmt
  }

data GState = GState
  { gCTM    :: !Mat
  , gFont   :: !(Maybe FontInfo)
  , gSize   :: !Double
  , gChar   :: !Double
  , gWord   :: !Double
  , gHScale :: !Double
  , gLead   :: !Double
  , gRise   :: !Double
  }

gInit :: GState
gInit = GState identityMat Nothing 0 0 0 1 0 0

data IState = IState
  { isG     :: !GState
  , isStack :: ![GState]
  , isTm    :: !Mat
  , isTlm   :: !Mat
  , isRuns  :: ![Run]        -- ^ Reversed.
  , isOps   :: !Int
  , isChars :: !Int
  }

data Tok = TObj !Obj | TOp !BS.ByteString

-- | Split a content stream into operands and operators.
--
-- Inline images (@BI … ID …binary… EI@) are the one construct that cannot be
-- tokenised as objects, since the bytes after @ID@ are raw samples that may
-- contain anything. They are skipped whole.
contentTokens :: BS.ByteString -> [Tok]
contentTokens bs = go 0
  where
    n = BS.length bs
    go !i
      | j >= n = []
      | startsObj (BS.index bs j) = case parseObj bs j of
          Just (o, k) | k > j -> TObj o : go k
          _ -> go (j + 1)
      | otherwise =
          let (kw, k) = readRegular bs j
          in if BS.null kw then go (j + 1)
             else if kw == BC.pack "BI" then go (skipInlineImage k)
             else TOp kw : go k
      where j = skipWs bs i
    startsObj c = c == 47 || c == 40 || c == 60 || c == 91
                    || (c >= 48 && c <= 57) || c == 43 || c == 45 || c == 46
    -- After ID comes one whitespace byte and then the samples; EI ends them.
    skipInlineImage i =
      let (_, post) = BS.breakSubstring (BC.pack "ID") (BS.drop i bs)
      in if BS.null post then n
         else findEI (i + (n - i - BS.length post) + 3)
    findEI !k
      | k >= n = n
      | BS.index bs k == 69 && byteAt bs (k + 1) == 73          -- "EI"
      , isWsB (byteAt bs (k - 1))
      , k + 2 >= n || isWsB (byteAt bs (k + 2)) = k + 2
      | otherwise = findEI (k + 1)

-- | Run a page's content, collecting positioned text runs.
runContent :: PdfFile -> Dict -> BS.ByteString -> [Run]
runContent f res0 dat = reverse (isRuns (exec 0 res0 (contentTokens dat) st0 []))
  where
    st0 = IState gInit [] identityMat identityMat [] 0 0

    exec :: Int -> Dict -> [Tok] -> IState -> [Obj] -> IState
    exec _ _ [] st _ = st
    exec depth res (TObj o : ts) st ops = exec depth res ts st (o : ops)
    exec depth res (TOp op : ts) st ops
      | isOps st > maxOpsPerPage || isChars st > maxPdfChars = st
      | otherwise =
          let st' = apply depth res (BC.unpack op) (reverse ops) st { isOps = isOps st + 1 }
          in exec depth res ts st' []

    num (ONum v) = v
    num _ = 0

    apply depth res op args st = case op of
      "q"  -> st { isStack = isG st : isStack st }
      "Q"  -> case isStack st of
                (g : gs) -> st { isG = g, isStack = gs }
                []       -> st
      "cm" | [a, b, c, d, e, ff] <- map num (take 6 args) ->
               st { isG = g { gCTM = matMul (a, b, c, d, e, ff) (gCTM g) } }
      "BT" -> st { isTm = identityMat, isTlm = identityMat }
      "ET" -> st
      "Tf" -> case args of
        [OName fn, sz] -> st { isG = g { gFont = Just (fontNamed res fn), gSize = num sz } }
        _ -> st
      "Td" | [tx, ty] <- map num (take 2 args) -> setLine (matMul (1, 0, 0, 1, tx, ty) (isTlm st)) st
      "TD" | [tx, ty] <- map num (take 2 args) ->
               setLine (matMul (1, 0, 0, 1, tx, ty) (isTlm st)) st { isG = g { gLead = -ty } }
      "Tm" | [a, b, c, d, e, ff] <- map num (take 6 args) -> setLine (a, b, c, d, e, ff) st
      "T*" -> nextLine st
      "TL" | (v : _) <- args -> st { isG = g { gLead = num v } }
      "Tc" | (v : _) <- args -> st { isG = g { gChar = num v } }
      "Tw" | (v : _) <- args -> st { isG = g { gWord = num v } }
      "Tz" | (v : _) <- args -> st { isG = g { gHScale = num v / 100 } }
      "Ts" | (v : _) <- args -> st { isG = g { gRise = num v } }
      "Tj" | (OStr s : _) <- args -> showStr s st
      "'"  | (OStr s : _) <- args -> showStr s (nextLine st)
      "\"" | [aw, ac, OStr s] <- args ->
               showStr s (nextLine st { isG = g { gWord = num aw, gChar = num ac } })
      "TJ" | (OArr xs : _) <- args -> foldl' tjItem st xs
      "Do" | (OName xn : _) <- args -> doXObject depth res xn st
      _ -> st
      where g = isG st

    setLine m st = st { isTm = m, isTlm = m }
    nextLine st = setLine (matMul (1, 0, 0, 1, 0, negate (gLead (isG st))) (isTlm st)) st

    tjItem st (OStr s) = showStr s st
    tjItem st (ONum v) =
      let gs = isG st
          tx = negate v / 1000 * gSize gs * gHScale gs
      in st { isTm = matMul (1, 0, 0, 1, tx, 0) (isTm st) }
    tjItem st _ = st

    -- Draw a string: one run, positioned by where the text matrix starts and
    -- ends. The advance is the sum of each glyph's width plus the character
    -- and word spacing, all scaled by the size and the horizontal scale —
    -- exactly the formula the spec gives, and the reason this view can tell
    -- an inter-word gap from a kerning adjustment.
    showStr s st = case gFont (isG st) of
      Nothing -> st
      Just fi ->
        let gs = isG st
            glyphs = decodeGlyphs fi s
            txt = T.concat (map snd glyphs)
            adv = sum [ (glyphWidth fi c * fiWScale fi * gSize gs
                          + gChar gs
                          + (if c == 32 && not (fiTwoByte fi) then gWord gs else 0))
                        * gHScale gs
                      | (c, _) <- glyphs ]
            base = matMul (isTm st) (gCTM gs)
            (a, b, c', d, e, ff) = base
            scaleY = sqrt (abs (c' * c' + d * d))
            scaleX = sqrt (abs (a * a + b * b))
            x0 = e; y0 = ff
            tm' = matMul (1, 0, 0, 1, adv, 0) (isTm st)
            (_, _, _, _, e2, f2) = matMul tm' (gCTM gs)
            fmt = PdfFmt (fiBold fi) (fiItalic fi) (fiMono fi) (gSize gs * scaleY)
            run = Run x0 y0 e2 f2 (max 0.01 (gSize gs * scaleY)) txt fmt
        in if T.null txt || T.all (== ' ') txt || scaleX <= 0
             then st { isTm = tm' }
             else st { isTm = tm'
                     , isRuns = run : isRuns st
                     , isChars = isChars st + T.length txt }

    -- A form XObject is a nested content stream with its own matrix and
    -- resources; images and anything else are skipped.
    doXObject depth res xn st
      | depth >= maxFormDepth = st
      | otherwise =
          let xobjs = dictOf f (lookD f res "XObject")
          in case resolve f (M.findWithDefault ONull xn xobjs) of
               OStream sd raw
                 | nameOf (lookD f sd "Subtype") == BC.pack "Form"
                 , Right body <- decodeStream f sd raw ->
                     let mtx = case map (numOf f . Just) (arrOf f (lookD f sd "Matrix")) of
                                 [a, b, c, d, e, ff] -> (a, b, c, d, e, ff)
                                 _ -> identityMat
                         inner = case lookD f sd "Resources" of
                                   ODict rd -> rd
                                   _        -> res
                         stIn = st { isG = (isG st) { gCTM = matMul mtx (gCTM (isG st)) }
                                   , isStack = [] }
                         stOut = exec (depth + 1) inner (contentTokens body) stIn []
                     in st { isRuns = isRuns stOut
                           , isOps = isOps stOut
                           , isChars = isChars stOut }
               _ -> st

    fontNamed res fn =
      let fonts = dictOf f (lookD f res "Font")
      in case M.lookup fn fonts of
           Just o  -> loadFont f (dictOf f o)
           Nothing -> emptyFont

------------------------------------------------------------------------------
-- Putting the page back together
--
-- This is the heuristic half. A PDF page is a bag of positioned glyph runs
-- with no lines, no paragraphs, no columns and no reading order; everything
-- below is about recovering those, and every threshold is expressed in ems so
-- it holds at any point size.

-- | One reconstructed line, before it becomes a paragraph.
data PLine = PLine
  { lY     :: !Double
  , lX0    :: !Double
  , lX1    :: !Double
  , lSize  :: !Double
  , lSpans :: ![PdfSpan]     -- ^ @psX@ still absolute at this stage.
  , lFixed :: !Bool          -- ^ Wide internal gaps: a table row, not prose.
  }

-- | Turn a page's runs into blocks, given the page box and its rotation.
assemblePage :: (Double, Double, Double, Double) -> Int -> [Run] -> [PdfPar]
assemblePage box rot runs0
  | null runs = []
  | otherwise = concatMap column columns
  where
    runs = map (rotateRun box rot) (filter (not . T.null . rTxt) runs0)
    (pw, ph) = pageSize box rot
    columns = splitColumns pw runs
    column rs =
      let ls = buildLines rs
          x0 = minimum (map lX0 ls)
          x1 = maximum (map lX1 ls)
          cw = max 1 (x1 - x0)
      in groupPars x0 cw (sortOn lY ls)
    _unused = ph

-- | Page width and height after rotation.
pageSize :: (Double, Double, Double, Double) -> Int -> (Double, Double)
pageSize (bx0, by0, bx1, by1) rot
  | rot == 90 || rot == 270 = (h, w)
  | otherwise = (w, h)
  where w = abs (bx1 - bx0); h = abs (by1 - by0)

-- | Move a run into a top-left origin, undoing @\/Rotate@ so a landscape page
-- reads in the right order. @rY@ afterwards grows downward.
--
-- Both endpoints go through the same point transform and are normalised
-- afterwards, so a run that advanced along @y@ in page space comes out with
-- the horizontal extent the assembly needs — see 'Run'.
rotateRun :: (Double, Double, Double, Double) -> Int -> Run -> Run
rotateRun (bx0, by0, bx1, by1) rot r =
  r { rX0 = min sx ex, rX1 = max sx ex, rY = sy, rY1 = ey }
  where
    w = abs (bx1 - bx0); h = abs (by1 - by0)
    (sx, sy) = place (rX0 r) (rY r)
    (ex, ey) = place (rX1 r) (rY1 r)
    -- Unrotated display coordinates: x from the left edge, y down from the
    -- top. /Rotate then turns that picture clockwise.
    place x y = let dx = x - bx0; dy = by1 - y in case rot of
      90  -> (h - dy, dx)
      180 -> (w - dx, h - dy)
      270 -> (dy, w - dx)
      _   -> (dx, dy)

-- | Split a page into columns at a vertical band no text crosses.
--
-- Runs wider than 60% of the page are left out of the occupancy test and
-- placed afterwards by their centre: a title spanning both columns would
-- otherwise fill the gutter and hide it, which is the failure mode that makes
-- naive column detection useless on real papers.
splitColumns :: Double -> [Run] -> [[Run]]
splitColumns pw runs = go 0 runs
  where
    nbins = 100 :: Int
    go :: Int -> [Run] -> [[Run]]
    go depth rs
      | depth >= 2 || length rs < 12 = [rs]
      | otherwise = case gapAt rs of
          Nothing -> [rs]
          Just cut ->
            let left = [ r | r <- rs, mid r < cut ]
                right = [ r | r <- rs, mid r >= cut ]
            in if length left < 5 || length right < 5
                 || length left * 5 < length rs || length right * 5 < length rs
                 then [rs]
                 else go (depth + 1) left ++ go (depth + 1) right
    mid r = (rX0 r + rX1 r) / 2
    gapAt rs =
      let narrow = [ r | r <- rs, rX1 r - rX0 r < 0.6 * pw ]
          xs = concatMap (\r -> [rX0 r, rX1 r]) narrow
      in if length narrow < 10 then Nothing else
        let lo = minimum xs; hi = maximum xs
            span' = max 1 (hi - lo)
            bin v = max 0 (min (nbins - 1) (floor ((v - lo) / span' * fromIntegral nbins)))
            filled = foldl' (\m r -> foldl' (flip (`IM.insert` True)) m
                                            [bin (rX0 r) .. bin (rX1 r)])
                            IM.empty narrow
            empties = [ i | i <- [0 .. nbins - 1], not (IM.member i filled) ]
            runsOf = groupRuns empties
            wide = [ g | g <- runsOf, length g >= 5
                   , head g > 8, last g < nbins - 9 ]
        in case wide of
             [] -> Nothing
             gs -> let g = last (sortOn length gs)
                       centre = fromIntegral (head g + last g) / 2
                   in Just (lo + centre / fromIntegral nbins * span')
    groupRuns [] = []
    groupRuns (x : xs) = let (a, b) = span2 (x + 1) xs in (x : a) : groupRuns b
      where span2 _ [] = ([], [])
            span2 k (y : ys) | y == k = let (p, q) = span2 (k + 1) ys in (y : p, q)
                             | otherwise = ([], y : ys)

-- | Cluster runs into lines by their baseline, then order each line by x and
-- join it into spans, inserting the spaces PDF does not record.
buildLines :: [Run] -> [PLine]
buildLines rs = map mkLine (cluster (sortOn rY rs))
  where
    cluster [] = []
    cluster (r : rest) = go [r] (rY r) (rSize r) rest
      where
        go acc y sz (x : xs)
          -- A baseline within about half an em of the running one belongs to
          -- the same line: superscripts, subscripts, inline maths and the
          -- separately-drawn halves of an accented glyph all sit off it.
          -- Comfortably under a line of leading either way, which is what
          -- stops this from swallowing the next line.
          | abs (rY x - y) <= max 0.5 (0.45 * sz) = go (x : acc) y (max sz (rSize x)) xs
          | otherwise = reverse acc : cluster (x : xs)
        go acc _ _ [] = [reverse acc]
    mkLine grp =
      let srt = sortOn rX0 grp
          sz = maximum (map rSize srt)
          (spans, fixed) = joinRuns sz srt
      in PLine { lY = minimum (map rY srt)
               , lX0 = minimum (map rX0 srt)
               , lX1 = maximum (map rX1 srt)
               , lSize = sz
               , lSpans = spans
               , lFixed = fixed }

-- | Join a line's runs, deciding at each junction between nothing (kerning),
-- one space (a word break) and a column boundary (which keeps the runs apart
-- and marks the line as positioned rather than prose).
joinRuns :: Double -> [Run] -> ([PdfSpan], Bool)
joinRuns sz (r : rest) = go (PdfSpan (rX0 r) (rTxt r) (rFmt r)) (rX1 r) rest [] False
  where
    wordGap = 0.22 * sz
    colGap = 1.6 * sz
    go cur _ [] acc fixed = (reverse (cur : acc), fixed)
    go cur end (x : xs) acc fixed
      | gap >= colGap =
          go (PdfSpan (rX0 x) (rTxt x) (rFmt x)) (rX1 x) xs (cur : acc) True
      | rFmt x /= psFmt cur =
          -- A format change ends the span, but the space (if any) belongs to
          -- the text that precedes it.
          let cur' = if needSpace then cur { psText = psText cur `T.snoc` ' ' } else cur
          in go (PdfSpan (rX0 x) (rTxt x) (rFmt x)) (rX1 x) xs (cur' : acc) fixed
      | otherwise =
          let sep = if needSpace then T.singleton ' ' else T.empty
          in go cur { psText = psText cur <> sep <> rTxt x } (max end (rX1 x)) xs acc fixed
      where
        gap = rX0 x - end
        needSpace = gap >= wordGap
                      && not (endsSpace (psText cur)) && not (startsSpace (rTxt x))
    endsSpace t = not (T.null t) && T.last t == ' '
    startsSpace t = not (T.null t) && T.head t == ' '
joinRuns _ [] = ([], False)

-- | Group lines into paragraphs and blank space.
--
-- A line joins the paragraph above it when the two are set the same way, the
-- leading between them is ordinary, and the line above ran to the right
-- margin — the last of which is what distinguishes wrapped prose from a
-- heading, a caption or the last line of a paragraph.
groupPars :: Double -> Double -> [PLine] -> [PdfPar]
groupPars colX0 colW ls0 = go ls0
  where
    -- Where the column's right margin is. A high percentile of where lines
    -- end, deliberately not the maximum: a page number or a running header
    -- sits out at the true page edge, and taking the maximum would mean no
    -- body line ever counts as "reaching the margin" — which is the one test
    -- that distinguishes a wrapped line from the last line of a paragraph.
    rightEdge = percentile 0.85 (map lX1 ls0)
    percentile p xs
      | null xs = 0
      | otherwise = let s = sortOn id xs
                        k = min (length s - 1) (floor (p * fromIntegral (length s)))
                    in s !! k
    frac v = (v - colX0) / colW
    -- The column's ordinary line spacing: the *most common* step between
    -- consecutive baselines, which is a far better signal for where a
    -- paragraph ends than anything derived from the font size. It has to be
    -- the mode and not the median — a page carrying several headings and
    -- paragraph breaks has enough wide steps to drag a median up past the
    -- body leading, and then every paragraph on it merges into one.
    leading =
      let dys = [ d | (a, b) <- zip ls0 (drop 1 ls0), let d = lY b - lY a, d > 0.5 ]
          tally = IM.fromListWith (+) [ (round d, 1 :: Int) | d <- dys ]
      in case sortOn (\(v, c) -> (negate c, v)) (IM.toList tally) of
           ((v, _) : _) -> fromIntegral v
           []           -> 0
    go [] = []
    go (l : ls)
      | lFixed l = fixedPar l : gap l ls
      | otherwise =
          let (body, rest) = grab l [l] ls
          in flowPar body : gap (last body) rest
    -- Collect the lines that continue this paragraph.
    grab prev acc (x : xs)
      | not (lFixed x)
      , dy > 0
      , sizeClose (lSize prev) (lSize x)
      , psFmtBold prev == psFmtBold x
      , lX0 x <= lX0 prev + 0.02 * colW
        -- Both signals must agree: the spacing is the column's ordinary
        -- leading (rather than the wider step a new paragraph is set with),
        -- and the line above ran to the right margin (rather than stopping
        -- short, the way the last line of a paragraph does).
      , dy <= leadTol (max (lSize prev) (lSize x))
      , lX1 prev >= rightEdge - 0.12 * colW
      = grab x (acc ++ [x]) xs
      where dy = lY x - lY prev
    grab _ acc rest = (acc, rest)
    -- A column of one or two lines has no leading to measure, so fall back
    -- to the point size there.
    leadTol sz = if leading > 0 then leading * 1.15 else 1.35 * sz
    sizeClose a b = abs (a - b) <= 0.15 * max a b
    psFmtBold l = case lSpans l of
      (s : _) -> pfBold (psFmt s)
      []      -> False
    -- Blank space between blocks, when the gap is noticeably more than one
    -- line's leading.
    gap prev rest = case rest of
      (x : _) | lY x - lY prev > max (leading * 1.6) (1.9 * max (lSize prev) (lSize x)) ->
        PdfPar PKBlank PAlignLeft 0 0 [] : go rest
      _ -> go rest

    fixedPar l = PdfPar PKFixed PAlignLeft 0 0
      [ s { psX = max 0 (min 0.98 (frac (psX s))) } | s <- lSpans l ]

    flowPar body =
      let firstL = head body
          margin = minimum (map lX0 body)
          runs = concatRuns body
          -- Centred and right-aligned blocks are recognised per line, and
          -- every line has to agree: a two-line paragraph whose short last
          -- line happens to balance is ordinary prose, not a centred block.
          align
            | all balanced body = PAlignCenter
            | length body == 1, leftPadOf firstL > 0.15, rightPadOf firstL < 0.03 = PAlignRight
            | otherwise = PAlignLeft
          leftPadOf l = frac (lX0 l)
          rightPadOf l = (rightEdge - lX1 l) / colW
          balanced l = leftPadOf l > 0.08 && rightPadOf l > 0.08
                         && abs (leftPadOf l - rightPadOf l) < 0.07
      in PdfPar { ppKind = PKFlow
                , ppAlign = align
                , ppMargin = clampFrac (if align == PAlignLeft then frac margin else 0)
                , ppFirst = if align == PAlignLeft
                              then clampFrac (frac (lX0 firstL)) - clampFrac (frac margin)
                              else 0
                , ppRuns = runs }
    clampFrac v = max 0 (min 0.35 v)

-- | Concatenate a paragraph's lines, restoring the space the line break stood
-- for — and undoing hyphenation, which is a line-breaking artefact of a page
-- width this view is not using.
concatRuns :: [PLine] -> [PdfSpan]
concatRuns = merge . foldr joinLine []
  where
    joinLine l acc = case acc of
      [] -> lSpans l
      _  -> lSpans l `bridge` acc
    bridge as bs = case (reverse as, bs) of
      (aLast : aRev, bFirst : bRest)
        | hyphenated (psText aLast) (psText bFirst) ->
            reverse (aLast { psText = T.init (psText aLast) } : aRev) ++ (bFirst : bRest)
        | otherwise ->
            reverse (aLast { psText = psText aLast `T.snoc` ' ' } : aRev) ++ bs
      _ -> as ++ bs
    hyphenated a b =
      not (T.null a) && T.last a == '-'
        && T.length a >= 2 && isLetter (T.index a (T.length a - 2))
        && not (T.null b) && isLetter (T.head b)
    isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c > '\xff'
    -- Adjacent spans that ended up with the same formatting are one span.
    merge (a : b : rest)
      | psFmt a == psFmt b = merge (a { psText = psText a <> psText b } : rest)
      | otherwise = a : merge (b : rest)
    merge xs = xs

------------------------------------------------------------------------------
-- Parsing a whole file

-- | Extract a readable document from PDF bytes.
--
-- The result is 'Left' only for a file this view cannot read at all (not a
-- PDF, encrypted, no pages); a file it reads *partially* comes back as a
-- document carrying a note, because half a document on screen beats an error
-- message.
parsePdf :: BS.ByteString -> Either String PdfDoc
parsePdf bs
  | not (sniffPdf bs) = Left "not a PDF"
  | BS.length bs > maxPdfBytes =
      Left ("PDF too large to read (" ++ show (BS.length bs `div` (1024 * 1024)) ++ " MB)")
  | M.member (BC.pack "Encrypt") trailer =
      Left "encrypted PDF \x2014 this view cannot decrypt it"
  | null pageDicts = Left "PDF has no pages"
  | otherwise = Right PdfDoc
      { pdPages = Seq.fromList pages
      , pdTop = 0
      , pdCache = Nothing
      , pdCaret = origin
      , pdAnchor = Nothing
      , pdTitle = title
      , pdVer = version
      , pdNote = note
      }
  where
    scanned = expandObjStms bs (scanObjects bs)
    objs = IM.map snd scanned
    trailer = findTrailer bs objs
    file = PdfFile objs trailer
    pageDicts = collectPages file
    truncated = length pageDicts >= maxPdfPages

    rawPages = goPages 1 0 pageDicts
    pages = markHeadings rawPages
    goPages _ _ [] = []
    goPages !k !chars (d : ds)
      | chars > maxPdfChars = []
      | otherwise =
          let pars = pageOf d
              n = sum [ T.length (psText s) | p <- pars, s <- ppRuns p ]
          in PdfPage k pars : goPages (k + 1) (chars + n) ds

    pageOf d =
      let res = dictOf file (M.findWithDefault ONull (BC.pack "Resources") d)
          box = mediaBox file d
          rot = ((intOf file (M.lookup (BC.pack "Rotate") d) `mod` 360) + 360) `mod` 360
          dat = contentsOf file d
      in assemblePage box rot (runContent file res dat)

    -- Body size is the size most of the document's characters are set in;
    -- anything meaningfully larger is a heading, which a terminal can only
    -- show as bold. (The same trick "Cmedit.Rtf" plays with \fs.) Measured
    -- before the marking pass, not after — reading it off the marked pages
    -- would be circular.
    body = bodySize rawPages
    markHeadings ps =
      [ p { pgPars = map markPar (pgPars p) } | p <- ps ]
      where
        markPar p = p { ppRuns = map markRun (ppRuns p) }
        markRun s
          | pfSize (psFmt s) >= body * 1.15 && pfSize (psFmt s) > body + 0.5 =
              s { psFmt = (psFmt s) { pfBold = True } }
          | otherwise = s

    title = case lookD file trailer "Info" of
      ODict info -> case lookD file info "Title" of
        OStr s -> textFromPdfString s
        _      -> T.empty
      _ -> T.empty

    version =
      let (_, post) = BS.breakSubstring (BC.pack "%PDF-") (BS.take 1024 bs)
      in T.pack (BC.unpack (BS.take 3 (BS.drop 5 post)))

    note
      | truncated = T.pack ("only the first " ++ show maxPdfPages ++ " pages are shown")
      | all (null . pgPars) pages =
          "no extractable text \x2014 the pages are images or use fonts without a Unicode map"
      | otherwise = T.empty

-- | A PDF text string: UTF-16BE when it carries the byte-order mark that says
-- so, PDFDocEncoding (near enough to Latin-1) otherwise.
textFromPdfString :: BS.ByteString -> Text
textFromPdfString s
  | BS.length s >= 2 && BS.index s 0 == 0xfe && BS.index s 1 == 0xff =
      decodeUtf16BE (BS.drop 2 s)
  | otherwise = T.pack (map w2c (BS.unpack s))

-- | The size most of the document's text is set in, weighted by how much text
-- is set in it. Rounded to the nearest half point so that a body face used at
-- 9.96pt and 10.0pt counts once.
bodySize :: [PdfPage] -> Double
bodySize ps
  | IM.null tally = 10
  | otherwise = fromIntegral (fst (last (sortOn snd (IM.toList tally)))) / 2
  where
    tally = foldl' add IM.empty
      [ (pfSize (psFmt s), T.length (psText s))
      | p <- ps, par <- pgPars p, s <- ppRuns par ]
    add m (sz, n) | sz <= 0 = m
                  | otherwise = IM.insertWith (+) (round (sz * 2)) n m

mediaBox :: PdfFile -> Dict -> (Double, Double, Double, Double)
mediaBox f d = case map (numOf f . Just) (arrOf f box) of
  [a, b, c, e] | abs (c - a) > 1 && abs (e - b) > 1 -> (min a c, min b e, max a c, max b e)
  _ -> (0, 0, 612, 792)   -- US Letter, PDF's default
  where
    box = case M.lookup (BC.pack "CropBox") d of
      Just b  -> resolve f b
      Nothing -> M.findWithDefault ONull (BC.pack "MediaBox") d

-- | A page's content: one stream, or an array of them that must be
-- concatenated (producers split content at arbitrary points, including in the
-- middle of an operator's operands).
contentsOf :: PdfFile -> Dict -> BS.ByteString
contentsOf f d = BS.intercalate (BC.pack "\n") (mapMaybe one streams)
  where
    streams = case M.findWithDefault ONull (BC.pack "Contents") d of
      OArr xs -> map (resolve f) xs
      o       -> [resolve f o]
    one (OStream sd raw) = either (const Nothing) Just (decodeStream f sd raw)
    one _ = Nothing

------------------------------------------------------------------------------
-- Layout

-- | Lay the extracted pages out for a terminal width, returning the lines and
-- the index at which each page starts (which is what page navigation moves
-- between).
--
-- Cost is linear in the document and it runs once per width — a resize — not
-- per frame or per scroll.
layoutPdf :: Int -> Int -> Seq PdfPage -> (Seq PdfLine, Seq Int)
layoutPdf tabw width pages
  | width <= 0 = (Seq.empty, Seq.empty)
  | otherwise = foldl' onePage (Seq.empty, Seq.empty) (toList pages)
  where
    onePage (acc, starts) pg =
      let sep = if Seq.null acc then Seq.empty
                  else Seq.fromList [ PdfLine 0 T.empty [] True (pgNumber pg) ]
          acc1 = acc <> sep
          body = Seq.fromList (concatMap (parLines (pgNumber pg)) (pgPars pg))
      in (acc1 <> body, starts |> Seq.length acc1)

    clampPad = max 0 . min (max 0 (width - 8))

    parLines page p = case ppKind p of
      PKBlank -> [ PdfLine 0 T.empty [] False page ]
      PKRule  -> [ PdfLine 0 T.empty [] True page ]
      PKFixed -> [ fixedLine page p ]
      PKFlow  -> flowLines page p

    -- A positioned line: each span goes where its fraction of the width says,
    -- never overlapping the one before it.
    fixedLine page p =
      let (txt, spans) = place 0 T.empty [] (ppRuns p)
      in PdfLine 0 txt spans False page
      where
        place _ txt spans [] = (txt, reverse spans)
        place !col txt spans (s : ss) =
          let want = max col (round (psX s * fromIntegral width))
              padN = max (if col == 0 then 0 else 1) (want - col)
              txt' = txt <> T.replicate padN (T.singleton ' ') <> psText s
              st = T.length txt + padN
              en = st + T.length (psText s)
          in if st >= width then (txt, reverse spans)
             else place en txt' ((st, min width en, psFmt s) : spans) ss

    flowLines page p =
      let (txt, spans) = flattenRuns (ppRuns p)
          margin = clampPad (round (ppMargin p * fromIntegral width))
          firstIn = clampPad (margin + round (ppFirst p * fromIntegral width))
          avail = max 1 (width - margin)
          segs = wrapLine tabw avail txt
      in [ mkLine page p (if k == (0 :: Int) then firstIn else margin) avail txt spans s e
         | (k, (s, e)) <- zip [0 ..] segs ]

    mkLine page p pad avail txt spans s e =
      let seg = T.take (e - s) (T.drop s txt)
          sp = sliceSpans s e spans
          wdt = lineDisplayWidth tabw seg
          extra = case ppAlign p of
                    PAlignCenter -> max 0 (avail - wdt) `div` 2
                    PAlignRight  -> max 0 (avail - wdt)
                    PAlignLeft   -> 0
      in PdfLine (pad + extra) seg sp False page

-- | Concatenate a paragraph's runs into one text plus the character ranges
-- each run occupies (the shape the renderer's per-character style lookup
-- wants).
flattenRuns :: [PdfSpan] -> (Text, [(Int, Int, PdfFmt)])
flattenRuns runs = (T.concat (map psText runs), go 0 runs)
  where
    go _ [] = []
    go !i (r : rs) =
      let n = T.length (psText r)
      in if n == 0 then go i rs else (i, i + n, psFmt r) : go (i + n) rs

sliceSpans :: Int -> Int -> [(Int, Int, PdfFmt)] -> [(Int, Int, PdfFmt)]
sliceSpans s e spans =
  [ (max 0 (a - s), min (e - s) (b - s), f)
  | (a, b, f) <- spans, b > s, a < e ]

------------------------------------------------------------------------------
-- The view

-- | Lay the document out for this width and tab size if that is not what the
-- cache already holds, and re-clamp the scroll position to the result.
pdfRelayout :: Int -> Int -> Int -> PdfDoc -> PdfDoc
pdfRelayout tabw width height pd
  | Just (w, tw, _, _) <- pdCache pd, w == width, tw == tabw = pd
  | width <= 0 = pd
  | otherwise =
      let (ls, starts) = layoutPdf tabw width (pdPages pd)
      in pdfClamp height pd { pdCache = Just (width, tabw, ls, starts)
                            -- A selection is a pair of indices into the old
                            -- layout; after a re-wrap they point at different
                            -- text, so they go rather than lie.
                            , pdCaret = origin, pdAnchor = Nothing }

pdfLines :: PdfDoc -> Seq PdfLine
pdfLines pd = case pdCache pd of
  Just (_, _, ls, _) -> ls
  Nothing            -> Seq.empty

pdfPageStarts :: PdfDoc -> Seq Int
pdfPageStarts pd = case pdCache pd of
  Just (_, _, _, st) -> st
  Nothing            -> Seq.empty

pdfLineCount :: PdfDoc -> Int
pdfLineCount = Seq.length . pdfLines

-- | First laid-out line of page @n@ (1-based).
--
-- Not @pdTop@ after a 'pdfGoToPage': that is the scroll position, which
-- 'pdfClamp' pulls back on the last pages of a document. See
-- 'Cmedit.Rtf.rtfSectionLine' for the same distinction.
pdfPageLine :: Int -> PdfDoc -> Int
pdfPageLine n pd =
  fromMaybe 0 (Seq.lookup (max 0 (n - 1)) (pdfPageStarts pd))

pdfPageCount :: PdfDoc -> Int
pdfPageCount = Seq.length . pdPages

------------------------------------------------------------------------------
-- Selection
--
-- The model is the text view's, minus everything that writes: a caret and an
-- optional anchor. It exists so that a passage can be copied out — looking
-- something up in a manual and pasting the answer somewhere else is most of
-- what a PDF gets opened for — and for the Find command to have somewhere to
-- put a hit. Nothing here can modify the document; there is no document to
-- modify, only the extracted text.

-- | The active selection as an ordered pair, or 'Nothing' when the caret and
-- anchor coincide (or there is no anchor at all).
pdfSelection :: PdfDoc -> Maybe (Pos, Pos)
pdfSelection pd = case pdAnchor pd of
  Just a | a /= pdCaret pd -> Just (if a <= pdCaret pd then (a, pdCaret pd) else (pdCaret pd, a))
  _ -> Nothing

pdfLineText :: PdfDoc -> Int -> Text
pdfLineText pd i = maybe T.empty plText (Seq.lookup i (pdfLines pd))

-- | Clamp a position onto the laid-out document.
pdfClampPos :: PdfDoc -> Pos -> Pos
pdfClampPos pd (Pos l c) =
  let n = pdfLineCount pd
      l' = max 0 (min (max 0 (n - 1)) l)
  in Pos l' (max 0 (min (T.length (pdfLineText pd l')) c))

-- | The selected text: the visible lines, sliced and joined.
--
-- What you see is what you copy — these are the reflowed lines on screen, not
-- the source paragraph, because the line breaks on screen are the only ones
-- this view can honestly claim the document has.
pdfSelText :: PdfDoc -> Text
pdfSelText pd = case pdfSelection pd of
  Nothing -> T.empty
  Just (Pos sl sc, Pos el ec)
    | sl == el  -> slice sl sc ec
    | otherwise -> T.intercalate (T.singleton '\n')
        ( slice sl sc maxBound
        : [ pdfLineText pd i | i <- [sl + 1 .. el - 1] ]
          ++ [ slice el 0 ec ] )
  where slice i a b = let t = pdfLineText pd i
                      in T.take (max 0 (min (T.length t) b - a)) (T.drop a t)

pdfSetCaret :: Pos -> PdfDoc -> PdfDoc
pdfSetCaret p pd = pd { pdCaret = pdfClampPos pd p, pdAnchor = Nothing }

-- | Select @[a, b)@, leaving the caret at @b@.
pdfSelectRange :: Pos -> Pos -> PdfDoc -> PdfDoc
pdfSelectRange a b pd = pd { pdAnchor = Just (pdfClampPos pd a), pdCaret = pdfClampPos pd b }

pdfSelectAll :: PdfDoc -> PdfDoc
pdfSelectAll pd
  | pdfLineCount pd == 0 = pd
  | otherwise =
      let l = pdfLineCount pd - 1
      in pd { pdAnchor = Just origin, pdCaret = Pos l (T.length (pdfLineText pd l)) }

pdfClearSel :: PdfDoc -> PdfDoc
pdfClearSel pd = pd { pdAnchor = Nothing }

-- | Move the caret, starting a selection from where it was if there is not
-- one already — the Shift+movement rule, in one place.
pdfExtendTo :: (PdfDoc -> PdfDoc) -> PdfDoc -> PdfDoc
pdfExtendTo move pd =
  let anchored = case pdAnchor pd of
                   Just _  -> pd
                   Nothing -> pd { pdAnchor = Just (pdCaret pd) }
  in move anchored

-- | The position under a cell, given the line it falls on and a display
-- column measured from the left edge of the text area. The line's leading pad
-- (its indent and alignment) is not text, so a click in it lands at column 0.
pdfPosAtCell :: Int -> Int -> Int -> PdfDoc -> Pos
pdfPosAtCell tabw line dcol pd =
  let Pos l _ = pdfClampPos pd (Pos line 0)
      pad = maybe 0 plPad (Seq.lookup l (pdfLines pd))
      txt = pdfLineText pd l
  in Pos l (displayToCol tabw (max 0 (dcol - pad)) txt)

-- | The inverse: which display column a position sits at, pad included. The
-- renderer and the cursor placement share it.
pdfCellOfPos :: Int -> Pos -> PdfDoc -> Int
pdfCellOfPos tabw (Pos l c) pd =
  let pad = maybe 0 plPad (Seq.lookup l (pdfLines pd))
  in pad + colToDisplay tabw c (pdfLineText pd l)

-- | The word around a position (a double-click), or the position twice when
-- it is not on one.
pdfWordRange :: Pos -> PdfDoc -> (Pos, Pos)
pdfWordRange p@(Pos l c) pd
  | T.null txt = (p, p)
  | c >= T.length txt || not (wordChar (T.index txt (min c (T.length txt - 1)))) = (p, p)
  | otherwise = (Pos l a, Pos l b)
  where
    txt = pdfLineText pd l
    a = c - length (takeWhile wordChar (reverse (T.unpack (T.take c txt))))
    b = c + length (takeWhile wordChar (T.unpack (T.drop c txt)))

pdfLineRange :: Pos -> PdfDoc -> (Pos, Pos)
pdfLineRange (Pos l _) pd = (Pos l 0, Pos l (T.length (pdfLineText pd l)))

wordChar :: Char -> Bool
wordChar ch = ch == '_' || ch > '\x7f'
                || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
                || (ch >= '0' && ch <= '9')

-- Caret movement. Each moves only the caret; the caller decides whether the
-- anchor comes along ('pdfExtendTo') or is dropped.
pdfCaretLeft, pdfCaretRight, pdfCaretUp, pdfCaretDown :: PdfDoc -> PdfDoc
pdfCaretLeft pd = case pdCaret pd of
  Pos l 0 | l > 0 -> pd { pdCaret = Pos (l - 1) (T.length (pdfLineText pd (l - 1))) }
  Pos l c -> pd { pdCaret = Pos l (max 0 (c - 1)) }
pdfCaretRight pd = case pdCaret pd of
  Pos l c | c >= T.length (pdfLineText pd l), l < pdfLineCount pd - 1 -> pd { pdCaret = Pos (l + 1) 0 }
          | otherwise -> pd { pdCaret = pdfClampPos pd (Pos l (c + 1)) }
pdfCaretUp pd = let Pos l c = pdCaret pd in pd { pdCaret = pdfClampPos pd (Pos (l - 1) c) }
pdfCaretDown pd = let Pos l c = pdCaret pd in pd { pdCaret = pdfClampPos pd (Pos (l + 1) c) }

pdfCaretHome, pdfCaretEnd, pdfCaretTop, pdfCaretBottom :: PdfDoc -> PdfDoc
pdfCaretHome pd = let Pos l _ = pdCaret pd in pd { pdCaret = Pos l 0 }
pdfCaretEnd pd = let Pos l _ = pdCaret pd
                 in pd { pdCaret = Pos l (T.length (pdfLineText pd l)) }
pdfCaretTop pd = pd { pdCaret = origin }
pdfCaretBottom pd = pdfClampCaret pd { pdCaret = Pos (max 0 (pdfLineCount pd - 1)) maxBound }

pdfClampCaret :: PdfDoc -> PdfDoc
pdfClampCaret pd = pd { pdCaret = pdfClampPos pd (pdCaret pd) }

-- | Scroll the window so the caret is on it, moving as little as possible.
pdfScrollToCaret :: Int -> PdfDoc -> PdfDoc
pdfScrollToCaret height pd
  | l < pdTop pd = pdfClamp height pd { pdTop = l }
  | l >= pdTop pd + h = pdfClamp height pd { pdTop = l - h + 1 }
  | otherwise = pd
  where Pos l _ = pdCaret pd
        h = max 1 height

------------------------------------------------------------------------------

-- | Scroll by @d@ lines, keeping at least one line on screen.
pdfScroll :: Int -> Int -> PdfDoc -> PdfDoc
pdfScroll height d pd = pdfClamp height pd { pdTop = pdTop pd + d }

pdfGoTop :: PdfDoc -> PdfDoc
pdfGoTop pd = pd { pdTop = 0 }

pdfGoBottom :: Int -> PdfDoc -> PdfDoc
pdfGoBottom height pd = pdfClamp height pd { pdTop = pdfLineCount pd }

pdfClamp :: Int -> PdfDoc -> PdfDoc
pdfClamp height pd =
  pd { pdTop = max 0 (min (max 0 (pdfLineCount pd - max 1 height)) (pdTop pd)) }

-- | Which page the top of the window is showing (1-based).
pdfCurrentPage :: PdfDoc -> Int
pdfCurrentPage pd =
  let starts = pdfPageStarts pd
      before = Seq.length (Seq.takeWhileL (<= pdTop pd) starts)
  in max 1 (min (max 1 (pdfPageCount pd)) before)

-- | Scroll so page @n@ (1-based) starts at the top of the window.
pdfGoToPage :: Int -> Int -> PdfDoc -> PdfDoc
pdfGoToPage height n pd =
  let k = max 0 (min (pdfPageCount pd - 1) (n - 1))
  in case Seq.lookup k (pdfPageStarts pd) of
       Just top -> pdfClamp height pd { pdTop = top }
       Nothing  -> pd

pdfNextPage :: Int -> PdfDoc -> PdfDoc
pdfNextPage height pd = pdfGoToPage height (pdfCurrentPage pd + 1) pd

-- | Back one page — or to the top of the current one when the window has
-- scrolled into its middle, which is what a reader means by "back".
pdfPrevPage :: Int -> PdfDoc -> PdfDoc
pdfPrevPage height pd =
  let cur = pdfCurrentPage pd
      start = fromMaybe 0 (Seq.lookup (cur - 1) (pdfPageStarts pd))
  in if pdTop pd > start then pdfGoToPage height cur pd
     else pdfGoToPage height (cur - 1) pd

-- | The status-bar text: where you are in the document, and that this is a
-- read-only rendering rather than the file's bytes.
pdfStatus :: PdfDoc -> String
pdfStatus pd =
  "Page " ++ show (pdfCurrentPage pd) ++ " of " ++ show (pdfPageCount pd)
    ++ "   Ln " ++ show (pdTop pd + 1) ++ " of " ++ show (pdfLineCount pd)
    ++ "   PDF "

-- | The whole document as plain text: one line per reconstructed block, in
-- reading order, with pages separated by a blank line.
--
-- The paragraphs rather than the laid-out lines, for the reason
-- 'Cmedit.Rtf.rtfPlainText' gives: a file wrapped to the width the terminal
-- happened to be is a worse artifact than one the reader can wrap itself.
pdfPlainText :: PdfDoc -> Text
pdfPlainText pd = T.unlines (concatMap pageLines (toList (pdPages pd)))
  where
    pageLines pg = map parLine (pgPars pg) ++ [T.empty]
    parLine p | ppKind p == PKBlank = T.empty
              | otherwise = T.concat (map psText (ppRuns p))
