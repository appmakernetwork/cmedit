-- | The text buffer and all pure operations over it. Lines are stored in a
-- 'Seq' of 'Text' (a persistent structure, so undo snapshots share unchanged
-- lines for free). Nothing here does any IO except the file load/save helpers
-- at the bottom.
{-# LANGUAGE MagicHash #-}
module Cmedit.TextBuffer
  ( -- * Buffer
    Buffer(bufLines, bufChars)
  , emptyBuffer
  , fromText
  , bufferToText
  , lineCount
  , getLine'
  , lineLen
  , isEmptyBuffer
    -- * Positions
  , clampPos
  , clampCol
  , endPos
  , posLE
    -- * Editing (pure)
  , insertChar
  , overwriteChar
  , insertText
  , splitLineAt
  , deleteBackward
  , deleteForward
  , deleteRange
  , textInRange
  , trimTrailingWs
    -- * Movement (pure, character-based)
  , moveLeft
  , moveRight
  , lineStart
  , lineEnd
  , docStart
  , docEnd
  , wordLeft
  , wordRight
  , wordRangeAt
  , lineRangeAt
  , matchBracket
    -- * Files
  , LineEnding(..)
  , Encoding(..)
  , lineEndingText
  , detectLineEnding
  , LoadResult(..)
  , emptyLoadResult
  , loadFile
  , loadFromBytes
  , looksBinary
  , canWrite
  , saveFile
  , replaceInFile
  , fileMtime
  ) where

import Control.Exception (IOException, SomeException, try)
import System.IO.Error (isPermissionError, isFullError, isAlreadyInUseError, ioeGetErrorString)
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (foldl', toList)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory (Permissions, doesFileExist, removeFile, renameFile, getPermissions, writable)
import System.IO (withBinaryFile, IOMode(WriteMode), hFlush)
import System.Posix.Files (FileStatus, getFileStatus, modificationTime)
import System.Posix.Types (EpochTime)

import Cmedit.Types (Pos(..), origin)

-- | The text content: a non-empty sequence of lines (newlines are implicit)
-- plus the total character count across all lines, maintained incrementally
-- by every edit so size fingerprints (the modified-flag check) are O(1) on
-- buffers of any size.
data Buffer = Buffer { bufLines :: !(Seq Text), bufChars :: !Int }
  deriving (Show)

-- Equality first compares the O(1) sizes, then the lines — with a pointer
-- shortcut per line, so two buffers that share most lines (an edit and its
-- undo snapshot) compare in one pointer test per unchanged line.
instance Eq Buffer where
  a == b = bufChars a == bufChars b
           && Seq.length (bufLines a) == Seq.length (bufLines b)
           && and (zipWith sameText (toList (bufLines a)) (toList (bufLines b)))

-- GC can move objects between comparisons, so a pointer mismatch proves
-- nothing (fall back to (==)) — but a pointer match is a sound "equal".
sameText :: Text -> Text -> Bool
sameText x y = isTrue# (reallyUnsafePtrEquality# x y) || x == y

-- Build a buffer from scratch, counting its characters (the only O(n) path;
-- edits below all adjust the count incrementally).
mkBuffer :: Seq Text -> Buffer
mkBuffer ls = Buffer ls (foldl' (\n t -> n + T.length t) 0 ls)

emptyBuffer :: Buffer
emptyBuffer = Buffer (Seq.singleton T.empty) 0

-- | True when the buffer holds nothing but a single empty line.
isEmptyBuffer :: Buffer -> Bool
isEmptyBuffer b = bufChars b == 0 && Seq.length (bufLines b) == 1

lineCount :: Buffer -> Int
lineCount = Seq.length . bufLines

-- | Total line of text at an index, or empty if out of range.
getLine' :: Int -> Buffer -> Text
getLine' i (Buffer ls _)
  | i >= 0 && i < Seq.length ls = Seq.index ls i
  | otherwise                   = T.empty

lineLen :: Int -> Buffer -> Int
lineLen i b = T.length (getLine' i b)

-- | Position just past the last character of the buffer.
endPos :: Buffer -> Pos
endPos b = let l = lineCount b - 1 in Pos l (lineLen l b)

-- | Total ordering on positions (line then column).
posLE :: Pos -> Pos -> Bool
posLE a b = (posLine a, posCol a) <= (posLine b, posCol b)

------------------------------------------------------------------------------
-- Clamping

clampPos :: Pos -> Buffer -> Pos
clampPos (Pos l c) b =
  let l' = max 0 (min l (lineCount b - 1))
      c' = max 0 (min c (lineLen l' b))
  in Pos l' c'

clampCol :: Int -> Int -> Buffer -> Int
clampCol l c b = max 0 (min c (lineLen l b))

------------------------------------------------------------------------------
-- Construction / serialisation

fromText :: Text -> Buffer
fromText t = mkBuffer (fst (splitContent t))

bufferToText :: LineEnding -> Bool -> Buffer -> Text
bufferToText le final (Buffer ls _) =
  let sep    = lineEndingText le
      joined = T.intercalate sep (toList ls)
  in if final then joined <> sep else joined

-- Split raw text into lines plus a flag for whether it ended with a newline.
splitContent :: Text -> (Seq Text, Bool)
splitContent t0 =
  let t        = normalizeNewlines t0
      hadFinal = not (T.null t) && T.last t == '\n'
      body     = if hadFinal then T.init t else t
      ls       = T.splitOn (T.pack "\n") body
      ls'      = if null ls then [T.empty] else ls
  in (Seq.fromList ls', hadFinal)

normalizeNewlines :: Text -> Text
normalizeNewlines =
  T.replace (T.pack "\r") (T.pack "\n") . T.replace (T.pack "\r\n") (T.pack "\n")

------------------------------------------------------------------------------
-- Editing

withLine :: Int -> (Text -> Text) -> Buffer -> Buffer
withLine i f b@(Buffer ls n)
  | i >= 0 && i < Seq.length ls =
      let old = Seq.index ls i
          new = f old
      in Buffer (Seq.update i new ls) (n + T.length new - T.length old)
  | otherwise = b

-- | Insert a single (non-newline) character, returning the new cursor.
insertChar :: Pos -> Char -> Buffer -> (Buffer, Pos)
insertChar pos ch b =
  let Pos l c = clampPos pos b
      b'      = withLine l (\t -> T.take c t <> T.singleton ch <> T.drop c t) b
  in (b', Pos l (c + 1))

-- | Overwrite the character at the cursor (Insert/overwrite mode). If the
-- cursor is at end of line this behaves like 'insertChar'.
overwriteChar :: Pos -> Char -> Buffer -> (Buffer, Pos)
overwriteChar pos ch b =
  let Pos l c = clampPos pos b
      b'      = withLine l (\t -> T.take c t <> T.singleton ch <> T.drop (c + 1) t) b
  in (b', Pos l (c + 1))

-- | Insert arbitrary text (which may contain newlines, e.g. a paste).
insertText :: Pos -> Text -> Buffer -> (Buffer, Pos)
insertText pos txt b
  | T.null txt = (b, clampPos pos b)
  | otherwise =
      let Pos l c   = clampPos pos b
          line      = getLine' l b
          before    = T.take c line
          after     = T.drop c line
          segs      = T.splitOn (T.pack "\n") (normalizeNewlines txt)
      in case segs of
           [single] ->
             ( withLine l (const (before <> single <> after)) b
             , Pos l (c + T.length single) )
           _ ->
             let firstL  = before <> head segs
                 lastSeg = last segs
                 midL    = init (tail segs)              -- middle whole lines
                 newLast = lastSeg <> after
                 inserted = Seq.fromList (firstL : midL ++ [newLast])
                 (pre, post) = Seq.splitAt l (bufLines b)
                 rest        = Seq.drop 1 post
                 ls'         = pre >< inserted >< rest
                 added       = foldl' (\n t -> n + T.length t) 0 inserted
                                 - T.length line
             in ( Buffer ls' (bufChars b + added)
                , Pos (l + length segs - 1) (T.length lastSeg) )

-- | Break the current line at the cursor (the Enter key).
splitLineAt :: Pos -> Buffer -> (Buffer, Pos)
splitLineAt pos b =
  let Pos l c   = clampPos pos b
      line      = getLine' l b
      before    = T.take c line
      after     = T.drop c line
      (pre, post) = Seq.splitAt l (bufLines b)
      rest        = Seq.drop 1 post
      ls'         = pre >< Seq.fromList [before, after] >< rest
  in (Buffer ls' (bufChars b), Pos (l + 1) 0)

-- | Delete the character before the cursor (Backspace).
deleteBackward :: Pos -> Buffer -> (Buffer, Pos)
deleteBackward pos b =
  let Pos l c = clampPos pos b
  in if c > 0
       then ( withLine l (\t -> T.take (c - 1) t <> T.drop c t) b
            , Pos l (c - 1) )
       else if l > 0
         then let prevLen = lineLen (l - 1) b
                  cur     = getLine' l b
                  b'      = joinLineWithPrev l cur b
              in (b', Pos (l - 1) prevLen)
         else (b, Pos 0 0)

-- | Delete the character at the cursor (Delete / Del).
deleteForward :: Pos -> Buffer -> (Buffer, Pos)
deleteForward pos b =
  let Pos l c = clampPos pos b
      len     = lineLen l b
  in if c < len
       then ( withLine l (\t -> T.take c t <> T.drop (c + 1) t) b
            , Pos l c )
       else if l < lineCount b - 1
         then let nextLine = getLine' (l + 1) b
                  merged   = getLine' l b <> nextLine
                  (pre, post) = Seq.splitAt l (bufLines b)
                  rest        = Seq.drop 2 post
                  ls'         = pre >< Seq.singleton merged >< rest
              in (Buffer ls' (bufChars b), Pos l c)
         else (b, Pos l c)

-- Merge line l into l-1 (used by backspace at column 0).
joinLineWithPrev :: Int -> Text -> Buffer -> Buffer
joinLineWithPrev l cur b =
  let prev        = getLine' (l - 1) b
      merged      = prev <> cur
      (pre, post) = Seq.splitAt (l - 1) (bufLines b)
      rest        = Seq.drop 2 post
  in Buffer (pre >< Seq.singleton merged >< rest) (bufChars b)

-- | Delete an arbitrary (normalised) range; the cursor lands at the start.
deleteRange :: Pos -> Pos -> Buffer -> (Buffer, Pos)
deleteRange a0 b0 b =
  let (a, c) = order a0 b0
      Pos la ca = clampPos a b
      Pos lc cc = clampPos c b
  in if la == lc
       then ( withLine la (\t -> T.take ca t <> T.drop cc t) b
            , Pos la ca )
       else
         let firstPart  = T.take ca (getLine' la b)
             lastPart   = T.drop cc (getLine' lc b)
             merged     = firstPart <> lastPart
             (pre, post) = Seq.splitAt la (bufLines b)
             rest        = Seq.drop (lc - la + 1) post
             ls'         = pre >< Seq.singleton merged >< rest
             removed     = sum [ lineLen i b | i <- [la .. lc] ]
         in (Buffer ls' (bufChars b + T.length merged - removed), Pos la ca)

-- | Strip trailing whitespace from every line, or 'Nothing' when no line has
-- any (so callers can skip the undo checkpoint). Unchanged lines keep their
-- original 'Text' object, preserving sharing with undo snapshots.
trimTrailingWs :: Buffer -> Maybe Buffer
trimTrailingWs (Buffer ls n) =
  let step (!removed, acc) t =
        let t' = T.stripEnd t
            d = T.length t - T.length t'
        in if d == 0 then (removed, acc Seq.|> t) else (removed + d, acc Seq.|> t')
      (removed, ls') = foldl' step (0, Seq.empty) ls
  in if removed == 0 then Nothing else Just (Buffer ls' (n - removed))

-- | The text contained in a range (for copy/cut).
textInRange :: Pos -> Pos -> Buffer -> Text
textInRange a0 b0 b =
  let (a, c) = order a0 b0
      Pos la ca = clampPos a b
      Pos lc cc = clampPos c b
  in if la == lc
       then T.take (cc - ca) (T.drop ca (getLine' la b))
       else
         let firstL  = T.drop ca (getLine' la b)
             midLs   = [ getLine' i b | i <- [la + 1 .. lc - 1] ]
             lastL   = T.take cc (getLine' lc b)
         in T.intercalate (T.pack "\n") (firstL : midLs ++ [lastL])

order :: Pos -> Pos -> (Pos, Pos)
order a b = if posLE a b then (a, b) else (b, a)

------------------------------------------------------------------------------
-- Character-based movement

moveLeft :: Pos -> Buffer -> Pos
moveLeft pos b =
  let Pos l c = clampPos pos b
  in if c > 0 then Pos l (c - 1)
     else if l > 0 then Pos (l - 1) (lineLen (l - 1) b)
     else Pos 0 0

moveRight :: Pos -> Buffer -> Pos
moveRight pos b =
  let Pos l c = clampPos pos b
  in if c < lineLen l b then Pos l (c + 1)
     else if l < lineCount b - 1 then Pos (l + 1) 0
     else Pos l c

lineStart :: Pos -> Pos
lineStart (Pos l _) = Pos l 0

lineEnd :: Pos -> Buffer -> Pos
lineEnd (Pos l _) b = Pos l (lineLen l b)

docStart :: Pos
docStart = origin

docEnd :: Buffer -> Pos
docEnd = endPos

data CharClass = CCSpace | CCWord | CCPunct deriving (Eq)

classify :: Char -> CharClass
classify c
  | isSpace c               = CCSpace
  | isAlphaNum c || c == '_' = CCWord
  | otherwise               = CCPunct

-- Run lengths at slice edges, used so word hops stream over the text instead
-- of stepping with @T.index@ — which is O(i) per call, turning a word hop
-- across a multi-megabyte single-line token into an O(n²) freeze.

-- | Length of the run of characters satisfying @p@ at the front of @t@.
runLen :: (Char -> Bool) -> Text -> Int
runLen p = T.length . T.takeWhile p

-- | Length of the run of characters satisfying @p@ at the end of @t@.
trailLen :: (Char -> Bool) -> Text -> Int
trailLen p = T.foldl' (\acc ch -> if p ch then acc + 1 else 0) 0

wordLeft :: Pos -> Buffer -> Pos
wordLeft pos b =
  let Pos l c = clampPos pos b
  in if c == 0
       then if l == 0 then Pos 0 0 else Pos (l - 1) (lineLen (l - 1) b)
       else Pos l (wordLeftCol (getLine' l b) c)

wordLeftCol :: Text -> Int -> Int
wordLeftCol line c0 =
  let pre0 = T.take c0 line
      i1   = c0 - trailLen isSpace pre0
      pre1 = T.take i1 pre0
  in if i1 == 0
       then 0
       else i1 - trailLen ((== classify (T.last pre1)) . classify) pre1

wordRight :: Pos -> Buffer -> Pos
wordRight pos b =
  let Pos l c = clampPos pos b
      len     = lineLen l b
  in if c >= len
       then if l < lineCount b - 1 then Pos (l + 1) 0 else Pos l c
       else Pos l (wordRightCol (getLine' l b) c)

wordRightCol :: Text -> Int -> Int
wordRightCol line c0 =
  let rest0 = T.drop c0 line
  in case T.uncons rest0 of
       Nothing -> c0
       Just (c, _) ->
         let cls0 = classify c
             skip = if cls0 == CCSpace then 0 else runLen ((== cls0) . classify) rest0
             spaces = runLen isSpace (T.drop skip rest0)
         in c0 + skip + spaces

-- | The range of the "word" at a position (the maximal run of one character
-- class — alphanumerics, whitespace, or punctuation), for double-click select.
wordRangeAt :: Pos -> Buffer -> (Pos, Pos)
wordRangeAt pos b =
  let Pos l c0 = clampPos pos b
      line = getLine' l b
      c    = max 0 (min c0 (T.length line))
      pre  = T.take c line
      post = T.drop c line
      anchor | not (T.null post) = Just (classify (T.head post))   -- char under cursor
             | not (T.null pre)  = Just (classify (T.last pre))    -- end of line: char to the left
             | otherwise         = Nothing
  in case anchor of
       Nothing -> (Pos l c, Pos l c)
       Just k  ->
         (Pos l (c - trailLen ((== k) . classify) pre)
         , Pos l (c + runLen ((== k) . classify) post))

-- | The whole line at a position, including its trailing newline (so it reaches
-- the next line's start), for triple-click select.
lineRangeAt :: Pos -> Buffer -> (Pos, Pos)
lineRangeAt pos b =
  let Pos l _ = clampPos pos b
  in if l + 1 < lineCount b
       then (Pos l 0, Pos (l + 1) 0)
       else (Pos l 0, Pos l (lineLen l b))

------------------------------------------------------------------------------
-- Bracket matching

openBrackets, closeBrackets :: String
openBrackets  = "([{"
closeBrackets = ")]}"

-- Cap on characters visited looking for a partner, so an unmatched bracket in
-- a huge file can't make every repaint scan to the end.
maxBracketScan :: Int
maxBracketScan = 200000

-- | The bracket at (or, failing that, just before) @pos@ and its matching
-- partner: @(bracketPos, partnerPos)@. Purely textual — strings and comments
-- are not understood — and bounded by 'maxBracketScan'.
matchBracket :: Pos -> Buffer -> Maybe (Pos, Pos)
matchBracket pos buf = do
  (bp, ch) <- bracketAtOrBefore
  case elemIndex ch openBrackets of
    Just k  -> (,) bp <$> scanFwd ch (closeBrackets !! k) bp
    Nothing -> do
      k <- elemIndex ch closeBrackets
      (,) bp <$> scanBwd (openBrackets !! k) ch bp
  where
    Pos l c = clampPos pos buf
    line = getLine' l buf
    n = lineCount buf
    isBr x = x `elem` (openBrackets ++ closeBrackets)
    charAt i
      | i >= 0 && i < T.length line && T.length line <= maxBracketScan = Just (T.index line i)
      | otherwise = Nothing
    bracketAtOrBefore = case charAt c of
      Just ch | isBr ch -> Just (Pos l c, ch)
      _ -> case charAt (c - 1) of
             Just ch | isBr ch -> Just (Pos l (c - 1), ch)
             _                 -> Nothing
    -- Forward: stream the rest of each line, counting nesting of this kind.
    scanFwd open close (Pos l0 c0) = goF l0 (c0 + 1) 1 maxBracketScan
      where
        goF li col !depth !budget
          | li >= n || budget <= 0 = Nothing
          | otherwise =
              let ln = getLine' li buf
              in case walkF col depth (T.drop col ln) of
                   Right i -> Just (Pos li i)
                   Left d' -> goF (li + 1) 0 d' (budget - (T.length ln - col) - 1)
        walkF !i !depth t = case T.uncons t of
          Nothing -> Left depth
          Just (x, rest)
            | x == open  -> walkF (i + 1) (depth + 1) rest
            | x == close -> if depth == 1 then Right i else walkF (i + 1) (depth - 1) rest
            | otherwise  -> walkF (i + 1) depth rest
    -- Backward: collect this kind's brackets in the line prefix (a streamed
    -- left-to-right pass), then count nesting over them right-to-left.
    scanBwd open close (Pos l0 c0) = goB l0 (Just c0) 1 maxBracketScan
      where
        goB li mcol !depth !budget
          | li < 0 || budget <= 0 = Nothing
          | otherwise =
              let ln = getLine' li buf
                  upto = fromMaybe (T.length ln) mcol
                  cols = [ (i, x) | (i, x) <- zip [0 ..] (T.unpack (T.take upto ln))
                                  , x == open || x == close ]
              in case walkB depth (reverse cols) of
                   Right i -> Just (Pos li i)
                   Left d' -> goB (li - 1) Nothing d' (budget - upto - 1)
        walkB !depth [] = Left depth
        walkB !depth ((i, x) : rest)
          | x == close = walkB (depth + 1) rest
          | depth == 1 = Right i
          | otherwise  = walkB (depth - 1) rest

------------------------------------------------------------------------------
-- Files

data LineEnding = LF | CRLF | CR
  deriving (Eq, Show)

data Encoding = Utf8 | Utf8Bom
  deriving (Eq, Show)

lineEndingText :: LineEnding -> Text
lineEndingText LF   = T.pack "\n"
lineEndingText CRLF = T.pack "\r\n"
lineEndingText CR   = T.pack "\r"

-- | Everything learned from loading a file, so saving can round-trip exactly.
data LoadResult = LoadResult
  { lrBuffer      :: !Buffer
  , lrLineEnding  :: !LineEnding
  , lrEncoding    :: !Encoding
  , lrFinalNewline :: !Bool
  , lrReadOnly    :: !Bool
  , lrMtime       :: !(Maybe EpochTime)  -- ^ On-disk modification time at load (for stale-file detection).
  } deriving (Show)

-- | The file's last-modification time, or @Nothing@ if it can't be stat'd
-- (missing/unreadable). Used to detect a file changing underneath us.
fileMtime :: FilePath -> IO (Maybe EpochTime)
fileMtime path = do
  r <- try (getFileStatus path) :: IO (Either SomeException FileStatus)
  pure (either (const Nothing) (Just . modificationTime) r)

-- | Load a file. A non-existent path yields an empty buffer (new file). IO
-- errors are reported as @Left@.
loadFile :: FilePath -> IO (Either String LoadResult)
loadFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right emptyLoadResult)
    else do
      eres <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      case eres of
        Left e   -> pure (Left (show e))
        Right bs -> do
          ro <- not <$> canWrite path
          mt <- fileMtime path
          pure (Right (loadFromBytes ro mt bs))

-- | The 'LoadResult' for a not-yet-existing file: an empty, writable buffer.
emptyLoadResult :: LoadResult
emptyLoadResult = LoadResult emptyBuffer LF Utf8 True False Nothing

-- | Decode already-read file bytes into a 'LoadResult'. Split out from
-- 'loadFile' so a caller that has the bytes in hand (e.g. an async loader that
-- also needs to sniff for images/binary) doesn't have to re-read the file.
loadFromBytes :: Bool -> Maybe EpochTime -> BS.ByteString -> LoadResult
loadFromBytes ro mt bs =
  let (enc, bs') = stripBom bs
      txt        = TE.decodeUtf8With TEE.lenientDecode bs'
      le         = detectLineEnding txt
      (ls, fin)  = splitContent txt
  in LoadResult (mkBuffer ls) le enc fin ro mt

-- | A cheap "is this a binary (non-text) file?" heuristic: a NUL byte anywhere
-- in the first 8 KiB. Matches what git/grep do, and reliably catches
-- executables, archives and other non-text blobs that must not be opened as
-- text (a huge one would otherwise decode into millions of junk lines).
looksBinary :: BS.ByteString -> Bool
looksBinary bs = 0 `BS.elem` BS.take 8192 bs

stripBom :: BS.ByteString -> (Encoding, BS.ByteString)
stripBom bs
  | BS.take 3 bs == BS.pack [0xEF, 0xBB, 0xBF] = (Utf8Bom, BS.drop 3 bs)
  | otherwise                                  = (Utf8, bs)

detectLineEnding :: Text -> LineEnding
detectLineEnding t
  | T.pack "\r\n" `T.isInfixOf` t = CRLF
  | T.pack "\r"   `T.isInfixOf` t = CR
  | otherwise                     = LF

canWrite :: FilePath -> IO Bool
canWrite path = do
  r <- try (getPermissions path) :: IO (Either SomeException Permissions)
  pure $ case r of
    Right p -> writable p
    Left _  -> True   -- unknown; assume writable and let the write fail loudly

-- | Save a buffer atomically (write to a temp file then rename). Returns the
-- number of bytes written and the file's new modification time on success.
saveFile :: FilePath -> Encoding -> LineEnding -> Bool -> Buffer
         -> IO (Either String (Int, Maybe EpochTime))
saveFile path enc le final b = do
  let txt   = bufferToText le final b
      body  = TE.encodeUtf8 txt
      bom   = if enc == Utf8Bom then BS.pack [0xEF, 0xBB, 0xBF] else BS.empty
      bytes = bom <> body
      tmp   = path ++ ".cmedit-tmp"
  -- Refuse to clobber an existing read-only file: the atomic temp+rename would
  -- otherwise silently replace it (a writable directory is enough), so check the
  -- target's permissions first and report it clearly.
  exists <- doesFileExist path
  canWrite <- if exists
                then either (const True) writable
                       <$> (try (getPermissions path) :: IO (Either SomeException Permissions))
                else pure True
  if exists && not canWrite
    then pure (Left (path ++ " is read-only \x2014 use Save As to write a copy"))
    else do
      r <- try (writeAtomic tmp path bytes) :: IO (Either IOException ())
      case r of
        Left e  -> do
          _ <- try (removeFile tmp) :: IO (Either SomeException ())
          pure (Left (saveErrorMessage path e))
        Right () -> do
          mt <- fileMtime path
          pure (Right (BS.length bytes, mt))

-- | Read a file, apply a whole-text substitution and write it back atomically,
-- preserving its BOM and (unnormalised) line endings. The substitution — the
-- caller's 'replaceAllText' — returns @(count, newText)@; when it changes
-- nothing (or the file is binary/unreadable) the file is left untouched. Used by
-- the workspace Replace All for files that are not currently open in the editor.
replaceInFile :: FilePath -> (Text -> (Int, Text)) -> IO (Either String Int)
replaceInFile path subst = do
  ebs <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
  case ebs of
    Left e -> pure (Left (show e))
    Right bs
      | looksBinary bs -> pure (Right 0)
      | otherwise ->
          let (enc, body) = stripBom bs
              txt         = TE.decodeUtf8With TEE.lenientDecode body
              (cnt, txt') = subst txt
          in if cnt == 0
               then pure (Right 0)
               else do
                 let bom = if enc == Utf8Bom then BS.pack [0xEF, 0xBB, 0xBF] else BS.empty
                     out = bom <> TE.encodeUtf8 txt'
                     tmp = path ++ ".cmedit-tmp"
                 r <- try (writeAtomic tmp path out) :: IO (Either IOException ())
                 case r of
                   Left e   -> do _ <- try (removeFile tmp) :: IO (Either SomeException ())
                                  pure (Left (saveErrorMessage path e))
                   Right () -> pure (Right cnt)

-- A readable reason for a failed write (without leaking the temp file name).
saveErrorMessage :: FilePath -> IOException -> String
saveErrorMessage path e
  | isPermissionError e    = "Permission denied saving " ++ path
  | isFullError e          = "No space left to save " ++ path
  | isAlreadyInUseError e  = path ++ " is in use by another program"
  | otherwise              = "Could not save " ++ path ++ ": " ++ ioeGetErrorString e

writeAtomic :: FilePath -> FilePath -> BS.ByteString -> IO ()
writeAtomic tmp path bytes = do
  withBinaryFile tmp WriteMode $ \h -> do
    BS.hPut h bytes
    hFlush h
  renameFile tmp path
