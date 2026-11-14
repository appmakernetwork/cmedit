{-# LANGUAGE BangPatterns #-}
-- | The PDF object layer: everything between a file's bytes and the things a
-- reader of it wants to know (its pages, their resources, their content
-- streams), shared by the reflow view ("Cmedit.Pdf") and the page renderer
-- ("Cmedit.PdfRender", "Cmedit.PdfImage", "Cmedit.PdfFunc").
--
-- Split out of "Cmedit.Pdf" unchanged (plan 0034 §C): the reflow view must
-- behave identically, so nothing moved here was reworded in a way that could
-- change what it computes. The load-bearing decisions are the ones that
-- module's header has always described:
--
--   * __Objects are found without the cross-reference table.__ 'scanObjects'
--     sweeps the file for @N G obj@ and parses each definition, resuming
--     /after/ the object just read, so an @obj@ inside a compressed stream's
--     bytes is never mistaken for a definition. Broken, patched and
--     linearised-then-appended files are all read the same way, and an
--     incremental update falls out for free: a later definition of an object
--     number wins. 'expandObjStms' then folds in the objects hidden in
--     compressed object streams under the same rule.
--
--   * __Every decode is charged to the document.__ 'decodeStream' takes what
--     remains of the document's byte budget ('maxPdfDecoded') and reports what
--     it spent, and no filter may produce more than is left; 'Fuel' carries
--     that budget plus the token and CMap-entry budgets through a whole
--     document, so the limits are on the /file/ and not on any one page.
--
--   * __Nothing here throws.__ An unrecognised token parses as 'ONull', a
--     dictionary with junk where a key belongs skips the junk, a stream with a
--     wrong @\/Length@ is measured by its @endstream@, and every recursion
--     (object nesting, reference chains, the page tree) has a depth limit.
--
-- What is new in the split is only API: accessors a renderer needs
-- ('boolOf', 'numsOf', 'matrixOf', 'rectOf'), page geometry as
-- "Cmedit.Geom" values ('pageMediaBox', 'pageCropBox', 'pageRotation'), a
-- content tokenizer that returns inline images instead of skipping them
-- ('contentTokensInline'), a stream decoder that stops in front of an image
-- codec ('decodeStreamPrefix'), and the standard encodings by glyph /name/
-- (what a font program is indexed by) next to the character-valued tables the
-- text view has always used.
module Cmedit.PdfObj
  ( -- * Objects
    Obj(..)
  , Dict
  , PdfFile(..)
    -- * Opening a file
  , sniffPdf
  , parsePdfFile
  , openPdfFile
  , pdfPages
  , pdfVersion
    -- * The object parser (for callers holding raw bytes: CMaps, inline images)
  , parseObj
  , parseObjects
  , scanObjects
  , expandObjStms
  , findTrailer
  , readNumber
    -- * Following references and reading values
  , resolve
  , dictOf
  , lookD
  , lookDAny
  , arrOf
  , nameOf
  , numOf
  , intOf
  , boolOf
  , strOf
  , numsOf
  , matrixOf
  , rectOf
  , refOf
    -- * Streams and filters
  , decodeStream
  , decodeStreamF
  , decodeStreamPrefix
  , streamFilters
  , canonicalFilter
  , isImageCodec
  , applyPredictor
  , ccittParams
    -- * The document budget
  , Fuel(..)
  , fuel0
  , fuelDefault
  , fuelSpent
  , spendBytes
  , spendTokens
  , spendEntries
    -- * Content streams
  , Tok(..)
  , contentTokens
  , contentTokensInline
    -- * Pages
  , collectPages
  , mediaBox
  , pageMediaBox
  , pageCropBox
  , pageRotation
  , contentsOf
    -- * Encodings and glyph names
  , glyphChar
  , glyphNames
  , glyphUnicode
  , namedEncoding
  , winAnsiMap
  , macRomanMap
  , applyDifferences
  , encodingByName
  , differences
  , standardEncodingNames
  , winAnsiEncodingNames
  , macRomanEncodingNames
  , macExpertEncodingNames
  , symbolEncodingNames
  , zapfDingbatsEncodingNames
    -- * Limits
  , maxPdfBytes
  , maxPdfPages
  , maxStreamBytes
  , maxPdfDecoded
  , maxPdfTokens
  , maxPdfCMapEntries
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, isHexDigit, digitToInt)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, (!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M

import Cmedit.Ccitt (CcittParams(..), CcittResult(..), decodeCcitt, defaultCcitt)
import Cmedit.Geom (Affine(..), Rect(..), rectIntersect, rectEmpty)
import Cmedit.Inflate (inflateDyn)

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

-- | Largest single decompressed stream.
maxStreamBytes :: Int
maxStreamBytes = 64 * 1024 * 1024

-- | Bytes of stream data the whole document may have decoded for it: every
-- filter step's input and output, counted each time a stream is decoded.
--
-- 'maxStreamBytes' bounds one stream and nothing bounded how many: 2000 pages
-- may all name the same 60 KB stream that inflates to 64 MB, a page may @Do@
-- one form 400 000 times, a file may hold hundreds of object streams, and
-- every one of those was a fresh 64 MB inflate (measured: minutes, from a
-- 400 KB file). This is what a /document/ can spend. A real text PDF decodes
-- a few megabytes in all — the most in a 159-file corpus was 6 MB, and a
-- 2000-page book is some tens — so a quarter of a gigabyte is forty times
-- anything seen and about three seconds of inflating. Past it the remaining
-- pages are not read, and the note says so.
maxPdfDecoded :: Int
maxPdfDecoded = 256 * 1024 * 1024

-- | Content-stream tokens (operators and their operands) interpreted across
-- the whole document, with the two expensive operators weighted. The same
-- idea as 'maxOpsPerPage' one level up: that limit times 'maxPdfPages' is 800
-- million operators, which is most of a minute. A page of prose is a few
-- thousand tokens (the corpus's largest document was 0.9 million in all), so
-- this is reached only by a file built to reach it.
maxPdfTokens :: Int
maxPdfTokens = 64 * 1000 * 1000

-- | @\/ToUnicode@ entries kept across all of a document's fonts. One CMap is
-- already capped ('maxCMapEntries'); a file can carry thousands of them.
maxPdfCMapEntries :: Int
maxPdfCMapEntries = 4 * 1024 * 1024

-- | Nesting of arrays and dictionaries inside one object. Real objects nest a
-- handful deep; 64 MB of @[@ is a recursion 64 million frames deep.
maxObjDepth :: Int
maxObjDepth = 256

-- | Elements kept from one array. The rest are still parsed (the parser has
-- to find the closing bracket) but dropped: one 64 MB array operand is
-- otherwise 32 million boxed numbers.
maxArrayElems :: Int
maxArrayElems = 1048576


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
parseObj = parseObjD 0

-- 'parseObj' at a nesting depth. Past 'maxObjDepth' an opening bracket is
-- stepped over like any other stray delimiter, so the recursion is bounded
-- and the scan still moves forward.
parseObjD :: Int -> BS.ByteString -> Int -> Maybe (Obj, Int)
parseObjD depth bs i0
  | i0 < 0 = Nothing                        -- an offset the file made up
  | i >= n = Nothing
  | depth >= maxObjDepth && (c == 91 || (c == 60 && byteAt bs (i + 1) == 60)) =
      Just (ONull, i + 1)
  | c == 47 = let (nm, j) = readName bs (i + 1) in Just (OName nm, j)
  | c == 40 = let (s, j) = readLitStr bs (i + 1) in Just (OStr s, j)
  | c == 60 && byteAt bs (i + 1) == 60 = parseDictAt (depth + 1) bs (i + 2)
  | c == 60 = let (s, j) = readHexStr bs (i + 1) in Just (OStr s, j)
  | c == 91 = parseArrAt (depth + 1) bs (i + 1) 0 []
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
readLitStr bs i0
  | not hasEsc = (BS.take (end - i0) (BS.drop i0 bs), after)
  | otherwise  = (fst (BS.unfoldrN (end - i0) step i0), after)
  where
    n = BS.length bs
    -- First where the string ends, without building anything: the closing
    -- parenthesis that balances the opener, or the end of the input. An
    -- escape hides exactly the byte after its backslash, which is all that
    -- finding the end needs to know about escapes. Most strings have none,
    -- and are then a slice; the rest are decoded in one pass into a buffer no
    -- larger than the source. (Accumulating a list of bytes, as this once
    -- did, cost 7 GB for a 64 MB string.)
    (end, after, hasEsc) = scan i0 (1 :: Int) False
    scan !i !depth !e
      | i >= n = (n, n, e)
      | otherwise = case BS.index bs i of
          92 -> if i + 1 >= n then (i, n, True) else scan (i + 2) depth True
          40 -> scan (i + 1) (depth + 1) e
          41 | depth <= 1 -> (i, i + 1, e)
             | otherwise  -> scan (i + 1) (depth - 1) e
          _  -> scan (i + 1) depth e
    step !i
      | i >= end = Nothing
      | otherwise = case BS.index bs i of
          92 -> esc (i + 1)
          b  -> Just (b, i + 1)
    esc !i
      | i >= end = Nothing
      | otherwise = case BS.index bs i of
          110 -> Just (10, i + 1)   -- n
          114 -> Just (13, i + 1)   -- r
          116 -> Just (9, i + 1)    -- t
          98  -> Just (8, i + 1)    -- b
          102 -> Just (12, i + 1)   -- f
          10  -> step (i + 1)       -- line continuation
          13  -> step (if byteAt bs (i + 1) == 10 then i + 2 else i + 1)
          b | b >= 48 && b <= 55 ->
                let ds = takeWhile (\k -> let x = byteAt bs (i + k)
                                          in k < 3 && x >= 48 && x <= 55) [0, 1, 2]
                    v = foldl' (\a k -> a * 8 + fromIntegral (byteAt bs (i + k)) - 48) 0 ds
                in Just (fromIntegral (v .&. 0xff :: Int), i + length ds)
            | otherwise -> Just (b, i + 1)

-- | A @<...>@ hex string; an odd final digit is padded with zero, per spec.
readHexStr :: BS.ByteString -> Int -> (BS.ByteString, Int)
readHexStr bs i0 = (fst (BS.unfoldrN ((end - i0 + 1) `div` 2) step i0), after)
  where
    n = BS.length bs
    end = maybe n (i0 +) (BS.elemIndex 62 (BS.drop i0 bs))
    after = if end < n then end + 1 else n
    -- The next hex digit at or after @i@, and where it is.
    digit !i
      | i >= end = Nothing
      | isHexDigit (w2c (BS.index bs i)) = Just (digitToInt (w2c (BS.index bs i)), i + 1)
      | otherwise = digit (i + 1)
    step i = case digit i of
      Nothing -> Nothing
      Just (a, j) -> case digit j of
        Nothing     -> Just (fromIntegral (a * 16), end)
        Just (b, k) -> Just (fromIntegral (a * 16 + b), k)

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
readNumber s0 = sign * (whole + frac)
  where
    -- Straight off the bytes, in the same order of floating-point operations
    -- a digit-list fold would use: numbers are most of a content stream.
    (ss, s1) = BS.span (\c -> c == 43 || c == 45) s0
    sign = if odd (BS.count 45 ss) then -1 else 1 :: Double
    isDig c = c >= 48 && c <= 57
    (ip, rest) = BS.span isDig s1
    fp = if not (BS.null rest) && BS.head rest == 46 then BS.takeWhile isDig (BS.drop 1 rest)
                                                      else BS.empty
    dig c = fromIntegral (c - 48) :: Double
    whole = BS.foldl' (\a d -> a * 10 + dig d) 0 (BS.take 12 ip)
    frac = BS.foldr (\d a -> (a + dig d) / 10) 0 (BS.take 10 fp)

parseArrAt :: Int -> BS.ByteString -> Int -> Int -> [Obj] -> Maybe (Obj, Int)
parseArrAt depth bs = go
  where
    go !i !cnt acc =
      let j = skipWs bs i
      in if j >= BS.length bs then Just (OArr (reverse acc), j)
         else if byteAt bs j == 93 then Just (OArr (reverse acc), j + 1)
         else case parseObjD depth bs j of
                Just (o, k) | k > j ->
                  -- Forced, so a long array is its elements and not a chain
                  -- of thunks each holding a slice of the stream.
                  o `seq` (if cnt + 1 < maxArrayElems then go k (cnt + 1) (o : acc)
                                                      else skipRest k (o : acc))
                _ -> Just (OArr (reverse acc), j + 1)
    -- Past 'maxArrayElems': find the closing bracket without building
    -- anything. Tokens are stepped over exactly as 'parseObjD' would consume
    -- them (a string or a nested container still has to be read to know
    -- where it ends), so the array ends where it always did.
    skipRest !i acc =
      let j = skipWs bs i
          c = byteAt bs j
      in if j >= BS.length bs then Just (OArr (reverse acc), j)
         else if c == 93 then Just (OArr (reverse acc), j + 1)
         else if isRegB c then skipRest (snd (readRegular bs j)) acc
         else if c == 47 then skipRest (snd (readRegular bs (j + 1))) acc
         else case parseObjD depth bs j of
                Just (_, k) | k > j -> skipRest k acc
                _ -> Just (OArr (reverse acc), j + 1)

-- | A dictionary, and — if @stream@ follows it — the stream it heads.
parseDictAt :: Int -> BS.ByteString -> Int -> Maybe (Obj, Int)
parseDictAt depth bs i0 = go i0 M.empty
  where
    n = BS.length bs
    go !i !acc
      | j >= n = Just (ODict acc, j)
      | byteAt bs j == 62 && byteAt bs (j + 1) == 62 = afterDict acc (j + 2)
      | byteAt bs j == 47 =
          let (k, j1) = readName bs (j + 1)
          in case parseObjD depth bs j1 of
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
--
-- Returns what is left of the decoding budget as well: a file may carry any
-- number of these, each inflating to 'maxStreamBytes'.
expandObjStms :: BS.ByteString -> Int -> IM.IntMap (Int, Obj) -> (IM.IntMap (Int, Obj), Int)
expandObjStms _bs left0 found = foldl' step (found, left0) (IM.toList found)
  where
    base = PdfFile (IM.map snd found) M.empty
    step (!acc, !left) (_, (off, OStream d raw))
      | M.lookup (BC.pack "Type") d == Just (OName (BC.pack "ObjStm")) =
          case decodeStream base left d raw of
            (Right dat, spent) -> (unpack acc off d dat, left - spent)
            (Left _, spent)    -> (acc, left - spent)
    step acc _ = acc
    unpack acc off d dat =
          let cnt = intOf base (M.lookup (BC.pack "N") d)
              first = intOf base (M.lookup (BC.pack "First") d)
              hdr = readPairs dat 0 (min cnt 100000)
              len = BS.length dat
              -- Offsets come from the stream's own header, so each is checked
              -- against the stream, and the bytes parsed are budgeted: N
              -- entries may all point at one long unterminated string, and
              -- parsing it N times is N copies of the stream.
              budget0 = 8 * len + 4096
          in fst (foldl' (\(a, left) (num, rel) ->
                        let at0 = first + rel
                        in if left <= 0 || at0 < 0 || at0 >= len then (a, left)
                           else case parseObj dat at0 of
                             Just (o, j) -> (IM.insertWith later num (off, o) a, left - max 1 (j - at0))
                             Nothing     -> (a, left - 1))
                    (acc, budget0) hdr)
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

-- | Decode a stream through its @\/Filter@ chain. Image codecs (DCT, JPX,
-- JBIG2) are reported rather than attempted: the caller (content-stream
-- loading) simply skips what it cannot read, and an image's caller uses
-- 'decodeStreamPrefix' instead.
--
-- @left@ is what remains of the document's 'maxPdfDecoded', and the second
-- result is what this decode spent of it: each filter step's input plus its
-- output (LZW's input counted eight times over, because it is that much
-- slower per byte than everything else here). No step may produce more than
-- what is left, so the budget cannot be overshot by more than one step's
-- input — and a document inside its budget decodes exactly as it would with
-- no budget at all. An unfiltered stream is charged its length: the budget is
-- on bytes handed onward to be interpreted, however they were arrived at.
decodeStream :: PdfFile -> Int -> Dict -> BS.ByteString -> (Either String BS.ByteString, Int)
decodeStream f left0 d raw = case decodeChain False f left0 d raw of
  (Left e, spent)       -> (Left e, spent)
  (Right (b, _), spent) -> (Right b, spent)

-- | Decode a stream's chain up to, not including, its first image codec
-- (@DCTDecode@, @JPXDecode@, @JBIG2Decode@), returning the bytes reached and
-- the filters still to apply (canonical names, with their parameters). The
-- remainder is empty for a stream with no codec, whose result is then exactly
-- 'decodeStream''s. Charged like 'decodeStream'; stopping in front of a codec
-- charges the bytes handed to it once.
decodeStreamPrefix :: PdfFile -> Fuel -> Dict -> BS.ByteString
                   -> (Either String (BS.ByteString, [(BS.ByteString, Dict)]), Fuel)
decodeStreamPrefix f fu d raw =
  let (r, spent) = decodeChain True f (fuBytes fu) d raw in (r, spendBytes spent fu)

-- | A stream's filters with their @\/DecodeParms@, names canonicalised (an
-- inline image may abbreviate them). Only the full keys are read: in a stream
-- dictionary @\/F@ is an external file specification, not a filter, so an
-- inline image's @\/F@ and @\/DP@ are its reader's to expand first.
streamFilters :: PdfFile -> Dict -> [(BS.ByteString, Dict)]
streamFilters f d = zip filters (parmList ++ repeat M.empty)
  where
    filters = map (canonicalFilter . nameOf) (arrOf f (lookD f d "Filter"))
    parmList = case lookD f d "DecodeParms" of
                 OArr xs -> map (dictOf f) xs
                 ONull   -> []
                 o       -> [dictOf f o]

-- | The full name of a filter, expanding the inline-image abbreviations.
canonicalFilter :: BS.ByteString -> BS.ByteString
canonicalFilter nm = case BC.unpack nm of
  "AHx" -> BC.pack "ASCIIHexDecode"
  "A85" -> BC.pack "ASCII85Decode"
  "LZW" -> BC.pack "LZWDecode"
  "Fl"  -> BC.pack "FlateDecode"
  "RL"  -> BC.pack "RunLengthDecode"
  "CCF" -> BC.pack "CCITTFaxDecode"
  "DCT" -> BC.pack "DCTDecode"
  _     -> nm

-- | Filters that produce an image rather than bytes, and so end a chain for
-- 'decodeStreamPrefix'.
isImageCodec :: BS.ByteString -> Bool
isImageCodec nm = nm `elem` map BC.pack ["DCTDecode", "JPXDecode", "JBIG2Decode"]

decodeChain :: Bool -> PdfFile -> Int -> Dict -> BS.ByteString
            -> (Either String (BS.ByteString, [(BS.ByteString, Dict)]), Int)
decodeChain stopAtCodec f left0 d raw
  | left0 <= 0   = (Left budgetMsg, 0)
  | null chain   = (Right (raw, []), BS.length raw)
  | otherwise    = go chain raw 0
  where
    chain = streamFilters f d
    budgetMsg = "the document's decoding budget is spent"
    go [] dat !spent = (Right (dat, []), spent)
    go rest0@((nm, pd) : rest) dat !spent
      | left <= 0 = (Left budgetMsg, spent)
      | stopAtCodec && isImageCodec nm =
          (Right (dat, rest0), if spent == 0 then BS.length dat else spent)
      | otherwise = case applyFilter (min maxStreamBytes left) f pd nm dat of
          Left e    -> (Left e, spent + cin)
          Right out ->
            let out' = applyPredictor f pd out
            -- (the filter's own output is what is charged: the predictor
            -- only ever shrinks it, and its work is in proportion to it)
            in go rest out' (spent + cin + BS.length out)
      where
        left = left0 - spent
        cin | nm == BC.pack "LZWDecode" = 8 * BS.length dat
            | otherwise = BS.length dat

-- One filter (canonical name, its parameters), producing at most @lim@ bytes.
applyFilter :: Int -> PdfFile -> Dict -> BS.ByteString -> BS.ByteString -> Either String BS.ByteString
applyFilter lim f pd nm dat = case BC.unpack nm of
  "FlateDecode"     -> flate
  "LZWDecode"       -> Right (lzwDecode early lim dat)
  "ASCIIHexDecode"  -> Right (hexDecode dat)
  "ASCII85Decode"   -> Right (a85Decode lim dat)
  "RunLengthDecode" -> Right (rleDecode lim dat)
  "CCITTFaxDecode"  -> Right (crData (decodeCcitt (ccittParams f pd) lim dat))
  "Crypt"           -> Right dat        -- the Identity crypt filter
  ""                -> Right dat
  other             -> Left (other ++ ": not a text filter")
  where
    -- /EarlyChange 0 widens LZW codes one entry later than the default.
    early = case M.lookup (BC.pack "EarlyChange") pd of
              Just o -> intOf f (Just o) /= 0
              Nothing -> True
    -- A zlib wrapper is the norm; a few producers emit raw DEFLATE, so a
    -- failed header check retries from byte zero rather than giving up.
    flate =
      let hdr2 = BS.length dat >= 2
                   && (fromIntegral (BS.index dat 0) .&. (0x0f :: Int)) == 8
                   && ((fromIntegral (BS.index dat 0) * 256
                        + fromIntegral (BS.index dat 1)) `mod` (31 :: Int)) == 0
      in case inflateDyn dat (if hdr2 then 2 else 0) lim of
           Right out -> Right out
           Left _ | hdr2 -> inflateDyn dat 0 lim
           Left e -> Left e

-- | A @CCITTFaxDecode@ filter's parameters, with the spec's defaults.
ccittParams :: PdfFile -> Dict -> CcittParams
ccittParams f pd = CcittParams
  { ccK = int "K" (ccK defaultCcitt)
  , ccColumns = int "Columns" (ccColumns defaultCcitt)
  , ccRows = int "Rows" (ccRows defaultCcitt)
  , ccBlackIs1 = bool "BlackIs1" (ccBlackIs1 defaultCcitt)
  , ccByteAlign = bool "EncodedByteAlign" (ccByteAlign defaultCcitt)
  , ccEndOfLine = bool "EndOfLine" (ccEndOfLine defaultCcitt)
  , ccEndOfBlock = bool "EndOfBlock" (ccEndOfBlock defaultCcitt)
  , ccDamagedRows = int "DamagedRowsBeforeError" (ccDamagedRows defaultCcitt)
  }
  where
    int k dflt = case lookD f pd k of
      ONum v | abs v < 1e9 -> truncate v
      _ -> dflt
    bool k dflt = maybe dflt id (boolOf f (lookD f pd k))

-- | Undo a PNG or TIFF predictor. Used by cross-reference streams and by any
-- stream whose producer thought it would compress better; harmless (identity)
-- when @\/Predictor@ is absent or 1.
--
-- Both run over an unboxed buffer the size of their output. They were list
-- pipelines once, and a list of bytes is forty times the size of the bytes:
-- a 64 MB stream with a predictor on it took over 10 GB.
applyPredictor :: PdfFile -> Dict -> BS.ByteString -> BS.ByteString
applyPredictor f pd dat
  | pred' < 2 = dat
  | pred' == 2 = tiff
  | otherwise = png
  where
    pred'   = intOf f (M.lookup (BC.pack "Predictor") pd)
    colors  = maxI 1 (intOf f (M.lookup (BC.pack "Colors") pd))
    -- (BitsPerComponent defaults to 8, not 1: at 1 a 3-colour row of 61
    -- samples is 23 bytes instead of 183, and every row decodes as garbage.)
    bpc     = maxI 8 (intOf f (M.lookup (BC.pack "BitsPerComponent") pd))
    columns = maxI 1 (intOf f (M.lookup (BC.pack "Columns") pd))
    maxI a b = if b <= 0 then a else b
    bpp = max 1 ((colors * bpc + 7) `div` 8)
    -- Clamped to the data: a row longer than the stream is one short row
    -- either way, and /Columns is whatever the file says.
    rowLen = max 1 (min len ((colors * bpc * columns + 7) `div` 8))
    len = BS.length dat

    fromArr :: Int -> UArray Int Word8 -> BS.ByteString
    fromArr k ua = fst (BS.unfoldrN k (\i -> Just (ua ! i, i + 1)) 0)

    -- TIFF predictor 2 at 8 bits: each component is a delta from the one
    -- @bpp@ bytes earlier in the same row.
    tiff | bpc /= 8 = dat
         | otherwise = fromArr len $ runSTUArray $ do
             out <- newArray (0, max 0 (len - 1)) 0
             let go !i
                   | i >= len = pure ()
                   | otherwise = do
                       let col = i `mod` rowLen
                       a <- if col >= bpp then readArray out (i - bpp) else pure 0
                       writeArray out i (BS.index dat i + a)
                       go (i + 1)
             go 0
             pure out

    -- PNG: every row is a filter-type byte and then up to @rowLen@ samples
    -- (the last row may be short). @a@ is the sample @bpp@ to the left, @b@
    -- the one above, @c@ the one above and to the left; all zero off the edge.
    png = fromArr outLen $ runSTUArray $ do
        out <- newArray (0, max 0 (outLen - 1)) 0
        let row !r
              | r * stride >= len = pure ()
              | otherwise = do
                  let src = r * stride
                      ft = fromIntegral (BS.index dat src) :: Int
                      k = min rowLen (len - src - 1)
                      o0 = r * rowLen
                      at i = readArray out i
                      px !col
                        | col >= k = pure ()
                        | otherwise = do
                            let v = BS.index dat (src + 1 + col)
                            a <- if col >= bpp then at (o0 + col - bpp) else pure 0
                            b <- if r > 0 then at (o0 - rowLen + col) else pure 0
                            c <- if r > 0 && col >= bpp then at (o0 - rowLen + col - bpp) else pure 0
                            let x = case ft of
                                      0 -> v
                                      1 -> v + a
                                      2 -> v + b
                                      3 -> v + fromIntegral ((fromIntegral a + fromIntegral b :: Int) `div` 2)
                                      4 -> v + paeth a b c
                                      _ -> v
                            writeArray out (o0 + col) x
                            px (col + 1)
                  px 0
                  row (r + 1)
        row (0 :: Int)
        pure out
      where
        stride = rowLen + 1
        (q, rm) = len `divMod` stride
        outLen = q * rowLen + max 0 (rm - 1)
        paeth :: Word8 -> Word8 -> Word8 -> Word8
        paeth a b c =
          let p = fromIntegral a + fromIntegral b - fromIntegral c :: Int
              pa = abs (p - fromIntegral a); pb = abs (p - fromIntegral b); pc = abs (p - fromIntegral c)
          in if pa <= pb && pa <= pc then a else if pb <= pc then b else c

hexDecode :: BS.ByteString -> BS.ByteString
hexDecode = fst . readHexStr'
  where readHexStr' bs = readHexStr (BS.snoc bs 62) 0

-- ASCII85, at most @lim@ bytes of it. Whitespace is skipped anywhere; a @z@
-- where a group would start is four zero bytes; a final group of @k@ digits
-- yields @k - 1@ bytes.
a85Decode :: Int -> BS.ByteString -> BS.ByteString
a85Decode lim bs0 = fst (BS.unfoldrN (max 0 (min lim (4 * n))) step (0, 0 :: Int, 0 :: Int))
  where
    -- Some producers include the "<~" introducer even though PDF's variant
    -- does not use it.
    body = let b = if BS.isPrefixOf (BC.pack "<~") bs0 then BS.drop 2 bs0 else bs0
               (pre, _) = BS.breakSubstring (BC.pack "~>") b
           in pre
    n = BS.length body
    skip !i | i < n && isWsB (BS.index body i) = skip (i + 1)
            | otherwise = i
    -- State: position, bytes of the current group still to emit, and those
    -- bytes (the next one in bits 24-31).
    step (!i, !pend, !v)
      | pend > 0 = Just (fromIntegral ((v `shiftR` 24) .&. 0xff), (i, pend - 1, (v `shiftL` 8) .&. 0xffffffff))
      | j >= n = Nothing
      | BS.index body j == 122 = step (j + 1, 4, 0)      -- 'z'
      | otherwise =
          let (k, acc, j') = group j (0 :: Int) (0 :: Int)
          in if k < 2 then Nothing
             else step (j', k - 1, (acc * 85 ^ (5 - k) + pad k) .&. 0xffffffff)
      where j = skip i
    -- Up to five digits, skipping whitespace between them.
    group !j !k !acc
      | k >= 5 = (k, acc, j)
      | j' >= n = (k, acc, j')
      | otherwise = group (j' + 1) (k + 1) (acc * 85 + (fromIntegral (BS.index body j') - 33))
      where j' = skip j
    -- A short final group is padded with 'u' (84) digits.
    pad k = sum [ 84 * 85 ^ e | e <- [0 .. 4 - k] ] :: Int

-- RunLength, at most @lim@ bytes of it: a length byte below 128 copies that
-- many plus one literal bytes, one above 128 repeats the next byte, and 128
-- ends the data.
rleDecode :: Int -> BS.ByteString -> BS.ByteString
rleDecode lim bs = fst (BS.unfoldrN (max 0 (min lim (128 * n))) step (0, 0 :: Int, 0 :: Int))
  where
    n = BS.length bs
    -- State: position of the next length byte, then either a literal run in
    -- progress (@lit > 0@ bytes left, read from @i@) or a repeat (@rep > 0@
    -- copies left of the byte at @i@).
    step (!i, !lit, !rep)
      | lit > 0 = if i < n then Just (BS.index bs i, (i + 1, lit - 1, 0))
                           else Nothing
      | rep > 0 = Just (byteAt bs i, if rep == 1 then (i + 1, 0, 0) else (i, 0, rep - 1))
      | i >= n = Nothing
      | l == 128 = Nothing
      | l < 128 = step (i + 1, l + 1, 0)
      | otherwise = step (i + 1, 0, 257 - l)
      where l = fromIntegral (BS.index bs i) :: Int

-- | PDF's LZW: MSB-first codes, and the code width grows one entry *early*
-- (the @\/EarlyChange@ default), which is the one place it differs from GIF's
-- — hence a separate implementation from the one in "Cmedit.Image".
lzwDecode :: Bool -> Int -> BS.ByteString -> BS.ByteString
lzwDecode early lim bs = BS.concat (map BS.concat (groupsOf 4096 (go 0 9 initTable 256 Nothing 0)))
  where
    nbits = 8 * BS.length bs
    initTable = IM.fromList [ (i, BS.singleton (fromIntegral i)) | i <- [0 .. 255] ]
    -- The entries come out as a lazy list and are joined a few thousand at a
    -- time, so what is live is the output and one group — not a list cell
    -- for every code in the stream, which was 2 GB for a 64 MB one.
    groupsOf :: Int -> [a] -> [[a]]
    groupsOf _ [] = []
    groupsOf k xs = let (a, b) = splitAt k xs in a : groupsOf k b
    getCode p w
      | p + w > nbits = Nothing
      | otherwise =
          -- The code straddles at most three bytes; read them as one number
          -- and cut the code out of it.
          let b0 = p `shiftR` 3
              three = (fromIntegral (byteAt bs b0) `shiftL` 16)
                        .|. (fromIntegral (byteAt bs (b0 + 1)) `shiftL` 8)
                        .|. fromIntegral (byteAt bs (b0 + 2)) :: Int
          in Just ((three `shiftR` (24 - (p .&. 7) - w)) .&. ((1 `shiftL` w) - 1))
    -- @out@ is the running output length. @tn@ is the table's size, carried
    -- because asking an 'IM.IntMap' for it walks the whole map, and this
    -- asked twice per code.
    go :: Int -> Int -> IM.IntMap BS.ByteString -> Int -> Maybe BS.ByteString -> Int -> [BS.ByteString]
    go !p !w !table !tn prev !out
      | out > lim = []
      | otherwise = case getCode p w of
          Nothing -> []
          Just 257 -> []
          Just 256 -> go (p + w) 9 initTable 256 Nothing out
          Just code ->
            let next = tn + 2
                entry = case IM.lookup code table of
                  Just e  -> e
                  -- The one self-referential case: a code for an entry that
                  -- this very step is about to define.
                  Nothing -> case prev of
                               Just pv -> pv `BS.snoc` BS.head pv
                               Nothing -> BS.empty
                (table', tn') = case prev of
                  -- 4096 codes is all a 12-bit stream has; past that the
                  -- encoder owes us a clear code, and adding entries anyway
                  -- would only desynchronise the two tables.
                  Just pv | not (BS.null entry), next < 4096 ->
                              (IM.insert next (pv `BS.snoc` BS.head entry) table, tn + 1)
                  _ -> (table, tn)
                sz = tn' + 2
                -- PDF's default /EarlyChange widens the code one entry before
                -- the table actually needs it.
                -- (/EarlyChange 0 waits for the entry that needs the bit.)
                e = if early then 0 else 1
                w' | sz - e >= 2047 = 12
                   | sz - e >= 1023 = 11
                   | sz - e >= 511  = 10
                   | otherwise      = 9
            in if BS.null entry then []
               else entry : go (p + w) w' table' tn' (Just entry) (out + BS.length entry)

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
    viaTree = take maxPdfPages (reverse (fst (walk 0 M.empty ([], IS.empty) pagesRef)))
    inheritable = map BC.pack ["Resources", "MediaBox", "CropBox", "Rotate"]
    -- The visited set is global, not the ancestor path: a page tree is a
    -- tree, so no node is reached twice, and with only the path checked a
    -- file listing the same child several times per level made the walk
    -- exponential in the depth (4^40 visits from 3 KB, and no pages at the
    -- end of them for 'take' to stop on).
    walk depth inh (acc, seen) node
      | depth > (64 :: Int) = (acc, seen)
      | ORef r <- node, r `IS.member` seen = (acc, seen)
      | length acc >= maxPdfPages = (acc, seen)
      | otherwise =
          let seen' = case node of ORef r -> IS.insert r seen; _ -> seen
              d = dictOf f node
              inh' = foldl' (\a k -> case M.lookup k d of
                                       Just v  -> M.insert k v a
                                       Nothing -> a) inh inheritable
              ty = M.lookup (BC.pack "Type") d
              kids = arrOf f (M.findWithDefault ONull (BC.pack "Kids") d)
          in if ty == Just (OName (BC.pack "Page")) || (null kids && M.member (BC.pack "Contents") d)
               then (M.union d inh' : acc, seen')
               else foldl' (walk (depth + 1) inh') (acc, seen') kids

------------------------------------------------------------------------------
-- Pages: geometry and content

-- | The box the text view lays a page out in: the @\/CropBox@ when the page
-- has one, else the @\/MediaBox@, else US Letter, as a tuple
-- @(x0, y0, x1, y1)@. (A renderer wants 'pageCropBox', which also clips the
-- crop box to the media box.)
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
--
-- Each stream is charged to the document's decoding budget, and a page stops
-- taking more once it holds 'maxStreamBytes' of content: the array is the
-- file's, and may name one stream any number of times.
contentsOf :: PdfFile -> Fuel -> Dict -> (BS.ByteString, Fuel)
contentsOf f fu0 d = (BS.intercalate (BC.pack "\n") (reverse parts), fuEnd)
  where
    (parts, _, fuEnd) = foldl' one ([], 0 :: Int, fu0) streams
    streams = case M.findWithDefault ONull (BC.pack "Contents") d of
      OArr xs -> map (resolve f) xs
      o       -> [resolve f o]
    one (acc, !tot, !fu) (OStream sd raw)
      | tot < maxStreamBytes =
          let (r, used) = decodeStream f (fuBytes fu) sd raw
              fu' = fu { fuBytes = fuBytes fu - used }
          in case r of
               Right b -> (b : acc, tot + BS.length b, fu')
               Left _  -> (acc, tot, fu')
    one st _ = st

------------------------------------------------------------------------------
-- Content streams

-- | One content-stream token.
data Tok
  = TObj !Obj
  | TOp !BS.ByteString
  | TInline !Dict !BS.ByteString
    -- ^ An inline image (@BI … ID … EI@): its dictionary exactly as written
    -- (abbreviated keys and all) and its raw, still-encoded data. Only
    -- 'contentTokensInline' produces these.
  deriving (Show)

-- | Split a content stream into operands and operators.
--
-- Inline images (@BI … ID …binary… EI@) are the one construct that cannot be
-- tokenised as objects, since the bytes after @ID@ are raw samples that may
-- contain anything. They are skipped whole: this is the text view's
-- tokenizer, and it has no use for pixels.
contentTokens :: BS.ByteString -> [Tok]
contentTokens = tokensWith False

-- | 'contentTokens' for a renderer: inline images come back as 'TInline'
-- in place of the @BI@ … @EI@ operators.
--
-- Where an inline image's data ends is the one genuinely ambiguous question
-- in the content-stream grammar (the samples may contain @EI@). When the
-- image is unfiltered and its dictionary determines its size, the data is
-- exactly that many bytes and the @EI@ after it is only checked for;
-- otherwise the data ends at the first @EI@ that has whitespace before it,
-- whitespace or a delimiter after it, and plausible content-stream text
-- (printable ASCII) following, which is the heuristic every viewer uses.
contentTokensInline :: BS.ByteString -> [Tok]
contentTokensInline = tokensWith True

tokensWith :: Bool -> BS.ByteString -> [Tok]
tokensWith keepImages bs = go 0
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
             else if kw == BC.pack "BI"
               then (if keepImages then inlineImage k else go (skipInlineImage k))
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

    -- The dictionary: key/value pairs up to the ID keyword. Bounded, and junk
    -- where a key belongs is stepped over, as in 'parseDictAt'.
    inlineImage i0 = dictLoop i0 M.empty (0 :: Int)
      where
        dictLoop !i !acc !cnt
          | j >= n = []
          | cnt > 256 = []
          | byteAt bs j == 47 =
              let (key, j1) = readName bs (j + 1)
              in case parseObj bs j1 of
                   Just (v, j2) | j2 > j1 -> dictLoop j2 (M.insert key v acc) (cnt + 1)
                   _ -> dictLoop j1 acc (cnt + 1)
          | otherwise =
              let (kw, k) = readRegular bs j
              in if kw == BC.pack "ID"
                   then let s = if isWsB (byteAt bs k) then k + 1 else k
                            (dat, after) = inlineData acc s
                        in TInline acc dat : go after
                 else if kw == BC.pack "EI" then go k      -- an image with no data
                 else if BS.null kw then dictLoop (j + 1) acc (cnt + 1)
                 else dictLoop k acc (cnt + 1)
          where j = skipWs bs i

    inlineData d s = case inlineLength d of
      Just len | s + len <= n
               , let e = skipWs bs (s + len)
               , isEI e -> (BS.take len (BS.drop s bs), e + 2)
      _ -> searchEI s s
      where
        searchEI start !k
          | k + 1 >= n = (BS.drop start bs, n)
          | BS.index bs k == 69 && BS.index bs (k + 1) == 73
          , k == start || isWsB (byteAt bs (k - 1))
          , isEI k
          , plausibleAfter (k + 2) =
              let end = if k > start && isWsB (byteAt bs (k - 1)) then k - 1 else k
              in (BS.take (end - start) (BS.drop start bs), k + 2)
          | otherwise = searchEI start (k + 1)
    isEI k = byteAt bs k == 69 && byteAt bs (k + 1) == 73
               && (k + 2 >= n || isWsB (byteAt bs (k + 2)) || isDelimB (byteAt bs (k + 2)))
    -- What follows a real EI is more content stream: operators and operands,
    -- printable ASCII. Sample bytes rarely manage sixteen of those in a row.
    plausibleAfter k = BS.all (\c -> isWsB c || (c >= 32 && c < 127)) (BS.take 16 (BS.drop k bs))

-- | The byte length of an unfiltered inline image whose dictionary says how
-- big it is; 'Nothing' when it is filtered or the colour space is a named
-- resource (whose component count is not known here).
inlineLength :: Dict -> Maybe Int
inlineLength d
  | any (`M.member` d) (map BC.pack ["F", "Filter"]) = Nothing
  | otherwise = do
      w <- posInt ["W", "Width"]
      h <- posInt ["H", "Height"]
      (comps, bpc) <- if isMask then Just (1, 1) else do
        b <- posInt ["BPC", "BitsPerComponent"]
        c <- comps0
        Just (c, b)
      if w > 1000000 || h > 1000000 || bpc > 16 then Nothing
        else Just (h * ((w * comps * bpc + 7) `div` 8))
  where
    get ks = case [ v | k <- ks, Just v <- [M.lookup (BC.pack k) d] ] of
               (v : _) -> Just v
               []      -> Nothing
    posInt ks = case get ks of
      Just (ONum v) | v >= 1 -> Just (truncate v)
      _ -> Nothing
    isMask = get ["IM", "ImageMask"] == Just (OBool True)
    comps0 = case get ["CS", "ColorSpace"] of
      Just (OName c) -> named c
      Just (OArr (OName c : _)) | c `elem` map BC.pack ["I", "Indexed"] -> Just 1
      _ -> Nothing
    named c = case BC.unpack c of
      "G" -> Just 1; "DeviceGray" -> Just 1
      "RGB" -> Just 3; "DeviceRGB" -> Just 3
      "CMYK" -> Just 4; "DeviceCMYK" -> Just 4
      _ -> Nothing

------------------------------------------------------------------------------
-- Encodings by character (the text view's tables)

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
  ('u' : hx) | length hx >= 4, all isHexDigit hx
             , let v = foldl' (\a c -> a * 16 + digitToInt c) 0 (take 6 hx), v <= 0x10ffff ->
    -- (bounded: 'chr' throws past U+10FFFF, and the name is the file's)
    Just (chr v)
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

------------------------------------------------------------------------------
-- Opening a file

-- | Parse a whole file into its object graph: the scan, the object streams
-- and the trailer. 'Left' for something that is not a PDF, is larger than
-- 'maxPdfBytes', or is encrypted (reported, never guessed at).
--
-- The budget is what expanding the object streams may decode (normally
-- 'maxPdfDecoded'); what is left of it comes back with the file, so a caller
-- that goes on to decode content streams starts from the same account.
openPdfFile :: Int -> BS.ByteString -> Either String (PdfFile, Int)
openPdfFile budget bs
  | not (sniffPdf bs) = Left "not a PDF"
  | BS.length bs > maxPdfBytes =
      Left ("PDF too large to read (" ++ show (BS.length bs `div` (1024 * 1024)) ++ " MB)")
  | M.member (BC.pack "Encrypt") trailer =
      Left "encrypted PDF \x2014 this view cannot decrypt it"
  | otherwise = Right (PdfFile objs trailer, bytesLeft)
  where
    (scanned, bytesLeft) = expandObjStms bs budget (scanObjects bs)
    objs = IM.map snd scanned
    trailer = findTrailer bs objs

-- | 'openPdfFile' under the default budget, for a renderer working from bytes.
parsePdfFile :: BS.ByteString -> Either String PdfFile
parsePdfFile = fmap fst . openPdfFile maxPdfDecoded

-- | The page dictionaries in reading order, each with its inherited
-- attributes (@\/Resources@, @\/MediaBox@, @\/CropBox@, @\/Rotate@) merged in.
-- At most 'maxPdfPages'.
pdfPages :: PdfFile -> [Dict]
pdfPages = collectPages

-- | The @%PDF-1.x@ header version (empty when there is none).
pdfVersion :: BS.ByteString -> BS.ByteString
pdfVersion bs =
  let (_, post) = BS.breakSubstring (BC.pack "%PDF-") (BS.take 1024 bs)
  in BS.take 3 (BS.drop 5 post)

-- | Every object in a byte string, in order (a CMap body, a @\/ToUnicode@
-- section, an inline image's dictionary). Stops at the first position where
-- nothing parses.
parseObjects :: BS.ByteString -> [Obj]
parseObjects b = go 0
  where
    go i = case parseObj b i of
      Just (o, j) | j > i -> o : go j
      _ -> []

------------------------------------------------------------------------------
-- More accessors

-- | The first of several keys present (inline images abbreviate theirs:
-- @lookDAny f d ["BitsPerComponent", "BPC"]@).
lookDAny :: PdfFile -> Dict -> [String] -> Obj
lookDAny f d ks = case [ v | k <- ks, Just v <- [M.lookup (BC.pack k) d] ] of
  (v : _) -> resolve f v
  []      -> ONull

boolOf :: PdfFile -> Obj -> Maybe Bool
boolOf f o = case resolve f o of
  OBool b -> Just b
  _       -> Nothing

strOf :: PdfFile -> Obj -> Maybe BS.ByteString
strOf f o = case resolve f o of
  OStr s -> Just s
  _      -> Nothing

-- | An array of numbers; anything that is not a number reads as 0, as
-- 'numOf' does everywhere else.
numsOf :: PdfFile -> Obj -> [Double]
numsOf f o = [ numOf f (Just x) | x <- arrOf f o ]

-- | A six-number matrix array (@\/Matrix@, a pattern's matrix, a @cm@).
matrixOf :: PdfFile -> Obj -> Maybe Affine
matrixOf f o = case numsOf f o of
  [a, b, c, d, e, ff] -> Just (Affine a b c d e ff)
  _ -> Nothing

-- | A four-number rectangle array, normalised so @x0 <= x1@ and @y0 <= y1@
-- (PDF allows any two opposite corners). PDF's own y-up coordinates.
rectOf :: PdfFile -> Obj -> Maybe Rect
rectOf f o = case numsOf f o of
  [a, b, c, d] -> Just (Rect (min a c) (min b d) (max a c) (max b d))
  _ -> Nothing

-- | The object number an entry refers to, if it is a reference: what a cache
-- of decoded fonts or images can be keyed on.
refOf :: Obj -> Maybe Int
refOf (ORef r) = Just r
refOf _ = Nothing

------------------------------------------------------------------------------
-- The document budget

-- | What the document as a whole may still spend. Threaded through every
-- page, so the limits are on the file and not on any one part of it.
data Fuel = Fuel
  { fuBytes   :: !Int   -- ^ Left of 'maxPdfDecoded'.
  , fuTokens  :: !Int   -- ^ Left of 'maxPdfTokens'.
  , fuEntries :: !Int   -- ^ Left of 'maxPdfCMapEntries'.
  } deriving (Eq, Show)

fuel0 :: Int -> Int -> Fuel
fuel0 bytes tokens = Fuel bytes tokens maxPdfCMapEntries

fuelDefault :: Fuel
fuelDefault = fuel0 maxPdfDecoded maxPdfTokens

-- | Has the document spent any of its budgets?
fuelSpent :: Fuel -> Bool
fuelSpent fu = fuBytes fu <= 0 || fuTokens fu <= 0

spendBytes, spendTokens, spendEntries :: Int -> Fuel -> Fuel
spendBytes k fu = fu { fuBytes = fuBytes fu - k }
spendTokens k fu = fu { fuTokens = fuTokens fu - k }
spendEntries k fu = fu { fuEntries = fuEntries fu - k }

-- | 'decodeStream' charged to a 'Fuel'.
decodeStreamF :: PdfFile -> Fuel -> Dict -> BS.ByteString -> (Either String BS.ByteString, Fuel)
decodeStreamF f fu d raw =
  let (r, spent) = decodeStream f (fuBytes fu) d raw in (r, spendBytes spent fu)

------------------------------------------------------------------------------
-- Pages

-- | The page's @\/MediaBox@ (US Letter when it has none, or one that encloses
-- nothing). PDF's own y-up user space.
pageMediaBox :: PdfFile -> Dict -> Rect
pageMediaBox f d = case rectOf f (M.findWithDefault ONull (BC.pack "MediaBox") d) of
  Just r | rectW' r > 1 && rectH' r > 1 -> r
  _ -> Rect 0 0 612 792
  where rectW' (Rect x0 _ x1 _) = x1 - x0
        rectH' (Rect _ y0 _ y1) = y1 - y0

-- | The visible region: @\/CropBox@ clipped to the media box, or the media box
-- itself when the crop box is absent or encloses nothing.
pageCropBox :: PdfFile -> Dict -> Rect
pageCropBox f d = case rectOf f (M.findWithDefault ONull (BC.pack "CropBox") d) of
  Just c | let r = rectIntersect c media, not (rectEmpty r) -> r
  _ -> media
  where media = pageMediaBox f d

-- | @\/Rotate@ as 0, 90, 180 or 270 (clockwise). A value that is not a
-- multiple of 90 is invalid and, as every viewer does, ignored.
pageRotation :: PdfFile -> Dict -> Int
pageRotation f d =
  let r = ((intOf f (M.lookup (BC.pack "Rotate") d) `mod` 360) + 360) `mod` 360
  in if r `mod` 90 == 0 then r else 0

------------------------------------------------------------------------------
-- Encodings by glyph name
--
-- A font program is indexed by glyph /name/ (Type 1 and CFF charstrings, a
-- TrueType @post@ table), so a renderer needs the standard encodings as names,
-- where the text view needs them as characters. The tables are PDF 32000
-- Annex D, generated from Ghostscript's copies; @"."@ marks an unused code.

encodingTable :: [String] -> IM.IntMap BS.ByteString
encodingTable rows =
  IM.fromList [ (i, BC.pack nm) | (i, nm) <- zip [0 ..] (concatMap words rows), nm /= "." ]

-- | A base encoding by its PDF name (@\/StandardEncoding@,
-- @\/WinAnsiEncoding@, @\/MacRomanEncoding@, @\/MacExpertEncoding@), or the
-- built-in encodings of the two symbolic standard-14 fonts by the font's
-- name (@Symbol@, @ZapfDingbats@).
encodingByName :: BS.ByteString -> Maybe (IM.IntMap BS.ByteString)
encodingByName n = case BC.unpack n of
  "StandardEncoding"  -> Just standardEncodingNames
  "WinAnsiEncoding"   -> Just winAnsiEncodingNames
  "MacRomanEncoding"  -> Just macRomanEncodingNames
  "MacExpertEncoding" -> Just macExpertEncodingNames
  "Symbol"            -> Just symbolEncodingNames
  "ZapfDingbats"      -> Just zapfDingbatsEncodingNames
  _                   -> Nothing

-- | Apply an encoding dictionary's @\/Differences@ (@code \/name code \/name
-- …@) over a name-valued base encoding. Codes outside 0–255 are dropped.
differences :: PdfFile -> Obj -> IM.IntMap BS.ByteString -> IM.IntMap BS.ByteString
differences f enc base = case resolve f enc of
  ODict d -> go (arrOf f (M.findWithDefault ONull (BC.pack "Differences") d)) 0 base
  _       -> base
  where
    go (o : rest) !cur acc = case resolve f o of
      ONum v  -> go rest (truncate v) acc
      OName n -> go rest (cur + 1) (if cur >= 0 && cur <= 255 then IM.insert cur n acc else acc)
      _       -> go rest cur acc
    go [] _ acc = acc

-- | A glyph name to the Unicode code point it draws: the standard encodings'
-- names (Adobe Glyph List values), the text view's Latin table, and the
-- @uniXXXX@ / @uXXXX[XX]@ conventions. A name with a suffix (@a.sc@,
-- @f_i@ is not split) is looked up by its part before the first period.
glyphUnicode :: BS.ByteString -> Maybe Int
glyphUnicode nm0
  | Just v <- M.lookup nm standardGlyphMap = Just v
  | Just c <- glyphChar (BC.unpack nm) = Just (fromEnum c)
  | otherwise = Nothing
  where nm = BC.takeWhile (/= '.') nm0

standardGlyphMap :: M.Map BS.ByteString Int
standardGlyphMap = M.fromList standardGlyphUnicode

-- | The Type 1 standard encoding (PDF 32000 Annex D).
standardEncodingNames :: IM.IntMap BS.ByteString
standardEncodingNames = encodingTable
  [ ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , "space exclam quotedbl numbersign dollar percent ampersand quoteright"
  , "parenleft parenright asterisk plus comma hyphen period slash"
  , "zero one two three four five six seven"
  , "eight nine colon semicolon less equal greater question"
  , "at A B C D E F G"
  , "H I J K L M N O"
  , "P Q R S T U V W"
  , "X Y Z bracketleft backslash bracketright asciicircum underscore"
  , "quoteleft a b c d e f g"
  , "h i j k l m n o"
  , "p q r s t u v w"
  , "x y z braceleft bar braceright asciitilde ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". exclamdown cent sterling fraction yen florin section"
  , "currency quotesingle quotedblleft guillemotleft guilsinglleft guilsinglright fi fl"
  , ". endash dagger daggerdbl periodcentered . paragraph bullet"
  , "quotesinglbase quotedblbase quotedblright guillemotright ellipsis perthousand . questiondown"
  , ". grave acute circumflex tilde macron breve dotaccent"
  , "dieresis . ring cedilla . hungarumlaut ogonek caron"
  , "emdash . . . . . . ."
  , ". . . . . . . ."
  , ". AE . ordfeminine . . . ."
  , "Lslash Oslash OE ordmasculine . . . ."
  , ". ae . . . dotlessi . ."
  , "lslash oslash oe germandbls . . . ." ]

-- | WinAnsiEncoding as PDF defines it (Windows-1252 with the unused codes drawn as bullets, 0xA0 as space and 0xAD as hyphen).
winAnsiEncodingNames :: IM.IntMap BS.ByteString
winAnsiEncodingNames = encodingTable
  [ ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , "space exclam quotedbl numbersign dollar percent ampersand quotesingle"
  , "parenleft parenright asterisk plus comma hyphen period slash"
  , "zero one two three four five six seven"
  , "eight nine colon semicolon less equal greater question"
  , "at A B C D E F G"
  , "H I J K L M N O"
  , "P Q R S T U V W"
  , "X Y Z bracketleft backslash bracketright asciicircum underscore"
  , "grave a b c d e f g"
  , "h i j k l m n o"
  , "p q r s t u v w"
  , "x y z braceleft bar braceright asciitilde bullet"
  , "Euro bullet quotesinglbase florin quotedblbase ellipsis dagger daggerdbl"
  , "circumflex perthousand Scaron guilsinglleft OE bullet Zcaron bullet"
  , "bullet quoteleft quoteright quotedblleft quotedblright bullet endash emdash"
  , "tilde trademark scaron guilsinglright oe bullet zcaron Ydieresis"
  , "space exclamdown cent sterling currency yen brokenbar section"
  , "dieresis copyright ordfeminine guillemotleft logicalnot hyphen registered macron"
  , "degree plusminus twosuperior threesuperior acute mu paragraph periodcentered"
  , "cedilla onesuperior ordmasculine guillemotright onequarter onehalf threequarters questiondown"
  , "Agrave Aacute Acircumflex Atilde Adieresis Aring AE Ccedilla"
  , "Egrave Eacute Ecircumflex Edieresis Igrave Iacute Icircumflex Idieresis"
  , "Eth Ntilde Ograve Oacute Ocircumflex Otilde Odieresis multiply"
  , "Oslash Ugrave Uacute Ucircumflex Udieresis Yacute Thorn germandbls"
  , "agrave aacute acircumflex atilde adieresis aring ae ccedilla"
  , "egrave eacute ecircumflex edieresis igrave iacute icircumflex idieresis"
  , "eth ntilde ograve oacute ocircumflex otilde odieresis divide"
  , "oslash ugrave uacute ucircumflex udieresis yacute thorn ydieresis" ]

-- | MacRomanEncoding as PDF defines it (0xDB is currency, not the later Euro).
macRomanEncodingNames :: IM.IntMap BS.ByteString
macRomanEncodingNames = encodingTable
  [ ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , "space exclam quotedbl numbersign dollar percent ampersand quotesingle"
  , "parenleft parenright asterisk plus comma hyphen period slash"
  , "zero one two three four five six seven"
  , "eight nine colon semicolon less equal greater question"
  , "at A B C D E F G"
  , "H I J K L M N O"
  , "P Q R S T U V W"
  , "X Y Z bracketleft backslash bracketright asciicircum underscore"
  , "grave a b c d e f g"
  , "h i j k l m n o"
  , "p q r s t u v w"
  , "x y z braceleft bar braceright asciitilde ."
  , "Adieresis Aring Ccedilla Eacute Ntilde Odieresis Udieresis aacute"
  , "agrave acircumflex adieresis atilde aring ccedilla eacute egrave"
  , "ecircumflex edieresis iacute igrave icircumflex idieresis ntilde oacute"
  , "ograve ocircumflex odieresis otilde uacute ugrave ucircumflex udieresis"
  , "dagger degree cent sterling section bullet paragraph germandbls"
  , "registered copyright trademark acute dieresis . AE Oslash"
  , ". plusminus . . yen mu . ."
  , ". . . ordfeminine ordmasculine . ae oslash"
  , "questiondown exclamdown logicalnot . florin . . guillemotleft"
  , "guillemotright ellipsis space Agrave Atilde Otilde OE oe"
  , "endash emdash quotedblleft quotedblright quoteleft quoteright divide ."
  , "ydieresis Ydieresis fraction currency guilsinglleft guilsinglright fi fl"
  , "daggerdbl periodcentered quotesinglbase quotedblbase perthousand Acircumflex Ecircumflex Aacute"
  , "Edieresis Egrave Iacute Icircumflex Idieresis Igrave Oacute Ocircumflex"
  , ". Ograve Uacute Ucircumflex Ugrave dotlessi circumflex tilde"
  , "macron breve dotaccent ring cedilla hungarumlaut ogonek caron" ]

-- | MacExpertEncoding (old-style figures, small capitals, fractions).
macExpertEncodingNames :: IM.IntMap BS.ByteString
macExpertEncodingNames = encodingTable
  [ ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , "space exclamsmall Hungarumlautsmall centoldstyle dollaroldstyle dollarsuperior ampersandsmall Acutesmall"
  , "parenleftsuperior parenrightsuperior twodotenleader onedotenleader comma hyphen period fraction"
  , "zerooldstyle oneoldstyle twooldstyle threeoldstyle fouroldstyle fiveoldstyle sixoldstyle sevenoldstyle"
  , "eightoldstyle nineoldstyle colon semicolon . threequartersemdash . questionsmall"
  , ". . . . Ethsmall . . onequarter"
  , "onehalf threequarters oneeighth threeeighths fiveeighths seveneighths onethird twothirds"
  , ". . . . . . ff fi"
  , "fl ffi ffl parenleftinferior . parenrightinferior Circumflexsmall hypheninferior"
  , "Gravesmall Asmall Bsmall Csmall Dsmall Esmall Fsmall Gsmall"
  , "Hsmall Ismall Jsmall Ksmall Lsmall Msmall Nsmall Osmall"
  , "Psmall Qsmall Rsmall Ssmall Tsmall Usmall Vsmall Wsmall"
  , "Xsmall Ysmall Zsmall colonmonetary onefitted rupiah Tildesmall ."
  , ". asuperior centsuperior . . . . Aacutesmall"
  , "Agravesmall Acircumflexsmall Adieresissmall Atildesmall Aringsmall Ccedillasmall Eacutesmall Egravesmall"
  , "Ecircumflexsmall Edieresissmall Iacutesmall Igravesmall Icircumflexsmall Idieresissmall Ntildesmall Oacutesmall"
  , "Ogravesmall Ocircumflexsmall Odieresissmall Otildesmall Uacutesmall Ugravesmall Ucircumflexsmall Udieresissmall"
  , ". eightsuperior fourinferior threeinferior sixinferior eightinferior seveninferior Scaronsmall"
  , ". centinferior twoinferior . Dieresissmall . Caronsmall osuperior"
  , "fiveinferior . commainferior periodinferior Yacutesmall . dollarinferior ."
  , ". Thornsmall . nineinferior zeroinferior Zcaronsmall AEsmall Oslashsmall"
  , "questiondownsmall oneinferior Lslashsmall . . . . ."
  , ". Cedillasmall . . . . . OEsmall"
  , "figuredash hyphensuperior . . . . exclamdownsmall ."
  , "Ydieresissmall . onesuperior twosuperior threesuperior foursuperior fivesuperior sixsuperior"
  , "sevensuperior ninesuperior zerosuperior . esuperior rsuperior tsuperior ."
  , ". isuperior ssuperior dsuperior . . . ."
  , ". lsuperior Ogoneksmall Brevesmall Macronsmall bsuperior nsuperior msuperior"
  , "commasuperior periodsuperior Dotaccentsmall Ringsmall . . . ." ]

-- | The built-in encoding of the standard-14 Symbol font.
symbolEncodingNames :: IM.IntMap BS.ByteString
symbolEncodingNames = encodingTable
  [ ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , "space exclam universal numbersign existential percent ampersand suchthat"
  , "parenleft parenright asteriskmath plus comma minus period slash"
  , "zero one two three four five six seven"
  , "eight nine colon semicolon less equal greater question"
  , "congruent Alpha Beta Chi Delta Epsilon Phi Gamma"
  , "Eta Iota theta1 Kappa Lambda Mu Nu Omicron"
  , "Pi Theta Rho Sigma Tau Upsilon sigma1 Omega"
  , "Xi Psi Zeta bracketleft therefore bracketright perpendicular underscore"
  , "radicalex alpha beta chi delta epsilon phi gamma"
  , "eta iota phi1 kappa lambda mu nu omicron"
  , "pi theta rho sigma tau upsilon omega1 omega"
  , "xi psi zeta braceleft bar braceright similar ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , "Euro Upsilon1 minute lessequal fraction infinity florin club"
  , "diamond heart spade arrowboth arrowleft arrowup arrowright arrowdown"
  , "degree plusminus second greaterequal multiply proportional partialdiff bullet"
  , "divide notequal equivalence approxequal ellipsis arrowvertex arrowhorizex carriagereturn"
  , "aleph Ifraktur Rfraktur weierstrass circlemultiply circleplus emptyset intersection"
  , "union propersuperset reflexsuperset notsubset propersubset reflexsubset element notelement"
  , "angle gradient registerserif copyrightserif trademarkserif product radical dotmath"
  , "logicalnot logicaland logicalor arrowdblboth arrowdblleft arrowdblup arrowdblright arrowdbldown"
  , "lozenge angleleft registersans copyrightsans trademarksans summation parenlefttp parenleftex"
  , "parenleftbt bracketlefttp bracketleftex bracketleftbt bracelefttp braceleftmid braceleftbt braceex"
  , ". angleright integral integraltp integralex integralbt parenrighttp parenrightex"
  , "parenrightbt bracketrighttp bracketrightex bracketrightbt bracerighttp bracerightmid bracerightbt ." ]

-- | The built-in encoding of the standard-14 ZapfDingbats font.
zapfDingbatsEncodingNames :: IM.IntMap BS.ByteString
zapfDingbatsEncodingNames = encodingTable
  [ ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , "space a1 a2 a202 a3 a4 a5 a119"
  , "a118 a117 a11 a12 a13 a14 a15 a16"
  , "a105 a17 a18 a19 a20 a21 a22 a23"
  , "a24 a25 a26 a27 a28 a6 a7 a8"
  , "a9 a10 a29 a30 a31 a32 a33 a34"
  , "a35 a36 a37 a38 a39 a40 a41 a42"
  , "a43 a44 a45 a46 a47 a48 a49 a50"
  , "a51 a52 a53 a54 a55 a56 a57 a58"
  , "a59 a60 a61 a62 a63 a64 a65 a66"
  , "a67 a68 a69 a70 a71 a72 a73 a74"
  , "a203 a75 a204 a76 a77 a78 a79 a81"
  , "a82 a83 a84 a97 a98 a99 a100 ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". . . . . . . ."
  , ". a101 a102 a103 a104 a106 a107 a108"
  , "a112 a111 a110 a109 a120 a121 a122 a123"
  , "a124 a125 a126 a127 a128 a129 a130 a131"
  , "a132 a133 a134 a135 a136 a137 a138 a139"
  , "a140 a141 a142 a143 a144 a145 a146 a147"
  , "a148 a149 a150 a151 a152 a153 a154 a155"
  , "a156 a157 a158 a159 a160 a161 a163 a164"
  , "a196 a165 a192 a166 a167 a168 a169 a170"
  , "a171 a172 a173 a162 a174 a175 a176 a177"
  , "a178 a179 a193 a180 a199 a181 a200 a182"
  , ". a201 a183 a184 a197 a185 a194 a198"
  , "a186 a195 a187 a188 a189 a190 a191 ." ]

-- | Glyph name to Unicode for every name the standard encodings use (taken
-- from the Adobe Glyph List; the ZapfDingbats @aN@ names are not in it).
standardGlyphUnicode :: [(BS.ByteString, Int)]
standardGlyphUnicode =
  [ ("A", 0x0041), ("AE", 0x00c6), ("AEsmall", 0xf7e6), ("Aacute", 0x00c1)
  , ("Aacutesmall", 0xf7e1), ("Acircumflex", 0x00c2), ("Acircumflexsmall", 0xf7e2), ("Acutesmall", 0xf7b4)
  , ("Adieresis", 0x00c4), ("Adieresissmall", 0xf7e4), ("Agrave", 0x00c0), ("Agravesmall", 0xf7e0)
  , ("Alpha", 0x0391), ("Aring", 0x00c5), ("Aringsmall", 0xf7e5), ("Asmall", 0xf761)
  , ("Atilde", 0x00c3), ("Atildesmall", 0xf7e3), ("B", 0x0042), ("Beta", 0x0392)
  , ("Brevesmall", 0xf6f4), ("Bsmall", 0xf762), ("C", 0x0043), ("Caronsmall", 0xf6f5)
  , ("Ccedilla", 0x00c7), ("Ccedillasmall", 0xf7e7), ("Cedillasmall", 0xf7b8), ("Chi", 0x03a7)
  , ("Circumflexsmall", 0xf6f6), ("Csmall", 0xf763), ("D", 0x0044), ("Delta", 0x2206)
  , ("Dieresissmall", 0xf7a8), ("Dotaccentsmall", 0xf6f7), ("Dsmall", 0xf764), ("E", 0x0045)
  , ("Eacute", 0x00c9), ("Eacutesmall", 0xf7e9), ("Ecircumflex", 0x00ca), ("Ecircumflexsmall", 0xf7ea)
  , ("Edieresis", 0x00cb), ("Edieresissmall", 0xf7eb), ("Egrave", 0x00c8), ("Egravesmall", 0xf7e8)
  , ("Epsilon", 0x0395), ("Esmall", 0xf765), ("Eta", 0x0397), ("Eth", 0x00d0)
  , ("Ethsmall", 0xf7f0), ("Euro", 0x20ac), ("F", 0x0046), ("Fsmall", 0xf766)
  , ("G", 0x0047), ("Gamma", 0x0393), ("Gravesmall", 0xf760), ("Gsmall", 0xf767)
  , ("H", 0x0048), ("Hsmall", 0xf768), ("Hungarumlautsmall", 0xf6f8), ("I", 0x0049)
  , ("Iacute", 0x00cd), ("Iacutesmall", 0xf7ed), ("Icircumflex", 0x00ce), ("Icircumflexsmall", 0xf7ee)
  , ("Idieresis", 0x00cf), ("Idieresissmall", 0xf7ef), ("Ifraktur", 0x2111), ("Igrave", 0x00cc)
  , ("Igravesmall", 0xf7ec), ("Iota", 0x0399), ("Ismall", 0xf769), ("J", 0x004a)
  , ("Jsmall", 0xf76a), ("K", 0x004b), ("Kappa", 0x039a), ("Ksmall", 0xf76b)
  , ("L", 0x004c), ("Lambda", 0x039b), ("Lslash", 0x0141), ("Lslashsmall", 0xf6f9)
  , ("Lsmall", 0xf76c), ("M", 0x004d), ("Macronsmall", 0xf7af), ("Msmall", 0xf76d)
  , ("Mu", 0x039c), ("N", 0x004e), ("Nsmall", 0xf76e), ("Ntilde", 0x00d1)
  , ("Ntildesmall", 0xf7f1), ("Nu", 0x039d), ("O", 0x004f), ("OE", 0x0152)
  , ("OEsmall", 0xf6fa), ("Oacute", 0x00d3), ("Oacutesmall", 0xf7f3), ("Ocircumflex", 0x00d4)
  , ("Ocircumflexsmall", 0xf7f4), ("Odieresis", 0x00d6), ("Odieresissmall", 0xf7f6), ("Ogoneksmall", 0xf6fb)
  , ("Ograve", 0x00d2), ("Ogravesmall", 0xf7f2), ("Omega", 0x2126), ("Omicron", 0x039f)
  , ("Oslash", 0x00d8), ("Oslashsmall", 0xf7f8), ("Osmall", 0xf76f), ("Otilde", 0x00d5)
  , ("Otildesmall", 0xf7f5), ("P", 0x0050), ("Phi", 0x03a6), ("Pi", 0x03a0)
  , ("Psi", 0x03a8), ("Psmall", 0xf770), ("Q", 0x0051), ("Qsmall", 0xf771)
  , ("R", 0x0052), ("Rfraktur", 0x211c), ("Rho", 0x03a1), ("Ringsmall", 0xf6fc)
  , ("Rsmall", 0xf772), ("S", 0x0053), ("Scaron", 0x0160), ("Scaronsmall", 0xf6fd)
  , ("Sigma", 0x03a3), ("Ssmall", 0xf773), ("T", 0x0054), ("Tau", 0x03a4)
  , ("Theta", 0x0398), ("Thorn", 0x00de), ("Thornsmall", 0xf7fe), ("Tildesmall", 0xf6fe)
  , ("Tsmall", 0xf774), ("U", 0x0055), ("Uacute", 0x00da), ("Uacutesmall", 0xf7fa)
  , ("Ucircumflex", 0x00db), ("Ucircumflexsmall", 0xf7fb), ("Udieresis", 0x00dc), ("Udieresissmall", 0xf7fc)
  , ("Ugrave", 0x00d9), ("Ugravesmall", 0xf7f9), ("Upsilon", 0x03a5), ("Upsilon1", 0x03d2)
  , ("Usmall", 0xf775), ("V", 0x0056), ("Vsmall", 0xf776), ("W", 0x0057)
  , ("Wsmall", 0xf777), ("X", 0x0058), ("Xi", 0x039e), ("Xsmall", 0xf778)
  , ("Y", 0x0059), ("Yacute", 0x00dd), ("Yacutesmall", 0xf7fd), ("Ydieresis", 0x0178)
  , ("Ydieresissmall", 0xf7ff), ("Ysmall", 0xf779), ("Z", 0x005a), ("Zcaron", 0x017d)
  , ("Zcaronsmall", 0xf6ff), ("Zeta", 0x0396), ("Zsmall", 0xf77a), ("a", 0x0061)
  , ("aacute", 0x00e1), ("acircumflex", 0x00e2), ("acute", 0x00b4), ("adieresis", 0x00e4)
  , ("ae", 0x00e6), ("agrave", 0x00e0), ("aleph", 0x2135), ("alpha", 0x03b1)
  , ("ampersand", 0x0026), ("ampersandsmall", 0xf726), ("angle", 0x2220), ("angleleft", 0x2329)
  , ("angleright", 0x232a), ("approxequal", 0x2248), ("aring", 0x00e5), ("arrowboth", 0x2194)
  , ("arrowdblboth", 0x21d4), ("arrowdbldown", 0x21d3), ("arrowdblleft", 0x21d0), ("arrowdblright", 0x21d2)
  , ("arrowdblup", 0x21d1), ("arrowdown", 0x2193), ("arrowhorizex", 0xf8e7), ("arrowleft", 0x2190)
  , ("arrowright", 0x2192), ("arrowup", 0x2191), ("arrowvertex", 0xf8e6), ("asciicircum", 0x005e)
  , ("asciitilde", 0x007e), ("asterisk", 0x002a), ("asteriskmath", 0x2217), ("asuperior", 0xf6e9)
  , ("at", 0x0040), ("atilde", 0x00e3), ("b", 0x0062), ("backslash", 0x005c)
  , ("bar", 0x007c), ("beta", 0x03b2), ("braceex", 0xf8f4), ("braceleft", 0x007b)
  , ("braceleftbt", 0xf8f3), ("braceleftmid", 0xf8f2), ("bracelefttp", 0xf8f1), ("braceright", 0x007d)
  , ("bracerightbt", 0xf8fe), ("bracerightmid", 0xf8fd), ("bracerighttp", 0xf8fc), ("bracketleft", 0x005b)
  , ("bracketleftbt", 0xf8f0), ("bracketleftex", 0xf8ef), ("bracketlefttp", 0xf8ee), ("bracketright", 0x005d)
  , ("bracketrightbt", 0xf8fb), ("bracketrightex", 0xf8fa), ("bracketrighttp", 0xf8f9), ("breve", 0x02d8)
  , ("brokenbar", 0x00a6), ("bsuperior", 0xf6ea), ("bullet", 0x2022), ("c", 0x0063)
  , ("caron", 0x02c7), ("carriagereturn", 0x21b5), ("ccedilla", 0x00e7), ("cedilla", 0x00b8)
  , ("cent", 0x00a2), ("centinferior", 0xf6df), ("centoldstyle", 0xf7a2), ("centsuperior", 0xf6e0)
  , ("chi", 0x03c7), ("circlemultiply", 0x2297), ("circleplus", 0x2295), ("circumflex", 0x02c6)
  , ("club", 0x2663), ("colon", 0x003a), ("colonmonetary", 0x20a1), ("comma", 0x002c)
  , ("commainferior", 0xf6e1), ("commasuperior", 0xf6e2), ("congruent", 0x2245), ("copyright", 0x00a9)
  , ("copyrightsans", 0xf8e9), ("copyrightserif", 0xf6d9), ("currency", 0x00a4), ("d", 0x0064)
  , ("dagger", 0x2020), ("daggerdbl", 0x2021), ("degree", 0x00b0), ("delta", 0x03b4)
  , ("diamond", 0x2666), ("dieresis", 0x00a8), ("divide", 0x00f7), ("dollar", 0x0024)
  , ("dollarinferior", 0xf6e3), ("dollaroldstyle", 0xf724), ("dollarsuperior", 0xf6e4), ("dotaccent", 0x02d9)
  , ("dotlessi", 0x0131), ("dotmath", 0x22c5), ("dsuperior", 0xf6eb), ("e", 0x0065)
  , ("eacute", 0x00e9), ("ecircumflex", 0x00ea), ("edieresis", 0x00eb), ("egrave", 0x00e8)
  , ("eight", 0x0038), ("eightinferior", 0x2088), ("eightoldstyle", 0xf738), ("eightsuperior", 0x2078)
  , ("element", 0x2208), ("ellipsis", 0x2026), ("emdash", 0x2014), ("emptyset", 0x2205)
  , ("endash", 0x2013), ("epsilon", 0x03b5), ("equal", 0x003d), ("equivalence", 0x2261)
  , ("esuperior", 0xf6ec), ("eta", 0x03b7), ("eth", 0x00f0), ("exclam", 0x0021)
  , ("exclamdown", 0x00a1), ("exclamdownsmall", 0xf7a1), ("exclamsmall", 0xf721), ("existential", 0x2203)
  , ("f", 0x0066), ("ff", 0xfb00), ("ffi", 0xfb03), ("ffl", 0xfb04)
  , ("fi", 0xfb01), ("figuredash", 0x2012), ("five", 0x0035), ("fiveeighths", 0x215d)
  , ("fiveinferior", 0x2085), ("fiveoldstyle", 0xf735), ("fivesuperior", 0x2075), ("fl", 0xfb02)
  , ("florin", 0x0192), ("four", 0x0034), ("fourinferior", 0x2084), ("fouroldstyle", 0xf734)
  , ("foursuperior", 0x2074), ("fraction", 0x2044), ("g", 0x0067), ("gamma", 0x03b3)
  , ("germandbls", 0x00df), ("gradient", 0x2207), ("grave", 0x0060), ("greater", 0x003e)
  , ("greaterequal", 0x2265), ("guillemotleft", 0x00ab), ("guillemotright", 0x00bb), ("guilsinglleft", 0x2039)
  , ("guilsinglright", 0x203a), ("h", 0x0068), ("heart", 0x2665), ("hungarumlaut", 0x02dd)
  , ("hyphen", 0x002d), ("hypheninferior", 0xf6e5), ("hyphensuperior", 0xf6e6), ("i", 0x0069)
  , ("iacute", 0x00ed), ("icircumflex", 0x00ee), ("idieresis", 0x00ef), ("igrave", 0x00ec)
  , ("infinity", 0x221e), ("integral", 0x222b), ("integralbt", 0x2321), ("integralex", 0xf8f5)
  , ("integraltp", 0x2320), ("intersection", 0x2229), ("iota", 0x03b9), ("isuperior", 0xf6ed)
  , ("j", 0x006a), ("k", 0x006b), ("kappa", 0x03ba), ("l", 0x006c)
  , ("lambda", 0x03bb), ("less", 0x003c), ("lessequal", 0x2264), ("logicaland", 0x2227)
  , ("logicalnot", 0x00ac), ("logicalor", 0x2228), ("lozenge", 0x25ca), ("lslash", 0x0142)
  , ("lsuperior", 0xf6ee), ("m", 0x006d), ("macron", 0x00af), ("minus", 0x2212)
  , ("minute", 0x2032), ("msuperior", 0xf6ef), ("mu", 0x00b5), ("multiply", 0x00d7)
  , ("n", 0x006e), ("nine", 0x0039), ("nineinferior", 0x2089), ("nineoldstyle", 0xf739)
  , ("ninesuperior", 0x2079), ("notelement", 0x2209), ("notequal", 0x2260), ("notsubset", 0x2284)
  , ("nsuperior", 0x207f), ("ntilde", 0x00f1), ("nu", 0x03bd), ("numbersign", 0x0023)
  , ("o", 0x006f), ("oacute", 0x00f3), ("ocircumflex", 0x00f4), ("odieresis", 0x00f6)
  , ("oe", 0x0153), ("ogonek", 0x02db), ("ograve", 0x00f2), ("omega", 0x03c9)
  , ("omega1", 0x03d6), ("omicron", 0x03bf), ("one", 0x0031), ("onedotenleader", 0x2024)
  , ("oneeighth", 0x215b), ("onefitted", 0xf6dc), ("onehalf", 0x00bd), ("oneinferior", 0x2081)
  , ("oneoldstyle", 0xf731), ("onequarter", 0x00bc), ("onesuperior", 0x00b9), ("onethird", 0x2153)
  , ("ordfeminine", 0x00aa), ("ordmasculine", 0x00ba), ("oslash", 0x00f8), ("osuperior", 0xf6f0)
  , ("otilde", 0x00f5), ("p", 0x0070), ("paragraph", 0x00b6), ("parenleft", 0x0028)
  , ("parenleftbt", 0xf8ed), ("parenleftex", 0xf8ec), ("parenleftinferior", 0x208d), ("parenleftsuperior", 0x207d)
  , ("parenlefttp", 0xf8eb), ("parenright", 0x0029), ("parenrightbt", 0xf8f8), ("parenrightex", 0xf8f7)
  , ("parenrightinferior", 0x208e), ("parenrightsuperior", 0x207e), ("parenrighttp", 0xf8f6), ("partialdiff", 0x2202)
  , ("percent", 0x0025), ("period", 0x002e), ("periodcentered", 0x00b7), ("periodinferior", 0xf6e7)
  , ("periodsuperior", 0xf6e8), ("perpendicular", 0x22a5), ("perthousand", 0x2030), ("phi", 0x03c6)
  , ("phi1", 0x03d5), ("pi", 0x03c0), ("plus", 0x002b), ("plusminus", 0x00b1)
  , ("product", 0x220f), ("propersubset", 0x2282), ("propersuperset", 0x2283), ("proportional", 0x221d)
  , ("psi", 0x03c8), ("q", 0x0071), ("question", 0x003f), ("questiondown", 0x00bf)
  , ("questiondownsmall", 0xf7bf), ("questionsmall", 0xf73f), ("quotedbl", 0x0022), ("quotedblbase", 0x201e)
  , ("quotedblleft", 0x201c), ("quotedblright", 0x201d), ("quoteleft", 0x2018), ("quoteright", 0x2019)
  , ("quotesinglbase", 0x201a), ("quotesingle", 0x0027), ("r", 0x0072), ("radical", 0x221a)
  , ("radicalex", 0xf8e5), ("reflexsubset", 0x2286), ("reflexsuperset", 0x2287), ("registered", 0x00ae)
  , ("registersans", 0xf8e8), ("registerserif", 0xf6da), ("rho", 0x03c1), ("ring", 0x02da)
  , ("rsuperior", 0xf6f1), ("rupiah", 0xf6dd), ("s", 0x0073), ("scaron", 0x0161)
  , ("second", 0x2033), ("section", 0x00a7), ("semicolon", 0x003b), ("seven", 0x0037)
  , ("seveneighths", 0x215e), ("seveninferior", 0x2087), ("sevenoldstyle", 0xf737), ("sevensuperior", 0x2077)
  , ("sigma", 0x03c3), ("sigma1", 0x03c2), ("similar", 0x223c), ("six", 0x0036)
  , ("sixinferior", 0x2086), ("sixoldstyle", 0xf736), ("sixsuperior", 0x2076), ("slash", 0x002f)
  , ("space", 0x0020), ("spade", 0x2660), ("ssuperior", 0xf6f2), ("sterling", 0x00a3)
  , ("suchthat", 0x220b), ("summation", 0x2211), ("t", 0x0074), ("tau", 0x03c4)
  , ("therefore", 0x2234), ("theta", 0x03b8), ("theta1", 0x03d1), ("thorn", 0x00fe)
  , ("three", 0x0033), ("threeeighths", 0x215c), ("threeinferior", 0x2083), ("threeoldstyle", 0xf733)
  , ("threequarters", 0x00be), ("threequartersemdash", 0xf6de), ("threesuperior", 0x00b3), ("tilde", 0x02dc)
  , ("trademark", 0x2122), ("trademarksans", 0xf8ea), ("trademarkserif", 0xf6db), ("tsuperior", 0xf6f3)
  , ("two", 0x0032), ("twodotenleader", 0x2025), ("twoinferior", 0x2082), ("twooldstyle", 0xf732)
  , ("twosuperior", 0x00b2), ("twothirds", 0x2154), ("u", 0x0075), ("uacute", 0x00fa)
  , ("ucircumflex", 0x00fb), ("udieresis", 0x00fc), ("ugrave", 0x00f9), ("underscore", 0x005f)
  , ("union", 0x222a), ("universal", 0x2200), ("upsilon", 0x03c5), ("v", 0x0076)
  , ("w", 0x0077), ("weierstrass", 0x2118), ("x", 0x0078), ("xi", 0x03be)
  , ("y", 0x0079), ("yacute", 0x00fd), ("ydieresis", 0x00ff), ("yen", 0x00a5)
  , ("z", 0x007a), ("zcaron", 0x017e), ("zero", 0x0030), ("zeroinferior", 0x2080)
  , ("zerooldstyle", 0xf730), ("zerosuperior", 0x2070), ("zeta", 0x03b6) ]
