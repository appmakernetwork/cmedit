{-# LANGUAGE BangPatterns #-}
-- | PostScript Type 1 fonts: PFA (hex or binary eexec section), PFB
-- (segmented), and PDF's @FontFile@ stream (with or without trustworthy
-- @Length1@\/@Length2@).
--
-- Load-bearing decisions:
--
-- * __The eexec boundary is found by the keyword, not the lengths.__ PDF
--   writers get @Length1@ wrong often enough that every serious reader
--   searches for @eexec@ and skips the whitespace after it; the encryption
--   itself guarantees the first cipher byte is not whitespace, which is
--   what makes skipping safe. The lengths are only a fallback for a font
--   whose cleartext has no keyword. Whether the section is hex or binary
--   is decided the way Adobe's spec says to: four hex digits in a row mean
--   hex.
--
-- * __No PostScript is executed.__ The font program is a PostScript
--   program, but every font in the wild follows Adobe's layout closely
--   enough to be /scanned/: @/FontMatrix [...]@, @dup <code> /<name> put@ in
--   the Encoding, @dup <i> <n> RD <binary>@ in Subrs and @/<name> <n> RD
--   <binary>@ in CharStrings. The one dynamic part is the name of the
--   binary-reading procedure (@RD@ or @-|@ by convention); any name the
--   Private dictionary defines over @readstring@ is accepted too.
--
-- * __OtherSubrs are the four standard ones, interpreted natively__: flex
--   (0–2: the seven @rmoveto@ points between @1 callothersubr@ and
--   @0 callothersubr@ become two curves) and hint replacement (3: returns its
--   argument so @pop callsubr@ calls the hint subroutine). Results travel on
--   a separate PostScript-stack list that @pop@ drains, exactly the
--   mechanism the spec describes.
--
-- * __@closepath@ keeps the current point__ (the FreeType/Ghostscript
--   reading); the next @rmoveto@ is relative to where the outline ended.
--
-- * __GIDs are CharStrings order with @.notdef@ swapped to 0__, FreeType's
--   convention, so GID 0 is the missing glyph in every format.
module Cmedit.FontT1
  ( parseType1
  , decryptT1
  , encryptT1
  ) where

import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import Data.Char (isDigit, isSpace, isHexDigit, digitToInt)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import qualified Data.Array as A
import qualified Data.Text as T
import Data.Word (Word8, Word16)

import Cmedit.Geom
import Cmedit.FontCore

------------------------------------------------------------------------------
-- Container

-- | Parse a Type 1 font. The optional lengths are PDF's @Length1@ and
-- @Length2@ (cleartext and encrypted sizes), used only when the cleartext
-- has no @eexec@ keyword.
parseType1 :: Maybe (Int, Int) -> BS.ByteString -> Either String Font
parseType1 lens raw = do
  (clear, enc) <- splitSections lens raw
  let cipher = BS.take maxPrivateBytes (if isHexSection enc then unhex enc else enc)
      priv = BS.drop 4 (decryptT1 55665 cipher)
  if BS.null priv then Left "Type1: empty encrypted section" else Right ()
  buildFont clear priv

-- | Cleartext and the (still encrypted) private section.
splitSections :: Maybe (Int, Int) -> BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
splitSections lens raw
  | BS.length raw >= 6 && u8 raw 0 == 0x80 = pfbSegments raw
  | otherwise =
      let (pre, post) = BS.breakSubstring (BC.pack "eexec") raw
      in if not (BS.null post)
           then Right (pre, BS.dropWhile isWs (BS.drop 5 post))
           else case lens of
                  Just (l1, l2) | l1 > 0 && l1 < BS.length raw ->
                    Right (BS.take l1 raw, (if l2 > 0 then BS.take l2 else id) (BS.drop l1 raw))
                  _ -> Left "Type1: no eexec section"
  where isWs w = w == 32 || w == 9 || w == 10 || w == 13 || w == 12 || w == 0

pfbSegments :: BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
pfbSegments bs0 = go bs0 [] [] (0 :: Int)
  where
    go bs asc bin n
      | n > 10000 || BS.length bs < 2 || u8 bs 0 /= 0x80 || u8 bs 1 == 3 = finish asc bin
      | otherwise =
          let t = u8 bs 1
              len = u8 bs 2 + 256 * u8 bs 3 + 65536 * u8 bs 4 + 16777216 * u8 bs 5
              dat = slice 6 len bs
              rest = BS.drop (6 + len) bs
          in case t of
               1 | null bin -> go rest (dat : asc) bin (n + 1)
                 | otherwise -> finish asc bin   -- trailing cleartext (the zeros)
               2 -> go rest asc (dat : bin) (n + 1)
               _ -> finish asc bin
    finish asc bin
      | null bin = case BS.breakSubstring (BC.pack "eexec") (BS.concat (reverse asc)) of
          (pre, post) | not (BS.null post) -> Right (pre, BS.dropWhile (\w -> w <= 32) (BS.drop 5 post))
          _ -> Left "Type1: PFB without a binary segment"
      | otherwise = Right (BS.concat (reverse asc), BS.concat (reverse bin))

isHexSection :: BS.ByteString -> Bool
isHexSection s = let h = BC.take 4 s in BS.length h == 4 && BC.all isHexDigit h

-- | Decode a hex eexec section straight into a 'BS.ByteString' (no
-- intermediate list: a 4 MB section used to cost hundreds of MB through
-- 'String'). It stops at the first byte that is neither hex nor
-- whitespace, and reads at most 'maxHexBytes' of input.
unhex :: BS.ByteString -> BS.ByteString
unhex s0 = fst (BS.unfoldrN (BS.length s `div` 2) step 0)
  where
    s = BS.take maxHexBytes (BS.takeWhile (\w -> hexVal w >= 0 || isWsW w) s0)
    len = BS.length s
    step !i = case nextHex i of
      Just (hi, j) -> case nextHex j of
        Just (lo, k) -> Just (fromIntegral (hi * 16 + lo), k)
        Nothing -> Nothing
      Nothing -> Nothing
    nextHex !i
      | i >= len = Nothing
      | otherwise = let w = BU.unsafeIndex s i; v = hexVal w
                    in if v >= 0 then Just (v, i + 1) else nextHex (i + 1)
    isWsW w = w == 32 || w == 9 || w == 10 || w == 13 || w == 12 || w == 0
    hexVal :: Word8 -> Int
    hexVal w | w >= 48 && w <= 57 = fromIntegral w - 48
             | w >= 65 && w <= 70 = fromIntegral w - 55
             | w >= 97 && w <= 102 = fromIntegral w - 87
             | otherwise = -1

-- | The most hex input read from one eexec section (16 MB decoded), and the
-- most binary: no real Type 1 font comes near either; a hostile one is cut
-- off rather than tokenised whole.
maxHexBytes, maxPrivateBytes :: Int
maxHexBytes = 32 * 1024 * 1024
maxPrivateBytes = 16 * 1024 * 1024

-- | Type 1 decryption (eexec uses r = 55665, charstrings r = 4330).
decryptT1 :: Word16 -> BS.ByteString -> BS.ByteString
decryptT1 r0 bs = fst (BS.unfoldrN (BS.length bs) step (r0, 0))
  where
    step (!r, !i)
      | i >= BS.length bs = Nothing
      | otherwise =
          let c = BU.unsafeIndex bs i
              p = c `xor` fromIntegral (r `shiftR` 8)
              r' = (fromIntegral c + r) * 52845 + 22719
          in Just (p, (r', i + 1))

-- | The inverse of 'decryptT1' (used by tests to build fixtures).
encryptT1 :: Word16 -> BS.ByteString -> BS.ByteString
encryptT1 r0 bs = fst (BS.unfoldrN (BS.length bs) step (r0, 0))
  where
    step (!r, !i)
      | i >= BS.length bs = Nothing
      | otherwise =
          let p = BU.unsafeIndex bs i
              c = p `xor` fromIntegral (r `shiftR` 8) :: Word8
              r' = (fromIntegral c + r) * 52845 + 22719
          in Just (c, (r', i + 1))

------------------------------------------------------------------------------
-- Tokens

data Tok = TNum !Double | TName !BS.ByteString | TLit !BS.ByteString
         | TStr !BS.ByteString | TOpen | TClose | TBin !BS.ByteString
  deriving (Eq, Show)

-- | A forgiving PostScript scanner. @rd@ names the binary-reading
-- procedures: a non-negative integer followed by one of them and a single
-- whitespace byte introduces that many raw bytes.
tokens :: [BS.ByteString] -> BS.ByteString -> [Tok]
tokens rd s = go 0
  where
    len = BS.length s
    at = u8 s
    go !p
      | p >= len = []
      | otherwise =
          let c = at p in
          if isWsB c then go (p + 1)
          else if c == 37 then go (skipLine p)                         -- %
          else if c == 47 then let (nm, p') = name (p + 1) in TLit nm : go p'   -- /
          else if c == 40 then let (str, p') = pstring (p + 1) in TStr str : go p'
          else if c == 123 || c == 91 then TOpen : go (p + 1)           -- { [
          else if c == 125 || c == 93 then TClose : go (p + 1)          -- } ]
          else if c == 60 || c == 62 then go (p + 1)                    -- < > (dict/hex delims)
          else let (word, p') = name p in
               if BS.null word then go (p + 1) else
               case number word of
                 Just v | v >= 0, v == fromIntegral (truncate v :: Int), Just (start, n) <- binAfter p' (truncate v) ->
                   TBin (slice start n s) : go (start + n)
                 Just v -> TNum v : go p'
                 Nothing -> TName word : go p'
    isWsB c = c == 32 || c == 9 || c == 10 || c == 13 || c == 12 || c == 0
    isDelim c = isWsB c || c `elem` [40, 41, 60, 62, 91, 93, 123, 125, 47, 37]
    name p = let q = scanTo p in (slice p (q - p) s, q)
    scanTo q | q < len && not (isDelim (at q)) = scanTo (q + 1)
             | otherwise = q
    skipLine q | q < len && at q /= 10 && at q /= 13 = skipLine (q + 1)
               | otherwise = q
    pstring p0 = goS p0 (0 :: Int)
      where goS q depth
              | q >= len = (slice p0 (q - p0) s, q)
              | at q == 92 = goS (q + 2) depth
              | at q == 40 = goS (q + 1) (depth + 1)
              | at q == 41 = if depth == 0 then (slice p0 (q - p0) s, q + 1) else goS (q + 1) (depth - 1)
              | otherwise = goS (q + 1) depth
    -- After an integer: whitespace, an RD name, exactly one whitespace byte.
    binAfter p n =
      let q = skipWs p
          (w, q') = name q
      in if q > p && w `elem` rd && q' < len && isWsB (at q') && n <= len - (q' + 1)
           then Just (q' + 1, n) else Nothing
    skipWs q | q < len && isWsB (at q) = skipWs (q + 1)
             | otherwise = q

number :: BS.ByteString -> Maybe Double
number w
  -- Fast path for the common case, a plain integer: no String.
  | BS.length w <= 9, Just (v, rest) <- BC.readInt w, BS.null rest, BS.all (\c -> c /= 43) w = Just (fromIntegral v)
  | otherwise = numberSlow w

numberSlow :: BS.ByteString -> Maybe Double
numberSlow w = case BC.unpack w of
  s | Just v <- radix s -> Just v
  ('-' : r) | ok r -> Just (negate (readD r))
  ('+' : r) | ok r -> Just (readD r)
  r | ok r -> Just (readD r)
  _ -> Nothing
  where
    ok r = not (null r) && any isDigit r && all (\c -> isDigit c || c == '.' || c == 'e' || c == 'E' || c == '-') r
           && length (filter (== '.') r) <= 1 && isDigit' r
    isDigit' r = case r of (c : _) -> isDigit c || c == '.'; [] -> False
    readD r =
      let (mant, ex) = break (\c -> c == 'e' || c == 'E') r
          (ip, fp) = break (== '.') mant
          iv = foldl (\a c -> a * 10 + fromIntegral (digitToInt c)) 0 (filter isDigit ip) :: Double
          fv = foldr (\c a -> (a + fromIntegral (digitToInt c)) / 10) 0 (filter isDigit (drop 1 fp))
          e = case ex of
                (_ : '-' : ds) | not (null ds), all isDigit ds -> negate (read (take 3 ds) :: Int)
                (_ : ds) | not (null ds), all isDigit ds -> read (take 3 ds)
                _ -> 0
      in (iv + fv) * 10 ^^ e
    radix s = case break (== '#') s of
      (b, '#' : ds) | not (null b), all isDigit b, not (null ds) ->
        let base = read (take 2 b) :: Int
            dv c = let d = digitToInt' c in if d < base then Just d else Nothing
        in if base >= 2 && base <= 36
             then fmap (fromIntegral . foldl (\a d -> a * base + d) 0) (mapM dv ds)
             else Nothing
      _ -> Nothing
    digitToInt' c
      | isDigit c = fromEnum c - 48
      | c >= 'a' && c <= 'z' = fromEnum c - 87
      | c >= 'A' && c <= 'Z' = fromEnum c - 55
      | otherwise = 99

------------------------------------------------------------------------------
-- The font

buildFont :: BS.ByteString -> BS.ByteString -> Either String Font
buildFont clear priv
  | null charStrings = Left "Type1: no CharStrings"
  | otherwise = Right font
  where
    cts = tokens [] clear
    -- The binary-reading procedure names: the two conventional ones plus
    -- anything defined as "{string currentfile exch readstring pop}".
    rdNames = BC.pack "RD" : BC.pack "-|" : definedReaders
    definedReaders = [ nm | (TLit nm : TOpen : TName a : TName b : TName c : TName d : _) <- tailsL (tokens [] (BS.take 8192 priv))
                          , map BC.unpack [a, b, c, d] == ["string", "currentfile", "exch", "readstring"] ]
    pts = tokens rdNames priv

    lookupNums key ts = case dropWhile (/= TLit (BC.pack key)) ts of
      (_ : TOpen : rest) -> [ v | TNum v <- takeWhile (/= TClose) rest ]
      (_ : rest) -> [ v | TNum v <- take 4 rest ]
      [] -> []
    lookupNum key ts = case dropWhile (/= TLit (BC.pack key)) ts of
      (_ : TNum v : _) -> Just v
      _ -> Nothing
    lookupStr key ts = case dropWhile (/= TLit (BC.pack key)) ts of
      (_ : TStr v : _) -> BC.unpack v
      (_ : TLit v : _) -> BC.unpack v
      _ -> ""
    lookupBool key ts = case dropWhile (/= TLit (BC.pack key)) ts of
      (_ : TName v : _) -> v == BC.pack "true"
      _ -> False

    matrix = case lookupNums "FontMatrix" cts of
      [a, b, c, d, e, f] | abs (a * d - b * c) > 1e-12 -> Affine a b c d e f
      _ -> scaleA 0.001 0.001
    bbox = case lookupNums "FontBBox" cts of
      [a, b, c, d] -> Rect (min a c) (min b d) (max a c) (max b d)
      _ -> Rect 0 0 0 0
    fontName = lookupStr "FontName" cts
    family = case lookupStr "FamilyName" cts of "" -> fontName; f -> f
    weight = lookupStr "Weight" cts
    fullName = lookupStr "FullName" cts
    italicAngle = fromMaybe 0 (lookupNum "ItalicAngle" cts)
    lw = T.toLower (T.pack (weight ++ " " ++ fontName ++ " " ++ fullName))
    bold = any (`T.isInfixOf` lw) ["bold", "black", "heavy", "demi"]
    italic = italicAngle /= 0 || any (`T.isInfixOf` lw) ["italic", "oblique"]

    encoding :: IM.IntMap String
    encoding = case dropWhile (/= TLit (BC.pack "Encoding")) cts of
      (_ : TName n : _) | n == BC.pack "StandardEncoding" -> standardEncodingNames
      (_ : rest) -> IM.fromList (encEntries rest)
      [] -> standardEncodingNames
    encEntries (TName d : TNum c : TLit n : TName pu : rest)
      | d == BC.pack "dup" && pu == BC.pack "put" && c >= 0 && c < 256 =
          (truncate c, BC.unpack n) : encEntries rest
    encEntries (TName d : _) | d == BC.pack "def" || d == BC.pack "readonly" = []
    encEntries (_ : rest) = encEntries rest
    encEntries [] = []

    -- One pass over the private section's tokens collects everything it
    -- is read for (lenIV, Subrs, CharStrings), so the token list streams
    -- through and is never held whole: a 16 MB section of filler costs its
    -- tokenising time, not a heap of tokens.
    (lenIV0, subrList, charStrings0) = scanPrivate pts
    lenIV = case lenIV0 of
      Just v | v >= -1 && v < 64 -> truncate v
      _ -> 4 :: Int
    decode b | lenIV < 0 = b
             | otherwise = BS.drop lenIV (decryptT1 4330 b)
    subrs :: IM.IntMap BS.ByteString
    subrs = IM.fromListWith (\_new old -> old) subrList
    charStrings = case break ((== ".notdef") . fst) charStrings0 of
      (before@(_ : _), nd : after) -> nd : tail before ++ [head before] ++ after
      _ -> charStrings0
    nGlyphs = length charStrings
    glyphArr = A.listArray (0, nGlyphs - 1) charStrings
    nameMap = M.fromListWith (\_n o -> o) (zip (map fst charStrings) [0 ..])
    byName n = M.lookup n nameMap
    subrBytes i = maybe BS.empty decode (IM.lookup i subrs)

    run :: Bool -> (Double, Double) -> Int -> (PathB, Double, Maybe Seac)
    run allowSeac org g
      | g < 0 || g >= nGlyphs = (pbEmpty, 0, Nothing)
      | otherwise = runType1 subrBytes org allowSeac False (decode (snd (glyphArr A.! g)))
    glyph g =
      let (pb, w, seac) = run True (0, 0) g
      in case seac of
           Just (Seac asb adx ady bc ac sbx)
             | Just bg <- IM.lookup bc standardEncodingNames >>= byName
             , Just ag <- IM.lookup ac standardEncodingNames >>= byName ->
                 let (pbB, _, _) = run False (0, 0) bg
                     (pbA, _, _) = run False (sbx + adx - asb, ady) ag
                 in (pbFinish pbB ++ pbFinish pbA, w)
           _ -> (pbFinish pb, w)

    -- Advances are memoised (lazily, one small thunk per glyph) and found
    -- by an interpretation that stops at hsbw/sbw, the charstring's first
    -- operator; outlines are the caller's to memoise (see "Cmedit.FontCFF").
    advMemo = A.listArray (0, nGlyphs - 1) [ widthOf g | g <- [0 .. nGlyphs - 1] ] :: A.Array Int Double
    widthOf g = let (_, w, _) = runType1 subrBytes (0, 0) False True (decode (snd (glyphArr A.! g))) in w

    Affine ma _ _ md _ _ = matrix
    Rect _ by0 _ by1 = bbox
    font = (emptyFontWith FmtType1)
      { fontMatrix = matrix
      , fontUnitsPerEm = if ma /= 0 then abs (1 / ma) else 1000
      , fontNumGlyphs = nGlyphs
      , fontFamily = T.pack family, fontStyle = T.pack weight
      , fontFullName = T.pack fullName, fontPSName = T.pack fontName
      , fontBold = bold, fontItalic = italic
      , fontFixedPitch = lookupBool "isFixedPitch" cts
      , fontWeight = weightFromName lw
      , fontItalicAngle = italicAngle
      , fontAscent = if by1 /= 0 then by1 * md else 0.8
      , fontDescent = if by0 /= 0 then by0 * md else -0.2
      , fontBBox = bbox
      , fontBuiltinEncoding = Just (IM.filter (\n -> M.member n nameMap) encoding)
      , fnGlyph = fst . glyph
      , fnAdvance = \g -> if g >= 0 && g < nGlyphs then advMemo A.! g else 0
      , fnName = \g -> if g >= 0 && g < nGlyphs then Just (fst (glyphArr A.! g)) else Nothing
      , fnByName = byName
      , fnForCID = \c -> if c >= 0 && c < nGlyphs then Just c else Nothing
      , fnForChar = \c -> listToMaybe (mapMaybe byName (aglCharToNames c))
      }

-- | lenIV, Subrs (@dup <i> <bin>@: only Subrs entries have that shape, so
-- they are taken from anywhere and a font that defines them after its
-- CharStrings still works) and the CharStrings entries (@/name <bin>@ from
-- the first @/CharStrings@ to its @end@).
scanPrivate :: [Tok] -> (Maybe Double, [(Int, BS.ByteString)], [(String, BS.ByteString)])
scanPrivate = go Nothing [] [] (0 :: Int)
  where
    -- csState: 0 before /CharStrings, 1 inside, 2 after its end.
    go liv ss cs st ts = case ts of
      [] -> (liv, reverse ss, reverse cs)
      (TLit l : TNum v : r) | l == BC.pack "lenIV", Nothing <- liv -> go (Just v) ss cs st r
      (TLit l : r) | st == 0, l == BC.pack "CharStrings" -> go liv ss cs 1 r
      (TName d : TNum i : TBin b : r)
        | d == BC.pack "dup" && i >= 0 && i < 65536 -> let e = (truncate i, b) in e `seq` go liv (e : ss) cs st r
      (TLit n : TBin b : r) | st == 1 -> let e = (BC.unpack n, b) in length (fst e) `seq` go liv ss (e : cs) st r
      (TName e : r) | st == 1, e == BC.pack "end" -> go liv ss cs 2 r
      (_ : r) -> go liv ss cs st r

tailsL :: [a] -> [[a]]
tailsL [] = []
tailsL xs@(_ : r) = xs : tailsL r

------------------------------------------------------------------------------
-- Type 1 charstrings

-- | A @seac@ request: asb, adx, ady, base code, accent code, and the
-- composite's own side bearing (FreeType's correction: the accent is
-- placed relative to it).
data Seac = Seac !Double !Double !Double !Int !Int !Double

data St = St
  { sStack :: ![Double]          -- top first
  , sDepth :: !Int
  , sX, sY :: !Double
  , sPath  :: !PathB
  , sWidth :: !Double
  , sSbx   :: !Double
  , sFlex  :: !(Maybe [Pt])      -- collected flex points, in reverse
  , sPS    :: ![Double]          -- the PostScript stack callothersubr fills, pop drains
  , sFuel  :: !Int
  , sSeacR :: !(Maybe Seac)
  }

data Outcome = Ret !St | Done !St | Fail !St

maxDepth, maxOps, maxStack :: Int
maxDepth = 10
maxOps = 100000
maxStack = 256

-- | @widthOnly@ stops at the side-bearing operator, which settles the width.
runType1 :: (Int -> BS.ByteString) -> (Double, Double) -> Bool -> Bool -> BS.ByteString -> (PathB, Double, Maybe Seac)
runType1 subrs (ox, oy) allowSeac widthOnly cs =
  let st0 = St [] 0 ox oy pbEmpty 0 0 Nothing [] maxOps Nothing
      out st = (sPath st, sWidth st, if allowSeac then sSeacR st else Nothing)
  in case exec 0 cs 0 st0 of
       Ret st -> out st
       Done st -> out st
       Fail st -> (sPath st, sWidth st, Nothing)
  where
    exec :: Int -> BS.ByteString -> Int -> St -> Outcome
    exec depth bs = loop
      where
        len = BS.length bs
        loop !p !st0
          | p >= len = Ret st0
          | sFuel st0 <= 0 = Fail st0
          | otherwise =
              let v = u8 bs p
                  st = st0 { sFuel = sFuel st0 - 1 }
              in if v >= 32 then
                   if v <= 246 then push (fromIntegral (v - 139)) (p + 1) st
                   else if v <= 250 then push (fromIntegral ((v - 247) * 256 + u8 bs (p + 1) + 108)) (p + 2) st
                   else if v <= 254 then push (fromIntegral (-(v - 251) * 256 - u8 bs (p + 1) - 108)) (p + 2) st
                   else push (fromIntegral (i32 bs (p + 1))) (p + 5) st
                 else if v == 12 then esc (u8 bs (p + 1)) (p + 2) st
                 else op v (p + 1) st

        push x p st
          | sDepth st >= maxStack = Fail st
          | otherwise = loop p st { sStack = x : sStack st, sDepth = sDepth st + 1 }
        clear st = st { sStack = [], sDepth = 0 }
        args st = reverse (sStack st)

        moveRel dx dy st =
          let x = sX st + dx; y = sY st + dy
          in case sFlex st of
               Just ps -> st { sX = x, sY = y, sFlex = Just (Pt x y : ps) }
               Nothing -> st { sX = x, sY = y, sPath = pbMove x y (sPath st) }
        lineRel dx dy st = let x = sX st + dx; y = sY st + dy
                           in st { sX = x, sY = y, sPath = pbLine x y (ensure st) }
        curveRel a b c d e f st =
          let x1 = sX st + a; y1 = sY st + b; x2 = x1 + c; y2 = y1 + d; x3 = x2 + e; y3 = y2 + f
          in st { sX = x3, sY = y3, sPath = pbCurve x1 y1 x2 y2 x3 y3 (ensure st) }
        -- A drawing operator after closepath (or before any moveto) starts
        -- its subpath at the current point.
        ensure st = let (px, py) = pbPoint (sPath st)
                    in if px == sX st && py == sY st then sPath st else pbMove (sX st) (sY st) (sPath st)

        op :: Int -> Int -> St -> Outcome
        op v p st = case (v, args st) of
          (1, _) -> loop p (clear st)                           -- hstem
          (3, _) -> loop p (clear st)                           -- vstem
          (4, [dy]) -> loop p (clear (moveRel 0 dy st))
          (5, [dx, dy]) -> loop p (clear (lineRel dx dy st))
          (6, [dx]) -> loop p (clear (lineRel dx 0 st))
          (7, [dy]) -> loop p (clear (lineRel 0 dy st))
          (8, [a, b, c, d, e, f]) -> loop p (clear (curveRel a b c d e f st))
          (9, _) -> loop p (clear st { sPath = pbClose (sPath st) })
          (10, _) -> case sStack st of
            (i : rest)
              | depth >= maxDepth -> Fail st
              | otherwise ->
                  let st1 = st { sStack = rest, sDepth = sDepth st - 1 }
                      k = toI i
                  in if k < 0 then Fail st1 else
                     case exec (depth + 1) (subrs k) 0 st1 of
                       Ret st2 -> loop p st2
                       other -> other
            [] -> Fail st
          (11, _) -> Ret st
          (13, [sbx, wx]) -> (if widthOnly then Done else loop p) (clear st { sX = sX st + sbx, sWidth = wx, sSbx = sbx
                                              , sPath = pbMove (sX st + sbx) (sY st) (sPath st) })
          (14, _) -> Done (clear st)
          (21, [dx, dy]) -> loop p (clear (moveRel dx dy st))
          (22, [dx]) -> loop p (clear (moveRel dx 0 st))
          (30, [dy1, dx2, dy2, dx3]) -> loop p (clear (curveRel 0 dy1 dx2 dy2 dx3 0 st))
          (31, [dx1, dx2, dy2, dy3]) -> loop p (clear (curveRel dx1 0 dx2 dy2 0 dy3 st))
          -- Wrong argument count for a known operator, or an unknown one:
          -- the glyph is malformed; drop the operands and carry on.
          _ -> loop p (clear st)

        esc :: Int -> Int -> St -> Outcome
        esc e p st = case (e, args st) of
          (0, _) -> loop p (clear st)                           -- dotsection
          (1, _) -> loop p (clear st)                           -- vstem3
          (2, _) -> loop p (clear st)                           -- hstem3
          (6, [asb, adx, ady, bc, ac]) ->
            Done (clear st) { sSeacR = Just (Seac asb adx ady (toI bc) (toI ac) (sSbx st)) }
          (7, [sbx, sby, wx, _wy]) -> (if widthOnly then Done else loop p) (clear st { sX = sX st + sbx, sY = sY st + sby, sWidth = wx, sSbx = sbx
                                                       , sPath = pbMove (sX st + sbx) (sY st + sby) (sPath st) })
          (12, _) -> case sStack st of
            (b : a : r) -> loop p st { sStack = (if b == 0 then 0 else a / b) : r, sDepth = sDepth st - 1 }
            _ -> Fail st
          (16, _) -> othersubr p st
          (17, _) -> case sPS st of
            (x : r) -> push x p st { sPS = r }
            [] -> loop p st
          (33, [x, y]) -> loop p (clear st { sX = x, sY = y })
          _ -> loop p (clear st)

        othersubr p st = case sStack st of
          (k : n : rest) ->
            let nn = max 0 (min (length rest) (toI n))
                (as', rest') = splitAt nn rest
                as = reverse as'                -- arg1 .. argn
                st1 = st { sStack = rest', sDepth = length rest' }
            in case toI k of
                 1 -> loop p st1 { sFlex = Just [], sPS = [] }
                 2 -> loop p st1 { sPS = [] }
                 0 -> case (sFlex st1, as) of
                   (Just rps, [_, x, y]) ->
                     let ps = reverse rps
                         st2 = st1 { sFlex = Nothing, sPS = [x, y] }
                     in case ps of
                          [_, p1, p2, p3, p4, p5, p6] ->
                            let Pt x1 y1 = p1; Pt x2 y2 = p2; Pt x3 y3 = p3
                                Pt x4 y4 = p4; Pt x5 y5 = p5; Pt x6 y6 = p6
                                pb = pbCurve x4 y4 x5 y5 x6 y6 (pbCurve x1 y1 x2 y2 x3 y3 (sPath st2))
                            in loop p st2 { sPath = pb, sX = x6, sY = y6 }
                          _ -> loop p st2 { sPath = pbLine x y (sPath st2), sX = x, sY = y }
                   _ -> loop p st1 { sFlex = Nothing, sPS = reverse as }
                 3 -> loop p st1 { sPS = as }
                 _ -> loop p st1 { sPS = as }
          _ -> Fail st

    toI v = if isNaN v || abs v > 1e7 then -1 else truncate v :: Int
