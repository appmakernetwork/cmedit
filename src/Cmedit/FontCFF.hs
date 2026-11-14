{-# LANGUAGE BangPatterns #-}
-- | Compact Font Format (CFF 1): bare (PDF @FontFile3 /Type1C@ and
-- @/CIDFontType0C@) or as the @CFF @ table of an OpenType font, with Type 2
-- charstrings.
--
-- Load-bearing decisions:
--
-- * __Nothing but offsets is read at load time.__ An INDEX is kept as the
--   position of its offset array, so item @i@ is two offset reads; charsets
--   and FDSelect (one small array per glyph) are the only per-glyph tables
--   materialised, because a name or FD lookup must be O(1) and both formats
--   store them as ranges.
--
-- * __The Type 2 interpreter is total and fuelled.__ It runs over the raw
--   bytes with a list stack (operators read their arguments bottom-up, so an
--   operator reverses the stack once); subroutine calls recurse to at most
--   'maxSubrDepth' (the spec's limit of 10), each glyph has an operator
--   budget ('maxOps'), and the stack is capped. Any violation ends the glyph
--   with what it has drawn closed off, never an exception: a corrupt glyph
--   is a missing glyph.
--
-- * __Hint operators matter even though hints do not.__ @hintmask@ and
--   @cntrmask@ are followed by one mask bit per stem, so the interpreter
--   must count every stem declared (including the implicit @vstem@ that
--   arguments left on the stack before the first @hintmask@ make) or it
--   reads mask bytes as operators.
--
-- * __CID-keyed fonts fold per-FD matrices into the glyph.__ Each FD may
--   carry its own @FontMatrix@; the font exposes one 'fontMatrix' (FD 0's
--   combined matrix), and a glyph from an FD whose matrix differs is
--   pre-transformed by the difference, so callers never need to know.
module Cmedit.FontCFF
  ( parseCFF
  , T2Ctx(..)
  , runType2
  ) where

import Data.Array.Unboxed (UArray, listArray, (!), bounds)
import qualified Data.Array as A
import Data.Bits ((.&.), testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T

import Cmedit.Geom
import Cmedit.FontCore

------------------------------------------------------------------------------
-- INDEX

-- | An INDEX: count, offset size, the position of the offset array, and the
-- position the (1-based) offsets are relative to.
data Index = Index { ixCount :: !Int, ixOffSize :: !Int, ixOffs :: !Int, ixBase :: !Int, ixEnd :: !Int }

emptyIndex :: Index
emptyIndex = Index 0 1 0 0 0

readIndex :: BS.ByteString -> Int -> Index
readIndex bs pos
  | pos < 0 || pos + 2 > BS.length bs = emptyIndex { ixEnd = pos + 2 }
  | count == 0 = emptyIndex { ixEnd = pos + 2 }
  | offSize < 1 || offSize > 4 = emptyIndex { ixEnd = BS.length bs }
  | otherwise =
      let offs = pos + 3
          base = offs + (count + 1) * offSize - 1
          ix = Index count offSize offs base 0
      in ix { ixEnd = base + offAt ix count }
  where
    count = u16 bs pos
    offSize = u8 bs (pos + 2)
    offAt ix i = readOff bs (ixOffSize ix) (ixOffs ix + i * ixOffSize ix)

readOff :: BS.ByteString -> Int -> Int -> Int
readOff bs n p = case n of
  1 -> u8 bs p
  2 -> u16 bs p
  3 -> u24 bs p
  _ -> u32 bs p

indexItem :: BS.ByteString -> Index -> Int -> BS.ByteString
indexItem bs ix i
  | i < 0 || i >= ixCount ix = BS.empty
  | otherwise =
      let a = readOff bs (ixOffSize ix) (ixOffs ix + i * ixOffSize ix)
          b = readOff bs (ixOffSize ix) (ixOffs ix + (i + 1) * ixOffSize ix)
      in if b > a && a >= 1 then slice (ixBase ix + a) (b - a) bs else BS.empty

------------------------------------------------------------------------------
-- DICT

type Dict = M.Map Int [Double]

-- | Parse a DICT: operands accumulate until an operator (0–21, or 12 x as
-- 1200 + x) claims them.
parseDict :: BS.ByteString -> Dict
parseDict d = go 0 [] M.empty (0 :: Int)
  where
    len = BS.length d
    go p ops acc n
      | p >= len || n > 4096 = acc
      | otherwise =
          let b = u8 d p in
          if b <= 21 then
            if b == 12 then go (p + 2) [] (M.insert (1200 + u8 d (p + 1)) (reverse ops) acc) (n + 1)
            else go (p + 1) [] (M.insert b (reverse ops) acc) (n + 1)
          else if b == 28 then go (p + 3) (fromIntegral (i16 d (p + 1)) : ops) acc n
          else if b == 29 then go (p + 5) (fromIntegral (i32 d (p + 1)) : ops) acc n
          else if b == 30 then let (v, p') = realNum (p + 1) in go p' (v : ops) acc n
          else if b >= 32 && b <= 246 then go (p + 1) (fromIntegral (b - 139) : ops) acc n
          else if b >= 247 && b <= 250 then go (p + 2) (fromIntegral ((b - 247) * 256 + u8 d (p + 1) + 108) : ops) acc n
          else if b >= 251 && b <= 254 then go (p + 2) (fromIntegral (-(b - 251) * 256 - u8 d (p + 1) - 108) : ops) acc n
          else go (p + 1) ops acc n          -- reserved byte: skip
    -- Real numbers: nibbles 0–9, a=., b=E, c=E-, e=minus, f=end.
    realNum p0 = let (s, p') = nibbles p0 [] in (readReal (reverse s), p')
    nibbles p acc
      | p >= len || length acc > 64 = (acc, p)
      | otherwise =
          let b = u8 d p; hi = b `div` 16; lo = b .&. 15
              add n a = case n of
                _ | n <= 9 -> toEnum (n + 48) : a
                10 -> '.' : a
                11 -> 'E' : a
                12 -> '-' : 'E' : a
                14 -> '-' : a
                _ -> a
              acc1 = add hi acc
          in if hi == 15 then (acc, p + 1)
             else if lo == 15 then (acc1, p + 1)
             else nibbles (p + 1) (add lo acc1)

-- | A DICT real, computed exactly (mantissa and exponent as a 'Rational')
-- so @.001@ is the double nearest 0.001, not 1/10/10/10.
readReal :: String -> Double
readReal s0 =
  let (sign, s1) = case s0 of ('-' : r) -> (-1, r); r -> (1, r)
      (mant, rest) = span (/= 'E') s1
      (ip, fp) = break (== '.') mant
      digits = take 30 . filter (`elem` ['0' .. '9'])
      fdig = digits (drop 1 fp)
      m = foldl (\a c -> a * 10 + toInteger (fromEnum c - 48)) 0 (digits ip ++ fdig) :: Integer
      ex = case rest of
             ('E' : '-' : e) -> negate (readInt e)
             ('E' : e) -> readInt e
             _ -> 0
      readInt e = let ds = take 4 (digits e) in if null ds then 0 else read ds :: Int
      e10 = max (-400) (min 400 (ex - length fdig))
      v = sign * fromRational (fromInteger m * (10 ^^ e10) :: Rational)
  in if isNaN v || isInfinite v then 0 else v

dictInt :: Int -> Int -> Dict -> Int
dictInt k def d = case M.lookup k d of
  Just (v : _) | not (isNaN v) && abs v < 1e9 -> truncate v
  _ -> def

dictNum :: Int -> Double -> Dict -> Double
dictNum k def d = case M.lookup k d of
  Just (v : _) -> v
  _ -> def

------------------------------------------------------------------------------
-- The font

-- | Per-FD (or the single Private DICT's) charstring context.
data FDInfo = FDInfo { fdSubrs :: !Index, fdDefW :: !Double, fdNomW :: !Double, fdAdjust :: !(Maybe Affine) }

parseCFF :: BS.ByteString -> Either String Font
parseCFF bs
  | BS.length bs < 4 = Left "CFF: too short"
  | u8 bs 0 /= 1 = Left ("CFF: unsupported major version " ++ show (u8 bs 0))
  | ixCount topIx < 1 = Left "CFF: no Top DICT"
  | nGlyphs <= 0 = Left "CFF: no CharStrings"
  | csType /= 2 = Left "CFF: Type 1 charstrings in CFF are not supported"
  | otherwise = Right font
  where
    hdrSize = u8 bs 2
    nameIx = readIndex bs hdrSize
    topIx = readIndex bs (ixEnd nameIx)
    strIx = readIndex bs (ixEnd topIx)
    gsubrIx = readIndex bs (ixEnd strIx)
    top = parseDict (indexItem bs topIx 0)
    psName = BC.unpack (indexItem bs nameIx 0)
    sid s | s < 0 = ""
          | s < 391 = cffStandardStrings A.! s
          | otherwise = BC.unpack (indexItem bs strIx (s - 391))
    csIx = readIndex bs (dictInt 17 (-1) top)
    nGlyphs = min 65536 (ixCount csIx)
    csType = dictInt 1206 2 top
    isCID = M.member 1230 top

    topMatExplicit = matrixOf top
    matrixOf d = case M.lookup 1207 d of
      Just [a, b, c, e, f, g] | a /= 0 || b /= 0, abs (a * e - b * c) > 1e-12 -> Just (Affine a b c e f g)
      _ -> Nothing

    privateOf d = case M.lookup 18 d of
      Just [sz, off] | sz >= 0, off >= 0, off < fromIntegral (BS.length bs) ->
        let o = truncate off; s = truncate sz
            pd = parseDict (slice o s bs)
            subrs = case M.lookup 19 pd of
                      Just (r : _) | r > 0 -> readIndex bs (o + truncate r)
                      _ -> emptyIndex
        in (pd, subrs)
      _ -> (M.empty, emptyIndex)

    -- Font DICTs: one (the top) for a name-keyed font, the FDArray for CID.
    fdDicts | isCID = let fa = readIndex bs (dictInt 1236 (-1) top)
                      in [ parseDict (indexItem bs fa i) | i <- [0 .. min 255 (ixCount fa) - 1] ]
            | otherwise = [top]
    combined d = case (topMatExplicit, if isCID then matrixOf d else Nothing) of
      (Just t, Just f) -> composeA t f
      (Just t, Nothing) -> t
      (Nothing, Just f) -> f
      (Nothing, Nothing) -> scaleA 0.001 0.001
    mats = map combined (if null fdDicts then [top] else fdDicts)
    mainMat = case mats of (m : _) -> m; [] -> scaleA 0.001 0.001
    invMain = invertA mainMat
    fds :: A.Array Int FDInfo
    fds = let infos = [ FDInfo subrs (dictNum 20 0 pd) (dictNum 21 0 pd) (adjust m)
                      | (d, m) <- zip (if null fdDicts then [top] else fdDicts) mats
                      , let (pd, subrs) = privateOf d ]
          in A.listArray (0, length infos - 1) infos
    adjust m | m == mainMat = Nothing
             | otherwise = fmap (\inv -> composeA inv m) invMain
    nFD = let (lo, hi) = A.bounds fds in hi - lo + 1

    fdSelect :: UArray Int Int
    fdSelect
      | not isCID = listArray (0, 0) [0]
      | otherwise =
          let p = dictInt 1237 (-1) top
          in case u8 bs p of
               _ | p < 0 -> listArray (0, 0) [0]
               0 -> listArray (0, nGlyphs - 1) [ clampFD (u8 bs (p + 1 + g)) | g <- [0 .. nGlyphs - 1] ]
               3 -> let nR = u16 bs (p + 1)
                        rng i = (u16 bs (p + 3 + 3 * i), u8 bs (p + 5 + 3 * i))
                        sentinel = u16 bs (p + 3 + 3 * nR)
                        ranges = [ rng i | i <- [0 .. min nR 65536 - 1] ] ++ [(sentinel, 0)]
                        -- Ranges must ascend; each is clipped to start where
                        -- the last ended, so a hostile list of overlapping
                        -- ranges still assigns each GID at most once.
                        covered = goR 0 (zip ranges (tail ranges))
                        goR _ [] = []
                        goR lo (((st, fd), (en, _)) : rest) =
                          let a = max lo st; b = min nGlyphs en
                          in [ (g, clampFD fd) | g <- [a .. b - 1] ] ++ goR (max lo b) rest
                        im = IM.fromList covered
                    in listArray (0, nGlyphs - 1) [ IM.findWithDefault 0 g im | g <- [0 .. nGlyphs - 1] ]
               _ -> listArray (0, 0) [0]
    clampFD x = if x < nFD then x else 0
    fdOf g = let (lo, hi) = bounds fdSelect in if g >= lo && g <= hi then fdSelect ! g else 0

    -- gid → SID (name-keyed) or CID.
    charset :: UArray Int Int
    charset = listArray (0, nGlyphs - 1) (take nGlyphs (0 : csEntries ++ repeat 0))
    csEntries = case dictInt 15 0 top of
      0 | not isCID -> [1 .. nGlyphs - 1]
      1 | not isCID -> expertCharset
      2 | not isCID -> expertSubsetCharset
      p | p <= 2 -> [1 .. nGlyphs - 1]
        | otherwise -> case u8 bs p of
            0 -> [ u16 bs (p + 1 + 2 * i) | i <- [0 .. nGlyphs - 2] ]
            1 -> ranges (p + 1) 1
            2 -> ranges (p + 1) 2
            _ -> []
    ranges p0 w = goR p0 (nGlyphs - 1)
      where goR p left
              | left <= 0 || p >= BS.length bs = []
              | otherwise = let first = u16 bs p
                                n = 1 + (if w == 1 then u8 bs (p + 2) else u16 bs (p + 2))
                                k = min n left
                            in [first .. first + k - 1] ++ goR (p + 2 + w) (left - k)
    glyphNameOf g
      | isCID || g < 0 || g >= nGlyphs = Nothing
      | otherwise = let s = sid (charset ! g) in if null s then Nothing else Just s
    nameMap = M.fromListWith (\_n o -> o) [ (n, g) | g <- [0 .. nGlyphs - 1], Just n <- [glyphNameOf g] ]
    byName n = M.lookup n nameMap
    cidMap = IM.fromListWith (\_n o -> o) [ (charset ! g, g) | g <- [0 .. nGlyphs - 1] ]
    forCID c | isCID = IM.lookup c cidMap
             | c >= 0 && c < nGlyphs = Just c
             | otherwise = Nothing

    -- Built-in encoding (name-keyed only): code → glyph name.
    builtinEnc
      | isCID = Nothing
      | otherwise = Just $ case dictInt 16 0 top of
          0 -> IM.filter (isJust . byName) standardEncodingNames
          1 -> IM.filter (isJust . byName) expertEncodingNames
          p -> customEncoding p
    customEncoding p =
      let fmt = u8 bs p
          base = case fmt .&. 0x7F of
            0 -> let n = u8 bs (p + 1) in [ (u8 bs (p + 2 + i), i + 1) | i <- [0 .. n - 1] ]
            1 -> let n = u8 bs (p + 1)
                     rs = [ (u8 bs (p + 2 + 2 * i), u8 bs (p + 3 + 2 * i)) | i <- [0 .. n - 1] ]
                     gids = scanl (+) 1 [ l + 1 | (_, l) <- rs ]
                 in [ (c + k, g + k) | ((c, l), g) <- zip rs gids, k <- [0 .. l] ]
            _ -> []
          baseLen = case fmt .&. 0x7F of
            0 -> 2 + u8 bs (p + 1)
            1 -> 2 + 2 * u8 bs (p + 1)
            _ -> 1
          sups | testBit fmt 7 =
                   let q = p + baseLen; n = u8 bs q
                   in [ (u8 bs (q + 1 + 3 * i), sid (u16 bs (q + 2 + 3 * i))) | i <- [0 .. n - 1] ]
               | otherwise = []
      in IM.fromList ([ (c, n) | (c, g) <- base, c < 256, g < nGlyphs, Just n <- [glyphNameOf g] ] ++ sups)

    seacGlyph code = do
      n <- IM.lookup code standardEncodingNames
      byName n
    ctxFor fd = T2Ctx { t2Gsubrs = indexItem bs gsubrIx, t2GsubrCount = ixCount gsubrIx
                      , t2Lsubrs = indexItem bs (fdSubrs fd), t2LsubrCount = ixCount (fdSubrs fd)
                      , t2Nominal = fdNomW fd, t2Default = fdDefW fd, t2WidthOnly = False }
    -- Advances are memoised, outlines are not. An advance is asked for at
    -- every occurrence of a glyph and is found by an interpretation that
    -- stops at the first stack-clearing operator (where Type 2 settles the
    -- width), so it costs a few bytes of charstring, never a second full
    -- run; the lazy array holds one small thunk per glyph until asked. The
    -- outline is the caller's to memoise: holding every decoded path here as
    -- well measured 370 MB live on a CJK face whose glyphs had all been drawn.
    advMemo :: A.Array Int Double
    advMemo = A.listArray (0, nGlyphs - 1) [ widthOf g | g <- [0 .. nGlyphs - 1] ]
    widthOf g =
      let fd = fds A.! fdOf g
          (_, w, _) = runType2 (ctxFor fd) { t2WidthOnly = True } (indexItem bs csIx g)
      in maybe w (\m -> w * adjX m) (fdAdjust fd)
    glyphDepth allowSeac g
      | g < 0 || g >= nGlyphs = ([], 0)
      | otherwise =
          let fdI = fdOf g
              fd = fds A.! fdI
              (path, w, seac) = runType2 (ctxFor fd) (indexItem bs csIx g)
              composed = case seac of
                Just (adx, ady, bc, ac) | allowSeac ->
                  case (seacGlyph bc, seacGlyph ac) of
                    (Just bg, Just ag) ->
                      fst (glyphDepth False bg) ++
                      transformPath (translateA adx ady) (fst (glyphDepth False ag))
                    _ -> path
                _ -> path
              out = case fdAdjust fd of
                Just m -> (transformPath m composed, w * adjX m)
                Nothing -> (composed, w)
          in out
    adjX (Affine a _ _ _ _ _) = a
    bbox = case M.lookup 5 top of
      Just [a, b, c, d] -> Rect a b c d
      _ -> Rect 0 0 0 0
    Affine _ _ _ md _ _ = mainMat
    Rect _ by0 _ by1 = bbox
    weightStr = sid (dictInt 4 (-1) top)
    fullStr = sid (dictInt 2 (-1) top)
    famStr = sid (dictInt 3 (-1) top)
    lw = T.toLower (T.pack (weightStr ++ " " ++ fullStr ++ " " ++ psName))
    bold = any (`T.isInfixOf` lw) ["bold", "black", "heavy", "demi", "semibold"]
    italicAngle = dictNum 1202 0 top
    italic = italicAngle /= 0 || any (`T.isInfixOf` lw) ["italic", "oblique"]
    Affine ma _ _ _ _ _ = mainMat
    font = (emptyFontWith (if isCID then FmtCFFCID else FmtCFF))
      { fontMatrix = mainMat
      , fontUnitsPerEm = if ma /= 0 then abs (1 / ma) else 1000
      , fontNumGlyphs = nGlyphs
      , fontFamily = T.pack (if null famStr then psName else famStr)
      , fontStyle = T.pack weightStr
      , fontFullName = T.pack fullStr
      , fontPSName = T.pack psName
      , fontBold = bold, fontItalic = italic
      , fontFixedPitch = dictInt 1201 0 top /= 0
      , fontWeight = weightFromName lw
      , fontItalicAngle = italicAngle
      , fontAscent = if by1 /= 0 then by1 * md else 0.8
      , fontDescent = if by0 /= 0 then by0 * md else -0.2
      , fontBBox = bbox
      , fontIsCID = isCID
      , fontBuiltinEncoding = builtinEnc
      , fnGlyph = fst . glyphDepth True
      , fnAdvance = \g -> if g >= 0 && g < nGlyphs then advMemo A.! g else 0
      , fnName = glyphNameOf
      , fnByName = byName
      , fnForCID = forCID
      , fnForChar = \c -> firstJust [ byName n | n <- aglCharToNames c ]
      }

firstJust :: [Maybe a] -> Maybe a
firstJust (Just x : _) = Just x
firstJust (_ : xs) = firstJust xs
firstJust [] = Nothing

------------------------------------------------------------------------------
-- Type 2 charstrings

-- | What a Type 2 program needs from its font.
data T2Ctx = T2Ctx
  { t2Gsubrs :: Int -> BS.ByteString, t2GsubrCount :: !Int
  , t2Lsubrs :: Int -> BS.ByteString, t2LsubrCount :: !Int
  , t2Nominal :: !Double, t2Default :: !Double
  , t2WidthOnly :: !Bool
      -- ^ Stop once the width is settled (the first stack-clearing
      -- operator): the advance without the outline.
  }

maxSubrDepth, maxOps, maxStack :: Int
maxSubrDepth = 10
maxOps = 200000
maxStack = 513

data St = St
  { sStack :: ![Double]       -- top first
  , sDepth :: !Int            -- stack size
  , sStems :: !Int
  , sHaveW :: !Bool            -- the width question has been settled
  , sWidth :: !(Maybe Double)
  , sPath  :: !PathB
  , sFuel  :: !Int
  , sTrans :: !(IM.IntMap Double)
  , sSeac  :: !(Maybe (Double, Double, Int, Int))
  }

data Outcome = Ret !St | Done !St | Fail !St

bias :: Int -> Int
bias n | n < 1240 = 107
       | n < 33900 = 1131
       | otherwise = 32768

-- | Run one glyph's charstring: the outline, the advance width, and the
-- @seac@-style accent request of a four-argument @endchar@, if any.
runType2 :: T2Ctx -> BS.ByteString -> (Path, Double, Maybe (Double, Double, Int, Int))
runType2 ctx cs =
  let st0 = St [] 0 0 False Nothing pbEmpty maxOps IM.empty Nothing
      fin st = ( pbFinish (sPath st)
               , maybe (t2Default ctx) (+ t2Nominal ctx) (sWidth st)
               , sSeac st )
  in case exec ctx 0 cs 0 st0 of
       Ret st -> fin st
       Done st -> fin st
       Fail st -> let (p, w, _) = fin st in (p, w, Nothing)

exec :: T2Ctx -> Int -> BS.ByteString -> Int -> St -> Outcome
exec ctx depth cs = loop
  where
    len = BS.length cs
    loop !p !st
      | p >= len = Ret st
      | sFuel st <= 0 = Fail st
      | otherwise =
          let b = u8 cs p
              st' = st { sFuel = sFuel st - 1 }
          in if b >= 32 then
               if b <= 246 then push (fromIntegral (b - 139)) (p + 1) st'
               else if b <= 250 then push (fromIntegral ((b - 247) * 256 + u8 cs (p + 1) + 108)) (p + 2) st'
               else if b <= 254 then push (fromIntegral (-(b - 251) * 256 - u8 cs (p + 1) - 108)) (p + 2) st'
               else push (fixed1616 cs (p + 1)) (p + 5) st'
             else if b == 28 then push (fromIntegral (i16 cs (p + 1))) (p + 3) st'
             else if b == 12 then escOp (u8 cs (p + 1)) (p + 2) st'
             else op b (p + 1) st'

    push v p st
      | sDepth st >= maxStack = Fail st
      | otherwise = loop p st { sStack = v : sStack st, sDepth = sDepth st + 1 }

    clear st = st { sStack = [], sDepth = 0 }
    args st = reverse (sStack st)

    -- Width: the first stack-clearing operator may carry one extra leading
    -- argument; whether it does is decided by the argument count's parity
    -- against what the operator takes.
    withWidth st extra
      | sHaveW st = (st, args st)
      | otherwise =
          let as = args st
          in if extra then case as of
                             (w : rest) -> (st { sHaveW = True, sWidth = Just w }, rest)
                             [] -> (st { sHaveW = True }, [])
             else (st { sHaveW = True }, as)

    move dx dy st = let (x, y) = pbPoint (sPath st) in st { sPath = pbMove (x + dx) (y + dy) (sPath st) }
    line dx dy pb = let (x, y) = pbPoint pb in pbLine (x + dx) (y + dy) pb
    curve a b c d e f pb =
      let (x, y) = pbPoint pb
          x1 = x + a; y1 = y + b; x2 = x1 + c; y2 = y1 + d; x3 = x2 + e; y3 = y2 + f
      in pbCurve x1 y1 x2 y2 x3 y3 pb
    draw f st = (clear st) { sPath = f (sPath st) }

    -- The width is settled by the first stack-clearing operator, which is
    -- always one of these (or endchar); a width-only run stops right after
    -- it. The test lives here and not at the top of 'loop', where it cost
    -- 25% on every number pushed.
    cont p st | t2WidthOnly ctx = Done st
              | otherwise = loop p st

    stemOp p st =
      let (st1, as) = withWidth st (odd (sDepth st))
      in cont p (clear st1 { sStems = sStems st1 + length as `div` 2 })

    op :: Int -> Int -> St -> Outcome
    op b p st = case b of
      1  -> stemOp p st
      3  -> stemOp p st
      18 -> stemOp p st
      23 -> stemOp p st
      19 -> mask p st
      20 -> mask p st
      21 -> let (st1, as) = withWidth st (sDepth st > 2)
            in case as of
                 (dx : dy : _) -> cont p (clear (move dx dy st1))
                 _ -> cont p (clear st1)
      22 -> let (st1, as) = withWidth st (sDepth st > 1)
            in case as of
                 (dx : _) -> cont p (clear (move dx 0 st1))
                 _ -> cont p (clear st1)
      4  -> let (st1, as) = withWidth st (sDepth st > 1)
            in case as of
                 (dy : _) -> cont p (clear (move 0 dy st1))
                 _ -> cont p (clear st1)
      5  -> loop p (draw (rlines (args st)) st)
      6  -> loop p (draw (altLines True (args st)) st)
      7  -> loop p (draw (altLines False (args st)) st)
      8  -> loop p (draw (rcurves (args st)) st)
      24 -> loop p (draw (rcurveline (args st)) st)
      25 -> loop p (draw (rlinecurve (args st)) st)
      26 -> loop p (draw (vvcurve (args st)) st)
      27 -> loop p (draw (hhcurve (args st)) st)
      30 -> loop p (draw (altCurves False (args st)) st)
      31 -> loop p (draw (altCurves True (args st)) st)
      10 -> callSubr False p st
      29 -> callSubr True p st
      11 -> Ret st
      14 -> let n = sDepth st
                (st1, as) = withWidth st (n == 1 || n == 5)
                st2 = st1
            in case as of
                 [adx, ady, bc, ac] -> Done (clear st2) { sSeac = Just (adx, ady, truncate' bc, truncate' ac) }
                 _ -> Done (clear st2)
      _  -> loop p (clear st)          -- reserved / CFF2-only (vsindex, blend)

    truncate' v = if isNaN v || abs v > 1e6 then -1 else truncate v :: Int

    mask p st =
      let (st1, as) = withWidth st (odd (sDepth st))
          stems = sStems st1 + length as `div` 2
          nBytes = (stems + 7) `div` 8
      in cont (p + nBytes) (clear st1 { sStems = stems })

    callSubr global p st = case sStack st of
      (v : rest)
        | depth >= maxSubrDepth -> Fail st
        | otherwise ->
            let (getS, cnt) = if global then (t2Gsubrs ctx, t2GsubrCount ctx) else (t2Lsubrs ctx, t2LsubrCount ctx)
                i = truncate' v + bias cnt
                st1 = st { sStack = rest, sDepth = sDepth st - 1 }
            in if i < 0 || i >= cnt then Fail st1
               else case exec ctx (depth + 1) (getS i) 0 st1 of
                      Ret st2 -> loop p st2
                      other -> other
      [] -> Fail st

    escOp :: Int -> Int -> St -> Outcome
    escOp e p st = case e of
      34 -> loop p (draw (\pb -> case args st of
               [dx1, dx2, dy2, dx3, dx4, dx5, dx6] ->
                 curve dx4 0 dx5 (-dy2) dx6 0 (curve dx1 0 dx2 dy2 dx3 0 pb)
               _ -> pb) st)
      35 -> loop p (draw (\pb -> case args st of
               [a1, b1, a2, b2, a3, b3, a4, b4, a5, b5, a6, b6, _] ->
                 curve a4 b4 a5 b5 a6 b6 (curve a1 b1 a2 b2 a3 b3 pb)
               _ -> pb) st)
      36 -> loop p (draw (\pb -> case args st of
               [dx1, dy1, dx2, dy2, dx3, dx4, dx5, dy5, dx6] ->
                 curve dx4 0 dx5 dy5 dx6 (-(dy1 + dy2 + dy5)) (curve dx1 dy1 dx2 dy2 dx3 0 pb)
               _ -> pb) st)
      37 -> loop p (draw (\pb -> case args st of
               [a1, b1, a2, b2, a3, b3, a4, b4, a5, b5, d6] ->
                 let dx = a1 + a2 + a3 + a4 + a5; dy = b1 + b2 + b3 + b4 + b5
                     (l6x, l6y) = if abs dx > abs dy then (d6, -dy) else (-dx, d6)
                 in curve a4 b4 a5 b5 l6x l6y (curve a1 b1 a2 b2 a3 b3 pb)
               _ -> pb) st)
      -- Arithmetic and storage (Type 2 §4.4–4.6), on the top of the stack.
      3  -> bin (\a b -> if a /= 0 && b /= 0 then 1 else 0)
      4  -> bin (\a b -> if a /= 0 || b /= 0 then 1 else 0)
      5  -> un (\a -> if a == 0 then 1 else 0)
      9  -> un abs
      10 -> bin (+)
      11 -> bin (-)
      12 -> bin (\a b -> if b == 0 then 0 else a / b)
      14 -> un negate
      15 -> bin (\a b -> if a == b then 1 else 0)
      18 -> case sStack st of
              (_ : r) -> loop p st { sStack = r, sDepth = sDepth st - 1 }
              [] -> Fail st
      20 -> case sStack st of
              (i : v : r) -> loop p st { sStack = r, sDepth = sDepth st - 2
                                       , sTrans = let k = truncate' i in if k >= 0 && k < 32 then IM.insert k v (sTrans st) else sTrans st }
              _ -> Fail st
      21 -> case sStack st of
              (i : r) -> loop p st { sStack = IM.findWithDefault 0 (truncate' i) (sTrans st) : r }
              [] -> Fail st
      22 -> case sStack st of
              (v2 : v1 : s2 : s1 : r) -> loop p st { sStack = (if v1 <= v2 then s1 else s2) : r, sDepth = sDepth st - 3 }
              _ -> Fail st
      23 -> push 0.5 p st          -- random: deterministic, this module is pure
      24 -> bin (*)
      26 -> un (\a -> if a < 0 then 0 else sqrt a)
      27 -> case sStack st of
              (a : r) -> push a p st { sStack = a : r }
              [] -> Fail st
      28 -> case sStack st of
              (a : b : r) -> loop p st { sStack = b : a : r }
              _ -> Fail st
      29 -> case sStack st of
              (i : r) -> let k = truncate' i
                             v = if k < 0 then headOr 0 r else headOr 0 (drop k r)
                         in loop p st { sStack = v : r }
              [] -> Fail st
      30 -> case sStack st of
              (j : n : r) ->
                let nn = truncate' n; jj = truncate' j
                    r' = r
                in if nn <= 0 || nn > length r' then Fail st
                   else let (seg, rest) = splitAt nn r'
                            -- top-first list: rolling "up" by j moves the top
                            -- element deeper.
                            k = jj `mod` nn
                            seg' = drop k seg ++ take k seg
                        in loop p st { sStack = seg' ++ rest, sDepth = sDepth st - 2 }
              _ -> Fail st
      _  -> loop p (clear st)       -- dotsection and reserved
      where
        bin f = case sStack st of
          (b : a : r) -> loop p st { sStack = f a b : r, sDepth = sDepth st - 1 }
          _ -> Fail st
        un f = case sStack st of
          (a : r) -> loop p st { sStack = f a : r }
          [] -> Fail st

    headOr d xs = case xs of (x : _) -> x; [] -> d

    rlines (dx : dy : r) pb = rlines r (line dx dy pb)
    rlines _ pb = pb
    altLines horiz (d : r) pb = altLines (not horiz) r (if horiz then line d 0 pb else line 0 d pb)
    altLines _ [] pb = pb
    rcurves (a : b : c : d : e : f : r) pb = rcurves r (curve a b c d e f pb)
    rcurves _ pb = pb
    rcurveline xs pb
      | length xs >= 8 = case xs of
          (a : b : c : d : e : f : r) -> rcurveline r (curve a b c d e f pb)
          _ -> pb
      | otherwise = case xs of
          [dx, dy] -> line dx dy pb
          _ -> pb
    rlinecurve xs pb
      | length xs > 6 = case xs of
          (dx : dy : r) -> rlinecurve r (line dx dy pb)
          _ -> pb
      | otherwise = case xs of
          [a, b, c, d, e, f] -> curve a b c d e f pb
          _ -> pb
    vvcurve xs pb = case xs of
      _ | odd (length xs), (dx1 : r) <- xs -> go dx1 r pb
      _ -> go 0 xs pb
      where go dx1 (dya : dxb : dyb : dyc : r) pb' = go 0 r (curve dx1 dya dxb dyb 0 dyc pb')
            go _ _ pb' = pb'
    hhcurve xs pb = case xs of
      _ | odd (length xs), (dy1 : r) <- xs -> go dy1 r pb
      _ -> go 0 xs pb
      where go dy1 (dxa : dxb : dyb : dxc : r) pb' = go 0 r (curve dxa dy1 dxb dyb dxc 0 pb')
            go _ _ pb' = pb'
    -- hvcurveto (horizontal first) / vhcurveto: alternating tangents, with an
    -- optional fifth argument on the last curve.
    altCurves horiz xs pb = case xs of
      [a, b, c, d, e] -> one horiz a b c d e pb
      (a : b : c : d : r@(_ : _)) -> altCurves (not horiz) r (one horiz a b c d 0 pb)
      [a, b, c, d] -> one horiz a b c d 0 pb
      _ -> pb
      where
        one True a b c d e = curve a 0 b c e d
        one False a b c d e = curve 0 a b c d e

-- | The Expert and ExpertSubset predefined charsets (SIDs for GIDs 1..).
expertCharset, expertSubsetCharset :: [Int]
expertCharset =
  [ 1, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 13, 14, 15, 99, 239, 240
  , 241, 242, 243, 244, 245, 246, 247, 248, 27, 28, 249, 250, 251, 252, 253, 254
  , 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 109, 110, 267, 268
  , 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284
  , 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300
  , 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316
  , 317, 318, 158, 155, 163, 319, 320, 321, 322, 323, 324, 325, 326, 150, 164, 169
  , 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342
  , 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358
  , 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374
  , 375, 376, 377, 378 ]
expertSubsetCharset =
  [ 1, 231, 232, 235, 236, 237, 238, 13, 14, 15, 99, 239, 240, 241, 242, 243, 244
  , 245, 246, 247, 248, 27, 28, 249, 250, 251, 253, 254, 255, 256, 257, 258, 259
  , 260, 261, 262, 263, 264, 265, 266, 109, 110, 267, 268, 269, 270, 272, 300, 301
  , 302, 305, 314, 315, 158, 155, 163, 320, 321, 322, 323, 324, 325, 326, 150, 164
  , 169, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341
  , 342, 343, 344, 345, 346 ]

