-- | Tests for the vector stack (plan 0034): Font.
--
-- Spec.hs calls 'fontSpec' with its own @check@, so a failure here is
-- counted and reported exactly like every other test. Fixtures are built
-- in code or checked in as bytes: the suite never shells out.
--
-- Every fixture is a complete, tiny font assembled byte by byte below: a
-- TrueType with simple, off-curve-only, composite (scaled, 2×2, point
-- matched, self-referencing) glyphs; a TrueType collection of it; a
-- name-keyed CFF exercising hint counting, both subroutine kinds, seac and
-- flex; a CID-keyed CFF with two FDs of different matrices; and a Type 1
-- whose private section and charstrings are encrypted here, carrying flex
-- and hint-replacement OtherSubrs and a seac, in PFA, hex-PFA and PFB form.
-- Each is then truncated at every length and bit-flipped at every offset
-- (Type 1 forms: sampled, see 'fuzzTests'), and every result is forced
-- completely: the readers must never throw.
module FontSpec (fontSpec, hostileHexPfa, hostileComposite) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_)
import Data.Bits (shiftR, xor, (.&.), shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import qualified Data.Text as T
import Data.Word (Word8)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import System.Timeout (timeout)

import Cmedit.Geom
import Cmedit.Font
import Cmedit.FontSys
import Cmedit.FontT1 (encryptT1, decryptT1)
import Cmedit.FontCore (cffStandardStrings)
import qualified Data.Array as A

fontSpec :: (String -> Bool -> IO ()) -> IO ()
fontSpec check = do
  trueTypeTests check
  cffTests check
  cidTests check
  type1Tests check
  sysTests check
  fuzzTests check

------------------------------------------------------------------------------
-- Byte building

be16, be32 :: Int -> [Word8]
be16 v = [fromIntegral (v `shiftR` 8), fromIntegral v]
be32 v = [fromIntegral (v `shiftR` 24), fromIntegral (v `shiftR` 16), fromIntegral (v `shiftR` 8), fromIntegral v]

bytes :: [Word8] -> BS.ByteString
bytes = BS.pack

pad4 :: BS.ByteString -> BS.ByteString
pad4 b = b <> BS.replicate ((4 - BS.length b `mod` 4) `mod` 4) 0

-- | An sfnt from (tag, table) pairs (checksums are not checked by readers).
sfnt :: [Word8] -> [(String, BS.ByteString)] -> BS.ByteString
sfnt version tables = BS.concat (header : dir : map (pad4 . snd) tables)
  where
    n = length tables
    header = bytes (version ++ be16 n ++ be16 0 ++ be16 0 ++ be16 0)
    offs = scanl (+) (12 + 16 * n) [ BS.length (pad4 t) | (_, t) <- tables ]
    dir = BS.concat [ BC.pack tag <> bytes (be32 0 ++ be32 o ++ be32 (BS.length t))
                    | ((tag, t), o) <- zip tables offs ]

------------------------------------------------------------------------------
-- TrueType fixture

-- | A simple glyph from contours of (x, y, on-curve). Uses short and long
-- coordinate forms and flag repeats.
simpleGlyph :: [[(Int, Int, Bool)]] -> BS.ByteString
simpleGlyph cs = bytes (be16 (length cs) ++ concatMap be16 [minimum xs, minimum ys, maximum xs, maximum ys]
                        ++ concatMap be16 ends ++ be16 0 ++ packFlags flags ++ concat xb ++ concat yb)
  where
    pts = concat cs
    xs = [ x | (x, _, _) <- pts ]; ys = [ y | (_, y, _) <- pts ]
    ends = tail (scanl (+) (-1) (map length cs))
    dxs = zipWith (-) xs (0 : xs); dys = zipWith (-) ys (0 : ys)
    enc d shortBit sameBit
      | d == 0 = (sameBit, [])
      | abs d < 256 = (shortBit + (if d > 0 then sameBit else 0), [fromIntegral (abs d)])
      | otherwise = (0, be16 (d .&. 0xFFFF))
    (xf, xb) = unzip [ enc d 2 16 | d <- dxs ]
    (yf, yb) = unzip [ enc d 4 32 | d <- dys ]
    flags = [ (if on then 1 else 0) + a + b | ((_, _, on), a, b) <- zip3 pts xf yf ]
    packFlags [] = []
    packFlags (f : rest) =
      let (same, more) = span (== f) rest
          r = min 255 (length same)
      in if r > 0 then fromIntegral (f + 8) : fromIntegral r : packFlags (drop r same ++ more)
         else fromIntegral f : packFlags rest

-- | A component: glyph, arguments (Left = x/y offset, Right = point
-- match), word-sized arguments, transform (1, 2 or 4 F2Dot14 values).
data Comp = Comp Int (Either (Int, Int) (Int, Int)) Bool (Maybe [Double])

compositeGlyph :: [Comp] -> BS.ByteString
compositeGlyph comps = bytes (be16 0xFFFF ++ concatMap be16 [0, 0, 0, 0] ++ concat (zipWith one [1 :: Int ..] comps))
  where
    n = length comps
    f2 v = be16 (round (v * 16384) .&. 0xFFFF)
    one k (Comp g args wordsA xf) =
      let (a1, a2, xy) = either (\(a, b) -> (a, b, True)) (\(a, b) -> (a, b, False)) args
          xfFlag = case xf of
            Just [_] -> 0x8
            Just [_, _] -> 0x40
            Just [_, _, _, _] -> 0x80
            _ -> 0
          flags = (if wordsA then 1 else 0) + (if xy then 2 else 0) + (if k < n then 0x20 else 0) + xfFlag
          argB | wordsA = be16 (a1 .&. 0xFFFF) ++ be16 (a2 .&. 0xFFFF)
               | otherwise = [fromIntegral (a1 .&. 0xFF), fromIntegral (a2 .&. 0xFF)]
      in be16 flags ++ be16 g ++ argB ++ concatMap f2 (maybe [] id xf)

-- Glyph 0 empty; 1 square; 2 off-curve diamond + a contour starting
-- off-curve; 3 composite (scaled square at words offset, rotated diamond at
-- signed byte offset); 4 point-matched composite; 5 self-reference; 6 a
-- composite naming itself 200 times (fan-out guard).
ttGlyphs :: [BS.ByteString]
ttGlyphs =
  [ BS.empty
  , simpleGlyph [[(0, 0, True), (500, 0, True), (500, 700, True), (0, 700, True)]]
  , simpleGlyph [ [(250, 0, False), (500, 350, False), (250, 700, False), (0, 350, False)]
                , [(600, 0, False), (700, 100, True), (600, 200, True)] ]
  , compositeGlyph [ Comp 1 (Left (1000, -100)) True (Just [0.5])
                   , Comp 2 (Left (-10, 20)) False (Just [0, 1, -1, 0]) ]
  , compositeGlyph [ Comp 1 (Left (0, 0)) False Nothing
                   , Comp 1 (Right (2, 0)) False Nothing ]
  , compositeGlyph [ Comp 5 (Left (0, 0)) False Nothing, Comp 1 (Left (0, 0)) False Nothing ]
  , compositeGlyph (replicate 200 (Comp 6 (Left (1, 1)) False Nothing))
  ]

ttFont :: BS.ByteString
ttFont = ttFontWith ttGlyphs

-- | The fuzzed variant leaves out glyph 6: its fan-out costs the full
-- composite budget on every variant, which is tested once, not 3000 times.
ttFontFuzz :: BS.ByteString
ttFontFuzz = ttFontWith (take 6 ttGlyphs)

ttFontWith :: [BS.ByteString] -> BS.ByteString
ttFontWith glyphs = sfnt [0, 1, 0, 0]
  [ ("cmap", cmap), ("glyf", glyf), ("head", headT), ("hhea", hhea), ("hmtx", hmtx)
  , ("kern", kern), ("loca", loca), ("maxp", maxp), ("name", name), ("post", post) ]
  where
    nG = length glyphs
    padded = map (\g -> g <> BS.replicate (BS.length g `mod` 2) 0) glyphs
    glyf = BS.concat padded
    locaOffs = scanl (+) 0 (map BS.length padded)
    loca = bytes (concatMap (be16 . (`div` 2)) locaOffs)
    headT = bytes (be32 0x00010000 ++ be32 0x00010000 ++ be32 0 ++ be32 0x5F0F3CF5 ++ be16 0
                   ++ be16 1000 ++ replicate 16 0 ++ concatMap be16 [0, (-200) .&. 0xFFFF, 1000, 800]
                   ++ be16 1 {- macStyle bold -} ++ be16 8 ++ be16 2 ++ be16 0 {- short loca -} ++ be16 0)
    maxp = bytes (be32 0x00005000 ++ be16 nG)
    hhea = bytes (be32 0x00010000 ++ be16 800 ++ be16 ((-200) .&. 0xFFFF) ++ replicate 26 0 ++ be16 3)
    hmtx = bytes (concat [ be16 a ++ be16 0 | a <- [500, 600, 700] ] ++ concat (replicate (nG - 3) (be16 0)))
    -- cmap: (3,1) format 4 with a delta segment and a range-offset segment;
    -- (1,0) format 0.
    f4 = let segs = 3
             ends' = [0x43, 0x62, 0xFFFF]; starts = [0x41, 0x61, 0xFFFF]
             deltas = [(1 - 0x41) .&. 0xFFFF, 0, 1]
             -- segment 1 uses the glyph array: idRangeOffset from its own slot
             -- (index 1) to the array just past the three offsets: (3 - 1) * 2.
             ros = [0, 4, 0]
             body = be16 (segs * 2) ++ be16 4 ++ be16 1 ++ be16 2 ++ concatMap be16 ends' ++ be16 0
                    ++ concatMap be16 starts ++ concatMap be16 deltas ++ concatMap be16 ros ++ concatMap be16 [4, 5]
         in be16 4 ++ be16 (6 + length body) ++ be16 0 ++ body
    f0 = be16 0 ++ be16 262 ++ be16 0 ++ [ if c == 0x41 then 1 else if c == 0x42 then 2 else 0 | c <- [0 .. 255 :: Int] ]
    cmap = bytes (be16 0 ++ be16 2 ++ be16 3 ++ be16 1 ++ be32 20 ++ be16 1 ++ be16 0 ++ be32 (20 + length f4) ++ f4 ++ f0)
    -- post format 2: .notdef, two custom names, then standard Mac names.
    post = bytes (be32 0x00020000 ++ replicate 28 0 ++ be16 nG
                  ++ concatMap be16 (take nG [0, 258, 259, 36, 37, 38, 39])
                  ++ pascal "square" ++ pascal "diamond")
    pascal s = fromIntegral (length s) : map (fromIntegral . ord) s
    -- name: Windows UTF-16 family/subfamily, Mac Roman full name.
    utf16 :: String -> [Word8]
    utf16 s = concatMap (be16 . ord) s
    strs = [ (3, 1, 0x409, 1, utf16 "Test Sans"), (3, 1, 0x409, 2, utf16 "Bold")
           , (1, 0, 0, 4, map (fromIntegral . ord) "Test Sans Bold") ]
    strOffs = scanl (+) 0 [ length s | (_, _, _, _, s) <- strs ]
    name = bytes (be16 0 ++ be16 (length strs) ++ be16 (6 + 12 * length strs)
                  ++ concat [ concatMap be16 [p, e, l, i, length s, o] | ((p, e, l, i, s), o) <- zip strs strOffs ]
                  ++ concat [ s | (_, _, _, _, s) <- strs ])
    kern = bytes (be16 0 ++ be16 1 ++ be16 0 ++ be16 20 ++ be16 1 ++ be16 1 ++ be16 0 ++ be16 0 ++ be16 0
                  ++ be16 1 ++ be16 2 ++ be16 ((-50) .&. 0xFFFF))

ttc :: BS.ByteString
ttc = BS.concat [hdr, shifted, shifted2]
  where
    ttFont = ttFontFuzz
    hdr = bytes (BS.unpack (BC.pack "ttcf") ++ be32 0x00010000 ++ be32 2 ++ be32 20 ++ be32 (20 + BS.length ttFont))
    -- Table offsets in a collection are from the start of the file.
    shifted = rebase 20
    shifted2 = rebase (20 + BS.length ttFont)
    rebase d = let n = fromIntegral (BS.index ttFont 4) * 256 + fromIntegral (BS.index ttFont 5)
                   recs = [ BS.take 8 (BS.drop r ttFont) <> bytes (be32 (rd32 (r + 8) + d)) <> BS.take 4 (BS.drop (r + 12) ttFont)
                          | i <- [0 .. n - 1], let r = 12 + 16 * i ]
               in BS.take 12 ttFont <> BS.concat recs <> BS.drop (12 + 16 * n) ttFont
    rd32 i = foldl' (\a k -> a * 256 + fromIntegral (BS.index ttFont (i + k))) 0 [0 .. 3]

trueTypeTests :: (String -> Bool -> IO ()) -> IO ()
trueTypeTests check = case parseFont ttFont of
  Left e -> check ("TrueType fixture parses (" ++ e ++ ")") False
  Right f -> do
    check "TT: format, glyph count, matrix" $
      fontFormat f == FmtTrueType && fontNumGlyphs f == 7 && fontMatrix f == scaleA 0.001 0.001
    check "TT: name table (UTF-16 family, subfamily, Mac Roman full name)" $
      fontFamily f == T.pack "Test Sans" && fontStyle f == T.pack "Bold" && fontFullName f == T.pack "Test Sans Bold"
    check "TT: bold from macStyle/subfamily; ascent/descent in ems" $
      fontBold f && not (fontItalic f) && fontAscent f == 0.8 && fontDescent f == -0.2
    check "TT: simple square outline" $
      glyphPath f 1 == [MoveTo (Pt 0 0), LineTo (Pt 500 0), LineTo (Pt 500 700), LineTo (Pt 0 700), ClosePath]
    check "TT: all-off-curve contour starts at the implied midpoint; off-first contour starts at its last point" $
      glyphPath f 2 ==
        [ MoveTo (Pt 125 175), QuadTo (Pt 250 0) (Pt 375 175), QuadTo (Pt 500 350) (Pt 375 525)
        , QuadTo (Pt 250 700) (Pt 125 525), QuadTo (Pt 0 350) (Pt 125 175), ClosePath
        , MoveTo (Pt 600 200), QuadTo (Pt 600 0) (Pt 700 100), ClosePath ]
    check "TT: composite with scale + word offset, 2x2 + negative byte offset" $
      take 5 (glyphPath f 3) == [MoveTo (Pt 1000 (-100)), LineTo (Pt 1250 (-100)), LineTo (Pt 1250 250), LineTo (Pt 1000 250), ClosePath]
      && glyphPath f 3 !! 5 == MoveTo (Pt (-185) 145)        -- (125,175) rotated 90° → (-175,125), + (-10,20)
    check "TT: point-matched component lands on the anchor" $
      drop 5 (glyphPath f 4) == [MoveTo (Pt 500 700), LineTo (Pt 1000 700), LineTo (Pt 1000 1400), LineTo (Pt 500 1400), ClosePath]
    check "TT: self-referencing composite terminates (depth-capped)" $
      let p = glyphPath f 5 in length p `seq` (length p > 0 && length p < 100)
    r <- timeout 5000000 (evaluate (length (glyphPath f 6)))
    check "TT: a composite fanning out to itself is bounded" (r /= Nothing)
    rc <- timeout 10000000 $ evaluate $ case parseFont hostileComposite of
      Right h -> let p = glyphPath h 2 in length p `seq` (length p, take 1 (drop (5 * 1023) p))
      Left _ -> (0, [])
    check "TT: 1024 point-matched components: all placed, staircase ends at the last anchor" $
      rc == Just (5 * 1024, [MoveTo (Pt (500 * 1023) (700 * 1023))])
    check "TT: hmtx advances, last metric repeated" $
      map (glyphAdvance f) [0, 1, 2, 3, 6] == [500, 600, 700, 700, 700]
    check "TT: cmap format 4 delta and glyph-array segments" $
      map (glyphForChar f) "ABCab" == map Just [1, 2, 3, 4, 5] && glyphForChar f 'Z' == Nothing
    check "TT: Mac Roman (1,0) subtable via cmapLookup" $
      fmap ($ 0x42) (cmapLookup f 1 0) == Just (Just 2) && map fst (fontCmaps f) == [(3, 1), (1, 0)]
    check "TT: post format 2 names, both directions" $
      map (glyphName f) [0, 1, 2, 3] == map Just [".notdef", "square", "diamond", "A"]
      && glyphByName f "diamond" == Just 2 && glyphByName f "B" == Just 4
    check "TT: kern format 0 pair" $ glyphKerning f 1 2 == -50 && glyphKerning f 2 1 == 0
    check "TT: identity CID mapping, out of range is Nothing" $
      glyphForCID f 3 == Just 3 && glyphForCID f 99 == Nothing && glyphPath f 99 == [] && glyphAdvance f (-1) == 0
    check "TTC: face count and both faces parse" $
      fontFaceCount ttc == 2 && either (const False) ((== 6) . fontNumGlyphs) (parseFontFace 1 ttc)
      && either (const False) (\g -> glyphPath g 1 == glyphPath f 1) (parseFontFace 0 ttc)
      && either (const True) (const False) (parseFontFace 2 ttc)

approxPath :: Path -> Path -> Bool
approxPath a b = length a == length b && and (zipWith same a b)
  where
    same (MoveTo p) (MoveTo q) = near p q
    same (LineTo p) (LineTo q) = near p q
    same (QuadTo p r) (QuadTo q t) = near p q && near r t
    same (CubicTo p r u) (CubicTo q t v) = near p q && near r t && near u v
    same ClosePath ClosePath = True
    same _ _ = False
    near (Pt x y) (Pt x' y') = abs (x - x') < 1e-9 && abs (y - y') < 1e-9

approxA :: Affine -> Affine -> Bool
approxA (Affine a b c d e f) (Affine a' b' c' d' e' f') =
  maximum (map abs [a - a', b - b', c - c', d - d', e - e', f - f']) < 1e-12

------------------------------------------------------------------------------
-- CFF fixture

-- | Type 2 / Type 1 charstring integer encoding.
csNum :: Int -> [Word8]
csNum v
  | v >= -107 && v <= 107 = [fromIntegral (v + 139)]
  | v >= 108 && v <= 1131 = let w = v - 108 in [fromIntegral (247 + w `div` 256), fromIntegral (w `mod` 256)]
  | v <= -108 && v >= -1131 = let w = -v - 108 in [fromIntegral (251 + w `div` 256), fromIntegral (w `mod` 256)]
  | otherwise = 28 : be16 (v .&. 0xFFFF)

-- | A charstring from numbers and operators (Right op, Right (12, op) as 1200+op).
cs :: [Either Int Int] -> BS.ByteString
cs = bytes . concatMap (either csNum op)
  where op o | o >= 1200 = [12, fromIntegral (o - 1200)]
             | otherwise = [fromIntegral o]

-- | INDEX with 4-byte offsets.
index :: [BS.ByteString] -> BS.ByteString
index [] = bytes (be16 0)
index items = bytes (be16 (length items) ++ [4] ++ concatMap be32 offs) <> BS.concat items
  where offs = scanl (+) 1 (map BS.length items)

-- | DICT entries: (operator, operands); integers as 5-byte 29 forms so
-- sizes never depend on values; reals as nibbles.
data Opnd = I Int | R String

dict :: [(Int, [Opnd])] -> BS.ByteString
dict = bytes . concatMap (\(o, as) -> concatMap enc as ++ opB o)
  where
    enc (I v) = 29 : be32 (v .&. 0xFFFFFFFF)
    enc (R s) = 30 : nibbles (map nib s ++ [15])
    nib c = case c of
      '.' -> 10; '-' -> 14; 'E' -> 11
      _ -> ord c - 48
    nibbles (a : b : r) = fromIntegral (a * 16 + b) : nibbles r
    nibbles [a] = [fromIntegral (a * 16 + 15)]
    nibbles [] = []
    opB o | o >= 1200 = [12, fromIntegral (o - 1200)]
          | otherwise = [fromIntegral o]

-- | Lay out a CFF whose Top DICT needs offsets to what follows it: the
-- builder is called with the final offsets, which works because every
-- integer operand has a fixed size.
data CffParts = CffParts { cpTop :: [(Int, Int)] -> BS.ByteString  -- offsets of blocks
                         , cpStrings :: [String], cpGsubrs :: [BS.ByteString]
                         , cpBlocks :: [BS.ByteString] }

buildCff :: String -> CffParts -> BS.ByteString
buildCff fname parts = BS.concat (prefix : cpBlocks parts)
  where
    hdr = bytes [1, 0, 4, 4]
    nameIx = index [BC.pack fname]
    strIx = index (map BC.pack (cpStrings parts))
    gIx = index (cpGsubrs parts)
    topWith offs = index [cpTop parts offs]
    sizes = map BS.length (cpBlocks parts)
    -- The Top DICT's size is the same for any offsets.
    dummyLen = BS.length (topWith [ (0, 0) | _ <- sizes ])
    base = BS.length hdr + BS.length nameIx + dummyLen + BS.length strIx + BS.length gIx
    offs = zip (scanl (+) base sizes) sizes
    prefix = BS.concat [hdr, nameIx, topWith offs, strIx, gIx]

op :: Int -> Either Int Int
op = Right

n :: Int -> Either Int Int
n = Left

-- Glyphs: .notdef; A (hstemhm + implicit vstem + hintmask whose mask byte
-- is 0x0e, then local and global subrs); acute; Aacute (endchar seac);
-- "custom" (arithmetic + hflex), named from the String INDEX.
cffFont :: BS.ByteString
cffFont = buildCff "TestCFF" CffParts
  { cpTop = \offs -> case offs of
      [(csO, _), (chO, _), (pO, pLen), _] ->
        dict [ (3, [I 391]), (4, [I 384]), (5, [I (-10), I (-20), I 900, I 800])
             , (1207, [R ".001", I 0, I 0, R ".001", I 0, I 0])
             , (15, [I chO]), (17, [I csO]), (18, [I pLen, I pO]) ]
      _ -> BS.empty
  , cpStrings = ["custom"]
  , cpGsubrs = [cs [n 10, n 0, n 10, n 10, n 0, n 10, op 8, op 11]]
  , cpBlocks = [csIx, charset, priv, lsubrs]
  }
  where
    glyphs =
      [ cs [op 14]
      , cs [n 50, n 0, n 10, n 30, n 10, op 18, n 5, n 10, op 19] <> bytes [0x0e]
        <> cs [n 10, n 20, op 21, n 100, n 0, op 5, n 0, n 100, op 5, n (-100), n 0, op 5
              , n (-107), op 10, n (-107), op 29, op 14]
      , cs [n 200, n 300, op 21, n 50, n 50, op 5, op 14]
      , cs [n 100, n 200, n 65, n 194, op 14]
      , cs [n 2, n 3, op 1224, n 4, op 1210, n 0, op 21, n 10, n 20, n 30, n 40, n 50, n 60, n 70, op 1234, op 14]
      ]
    csIx = index glyphs
    charset = bytes (0 : concatMap be16 [34, 125, 171, 391])
    lsubrs = index [cs [n 0, n (-50), op 5, op 11]]
    -- Private: Subrs offset is relative to the Private DICT; it is the
    -- next block, so its offset is the Private DICT's own size.
    privBody sz = dict [(20, [I 500]), (21, [I 600]), (19, [I sz])]
    priv = privBody (BS.length (privBody 0))

-- Hostile charstrings: a self-calling subr, a stack overflow, a hintmask
-- running off the end, an out-of-range subr index.
cffEvil :: BS.ByteString
cffEvil = buildCff "Evil" CffParts
  { cpTop = \offs -> case offs of
      [(csO, _), (pO, pLen), _] -> dict [ (17, [I csO]), (18, [I pLen, I pO]) ]
      _ -> BS.empty
  , cpStrings = [], cpGsubrs = []
  , cpBlocks = [csIx, priv, lsubrs] }
  where
    csIx = index
      [ cs [op 14]
      , cs [n 0, n 0, op 21, n 10, n 0, op 5, n (-107), op 10]
      , cs (replicate 600 (n 1) ++ [op 14])
      , cs [n 1, n 2, n 3, n 4, op 18, op 19]
      , cs [n 0, n 0, op 21, n 10, n 0, op 5, n 500, op 10, n 5, n 5, op 5, op 14] ]
    lsubrs = index [cs [n 10, n 0, op 5, n (-107), op 10, op 11]]
    privBody sz = dict [(19, [I sz])]
    priv = privBody (BS.length (privBody 0))

cffTests :: (String -> Bool -> IO ()) -> IO ()
cffTests check = do
 case parseFont cffEvil of
  Left e -> check ("CFF hostile fixture parses (" ++ e ++ ")") False
  Right f -> do
    r <- timeout 5000000 (evaluate (sum (map (length . glyphPath f) [0 .. 4])))
    check "CFF: recursive subrs, stack overflow, runaway hintmask, bad subr index all terminate" (r /= Nothing)
    check "CFF: a self-calling subr keeps what it drew before the depth limit" $
      let p = glyphPath f 1 in length p > 3 && length p < 20
    check "CFF: an out-of-range subr ends the glyph with its outline closed" $
      glyphPath f 4 == [MoveTo (Pt 0 0), LineTo (Pt 10 0), ClosePath]
 cffTests' check

cffTests' :: (String -> Bool -> IO ()) -> IO ()
cffTests' check = case parseFont cffFont of
  Left e -> check ("CFF fixture parses (" ++ e ++ ")") False
  Right f -> do
    check "CFF: format, count, names, matrix" $
      fontFormat f == FmtCFF && fontNumGlyphs f == 5 && fontPSName f == T.pack "TestCFF"
      && fontFamily f == T.pack "custom" && fontStyle f == T.pack "Bold" && fontBold f
      && approxA (fontMatrix f) (scaleA 0.001 0.001)
    check "CFF: hintmask bytes counted (incl. implicit vstem), local + global subrs" $
      glyphPath f 1 ==
        [ MoveTo (Pt 10 20), LineTo (Pt 110 20), LineTo (Pt 110 120), LineTo (Pt 10 120)
        , LineTo (Pt 10 70), CubicTo (Pt 20 70) (Pt 30 80) (Pt 30 90), ClosePath ]
    check "CFF: width = nominalWidthX + arg; defaultWidthX otherwise" $
      glyphAdvance f 1 == 650 && glyphAdvance f 0 == 500 && glyphAdvance f 2 == 500
    check "CFF: endchar seac composes base and offset accent" $
      glyphPath f 3 == glyphPath f 1 ++ transformPath (translateA 100 200) (glyphPath f 2)
    check "CFF: arithmetic operators and hflex" $
      glyphPath f 4 == [ MoveTo (Pt 10 0), CubicTo (Pt 20 0) (Pt 40 30) (Pt 80 30)
                       , CubicTo (Pt 130 30) (Pt 190 0) (Pt 260 0), ClosePath ]
    check "CFF: charset names (standard and custom strings), both directions" $
      map (glyphName f) [0 .. 4] == map Just [".notdef", "A", "acute", "Aacute", "custom"]
      && glyphByName f "custom" == Just 4 && glyphByName f "nope" == Nothing
    check "CFF: built-in StandardEncoding limited to present glyphs" $
      fontBuiltinEncoding f == Just (IM.fromList [(65, "A"), (194, "acute")])
    check "CFF: glyphForChar through glyph names" $
      glyphForChar f 'A' == Just 1 && glyphForChar f '\xc1' == Just 3 && glyphForChar f 'b' == Nothing
    check "CFF: parseFontAs HintCFF and a wrong hint both succeed" $
      either (const False) ((== 5) . fontNumGlyphs) (parseFontAs HintCFF cffFont)
      && either (const False) ((== 5) . fontNumGlyphs) (parseFontAs HintTrueType cffFont)

-- CID-keyed: ROS, charset format 2 (GIDs 1–3 → CIDs 100–102), FDSelect
-- format 3 (GIDs 0–1 → FD 0, 2–3 → FD 1), FD 1 with a FontMatrix twice the
-- default, so its glyphs come back pre-scaled into FD 0's units.
cidFont :: BS.ByteString
cidFont = buildCff "TestCID" CffParts
  { cpTop = \offs -> case offs of
      [(csO, _), (chO, _), (fdsO, _), (fdaO, _), _, _] ->
        dict [ (1230, [I 391, I 392, I 0]), (15, [I chO]), (17, [I csO]), (1236, [I fdaO]), (1237, [I fdsO]) ]
      _ -> BS.empty
  , cpStrings = ["Adobe", "Identity"]
  , cpGsubrs = []
  , cpBlocks = [csIx, charset, fdSelect, fdArray p0, priv0, priv1]
  }
  where
    sq = cs [n 0, n 0, op 21, n 100, n 0, op 5, n 0, n 100, op 5, n (-100), n 0, op 5, op 14]
    csIx = index [cs [op 14], sq, sq, sq]
    charset = bytes (2 : be16 100 ++ be16 2)
    fdSelect = bytes ([3] ++ be16 2 ++ be16 0 ++ [0] ++ be16 2 ++ [1] ++ be16 4)
    privD = dict [(20, [I 1000])]
    -- Absolute offsets of the two Private DICTs: they follow the FDArray,
    -- whose size does not depend on them.
    fdArray (o0, o1) = index [ dict [(18, [I (BS.length privD), I o0])]
                             , dict [(1207, [R ".002", I 0, I 0, R ".002", I 0, I 0]), (18, [I (BS.length privD), I o1])] ]
    priv0 = privD
    priv1 = privD
    -- Resolve the Private offsets from a first layout pass.
    p0 = let pre = buildCff "TestCID" CffParts
                     { cpTop = \_ -> dict [ (1230, [I 391, I 392, I 0]), (15, [I 0]), (17, [I 0]), (1236, [I 0]), (1237, [I 0]) ]
                     , cpStrings = ["Adobe", "Identity"], cpGsubrs = []
                     , cpBlocks = [csIx, charset, fdSelect, fdArray (0, 0)] }
         in (BS.length pre, BS.length pre + BS.length privD)

cidTests :: (String -> Bool -> IO ()) -> IO ()
cidTests check = case parseFont cidFont of
  Left e -> check ("CID CFF fixture parses (" ++ e ++ ")") False
  Right f -> do
    check "CID: format and CID flag" $ fontFormat f == FmtCFFCID && fontIsCID f
    check "CID: charset maps CID to GID" $
      map (glyphForCID f) [0, 100, 101, 102, 103] == [Just 0, Just 1, Just 2, Just 3, Nothing]
    check "CID: FD 0 glyph in font units" $
      glyphPath f 1 == [MoveTo (Pt 0 0), LineTo (Pt 100 0), LineTo (Pt 100 100), LineTo (Pt 0 100), ClosePath]
    check "CID: FD 1 glyph folded through its own FontMatrix" $
      approxPath (glyphPath f 2) [MoveTo (Pt 0 0), LineTo (Pt 200 0), LineTo (Pt 200 200), LineTo (Pt 0 200), ClosePath]
      && abs (glyphAdvance f 2 - 2000) < 1e-9 && glyphAdvance f 1 == 1000
    check "CID: no glyph names" $ glyphName f 1 == Nothing

------------------------------------------------------------------------------
-- Type 1 fixture

t1Clear :: String
t1Clear = unlines
  [ "%!PS-AdobeFont-1.0: TestT1 001"
  , "11 dict begin"
  , "/FontInfo 8 dict dup begin"
  , "/FamilyName (Test (Serif)) readonly def"
  , "/Weight (Bold) readonly def"
  , "/ItalicAngle -12 def"
  , "/isFixedPitch true def"
  , "end readonly def"
  , "/FontName /TestT1 def"
  , "/PaintType 0 def /FontType 1 def"
  , "/FontMatrix [0.001 0 0 0.001 0 0] readonly def"
  , "/Encoding 256 array"
  , "0 1 255 {1 index exch /.notdef put} for"
  , "dup 65 /A put"
  , "dup 66 /B put"
  , "dup 193 /Aacute put"
  , "readonly def"
  , "/FontBBox {-10 -20 900 800} readonly def"
  , "currentdict end"
  , "currentfile eexec" ]

-- | A Type 1 charstring, encrypted with lenIV = 4.
t1cs :: [Either Int Int] -> BS.ByteString
t1cs prog = encryptT1 4330 (bytes [1, 2, 3, 4] <> cs1 prog)
  where cs1 = bytes . concatMap (either num opc)
        num v | v >= -107 && v <= 107 = [fromIntegral (v + 139)]
              | v >= 108 && v <= 1131 = let w = v - 108 in [fromIntegral (247 + w `div` 256), fromIntegral (w `mod` 256)]
              | v <= -108 && v >= -1131 = let w = -v - 108 in [fromIntegral (251 + w `div` 256), fromIntegral (w `mod` 256)]
              | otherwise = 255 : be32 v
        opc o | o >= 1200 = [12, fromIntegral (o - 1200)]
              | otherwise = [fromIntegral o]

t1Private :: BS.ByteString
t1Private = BS.concat $
  [ BC.pack "dup /Private 8 dict dup begin\n"
  , BC.pack "/RD{string currentfile exch readstring pop}executeonly def\n"
  , BC.pack "/ND{noaccess def}executeonly def\n/NP{noaccess put}executeonly def\n"
  , BC.pack "/lenIV 4 def\n/MinFeature{16 16} def\n/password 5839 def\n"
  , BC.pack "/Subrs 5 array\n" ]
  ++ [ entry (BC.pack ("dup " ++ show i ++ " ")) s (BC.pack " NP\n") | (i, s) <- zip [0 :: Int ..] subrs ]
  ++ [ BC.pack "ND\n2 index /CharStrings 6 dict dup begin\n" ]
  ++ [ entry (BC.pack ("/" ++ nm ++ " ")) s (BC.pack " ND\n") | (nm, s) <- glyphs ]
  ++ [ BC.pack "end\nend\nreadonly put\nnoaccess put\ndup/FontName get exch definefont pop\nmark currentfile closefile\n" ]
  where
    entry pre s post = pre <> BC.pack (show (BS.length s) ++ " RD ") <> s <> post
    subrs = map t1cs
      [ [n 3, n 0, op 1216, op 1217, op 1217, op 1233, op 11]
      , [n 0, n 1, op 1216, op 11]
      , [n 0, n 2, op 1216, op 11]
      , [op 11]
      , [n 10, n 0, op 5, n 4, op 10, op 11] ]
    glyphs =
      [ (".notdef", t1cs [n 0, n 250, op 13, op 14])
      , ("A", t1cs [n 20, n 500, op 13, n 100, n 0, op 5, n 0, n 100, op 5, n (-100), n 0, op 5, op 9, op 14])
      , ("B", t1cs ([n 0, n 600, op 13, n 3, n 1, n 3, op 1216, op 1217, op 10
                    , n 0, n 100, op 21, n 1, op 10]
                    ++ concat [ [n dx, n dy, op 21, n 2, op 10]
                              | (dx, dy) <- [(50, 0), (10, 10), (20, 10), (20, 0), (20, 0), (20, -10), (10, -10)] ]
                    ++ [n 50, n 150, n 100, n 0, op 10, n 0, n (-100), op 5, op 9, op 14]))
      , ("acute", t1cs [n 40, n 300, op 13, n 0, n 0, op 21, n 50, n 0, op 5, n 0, n 50, op 5, op 9, op 14])
      , ("Aacute", t1cs [n 30, n 600, op 13, n 40, n 10, n 200, n 65, n 194, op 1206])
      , ("D", t1cs [n 0, n 700, op 13, n 7, n 2, op 1212, n 0, op 21, n 30, n 0, op 5, op 9, op 14])
      , ("E", t1cs [n 0, n 700, op 13, n 4, op 10, op 14]) ]

t1Cipher :: BS.ByteString
t1Cipher = encryptT1 55665 (bytes [0x11, 0x22, 0x33, 0x44] <> t1Private)

pfa, pfaHex, pfb :: BS.ByteString
pfa = BC.pack t1Clear <> t1Cipher <> BC.pack (replicate 64 '0' ++ "\ncleartomark\n")
pfaHex = BC.pack t1Clear <> BC.pack (hexLines (BS.unpack t1Cipher)) <> BC.pack "\ncleartomark\n"
  where hexLines ws = unlines (chunks (concatMap hex2 ws))
        hex2 w = [hexD (fromIntegral w `div` 16), hexD (fromIntegral w `mod` 16)]
        hexD d = "0123456789abcdef" !! d
        chunks [] = []
        chunks s = take 64 s : chunks (drop 64 s)
pfb = seg 1 (BC.pack t1Clear) <> seg 2 t1Cipher <> seg 1 (BC.pack "cleartomark\n") <> bytes [0x80, 3]
  where seg t d = let l = BS.length d
                  in bytes [0x80, t, fromIntegral l, fromIntegral (l `shiftR` 8), fromIntegral (l `shiftR` 16), fromIntegral (l `shiftR` 24)] <> d

-- | A hex PFA whose private section carries @pad@ bytes of filler (lines
-- of numbers, so the tokeniser has work to do), hex-encoded in upper case
-- with CRLF line breaks: the shape a hostile PDF @FontFile@ would take to
-- make the hex decoder and tokeniser expensive.
hostileHexPfa :: Int -> BS.ByteString
hostileHexPfa pad = BC.pack t1Clear <> hexed <> BC.pack "\r\ncleartomark\r\n"
  where
    filler = BC.pack (take pad (cycle "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15\n"))
    priv = BC.pack "% filler\n" <> filler <> t1Private
    cipher = encryptT1 55665 (bytes [0x11, 0x22, 0x33, 0x44] <> priv)
    hexed = BS.intercalate (BC.pack "\r\n") (chunk (BC.map toUpperHex (BL.toStrict (BB.toLazyByteString (BB.byteStringHex cipher)))))
    toUpperHex c = if c >= 'a' && c <= 'f' then toEnum (fromEnum c - 32) else c
    chunk b | BS.null b = []
            | otherwise = BS.take 64 b : chunk (BS.drop 64 b)

-- | A TrueType whose glyph 2 is 1024 point-matched components, each square
-- anchored at the previous square's top-right corner: a staircase whose
-- last step proves every match landed, and whose cost was quadratic when
-- the composed points were re-flattened per component.
hostileComposite :: BS.ByteString
hostileComposite = ttFontWith
  [ BS.empty
  , simpleGlyph [[(0, 0, True), (500, 0, True), (500, 700, True), (0, 700, True)]]
  , compositeGlyph (Comp 1 (Left (0, 0)) True Nothing
                    : [ Comp 1 (Right (4 * k + 2, 0)) True Nothing | k <- [0 .. 1022] ]) ]

type1Tests :: (String -> Bool -> IO ()) -> IO ()
type1Tests check = do
  check "Tables: 391 CFF standard strings, 258 Mac names, StandardEncoding" $
    snd (A.bounds cffStandardStrings) == 390 && cffStandardStrings A.! 390 == "Semibold"
    && snd (A.bounds macGlyphNames) == 257 && macGlyphNames A.! 257 == "dcroat"
    && IM.lookup 39 standardEncodingNames == Just "quoteright" && IM.lookup 251 standardEncodingNames == Just "germandbls"
    && IM.size standardEncodingNames == 149
    && IM.lookup 0x80 winAnsiEncodingNames == Just "Euro" && IM.lookup 0xDB macRomanEncodingNames == Just "currency"
    && aglNameToChar "uni20AC" == Just '\x20ac' && aglNameToChar "a.sc" == Just 'a' && aglNameToChar "Eacute" == Just '\xc9'
    && take 1 (aglCharToNames '\xb5') == ["mu"] && last (aglCharToNames '\x1F600') == "u1F600"
  check "T1: eexec decryption inverts encryption" $
    decryptT1 55665 (encryptT1 55665 (BC.pack "hello")) == BC.pack "hello"
  forM_ [("PFA", pfa), ("hex PFA", pfaHex), ("PFB", pfb)] $ \(label, bs) -> case parseFont bs of
    Left e -> check ("T1 " ++ label ++ " parses (" ++ e ++ ")") False
    Right f -> do
      check ("T1 " ++ label ++ ": metadata") $
        fontFormat f == FmtType1 && fontNumGlyphs f == 7 && fontPSName f == T.pack "TestT1"
        && fontFamily f == T.pack "Test (Serif)" && fontBold f && fontItalic f && fontFixedPitch f
        && fontItalicAngle f == -12 && fontBBox f == Rect (-10) (-20) 900 800
      check ("T1 " ++ label ++ ": .notdef is GID 0, names round-trip") $
        glyphName f 0 == Just ".notdef" && glyphByName f "B" == Just 2 && glyphName f 2 == Just "B"
      check ("T1 " ++ label ++ ": hsbw side bearing + lines + closepath") $
        glyphPath f 1 == [MoveTo (Pt 20 0), LineTo (Pt 120 0), LineTo (Pt 120 100), LineTo (Pt 20 100), ClosePath]
        && glyphAdvance f 1 == 500
      check ("T1 " ++ label ++ ": flex OtherSubrs become two curves; hint replacement pops its subr") $
        glyphPath f 2 ==
          [ MoveTo (Pt 0 100), CubicTo (Pt 60 110) (Pt 80 120) (Pt 100 120)
          , CubicTo (Pt 120 120) (Pt 140 110) (Pt 150 100), LineTo (Pt 150 0), ClosePath ]
      check ("T1 " ++ label ++ ": seac places the accent from the composite's side bearing") $
        glyphPath f 4 == glyphPath f 1 ++ [MoveTo (Pt 40 200), LineTo (Pt 90 200), LineTo (Pt 90 250), ClosePath]
        && glyphAdvance f 4 == 600
      check ("T1 " ++ label ++ ": div") $
        glyphPath f 5 == [MoveTo (Pt 3.5 0), LineTo (Pt 33.5 0), ClosePath]
      check ("T1 " ++ label ++ ": a self-calling subr stops at the depth limit") $
        let p = glyphPath f 6 in length p > 2 && length p < 20
      check ("T1 " ++ label ++ ": explicit Encoding") $
        fontBuiltinEncoding f == Just (IM.fromList [(65, "A"), (66, "B"), (193, "Aacute")])
      check ("T1 " ++ label ++ ": glyphForChar through names") $
        glyphForChar f 'A' == Just 1 && glyphForChar f '\xc1' == Just 4
  case parseFont (hostileHexPfa 50000) of
    Left e -> check ("T1: large upper-case CRLF hex section parses (" ++ e ++ ")") False
    Right f -> check "T1: large upper-case CRLF hex section decodes to the same font" $
      fontNumGlyphs f == 7 && either (const False) (\g -> glyphPath g 2 == glyphPath f 2 && glyphAdvance g 4 == glyphAdvance f 4) (parseFont pfa)
  -- PDF FontFile whose cleartext has lost its keyword: the lengths decide.
  let clear' = BC.pack (take (length t1Clear - 6) t1Clear) <> BC.pack "EEXEC\n"
      raw = clear' <> t1Cipher
  check "T1: PDF Length1/Length2 used when there is no eexec keyword" $
    either (const False) ((== 7) . fontNumGlyphs) (parseFontAs (HintType1 (Just (BS.length clear', BS.length t1Cipher))) raw)
  check "T1: StandardEncoding built-in when the font says so" $
    either (const False) (\f -> fmap (IM.lookup 65) (fontBuiltinEncoding f) == Just (Just "A"))
      (parseFont (BC.pack (replaceEnc t1Clear) <> t1Cipher))
  where
    replaceEnc s = unlines (map (\l -> if l == "/Encoding 256 array" then "/Encoding StandardEncoding def" else l)
                     (filter (\l -> not (any (`elem` words l) ["dup", "for"]) && l /= "readonly def") (lines s)))

------------------------------------------------------------------------------
-- FontSys

sysTests :: (String -> Bool -> IO ()) -> IO ()
sysTests check = do
  let linux = [ "/usr/share/fonts/opentype/urw-base35/" ++ f | f <- ["NimbusSans-Regular.otf", "NimbusSans-Bold.otf", "NimbusSans-BoldItalic.otf"
                                                                     , "NimbusRoman-Regular.otf", "NimbusRoman-Italic.otf", "NimbusMonoPS-Regular.otf"
                                                                     , "StandardSymbolsPS.otf", "D050000L.otf", "P052-Roman.otf", "C059-Bold.otf"] ]
           ++ [ "/usr/share/fonts/type1/urw-base35/NimbusSans-BoldItalic.t1", "/usr/share/fonts/type1/urw-base35/NimbusSans-BoldItalic.afm" ]
           ++ [ "/usr/share/fonts/truetype/liberation/" ++ f | f <- ["LiberationSans-Regular.ttf", "LiberationSerif-Italic.ttf", "LiberationMono-Bold.ttf"] ]
           ++ [ "/usr/share/fonts/truetype/dejavu/" ++ f | f <- ["DejaVuSans.ttf", "DejaVuSans-Bold.ttf", "DejaVuSerif.ttf", "DejaVuSansMono.ttf"] ]
           ++ [ "/usr/share/fonts/truetype/noto/NotoSerifDisplay-Bold.ttf", "/usr/share/fonts/X11/misc/6x13.pcf.gz" ]
      windows = [ "C:\\Windows\\Fonts\\" ++ f | f <- ["arial.ttf", "arialbd.ttf", "ariali.ttf", "times.ttf", "timesbi.ttf", "cour.ttf", "symbol.ttf", "calibri.ttf", "verdana.ttf"] ]
      best files fam b i = case substituteFor files (fontRequest (T.pack fam)) { frBold = b, frItalic = i } of
        (x : _) -> reverse (takeWhile (\c -> c /= '/' && c /= '\\') (reverse x))
        [] -> ""
      bestF files req = case substituteFor files req of
        (x : _) -> reverse (takeWhile (\c -> c /= '/' && c /= '\\') (reverse x))
        [] -> ""
  check "Sys: family names parse (subset tag, vendor suffix, style)" $
    parseFamilyName (T.pack "ABCDEF+TimesNewRomanPS-BoldItalicMT") == ("timesnewroman", True, True)
    && parseFamilyName (T.pack "ArialMT") == ("arial", False, False)
    && parseFamilyName (T.pack "Helvetica-Oblique") == ("helvetica", False, True)
    && parseFamilyName (T.pack "Arial,Bold") == ("arial", True, False)
    && parseFamilyName (T.pack "Nimbus Roman") == ("nimbusroman", False, False)
  check "Sys: standard 14 on Linux map to URW base35, style from the name" $
    best linux "Helvetica-BoldOblique" False False == "NimbusSans-BoldItalic.otf"
    && best linux "Helvetica" False False == "NimbusSans-Regular.otf"
    && best linux "Times-Italic" False False == "NimbusRoman-Italic.otf"
    && best linux "Courier" False False == "NimbusMonoPS-Regular.otf"
    && best linux "Symbol" False False == "StandardSymbolsPS.otf"
    && best linux "ZapfDingbats" False False == "D050000L.otf"
  check "Sys: OpenType preferred over the Type 1 twin; non-fonts never returned" $
    let r = substituteFor linux (fontRequest (T.pack "Helvetica-BoldOblique"))
    in take 2 (map (reverse . takeWhile (/= '/') . reverse) r) == ["NimbusSans-BoldItalic.otf", "NimbusSans-BoldItalic.t1"]
       && not (any (\f -> ".afm" `isSuffixOfS` f || ".gz" `isSuffixOfS` f) r)
  check "Sys: Arial / Times New Roman go to Liberation first" $
    best linux "ArialMT" False False == "LiberationSans-Regular.ttf"
    && best linux "TimesNewRomanPS-ItalicMT" False False == "LiberationSerif-Italic.ttf"
    && best linux "CourierNewPS-BoldMT" False False == "LiberationMono-Bold.ttf"
  check "Sys: flags add style; missing style falls back within the family" $
    best linux "Helvetica" True False == "NimbusSans-Bold.otf"
    && best linux "Palatino-Bold" False False == "P052-Roman.otf"
    && best linux "NewCenturySchlbk-Roman" False False == "C059-Bold.otf"
  check "Sys: CSS generics and literal families" $
    best linux "sans-serif" False False == "DejaVuSans.ttf"
    && best linux "serif" False False == "DejaVuSerif.ttf"
    && best linux "monospace" False False == "DejaVuSansMono.ttf"
    && best linux "DejaVu Sans" True False == "DejaVuSans-Bold.ttf"
    && best linux "Noto Serif Display" True False == "NotoSerifDisplay-Bold.ttf"
  check "Sys: unknown families follow the descriptor flags" $
    bestF linux (fontRequest (T.pack "ABCDEF+Weird")) { frMono = True } == "DejaVuSansMono.ttf"
    && bestF linux (fontRequest (T.pack "Weird")) { frSerif = True } == "DejaVuSerif.ttf"
    && bestF linux (fontRequest (T.pack "Weird")) == "DejaVuSans.ttf"
  check "Sys: Windows core font names" $
    best windows "Helvetica" False False == "arial.ttf"
    && best windows "Arial" True False == "arialbd.ttf"
    && best windows "Times-BoldItalic" False False == "timesbi.ttf"
    && best windows "Courier" False False == "cour.ttf"
    && best windows "Symbol" False False == "symbol.ttf"
    && best windows "Calibri" False False == "calibri.ttf"
  check "Sys: empty list gives no candidates" $ null (substituteFor [] (fontRequest (T.pack "Helvetica")))
  check "Sys: font directories per OS" $
    "/usr/share/fonts" `elem` fontDirectories OSLinux [] "/home/u" && "/home/u/.local/share/fonts" `elem` fontDirectories OSLinux [] "/home/u"
    && "/home/u/Library/Fonts" `elem` fontDirectories OSMac [] "/home/u"
    && fontDirectories OSWindows [] "C:\\Users\\u"
         == ["C:\\Windows\\Fonts", "C:\\Users\\u\\AppData\\Local\\Microsoft\\Windows\\Fonts"]
    && fontDirectories OSWindows [("WINDIR", "D:\\WIN\\"), ("LOCALAPPDATA", "E:\\Local")] "C:\\Users\\u"
         == ["D:\\WIN\\Fonts", "E:\\Local\\Microsoft\\Windows\\Fonts"]
    && head (fontDirectories OSWindows [("SystemRoot", "F:\\Windows")] "C:\\Users\\u") == "F:\\Windows\\Fonts"
  where isSuffixOfS suf s = reverse suf == take (length suf) (reverse s)

------------------------------------------------------------------------------
-- Fuzz

-- | Force everything a caller could ask of a font.
forceFont :: Font -> Double
forceFont f =
  let ng = min 64 (fontNumGlyphs f)
      gs = [0 .. ng - 1]
      pathSum p = foldl' (+) 0 [ x + y | s <- p, Pt x y <- segPts s ]
      segPts s = case s of
        MoveTo a -> [a]; LineTo a -> [a]; QuadTo a b -> [a, b]; CubicTo a b c -> [a, b, c]; ClosePath -> []
      names = sum [ maybe 0 length (glyphName f g) | g <- gs ]
      cm = sum [ maybe 0 id (look c) | (_, look) <- fontCmaps f, c <- [0, 65, 0x20AC, 0x1F600] ]
      enc = maybe 0 (sum . map length . IM.elems) (fontBuiltinEncoding f)
      Affine a b c d e g = fontMatrix f
      Rect x0 y0 x1 y1 = fontBBox f
  in sum [ pathSum (glyphPath f k) + glyphAdvance f k | k <- gs ]
     + fromIntegral (names + cm + enc + T.length (fontFamily f) + T.length (fontStyle f) + T.length (fontPSName f))
     + a + b + c + d + e + g + x0 + y0 + x1 + y1 + fontAscent f + fontDescent f
     + maybe 0 fromIntegral (glyphForChar f 'A') + maybe 0 fromIntegral (glyphForCID f 1)
     + maybe 0 fromIntegral (glyphByName f "A") + glyphKerning f 1 2

forceResult :: Either String Font -> Double
forceResult (Left e) = fromIntegral (length e)
forceResult (Right f) = forceFont f

fuzzTests :: (String -> Bool -> IO ()) -> IO ()
fuzzTests check = do
  let fixtures = [("TrueType", ttFontFuzz), ("TTC", ttc), ("CFF", cffFont), ("CID CFF", cidFont), ("CFF hostile", cffEvil)
                 , ("Type1 PFA", pfa), ("Type1 hex", pfaHex), ("Type1 PFB", pfb)]
  forM_ fixtures $ \(label, bs) -> do
    let len = BS.length bs
        -- Type 1 parses tokenise the whole program, which is slow at -O0;
        -- its flips are sampled more sparsely (truncations stay exhaustive).
        step = if take 4 label == "Type" then 3 else 1
        -- The hex form is the binary form's bytes spelled twice as long;
        -- its truncations are sampled, the other forms' are exhaustive.
        truncs = [ BS.take k bs | k <- [0, (if label == "Type1 hex" then 4 else 1) .. len] ]
        flips = [ flipBit k bs | k <- [0, step .. len - 1] ]
        -- Bytes set to their extremes too: counts, offsets and lengths
        -- become huge or zero.
        extremes = [ setByte k v bs | k <- [0, 5 * step .. len - 1], v <- [0, 0xFF] ]
        run variants = timeout 60000000 $ try (evaluate (sum (map (forceResult . parseFont) variants)))
                         :: IO (Maybe (Either SomeException Double))
    r1 <- run truncs
    check ("fuzz " ++ label ++ ": truncations are total") (ok r1)
    r2 <- run flips
    check ("fuzz " ++ label ++ ": bit flips are total") (ok r2)
    r3 <- run extremes
    check ("fuzz " ++ label ++ ": 0x00/0xFF bytes are total") (ok r3)
  where
    ok (Just (Right _)) = True
    ok _ = False
    flipBit k bs = let (a, b) = BS.splitAt k bs
                   in case BS.uncons b of
                        Just (w, rest) -> a <> BS.cons (w `xor` (1 `shiftL` (k `mod` 8))) rest
                        Nothing -> bs
    setByte k v bs = let (a, b) = BS.splitAt k bs in a <> BS.cons v (BS.drop 1 b)
