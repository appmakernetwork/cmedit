-- | Oracle cases for "Cmedit.Raster" (plan 0034, workstream A).
--
-- Each case is an SVG document and the 'Scene' a front-end ought to build
-- from it, written side by side by hand (the geometry comes from one Haskell
-- 'Path' rendered both ways, so the comparison measures the rasteriser and
-- not two different circle approximations). This program writes, for every
-- case, @NAME.svg@ and @NAME.out.png@ (our rendering) into a directory;
-- @rastercheck.sh@ renders each SVG with resvg and compares the pair with
-- ImageMagick. No process is spawned from here.
--
-- With the single argument @bench@ it instead times the plan's performance
-- targets (rastercheck.sh bench).
module Main (main) where

import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import Data.List (intercalate)
import Data.Word (Word8, Word32)
import Numeric (showFFloat)
import System.Environment (getArgs)
import Control.Exception (evaluate, try, SomeException)
import Control.Monad (when, forM_)
import Data.Array.ST (runSTUArray, newArray, writeArray)
import System.Exit (exitFailure)
import GHC.Clock (getMonotonicTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Data.Array.Unboxed as U

import Cmedit.Geom
import Cmedit.Image (Image(..))
import Cmedit.Raster
import Cmedit.Scene

------------------------------------------------------------------------------
-- PNG writing (stored deflate: no compression, which ImageMagick and resvg
-- read like any other PNG)

crcTable :: U.UArray Int Word32
crcTable = U.listArray (0, 255) [ go (fromIntegral n) (8 :: Int) | n <- [0 .. 255 :: Int] ]
  where go c 0 = c
        go c k = go (if c .&. 1 /= 0 then 0xedb88320 `xor` (c `shiftR` 1) else c `shiftR` 1) (k - 1)

crc32 :: BS.ByteString -> Word32
crc32 = xor 0xffffffff . BS.foldl' step 0xffffffff
  where step c b = (crcTable U.! fromIntegral ((c `xor` fromIntegral b) .&. 0xff)) `xor` (c `shiftR` 8)

adler32 :: BS.ByteString -> Word32
adler32 bs = let (a, b) = BS.foldl' (\(x, y) w -> let x' = (x + fromIntegral w) `mod` 65521 in (x', (y + x') `mod` 65521)) (1, 0) bs
             in b * 65536 + a

be32 :: Word32 -> BS.ByteString
be32 v = BS.pack [ fromIntegral (v `shiftR` s) | s <- [24, 16, 8, 0] ]

chunk :: String -> BS.ByteString -> BS.ByteString
chunk ty dat = let t = BC.pack ty in BS.concat [be32 (fromIntegral (BS.length dat)), t, dat, be32 (crc32 (t <> dat))]

pngBytes :: Int -> Int -> [Word8] -> BS.ByteString
pngBytes w h rgba =
  let rows = [ BS.pack (0 : take (4 * w) (drop (4 * w * y) rgba)) | y <- [0 .. h - 1] ]
      raw = BS.concat rows
      blocks = splitN 65535 raw
      stored = BS.concat [ BS.concat [ BS.pack [if lst then 1 else 0, fromIntegral l, fromIntegral (l `shiftR` 8)
                                               , fromIntegral (complement16 l), fromIntegral (complement16 l `shiftR` 8)], b ]
                         | (b, lst) <- zip blocks (map (== length blocks) [1 ..]), let l = BS.length b ]
      complement16 l = 65535 - l
      zlib = BS.concat [BS.pack [0x78, 0x01], stored, be32 (adler32 raw)]
      ihdr = BS.concat [be32 (fromIntegral w), be32 (fromIntegral h), BS.pack [8, 6, 0, 0, 0]]
  in BS.concat [BS.pack [137, 80, 78, 71, 13, 10, 26, 10], chunk "IHDR" ihdr, chunk "IDAT" zlib, chunk "IEND" BS.empty]
  where
    splitN n b | BS.null b = [BS.empty]
               | otherwise = go b
      where go x | BS.null x = []
                 | otherwise = let (a, r) = BS.splitAt n x in a : go r

imagePng :: Image -> BS.ByteString
imagePng img = pngBytes (imgW img) (imgH img) (U.elems (imgPix img))

base64 :: BS.ByteString -> String
base64 = go . BS.unpack
  where
    tbl = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "+/"
    enc i = tbl !! fromIntegral i
    go (a : b : c : r) = let n = (fromIntegral a * 65536 + fromIntegral b * 256 + fromIntegral c) :: Int
                         in map (enc . (\s -> (n `shiftR` s) .&. 63)) [18, 12, 6, 0] ++ go r
    go [a, b] = let n = (fromIntegral a * 65536 + fromIntegral b * 256) :: Int
                in map (enc . (\s -> (n `shiftR` s) .&. 63)) [18, 12, 6] ++ "="
    go [a] = let n = (fromIntegral a * 65536) :: Int
             in map (enc . (\s -> (n `shiftR` s) .&. 63)) [18, 12] ++ "=="
    go [] = []

------------------------------------------------------------------------------
-- Shared geometry

num :: Double -> String
num v = showFFloat (Just 4) v ""

pathD :: Path -> String
pathD = unwords . map seg
  where
    p (Pt x y) = num x ++ "," ++ num y
    seg s = case s of
      MoveTo a -> "M" ++ p a
      LineTo a -> "L" ++ p a
      QuadTo a b -> "Q" ++ p a ++ " " ++ p b
      CubicTo a b c -> "C" ++ p a ++ " " ++ p b ++ " " ++ p c
      ClosePath -> "Z"

circleP :: Double -> Double -> Double -> Path
circleP cx cy r = ellipseP cx cy r r

ellipseP :: Double -> Double -> Double -> Double -> Path
ellipseP cx cy rx ry =
  let k = 0.5522847498; kx = k * rx; ky = k * ry in
  [ MoveTo (Pt (cx + rx) cy), CubicTo (Pt (cx + rx) (cy + ky)) (Pt (cx + kx) (cy + ry)) (Pt cx (cy + ry))
  , CubicTo (Pt (cx - kx) (cy + ry)) (Pt (cx - rx) (cy + ky)) (Pt (cx - rx) cy)
  , CubicTo (Pt (cx - rx) (cy - ky)) (Pt (cx - kx) (cy - ry)) (Pt cx (cy - ry))
  , CubicTo (Pt (cx + kx) (cy - ry)) (Pt (cx + rx) (cy - ky)) (Pt (cx + rx) cy), ClosePath ]

rectP :: Double -> Double -> Double -> Double -> Path
rectP = rectPath

star :: Double -> Double -> Double -> Path
star cx cy r = case [ Pt (cx + r * sin a) (cy - r * cos a) | k <- [0, 2, 4, 1, 3 :: Int], let a = fromIntegral k * 2 * pi / 5 ] of
  (q : qs) -> MoveTo q : map LineTo qs ++ [ClosePath]
  [] -> []

colorS :: Color -> String
colorS (Color r g b _) = "rgb(" ++ intercalate "," [ show (round (v * 255) :: Int) | v <- [r, g, b] ] ++ ")"

-- Colours exactly representable in 8 bits, so the SVG says what the scene says.
c8 :: Int -> Int -> Int -> Color
c8 r g b = Color (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) 1

red, green, blue, yellow, navy, orange, teal :: Color
red = c8 220 30 40
green = c8 40 170 60
blue = c8 30 80 210
yellow = c8 240 200 30
navy = c8 20 30 90
orange = c8 250 140 20
teal = c8 20 160 160

withA :: Double -> Color -> Color
withA a (Color r g b _) = Color r g b a

fillS :: Color -> String
fillS c@(Color _ _ _ a) = "fill=\"" ++ colorS c ++ "\"" ++ (if a < 1 then " fill-opacity=\"" ++ num a ++ "\"" else "")

pathEl :: String -> Path -> String
pathEl attrs p = "<path d=\"" ++ pathD p ++ "\" " ++ attrs ++ "/>"

fillC :: Color -> Path -> Cmd
fillC c = CFill identityA NonZero (PSolid c)

------------------------------------------------------------------------------
-- Cases

data Case = Case
  { cName  :: String
  , cW, cH :: Int
  , cSvg   :: String    -- ^ Body (inside <svg>), defs included.
  , cCmds  :: [Cmd]
  }

svgDoc :: Case -> String
svgDoc c = concat
  [ "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\""
  , show (cW c), "\" height=\"", show (cH c), "\" viewBox=\"0 0 ", show (cW c), " ", show (cH c), "\">\n"
  , cSvg c, "\n</svg>\n" ]

sq :: String -> String -> [Cmd] -> Case
sq n s cmds = Case n 120 120 s cmds

stroke :: StrokeStyle -> Color -> Path -> (String, Cmd)
stroke ss c p =
  let capS = case ssCap ss of CapButt -> "butt"; CapRound -> "round"; CapSquare -> "square"
      joinS = case ssJoin ss of JoinMiter -> "miter"; JoinRound -> "round"; JoinBevel -> "bevel"
      dash = if null (ssDash ss) then "" else " stroke-dasharray=\"" ++ unwords (map num (ssDash ss)) ++ "\" stroke-dashoffset=\"" ++ num (ssDashPhase ss) ++ "\""
      Color _ _ _ a = c
      op = if a < 1 then " stroke-opacity=\"" ++ num a ++ "\"" else ""
  in ( pathEl ("fill=\"none\" stroke=\"" ++ colorS c ++ "\" stroke-width=\"" ++ num (ssWidth ss) ++ "\" stroke-linecap=\"" ++ capS
               ++ "\" stroke-linejoin=\"" ++ joinS ++ "\" stroke-miterlimit=\"" ++ num (ssMiterLimit ss) ++ "\"" ++ dash ++ op) p
     , CStroke identityA ss (PSolid c) p )

svgStroke :: StrokeStyle
svgStroke = defaultStroke { ssMiterLimit = 4 }

zigzag :: Path
zigzag = [MoveTo (Pt 15 90), LineTo (Pt 40 25), LineTo (Pt 60 80), LineTo (Pt 80 30), LineTo (Pt 105 95)]

blends :: [(String, Blend)]
blends =
  [ ("normal", BlendNormal), ("multiply", BlendMultiply), ("screen", BlendScreen), ("overlay", BlendOverlay)
  , ("darken", BlendDarken), ("lighten", BlendLighten), ("color-dodge", BlendColorDodge), ("color-burn", BlendColorBurn)
  , ("hard-light", BlendHardLight), ("soft-light", BlendSoftLight), ("difference", BlendDifference)
  , ("exclusion", BlendExclusion), ("hue", BlendHue), ("saturation", BlendSaturation), ("color", BlendColor)
  , ("luminosity", BlendLuminosity) ]

linGrad :: [Stop]
linGrad = [Stop 0 red, Stop 0.5 yellow, Stop 1 blue]

stopsS :: [Stop] -> String
stopsS ss = concat [ "<stop offset=\"" ++ num o ++ "\" stop-color=\"" ++ colorS c ++ "\"" ++ (if a < 1 then " stop-opacity=\"" ++ num a ++ "\"" else "") ++ "/>"
                   | Stop o c@(Color _ _ _ a) <- ss ]

spreadS :: Spread -> String
spreadS s = case s of SpreadReflect -> "reflect"; SpreadRepeat -> "repeat"; _ -> "pad"

testImage :: Int -> Int -> Image
testImage w h = Image w h "test" (U.listArray (0, 4 * w * h - 1) (concat
  [ [ fromIntegral ((x * 255) `div` max 1 (w - 1)), fromIntegral ((y * 255) `div` max 1 (h - 1))
    , if (x + y) `mod` 2 == 0 then 230 else 40, if x == 0 && y == 0 then 128 else 255 ]
  | y <- [0 .. h - 1], x <- [0 .. w - 1] ]))

imageEl :: Image -> Double -> Double -> Double -> Double -> String -> String
imageEl img x y w h extra =
  "<image x=\"" ++ num x ++ "\" y=\"" ++ num y ++ "\" width=\"" ++ num w ++ "\" height=\"" ++ num h
  ++ "\" preserveAspectRatio=\"none\" " ++ extra ++ " xlink:href=\"data:image/png;base64," ++ base64 (imagePng img) ++ "\"/>"

filterCase :: String -> String -> [FilterPrim] -> Case
filterCase n primsSvg prims =
  sq n ("<filter id=\"f\" filterUnits=\"userSpaceOnUse\" x=\"10\" y=\"10\" width=\"100\" height=\"100\">" ++ primsSvg ++ "</filter>"
        ++ "<g filter=\"url(#f)\">" ++ pathEl (fillS blue) (rectP 35 35 40 40) ++ pathEl (fillS (withA 0.8 orange)) (circleP 70 70 18) ++ "</g>")
     [ CGroup defaultGroup { goIsolated = True, goFilter = Just (Filter (Rect 10 10 110 110) identityA prims) }
         [fillC blue (rectP 35 35 40 40), fillC (withA 0.8 orange) (circleP 70 70 18)] ]

prim :: Bool -> [FilterIn] -> Int -> FilterOp -> FilterPrim
prim lin ins res op = FilterPrim op ins Nothing lin res

cases :: [Case]
cases =
  -- Fills
  [ sq "fill-rect-frac" (pathEl (fillS red) (rectP 10.3 20.7 50.4 30.2)) [fillC red (rectP 10.3 20.7 50.4 30.2)]
  , sq "fill-circle" (pathEl (fillS blue) (circleP 60 60 45)) [fillC blue (circleP 60 60 45)]
  , sq "fill-star-nonzero" (pathEl (fillS green) (star 60 62 52)) [fillC green (star 60 62 52)]
  , sq "fill-star-evenodd" (pathEl (fillS green ++ " fill-rule=\"evenodd\"") (star 60 62 52))
      [CFill identityA EvenOdd (PSolid green) (star 60 62 52)]
  , let p = rectP 10 10 60 60 ++ rectP 40 40 70 70 in
    sq "fill-overlap-same" (pathEl (fillS navy) p) [fillC navy p]
  , let p = rectP 10 10 100 100 ++ reverseRect 30 30 60 60 in
    sq "fill-overlap-hole" (pathEl (fillS navy) p) [fillC navy p]
  , let p = [MoveTo (Pt 10 100), QuadTo (Pt 60 (-20)) (Pt 110 100), CubicTo (Pt 80 60) (Pt 40 130) (Pt 10 100), ClosePath] in
    sq "fill-curves" (pathEl (fillS orange) p) [fillC orange p]
  , let p = [MoveTo (Pt 5 5), LineTo (Pt 115 60), LineTo (Pt 5 63), ClosePath] in
    sq "fill-sliver" (pathEl (fillS navy) p) [fillC navy p]
  , let m = composeA (translateA 60 60) (composeA (rotateA (pi / 6)) (scaleA 1.5 0.7)) in
    sq "fill-transform" ("<g transform=\"translate(60 60) rotate(30) scale(1.5 0.7)\">" ++ pathEl (fillS teal) (ellipseP 0 0 35 25) ++ "</g>")
      [CTransform m [fillC teal (ellipseP 0 0 35 25)]]
  , sq "fill-opacity-overlap" (pathEl (fillS (withA 0.5 red)) (circleP 45 60 35) ++ pathEl (fillS (withA 0.5 blue)) (circleP 75 60 35))
      [fillC (withA 0.5 red) (circleP 45 60 35), fillC (withA 0.5 blue) (circleP 75 60 35)]
  ] ++
  -- Strokes
  [ let (s, c) = stroke svgStroke { ssWidth = 12, ssJoin = j } navy zigzag in sq ("stroke-join-" ++ n) s [c]
  | (n, j) <- [("miter", JoinMiter), ("round", JoinRound), ("bevel", JoinBevel)] ] ++
  [ let (s, c) = stroke svgStroke { ssWidth = 14, ssCap = cp } navy [MoveTo (Pt 25 30), LineTo (Pt 95 30), MoveTo (Pt 25 80), LineTo (Pt 95 95)]
    in sq ("stroke-cap-" ++ n) s [c]
  | (n, cp) <- [("butt", CapButt), ("round", CapRound), ("square", CapSquare)] ] ++
  [ let (s, c) = stroke svgStroke { ssWidth = 8, ssMiterLimit = 10 } red [MoveTo (Pt 10 100), LineTo (Pt 60 20), LineTo (Pt 75 100)] in sq "stroke-miter-long" s [c]
  , let (s, c) = stroke svgStroke { ssWidth = 8, ssMiterLimit = 2 } red [MoveTo (Pt 10 100), LineTo (Pt 60 20), LineTo (Pt 75 100)] in sq "stroke-miter-limited" s [c]
  , let (s, c) = stroke svgStroke { ssWidth = 4, ssDash = [10, 5], ssDashPhase = 3 } blue
                   [MoveTo (Pt 10 60), CubicTo (Pt 30 (-10)) (Pt 90 130) (Pt 110 60)] in sq "stroke-dash-curve" s [c]
  , let (s, c) = stroke svgStroke { ssWidth = 5, ssDash = [17, 6], ssDashPhase = 0, ssJoin = JoinMiter } green (rectP 15 15 90 90) in sq "stroke-dash-closed" s [c]
  , let (s, c) = stroke svgStroke { ssWidth = 8, ssDash = [0, 14], ssCap = CapRound } navy [MoveTo (Pt 10 60), LineTo (Pt 110 60)] in sq "stroke-dash-dots" s [c]
  , let (s, c) = stroke svgStroke { ssWidth = 20, ssJoin = JoinRound, ssCap = CapRound } (withA 0.5 teal)
                   [MoveTo (Pt 20 20), LineTo (Pt 100 100), LineTo (Pt 100 20), LineTo (Pt 20 100)] in sq "stroke-self-overlap" s [c]
  , let (s, c) = stroke svgStroke { ssWidth = 18 } orange [MoveTo (Pt 10 100), CubicTo (Pt 10 0) (Pt 110 0) (Pt 60 70)] in sq "stroke-wide-curve" s [c]
  , sq "stroke-nonuniform" ("<g transform=\"scale(3 1)\">" ++ fst (stroke svgStroke { ssWidth = 3 } navy (circleP 20 60 15)) ++ "</g>")
      [CTransform (scaleA 3 1) [snd (stroke svgStroke { ssWidth = 3 } navy (circleP 20 60 15))]]
  , let (s, c) = stroke svgStroke { ssWidth = 0.5 } navy [MoveTo (Pt 10 10.3), LineTo (Pt 110 80), MoveTo (Pt 10 50), LineTo (Pt 110 50)] in sq "stroke-thin" s [c]
  ] ++
  -- Gradients
  [ sq ("grad-linear-" ++ spreadS sp)
      ("<defs><linearGradient id=\"g\" gradientUnits=\"userSpaceOnUse\" x1=\"40\" y1=\"30\" x2=\"80\" y2=\"60\" spreadMethod=\""
       ++ spreadS sp ++ "\">" ++ stopsS linGrad ++ "</linearGradient></defs>" ++ pathEl "fill=\"url(#g)\"" (rectP 5 5 110 110))
      [CFill identityA NonZero (PLinear identityA (Pt 40 30) (Pt 80 60) linGrad sp) (rectP 5 5 110 110)]
  | sp <- [SpreadPad, SpreadReflect, SpreadRepeat] ] ++
  [ sq "grad-radial"
      ("<defs><radialGradient id=\"g\" gradientUnits=\"userSpaceOnUse\" cx=\"60\" cy=\"60\" r=\"50\">" ++ stopsS linGrad ++ "</radialGradient></defs>"
       ++ pathEl "fill=\"url(#g)\"" (rectP 0 0 120 120))
      [CFill identityA NonZero (PRadial identityA (Pt 60 60) 0 (Pt 60 60) 50 linGrad SpreadPad) (rectP 0 0 120 120)]
  , sq "grad-radial-focal"
      ("<defs><radialGradient id=\"g\" gradientUnits=\"userSpaceOnUse\" cx=\"60\" cy=\"60\" r=\"50\" fx=\"40\" fy=\"45\">" ++ stopsS linGrad ++ "</radialGradient></defs>"
       ++ pathEl "fill=\"url(#g)\"" (rectP 0 0 120 120))
      [CFill identityA NonZero (PRadial identityA (Pt 40 45) 0 (Pt 60 60) 50 linGrad SpreadPad) (rectP 0 0 120 120)]
  , sq "grad-radial-reflect"
      ("<defs><radialGradient id=\"g\" gradientUnits=\"userSpaceOnUse\" cx=\"60\" cy=\"60\" r=\"20\" spreadMethod=\"reflect\">" ++ stopsS linGrad ++ "</radialGradient></defs>"
       ++ pathEl "fill=\"url(#g)\"" (rectP 0 0 120 120))
      [CFill identityA NonZero (PRadial identityA (Pt 60 60) 0 (Pt 60 60) 20 linGrad SpreadReflect) (rectP 0 0 120 120)]
  , sq "grad-transform-opacity"
      ("<defs><linearGradient id=\"g\" gradientUnits=\"userSpaceOnUse\" x1=\"0\" y1=\"0\" x2=\"60\" y2=\"0\" gradientTransform=\"translate(60 60) rotate(40) translate(-30 0)\">"
       ++ stopsS [Stop 0 (withA 0.2 red), Stop 1 blue] ++ "</linearGradient></defs>"
       ++ pathEl (fillS yellow) (rectP 0 0 60 120) ++ pathEl "fill=\"url(#g)\"" (circleP 60 60 50))
      [ fillC yellow (rectP 0 0 60 120)
      , CFill identityA NonZero (PLinear (composeA (translateA 60 60) (composeA (rotateA (40 * pi / 180)) (translateA (-30) 0)))
                                   (Pt 0 0) (Pt 60 0) [Stop 0 (withA 0.2 red), Stop 1 blue] SpreadPad) (circleP 60 60 50) ]
  ] ++
  -- Groups and blending
  [ sq "group-opacity"
      ("<g opacity=\"0.5\">" ++ pathEl (fillS red) (circleP 45 60 35) ++ pathEl (fillS blue) (circleP 75 60 35) ++ "</g>")
      [CGroup defaultGroup { goOpacity = 0.5, goIsolated = True } [fillC red (circleP 45 60 35), fillC blue (circleP 75 60 35)]]
  , sq "group-opacity-nested"
      ("<g opacity=\"0.7\">" ++ pathEl (fillS red) (circleP 45 60 35) ++ "<g opacity=\"0.5\">" ++ pathEl (fillS blue) (circleP 75 60 35)
       ++ pathEl (fillS green) (rectP 50 20 30 80) ++ "</g></g>")
      [CGroup defaultGroup { goOpacity = 0.7, goIsolated = True }
         [ fillC red (circleP 45 60 35)
         , CGroup defaultGroup { goOpacity = 0.5, goIsolated = True } [fillC blue (circleP 75 60 35), fillC green (rectP 50 20 30 80)] ]]
  ] ++
  [ sq ("blend-" ++ n)
      ("<defs><linearGradient id=\"g\" gradientUnits=\"userSpaceOnUse\" x1=\"0\" y1=\"0\" x2=\"120\" y2=\"0\">"
       ++ stopsS [Stop 0 (c8 255 255 255), Stop 0.33 teal, Stop 0.66 orange, Stop 1 (c8 0 0 0)] ++ "</linearGradient></defs>"
       ++ pathEl "fill=\"url(#g)\"" (rectP 0 10 120 100)
       ++ "<g style=\"mix-blend-mode:" ++ n ++ "\">" ++ pathEl (fillS (c8 200 60 160)) (circleP 40 60 32) ++ pathEl (fillS (c8 80 200 90)) (circleP 80 60 32) ++ "</g>")
      [ CFill identityA NonZero (PLinear identityA (Pt 0 0) (Pt 120 0) [Stop 0 (c8 255 255 255), Stop 0.33 teal, Stop 0.66 orange, Stop 1 (c8 0 0 0)] SpreadPad) (rectP 0 10 120 100)
      , CGroup defaultGroup { goBlend = b, goIsolated = True } [fillC (c8 200 60 160) (circleP 40 60 32), fillC (c8 80 200 90) (circleP 80 60 32)] ]
  | (n, b) <- blends ] ++
  -- Masks
  [ sq "mask-luminance"
      ("<defs><linearGradient id=\"g\" gradientUnits=\"userSpaceOnUse\" x1=\"0\" y1=\"0\" x2=\"120\" y2=\"0\">" ++ stopsS [Stop 0 (c8 255 255 255), Stop 1 (c8 0 0 0)]
       ++ "</linearGradient><mask id=\"m\" maskUnits=\"userSpaceOnUse\" x=\"0\" y=\"0\" width=\"120\" height=\"120\">"
       ++ pathEl "fill=\"url(#g)\"" (rectP 0 0 120 120) ++ pathEl (fillS (c8 255 0 0)) (circleP 60 90 20) ++ "</mask></defs>"
       ++ "<g mask=\"url(#m)\">" ++ pathEl (fillS blue) (rectP 10 10 100 100) ++ "</g>")
      [CGroup defaultGroup { goIsolated = True, goMask = Just (SoftMask (-1) MaskLuminance transparent
          [ CFill identityA NonZero (PLinear identityA (Pt 0 0) (Pt 120 0) [Stop 0 (c8 255 255 255), Stop 1 (c8 0 0 0)] SpreadPad) (rectP 0 0 120 120)
          , fillC (c8 255 0 0) (circleP 60 90 20) ] Nothing) }
         [fillC blue (rectP 10 10 100 100)]]
  , sq "mask-alpha"
      ("<defs><mask id=\"m\" mask-type=\"alpha\" maskUnits=\"userSpaceOnUse\" x=\"0\" y=\"0\" width=\"120\" height=\"120\">"
       ++ pathEl (fillS (withA 0.4 (c8 0 0 0))) (circleP 50 50 35) ++ pathEl (fillS (c8 0 0 0)) (circleP 75 75 30) ++ "</mask></defs>"
       ++ "<g mask=\"url(#m)\">" ++ pathEl (fillS red) (rectP 10 10 100 100) ++ "</g>")
      [CGroup defaultGroup { goIsolated = True, goMask = Just (SoftMask (-1) MaskAlpha transparent
          [ fillC (withA 0.4 (c8 0 0 0)) (circleP 50 50 35), fillC (c8 0 0 0) (circleP 75 75 30) ] Nothing) }
         [fillC red (rectP 10 10 100 100)]]
  ] ++
  -- Clips
  [ sq "clip-circle"
      ("<defs><clipPath id=\"c\">" ++ pathEl "" (circleP 60 60 40) ++ "</clipPath></defs><g clip-path=\"url(#c)\">"
       ++ pathEl (fillS red) (rectP 0 0 60 120) ++ pathEl (fillS blue) (rectP 60 0 60 120) ++ "</g>")
      [CClip [ClipShape identityA NonZero (circleP 60 60 40)] [fillC red (rectP 0 0 60 120), fillC blue (rectP 60 0 60 120)]]
  , sq "clip-nested"
      ("<defs><clipPath id=\"c\">" ++ pathEl "" (circleP 60 60 45) ++ "</clipPath><clipPath id=\"d\">" ++ pathEl "" (rectP 20.5 30.25 70.3 55.5)
       ++ "</clipPath></defs><g clip-path=\"url(#c)\"><g clip-path=\"url(#d)\">" ++ pathEl (fillS green) (rectP 0 0 120 120) ++ "</g></g>")
      [CClip [ClipShape identityA NonZero (circleP 60 60 45)] [CClip [ClipShape identityA NonZero (rectP 20.5 30.25 70.3 55.5)] [fillC green (rectP 0 0 120 120)]]]
  , sq "clip-evenodd"
      ("<defs><clipPath id=\"c\">" ++ pathEl "clip-rule=\"evenodd\"" (star 60 62 55) ++ "</clipPath></defs><g clip-path=\"url(#c)\">"
       ++ pathEl (fillS orange) (rectP 0 0 120 120) ++ "</g>")
      [CClip [ClipShape identityA EvenOdd (star 60 62 55)] [fillC orange (rectP 0 0 120 120)]]
  , sq "clip-union"
      ("<defs><clipPath id=\"c\">" ++ pathEl "" (circleP 40 60 30) ++ pathEl "" (circleP 80 60 30) ++ "</clipPath></defs><g clip-path=\"url(#c)\" opacity=\"0.6\">"
       ++ pathEl (fillS navy) (rectP 0 0 120 120) ++ "</g>")
      [CGroup defaultGroup { goOpacity = 0.6, goIsolated = True }
         [CClip [ClipShape identityA NonZero (circleP 40 60 30), ClipShape identityA NonZero (circleP 80 60 30)] [fillC navy (rectP 0 0 120 120)]]]
  ] ++
  -- Patterns
  [ sq "pattern-dots"
      ("<defs><pattern id=\"p\" patternUnits=\"userSpaceOnUse\" width=\"20\" height=\"20\">" ++ pathEl (fillS blue) (circleP 10 10 6)
       ++ pathEl (fillS red) (rectP 0 0 4 20) ++ "</pattern></defs>" ++ pathEl "fill=\"url(#p)\"" (circleP 60 60 52))
      [CFill identityA NonZero (PTiling 1 identityA (Rect 0 0 20 20) 20 20 [fillC blue (circleP 10 10 6), fillC red (rectP 0 0 4 20)]) (circleP 60 60 52)]
  , sq "pattern-rotated"
      ("<defs><pattern id=\"p\" patternUnits=\"userSpaceOnUse\" width=\"16\" height=\"16\" patternTransform=\"rotate(30)\">" ++ pathEl (fillS green) (rectP 0 0 8 16)
       ++ "</pattern></defs>" ++ pathEl "fill=\"url(#p)\"" (rectP 5 5 110 110))
      [CFill identityA NonZero (PTiling 2 (rotateA (pi / 6)) (Rect 0 0 16 16) 16 16 [fillC green (rectP 0 0 8 16)]) (rectP 5 5 110 110)]
  -- The lattice origin is pattern-space (0,0) and the cell sits at its own
  -- x/y: a scaled, offset pattern (with a viewBox) must keep its phase.
  -- (Scale 0.4 keeps the period a whole number of pixels: resvg renders a
  -- tile at a rounded pixel size and repeats it 1:1, so at scale 0.5 its
  -- 12.5 px period becomes 13 px and drifts half a tile in four periods.
  -- It also sizes the tile from the matrix's row norms, not its column
  -- norms, so a rotation with a non-uniform scale drifts too; the rotated
  -- case therefore scales uniformly.)
  , sq "pattern-scaled-offset"
      ("<defs><pattern id=\"p\" patternUnits=\"userSpaceOnUse\" x=\"5\" y=\"5\" width=\"25\" height=\"25\" viewBox=\"0 0 10 10\" patternTransform=\"scale(0.4)\">"
       ++ pathEl (fillS orange) (circleP 5 5 4) ++ pathEl (fillS blue) (rectP 0 0 3 3) ++ "</pattern></defs>" ++ pathEl "fill=\"url(#p)\"" (rectP 5 5 110 110))
      [CFill identityA NonZero (PTiling 3 (scaleA 0.4 0.4) (Rect 5 5 30 30) 25 25
          [CTransform (Affine 2.5 0 0 2.5 5 5) [fillC orange (circleP 5 5 4), fillC blue (rectP 0 0 3 3)]]) (rectP 5 5 110 110)]
  , sq "pattern-scaled-offset-small"
      ("<defs><pattern id=\"p\" patternUnits=\"userSpaceOnUse\" x=\"5\" y=\"5\" width=\"25\" height=\"25\" viewBox=\"0 0 10 10\" patternTransform=\"scale(0.4)\">"
       ++ pathEl (fillS orange) (circleP 5 5 4) ++ pathEl (fillS blue) (rectP 0 0 3 3) ++ "</pattern></defs>" ++ pathEl "fill=\"url(#p)\"" (rectP 20 20 15 15))
      [CFill identityA NonZero (PTiling 3 (scaleA 0.4 0.4) (Rect 5 5 30 30) 25 25
          [CTransform (Affine 2.5 0 0 2.5 5 5) [fillC orange (circleP 5 5 4), fillC blue (rectP 0 0 3 3)]]) (rectP 20 20 15 15)]
  , sq "pattern-rotated-offset"
      ("<defs><pattern id=\"p\" patternUnits=\"userSpaceOnUse\" x=\"3\" y=\"7\" width=\"20\" height=\"10\" patternTransform=\"translate(4 2) rotate(20) scale(1.2)\">"
       ++ pathEl (fillS teal) (rectP 3 7 8 13) ++ pathEl (fillS red) (circleP 14 12 3) ++ "</pattern></defs>" ++ pathEl "fill=\"url(#p)\"" (rectP 5 5 110 110))
      [CFill identityA NonZero (PTiling 4 (composeA (translateA 4 2) (composeA (rotateA (20 * pi / 180)) (scaleA 1.2 1.2))) (Rect 3 7 23 17) 20 10
          [CTransform (translateA 3 7) [fillC teal (rectP 3 7 8 13), fillC red (circleP 14 12 3)]]) (rectP 5 5 110 110)]
  ] ++
  -- Images
  [ let img = testImage 8 6 in
    sq "image-magnify" (imageEl img 10 20 100 75 "") [CImage (Affine 100 0 0 75 10 20) img defaultImageOpts]
  , let img = testImage 8 6 in
    sq "image-pixelated" (imageEl img 10 20 100 75 "style=\"image-rendering:pixelated\"")
      [CImage (Affine 100 0 0 75 10 20) img defaultImageOpts { ioInterp = InterpNearest }]
  , let img = testImage 200 160 in
    sq "image-minify" (imageEl img 10 10 50 40 "") [CImage (Affine 50 0 0 40 10 10) img defaultImageOpts]
  , let img = testImage 30 20 in
    sq "image-rotated" ("<g transform=\"translate(60 60) rotate(25)\">" ++ imageEl img (-45) (-30) 90 60 "" ++ "</g>")
      [CTransform (composeA (translateA 60 60) (rotateA (25 * pi / 180))) [CImage (Affine 90 0 0 60 (-45) (-30)) img defaultImageOpts]]
  , let img = testImage 16 16 in
    sq "image-opacity" (pathEl (fillS yellow) (rectP 0 0 120 60) ++ "<g opacity=\"0.5\">" ++ imageEl img 20 20 80 80 "" ++ "</g>")
      [fillC yellow (rectP 0 0 120 60), CGroup defaultGroup { goOpacity = 0.5, goIsolated = True } [CImage (Affine 80 0 0 80 20 20) img defaultImageOpts]]
  ] ++
  -- Filters
  [ filterCase "filter-blur3" "<feGaussianBlur stdDeviation=\"3\" color-interpolation-filters=\"sRGB\"/>" [prim False [] 1 (FeBlur 3 3)]
  , filterCase "filter-blur1" "<feGaussianBlur stdDeviation=\"1\" color-interpolation-filters=\"sRGB\"/>" [prim False [] 1 (FeBlur 1 1)]
  , filterCase "filter-blur-linear" "<feGaussianBlur stdDeviation=\"5 2\"/>" [prim True [] 1 (FeBlur 5 2)]
  , filterCase "filter-offset-merge"
      ("<feOffset in=\"SourceAlpha\" dx=\"6\" dy=\"4\" result=\"o\"/><feGaussianBlur in=\"o\" stdDeviation=\"2\" result=\"b\" color-interpolation-filters=\"sRGB\"/>"
       ++ "<feMerge><feMergeNode in=\"b\"/><feMergeNode in=\"SourceGraphic\"/></feMerge>")
      [prim False [SourceAlpha] 1 (FeOffset 6 4), prim False [Result 1] 2 (FeBlur 2 2), prim True [Result 2, SourceGraphic] 3 FeMerge]
  , filterCase "filter-flood-in"
      "<feFlood flood-color=\"rgb(40,170,60)\" flood-opacity=\"0.7\" result=\"f\"/><feComposite in=\"f\" in2=\"SourceGraphic\" operator=\"in\"/>"
      [prim True [] 1 (FeFlood (withA 0.7 green)), prim True [Result 1, SourceGraphic] 2 (FeComposite CompIn)]
  , filterCase "filter-colormatrix"
      "<feColorMatrix type=\"matrix\" values=\"0 1 0 0 0  0 0 1 0 0  1 0 0 0 0  0 0 0 0.6 0\" color-interpolation-filters=\"sRGB\"/>"
      [prim False [] 1 (FeColorMatrix [0,1,0,0,0, 0,0,1,0,0, 1,0,0,0,0, 0,0,0,0.6,0])]
  , filterCase "filter-colormatrix-linear"
      "<feColorMatrix type=\"matrix\" values=\"0.5 0 0 0 0.2  0 0.5 0 0 0.1  0 0 0.5 0 0  0 0 0 1 0\"/>"
      [prim True [] 1 (FeColorMatrix [0.5,0,0,0,0.2, 0,0.5,0,0,0.1, 0,0,0.5,0,0, 0,0,0,1,0])]
  -- resvg 0.45 takes feDropShadow's flood colour through the wrong colour
  -- space (in either setting), so the reference is the spec's own
  -- definition of the primitive, spelled out.
  , filterCase "filter-dropshadow"
      ("<feGaussianBlur in=\"SourceAlpha\" stdDeviation=\"3\" color-interpolation-filters=\"sRGB\"/><feOffset dx=\"5\" dy=\"5\" result=\"o\"/>"
       ++ "<feFlood flood-color=\"rgb(20,30,90)\" flood-opacity=\"0.6\" color-interpolation-filters=\"sRGB\"/><feComposite in2=\"o\" operator=\"in\" color-interpolation-filters=\"sRGB\"/>"
       ++ "<feMerge color-interpolation-filters=\"sRGB\"><feMergeNode/><feMergeNode in=\"SourceGraphic\"/></feMerge>")
      [prim False [] 1 (FeDropShadow 3 3 5 5 (withA 0.6 navy))]
  , filterCase "filter-blend"
      "<feFlood flood-color=\"rgb(250,140,20)\" result=\"f\"/><feBlend in=\"SourceGraphic\" in2=\"f\" mode=\"multiply\" color-interpolation-filters=\"sRGB\"/>"
      [prim True [] 1 (FeFlood orange), prim False [SourceGraphic, Result 1] 2 (FeBlend BlendMultiply)]
  , filterCase "filter-arithmetic"
      ("<feFlood flood-color=\"rgb(240,200,30)\" result=\"f\" color-interpolation-filters=\"sRGB\"/>"
       ++ "<feComposite in=\"SourceGraphic\" in2=\"f\" operator=\"arithmetic\" k1=\"0.5\" k2=\"0.5\" k3=\"0.3\" k4=\"0.05\" color-interpolation-filters=\"sRGB\"/>")
      [prim False [] 1 (FeFlood yellow), prim False [SourceGraphic, Result 1] 2 (FeComposite (CompArithmetic 0.5 0.5 0.3 0.05))]
  -- Offsets go through the whole linear part of user -> device, rotation
  -- and flips included, as the spec and the browsers have it. resvg 0.45
  -- does not: it multiplies dx and dy by the matrix's axis lengths, so a
  -- rotated or flipped element's shadow falls on the unrotated side. The
  -- SVG half of these cases therefore states the spec's device offset in
  -- resvg's own terms (that offset divided by the scale), so resvg draws
  -- what the spec says; the scene keeps the real user-space offset (12, 4).
  ] ++
  [ sq ("filter-offset-" ++ n)
      ("<filter id=\"f\" filterUnits=\"userSpaceOnUse\" x=\"-60\" y=\"-60\" width=\"120\" height=\"120\" color-interpolation-filters=\"sRGB\">"
       ++ "<feOffset in=\"SourceAlpha\" dx=\"" ++ num rdx ++ "\" dy=\"" ++ num rdy ++ "\" result=\"o\"/><feMerge><feMergeNode in=\"o\"/><feMergeNode in=\"SourceGraphic\"/></feMerge></filter>"
       ++ "<g transform=\"" ++ t ++ "\"><g filter=\"url(#f)\">" ++ pathEl (fillS blue) (rectP (-20) (-15) 30 20) ++ "</g></g>")
      [CTransform m [CGroup defaultGroup { goIsolated = True, goFilter = Just (Filter (Rect (-60) (-60) 60 60) identityA
          [prim False [SourceAlpha] 1 (FeOffset 12 4), prim False [Result 1, SourceGraphic] 2 FeMerge]) }
          [fillC blue (rectP (-20) (-15) 30 20)]]]
  | (n, t, m, k) <- [ ("rotate90", "translate(60 60) rotate(90)", composeA (translateA 60 60) (rotateA (pi / 2)), 1)
                    , ("flipx", "translate(60 60) scale(-1 1)", composeA (translateA 60 60) (scaleA (-1) 1), 1)
                    , ("rot30-scale", "translate(60 60) rotate(30) scale(1.5)", composeA (translateA 60 60) (composeA (rotateA (pi / 6)) (scaleA 1.5 1.5)), 1.5) ]
  , let Pt ddx ddy = applyLinearA m (Pt 12 4); rdx = ddx / k; rdy = ddy / k ] ++
  -- (user offset (10, 3) under rotate(90) is device (-3, 10); see above)
  [ sq "filter-dropshadow-rotate90"
      ("<filter id=\"f\" filterUnits=\"userSpaceOnUse\" x=\"-60\" y=\"-60\" width=\"120\" height=\"120\" color-interpolation-filters=\"sRGB\">"
       ++ "<feGaussianBlur in=\"SourceAlpha\" stdDeviation=\"2\"/><feOffset dx=\"-3\" dy=\"10\" result=\"o\"/>"
       ++ "<feFlood flood-color=\"rgb(20,30,90)\" flood-opacity=\"0.6\"/><feComposite in2=\"o\" operator=\"in\"/>"
       ++ "<feMerge><feMergeNode/><feMergeNode in=\"SourceGraphic\"/></feMerge></filter>"
       ++ "<g transform=\"translate(60 60) rotate(90)\"><g filter=\"url(#f)\">" ++ pathEl (fillS orange) (rectP (-20) (-15) 30 20) ++ "</g></g>")
      [CTransform (composeA (translateA 60 60) (rotateA (pi / 2))) [CGroup defaultGroup { goIsolated = True, goFilter = Just (Filter (Rect (-60) (-60) 60 60) identityA
          [prim False [] 1 (FeDropShadow 2 2 10 3 (withA 0.6 navy))]) }
          [fillC orange (rectP (-20) (-15) 30 20)]]]
  -- A filter on nothing still runs: a flood needs no source graphic.
  , sq "filter-empty-source"
      ("<filter id=\"f\" filterUnits=\"userSpaceOnUse\" x=\"20\" y=\"30\" width=\"70\" height=\"50\" color-interpolation-filters=\"sRGB\">"
       ++ "<feFlood flood-color=\"rgb(250,140,20)\" flood-opacity=\"0.7\" result=\"f\"/><feOffset dx=\"10\" dy=\"10\"/><feMerge><feMergeNode in=\"f\"/><feMergeNode/></feMerge></filter>"
       ++ "<rect x=\"40\" y=\"40\" width=\"10\" height=\"10\" fill=\"none\" filter=\"url(#f)\"/>")
      [CGroup defaultGroup { goIsolated = True, goFilter = Just (Filter (Rect 20 30 90 80) identityA
          [prim False [] 1 (FeFlood (withA 0.7 orange)), prim False [Result 1] 2 (FeOffset 10 10), prim False [Result 1, Result 2] 3 FeMerge]) } []]
  , filterCase "filter-subregion"
      "<feFlood x=\"30\" y=\"30\" width=\"40\" height=\"50\" flood-color=\"rgb(20,160,160)\" color-interpolation-filters=\"sRGB\"/>"
      [FilterPrim (FeFlood teal) [] (Just (Rect 30 30 70 80)) False 1]
  ]
  where
    reverseRect x y w h = [MoveTo (Pt x y), LineTo (Pt x (y + h)), LineTo (Pt (x + w) (y + h)), LineTo (Pt (x + w) y), ClosePath]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["bench"] -> bench
    ["safe"] -> safe
    _ -> oracle args

-- | Exercise the core under bounds-checked array access (build with
-- -DRASTER_SAFE, as @rastercheck.sh safe@ does): every oracle case, shapes
-- crossing the left edge at many sub-pixel offsets (where an edge can step
-- just below x = 0), and seeded random scenes with degenerate geometry. An
-- out-of-range index is an exception here, reported with the case.
safe :: IO ()
safe = do
  let render1 w h cmds = renderScene (defaultRenderOpts w h identityA) { roBudget = 3000000 } (Scene (Rect 0 0 (fromIntegral w) (fromIntegral h)) cmds [])
      force (img, st) = evaluate (sum (map fromIntegral (U.elems (imgPix img))) + rsPixels st :: Int)
      runCase name w h cmds = do
        r <- try (force (render1 w h cmds)) :: IO (Either SomeException Int)
        case r of
          Left e -> putStrLn ("FAIL " ++ name ++ ": " ++ show e) >> return False
          Right _ -> return True
      lefts = [ ("left-edge " ++ show i, 12, 12,
                 [ CFill identityA rule (PSolid black)
                     [MoveTo (Pt (-3 - dx) (0.1 + dy)), LineTo (Pt (7 + dx) (0.1 + dy)), LineTo (Pt (0 - 3 * dx) (9.7 - dy)), ClosePath]
                 , CStroke identityA defaultStroke { ssWidth = 1.3 + dx } (PSolid black) [MoveTo (Pt (-0.5 + dx) dy), LineTo (Pt 9 (11 - dy))] ])
              | i <- [0 .. 599 :: Int], let dx = fromIntegral (i `mod` 37) * 0.0271, let dy = fromIntegral (i `mod` 23) * 0.0437
              , let rule = if even i then NonZero else EvenOdd ]
      randoms = [ ("random " ++ show k, 24, 20, randomScene k) | k <- [1 .. 400 :: Int] ]
      -- translucent runs of cached glyphs at every quarter phase (they go
      -- through a run mask measured from the exact outlines)
      glyphRuns = [ ("glyph phase " ++ show i ++ "," ++ show j, 14, 12,
                     [ CGlyphs identityA (PSolid (Color 0 0 0 0.5))
                         [ Glyph (translateA (1 + fromIntegral i / 16 + fromIntegral n * 4.03) (2 + fromIntegral j / 16)) 5
                                 (rectPath 0.95 0.97 2.06 2.03 ++ circleP 2 5 1.4)
                         | n <- [0 .. 2 :: Int] ] ])
                  | i <- [0 .. 15 :: Int], j <- [0 .. 15 :: Int] ]
      -- large images minified through the streaming prescaler, at awkward
      -- factors, rotated and not
      bigImgs = [ ("big image " ++ show k, 31, 23,
                   [ CImage (composeA (translateA 15 11) (composeA (rotateA (fromIntegral k * 0.3)) (Affine (17 + fromIntegral k) 0 0 13 (-9) (-6))))
                            (testImage (300 + 7 * k) (211 + 3 * k)) defaultImageOpts
                   , CStencil (Affine 29 0 0 21 1 1) (Mask (97 + k) 61 (U.listArray (0, (97 + k) * 61 - 1) (cycle [0, 255, 128]))) (PSolid red) InterpSmooth ])
                | k <- [0 .. 5 :: Int] ]
  oks <- mapM (\c -> runCase (cName c) (cW c) (cH c) (cCmds c)) cases
  oks2 <- mapM (\(n, w, h, c) -> runCase n w h c) (lefts ++ randoms ++ glyphRuns ++ bigImgs)
  let bad = length (filter not (oks ++ oks2))
  putStrLn (show (length oks + length oks2) ++ " renders under bounds-checked access, " ++ show bad ++ " failed")
  when (bad > 0) exitFailure

-- A small seeded scene generator: overlapping fills of both rules,
-- strokes with dashes and caps, clips, groups, at awkward coordinates.
randomScene :: Int -> [Cmd]
randomScene seed = take 8 (go (fromIntegral seed * 2654435761 + 12345 :: Integer))
  where
    nxt x = (x * 6364136223846793005 + 1442695040888963407) `mod` (2 ^ (63 :: Int))
    val x = fromIntegral (x `div` 1024 `mod` 100000) / 100000 :: Double
    vals x = let x1 = nxt x; x2 = nxt x1; x3 = nxt x2; x4 = nxt x3; x5 = nxt x4; x6 = nxt x5 in ([val x1, val x2, val x3, val x4, val x5, val x6], x6)
    go x = let (v, x') = vals x
               [a, b, c, d, e, f] = map (\t -> t * 30 - 3) v
               poly = [MoveTo (Pt a b), LineTo (Pt c d), CubicTo (Pt e f) (Pt b a) (Pt d c), LineTo (Pt f e), ClosePath]
               k = x' `mod` 5
               cmd = case k of
                 0 -> CFill identityA NonZero (PSolid (Color 0.2 0.3 0.9 0.7)) (poly ++ poly)
                 1 -> CFill identityA EvenOdd (PSolid black) (poly ++ rectPath c d (e - c) (f - d))
                 2 -> CStroke identityA defaultStroke { ssWidth = abs (e - a) / 8, ssDash = [abs b / 10, 0.7], ssCap = CapRound, ssJoin = JoinRound } (PSolid black) poly
                 3 -> CClip [ClipShape identityA EvenOdd poly] [CFill identityA NonZero (PSolid white) (rectPath 0 0 24 20)]
                 _ -> CGroup defaultGroup { goIsolated = True, goOpacity = 0.6, goKnockout = odd x' }
                        [CFill identityA NonZero (PSolid black) poly, CStroke identityA defaultStroke (PSolid white) poly]
           in cmd : go (nxt x')

-- | The plan's performance targets, timed at -O2 (min over fresh renders).
bench :: IO ()
bench = do
  let glyph = circleP 4 5 3.5 ++ reverseCircle 4 5 2.2 ++ rectP 6.6 1 1.2 8
              ++ [MoveTo (Pt 1 9), QuadTo (Pt 4 11) (Pt 7 9), LineTo (Pt 7 9.8), QuadTo (Pt 4 11.8) (Pt 1 9.8), ClosePath]
      reverseCircle cx cy r = let k = 0.5522847498 * r in
        [ MoveTo (Pt (cx + r) cy), CubicTo (Pt (cx + r) (cy - k)) (Pt (cx + k) (cy - r)) (Pt cx (cy - r))
        , CubicTo (Pt (cx - k) (cy - r)) (Pt (cx - r) (cy - k)) (Pt (cx - r) cy)
        , CubicTo (Pt (cx - r) (cy + k)) (Pt (cx - k) (cy + r)) (Pt cx (cy + r))
        , CubicTo (Pt (cx + k) (cy + r)) (Pt (cx + r) (cy + k)) (Pt (cx + r) cy), ClosePath ]
      text = Scene (Rect 0 0 1000 1000)
        ( fillC white (rectP 0 0 1000 1000) :
          [ CGlyphs identityA (PSolid black) [ Glyph (translateA (fromIntegral c * 10 + 0.3) (fromIntegral r * 10 + 0.17)) 1 glyph | c <- [0 .. 99 :: Int] ]
          | r <- [0 .. 99 :: Int] ]) []
      glyphO = circleP 3.6 5 3.2 ++ reverseCircle 3.6 5 2 ++ rectP 7.4 1 1.2 8
      textO = Scene (Rect 0 0 1000 1000)
        ( fillC white (rectP 0 0 1000 1000) :
          [ CGlyphs identityA (PSolid black) [ Glyph (translateA (fromIntegral c * 10 + 0.3) (fromIntegral r * 10 + 0.17)) 2 glyphO | c <- [0 .. 99 :: Int] ]
          | r <- [0 .. 99 :: Int] ]) []
      uncached sc = sc { scCmds = [ case c of
                                      CGlyphs m p gs -> CGlyphs m p [ Glyph a (-1) q | Glyph a _ q <- gs ]
                                      _ -> c | c <- scCmds sc ] }
      icon = Scene (Rect 0 0 64 64)
        [ CFill identityA NonZero (PLinear identityA (Pt 0 0) (Pt 64 64) [Stop 0 red, Stop 1 blue] SpreadPad) (circleP 32 32 28)
        , CStroke identityA defaultStroke { ssWidth = 3, ssJoin = JoinRound } (PSolid black) (circleP 32 32 28)
        , CFill identityA EvenOdd (PSolid (withA 0.8 white)) (rectP 16 16 32 32 ++ rectP 24 24 16 16)
        , CGroup defaultGroup { goOpacity = 0.5, goIsolated = True } [fillC black (circleP 20 20 8), fillC black (circleP 26 26 8)] ] []
      photo = testImage 300 200
      doc = Scene (Rect 0 0 1200 1700)
        ( fillC white (rectP 0 0 1200 1700) :
          [ CGlyphs (translateA 100 (100 + fromIntegral r * 14)) (PSolid black) [ Glyph (translateA (fromIntegral c * 8) 0) 1 glyph | c <- [0 .. 119 :: Int] ]
          | r <- [0 .. 99 :: Int] ] ++
          [ CStroke identityA defaultStroke { ssWidth = 0.5 } (PSolid black) [MoveTo (Pt 100 (fromIntegral y)), LineTo (Pt 1100 (fromIntegral y))] | y <- [100, 120 .. 1600 :: Int] ] ++
          [ CImage (Affine 300 0 0 200 (100 + fromIntegral k * 310) 1450) photo defaultImageOpts | k <- [0 .. 2 :: Int] ] ++
          [ CFill identityA NonZero (PRadial identityA (Pt 600 800) 0 (Pt 600 800) 300 [Stop 0 (withA 0.3 yellow), Stop 1 (withA 0.3 teal)] SpreadPad) (circleP 600 800 300) ]) []
      run name reps w h sc = do
        ts <- mapM (\i -> do
                t0 <- getMonotonicTime
                let (img, _) = renderScene (defaultRenderOpts w h identityA) { roBudget = defaultBudget + i } sc
                _ <- evaluate (sum (map fromIntegral (U.elems (imgPix img))) :: Int)
                t1 <- getMonotonicTime
                return (t1 - t0)) [0 .. reps]
        putStrLn (name ++ ": " ++ showFFloat (Just 2) (minimum (drop 1 ts) * 1000) " ms")
  run "1 MP, 10 000 glyph fills (overlapping contours)" 5 1000 1000 text
  run "1 MP, 10 000 glyph fills (an o and a stem, no overlap)" 5 1000 1000 textO
  run "  the same, uncached (key -1)" 5 1000 1000 (uncached textO)
  run "  overlapping, uncached (key -1)" 5 1000 1000 (uncached text)
  run "64x64 icon" 50 64 64 icon
  run "2 MP page: 12 000 glyphs, 76 rules, 3 images, a gradient" 5 1200 1700 doc
  -- a page whose cover photo is far larger than the output (the streaming
  -- prescaler: memory independent of the source, see rsPeakBytes)
  let photo36 = Image 7200 5000 "36 MP" (runSTUArray (do
        a <- newArray (0, 4 * 7200 * 5000 - 1) 255
        forM_ [0 .. 5000 - 1] $ \y -> forM_ [0 .. 7200 - 1] $ \x -> do
          let i = 4 * (y * 7200 + x)
          writeArray a i (fromIntegral (x `mod` 256))
          writeArray a (i + 1) (fromIntegral (y `mod` 256))
          writeArray a (i + 2) (fromIntegral ((x + y) `mod` 256))
        return a))
      coverPage = Scene (Rect 0 0 1000 1400)
        [ fillC white (rectP 0 0 1000 1400), CImage (Affine 900 0 0 625 50 400) photo36 defaultImageOpts ] []
  _ <- evaluate (U.bounds (imgPix photo36))
  run "1000x1400 page with a 36 MP photo" 5 1000 1400 coverPage
  let (_, cst) = renderScene (defaultRenderOpts 1000 1400 identityA) coverPage
  putStrLn ("  peak charged memory " ++ show (rsPeakBytes cst `div` 1000) ++ " kB (canvas " ++ show (1000 * 1400 * 16 `div` 1000)
            ++ " kB), images dropped " ++ show (rsImagesDropped cst))

oracle :: [String] -> IO ()
oracle args = do
  let dir = case args of (d : _) -> d; [] -> "rastercheck-out"
  createDirectoryIfMissing True dir
  mapM_ (\c -> do
          writeFile (dir </> cName c ++ ".svg") (svgDoc c)
          let opts = defaultRenderOpts (cW c) (cH c) identityA
              (img, _) = renderScene opts (Scene (Rect 0 0 (fromIntegral (cW c)) (fromIntegral (cH c))) (cCmds c) [])
          BS.writeFile (dir </> cName c ++ ".out.png") (imagePng img)) cases
  putStrLn (show (length cases) ++ " cases written to " ++ dir)

