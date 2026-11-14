-- | Tests for the vector stack (plan 0034): Raster.
--
-- Spec.hs calls 'rasterSpec' with its own @check@, so a failure here is
-- counted and reported exactly like every other test. Fixtures are built
-- in code or checked in as bytes: the suite never shells out.
--
-- Expected values are computed by hand (areas, formulas) or by an
-- independent restatement here (the blend formulas, a brute-force radial
-- solver), never by calling the code under test a second way. The suite
-- runs at -O0, so every canvas is small.
module RasterSpec (rasterSpec) where

import Control.Exception (SomeException, evaluate, try)
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Bits (shiftR, xor, (.&.))
import Data.List (foldl')
import Data.Word (Word8, Word64)

import Cmedit.Geom
import Cmedit.Image (Image(..))
import Cmedit.Raster
import Cmedit.Scene

------------------------------------------------------------------------------
-- Helpers

render :: Int -> Int -> [Cmd] -> (Image, RenderStats)
render w h cmds =
  renderScene (defaultRenderOpts w h identityA) (Scene (Rect 0 0 (fromIntegral w) (fromIntegral h)) cmds [])

renderBg :: Color -> Int -> Int -> [Cmd] -> Image
renderBg bg w h cmds =
  fst (renderScene (defaultRenderOpts w h identityA) { roBackground = bg }
                   (Scene (Rect 0 0 (fromIntegral w) (fromIntegral h)) cmds []))

img0 :: Int -> Int -> [Cmd] -> Image
img0 w h = fst . render w h

px :: Image -> Int -> Int -> (Int, Int, Int, Int)
px img x y =
  let i = 4 * (y * imgW img + x)
      g k = fromIntegral (imgPix img ! (i + k))
  in (g 0, g 1, g 2, g 3)

alphaAt :: Image -> Int -> Int -> Int
alphaAt img x y = let (_, _, _, a) = px img x y in a

-- Total coverage in pixels (alpha summed over the image, in [0,1] units).
ink :: Image -> Double
ink img = sum [ fromIntegral (imgPix img ! (4 * i + 3)) / 255 | i <- [0 .. imgW img * imgH img - 1] ]

near :: Double -> Double -> Double -> Bool
near tol a b = abs (a - b) <= tol

within :: Int -> Int -> Int -> Bool
within tol a b = abs (a - b) <= tol

fillB :: Path -> Cmd
fillB = CFill identityA NonZero (PSolid black)

solid :: Color -> Path -> Cmd
solid c = CFill identityA NonZero (PSolid c)

circleP :: Double -> Double -> Double -> Path
circleP cx cy r =
  let k = 0.5522847498 * r in
  [ MoveTo (Pt (cx + r) cy), CubicTo (Pt (cx + r) (cy + k)) (Pt (cx + k) (cy + r)) (Pt cx (cy + r))
  , CubicTo (Pt (cx - k) (cy + r)) (Pt (cx - r) (cy + k)) (Pt (cx - r) cy)
  , CubicTo (Pt (cx - r) (cy - k)) (Pt (cx - k) (cy - r)) (Pt cx (cy - r))
  , CubicTo (Pt (cx + k) (cy - r)) (Pt (cx + r) (cy - k)) (Pt (cx + r) cy), ClosePath ]

-- The cubic circle's own area (its radius overshoots by at most 0.027%).
circleArea :: Double -> Double
circleArea r = pi * r * r

pentagram :: Double -> Double -> Double -> Path
pentagram cx cy r = case [ Pt (cx + r * sin a) (cy - r * cos a) | k <- [0, 2, 4, 1, 3 :: Int], let a = fromIntegral k * 2 * pi / 5 ] of
  (q : qs) -> MoveTo q : map LineTo qs ++ [ClosePath]
  [] -> []

line :: Double -> Double -> Double -> Double -> Path
line x0 y0 x1 y1 = [MoveTo (Pt x0 y0), LineTo (Pt x1 y1)]

strokeArea :: StrokeStyle -> Path -> Double
strokeArea ss p = ink (img0 64 64 [CStroke identityA ss (PSolid black) p])

------------------------------------------------------------------------------
-- Independent blend-mode reference (W3C Compositing Level 1), straight
-- colours in [0,1].

refBlend :: Blend -> (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
refBlend bm b@(br, bg, bb) s@(sr, sg, sb) = case bm of
  BlendHue -> setLum (setSat s (sat b)) (lum b)
  BlendSaturation -> setLum (setSat b (sat s)) (lum b)
  BlendColor -> setLum s (lum b)
  BlendLuminosity -> setLum b (lum s)
  _ -> (f br sr, f bg sg, f bb sb)
  where
    f cb cs = case bm of
      BlendNormal -> cs
      BlendMultiply -> cb * cs
      BlendScreen -> cb + cs - cb * cs
      BlendOverlay -> hardLight cs cb
      BlendDarken -> min cb cs
      BlendLighten -> max cb cs
      BlendColorDodge -> if cb == 0 then 0 else if cs >= 1 then 1 else min 1 (cb / (1 - cs))
      BlendColorBurn -> if cb >= 1 then 1 else if cs <= 0 then 0 else 1 - min 1 ((1 - cb) / cs)
      BlendHardLight -> hardLight cb cs
      BlendSoftLight ->
        if cs <= 0.5 then cb - (1 - 2 * cs) * cb * (1 - cb)
        else let d = if cb <= 0.25 then ((16 * cb - 12) * cb + 4) * cb else sqrt cb
             in cb + (2 * cs - 1) * (d - cb)
      BlendDifference -> abs (cb - cs)
      BlendExclusion -> cb + cs - 2 * cb * cs
      _ -> cs
    hardLight cb cs = if cs <= 0.5 then cb * 2 * cs else let t = 2 * cs - 1 in cb + t - cb * t
    lum (r, g, bl) = 0.3 * r + 0.59 * g + 0.11 * bl
    sat (r, g, bl) = maximum [r, g, bl] - minimum [r, g, bl]
    setLum c@(r, g, bl) l = let d = l - lum c in clipC (r + d, g + d, bl + d)
    clipC c@(r, g, bl) =
      let l = lum c; n = minimum [r, g, bl]; x = maximum [r, g, bl]
          lo v = if n < 0 then l + (v - l) * l / (l - n) else v
          hi v = if x > 1 then l + (v - l) * (1 - l) / (x - l) else v
      in (hi (lo r), hi (lo g), hi (lo bl))
    setSat (r, g, bl) sv =
      let mx = maximum [r, g, bl]; mn = minimum [r, g, bl]
          g' v = if mx > mn then (v - mn) * sv / (mx - mn) else 0
      in (g' r, g' g, g' bl)

allBlends :: [Blend]
allBlends =
  [ BlendNormal, BlendMultiply, BlendScreen, BlendOverlay, BlendDarken, BlendLighten
  , BlendColorDodge, BlendColorBurn, BlendHardLight, BlendSoftLight, BlendDifference
  , BlendExclusion, BlendHue, BlendSaturation, BlendColor, BlendLuminosity ]

to8 :: Double -> Int
to8 v = round (max 0 (min 1 v) * 255)

------------------------------------------------------------------------------
-- A brute-force two-point conical solver: scan t downward from the largest
-- candidate and take the first sign change of |p - c(t)| - r(t) with
-- r(t) >= 0.

bruteRadialT :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double) -> Maybe Double
bruteRadialT (x0, y0, r0) (x1, y1, r1) (px', py') =
  let f t = let cx = x0 + t * (x1 - x0); cy = y0 + t * (y1 - y0); r = r0 + t * (r1 - r0)
            in sqrt ((px' - cx) ^ (2 :: Int) + (py' - cy) ^ (2 :: Int)) - r
      ok t = r0 + t * (r1 - r0) >= 0
      ts = [ 4 - fromIntegral k * 1e-4 | k <- [0 .. 80000 :: Int] ]
      pairs = zip ts (drop 1 ts)
  in case [ (a + b) / 2 | (a, b) <- pairs, ok b, signum (f a) /= signum (f b) ] of
       (t : _) -> Just t
       [] -> Nothing

------------------------------------------------------------------------------
-- A small deterministic generator for the fuzz scenes.

newtype Gen = Gen Word64

next :: Gen -> (Word64, Gen)
next (Gen s) = let s1 = s `xor` (s * 0x2545F4914F6CDD1D + 0x9E3779B97F4A7C15)
                   s2 = s1 `xor` (s1 `shiftR` 29)
               in (s2, Gen (s2 * 6364136223846793005 + 1442695040888963407))

pick :: Gen -> Int -> (Int, Gen)
pick g n = let (w, g') = next g in (fromIntegral (w `shiftR` 33) `mod` max 1 n, g')

nasty :: [Double]
nasty = [0, 1, -1, 0.5, 63.9, 1e-9, -1e-9, 1e12, -1e12, 1e300, -1e300, 0 / 0, 1 / 0, -1 / 0, 3.7, 40, 1e6, -2.5]

dbl :: Gen -> (Double, Gen)
dbl g = let (k, g1) = pick g 40
        in if k < length nasty then (nasty !! k, g1)
           else let (w, g2) = next g1 in (fromIntegral (w `shiftR` 40) / 65536 * 80 - 8, g2)

ptG :: Gen -> (Pt, Gen)
ptG g = let (x, g1) = dbl g; (y, g2) = dbl g1 in (Pt x y, g2)

affG :: Gen -> (Affine, Gen)
affG g = let (k, g1) = pick g 6 in case k of
  0 -> (identityA, g1)
  1 -> let (a, g2) = dbl g1; (b, g3) = dbl g2 in (Affine a 0 0 b 3 4, g3)
  2 -> (Affine 0 0 0 0 5 5, g1)                -- singular
  3 -> let (a, g2) = dbl g1 in (rotateA a, g2)
  4 -> let (a, g2) = dbl g1; (b, g3) = dbl g2; (c, g4) = dbl g3 in (Affine a b c 1 0 0, g4)
  _ -> (Affine 0.5 0.2 (-0.3) 1.5 10 (-4), g1)

pathG :: Gen -> Int -> (Path, Gen)
pathG g 0 = ([], g)
pathG g n =
  let (k, g1) = pick g 6
      (p, g2) = ptG g1
      (q, g3) = ptG g2
      (r, g4) = ptG g3
      seg = case k of
        0 -> MoveTo p
        1 -> LineTo p
        2 -> QuadTo p q
        3 -> CubicTo p q r
        4 -> ClosePath
        _ -> LineTo p
      (rest, g5) = pathG g4 (n - 1)
  in (seg : rest, g5)

paintG :: Gen -> Int -> (Paint, Gen)
paintG g depth =
  let (k, g1) = pick g 7
      (a, g2) = affG g1
      (p, g3) = ptG g2
      (q, g4) = ptG g3
      (d, g5) = dbl g4
      stops = [Stop 0 (Color 1 0 0 1), Stop d (Color 0 1 0 0.5), Stop 1 (Color 0 0 1 1)]
      (sk, g6) = pick g5 6
      sp = [SpreadPad, SpreadReflect, SpreadRepeat, SpreadNone, SpreadPadStart, SpreadPadEnd] !! sk
  in case k of
       0 -> (PSolid (Color 0.2 0.4 0.6 0.7), g6)
       1 -> (PLinear a p q stops sp, g6)
       2 -> (PRadial a p d q 10 stops sp, g6)
       3 -> (PRadial a p 0 p d stops sp, g6)
       4 -> (PSampled a tinyImg sp, g6)
       5 | depth < 2 -> let (cmds, g7) = cmdsG g6 2 (depth + 1) in (PTiling (fromIntegral k) a (Rect 0 0 d 7) d 7 cmds, g7)
       _ -> (PSolid (Color 0 0 0 1), g6)

tinyImg :: Image
tinyImg = Image 3 2 "t" (listArray (0, 23) [ fromIntegral (i * 37 `mod` 256) | i <- [0 .. 23 :: Int] ])

cmdG :: Gen -> Int -> (Cmd, Gen)
cmdG g depth =
  let (k, g1) = pick g 12
      (a, g2) = affG g1
      (np, g3) = pick g2 8
      (path, g4) = pathG g3 np
      (paint, g5) = paintG g4 depth
      (w, g6) = dbl g5
      (dk, g7) = pick g6 4
      dash = [[], [0, 3], [w, 2, 0.5], [-1, 2]] !! dk
      ss = StrokeStyle w ([CapButt, CapRound, CapSquare] !! (dk `mod` 3)) ([JoinMiter, JoinRound, JoinBevel] !! (np `mod` 3)) w dash w (odd np)
      sub n = cmdsG g7 n (depth + 1)
      (bk, _) = pick g7 16
  in case k of
       0 -> (CFill a NonZero paint path, g7)
       1 -> (CFill a EvenOdd paint path, g7)
       2 -> (CStroke a ss paint path, g7)
       3 -> (CGlyphs a paint [Glyph a 1 path, Glyph identityA (-1) path], g7)
       4 -> (CImage a (if odd np then tinyImg else Image 0 0 "e" (listArray (0, 0) [0])) (ImageOpts InterpNearest w), g7)
       5 -> (CStencil a (Mask 2 2 (listArray (0, 3) [0, 255, 128, 7])) paint InterpSmooth, g7)
       6 -> let p0 = Pt 0 0 in
            (CGouraud a [GouraudTri p0 red' (Pt w 3) green' (Pt 4 w) blue', GouraudTri p0 red' p0 red' p0 red'], g7)
       7 -> (CGouraudRamp a [Stop 0 red', Stop 1 blue'] [RampTri (Pt 0 0) w (Pt 20 1) 0.5 (Pt 3 20) (-1)], g7)
       8 | depth < 3 -> let (kids, g8) = sub 3 in (CClip [ClipShape a NonZero path, ClipShape identityA EvenOdd path] kids, g8)
       9 | depth < 3 ->
             let (kids, g8) = sub 3
                 (mk, g9) = cmdsG g8 2 (depth + 1)
                 opts = GroupOpts (abs w) (allBlends !! bk) (if odd np then Just (SoftMask 1 MaskLuminosity (Color 0 0 0 1) mk (Just (listArray (0, 255) [ fromIntegral (255 - i) | i <- [0 .. 255 :: Int] ]))) else Nothing)
                                  (if dk == 1 then Just (Filter (Rect 0 0 w 30) a [ FilterPrim op [Previous, SourceAlpha] Nothing (odd bk) bk | op <- filterOps w ]) else Nothing)
                                  (even bk) (dk == 2)
             in (CGroup opts kids, g9)
       10 | depth < 3 -> let (kids, g8) = sub 3 in (CTransform a kids, g8)
       _ -> (CFill identityA NonZero (PSolid (Color 0 0 0 0.3)) (rectPath 1 1 5 5), g7)
  where
    red' = Color 1 0 0 1
    green' = Color 0 1 0 1
    blue' = Color 0 0 1 1

filterOps :: Double -> [FilterOp]
filterOps w =
  [ FeBlur w 1, FeOffset w (-2), FeFlood (Color 0.1 0.2 0.3 0.4), FeColorMatrix [w, 1, 2], FeComposite CompXor
  , FeComposite (CompArithmetic w 1 0.5 0), FeMerge, FeBlend BlendHue, FeDropShadow 1 w 2 3 (Color 0 0 0 0.5) ]

cmdsG :: Gen -> Int -> Int -> ([Cmd], Gen)
cmdsG g n depth = go g n
  where
    go gg 0 = ([], gg)
    go gg k = let (c, g1) = cmdG gg depth; (cs, g2) = go g1 (k - 1) in (c : cs, g2)

------------------------------------------------------------------------------
-- An independent exact area: a polygon clipped to the canvas rectangle
-- (Sutherland–Hodgman), then the shoelace formula.

clippedArea :: Double -> Double -> [Pt] -> Double
clippedArea w h poly =
  let clipBy inside cross ps = case ps of
        [] -> []
        _ -> concat [ edge a b | (a, b) <- zip ps (drop 1 ps ++ take 1 ps) ]
        where edge a b = case (inside a, inside b) of
                (True, True) -> [b]
                (True, False) -> [cross a b]
                (False, True) -> [cross a b, b]
                (False, False) -> []
      atX x (Pt ax ay) (Pt bx by) = let t = (x - ax) / (bx - ax) in Pt x (ay + t * (by - ay))
      atY y (Pt ax ay) (Pt bx by) = let t = (y - ay) / (by - ay) in Pt (ax + t * (bx - ax)) y
      p1 = clipBy (\(Pt x _) -> x >= 0) (atX 0) poly
      p2 = clipBy (\(Pt x _) -> x <= w) (atX w) p1
      p3 = clipBy (\(Pt _ y) -> y >= 0) (atY 0) p2
      p4 = clipBy (\(Pt _ y) -> y <= h) (atY h) p3
  in abs (sum [ ax * by - bx * ay | (Pt ax ay, Pt bx by) <- zip p4 (drop 1 p4 ++ take 1 p4) ]) / 2

polyPath :: [Pt] -> Path
polyPath [] = []
polyPath (q : qs) = MoveTo q : map LineTo qs ++ [ClosePath]

isIn :: String -> String -> Bool
isIn needle hay = any (\t -> take (length needle) t == needle) (suffixes hay)
  where suffixes xs = xs : case xs of { [] -> []; (_ : r) -> suffixes r }

rowAlphas :: Image -> Int -> [Int]
rowAlphas img y = [ alphaAt img x y | x <- [0 .. imgW img - 1] ]

------------------------------------------------------------------------------

rasterSpec :: (String -> Bool -> IO ()) -> IO ()
rasterSpec check = do
  -- Coverage --------------------------------------------------------------
  let rectImg = img0 64 64 [fillB (rectPath 10.25 10.5 20.5 30.25)]
  check "raster: a sub-pixel rectangle's ink is its exact area"
    (near 0.3 (ink rectImg) (20.5 * 30.25))
  check "raster: a rectangle's edge pixels carry the covered fraction"
    (alphaAt rectImg 10 20 == 191 && alphaAt rectImg 10 10 == 96 && alphaAt rectImg 20 20 == 255
     && alphaAt rectImg 30 20 == 191 && alphaAt rectImg 9 20 == 0 && alphaAt rectImg 31 20 == 0
     && alphaAt rectImg 20 40 == 191)
  check "raster: a pixel-aligned rectangle is exactly opaque inside and clear outside"
    (let im = img0 32 32 [fillB (rectPath 4 4 8 8)]
     in ink im == 64 && alphaAt im 4 4 == 255 && alphaAt im 11 11 == 255 && alphaAt im 12 11 == 0)
  check "raster: a circle's area is within 0.5%"
    (let a = ink (img0 64 64 [fillB (circleP 32 32 20)]) in abs (a - circleArea 20) / circleArea 20 < 0.005)
  check "raster: a small circle's area is within 1%"
    (let a = ink (img0 16 16 [fillB (circleP 8 8 3)]) in abs (a - circleArea 3) / circleArea 3 < 0.01)
  check "raster: a shape beyond every edge of the canvas fills it exactly"
    (ink (img0 20 20 [fillB (rectPath (-100) (-50) 300 400)]) == 400)
  check "raster: a shape crossing the left edge keeps its winding"
    (let im = img0 20 20 [fillB [MoveTo (Pt (-30) 2), LineTo (Pt 10 2), LineTo (Pt 10 12), LineTo (Pt (-30) 12), ClosePath]]
     in ink im == 100 && alphaAt im 0 5 == 255 && alphaAt im 10 5 == 0)
  let starNZ = img0 64 64 [CFill identityA NonZero (PSolid black) (pentagram 32 33 28)]
      starEO = img0 64 64 [CFill identityA EvenOdd (PSolid black) (pentagram 32 33 28)]
  check "raster: a pentagram's centre is filled under non-zero"
    (alphaAt starNZ 32 33 == 255)
  check "raster: a pentagram's centre is empty under even-odd"
    (alphaAt starEO 32 33 == 0)
  check "raster: a pentagram's points are filled under both rules"
    (alphaAt starNZ 32 8 == 255 && alphaAt starEO 32 8 == 255)
  check "raster: even-odd ink is non-zero ink minus the pentagon"
    (let pent = ink starNZ - ink starEO
         -- inner pentagon of a pentagram with circumradius 28
         ri = 28 * cos (2 * pi / 5) / cos (pi / 5)
         area = 5 / 2 * ri * ri * sin (2 * pi / 5)
     in abs (pent - area) / area < 0.01)
  check "raster: overlapping same-direction subpaths fill once under non-zero"
    (let im = img0 40 40 [fillB (rectPath 5 5 20 20 ++ rectPath 15 15 20 20)]
     in ink im == 400 + 400 - 100 && alphaAt im 20 20 == 255)
  check "raster: an opposite-winding subpath cuts a hole under non-zero"
    (let hole = [MoveTo (Pt 10 10), LineTo (Pt 10 20), LineTo (Pt 20 20), LineTo (Pt 20 10), ClosePath]
         im = img0 30 30 [fillB (rectPath 0 0 30 30 ++ hole)]
     in alphaAt im 15 15 == 0 && ink im == 900 - 100)
  check "raster: a twice-covered region stays covered under even-odd only once"
    (let im = img0 40 40 [CFill identityA EvenOdd (PSolid black) (rectPath 0 0 20 20 ++ rectPath 10 0 20 20)]
     in alphaAt im 15 5 == 0 && alphaAt im 5 5 == 255 && alphaAt im 25 5 == 255)
  check "raster: a transformed fill lands where the matrix puts it"
    (let im = img0 40 40 [CTransform (Affine 2 0 0 3 10 5) [fillB (rectPath 0 0 5 5)]]
     in ink im == 150 && alphaAt im 10 5 == 255 && alphaAt im 19 19 == 255 && alphaAt im 20 19 == 0)
  check "raster: the view matrix scales the whole scene"
    (let (im, _) = renderScene (defaultRenderOpts 40 40 (scaleA 4 4)) (Scene (Rect 0 0 10 10) [fillB (rectPath 1 1 2 2)] [])
     in ink im == 64)

  -- Strokes ---------------------------------------------------------------
  let base = defaultStroke { ssWidth = 4 }
      hline = line 10 30 50 30
  check "raster: a butt-capped line's area is length × width"
    (near 0.5 (strokeArea base hline) 160)
  check "raster: a square cap adds half the width at each end"
    (near 0.5 (strokeArea base { ssCap = CapSquare } hline) (44 * 4))
  check "raster: a round cap adds a half disc at each end"
    (near 0.6 (strokeArea base { ssCap = CapRound } hline) (160 + pi * 4))
  let ell = [MoveTo (Pt 10 30), LineTo (Pt 30 30), LineTo (Pt 30 50)]
      -- two 20 × 4 arms, minus the shared 2 × 2, plus the outside corner
      ellBody = 80 + 80 - 4
  check "raster: a right-angle miter join fills the corner square"
    (near 0.5 (strokeArea base { ssJoin = JoinMiter } ell) (ellBody + 4))
  check "raster: a bevel join cuts the corner square in half"
    (near 0.5 (strokeArea base { ssJoin = JoinBevel } ell) (ellBody + 2))
  check "raster: a round join adds a quarter disc"
    (near 0.6 (strokeArea base { ssJoin = JoinRound } ell) (ellBody + pi))
  check "raster: a miter past its limit becomes a bevel"
    (let sharp = [MoveTo (Pt 10 50), LineTo (Pt 32 10), LineTo (Pt 54 50)]
     in strokeArea base { ssJoin = JoinMiter, ssMiterLimit = 1.5 } sharp
        == strokeArea base { ssJoin = JoinBevel } sharp
        && strokeArea base { ssJoin = JoinMiter, ssMiterLimit = 10 } sharp
           > strokeArea base { ssJoin = JoinBevel } sharp + 5)
  check "raster: a closed square stroke is the outer square minus the inner"
    (near 0.5 (strokeArea base (rectPath 10 10 30 30)) (34 * 34 - 26 * 26))
  check "raster: a self-crossing stroke is one shape (no double coverage)"
    (let im = img0 64 64 [CStroke identityA base (PSolid (Color 0 0 0 0.5)) [MoveTo (Pt 10 10), LineTo (Pt 50 50), MoveTo (Pt 50 10), LineTo (Pt 10 50)]]
     in within 1 (alphaAt im 30 30) 128)
  check "raster: a stroke under a non-uniform scale is elliptical"
    (let im = img0 64 64 [CTransform (scaleA 4 1) [CStroke identityA defaultStroke { ssWidth = 2 } (PSolid black) (line 2 30 12 30)]]
     in near 0.5 (ink im) (40 * 2))
  check "raster: a stroke under a vertical stretch is thick across"
    (let im = img0 64 64 [CTransform (scaleA 1 4) [CStroke identityA defaultStroke { ssWidth = 2 } (PSolid black) (line 10 5 50 5)]]
     in near 0.5 (ink im) (40 * 8))
  check "raster: a zero-width stroke is one device pixel wide"
    (let im = img0 64 64 [CStroke identityA defaultStroke { ssWidth = 0 } (PSolid black) (line 5 10.5 55 10.5)]
     in near 0.5 (ink im) 50 && alphaAt im 20 10 == 255)
  check "raster: a hairline stays one pixel under a magnifying matrix"
    (let im = img0 64 64 [CTransform (scaleA 10 10) [CStroke identityA defaultStroke { ssWidth = 0 } (PSolid black) (line 0.5 1.05 5.5 1.05)]]
     in near 0.5 (ink im) 50)
  check "raster: stroke adjustment keeps a thin line one pixel wide"
    (let ss = defaultStroke { ssWidth = 0.1, ssAdjust = True }
     in near 0.5 (strokeArea ss (line 5 10.5 55 10.5)) 50
        && near 0.3 (strokeArea ss { ssAdjust = False } (line 5 10.5 55 10.5)) 5)
  check "raster: dashes cover exactly the on-intervals"
    (let ss = defaultStroke { ssWidth = 2, ssDash = [10, 10] }
         im = img0 100 20 [CStroke identityA ss (PSolid black) (line 0 10 100 10)]
     in near 0.5 (ink im) 100 && alphaAt im 5 10 == 255 && alphaAt im 15 10 == 0 && alphaAt im 25 10 == 255)
  check "raster: the dash phase shifts the pattern"
    (let ss = defaultStroke { ssWidth = 2, ssDash = [10, 10], ssDashPhase = 5 }
         im = img0 100 20 [CStroke identityA ss (PSolid black) (line 0 10 100 10)]
     in alphaAt im 2 10 == 255 && alphaAt im 7 10 == 0 && alphaAt im 16 10 == 255 && near 0.5 (ink im) 100)
  check "raster: an odd dash array repeats itself"
    (let ss = defaultStroke { ssWidth = 2, ssDash = [10] }
         im = img0 100 20 [CStroke identityA ss (PSolid black) (line 0 10 100 10)]
     in near 0.5 (ink im) 100 && alphaAt im 15 10 == 0)
  check "raster: an all-zero or negative dash array strokes solid"
    (strokeArea defaultStroke { ssWidth = 2, ssDash = [0, 0] } (line 0 10 50 10) == 100
     && strokeArea defaultStroke { ssWidth = 2, ssDash = [5, -1] } (line 0 10 50 10) == 100)
  check "raster: a dash running through a closed path's start is joined there"
    (let ss = defaultStroke { ssWidth = 4, ssDash = [50, 10], ssDashPhase = 40, ssJoin = JoinMiter }
         im = img0 64 64 [CStroke identityA ss (PSolid black) (rectPath 20 20 40 40)]
     in alphaAt im 18 18 == 255 && alphaAt im 19 19 == 255)
  check "raster: zero-length dashes with round caps are dots"
    (let ss = defaultStroke { ssWidth = 6, ssDash = [0, 20], ssCap = CapRound }
         a = ink (img0 100 20 [CStroke identityA ss (PSolid black) (line 10 10 90 10)])
     -- dots at 0, 20, 40 and 60; the interval that would start exactly at
     -- the end draws nothing (resvg agrees)
     in abs (a - 4 * pi * 9) / (4 * pi * 9) < 0.03)
  check "raster: a zero-length subpath with round caps is a dot, with butt caps nothing"
    (let dot cap = ink (img0 40 40 [CStroke identityA defaultStroke { ssWidth = 10, ssCap = cap } (PSolid black) [MoveTo (Pt 20 20), LineTo (Pt 20 20)]])
     in abs (dot CapRound - 25 * pi) / (25 * pi) < 0.02 && near 0.5 (dot CapSquare) 100 && dot CapButt == 0)
  check "raster: a bare moveto strokes nothing; moveto-closepath with round caps is a dot"
    (ink (img0 40 40 [CStroke identityA defaultStroke { ssWidth = 10, ssCap = CapRound } (PSolid black) [MoveTo (Pt 20 20)]]) == 0
     && ink (img0 40 40 [CStroke identityA defaultStroke { ssWidth = 10, ssCap = CapRound } (PSolid black) [MoveTo (Pt 20 20), ClosePath]]) > 70)
  check "raster: strokeToPath filled non-zero equals the stroke"
    (let ss = defaultStroke { ssWidth = 5, ssJoin = JoinRound, ssCap = CapSquare, ssDash = [7, 3] }
         p = [MoveTo (Pt 5 40), CubicTo (Pt 20 0) (Pt 40 70) (Pt 58 20)]
         a = ink (img0 64 64 [CStroke identityA ss (PSolid black) p])
         b = ink (img0 64 64 [fillB (strokeToPath ss 0.05 p)])
     in near 1 a b && a > 100)
  check "raster: a hairpin turn strokes without a hole"
    (let im = img0 64 64 [CStroke identityA defaultStroke { ssWidth = 6, ssJoin = JoinRound } (PSolid black)
                            [MoveTo (Pt 10 30), LineTo (Pt 50 30), LineTo (Pt 10 31)]]
     -- the turn's round join reaches x = 52.96 on this row
     in alphaAt im 30 30 == 255 && alphaAt im 51 30 == 255 && alphaAt im 52 30 > 230)

  -- Clips -----------------------------------------------------------------
  check "raster: a rectangular clip intersects exactly, sub-pixel edges included"
    (let im = img0 40 40 [CClip [ClipShape identityA NonZero (rectPath 5.5 5 10 10)] [fillB (rectPath 0 0 40 40)]]
     in near 0.2 (ink im) 100 && alphaAt im 5 8 == 128 && alphaAt im 15 8 == 128 && alphaAt im 10 8 == 255)
  check "raster: nested clips intersect"
    (let im = img0 40 40 [CClip [ClipShape identityA NonZero (rectPath 0 0 20 20)]
                            [CClip [ClipShape identityA NonZero (rectPath 10 10 20 20)] [fillB (rectPath 0 0 40 40)]]]
     in ink im == 100 && alphaAt im 15 15 == 255 && alphaAt im 5 5 == 0 && alphaAt im 25 25 == 0)
  check "raster: a clip is the union of its shapes"
    (let im = img0 40 40 [CClip [ClipShape identityA NonZero (rectPath 0 0 10 10), ClipShape identityA NonZero (rectPath 20 20 10 10)]
                            [fillB (rectPath 0 0 40 40)]]
     in ink im == 200)
  check "raster: a curved clip inside a rectangular one"
    (let im = img0 64 64 [CClip [ClipShape identityA NonZero (rectPath 0 0 32 64)]
                            [CClip [ClipShape identityA NonZero (circleP 32 32 20)] [fillB (rectPath 0 0 64 64)]]]
     in abs (ink im - circleArea 20 / 2) / (circleArea 20 / 2) < 0.01)
  check "raster: a rotated rectangle clip goes through the mask path"
    (let sq = composeA (translateA 32 32) (rotateA (pi / 4))
         im = img0 64 64 [CClip [ClipShape sq NonZero (rectPath (-10) (-10) 20 20)] [fillB (rectPath 0 0 64 64)]]
     in near 2 (ink im) 400)
  check "raster: an empty clip list clips everything away"
    (ink (img0 20 20 [CClip [] [fillB (rectPath 0 0 20 20)]]) == 0)
  check "raster: an even-odd clip shape is honoured"
    (let im = img0 40 40 [CClip [ClipShape identityA EvenOdd (rectPath 0 0 30 30 ++ rectPath 10 10 10 10)] [fillB (rectPath 0 0 40 40)]]
     in alphaAt im 15 15 == 0 && alphaAt im 5 5 == 255 && ink im == 800)

  -- Paints ----------------------------------------------------------------
  let ramp sp = [CFill identityA NonZero (PLinear identityA (Pt 0 0) (Pt 100 0) [Stop 0 black, Stop 1 white] sp) (rectPath 0 0 200 4)]
      grey im x = let (r, _, _, a) = px im x 1 in (r, a)
  check "raster: a linear gradient is linear in t at pixel centres"
    (let im = img0 200 4 (ramp SpreadPad)
     in grey im 25 == (to8 0.255, 255) && grey im 50 == (to8 0.505, 255) && grey im 150 == (255, 255) && grey im 0 == (1, 255))
  check "raster: linear spreads: reflect, repeat and none beyond the end"
    (let r = img0 200 4 (ramp SpreadReflect); p = img0 200 4 (ramp SpreadRepeat); n = img0 200 4 (ramp SpreadNone)
     in grey r 130 == (to8 0.695, 255) && grey p 130 == (to8 0.305, 255) && snd (grey n 130) == 0 && grey n 30 == (to8 0.305, 255))
  check "raster: pad-start and pad-end extend one end only"
    (let s = img0 200 4 [CFill identityA NonZero (PLinear identityA (Pt 50 0) (Pt 150 0) [Stop 0 black, Stop 1 white] SpreadPadStart) (rectPath 0 0 200 4)]
         e = img0 200 4 [CFill identityA NonZero (PLinear identityA (Pt 50 0) (Pt 150 0) [Stop 0 black, Stop 1 white] SpreadPadEnd) (rectPath 0 0 200 4)]
     in grey s 10 == (0, 255) && snd (grey s 180) == 0 && snd (grey e 10) == 0 && grey e 180 == (255, 255))
  check "raster: gradient stops interpolate straight colour, then premultiply"
    (let im = img0 100 4 [CFill identityA NonZero (PLinear identityA (Pt 0 0) (Pt 100 0) [Stop 0 (Color 1 0 0 1), Stop 1 (Color 0 0 1 0)] SpreadPad) (rectPath 0 0 100 4)]
         (r, _, b, a) = px im 50 1
     in within 1 a (to8 0.495) && within 1 r (to8 0.495) && within 1 b (to8 0.505))
  let radialAt c0 r0 c1 r1 sp (x, y) =
        let im = img0 100 100 [CFill identityA NonZero (PRadial identityA c0 r0 c1 r1 [Stop 0 black, Stop 1 white] sp) (rectPath 0 0 100 100)]
        in px im x y
      Pt ax ay `pt3` r = (ax, ay, r)
  check "raster: a concentric radial gradient's t is distance over radius"
    (let (r, _, _, _) = radialAt (Pt 50 50) 0 (Pt 50 50) 40 SpreadPad (70, 50)
     in within 1 r (to8 (sqrt (20.5 * 20.5 + 0.25) / 40)))
  check "raster: focal radial gradients take the largest t (brute-force reference)"
    (and [ let (r, _, _, _) = radialAt (Pt 40 50) 0 (Pt 50 50) 40 SpreadPad (x, y)
               t = bruteRadialT (Pt 40 50 `pt3` 0) (Pt 50 50 `pt3` 40) (fromIntegral x + 0.5, fromIntegral y + 0.5)
           in maybe False (within 1 r . to8) t
         | (x, y) <- [(60, 50), (30, 50), (45, 20), (80, 70)] ])
  check "raster: a two-circle cone takes the larger root and spreads correctly"
    (and [ let (r, _, _, a) = radialAt (Pt 20 50) 5 (Pt 80 50) 10 sp (x, y)
               t = bruteRadialT (Pt 20 50 `pt3` 5) (Pt 80 50 `pt3` 10) (fromIntegral x + 0.5, fromIntegral y + 0.5)
           in case t of
                Just tv | tv >= 0 && tv <= 1 -> a == 255 && within 1 r (to8 tv)
                _ -> True
         | sp <- [SpreadNone, SpreadPad], (x, y) <- [(50, 50), (60, 52), (35, 49)] ]
     && let (_, _, _, a) = radialAt (Pt 20 50) 5 (Pt 80 50) 10 SpreadNone (50, 10) in a == 0)
  check "raster: SpreadNone limits a radial to t in [0,1]"
    (let (_, _, _, a) = radialAt (Pt 50 50) 10 (Pt 50 50) 30 SpreadNone (50, 52)
         (_, _, _, b) = radialAt (Pt 50 50) 10 (Pt 50 50) 30 SpreadNone (50, 90)
         (r, _, _, c) = radialAt (Pt 50 50) 10 (Pt 50 50) 30 SpreadNone (70, 50)
     in a == 0 && b == 0 && c == 255 && within 1 r (to8 ((20.5 - 10) / 20)))
  check "raster: a tiling pattern repeats its cell"
    (let pat = PTiling 1 identityA (Rect 0 0 10 10) 10 10 [solid (Color 1 0 0 1) (rectPath 0 0 5 10)]
         im = img0 60 60 [CFill identityA NonZero pat (rectPath 0 0 60 60)]
         (r1, _, _, a1) = px im 2 30; (_, _, _, a2) = px im 7 30; (r3, _, _, a3) = px im 52 41
     in r1 == 255 && a1 == 255 && a2 == 0 && r3 == 255 && a3 == 255 && near 1 (ink im) 1800)
  check "raster: a tiling pattern's cell overlapping its step tiles correctly"
    -- cells 7 wide every 5: each period sees its own cell and the tail of
    -- the previous one, composited in order
    (let pat = PTiling 2 identityA (Rect 0 0 10 10) 5 10 [solid (Color 0 0 1 0.5) (rectPath 0 0 7 10)]
         im = img0 30 10 [CFill identityA NonZero pat (rectPath 0 0 30 10)]
     in and (zipWith (within 1) [ alphaAt im x 5 | x <- [0 .. 9] ] [191, 191, 128, 128, 128, 191, 191, 128, 128, 128]))
  check "raster: a transformed, offset pattern's lattice is pattern space's (analytic)"
    -- cell (i, j) is the cell rectangle moved by (i·xs, j·ys) in pattern
    -- space, then mapped through the pattern, command and view matrices
    (and [ let pt = composeA (translateA 4 2) (composeA (rotateA (20 * pi / 180)) (scaleA s1 s2))
               inv = maybe identityA id (invertA (composeA view pt))
               inCell (Pt u v) = let fu = (u - 3) / 17; fv = (v - 7) / 13
                                     uu = 3 + 17 * (fu - fromIntegral (floor fu :: Int)); vv = 7 + 13 * (fv - fromIntegral (floor fv :: Int))
                                 in (uu >= 6 && uu < 14 && vv >= 14 && vv < 20, min (min (abs (uu - 6)) (abs (uu - 14))) (min (abs (vv - 14)) (abs (vv - 20))))
               pat = PTiling 4 pt (Rect 3 7 20 20) 17 13 [CTransform (translateA 3 7) [fillB (rectPath 3 7 8 13)]]
               (im, _) = renderScene (defaultRenderOpts 48 48 view) (Scene (Rect 0 0 48 48) [CFill identityA NonZero pat (rectPath (-100) (-100) 300 300)] [])
           in and [ a == expect
                  | x <- [0, 3 .. 47], y <- [0, 3 .. 47]
                  , let (inside, margin) = inCell (applyA inv (Pt (fromIntegral x + 0.5) (fromIntegral y + 0.5)))
                  , margin * min s1 s2 > 1.2
                  , let a = alphaAt im x y > 127; expect = inside ]
         | (s1, s2, view) <- [(1.3, 0.8, identityA), (0.5, 0.5, scaleA 1.5 1.5), (1, 1, Affine 1 0 0.3 1 2 5)] ])
  check "raster: a sampled paint maps the image's unit square"
    (let im2 = Image 2 1 "t" (listArray (0, 7) [255, 0, 0, 255, 0, 0, 255, 255])
         im = img0 20 10 [CFill identityA NonZero (PSampled (Affine 20 0 0 10 0 0) im2 SpreadPad) (rectPath 0 0 20 10)]
         (r, _, b, _) = px im 2 5; (r', _, b', _) = px im 17 5
     in r == 255 && b == 0 && r' == 0 && b' == 255)

  -- Images and stencils ---------------------------------------------------
  let checker = Image 4 4 "c" (listArray (0, 63) (concat [ if even (x + y) then [0, 0, 0, 255] else [255, 255, 255, 255] | y <- [0 .. 3 :: Int], x <- [0 .. 3 :: Int] ]))
  check "raster: a minified image is area-averaged"
    (let im = img0 2 2 [CImage (Affine 2 0 0 2 0 0) checker defaultImageOpts]
     in all (\(x, y) -> let (r, _, _, a) = px im x y in within 1 r 128 && a == 255) [(0, 0), (1, 0), (0, 1), (1, 1)])
  check "raster: a minified image at a non-integer factor averages too"
    (let im = img0 3 3 [CImage (Affine 3 0 0 3 0 0) (Image 9 9 "s" (listArray (0, 323) (concat [ if x `mod` 3 == 0 then [255, 255, 255, 255] else [0, 0, 0, 255] | _y <- [0 .. 8 :: Int], x <- [0 .. 8 :: Int] ]))) defaultImageOpts]
         (r, _, _, _) = px im 1 1
     in within 2 r 85)
  check "raster: a magnified image samples nearest when asked"
    (let im2 = Image 2 2 "q" (listArray (0, 15) [255, 0, 0, 255, 0, 255, 0, 255, 0, 0, 255, 255, 255, 255, 255, 255])
         im = img0 20 20 [CImage (Affine 20 0 0 20 0 0) im2 defaultImageOpts { ioInterp = InterpNearest }]
     in px im 9 9 == (255, 0, 0, 255) && px im 10 9 == (0, 255, 0, 255) && px im 9 10 == (0, 0, 255, 255) && px im 19 19 == (255, 255, 255, 255))
  check "raster: a magnified image interpolates bilinearly by default"
    (let im2 = Image 2 1 "g" (listArray (0, 7) [0, 0, 0, 255, 255, 255, 255, 255])
         im = img0 20 1 [CImage (Affine 20 0 0 1 0 0) im2 defaultImageOpts]
         (r, _, _, _) = px im 10 0
     in within 1 r (to8 (0.5 + 0.5 / 10)) && px im 0 0 == (0, 0, 0, 255))
  check "raster: image opacity scales alpha"
    (let im = img0 4 4 [CImage (Affine 4 0 0 4 0 0) checker defaultImageOpts { ioOpacity = 0.5 }]
     in all (\(x, y) -> within 1 (alphaAt im x y) 128) [(0, 0), (3, 3)])
  check "raster: a rotated image keeps its area"
    (let m = composeA (translateA 32 32) (composeA (rotateA 0.5) (Affine 20 0 0 10 (-10) (-5)))
     in near 1.5 (ink (img0 64 64 [CImage m checker defaultImageOpts])) 200)
  check "raster: a stencil mask is painted with the paint"
    (let mk = Mask 2 2 (listArray (0, 3) [255, 0, 0, 255])
         im = img0 20 20 [CStencil (Affine 20 0 0 20 0 0) mk (PSolid (Color 0 0 1 1)) InterpNearest]
     in px im 5 5 == (0, 0, 255, 255) && alphaAt im 15 5 == 0 && alphaAt im 5 15 == 0 && px im 15 15 == (0, 0, 255, 255))
  check "raster: a minified stencil averages its coverage"
    (let mk = Mask 4 4 (listArray (0, 15) [ if even (x + y) then 255 else 0 | y <- [0 .. 3 :: Int], x <- [0 .. 3 :: Int] ])
         im = img0 2 2 [CStencil (Affine 2 0 0 2 0 0) mk (PSolid black) InterpSmooth]
     in within 1 (alphaAt im 0 0) 128 && within 1 (alphaAt im 1 1) 128)

  -- Gouraud ---------------------------------------------------------------
  check "raster: a Gouraud triangle interpolates its corner colours"
    (let t = GouraudTri (Pt 0 0) (Color 1 0 0 1) (Pt 60 0) (Color 0 1 0 1) (Pt 0 60) (Color 0 0 1 1)
         im = img0 60 60 [CGouraud identityA [t]]
         (r, g, b, a) = px im 19 19
     in a == 255 && within 2 r (to8 (1 - 39 / 60)) && within 2 g (to8 (19.5 / 60)) && within 2 b (to8 (19.5 / 60)))
  check "raster: adjacent Gouraud triangles leave no seam"
    (let c = Color 0.2 0.6 0.4 1
         ts = [GouraudTri (Pt 0 0) c (Pt 40 0) c (Pt 0 40) c, GouraudTri (Pt 40 0) c (Pt 40 40) c (Pt 0 40) c]
         im = img0 40 40 [CGouraud identityA ts]
     in all (\x -> alphaAt im x (39 - x) == 255) [0 .. 39] && ink im == 1600)
  check "raster: a ramp mesh interpolates t, then looks it up"
    (let im = img0 40 40 [CGouraudRamp identityA [Stop 0 black, Stop 0.5 (Color 1 0 0 1), Stop 1 white]
                            [RampTri (Pt 0 0) 0 (Pt 40 0) 1 (Pt 0 40) 0, RampTri (Pt 40 0) 1 (Pt 40 40) 1 (Pt 0 40) 0]]
         (r, g, _, _) = px im 30 10
     in within 2 r 255 && within 3 g (to8 ((30.5 / 40 - 0.5) * 2)))

  -- Groups and compositing ------------------------------------------------
  check "raster: group opacity composites the group once"
    (let im = img0 40 40 [CGroup defaultGroup { goOpacity = 0.5, goIsolated = True } [fillB (rectPath 0 0 30 30), fillB (rectPath 10 10 30 30)]]
     in within 1 (alphaAt im 20 20) 128 && within 1 (alphaAt im 5 5) 128)
  check "raster: a default group is drawn as if absent"
    (let a = img0 40 40 [CGroup defaultGroup [solid (Color 1 0 0 0.5) (rectPath 0 0 30 30), solid (Color 0 0 1 0.5) (rectPath 10 10 30 30)]]
         b = img0 40 40 [solid (Color 1 0 0 0.5) (rectPath 0 0 30 30), solid (Color 0 0 1 0.5) (rectPath 10 10 30 30)]
     in imgPix a == imgPix b)
  check "raster: a group around one leaf folds its opacity into coverage"
    (let a = img0 40 40 [CGroup defaultGroup { goOpacity = 0.4, goIsolated = True } [solid (Color 0 1 0 1) (circleP 20 20 15)]]
         b = img0 40 40 [solid (Color 0 1 0 0.4) (circleP 20 20 15)]
     in imgPix a == imgPix b)
  check "raster: every blend mode matches its W3C formula on opaque colours"
    (and [ let cb = (0.8, 0.3, 0.1); cs = (0.2, 0.5, 0.9)
               toC (r, g, b) = Color r g b 1
               im = renderBg (toC cb) 4 4 [CGroup defaultGroup { goBlend = bm, goIsolated = True } [solid (toC cs) (rectPath 0 0 4 4)]]
               (er, eg, eb) = refBlend bm cb cs
               (r, g, b, a) = px im 1 1
           in a == 255 && within 1 r (to8 er) && within 1 g (to8 eg) && within 1 b (to8 eb)
         | bm <- allBlends ])
  check "raster: every blend mode matches the general formula with a translucent source"
    (and [ let cb = (0.25, 0.7, 0.5); cs = (0.9, 0.4, 0.2); as = 0.6
               im = renderBg (let (r, g, b) = cb in Color r g b 1) 4 4
                      [CGroup defaultGroup { goBlend = bm, goIsolated = True } [solid (let (r, g, b) = cs in Color r g b as) (rectPath 0 0 4 4)]]
               (br, bg, bb) = refBlend bm cb cs
               mix c bcol = (1 - as) * c + as * bcol
               (cbr, cbg, cbb) = cb
               (r, g, b, _) = px im 2 2
           in within 1 r (to8 (mix cbr br)) && within 1 g (to8 (mix cbg bg)) && within 1 b (to8 (mix cbb bb))
         | bm <- allBlends ])
  check "raster: a blend over a transparent backdrop is the source"
    (let im = img0 4 4 [CGroup defaultGroup { goBlend = BlendMultiply, goIsolated = True } [solid (Color 0.2 0.4 0.6 1) (rectPath 0 0 4 4)]]
     in px im 1 1 == (51, 102, 153, 255))
  check "raster: a blended group of two shapes uses its layer"
    (let im = renderBg white 40 20 [CGroup defaultGroup { goBlend = BlendMultiply, goIsolated = True }
                                      [solid (Color 1 0 0 1) (rectPath 0 0 20 20), solid (Color 0 0 1 0.5) (rectPath 10 0 20 20)]]
         (r, g, b, _) = px im 15 5
     in within 1 r 128 && g == 0 && within 1 b 128)
  check "raster: a knockout group composites each child against the initial backdrop"
    (let kids = [solid (Color 1 0 0 0.5) (rectPath 0 0 20 20), solid (Color 0 0 1 0.5) (rectPath 10 0 20 20)]
         ko = img0 40 20 [CGroup defaultGroup { goKnockout = True, goIsolated = True } kids]
         plain = img0 40 20 [CGroup defaultGroup { goIsolated = True, goOpacity = 0.999999 } kids]
         (r, _, b, a) = px ko 15 5
     in within 1 a 128 && r == 0 && b == 255 && within 1 (alphaAt plain 15 5) 191)
  check "raster: a non-isolated group's blends see the backdrop, then the backdrop is removed"
    (let bd = Color 0 0 1 1
         kid = CGroup defaultGroup { goBlend = BlendMultiply, goIsolated = True } [solid (Color 1 1 0 1) (rectPath 0 0 10 10)]
         nonIso = renderBg bd 10 10 [CGroup defaultGroup { goOpacity = 0.5, goIsolated = False } [kid]]
         iso = renderBg bd 10 10 [CGroup defaultGroup { goOpacity = 0.5, goIsolated = True } [kid]]
         -- non-isolated: yellow × blue = black inside the group; then half over blue
         (r1, g1, b1, _) = px nonIso 5 5
         -- isolated: multiply against transparent = yellow; then half over blue
         (r2, g2, b2, _) = px iso 5 5
     in (r1, g1) == (0, 0) && within 1 b1 128 && within 1 r2 128 && within 1 g2 128 && within 1 b2 128)
  check "raster: a luminosity soft mask scales by the mask's luminance"
    (let sm = SoftMask 1 MaskLuminosity (Color 0 0 0 1) [solid white (rectPath 0 0 10 20), solid (Color 0.5 0.5 0.5 1) (rectPath 10 0 10 20)] Nothing
         im = img0 30 20 [CGroup defaultGroup { goMask = Just sm, goIsolated = True } [fillB (rectPath 0 0 30 20)]]
     in alphaAt im 5 5 == 255 && within 1 (alphaAt im 15 5) 128 && alphaAt im 25 5 == 0)
  check "raster: a luminosity mask's backdrop shows where the mask draws nothing"
    (let sm = SoftMask 2 MaskLuminosity (Color 1 1 1 1) [solid black (rectPath 0 0 10 20)] Nothing
         im = img0 20 20 [CGroup defaultGroup { goMask = Just sm, goIsolated = True } [fillB (rectPath 0 0 20 20)]]
     in alphaAt im 5 5 == 0 && alphaAt im 15 5 == 255)
  check "raster: an alpha soft mask uses the mask's alpha, and the transfer function"
    (let sm lut = SoftMask 3 MaskAlpha transparent [solid (Color 1 0 0 0.25) (rectPath 0 0 20 20)] lut
         inv = listArray (0, 255) [ 255 - fromIntegral i | i <- [0 .. 255 :: Int] ] :: UArray Int Word8
         a = img0 20 20 [CGroup defaultGroup { goMask = Just (sm Nothing), goIsolated = True } [fillB (rectPath 0 0 20 20)]]
         b = img0 20 20 [CGroup defaultGroup { goMask = Just (sm (Just inv)), goIsolated = True } [fillB (rectPath 0 0 20 20)]]
     in within 1 (alphaAt a 5 5) 64 && within 1 (alphaAt b 5 5) 191)
  check "raster: a translucent glyph run does not double its overlaps"
    (let g = Glyph identityA 1 (rectPath 0 0 12 10)
         im = img0 30 10 [CGlyphs identityA (PSolid (Color 0 0 0 0.5)) [g, Glyph (translateA 8 0) 1 (rectPath 0 0 12 10)]]
     in within 1 (alphaAt im 10 5) 128 && within 1 (alphaAt im 2 5) 128)
  check "raster: glyphs are filled non-zero one by one, so opposite windings do not cancel"
    (let cw = rectPath 0 0 10 10
         ccw = [MoveTo (Pt 5 0), LineTo (Pt 5 10), LineTo (Pt 15 10), LineTo (Pt 15 0), ClosePath]
         im = img0 20 10 [CGlyphs identityA (PSolid black) [Glyph identityA 1 cw, Glyph identityA 2 ccw]]
     in alphaAt im 7 5 == 255 && ink im == 150)

  -- Filters ---------------------------------------------------------------
  let filt prims = CGroup defaultGroup { goIsolated = True, goFilter = Just (Filter (Rect 0 0 60 60) identityA prims) }
      fprim op ins = FilterPrim op ins Nothing False 1
  check "raster: a blur spreads an edge but keeps the ink"
    (let im = img0 60 60 [filt [fprim (FeBlur 3 3) []] [fillB (rectPath 20 20 20 20)]]
     in near 6 (ink im) 400 && alphaAt im 18 30 > 30 && alphaAt im 30 30 > 240)
  check "raster: a small-deviation blur keeps the ink too"
    (near 3 (ink (img0 60 60 [filt [fprim (FeBlur 1 1) []] [fillB (rectPath 20 20 20 20)]])) 400)
  check "raster: an offset moves the source"
    (let im = img0 60 60 [filt [fprim (FeOffset 5 7) []] [fillB (rectPath 10 10 10 10)]]
     in alphaAt im 16 18 == 255 && alphaAt im 12 12 == 0 && ink im == 100)
  check "raster: a flood fills its subregion"
    (let im = img0 60 60 [filt [FilterPrim (FeFlood (Color 0 1 0 1)) [] (Just (Rect 10 10 30 20)) False 1] [fillB (rectPath 0 0 1 1)]]
     in ink im == 200 && px im 15 15 == (0, 255, 0, 255))
  check "raster: composite in keeps the flood where the source is"
    (let im = img0 60 60 [filt [fprim (FeFlood (Color 1 0 0 1)) [], FilterPrim (FeComposite CompIn) [Previous, SourceGraphic] Nothing False 2]
                            [fillB (rectPath 10 10 10 10)]]
     in px im 15 15 == (255, 0, 0, 255) && ink im == 100)
  check "raster: a colour matrix swaps channels"
    (let im = img0 60 60 [filt [fprim (FeColorMatrix [0,0,1,0,0, 0,1,0,0,0, 1,0,0,0,0, 0,0,0,1,0]) []] [solid (Color 1 0 0 1) (rectPath 10 10 10 10)]]
     in px im 15 15 == (0, 0, 255, 255))
  check "raster: merge composites inputs in order"
    (let im = img0 60 60 [filt [ FilterPrim (FeOffset 5 0) [SourceGraphic] Nothing False 1
                               , FilterPrim FeMerge [Result 1, SourceGraphic] Nothing False 2 ]
                            [solid (Color 0 0 1 1) (rectPath 10 10 10 10)]]
     in ink im == 150)
  check "raster: a drop shadow is the source over its blurred, offset, tinted alpha"
    (let im = img0 60 60 [filt [fprim (FeDropShadow 0.5 0.5 10 10 (Color 1 0 0 1)) []] [solid (Color 0 0 1 1) (rectPath 10 10 10 10)]]
     in px im 15 15 == (0, 0, 255, 255) && let (r, _, _, a) = px im 25 25 in r == 255 && a > 240)
  check "raster: linearRGB filters convert in and out"
    (let im = img0 60 60 [filt [FilterPrim (FeColorMatrix [1,0,0,0,0, 0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0]) [] Nothing True 1]
                            [solid (Color 0.5 0.5 0.5 1) (rectPath 10 10 10 10)]]
         (r, _, _, _) = px im 15 15
     in within 1 r 128)
  check "raster: a filter's output stays inside the filter region"
    (let im = img0 60 60 [CGroup defaultGroup { goIsolated = True, goFilter = Just (Filter (Rect 0 0 30 60) identityA [fprim (FeOffset 20 0) []]) }
                            [fillB (rectPath 5 5 10 10)]]
     in alphaAt im 27 10 == 255 && alphaAt im 31 10 == 0 && ink im == 50)

  check "raster: a filter with no source graphic still runs (a flood from nothing)"
    (let im = img0 60 60 [CGroup defaultGroup { goIsolated = True, goFilter = Just (Filter (Rect 10 10 30 20) identityA [fprim (FeFlood (Color 0 0 1 1)) []]) } []]
     in ink im == 200 && px im 15 15 == (0, 0, 255, 255))
  check "raster: a filter over invisible children still runs (an offset flood)"
    (let im = img0 60 60 [CGroup defaultGroup { goIsolated = True, goFilter = Just (Filter (Rect 0 0 40 40) identityA
                              [fprim (FeFlood (Color 1 0 0 1)) [], FilterPrim (FeOffset 20 20) [Previous] Nothing False 2]) }
                            [solid (Color 0 0 0 0) (rectPath 0 0 10 10)]]
     in ink im == 400 && alphaAt im 25 25 == 255 && alphaAt im 15 15 == 0)


  -- Milestone review fixes ------------------------------------------------
  check "raster: shapes crossing the left edge at many sub-pixel offsets keep their exact area"
    -- an edge collapsed onto the left side can step to just below x = 0;
    -- before the clamp that wrote one cell before the row
    (and [ let tri = [Pt (-3 - dx) (0.1 + dy), Pt (7 + dx) (0.1 + dy), Pt (0 - dx * 3) (9.7 - dy)]
               im = img0 12 12 [fillB (polyPath tri)]
           in near 0.05 (ink im) (clippedArea 12 12 tri)
         | i <- [0 .. 23 :: Int], let dx = fromIntegral i * 0.0417, let dy = fromIntegral (i `mod` 7) * 0.137 ])
  check "raster: the reviewer's left-edge edge (7,0.1)-(0,0.7) and its neighbours rasterise exactly"
    (and [ let tri = [Pt 7 (0.1 + d), Pt (-2) (0.7 + d), Pt (-2) (5 + d), Pt 7 (4 + d)]
           in near 0.05 (ink (img0 10 10 [fillB (polyPath tri)])) (clippedArea 10 10 tri)
         | k <- [0 .. 15 :: Int], let d = fromIntegral k / 16 ])
  check "raster: a rectangle drawn twice under even-odd is empty, edges included"
    (let r = rectPath 1.5 1 4 6
     in all (== 0) (rowAlphas (img0 8 8 [CFill identityA EvenOdd (PSolid black) (r ++ r)]) 3))
  check "raster: abutting opposite-winding rectangles leave no seam under non-zero"
    (let left = rectPath 0.5 1 3 6
         right = [MoveTo (Pt 3.5 1), LineTo (Pt 3.5 7), LineTo (Pt 6.5 7), LineTo (Pt 6.5 1), ClosePath]
     in rowAlphas (img0 8 8 [fillB (left ++ right)]) 3 == [128, 255, 255, 255, 255, 255, 128, 0])
  check "raster: a retraced stroke (M a L b Z) has single, not doubled, fringes"
    (let st = CStroke identityA defaultStroke { ssWidth = 2 } (PSolid black) [MoveTo (Pt 1 4.3), LineTo (Pt 7 4.3), ClosePath]
         im = img0 8 8 [st]
     in within 6 (alphaAt im 4 3) 179 && within 6 (alphaAt im 4 5) 77 && alphaAt im 4 4 == 255)
  check "raster: overlapping same-direction shapes whose tops share a row are not over-covered"
    (let im = img0 12 8 [fillB (rectPath 1 3.3 8 4 ++ rectPath 3 3.6 8 4)]
     -- row 3 over both: covered from y = 3.3 down, so 0.7, not 0.7 + 0.4
     in within 6 (alphaAt im 5 3) 179 && within 6 (alphaAt im 2 3) 179 && within 6 (alphaAt im 10 3) 102)
  check "raster: even-odd overlaps (pentagram, self-crossing) are still exact in area"
    (let starEO = img0 64 64 [CFill identityA EvenOdd (PSolid black) (pentagram 32 33 28)]
         starNZ = img0 64 64 [CFill identityA NonZero (PSolid black) (pentagram 32 33 28)]
         ri = 28 * cos (2 * pi / 5) / cos (pi / 5)
         pent = 5 / 2 * ri * ri * sin (2 * pi / 5)
     in abs ((ink starNZ - ink starEO) - pent) / pent < 0.01)
  check "raster: a knockout group's soft-edged child group erases siblings only by its shape"
    -- the child group's right edge half-covers column 10: there it may only
    -- half-replace the red, while inside it replaces the red outright
    (let kids = [ solid (Color 1 0 0 1) (rectPath 0 0 20 20)
                , CGroup defaultGroup { goIsolated = True }
                    [solid (Color 0 0 1 1) (rectPath 0 0 10.5 20), solid (Color 0 0 1 1) (rectPath 0 18 2 2)] ]
         im = img0 20 20 [CGroup defaultGroup { goKnockout = True, goIsolated = True } kids]
         (r, g, b, a) = px im 10 5
     in within 2 r 128 && g == 0 && within 2 b 128 && a == 255 && px im 5 5 == (0, 0, 255, 255) && px im 15 5 == (255, 0, 0, 255))
  check "raster: a huge miter limit at an exact reversal strokes like a bevel, with no NaN"
    (let p = [MoveTo (Pt 5 20), LineTo (Pt 40 20), LineTo (Pt 5 20)]
         at ml = img0 64 40 [CStroke identityA defaultStroke { ssWidth = 6, ssMiterLimit = ml } (PSolid black) p]
         a = at 1e300; b = at 10
     in imgPix a == imgPix b && ink a > 200 && ink a < 260)
  check "raster: a stroke whose outline is not finite is skipped, not rasterised"
    (let (im, st) = render 20 20 [CStroke identityA defaultStroke { ssWidth = 1e308 } (PSolid black) [MoveTo (Pt 5 5), LineTo (Pt 6 6)]]
     in ink im == 0 || rsSkipped st >= 1)
  check "raster: a zero-length dash exactly at an open path's end draws nothing there (as resvg)"
    (let ss = defaultStroke { ssWidth = 2, ssDash = [0, 3], ssCap = CapRound }
         im = img0 30 10 [CStroke identityA ss (PSolid black) (line 10.5 5.5 16.5 5.5)]
     in alphaAt im 10 5 > 200 && alphaAt im 13 5 > 200 && alphaAt im 16 5 == 0)
  check "raster: a cached glyph at a quantised phase is pixel-identical to the uncached one"
    (let g k = CGlyphs identityA (PSolid black) [ Glyph (translateA (fromIntegral i * 7 + 2.25) 3.5) k (circleP 3 3 2.6 ++ rectPath 1 6 4 1) | i <- [0 .. 5 :: Int] ]
     in imgPix (img0 48 12 [g 1]) == imgPix (img0 48 12 [g (-1)]))
  check "raster: a cached glyph at an arbitrary phase lands within an eighth of a pixel"
    (let g k = CGlyphs identityA (PSolid black) [ Glyph (translateA (fromIntegral i * 7 + 2.31) 3.43) k (circleP 3 3 2.6) | i <- [0 .. 5 :: Int] ]
         a = img0 48 12 [g 1]; b = img0 48 12 [g (-1)]
     in near 0.6 (ink a) (ink b) && maximum [ abs (fromIntegral (imgPix a ! i) - fromIntegral (imgPix b ! i)) :: Int | i <- [3, 7 .. 4 * 48 * 12 - 1] ] < 80)
  check "raster: many objects under one keyed soft mask equal the uncached rendering"
    (let sm k = SoftMask k MaskLuminosity (Color 0 0 0 1)
                  [CFill identityA NonZero (PLinear identityA (Pt 0 0) (Pt 40 0) [Stop 0 white, Stop 1 black] SpreadPad) (rectPath 0 0 40 40)] Nothing
         objs k = [ CGroup defaultGroup { goIsolated = True, goMask = Just (sm k) } [solid (Color 0 0.5 1 1) (rectPath (fromIntegral (i `mod` 8) * 5) (fromIntegral (i `div` 8) * 5) 4 4)]
                  | i <- [0 .. 63 :: Int] ]
     in imgPix (img0 40 40 (objs 9)) == imgPix (img0 40 40 (objs (-1))))
  check "raster: live memory never exceeds the cap, whatever the scene asks for"
    (let cap = 200 * 1000
         pats = [ CFill identityA NonZero (PTiling k identityA (Rect 0 0 9 9) 9 9 [solid (Color 0 0 1 1) (circleP 4.5 4.5 3)]) (rectPath 0 0 40 40)
                | k <- [1 .. 40] ]
         masked = [ CGroup defaultGroup { goIsolated = True, goMask = Just (SoftMask (-1) MaskAlpha transparent
                      [CGroup defaultGroup { goIsolated = True, goMask = Just (SoftMask (-1) MaskAlpha transparent [solid black (rectPath 0 0 40 40)] Nothing) }
                         [solid black (rectPath 0 0 30 30)]] Nothing) } [solid (Color 1 0 0 1) (rectPath 0 0 40 40)] ]
         (_, st) = renderScene (defaultRenderOpts 40 40 identityA) { roMaxLayerBytes = cap } (Scene (Rect 0 0 40 40) (pats ++ masked) [])
     in rsPeakBytes st <= cap && rsPeakBytes st >= 40 * 40 * 16)
  check "raster: cached tiles are evicted, not retained, past the cache's share"
    (let pats = [ CFill identityA NonZero (PTiling k identityA (Rect 0 0 9 9) 9 9 [solid (Color 0 0 1 1) (circleP 4.5 4.5 3)]) (rectPath 0 0 40 40)
                | k <- [1 .. 400] ]
         cap = 400 * 1000
         (im, st) = renderScene (defaultRenderOpts 40 40 identityA) { roMaxLayerBytes = cap } (Scene (Rect 0 0 40 40) pats [])
     in rsPeakBytes st <= cap && rsSkipped st == 0 && alphaAt im 4 4 == 255)
  check "raster: refused allocations are charged to the budget"
    (let groups = replicate 600 (CGroup defaultGroup { goIsolated = True, goOpacity = 0.5 } [fillB (rectPath 0 0 40 40), fillB (rectPath 1 1 5 5)])
         (_, st) = renderScene (defaultRenderOpts 40 40 identityA) { roMaxLayerBytes = 40 * 40 * 16, roBudget = 400000 }
                              (Scene (Rect 0 0 40 40) groups [])
     in rsSkipped st > 0 && rsExhausted st)
  check "raster: the canvas is capped at 16 MP and the memory cap defaults to about three canvases"
    (rsTooLarge (snd (renderScene (defaultRenderOpts 5000 4000 identityA) (emptyScene (Rect 0 0 1 1))))
     && defaultMaxLayerBytes 100 100 == 16 * 1024 * 1024
     && defaultMaxLayerBytes 1000 1000 == 48 * 1000 * 1000
     && defaultMaxLayerBytes 4000 4000 == 512 * 1024 * 1024)

  check "raster: feOffset under rotate(90) moves along the rotated axes (spec, not resvg)"
    -- user (12, 4) under a 90° rotation is device (-4, 12)
    (let fl prims = CGroup defaultGroup { goIsolated = True, goFilter = Just (Filter (Rect (-30) (-30) 30 30) identityA prims) }
         im = img0 60 60 [CTransform (composeA (translateA 30 30) (rotateA (pi / 2)))
                [fl [FilterPrim (FeOffset 12 4) [SourceGraphic] Nothing False 1] [fillB (rectPath (-5) (-5) 10 10)]]]
     -- the square (device 25..35 both ways) lands at x 21..31, y 37..47
     in alphaAt im 22 38 == 255 && alphaAt im 30 46 == 255 && alphaAt im 32 40 == 0 && alphaAt im 26 36 == 0 && ink im == 100)
  check "raster: feOffset and feDropShadow under scale(-1,1) move the other way"
    (let fl prims = CGroup defaultGroup { goIsolated = True, goFilter = Just (Filter (Rect (-30) (-30) 30 30) identityA prims) }
         flip = composeA (translateA 30 30) (scaleA (-1) 1)
         off = img0 60 60 [CTransform flip [fl [FilterPrim (FeOffset 12 4) [SourceGraphic] Nothing False 1] [fillB (rectPath (-5) (-5) 10 10)]]]
         ds = img0 60 60 [CTransform flip [fl [FilterPrim (FeDropShadow 0.3 0.3 12 4 (Color 1 0 0 1)) [] Nothing False 1] [solid (Color 0 0 1 1) (rectPath (-5) (-5) 10 10)]]]
     -- device square 25..35 shifts to x 13..23, y 29..39
     in alphaAt off 14 30 == 255 && alphaAt off 26 30 == 0 && ink off == 100
        && px ds 15 37 == (255, 0, 0, 255) && px ds 30 30 == (0, 0, 255, 255) && alphaAt ds 37 33 == 0)

  check "raster: a large image is downscaled in memory independent of its size"
    -- 1200×900 into 40×30 under a cap of the canvas plus 12 KB: the old
    -- separable pass needed 40 × 900 × 16 = 576 KB and refused the image
    (let big = Image 1200 900 "checker" (listArray (0, 1200 * 900 * 4 - 1)
                 (concat [ if even (x + y) then [255, 255, 255, 255] else [0, 0, 0, 255] | y <- [0 .. 899 :: Int], x <- [0 .. 1199 :: Int] ]))
         canvas = 40 * 30 * 16
         (im, st) = renderScene (defaultRenderOpts 40 30 identityA) { roMaxLayerBytes = canvas + 12000 }
                                (Scene (Rect 0 0 40 30) [CImage (Affine 40 0 0 30 0 0) big defaultImageOpts] [])
     in rsImagesDropped st == 0 && rsPeakBytes st <= canvas + 12000
        && all (\(x, y) -> let (r, _, _, a) = px im x y in within 1 r 128 && a == 255) [(0, 0), (20, 15), (39, 29)])
  check "raster: an image the memory cap refuses is counted and noted, never silently dropped"
    (let img = Image 400 400 "grey" (listArray (0, 400 * 400 * 4 - 1) (cycle [90, 90, 90, 255]))
         (_, st) = renderScene (defaultRenderOpts 20 20 identityA) { roMaxLayerBytes = 20 * 20 * 16 }
                               (Scene (Rect 0 0 20 20) [CImage (Affine 20 0 0 20 0 0) img defaultImageOpts] [])
     in rsImagesDropped st == 1 && any ("not drawn" `isIn`) (rsNotes st))
  check "raster: a straddling source row is shared exactly between two output rows"
    -- 3 rows into 2: the middle row is half in each, so row 0 averages
    -- rows 0 and 1 as (1 + 0.5·v1) / 1.5
    (let img = Image 1 3 "rows" (listArray (0, 11) [255, 255, 255, 255, 0, 0, 0, 255, 255, 255, 255, 255])
         im = img0 1 2 [CImage (Affine 1 0 0 2 0 0) img defaultImageOpts]
         (r0, _, _, _) = px im 0 0
     in within 1 r0 (to8 (1 / 1.5)))

  check "raster: cached glyphs at every quarter phase stay inside their run's tight mask"
    -- a translucent run composites through a mask sized from the exact
    -- outlines; a cached glyph drawn up to an eighth of a pixel left or up
    -- of its true position must not lose the coverage that falls a pixel
    -- outside (nor write outside the mask: the backstop clamp would hide it)
    (and [ let g k = CGlyphs identityA (PSolid (Color 0 0 0 0.5))
                       [Glyph (translateA (4 + dx) (3 + dy)) k (rectPath 0.95 0.97 2.06 2.03)]
               a = img0 10 9 [g 7]; b = img0 10 9 [g (-1)]
           in near 0.02 (ink a) (ink b) && near 0.05 (ink a) (0.5 * 2.06 * 2.03)
         | i <- [0 .. 15 :: Int], j <- [0, 5, 11 :: Int]
         , let dx = fromIntegral i / 16, let dy = fromIntegral j / 16 ])
  check "raster: a run of cached translucent glyphs equals the uncached run in ink"
    (let run k = CGlyphs identityA (PSolid (Color 0.2 0.1 0.6 0.4))
                   [ Glyph (translateA (fromIntegral i * 3.13 + 0.96) (1.97 + fromIntegral (i `mod` 3) * 0.37)) k (circleP 1.2 1.2 1.15)
                   | i <- [0 .. 11 :: Int] ]
     in near 0.3 (ink (img0 44 8 [run 3])) (ink (img0 44 8 [run (-1)])))

  check "raster: a huge mesh stops at the budget with the exhausted flag (not minutes later)"
    -- 6 000 canvas-sized ramp triangles: the colour pass charges each its
    -- box, and the coverage pass stops adding them once the budget is gone
    (let tris = [ RampTri (Pt (-5) (-5)) 0 (Pt 120 (fromIntegral (k `mod` 7))) 0.5 (Pt (fromIntegral (k `mod` 11)) 130) 1 | k <- [0 .. 5999 :: Int] ]
         (im, st) = renderScene (defaultRenderOpts 100 100 identityA) { roBudget = 300000 }
                                (Scene (Rect 0 0 100 100) [CGouraudRamp identityA [Stop 0 black, Stop 1 white] tris] [])
     in rsExhausted st && ink im >= 0 && any ("incomplete" `isIn`) (rsNotes st))
  check "raster: a mesh's coverage is the union of its triangles without the exactness repair"
    (let c = Color 0.3 0.6 0.2 1
         ts = concat [ [ GouraudTri (Pt x y) c (Pt (x + 2) y) c (Pt x (y + 2)) c
                       , GouraudTri (Pt (x + 2) y) c (Pt (x + 2) (y + 2)) c (Pt x (y + 2)) c ]
                     | i <- [0 .. 19 :: Int], j <- [0 .. 19 :: Int], let x = fromIntegral i * 2 + 0.3, let y = fromIntegral j * 2 + 0.7 ]
         im = img0 44 44 [CGouraud identityA ts]
     in near 0.5 (ink im) 1600 && alphaAt im 20 20 == 255)
  check "raster: thousands of overlapping contours in one row stay bounded (no quadratic repair)"
    -- every row crosses ~1 600 same-orientation edges: past the crossing
    -- cap the fast path stands, and with a small budget the fill stops
    (let rects = concat [ rectPath (fromIntegral (k `mod` 40) * 0.5) (fromIntegral (k `mod` 13)) 20 10 | k <- [0 .. 799 :: Int] ]
         full = render 40 30 [CFill identityA EvenOdd (PSolid black) rects]
         small = renderScene (defaultRenderOpts 40 30 identityA) { roBudget = 20000 }
                             (Scene (Rect 0 0 40 30) [CFill identityA EvenOdd (PSolid black) rects] [])
     in not (rsExhausted (snd full)) && rsExhausted (snd small))

  -- API helpers -----------------------------------------------------------
  check "raster: fitView centres a wide rectangle"
    (fitView (Rect 0 0 200 100) 100 100 == Affine 0.5 0 0 0.5 0 25
     && fitView (Rect 10 10 10 10) 50 50 == Affine 1 0 0 1 (-10) (-10))
  check "raster: dashPath yields the dashes as open subpaths"
    (length [ () | MoveTo _ <- dashPath [10, 10] 0 0.1 (line 0 0 100 0) ] == 5)
  check "raster: flattenPath stays within tolerance of a circle"
    (case flattenPath 0.05 (circleP 0 0 50) of
       [(ps, True)] -> all (\p -> let d = ptLen p in d >= 49.9 && d <= 50.05) ps && length ps > 16
       _ -> False)

  -- Bounds ----------------------------------------------------------------
  check "raster: a refused output size says so"
    (let (im, st) = renderScene (defaultRenderOpts 100000 100000 identityA) (emptyScene (Rect 0 0 1 1))
     in rsTooLarge st && imgW im == 0)
  check "raster: a zero-size output is refused, not crashed"
    (rsTooLarge (snd (renderScene (defaultRenderOpts 0 10 identityA) (emptyScene (Rect 0 0 1 1)))))
  check "raster: an exhausted budget returns what was drawn, flagged"
    (let cmds = [ fillB (rectPath (fromIntegral (i `mod` 10) * 4) (fromIntegral (i `div` 10) * 4) 4 4) | i <- [0 .. 99 :: Int] ]
         (im, st) = renderScene (defaultRenderOpts 40 40 identityA) { roBudget = 600 } (Scene (Rect 0 0 40 40) cmds [])
         (_, full) = render 40 40 cmds
     in rsExhausted st && ink im > 0 && ink im < 1600 && not (rsExhausted full) && rsCommands st < 100)
  check "raster: a group nested past the depth cap is skipped, not overflowed"
    (let deep = foldr (\_ k -> CGroup defaultGroup { goOpacity = 0.99, goIsolated = True } [k, fillB (rectPath 0 0 1 1)]) (fillB (rectPath 0 0 4 4)) [1 .. 200 :: Int]
         (im, st) = render 8 8 [deep]
     in rsSkipped st > 0 && ink im > 0)
  check "raster: non-finite and absurd coordinates draw nothing and throw nothing"
    (let bad = [ fillB [MoveTo (Pt (0 / 0) 1), LineTo (Pt 5 5), LineTo (Pt 1 9)]
               , fillB [MoveTo (Pt 1 1), LineTo (Pt (1 / 0) 5), LineTo (Pt 1 9)]
               , fillB [MoveTo (Pt 1e300 1), LineTo (Pt 5 5), LineTo (Pt 1 9)] ]
         (im, st) = render 10 10 bad
     in ink im == 0 && rsSkipped st == 3)
  check "raster: a singular matrix draws nothing"
    (ink (img0 10 10 [CFill (Affine 0 0 0 0 5 5) NonZero (PSolid black) (rectPath 0 0 5 5)
                     , CImage (Affine 1 2 2 4 0 0) checker defaultImageOpts
                     , CFill identityA NonZero (PLinear (Affine 0 0 0 0 0 0) (Pt 0 0) (Pt 1 0) [Stop 0 black, Stop 1 white] SpreadPad) (rectPath 0 0 5 5)]) == 0)
  check "raster: huge but finite coordinates still clip correctly"
    (let im = img0 20 20 [fillB [MoveTo (Pt (-1e11) (-1e11)), LineTo (Pt 1e11 (-1e11)), LineTo (Pt 1e11 10), LineTo (Pt (-1e11) 10), ClosePath]]
     in ink im == 200)
  r <- try (evaluate (fuzzSum 160)) :: IO (Either SomeException Double)
  check "raster: random and degenerate scenes never throw (160 scenes, fully evaluated)"
    (either (const False) (\v -> not (isNaN v)) r)
  where
    fuzzSum :: Int -> Double
    fuzzSum n = foldl' (\acc k ->
                  let (cmds, _) = cmdsG (Gen (fromIntegral k * 7919 + 13)) 6 0
                      (im, st) = renderScene (defaultRenderOpts 24 20 (Affine 1.5 0 0.2 1 1 2)) { roBudget = 200000 }
                                             (Scene (Rect 0 0 24 20) cmds [])
                      s = sum [ fromIntegral (imgPix im ! i) | i <- [0 .. 4 * imgW im * imgH im - 1] ] :: Double
                  in acc + s + fromIntegral (rsSegments st + rsPixels st + rsSkipped st)) 0 [1 .. n]
    _unused = (shiftR :: Word64 -> Int -> Word64, (.&.) :: Int -> Int -> Int)
