{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, MagicHash #-}
-- | The pixel engine under "Cmedit.Raster": edges in, premultiplied float
-- pixels out. Everything here is 'ST' and unboxed arrays; the scene walk,
-- paints-from-'Scene' and groups live in "Cmedit.Raster".
--
-- __Coverage is exact-area signed accumulation, kept sparse.__ A path is
-- flattened (device space, 'flatTol') into line edges clipped to the fill
-- box. Each edge deposits, row by row, the exact signed area it sweeps into
-- an accumulation buffer (the font-rs / stb_truetype formulation: one cell
-- receives the partial trapezoid, the next the remainder, and a running sum
-- along the row reproduces the area to the left of every edge). A prefix sum
-- along a row then gives, at each pixel, the integral of the winding number
-- over that pixel, from which non-zero takes @min 1 |a|@ and even-odd folds
-- @|a|@ modulo 2. Both rules are therefore exact wherever at most one edge
-- crosses a pixel and consistent where several do, and the topology
-- (overlapping subpaths, self-intersections, opposite windings) is right by
-- construction rather than by special cases.
--
-- __Where the integral is not enough, rows are resolved exactly.__ The
-- fill rule of the /integrated/ winding is the true coverage wherever the
-- winding inside a pixel takes only 0 and one value of a fixed sign; it is
-- wrong where it reaches ±2 or changes sign inside a pixel (a rectangle
-- drawn twice under even-odd folds "half of 2" to opaque; abutting
-- opposite-winding shapes cancel to a seam; a retraced stroke integrates
-- past 1). Each edge carries its contour's index range and orientation, so
-- two cheap signals find the rows at risk: a cell where two unrelated edges
-- of the same direction deposit area (a contour's own consecutive chords,
-- or two edges of opposite direction, are harmless), and a row in which two
-- contours of the same orientation overlap in x (horizontal edges deposit
-- nothing, so overlapping tops in one row show in no cell). Opposite
-- orientations, which is every font counter and every closed stroke's two
-- offsets, raise neither. A flagged row's true winding is sampled on four
-- sub-scanlines; only if it leaves {0, s} is the row recomputed on sixteen
-- with exact horizontal coverage ('resolveRow'). Text never gets that far.
--
-- Three things keep it sparse. Rows are processed in bands of 'bandH', so
-- the buffer is @(box width + 2) × bandH@ and never a mask of the path;
-- edges are bucketed by the band they start in and carried in an active
-- list, so each is visited once per band it spans. Each band row records
-- the leftmost and rightmost cell any edge touched, and the sweep reads
-- (and re-zeroes) only that range, so a thin diagonal across the page costs
-- its length, not the page. When the running sum is still non-zero at the
-- right end of the touched range (a shape whose right edge was clipped away)
-- the rest of the row is a constant run. Edges left of the box are not
-- dropped but collapsed onto its left edge, since everything to their right
-- still depends on their winding; edges right of the box are dropped, since
-- nothing visible does.
--
-- __Compositing is premultiplied 'Float' in sRGB values__ (no gamma, as PDF
-- and SVG specify and pdftoppm/resvg do). A paint is a 'Shader', evaluated
-- per run of non-zero coverage into a colour row, then blended into a
-- 'Layer'. The solid, normal-blend, plain-layer case (all text) has its own
-- loop; everything else goes through 'compositePx', which knows the 16
-- blend modes (W3C Compositing Level 1 formulas, including the
-- non-separable four), knockout layers and the group-alpha channel that a
-- non-isolated group needs to remove its backdrop again.
--
-- __Every hot loop is a local tail-recursive function over unboxed reads__,
-- per the 0032 lessons: no 'ST' helper returns a boxed number on the
-- per-pixel path ('emitRow' is called with the row's end column instead of
-- the sweep returning it), and the coverage rule is passed to an @INLINE@
-- sweep so each rule gets its own specialised loop.
module Cmedit.RasterCore
  ( -- * Small types
    C4(..)
  , IBox(..)
  , iboxEmpty
  , iboxIntersect
  , iboxW
  , iboxH
  , rectToIBox
    -- * Gradients and textures
  , Stops(..)
  , mkStops
  , stopsColor
  , spreadT
  , Tex(..)
  , sampleTex
  , sampleMaskTex
  , Radial(..)
  , radialT
    -- * Shaders, clips, layers, sinks
  , Shader(..)
  , Clip(..)
  , clipIBox
  , Layer(..)
  , newLayer
  , layerBytes
  , Sink(..)
  , Stencil(..)
    -- * The engine
  , Env(..)
  , newEnv
  , flatTol
  , beginFill
  , addEdge
  , flattenFill
  , addPolygon
  , rasterFill
  , emitMask
  , emitMaskAt
  , charge
  , budgetLeft
  , countSkip
    -- * Compositing
  , compositePx
  , blendOver
  , compositeLayer
  , fillLayer
  , copyRegion
  , layerToImage
  , clipRow
  , slotBudget, slotSegs, slotPixels, slotExhausted, slotSkipped, slotLayerBytes, slotCmds, slotPeakBytes, slotImagesDropped, slotNoResolve
    -- * Array access (bounds-checked under @-DRASTER_SAFE@)
  , rdA
  , wrA
  , atA
  , rgbaWord
  ) where

import Control.Monad (when, unless, forM_)
import Control.Monad.ST (ST)
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt, getNumElements, unsafeFreeze)
import Data.Array.ST (STUArray, newArray)
import Data.Array.MArray (MArray)
import Data.Array.Unboxed (UArray, listArray)
import Data.Array.IArray (IArray)
import Data.Array.Base (UArray(..))
import GHC.ByteOrder (ByteOrder(..), targetByteOrder)
import GHC.Exts (Int(I#), indexWord32Array#)
import GHC.Word (Word32(W32#))
#ifdef RASTER_SAFE
import Data.Array.MArray (readArray, writeArray)
import Data.Array.IArray ((!))
#endif
import Data.STRef
import Data.Word (Word8)
import GHC.Float (double2Float)

import Cmedit.Geom
import Cmedit.Image (Image(..))
import Cmedit.Scene (Blend(..), FillRule(..), Spread(..), Stop(..), Color(..))

------------------------------------------------------------------------------
-- Array access

-- Every array access in the raster modules goes through these three. The
-- normal build is unchecked (these are the hot loops); building with
-- @-DRASTER_SAFE@ (see @docs/plans/bench/rastercheck.sh safe@) makes every
-- one bounds-checked, so an index arithmetic slip is an exception in that
-- build rather than silent memory corruption in this one. All arrays here
-- are 0-based, so the checked and unchecked index mean the same thing.
rdA :: MArray (STUArray s) e (ST s) => STUArray s Int e -> Int -> ST s e
wrA :: MArray (STUArray s) e (ST s) => STUArray s Int e -> Int -> e -> ST s ()
atA :: IArray UArray e => UArray Int e -> Int -> e
#ifdef RASTER_SAFE
rdA = readArray
wrA = writeArray
atA = (!)
#else
rdA = unsafeRead
wrA = unsafeWrite
atA = unsafeAt
#endif
{-# INLINE rdA #-}
{-# INLINE wrA #-}
{-# INLINE atA #-}

-- | Pixel @k@ of straight RGBA bytes as one word, red in the low byte,
-- alpha in the high one. One aligned load instead of four: the image
-- prescaler reads every pixel of a source that can be tens of megapixels.
-- (A big-endian host takes the byte path; the result is the same.)
rgbaWord :: UArray Int Word8 -> Int -> Word
rgbaWord arr@(UArray _ _ _ ba) k@(I# k#)
#ifdef RASTER_SAFE
  | 4 * k + 3 >= numElementsU arr || k < 0 = error ("rgbaWord: pixel " ++ show k ++ " out of range")
#endif
  | targetByteOrder == LittleEndian = fromIntegral (W32# (indexWord32Array# ba k#))
  | otherwise =
      let b i = fromIntegral (atA arr (4 * k + i)) :: Word
      in b 0 + b 1 * 256 + b 2 * 65536 + b 3 * 16777216
  where _ = arr
{-# INLINE rgbaWord #-}

#ifdef RASTER_SAFE
numElementsU :: UArray Int Word8 -> Int
numElementsU (UArray _ _ n _) = n
#endif

------------------------------------------------------------------------------
-- Small types

-- | A premultiplied colour (or any four floats).
data C4 = C4 {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  deriving (Eq, Show)

data C3 = C3 {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float

-- | An integer pixel box, @x0 y0 x1 y1@, exclusive of the far edges.
data IBox = IBox {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

iboxEmpty :: IBox -> Bool
iboxEmpty (IBox x0 y0 x1 y1) = x1 <= x0 || y1 <= y0

iboxIntersect :: IBox -> IBox -> IBox
iboxIntersect (IBox a b c d) (IBox a' b' c' d') = IBox (max a a') (max b b') (min c c') (min d d')

iboxW, iboxH :: IBox -> Int
iboxW (IBox x0 _ x1 _) = x1 - x0
iboxH (IBox _ y0 _ y1) = y1 - y0

-- | The pixels a real rectangle touches. Coordinates are clamped to a range
-- no layer can exceed, so a huge or non-finite rectangle is merely large.
rectToIBox :: Rect -> IBox
rectToIBox (Rect x0 y0 x1 y1) = IBox (fl x0) (fl y0) (ce x1) (ce y1)
  where
    lim = 1e8
    cl v | isNaN v = 0
         | v < -lim = -lim
         | v > lim = lim
         | otherwise = v
    fl v = floor (cl v)
    ce v = ceiling (cl v)

------------------------------------------------------------------------------
-- Gradient stops

-- | A gradient ramp: count, offsets (non-decreasing, in [0,1]) and straight
-- RGBA (four per stop, clamped to [0,1]).
data Stops = Stops !Int !(UArray Int Double) !(UArray Int Float)
  deriving (Show)

mkStops :: [Stop] -> Stops
mkStops ss =
  let offs = monotone 0 [ clamp01 o | Stop o _ <- ss ]
      cols = concat [ [f r, f g, f b, f a] | Stop _ (Color r g b a) <- ss ]
      n = length offs
      f v = double2Float (clamp01 v)
  in Stops n (listArray (0, max 0 (n - 1)) (if n == 0 then [0] else offs))
             (listArray (0, max 3 (4 * n - 1)) (if n == 0 then [0, 0, 0, 0] else cols))
  where
    monotone _ [] = []
    monotone lo (o : os) = let o' = max lo o in o' : monotone o' os
    clamp01 v | v >= 1 = 1
              | v > 0 = v
              | otherwise = 0          -- also NaN

-- | The premultiplied colour of a ramp at @t@ (already spread into [0,1]).
stopsColor :: Stops -> Double -> C4
stopsColor (Stops n offs cols) t
  | n <= 0 = C4 0 0 0 0
  | t <= atA offs 0 = col 0
  | t >= atA offs (n - 1) = col (n - 1)
  | n <= 8 = go 0
  | otherwise = go (search 0 (n - 1))
  where
    -- PDF shadings arrive sampled into as many as 256 stops, and a mesh
    -- looks one up per pixel of every triangle: find the segment by
    -- bisection (largest i with offs[i] <= t), then interpolate as before.
    search !lo !hi
      | hi - lo <= 1 = lo
      | otherwise = let mid = (lo + hi) `quot` 2
                    in if atA offs mid <= t then search mid hi else search lo mid
    col i = let a = atA cols (4 * i + 3)
            in C4 (atA cols (4 * i) * a) (atA cols (4 * i + 1) * a)
                  (atA cols (4 * i + 2) * a) a
    go !i
      | i + 1 >= n = col (n - 1)
      | t < atA offs (i + 1) =
          let o0 = atA offs i; o1 = atA offs (i + 1)
              f = double2Float ((t - o0) / (o1 - o0))
              lerp k = let a = atA cols (4 * i + k); b = atA cols (4 * i + 4 + k)
                       in a + (b - a) * f
              al = lerp 3
          in C4 (lerp 0 * al) (lerp 1 * al) (lerp 2 * al) al
      | otherwise = go (i + 1)

-- | Map a gradient parameter through a spread; NaN means "transparent".
spreadT :: Spread -> Double -> Double
spreadT sp t = case sp of
  SpreadPad -> clampT t
  SpreadReflect -> let u = t - 2 * fromIntegral (floorI (t * 0.5))
                   in if u > 1 then 2 - u else u
  SpreadRepeat -> t - fromIntegral (floorI t)
  SpreadNone -> if t < 0 || t > 1 then nan else t
  SpreadPadStart -> if t > 1 then nan else max 0 t
  SpreadPadEnd -> if t < 0 then nan else min 1 t
  where
    clampT v | v <= 0 = 0
             | v >= 1 = 1
             | otherwise = v
{-# INLINE spreadT #-}

nan :: Double
nan = 0 / 0

floorI :: Double -> Int
floorI v | v > -1e15 && v < 1e15 = floor v
         | otherwise = 0
{-# INLINE floorI #-}

------------------------------------------------------------------------------
-- Two-point conical gradients

-- | Start circle (centre, radius), end circle, in gradient space.
data Radial = Radial !Double !Double !Double !Double !Double !Double
  deriving (Show)

-- | The largest @t@ whose circle passes through the point with a
-- non-negative radius, among those the spread allows (see "Cmedit.Scene");
-- NaN when there is none.
radialT :: Radial -> Spread -> Double -> Double -> Double
radialT (Radial cx0 cy0 r0 cx1 cy1 r1) sp px py =
  let cdx = cx1 - cx0; cdy = cy1 - cy0; dr = r1 - r0
      a = cdx * cdx + cdy * cdy - dr * dr
      pdx = px - cx0; pdy = py - cy0
      b = pdx * cdx + pdy * cdy + r0 * dr
      c = pdx * pdx + pdy * pdy - r0 * r0
      scale = cdx * cdx + cdy * cdy + dr * dr
      ok t = r0 + t * dr >= 0 && allowed t
      allowed t = case sp of
        SpreadNone -> t >= 0 && t <= 1
        SpreadPadStart -> t <= 1
        SpreadPadEnd -> t >= 0
        _ -> True
  in if abs a <= 1e-9 * scale
       then if abs b <= 1e-12 * (scale + 1) then nan
            else let t = c / (2 * b) in if ok t then t else nan
       else let disc = b * b - a * c
            in if disc < 0 then nan
               else let sq = sqrt disc
                        t1 = (b + sq) / a; t2 = (b - sq) / a
                        hi = max t1 t2; lo = min t1 t2
                    in if ok hi then hi else if ok lo then lo else nan
{-# INLINE radialT #-}

------------------------------------------------------------------------------
-- Textures

-- | An image to sample: straight RGBA bytes (as decoded), premultiplied
-- floats (prescaled or rendered), or single-channel masks.
data Tex
  = Tex8 !Int !Int !(UArray Int Word8)
  | TexF !Int !Int !(UArray Int Float)
  | Mask8 !Int !Int !(UArray Int Word8)
  | MaskF !Int !Int !(UArray Int Float)

instance Show Tex where
  show t = case t of
    Tex8 w h _ -> "Tex8 " ++ show w ++ "x" ++ show h
    TexF w h _ -> "TexF " ++ show w ++ "x" ++ show h
    Mask8 w h _ -> "Mask8 " ++ show w ++ "x" ++ show h
    MaskF w h _ -> "MaskF " ++ show w ++ "x" ++ show h

texDims :: Tex -> (Int, Int)
texDims t = case t of
  Tex8 w h _ -> (w, h)
  TexF w h _ -> (w, h)
  Mask8 w h _ -> (w, h)
  MaskF w h _ -> (w, h)

-- Edge modes: 0 clamp, 1 repeat, 2 reflect, 3 transparent outside.
edgeIx :: Int -> Int -> Int -> Int
edgeIx mode n i = case mode of
  1 -> let m = i `mod` n in m
  2 -> let p = 2 * n; m = i `mod` p in if m < n then m else p - 1 - m
  _ -> if i < 0 then 0 else if i >= n then n - 1 else i
{-# INLINE edgeIx #-}

fetchC :: Tex -> Int -> Int -> C4
fetchC tex x y = case tex of
  Tex8 w _ px ->
    let i = (y * w + x) * 4
        a = fromIntegral (atA px (i + 3)) * (1 / 255) :: Float
        k = a * (1 / 255)
    in C4 (fromIntegral (atA px i) * k) (fromIntegral (atA px (i + 1)) * k)
          (fromIntegral (atA px (i + 2)) * k) a
  TexF w _ px ->
    let i = (y * w + x) * 4
    in C4 (atA px i) (atA px (i + 1)) (atA px (i + 2)) (atA px (i + 3))
  Mask8 w _ px -> let v = fromIntegral (atA px (y * w + x)) * (1 / 255) in C4 v v v v
  MaskF w _ px -> let v = atA px (y * w + x) in C4 v v v v
{-# INLINE fetchC #-}

-- | Sample a colour texture at texel coordinates @(u, v)@ (texel @i@ spans
-- @[i, i+1)@): edge mode, nearest-or-bilinear. Premultiplied result.
sampleTex :: Tex -> Int -> Bool -> Double -> Double -> C4
sampleTex tex mode nearest u v
  | w <= 0 || h <= 0 = C4 0 0 0 0
  | mode == 3 && (not (u >= 0) || not (v >= 0) || u > fromIntegral w || v > fromIntegral h) = C4 0 0 0 0
  | nearest =
      fetchC tex (edgeIx mode w (floorI u)) (edgeIx mode h (floorI v))
  | otherwise =
      let fu = u - 0.5; fv = v - 0.5
          x0 = floorI fu; y0 = floorI fv
          ax = double2Float (fu - fromIntegral x0); ay = double2Float (fv - fromIntegral y0)
          xa = edgeIx mode w x0; xb = edgeIx mode w (x0 + 1)
          ya = edgeIx mode h y0; yb = edgeIx mode h (y0 + 1)
          C4 r00 g00 b00 a00 = fetchC tex xa ya
          C4 r10 g10 b10 a10 = fetchC tex xb ya
          C4 r01 g01 b01 a01 = fetchC tex xa yb
          C4 r11 g11 b11 a11 = fetchC tex xb yb
          l p q = p + (q - p) * ax
          m p q = p + (q - p) * ay
      in C4 (m (l r00 r10) (l r01 r11)) (m (l g00 g10) (l g01 g11))
            (m (l b00 b10) (l b01 b11)) (m (l a00 a10) (l a01 a11))
  where (w, h) = texDims tex

-- | Sample a single-channel mask (clamped edges).
sampleMaskTex :: Tex -> Bool -> Double -> Double -> Float
sampleMaskTex tex nearest u v = let C4 _ _ _ a = sampleTex tex 0 nearest u v in a
{-# INLINE sampleMaskTex #-}

------------------------------------------------------------------------------
-- Shaders, clips, layers

-- | A paint resolved to device space. Matrices map device pixel centres
-- into the paint's own space.
data Shader
  = ShSolid !Float !Float !Float !Float                -- ^ Premultiplied.
  | ShLinear !Double !Double !Double !Stops !Spread     -- ^ @t = a x + b y + c@ at the pixel centre.
  | ShRadial !Affine !Radial !Stops !Spread
  | ShTex !Affine !Tex !Int !Bool !Float                -- ^ Device → texel coordinates, edge mode, nearest, opacity.
  | ShScratch !Int !Int !Int !Int !(UArray Int Float)   -- ^ Per-device-pixel premultiplied colours over a box (x0, y0, width, height); transparent outside it.
  | ShNone
  deriving (Show)

-- | The current clip: a real rectangle (the common case, and the fast
-- path), or a coverage mask over an integer box.
data Clip s
  = ClipRect !Double !Double !Double !Double
  | ClipMask !Int !Int !Int !Int !(STUArray s Int Float)   -- ^ x0 y0 w h, coverage.

clipIBox :: Clip s -> IBox
clipIBox (ClipRect x0 y0 x1 y1) = rectToIBox (Rect x0 y0 x1 y1)
clipIBox (ClipMask x y w h _) = IBox x y (x + w) (y + h)

-- | A premultiplied RGBA float surface over a device box. A knockout layer
-- carries its initial backdrop; a non-isolated group's layer carries its
-- own alpha (shape accumulation) so the backdrop can be removed again.
data Layer s = Layer
  { lyX     :: !Int
  , lyY     :: !Int
  , lyW     :: !Int
  , lyH     :: !Int
  , lyPix   :: !(STUArray s Int Float)
  , lyKnock :: !(Maybe (STUArray s Int Float))
  , lyGA    :: !(Maybe (STUArray s Int Float))
  }

newLayer :: IBox -> ST s (Layer s)
newLayer (IBox x0 y0 x1 y1) = do
  let w = max 0 (x1 - x0); h = max 0 (y1 - y0)
  pix <- newArray (0, max 1 (w * h * 4) - 1) 0
  return (Layer x0 y0 w h pix Nothing Nothing)

layerBytes :: IBox -> Int
layerBytes b = max 0 (iboxW b) * max 0 (iboxH b) * 16

-- | A coverage-mask stencil: device → mask texel coordinates, the mask, and
-- whether to sample it nearest.
data Stencil = Stencil !Affine !Tex !Bool

-- | Where a rasterised row goes.
data Sink s
  = SkPaint !(Layer s) !Shader !Blend !Float !(Maybe Stencil)
    -- ^ Paint with a shader into a layer: blend mode, opacity, optional stencil.
  | SkMask !Int !Int !Int !(STUArray s Int Float)
    -- ^ Union coverage into a mask over a box (x0, y0, width): @m = min 1 (m + c)@.

------------------------------------------------------------------------------
-- The engine state

-- | Reusable buffers and counters for one render.
data Env s = Env
  { envI     :: !(STUArray s Int Int)
  , envD     :: !(STUArray s Int Double)
  , envEdges :: !(STRef s (STUArray s Int Double))
  , envAcc   :: !(STRef s (STUArray s Int Double))
  , envRMin  :: !(STUArray s Int Int)
  , envRMax  :: !(STUArray s Int Int)
  , envCov   :: !(STRef s (STUArray s Int Float))
  , envCol   :: !(STRef s (STUArray s Int Float))
  , envIdx   :: !(STRef s (STUArray s Int Int))
  , envAct   :: !(STRef s (STUArray s Int Int))
  , envCnt   :: !(STRef s (STUArray s Int Int))
  , envCon   :: !(STRef s (STUArray s Int Int))     -- ^ Per edge: its contour's first and one-past-last edge index, and orientation (±1).
  , envOwn   :: !(STRef s (STUArray s Int Int))     -- ^ Per accumulation cell: last edge to deposit (-1 none, -2 conflict).
  , envFlag  :: !(STUArray s Int Int)               -- ^ Per band row: needs the winding check (two contours, or a cell conflict).
  , envRowC  :: !(STUArray s Int Double)            -- ^ Per band row, 8 values: current contour, its x-extent and orientation, and the x-extents of earlier contours of each orientation.
  , envRowE  :: !(STRef s (STUArray s Int Int))     -- ^ Scratch: edges crossing a resolved row.
  , envXs    :: !(STRef s (STUArray s Int Double))  -- ^ Scratch: sub-scanline crossings (x, direction).
  }

slotEdges, slotBudget, slotSegs, slotPixels, slotExhausted, slotSkipped, slotLayerBytes, slotCmds :: Int
slotEdges = 0
slotBudget = 1
slotSegs = 2
slotPixels = 3
slotExhausted = 4
slotSkipped = 5
slotLayerBytes = 6
slotCmds = 7

slotConStart, slotPeakBytes, slotImagesDropped, slotNoResolve :: Int
slotConStart = 8
slotPeakBytes = 9
slotImagesDropped = 10
slotNoResolve = 11

-- | Rows per accumulation band.
bandH :: Int
bandH = 32

-- | Curve flattening tolerance in device pixels. Chords always cut inside a
-- convex curve, so this is a bias, not noise: at 0.05 a 3-pixel-radius
-- circle lost 1.6% of its area (text visibly lighter); 0.02 costs ~10% more
-- edges on a page of text and loses about 0.6%.
flatTol :: Double
flatTol = 0.02

newEnv :: Int -> ST s (Env s)
newEnv budget = do
  ints <- newArray (0, 15) 0
  wrA ints slotBudget budget
  dbl <- newArray (0, 7) 0
  edges <- newArray (0, 5 * 256 - 1) 0 >>= newSTRef
  acc <- newArray (0, 1023) 0 >>= newSTRef
  rmin <- newArray (0, bandH - 1) maxBound
  rmax <- newArray (0, bandH - 1) (-1)
  cov <- newArray (0, 1023) 0 >>= newSTRef
  col <- newArray (0, 4095) 0 >>= newSTRef
  idx <- newArray (0, 255) 0 >>= newSTRef
  act <- newArray (0, 255) 0 >>= newSTRef
  cnt <- newArray (0, 63) 0 >>= newSTRef
  con <- newArray (0, 511) 0 >>= newSTRef
  own <- newArray (0, 1023) (-1) >>= newSTRef
  flg <- newArray (0, bandH - 1) 0
  rowC <- newArray (0, 8 * bandH - 1) (0 :: Double)
  forM_ [0 .. bandH - 1] $ \r -> resetRowC rowC r
  rowE <- newArray (0, 255) 0 >>= newSTRef
  xs <- newArray (0, 511) 0 >>= newSTRef
  return (Env ints dbl edges acc rmin rmax cov col idx act cnt con own flg rowC rowE xs)

-- Grow a buffer to at least n elements. Contents are not preserved unless
-- asked (only the edge buffer needs them).
growD :: STRef s (STUArray s Int Double) -> Int -> Bool -> ST s (STUArray s Int Double)
growD ref n keep = do
  a <- readSTRef ref
  cap <- getNumElements a
  if cap >= n then return a else do
    b <- newArray (0, max n (2 * cap) - 1) 0
    when keep $ copyLoop a b 0 cap
    writeSTRef ref b
    return b
  where
    copyLoop a b !i !m | i >= m = return ()
                       | otherwise = rdA a i >>= wrA b i >> copyLoop a b (i + 1) m

growF :: STRef s (STUArray s Int Float) -> Int -> ST s (STUArray s Int Float)
growF ref n = do
  a <- readSTRef ref
  cap <- getNumElements a
  if cap >= n then return a else do
    b <- newArray (0, max n (2 * cap) - 1) 0
    writeSTRef ref b
    return b

growI :: STRef s (STUArray s Int Int) -> Int -> ST s (STUArray s Int Int)
growI ref n = growIWith ref n 0 False

-- Grow an Int buffer, new cells holding a fill value, optionally keeping the
-- old contents.
growIWith :: STRef s (STUArray s Int Int) -> Int -> Int -> Bool -> ST s (STUArray s Int Int)
growIWith ref n fill keep = do
  a <- readSTRef ref
  cap <- getNumElements a
  if cap >= n then return a else do
    b <- newArray (0, max n (2 * cap) - 1) fill
    let copyLoop !i | i >= cap = return ()
                    | otherwise = rdA a i >>= wrA b i >> copyLoop (i + 1)
    when keep $ copyLoop 0
    writeSTRef ref b
    return b

-- | Spend budget.
charge :: Env s -> Int -> ST s ()
charge env k = do
  b <- rdA (envI env) slotBudget
  wrA (envI env) slotBudget (b - k)
{-# INLINE charge #-}

budgetLeft :: Env s -> ST s Bool
budgetLeft env = do
  b <- rdA (envI env) slotBudget
  if b > 0 then return True else do
    wrA (envI env) slotExhausted 1
    return False

countSkip :: Env s -> ST s ()
countSkip env = do
  k <- rdA (envI env) slotSkipped
  wrA (envI env) slotSkipped (k + 1)

------------------------------------------------------------------------------
-- Edges

-- | Start a fill: empty the edge buffer.
beginFill :: Env s -> ST s ()
beginFill env = do
  wrA (envI env) slotEdges 0
  wrA (envI env) slotConStart 0
  wrA (envD env) 2 0
{-# INLINE beginFill #-}

resetRowC :: STUArray s Int Double -> Int -> ST s ()
resetRowC rc r = do
  let o = 8 * r
  wrA rc o (-1)
  wrA rc (o + 4) 1e300
  wrA rc (o + 5) (-1e300)
  wrA rc (o + 6) 1e300
  wrA rc (o + 7) (-1e300)

-- | Close the current contour: every edge added since the last call is
-- tagged with the contour's index range. Coverage uses it to tell a curve's
-- own consecutive segments (which may share a pixel harmlessly) from two
-- unrelated parts of the outline meeting in one pixel (which may not; see
-- 'resolveRow').
endContour :: Env s -> ST s ()
endContour env = do
  start <- rdA (envI env) slotConStart
  n <- rdA (envI env) slotEdges
  area <- rdA (envD env) 2
  wrA (envD env) 2 0
  con <- growIWith (envCon env) (3 * n + 3) 0 True
  let sg = if area < 0 then -1 else 1 :: Int
      go !e | e >= n = return ()
            | otherwise = do
                wrA con (3 * e) start
                wrA con (3 * e + 1) n
                wrA con (3 * e + 2) sg
                go (e + 1)
  go start
  wrA (envI env) slotConStart n

-- | Add one line edge in box-relative coordinates (box @w × h@). The edge is
-- clipped to the box rows and to its right edge, and anything left of the
-- box is collapsed onto the left edge (see the module header).
addEdge :: Env s -> Double -> Double -> Double -> Double -> Double -> Double -> ST s ()
addEdge env !wd !hd !x0 !y0 !x1 !y1 = do
  -- the contour's orientation, from its unclipped geometry
  a <- rdA (envD env) 2
  wrA (envD env) 2 (a + (x0 * y1 - x1 * y0))
  addEdge' env wd hd x0 y0 x1 y1

addEdge' :: Env s -> Double -> Double -> Double -> Double -> Double -> Double -> ST s ()
addEdge' env !wd !hd !x0 !y0 !x1 !y1
  -- every caller screens coordinates first; this is the backstop that keeps
  -- a NaN out of the edge buffer whatever a future caller does
  | not (abs x0 < 1e13 && abs y0 < 1e13 && abs x1 < 1e13 && abs y1 < 1e13) = return ()
  | y0 == y1 = return ()
  | y0 < y1 = clipY 1 x0 y0 x1 y1
  | otherwise = clipY (-1) x1 y1 x0 y0
  where
    clipY !dir !xa !ya !xb !yb
      | yb <= 0 || ya >= hd = return ()
      | otherwise =
          let k = (xb - xa) / (yb - ya)
              (xa', ya') = if ya < 0 then (xa - ya * k, 0) else (xa, ya)
              (xb', yb') = if yb > hd then (xa + (hd - ya) * k, hd) else (xb, yb)
          in clipX dir xa' ya' xb' yb'
    clipX !dir !xa !ya !xb !yb
      | not (ya < yb) = return ()
      | lo >= wd = return ()
      | hi <= 0 = put dir 0 ya 0 yb
      | lo >= 0 && hi <= wd = put dir xa ya xb yb
      | otherwise =
          -- Straddles a vertical box edge: split at x = 0 and/or x = w.
          let yAt xv = ya + (xv - xa) * (yb - ya) / (xb - xa)
              xAt yv = xa + (yv - ya) * (xb - xa) / (yb - ya)
              cuts = [ yAt 0 | lo < 0, hi > 0 ] ++ [ yAt wd | lo < wd, hi > wd ]
              ys = ya : sortPair (filter (\y -> y > ya && y < yb) cuts) ++ [yb]
          in pieces dir xAt ys
      where lo = min xa xb; hi = max xa xb
    pieces !dir xAt (u : rest@(v : _)) = do
      let xm = xAt ((u + v) / 2)
      if xm <= 0 then put dir 0 u 0 v
      else if xm >= wd then return ()
      else put dir (clampX (xAt u)) u (clampX (xAt v)) v
      pieces dir xAt rest
    pieces _ _ _ = return ()
    clampX x = if x < 0 then 0 else if x > wd then wd else x
    sortPair [a, b] | b < a = [b, a]
    sortPair l = l
    put !dir !xa !ya !xb !yb = do
      n <- rdA (envI env) slotEdges
      buf <- growD (envEdges env) (5 * n + 5) True
      let o = 5 * n
      wrA buf o (clampX xa)
      wrA buf (o + 1) ya
      wrA buf (o + 2) (clampX xb)
      wrA buf (o + 3) yb
      wrA buf (o + 4) dir
      wrA (envI env) slotEdges (n + 1)
      s <- rdA (envI env) slotSegs
      wrA (envI env) slotSegs (s + 1)
      charge env 1

-- | Flatten a path through a device matrix into the edge buffer, relative
-- to a box at @(bx, by)@ of size @w × h@. Each subpath is closed implicitly.
-- Returns 'False' (and adds nothing useful) when a coordinate is not finite
-- or is beyond any sane range, or when the budget runs out part-way (a path
-- cut short would have broken winding, so it is not drawn at all); the
-- caller then skips the command.
flattenFill :: Env s -> Affine -> Double -> Double -> Int -> Int -> Path -> ST s Bool
flattenFill env (Affine ma mb mc md me mf) !bx !by !w !h = go 0 0 0 0 False
  where
    !wd = fromIntegral w
    !hd = fromIntegral h
    ok v = abs v < 1e12
    line = addEdge env wd hd
    go !cx !cy !sx !sy !has segs = do
      b <- rdA (envI env) slotBudget
      if b <= 0 then wrA (envI env) slotExhausted 1 >> return False else go' cx cy sx sy has segs
    go' !cx !cy !sx !sy !has segs = case segs of
      [] -> do
        when has (line cx cy sx sy)
        endContour env
        return True
      MoveTo (Pt x y) : r ->
        let !px = ma * x + mc * y + me - bx; !py = mb * x + md * y + mf - by
        in if not (ok px && ok py) then return False else do
             when has (line cx cy sx sy)
             endContour env
             go px py px py True r
      LineTo (Pt x y) : r ->
        let !px = ma * x + mc * y + me - bx; !py = mb * x + md * y + mf - by
        in if not (ok px && ok py) then return False
           else if has then line cx cy px py >> go px py sx sy True r
           else go px py px py True r
      QuadTo (Pt x1 y1) (Pt x2 y2) : r ->
        let !p1x = ma * x1 + mc * y1 + me - bx; !p1y = mb * x1 + md * y1 + mf - by
            !p2x = ma * x2 + mc * y2 + me - bx; !p2y = mb * x2 + md * y2 + mf - by
        in if not (ok p1x && ok p1y && ok p2x && ok p2y) then return False else
           if has then quad cx cy p1x p1y p2x p2y >> go p2x p2y sx sy True r
           else quad p1x p1y p1x p1y p2x p2y >> go p2x p2y p1x p1y True r
      CubicTo (Pt x1 y1) (Pt x2 y2) (Pt x3 y3) : r ->
        let !p1x = ma * x1 + mc * y1 + me - bx; !p1y = mb * x1 + md * y1 + mf - by
            !p2x = ma * x2 + mc * y2 + me - bx; !p2y = mb * x2 + md * y2 + mf - by
            !p3x = ma * x3 + mc * y3 + me - bx; !p3y = mb * x3 + md * y3 + mf - by
        in if not (ok p1x && ok p1y && ok p2x && ok p2y && ok p3x && ok p3y) then return False else
           if has then cubic cx cy p1x p1y p2x p2y p3x p3y >> go p3x p3y sx sy True r
           else cubic p1x p1y p1x p1y p2x p2y p3x p3y >> go p3x p3y p1x p1y True r
      ClosePath : r
        | has -> line cx cy sx sy >> endContour env >> go sx sy sx sy True r
        | otherwise -> go cx cy sx sy has r

    -- A curve wholly above, below or right of the box affects the visible
    -- winding exactly as its chord does (and one wholly left of it does too:
    -- every point collapses onto the left edge), so it costs one edge.
    outside4 x0 y0 x1 y1 x2 y2 x3 y3 =
      (y0 <= 0 && y1 <= 0 && y2 <= 0 && y3 <= 0) || (y0 >= hd && y1 >= hd && y2 >= hd && y3 >= hd)
      || (x0 >= wd && x1 >= wd && x2 >= wd && x3 >= wd) || (x0 <= 0 && x1 <= 0 && x2 <= 0 && x3 <= 0)

    quad !x0 !y0 !x1 !y1 !x2 !y2
      | outside4 x0 y0 x1 y1 x2 y2 x2 y2 = line x0 y0 x2 y2
      | otherwise =
          let n = quadSteps' x0 y0 x1 y1 x2 y2
              inv = 1 / fromIntegral n
              loop !k !px !py
                | k >= n = line px py x2 y2
                | otherwise = do
                    let t = fromIntegral k * inv; u = 1 - t
                        qx = u * u * x0 + 2 * u * t * x1 + t * t * x2
                        qy = u * u * y0 + 2 * u * t * y1 + t * t * y2
                    line px py qx qy
                    loop (k + 1) qx qy
          in loop 1 x0 y0
    cubic !x0 !y0 !x1 !y1 !x2 !y2 !x3 !y3
      | outside4 x0 y0 x1 y1 x2 y2 x3 y3 = line x0 y0 x3 y3
      | otherwise =
          let n = cubicSteps' x0 y0 x1 y1 x2 y2 x3 y3
              inv = 1 / fromIntegral n
              loop !k !px !py
                | k >= n = line px py x3 y3
                | otherwise = do
                    let t = fromIntegral k * inv; u = 1 - t
                        b0 = u * u * u; b1 = 3 * u * u * t; b2 = 3 * u * t * t; b3 = t * t * t
                        qx = b0 * x0 + b1 * x1 + b2 * x2 + b3 * x3
                        qy = b0 * y0 + b1 * y1 + b2 * y2 + b3 * y3
                    line px py qx qy
                    loop (k + 1) qx qy
          in loop 1 x0 y0

quadSteps' :: Double -> Double -> Double -> Double -> Double -> Double -> Int
quadSteps' x0 y0 x1 y1 x2 y2 =
  let dx = x0 - 2 * x1 + x2; dy = y0 - 2 * y1 + y2
  in stepsFor (0.25 * sqrt (dx * dx + dy * dy))

cubicSteps' :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Int
cubicSteps' x0 y0 x1 y1 x2 y2 x3 y3 =
  let ax = x0 - 2 * x1 + x2; ay = y0 - 2 * y1 + y2
      bx = x1 - 2 * x2 + x3; by = y1 - 2 * y2 + y3
  in stepsFor (0.75 * sqrt (max (ax * ax + ay * ay) (bx * bx + by * by)))

stepsFor :: Double -> Int
stepsFor k
  | not (k > 0) = 1
  | otherwise = let n = sqrt (k / flatTol)
                in if n < 4096 then max 1 (ceiling n) else 4096

-- | Add a closed polygon (device coordinates, box-relative after subtracting
-- @bx, by@).
addPolygon :: Env s -> Double -> Double -> Int -> Int -> [Pt] -> ST s ()
addPolygon env !bx !by !w !h pts = case pts of
  [] -> return ()
  (Pt x0 y0 : rest) -> go (x0 - bx) (y0 - by) (x0 - bx) (y0 - by) rest
  where
    !wd = fromIntegral w
    !hd = fromIntegral h
    go !sx !sy !px !py [] = addEdge env wd hd px py sx sy >> endContour env
    go !sx !sy !px !py (Pt x y : r) =
      let qx = x - bx; qy = y - by in addEdge env wd hd px py qx qy >> go sx sy qx qy r

------------------------------------------------------------------------------
-- Accumulation

-- One edge (top to bottom, ya < yb, band-relative rows [0, nrows)) into the
-- accumulation buffer, recording which edge last deposited into each cell.
--
-- Every x is clamped to [0, w] (w = stride - 2) as it is stepped, not just
-- at the ends: an edge that ends exactly on the box's left side (which the
-- left-edge collapse produces for anything crossing it) can step to -1e-16,
-- and floor of that is cell -1, one write before the row (on band row 0,
-- before the buffer).
-- The buffers one fill accumulates into, bundled so 'accLine' stays under
-- GHC's worker-argument limit (past ten arguments its Doubles are boxed:
-- measured, a 70% slowdown on a page of text).
data AccB s = AccB
  !(STUArray s Int Double)   -- accumulation
  !(STUArray s Int Int)      -- row min touched
  !(STUArray s Int Int)      -- row max touched
  !(STUArray s Int Int)      -- cell owners
  !(STUArray s Int Int)      -- row flags
  !(STUArray s Int Double)   -- row contour extents
  !(STUArray s Int Double)   -- edges
  !(STUArray s Int Int)      -- contour ranges
  !Int                       -- stride

accLine :: AccB s -> Int -> Double -> Int -> ST s ()
accLine (AccB acc rmin rmax owner flags rowC edges con stride) !nrows !top !e = do
  let o5 = 5 * e
  xa <- rdA edges o5
  ya0 <- rdA edges (o5 + 1)
  xb <- rdA edges (o5 + 2)
  yb0 <- rdA edges (o5 + 3)
  dir <- rdA edges (o5 + 4)
  cs <- rdA con (3 * e)
  ce <- rdA con (3 * e + 1)
  sg <- rdA con (3 * e + 2)
  let !ya = ya0 - top
      !yb = yb0 - top
  let !ys = if ya > 0 then ya else 0
      !nr = fromIntegral nrows
      !ye = if yb < nr then yb else nr
      !wd = fromIntegral (stride - 2) :: Double
  when (ys < ye) $ do
    let !dxdy = (xb - xa) / (yb - ya)
        !xs = clampW wd (xa + (ys - ya) * dxdy)
        !rs = floor ys :: Int
        !re = ceiling ye :: Int
        go !r !x
          | r >= re = return ()
          | otherwise = do
              let !rt = fromIntegral r
                  !dy = (if rt + 1 < ye then rt + 1 else ye) - (if rt > ys then rt else ys)
                  !xn = clampW wd (x + dxdy * dy)
                  !d = dy * dir
                  !xl = if x < xn then x else xn
                  !xr = if x < xn then xn else x
                  !x0f = fromIntegral (floor xl :: Int) :: Double
                  !x0i = floor xl :: Int
                  !x1i = ceiling xr :: Int
                  !ls = r * stride
              if x1i <= x0i + 1
                then do
                  let !xmf = 0.5 * (x + xn) - x0f
                  dep acc owner flags edges e cs ce dir r (ls + x0i) (d - d * xmf)
                  dep acc owner flags edges e cs ce dir r (ls + x0i + 1) (d * xmf)
                  touchRow rmin rmax rowC flags cs sg r x0i (x0i + 1) xl xr
                else do
                  let !s = 1 / (xr - xl)
                      !x0fr = xl - x0f
                      !a0 = 0.5 * s * (1 - x0fr) * (1 - x0fr)
                      !x1fr = xr - fromIntegral x1i + 1
                      !am = 0.5 * s * x1fr * x1fr
                  dep acc owner flags edges e cs ce dir r (ls + x0i) (d * a0)
                  if x1i == x0i + 2
                    then dep acc owner flags edges e cs ce dir r (ls + x0i + 1) (d * (1 - a0 - am))
                    else do
                      let !a1 = s * (1.5 - x0fr)
                          !ds = d * s
                      dep acc owner flags edges e cs ce dir r (ls + x0i + 1) (d * (a1 - a0))
                      let mid !xi | xi >= x1i - 1 = return ()
                                  | otherwise = dep acc owner flags edges e cs ce dir r (ls + xi) ds >> mid (xi + 1)
                      mid (x0i + 2)
                      let !a2 = a1 + fromIntegral (x1i - x0i - 3) * s
                      dep acc owner flags edges e cs ce dir r (ls + x1i - 1) (d * (1 - a2 - am))
                  dep acc owner flags edges e cs ce dir r (ls + x1i) (d * am)
                  touchRow rmin rmax rowC flags cs sg r x0i x1i xl xr
              go (r + 1) xn
    go rs xs

clampW :: Double -> Double -> Double
clampW wd v = if v > 0 then (if v < wd then v else wd) else 0
{-# INLINE clampW #-}

-- Deposit area into a cell and keep its owner: the first edge to deposit,
-- or -2 once two unrelated edges of the same direction have (two of
-- opposite direction give at most two consecutive winding values inside
-- the pixel, where both fill rules are exact; two of the same direction can
-- step the winding by 2). "Unrelated" means not the same contour within
-- 'adjWindow' edges of each other. The owner is written once per cell, which
-- is most of what keeps this cheap. Top level and INLINE, with its arrays
-- passed explicitly, so it costs no closure per edge.
dep :: STUArray s Int Double -> STUArray s Int Int -> STUArray s Int Int -> STUArray s Int Double
    -> Int -> Int -> Int -> Double -> Int -> Int -> Double -> ST s ()
dep acc owner flags edges !e !cs !ce !dir !r !i !v = do
  o <- rdA acc i
  wrA acc i (o + v)
  when (v /= 0) $ do
    ow <- rdA owner i
    if ow == -1 then wrA owner i e
    else when (ow /= e && ow /= -2 && not (related ow)) $ do
      od <- rdA edges (5 * ow + 4)
      when (od == dir) $ wrA owner i (-2) >> wrA flags r 1
  where
    related ow = ow >= cs && ow < ce && (let dd = abs (ow - e) in dd <= adjWindow || dd >= ce - cs - adjWindow)
{-# INLINE dep #-}

-- Edges this close along one contour (cyclically) are its own consecutive
-- chords; a cell keeps its first depositor, so a curve flattened into
-- several short chords inside one pixel must still count as one edge.
adjWindow :: Int
adjWindow = 8

-- Widen a band row's touched range, and track the contours touching it:
-- two of the same orientation overlapping in x may overlap in area
-- (winding 2), which no single cell need show (their horizontal edges
-- deposit nothing), so such a row is flagged for the winding check.
-- Opposite orientations (a counter inside its glyph, a closed stroke's two
-- offsets) can only go wrong where their edges cross, which 'dep' catches.
touchRow :: STUArray s Int Int -> STUArray s Int Int -> STUArray s Int Double -> STUArray s Int Int
         -> Int -> Int -> Int -> Int -> Int -> Double -> Double -> ST s ()
touchRow rmin rmax rowC flags !csI !sgI !r !lo0 !hi0 !lo !hi = do
  a <- rdA rmin r
  when (lo0 < a) $ wrA rmin r lo0
  b <- rdA rmax r
  when (hi0 > b) $ wrA rmax r hi0
  let o = 8 * r
      cs = fromIntegral csI :: Double
      sg = fromIntegral sgI :: Double
  cur <- rdA rowC o
  if cur == cs
    then do
      cl <- rdA rowC (o + 1)
      when (lo < cl) $ wrA rowC (o + 1) lo
      ch <- rdA rowC (o + 2)
      when (hi > ch) $ wrA rowC (o + 2) hi
    else do
      when (cur /= -1) $ do
        cl <- rdA rowC (o + 1)
        ch <- rdA rowC (o + 2)
        csg <- rdA rowC (o + 3)
        let so = if csg < 0 then o + 6 else o + 4
        sl <- rdA rowC so
        when (cl < sl) $ wrA rowC so cl
        sh <- rdA rowC (so + 1)
        when (ch > sh) $ wrA rowC (so + 1) ch
      wrA rowC o cs
      wrA rowC (o + 1) lo
      wrA rowC (o + 2) hi
      wrA rowC (o + 3) sg
  let so = if sg < 0 then o + 6 else o + 4
  sl <- rdA rowC so
  sh <- rdA rowC (so + 1)
  -- strictly: shapes that only meet inside a pixel do not overlap
  when (lo < sh && hi > sl) $ wrA flags r 1
{-# INLINE touchRow #-}

covEps :: Double
covEps = 1 / 4096

covNZ :: Double -> Float
covNZ a = let b = abs a in if b >= 1 then 1 else if b < covEps then 0 else double2Float b
{-# INLINE covNZ #-}

covEO :: Double -> Float
covEO a =
  let b = abs a
      m = b - 2 * fromIntegral (floor (b * 0.5) :: Int)
      c = if m > 1 then 2 - m else m
  in if c < covEps then 0 else double2Float c
{-# INLINE covEO #-}

-- Which edges a band's rows may need to see again (for 'resolveRow'): all
-- of them, or the first @n@ entries of the active list.
data EdgeSrc s = SrcAll !Int | SrcAct !(STUArray s Int Int) !Int

-- | Rasterise the edge buffer over a box (device origin @bx, by@, size
-- @w × h@) and hand every non-empty row to the sink, through the clip.
rasterFill :: Env s -> Int -> Int -> Int -> Int -> FillRule -> Maybe (Clip s) -> Sink s -> ST s ()
rasterFill env !bx !by !w !h rule mclip sink = do
  n <- rdA (envI env) slotEdges
  when (n > 0 && w > 0 && h > 0) $ do
    edges <- readSTRef (envEdges env)
    -- Edges added outside any contour (not through flattenFill/addPolygon)
    -- form one contour of their own.
    cs0 <- rdA (envI env) slotConStart
    when (cs0 < n) $ endContour env
    con <- readSTRef (envCon env)
    let dd = envD env
        scanY !i !lo !hi
          | i >= n = wrA dd 0 lo >> wrA dd 1 hi
          | otherwise = do
              y0 <- rdA edges (5 * i + 1)
              y1 <- rdA edges (5 * i + 3)
              scanY (i + 1) (if y0 < lo then y0 else lo) (if y1 > hi then y1 else hi)
    scanY 0 1e300 (-1e300)
    ymin <- rdA dd 0
    ymax <- rdA dd 1
    let r0 = max 0 (floor ymin) :: Int
        r1 = min h (ceiling ymax) :: Int
        stride = w + 2
    when (r1 > r0) $ do
      acc <- growD (envAcc env) (stride * bandH) False
      owner <- growIWith (envOwn env) (stride * bandH) (-1) False
      cov <- growF (envCov env) (w + 2)
      let rmin = envRMin env; rmax = envRMax env; flags = envFlag env
          sweep !top !nrows src = case rule of
            NonZero -> sweepBand covNZ env acc owner cov stride w bx by top nrows rule src mclip sink
            EvenOdd -> sweepBand covEO env acc owner cov stride w bx by top nrows rule src mclip sink
          ab = AccB acc rmin rmax owner flags (envRowC env) edges con stride
          accOne !top !nrows !i = accLine ab nrows top i
          accRange !i !e !top !nrows
            | i >= e = return ()
            | otherwise = accOne top nrows i >> accRange (i + 1) e top nrows
      if r1 - r0 <= bandH
        then do
          accRange 0 n (fromIntegral r0) (r1 - r0)
          sweep r0 (r1 - r0) (SrcAll n)
        else do
          let nb = (r1 - r0 + bandH - 1) `quot` bandH
              bandOf !i = do
                y0 <- rdA edges (5 * i + 1)
                let b = (floor y0 - r0) `quot` bandH
                return $! if b < 0 then 0 else if b >= nb then nb - 1 else b
          cnt <- growI (envCnt env) (nb + 1)
          forM_ [0 .. nb] $ \b -> wrA cnt b 0
          let countLoop !i | i >= n = return ()
                           | otherwise = do
                               b <- bandOf i
                               c <- rdA cnt b
                               wrA cnt b (c + 1)
                               countLoop (i + 1)
          countLoop 0
          let prefix !b !s | b >= nb = return ()
                           | otherwise = do
                               c <- rdA cnt b
                               wrA cnt b s
                               prefix (b + 1) (s + c)
          prefix 0 0
          idx <- growI (envIdx env) n
          let place !i | i >= n = return ()
                       | otherwise = do
                           b <- bandOf i
                           p <- rdA cnt b
                           wrA idx p i
                           wrA cnt b (p + 1)
                           place (i + 1)
          place 0
          act <- growI (envAct env) n
          let bands !b !nAct !start
                | b >= nb = return ()
                | otherwise = do
                    let top = r0 + b * bandH
                        rows = min bandH (r1 - top)
                        bot = fromIntegral (top + rows) :: Double
                        topD = fromIntegral top :: Double
                    end <- rdA cnt b
                    let append !k !m
                          | k >= end = return m
                          | otherwise = do
                              e <- rdA idx k
                              wrA act m e
                              append (k + 1) (m + 1)
                    total <- append start nAct
                    let accAll !k | k >= total = return ()
                                  | otherwise = do
                                      e <- rdA act k
                                      accOne topD rows e
                                      accAll (k + 1)
                    accAll 0
                    -- Sweep before compacting: a row being resolved needs the
                    -- edges that end inside this band too.
                    sweep top rows (SrcAct act total)
                    let compact !k !keep
                          | k >= total = bands (b + 1) keep end
                          | otherwise = do
                              e <- rdA act k
                              y1 <- rdA edges (5 * e + 3)
                              if y1 > bot
                                then wrA act keep e >> compact (k + 1) (keep + 1)
                                else compact (k + 1) keep
                    compact 0 0
          bands 0 0 0

-- Sweep the rows of one band, emitting each touched row.
sweepBand :: (Double -> Float) -> Env s -> STUArray s Int Double -> STUArray s Int Int -> STUArray s Int Float
          -> Int -> Int -> Int -> Int -> Int -> Int -> FillRule -> EdgeSrc s -> Maybe (Clip s) -> Sink s -> ST s ()
sweepBand covf env acc owner cov !stride !w !bx !by !top !nrows rule src mclip sink = rows 0
  where
    rmin = envRMin env
    rmax = envRMax env
    flags = envFlag env
    rows !r
      | r >= nrows = return ()
      | otherwise = do
          mx <- rdA rmax r
          if mx < 0 then rows (r + 1) else do
            mn0 <- rdA rmin r
            fl <- rdA flags r
            wrA rmin r maxBound
            wrA rmax r (-1)
            wrA flags r 0
            resetRowC (envRowC env) r
            let base = r * stride
                mn = if mn0 < 0 then 0 else mn0
                y = by + top + r
                go !x !a
                  | x > mx = finish x a
                  | otherwise = do
                      v <- rdA acc (base + x)
                      wrA acc (base + x) 0
                      -- a flagged row's owners are read (then cleared) by resolveRow
                      when (fl == 0) $ wrA owner (base + x) (-1)
                      let a' = a + v
                      when (x < w) $ wrA cov x (covf a')
                      go (x + 1) a'
                finish !x !a = do
                  let tailRun = x < w && abs a > covEps
                      xe | tailRun = w
                         | x < w = x
                         | otherwise = w
                  when tailRun $ do
                    let c = covf a
                        fill !k | k >= w = return ()
                                | otherwise = wrA cov k c >> fill (k + 1)
                    fill x
                  when (fl /= 0) $ resolveRow env owner cov stride w top r mn mx xe rule src
                  when (xe > mn) $ emitRow env sink mclip cov y bx (bx + mn) (bx + xe)
                  rows (r + 1)
            go mn 0
{-# INLINE sweepBand #-}

-- Sub-scanlines a row that fails the winding check is recomputed with.
subLines :: Int
subLines = 16

-- | Check, and if need be recompute, a row the fast path may have got wrong
-- ('resolveRow''), unless the fill asked for none ('slotNoResolve') or the
-- budget is gone; either way the row's cell owners are left clean.
resolveRow :: Env s -> STUArray s Int Int -> STUArray s Int Float -> Int -> Int -> Int -> Int -> Int -> Int -> Int
           -> FillRule -> EdgeSrc s -> ST s ()
resolveRow env owner cov !stride !w !top !r !mn !mx !xe rule src = do
  noRes <- rdA (envI env) slotNoResolve
  ok <- budgetLeft env
  when (noRes == 0 && ok) $ do
    -- even looking costs a scan of the band's edges
    charge env (case src of { SrcAll k -> k; SrcAct _ k -> k })
    ok' <- budgetLeft env
    when ok' $ resolveRow' env owner cov stride w top r mn mx xe rule src
  let base = r * stride
      clear !x | x > mx = return ()
               | otherwise = wrA owner (base + x) (-1) >> clear (x + 1)
  clear mn

-- | Rows crossed by more edges than this are not repaired.
maxResolveCrossings :: Int
maxResolveCrossings = 256

-- | Check, and if need be recompute, a row the fast path may have got wrong.
--
-- The fast path applies the fill rule to the /integrated/ winding of each
-- pixel. That is exact wherever the winding inside the pixel only takes 0
-- and one other value of a fixed sign (one edge, a contour's own
-- consecutive segments, nested contours of opposite winding like an "o"),
-- and both rules are exact there. It is wrong where the winding reaches ±2
-- or changes sign inside a pixel: a rectangle drawn twice under even-odd
-- integrates to "half of 2", which folds to opaque; abutting
-- opposite-winding shapes cancel to a transparent seam; a retraced stroke,
-- or overlapping shapes whose tops lie in one row, integrate past 1 where
-- the true coverage is a fraction. None of those can happen in a row only
-- one contour touches without crossing itself, so only rows touched by two
-- contours, or with a cell where unrelated edges met, come here (which is
-- what keeps text fast).
--
-- The row's true winding is sampled on four sub-scanlines; if every value
-- is 0 or one fixed ±1 the fast result stands. Otherwise the whole emitted
-- span is recomputed on 'subLines' sub-scanlines with exact horizontal
-- coverage, applying the rule to every span's real winding. The midpoint
-- rule is exact for a straight edge across a pixel, so the error is confined
-- to where an edge enters or leaves through the pixel's sides.
resolveRow' :: Env s -> STUArray s Int Int -> STUArray s Int Float -> Int -> Int -> Int -> Int -> Int -> Int -> Int
           -> FillRule -> EdgeSrc s -> ST s ()
resolveRow' env _owner cov !_stride !w !top !r !mn !_mx !xe rule src = do
  edges <- readSTRef (envEdges env)
  let nEdges = case src of
        SrcAll k -> k
        SrcAct _ k -> k
      edgeAt !k = case src of
        SrcAll _ -> return k
        SrcAct a _ -> rdA a k
      yr0 = fromIntegral (top + r) :: Double
      yr1 = yr0 + 1
  rowE <- growI (envRowE env) (nEdges + 1)
  let collect !k !m
        | k >= nEdges = return m
        | otherwise = do
            e <- edgeAt k
            y0 <- rdA edges (5 * e + 1)
            y1 <- rdA edges (5 * e + 3)
            if y1 > yr0 && y0 < yr1 then wrA rowE m e >> collect (k + 1) (m + 1)
            else collect (k + 1) m
  m <- collect 0 0
  -- A row crossed by more edges than this is left to the fast path: its
  -- repair would cost m² per sub-scanline (insertion sort), and a row that
  -- busy is a mesh or a hatching where the integral is already very close.
  unless (m > maxResolveCrossings) $ do
    xs <- growD (envXs env) (2 * m + 2) False
    let -- Sorted crossings of sub-scanline y into xs (x, direction pairs).
        crossings !yk = do
          let gather !j !c
                | j >= m = return c
                | otherwise = do
                    e <- rdA rowE j
                    let o = 5 * e
                    y0 <- rdA edges (o + 1)
                    y1 <- rdA edges (o + 3)
                    if yk >= y0 && yk < y1
                      then do
                        x0 <- rdA edges o
                        x1 <- rdA edges (o + 2)
                        dir <- rdA edges (o + 4)
                        insertSorted c (x0 + (yk - y0) * (x1 - x0) / (y1 - y0)) dir
                        gather (j + 1) (c + 1)
                      else gather (j + 1) c
          gather 0 0
        -- insertion into the sorted list; a row's crossings are few, and the
        -- work is charged
        insertSorted !c !xk !dir = do
          let shift !j
                | j <= 0 = place 0
                | otherwise = do
                    xp <- rdA xs (2 * (j - 1))
                    if xp > xk
                      then do
                        dp <- rdA xs (2 * (j - 1) + 1)
                        wrA xs (2 * j) xp
                        wrA xs (2 * j + 1) dp
                        shift (j - 1)
                      else place j
              place j = wrA xs (2 * j) xk >> wrA xs (2 * j + 1) dir
          shift c
        -- Does any winding on this sub-scanline leave {0, s}? (s = 0: not yet known)
        badLine !yk !s0 = do
          c <- crossings yk
          let walk !i !wn !s
                | i >= c = return (if s == 0 then 0 else s)
                | otherwise = do
                    di <- rdA xs (2 * i + 1)
                    let wn' = wn + round di :: Int
                    if abs wn' > 1 || (wn' /= 0 && s /= 0 && signum wn' /= s) then return 2
                    else walk (i + 1) wn' (if wn' /= 0 then signum wn' else s)
          walk 0 0 s0
        check !k !s
          | k >= 4 = return False
          | otherwise = do
              let yk = yr0 + (fromIntegral k + 0.5) * 0.25
              v <- badLine yk s
              if v == 2 then return True else check (k + 1) v
    charge env (4 * m + m * m)
    okCheck <- budgetLeft env
    complexRow <- if okCheck then check (0 :: Int) (0 :: Int) else return False
    let lastCol = xe - 1
        ncol = lastCol - mn + 1
    charge env (if complexRow then subLines * (m + ncol + m * m `quot` 2) else 0)
    okFix <- budgetLeft env
    when (complexRow && okFix) $ do
      res <- growF (envCol env) (ncol + 1)
      forM_ [0 .. ncol - 1] $ \c -> wrA res c 0
      let inside wn = case rule of
            NonZero -> wn /= (0 :: Int)
            EvenOdd -> odd wn
          weight = 1 / fromIntegral subLines :: Double
          loL = fromIntegral mn :: Double
          hiR = fromIntegral (lastCol + 1) :: Double
          addSpan !a0 !b0 = do
            let a = max a0 loL; b = min b0 hiR
            when (b > a) $ do
              let c0 = floor a :: Int; c1 = min lastCol (ceiling b - 1)
                  cols !cc | cc > c1 = return ()
                           | otherwise = do
                               let lo = max a (fromIntegral cc); hi = min b (fromIntegral cc + 1)
                               when (hi > lo) $ do
                                 v <- rdA res (cc - mn)
                                 wrA res (cc - mn) (v + double2Float ((hi - lo) * weight))
                               cols (cc + 1)
              cols (max mn c0)
          sub !k
            | k >= subLines = return ()
            | otherwise = do
                let yk = yr0 + (fromIntegral k + 0.5) * weight
                c <- crossings yk
                -- winding wn holds on [x_i, x_{i+1}); past the last crossing
                -- it is the total, non-zero when the box clipped edges away
                let walk !i !wn !xPrev
                      | i >= c = when (c > 0 && inside wn) $ addSpan xPrev (fromIntegral w)
                      | otherwise = do
                          xi <- rdA xs (2 * i)
                          di <- rdA xs (2 * i + 1)
                          when (inside wn) $ addSpan xPrev xi
                          walk (i + 1) (wn + round di) xi
                walk 0 0 0
                sub (k + 1)
      sub 0
      forM_ [0 .. ncol - 1] $ \c -> do
        v <- rdA res c
        wrA cov (mn + c) (if v > 1 then 1 else if v < 0 then 0 else v)

------------------------------------------------------------------------------
-- Rows → clip → sink

-- | Multiply a coverage row (indexed from @bx@) by the clip over device
-- columns @[xa, xe)@ of row @y@.
clipRow :: Clip s -> STUArray s Int Float -> Int -> Int -> Int -> Int -> ST s ()
clipRow cl cov !y !bx !xa !xe = case cl of
  ClipRect x0 y0 x1 y1 -> do
    let yd = fromIntegral y :: Double
        fy = overlap yd (yd + 1) y0 y1
    if fy <= 0 then zero xa else do
      let cl0 = floorI x0
          cr0 = ceiling' x1 - 1
          fyF = double2Float fy
          colF x = double2Float (overlap (fromIntegral x) (fromIntegral x + 1) x0 x1)
          scale !x !f = when (x >= xa && x < xe) $ do
            c <- rdA cov (x - bx)
            wrA cov (x - bx) (c * f)
      if fy >= 1
        then do
          let f0 = colF cl0
          when (f0 < 1) $ scale cl0 f0
          when (cr0 /= cl0) $ do
            let f1 = colF cr0
            when (f1 < 1) $ scale cr0 f1
        else do
          let allRow !x | x >= xe = return ()
                        | otherwise = do
                            let f = if x == cl0 || x == cr0 then fyF * colF x else fyF
                            c <- rdA cov (x - bx)
                            wrA cov (x - bx) (c * f)
                            allRow (x + 1)
          allRow xa
  ClipMask mx my mw mh marr -> do
    let row = y - my
    if row < 0 || row >= mh then zero xa else do
      let go !x | x >= xe = return ()
                | otherwise = do
                    let cx = x - mx
                    if cx < 0 || cx >= mw then wrA cov (x - bx) 0 else do
                      m <- rdA marr (row * mw + cx)
                      c <- rdA cov (x - bx)
                      wrA cov (x - bx) (c * m)
                    go (x + 1)
      go xa
  where
    zero !x | x >= xe = return ()
            | otherwise = wrA cov (x - bx) 0 >> zero (x + 1)
    ceiling' v | v > -1e15 && v < 1e15 = ceiling v
               | otherwise = 0 :: Int

overlap :: Double -> Double -> Double -> Double -> Double
overlap a b c d = let lo = max a c; hi = min b d in if hi > lo then hi - lo else 0
{-# INLINE overlap #-}

emitRow :: Env s -> Sink s -> Maybe (Clip s) -> STUArray s Int Float -> Int -> Int -> Int -> Int -> ST s ()
emitRow env sink mclip cov !y !bx !xa !xe = do
  charge env (xe - xa)
  px <- rdA (envI env) slotPixels
  wrA (envI env) slotPixels (px + xe - xa)
  case mclip of
    Nothing -> return ()
    Just cl -> clipRow cl cov y bx xa xe
  case sink of
    SkMask mx my mw marr -> do
      -- Clamped to the mask's own box, as a backstop. The bug it was added
      -- for: a cached glyph is drawn at its quarter-pixel phase, up to an
      -- eighth of a pixel from where its run measured the mask from the
      -- exact outlines, so its coverage could start a pixel before the box
      -- and the write landed before the row (a translucent glyph on a real
      -- page, a segfault in the unchecked build). The run now grows each
      -- glyph's bounds by a pixel and 'fillGlyph' blits only within the
      -- mask, so nothing should reach this clamp; it stays because a write
      -- outside an unchecked array is memory corruption, not a wrong pixel.
      n <- getNumElements marr
      let base = (y - my) * mw - mx
          xa' = max xa mx
          xe' = min xe (mx + mw)
          go !x | x >= xe' = return ()
                | otherwise = do
                    c <- rdA cov (x - bx)
                    when (c > 0) $ do
                      m <- rdA marr (base + x)
                      let s = m + c
                      wrA marr (base + x) (if s > 1 then 1 else s)
                    go (x + 1)
      when (y >= my && mw > 0 && (y - my + 1) * mw <= n) $ go xa'
    SkPaint ly sh bl op mst -> do
      case mst of
        Nothing -> return ()
        Just st -> stencilRow st cov y bx xa xe
      paintRow env ly sh bl op cov y bx xa xe

stencilRow :: Stencil -> STUArray s Int Float -> Int -> Int -> Int -> Int -> ST s ()
stencilRow (Stencil (Affine a b c d e f) tex near) cov !y !bx !xa !xe = go xa
  where
    yc = fromIntegral y + 0.5 :: Double
    go !x | x >= xe = return ()
          | otherwise = do
              cv <- rdA cov (x - bx)
              when (cv > 0) $ do
                let xc = fromIntegral x + 0.5
                    u = a * xc + c * yc + e
                    v = b * xc + d * yc + f
                    m = sampleMaskTex tex near u v
                wrA cov (x - bx) (cv * m)
              go (x + 1)

-- | Composite one coverage row into a layer with a shader.
paintRow :: Env s -> Layer s -> Shader -> Blend -> Float -> STUArray s Int Float
         -> Int -> Int -> Int -> Int -> ST s ()
paintRow env ly sh bl op cov !y !bx !xa !xe = case sh of
  ShNone -> return ()
  ShSolid r g b a
    | plain -> solidRow ly cov y bx xa xe r g b a op
    | otherwise -> genericSolid
  _ -> do
    col <- growF (envCol env) (4 * (xe - xa) + 4)
    let loop !x !start
          | x >= xe = when (start >= 0) (flush col start xe)
          | otherwise = do
              c <- rdA cov (x - bx)
              if c > 0
                then loop (x + 1) (if start < 0 then x else start)
                else if start >= 0 then flush col start x >> loop (x + 1) (-1)
                else loop (x + 1) (-1)
    loop xa (-1)
  where
    plain = bl == BlendNormal && isNothing' (lyKnock ly) && isNothing' (lyGA ly)
    rowBase = ((y - lyY ly) * lyW ly - lyX ly) * 4
    genericSolid = case sh of
      ShSolid r g b a ->
        let go !x | x >= xe = return ()
                  | otherwise = do
                      c <- rdA cov (x - bx)
                      when (c > 0) $ compositePx ly bl (rowBase + 4 * x) c op r g b a
                      go (x + 1)
        in go xa
      _ -> return ()
    flush col !s !e = do
      shadeRun sh col y s e
      if plain then colorRunNormal ly cov col y bx s e op
      else do
        let go !x | x >= e = return ()
                  | otherwise = do
                      c <- rdA cov (x - bx)
                      let k = 4 * (x - s)
                      sr <- rdA col k
                      sg <- rdA col (k + 1)
                      sb <- rdA col (k + 2)
                      sa <- rdA col (k + 3)
                      compositePx ly bl (rowBase + 4 * x) c op sr sg sb sa
                      go (x + 1)
        go s

isNothing' :: Maybe a -> Bool
isNothing' Nothing = True
isNothing' _ = False
{-# INLINE isNothing' #-}

-- The text path: a solid colour, normal blend, plain layer.
solidRow :: Layer s -> STUArray s Int Float -> Int -> Int -> Int -> Int
         -> Float -> Float -> Float -> Float -> Float -> ST s ()
solidRow ly cov !y !bx !xa !xe !r !g !b !a !op = go xa
  where
    pix = lyPix ly
    rowBase = ((y - lyY ly) * lyW ly - lyX ly) * 4
    opaque = a >= 1 && op >= 1
    go !x
      | x >= xe = return ()
      | otherwise = do
          c0 <- rdA cov (x - bx)
          if c0 <= 0 then go (x + 1) else do
            let i = rowBase + 4 * x
            if c0 >= 1 && opaque
              then do
                wrA pix i r
                wrA pix (i + 1) g
                wrA pix (i + 2) b
                wrA pix (i + 3) 1
              else do
                let k = c0 * op
                    ia = 1 - a * k
                dr <- rdA pix i
                dg <- rdA pix (i + 1)
                db <- rdA pix (i + 2)
                da <- rdA pix (i + 3)
                wrA pix i (r * k + dr * ia)
                wrA pix (i + 1) (g * k + dg * ia)
                wrA pix (i + 2) (b * k + db * ia)
                wrA pix (i + 3) (a * k + da * ia)
            go (x + 1)

colorRunNormal :: Layer s -> STUArray s Int Float -> STUArray s Int Float -> Int -> Int -> Int -> Int -> Float -> ST s ()
colorRunNormal ly cov col !y !bx !s !e !op = go s
  where
    pix = lyPix ly
    rowBase = ((y - lyY ly) * lyW ly - lyX ly) * 4
    go !x
      | x >= e = return ()
      | otherwise = do
          c0 <- rdA cov (x - bx)
          let k = c0 * op
              j = 4 * (x - s)
              i = rowBase + 4 * x
          sr <- rdA col j
          sg <- rdA col (j + 1)
          sb <- rdA col (j + 2)
          sa <- rdA col (j + 3)
          let ia = 1 - sa * k
          dr <- rdA pix i
          dg <- rdA pix (i + 1)
          db <- rdA pix (i + 2)
          da <- rdA pix (i + 3)
          wrA pix i (sr * k + dr * ia)
          wrA pix (i + 1) (sg * k + dg * ia)
          wrA pix (i + 2) (sb * k + db * ia)
          wrA pix (i + 3) (sa * k + da * ia)
          go (x + 1)

-- | Evaluate a shader for device columns @[s, e)@ of row @y@ into the colour
-- row (four floats per pixel, from index 0).
shadeRun :: Shader -> STUArray s Int Float -> Int -> Int -> Int -> ST s ()
shadeRun sh col !y !s !e = case sh of
  ShNone -> fillC 0 0 0 0
  ShSolid r g b a -> fillC r g b a
  ShLinear ta tb tc stops sp -> do
    let yc = fromIntegral y + 0.5
        go !x | x >= e = return ()
              | otherwise = do
                  let t = ta * (fromIntegral x + 0.5) + tb * yc + tc
                      t' = spreadT sp t
                  put x (if isNaN t' then C4 0 0 0 0 else stopsColor stops t')
                  go (x + 1)
    go s
  ShRadial (Affine a b c d ee f) rad stops sp -> do
    let yc = fromIntegral y + 0.5
        go !x | x >= e = return ()
              | otherwise = do
                  let xc = fromIntegral x + 0.5
                      gx = a * xc + c * yc + ee
                      gy = b * xc + d * yc + f
                      t = radialT rad sp gx gy
                      t' = if isNaN t then t else spreadT sp t
                  put x (if isNaN t' then C4 0 0 0 0 else stopsColor stops t')
                  go (x + 1)
    go s
  ShTex (Affine a b c d ee f) tex mode near op -> do
    let yc = fromIntegral y + 0.5
        go !x | x >= e = return ()
              | otherwise = do
                  let xc = fromIntegral x + 0.5
                      u = a * xc + c * yc + ee
                      v = b * xc + d * yc + f
                      C4 r g bb al = sampleTex tex mode near u v
                  put x (C4 (r * op) (g * op) (bb * op) (al * op))
                  go (x + 1)
    go s
  ShScratch gx gy gw gh arr
    | y < gy || y >= gy + gh -> fillC 0 0 0 0
    | otherwise -> do
        let base = ((y - gy) * gw - gx) * 4
            go !x | x >= e = return ()
                  | x < gx || x >= gx + gw = put x (C4 0 0 0 0) >> go (x + 1)
                  | otherwise = do
                      let i = base + 4 * x
                      put x (C4 (atA arr i) (atA arr (i + 1)) (atA arr (i + 2)) (atA arr (i + 3)))
                      go (x + 1)
        go s
  where
    put !x (C4 r g b a) = do
      let k = 4 * (x - s)
      wrA col k r
      wrA col (k + 1) g
      wrA col (k + 2) b
      wrA col (k + 3) a
    {-# INLINE put #-}
    fillC r g b a = let go !x | x >= e = return ()
                              | otherwise = put x (C4 r g b a) >> go (x + 1)
                    in go s

------------------------------------------------------------------------------
-- Compositing

-- | Composite one premultiplied source pixel (alpha /without/ coverage)
-- into a layer at float index @i@: coverage (the object's shape at this
-- pixel), opacity, blend mode. On a knockout layer the source is composited
-- against the layer's initial backdrop and the result replaces the current
-- value in proportion to the shape (PDF 11.4.8.4).
compositePx :: Layer s -> Blend -> Int -> Float -> Float -> Float -> Float -> Float -> Float -> ST s ()
compositePx ly bl !i !c !op !sr0 !sg0 !sb0 !sa0 = do
  let pix = lyPix ly
  case lyKnock ly of
    Nothing -> do
      let k = c * op
      dr <- rdA pix i
      dg <- rdA pix (i + 1)
      db <- rdA pix (i + 2)
      da <- rdA pix (i + 3)
      let C4 r g b a = blendOver bl (sr0 * k) (sg0 * k) (sb0 * k) (sa0 * k) dr dg db da
      wrA pix i r
      wrA pix (i + 1) g
      wrA pix (i + 2) b
      wrA pix (i + 3) a
      case lyGA ly of
        Nothing -> return ()
        Just ga -> do
          let j = i `quot` 4
              s = sa0 * k
          g0 <- rdA ga j
          wrA ga j (g0 + s - g0 * s)
    Just bd -> do
      let sr = sr0 * op; sg = sg0 * op; sb = sb0 * op; sa = sa0 * op
      br <- rdA bd i
      bg <- rdA bd (i + 1)
      bb <- rdA bd (i + 2)
      ba <- rdA bd (i + 3)
      let C4 r g b a = blendOver bl sr sg sb sa br bg bb ba
      dr <- rdA pix i
      dg <- rdA pix (i + 1)
      db <- rdA pix (i + 2)
      da <- rdA pix (i + 3)
      wrA pix i (dr + (r - dr) * c)
      wrA pix (i + 1) (dg + (g - dg) * c)
      wrA pix (i + 2) (db + (b - db) * c)
      wrA pix (i + 3) (da + (a - da) * c)
      case lyGA ly of
        Nothing -> return ()
        Just ga -> do
          let j = i `quot` 4
          g0 <- rdA ga j
          wrA ga j (g0 + (sa - g0) * c)

-- | Premultiplied source over premultiplied destination under a blend mode:
-- @co = cs(1 - ab) + cb(1 - as) + as·ab·B(Cb, Cs)@.
blendOver :: Blend -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> C4
blendOver bl !sr !sg !sb !sa !dr !dg !db !da = case bl of
  BlendNormal -> let ia = 1 - sa in C4 (sr + dr * ia) (sg + dg * ia) (sb + db * ia) (sa + da * ia)
  _ | sa <= 0 -> C4 dr dg db da
    | da <= 0 -> C4 sr sg sb sa
    | otherwise ->
        let isa = 1 / sa; ida = 1 / da
            C3 br bg bb = blendFn bl (cl (dr * ida)) (cl (dg * ida)) (cl (db * ida))
                                     (cl (sr * isa)) (cl (sg * isa)) (cl (sb * isa))
            t = sa * da
            ksa = 1 - sa; kda = 1 - da
        in C4 (sr * kda + dr * ksa + t * br) (sg * kda + dg * ksa + t * bg)
              (sb * kda + db * ksa + t * bb) (sa + da - t)
  where
    cl v = if v < 0 then 0 else if v > 1 then 1 else v

-- B(Cb, Cs) for every non-normal mode, straight colours in [0,1].
blendFn :: Blend -> Float -> Float -> Float -> Float -> Float -> Float -> C3
blendFn bl br bg bb sr sg sb = case bl of
  BlendHue -> setLum (setSat (C3 sr sg sb) (sat3 br bg bb)) (lum3 br bg bb)
  BlendSaturation -> setLum (setSat (C3 br bg bb) (sat3 sr sg sb)) (lum3 br bg bb)
  BlendColor -> setLum (C3 sr sg sb) (lum3 br bg bb)
  BlendLuminosity -> setLum (C3 br bg bb) (lum3 sr sg sb)
  _ -> C3 (sep bl br sr) (sep bl bg sg) (sep bl bb sb)

sep :: Blend -> Float -> Float -> Float
sep bl cb cs = case bl of
  BlendMultiply -> cb * cs
  BlendScreen -> cb + cs - cb * cs
  BlendOverlay -> hard cs cb
  BlendDarken -> min cb cs
  BlendLighten -> max cb cs
  BlendColorDodge
    | cb <= 0 -> 0
    | cs >= 1 -> 1
    | otherwise -> min 1 (cb / (1 - cs))
  BlendColorBurn
    | cb >= 1 -> 1
    | cs <= 0 -> 0
    | otherwise -> 1 - min 1 ((1 - cb) / cs)
  BlendHardLight -> hard cb cs
  BlendSoftLight
    | cs <= 0.5 -> cb - (1 - 2 * cs) * cb * (1 - cb)
    | otherwise ->
        let dd = if cb <= 0.25 then ((16 * cb - 12) * cb + 4) * cb else sqrt cb
        in cb + (2 * cs - 1) * (dd - cb)
  BlendDifference -> abs (cb - cs)
  BlendExclusion -> cb + cs - 2 * cb * cs
  _ -> cs
  where
    -- HardLight(Cb, Cs)
    hard b s | s <= 0.5 = b * 2 * s
             | otherwise = let s' = 2 * s - 1 in b + s' - b * s'

lum3 :: Float -> Float -> Float -> Float
lum3 r g b = 0.3 * r + 0.59 * g + 0.11 * b

sat3 :: Float -> Float -> Float -> Float
sat3 r g b = max r (max g b) - min r (min g b)

setLum :: C3 -> Float -> C3
setLum (C3 r g b) l =
  let d = l - lum3 r g b
  in clipColor (r + d) (g + d) (b + d)

clipColor :: Float -> Float -> Float -> C3
clipColor r g b =
  let l = lum3 r g b
      n = min r (min g b)
      x = max r (max g b)
      lowFix v = if n < 0 && l - n > 0 then l + (v - l) * l / (l - n) else v
      highFix v = if x > 1 && x - l > 0 then l + (v - l) * (1 - l) / (x - l) else v
  in C3 (highFix (lowFix r)) (highFix (lowFix g)) (highFix (lowFix b))

setSat :: C3 -> Float -> C3
setSat (C3 r g b) s =
  let mx = max r (max g b); mn = min r (min g b)
      f v = if mx > mn then (v - mn) * s / (mx - mn) else 0
  in C3 (f r) (f g) (f b)

-- | Composite a finished group layer into its parent over the group's box:
-- blend, opacity, an optional soft mask (over its own device box, zero
-- outside it, so one cached mask can serve many groups), an optional clip,
-- and, for a non-isolated group, backdrop removal
-- (PDF 11.4.8: @C = Cn + (Cn - C0)(α0/αg - α0)@ with the group's own alpha).
compositeLayer :: Env s -> Layer s -> Layer s -> Blend -> Float -> Maybe (IBox, STUArray s Int Float)
               -> Maybe (Clip s) -> Bool -> ST s ()
compositeLayer env parent src bl op mmask mclip nonIso = do
  let IBox x0 y0 x1 y1 = iboxIntersect (IBox (lyX src) (lyY src) (lyX src + lyW src) (lyY src + lyH src))
                                       (IBox (lyX parent) (lyY parent) (lyX parent + lyW parent) (lyY parent + lyH parent))
      IBox cx0 cy0 cx1 cy1 = maybe (IBox x0 y0 x1 y1) (iboxIntersect (IBox x0 y0 x1 y1) . clipIBox) mclip
  when (cx1 > cx0 && cy1 > cy0) $ do
    cov <- growF (envCov env) (cx1 - cx0 + 2)
    let sp = lyPix src; pp = lyPix parent
        sw = lyW src
        rows !y
          | y >= cy1 = return ()
          | otherwise = do
              let sRow = (y - lyY src) * sw - lyX src
                  fillCov !x | x >= cx1 = return ()
                             | otherwise = do
                                 m <- case mmask of
                                   Nothing -> return 1
                                   Just (IBox mx0 my0 mx1 my1, ma)
                                     | x >= mx0 && x < mx1 && y >= my0 && y < my1 ->
                                         rdA ma ((y - my0) * (mx1 - mx0) + (x - mx0))
                                     | otherwise -> return 0
                                 wrA cov (x - cx0) m
                                 fillCov (x + 1)
              fillCov cx0
              case mclip of
                Nothing -> return ()
                Just cl -> clipRow cl cov y cx0 cx0 cx1
              let pRow = ((y - lyY parent) * lyW parent - lyX parent) * 4
                  go !x
                    | x >= cx1 = return ()
                    | otherwise = do
                        c <- rdA cov (x - cx0)
                        let si = 4 * (sRow + x)
                        sa <- rdA sp (si + 3)
                        if c <= 0 || (sa <= 0 && not nonIso) then go (x + 1) else do
                          sr <- rdA sp si
                          sg <- rdA sp (si + 1)
                          sb <- rdA sp (si + 2)
                          let pi' = pRow + 4 * x
                          if nonIso
                            then do
                              ga <- case lyGA src of
                                Just g -> rdA g (sRow + x)
                                Nothing -> return sa
                              if ga <= 0 then go (x + 1) else do
                                br <- rdA pp pi'
                                bg <- rdA pp (pi' + 1)
                                bb <- rdA pp (pi' + 2)
                                ba <- rdA pp (pi' + 3)
                                let unp v a = if a > 0 then v / a else 0
                                    k = if ga > 0 then ba / ga - ba else 0
                                    fix cn c0 = let v = cn + (cn - c0) * k in if v < 0 then 0 else if v > 1 then 1 else v
                                    rr = fix (unp sr sa) (unp br ba)
                                    rg = fix (unp sg sa) (unp bg ba)
                                    rb = fix (unp sb sa) (unp bb ba)
                                compositePx parent bl pi' (c * ga) op rr rg rb 1
                                go (x + 1)
                            else do
                              -- The group's alpha is its shape: a knockout
                              -- parent replaces earlier siblings only as
                              -- far as the group actually covers (its soft
                              -- edges must not erase them), and for a plain
                              -- parent shape × straight colour is the same
                              -- premultiplied source as before.
                              let ia = 1 / sa
                                  un v = let u = v * ia in if u > 1 then 1 else u
                              compositePx parent bl pi' (c * sa) op (un sr) (un sg) (un sb) 1
                              go (x + 1)
              go cx0
              rows (y + 1)
    rows cy0

-- | Emit a coverage mask (over a box, stride = box width) through a clip
-- into a sink, as if it had just been rasterised.
emitMask :: Env s -> IBox -> STUArray s Int Float -> Maybe (Clip s) -> Sink s -> ST s ()
emitMask env (IBox x0 y0 x1 y1) marr mclip sink = do
  let w = x1 - x0
  when (w > 0 && y1 > y0) $ do
    cov <- growF (envCov env) (w + 2)
    let rows !y
          | y >= y1 = return ()
          | otherwise = do
              let base = (y - y0) * w
                  copy !x !lo !hi
                    | x >= w = finish lo hi
                    | otherwise = do
                        m <- rdA marr (base + x)
                        wrA cov x m
                        if m > 0 then copy (x + 1) (if lo < 0 then x else lo) (x + 1)
                        else copy (x + 1) lo hi
                  finish lo hi = do
                    when (lo >= 0) $ emitRow env sink mclip cov y x0 (x0 + lo) (x0 + hi)
                    rows (y + 1)
              copy 0 (-1) 0
    rows y0

-- | Emit a coverage bitmap held over its own device box (stride = box
-- width), restricted to a visible box, through a clip into a sink.
emitMaskAt :: Env s -> IBox -> UArray Int Float -> IBox -> Maybe (Clip s) -> Sink s -> ST s ()
emitMaskAt env (IBox sx0 sy0 sx1 _) arr vis mclip sink = do
  let IBox vx0 vy0 vx1 vy1 = vis
      sw = sx1 - sx0
      w = vx1 - vx0
  when (w > 0 && vy1 > vy0) $ do
    cov <- growF (envCov env) (w + 2)
    let rows !y
          | y >= vy1 = return ()
          | otherwise = do
              let base = (y - sy0) * sw - sx0
                  copy !x !lo !hi
                    | x >= vx1 = finish lo hi
                    | otherwise = do
                        let m = atA arr (base + x)
                        wrA cov (x - vx0) m
                        if m > 0 then copy (x + 1) (if lo < 0 then x else lo) (x + 1)
                        else copy (x + 1) lo hi
                  finish lo hi = do
                    when (lo >= 0) $ emitRow env sink mclip cov y vx0 lo hi
                    rows (y + 1)
              copy vx0 (-1) 0
    rows vy0

-- | Fill a layer with a premultiplied colour.
fillLayer :: Layer s -> C4 -> ST s ()
fillLayer ly (C4 r g b a) = do
  let n = lyW ly * lyH ly
      pix = lyPix ly
      go !i | i >= n = return ()
            | otherwise = do
                wrA pix (4 * i) r
                wrA pix (4 * i + 1) g
                wrA pix (4 * i + 2) b
                wrA pix (4 * i + 3) a
                go (i + 1)
  when (a /= 0 || r /= 0 || g /= 0 || b /= 0) $ go 0

-- | Copy the overlapping part of one layer's pixels into another buffer of
-- the destination's geometry (used to seed non-isolated and knockout
-- layers with their backdrop).
copyRegion :: Layer s -> Layer s -> STUArray s Int Float -> ST s ()
copyRegion from to dst = do
  let IBox x0 y0 x1 y1 = iboxIntersect (IBox (lyX from) (lyY from) (lyX from + lyW from) (lyY from + lyH from))
                                       (IBox (lyX to) (lyY to) (lyX to + lyW to) (lyY to + lyH to))
      fp = lyPix from
  forM_ [y0 .. y1 - 1] $ \y -> do
    let fRow = ((y - lyY from) * lyW from - lyX from) * 4
        tRow = ((y - lyY to) * lyW to - lyX to) * 4
        go !i !e | i >= e = return ()
                 | otherwise = do
                     v <- rdA fp (fRow + i)
                     wrA dst (tRow + i) v
                     go (i + 1) e
    go (4 * x0) (4 * x1)

-- | Convert a layer to a straight-alpha RGBA image, rounding to nearest.
layerToImage :: Layer s -> ST s Image
layerToImage ly = do
  let w = lyW ly; h = lyH ly; n = w * h
      pix = lyPix ly
  out <- newArray (0, max 1 (4 * n) - 1) 0 :: ST s (STUArray s Int Word8)
  let to8 :: Float -> Word8
      to8 v = if v <= 0 then 0 else if v >= 1 then 255 else fromIntegral (truncate (v * 255 + 0.5) :: Int)
      go !i
        | i >= n = return ()
        | otherwise = do
            let j = 4 * i
            a <- rdA pix (j + 3)
            if a <= 0.5 / 255 then go (i + 1) else do
              r <- rdA pix j
              g <- rdA pix (j + 1)
              b <- rdA pix (j + 2)
              let ia = 1 / a
              wrA out j (to8 (r * ia))
              wrA out (j + 1) (to8 (g * ia))
              wrA out (j + 2) (to8 (b * ia))
              wrA out (j + 3) (to8 a)
              go (i + 1)
  go 0
  frozen <- unsafeFreeze out
  return (Image w h "vector" frozen)
