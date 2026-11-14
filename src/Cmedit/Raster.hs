{-# LANGUAGE BangPatterns #-}
-- | The vector rasteriser: a "Cmedit.Scene" display list in, an anti-aliased
-- RGBA "Cmedit.Image" out, at any resolution and crop (the scene → device
-- matrix is an input, so zooming is a different matrix and nothing else).
--
-- The pixel engine is "Cmedit.RasterCore" (exact-area coverage, sparse
-- bands, premultiplied float compositing, blend modes), the geometry is
-- "Cmedit.RasterGeom" (flattening, dashing, the stroker) and the filter
-- primitives are "Cmedit.RasterFilter". This module walks the scene and
-- decides, per command, how much machinery it needs; those decisions are
-- where both the correctness and the speed are.
--
-- __Layers only when the semantics need one.__ A group costs a layer the
-- size of its content (its device bounds intersected with the clip), which
-- is allocated, composited and dropped. Two cases never get one, because
-- drawing directly is exact rather than an approximation: a 'defaultGroup'
-- (opaque, normal, unmasked, unfiltered, non-knockout; an isolated one
-- only when nothing inside blends non-normally), whose children are drawn
-- into the parent as if the group were not there; and a group around a
-- single leaf with no mask, filter or knockout, which is the leaf painted
-- with the group's opacity folded into its coverage and the group's blend
-- mode used for its own compositing (one shape composited once is exactly
-- what the layer would have held). PDF's per-object @ca@ and blend modes
-- and SVG's @fill-opacity@ on a gradient all arrive in that second form, so
-- the common transparency in real documents allocates nothing. A
-- non-isolated group is only treated as one (backdrop copied in, group alpha
-- tracked, backdrop removed on the way out) when some descendant blends
-- non-normally; otherwise it is indistinguishable from an isolated group,
-- and cheaper.
--
-- __Strokes are computed in user space__ (so a pen under a non-uniform
-- scale is elliptical, as both formats require) with the flattening
-- tolerance divided by the matrix's expansion, then transformed and filled
-- non-zero. A stroke thinner than a device pixel is a different object: a
-- zero width (PDF's thinnest line) or a stroke-adjusted line that would come
-- out thinner than one pixel is flattened and dashed in user space and then
-- stroked one device pixel wide /in device space/.
--
-- __Glyph runs__ are filled glyph by glyph under non-zero (TrueType and CFF
-- outlines wind oppositely). With an opaque solid paint each glyph is
-- composited as it is rasterised: overlapping coverage composites to the
-- same colour, so this is the run's union to within anti-aliasing. Anything
-- else (translucent, gradient, non-normal blend, knockout) unions the
-- glyphs' coverage into one mask first and composites that once, so a
-- translucent run's overlaps do not double up.
--
-- __Images are area-averaged when minified__: the source is box-filtered
-- (exact fractional coverage, premultiplied) down to roughly the device
-- resolution along each of its own axes, which makes any rotation or skew
-- minify correctly, and only then sampled bilinearly or nearest. A
-- magnified image is sampled straight from its bytes, premultiplying four
-- texels at a time, so a large image costs no float copy of itself.
--
-- __Tiling patterns__ render one period of the pattern (with every
-- overlapping neighbour cell drawn into it, so cells larger than the step
-- tile correctly) into a texture at device resolution, cached per render by
-- key and device geometry, then sample it with wrap-around. The lattice is
-- pattern space's: cell @(i, j)@ is the cell rectangle moved by
-- @(i·xstep, j·ystep)@ /in pattern space/, then mapped through the whole
-- matrix chain. When the pattern matrix is axis-aligned and the period is a
-- whole number of device pixels the tile is rendered at the device's pixel
-- phase and sampled nearest, so it repeats with no resampling at all;
-- otherwise it is sampled bilinearly (exact period, at most half a texel of
-- softening). A wrapped texture is seamless by construction, which is why
-- it is preferred to drawing the cell once per copy: copies composited
-- separately leave a seam wherever content crosses a cell edge. Copies are
-- drawn directly only when few are visible and a full-resolution period
-- would not fit the texel cap (a huge cell, zoomed in).
--
-- __Caches, all per render and all charged to memory.__ Glyph coverage
-- ('fillGlyph': key × linear matrix × quarter-pixel phase), pattern tiles
-- (key × device geometry) and soft masks (key × matrix × visible box; a
-- PDF soft mask stays active across thousands of objects, and each would
-- otherwise re-render it) keep their entries charged to the memory ledger
-- up to a share of the cap, and are emptied when full. A negative key is
-- never cached.
--
-- __Bounds and budget.__ The output is refused above 'rasterMaxPixels'
-- (16 MP: the viewer renders the visible crop, never a whole zoomed page);
-- group nesting is capped at 'maxGroupDepth'. Every pixel buffer (layers,
-- masks, tiles, prescaled images, scratch) is /reserved/ against
-- 'roMaxLayerBytes' before it is allocated: one that would not fit is not
-- allocated and its command degrades (a group drawn without its layer, a
-- clip or mask that does not fit skipping its contents) and is counted as
-- skipped. Scoped buffers are released when their command ends; a refusal
-- is charged to the budget like the allocation it replaced, so a scene
-- cannot loop cheaply on refusals. The budget ('roBudget') is spent per
-- flattened edge, per covered pixel and per reserved pixel, checked before
-- every command and inside every per-element loop, so a hostile scene stops
-- and returns what it drew, with 'rsExhausted' set. Non-finite or absurd
-- coordinates skip their command; a singular matrix draws nothing. Nothing
-- here throws, and a @-DRASTER_SAFE@ build bounds-checks every array access
-- to keep it that way ('rdA').
module Cmedit.Raster
  ( -- * Rendering
    RenderOpts(..)
  , defaultRenderOpts
  , defaultBudget
  , RenderStats(..)
  , renderScene
  , fitView
    -- * Limits
  , rasterMaxPixels
  , defaultMaxLayerBytes
  , maxGroupDepth
  , maxNesting
  , maxLayerBytes
    -- * Geometry for front-ends
  , strokeToPath
  , dashPath
  , flattenPath
  , strokeExtent
  , cmdsBounds
    -- * Image reduction for front-ends
  , prescaleRGBA
  , prescaleMask
  ) where

import Control.Monad (when, unless, forM_, foldM)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeFreeze, numElements, STUArray(..))
import Data.Array.ST (newArray)
import Data.Array.Unboxed (UArray)
import qualified Data.Map.Strict as M
import Data.STRef
import Data.Word (Word8)
import Data.Bits (shiftR, (.&.))
import Data.Maybe (isNothing, isJust)
import GHC.Float (double2Float)

import Cmedit.Geom
import Cmedit.Image (Image(..))
import Cmedit.RasterCore
import Cmedit.RasterFilter
import Cmedit.RasterGeom
import Cmedit.Scene

------------------------------------------------------------------------------
-- Public types

data RenderOpts = RenderOpts
  { roWidth      :: !Int
  , roHeight     :: !Int
  , roView       :: !Affine   -- ^ Scene space → device pixels.
  , roBackground :: !Color    -- ^ Straight alpha; transparent allowed.
  , roBudget     :: !Int      -- ^ Work units: one per flattened edge, one per covered pixel (see 'defaultBudget').
  , roMaxLayerBytes :: !Int   -- ^ Live pixel memory (canvas, layers, masks, caches); see 'defaultMaxLayerBytes'.
  } deriving (Show)

-- | Enough for any real page at a few megapixels many times over (a
-- 2-megapixel page full of text spends a few million), and small enough
-- that a hostile scene stops within seconds.
defaultBudget :: Int
defaultBudget = 400 * 1000 * 1000

defaultRenderOpts :: Int -> Int -> Affine -> RenderOpts
defaultRenderOpts w h m = RenderOpts w h m transparent defaultBudget (defaultMaxLayerBytes w h)

-- | About three canvases' worth of float pixels (the canvas plus two
-- full-size layers), at least 16 MB so a small view can still filter and
-- tile, and never more than 512 MB.
defaultMaxLayerBytes :: Int -> Int -> Int
defaultMaxLayerBytes w h =
  let canvas = max 0 w * max 0 h * 16
  in min (512 * 1024 * 1024) (max (16 * 1024 * 1024) (3 * canvas))

data RenderStats = RenderStats
  { rsExhausted :: !Bool   -- ^ The budget ran out: the image is what was drawn so far.
  , rsTooLarge  :: !Bool   -- ^ The requested size was refused (the image is 0×0).
  , rsCommands  :: !Int
  , rsSegments  :: !Int    -- ^ Flattened edges rasterised.
  , rsPixels    :: !Int    -- ^ Coverage pixels swept.
  , rsSkipped   :: !Int    -- ^ Commands dropped (bad coordinates, depth or memory caps).
  , rsPeakBytes :: !Int    -- ^ High-water mark of live pixel memory (canvas included), never above the cap.
  , rsImagesDropped :: !Int  -- ^ Images, stencils and sampled paints not drawn because they would not fit the memory cap.
  , rsNotes     :: [String]  -- ^ What a caller should tell the user: dropped images, a truncated render.
  } deriving (Eq, Show)

-- | The largest canvas rendered: 16 MP is a 256 MB float canvas. The viewer
-- renders the visible crop, never a whole zoomed page, so this is far above
-- any real request.
rasterMaxPixels :: Int
rasterMaxPixels = 16 * 1000 * 1000

maxGroupDepth :: Int
maxGroupDepth = 48

-- | Containers of any kind (clips, transforms, groups) nested deeper than
-- this are skipped: a scene is a tree a file built, and a file can build a
-- deep one.
maxNesting :: Int
maxNesting = 256

-- | The ceiling 'defaultMaxLayerBytes' never exceeds.
maxLayerBytes :: Int
maxLayerBytes = 512 * 1024 * 1024

-- | The largest tile a pattern cell is rendered at, per side and in total.
maxTileSide, maxTilePixels :: Int
maxTileSide = 2048
maxTilePixels = 2 * 1024 * 1024

-- | Fit a scene rectangle into a @w × h@ box, preserving aspect, centred.
fitView :: Rect -> Int -> Int -> Affine
fitView (Rect x0 y0 x1 y1) w h
  | not (rw > 0 && rh > 0) || isInfinite rw || isInfinite rh || w <= 0 || h <= 0 =
      translateA (fin (-x0)) (fin (-y0))
  | otherwise =
      let s = min (fromIntegral w / rw) (fromIntegral h / rh)
          ox = (fromIntegral w - s * rw) / 2
          oy = (fromIntegral h - s * rh) / 2
      in Affine s 0 0 s (ox - s * x0) (oy - s * y0)
  where
    rw = x1 - x0
    rh = y1 - y0
    fin v = if isNaN v || isInfinite v then 0 else v

------------------------------------------------------------------------------
-- Rendering

data Ctx s = Ctx
  { cxEnv     :: !(Env s)
  , cxMax     :: !Int                       -- ^ The memory cap ('roMaxLayerBytes').
  , cxPending :: !(STRef s Int)             -- ^ Bytes reserved for the command being drawn (released when it ends).
  , cxTiles   :: !(Cache s TileKey Tex)
  , cxMasks   :: !(Cache s MaskKey (IBox, STUArray s Int Float))
  , cxGlyphs  :: !(Cache s GlyphKey GlyphCov)
  }

type TileKey = (Int, Int, Int, Int, Int, Int, Int, Int, Int)
type MaskKey = (Int, (Double, Double, Double, Double, Double, Double), IBox)
type GlyphKey = (Int, Int, Int, Int, Int, Int, Int)

-- | A glyph's coverage at one sub-pixel phase: the box relative to the
-- integer pixel its origin falls in, and the coverage over it.
data GlyphCov = GlyphCov !Int !Int !Int !Int !(UArray Int Float)

-- | A per-render cache whose entries stay charged to the memory ledger, up
-- to its own share of the cap; when full it is emptied (released) before
-- the next entry goes in. Entries live only for one render: the API is pure.
data Cache s k v = Cache
  { caMap   :: !(STRef s (M.Map k v))
  , caBytes :: !(STRef s Int)
  , caCap   :: !Int
  }

newCache :: Int -> ST s (Cache s k v)
newCache cap = Cache <$> newSTRef M.empty <*> newSTRef 0 <*> pure cap

cacheLookup :: Ord k => Cache s k v -> k -> ST s (Maybe v)
cacheLookup c k = M.lookup k <$> readSTRef (caMap c)

-- | Hand an entry whose bytes are already reserved to the cache; one too
-- big for it stays a transient of the current command instead.
cacheAdopt :: Ord k => Ctx s -> Cache s k v -> k -> v -> Int -> ST s ()
cacheAdopt ctx c k v bytes
  | bytes > caCap c = modifySTRef' (cxPending ctx) (+ bytes)
  | otherwise = do
      used <- readSTRef (caBytes c)
      when (used + bytes > caCap c) $ do
        release ctx used
        writeSTRef (caMap c) M.empty
        writeSTRef (caBytes c) 0
      modifySTRef' (caMap c) (M.insert k v)
      modifySTRef' (caBytes c) (+ bytes)

-- | Reserve pixel memory against the cap. Allocating and clearing it is work,
-- so it is charged to the budget whether or not it is granted; a refusal
-- counts as a skip and the caller draws without it.
reserve :: Ctx s -> Int -> ST s Bool
reserve ctx bytes = do
  let env = cxEnv ctx
  charge env (max 0 bytes `quot` 16)
  used <- rdA (envI env) slotLayerBytes
  if bytes < 0 || used + bytes > cxMax ctx
    then countSkip env >> return False
    else do
      wrA (envI env) slotLayerBytes (used + bytes)
      peak <- rdA (envI env) slotPeakBytes
      when (used + bytes > peak) $ wrA (envI env) slotPeakBytes (used + bytes)
      return True

release :: Ctx s -> Int -> ST s ()
release ctx bytes = do
  let env = cxEnv ctx
  used <- rdA (envI env) slotLayerBytes
  wrA (envI env) slotLayerBytes (used - bytes)

-- | Reserve memory that lives until the current command finishes.
transient :: Ctx s -> Int -> ST s Bool
transient ctx bytes = do
  ok <- reserve ctx bytes
  when ok $ modifySTRef' (cxPending ctx) (+ bytes)
  return ok

data St s = St
  { stCtm   :: !Affine
  , stClip  :: !(Clip s)
  , stLayer :: !(Layer s)
  , stDepth :: !Int       -- ^ Group / pattern nesting (each may cost a layer).
  , stNest  :: !Int       -- ^ Any container nesting (clips and transforms too).
  , stOp    :: !Float     -- ^ Opacity folded in by the single-leaf group path.
  , stBlend :: !Blend     -- ^ Blend mode set by the single-leaf group path.
  }

renderScene :: RenderOpts -> Scene -> (Image, RenderStats)
renderScene opts sc
  | w <= 0 || h <= 0 || w > rasterMaxPixels || h > rasterMaxPixels
    || w * h > rasterMaxPixels =
      (Image 0 0 "vector" emptyPix, RenderStats False True 0 0 0 0 0 0 ["picture too large to render"])
  | otherwise = runST $ do
      env <- newEnv (max 0 (roBudget opts))
      pending <- newSTRef 0
      let cap = max 0 (roMaxLayerBytes opts)
      tiles <- newCache (cap `quot` 8)
      masks <- newCache (cap `quot` 8)
      glyphs <- newCache (min (32 * 1024 * 1024) (cap `quot` 16))
      let ctx = Ctx env cap pending tiles masks glyphs
          box = IBox 0 0 w h
      -- The canvas is always allocated; the cap governs everything else.
      wrA (envI env) slotLayerBytes (layerBytes box)
      wrA (envI env) slotPeakBytes (layerBytes box)
      base <- newLayer box
      fillLayer base (premul (roBackground opts))
      let st = St (roView opts) (ClipRect 0 0 (fromIntegral w) (fromIntegral h)) base 0 0 1 BlendNormal
      drawCmds ctx st (scCmds sc)
      img <- layerToImage base
      let i k = rdA (envI env) k
      ex <- i slotExhausted
      bud <- i slotBudget
      cmds <- i slotCmds
      segs <- i slotSegs
      pxs <- i slotPixels
      sk <- i slotSkipped
      peak <- i slotPeakBytes
      dropped <- i slotImagesDropped
      let exhausted = ex /= 0 || bud <= 0
          notes = [ show dropped ++ (if dropped == 1 then " image" else " images") ++ " not drawn (over the memory cap)" | dropped > 0 ]
                  ++ [ "picture incomplete (work budget reached)" | exhausted ]
      return (img, RenderStats exhausted False cmds segs pxs sk peak dropped notes)
  where
    w = roWidth opts
    h = roHeight opts

emptyPix :: UArray Int Word8
emptyPix = runST (newArray (0, 0) 0 >>= unsafeFreeze')
  where unsafeFreeze' :: STUArray s Int Word8 -> ST s (UArray Int Word8)
        unsafeFreeze' = unsafeFreeze

premul :: Color -> C4
premul (Color r g b a) =
  let a' = c01 a in C4 (f (c01 r * a')) (f (c01 g * a')) (f (c01 b * a')) (f a')
  where f = double2Float

c01 :: Double -> Double
c01 v | v >= 1 = 1
      | v > 0 = v
      | otherwise = 0

drawCmds :: Ctx s -> St s -> [Cmd] -> ST s ()
drawCmds _ _ [] = return ()
drawCmds ctx st (c : cs) = do
  ok <- budgetLeft (cxEnv ctx)
  when ok $ do
    drawCmd ctx st c
    drawCmds ctx st cs

drawCmd :: Ctx s -> St s -> Cmd -> ST s ()
drawCmd ctx st cmd = do
  p0 <- readSTRef (cxPending ctx)
  drawCmd' ctx st cmd
  p1 <- readSTRef (cxPending ctx)
  release ctx (p1 - p0)
  writeSTRef (cxPending ctx) p0

drawCmd' :: Ctx s -> St s -> Cmd -> ST s ()
drawCmd' ctx st cmd = do
  let env = cxEnv ctx
  n <- rdA (envI env) slotCmds
  wrA (envI env) slotCmds (n + 1)
  charge env 8
  case cmd of
    CFill m rule paint path -> do
      let dev = composeA (stCtm st) m
      case devPathBounds dev path of
        Nothing -> return ()
        Just r | not (saneRect r) -> countSkip env
               | otherwise -> do
                   let area = fillBoxFor st r
                   unless (iboxEmpty area) $ do
                     sh <- mkShader ctx st dev paint area
                     fillPath ctx st dev rule path sh Nothing
    CStroke m ss paint path -> strokeCmd ctx st (composeA (stCtm st) m) ss paint path
    CGlyphs m paint gs -> glyphsCmd ctx st (composeA (stCtm st) m) paint gs
    CImage m img opts -> imageCmd ctx st (composeA (stCtm st) m) img opts
    CStencil m mask paint interp -> stencilCmd ctx st (composeA (stCtm st) m) mask paint interp
    CGouraud m tris -> gouraudCmd ctx st (composeA (stCtm st) m) (Left tris)
    CGouraudRamp m stops tris -> gouraudCmd ctx st (composeA (stCtm st) m) (Right (mkStops stops, tris))
    CClip shapes kids -> nest $ \st' -> clipCmd ctx st' shapes kids
    CGroup opts kids -> nest $ \st' -> groupCmd ctx st' opts kids
    CTransform m kids -> nest $ \st' -> drawCmds ctx st' { stCtm = composeA (stCtm st) m } kids
  where
    nest k | stNest st >= maxNesting = countSkip (cxEnv ctx)
           | otherwise = k st { stNest = stNest st + 1 }

-- The pixels a fill may touch: its device bounds within the clip and layer.
fillBoxFor :: St s -> Rect -> IBox
fillBoxFor st r =
  let ly = stLayer st
  in iboxIntersect (rectToIBox r)
       (iboxIntersect (clipIBox (stClip st)) (IBox (lyX ly) (lyY ly) (lyX ly + lyW ly) (lyY ly + lyH ly)))

-- The clip to apply over a box, or Nothing when the box is wholly inside a
-- rectangular clip.
clipOver :: Clip s -> IBox -> Maybe (Clip s)
clipOver cl@(ClipRect x0 y0 x1 y1) (IBox bx0 by0 bx1 by1)
  | x0 <= fromIntegral bx0 && y0 <= fromIntegral by0
    && x1 >= fromIntegral bx1 && y1 >= fromIntegral by1 = Nothing
  | otherwise = Just cl
clipOver cl _ = Just cl

paintSink :: St s -> Shader -> Maybe Stencil -> Sink s
paintSink st sh mst = SkPaint (stLayer st) sh (stBlend st) (stOp st) mst

-- Fill a path under a device matrix with a shader.
fillPath :: Ctx s -> St s -> Affine -> FillRule -> Path -> Shader -> Maybe Stencil -> ST s ()
fillPath ctx st dev rule path sh mst = case sh of
  ShNone -> return ()
  _ -> case devPathBounds dev path of
    Nothing -> return ()
    Just r
      | not (saneRect r) -> countSkip (cxEnv ctx)
      | otherwise -> do
      let box@(IBox bx by bx1 by1) = fillBoxFor st r
          env = cxEnv ctx
      unless (iboxEmpty box) $ do
        beginFill env
        ok <- flattenFill env dev (fromIntegral bx) (fromIntegral by) (bx1 - bx) (by1 - by) path
        if ok
          then rasterFill env bx by (bx1 - bx) (by1 - by) rule (clipOver (stClip st) box) (paintSink st sh mst)
          else skipUnlessExhausted env

-- A fill whose flattening gave up: a bad coordinate is a skipped command,
-- the budget running out is not (it is reported as exhausted).
skipUnlessExhausted :: Env s -> ST s ()
skipUnlessExhausted env = do
  ok <- budgetLeft env
  when ok $ countSkip env

-- Device bounds of a path's control points. A non-finite or absurd
-- coordinate (beyond 1e12, where no clip arithmetic is meaningful) yields a
-- NaN rectangle, which every caller treats as "skip this command".
devPathBounds :: Affine -> Path -> Maybe Rect
devPathBounds (Affine a b c d e f) = go False False 0 0 0 0
  where
    go :: Bool -> Bool -> Double -> Double -> Double -> Double -> Path -> Maybe Rect
    go has bad !x0 !y0 !x1 !y1 segs = case segs of
      [] | not has -> Nothing
         | bad -> Just (Rect nan nan nan nan)
         | otherwise -> Just (Rect x0 y0 x1 y1)
      (s : r) -> case s of
        MoveTo p -> pt p r
        LineTo p -> pt p r
        QuadTo p q -> pt p (LineTo q : r)
        CubicTo p q u -> pt p (LineTo q : LineTo u : r)
        ClosePath -> go has bad x0 y0 x1 y1 r
      where
        pt (Pt x y) r =
          let px = a * x + c * y + e; py = b * x + d * y + f
              bad' = bad || not (abs px < 1e12 && abs py < 1e12)
          in if has then go True bad' (min x0 px) (min y0 py) (max x1 px) (max y1 py) r
             else go True bad' px py px py r
    nan = 0 / 0

------------------------------------------------------------------------------
-- Paints

-- | Resolve a paint against the command's device matrix; the box is where
-- it will be sampled (a pattern uses it to decide how to render itself).
mkShader :: Ctx s -> St s -> Affine -> Paint -> IBox -> ST s Shader
mkShader ctx st dev paint area = case paint of
  PSolid col -> let C4 r g b a = premul col in
    return (if a <= 0 then ShNone else ShSolid r g b a)
  PLinear pm (Pt x0 y0) (Pt x1 y1) stops sp -> return $
    case (stops, invertA (composeA dev pm)) of
      ([], _) -> ShNone
      (_, Nothing) -> ShNone
      ([Stop _ c], _) -> solidOf c
      (_, Just (Affine ia ib ic id' ie if')) ->
        let dx = x1 - x0; dy = y1 - y0; l2 = dx * dx + dy * dy
        in if not (l2 > 1e-24) || isInfinite l2
             then solidOf (let Stop _ c = last stops in c)
             else ShLinear ((ia * dx + ib * dy) / l2) ((ic * dx + id' * dy) / l2)
                           (((ie - x0) * dx + (if' - y0) * dy) / l2) (mkStops stops) sp
  PRadial pm (Pt cx0 cy0) r0 (Pt cx1 cy1) r1 stops sp -> return $
    case (stops, invertA (composeA dev pm)) of
      ([], _) -> ShNone
      (_, Nothing) -> ShNone
      ([Stop _ c], _) -> solidOf c
      (_, Just inv)
        | not (all finite [cx0, cy0, r0, cx1, cy1, r1]) -> ShNone
        | otherwise -> ShRadial inv (Radial cx0 cy0 (max 0 r0) cx1 cy1 (max 0 r1)) (mkStops stops) sp
  PSampled pm img sp -> do
    let edge = case sp of
          SpreadRepeat -> 1
          SpreadReflect -> 2
          SpreadNone -> 3
          _ -> 0
        m = composeA dev pm
    ok <- transient ctx (prescaleBytes m (imgW img) (imgH img) 4)
    unless ok $ imageDropped ctx
    return (if ok then imageShader m img edge InterpSmooth 1 else ShNone)
  PTiling key pm cell xs ys cmds -> tileShader ctx st (composeA dev pm) key cell xs ys cmds area
  where
    solidOf c = let C4 r g b a = premul c in if a <= 0 then ShNone else ShSolid r g b a
    finite v = not (isNaN v || isInfinite v)

-- A shader sampling an image mapped onto the unit square by @m@ (unit
-- square → device), prescaled when minified.
imageShader :: Affine -> Image -> Int -> ImgInterp -> Double -> Shader
imageShader m img edge interp opacity
  | w <= 0 || h <= 0 || numElements (imgPix img) < 4 * w * h = ShNone
  | otherwise = case invertA m of
      Nothing -> ShNone
      Just inv ->
        let (w', h') = prescaleDims m w h
            tex | w' < w || h' < h = Tex8 w' h' (prescaleRGBA w h (imgPix img) w' h')
                | otherwise = Tex8 w h (imgPix img)
            near = interp == InterpNearest && w' >= w && h' >= h
        in ShTex (composeA (scaleA (fromIntegral w') (fromIntegral h')) inv) tex edge near
                 (double2Float (c01 opacity))
  where
    w = imgW img
    h = imgH img

-- The resolution an image needs along each of its own axes: its pixel
-- count, or the device pixels it spans if that is fewer.
prescaleDims :: Affine -> Int -> Int -> (Int, Int)
prescaleDims (Affine a b c d _ _) w h =
  let kx = sqrt (a * a + b * b)        -- device pixels across the whole image width
      ky = sqrt (c * c + d * d)
      dim n k | not (k < fromIntegral n) = n
              | otherwise = max 1 (ceiling (k - 1e-6))
  in (dim w kx, dim h ky)

-- | Exact area-averaging box filter from @w × h@ to @w' × h'@ (each no
-- larger), straight RGBA bytes in and out, averaged premultiplied.
--
-- __Memory is independent of the source size.__ The obvious separable
-- filter keeps its horizontal pass, @w' × h@ floats, and for a 36 MP photo
-- on a page that was more than the whole render's memory cap, so the photo
-- was refused. This streams instead: each output row is built from the
-- source rows it covers, each reduced horizontally into one reusable row
-- and accumulated, so the live state is the result plus two rows. A source
-- row straddling two output rows is reduced twice, which costs @h'@ extra
-- rows out of @h@. The result is stored as 8-bit straight RGBA (a quarter
-- of the float texture's memory, and sampled exactly like a decoded image).
--
-- __It is also fast:__ a span's interior pixels have weight 1, so they are
-- summed in integers (premultiplied by an integer multiply), and only the
-- two edge pixels of each span take fractional weights.
prescaleRGBA :: Int -> Int -> UArray Int Word8 -> Int -> Int -> UArray Int Word8
prescaleRGBA w h px w' h' = runST $ do
  out <- newArray (0, max 1 (4 * w' * h') - 1) 0 :: ST s (STUArray s Int Word8)
  hrow <- newArray (0, 4 * w' + 3) 0 :: ST s (STUArray s Int Int)
  acc <- newArray (0, 4 * w' + 3) 0 :: ST s (STUArray s Int Int)
  let sx = fromIntegral w / fromIntegral w' :: Double
      sy = fromIntegral h / fromIntegral h' :: Double
  -- Column spans are the same for every row: computed once.
  (spanI, spanW) <- columnSpans w w'
  let -- One source row, reduced to w' premultiplied columns (unnormalised,
      -- in 1/256ths of a texel's weight).
      reduceRow !y = do
        let rowBase = y * w
            col !j
              | j >= w' = return ()
              | otherwise = do
                  let i0 = atA spanI (2 * j); i1 = atA spanI (2 * j + 1)
                      wl = atA spanW (2 * j); wr = atA spanW (2 * j + 1)
                      o = 4 * j
                      -- interior pixels: integer sums, written once at the end
                      inner !k !e !r !g !b !a
                        | k >= e = do
                            let vl = rgbaWord px (rowBase + i0); vr = rgbaWord px (rowBase + i1)
                                la = fromIntegral (vl `shiftR` 24) :: Int
                                ra = fromIntegral (vr `shiftR` 24) :: Int
                                edge sh = wl * (fromIntegral ((vl `shiftR` sh) .&. 0xff) * la)
                                          + wr * (fromIntegral ((vr `shiftR` sh) .&. 0xff) * ra)
                            wrA hrow o (r * 256 + edge 0)
                            wrA hrow (o + 1) (g * 256 + edge 8)
                            wrA hrow (o + 2) (b * 256 + edge 16)
                            wrA hrow (o + 3) (a * 256 + wl * la + wr * ra)
                            col (j + 1)
                        | otherwise = do
                            let v = rgbaWord px k
                                al = fromIntegral (v `shiftR` 24) :: Int
                                ch sh = fromIntegral ((v `shiftR` sh) .&. 0xff) * al
                            inner (k + 1) e (r + ch 0) (g + ch 8) (b + ch 16) (a + al)
                  if i1 > i0 + 1
                    then inner (rowBase + i0 + 1) (rowBase + i1) 0 0 0 0
                    else inner 0 0 0 0 0 0
        col 0
      addRow !wt = let go !k | k >= 4 * w' = return ()
                                | otherwise = do
                                    v <- rdA hrow k
                                    o <- rdA acc k
                                    wrA acc k (o + v * wt)
                                    go (k + 1)
                   in go 0
      clear = let go !k | k >= 4 * w' = return ()
                        | otherwise = wrA acc k 0 >> go (k + 1)
              in go 0
      -- Sums of channel × alpha and of alpha, each in 1/65536ths of a
      -- texel's weight, over an area of sx·sy texels → straight bytes:
      -- alpha is its mean, a channel its sum over alpha's.
      emit !j = do
        let norm = 1 / (sx * sy * 65536)
            go !c
              | c >= w' = return ()
              | otherwise = do
                  let o = 4 * c
                  a <- rdA acc (o + 3)
                  let al = fromIntegral a * norm                -- alpha in [0, 255]
                      ob = 4 * (j * w' + c)
                      b8 v = let r = truncate (v + 0.5) :: Int in fromIntegral (max 0 (min 255 r)) :: Word8
                  if al < 0.5 then go (c + 1) else do
                    r <- rdA acc o
                    g <- rdA acc (o + 1)
                    b <- rdA acc (o + 2)
                    let k = 1 / fromIntegral a :: Double     -- straight = premult / alpha
                    wrA out ob (b8 (fromIntegral r * k))
                    wrA out (ob + 1) (b8 (fromIntegral g * k))
                    wrA out (ob + 2) (b8 (fromIntegral b * k))
                    wrA out (ob + 3) (b8 al)
                    go (c + 1)
        go 0
      rowsOut !j
        | j >= h' = return ()
        | otherwise = do
            let ya = fromIntegral j * sy; yb = ya + sy
                r0 = min (h - 1) (floor ya) :: Int
                r1 = min (h - 1) (max r0 (ceiling yb - 1)) :: Int
                srcRows !r
                  | r > r1 = return ()
                  | otherwise = do
                      reduceRow r
                      addRow (fixed (min (fromIntegral r + 1) yb - max (fromIntegral r) ya))
                      srcRows (r + 1)
            clear
            srcRows r0
            emit j
            rowsOut (j + 1)
  rowsOut 0
  unsafeFreeze out

-- A fractional weight as 1/256ths. Edge weights are at most one texel and
-- a whole span at most the reduction factor, so the sums stay far inside an
-- Int; an eighth of a percent of an edge texel is below what 8-bit output
-- can show.
fixed :: Double -> Int
fixed v = round (v * 256)
{-# INLINE fixed #-}

-- The source span of each output column when @w@ texels are box-averaged
-- down to @w'@: first and last texel, and the fractional weights of those
-- two in 1/256ths (the texels between them weigh 1).
columnSpans :: Int -> Int -> ST s (UArray Int Int, UArray Int Int)
columnSpans w w' = do
  is <- newArray (0, max 1 (2 * w') - 1) 0 :: ST s (STUArray s Int Int)
  ws <- newArray (0, max 1 (2 * w') - 1) 0 :: ST s (STUArray s Int Int)
  let sx = fromIntegral w / fromIntegral w' :: Double
  forM_ [0 .. w' - 1] $ \j -> do
    let xa = fromIntegral j * sx; xb = xa + sx
        i0 = min (w - 1) (floor xa) :: Int
        i1 = min (w - 1) (max i0 (ceiling xb - 1)) :: Int
    wrA is (2 * j) i0
    wrA is (2 * j + 1) i1
    wrA ws (2 * j) (fixed (if i1 > i0 then fromIntegral (i0 + 1) - xa else sx))
    wrA ws (2 * j + 1) (fixed (if i1 > i0 then xb - fromIntegral i1 else 0))
  a <- unsafeFreeze is
  b <- unsafeFreeze ws
  return (a, b)

-- | The same for a single-channel mask (bytes in, bytes out), streamed the
-- same way.
prescaleMask :: Int -> Int -> UArray Int Word8 -> Int -> Int -> UArray Int Word8
prescaleMask w h px w' h' = runST $ do
  out <- newArray (0, max 1 (w' * h') - 1) 0 :: ST s (STUArray s Int Word8)
  hrow <- newArray (0, w' + 1) 0 :: ST s (STUArray s Int Double)
  acc <- newArray (0, w' + 1) 0 :: ST s (STUArray s Int Double)
  let sx = fromIntegral w / fromIntegral w' :: Double
      sy = fromIntegral h / fromIntegral h' :: Double
  (spanI, spanW) <- columnSpans w w'
  let reduceRow !y = do
        let rowBase = y * w
            col !j
              | j >= w' = return ()
              | otherwise = do
                  let i0 = atA spanI (2 * j); i1 = atA spanI (2 * j + 1)
                      wl = fromIntegral (atA spanW (2 * j)) / 256 :: Double
                      wr = fromIntegral (atA spanW (2 * j + 1)) / 256 :: Double
                      at i = fromIntegral (atA px (rowBase + i)) :: Int
                      inner !i !s | i >= i1 = s
                                  | otherwise = inner (i + 1) (s + at i)
                      mid = if i1 > i0 + 1 then inner (i0 + 1) 0 else 0
                  wrA hrow j (fromIntegral mid + wl * fromIntegral (at i0) + wr * fromIntegral (at i1))
                  col (j + 1)
        col 0
      rowsOut !j
        | j >= h' = return ()
        | otherwise = do
            let ya = fromIntegral j * sy; yb = ya + sy
                r0 = min (h - 1) (floor ya) :: Int
                r1 = min (h - 1) (max r0 (ceiling yb - 1)) :: Int
                srcRows !r
                  | r > r1 = return ()
                  | otherwise = do
                      reduceRow r
                      let wt = min (fromIntegral r + 1) yb - max (fromIntegral r) ya
                          go !c | c >= w' = return ()
                                | otherwise = do
                                    v <- rdA hrow c
                                    o <- rdA acc c
                                    wrA acc c (o + v * wt)
                                    go (c + 1)
                      go 0
                      srcRows (r + 1)
                clear !c | c >= w' = return ()
                         | otherwise = wrA acc c 0 >> clear (c + 1)
                emit !c | c >= w' = return ()
                        | otherwise = do
                            v <- rdA acc c
                            let r = truncate (v / (sx * sy) + 0.5) :: Int
                            wrA out (j * w' + c) (fromIntegral (max 0 (min 255 r)))
                            emit (c + 1)
            clear 0
            srcRows r0
            emit 0
            rowsOut (j + 1)
  rowsOut 0
  unsafeFreeze out

-- Tiling patterns. When only a few periods cover the area being painted
-- (a large cell, or a small fill) the pattern is drawn directly into a
-- device-space layer over that area, one clipped copy of the cell per
-- lattice position: exact, with no resampling at all, and no texture
-- larger than the area. Otherwise one period is rendered into a texture at
-- device resolution and sampled with wrap-around.
tileShader :: Ctx s -> St s -> Affine -> Int -> Rect -> Double -> Double -> [Cmd] -> IBox -> ST s Shader
tileShader ctx st p key cell@(Rect cx0 cy0 cx1 cy1) xs0 ys0 cmds area
  | not (xs > 0 && ys > 0) || isInfinite xs || isInfinite ys = return ShNone
  | not (lx > 1e-9 && ly > 1e-9) || isNaN lx || isNaN ly || isInfinite lx || isInfinite ly = return ShNone
  | rectEmpty cell || not (saneRect cell) || iboxEmpty area = return ShNone
  | stDepth st >= maxGroupDepth = countSkip (cxEnv ctx) >> return ShNone
  | otherwise = case invertA p of
      Nothing -> return ShNone
      Just pinv
        | tileTooBig, Just (i0, i1, j0, j1) <- fewCopies pinv -> direct i0 i1 j0 j1
        | otherwise -> texture pinv
  where
    xs = abs xs0
    ys = abs ys0
    Pt ex ey = applyLinearA p (Pt xs 0)
    Pt fx fy = applyLinearA p (Pt 0 ys)
    lx = sqrt (ex * ex + ey * ey)
    ly = sqrt (fx * fx + fy * fy)
    frac v = v - fromIntegral (floor v :: Int)
    qz v = if isNaN v || isInfinite v then 0 else round (v * 1024) :: Int
    IBox ax0 ay0 ax1 ay1 = area
    cellPath = rectPath cx0 cy0 (cx1 - cx0) (cy1 - cy0)

    -- The lattice range whose cells meet the area, if it is small.
    fewCopies pinv =
      let Rect qx0 qy0 qx1 qy1 = transformRect pinv (Rect (fromIntegral ax0) (fromIntegral ay0) (fromIntegral ax1) (fromIntegral ay1))
          i0 = (qx0 - cx1) / xs; i1 = (qx1 - cx0) / xs
          j0 = (qy0 - cy1) / ys; j1 = (qy1 - cy0) / ys
          ok v = abs v < 1e6
      in if all ok [i0, i1, j0, j1] && (i1 - i0 + 1) * (j1 - j0 + 1) <= 9
           then Just (ceiling i0, floor i1, ceiling j0, floor j1)
           else Nothing

    -- Would a full-resolution period exceed the texel cap (and so be
    -- rendered blurred)? Only then is drawing copies directly worth it:
    -- copies composited separately conflate where content crosses a cell
    -- edge (two half-covered pixels make a three-quarter one, a visible
    -- seam), which a wrapped texture never does.
    tileTooBig =
      let t v = if v < 1e9 then ceiling v else 1000000000 :: Int
      in fromIntegral (t lx) * fromIntegral (t ly) > (fromIntegral (tileCap area) :: Double)

    direct i0 i1 j0 j1 = do
      let w = ax1 - ax0
      -- lives as long as the fill that samples it
      ok <- transient ctx (layerBytes area)
      if not ok then return ShNone else do
        layer <- newLayer area
        let st' = St identityA (ClipRect (fromIntegral ax0) (fromIntegral ay0) (fromIntegral ax1) (fromIntegral ay1))
                     layer (stDepth st + 1) (stNest st + 1) 1 BlendNormal
        forM_ [ (i, j) | j <- [j0 .. j1], i <- [i0 .. i1] ] $ \(i, j) ->
          drawCmds ctx st' [CClip [ClipShape (composeA p (translateA (fromIntegral i * xs) (fromIntegral j * ys))) NonZero cellPath]
                                  [CTransform (composeA p (translateA (fromIntegral i * xs) (fromIntegral j * ys))) cmds]]
        frozen <- unsafeFreeze (lyPix layer)
        return (ShScratch ax0 ay0 w (ay1 - ay0) frozen)

    texture pinv = do
      let Affine pa pb pc pd _ _ = p
          axis = abs pb <= 1e-9 * (abs pa + abs pd) && abs pc <= 1e-9 * (abs pa + abs pd)
          exact v = abs (v - fromIntegral (round v :: Int)) < 1e-6
          aligned = axis && exact lx && exact ly
          fit v = max 1 (min maxTileSide (if aligned then round v else ceiling (v - 1e-9))) :: Int
          tw0 = fit lx; th0 = fit ly
          -- More texels than the area has pixels can only happen under a
          -- near-singular matrix (otherwise 64+ periods would not fit), and
          -- then most of them would never be sampled.
          cap = tileCap area
          shrink = if tw0 * th0 > cap
                     then sqrt (fromIntegral cap / fromIntegral (tw0 * th0)) else 1 :: Double
          tw = max 1 (floor (fromIntegral tw0 * shrink)) :: Int
          th = max 1 (floor (fromIntegral th0 * shrink)) :: Int
          sxT = fromIntegral tw / xs; syT = fromIntegral th / ys
          -- Device position of the period origin, for pixel phase.
          Pt ox oy = applyA p (Pt cx0 cy0)
          phx = if aligned then (if pa > 0 then frac ox else frac (-ox)) else 0
          phy = if aligned then (if pd > 0 then frac oy else frac (-oy)) else 0
          -- pattern space → tile pixels
          q = composeA (translateA phx phy) (composeA (scaleA sxT syT) (translateA (-cx0) (-cy0)))
          keyT = ( key, tw, th, qz phx, qz phy, qz (pa * 1e3), qz (pb * 1e3), qz (pc * 1e3), qz (pd * 1e3) )
      hit <- if key >= 0 then cacheLookup (cxTiles ctx) keyT else return Nothing
      case hit of
        Just t -> return (ShTex (composeA q pinv) t 1 aligned 1)
        Nothing -> do
          let bytes = layerBytes (IBox 0 0 tw th)
          ok <- reserve ctx bytes
          if not ok then return ShNone else do
            t <- renderTile q tw th
            -- a cached tile stays charged while it is cached
            if key >= 0 then cacheAdopt ctx (cxTiles ctx) keyT t bytes
            else modifySTRef' (cxPending ctx) (+ bytes)
            return (ShTex (composeA q pinv) t 1 aligned 1)

    renderTile q tw th = do
      let box = IBox 0 0 tw th
      layer <- newLayer box
      let Rect ux0 uy0 ux1 uy1 = transformRect q cell
          cw = ux1 - ux0; chh = uy1 - uy0
          twD = fromIntegral tw; thD = fromIntegral th
          range lo hi per = let iMin = floor ((0 - hi) / per) + 1; iMax = ceiling ((per - lo) / per) - 1
                            in [max (-4) iMin .. min 4 iMax] :: [Int]
          copies = [ (i, j) | i <- range ux0 (ux0 + cw) twD, j <- range uy0 (uy0 + chh) thD ]
      forM_ copies $ \(i, j) -> do
        let ox' = fromIntegral i * twD; oy' = fromIntegral j * thD
            clip = ClipRect (max 0 (ux0 + ox')) (max 0 (uy0 + oy')) (min twD (ux1 + ox')) (min thD (uy1 + oy'))
            st'' = St (composeA (translateA ox' oy') q) clip layer (stDepth st + 1) (stNest st + 1) 1 BlendNormal
        unless (iboxEmpty (clipIBox clip)) $ drawCmds ctx st'' cmds
      frozen <- unsafeFreeze (lyPix layer)
      return (TexF tw th frozen)

-- The most texels a pattern period is rendered with for a given area.
tileCap :: IBox -> Int
tileCap area = min maxTilePixels (max 4096 (4 * iboxW area * iboxH area))

------------------------------------------------------------------------------
-- Strokes

strokeCmd :: Ctx s -> St s -> Affine -> StrokeStyle -> Paint -> Path -> ST s ()
strokeCmd ctx st dev ss paint path
  | not (ex > 0) || isInfinite ex = return ()
  | otherwise = case devPathBounds dev path of
      Nothing -> return ()
      Just r -> do
        let reach = if hair then 2 else strokeExtent ss * ex + 2
            box = fillBoxFor st (rectInflate reach r)
        unless (iboxEmpty box) $ do
          sh <- mkShader ctx st dev paint box
          case sh of
            ShNone -> return ()
            _ -> do
              let tolU = flatTol / ex
                  pls = flattenPolylines tolU path
                  pls' = case validDash (ssDash ss) of
                           Just pat -> dashPolylines pat (ssDashPhase ss) pls
                           Nothing -> pls
                  polys
                    | hair =
                        let toDev (PolyLine vs c d) =
                              PolyLine [ let Pt x y = applyA dev (Pt vx vy) in Vtx x y k | Vtx vx vy k <- vs ]
                                       c (applyLinearA dev d)
                        in strokePolylines ss { ssWidth = 1 } flatTol (map toDev pls')
                    | otherwise = map (map (applyA dev)) (strokePolylines ss tolU pls')
              fillPolys ctx st polys NonZero sh
  where
    ex = expansionA dev
    smin = abs (determinantA dev) / ex
    w = ssWidth ss
    hair = not (w > 0) || (ssAdjust ss && w * smin < 1)

-- Fill device-space polygons (non-zero union) with a shader.
fillPolys :: Ctx s -> St s -> [[Pt]] -> FillRule -> Shader -> ST s ()
fillPolys ctx st polys rule sh = case polysBounds polys of
  Nothing -> return ()
  Just r | not (saneRect r) -> countSkip (cxEnv ctx)
  Just r -> do
    let box@(IBox bx by bx1 by1) = fillBoxFor st r
        env = cxEnv ctx
    unless (iboxEmpty box) $ do
        beginFill env
        -- each polygon is closed, so stopping when the budget runs out
        -- leaves a smaller stroke, not broken winding
        let add [] = return ()
            add (p : ps) = do
              ok <- budgetLeft env
              when ok $ addPolygon env (fromIntegral bx) (fromIntegral by) (bx1 - bx) (by1 - by) p >> add ps
        add polys
        rasterFill env bx by (bx1 - bx) (by1 - by) rule (clipOver (stClip st) box) (paintSink st sh Nothing)

saneRect :: Rect -> Bool
saneRect (Rect a b c d) = abs a < 1e12 && abs b < 1e12 && abs c < 1e12 && abs d < 1e12

-- Bounds of device points; a NaN rectangle when any point is not finite or
-- beyond 1e12 (min and max would otherwise quietly drop a NaN, and the edge
-- buffer would receive it).
polysBounds :: [[Pt]] -> Maybe Rect
polysBounds = go False Nothing . concat
  where
    go bad acc [] = if bad then Just (Rect nan nan nan nan) else acc
    go bad acc (Pt x y : ps) =
      let bad' = bad || not (abs x < 1e12 && abs y < 1e12)
          acc' = case acc of
            Nothing -> Just (Rect x y x y)
            Just (Rect a b c d) -> Just (Rect (min a x) (min b y) (max c x) (max d y))
      in bad' `seq` acc' `seq` go bad' acc' ps
    nan = 0 / 0

------------------------------------------------------------------------------
-- Glyphs

glyphsCmd :: Ctx s -> St s -> Affine -> Paint -> [Glyph] -> ST s ()
glyphsCmd ctx st dev paint glyphs = do
  -- Each glyph's bounds grow by a pixel: a cached glyph is drawn at its
  -- quarter-pixel phase, up to an eighth of a pixel from its true position,
  -- so its coverage can reach a pixel beyond the exact outline's pixel box,
  -- and the run's mask must hold all of it ('fillGlyph').
  let bounds = [ rectInflate 1 r | Glyph ga _ path <- glyphs, Just r <- [devPathBounds (composeA dev ga) path], saneRect r ]
      area = case bounds of
        [] -> IBox 0 0 0 0
        (b0 : brest) -> fillBoxFor st (foldl rectUnion b0 brest)
  sh <- if iboxEmpty area then return ShNone else mkShader ctx st dev paint area
  let env = cxEnv ctx
      direct = case sh of
        ShSolid _ _ _ a -> a >= 1 && stOp st >= 1 && stBlend st == BlendNormal
                           && isNothing (lyKnock (stLayer st))
        _ -> False
  case sh of
    ShNone -> return ()
    _ | direct ->
          let go [] = return ()
              go (Glyph ga gk path : rest) = do
                ok <- budgetLeft env
                when ok $ do
                  fillGlyph ctx st (composeA dev ga) gk path (Just (clipOver' st)) (paintSink st sh Nothing)
                  go rest
          in go glyphs
      | otherwise -> do
              let box@(IBox bx by bx1 by1) = area
                  mw = bx1 - bx; mh = by1 - by
              unless (iboxEmpty box) $ withMem ctx (mw * mh * 4) (drawUnclipped sh glyphs) $ do
                mask <- newArray (0, max 1 (mw * mh) - 1) 0
                let go [] = return ()
                    go (Glyph ga gk path : rest) = do
                      ok <- budgetLeft env
                      when ok $ do
                        fillGlyph ctx st (composeA dev ga) gk path Nothing (SkMask bx by mw mask)
                        go rest
                go glyphs
                emitMask env box mask (clipOver (stClip st) box) (paintSink st sh Nothing)
  where
    clipOver' s = stClip s
    drawUnclipped sh gs = forM_ gs $ \(Glyph ga _ path) ->
      fillPath ctx st (composeA dev ga) NonZero path sh Nothing

-- Fill one glyph into a sink, through the per-render glyph cache when its
-- key allows. The cache holds coverage, keyed by the glyph's key, the
-- linear part of its device matrix and its sub-pixel phase quantised to a
-- quarter pixel in x and y (so a glyph lands within an eighth of a pixel of
-- where it would uncached, the usual sub-pixel positioning of text
-- renderers). A page of text repeats a few dozen glyphs thousands of times,
-- and each repeat then costs a blit instead of flattening and
-- rasterising. Glyphs larger than 'maxCachedGlyph' pixels (few, and large)
-- are rasterised directly, exactly.
fillGlyph :: Ctx s -> St s -> Affine -> Int -> Path -> Maybe (Clip s) -> Sink s -> ST s ()
fillGlyph ctx st gm@(Affine a b c d e f) key path mclip sink
  | key < 0 || not (abs e < 1e9 && abs f < 1e9 && all (\v -> abs v < 1e6) [a, b, c, d]) = exact
  | otherwise = do
      let ix0 = floor e :: Int; iy0 = floor f :: Int
          px0 = round ((e - fromIntegral ix0) * 4) :: Int
          py0 = round ((f - fromIntegral iy0) * 4) :: Int
          (ix, px) = if px0 >= 4 then (ix0 + 1, 0) else (ix0, px0)
          (iy, py) = if py0 >= 4 then (iy0 + 1, 0) else (iy0, py0)
          q v = round (v * 65536) :: Int
          k = (key, q a, q b, q c, q d, px, py)
          local = Affine a b c d (fromIntegral px / 4) (fromIntegral py / 4)
      hit <- cacheLookup (cxGlyphs ctx) k
      gc <- case hit of
        Just g -> return (Just g)
        Nothing -> renderGlyph local k
      case gc of
        Nothing -> exact
        Just (GlyphCov gx gy gw gh arr) -> do
          let dbox = IBox (ix + gx) (iy + gy) (ix + gx + gw) (iy + gy + gh)
              -- Within the clip and layer, and within the sink's own box when
              -- the sink is a run's mask: that box was measured from the
              -- exact outlines (grown for the phase), and a blit past it
              -- would write before or after the mask's rows.
              sinkBox = case sink of
                SkMask mx my mw marr' -> IBox mx my (mx + mw) (my + maskRows mw marr')
                _ -> dbox
              vis = iboxIntersect sinkBox $ iboxIntersect dbox (fillBoxFor st (Rect (fromIntegral (ix + gx)) (fromIntegral (iy + gy))
                                                             (fromIntegral (ix + gx + gw)) (fromIntegral (iy + gy + gh))))
          unless (iboxEmpty vis) $
            emitMaskAt (cxEnv ctx) dbox arr vis (mclip >>= \cl -> clipOver cl vis) sink
  where
    exact = fillPathRaw ctx st gm NonZero path mclip sink
    renderGlyph local k = case devPathBounds local path of
      Just r | saneRect r -> do
        let IBox gx gy gx1 gy1 = rectToIBox r
            gw = gx1 - gx; gh = gy1 - gy
            bytes = 4 * gw * gh + 64
        if gw <= 0 || gh <= 0 || gw * gh > maxCachedGlyph then return Nothing else do
          ok <- reserve ctx bytes
          if not ok then return Nothing else do
            let env = cxEnv ctx
            mask <- newArray (0, max 1 (gw * gh) - 1) 0
            beginFill env
            fine <- flattenFill env local (fromIntegral gx) (fromIntegral gy) gw gh path
            when fine $ rasterFill env gx gy gw gh NonZero Nothing (SkMask gx gy gw mask)
            frozen <- unsafeFreeze mask
            let g = GlyphCov gx gy gw gh frozen
            cacheAdopt ctx (cxGlyphs ctx) k g bytes
            return (if fine then Just g else Nothing)
      _ -> return Nothing

-- Rows in a mask of the given width (its element count over the width).
maskRows :: Int -> STUArray s Int Float -> Int
maskRows mw (STUArray _ _ n _) = if mw > 0 then n `quot` mw else 0

-- | Pixels above which a glyph is rasterised directly rather than cached.
maxCachedGlyph :: Int
maxCachedGlyph = 64 * 64

-- Fill a path into an explicit sink; the clip, when given, is applied per
-- row (Nothing when the caller applies it later).
fillPathRaw :: Ctx s -> St s -> Affine -> FillRule -> Path -> Maybe (Clip s) -> Sink s -> ST s ()
fillPathRaw ctx st dev rule path mclip sink = case devPathBounds dev path of
  Nothing -> return ()
  Just r | not (saneRect r) -> countSkip (cxEnv ctx)
         | otherwise -> do
    let box@(IBox bx by bx1 by1) = fillBoxFor st r
        env = cxEnv ctx
    unless (iboxEmpty box) $ do
      beginFill env
      ok <- flattenFill env dev (fromIntegral bx) (fromIntegral by) (bx1 - bx) (by1 - by) path
      if ok
        then rasterFill env bx by (bx1 - bx) (by1 - by) rule (mclip >>= \cl -> clipOver cl box) sink
        else skipUnlessExhausted env

-- Run an allocation under the layer-memory cap, or the fallback when it
-- would not fit.
withMem :: Ctx s -> Int -> ST s () -> ST s () -> ST s ()
withMem ctx bytes fallback act = do
  ok <- reserve ctx bytes
  if not ok then fallback else act >> release ctx bytes

------------------------------------------------------------------------------
-- Images and stencils

unitSquare :: Path
unitSquare = rectPath 0 0 1 1

imageCmd :: Ctx s -> St s -> Affine -> Image -> ImageOpts -> ST s ()
imageCmd ctx st dev img opts =
  when (visibleUnit st dev) $ do
    charge (cxEnv ctx) (imgW img * imgH img `quot` 4)
    ok <- transient ctx (prescaleBytes dev (imgW img) (imgH img) 4)
    unless ok $ imageDropped ctx
    when ok $ fillPath ctx st dev NonZero unitSquare (imageShader dev img 0 (ioInterp opts) (ioOpacity opts)) Nothing

-- | Count an image the memory cap refused, so the caller can say so
-- ('rsImagesDropped', 'rsNotes'): an image is never dropped silently.
imageDropped :: Ctx s -> ST s ()
imageDropped ctx = do
  let env = cxEnv ctx
  k <- rdA (envI env) slotImagesDropped
  wrA (envI env) slotImagesDropped (k + 1)

-- | What prescaling an image for a matrix allocates: the result (bytes per
-- texel given) and two rows of double accumulators; nothing when it is not
-- minified. Independent of the source size ('prescaleRGBA').
prescaleBytes :: Affine -> Int -> Int -> Int -> Int
prescaleBytes m w h per
  | w <= 0 || h <= 0 = 0
  | w' < w || h' < h = per * w' * h' + 2 * 8 * per * (w' + 1)
  | otherwise = 0
  where (w', h') = prescaleDims m w h

visibleUnit :: St s -> Affine -> Bool
visibleUnit st dev = case devPathBounds dev unitSquare of
  Nothing -> False
  Just r -> not (iboxEmpty (fillBoxFor st r))

stencilCmd :: Ctx s -> St s -> Affine -> Mask -> Paint -> ImgInterp -> ST s ()
stencilCmd ctx st dev (Mask w h px) paint interp
  | w <= 0 || h <= 0 || numElements px < w * h = return ()
  | not (visibleUnit st dev) = return ()
  | otherwise = case invertA dev of
      Nothing -> return ()
      Just inv -> do
        charge (cxEnv ctx) (w * h `quot` 8)
        ok <- transient ctx (prescaleBytes dev w h 1)
        unless ok $ imageDropped ctx
        sh <- if not ok then return ShNone
              else mkShader ctx st dev paint (maybe (IBox 0 0 0 0) (fillBoxFor st) (devPathBounds dev unitSquare))
        let (w', h') = prescaleDims dev w h
            tex | w' < w || h' < h = Mask8 w' h' (prescaleMask w h px w' h')
                | otherwise = Mask8 w h px
            near = interp == InterpNearest && w' >= w && h' >= h
            stn = Stencil (composeA (scaleA (fromIntegral w') (fromIntegral h')) inv) tex near
        fillPath ctx st dev NonZero unitSquare sh (Just stn)

------------------------------------------------------------------------------
-- Gouraud meshes

gouraudCmd :: Ctx s -> St s -> Affine -> Either [GouraudTri] (Stops, [RampTri]) -> ST s ()
gouraudCmd ctx st dev tris = do
  let env = cxEnv ctx
      devTris :: [(Pt, Pt, Pt, Either (C4, C4, C4) (Double, Double, Double))]
      devTris = case tris of
        Left ts -> [ (ap p0, ap p1, ap p2, Left (premul c0, premul c1, premul c2))
                   | GouraudTri p0 c0 p1 c1 p2 c2 <- ts ]
        Right (_, ts) -> [ (ap p0, ap p1, ap p2, Right (t0, t1, t2)) | RampTri p0 t0 p1 t1 p2 t2 <- ts ]
      ap = applyA dev
      stops = case tris of
        Right (s, _) -> s
        Left _ -> mkStops []
      pts = concat [ [a, b, c] | (a, b, c, _) <- devTris ]
  case polysBounds [pts] of
    Nothing -> return ()
    Just r | not (saneRect r) -> countSkip env
           | otherwise -> do
      let box@(IBox bx by bx1 by1) = fillBoxFor st (rectInflate 1 r)
          w = bx1 - bx; h = by1 - by
      unless (iboxEmpty box) $ withMem ctx (w * h * 17) (return ()) $ do
        scratch <- newArray (0, max 1 (4 * w * h) - 1) 0 :: ST s (STUArray s Int Float)
        strong <- newArray (0, max 1 (w * h) - 1) 0 :: ST s (STUArray s Int Word8)
        let tri (Pt ax ay, Pt bx' by', Pt cx cy, colr) = do
              ok <- budgetLeft env
              let den = (by' - cy) * (ax - cx) + (cx - bx') * (ay - cy)
              when (ok && den /= 0 && not (isNaN den)) $ do
                let mn3 p q r = min p (min q r); mx3 p q r = max p (max q r)
                    x0 = max bx (floor (mn3 ax bx' cx) - 1)
                    x1 = min bx1 (ceiling (mx3 ax bx' cx) + 1)
                    y0 = max by (floor (mn3 ay by' cy) - 1)
                    y1 = min by1 (ceiling (mx3 ay by' cy) + 1)
                    -- barycentric weights are affine in the pixel centre
                    ka = (by' - cy) / den; kb = (cx - bx') / den
                    kc = (cy - ay) / den; kd = (ax - cx) / den
                    pixel !x !y = do
                      let px = fromIntegral x + 0.5 - cx; py = fromIntegral y + 0.5 - cy
                          l0 = ka * px + kb * py
                          l1 = kc * px + kd * py
                          l2 = 1 - l0 - l1
                          inside = l0 >= -1e-9 && l1 >= -1e-9 && l2 >= -1e-9
                          i = (y - by) * w + (x - bx)
                      s0 <- rdA strong i
                      when (inside || s0 == 0) $ do
                        let (m0, m1, m2) = if inside then (l0, l1, l2) else clampBary l0 l1 l2
                            C4 cr cg cb ca = case colr of
                              Left (C4 r0 g0 b0 a0, C4 r1 g1 b1 a1, C4 r2 g2 b2 a2) ->
                                let f0 = double2Float m0; f1 = double2Float m1; f2 = double2Float m2
                                in C4 (r0 * f0 + r1 * f1 + r2 * f2) (g0 * f0 + g1 * f1 + g2 * f2)
                                      (b0 * f0 + b1 * f1 + b2 * f2) (a0 * f0 + a1 * f1 + a2 * f2)
                              Right (t0, t1, t2) ->
                                stopsColor stops (spreadT SpreadPad (t0 * m0 + t1 * m1 + t2 * m2))
                        wrA scratch (4 * i) cr
                        wrA scratch (4 * i + 1) cg
                        wrA scratch (4 * i + 2) cb
                        wrA scratch (4 * i + 3) ca
                        when inside $ wrA strong i 1
                    rows !y | y >= y1 = return ()
                            | otherwise = cols x0 y >> rows (y + 1)
                    cols !x !y | x >= x1 = return ()
                               | otherwise = pixel x y >> cols (x + 1) y
                charge env (max 0 (x1 - x0) * max 0 (y1 - y0) + 16)
                rows y0
        mapM_ tri devTris
        frozen <- unsafeFreeze scratch
        -- Coverage: the union of the triangles, each oriented the same way,
        -- added only while the budget lasts (each triangle is a closed
        -- polygon, so a budget-truncated mesh is just a smaller one). A mesh
        -- tiles its area, so the integral is exact everywhere except where
        -- patches overlap by design, and its rows are not sent to the
        -- exactness repair: every row of a mesh crosses thousands of
        -- same-orientation triangles, which is precisely the case that
        -- repair is far too dear for.
        beginFill env
        let cover [] = return ()
            cover ((a, b, c, _) : rest) = do
              ok <- budgetLeft env
              when ok $ do
                let Pt ax ay = a; Pt bx' by' = b; Pt cx cy = c
                    cr = (bx' - ax) * (cy - ay) - (by' - ay) * (cx - ax)
                addPolygon env (fromIntegral bx) (fromIntegral by) w h (if cr < 0 then [a, b, c] else [a, c, b])
                cover rest
        cover devTris
        wrA (envI env) slotNoResolve 1
        rasterFill env bx by w h NonZero (clipOver (stClip st) box)
                   (paintSink st (ShScratch bx by w h frozen) Nothing)
        wrA (envI env) slotNoResolve 0
  where
    clampBary l0 l1 l2 =
      let m0 = max 0 l0; m1 = max 0 l1; m2 = max 0 l2; s = m0 + m1 + m2
      in if s > 0 then (m0 / s, m1 / s, m2 / s) else (1 / 3, 1 / 3, 1 / 3)

------------------------------------------------------------------------------
-- Clips

clipCmd :: Ctx s -> St s -> [ClipShape] -> [Cmd] -> ST s ()
clipCmd ctx st shapes kids
  | null shapes = return ()
  | otherwise = do
      mc <- intersectClip ctx st shapes
      case mc of
        Nothing -> return ()
        Just cl -> drawCmds ctx st { stClip = cl } kids

-- The new clip, or Nothing when nothing is left visible (or its mask would
-- not fit the memory cap). A mask is a transient of the clip command.
intersectClip :: Ctx s -> St s -> [ClipShape] -> ST s (Maybe (Clip s))
intersectClip ctx st shapes
  | [ClipShape m _ path] <- shapes, Just (Rect x0 y0 x1 y1) <- axisRect (composeA (stCtm st) m) path =
      case stClip st of
        ClipRect a b c d ->
          let r@(Rect ra rb rc rd) = Rect (max a x0) (max b y0) (min c x1) (min d y1)
          in return (if rc > ra && rd > rb && not (iboxEmpty (rectToIBox r)) then Just (ClipRect ra rb rc rd) else Nothing)
        ClipMask {} -> general
  | otherwise = general
  where
    cur = stClip st
    general = do
      let bs = [ r | ClipShape m _ p <- shapes, Just r <- [devPathBounds (composeA (stCtm st) m) p] ]
      case bs of
        [] -> return Nothing
        (b0 : brest) -> do
          let box@(IBox bx by bx1 by1) = fillBoxFor st (foldl rectUnion b0 brest)
              w = bx1 - bx; h = by1 - by
          ok <- if iboxEmpty box then return False else transient ctx (w * h * 4 + 4 * (w + 2))
          if not ok then return Nothing else do
            mask <- newArray (0, max 1 (w * h) - 1) 0
            forM_ shapes $ \(ClipShape m rule p) -> do
              ok <- budgetLeft (cxEnv ctx)
              when ok $ fillPathRaw ctx st (composeA (stCtm st) m) rule p Nothing (SkMask bx by w mask)
            -- Intersect with the enclosing clip, row by row.
            case clipOver cur box of
              Nothing -> return ()
              Just cl -> do
                row <- newArray (0, w + 1) 0 :: ST s (STUArray s Int Float)
                forM_ [by .. by1 - 1] $ \y -> do
                  let base = (y - by) * w
                  forM_ [0 .. w - 1] $ \x -> rdA mask (base + x) >>= wrA row x
                  clipRow cl row y bx bx bx1
                  forM_ [0 .. w - 1] $ \x -> rdA row x >>= wrA mask (base + x)
            return (Just (ClipMask bx by w h mask))

-- A path that is exactly an axis-aligned rectangle in device space.
axisRect :: Affine -> Path -> Maybe Rect
axisRect m path = case pts of
  [p0, p1, p2, p3] -> rect4 p0 p1 p2 p3
  [p0, p1, p2, p3, p4] | close p4 p0 -> rect4 p0 p1 p2 p3
  _ -> Nothing
  where
    pts = go path
    go (MoveTo p : r) = applyA m p : lines' r
    go _ = [Pt (0 / 0) 0, Pt 0 0, Pt 0 0]
    lines' (LineTo p : r) = applyA m p : lines' r
    lines' [ClosePath] = []
    lines' [] = []
    lines' _ = [Pt (0 / 0) 0]
    eps a b = abs (a - b) <= 1e-9 * (1 + abs a + abs b)
    close (Pt a b) (Pt c d) = eps a c && eps b d
    rect4 (Pt ax ay) (Pt bx by) (Pt cx cy) (Pt dx dy)
      | any bad [ax, ay, bx, by, cx, cy, dx, dy] = Nothing
      | eps ay by && eps bx cx && eps cy dy && eps dx ax = mk
      | eps ax bx && eps by cy && eps cx dx && eps dy ay = mk
      | otherwise = Nothing
      where
        mk = let x0 = minimum [ax, bx, cx, dx]; x1 = maximum [ax, bx, cx, dx]
                 y0 = minimum [ay, by, cy, dy]; y1 = maximum [ay, by, cy, dy]
             in Just (Rect x0 y0 x1 y1)
    bad v = isNaN v || isInfinite v || abs v > 1e12

------------------------------------------------------------------------------
-- Groups

isLeaf :: Cmd -> Bool
isLeaf c = case c of
  CFill {} -> True
  CStroke {} -> True
  CGlyphs {} -> True
  CImage {} -> True
  CStencil {} -> True
  CGouraud {} -> True
  CGouraudRamp {} -> True
  CTransform _ [k] -> isLeaf k
  CClip _ [k] -> isLeaf k
  _ -> False

-- Does anything in the subtree blend non-normally (and so see a
-- non-isolated group's backdrop)? Bounded; an over-large subtree answers
-- yes, which is merely slower.
anyNonNormal :: [Cmd] -> Bool
anyNonNormal cmds0 = go (4096 :: Int) cmds0 < 0
  where
    go fuel [] = fuel
    go fuel _ | fuel <= 0 = -1
    go fuel (c : cs) = case c of
      CGroup o kids | goBlend o /= BlendNormal -> -1
                    | otherwise -> go (go (fuel - 1) kids) cs
      CClip _ kids -> go (go (fuel - 1) kids) cs
      CTransform _ kids -> go (go (fuel - 1) kids) cs
      _ -> go (fuel - 1) cs

groupCmd :: Ctx s -> St s -> GroupOpts -> [Cmd] -> ST s ()
groupCmd ctx st opts kids
  | stDepth st >= maxGroupDepth = countSkip (cxEnv ctx)
  -- A filter can make pixels from nothing (a flood, an offset or drop
  -- shadow of one), so an empty or invisible source graphic is still run.
  | null kids && isNothing (goFilter opts) = return ()
  | not (op > 0) = return ()
  | flattenable = drawCmds ctx st' kids
  | [k] <- kids, isLeaf k, isNothing (goMask opts), isNothing (goFilter opts), not (goKnockout opts) =
      drawCmd ctx st { stOp = stOp st * opF, stBlend = goBlend opts, stDepth = stDepth st + 1 } k
  | otherwise = case goFilter opts of
      Just f -> filteredGroup ctx st opts f kids
      Nothing -> layeredGroup ctx st opts kids
  where
    op = c01 (goOpacity opts)
    opF = double2Float op
    st' = st { stDepth = stDepth st + 1 }
    flattenable = goOpacity opts >= 1 && goBlend opts == BlendNormal && isNothing (goMask opts)
                  && isNothing (goFilter opts) && not (goKnockout opts)
                  && (not (goIsolated opts) || not (anyNonNormal kids))
                  && stOp st >= 1 && stBlend st == BlendNormal

layerIBox :: Layer s -> IBox
layerIBox ly = IBox (lyX ly) (lyY ly) (lyX ly + lyW ly) (lyY ly + lyH ly)

layeredGroup :: Ctx s -> St s -> GroupOpts -> [Cmd] -> ST s ()
layeredGroup ctx st opts kids = do
  let env = cxEnv ctx
      parent = stLayer st
      clipB = iboxIntersect (clipIBox (stClip st)) (layerIBox parent)
      box = case cmdsBounds (stCtm st) kids of
        Nothing -> IBox 0 0 0 0
        Just r -> iboxIntersect clipB (rectToIBox r)
      nonIso = not (goIsolated opts) && anyNonNormal kids
      extra = (if nonIso then 4 else 0) + (if goKnockout opts then 16 else 0) + (if isJust (goMask opts) then 4 else 0)
      bytes = layerBytes box + iboxW box * iboxH box * extra
  unless (iboxEmpty box) $
    withMem ctx bytes (drawCmds ctx st { stDepth = stDepth st + 1 } kids) $ do
      layer0 <- newLayer box
      let n = iboxW box * iboxH box
      when nonIso $ copyRegion parent layer0 (lyPix layer0)
      ga <- if nonIso then Just <$> newArray (0, max 1 n - 1) 0 else return Nothing
      kn <- if goKnockout opts
              then do
                bd <- newArray (0, max 1 (4 * n) - 1) 0
                when nonIso $ copyRegion parent layer0 bd
                return (Just bd)
              else return Nothing
      let layer = layer0 { lyKnock = kn, lyGA = ga }
      drawCmds ctx st { stLayer = layer, stDepth = stDepth st + 1, stOp = 1, stBlend = BlendNormal } kids
      mmask <- case goMask opts of
        Nothing -> return (Right Nothing)
        Just sm -> maybe (Left ()) (Right . Just) <$> renderSoftMask ctx st box sm
      -- a mask that did not fit leaves the group undrawn (counted as skipped)
      case mmask of
        Left () -> return ()
        Right mk -> compositeLayer env parent layer { lyKnock = Nothing } (goBlend opts)
                      (double2Float (c01 (goOpacity opts)) * stOp st) mk Nothing nonIso
  where
    
filteredGroup :: Ctx s -> St s -> GroupOpts -> Filter -> [Cmd] -> ST s ()
filteredGroup ctx st opts f kids = do
  let env = cxEnv ctx
      parent = stLayer st
      dm = composeA (stCtm st) (fAffine f)
      regionR = transformRect dm (fRegion f)
      margin = filterMargin dm (fPrims f)
      visible = iboxIntersect (clipIBox (stClip st)) (layerIBox parent)
      IBox vx0 vy0 vx1 vy1 = visible
      -- Content further out than this contributes nothing visible to a
      -- blur that wide (it is spread over the whole margin), so the margin
      -- is capped rather than letting a huge deviation size the layer.
      -- (and in proportion to what is visible: a zoomed-in view of a
      -- blur needs the neighbourhood of the view, not of the page)
      mi = ceiling (minimum [4096, margin, fromIntegral (max 256 (2 * max (vx1 - vx0) (vy1 - vy0)))]) :: Int
      grown = IBox (vx0 - mi) (vy0 - mi) (vx1 + mi) (vy1 + mi)
      region = rectToIBox regionR
      fb = iboxIntersect region grown
      n = iboxW fb * iboxH fb
      bytes = n * 16 * (length (fPrims f) + 3)
  unless (iboxEmpty fb || iboxEmpty visible || rectEmpty regionR || not (saneRect regionR)) $
    withMem ctx bytes (drawCmds ctx st { stDepth = stDepth st + 1 } kids) $ do
      src <- newLayer fb
      let IBox fx0 fy0 fx1 fy1 = fb
          st' = st { stLayer = src, stClip = ClipRect (fromIntegral fx0) (fromIntegral fy0) (fromIntegral fx1) (fromIntegral fy1)
                   , stDepth = stDepth st + 1, stOp = 1, stBlend = BlendNormal }
      drawCmds ctx st' kids
      res <- runFilter env dm fb region (fPrims f) src
      mmask <- case goMask opts of
        Nothing -> return (Right Nothing)
        Just sm -> maybe (Left ()) (Right . Just) <$> renderSoftMask ctx st fb sm
      case mmask of
        Left () -> return ()
        Right mk -> compositeLayer env parent res (goBlend opts)
                      (double2Float (c01 (goOpacity opts)) * stOp st) mk (Just (stClip st)) False

-- Render a soft mask and reduce it to coverage over its own box; Nothing
-- when it would not fit the memory cap.
--
-- A mask with a cache key (PDF's soft masks, which a content stream leaves
-- active across any number of objects) is rendered once per render, matrix
-- and visible box, over that box rather than the group's, so every object
-- under it reuses one rendering and indexes into it; its coverage stays
-- charged while cached. A mask without a key is rendered over the group's
-- box and lives as long as the group command.
renderSoftMask :: Ctx s -> St s -> IBox -> SoftMask -> ST s (Maybe (IBox, STUArray s Int Float))
renderSoftMask ctx st groupBox sm = do
  let cacheable = smKey sm >= 0
      visBox = iboxIntersect (clipIBox (stClip st)) (layerIBox (stLayer st))
      box = if cacheable then visBox else groupBox
      Affine ma mb mc md me mf = stCtm st
      key = (smKey sm, (ma, mb, mc, md, me, mf), box)
  hit <- if cacheable then cacheLookup (cxMasks ctx) key else return Nothing
  case hit of
    Just m -> return (Just m)
    Nothing
      | iboxEmpty box -> return Nothing
      | otherwise -> do
          let n = iboxW box * iboxH box
              lb = layerBytes box
              mbytes = 4 * n
          okL <- reserve ctx lb
          okM <- if okL then reserve ctx mbytes else return False
          if not (okL && okM) then (when okL (release ctx lb)) >> return Nothing else do
            out <- renderMaskInto ctx st box sm n
            release ctx lb
            if cacheable then cacheAdopt ctx (cxMasks ctx) key (box, out) mbytes
            else modifySTRef' (cxPending ctx) (+ mbytes)
            return (Just (box, out))

renderMaskInto :: Ctx s -> St s -> IBox -> SoftMask -> Int -> ST s (STUArray s Int Float)
renderMaskInto ctx st box sm n = do
  layer <- newLayer box
  let IBox x0 y0 x1 y1 = box
      st' = st { stLayer = layer, stClip = ClipRect (fromIntegral x0) (fromIntegral y0) (fromIntegral x1) (fromIntegral y1)
               , stDepth = stDepth st + 1, stOp = 1, stBlend = BlendNormal }
  drawCmds ctx st' (smCmds sm)
  let pix = lyPix layer
      C4 br bg bb _ = premul (smBackdrop sm)
      -- PDF's luminosity is its blend-mode Lum (0.3, 0.59, 0.11); an SVG
      -- luminance mask uses the coefficients resvg and the browsers apply to
      -- sRGB values. The front-end says which ('MaskKind').
      (kr, kg, kb) = case smKind sm of
        MaskLuminance -> (0.2125, 0.7154, 0.0721)
        _             -> (0.3, 0.59, 0.11) :: (Float, Float, Float)
      lut = smTransfer sm
  out <- newArray (0, max 1 n - 1) 0
  forM_ [0 .. n - 1] $ \i -> do
    r <- rdA pix (4 * i)
    g <- rdA pix (4 * i + 1)
    b <- rdA pix (4 * i + 2)
    a <- rdA pix (4 * i + 3)
    let v0 = case smKind sm of
          MaskAlpha -> a
          _ ->
            let ia = 1 - a
            in kr * (r + br * ia) + kg * (g + bg * ia) + kb * (b + bb * ia)
        v1 = if v0 < 0 then 0 else if v0 > 1 then 1 else v0
        v = case lut of
          Nothing -> v1
          Just t | numElements t >= 256 ->
                     fromIntegral (atA t (truncate (v1 * 255 + 0.5))) * (1 / 255)
                 | otherwise -> v1
    wrA out i v
  return out

------------------------------------------------------------------------------
-- Bounds

-- | Device bounds of commands drawn under a matrix ('Nothing' when they
-- draw nothing). Conservative: control points, strokes inflated by their
-- reach, filtered groups by their filter region. Large subtrees are not
-- walked in full; past a fixed amount of work the answer is "everything".
cmdsBounds :: Affine -> [Cmd] -> Maybe Rect
cmdsBounds m0 cmds0 = case goList (20000 :: Int) m0 cmds0 of
  (f, r) | f < 0 -> Just everything
         | otherwise -> r
  where
    everything = Rect (-1e9) (-1e9) 1e9 1e9
    goList fuel _ [] = (fuel, Nothing)
    goList fuel m (c : cs)
      | fuel <= 0 = (-1, Nothing)
      | otherwise =
          let (f1, r1) = one (fuel - 1) m c
              (f2, r2) = goList f1 m cs
          in (f2, unionM r1 r2)
    unionM Nothing b = b
    unionM a Nothing = a
    unionM (Just a) (Just b) = Just (rectUnion a b)
    interM (Just (Rect a b c d)) (Just (Rect a' b' c' d')) =
      let r = Rect (max a a') (max b b') (min c c') (min d d') in if rectW r >= 0 && rectH r >= 0 then Just r else Nothing
    interM _ _ = Nothing
    one fuel m c = case c of
      CFill a _ _ p -> (fuel, devPathBounds (composeA m a) p)
      CStroke a ss _ p ->
        let dev = composeA m a
            ex = expansionA dev
            reach = (if ssWidth ss > 0 then strokeExtent ss * ex else 0) + 2
        in (fuel, rectInflate reach <$> devPathBounds dev p)
      CGlyphs a _ gs ->
        let dev = composeA m a
        in (fuel - length gs, foldr (\(Glyph ga _ p) acc -> unionM (devPathBounds (composeA dev ga) p) acc) Nothing gs)
      CImage a _ _ -> (fuel, devPathBounds (composeA m a) unitSquare)
      CStencil a _ _ _ -> (fuel, devPathBounds (composeA m a) unitSquare)
      CGouraud a ts -> (fuel, polysBounds [[ applyA (composeA m a) p | GouraudTri p0 _ p1 _ p2 _ <- ts, p <- [p0, p1, p2] ]])
      CGouraudRamp a _ ts -> (fuel, polysBounds [[ applyA (composeA m a) p | RampTri p0 _ p1 _ p2 _ <- ts, p <- [p0, p1, p2] ]])
      CClip shapes kids ->
        let sb = foldr (\(ClipShape a _ p) acc -> unionM (devPathBounds (composeA m a) p) acc) Nothing shapes
            (f', kb) = goList fuel m kids
        in (f', interM sb kb)
      CGroup o kids -> case goFilter o of
        Just f -> (fuel, Just (transformRect (composeA m (fAffine f)) (fRegion f)))
        Nothing -> goList fuel m kids
      CTransform a kids -> goList fuel (composeA m a) kids
