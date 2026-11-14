{-# LANGUAGE BangPatterns #-}
-- Scene measurement runs on job threads, over loops that may not allocate:
-- without yield points there a kill (a newer request, Ctrl+Q) never lands.
{-# OPTIONS_GHC -fno-omit-yields #-}
-- | The vector-picture view model (plan 0034, workstream F): what the image
-- view needs to show a "Cmedit.Scene" instead of a decoded bitmap.
--
-- Pure, and deliberately small. The scene is parsed by the driver (it needs
-- fonts and external images, which are IO) and drawn by "Cmedit.Raster";
-- this module owns the three things in between that both the pure editor
-- and the driver must agree on:
--
-- __A virtual pixel grid.__ The image view's geometry (fit, wheel zoom,
-- drag-to-zoom, the kitty/sixel placement) is written in integer source
-- pixels, and a vector picture has none. Rather than fork all of that, a
-- picture presents a /virtual/ size whose longer side is 'vecVirtualSide'
-- (65 536) units, and every crop is an integer rectangle in those units. At
-- the deepest zoom ('vecMaxZoom') the window is still over a hundred virtual
-- units across, so integer crops never quantise the anchor visibly, and the
-- existing geometry works unchanged. Nothing ever allocates the virtual
-- grid: 'vecView' maps a crop straight to a scene → device matrix.
--
-- __Rasterising at exactly the output size.__ 'rasterVec' draws only the
-- visible crop, at exactly the requested pixel size (the cell picture's
-- sub-pixel box, or the overlay placement's device pixels), so zooming
-- re-rasterises the vectors sharply and never scales a bitmap. Requests are
-- capped at 'maxVecRasterPixels' whatever the caller asks for.
--
-- __Render identities.__ A 'VecKey' (scene id, crop, size) names one
-- raster. The scene id is assigned by the driver when a parse lands and is
-- unique for the session, so a key can never match a raster of an older
-- version of the document.
module Cmedit.VecView
  ( -- * The model
    VecDoc(..)
  , emptyVecDoc
  , svgSizeLabel
  , VecKey(..)
    -- * Geometry
  , vecVirtualSide
  , vecDims
  , vecView
  , vecMaxZoom
    -- * Rendering
  , rasterVec
  , rasterNoteList
  , vecTruncatedText
  , rasterVecWith
  , vecSyncBudget
    -- * Memory policy
  , keepScenes
  , pageKeepBytes
  , keepPreviousPlacement
  , vecKeepScenes
  , vecKeepCost
  , maxVecRasterPixels
  , vecBudget
  , placeholderImage
  , imageRGBA
    -- * Cost
  , SceneSize(..)
  , measureScene
  , forceScene
  , sceneSize
  , sceneCost
  , sceneBytes
  , sceneCostCap
  , sceneBytesCap
  , vecSyncCostMax
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IM
import Data.IORef (newIORef, readIORef, modifyIORef')
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName, hashStableName)
import Data.Array.Base (unsafeAt, numElements)
import Data.Array.Unboxed (listArray)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import Foreign.Storable (pokeByteOff)
import Data.List (foldl', isInfixOf, isPrefixOf, sortOn)

import Cmedit.Geom
import Cmedit.Image (Image(..))
import Cmedit.Raster (RenderOpts(..), RenderStats(..), defaultRenderOpts, renderScene)
import Cmedit.Scene

-- | The vector side of an image-view document. Lives inside
-- 'Cmedit.EditorState.ImageDoc' (@idVec@), so it travels through the zipper
-- with the rest of the view state.
data VecDoc = VecDoc
  { vdScene     :: !(Maybe Scene)   -- ^ The parsed picture; 'Nothing' until the first parse lands.
  , vdSceneId   :: !Int             -- ^ Driver-assigned, session-unique id of 'vdScene' (0 = none).
  , vdSeq       :: !Int             -- ^ The document edit counter ('edDocSeq') the scene was parsed from; -1 = never parsed.
  , vdSize      :: !(Double, Double) -- ^ The picture's own size in CSS px (the scene bounds).
  , vdCost      :: !Int             -- ^ 'szCost' of the scene, as the job measured it (decides synchronous cell rendering).
  , vdBytes     :: !Int             -- ^ 'szBytes' of the scene, as the job measured it (the PDF page cache's budget).
  , vdCells     :: !(Maybe (VecKey, Image)) -- ^ The latest cell-resolution raster.
  , vdNotes     :: ![String]        -- ^ The scene's own notes plus the driver's (fonts, images).
  , vdError     :: !(Maybe String)  -- ^ The last parse failed with this.
  , vdExhausted :: !Bool            -- ^ The last cell raster ran out of budget.
  , vdEpoch     :: !Int
    -- ^ Session-unique identity of /this/ picture model, stamped when it is
    -- created (open, revert, re-open) or invalidated (fonts arrived). A parse
    -- is requested and accepted by (path, epoch, 'vdSeq'), never by (path,
    -- seq) alone: a document's edit counter restarts at 0 on every load, so
    -- without the epoch a parse begun before a Revert would land afterwards
    -- as the picture of the reverted file.
  , vdUsed      :: !Int             -- ^ When this picture was last the active document (recency, for 'keepScenes').
  , vdFontless  :: !Bool            -- ^ Parsed before font discovery finished: redraw once fonts are known.
  , vdCellsPartial :: !Bool         -- ^ 'vdCells' is a truncated synchronous raster; the background job should redo it.
  , vdCellsFailed  :: !(Maybe VecKey) -- ^ The cell raster for this key failed; do not ask again for it.
  , vdRasterNotes  :: ![String]       -- ^ What the last cell raster had to leave out ('rasterNoteList').
  } deriving (Show)

-- | A picture view waiting for its first parse, with its epoch.
emptyVecDoc :: Int -> VecDoc
emptyVecDoc epoch = VecDoc Nothing 0 (-1) (1, 1) 0 0 Nothing [] Nothing False epoch epoch False False Nothing []

-- | "48×48" (CSS px, rounded) for the status bar.
svgSizeLabel :: VecDoc -> String
svgSizeLabel vd = let (w, h) = vdSize vd in showDim w ++ "\xd7" ++ showDim h
  where showDim v = let r = round v :: Int
                    in if abs (v - fromIntegral r) < 0.05 then show r
                       else show (fromIntegral (round (v * 10) :: Int) / (10 :: Double))

-- | One raster: which scene, which virtual crop @(x, y, w, h)@, and the
-- output size in pixels.
data VecKey = VecKey
  { vkScene :: !Int
  , vkCrop  :: !(Int, Int, Int, Int)
  , vkW     :: !Int
  , vkH     :: !Int
  } deriving (Eq, Ord, Show)

-- | The longer side of a picture's virtual pixel grid (see the module
-- header). 2^24: at the deepest zoom the window is still 65 536 units
-- across, and a picture with a 1000:1 aspect ratio still has a short side of
-- 16 777 units, so integer crops neither quantise the anchor nor distort the
-- aspect. Areas stay well inside 'Int' (2^48 at most).
vecVirtualSide :: Int
vecVirtualSide = 16777216

-- | The virtual size of a picture of this many CSS px: the longer side is
-- 'vecVirtualSide', the shorter keeps the aspect ratio (at least 1).
vecDims :: (Double, Double) -> (Int, Int)
vecDims (w0, h0) =
  let w = if ok w0 then w0 else 1
      h = if ok h0 then h0 else 1
      k = fromIntegral vecVirtualSide / max w h
  in (clampV (round (w * k)), clampV (round (h * k)))
  where ok v = v > 0 && not (isNaN v) && not (isInfinite v)
        clampV v = max 1 (min vecVirtualSide v)

-- | How far a vector picture may be zoomed in, relative to its fit: deep
-- enough to read the detail of a map or a schematic, bounded so a runaway
-- wheel cannot ask for a window narrower than a hundred virtual units.
vecMaxZoom :: Double
vecMaxZoom = 256

-- | The scene → device matrix drawing the virtual crop @(cx, cy, cw, ch)@ of
-- a scene with these bounds into a @w × h@ raster. Non-uniform on purpose:
-- a half-block sub-pixel is not square, and the caller's @w × h@ already
-- carries the aspect correction.
vecView :: Rect -> (Int, Int) -> (Int, Int, Int, Int) -> Int -> Int -> Affine
vecView (Rect x0 y0 x1 y1) (vw, vh) (cx, cy, cw, ch) w h =
  let sw = max 1e-9 (x1 - x0); sh = max 1e-9 (y1 - y0)
      -- scene → virtual
      kx = fromIntegral vw / sw; ky = fromIntegral vh / sh
      -- virtual → device
      dx = fromIntegral w / fromIntegral (max 1 cw)
      dy = fromIntegral h / fromIntegral (max 1 ch)
      ax = kx * dx; ay = ky * dy
  in Affine ax 0 0 ay (-(x0 * kx + fromIntegral cx) * dx) (-(y0 * ky + fromIntegral cy) * dy)

-- | Most pixels one vector raster may have, whatever the caller asks for
-- (the overlay is already held to 'Cmedit.Gfx.maxGfxPixels', and a cell
-- picture is a few tens of thousands).
maxVecRasterPixels :: Int
maxVecRasterPixels = 4 * 1000 * 1000

-- | Work budget for one interactive raster. A quarter of the rasteriser's
-- default: a picture that needs more than this per frame is not one anybody
-- is looking at interactively, and the note says the drawing was cut short.
vecBudget :: Int
vecBudget = 100 * 1000 * 1000

-- | Rasterise the virtual crop of a scene at exactly @w × h@ pixels
-- (transparent background). 'Nothing' for a request outside the bounds;
-- never anything else that the caller would have to guard against: the
-- image is always exactly @w × h@.
rasterVec :: Scene -> (Int, Int, Int, Int) -> Int -> Int -> Maybe (Image, RenderStats)
rasterVec = rasterVecWith vecBudget

-- | What the user should be told about one raster: the rasteriser's own
-- notes ('rsNotes': images it could not fit in its memory cap, ...), with
-- its budget note replaced by ours ('vecTruncatedText'), so the status line
-- never says "truncated" twice in two wordings.
rasterNoteList :: RenderStats -> [String]
rasterNoteList st =
  [ vecTruncatedText | rsExhausted st ]
    ++ filter (not . isBudgetNote) (rsNotes st)
  where isBudgetNote n = "picture incomplete" `isPrefixOf` n || "work budget" `isInfixOf` n

vecTruncatedText :: String
vecTruncatedText = "This picture is too complex to draw in full \x2014 drawing truncated"

-- | The work budget of the synchronous cell raster, the one raster made on
-- the event-loop thread. 'sceneCost' cannot see dash expansion or a tiling
-- pattern's fan-out, so the cost threshold alone does not bound the work;
-- this does (about 2 ms of rasterising), and a scene that runs out of it is
-- shown truncated for a moment and redone by the background job.
vecSyncBudget :: Int
vecSyncBudget = 2 * 1000 * 1000

-- | 'rasterVec' with a given work budget.
rasterVecWith :: Int -> Scene -> (Int, Int, Int, Int) -> Int -> Int -> Maybe (Image, RenderStats)
rasterVecWith budget sc crop w h
  | w <= 0 || h <= 0 || w * h > maxVecRasterPixels = Nothing
  | otherwise =
      let dims = vecDims (rectW b, rectH b)
          opts = (defaultRenderOpts w h (vecView b dims crop w h)) { roBudget = budget }
          (img, st) = renderScene opts sc
      in if imgW img == w && imgH img == h then Just (img, st) else Nothing
  where b = scBounds sc

-- | An image's RGBA bytes as the payload the kitty/sixel encoders take: a
-- straight copy (a raster is made at exactly the placement's size, so there
-- is nothing to resample; 'Cmedit.Image.scaleRGBA' would box-average each
-- byte for nothing).
imageRGBA :: Image -> BS.ByteString
imageRGBA img =
  let n = min (4 * imgW img * imgH img) (numElements (imgPix img))
      arr = imgPix img
  in if n <= 0 then BS.empty
     else BI.unsafeCreate n (\p -> forM_ [0 .. n - 1] (\i -> pokeByteOff p i (unsafeAt arr i)))

-- | Which of several cached scenes to keep: the most recently used first,
-- at most @n@ of them and within a total cost budget (the most recent one is
-- always kept, whatever it costs). Input and output are in the same order;
-- each item is (last-used stamp, cost). Pure and generic on purpose: the
-- open SVG documents use it now, and the PDF page view's per-page scenes are
-- the same problem.
keepScenes :: Int -> Int -> [(Int, Int)] -> [Bool]
keepScenes n budget items =
  let ranked = sortOn (\(_, (u, _)) -> negate u) (zip [0 :: Int ..] items)
      pick _ _ [] = []
      pick k used ((i, (_, c)) : rest)
        | k >= n = []
        | k > 0 && used + c > budget = pick k used rest
        | otherwise = i : pick (k + 1) (used + c) rest
      kept = pick 0 0 ranked
  in [ i `elem` kept | i <- [0 .. length items - 1] ]

-- | While a picture's new overlay is not ready, may the previous placement
-- stay on screen? Only at the same place (document, geometry, cell size,
-- protocol), only while the picture has not failed, and only while the new
-- raster is genuinely pending (the cache has no entry for it: @Nothing@);
-- a refused raster (@Just Nothing@) or a failed parse means nothing is
-- coming, and the old pixels would otherwise stand for the new picture
-- indefinitely (a failed PDF page under the previous page's image).
keepPreviousPlacement :: Bool -> Bool -> Maybe (Maybe ()) -> Bool
keepPreviousPlacement samePlace failed cached = samePlace && not failed && isNothingM cached
  where isNothingM Nothing = True
        isNothingM (Just _) = False

-- | What a scene costs to hold and to draw, measured once.
--
-- * 'szCost': a rough cost of drawing the scene once (commands, path
--   segments, glyph outlines, image pixels, filters and soft masks weighted
--   up, since each is a layer and a pass over it). This is /drawing/ work, so
--   a shared sub-scene counts every time it is drawn. Saturates at
--   'sceneCostCap'. Decides synchronous cell rendering ('vecSyncCostMax')
--   and which inactive SVG scenes are kept ('vecKeepCost').
-- * 'szBytes': a scene's memory, roughly: what it holds that scales with the
--   file, which for a real PDF page is its decoded images (four bytes a
--   pixel) far more than its paths. The drawing cost was the wrong measure
--   for "how many pages can we keep": a page of photographs is cheap to draw
--   and holds hundreds of megabytes. Saturates at 'sceneBytesCap'.
data SceneSize = SceneSize
  { szCost  :: !Int
  , szBytes :: !Int
  } deriving (Eq, Show)

-- | Where the two measures stop counting. Every consumer compares against
-- far smaller numbers ('vecKeepCost', 'pageKeepBytes'), so a saturated
-- measure only ever means "too big".
sceneCostCap, sceneBytesCap :: Int
sceneCostCap = 10 * 1000 * 1000
sceneBytesCap = 4 * 1024 * 1024 * 1024

-- | Measure a scene /and force it all the way down/ (every command, path and
-- point), on a background job, so the job pays for every lazy thunk the
-- front-end left behind (glyph outlines decoded on demand, paths built
-- lazily) and none of it is paid later on the event-loop thread.
--
-- __Sharing is the hazard, so the walk is identity-aware.__ A scene shares
-- sub-scenes through 'CTransform' (a form XObject, a Type 3 glyph, an SVG
-- @<use>@), and nesting multiplies: a 10 KB PDF whose Type 3 glyphs draw
-- each other fifteen deep is a modest heap and an astronomically large
-- /tree/. A plain recursive walk of that tree never finished, and being a
-- loop over already-evaluated data it never allocated either, so the job
-- could not even be killed. Here every command list is visited once per heap
-- object (by 'StableName'), its drawing cost is memoised and re-added on each
-- further reference (so 'szCost' is still the expanded cost, in time linear
-- in the heap), and an image counts towards 'szBytes' once however often it
-- is drawn (a logo on every page section is one decoded image). The walk is
-- also fuelled ('measureFuel' units, one per command and per path segment):
-- what is left unforced when the fuel runs out is forced by the rasteriser
-- (on a job thread too), and the measure reports the caps. The module is
-- compiled with @-fno-omit-yields@ so a kill always lands.
measureScene :: Scene -> IO SceneSize
measureScene = walkScene True measureFuel

-- | 'measureScene' without its result, for callers that only force.
forceScene :: Scene -> IO ()
forceScene sc = () <$ measureScene sc

-- | The same measures, sharing-blind: every reference counted as if it were
-- its own copy (so 'szBytes' over-counts a shared image), and fuelled more
-- tightly. For tests and benchmarks; the editor uses what the job measured.
sceneSize :: Scene -> SceneSize
sceneSize sc = unsafePerformIO (walkScene False plainFuel sc)
{-# NOINLINE sceneSize #-}

sceneCost, sceneBytes :: Scene -> Int
sceneCost = szCost . sceneSize
sceneBytes = szBytes . sceneSize

measureFuel, plainFuel :: Int
measureFuel = 64 * 1000 * 1000
plainFuel = 16 * 1000 * 1000

-- | The walk behind 'measureScene' (@dedup@) and 'sceneSize' (not). Without
-- @dedup@ it touches only local references, so 'sceneSize' is a pure
-- function of the scene's value.
walkScene :: Bool -> Int -> Scene -> IO SceneSize
walkScene dedup fuel0 sc = do
  _ <- evaluate (rectF (scBounds sc))
  fuel <- newIORef fuel0
  lists <- newIORef (IM.empty :: IM.IntMap [(StableName [Cmd], Int)])
  images <- newIORef (IM.empty :: IM.IntMap [StableName Image])
  let spend :: Int -> IO ()
      spend n = modifyIORef' fuel (subtract n)
      live = (> 0) <$> readIORef fuel
      ev :: a -> IO ()
      ev x = () <$ evaluate x
      satC x = min sceneCostCap x
      satB x = min sceneBytesCap x

      -- a command list: once per heap object when deduplicating
      list :: [Cmd] -> IO (Int, Int)
      list cs0 = do
        ok <- live
        if not ok then pure (sceneCostCap, 0) else do
          cs <- evaluate cs0
          case cs of
            [] -> pure (0, 0)
            _ | not dedup -> cmds 0 0 cs
              | otherwise -> do
                  sn <- makeStableName cs
                  let h = hashStableName sn
                  known <- lookup sn . IM.findWithDefault [] h <$> readIORef lists
                  case known of
                    Just c -> spend 1 >> pure (c, 0)
                    Nothing -> do
                      -- provisional entry: a list that (via a cycle) contains
                      -- itself costs the cap rather than recursing forever
                      modifyIORef' lists (IM.insertWith (++) h [(sn, sceneCostCap)])
                      r@(c, _) <- cmds 0 0 cs
                      modifyIORef' lists (IM.adjust (map (\e@(s, _) -> if s == sn then (s, c) else e)) h)
                      pure r

      cmds :: Int -> Int -> [Cmd] -> IO (Int, Int)
      cmds !c !b xs0 = do
        xs <- evaluate xs0
        case xs of
          [] -> pure (c, b)
          x : rest -> do
            ok <- live
            if not ok then pure (sceneCostCap, sceneBytesCap) else do
              spend 1
              (c1, b1) <- cmd x
              cmds (satC (c + c1)) (satB (b + b1)) rest

      cmd :: Cmd -> IO (Int, Int)
      cmd x0 = evaluate x0 >>= \x -> case x of
        CFill m r p path -> do
          ev m >> ev r
          (pc, pb) <- paint p
          n <- pathF path
          pure (1 + n + pc, 64 + segBytes * n + pb)
        CStroke m s p path -> do
          ev m >> ev s >> ev (length (ssDash s))
          (pc, pb) <- paint p
          n <- pathF path
          pure (satC (1 + 3 * n + pc + dashCost s path), 64 + segBytes * n + pb)
        CGlyphs m p gs -> do
          ev m
          (pc, pb) <- paint p
          (k, n) <- glyphsF 0 0 gs
          pure (1 + pc + n, 64 + pb + 32 * k + segBytes * n)
        CImage m img o -> do
          ev m >> ev o
          fresh <- image img
          let px = imgW img * imgH img
          pure (1 + px `div` 64, 64 + (if fresh then 4 * px else 0))
        CStencil m mk p i -> do
          ev m >> ev mk >> ev i
          (_, pb) <- paint p
          let px = mkW mk * mkH mk
          pure (1 + px `div` 64, 64 + px + pb)
        CGouraud m ts -> do
          ev m
          n <- spineF ts
          pure (1 + 8 * n, 64 + 80 * n)
        CGouraudRamp m st ts -> do
          ev m >> ev (forceList st)
          n <- spineF ts
          pure (1 + 8 * n, 64 + 56 * n)
        CClip shapes kids -> do
          n <- shapesF 0 shapes
          (kc, kb) <- list kids
          pure (satC (1 + n + kc), satB (64 + segBytes * n + kb))
        CGroup g kids -> do
          ev (goOpacity g) >> ev (goBlend g)
          (mc, mb) <- case goMask g of
            Nothing -> pure (0, 0)
            Just m -> do
              ev (smKind m) >> ev (smBackdrop m) >> ev (smTransfer m)
              (c, b) <- list (smCmds m)
              pure (2000 + c, b)
          fc <- case goFilter g of
            Nothing -> pure 0
            Just f -> ev (fRegion f) >> ev (length (fPrims f)) >> pure 2000
          (kc, kb) <- list kids
          pure (satC (1 + fc + mc + kc), satB (64 + mb + kb))
        CTransform m kids -> do
          ev m
          (kc, kb) <- list kids
          pure (satC (1 + kc), satB (64 + kb))

      paint :: Paint -> IO (Int, Int)
      paint p0 = evaluate p0 >>= \p -> case p of
        PSolid _ -> pure (0, 0)
        PLinear _ _ _ st _ -> (0, 0) <$ ev (forceList st)
        PRadial _ _ _ _ _ st _ -> (0, 0) <$ ev (forceList st)
        PSampled _ img _ -> do
          fresh <- image img
          pure (0, if fresh then 4 * imgW img * imgH img else 0)
        PTiling _ _ r _ _ kids -> do
          ev (rectF r)
          (kc, kb) <- list kids
          pure (satC (500 + 4 * kc), kb)

      -- an image counts towards the bytes once, however often it is drawn
      image :: Image -> IO Bool
      image img0 = do
        img <- evaluate img0
        if not dedup then pure True else do
          sn <- makeStableName img
          let h = hashStableName sn
          seen <- elem sn . IM.findWithDefault [] h <$> readIORef images
          if seen then pure False else do
            modifyIORef' images (IM.insertWith (++) h [sn])
            pure True

      -- force a list's elements to WHNF (every element type here has
      -- strict fields, so that is all the way down), within the fuel
      pathF :: [a] -> IO Int
      pathF xs = do
        f <- readIORef fuel
        n <- evaluate (forceUpTo (max 0 f) xs)
        spend n
        pure n
      spineF :: [a] -> IO Int
      spineF = pathF

      glyphsF :: Int -> Int -> [Glyph] -> IO (Int, Int)
      glyphsF !k !n gs0 = do
        gs <- evaluate gs0
        case gs of
          [] -> pure (k, n)
          Glyph _ _ path : rest -> do
            ok <- live
            if not ok then pure (k, n) else do
              m <- pathF path
              glyphsF (k + 1) (n + m) rest

      shapesF :: Int -> [ClipShape] -> IO Int
      shapesF !n ss0 = do
        ss <- evaluate ss0
        case ss of
          [] -> pure n
          ClipShape _ _ path : rest -> do
            m <- pathF path
            shapesF (n + m) rest

  (c, b) <- list (scCmds sc)
  ev (forceList (map length (scNotes sc)))
  left <- readIORef fuel
  pure (if left <= 0 then SceneSize sceneCostCap sceneBytesCap else SceneSize c b)
  where
    rectF (Rect a b c d) = a `seq` b `seq` c `seq` d `seq` ()
    segBytes = 40

-- | Walk at most @lim@ elements of a list, forcing each to WHNF; how many.
forceUpTo :: Int -> [a] -> Int
forceUpTo lim = go 0
  where
    go !n [] = n
    go !n (x : xs)
      | n >= lim = n
      | otherwise = x `seq` go (n + 1) xs

forceList :: [a] -> ()
forceList = foldl' (\() x -> x `seq` ()) ()

-- | A dashed stroke is as many shapes as it has dashes, which the segment
-- count cannot see: estimate them from the control polygon's length over the
-- dash period (user space, so the estimate is zoom-independent), four
-- segments a dash, capped where the dasher itself gives up.
dashCost :: StrokeStyle -> Path -> Int
dashCost ss path
  | per <= 0 || any (< 0) (ssDash ss) = 0
  | otherwise = 4 * min 100000 (floor (min 1e9 (polyLen / per)) :: Int)
  where
    per = sum (ssDash ss)
    polyLen = snd (foldl' step (Nothing, 0) path)
    step (prev, !l) sg = case sg of
      MoveTo q -> (Just q, l)
      LineTo q -> (Just q, l + d prev q)
      QuadTo a q -> (Just q, l + d prev a + d (Just a) q)
      CubicTo a b q -> (Just q, l + d prev a + d (Just a) b + d (Just b) q)
      ClosePath -> (prev, l)
    d (Just a) b = ptLen (ptSub b a)
    d Nothing _ = 0

-- | What the PDF page view may hold for the pages beside the current one
-- (the current page is always held): 'sceneBytes' summed over the cached
-- neighbours. A neighbour that does not fit is not cached at all, and so not
-- prefetched again ('Cmedit.EditorState.pvNoFit'): on a document of large
-- photographs PgDn then pays for the page's decode, which is the price of
-- not holding three pages of decoded images.
pageKeepBytes :: Int
pageKeepBytes = 160 * 1024 * 1024

-- | Scenes the inactive SVG documents may keep (the active one always
-- keeps its own), and their total 'sceneCost'.
vecKeepScenes, vecKeepCost :: Int
vecKeepScenes = 2
vecKeepCost = 2 * 1000 * 1000

-- | A 1×1 transparent image: what 'Cmedit.EditorState.ImageDoc''s bitmap
-- fields hold for a vector picture. Nothing samples it (every pixel path
-- checks for the vector first); it exists so the record stays total.
placeholderImage :: Image
placeholderImage = Image 1 1 "SVG" (listArray (0, 3) [0, 0, 0, 0])

-- | Scenes cheaper than this draw their cell picture synchronously, inside
-- the pure model, so an ordinary icon or diagram zooms with no frame of
-- latency; anything dearer goes to the background render job. Measured
-- (plan 0034 log, docs/plans/bench/SvgViewBench.hs): every system icon up to
-- cost ~20 000 rasterises at 118×76 sub-pixels in at most ~5 ms at -O2.
vecSyncCostMax :: Int
vecSyncCostMax = 20000
