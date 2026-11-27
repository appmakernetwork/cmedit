{-# LANGUAGE BangPatterns #-}
-- | The PDF page view's continuous layout ("fit width"): pages stacked in one
-- column, each as wide as the column, a gap between them, scrolled by
-- /pixels/ rather than turned by pages. Pure: geometry, tile identities, the
-- tile rasteriser and the compositors. The pure model ("Cmedit.EditorDoc")
-- decides what is wanted, the driver ("Cmedit.App") runs the jobs and emits
-- the pixels.
--
-- Three decisions carry it.
--
-- __Position is an anchor, never a document offset.__ Where the view is is
-- @(page, offset)@: the viewport's top edge lies @offset@ raster pixels
-- below the top of @page@ (an offset past the page's bottom is inside the
-- gap after it). Everything the view needs, which pages are visible and
-- where, is found by walking forward from the anchor for one screen, so it
-- costs O(screen) whatever the page count, and nothing needs the height of
-- page 1 to draw page 900 (the 'Cmedit.Pager' bargain). The model stores the
-- offset in units of the anchor page's /width/ ('offToPx'/'pxToOff'), so a
-- resize or a zoom keeps the reading position instead of the pixel count.
--
-- __Raster pixels, with an aspect.__ The same geometry serves the pixel
-- overlay (one raster pixel = one device pixel, 'sgAspect' 1) and the
-- half-block cell picture (one raster pixel = one sub-pixel, half a cell
-- tall, 'sgAspect' = its height over its width). A page is drawn
-- @colWidth@ pixels wide and @pageHeightPx@ tall in either.
--
-- __Pages are drawn once, in bands, and composited.__ Re-rasterising the
-- window on every scroll step measured 80–600 ms for a real page at 1920 px
-- wide, which is not scrolling. So a page is rasterised into horizontal
-- bands ('bandPx' tall) keyed by 'TileKey' (the scene, the scale, the
-- horizontal window, the band), each made once on a job and kept while it is
-- near the view. A scroll step then rasterises nothing: on kitty the bands
-- are uploaded once and merely re-placed (a few dozen bytes per band), and
-- everywhere else 'composeView' copies rows.
module Cmedit.PdfStrip
  ( -- * Geometry
    StripGeom(..)
  , colWidth
  , pageHeightPx
  , colLeft
  , panMax
  , Anchor
  , offToPx
  , pxToOff
  , Piece(..)
  , piecesIn
  , visiblePieces
  , settleAnchor
  , maxAnchor
  , scrollAnchor
  , currentPiece
  , anchorFraction
  , fractionAnchor
    -- * Coordinates
  , pieceScale
  , rectToPages
  , pageRectToScreen
  , pointToPage
    -- * Tiles
  , bandPx
  , TileKey(..)
  , TileBits(..)
  , tileBytes
  , tilesFor
  , renderTile
    -- * Compositing
  , composeView
  , HlRect(..)
  , tintRGBA
  , highlightImage
  , stripSample
  , cellsFrom
  ) where

import Control.Monad (forM_, when)
import Data.Array (Array, listArray)
import Data.Array.Base (unsafeAt)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Cmedit.Geom
import Cmedit.Image (Image(..), ImgMode(..))
import Cmedit.Raster (RenderOpts(..), RenderStats(..), defaultRenderOpts, renderScene)
import Cmedit.Scene (Scene(..))
import qualified Cmedit.Scene as Sc
import Cmedit.Types (Cell(..), Style(..), attrNone, blankCell)
import qualified Cmedit.Types as Ty

------------------------------------------------------------------------------
-- Geometry

-- | The column of pages, in raster pixels.
data StripGeom = StripGeom
  { sgViewW  :: !Int                       -- ^ Width of the text area.
  , sgViewH  :: !Int                       -- ^ Height of the text area.
  , sgAspect :: !Double                    -- ^ A raster pixel's height over its width (1 for device pixels).
  , sgGap    :: !Int                       -- ^ Pixels between one page and the next.
  , sgZoom   :: !Double                    -- ^ Column width over view width (1 = fit width).
  , sgPages  :: !Int
  , sgSize   :: Int -> (Double, Double)    -- ^ A page's displayed size in points (1-based).
  }

-- | The column's width: every page is drawn this wide.
colWidth :: StripGeom -> Int
colWidth g = max 8 (round (fromIntegral (max 1 (sgViewW g)) * sgZoom g))

-- | A page's height when drawn 'colWidth' wide.
pageHeightPx :: StripGeom -> Int -> Int
pageHeightPx g k =
  let (w, h) = sgSize g k
  in max 1 (round (fromIntegral (colWidth g) * max 1 h / max 1 w / max 0.05 (sgAspect g)))

-- | How far the column can pan horizontally (0 unless it is wider than the view).
panMax :: StripGeom -> Int
panMax g = max 0 (colWidth g - sgViewW g)

-- | Screen x of the column's left edge: centred when it is narrower than the
-- view, otherwise panned (@pan@ pixels from its left edge, clamped).
colLeft :: StripGeom -> Int -> Int
colLeft g pan
  | colWidth g <= sgViewW g = (sgViewW g - colWidth g) `div` 2
  | otherwise = negate (max 0 (min (panMax g) pan))

-- | @(page, offset)@: the view's top edge is @offset@ pixels below the top
-- of @page@; an offset at or past the page's height is in the gap after it.
type Anchor = (Int, Double)

-- | The model's offset (in units of the page's width in points, i.e. points
-- down the page over its width) as pixels in this geometry, and back.
offToPx :: StripGeom -> Int -> Double -> Double
offToPx g _ off = off * fromIntegral (colWidth g) / max 0.05 (sgAspect g)

pxToOff :: StripGeom -> Int -> Double -> Double
pxToOff g _ px = px / (fromIntegral (colWidth g) / max 0.05 (sgAspect g))

-- | One page as placed on screen.
data Piece = Piece
  { pcPage :: !Int
  , pcTop  :: !Int   -- ^ Screen y of the page's top edge (negative when scrolled past).
  , pcH    :: !Int   -- ^ Its height.
  } deriving (Eq, Ord, Show)

-- | The pages overlapping screen rows @[lo, hi)@ (which may reach above or
-- below the view, for prefetching), top to bottom.
piecesIn :: StripGeom -> Anchor -> Int -> Int -> [Piece]
piecesIn g (p0, off) lo hi =
  let top0 = negate (round off) :: Int
      -- walk back while the region reaches above the anchor page
      back k t | k > 1 && t > lo = let k' = k - 1; h = pageHeightPx g k' in back k' (t - sgGap g - h)
               | otherwise = (k, t)
      (k1, t1) = back (max 1 (min (sgPages g) p0)) top0
      fwd k t | k > sgPages g || t >= hi = []
              | otherwise = let h = pageHeightPx g k
                                rest = fwd (k + 1) (t + h + sgGap g)
                            in if t + h > lo then Piece k t h : rest else rest
  in fwd k1 t1

-- | The pages on screen.
visiblePieces :: StripGeom -> Anchor -> [Piece]
visiblePieces g a = piecesIn g a 0 (sgViewH g)

-- | Move the anchor onto the page its offset is actually in (an offset may
-- have been pushed past a page or before it by a scroll), then hold it
-- between the top of the document and 'maxAnchor'.
settleAnchor :: StripGeom -> Anchor -> Anchor
settleAnchor g (p0, o0) = clampA (norm (max 1 (min n p0)) o0)
  where
    n = max 1 (sgPages g)
    norm p o
      | o < 0, p > 1 = let p' = p - 1 in norm p' (o + fromIntegral (pageHeightPx g p' + sgGap g))
      | o < 0 = (1, 0)
      | p < n, o >= fromIntegral (pageHeightPx g p + sgGap g) = norm (p + 1) (o - fromIntegral (pageHeightPx g p + sgGap g))
      | otherwise = (p, o)
    clampA a@(p, o) =
      let (mp, mo) = maxAnchor g
      in if p > mp || (p == mp && o > mo) then (mp, mo) else a

-- | The furthest the view scrolls: the last page's bottom edge on the view's
-- bottom edge (or the top of the document, when it all fits).
maxAnchor :: StripGeom -> Anchor
maxAnchor g = go (max 1 (sgPages g)) (fromIntegral (sgViewH g))
  where
    go k rem'
      | h >= rem' = (k, h - rem')
      | k <= 1 = (1, 0)
      | gap >= rem' - h = (k - 1, fromIntegral (pageHeightPx g (k - 1)) + gap - (rem' - h))
      | otherwise = go (k - 1) (rem' - h - gap)
      where h = fromIntegral (pageHeightPx g k)
            gap = fromIntegral (sgGap g)

-- | Scroll by @d@ pixels (down when positive).
scrollAnchor :: StripGeom -> Double -> Anchor -> Anchor
scrollAnchor g d (p, o) = settleAnchor g (p, o + d)

-- | The page the view is "on": the one with the most rows showing (the
-- upper one on a tie).
currentPiece :: StripGeom -> Anchor -> Maybe Piece
currentPiece g a = case visiblePieces g a of
  [] -> Nothing
  ps -> Just (snd (foldl1 better [ (shown pc, pc) | pc <- ps ]))
  where
    shown pc = min (sgViewH g) (pcTop pc + pcH pc) - max 0 (pcTop pc)
    better x@(sx, _) y@(sy, _) = if sy > sx then y else x

-- | Where the anchor is, as a fraction of the document (for the scroll bar):
-- pages are counted as equal, which keeps this O(1) on a thousand-page file
-- and is exact for the usual document of one page size.
anchorFraction :: StripGeom -> Anchor -> Double
anchorFraction g (p, o) =
  let n = max 1 (sgPages g)
      unit = fromIntegral (pageHeightPx g p + sgGap g)
  in (fromIntegral (p - 1) + min 1 (o / max 1 unit)) / fromIntegral n

-- | The inverse of 'anchorFraction' (settled).
fractionAnchor :: StripGeom -> Double -> Anchor
fractionAnchor g f =
  let n = max 1 (sgPages g)
      x = max 0 (min 1 f) * fromIntegral n
      p = max 1 (min n (floor x + 1))
      within = x - fromIntegral (p - 1)
  in settleAnchor g (p, within * fromIntegral (pageHeightPx g p + sgGap g))

------------------------------------------------------------------------------
-- Coordinates

-- | Raster pixels per point on a page: @(x, y)@.
pieceScale :: StripGeom -> Int -> (Double, Double)
pieceScale g k =
  let (w, _) = sgSize g k
      sx = fromIntegral (colWidth g) / max 1 w
  in (sx, sx / max 0.05 (sgAspect g))

-- | A screen rectangle (raster pixels, @x0 y0 x1 y1@) as the part of it on
-- each page, in that page's points (scene coordinates). A rectangle across a
-- page break is two rectangles; one in a gap or beside the column is none.
rectToPages :: StripGeom -> Int -> Anchor -> Rect -> [(Int, Rect)]
rectToPages g pan a (Rect x0 y0 x1 y1) =
  [ (pcPage pc, Rect ((cx0 - cl) / sx) ((cy0 - t) / sy) ((cx1 - cl) / sx) ((cy1 - t) / sy))
  | pc <- piecesIn g a (floor y0) (ceiling y1 + 1)
  , let t = fromIntegral (pcTop pc)
        b = t + fromIntegral (pcH pc)
        cl = fromIntegral (colLeft g pan)
        cr = cl + fromIntegral (colWidth g)
        (sx, sy) = pieceScale g (pcPage pc)
        cy0 = max t y0; cy1 = min b y1
        cx0 = max cl x0; cx1 = min cr x1
  , cy1 > cy0, cx1 > cx0 ]

-- | A rectangle in a page's points, on screen (raster pixels), when the page
-- is placed as @pc@.
pageRectToScreen :: StripGeom -> Int -> Piece -> Rect -> Rect
pageRectToScreen g pan pc (Rect x0 y0 x1 y1) =
  let (sx, sy) = pieceScale g (pcPage pc)
      cl = fromIntegral (colLeft g pan)
      t = fromIntegral (pcTop pc)
  in Rect (cl + x0 * sx) (t + y0 * sy) (cl + x1 * sx) (t + y1 * sy)

-- | The page and point under a screen pixel, if it is on a page. A pixel in
-- a gap belongs to the nearer page (so zooming about a pointer between two
-- pages still has a page to hold still).
pointToPage :: StripGeom -> Int -> Anchor -> (Double, Double) -> Maybe (Int, (Double, Double))
pointToPage g pan a (x, y) =
  case piecesIn g a (floor y - sgGap g - 1) (floor y + sgGap g + 2) of
    [] -> Nothing
    ps ->
      let dist pc = let t = fromIntegral (pcTop pc); b = t + fromIntegral (pcH pc)
                    in if y < t then t - y else if y >= b then y - b + 1 else 0
          pc = snd (minimum [ (dist p, (pcPage p, p)) | p <- ps ])
          (sx, sy) = pieceScale g (pcPage (snd pc))
          cl = fromIntegral (colLeft g pan)
          t = fromIntegral (pcTop (snd pc))
      in Just (fst pc, ((x - cl) / sx, (y - t) / sy))

------------------------------------------------------------------------------
-- Tiles

-- | Height of one band of a page (device pixels). Small enough that the
-- first screen is a handful of short jobs, large enough that a page is a few
-- dozen of them at most.
bandPx :: Int
bandPx = 256

-- | One band of one page at one scale: the scene, the page, the column width
-- and pixel aspect it was drawn for, and the pixel window inside the page
-- (@x, w@ across the column; @y, h@ down the page).
data TileKey = TileKey
  { tkScene  :: !Int
  , tkPage   :: !Int
  , tkColW   :: !Int
  , tkAspect :: !Int    -- ^ 'sgAspect' × 1000, rounded: a cell raster and a pixel raster never share a key.
  , tkX      :: !Int
  , tkW      :: !Int
  , tkY      :: !Int
  , tkH      :: !Int
  } deriving (Eq, Ord, Show)

-- | A rendered band: RGBA rows, and (when the terminal takes compressed
-- payloads) its zlib stream, compressed on the job.
data TileBits = TileBits
  { tbW    :: !Int
  , tbH    :: !Int
  , tbRGBA :: !BS.ByteString
  , tbZlib :: !(Maybe BS.ByteString)
  }

instance Show TileBits where
  show t = "TileBits " ++ show (tbW t) ++ "x" ++ show (tbH t)

tileBytes :: TileBits -> Int
tileBytes t = BS.length (tbRGBA t) + maybe 0 BS.length (tbZlib t)

-- | Width of one horizontal tile of a column wider than the view (zoomed
-- in): panning then draws only the tiles it exposes, never the whole band.
tileColPx :: Int
tileColPx = 1024

-- | The bands a piece needs for screen rows @[lo, hi)@, in order. A column
-- that fits the view is one band wide; a wider one is cut into
-- 'tileColPx'-wide tiles at fixed positions, so a pan keeps every tile still
-- in view. With @whole@ (the cell picture: a page there is a few hundred
-- sub-pixels) the page is one tile.
tilesFor :: StripGeom -> Int -> Bool -> (Int -> Int) -> Piece -> Int -> Int -> [TileKey]
tilesFor g pan whole sceneOf pc lo hi =
  let cw = colWidth g
      cl = colLeft g pan
      xa = max 0 (negate cl)
      xb = min cw (sgViewW g - cl)
      ya = max 0 (lo - pcTop pc)
      yb = min (pcH pc) (hi - pcTop pc)
      asp = round (sgAspect g * 1000)
      key x w y h = TileKey (sceneOf (pcPage pc)) (pcPage pc) cw asp x w y h
      xs | whole || cw <= sgViewW g = [(0, cw)]
         | otherwise = [ (x, min tileColPx (cw - x)) | c <- [xa `div` tileColPx .. (xb - 1) `div` tileColPx], let x = c * tileColPx ]
  in if xb <= xa || yb <= ya then []
     else if whole then [key 0 cw 0 (pcH pc)]
     else [ key x w y (min bandPx (pcH pc - y))
          | b <- [ya `div` bandPx .. (yb - 1) `div` bandPx], let y = b * bandPx, (x, w) <- xs ]

-- | Rasterise one band of a page's scene (white paper behind it, so a
-- rounding edge never shows through as a hole). The scene's own bounds are
-- the page: its width is spanned by 'tkColW' pixels.
renderTile :: Int -> Scene -> TileKey -> Maybe (TileBits, RenderStats)
renderTile budget sc k
  | tkW k <= 0 || tkH k <= 0 || tkW k * tkH k > 16 * 1000 * 1000 = Nothing
  | otherwise =
      let Rect x0 y0 x1 _ = scBounds sc
          sx = fromIntegral (tkColW k) / max 1e-9 (x1 - x0)
          sy = sx / max 0.05 (fromIntegral (tkAspect k) / 1000)
          view = Affine sx 0 0 sy (negate (x0 * sx) - fromIntegral (tkX k)) (negate (y0 * sy) - fromIntegral (tkY k))
          opts = (defaultRenderOpts (tkW k) (tkH k) view) { roBudget = budget, roBackground = Sc.white }
          (img, st) = renderScene opts sc
      in if imgW img == tkW k && imgH img == tkH k
           then Just (TileBits (tkW k) (tkH k) (rgbaOf img) Nothing, st)
           else Nothing

-- An image's pixels as a strict ByteString (one copy).
rgbaOf :: Image -> BS.ByteString
rgbaOf img =
  let n = 4 * imgW img * imgH img
      arr = imgPix img
  in if n <= 0 then BS.empty
     else BI.unsafeCreate n (\p -> let go !i | i >= n = pure ()
                                             | otherwise = pokeByteOff p i (unsafeAt arr i) >> go (i + 1)
                                   in go 0)

------------------------------------------------------------------------------
-- Compositing

-- | A highlight to blend over the picture: a rectangle in raster pixels, its
-- colour and opacity, and whether it is an outline (the rubber band) or a
-- fill (selected text).
data HlRect = HlRect
  { hrRect    :: !Rect
  , hrRGB     :: !(Word8, Word8, Word8)
  , hrAlpha   :: !Double
  , hrOutline :: !Int     -- ^ 0: a fill; otherwise an outline this many pixels wide (the rubber band).
  } deriving (Eq, Show)

-- | The view as one RGBA picture (@viewW × viewH@): the bands that are
-- present copied into place, the parts of pages whose bands are still being
-- drawn as plain paper, and everything else (the gaps, beside the column)
-- transparent, or the opaque @desk@ colour when given. (Sixel needs the
-- desk: a transparent sixel pixel leaves whatever was drawn there before,
-- and the cells under it never change, so the last frame's page would show
-- through the gaps.) Then the highlights.
composeView :: StripGeom -> Int -> [Piece] -> (TileKey -> Maybe TileBits) -> (Int -> [TileKey]) -> [HlRect]
            -> Maybe (Word8, Word8, Word8) -> BS.ByteString
composeView g pan pieces look keysOf hls desk =
  let vw = sgViewW g; vh = sgViewH g
      n = vw * vh * 4
      cl = colLeft g pan
  in if n <= 0 then BS.empty else tintRGBA vw vh hls $ BI.unsafeCreate n $ \dst -> do
       case desk of
         Nothing -> fillBytes dst 0 n
         Just (r, gg, b) -> forM_ [0, 4 .. n - 4] $ \o -> do
           pokeByteOff dst o r; pokeByteOff dst (o + 1) gg; pokeByteOff dst (o + 2) b
           pokeByteOff dst (o + 3) (0xFF :: Word8)
       forM_ pieces $ \pc -> forM_ (keysOf (pcPage pc)) $ \k -> do
         let sx0 = cl + tkX k           -- screen x of the tile's left edge
             sy0 = pcTop pc + tkY k     -- screen y of its top edge
             x0 = max 0 sx0; x1 = min vw (sx0 + tkW k)
             y0 = max 0 sy0; y1 = min vh (sy0 + tkH k)
             w = x1 - x0
         when (w > 0 && y1 > y0) $ case look k of
           Just tb | tbW tb == tkW k, tbH tb == tkH k, BS.length (tbRGBA tb) >= 4 * tbW tb * tbH tb ->
             BU.unsafeUseAsCString (tbRGBA tb) $ \src0 -> do
               let src = castPtr' src0
               forM_ [y0 .. y1 - 1] $ \y ->
                 copyBytes (dst `plusPtr` ((y * vw + x0) * 4))
                           (src `plusPtr` (((y - sy0) * tbW tb + (x0 - sx0)) * 4))
                           (w * 4)
           _ -> forM_ [y0 .. y1 - 1] $ \y -> paper dst ((y * vw + x0) * 4) w
  where
    castPtr' :: Ptr a -> Ptr Word8
    castPtr' p = p `plusPtr` 0
    -- unrendered page: a light paper grey, so its shape shows while it draws
    paper :: Ptr Word8 -> Int -> Int -> IO ()
    paper dst off w = forM_ [0 .. w - 1] $ \i -> do
      let o = off + 4 * i
      pokeByteOff dst o (0xEE :: Word8)
      pokeByteOff dst (o + 1) (0xEE :: Word8)
      pokeByteOff dst (o + 2) (0xEC :: Word8)
      pokeByteOff dst (o + 3) (0xFF :: Word8)

-- | Blend highlights into an RGBA picture (@w × h@). A fill blends over
-- whatever is there; an outline is two pixels wide. Transparent pixels stay
-- transparent under a fill (a highlight never paints the gap between pages).
tintRGBA :: Int -> Int -> [HlRect] -> BS.ByteString -> BS.ByteString
tintRGBA _ _ [] bs = bs
tintRGBA w h hls bs
  | BS.length bs < w * h * 4 = bs
  | otherwise = unsafeDupablePerformIO $ BI.create (BS.length bs) $ \dst -> do
      BU.unsafeUseAsCString bs $ \src -> copyBytes dst (src `plusPtr` 0) (BS.length bs)
      forM_ hls $ \hr -> do
        let Rect fx0 fy0 fx1 fy1 = hrRect hr
            x0 = max 0 (floor fx0); y0 = max 0 (floor fy0)
            x1 = min w (ceiling fx1); y1 = min h (ceiling fy1)
            (cr, cg, cb) = hrRGB hr
            a = max 0 (min 1 (hrAlpha hr))
            lw = hrOutline hr
        forM_ [y0 .. y1 - 1] $ \y -> forM_ (spanXs lw x0 x1 y0 y1 y) $ \x -> do
            let o = (y * w + x) * 4
            pa <- peekByteOff dst (o + 3) :: IO Word8
            if lw > 0
              then do
                blend dst o 0 cr a; blend dst o 1 cg a; blend dst o 2 cb a
                pokeByteOff dst (o + 3) (max pa (round (255 * a)) :: Word8)
              else when (pa > 0) $ do
                blend dst o 0 cr a; blend dst o 1 cg a; blend dst o 2 cb a
  where
    blend :: Ptr Word8 -> Int -> Int -> Word8 -> Double -> IO ()
    blend p o i c a = do
      v <- peekByteOff p (o + i) :: IO Word8
      pokeByteOff p (o + i) (round (fromIntegral v * (1 - a) + fromIntegral c * a) :: Word8)

-- The columns of row @y@ a highlight covers: all of them for a fill (@lw@
-- 0) and for the top and bottom edges of an outline, only its two side
-- strips otherwise (so an outline costs its perimeter, not its area).
spanXs :: Int -> Int -> Int -> Int -> Int -> Int -> [Int]
spanXs lw x0 x1 y0 y1 y
  | lw <= 0 || y < y0 + lw || y >= y1 - lw = [x0 .. x1 - 1]
  | otherwise = [x0 .. min (x1 - 1) (x0 + lw - 1)] ++ [max (x0 + lw) (x1 - lw) .. x1 - 1]

-- | A highlight layer on its own, for a terminal that stacks images (kitty):
-- a transparent @w × h@ RGBA picture with the highlights drawn into it, the
-- rectangles relative to its top-left corner.
highlightImage :: Int -> Int -> [HlRect] -> BS.ByteString
highlightImage w h hls
  | w <= 0 || h <= 0 = BS.empty
  | otherwise = unsafeDupablePerformIO $ BI.create (w * h * 4) $ \dst -> do
      fillBytes dst 0 (w * h * 4)
      forM_ hls $ \hr -> do
        let Rect fx0 fy0 fx1 fy1 = hrRect hr
            x0 = max 0 (floor fx0); y0 = max 0 (floor fy0)
            x1 = min w (ceiling fx1); y1 = min h (ceiling fy1)
            (cr, cg, cb) = hrRGB hr
            a = round (255 * max 0 (min 1 (hrAlpha hr))) :: Word8
            lw = hrOutline hr
        forM_ [y0 .. y1 - 1] $ \y -> forM_ (spanXs lw x0 x1 y0 y1 y) $ \x -> do
            let o = (y * w + x) * 4
            pokeByteOff dst o cr
            pokeByteOff dst (o + 1) cg
            pokeByteOff dst (o + 2) cb
            pokeByteOff dst (o + 3) a

-- | The colour of the layout at a raster pixel (its centre @(x, y)@):
-- the band covering it, paper for a page whose band is still being drawn,
-- 'Nothing' off the pages (the gaps, beside the column). Highlight fills are
-- blended in. One lookup per call, for the cell picture's few thousand
-- sub-pixels; the pixel overlay never samples.
stripSample :: StripGeom -> Int -> [Piece] -> (TileKey -> Maybe TileBits) -> (Int -> [TileKey]) -> [HlRect]
            -> (Double, Double) -> Maybe (Word8, Word8, Word8)
stripSample g pan pieces look keysOf hls (x, y) =
  case [ pc | pc <- pieces, fromIntegral (pcTop pc) <= y, y < fromIntegral (pcTop pc + pcH pc) ] of
    [] -> Nothing
    (pc : _) ->
      let cl = colLeft g pan
          px = floor x - cl
          py = floor y - pcTop pc
      in if px < 0 || px >= colWidth g then Nothing else
           let base = case [ (k, tb) | k <- keysOf (pcPage pc), px >= tkX k, px < tkX k + tkW k
                                     , py >= tkY k, py < tkY k + tkH k, Just tb <- [look k] ] of
                 ((k, tb) : _) | BS.length (tbRGBA tb) >= 4 * tbW tb * tbH tb ->
                   let o = ((py - tkY k) * tbW tb + (px - tkX k)) * 4
                       c i = BU.unsafeIndex (tbRGBA tb) (o + i)
                   in (c 0, c 1, c 2)
                 _ -> (0xEE, 0xEE, 0xEC)
           in Just (foldl (tint (x, y)) base hls)
  where
    tint (px, py) c@(r, gg, b) hr =
      let Rect x0 y0 x1 y1 = hrRect hr
          a = hrAlpha hr
          (cr, cg, cb) = hrRGB hr
          mix v w = round (fromIntegral v * (1 - a) + fromIntegral w * a) :: Word8
      in if hrOutline hr == 0 && px >= x0 && px < x1 && py >= y0 && py < y1
           then (mix r cr, mix gg cg, mix b cb) else c

-- | Half-block (or ASCII) cells for a @cols × rows@ text area from a
-- sampler over its @cols × 2·rows@ sub-pixels ('Nothing': the terminal's
-- background).
cellsFrom :: ImgMode -> Int -> Int -> (Int -> Int -> Maybe (Word8, Word8, Word8)) -> Array (Int, Int) Cell
cellsFrom mode cols rows px =
  listArray ((0, 0), (rows - 1, cols - 1)) [ cellAt r c | r <- [0 .. rows - 1], c <- [0 .. cols - 1] ]
  where
    rgb (r, g, b) = Ty.ColorRGB r g b
    cellAt r c = case mode of
      HalfBlock -> case (px c (2 * r), px c (2 * r + 1)) of
        (Nothing, Nothing) -> blankCell
        (Just t, Nothing)  -> Cell '\x2580' (Style (rgb t) Ty.Default attrNone)
        (Nothing, Just b)  -> Cell '\x2584' (Style (rgb b) Ty.Default attrNone)
        (Just t, Just b)   -> Cell '\x2580' (Style (rgb t) (rgb b) attrNone)
      Ascii -> case [ v | Just v <- [px c (2 * r), px c (2 * r + 1)] ] of
        [] -> blankCell
        vs -> let n = length vs
                  s f = sum (map (fromIntegral . f) vs) `div` n :: Int
                  r' = s (\(a, _, _) -> a); g' = s (\(_, b, _) -> b); b' = s (\(_, _, d) -> d)
                  lum = (r' * 30 + g' * 59 + b' * 11) `div` 100
              in Cell (ramp lum) (Style (Ty.ColorRGB (fromIntegral r') (fromIntegral g') (fromIntegral b')) Ty.Default attrNone)
    -- the image view's ramp (darkest to brightest)
    ramp lum = let cs = " .:-=+*#%@" :: String
                   i = max 0 (min 255 lum) * (length cs - 1) `div` 255
               in cs !! max 0 (min (length cs - 1) i)
