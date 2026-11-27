{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the PDF page view's fit-width layout ("Cmedit.PdfStrip" and its
-- model in "Cmedit.EditorDoc") and the select tool. The driver's half (band
-- jobs, kitty placements, the highlight layer) is exercised by
-- docs/plans/bench/pty_pdfstrip.py.
--
-- Spec.hs calls 'pdfStripSpec' with its own @check@.
module PdfStripSpec (pdfStripSpec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T
import qualified Data.Array as A

import Cmedit.Types
import Cmedit.Editor
import Cmedit.ConfigFile (parseConfigText, updateConfigText)
import Cmedit.EditorState (Effect(..), computeLayout, Layout(..), scrollBarInfo, imageOverlayWanted)
import Cmedit.Geom
import Cmedit.Image (ImgMode(..))
import Cmedit.Pdf (PdfDoc(..), parsePdf, pdfPageSource, pdfPageGlyphs)
import Cmedit.PdfObj (fuelDefault)
import qualified Cmedit.PdfRender as PR
import Cmedit.PdfStrip
import Cmedit.Scene (Scene(..))
import Cmedit.VecView (SceneSize(..), sceneSize)

pdfStripSpec :: (String -> Bool -> IO ()) -> IO ()
pdfStripSpec check = do
  geomTests check
  tileTests check
  modelTests check
  selectTests check
  pointerTests check
  cropShapeTests check
  configTests check

------------------------------------------------------------------------------
-- Fixtures

pdfBytes :: Int -> BC.ByteString
pdfBytes n =
  let page i = concat
        [ "BT /F1 24 Tf 72 720 Td (This is page " ++ show i ++ ") Tj ET\n"
        , concat [ "BT /F1 11 Tf 72 " ++ show (690 - 15 * k) ++ " Td (Body line " ++ show k ++ " of page " ++ show i ++ ") Tj ET\n" | k <- [0 .. 39 :: Int] ] ]
      objs = [ "<< /Type /Catalog /Pages 2 0 R >>"
             , "<< /Type /Pages /Kids [" ++ unwords [ show (4 + 2 * k) ++ " 0 R" | k <- [0 .. n - 1] ] ++ "] /Count " ++ show n ++ " >>"
             , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>" ]
             ++ concat [ [ "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 3 0 R >> >> /Contents " ++ show (5 + 2 * k) ++ " 0 R >>"
                         , let c = page (k + 1) in "<< /Length " ++ show (length c) ++ " >>\nstream\n" ++ c ++ "\nendstream" ]
                       | k <- [0 .. n - 1] ]
      body = concat [ show k ++ " 0 obj\n" ++ o ++ "\nendobj\n" | (k, o) <- zip [1 :: Int ..] objs ]
  in BC.pack ("%PDF-1.4\n" ++ body ++ "trailer\n<< /Size " ++ show (length objs + 1) ++ " /Root 1 0 R >>\n%%EOF\n")

doc5 :: PdfDoc
doc5 = either error id (parsePdf (pdfBytes 5))

-- A five-page US-letter column, 800 px wide, 600 tall, a 16 px gap.
geom :: StripGeom
geom = StripGeom 800 600 1 16 1 5 (const (612, 792))

-- The fit-width page view of doc5 on a terminal of the given kind.
openStrip :: Bool -> Editor
openStrip gfx =
  let cfg = defaultConfig { cfgPdfView = PdfViewPage, cfgPdfFit = PdfFitWidth }
      e = (newEditor (40, 100) cfg) { edGfxCaps = gfx, edGfxKitty = gfx, edCellPx = if gfx then Just (8, 16) else Nothing }
  in pdfLoaded "/tmp/doc.pdf" doc5 e

key :: Key -> Editor -> Editor
key k e = fst (update k e)

pv' :: Editor -> PdfView
pv' = fromMaybe (error "no page view") . edPdfView

-- Play the driver: parse every page the view wants (scene and glyphs), then
-- render every band it wants, until nothing more is asked for.
settle :: Editor -> Editor
settle = go (50 :: Int)
  where
    go 0 ed = ed
    go n ed = case pdfPageWant ed of
      Just w -> go (n - 1) (parse w ed)
      Nothing -> case pdfPrefetchWant ed of
        Just w -> go (n - 1) (parse w ed)
        Nothing -> case pdfTileWants ed of
          ((k, sc) : _) -> go (n - 1) (pdfTileRendered k (fmap fst (renderTile 50000000 sc k)) ed)
          [] -> ed
    parse w ed = case pdfPageSource doc5 (pwPage w) of
      Nothing -> ed
      Just (f, d) ->
        let (sc, _) = PR.renderPage (PR.RenderEnv (const Nothing) True) f fuelDefault d
        in pdfPageParsed w (1000 + pwPage w) False (Right (sc, sceneSize sc, []))
             (pdfGlyphsArrived w (pdfPageGlyphs f d) ed)

mouse :: MouseButton -> Bool -> Bool -> Int -> Int -> Editor -> Editor
mouse b pressed drag row col = key (KMouse (MouseEvent b col row pressed drag noMods 1))

------------------------------------------------------------------------------

geomTests :: (String -> Bool -> IO ()) -> IO ()
geomTests check = do
  check "strip: the column is the view's width" (colWidth geom == 800)
  check "strip: a letter page at 800 px is 1035 tall" (pageHeightPx geom 1 == 1035)
  check "strip: at the top, one page shows" (visiblePieces geom (1, 0) == [Piece 1 0 1035])
  let a1 = scrollAnchor geom 1000 (1, 0)
  check "strip: scrolling 1000 px stays on page 1" (a1 == (1, 1000))
  check "strip: ... and shows the next page below the gap"
    (visiblePieces geom a1 == [Piece 1 (-1000) 1035, Piece 2 51 1035])
  check "strip: an anchor in the gap stays on its page" (scrollAnchor geom 1040 (1, 0) == (1, 1040))
  check "strip: past the gap the anchor moves to the next page" (scrollAnchor geom 1060 (1, 0) == (2, 9))
  check "strip: scrolling up across a page break" (scrollAnchor geom (-20) (2, 9) == (1, 1040))
  check "strip: the top clamps" (scrollAnchor geom (-50) (1, 10) == (1, 0))
  check "strip: the end is the last page's bottom on the view's" (maxAnchor geom == (5, 435))
  check "strip: scrolling past the end clamps to it" (scrollAnchor geom 99999 (1, 0) == (5, 435))
  check "strip: a document shorter than the view does not scroll"
    (maxAnchor geom { sgViewH = 99999 } == (1, 0))
  check "strip: the current page is the most visible one"
    (fmap pcPage (currentPiece geom (1, 800)) == Just 2 && fmap pcPage (currentPiece geom (1, 700)) == Just 1)
  check "strip: the anchor as a fraction and back"
    (let a = (3, 500); f = anchorFraction geom a; (p, o) = fractionAnchor geom f in p == 3 && abs (o - 500) < 1)
  check "strip: a rectangle across a page break is two"
    (map fst (rectToPages geom 0 (1, 1000) (Rect 100 10 300 100)) == [1, 2])
  check "strip: a rectangle in the gap is none" (null (rectToPages geom 0 (1, 1000) (Rect 100 36 300 50)))
  check "strip: points map to the page and back"
    (case pointToPage geom 0 (1, 1000) (400, 300) of
       Just (2, (x, y)) -> let Rect sx sy _ _ = pageRectToScreen geom 0 (Piece 2 51 1035) (Rect x y x y)
                           in abs (sx - 400) < 1e-6 && abs (sy - 300) < 1e-6
       _ -> False)
  -- zoomed in, the column is wider than the view and pans
  let gz = geom { sgZoom = 2 }
  check "strip: zoomed, the column pans" (panMax gz == 800 && colLeft gz 300 == -300 && colLeft gz 5000 == -800)
  check "strip: zoomed out, the column is centred" (colLeft geom { sgZoom = 0.5 } 0 == 200)
  -- the cell geometry: a sub-pixel is not square
  let gc = geom { sgAspect = 0.5 }
  check "strip: a shorter pixel (half-block) takes more of them" (pageHeightPx gc 1 == 2071)

tileTests :: (String -> Bool -> IO ()) -> IO ()
tileTests check = do
  let pc = Piece 1 0 1035
      ks = tilesFor geom 0 False (const 7) pc 0 600
  check "tiles: the bands on screen" (map tkY ks == [0, 256, 512] && all ((== 256) . tkH) ks)
  check "tiles: ... span the column" (all (\k -> tkX k == 0 && tkW k == 800 && tkScene k == 7) ks)
  check "tiles: the last band is short" (map tkH (tilesFor geom 0 False (const 7) pc 0 2000) == [256, 256, 256, 256, 11])
  check "tiles: a cell picture's page is one band" (map tkH (tilesFor geom 0 True (const 7) pc 0 600) == [1035])
  check "tiles: zoomed, a band is cut into fixed tiles across the view"
    (map (\k -> (tkX k, tkW k)) (tilesFor geom { sgZoom = 2 } 300 False (const 7) (Piece 1 0 2070) 0 100) == [(0, 1024), (1024, 576)])
  check "tiles: a pan keeps the tiles still in view"
    (tilesFor geom { sgZoom = 2 } 0 False (const 7) (Piece 1 0 2070) 0 100
       == take 1 (tilesFor geom { sgZoom = 2 } 300 False (const 7) (Piece 1 0 2070) 0 100))
  let desk = composeView (StripGeom 8 10 1 1 1 2 (const (8, 2))) 0 (visiblePieces (StripGeom 8 10 1 1 1 2 (const (8, 2))) (1, 0))
               (const Nothing) (const []) [] (Just (1, 2, 3))
  check "compose: with a desk colour nothing is transparent" (BS.unpack (BS.take 4 (BS.drop (2 * 8 * 4) desk)) == [1, 2, 3, 255])
  -- compositing: one red band, one missing (paper), the gap transparent
  let g = StripGeom 8 10 1 1 1 2 (const (8, 2))     -- pages 8×2 px, one-pixel gap
      pieces = visiblePieces g (1, 0)
      red = TileBits 8 2 (BS.pack (concat (replicate 16 [255, 0, 0, 255]))) Nothing
      keysOf k = tilesFor g 0 False (const k) (head [ p | p <- pieces, pcPage p == k ]) 0 10
      look k = if tkPage k == 1 then Just red else Nothing
      out = composeView g 0 pieces look keysOf [] Nothing
      px x y = BS.unpack (BS.take 4 (BS.drop ((y * 8 + x) * 4) out))
  check "compose: pieces are the two pages and the gap" (map pcTop pieces == [0, 3])
  check "compose: a band is copied into place" (px 1 1 == [255, 0, 0, 255])
  check "compose: the gap is transparent" (px 1 2 == [0, 0, 0, 0])
  check "compose: a band still drawing is paper" (px 1 3 == [0xEE, 0xEE, 0xEC, 0xFF])
  let tinted = composeView g 0 pieces look keysOf [HlRect (Rect 0 0 2 1) (0, 0, 255) 1 0] Nothing
      tpx x y = BS.unpack (BS.take 4 (BS.drop ((y * 8 + x) * 4) tinted))
  check "compose: a highlight fill tints the page" (tpx 0 0 == [0, 0, 255, 255] && tpx 3 0 == [255, 0, 0, 255])
  let outlined = tintRGBA 8 10 [HlRect (Rect 0 0 8 10) (0, 255, 0) 1 1] out
      opx x y = BS.unpack (BS.take 4 (BS.drop ((y * 8 + x) * 4) outlined))
  check "outline: the edge is drawn, the inside is not" (opx 0 1 == [0, 255, 0, 255] && opx 1 1 == [255, 0, 0, 255])
  check "outline: it crosses the gap too" (opx 0 2 == [0, 255, 0, 255])
  let hi = highlightImage 3 2 [HlRect (Rect 1 0 3 2) (1, 2, 3) 1 0]
  check "highlight image: transparent outside the rectangles"
    (BS.unpack (BS.take 4 hi) == [0, 0, 0, 0] && BS.unpack (BS.take 4 (BS.drop 4 hi)) == [1, 2, 3, 255])

modelTests :: (String -> Bool -> IO ()) -> IO ()
modelTests check = do
  let s0 = openStrip True
  check "model: pdf-fit = width opens the strip" (pageStripOn s0)
  check "model: the single-picture machinery is off" (isNothing (activePicture s0) && not (imageOverlayWanted s0))
  check "model: the first page is wanted" (fmap pwPage (pdfPageWant s0) == Just 1)
  check "model: no bands without a scene" (null (pdfTileWants s0))
  let s1 = settle s0
      tiles1 = pvTiles (pv' s1)
  check "model: after the jobs every band on screen is drawn" (stripScreenReady s1 && not (M.null tiles1))
  check "model: the status says fit width" ("Width 100%" `T.isInfixOf` T.pack (pageStatusFor s1 (pv' s1)))
  check "model: the scroll bar is up" (isJust (scrollBarInfo s1))
  -- scrolling is by rows, in pixels, across page breaks
  let s2 = key (KArrow DDown noMods) s1
  check "model: Down scrolls two rows (32 px)" (pvAnchor (pv' s2) == 1 && abs (pvOff (pv' s2) * 800 - 32) < 0.5)
  let s3 = foldr (const (key (KPageDown noMods))) s1 [1 .. 3 :: Int]
  check "model: PgDn scrolls into the next pages" (pvPage (pv' s3) >= 2)
  check "model: a scroll keeps the bands still on screen"
    (not (M.null (M.intersection tiles1 (pvTiles (pv' (key (KArrow DDown noMods) s1))))))
  let s4 = key (KChar ']') s1
  check "model: ] goes to the next page's top" (pvAnchor (pv' s4) == 2 && pvOff (pv' s4) == 0 && pvPage (pv' s4) == 2)
  check "model: End reaches the last page" (pvPage (pv' (key (KEnd noMods) s1)) == 5)
  check "model: Ctrl+G's page jump lands in the strip" (pvAnchor (pv' (pdfTurnTo 4 s1)) == 4)
  check "model: the wheel scrolls three rows"
    (abs (pvOff (pv' (mouse MBWheelDown False False 10 10 s1)) * 800 - 48) < 0.5)
  -- zoom
  let z1 = key (KChar '+') s1
  check "model: + zooms the column" (pvZoom (pv' z1) > 1 && "Width 125%" `T.isInfixOf` T.pack (pageStatusFor z1 (pv' z1)))
  check "model: zooming drops the bands of the old scale" (M.null (M.intersection tiles1 (pvTiles (pv' z1))))
  check "model: zoomed past the view, Right pans" (pvPanX (pv' (key (KArrow DRight noMods) z1)) > pvPanX (pv' z1))
  check "model: 0 returns to the width" (pvZoom (pv' (key (KChar '0') z1)) == 1)
  -- toggling layout keeps the page
  let f1 = key (KChar 'w') s4
  check "model: W back to fit page keeps the page" (not (pageStripOn f1) && pvPage (pv' f1) == 2 && pageViewOn f1)
  check "model: ... with its scene from the pool" (isNothing (pdfPageWant f1))
  let f2 = key (KChar 'w') f1
  check "model: W again returns to the strip on that page" (pageStripOn f2 && pvAnchor (pv' f2) == 2)
  -- a scrollbar drag lands proportionally
  let lo = computeLayout s1
      barCol = loCols lo - 1
      b1 = mouse MBLeft True False (loTextTop lo + loTextHeight lo - 1) barCol s1
  check "model: dragging the scroll bar to the bottom reaches the end" (pvPage (pv' b1) == 5)
  -- the cell picture on a terminal without pixels
  let c1 = settle (openStrip False)
  check "cells: the strip draws without pixels" (isJust (stripCellGrid c1))
  check "cells: its bands are whole pages"
    (let ks = M.keys (pvTiles (pv' c1)) in not (null ks) && all ((== 0) . tkY) ks && length ks == length (IM.fromList [ (tkPage k, ()) | k <- ks ]))
  check "cells: there is a gap row of background between pages"
    (case stripCellGrid (key (KPageDown noMods) c1) of
       Just g -> any (\r -> all (\c -> g `at` (r, c) == blankCell) [0 .. 9]) [0 .. 30]
       Nothing -> False)
  where at g i = g A.! i

selectTests :: (String -> Bool -> IO ()) -> IO ()
selectTests check = do
  let s1 = settle (openStrip True)
      s2 = key (KChar 's') s1
  check "select: S switches the drag to selecting" (pvDrag (pv' s2) == PdfDragSelect)
  check "select: ... and the status says so" ("Select" `T.isInfixOf` T.pack (pageStatusFor s2 (pv' s2)))
  -- the title line "This is page 1" sits near the top of the first page:
  -- 72 pt down at 800/612 px per pt ≈ 94 px ≈ row 5 of 16 px cells
  let lo = computeLayout s2
      r0 = loTextTop lo + 3; r1 = loTextTop lo + 7
      c0 = loTextLeft lo + 5; c1 = loTextLeft lo + 60
      d1 = mouse MBLeft True False r0 c0 s2
      d2 = mouse MBLeft True True r1 c1 d1
  check "select: a drag shows its box" (isJust (pvBox (pv' d2)))
  check "select: ... and selects live" (maybe False (("This is page 1" `T.isInfixOf`) . bsText) (pvSel (pv' d2)))
  check "select: ... drawing its rubber band" (any ((> 0) . hrOutline) (stripHighlights d2))
  let d3 = mouse MBLeft False False r1 c1 d2
  check "select: release keeps the selection" (isNothing (pvBox (pv' d3)) && isJust (pvSel (pv' d3)))
  check "select: ... highlighted line by line" (any ((== 0) . hrOutline) (stripHighlights d3))
  let (d4, effs) = update (KCtrlChar 'c') d3
  check "select: Ctrl+C copies the text"
    (any (\e -> case e of EffCopy t -> "This is page 1" `T.isInfixOf` t; _ -> False) effs)
  check "select: ... and says how much" ("Copied" `T.isInfixOf` edStatus d4)
  let d5 = key KEsc d3
  check "select: Esc drops the selection" (isNothing (pvSel (pv' d5)))
  let a1 = key (KCtrlChar 'a') s1
  check "select: Ctrl+A takes the page's text"
    (maybe False (\bs -> "Body line 39 of page 1" `T.isInfixOf` bsText bs) (pvSel (pv' a1)))
  -- zoom by box in the strip
  let z1 = mouse MBLeft True False r0 c0 s1
      z2 = mouse MBLeft True True r1 c1 z1
      z3 = mouse MBLeft False False r1 c1 z2
  check "zoom box: a drag in zoom mode zooms into it" (pvZoom (pv' z3) > 1.5)
  check "zoom box: a click goes back to the width" (pvZoom (pv' (mouse MBLeft False False 10 10 (mouse MBLeft True False 10 10 z3))) == 1)
  -- the fit-page layout selects too
  let f0 = key (KChar 'w') s2
      f1 = mouse MBLeft True False (loTextTop lo + 1) (loTextLeft lo + 1) f0
      f2 = mouse MBLeft True True (loTextTop lo + 30) (loTextLeft lo + 99) f1
      f3 = mouse MBLeft False False (loTextTop lo + 30) (loTextLeft lo + 99) f2
  check "select: in fit page a box selects the page's text"
    (maybe False (("Body line" `T.isInfixOf`) . bsText) (pvSel (pv' f3)))
  -- a zoom drag over a picture keeps its pixels up (the band is drawn over them)
  let pz = key (KChar 's') f0                       -- back to zoom mode, fit page
      pz1 = mouse MBLeft True False 5 5 pz
      pz2 = mouse MBLeft True True 15 30 pz1
  check "zoom drag: the pixel overlay stays wanted mid-drag"
    (maybe False (isJust . idDrag) (activePicture pz2) && imageOverlayWanted pz2)

pointerTests :: (String -> Bool -> IO ()) -> IO ()
pointerTests check = do
  let s1 = settle (openStrip True)
      lo = computeLayout s1
      r = loTextTop lo + 10; c = loTextLeft lo + 20
      hover m e = pointerShapeAt (MouseEvent MBNone c r True True m 1) e
      ctrl = noMods { modCtrl = True }
  check "pointer: zoom drag is a crosshair in fit width" (hover noMods s1 == "crosshair")
  check "pointer: the select tool is an I-beam" (hover noMods (key (KChar 's') s1) == "text")
  let f1 = key (KChar 'w') s1
  check "pointer: ... and the same in fit page" (hover noMods f1 == "crosshair" && hover noMods (key (KChar 's') f1) == "text")
  check "pointer: Ctrl over the fit-width column is a hand" (hover ctrl s1 == "grab")
  check "pointer: Ctrl over an unzoomed fitted page is not (nothing to pan)" (hover ctrl f1 == "crosshair")
  -- Ctrl+drag pans the column: the grabbed point follows the pointer
  let ev b p d rr cc = KMouse (MouseEvent b cc rr p d ctrl 1)
      p1 = key (ev MBLeft True False r c) s1
      p2 = key (ev MBLeft True True (r - 3) c) p1
      p3 = key (ev MBLeft False False (r - 3) c) p2
  check "pan: Ctrl+press starts a pan" (isJust (edPanDrag p1) && hover noMods p1 == "grabbing")
  check "pan: dragging up three rows scrolls down 48 px"
    (pvAnchor (pv' p2) == 1 && abs (pvOff (pv' p2) * 800 - 48) < 0.5)
  check "pan: no selection or zoom box was made" (isNothing (pvBox (pv' p2)) && isNothing (pvSel (pv' p2)) && pvZoom (pv' p2) == 1)
  check "pan: release ends it" (isNothing (edPanDrag p3) && not (edMouseSelecting p3))
  -- the middle button pans a zoomed fitted page
  let z1 = key (KChar '+') f1
      crop0 = idCrop =<< activePicture z1
      m1 = key (KMouse (MouseEvent MBMiddle c r True False noMods 1)) z1
      m2 = key (KMouse (MouseEvent MBMiddle c (r - 5) True True noMods 1)) m1
  -- (zoomed 125% on a portrait page in a wide window, only up and down can move)
  check "pan: middle-drag moves a zoomed page's crop"
    (isJust crop0 && fmap (\(_, y, _, _) -> y) (idCrop =<< activePicture m2) > fmap (\(_, y, _, _) -> y) crop0)
  check "pan: a motion with no button ends a pan whose release was missed"
    (isNothing (edPanDrag (key (KMouse (MouseEvent MBNone c r True True noMods 1)) m2)))

cropShapeTests :: (String -> Bool -> IO ()) -> IO ()
cropShapeTests check = do
  -- fit page (zoom mode), a tall narrow box in a wide window
  let f0 = key (KChar 'w') (settle (openStrip True))
      lo = computeLayout f0
      t = loTextTop lo; l = loTextLeft lo
      d1 = mouse MBLeft True False (t + 5) (l + 48) f0
      d2 = mouse MBLeft True True (t + 30) (l + 52) d1
      d3 = mouse MBLeft False False (t + 30) (l + 52) d2
      Just (cx, cy, cw, ch) = idCrop =<< activePicture d3
      Just pic = activePicture d3
      (iw, ih) = imageDims pic
      boxC = cellRectToCrop d2 pic (t + 5 - t, l + 48 - l, t + 30 - t, l + 52 - l)
      viewRatio = fromIntegral (loTextWidth lo) / (2 * fromIntegral (loTextHeight lo) * 1.0) :: Double
      ratio = fromIntegral cw / fromIntegral ch :: Double
  check "zoom box: a tall box is widened to the view's shape (or to the page's width)"
    (abs (ratio - viewRatio) / viewRatio < 0.05 || cw == iw)
  check "zoom box: ... keeping the box inside it"
    (let (bx, by, bw, bh) = boxC in cx <= bx && cx + cw >= bx + bw && cy <= by + 1 && cy + ch >= by + bh - 1)
  check "zoom box: ... and the picture" (cx >= 0 && cy >= 0 && cx + cw <= iw && cy + ch <= ih)

configTests :: (String -> Bool -> IO ()) -> IO ()
configTests check = do
  check "pdf-fit = width parses" (cfgPdfFit (fst (parseConfigText "pdf-fit = width\n" defaultConfig)) == PdfFitWidth)
  check "pdf-drag = select parses" (cfgPdfDrag (fst (parseConfigText "pdf-drag = select\n" defaultConfig)) == PdfDragSelect)
  check "pdf-fit rejects other values" (length (snd (parseConfigText "pdf-fit = both\n" defaultConfig)) == 1)
  check "pdf-fit and pdf-drag default to page and zoom"
    (cfgPdfFit defaultConfig == PdfFitPage && cfgPdfDrag defaultConfig == PdfDragZoom)
  check "pdf-fit round-trips through the writer"
    (cfgPdfFit (fst (parseConfigText (updateConfigText (defaultConfig { cfgPdfFit = PdfFitWidth }) "") defaultConfig)) == PdfFitWidth)
  check "Settings rows 16 and 17 set the layout and the drag"
    (cfgPdfFit (edConfig (applySettingRow 16 1 ed0)) == PdfFitWidth
     && cfgPdfDrag (edConfig (applySettingRow 17 1 ed0)) == PdfDragSelect)
  where ed0 = newEditor (40, 100) defaultConfig
