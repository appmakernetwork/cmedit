{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the PDF page view (plan 0034, workstream F2): the pure model's
-- half of the page view as a mode of the PDF view. The driver's jobs are
-- exercised by docs/plans/bench/pty_pdf.py.
--
-- Spec.hs calls 'pdfViewSpec' with its own @check@.
module PdfViewSpec (pdfViewSpec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T
import System.Timeout (timeout)

import Cmedit.Types
import Cmedit.TextBuffer
import Cmedit.Editor
import Cmedit.ConfigFile (parseConfigText, updateConfigText, configKeysHelp)
import Cmedit.EditorState (ImageDoc(..), Effect(..), computeLayout, Layout(..), scrollBarInfo)
import Cmedit.Geom
import Cmedit.Pdf (PdfDoc(..), parsePdf, pdfCurrentPage, pdfGoToPage, pdfPageSource)
import Cmedit.PdfObj (fuelDefault)
import qualified Cmedit.PdfRender as PR
import Cmedit.Scene (Scene(..), Cmd(..), FillRule(..), Paint(..), black, defaultImageOpts)
import Cmedit.Image (Image(..))
import Data.Array.Unboxed (listArray)
import Cmedit.VecView
import Cmedit.Raster (RenderStats(..))
import Cmedit.Gfx (base64Strict)
import Cmedit.Dialog (Dialog(..), DialogKind(..))
import Cmedit.Menu (Menu(..), MenuEntry(..), MenuAction(..), menuBar)

pdfViewSpec :: (String -> Bool -> IO ()) -> IO ()
pdfViewSpec check = do
  modeTests check
  navTests check
  identityTests check
  actionTests check
  zoomTests check
  reviewTests check
  sharingTests check

------------------------------------------------------------------------------
-- Fixtures

-- | A minimal multi-page PDF, one line of text per page plus enough body
-- lines for the reflow view to scroll page by page (the same file
-- pty_pdf.py writes).
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

doc5, doc20 :: PdfDoc
doc5 = either error id (parsePdf (pdfBytes 5))
doc20 = either error id (parsePdf (pdfBytes 20))

ed0 :: Editor
ed0 = newEditor (40, 100) defaultConfig

openPdf :: Config -> Bool -> Editor
openPdf cfg gfx =
  let e = (newEditor (40, 100) cfg) { edGfxCaps = gfx, edGfxKitty = gfx }
  in pdfLoaded "/tmp/doc.pdf" doc5 e

key :: Key -> Editor -> Editor
key k e = fst (update k e)

curPage :: Editor -> Int
curPage = maybe 0 pvPage . edPdfView

-- Play the driver: render the wanted page with the stub and install it.
drive :: Int -> Editor -> Editor
drive sid ed = case pdfPageWant ed of
  Nothing -> ed
  Just w -> case edPdf ed >>= \pd -> pdfPageSource pd (pwPage w) of
    Nothing -> ed
    Just (f, d) ->
      let (sc, _) = PR.renderPage (PR.RenderEnv (const Nothing) True) f fuelDefault d
      in pdfPageParsed w sid False (okScene sc) ed

-- A parse result as the job posts it, measured (here, sharing-blind).
okScene :: Scene -> Either String (Scene, SceneSize, [String])
okScene sc = Right (sc, sceneSize sc, [])

pageScene :: Int -> Scene
pageScene n = case pdfPageSource doc5 n of
  Just (f, d) -> fst (PR.renderPage (PR.RenderEnv (const Nothing) True) f fuelDefault d)
  Nothing -> error "no page"

------------------------------------------------------------------------------

modeTests :: (String -> Bool -> IO ()) -> IO ()
modeTests check = do
  check "pdf: the fixture parses into five pages" (pdfSourcePages doc5 == 5)
  let a0 = openPdf defaultConfig False
  check "pdf-view auto, no pixels: opens in reflow" (not (pageViewOn a0))
  check "pdf-view auto, no pixels: waits for caps" (maybe False pvAutoPending (edPdfView a0))
  check "pdf-view auto with pixels: opens in the page view" (pageViewOn (openPdf defaultConfig True))
  check "pdf-view = page opens pages without pixels" (pageViewOn (openPdf defaultConfig { cfgPdfView = PdfViewPage } False))
  check "pdf-view = reflow opens reflow with pixels" (not (pageViewOn (openPdf defaultConfig { cfgPdfView = PdfViewReflow } True)))
  -- Caps arriving after the open switch auto to pages, once.
  let a1 = pdfCapsArrived (setGfxCaps True False False a0)
  check "caps arriving switch auto to the page view" (pageViewOn a1)
  check "... once: the pending flag is spent" (not (maybe True pvAutoPending (edPdfView a1)))
  let a2 = key (KAltChar 't') a1                     -- the user goes back to reflow
      a3 = pdfCapsArrived (setGfxCaps True False False a2)
  check "a second caps reply never switches again" (not (pageViewOn a3))
  let m1 = key (KAltChar 't') a0                     -- manual page view before caps
      m2 = key (KAltChar 't') m1                     -- manual reflow
      m3 = pdfCapsArrived (setGfxCaps True False False m2)
  check "Alt+T toggles reflow and page" (pageViewOn m1 && not (pageViewOn m2))
  check "a manual reflow is not overridden by caps" (not (pageViewOn m3))
  check "caps without pixels change nothing" (not (pageViewOn (pdfCapsArrived (setGfxCaps False False False a0))))
  -- inactive documents too
  let z0 = setLoadedNew "/tmp/a.txt" (LoadResult (fromText "x") LF Utf8 True False Nothing) a0
      z1 = pdfCapsArrived (setGfxCaps True False False z0)
      zd = [ d | d <- edBefore z1 ++ edAfter z1, isJust (docPdf d) ]
  check "caps arriving switch an inactive auto PDF too" (any (maybe False pvOn . docPdfView) zd)
  -- config key
  let (c1, w1) = parseConfigText "pdf-view = reflow\n" defaultConfig
  check "pdf-view = reflow parses" (cfgPdfView c1 == PdfViewReflow && null w1)
  check "pdf-view = page parses" (cfgPdfView (fst (parseConfigText "pdf-view = page\n" defaultConfig)) == PdfViewPage)
  check "pdf-view rejects other values" (length (snd (parseConfigText "pdf-view = both\n" defaultConfig)) == 1)
  check "pdf-view defaults to auto" (cfgPdfView defaultConfig == PdfViewAuto)
  check "pdf-view round-trips through the writer"
    (cfgPdfView (fst (parseConfigText (updateConfigText defaultConfig { cfgPdfView = PdfViewPage } "") defaultConfig)) == PdfViewPage)
  check "pdf-view is in the --help key list" (any (T.isInfixOf "pdf-view" . T.pack) configKeysHelp)
  check "the Settings row sets pdf-view"
    (cfgPdfView (edConfig (applySettingRow 15 2 ed0)) == PdfViewReflow
     && cfgPdfView (edConfig (applySettingRow 15 1 ed0)) == PdfViewPage
     && cfgPdfView (edConfig (applySettingRow 15 0 ed0)) == PdfViewAuto)
  -- the mode is per document, through the zipper
  let p1 = openPdf defaultConfig True
      p2 = setLoadedNew "/tmp/a.txt" (LoadResult (fromText "x") LF Utf8 True False Nothing) p1
      p3 = switchToFile 0 p2
  check "the page view survives a document switch" (pageViewOn p3)
  check "an inactive PDF keeps no page scenes"
    (and [ maybe True (isNothing . vdScene) (pvPic pv `seq` idVec (pvPic pv)) && null (pvScenes pv)
         | d <- edBefore p2 ++ edAfter p2, Just pv <- [docPdfView d] ])
  check "a document switched back to re-asks for its page" (isJust (pdfPageWant p3))

navTests :: (String -> Bool -> IO ()) -> IO ()
navTests check = do
  let p = drive 1 (openPdf defaultConfig True)
  check "page view starts on page 1" (curPage p == 1)
  check "PgDn turns the page" (curPage (key (KPageDown noMods) p) == 2)
  check "PgUp at the first page stays" (curPage (key (KPageUp noMods) p) == 1)
  check "End goes to the last page" (curPage (key (KEnd noMods) p) == 5)
  check "PgDn at the last page stays" (curPage (key (KPageDown noMods) (key (KEnd noMods) p)) == 5)
  check "Home goes back to the first" (curPage (key (KHome noMods) (key (KEnd noMods) p)) == 1)
  check "Space and ] turn forward, [ back"
    (curPage (key (KChar ' ') p) == 2 && curPage (key (KChar ']') p) == 2
     && curPage (key (KChar '[') (key (KChar ']') p)) == 1)
  check "at the fit the arrows turn pages" (curPage (key (KArrow DRight noMods) p) == 2 && curPage (key (KArrow DDown noMods) p) == 2)
  check "Go To page 4" (curPage (gotoLine "4" p) == 4)
  check "Go To page clamps" (curPage (gotoLine "99" p) == 5)
  -- mode switches keep the page
  -- (twenty pages: a short document's reflow view cannot scroll its last
  -- pages to the top, so it is a different test)
  let r0 = pdfLoaded "/tmp/doc.pdf" doc20 ((newEditor (40, 100) defaultConfig))
      r1 = r0 { edPdf = fmap (pdfGoToPage 30 3) (edPdf r0) }
      r2 = key (KAltChar 't') r1
  check ("reflow -> page lands on the reflow view's page (" ++ show (fmap pdfCurrentPage (edPdf r1)) ++ ")")
    (curPage r2 == maybe 0 pdfCurrentPage (edPdf r1) && curPage r2 > 1)
  let r3 = key (KPageDown noMods) r2
      r4 = key (KAltChar 't') r3
  check ("page -> reflow lands on that page " ++ show (fmap pdfCurrentPage (edPdf r4), curPage r3, fmap pdTop (edPdf r4)))
    (fmap pdfCurrentPage (edPdf r4) == Just (curPage r3))
  -- the scrollbar measures pages
  let lo = computeLayout p
      h = loTextHeight lo
  check "the page view's scrollbar measures pages"
    (fmap (\(_, _, hh, total, win) -> (hh, total, win)) (scrollBarInfo (key (KPageDown noMods) p)) == Just (h, 5 * h, h))
  let barCol = loCols lo - 1
      clickRow = loTextTop lo + (h * 3) `div` 5 + 1
      clicked = key (KMouse (MouseEvent MBLeft barCol clickRow True False noMods 1)) p
  check ("a click on the scrollbar turns to that page (" ++ show (curPage clicked) ++ ")") (curPage clicked == 4)

identityTests :: (String -> Bool -> IO ()) -> IO ()
identityTests check = do
  let p0 = openPdf defaultConfig True
  case pdfPageWant p0 of
    Nothing -> check "the page view asks for its first page" False
    Just w1 -> do
      check "the page view asks for its first page" (pwPage w1 == 1 && pwPath w1 == "/tmp/doc.pdf")
      let p1 = drive 7 p0
      check "the stub scene is installed" (maybe False ((== 7) . vdSceneId) (activePicVec p1))
      check "the scene is the page's size" (maybe False ((== (612, 792)) . vdSize) (activePicVec p1))
      check "no more is asked once it is in" (isNothing (pdfPageWant p1))
      check "the stub draws paper white first" (length (scCmds (pageScene 1)) >= 1 && scBounds (pageScene 1) == Rect 0 0 612 792)
      -- stale results
      let stale = pdfPageParsed w1 { pwEpoch = pwEpoch w1 + 99 } 8 False (okScene (pageScene 1)) p0
      check "a result for another opening is dropped" (isNothing (activePicVec stale >>= vdScene))
      let otherDoc = pdfPageParsed w1 { pwPath = "/tmp/other.pdf" } 8 False (okScene (pageScene 1)) p0
      check "a result for another document is dropped" (isNothing (activePicVec otherDoc >>= vdScene))
      let far = pdfPageParsed w1 { pwPage = 4 } 9 False (okScene (pageScene 4)) p1
      check "a result for a far page is dropped" (maybe True null (fmap pvScenes (edPdfView far)))
      let p2 = pdfLoaded "/tmp/doc.pdf" doc5 p1
      check "re-opening gives a new epoch" (fmap pwEpoch (pdfPageWant p2) /= Just (pwEpoch w1))
      -- prefetch: next page first, then the previous
      check "prefetch asks for the next page once the current is settled"
        (fmap pwPage (pdfPrefetchWant p1) == Just 2)
      check "no prefetch before the current page is in" (isNothing (pdfPrefetchWant p0))
      case pdfPrefetchWant p1 of
        Nothing -> check "prefetch asks for the next page once the current is settled" False
        Just pw2 -> do
          let p3 = pdfPageParsed pw2 10 False (okScene (pageScene 2)) p1
          check "a prefetched neighbour goes into the cache" (fmap (map fst . pvScenes) (edPdfView p3) == Just [2])
          check "with the next page cached, page 1 has nothing more to prefetch" (isNothing (pdfPrefetchWant p3))
          let p4 = key (KPageDown noMods) p3
          check "turning to a prefetched page installs it at once" (isNothing (pdfPageWant p4) && maybe False ((== 10) . vdSceneId) (activePicVec p4))
          check "the page left behind is cached" (fmap (map fst . pvScenes) (edPdfView p4) == Just [1])
          check "from page 2 the prefetch is page 3" (fmap pwPage (pdfPrefetchWant p4) == Just 3)
          -- the cache holds the current page +-1 only
          let pool = [ (k, PageScene k (pageScene 1) [] False (sceneSize (pageScene 1))) | k <- [1, 2, 4, 5] ]
          check "the page cache keeps only the neighbours" (map fst (trimPageCache 3 pool) == [2, 4])
          let p5 = pdfTurnTo 5 p4
          check "a long jump leaves no stale neighbours" (maybe True (all ((>= 4) . fst) . pvScenes) (edPdfView p5))

actionTests :: (String -> Bool -> IO ()) -> IO ()
actionTests check = do
  let p = drive 1 (openPdf defaultConfig True)
      (pc, effs) = update (KCtrlChar 'c') p
  check "Ctrl+C in the page view copies nothing" (null [ () | EffCopy _ <- effs ])
  check "... and says where copying works" ("reflow" `T.isInfixOf` edStatus pc)
  let pf = key (KCtrlChar 'f') (key (KPageDown noMods) p)
  check "Ctrl+F in the page view goes to reflow and opens Find"
    (not (pageViewOn pf) && fmap dlgKind (edDialog pf) == Just DKFind)
  check "... on the same page" (fmap pdfCurrentPage (edPdf pf) == Just 2)
  let pt = key (KChar 'x') p
  check "typing in the page view edits nothing" (pageViewOn pt && curPage pt == 1)
  check "View menu offers the page view for a PDF"
    (any (\e -> case e of MEItem _ _ MATogglePdfPage -> True; _ -> False) (entriesFor p viewMenuIx))
  check "... relabelled while the pages show"
    (any (\e -> case e of MEItem l _ MATogglePdfPage -> "Reflow" `T.isInfixOf` l; _ -> False) (entriesFor p viewMenuIx))
  check "... and not for a text file"
    (not (any (\e -> case e of MEItem _ _ MATogglePdfPage -> True; _ -> False) (entriesFor ed0 viewMenuIx)))
  check "the page status shows page and zoom" ("Page 1/5 \xb7 100%" `T.isPrefixOf` T.pack (maybe "" pageStatus (edPdfView p)))
  check "the page view has no cursor" True

viewMenuIx :: Int
viewMenuIx = head [ i | (i, Menu t _) <- zip [0 ..] menuBar, "View" `T.isInfixOf` t ]

zoomTests :: (String -> Bool -> IO ()) -> IO ()
zoomTests check = do
  let p = drive 1 (openPdf defaultConfig True)
      lo = computeLayout p
      midR = loTextTop lo + loTextHeight lo `div` 2
      midC = loTextLeft lo + loTextWidth lo `div` 2
      wheel b mods = KMouse (MouseEvent b midC midR True False mods 1)
      ctrl = ctrlOnly
      z1 = key (wheel MBWheelUp ctrl) p
      crop = activePicture z1 >>= idCrop
  check "Ctrl+wheel zooms into a crop" (isJust crop && curPage z1 == 1)
  check "the plain wheel at the fit turns the page" (curPage (key (wheel MBWheelDown noMods) p) == 2)
  check "zoomed, the plain wheel zooms instead of turning" (curPage (key (wheel MBWheelDown noMods) z1) == 1)
  check "zoomed, the status shows the zoom" (not ("100%" `T.isInfixOf` T.pack (maybe "" pageStatus (edPdfView z1))))
  let z2 = key (KArrow DDown noMods) (key (KChar '+') (key (KChar '+') p))
      c1 = activePicture (key (KChar '+') (key (KChar '+') p)) >>= idCrop
      c2 = activePicture z2 >>= idCrop
  check "+ zooms and, zoomed, an arrow pans instead of turning"
    (isJust c1 && c1 /= c2 && curPage z2 == 1)
  let (vw, vh) = maybe (0, 0) imageDims (activePicture p)
      panned = iterate (key (KArrow DRight noMods)) (key (KChar '+') p) !! 40
  check "panning is clamped to the page"
    (case activePicture panned >>= idCrop of
       Just (x, y, w, h) -> x >= 0 && y >= 0 && x + w <= vw && y + h <= vh
       Nothing -> False)
  check "0 resets the zoom" (isNothing (activePicture (key (KChar '0') z1) >>= idCrop))
  check "Esc resets the zoom" (isNothing (activePicture (key KEsc z1) >>= idCrop))
  check "a page turn returns to the fit" (isNothing (activePicture (key (KPageDown noMods) z1) >>= idCrop))
  check "the page picture's cell grid is drawn"
    (case activePicture p >>= idCache of
       Just (_, _, _, _, _, _, g) -> length (filter ((/= ' ') . cellChar) (foldr (:) [] g)) > 50
       Nothing -> False)

------------------------------------------------------------------------------
-- The milestone review's fixes

-- A scene whose cost is over half the page cache's budget: two such
-- neighbours cannot both be kept.
-- (a page of photographs: an image whose decoded size alone is over half
-- the neighbour budget; the pixel array is a stand-in, since 'sceneBytes'
-- goes by the image's dimensions and nothing here rasterises it)
heavyScene :: Scene
heavyScene = Scene (Rect 0 0 612 792)
  [CImage identityA (Image side side "test" (listArray (0, 3) [0, 0, 0, 0])) defaultImageOpts] []
  where side = 1 + floor (sqrt (fromIntegral pageKeepBytes / 8 + 1 :: Double))

-- An 8-page document with one short line a page: its reflow view fits the
-- window, so the window can never scroll a later page to the top.
shortDoc :: PdfDoc
shortDoc = either error id (parsePdf (shortBytes 8))
  where
    shortBytes n =
      let objs = [ "<< /Type /Catalog /Pages 2 0 R >>"
                 , "<< /Type /Pages /Kids [" ++ unwords [ show (4 + 2 * k) ++ " 0 R" | k <- [0 .. n - 1] ] ++ "] /Count " ++ show n ++ " >>"
                 , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>" ]
                 ++ concat [ [ "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 3 0 R >> >> /Contents " ++ show (5 + 2 * k) ++ " 0 R >>"
                             , let c = "BT /F1 12 Tf 72 700 Td (Page " ++ show (k + 1) ++ ") Tj ET" in "<< /Length " ++ show (length c) ++ " >>\nstream\n" ++ c ++ "\nendstream" ]
                           | k <- [0 .. n - 1] ]
          body = concat [ show k ++ " 0 obj\n" ++ o ++ "\nendobj\n" | (k, o) <- zip [1 :: Int ..] objs ]
      in BC.pack ("%PDF-1.4\n" ++ body ++ "trailer\n<< /Size " ++ show (length objs + 1) ++ " /Root 1 0 R >>\n%%EOF\n")

-- Pages whose first has no text at all (a cover image), so the first two
-- start on the same laid-out line of the reflow view.
coverBytes :: Int -> BC.ByteString
coverBytes n =
  let objs = [ "<< /Type /Catalog /Pages 2 0 R >>"
             , "<< /Type /Pages /Kids [" ++ unwords [ show (4 + 2 * k) ++ " 0 R" | k <- [0 .. n - 1] ] ++ "] /Count " ++ show n ++ " >>"
             , "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>" ]
             ++ concat [ [ "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 3 0 R >> >> /Contents " ++ show (5 + 2 * k) ++ " 0 R >>"
                         , let c = if k == 0 then "0 0 1 rg 100 100 400 400 re f" else "BT /F1 12 Tf 72 700 Td (Page " ++ show (k + 1) ++ ") Tj ET"
                           in "<< /Length " ++ show (length c) ++ " >>\nstream\n" ++ c ++ "\nendstream" ]
                       | k <- [0 .. n - 1] ]
      body = concat [ show k ++ " 0 obj\n" ++ o ++ "\nendobj\n" | (k, o) <- zip [1 :: Int ..] objs ]
  in BC.pack ("%PDF-1.4\n" ++ body ++ "trailer\n<< /Size " ++ show (length objs + 1) ++ " /Root 1 0 R >>\n%%EOF\n")

-- Answer prefetch wants with @answer@ until there are none (or 10 rounds),
-- returning the pages asked for.
prefetchRounds :: (Int -> Either String (Scene, SceneSize, [String])) -> Editor -> ([Int], Editor)
prefetchRounds answer = go (10 :: Int) []
  where
    go 0 acc e = (reverse acc, e)
    go k acc e = case pdfPrefetchWant e of
      Nothing -> (reverse acc, e)
      Just w -> go (k - 1) (pwPage w : acc) (pdfPageParsed w (100 + pwPage w) False (answer (pwPage w)) e)

reviewTests :: (String -> Bool -> IO ()) -> IO ()
reviewTests check = do
  let p3 = drive 1 (pdfTurnTo 3 (drive 1 (openPdf defaultConfig True)))
  check "review: on page 3 with its scene in" (curPage p3 == 3 && isNothing (pdfPageWant p3))
  -- 1. Two neighbours too heavy to keep together converge.
  let (asked, settled) = prefetchRounds (const (okScene heavyScene)) p3
  check ("review: heavy neighbours converge after at most two prefetches " ++ show asked)
    (length asked <= 2 && isNothing (pdfPrefetchWant settled))
  check "review: the next page is the one kept" (fmap (map fst . pvScenes) (edPdfView settled) == Just [4])
  -- a neighbour bigger than the whole budget is not kept at all
  let huge = Scene (Rect 0 0 612 792) [CImage identityA (Image 20000 20000 "test" (listArray (0, 3) [0, 0, 0, 0])) defaultImageOpts] []
      (askedH, settledH) = prefetchRounds (const (okScene huge)) p3
  check ("review: an over-budget neighbour is not cached and not asked for again " ++ show askedH)
    (askedH == [4, 2] && fmap (null . pvScenes) (edPdfView settledH) == Just True && isNothing (pdfPrefetchWant settledH))
  check "pages: sceneBytes counts decoded images" (sceneBytes huge >= 4 * 20000 * 20000)
  -- a cover page with no text: the page view opens on page 1, not 2
  let coverPdf = either error id (parsePdf (coverBytes 3))
      cv = pdfLoaded "/tmp/cover.pdf" coverPdf ((newEditor (40, 100) defaultConfig { cfgPdfView = PdfViewPage }) { edGfxCaps = True })
  check ("review: a textless first page still opens on page 1 (" ++ show (curPage cv) ++ ")") (curPage cv == 1)
  let cv2 = pdfCapsArrived (setGfxCaps True False False (pdfLoaded "/tmp/cover.pdf" coverPdf ed0))
  check "review: ... and so does the auto switch" (pageViewOn cv2 && curPage cv2 == 1)
  -- light neighbours are both kept
  let (askedL, settledL) = prefetchRounds (\n -> okScene (pageScene n)) p3
  check ("review: light neighbours: next first, then previous " ++ show askedL) (askedL == [4, 2])
  check "review: ... and both are kept" (fmap (length . pvScenes) (edPdfView settledL) == Just 2)
  -- a turn lets a page that did not fit be tried again
  let turned = pdfTurnTo 4 settled
  check "review: a turn forgets what did not fit" (maybe False (null . pvNoFit) (edPdfView turned))
  -- 2. A failed prefetch is not retried, and shows when turned to.
  let (askedF, settledF) = prefetchRounds (\n -> if n == 4 then Left "broken page" else okScene (pageScene n)) p3
  check ("review: a failed prefetch is asked for once " ++ show askedF) (askedF == [4, 2] && isNothing (pdfPrefetchWant settledF))
  let toFailed = key (KPageDown noMods) settledF
  check "review: turning to a failed page shows its error"
    (curPage toFailed == 4 && maybe False ((== Just "broken page") . vdError) (activePicVec toFailed)
     && "broken page" `T.isInfixOf` edStatus toFailed)
  check "review: ... and does not parse it again" (isNothing (pdfPageWant toFailed))
  let curFail = pdfPageParsed (PageWant "/tmp/doc.pdf" (maybe 0 pvEpoch (edPdfView p3)) 3) 9 False (Left "bad") (pdfTurnTo 3 (openPdf defaultConfig True))
  check "review: a failed current page is quiet" (isNothing (pdfPageWant curFail) && isNothing (pdfPrefetchWant curFail))
  -- 3. The previous placement survives only a genuinely pending raster.
  check "placement: kept while the new one is pending" (keepPreviousPlacement True False Nothing)
  check "placement: dropped when the raster was refused" (not (keepPreviousPlacement True False (Just Nothing)))
  check "placement: dropped when the picture failed" (not (keepPreviousPlacement True True Nothing))
  check "placement: dropped at another place" (not (keepPreviousPlacement False False Nothing))
  -- 4. Page -> reflow -> page keeps the page, on every page of a short document.
  let s0 = pdfLoaded "/tmp/short.pdf" shortDoc ((newEditor (40, 100) defaultConfig { cfgPdfView = PdfViewPage }) { edGfxCaps = True })
      roundTrip k = curPage (key (KAltChar 't') (key (KAltChar 't') (pdfTurnTo k s0)))
  check ("review: a short document's round trip keeps every page " ++ show (map roundTrip [1 .. 8]))
    (map roundTrip [1 .. 8] == [1 .. 8])
  let viaFind = key KEsc (key (KCtrlChar 'f') (pdfTurnTo 6 s0))
  check "review: Ctrl+F then back keeps the page" (curPage (key (KAltChar 't') viaFind) == 6)
  -- 5. Fontless pages once fonts are ready.
  let f0 = (openPdf defaultConfig True) { edFontsReady = True }
      fw = fromMaybe (PageWant "" 0 0) (pdfPageWant f0)
      f1 = pdfPageParsed fw 3 True (okScene (pageScene 1)) f0
  check "review: a fontless current page after fonts are ready is shown and redone"
    (maybe False (isJust . vdScene) (activePicVec f1) && isJust (pdfPageWant f1))
  let g0 = drive 1 (openPdf defaultConfig True) { edFontsReady = True }
      gw = fromMaybe (PageWant "" 0 0) (pdfPrefetchWant g0)
      g1 = pdfPageParsed gw 4 True (okScene (pageScene 2)) g0
  check "review: a fontless neighbour is not cached once fonts are ready" (maybe False (null . pvScenes) (edPdfView g1))
  -- 6. The next page's overlay key comes from the cache, without a turn.
  let n0 = snd (prefetchRounds (\n -> okScene (pageScene n)) (drive 1 (openPdf defaultConfig True)))
  check "review: the next page's fit comes from its cached scene"
    (case pdfNextPageFit n0 of
       Just (sid, _, (0, 0, w, h)) -> sid == 102 && (w, h) == vecDims (612, 792)
       _ -> False)
  check "review: no next-page fit while zoomed" (isNothing (pdfNextPageFit (key (KChar '+') n0)))
  -- the in-place base64 encoder matches the definition at every length mod 3
  let ref bs = let tab = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
                   go (a : b : c : r) = let n = a * 65536 + b * 256 + c in map ((tab !!) . (`mod` 64) . (n `div`)) [262144, 4096, 64, 1] ++ go r
                   go [a, b] = let n = a * 65536 + b * 256 in map ((tab !!) . (`mod` 64) . (n `div`)) [262144, 4096, 64] ++ "="
                   go [a] = let n = a * 65536 in map ((tab !!) . (`mod` 64) . (n `div`)) [262144, 4096] ++ "=="
                   go [] = ""
               in go (map fromIntegral (BS.unpack bs) :: [Int])
      samples = [ BS.pack [ fromIntegral ((i * 37 + k * 11) `mod` 256) | i <- [0 .. k - 1] ] | k <- [0 .. 40] ]
  check "gfx: base64Strict matches the definition for lengths 0..40"
    (all (\b -> BC.unpack (base64Strict b) == ref b) samples)
  -- the rasteriser's notes reach the status line, its budget note in our words, once
  let st = RenderStats True False 0 0 0 0 0 1 ["1 image not drawn (over the memory cap)", "picture incomplete (work budget reached)"]
  check "notes: rasterNoteList keeps one truncation note and the rest"
    (rasterNoteList st == [vecTruncatedText, "1 image not drawn (over the memory cap)"])
  case activePicVec n0 >>= vdCells of
    Just (kc, img) -> do
      let rn = vecCellsRendered kc img False ["1 image not drawn (over the memory cap)"] n0
      check "notes: a cell raster's notes go to the status line" ("image not drawn" `T.isInfixOf` edStatus rn)
      check "notes: ... and to the page status" ("image not drawn" `T.isInfixOf` T.pack (maybe "" pageStatus (edPdfView rn)))
    Nothing -> check "notes: a cell raster exists to test with" False

-- Release review B1: a scene that shares its sub-scenes through 'CTransform'
-- (nested Type 3 glyphs, forms) is a small heap and an astronomically large
-- tree. The job's measure walks each shared body once: it must finish at
-- once, report the drawing cost saturated, and count memory by the heap.
sharingTests :: (String -> Bool -> IO ()) -> IO ()
sharingTests check = do
  let leaf = [CFill identityA NonZero (PSolid black) [MoveTo (Pt 0 0), LineTo (Pt 1 0), LineTo (Pt 0 1), ClosePath]]
      -- 2^40 leaves when expanded
      bomb :: Int -> [Cmd]
      bomb 0 = leaf
      bomb k = let b = bomb (k - 1) in [CTransform identityA b, CTransform identityA b]
      sc = Scene (Rect 0 0 10 10) (bomb 40) []
  r <- timeout 5000000 (measureScene sc)
  check ("sharing: measureScene finishes on a 2^40-leaf shared scene " ++ show r)
    (maybe False (\sz -> szCost sz == sceneCostCap && szBytes sz < 100000) r)
  -- the expanded drawing cost is exact below the cap (memoised per body)
  let small = Scene (Rect 0 0 10 10) (bomb 6) []
  sm <- measureScene small
  check ("sharing: the memoised cost equals the plain cost " ++ show (sm, sceneSize small))
    (szCost sm == szCost (sceneSize small) && szBytes sm < szBytes (sceneSize small))
  -- an image drawn many times is held once
  let img = Image 100 100 "test" (listArray (0, 3) [0, 0, 0, 0])
      many = Scene (Rect 0 0 10 10) (replicate 50 (CImage identityA img defaultImageOpts)) []
  mi <- measureScene many
  check ("sharing: a shared image counts its pixels once " ++ show mi)
    (szBytes mi < 2 * 4 * 100 * 100 && szBytes (sceneSize many) >= 50 * 4 * 100 * 100)
  -- a cyclic command list (no front-end builds one, but nothing may hang)
  let cyc = let xs = CTransform identityA xs : [] in xs
  rc <- timeout 5000000 (measureScene (Scene (Rect 0 0 1 1) cyc []))
  check ("sharing: a cyclic scene is measured, not walked forever " ++ show rc) (isJust rc)

