{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the SVG picture view (plan 0034, workstream F): the pure
-- model's half of opening an SVG as a picture derived from its text buffer,
-- the Alt+T round trip, re-derivation after edits, the zoom geometry in the
-- virtual pixel grid, the render-request keys, and the font provider's
-- pure parts. The driver's jobs are exercised by docs/plans/bench/pty_svg.py.
--
-- Spec.hs calls 'svgViewSpec' with its own @check@.
module SvgViewSpec (svgViewSpec) where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Array (elems)
import GHC.Clock (getMonotonicTime)

import Cmedit.Types
import Cmedit.TextBuffer
import Cmedit.Editor
import Cmedit.ConfigFile (parseConfigText, updateConfigText, configKeysHelp)
import Cmedit.EditorState (ImageDoc(..), Effect(..), journalableDoc, isPlainDoc, computeLayout, Layout(..))
import Cmedit.Geom
import Cmedit.Image (Image(..))
import Cmedit.Scene (Scene(..), Color(..))
import Cmedit.Svg
import Cmedit.Raster (RenderStats(..))
import Cmedit.VecView
import Cmedit.FontProvider
import Cmedit.FontSys (FontRequest(..))
import Cmedit.Menu (Menu(..), MenuEntry(..), MenuAction(..), menuBar)
import Cmedit.Gfx (GfxKind(..), gfxFitVector, gfxFit, maxGfxPixels, maxSixelVectorPixels)
import Cmedit.Jobs (newJobTable, startJobIn, cancelJobIn, runningJob)
import Control.Concurrent (threadDelay, myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, tryTakeMVar, takeMVar)
import Control.Exception (evaluate, try, throwIO, SomeException, AsyncException(..), fromException, finally)
import GHC.Conc (threadStatus, ThreadStatus(..))
import System.Timeout (timeout)

svgViewSpec :: (String -> Bool -> IO ()) -> IO ()
svgViewSpec check = do
  openTests check
  toggleTests check
  editTests check
  configTests check
  zoomTests check
  keyTests check
  providerTests check
  safetyTests check
  reviewTests check

------------------------------------------------------------------------------
-- Fixtures

circleSvg :: T.Text
circleSvg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"48\" height=\"48\" viewBox=\"0 0 48 48\">\n<circle cx=\"24\" cy=\"24\" r=\"20\" fill=\"#ff0000\"/>\n</svg>\n"

squareSvg :: T.Text
squareSvg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"48\" height=\"48\"><rect width=\"48\" height=\"48\" fill=\"#0000ff\"/></svg>\n"

mkLR :: T.Text -> LoadResult
mkLR t = LoadResult (fromText t) LF Utf8 True False Nothing

ed0 :: Editor
ed0 = newEditor (24, 80) defaultConfig

openSvg :: T.Text -> Editor
openSvg t = setLoaded "/tmp/pic.svg" (mkLR t) ed0

-- Play the driver: parse what the model asks for, as a parse job would.
driveParse :: Int -> Editor -> Editor
driveParse sid ed = case svgParseWant ed of
  Nothing -> ed
  Just w ->
    let txt = bufferToText LF False (edBuffer ed)
        res = fmap (\sc -> (sc, sceneSize sc, scNotes sc)) (parseSvgText defaultSvgEnv txt)
    in svgParsed w sid False res ed

key :: Key -> Editor -> Editor
key k e = fst (update k e)

isPicture :: Editor -> Bool
isPicture = isJust . activeVec

-- Cells of the picture's grid that are not blank.
inkCells :: Editor -> Int
inkCells ed = case edImage ed >>= idCache of
  Just (_, _, _, _, _, _, g) -> length [ () | c <- elems g, cellChar c /= ' ' ]
  Nothing -> 0

------------------------------------------------------------------------------

openTests :: (String -> Bool -> IO ()) -> IO ()
openTests check = do
  let e1 = openSvg circleSvg
  check "svg: opening an .svg enters the picture view" (isPicture e1)
  check "svg: the buffer under the picture is the document"
    (bufferToText LF False (edBuffer e1) == T.stripEnd circleSvg && not (edReadOnly e1))
  check "svg: a fresh picture asks the driver for a parse"
    (fmap (\w -> (swPath w, swSeq w)) (svgParseWant e1) == Just ("/tmp/pic.svg", edDocSeq e1))
  let e2 = driveParse 7 e1
  check "svg: the parse installs the scene" (maybe False (isJust . vdScene) (activeVec e2))
  check "svg: once parsed nothing more is asked for" (isNothing (svgParseWant e2))
  check "svg: the scene id is the driver's" (fmap vdSceneId (activeVec e2) == Just 7)
  check "svg: the size label is the intrinsic size" (fmap svgSizeLabel (activeVec e2) == Just "48\xd7\&48")
  check "svg: the cell picture is drawn (cheap scene, synchronous)" (inkCells e2 > 20)
  check "svg: a cheap scene leaves no background cell raster to do" (isNothing (vecCellWant e2))
  check "svg: the status says SVG" ("SVG 48" `T.isPrefixOf` edStatus e2)
  -- A file with the extension but no <svg> root stays in its source.
  let e3 = setLoaded "/tmp/x.svg" (mkLR "<html><body><svg/></body></html>") ed0
  check "svg: an .svg that is not SVG opens as source" (not (isPicture e3) && isNothing (edImage e3))
  check "svg: ... and says why" ("does not look like SVG" `T.isInfixOf` edStatus e3)
  -- An XML file mentioning <svg is never a picture.
  let e4 = setLoaded "/tmp/doc.xml" (mkLR circleSvg) ed0
  check "svg: an .xml file with an <svg> root is not a picture" (not (isPicture e4))
  -- A search-result jump into an SVG lands in the source.
  let jumpEd = ed0 { edPendingJump = Just ("/tmp/pic.svg", 1, 0, 6) }
      e5 = applyPendingJump (setLoaded "/tmp/pic.svg" (mkLR circleSvg) jumpEd)
  check "svg: opening to land on a match shows the source" (not (isPicture e5))
  -- A parse failure falls back to the source view with the reason.
  let w1 = fromMaybe (SvgWant "" 0 0) (svgParseWant e1)
      e6 = svgParsed w1 9 False (Left "boom") e1
  check "svg: a parse failure shows the source" (not (isPicture e6) && isJust (edSvgStash e6))
  check "svg: ... with an error note" ("Cannot show this SVG: boom" `T.isPrefixOf` edStatus e6)
  let e7 = key (KAltChar 't') e6
  check "svg: Alt+T after an unchanged failure stays in source" (not (isPicture e7))
  -- A stale parse (the buffer moved on) is dropped.
  let e8 = svgParsed w1 { swSeq = edDocSeq e1 + 5 } 3 False (Left "late") e1
  check "svg: a parse for another version is ignored" (isPicture e8 && edStatus e8 == edStatus e1)
  -- Second file on the command line opens as a picture too.
  let e9 = addDocument "/tmp/two.svg" (mkLR squareSvg) e2
      two = last (edAfter e9)
  check "svg: a background-opened SVG is a picture document" (maybe False (isJust . idVec) (docImage two))

toggleTests :: (String -> Bool -> IO ()) -> IO ()
toggleTests check = do
  let e1 = driveParse 1 (openSvg circleSvg)
      e2 = key (KAltChar 't') e1
  check "svg: Alt+T shows the source" (not (isPicture e2) && isJust (edSvgStash e2))
  check "svg: ... which is editable text" (isNothing (edImage e2) && not (edReadOnly e2))
  let e3 = key (KAltChar 't') e2
  check "svg: Alt+T again shows the picture" (isPicture e3)
  check "svg: an unedited round trip needs no re-parse" (isNothing (svgParseWant e3))
  check "svg: ... and keeps the scene" (fmap vdSceneId (activeVec e3) == Just 1)
  -- The View menu offers the toggle only for SVG files.
  check "svg: View menu has the SVG toggle for an .svg"
    (any isSvgItem (entriesFor e1 viewMenuIx))
  check "svg: ... and not for a .txt"
    (not (any isSvgItem (entriesFor (setLoaded "/tmp/a.txt" (mkLR "x") ed0) viewMenuIx)))
  -- Switching documents carries the picture through the zipper.
  let e4 = setLoadedNew "/tmp/b.txt" (mkLR "hello") e1
      e5 = switchToFile 0 e4
  check "svg: the picture survives a document switch" (isPicture e5 && isNothing (svgParseWant e5))
  where
    isSvgItem e = case e of
      MEItem _ _ MAToggleSvg -> True
      _ -> False

vs :: Int
vs = vecVirtualSide

viewMenuIx :: Int
viewMenuIx = head [ i | (i, Menu t _) <- zip [0 ..] menuBar, "View" `T.isInfixOf` t ]

editTests :: (String -> Bool -> IO ()) -> IO ()
editTests check = do
  let e1 = driveParse 1 (openSvg circleSvg)
      -- Edit the source: replace the colour.
      e2 = key (KAltChar 't') e1
      e3 = e2 { edBuffer = fromText (T.replace "#ff0000" "#00ff00" circleSvg) }
      e3' = (key (KChar ' ') e3)   -- a real edit bumps the document's counter
      e3'' = key KBackspace e3'
      e4 = key (KAltChar 't') e3''
  check "svg: an edited source asks for a re-parse on the way back"
    (fmap swSeq (svgParseWant e4) == Just (edDocSeq e4))
  check "svg: ... while the old picture stays on screen" (inkCells e4 > 20)
  let e5 = driveParse 2 e4
  check "svg: the re-parse installs a new scene" (fmap vdSceneId (activeVec e5) == Just 2)
  -- Save from the picture view writes the buffer (the path is the file).
  let (_, effs) = update (KCtrlChar 's') e5
  check "svg: Ctrl+S in the picture view saves the buffer"
    (any (\f -> case f of EffSaveTo p -> p == "/tmp/pic.svg"; _ -> False) effs)
  check "svg: a modified SVG in picture view is journalled"
    (journalableDoc (captureDoc e5) && isPlainDoc (captureDoc e5))
  check "svg: Save As is not refused in the picture view" (isNothing (saveAsRefusal e5))
  -- Typing and pasting in the picture view never edit the invisible buffer.
  let before = bufferToText LF False (edBuffer e5)
      e6 = key (KChar 'x') e5
      (e7, effs7) = update (KCtrlChar 'v') e6
      e8 = key (KCtrlChar 'x') (key (KCtrlChar 'a') e7)
  check "svg: typing in the picture view does not edit"
    (bufferToText LF False (edBuffer e8) == before)
  check "svg: Ctrl+V in the picture view does not paste" (null [ () | EffPaste <- effs7 ])
  -- Undo in the picture view acts on the source and re-derives the picture.
  let e9 = key (KCtrlChar 'z') e5
  check "svg: undo in the picture view moves the buffer and asks for a parse"
    (edDocSeq e9 /= edDocSeq e5 && isJust (svgParseWant e9))

configTests :: (String -> Bool -> IO ()) -> IO ()
configTests check = do
  let (c1, w1) = parseConfigText "svg-view = source\n" defaultConfig
  check "svg-view = source parses" (not (cfgSvgPicture c1) && null w1)
  let (c2, _) = parseConfigText "svg-view = picture\n" c1
  check "svg-view = picture parses" (cfgSvgPicture c2)
  let (_, w3) = parseConfigText "svg-view = maybe\n" defaultConfig
  check "svg-view rejects other values" (length w3 == 1)
  check "svg-view defaults to picture" (cfgSvgPicture defaultConfig)
  let txt = updateConfigText (defaultConfig { cfgSvgPicture = False }) ""
  check "svg-view round-trips through the writer"
    (not (cfgSvgPicture (fst (parseConfigText txt defaultConfig))))
  check "svg-view is in the --help key list" (any ("svg-view" `isInfixOfS`) configKeysHelp)
  let edSrc = newEditor (24, 80) defaultConfig { cfgSvgPicture = False }
      e1 = setLoaded "/tmp/pic.svg" (mkLR circleSvg) edSrc
  check "svg-view = source opens the markup" (not (isPicture e1))
  check "svg: Alt+T still shows the picture with svg-view = source" (isPicture (key (KAltChar 't') e1))
  check "svg: the Settings row toggles svg-view"
    (not (cfgSvgPicture (edConfig (applySettingRow 14 0 ed0)))
     && cfgSvgPicture (edConfig (applySettingRow 14 1 ed0)))
  where isInfixOfS a b = T.pack a `T.isInfixOf` T.pack b

zoomTests :: (String -> Bool -> IO ()) -> IO ()
zoomTests check = do
  let e1 = driveParse 1 (openSvg circleSvg)
      idoc = fromMaybe (mkVectorDoc 0) (edImage e1)
  check "svg: the virtual grid's long side is 2^24" (imageDims idoc == (vs, vs))
  check "svg: vecDims keeps the aspect" (vecDims (200, 100) == (vs, vs `div` 2))
  check "svg: a 1000:1 picture keeps a short side of thousands of units"
    (snd (vecDims (1000, 1)) >= 16000)
  check "svg: no native-size cap for vectors" (isNothing (imageFitCap e1 { edCellPx = Just (8, 16) } idoc))
  let lo = computeLayout e1
      midR = loTextHeight lo `div` 2; midC = loTextWidth lo `div` 2
      e2 = wheelZoom True midR midC e1
      crop2 = edImage e2 >>= idCrop
  check "svg: a wheel notch zooms to a crop inside the virtual grid"
    (case crop2 of
       Just (x, y, w, h) -> x >= 0 && y >= 0 && w > 0 && h > 0 && x + w <= vs && y + h <= vs && w * h < vs * vs
       Nothing -> False)
  check "svg: zooming re-rasterises the cells for the new crop"
    (fmap (vkCrop . fst) (activeVec e2 >>= vdCells) == crop2)
  -- Deep zoom is allowed and capped.
  let deep = iterate (wheelZoom True midR midC) e1 !! 60
      (_, _, dw, _) = fromMaybe (0, 0, vs, vs) (edImage deep >>= idCrop)
  check "svg: deep zoom goes far past a bitmap's limit" (dw < vs `div` 100)
  let deeper = wheelZoom True midR midC deep
      (_, _, dw', _) = fromMaybe (0, 0, vs, vs) (edImage deeper >>= idCrop)
  check "svg: ... and stops at the cap" (abs (dw' - dw) * 50 <= dw)
  check "svg: ... with the window still tens of thousands of virtual units" (dw >= 30000)
  -- Zooming back out returns to the fit.
  let back = iterate (wheelZoom False midR midC) e2 !! 3
  check "svg: zooming out returns to the fit" (fmap idCrop (edImage back) == Just Nothing)
  -- The scene → device matrix maps the crop onto the raster exactly.
  let b = Rect 0 0 48 48
      m = vecView b (vs, vs) (vs `div` 4, vs `div` 4, vs `div` 2, vs `div` 2) 100 50
      Pt x0 y0 = applyA m (Pt 12 12)
      Pt x1 y1 = applyA m (Pt 36 36)
  check "svg: vecView maps the crop's corners to the raster's"
    (abs x0 < 1e-6 && abs y0 < 1e-6 && abs (x1 - 100) < 1e-6 && abs (y1 - 50) < 1e-6)
  -- Drag-to-zoom maps cells into the virtual grid.
  let (cx, cy, cw, ch) = cellRectToCrop e1 idoc (2, 10, 8, 30)
  check "svg: drag-to-zoom gives a virtual crop" (cw > 0 && ch > 0 && cx >= 0 && cy >= 0 && cx + cw <= vs)

keyTests :: (String -> Bool -> IO ()) -> IO ()
keyTests check = do
  let e1 = driveParse 4 (openSvg circleSvg)
      k1 = fromMaybe (VecKey 0 (0, 0, 0, 0) 0 0) (vecCellKey e1)
  check "svg: the cell key names the scene and the fitted size"
    (vkScene k1 == 4 && vkW k1 > 0 && vkH k1 > 0 && vkW k1 <= loTextWidth (computeLayout e1))
  -- An expensive scene is not rasterised synchronously: the driver is asked.
  let big = T.concat ("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
                      : [ T.pack ("<path d=\"M0 0 L" ++ show i ++ " 100 L100 " ++ show (i `mod` 97) ++ " Z\"/>") | i <- [1 .. 9000 :: Int] ]
                      ++ ["</svg>"])
      eb = driveParse 5 (openSvg big)
      vd = activeVec eb
  check "svg: an expensive scene's cost is over the sync threshold"
    (maybe False ((> vecSyncCostMax) . vdCost) vd)
  check "svg: ... so its cells are a background request" (fmap (vkScene . fst) (vecCellWant eb) == Just 5)
  -- A raster for the scene showing is installed; one for another scene is not.
  case vecCellWant eb of
    Nothing -> check "svg: the expensive scene has a cell request" False
    Just (kb, sc) -> case rasterVec sc (vkCrop kb) (vkW kb) (vkH kb) of
      Nothing -> check "svg: the cell request can be rasterised" False
      Just (img, st) -> bgTests check eb kb sc img st
  -- Timing: a cheap scene's synchronous cell raster stays cheap.
  t0 <- getMonotonicTime
  let ez2 = iterate (\e -> wheelZoom True 5 5 e) (driveParse 1 (openSvg circleSvg)) !! 8
  _ <- pure $! inkCells ez2
  t1 <- getMonotonicTime
  check "svg: eight synchronous zoom steps of an icon take under 2 s (-O0)" (t1 - t0 < 2)

bgTests :: (String -> Bool -> IO ()) -> Editor -> VecKey -> Scene -> Image -> RenderStats -> IO ()
bgTests check eb kb sc img st = do
  let eb2 = vecCellsRendered kb img (rsExhausted st) [] eb
  check "svg: the background raster fills the picture" (inkCells eb2 > 0 && isNothing (vecCellWant eb2))
  let stale = kb { vkScene = 99 }
      eb3 = vecCellsRendered stale img False [] eb
  check "svg: a raster of a superseded scene is dropped" (inkCells eb3 == 0)
  check "svg: a raster of the wrong size is dropped"
    (inkCells (vecCellsRendered kb { vkW = vkW kb + 1 } img False [] eb) == 0)
  -- Zooming changes the key, so the old raster no longer satisfies it.
  let lo = computeLayout eb2
      ez = wheelZoom True (loTextHeight lo `div` 2) (loTextWidth lo `div` 2) eb2
  check "svg: a zoom supersedes the cell request with a new crop"
    (case vecCellWant ez of Just (k', _) -> vkCrop k' /= vkCrop kb; Nothing -> False)
  check "svg: ... while the previous grid stays on screen" (inkCells ez > 0)
  -- rasterVec refuses an oversized request rather than allocating it.
  check "svg: rasterVec caps the output size" (isNothing (rasterVec sc (0, 0, 10, 10) 5000 5000))
  check "svg: rasterVec draws exactly the requested size"
    (fmap (\(i, _) -> (imgW i, imgH i)) (rasterVec sc (0, 0, vs, vs) 37 19) == Just (37, 19))

providerTests :: (String -> Bool -> IO ()) -> IO ()
providerTests check = do
  let doc = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\"><text font-family=\"Foo Sans, monospace\" font-weight=\"bold\">Hi<tspan font-style=\"italic\">!</tspan></text><image href=\"pic.png\" width=\"1\" height=\"1\"/><image href=\"data:image/png;base64,AA==\"/></svg>"
      qs = svgFontQueries defaultSvgEnv doc
  check "fonts: the pre-pass lists the text's font query first"
    (case qs of (q : _) -> fqFamilies q == ["Foo Sans", "monospace"] && fqWeight q == 700; [] -> False)
  check "fonts: ... each family alone, and the generics"
    (all (\f -> any ((== [f]) . fqFamilies) qs) ["monospace", "sans-serif", "serif"])
  check "fonts: ... and the tspan's italic style" (any ((== FontItalic) . fqStyle) qs)
  check "fonts: a document without text needs no fonts" (null (svgFontQueries defaultSvgEnv circleSvg))
  check "fonts: external refs exclude data: URIs" (svgExternalRefs doc == ["pic.png"])
  let r = queryRequest (FontQuery ["Foo Sans", "serif"] 700 FontItalic)
  check "fonts: a query becomes a bold italic request for its first family"
    (frFamily r == "Foo Sans" && frBold r && frItalic r && not (frMono r))
  check "fonts: monospace sets the fixed-pitch flag" (frMono (queryRequest (FontQuery ["monospace"] 400 FontNormal)))
  -- The cache's bounds: count, then LRU order.
  check "fonts: the cache starts empty" (M.null (fcEntries emptyFontCache))
  check "fonts: no fonts and no provider means text is omitted with a note"
    (any ("no font" `isIn`) (either (const []) scNotes (parseSvgText defaultSvgEnv doc)))
  where isIn a b = T.pack a `T.isInfixOf` T.pack b

safetyTests :: (String -> Bool -> IO ()) -> IO ()
safetyTests check = do
  let e1 = openSvg circleSvg
  -- Before any parse, the picture is blank, not a crash.
  check "svg: drawing before the first parse is safe" (inkCells e1 == 0)
  -- A garbage SVG body (root is svg, content nonsense) still parses or fails cleanly.
  let junk = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"0\" height=\"0\"><path d=\"M Z Q\"/></svg>"
      ej = driveParse 3 (openSvg junk)
  check "svg: a zero-size SVG falls back to the source" (not (isPicture ej))
  -- the job's measure agrees with the plain one on a scene with no sharing
  case parseSvgText defaultSvgEnv circleSvg of
    Right sc -> do
      sz <- measureScene sc
      check "svg: measureScene agrees with sceneSize on an unshared scene" (sz == sceneSize sc)
    Left _ -> check "svg: measureScene agrees with sceneSize on an unshared scene" False
  check "svg: sceneCost is bounded and positive"
    (either (const False) ((> 0) . sceneCost) (parseSvgText defaultSvgEnv circleSvg))
  -- The explorer marks SVG files as images.
  check "svg: isSvgPath knows .svg and .svgz" (isSvgPath "a/B.SVG" && isSvgPath "x.svgz" && not (isSvgPath "x.xml"))
  -- placeholder bytes never sampled
  check "svg: the placeholder is 1x1" (imgW placeholderImage == 1 && imgH placeholderImage == 1)

------------------------------------------------------------------------------
-- The milestone review's driver fixes

-- A busy pure loop that allocates (as the rasteriser does) and never ends.
spin :: Int -> Int
spin n = if n < 0 then n else spin (length (show n) + n `mod` 7919 + 1)

reviewTests :: (String -> Bool -> IO ()) -> IO ()
reviewTests check = do
  -- 1. A superseded job dies promptly, though it was started inside mask_.
  tbl <- newJobTable
  started <- newEmptyMVar
  died <- newEmptyMVar
  _ <- startJobIn tbl (0 :: Int) ((putMVar started () >> evaluate (spin 1) >> pure ()) `finally` putMVar died ())
  _ <- timeout 2000000 (takeMVar started)
  threadDelay 20000
  t0 <- getMonotonicTime
  killed <- startJobIn tbl 0 (pure ())
  t1 <- getMonotonicTime
  gone <- timeout 1000000 (takeMVar died)
  check "jobs: starting a job kills the previous one" (killed && isJust gone)
  check (("jobs: the kill of a busy pure job returns in under 50 ms (took " ++ show (round ((t1 - t0) * 1000) :: Int)) ++ " ms)")
        (t1 - t0 < 0.05)
  -- cancel, and the table entry clears itself when a job ends
  done <- newEmptyMVar
  _ <- startJobIn tbl 1 (putMVar done ())
  _ <- takeMVar done
  threadDelay 20000
  r1 <- runningJob tbl 1
  check "jobs: a finished job leaves no table entry" (not r1)
  died2 <- newEmptyMVar
  _ <- startJobIn tbl 2 ((evaluate (spin 1) >> pure ()) `finally` putMVar died2 ())
  threadDelay 20000
  c <- cancelJobIn tbl 2
  g2 <- timeout 1000000 (takeMVar died2)
  r2 <- runningJob tbl 2
  check "jobs: cancelling kills the job and clears the entry" (c && isJust g2 && not r2)

  -- 2. trySync lets a kill through (so fontFor cannot blacklist a font for it).
  r <- try (trySync (throwIO ThreadKilled)) :: IO (Either AsyncException (Either SomeException ()))
  check "fonts: trySync rethrows asynchronous exceptions" (either (== ThreadKilled) (const False) r)
  r' <- trySync (throwIO (userError "bad font")) :: IO (Either SomeException ())
  check "fonts: trySync catches ordinary failures" (either (const True) (const False) r')

  -- 3. A parse begun before a Revert cannot land afterwards.
  let e1 = openSvg circleSvg
      w0 = fromMaybe (SvgWant "" (-1) (-1)) (svgParseWant e1)
      e2 = revertLoaded "/tmp/pic.svg" (mkLR squareSvg) e1
      w1 = fromMaybe (SvgWant "" (-2) (-2)) (svgParseWant e2)
  check "svg: a revert gives the picture a new epoch" (swEpoch w1 /= swEpoch w0 && swSeq w1 == swSeq w0)
  let stale = fmap (\sc -> (sc, sceneSize sc, [])) (parseSvgText defaultSvgEnv circleSvg)
      e3 = svgParsed w0 50 False stale e2
  check "svg: the pre-revert parse is not installed" (maybe True (isNothing . vdScene) (activeVec e3))
  check "svg: ... and the reverted picture still wants its own" (svgParseWant e3 == Just w1)
  -- re-opening a closed SVG likewise
  let e4 = setLoaded "/tmp/pic.svg" (mkLR circleSvg) (driveParse 1 e1)
  check "svg: re-opening gives a new epoch" (fmap swEpoch (svgParseWant e4) /= Just (swEpoch w0))

  -- 4. Deep zoom is vector-sharp: the payload is the display's, not the crop's.
  let (_, _, cols, rows, pw, ph) = gfxFitVector GfxKitty (8, 16) (1, 0, 200, 50) (256, 128)
  check ("gfx: a deep-zoom kitty payload fills the box (" ++ show (pw, ph) ++ " for " ++ show (cols * 8, rows * 16) ++ ")")
    (pw * ph <= maxGfxPixels && pw * ph * 100 >= maxGfxPixels * 95 && pw > 256)
  let (_, _, cs, rs, sw, sh) = gfxFitVector GfxSixel (8, 16) (1, 0, 200, 50) (256, 128)
  check ("gfx: a deep-zoom sixel payload is the full box, drawn 1:1 (" ++ show (sw, sh) ++ ")")
    (sw == cs * 8 && sh == rs * 16 && sw >= 1500)
  let (_, _, _, _, bw, _) = gfxFit (8, 16) (1, 0, 200, 50) (256, 128) True
  check "gfx: the bitmap fit still caps at the source" (bw <= 256)
  let (_, _, cs2, rs2, sw2, sh2) = gfxFitVector GfxSixel (8, 16) (1, 0, 400, 120) (4096, 4096)
  check "gfx: a huge sixel box shrinks the box with the payload"
    (sw2 == cs2 * 8 && sh2 == rs2 * 16 && sw2 * sh2 <= maxSixelVectorPixels)

  -- 5. Save As to another name leaves the picture; Alt+T always gets out.
  let p1 = driveParse 1 (openSvg circleSvg)
      (p2, _) = saveAsTo "/tmp/notes.txt" p1
  check "svg: Save As to a .txt leaves the picture view" (not (isPicture p2))
  let renamed = p1 { edPath = Just "/tmp/data.csv" }
      p3 = key (KAltChar 't') renamed
  check "svg: Alt+T leaves a picture whose file is no longer an .svg" (not (isPicture p3) && isNothing (edCsv p3))

  -- 6. Only a few inactive SVGs keep their scenes.
  check "lru: keeps the most recent within the count" (keepScenes 2 100 [(1, 1), (5, 1), (3, 1), (4, 1)] == [False, True, False, True])
  check "lru: keeps within the cost budget" (keepScenes 3 10 [(9, 4), (8, 5), (7, 6)] == [True, True, False])
  check "lru: the newest is kept whatever it costs" (keepScenes 3 10 [(9, 50), (8, 5), (7, 6)] == [True, False, False])
  let opened = foldl (\e i -> driveParse i (setLoadedNew ("/tmp/p" ++ show i ++ ".svg") (mkLR circleSvg) e)) ed0 [1 .. 5 :: Int]
      cycled = foldl (\e i -> switchToFile i e) opened [0 .. 4]
      scenes = length [ () | d <- edBefore cycled ++ edAfter cycled, Just im <- [docImage d], Just vd <- [idVec im], isJust (vdScene vd) ]
  check ("lru: at most two inactive SVG documents keep a scene (" ++ show scenes ++ ")") (scenes <= vecKeepScenes)
  let back = switchToFile 0 cycled
  check "lru: a trimmed document re-parses when shown" (isJust (svgParseWant back) || maybe False (isJust . vdScene) (activeVec back))

  -- 7. The cells stay painted until the vector placement exists.
  let k1 = (driveParse 1 (openSvg circleSvg)) { edGfxCaps = True, edGfxKitty = True }
  check "overlay: wanted for a vector picture on a pixel terminal" (imageOverlayWanted k1)
  check "overlay: not active (cells painted) until the placement is ready" (not (imageOverlayActive k1))
  check "overlay: active once ready" (imageOverlayActive k1 { edVecGfxReady = True })

  -- 9. Drawn before font discovery finished: redrawn when fonts arrive.
  let f0 = openSvg circleSvg
      fw = fromMaybe (SvgWant "" (-1) (-1)) (svgParseWant f0)
      f1 = svgParsed fw 3 True (fmap (\sc -> (sc, sceneSize sc, [])) (parseSvgText defaultSvgEnv circleSvg)) f0
  check "fonts: a fontless parse is installed but marked" (maybe False vdFontless (activeVec f1) && isNothing (svgParseWant f1))
  let f2 = fontsReady f1
  check "fonts: fonts arriving re-requests the parse with a new epoch"
    (case svgParseWant f2 of Just w' -> swEpoch w' /= swEpoch fw; Nothing -> False)
  let f3 = svgParsed fw 4 True (fmap (\sc -> (sc, sceneSize sc, [])) (parseSvgText defaultSvgEnv circleSvg)) f0 { edFontsReady = True }
  check "fonts: a fontless parse landing after fonts arrived is redone at once" (isJust (svgParseWant f3))

  -- 10. A failed cell raster is not re-requested every frame.
  let bigE = driveParse 5 (openSvg bigSvg)
  case vecCellWant bigE of
    Just (kb, _) -> check "cells: a failed raster is not asked for again" (isNothing (vecCellWant (vecCellsFailed kb bigE)))
    Nothing -> check "cells: a failed raster is not asked for again" False

  -- 11. Dashes count in the cost (so a dash-heavy stroke goes to the job),
  -- and the synchronous raster has a small budget besides: a scene that
  -- looks cheap but covers the canvas thousands of times is shown truncated
  -- and redone by the job.
  let dashed = T.concat ("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
                         : [ T.pack ("<path d=\"M0 " ++ show i ++ " L100 " ++ show i ++ "\" stroke=\"black\" stroke-width=\"50\" stroke-dasharray=\"0.0011 0.0011\"/>") | i <- [1 .. 40 :: Int] ]
                         ++ ["</svg>"])
      ds = driveParse 6 (openSvg dashed)
  check "cost: dash expansion counts, so a dash-heavy scene is a background job"
    (maybe False ((> vecSyncCostMax) . vdCost) (activeVec ds) && isJust (vecCellWant ds))
  let layered = T.concat ("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
                          : replicate 2500 "<rect width=\"100\" height=\"100\" fill=\"#336699\" fill-opacity=\"0.5\"/>"
                          ++ ["</svg>"])
      ls = driveParse 7 (openSvg layered)
  check ("sync: a cheap-looking but heavy scene is drawn truncated and redone by the job "
         ++ show (fmap (\v -> (vdCost v, vdCellsPartial v)) (activeVec ls)))
    (maybe False ((<= vecSyncCostMax) . vdCost) (activeVec ls)
     && maybe False vdCellsPartial (activeVec ls) && isJust (vecCellWant ls))

  -- 19. The source view never queues a parse.
  let src = key (KAltChar 't') (driveParse 1 (openSvg circleSvg))
      edited = key KBackspace (key (KChar ' ') src)
  check "svg: typing in the source asks for no parse" (isNothing (svgParseWant edited))

bigSvg :: T.Text
bigSvg = T.concat ("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100\" height=\"100\">"
                   : [ T.pack ("<path d=\"M0 0 L" ++ show i ++ " 100 L100 " ++ show (i `mod` 97) ++ " Z\"/>") | i <- [1 .. 9000 :: Int] ]
                   ++ ["</svg>"])
