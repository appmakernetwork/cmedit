-- Benchmarks for cmedit long-session behaviour. Built against src/ directly.
{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main (main) where

import Control.Monad (forM_, when)
import Data.List (foldl')
import Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Sequence as Seq
import GHC.Clock (getMonotonicTime)
import GHC.Stats
import System.Environment (getArgs)
import System.Mem (performMajorGC)

import Cmedit.Editor
import qualified Cmedit.Image as Img
import Cmedit.Editor
import qualified Cmedit.Image as ImgState
import Cmedit.TextBuffer
import Cmedit.Types
import Cmedit.Syntax
import Cmedit.Render
import Cmedit.ConfigFile (defaultConfig)
import qualified Data.ByteString as BS
import qualified Cmedit.Csv as Csv
import qualified Cmedit.Search as S
import qualified Cmedit.Browser as Br
import qualified Cmedit.Gfx as Gfx
import Cmedit.Input (ByteSource(..), nextKey)
import Data.IORef
import Data.Word (Word8)
import Cmedit.Caps (defaultCaps, renderCapsOf)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (toLazyByteString)

mkEd :: Int -> Int -> FilePath -> Editor
mkEd nlines width path =
  let txt = T.unlines [ T.pack ("def f" ++ show i ++ "(x): return x + " ++ show i
                                ++ "  # line " ++ show i ++ replicate width ' ')
                      | i <- [1 .. nlines] ]
      lr  = loadFromBytesText txt
  in setLoaded path lr (newEditor (40, 120) defaultConfig)
  where loadFromBytesText t = emptyLoadResult { lrBuffer = fromText t }

resid :: IO Double
resid = do
  performMajorGC
  s <- getRTSStats
  pure (fromIntegral (gcdetails_live_bytes (gc s)) / 1024 / 1024)

allocMB :: IO Double
allocMB = do
  s <- getRTSStats
  pure (fromIntegral (allocated_bytes s) / 1024 / 1024)

timed :: String -> IO a -> IO a
timed lbl act = do
  a0 <- allocMB
  t0 <- getMonotonicTime
  r <- act
  t1 <- getMonotonicTime
  a1 <- allocMB
  putStrLn (lbl ++ ": " ++ show (round ((t1 - t0) * 1000) :: Int) ++ " ms, "
            ++ show (round (a1 - a0) :: Int) ++ " MB allocated")
  pure r

-- 1. Undo retention: many distinct edit kinds so every edit pushes a snapshot.
benchUndo :: Int -> IO ()
benchUndo n = do
  let ed0 = mkEd 2000 0 "/tmp/bench.py"
  r0 <- resid
  let step !ed i =
        let k = if even i then KChar 'x' else KBackspace
        in fst (update k ed)
      edN = foldl' step ed0 [1 .. n]
  edN `seq` edBuffer edN `seq` pure ()
  r1 <- resid                                  -- edN still reachable below
  let !d = Seq.length (edUndo edN)             -- (now O(1): the bound is structural)
  r2 <- resid
  putStrLn ("undo: " ++ show n ++ " snapshot-pushing edits on a 2000-line file")
  putStrLn ("      live before: " ++ show (round r0 :: Int)
            ++ " MB   after (spine unforced): " ++ show (round r1 :: Int)
            ++ " MB   after forcing the spine: " ++ show (round r2 :: Int)
            ++ " MB   depth = " ++ show d ++ " (cap " ++ show (1000::Int) ++ ")")
  putStrLn ("      [keepalive " ++ show (lineCount (edBuffer edN)) ++ "]")

-- 2. lexWith cost vs line length (quadratic?)
benchLex :: IO ()
benchLex = forM_ [500, 1000, 2000, 4000, 8000] $ \len -> do
  let line = T.pack (concat (replicate (len `div` 10) "select a, "))
  timed ("lex python line of " ++ show (T.length line) ++ " chars")
        (let (ts, st) = lexLine Python StNormal line in length ts `seq` st `seq` pure ())

-- 3. syncCache cost per keystroke vs file size
benchSync :: IO ()
benchSync = forM_ [1000, 10000, 100000, 400000] $ \n -> do
  let ed0 = mkEd n 0 "/tmp/bench.py"
      ed1 = refreshHighlight ed0
  timed ("100 keystrokes on a " ++ show n ++ "-line file (edit+refreshHighlight)") $ do
    let step !ed _ = refreshHighlight (fst (update (KChar 'q') ed))
        edN = foldl' step ed1 [1 :: Int .. 100]
    edN `seq` edBuffer edN `seq` pure ()

-- 4. Full frame render cost
benchRender :: IO ()
benchRender = do
  let ed = refreshHighlight (mkEd 50000 0 "/tmp/bench.py")
  timed "200 renderEditor frames (40x120)" $ do
    forM_ [1 :: Int .. 200] $ \i -> do
      let e = ed { edTop = i }
          s = renderEditor (refreshHighlight e)
      scrW s `seq` scrCells s `seq` pure ()

-- 3b. update alone (no highlighting), and a plain-text (unhighlighted) doc
benchEdit :: IO ()
benchEdit = forM_ [1000, 10000, 100000, 400000] $ \n -> do
  let ed0 = mkEd n 0 "/tmp/bench.py"
      edT = mkEd n 0 "/tmp/bench.unknownext"
  timed ("100 keystrokes, update only, .py " ++ show n ++ " lines") $ do
    let step !ed _ = fst (update (KChar 'q') ed)
        edN = foldl' step ed0 [1 :: Int .. 100]
    edN `seq` edBuffer edN `seq` pure ()
  timed ("100 keystrokes, update only, no lexer " ++ show n ++ " lines") $ do
    let step !ed _ = fst (update (KChar 'q') ed)
        edN = foldl' step edT [1 :: Int .. 100]
    edN `seq` edBuffer edN `seq` pure ()
  -- cursor at the END of the file: the saved-buffer comparison walks further
  timed ("100 keystrokes at EOF, update only, " ++ show n ++ " lines") $ do
    let edE = ed0 { edCursor = Pos (n - 1) 0 }
        step !ed _ = fst (update (KChar 'q') ed)
        edN = foldl' step edE [1 :: Int .. 100]
    edN `seq` edBuffer edN `seq` pure ()

-- Force a buffer completely (every line to WHNF plus the char count), so
-- construction cost never leaks into a measurement.
forceEd :: Editor -> IO Editor
forceEd ed = do
  let b = edBuffer ed
      !n = sum [ T.length (getLine' i b) | i <- [0 .. lineCount b - 1] ]
  n `seq` pure ed

benchEdit2 :: IO ()
benchEdit2 = forM_ [1000, 10000, 100000, 400000] $ \n -> do
  ed0 <- forceEd (mkEd n 0 "/tmp/bench.py")
  edT <- forceEd (mkEd n 0 "/tmp/bench.unknownext")
  edE <- forceEd (ed0 { edCursor = Pos (n - 1) 0 })
  timed ("  " ++ show n ++ " lines: 1000 keystrokes, update only (.py)") $ do
    let step !ed _ = fst (update (KChar 'q') ed)
        edN = foldl' step ed0 [1 :: Int .. 1000]
    forceEd edN
  timed ("  " ++ show n ++ " lines: 1000 keystrokes, update only (no lexer)") $ do
    let step !ed _ = fst (update (KChar 'q') ed)
        edN = foldl' step edT [1 :: Int .. 1000]
    forceEd edN
  timed ("  " ++ show n ++ " lines: 1000 keystrokes at EOF") $ do
    let step !ed _ = fst (update (KChar 'q') ed)
        edN = foldl' step edE [1 :: Int .. 1000]
    forceEd edN
  timed ("  " ++ show n ++ " lines: 1000 keystrokes + refreshHighlight (.py)") $ do
    let step !ed _ = refreshHighlight (fst (update (KChar 'q') ed))
        edN = foldl' step ed0 [1 :: Int .. 1000]
    forceEd edN
  timed ("  " ++ show n ++ " lines: 1000 x (update + refreshHighlight + renderEditor)") $ do
    let step !ed _ = let e = refreshHighlight (fst (update (KChar 'q') ed))
                         s = renderEditor e
                     in scrW s `seq` scrCells s `seq` e
        edN = foldl' step ed0 [1 :: Int .. 1000]
    forceEd edN

-- Rendering a document whose lines are long (minified JS / one-line JSON /
-- wide SQL): how much does the per-line lexer cost per frame?
benchLongLines :: IO ()
benchLongLines = forM_ [200, 800, 2000, 5000, 12000] $ \len -> do
  let body = T.pack (take len (cycle "var alpha = beta + 12; foo(bar, baz); "))
      txt  = T.unlines (replicate 200 body)
      ed0  = setLoaded "/tmp/bench.js" (emptyLoadResult { lrBuffer = fromText txt })
                       (newEditor (40, 120) defaultConfig)
  ed <- forceEd ed0
  timed ("  40x120 frame over 200 lines of " ++ show len ++ " chars (20 frames)") $
    forM_ [1 :: Int .. 20] $ \i -> do
      let e = refreshHighlight (ed { edTop = i })
          s = renderEditor e
      scrW s `seq` scrCells s `seq` pure ()

benchLex2 :: IO ()
benchLex2 = do
  let contents = [ ("code", "var alpha = beta + 12; foo(bar, baz); ")
                 , ("commas", "select a, ")
                 , ("spaces", "          ")
                 , ("quotes", "x = 'abc' + \"def\"; ") ]
      langs = [("Python", Python), ("JS", JS), ("SQL", SQL), ("Haskell", Haskell), ("YAML", YAML)]
  forM_ langs $ \(ln, lang) -> forM_ contents $ \(cn, c) -> forM_ [2000, 8000] $ \len -> do
    let line = T.pack (take len (cycle c))
    timed ("  " ++ ln ++ "/" ++ cn ++ " " ++ show len ++ " chars")
          (let (ts, st) = lexLine lang StNormal line in length ts `seq` st `seq` pure ())

-- The REAL per-frame cost: renderEditor + renderFrame (the diff, which is what
-- forces the lazily-built cells) + serialising the Builder, as the driver does.
frameBytes :: Maybe Screen -> Screen -> Int
frameBytes prev scr =
  fromIntegral (BL.length (toLazyByteString (renderFrame (renderCapsOf defaultCaps) prev scr)))

benchFrame :: String -> Int -> Int -> IO ()
benchFrame path lineLen nlines = do
  let body = T.pack (take lineLen (cycle "var alpha = beta + 12; foo(bar, baz); "))
      txt  = T.unlines (replicate nlines body)
      ed0  = setLoaded path (emptyLoadResult { lrBuffer = fromText txt })
                       (newEditor (50, 200) defaultConfig)
  ed <- forceEd ed0
  -- Scrolling frames: each new top gives a fresh screen diffed against the last.
  timed ("  " ++ path ++ " " ++ show nlines ++ "x" ++ show lineLen
         ++ ": 100 full frames (render+diff+emit)") $ do
    let go !prev !acc i
          | i > (100 :: Int) = pure acc
          | otherwise = do
              let e = refreshHighlight (ed { edTop = i })
                  s = renderEditor e
                  !n = frameBytes prev s
              go (Just s) (acc + n) (i + 1)
    n <- go Nothing 0 1
    putStrLn ("    (" ++ show (n `div` 100) ++ " bytes/frame)")

-- Same, but threading the editor (and therefore the HlCache) across frames the
-- way App.renderNow does, and typing rather than scrolling: the steady-state
-- interactive cost.
benchType :: String -> Int -> Int -> IO ()
benchType path lineLen nlines = do
  let body = T.pack (take lineLen (cycle "var alpha = beta + 12; foo(bar, baz); "))
      txt  = T.unlines (replicate nlines body)
      ed0  = setLoaded path (emptyLoadResult { lrBuffer = fromText txt })
                       (newEditor (50, 200) defaultConfig)
  ed <- forceEd ed0
  timed ("  " ++ path ++ " " ++ show nlines ++ "x" ++ show lineLen
         ++ ": 100 keystrokes, full driver cycle") $ do
    let go !e !prev !acc i
          | i > (100 :: Int) = pure acc
          | otherwise = do
              let e1 = refreshHighlight (fst (update (KChar 'q') e))
                  s  = renderEditor e1
                  !n = frameBytes prev s
              go e1 (Just s) (acc + n) (i + 1)
    n <- go ed Nothing 0 1
    putStrLn ("    (" ++ show (n `div` 100) ++ " bytes/frame emitted)")

benchTypes :: IO ()
benchTypes = do
  benchType "/tmp/b.js" 120 5000
  benchType "/tmp/b.js" 600 5000
  benchType "/tmp/b.js" 3000 5000
  benchType "/tmp/b.txt" 120 5000
  benchType "/tmp/b.txt" 3000 5000
  benchType "/tmp/b.js" 120 200000

benchFrames :: IO ()
benchFrames = do
  benchFrame "/tmp/b.js" 120 5000
  benchFrame "/tmp/b.js" 600 5000
  benchFrame "/tmp/b.js" 3000 5000
  benchFrame "/tmp/b.txt" 120 5000
  benchFrame "/tmp/b.txt" 3000 5000

-- Where does a frame's time and allocation actually go?
benchDecomp :: String -> Int -> IO ()
benchDecomp path lineLen = do
  let body = T.pack (take lineLen (cycle "var alpha = beta + 12; foo(bar, baz); "))
      txt  = T.unlines (replicate 5000 body)
      ed0  = setLoaded path (emptyLoadResult { lrBuffer = fromText txt })
                       (newEditor (50, 200) defaultConfig)
  ed <- forceEd ed0
  putStrLn ("  -- " ++ path ++ " lines of " ++ show lineLen ++ " chars, 100 iterations")
  timed "     update only" $ do
    let go !e i | i > (100::Int) = pure e
                | otherwise = go (fst (update (KChar 'q') e)) (i+1)
    _ <- (go ed 1 >>= forceEd); pure ()
  timed "     update + refreshHighlight" $ do
    let go !e i | i > (100::Int) = pure e
                | otherwise = go (refreshHighlight (fst (update (KChar 'q') e))) (i+1)
    _ <- (go ed 1 >>= forceEd); pure ()
  timed "     + renderEditor (cells forced via diff vs itself)" $ do
    let go !e i | i > (100::Int) = pure e
                | otherwise =
                    let e1 = refreshHighlight (fst (update (KChar 'q') e))
                        s = renderEditor e1
                        !_ = frameBytes (Just s) s
                    in go e1 (i+1)
    _ <- (go ed 1 >>= forceEd); pure ()
  timed "     + renderEditor + full-redraw diff (prev = Nothing)" $ do
    let go !e i | i > (100::Int) = pure e
                | otherwise =
                    let e1 = refreshHighlight (fst (update (KChar 'q') e))
                        s = renderEditor e1
                        !_ = frameBytes Nothing s
                    in go e1 (i+1)
    _ <- (go ed 1 >>= forceEd); pure ()

benchDecomps :: IO ()
benchDecomps = mapM_ (uncurry benchDecomp)
  [("/tmp/b.js", 120), ("/tmp/b.txt", 120), ("/tmp/b.js", 3000)]

-- Load / save memory and time on a large file.
benchIO :: FilePath -> IO ()
benchIO path = do
  bs <- BS.readFile path
  putStrLn ("  file: " ++ show (BS.length bs `div` (1024*1024)) ++ " MB on disk")
  r0 <- resid
  lr <- timed "  loadFromBytes (decode + split)" $ do
    let lr = loadFromBytes False Nothing bs
    _ <- forceEd (setLoaded path lr (newEditor (50,200) defaultConfig))
    pure lr
  r1 <- resid
  putStrLn ("    live after load: " ++ show (round r1 :: Int) ++ " MB (was "
            ++ show (round r0 :: Int) ++ ")")
  s0 <- allocMB
  _ <- timed "  saveFile (whole-text build + encode + copy)" $
         saveFile (path ++ ".out") Utf8 LF True (lrBuffer lr)
  s1 <- allocMB
  putStrLn ("    save allocated " ++ show (round (s1 - s0) :: Int) ++ " MB")
  putStrLn ("    [keepalive " ++ show (lineCount (lrBuffer lr)) ++ " lines]")

-- Do the per-line Texts of a loaded file share the whole file's byte array?
benchShare :: FilePath -> IO ()
benchShare path = do
  bs <- BS.readFile path
  let lr = loadFromBytes False Nothing bs
      b  = lrBuffer lr
      !_ = sum [ T.length (getLine' i b) | i <- [0 .. lineCount b - 1] ]
  r1 <- resid
  -- Keep only 10 lines; everything else (including bs and the big Text) is garbage.
  let kept = [ getLine' i b | i <- [0 .. 9] ]
      !n = sum (map T.length kept)
  r2 <- resid
  putStrLn ("  live with whole file loaded: " ++ show (round r1 :: Int) ++ " MB")
  putStrLn ("  live keeping only 10 lines : " ++ show (round r2 :: Int) ++ " MB  (chars kept: " ++ show n ++ ")")
  -- Same, but forcing a copy of each kept line.
  let copied = map (T.copy) kept
      !m = sum (map T.length copied)
  r3 <- resid
  putStrLn ("  live keeping 10 T.copy'd   : " ++ show (round r3 :: Int) ++ " MB  (chars kept: " ++ show m ++ ")")

-- Word-wrap path vs the horizontal-scroll path, same content.
benchWrap :: IO ()
benchWrap = forM_ [(120::Int), 600, 3000] $ \len -> forM_ [False, True] $ \wrap -> do
  let body = T.pack (take len (cycle "var alpha = beta + 12; foo(bar, baz); "))
      txt  = T.unlines (replicate 5000 body)
      ed0  = (setLoaded "/tmp/b.txt" (emptyLoadResult { lrBuffer = fromText txt })
                       (newEditor (50, 200) defaultConfig)) { edWordWrap = wrap }
  ed <- forceEd ed0
  timed ("  wrap=" ++ show wrap ++ " lines of " ++ show len
         ++ ": 100 keystrokes, full cycle") $ do
    let go !e !prev !acc i
          | i > (100 :: Int) = pure acc
          | otherwise =
              let e1 = refreshHighlight (fst (update (KChar 'q') e))
                  s  = renderEditor e1
                  !n = frameBytes prev s
              in go e1 (Just s) (acc + n) (i + 1)
    _ <- go ed Nothing (0::Int) 1
    pure ()
  -- and a page-down heavy pass (the wrapped scroll math)
  timed ("  wrap=" ++ show wrap ++ " lines of " ++ show len ++ ": 100 PageDowns") $ do
    let go !e i | i > (100 :: Int) = pure e
                | otherwise =
                    let e1 = refreshHighlight (fst (update (KPageDown noMods) e))
                        s  = renderEditor e1
                        !_ = frameBytes Nothing s
                    in go e1 (i+1)
    _ <- (go ed 1 >>= forceEd); pure ()

-- What an open CSV document actually costs to hold: the buffer *and* the grid,
-- one construction path per process so the other cannot inflate the figure
-- (0026). "text" is the pre-0026 route — re-join the buffer into one Text and
-- parse that, which leaves the cells pointing into a second whole copy of the
-- file; "lines" is the shipped one. RSS is read from /proc, since the point of
-- the exercise is what `top` shows.
benchCsvLive :: FilePath -> String -> IO ()
benchCsvLive path how = do
  bs <- BS.readFile path
  let buf = lrBuffer (loadFromBytes False Nothing bs)
      v | how == "text" = Csv.mkCsvView ',' (bufferToText LF False buf)
        | otherwise     = Csv.mkCsvLines ',' (bufLines buf)
      !n = Csv.nRows v * Csv.nCols v
  n `seq` length (Csv.columnWidths v) `seq` pure ()
  r <- resid
  st <- readFile "/proc/self/status"
  let field k = case [ w | l <- lines st, (k' : w : _) <- [words l], k' == k ] of
                  (w : _) -> (read w :: Double) / 1024
                  _       -> 0
  -- Both must still be reachable past 'resid' (trap 1, in reverse).
  putStrLn ("  " ++ how ++ ": live " ++ show (round r :: Int) ++ " MB, RSS "
            ++ show (round (field "VmRSS:") :: Int) ++ " MB, peak "
            ++ show (round (field "VmHWM:") :: Int) ++ " MB ["
            ++ show (Csv.nRows v) ++ " rows, " ++ show (lineCount buf) ++ " lines]")

-- CSV table mode at scale: parse, per-keystroke edit, per-frame render.
benchCsv :: FilePath -> IO ()
benchCsv path = do
  bs <- BS.readFile path
  let txt = TE.decodeUtf8With TEE.lenientDecode bs
  putStrLn ("  csv: " ++ show (BS.length bs `div` (1024*1024)) ++ " MB")
  r0 <- resid
  v0 <- timed "  mkCsvView (parse + width cache)" $ do
    let v = Csv.mkCsvView ',' txt
        !n = Csv.nRows v * Csv.nCols v
    n `seq` length (Csv.columnWidths v) `seq` pure v
  r1 <- resid
  putStrLn ("    live after parse: " ++ show (round r1 :: Int) ++ " MB (was "
            ++ show (round r0 :: Int) ++ "), " ++ show (Csv.nRows v0) ++ " rows x "
            ++ show (Csv.nCols v0) ++ " cols")
  -- 0026: the load path as the editor actually runs it — the buffer's lines,
  -- not a whole-file Text. The pieces are timed separately because the width
  -- cache, not the parse, was the expensive half (2 697 MB of 3 719 MB).
  buf <- timed "  loadFromBytes (decode + split lines)" $ do
    let b = lrBuffer (loadFromBytes False Nothing bs)
        !n = lineCount b
    n `seq` pure b
  timed "  bufferToText (the re-join 0026 removed)" $ do
    let !n = T.length (bufferToText LF False buf)
    n `seq` pure ()
  timed "  csvParseLines (buffer lines) alone" $ do
    -- Deep-forced: a Seq is element-lazy, so WHNF parses nothing (trap 2).
    let g = Csv.csvParseLines ',' (bufLines buf)
        !n = foldl' (\ !a row -> foldl' (\ !b c -> b + T.length c) a row) (0 :: Int) g
    n `seq` pure ()
  vL <- timed "  mkCsvLines (parse + width cache)" $ do
    let v = Csv.mkCsvLines ',' (bufLines buf)
        !n = Csv.nRows v * Csv.nCols v
    n `seq` length (Csv.columnWidths v) `seq` pure v
  -- (No live-heap figure here: the text-parsed grid above is still reachable,
  -- so it would measure both. `bench csvlive FILE lines|text` retains exactly
  -- one path, which is what 0026's live/RSS numbers came from.)
  Csv.nRows vL `seq` pure ()
  -- Typing into a cell: begin edit then 100 inserts + commits (each commit
  -- goes through withRows/syncWidths and the modified check).
  timed "  100 cell edits (begin/insert/commit) at row 0" $ do
    let step !v _ = let v1 = Csv.beginEdit v
                        v2 = Csv.editInsert 'x' v1
                        v3 = Csv.commitEdit v2
                    in Csv.isModified v3 `seq` v3
        vN = foldl' step v0 [1 :: Int .. 100]
    Csv.nRows vN `seq` pure ()
  timed "  100 cell edits at the LAST row" $ do
    let vEnd = Csv.setCursor (Csv.nRows v0 - 1) 3 v0
        step !v _ = let v1 = Csv.beginEdit v
                        v2 = Csv.editInsert 'x' v1
                        v3 = Csv.commitEdit v2
                    in Csv.isModified v3 `seq` v3
        vN = foldl' step vEnd [1 :: Int .. 100]
    Csv.nRows vN `seq` pure ()
  timed "  100 editInsert keystrokes inside ONE cell (no commit)" $ do
    let v1 = Csv.beginEdit (Csv.setCursor (Csv.nRows v0 - 1) 3 v0)
        step !v _ = Csv.editInsert 'x' v
        vN = foldl' step v1 [1 :: Int .. 100]
    Csv.currentCellText vN `seq` pure ()
  timed "  100 commitEdit (Tab between cells) at the LAST row" $ do
    let step !v _ = Csv.commitEdit (Csv.editInsert 'x' (Csv.beginEdit v))
        vN = foldl' step (Csv.setCursor (Csv.nRows v0 - 1) 3 v0) [1 :: Int .. 100]
    Csv.nRows vN `seq` pure ()
  timed "  100 commitEdit WITHOUT the isModified check" $ do
    let step !v _ = Csv.commitEdit (Csv.editInsert 'x' (Csv.beginEdit v))
        vN = foldl' step (Csv.setCursor (Csv.nRows v0 - 1) 3 v0) [1 :: Int .. 100]
    Csv.nRows vN `seq` pure ()
  -- 0028: 'Csv.isModified v' is loop-invariant, and full laziness floats it out
  -- of a repeat loop — the probe would time one call and 99 comparisons. Moving
  -- the cursor first makes each iteration a distinct view (an O(1) record
  -- update that touches neither grid), which is enough to keep the call inside.
  let modProbe lbl v = timed lbl $ do
        let !k = length [ () | i <- [1 :: Int .. 100]
                             , Csv.isModified (Csv.setCursor 0 (i `mod` 3) v) ]
        k `seq` pure ()
  modProbe "  100 isModified calls alone (UNMODIFIED grid)" v0
  -- The same call once the grid has actually been edited. 0016 measured only
  -- the line above, where the top-level pointer test short-circuits; after one
  -- edit the saved grid is no longer the same object and the comparison had to
  -- walk every row, on every keystroke.
  let vMod = Csv.commitEdit (Csv.editInsert 'x' (Csv.beginEdit
               (Csv.setCursor (Csv.nRows v0 - 1) 3 v0)))
  modProbe "  100 isModified calls alone (MODIFIED grid)" vMod
  -- ...and with the edit at the *first* row, where the old comparison found its
  -- difference immediately and stopped. Same edit, 3 000x the cost, depending
  -- only on where in the file it was: the shape of the defect.
  let vMod0 = Csv.commitEdit (Csv.editInsert 'x' (Csv.beginEdit (Csv.setCursor 0 3 v0)))
  modProbe "  100 isModified calls alone (MODIFIED at row 0)" vMod0
  -- The comparison 0028 replaced, kept here so its cost is still measurable —
  -- once with the pointer shortcuts it was designed around and once without,
  -- because whether they fire under -O2 is the difference between 2 ms and
  -- 400 ms and is not something the source can be read to decide. Also run on
  -- the grid built from the *buffer's lines*, which is what the editor opens.
  let sameGridRef fast a b =
        ptrEq a b
          || (Seq.length a == Seq.length b && and (zipWith sameRow (toList a) (toList b)))
        where
          sameRow r s = (fast && ptrEq r s)
                        || (Seq.length r == Seq.length s
                            && and (zipWith sameCell (toList r) (toList s)))
          sameCell x y = (fast && ptrEq x y) || x == y
      refProbe lbl fast v = timed lbl $ do
        let !k = length [ () | i <- [1 :: Int .. 100]
                             , sameGridRef fast (Csv.csvRows v)
                                 (Csv.csvSaved (Csv.setCursor 0 (i `mod` 3) v)) ]
        k `seq` pure ()
  refProbe "  100 pre-0028 comparisons, pointer shortcuts on " True vMod
  refProbe "  100 pre-0028 comparisons, pointer shortcuts off" False vMod
  let vLMod = Csv.commitEdit (Csv.editInsert 'x' (Csv.beginEdit
                (Csv.setCursor (Csv.nRows vL - 1) 3 vL)))
  refProbe "  100 pre-0028 comparisons, grid from buffer lines" True vLMod
  modProbe "  100 isModified calls alone (grid from buffer lines)" vLMod
  timed "  100 undo/redo pairs (with the isModified check)" $ do
    let step !v _ = let u = Csv.undo v
                        r = Csv.redo u
                    in Csv.isModified u `seq` Csv.isModified r `seq` r
        vN = foldl' step vMod [1 :: Int .. 100]
    Csv.nRows vN `seq` pure ()
  timed "  100 undo/redo pairs WITHOUT the isModified check" $ do
    let step !v _ = Csv.redo (Csv.undo v)
        vN = foldl' step vMod [1 :: Int .. 100]
    Csv.nRows vN `seq` pure ()
  timed "  100 columnWidths calls alone" $ do
    let !k = sum [ sum (Csv.columnWidths v0) | _ <- [1 :: Int .. 100] ]
    k `seq` pure ()
  -- 0029: "which line of the serialised file does this cell start on?" — asked
  -- by the recents, the session file, the crash journal and the nav history,
  -- and (before 0029) reached from every keystroke via 'sessionShape'.
  --
  -- Both traps apply here at once. The result is a *tuple*, so `pure $!` forces
  -- the constructor and computes neither component (trap 2 — this is exactly
  -- how the driver-vs-in-process discrepancy in 0028 came about); and the call
  -- is loop-invariant, so it floats out of a repeat loop (trap 3). Hence
  -- 'force2' and the varying row.
  let force2 (a, b) = a + b
      posProbe lbl r = timed lbl $ do
        let !k = foldl' (\ !acc i -> acc + force2 (Csv.cellTextPos v0 (r - i `mod` 3) 3))
                        (0 :: Int) [1 :: Int .. 10]
        k `seq` pure ()
  posProbe "  10 cellTextPos at row 0     " 3
  posProbe "  10 cellTextPos at the middle" (Csv.nRows v0 `div` 2)
  posProbe "  10 cellTextPos at the LAST row" (Csv.nRows v0 - 1)
  let cellProbe lbl ln = timed lbl $ do
        let !k = foldl' (\ !acc i -> acc + force2 (Csv.textPosCell v0 (ln - i `mod` 3) 3))
                        (0 :: Int) [1 :: Int .. 10]
        k `seq` pure ()
  cellProbe "  10 textPosCell at line 0    " 3
  cellProbe "  10 textPosCell at the middle" (Csv.nRows v0 `div` 2)
  cellProbe "  10 textPosCell at the LAST line" (Csv.nRows v0 - 1)
  -- The cache that makes those cheap, built from scratch (the load-path cost).
  timed "  computeNl over the whole grid (load path)" $ do
    let !k = length (show (Csv.computeNl ',' (Csv.csvRows v0)))
    k `seq` pure ()
  -- The whole reason 0029 exists: the driver evaluates this after every key
  -- batch, and it must not depend on where the cursor is.
  let edCsvDoc = setLoaded path (loadFromBytes False Nothing bs)
                           (newEditor (50, 200) defaultConfig)
      atRow r e = case edCsv e of
        Nothing -> e
        Just vv -> e { edCsv = Just (Csv.setCursor r 0 vv) }
      -- Forced the way the driver forces it: it *compares* the shape with the
      -- last one, which walks the paths character by character. `length ps`
      -- alone would force only the list spine and leave every path a thunk,
      -- and the whole defect is one level further in (trap 2 again).
      shapeProbe lbl r = timed lbl $ do
        let one i = case sessionShape (atRow (max 0 (r - i `mod` 3)) edCsvDoc) of
                      (f, ps, a) -> length (show f) + sum (map length ps) + a
            !k = foldl' (\ !acc i -> acc + one i) (0 :: Int) [1 :: Int .. 100]
        k `seq` pure ()
  _ <- pure $! (case sessionShape edCsvDoc of (_, ps, a) -> sum (map length ps) + a)
  shapeProbe "  100 sessionShape at row 0     " 0
  shapeProbe "  100 sessionShape at the LAST row" (Csv.nRows v0 - 1)
  r2 <- resid
  putStrLn ("    live after edits: " ++ show (round r2 :: Int) ++ " MB")
  putStrLn ("    [keepalive " ++ show (Csv.nRows v0) ++ "]")

-- A ByteSource over an in-memory ByteString (no terminal needed).
memSource :: BS.ByteString -> IO ByteSource
memSource bs0 = do
  ref <- newIORef bs0
  let next = atomicModifyIORef' ref (\bs -> case BS.uncons bs of
                Just (b, r) -> (r, Just b)
                Nothing     -> (bs, Nothing))
      chunk = atomicModifyIORef' ref (\bs -> (BS.drop 65536 bs, BS.take 65536 bs))
      pushBack bs = modifyIORef' ref (bs <>)
  pure (ByteSource next (const next) chunk pushBack)

-- Bracketed paste of N bytes through the real parser.
benchPaste :: Int -> IO ()
benchPaste n = do
  let payload = BS.replicate n 120   -- 'x'
      stream  = BS.concat [ BS.pack [0x1b,0x5b,0x32,0x30,0x30,0x7e]  -- ESC[200~
                          , payload
                          , BS.pack [0x1b,0x5b,0x32,0x30,0x31,0x7e] ]
  src <- memSource stream
  timed ("  bracketed paste of " ++ show (n `div` 1024) ++ " KiB") $ do
    k <- nextKey src
    case k of
      KPaste t -> putStrLn ("    -> KPaste of " ++ show (T.length t) ++ " chars")
      other    -> putStrLn ("    -> unexpected: " ++ take 40 (show other))

benchPastes :: IO ()
benchPastes = mapM_ benchPaste [64*1024, 256*1024, 1024*1024, 4*1024*1024]

-- Ordinary typing through the parser, for scale.
benchKeys :: Int -> IO ()
benchKeys n = do
  src <- memSource (BS.replicate n 97)
  timed ("  " ++ show n ++ " plain keystrokes through nextKey") $ do
    let go 0 = pure ()
        go k = nextKey src >> go (k - 1 :: Int)
    go n

-- Image decode + scale + cell render.
benchImage :: FilePath -> IO ()
benchImage path = do
  bs <- BS.readFile path
  putStrLn ("  " ++ path ++ ": " ++ show (BS.length bs `div` 1024) ++ " KiB")
  r0 <- resid
  img <- timed "  decodeImage" $ case Img.decodeImage bs of
    Left e    -> error e
    Right im  -> do let !w = Img.imgW im; !h = Img.imgH im
                    w `seq` h `seq` pure im
  r1 <- resid
  putStrLn ("    " ++ show (Img.imgW img) ++ "x" ++ show (Img.imgH img)
            ++ ", live " ++ show (round r0 :: Int) ++ " -> " ++ show (round r1 :: Int) ++ " MB")
  timed "  scaleRGBA to 800x600 (a kitty placement)" $ do
    let im2 = Img.scaleRGBA img (0, 0, Img.imgW img, Img.imgH img) 800 600
    BS.length im2 `seq` pure ()
  timed "  10x scaleRGBA to 800x600 (resize storm)" $
    forM_ [1 :: Int .. 10] $ \_ -> do
      let im2 = Img.scaleRGBA img (0, 0, Img.imgW img, Img.imgH img) 800 600
      BS.length im2 `seq` pure ()

-- Search: literal vs regex matching throughput over a big text.
benchSearch :: FilePath -> IO ()
benchSearch path = do
  bs <- BS.readFile path
  let txt = TE.decodeUtf8With TEE.lenientDecode bs
      mb  = fromIntegral (BS.length bs) / 1048576 :: Double
  putStrLn ("  corpus: " ++ show (round mb :: Int) ++ " MB")
  forM_ [ ("literal, case-insensitive", S.compileMatcher False False False (T.pack "line 12345"))
        , ("literal, case-sensitive",   S.compileMatcher True  False False (T.pack "line 12345"))
        , ("literal CI, rare first char", S.compileMatcher False False False (T.pack "zebra"))
        , ("literal CI, common first ch",  S.compileMatcher False False False (T.pack "line zzz"))
        , ("literal, whole word",       S.compileMatcher True  True  False (T.pack "line"))
        , ("regex  ^line [0-9]+",       S.compileMatcher True  False True  (T.pack "^line [0-9]+"))
        , ("regex  [a-h]{4} [a-h]{4}",  S.compileMatcher True  False True  (T.pack "[a-h]{4} [a-h]{4}"))
        ] $ \(lbl, em) -> case em of
      Left e -> putStrLn ("  " ++ lbl ++ ": compile error " ++ e)
      Right m -> timed ("  " ++ lbl) $ do
        let (ms, _, cnt) = S.fileMatchesWith (S.matcherLine m) txt
        length ms `seq` cnt `seq` putStrLn ("    -> " ++ show cnt ++ " occurrences")

-- Explorer panel with a large expanded directory.
benchExplorer :: IO ()
benchExplorer = forM_ [(200::Int), 5000, 20000] $ \n -> do
  let entries = [ ("/w/f" ++ show i, False, Just 100) | i <- [1 .. n] ]
      br = Br.mkBrowserNoParent "/w" entries
      body = T.pack (take 120 (cycle "var alpha = beta + 12; "))
      txt = T.unlines (replicate 2000 body)
      ed0 = (setLoaded "/tmp/b.js" (emptyLoadResult { lrBuffer = fromText txt })
               (newEditor (50, 200) defaultConfig)) { edExplorer = Just br }
  ed <- forceEd ed0
  timed ("  " ++ show n ++ " entries: 100 x Br.visibleRows") $ do
    let !k = sum [ length (Br.visibleRows br) | _ <- [1 :: Int .. 100] ]
    k `seq` pure ()
  timed ("  " ++ show n ++ " entries: 100 full frames with the panel open") $ do
    let go !prev !acc i
          | i > (100 :: Int) = pure acc
          | otherwise =
              let e = refreshHighlight (ed { edTop = i })
                  s = renderEditor e
                  !b = frameBytes prev s
              in go (Just s) (acc + b) (i + 1)
    _ <- go Nothing (0::Int) 1
    pure ()

-- Pixel-placement encoders: cost per emitted frame.
benchGfx :: FilePath -> IO ()
benchGfx path = do
  bs <- BS.readFile path
  let img = either error id (Img.decodeImage bs)
      rgba = Img.scaleRGBA img (0, 0, Img.imgW img, Img.imgH img) 800 600
  BS.length rgba `seq` pure ()
  timed "  10 x kittyPlace 800x600 (base64 RGBA)" $
    forM_ [1 :: Int .. 10] $ \_ ->
      let b = Gfx.kittyPlace (1, 1) (100, 25) (800, 600) rgba
      in BL.length (toLazyByteString b) `seq` pure ()
  timed "  10 x sixelPlace 800x600 (palette + RLE)" $
    forM_ [1 :: Int .. 10] $ \_ ->
      let b = Gfx.sixelPlace (1, 1) (800, 600) rgba
      in BL.length (toLazyByteString b) `seq` pure ()
  let kb = BL.length (toLazyByteString (Gfx.kittyPlace (1,1) (100,25) (800,600) rgba)) `div` 1024
      sb = BL.length (toLazyByteString (Gfx.sixelPlace (1,1) (800,600) rgba)) `div` 1024
  putStrLn ("    payload: kitty " ++ show kb ++ " KiB, sixel " ++ show sb ++ " KiB")

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["undo", n] -> benchUndo (read n)
    ["lex"]     -> benchLex
    ["sync"]    -> benchSync
    ["render"]  -> benchRender
    ["edit"]    -> benchEdit
    ["edit2"]   -> benchEdit2
    ["long"]    -> benchLongLines
    ["lex2"]    -> benchLex2
    ["frame"]   -> benchFrames
    ["type"]    -> benchTypes
    ["decomp"]  -> benchDecomps
    ["io", f]   -> benchIO f
    ["share", f] -> benchShare f
    ["wrap"]    -> benchWrap
    ["csv", f]  -> benchCsv f
    ["csvlive", f, how] -> benchCsvLive f how
    ["paste"]   -> benchPastes >> benchKeys 200000
    ["image", f] -> benchImage f
    ["search", f] -> benchSearch f
    ["explorer"] -> benchExplorer
    ["gfx", f]  -> benchGfx f
    _           -> putStrLn "usage: bench (undo N|lex|sync|render)"
