-- | Long-session soak: drives the pure editor model through a scripted session
-- for as long as you ask, sampling the heap as it goes, and fails if memory or
-- cost per operation grows with session length.
--
-- This is the guard the plans in @docs/plans@ needed: every one of them claims
-- a stability property (bounded history, bounded processes, flat allocation),
-- and none of those claims survives a refactor unless something checks them
-- repeatedly. The undo-history leak was not subtle — it was one idiom repeated
-- at nine sites — and it stayed invisible because nothing had ever measured the
-- editor's heap after ten thousand edits.
--
-- Build with @make soak@ (which needs @-O2@ and @-with-rtsopts=-T@; the ordinary
-- test suite builds @-O0@, which is right for correctness and wrong for this).
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import GHC.Stats
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Mem (performMajorGC)
import Text.Printf (printf)

import Cmedit.Editor
import Cmedit.TextBuffer
import Cmedit.Types
import Cmedit.ConfigFile (defaultConfig)
import qualified Cmedit.Csv as Csv

------------------------------------------------------------------------------
-- The scripted session

-- | A cheap deterministic generator: the script must be reproducible so a
-- failure can be re-run. (The RTS forbids Math.random-style entropy here
-- anyway — determinism is the point.)
lcg :: Int -> Int
lcg s = (s * 1103515245 + 12345) `mod` 2147483648

-- | One step of a long editing session. The mix is weighted the way real use
-- is — mostly typing — but every branch touches state that *accumulates*, which
-- is what this harness exists to watch:
--
--   * typing / deleting / undo / redo  -> undo and redo stacks
--   * switching and closing documents  -> the zipper, recents, per-doc caches
--   * find terms                       -> the persisted input history
--   * long jumps                       -> the navigation trail
--   * CSV cell edits                   -> the table's own undo and width cache
--   * wrap and resize                  -> layout and the highlight cache
step :: Int -> Editor -> Editor
step r ed = case r `mod` 20 of
  0  -> fst (update (KChar 'a') ed)
  1  -> fst (update (KChar 'b') ed)
  2  -> fst (update (KChar ' ') ed)
  3  -> fst (update KBackspace ed)
  4  -> fst (update KEnter ed)
  5  -> fst (update (KCtrlChar 'z') ed)          -- undo
  6  -> fst (update (KCtrlChar 'y') ed)          -- redo
  7  -> fst (update (KArrow DDown noMods) ed)
  8  -> fst (update (KArrow DUp noMods) ed)
  9  -> fst (update (KCtrlChar 'a') ed)          -- select all
  10 -> fst (update (KCtrlChar 'c') ed)          -- copy (clipboard retention)
  11 -> fst (update (KEnd (Mods False True False)) ed)   -- Ctrl+End: a far jump
  12 -> fst (update (KHome (Mods False True False)) ed)  -- Ctrl+Home: another
  13 -> fst (update (KChar 'q') ed)
  14 -> fst (update (KCtrlChar 'v') ed)          -- paste from the internal mirror
  15 -> resize (24 + (r `mod` 20), 80 + (r `mod` 60)) ed
  16 -> fst (update (KCtrlChar 'd') ed)          -- duplicate line
  17 -> fst (update (KArrow DRight (Mods False False True)) ed)
  18 -> fst (update (KChar 'z') ed)
  _  -> fst (update (KChar 'x') ed)

-- | Seed a session: a document with a bit of everything, plus a couple of
-- other open files so the zipper is exercised.
seedEditor :: Editor
seedEditor =
  let body = T.unlines [ T.pack ("line " ++ show i ++ " some text here")
                       | i <- [1 .. 400 :: Int] ]
      ed0  = setLoaded "/soak/main.py"
               (emptyLoadResult { lrBuffer = fromText body })
               (newEditor (40, 120) defaultConfig)
      ed1  = addDocument "/soak/other.txt"
               (emptyLoadResult { lrBuffer = fromText (T.pack "second file\n") }) ed0
  in addDocument "/soak/third.md"
       (emptyLoadResult { lrBuffer = fromText (T.pack "# third\n") }) ed1

-- | The CSV half of the session, run on its own view so the table's undo
-- history and width cache accumulate too.
csvStep :: Int -> Csv.CsvView -> Csv.CsvView
csvStep r v = case r `mod` 8 of
  0 -> Csv.setCurrentCell (T.pack (replicate (1 + r `mod` 20) 'x')) v
  1 -> Csv.commitEdit (Csv.editInsert 'q' (Csv.beginEdit v))
  2 -> Csv.insertRowBelow v
  3 -> Csv.deleteRow v
  4 -> Csv.undo v
  5 -> Csv.redo v
  6 -> Csv.moveCursor DDown v
  _ -> Csv.moveCursor DRight v

------------------------------------------------------------------------------
-- Sampling

data Sample = Sample
  { sIter  :: !Int
  , sLive  :: !Double   -- ^ MB live after a major collection
  , sAlloc :: !Double   -- ^ MB allocated since the previous sample
  , sSecs  :: !Double   -- ^ seconds since the previous sample
  }

sampleNow :: Int -> Double -> Double -> IO Sample
sampleNow i prevAlloc prevT = do
  performMajorGC
  st <- getRTSStats
  now <- getMonotonicTime
  let live  = fromIntegral (gcdetails_live_bytes (gc st)) / 1048576
      alloc = fromIntegral (allocated_bytes st) / 1048576
  pure (Sample i live (alloc - prevAlloc) (now - prevT))

------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let iters = case args of (n : _) | [(v, "")] <- reads n -> v; _ -> 60000
      every = max 1000 (iters `div` 40)
  enabled <- getRTSStatsEnabled
  unless enabled $ do
    putStrLn "soak: needs the -T RTS flag (build with make soak)"
    exitFailure
  putStrLn ("soak: " ++ show iters ++ " operations, sampling every " ++ show every)

  t0 <- getMonotonicTime
  st0 <- getRTSStats
  let a0 = fromIntegral (allocated_bytes st0) / 1048576
      csv0 = Csv.mkCsvView ',' (T.pack "a,b,c\n1,2,3\n4,5,6")

      loop !i !ed !cv !seed !prevA !prevT acc
        | i > iters = pure (reverse acc)
        | otherwise = do
            let seed' = lcg seed
                ed'   = step seed' ed
                cv'   = if seed' `mod` 5 == 0 then csvStep seed' cv else cv
            if i `mod` every == 0
              then do
                -- Force what we are measuring; a lazy fold would measure its
                -- own thunk chain (see bench/README.md).
                lineCount (edBuffer ed') `seq` Csv.nRows cv' `seq` pure ()
                s <- sampleNow i prevA prevT
                loop (i + 1) ed' cv' seed' (prevA + sAlloc s) (prevT + sSecs s) (s : acc)
              else loop (i + 1) ed' cv' seed' prevA prevT acc

  samples <- loop 1 seedEditor csv0 12345 a0 t0 []
  when (length samples < 4) $ do
    putStrLn "soak: too few samples to judge (raise the iteration count)"
    exitFailure

  putStrLn "   iter        live MB    alloc MB/1k      ms/1k"
  forM_ samples $ \s ->
    printf "%8d %12.2f %14.2f %10.2f\n" (sIter s) (sLive s)
           (sAlloc s / fromIntegral every * 1000)
           (sSecs s / fromIntegral every * 1000 * 1000)

  -- Compare the first and last quarter of the run. Ratios, not absolutes, so
  -- the thresholds survive any machine; generous, because this is guarding an
  -- order of magnitude, not a percentage.
  let n = length samples
      q = max 1 (n `div` 4)
      firstQ = take q samples
      lastQ  = drop (n - q) samples
      avg f xs = sum (map f xs) / fromIntegral (length xs)
      liveA  = avg sLive firstQ;  liveB  = avg sLive lastQ
      allocA = avg sAlloc firstQ; allocB = avg sAlloc lastQ
      timeA  = avg sSecs firstQ;  timeB  = avg sSecs lastQ
      checks =
        -- The additive slack has to stay small: an earlier +4 MB swallowed the
        -- very leak this exists to catch (unbounded undo history showed as
        -- 0.7 -> 4.0 MB and still "passed"). Verified in both directions —
        -- removing the history bound now fails this check.
        [ ("live heap is flat", liveB <= liveA * 2 + 1.5
          , printf "%.1f MB -> %.1f MB" liveA liveB)
        , ("allocation per operation is flat", allocB <= allocA * 1.5 + 1
          , printf "%.1f MB -> %.1f MB per sample" allocA allocB)
        , ("time per operation is flat", timeB <= timeA * 2 + 0.05
          , printf "%.0f ms -> %.0f ms per sample" (timeA * 1000) (timeB * 1000))
        ]
  putStrLn ""
  forM_ checks $ \(name, ok, detail) ->
    putStrLn ((if ok then "PASS  " else "FAIL  ") ++ name ++ "  (" ++ detail ++ ")")
  if all (\(_, ok, _) -> ok) checks then exitSuccess else exitFailure
