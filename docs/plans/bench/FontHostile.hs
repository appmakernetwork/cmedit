-- | Hostile-input costs for the font readers (plan 0034 B review items 15,
-- 16, 18). Build against any src/ tree to compare before/after:
--   ghc --make docs/plans/bench/FontHostile.hs -isrc -itest -O2 -rtsopts \
--       -with-rtsopts=-T -outputdir /tmp/fh -o /tmp/fh/fonthostile <exts> -w
--   /tmp/fh/fonthostile gen /tmp      # write the hostile fixtures
--   /tmp/fh/fonthostile /tmp/hostile-hex.pfa   # one file per process
-- Reports wall time, bytes allocated and peak live heap.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.List (foldl')
import GHC.Clock (getMonotonicTime)
import GHC.Stats
import System.Environment (getArgs)
import System.Mem (performMajorGC)
import Text.Printf (printf)

import Cmedit.Geom
import Cmedit.Font
import FontSpec (hostileHexPfa, hostileComposite)

measure :: String -> IO Double -> IO ()
measure label act = do
  performMajorGC
  s0 <- getRTSStats
  t0 <- getMonotonicTime
  v <- act
  t1 <- getMonotonicTime
  s1 <- getRTSStats
  printf "%-40s %9.1f ms  alloc %8.1f MB  peak live %7.1f MB  (%g)\n" label ((t1 - t0) * 1000)
    (fromIntegral (allocated_bytes s1 - allocated_bytes s0) / 1e6 :: Double)
    (fromIntegral (max_live_bytes s1) / 1e6 :: Double) v

pathSum :: Path -> Double
pathSum = foldl' (\a s -> a + case s of
  MoveTo (Pt x y) -> x + y; LineTo (Pt x y) -> x + y; QuadTo _ (Pt x y) -> x + y
  CubicTo _ _ (Pt x y) -> x + y; ClosePath -> 1) 0

-- | @gen DIR@ writes the hostile fixtures (a 4 MB hex eexec Type 1 and a
-- 1024-point-matched-component TrueType); @FILE...@ measures each in turn.
-- Run one file per process for a meaningful peak (it is process-wide).
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["gen", dir] -> do
      BS.writeFile (dir ++ "/hostile-hex.pfa") (hostileHexPfa 2000000)
      BS.writeFile (dir ++ "/hostile-composite.ttf") hostileComposite
    files -> forM_ files $ \p -> do
      bs <- BS.readFile p
      _ <- evaluate (BS.length bs)
      measure ("parse + outline + advance all: " ++ reverse (takeWhile (/= '/') (reverse p))) $
        pure $! either (const (-1)) (\f -> sum [ pathSum (glyphPath f g) + glyphAdvance f g | g <- [0 .. fontNumGlyphs f - 1] ]) (parseFont bs)
