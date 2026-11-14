-- | Timings for the SVG picture view's hot paths (plan 0034 F): parse, the
-- cell raster, the overlay raster and its payload copy, for a given file.
--
--   ghc -O2 -isrc docs/plans/bench/SvgViewBench.hs -outputdir /tmp/svb -o /tmp/svb/svgviewbench
--   /tmp/svb/svgviewbench file.svg ...
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import Text.Printf (printf)

import Cmedit.Geom (rectW, rectH)
import Cmedit.Raster (RenderStats(..))
import Cmedit.Scene (Scene(..))
import Cmedit.Svg (defaultSvgEnv, parseSvgText)
import Cmedit.VecView

timed :: IO a -> IO (a, Double)
timed act = do
  t0 <- getMonotonicTime
  x <- act
  t1 <- getMonotonicTime
  pure (x, (t1 - t0) * 1000)

main :: IO ()
main = getArgs >>= mapM_ one
  where
    one path = do
      bs <- BS.readFile path
      let txt = TE.decodeUtf8With TEE.lenientDecode bs
      (r, tp) <- timed (case parseSvgText defaultSvgEnv txt of
                          Left e -> pure (Left e)
                          Right sc -> forceScene sc >> pure (Right sc))
      case r of
        Left e -> printf "%s: parse failed (%s)\n" path e
        Right sc -> do
          let b = scBounds sc
              full = (0, 0, fst (vecDims (rectW b, rectH b)), snd (vecDims (rectW b, rectH b)))
              (vw, vh) = vecDims (rectW b, rectH b)
              zoom = (vw `div` 4, vh `div` 4, vw `div` 8, vh `div` 8)
              raster crop w h = case rasterVec sc crop w h of
                Just (img, st) -> evaluate img >> pure (Just (img, st))
                Nothing -> pure Nothing
          (_, tc) <- timed (raster full 118 76)
          (_, ts) <- timed (case rasterVecWith vecSyncBudget sc full 118 76 of
                              Just (img, _) -> evaluate img >> pure ()
                              Nothing -> pure ())
          (g, tg) <- timed (raster full 944 608)
          (_, tz) <- timed (raster zoom 944 608)
          (_, tb) <- timed (case g of
                              Just (img, _) -> evaluate (BS.length (imageRGBA img))
                              Nothing -> pure 0)
          printf "%s: cost %d  parse %.1f ms  cells(118x76) %.1f ms  sync-budget cells %.1f ms  overlay(944x608) %.1f ms  zoom8x %.1f ms  payload %.1f ms%s\n"
            path (sceneCost sc) tp tc ts tg tz tb
            (case g of Just (_, st) | rsExhausted st -> "  EXHAUSTED" :: String; _ -> "")
