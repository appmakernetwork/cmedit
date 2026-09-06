-- Optimized decode benchmark; see plan 0018 for the build command.
-- Input IO, output hashing and optional RGBA dumps are outside the measurement.
module Main where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.Array.Unboxed as U
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word64)
import GHC.Stats (getRTSStats, allocated_bytes)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs, lookupEnv)
import System.FilePath ((</>), takeFileName)
import System.Mem (performGC)
import Text.Printf (printf)
import Cmedit.Image

main :: IO ()
main = do
  paths <- getArgs
  dumpDir <- lookupEnv "IMAGE_RGBA_DIR"
  forM_ paths $ \path -> do
    bs <- BS.readFile path
    evaluate (BS.length bs)
    performGC
    before <- getRTSStats
    start <- getCPUTime
    im <- either fail (evaluate . imgPix) (decodeImage bs)
    end <- getCPUTime
    performGC
    after <- getRTSStats
    let hash = foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211)
                     (14695981039346656037 :: Word64) (U.elems im)
    printf "%s: %.3f ms, %d bytes, RGBA FNV-1a %016x\n" path
      (fromIntegral (end - start) / 1e9 :: Double)
      (allocated_bytes after - allocated_bytes before) hash
    case dumpDir of
      Nothing -> pure ()
      Just dir -> BS.writeFile (dir </> takeFileName path ++ ".rgba") (BS.pack (U.elems im))
