-- Optimized decode benchmark; see plan 0018 / 0032 for the build command.
-- Input IO, output hashing and optional RGBA dumps are outside the measurement.
--
-- Output per file: decode CPU ms, bytes allocated during the decode, the
-- RTS's peak memory in use (whole process, so it includes the input bytes;
-- run one file per process for a per-file peak), and an FNV-1a hash of every
-- frame's RGBA so revisions can be compared for byte-identical output.
-- FRAMES=1 decodes through 'decodeFrames' (every GIF frame) instead.
module Main where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.Array.Unboxed as U
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word64)
import GHC.Stats (getRTSStats, allocated_bytes, max_mem_in_use_bytes, max_live_bytes)
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
  frames <- maybe False (== "1") <$> lookupEnv "FRAMES"
  forM_ paths $ \path -> do
    bs <- BS.readFile path
    evaluate (BS.length bs)
    performGC
    before <- getRTSStats
    start <- getCPUTime
    ims <- if frames
      then either fail (\fs -> mapM (evaluate . imgPix . fst) fs) (decodeFrames bs)
      else either fail (\im -> (: []) <$> evaluate (imgPix im)) (decodeImage bs)
    end <- getCPUTime
    after <- getRTSStats
    let hash = foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211)
                     (14695981039346656037 :: Word64) (concatMap U.elems ims)
    printf "%s: %.3f ms, %d bytes, peak %d MB (live %d MB), %d frame(s), RGBA FNV-1a %016x\n" path
      (fromIntegral (end - start) / 1e9 :: Double)
      (allocated_bytes after - allocated_bytes before)
      (max_mem_in_use_bytes after `div` 1000000)
      (max_live_bytes after `div` 1000000)
      (length ims) hash
    case dumpDir of
      Nothing -> pure ()
      Just dir -> BS.writeFile (dir </> takeFileName path ++ ".rgba") (BS.pack (concatMap U.elems ims))
