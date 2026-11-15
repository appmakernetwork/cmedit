-- Optimized decode benchmark; see plan 0018 / 0032 for the build command.
-- Input IO, output hashing and optional RGBA dumps are outside the measurement.
--
-- Output per file: decode wall-clock ms (CPU ms too, in the parenthesis;
-- they differ once a decoder runs in parallel under -threaded), bytes allocated during the decode, the
-- RTS's peak memory in use (whole process, so it includes the input bytes;
-- run one file per process for a per-file peak), and an FNV-1a hash of every
-- frame's RGBA so revisions can be compared for byte-identical output.
-- FRAMES=1 decodes through 'decodeFrames' (every GIF frame) instead.
-- IMAGE_RAW=1 decodes a JPEG through 'decodeJPEGSamples' (what a PDF's
-- DCTDecode gets: four bytes per pixel, the samples as stored after the
-- colour transform), hashing and dumping those instead (jpegcheck.sh --raw).
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
import GHC.Clock (getMonotonicTimeNSec)
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
  raw <- maybe False (== "1") <$> lookupEnv "IMAGE_RAW"
  forM_ paths $ \path -> do
    bs <- BS.readFile path
    evaluate (BS.length bs)
    performGC
    before <- getRTSStats
    start <- getCPUTime
    wstart <- getMonotonicTimeNSec
    ims <- if raw
      then either fail (\js -> (: []) <$> evaluate (jsData js)) (decodeJPEGSamples Nothing bs)
      else if frames
      then either fail (\fs -> mapM (evaluate . imgPix . fst) fs) (decodeFrames bs)
      else either fail (\im -> (: []) <$> evaluate (imgPix im)) (decodeImage bs)
    wend <- getMonotonicTimeNSec
    end <- getCPUTime
    after <- getRTSStats
    -- Index loops, never 'U.elems': a list of every byte of a 28 MP picture
    -- is ~114M cons cells, and the dump below used to materialise it whole
    -- (BS.pack needs the length first): over 10 GB resident for one file.
    let fnv h b = (h `xor` fromIntegral b) * 1099511628211
        hashArr h a = let (lo, hi) = U.bounds a
                      in foldl' (\acc i -> fnv acc (a U.! i)) h [lo .. hi]
        hash = foldl' hashArr (14695981039346656037 :: Word64) ims
        toBytes a = let (lo, hi) = U.bounds a
                    in fst (BS.unfoldrN (hi - lo + 1) (\i -> Just (a U.! i, i + 1)) lo)
    printf "%s: %.3f ms, %d bytes, peak %d MB (live %d MB, cpu %.1f ms), %d frame(s), RGBA FNV-1a %016x\n" path
      (fromIntegral (wend - wstart) / 1e6 :: Double)
      (allocated_bytes after - allocated_bytes before)
      (max_mem_in_use_bytes after `div` 1000000)
      (max_live_bytes after `div` 1000000)
      (fromIntegral (end - start) / 1e9 :: Double)
      (length ims) hash
    case dumpDir of
      Nothing -> pure ()
      Just dir -> BS.writeFile (dir </> takeFileName path ++ ".rgba") (BS.concat (map toBytes ims))
