-- Corruption fuzzer for the image decoders (plan 0032). For each input file
-- it decodes N seeded mutants (byte overwrites, a truncation, or both) and
-- prints one line per mutant: an RGBA hash or the error. Run two builds on
-- the same inputs and diff the output: an optimisation must not change what
-- a corrupt file decodes to, and a crash (the decoders use unchecked array
-- access) shows up as a missing tail. Build like ImageDecode.hs.
--   fuzz N FILE...
module Main where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_)
import qualified Data.Array.Unboxed as U
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word64, Word8)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Timeout (timeout)
import Cmedit.Image

lcg :: Word64 -> Word64
lcg x = x * 6364136223846793005 + 1442695040888963407

mutate :: Int -> BS.ByteString -> BS.ByteString
mutate seed bs = cut (foldl' flipAt bs (take nFlips rs))
  where
    rs = tail (iterate lcg (fromIntegral seed * 2654435761 + 1))
    n = BS.length bs
    kind = seed `mod` 3
    nFlips = if kind == 1 then 0 else 1 + seed `mod` 8
    flipAt b r
      | n == 0 = b
      | otherwise =
          let i = fromIntegral ((r `shiftR` 16) `mod` fromIntegral n)
              v = fromIntegral (r `shiftR` 40) :: Word8
          in BS.concat [BS.take i b, BS.singleton v, BS.drop (i + 1) b]
    cut b | kind == 0 = b
          | otherwise = BS.take (fromIntegral ((rs !! 9 `shiftR` 20) `mod` fromIntegral (max 1 n))) b

main :: IO ()
main = do
  (nS : paths) <- getArgs
  forM_ paths $ \path -> do
    bs <- BS.readFile path
    forM_ [1 .. read nS] $ \seed -> do
      r0 <- timeout 10000000 $ try (case decodeFrames (mutate seed bs) of
                  Left e -> pure ("Left " ++ e)
                  Right fs -> do
                    h <- evaluate (foldl' (\a b -> (a `xor` fromIntegral b) * 1099511628211)
                                    (14695981039346656037 :: Word64)
                                    (concatMap (U.elems . imgPix . fst) fs))
                    pure (show (length fs) ++ " " ++ show h))
      let line = case r0 of
            Nothing -> "TIMEOUT"
            Just r  -> either (\e -> "EXC " ++ takeWhile (/= '\n') (show (e :: SomeException))) id r
      putStrLn (path ++ " #" ++ show seed ++ ": " ++ line)
      hFlush stdout
