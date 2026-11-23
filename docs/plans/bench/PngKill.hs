-- Kill test for a decoder's parallel stages: interrupt a decode at N evenly
-- spaced points, check no worker thread survives, then force the same thunk
-- again and compare with an undisturbed decode. Build like ImageDecode.hs
-- (threaded, -N4).   PngKill N FILE...
module Main where
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Array.Unboxed as U
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import GHC.Conc (listThreads, threadStatus, ThreadStatus(..))
import System.Environment (getArgs)
import GHC.Clock (getMonotonicTimeNSec)
import Cmedit.Image

hashImg :: Either String Image -> Word64
hashImg (Left e) = fromIntegral (length e)
hashImg (Right im) = let p = imgPix im; (lo, hi) = U.bounds p
                         go !i !h | i > hi = h
                                  | otherwise = go (i + 1) ((h `xor` fromIntegral (p U.! i)) * 1099511628211)
                     in go lo 14695981039346656037

live :: IO Int
live = do ts <- listThreads
          ss <- mapM threadStatus ts
          pure (length [() | s <- ss, s /= ThreadFinished, s /= ThreadDied])

main :: IO ()
main = do
  (nS : files) <- getArgs
  let n = read nS :: Int
  forM_ files $ \f -> do
    bs <- BS.readFile f
    t0 <- getMonotonicTimeNSec
    ref <- evaluate (hashImg (decodeImage bs))
    t1 <- getMonotonicTimeNSec
    let durUs = fromIntegral (t1 - t0) `div` 1000 :: Int
    base <- live
    bad <- fmap sum $ forM [0 .. n - 1] $ \k -> do
      copy <- evaluate (BS.copy bs)
      let r = decodeImage copy
      done <- newEmptyMVar
      t <- forkIO ((evaluate (hashImg r) >> pure ()) `finally` putMVar done ())
      threadDelay (max 1 (durUs * (k + 1) `div` (n + 1)))
      killThread t
      takeMVar done
      let waitDead = do st <- threadStatus t
                        unless (st == ThreadFinished || st == ThreadDied) (yield >> waitDead)
      waitDead
      let settle i = do v <- live
                        if v /= base && i < (2000 :: Int) then yield >> settle (i + 1) else pure v
      l <- settle 0
      h <- evaluate (hashImg r)
      l2 <- live
      when (l /= base || l2 /= base) $ putStrLn ("  threads leaked at k=" ++ show k ++ ": " ++ show (base, l, l2))
      when (h /= ref) $ putStrLn ("  hash differs at k=" ++ show k)
      pure (if l /= base || l2 /= base || h /= ref then 1 else 0 :: Int)
    putStrLn (f ++ ": " ++ show durUs ++ " us, " ++ show n ++ " kills, " ++ show bad ++ " bad")
