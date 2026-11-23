-- Kill test for the streamed PNG open ('Cmedit.App.streamPng', files of
-- 8 MiB or more): kill the loading thread at N evenly spaced points, check
-- that no worker thread and no file descriptor survives, then open the file
-- again and compare with the whole-file decode. Build like the test suite
-- but optimised and threaded (-O2 -threaded, +RTS -N4).
--   PngStreamKill N FILE...
module Main where
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Array.Unboxed as U
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (listThreads, threadStatus, ThreadStatus(..))
import System.Directory (listDirectory)
import System.Environment (getArgs)
import Cmedit.App (streamPng, LoadOutcome(..))
import Cmedit.Editor (Decoded(..))
import Cmedit.Image

hashImg :: Image -> Word64
hashImg im = let p = imgPix im; (lo, hi) = U.bounds p
                 go !i !h | i > hi = h
                          | otherwise = go (i + 1) ((h `xor` fromIntegral (p U.! i)) * 1099511628211)
             in go lo 14695981039346656037

outcome :: Maybe LoadOutcome -> Maybe Word64
outcome (Just (OutImage _ dc)) = Just (hashImg (fst (head (dcFrames dc))))
outcome _ = Nothing

live :: IO Int
live = do ts <- listThreads
          ss <- mapM threadStatus ts
          pure (length [() | s <- ss, s /= ThreadFinished, s /= ThreadDied])

fds :: IO Int
fds = length <$> listDirectory "/proc/self/fd"

main :: IO ()
main = do
  (nS : files) <- getArgs
  let n = read nS :: Int
  forM_ files $ \f -> do
    bs <- BS.readFile f
    ref <- evaluate (either (const Nothing) (Just . hashImg) (decodeImage bs))
    t0 <- getMonotonicTimeNSec
    first <- streamPng f >>= evaluate . outcome
    t1 <- getMonotonicTimeNSec
    when (first /= ref || first == Nothing) $ putStrLn "  streamed open differs from the whole-file decode"
    let durUs = fromIntegral (t1 - t0) `div` 1000 :: Int
    base <- live
    fd0 <- fds
    bad <- fmap sum $ forM [0 .. n - 1] $ \k -> do
      done <- newEmptyMVar
      finished <- newEmptyMVar
      t <- forkIO ((streamPng f >>= evaluate . outcome >>= putMVar finished) `finally` putMVar done ())
      threadDelay (max 1 (durUs * (k + 1) `div` (n + 1)))
      killThread t
      takeMVar done
      let waitDead = do st <- threadStatus t
                        unless (st == ThreadFinished || st == ThreadDied) (yield >> waitDead)
      waitDead
      let settle i = do v <- live
                        if v /= base && i < (2000 :: Int) then yield >> settle (i + 1) else pure v
      l <- settle 0
      fd1 <- fds
      survived <- tryTakeMVar finished          -- it finished before the kill landed
      again <- streamPng f >>= evaluate . outcome
      l2 <- live
      fd2 <- fds
      when (l /= base || l2 /= base) $ putStrLn ("  threads leaked at k=" ++ show k ++ ": " ++ show (base, l, l2))
      when (fd1 /= fd0 || fd2 /= fd0) $ putStrLn ("  descriptors leaked at k=" ++ show k ++ ": " ++ show (fd0, fd1, fd2))
      when (again /= ref) $ putStrLn ("  reopen differs at k=" ++ show k)
      when (maybe False (/= ref) survived) $ putStrLn ("  a completed open differs at k=" ++ show k)
      pure (if l /= base || l2 /= base || fd1 /= fd0 || fd2 /= fd0 || again /= ref
               || maybe False (/= ref) survived then 1 else 0 :: Int)
    putStrLn (f ++ ": " ++ show durUs ++ " us, " ++ show n ++ " kills, " ++ show bad ++ " bad")
