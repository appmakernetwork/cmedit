{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables #-}
-- | Deterministic, cancellation-safe parallelism for the JPEG XL decoder.
--
-- The decoder is a pure function ('Cmedit.Jxl.decodeJxl'), run by the
-- editor on a background thread that may be killed at any moment (the
-- user opens another file while a wallpaper is decoding). Parallel work
-- inside it therefore has to be invisible from outside: the result and
-- any error must not depend on scheduling, and an asynchronous exception
-- delivered to the decoding thread must stop and join every worker
-- before it propagates, so no worker ever outlives the call that forked
-- it. 'parOrdered' is the one primitive that provides this; the decoder
-- builds row bands and filter tiles on it.
-- Original scoped scheduler: Fable. Partial-spawn cleanup, repeatable
-- completion latches and consume-before-credit: Astra.
--
-- Base only, no FFI, portable (the Windows build typechecks it).
module Cmedit.JxlPar
  ( parOrdered
  , parWorkers
  ) where

import Control.Concurrent (forkIOWithUnmask, killThread, getNumCapabilities)
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad (forM, replicateM, when)
import Data.Array (Array, listArray, (!))

-- | How many worker threads a parallel stage may use: the capability
-- count, which the decoder never changes. One means "run sequentially".
parWorkers :: IO Int
parWorkers = getNumCapabilities

-- | @parOrdered nworkers look n job consume@ runs @job i@ for every
-- @i@ in @[0 .. n - 1]@ on @nworkers@ threads and hands each result to
-- @consume i@ on the calling thread, in index order, with at most @look@
-- results claimed ahead of the one being consumed. @consume@ returns
-- whether to carry on; 'False' stops the stage (the remaining jobs are
-- never started, running ones are killed and joined).
--
-- Determinism: jobs must not read storage another job writes. They may write
-- disjoint regions of private decoder storage; otherwise shared writes
-- belong in @consume@, which runs on one thread in order. A job's own exception is re-thrown by the consumer at that
-- job's turn, so errors too are reported in index order.
--
-- Cancellation: the whole call runs under 'mask'. On any exception in
-- the consumer (including an asynchronous one aimed at the calling
-- thread) every worker is killed and joined under 'uninterruptibleMask'
-- before the exception propagates; on normal completion the workers are
-- joined too. No worker survives the call, and no MVar it owns is left
-- unfilled: each result slot is written by exactly the worker that
-- claimed it, and a worker that dies for any reason while holding a
-- claim writes the exception into that slot first.
--
-- With one worker (or one job) the call is the plain sequential loop,
-- so a single-capability program runs exactly the code it did before.
parOrdered :: forall a. Int -> Int -> Int -> (Int -> IO a) -> (Int -> a -> IO Bool) -> IO ()
parOrdered nworkers look n job consume
  | n <= 0 = pure ()
  | nworkers <= 1 || n == 1 = sequential 0
  | otherwise = mask $ \restore -> do
      slots <- replicateM n newEmptyMVar
      let slotOf = listArray (0, n - 1) slots :: Array Int (MVar (Either SomeException a))
      next <- newMVar (0 :: Int)
      gate <- newQSem (max 1 look)
      -- Workers run masked and unmask only to wait for the gate and to
      -- run the job, so a claim can never be abandoned: once a worker
      -- holds index i, the slot for i is written whatever happens.
      let worker :: (forall b. IO b -> IO b) -> IO ()
          worker unmask = do
            unmask (waitQSem gate)
            i <- modifyMVar next (\k -> pure (k + 1, k))
            if i >= n then signalQSem gate else do
              -- `try` also catches an asynchronous exception that lands
              -- inside the job (ThreadKilled from the teardown below):
              -- it is recorded, then re-thrown so the worker still dies.
              r <- try (unmask (job i >>= evaluate))
              putMVar (slotOf ! i) r
              case r of
                Left e | isAsync e -> throwIO e
                _ -> worker unmask
      dones <- replicateM (min nworkers n) newEmptyMVar
      let spawn [] = pure []
          spawn (done : rest) = do
            tid <- forkIOWithUnmask $ \unmask -> worker unmask `finally` putMVar done ()
            tids <- spawn rest `onException` uninterruptibleMask_ (killThread tid >> readMVar done)
            pure (tid : tids)
      tids <- spawn dones
      let stopAll = uninterruptibleMask_ $ do
            mapM_ killThread tids
            mapM_ readMVar dones
          consumer !i
            | i >= n = pure ()
            | otherwise = do
                r <- takeMVar (slotOf ! i)
                v <- either throwIO pure r
                go <- consume i v
                signalQSem gate
                when go (consumer (i + 1))
      restore (consumer 0) `onException` stopAll
      stopAll
  where
    sequential !i
      | i >= n = pure ()
      | otherwise = do
          v <- job i
          go <- consume i v
          when go (sequential (i + 1))
    isAsync e = case fromException e of
      Just (SomeAsyncException _) -> True
      Nothing -> False
