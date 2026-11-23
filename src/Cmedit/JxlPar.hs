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
  , parOrderedResumable
  , parWorkers
  , workerCap
  ) where

import Control.Concurrent (forkOnWithUnmask, killThread, getNumCapabilities, myThreadId, throwTo, threadCapability)
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad (forM, replicateM, when)
import Data.Array (Array, listArray, (!))

-- | How many worker threads a parallel stage may use: the capability
-- count, which the decoder never changes. One means "run sequentially".
parWorkers :: IO Int
parWorkers = getNumCapabilities

-- | The capability for worker @j@ of a stage started on this thread: the
-- ones after the caller's, in turn. Workers are placed explicitly rather
-- than left to the scheduler's load balancing, so a stage keeps its
-- parallelism when that balancing is off (the editor runs with @-qm@; see
-- the Makefile's RTSOPTS).
workerCap :: Int -> IO Int
workerCap j = do
  (c, _) <- threadCapability =<< myThreadId
  n <- getNumCapabilities
  pure ((c + 1 + j) `mod` max 1 n)

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
-- The re-throw is synchronous, so the thunks under evaluation on the
-- calling thread are poisoned rather than frozen; a caller that can be
-- forced again after a kill uses 'parOrderedResumable' instead.
--
-- With one worker (or one job) the call is the plain sequential loop,
-- so a single-capability program runs exactly the code it did before.
parOrdered :: Int -> Int -> Int -> (Int -> IO a) -> (Int -> a -> IO Bool) -> IO ()
parOrdered nworkers look n job consume = () <$ parCore False nworkers look n job consume

-- | 'parOrdered' for a caller that runs inside a pure computation whose
-- thunk may be forced again after the forcing thread is killed. It returns
-- 'True' when the stage ran (to the end, or until @consume@ stopped it),
-- and 'False' when it was interrupted by an asynchronous exception and the
-- thunk has since been forced again: the stage's work is then incomplete
-- and whatever it was writing is in an unknown state, so the caller must
-- discard it and redo the computation another way (sequentially).
--
-- Why it exists: 'parOrdered' tears down and then re-throws an
-- asynchronous exception with 'throwIO', which is a /synchronous/ raise
-- from the handler. GHC freezes the thunks under evaluation on a thread
-- only when an exception arrives asynchronously; a synchronous raise
-- updates every one of them to "raise ThreadKilled", so a decode killed
-- in the consumer was poisoned for good and every later force of that
-- thunk (by another job that wants the same picture) failed at once.
-- Here, after the workers are joined, the exception is re-delivered with
-- 'throwTo' to the calling thread itself, which the RTS raises
-- asynchronously even while masked: the thunks are frozen, the thread
-- dies with the original exception, and a later force resumes the frozen
-- stack just after the 'throwTo', where this returns 'False'. The workers
-- are gone by then and cannot be resumed, which is why the caller must
-- start over. The resumed stack still holds 'mask''s exit frame, so the
-- resuming thread leaves this call unmasked whatever its own masking state
-- was; the editor's jobs force pictures unmasked, so that is harmless.
-- Synchronous exceptions (a job's own error) propagate with 'throwIO' as
-- in 'parOrdered'.
parOrderedResumable :: Int -> Int -> Int -> (Int -> IO a) -> (Int -> a -> IO Bool) -> IO Bool
parOrderedResumable = parCore True

parCore :: forall a. Bool -> Int -> Int -> Int -> (Int -> IO a) -> (Int -> a -> IO Bool) -> IO Bool
parCore resumable nworkers look n job consume
  | n <= 0 = pure True
  | nworkers <= 1 || n == 1 = True <$ sequential 0
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
      let spawn _ [] = pure []
          spawn !j (done : rest) = do
            cap <- workerCap j
            tid <- forkOnWithUnmask cap $ \unmask -> worker unmask `finally` putMVar done ()
            tids <- spawn (j + 1) rest `onException` uninterruptibleMask_ (killThread tid >> readMVar done)
            pure (tid : tids)
      tids <- spawn (0 :: Int) dones
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
      r <- try (restore (consumer 0)) :: IO (Either SomeException ())
      stopAll
      case r of
        Right () -> pure True
        Left e
          | resumable && isAsync e -> do
              me <- myThreadId
              throwTo me e
              pure False
          | otherwise -> throwIO e
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
