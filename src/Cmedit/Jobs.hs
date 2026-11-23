-- | Background-job supervision for the driver: at most one job of each kind,
-- the newest cancelling the previous, and a job that can be cancelled when
-- nothing wants its result any more.
--
-- Split out of "Cmedit.App" so the one property everything depends on can be
-- tested directly: __a superseded job dies promptly__. The job thread is
-- forked with its asynchronous exceptions /unmasked/ ('forkIOWithUnmask'),
-- whatever the caller's masking state. That is load-bearing: the table update
-- runs under 'mask_' (so a job cannot slip out of the table between the fork
-- and the insert), and a thread forked inside 'mask_' with a plain 'forkIO'
-- inherits the mask, so 'killThread' on it blocks until the whole job has
-- finished. Measured on a busy pure loop, that was 3.35 s per kill: a zoom
-- notch on a heavy picture froze the event loop for the length of the render
-- it was meant to cancel.
--
-- A job may still mask its own critical sections (a journal rename is one);
-- it just does not start masked.
module Cmedit.Jobs
  ( JobTable
  , newJobTable
  , startJobIn
  , cancelJobIn
  , runningJob
  , backgroundCap
  , forkBackground
  ) where

import Control.Concurrent (ThreadId, forkOn, forkOnWithUnmask, getNumCapabilities, killThread, myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally, mask_)
import Data.IORef
import qualified Data.Map.Strict as M
import System.IO.Unsafe (unsafePerformIO)

-- | The running job of each kind.
type JobTable k = IORef (M.Map k ThreadId)

newJobTable :: IO (JobTable k)
newJobTable = newIORef M.empty

-- | Start a job of this kind, killing any previous one first. Returns
-- whether a previous job was killed. The job removes itself from the table
-- when it ends, unless a newer job of the same kind has replaced it.
startJobIn :: Ord k => JobTable k -> k -> IO () -> IO Bool
startJobIn tbl kind act = mask_ $ do
  prev <- atomicModifyIORef' tbl (\m -> (M.delete kind m, M.lookup kind m))
  mapM_ killThread prev
  -- The child waits for its own table entry before it starts, so a job that
  -- finishes instantly can never run its 'clear' before the insert and leave
  -- a dead entry behind.
  go <- newEmptyMVar
  cap <- backgroundCap
  tid <- forkOnWithUnmask cap (\unmask -> (takeMVar go >> unmask act) `finally` clear)
  atomicModifyIORef' tbl (\m -> (M.insert kind tid m, ()))
  putMVar go ()
  pure (maybe False (const True) prev)
  where
    clear = do
      me <- myThreadId
      atomicModifyIORef' tbl $ \m ->
        (M.update (\t -> if t == me then Nothing else Just t) kind m, ())

-- | Cancel the running job of this kind, if any (its result is no longer
-- wanted). Returns whether one was running.
cancelJobIn :: Ord k => JobTable k -> k -> IO Bool
cancelJobIn tbl kind = mask_ $ do
  prev <- atomicModifyIORef' tbl (\m -> (M.delete kind m, M.lookup kind m))
  mapM_ killThread prev
  pure (maybe False (const True) prev)

-- | Is a job of this kind running?
runningJob :: Ord k => JobTable k -> k -> IO Bool
runningJob tbl kind = M.member kind <$> readIORef tbl

-- | A capability for a background thread: the capabilities other than the
-- UI thread's (0, where the main thread starts) in turn, or 0 when there is
-- only one. Background work is placed explicitly because the editor runs
-- with the scheduler's automatic thread migration off (@-qm@ in the
-- Makefile's RTSOPTS): a thread left where it was forked would share the UI
-- thread's capability, and one that migrates touches the allocation area of
-- every capability it visits.
backgroundCap :: IO Int
backgroundCap = do
  n <- getNumCapabilities
  if n <= 1 then pure 0 else do
    k <- atomicModifyIORef' bgNext (\i -> (i + 1, i))
    pure (1 + k `mod` (n - 1))

{-# NOINLINE bgNext #-}
bgNext :: IORef Int
bgNext = unsafePerformIO (newIORef 0)

-- | 'forkIO' onto a 'backgroundCap'.
forkBackground :: IO () -> IO ThreadId
forkBackground act = backgroundCap >>= \c -> forkOn c act
