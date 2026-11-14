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
  ) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally, mask_)
import Data.IORef
import qualified Data.Map.Strict as M

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
  tid <- forkIOWithUnmask (\unmask -> (takeMVar go >> unmask act) `finally` clear)
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
