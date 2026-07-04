-- | Low-level terminal control: raw mode (via the POSIX @termios@ API exposed
-- by the @unix@ package), window-size queries (via a tiny @ioctl@ C helper)
-- and signal wiring. This is the only module that talks to the real tty.
module Cmedit.Term
  ( -- * Raw mode
    setRawMode
  , restoreTermAttrs
  , withRawMode
  , SavedTerm
    -- * Size
  , getTerminalSize
  , getTerminalPixels
  , isTerminal
    -- * Signals
  , installResizeHandler
  , installInterruptHandlers
    -- * Handle setup
  , configureHandles
  ) where

import Control.Concurrent (ThreadId)
import Control.Exception (bracket, throwTo, AsyncException(UserInterrupt))
import Control.Monad (void, when)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.Environment (lookupEnv)
import System.IO
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Signals
import System.Posix.Signals.Exts (windowChange)
import System.Posix.Terminal
import System.Posix.Types (Fd(..))
import Text.Read (readMaybe)

foreign import ccall unsafe "cmedit_get_winsize"
  c_get_winsize :: CInt -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "cmedit_get_winsize_px"
  c_get_winsize_px :: CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- | Opaque saved terminal attributes, restored on teardown.
newtype SavedTerm = SavedTerm TerminalAttributes

-- Modes turned off to enter "raw" mode (equivalent to @cfmakeraw@ plus a
-- couple of extras so Ctrl-C/Ctrl-S/Ctrl-Z reach us as bytes rather than
-- signals or flow control).
rawOff :: [TerminalMode]
rawOff =
  [ ProcessInput        -- ICANON: read byte-at-a-time, no line editing
  , EnableEcho          -- ECHO
  , EchoErase, EchoKill, EchoLF
  , KeyboardInterrupts  -- ISIG: deliver ^C/^Z/^\ as bytes
  , ExtendedFunctions   -- IEXTEN
  , StartStopInput      -- IXOFF
  , StartStopOutput     -- IXON: free up ^S/^Q
  , MapCRtoLF           -- ICRNL: keep CR distinct from LF
  , MapLFtoCR, IgnoreCR
  , InterruptOnBreak    -- BRKINT
  , ProcessOutput       -- OPOST: we position everything explicitly
  , CheckParity, StripHighBit, MarkParityErrors
  ]

makeRaw :: TerminalAttributes -> TerminalAttributes
makeRaw a0 =
  let a1 = foldl withoutMode a0 rawOff
      a2 = a1 `withBits` 8
      a3 = a2 `withMinInput` 1
      a4 = a3 `withTime` 0
  in a4

-- | Put the controlling terminal into raw mode, returning the previous
-- attributes so they can be restored later.
setRawMode :: IO SavedTerm
setRawMode = do
  old <- getTerminalAttributes stdInput
  setTerminalAttributes stdInput (makeRaw old) WhenFlushed
  pure (SavedTerm old)

-- | Restore previously-saved terminal attributes.
restoreTermAttrs :: SavedTerm -> IO ()
restoreTermAttrs (SavedTerm old) =
  setTerminalAttributes stdInput old WhenDrained

-- | Run an action with the terminal in raw mode, restoring on the way out.
withRawMode :: IO a -> IO a
withRawMode act = bracket setRawMode restoreTermAttrs (const act)

-- | Is the given handle an interactive terminal?
isTerminal :: Handle -> IO Bool
isTerminal h = hIsTerminalDevice h

-- | Query the terminal size as @(rows, cols)@. Falls back to @$LINES@/
-- @$COLUMNS@ and finally to a conservative 24x80 if the ioctl fails.
getTerminalSize :: IO (Int, Int)
getTerminalSize = do
  r <- tryFd 1
  case r of
    Just sz -> pure sz
    Nothing -> do
      r0 <- tryFd 0
      case r0 of
        Just sz -> pure sz
        Nothing -> do
          r2 <- tryFd 2
          case r2 of
            Just sz -> pure sz
            Nothing -> envFallback
  where
    tryFd :: CInt -> IO (Maybe (Int, Int))
    tryFd fd = alloca $ \pr -> alloca $ \pc -> do
      ok <- c_get_winsize fd pr pc
      if ok == 0
        then do
          rows <- peek pr
          cols <- peek pc
          pure (Just (fromIntegral rows, fromIntegral cols))
        else pure Nothing
    envFallback = do
      ls <- lookupEnv "LINES"
      cs <- lookupEnv "COLUMNS"
      let rows = maybe 24 id (ls >>= readMaybe)
          cols = maybe 80 id (cs >>= readMaybe)
      pure (max 1 rows, max 1 cols)

-- | The terminal's text area in pixels as @(width, height)@, when the kernel
-- knows it (the same @TIOCGWINSZ@ ioctl carries @ws_xpixel@/@ws_ypixel@).
-- Many terminals leave the fields zero — 'Nothing' then, and the driver falls
-- back to the XTWINOPS pixel queries, and past that to a 2:1 cell assumption.
getTerminalPixels :: IO (Maybe (Int, Int))
getTerminalPixels = firstJust [1, 0, 2]
  where
    firstJust [] = pure Nothing
    firstJust (fd : rest) = do
      r <- tryFd fd
      maybe (firstJust rest) (pure . Just) r
    tryFd :: CInt -> IO (Maybe (Int, Int))
    tryFd fd = alloca $ \px -> alloca $ \py -> do
      ok <- c_get_winsize_px fd px py
      if ok == 0
        then do
          x <- peek px
          y <- peek py
          pure (Just (fromIntegral x, fromIntegral y))
        else pure Nothing

-- | Install a SIGWINCH handler that runs the given action on every resize.
installResizeHandler :: IO () -> IO ()
installResizeHandler action =
  void $ installHandler windowChange (Catch action) Nothing

-- | Install handlers for terminating signals so that a controlled shutdown
-- (which restores the terminal) happens instead of an abrupt kill. The
-- handler throws 'UserInterrupt' to the main thread so @bracket@ cleanup runs.
installInterruptHandlers :: ThreadId -> IO ()
installInterruptHandlers tid = do
  let toMain = Catch (throwTo tid UserInterrupt)
  void $ installHandler sigTERM toMain Nothing
  void $ installHandler sigHUP  toMain Nothing

-- | Put stdin/stdout into the binary, unbuffered state the renderer and
-- input parser expect.
configureHandles :: IO ()
configureHandles = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout (BlockBuffering Nothing)
  hSetEcho stdin False
