{-# LANGUAGE ForeignFunctionInterface #-}
-- | The Windows platform layer: the twin of @platform/posix/Cmedit/Term.hs@
-- with the identical export list; the build picks one. Raw mode is the
-- console API's virtual-terminal mode — @SetConsoleMode@ with
-- ENABLE_VIRTUAL_TERMINAL_INPUT / ENABLE_VIRTUAL_TERMINAL_PROCESSING — so
-- keystrokes, mouse reports and our output travel as the same VT escape
-- sequences the rest of the editor already speaks. Everything here is
-- hand-rolled kernel32 FFI (in the spirit of the project, and so the build
-- needs no @Win32@ package). Requires a VT-capable console: Windows 10
-- 1809+ (Windows Terminal recommended; it is the Windows 11 default).
--
-- There is no SIGWINCH on Windows, so resize is a 200 ms poll of the console
-- geometry; console close/logoff/shutdown arrive via @SetConsoleCtrlHandler@
-- and are converted to the same 'UserInterrupt' the POSIX side uses, so the
-- driver's bracketed teardown still runs.
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
    -- * Directory-walk stat
  , EntryStat(..)
  , statEntry
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Exception (SomeException, bracket, handle, throwTo, AsyncException(UserInterrupt))
import Control.Monad (void, when)
import Data.Bits ((.&.), (.|.), complement)
import Data.Int (Int16)
import Data.Word (Word32)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (peekByteOff)
import System.Directory (doesDirectoryExist, doesFileExist, getFileSize, pathIsSymbolicLink)
import System.Environment (lookupEnv)
import System.IO
import Text.Read (readMaybe)

type HANDLE = Ptr ()
type DWORD  = Word32

foreign import ccall unsafe "GetStdHandle"
  c_GetStdHandle :: DWORD -> IO HANDLE
foreign import ccall unsafe "GetConsoleMode"
  c_GetConsoleMode :: HANDLE -> Ptr DWORD -> IO CInt
foreign import ccall unsafe "SetConsoleMode"
  c_SetConsoleMode :: HANDLE -> DWORD -> IO CInt
foreign import ccall unsafe "GetConsoleScreenBufferInfo"
  c_GetConsoleScreenBufferInfo :: HANDLE -> Ptr () -> IO CInt
foreign import ccall unsafe "GetConsoleCP"
  c_GetConsoleCP :: IO DWORD
foreign import ccall unsafe "GetConsoleOutputCP"
  c_GetConsoleOutputCP :: IO DWORD
foreign import ccall unsafe "SetConsoleCP"
  c_SetConsoleCP :: DWORD -> IO CInt
foreign import ccall unsafe "SetConsoleOutputCP"
  c_SetConsoleOutputCP :: DWORD -> IO CInt

-- The console ctrl-event callback runs on an OS thread of the system's
-- choosing; the threaded RTS (which the build always uses) handles that.
type CtrlHandler = DWORD -> IO CInt
foreign import ccall "wrapper"
  mkCtrlHandler :: CtrlHandler -> IO (FunPtr CtrlHandler)
foreign import ccall unsafe "SetConsoleCtrlHandler"
  c_SetConsoleCtrlHandler :: FunPtr CtrlHandler -> CInt -> IO CInt

stdInputHandle, stdOutputHandle :: DWORD
stdInputHandle  = 0xFFFFFFF6  -- STD_INPUT_HANDLE  (-10)
stdOutputHandle = 0xFFFFFFF5  -- STD_OUTPUT_HANDLE (-11)

-- Input-mode flags.
eProcessedInput, eLineInput, eEchoInput, eMouseInput, eQuickEdit,
  eExtendedFlags, eVtInput :: DWORD
eProcessedInput = 0x0001   -- ^C as an event, not a byte — off in raw mode
eLineInput      = 0x0002   -- cooked line editing (ICANON's cousin)
eEchoInput      = 0x0004
eMouseInput     = 0x0010   -- INPUT_RECORD mouse events; we take mouse via VT instead
eQuickEdit      = 0x0040   -- click-to-select would swallow the mouse
eExtendedFlags  = 0x0080   -- required for eQuickEdit to be honoured
eVtInput        = 0x0200   -- ENABLE_VIRTUAL_TERMINAL_INPUT: keys arrive as VT bytes

-- Output-mode flags.
eProcessedOutput, eVtProcessing, eNoAutoReturn :: DWORD
eProcessedOutput = 0x0001
eVtProcessing    = 0x0004  -- ENABLE_VIRTUAL_TERMINAL_PROCESSING: interpret our escapes
eNoAutoReturn    = 0x0008  -- DISABLE_NEWLINE_AUTO_RETURN: we position explicitly

utf8CodePage :: DWORD
utf8CodePage = 65001

-- | Saved console modes and code pages, restored on teardown. A 'Nothing'
-- mode means the handle wasn't a console (e.g. redirected) and is left alone.
data SavedTerm = SavedTerm
  { savedInMode  :: !(Maybe DWORD)
  , savedOutMode :: !(Maybe DWORD)
  , savedInCP    :: !DWORD
  , savedOutCP   :: !DWORD
  }

getMode :: HANDLE -> IO (Maybe DWORD)
getMode h = allocaBytes 4 $ \p -> do
  ok <- c_GetConsoleMode h p
  if ok == 0 then pure Nothing else Just <$> peekByteOff p 0

-- | Put the console into raw VT mode, returning the previous state so it can
-- be restored later.
setRawMode :: IO SavedTerm
setRawMode = do
  hin  <- c_GetStdHandle stdInputHandle
  hout <- c_GetStdHandle stdOutputHandle
  mIn  <- getMode hin
  mOut <- getMode hout
  inCP  <- c_GetConsoleCP
  outCP <- c_GetConsoleOutputCP
  case mIn of
    Nothing -> pure ()
    Just m  -> void $ c_SetConsoleMode hin $
      (m .&. complement (eProcessedInput .|. eLineInput .|. eEchoInput
                         .|. eMouseInput .|. eQuickEdit))
        .|. eExtendedFlags .|. eVtInput
  case mOut of
    Nothing -> pure ()
    Just m  -> void $ c_SetConsoleMode hout $
      m .|. eProcessedOutput .|. eVtProcessing .|. eNoAutoReturn
  void $ c_SetConsoleCP utf8CodePage
  void $ c_SetConsoleOutputCP utf8CodePage
  pure (SavedTerm mIn mOut inCP outCP)

-- | Restore previously-saved console modes and code pages.
restoreTermAttrs :: SavedTerm -> IO ()
restoreTermAttrs (SavedTerm mIn mOut inCP outCP) = do
  hin  <- c_GetStdHandle stdInputHandle
  hout <- c_GetStdHandle stdOutputHandle
  maybe (pure ()) (void . c_SetConsoleMode hin)  mIn
  maybe (pure ()) (void . c_SetConsoleMode hout) mOut
  void $ c_SetConsoleCP inCP
  void $ c_SetConsoleOutputCP outCP

-- | Run an action with the console in raw mode, restoring on the way out.
withRawMode :: IO a -> IO a
withRawMode act = bracket setRawMode restoreTermAttrs (const act)

-- | Is the given handle an interactive terminal?
isTerminal :: Handle -> IO Bool
isTerminal h = hIsTerminalDevice h

-- | Query the console size as @(rows, cols)@ from the visible window
-- rectangle (the screen buffer can be much taller than the window). Falls
-- back to @$LINES@/@$COLUMNS@ and finally to a conservative 24x80.
getTerminalSize :: IO (Int, Int)
getTerminalSize = do
  h <- c_GetStdHandle stdOutputHandle
  r <- windowRect h
  case r of
    Just sz -> pure sz
    Nothing -> envFallback
  where
    envFallback = do
      ls <- lookupEnv "LINES"
      cs <- lookupEnv "COLUMNS"
      let rows = maybe 24 id (ls >>= readMaybe)
          cols = maybe 80 id (cs >>= readMaybe)
      pure (max 1 rows, max 1 cols)

-- CONSOLE_SCREEN_BUFFER_INFO is 22 bytes; srWindow (a SMALL_RECT of four
-- SHORTs: Left, Top, Right, Bottom) sits at byte offset 10.
windowRect :: HANDLE -> IO (Maybe (Int, Int))
windowRect h = allocaBytes 24 $ \p -> do
  ok <- c_GetConsoleScreenBufferInfo h p
  if ok == 0
    then pure Nothing
    else do
      left   <- peekByteOff p 10 :: IO Int16
      top    <- peekByteOff p 12 :: IO Int16
      right  <- peekByteOff p 14 :: IO Int16
      bottom <- peekByteOff p 16 :: IO Int16
      let rows = fromIntegral bottom - fromIntegral top  + 1
          cols = fromIntegral right  - fromIntegral left + 1
      pure $ if rows <= 0 || cols <= 0 then Nothing else Just (rows, cols)

-- | The console knows nothing about pixels. 'Nothing' here means the driver
-- falls back to the XTWINOPS pixel queries (which Windows Terminal answers),
-- and past that to the 2:1 cell assumption.
getTerminalPixels :: IO (Maybe (Int, Int))
getTerminalPixels = pure Nothing

-- | There is no SIGWINCH: poll the window rectangle and run the action when
-- it moves. 200 ms keeps a drag-resize feeling live at negligible cost (the
-- query is one memory read from the console server).
installResizeHandler :: IO () -> IO ()
installResizeHandler action = void $ forkIO $ do
  sz0 <- getTerminalSize
  let loop prev = do
        threadDelay 200000
        sz <- getTerminalSize
        if sz /= prev then action >> loop sz else loop prev
  loop sz0

-- | Convert console close/logoff/shutdown events into the same
-- 'UserInterrupt' the POSIX signal handlers throw, so the driver's bracketed
-- teardown (restore modes, leave the alt screen) runs before the process
-- dies. The short sleep keeps the handler alive while the main thread
-- unwinds — Windows terminates the process once the handler returns.
installInterruptHandlers :: ThreadId -> IO ()
installInterruptHandlers tid = do
  cb <- mkCtrlHandler $ \ev ->
    if ev == 2 || ev == 5 || ev == 6   -- CTRL_CLOSE / CTRL_LOGOFF / CTRL_SHUTDOWN
      then do
        throwTo tid UserInterrupt
        threadDelay 500000
        pure 1
      else pure 1  -- ^C/^Break arrive as input bytes in raw mode; never kill us
  -- The FunPtr is deliberately never freed: the console owns it for the
  -- lifetime of the process.
  void $ c_SetConsoleCtrlHandler cb 1

-- | Put stdin/stdout into the binary, unbuffered state the renderer and
-- input parser expect.
configureHandles :: IO ()
configureHandles = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout (BlockBuffering Nothing)
  hSetEcho stdin False

-- | What a directory-walk entry is — and, for files, its size. Symbolic
-- links (and junctions) are never followed ('EntryOther'), so tree walks
-- cannot loop.
data EntryStat = EntryDir | EntryFile !Integer | EntryOther

-- | Classify one directory entry. POSIX does this in a single @lstat@; here
-- it is a few queries against the directory package, which is fine — the
-- walkers are pooled background threads and NTFS metadata is cached.
-- 'Nothing' means the entry vanished or can't be stat'd.
statEntry :: FilePath -> IO (Maybe EntryStat)
statEntry path = handle (\(_ :: SomeException) -> pure Nothing) $ do
  isLink <- pathIsSymbolicLink path
  if isLink
    then pure (Just EntryOther)
    else do
      isDir <- doesDirectoryExist path
      if isDir
        then pure (Just EntryDir)
        else do
          isFile <- doesFileExist path
          if isFile
            then Just . EntryFile <$> getFileSize path
            else pure (Just EntryOther)
