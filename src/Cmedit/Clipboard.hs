-- | System clipboard integration. Copy/paste go through whichever external
-- helper is available (@wl-copy@/@wl-paste@ on Wayland, @xclip@ or @xsel@ on
-- X11, @pbcopy@/@pbpaste@ on macOS, @clip.exe@/PowerShell's @Get-Clipboard@
-- on Windows). When no helper works we fall back to an OSC 52 escape
-- sequence, which many terminals honour even over SSH.
module Cmedit.Clipboard
  ( CopyOutcome(..)
  , copyToClipboard
  , pasteFromClipboard
  , osc52Copy
  ) where

import Control.Exception (SomeException, handle)
import Data.ByteString.Builder (Builder, char7, string7)
import Data.Word (Word8)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.IO
import System.Process
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Text (Text)

-- | What happened when we tried to copy.
data CopyOutcome
  = CopiedSystem   -- ^ An external helper accepted the text.
  | UseOsc52       -- ^ No helper available; caller should emit 'osc52Copy'.
  | CopyFailed     -- ^ A helper was tried but failed.
  deriving (Eq, Show)

-- | Copy text to the system clipboard, trying each available backend in
-- priority order. Returns 'UseOsc52' if nothing is installed so the caller can
-- fall back to the escape-sequence method.
copyToClipboard :: Text -> IO CopyOutcome
copyToClipboard txt = do
  backends <- detectCopyBackends
  go backends
  where
    go [] = pure UseOsc52
    go ((cmd, args) : rest) = do
      ok <- runCopy cmd args txt
      if ok then pure CopiedSystem else go rest

-- | Read text from the system clipboard, or 'Nothing' if no backend works.
pasteFromClipboard :: IO (Maybe Text)
pasteFromClipboard = do
  backends <- detectPasteBackends
  go backends
  where
    go [] = pure Nothing
    go ((cmd, args) : rest) = do
      r <- runPaste cmd args
      case r of
        Just t  -> pure (Just t)
        Nothing -> go rest

------------------------------------------------------------------------------
-- Backend selection

detectCopyBackends :: IO [(String, [String])]
detectCopyBackends = do
  wayland <- isSet "WAYLAND_DISPLAY"
  x11     <- isSet "DISPLAY"
  candidates $
       [ ("clip", []) | windows ]        -- clip.exe reads stdin (UTF-8 via our 65001 code page)
    ++ [ ("wl-copy", []) | wayland ]
    ++ [ ("xclip", ["-selection", "clipboard"]) | x11 ]
    ++ [ ("xsel", ["--clipboard", "--input"])   | x11 ]
    ++ [ ("pbcopy", []) ]
    ++ [ ("wl-copy", []) | not wayland ]   -- last-ditch even without the env var

detectPasteBackends :: IO [(String, [String])]
detectPasteBackends = do
  wayland <- isSet "WAYLAND_DISPLAY"
  x11     <- isSet "DISPLAY"
  candidates $
       [ ("powershell", ["-NoProfile", "-Command", "Get-Clipboard -Raw"]) | windows ]
    ++ [ ("wl-paste", ["--no-newline"]) | wayland ]
    ++ [ ("xclip", ["-selection", "clipboard", "-o"]) | x11 ]
    ++ [ ("xsel", ["--clipboard", "--output"])        | x11 ]
    ++ [ ("pbpaste", []) ]
    ++ [ ("wl-paste", ["--no-newline"]) | not wayland ]

-- Keep only candidates whose executable is actually on PATH.
candidates :: [(String, [String])] -> IO [(String, [String])]
candidates = go
  where
    go [] = pure []
    go (c@(cmd, _) : rest) = do
      found <- findExecutable cmd
      others <- go rest
      pure $ case found of
        Just _  -> c : others
        Nothing -> others

isSet :: String -> IO Bool
isSet name = maybe False (not . null) <$> lookupEnv name

windows :: Bool
windows = os == "mingw32"

------------------------------------------------------------------------------
-- Process plumbing

runCopy :: String -> [String] -> Text -> IO Bool
runCopy cmd args txt = handle onErr $ do
  (mIn, _, _, ph) <- createProcess (proc cmd args)
    { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  case mIn of
    Nothing  -> pure False
    Just hin -> do
      hSetBinaryMode hin True
      BS.hPutStr hin (TE.encodeUtf8 txt)
      hClose hin
      ec <- waitForProcess ph
      pure (ec == ExitSuccess)
  where
    onErr :: SomeException -> IO Bool
    onErr _ = pure False

runPaste :: String -> [String] -> IO (Maybe Text)
runPaste cmd args = handle onErr $ do
  (_, mOut, _, ph) <- createProcess (proc cmd args)
    { std_out = CreatePipe, std_err = CreatePipe }
  case mOut of
    Nothing   -> pure Nothing
    Just hout -> do
      hSetBinaryMode hout True
      bs <- BS.hGetContents hout
      _  <- waitForProcess ph
      pure (Just (TE.decodeUtf8With TEE.lenientDecode bs))
  where
    onErr :: SomeException -> IO (Maybe Text)
    onErr _ = pure Nothing

------------------------------------------------------------------------------
-- OSC 52 fallback

-- | Build an OSC 52 sequence that sets the clipboard to the given text.
osc52Copy :: Text -> Builder
osc52Copy txt =
  char7 '\ESC' <> string7 "]52;c;"
    <> string7 (base64 (BS.unpack (TE.encodeUtf8 txt)))
    <> char7 '\BEL'

-- A minimal, dependency-free base64 encoder.
base64 :: [Word8] -> String
base64 = go
  where
    go (a : b : c : rest) =
      let n = (fromIntegral a `shiftL3` 16) + (fromIntegral b `shiftL3` 8) + fromIntegral c
      in enc (n `div` 262144 `mod` 64)
       : enc (n `div` 4096   `mod` 64)
       : enc (n `div` 64     `mod` 64)
       : enc (n              `mod` 64)
       : go rest
    go [a, b] =
      let n = (fromIntegral a * 65536) + (fromIntegral b * 256)
      in [ enc (n `div` 262144 `mod` 64)
         , enc (n `div` 4096   `mod` 64)
         , enc (n `div` 64     `mod` 64)
         , '=' ]
    go [a] =
      let n = fromIntegral a * 65536
      in [ enc (n `div` 262144 `mod` 64)
         , enc (n `div` 4096   `mod` 64)
         , '=', '=' ]
    go [] = []
    enc :: Int -> Char
    enc i = alphabet !! i
    alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/"
    shiftL3 x k = x * (2 ^ (k :: Int))
