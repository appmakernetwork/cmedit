-- | Cmedit entry point: parse the command line, then either print help/version
-- or launch the editor.
module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Text.Read (readMaybe)

import Cmedit.App (run, convertFiles)
import Cmedit.ConfigFile (Config(..), loadConfigFile)
import Cmedit.Help (helpString, versionString)

-- Accumulated command-line options.
data Options = Options
  { optConfig   :: Config
  , optFiles    :: [FilePath]
  , optReadOnly :: Bool
  , optStats    :: Bool
  , optConvert  :: Bool   -- ^ Force conversion even when stdout is a terminal.
  , optRestore  :: Bool   -- ^ Reopen the last session (folder + files), whatever else is named.
  , optSheet    :: Int    -- ^ Which sheet of a workbook to convert (1-based).
  , optHelp     :: Bool
  , optVersion  :: Bool
  , optError    :: Maybe String
  }

baseOptions :: Config -> Options
baseOptions cfg = Options
  { optConfig   = cfg
  , optFiles    = []
  , optReadOnly = False
  , optStats    = False
  , optConvert  = False
  , optRestore  = False
  , optSheet    = 1
  , optHelp     = False
  , optVersion  = False
  , optError    = Nothing
  }

main :: IO ()
main = do
  args <- getArgs
  -- The config file supplies the defaults; command-line flags override it.
  (fileCfg, cfgWarns) <- loadConfigFile
  let opts = parseArgs args (baseOptions fileCfg)
  case optError opts of
    Just err -> do
      hPutStrLn stderr ("cmedit: " ++ err)
      hPutStrLn stderr "Try 'cmedit --help' for more information."
      exitFailure
    Nothing
      | optHelp opts    -> putStr helpString >> exitSuccess
      | optVersion opts -> putStrLn versionString >> exitSuccess
      | otherwise       -> do
          -- The editor draws on stdout, so a redirected stdout cannot mean
          -- "open the editor" — it can only mean "give me the text". No flag
          -- is needed for `cmedit paper.pdf > paper.txt` to do the obvious
          -- thing; --convert is for forcing it at a terminal.
          tty <- hIsTerminalDevice stdout
          let files = reverse (optFiles opts)
          if optConvert opts || not tty
            then convert opts files
            else run (optConfig opts) cfgWarns files
                     (optReadOnly opts) (optStats opts) (optRestore opts)

-- Command-line conversion: the text on stdout, a line about it on stderr.
convert :: Options -> [FilePath] -> IO ()
convert opts files
  | null files = do
      hPutStrLn stderr "cmedit: nothing to convert \x2014 name a file to convert"
      hPutStrLn stderr "Try 'cmedit --help' for more information."
      exitFailure
  | otherwise = do
      ok <- convertFiles (optConfig opts) (optSheet opts) files
      if ok then exitSuccess else exitFailure

parseArgs :: [String] -> Options -> Options
parseArgs [] o = o
parseArgs (a : rest) o = case a of
  "-h"      -> parseArgs rest o { optHelp = True }
  "--help"  -> parseArgs rest o { optHelp = True }
  "-v"      -> parseArgs rest o { optVersion = True }
  "--version" -> parseArgs rest o { optVersion = True }
  "--tabs"  -> parseArgs rest o { optConfig = (optConfig o) { cfgTabsToSpaces = False } }
  "--spaces" -> parseArgs rest o { optConfig = (optConfig o) { cfgTabsToSpaces = True } }
  "--no-line-numbers" -> parseArgs rest o { optConfig = (optConfig o) { cfgLineNumbers = False } }
  "--line-numbers"    -> parseArgs rest o { optConfig = (optConfig o) { cfgLineNumbers = True } }
  "--no-auto-indent"  -> parseArgs rest o { optConfig = (optConfig o) { cfgAutoIndent = False } }
  "--readonly" -> parseArgs rest o { optReadOnly = True }
  "--stats-on-exit" -> parseArgs rest o { optStats = True }
  "--convert" -> parseArgs rest o { optConvert = True }
  "--restore" -> parseArgs rest o { optRestore = True }
  "--sheet" -> takeSheet rest o
  "-t" -> takeTabWidth rest o
  "--tab-width" -> takeTabWidth rest o
  _ | take 2 a == "--" -> o { optError = Just ("unknown option " ++ a) }
    | take 1 a == "-" && a /= "-" -> o { optError = Just ("unknown option " ++ a) }
    | otherwise -> parseArgs rest o { optFiles = a : optFiles o }

takeSheet :: [String] -> Options -> Options
takeSheet [] o = o { optError = Just "--sheet requires a number" }
takeSheet (n : rest) o = case readMaybe n of
  Just k | k >= 1 -> parseArgs rest o { optSheet = k, optConvert = True }
  _ -> o { optError = Just "--sheet expects a sheet number from 1" }

takeTabWidth :: [String] -> Options -> Options
takeTabWidth [] o = o { optError = Just "--tab-width requires an argument" }
takeTabWidth (n : rest) o = case readMaybe n of
  Just w | w >= 1 && w <= 16 -> parseArgs rest o { optConfig = (optConfig o) { cfgTabWidth = w } }
  _ -> o { optError = Just "--tab-width expects a number between 1 and 16" }
