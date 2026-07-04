-- | Cmedit entry point: parse the command line, then either print help/version
-- or launch the editor.
module Main (main) where

import Control.Monad (when)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Text.Read (readMaybe)

import Cmedit.App (run)
import Cmedit.ConfigFile (Config(..), loadConfigFile)
import Cmedit.Help (helpString, versionString)

-- Accumulated command-line options.
data Options = Options
  { optConfig   :: Config
  , optFiles    :: [FilePath]
  , optReadOnly :: Bool
  , optHelp     :: Bool
  , optVersion  :: Bool
  , optError    :: Maybe String
  }

baseOptions :: Config -> Options
baseOptions cfg = Options
  { optConfig   = cfg
  , optFiles    = []
  , optReadOnly = False
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
      | otherwise       -> run (optConfig opts) cfgWarns (reverse (optFiles opts)) (optReadOnly opts)

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
  "-t" -> takeTabWidth rest o
  "--tab-width" -> takeTabWidth rest o
  _ | take 2 a == "--" -> o { optError = Just ("unknown option " ++ a) }
    | take 1 a == "-" && a /= "-" -> o { optError = Just ("unknown option " ++ a) }
    | otherwise -> parseArgs rest o { optFiles = a : optFiles o }

takeTabWidth :: [String] -> Options -> Options
takeTabWidth [] o = o { optError = Just "--tab-width requires an argument" }
takeTabWidth (n : rest) o = case readMaybe n of
  Just w | w >= 1 && w <= 16 -> parseArgs rest o { optConfig = (optConfig o) { cfgTabWidth = w } }
  _ -> o { optError = Just "--tab-width expects a number between 1 and 16" }
