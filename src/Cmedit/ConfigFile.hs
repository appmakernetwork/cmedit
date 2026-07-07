-- | The user configuration file and the persisted recent-files list.
--
-- This is a leaf module (it imports nothing from Cmedit) so the pure model in
-- "Cmedit.Editor" can import it without cycles. The parsing is pure and unit
-- tested; only the small load/save helpers at the bottom do IO.
--
-- The config file lives at @~\/.config\/cmedit\/config@ (respecting
-- @XDG_CONFIG_HOME@) and holds @key = value@ lines; the recent-files list at
-- @~\/.config\/cmedit\/recent@ holds one @line:col:path@ entry per line
-- (1-based, most recent first) so re-opening a file restores the cursor.
module Cmedit.ConfigFile
  ( -- * Configuration
    Config(..)
  , ThemeName(..)
  , defaultConfig
  , parseConfigText
  , updateConfigText
  , configKeysHelp
  , configFilePath
  , loadConfigFile
    -- * Recent files
  , RecentEntry(..)
  , maxRecentEntries
  , parseRecentText
  , renderRecentText
  , recentFilePath
  , loadRecentFile
  , saveRecentFile
    -- * Find/replace input history
  , maxHistoryEntries
  , parseHistoryText
  , renderHistoryText
  , historyFilePath
  , loadHistoryFile
  , saveHistoryFile
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import Cmedit.Lint (LinterId, Linter(..), linters)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
  ( XdgDirectory(XdgConfig), createDirectoryIfMissing, doesFileExist
  , getXdgDirectory, renameFile )
import System.FilePath ((</>), takeDirectory)
import Text.Read (readMaybe)

------------------------------------------------------------------------------
-- Configuration

-- | The colour theme (interpreted by "Cmedit.Render"). 'ThemeAuto' follows
-- the terminal's background colour when the terminal reports it (the driver's
-- OSC 11 query), and falls back to dark where it doesn't. 'ThemeDark' and
-- 'ThemeLight' keep the terminal's own background (hence \"dark terminal\" /
-- \"light terminal\" in the UI); 'ThemeCherryBlossom' (light pink),
-- 'ThemeFlashbang' (blinding white) and 'ThemeMidnight' (deep navy) paint
-- their own background on every cell, so they never depend on the terminal's
-- palette.
data ThemeName = ThemeDark | ThemeLight | ThemeAuto | ThemeCherryBlossom
               | ThemeFlashbang | ThemeMidnight
  deriving (Eq, Show)

data Config = Config
  { cfgTabWidth       :: !Int
  , cfgTabsToSpaces   :: !Bool
  , cfgAutoIndent     :: !Bool
  , cfgWordWrap       :: !Bool
  , cfgLineNumbers    :: !Bool
  , cfgShowWhitespace :: !Bool
  , cfgTrimTrailingWs :: !Bool   -- ^ Strip trailing whitespace from each line on save.
  , cfgEnsureFinalNl  :: !Bool   -- ^ Make sure the file ends with a newline on save.
  , cfgFreezeHeader   :: !Bool   -- ^ CSV table view: pin the first row while scrolling (View ▸ Freeze Header Row toggles it per-session).
  , cfgTheme          :: !ThemeName
  , cfgLint           :: !Bool                -- ^ Master switch for external-linter diagnostics.
  , cfgLintOn         :: ![(LinterId, Bool)]  -- ^ Per-linter enable flags, one entry per 'Cmedit.Lint.linters' row.
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { cfgTabWidth       = 4
  , cfgTabsToSpaces   = False   -- real tabs by default (override with --spaces)
  , cfgAutoIndent     = True
  , cfgWordWrap       = False
  , cfgLineNumbers    = False   -- gutter hidden by default (override with --line-numbers)
  , cfgShowWhitespace = False
  , cfgTrimTrailingWs = False
  , cfgEnsureFinalNl  = False
  , cfgFreezeHeader   = True    -- spreadsheets almost always have a header row
  , cfgTheme          = ThemeAuto   -- follow the terminal background; dark when undetectable
  , cfgLint           = True
  , cfgLintOn         = [ (linId l, linDefaultOn l) | l <- linters ]
  }

-- | Apply a config file's text to a base config. Unknown keys and unparsable
-- values are reported as warnings (with their line number) rather than
-- aborting, so one bad line never takes the rest of the file down with it.
parseConfigText :: Text -> Config -> (Config, [String])
parseConfigText txt cfg0 = foldl step (cfg0, []) (zip [1 :: Int ..] (T.lines txt))
  where
    step (cfg, warns) (ln, raw) =
      let line = T.strip (T.takeWhile (/= '#') raw)
      in if T.null line
           then (cfg, warns)
           else case T.breakOn "=" line of
             (_, rhs) | T.null rhs ->
               (cfg, warns ++ [warn ln "expected 'key = value'"])
             (k, rhs) ->
               let key = T.unpack (T.strip k)
                   val = T.unpack (T.strip (T.drop 1 rhs))
               in case applyKey key val cfg of
                    Right cfg' -> (cfg', warns)
                    Left err   -> (cfg, warns ++ [warn ln err])
    warn ln msg = "line " ++ show ln ++ ": " ++ msg

applyKey :: String -> String -> Config -> Either String Config
applyKey key val cfg = case key of
  "tab-width" -> case readMaybe val of
    Just w | w >= 1 && w <= 16 -> Right cfg { cfgTabWidth = w }
    _ -> Left "tab-width expects a number between 1 and 16"
  "indent" -> case map toLower val of
    "tabs"   -> Right cfg { cfgTabsToSpaces = False }
    "spaces" -> Right cfg { cfgTabsToSpaces = True }
    _        -> Left "indent expects 'tabs' or 'spaces'"
  "auto-indent"  -> boolKey (\b -> cfg { cfgAutoIndent = b })
  "word-wrap"    -> boolKey (\b -> cfg { cfgWordWrap = b })
  "line-numbers" -> boolKey (\b -> cfg { cfgLineNumbers = b })
  "whitespace"   -> boolKey (\b -> cfg { cfgShowWhitespace = b })
  "trim-trailing-whitespace" -> boolKey (\b -> cfg { cfgTrimTrailingWs = b })
  "final-newline"            -> boolKey (\b -> cfg { cfgEnsureFinalNl = b })
  "freeze-header"            -> boolKey (\b -> cfg { cfgFreezeHeader = b })
  "theme" -> case map toLower val of
    "dark-terminal"  -> Right cfg { cfgTheme = ThemeDark }
    "dark"           -> Right cfg { cfgTheme = ThemeDark }   -- legacy spelling
    "light-terminal" -> Right cfg { cfgTheme = ThemeLight }
    "light"          -> Right cfg { cfgTheme = ThemeLight }  -- legacy spelling
    "auto"           -> Right cfg { cfgTheme = ThemeAuto }
    "cherry-blossom" -> Right cfg { cfgTheme = ThemeCherryBlossom }
    "cherryblossom"  -> Right cfg { cfgTheme = ThemeCherryBlossom }
    "cherry"         -> Right cfg { cfgTheme = ThemeCherryBlossom }
    "flashbang"      -> Right cfg { cfgTheme = ThemeFlashbang }
    "midnight"       -> Right cfg { cfgTheme = ThemeMidnight }
    _ -> Left "theme expects 'auto', 'dark-terminal', 'light-terminal', 'cherry-blossom', 'flashbang' or 'midnight'"
  "lint" -> boolKey (\b -> cfg { cfgLint = b })
  _ | Just suffix <- stripPrefix' "lint-" key
    , Just l <- lookupLinter suffix ->
        boolKey (\b -> cfg { cfgLintOn = setLintOn (linId l) b (cfgLintOn cfg) })
  _ -> Left ("unknown key '" ++ key ++ "'")
  where
    boolKey set = case parseBool val of
      Just b  -> Right (set b)
      Nothing -> Left (key ++ " expects true or false")
    stripPrefix' p s = if p `isPrefixOf` s then Just (drop (length p) s) else Nothing
    lookupLinter nm = case [ l | l <- linters, linName l == nm ] of
      (l : _) -> Just l
      []      -> Nothing
    setLintOn lid b = map (\(i, x) -> if i == lid then (i, b) else (i, x))

parseBool :: String -> Maybe Bool
parseBool s = case map toLower s of
  x | x `elem` ["true", "yes", "on", "1"]  -> Just True
    | x `elem` ["false", "no", "off", "0"] -> Just False
  _ -> Nothing

-- | The recognised keys and their meaning, for @--help@.
configKeysHelp :: [String]
configKeysHelp =
  [ "tab-width = N        Tab width in columns, 1-16 (default 4)."
  , "indent = tabs|spaces Indent with real tabs or spaces (default tabs)."
  , "auto-indent = BOOL   Copy indentation onto new lines (default true)."
  , "word-wrap = BOOL     Start with word wrap on (default false)."
  , "line-numbers = BOOL  Show the line-number gutter (default false)."
  , "whitespace = BOOL    Show whitespace markers (default false)."
  , "trim-trailing-whitespace = BOOL"
  , "                     Strip trailing spaces/tabs on save (default false)."
  , "final-newline = BOOL Ensure the file ends with a newline on save"
  , "                     (default false)."
  , "freeze-header = BOOL Pin a CSV table's first row while scrolling"
  , "                     (default true)."
  , "theme = auto|dark-terminal|light-terminal|cherry-blossom|flashbang|midnight"
  , "                     Colour theme; 'auto' follows the terminal"
  , "                     background (default dark). The terminal themes keep"
  , "                     the terminal's own background; cherry-blossom (light"
  , "                     pink), flashbang (bright white) and midnight (deep"
  , "                     navy) paint their own background colours."
  , "lint = BOOL          Run external linters on the active file (default"
  , "                     true). Per-linter switches: lint-ruff, lint-flake8,"
  , "                     lint-eslint, lint-stylelint, lint-pyright,"
  , "                     lint-shellcheck (each = on|off)."
  ]

------------------------------------------------------------------------------
-- Writing the config back

-- | The supported keys and how to render each one's value from a 'Config'. The
-- rendering must round-trip through 'applyKey' (bools as @on@\/@off@, @indent@
-- as @tabs@\/@spaces@, @theme@ as its canonical word), so the writer and parser
-- can't drift apart.
configFields :: [(Text, Config -> Text)]
configFields =
  [ ("tab-width",                \c -> T.pack (show (cfgTabWidth c)))
  , ("indent",                   \c -> if cfgTabsToSpaces c then "spaces" else "tabs")
  , ("auto-indent",              renderBool . cfgAutoIndent)
  , ("word-wrap",                renderBool . cfgWordWrap)
  , ("line-numbers",             renderBool . cfgLineNumbers)
  , ("whitespace",               renderBool . cfgShowWhitespace)
  , ("trim-trailing-whitespace", renderBool . cfgTrimTrailingWs)
  , ("final-newline",            renderBool . cfgEnsureFinalNl)
  , ("freeze-header",            renderBool . cfgFreezeHeader)
  , ("theme",                    renderTheme . cfgTheme)
  , ("lint",                     renderBool . cfgLint)
  ] ++
  [ ( T.pack ("lint-" ++ linName l)
    , \c -> renderBool (maybe (linDefaultOn l) id (lookup (linId l) (cfgLintOn c))) )
  | l <- linters ]

renderBool :: Bool -> Text
renderBool b = if b then "on" else "off"

renderTheme :: ThemeName -> Text
renderTheme t = case t of
  ThemeAuto          -> "auto"
  ThemeDark          -> "dark-terminal"
  ThemeLight         -> "light-terminal"
  ThemeCherryBlossom -> "cherry-blossom"
  ThemeFlashbang     -> "flashbang"
  ThemeMidnight      -> "midnight"

-- | Produce config-file text setting every key to @desired@, editing the given
-- current text as little as possible: a supported key already present has only
-- its value rewritten (leading indentation, the @=@ spacing and any trailing
-- @# comment@ are preserved, and every occurrence is updated since the parser
-- lets a later line win); comments, blank lines, unknown keys and malformed
-- lines pass through untouched. Keys absent from the file are appended at the
-- end only when their desired value differs from 'defaultConfig' (so a pristine
-- file isn't spammed with defaults), separated from existing content by one
-- blank line. It satisfies @fst (parseConfigText (updateConfigText c t)
-- defaultConfig) == c@.
updateConfigText :: Config -> Text -> Text
updateConfigText desired txt =
  let results = map (rewriteLine desired) (T.lines txt)
      body    = map fst results
      present = [ k | (_, Just k) <- results ]
      missing = [ k <> " = " <> render desired
                | (k, render) <- configFields
                , k `notElem` present
                , render desired /= render defaultConfig ]
  in if null missing
       then T.unlines body
       else let sep = [ "" | not (null body), not (isBlank (last body)) ]
            in T.unlines (body ++ sep ++ missing)
  where
    isBlank = T.null . T.strip

-- | Rewrite one line if it sets a supported key, returning the new line and the
-- key it set (so the caller knows which keys were present). Anything that isn't
-- a @supported-key = value@ line is returned verbatim.
rewriteLine :: Config -> Text -> (Text, Maybe Text)
rewriteLine cfg raw =
  let (code, comment) = T.break (== '#') raw
  in if T.null (T.strip code)
       then (raw, Nothing)
       else case T.breakOn "=" code of
         (_, rest) | T.null rest -> (raw, Nothing)          -- no '=', malformed
         (lhs, rest) ->
           case lookup (T.strip lhs) configFields of
             Nothing     -> (raw, Nothing)                  -- unknown key
             Just render ->
               let afterEq   = T.drop 1 rest                -- text after '='
                   (ws1, r1) = T.span isSpace afterEq       -- leading value spacing
                   trimmed   = T.stripEnd r1
                   ws2       = T.drop (T.length trimmed) r1 -- trailing spacing before comment
                   newCode   = lhs <> "=" <> ws1 <> render cfg <> ws2
               in (newCode <> comment, Just (T.strip lhs))

------------------------------------------------------------------------------
-- Recent files

-- | One remembered file: its path and the cursor position (0-based) it had
-- when last closed, so re-opening it puts the cursor back.
data RecentEntry = RecentEntry
  { rePath :: !FilePath
  , reLine :: !Int
  , reCol  :: !Int
  } deriving (Eq, Show)

-- | How many entries the recent-files list keeps (the File menu shows fewer).
maxRecentEntries :: Int
maxRecentEntries = 50

-- | Parse the recent file's contents: @line:col:path@ per line, 1-based, most
-- recent first. Malformed lines are skipped (the file is user-visible state,
-- not a format we can assume intact).
parseRecentText :: Text -> [RecentEntry]
parseRecentText txt =
  take maxRecentEntries
    [ e | raw <- T.lines txt, Just e <- [parseLine (T.strip raw)] ]
  where
    parseLine line
      | T.null line = Nothing
      | otherwise =
          let (lt, rest1) = T.breakOn ":" line
              (ct, rest2) = T.breakOn ":" (T.drop 1 rest1)
              path        = T.unpack (T.drop 1 rest2)
          in do l <- readMaybe (T.unpack lt)
                c <- readMaybe (T.unpack ct)
                if null path || T.null rest1 || T.null rest2
                  then Nothing
                  else Just (RecentEntry path (max 0 (l - 1)) (max 0 (c - 1)))

renderRecentText :: [RecentEntry] -> Text
renderRecentText entries = T.unlines
  [ T.pack (show (reLine e + 1) ++ ":" ++ show (reCol e + 1) ++ ":" ++ rePath e)
  | e <- take maxRecentEntries entries ]

------------------------------------------------------------------------------
-- Find / replace input history

-- | How many find/replace terms are remembered (each list).
maxHistoryEntries :: Int
maxHistoryEntries = 50

-- | Parse @~\/.config\/cmedit\/history@: @find <term>@ / @repl <term>@ lines,
-- newest first, with the term Haskell-string-escaped so multi-line terms
-- survive. Returns (find history, replace history).
parseHistoryText :: Text -> ([Text], [Text])
parseHistoryText txt =
  ( take maxHistoryEntries [ t | ("find", t) <- entries ]
  , take maxHistoryEntries [ t | ("repl", t) <- entries ] )
  where
    entries = [ (T.unpack kind, T.pack s)
              | line <- T.lines txt
              , let (kind, rest) = T.breakOn " " line
              , Just s <- [readMaybe (T.unpack (T.drop 1 rest)) :: Maybe String] ]

renderHistoryText :: [Text] -> [Text] -> Text
renderHistoryText finds repls = T.unlines $
  [ T.pack ("find " ++ show (T.unpack t)) | t <- take maxHistoryEntries finds ]
    ++ [ T.pack ("repl " ++ show (T.unpack t)) | t <- take maxHistoryEntries repls ]

historyFilePath :: IO FilePath
historyFilePath = (</> "history") <$> configDir

loadHistoryFile :: IO ([Text], [Text])
loadHistoryFile = do
  r <- try readIt :: IO (Either SomeException ([Text], [Text]))
  pure (either (const ([], [])) id r)
  where
    readIt = do
      path <- historyFilePath
      exists <- doesFileExist path
      if exists then parseHistoryText <$> TIO.readFile path else pure ([], [])

saveHistoryFile :: [Text] -> [Text] -> IO ()
saveHistoryFile finds repls = do
  _ <- try writeIt :: IO (Either SomeException ())
  pure ()
  where
    writeIt = do
      path <- historyFilePath
      createDirectoryIfMissing True (takeDirectory path)
      let tmp = path ++ ".tmp"
      TIO.writeFile tmp (renderHistoryText finds repls)
      renameFile tmp path

------------------------------------------------------------------------------
-- IO

configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig "cmedit"

-- | @~\/.config\/cmedit\/config@ (respecting @XDG_CONFIG_HOME@).
configFilePath :: IO FilePath
configFilePath = (</> "config") <$> configDir

-- | @~\/.config\/cmedit\/recent@.
recentFilePath :: IO FilePath
recentFilePath = (</> "recent") <$> configDir

-- | Load the user config, if present. Never fails: a missing file is the
-- default config, and IO/parse problems come back as warnings.
loadConfigFile :: IO (Config, [String])
loadConfigFile = do
  r <- try readIt :: IO (Either SomeException (Config, [String]))
  pure (either (const (defaultConfig, [])) id r)
  where
    readIt = do
      path <- configFilePath
      exists <- doesFileExist path
      if not exists
        then pure (defaultConfig, [])
        else do
          txt <- TIO.readFile path
          pure (parseConfigText txt defaultConfig)

-- | Load the recent-files list (empty on any problem).
loadRecentFile :: IO [RecentEntry]
loadRecentFile = do
  r <- try readIt :: IO (Either SomeException [RecentEntry])
  pure (either (const []) id r)
  where
    readIt = do
      path <- recentFilePath
      exists <- doesFileExist path
      if exists then parseRecentText <$> TIO.readFile path else pure []

-- | Persist the recent-files list (atomically, via a temp file). Failures are
-- swallowed: losing the recents list must never take the editor down.
saveRecentFile :: [RecentEntry] -> IO ()
saveRecentFile entries = do
  _ <- try writeIt :: IO (Either SomeException ())
  pure ()
  where
    writeIt = do
      path <- recentFilePath
      createDirectoryIfMissing True (takeDirectory path)
      let tmp = path ++ ".tmp"
      TIO.writeFile tmp (renderRecentText entries)
      renameFile tmp path
