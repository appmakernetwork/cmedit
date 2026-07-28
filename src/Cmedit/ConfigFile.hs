-- | The user configuration file and the persisted recent-files list.
--
-- This is a leaf module (it imports nothing from Cmedit) so the pure model in
-- "Cmedit.Editor" can import it without cycles. The parsing is pure and unit
-- tested; only the small load/save helpers at the bottom do IO.
--
-- The config file lives at @~\/.config\/cmedit\/config@ (respecting
-- @XDG_CONFIG_HOME@) and holds @key = value@ lines; the recent-files list at
-- @~\/.config\/cmedit\/recent@ holds one @line:col:path@ entry per line
-- (1-based, most recent first) so re-opening a file restores the cursor; the
-- sessions under @~\/.config\/cmedit\/sessions\/@ (one file per workspace
-- folder, plus the folderless @~\/.config\/cmedit\/session@) record what was
-- open last time.
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
    -- * Session (what @--restore@ reopens)
  , Session(..)
  , SessionFile(..)
  , SessionSummary(..)
  , maxSessionFiles
  , sessionDirMax
  , sessionVersion
  , splitLeadingFields
  , parseSessionText
  , renderSessionText
  , summarizeSession
  , newestSession
  , RestorePlan(..)
  , planRestore
  , sessionFilePath
  , sessionsDirPath
  , loadSessionFile
  , saveSessionFile
  , loadSessionFrom
  , saveSessionTo
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
-- 'ThemeFlashbang' (blinding white), 'ThemeMidnight' (deep navy) and
-- 'ThemeGraphite' (neutral near-black) paint their own background on every
-- cell, so they never depend on the terminal's palette.
data ThemeName = ThemeDark | ThemeLight | ThemeAuto | ThemeCherryBlossom
               | ThemeFlashbang | ThemeMidnight | ThemeGraphite
  deriving (Eq, Show)

data Config = Config
  { cfgTabWidth       :: !Int
  , cfgTabsToSpaces   :: !Bool
  , cfgAutoIndent     :: !Bool
  , cfgWordWrap       :: !Bool
  , cfgLineNumbers    :: !Bool
  , cfgShowWhitespace :: !Bool
  , cfgScrollBarV     :: !Bool   -- ^ Show the vertical scrollbar (the rightmost column is reserved only when on).
  , cfgScrollBarH     :: !Bool   -- ^ Show the horizontal scrollbar along the bottom of the text area.
  , cfgTrimTrailingWs :: !Bool   -- ^ Strip trailing whitespace from each line on save.
  , cfgEnsureFinalNl  :: !Bool   -- ^ Make sure the file ends with a newline on save.
  , cfgFreezeHeader   :: !Bool   -- ^ CSV table view: pin the first row while scrolling (View ▸ Freeze Header Row toggles it per-session).
  , cfgJournal        :: !Bool   -- ^ Journal unsaved changes to @~\/.cache\/cmedit\/journal@ so a crash can be recovered from (off for editing secrets).
  , cfgRestoreSession :: !Bool   -- ^ Reopen the last session (folder + files) when started with no arguments; @--restore@ forces it.
  , cfgTheme          :: !ThemeName
  , cfgPagedView      :: !Bool   -- ^ Offer the read-only paged view for files too large to load, instead of refusing them.
  , cfgDebugStats     :: !Bool   -- ^ Show live session counters (frames, heap, jobs) on the status bar.
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
  , cfgScrollBarV     = True
  , cfgScrollBarH     = True
  , cfgTrimTrailingWs = False
  , cfgEnsureFinalNl  = False
  , cfgFreezeHeader   = True    -- spreadsheets almost always have a header row
  , cfgJournal        = True    -- losing unsaved work is worse than a cache file
  , cfgRestoreSession = False   -- opt-in: a bare `cmedit` should stay a blank page
  , cfgTheme          = ThemeAuto   -- follow the terminal background; dark when undetectable
  , cfgPagedView      = True
  , cfgDebugStats     = False
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
  "scrollbar-vertical"   -> boolKey (\b -> cfg { cfgScrollBarV = b })
  "scrollbar-horizontal" -> boolKey (\b -> cfg { cfgScrollBarH = b })
  "trim-trailing-whitespace" -> boolKey (\b -> cfg { cfgTrimTrailingWs = b })
  "final-newline"            -> boolKey (\b -> cfg { cfgEnsureFinalNl = b })
  "freeze-header"            -> boolKey (\b -> cfg { cfgFreezeHeader = b })
  "journal"                  -> boolKey (\b -> cfg { cfgJournal = b })
  "restore-session"          -> boolKey (\b -> cfg { cfgRestoreSession = b })
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
    "graphite"       -> Right cfg { cfgTheme = ThemeGraphite }
    _ -> Left "theme expects 'auto', 'dark-terminal', 'light-terminal', 'cherry-blossom', 'flashbang', 'midnight' or 'graphite'"
  "paged-view" -> boolKey (\b -> cfg { cfgPagedView = b })
  "debug-stats" -> boolKey (\b -> cfg { cfgDebugStats = b })
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
  , "scrollbar-vertical = BOOL"
  , "                     Show the vertical scrollbar (default true)."
  , "scrollbar-horizontal = BOOL"
  , "                     Show the horizontal scrollbar (default true)."
  , "trim-trailing-whitespace = BOOL"
  , "                     Strip trailing spaces/tabs on save (default false)."
  , "final-newline = BOOL Ensure the file ends with a newline on save"
  , "                     (default false)."
  , "freeze-header = BOOL Pin a CSV table's first row while scrolling"
  , "                     (default true)."
  , "journal = BOOL       Cache buffer contents under ~/.cache/cmedit for"
  , "                     crash recovery and session snapshots (default true)."
  , "                     Unsaved changes are journalled as you type and"
  , "                     offered back the next time cmedit starts; a clean"
  , "                     exit also snapshots every open document, so a"
  , "                     restored session can offer the files as you left"
  , "                     them when they have changed on disk since. Both"
  , "                     hold file content, so set it to off when editing"
  , "                     secrets."
  , "restore-session = BOOL"
  , "                     Reopen this directory's session (folder and files)"
  , "                     when cmedit is started with no arguments (default"
  , "                     false). --restore does the same on demand."
  , "theme = auto|dark-terminal|light-terminal|cherry-blossom|flashbang|"
  , "        midnight|graphite"
  , "                     Colour theme; 'auto' follows the terminal"
  , "                     background (default dark). The terminal themes keep"
  , "                     the terminal's own background; cherry-blossom (light"
  , "                     pink), flashbang (bright white), midnight (deep"
  , "                     navy) and graphite (neutral near-black) paint their"
  , "                     own background colours."
  , "lint = BOOL          Run external linters on the active file (default"
  , "                     true). Per-linter switches: lint-ruff, lint-flake8,"
  , "                     lint-eslint, lint-stylelint, lint-pyright,"
  , "                     lint-shellcheck (each = on|off)."
  , "paged-view = BOOL    Open files too large to edit in a read-only paged"
  , "                     viewer instead of refusing them (default true)."
  , "debug-stats = BOOL   Show live session counters (frame time, heap, jobs)"
  , "                     on the status bar (default off). For a one-shot"
  , "                     summary instead, run with --stats-on-exit."
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
  , ("scrollbar-vertical",       renderBool . cfgScrollBarV)
  , ("scrollbar-horizontal",     renderBool . cfgScrollBarH)
  , ("trim-trailing-whitespace", renderBool . cfgTrimTrailingWs)
  , ("final-newline",            renderBool . cfgEnsureFinalNl)
  , ("freeze-header",            renderBool . cfgFreezeHeader)
  , ("journal",                  renderBool . cfgJournal)
  , ("restore-session",          renderBool . cfgRestoreSession)
  , ("theme",                    renderTheme . cfgTheme)
  , ("paged-view",               renderBool . cfgPagedView)
  , ("debug-stats",              renderBool . cfgDebugStats)
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
  ThemeGraphite      -> "graphite"

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
    [ e | raw <- T.lines txt, Just e <- [parseRecentLine (T.strip raw)] ]

-- | Split a @f1:f2:\@\<k fields\>:path@ line: exactly @k@ colon-separated
-- leading fields, and /everything/ after the k-th colon is the path.
--
-- This is the one property the recents encoding has that must not be lost — a
-- POSIX filename may contain colons, so only the first @k@ of them separate.
-- Two callers: the recents and v1 sessions pass @k = 2@ (@line:col:path@), v2
-- sessions pass @k = 3@ (@line:col:mtime:path@). A third fixed field before the
-- path keeps the property; appending one after the path would destroy it.
splitLeadingFields :: Int -> Text -> Maybe ([Text], FilePath)
splitLeadingFields k0 line0 = go k0 line0 []
  where
    go 0 rest acc =
      let p = T.unpack rest
      in if null p then Nothing else Just (reverse acc, p)
    go n t acc = case T.breakOn ":" t of
      (_, r) | T.null r -> Nothing              -- fewer than k colons
      (f, r)            -> go (n - 1 :: Int) (T.drop 1 r) (f : acc)

-- | One @line:col:path@ entry (1-based on disk, 0-based in the record).
parseRecentLine :: Text -> Maybe RecentEntry
parseRecentLine line
  | T.null line = Nothing
  | otherwise = do
      ([lt, ct], path) <- splitLeadingFields 2 line
      l <- readMaybe (T.unpack lt)
      c <- readMaybe (T.unpack ct)
      pure (RecentEntry path (max 0 (l - 1)) (max 0 (c - 1)))

renderRecentLine :: RecentEntry -> Text
renderRecentLine e =
  T.pack (show (reLine e + 1) ++ ":" ++ show (reCol e + 1) ++ ":" ++ rePath e)

renderRecentText :: [RecentEntry] -> Text
renderRecentText entries =
  T.unlines (map renderRecentLine (take maxRecentEntries entries))

------------------------------------------------------------------------------
-- Session
--
-- What was open last time, so @--restore@ (or @restore-session = on@) can put
-- it back. The file is rewritten whenever the session's /shape/ changes and
-- once more on the way out — the same discipline as the recents list, and for
-- a sharper reason: a session that is only written on a clean exit is exactly
-- the one a SIGKILL loses, which is the case the crash journal exists for.
--
-- It records paths, not content. Untitled buffers are deliberately absent:
-- there is nothing to reopen, and their content is the journal's business.

-- | One open document as a session records it: its path, the cursor it had,
-- and the on-disk timestamp the session last saw for it.
--
-- The timestamp is exact picoseconds since the Unix epoch
-- ('Cmedit.Journal.diskTimeToPicos'), not a decimal, because the question it
-- exists to answer is @recorded == current@ — and a lossy rendering makes that
-- answer \"changed\" for a file nobody touched on any filesystem with
-- sub-second timestamps. 'Nothing' (written @-@) is a file with no baseline:
-- named but never created, or recorded by a v1 session that had no such field.
-- It means \"no change detection for this one\", never \"unchanged\".
data SessionFile = SessionFile
  { sfPath  :: !FilePath
  , sfLine  :: !Int                -- ^ 0-based, like 'RecentEntry'.
  , sfCol   :: !Int                -- ^ 0-based.
  , sfMtime :: !(Maybe Integer)    -- ^ Picoseconds since the epoch, or 'Nothing'.
  } deriving (Eq, Show)

-- | The open documents (in zipper order, with their cursors), which of them
-- was active, the workspace folder if one was open, and when the file was
-- last written.
data Session = Session
  { seFolder :: !(Maybe FilePath)  -- ^ The open workspace folder, if any.
  , seFiles  :: ![SessionFile]     -- ^ Open documents in order, with cursor positions.
  , seActive :: !Int               -- ^ Index into 'seFiles' of the active document.
  , seClosed :: !(Maybe Integer)
    -- ^ When this file was last written, in picoseconds since the epoch
    -- ('Nothing' for a v1 file, which had no such field). Written on /every/
    -- write, not only at exit: for a session that ended, the exit write is the
    -- last one and it reads as \"closed at\"; for one that was killed it is the
    -- last shape change, which is the honest answer and the same thing every
    -- other field in the file is saying. It is /recorded/ rather than read back
    -- off the filesystem, so the menu's ordering survives an @rsync@ or a
    -- restore of @~\/.config@, which would flatten every mtime into one instant.
  } deriving (Eq, Show)

-- | How many open files a session records. A session larger than this is a
-- runaway, not a workspace, and restoring it would be slower than opening the
-- files by hand.
maxSessionFiles :: Int
maxSessionFiles = 50

-- | How many per-workspace session files @~\/.config\/cmedit\/sessions@ keeps
-- ('maxRecentEntries'' number, for the same reason). Evicted oldest-'seClosed'
-- first on write; the folderless @session@ file is not in that directory and is
-- therefore never touched by the cap.
sessionDirMax :: Int
sessionDirMax = 50

-- | The format version written. v1 (plan 0025) is still /read/ — see
-- 'parseSessionText'.
sessionVersion :: Int
sessionVersion = 2

-- | Render a session (always at 'sessionVersion'). The first line is the
-- version, so a past or future format can be told from this one instead of
-- being half-understood.
renderSessionText :: Session -> Text
renderSessionText s = T.unlines $
  [ T.pack ("cmedit-session " ++ show sessionVersion) ]
    ++ [ T.pack ("folder: " ++ f) | Just f <- [seFolder s] ]
    ++ [ T.pack ("closed: " ++ show c) | Just c <- [seClosed s] ]
    ++ [ T.pack ("active: " ++ show (seActive s)) ]
    ++ map renderSessionLine (take maxSessionFiles (seFiles s))

renderSessionLine :: SessionFile -> Text
renderSessionLine f =
  T.pack (show (sfLine f + 1) ++ ":" ++ show (sfCol f + 1) ++ ":"
          ++ maybe "-" show (sfMtime f) ++ ":" ++ sfPath f)

-- | Parse a session entry line at the given format version: @k = 2@ for v1
-- (@line:col:path@) and @k = 3@ for v2 (@line:col:mtime:path@), through the one
-- 'splitLeadingFields'.
parseSessionLine :: Int -> Text -> Maybe SessionFile
parseSessionLine 1 line = do
  ([lt, ct], path) <- splitLeadingFields 2 line
  l <- readMaybe (T.unpack lt)
  c <- readMaybe (T.unpack ct)
  pure (SessionFile path (max 0 (l - 1)) (max 0 (c - 1)) Nothing)
parseSessionLine _ line = do
  ([lt, ct, mt], path) <- splitLeadingFields 3 line
  l <- readMaybe (T.unpack lt)
  c <- readMaybe (T.unpack ct)
  m <- if mt == T.pack "-" then Just Nothing
                           else Just <$> readMaybe (T.unpack mt)
  pure (SessionFile path (max 0 (l - 1)) (max 0 (c - 1)) m)

-- | Parse the session file. Unknown lines are skipped the way the config
-- parser skips them (this is user-visible state on disk, not a format we can
-- assume intact); an unknown version means \"no session\" rather than a
-- guess, since a later format's lines would only look like malformed ones.
--
-- The version line governs, as it always has. A v1 file parses exactly as it
-- did before with every 'sfMtime' 'Nothing' — so no file is ever reported
-- changed and a first restore after the upgrade behaves precisely like a 0025
-- restore, which is the right outcome for a session recorded by a binary that
-- did not know what to record.
parseSessionText :: Text -> Maybe Session
parseSessionText txt = case dropWhile (T.null . T.strip) rawLines of
  (v : rest) | Just ver <- versionOf v, ver `elem` supportedVersions ->
      Just (finish (foldl (step ver) (Nothing, [], 0, Nothing) rest))
  _ -> Nothing
  where
    supportedVersions = [1, sessionVersion]
    rawLines = map (T.strip . T.dropWhileEnd (== '\r')) (T.lines txt)
    versionOf line = case T.words line of
      ["cmedit-session", n] -> readMaybe (T.unpack n) :: Maybe Int
      _                     -> Nothing
    step ver acc@(folder, files, active, closed) line
      | T.null line = acc
      | Just f <- T.stripPrefix "folder:" line =
          -- An empty value is no folder, not a folder named "".
          ( if T.null (T.strip f) then Nothing else Just (T.unpack (T.strip f))
          , files, active, closed )
      | Just a <- T.stripPrefix "active:" line =
          (folder, files, maybe active id (readMaybe (T.unpack (T.strip a))), closed)
      | Just c <- T.stripPrefix "closed:" line =
          (folder, files, active, readMaybe (T.unpack (T.strip c)) `orElse` closed)
      | Just e <- parseSessionLine ver line = (folder, e : files, active, closed)
      | otherwise = acc
    orElse (Just x) _ = Just x
    orElse Nothing  y = y
    finish (folder, files, active, closed) =
      Session folder (take maxSessionFiles (reverse files)) active closed

-- | What the File menu needs to know about one session file without reading
-- its contents twice: which file it is, whose folder it describes, how many
-- documents it recorded, and when it was written.
--
-- Deliberately not the file list: the menu shows a label, and the /restore/
-- re-reads the file at the moment it acts, so a session another instance has
-- rewritten since the listing is honoured rather than remembered.
data SessionSummary = SessionSummary
  { sumFile   :: !FilePath          -- ^ The session file's path (what a restore reads).
  , sumFolder :: !(Maybe FilePath)  -- ^ Its workspace folder; 'Nothing' is the folderless session.
  , sumCount  :: !Int               -- ^ How many documents it recorded.
  , sumClosed :: !(Maybe Integer)   -- ^ Its 'seClosed' stamp (orders the menu).
  } deriving (Eq, Show)

summarizeSession :: FilePath -> Session -> SessionSummary
summarizeSession path s =
  SessionSummary path (seFolder s) (length (seFiles s)) (seClosed s)

-- | The most recently written session of any kind — what @--restore@ falls back
-- to when the current directory has no session of its own (plan 0030 §2.3 step
-- 2). That fallback is what preserves the muscle memory 0025 built: a user who
-- has always typed @cmedit --restore@ from their home directory keeps getting
-- their last session, and the status line names which folder it came from.
--
-- An unstamped v1 file ('Nothing') loses to anything that knows when it was
-- written, and only wins when it is all there is.
newestSession :: [SessionSummary] -> Maybe SessionSummary
newestSession [] = Nothing
newestSession xs = Just (foldr1 newer xs)
  where newer a b = if sumClosed b > sumClosed a then b else a

-- | What a restore should actually do, given which recorded files are still
-- there. Separated from the IO so the index arithmetic — the only part that
-- can be wrong — is a pure function with a test.
data RestorePlan = RestorePlan
  { rpFiles    :: ![SessionFile]  -- ^ The files to open, in session order.
  , rpActive   :: !Int            -- ^ Index into 'rpFiles' of the one to make active.
  , rpRecorded :: !Int            -- ^ How many files the session recorded (for \"4 of 5\").
  } deriving (Eq, Show)

-- | Plan a restore: @exists@ carries one flag per 'seFiles' entry, in order
-- (a short list is treated as \"the rest are gone\").
planRestore :: [Bool] -> Session -> RestorePlan
planRestore exists s = RestorePlan kept active (length files)
  where
    files  = seFiles s
    tagged = zip files (exists ++ repeat False)
    kept   = [ e | (e, True) <- tagged ]
    -- The recorded index addresses the list as it was; after the missing files
    -- are dropped it has to address the list as it is. Counting the survivors
    -- ahead of it does that, and when the active file is itself gone the count
    -- lands on the next survivor — the nearest thing to where the user left.
    want   = max 0 (min (length files - 1) (seActive s))
    active = max 0 (min (length kept - 1) (length (filter snd (take want tagged))))

-- | @~\/.config\/cmedit\/session@ — the session for a run with /no workspace
-- folder open/, and the read fallback for anything written before plan 0030.
-- Deliberately unmoved: a folderless session is a real session, it needs a key,
-- and the file 0025 already writes is the obvious key for it.
sessionFilePath :: IO FilePath
sessionFilePath = (</> "session") <$> configDir

-- | @~\/.config\/cmedit\/sessions@ — one file per workspace folder, named by
-- 'Cmedit.Journal.sessionFileName' so a given folder's session file can be
-- /computed/ rather than searched for.
sessionsDirPath :: IO FilePath
sessionsDirPath = (</> "sessions") <$> configDir

-- | Load the folderless session, if there is a readable one.
loadSessionFile :: IO (Maybe Session)
loadSessionFile = sessionFilePath >>= loadSessionFrom

-- | Persist the folderless session.
saveSessionFile :: Session -> IO ()
saveSessionFile s = sessionFilePath >>= \p -> saveSessionTo p s

-- | Load a session from a named file (missing, unreadable or unparsable ⇒
-- 'Nothing' — never an exception, because this runs on the startup path).
loadSessionFrom :: FilePath -> IO (Maybe Session)
loadSessionFrom path = do
  r <- try readIt :: IO (Either SomeException (Maybe Session))
  pure (either (const Nothing) id r)
  where
    readIt = do
      exists <- doesFileExist path
      if exists then parseSessionText <$> TIO.readFile path else pure Nothing

-- | Persist a session to a named file (atomically, via a temp file). Failures
-- are swallowed, like the recents: losing the session must never take the
-- editor down.
saveSessionTo :: FilePath -> Session -> IO ()
saveSessionTo path s = do
  _ <- try writeIt :: IO (Either SomeException ())
  pure ()
  where
    writeIt = do
      createDirectoryIfMissing True (takeDirectory path)
      let tmp = path ++ ".tmp"
      TIO.writeFile tmp (renderSessionText s)
      renameFile tmp path

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
