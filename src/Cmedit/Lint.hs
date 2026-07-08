-- | External-linter integration: the pure, table-driven core. This module owns
-- the catalogue of supported linters (ruff, flake8, eslint, stylelint, pyright,
-- shellcheck), the extension routing, and the parsers that turn each tool's
-- textual output into 'Diag' records. It is a leaf module — it imports nothing
-- from Cmedit and does no IO — so "Cmedit.ConfigFile" (itself a leaf) can import
-- it without introducing a cycle. The driver ("Cmedit.App") does the actual
-- process running and feeds the captured stdout back through 'parseLintOutput';
-- the renderer draws the squiggles from 'diagSpans'.
module Cmedit.Lint
  ( LinterId(..), Linter(..), Severity(..), Diag(..), LintAvail
  , linters, linterById, lintersForPath
  , parseLintOutput, diagSpans, diagAt
  , maxDiagsPerFile
  ) where

import Data.Char (isAsciiUpper, isDigit, isSpace, isAlphaNum, toLower)
import Data.List (dropWhileEnd, elemIndices, intercalate, isInfixOf, isPrefixOf, sortOn, stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T

-- | The linters we know how to run and parse. 'Ord'/'Enum'/'Bounded' let the
-- config and settings UI iterate the whole set generically.
data LinterId = LRuff | LFlake8 | LEslint | LStylelint | LPyright | LShellcheck
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Diagnostic severity. The 'Ord' instance is deliberately most-severe-first
-- (@SevError < SevWarning < SevInfo@) so a plain sort surfaces errors.
data Severity = SevError | SevWarning | SevInfo deriving (Eq, Ord, Show)

-- | One diagnostic, positioned in the buffer (0-based) and attributed to the
-- tool that produced it. Positions may go stale between lint passes, so every
-- consumer must clamp before indexing a line.
data Diag = Diag
  { dgLine :: !Int          -- ^ 0-based buffer line
  , dgCol  :: !Int          -- ^ 0-based character column
  , dgSev  :: !Severity
  , dgCode :: !Text         -- ^ rule id ("E501", "no-unused-vars", "SC2034"); may be empty
  , dgMsg  :: !Text
  , dgTool :: !LinterId
  } deriving (Eq, Show)

-- | Resolved availability: @Nothing@ = not installed; @Just (exePath, versionLine)@.
type LintAvail = [(LinterId, Maybe (FilePath, String))]

-- | Static description of a linter: how to invoke it and how to surface it in
-- config / the settings dialog.
data Linter = Linter
  { linId        :: !LinterId
  , linName      :: !String              -- ^ config-key suffix + display name: "ruff"
  , linExts      :: ![String]            -- ^ lowercase, with dot: [".py"]
  , linCmd       :: !String              -- ^ executable to look up
  , linArgs      :: FilePath -> [String] -- ^ args given the display file path
  , linStdin     :: !Bool                -- ^ True: feed buffer on stdin (edit-time);
                                         --   False: runs on the saved disk file (save-time only)
  , linInstall   :: !String              -- ^ install hint: "pip install ruff"
  , linSupersededBy :: !(Maybe LinterId) -- ^ flake8 -> Just LRuff
  , linDefaultOn :: !Bool
  , linNodeTool  :: !Bool                -- ^ also look in <root>/node_modules/.bin,
                                         --   or run via <root>/.pnp.cjs (Yarn PnP)
  }

-- | The linter catalogue, in a fixed order the config writer and settings UI
-- both rely on.
linters :: [Linter]
linters =
  [ Linter { linId = LRuff, linName = "ruff", linExts = [".py", ".pyi"]
           , linCmd = "ruff"
           -- --force-exclude: honour the project's [tool.ruff] exclude list
           -- even for stdin input (by default ruff lints explicit files
           -- regardless of excludes).
           , linArgs = \p -> ["check", "--output-format", "concise", "--force-exclude", "--stdin-filename", p, "-"]
           , linStdin = True, linInstall = "pip install ruff"
           , linSupersededBy = Nothing, linDefaultOn = True, linNodeTool = False }
  , Linter { linId = LFlake8, linName = "flake8", linExts = [".py", ".pyi"]
           , linCmd = "flake8"
           , linArgs = \p -> ["--stdin-display-name", p, "-"]
           , linStdin = True, linInstall = "pip install flake8"
           , linSupersededBy = Just LRuff, linDefaultOn = True, linNodeTool = False }
  , Linter { linId = LEslint, linName = "eslint"
           , linExts = [".js", ".jsx", ".ts", ".tsx", ".mjs", ".cjs"]
           , linCmd = "eslint"
           -- stylish, not unix: eslint 9 removed the unix/compact formatters
           -- from core, and stylish is built into both 8 and 9. --no-color
           -- guards against a FORCE_COLOR environment defeating the parser.
           , linArgs = \p -> ["--format", "stylish", "--no-color", "--stdin", "--stdin-filename", p]
           , linStdin = True, linInstall = "npm install -D eslint"
           , linSupersededBy = Nothing, linDefaultOn = True, linNodeTool = True }
  , Linter { linId = LStylelint, linName = "stylelint"
           , linExts = [".css", ".scss", ".sass", ".less"]
           , linCmd = "stylelint"
           , linArgs = \p -> ["--stdin", "--stdin-filename", p, "--formatter", "unix"]
           , linStdin = True, linInstall = "npm install -D stylelint"
           , linSupersededBy = Nothing, linDefaultOn = True, linNodeTool = True }
  , Linter { linId = LPyright, linName = "pyright", linExts = [".py", ".pyi"]
           , linCmd = "pyright"
           , linArgs = \p -> [p]
           , linStdin = False, linInstall = "pip install pyright"
           , linSupersededBy = Nothing, linDefaultOn = False, linNodeTool = False }
  , Linter { linId = LShellcheck, linName = "shellcheck", linExts = [".sh", ".bash"]
           , linCmd = "shellcheck"
           , linArgs = \_ -> ["--format", "gcc", "-"]
           , linStdin = True, linInstall = "apt install shellcheck"
           , linSupersededBy = Nothing, linDefaultOn = True, linNodeTool = False }
  ]

-- | Total table lookup (the table covers every constructor).
linterById :: LinterId -> Linter
linterById i = head [ l | l <- linters, linId l == i ]

-- | The extension of a path, lowercased, including the leading dot ("" if none).
-- A local @takeExtension@ so the module stays free of System.FilePath: strip any
-- directory prefix, then take from the last dot of the basename (a leading dot,
-- as in @.bashrc@, is not an extension).
extOf :: FilePath -> String
extOf path =
  let base = reverse (takeWhile notSep (reverse path))
      notSep c = c /= '/' && c /= '\\'
  in case elemIndices '.' base of
       [] -> ""
       is -> let i = last is
             in if i == 0 then "" else map toLower (drop i base)

-- | The linters whose extension list matches the path (case-insensitive).
lintersForPath :: FilePath -> [Linter]
lintersForPath path = [ l | l <- linters, extOf path `elem` linExts l ]

-- | Never keep more than this many diagnostics per file.
maxDiagsPerFile :: Int
maxDiagsPerFile = 500

-- Parsing --------------------------------------------------------------------

-- | Parse a tool's stdout into diagnostics, sorted by (line, col), 1-based
-- positions converted to 0-based (clamped at 0), and capped at
-- 'maxDiagsPerFile'. The display path the tool was invoked with is not needed
-- for parsing (shellcheck reports the path as @-@; we accept any path field).
parseLintOutput :: LinterId -> FilePath -> Text -> [Diag]
parseLintOutput lid _path txt =
  take maxDiagsPerFile
    $ sortOn (\d -> (dgLine d, dgCol d))
    $ concatMap (parseLine lid) (map T.unpack (T.lines txt))

parseLine :: LinterId -> String -> [Diag]
parseLine LPyright s = maybe [] (: []) (parsePyright s)
parseLine LEslint s = maybe [] (: []) (parseEslintStylish s)
parseLine lid s =
  case scanGcc s of
    Nothing -> []
    Just (ln, col, rest) ->
      let (sev, code, msg) = interpret lid rest
      in [ mkDiag lid ln col sev code msg ]

-- | Build a diag, converting the 1-based (line, col) to 0-based (clamped).
mkDiag :: LinterId -> Int -> Int -> Severity -> String -> String -> Diag
mkDiag lid ln col sev code msg =
  Diag { dgLine = max 0 (ln - 1), dgCol = max 0 (col - 1)
       , dgSev = sev, dgCode = T.pack code, dgMsg = T.pack msg, dgTool = lid }

-- | Scan for the first @:\<digits\>:\<digits\>:@ (path prefixes may contain
-- colons on Windows, so we search for the numeric pattern rather than splitting
-- on the first colon). Returns (line, col, rest) with @rest@ the text after the
-- third colon and a single following space stripped.
scanGcc :: String -> Maybe (Int, Int, String)
scanGcc = go
  where
    go [] = Nothing
    go (':' : t) =
      case spanNum t of
        Just (ln, ':' : t2) ->
          case spanNum t2 of
            Just (col, ':' : t3) -> Just (ln, col, dropSp t3)
            _ -> go t
        _ -> go t
    go (_ : t) = go t
    dropSp (' ' : r) = r
    dropSp r = r

-- | Parse a leading run of digits (non-empty), returning the number and the
-- remaining string. The read is capped at nine digits so a pathological or
-- corrupted position in tool output cannot wrap 'Int'; downstream clamps make
-- an over-large (garbage) position land harmlessly at the buffer edge.
spanNum :: String -> Maybe (Int, String)
spanNum s = case span isDigit s of
  (ds@(_ : _), rest) -> Just (read (take 9 ds), rest)
  _ -> Nothing

-- | Interpret the message body ('rest') for each gcc-ish linter into
-- (severity, code, message).
interpret :: LinterId -> String -> (Severity, String, String)
interpret LRuff rest =
  case words rest of
    (c : rest2) | isCodeTok c ->
      let rest3 = case rest2 of ("[*]" : r) -> r; r -> r   -- skip a fixable marker
          msg = unwords rest3
          sev = if c == "E999" || "SyntaxError" `isInfixOf` msg then SevError else SevWarning
      in (sev, c, msg)
    _ -> (SevWarning, "", rest)
interpret LFlake8 rest =
  case words rest of
    (c : rest2) | isCodeTok c ->
      let msg = unwords rest2
          sev = if "E9" `isPrefixOf` c || "F8" `isPrefixOf` c then SevError else SevWarning
      in (sev, c, msg)
    _ -> (SevWarning, "", rest)
interpret LEslint rest = (SevWarning, "", rest)   -- unused: eslint has its own parser
interpret LStylelint rest =
  case splitTrailing '[' ']' rest of
    Just (before, inner) ->
      let (msg, code) = case splitTrailing '(' ')' before of
                          Just (m, c) -> (m, c)   -- trailing (rule-name) is the code
                          Nothing -> (before, "")
      in (sevFromWord inner, code, msg)
    Nothing -> (SevWarning, "", rest)
interpret LShellcheck rest =
  let (sevWord, afterColon) = break (== ':') rest
      msg0 = dropWhile isSpace (drop 1 afterColon)
      sev = case map toLower (dropWhileEnd isSpace sevWord) of
              "error" -> SevError
              "warning" -> SevWarning
              "note" -> SevInfo
              _ -> SevWarning
      (msg, code) = case splitTrailing '[' ']' msg0 of
                      Just (m, c) -> (m, c)
                      Nothing -> (msg0, "")
  in (sev, code, msg)
interpret LPyright rest = (SevWarning, "", rest)   -- unused: pyright has its own parser

-- | Pyright emits its own text format: @  /path/file.py:12:5 - error: Message@.
-- Trim leading spaces, find @:\<digits\>:\<digits\> - @, read the severity word
-- before the colon, and lift a trailing @(reportXxx)@ into the code.
parsePyright :: String -> Maybe Diag
parsePyright s0 =
  case scanPyright (dropWhile isSpace s0) of
    Nothing -> Nothing
    Just (ln, col, rest) ->
      let (sevWord, afterColon) = break (== ':') rest
      in if null afterColon
         then Nothing
         else
           let msg0 = dropWhile isSpace (drop 1 afterColon)
               sev = case map toLower (dropWhileEnd isSpace sevWord) of
                       "error" -> SevError
                       "warning" -> SevWarning
                       "information" -> SevInfo
                       _ -> SevWarning
               (msg, code) = case splitTrailing '(' ')' msg0 of
                               Just (m, c) | "report" `isPrefixOf` c -> (m, c)
                               _ -> (msg0, "")
           in Just (mkDiag LPyright ln col sev code msg)

-- | Scan for the first @:\<digits\>:\<digits\> - @, returning (line, col, rest).
scanPyright :: String -> Maybe (Int, Int, String)
scanPyright = go
  where
    go [] = Nothing
    go (':' : t) =
      case spanNum t of
        Just (ln, ':' : t2) ->
          case spanNum t2 of
            Just (col, t3) ->
              case stripPrefix " - " t3 of
                Just r -> Just (ln, col, r)
                Nothing -> go t
            _ -> go t
        _ -> go t
    go (_ : t) = go t

-- | eslint's @stylish@ formatter (the only machine-parsable format left in
-- core eslint 9 — @unix@ and @compact@ were removed): a file-path header
-- line, then one indented row per problem, columns padded to 2+ spaces:
--
-- >   660:7  warning  'x' is assigned a value but never used  no-unused-vars
--
-- The trailing rule id is absent for parsing errors, and a message can itself
-- contain a double space, so the last field only counts as the rule id when
-- it looks like one ('isRuleId'). Header, blank and summary ("2 problems")
-- lines all fail the leading @\<space\>...\<digits\>:\<digits\>@ shape and
-- parse to nothing.
parseEslintStylish :: String -> Maybe Diag
parseEslintStylish s0 = case s0 of
  (c0 : _) | isSpace c0 ->
    case spanNum (dropWhile isSpace s0) of
      Just (ln, ':' : t) ->
        case spanNum t of
          Just (col, rest) ->
            case splitWide (dropWhileEnd isSpace rest) of
              (sevWord : fs@(_ : _)) ->
                let (msg, code) = case reverse fs of
                      (lastF : preR@(_ : _)) | isRuleId lastF
                        -> (intercalate "  " (reverse preR), lastF)
                      _ -> (intercalate "  " fs, "")
                in Just (mkDiag LEslint ln col (sevFromWord sevWord) code msg)
              _ -> Nothing
          _ -> Nothing
      _ -> Nothing
  _ -> Nothing

-- | Split on runs of two or more spaces (stylish column padding); single
-- spaces stay inside a field. Wide runs at either end produce no empty
-- fields.
splitWide :: String -> [String]
splitWide = go . dropPad
  where
    dropPad = dropWhile (== ' ')
    go [] = []
    go s = let (f, r) = breakWide s in f : go (dropPad r)
    breakWide s@(' ' : ' ' : _) = ([], s)
    breakWide (c : r) = let (f, r2) = breakWide r in (c : f, r2)
    breakWide [] = ([], [])

-- | Does a field look like an eslint rule id (@semi@, @no-var@,
-- @\@typescript-eslint/no-unused-vars@)? Alphanumerics plus a little
-- punctuation and no spaces — a message tail has spaces, so this keeps real
-- messages out of the code slot.
isRuleId :: String -> Bool
isRuleId t = any isAlphaNum t && all ok t
  where ok c = isAlphaNum c || c `elem` ("@/-_+:." :: String)

-- | A leading run of ASCII uppercase letters followed by digits (e.g. "E501").
isCodeTok :: String -> Bool
isCodeTok c =
  let (as, bs) = span isAsciiUpper c
  in not (null as) && not (null bs) && all isDigit bs

-- | eslint/stylelint severity word: contains "error" → SevError, else warning.
sevFromWord :: String -> Severity
sevFromWord w
  | "error" `isInfixOf` map toLower w = SevError
  | otherwise = SevWarning

-- | If @s@ (trailing whitespace ignored) ends with the closing char, split it
-- into (text before the matching last opening char, inner text between them).
splitTrailing :: Char -> Char -> String -> Maybe (String, String)
splitTrailing open close s0 =
  let s = dropWhileEnd isSpace s0
  in if not (null s) && last s == close
     then case elemIndices open s of
            [] -> Nothing
            is -> let i = last is
                      inner = take (length s - i - 2) (drop (i + 1) s)
                  in Just (dropWhileEnd isSpace (take i s), inner)
     else Nothing

-- Span + query helpers -------------------------------------------------------

-- | Is this an identifier char (a squiggle extends over a run of them)?
isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

-- | The half-open column span @[s, e)@ a diag covers on its line, clamped into
-- @[0, length]@. If the char at the column is an identifier char the span runs
-- to the end of that identifier; otherwise it is a single char. A column past
-- the end spans the last char; an empty line yields @(0, 0)@ (the renderer skips
-- it).
spanOf :: Text -> Diag -> (Int, Int)
spanOf line d =
  let n = T.length line
      col = max 0 (dgCol d)
  in if n == 0 then (0, 0)
     else if col >= n then (n - 1, n)
     else let c = T.index line col
              e = if isIdentChar c
                  then col + T.length (T.takeWhile isIdentChar (T.drop col line))
                  else col + 1
          in (col, min n e)

-- | Underline spans for one buffer line's diags (caller pre-filters by line).
-- Sorted most-severe-first so a renderer taking the first covering span shows
-- the highest severity where spans overlap.
diagSpans :: Text -> [Diag] -> [(Int, Int, Severity)]
diagSpans line ds =
  sortOn (\(_, _, sev) -> sev)
    [ (s, e, dgSev d) | d <- ds, let (s, e) = spanOf line d ]

-- | The diag whose span covers (line, col) on the given line text; prefer the
-- most severe, then the leftmost.
diagAt :: Int -> Int -> Text -> [Diag] -> Maybe Diag
diagAt line col lineText ds =
  case sortOn (\d -> (dgSev d, dgCol d)) covering of
    (d : _) -> Just d
    [] -> Nothing
  where
    covering =
      [ d | d <- ds, dgLine d == line
          , let (s, e) = spanOf lineText d
          , s < e, col >= s, col < e ]
