-- | Go-to-definition: pure, language-aware detection of the lines where a
-- named function / class / symbol is /defined/ (as opposed to merely used),
-- plus the state of the definition-picker dialog.
--
-- The model is deliberately simple and fast — no parsing. For a clicked
-- identifier we find its word-bounded occurrences on a line (the same linear
-- scan the workspace search uses) and then check the immediate /context/
-- around each occurrence against a per-language table of definition shapes
-- ("the last word before it is @def@", "the line starts @CREATE … FUNCTION@",
-- …). That is the ctags/Sublime level of fidelity: cheap enough to run as a
-- streaming scan over a whole workspace, and right in practice.
--
-- Like "Cmedit.Menu" and "Cmedit.Dialog", this module is Editor-independent
-- data + pure helpers: "Cmedit.Editor" interprets what picking an item means,
-- and the IO walk lives in "Cmedit.App".
module Cmedit.Definition
  ( -- * Languages
    DefLang(..)
  , langOf
  , defExtensionGlobs
    -- * Definition detection
  , defLineCols
    -- * The scan request (Editor -> driver)
  , DefReq(..)
    -- * Picker-dialog state
  , DefItem(..)
  , DefPick(..)
  , newDefPick
  , dpAddItems
  , dpMoveSel
  , dpSelTo
  , dpClampScroll
  ) where

import Data.Char (isAlphaNum, toLower)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Cmedit.Search as S

------------------------------------------------------------------------------
-- Languages

-- | Languages the definition scan understands. JS covers the whole JS/TS
-- family (the definition shapes are the same).
data DefLang = LPython | LJs | LSql | LHaskell | LShell | LRuby | LGo | LPhp
  deriving (Eq, Show)

-- | The language of a file, by extension; 'Nothing' = not scanned.
langOf :: FilePath -> Maybe DefLang
langOf path = lookup (map toLower (extOf path)) extLangs

-- Extension (without the dot) of a path's basename; "" when none (or for a
-- dotfile). Works on the reversed basename so the LAST dot splits.
extOf :: FilePath -> String
extOf p = case break (== '.') (takeWhile (/= '/') (reverse p)) of
  (revExt, '.' : _ : _) -> reverse revExt
  _                     -> ""

extLangs :: [(String, DefLang)]
extLangs =
  [ ("py", LPython), ("pyw", LPython)
  , ("js", LJs), ("jsx", LJs), ("ts", LJs), ("tsx", LJs)
  , ("mjs", LJs), ("cjs", LJs), ("vue", LJs)
  , ("sql", LSql), ("psql", LSql), ("pgsql", LSql)
  , ("hs", LHaskell)
  , ("sh", LShell), ("bash", LShell)
  , ("rb", LRuby)
  , ("go", LGo)
  , ("php", LPhp)
  ]

-- | Include globs for the workspace walk: only file formats we can detect
-- definitions in are read at all.
defExtensionGlobs :: [String]
defExtensionGlobs = [ "*." ++ e | (e, _) <- extLangs ]

------------------------------------------------------------------------------
-- Definition detection

-- | @(startCol, length)@ of each place @name@ is /defined/ on this line.
-- Word-bounded occurrences of the name are found first (linear), then each is
-- kept only when its surrounding context looks like a definition for the
-- language. SQL identifiers compare case-insensitively.
defLineCols :: DefLang -> Text -> Text -> [(Int, Int)]
defLineCols lang name line =
  [ (i, len)
  | (i, len) <- S.lineMatches (lang /= LSql) True name line
  , isDefAt lang name (T.take i line) (T.drop (i + len) line) line
  ]

isWordCh :: Char -> Bool
isWordCh c = isAlphaNum c || c == '_'

-- @kw@ appears at the start of @t@ as a whole word.
wordPrefix :: Text -> Text -> Bool
wordPrefix kw t =
  T.isPrefixOf kw t
    && (T.length t == T.length kw || not (isWordCh (T.index t (T.length kw))))

-- Does an occurrence, given the text before it (@pre@) and after it (@post@),
-- sit in definition position?
isDefAt :: DefLang -> Text -> Text -> Text -> Text -> Bool
isDefAt lang name pre post line = case lang of
  LPython -> T.strip pre `elem` ["def", "async def", "class"]

  LJs -> lastWord p `elem` ["function", "function*", "const", "let", "var", "class"]
           || assignedCallable
           || methodShape
    where
      p = T.strip pre
      postT = T.stripStart post
      -- name = function… / name = (…) => / name: async (…) — an assignment or
      -- object key whose value is callable-shaped. "==", "===" and "=>" are
      -- comparisons / arrows, not definitions.
      assignedCallable = case T.uncons postT of
        Just ('=', r) | not (T.isPrefixOf "=" r), not (T.isPrefixOf ">" r)
          -> callableRhs (T.stripStart r)
        Just (':', r) -> callableRhs (T.stripStart r)
        _ -> False
      callableRhs r =
        wordPrefix "function" r || wordPrefix "async" r || T.isPrefixOf "(" r
      -- A method in a class/object body: the line starts (after modifiers)
      -- with "name(" and opens a block. Keyword statements are excluded.
      methodShape =
        p `elem` ["", "async", "static", "get", "set", "static async"]
          && T.isPrefixOf "(" postT
          && T.isSuffixOf "{" (T.stripEnd line)
          && name `notElem` jsKeywords

  LPhp -> lastWord (T.strip pre) `elem` ["function", "class", "const", "interface", "trait"]

  -- CREATE [OR REPLACE] FUNCTION|PROCEDURE [schema.]name — case-folded, with
  -- any trailing "schema." qualifiers peeled off the prefix first.
  LSql -> case T.words (dropQuals (T.strip (T.toLower pre))) of
            ("create" : rest@(_ : _)) -> last rest `elem` ["function", "procedure"]
            _                         -> False
    where
      dropQuals t
        | T.isSuffixOf "." t = dropQuals (T.dropWhileEnd sqlIdentCh (T.init t))
        | otherwise          = t
      sqlIdentCh c = isWordCh c || c == '$'

  -- Top-level binding (signature or an equation) at column 0, or a
  -- type/class declaration head.
  LHaskell
    | T.null pre -> T.isPrefixOf "::" postT
                      || " = " `T.isInfixOf` post
                      || T.isSuffixOf " =" (T.stripEnd post)
    | otherwise  -> T.strip pre `elem` ["data", "newtype", "type", "class"]
    where postT = T.stripStart post

  LShell -> (T.null p && T.isPrefixOf "()" postNoSp)
              || (p == "function"
                  && (T.null postT || T.isPrefixOf "(" postT || T.isPrefixOf "{" postT))
    where
      p = T.strip pre
      postT = T.stripStart post
      postNoSp = T.filter (/= ' ') post

  LRuby -> p `elem` ["def", "class", "module"] || T.isSuffixOf "def self." p
    where p = T.strip pre

  LGo -> p == "func" || p == "type"
           || (T.isPrefixOf "func (" p && T.isSuffixOf ")" p)
    where p = T.strip pre

jsKeywords :: [Text]
jsKeywords =
  [ "if", "for", "while", "switch", "catch", "return", "function"
  , "do", "else", "with", "new", "typeof", "await", "yield" ]

lastWord :: Text -> Text
lastWord t = case T.words t of
  [] -> ""
  ws -> last ws

------------------------------------------------------------------------------
-- Scan request

-- | What the driver needs to run a background definition scan.
data DefReq = DefReq
  { dfGen  :: !Int          -- ^ Supersede id (like a search's 'S.sqGen').
  , dfName :: !Text         -- ^ The identifier to find definitions of.
  , dfRoot :: !FilePath     -- ^ Workspace root to walk.
  , dfSkip :: ![FilePath]   -- ^ Open files (already seeded from their buffers).
  } deriving (Show)

------------------------------------------------------------------------------
-- Picker-dialog state

-- | One definition site.
data DefItem = DefItem
  { diPath :: !FilePath
  , diLine :: !Int      -- ^ 0-based.
  , diCol  :: !Int
  , diLen  :: !Int
  , diText :: !Text     -- ^ The (clipped) line, for the snippet column.
  } deriving (Eq, Show)

-- | The modal "go to definition" picker: a scrollable list of sites.
data DefPick = DefPick
  { dpGen     :: !Int
  , dpName    :: !Text
  , dpRoot    :: !FilePath    -- ^ For displaying workspace-relative paths.
  , dpItems   :: ![DefItem]
  , dpSel     :: !Int
  , dpTop     :: !Int
  , dpRunning :: !Bool        -- ^ The background scan is still streaming.
  } deriving (Eq, Show)

newDefPick :: Int -> Text -> FilePath -> DefPick
newDefPick gen name root = DefPick gen name root [] 0 0 True

-- | Append streamed items (arrival order; the walk is roughly alphabetical).
dpAddItems :: [DefItem] -> DefPick -> DefPick
dpAddItems new dp = dp { dpItems = dpItems dp ++ new }

-- | Move the selection by @d@ rows, keeping it visible in a @vh@-row list.
dpMoveSel :: Int -> Int -> DefPick -> DefPick
dpMoveSel d vh dp = dpSelTo (dpSel dp + d) vh dp

dpSelTo :: Int -> Int -> DefPick -> DefPick
dpSelTo i vh dp =
  let n = length (dpItems dp)
  in dpClampScroll vh dp { dpSel = max 0 (min (n - 1) i) }

-- | Clamp the scroll offset so the selection is on screen.
dpClampScroll :: Int -> DefPick -> DefPick
dpClampScroll vh dp =
  let top | dpSel dp < dpTop dp       = dpSel dp
          | dpSel dp >= dpTop dp + vh = dpSel dp - vh + 1
          | otherwise                 = dpTop dp
      maxTop = max 0 (length (dpItems dp) - vh)
  in dp { dpTop = max 0 (min maxTop top) }
