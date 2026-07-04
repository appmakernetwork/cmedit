-- | Workspace-wide (multi-file) search model. This module is /pure/: it holds
-- the search-panel state (input fields, options, and the grouped results tree)
-- and the navigation, glob-matching and per-file matching helpers. The actual
-- directory walking and file reading is IO and lives in "Cmedit.App", which
-- streams results back in through the callbacks in "Cmedit.Editor" — the same
-- effect/round-trip pattern used for the file browser and async file loads.
--
-- It is deliberately independent of 'Cmedit.Editor' (no import cycle): the
-- editor interprets what a result /means/ (open the file, move the cursor) and
-- owns the effect wiring; this module is data plus pure operations on it.
module Cmedit.Search
  ( -- * Options and request
    SearchField(..)
  , SField(..)
  , mkField
  , SearchReq(..)
    -- * Matches and results
  , Match(..)
  , FileResult(..)
  , matchCount
  , fileMatchCount
    -- * Panel state
  , SearchState(..)
  , newSearchState
  , searchActiveTerm
    -- * Matching (also used by the IO walker)
  , lineMatches
  , scanMatches
  , fileMatches
  , Matcher
  , compileMatcher
  , matcherLine
  , fileMatchesM
  , fileMatchesWith
  , regexReplaceText
  , maxMatchesPerFile
  , maxTotalMatches
  , maxResultFiles
  , maxFileBytesToSearch
    -- * Globs / scope
  , parseGlobs
  , globMatch
  , pathIncluded
  , dirPruned
  , defaultExcludes
  , binaryExtension
    -- * Field editing
  , fieldInsert
  , fieldBackspace
  , fieldDelete
  , fieldLeft
  , fieldRight
  , fieldHome
  , fieldEnd
  , fieldDeleteWordLeft
    -- * Header + result rows (shared with the renderer and mouse hit-testing)
  , HLine(..)
  , headerLines
  , headerHeight
  , SRow(..)
  , resultRows
  , FocusItem(..)
  , focusItems
  , headerItems
  , focusedField
  , focusedReplaceAll
  , selectedRow
  , cursorRowInResults
    -- * Navigation
  , moveCursor
  , setCursorField
  , setCursorResultRow
  , clampCursor
  , scrollInto
  , toggleFileCollapsed
  , resultPaths
  ) where

import Data.Array (Array, listArray, (!))
import Data.Char (isSpace, isAlphaNum, toLower)
import Data.Foldable (toList)
import Data.List (foldl', findIndex)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Cmedit.Regex (Regex)
import qualified Cmedit.Regex as Rx

------------------------------------------------------------------------------
-- Fields

-- | Which input widget of the search panel is being addressed.
data SearchField = SFFind | SFReplace | SFInclude | SFExclude
  deriving (Eq, Show)

-- | A single-line editable input (text + cursor as a character index).
data SField = SField { sfText :: !Text, sfCur :: !Int } deriving (Eq, Show)

mkField :: Text -> SField
mkField t = SField t (T.length t)

fieldInsert :: Char -> SField -> SField
fieldInsert ch (SField t c) = SField (T.take c t <> T.singleton ch <> T.drop c t) (c + 1)

fieldBackspace :: SField -> SField
fieldBackspace (SField t c) = if c > 0 then SField (T.take (c - 1) t <> T.drop c t) (c - 1)
                                       else SField t c

fieldDelete :: SField -> SField
fieldDelete (SField t c) = if c < T.length t then SField (T.take c t <> T.drop (c + 1) t) c
                                             else SField t c

fieldLeft :: SField -> SField
fieldLeft (SField t c) = SField t (max 0 (c - 1))

fieldRight :: SField -> SField
fieldRight (SField t c) = SField t (min (T.length t) (c + 1))

fieldHome :: SField -> SField
fieldHome (SField t _) = SField t 0

fieldEnd :: SField -> SField
fieldEnd (SField t _) = SField t (T.length t)

fieldDeleteWordLeft :: SField -> SField
fieldDeleteWordLeft (SField t c) =
  let before = reverse (T.unpack (T.take c t))
      kept   = reverse (dropWhile (not . isSpace) (dropWhile isSpace before))
  in SField (T.pack kept <> T.drop c t) (length kept)

------------------------------------------------------------------------------
-- Request handed to the IO walker

-- | Everything the background walker needs to run one search. @sqSkip@ holds the
-- canonical paths of files that are open in the editor: those are searched from
-- their (possibly unsaved) in-memory buffers instead, so the walker skips them.
data SearchReq = SearchReq
  { sqGen     :: !Int
  , sqRoot    :: !FilePath
  , sqTerm    :: !Text
  , sqCase    :: !Bool
  , sqWord    :: !Bool
  , sqRegex   :: !Bool
  , sqInclude :: ![String]
  , sqExclude :: ![String]
  , sqSkip    :: ![FilePath]
  } deriving (Show)

------------------------------------------------------------------------------
-- Matches / results

-- | The matches on a single line (one panel row per matching line; a line with
-- several occurrences highlights them all). @mText@ is the line, clipped to a
-- sane width so a pathological long line can't blow up memory.
data Match = Match
  { mLine :: !Int            -- ^ 0-based line number in the file.
  , mCols :: ![(Int, Int)]   -- ^ (startColumn, length) of each occurrence, in char columns.
  , mText :: !Text           -- ^ The line text for the snippet (clipped).
  } deriving (Eq, Show)

-- | All matches for one file, plus display state.
data FileResult = FileResult
  { frPath      :: !FilePath
  , frMatches   :: ![Match]
  , frCollapsed :: !Bool
  , frTruncated :: !Bool     -- ^ matches for this file were capped.
  } deriving (Eq, Show)

matchCount :: Match -> Int
matchCount = length . mCols

fileMatchCount :: FileResult -> Int
fileMatchCount = sum . map matchCount . frMatches

------------------------------------------------------------------------------
-- Panel state

-- | The whole search view: input fields, toggles, the streamed results, and the
-- cursor/scroll of the results list. Held in 'Cmedit.Editor.edSearch'.
data SearchState = SearchState
  { ssFind        :: !SField
  , ssReplace     :: !SField
  , ssInclude     :: !SField
  , ssExclude     :: !SField
  , ssCase        :: !Bool
  , ssWord        :: !Bool
  , ssRegex       :: !Bool       -- ^ interpret the Find term as a regular expression.
  , ssShowReplace :: !Bool       -- ^ the Replace row is shown (F6).
  , ssResults     :: !(Seq FileResult)   -- ^ a Seq for O(1) streaming append + length.
  , ssCursor      :: !Int        -- ^ index into 'focusItems' (fields then result rows).
  , ssTop         :: !Int        -- ^ first visible /result/ row (scroll offset).
  , ssLeft        :: !Int        -- ^ extra horizontal pan of the result snippets (Shift+wheel).
  , ssRunning     :: !Bool       -- ^ a search is in progress (spinner).
  , ssTotal       :: !Int        -- ^ running total match count (kept incrementally so streaming stays O(1) per file).
  , ssGen         :: !Int        -- ^ id of the latest search (stale updates are dropped).
  , ssSpin        :: !Int        -- ^ spinner frame.
  , ssScanned     :: !Int        -- ^ files scanned so far (progress).
  , ssTruncated   :: !Bool       -- ^ the global match cap was hit.
  , ssMessage     :: !Text       -- ^ status / "no results" note.
  , ssRoot        :: !FilePath    -- ^ directory being searched.
  , ssSearched    :: !Bool       -- ^ a search has been run at least once this session.
  } deriving (Show)

newSearchState :: FilePath -> SearchState
newSearchState root = SearchState
  { ssFind = mkField "", ssReplace = mkField "", ssInclude = mkField "", ssExclude = mkField ""
  , ssCase = False, ssWord = False, ssRegex = False, ssShowReplace = False
  , ssResults = Seq.empty, ssCursor = 0, ssTop = 0, ssLeft = 0
  , ssRunning = False, ssTotal = 0, ssGen = 0, ssSpin = 0, ssScanned = 0
  , ssTruncated = False, ssMessage = "", ssRoot = root, ssSearched = False }

-- | The find term with surrounding structure stripped (currently just as typed).
searchActiveTerm :: SearchState -> Text
searchActiveTerm = sfText . ssFind

------------------------------------------------------------------------------
-- Per-line / per-file matching

-- | Caps that keep a huge tree or a hostile file from exhausting memory.
maxMatchesPerFile :: Int
maxMatchesPerFile = 2000

maxTotalMatches :: Int
maxTotalMatches = 20000

-- | Cap on the number of distinct files shown in the results, so a very broad
-- search (e.g. a common word over a home directory) can't grow the result list
-- without bound. Beyond this the results are marked truncated.
maxResultFiles :: Int
maxResultFiles = 5000

-- | Files bigger than this are skipped by the walker (they are unlikely source
-- files and would slow the sweep).
maxFileBytesToSearch :: Integer
maxFileBytesToSearch = 8 * 1024 * 1024

normCase :: Bool -> Text -> Text
normCase cs = if cs then id else T.toLower

isWordCh :: Char -> Bool
isWordCh c = isAlphaNum c || c == '_'

-- | Match start offsets of @nterm@ in @nline@ (both already case-normalised),
-- honouring the whole-word option. Walks the line ONCE with 'T.breakOn'
-- (linear), and checks word boundaries on the slice edges ('T.last' of the
-- gap / 'T.uncons' of the remainder, both O(1)) with the char preceding the
-- current remainder threaded through — an indexed @T.index line i@ boundary
-- check is O(i) per candidate, which on a minified multi-megabyte line with a
-- candidate every few chars is O(n²): minutes inside one search worker.
-- @overlap@ selects overlapping matches (advance one char past an accepted
-- match — find-next wants the match at every start column) or non-overlapping
-- (advance past the whole match — search results and replace-all).
scanMatches :: Bool -> Bool -> Text -> Text -> [Int]
scanMatches overlap ww nterm nline
  | T.null nterm = []
  | otherwise = go 0 Nothing nline
  where
    nlen  = T.length nterm
    lastT = T.last nterm
    go !off prev t =
      let (pre, rest) = T.breakOn nterm t
      in if T.null rest then []
         else
           let i = off + T.length pre
               beforeC = if T.null pre then prev else Just (T.last pre)
               after   = T.drop nlen rest
               ok = not ww
                    || (maybe True (not . isWordCh) beforeC
                        && maybe True (not . isWordCh . fst) (T.uncons after))
               stepOne = case T.uncons rest of
                 Just (h, t1) -> go (i + 1) (Just h) t1
                 Nothing      -> []
           in if ok
                then i : (if overlap then stepOne else go (i + nlen) (Just lastT) after)
                else stepOne

-- | Non-overlapping (startCol, length) matches of @term@ in @line@, honouring
-- case-insensitivity (@cs@ False = ignore case) and whole-word (@ww@).
-- Linear in the line length whatever the options (see 'scanMatches').
lineMatches :: Bool -> Bool -> Text -> Text -> [(Int, Int)]
lineMatches cs ww term line
  | T.null term = []
  | otherwise =
      let len = T.length term
      in [ (i, len) | i <- scanMatches False ww (normCase cs term) (normCase cs line) ]

-- | Search a whole file's decoded text (literal term). Returns one 'Match' per
-- matching line (up to 'maxMatchesPerFile' lines), whether that per-file cap was
-- hit, and the total occurrence count. Only plain (newline-free) terms are
-- supported; a term with a newline yields nothing.
fileMatches :: Bool -> Bool -> Text -> Text -> ([Match], Bool, Int)
fileMatches cs ww term txt
  | T.null term || T.any (== '\n') term = ([], False, 0)
  | otherwise = collectMatches (lineMatches cs ww term) txt

-- | A compiled query: a fast literal scan, or a regular expression.
data Matcher = MLit !Bool !Bool !Text | MRe !Regex

-- | Compile the Find term to a 'Matcher'. Fails (with a message) on a bad regex.
compileMatcher :: Bool -> Bool -> Bool -> Text -> Either String Matcher
compileMatcher cs ww regex term
  | T.null term            = Left "empty search term"
  | regex                  = MRe <$> Rx.compile (not cs) term
  | T.any (== '\n') term   = Left "multi-line term"
  | otherwise              = Right (MLit cs ww term)

-- | The (startCol, length) matches of a 'Matcher' on one line. Whole-word only
-- applies to literal matchers; a regex controls its own boundaries with @\\b@.
matcherLine :: Matcher -> Text -> [(Int, Int)]
matcherLine (MLit cs ww term) = lineMatches cs ww term
matcherLine (MRe r)           = filter ((> 0) . snd) . Rx.lineMatches r

-- | 'fileMatches' for an arbitrary compiled matcher.
fileMatchesM :: Matcher -> Text -> ([Match], Bool, Int)
fileMatchesM m = collectMatches (matcherLine m)

-- | 'fileMatches' for an arbitrary per-line matching function (used by the
-- go-to-definition scan, whose matcher depends on the file's language).
fileMatchesWith :: (Text -> [(Int, Int)]) -> Text -> ([Match], Bool, Int)
fileMatchesWith = collectMatches

-- Shared core: collect one 'Match' per matching line using @perLine@.
collectMatches :: (Text -> [(Int, Int)]) -> Text -> ([Match], Bool, Int)
collectMatches perLine txt = go 0 (T.lines txt) [] 0
  where
    go _ [] acc !cnt = (reverse acc, False, cnt)
    go !ln (l : ls) acc !cnt
      | length acc >= maxMatchesPerFile = (reverse acc, True, cnt)
      | otherwise = case perLine l of
          []   -> go (ln + 1) ls acc cnt
          cols -> go (ln + 1) ls (Match ln cols (clip l) : acc) (cnt + length cols)
    -- Keep snippets bounded, and 'T.copy' them: a Text slice shares its source
    -- array, so an uncopied snippet would pin the whole decoded file (up to
    -- megabytes) in memory for as long as its result row is on screen.
    clip l = T.copy (T.take 2000 l)

-- | Regex replace across a file's text, per physical line, preserving CRLF line
-- endings (the trailing @\\r@ is set aside so @$@ anchors at the true line end
-- and @.*@ can't swallow it). Returns the substitution count and new text.
regexReplaceText :: Regex -> Text -> Text -> (Int, Text)
regexReplaceText r repl txt =
  let segs = T.splitOn (T.pack "\n") txt
      step seg =
        let (crlf, body) = if not (T.null seg) && T.last seg == '\r'
                             then (True, T.init seg) else (False, seg)
            (c, body') = Rx.replaceLine r repl body
        in (c, if crlf then body' <> T.pack "\r" else body')
      results = map step segs
  in (sum (map fst results), T.intercalate (T.pack "\n") (map snd results))

------------------------------------------------------------------------------
-- Globs and scope

-- | Split a comma separated glob list ("*.hs, src/**") into patterns.
parseGlobs :: Text -> [String]
parseGlobs = filter (not . null) . map (T.unpack . T.strip) . T.split (\c -> c == ',' || c == '\n')

-- | Match a single glob against a path. @*@ matches within a path segment,
-- @**@ matches across segments (any characters including @/@), @?@ matches one
-- non-@/@ character. A pattern with no @/@ is matched against the basename
-- (VS Code semantics: @*.hs@ means @**/*.hs@).
globMatch :: String -> FilePath -> Bool
globMatch pat path
  | '/' `elem` pat = matchGlob (norm pat) (norm path)
  | otherwise      = matchGlob (norm pat) (basename (norm path))
  where
    norm = map toLower . dropSlash
    dropSlash ('/' : r) = dropSlash r
    dropSlash s = s
    basename = reverse . takeWhile (/= '/') . reverse

-- Core glob matcher over already-lower-cased strings.
-- Memoised over (pattern position, path position), so matching is
-- O(pattern × path) — plain backtracking is exponential in the number of
-- stars (a user glob like @*a*a*a*a*a*b@ took seconds *per file* inside the
-- search walker, an effective hang). Runs per candidate path, so it must stay
-- robust against whatever include/exclude pattern the user types.
matchGlob :: String -> String -> Bool
matchGlob pat path = memo ! (0, 0)
  where
    m = length pat
    n = length path
    pv = listArray (0, max 0 (m - 1)) pat  :: Array Int Char
    cv = listArray (0, max 0 (n - 1)) path :: Array Int Char
    memo :: Array (Int, Int) Bool
    memo = listArray ((0, 0), (m, n))
             [ step i j | i <- [0 .. m], j <- [0 .. n] ]
    step i j
      | i >= m = j >= n
      | otherwise = case pv ! i of
          '*' | i + 1 < m && pv ! (i + 1) == '*' ->
                -- ** : match any run (including '/'); collapse consecutive stars.
                memo ! (dropStars (i + 2), j) || (j < n && memo ! (i, j + 1))
              | otherwise ->
                -- * : match within a segment (no '/').
                memo ! (i + 1, j) || (j < n && cv ! j /= '/' && memo ! (i, j + 1))
          '?' -> j < n && cv ! j /= '/' && memo ! (i + 1, j + 1)
          c   -> j < n && cv ! j == c && memo ! (i + 1, j + 1)
    dropStars i | i < m && pv ! i == '*' = dropStars (i + 1)
                | otherwise = i

-- | Should this relative path be searched? Passes when it matches at least one
-- include glob (or there are none) and no exclude glob and no default exclude.
pathIncluded :: [String] -> [String] -> FilePath -> Bool
pathIncluded includes excludes rel =
  includeOK && not (any (`globMatch` rel) excludes) && not excludedByDefault
  where
    includeOK = null includes || any (`globMatch` rel) includes
    excludedByDefault = any (`segMatch` rel) defaultExcludes
    segMatch name p = name `elem` splitSegs p

splitSegs :: FilePath -> [String]
splitSegs = foldr step [[]] . map toLower
  where step '/' acc = [] : acc
        step c (cur : rest) = (c : cur) : rest
        step _ [] = [[]]

-- | Should the walker skip descending into this directory (given its /basename/
-- and the user exclude globs)? Prunes dot-directories, the built-in heavy dirs,
-- and anything the exclude patterns name — the key to not melting on big trees.
dirPruned :: [String] -> FilePath -> Bool
dirPruned excludes name =
  isDot name
    || map toLower name `elem` defaultExcludes
    || any (`globMatch` name) excludes
  where isDot ('.' : _) = True
        isDot _ = False

-- | Directories never worth searching (mirrors VS Code's default search excludes).
defaultExcludes :: [String]
defaultExcludes =
  [ ".git", ".hg", ".svn", "node_modules", "bower_components", "dist"
  , "dist-newstyle", "dist-build", "dist-test", "build", "target"
  , ".stack-work", ".mypy_cache", "__pycache__", ".pytest_cache"
  , ".venv", "venv", ".tox", ".idea", ".vscode", ".next", ".nuxt"
  , "out", "vendor", ".cache", "coverage", ".gradle", ".terraform" ]

-- | Does this file name carry an extension that is effectively always binary?
-- The walker skips these without opening the file at all; anything not listed
-- is still NUL-sniffed after reading its first block, so the list only needs
-- to cover the common bulky formats (media, archives, objects) that would
-- otherwise cost an open+read each on a big tree.
binaryExtension :: FilePath -> Bool
binaryExtension name = case break (== '.') (reverse name) of
  (revExt, '.' : _ : _) -> map toLower (reverse revExt) `elem` binaryExtensions
  _                     -> False   -- no dot, a trailing dot, or a dotfile

binaryExtensions :: [String]
binaryExtensions =
  [ -- images / media
    "png", "jpg", "jpeg", "gif", "bmp", "ico", "webp", "tiff", "heic"
  , "mp3", "mp4", "m4a", "aac", "avi", "mkv", "mov", "webm", "ogg", "flac", "wav"
  , "woff", "woff2", "ttf", "otf", "eot"
    -- archives / packages
  , "zip", "gz", "tgz", "xz", "bz2", "zst", "7z", "rar", "tar", "jar", "war"
  , "deb", "rpm", "dmg", "iso", "img"
    -- compiled objects and other opaque blobs
  , "so", "o", "a", "dylib", "dll", "exe", "obj", "class", "pyc", "pyo", "wasm"
  , "pdf", "sqlite", "sqlite3", "hi", "rlib" ]

------------------------------------------------------------------------------
-- Header + result rows (shared layout for rendering and mouse hit-testing)

-- | The fixed rows above the (scrolling) results list.
data HLine = HLScope | HLFind | HLReplace | HLInclude | HLExclude | HLSummary | HLDivider
  deriving (Eq, Show)

headerLines :: SearchState -> [HLine]
headerLines ss =
  [HLScope, HLFind]
    ++ (if ssShowReplace ss then [HLReplace] else [])
    ++ [HLInclude, HLExclude, HLSummary, HLDivider]

headerHeight :: SearchState -> Int
headerHeight = length . headerLines

-- | A row of the results list: a file header, or a matching-line under it.
data SRow = SRFile !Int | SRMatch !Int !Int   -- fileIndex ; (fileIndex, matchIndex)
  deriving (Eq, Show)

-- | The flattened, currently-visible result rows (respecting per-file collapse).
resultRows :: SearchState -> [SRow]
resultRows ss = concat
  [ SRFile fi : (if frCollapsed fr then [] else [ SRMatch fi mi | mi <- [0 .. length (frMatches fr) - 1] ])
  | (fi, fr) <- zip [0 ..] (toList (ssResults ss)) ]

-- | A focusable item: an input field, the Replace All button, or a result row.
-- 'ssCursor' indexes this. The button is only present while Replace is shown.
data FocusItem = FIField !SearchField | FIReplaceAll | FIRow !SRow
  deriving (Eq, Show)

-- | The fixed (non-result-row) focusable items, in Tab order: the input fields,
-- with the Replace All button right after the Replace field.
headerItems :: SearchState -> [FocusItem]
headerItems ss =
  [FIField SFFind]
    ++ (if ssShowReplace ss then [FIField SFReplace, FIReplaceAll] else [])
    ++ [FIField SFInclude, FIField SFExclude]

focusItems :: SearchState -> [FocusItem]
focusItems ss = headerItems ss ++ map FIRow (resultRows ss)

-- | The field the cursor is on, if any (else a button or a result row).
focusedField :: SearchState -> Maybe SearchField
focusedField ss = case drop (ssCursor ss) (focusItems ss) of
  (FIField f : _) -> Just f
  _               -> Nothing

-- | Is the cursor on the Replace All button?
focusedReplaceAll :: SearchState -> Bool
focusedReplaceAll ss = case drop (ssCursor ss) (focusItems ss) of
  (FIReplaceAll : _) -> True
  _                  -> False

-- | The result row the cursor is on, if any.
selectedRow :: SearchState -> Maybe SRow
selectedRow ss = case drop (ssCursor ss) (focusItems ss) of
  (FIRow r : _) -> Just r
  _             -> Nothing

-- | Index of the cursor within the result rows (for scrolling), or Nothing when
-- it is on a header field/button.
cursorRowInResults :: SearchState -> Maybe Int
cursorRowInResults ss =
  let nf = length (headerItems ss)
  in if ssCursor ss >= nf then Just (ssCursor ss - nf) else Nothing

------------------------------------------------------------------------------
-- Navigation

clampCursor :: SearchState -> SearchState
clampCursor ss =
  let n = length (focusItems ss)
  in ss { ssCursor = max 0 (min (max 0 (n - 1)) (ssCursor ss)) }

moveCursor :: Int -> SearchState -> SearchState
moveCursor d ss =
  let n = length (focusItems ss)
  in if n == 0 then ss else ss { ssCursor = max 0 (min (n - 1) (ssCursor ss + d)) }

-- | Put the cursor on a specific input field (used by Tab and mouse).
setCursorField :: SearchField -> SearchState -> SearchState
setCursorField f ss = case findIndex (== FIField f) (focusItems ss) of
  Just i  -> ss { ssCursor = i }
  Nothing -> ss

-- | Put the cursor on result row @k@ (index into 'resultRows').
setCursorResultRow :: Int -> SearchState -> SearchState
setCursorResultRow k ss =
  let nf = length (headerItems ss)
      nr = length (resultRows ss)
  in if nr == 0 then ss else ss { ssCursor = nf + max 0 (min (nr - 1) k) }

-- | Keep the selected /result/ row within a viewport of @height@ rows.
scrollInto :: Int -> SearchState -> SearchState
scrollInto height ss = case cursorRowInResults ss of
  Nothing  -> ss { ssTop = 0 }
  Just row ->
    let top0 = ssTop ss
        top1 | row < top0            = row
             | row >= top0 + height  = row - height + 1
             | otherwise             = top0
    in ss { ssTop = max 0 top1 }

-- | Collapse/expand the file at index @fi@.
toggleFileCollapsed :: Int -> SearchState -> SearchState
toggleFileCollapsed fi ss =
  ss { ssResults = Seq.adjust (\fr -> fr { frCollapsed = not (frCollapsed fr) }) fi (ssResults ss) }

-- | Distinct file paths that currently have results.
resultPaths :: SearchState -> [FilePath]
resultPaths = map frPath . toList . ssResults
