-- | Syntax highlighting. Each supported language has a lexer that turns a line
-- into compact 'TokRun's, threading a small 'HlState' across lines so
-- multi-line constructs (block comments, PostgreSQL dollar-quoted strings,
-- Python triple-quoted strings, Markdown fenced code, HTML comments) are
-- highlighted correctly. 'lexLine' expands the runs for compatibility; the
-- renderer consumes them directly.
--
-- 'HlCache' keeps the per-line lexer states between frames so the renderer
-- only lexes the visible window instead of a bounded look-back. The general
-- API self-validates against its remembered line 'Seq'; the renderer uses the
-- editor's explicit document version and validates only through the requested
-- viewport, preventing an accidental full-file comparison per frame.
{-# LANGUAGE MagicHash #-}
module Cmedit.Syntax
  ( Lang(..)
  , Tok(..)
  , HlState(..)
  , langForPath
  , initialState
  , lexLine
  , TokRun(..)
  , lexLineRuns
  , expandTokRuns
  , maxHlLine
    -- * Comment syntax (Edit ▸ Toggle Comment)
  , CommentSyntax(..)
  , langComment
    -- * Cached highlighting state
  , HlCache
  , refreshHlCache
  , refreshHlCacheFor
  , hlStateBefore
  , hlCoverage
  , hlTokenRuns
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper, toLower)
import Data.Foldable (toList)
import Data.List (foldl', isPrefixOf)
import qualified Cmedit.Zip as Zip
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import System.FilePath (takeExtension)

data Lang = SQL | Python | Markdown | HTML
          | JS | CSS | Shell | JSON | YAML | TOML | INI | FTL | Jinja | CSV
          | Haskell | RTF | Archive
  deriving (Eq, Show)

-- | A highlighting token class assigned to each character.
data Tok
  = TkText | TkKeyword | TkType | TkString | TkComment | TkNumber
  | TkFunction | TkBuiltin | TkDecorator | TkTag | TkAttr
  | TkHeading | TkEmph | TkStrong | TkCode | TkLink | TkPunct
  | TkProperty       -- ^ Keys / properties (config keys, CSS properties).
  deriving (Eq, Show)

-- | A run of characters sharing one token class. The length is in buffer
-- characters, not terminal cells. Keeping the lexer output in this form avoids
-- allocating one list cell per character merely for the renderer to turn that
-- list into another lookup structure.
data TokRun = TokRun !Int !Tok
  deriving (Eq, Show)

-- | Lexer state carried between lines for multi-line constructs.
data HlState
  = StNormal
  | StBlock          -- ^ Block comment: @/* */@ or @<!-- -->@.
  | StString !Char   -- ^ An unterminated single/double-quoted string.
  | StTriple !Char   -- ^ Python triple-quoted string (the quote char).
  | StDollar !String -- ^ PostgreSQL dollar-quoted string (the @$tag$@ delimiter).
  | StFence          -- ^ Markdown fenced code block.
  | StTag            -- ^ Inside an HTML tag (between @<@ and @>@).
  | StTemplate       -- ^ JavaScript template literal (backtick string).
  | StJinjaComment   -- ^ Jinja @{# ... #}@ comment.
  | StNestComment !Int -- ^ Haskell @{- -}@ block comment; the int is the nesting depth.
  deriving (Eq, Show)

initialState :: HlState
initialState = StNormal

-- | How Edit ▸ Toggle Comment comments a line in each language: a line-comment
-- prefix, or an open/close pair for languages that only have block comments.
data CommentSyntax = LineComment !Text | BlockComment !Text !Text
  deriving (Eq, Show)

langComment :: Lang -> Maybe CommentSyntax
langComment lang = case lang of
  SQL      -> Just (LineComment "--")
  Python   -> Just (LineComment "#")
  Shell    -> Just (LineComment "#")
  YAML     -> Just (LineComment "#")
  TOML     -> Just (LineComment "#")
  INI      -> Just (LineComment "#")
  JS       -> Just (LineComment "//")
  JSON     -> Just (LineComment "//")   -- JSONC-style; toggling is an explicit user action
  Haskell  -> Just (LineComment "--")
  CSS      -> Just (BlockComment "/*" "*/")
  HTML     -> Just (BlockComment "<!--" "-->")
  Markdown -> Just (BlockComment "<!--" "-->")
  FTL      -> Just (BlockComment "<#--" "-->")
  Jinja    -> Just (BlockComment "{#" "#}")
  CSV      -> Nothing                   -- comments aren't a thing in CSV data
  RTF      -> Nothing                   -- RTF has {\*\comment ...}, but commenting out markup is not an edit anyone wants
  Archive  -> Nothing                   -- the listing is generated and read-only

-- | Choose a language from a file path's extension, if supported.
langForPath :: Maybe FilePath -> Maybe Lang
langForPath Nothing = Nothing
langForPath (Just p) = case map toLower (takeExtension p) of
  ".sql"  -> Just SQL
  ".py"   -> Just Python
  ".md"       -> Just Markdown
  ".markdown" -> Just Markdown
  ".html" -> Just HTML
  ".htm"  -> Just HTML
  ".xml"  -> Just HTML
  ".svg"  -> Just HTML
  ".js"   -> Just JS
  ".mjs"  -> Just JS
  ".cjs"  -> Just JS
  ".jsx"  -> Just JS
  ".ts"   -> Just JS
  ".tsx"  -> Just JS
  ".css"  -> Just CSS
  ".scss" -> Just CSS
  ".less" -> Just CSS
  ".sh"   -> Just Shell
  ".bash" -> Just Shell
  ".json" -> Just JSON
  ".yaml" -> Just YAML
  ".yml"  -> Just YAML
  ".toml" -> Just TOML
  ".ini"  -> Just INI
  ".conf" -> Just INI
  ".cfg"  -> Just INI
  ".ftl"  -> Just FTL
  ".jinja"  -> Just Jinja
  ".jinja2" -> Just Jinja
  ".j2"     -> Just Jinja
  ".rtf"  -> Just RTF
  ".csv"  -> Just CSV
  ".tsv"  -> Just CSV
  ".hs"   -> Just Haskell
  -- Not the file's own syntax: an archive is binary, and what the editor shows
  -- for one is the listing "Cmedit.Zip" derives from its table of contents.
  e | drop 1 e `elem` Zip.archiveExtensions -> Just Archive
  _       -> Nothing

-- | Above this length a line is rendered unstyled: tokenising a
-- multi-megabyte minified line on every frame would dwarf everything else the
-- editor does. The lexer state passes through unchanged so later lines keep
-- a sane state (a construct opened *inside* such a line is a lost cause).
maxHlLine :: Int
maxHlLine = 20000

-- | Lex one line, returning a token per character and the trailing state.
lexLine :: Lang -> HlState -> Text -> ([Tok], HlState)
lexLine lang st line =
  let (runs, st') = lexLineRuns lang st line
  in (expandTokRuns runs, st')

-- | Compact lexer output used by rendering and by the highlight cache. The
-- generic source-language lexers already discover runs, so retain them instead
-- of expanding with @replicate@. The three positional lexers keep their
-- established implementations and are compacted at their boundary; those
-- views are read-only or line-oriented and, once cached, pay that conversion
-- only when their text changes.
lexLineRuns :: Lang -> HlState -> Text -> ([TokRun], HlState)
lexLineRuns _ st line
  | T.length line > maxHlLine = ([], st)
lexLineRuns lang st line = case lang of
  SQL      -> lexWithRuns sqlStep st line
  Python   -> lexWithRuns pyStep st line
  HTML     -> lexWithRuns htmlStep st line
  Markdown -> compactResult (lexMarkdown st line)
  JS       -> lexWithRuns jsStep st line
  CSS      -> lexWithRuns cssStep st line
  Shell    -> lexWithRuns shStep st line
  JSON     -> lexWithRuns jsonStep st line
  YAML     -> lexWithRuns yamlStep st line
  TOML     -> lexWithRuns tomlStep st line
  INI      -> lexWithRuns iniStep st line
  FTL      -> lexWithRuns ftlStep st line
  Jinja    -> lexWithRuns jinjaStep st line
  Haskell  -> lexWithRuns hsStep st line
  CSV      -> compactResult (lexCsv line)
  RTF      -> compactResult (lexRtf line)
  Archive  -> compactResult (lexArchive line)
  where
    compactResult (ts, st') = (compactTokens ts, st')

-- | Expand compact runs for callers that require the historical one-token-per-
-- character API.
expandTokRuns :: [TokRun] -> [Tok]
expandTokRuns = concatMap (\(TokRun n tok) -> replicate n tok)

compactTokens :: [Tok] -> [TokRun]
compactTokens = reverse . foldl' add []
  where
    add (TokRun n old : rest) tok | old == tok = TokRun (n + 1) old : rest
    add acc tok = TokRun 1 tok : acc

------------------------------------------------------------------------------
-- Cached lexer states

-- | Cross-frame cache of lexer states. @hcStates@ holds one trusted entry per
-- line: entry @i@ is the state *after* lexing lines @0..i@ of @hcLines@ (so
-- the state before line @i@ is entry @i-1@, or 'initialState' for line 0).
-- @hcTail@ keeps the previously-trusted entries after an edit: while
-- re-lexing past the edit, the first line whose freshly computed state equals
-- its old entry proves everything below it unchanged, and the rest of the
-- tail is adopted wholesale instead of re-lexed ("re-convergence").
data HlCache = HlCache
  { hcLang      :: !Lang
  , hcLines     :: !(Seq Text)     -- ^ The buffer lines the cache is synced to.
  , hcStates    :: !(Seq HlState)  -- ^ Trusted end-of-line states, one per covered line.
  , hcTail      :: !(Seq HlState)  -- ^ Old end-of-line states kept for re-convergence.
  , hcTailFloor :: !Int            -- ^ Tail entries are adoptable only at indices >= this.
  , hcVersion   :: !Int            -- ^ Explicit document version; negative for self-validating callers.
  , hcTokens    :: !(Map.Map Int (Text, HlState, HlState, [TokRun]))
                                    -- ^ Visible line, incoming/trailing state and compact tokens.
  } deriving (Show)

-- GC can move objects between comparisons, so a pointer mismatch proves
-- nothing (fall back to (==)) — but a pointer match is a sound "equal".
ptrEq :: a -> a -> Bool
ptrEq a b = isTrue# (reallyUnsafePtrEquality# a b)

sameText :: Text -> Text -> Bool
sameText a b = ptrEq a b || a == b

freshCache :: Lang -> Seq Text -> HlCache
freshCache lang cur = HlCache lang cur Seq.empty Seq.empty 0 (-1) Map.empty

-- | Sync the cache with the buffer (locating any edits itself) and extend the
-- trusted states to cover line @target@. O(1) when the buffer is untouched
-- and the target already covered; otherwise proportional to the edited/newly
-- covered region.
refreshHlCache :: Lang -> Seq Text -> Int -> Maybe HlCache -> HlCache
refreshHlCache lang cur target mc = extendTo target (syncCache lang cur mc)

-- | Editor-facing refresh with an explicit per-document edit version and a
-- bounded token window. When the version is unchanged there is no buffer
-- comparison at all. This is deliberately separate from 'refreshHlCache':
-- tests and non-editor callers keep the self-validating API, while the hot
-- frame path uses the counter that the editor already maintains as the honest
-- description of whether document text changed.
refreshHlCacheFor :: Lang -> Seq Text -> Int -> Int -> Int -> Maybe HlCache -> HlCache
refreshHlCacheFor lang cur version first target mc =
  fillTokens (max 0 first) target extended
  where
    synced = case mc of
      Just c | hcLang c == lang && hcVersion c == version -> c
      _ -> (syncCacheTo lang cur target mc) { hcVersion = version }
    extended = extendToWithTokens target synced

-- Extend lexer states while retaining the compact output produced along the
-- way. A cached line whose text and incoming state still match supplies both
-- its tokens and trailing state without running the lexer at all. This is the
-- common cursor-move case and, after a local edit, the re-converged suffix of
-- the viewport.
extendToWithTokens :: Int -> HlCache -> HlCache
extendToWithTokens target c0 = go c0 (hcStates c0) prev0 start
  where
    n0 = Seq.length (hcStates c0)
    prev0 = if n0 == 0 then initialState else Seq.index (hcStates c0) (n0 - 1)
    start = toList (Seq.drop n0 (hcLines c0))
    stop = min target (Seq.length (hcLines c0) - 1)
    go c sts _ [] = c { hcStates = sts }
    go c sts prev (ln : rest)
      | Seq.length sts > stop = c { hcStates = sts }
      | otherwise =
          let i = Seq.length sts
              (st', runs) = case Map.lookup i (hcTokens c) of
                Just (oldLine, oldIn, oldOut, oldRuns)
                  | sameText oldLine ln && oldIn == prev -> (oldOut, oldRuns)
                _ -> let (newRuns, newOut) = lexLineRuns (hcLang c) prev ln
                     in (newOut, newRuns)
              toks' = Map.insert i (ln, prev, st', runs) (hcTokens c)
          in go c { hcTokens = toks' } (sts Seq.|> st') st' rest

-- The editor has an explicit document version, so on a changed version it
-- only needs to validate cache entries that can affect the requested target.
-- Keeping a previously trusted tail would require proving the entire suffix
-- unchanged -- the O(file) scan that made the first token-cache prototype
-- 3.7x slower on 200k-line files. Instead, preserve the equal prefix through
-- @target@ and discard everything deeper. A later scroll extends from that
-- trusted frontier over the current lines, so no unverified state is ever
-- observed and edit-time work stays bounded by what the frame can use.
syncCacheTo :: Lang -> Seq Text -> Int -> Maybe HlCache -> HlCache
syncCacheTo lang cur _ Nothing = freshCache lang cur
syncCacheTo lang cur target (Just c)
  | hcLang c /= lang = freshCache lang cur
  | otherwise =
      let limit = max 0 (target + 1)
          old = take limit (toList (hcLines c))
          new = take limit (toList cur)
          f = length (takeWhile (uncurry sameText) (zip old new))
          keep = min f (Seq.length (hcStates c))
      in HlCache lang cur (Seq.take keep (hcStates c)) Seq.empty 0 (-1)
           (hcTokens c)

-- Keep only the requested viewport. Entries are validated by both the source
-- line and incoming state, so a one-line edit retains unaffected runs when the
-- state re-converges; a stale entry can cost a re-lex but never correctness.
fillTokens :: Int -> Int -> HlCache -> HlCache
fillTokens first target c = c { hcTokens = go first kept }
  where
    lastL = min target (Seq.length (hcLines c) - 1)
    kept = Map.filterWithKey (\i _ -> i >= first && i <= lastL) (hcTokens c)
    go i acc
      | i > lastL = acc
      | otherwise =
          let line = Seq.index (hcLines c) i
              st = hlStateBefore c i
              st' = Seq.index (hcStates c) i
          in case Map.lookup i acc of
               Just (oldLine, oldSt, oldOut, _)
                 | sameText oldLine line && oldSt == st && oldOut == st' -> go (i + 1) acc
               _ -> let (runs, out) = lexLineRuns (hcLang c) st line
                    in go (i + 1) (Map.insert i (line, st, out, runs) acc)

-- | Compact tokens cached for a line in the most recently requested window.
hlTokenRuns :: HlCache -> Int -> [TokRun]
hlTokenRuns c i = case Map.lookup i (hcTokens c) of
  Just (_, _, _, runs) -> runs
  Nothing              -> []

-- | The lexer state at the start of line @i@. Only meaningful when a
-- 'refreshHlCache' covered line @i-1@; anything else gets 'initialState'.
hlStateBefore :: HlCache -> Int -> HlState
hlStateBefore c i
  | i <= 0 = initialState
  | i - 1 < Seq.length (hcStates c) = Seq.index (hcStates c) (i - 1)
  | otherwise = initialState

-- | How many leading lines the cache holds trusted states for.
hlCoverage :: HlCache -> Int
hlCoverage = Seq.length . hcStates

-- Drop cache entries invalidated by whatever changed since the last sync.
-- Entry i depends only on lines 0..i, so entries before the first changed
-- line survive any edit (including line inserts/deletes). When exactly one
-- line changed in place, the old entries stay index-aligned and become the
-- re-convergence tail.
syncCache :: Lang -> Seq Text -> Maybe HlCache -> HlCache
syncCache lang cur Nothing = freshCache lang cur
syncCache lang cur (Just c)
  | hcLang c /= lang      = freshCache lang cur
  | ptrEq (hcLines c) cur = c
  | otherwise =
      let old   = hcLines c
          pairs = zip (toList old) (toList cur)
          f     = length (takeWhile (uncurry sameText) pairs)
          singleLine = Seq.length old == Seq.length cur
                       && f < Seq.length cur
                       && all (uncurry sameText) (drop (f + 1) pairs)
          -- Prefer the longer of the trusted states / previous tail as the new
          -- tail; the floor accumulates so entries are only adopted below every
          -- line edited since they were computed.
          (tl, fl)
            | not singleLine = (Seq.empty, 0)
            | Seq.length (hcStates c) >= Seq.length (hcTail c) = (hcStates c, f)
            | otherwise = (hcTail c, max (hcTailFloor c) f)
      in HlCache lang cur (Seq.take f (hcStates c)) tl fl (-1)
           (Map.filterWithKey (\i _ -> i < f) (hcTokens c))

-- Lex forward until line @target@ is covered, adopting the old tail when the
-- computed state re-converges with it.
extendTo :: Int -> HlCache -> HlCache
extendTo target c0 = start c0
  where
    start c =
      let sts  = hcStates c
          n    = Seq.length sts
          prev = if n == 0 then initialState else Seq.index sts (n - 1)
      in go c sts prev (toList (Seq.drop n (hcLines c)))
    stop = min target (Seq.length (hcLines c0) - 1)
    go c sts _ [] = c { hcStates = sts }
    go c sts prev (ln : rest)
      | Seq.length sts > stop = c { hcStates = sts }
      | otherwise =
          let i   = Seq.length sts
              st' = snd (lexLineRuns (hcLang c) prev ln)
              tl  = hcTail c
          in if i >= hcTailFloor c && i + 1 < Seq.length tl && Seq.index tl i == st'
               then start c { hcStates = (sts Seq.|> st') Seq.>< Seq.drop (i + 1) tl
                            , hcTail = Seq.empty, hcTailFloor = 0 }
               else go c (sts Seq.|> st') st' rest

------------------------------------------------------------------------------
-- Generic char-stepping driver

-- A step consumes >=1 chars from the front of the remaining string and returns
-- (count, token for those chars, new state).
type Step = HlState -> String -> (Int, Tok, HlState)

-- The lexer steps already return a run length. Accumulate those runs directly,
-- coalescing adjacent steps with the same token class; this is the allocation
-- saved by the renderer path.
lexWithRuns :: Step -> HlState -> Text -> ([TokRun], HlState)
lexWithRuns step st0 line = loop st0 (T.unpack line) []
  where
    loop st [] acc = (reverse acc, st)
    loop st cs acc =
      let (n, tok, st') = step st cs
          n' = max 1 (clampLen n cs)
          acc' = case acc of
            TokRun k old : rest | old == tok -> TokRun (k + n') old : rest
            _                                -> TokRun n' tok : acc
      in loop st' (drop n' cs) acc'

-- @min n (length xs)@, stopping as soon as @n@ elements have been seen. The
-- clamp exists to survive a step that over-reports its count; measuring the
-- whole remainder to find that out made every lexer O(line²) — a 3000-column
-- minified line cost ~800ms per frame, which is a hung editor. Steps never
-- over-report in practice, so the bounded walk is pure insurance.
clampLen :: Int -> [a] -> Int
clampLen n xs0 = go 0 xs0
  where
    go !k _ | k >= n = n
    go !k [] = k
    go !k (_ : r) = go (k + 1) r

-- Index of the first occurrence of a substring.
findSub :: String -> String -> Maybe Int
findSub needle = go 0
  where
    go _ [] = Nothing
    go i hay@(_ : t)
      | needle `isPrefixOf` hay = Just i
      | otherwise               = go (i + 1) t

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

isNumChar :: Char -> Bool
isNumChar c = isAlphaNum c || c == '.'

------------------------------------------------------------------------------
-- SQL (PostgreSQL)

sqlStep :: Step
sqlStep st cs = case st of
  StBlock -> closeWith "*/" StBlock TkComment cs
  StString q -> sqlScanString q ('x' : cs) True   -- continuation; pretend an opener
  _ -> sqlNormal cs

sqlNormal :: String -> (Int, Tok, HlState)
sqlNormal cs@(c : _)
  | "--" `isPrefixOf` cs = (length cs, TkComment, StNormal)
  | "/*" `isPrefixOf` cs = case findSub "*/" (drop 2 cs) of
      Just i  -> (2 + i + 2, TkComment, StNormal)
      Nothing -> (length cs, TkComment, StBlock)
  | c == '\'' = sqlScanString '\'' cs False
  -- A PostgreSQL dollar-quote ($$ or $tag$) is a delimiter; the body between
  -- delimiters is plpgsql/SQL code, so we colour only the marker and keep
  -- lexing the body as SQL rather than as one big string.
  | Just tag <- dollarTag cs = (length tag, TkString, StNormal)
  | c == '"' = let body = takeWhile (/= '"') (drop 1 cs)
                   n = length body + 2
               in (n, TkType, StNormal)
  | isDigit c = (length (takeWhile isNumChar cs), TkNumber, StNormal)
  | isIdentStart c =
      let w = takeWhile isIdentChar cs
      in (length w, classifySql w, StNormal)
  | otherwise = (1, TkText, StNormal)
sqlNormal [] = (0, TkText, StNormal)

-- Scan a single-quoted string with '' as the escape; cont=True when resuming.
sqlScanString :: Char -> String -> Bool -> (Int, Tok, HlState)
sqlScanString q cs cont = go (if cont then 0 else 1) (drop (if cont then 1 else 1) cs)
  where
    go acc (a : b : rest)
      | a == q && b == q = go (acc + 2) rest
    go acc (a : rest)
      | a == q    = (acc + 1, TkString, StNormal)
      | otherwise = go (acc + 1) rest
    go acc [] = (acc, TkString, StString q)

-- A PostgreSQL dollar-quote opener like @$$@ or @$tag$@ at the front.
dollarTag :: String -> Maybe String
dollarTag ('$' : rest) =
  let (word, after) = span isIdentChar rest
  in case after of
       ('$' : _) -> Just ('$' : word ++ "$")
       _         -> Nothing
dollarTag _ = Nothing

-- Close a block-style construct, consuming up to and including the terminator;
-- stay in @contSt@ if the terminator is not found on this line.
closeWith :: String -> HlState -> Tok -> String -> (Int, Tok, HlState)
closeWith term contSt tok cs = case findSub term cs of
  Just i  -> (i + length term, tok, StNormal)
  Nothing -> (length cs, tok, contSt)

classifySql :: String -> Tok
classifySql w
  | lw `Set.member` sqlKeywords = TkKeyword
  | lw `Set.member` sqlTypes    = TkType
  | otherwise                   = TkText
  where lw = map toLower w

sqlKeywords :: Set.Set String
sqlKeywords = Set.fromList
  [ "select","from","where","insert","into","values","update","set","delete"
  , "create","table","alter","drop","add","column","constraint","primary","key"
  , "foreign","references","unique","not","null","default","check","index","view"
  , "join","inner","left","right","full","outer","cross","on","using","natural"
  , "group","by","having","order","asc","desc","limit","offset","distinct","all"
  , "union","intersect","except","as","and","or","in","like","ilike","between"
  , "is","exists","case","when","then","else","end","begin","commit","rollback"
  , "transaction","grant","revoke","with","returning","function","returns","language"
  , "declare","loop","if","elsif","while","for","return","trigger","before","after"
  , "each","row","execute","procedure","cast","coalesce","nullif","using","do"
  , "schema","sequence","temporary","temp","analyze","vacuum","explain","truncate"
  , "primary","cascade","restrict","replace","materialized","window","over","partition"
  -- plpgsql / function keywords
  , "setof","perform","raise","notice","warning","exception","exit","continue"
  , "found","strict","out","inout","variadic","call","next","query","foreach"
  , "diagnostics","get","assert","reverse","slice","by","into","new","old"
  , "true","false","unknown","of" ]

sqlTypes :: Set.Set String
sqlTypes = Set.fromList
  [ "int","integer","smallint","bigint","serial","bigserial","numeric","decimal"
  , "real","double","precision","money","char","varchar","character","varying"
  , "text","bytea","boolean","bool","date","time","timestamp","timestamptz"
  , "interval","uuid","json","jsonb","xml","array","point","inet","cidr","macaddr"
  , "tsvector","tsquery","oid","name","float","float4","float8"
  , "record","void","trigger","rowtype","anyelement","anyarray","regclass"
  , "regtype","regproc","bit","box","circle","line","lseg","path","polygon" ]

------------------------------------------------------------------------------
-- Python

pyStep :: Step
pyStep st cs = case st of
  StTriple q -> case findSub [q, q, q] cs of
    Just i  -> (i + 3, TkString, StNormal)
    Nothing -> (length cs, TkString, StTriple q)
  StString q -> pyScanString q ('x' : cs) True
  _ -> pyNormal cs

pyNormal :: String -> (Int, Tok, HlState)
pyNormal cs@(c : _)
  | c == '#' = (length cs, TkComment, StNormal)
  | Just (q, pre) <- pyTripleStart cs =
      case findSub [q, q, q] (drop (pre + 3) cs) of
        Just i  -> (pre + 3 + i + 3, TkString, StNormal)
        Nothing -> (length cs, TkString, StTriple q)
  | Just (q, pre) <- pyStrStart cs = pyScanFrom q pre cs
  | c == '@' && atLineDecorator cs = (length (takeWhile (\x -> isIdentChar x || x == '.') cs), TkDecorator, StNormal)
  | isDigit c = (length (takeWhile isNumChar cs), TkNumber, StNormal)
  | isIdentStart c =
      let w = takeWhile isIdentChar cs in (length w, classifyPy w, StNormal)
  | otherwise = (1, TkText, StNormal)
pyNormal [] = (0, TkText, StNormal)

-- A string opener possibly preceded by r/b/f/u prefixes; returns (quote, prefixLen).
pyStrStart :: String -> Maybe (Char, Int)
pyStrStart cs =
  let (pre, rest) = span (`elem` ("rbfuRBFU" :: String)) cs
  in case rest of
       (q : _) | q == '"' || q == '\'' , length pre <= 2 -> Just (q, length pre)
       _ -> Nothing

pyTripleStart :: String -> Maybe (Char, Int)
pyTripleStart cs = case pyStrStart cs of
  Just (q, pre) | take 3 (drop pre cs) == [q, q, q] -> Just (q, pre)
  _ -> Nothing

pyScanFrom :: Char -> Int -> String -> (Int, Tok, HlState)
pyScanFrom q pre cs = go (pre + 1) (drop (pre + 1) cs)
  where
    go acc ('\\' : _ : rest) = go (acc + 2) rest
    go acc (a : rest) | a == q    = (acc + 1, TkString, StNormal)
                      | otherwise = go (acc + 1) rest
    go acc [] = (acc, TkString, StString q)

pyScanString :: Char -> String -> Bool -> (Int, Tok, HlState)
pyScanString q cs _ = go 0 (drop 1 cs)
  where
    go acc ('\\' : _ : rest) = go (acc + 2) rest
    go acc (a : rest) | a == q    = (acc + 1, TkString, StNormal)
                      | otherwise = go (acc + 1) rest
    go acc [] = (acc, TkString, StString q)

atLineDecorator :: String -> Bool
atLineDecorator ('@' : c : _) = isIdentStart c
atLineDecorator _ = False

classifyPy :: String -> Tok
classifyPy w
  | w `Set.member` pyKeywords = TkKeyword
  | w `Set.member` pyBuiltins = TkBuiltin
  | otherwise                 = TkText

pyKeywords :: Set.Set String
pyKeywords = Set.fromList
  [ "def","class","return","if","elif","else","for","while","break","continue"
  , "import","from","as","pass","with","try","except","finally","raise","yield"
  , "lambda","global","nonlocal","assert","del","in","is","not","and","or"
  , "async","await","match","case" ]

pyBuiltins :: Set.Set String
pyBuiltins = Set.fromList
  [ "True","False","None","self","cls","print","len","range","int","str","float"
  , "bool","list","dict","set","tuple","object","type","super","isinstance"
  , "enumerate","zip","map","filter","open","input","sorted","sum","min","max"
  , "abs","round","format","repr","hasattr","getattr","setattr","Exception" ]

------------------------------------------------------------------------------
-- HTML

htmlStep :: Step
htmlStep st cs = case st of
  StBlock -> closeWith "-->" StBlock TkComment cs
  StTag   -> htmlInTag cs
  _       -> htmlNormal cs

htmlNormal :: String -> (Int, Tok, HlState)
htmlNormal cs@(c : _)
  | "<!--" `isPrefixOf` cs = case findSub "-->" (drop 4 cs) of
      Just i  -> (4 + i + 3, TkComment, StNormal)
      Nothing -> (length cs, TkComment, StBlock)
  | c == '<' =
      let slash = if take 1 (drop 1 cs) == "/" then 1 else 0
          nameLen = length (takeWhile isIdentChar (drop (1 + slash) cs))
      in (1 + slash + nameLen, TkTag, StTag)   -- '<', optional '/', tag name
  | c == '&' = let ent = takeWhile (/= ';') cs
                   n = if ';' `elem` cs && length ent < 12 then length ent + 1 else 1
               in (n, TkType, StNormal)
  | otherwise = let n = length (takeWhile (\x -> x /= '<' && x /= '&') cs)
                in (max 1 n, TkText, StNormal)
htmlNormal [] = (0, TkText, StNormal)

-- Inside a tag: attribute names, '=' , quoted values, and the closing '>'.
htmlInTag :: String -> (Int, Tok, HlState)
htmlInTag cs@(c : _)
  | c == '>'           = (1, TkTag, StNormal)
  | c == '/'           = (1, TkTag, StTag)
  | isSpace c          = (length (takeWhile isSpace cs), TkText, StTag)
  | c == '"' || c == '\'' =
      let body = takeWhile (/= c) (drop 1 cs)
          closed = length body < length (drop 1 cs)
          n = length body + (if closed then 2 else 1)
      in (n, TkString, StTag)
  | c == '='           = (1, TkText, StTag)
  | isIdentStart c     = (length (takeWhile (\x -> isIdentChar x || x == '-' || x == ':') cs), TkAttr, StTag)
  | otherwise          = (1, TkText, StTag)
htmlInTag [] = (0, TkText, StTag)

------------------------------------------------------------------------------
-- Markdown (line-oriented, with inline spans)

lexMarkdown :: HlState -> Text -> ([Tok], HlState)
lexMarkdown StFence line =
  let s = T.unpack line
  in if isFence s then (replicate (length s) TkCode, StNormal)
                  else (replicate (length s) TkCode, StFence)
lexMarkdown StNormal line =
  let s = T.unpack line
      trimmed = dropWhile isSpace s
  in if isFence trimmed then (replicate (length s) TkCode, StFence)
     else if take 1 trimmed == "#" then (replicate (length s) TkHeading, StNormal)
     else if take 1 trimmed == ">" then (replicate (length s) TkComment, StNormal)
     else (mdInline s, StNormal)
lexMarkdown st line = (replicate (T.length line) TkText, st)

isFence :: String -> Bool
isFence s = "```" `isPrefixOf` dropWhile isSpace s || "~~~" `isPrefixOf` dropWhile isSpace s

-- Inline markdown: code spans, bold/italic, links, list markers.
mdInline :: String -> [Tok]
mdInline = goStart True
  where
    -- At line start, recognise list markers and leading spaces.
    goStart atStart s@(c : _)
      | atStart, (sp, rest) <- span (== ' ') s, not (null sp) =
          replicate (length sp) TkText ++ goStart True rest
      | atStart, c `elem` ("-*+" :: String), take 1 (drop 1 s) == " " =
          TkKeyword : go (drop 1 s)
      | otherwise = go s
    goStart _ [] = []

    go [] = []
    go ('`' : rest) =
      let (code, after) = break (== '`') rest
      in case after of
           ('`' : more) -> TkCode : replicate (length code) TkCode ++ [TkCode] ++ go more
           _            -> TkCode : replicate (length code) TkCode
    go ('*' : '*' : rest) = spanUntil "**" TkStrong rest 2
    go ('_' : '_' : rest) = spanUntil "__" TkStrong rest 2
    go ('*' : rest)       = spanUntil "*" TkEmph rest 1
    go ('[' : rest) =
      let (txt, after) = break (== ']') rest
      in case after of
           (']' : '(' : more) ->
             let (url, after2) = break (== ')') more
             in case after2 of
                  (')' : r2) -> replicate (1 + length txt + 2) TkLink
                                ++ replicate (length url) TkLink ++ [TkLink] ++ go r2
                  _          -> TkLink : map (const TkLink) txt
           _ -> TkText : go rest
    go (_ : rest) = TkText : go rest

    -- Emit a span up to and including the closing delimiter, as one token kind.
    spanUntil delim tok rest opened = case findSub delim rest of
      Just i  -> replicate (opened + i + length delim) tok
                 ++ go (drop (i + length delim) rest)
      Nothing -> replicate opened tok ++ map (const tok) rest

------------------------------------------------------------------------------
-- Shared scanners for the languages below

-- Quoted string with backslash escapes; an unterminated one continues as
-- StString on the next line.
goEsc :: Char -> Int -> String -> (Int, Tok, HlState)
goEsc q acc ('\\' : _ : rest) = goEsc q (acc + 2) rest
goEsc q acc (a : rest)
  | a == q    = (acc + 1, TkString, StNormal)
  | otherwise = goEsc q (acc + 1) rest
goEsc q acc [] = (acc, TkString, StString q)

strEsc :: Char -> String -> (Int, Tok, HlState)
strEsc q cs = goEsc q 1 (drop 1 cs)

-- Quoted string with no escapes (shell single quotes).
goNoEsc :: Char -> Int -> String -> (Int, Tok, HlState)
goNoEsc q acc (a : rest)
  | a == q    = (acc + 1, TkString, StNormal)
  | otherwise = goNoEsc q (acc + 1) rest
goNoEsc q acc [] = (acc, TkString, StString q)

strNoEsc :: Char -> String -> (Int, Tok, HlState)
strNoEsc q cs = goNoEsc q 1 (drop 1 cs)

numTok :: String -> (Int, Tok, HlState)
numTok cs =
  (max 1 (length (takeWhile (\x -> isAlphaNum x || x == '.' || x == '_') cs)), TkNumber, StNormal)

classify3 :: Set.Set String -> Set.Set String -> Set.Set String -> String -> Tok
classify3 kw ty bi w
  | w `Set.member` kw = TkKeyword
  | w `Set.member` ty = TkType
  | w `Set.member` bi = TkBuiltin
  | otherwise         = TkText

isHex :: Char -> Bool
isHex c = isDigit c || (toLower c >= 'a' && toLower c <= 'f')

oneOf :: Char -> String -> Bool
oneOf = elem

------------------------------------------------------------------------------
-- JavaScript / TypeScript (.js .mjs .cjs .jsx .ts .tsx)

jsStep :: Step
jsStep st cs = case st of
  StBlock    -> closeWith "*/" StBlock TkComment cs
  StTemplate -> jsTemplate 0 cs
  StString q -> goEsc q 0 cs
  _          -> jsNormal cs

jsNormal :: String -> (Int, Tok, HlState)
jsNormal cs@(c : _)
  | "//" `isPrefixOf` cs = (length cs, TkComment, StNormal)
  | "/*" `isPrefixOf` cs = case findSub "*/" (drop 2 cs) of
      Just i  -> (2 + i + 2, TkComment, StNormal)
      Nothing -> (length cs, TkComment, StBlock)
  | c == '`'  = jsTemplate 1 (drop 1 cs)
  | c == '\'' || c == '"' = strEsc c cs
  | c == '@' && jsDecorator cs =
      (length (takeWhile (\x -> isIdentChar x || x == '.') cs), TkDecorator, StNormal)
  | isDigit c = numTok cs
  | isIdentStart c || c == '$' =
      let w = takeWhile (\x -> isIdentChar x || x == '$') cs
      in (length w, classify3 jsKeywords jsTypes jsConsts w, StNormal)
  | otherwise = (1, TkText, StNormal)
jsNormal [] = (0, TkText, StNormal)

jsTemplate :: Int -> String -> (Int, Tok, HlState)
jsTemplate acc ('\\' : _ : rest) = jsTemplate (acc + 2) rest
jsTemplate acc ('`' : _)         = (acc + 1, TkString, StNormal)
jsTemplate acc (_ : rest)        = jsTemplate (acc + 1) rest
jsTemplate acc []                = (acc, TkString, StTemplate)

jsDecorator :: String -> Bool
jsDecorator ('@' : c : _) = isIdentStart c
jsDecorator _ = False

jsKeywords, jsTypes, jsConsts :: Set.Set String
jsKeywords = Set.fromList
  [ "function","return","if","else","for","while","do","switch","case","break"
  , "continue","var","let","const","class","extends","new","super","import"
  , "export","from","default","try","catch","finally","throw","typeof"
  , "instanceof","in","of","delete","void","yield","async","await","static"
  , "get","set","with","debugger","interface","type","enum","namespace"
  , "declare","public","private","protected","readonly","implements","abstract"
  , "as","is","keyof","infer","satisfies","override","module","require" ]
jsTypes = Set.fromList
  [ "string","number","boolean","any","unknown","never","void","object"
  , "symbol","bigint","Array","Promise","Record","Map","Set","Object","String"
  , "Number","Boolean","Date","RegExp","Error","Partial","Readonly" ]
jsConsts = Set.fromList
  [ "true","false","null","undefined","NaN","Infinity","this","arguments" ]

------------------------------------------------------------------------------
-- CSS / SCSS / LESS

cssStep :: Step
cssStep st cs = case st of
  StBlock    -> closeWith "*/" StBlock TkComment cs
  StString q -> goEsc q 0 cs
  _          -> cssNormal cs

cssNormal :: String -> (Int, Tok, HlState)
cssNormal cs@(c : _)
  | "/*" `isPrefixOf` cs = case findSub "*/" (drop 2 cs) of
      Just i  -> (2 + i + 2, TkComment, StNormal)
      Nothing -> (length cs, TkComment, StBlock)
  | "//" `isPrefixOf` cs = (length cs, TkComment, StNormal)
  | "--" `isPrefixOf` cs =
      (length (takeWhile (\x -> isIdentChar x || x == '-') cs), TkType, StNormal)
  | c == '\'' || c == '"' = strEsc c cs
  | c == '#' =
      let h = takeWhile isHex (drop 1 cs); n = length h
      in if n `elem` [3,4,6,8]
           then (1 + n, TkNumber, StNormal)
           else (1 + length (takeWhile isIdentChar (drop 1 cs)), TkKeyword, StNormal)
  | c == '$' =
      (1 + length (takeWhile (\x -> isIdentChar x || x == '-') (drop 1 cs)), TkType, StNormal)
  | c == '@' =
      (1 + length (takeWhile (\x -> isIdentChar x || x == '-') (drop 1 cs)), TkKeyword, StNormal)
  | c == '!' = (length (takeWhile (\x -> isIdentChar x || x == '!') cs), TkKeyword, StNormal)
  | isDigit c =
      (length (takeWhile (\x -> isAlphaNum x || x == '.' || x == '%') cs), TkNumber, StNormal)
  | isIdentStart c =
      let w = takeWhile (\x -> isIdentChar x || x == '-') cs
          after = dropWhile (== ' ') (drop (length w) cs)
      in if take 1 after == ":" then (length w, TkProperty, StNormal)
                                else (length w, TkText, StNormal)
  | otherwise = (1, TkText, StNormal)
cssNormal [] = (0, TkText, StNormal)

------------------------------------------------------------------------------
-- Shell (.sh .bash)

shStep :: Step
shStep st cs = case st of
  StString q -> goEsc q 0 cs
  _          -> shNormal cs

shNormal :: String -> (Int, Tok, HlState)
shNormal cs@(c : _)
  | c == '#'  = (length cs, TkComment, StNormal)
  | c == '\'' = strNoEsc '\'' cs
  | c == '"'  = strEsc '"' cs
  | c == '$'  = shVar cs
  | isDigit c = numTok cs
  | isIdentStart c =
      let w = takeWhile isIdentChar cs
      in (length w, classify3 shKeywords Set.empty shBuiltins w, StNormal)
  | otherwise = (1, TkText, StNormal)
shNormal [] = (0, TkText, StNormal)

shVar :: String -> (Int, Tok, HlState)
shVar cs = case drop 1 cs of
  ('{' : rest) -> let body = takeWhile (/= '}') rest
                      closed = length body < length rest
                  in (2 + length body + (if closed then 1 else 0), TkType, StNormal)
  (d : _)
    | isIdentStart d -> (1 + length (takeWhile isIdentChar (drop 1 cs)), TkType, StNormal)
    | d `oneOf` "#@*?$!0123456789-" -> (2, TkType, StNormal)
  _ -> (1, TkText, StNormal)

shKeywords, shBuiltins :: Set.Set String
shKeywords = Set.fromList
  [ "if","then","else","elif","fi","for","while","until","do","done","case"
  , "esac","function","in","select","return","break","continue","local"
  , "export","readonly","declare","unset","shift","source","exit","trap","set" ]
shBuiltins = Set.fromList
  [ "echo","printf","read","cd","pwd","ls","cat","grep","sed","awk","cut","sort"
  , "uniq","head","tail","find","test","true","false","exec","eval","sleep","mkdir" ]

------------------------------------------------------------------------------
-- JSON

jsonStep :: Step
jsonStep st cs = case st of
  StString q -> goEsc q 0 cs
  _          -> jsonNormal cs

jsonNormal :: String -> (Int, Tok, HlState)
jsonNormal cs@(c : _)
  | c == '"' =
      let (n, _, st') = strEsc '"' cs
          after = dropWhile (== ' ') (drop n cs)
      in if take 1 after == ":" then (n, TkProperty, st') else (n, TkString, st')
  | "//" `isPrefixOf` cs = (length cs, TkComment, StNormal)
  | isDigit c || c == '-' =
      (length (takeWhile (\x -> isAlphaNum x || x `oneOf` ".+-") cs), TkNumber, StNormal)
  | isIdentStart c =
      let w = takeWhile isIdentChar cs
      in (length w, if w `elem` ["true","false","null"] then TkBuiltin else TkText, StNormal)
  | otherwise = (1, TkText, StNormal)
jsonNormal [] = (0, TkText, StNormal)

------------------------------------------------------------------------------
-- YAML

yamlStep :: Step
yamlStep st cs = case st of
  StString q -> goEsc q 0 cs
  _          -> yamlNormal cs

yamlNormal :: String -> (Int, Tok, HlState)
yamlNormal cs@(c : _)
  | c == '#'  = (length cs, TkComment, StNormal)
  | c == '\'' || c == '"' = strEsc c cs
  | c == '-' && (take 2 cs == "- " || cs == "-") = (1, TkPunct, StNormal)
  | c == '&' || c == '*' =
      (1 + length (takeWhile isIdentChar (drop 1 cs)), TkType, StNormal)
  | isDigit c = numTok cs
  | isIdentStart c =
      let w = takeWhile (\x -> isIdentChar x || x `oneOf` "-.") cs
          after = drop (length w) cs
      in if take 1 after == ":" && (take 2 after == ": " || after == ":")
           then (length w, TkProperty, StNormal)
         else if map toLower w `elem` ["true","false","null","yes","no","on","off","none"]
           then (length w, TkBuiltin, StNormal)
         else (length w, TkText, StNormal)
  | otherwise = (1, TkText, StNormal)
yamlNormal [] = (0, TkText, StNormal)

------------------------------------------------------------------------------
-- TOML

tomlStep :: Step
tomlStep st cs = case st of
  StString q -> goEsc q 0 cs
  _          -> tomlNormal cs

tomlNormal :: String -> (Int, Tok, HlState)
tomlNormal cs@(c : _)
  | c == '#'  = (length cs, TkComment, StNormal)
  | c == '['  = (1 + length (takeWhile (/= ']') (drop 1 cs))
                   + (if ']' `elem` drop 1 cs then 1 else 0), TkKeyword, StNormal)
  | c == '\'' || c == '"' = strEsc c cs
  | isDigit c =
      (length (takeWhile (\x -> isAlphaNum x || x `oneOf` ".:+-_") cs), TkNumber, StNormal)
  | isIdentStart c =
      let w = takeWhile (\x -> isIdentChar x || x == '-') cs
          after = dropWhile (== ' ') (drop (length w) cs)
      in if take 1 after == "=" then (length w, TkProperty, StNormal)
         else if w `elem` ["true","false"] then (length w, TkBuiltin, StNormal)
         else (length w, TkText, StNormal)
  | otherwise = (1, TkText, StNormal)
tomlNormal [] = (0, TkText, StNormal)

------------------------------------------------------------------------------
-- INI / conf (.ini .conf .cfg)

iniStep :: Step
iniStep st cs = case st of
  StString q -> goEsc q 0 cs
  _          -> iniNormal cs

iniNormal :: String -> (Int, Tok, HlState)
iniNormal cs@(c : _)
  | c == ';' || c == '#' = (length cs, TkComment, StNormal)
  | c == '['  = (1 + length (takeWhile (/= ']') (drop 1 cs))
                   + (if ']' `elem` drop 1 cs then 1 else 0), TkKeyword, StNormal)
  | c == '\'' || c == '"' = strEsc c cs
  | isDigit c = numTok cs
  | isIdentStart c || c == '.' =
      let w = takeWhile (\x -> isIdentChar x || x `oneOf` ".-") cs
          after = dropWhile (== ' ') (drop (length w) cs)
      in if take 1 after `elem` ["=",":"] then (length w, TkProperty, StNormal)
                                          else (length w, TkText, StNormal)
  | otherwise = (1, TkText, StNormal)
iniNormal [] = (0, TkText, StNormal)

------------------------------------------------------------------------------
-- FreeMarker (.ftl)

ftlStep :: Step
ftlStep st cs = case st of
  StBlock    -> closeWith "-->" StBlock TkComment cs
  StTag      -> htmlInTag cs
  StString q -> goEsc q 0 cs
  _          -> ftlNormal cs

ftlNormal :: String -> (Int, Tok, HlState)
ftlNormal cs@(c : _)
  | "<#--" `isPrefixOf` cs || "<!--" `isPrefixOf` cs =
      case findSub "-->" (drop 4 cs) of
        Just i  -> (4 + i + 3, TkComment, StNormal)
        Nothing -> (length cs, TkComment, StBlock)
  | "${" `isPrefixOf` cs =
      let body = takeWhile (/= '}') (drop 2 cs)
          closed = '}' `elem` drop 2 cs
      in (2 + length body + (if closed then 1 else 0), TkType, StNormal)
  | "<#" `isPrefixOf` cs || "</#" `isPrefixOf` cs
    || "<@" `isPrefixOf` cs || "</@" `isPrefixOf` cs =
      let pre = length (takeWhile (`oneOf` "</#@") cs)
          nm  = takeWhile isIdentChar (drop pre cs)
      in (pre + length nm, TkKeyword, StTag)
  | c == '<' =
      let slash = if take 1 (drop 1 cs) == "/" then 1 else 0
          nm = takeWhile isIdentChar (drop (1 + slash) cs)
      in (1 + slash + length nm, TkTag, StTag)
  | otherwise =
      (max 1 (length (takeWhile (\x -> x /= '<' && x /= '$') cs)), TkText, StNormal)
ftlNormal [] = (0, TkText, StNormal)

------------------------------------------------------------------------------
-- Jinja (.jinja .jinja2 .j2)

jinjaStep :: Step
jinjaStep st cs = case st of
  StBlock        -> closeWith "-->" StBlock TkComment cs
  StJinjaComment -> closeWith "#}" StJinjaComment TkComment cs
  StTag          -> htmlInTag cs
  StString q     -> goEsc q 0 cs
  _              -> jinjaNormal cs

jinjaNormal :: String -> (Int, Tok, HlState)
jinjaNormal cs@(c : _)
  | "{#" `isPrefixOf` cs = case findSub "#}" (drop 2 cs) of
      Just i  -> (2 + i + 2, TkComment, StNormal)
      Nothing -> (length cs, TkComment, StJinjaComment)
  | "{{" `isPrefixOf` cs = jinjaSpan "}}" TkType cs
  | "{%" `isPrefixOf` cs = jinjaSpan "%}" TkKeyword cs
  | "<!--" `isPrefixOf` cs = case findSub "-->" (drop 4 cs) of
      Just i  -> (4 + i + 3, TkComment, StNormal)
      Nothing -> (length cs, TkComment, StBlock)
  | c == '<' =
      let slash = if take 1 (drop 1 cs) == "/" then 1 else 0
          nm = takeWhile isIdentChar (drop (1 + slash) cs)
      in (1 + slash + length nm, TkTag, StTag)
  | otherwise =
      (max 1 (length (takeWhile (\x -> x /= '<' && x /= '{') cs)), TkText, StNormal)
jinjaNormal [] = (0, TkText, StNormal)

jinjaSpan :: String -> Tok -> String -> (Int, Tok, HlState)
jinjaSpan close tok cs = case findSub close (drop 2 cs) of
  Just i  -> (2 + i + length close, tok, StNormal)
  Nothing -> (length cs, tok, StNormal)

------------------------------------------------------------------------------
-- Haskell (.hs)

hsStep :: Step
hsStep st cs = case st of
  StNestComment d -> hsBlock d cs
  StString q      -> goEsc q 0 cs   -- resume a string continued over a gap
  _               -> hsNormal cs

hsNormal :: String -> (Int, Tok, HlState)
hsNormal cs@(c : _)
  -- A pragma @{-# ... #-}@ that closes on this line is coloured as a unit.
  | "{-#" `isPrefixOf` cs, Just i <- findSub "#-}" (drop 3 cs) =
      (3 + i + 3, TkDecorator, StNormal)
  -- A @{- ... -}@ block comment; Haskell nests these, so track the depth.
  | "{-" `isPrefixOf` cs = hsBlock 0 cs
  -- A line comment is two-or-more dashes not forming part of a longer operator.
  | dashComment cs = (length cs, TkComment, StNormal)
  | c == '"'  = strEsc '"' cs
  | c == '\'' , Just n <- charLit cs = (n, TkString, StNormal)
  | isDigit c = numTok cs
  | isIdentStart c =
      let w = takeWhile isHsIdentChar cs in (length w, classifyHs w, StNormal)
  | otherwise = (1, TkText, StNormal)
hsNormal [] = (0, TkText, StNormal)

-- Scan a (possibly nested) block comment starting at the current position.
-- @depth@ is the nesting level already open (0 when we are sitting on the
-- opening @{-@). Returns to 'StNormal' once every @{-@ is balanced by @-}@.
hsBlock :: Int -> String -> (Int, Tok, HlState)
hsBlock depth0 = scan depth0 0
  where
    scan d n ('{' : '-' : rest) = scan (d + 1) (n + 2) rest
    scan d n ('-' : '}' : rest)
      | d - 1 <= 0 = (n + 2, TkComment, StNormal)
      | otherwise  = scan (d - 1) (n + 2) rest
    scan d n (_ : rest) = scan d (n + 1) rest
    scan d n []         = (n, TkComment, StNestComment (max 1 d))

-- True when @cs@ begins a line comment: a run of >=2 dashes whose following
-- character is not an operator symbol (so @-->@ and @--|@ stay operators).
dashComment :: String -> Bool
dashComment cs =
  let (dashes, rest) = span (== '-') cs
  in length dashes >= 2 && case rest of
       (r : _) -> not (isOpSym r)
       []      -> True

isOpSym :: Char -> Bool
isOpSym c = c `oneOf` "!#$%&*+./<=>?@\\^|-~:"

-- A character literal: @'a'@, @'\n'@, @'\\'@, @'\NUL'@, etc. Returns its length.
charLit :: String -> Maybe Int
charLit ('\'' : rest) = case rest of
  ('\\' : r) -> let (esc, after) = break (== '\'') r
                in case after of
                     ('\'' : _) -> Just (2 + length esc + 1)
                     _          -> Nothing
  (_ : '\'' : _) -> Just 3
  _              -> Nothing
charLit _ = Nothing

-- Identifiers may contain trailing primes (e.g. @foldl'@, @x''@).
isHsIdentChar :: Char -> Bool
isHsIdentChar c = isIdentChar c || c == '\''

classifyHs :: String -> Tok
classifyHs w
  | w `Set.member` hsKeywords = TkKeyword
  | isUpper (head w)          = TkType          -- constructors, types, modules
  | w `Set.member` hsBuiltins = TkBuiltin
  | otherwise                 = TkText

hsKeywords :: Set.Set String
hsKeywords = Set.fromList
  [ "case","class","data","default","deriving","do","else","foreign","if"
  , "import","in","infix","infixl","infixr","instance","let","module"
  , "newtype","of","then","type","where"
  -- contextual and extension keywords commonly highlighted
  , "as","hiding","qualified","forall","family","mdo","proc","rec","pattern"
  , "role","stock","anyclass","via" ]

hsBuiltins :: Set.Set String
hsBuiltins = Set.fromList
  [ "map","filter","foldr","foldl","foldl'","foldr'","zip","zipWith","concat"
  , "concatMap","reverse","length","head","tail","init","last","take","drop"
  , "takeWhile","dropWhile","span","break","elem","notElem","lookup","null"
  , "and","or","any","all","sum","product","maximum","minimum","fst","snd"
  , "curry","uncurry","id","const","flip","not","otherwise","error","undefined"
  , "seq","fromIntegral","fromEnum","toEnum","show","read","print","putStr"
  , "putStrLn","getLine","return","pure","fmap","mapM","mapM_","forM","forM_"
  , "sequence","sequence_","when","unless","either","maybe" ]

------------------------------------------------------------------------------
-- Rich Text Format
--
-- The raw, editable view of an @.rtf@ file (Alt+T from the formatted view).
-- RTF has no multi-line constructs to carry across lines, so this needs no
-- state: control words are keywords, their numeric parameters numbers,
-- @\'hh@ and the other control symbols escapes, and braces punctuation —
-- which leaves the document's actual text as the only unstyled thing on
-- screen, the whole point of highlighting markup.

lexRtf :: Text -> ([Tok], HlState)
lexRtf line = (go (T.unpack line), StNormal)
  where
    go [] = []
    go ('{' : rest) = TkPunct : go rest
    go ('}' : rest) = TkPunct : go rest
    go ('\\' : rest) = case rest of
      (c : more)
        | isAlpha c ->
            let (word, r1)   = span isAlpha (c : more)
                (sign, r2)   = span (== '-') r1
                (digits, r3) = span isDigit r2
            in TkKeyword : map (const TkKeyword) word
                 ++ map (const TkNumber) (sign ++ digits)
                 ++ go r3
        -- \'hh, and the one-character control symbols (\\ \{ \} \~ \- ...).
        | c == '\'' -> let (hex, r) = splitAt 2 more
                       in TkString : TkString : map (const TkString) hex ++ go r
        | otherwise -> TkString : TkString : go more
      [] -> [TkKeyword]
    go (_ : rest) = TkText : go rest

------------------------------------------------------------------------------
-- Archive listings
--
-- Not a file format: this lexes the tree "Cmedit.Zip" renders for a @.zip@ and
-- its relatives, which is the only thing the editor ever shows for one. The
-- listing is generated, so its shape is known exactly and a positional lexer
-- is enough — no state, and no guessing.
--
-- The one thing worth being careful about is where a member's name ends.
-- Names contain spaces; the gap before the size column is two or more, which
-- is what 'gapAt' looks for.

lexArchive :: Text -> ([Tok], HlState)
lexArchive line = (go (T.unpack line), StNormal)
  where
    go s
      -- The rule under the column headings.
      | not (null s) && all (== '\x2500') s = map (const TkComment) s
      -- No tree prefix: one of the summary lines above the tree.
      | null prefix = map (const TkHeading) name ++ map (const TkComment) rest
      | otherwise   = map (const TkPunct) prefix
                        ++ map (const (if last' name == '/' then TkType else TkText)) name
                        ++ map meta rest
      where
        (prefix, s')   = span isTreeChar s
        (name, rest)   = maybe (s', "") (`splitAt` s') (gapAt s')
    isTreeChar c = c `elem` ("\x2502\x251c\x2514\x2500 " :: String)
    last' xs = if null xs then ' ' else last xs
    -- Offset of the first two-space run, i.e. where the name column ends.
    gapAt = search (0 :: Int)
      where search !i s@(_ : more) = case s of
              (' ' : ' ' : _) -> Just i
              _               -> search (i + 1) more
            search _ [] = Nothing
    meta c | isDigit c = TkNumber
           | otherwise = TkComment

------------------------------------------------------------------------------
-- CSV / TSV

lexCsv :: Text -> ([Tok], HlState)
lexCsv line = (go (T.unpack line), StNormal)
  where
    go [] = []
    go ('"' : rest) =
      let (field, after) = break (== '"') rest
      in case after of
           ('"' : more) -> TkString : map (const TkString) field ++ TkString : go more
           _            -> TkString : map (const TkString) field
    go (',' : rest)  = TkPunct : go rest
    go ('\t' : rest) = TkPunct : go rest
    go (c : rest)    = (if isDigit c then TkNumber else TkText) : go rest
