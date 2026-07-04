-- | Syntax highlighting. Each supported language has a lexer that turns a line
-- of text into one 'Tok' per character, threading a small 'HlState' across
-- lines so multi-line constructs (block comments, PostgreSQL dollar-quoted
-- strings, Python triple-quoted strings, Markdown fenced code, HTML comments)
-- are highlighted correctly. The renderer maps 'Tok's to colours.
--
-- 'HlCache' keeps the per-line lexer states between frames so the renderer
-- only lexes the visible window instead of a bounded look-back. The cache is
-- self-validating: it remembers the exact line 'Seq' it was computed from and
-- locates edits itself by comparison (pointer-equality first, so unchanged
-- shared lines cost one comparison), which means no buffer-editing code path
-- has to remember to invalidate it.
{-# LANGUAGE MagicHash #-}
module Cmedit.Syntax
  ( Lang(..)
  , Tok(..)
  , HlState(..)
  , langForPath
  , initialState
  , lexLine
    -- * Comment syntax (Edit ▸ Toggle Comment)
  , CommentSyntax(..)
  , langComment
    -- * Cached highlighting state
  , HlCache
  , refreshHlCache
  , hlStateBefore
  , hlCoverage
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper, toLower)
import Data.Foldable (toList)
import Data.List (isPrefixOf)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import System.FilePath (takeExtension)

data Lang = SQL | Python | Markdown | HTML
          | JS | CSS | Shell | JSON | YAML | TOML | INI | FTL | Jinja | CSV
          | Haskell
  deriving (Eq, Show)

-- | A highlighting token class assigned to each character.
data Tok
  = TkText | TkKeyword | TkType | TkString | TkComment | TkNumber
  | TkFunction | TkBuiltin | TkDecorator | TkTag | TkAttr
  | TkHeading | TkEmph | TkStrong | TkCode | TkLink | TkPunct
  | TkProperty       -- ^ Keys / properties (config keys, CSS properties).
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
  ".csv"  -> Just CSV
  ".tsv"  -> Just CSV
  ".hs"   -> Just Haskell
  _       -> Nothing

-- | Above this length a line is rendered unstyled: tokenising a
-- multi-megabyte minified line on every frame would dwarf everything else the
-- editor does. The lexer state passes through unchanged so later lines keep
-- a sane state (a construct opened *inside* such a line is a lost cause).
maxHlLine :: Int
maxHlLine = 20000

-- | Lex one line, returning a token per character and the trailing state.
lexLine :: Lang -> HlState -> Text -> ([Tok], HlState)
lexLine _ st line
  | T.length line > maxHlLine = ([], st)
lexLine lang st line = case lang of
  SQL      -> lexWith sqlStep st line
  Python   -> lexWith pyStep st line
  HTML     -> lexWith htmlStep st line
  Markdown -> lexMarkdown st line
  JS       -> lexWith jsStep st line
  CSS      -> lexWith cssStep st line
  Shell    -> lexWith shStep st line
  JSON     -> lexWith jsonStep st line
  YAML     -> lexWith yamlStep st line
  TOML     -> lexWith tomlStep st line
  INI      -> lexWith iniStep st line
  FTL      -> lexWith ftlStep st line
  Jinja    -> lexWith jinjaStep st line
  Haskell  -> lexWith hsStep st line
  CSV      -> lexCsv line

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
  } deriving (Show)

-- GC can move objects between comparisons, so a pointer mismatch proves
-- nothing (fall back to (==)) — but a pointer match is a sound "equal".
ptrEq :: a -> a -> Bool
ptrEq a b = isTrue# (reallyUnsafePtrEquality# a b)

sameText :: Text -> Text -> Bool
sameText a b = ptrEq a b || a == b

freshCache :: Lang -> Seq Text -> HlCache
freshCache lang cur = HlCache lang cur Seq.empty Seq.empty 0

-- | Sync the cache with the buffer (locating any edits itself) and extend the
-- trusted states to cover line @target@. O(1) when the buffer is untouched
-- and the target already covered; otherwise proportional to the edited/newly
-- covered region.
refreshHlCache :: Lang -> Seq Text -> Int -> Maybe HlCache -> HlCache
refreshHlCache lang cur target mc = extendTo target (syncCache lang cur mc)

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
      in HlCache lang cur (Seq.take f (hcStates c)) tl fl

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
              st' = snd (lexLine (hcLang c) prev ln)
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

lexWith :: Step -> HlState -> Text -> ([Tok], HlState)
lexWith step st0 line = loop st0 (T.unpack line)
  where
    loop st [] = ([], st)
    loop st cs =
      let (n, tok, st') = step st cs
          n' = max 1 (min n (length cs))
          (rest, stEnd) = loop st' (drop n' cs)
      in (replicate n' tok ++ rest, stEnd)

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
