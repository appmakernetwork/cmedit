-- | A small, dependency-free regular-expression engine, written from first
-- principles (no @regex-*@ package ships with GHC). It supports the common
-- subset used for editor find/replace: literals, @.@, character classes
-- @[...]@ (ranges and negation), the escapes @\\d \\D \\w \\W \\s \\S \\b \\B@,
-- anchors @^ $@, groups @( )@ and @(?: )@, alternation @|@, and the quantifiers
-- @* + ?@ and @{n} {n,} {n,m}@ (greedy, or lazy with a trailing @?@).
--
-- The pattern is compiled to a Thompson NFA program executed by a Pike VM
-- (lock-step thread simulation with capture slots), so matching is
-- **linear-time in the line length** — a pathological pattern like @(a+)+b@
-- cannot backtrack catastrophically, and no match is ever silently dropped to
-- a step budget. Thread priority reproduces the leftmost / greedy / lazy
-- semantics a backtracker would give. Capture groups are tracked so
-- replacement templates can reference @$1@..@$9@ (and @$0@ / @$&@ for the
-- whole match). It is intentionally line-oriented (the workspace search runs
-- it per line).
module Cmedit.Regex
  ( Regex
  , compile
  , lineMatches      -- ^ non-overlapping (start,len) matches on one line
  , replaceLine      -- ^ substitute a template into one line, returning (count, newLine)
  , maxRegexLine
  ) where

import Data.Array (Array, listArray, (!), bounds)
import Data.Char (toLower, isAlphaNum, isSpace, isDigit)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | Lines longer than this are not scanned with a regex. Matching is linear,
-- so this is only a latency guard for absurd single-line files, not a
-- correctness cap — it comfortably covers minified JS/eps lines.
maxRegexLine :: Int
maxRegexLine = 1000000

-- | A compiled pattern: an NFA program for the Pike VM.
newtype Regex = Regex (Array Int Inst)

data Re
  = REmpty
  | RPred (Char -> Bool)         -- single char matching a predicate
  | RSeq [Re]
  | RAlt [Re]
  | RRep !Bool !Int !(Maybe Int) Re  -- greedy?, min, max, body
  | RGroup !Int Re
  | RStartAnchor
  | REndAnchor
  | RWordB !Bool                 -- \b (True) or \B (False)

------------------------------------------------------------------------------
-- Parser (recursive descent)

-- | Compile a pattern. Returns @Left@ with a message on a syntax error (or on
-- a bounded repetition that would expand to an absurdly large program).
compile :: Bool -> Text -> Either String Regex
compile ci pat =
  case runP pAlt (T.unpack pat) 1 of
    Left e -> Left e
    Right (re, rest, _)
      | not (null rest)          -> Left ("unexpected " ++ take 1 rest)
      | progSize re > maxInsts   -> Left "pattern too large"
      | otherwise ->
          let prog = assemble re
          in Right (Regex (listArray (0, length prog - 1) prog))
  where
    -- Parser state threads the input string and the next group number.
    runP p s g = p ci s g

type P a = Bool -> String -> Int -> Either String (a, String, Int)

pAlt :: P Re
pAlt ci s g = do
  (first, s1, g1) <- pSeq ci s g
  go [first] s1 g1
  where
    go acc ('|' : rest) gg = do
      (nx, s', g') <- pSeq ci rest gg
      go (nx : acc) s' g'
    go acc rest gg = Right (mkAlt (reverse acc), rest, gg)
    mkAlt [x] = x
    mkAlt xs  = RAlt xs

pSeq :: P Re
pSeq ci = go []
  where
    go acc s g = case s of
      []            -> done acc s g
      ('|' : _)     -> done acc s g
      (')' : _)     -> done acc s g
      _ -> do
        (atom, s1, g1) <- pQuant ci s g
        go (atom : acc) s1 g1
    done acc s g = Right (mkSeq (reverse acc), s, g)
    mkSeq [x] = x
    mkSeq xs  = RSeq xs

-- An atom followed by an optional quantifier.
pQuant :: P Re
pQuant ci s g = do
  (atom, s1, g1) <- pAtom ci s g
  case s1 of
    ('*' : r) -> lazy (RRep True 0 Nothing atom) r g1
    ('+' : r) -> lazy (RRep True 1 Nothing atom) r g1
    ('?' : r) -> lazy (RRep True 0 (Just 1) atom) r g1
    ('{' : r) -> pBrace atom r g1
    _         -> Right (atom, s1, g1)
  where
    -- a trailing '?' makes the quantifier lazy
    lazy (RRep _ lo hi body) ('?' : r) gg = Right (RRep False lo hi body, r, gg)
    lazy re rest gg = Right (re, rest, gg)
    pBrace atom r gg =
      let (digits, r1) = span isDigit r
      in case r1 of
           (',' : r2) ->
             let (digits2, r3) = span isDigit r2
             in case r3 of
                  ('}' : r4) ->
                    let lo = readBound digits
                        hi = if null digits2 then Nothing else Just (readBound digits2)
                    in lazyB (RRep True lo hi atom) r4 gg
                  _ -> Left "expected } in {n,m}"
           ('}' : r2) | not (null digits) ->
             let n = readBound digits in lazyB (RRep True n (Just n) atom) r2 gg
           _ -> Left "malformed {..} quantifier"
    -- Clamp repetition bounds so a silly {999999999999} can't overflow Int
    -- arithmetic; the program-size cap rejects anything this large anyway.
    readBound = min 10000000 . max 0 . readIntDef 0
    lazyB (RRep _ lo hi body) ('?' : r) gg = Right (RRep False lo hi body, r, gg)
    lazyB re rest gg = Right (re, rest, gg)

pAtom :: P Re
pAtom ci s g = case s of
  ('(' : '?' : ':' : r) -> do            -- non-capturing group
    (inner, r1, g1) <- pAlt ci r g
    case r1 of (')' : r2) -> Right (inner, r2, g1); _ -> Left "missing )"
  ('(' : r) -> do                        -- capturing group
    let myNum = g
    (inner, r1, g1) <- pAlt ci r (g + 1)
    case r1 of (')' : r2) -> Right (RGroup myNum inner, r2, g1); _ -> Left "missing )"
  ('[' : r)  -> pClass ci r g
  ('.' : r)  -> Right (RPred (const True), r, g)
  ('^' : r)  -> Right (RStartAnchor, r, g)
  ('$' : r)  -> Right (REndAnchor, r, g)
  ('\\' : c : r) -> Right (escToRe ci c, r, g)
  ('\\' : []) -> Left "trailing backslash"
  (c : r) | c `elem` ("*+?)" :: String) -> Left ("unexpected " ++ [c])
          | otherwise -> Right (RPred (chEq ci c), r, g)
  [] -> Right (REmpty, [], g)

-- Character class [ ... ]
pClass :: P Re
pClass ci s0 g =
  let (neg, s1) = case s0 of ('^' : r) -> (True, r); _ -> (False, s0)
  in go [] s1 neg
  where
    go preds s neg = case s of
      (']' : r) | not (null preds) || False -> Right (RPred (mkClass ci neg preds), r, g)
      [] -> Left "unterminated character class"
      ('\\' : c : r) -> go (classEsc ci c : preds) r neg
      (a : '-' : b : r) | b /= ']' -> go (rangePred ci a b : preds) r neg
      (c : r) -> go (chEq ci c : preds) r neg

mkClass :: Bool -> Bool -> [Char -> Bool] -> (Char -> Bool)
mkClass _ neg preds ch =
  let hit = any ($ ch) preds
  in if neg then not hit else hit

rangePred :: Bool -> Char -> Char -> (Char -> Bool)
rangePred ci a b ch =
  let lo = min a b; hi = max a b
      inR x = x >= lo && x <= hi
  in if ci then inR (toLower ch) || inR ch || (toLower ch >= toLower lo && toLower ch <= toLower hi)
           else inR ch

escToRe :: Bool -> Char -> Re
escToRe _ 'd' = RPred isDigit
escToRe _ 'D' = RPred (not . isDigit)
escToRe _ 'w' = RPred isWordCh
escToRe _ 'W' = RPred (not . isWordCh)
escToRe _ 's' = RPred isSpace
escToRe _ 'S' = RPred (not . isSpace)
escToRe _ 'b' = RWordB True
escToRe _ 'B' = RWordB False
escToRe ci c  = RPred (chEq ci (unesc c))

classEsc :: Bool -> Char -> (Char -> Bool)
classEsc _ 'd' = isDigit
classEsc _ 'D' = not . isDigit
classEsc _ 'w' = isWordCh
classEsc _ 'W' = not . isWordCh
classEsc _ 's' = isSpace
classEsc _ 'S' = not . isSpace
classEsc ci c  = chEq ci (unesc c)

unesc :: Char -> Char
unesc 'n' = '\n'; unesc 't' = '\t'; unesc 'r' = '\r'; unesc c = c

isWordCh :: Char -> Bool
isWordCh c = isAlphaNum c || c == '_'

chEq :: Bool -> Char -> (Char -> Bool)
chEq ci c = if ci then \x -> toLower x == toLower c else (== c)

readIntDef :: Int -> String -> Int
readIntDef d s = case reads s of ((n, _) : _) -> n; _ -> d

------------------------------------------------------------------------------
-- NFA compilation

-- The Pike VM's instruction set. 'ISplit' tries its first target before its
-- second (thread priority encodes greedy vs lazy); 'ISave' records the current
-- position in a capture slot (2g = group g start, 2g+1 = its end; slots 0/1
-- are the whole match).
data Inst
  = IChar !(Char -> Bool)
  | ISplit !Int !Int
  | IJmp !Int
  | ISave !Int
  | IAssert !Assertion
  | IMatch

data Assertion = ABol | AEol | AWordB !Bool

-- Bounded repetitions expand into copies of their body; refuse patterns whose
-- program would explode (e.g. @(a{9999}){9999}@).
maxInsts :: Int
maxInsts = 20000

-- Program size of the expansion, computed arithmetically (no expansion) and
-- capped at each level so nested huge bounds can't overflow.
progSize :: Re -> Int
progSize re = cap $ case re of
  REmpty        -> 0
  RPred _       -> 1
  RSeq xs       -> sum (map progSize xs)
  RAlt []       -> 0
  RAlt [x]      -> progSize x
  RAlt (x : xs) -> 2 + progSize x + progSize (RAlt xs)
  RGroup _ x    -> 2 + progSize x
  RStartAnchor  -> 1
  REndAnchor    -> 1
  RWordB _      -> 1
  RRep _ lo hi b ->
    let sb = progSize b
    in case hi of
         Nothing -> cap (lo * sb) + sb + 2
         Just m  -> cap (lo * sb) + cap (max 0 (m - lo) * (sb + 1))
  where cap = min (maxInsts + 1)

-- The full program: an unanchored-search prefix (a lazy "consume anything",
-- so threads start the pattern at every position, earliest first), the match
-- start marker, the pattern, the match end marker.
assemble :: Re -> [Inst]
assemble re =
  let prefix = [ISplit 3 1, IChar (const True), IJmp 0, ISave 0]
      (body, _) = emit re 4
  in prefix ++ body ++ [ISave 1, IMatch]

-- Emit instructions for a node starting at address @a@; returns them plus the
-- next free address. Targets are computed from the recursive results, so no
-- backpatching pass is needed.
emit :: Re -> Int -> ([Inst], Int)
emit re a = case re of
  REmpty  -> ([], a)
  RPred p -> ([IChar p], a + 1)
  RSeq xs -> foldl (\(is, aa) x -> let (i2, a2) = emit x aa in (is ++ i2, a2)) ([], a) xs
  RAlt []       -> ([], a)
  RAlt [x]      -> emit x a
  RAlt (x : xs) ->
    let (bx, ax) = emit x (a + 1)
        (br, ar) = emit (RAlt xs) (ax + 1)
    in (ISplit (a + 1) (ax + 1) : bx ++ [IJmp ar] ++ br, ar)
  RGroup g x ->
    let (bx, ax) = emit x (a + 1)
    in (ISave (2 * g) : bx ++ [ISave (2 * g + 1)], ax + 1)
  RStartAnchor -> ([IAssert ABol], a + 1)
  REndAnchor   -> ([IAssert AEol], a + 1)
  RWordB w     -> ([IAssert (AWordB w)], a + 1)
  RRep greedy lo hi body ->
    let (mand, a1) = emitN lo body a
    in case hi of
         Nothing ->                      -- star: split(body|out) body jmp-back
           let (bb, ab) = emit body (a1 + 1)
               end = ab + 1
               s = if greedy then ISplit (a1 + 1) end else ISplit end (a1 + 1)
           in (mand ++ [s] ++ bb ++ [IJmp a1], end)
         Just m ->                       -- (m-lo) optional copies, each may skip to the end
           let k = max 0 (m - lo)
               sb = snd (emit body 0)
               end = a1 + k * (sb + 1)
               opts aa j
                 | j >= k = []
                 | otherwise =
                     let (bb, _) = emit body (aa + 1)
                         s = if greedy then ISplit (aa + 1) end else ISplit end (aa + 1)
                     in (s : bb) ++ opts (aa + sb + 1) (j + 1)
           in (mand ++ opts a1 0, end)

-- @k@ consecutive copies of a node.
emitN :: Int -> Re -> Int -> ([Inst], Int)
emitN 0 _ a = ([], a)
emitN k body a =
  let (b1, a1) = emit body a
      (bs, a2) = emitN (k - 1) body a1
  in (b1 ++ bs, a2)

------------------------------------------------------------------------------
-- Matching (Pike VM: lock-step NFA simulation, linear in the line length)

type Caps = IM.IntMap (Int, Int)

-- | All non-overlapping matches on a single line, as (startCol, length).
lineMatches :: Regex -> Text -> [(Int, Int)]
lineMatches re line
  | T.length line > maxRegexLine = []
  | otherwise = map (\(s, e, _) -> (s, e - s)) (allMatches re line)

-- Non-overlapping matches with their capture groups, left to right.
allMatches :: Regex -> Text -> [(Int, Int, Caps)]
allMatches (Regex prog) line = go 0
  where
    arr = mkArr line
    n   = T.length line
    go i
      | i > n = []
      | otherwise = case vmRun prog arr n i of
          Nothing -> []
          Just (s, e, caps) ->
            (s, e, caps) : go (if e > s then e else e + 1)   -- advance past empty matches

-- One VM run: the leftmost match at or after @s0@, with the greedy/lazy
-- choices a backtracker would make (encoded in thread priority). Every thread
-- list is deduplicated by program counter, so each position does O(program)
-- work regardless of the pattern.
vmRun :: Array Int Inst -> Array Int Char -> Int -> Int -> Maybe (Int, Int, Caps)
vmRun prog arr n s0 = loop s0 (startThreads s0) Nothing
  where
    startThreads pos = reverse (snd (addT pos (IS.empty, []) 0 IM.empty))

    -- Epsilon-closure in priority order; stops at IChar / IMatch.
    addT pos st@(seen, acc) pc slots
      | pc `IS.member` seen = st
      | otherwise =
          let st1 = (IS.insert pc seen, acc)
          in case prog ! pc of
               ISplit x y -> let st2 = addT pos st1 x slots in addT pos st2 y slots
               IJmp x     -> addT pos st1 x slots
               ISave k    -> addT pos st1 (pc + 1) (IM.insert k pos slots)
               IAssert w  -> if assertOk w pos then addT pos st1 (pc + 1) slots else st1
               _          -> (fst st1, (pc, slots) : snd st1)   -- IChar / IMatch

    assertOk ABol pos       = pos == 0
    assertOk AEol pos       = pos == n
    assertOk (AWordB w) pos = wordBoundary arr n pos == w

    -- Process one position's threads in priority order. A thread reaching
    -- IMatch records its match and cuts every lower-priority thread (they
    -- could only produce a later-starting or less-preferred match).
    step pos ts0 = walk ts0 (IS.empty, []) Nothing
      where
        walk [] (_, acc) m = (reverse acc, m)
        walk ((pc, slots) : ts) st m = case prog ! pc of
          IChar p
            | pos < n && p (arr ! pos) -> walk ts (addT (pos + 1) st (pc + 1) slots) m
            | otherwise                -> walk ts st m
          IMatch -> (reverse (snd st), Just slots)
          _      -> walk ts st m
    loop pos threads best
      | null threads = fromSlots <$> best
      | otherwise =
          let (nt, m) = step pos threads
              best' = maybe best Just m   -- a later match is a higher-priority (longer) one
          in if pos >= n then fromSlots <$> best' else loop (pos + 1) nt best'

    fromSlots slots =
      let s = IM.findWithDefault 0 0 slots
          e = IM.findWithDefault s 1 slots
          groups = IM.fromList
            [ (g, (gs, ge))
            | g <- [1 .. 9]
            , Just gs <- [IM.lookup (2 * g) slots]
            , Just ge <- [IM.lookup (2 * g + 1) slots] ]
      in (s, e, groups)

mkArr :: Text -> Array Int Char
mkArr t = listArray (0, max 0 (T.length t - 1)) (T.unpack t ++ [' '])

wordBoundary :: Array Int Char -> Int -> Int -> Bool
wordBoundary arr n pos =
  let before = pos > 0 && isWordCh (arr ! (pos - 1))
      after  = pos < n && isWordCh (arr ! pos)
  in before /= after

------------------------------------------------------------------------------
-- Replacement

-- | Replace every non-overlapping match on a line, expanding @$0@..@$9@ (and
-- @$&@) / @\\1@..@\\9@ in the template to the captured groups. Returns the number
-- of substitutions and the rewritten line.
replaceLine :: Regex -> Text -> Text -> (Int, Text)
replaceLine re tmpl line
  | T.length line > maxRegexLine = (0, line)
  | otherwise =
      let ms = allMatches re line
          go _ [] = ([], 0)
          go prev ((s, e, caps) : rest) =
            let pre  = T.take (s - prev) (T.drop prev line)
                repl = expand tmpl caps line (s, e)
                (more, cnt) = go e rest
            in (pre : repl : more, cnt + 1)
          (pieces, count) = go 0 ms
          tailTxt = T.drop (lastEnd ms) line
      in (count, T.concat pieces <> tailTxt)
  where
    lastEnd [] = 0
    lastEnd xs = let (_, e, _) = last xs in e

-- Expand a replacement template using the captured groups.
expand :: Text -> Caps -> Text -> (Int, Int) -> Text
expand tmpl caps line whole = T.pack (go (T.unpack tmpl))
  where
    grp 0 = slice whole
    grp k = maybe "" slice (IM.lookup k caps)
    slice (a, b) = T.unpack (T.take (b - a) (T.drop a line))
    go [] = []
    go ('$' : '&' : r) = slice whole ++ go r
    go ('$' : d : r) | isDigit d = grp (fromEnum d - fromEnum '0') ++ go r
    go ('$' : '$' : r) = '$' : go r
    go ('\\' : d : r) | isDigit d = grp (fromEnum d - fromEnum '0') ++ go r
    go ('\\' : 'n' : r) = '\n' : go r
    go ('\\' : 't' : r) = '\t' : go r
    go ('\\' : '\\' : r) = '\\' : go r
    go (c : r) = c : go r
