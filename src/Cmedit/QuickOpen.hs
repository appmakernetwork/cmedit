-- | The Ctrl+P "Go to File" quick-open picker: a query field, the streamed
-- list of workspace files, and a from-scratch fuzzy matcher that ranks them.
--
-- Like "Cmedit.Definition" this is Editor-independent data + pure helpers:
-- "Cmedit.Editor" interprets what picking a file means, and the IO tree walk
-- lives in "Cmedit.App" (streamed in over the same channel as search results,
-- with its own supersede generation).
--
-- Cost model: a full re-rank of every known file happens only when the QUERY
-- changes (one linear scoring pass per keystroke). Streamed batches of newly
-- discovered files are scored alone and merged into the ranked list, so a
-- 50k-file walk arriving in hundreds of batches never re-scores the world.
module Cmedit.QuickOpen
  ( QuickOpen(..)
  , newQuickOpen
  , qoQuery
  , qoAddFiles
  , qoEditField
  , qoRescore
  , qoMoveSel
  , qoSelTo
  , maxQuickFiles
  , maxQuickResults
    -- * Command-palette mode (a leading '>')
  , qoCommandMode
  , qoPickedCommand
    -- * Fuzzy matching (exposed for tests)
  , fuzzyMatch
  ) where

import Data.Char (isUpper)
import Data.List (foldl', sortBy)
import Data.Ord (comparing, Down(..))
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import Cmedit.Menu (MenuAction)
import Cmedit.Search (SField(..), mkField)

-- | Hard cap on files collected by the walk (a runaway tree stays cheap).
maxQuickFiles :: Int
maxQuickFiles = 50000

-- | How many ranked results are kept for display/scrolling.
maxQuickResults :: Int
maxQuickResults = 256

-- A ranked entry: score, path, matched char positions (for highlighting).
type Ranked = (Int, Text, [Int])

data QuickOpen = QuickOpen
  { qoGen     :: !Int              -- ^ Supersede id for the background walk.
  , qoRoot    :: !FilePath         -- ^ Workspace root the paths are relative to.
  , qoField   :: !SField           -- ^ The query input.
  , qoFiles   :: !(Seq Text)       -- ^ Discovered files (workspace-relative), walk order.
  , qoRecent  :: ![Text]           -- ^ Recently-used files (relative), best-first — lead when the query is empty.
  , qoCommands :: ![(Text, MenuAction)] -- ^ Every menu command (label, action) — searched when the query starts with @>@.
  , qoTrunc   :: !Bool             -- ^ The walk hit 'maxQuickFiles'.
  , qoRunning :: !Bool             -- ^ The walk is still streaming.
  , qoMatches :: ![Ranked]         -- ^ Ranked results, best first (display order).
  , qoTotal   :: !Int              -- ^ Matches before the display cap.
  , qoSel     :: !Int
  , qoTop     :: !Int
  } deriving (Eq, Show)

newQuickOpen :: Int -> FilePath -> [Text] -> [(Text, MenuAction)] -> QuickOpen
newQuickOpen gen root recent commands = qoRescore QuickOpen
  { qoGen = gen, qoRoot = root, qoField = mkField ""
  , qoFiles = Seq.empty, qoRecent = recent, qoCommands = commands
  , qoTrunc = False, qoRunning = True
  , qoMatches = [], qoTotal = 0, qoSel = 0, qoTop = 0 }

-- | Is the picker in command-palette mode (query starts with @>@)?
qoCommandMode :: QuickOpen -> Bool
qoCommandMode = T.isPrefixOf ">" . qoQuery

-- | The action of the selected row, when in command-palette mode.
qoPickedCommand :: QuickOpen -> Maybe MenuAction
qoPickedCommand qo
  | not (qoCommandMode qo) = Nothing
  | otherwise = case drop (qoSel qo) (qoMatches qo) of
      ((_, lbl, _) : _) -> lookup lbl (qoCommands qo)
      []                -> Nothing

qoQuery :: QuickOpen -> Text
qoQuery = sfText . qoField

-- Ranking order: best score first, then shortest path, then name.
rankOrd :: Ranked -> (Down Int, Int, Text)
rankOrd (s, p, _) = (Down s, T.length p, p)

-- Recents outrank everything when the query is empty, best-first.
recentRanked :: [Text] -> [Ranked]
recentRanked rs = [ (recentScoreBase - i, p, []) | (i, p) <- zip [0 ..] rs ]

recentScoreBase :: Int
recentScoreBase = 1000000

-- | Fold a streamed batch of discovered files in and merge them into the
-- ranked list — scoring only the batch, not the whole set.
qoAddFiles :: [Text] -> QuickOpen -> QuickOpen
qoAddFiles new qo
  -- In command mode streamed files only accumulate; the visible list is
  -- commands and must not be disturbed.
  | qoCommandMode qo =
      let room = maxQuickFiles - Seq.length (qoFiles qo)
      in qo { qoFiles = foldl' (|>) (qoFiles qo) (take room new)
            , qoTrunc = qoTrunc qo || length new > room }
qoAddFiles new qo =
  let room = maxQuickFiles - Seq.length (qoFiles qo)
      add  = take room new
      q = qoQuery qo
      recentSet = Set.fromList (qoRecent qo)
      scoredNew
        | T.null q  = [ (0, p, []) | p <- add, not (Set.member p recentSet) ]
        | otherwise = [ (s, p, ps) | p <- add, Just (s, ps) <- [fuzzyMatch q p] ]
      sortedNew = sortBy (comparing rankOrd) scoredNew
      merged = take maxQuickResults (mergeOn rankOrd (qoMatches qo) sortedNew)
  in clampSel qo { qoFiles = foldl' (|>) (qoFiles qo) add
                 , qoTrunc = qoTrunc qo || length new > room
                 , qoMatches = merged
                 , qoTotal = qoTotal qo + length scoredNew }

mergeOn :: Ord k => (a -> k) -> [a] -> [a] -> [a]
mergeOn key = go
  where
    go [] ys = ys
    go xs [] = xs
    go (x : xs) (y : ys)
      | key x <= key y = x : go xs (y : ys)
      | otherwise      = y : go (x : xs) ys

-- | Apply an edit to the query field, then re-rank from scratch (resetting
-- the selection to the best match).
qoEditField :: (SField -> SField) -> QuickOpen -> QuickOpen
qoEditField f qo = qoRescore qo { qoField = f (qoField qo), qoSel = 0, qoTop = 0 }

-- | Recompute the ranked matches for the current query over everything known.
-- Empty query: recents lead, then all files (shortest path first). Otherwise
-- every file is fuzzy-scored and the best 'maxQuickResults' kept.
qoRescore :: QuickOpen -> QuickOpen
qoRescore qo
  -- Command palette: fuzzy-rank the menu commands ('>' alone lists them all
  -- in menu order).
  | Just cq0 <- T.stripPrefix ">" q =
      let cq = T.strip cq0
          scored
            | T.null cq = [ (0, lbl, []) | (lbl, _) <- qoCommands qo ]
            | otherwise = [ (s, lbl, ps)
                          | (lbl, _) <- qoCommands qo
                          , Just (s, ps) <- [fuzzyMatch cq lbl] ]
          ranked | T.null cq = scored                       -- keep menu order
                 | otherwise = sortBy (comparing rankOrd) scored
      in clampSel qo { qoMatches = take maxQuickResults ranked
                     , qoTotal = length scored }
  | T.null q =
      let recentSet = Set.fromList (qoRecent qo)
          rest = [ (0, p, []) | p <- toList (qoFiles qo), not (Set.member p recentSet) ]
          ranked = recentRanked (qoRecent qo) ++ sortBy (comparing rankOrd) rest
      in clampSel qo { qoMatches = take maxQuickResults ranked
                     , qoTotal = length (qoRecent qo) + length rest }
  | otherwise =
      let scored = [ (s, p, ps)
                   | p <- toList (qoFiles qo)
                   , Just (s, ps) <- [fuzzyMatch q p] ]
      in clampSel qo { qoMatches = take maxQuickResults (sortBy (comparing rankOrd) scored)
                     , qoTotal = length scored }
  where q = qoQuery qo

clampSel :: QuickOpen -> QuickOpen
clampSel qo =
  let n = length (qoMatches qo)
  in qo { qoSel = max 0 (min (max 0 (n - 1)) (qoSel qo)) }

-- | Move the selection by @d@ rows, keeping it inside a @vh@-row window.
qoMoveSel :: Int -> Int -> QuickOpen -> QuickOpen
qoMoveSel d vh qo = qoSelTo (qoSel qo + d) vh qo

qoSelTo :: Int -> Int -> QuickOpen -> QuickOpen
qoSelTo i vh qo =
  let n = length (qoMatches qo)
      sel = max 0 (min (max 0 (n - 1)) i)
      top | sel < qoTop qo       = sel
          | sel >= qoTop qo + vh = sel - vh + 1
          | otherwise            = qoTop qo
      maxTop = max 0 (n - vh)
  in qo { qoSel = sel, qoTop = max 0 (min maxTop top) }

------------------------------------------------------------------------------
-- Fuzzy matching

-- | Score @query@ against @path@ (case-insensitive subsequence). Returns the
-- score and the matched character positions for highlighting; 'Nothing' when
-- the query is not a subsequence. Two alignments are tried — from the path
-- start and anchored at the basename — and the better one wins, so @edi@
-- prefers @src\/editor.hs@'s basename over a scattered directory match.
fuzzyMatch :: Text -> Text -> Maybe (Int, [Int])
fuzzyMatch q0 path
  | T.null q0 = Just (0, [])
  | otherwise = case candidates of
      [] -> Nothing
      xs -> Just (foldr1 (\a b -> if fst a >= fst b then a else b) xs)
  where
    q = T.toLower q0
    lpath = T.toLower path
    n = T.length path
    baseStart = case T.findIndex (== '/') (T.reverse path) of
      Just k  -> n - k
      Nothing -> 0
    candidates =
      [ (scorePositions ps, ps) | Just ps <- [greedyFrom 0] ] ++
      [ (scorePositions ps, ps) | baseStart > 0, Just ps <- [greedyFrom baseStart] ]
    -- Leftmost greedy subsequence match, scanning from @start@.
    greedyFrom start = go start (T.unpack (T.drop start lpath)) (T.unpack q) []
      where
        go _ _ [] acc = Just (reverse acc)
        go _ [] _ _ = Nothing
        go i (c : cs) qq@(x : xs) acc
          | c == x    = go (i + 1) cs xs (i : acc)
          | otherwise = go (i + 1) cs qq acc
    -- Boundary = path start, after a separator, or a camelCase hump.
    boundary i
      | i <= 0 = True
      | otherwise =
          let prev = T.index path (i - 1)
              cur  = T.index path i
          in prev `elem` ("/_-. " :: String) || (isUpper cur && not (isUpper prev))
    scorePositions ps =
      let consec = length [ () | (a, b) <- zip ps (drop 1 ps), b == a + 1 ]
          bounds = length (filter boundary ps)
          inBase = if not (null ps) && all (>= baseStart) ps then 24 else 0
          spread = case ps of
            (p1 : _) -> last ps - p1 - (length ps - 1)
            []       -> 0
          startBonus = case ps of
            (p1 : _) | p1 == baseStart -> 8   -- query starts where the filename starts
            _ -> 0
      in 10 * length ps + 8 * consec + 12 * bounds + inBase + startBonus - min 40 spread
