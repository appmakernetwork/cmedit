-- | Find & replace: the in-file search engine and dialogs, live
-- match feedback, input history, and the workspace-wide
-- search/replace model.
module Cmedit.EditorFind where


import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)
import Data.Foldable (toList)
import Data.List (findIndex, intercalate, isPrefixOf, isSuffixOf, sortBy)
import Data.Ord (comparing)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeDirectory, takeExtension, takeFileName)
import System.Posix.Types (EpochTime)

import Data.Array (Array)

import Cmedit.Types
import Cmedit.TextBuffer
import Cmedit.Width (colToDisplay, displayToCol, wrapLine)
import Cmedit.ConfigFile
  ( Config(..), ThemeName(..), defaultConfig, RecentEntry(..)
  , maxRecentEntries, maxHistoryEntries )
import Cmedit.Menu
import Cmedit.Dialog
import Cmedit.Browser (Browser(..), FileNode(..), Entry)
import qualified Cmedit.Browser as Br
import Cmedit.Search
  ( SearchState(..), SField(..), SearchField(..), SearchReq(..)
  , FileResult(..), Match(..), SRow(..), HLine(..), FocusItem(..) )
import qualified Cmedit.Search as S
import Cmedit.Definition (DefReq(..), DefItem(..), DefPick(..))
import qualified Cmedit.Definition as D
import Cmedit.QuickOpen (QuickOpen(..))
import qualified Cmedit.QuickOpen as Q
import qualified Cmedit.Regex as Rx
import Cmedit.Csv (CsvView(..))
import qualified Cmedit.Csv as Csv
import Cmedit.About (aboutCanvasH, aboutCanvasMinW, aboutTotalFrames)
import Cmedit.Clipboard (CopyOutcome(..))
import Cmedit.Image (Image(..), ImgMode(..), renderImage, viewFit)
import Cmedit.Syntax (HlCache, CommentSyntax(..), langComment, langForPath)

import Cmedit.EditorState
import Cmedit.EditorEdit
import Cmedit.EditorDoc


-- | The driver calls this with the clipboard contents for a paste request.
applyPaste :: Text -> Editor -> Editor
applyPaste txt ed0
  | FDialog <- edFocus ed0, Just d0 <- edDialog ed0 =  -- paste into the focused dialog field (newlines kept)
      let d = if dlgPristine d0 && focusedField d0 == Just 0
                then (setFieldText 0 "" d0) { dlgPristine = False }   -- paste replaces a seeded term too
                else d0
      in refreshFindCount ed0 { edDialog = Just (foldl (flip fieldInsert) d (T.unpack txt)) }
  | Just v <- edCsv ed0 =      -- table mode: paste into the current cell
      if edReadOnly ed0 then ed0 { edStatus = "File is read-only" }
      else let (v', msg) = Csv.pasteClip txt v in (csvMod v' ed0) { edStatus = msg }
  | edReadOnly ed0 = ed0 { edStatus = "File is read-only" }
  | T.null txt = ed0
  | otherwise =
      let ed1 = removeSelection (beginEdit EKOther ed0)
          ed2 = insertRaw txt ed1
      in setDesired (afterEdit ed2) { edStatus = "Pasted" }

openFind :: Editor -> Editor
openFind ed = refreshFindCount (openDialog (mkFind (findSeed ed) (edSearchCase ed) (edSearchWord ed)) ed)

openReplace :: Editor -> Editor
openReplace ed = refreshFindCount (openDialog (mkReplace (findSeed ed) (edReplaceTerm ed) (edSearchCase ed)) ed)

-- The Find field is seeded from the text selection when there is one (so
-- "select a word, hit Replace" fills it in); otherwise the last search term.
-- Multi-line selections are fine — the search engine matches across lines — but
-- in CSV mode edBuffer/edSelAnchor are stale, so fall back to the last term.
findSeed :: Editor -> Text
findSeed ed
  | Nothing <- edCsv ed
  , Just (a, b) <- getSelection ed = textInRange a b (edBuffer ed)
  | otherwise = edSearchTerm ed

-- Match start indices in a line (overlapping, for find-next), honouring case-
-- and whole-word options. The shared 'S.scanMatches' walk keeps whole-word
-- boundary checks O(1) per candidate — an indexed check is quadratic on huge
-- single-line files.
matchIndices :: Bool -> Bool -> Text -> Text -> [Int]
matchIndices cs ww term line =
  S.scanMatches True ww (normCase cs term) (normCase cs line)

-- Non-overlapping match indices (for replace-all); linear for the same reason.
matchIndicesNO :: Bool -> Bool -> Text -> Text -> [Int]
matchIndicesNO cs ww term line =
  S.scanMatches False ww (normCase cs term) (normCase cs line)

-- Find the first match at or after a position, wrapping once.
searchFrom :: Text -> Bool -> Bool -> Pos -> Buffer -> Maybe (Pos, Pos)
searchFrom term cs ww start buf
  | T.null term = Nothing
  | hasNewline term = searchFromML term cs ww start buf
  | otherwise = go tries
  where
    n = lineCount buf
    inLine li fromCol =
      case filter (>= fromCol) (matchIndices cs ww term (getLine' li buf)) of
        (i : _) -> Just i
        []      -> Nothing
    tries = (posLine start, posCol start)
            : [ (li, 0) | li <- [posLine start + 1 .. n - 1] ]
            ++ [ (li, 0) | li <- [0 .. posLine start] ]
    go [] = Nothing
    go ((li, fc) : rest) = case inLine li fc of
      Just i  -> Just (Pos li i, Pos li (i + T.length term))
      Nothing -> go rest

-- Find the last match strictly before a position, wrapping once.
searchBack :: Text -> Bool -> Bool -> Pos -> Buffer -> Maybe (Pos, Pos)
searchBack term cs ww start buf
  | T.null term = Nothing
  | hasNewline term = searchBackML term cs ww start buf
  | otherwise = go tries
  where
    n = lineCount buf
    before li bc = case reverse (filter (< bc) (matchIndices cs ww term (getLine' li buf))) of
                     (i : _) -> Just i
                     []      -> Nothing
    anyIn li = case reverse (matchIndices cs ww term (getLine' li buf)) of
                 (i : _) -> Just i
                 []      -> Nothing
    tries = Left (posLine start, posCol start)
            : [ Right li | li <- [posLine start - 1, posLine start - 2 .. 0] ]
            ++ [ Right li | li <- [n - 1, n - 2 .. posLine start] ]
    go [] = Nothing
    go (t : rest) = case t of
      Left (li, bc) -> maybe (go rest) (mk li) (before li bc)
      Right li      -> maybe (go rest) (mk li) (anyIn li)
    mk li i = Just (Pos li i, Pos li (i + T.length term))

-- A search term that contains newlines can only match where the term's internal
-- line breaks coincide with real ones in the buffer: its first segment must be
-- the tail of some line, the middle segments must be whole lines, and the last
-- segment a prefix of a line. So a match starting on line @li@ has a *unique*
-- start column, which 'mlMatchAt' computes (ignoring any column bounds).
mlMatchAt :: [Text] -> Bool -> Bool -> Buffer -> Int -> Maybe (Pos, Pos)
mlMatchAt segs cs ww buf li
  | li + k >= n                                 = Nothing
  | col0 < 0                                    = Nothing
  | not (eq s0 (T.drop col0 line0))             = Nothing
  | not middlesOK                               = Nothing
  | not (eq sk (T.take (T.length sk) lastLine)) = Nothing
  | ww && not boundary                          = Nothing
  | otherwise = Just (Pos li col0, Pos (li + k) (T.length sk))
  where
    n        = lineCount buf
    k        = length segs - 1          -- number of line breaks (>= 1)
    s0       = head segs
    sk       = last segs
    middles  = init (tail segs)         -- the whole-line segments s1 .. s(k-1)
    line0    = getLine' li buf
    col0     = T.length line0 - T.length s0
    lastLine = getLine' (li + k) buf
    eq a b   = normCase cs a == normCase cs b
    middlesOK = and [ eq m (getLine' (li + 1 + j) buf) | (j, m) <- zip [0 ..] middles ]
    boundary  =
      let before = col0 == 0 || not (isWordCh (T.index line0 (col0 - 1)))
          afterPos = T.length sk
          after = afterPos >= T.length lastLine || not (isWordCh (T.index lastLine afterPos))
      in before && after

-- Multi-line forward search: scan candidate start lines in wrap order, taking
-- the unique candidate on each (filtered by the start column on the first line).
searchFromML :: Text -> Bool -> Bool -> Pos -> Buffer -> Maybe (Pos, Pos)
searchFromML term cs ww start buf = go tries
  where
    segs = T.splitOn (T.pack "\n") term
    n    = lineCount buf
    tries = (posLine start, posCol start)
            : [ (li, 0) | li <- [posLine start + 1 .. n - 1] ]
            ++ [ (li, 0) | li <- [0 .. posLine start] ]
    go [] = Nothing
    go ((li, fc) : rest) = case mlMatchAt segs cs ww buf li of
      Just m@(a, _) | posCol a >= fc -> Just m
      _                              -> go rest

-- Multi-line backward search: mirror of 'searchBack' over unique candidates.
searchBackML :: Text -> Bool -> Bool -> Pos -> Buffer -> Maybe (Pos, Pos)
searchBackML term cs ww start buf = go tries
  where
    segs = T.splitOn (T.pack "\n") term
    n    = lineCount buf
    tries = Left (posLine start, posCol start)
            : [ Right li | li <- [posLine start - 1, posLine start - 2 .. 0] ]
            ++ [ Right li | li <- [n - 1, n - 2 .. posLine start] ]
    go [] = Nothing
    go (t : rest) = case t of
      Left (li, bc) -> case mlMatchAt segs cs ww buf li of
                         Just m@(a, _) | posCol a < bc -> Just m
                         _                             -> go rest
      Right li      -> case mlMatchAt segs cs ww buf li of
                         Just m  -> Just m
                         Nothing -> go rest

------------------------------------------------------------------------------
-- Live find feedback (match-all highlight + counters)

-- | Counting/highlight caps: past 'matchCountCap' matches the counter shows
-- "1000+", and buffers over 'liveCountMaxChars' skip live counting entirely so
-- typing in the Find dialog stays instant on huge files.
matchCountCap :: Int
matchCountCap = 1000

liveCountMaxChars :: Int
liveCountMaxChars = 4 * 1024 * 1024

-- | While the Find/Replace dialog is open with a single-line term: that term
-- and its options, live from the dialog field (so the highlight follows as you
-- type). Multi-line terms and the CSV table view opt out.
liveFindTerm :: Editor -> Maybe (Text, Bool, Bool)
liveFindTerm ed = case edDialog ed of
  Just d | dlgKind d `elem` [DKFind, DKReplace]
         , isNothing (edCsv ed)
         , let t = fieldValue 0 d
         , not (T.null t), not (hasNewline t)
         -> Just (t, optionValue 0 d, dlgKind d == DKFind && optionValue 1 d)
  _ -> Nothing

-- | Spans of the live Find term in one line of text (for the renderer's
-- highlight-every-match pass). Empty when no Find/Replace dialog is up.
liveMatchSpans :: Editor -> Text -> [(Int, Int)]
liveMatchSpans ed line = case liveFindTerm ed of
  Nothing -> []
  Just (t, cs, ww) -> [ (i, i + T.length t) | i <- matchIndicesNO cs ww t line ]

-- Match count over the whole buffer, stopping as soon as the cap is passed.
countMatchesCapped :: Int -> Bool -> Bool -> Text -> Buffer -> Int
countMatchesCapped cap cs ww t buf = go 0 0
  where
    n = lineCount buf
    go !acc li
      | acc > cap || li >= n = acc
      | otherwise = go (acc + length (matchIndicesNO cs ww t (getLine' li buf))) (li + 1)

-- | Keep the Find/Replace dialog's message line showing a live match count.
-- Applied after every dialog keystroke and when the dialog opens.
refreshFindCount :: Editor -> Editor
refreshFindCount ed = case edDialog ed of
  Just d | dlgKind d `elem` [DKFind, DKReplace] ->
    ed { edDialog = Just d { dlgMessage = findCountMsg ed d } }
  _ -> ed

findCountMsg :: Editor -> Dialog -> Text
findCountMsg ed d
  | isJust (edCsv ed) || T.null t || hasNewline t = ""
  | bufChars (edBuffer ed) > liveCountMaxChars = ""
  | n == 0 = "No matches"
  | n > matchCountCap = T.pack (show matchCountCap ++ "+ matches")
  | otherwise = T.pack (show n ++ " match" ++ (if n == 1 then "" else "es"))
  where
    t  = fieldValue 0 d
    cs = optionValue 0 d
    ww = dlgKind d == DKFind && optionValue 1 d
    n  = countMatchesCapped matchCountCap cs ww t (edBuffer ed)

-- | "Match 3 of 17" for the just-found match at @a@ (blank when the buffer or
-- the match count is too large to count cheaply, or the term spans lines).
matchOrdinalMsg :: Pos -> Editor -> Text
matchOrdinalMsg a ed
  | hasNewline t || bufChars buf > liveCountMaxChars = ""
  | length capped > matchCountCap = ""
  | k > total = ""
  | otherwise = T.pack ("Match " ++ show k ++ " of " ++ show total)
  where
    t = edSearchTerm ed
    buf = edBuffer ed
    starts = [ Pos li i
             | li <- [0 .. lineCount buf - 1]
             , i <- matchIndices (edSearchCase ed) (edSearchWord ed) t (getLine' li buf) ]
    capped = take (matchCountCap + 1) starts
    total = length capped
    k = length (takeWhile (/= a) capped) + 1

findAgain :: Bool -> Editor -> Editor
findAgain forward ed
  | T.null (edSearchTerm ed) = openFind ed
  | Just v <- edCsv ed = csvFindWith False forward ed v   -- search cells, advance
  | otherwise =
      let buf = edBuffer ed
          res = if forward
                  then searchFrom (edSearchTerm ed) (edSearchCase ed) (edSearchWord ed)
                         (moveRight (edCursor ed) buf) buf
                  else searchBack (edSearchTerm ed) (edSearchCase ed) (edSearchWord ed) (edCursor ed) buf
      in selectMatch res ed

-- Run a search and select the match (used when confirming the Find dialog).
doFind :: Editor -> Editor
doFind ed
  | Just v <- edCsv ed = csvFindWith True True ed v        -- search cells from current
  | otherwise = selectMatch
      (searchFrom (edSearchTerm ed) (edSearchCase ed) (edSearchWord ed) (edCursor ed) (edBuffer ed)) ed

-- Search the table cells (row-major) for the search term and move the
-- selection to the matching cell, which the renderer highlights.
csvFindWith :: Bool -> Bool -> Editor -> CsvView -> Editor
csvFindWith inclusive forward ed v =
  let matches (r, c) = not (null (matchIndices (edSearchCase ed) (edSearchWord ed)
                                    (edSearchTerm ed) (Csv.cellAt r c v)))
  in case filter matches (csvSearchOrder v inclusive forward) of
       ((r, c) : _) -> (csvPut (Csv.setCursor r c v) ed)
                         { edStatus = T.pack ("Found in " ++ Csv.colLabel c ++ show (r + 1)) }
       []           -> ed { edStatus = "Not found: " <> flat1 (edSearchTerm ed) }

-- The order in which to scan cells, starting at (or after) the current cell.
csvSearchOrder :: CsvView -> Bool -> Bool -> [(Int, Int)]
csvSearchOrder v inclusive forward =
  let ncol = Csv.nCols v; nrow = Csv.nRows v
      cells = [ (r, c) | r <- [0 .. nrow - 1], c <- [0 .. ncol - 1] ]
      n = max 1 (length cells)
      idx = csvCurRow v * ncol + csvCurCol v
      rot k = let k' = ((k `mod` n) + n) `mod` n in drop k' cells ++ take k' cells
  in if forward
       then rot (if inclusive then idx else idx + 1)
       else reverse (rot (if inclusive then idx + 1 else idx))

selectMatch :: Maybe (Pos, Pos) -> Editor -> Editor
selectMatch res ed0 = case res of
  Nothing -> ed0 { edStatus = "Not found: " <> flat1 (edSearchTerm ed0) }
  Just (a, b) ->
    let ed = pushNavIfFar (edPath ed0) a ed0 in
    ensureVisible ed
      { edCursor = b, edSelAnchor = Just a
      , edDesiredCol = colToDisplay (tabWidthOf ed) (posCol b) (getLine' (posLine b) (edBuffer ed))
      , edStatus = matchOrdinalMsg a ed }

replaceOne :: Editor -> Editor
replaceOne ed0
  | Just v <- edCsv ed0 = csvReplaceOne ed0 v
  | edReadOnly ed0 = ed0 { edStatus = "File is read-only" }
  | otherwise =
      -- If the current selection is exactly the search term, replace it; then find next.
      let sel = getSelection ed0
          isMatch = case sel of
            Just (a, b) -> normCase (edSearchCase ed0) (textInRange a b (edBuffer ed0))
                             == normCase (edSearchCase ed0) (edSearchTerm ed0)
            Nothing -> False
      in if isMatch
           then case sel of
             Just (a, b) ->
               let ed1 = beginEdit EKOther ed0
                   (buf1, _) = deleteRange a b (edBuffer ed1)
                   (buf2, cur2) = insertText a (edReplaceTerm ed1) buf1
                   ed2 = afterEdit ed1 { edBuffer = buf2, edCursor = cur2, edSelAnchor = Nothing }
               in doFind ed2
             Nothing -> doFind ed0
           else doFind ed0

replaceAll :: Editor -> Editor
replaceAll ed0
  | Just v <- edCsv ed0 = csvReplaceAll ed0 v
  | edReadOnly ed0 = ed0 { edStatus = "File is read-only" }
  | T.null (edSearchTerm ed0) = ed0
  | otherwise =
      let ed1 = beginEdit EKOther ed0
          cs = edSearchCase ed1; ww = edSearchWord ed1
          term = edSearchTerm ed1; repl = edReplaceTerm ed1
          -- Operate on the whole buffer as one Text: a multi-line term matches
          -- across line breaks, and the substitution stays linear-time. Re-split
          -- on '\n' so a replacement that itself contains newlines lands cleanly.
          big = bufferToText LF False (edBuffer ed1)
          (count, big', mEnd) = replaceAllText cs ww term repl big
          buf' = fromText big'
          -- Drop the cursor at the end of the last replacement (or leave it put
          -- when nothing matched), so the view jumps to the final change.
          cur' = case mEnd of
                   Just o  -> let (l, c) = Csv.cursorLineCol big' o in clampPos (Pos l c) buf'
                   Nothing -> clampPos (edCursor ed1) buf'
          edited = afterEdit ed1 { edBuffer = buf', edCursor = cur', edSelAnchor = Nothing }
      in edited { edStatus = replaceAllStatus count term }   -- afterEdit blanks edStatus, so set it last

-- | Replace every non-overlapping occurrence of @term@ with @repl@ in @hay@,
-- honouring case-insensitivity (@cs@ False = ignore case) and whole-word (@ww@).
-- Returns the match count and, when something matched, the character offset in
-- the result just past the last replacement (for moving the cursor there).
-- Speed matters: replace-all runs over the whole buffer as one Text and the old
-- per-position scan was O(n^2) — a 13k-line file took ~a minute. The plain case
-- (case-sensitive, not whole-word) uses Data.Text's linear split/intercalate
-- (the kind of fast scan sed does); the option-sensitive cases walk the text
-- once with 'T.breakOn'.
replaceAllText :: Bool -> Bool -> Text -> Text -> Text -> (Int, Text, Maybe Int)
replaceAllText cs ww term repl hay
  | T.null term  = (0, hay, Nothing)
  | cs && not ww =
      let pieces  = T.splitOn term hay              -- non-overlapping split points
          cnt     = length pieces - 1
          result  = T.intercalate repl pieces
          lastEnd = T.length result - T.length (last pieces)
      in (cnt, result, if cnt == 0 then Nothing else Just lastEnd)
  | otherwise =
      let (cnt, lastEnd, chunks) = walk [] 0 0 0 0 Nothing hay (normCase cs hay)
      in (cnt, T.concat (reverse chunks), if cnt == 0 then Nothing else Just lastEnd)
  where
    nterm = normCase cs term
    tlen  = T.length term
    rlen  = T.length repl
    total = T.length hay
    -- Walk the normalised haystack with breakOn, slicing the original in
    -- lockstep. @consumed@ = original chars before @origRem@; @outLen@ = chars
    -- emitted so far; @lastEnd@ = output offset just past the last replacement;
    -- @prev@ = the original char before @origRem@ (whole-word boundary check).
    walk acc !cnt !consumed !outLen !lastEnd prev origRem normRem =
      case T.breakOn nterm normRem of
        (npre, nrest)
          | T.null nrest -> (cnt, lastEnd, origRem : acc)
          | otherwise ->
              let plen = T.length npre
                  (origPre, atMatch) = T.splitAt plen origRem
                  absPos   = consumed + plen
                  beforeOK = absPos == 0 || not (isWordCh (befChar origPre prev))
                  afterOK  = absPos + tlen >= total
                             || not (isWordCh (T.index atMatch tlen))
              in if not ww || (beforeOK && afterOK)
                   then let outLen' = outLen + plen + rlen
                        in walk (repl : origPre : acc) (cnt + 1) (absPos + tlen)
                                outLen' outLen' (Just (T.index atMatch (tlen - 1)))
                                (T.drop tlen atMatch) (T.drop (plen + tlen) normRem)
                   else let ch = T.head atMatch     -- whole-word failed: keep 1 char, retry
                        in walk (T.singleton ch : origPre : acc) cnt (absPos + 1)
                                (outLen + plen + 1) lastEnd (Just ch)
                                (T.drop 1 atMatch) (T.drop (plen + 1) normRem)
    befChar origPre prev
      | not (T.null origPre) = T.last origPre
      | otherwise            = fromMaybe ' ' prev   -- unused when absPos == 0

-- A friendly, pluralised summary line for Replace All.
replaceAllStatus :: Int -> Text -> Text
replaceAllStatus n term
  | n <= 0    = "No matches for \x201C" <> flat1 term <> "\x201D"
  | n == 1    = "Replaced 1 match"
  | otherwise = "Replaced " <> T.pack (groupThousands n) <> " matches"

-- Replace every (non-overlapping) match in a single line.
replaceLine :: Bool -> Bool -> Text -> Text -> Text -> Text
replaceLine cs ww term repl line =
  let idxs = matchIndicesNO cs ww term line
      len = T.length term
  in foldl (\acc i -> T.take i acc <> repl <> T.drop (i + len) acc) line (reverse idxs)

-- Replace only the first match in a single line.
replaceFirst :: Bool -> Bool -> Text -> Text -> Text -> Text
replaceFirst cs ww term repl line =
  case matchIndicesNO cs ww term line of
    []      -> line
    (i : _) -> T.take i line <> repl <> T.drop (i + T.length term) line

-- Replace within CSV cells only: the table holds parsed cell values, so a
-- delimiter can never be matched or rewritten (replacing every "," strips commas
-- inside cells but leaves the column separators intact).
csvReplaceAll :: Editor -> CsvView -> Editor
csvReplaceAll ed v
  | T.null (edSearchTerm ed) = ed
  | total == 0 = ed { edStatus = replaceAllStatus 0 (edSearchTerm ed) }
  | otherwise  = (csvMod (Csv.mapCells (replaceLine cs ww term repl) v) ed)
                   { edStatus = replaceAllStatus total (edSearchTerm ed) }
  where
    cs = edSearchCase ed; ww = edSearchWord ed
    term = edSearchTerm ed; repl = edReplaceTerm ed
    total = sum [ length (matchIndicesNO cs ww term (Csv.cellAt r c v))
                | r <- [0 .. Csv.nRows v - 1], c <- [0 .. Csv.nCols v - 1] ]

-- Replace the first match in the next matching cell (from the current cell,
-- searching forward and wrapping), and move the selection there.
csvReplaceOne :: Editor -> CsvView -> Editor
csvReplaceOne ed v
  | T.null (edSearchTerm ed) = ed
  | otherwise = case filter cellMatches (csvSearchOrder v True True) of
      []          -> ed { edStatus = "Not found: " <> flat1 (edSearchTerm ed) }
      ((r, c) : _) ->
        let cell' = replaceFirst cs ww term repl (Csv.cellAt r c v)
            v'    = Csv.setCurrentCell cell' (Csv.setCursor r c v)
        in (csvMod v' ed) { edStatus = T.pack ("Replaced in " ++ Csv.colLabel c ++ show (r + 1)) }
  where
    cs = edSearchCase ed; ww = edSearchWord ed
    term = edSearchTerm ed; repl = edReplaceTerm ed
    cellMatches (r, c) = not (null (matchIndices cs ww term (Csv.cellAt r c v)))

------------------------------------------------------------------------------
-- Find / replace input history (Up/Down in the dialog fields)

pushFindHist :: Text -> Editor -> Editor
pushFindHist t ed
  | T.null t = ed
  | otherwise = ed { edFindHist = take maxHistoryEntries (t : filter (/= t) (edFindHist ed)) }

pushReplHist :: Text -> Editor -> Editor
pushReplHist t ed
  | T.null t = ed
  | otherwise = ed { edReplHist = take maxHistoryEntries (t : filter (/= t) (edReplHist ed)) }

-- Does the focused field of this dialog have a recall history?
histFieldOf :: Editor -> Dialog -> Maybe (Int, [Text])
histFieldOf ed d
  | dlgKind d `elem` [DKFind, DKReplace] = do
      i <- focusedField d
      let hist = if i == 0 then edFindHist ed else edReplHist ed
      if null hist then Nothing else Just (i, hist)
  | otherwise = Nothing

-- | Step through the focused field's history: @dir@ 1 = older, -1 = newer.
-- Stepping past the newest entry restores whatever was being typed.
histRecall :: Int -> Editor -> Editor
histRecall dir ed = case edDialog ed of
  Just d | Just (i, hist) <- histFieldOf ed d ->
    let cur   = fromMaybe (-1) (edHistPos ed)
        stash = if cur == -1 then fieldValue i d else edHistStash ed
        pos   = max (-1) (min (length hist - 1) (cur + dir))
        newT  = if pos == -1 then stash else hist !! pos
    in ed { edDialog = Just (setFieldText i newT d)
          , edHistPos = if pos == -1 then Nothing else Just pos
          , edHistStash = stash }
  _ -> ed

-- Rows available for the (scrolling) results list, below the fixed header.
searchResultsHeight :: SearchState -> Editor -> Int
searchResultsHeight ss ed =
  let (_, _, h, _) = searchRegion (computeLayout ed) in max 1 (h - S.headerHeight ss)

-- | Open (or re-focus) the search view. @showRepl@ shows the Replace row and
-- focuses it (F6); otherwise the Find row is focused (F4).
openSearchPanel :: Bool -> Editor -> (Editor, [Effect])
openSearchPanel showRepl ed =
  let root = guessRoot ed
      base = fromMaybe (S.newSearchState root) (edSearch ed)
      -- Seed the Find field from the document selection only when opening fresh
      -- from the editor — re-invoking inside the panel must keep the typed term.
      seedTerm
        | edFocus ed /= FSearch, Nothing <- edCsv ed, Just (a, b) <- getSelection ed
        , not (hasNewline (textInRange a b (edBuffer ed))) = textInRange a b (edBuffer ed)
        | not (T.null (sfText (ssFind base)))              = sfText (ssFind base)
        | otherwise                                        = edSearchTerm ed
      -- F4 opens a find-only view; F6 shows the replace row.
      -- Setting (not OR-ing) means going "back to find" reliably hides replace,
      -- so an accidental Replace All is harder to trigger.
      ss0 = base { ssRoot = root
                 , ssShowReplace = showRepl
                 , ssFind = S.mkField seedTerm
                 , ssReplace = if T.null (sfText (ssReplace base)) then S.mkField (edReplaceTerm ed) else ssReplace base
                 , ssCase = edSearchCase ed, ssWord = edSearchWord ed }
      ss1 = S.clampCursor (S.setCursorField (if showRepl then SFReplace else SFFind) ss0)
  in noEff ed { edSearch = Just ss1, edSearchMode = True, edFocus = FSearch, edMenu = closedMenu, edStatus = "" }

-- Hide the panel (keep its state) and return focus to the editor.
closeSearchView :: Editor -> Editor
closeSearchView ed = ed { edSearchMode = False, edFocus = FEdit, edStatus = "" }

buildSearchReq :: Int -> SearchState -> Editor -> SearchReq
buildSearchReq gen ss ed = SearchReq
  { sqGen = gen, sqRoot = ssRoot ss, sqTerm = sfText (ssFind ss)
  , sqCase = ssCase ss, sqWord = ssWord ss, sqRegex = ssRegex ss
  , sqInclude = S.parseGlobs (sfText (ssInclude ss))
  , sqExclude = S.parseGlobs (sfText (ssExclude ss))
  , sqSkip = openPathsList ed }

-- | Start a search from the current panel state (Enter, or an option toggle).
runSearch :: Editor -> (Editor, [Effect])
runSearch ed = case edSearch ed of
  Nothing -> noEff ed
  Just ss
    | T.null (sfText (ssFind ss)) ->
        noEff ed { edSearch = Just ss { ssResults = Seq.empty, ssTotal = 0, ssRunning = False
                                      , ssSearched = True, ssMessage = "Type a search term" } }
    -- Reject a bad regular expression up front (don't start the walk).
    | ssRegex ss, Left err <- Rx.compile (not (ssCase ss)) (sfText (ssFind ss)) ->
        noEff ed { edSearch = Just ss { ssResults = Seq.empty, ssTotal = 0, ssRunning = False
                                      , ssSearched = True, ssMessage = T.pack ("Invalid regex: " ++ err) } }
    | otherwise ->
        let gen = ssGen ss + 1
            ss1 = ss { ssGen = gen, ssRunning = True, ssResults = Seq.empty, ssTotal = 0, ssScanned = 0
                     , ssTruncated = False, ssMessage = "", ssSearched = True
                     , ssTop = 0, ssCursor = 0, ssSpin = 0, ssLeft = 0 }
            req = buildSearchReq gen ss1 ed
            ed1 = pushFindHist (sfText (ssFind ss1))   -- panel searches join the recall history
                    ed { edSearch = Just ss1
                       , edSearchTerm = sfText (ssFind ss1), edReplaceTerm = sfText (ssReplace ss1)
                       , edSearchCase = ssCase ss1, edSearchWord = ssWord ss1 }
        in (ed1, [EffStartSearch req])

-- | Search the open documents' in-memory buffers (so unsaved edits are matched),
-- for those under @root@ that pass the globs. Called by the driver after it has
-- canonicalised the root, to seed the results before the disk walk streams in.
searchOpenDocs :: FilePath -> SearchReq -> Editor -> [FileResult]
searchOpenDocs root req ed =
  case S.compileMatcher (sqCase req) (sqWord req) (sqRegex req) (sqTerm req) of
    Left _  -> []
    Right m ->
      [ FileResult p ms False trunc
      | d <- allOpenDocs (syncCsvToBuffer ed)
      , Just p <- [docPath d]
      , Just rel <- [relativeTo root p]
      , S.pathIncluded (sqInclude req) (sqExclude req) rel
      , let (ms, trunc, _) = S.fileMatchesM m (docText d)
      , not (null ms) ]

-- | Driver callback: install the open-document seed results for search @gen@.
searchSeed :: Int -> FilePath -> [FileResult] -> Editor -> Editor
searchSeed gen root results ed = case edSearch ed of
  Just ss | ssGen ss == gen ->
    ed { edSearch = Just (S.clampCursor ss { ssRoot = root
                                           , ssResults = Seq.fromList (sortBy (comparing frPath) results)
                                           , ssTotal = sum (map S.fileMatchCount results) }) }
  _ -> ed

-- | Driver callback: one file's matches arrived from the disk walk. Kept O(1)
-- per file (no full re-scan, no cursor re-clamp) so streaming thousands of hits
-- stays cheap; the running 'ssTotal' and the file count drive the caps. Appending
-- is fine because the walker yields unique paths in (roughly) sorted order.
searchFileFound :: Int -> FileResult -> Editor -> Editor
searchFileFound gen fr ed = case edSearch ed of
  Just ss | ssGen ss == gen ->
    if ssTotal ss >= S.maxTotalMatches || Seq.length (ssResults ss) >= S.maxResultFiles
      then ed { edSearch = Just ss { ssTruncated = True } }
      else ed { edSearch = Just ss { ssResults = ssResults ss |> fr
                                   , ssTotal = ssTotal ss + S.fileMatchCount fr } }
  _ -> ed

-- | Driver callback: progress update (files scanned so far).
searchProgress :: Int -> Int -> Editor -> Editor
searchProgress gen n ed = case edSearch ed of
  -- max: the walker's worker pool reports concurrently, so counts can arrive
  -- slightly out of order — the display should never step backwards.
  Just ss | ssGen ss == gen -> ed { edSearch = Just ss { ssScanned = max (ssScanned ss) n } }
  _ -> ed

-- | Driver callback: the search finished. Sets the summary message.
searchDone :: Int -> Bool -> Editor -> Editor
searchDone gen trunc ed = case edSearch ed of
  Just ss | ssGen ss == gen ->
    let nMatches = ssTotal ss
        nFiles = Seq.length (ssResults ss)
        truncd = ssTruncated ss || trunc
        msg | nMatches == 0 = "No results found"
            | otherwise = T.pack (groupThousands nMatches ++ " result" ++ plural nMatches
                                    ++ " in " ++ show nFiles ++ " file" ++ plural nFiles
                                    ++ (if truncd then " (truncated)" else ""))
    in ed { edSearch = Just ss { ssRunning = False, ssTruncated = truncd, ssMessage = msg } }
  _ -> ed

-- | Advance the search spinner one frame (driver tick).
searchTick :: Editor -> Editor
searchTick ed = case edSearch ed of
  Just ss | ssRunning ss -> ed { edSearch = Just ss { ssSpin = ssSpin ss + 1 } }
  _ -> ed

------------------------------------------------------------------------------
-- Replace across the workspace

-- | Replace All across every file with results: open documents are edited in
-- their buffers (so the change is undoable and the user chooses when to save);
-- closed files are rewritten on disk by the driver. Afterwards the search is
-- re-run to refresh the (now reduced) results.
-- | Files touched by a Replace All above which we ask for confirmation first
-- (a bulk workspace edit is easy to trigger and hard to fully undo).
replaceConfirmThreshold :: Int
replaceConfirmThreshold = 10

-- | Replace across every file with results (Alt+R / the Replace All button).
-- A large sweep (more than 'replaceConfirmThreshold' files) asks first.
runReplaceAll :: Editor -> (Editor, [Effect])
runReplaceAll ed = case edSearch ed of
  Nothing -> noEff ed
  Just ss
    | null (S.resultPaths ss) -> noEff ed { edSearch = Just ss { ssMessage = "Nothing to replace" } }
    | length (S.resultPaths ss) > replaceConfirmThreshold ->
        let n = length (S.resultPaths ss)
        in noEff (openDialog (mkConfirm DKConfirmReplaceAll "Replace in Files"
             (T.pack ("Replace every occurrence of \x201C" ++ T.unpack (flat1 (sfText (ssFind ss)))
                      ++ "\x201D across " ++ show n ++ " files?"))
             ["Replace All", "Cancel"]) ed)
    | otherwise -> doReplace (S.resultPaths ss) ed

-- | Replace only within the file of the currently-selected result row
-- (Ctrl/Shift+Enter on a result). Lets you apply changes file-by-file rather
-- than all at once — the terminal analogue of VS Code's per-file replace.
runReplaceFile :: Editor -> (Editor, [Effect])
runReplaceFile ed = case edSearch ed of
  Just ss -> case selectedResultFile ss of
    Just p  -> doReplace [p] ed
    Nothing -> noEff ed
  Nothing -> noEff ed

-- The file path of the file under (or containing) the selected result row.
selectedResultFile :: SearchState -> Maybe FilePath
selectedResultFile ss = case S.selectedRow ss of
  Just (SRFile fi)    -> frPath <$> Seq.lookup fi (ssResults ss)
  Just (SRMatch fi _) -> frPath <$> Seq.lookup fi (ssResults ss)
  Nothing             -> Nothing

-- | Above this many files a Replace All is written straight to disk (with a
-- re-run search to refresh) rather than opened as unsaved tabs — staging hundreds
-- of editors for review is neither useful nor cheap.
maxStageReplaceFiles :: Int
maxStageReplaceFiles = 50

doReplace :: [FilePath] -> Editor -> (Editor, [Effect])
doReplace paths ed = case edSearch ed of
  Nothing -> noEff ed
  Just ss
    | T.null (sfText (ssFind ss)) -> noEff ed
    | null paths -> noEff ed { edSearch = Just ss { ssMessage = "Nothing to replace" } }
    | length paths > maxStageReplaceFiles ->
        -- Too many to open for review: rewrite in place (open buffers + on disk).
        let (term, repl, cs, ww, rx) = replaceParams ss
            subst = replaceSubst cs ww rx term repl
            (ed1, openCount) = replaceInOpenDocs subst paths ed
            openSet = openPathsList ed
            closedPaths = [ p | p <- paths, p `notElem` openSet ]
            req = ReplaceReq (ssGen ss + 1) term repl cs ww rx closedPaths openCount
        in (ed1 { edSearch = Just ss { ssRunning = True, ssMessage = "Replacing\x2026" } }
           , [EffReplaceOnDisk req])
    | otherwise ->
        -- Stage: edit open buffers now, and hand the closed files to the driver
        -- to open (unsaved) so every change is reviewable before you save it.
        let (term, repl, cs, ww, rx) = replaceParams ss
            subst = replaceSubst cs ww rx term repl
            (ed1, openCount) = replaceInOpenDocs subst paths ed
            openSet = openPathsList ed
            closedPaths = [ p | p <- paths, p `notElem` openSet ]
            req = ReplaceReq (ssGen ss + 1) term repl cs ww rx closedPaths openCount
        in (ed1 { edSearch = Just ss { ssMessage = "Replacing\x2026" } }, [EffStageReplace req])

replaceParams :: SearchState -> (Text, Text, Bool, Bool, Bool)
replaceParams ss = (sfText (ssFind ss), sfText (ssReplace ss), ssCase ss, ssWord ss, ssRegex ss)

-- | Driver callback (staged path): the closed files have been opened as unsaved
-- documents and revealed in the explorer. Report the total and hand focus to the
-- explorer (if a folder is open) so the user can walk the changed files and save.
stageReplaceDone :: Int -> Editor -> Editor
stageReplaceDone total ed =
  let nDirty = length (modifiedDocPaths ed)
      note   = "Replaced " ++ groupThousands total ++ " occurrence" ++ plural total
                 ++ " in " ++ show nDirty ++ " file" ++ plural nDirty
                 ++ " \x2014 review, then Ctrl+S (File \x25b8 Save All to save all)"
  in (relayout ed) { edSearchMode = False
                   , edFocus = if isJust (edExplorer ed) then FExplorer else FEdit
                   , edStatus = T.pack note }

-- | Open a just-read closed file as an /unsaved/ document with the replacement
-- already applied (so it shows up dirty and can be reviewed/saved). Returns the
-- number of occurrences replaced (0, and no document added, if nothing matched).
addStagedDoc :: FilePath -> LoadResult -> (Text -> (Int, Text)) -> Editor -> (Editor, Int)
addStagedDoc path lr subst ed =
  let (cnt, txt') = subst (bufferToText LF False (lrBuffer lr))
  in if cnt == 0 then (ed, 0)
     else (ed { edAfter = edAfter ed ++ [stagedDoc path lr (fromText txt')] }, cnt)

-- A dirty 'Document' for a staged replace: the new buffer, its on-disk content as
-- the saved baseline (so it reads as modified and Save writes the change), and an
-- undo checkpoint back to the original.
stagedDoc :: FilePath -> LoadResult -> Buffer -> Document
stagedDoc path lr buf' = Document
  { docBuffer = buf', docSavedBuffer = lrBuffer lr, docCursor = origin, docSelAnchor = Nothing
  , docDesiredCol = 0, docTop = 0, docLeft = 0
  , docPath = Just path, docModified = True
  , docDiskMtime = lrMtime lr, docDiskChanged = False
  , docLineEnding = lrLineEnding lr, docSavedEol = lrLineEnding lr
  , docEncoding = lrEncoding lr, docSavedEnc = lrEncoding lr
  , docFinalNewline = lrFinalNewline lr, docReadOnly = lrReadOnly lr
  , docUndo = [UndoState (lrBuffer lr) origin Nothing], docRedo = [], docLastEdit = EKNone
  , docOverwrite = False, docDiscard = False
  , docCsv = Nothing, docCsvStash = Nothing, docImage = Nothing
  , docHlCache = Nothing
  }

-- | The substitution used by a workspace Replace All, shared by the open-buffer
-- edits (here) and the on-disk rewrites (the driver): a whole-text function
-- returning @(count, newText)@. Regex expands @$1@/@\\1@ group references; a bad
-- regex substitutes nothing.
replaceSubst :: Bool -> Bool -> Bool -> Text -> Text -> (Text -> (Int, Text))
replaceSubst cs ww regex term repl
  | regex = case Rx.compile (not cs) term of
              Right r -> S.regexReplaceText r repl
              Left _  -> \t -> (0, t)
  | otherwise = \t -> let (c, t', _) = replaceAllText cs ww term repl t in (c, t')

-- Apply the replacement to every open plain-text document whose path is in the
-- result set, returning the updated editor and the number of occurrences changed.
-- Each edited document gets an undo checkpoint so the whole workspace replace can
-- be undone file-by-file (the active one via 'beginEdit', the rest by pushing a
-- snapshot onto their own undo stacks).
replaceInOpenDocs :: (Text -> (Int, Text)) -> [FilePath] -> Editor -> (Editor, Int)
replaceInOpenDocs subst paths ed =
  let hit d = isPlainDoc d && maybe False (`elem` paths) (docPath d)
      doBuf buf = let (c, big') = subst (bufferToText LF False buf)
                  in (fromText big', c)
      activeHit = isNothing (edCsv ed) && isNothing (edImage ed)
                  && maybe False (`elem` paths) (edPath ed)
      (edA, activeCount)
        | activeHit = let ed1    = beginEdit EKOther ed
                          (b, c) = doBuf (edBuffer ed1)
                      in ( ed1 { edBuffer = b, edCursor = clampPos (edCursor ed1) b
                               , edSelAnchor = Nothing, edModified = True, edStatus = "" }, c)
        | otherwise = (ed, 0)
      onDoc d
        | hit d = let snap   = UndoState (docBuffer d) (docCursor d) (docSelAnchor d)
                      (b, c) = doBuf (docBuffer d)
                  in (d { docBuffer = b, docModified = True
                        , docUndo = take maxUndo (snap : docUndo d), docRedo = [] }, c)
        | otherwise = (d, 0)
      (before', bc) = unzipSum (map onDoc (edBefore edA))
      (after',  ac) = unzipSum (map onDoc (edAfter edA))
  in (edA { edBefore = before', edAfter = after' }, activeCount + bc + ac)

unzipSum :: [(a, Int)] -> ([a], Int)
unzipSum xs = (map fst xs, sum (map snd xs))

-- | Driver callback after closed files were rewritten: report the total count
-- and re-run the search to refresh the results.
replaceDone :: Int -> Editor -> (Editor, [Effect])
replaceDone total ed =
  let ed1 = ed { edStatus = T.pack ("Replaced " ++ groupThousands total ++ " occurrence" ++ plural total) }
  in runSearch ed1

------------------------------------------------------------------------------
-- Search-view key handling

-- Controls drawn on the Find/Replace header lines (also used for mouse hits).
data SearchCtl = CtlCase | CtlWord | CtlRegex | CtlReplToggle | CtlReplaceAll
  deriving (Eq, Show)

-- | The clickable controls on the Find line, right-aligned within the region
-- @[x0, x0+w)@: match-case, whole-word, regex, and the replace-row toggle.
-- Returns @(startCol, text, ctl)@; the renderer and the mouse handler share this.
findLineCtls :: Int -> Int -> [(Int, String, SearchCtl)]
findLineCtls x0 w =
  let items = [("Aa", CtlCase), ("W", CtlWord), (".*", CtlRegex), ("\x21c5R", CtlReplToggle)]
      labels = map (\(s, c) -> ('[' : s ++ "]", c)) items
      total = sum (map (\(s, _) -> length s + 1) labels)
      start = x0 + w - total
      go _ [] = []
      go c ((s, ctl) : rest) = (c, s, ctl) : go (c + length s + 1) rest
  in go (max x0 start) labels

-- The "[Replace All]" button, right-aligned on the Replace line.
replaceAllLabel :: String
replaceAllLabel = "[Replace All]"

-- Column (relative to the region left) where a header field's value box starts.
-- Must match the label width used by the renderer.
searchFieldLabelW :: Int
searchFieldLabelW = 9    -- e.g. " Replace " padded

searchFieldValueCol :: Int
searchFieldValueCol = searchFieldLabelW
