-- | The CSV "table" model: a spreadsheet-style view of a CSV/TSV file. This
-- module is pure — it parses CSV text into a grid, lets you navigate and edit
-- cells, insert/delete rows and columns, and serialises back to CSV text. The
-- editor drives it; the renderer draws it.
module Cmedit.Csv
  ( CsvView(..)
  , mkCsvView
  , mkCsvGrid
  , mkCsvLines
  , csvToText
  , csvParse
  , csvParseLines
    -- * Dimensions / access
  , nRows
  , nCols
  , cellAt
  , colLabel
  , columnWidths
  , cellWidth
  , setColWidth
  , resetColWidth
  , maxCellLines
  , cellLineCount
  , rowLineCount
  , rowHeight
  , cursorLineCol
  , rowAtLineOffset
    -- * Navigation
  , moveCursor
  , moveToHomeRow
  , moveToEndRow
  , moveToTop
  , moveToBottom
  , pageMove
  , nextCellTab
  , setCursor
  , ensureVisible
  , hScrollTo
  , editLineUp
  , editLineDown
  , cellTextPos
  , textPosCell
    -- * Cell editing
  , isEditing
  , beginEdit
  , beginEditFresh
  , commitEdit
  , cancelEdit
  , editInsert
  , editBackspace
  , editDelete
  , editLeft
  , editRight
  , editHome
  , editEnd
  , clearCell
  , setCurrentCell
  , mapCells
  , currentCellText
    -- * Selection
  , selRect
  , hasSelection
  , clearSel
  , withSel
  , copyText
  , clearSelCells
  , fillSelCells
  , pasteClip
    -- * Structure
  , insertRowAbove
  , insertRowBelow
  , deleteRow
  , insertColLeft
  , insertColRight
  , deleteCol
  , sortByColumn
  , sortedAscBy
    -- * Undo
  , undo
  , redo
  , rebaseHistory
    -- * The modified flag
  , isModified
  , markSaved
  , markUnsaved
  , CsvDirty(..)
  , dirtyFrom
    -- * The embedded-newline map (0029)
  , computeNl
  , rowNl
  , linesBefore
  ) where

import Data.Char (chr, ord)
import Data.Foldable (foldl', toList)
import Data.List (elemIndex, nub, sortBy)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_, when)
import Data.Array.Unboxed (elems)
import Data.Array.ST (STUArray, newArray, readArray, writeArray, runSTUArray)

import Cmedit.Types (Dir(..), ptrEq)
import Cmedit.History (pushHist)
import Cmedit.Width (charWidth, isInvisibleFormat)

type Row  = Seq Text
type Grid = Seq Row

-- | The full table-view state for one CSV document.
data CsvView = CsvView
  { csvRows   :: !Grid
  , csvCurRow :: !Int
  , csvCurCol :: !Int
  , csvTop    :: !Int                 -- ^ First visible row (vertical scroll).
  , csvLeft   :: !Int                 -- ^ First visible column (horizontal scroll).
  , csvXOff   :: !Int                 -- ^ Extra display cells the column region is
                                      --   shifted left past the start of column 'csvLeft'
                                      --   (0 <= csvXOff <= effective width of that column).
                                      --   Only the scrollbar produces sub-column offsets;
                                      --   keyboard/cursor scrolling snaps it back to 0.
                                      --   Pure scroll state: never touches undo/widths/modified.
  , csvEdit   :: !(Maybe (Int, Text)) -- ^ (in-cell cursor, original value) while editing.
  , csvDelim  :: !Char
  , csvUndo   :: !(Seq Grid)
  , csvRedo   :: !(Seq Grid)
  , csvSaved  :: !Grid                -- ^ Grid as last saved/loaded (for the modified flag).
  , csvDirty  :: !CsvDirty            -- ^ How 'csvRows' differs from 'csvSaved', maintained
                                      --   incrementally by 'withCell'/'withRows'/undo/redo
                                      --   (comparing the two grids per keystroke cost 2.3 ms
                                      --   and 14 MB on a 223 000-row table). See 'CsvDirty'.
  , csvSelAnchor :: !(Maybe (Int, Int)) -- ^ Other corner of a rectangular cell selection.
  , csvWidths :: !(Seq Int)           -- ^ Clamped display width per column, kept in sync with
                                      --   'csvRows' by 'withRows'/undo/redo (scanning the whole
                                      --   grid per keystroke would freeze large tables).
  , csvUserW  :: !(Map Int Int)       -- ^ User width overrides (header-border drag), by column.
                                      --   Sparse; wins over the content-fitted 'csvWidths' entry.
  , csvNl     :: !(Map Int Int)       -- ^ Rows that carry embedded newlines, and how many each
                                      --   contributes to the serialised CSV. Sparse — absent
                                      --   means zero — and kept in sync with 'csvRows' by
                                      --   'withRows'\/'withCell'\/undo\/redo. This is what makes
                                      --   'cellTextPos' cheap; see 'linesBefore'.
  } deriving (Show)

-- | How far 'csvRows' has diverged from 'csvSaved' — the modified flag, kept as
-- state instead of recomputed.
--
-- The invariant is a biconditional, and both directions are load-bearing:
--
--  * @DirtyShape@ holds /exactly when/ the two grids have different shapes —
--    a different row count, or some row index at which the two rows have
--    different lengths. It is not an "I don't know" marker: 'withCell' relies
--    on the forward direction to keep it across a cell write without looking
--    at the grid, and every constructor of it relies on the reverse direction
--    to be allowed to say @DirtyCells@ at all.
--  * @DirtyCells n@ holds exactly when the shapes are equal and exactly @n@
--    cell positions hold different text.
--
-- So @isModified v@ is @csvRows v /= csvSaved v@, in O(1). Cells rather than
-- rows because the count is then maintained by a /delta/ — a cell write knows
-- the old text, the new text and the saved text, which is everything needed to
-- add or drop one from the count without consulting anything else. That is
-- what makes the common case (typing) O(log rows) rather than O(rows), and it
-- is why editing a cell back to its saved value correctly clears the flag.
data CsvDirty = DirtyShape | DirtyCells !Int
  deriving (Show, Eq)

maxUndo :: Int
maxUndo = 500

------------------------------------------------------------------------------
-- Construction / serialisation

-- | Build a table view from CSV text, using the given delimiter.
mkCsvView :: Char -> Text -> CsvView
mkCsvView delim t = mkCsvGrid delim (csvParse delim t)

-- | Build a table view from a grid somebody else produced.
--
-- The grid is the whole model, so a producer that is not CSV text — an XLSX
-- worksheet ("Cmedit.Xlsx") — reaches the table view through here and gets
-- navigation, selection, copy, column widths and both scroll bars for free.
-- What it does /not/ get is a way back: 'csvToText' would serialise a
-- workbook's sheet to CSV, which is not what its file is, so the views built
-- this way are marked read-only by the editor and never saved.
mkCsvGrid :: Char -> Grid -> CsvView
mkCsvGrid delim rows0 =
  let rows  = if Seq.null rows0 then Seq.singleton (Seq.singleton T.empty) else rows0
  in CsvView
       { csvRows = rows, csvCurRow = 0, csvCurCol = 0, csvTop = 0, csvLeft = 0
       , csvXOff = 0
       , csvEdit = Nothing, csvDelim = delim, csvUndo = Seq.empty, csvRedo = Seq.empty
       , csvSaved = rows, csvDirty = DirtyCells 0, csvSelAnchor = Nothing
       , csvWidths = computeWidths rows, csvUserW = Map.empty
       , csvNl = computeNl delim rows }

-- | Serialise the grid back to CSV text (records joined by @\\n@; the caller's
-- buffer applies the file's actual line ending).
csvToText :: CsvView -> Text
csvToText v =
  T.intercalate (T.pack "\n")
    [ T.intercalate (T.singleton (csvDelim v)) (map (quoteField (csvDelim v)) (toList row))
    | row <- toList (csvRows v) ]

quoteField :: Char -> Text -> Text
quoteField delim f
  | needs     = T.concat [T.pack "\"", T.replace (T.pack "\"") (T.pack "\"\"") f, T.pack "\""]
  | otherwise = f
  where
    needs = T.any (\c -> c == delim || c == '"' || c == '\n' || c == '\r') f

-- | Parse CSV text into a grid of cells (RFC 4180-ish: quoted fields, @\"\"@
-- escapes, embedded delimiters and newlines).
--
-- A thin wrapper over 'csvParseLines': the parser's input is a list of lines,
-- and a whole-file 'Text' is one @T.split@ away from being that. Splitting
-- first is not a reinterpretation — a @\\n@ is a record terminator in every
-- position the parser cares about — and it means there is exactly *one* CSV
-- parser rather than a text one and a line one that could drift apart.
-- ('pasteClip' and the tests are the only remaining callers; the editor's own
-- load path goes straight to 'csvParseLines'.)
--
-- Quirks of the original @String@ parser are preserved deliberately: a doubled
-- @\"\"@ inside a quoted field is an escaped quote, stray text after a closing
-- quote is appended to the field rather than rejected, and CR / CRLF / LF all
-- end a record.
csvParse :: Char -> Text -> Grid
csvParse delim t = parseFrom delim (T.split (== '\n') t)

-- | Parse a grid straight from a buffer's lines — the editor's own
-- representation of a loaded file.
--
-- This is the load path, and skipping the round trip through one whole-file
-- 'Text' is the point of it. @mkCsvView delim (bufferToText LF False buf)@
-- rebuilt the entire file as a second array (125 MB of allocation on a 32 MB
-- file) purely so the parser could take it apart again along exactly the
-- newlines the buffer had just been split on; worse, the cells then pointed
-- into *that* copy, so the document held two whole copies of the file for as
-- long as it stayed open. Parsing from the lines makes every unquoted cell a
-- slice of the same array the buffer's lines are slices of.
--
-- Equivalence with the text parser is by construction (they are the same
-- engine, and @T.split (== '\\n') . T.intercalate "\\n"@ is the identity on a
-- list of lines) and is pinned by a fuzz test in the suite.
csvParseLines :: Char -> Seq Text -> Grid
csvParseLines delim = parseFrom delim . toList

-- | The table view for a buffer's lines. 'mkCsvView' is this over 'csvParse'.
mkCsvLines :: Char -> Seq Text -> CsvView
mkCsvLines delim ls = mkCsvGrid delim (csvParseLines delim ls)

-- The parser proper. The input is a cursor into a list of lines: the
-- unconsumed remainder of the current line, and the lines after it. The
-- newline between one line and the next is implicit — never materialised, and
-- never scanned for — which is what makes this cheap enough to run on a file's
-- worth of lines.
--
-- The common case, a line with no quote and no stray CR, takes a fast path
-- ('fastRow') that is one 'T.break' loop over the delimiter and nothing else,
-- every cell a slice with no copying. Only lines that really contain a quote
-- pay for the character-level machinery, and only fields that contain a
-- doubled quote are rebuilt.
--
-- Cells are forced before they are stored, here as everywhere: a 'Seq' is
-- element-lazy, and a grid of two million thunks each holding a parser closure
-- is the shape of problem this codebase has had before.
data Cur = Cur !Text ![Text]

-- The outcome of one field: its text, the cursor after it, and what ended it.
-- The cursor is UNPACKed into it: a field's result is then one allocation
-- rather than two, which at a dozen fields a row over a million rows is worth
-- having.
data Fld = FDelim   !Text {-# UNPACK #-} !Cur
         | FNewline !Text {-# UNPACK #-} !Cur
         | FEof     !Text {-# UNPACK #-} !Cur

parseFrom :: Char -> [Text] -> Grid
parseFrom delim ls0 = Seq.fromList (rows (mkCur ls0))
  where
    mkCur []       = Cur T.empty []
    mkCur (l : ls) = Cur l ls

    -- "The remaining text is empty" — the text parser's termination test,
    -- expressed on the cursor.
    atEnd (Cur cur rest) = T.null cur && null rest

    -- 'rows' is entered at the start of a record, which is the start of a line
    -- except after a lone CR mid-line (which ends a record where it stands).
    -- The fast path is right either way: it says "this record is the rest of
    -- the cursor's line", and a record does end at the line end unless a quote
    -- or a CR intervenes — which is exactly what 'plain' rules out.
    rows c@(Cur cur rest)
      | atEnd c = []
      | plain cur =
          fastRow cur :
            (case rest of
               []       -> []
               (l : ls) -> let c' = Cur l ls in if atEnd c' then [] else rows c')
      | otherwise =
          let (row, c', more) = oneRow c
          in row : if more then rows c' else []

    -- No quoting and no embedded CR: the record is exactly this line's fields.
    plain = not . T.any (\ch -> ch == '"' || ch == '\r')

    -- @Seq.fromList . T.split (== delim)@, but strict: 'T.split' returns a
    -- lazy list whose tail thunks cost more than the cells they defer, and the
    -- bang is what keeps a parsed cell from being a selector thunk on the
    -- 'T.break' pair.
    fastRow t0 = mkRow (go t0 [])
      where go t acc = case T.break (== delim) t of
              (!f, r) | T.null r  -> f : acc
                      | otherwise -> go (T.tail r) (f : acc)

    oneRow c0 = collect c0 []
    collect c acc = case field c of
      FDelim   f c' -> collect c' (f : acc)
      FNewline f c' -> (mkRow (f : acc), c', not (atEnd c'))
      FEof     f c' -> (mkRow (f : acc), c', False)
    mkRow acc = Seq.fromList (reverse acc)

    field c@(Cur cur rest)
      | not (T.null cur), T.head cur == '"' = quoted (Cur (T.tail cur) rest) []
      | otherwise                           = unquoted c

    -- Unquoted: everything up to the next delimiter or record end. One scan,
    -- no copy (the value is a slice of the source).
    unquoted (Cur cur rest) =
      let (val, r) = T.break (\ch -> ch == delim || ch == '\r') cur
      in if T.null r
           then endOfLine val rest
           else if T.head r == delim
                  then FDelim val (Cur (T.tail r) rest)
                  else afterCR val (T.tail r) rest

    -- End of a line with nothing left to consume on it: the implicit newline
    -- ends the record, unless there is no next line, in which case it is EOF.
    endOfLine val []        = FEof val (Cur T.empty [])
    endOfLine val (l : ls)  = FNewline val (Cur l ls)

    -- A CR ends the record and swallows an LF after it — which, at the end of
    -- a line, is the implicit newline, so the next record starts on the line
    -- after (this is the CR half of CRLF as seen by 'csvParse', whose input
    -- has already been split on LF).
    afterCR val cs rest
      | T.null cs = endOfLine val rest
      | otherwise = FNewline val (Cur cs rest)

    -- Quoted: segments between quote characters, joined only when the field
    -- really spans lines or contains a doubled quote.
    quoted (Cur cur rest) acc =
      let (seg, r) = T.break (== '"') cur
      in if T.null r
           then case rest of                       -- the quote spans the line end
                  []       -> FEof (joinSegs (seg : acc)) (Cur T.empty [])
                  (l : ls) -> quoted (Cur l ls) (nlText : seg : acc)
           else
             let r2 = T.tail r
             in if not (T.null r2) && T.head r2 == '"'
                  then quoted (Cur (T.tail r2) rest) (quoteText : seg : acc)
                  else close (Cur r2 rest) (joinSegs (seg : acc))

    joinSegs [seg] = seg
    joinSegs segs  = T.concat (reverse segs)

    -- After a closing quote: a delimiter or record end, or (tolerated) stray
    -- text, which is appended to the field.
    close (Cur cur rest) val
      | T.null cur = endOfLine val rest
      | c == delim = FDelim val (Cur cs rest)
      | c == '\r'  = afterCR val cs rest
      | otherwise  = case unquoted (Cur cs rest) of
          FDelim   v2 c' -> FDelim   (val <> T.singleton c <> v2) c'
          FNewline v2 c' -> FNewline (val <> T.singleton c <> v2) c'
          FEof     v2 c' -> FEof     (val <> T.singleton c <> v2) c'
      where c  = T.head cur
            cs = T.tail cur

nlText, quoteText :: Text
nlText    = T.singleton '\n'
quoteText = T.singleton '"'

------------------------------------------------------------------------------
-- Dimensions / access

nRows :: CsvView -> Int
nRows = Seq.length . csvRows

-- | Number of columns: the widest row (at least 1). O(1) via the width cache
-- (whose length is exactly the column count).
nCols :: CsvView -> Int
nCols v = max 1 (Seq.length (csvWidths v))

rowAt :: Int -> CsvView -> Row
rowAt r v = case Seq.lookup r (csvRows v) of
  Just row -> row
  Nothing  -> Seq.empty

cellAt :: Int -> Int -> CsvView -> Text
cellAt r c v = case Seq.lookup c (rowAt r v) of
  Just t  -> t
  Nothing -> T.empty

currentCellText :: CsvView -> Text
currentCellText v = cellAt (csvCurRow v) (csvCurCol v) v

-- | Spreadsheet column label: 0 -> "A", 25 -> "Z", 26 -> "AA".
colLabel :: Int -> String
colLabel n0 = go (n0 + 1) ""
  where
    go 0 acc = acc
    go k acc = let (q, r) = (k - 1) `divMod` 26
               in go q (chr (ord 'A' + r) : acc)

-- | Display width of each column (clamped to a sensible range). Reads the
-- cache maintained alongside every grid change — O(columns), never O(cells) —
-- because this runs on every repaint and every cursor move. A user override
-- (from a header-border drag) replaces the content-fitted width outright.
columnWidths :: CsvView -> [Int]
columnWidths v = zipWith eff [0 ..] (toList (csvWidths v))
  where eff c w = Map.findWithDefault w c (csvUserW v)

-- | Set a user width override for a column (the header-border drag). Sticks
-- until 'resetColWidth', whatever the content does.
setColWidth :: Int -> Int -> CsvView -> CsvView
setColWidth c w v
  | c < 0 || c >= nCols v = v
  | otherwise = v { csvUserW = Map.insert c (clampUserW w) (csvUserW v) }

-- | Drop a column's width override, returning it to the content-fitted width
-- (double-click on the header border).
resetColWidth :: Int -> CsvView -> CsvView
resetColWidth c v = v { csvUserW = Map.delete c (csvUserW v) }

clampW :: Int -> Int
clampW w = max 3 (min 32 w)

-- A dragged width may be narrower or far wider than the automatic clamp;
-- horizontal scrolling already handles columns wider than the viewport.
clampUserW :: Int -> Int
clampUserW w = max 2 (min 200 w)

-- Full-grid width computation: one pass over every cell. Only used when a
-- grid appears wholesale (load) or changes shape (row/column insert/delete);
-- per-cell edits maintain the cache incrementally in 'syncWidths'.
-- | The clamped display width of every column, from scratch.
--
-- Accumulates into an unboxed array rather than a 'Seq': the previous version
-- did a @Seq.adjust'@ per cell, rebuilding O(log cols) spine nodes 3.6 million
-- times when a large table is opened. This runs on load and on any change of
-- shape, so it is worth the explicit loop.
-- Both loops walk the 'Seq's directly rather than through 'toList': a list cell
-- per grid cell is 2.7 million cons cells on a 32 MB table, for nothing.
computeWidths :: Grid -> Seq Int
computeWidths rows =
  let cols = max 1 (foldl' (\ !m row -> max m (Seq.length row)) 0 rows)
  in Seq.fromList (elems (runSTUArray (do
       a <- newArray (0, cols - 1) (clampW 1)
       forM_ rows $ \row ->
         let go !_ [] = pure ()
             go !c (cell : rest)
               | c >= cols = pure ()
               | otherwise = do
                   let w = clampW (cellWidth cell)
                   old <- readArray a c
                   when (w > old) (writeArray a c w)
                   go (c + 1) rest
         in go (0 :: Int) (toList row)
       pure a)))

-- The true width of one column (for when an edit shrinks a cell that may have
-- been the column's widest).
colWidth :: Grid -> Int -> Int
colWidth rows c =
  clampW (maximum (1 : [ cellWidth cell | row <- toList rows
                                        , Just cell <- [Seq.lookup c row] ]))

-- | Carry the column-width cache across a grid change. Pointer-equal rows and
-- cells are skipped (persistent 'Seq' updates share everything untouched), so
-- a cell edit costs O(rows) pointer hops plus the one changed cell — the same
-- trick the highlight cache uses. Any change of shape (row count or a row's
-- length) falls back to the full recomputation.
syncWidths :: Grid -> Seq Int -> Grid -> Seq Int
syncWidths old ws new
  | ptrEq old new = ws
  | Seq.length old /= Seq.length new = computeWidths new
  | otherwise =
      case rowsDiff (toList old) (toList new) [] of
        Nothing      -> computeWidths new
        Just changes -> applyChanges changes
  where
    -- (column, old cell, new cell) for every changed cell; Nothing when a
    -- row's length changed (a column appeared/disappeared).
    rowsDiff [] [] acc = Just acc
    rowsDiff (o : os) (n : ns) acc
      | ptrEq o n = rowsDiff os ns acc
      | Seq.length o /= Seq.length n = Nothing
      | otherwise = rowsDiff os ns (cellDiffs (toList o) (toList n) 0 acc)
    rowsDiff _ _ acc = Just acc
    cellDiffs [] [] _ acc = acc
    cellDiffs (a : as) (b : bs) !c acc
      | ptrEq a b || a == b = cellDiffs as bs (c + 1) acc
      | otherwise           = cellDiffs as bs (c + 1) ((c, a, b) : acc)
    cellDiffs _ _ _ acc = acc
    applyChanges changes =
      let upd (w, redo) (c, oldCell, newCell) =
            let cur  = maybe (clampW 1) id (Seq.lookup c w)
                newW = clampW (cellWidth newCell)
                oldW = clampW (cellWidth oldCell)
            in if newW >= cur then (Seq.update c newW w, redo)
               else if oldW == cur then (w, c : redo)   -- may have held the max
               else (w, redo)
          (w1, redoCols) = foldl upd (ws, []) changes
      in foldl (\w c -> Seq.update c (colWidth new c) w) w1 (nub redoCols)

------------------------------------------------------------------------------
-- Embedded newlines, as maintained state
--
-- Third cache on the same discipline as 'csvWidths' (0016) and 'csvDirty'
-- (0028), for the same reason: 'cellTextPos' — "which line of the serialised
-- file does this cell start on?" — is asked once per document event and used
-- to be answered by re-serialising every row above the cursor. On a
-- 223 209-row table at the last row that cost 383 ms and 1 651 MB (plan 0029).
--
-- What it stores is the /sparse/ map of rows that contain an embedded newline
-- at all, and how many each contributes. Sparse because that is the shape of
-- real data: the 32 MB benchmark corpus has 446 such rows out of 223 209, and
-- an ordinary table has none, in which case the map is empty and the prefix
-- sum is zero without looking at anything.
--
-- The invariant is an equality, and 'cellTextPos' depends on it exactly: for
-- every row index @i@,
--
--   @Map.findWithDefault 0 i (csvNl v)@  ==  the number of newlines in row
--   @i@'s serialised form — that is, in @csvToText@ of a one-row grid holding
--   it, which is what the test's oracle actually computes
--
-- which holds because serialisation cannot change a newline count:
-- 'quoteField' only wraps a field in quotes and doubles the quotes inside it,
-- and the joins between fields contribute a delimiter each (which is a
-- newline only in the degenerate case the second term below covers).

-- | Newlines row @row@ contributes to the serialised CSV.
rowNl :: Char -> Row -> Int
rowNl delim row =
  foldl' (\ !a c -> a + nlCount c) 0 row
    + (if delim == '\n' then max 0 (Seq.length row - 1) else 0)

-- | The whole map, from scratch. O(grid); runs on load and wherever a change
-- cannot be carried.
computeNl :: Char -> Grid -> Map Int Int
computeNl delim rows =
  Map.fromDistinctAscList (reverse (Seq.foldlWithIndex step [] rows))
  where
    -- Walks the 'Seq's directly: a 'toList' here is a cons cell and a tuple per
    -- row for nothing, the same reason 'computeWidths' avoids one.
    step acc i row = let k = rowNl delim row in if k > 0 then (i, k) : acc else acc

-- | Carry the map across a grid change — the twin of 'syncWidths' and
-- 'syncDirty', with the same cost model: pointer-equal rows are skipped, so a
-- change that touches one row costs O(rows) pointer hops and one row's cells.
-- A change of row /count/ shifts every later index, so it recomputes.
syncNl :: Char -> Grid -> Map Int Int -> Grid -> Map Int Int
syncNl delim old m new
  | ptrEq old new = m
  | Seq.length old /= Seq.length new = computeNl delim new
  | otherwise = go m 0 (toList old) (toList new)
  where
    go !acc !i (o : os) (w : ws)
      | ptrEq o w = go acc (i + 1) os ws
      | otherwise = go (setNl i (rowNl delim w) acc) (i + 1) os ws
    go !acc _ _ _ = acc

-- | Record a row's count, keeping the map sparse (zero means absent).
setNl :: Int -> Int -> Map Int Int -> Map Int Int
setNl i k m
  | k <= 0    = Map.delete i m
  | otherwise = Map.insert i k m

-- | Extra lines contributed by the rows /above/ row @r@ — the quantity
-- 'cellTextPos' needs. O(log rows + rows-above-that-are-multi-line), which is
-- O(log rows) for a table with no embedded newlines at all.
linesBefore :: CsvView -> Int -> Int
linesBefore v r
  | Map.null m || r <= 0 = 0
  | otherwise            = Map.foldl' (+) 0 (fst (Map.split r m))
  where m = csvNl v

------------------------------------------------------------------------------
-- The modified flag, as maintained state
--
-- Same discipline as the width cache above, and for the same reason: the
-- quantity is wanted on every keystroke, and computing it from the grid is
-- O(rows). The three functions below are the only producers of a 'CsvDirty',
-- and 'withCell' / 'withRows' / undo / redo / 'markSaved' / 'markUnsaved' /
-- 'mkCsvGrid' / 'rebaseHistory' are the only places a 'CsvView' may set the
-- field — exactly the set of places allowed to set 'csvWidths', plus the three
-- that move 'csvSaved'.

-- | The dirty state from scratch: one pointer-accelerated pass over the grid.
--
-- Run when a grid appears wholesale (a mode toggle rebasing onto a new parse)
-- or when the incremental state cannot be carried (a cell write that padded the
-- grid, or a shape change that may have restored the saved shape). Never on the
-- typing path.
--
-- Every 'ptrEq' in this section is a fast path in front of a real comparison,
-- and a spurious 'False' costs time and never correctness. That is not a
-- formality here: the design this replaced leaned on @ptrEq (csvRows v)
-- (csvSaved v)@ to make the unmodified case free, and measurement showed that
-- particular test returning 'False' under @-O2@ for a grid whose two fields
-- were assigned from the same binding a moment earlier — while the row-level
-- tests below, on elements of two 'toList's, do fire. So a correctness argument
-- may never rest on one, and a cost argument may not either (plan 0028).
dirtyFrom :: Grid -> Grid -> CsvDirty
dirtyFrom saved rows
  | ptrEq saved rows = DirtyCells 0
  | Seq.length saved /= Seq.length rows = DirtyShape
  | otherwise = go 0 (toList rows) (toList saved)
  where
    go !n (r : rs) (s : ss)
      | ptrEq r s                      = go n rs ss
      | Seq.length r /= Seq.length s   = DirtyShape
      | otherwise                      = go (n + rowDirty s r) rs ss
    go !n _ _ = DirtyCells n

-- | How many cells of two same-length rows differ.
rowDirty :: Row -> Row -> Int
rowDirty a b = go 0 (toList a) (toList b)
  where
    go !n (x : xs) (y : ys) | ptrEq x y || x == y = go n xs ys
                            | otherwise           = go (n + 1) xs ys
    go !n _ _ = n

-- | Carry the dirty state across a grid change — the twin of 'syncWidths', and
-- with the same cost model: pointer-equal rows are skipped, so a change that
-- touches one row costs O(rows) pointer hops and one row comparison.
--
-- A change of shape is the cheap case rather than the expensive one: a row
-- inserted or deleted is answered by the row-count test alone, and a column
-- inserted or deleted by the first row the walk looks at. Only the shape-equal
-- changes (sort, replace-all, a same-shaped paste) actually count cells, and
-- those already rebuild the grid.
syncDirty :: Grid -> CsvDirty -> Grid -> Grid -> CsvDirty
syncDirty saved d old new
  | ptrEq old new = d
  | otherwise = case d of
      -- The shape differed before; this change may have restored it, and only
      -- a full pass can say. (Structural edits are rare next to typing.)
      DirtyShape -> dirtyFrom saved new
      DirtyCells n
        | Seq.length old /= Seq.length new -> DirtyShape
        | otherwise -> go n 0 (toList old) (toList new)
  where
    go !n !i (o : os) (w : ws)
      | ptrEq o w = go n (i + 1) os ws
      | otherwise = case Seq.lookup i saved of
          Just sv | Seq.length w == Seq.length sv ->
                      go (n - rowDirty sv o + rowDirty sv w) (i + 1) os ws
                  | otherwise -> DirtyShape
          Nothing -> dirtyFrom saved new    -- invariant broken; recompute exactly
    go !n _ _ _ = DirtyCells n

-- | Carry the dirty state across a write of one cell, in O(log rows): the
-- caller knows the cell, so the count moves by at most one and nothing else in
-- the grid needs looking at.
--
-- @old@ is the cell's text before the write, @new@ after. A write that /grew/
-- the grid (padding a short row or appending rows) changes the shape and is
-- handed to 'dirtyFrom'; the caller passes @grew@ because it is the one that
-- knows whether 'ensureCell' had anything to do.
dirtyCell :: Grid -> CsvDirty -> Bool -> Int -> Int -> Text -> Text -> Grid -> CsvDirty
dirtyCell saved d grew r c old new rows'
  | grew = dirtyFrom saved rows'
  | otherwise = case d of
      -- The shape still differs from saved: overwriting a cell cannot change
      -- that, and the biconditional in 'CsvDirty' is what lets us say so
      -- without looking.
      DirtyShape   -> DirtyShape
      DirtyCells n -> case Seq.lookup r saved >>= Seq.lookup c of
        Nothing -> dirtyFrom saved rows'    -- invariant broken; recompute exactly
        Just sv -> DirtyCells (n - diffBit old sv + diffBit new sv)
  where
    diffBit x y = if ptrEq x y || x == y then 0 else 1 :: Int

-- Display width of a cell: its widest line (cells may contain newlines).
-- Uses @max 1@ per glyph to agree with the renderer, which emits at least
-- one grid cell per code point; this matters for emoji whose presentation is
-- selected by a following variation selector U+FE0F (e.g. ℹ️, ⚠️, ❤️): the
-- selector is zero-width but the terminal folds it into a two-cell emoji,
-- so a column that ignored it would come up one cell short and the whole
-- row's right-hand columns would shift. Truly invisible formatting controls
-- (ZWSP, LTR/RTL marks, BOM, …) are counted as zero — terminals emit
-- nothing for those and the cursor does not advance, so including them
-- would over-reserve the column by one cell per occurrence.
--
-- One strict fold over the text, carrying (widest line so far, current line).
-- The obvious spelling — @maximum . map (sum . map effW . T.unpack) . T.splitOn
-- "\\n"@ — unpacks every cell to a @String@ (a cons cell and a boxed 'Char' per
-- character) and allocates a list of slices per cell. That is the single
-- largest cost of opening a table: 2 697 MB of the 3 719 MB a 32 MB CSV used to
-- allocate, because 'computeWidths' calls this on every one of its 2.7 million
-- cells. The fold allocates nothing.
cellWidth :: Text -> Int
cellWidth t = case T.foldl' step (W 0 0) t of W best cur -> max best cur
  where
    step (W best cur) c
      | c == '\n'           = W (max best cur) 0
      | isInvisibleFormat c = W best cur
      | otherwise           = W best (cur + max 1 (charWidth c))

-- Strict accumulator for 'cellWidth': (widest completed line, current line).
data W = W !Int !Int

-- | A cell's display rows grow with embedded newlines, but a row is capped at
-- this many lines on screen (taller cells scroll while being edited).
maxCellLines :: Int
maxCellLines = 3

-- | Number of newline-separated lines in a cell.
cellLineCount :: Text -> Int
cellLineCount t = 1 + T.count (T.pack "\n") t

-- | The number of lines the tallest cell in a row actually has (uncapped).
rowLineCount :: CsvView -> Int -> Int
rowLineCount v r = maximum (1 : map cellLineCount (toList (rowAt r v)))

-- | On-screen height of a table row: its tallest cell, capped at 'maxCellLines'.
rowHeight :: CsvView -> Int -> Int
rowHeight v r = min maxCellLines (rowLineCount v r)

-- | The (line, column) of a character index within (possibly multi-line) text.
cursorLineCol :: Text -> Int -> (Int, Int)
cursorLineCol t c =
  let before = T.take c t
  in (T.count nl before, T.length (last (T.splitOn nl before)))
  where nl = T.pack "\n"

-- | The character index of a (line, column), clamped to the text.
lineColToCursor :: Text -> Int -> Int -> Int
lineColToCursor t line col =
  let ls    = T.splitOn (T.pack "\n") t
      line' = max 0 (min (length ls - 1) line)
      col'  = max 0 (min col (T.length (ls !! line')))
  in sum (map ((+ 1) . T.length) (take line' ls)) + col'

-- | The table row shown @off@ display lines below the first data row (used to
-- map a mouse click to a row when rows have varying heights).
rowAtLineOffset :: CsvView -> Int -> Int
rowAtLineOffset v off = go (csvTop v) 0
  where
    n = nRows v
    go r acc
      | r >= n - 1                 = max 0 (n - 1)
      | off < acc + rowHeight v r  = r
      | otherwise                  = go (r + 1) (acc + rowHeight v r)

------------------------------------------------------------------------------
-- Internal helpers

withRows :: (Grid -> Grid) -> CsvView -> CsvView
withRows f v =
  let rows' = f (csvRows v)
  in v { csvRows = rows', csvWidths = syncWidths (csvRows v) (csvWidths v) rows'
       , csvDirty = syncDirty (csvSaved v) (csvDirty v) (csvRows v) rows'
       , csvNl = syncNl (csvDelim v) (csvRows v) (csvNl v) rows' }

-- | Write one cell, updating the width cache in O(1) for the common case.
--
-- 'withRows' has to *discover* what changed by walking every row
-- ('syncWidths'), which costs O(rows) per keystroke — 7.4 ms and 22 MB on a
-- 300 000-row table, on every character typed. The caller of a single-cell edit
-- already knows the cell, so tell the cache instead of making it search:
--
--  * the new text is at least as wide as the column  -> one 'Seq.update';
--  * the old text was not the column's widest        -> nothing to do;
--  * otherwise the column may have shrunk            -> recompute that one
--    column (O(rows), and only when the widest cell in a column gets narrower —
--    typing widens, so this is rare).
--
-- Structural edits (row/column insert & delete, sort, paste, mapCells) keep
-- using 'withRows', where a diff or a full recomputation is the right answer.
--
-- The modified flag rides along on the same knowledge (plan 0028): the cell's
-- old text, its new text and its saved text are everything needed to move the
-- dirty-cell count by one, so 'isModified' stays exact without the grid
-- comparison that used to cost 2.3 ms and 14 MB per keystroke once a large
-- table had been edited at all.
withCell :: Int -> Int -> Text -> CsvView -> CsvView
withCell r c t v =
  let oldRow = Seq.lookup r (csvRows v)
      old    = maybe T.empty (fromMaybe T.empty . Seq.lookup c) oldRow
      rows'  = setCell r c t (csvRows v)
      -- Did 'ensureCell' have to pad? Then the shape moved and the counts and
      -- widths both need recomputing. In practice the cursor is always inside
      -- the grid, so this never fires.
      grew   = case oldRow of
                 Just row -> c < 0 || c >= Seq.length row
                 Nothing  -> True
      ws    = csvWidths v
      wNew  = clampW (cellWidth t)
      wOld  = clampW (cellWidth old)
      ws'
        | c < 0 || c >= Seq.length ws = computeWidths rows'   -- new column: shape changed
        | wNew >= cur                 = Seq.update c wNew ws
        | wOld < cur                  = ws
        | otherwise                   = Seq.update c (colWidth rows' c) ws
        where cur = Seq.index ws c
      -- The newline map rides the same knowledge: the row's count moves by the
      -- difference between the cell that left and the one that arrived, so
      -- nothing outside this row is looked at. A write that /grew/ the grid
      -- moved every later row index, so that one recomputes.
      nl'
        | grew      = computeNl (csvDelim v) rows'
        | otherwise = setNl r (Map.findWithDefault 0 r (csvNl v) - nlCount old + nlCount t)
                            (csvNl v)
  in v { csvRows = rows', csvWidths = ws'
       , csvDirty = dirtyCell (csvSaved v) (csvDirty v) grew r c old t rows'
       , csvNl = nl' }

snapshot :: CsvView -> CsvView
snapshot v = v { csvUndo = pushHist maxUndo (csvRows v) (csvUndo v), csvRedo = Seq.empty }

-- Ensure a cell (r,c) exists by padding rows/cells with empties.
ensureCell :: Int -> Int -> Grid -> Grid
ensureCell r c grid0 =
  let grid1 = padRows (r + 1) grid0
  in Seq.adjust' (padCells (c + 1)) r grid1
  where
    padRows n g
      | Seq.length g >= n = g
      | otherwise = g <> Seq.replicate (n - Seq.length g) (Seq.singleton T.empty)
    padCells n row
      | Seq.length row >= n = row
      | otherwise = row <> Seq.replicate (n - Seq.length row) T.empty

setCell :: Int -> Int -> Text -> Grid -> Grid
setCell r c t grid = Seq.adjust' (Seq.update c t) r (ensureCell r c grid)

clampCursor :: CsvView -> CsvView
clampCursor v =
  let r = max 0 (min (nRows v - 1) (csvCurRow v))
      c = max 0 (min (nCols v - 1) (csvCurCol v))
  in v { csvCurRow = r, csvCurCol = c }

------------------------------------------------------------------------------
-- Navigation (only when not editing)

moveCursor :: Dir -> CsvView -> CsvView
moveCursor d v = clampCursor $ case d of
  DUp    -> v { csvCurRow = csvCurRow v - 1 }
  DDown  -> v { csvCurRow = csvCurRow v + 1 }
  DLeft  -> v { csvCurCol = csvCurCol v - 1 }
  DRight -> v { csvCurCol = csvCurCol v + 1 }

moveToHomeRow :: CsvView -> CsvView
moveToHomeRow v = v { csvCurCol = 0 }

moveToEndRow :: CsvView -> CsvView
moveToEndRow v = v { csvCurCol = nCols v - 1 }

moveToTop :: CsvView -> CsvView
moveToTop v = clampCursor v { csvCurRow = 0 }

moveToBottom :: CsvView -> CsvView
moveToBottom v = clampCursor v { csvCurRow = nRows v - 1 }

pageMove :: Int -> CsvView -> CsvView
pageMove delta v = clampCursor v { csvCurRow = csvCurRow v + delta }

-- | Place the cursor at a specific (row, col), clamped.
setCursor :: Int -> Int -> CsvView -> CsvView
setCursor r c v = clampCursor v { csvCurRow = r, csvCurCol = c }

-- Tab moves right, wrapping to the start of the next row at the end.
nextCellTab :: Bool -> CsvView -> CsvView
nextCellTab back v
  | back =
      if csvCurCol v > 0 then v { csvCurCol = csvCurCol v - 1 }
      else if csvCurRow v > 0 then clampCursor v { csvCurRow = csvCurRow v - 1, csvCurCol = nCols v - 1 }
      else v
  | otherwise =
      if csvCurCol v < nCols v - 1 then v { csvCurCol = csvCurCol v + 1 }
      else if csvCurRow v < nRows v - 1 then v { csvCurRow = csvCurRow v + 1, csvCurCol = 0 }
      else v

-- | Adjust scroll so the current cell is visible within @rowsVisible@ rows and
-- the given list of column widths fitting in @width@ display columns.
-- | Adjust scroll so the current cell is visible. @availLines@ is the number of
-- display lines below the header; rows can be taller than one line, so the top
-- row is advanced until the current row fits.
-- @availLines@ is the height of the scrolling area; @freezeRows@ rows are pinned
-- at the top (0, or 1 when the header is frozen) and excluded from scrolling.
ensureVisible :: Int -> Int -> Int -> CsvView -> CsvView
ensureVisible availLines freezeRows width v =
  let v1 = v { csvTop = scrollTop (max 1 availLines) freezeRows v }
  in if cellFullyVisible width v1
       -- Already fully in view given (csvLeft, csvXOff): leave the horizontal
       -- scroll untouched, so a scrollbar-produced sub-column offset survives a
       -- csvPut that doesn't move the cursor.
       then v1
       -- Cursor-driven scrolling snaps back to a column boundary (csvXOff = 0).
       else v1 { csvLeft = scrollLeft width v1, csvXOff = 0 }

-- Absolute display-cell offset (gutter excluded) at which column @c@'s cells
-- begin: the summed widths+separators of every earlier column.
colRegionStart :: [Int] -> Int -> Int
colRegionStart effs c = sum (map (+ 1) (take c effs))

-- Is the current cell fully readable within @width@ display cells given the
-- current (csvLeft, csvXOff)? A cell wider than the viewport counts as visible
-- once it starts at the viewport's left edge (today's "left-aligned is
-- visible-enough" semantics), so scrolling never chases an over-wide cell.
cellFullyVisible :: Int -> CsvView -> Bool
cellFullyVisible width v =
  let effs    = columnWidths v
      cc      = csvCurCol v
      start   = colRegionStart effs (csvLeft v) + csvXOff v
      ccStart = colRegionStart effs cc
      wcc     = if cc < length effs then effs !! cc else 0
  in ccStart >= start
       -- The trailing separator (+1) is counted in "fits", matching 'scrollLeft'
       -- exactly, so a keep/recompute decision never disagrees with it.
       && (ccStart + wcc + 1 <= start + width
           || (wcc + 1 > width && ccStart == start))

-- Smallest top row (>= freezeRows, no greater than the current row) such that
-- rows top..current fit within @availLines@ display lines.
scrollTop :: Int -> Int -> CsvView -> Int
scrollTop availLines freezeRows v =
  let cr = csvCurRow v
  in if cr < freezeRows
       then max freezeRows (csvTop v)   -- cursor sits in the frozen area; keep scroll
       else
         let top0 = max freezeRows (min (csvTop v) cr)
             -- Walk up from the cursor accumulating row heights until they would
             -- overflow. O(visible rows), so a jump to the bottom is instant.
             goUp t acc
               | t <= freezeRows                        = freezeRows
               | acc + rowHeight v (t - 1) > availLines  = t
               | otherwise                               = goUp (t - 1) (acc + rowHeight v (t - 1))
             lo = goUp cr (rowHeight v cr)
         in max freezeRows (max top0 lo)

-- Choose the left-most visible column so the current column fits in @width@:
-- keep the current scroll if everything from it through the cursor fits, else
-- the smallest column that fits (the cursor column itself when even that is
-- too wide). One right-to-left walk over at most the visible columns.
scrollLeft :: Int -> CsvView -> Int
scrollLeft width v =
  let ws = csvWidths v
      cc = csvCurCol v
      left0 = max 0 (min (csvLeft v) cc)
      -- Effective width: a user override wins, as in 'columnWidths'.
      costAt c = case Seq.lookup c ws of
        Nothing -> 0
        Just w  -> Map.findWithDefault w c (csvUserW v) + 1
      walk l acc
        | l < 0 = 0
        | acc + costAt l > width = l + 1
        | otherwise = walk (l - 1) (acc + costAt l)
      lmin = min cc (walk cc 0)
  in max left0 lmin

-- | Scroll horizontally to an absolute display-cell offset @x@ measured from
-- the start of the column region (the row-number gutter excluded; column @c@
-- spans @effWidth c + 1@ cells, its cells plus the trailing @│@). Sets
-- (csvLeft, csvXOff) to the column containing @x@ and the remainder within it.
-- Used only by the scrollbar drag/click; pure scroll state, so it never touches
-- rows/widths/cursor/undo. @x@ is clamped at 0 below; the caller clamps above.
hScrollTo :: Int -> CsvView -> CsvView
hScrollTo x v =
  let effs   = columnWidths v
      n      = length effs
      target = max 0 x
      go c acc
        | c >= n                          = (max 0 (n - 1), 0)  -- past the end
        | target < acc + (effs !! c) + 1  = (c, target - acc)
        | otherwise                       = go (c + 1) (acc + (effs !! c) + 1)
      (l, off) = go 0 0
  in v { csvLeft = l, csvXOff = off }

------------------------------------------------------------------------------
-- Mapping between cells and serialised-text positions
--
-- These let the editor keep the cursor in the same place when toggling between
-- the table view and the plain-text view. They mirror 'csvToText' exactly:
-- records joined by @\\n@, fields by the delimiter, each field requoted.

-- (There used to be a @rowSerial@ here — the serialised text of a whole row —
-- and both mappings below walked every row above the target through it. That
-- is what plan 0029 removed: the only thing they needed from those rows was
-- how many lines each takes up, and 'csvNl' knows that without building any
-- text at all.)

-- Serialised prefix of row @r@ up to (not including) field @c@, with the
-- trailing delimiter that precedes field @c@.
prefixSerial :: CsvView -> Int -> Int -> Text
prefixSerial v r c =
  let body = T.intercalate (delimT v) (map (quoteField (csvDelim v)) (take c (toList (rowAt r v))))
  in if c > 0 then body <> delimT v else body

delimT :: CsvView -> Text
delimT = T.singleton . csvDelim

nlCount :: Text -> Int
nlCount = T.count (T.pack "\n")

lastLineLen :: Text -> Int
lastLineLen = T.length . last . T.splitOn (T.pack "\n")

-- | Buffer @(line, column)@ at which a given cell begins in the serialised CSV
-- (accounting for any earlier rows/fields that contain embedded newlines).
--
-- The prefix of rows above @r@ is answered from 'csvNl' rather than by
-- re-serialising them. It used to be the latter, and since the recents, the
-- session file and the crash journal all ask a table document where its cursor
-- is, "re-serialise everything above the cursor" was reachable from an ordinary
-- keystroke: 383 ms and 1 651 MB at the last row of a 223 209-row table
-- (plan 0029).
cellTextPos :: CsvView -> Int -> Int -> (Int, Int)
cellTextPos v r c =
  let baseLine = r + linesBefore v r
      pre      = prefixSerial v r c
  in (baseLine + nlCount pre, lastLineLen pre)

-- | The cell @(row, col)@ that a buffer position falls in. A position sitting
-- on a delimiter maps to the field just before it; out-of-range positions clamp
-- to the nearest cell.
--
-- The inverse of 'cellTextPos', and it reads 'csvNl' the same way: row @i@
-- starts at line @i + linesBefore v i@, which is strictly increasing in @i@, so
-- the row is found by binary search instead of by serialising the file down to
-- the target line.
textPosCell :: CsvView -> Int -> Int -> (Int, Int)
textPosCell v line col =
  let n        = nRows v
      r        = clampI 0 (n - 1) (lastRowAtOrBefore n line)
      fs       = map (quoteField (csvDelim v)) (toList (rowAt r v))
      colStart = scanl (\acc f -> acc + T.length f + 1) 0 fs
      c        = clampI 0 (max 0 (length fs - 1)) (lastLE (dropLast colStart) col)
  in (r, c)
  where
    clampI lo hi = max lo . min hi
    lastLE xs target = length (takeWhile (<= target) xs) - 1
    dropLast [] = []
    dropLast xs = init xs
    rowStart i = i + linesBefore v i
    -- Largest i in [0, n] whose row starts at or before @target@; row 0 starts
    -- at line 0, so a negative target has no answer and clamps to 0 above.
    lastRowAtOrBefore n target
      | target < 0 = -1
      | otherwise  = go 0 n
      where
        go lo hi
          | lo >= hi  = lo
          | otherwise = let mid = (lo + hi + 1) `div` 2
                        in if rowStart mid <= target then go mid hi else go lo (mid - 1)

------------------------------------------------------------------------------
-- Cell editing
--
-- Edits are applied to the grid cell *immediately* (the grid is the live edit
-- target), so any serialisation/sync reflects what has been typed even before
-- the edit is committed. 'csvEdit' only carries the in-cell cursor position
-- and the cell's original value (so Esc can cancel, and commit can record one
-- undo step for the whole edit).

isEditing :: CsvView -> Bool
isEditing v = case csvEdit v of Just _ -> True; Nothing -> False

-- Replace the current cell's text and move the cursor.
putCellCursor :: Text -> Int -> CsvView -> CsvView
putCellCursor t c v = withCell (csvCurRow v) (csvCurCol v) t v { csvEdit = setCur }
  where setCur = fmap (\(_, o) -> (c, o)) (csvEdit v)

-- Begin editing the current cell with its existing contents.
beginEdit :: CsvView -> CsvView
beginEdit v = let t = currentCellText v in v { csvEdit = Just (T.length t, t), csvSelAnchor = Nothing }

-- Begin editing fresh: the typed character replaces the cell immediately.
beginEditFresh :: Char -> CsvView -> CsvView
beginEditFresh ch v =
  let orig = currentCellText v
  in withCell (csvCurRow v) (csvCurCol v) (T.singleton ch)
       v { csvEdit = Just (1, orig), csvSelAnchor = Nothing }

-- Finish editing: keep the (already-applied) cell text; record one undo step
-- for the whole edit if the value actually changed.
commitEdit :: CsvView -> CsvView
commitEdit v = case csvEdit v of
  Nothing -> v
  Just (_, orig)
    | currentCellText v == orig -> v { csvEdit = Nothing }
    | otherwise ->
        let before = setCell (csvCurRow v) (csvCurCol v) orig (csvRows v)
        in v { csvEdit = Nothing
             , csvUndo = pushHist maxUndo before (csvUndo v), csvRedo = Seq.empty }

-- Cancel editing: restore the cell to its original value.
cancelEdit :: CsvView -> CsvView
cancelEdit v = case csvEdit v of
  Nothing       -> v
  Just (_, orig) -> withCell (csvCurRow v) (csvCurCol v) orig v { csvEdit = Nothing }

editCursor :: CsvView -> Int
editCursor v = maybe 0 fst (csvEdit v)

editInsert :: Char -> CsvView -> CsvView
editInsert ch v =
  let cur = currentCellText v; c = editCursor v
  in putCellCursor (T.take c cur <> T.singleton ch <> T.drop c cur) (c + 1) v

editBackspace :: CsvView -> CsvView
editBackspace v =
  let cur = currentCellText v; c = editCursor v
  in if c > 0 then putCellCursor (T.take (c - 1) cur <> T.drop c cur) (c - 1) v else v

editDelete :: CsvView -> CsvView
editDelete v =
  let cur = currentCellText v; c = editCursor v
  in if c < T.length cur then putCellCursor (T.take c cur <> T.drop (c + 1) cur) c v else v

onCur :: (Int -> Int) -> CsvView -> CsvView
onCur f v = case csvEdit v of Just (c, o) -> v { csvEdit = Just (f c, o) }; Nothing -> v

editLeft :: CsvView -> CsvView
editLeft = onCur (\c -> max 0 (c - 1))

editRight :: CsvView -> CsvView
editRight v = onCur (\c -> min (T.length (currentCellText v)) (c + 1)) v

editHome :: CsvView -> CsvView
editHome = onCur (const 0)

editEnd :: CsvView -> CsvView
editEnd v = onCur (const (T.length (currentCellText v))) v

-- | Move the in-cell cursor up/down a line within a multi-line cell, keeping the
-- column. 'Nothing' if there is no line in that direction (the caller then
-- commits and moves to the adjacent cell).
editLineUp :: CsvView -> Maybe CsvView
editLineUp v = case csvEdit v of
  Nothing     -> Nothing
  Just (c, o) ->
    let t = currentCellText v; (line, col) = cursorLineCol t c
    in if line <= 0 then Nothing
       else Just v { csvEdit = Just (lineColToCursor t (line - 1) col, o) }

editLineDown :: CsvView -> Maybe CsvView
editLineDown v = case csvEdit v of
  Nothing     -> Nothing
  Just (c, o) ->
    let t = currentCellText v; (line, col) = cursorLineCol t c
    in if line >= cellLineCount t - 1 then Nothing
       else Just v { csvEdit = Just (lineColToCursor t (line + 1) col, o) }

-- Clear the current cell (navigation mode).
clearCell :: CsvView -> CsvView
clearCell v = withCell (csvCurRow v) (csvCurCol v) T.empty (snapshot v)

-- Set the current cell to a value (used by paste).
setCurrentCell :: Text -> CsvView -> CsvView
setCurrentCell t v = withCell (csvCurRow v) (csvCurCol v) t (snapshot v)

-- | Apply a function to every cell's text (records one undo step). Used by
-- find-and-replace, which therefore only ever touches cell *contents*, never
-- the delimiters between them.
mapCells :: (Text -> Text) -> CsvView -> CsvView
mapCells f v = withRows (fmap (fmap f)) (snapshot v)

------------------------------------------------------------------------------
-- Rectangular cell selection

-- | The selected rectangle as (minRow, minCol, maxRow, maxCol). With no anchor
-- it is just the current cell.
selRect :: CsvView -> (Int, Int, Int, Int)
selRect v = case csvSelAnchor v of
  Nothing       -> (csvCurRow v, csvCurCol v, csvCurRow v, csvCurCol v)
  Just (ar, ac) -> (min ar (csvCurRow v), min ac (csvCurCol v)
                   , max ar (csvCurRow v), max ac (csvCurCol v))

-- | Is more than one cell selected?
hasSelection :: CsvView -> Bool
hasSelection v = let (r0, c0, r1, c1) = selRect v in r0 /= r1 || c0 /= c1

clearSel :: CsvView -> CsvView
clearSel v = v { csvSelAnchor = Nothing }

-- | Anchor the current cell (if not already anchored), then apply a movement —
-- so the selection grows from the original cell. For Shift+navigation / drag.
withSel :: (CsvView -> CsvView) -> CsvView -> CsvView
withSel move v = move $ case csvSelAnchor v of
  Just _  -> v
  Nothing -> v { csvSelAnchor = Just (csvCurRow v, csvCurCol v) }

selCells :: CsvView -> [(Int, Int)]
selCells v = let (r0, c0, r1, c1) = selRect v in [ (r, c) | r <- [r0 .. r1], c <- [c0 .. c1] ]

-- The selected rectangle serialised as a (mini) CSV.
selectionText :: CsvView -> Text
selectionText v =
  let (r0, c0, r1, c1) = selRect v
  in T.intercalate (T.pack "\n")
       [ T.intercalate (delimT v) [ quoteField (csvDelim v) (cellAt r c v) | c <- [c0 .. c1] ]
       | r <- [r0 .. r1] ]

-- | Text to put on the clipboard for copy/cut: the raw value for one cell, a
-- mini-CSV for a rectangle.
copyText :: CsvView -> Text
copyText v = if hasSelection v then selectionText v else currentCellText v

-- Write a list of (row, col, text), recording one undo step.
setCells :: [(Int, Int, Text)] -> CsvView -> CsvView
setCells cells v =
  withRows (\g -> foldl (\grid (r, c, t) -> setCell r c t grid) g cells) (snapshot v)

-- | Clear / fill all selected cells (one undo step), keeping the selection.
clearSelCells :: CsvView -> CsvView
clearSelCells v = setCells [ (r, c, T.empty) | (r, c) <- selCells v ] v

fillSelCells :: Text -> CsvView -> CsvView
fillSelCells t v = setCells [ (r, c, t) | (r, c) <- selCells v ] v

-- Write a grid with its top-left corner at (r0,c0), expanding the table to fit.
writeGridAt :: Int -> Int -> Grid -> CsvView -> CsvView
writeGridAt r0 c0 cg v =
  clampCursor (setCells [ (r0 + i, c0 + j, t)
                        | (i, row) <- zip [0 ..] (toList cg)
                        , (j, t)   <- zip [0 ..] (toList row) ] v)

-- | Paste clipboard text per the selection, returning the new view and a status
-- message. A single scalar fills a multi-cell selection (or sets one cell); a
-- grid spreads from a single cell, or overwrites a same-shaped selection.
pasteClip :: Text -> CsvView -> (CsvView, Text)
pasteClip txt v =
  let body = T.dropWhileEnd (== '\n') txt
      clip = csvParse (csvDelim v) body
      cgR  = Seq.length clip
      cgC  = maximum (1 : map Seq.length (toList clip))
      single = cgR <= 1 && cgC <= 1
      (r0, c0, r1, c1) = selRect v
      selR = r1 - r0 + 1; selC = c1 - c0 + 1
      multi = hasSelection v
  in if single
       then if multi then (fillSelCells body v, T.pack "Filled selection")
                     else (setCurrentCell body v, T.pack "Pasted")
       else if not multi
              then (writeGridAt (csvCurRow v) (csvCurCol v) clip v, T.pack "Pasted")
              else if cgR == selR && cgC == selC
                     then (writeGridAt r0 c0 clip v, T.pack "Pasted")
                     else (v, T.pack "Clipboard shape doesn't match the selection")

------------------------------------------------------------------------------
-- Structure: rows and columns

emptyRow :: CsvView -> Row
emptyRow v = Seq.replicate (nCols v) T.empty

insertRowAbove :: CsvView -> CsvView
insertRowAbove v =
  let v' = snapshot v
  in clampCursor (withRows (Seq.insertAt (csvCurRow v) (emptyRow v)) v')

insertRowBelow :: CsvView -> CsvView
insertRowBelow v =
  let v' = snapshot v
      r  = csvCurRow v + 1
  in clampCursor (withRows (Seq.insertAt r (emptyRow v)) v') { csvCurRow = r }

deleteRow :: CsvView -> CsvView
deleteRow v
  | nRows v <= 1 = withRows (const (Seq.singleton (Seq.singleton T.empty))) (snapshot v)
                     { csvCurRow = 0, csvCurCol = 0 }
  | otherwise =
      let v' = snapshot v
      in clampCursor (withRows (Seq.deleteAt (csvCurRow v)) v')

insertColAt :: Int -> CsvView -> CsvView
insertColAt c v =
  let v' = snapshot v
      cols = nCols v
      ins row = Seq.insertAt (min c (Seq.length row)) T.empty (padTo cols row)
      padTo n row | Seq.length row >= n = row
                  | otherwise = row <> Seq.replicate (n - Seq.length row) T.empty
      -- Width overrides follow their columns rightward past the insertion.
      userW = Map.mapKeysMonotonic (\k -> if k >= c then k + 1 else k) (csvUserW v)
  in (withRows (fmap ins) v') { csvUserW = userW }

insertColLeft :: CsvView -> CsvView
insertColLeft v = insertColAt (csvCurCol v) v

insertColRight :: CsvView -> CsvView
insertColRight v = (insertColAt (csvCurCol v + 1) v) { csvCurCol = csvCurCol v + 1 }

deleteCol :: CsvView -> CsvView
deleteCol v
  | nCols v <= 1 = withRows (fmap (const (Seq.singleton T.empty))) (snapshot v)
                     { csvCurCol = 0 }
  | otherwise =
      let v' = snapshot v
          c  = csvCurCol v
          del row = if c < Seq.length row then Seq.deleteAt c row else row
          -- Drop the deleted column's override; the rest follow their columns.
          userW = Map.mapKeysMonotonic (\k -> if k > c then k - 1 else k)
                    (Map.delete c (csvUserW v))
      in clampCursor ((withRows (fmap del) v') { csvUserW = userW })

------------------------------------------------------------------------------
-- Sorting
--
-- Before sorting we sniff the column's data type from the first non-empty
-- cell and verify every other non-empty cell matches — timestamps, dates
-- (ISO / DMY / MDY, disambiguated by which format the whole column agrees
-- on), times (12h/24h), money, percentages, and thousands-grouped numerics
-- each sort by their true value rather than alphabetically. Empties always
-- sink to the bottom in both directions. If any cell breaks the pattern,
-- we fall back to plain alphanumeric sorting (never error out) so the
-- user's arrow-key sort is guaranteed to do *something* reasonable.

-- Parsers listed most-specific first: the first one whose interpretation
-- fits every non-empty cell wins.
colParsers :: [Text -> Maybe Double]
colParsers =
  [ parseTimestampISO
  , parseISODate
  , parseDMYDate
  , parseMDYDate
  , parseTime
  , parseMoney
  , parsePercent
  , parseNumericTyped
  ]

-- Pick the parser under which every non-empty cell in @raws@ agrees on a
-- typed value; 'Nothing' means the column is heterogeneous and should sort
-- as plain text.
detectColParser :: [Text] -> Maybe (Text -> Maybe Double)
detectColParser raws =
  let nonEmpty = [ r | r <- raws, not (T.null (T.strip r)) ]
  in case nonEmpty of
       []    -> Nothing
       (h:_) ->
         let ok p = case p h of
               Just _  -> all (\c -> case p c of Just _ -> True; _ -> False) nonEmpty
               Nothing -> False
         in case filter ok colParsers of
              []      -> Nothing
              (p : _) -> Just p

-- Plain-text sort key: empty cells last (in both directions), any cell that
-- happens to look like a plain Haskell-readable number sorts before text
-- (case-folded) — the ordering a spreadsheet user expects when nothing
-- fancier is detected.
sortKeyOf :: Text -> (Bool, Either Double Text)
sortKeyOf t =
  let s = T.strip t
  in ( T.null s
     , case readMaybe (T.unpack s) :: Maybe Double of
         Just d  -> Left d
         Nothing -> Right (T.toCaseFold s) )

-- Typed sort key: empties last, then numeric under the sniffed parser, then
-- anything that doesn't parse falls through to text. In practice the
-- fallthrough never fires because detectColParser only picks a parser that
-- succeeds on every non-empty cell — but keep it defensive so a rogue cell
-- can't crash the sort.
sortKeyOfTyped :: (Text -> Maybe Double) -> Text -> (Bool, Either Double Text)
sortKeyOfTyped p t =
  let s = T.strip t
  in ( T.null s
     , case p t of
         Just d  -> Left d
         Nothing -> Right (T.toCaseFold s) )

cellIn :: Int -> Row -> Text
cellIn c row = if c < Seq.length row then Seq.index row c else T.empty

-- | Sort the rows by column @c@. @keepHeader@ pins row 0 (the frozen header).
-- Undoable (one checkpoint), stable, and the cursor follows its row to the
-- row's new position.
sortByColumn :: Int -> Bool -> Bool -> CsvView -> CsvView
sortByColumn c asc keepHeader v0 =
  let v1 = snapshot (commitEdit v0)
      rows = csvRows v1
      hdrN = if keepHeader && not (Seq.null rows) then 1 else 0
      body = toList (Seq.drop hdrN rows)
      cells = [ cellIn c row | row <- body ]
      keyF = case detectColParser cells of
        Just p  -> sortKeyOfTyped p
        Nothing -> sortKeyOf
      dec  = [ (keyF (cellIn c row), i, row) | (i, row) <- zip [0 :: Int ..] body ]
      cmp ((e1, k1), _, _) ((e2, k2), _, _) =
        compare e1 e2 <> (if asc then compare k1 k2 else compare k2 k1)
      sorted = sortBy cmp dec
      order  = [ i | (_, i, _) <- sorted ]
      rows'  = Seq.take hdrN rows <> Seq.fromList [ r | (_, _, r) <- sorted ]
      oldCur = csvCurRow v1
      cur' | oldCur < hdrN = oldCur
           | otherwise = hdrN + fromMaybe 0 (elemIndex (oldCur - hdrN) order)
  in clampCursor (withRows (const rows') v1) { csvCurRow = cur' }

-- | Are the (non-pinned) rows already in ascending order by column @c@? Used
-- to make the sort key toggle: ascending first, descending when re-applied.
-- Uses the same typed sniff as 'sortByColumn' so a date-sorted column doesn't
-- re-sort ascending under an alpha comparison — Alt+S toggles cleanly.
sortedAscBy :: Int -> Bool -> CsvView -> Bool
sortedAscBy c keepHeader v =
  let hdrN = if keepHeader && not (Seq.null (csvRows v)) then 1 else 0
      cells = [ cellIn c row | row <- toList (Seq.drop hdrN (csvRows v)) ]
      keyF = case detectColParser cells of
        Just p  -> sortKeyOfTyped p
        Nothing -> sortKeyOf
      keys = map keyF cells
  in and (zipWith (<=) keys (drop 1 keys))

------------------------------------------------------------------------------
-- Typed cell parsers. Each returns 'Just' a comparable 'Double' when the
-- text parses under its format, and 'Nothing' otherwise. Trailing junk is
-- always rejected so a partial match can't masquerade as the type.

isDig :: Char -> Bool
isDig c = c >= '0' && c <= '9'

readDigits :: Text -> Int
readDigits = T.foldl' (\a c -> a * 10 + (ord c - ord '0')) 0

-- Consume 1..n digits.
takeIntUpTo :: Int -> Text -> Maybe (Int, Text)
takeIntUpTo n t =
  let (ds, rest) = T.span isDig t
      k = T.length ds
  in if k >= 1 && k <= n then Just (readDigits ds, rest) else Nothing

-- Consume exactly n digits.
takeIntN :: Int -> Text -> Maybe (Int, Text)
takeIntN n t =
  let (ds, rest) = T.span isDig t
  in if T.length ds == n then Just (readDigits ds, rest) else Nothing

consumeChar :: Char -> Text -> Maybe Text
consumeChar c t = case T.uncons t of
  Just (x, r) | x == c -> Just r
  _                    -> Nothing

isLeap :: Int -> Bool
isLeap y = (y `mod` 4 == 0 && y `mod` 100 /= 0) || y `mod` 400 == 0

daysInMonth :: Int -> Int -> Int
daysInMonth y m = case m of
  1 -> 31; 3 -> 31; 5 -> 31; 7 -> 31; 8 -> 31; 10 -> 31; 12 -> 31
  4 -> 30; 6 -> 30; 9 -> 30; 11 -> 30
  2 -> if isLeap y then 29 else 28
  _ -> 0

validDate :: Int -> Int -> Int -> Bool
validDate y m d = y >= 1 && m >= 1 && m <= 12 && d >= 1 && d <= daysInMonth y m

-- 2-digit years: 00..69 → 2000..2069; 70..99 → 1970..1999 (Excel convention).
takeYear :: Text -> Maybe (Int, Text)
takeYear t =
  let (ds, rest) = T.span isDig t
  in case T.length ds of
       4 -> Just (readDigits ds, rest)
       2 -> let n = readDigits ds
            in Just (if n < 70 then 2000 + n else 1900 + n, rest)
       _ -> Nothing

-- Compact monotone ordinals — used as comparison keys, not real dates.
dateOrdinal :: Int -> Int -> Int -> Double
dateOrdinal y m d = fromIntegral (y * 10000 + m * 100 + d)

dtOrdinal :: Int -> Int -> Int -> Int -> Int -> Int -> Double
dtOrdinal y mo d h mi s = fromIntegral $
  ((y * 10000 + mo * 100 + d) * 1000000) + h * 10000 + mi * 100 + s

parseISODate :: Text -> Maybe Double
parseISODate raw = do
  let t = T.strip raw
  (y, t1) <- takeIntN 4 t
  t2      <- consumeChar '-' t1
  (m, t3) <- takeIntN 2 t2
  t4      <- consumeChar '-' t3
  (d, t5) <- takeIntN 2 t4
  if T.null t5 && validDate y m d
    then Just (dateOrdinal y m d)
    else Nothing

parseDMYDate :: Text -> Maybe Double
parseDMYDate raw = do
  let t = T.strip raw
  (d, t1)    <- takeIntUpTo 2 t
  (sep, t2)  <- case T.uncons t1 of
    Just (c, r) | c `elem` ("/-." :: String) -> Just (c, r)
    _                                        -> Nothing
  (m, t3)    <- takeIntUpTo 2 t2
  t4         <- consumeChar sep t3
  (y, t5)    <- takeYear t4
  if T.null t5 && validDate y m d
    then Just (dateOrdinal y m d)
    else Nothing

parseMDYDate :: Text -> Maybe Double
parseMDYDate raw = do
  let t = T.strip raw
  (m, t1)    <- takeIntUpTo 2 t
  (sep, t2)  <- case T.uncons t1 of
    Just (c, r) | c `elem` ("/-." :: String) -> Just (c, r)
    _                                        -> Nothing
  (d, t3)    <- takeIntUpTo 2 t2
  t4         <- consumeChar sep t3
  (y, t5)    <- takeYear t4
  if T.null t5 && validDate y m d
    then Just (dateOrdinal y m d)
    else Nothing

parseTime :: Text -> Maybe Double
parseTime raw = do
  let t0 = T.strip raw
  (h0, t1) <- takeIntUpTo 2 t0
  t2       <- consumeChar ':' t1
  (mi, t3) <- takeIntN 2 t2
  (s, t4)  <- case T.uncons t3 of
    Just (':', r) -> takeIntN 2 r
    _             -> Just (0, t3)
  let suff = T.toLower (T.strip t4)
  h <- if T.null suff
         then if h0 <= 23 then Just h0 else Nothing
         else if suff == T.pack "am" && h0 >= 1 && h0 <= 12
                then Just (if h0 == 12 then 0 else h0)
                else if suff == T.pack "pm" && h0 >= 1 && h0 <= 12
                        then Just (if h0 == 12 then 12 else h0 + 12)
                        else Nothing
  if mi < 60 && s < 60
    then Just (fromIntegral (h * 3600 + mi * 60 + s))
    else Nothing

parseTimestampISO :: Text -> Maybe Double
parseTimestampISO raw = do
  let t = T.strip raw
  (y, t1)  <- takeIntN 4 t
  t2       <- consumeChar '-' t1
  (mo, t3) <- takeIntN 2 t2
  t4       <- consumeChar '-' t3
  (d, t5)  <- takeIntN 2 t4
  t6       <- case T.uncons t5 of
    Just ('T', r) -> Just r
    Just (' ', r) -> Just r
    _             -> Nothing
  (h, t7)  <- takeIntUpTo 2 t6
  t8       <- consumeChar ':' t7
  (mi, t9) <- takeIntN 2 t8
  (s, t10) <- case T.uncons t9 of
    Just (':', r) -> takeIntN 2 r
    _             -> Just (0, t9)
  let tE = T.strip (case T.uncons t10 of
                      Just ('Z', r) -> r
                      _             -> t10)
  if T.null tE && validDate y mo d && h <= 23 && mi < 60 && s < 60
    then Just (dtOrdinal y mo d h mi s)
    else Nothing

-- Numeric with optional sign, optional decimal, and optional US-style
-- thousands separators. Strict: '1,23' or '12,3456' are rejected so the
-- sniffer can distinguish a real thousands-grouped column from noise.
parseNumericTyped :: Text -> Maybe Double
parseNumericTyped raw =
  let t = T.strip raw
      (sign, t1) = case T.uncons t of
        Just ('-', r) -> (-1 :: Double, r)
        Just ('+', r) -> (1, r)
        _             -> (1, t)
  in case parseIntPartStrict t1 of
       Nothing            -> Nothing
       Just (intTxt, t2)  ->
         let mDec = case T.uncons t2 of
               Just ('.', r) ->
                 let (ds, r') = T.span isDig r
                 in if T.null ds then Nothing else Just (T.cons '.' ds, r')
               _             -> Just (T.empty, t2)
         in case mDec of
              Nothing            -> Nothing
              Just (decTxt, t3)
                | T.null t3 ->
                    case readMaybe (T.unpack (intTxt <> decTxt)) :: Maybe Double of
                      Just d  -> Just (sign * d)
                      Nothing -> Nothing
                | otherwise -> Nothing

parseIntPartStrict :: Text -> Maybe (Text, Text)
parseIntPartStrict s =
  let (g0, r0) = T.span isDig s
  in if T.null g0 then Nothing
     else case T.uncons r0 of
       Just (',', _)
         | T.length g0 <= 3 -> groupsOnce g0 r0
         | otherwise        -> Nothing
       _                    -> Just (g0, r0)
  where
    groupsOnce acc r = case T.uncons r of
      Just (',', rest) ->
        let (grp, rest') = T.span isDig rest
        in if T.length grp == 3 then groupsOnce (acc <> grp) rest'
                                else Nothing
      _ -> Just (acc, r)

-- Money: requires a currency symbol (prefix or suffix). Accounting parens
-- for negatives are allowed ("($1,234.50)").
parseMoney :: Text -> Maybe Double
parseMoney raw =
  let t = T.strip raw
      (negParen, tCore) =
        case (T.uncons t, T.unsnoc t) of
          (Just ('(', _), Just (_, ')'))
            | T.length t >= 2 -> (True, T.dropEnd 1 (T.drop 1 t))
          _                   -> (False, t)
      (sign1, tSign) = case T.uncons tCore of
        Just ('-', r) -> (-1 :: Double, r)
        Just ('+', r) -> (1, r)
        _             -> (1, tCore)
      sign = if negParen then -sign1 else sign1
      currencyChars = "$£€¥₩₹₽¢" :: String
      (tNum, hasSym) =
        case T.uncons tSign of
          Just (c, r) | c `elem` currencyChars -> (T.stripStart r, True)
          _ ->
            case T.unsnoc tSign of
              Just (r, c) | c `elem` currencyChars -> (T.stripEnd r, True)
              _                                    -> (tSign, False)
  in if hasSym
       then fmap (sign *) (parseNumericTyped tNum)
       else Nothing

-- Percent: number with a trailing '%'.
parsePercent :: Text -> Maybe Double
parsePercent raw =
  let t = T.strip raw
  in case T.unsnoc t of
       Just (r, '%') -> parseNumericTyped (T.stripEnd r)
       _             -> Nothing

------------------------------------------------------------------------------
-- Undo / redo

undo :: CsvView -> CsvView
undo v = case Seq.viewl (csvUndo v) of
  Seq.EmptyL -> v
  (g Seq.:< gs) ->
    clampCursor v { csvRows = g, csvUndo = gs
                  , csvRedo = pushHist maxUndo (csvRows v) (csvRedo v)
                  , csvEdit = Nothing, csvSelAnchor = Nothing
                  , csvWidths = syncWidths (csvRows v) (csvWidths v) g
                  , csvDirty = syncDirty (csvSaved v) (csvDirty v) (csvRows v) g
                  , csvNl = syncNl (csvDelim v) (csvRows v) (csvNl v) g }

redo :: CsvView -> CsvView
redo v = case Seq.viewl (csvRedo v) of
  Seq.EmptyL -> v
  (g Seq.:< gs) ->
    clampCursor v { csvRows = g, csvRedo = gs
                  , csvUndo = pushHist maxUndo (csvRows v) (csvUndo v)
                  , csvEdit = Nothing, csvSelAnchor = Nothing
                  , csvWidths = syncWidths (csvRows v) (csvWidths v) g
                  , csvDirty = syncDirty (csvSaved v) (csvDirty v) (csvRows v) g
                  , csvNl = syncNl (csvDelim v) (csvRows v) (csvNl v) g }

-- | Give @new@ the undo history of @old@, with @old@'s grid pushed as the most
-- recent undo step. Used when a CSV document is edited as plain text and then
-- switched back to the table: undoing first reverts the text edit (back to
-- @old@), then continues through the table's earlier history.
rebaseHistory :: CsvView -> CsvView -> CsvView
rebaseHistory old new =
  new { csvUndo = pushHist maxUndo (csvRows old) (csvUndo old), csvRedo = Seq.empty
      , csvSaved = csvSaved old   -- keep the original saved point across a text edit
      -- A new saved point means the carried dirty state means nothing; the
      -- whole file was just re-parsed, so one more pass over it is noise.
      , csvDirty = dirtyFrom (csvSaved old) (csvRows new) }
      -- 'csvWidths' and 'csvNl' are deliberately absent: both are functions of
      -- 'csvRows' alone, this keeps @new@'s grid, and @new@ built them itself.

-- | Has the grid diverged from the last saved/loaded state?
--
-- O(1): the answer is maintained as state ('csvDirty') by the handful of
-- functions allowed to move 'csvRows' or 'csvSaved', rather than recomputed
-- here. It is exact at any table size — the flag drives the title bar, the
-- quit confirmation, Save All and the crash journal, so an approximation or a
-- big-table cutoff would be a way to lose a user's work.
--
-- It used to be a pointer-accelerated comparison of the two grids, which is
-- free while the grid /is/ the saved grid (one pointer test) and O(rows) as
-- soon as it is not. That made the cost land exactly where it hurts: 2.3 ms
-- and 14 MB per keystroke on a 223 000-row table, from the first edit onwards.
isModified :: CsvView -> Bool
isModified v = case csvDirty v of
  DirtyShape   -> True
  DirtyCells n -> n /= 0

-- | Mark the current grid as the saved state (called after writing the file).
markSaved :: CsvView -> CsvView
markSaved v = v { csvSaved = csvRows v, csvDirty = DirtyCells 0 }

-- | Declare that there is no saved grid to compare against — the table twin of
-- a recovered document's empty 'Cmedit.EditorState.docSavedBuffer'.
--
-- A view built by 'mkCsvGrid' takes the grid it was handed as its own saved
-- point, which is right for a file just read off disk and *wrong* for a grid
-- recovered from a crash journal: that one differs from disk by definition —
-- that is why the journal existed — so adopting it as the baseline let the
-- first undo declare the table clean, which drops the journal that is still
-- the only copy of the work (and then lets Ctrl+Q leave without asking).
--
-- The empty grid is an exact baseline rather than a sentinel, so it needs no
-- special case anywhere else: 'mkCsvGrid' guarantees at least one row, so the
-- shapes differ, 'DirtyShape' is the biconditional's own answer, and no cell
-- write can restore a shape of zero rows. Every dirty path stays O(1) on it
-- (the row-count test answers first), and 'markSaved' re-baselines normally on
-- the save that resolves the recovery.
markUnsaved :: CsvView -> CsvView
markUnsaved v = v { csvSaved = Seq.empty, csvDirty = DirtyShape }
