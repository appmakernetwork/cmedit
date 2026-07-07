-- | The CSV "table" model: a spreadsheet-style view of a CSV/TSV file. This
-- module is pure — it parses CSV text into a grid, lets you navigate and edit
-- cells, insert/delete rows and columns, and serialises back to CSV text. The
-- editor drives it; the renderer draws it.
module Cmedit.Csv
  ( CsvView(..)
  , mkCsvView
  , csvToText
  , csvParse
    -- * Dimensions / access
  , nRows
  , nCols
  , cellAt
  , colLabel
  , columnWidths
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
  , isModified
  , markSaved
  ) where

import Data.Char (chr, ord)
import Data.Foldable (toList)
import Data.List (elemIndex, nub, sortBy)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

import Cmedit.Types (Dir(..), ptrEq)
import Cmedit.Width (charWidth)

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
  , csvUndo   :: ![Grid]
  , csvRedo   :: ![Grid]
  , csvSaved  :: !Grid                -- ^ Grid as last saved/loaded (for the modified flag).
  , csvSelAnchor :: !(Maybe (Int, Int)) -- ^ Other corner of a rectangular cell selection.
  , csvWidths :: !(Seq Int)           -- ^ Clamped display width per column, kept in sync with
                                      --   'csvRows' by 'withRows'/undo/redo (scanning the whole
                                      --   grid per keystroke would freeze large tables).
  , csvUserW  :: !(Map Int Int)       -- ^ User width overrides (header-border drag), by column.
                                      --   Sparse; wins over the content-fitted 'csvWidths' entry.
  } deriving (Show)

maxUndo :: Int
maxUndo = 500

------------------------------------------------------------------------------
-- Construction / serialisation

-- | Build a table view from CSV text, using the given delimiter.
mkCsvView :: Char -> Text -> CsvView
mkCsvView delim t =
  let rows0 = csvParse delim t
      rows  = if Seq.null rows0 then Seq.singleton (Seq.singleton T.empty) else rows0
  in CsvView
       { csvRows = rows, csvCurRow = 0, csvCurCol = 0, csvTop = 0, csvLeft = 0
       , csvXOff = 0
       , csvEdit = Nothing, csvDelim = delim, csvUndo = [], csvRedo = []
       , csvSaved = rows, csvSelAnchor = Nothing
       , csvWidths = computeWidths rows, csvUserW = Map.empty }

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
csvParse :: Char -> Text -> Grid
csvParse delim = Seq.fromList . map Seq.fromList . rows . T.unpack
  where
    rows [] = []
    rows s  = let (row, rest, more) = oneRow s
              in row : if more then rows rest else []

    oneRow s = collect s []
    collect s acc =
      let (f, rest, term) = field s
      in case term of
           TDelim   -> collect rest (f : acc)
           TNewline -> (reverse (f : acc), rest, not (null rest))
           TEof     -> (reverse (f : acc), rest, False)

    field ('"' : cs) = quoted cs []
    field cs         = unquoted cs []

    quoted ('"' : '"' : cs) acc = quoted cs ('"' : acc)
    quoted ('"' : cs) acc       = close cs (reverse acc)
    quoted (c : cs) acc         = quoted cs (c : acc)
    quoted [] acc               = (T.pack (reverse acc), [], TEof)

    -- After a closing quote, expect a delimiter or newline; tolerate stray text.
    close (c : cs) val
      | c == delim = (T.pack val, cs, TDelim)
      | c == '\n'  = (T.pack val, cs, TNewline)
      | c == '\r'  = (T.pack val, dropLF cs, TNewline)
      | otherwise  = unquoted cs (reverse (c : reverse val))   -- append stray char
    close [] val = (T.pack val, [], TEof)

    unquoted (c : cs) acc
      | c == delim = (T.pack (reverse acc), cs, TDelim)
      | c == '\n'  = (T.pack (reverse acc), cs, TNewline)
      | c == '\r'  = (T.pack (reverse acc), dropLF cs, TNewline)
      | otherwise  = unquoted cs (c : acc)
    unquoted [] acc = (T.pack (reverse acc), [], TEof)

    dropLF ('\n' : cs) = cs
    dropLF cs          = cs

data Term = TDelim | TNewline | TEof

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
computeWidths :: Grid -> Seq Int
computeWidths rows =
  let cols = max 1 (maximum (0 : map Seq.length (toList rows)))
      step ws row = foldl bump ws (zip [0 ..] (toList row))
      bump ws (c, cell) = Seq.adjust' (max (clampW (cellWidth cell))) c ws
  in foldl step (Seq.replicate cols (clampW 1)) (toList rows)

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

-- Display width of a cell: its widest line (cells may contain newlines).
cellWidth :: Text -> Int
cellWidth = maximum . (0 :) . map lineW . T.splitOn (T.pack "\n")
  where lineW = sum . map (max 0 . charWidth) . T.unpack

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
  in v { csvRows = rows', csvWidths = syncWidths (csvRows v) (csvWidths v) rows' }

snapshot :: CsvView -> CsvView
snapshot v = v { csvUndo = take maxUndo (csvRows v : csvUndo v), csvRedo = [] }

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

-- Serialised text of a whole row (matches one line family of 'csvToText').
rowSerial :: CsvView -> Int -> Text
rowSerial v i = T.intercalate (delimT v) (map (quoteField (csvDelim v)) (toList (rowAt i v)))

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
cellTextPos :: CsvView -> Int -> Int -> (Int, Int)
cellTextPos v r c =
  let baseLine = r + sum [ nlCount (rowSerial v i) | i <- [0 .. r - 1] ]
      pre      = prefixSerial v r c
  in (baseLine + nlCount pre, lastLineLen pre)

-- | The cell @(row, col)@ that a buffer position falls in. A position sitting
-- on a delimiter maps to the field just before it; out-of-range positions clamp
-- to the nearest cell.
textPosCell :: CsvView -> Int -> Int -> (Int, Int)
textPosCell v line col =
  let n        = nRows v
      starts   = scanl (\acc i -> acc + 1 + nlCount (rowSerial v i)) 0 [0 .. n - 1]
      r        = clampI 0 (n - 1) (lastLE starts line)
      fs       = map (quoteField (csvDelim v)) (toList (rowAt r v))
      colStart = scanl (\acc f -> acc + T.length f + 1) 0 fs
      c        = clampI 0 (max 0 (length fs - 1)) (lastLE (dropLast colStart) col)
  in (r, c)
  where
    clampI lo hi = max lo . min hi
    lastLE xs target = length (takeWhile (<= target) xs) - 1
    dropLast [] = []
    dropLast xs = init xs

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
putCellCursor t c v = withRows (setCell (csvCurRow v) (csvCurCol v) t) v { csvEdit = setCur }
  where setCur = fmap (\(_, o) -> (c, o)) (csvEdit v)

-- Begin editing the current cell with its existing contents.
beginEdit :: CsvView -> CsvView
beginEdit v = let t = currentCellText v in v { csvEdit = Just (T.length t, t), csvSelAnchor = Nothing }

-- Begin editing fresh: the typed character replaces the cell immediately.
beginEditFresh :: Char -> CsvView -> CsvView
beginEditFresh ch v =
  let orig = currentCellText v
  in withRows (setCell (csvCurRow v) (csvCurCol v) (T.singleton ch))
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
             , csvUndo = take maxUndo (before : csvUndo v), csvRedo = [] }

-- Cancel editing: restore the cell to its original value.
cancelEdit :: CsvView -> CsvView
cancelEdit v = case csvEdit v of
  Nothing       -> v
  Just (_, orig) -> withRows (setCell (csvCurRow v) (csvCurCol v) orig) v { csvEdit = Nothing }

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
clearCell v = withRows (setCell (csvCurRow v) (csvCurCol v) T.empty) (snapshot v)

-- Set the current cell to a value (used by paste).
setCurrentCell :: Text -> CsvView -> CsvView
setCurrentCell t v = withRows (setCell (csvCurRow v) (csvCurCol v) t) (snapshot v)

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

-- The comparison key for one cell: empty cells sort after everything (in both
-- directions), numbers compare numerically and sort before text (which
-- compares case-folded) — the ordering a spreadsheet user expects.
sortKeyOf :: Text -> (Bool, Either Double Text)
sortKeyOf t =
  let s = T.strip t
  in ( T.null s
     , case readMaybe (T.unpack s) :: Maybe Double of
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
      dec  = [ (sortKeyOf (cellIn c row), i, row) | (i, row) <- zip [0 :: Int ..] body ]
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
sortedAscBy :: Int -> Bool -> CsvView -> Bool
sortedAscBy c keepHeader v =
  let hdrN = if keepHeader && not (Seq.null (csvRows v)) then 1 else 0
      keys = [ sortKeyOf (cellIn c row) | row <- toList (Seq.drop hdrN (csvRows v)) ]
  in and (zipWith (<=) keys (drop 1 keys))

------------------------------------------------------------------------------
-- Undo / redo

undo :: CsvView -> CsvView
undo v = case csvUndo v of
  []       -> v
  (g : gs) -> clampCursor v { csvRows = g, csvUndo = gs, csvRedo = csvRows v : csvRedo v
                            , csvEdit = Nothing, csvSelAnchor = Nothing
                            , csvWidths = syncWidths (csvRows v) (csvWidths v) g }

redo :: CsvView -> CsvView
redo v = case csvRedo v of
  []       -> v
  (g : gs) -> clampCursor v { csvRows = g, csvRedo = gs, csvUndo = csvRows v : csvUndo v
                            , csvEdit = Nothing, csvSelAnchor = Nothing
                            , csvWidths = syncWidths (csvRows v) (csvWidths v) g }

-- | Give @new@ the undo history of @old@, with @old@'s grid pushed as the most
-- recent undo step. Used when a CSV document is edited as plain text and then
-- switched back to the table: undoing first reverts the text edit (back to
-- @old@), then continues through the table's earlier history.
rebaseHistory :: CsvView -> CsvView -> CsvView
rebaseHistory old new =
  new { csvUndo = take maxUndo (csvRows old : csvUndo old), csvRedo = []
      , csvSaved = csvSaved old }   -- keep the original saved point across a text edit

-- | Has the grid diverged from the last saved/loaded state? Runs on every
-- keystroke, so the comparison is pointer-accelerated: persistent 'Seq' edits
-- share every untouched row/cell with the saved grid, so unchanged structure
-- short-circuits in one pointer test instead of a content compare (editing
-- the last row of a huge table would otherwise re-compare everything above
-- it, every key).
isModified :: CsvView -> Bool
isModified v = not (sameGrid (csvRows v) (csvSaved v))

sameGrid :: Grid -> Grid -> Bool
sameGrid a b =
  ptrEq a b
    || (Seq.length a == Seq.length b
        && and (zipWith sameRow (toList a) (toList b)))
  where
    sameRow r s =
      ptrEq r s
        || (Seq.length r == Seq.length s
            && and (zipWith sameCell (toList r) (toList s)))
    sameCell x y = ptrEq x y || x == y

-- | Mark the current grid as the saved state (called after writing the file).
markSaved :: CsvView -> CsvView
markSaved v = v { csvSaved = csvRows v }
