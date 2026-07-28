-- | Reading a spreadsheet: an @.xlsx@ workbook's sheets mapped onto the CSV
-- table view's grid.
--
-- The relationship to "Cmedit.Csv" is the one "Cmedit.Docx" has to
-- "Cmedit.Rtf": everything that makes a grid usable in a terminal — variable
-- row heights, a frozen header, column widths, rectangular selection, copy,
-- both scroll bars — already exists, so this module is a /mapping/ from
-- OOXML's spreadsheet parts to a grid of 'Text' and nothing more.
--
-- __Read-only, and that is a design decision rather than a missing feature.__
-- The CSV view /is/ the document while it is showing, so it serialises back
-- to the buffer. A workbook cannot: it carries formulas, formats, charts,
-- pivot tables, conditional formatting and defined names that this reader
-- does not model, and writing a grid back over them would destroy all of it.
-- So there is no serialiser, no buffer underneath, and the editing and sort
-- actions are pruned from the menus and guarded in @runAction@ — the exact
-- bargain "Cmedit.Rtf" strikes.
--
-- __Formulas: the file's answer first, ours only where it gave none.__ Excel
-- writes the result of every formula into the file beside it (@\<v\>@), so for
-- a workbook a spreadsheet program saved, what is on screen is that program's
-- own answer and is never recomputed. A workbook written by a /library/
-- (@openpyxl@, @xlsxwriter@, @pandas@) carries the formula and no value at
-- all, and those cells used to show blank; "Cmedit.Formula" fills them in,
-- and the status bar says how many it did and how many it could not.
-- 'sheetGrid' is where the distinction is drawn: only a formula with no
-- cached value goes into its formula map, so there is no path by which a
-- computed number can displace one the file supplied.
--
-- __One limit is stated in the UI rather than guessed at: number formats are
-- not applied.__ A cell holding a date is stored as a serial number, and its
-- @\<numFmt\>@ says how to print it; resolving that means the style chain and
-- a format-string interpreter. Until then the stored value is shown, and the
-- status bar says so. Honest and useful beats plausible and wrong.
module Cmedit.Xlsx
  ( -- * Detection
    workbookPath
  , isXlsx
    -- * The container
  , sheetRefs
  , relTargets
  , sharedStrings
  , sheetMemberPath
    -- * Sheets
  , sheetGrid
  , resolveFormulas
  , cellRef
  , cellName
  , maxSheetCells
  , maxSheetCols
    -- * The open workbook
  , Workbook(..)
  , mkWorkbook
  , wbCount
  , wbName
  , wbView
  , wbPut
  , wbGoTo
  , wbNext
  , wbPrev
  , wbStatus
  ) where

import qualified Data.ByteString as BS
import Data.Char (chr, isDigit, isUpper, ord, toUpper)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Cmedit.Width (lineDisplayWidth)
import Cmedit.Csv (CsvView)
import qualified Cmedit.Csv as Csv
import qualified Cmedit.Formula as F
import Cmedit.Xml (XmlEvent(..), elemText, parseXmlBytes, xAttr)

------------------------------------------------------------------------------
-- Detection

-- | The member every @.xlsx@ has, and the entry point to the rest of it.
workbookPath :: Text
workbookPath = "xl/workbook.xml"

-- | Does this archive's table of contents look like a workbook?
isXlsx :: [Text] -> Bool
isXlsx names = workbookPath `elem` names

------------------------------------------------------------------------------
-- Bounds
--
-- A workbook is a grid, so its cost is its cell count and not its byte count;
-- these bound what a generated file can make the editor materialise. Hitting
-- either truncates with a note, the way the paged view truncates a line.

-- | Most cells one sheet will yield.
maxSheetCells :: Int
maxSheetCells = 2000000

-- | Most columns one sheet will yield. Excel's own limit is 16384, so a
-- reference past it is a corrupt file rather than a wide sheet — and without
-- the cap one bad @r=\"XFD1048576\"@ would materialise a million blank cells
-- in every row.
maxSheetCols :: Int
maxSheetCols = 16384

------------------------------------------------------------------------------
-- The container

-- | The workbook's sheets in tab order, as @(name, relationship id)@.
--
-- The order here is the order Excel shows, which is why the sheets are read
-- from @xl\/workbook.xml@ rather than from the @xl\/worksheets\/@ member names:
-- those are numbered by creation, and a workbook whose tabs have been dragged
-- around has them in neither order.
sheetRefs :: BS.ByteString -> [(Text, Text)]
sheetRefs bs =
  [ (nm, maybe T.empty id (xAttr "id" as))
  | XStart "sheet" as <- parseXmlBytes bs
  , Just nm <- [xAttr "name" as] ]

-- | @xl\/_rels\/workbook.xml.rels@ as @(id, target)@ pairs. The target is the
-- sheet's member path relative to @xl\/@.
relTargets :: BS.ByteString -> [(Text, Text)]
relTargets bs =
  [ (i, t)
  | XStart "Relationship" as <- parseXmlBytes bs
  , Just i <- [xAttr "Id" as], Just t <- [xAttr "Target" as] ]

-- | The archive member holding sheet @k@ (0-based, in tab order).
--
-- The relationship table is authoritative and is tried first. Its absence is
-- not fatal: workbooks written by simpler producers routinely have none, and
-- the conventional @xl\/worksheets\/sheetN.xml@ naming is right for all of
-- them. A target may also be written absolute (@\/xl\/worksheets\/...@), which
-- is relative to the archive root rather than to @xl\/@.
sheetMemberPath :: [(Text, Text)] -> [(Text, Text)] -> Int -> Text
sheetMemberPath refs rels k = case lookup rid rels of
  Just t | not (T.null t) -> resolve t
  _                       -> T.pack ("xl/worksheets/sheet" ++ show (k + 1) ++ ".xml")
  where
    rid = case drop k refs of ((_, r) : _) -> r; [] -> T.empty
    resolve t
      | "/" `T.isPrefixOf` t = T.drop 1 t
      | otherwise            = "xl/" <> T.dropWhile (== '/') (dropDot t)
    dropDot t = maybe t id (T.stripPrefix "./" t)

-- | The shared-string table: every distinct string in the workbook, indexed by
-- the @t=\"s\"@ cells that use it.
--
-- Detached ('T.copy'), per the slice-pinning rule: each string is a slice of
-- one decoded member, and a table of them retained for the life of the open
-- workbook would otherwise pin the whole of @sharedStrings.xml@ — which in a
-- text-heavy workbook is the largest member in the archive.
sharedStrings :: BS.ByteString -> Seq Text
sharedStrings bs = go Seq.empty (parseXmlBytes bs)
  where
    go !acc (XStart "si" _ : es) =
      -- Forced, not just written: a lazy 'T.copy' is a thunk that /retains/
      -- the slice it was meant to release, which is the exact opposite of
      -- what it is here for.
      let (t, rest) = elemText es
          !s' = T.copy t
      in go (acc |> s') rest
    go !acc (_ : es) = go acc es
    go !acc []       = acc

------------------------------------------------------------------------------
-- Sheets

-- | Parse one worksheet into a rectangular grid, the formulas that came with
-- __no cached value__, and whether a bound cut it short.
--
-- Gaps are real and are materialised: @\<c r=\"C1\"\/\>@ after @\<c
-- r=\"A1\"\/\>@ means column B is empty, and a row may be missing altogether.
-- A spreadsheet's shape is part of its meaning, so the empty cells have to
-- exist rather than be closed up — which is what the @r@ reference on each
-- cell and each row is for, and why they are read rather than assumed
-- sequential.
--
-- The formula map is deliberately /only/ the cells the file did not answer
-- for itself. A formula that came with its value is not in it and is never
-- recomputed — see "Cmedit.Formula" for why that is the whole design.
sheetGrid :: Seq Text -> BS.ByteString -> (Seq (Seq Text), Map (Int, Int) Text, Bool)
sheetGrid strs bs = finish (foldl' step (ss0 strs) (parseXmlBytes bs))
  where
    finish st =
      let st' = endRow st
          w   = ssWidth st'
      in ( fmap (padTo w) (ssOut st')
         , ssForms st'
         , ssCut st' )
    padTo w r = r <> Seq.replicate (max 0 (w - Seq.length r)) T.empty

    step st ev
      | ssCut st  = st
      | otherwise = case ev of
          XStart "row" as -> let st' = endRow st
                             in st' { ssRowAt = rowIndex as (ssNext st') }
          XEnd   "row"    -> endRow st
          XStart "c" as   ->
            st { ssColAt = colIndex as (ssCol st)
               , ssType  = maybe "n" id (xAttr "t" as)
               , ssVal   = [], ssCap = False
               , ssFSrc  = [], ssHasF = False, ssCapF = False }
          XEnd   "c"      -> pushCell st
          -- Only <v> (a value) and <t> (inline-string text) are cell content.
          -- <f> is the formula source and must never be shown in its place —
          -- but it is kept, because a cell with a formula and no value is the
          -- one case "Cmedit.Formula" exists for.
          XStart "v" _    -> st { ssCap = True }
          XEnd   "v"      -> st { ssCap = False }
          XStart "t" _    -> st { ssCap = True }
          XEnd   "t"      -> st { ssCap = False }
          XStart "f" _    -> st { ssCapF = True, ssHasF = True }
          XEnd   "f"      -> st { ssCapF = False }
          XText t | ssCapF st -> st { ssFSrc = t : ssFSrc st }
                  | ssCap st  -> st { ssVal = t : ssVal st }
          _               -> st

-- Sparse-aware accumulator. Rows and columns are placed by their recorded
-- reference, so this is a fill rather than an append; @ssNext@/@ssCol@ carry
-- the sequential fallback for producers that omit the references.
data SS = SS
  { ssOut   :: !(Seq (Seq Text))
  , ssRow   :: !(Seq Text)   -- ^ Cells of the row in progress, already gap-filled.
  , ssRowAt :: !Int          -- ^ Its 0-based index.
  , ssNext  :: !Int          -- ^ Index the next unreferenced row would take.
  , ssCol   :: !Int          -- ^ Index the next unreferenced cell would take.
  , ssColAt :: !Int          -- ^ Column the cell in progress belongs to.
  , ssType  :: !Text         -- ^ Its @t@ attribute.
  , ssVal   :: ![Text]       -- ^ Its captured text, reversed.
  , ssCap   :: !Bool         -- ^ Capturing character data.
  , ssWidth :: !Int          -- ^ Widest row seen.
  , ssCells :: !Int          -- ^ Cells materialised so far.
  , ssCut   :: !Bool
  , ssOpen  :: !Bool         -- ^ A row is in progress.
  , ssStrs  :: !(Seq Text)   -- ^ The workbook's shared strings, for @t="s"@ cells.
  , ssFSrc  :: ![Text]       -- ^ Formula source of the cell in progress, reversed.
  , ssHasF  :: !Bool         -- ^ It had an @\<f\>@ at all (a shared formula's followers have an empty one).
  , ssCapF  :: !Bool         -- ^ Capturing that formula's text.
  , ssForms :: !(Map (Int, Int) Text)  -- ^ Formulas with no cached value, by @(row, column)@.
  }

ss0 :: Seq Text -> SS
ss0 strs = SS Seq.empty Seq.empty 0 0 0 0 "n" [] False 0 0 False False strs
              [] False False M.empty

rowIndex :: [(Text, Text)] -> Int -> Int
rowIndex as dflt = case xAttr "r" as >>= readNat of
  Just n | n >= 1 -> n - 1
  _               -> dflt

colIndex :: [(Text, Text)] -> Int -> Int
colIndex as dflt = case xAttr "r" as >>= cellRef of
  Just (c, _) -> c
  Nothing     -> dflt

-- | An A1-style reference to 0-based @(column, row)@. @\"AB12\"@ is
-- @(27, 11)@. Case-insensitive, because a hand-written sheet may not shout.
cellRef :: Text -> Maybe (Int, Int)
cellRef t
  | T.null letters || T.null digits           = Nothing
  | T.length letters > 3                      = Nothing   -- past column XFD
  | otherwise = case readNat digits of
      Just r | r >= 1 -> Just (col - 1, r - 1)
      _               -> Nothing
  where
    up      = T.map toUpper t
    letters = T.takeWhile isUpper up
    digits  = T.takeWhile isDigit (T.drop (T.length letters) up)
    col     = T.foldl' (\ !a c -> a * 26 + (ord c - ord 'A' + 1)) 0 letters

-- | The inverse of 'cellRef': 0-based @(row, column)@ to an A1-style reference.
-- @(11, 27)@ is @\"AB12\"@.
--
-- Bijective base-26, which is the fiddly part — there is no zero digit, so
-- column 26 is @Z@ and column 27 is @AA@ rather than @BA@.
cellName :: Int -> Int -> Text
cellName r c = T.pack (letters (max 0 c) "" ++ show (max 0 r + 1))
  where
    letters n acc =
      let (q, m) = (n `div` 26, n `mod` 26)
          acc'   = chr (ord 'A' + m) : acc
      in if q == 0 then acc' else letters (q - 1) acc'

readNat :: Text -> Maybe Int
readNat t
  | T.null ds || T.length ds > 8 = Nothing
  | otherwise                    = Just (T.foldl' (\ !a c -> a * 10 + (ord c - ord '0')) 0 ds)
  where ds = T.takeWhile isDigit t

-- Place the cell in progress at its column, filling any gap before it.
pushCell :: SS -> SS
pushCell st
  | ssColAt st >= maxSheetCols        = st
  | ssCells st >= maxSheetCells       = st { ssCut = True }
  | otherwise =
      let c    = ssColAt st
          gap  = max 0 (c - Seq.length (ssRow st))
          -- Forced before it is stored. 'Seq' is spine-strict but
          -- element-lazy, so an unforced cell is a thunk over the whole
          -- parser state — including the grid built so far and the shared
          -- string table — held once per cell of the sheet.
          !v   = value st
          row' = ssRow st <> Seq.replicate gap T.empty |> v
          -- A formula with no value is the one thing worth evaluating; a
          -- formula *with* one is already answered, by the program that owns
          -- the format. Recording the empty source of a shared formula's
          -- follower is deliberate too: it cannot be computed, and counting it
          -- is how the status bar stays honest about that.
          forms | ssHasF st, null (ssVal st) =
                    M.insert (ssRowAt st, c) (T.concat (reverse (ssFSrc st))) (ssForms st)
                | otherwise = ssForms st
      in st { ssRow = row', ssCol = c + 1, ssOpen = True
            , ssCells = ssCells st + gap + 1
            , ssForms = forms
            , ssVal = [], ssType = "n", ssFSrc = [], ssHasF = False }

-- A cell's displayed text, from its type. Everything unrecognised falls
-- through to the raw stored value, which is the honest answer for a type this
-- reader has not been taught.
value :: SS -> Text
value st = case ssType st of
  "s"         -> case readNat raw of
                   Just i  -> maybe raw id (Seq.lookup i strs)
                   Nothing -> raw
  "b"         -> if raw == "1" then "TRUE" else "FALSE"
  _           -> raw
  where
    -- Detached: without it every cell of the sheet is a slice of the decoded
    -- worksheet member, and the grid pins it for as long as the file is open.
    raw  = T.copy (T.concat (reverse (ssVal st)))
    strs = ssStrs st

endRow :: SS -> SS
endRow st
  | not (ssOpen st) = st { ssNext = max (ssNext st) (ssRowAt st) }
  | otherwise =
      let r    = ssRowAt st
          gap  = max 0 (r - Seq.length (ssOut st))
          out' = ssOut st <> Seq.replicate gap Seq.empty |> ssRow st
      in st { ssOut = out', ssRow = Seq.empty, ssCol = 0, ssOpen = False
            , ssNext = r + 1, ssRowAt = r + 1
            , ssWidth = max (ssWidth st) (Seq.length (ssRow st))
            , ssCells = ssCells st + gap
            , ssCut = ssCut st || ssCells st >= maxSheetCells }

------------------------------------------------------------------------------
-- The open workbook
--
-- A workbook is several grids, and the table view holds one. So the open
-- workbook keeps the sheets and which of them is showing, and the editor
-- keeps the showing one in 'Cmedit.EditorState.edCsv' — where the renderer,
-- the key handler, the mouse hit-tests and both scroll bars already know how
-- to find it. That is the whole of multi-sheet support: everything else is
-- the CSV view, unchanged.
--
-- The consequence to remember is that @'wbSheets' `Seq.index` 'wbIdx'@ goes
-- /stale/ while its sheet is showing, because the live copy is the editor's.
-- 'wbPut' writes it back, and every sheet change goes through 'wbGoTo', which
-- calls it — so nothing else needs to know.

-- | An open @.xlsx@: its sheets, in tab order, and which one is showing.
data Workbook = Workbook
  { wbNames  :: !(Seq Text)
  , wbIdx    :: !Int              -- ^ 0-based index of the sheet on screen.
  , wbSheets :: !(Seq CsvView)    -- ^ One per name; the entry at 'wbIdx' is stale while showing.
  , wbNote   :: !Text             -- ^ Limits worth stating (truncation, unreadable sheets).
  } deriving (Show)

mkWorkbook :: [(Text, Seq (Seq Text))] -> Text -> Workbook
mkWorkbook sheets note = Workbook
  { wbNames  = Seq.fromList (map fst sheets)
  , wbIdx    = 0
    -- The delimiter is what a copied selection is joined with; a workbook has
    -- no delimiter of its own, and comma is what a spreadsheet's clipboard
    -- pastes as everywhere else.
  , wbSheets = Seq.fromList [ Csv.mkCsvGrid ',' g | (_, g) <- sheets ]
  , wbNote   = note
  }

wbCount :: Workbook -> Int
wbCount = Seq.length . wbNames

-- | The 1-based sheet number and name, for the status bar.
wbName :: Workbook -> Text
wbName wb = maybe T.empty id (Seq.lookup (wbIdx wb) (wbNames wb))

-- | The view for the showing sheet (as last stored — see the note above).
wbView :: Workbook -> Maybe Csv.CsvView
wbView wb = Seq.lookup (wbIdx wb) (wbSheets wb)

-- | Store the live view back into the showing sheet's slot.
wbPut :: Csv.CsvView -> Workbook -> Workbook
wbPut v wb
  | wbIdx wb >= 0 && wbIdx wb < Seq.length (wbSheets wb) =
      -- 'adjust'' rather than 'update': the latter stores @const v@ applied to
      -- the sheet it replaced, so every switch back and forth would chain
      -- another thunk onto the one before it.
      wb { wbSheets = Seq.adjust' (const v) (wbIdx wb) (wbSheets wb) }
  | otherwise = wb

-- | Show sheet @k@ (0-based, clamped), saving the live view of the one being
-- left so returning to it finds the cursor and scroll where they were.
wbGoTo :: Csv.CsvView -> Int -> Workbook -> (Workbook, Csv.CsvView)
wbGoTo live k wb =
  let wb'  = wbPut live wb
      k'   = max 0 (min (wbCount wb - 1) k)
      wb'' = wb' { wbIdx = k' }
  in (wb'', maybe live id (wbView wb''))

wbNext :: Csv.CsvView -> Workbook -> (Workbook, Csv.CsvView)
wbNext live wb = wbGoTo live (wbIdx wb + 1) wb

wbPrev :: Csv.CsvView -> Workbook -> (Workbook, Csv.CsvView)
wbPrev live wb = wbGoTo live (wbIdx wb - 1) wb

-- | The status-bar text: which sheet, of how many, and the limits this view
-- is honest about rather than guessing at.
wbStatus :: Workbook -> String
wbStatus wb =
  "Sheet " ++ show (wbIdx wb + 1) ++ "/" ++ show (wbCount wb)
    ++ (let n = narrowOnly (T.take 20 (wbName wb))
        in if T.null n then "" else " " ++ T.unpack n)
    ++ "   XLSX "

-- | A name only if every character of it occupies one terminal cell.
--
-- The status bar's right-hand block is measured in /characters/ by
-- 'Cmedit.EditorEdit.statusRightInfo' and its click zones — a documented
-- 1-char-is-1-cell assumption that holds because everything there is ASCII.
-- A sheet may be called anything, so a name with a wide glyph in it would
-- shift every zone after it and misplace the clicks. The number is the part
-- that matters; the full name is in the open message and the Window menu.
narrowOnly :: Text -> Text
narrowOnly t | lineDisplayWidth 8 t == T.length t = t
             | otherwise                          = T.empty

------------------------------------------------------------------------------
-- Formulas the file did not answer for itself

-- | Fill in the formula cells that came with no cached value, across the whole
-- workbook (so a @=Summary!B2@ resolves), and report how many were computed
-- and how many this reader could not.
--
-- Whole-workbook rather than per-sheet purely so cross-sheet references work;
-- everything else about it is "Cmedit.Formula"'s, including the rule that
-- matters most — a value the file already gave is never touched.
resolveFormulas :: [(Text, Seq (Seq Text), Map (Int, Int) Text)]
                -> ([(Text, Seq (Seq Text))], Int, Int)
resolveFormulas sheets
  | all (\(_, _, fs) -> M.null fs) sheets = ([ (n, g) | (n, g, _) <- sheets ], 0, 0)
  | otherwise = F.evalWorkbook [ F.SheetIn n g fs | (n, g, fs) <- sheets ]
