-- | Document lifecycle: loading, the open-files zipper, view modes
-- (CSV table / image), recents, navigation history, quick open and
-- the save/close/quit flows.
module Cmedit.EditorDoc where


import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)
import Data.Foldable (toList)
import Data.List (findIndex, intercalate, isPrefixOf, isSuffixOf, sortBy, sortOn)
import Data.Ord (Down(..), comparing)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName)

import Data.Array (Array)
import Data.Word (Word8)

import Cmedit.Types
import Cmedit.TextBuffer
import Cmedit.Width (colToDisplay, displayToCol, wrapLine)
import Cmedit.ConfigFile
  ( Config(..), ThemeName(..), PdfViewMode(..), PdfFit(..), PdfDrag(..), defaultConfig, RecentEntry(..)
  , maxRecentEntries, maxHistoryEntries, Session(..), SessionFile(..)
  , SessionSummary(..), maxSessionFiles )
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
import Cmedit.Manual (manualPath, manualText)
import Cmedit.Clipboard (CopyOutcome(..))
import Cmedit.Image (Image(..), ImgMode(..), renderImage, renderImagePlaced, viewFit, reduceImage)
import Cmedit.Geom (Rect(..), rectW, rectH)
import Cmedit.PdfStrip (StripGeom(..), Piece(..), TileKey(..), TileBits(..), HlRect(..))
import qualified Cmedit.PdfStrip as St
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Cmedit.Scene (Scene(..))
import Cmedit.Svg (sniffSvgText)
import Cmedit.Raster (RenderStats(..))
import Cmedit.VecView
import Cmedit.Syntax (HlCache, CommentSyntax(..), langComment, langForPath)

import Cmedit.History (pushHist)
import Cmedit.Journal (Journal(..), RecoveryCase(..))
import qualified Cmedit.Journal as J
import Cmedit.Pager (PagerDoc(..))
import Cmedit.Rtf (RtfDoc(..))
import Cmedit.Pdf (PdfDoc(..))
import qualified Cmedit.Pdf as Pdf
import qualified Cmedit.PdfObj as PO
import qualified Cmedit.Rtf as Rtf
import Cmedit.Xlsx (Workbook(..))
import qualified Cmedit.Xlsx as Xlsx
import qualified Cmedit.Pager as Pg
import Cmedit.EditorState
import Cmedit.EditorEdit


-- Re-clamp scrolling / re-scale an image / re-wrap the formatted RTF and PDF
-- views after a layout change (e.g. the panel width or collapse state changed,
-- which shifts the text area).
relayout :: Editor -> Editor
relayout = refreshPdf . refreshRtf . refreshImage . ensureVisible

------------------------------------------------------------------------------
-- Files / lifecycle

-- | Install a freshly-loaded file into the editor. CSV/TSV files default to
-- the table view.
setLoaded :: FilePath -> LoadResult -> Editor -> Editor
setLoaded path lr ed =
  let ed1 = ensureVisible ed
        { edBuffer = lrBuffer lr
        , edSavedBuffer = lrBuffer lr
        , edCursor = origin
        , edSelAnchor = Nothing
        , edDesiredCol = 0
        , edTop = 0, edLeft = 0
        , edPath = Just path
        , edModified = False
        , edDiskMtime = lrMtime lr
        , edDiskChanged = False
        , edLineEnding = lrLineEnding lr
        , edSavedEol = lrLineEnding lr
        , edEncoding = lrEncoding lr
        , edSavedEnc = lrEncoding lr
        , edFinalNewline = lrFinalNewline lr
        , edReadOnly = lrReadOnly lr
        , edUndo = Seq.empty, edRedo = Seq.empty, edLastEdit = EKNone
        , edStatus = T.pack ("Opened " ++ path
                      ++ (if lrReadOnly lr then " [read-only]" else ""))
        , edFocus = FEdit, edDialog = Nothing, edSearchMode = False
        , edDefPick = Nothing, edQuickOpen = Nothing, edComplete = Nothing
        , edCsv = Nothing, edCsvStash = Nothing
        , edImage = Nothing, edSvgStash = Nothing, edPdfView = Nothing, edRtf = Nothing, edSheets = Nothing, edPdf = Nothing
        , edDiags = []                        -- a reloaded file's stale diags must not survive
        , edEditSeq = edEditSeq ed + 1         -- fresh buffer: the driver must re-lint (also covers Revert)
        }
      -- CSV/TSV open in the table view and RTF in the formatted view: for both
      -- the markup is the thing you least often want to look at. Alt+T shows
      -- the text underneath.
      --
      -- Except when we are opening this file *to land on a position in it* —
      -- a workspace search result, a definition site. Those matched the
      -- markup, which the formatted view does not show and has no cursor to
      -- put anywhere, so the raw view is the only one where the jump means
      -- anything. (CSV maps text positions back to cells, so it needs no such
      -- exception.)
      jumpingHere = case edPendingJump ed of
                      Just (p, _, _, _) -> p == path
                      Nothing           -> False
      ed2 | isCsvPath path                  = enterCsv ed1
          | isRtfPath path && jumpingHere   = restoreRecentPos path ed1
          | isRtfPath path                  = enterRtf ed1
          -- An SVG opens as its picture (svg-view = picture), except when we
          -- are landing on a position in its markup, as for RTF above.
          | isSvgPath path && not jumpingHere && cfgSvgPicture (edConfig ed)
                                            = enterSvg (restoreRecentPos path ed1)
          | otherwise                       = restoreRecentPos path ed1
  in touchRecent path ed2

-- Parse the current buffer into a fresh CSV table view (used when first opening
-- a file; toggles go through 'plainToCsv', which preserves the undo history).
enterCsv :: Editor -> Editor
enterCsv ed =
  ed { edCsv = Just (Csv.mkCsvLines (csvDelimOf ed) (bufLines (edBuffer ed)))
     , edCsvStash = Nothing }

-- Serialise the table back into the line buffer (so plaintext / save see it).
--
-- A workbook's grid is exempt, and this is the one place that has to know it:
-- an @.xlsx@ has no buffer under it and no serialiser back to its own format,
-- so turning a sheet into CSV here and letting a save path write it would
-- replace someone's workbook with one sheet of it as text. Every route to
-- that is already guarded ('containerDisabledActions'), and this is the
-- backstop under all of them.
syncCsvToBuffer :: Editor -> Editor
syncCsvToBuffer ed
  | isJust (edSheets ed) = ed
  | otherwise = case edCsv ed of
      Nothing -> ed
      Just v  -> ed { edBuffer = fromText (Csv.csvToText v) }

-- | Toggle between the CSV table view and plain text (only for CSV files).
-- The cursor is carried across: leaving the table drops the text cursor at the
-- start of the current cell; entering it selects the cell the cursor was in.
toggleCsv :: Editor -> Editor
toggleCsv ed
  | not (isCsvFile ed) = ed { edStatus = "Table view is only for .csv / .tsv files" }
  | otherwise = case edCsv ed of
      Just v  -> csvToPlain v ed
      Nothing -> plainToCsv ed

csvToPlain :: CsvView -> Editor -> Editor
csvToPlain v0 ed =
  let v         = Csv.commitEdit v0          -- flush any in-cell edit (records its undo step)
      ed1       = ed { edBuffer = fromText (Csv.csvToText v), edCsv = Nothing
                     , edCsvStash = Just v, edHlCache = Nothing
                     , edStatus = "Plain-text mode" }
      (ln, col) = Csv.cellTextPos v (Csv.csvCurRow v) (Csv.csvCurCol v)
      pos       = clampPos (Pos ln col) (edBuffer ed1)
      line      = getLine' (posLine pos) (edBuffer ed1)
  in ensureVisible ed1
       { edCursor = pos, edSelAnchor = Nothing
       , edDesiredCol = colToDisplay (tabWidthOf ed1) (posCol pos) line }

plainToCsv :: Editor -> Editor
plainToCsv ed =
  let Pos l c  = edCursor ed
      -- Only the stash comparison needs the buffer as one 'Text'; the parse
      -- reads the lines directly, so a toggle with nothing stashed never
      -- materialises a second copy of the file.
      bufText  = bufferToText LF False (edBuffer ed)
      base     = Csv.mkCsvLines (csvDelimOf ed) (bufLines (edBuffer ed))
      -- With nothing stashed the buffer is the only source for the table's
      -- saved baseline, and 'mkCsvLines' adopts whatever it is handed — which
      -- is right for a document that is clean and wrong for one that is dirty
      -- for a reason the table cannot see. A staged workspace replace is
      -- exactly that: 'stagedDoc' opens a .csv with no table and no stash and a
      -- buffer that differs from disk, so parsing it into a fresh baseline made
      -- the next 'csvMod' declare the document clean and drop the staged change
      -- (and its journal) silently. 'markUnsaved' is the same guard the
      -- crash-recovery installer uses, and for the same reason.
      fresh | bufModified ed (edBuffer ed) || metaModified ed = Csv.markUnsaved base
            | otherwise                                       = base
      -- Reuse the stashed table model (and its undo) if the text was not edited
      -- while in plain-text view; otherwise keep the history but rebase onto the
      -- newly-parsed grid so an undo still reverts the text edit.
      v        = case edCsvStash ed of
                   Just s | Csv.csvToText s == bufText -> s
                          | otherwise                  -> Csv.rebaseHistory s base
                   Nothing -> fresh
      (r, cc)  = Csv.textPosCell v l c
  in (csvPut (Csv.setCursor r cc v) ed { edCsvStash = Nothing }) { edStatus = "Table mode" }

------------------------------------------------------------------------------
-- RTF formatted view (read-only). Structurally the CSV toggle's simpler
-- sibling: same per-document field, same Alt+T, but the traffic is one-way.
--
-- The CSV view *is* the document while it is showing, so it serialises back
-- into the buffer ('syncCsvToBuffer') on save and on toggling out. The RTF
-- view must not: it models bold, colour, alignment and indentation, and a
-- real document also carries style sheets, tables, embedded pictures and
-- revision marks that it does not. Writing this model back would silently
-- drop all of that, so nothing ever writes it back — the buffer stays the
-- document, Save writes the buffer, and leaving the view simply discards the
-- projection. Editing an RTF file means editing its markup (Alt+T).

-- | Build the formatted view from the current buffer.
enterRtf :: Editor -> Editor
enterRtf ed =
  let rd = Rtf.mkRtfDoc (edEditSeq ed) (bufLines (edBuffer ed))
      -- Say so rather than showing a document that quietly stops early.
      warn | bufChars (edBuffer ed) > Rtf.maxRtfChars =
               "Formatted view \x2014 only the first "
                 ++ show (Rtf.maxRtfChars `div` (1024 * 1024)) ++ " MB is shown"
           | Rtf.looksLikeRtf (bufferToText LF False (edBuffer ed)) = "Formatted view"
           | otherwise = "Formatted view \x2014 this file does not begin like RTF"
  in refreshRtf ed { edRtf = Just rd, edStatus = T.pack warn }

-- | Toggle between the formatted view and the raw RTF markup (Alt+T, the same
-- key the CSV table view uses — a file is one or the other).
toggleRtf :: Editor -> Editor
toggleRtf ed
  | not (isRtfFile ed) = ed { edStatus = "Formatted view is only for .rtf files" }
  | isJust (edRtf ed) =
      ensureVisible ed { edRtf = Nothing, edSheets = Nothing, edStatus = "Raw RTF markup \x2014 editable" }
  | otherwise = enterRtf ed

-- | Re-derive and re-lay-out the formatted view when anything it is computed
-- from has moved: the buffer (a workspace replace, a revert), or the width it
-- was wrapped to (a resize, the explorer panel opening). Called from the same
-- places as 'refreshImage', whose cache this mirrors — a comparison per
-- keystroke, real work only when the key changes.
refreshRtf :: Editor -> Editor
refreshRtf ed = case edRtf ed of
  Nothing -> ed
  Just rd ->
    let lo   = computeLayout ed
        rd1  = if Rtf.rtfStale (edEditSeq ed) rd
                 then (Rtf.mkRtfDoc (edEditSeq ed) (bufLines (edBuffer ed)))
                        { rdTop = rdTop rd }
                 else rd
    in ed { edRtf = Just (Rtf.rtfRelayout (tabWidthOf ed) (loTextWidth lo)
                            (loTextHeight lo) rd1) }

------------------------------------------------------------------------------
-- PDF reading view (read-only). Structured like the image view, not like the
-- RTF one: a PDF is binary, so there is no buffer under it to toggle to and
-- nothing that Save could write. The extracted model arrives from the driver
-- already parsed (it is the expensive part) and is laid out here, against the
-- window width, the first time it is shown.

-- | The shape every buffer-less read-only document has in common: an empty
-- buffer, no undo, no diagnostics, every view mode cleared and the file marked
-- read-only. The caller sets its own view field and status on top.
--
-- Four things now arrive this way — a PDF, a DOCX or EPUB reading view and an
-- XLSX workbook — and the danger of writing the record out four times is not
-- duplication but /drift/: a field added to 'Editor' and set in three of the
-- four leaves one view mode able to survive into a document that is not it.
blankReadOnly :: FilePath -> String -> Editor -> Editor
blankReadOnly path status ed = ed
  { edBuffer = emptyBuffer, edSavedBuffer = emptyBuffer
  , edCursor = origin, edSelAnchor = Nothing, edDesiredCol = 0
  , edTop = 0, edLeft = 0
  , edPath = Just path, edModified = False
  , edDiskMtime = Nothing, edDiskChanged = False
  , edLineEnding = LF, edSavedEol = LF, edEncoding = Utf8, edSavedEnc = Utf8
  , edFinalNewline = True
  , edReadOnly = True
  , edUndo = Seq.empty, edRedo = Seq.empty, edLastEdit = EKNone
  , edStatus = T.pack status
    -- Same rule as the image and paged views: an open from the explorer panel
    -- keeps its focus, because a read-only view has no keystroke editing to
    -- receive it.
  , edFocus = if edFocus ed == FExplorer then FExplorer else FEdit
  , edDialog = Nothing, edSearchMode = False
  , edDefPick = Nothing, edQuickOpen = Nothing, edComplete = Nothing
  , edCsv = Nothing, edCsvStash = Nothing, edImage = Nothing, edSvgStash = Nothing, edPdfView = Nothing, edRtf = Nothing
  , edSheets = Nothing, edPager = Nothing, edPdf = Nothing
  , edHlCache = Nothing, edDiags = []
  }

-- | The same, as a zipper snapshot (for a file opened but not made active).
blankReadOnlyDoc :: FilePath -> Document
blankReadOnlyDoc path = Document
  { docBuffer = emptyBuffer, docSavedBuffer = emptyBuffer, docCursor = origin
  , docSelAnchor = Nothing, docDesiredCol = 0, docTop = 0, docLeft = 0
  , docPath = Just path, docModified = False
  , docDiskMtime = Nothing, docDiskChanged = False
  , docLineEnding = LF, docSavedEol = LF, docEncoding = Utf8, docSavedEnc = Utf8
  , docFinalNewline = True, docReadOnly = True
  , docUndo = Seq.empty, docRedo = Seq.empty, docLastEdit = EKNone, docOverwrite = False
  , docDiscard = False, docCsv = Nothing, docCsvStash = Nothing
  , docImage = Nothing, docSvgStash = Nothing, docPdfView = Nothing, docRtf = Nothing, docSheets = Nothing, docPager = Nothing
  , docPdf = Nothing
  , docHlCache = Nothing
  , docDiags = []
  -- Read-only, so never journalled; and titled, so the id is unused anyway.
  , docDocId = 0, docDocSeq = 0, docJournalSeq = 0
  }

-- | Install a parsed PDF as the active document.
pdfLoaded :: FilePath -> PdfDoc -> Editor -> Editor
pdfLoaded path pd ed = touchRecent path $ refreshImage $ refreshPdf $ ensureVisible $
  (blankReadOnly path status ed1)
    { edPdf = Just pd, edPdfView = Just (newPdfView epoch (edConfig ed) (edGfxCaps ed) pd) }
  where
    status = "Viewing " ++ takeFileName path ++ "  "
               ++ show (Pdf.pdfPageCount pd) ++ " page" ++ plural (Pdf.pdfPageCount pd)
               ++ (if T.null (pdNote pd) then "" else "  \x2014 " ++ T.unpack (pdNote pd))
               ++ "  \x2014 read-only"
    (epoch, ed1) = tickVecClock ed

-- | Open a parsed PDF as a new document (reusing a pristine empty buffer if
-- present), mirroring 'setLoadedNew' for text.
pdfLoadedNew :: FilePath -> PdfDoc -> Editor -> Editor
pdfLoadedNew path pd ed = case switchToOpen path ed of
  Just ed'
    | edFocus ed == FExplorer -> ed' { edFocus = FExplorer }
    | otherwise               -> ed'
  Nothing
    | isPristine ed -> pdfLoaded path pd ed
    | otherwise     -> pdfLoaded path pd (pushActive ed)

-- | Append a PDF to the open-files list (startup, 2nd+ file).
addPdfDocument :: FilePath -> PdfDoc -> Editor -> Editor
addPdfDocument path pd ed =
  let (epoch, ed1) = tickVecClock ed
      d = (pdfDocSnapshot path pd) { docPdfView = Just (newPdfView epoch (edConfig ed) (edGfxCaps ed) pd) }
  in touchRecent path ed1 { edAfter = edAfter ed1 ++ [d] }

pdfDocSnapshot :: FilePath -> PdfDoc -> Document
pdfDocSnapshot path pd = (blankReadOnlyDoc path) { docPdf = Just pd }

-- | Re-lay-out the PDF view when the width it was laid out for has moved (a
-- resize, the explorer panel opening). Mirrors 'refreshRtf' and
-- 'refreshImage': a comparison per keystroke, real work only when the cache
-- key changes. There is no staleness check to make — nothing can edit a PDF
-- document, so the extracted pages never move under the layout.
refreshPdf :: Editor -> Editor
refreshPdf ed = case edPdf ed of
  Nothing -> ed
  Just pd ->
    let lo = computeLayout ed
    in stripSync ed { edPdf = Just (Pdf.pdfRelayout (tabWidthOf ed) (loTextWidth lo)
                            (loTextHeight lo) pd) }

------------------------------------------------------------------------------
-- Container reading views (DOCX, EPUB, XLSX)
--
-- These are the PDF view's shape, not the RTF one's, and for the PDF view's
-- reason: the file is a ZIP full of XML, so it is binary, so there is no
-- buffer under the view and nothing Save could write. What they do /not/ need
-- is new machinery — a DOCX or EPUB is 'edRtf' with a container origin
-- ("Cmedit.Rtf"), an XLSX is 'edCsv' with 'edSheets' beside it, and the
-- renderer, key handlers, scroll bars and layout all carry on unchanged.

-- | Install a container-derived formatted document (DOCX, EPUB) as the active
-- document.
containerDocLoaded :: FilePath -> RtfDoc -> Editor -> Editor
containerDocLoaded path rd ed = touchRecent path $ refreshRtf $ ensureVisible $
  (blankReadOnly path status ed) { edRtf = Just rd }
  where
    status = "Viewing " ++ takeFileName path
               ++ (if Seq.null (rdSects rd) then ""
                     else "  " ++ show (Rtf.rtfSectionCount rd) ++ " chapter"
                            ++ plural (Rtf.rtfSectionCount rd))
               ++ (if T.null (rdNote rd) then "" else "  \x2014 " ++ T.unpack (rdNote rd))
               ++ "  \x2014 read-only"

-- | Open one as a new document (reusing a pristine empty buffer), or switch to
-- it if it is already open. Like the PDF and paged views this needs no second
-- installer for the already-open case: the view is read-only, so switching to
-- the open copy is the only sane result.
containerDocLoadedNew :: FilePath -> RtfDoc -> Editor -> Editor
containerDocLoadedNew path rd ed = case switchToOpen path ed of
  Just ed'
    | edFocus ed == FExplorer -> ed' { edFocus = FExplorer }
    | otherwise               -> ed'
  Nothing
    | isPristine ed -> containerDocLoaded path rd ed
    | otherwise     -> containerDocLoaded path rd (pushActive ed)

-- | Append one to the open-files list (startup, 2nd+ file).
addContainerDoc :: FilePath -> RtfDoc -> Editor -> Editor
addContainerDoc path rd ed =
  touchRecent path ed { edAfter = edAfter ed ++ [(blankReadOnlyDoc path) { docRtf = Just rd }] }

-- | Install an open workbook as the active document: the showing sheet becomes
-- the table view, the rest wait in 'edSheets'.
workbookLoaded :: FilePath -> Workbook -> Editor -> Editor
workbookLoaded path wb ed = touchRecent path $ ensureVisible $
  (blankReadOnly path status ed)
    { edSheets = Just wb, edCsv = Xlsx.wbView wb }
  where
    status = "Viewing " ++ takeFileName path ++ "  "
               ++ show (Xlsx.wbCount wb) ++ " sheet" ++ plural (Xlsx.wbCount wb)
               ++ (if T.null (wbNote wb) then "" else "  \x2014 " ++ T.unpack (wbNote wb))
               ++ "  \x2014 read-only"

workbookLoadedNew :: FilePath -> Workbook -> Editor -> Editor
workbookLoadedNew path wb ed = case switchToOpen path ed of
  Just ed'
    | edFocus ed == FExplorer -> ed' { edFocus = FExplorer }
    | otherwise               -> ed'
  Nothing
    | isPristine ed -> workbookLoaded path wb ed
    | otherwise     -> workbookLoaded path wb (pushActive ed)

addWorkbookDocument :: FilePath -> Workbook -> Editor -> Editor
addWorkbookDocument path wb ed =
  touchRecent path ed
    { edAfter = edAfter ed
        ++ [(blankReadOnlyDoc path) { docSheets = Just wb, docCsv = Xlsx.wbView wb }] }

-- | Show a different sheet of the open workbook (0-based, clamped), keeping
-- the one being left exactly where it was.
goToSheetIn :: Int -> Editor -> Editor
goToSheetIn k ed = case (edSheets ed, edCsv ed) of
  (Just wb, Just v) ->
    let (wb', v') = Xlsx.wbGoTo v k wb
    in ed { edSheets = Just wb', edCsv = Just v', edStatus = "" }
  _ -> ed

-- | Jump the reading view to a page (the Go To command in this mode).
pdfGoToPageIn :: Int -> Editor -> Editor
pdfGoToPageIn n ed
  | pageViewOn ed = (pdfTurnTo n ed) { edStatus = "" }
pdfGoToPageIn n ed = case edPdf ed of
  Nothing -> ed
  Just pd -> ed { edPdf = Just (Pdf.pdfGoToPage (pdfHeight ed) n pd), edStatus = "" }

-- Capture the active document fields into a saveable 'Document'.
captureDoc :: Editor -> Document
captureDoc ed = Document
  { docBuffer = edBuffer ed, docSavedBuffer = edSavedBuffer ed, docCursor = edCursor ed, docSelAnchor = edSelAnchor ed
  , docDesiredCol = edDesiredCol ed, docTop = edTop ed, docLeft = edLeft ed
  , docPath = edPath ed, docModified = edModified ed
  , docDiskMtime = edDiskMtime ed, docDiskChanged = edDiskChanged ed
  , docLineEnding = edLineEnding ed, docSavedEol = edSavedEol ed
  , docEncoding = edEncoding ed, docSavedEnc = edSavedEnc ed
  , docFinalNewline = edFinalNewline ed, docReadOnly = edReadOnly ed
  , docUndo = edUndo ed, docRedo = edRedo ed, docLastEdit = edLastEdit ed
  , docOverwrite = edOverwrite ed, docDiscard = edDiscard ed
  , docCsv = edCsv ed
  , docCsvStash = edCsvStash ed
    -- (stamped with the clock: 'trimVecScenes' keeps the most recently shown)
  , docImage = fmap (stampVecUse (edVecClock ed)) (edImage ed)
  , docSvgStash = fmap (stampVecUse (edVecClock ed)) (edSvgStash ed)
    -- A page view keeps no scenes while its document is not showing: they
    -- are the expensive part, and re-parsing the page on return is cheap
    -- next to holding every open PDF's pages.
  , docPdfView = fmap dropPageScenes (edPdfView ed)
  , docPager = edPager ed
  , docRtf = edRtf ed
  , docSheets = edSheets ed
  , docPdf = edPdf ed
  , docHlCache = edHlCache ed
  , docDiags = edDiags ed
  , docDocId = edDocId ed, docDocSeq = edDocSeq ed, docJournalSeq = edJournalSeq ed
  }

-- Make a saved 'Document' the active one.
restoreDoc :: Document -> Editor -> Editor
restoreDoc d ed0 = refreshPdf $ refreshRtf $ refreshImage $ trimVecScenes $ ensureVisible ed
  { edBuffer = docBuffer d, edSavedBuffer = docSavedBuffer d, edCursor = docCursor d, edSelAnchor = docSelAnchor d
  , edDesiredCol = docDesiredCol d, edTop = docTop d, edLeft = docLeft d
  , edPath = docPath d, edModified = docModified d
  , edDiskMtime = docDiskMtime d, edDiskChanged = docDiskChanged d
  , edLineEnding = docLineEnding d, edSavedEol = docSavedEol d
  , edEncoding = docEncoding d, edSavedEnc = docSavedEnc d
  , edFinalNewline = docFinalNewline d, edReadOnly = docReadOnly d
  , edUndo = docUndo d, edRedo = docRedo d, edLastEdit = docLastEdit d
  , edOverwrite = docOverwrite d, edDiscard = docDiscard d
  , edCsv = docCsv d
  , edCsvStash = docCsvStash d
  , edImage = docImage d, edSvgStash = docSvgStash d, edPdfView = docPdfView d
  , edPager = docPager d
  , edRtf = docRtf d
  , edSheets = docSheets d
  , edPdf = docPdf d
  , edHlCache = docHlCache d
  , edDiags = docDiags d
  , edDocId = docDocId d, edDocSeq = docDocSeq d, edJournalSeq = docJournalSeq d
  , edFocus = FEdit, edDialog = Nothing, edSearchMode = False
  , edDefPick = Nothing, edQuickOpen = Nothing, edComplete = Nothing
  }
  -- Every switch moves the vector clock on, so the picture captured just
  -- before this is the most recent one 'trimVecScenes' sees.
  where ed = ed0 { edVecClock = edVecClock ed0 + 1 }

-- | Record when a picture model was last the active document's.
stampVecUse :: Int -> ImageDoc -> ImageDoc
stampVecUse t im = im { idVec = fmap (\vd -> vd { vdUsed = t }) (idVec im) }

-- | Number of open files.
fileCount :: Editor -> Int
fileCount ed = length (edBefore ed) + 1 + length (edAfter ed)

-- | 1-based index of the active file in open order.
fileIndex :: Editor -> Int
fileIndex ed = length (edBefore ed) + 1

-- True when the active document is an untouched, untitled, empty buffer and is
-- the only open file.
isPristine :: Editor -> Bool
isPristine ed = edPath ed == Nothing && not (edModified ed)
             && isEmptyBuffer (edBuffer ed)
             && null (edBefore ed) && null (edAfter ed)

-- | 0-based position (in open order) of an already-open file with this path,
-- if any. Used so re-opening a file switches to it rather than opening a second
-- copy.
findOpenIndex :: FilePath -> Editor -> Maybe Int
findOpenIndex path ed = findIndex ((== Just path) . docPath) (allOpenDocs ed)

-- | Switch to an already-open file with this path, returning the moved editor
-- (with a status note), or 'Nothing' if it is not open.
switchToOpen :: FilePath -> Editor -> Maybe Editor
switchToOpen path ed = fmap toFile (findOpenIndex path ed)
  where
    toFile k = (switchToFile k ed)
                 { edStatus = T.pack (takeFileName path ++ " is already open") }

-- | Load a freshly-read file, opening it as a new document after the active
-- one (unless the active one is a pristine empty buffer, which is reused). If a
-- file with the same path is already open, switch to it instead of opening a
-- second copy.
setLoadedNew :: FilePath -> LoadResult -> Editor -> Editor
setLoadedNew path lr ed = case switchToOpen path ed of
  Just ed' -> ed'
  Nothing
    | isPristine ed -> setLoaded path lr ed
    | otherwise     -> setLoaded path lr (pushActive ed)

-- | Reload the active document from disk in place (the Revert command),
-- discarding unsaved edits. Behaves like a fresh load but keeps the cursor where
-- it still fits and reports it as a revert.
revertLoaded :: FilePath -> LoadResult -> Editor -> Editor
revertLoaded path lr ed =
  let cur = edCursor ed
      -- An SVG reverted from its source view stays in its source view
      -- (setLoaded would otherwise re-enter the picture).
      ed1 = keepSvgSource (setLoaded path lr ed)
      keepSvgSource e
        | isSvgPath path, isNothing (edImage ed), isJust (activeVec e) = toggleSvg e
        | otherwise = e
      pos = clampPos cur (edBuffer ed1)
  in ensureVisible ed1
       { edCursor = pos
       , edStatus = T.pack ("Reverted " ++ takeFileName path) }

-- | The driver's reply to 'EffStatFile': record whether the file on disk is now
-- newer than the baseline captured at load/save, so the File menu can offer
-- Revert. A missing/unreadable file (Nothing) is treated as unchanged.
noteDiskMtime :: Maybe DiskTime -> Editor -> Editor
noteDiskMtime mt ed = ed { edDiskChanged = changed }
  where
    changed = case (mt, edDiskMtime ed) of
                (Just now, Just base) -> now > base
                _                     -> False

-- | Fold freshly-stat'ed mtimes for open files (the background poll / a
-- terminal focus-in) into the stale-on-disk flags of every open document,
-- active or not — same newer-than-baseline rule as 'noteDiskMtime'. The
-- explorer's ◆ markers follow the flags. A newly-stale active file also gets
-- a status-line notice (once), since its buffer is what the user is looking at.
noteDiskMtimes :: [(FilePath, Maybe DiskTime)] -> Editor -> Editor
noteDiskMtimes stats ed =
  edActive { edBefore = map upDoc (edBefore ed), edAfter = map upDoc (edAfter ed) }
  where
    newerThan base p = case (lookup p stats, base) of
      (Just (Just now), Just b) -> now > b
      _                         -> False
    upDoc d
      | not (docDiskChanged d), Just p <- docPath d, newerThan (docDiskMtime d) p =
          d { docDiskChanged = True }
      | otherwise = d
    edActive
      | not (edDiskChanged ed), Just p <- edPath ed, newerThan (edDiskMtime ed) p =
          -- The transition fires once per external change, and knowing the
          -- buffer is stale beats whatever hint was showing before.
          ed { edDiskChanged = True
             , edStatus = T.pack (takeFileName p ++ " changed on disk \x2014 File \x25b8 Revert reloads it") }
      | otherwise = ed

-- | Append a document to the end of the open-files list (used at startup for
-- the second and subsequent files named on the command line).
addDocument :: FilePath -> LoadResult -> Editor -> Editor
addDocument path lr ed =
  touchRecent path ed { edAfter = edAfter ed ++ [svgDocView (docFromLoad (edEditSeq ed) path lr)]
                      , edVecClock = edVecClock ed + 1 }
  where
    -- A second (third, ...) SVG on the command line opens as a picture too;
    -- the first parse happens when it becomes the active document.
    svgDocView d
      | isSvgPath path && cfgSvgPicture (edConfig ed) && svgBufferLooksSvg (lrBuffer lr) =
          d { docImage = Just (mkVectorDoc (edVecClock ed)) }
      | otherwise = d

-- The edit counter is threaded in so an RTF document parsed here is stamped
-- with the same value 'refreshRtf' will compare against when it first becomes
-- active; otherwise it would be re-parsed for nothing on the first switch to it.
docFromLoad :: Int -> FilePath -> LoadResult -> Document
docFromLoad seqNow path lr = Document
  { docBuffer = lrBuffer lr, docSavedBuffer = lrBuffer lr, docCursor = origin, docSelAnchor = Nothing
  , docDesiredCol = 0, docTop = 0, docLeft = 0
  , docPath = Just path, docModified = False
  , docDiskMtime = lrMtime lr, docDiskChanged = False
  , docLineEnding = lrLineEnding lr, docSavedEol = lrLineEnding lr
  , docEncoding = lrEncoding lr, docSavedEnc = lrEncoding lr
  , docFinalNewline = lrFinalNewline lr, docReadOnly = lrReadOnly lr
  , docUndo = Seq.empty, docRedo = Seq.empty, docLastEdit = EKNone, docOverwrite = False
  , docDiscard = False
  , docCsvStash = Nothing
  , docImage = Nothing, docSvgStash = Nothing, docPdfView = Nothing
  , docPager = Nothing
  , docPdf = Nothing
  , docSheets = Nothing
  , docHlCache = Nothing
  , docDiags = []
  -- Titled, so its journal is named by its path and the id is never read.
  , docDocId = 0, docDocSeq = 0, docJournalSeq = 0
  , docCsv = if isCsvPath path
               then Just (Csv.mkCsvLines (csvDelimForPath path) (bufLines (lrBuffer lr)))
               else Nothing
    -- Parsed now, laid out when it first becomes the active document and a
    -- window width exists to wrap to (restoreDoc -> refreshRtf).
  , docRtf = if isRtfPath path
               then Just (Rtf.mkRtfDoc seqNow (bufLines (lrBuffer lr)))
               else Nothing
  }

------------------------------------------------------------------------------
-- Image view mode (read-only). A wholly separate mode to text and CSV: the
-- decoded image lives in 'edImage' and the line buffer stays empty/unused.

-- | Install a decoded image (its full frame sequence) as the active read-only
-- image document.
imageLoaded :: FilePath -> Decoded -> Editor -> Editor
imageLoaded path dc ed0 = touchRecent path $ refreshImage $ ensureVisible ed
  { edBuffer = emptyBuffer, edSavedBuffer = emptyBuffer
  , edCursor = origin, edSelAnchor = Nothing, edDesiredCol = 0
  , edTop = 0, edLeft = 0
  , edPath = Just path, edModified = False
  , edDiskMtime = dcMtime dc, edDiskChanged = False
  , edLineEnding = LF, edSavedEol = LF, edEncoding = Utf8, edSavedEnc = Utf8
  , edFinalNewline = True
  , edReadOnly = True
  , edUndo = Seq.empty, edRedo = Seq.empty, edLastEdit = EKNone
  , edStatus = T.pack ("Viewing image  " ++ imgFmt img ++ " "
                ++ show fullW ++ "x" ++ show fullH
                ++ (if nframes > 1 then ", " ++ show nframes ++ " frames" else "")
                ++ "  —  press 'a' for ASCII/colour")
    -- Opened from the file-explorer panel: keep the selection focus there
    -- (an image view has no keystroke editing to hand the focus to); every
    -- other open route arrives here with FEdit or a modal focus.
  , edFocus = if edFocus ed == FExplorer then FExplorer else FEdit
  , edDialog = Nothing, edSearchMode = False
  , edDefPick = Nothing, edQuickOpen = Nothing, edComplete = Nothing
  , edCsv = Nothing, edCsvStash = Nothing
  , edImage = Just (mkImageDoc epoch dc), edSvgStash = Nothing, edPdfView = Nothing
  }
  where (epoch, ed) = tickVecClock ed0
        frames = dcFrames dc
        img = case frames of ((i, _) : _) -> i
                             []           -> error "imageLoaded: empty frame list"
        nframes = length frames
        (fullW, fullH) = dcFull dc

-- | Open a decoded image as a new document (reusing a pristine empty buffer if
-- present), mirroring 'setLoadedNew' for text.
imageLoadedNew :: FilePath -> Decoded -> Editor -> Editor
imageLoadedNew path frames ed = case switchToOpen path ed of
  Just ed'
    | edFocus ed == FExplorer -> ed' { edFocus = FExplorer }  -- see 'imageLoaded'
    | otherwise               -> ed'
  Nothing
    | isPristine ed -> imageLoaded path frames ed
    | otherwise     -> imageLoaded path frames (pushActive ed)

-- | Install a paged (too-large-to-load) file as the active document. The index
-- pass has already run; the first window arrives via 'EffPagerFill'.
pagerLoaded :: PagerDoc -> Editor -> Editor
pagerLoaded pg ed = touchRecent (pgPath pg) ed
  { edBuffer = emptyBuffer, edSavedBuffer = emptyBuffer
  , edCursor = origin, edSelAnchor = Nothing, edDesiredCol = 0
  , edTop = 0, edLeft = 0
  , edPath = Just (pgPath pg), edModified = False
  , edDiskMtime = Nothing, edDiskChanged = False
  , edLineEnding = pgEol pg, edSavedEol = pgEol pg
  , edEncoding = pgEnc pg, edSavedEnc = pgEnc pg
  , edFinalNewline = True
  , edReadOnly = True
  , edUndo = Seq.empty, edRedo = Seq.empty, edLastEdit = EKNone
  , edStatus = T.pack ("Viewing " ++ takeFileName (pgPath pg) ++ "  "
                ++ Pg.humanBytes (pgSize pg) ++ ", " ++ show (pgLineCount pg)
                ++ " lines  \x2014 read-only paged view")
    -- Same rule as the image view: an open from the explorer panel keeps its
    -- focus, because a read-only view has no keystroke editing to receive it.
  , edFocus = if edFocus ed == FExplorer then FExplorer else FEdit
  , edDialog = Nothing, edSearchMode = False
  , edDefPick = Nothing, edQuickOpen = Nothing, edComplete = Nothing
  , edCsv = Nothing, edCsvStash = Nothing, edImage = Nothing, edSvgStash = Nothing, edPdfView = Nothing, edRtf = Nothing, edSheets = Nothing
  , edPdf = Nothing
  , edPager = Just pg
  , edHlCache = Nothing, edDiags = []
  }

-- | 'pagerLoaded' for a new document (switching to it if already open).
pagerLoadedNew :: PagerDoc -> Editor -> Editor
pagerLoadedNew pg ed = case switchToOpen (pgPath pg) ed of
  Just ed'
    | edFocus ed == FExplorer -> ed' { edFocus = FExplorer }
    | otherwise               -> ed'
  Nothing
    | isPristine ed -> pagerLoaded pg ed
    | otherwise     -> pagerLoaded pg (pushActive ed)

-- | Append a paged document to the open-files list (startup, 2nd+ file).
addPagerDocument :: PagerDoc -> Editor -> Editor
addPagerDocument pg ed =
  touchRecent (pgPath pg) ed { edAfter = edAfter ed ++ [pagerDocSnapshot pg] }

-- | Install a window of lines read for the paged view (driver callback).
pagerFilled :: Int -> Seq Text -> Editor -> Editor
pagerFilled from lns ed = case edPager ed of
  Nothing -> ed
  Just pg -> ed { edPager = Just (Pg.pagerFilled from lns pg) }

-- | The window the paged view needs for the current viewport, if any. The hub
-- turns this into an 'EffPagerFill' after every key.
pagerFillRequest :: Editor -> Maybe (FilePath, Int, Int)
pagerFillRequest ed = do
  pg <- edPager ed
  (from, n) <- Pg.pagerNeedsFill (pagerHeight ed) pg
  pure (pgPath pg, from, n)

-- | Append an image document to the open-files list (startup, 2nd+ file).
addImageDocument :: FilePath -> Decoded -> Editor -> Editor
addImageDocument path dc ed0 =
  touchRecent path ed { edAfter = edAfter ed ++ [imageDocSnapshot path epoch dc] }
  where (epoch, ed) = tickVecClock ed0

imageDocSnapshot :: FilePath -> Int -> Decoded -> Document
imageDocSnapshot path epoch dc = Document
  { docBuffer = emptyBuffer, docSavedBuffer = emptyBuffer, docCursor = origin
  , docSelAnchor = Nothing, docDesiredCol = 0, docTop = 0, docLeft = 0
  , docPath = Just path, docModified = False
  , docDiskMtime = dcMtime dc, docDiskChanged = False
  , docLineEnding = LF, docSavedEol = LF, docEncoding = Utf8, docSavedEnc = Utf8
  , docFinalNewline = True, docReadOnly = True
  , docUndo = Seq.empty, docRedo = Seq.empty, docLastEdit = EKNone, docOverwrite = False
  , docDiscard = False, docCsv = Nothing, docCsvStash = Nothing
  , docImage = Just (mkImageDoc epoch dc), docSvgStash = Nothing, docPdfView = Nothing
  , docPager = Nothing, docRtf = Nothing, docSheets = Nothing, docPdf = Nothing
  , docHlCache = Nothing
  , docDiags = []
  -- Read-only, so never journalled; and titled, so the id is unused anyway.
  , docDocId = 0, docDocSeq = 0, docJournalSeq = 0
  }

-- | A 'Document' snapshot for a paged (too-large-to-load) file, so it can sit
-- in the open-files zipper like any other. There is no buffer: the view reads
-- what it needs from disk (see "Cmedit.Pager").
pagerDocSnapshot :: PagerDoc -> Document
pagerDocSnapshot pg = Document
  { docBuffer = emptyBuffer, docSavedBuffer = emptyBuffer, docCursor = origin
  , docSelAnchor = Nothing, docDesiredCol = 0, docTop = 0, docLeft = 0
  , docPath = Just (pgPath pg), docModified = False
  , docDiskMtime = Nothing, docDiskChanged = False
  , docLineEnding = pgEol pg, docSavedEol = pgEol pg
  , docEncoding = pgEnc pg, docSavedEnc = pgEnc pg
  , docFinalNewline = True, docReadOnly = True
  , docUndo = Seq.empty, docRedo = Seq.empty, docLastEdit = EKNone, docOverwrite = False
  , docDiscard = False, docCsv = Nothing, docCsvStash = Nothing
  , docImage = Nothing, docSvgStash = Nothing, docPdfView = Nothing, docRtf = Nothing, docSheets = Nothing, docPdf = Nothing
  , docPager = Just pg
  , docHlCache = Nothing
  , docDiags = []
  -- Read-only, so never journalled; and titled, so the id is unused anyway.
  , docDocId = 0, docDocSeq = 0, docJournalSeq = 0
  }

-- | Re-scale the active image's cached cell grid if the view size, paint
-- mode or cell pixel geometry has changed. Cheap (a key comparison) when
-- nothing changed.
refreshImage :: Editor -> Editor
refreshImage ed = case activePicture ed of
  Nothing -> ed
  Just idoc | Just vd <- idVec idoc -> refreshVec idoc vd ed
  Just idoc ->
    let lo   = computeLayout ed
        cols = loTextWidth lo
        rows = loTextHeight lo
        crop = imageCrop idoc
        pxk  = cellPxKey ed
        stale = case idCache idoc of
                  Just (c,r,m,cr,px,fr,_) -> c /= cols || r /= rows || m /= idMode idoc
                                               || cr /= crop || px /= pxk || fr /= idFrame idoc
                  Nothing                 -> True
    in if stale && cols > 0 && rows > 0
         then modPicture (const idoc
                     { idCache = Just (cols, rows, idMode idoc, crop, pxk, idFrame idoc
                                      , renderPicture ed idoc cols rows) }) ed
         else ed

-- | Install a finer decode of the active JPEG (the driver's 'JImgDecode'
-- result for @w@). Installs only into the very picture model that asked
-- (same path and epoch: a Revert or a re-open in between makes a new one),
-- only a scale finer than the one showing (never coarser), and only pixels
-- of exactly the size that scale of the asked-for full size has. A failure,
-- or a file whose picture is no longer that size, marks the document stuck
-- at its current scale with a note, so it is not asked for again. A result
-- for a document no longer showing is dropped; it asks again when it is.
imageRedecoded :: ImgWant -> Either String Image -> Editor -> Editor
imageRedecoded w res ed = case edImage ed of
  Just idoc | isNothing (idVec idoc), edPath ed == Just (iwPath w), idEpoch idoc == iwEpoch w ->
    let stuck why = (modImage (\d -> d { idDenStuck = True }) ed)
                      { edStatus = T.pack ("Showing this picture at 1/" ++ show (idDen idoc)
                                           ++ " scale: " ++ why) }
        (fw, fh) = iwFull w
        sized = ((fw + iwDen w - 1) `quot` iwDen w, (fh + iwDen w - 1) `quot` iwDen w)
    in case res of
         _ | iwDen w >= idDen idoc -> ed
         Left err -> stuck err
         Right img
           | iwFull w /= idFull idoc || (imgW img, imgH img) /= sized ->
               stuck "the file has changed since it was opened"
           | otherwise ->
               let delay = case idFrames idoc of ((_, dl) : _) -> dl; [] -> 0
               in refreshImage (modImage (\d -> d { idImage = img, idFrames = [(img, delay)], idFrame = 0
                                                    , idDen = iwDen w, idCache = Nothing
                                                    , idRefines = idRefines d + 1 }) ed)
  _ -> ed

-- | A picture going inactive gives back a refined decode: it returns to its
-- opening scale ('idBaseDen'), reduced from the pixels it has (a box
-- filter; see 'Cmedit.Image.reduceImage'), so an open document that is not
-- showing never holds more than its opening decode, and its refinement
-- count starts again. Showing it again re-asks for the finer scale if the
-- view (whose crop is kept) still needs it. Anything else is unchanged.
parkImage :: ImageDoc -> ImageDoc
parkImage idoc
  | isNothing (idVec idoc), base > idDen idoc, [(img, dl)] <- idFrames idoc =
      let !img' = reduceImage (base `quot` idDen idoc) img
      in idoc { idImage = img', idFrames = [(img', dl)], idFrame = 0, idDen = base
              , idCache = Nothing, idRefines = 0 }
  | otherwise = idoc
  where base = idBaseDen idoc

-- | Move the active document into the zipper, after the others (it is about
-- to be replaced by a newly opened or new document), parked ('parkDoc').
pushActive :: Editor -> Editor
pushActive ed = let !d = parkDoc ed in ed { edBefore = edBefore ed ++ [d] }

-- | 'captureDoc' for a document going into the zipper (it stops being the
-- active one), with its picture parked ('parkImage'). Strict in the parked
-- picture, so the refined pixels are released when the switch happens, not
-- whenever something next looks at the document.
parkDoc :: Editor -> Document
parkDoc = parkDocument . captureDoc

-- | 'parkImage' on a document's picture, strictly (see 'parkDoc'). A no-op
-- on one already parked, so it is safe on anything taken from the zipper.
parkDocument :: Document -> Document
parkDocument d = case docImage d of
  Just im -> let !im' = parkImage im in d { docImage = Just im' }
  Nothing -> d

-- | Lay out a rebuilt zipper around the document at index @k@, which becomes
-- the active one: every other document is parked ('parkDocument'), forced
-- now, so a refined picture that ends up inactive gives its pixels back
-- here rather than whenever something next looks at it.
rebuildZipper :: Int -> [Document] -> Editor -> Editor
rebuildZipper k docs ed =
  let !bef = parkAll (take k docs)
      !aft = parkAll (drop (k + 1) docs)
  in restoreDoc (docs !! k) ed { edBefore = bef, edAfter = aft }
  where
    parkAll ds = let ps = map parkDocument ds in foldr seq () ps `seq` ps

-- | Advance the animation one frame (driver tick; a no-op unless the editor
-- is the one stepping the animation right now — see
-- 'Cmedit.EditorState.imageTickUs', which the driver uses to arm the timer
-- and this re-checks, since a menu or capability reply may have changed who
-- owns playback between arming and firing).
tickImage :: Editor -> Editor
tickImage ed = case (imageTickUs ed, edImage ed) of
  (Just _, Just idoc) ->
    let n = max 1 (length (idFrames idoc))
        next = (idFrame idoc + 1) `mod` n
        (img, _) = idFrames idoc !! next
    in refreshImage ed { edImage = Just idoc { idFrame = next, idImage = img } }
  _ -> ed

-- | Record the terminal's cell pixel geometry (driver callback: the winsize
-- ioctl on startup/resize, or the XTWINOPS reply as a fallback) and re-fit
-- the image view to the true aspect ratio.
setCellPx :: (Int, Int) -> Editor -> Editor
setCellPx wh ed = refreshImage ed { edCellPx = Just wh }

-- Update the active image doc (no-op if there isn't one).
modImage :: (ImageDoc -> ImageDoc) -> Editor -> Editor
modImage f ed = modPicture f ed

-- Reset to the full-image fit.
zoomFull :: Editor -> Editor
zoomFull = modImage (\d -> d { idCrop = Nothing, idDrag = Nothing })

-- Map a text-area cell rectangle to a source-pixel rectangle within the current
-- view, using the same fit geometry the renderer uses.
cellRectToCrop :: Editor -> ImageDoc -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
cellRectToCrop ed idoc (r0,c0,r1,c1) =
  let lo = computeLayout ed
      cols = loTextWidth lo; rows = loTextHeight lo
      (iw, ih) = imageDims idoc
      (cx,cy,cw,ch) = imageCrop idoc
      (outW,outH,offX,offY) = viewFit (cellAspect ed) (imageFitCap ed idoc) cols rows cw ch
      fx k = fromIntegral cx + fromIntegral (k - offX) * fromIntegral cw / fromIntegral outW
      fy k = fromIntegral cy + fromIntegral (k - offY) * fromIntegral ch / fromIntegral outH
      clampD lo' hi v = max lo' (min hi v) :: Double
      x0d = clampD (fromIntegral cx) (fromIntegral (cx+cw)) (fx c0)
      x1d = clampD (fromIntegral cx) (fromIntegral (cx+cw)) (fx (c1+1))
      y0d = clampD (fromIntegral cy) (fromIntegral (cy+ch)) (fy (2*r0))
      y1d = clampD (fromIntegral cy) (fromIntegral (cy+ch)) (fy (2*r1+2))
      -- (a drag wholly right of or below the picture clamps to its far edge,
      -- which is one past the last pixel: keep the origin on the image)
      x0 = min (iw - 1) (floor x0d); x1 = ceiling x1d
      y0 = min (ih - 1) (floor y0d); y1 = ceiling y1d
      nw = max 1 (min (iw - x0) (x1 - x0))
      nh = max 1 (min (ih - y0) (y1 - y0))
  in (max 0 x0, max 0 y0, nw, nh)

-- | Widen a crop (source pixels) to the text area's shape, about its centre:
-- a box dragged taller or wider than the view would otherwise fill only a
-- slice of it, with the picture on either side of that slice not shown though
-- there is room for it. The box always stays inside the result (it is what
-- was asked to see); the result is clamped to the picture, sliding back
-- inside at an edge, and simply spans the picture in a direction the view's
-- shape asks for more than it has.
fitCropToView :: Editor -> ImageDoc -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
fitCropToView ed idoc (x, y, w, h) =
  let lo = computeLayout ed
      cols = max 1 (loTextWidth lo); rows = max 1 (loTextHeight lo)
      (iw, ih) = imageDims idoc
      -- the view's shape in source pixels: a cell is one sub-pixel wide and
      -- two tall, and a sub-pixel is 'cellAspect' times taller than wide
      ratio = fromIntegral cols / (2 * fromIntegral rows * cellAspect ed) :: Double
      wd = fromIntegral (max 1 w); hd = fromIntegral (max 1 h)
      (w', h') | wd / hd < ratio = (min iw (max w (round (hd * ratio))), h)
               | otherwise       = (w, min ih (max h (round (wd / ratio))))
      place pos len len' total = max 0 (min (total - len') (pos - (len' - len) `div` 2))
  in (place x w w' iw, place y h h' ih, w', h')

-- | One scroll-wheel notch of zoom in the image view, anchored on the text-area
-- cell @(row, col)@ under the pointer: the source pixel there stays put on
-- screen, as in a desktop image viewer. Each notch scales by 'wheelZoomStep';
-- the visible window then fills the text area (so zooming in turns a
-- letterboxed picture into a full-screen one) and is clamped to the image,
-- which is why the anchor can only hold in a direction the window does not
-- already span. Zooming out to the full-image fit returns to the unzoomed
-- view ('idCrop' = 'Nothing'), so a click or Esc still resets as before.
--
-- The geometry is 'viewFit''s: the physical scale is display cells per
-- source pixel horizontally, a sub-pixel (half a cell row) is 'cellAspect'
-- times as tall as a cell is wide, and the window in source pixels is the
-- text area divided by the new scale. One wrinkle is inherited from
-- 'imageFitCap': an image shown at native size is capped below the fill
-- scale, and any crop lifts the cap, so the first notch on a small image
-- jumps to fill the window rather than growing by one step.
wheelZoom :: Bool -> Int -> Int -> Editor -> Editor
wheelZoom zoomIn row col ed = case activePicture ed of
  Nothing -> ed
  Just idoc ->
    let lo = computeLayout ed
        cols = max 1 (loTextWidth lo)
        subH = 2 * max 1 (loTextHeight lo)
        a = cellAspect ed
        (iw, ih) = imageDims idoc
        (cx, cy, cw, ch) = imageCrop idoc
        (outW, outH, offX, offY) = viewFit a (imageFitCap ed idoc) cols (subH `div` 2) cw ch
        (fullW, _, _, _) = viewFit a (imageFitCap ed idoc { idCrop = Nothing }) cols (subH `div` 2) iw ih
        sCur  = fromIntegral outW / fromIntegral (max 1 cw) :: Double
        sFull = fromIntegral fullW / fromIntegral (max 1 iw) :: Double
        -- never closer than four source pixels across the text area; a
        -- vector picture re-rasterises sharply at any zoom, so it is held to
        -- a multiple of its fit instead ('vecMaxZoom')
        sMax | isJust (idVec idoc) = sFull * vecMaxZoom
             | otherwise = max sFull (fromIntegral cols / 4)
        s' | zoomIn    = min sMax (sCur * wheelZoomStep)
           | otherwise = sCur / wheelZoomStep
        -- the source point under the pointer (the centre of its cell; a cell
        -- row is two sub-pixels), clamped onto the picture
        clampD l h v = max l (min h v) :: Double
        px = clampD (fromIntegral cx) (fromIntegral (cx + cw))
               (fromIntegral cx + (fromIntegral (col - offX) + 0.5) * fromIntegral cw / fromIntegral (max 1 outW))
        py = clampD (fromIntegral cy) (fromIntegral (cy + ch))
               (fromIntegral cy + (fromIntegral (2 * row + 1 - offY)) * fromIntegral ch / fromIntegral (max 1 outH))
        winW = fromIntegral cols / s'
        winH = fromIntegral subH * a / s'
        -- place the window so the anchor keeps its screen position, then
        -- clamp it to the image (a dimension it spans is the whole image,
        -- centred by 'viewFit')
        place p screen win full
          | win >= fromIntegral full = (0, full)
          | otherwise =
              let w = max 1 (min full (round win))
                  x0 = round (p - screen / s')
              in (max 0 (min (full - w) x0), w)
        (nx, nw) = place px (fromIntegral col + 0.5) winW iw
        (ny, nh) = place py (fromIntegral (2 * row + 1) * a) winH ih
        -- A window that spans the image both ways is the whole picture, shown
        -- at the fill scale (any crop lifts 'imageFitCap'), whatever s' asked
        -- for. That only differs from the fit for a small image held at
        -- native size, and there it must be handled: zooming *out* to it has
        -- to return to the fit, or the fill scale is re-derived from the crop
        -- on every notch and the view can never get back.
        whole = nw == iw && nh == ih
        (fillW, _, _, _) = viewFit a Nothing cols (subH `div` 2) iw ih
        sFill = fromIntegral fillW / fromIntegral (max 1 iw) :: Double
        factor = (if whole then sFill else s') / sFull
        msg z = T.pack ("Zoom " ++ showZoom z ++ " \x2014 scroll to zoom, click or Esc to reset")
    in if zoomIn && sCur >= sMax
         then ed { edStatus = "Maximum zoom" }
         else if s' <= sFull * 1.0001 || (whole && not zoomIn)
           then refreshImage (modImage (\d -> d { idCrop = Nothing }) ed { edStatus = "Zoom reset to fit" })
           else refreshImage (modImage (\d -> d { idCrop = Just (nx, ny, nw, nh) }) ed { edStatus = msg factor })
  where
    -- relative to the whole-image fit, which is what 100% looks like on screen
    showZoom z = show (round (z * 100) :: Int) ++ "%"

-- | Scale factor of one wheel notch in the image view.
wheelZoomStep :: Double
wheelZoomStep = 1.25

setImgMode :: ImgMode -> Editor -> Editor
setImgMode m ed = case activePicture ed of
  Just d  -> (modPicture (const d { idMode = m }) ed)
                { edStatus = T.pack ("Image view: " ++ modeName m) }
  Nothing -> ed
  where modeName HalfBlock = "colour (half-block)"
        modeName Ascii     = "ASCII"

------------------------------------------------------------------------------
-- Vector pictures (SVG). The RTF view's one-way model on the image view's
-- machinery: the text buffer stays the document (Save, Save As, Revert, the
-- journal, the session, undo all see only it), and the picture is a
-- /derived/ view of it — parsed by the driver (fonts and external images are
-- IO, see "Cmedit.FontProvider"), stamped with the document's own edit
-- counter, and re-derived whenever that counter moves. Rasters are made at
-- exactly the size they are shown at, from the scene ("Cmedit.VecView").

-- | Does this buffer begin like an SVG document (first element @<svg>@)?
-- Looks at the first 8 KiB only.
svgBufferLooksSvg :: Buffer -> Bool
svgBufferLooksSvg buf = sniffSvgText (bufferHead 8192 buf)

-- The first @n@ characters of a buffer, without joining the whole of it.
bufferHead :: Int -> Buffer -> Text
bufferHead n buf = go n (toList (Seq.take 512 (bufLines buf))) []
  where
    go _ [] acc = T.concat (reverse acc)
    go k (l : ls) acc
      | k <= 0 = T.concat (reverse acc)
      | otherwise = let t = T.take k l in go (k - T.length t - 1) ls ("\n" : t : acc)

-- | Show the active SVG as its picture: on open (svg-view = picture) and
-- from the source view (Alt+T). The stashed picture model is reused, so a
-- toggle with no edit in between costs nothing; an edited buffer keeps
-- showing the old picture until the driver's re-parse lands. A fresh model
-- takes a fresh epoch ('vdEpoch'), so nothing parsed for an earlier model of
-- this path (before a Revert, a re-open) can be installed into it.
enterSvg :: Editor -> Editor
enterSvg ed
  | not (svgBufferLooksSvg (edBuffer ed)) =
      ed { edStatus = "This file does not look like SVG (no <svg> root element) \x2014 showing the source" }
  | Just st <- edSvgStash ed, Just vd <- idVec st, Just err <- vdError vd, vdSeq vd == edDocSeq ed =
      ed { edStatus = T.pack ("Cannot show this SVG: " ++ err ++ " \x2014 edit the source and press Alt+T to try again") }
  | otherwise =
      let (epoch, ed1) = tickVecClock ed
          idoc = (fromMaybe (mkVectorDoc epoch) (edSvgStash ed1)) { idDrag = Nothing }
      in refreshImage ed1 { edImage = Just idoc, edSvgStash = Nothing, edComplete = Nothing
                          , edStatus = svgPictureStatus (fromMaybe (emptyVecDoc epoch) (idVec idoc)) }

-- | Alt+T / View \x25b8 SVG Picture: swap between the picture and the source.
-- Leaving the picture comes first and asks nothing of the path: a document
-- can be showing its picture under a name that is no longer an SVG's (a
-- rename), and the way back to its text must never be refused.
toggleSvg :: Editor -> Editor
toggleSvg ed
  | Just idoc <- edImage ed, isJust (idVec idoc) =
      ensureVisible ed { edImage = Nothing, edSvgStash = Just idoc { idDrag = Nothing }
                       , edStatus = "SVG source \x2014 editable; Alt+T shows the picture" }
  | not (isSvgFile ed) = ed { edStatus = "The picture view is only for .svg files" }
  | isJust (edImage ed) = ed
  | otherwise = enterSvg ed

-- | A picture view under a path that is no longer an SVG's (Save As
-- @notes.txt@, an explorer rename) goes back to its text, for good: Alt+T on
-- a .txt would have nowhere to come back to.
leaveSvgIfRenamed :: Editor -> Editor
leaveSvgIfRenamed ed
  | isJust (activeVec ed), not (isSvgFile ed) =
      ensureVisible ed { edImage = Nothing, edSvgStash = Nothing }
  | otherwise = ed

svgPictureStatus :: VecDoc -> Text
svgPictureStatus vd = case vdScene vd of
  Nothing -> "Rendering SVG\x2026"
  Just _  -> T.pack ("SVG " ++ svgSizeLabel vd ++ " \x2014 scroll or drag to zoom, Alt+T edits the source"
                     ++ concatMap ("; " ++) (vdNotes vd))

-- | The cell raster the vector picture needs right now: the virtual crop at
-- exactly the fitted sub-pixel size of the text area.
vecCellKey :: Editor -> Maybe VecKey
vecCellKey ed = do
  idoc <- activePicture ed
  vd <- idVec idoc
  _ <- vdScene vd
  let lo = computeLayout ed
      cols = loTextWidth lo; rows = loTextHeight lo
      crop@(_, _, cw, ch) = imageCrop idoc
      (outW, outH, _, _) = viewFit (cellAspect ed) Nothing cols rows cw ch
  if cols <= 0 || rows <= 0 then Nothing else Just (VecKey (vdSceneId vd) crop outW outH)

-- | The cell raster to render on a background job: wanted and not held (or
-- held only as a truncated synchronous raster), and not already known to
-- fail. The driver checks this before every frame. Not requested while a
-- pixel placement actually covers the cells ('imageOverlayActive', which for
-- a vector picture means the placement exists, not merely that one is
-- wanted), so a slow or refused overlay still leaves the cell picture.
vecCellWant :: Editor -> Maybe (VecKey, Scene)
vecCellWant ed = do
  if imageOverlayActive ed then Nothing else Just ()
  vd <- activePicVec ed
  sc <- vdScene vd
  k <- vecCellKey ed
  if vdCellsFailed vd == Just k then Nothing else Just ()
  case vdCells vd of
    Just (k', _) | k' == k, not (vdCellsPartial vd) -> Nothing
    _ -> Just (k, sc)

-- Re-derive the vector picture's cell grid: rasterise synchronously when the
-- scene is cheap (under a small work budget: a scene that runs out of it is
-- shown truncated and redone by the job), otherwise keep the grid it has
-- (stale but on screen) and let the driver's job deliver the new raster
-- ('vecCellsRendered').
refreshVec :: ImageDoc -> VecDoc -> Editor -> Editor
refreshVec idoc vd ed =
  let lo   = computeLayout ed
      cols = loTextWidth lo
      rows = loTextHeight lo
      pxk  = cellPxKey ed
      vd1 = case (vecCellKey ed, vdScene vd) of
        (Just k, Just sc)
          | maybe True ((/= k) . fst) (vdCells vd), vdCost vd <= vecSyncCostMax
          , Just (img, st) <- rasterVecWith vecSyncBudget sc (vkCrop k) (vkW k) (vkH k) ->
              vd { vdCells = Just (k, img), vdCellsPartial = rsExhausted st, vdExhausted = False
                 , vdRasterNotes = filter (/= vecTruncatedText) (rasterNoteList st) }
        _ -> vd
      grid = case vdCells vd1 of
        Just (k, img) ->
          let offX = (cols - vkW k) `div` 2; offY = (2 * rows - vkH k) `div` 2
              key = (cols, rows, idMode idoc, vkCrop k, pxk, vkScene k)
              fresh = case idCache idoc of
                Just (c, r, m, cr, px, fr, _) -> (c, r, m, cr, px, fr) == key
                Nothing -> False
          in if fresh then idCache idoc
             else Just (cols, rows, idMode idoc, vkCrop k, pxk, vkScene k
                       , renderImagePlaced (idMode idoc) cols rows (vkW k, vkH k, offX, offY)
                                           (0, 0, vkW k, vkH k) img)
        Nothing -> idCache idoc
  in if cols <= 0 || rows <= 0 then ed
     else modPicture (const idoc { idVec = Just vd1, idCache = grid }) ed

-- | The identity of one parse request: path, picture epoch, edit counter.
data SvgWant = SvgWant
  { swPath  :: !FilePath
  , swEpoch :: !Int
  , swSeq   :: !Int
  } deriving (Eq, Show)

-- | What the driver should parse, if anything: the active document's
-- picture, when it is showing and the buffer has moved past the version it
-- was parsed from. Only the showing picture: a stash (the source is on
-- screen) is re-parsed when Alt+T brings it back, so typing in the source
-- never queues parses nobody is looking at.
svgParseWant :: Editor -> Maybe SvgWant
svgParseWant ed = do
  p <- edPath ed
  vd <- activeVec ed
  if vdSeq vd /= edDocSeq ed then Just (SvgWant p (vdEpoch vd) (edDocSeq ed)) else Nothing

-- | A parse finished (driver callback): the request it answers, the scene id
-- the driver gave it, whether it had to be drawn before font discovery
-- finished, and the scene or the reason there is none. Applied only to the
-- very picture model that asked (same path, epoch and edit counter). A
-- failure leaves the picture view for the source, with the reason.
svgParsed :: SvgWant -> Int -> Bool -> Either String (Scene, SceneSize, [String]) -> Editor -> Editor
svgParsed w sceneId fontless res ed
  | edPath ed /= Just (swPath w) || edDocSeq ed /= swSeq w = ed
  | otherwise = case edImage ed of
      Just idoc | Just vd <- idVec idoc, vdEpoch vd == swEpoch w -> case res of
        Right (sc, sz, notes) ->
          let b = scBounds sc
              size = (rectW b, rectH b)
              -- Drawn without fonts because discovery had not finished: if it
              -- has finished since, redraw now (a fresh epoch, so this one is
              -- not mistaken for the redraw); otherwise 'fontsReady' will.
              (redo, ed1) = if fontless && edFontsReady ed then let (e, ed') = tickVecClock ed in (Just e, ed')
                            else (Nothing, ed)
              vd' = vd { vdScene = Just sc, vdSceneId = sceneId
                       , vdSeq = if isJust redo then -1 else swSeq w
                       , vdEpoch = fromMaybe (vdEpoch vd) redo
                       , vdFontless = fontless && isNothing redo
                       , vdSize = size, vdCost = szCost sz, vdBytes = szBytes sz, vdNotes = notes, vdError = Nothing
                       , vdCellsFailed = Nothing }
              keepCrop = vecDims size == vecDims (vdSize vd) && isJust (vdScene vd)
              idoc' = idoc { idVec = Just vd', idCrop = if keepCrop then idCrop idoc else Nothing }
          in refreshImage ed1 { edImage = Just idoc', edStatus = svgPictureStatus vd' }
        Left err ->
          let vd' = vd { vdSeq = swSeq w, vdError = Just err }
          in ensureVisible ed { edImage = Nothing, edSvgStash = Just idoc { idVec = Just vd', idDrag = Nothing }
                              , edStatus = T.pack ("Cannot show this SVG: " ++ err ++ " \x2014 showing the source") }
      _ -> ed

-- | Font discovery finished (driver callback): every picture that was drawn
-- without its fonts, active, stashed or in the zipper, is redrawn when it is
-- next shown. Each gets a fresh epoch, and keeps its old scene on screen
-- until the redraw lands.
fontsReady :: Editor -> Editor
fontsReady ed0 =
  let ed = ed0 { edFontsReady = True }
      bump e mid = case mid >>= \i -> fmap ((,) i) (idVec i) of
        Just (i, vd) | vdFontless vd ->
          let (ep, e') = tickVecClock e
          in (e', Just i { idVec = Just vd { vdEpoch = ep, vdSeq = -1, vdFontless = False } })
        _ -> (e, mid)
      (ed1, img') = bump ed (edImage ed)
      (ed2, st')  = bump ed1 (edSvgStash ed1)
      goDocs e [] = (e, [])
      goDocs e (d : ds) =
        let (e1, di) = bump e (docImage d)
            (e2, ds') = bump e1 (docSvgStash d)
            (e3, rest) = goDocs e2 ds
        in (e3, d { docImage = di, docSvgStash = ds' } : rest)
      (ed3, before) = goDocs ed2 (edBefore ed2)
      (ed4, after) = goDocs ed3 (edAfter ed3)
  in ed4 { edImage = img', edSvgStash = st', edBefore = before, edAfter = after
          , edPdfView = fmap pdfFontsReady (edPdfView ed4) }

-- | A background cell raster finished (driver callback). Kept when it is of
-- the scene still showing; the grid is rebuilt from it.
vecCellsRendered :: VecKey -> Image -> Bool -> [String] -> Editor -> Editor
vecCellsRendered k img exhausted notes ed = case activePicture ed of
  Just idoc | Just vd <- idVec idoc, vdSceneId vd == vkScene k, imgW img == vkW k, imgH img == vkH k ->
    -- (the grid is rebuilt even for the same key: this raster may replace a
    -- truncated synchronous one)
    let ed1 = refreshImage (modPicture (const idoc { idCache = Nothing
                                                    , idVec = Just vd { vdCells = Just (k, img), vdExhausted = exhausted
                                                                      , vdRasterNotes = filter (/= vecTruncatedText) notes
                                                                      , vdCellsPartial = False, vdCellsFailed = Nothing } }) ed)
    in if null notes then ed1 else ed1 { edStatus = T.pack (intercalate "; " notes) }
  _ -> ed

-- | A background cell raster failed (driver callback): remember the key, so
-- the request is not repeated every frame, and say so.
vecCellsFailed :: VecKey -> Editor -> Editor
vecCellsFailed k ed = case activePicture ed of
  Just idoc | Just vd <- idVec idoc, vdSceneId vd == vkScene k ->
    (modPicture (const idoc { idVec = Just vd { vdCellsFailed = Just k } }) ed)
       { edStatus = "This view of the picture could not be drawn" }
  _ -> ed

-- | What the status line says when a raster ran out of budget.
vecTruncatedNote :: Text
vecTruncatedNote = T.pack vecTruncatedText

-- | Bound the memory the inactive SVG documents hold: only the
-- 'vecKeepScenes' most recently shown keep their scenes (within
-- 'vecKeepCost'); the rest drop their scene, cell raster and grid and
-- re-parse when next shown. The active document is never trimmed. Run after
-- every document switch ('restoreDoc').
trimVecScenes :: Editor -> Editor
trimVecScenes ed =
  let slots = [ (i, which, vd) | (i, d) <- zip [0 :: Int ..] (edBefore ed ++ edAfter ed)
                               , (which, Just im) <- [(0 :: Int, docImage d), (1, docSvgStash d)]
                               , Just vd <- [idVec im], isJust (vdScene vd) ]
      keep = keepScenes vecKeepScenes vecKeepCost [ (vdUsed vd, vdCost vd) | (_, _, vd) <- slots ]
      dropSet = [ (i, which) | ((i, which, _), k) <- zip slots keep, not k ]
      dropIm im = im { idCache = Nothing
                     , idVec = fmap (\vd -> vd { vdScene = Nothing, vdCells = Nothing, vdSeq = -1 }) (idVec im) }
      fixDoc (i, d) =
        d { docImage = if (i, 0) `elem` dropSet then fmap dropIm (docImage d) else docImage d
          , docSvgStash = if (i, 1) `elem` dropSet then fmap dropIm (docSvgStash d) else docSvgStash d }
      nb = length (edBefore ed)
      docs' = map fixDoc (zip [0 ..] (edBefore ed ++ edAfter ed))
  in if null dropSet then ed else ed { edBefore = take nb docs', edAfter = drop nb docs' }



------------------------------------------------------------------------------
-- PDF page view. A mode of the PDF view, not an image document: the
-- document stays 'edPdf', and 'edPdfView' adds a vector picture of one page
-- ('pvPic') that the picture machinery reaches through 'activePicture'. The
-- page's scene comes from the driver (a background job runs
-- 'Cmedit.PdfRender.renderPage', fonts first), keyed by (path, epoch, page),
-- exactly as an SVG's parse is keyed by (path, epoch, edit counter); a PDF
-- has no edit counter, but it has pages.

-- | The identity of one page-scene request.
data PageWant = PageWant
  { pwPath  :: !FilePath
  , pwEpoch :: !Int
  , pwPage  :: !Int
  } deriving (Eq, Show)

-- | How many page dictionaries the file has (the page view's page count:
-- extraction may have stopped short of them on a budget, rendering need not).
pdfSourcePages :: PdfDoc -> Int
pdfSourcePages pd = max 1 (Seq.length (Pdf.srcPages (pdSource pd)))

-- A page's picture with no scene yet.
pagePicture :: Int -> PdfDoc -> Int -> ImageDoc
pagePicture epoch pd n =
  let im = mkVectorDoc epoch
  in im { idVec = fmap (\vd -> vd { vdSize = pdfPageSize pd n }) (idVec im) }

-- | The page view's initial state for a freshly opened PDF. @pdf-view =
-- auto@ shows pages when the terminal draws pixels; when it chose reflow for
-- want of them it remembers that ('pvAutoPending'), because the probe
-- replies arrive after the first frame.
newPdfView :: Int -> Config -> Bool -> PdfDoc -> PdfView
newPdfView epoch cfg gfx pd =
  let (on, pending) = case cfgPdfView cfg of
        PdfViewPage   -> (True, False)
        PdfViewReflow -> (False, False)
        PdfViewAuto   -> (gfx, not gfx)
  in PdfView { pvOn = on, pvManual = False, pvAutoPending = pending
             , pvPage = 1, pvPages = pdfSourcePages pd, pvEpoch = epoch
             , pvPic = pagePicture epoch pd 1, pvScenes = []
             , pvFailed = [], pvNoFit = [], pvLeftAt = Nothing
             , pvFit = cfgPdfFit cfg, pvDrag = cfgPdfDrag cfg
             , pvAnchor = 1, pvOff = 0, pvZoom = 1, pvPanX = 0
             , pvTiles = M.empty, pvTileFailed = [], pvGlyphs = IM.empty
             , pvBox = Nothing, pvSel = Nothing }

-- | Drop every scene a page view holds (an inactive document keeps none);
-- the current page re-parses when the document is shown again.
dropPageScenes :: PdfView -> PdfView
dropPageScenes pv =
  let pic = pvPic pv
  in pv { pvScenes = [], pvTiles = M.empty, pvBox = Nothing, pvGlyphs = IM.empty
        , pvPic = pic { idCache = Nothing
                      , idVec = fmap (\vd -> vd { vdScene = Nothing, vdCells = Nothing, vdSeq = -1
                                                , vdCellsPartial = False, vdCellsFailed = Nothing }) (idVec pic) } }

-- | Alt+T / View ▸ Page View in a PDF: switch reflow ↔ page, keeping the
-- place. Reflow → page lands on the page holding the reflow window's top
-- line; page → reflow lands on that page's first line. A manual choice
-- sticks: @pdf-view = auto@ never switches this document again.
togglePdfPage :: Editor -> Editor
togglePdfPage ed = case (edPdf ed, edPdfView ed) of
  (Just pd, Just pv)
    | pvOn pv ->
        let pd' = Pdf.pdfGoToPage (pdfHeight ed) (min (Pdf.pdfPageCount pd) (pvPage pv)) pd
        in ed { edPdf = Just pd', edPdfView = Just pv { pvOn = False, pvManual = True, pvAutoPending = False
                                                       , pvLeftAt = Just (pdTop pd'), pvTiles = M.empty, pvBox = Nothing }
              , edMouseSelecting = False
              , edStatus = "Reflow view \x2014 select, copy and find the text; Alt+T shows the pages" }
    | otherwise -> enterPageView True ed
  _ -> ed { edStatus = "The page view is only for PDF files" }

-- Show the page view on the reflow view's current page.
enterPageView :: Bool -> Editor -> Editor
enterPageView manual ed = case (edPdf ed, edPdfView ed) of
  (Just pd, Just pv) ->
    -- 'pvPage' stays authoritative while the reflow window is where leaving
    -- put it; only a window the user (or a find) has moved since says
    -- which page to show.
    let n | pvLeftAt pv == Just (pdTop pd) = pvPage pv
          | otherwise = max 1 (min (pvPages pv) (reflowPageAt pd))
        pv1 = pv { pvOn = True, pvManual = pvManual pv || manual, pvAutoPending = False, pvLeftAt = Nothing }
        ed1 = ed { edPdfView = Just pv1, edComplete = Nothing }
        ed2 = if n == pvPage pv then ed1 else pdfTurnTo n ed1
    in refreshImage ed2 { edStatus = pageStatusNote ed2 }
  _ -> ed

-- The page the reflow window is showing, for the page view to open on.
-- 'Pdf.pdfCurrentPage' counts every page whose first line is at or above the
-- window's top, which is right for reading but wrong here when pages have no
-- text: a cover image and the page after it start on the same laid-out line,
-- and a fresh document then opened its page view on page 2. Of the pages
-- starting on that line, the first is the one the window shows.
reflowPageAt :: PdfDoc -> Int
reflowPageAt pd =
  let cur = Pdf.pdfCurrentPage pd
      start = Pdf.pdfPageLine cur pd
  in case [ k | k <- [1 .. cur], Pdf.pdfPageLine k pd == start ] of
       (k : _) -> k
       [] -> cur

-- The status-line note on entering the page view: on a terminal that cannot
-- draw pixels the page is a thumbnail in cells, and reading it means reflow.
pageStatusNote :: Editor -> Text
pageStatusNote ed
  | edGfxCaps ed = "Page view \x2014 PgUp/PgDn turn pages, Ctrl+wheel or +/- zoom, Alt+T for text"
  | otherwise = "Page view (thumbnail: this terminal shows no pixels) \x2014 Alt+T to read the text"

-- | Caps arrived (driver callback, after 'setGfxCaps'): every PDF that
-- @pdf-view = auto@ opened in reflow for want of pixels switches to the
-- page view, once, unless its user already chose a mode.
pdfCapsArrived :: Editor -> Editor
pdfCapsArrived ed0
  | not (edGfxCaps ed0) = ed0
  | otherwise =
      let ed1 = case edPdfView ed0 of
            Just pv | pvAutoPending pv && not (pvManual pv) && isJust (edPdf ed0) -> enterPageView False ed0
            _ -> ed0
          fixDoc d = case (docPdf d, docPdfView d) of
            (Just pd, Just pv) | pvAutoPending pv && not (pvManual pv) ->
              let n = max 1 (min (pvPages pv) (reflowPageAt pd))
              in d { docPdfView = Just pv { pvOn = True, pvAutoPending = False, pvPage = n
                                          , pvAnchor = n, pvOff = 0
                                          , pvPic = pagePicture (pvEpoch pv) pd n, pvScenes = [] } }
            _ -> d
      in ed1 { edBefore = map fixDoc (edBefore ed1), edAfter = map fixDoc (edAfter ed1) }

-- | Turn to page @n@ (clamped). The page being left goes into the scene
-- cache, the page arriving comes out of it when it is there (so a
-- prefetched PgDn is instant) and otherwise waits for its parse; the cache
-- is then held to the new page ±1 within 'vecKeepCost'. The view returns to
-- the fit on every turn.
pdfTurnTo :: Int -> Editor -> Editor
pdfTurnTo n0 ed = case (edPdf ed, edPdfView ed) of
  (Just _, Just pv) | pvOn pv, pvFit pv == PdfFitWidth -> stripGoPage n0 ed
  (Just pd, Just pv) | pvOn pv ->
    let n = max 1 (min (pvPages pv) n0)
        cur = pvPage pv
        old = pvPic pv
        leaving = case idVec old of
          Just vd | Just sc <- vdScene vd, vdSeq vd == cur ->
            [(cur, PageScene (vdSceneId vd) sc (vdNotes vd) (vdFontless vd) (SceneSize (vdCost vd) (vdBytes vd)))]
          _ -> []
        pool = leaving ++ filter ((/= cur) . fst) (pvScenes pv)
        fresh = (pagePicture (pvEpoch pv) pd n) { idMode = idMode old }
        pic = case (lookup n pool, lookup n (pvFailed pv)) of
          (Just ps, _) -> fresh { idVec = fmap (\vd -> installPageScene n ps vd) (idVec fresh) }
          -- a page that already failed in this opening shows its error at
          -- once, rather than being parsed (and failing) again
          (_, Just err) -> fresh { idVec = fmap (\vd -> vd { vdSeq = n, vdError = Just err }) (idVec fresh) }
          _ -> fresh
        cache = trimPageCache n (filter ((/= n) . fst) pool)
        note = case lookup n (pvFailed pv) of
          Just err -> T.pack ("Page " ++ show n ++ " could not be drawn: " ++ err)
          Nothing -> edStatus ed
    in if n == cur then ed
       -- (a selection is of the page it was made on, the only one highlighted)
       else refreshImage ed { edPdfView = Just pv { pvPage = n, pvPic = pic, pvScenes = cache, pvNoFit = [], pvSel = Nothing }
                            , edStatus = note }
  _ -> ed

installPageScene :: Int -> PageScene -> VecDoc -> VecDoc
installPageScene n ps vd =
  let b = scBounds (psScene ps)
  in vd { vdScene = Just (psScene ps), vdSceneId = psSceneId ps, vdSeq = n
        , vdSize = (rectW b, rectH b), vdCost = szCost (psSize ps), vdBytes = szBytes (psSize ps)
        , vdNotes = psNotes ps, vdFontless = psFontless ps, vdError = Nothing
        , vdCells = Nothing, vdCellsPartial = False, vdCellsFailed = Nothing }

-- Hold the scene cache to the pages beside @n@ (at most 'vecKeepScenes')
-- within 'vecKeepCost', nearest first: 'keepScenes' with recency = closeness.
trimPageCache :: Int -> [(Int, PageScene)] -> [(Int, PageScene)]
trimPageCache n pool =
  let near = [ e | e@(p, _) <- pool, abs (p - n) <= 1 ]
      -- the next page ranks above the previous (reading goes forward), so
      -- when only one fits the choice is deterministic, not "the newest";
      -- and the budget is the scenes' /memory/ ('szBytes', measured by the job), which for a
      -- real page is its decoded images, with no exception for the first:
      -- a neighbour too big for 'pageKeepBytes' is simply not kept
      ranked = sortOn (\(p, _) -> 2 * abs (p - n) + (if p < n then 1 else 0)) near
      pick _ [] = []
      pick used (e@(_, ps) : rest)
        | used + b <= pageKeepBytes = e : pick (used + b) rest
        | otherwise = pick used rest
        where b = szBytes (psSize ps)
      kept = map fst (pick 0 ranked)
  in [ e | e@(p, _) <- near, p `elem` kept ]

-- | The current page's scene to request, when it has none.
pdfPageWant :: Editor -> Maybe PageWant
pdfPageWant ed
  | pageStripOn ed = do
      p <- edPath ed
      pv <- edPdfView ed
      n <- listToMaybe [ k | k <- stripVisiblePages ed, needsScene pv k ]
      pure (PageWant p (pvEpoch pv) n)
pdfPageWant ed = do
  p <- edPath ed
  pv <- edPdfView ed
  if pageViewOn ed then Just () else Nothing
  vd <- idVec (pvPic pv)
  if vdSeq vd /= pvPage pv then Just (PageWant p (pvEpoch pv) (pvPage pv)) else Nothing

-- | The page to prefetch, once the current one is settled (its scene is in
-- and its cell picture drawn): the next page, then the previous, if not
-- cached already. The driver adds its own condition (no job in flight).
pdfPrefetchWant :: Editor -> Maybe PageWant
pdfPrefetchWant ed
  | pageStripOn ed = do
      p <- edPath ed
      pv <- edPdfView ed
      if any (needsScene pv) (stripVisiblePages ed) then Nothing else Just ()
      n <- listToMaybe [ k | k <- stripNearPages ed, needsScene pv k, k `notElem` pvNoFit pv ]
      pure (PageWant p (pvEpoch pv) n)
pdfPrefetchWant ed = do
  p <- edPath ed
  pv <- edPdfView ed
  if pageViewOn ed then Just () else Nothing
  vd <- idVec (pvPic pv)
  _ <- vdScene vd
  if vdSeq vd == pvPage pv && isNothing (vecCellWant ed) then Just () else Nothing
  n <- listToMaybe [ k | k <- [pvPage pv + 1, pvPage pv - 1], k >= 1, k <= pvPages pv
                       , isNothing (lookup k (pvScenes pv))
                       , isNothing (lookup k (pvFailed pv))   -- failed: never again
                       , k `notElem` pvNoFit pv ]              -- did not fit: not for this page
  pure (PageWant p (pvEpoch pv) n)

-- | A page scene arrived (driver callback): installed when it is the
-- current page of the very view that asked for it, cached when it is a
-- neighbour, dropped otherwise (another document, another opening of this
-- one, a page since left behind).
pdfPageParsed :: PageWant -> Int -> Bool -> Either String (Scene, SceneSize, [String]) -> Editor -> Editor
pdfPageParsed w sid fontless res ed = case edPdfView ed of
  Just pv | pvFit pv == PdfFitWidth, pvOn pv, edPath ed == Just (pwPath w), pvEpoch pv == pwEpoch w, isJust (edPdf ed) ->
    stripPageParsed w sid fontless res pv ed
  Just pv | edPath ed == Just (pwPath w), pvEpoch pv == pwEpoch w, isJust (edPdf ed) -> case res of
    Right (sc, sz, notes)
      | pwPage w == pvPage pv ->
          let pic = pvPic pv
              ps = PageScene sid sc notes fontless sz
              -- Drawn before font discovery finished, but the fonts have
              -- arrived since: show it, and redraw at once (as 'svgParsed').
              redo = fontless && edFontsReady ed
              install vd = (installPageScene (pwPage w) ps vd)
                             { vdSeq = if redo then -1 else pwPage w, vdFontless = fontless && not redo }
              pic' = pic { idCache = idCache pic, idVec = fmap install (idVec pic) }
              ed1 = refreshImage ed { edPdfView = Just pv { pvPic = pic' } }
          in if null notes || not (pvOn pv) then ed1
             else ed1 { edStatus = T.pack ("Page " ++ show (pwPage w) ++ ": " ++ intercalate "; " notes) }
      | abs (pwPage w - pvPage pv) <= 1 ->
          let pool = (pwPage w, PageScene sid sc notes fontless sz) : filter ((/= pwPage w) . fst) (pvScenes pv)
              -- a fontless neighbour, once fonts are known, is not worth keeping
              pool' = if fontless && edFontsReady ed then filter ((/= pwPage w) . fst) pool else pool
              kept = trimPageCache (pvPage pv) pool'
              -- whatever the budget could not keep (the arrival or a page it
              -- displaced) is not prefetched again for this page
              lost = [ q | (q, _) <- pool, abs (q - pvPage pv) <= 1, isNothing (lookup q kept) ]
          in ed { edPdfView = Just pv { pvScenes = kept, pvNoFit = lost ++ filter (`notElem` lost) (pvNoFit pv) } }
      | otherwise -> ed
    Left err ->
      let pv1 = pv { pvFailed = (pwPage w, err) : filter ((/= pwPage w) . fst) (pvFailed pv) }
      in if pwPage w == pvPage pv
           then let pic = pvPic pv
                    pic' = pic { idVec = fmap (\vd -> vd { vdSeq = pwPage w, vdError = Just err }) (idVec pic) }
                in ed { edPdfView = Just pv1 { pvPic = pic' }
                      , edStatus = T.pack ("Page " ++ show (pwPage w) ++ " could not be drawn: " ++ err) }
           else ed { edPdfView = Just pv1 }
  _ -> ed

-- | The next page's overlay as a turn would ask for it, without turning: its
-- cached scene id and size, at the fit. The driver sizes the payload from
-- this ('gfxFitVector') and prefetches it; nothing is rasterised here (the
-- first version simulated the turn, which re-ran the synchronous cell
-- raster for the next page on every frame).
pdfNextPageFit :: Editor -> Maybe (Int, Scene, (Int, Int, Int, Int))
pdfNextPageFit ed = do
  pv <- edPdfView ed
  if pageViewOn ed && not (pageStripOn ed) && isNothing (idCrop (pvPic pv)) then Just () else Nothing
  ps <- lookup (pvPage pv + 1) (pvScenes pv)
  let b = scBounds (psScene ps)
      (vw, vh) = vecDims (rectW b, rectH b)
  pure (psSceneId ps, psScene ps, (0, 0, vw, vh))

-- | Fonts arrived: a page drawn without them (and any cached one) is redrawn.
pdfFontsReady :: PdfView -> PdfView
pdfFontsReady pv =
  let pic = pvPic pv
      stale = maybe False vdFontless (idVec pic) || any (psFontless . snd) (pvScenes pv)
  in if not stale then pv
     else pv { pvScenes = filter (not . psFontless . snd) (pvScenes pv)
             , pvPic = pic { idVec = fmap (\vd -> if vdFontless vd then vd { vdSeq = -1, vdFontless = False } else vd) (idVec pic) } }

-- | Does this mouse event start a pan drag: Ctrl+drag or the middle button,
-- over a picture that can move (zoomed in, or the PDF fit-width column)?
-- (Not Shift: terminals keep Shift+drag for their own text selection.)
panStarts :: MouseEvent -> Editor -> Bool
panStarts me ed =
  mePressed me && not (meDrag me)
    && (meButton me == MBMiddle || (meButton me == MBLeft && modCtrl (meMods me)))
    && edFocus ed == FEdit && picturePannable ed
    && row >= 0 && row < loTextHeight lo && col >= 0 && col < loTextWidth lo
  where lo = computeLayout ed
        row = meRow me - loTextTop lo
        col = meCol me - loTextLeft lo

-- | Begin a pan drag at the pointer's cell.
beginPan :: MouseEvent -> Editor -> Editor
beginPan me ed = ed { edPanDrag = Just (meRow me, meCol me), edMouseSelecting = True }

-- | A mouse event during a pan drag: a motion moves the picture with the
-- pointer (the point grabbed stays under it), a release ends the drag. A
-- motion with no button held means the release was missed (it happened
-- outside the window): the drag ends there too.
panDragMove :: MouseEvent -> (Int, Int) -> Editor -> Editor
panDragMove me (r0, c0) ed
  | not (mePressed me) || (meDrag me && meButton me == MBNone) =
      ed { edPanDrag = Nothing, edMouseSelecting = False }
  | meDrag me =
      (panByCells (meCol me - c0) (meRow me - r0) ed) { edPanDrag = Just (meRow me, meCol me) }
  | otherwise = ed

-- Move the picture by whole cells (right and down when positive).
panByCells :: Int -> Int -> Editor -> Editor
panByCells dc dr ed
  | dc == 0 && dr == 0 = ed
  | Just (_, _, g, a, pan) <- stripState ed =
      let (cw, _) = stripCellPx ed
          colPx = if stripPixel ed then cw else 1
      in stripSet g (St.scrollAnchor g (negate (fromIntegral dr * stripRowPx ed)) a) (pan - dc * colPx) ed
  | Just pic <- activePicture ed, Just (x, y, w, h) <- idCrop pic =
      let lo = computeLayout ed
          (outW, outH, _, _) = viewFit (cellAspect ed) (imageFitCap ed pic) (loTextWidth lo) (loTextHeight lo) w h
          (iw, ih) = imageDims pic
          nx = max 0 (min (iw - w) (x - round (fromIntegral dc * fromIntegral w / fromIntegral (max 1 outW) :: Double)))
          ny = max 0 (min (ih - h) (y - round (fromIntegral (2 * dr) * fromIntegral h / fromIntegral (max 1 outH) :: Double)))
      in refreshImage (modPicture (\p -> p { idCrop = Just (nx, ny, w, h) }) ed)
  | otherwise = ed

-- | Pan a zoomed page by a fraction of the view (arrow keys), clamped to the
-- page.
pdfPan :: Double -> Double -> Editor -> Editor
pdfPan fx fy ed = case activePicture ed of
  Just pic | Just (x, y, w, h) <- idCrop pic ->
    let (iw, ih) = imageDims pic
        nx = max 0 (min (iw - w) (x + round (fx * fromIntegral w)))
        ny = max 0 (min (ih - h) (y + round (fy * fromIntegral h)))
    in refreshImage (modPicture (\p -> p { idCrop = Just (nx, ny, w, h) }) ed)
  _ -> ed

-- | Zoom one step about the centre of the text area (+ and - keys).
pdfZoomCentre :: Bool -> Editor -> Editor
pdfZoomCentre zin ed =
  let lo = computeLayout ed
  in wheelZoom zin (loTextHeight lo `div` 2) (loTextWidth lo `div` 2) ed

------------------------------------------------------------------------------
-- PDF page view: the fit-width layout ("Cmedit.PdfStrip"). Pages stacked in
-- one column, scrolled by pixels. Its scenes share the page view's pool
-- ('pvScenes', every page in it; 'pvPic' is the fit-page layout's and sits
-- idle here), its bands are 'pvTiles', and its position is an anchor
-- ('pvAnchor', 'pvOff'). Everything here addresses the geometry of the
-- terminal's raster: device pixels where the overlay draws, half-block
-- sub-pixels otherwise.

-- | One text row in the layout's pixels (what an arrow key scrolls by).
stripRowPx :: Editor -> Double
stripRowPx ed = if stripPixel ed then fromIntegral (snd (stripCellPx ed)) else 2

-- Store a new anchor and pan (settled and clamped), then 'stripSync'.
stripSet :: StripGeom -> St.Anchor -> Int -> Editor -> Editor
stripSet g a pan ed = case edPdfView ed of
  Just pv ->
    let (p, o) = St.settleAnchor g a
        pan' = max 0 (min (St.panMax g) pan)
        pv' = pv { pvAnchor = p, pvOff = St.pxToOff g p o
                 , pvPanX = if St.colWidth g > 0 then fromIntegral pan' / fromIntegral (St.colWidth g) else 0 }
    in stripSync ed { edPdfView = Just pv' }
  Nothing -> ed

-- | Keep the derived state in step with the anchor: settle it (a resize or
-- zoom may have pushed it past the end), make 'pvPage' the page the view is
-- on, and hold the bands and scenes to the view's neighbourhood. A band is
-- kept only while it is exactly one the view would ask for now (same scene,
-- scale, window), which is what drops every band of the old zoom at once.
-- Cheap (one screen of pages); 'refreshPdf' runs it after every key.
stripSync :: Editor -> Editor
stripSync ed = case stripState ed of
  Nothing -> ed
  Just (_, pv, g, a0, pan) ->
    let a@(p, o) = St.settleAnchor g a0
        cur = maybe (pvPage pv) pcPage (St.currentPiece g a)
        wanted = M.fromList [ (k, ()) | k <- stripTileKeys ed pv g a pan (stripNearLo g) (stripNearHi g) ]
        tiles = M.intersection (pvTiles pv) wanted
        near = stripPagesIn g a (stripNearLo g) (stripNearHi g)
        glyphs = IM.filterWithKey (\k _ -> abs (k - cur) <= glyphKeepPages || k `elem` near) (pvGlyphs pv)
        pv' = pv { pvAnchor = p, pvOff = St.pxToOff g p o, pvPage = cur
                 , pvTiles = tiles, pvGlyphs = glyphs
                 -- what did not fit the budget is tried again from a new page
                 , pvNoFit = if cur /= pvPage pv then [] else pvNoFit pv }
    in ed { edPdfView = Just pv' }

-- The neighbourhood held around the view: a screen above, two below
-- (reading goes down).
stripNearLo, stripNearHi :: StripGeom -> Int
stripNearLo g = negate (sgViewH g)
stripNearHi g = 2 * sgViewH g

stripPagesIn :: StripGeom -> St.Anchor -> Int -> Int -> [Int]
stripPagesIn g a lo hi = map pcPage (St.piecesIn g a lo hi)

-- | The pages on screen in the fit-width layout, top to bottom.
stripVisiblePages :: Editor -> [Int]
stripVisiblePages ed = case stripState ed of
  Just (_, _, g, a, _) -> map pcPage (St.visiblePieces g a)
  Nothing -> []

-- | The pages to prefetch: below the view, then above it.
stripNearPages :: Editor -> [Int]
stripNearPages ed = case stripState ed of
  Just (_, _, g, a, _) ->
    let vis = map pcPage (St.visiblePieces g a)
        below = [ k | k <- stripPagesIn g a (sgViewH g) (stripNearHi g), k `notElem` vis ]
        above = reverse [ k | k <- stripPagesIn g a (stripNearLo g) 0, k `notElem` vis ]
    in below ++ [ k | k <- above, k `notElem` below ]
  Nothing -> []

-- | Is page @k@ one the page view keeps a scene for? (The driver asks, to
-- cancel a prefetch nobody needs any more.)
pdfPageNear :: Editor -> Int -> Bool
pdfPageNear ed k = case edPdfView ed of
  Just pv | pageStripOn ed -> k `elem` (stripVisiblePages ed ++ stripNearPages ed)
          | otherwise -> abs (k - pvPage pv) <= 1
  Nothing -> False

-- A page that has no scene yet and has not failed.
needsScene :: PdfView -> Int -> Bool
needsScene pv k = isNothing (lookup k (pvScenes pv)) && isNothing (lookup k (pvFailed pv))

-- | The bands to render, most wanted first: what is on screen (top to
-- bottom), then below the view, then above it; each with the scene to draw
-- it from. The driver runs the first few on its tile jobs.
pdfTileWants :: Editor -> [(TileKey, Scene)]
pdfTileWants ed = case stripState ed of
  Nothing -> []
  Just (_, pv, g, a, pan) ->
    let vh = sgViewH g
        order = stripTileKeys ed pv g a pan 0 vh
                ++ stripTileKeys ed pv g a pan vh (stripNearHi g)
                ++ reverse (stripTileKeys ed pv g a pan (stripNearLo g) 0)
        missing k = M.notMember k (pvTiles pv) && k `notElem` pvTileFailed pv
        dedup = go M.empty
          where go _ [] = []
                go seen (k : ks) | M.member k seen = go seen ks
                                 | otherwise = k : go (M.insert k () seen) ks
    in [ (k, psScene ps) | k <- dedup order, missing k, Just ps <- [lookup (tkPage k) (pvScenes pv)] ]

-- | A band arrived (driver callback). Kept when the view would still ask
-- for exactly it; a failure is remembered so it is not asked for again.
pdfTileRendered :: TileKey -> Maybe TileBits -> Editor -> Editor
pdfTileRendered k res ed = case stripState ed of
  Nothing -> ed
  Just (_, pv, g, a, pan)
    | k `elem` stripTileKeys ed pv g a pan (stripNearLo g) (stripNearHi g) -> case res of
        Just tb -> ed { edPdfView = Just pv { pvTiles = M.insert k tb (pvTiles pv) } }
        Nothing -> ed { edPdfView = Just pv { pvTileFailed = take 64 (k : pvTileFailed pv) } }
    | otherwise -> ed

-- A page scene for the fit-width layout: into the pool when the view is
-- near it, held to the neighbourhood within the memory budget (the pages on
-- screen are always kept: dropping one would re-parse it forever).
stripPageParsed :: PageWant -> Int -> Bool -> Either String (Scene, SceneSize, [String]) -> PdfView -> Editor -> Editor
stripPageParsed w sid fontless res pv ed = case res of
  Right (sc, sz, notes)
    | pwPage w `elem` vis || pwPage w `elem` near ->
        let -- drawn before font discovery finished, but the fonts are here
            -- now: not kept, so the page is simply asked for again (with them)
            redo = fontless && edFontsReady ed
            ps = PageScene sid sc notes fontless sz
            pool = [ (pwPage w, ps) | not redo ] ++ filter ((/= pwPage w) . fst) (pvScenes pv)
            kept = stripTrimScenes vis near pool
            lost = [ q | (q, _) <- pool, q `elem` near, isNothing (lookup q kept) ]
            pv1 = pv { pvScenes = kept, pvNoFit = lost ++ filter (`notElem` lost) (pvNoFit pv) }
            note = if null notes || pwPage w `notElem` vis then edStatus ed
                   else T.pack ("Page " ++ show (pwPage w) ++ ": " ++ intercalate "; " notes)
        in stripSync ed { edPdfView = Just pv1, edStatus = note }
    | otherwise -> ed
  Left err ->
    let pv1 = pv { pvFailed = (pwPage w, err) : filter ((/= pwPage w) . fst) (pvFailed pv) }
    in ed { edPdfView = Just pv1
          , edStatus = if pwPage w `elem` vis then T.pack ("Page " ++ show (pwPage w) ++ " could not be drawn: " ++ err)
                       else edStatus ed }
  where
    vis = stripVisiblePages ed
    near = stripNearPages ed

-- The fit-width layout's scene cache: every page on screen, then the
-- neighbours in order (below first) while they fit 'pageKeepBytes'.
stripTrimScenes :: [Int] -> [Int] -> [(Int, PageScene)] -> [(Int, PageScene)]
stripTrimScenes vis near pool =
  let onScreen = [ e | e@(p, _) <- pool, p `elem` vis ]
      used0 = sum [ szBytes (psSize ps) | (_, ps) <- onScreen ]
      pick _ [] = []
      pick used (q : qs) = case lookup q pool of
        Just ps | used + szBytes (psSize ps) <= pageKeepBytes -> (q, ps) : pick (used + szBytes (psSize ps)) qs
        _ -> pick used qs
  in onScreen ++ pick used0 [ q | q <- near, q `notElem` vis ]

-- | How many pages either side of the current one keep their characters
-- (a page's are a few hundred kilobytes; they are re-extracted on return).
glyphKeepPages :: Int
glyphKeepPages = 4

-- | The page whose characters the select tool needs and does not have: the
-- current page in fit page, the pages on screen in fit width. The driver
-- extracts them on a job of their own ('JPdfGlyphs'), after the pictures.
pdfGlyphWant :: Editor -> Maybe PageWant
pdfGlyphWant ed = do
  p <- edPath ed
  pv <- edPdfView ed
  if pageViewOn ed then Just () else Nothing
  let pages = if pageStripOn ed then stripVisiblePages ed else [pvPage pv]
  n <- listToMaybe [ k | k <- pages, IM.notMember k (pvGlyphs pv) ]
  pure (PageWant p (pvEpoch pv) n)

-- | Characters of a page arrived (driver callback).
pdfGlyphsArrived :: PageWant -> [Pdf.PdfGlyph] -> Editor -> Editor
pdfGlyphsArrived w gs ed = case edPdfView ed of
  Just pv | edPath ed == Just (pwPath w), pvEpoch pv == pwEpoch w ->
    let near k = abs (k - pvPage pv) <= glyphKeepPages || k `elem` stripVisiblePages ed
        pv1 = pv { pvGlyphs = IM.filterWithKey (\k _ -> near k) (IM.insert (pwPage w) gs (pvGlyphs pv)) }
        -- a selection made before this page's text was known takes it now
        pv2 = case pvSel pv1 of
          Just bs | any ((== pwPage w) . fst) (bsBoxes bs) -> pv1 { pvSel = Just (boxSelection pv1 (bsBoxes bs)) }
          _ -> pv1
    in ed { edPdfView = Just pv2 }
  _ -> ed

------------------------------------------------------------------------------
-- Moving the fit-width view

-- | Scroll by @d@ pixels of the layout (down when positive).
stripScroll :: Double -> Editor -> Editor
stripScroll d ed = case stripState ed of
  Just (_, _, g, a, pan) -> stripSet g (St.scrollAnchor g d a) pan ed
  Nothing -> ed

-- | Scroll by @n@ text rows.
stripScrollRows :: Double -> Editor -> Editor
stripScrollRows n ed = stripScroll (n * stripRowPx ed) ed

-- | Scroll by a screen, less two rows of overlap, so a line cut by the edge
-- is seen whole on the next one.
stripScrollScreen :: Int -> Editor -> Editor
stripScrollScreen dir ed = case stripState ed of
  Just (_, _, g, _, _) ->
    let step = max (stripRowPx ed) (fromIntegral (sgViewH g) - 2 * stripRowPx ed)
    in stripScroll (fromIntegral dir * step) ed
  Nothing -> ed

-- | Put the top of page @n@ (clamped) at the top of the view.
stripGoPage :: Int -> Editor -> Editor
stripGoPage n ed = case stripState ed of
  Just (_, pv, g, _, pan) -> stripSet g (max 1 (min (pvPages pv) n), 0) pan ed
  Nothing -> ed

-- | The end of the document, or its start.
stripGoEnd, stripGoStart :: Editor -> Editor
stripGoEnd ed = case stripState ed of
  Just (_, _, g, _, pan) -> stripSet g (St.maxAnchor g) pan ed
  Nothing -> ed
stripGoStart ed = case stripState ed of
  Just (_, _, g, _, pan) -> stripSet g (1, 0) pan ed
  Nothing -> ed

-- | Pan a column wider than the view by a fraction of the view's width.
stripPan :: Double -> Editor -> Editor
stripPan f ed = case stripState ed of
  Just (_, _, g, a, pan) -> stripSet g a (pan + round (f * fromIntegral (sgViewW g))) ed
  Nothing -> ed

-- | Can the column pan sideways (it is wider than the view)?
stripPannable :: Editor -> Bool
stripPannable ed = maybe False (\(_, _, g, _, _) -> St.panMax g > 0) (stripState ed)

-- | The fit-width layout's zoom bounds.
stripZoomMin, stripZoomMax :: Double
stripZoomMin = 0.2
stripZoomMax = 8

-- | Zoom to @z@ (clamped), holding still the page point under the layout
-- pixel @(x, y)@.
stripZoomTo :: Double -> (Double, Double) -> Editor -> Editor
stripZoomTo z0 (x, y) ed = case stripState ed of
  Just (_, pv, g, a, pan) ->
    let z = max stripZoomMin (min stripZoomMax z0)
        g' = g { sgZoom = z }
        set = stripSet g'
    in case St.pointToPage g pan a (x, y) of
         Just (k, (px, py)) ->
           let (sx, sy) = St.pieceScale g' k
               top = y - py * sy                      -- where page k's top goes
               cl = x - px * sx                       -- and its left edge
               pan' = if St.colWidth g' > sgViewW g' then round (negate cl) else 0
           in set (k, negate top) pan' (ed { edPdfView = Just pv { pvZoom = z } })
         Nothing -> set a 0 (ed { edPdfView = Just pv { pvZoom = z } })
  Nothing -> ed

-- | One zoom step (in or out) about a layout pixel.
stripZoomStep :: Bool -> (Double, Double) -> Editor -> Editor
stripZoomStep zin at ed = case edPdfView ed of
  Just pv -> stripZoomTo (if zin then pvZoom pv * wheelZoomStep else pvZoom pv / wheelZoomStep) at ed
  Nothing -> ed

-- | The middle of the view, in layout pixels.
stripCentre :: Editor -> (Double, Double)
stripCentre ed = case stripState ed of
  Just (_, _, g, _, _) -> (fromIntegral (sgViewW g) / 2, fromIntegral (sgViewH g) / 2)
  Nothing -> (0, 0)

-- | A text-area cell's centre in layout pixels.
stripCellCentre :: Editor -> Int -> Int -> (Double, Double)
stripCellCentre ed row col
  | stripPixel ed = let (cw, ch) = stripCellPx ed
                    in ((fromIntegral col + 0.5) * fromIntegral cw, (fromIntegral row + 0.5) * fromIntegral ch)
  | otherwise = (fromIntegral col + 0.5, 2 * fromIntegral row + 1)

-- | A text-area cell rectangle (inclusive corners) in layout pixels.
stripCellRect :: Editor -> (Int, Int, Int, Int) -> Rect
stripCellRect ed (ar, ac, br, bc) =
  let r0 = min ar br; r1 = max ar br; c0 = min ac bc; c1 = max ac bc
  in if stripPixel ed
       then let (cw, ch) = stripCellPx ed
            in Rect (fromIntegral (c0 * cw)) (fromIntegral (r0 * ch)) (fromIntegral ((c1 + 1) * cw)) (fromIntegral ((r1 + 1) * ch))
       else Rect (fromIntegral c0) (fromIntegral (2 * r0)) (fromIntegral (c1 + 1)) (fromIntegral (2 * r1 + 2))

-- | Zoom so that a cell rectangle the user dragged fills the view (by its
-- width or its height, whichever is tighter), centred.
stripZoomToBox :: (Int, Int, Int, Int) -> Editor -> Editor
stripZoomToBox box ed = case stripState ed of
  Just (_, pv, g, _, _) ->
    let Rect x0 y0 x1 y1 = stripCellRect ed box
        f = min (fromIntegral (sgViewW g) / max 1 (x1 - x0)) (fromIntegral (sgViewH g) / max 1 (y1 - y0))
        mid = ((x0 + x1) / 2, (y0 + y1) / 2)
        -- zoom about the box's centre, then bring that centre to the view's
        ed1 = stripZoomTo (pvZoom pv * f) mid ed
    in case stripState ed1 of
         Just (_, _, g1, a1, pan1) ->
           let (cx, cy) = stripCentre ed1
           in stripSet g1 (St.scrollAnchor g1 (snd mid - cy) a1) (pan1 + round (fst mid - cx)) ed1
         Nothing -> ed1
  Nothing -> ed

------------------------------------------------------------------------------
-- Switching layout and drag mode

-- | W / View ▸ Fit Width: switch the page view between one page fitted in
-- the window and the continuous fit-width column, staying on the same page.
togglePdfFit :: Editor -> Editor
togglePdfFit ed = case (edPdf ed, edPdfView ed) of
  (Just pd, Just pv) | pvOn pv -> case pvFit pv of
    PdfFitPage ->
      -- the page on screen joins the pool the column draws from
      let pic = pvPic pv
          leaving = case idVec pic of
            Just vd | Just sc <- vdScene vd, vdSeq vd == pvPage pv ->
              [(pvPage pv, PageScene (vdSceneId vd) sc (vdNotes vd) (vdFontless vd) (SceneSize (vdCost vd) (vdBytes vd)))]
            _ -> []
          pool = leaving ++ filter ((/= pvPage pv) . fst) (pvScenes pv)
          pv1 = pv { pvFit = PdfFitWidth, pvScenes = pool, pvAnchor = pvPage pv, pvOff = 0
                   , pvZoom = 1, pvPanX = 0, pvBox = Nothing
                     -- idle while the column shows: its scene is in the pool
                   , pvPic = pic { idCrop = Nothing, idDrag = Nothing, idCache = Nothing
                                 , idVec = fmap (\vd -> vd { vdScene = Nothing, vdCells = Nothing, vdSeq = -1 }) (idVec pic) } }
          ed1 = stripSync ed { edPdfView = Just pv1, edMouseSelecting = False }
      in ed1 { edStatus = "Fit width \x2014 scroll through the pages; W fits one page, +/- zoom" }
    PdfFitWidth ->
      let n = pvPage pv
          fresh = (pagePicture (pvEpoch pv) pd n) { idMode = idMode (pvPic pv) }
          pic = case lookup n (pvScenes pv) of
            Just ps -> fresh { idVec = fmap (installPageScene n ps) (idVec fresh) }
            Nothing -> fresh
          pv1 = pv { pvFit = PdfFitPage, pvPic = pic, pvTiles = M.empty, pvBox = Nothing
                   , pvScenes = trimPageCache n (filter ((/= n) . fst) (pvScenes pv)) }
      in refreshImage ed { edPdfView = Just pv1, edMouseSelecting = False, edStatus = "Fit page \x2014 PgUp/PgDn turn pages; W for the continuous fit-width view" }
  _ -> ed { edStatus = "Fit width is for the PDF page view" }

-- | S / View ▸ Drag Selects Text: what dragging a box over the page view does.
togglePdfDrag :: Editor -> Editor
togglePdfDrag ed = case edPdfView ed of
  Just pv | pageViewOn ed ->
    let d = if pvDrag pv == PdfDragZoom then PdfDragSelect else PdfDragZoom
    in ed { edPdfView = Just pv { pvDrag = d, pvBox = Nothing, pvPic = (pvPic pv) { idDrag = Nothing } }
          , edMouseSelecting = False
          , edStatus = case d of
              PdfDragSelect -> "Drag selects text \x2014 draw a box over the text, Ctrl+C copies it; S to zoom by dragging"
              PdfDragZoom -> "Drag zooms \x2014 draw a box to zoom into it; S to select text by dragging" }
  _ -> ed { edStatus = "Selecting by box is for the PDF page view" }

------------------------------------------------------------------------------
-- The select tool: a box over the page view, its text, its highlight

-- | The page-point rectangles a text-area cell box covers, per page, in
-- whichever layout is showing.
pdfBoxPages :: Editor -> (Int, Int, Int, Int) -> [(Int, Rect)]
pdfBoxPages ed box = case stripState ed of
  Just (_, _, g, a, pan) -> St.rectToPages g pan a (stripCellRect ed box)
  Nothing -> case (edPdfView ed, activePicture ed) of
    (Just pv, Just pic) | pageViewOn ed, Just vd <- idVec pic, isJust (vdScene vd) ->
      let (ar, ac, br, bc) = box
          r0 = min ar br; r1 = max ar br; c0 = min ac bc; c1 = max ac bc
          Rect sx0 sy0 sx1 sy1 = picCellRectToScene ed pic (r0, c0, r1, c1)
      in if sx1 > sx0 && sy1 > sy0 then [(pvPage pv, Rect sx0 sy0 sx1 sy1)] else []
    _ -> []

-- | A cell rectangle of the fitted page picture, in its scene's points
-- (the inverse of the fit the cell picture and drag-to-zoom use).
picCellRectToScene :: Editor -> ImageDoc -> (Int, Int, Int, Int) -> Rect
picCellRectToScene ed pic (r0, c0, r1, c1) =
  let lo = computeLayout ed
      cols = loTextWidth lo; rows = loTextHeight lo
      (cx, cy, cw, ch) = imageCrop pic
      (outW, outH, offX, offY) = viewFit (cellAspect ed) (imageFitCap ed pic) cols rows cw ch
      (vw, vh) = imageDims pic
      (sw, sh) = maybe (1, 1) vdSize (idVec pic)
      fx k = (fromIntegral cx + fromIntegral (k - offX) * fromIntegral cw / fromIntegral (max 1 outW)) * sw / fromIntegral (max 1 vw)
      fy k = (fromIntegral cy + fromIntegral (k - offY) * fromIntegral ch / fromIntegral (max 1 outH)) * sh / fromIntegral (max 1 vh)
      clampX = max 0 . min sw; clampY = max 0 . min sh
  in Rect (clampX (fx c0)) (clampY (fy (2 * r0))) (clampX (fx (c1 + 1))) (clampY (fy (2 * r1 + 2)))

-- | The fitted page picture's mapping from scene points to text-area
-- sub-pixels (the cell picture's grid: @cols × 2·rows@), for drawing a
-- highlight over it.
picSceneToSub :: Editor -> ImageDoc -> Rect -> Rect
picSceneToSub ed pic (Rect x0 y0 x1 y1) =
  let lo = computeLayout ed
      cols = loTextWidth lo; rows = loTextHeight lo
      (cx, cy, cw, ch) = imageCrop pic
      (outW, outH, offX, offY) = viewFit (cellAspect ed) (imageFitCap ed pic) cols rows cw ch
      (vw, vh) = imageDims pic
      (sw, sh) = maybe (1, 1) vdSize (idVec pic)
      gx x = fromIntegral offX + (x * fromIntegral vw / sw - fromIntegral cx) * fromIntegral outW / fromIntegral (max 1 cw)
      gy y = fromIntegral offY + (y * fromIntegral vh / sh - fromIntegral cy) * fromIntegral outH / fromIntegral (max 1 ch)
  in Rect (gx x0) (gy y0) (gx x1) (gy y1)

-- | The selection the boxes take: every character whose centre is inside,
-- per page, with its text and one highlight rectangle per line. A page whose
-- characters have not arrived yet contributes its box and no text (they are
-- folded in when they come: 'pdfGlyphsArrived').
boxSelection :: PdfView -> [(Int, Rect)] -> PdfBoxSel
boxSelection pv boxes =
  let per = [ (k, Pdf.pdfGlyphsIn r (IM.findWithDefault [] k (pvGlyphs pv))) | (k, r) <- boxes ]
      texts = [ t | (_, gs) <- per, not (null gs), let t = Pdf.pdfGlyphsText gs, not (T.null t) ]
  in PdfBoxSel { bsBoxes = boxes
               , bsRects = [ (k, r) | (k, gs) <- per, r <- Pdf.pdfGlyphRects gs ]
               , bsText = T.intercalate "\n\n" texts }

-- | Select every character of the page the view is on (Ctrl+A).
pdfSelectPage :: Editor -> Editor
pdfSelectPage ed = case (edPdf ed, edPdfView ed) of
  (Just pd, Just pv) | pageViewOn ed ->
    let n = pvPage pv
        (w, h) = pdfPageSize pd n
        bs = boxSelection pv [(n, Rect (-1) (-1) (w + 1) (h + 1))]
    in ed { edPdfView = Just pv { pvSel = Just bs }, edStatus = selNote n bs }
  _ -> ed
  where
    selNote n bs
      | T.null (bsText bs) = T.pack ("Page " ++ show n ++ " has no text to select" ++ loadingNote n)
      | otherwise = T.pack ("Selected page " ++ show n ++ ": " ++ show (T.length (bsText bs)) ++ " characters \x2014 Ctrl+C copies")
    loadingNote n = case edPdfView ed of
      Just pv | IM.notMember n (pvGlyphs pv) -> " yet (still loading)"
      _ -> ""

-- | Ctrl+C in the page view: copy the box selection's text.
pdfCopyBox :: Editor -> (Editor, [Effect])
pdfCopyBox ed = case edPdfView ed >>= pvSel of
  Just bs | not (T.null (bsText bs)) ->
    let t = detach (bsText bs)
    in (ed { edClipboard = t, edStatus = T.pack ("Copied " ++ show (T.length t) ++ " characters") }, [EffCopy t])
  Just _ -> (ed { edStatus = "No text in the selected box" }, [])
  Nothing -> (ed { edStatus = case fmap pvDrag (edPdfView ed) of
                     Just PdfDragSelect -> "Drag a box over the text to select it, then Ctrl+C (Ctrl+A takes the whole page)"
                     _ -> "Press S, then drag a box over the text to select it (Ctrl+A takes the whole page); or Alt+T for the reflow view" }, [])

-- | Mouse press / drag / release of a box over the page view (select mode in
-- either layout, and zoom mode in the fit-width one; the fit-page zoom drag
-- is the image view's own). Coordinates are text-area cells.
pdfBoxMouse :: MouseEvent -> Editor -> Editor
pdfBoxMouse me ed = case edPdfView ed of
  Nothing -> ed
  Just pv
    | mePressed me && not (meDrag me) && meButton me == MBLeft ->
        if inArea then put pv { pvBox = Just (row, col, row, col) } ed { edMouseSelecting = True } else ed
    | meDrag me, Just (ar, ac, _, _) <- pvBox pv ->
        let box = (ar, ac, clampR row, clampC col)
            pv1 = pv { pvBox = Just box }
        in if pvDrag pv == PdfDragSelect
             then put pv1 { pvSel = Just (boxSelection pv1 (pdfBoxPages ed box)) } ed
             else put pv1 ed
    | not (mePressed me), Just box@(ar, ac, br, bc) <- pvBox pv ->
        let moved = (ar, ac) /= (br, bc)
            ed1 = ed { edMouseSelecting = False }
            pv1 = pv { pvBox = Nothing }
        in case pvDrag pv of
             PdfDragSelect
               | moved ->
                   let bs = boxSelection pv1 (pdfBoxPages ed box)
                       missing = [ k | (k, _) <- bsBoxes bs, IM.notMember k (pvGlyphs pv1) ]
                       msg | not (T.null (bsText bs)) = T.pack ("Selected " ++ show (T.length (bsText bs)) ++ " characters \x2014 Ctrl+C copies")
                           | not (null missing) = "The page's text is still loading \x2014 it will be selected when it arrives"
                           | null (bsBoxes bs) = "The box is not over a page"
                           | otherwise = "No text in the box (a picture or a scan has none to copy)"
                   in put pv1 { pvSel = Just bs } ed1 { edStatus = msg }
               | otherwise -> put pv1 { pvSel = Nothing } ed1    -- a click drops the selection
             PdfDragZoom
               | moved && abs (br - ar) >= 1 && abs (bc - ac) >= 1 -> stripZoomToBox box (put pv1 ed1)
               | pvZoom pv /= 1 -> stripZoomTo 1 (stripCellCentre ed row col) (put pv1 ed1)
               | otherwise -> put pv1 ed1
    | otherwise -> ed
  where
    lo = computeLayout ed
    rows = loTextHeight lo; cols = loTextWidth lo
    row = meRow me - loTextTop lo
    col = meCol me - loTextLeft lo
    inArea = row >= 0 && row < rows && col >= 0 && col < cols
    clampR r = max 0 (min (rows - 1) r)
    clampC c = max 0 (min (cols - 1) c)
    put pv' e = e { edPdfView = Just pv' }

-- | The highlights over the page view, in page points: the selected lines
-- (fills), each with its page. The rubber band of a drag in progress is in
-- cells ('pvBox') and drawn by the caller.
pdfSelHighlights :: Editor -> [(Int, Rect)]
pdfSelHighlights ed = case edPdfView ed of
  Just pv | pageViewOn ed, Just bs <- pvSel pv -> bsRects bs
  _ -> []

-- | The colour and opacity of a text selection over a page, and of the
-- rubber band.
selFillRGB, bandRGB :: (Word8, Word8, Word8)
selFillRGB = (0x2F, 0x7C, 0xF6)
bandRGB = (0x2F, 0x7C, 0xF6)

-- | How wide the rubber band's line is in device pixels: a fine line, one
-- pixel on an ordinary display and two on a dense one (big cells).
bandWidth :: Editor -> Int
bandWidth ed = if fst (stripCellPx ed) >= 14 then 2 else 1

-- | Every highlight over the fit-width layout, in its pixels.
stripHighlights :: Editor -> [HlRect]
stripHighlights ed = case stripState ed of
  Nothing -> []
  Just (_, pv, g, a, pan) ->
    let pieces = St.piecesIn g a 0 (sgViewH g)
        fills = [ HlRect (St.pageRectToScreen g pan pc r) selFillRGB 0.35 0
                | (k, r) <- pdfSelHighlights ed, pc <- pieces, pcPage pc == k ]
        band = case pvBox pv of
          Just box | (\(ar, ac, br, bc) -> (ar, ac) /= (br, bc)) box ->
            [ HlRect (stripCellRect ed box) bandRGB 0.9 (bandWidth ed) ]
          _ -> []
    in fills ++ band

-- | Highlights over the fitted page picture, in the cell picture's
-- sub-pixels (fills only; the rubber band is cells).
picHighlightsSub :: Editor -> [Rect]
picHighlightsSub ed = case (edPdfView ed, activePicture ed) of
  (Just pv, Just pic) | pageViewOn ed ->
    [ picSceneToSub ed pic r | (k, r) <- pdfSelHighlights ed, k == pvPage pv ]
  _ -> []

-- | The fit-width layout's cell picture: the bands sampled into half-block
-- cells (a pixel terminal's device-pixel bands too, when a menu or dialog
-- covers the overlay), unrendered pages as paper, the gaps as background,
-- selections tinted.
stripCellGrid :: Editor -> Maybe (Array (Int, Int) Cell)
stripCellGrid ed = case stripState ed of
  Nothing -> Nothing
  Just (_, pv, g, a, pan) ->
    let lo = computeLayout ed
        tw = loTextWidth lo; th = loTextHeight lo
        pieces = St.piecesIn g a 0 (sgViewH g)
        -- each page's bands, worked out once per picture, not per sub-pixel
        byPage = IM.fromListWith (flip (++))
                   [ (pcPage pc, St.tilesFor g pan (not (stripPixel ed)) (stripSceneOf pv) pc 0 (sgViewH g)) | pc <- pieces ]
        keysOf k = IM.findWithDefault [] k byPage
        hls = [ hr | hr <- stripHighlights ed, hrOutline hr == 0 ]
        (cw, ch) = stripCellPx ed
        toRaster sx sy
          | stripPixel ed = ((fromIntegral sx + 0.5) * fromIntegral cw, (fromIntegral sy + 0.5) * fromIntegral ch / 2)
          | otherwise = (fromIntegral sx + 0.5, fromIntegral sy + 0.5)
        sample sx sy = let (x, y) = toRaster sx sy
                       in St.stripSample g pan pieces (\k -> M.lookup k (pvTiles pv)) keysOf hls (x, y)
    in if tw <= 0 || th <= 0 then Nothing
       else Just (St.cellsFrom (idMode (pvPic pv)) tw th sample)

-- Cycle to the next open file, wrapping around at the end.
nextFile :: Editor -> Editor
nextFile ed
  | fileCount ed <= 1 = ed { edStatus = "No other open files" }
  | otherwise = let !parked = parkDoc ed in case edAfter ed of
      (d : ds) -> switchTo d ed { edBefore = edBefore ed ++ [parked], edAfter = ds }
      [] -> case edBefore ed of      -- wrap to the first file
        (d : ds) -> switchTo d ed { edBefore = [], edAfter = ds ++ [parked] }
        []       -> ed

prevFile :: Editor -> Editor
prevFile ed
  | fileCount ed <= 1 = ed { edStatus = "No other open files" }
  | otherwise = let !parked = parkDoc ed in case reverse (edBefore ed) of
      (d : rb) -> switchTo d ed { edBefore = reverse rb, edAfter = parked : edAfter ed }
      [] -> case reverse (edAfter ed) of   -- wrap to the last file
        (d : ra) -> switchTo d ed { edBefore = parked : reverse ra, edAfter = [] }
        []       -> ed

switchTo :: Document -> Editor -> Editor
switchTo d ed = (restoreDoc d ed) { edStatus = T.pack ("File " ++ fileLabel d) }

fileLabel :: Document -> String
fileLabel d = maybe "untitled" takeFileName (docPath d)

-- | Whether "Revert" should be offered: the active file has a path on disk and
-- either has unsaved edits or has changed on disk since we loaded/saved it.
revertAvailable :: Editor -> Bool
revertAvailable ed = isJust (edPath ed) && (edModified ed || edDiskChanged ed)

-- | Switch directly to the open file at index @k@ (0-based, in open order).
switchToFile :: Int -> Editor -> Editor
switchToFile k ed
  | k < 0 || k >= fileCount ed = ed
  | k == length (edBefore ed)  = ed            -- already active
  | otherwise =
      let !parked = parkDoc ed
          allDocs = edBefore ed ++ [parked] ++ edAfter ed
          target  = allDocs !! k
      in (restoreDoc target ed { edBefore = take k allDocs, edAfter = drop (k + 1) allDocs })
           { edStatus = T.pack ("File " ++ fileLabel target) }

-- | The driver calls this after a successful save, passing the file's new
-- on-disk modification time.
onSaved :: Int -> Maybe DiskTime -> Editor -> (Editor, [Effect])
onSaved bytes mtime ed0 =
  let ed   = case edPath ed0 of    -- freshly saved = recently used (covers Save As)
               Just p  -> recordRecent p (activeCursorPos ed0) ed0
               Nothing -> ed0
      name = fromMaybe "file" (edPath ed)
      ed1  = ed { edModified = False
                , edSavedBuffer = edBuffer ed
                , edSavedEol = edLineEnding ed  -- the save wrote these; they are the new baseline
                , edSavedEnc = edEncoding ed
                , edDiskMtime = mtime           -- new baseline; we just wrote it
                , edDiskChanged = False
                , edCsv = fmap Csv.markSaved (edCsv ed)   -- table's saved point too
                -- Break edit coalescing so the next keystroke starts a new undo
                -- checkpoint; otherwise undo would skip past the just-saved state.
                , edLastEdit = EKNone
                , edStatus = T.pack ("Saved " ++ name ++ " (" ++ show bytes ++ " bytes)") }
  in if edQuitting ed1
       then noEff (quitStep ed1)                       -- continue the quit sequence
       else if edPendingClose ed1
         then noEff (doClose (ed1 { edPendingClose = False }))
         -- Re-lint now that the file is on disk (save-time-only tools too).
         else (ed1, [EffSetTitle (windowTitle ed1), EffLintNow])

-- | Report an IO error to the user.
setError :: String -> Editor -> Editor
setError msg ed = (clearQuitState ed)
                     { edDialog = Just (mkMessage "Error" (T.pack msg))
                     , edFocus = FDialog
                     , edPendingClose = False }

------------------------------------------------------------------------------
-- Recent files

-- | Move a path to the front of the recent-files list, keeping any stored
-- cursor position (a fresh entry starts at the origin). Returns the editor
-- unchanged (same list object) when the path is already at the front, so the
-- driver's pointer check can skip persisting.
touchRecent :: FilePath -> Editor -> Editor
touchRecent path ed = case edRecent ed of
  (e : _) | rePath e == path -> ed
  entries ->
    let old = [ e | e <- entries, rePath e == path ]
        entry = case old of (e : _) -> e; [] -> RecentEntry path 0 0
    in ed { edRecent = take maxRecentEntries
                         (entry : filter ((/= path) . rePath) entries) }

-- | Move a path to the front of the recent-files list with a fresh cursor
-- position (recorded when a file is closed or saved).
recordRecent :: FilePath -> Pos -> Editor -> Editor
recordRecent path (Pos l c) ed =
  ed { edRecent = take maxRecentEntries
                    (RecentEntry path l c
                     : filter ((/= path) . rePath) (edRecent ed)) }

-- | Restore the remembered cursor position for a freshly-loaded file, if the
-- recents list has one that still fits the buffer.
restoreRecentPos :: FilePath -> Editor -> Editor
restoreRecentPos path ed = case [ e | e <- edRecent ed, rePath e == path ] of
  (e : _) | reLine e > 0 || reCol e > 0 ->
    let pos = clampPos (Pos (reLine e) (reCol e)) (edBuffer ed)
        line = getLine' (posLine pos) (edBuffer ed)
    in ensureVisible ed { edCursor = pos
                        , edDesiredCol = colToDisplay (tabWidthOf ed) (posCol pos) line }
  _ -> ed

-- | The recents list with the live cursor positions of still-open documents
-- folded in — what the driver writes to disk, so quitting records where the
-- cursor was in every open file.
recentsForPersist :: Editor -> [RecentEntry]
recentsForPersist ed = map overlay (edRecent ed)
  where
    openPos = [ (p, docCursorPos d) | d <- allOpenDocs ed, Just p <- [docPath d] ]
    overlay e = case lookup (rePath e) openPos of
      Just (Pos l c) -> e { reLine = l, reCol = c }
      Nothing        -> e

------------------------------------------------------------------------------
-- Session restore: what is recorded, and where a restored cursor lands
--
-- The driver owns the file ("Cmedit.App"); this is the question it asks and
-- the answer it installs. Only paths are recorded — an untitled buffer has
-- nothing to reopen, and what it *contains* is the crash journal's job, which
-- is why the two features compose rather than overlap.

-- | The documents the session file records: their index in the zipper, their
-- path and the document itself, in open order.
--
-- Shared by 'sessionForPersist' and 'sessionShape' so the two cannot disagree
-- about /which/ documents are recorded — and, deliberately, carrying the
-- 'Document' rather than a finished 'RecentEntry', so that the shape can be
-- computed without ever asking a document where its cursor is. See
-- 'sessionShape'.
--
-- The manual's @cmedit:\/\/@ pseudo-path is excluded for the same reason the
-- recents exclude it: there is no such file to reopen.
sessionDocs :: Editor -> [(Int, FilePath, Document)]
sessionDocs ed =
  [ (i, p, d)
  | (i, d) <- zip [0 :: Int ..] (allOpenDocs ed)
  , Just p <- [docPath d]
  , not ("cmedit://" `isPrefixOf` p) ]

-- | Which recorded document is the active one. Counting the recorded documents
-- ahead of the active one maps its position in the zipper onto its position in
-- the list we write; when the active document is itself unrecorded (untitled),
-- that lands on the next one, which is the closest thing a restore can honour.
sessionActiveIx :: Editor -> [(Int, FilePath, Document)] -> Int
sessionActiveIx ed recorded =
  min (max 0 (length recorded - 1)) (length [ () | (i, _, _) <- recorded, i < here ])
  where here = length (edBefore ed)

-- | The session as it should be persisted: the open folder, every open
-- document that has a real path (in open order, with live cursor positions and
-- the on-disk baseline each was loaded\/saved at), and which of them is active.
--
-- The per-file mtime costs __no extra stats__: every document already carries
-- 'docDiskMtime', recorded by @loadFile@\/@saveFile@ and refreshed by the
-- driver's existing freshness pass. @closed:@ is filled in by the driver, which
-- is the only side that can read a clock.
sessionForPersist :: Editor -> Session
sessionForPersist ed = Session
  { seFolder = explorerRoot ed
  , seFiles  = take maxSessionFiles
                 [ SessionFile p (posLine pos) (posCol pos)
                     (J.diskTimeToPicos <$> docDiskMtime d)
                 | (_, p, d) <- recorded, let pos = docCursorPos d ]
  , seActive = sessionActiveIx ed recorded
  , seClosed = Nothing
  }
  where recorded = sessionDocs ed

-- | Fingerprint of the session's /shape/ — everything the file records apart
-- from cursor positions. The driver rewrites when this moves, exactly as it
-- does for the recents: opening, closing and switching files are session
-- changes; moving the cursor is not.
--
-- It is computed from the paths directly, and that is load-bearing rather than
-- tidy. It used to project the shape out of a whole 'sessionForPersist' — which
-- reads as free, since the positions are dropped — but 'RecentEntry' has strict
-- fields, so building one forces the position it was about to discard, and
-- 'docCursorPos' on a table document is a walk over the rows above the cursor.
-- The driver asks for this after /every/ key batch, so that put ~390 ms of
-- re-serialisation on each keystroke typed into the last row of a large CSV
-- (plan 0029). Nothing here may force a 'docCursorPos'.
sessionShape :: Editor -> (Maybe FilePath, [FilePath], Int)
sessionShape ed =
  ( explorerRoot ed
  , take maxSessionFiles [ p | (_, p, _) <- recorded ]
  , sessionActiveIx ed recorded )
  where recorded = sessionDocs ed

-- | Put a restored document's cursor where the session recorded it, clamped to
-- the buffer as it is now — the file may have been edited (or truncated) by
-- something else since. Same shape as 'restoreRecentPos', but addressing a
-- named document rather than the active one, because a restore opens several
-- files before any of them is looked at. Views with no text cursor (CSV,
-- image, pager, PDF, container-derived) are left alone.
seedSessionPos :: FilePath -> Pos -> Editor -> Editor
seedSessionPos path want ed
  | edPath ed == Just path, isPlainDoc (captureDoc ed) =
      let (pos, dc) = seatIn (edBuffer ed)
      in ensureVisible ed { edCursor = pos, edDesiredCol = dc }
  | otherwise = ed { edBefore = map upd (edBefore ed), edAfter = map upd (edAfter ed) }
  where
    seatIn buf = let p = clampPos want buf
                     ln = getLine' (posLine p) buf
                 in (p, colToDisplay (tabWidthOf ed) (posCol p) ln)
    -- docTop is left at 0: restoreDoc runs ensureVisible when the document is
    -- switched to, so the window is chosen against the size it will be shown at.
    upd d | docPath d == Just path, isPlainDoc d =
              let (p, dc) = seatIn (docBuffer d) in d { docCursor = p, docDesiredCol = dc }
          | otherwise = d

-- | How many recent files the File menu offers.
recentMenuMax :: Int
recentMenuMax = 6

-- | The recent files shown in the File menu: the most recent ones that are not
-- already open (open files are reachable from the Window menu).
recentMenuPaths :: Editor -> [FilePath]
recentMenuPaths ed =
  take recentMenuMax
    [ rePath e | e <- edRecent ed, isNothing (findOpenIndex (rePath e) ed) ]

-- Menu entries for the recent files, numbered &1..&6 with over-long paths
-- elided from the left (the filename end is the identifying part).
recentMenuEntries :: Editor -> [MenuEntry]
recentMenuEntries ed =
  [ MEItem (T.pack ("&" ++ show (k + 1 :: Int) ++ " " ++ elide p)) "" (MARecentFile k)
  | (k, p) <- zip [0 ..] (recentMenuPaths ed) ]
  where
    maxW = 44
    elide p | length p <= maxW = p
            | otherwise        = "\x2026" ++ drop (length p - maxW + 1) p

-- Splice the recent-files section into the File menu, just above the
-- Settings…/Exit block at the bottom (dropping a duplicate separator when
-- the static menu already ends its upper section with one).
addRecentEntries :: Editor -> [MenuEntry] -> [MenuEntry]
addRecentEntries ed es = spliceAboveTail (recentMenuEntries ed) es

-- The splice both dynamic File-menu sections use: insert above the
-- Settings…/Exit tail, separated. Applying it twice (sessions, then recents)
-- leaves the second section /below/ the first, which is why 'entriesFor' adds
-- the sessions first.
spliceAboveTail :: [MenuEntry] -> [MenuEntry] -> [MenuEntry]
spliceAboveTail [] es = es
spliceAboveTail rs es = case break isTail es of
  (pre, tl@(_ : _)) -> dropTrailingSep pre ++ [MESep] ++ rs ++ [MESep] ++ tl
  (_, [])           -> es ++ [MESep] ++ rs
  where
    isTail (MEItem _ _ MAExit)     = True
    isTail (MEItem _ _ MASettings) = True
    isTail _                       = False
    dropTrailingSep xs = case reverse xs of
      (MESep : r) -> reverse r
      _           -> xs

------------------------------------------------------------------------------
-- The File menu's recent sessions (plan 0030 §2.4)

-- | How many past sessions the File menu offers.
sessionMenuMax :: Int
sessionMenuMax = 4

-- | Driver callback for 'EffListSessions'.
sessionsListed :: [SessionSummary] -> Editor -> Editor
sessionsListed sums ed = ed { edSessions = sums }

-- | The folder-backed sessions the File menu offers, in the order it offers
-- them — newest-'sumClosed' first, at most 'sessionMenuMax'. A folderless
-- session is still available to startup restore, but there is no project
-- folder for its menu entry to reopen, so it is omitted here.
--
-- The __live session's own key is excluded__
-- (offering to restore what is already open is noise), and the key is a
-- function of the folder, so comparing folders /is/ comparing keys. And the
-- list is __de-duplicated by folder__ so a legacy session that names a folder
-- now owning its own file does not appear twice — sorted first, so the newer
-- @closed:@ is the one kept.
--
-- 'MARestoreSession' addresses this list by index, so the label and the action
-- cannot disagree.
sessionMenuList :: Editor -> [SessionSummary]
sessionMenuList ed =
  take sessionMenuMax (dedup [] (sortBy newestFirst others))
  where
    others = [ s | s <- edSessions ed
                 , isJust (sumFolder s)
                 , sumFolder s /= explorerRoot ed ]
    -- Descending, so an unstamped v1 file (Nothing, which sorts first
    -- ascending) lands last, which is what it deserves.
    newestFirst = comparing (Down . sumClosed)
    dedup _ [] = []
    dedup seen (s : rest)
      | sumFolder s `elem` seen = dedup seen rest
      | otherwise               = s : dedup (sumFolder s : seen) rest

-- | One session's menu label: the folder's __basename__ and the recorded file
-- count, elided from the left past @maxW@ exactly as 'recentMenuEntries' elides
-- paths. The path is what disambiguates two sessions, but two simultaneously
-- listed sessions with the same basename is rare enough to answer with the
-- status line after the fact rather than with 44 columns of menu.
--
-- @&@ is stripped from the basename because it is this menu's mnemonic markup:
-- a folder called @a&b@ must not silently claim @b@ as an accelerator.
sessionMenuLabel :: SessionSummary -> Text
sessionMenuLabel s = T.pack (elide (name ++ " (" ++ show n ++ " file" ++ plural n ++ ")"))
  where
    n = sumCount s
    name = case sumFolder s of
      Nothing -> "(no folder)"
      Just f  -> case filter (/= '&') (takeFileName f) of
        "" -> filter (/= '&') f      -- a root path: "/" has no basename
        b  -> b
    maxW = 44
    elide p | length p <= maxW = p
            | otherwise        = "\x2026" ++ drop (length p - maxW + 1) p

sessionMenuEntries :: Editor -> [MenuEntry]
sessionMenuEntries ed =
  [ MEItem (sessionMenuLabel s) "" (MARestoreSession k)
  | (k, s) <- zip [0 ..] (sessionMenuList ed) ]

-- | Splice the recent-sessions section into the File menu. Files and sessions
-- are both \"things I had open\"; sessions are the coarser unit, so they read
-- better first — which 'entriesFor' arranges by adding them before the recents.
addSessionEntries :: Editor -> [MenuEntry] -> [MenuEntry]
addSessionEntries ed es = spliceAboveTail (sessionMenuEntries ed) es

-- | Give each session row the first letter of its label as a mnemonic, when no
-- other entry in the same dropdown has claimed it.
--
-- A post-pass over the /finished/ entry list, which is exactly where it can see
-- what is taken. Sessions cannot take digits: 'recentMenuEntries' numbers
-- @&1@..@&6@ and 'mnemonicItemIn' returns the __first__ match, so a second
-- numbered list would give the user two @1@s; continuing the sequence is worse,
-- because 'recentMenuPaths' filters out already-open files and every session's
-- digit would move as files are opened.
--
-- A session whose initial is taken, a digit or a non-letter simply renders
-- without an underline and stays reachable by arrows and mouse. That is a
-- graceful floor rather than a hole: nothing else in the menu depends on a
-- mnemonic existing.
assignSessionMnemonics :: [MenuEntry] -> [MenuEntry]
assignSessionMnemonics es = go taken0 es
  where
    taken0 = [ c | MEItem lbl _ a <- es, not (isSessionEntry a)
                 , Just c <- [mnemonicChar lbl] ]
    isSessionEntry (MARestoreSession _) = True
    isSessionEntry _                    = False
    go _ [] = []
    go taken (e@(MEItem lbl acc a) : rest)
      | isSessionEntry a = case initialOf lbl of
          Just c | c `notElem` taken ->
            MEItem ("&" <> lbl) acc a : go (c : taken) rest
          _ -> e : go taken rest
    go taken (e : rest) = e : go taken rest
    initialOf lbl = case T.uncons lbl of
      Just (c, _) | isAlpha c && fromEnum c < 128 -> Just (toLower c)
      _                                          -> Nothing

save :: Editor -> (Editor, [Effect])
save ed
  | edReadOnly ed = noEff ed { edStatus = "File is read-only \x2014 Save As (Ctrl+Shift+S) to write a copy" }
  | otherwise = case edPath ed of
      Just p  -> (ed, [EffSaveTo p])
      Nothing -> noEff (saveAsDialogFlow ed)

-- | Save every open document that has unsaved changes (File ▸ Save All). Asks
-- for confirmation first, since it writes several files at once. Docs without a
-- path (untitled) can't be batch-saved and are left for a manual Save.
saveAll :: Editor -> (Editor, [Effect])
saveAll ed
  | not (anyDocModified ed) = noEff ed { edStatus = "No unsaved changes" }
  | otherwise =
      let n = length (filter docModified (allOpenDocs ed))
      in noEff (openDialog (mkConfirm DKConfirmSaveAll "Save All"
           (T.pack ("Save " ++ show n ++ " file" ++ plural n ++ " with unsaved changes?"))
           ["Save All", "Cancel"]) ed)

anyDocModified :: Editor -> Bool
anyDocModified ed = edModified ed || any docModified (edBefore ed ++ edAfter ed)

-- Revert: reload the active file from disk, discarding unsaved edits. Prompts
-- for confirmation when there are unsaved changes; reloads straight away when
-- the only reason Revert is offered is that the file changed underneath us.
revert :: Editor -> (Editor, [Effect])
revert ed = case edPath ed of
  Nothing -> noEff ed { edStatus = "Nothing to revert" }
  Just p
    | edModified ed -> noEff (openDialog (mkConfirm DKConfirmRevert "Revert"
        (T.pack ("Discard unsaved changes and reload " ++ takeFileName p ++ "?"))
        ["Revert", "Cancel"]) ed)
    | otherwise     -> (ed, [EffRevert p])

-- Beyond this many unsaved files, quit asks once ("Save all / Discard all")
-- instead of stepping through them one dialog at a time.
quitBulkThreshold :: Int
quitBulkThreshold = 8

-- Quitting: quit immediately when nothing is unsaved; step through a handful of
-- unsaved files one prompt at a time; but for a large batch (> the threshold),
-- ask once whether to save them all or discard them all.
quit :: Editor -> (Editor, [Effect])
quit ed0 =
  let ed = clearQuitState ed0   -- start from a clean slate (no stale "discard" marks)
      nUnsaved = length (filter docUnsaved (allOpenDocs ed))
  in if nUnsaved == 0
       then (ed { edQuit = True }, [])
     else if nUnsaved > quitBulkThreshold
       then noEff (openDialog (mkConfirm DKConfirmQuitAll "Unsaved Changes"
              (T.pack (show nUnsaved ++ " files have unsaved changes."))
              ["Save All", "Discard All", "Cancel"]) ed { edQuitting = True })
       else noEff (quitStep ed { edQuitting = True })

allOpenDocs :: Editor -> [Document]
allOpenDocs ed = edBefore ed ++ [captureDoc ed] ++ edAfter ed

docUnsaved :: Document -> Bool
docUnsaved d = docModified d && not (docDiscard d)

-- Switch to the next file with unsaved changes and prompt; if none remain,
-- actually quit.
quitStep :: Editor -> Editor
quitStep ed = case findIndex docUnsaved (allOpenDocs ed) of
  Nothing -> (clearQuitState ed) { edQuit = True }
  Just k  ->
    let ed1  = switchToFile k ed
        name = maybe "untitled" takeFileName (edPath ed1)
    in openDialog (mkConfirm DKConfirmQuit "Unsaved Changes"
         (T.pack ("Save changes to " ++ name ++ "?")) ["Save", "Don't Save", "Cancel"]) ed1

-- Clear the in-progress quit flags and any "don't save" marks (on cancel, or
-- once the sequence completes).
clearQuitState :: Editor -> Editor
clearQuitState ed = ed
  { edQuitting = False
  , edDiscard = False
  , edBefore = map (\d -> d { docDiscard = False }) (edBefore ed)
  , edAfter  = map (\d -> d { docDiscard = False }) (edAfter ed)
  }

-- New always opens a fresh buffer in its own window, leaving any current file
-- open (so nothing is discarded); a pristine empty buffer is reused in place.
newFileFlow :: Editor -> Editor
newFileFlow ed
  | isPristine ed = doNew ed
  | otherwise     = doNew (pushActive ed)

-- The one place (besides 'newEditor') an untitled buffer is born, so the one
-- place a fresh document id must be handed out: an untitled journal is named
-- @untitled-\<id\>@, and reusing the id of a buffer still open in the zipper
-- would point two documents at one journal file.
doNew :: Editor -> Editor
doNew ed0 = ensureVisible ed
  { edBuffer = emptyBuffer, edSavedBuffer = emptyBuffer, edCursor = origin, edSelAnchor = Nothing
  , edDesiredCol = 0, edTop = 0, edLeft = 0
  , edPath = Nothing, edModified = False
  , edDiskMtime = Nothing, edDiskChanged = False
  , edLineEnding = LF, edSavedEol = LF, edEncoding = Utf8, edSavedEnc = Utf8
  , edFinalNewline = True, edReadOnly = False
  , edUndo = Seq.empty, edRedo = Seq.empty, edLastEdit = EKNone
  , edStatus = "New file", edFocus = FEdit, edDialog = Nothing, edSearchMode = False
  , edDefPick = Nothing, edQuickOpen = Nothing, edComplete = Nothing
  , edCsv = Nothing, edCsvStash = Nothing, edImage = Nothing, edSvgStash = Nothing, edPdfView = Nothing, edRtf = Nothing, edSheets = Nothing
  , edPdf = Nothing
  }
  where ed = freshDocId ed0

-- | Give the active slot a brand-new document identity, with no journal
-- written for it yet.
freshDocId :: Editor -> Editor
freshDocId ed = ed { edDocId = edNextDocId ed, edNextDocId = edNextDocId ed + 1
                   , edDocSeq = 0, edJournalSeq = 0 }

-- | Open the built-in manual ("Cmedit.Manual") as a read-only Markdown
-- document -- an ordinary document, so navigation, find, word wrap and the
-- Markdown lexer all just work. Already open -> switch to it. The manual's
-- pseudo-path is kept out of the recent-files list ('doClose'), and the
-- jump is recorded in the navigation history like any other.
openManual :: Editor -> Editor
openManual ed0 = case findOpenIndex manualPath ed of
  Just k  -> (switchToFile k ed) { edStatus = manualStatus }
  Nothing ->
    let ed1 | isPristine ed = ed
            | otherwise     = (pushActive ed)
        buf = fromText manualText
    in ensureVisible ed1
         { edBuffer = buf, edSavedBuffer = buf
         , edCursor = origin, edSelAnchor = Nothing, edDesiredCol = 0
         , edTop = 0, edLeft = 0
         , edPath = Just manualPath, edModified = False
         , edDiskMtime = Nothing, edDiskChanged = False
         , edLineEnding = LF, edSavedEol = LF, edEncoding = Utf8, edSavedEnc = Utf8
         , edFinalNewline = True
         , edReadOnly = True
         , edUndo = Seq.empty, edRedo = Seq.empty, edLastEdit = EKNone
         , edStatus = manualStatus
         , edFocus = FEdit, edDialog = Nothing, edSearchMode = False
         , edDefPick = Nothing, edQuickOpen = Nothing, edComplete = Nothing
         , edCsv = Nothing, edCsvStash = Nothing, edImage = Nothing, edSvgStash = Nothing, edPdfView = Nothing, edRtf = Nothing, edSheets = Nothing
         , edPdf = Nothing
         }
  where
    ed = pushNavIfFar (Just manualPath) origin ed0
    manualStatus = "Manual \x2014 Ctrl+F searches it, Ctrl+W closes it"

closeFlow :: Editor -> Editor
closeFlow ed
  | edModified ed = openDialog (mkConfirm DKConfirmClose "Unsaved Changes"
                      (T.pack ("Save changes to " ++ maybe "untitled" takeFileName (edPath ed) ++ "?"))
                      ["Save", "Don't Save", "Cancel"]) ed
  | otherwise = doClose ed

-- Close the active document: switch to the next open file (or the previous
-- one if this was the last), or empty the buffer if it was the only file.
-- The cursor position is recorded in the recents first, so re-opening the
-- file comes back to the same spot.
doClose :: Editor -> Editor
doClose ed0 = case edAfter ed of
  (d : ds) -> switchTo d ed { edAfter = ds }
  [] -> case reverse (edBefore ed) of
    (d : rb) -> switchTo d ed { edBefore = reverse rb, edAfter = [] }
    []       -> doNew ed
  where
    -- The manual's pseudo-path is not a real file; keep it out of the recents.
    ed = case edPath ed0 of
      Just p | p /= manualPath -> recordRecent p (activeCursorPos ed0) ed0
      _ -> ed0

-- | What Save As means in a view that has no buffer: a suggested filename and
-- the text to write there.
--
-- These views are read-only because nothing can write their format back — but
-- that is an argument against writing a @.xlsx@, not against writing anything
-- at all. So Save As becomes an /export/ of what is on screen, in the plainest
-- format that holds it: a workbook's sheet as CSV, a document as text.
--
-- 'Nothing' means the view has nothing to export. That is the image view
-- (there is no text) and the paged view (the file on disk already /is/ the
-- text, so an export would be a slow copy of it).
--
-- Note what this is not: it is not @edBuffer@. Before it existed, Save As on
-- any of these views wrote the empty buffer underneath them and re-pointed the
-- document at the file it had just emptied.
exportSuggestion :: Editor -> Maybe (FilePath, Text)
exportSuggestion ed
  | Just _ <- edSheets ed, Just v <- edCsv ed =
      Just (base ++ sheetSuffix ++ ".csv", Csv.csvToText v)
  | Just rd <- edRtf ed, Rtf.rtfDerived rd = Just (base ++ ".txt", Rtf.rtfPlainText rd)
  | Just pd <- edPdf ed                    = Just (base ++ ".txt", Pdf.pdfPlainText pd)
  | otherwise = Nothing
  where
    base = maybe "untitled" dropExtension (edPath ed)
    -- A workbook has several sheets and this exports one of them, so the name
    -- says which. Sanitised, because a sheet may be called "Q1/Q2".
    sheetSuffix = case edSheets ed of
      Just wb | not (T.null (Xlsx.wbName wb)) ->
        "-" ++ map safe (T.unpack (T.take 40 (Xlsx.wbName wb)))
      _ -> ""
    safe c | c `elem` ("/\\:*?\"<>|" :: String) || c < ' ' = '_'
           | otherwise = c

-- | Is Save As an export in this view, and does it have anything to export?
-- The views with neither a buffer nor exportable text refuse rather than
-- writing an empty file.
saveAsRefusal :: Editor -> Maybe String
saveAsRefusal ed
  | isJust (exportSuggestion ed) = Nothing
  | bufferlessImage (edImage ed) = Just "There is no text in an image to save"
  | isJust (edPager ed)  = Just "This file is open read-only \x2014 it is already on disk as it is"
  | otherwise            = Nothing

saveAsDialogFlow :: Editor -> Editor
saveAsDialogFlow ed = case saveAsRefusal ed of
  Just msg -> ed { edStatus = T.pack msg }
  Nothing  -> case exportSuggestion ed of
    Just (p, _) -> openDialog (mkExportAs (T.pack (exportTitle ed)) (T.pack p)) ed
    Nothing     -> openDialog (mkSaveAs (T.pack (seed (edPath ed)))) ed
  where
    -- The manual's pseudo-path is not writable; offer a plain filename instead.
    -- An archive's buffer is its listing, not its bytes ("Cmedit.Zip"), so
    -- seeding its own path would offer to replace the archive with a
    -- description of itself. (Typing that name back in is still asked about.)
    seed (Just p) | p == manualPath  = takeFileName manualPath
                  | isArchivePath p  = p ++ ".txt"
                  | otherwise        = p
    seed Nothing  = ""

exportOverSourceMsg :: Text
exportOverSourceMsg = "That is the file this view came from \x2014 choose another name"

-- | Carry out a Save As whose target is settled (it does not exist, it is the
-- document's own file, or the user has agreed to replace it). In a view with
-- no buffer this is an export: a copy is written and the document keeps its
-- own path, because it is still the workbook (or PDF, or e-book) and a later
-- Ctrl+S must not aim at the CSV.
saveAsTo :: FilePath -> Editor -> (Editor, [Effect])
saveAsTo p ed0 = case exportSuggestion ed of
  Just (_, txt) -> (ed, [EffExportTo p txt])
  Nothing       -> (leaveSvgIfRenamed ed { edPath = Just p }, [EffSaveTo p])
  where ed = ed0 { edSaveAsTarget = Nothing }

-- | Save As named a file that already exists and is not this document's own:
-- ask before replacing it. The focus starts on Cancel, like the explorer's
-- delete prompt, so a reflexive Enter cannot destroy a file.
askOverwrite :: FilePath -> Editor -> Editor
askOverwrite p ed =
  openDialog (setFocus 1 (mkConfirm DKConfirmOverwrite "Replace File"
      (T.pack (takeFileName p ++ " already exists.\nReplace it?"))
      ["Replace", "Cancel"]))
    ed { edSaveAsTarget = Just p }

-- | The export dialog's title, which is where the difference from an ordinary
-- Save As is explained: this writes a copy and leaves the open document alone.
exportTitle :: Editor -> String
exportTitle ed
  | isJust (edSheets ed) = "Export Sheet as CSV"
  | otherwise            = "Export as Text"

gotoLine :: Text -> Editor -> Editor
-- A workbook is divided into sheets and an EPUB into chapters, so Go To means
-- those here — the same reinterpretation the PDF view makes for pages, and for
-- the same reason. Both are checked before the PDF and pager cases only
-- because they cannot coexist with them; the order carries no meaning.
gotoLine t ed | isJust (edSheets ed) =
  case reads (T.unpack (T.strip t)) :: [(Int, String)] of
    ((n, _) : _) -> goToSheetIn (n - 1) ed
    _ -> ed { edStatus = "Invalid sheet number" }
gotoLine t ed | Just rd <- edRtf ed, Rtf.rtfSectionCount rd > 0 =
  case reads (T.unpack (T.strip t)) :: [(Int, String)] of
    ((n, _) : _) ->
      ed { edRtf = Just (Rtf.rtfGoToSection (rtfHeight ed) n rd), edStatus = "" }
    _ -> ed { edStatus = "Invalid chapter number" }
-- A PDF is divided into pages, so this is where a reader wants to go: the
-- dialog is titled "Go to Page" in this mode ('openGoTo') and the number is
-- read as one. Laid-out row numbers move with the window width and would mean
-- nothing to anyone.
gotoLine t ed | isJust (edPdf ed) =
  case reads (T.unpack (T.strip t)) :: [(Int, String)] of
    ((n, _) : _) -> pdfGoToPageIn n ed
    _ -> ed { edStatus = "Invalid page number" }
-- In the paged view there is no buffer to move a cursor in: jump the viewer
-- instead. This is the one in-file navigation the paged view does support, and
-- the reason it exists — "show me line 4 million of this log".
gotoLine t ed | Just pg <- edPager ed =
  case reads (T.unpack (T.strip t)) :: [(Int, String)] of
    ((n, _) : _) ->
      ed { edPager = Just (Pg.pagerMoveTo (pagerHeight ed) (n - 1) pg), edStatus = "" }
    _ -> ed { edStatus = "Invalid line number" }
gotoLine t ed =
  case reads (T.unpack (T.strip t)) :: [(Int, String)] of
    ((n, _) : _) ->
      let l = max 0 (min (lineCount (edBuffer ed) - 1) (n - 1))
          ed1 = pushNavIfFar (edPath ed) (Pos l 0) ed
      in ensureVisible ed1 { edCursor = Pos l 0, edSelAnchor = Nothing, edDesiredCol = 0, edStatus = "" }
    _ -> ed { edStatus = "Invalid line number" }

openPathsList :: Editor -> [FilePath]
openPathsList = mapMaybe docPath . allOpenDocs

------------------------------------------------------------------------------
-- Opening a result

-- | Move the cursor to a match once its file is the active document. Kept as a
-- pending action so it also works after an async (large-file) load completes.
applyPendingJump :: Editor -> Editor
applyPendingJump ed0 = case edPendingJump ed of
  Just (p, l, c, len) | edPath ed == Just p, isNothing (edImage ed), isNothing (edPdf ed) ->
    let buf = edBuffer ed
        a = clampPos (Pos l c) buf
        b = clampPos (Pos l (c + len)) buf
    in ensureVisible ed { edCursor = b, edSelAnchor = Just a, edPendingJump = Nothing
                        , edDesiredCol = colToDisplay (tabWidthOf ed) (posCol b) (getLine' (posLine b) buf)
                        , edFocus = FEdit }
  _ -> ed
  where
    -- A match is a position in an SVG's markup, which the picture does not
    -- show: land in the source view, the way an RTF match opens its markup.
    ed = case edPendingJump ed0 of
      Just (p, _, _, _) | edPath ed0 == Just p, isJust (activeVec ed0) -> toggleSvg ed0
      _ -> ed0

-- Open the file for a match and jump to it (already-open files switch instantly;
-- others load via EffOpen and the jump applies on completion). Records the
-- origin in the navigation history so Alt+Left comes back.
openMatch :: FilePath -> Int -> Int -> Int -> Editor -> (Editor, [Effect])
openMatch path line col len ed =
  openMatchRaw path line col len (pushNavIfFar (Just path) (Pos line col) ed)

-- The history-free version, used by Alt+Left/Right themselves.
openMatchRaw :: FilePath -> Int -> Int -> Int -> Editor -> (Editor, [Effect])
openMatchRaw path line col len ed =
  let ed0 = ed { edPendingJump = Just (path, line, col, len), edSearchMode = False }
  in case findOpenIndex path ed0 of
       Just k  -> noEff (applyPendingJump (switchToFile k ed0))
       Nothing -> (ed0 { edFocus = FEdit }, [EffOpen path])

-- | Paths of all open documents that currently have unsaved changes.
modifiedDocPaths :: Editor -> [FilePath]
modifiedDocPaths ed =
  [ p | d <- allOpenDocs ed, docModified d, Just p <- [docPath d] ]

-- | Save parameters for every modified, titled document (for Save All). The
-- active doc is CSV-synced first so a table's edits are written correctly.
modifiedDocsToSave :: Editor -> [(FilePath, Encoding, LineEnding, Bool, Buffer)]
modifiedDocsToSave ed0 =
  let ed = syncCsvToBuffer ed0
      -- Never a read-only document: an archive listing is one, under the
      -- archive's own path, and writing it would replace the archive with
      -- its table of contents.
      fromDoc d = [ (p, docEncoding d, docLineEnding d, docFinalNewline d, docBuffer d)
                  | docModified d, not (docReadOnly d), Just p <- [docPath d] ]
      active = [ (p, edEncoding ed, edLineEnding ed, edFinalNewline ed, edBuffer ed)
               | edModified ed, not (edReadOnly ed), Just p <- [edPath ed] ]
  in concatMap fromDoc (edBefore ed) ++ active ++ concatMap fromDoc (edAfter ed)

-- | Driver callback: mark the given (path, new-mtime) documents saved after a
-- Save All, and report how many were written.
savedAll :: [(FilePath, Maybe DiskTime)] -> Editor -> (Editor, [Effect])
savedAll saved ed =
  let saveMap = saved
      applyDoc d = case docPath d of
        Just p | Just mt <- lookup p saveMap
               -> d { docModified = False, docSavedBuffer = docBuffer d
                    , docSavedEol = docLineEnding d, docSavedEnc = docEncoding d
                    , docDiskMtime = mt, docDiskChanged = False, docLastEdit = EKNone }
        _ -> d
      activeSaved = case edPath ed of
        Just p | Just mt <- lookup p saveMap -> Just mt
        _ -> Nothing
      ed1 = ed { edBefore = map applyDoc (edBefore ed)
               , edAfter  = map applyDoc (edAfter ed) }
      n = length saved
      ed2 = case activeSaved of
        Just mt -> ed1 { edModified = False, edSavedBuffer = edBuffer ed1
                       , edSavedEol = edLineEnding ed1, edSavedEnc = edEncoding ed1
                       , edDiskMtime = mt, edDiskChanged = False, edLastEdit = EKNone
                       , edStatus = T.pack ("Saved " ++ show n ++ " file" ++ plural n) }
        Nothing -> ed1 { edStatus = T.pack ("Saved " ++ show n ++ " file" ++ plural n) }
  -- When Save All was the answer to the quit-all prompt, resume quitting: quit
  -- outright if nothing is left, else fall back to per-file prompts (e.g. for any
  -- untitled files that Save All couldn't write). Otherwise re-lint the active
  -- document now that it is on disk (save-time-only tools too).
  in if edQuitting ed2 then noEff (quitStep ed2) else (ed2, [EffLintNow])

------------------------------------------------------------------------------
-- Crash-recovery journal: what to write, and what recovery installs
--
-- The driver owns the timer, the directory and every byte of IO
-- ("Cmedit.App"); this is the question it asks. Two invariants live here and
-- nowhere else:
--
--   * __The set of journals that should exist is derived, never maintained.__
--     'journalLiveKeys' answers "which documents would a crash lose right
--     now", and the driver deletes any journal it wrote that is not in that
--     answer. So a save, a close, a revert, an undo back to unmodified and a
--     toggle into a view with no buffer all drop the journal without any of
--     them knowing the journal exists — there is no save or close path that
--     can forget to say so, because none of them says anything.
--
--   * __A journal is rewritten only when its document moved.__ That is the
--     'docJournalSeq' \/ 'docDocSeq' comparison, and it is per document for
--     the reason spelled out at 'edDocSeq': the global edit counter would make
--     typing in one file rewrite every other file's journal.

-- | Every open document that would lose content in a crash, keyed by journal
-- file name. The driver's authority on which journals should exist.
-- Turning the key off mid-session answers "nothing", which makes the driver
-- delete this session's journals — the point of @journal = off@ is that no
-- copy of what you are editing is left in the cache, and one written before
-- the switch is exactly such a copy.
journalLiveKeys :: Editor -> [FilePath]
journalLiveKeys ed
  | not (cfgJournal (edConfig ed)) = []
  | otherwise = [ journalKeyOf d | d <- allOpenDocs ed, journalableDoc d ]

-- | The documents whose journal is out of date: modified, journalable, and
-- carrying edits their journal file does not hold.
staleJournalDocs :: Editor -> [Document]
staleJournalDocs ed
  | not (cfgJournal (edConfig ed)) = []
  | otherwise = [ d | d <- allOpenDocs ed
                    , journalableDoc d
                    , docJournalSeq d /= docDocSeq d ]

-- | The journals that are out of date and should be written now, each with the
-- value of its document's edit counter that the record captures.
--
-- The counter travels with the request because the write is asynchronous: by
-- the time the file is on disk the document may have moved again, and settling
-- 'docJournalSeq' to /that/ value (rather than to what was written) would
-- declare a journal current which is one keystroke behind — the newer edits
-- would then never be written. See 'journalsWritten'.
journalRequests :: Editor -> [(FilePath, Int, Journal)]
journalRequests ed =
  [ (journalKeyOf d, docDocSeq d, journalOf d) | d <- staleJournalDocs ed ]

-- | Roughly how many bytes the next write-behind pass would write.
--
-- Deliberately an estimate: it exists to /schedule/ the write, and the only
-- way to know exactly is to serialise the buffers — which is the work being
-- scheduled. One character is counted as one byte (true for the ASCII that
-- dominates source and log files, an under-count for text that is mostly
-- non-Latin) plus one separator per line. The count comes from 'bufChars',
-- which the buffer maintains, so this is O(open documents) however large they
-- are.
--
-- A CSV document's buffer is stale under its table, so its estimate is the
-- text as of the last sync rather than the grid that will actually be written.
-- Same order of magnitude, which is all a timer interval needs.
journalPendingBytes :: Editor -> Int
journalPendingBytes ed = sum (map docJournalBytes (staleJournalDocs ed))
  where docJournalBytes d = let b = docBuffer d in bufChars b + lineCount b

-- | Minimum spacing (µs) between two journal write-behind passes, given the
-- bytes one pass would write. Also the debounce after the last edit, at the
-- floor.
--
-- The journal rewrites whole buffers, so its cost is the buffer's size and its
-- /traffic/ is that size divided by the interval. At the shipped 2 s a 40 MB
-- buffer under editing means ~10 MB/s of writes to @~\/.cache@ for as long as
-- the session lasts — measured, and far more than this feature is worth.
-- Spacing the passes by size instead bounds that at 'journalBudgetBps' while
-- leaving ordinary files (anything up to 4 MB) at exactly the 2 s they have
-- always had.
--
-- Both ends are clamped on purpose. The floor is what a journal is /for/: 2 s
-- is the promise about how much a crash can take. The ceiling is the same
-- promise from the other side — past half a minute the feature starts failing
-- at its own job, so the very largest buffers give the budget back rather than
-- widening the window further (a 100 MB buffer, the largest that opens as
-- text, lands at ~3.3 MB/s instead of 2).
-- The size test comes before the arithmetic so the multiplication cannot
-- overflow: past the byte count that already reaches the ceiling there is
-- nothing left to compute.
journalDelayUs :: Int -> Int
journalDelayUs bytes
  | bytes <= 0                = journalMinDelayUs
  | bytes >= atCeilingBytes   = journalMaxDelayUs
  | otherwise                 = max journalMinDelayUs
                                    ((bytes * 1000000) `div` journalBudgetBps)
  where atCeilingBytes = (journalMaxDelayUs `div` 1000000) * journalBudgetBps

-- | The floor on 'journalDelayUs': the write-behind debounce every buffer
-- small enough not to matter still gets.
journalMinDelayUs :: Int
journalMinDelayUs = 2000000

-- | The ceiling on 'journalDelayUs'; see there for why it is not simply larger.
journalMaxDelayUs :: Int
journalMaxDelayUs = 30000000

-- | Steady-state write budget (bytes\/second) the write-behind is allowed to
-- spend on @~\/.cache@ while a large buffer is being edited. Two megabytes a
-- second is around what one ordinary save costs, spread over a second — enough
-- that the journal is never the reason a disk is busy, and small enough that a
-- laptop's SSD or a network home directory does not notice it.
journalBudgetBps :: Int
journalBudgetBps = 2 * 1024 * 1024

-- | What the driver watches to decide a journal write is due. Changes when a
-- document is edited, saved, closed or opened — and when the config key is
-- toggled, so turning journalling on mid-session writes the backlog.
journalFingerprint :: Editor -> (Bool, [(FilePath, Int)])
journalFingerprint ed =
  ( cfgJournal (edConfig ed)
  , [ (journalKeyOf d, docDocSeq d) | d <- allOpenDocs ed, journalableDoc d ] )

-- | Driver callback: these journal files now hold the content their document
-- had at the given edit counter, so stop offering to rewrite /that/ version.
-- Matching is by journal key rather than by path because an untitled buffer
-- has no path.
--
-- The counter is the one the write captured, not the document's current one:
-- the write happens off the event-loop thread, so the document may have been
-- edited since it started, and those edits are still unjournalled. Recording
-- the captured value leaves the document stale, which is exactly right — the
-- next tick writes it again.
journalsWritten :: [(FilePath, Int)] -> Editor -> Editor
journalsWritten done ed = ed
  { edJournalSeq = maybe (edJournalSeq ed) id (lookup (journalKeyOf (captureDoc ed)) done)
  , edBefore = map upd (edBefore ed)
  , edAfter  = map upd (edAfter ed)
  }
  where upd d = case lookup (journalKeyOf d) done of
                  Just s  -> d { docJournalSeq = s }
                  Nothing -> d

-- | Startup: make sure a new untitled buffer cannot be given the id of an
-- untitled journal that is still on disk (one the user kept for later, or one
-- we have just recovered) and clobber it on the next write-behind tick.
seedJournalIds :: [Int] -> Editor -> Editor
seedJournalIds used ed =
  ed { edNextDocId = maximum (edNextDocId ed : map (+ 1) used) }

-- | Open the startup recovery prompt for the journals the driver found.
-- Nothing to offer ⇒ the editor is returned untouched, so this is safe to
-- call unconditionally.
--
-- When a dialog is /already/ open — which at startup means the changed-files
-- prompt (plan 0030 §2.8: restore, then changed files, then the journal) — the
-- items are stashed and nothing is shown. 'afterSessionChanged' opens this once
-- that question has been answered, so the journal's answer is asked about the
-- state the first answer produced. Two stacked modal dialogs at startup is the
-- accepted design: each is independently conditional and rare, and merging them
-- would fuse two questions with different answers and different consequences.
openRecoverDialog :: [RecoverItem] -> Editor -> Editor
openRecoverDialog [] ed = ed
openRecoverDialog items ed
  | isJust (edDialog ed) = ed { edRecover = items }
openRecoverDialog items ed =
  openDialog (mkConfirm DKRecover "Unsaved Changes Recovered" msg
                ["Recover", "Discard", "Keep for later"])
             ed { edRecover = items }
  where
    n   = length items
    msg = T.unlines $
      [ T.pack (show n ++ " file" ++ plural n
                ++ " had unsaved changes when CMeDit last exited.")
      , "" ]
        ++ [ "  " <> describeRecover it | it <- take maxRecoverListed items ]
        ++ [ T.pack ("  \x2026 and " ++ show (n - maxRecoverListed) ++ " more")
           | n > maxRecoverListed ]

-- How many files the recovery prompt names before it stops and counts. The
-- dialog is a modal box on an 80-column terminal, not a report.
maxRecoverListed :: Int
maxRecoverListed = 8

-- One line of the recovery listing: what it is, and the caveat if any.
describeRecover :: RecoverItem -> Text
describeRecover it =
  label <> maybe "" (\note -> " \x2014 " <> note) (J.recoveryNote (riCase it))
  where
    label = case jPath (riJournal it) of
      Just p  -> T.pack (takeFileName p)
      Nothing -> "(untitled buffer)"

-- | The Recover button: install every offered journal as a modified document.
--
-- Shaped like 'Cmedit.EditorFind.addStagedDoc'\'s result — a dirty buffer with
-- the file's own metadata — with two differences that matter. The disk-mtime
-- baseline is the one the /journal/ recorded, not the file's mtime now, so
-- 'RecoverChanged' arrives already carrying the @◆@ the stale-file machinery
-- would have shown; and a journal whose path is already open (a file named on
-- the command line, say) patches that document instead of opening a second
-- copy of it.
--
-- Nothing here writes anything: recovering leaves every file on disk exactly
-- as it was, and the recovered documents are unsaved buffers the user can
-- inspect, save or close.
recoverJournals :: Editor -> Editor
recoverJournals ed0
  | null items = closeDialog ed0 { edRecover = [] }
  | otherwise  = (rebuildZipper k docs ed1)
                   { edStatus = T.pack ("Recovered " ++ show n ++ " unsaved file"
                                        ++ plural n ++ " \x2014 nothing has been "
                                        ++ "written to disk") }
  where
    items = edRecover ed0
    n     = length items
    ed1   = (closeDialog ed0) { edRecover = [] }
    -- A pristine empty buffer is replaced rather than kept around beside the
    -- recovered work, exactly as opening a file into it would.
    base  = if isPristine ed1 then [] else allOpenDocs ed1
    (docs, landed) = foldl step (base, []) items
    -- Land on the first thing recovered.
    k = case reverse landed of
          (i : _) -> i
          []      -> min (length (edBefore ed1)) (length docs - 1)
    step (ds, seen) it =
      let d = recoveredDoc it
      in case findIndex ((== docPath d) . docPath) ds of
           -- The same file is already open: apply the journal to it, so
           -- recovery cannot produce two documents for one path.
           Just i | isJust (docPath d) ->
             (take i ds ++ [d] ++ drop (i + 1) ds, seen ++ [i])
           _ -> (ds ++ [d], seen ++ [length ds])

-- One recovered journal as an unsaved document.
recoveredDoc :: RecoverItem -> Document
recoveredDoc it = Document
  { docBuffer = buf
    -- Deliberately not @buf@: the saved baseline is what the modified flag is
    -- computed against, and a recovered document differs from disk by
    -- definition — that is why its journal existed. Seeding the baseline with
    -- the recovered text would let the first edit-and-undo declare the
    -- document clean and delete the journal that is still the only copy.
  , docSavedBuffer = emptyBuffer
  , docCursor = clampPos (jCursor j) buf, docSelAnchor = Nothing
  , docDesiredCol = 0, docTop = 0, docLeft = 0
  , docPath = jPath j, docModified = True
  , docDiskMtime = jMtime j, docDiskChanged = riCase it == RecoverChanged
  , docLineEnding = jEol j, docSavedEol = jEol j
  , docEncoding = jEnc j, docSavedEnc = jEnc j
  , docFinalNewline = jFinalNewline j, docReadOnly = jReadOnly j
  , docUndo = Seq.empty, docRedo = Seq.empty, docLastEdit = EKNone
  , docOverwrite = False, docDiscard = False
  , docCsvStash = Nothing, docImage = Nothing, docSvgStash = Nothing, docPdfView = Nothing, docPager = Nothing
  , docPdf = Nothing, docSheets = Nothing, docHlCache = Nothing, docDiags = []
    -- 'Csv.markUnsaved' for exactly the reason 'docSavedBuffer' is empty above,
    -- and it has to be said twice because the table carries its own baseline:
    -- 'mkCsvLines' would adopt the recovered grid as the saved one, and then a
    -- single Ctrl+Z (which need not even have an undo step to pop) recomputes
    -- the flag through 'csvMod', declares the document clean, sweeps away the
    -- journal that is the only copy of the work and lets Ctrl+Q leave silently.
  , docCsv = case jPath j of
      Just p | isCsvPath p ->
        Just (Csv.markUnsaved (Csv.mkCsvLines (csvDelimForPath p) (bufLines buf)))
      _ -> Nothing
  , docRtf = case jPath j of
      Just p | isRtfPath p -> Just (Rtf.mkRtfDoc 0 (bufLines buf))
      _ -> Nothing
    -- The id has to be the one the journal file is *named* after, or the
    -- write-behind would write the recovered buffer to a second file and
    -- leave the original behind for the next startup to offer again.
  , docDocId = fromMaybe 0 (J.untitledIndexOf (riKey it))
    -- The journal on disk already holds exactly this content, so it is
    -- current: adopting it must not trigger a redundant rewrite.
  , docDocSeq = 1, docJournalSeq = 1
  }
  where
    j   = riJournal it
    buf = J.journalBuffer j

------------------------------------------------------------------------------
-- The changed-files dialog (plan 0030 §2.6)
--
-- A session restored on Monday describes files as they were on Friday, and
-- @git pull@ happened in between. The ◆ machinery cannot show that: it compares
-- against a baseline taken /at load/, and the load just happened. A session that
-- records what it saw can say so instead — and, when a clean-exit snapshot
-- exists (§2.7), can offer the version the user actually had.

-- | Open the changed-files prompt for whatever the driver's comparison found.
-- Nothing changed ⇒ the editor is returned untouched, so this is safe to call
-- unconditionally after any restore.
--
-- __There is no Cancel, and Esc is safe.__ By the time this appears the files
-- are open at their newest state, so @Latest on Disk@ is a no-op and
-- 'Cmedit.Editor.cancelDialog' maps to it. __When there is nothing to offer__ —
-- a crashed session (no snapshots), every file over the cap, or
-- @journal = off@ — the second choice would do nothing, and offering it anyway
-- would be a lie, so the dialog degrades to a single @OK@ and the informational
-- wording, joining the single-button family that dismisses on a click off the
-- box.
openSessionChangedDialog :: Editor -> Editor
openSessionChangedDialog ed = case edSessionChanged ed of
  []    -> ed
  items -> openDialog (mkConfirm DKSessionChanged "Files Changed Since This Session"
                         (msg items) (buttons items)) ed
  where
    buttons items
      | any (isJust . cfSnapshot) items = ["Latest on Disk", "As You Left Them"]
      | otherwise                       = ["OK"]
    msg items = T.unlines $
      [ T.pack (show n ++ " file" ++ plural n ++ " ha" ++ (if n == 1 then "s" else "ve")
                ++ " changed on disk since this session ended.")
      , "" ]
        ++ [ "  " <> describeChanged it | it <- take maxRecoverListed items ]
        ++ [ T.pack ("  \x2026 and " ++ show (n - maxRecoverListed) ++ " more")
           | n > maxRecoverListed ]
        ++ (if any (isJust . cfSnapshot) items
              then [ "", "Open the newest version, or the files as you left them?" ]
              else [])
      where n = length items

-- One line of the changed-files listing. The ◆ is the marker the editor already
-- uses for "this file changed underneath you", so it needs no explaining; a file
-- with no usable snapshot says so and stays at its disk version either way.
describeChanged :: ChangedFile -> Text
describeChanged cf =
  "\x25c6 " <> T.pack (takeFileName (cfPath cf))
    <> (case cfSnapshot cf of
          Just _  -> ""
          Nothing -> " \x2014 no saved copy from that session")

-- | The @As You Left Them@ answer: replace each offered document's buffer with
-- the session's snapshot of it, as a __modified, unsaved buffer__.
--
-- The shape is 'recoveredDoc'\'s, with the two differences the situation
-- demands. 'docSavedBuffer' is the file's /current on-disk content/ — which the
-- document is holding right now, because it was loaded from disk moments ago —
-- so the modified flag is computed against what is really there and Ctrl+S has
-- something honest to compare with; and 'docDiskMtime' is left at the
-- __session's recorded baseline__ (already installed by the restore) so the
-- document carries ◆ from the first frame. Nothing here writes: saving over the
-- newer file is then a deliberate Ctrl+S by a user who has been told twice.
--
-- Documents whose snapshot is missing are left exactly as loaded.
installSessionSnapshots :: Editor -> Editor
installSessionSnapshots ed0
  | n == 0    = ed0 { edSessionChanged = [] }
  | otherwise = (rebuildZipper here docs ed1)
                  { edStatus = T.pack ("Restored " ++ show n ++ " file" ++ plural n
                                       ++ " as the session left them \x2014 unsaved, "
                                       ++ "nothing has been written to disk") }
  where
    offered = [ (cfPath cf, j) | cf <- edSessionChanged ed0, Just j <- [cfSnapshot cf] ]
    ed1     = ed0 { edSessionChanged = [] }
    here    = min (length (edBefore ed1)) (max 0 (length docs - 1))
    (docs, n) = let ps = map patch (allOpenDocs ed1)
                in (map fst ps, length (filter snd ps))
    -- 'snapshotableDoc' guards the patch, not just the write: the restore
    -- reopened every path through the ordinary guards, so a file that has since
    -- grown past 'maxOpenBytes' or been replaced by a PDF is now a view with no
    -- buffer under it, and writing a buffer into one would be nonsense. Such a
    -- document keeps its disk version, which is the same floor a missing
    -- snapshot gets.
    --
    -- __An unsaved document keeps its edits__, and gets that same floor. At
    -- startup this never fires (every restored document was loaded from disk
    -- moments ago), but a /menu/ restore runs on a live editor, where a
    -- recorded path may already be open with work in it — and this patch
    -- replaces the buffer and clears the undo history, so without the guard one
    -- click on @As You Left Them@ would silently destroy it. Plan 0030 §2.5's
    -- rule for the menu route is that it adds and never closes, which is why it
    -- needs no unsaved-changes prompt of its own; overwriting a dirty buffer
    -- would be strictly worse than closing one.
    patch d = case [ j | (p, j) <- offered, Just p == docPath d
                       , snapshotableDoc d, not (docModified d) ] of
      (j : _) -> (snapshotDoc j d, True)
      []      -> (d, False)

-- One document rebuilt around its snapshot. The identity fields (the document
-- id, the read-only flag, the diagnostics) stay the document's own; the content,
-- cursor and file-shape metadata come from the snapshot, and the /saved/
-- baselines stay the disk file's, so a difference in either reads as modified.
--
-- 'docDocSeq' is advanced past 'docJournalSeq' deliberately: this is unsaved
-- content now, and the write-behind must journal it.
snapshotDoc :: Journal -> Document -> Document
snapshotDoc j d = d
  { docBuffer      = buf
  , docSavedBuffer = docBuffer d          -- what is on disk right now
  , docCursor      = clampPos (jCursor j) buf
  , docSelAnchor   = Nothing, docDesiredCol = 0, docTop = 0, docLeft = 0
  , docModified    = True
    -- The snapshot's own baseline *is* the session's recorded one (both come
    -- from 'docDiskMtime' at the moment the session ended), so the document
    -- arrives already carrying the ◆ the stale-file machinery would have shown
    -- — which it could not, because ◆ compares against a baseline taken at load
    -- and the load just happened. 'noteDiskMtimes' never rewrites a baseline, so
    -- the freshness poll confirms this rather than clobbering it.
  , docDiskMtime   = jMtime j
  , docDiskChanged = True                 -- ◆ from the first frame
  , docLineEnding  = jEol j, docEncoding = jEnc j, docFinalNewline = jFinalNewline j
  , docUndo        = Seq.empty, docRedo = Seq.empty, docLastEdit = EKNone
  , docHlCache     = Nothing
  , docCsv = case docPath d of
      -- 'Csv.markUnsaved' for exactly the reason 'docSavedBuffer' is the disk
      -- text above: 'mkCsvLines' would adopt the snapshot grid as the saved one,
      -- and a single Ctrl+Z would then declare the document clean.
      Just p | isCsvPath p, isNothing (docSheets d) ->
        Just (Csv.markUnsaved (Csv.mkCsvLines (csvDelimForPath p) (bufLines buf)))
      _ -> docCsv d
  , docRtf = case docPath d of
      Just p | isRtfPath p, not (maybe False Rtf.rtfDerived (docRtf d)) ->
        Just (Rtf.mkRtfDoc 0 (bufLines buf))
      _ -> docRtf d
  , docDocSeq = docDocSeq d + 1
  }
  where buf = J.journalBuffer j

-- | What happens when the changed-files prompt is answered, whichever way: the
-- queued journal-recovery prompt (if the startup scan found one) takes the
-- screen. Plan 0030 §2.8's \"journal last\" — a journal is unsaved content from a
-- session that died, a snapshot is saved content from one that ended tidily, and
-- when both speak for a file the unsaved edits must win.
--
-- After startup 'edRecover' is always empty (the prompt is answered once per
-- process), so a menu-driven restore's answer opens nothing, which is §2.5's
-- rule that journal recovery does not re-run.
afterSessionChanged :: Editor -> Editor
afterSessionChanged ed = openRecoverDialog (edRecover ed) ed { edSessionChanged = [] }

-- | The snapshots a clean exit should write: one journal record per open
-- document worth snapshotting ('snapshotOf'), keyed by the file name it goes
-- under. Pure, so \"which documents get snapshotted\" is a unit test rather than
-- a property of the driver.
snapshotRequests :: Editor -> [(FilePath, Journal)]
snapshotRequests ed = mapMaybe snapshotOf (allOpenDocs ed)

-- | Alt+Left: go back to the previous location (pushing the current one onto
-- the forward trail). Stops in untitled buffers are only reachable while that
-- buffer is still active; dead ones are dropped.
navBack :: Editor -> (Editor, [Effect])
navBack ed = case Seq.viewl (edNavBack ed) of
  Seq.EmptyL -> noEff ed { edStatus = "No earlier location" }
  (s Seq.:< rest)
    | not (stopReachable s ed) -> navBack ed { edNavBack = rest }
    | otherwise ->
        gotoStop s ed { edNavBack = rest
                      , edNavFwd = pushHist maxNavStops (currentStop ed) (edNavFwd ed) }

-- | Alt+Right: re-visit a location undone by Go Back.
navFwd :: Editor -> (Editor, [Effect])
navFwd ed = case Seq.viewl (edNavFwd ed) of
  Seq.EmptyL -> noEff ed { edStatus = "No later location" }
  (s Seq.:< rest)
    | not (stopReachable s ed) -> navFwd ed { edNavFwd = rest }
    | otherwise ->
        gotoStop s ed { edNavFwd = rest
                      , edNavBack = pushHist maxNavStops (currentStop ed) (edNavBack ed) }

stopReachable :: NavStop -> Editor -> Bool
stopReachable (NavStop Nothing _) ed = isNothing (edPath ed)
stopReachable _ _ = True

-- Navigate to a stop WITHOUT recording history (the stacks were already
-- adjusted by the caller): titled files ride the same open/switch/pending-jump
-- machinery as a search result; an untitled stop jumps within the buffer.
gotoStop :: NavStop -> Editor -> (Editor, [Effect])
gotoStop (NavStop mpath pos) ed = case mpath of
  Just p  -> openMatchRaw p (posLine pos) (posCol pos) 0 ed
  Nothing ->
    let cur = clampPos pos (edBuffer ed)
        line = getLine' (posLine cur) (edBuffer ed)
    in noEff (ensureVisible ed { edCursor = cur, edSelAnchor = Nothing
                               , edDesiredCol = colToDisplay (tabWidthOf ed) (posCol cur) line })

closeQuickOpen :: Editor -> Editor
closeQuickOpen ed = ed { edQuickOpen = Nothing, edFocus = contentFocus ed }

-- | Driver callback: the root has been canonicalised — install it, seed the
-- recents-first ordering (recently-used and open files under the root lead
-- while the walk streams in), and re-rank.
quickOpenSeed :: Int -> FilePath -> Editor -> Editor
quickOpenSeed gen root ed = case edQuickOpen ed of
  Just qo | qoGen qo == gen ->
    let recents = [ T.pack rel
                  | p <- map rePath (edRecent ed) ++ openPathsList ed
                  , edPath ed /= Just p    -- you're already looking at the active file
                  , Just rel <- [relativeTo root p] ]
        dedup [] = []
        dedup (x : xs) = x : dedup (filter (/= x) xs)
    in ed { edQuickOpen = Just (Q.qoRescore qo { qoRoot = root
                                               , qoRecent = take 20 (dedup recents) }) }
  _ -> ed

-- | Driver callback: a batch of discovered files (workspace-relative paths).
quickFilesFound :: Int -> [Text] -> Editor -> Editor
quickFilesFound gen paths ed = case edQuickOpen ed of
  Just qo | qoGen qo == gen -> ed { edQuickOpen = Just (Q.qoAddFiles paths qo) }
  _ -> ed

-- | Driver callback: the walk finished.
quickDone :: Int -> Editor -> Editor
quickDone gen ed = case edQuickOpen ed of
  Just qo | qoGen qo == gen -> ed { edQuickOpen = Just qo { qoRunning = False } }
  _ -> ed

-- | Geometry of the quick-open box: @(top, left, height, width)@. Shared by
-- the renderer and mouse hit-testing. Rows: border/title, the query input,
-- 'quickOpenViewH' list rows, a footer, and the bottom border.
quickOpenGeom :: Editor -> (Int, Int, Int, Int)
quickOpenGeom ed =
  let (rows, cols) = edSize ed
      n  = maybe 0 (length . qoMatches) (edQuickOpen ed)
      w  = max 40 (min 76 (cols - 4))
      vh = max 1 (min (max 1 n) (max 3 (rows - 9)))
      h  = vh + 4
      x  = max 0 ((cols - w) `div` 2)
      y  = max 1 ((rows - h) `div` 3)   -- sit high like a palette, not centred
  in (y, x, h, w)

quickOpenViewH :: Editor -> Int
quickOpenViewH ed = let (_, _, h, _) = quickOpenGeom ed in max 1 (h - 4)

------------------------------------------------------------------------------
-- CSV table mode

-- ('csvGutterWidthFor', the row-number gutter width, lives in EditorState so
-- the pointer-shape hint can share the border geometry.)

-- (visible data rows, width available for columns).
-- (scrolling-area height, freeze-row count, scrolling-area width). When the
-- header is frozen the first row is pinned, so it eats a row of height and is
-- excluded from the scroll.
csvViewportFor :: Editor -> CsvView -> (Int, Int, Int)
csvViewportFor ed v =
  let lo = computeLayout ed
      frozen = if edFreezeHeader ed && Csv.nRows v > 0 then Csv.rowHeight v 0 else 0
      freezeRows = if edFreezeHeader ed && Csv.nRows v > 0 then 1 else 0
  in ( max 1 (loTextHeight lo - 1 - frozen), freezeRows
       -- loVBarW: the rightmost column is the scrollbar's, when enabled (as in 'computeLayout')
     , max 1 (loCols lo - loContentLeft lo - csvGutterWidthFor v - loVBarW lo) )

-- Set the table view, scrolling so the current cell is visible.
csvPut :: CsvView -> Editor -> Editor
csvPut v ed = let (rv, fr, w) = csvViewportFor ed v
              in ed { edCsv = Just (Csv.ensureVisible rv fr w v) }

-- A mutating table change (marks the document modified).
-- Apply a new table view and take the modified flag from the table, which
-- maintains it as state ('Csv.csvDirty') rather than comparing the grid against
-- the saved one — so editing a cell back to its original value clears the flag,
-- exactly, at any table size, in O(1) (plan 0028).
-- The per-document edit counter is bumped here and not in 'afterEdit': a table
-- edit never touches the line buffer, so this is the only funnel through which
-- the crash-recovery journal can learn that the grid moved.
csvMod :: CsvView -> Editor -> Editor
csvMod v ed = (csvPut v ed) { edModified = Csv.isModified v || metaModified ed
                            , edDocSeq = edDocSeq ed + 1, edStatus = "" }

-- Kept as a named alias for the undo/redo call sites; 'Csv.isModified' is a
-- field read, so the exact reconciliation is cheap at any table size (there is
-- no longer a big-table cutoff that fakes the flag).
csvModUndo :: CsvView -> Editor -> Editor
csvModUndo = csvMod

csvPageSize :: Editor -> Int
csvPageSize ed = max 1 (loTextHeight (computeLayout ed) - 2)

csvJump :: Dir -> CsvView -> CsvView
csvJump DUp    = Csv.moveToTop
csvJump DDown  = Csv.moveToBottom
csvJump DLeft  = Csv.moveToHomeRow
csvJump DRight = Csv.moveToEndRow

csvStruct :: Dir -> CsvView -> Editor -> Editor
csvStruct DUp    v = csvMod (Csv.insertRowAbove v)
csvStruct DDown  v = csvMod (Csv.insertRowBelow v)
csvStruct DLeft  v = csvMod (Csv.insertColLeft v)
csvStruct DRight v = csvMod (Csv.insertColRight v)
