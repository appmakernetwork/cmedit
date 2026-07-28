-- | Document lifecycle: loading, the open-files zipper, view modes
-- (CSV table / image), recents, navigation history, quick open and
-- the save/close/quit flows.
module Cmedit.EditorDoc where


import Data.Char (isAlpha, isAlphaNum, isSpace, isDigit)
import Data.Foldable (toList)
import Data.List (findIndex, intercalate, isPrefixOf, isSuffixOf, sortBy)
import Data.Ord (Down(..), comparing)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName)

import Data.Array (Array)

import Cmedit.Types
import Cmedit.TextBuffer
import Cmedit.Width (colToDisplay, displayToCol, wrapLine)
import Cmedit.ConfigFile
  ( Config(..), ThemeName(..), defaultConfig, RecentEntry(..)
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
import Cmedit.Image (Image(..), ImgMode(..), renderImage, viewFit)
import Cmedit.Syntax (HlCache, CommentSyntax(..), langComment, langForPath)

import Cmedit.History (pushHist)
import Cmedit.Journal (Journal(..), RecoveryCase(..))
import qualified Cmedit.Journal as J
import Cmedit.Pager (PagerDoc(..))
import Cmedit.Rtf (RtfDoc(..))
import Cmedit.Pdf (PdfDoc(..))
import qualified Cmedit.Pdf as Pdf
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
        , edImage = Nothing, edRtf = Nothing, edSheets = Nothing, edPdf = Nothing
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
                     , edCsvStash = Just v, edStatus = "Plain-text mode" }
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
  , edCsv = Nothing, edCsvStash = Nothing, edImage = Nothing, edRtf = Nothing
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
  , docImage = Nothing, docRtf = Nothing, docSheets = Nothing, docPager = Nothing
  , docPdf = Nothing
  , docHlCache = Nothing
  , docDiags = []
  -- Read-only, so never journalled; and titled, so the id is unused anyway.
  , docDocId = 0, docDocSeq = 0, docJournalSeq = 0
  }

-- | Install a parsed PDF as the active document.
pdfLoaded :: FilePath -> PdfDoc -> Editor -> Editor
pdfLoaded path pd ed = touchRecent path $ refreshPdf $ ensureVisible $
  (blankReadOnly path status ed) { edPdf = Just pd }
  where
    status = "Viewing " ++ takeFileName path ++ "  "
               ++ show (Pdf.pdfPageCount pd) ++ " page" ++ plural (Pdf.pdfPageCount pd)
               ++ (if T.null (pdNote pd) then "" else "  \x2014 " ++ T.unpack (pdNote pd))
               ++ "  \x2014 read-only"

-- | Open a parsed PDF as a new document (reusing a pristine empty buffer if
-- present), mirroring 'setLoadedNew' for text.
pdfLoadedNew :: FilePath -> PdfDoc -> Editor -> Editor
pdfLoadedNew path pd ed = case switchToOpen path ed of
  Just ed'
    | edFocus ed == FExplorer -> ed' { edFocus = FExplorer }
    | otherwise               -> ed'
  Nothing
    | isPristine ed -> pdfLoaded path pd ed
    | otherwise     -> pdfLoaded path pd ed { edBefore = edBefore ed ++ [captureDoc ed] }

-- | Append a PDF to the open-files list (startup, 2nd+ file).
addPdfDocument :: FilePath -> PdfDoc -> Editor -> Editor
addPdfDocument path pd ed =
  touchRecent path ed { edAfter = edAfter ed ++ [pdfDocSnapshot path pd] }

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
    in ed { edPdf = Just (Pdf.pdfRelayout (tabWidthOf ed) (loTextWidth lo)
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
    | otherwise     -> containerDocLoaded path rd ed { edBefore = edBefore ed ++ [captureDoc ed] }

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
    | otherwise     -> workbookLoaded path wb ed { edBefore = edBefore ed ++ [captureDoc ed] }

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
  , docImage = edImage ed
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
restoreDoc d ed = refreshPdf $ refreshRtf $ refreshImage $ ensureVisible ed
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
  , edImage = docImage d
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
    | otherwise     -> setLoaded path lr ed { edBefore = edBefore ed ++ [captureDoc ed] }

-- | Reload the active document from disk in place (the Revert command),
-- discarding unsaved edits. Behaves like a fresh load but keeps the cursor where
-- it still fits and reports it as a revert.
revertLoaded :: FilePath -> LoadResult -> Editor -> Editor
revertLoaded path lr ed =
  let cur = edCursor ed
      ed1 = setLoaded path lr ed
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
  touchRecent path ed { edAfter = edAfter ed ++ [docFromLoad (edEditSeq ed) path lr] }

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
  , docImage = Nothing
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
imageLoaded :: FilePath -> [(Image, Int)] -> Editor -> Editor
imageLoaded path frames ed = touchRecent path $ refreshImage $ ensureVisible ed
  { edBuffer = emptyBuffer, edSavedBuffer = emptyBuffer
  , edCursor = origin, edSelAnchor = Nothing, edDesiredCol = 0
  , edTop = 0, edLeft = 0
  , edPath = Just path, edModified = False
  , edDiskMtime = Nothing, edDiskChanged = False
  , edLineEnding = LF, edSavedEol = LF, edEncoding = Utf8, edSavedEnc = Utf8
  , edFinalNewline = True
  , edReadOnly = True
  , edUndo = Seq.empty, edRedo = Seq.empty, edLastEdit = EKNone
  , edStatus = T.pack ("Viewing image  " ++ imgFmt img ++ " "
                ++ show (imgW img) ++ "x" ++ show (imgH img)
                ++ (if nframes > 1 then ", " ++ show nframes ++ " frames" else "")
                ++ "  —  press 'a' for ASCII/colour")
    -- Opened from the file-explorer panel: keep the selection focus there
    -- (an image view has no keystroke editing to hand the focus to); every
    -- other open route arrives here with FEdit or a modal focus.
  , edFocus = if edFocus ed == FExplorer then FExplorer else FEdit
  , edDialog = Nothing, edSearchMode = False
  , edDefPick = Nothing, edQuickOpen = Nothing, edComplete = Nothing
  , edCsv = Nothing, edCsvStash = Nothing
  , edImage = Just (mkImageDoc frames)
  }
  where img = case frames of ((i, _) : _) -> i
                             []           -> error "imageLoaded: empty frame list"
        nframes = length frames

-- | Open a decoded image as a new document (reusing a pristine empty buffer if
-- present), mirroring 'setLoadedNew' for text.
imageLoadedNew :: FilePath -> [(Image, Int)] -> Editor -> Editor
imageLoadedNew path frames ed = case switchToOpen path ed of
  Just ed'
    | edFocus ed == FExplorer -> ed' { edFocus = FExplorer }  -- see 'imageLoaded'
    | otherwise               -> ed'
  Nothing
    | isPristine ed -> imageLoaded path frames ed
    | otherwise     -> imageLoaded path frames ed { edBefore = edBefore ed ++ [captureDoc ed] }

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
  , edCsv = Nothing, edCsvStash = Nothing, edImage = Nothing, edRtf = Nothing, edSheets = Nothing
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
    | otherwise     -> pagerLoaded pg ed { edBefore = edBefore ed ++ [captureDoc ed] }

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
addImageDocument :: FilePath -> [(Image, Int)] -> Editor -> Editor
addImageDocument path frames ed =
  touchRecent path ed { edAfter = edAfter ed ++ [imageDocSnapshot path frames] }

imageDocSnapshot :: FilePath -> [(Image, Int)] -> Document
imageDocSnapshot path frames = Document
  { docBuffer = emptyBuffer, docSavedBuffer = emptyBuffer, docCursor = origin
  , docSelAnchor = Nothing, docDesiredCol = 0, docTop = 0, docLeft = 0
  , docPath = Just path, docModified = False
  , docDiskMtime = Nothing, docDiskChanged = False
  , docLineEnding = LF, docSavedEol = LF, docEncoding = Utf8, docSavedEnc = Utf8
  , docFinalNewline = True, docReadOnly = True
  , docUndo = Seq.empty, docRedo = Seq.empty, docLastEdit = EKNone, docOverwrite = False
  , docDiscard = False, docCsv = Nothing, docCsvStash = Nothing
  , docImage = Just (mkImageDoc frames)
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
  , docImage = Nothing, docRtf = Nothing, docSheets = Nothing, docPdf = Nothing
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
refreshImage ed = case edImage ed of
  Nothing -> ed
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
         then ed { edImage = Just idoc
                     { idCache = Just (cols, rows, idMode idoc, crop, pxk, idFrame idoc
                                      , renderImage (cellAspect ed) (imageFitCap ed idoc) (idMode idoc) cols rows crop (idImage idoc)) } }
         else ed

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

-- | The current view rectangle in source pixels (the whole image when unzoomed).
imageCrop :: ImageDoc -> (Int, Int, Int, Int)
imageCrop idoc = case idCrop idoc of
  Just r  -> r
  Nothing -> (0, 0, imgW (idImage idoc), imgH (idImage idoc))

-- Update the active image doc (no-op if there isn't one).
modImage :: (ImageDoc -> ImageDoc) -> Editor -> Editor
modImage f ed = case edImage ed of
  Just d  -> ed { edImage = Just (f d) }
  Nothing -> ed

-- Reset to the full-image fit.
zoomFull :: Editor -> Editor
zoomFull = modImage (\d -> d { idCrop = Nothing, idDrag = Nothing })

-- Map a text-area cell rectangle to a source-pixel rectangle within the current
-- view, using the same fit geometry the renderer uses.
cellRectToCrop :: Editor -> ImageDoc -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
cellRectToCrop ed idoc (r0,c0,r1,c1) =
  let lo = computeLayout ed
      cols = loTextWidth lo; rows = loTextHeight lo
      img = idImage idoc
      (cx,cy,cw,ch) = imageCrop idoc
      (outW,outH,offX,offY) = viewFit (cellAspect ed) (imageFitCap ed idoc) cols rows cw ch
      fx k = fromIntegral cx + fromIntegral (k - offX) * fromIntegral cw / fromIntegral outW
      fy k = fromIntegral cy + fromIntegral (k - offY) * fromIntegral ch / fromIntegral outH
      clampD lo' hi v = max lo' (min hi v) :: Double
      x0d = clampD (fromIntegral cx) (fromIntegral (cx+cw)) (fx c0)
      x1d = clampD (fromIntegral cx) (fromIntegral (cx+cw)) (fx (c1+1))
      y0d = clampD (fromIntegral cy) (fromIntegral (cy+ch)) (fy (2*r0))
      y1d = clampD (fromIntegral cy) (fromIntegral (cy+ch)) (fy (2*r1+2))
      x0 = floor x0d; x1 = ceiling x1d
      y0 = floor y0d; y1 = ceiling y1d
      nw = max 1 (min (imgW img - x0) (x1 - x0))
      nh = max 1 (min (imgH img - y0) (y1 - y0))
  in (max 0 x0, max 0 y0, nw, nh)

setImgMode :: ImgMode -> Editor -> Editor
setImgMode m ed = case edImage ed of
  Just d  -> ed { edImage = Just d { idMode = m }
                , edStatus = T.pack ("Image view: " ++ modeName m) }
  Nothing -> ed
  where modeName HalfBlock = "colour (half-block)"
        modeName Ascii     = "ASCII"

-- Cycle to the next open file, wrapping around at the end.
nextFile :: Editor -> Editor
nextFile ed
  | fileCount ed <= 1 = ed { edStatus = "No other open files" }
  | otherwise = case edAfter ed of
      (d : ds) -> switchTo d ed { edBefore = edBefore ed ++ [captureDoc ed], edAfter = ds }
      [] -> case edBefore ed of      -- wrap to the first file
        (d : ds) -> switchTo d ed { edBefore = [], edAfter = ds ++ [captureDoc ed] }
        []       -> ed

prevFile :: Editor -> Editor
prevFile ed
  | fileCount ed <= 1 = ed { edStatus = "No other open files" }
  | otherwise = case reverse (edBefore ed) of
      (d : rb) -> switchTo d ed { edBefore = reverse rb, edAfter = captureDoc ed : edAfter ed }
      [] -> case reverse (edAfter ed) of   -- wrap to the last file
        (d : ra) -> switchTo d ed { edBefore = captureDoc ed : reverse ra, edAfter = [] }
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
      let allDocs = edBefore ed ++ [captureDoc ed] ++ edAfter ed
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

-- | The sessions the File menu offers, in the order it offers them —
-- newest-'sumClosed' first, at most 'sessionMenuMax'.
--
-- Two filters, both deliberate. The __live session's own key is excluded__
-- (offering to restore what is already open is noise), and the key is a
-- function of the folder, so comparing folders /is/ comparing keys. And the
-- list is __de-duplicated by folder__ so a legacy folderless session that names
-- a folder now owning its own file does not appear twice — sorted first, so the
-- newer @closed:@ is the one kept.
--
-- 'MARestoreSession' addresses this list by index, so the label and the action
-- cannot disagree.
sessionMenuList :: Editor -> [SessionSummary]
sessionMenuList ed =
  take sessionMenuMax (dedup [] (sortBy newestFirst others))
  where
    others = [ s | s <- edSessions ed, sumFolder s /= explorerRoot ed ]
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
  | otherwise     = doNew (ed { edBefore = edBefore ed ++ [captureDoc ed] })

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
  , edCsv = Nothing, edCsvStash = Nothing, edImage = Nothing, edRtf = Nothing, edSheets = Nothing
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
            | otherwise     = ed { edBefore = edBefore ed ++ [captureDoc ed] }
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
         , edCsv = Nothing, edCsvStash = Nothing, edImage = Nothing, edRtf = Nothing, edSheets = Nothing
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
  | isJust (edImage ed)  = Just "There is no text in an image to save"
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
    -- description of itself — and Save As does not ask before overwriting.
    seed (Just p) | p == manualPath  = takeFileName manualPath
                  | isArchivePath p  = p ++ ".txt"
                  | otherwise        = p
    seed Nothing  = ""

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
applyPendingJump ed = case edPendingJump ed of
  Just (p, l, c, len) | edPath ed == Just p, isNothing (edImage ed), isNothing (edPdf ed) ->
    let buf = edBuffer ed
        a = clampPos (Pos l c) buf
        b = clampPos (Pos l (c + len)) buf
    in ensureVisible ed { edCursor = b, edSelAnchor = Just a, edPendingJump = Nothing
                        , edDesiredCol = colToDisplay (tabWidthOf ed) (posCol b) (getLine' (posLine b) buf)
                        , edFocus = FEdit }
  _ -> ed

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
      fromDoc d = [ (p, docEncoding d, docLineEnding d, docFinalNewline d, docBuffer d)
                  | docModified d, Just p <- [docPath d] ]
      active = [ (p, edEncoding ed, edLineEnding ed, edFinalNewline ed, edBuffer ed)
               | edModified ed, Just p <- [edPath ed] ]
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
  | otherwise  = (restoreDoc (docs !! k) ed1 { edBefore = take k docs
                                             , edAfter  = drop (k + 1) docs })
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
  , docCsvStash = Nothing, docImage = Nothing, docPager = Nothing
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
  | otherwise = (restoreDoc (docs !! here) ed1 { edBefore = take here docs
                                               , edAfter  = drop (here + 1) docs })
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
