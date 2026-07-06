-- | Document lifecycle: loading, the open-files zipper, view modes
-- (CSV table / image), recents, navigation history, quick open and
-- the save/close/quit flows.
module Cmedit.EditorDoc where


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
import Cmedit.Manual (manualPath, manualText)
import Cmedit.Clipboard (CopyOutcome(..))
import Cmedit.Image (Image(..), ImgMode(..), renderImage, viewFit)
import Cmedit.Syntax (HlCache, CommentSyntax(..), langComment, langForPath)

import Cmedit.EditorState
import Cmedit.EditorEdit


-- Re-clamp scrolling / re-scale an image after a layout change (e.g. the panel
-- width or collapse state changed, which shifts the text area).
relayout :: Editor -> Editor
relayout = refreshImage . ensureVisible

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
        , edUndo = [], edRedo = [], edLastEdit = EKNone
        , edStatus = T.pack ("Opened " ++ path
                      ++ (if lrReadOnly lr then " [read-only]" else ""))
        , edFocus = FEdit, edDialog = Nothing, edSearchMode = False
        , edDefPick = Nothing, edQuickOpen = Nothing, edComplete = Nothing
        , edCsv = Nothing, edCsvStash = Nothing
        , edImage = Nothing
        }
      ed2 = if isCsvPath path then enterCsv ed1 else restoreRecentPos path ed1
  in touchRecent path ed2

-- Parse the current buffer into a fresh CSV table view (used when first opening
-- a file; toggles go through 'plainToCsv', which preserves the undo history).
enterCsv :: Editor -> Editor
enterCsv ed =
  ed { edCsv = Just (Csv.mkCsvView (csvDelimOf ed) (bufferToText LF False (edBuffer ed)))
     , edCsvStash = Nothing }

-- Serialise the table back into the line buffer (so plaintext / save see it).
syncCsvToBuffer :: Editor -> Editor
syncCsvToBuffer ed = case edCsv ed of
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
      bufText  = bufferToText LF False (edBuffer ed)
      base     = Csv.mkCsvView (csvDelimOf ed) bufText
      -- Reuse the stashed table model (and its undo) if the text was not edited
      -- while in plain-text view; otherwise keep the history but rebase onto the
      -- newly-parsed grid so an undo still reverts the text edit.
      v        = case edCsvStash ed of
                   Just s | Csv.csvToText s == bufText -> s
                          | otherwise                  -> Csv.rebaseHistory s base
                   Nothing -> base
      (r, cc)  = Csv.textPosCell v l c
  in (csvPut (Csv.setCursor r cc v) ed { edCsvStash = Nothing }) { edStatus = "Table mode" }

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
  , docHlCache = edHlCache ed
  }

-- Make a saved 'Document' the active one.
restoreDoc :: Document -> Editor -> Editor
restoreDoc d ed = refreshImage $ ensureVisible ed
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
  , edHlCache = docHlCache d
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
addDocument path lr ed = touchRecent path ed { edAfter = edAfter ed ++ [docFromLoad path lr] }

docFromLoad :: FilePath -> LoadResult -> Document
docFromLoad path lr = Document
  { docBuffer = lrBuffer lr, docSavedBuffer = lrBuffer lr, docCursor = origin, docSelAnchor = Nothing
  , docDesiredCol = 0, docTop = 0, docLeft = 0
  , docPath = Just path, docModified = False
  , docDiskMtime = lrMtime lr, docDiskChanged = False
  , docLineEnding = lrLineEnding lr, docSavedEol = lrLineEnding lr
  , docEncoding = lrEncoding lr, docSavedEnc = lrEncoding lr
  , docFinalNewline = lrFinalNewline lr, docReadOnly = lrReadOnly lr
  , docUndo = [], docRedo = [], docLastEdit = EKNone, docOverwrite = False
  , docDiscard = False
  , docCsvStash = Nothing
  , docImage = Nothing
  , docHlCache = Nothing
  , docCsv = if isCsvPath path
               then Just (Csv.mkCsvView (csvDelimForPath path)
                            (bufferToText LF False (lrBuffer lr)))
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
  , edUndo = [], edRedo = [], edLastEdit = EKNone
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
  , docUndo = [], docRedo = [], docLastEdit = EKNone, docOverwrite = False
  , docDiscard = False, docCsv = Nothing, docCsvStash = Nothing
  , docImage = Just (mkImageDoc frames)
  , docHlCache = Nothing
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
         else (ed1, [EffSetTitle (windowTitle ed1)])

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

-- Splice the recent-files section into the File menu, just above Exit.
addRecentEntries :: Editor -> [MenuEntry] -> [MenuEntry]
addRecentEntries ed es = case recentMenuEntries ed of
  [] -> es
  rs -> case break isExit es of
    (pre, exit@(_ : _)) -> pre ++ [MESep] ++ rs ++ [MESep] ++ exit
    (_, [])             -> es ++ [MESep] ++ rs
  where
    isExit (MEItem _ _ MAExit) = True
    isExit _                   = False

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

doNew :: Editor -> Editor
doNew ed = ensureVisible ed
  { edBuffer = emptyBuffer, edSavedBuffer = emptyBuffer, edCursor = origin, edSelAnchor = Nothing
  , edDesiredCol = 0, edTop = 0, edLeft = 0
  , edPath = Nothing, edModified = False
  , edDiskMtime = Nothing, edDiskChanged = False
  , edLineEnding = LF, edSavedEol = LF, edEncoding = Utf8, edSavedEnc = Utf8
  , edFinalNewline = True, edReadOnly = False
  , edUndo = [], edRedo = [], edLastEdit = EKNone
  , edStatus = "New file", edFocus = FEdit, edDialog = Nothing, edSearchMode = False
  , edDefPick = Nothing, edQuickOpen = Nothing, edComplete = Nothing
  , edCsv = Nothing, edCsvStash = Nothing, edImage = Nothing
  }

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
         , edUndo = [], edRedo = [], edLastEdit = EKNone
         , edStatus = manualStatus
         , edFocus = FEdit, edDialog = Nothing, edSearchMode = False
         , edDefPick = Nothing, edQuickOpen = Nothing, edComplete = Nothing
         , edCsv = Nothing, edCsvStash = Nothing, edImage = Nothing
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

saveAsDialogFlow :: Editor -> Editor
saveAsDialogFlow ed = openDialog (mkSaveAs (T.pack (seed (edPath ed)))) ed
  where
    -- The manual's pseudo-path is not writable; offer a plain filename instead.
    seed (Just p) | p == manualPath = takeFileName manualPath
                  | otherwise       = p
    seed Nothing  = ""

gotoLine :: Text -> Editor -> Editor
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
  Just (p, l, c, len) | edPath ed == Just p, isNothing (edImage ed) ->
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
savedAll :: [(FilePath, Maybe DiskTime)] -> Editor -> Editor
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
  -- untitled files that Save All couldn't write).
  in if edQuitting ed2 then quitStep ed2 else ed2

-- | Alt+Left: go back to the previous location (pushing the current one onto
-- the forward trail). Stops in untitled buffers are only reachable while that
-- buffer is still active; dead ones are dropped.
navBack :: Editor -> (Editor, [Effect])
navBack ed = case edNavBack ed of
  [] -> noEff ed { edStatus = "No earlier location" }
  (s : rest)
    | not (stopReachable s ed) -> navBack ed { edNavBack = rest }
    | otherwise ->
        gotoStop s ed { edNavBack = rest
                      , edNavFwd = take maxNavStops (currentStop ed : edNavFwd ed) }

-- | Alt+Right: re-visit a location undone by Go Back.
navFwd :: Editor -> (Editor, [Effect])
navFwd ed = case edNavFwd ed of
  [] -> noEff ed { edStatus = "No later location" }
  (s : rest)
    | not (stopReachable s ed) -> navFwd ed { edNavFwd = rest }
    | otherwise ->
        gotoStop s ed { edNavFwd = rest
                      , edNavBack = take maxNavStops (currentStop ed : edNavBack ed) }

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

-- Width of the row-number gutter for a given table.
csvGutterWidthFor :: CsvView -> Int
csvGutterWidthFor v = max 3 (length (show (Csv.nRows v)) + 1)

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
       -- -1: the rightmost column is the scrollbar's (as in 'computeLayout')
     , max 1 (loCols lo - loContentLeft lo - csvGutterWidthFor v - 1) )

-- Set the table view, scrolling so the current cell is visible.
csvPut :: CsvView -> Editor -> Editor
csvPut v ed = let (rv, fr, w) = csvViewportFor ed v
              in ed { edCsv = Just (Csv.ensureVisible rv fr w v) }

-- A mutating table change (marks the document modified).
-- Apply a new table view and recompute the modified flag by comparing the grid
-- against the saved state, so editing a cell back to its original value clears
-- the flag. The comparison short-circuits at the first changed cell, and is
-- skipped entirely for very large tables to keep per-keystroke editing snappy.
csvMod :: CsvView -> Editor -> Editor
csvMod v ed = (csvPut v ed) { edModified = Csv.isModified v || metaModified ed, edStatus = "" }

-- Kept as a named alias for the undo/redo call sites; 'Csv.isModified' is
-- pointer-accelerated, so the exact reconciliation is cheap at any table size
-- (there is no longer a big-table cutoff that fakes the flag).
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
