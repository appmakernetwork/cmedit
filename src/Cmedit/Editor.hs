-- | The editor model and its pure update function. All editing, navigation,
-- selection, menu and dialog logic lives here as pure transformations of
-- 'Editor'. Anything that needs the outside world (clipboard, files, quitting)
-- is requested by returning an 'Effect' for the IO driver in "Cmedit.App" to
-- carry out.
module Cmedit.Editor
  ( -- * State
    Editor(..)
  , Config(..)
  , defaultConfig
  , Focus(..)
  , EditKind(..)
  , NavStop(..)
  , UndoState(..)
  , Document(..)
  , newEditor
  , addDocument
  , fileCount
  , fileIndex
  , entriesFor
    -- * Recent files
  , touchRecent
  , recordRecent
  , recentsForPersist
  , recentMenuPaths
    -- * Image view mode
  , ImageDoc(..)
  , mkImageDoc
  , imageLoaded
  , imageLoadedNew
  , addImageDocument
  , refreshImage
  , imageCrop
  , imageAnim
  , imageKittyAnim
  , imageTickUs
  , tickImage
    -- * Terminal-reported appearance (driver callbacks)
  , resolvedTheme
  , setDetectedDark
  , setCellPx
  , cellAspect
  , cellPxKey
  , imageFitCap
  , setGfxCaps
  , imageOverlayActive
  , pointerShapeFor
    -- * Layout (shared with the renderer)
  , Layout(..)
  , computeLayout
  , DRow(..)
  , dialogRows
  , dialogGeom
  , fieldRowIndex
  , fieldLineWidth
  , fieldVisH
    -- * Effects
  , Effect(..)
    -- * Driving the model
  , update
  , resize
  , openManual
    -- * IO callbacks (results handed back by the driver)
  , setLoaded
  , setLoadedNew
  , revertLoaded
  , noteDiskMtime
  , noteDiskMtimes
  , onSaved
  , setError
  , setStatus
  , applyPaste
  , confirmCopyOutcome
  , startBrowser
  , browserLoaded
  , browserBox
  , browserTreeHeight
    -- * File explorer panel (workspace sidebar)
  , FileMark(..)
  , explorerStart
  , explorerLoaded
  , explorerTreeHeight
  , explorerRootName
  , explorerCloseCol
  , explorerCollapseCol
  , fileMarkFor
  , sidebarWidth
    -- * Explorer file management (driver callbacks)
  , fileOpDone
  , selectInExplorer
  , renamePaths
    -- * Workspace search panel
  , ReplaceReq(..)
  , searchViewActive
  , searchRunning
  , searchRegion
  , searchSeed
  , searchFileFound
  , searchProgress
  , searchDone
  , searchTick
  , searchOpenDocs
  , replaceDone
  , addStagedDoc
  , stageReplaceDone
  , modifiedDocPaths
  , modifiedDocsToSave
  , saveAll
  , savedAll
  , applyPendingJump
  , SearchCtl(..)
  , findLineCtls
  , searchFieldValueCol
  , replaceAllLabel
  , replaceSubst
    -- * Go to Definition
  , goToDefinition
  , defFound
  , defDone
  , defPickGeom
  , defPickViewH
    -- * Quick open (Ctrl+P)
  , quickOpenSeed
  , quickFilesFound
  , quickDone
  , quickOpenGeom
  , quickOpenViewH
    -- * Word completion (Ctrl+Space)
  , Complete(..)
  , completeGeom
    -- * Background file loading (spinner)
  , beginLoading
  , tickLoading
  , endLoading
  , spinnerFrames
    -- * About-box animation
  , openAbout
  , tickAbout
  , aboutAnimating
  , maxOpenBytes
  , sizeLabelThreshold
  , shortSize
  , humanSize
    -- * Queries used by the renderer
  , getSelection
  , currentLine
  , liveMatchSpans
  , bracketPair
  , scrollBarInfo
  , scrollThumb
  , StatusZone(..)
  , statusRightInfo
  , applySaveFixups
  , applySaveFixupsAll
  , tabWidthOf
  , windowTitle
  , lineSegs
  , segIndexOf
  , visualOffset
  , dropdownGeom
  , syncCsvToBuffer
  , csvGutterWidthFor
    -- * Pure helpers exposed for testing
  , replaceAllText
  , replaceAllStatus
  ) where

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
import Cmedit.Clipboard (CopyOutcome(..))
import Cmedit.Image (Image(..), ImgMode(..), renderImage, viewFit)
import Cmedit.Syntax (HlCache, CommentSyntax(..), langComment, langForPath)

import Cmedit.EditorState
import Cmedit.EditorEdit
import Cmedit.EditorDoc
import Cmedit.EditorFind


-- | Keys while viewing an image. Image-specific keys are handled here; global
-- shortcuts (open/close/quit/menus/help) are delegated so the user is never
-- trapped, and text-editing keys are swallowed (the doc is read-only).
handleImageKey :: Key -> ImageDoc -> Editor -> (Editor, [Effect])
handleImageKey key idoc ed = case key of
  KChar c | c == 'a' || c == 'A' ->
      noEff (refreshImage (setImgMode (otherMode (idMode idoc)) ed))
  KMouse me   -> noEff (handleImageMouse me idoc ed)
  -- Esc: zoom back out to the whole image if zoomed in, else normal Esc.
  KEsc | isJust (idCrop idoc) -> noEff (refreshImage (zoomFull ed))
  KCtrlChar _ -> handleEditKey key ed
  KAltChar _  -> handleEditKey key ed
  KFn _ _     -> handleEditKey key ed
  -- Alt+Left/Right navigation history works from the image view too.
  KArrow _ m | hasAlt m -> handleEditKey key ed
  KEsc        -> handleEditKey key ed
  _           -> noEff ed
  where otherMode HalfBlock = Ascii
        otherMode Ascii     = HalfBlock

-- | Mouse on the image view: left-drag selects a rectangle that becomes the new
-- (zoomed) view; a single click — or Esc — snaps back to the whole image.
handleImageMouse :: MouseEvent -> ImageDoc -> Editor -> Editor
handleImageMouse me idoc ed
  | meButton me == MBWheelUp || meButton me == MBWheelDown = ed   -- ignore wheel
  -- press: begin a drag if inside the image area
  | mePressed me && not (meDrag me) && meButton me == MBLeft =
      if inArea then modImage (\d -> d { idDrag = Just (row, col, row, col) }) ed else ed
  -- motion while a drag is active: grow the rectangle (clamped to the area)
  | meDrag me, Just (ar, ac, _, _) <- idDrag idoc =
      modImage (\d -> d { idDrag = Just (ar, ac, clampRow row, clampCol col) }) ed
  -- release: commit the rectangle (zoom) or treat as a click (reset)
  | not (mePressed me) && isJust (idDrag idoc) = finishImageDrag idoc ed
  | otherwise = ed
  where
    lo   = computeLayout ed
    rows = loTextHeight lo
    cols = loTextWidth lo
    row  = meRow me - loTextTop lo
    col  = meCol me - loTextLeft lo
    inArea = row >= 0 && row < rows && col >= 0 && col < cols
    clampRow r = max 0 (min (rows-1) r)
    clampCol c = max 0 (min (cols-1) c)

finishImageDrag :: ImageDoc -> Editor -> Editor
finishImageDrag idoc ed =
  case idDrag idoc of
    Nothing -> ed
    Just (ar, ac, br, bc) ->
      let r0 = min ar br; r1 = max ar br; c0 = min ac bc; c1 = max ac bc
          ed1 = modImage (\d -> d { idDrag = Nothing }) ed
      in refreshImage $
         if (r1 - r0) >= 1 && (c1 - c0) >= 1
           then modImage (\d -> d { idCrop = Just (cellRectToCrop ed idoc (r0,c0,r1,c1)) }) ed1
           else modImage (\d -> d { idCrop = Nothing }) ed1   -- a click: zoom out fully

-- | Entries for a menu: the dynamic open-files list for the Window menu,
-- otherwise the static definition. Used by both navigation and rendering.
entriesFor :: Editor -> Int -> [MenuEntry]
entriesFor ed mi
  | isWindowMenu mi = windowEntries ed
  | isFileMenu mi   = addRecentEntries ed (pruneEntries ed (entriesOf mi))
  | otherwise       = pruneEntries ed (entriesOf mi)

-- Hide context-dependent items: the File menu's "Revert" unless the active file
-- can be reverted, the View menu's "Table View" unless the active file is a CSV,
-- and the Edit menu's "Delete" unless there is a selection.
pruneEntries :: Editor -> [MenuEntry] -> [MenuEntry]
pruneEntries ed = map (relabelEntry ed)
                    . dropRevert . dropCloseFolder . dropSaveAll . dropImageFind . dropDelete
                    . dropLineOps . dropFileProps . dropToggles . dropTV
  where
    -- Line ending / BOM are meaningless for a read-only image.
    dropFileProps es
      | isJust (edImage ed) = tidySeps (filter keep es)
      | otherwise = es
      where keep (MEItem _ _ a) = a `notElem` [MACycleLineEnding, MAToggleBom]
            keep MESep          = True
    -- Line operations act on the text buffer, which is stale/absent in the
    -- table and image views — hide the whole group there.
    dropLineOps es
      | isJust (edCsv ed) || isJust (edImage ed) = tidySeps (filter keep es)
      | otherwise = es
      where keep (MEItem _ _ a) = a `notElem` lineOpActions
            keep MESep          = True
    dropTV es   = if isCsvFile ed then es else dropTableView es
    -- In the read-only image view there is no text to search: drop the in-file
    -- Find / Replace / Go to Line entries (workspace Find/Replace in Files stay).
    dropImageFind es
      | isJust (edImage ed) =
          let es' = filter keep es
          in if length es' == length es then es else tidySeps es'
      | otherwise = es
      where keep (MEItem _ _ a) = a `notElem` imageDisabledFind
            keep MESep          = True
    -- Save All only appears when more than one file is open and something is
    -- unsaved (there's nothing to "save all" of a single file).
    dropSaveAll es
      | fileCount ed > 1 && anyDocModified ed = es
      | otherwise = filter (\e -> case e of MEItem _ _ MASaveAll -> False; _ -> True) es
    dropCloseFolder es
      | isJust (edExplorer ed) = es
      | otherwise = filter (\e -> case e of MEItem _ _ MACloseFolder -> False; _ -> True) es
    -- In the table view the text-rendering toggles do nothing, so hide them.
    dropToggles es = if isJust (edCsv ed) then dropTextToggles es else es
    dropDelete es
      | isJust (getSelection ed) = es
      | otherwise = filter (\e -> case e of MEItem _ _ MADelete -> False; _ -> True) es
    dropRevert es
      | revertAvailable ed = es
      | otherwise = filter (\e -> case e of MEItem _ _ MARevert -> False; _ -> True) es

-- Drop the text-only View toggles (word wrap, line numbers, whitespace), plus a
-- separator left dangling at the end. Used in table view, where they're inert.
dropTextToggles :: [MenuEntry] -> [MenuEntry]
dropTextToggles es = dropTrailingSep (filter (not . isTextToggle) es)
  where
    isTextToggle (MEItem _ _ a) =
      a == MAToggleWordWrap || a == MAToggleLineNumbers || a == MAToggleWhitespace
    isTextToggle _ = False
    dropTrailingSep xs = case reverse xs of
      (MESep : rest) -> reverse rest
      _              -> xs

-- The in-file find actions that don't apply to a read-only image (there's no
-- text to search or line to jump to). Workspace Find/Replace in Files still do.
imageDisabledFind :: [MenuAction]
imageDisabledFind = [MAFind, MAFindNext, MAFindPrev, MAReplace, MAGoToLine, MAGoToDef, MAGoToBracket]

-- Rewrite value-carrying menu labels to show the document's current setting.
relabelEntry :: Editor -> MenuEntry -> MenuEntry
relabelEntry ed e = case e of
  MEItem _ acc MACycleLineEnding ->
    MEItem (T.pack ("Line E&ndings: " ++ eolName (edLineEnding ed))) acc MACycleLineEnding
  MEItem _ acc MAToggleBom ->
    MEItem (T.pack ("&UTF-8 BOM: " ++ (if edEncoding ed == Utf8Bom then "on" else "off"))) acc MAToggleBom
  MEItem _ acc MAToggleTheme ->
    MEItem (T.pack ("The&me: " ++ themeLabel (cfgTheme (edConfig ed)))) acc MAToggleTheme
  _ -> e

-- The Edit menu's line-operation group (hidden outside the plain-text view).
lineOpActions :: [MenuAction]
lineOpActions = [ MADuplicateLine, MAMoveLineUp, MAMoveLineDown, MADeleteLine
                , MAJoinLines, MAToggleComment ]

-- Drop leading, trailing and consecutive separators (after entries were removed).
tidySeps :: [MenuEntry] -> [MenuEntry]
tidySeps es = reverse (dropWhile isSep (reverse (collapse (dropWhile isSep es))))
  where
    isSep MESep = True
    isSep _     = False
    collapse (MESep : rest@(MESep : _)) = collapse rest
    collapse (x : rest)                 = x : collapse rest
    collapse []                         = []

-- Remove the table-only View entries (Table View, Freeze Header) and the
-- separator after them. These actions only appear at the head of the View menu.
dropTableView :: [MenuEntry] -> [MenuEntry]
dropTableView es = case span isTableItem es of
  ([], _)            -> es                 -- not the View menu; leave it alone
  (_,  MESep : rest) -> rest               -- also drop the separator that followed
  (_,  rest)         -> rest
  where
    isTableItem (MEItem _ _ a) =
      a == MAToggleCsv || a == MAToggleFreezeHeader || a == MASortColumn
    isTableItem _              = False

windowEntries :: Editor -> [MenuEntry]
windowEntries ed =
  [ MEItem (T.pack (mark k ++ show (k + 1) ++ " " ++ dirty m ++ lbl)) "" (MASwitchFile k)
  | (k, (lbl, m)) <- zip [0 ..] (openDocInfo ed) ]
  where
    active = length (edBefore ed)
    mark k = if k == active then "\x2713 " else "  "    -- active file
    dirty m = if m then "\x25cf " else "  "             -- unsaved changes (●)

-- The open files in order, each as (display name, has-unsaved-changes). The
-- active file's flag is the live 'edModified'; the rest use their snapshots.
openDocInfo :: Editor -> [(String, Bool)]
openDocInfo ed =
  map docInfo (edBefore ed) ++ [(activeLabel, edModified ed)] ++ map docInfo (edAfter ed)
  where
    docInfo d = (maybe "untitled" takeFileName (docPath d), docModified d)
    activeLabel = maybe "untitled" takeFileName (edPath ed)

------------------------------------------------------------------------------
-- Menu actions

runAction :: MenuAction -> Editor -> (Editor, [Effect])
runAction a ed0 =
  let ed = leaveMenu ed0
  in if isJust (edImage ed) && a `elem` imageDisabledFind
       -- In-file find is meaningless on a read-only image (also blocks the
       -- keyboard shortcuts Ctrl+F/R/G and F3, which bypass the pruned menu).
       then noEff ed { edStatus = "Not available in image view" }
     else case a of
       MANew        -> noEff (newFileFlow ed)
       MAOpen       -> openBrowser ed
       MAQuickOpen  -> openQuickOpen ed
       MAPalette    -> openPalette ed
       MAOpenFolder -> openBrowserWith True ed
       MACloseFolder -> closeFolderFlow ed
       MASave       -> save ed
       MASaveAs     -> noEff (saveAsDialogFlow ed)
       MASaveAll    -> saveAll ed
       MARevert     -> revert ed
       MACloseFile  -> noEff (closeFlow ed)
       MAExit       -> quit ed
       MAUndo       -> noEff (undo ed)
       MARedo       -> noEff (redo ed)
       MACut        -> cut ed
       MACopy       -> copy ed
       MAPaste      -> (ed, [EffPaste])
       MADelete     -> noEff (deleteForwardOrSel ed)
       MASelectAll  -> noEff (selectAll ed)
       MADuplicateLine -> noEff (duplicateLinesDir True ed)
       MAMoveLineUp    -> noEff (moveLines (-1) ed)
       MAMoveLineDown  -> noEff (moveLines 1 ed)
       MADeleteLine    -> noEff (deleteLines ed)
       MAJoinLines     -> noEff (joinLines ed)
       MAToggleComment -> noEff (toggleComment ed)
       MAFind       -> noEff (openFind ed)
       MAFindNext   -> noEff (findAgain True ed)
       MAFindPrev   -> noEff (findAgain False ed)
       MAReplace    -> noEff (openReplace ed)
       MAFindInFiles    -> openSearchPanel False ed
       MAReplaceInFiles -> openSearchPanel True ed
       MAGoToDef    -> goToDefinition (edCursor ed) ed
       MAGoToLine   -> noEff (openGoTo ed)
       MAGoToBracket -> noEff (gotoBracket ed)
       MANavBack    -> navBack ed
       MANavFwd     -> navFwd ed
       MAToggleWordWrap    -> noEff (toggleWrap ed)
       MAToggleLineNumbers -> noEff (ensureVisible ed { edShowLineNumbers = not (edShowLineNumbers ed)
                                                      , edStatus = "" })
       MAToggleWhitespace  -> noEff ed { edShowWhitespace = not (edShowWhitespace ed) }
       MAToggleCsv  -> noEff (toggleCsv ed)
       MAToggleFreezeHeader -> noEff (toggleFreezeHeader ed)
       MASortColumn -> noEff (sortCsvColumn ed)
       MACycleLineEnding -> noEff (cycleLineEnding ed)
       MAToggleBom       -> noEff (toggleBom ed)
       MAToggleTheme     -> noEff (toggleTheme ed)
       MAToggleExplorer -> toggleExplorer ed
       MASwitchFile k ->
         let target = case drop k (allOpenDocs ed) of
               (d : _) -> pushNavIfFar (docPath d) (docCursorPos d) ed
               []      -> ed
         in noEff (switchToFile k target)
       MARecentFile k -> case drop k (recentMenuPaths ed) of
                           (p : _) -> (pushNavIfFar (Just p) origin ed, [EffOpen p])
                           []      -> noEff ed
       MANextFile   -> noEff (nextFile ed)
       MAPrevFile   -> noEff (prevFile ed)
       MAAbout      -> noEff (openAbout ed)
       MAHelp       -> noEff (openHelp ed)
       MAManual     -> noEff (openManual ed)
       MANop        -> noEff ed

toggleWrap :: Editor -> Editor
toggleWrap ed = ed { edWordWrap = not (edWordWrap ed)
                   , edStatus = if edWordWrap ed then "Word wrap off" else "Word wrap on" }

-- | Alt+S / View \x25b8 Sort by Column: sort the table rows by the current
-- column — ascending first, descending when already ascending. Respects the
-- frozen header row.
sortCsvColumn :: Editor -> Editor
sortCsvColumn ed = case edCsv ed of
  Nothing
    | isCsvFile ed -> ed { edStatus = "Sorting works in the table view (Alt+T to switch)" }
    | otherwise    -> ed { edStatus = "Sorting works in CSV table view" }
  Just v
    | edReadOnly ed -> ed { edStatus = "File is read-only" }
    | Csv.nRows v < 2 -> ed { edStatus = "Nothing to sort" }
    | otherwise ->
        let c = csvCurCol v
            keepHdr = edFreezeHeader ed
            asc = not (Csv.sortedAscBy c keepHdr v)
            v' = Csv.sortByColumn c asc keepHdr v
        in (csvMod v' ed)
             { edStatus = T.pack ("Sorted by " ++ Csv.colLabel c
                                  ++ (if asc then " ascending" else " descending")) }

-- Toggle pinning the first table row so the header stays visible while scrolling.
toggleFreezeHeader :: Editor -> Editor
toggleFreezeHeader ed =
  let ed1 = ed { edFreezeHeader = not (edFreezeHeader ed)
               , edStatus = if edFreezeHeader ed then "Header row unfrozen" else "Header row frozen" }
  in case edCsv ed1 of
       Just v  -> csvPut v ed1     -- re-scroll under the new freeze setting
       Nothing -> ed1

------------------------------------------------------------------------------
-- The update entry point

update :: Key -> Editor -> (Editor, [Effect])
-- While a file is loading in the background, swallow input so nothing races the
-- load (the driver applies the result and clears the flag). Resize/EOF are
-- handled by the driver before reaching here, so they still work.
update _ ed | isJust (edLoading ed) = noEff ed
update key ed =
  let (ed', effs) = dispatchKey key ed
  -- When a menu has just been opened, refresh the active file's stale-on-disk
  -- flag so the File menu can offer Revert if the file changed underneath us.
  in case edPath ed' of
       Just p | edFocus ed /= FMenu && edFocus ed' == FMenu -> (ed', effs ++ [EffStatFile p])
       _                                                    -> (ed', effs)

dispatchKey :: Key -> Editor -> (Editor, [Effect])
-- Terminal focus reports are the driver's cue to refresh disk state; they must
-- never disturb the model (whatever has focus).
dispatchKey (KFocus _) ed = noEff ed
-- Terminal query replies are likewise consumed by the driver; if one slips
-- through to the pure model it means nothing here.
dispatchKey (KReply _) ed = noEff ed
-- A panel-width drag swallows all mouse events until release, whatever the focus.
dispatchKey key ed
  | KMouse me <- key, edSidebarDrag ed = noEff (sidebarDragMove me ed)
-- A scrollbar-thumb drag swallows mouse events until release, likewise.
dispatchKey key ed
  | KMouse me <- key, edScrollDrag ed = noEff (scrollDragMove me ed)
dispatchKey key ed = case edFocus ed of
  -- The Find/Replace dialogs keep a live match count in their message line;
  -- any key other than Up/Down ends a history-browsing run.
  FDialog  -> let (ed', effs) = handleDialogKey key ed
                  ed'' = case key of
                    KArrow DUp _   -> ed'
                    KArrow DDown _ -> ed'
                    _              -> ed' { edHistPos = Nothing }
                  -- The seeded-term "replace on first keystroke" window closes
                  -- after any key at all.
                  ed3 = case edDialog ed'' of
                    Just d' | dlgPristine d' -> ed'' { edDialog = Just d' { dlgPristine = False } }
                    _ -> ed''
              in (refreshFindCount ed3, effs)
  FMenu    -> handleMenuKey key ed
  FBrowser -> handleBrowserKey key ed
  FDefPick -> handleDefPickKey key ed
  FQuickOpen -> handleQuickOpenKey key ed
  FExplorer -> handleExplorerKey key ed
  -- A press on the menu bar opens a menu over the search view (it stays drawn).
  FSearch | KMouse me <- key, menuBarPress ed me -> noEff (mouseMenuBar (meCol me) ed)
  -- A press in the explorer panel routes there even while the search view is up.
  FSearch | KMouse me <- key, not (edMouseSelecting ed), inExplorerRegion ed me -> explorerMouse me ed
  -- A press on the scrollbar column jumps/drags the results.
  FSearch | KMouse me <- key, scrollBarPress ed me -> noEff (scrollBarClick me ed)
  FSearch  -> handleSearchKey key ed
  -- A press on the menu bar opens a menu regardless of the FEdit sub-mode (text
  -- or CSV table), so it has to be handled before the text/CSV mouse split.
  FEdit | KMouse me <- key, menuBarPress ed me -> noEff (mouseMenuBar (meCol me) ed)
  -- A press in the explorer panel routes to it (and focuses it) even from FEdit,
  -- unless a text selection drag is in progress (don't hijack it mid-drag).
  FEdit | KMouse me <- key, not (edMouseSelecting ed), inExplorerRegion ed me -> explorerMouse me ed
  -- A press on the status bar's Ln/Col, INS/OVR, encoding or line-ending cells
  -- acts on that property (handled before the text/CSV/image split so it works
  -- in every document mode).
  FEdit | KMouse me <- key, not (edMouseSelecting ed), statusBarPress ed me -> statusClick (meCol me) ed
  -- A press on the scrollbar column jumps there and starts a thumb drag
  -- (before the text/CSV split so it works in both).
  FEdit | KMouse me <- key, not (edMouseSelecting ed), scrollBarPress ed me -> noEff (scrollBarClick me ed)
  -- The Ctrl+Space completion popup captures keys while it is up.
  FEdit | Just cp <- edComplete ed -> handleCompleteKey key cp ed
  -- Image view is a separate mode (like CSV) and takes priority over text/CSV.
  FEdit | Just idoc <- edImage ed -> handleImageKey key idoc ed
  FEdit    -> case edCsv ed of
                Just v  -> handleCsvKey key v ed
                Nothing -> handleEditKey key ed

-- | Handle a terminal resize: record the new size and re-clamp scrolling. The
-- image view re-scales to the new size (its render cache is keyed by size).
resize :: (Int, Int) -> Editor -> Editor
resize size ed = refreshImage (ensureVisible ed { edSize = size })

------------------------------------------------------------------------------
-- Edit-mode key handling

handleEditKey :: Key -> Editor -> (Editor, [Effect])
handleEditKey key ed = case key of
  -- Global shortcuts
  KCtrlChar 'q' -> runAction MAExit ed
  KCtrlChar 's' -> runAction MASave ed
  KCtrlChar 'o' -> runAction MAOpen ed
  KCtrlChar 'n' -> runAction MANew ed
  KCtrlChar 'p' -> runAction MAQuickOpen ed
  KCtrlChar 'w' -> runAction MACloseFile ed
  KCtrlChar 'z' -> runAction MAUndo ed
  KCtrlChar 'y' -> runAction MARedo ed
  KCtrlChar 'x' -> runAction MACut ed
  KCtrlChar 'c' -> runAction MACopy ed
  KCtrlChar 'v' -> runAction MAPaste ed
  KCtrlChar 'a' -> runAction MASelectAll ed
  KCtrlChar 'f' -> runAction MAFind ed
  KCtrlChar 'r' -> runAction MAReplace ed
  KCtrlChar 'g' -> runAction MAGoToLine ed
  KCtrlChar 'b' -> runAction MAToggleExplorer ed -- show / focus / collapse the explorer
  KCtrlChar 'k' -> cut ed                       -- nano-style cut line
  KCtrlChar 'l' -> runAction MAGoToLine ed
  KCtrlChar 'h' -> noEff (deleteWordLeft ed)    -- Ctrl+Backspace (^H): delete word back
  KCtrlChar 'd' -> runAction MADuplicateLine ed
  KCtrlChar 'j' -> runAction MAJoinLines ed     -- only reachable under the Kitty protocol (legacy ^J is Enter); Alt+J is the portable binding
  KCtrlChar '_' -> runAction MAToggleComment ed -- legacy Ctrl+/ arrives as ^_ (0x1f)
  KCtrlChar '/' -> runAction MAToggleComment ed -- Kitty-protocol Ctrl+/
  KCtrlChar ']' -> runAction MAGoToBracket ed   -- jump to matching bracket
  KCtrlChar ' ' -> noEff (startComplete ed)     -- word completion popup

  -- Ctrl+Shift+F/H kept as aliases for terminals that pass them through; the
  -- primary bindings are F4/F6 below (many terminals grab Ctrl+Shift+letter).
  KCtrlShiftChar 'f' -> runAction MAFindInFiles ed     -- workspace-wide find
  KCtrlShiftChar 'h' -> runAction MAReplaceInFiles ed  -- workspace-wide replace
  KCtrlShiftChar 's' -> runAction MASaveAs ed          -- Ctrl+Shift+S: Save As
  KCtrlShiftChar 'k' -> runAction MADeleteLine ed      -- Ctrl+Shift+K: delete line
  KCtrlShiftChar 'p' -> runAction MAPalette ed         -- Ctrl+Shift+P: command palette
  KCtrlShiftChar _   -> noEff ed

  KFn 1 _ -> runAction MAHelp ed
  KFn 3 m | hasShift m -> runAction MAFindPrev ed
          | otherwise  -> runAction MAFindNext ed
  KFn 4 _ -> runAction MAFindInFiles ed     -- workspace-wide find
  KFn 6 _ -> runAction MAReplaceInFiles ed  -- workspace-wide replace
  KFn 10 _ -> noEff (enterMenu ed)
  KFn 12 _ -> goToDefinition (edCursor ed) ed   -- Sublime's Goto Definition key

  KAltChar c -> handleAlt c ed

  -- Navigation
  KArrow DLeft  m | hasCtrl m -> noEff (moveHoriz (hasShift m) (\e -> wordLeft (edCursor e) (edBuffer e)) ed)
                  | hasAlt m, not (hasShift m) -> navBack ed    -- Alt+Left: go back
                  | otherwise -> noEff (moveHoriz (hasShift m) (\e -> moveLeft (edCursor e) (edBuffer e)) ed)
  KArrow DRight m | hasCtrl m -> noEff (moveHoriz (hasShift m) (\e -> wordRight (edCursor e) (edBuffer e)) ed)
                  | hasAlt m, not (hasShift m) -> navFwd ed     -- Alt+Right: go forward
                  | otherwise -> noEff (moveHoriz (hasShift m) (\e -> moveRight (edCursor e) (edBuffer e)) ed)
  KArrow DUp    m | hasCtrl m -> noEff (scrollLine (-1) ed)
                  | hasAlt m, hasShift m -> noEff (duplicateLinesDir False ed)  -- copy line up
                  | hasAlt m  -> noEff (moveLines (-1) ed)
                  | otherwise -> noEff (moveVert (hasShift m) (-1) ed)
  KArrow DDown  m | hasCtrl m -> noEff (scrollLine 1 ed)
                  | hasAlt m, hasShift m -> noEff (duplicateLinesDir True ed)   -- copy line down
                  | hasAlt m  -> noEff (moveLines 1 ed)
                  | otherwise -> noEff (moveVert (hasShift m) 1 ed)

  KHome m | hasCtrl m -> noEff (moveHoriz (hasShift m) (const docStart)
                                  (pushNavIfFar (edPath ed) docStart ed))
          | otherwise -> noEff (moveHoriz (hasShift m) (\e -> smartHome e) ed)
  KEnd  m | hasCtrl m -> noEff (moveHoriz (hasShift m) (\e -> docEnd (edBuffer e))
                                  (pushNavIfFar (edPath ed) (docEnd (edBuffer ed)) ed))
          | otherwise -> noEff (moveHoriz (hasShift m) (\e -> lineEnd (edCursor e) (edBuffer e)) ed)

  KPageUp   m | hasCtrl m -> noEff (prevFile ed)
              | otherwise -> noEff (moveVert (hasShift m) (negate (pageSize ed)) ed)
  KPageDown m | hasCtrl m -> noEff (nextFile ed)
              | otherwise -> noEff (moveVert (hasShift m) (pageSize ed) ed)

  KDelete m | hasCtrl m -> noEff (deleteWordRight ed)      -- Ctrl+Delete: delete word forward
            | otherwise -> noEff (deleteForwardOrSel ed)
  KInsert _ -> noEff ed { edOverwrite = not (edOverwrite ed)
                        , edStatus = if edOverwrite ed then "Insert mode" else "Overwrite mode" }

  -- Editing
  KEnter      -> noEff (newline ed)
  KModEnter   -> noEff (newline ed)         -- Ctrl/Shift+Enter behaves as a normal newline here
  KTab        -> noEff (insertTab ed)
  KBackTab    -> noEff (outdentSelection ed)
  KBackspace  -> noEff (backspace ed)
  KChar c     -> noEff (typeChar c ed)
  KPaste s    -> noEff (applyPaste s ed)

  KEsc        -> noEff (clearSel ed { edStatus = "" })
  -- Ctrl+Click on an identifier looks up its definition.
  KMouse me
    | modCtrl (meMods me), mePressed me, not (meDrag me), meButton me == MBLeft
    , Just pos <- textClickPos me ed -> goToDefinition pos ed
  KMouse me   -> noEff (handleMouse me ed)
  _           -> noEff ed

handleAlt :: Char -> Editor -> (Editor, [Effect])
handleAlt c ed = case c of
  'z' -> runAction MAToggleWordWrap ed
  'l' -> runAction MAToggleLineNumbers ed
  't' -> runAction MAToggleCsv ed
  's' -> runAction MASortColumn ed
  'j' -> runAction MAJoinLines ed
  '.' -> noEff (nextFile ed)
  ',' -> noEff (prevFile ed)
  '\DEL' -> noEff (deleteWordLeft ed)
  -- Route through the menu action so the switch lands in the navigation history.
  d | d >= '1' && d <= '9' -> runAction (MASwitchFile (fromEnum d - fromEnum '1')) ed
  _ -> case menuAccelFor c of
         Just i  -> noEff ed { edFocus = FMenu
                             , edMenu = MenuState i True (firstSelectable (entriesFor ed i)) }
         Nothing -> noEff ed

enterMenu :: Editor -> Editor
enterMenu ed = ed { edFocus = FMenu
                  , edMenu = MenuState (msMenuIx (edMenu ed)) False 0
                  , edStatus = "" }

------------------------------------------------------------------------------
-- Menu-mode key handling

handleMenuKey :: Key -> Editor -> (Editor, [Effect])
handleMenuKey key ed =
  let ms = edMenu ed in case key of
    KEsc            -> noEff (leaveMenu ed)
    KArrow DLeft _  -> noEff (menuLeft ed)
    KArrow DRight _ -> noEff (menuRight ed)
    KArrow DUp _    -> noEff (menuUp ed)
    KArrow DDown _  -> noEff (menuDown ed)
    KHome _         -> noEff ed { edMenu = ms { msItemIx = firstSelectable (entriesFor ed (msMenuIx ms)), msOpen = True } }
    KEnter          -> menuActivate ed
    KFn 10 _        -> noEff (leaveMenu ed)
    KMouse me       -> handleMenuMouse me ed
    KAltChar c      -> handleAlt c ed
    -- Keyboard shortcuts shown in the menus keep working while a menu is open.
    KCtrlChar _      -> handleEditKey key (leaveMenu ed)
    KCtrlShiftChar _ -> handleEditKey key (leaveMenu ed)
    KFn _ _          -> handleEditKey key (leaveMenu ed)
    -- A letter activates the item with that mnemonic (e.g. x -> Exit).
    KChar c         -> menuMnemonic c ed
    _               -> noEff ed

-- Activate a menu item by its mnemonic letter, or (when only the bar is
-- focused) open the menu whose title starts with that letter.
menuMnemonic :: Char -> Editor -> (Editor, [Effect])
menuMnemonic c ed =
  let ms = edMenu ed in
  if msOpen ms
    then case mnemonicItemIn (entriesFor ed (msMenuIx ms)) c of
           Just j -> case drop j (entriesFor ed (msMenuIx ms)) of
                       (MEItem _ _ act : _) -> runAction act ed
                       _                    -> noEff ed
           Nothing -> noEff ed
    else case menuAccelFor c of
           Just i  -> noEff ed { edMenu = MenuState i True (firstSelectable (entriesFor ed i)) }
           Nothing -> noEff ed

menuLeft :: Editor -> Editor
menuLeft ed =
  let ms = edMenu ed
      i' = (msMenuIx ms - 1) `mod` length menuBar
  in ed { edMenu = ms { msMenuIx = i', msItemIx = firstSelectable (entriesFor ed i') } }

menuRight :: Editor -> Editor
menuRight ed =
  let ms = edMenu ed
      i' = (msMenuIx ms + 1) `mod` length menuBar
  in ed { edMenu = ms { msMenuIx = i', msItemIx = firstSelectable (entriesFor ed i') } }

menuDown :: Editor -> Editor
menuDown ed =
  let ms = edMenu ed
      es = entriesFor ed (msMenuIx ms)
      sel = selectableIndices es
  in if not (msOpen ms)
       then ed { edMenu = ms { msOpen = True, msItemIx = firstSelectable es } }
       else ed { edMenu = ms { msItemIx = nextIn sel (msItemIx ms) } }

menuUp :: Editor -> Editor
menuUp ed =
  let ms = edMenu ed
      es = entriesFor ed (msMenuIx ms)
      sel = selectableIndices es
  in if not (msOpen ms)
       then ed { edMenu = ms { msOpen = True, msItemIx = lastDef 0 sel } }
       else ed { edMenu = ms { msItemIx = prevIn sel (msItemIx ms) } }

nextIn :: [Int] -> Int -> Int
nextIn [] x = x
nextIn xs x = case dropWhile (<= x) xs of (y : _) -> y; [] -> head xs

prevIn :: [Int] -> Int -> Int
prevIn [] x = x
prevIn xs x = case reverse (takeWhile (< x) xs) of (y : _) -> y; [] -> last xs

lastDef :: a -> [a] -> a
lastDef d [] = d
lastDef _ xs = last xs

menuActivate :: Editor -> (Editor, [Effect])
menuActivate ed =
  let ms = edMenu ed in
  if not (msOpen ms)
    then noEff ed { edMenu = ms { msOpen = True, msItemIx = firstSelectable (entriesFor ed (msMenuIx ms)) } }
    else case drop (msItemIx ms) (entriesFor ed (msMenuIx ms)) of
           (MEItem _ _ act : _) -> runAction act ed
           _                    -> noEff ed

------------------------------------------------------------------------------
-- Dialog-mode key handling

handleDialogKey :: Key -> Editor -> (Editor, [Effect])
handleDialogKey key ed = case edDialog ed of
  Nothing -> noEff (ed { edFocus = FEdit })
  Just d  -> case key of
    KEsc        -> noEff (cancelDialog ed)
    KTab        -> noEff ed { edDialog = Just (focusNext d) }
    KBackTab    -> noEff ed { edDialog = Just (focusPrev d) }
    -- Up/Down: move within a multi-line field first; then recall input
    -- history in the Find/Replace fields; else move dialog focus.
    KArrow DUp _   | Just d' <- fieldLineUp d   -> noEff ed { edDialog = Just d' }
                   | isJust (histFieldOf ed d)  -> noEff (histRecall 1 ed)
                   | otherwise                  -> noEff ed { edDialog = Just (focusPrev d) }
    KArrow DDown _ | Just d' <- fieldLineDown d -> noEff ed { edDialog = Just d' }
                   | isJust (edHistPos ed)      -> noEff (histRecall (-1) ed)
                   | otherwise                  -> noEff ed { edDialog = Just (focusNext d) }
    KArrow DLeft _  | isJust (focusedField d) -> noEff ed { edDialog = Just (fieldLeft d) }
                    | otherwise -> noEff ed { edDialog = Just (focusPrev d) }
    KArrow DRight _ | isJust (focusedField d) -> noEff ed { edDialog = Just (fieldRight d) }
                    | otherwise -> noEff ed { edDialog = Just (focusNext d) }
    KHome _     -> noEff ed { edDialog = Just (fieldHome d) }
    KEnd _      -> noEff ed { edDialog = Just (fieldEnd d) }
    KBackspace  -> noEff ed { edDialog = Just (fieldBackspace d) }
    KCtrlChar 'h' -> noEff ed { edDialog = Just (fieldDeleteWordLeft d) }
    KDelete _   -> noEff ed { edDialog = Just (fieldDelete d) }
    KChar ' ' | not (isJust (focusedField d)) && not (focusIsButton d)
                  -> noEff ed { edDialog = Just (toggleOption d) }
    -- A pristine seeded term behaves like a selected value: the first typed
    -- character replaces it (the flag is cleared by the dispatch wrapper).
    KChar c | dlgPristine d, focusedField d == Just 0
                -> noEff ed { edDialog = Just (setFieldText 0 (T.singleton c) d) }
    KChar c     -> noEff ed { edDialog = Just (fieldInsert c d) }
    KPaste s    -> noEff ed { edDialog = Just (foldl (flip fieldInsert) d (T.unpack s)) }
    KCtrlChar 'v' -> (ed, [EffPaste])                                -- paste clipboard into the focused field
    KModEnter   -> noEff ed { edDialog = Just (fieldInsert '\n' d) }  -- Shift/Ctrl+Enter: newline in field
    KEnter      -> confirmDialog d ed
    KMouse me   -> handleDialogMouse me ed
    _           -> noEff ed

-- A click inside a dialog: fire a button, focus a field, or toggle an option.
handleDialogMouse :: MouseEvent -> Editor -> (Editor, [Effect])
handleDialogMouse me ed = case edDialog ed of
  Nothing -> noEff ed
  Just d
    | not (mePressed me) || meButton me /= MBLeft -> noEff ed
    | otherwise ->
        let lo = computeLayout ed
            (y, x, h, w) = dialogGeom ed d lo
            inside = meRow me >= y && meRow me < y + h && meCol me >= x && meCol me < x + w
            -- An informational dialog (a single dismiss button, e.g. the binary-file
            -- warning or About — plus Help, whose second button only opens the
            -- Manual) closes when you click anywhere off it; a multi-button
            -- confirm stays modal so a stray click can't answer it.
            dismissable = length (dlgButtons d) <= 1 || dlgKind d == DKHelp
            rs = dialogRows d
            innerX = x + 2; innerW = w - 4
            rowIx = meRow me - (y + 1)
        in if not inside
             then if dismissable then noEff (cancelDialog ed) else noEff ed
             else if rowIx < 0 || rowIx >= length rs
             then noEff ed
             else case rs !! rowIx of
               DRButtons   -> case hitDialogButton d innerX innerW (meCol me) of
                                Just b  -> dispatchDialog (dlgKind d) b d ed
                                Nothing -> noEff ed
               DRField fi li visH -> noEff ed { edDialog = Just (clickField fi li visH innerX innerW (meCol me) d) }
               DROption oi -> noEff ed { edDialog = Just (toggleOption (setFocus (length (dlgFields d) + oi) d)) }
               _           -> noEff ed

-- Focus a field and place its cursor at the clicked cell. We map the clicked
-- screen column/line back to a text (line, column) using the same vertical
-- (winTop) and horizontal (off) scrolling the renderer applied to this field, so
-- the cursor lands on exactly the character that was clicked.
clickField :: Int -> Int -> Int -> Int -> Int -> Int -> Dialog -> Dialog
clickField fi li visH innerX innerW clickCol d =
  let f          = dlgFields d !! fi
      labelW     = T.length (fLabel f) + 1
      valStart   = innerX + labelW
      valW       = max 1 (innerW - labelW)
      focusedNow = focusedField d == Just fi
      (cl, cc)   = Csv.cursorLineCol (fText f) (fCur f)
      winTop     = if focusedNow && cl >= visH then cl - visH + 1 else 0
      textLine   = winTop + li
      off        = if focusedNow && textLine == cl && cc >= valW then cc - valW + 1 else 0
      textCol    = off + max 0 (clickCol - valStart)
  in fieldSetCursorLineCol textLine textCol (setFocus fi d)

-- Index of the button under display column @c@ on the button row, mirroring the
-- renderer's layout ("  label  " padding, one space between buttons, centred).
hitDialogButton :: Dialog -> Int -> Int -> Int -> Maybe Int
hitDialogButton d innerX innerW c =
  let btns = dlgButtons d
      total = sum [ T.length b + 4 | b <- btns ] + (length btns - 1)
      start = innerX + max 0 ((innerW - total) `div` 2)
      go _ [] = []
      go col ((i, b) : rest) = let lab = T.length b + 4 in (i, col, col + lab) : go (col + lab + 1) rest
  in case [ i | (i, lo', hi') <- go start (zip [0 ..] btns), c >= lo', c < hi' ] of
       (i : _) -> Just i
       []      -> Nothing

cancelDialog :: Editor -> Editor
cancelDialog ed =
  let base = (clearQuitState (closeDialog ed)) { edPendingClose = False, edStatus = "" }
  in case dlgKind <$> edDialog ed of
       -- The explorer's file-management prompts hand focus back to the panel.
       Just k | k `elem` [DKNewPath, DKRename, DKConfirmDelete] -> backToExplorer base
       _ -> base

-- Enter pressed: pick the focused button (or the primary one).
confirmDialog :: Dialog -> Editor -> (Editor, [Effect])
confirmDialog d ed =
  let btn = fromMaybe 0 (focusedButton d)
  in dispatchDialog (dlgKind d) btn d ed

dispatchDialog :: DialogKind -> Int -> Dialog -> Editor -> (Editor, [Effect])
dispatchDialog kind btn d ed = case kind of
  DKOpen
    | btn == 0 -> let p = T.unpack (T.strip (fieldValue 0 d))
                  in if null p then noEff (cancelDialog ed)
                     else (pushNavIfFar (Just p) origin (closeDialog ed), [EffOpen p])
    | otherwise -> noEff (cancelDialog ed)
  DKSaveAs
    | btn == 0 -> let p = T.unpack (T.strip (fieldValue 0 d))
                  in if null p then noEff (cancelDialog ed)
                     else (closeDialog ed { edPath = Just p }, [EffSaveTo p])
    | otherwise -> noEff (cancelDialog ed)
  DKGoToLine
    | btn == 0 -> noEff (gotoLine (fieldValue 0 d) (closeDialog ed))
    | otherwise -> noEff (cancelDialog ed)
  DKNewPath
    | btn == 0, name <- T.unpack (T.strip (fieldValue 0 d)), not (null name)
    , Just dir <- explorerTargetDir ed
    -> (backToExplorer (closeDialog ed), [EffCreatePath (dir ++ "/" ++ name)])
    | otherwise -> noEff (backToExplorer (cancelDialog ed))
  DKRename
    | btn == 0, name <- T.unpack (T.strip (fieldValue 0 d)), not (null name)
    , Just n <- explorerSelectedNode ed
    , let old = fnPath n
    , let new = takeDirectory old ++ "/" ++ name
    , new /= old
    -> (backToExplorer (closeDialog ed), [EffRenamePath old new])
    | otherwise -> noEff (backToExplorer (cancelDialog ed))
  DKConfirmDelete
    | btn == 0, Just n <- explorerSelectedNode ed
    -> (backToExplorer (closeDialog ed), [EffDeletePath (fnPath n)])
    | otherwise -> noEff (backToExplorer (cancelDialog ed))
  DKFind
    | btn == 0 -> noEff (doFind (closeDialog (storeSearch d ed)))
    | otherwise -> noEff (cancelDialog ed)
  DKReplace
    | btn == 0 -> noEff (replaceOne (closeDialog (storeReplace d ed)))
    | btn == 1 -> noEff (replaceAll (closeDialog (storeReplace d ed)))
    | otherwise -> noEff (cancelDialog ed)
  DKConfirmQuit  -> confirmQuitSeq btn ed
  DKConfirmQuitAll -> case btn of
    0 -> (closeDialog ed, [EffSaveAll])   -- Save All, then quit (savedAll continues the quit)
    1 -> noEff ((clearQuitState (closeDialog ed)) { edQuit = True })   -- Discard All and quit
    _ -> noEff (cancelDialog ed)           -- Cancel: abort the quit
  DKConfirmSaveAll
    | btn == 0  -> (closeDialog ed, [EffSaveAll])   -- confirmed Save All
    | otherwise -> noEff (cancelDialog ed)
  DKConfirmClose -> confirmClose btn ed
  DKConfirmCloseFolder
    | btn == 0  -> noEff (closeFolder (closeDialog ed))
    | otherwise -> noEff (cancelDialog ed)
  DKConfirmRevert
    | btn == 0  -> case edPath ed of
                     Just p  -> (closeDialog ed, [EffRevert p])
                     Nothing -> noEff (cancelDialog ed)
    | otherwise -> noEff (cancelDialog ed)
  DKConfirmOverwrite
    | btn == 0 -> case edPath ed of Just p -> (closeDialog ed, [EffSaveTo p]); Nothing -> noEff (closeDialog ed)
    | otherwise -> noEff (cancelDialog ed)
  DKConfirmReplaceAll
    | btn == 0  -> case edSearch ed of
                     Just ss -> doReplace (S.resultPaths ss) (closeDialog ed)
                     Nothing -> noEff (closeDialog ed)
    | otherwise -> noEff (cancelDialog ed)
  DKAbout   -> noEff (closeDialog ed)
  DKHelp
    | btn == 0  -> noEff (openManual (closeDialog ed))
    | otherwise -> noEff (cancelDialog ed)
  DKMessage -> noEff (closeDialog ed)

storeSearch :: Dialog -> Editor -> Editor
storeSearch d ed = pushFindHist (fieldValue 0 d)
                     ed { edSearchTerm = fieldValue 0 d
                        , edSearchCase = optionValue 0 d
                        , edSearchWord = optionValue 1 d }

storeReplace :: Dialog -> Editor -> Editor
storeReplace d ed = pushReplHist (fieldValue 1 d) $ pushFindHist (fieldValue 0 d)
                      ed { edSearchTerm = fieldValue 0 d
                         , edReplaceTerm = fieldValue 1 d
                         , edSearchCase = optionValue 0 d }

-- One step of the multi-file quit sequence.
confirmQuitSeq :: Int -> Editor -> (Editor, [Effect])
confirmQuitSeq btn ed = case btn of
  0 -> -- Save this file, then continue to the next unsaved one (via onSaved).
    let ed1 = closeDialog ed
    in case edPath ed1 of
         Just p  -> (ed1, [EffSaveTo p])
         Nothing -> noEff (saveAsDialogFlow ed1)
  1 -> -- Don't Save: mark this file discarded and move on.
    noEff (quitStep (closeDialog ed) { edDiscard = True })
  _ -> -- Cancel: abort the whole quit.
    noEff (cancelDialog ed)

-- Confirm dialog for closing a single file (Ctrl+W).
confirmClose :: Int -> Editor -> (Editor, [Effect])
confirmClose btn ed = case btn of
  0 -> let ed1 = (closeDialog ed) { edPendingClose = True }
       in case edPath ed1 of
            Just p  -> (ed1, [EffSaveTo p])
            Nothing -> noEff (saveAsDialogFlow ed1)
  1 -> noEff (doClose (closeDialog ed))
  _ -> noEff (cancelDialog ed)

------------------------------------------------------------------------------
-- Mouse

-- A genuine left-button press on the menu bar (not a release or a drag — a
-- motion event also sets mePressed, differing only in its final byte). Handled
-- before the text/CSV split so brushing the bar mid-click can't toggle a menu.
menuBarPress :: Editor -> MouseEvent -> Bool
menuBarPress ed me =
  edShowMenu ed && mePressed me && not (meDrag me) && meButton me == MBLeft
    && meRow me == loMenuRow (computeLayout ed)

-- A left press on the scrollbar column, while a bar is showing.
scrollBarPress :: Editor -> MouseEvent -> Bool
scrollBarPress ed me = case scrollBarInfo ed of
  Just (x, top, h, _, _) ->
    mePressed me && meButton me == MBLeft && not (meDrag me)
      && meCol me == x && meRow me >= top && meRow me < top + h
  Nothing -> False

-- Jump so the thumb lands under the click, and begin a drag.
scrollBarClick :: MouseEvent -> Editor -> Editor
scrollBarClick me ed = (scrollBarTo (meRow me) ed) { edScrollDrag = True }

-- Continue / finish a thumb drag (any release ends it).
scrollDragMove :: MouseEvent -> Editor -> Editor
scrollDragMove me ed
  | not (mePressed me) = ed { edScrollDrag = False }
  | otherwise          = scrollBarTo (meRow me) ed

-- Scroll whichever view owns the bar to the track position at @row@.
scrollBarTo :: Int -> Editor -> Editor
scrollBarTo row ed = case scrollBarInfo ed of
  Nothing -> ed
  Just (_, top, h, total, _) ->
    let target = scrollTrackTarget h total (max 0 (min (h - 1) (row - top)))
    in if searchViewActive ed
         then case edSearch ed of
                Just ss -> ed { edSearch = Just ss { ssTop = target } }
                Nothing -> ed
         else case edCsv ed of
                Just v  -> csvPut (Csv.clearSel
                             (Csv.setCursor (min (Csv.nRows v - 1) target) (csvCurCol v) v)) ed
                Nothing -> scrollLine (target - edTop ed) ed

handleMouse :: MouseEvent -> Editor -> Editor
handleMouse me ed
  -- Shift+wheel (and true horizontal wheels) pan sideways; plain wheel scrolls.
  | meButton me == MBWheelUp, hasShift (meMods me)   = scrollCol (-6) ed
  | meButton me == MBWheelDown, hasShift (meMods me) = scrollCol 6 ed
  | meButton me == MBWheelLeft  = scrollCol (-6) ed
  | meButton me == MBWheelRight = scrollCol 6 ed
  | meButton me == MBWheelUp   = scrollLine (-3) ed
  | meButton me == MBWheelDown = scrollLine 3 ed
  | otherwise =
      let lo = computeLayout ed
          row = meRow me
          col = meCol me
      in if row >= loTextTop lo && row < loTextTop lo + loTextHeight lo
                  && col >= loTextLeft lo
             then mouseText me lo ed
             else ed

-- The buffer position (and display column) under a mouse event in the text
-- area — shared by click/drag handling and Ctrl+Click go-to-definition.
mouseBufPos :: MouseEvent -> Layout -> Editor -> (Pos, Int)
mouseBufPos me lo ed
  | edWordWrap ed =
      let vrow = meRow me - loTextTop lo
          d = max 0 (meCol me - loTextLeft lo)
          (l, seg) = visualRowToLineSeg ed (edTop ed) vrow
      in (posInSeg ed l seg d, d)
  | otherwise =
      let line = edTop ed + (meRow me - loTextTop lo)
          l = max 0 (min (lineCount (edBuffer ed) - 1) line)
          d = max 0 (edLeft ed + (meCol me - loTextLeft lo))
          c = displayToCol (tabWidthOf ed) d (getLine' l (edBuffer ed))
      in (Pos l c, d)

-- The buffer position under a mouse event, when it falls in the text area.
textClickPos :: MouseEvent -> Editor -> Maybe Pos
textClickPos me ed =
  let lo = computeLayout ed
  in if meRow me >= loTextTop lo && meRow me < loTextTop lo + loTextHeight lo
        && meCol me >= loTextLeft lo
       then Just (fst (mouseBufPos me lo ed))
       else Nothing

mouseText :: MouseEvent -> Layout -> Editor -> Editor
mouseText me lo ed =
  let (pos, dcol) = mouseBufPos me lo ed
  in if mePressed me && not (meDrag me)
       then case meClicks me of
              n | n >= 3    -> selectRange (lineRangeAt pos (edBuffer ed)) ed   -- triple: line
                | n == 2    -> selectRange (wordRangeAt pos (edBuffer ed)) ed   -- double: word
                | otherwise -> ensureVisible ed { edCursor = pos, edSelAnchor = Just pos
                                                , edDesiredCol = dcol, edMouseSelecting = True }
       else if meDrag me && edMouseSelecting ed
         then ensureVisible ed { edCursor = pos, edDesiredCol = dcol }
         -- Release (or a stray event): a plain click leaves anchor == cursor;
         -- drop that empty selection so the next keystroke just inserts.
         else ed { edMouseSelecting = False, edSelAnchor = dropEmptySel ed }

-- Select a (start, end) range, putting the cursor at the end (used by
-- double/triple-click word/line selection).
selectRange :: (Pos, Pos) -> Editor -> Editor
selectRange (a, e) ed = ensureVisible ed
  { edSelAnchor = Just a, edCursor = e, edMouseSelecting = False
  , edDesiredCol = colToDisplay (tabWidthOf ed) (posCol e) (getLine' (posLine e) (edBuffer ed))
  , edStatus = "" }

-- Discard a zero-width selection (anchor sitting on the cursor).
dropEmptySel :: Editor -> Maybe Pos
dropEmptySel ed = case edSelAnchor ed of
  Just a | a == edCursor ed -> Nothing
  other                     -> other

-- Map a visual row in the viewport to a (buffer line, segment) under wrap.
visualRowToLineSeg :: Editor -> Int -> Int -> (Int, (Int, Int))
visualRowToLineSeg ed top vrow0 = go top (max 0 vrow0)
  where
    n = lineCount (edBuffer ed)
    go l vrow
      | l >= n = let l' = n - 1 in (l', last (lineSegs ed l'))
      | otherwise =
          let segs = lineSegs ed l
              k = length segs
          in if vrow < k then (l, segs !! vrow) else go (l + 1) (vrow - k)

mouseMenuBar :: Int -> Editor -> Editor
mouseMenuBar col ed =
  case menuHitTest col of
    Just i  -> ed { edFocus = FMenu, edMenu = MenuState i True (firstSelectable (entriesFor ed i)) }
    Nothing -> ed

-- Map an x position on the menu bar to a menu index, matching the renderer's
-- layout (" File  Edit  ... " with two leading spaces and padding).
menuHitTest :: Int -> Maybe Int
menuHitTest x = go 1 (zip [0 ..] menuBar)
  where
    go _ [] = Nothing
    go start ((i, m) : rest) =
      let w = T.length (menuTitleDisp m) + 2
      in if x >= start && x < start + w then Just i else go (start + w + 1) rest

-- Starting column of menu i on the bar (matches drawMenuBar / menuHitTest).
menuStartCol :: Int -> Int
menuStartCol target = go 1 (zip [0 ..] menuBar)
  where
    go col [] = col
    go col ((i, m) : rest)
      | i == target = col
      | otherwise   = go (col + T.length (menuTitleDisp m) + 3) rest

-- | Geometry of the currently-open dropdown: @(y0, x0, height, innerWidth)@.
-- Shared by the renderer and mouse hit-testing so they never disagree.
dropdownGeom :: Editor -> (Int, Int, Int, Int)
dropdownGeom ed =
  let lo = computeLayout ed
      cols = loCols lo
      mi = msMenuIx (edMenu ed)
      entries = entriesFor ed mi
      labelW = maximum (1 : [ T.length (fst (parseMnemonic l)) | MEItem l _ _ <- entries ])
      accelW = maximum (0 : [ T.length a | MEItem _ a _ <- entries ])
      innerW = labelW + 2 + accelW + 2
      x0 = min (menuStartCol mi) (max 0 (cols - innerW - 2))
      y0 = loMenuRow lo + 1
      h = length entries + 2
  in (y0, x0, h, innerW)

-- Mouse handling while a menu is focused: clicking the bar switches menus,
-- clicking a dropdown item activates it, clicking elsewhere closes the menu.
handleMenuMouse :: MouseEvent -> Editor -> (Editor, [Effect])
handleMenuMouse me ed
  | not (mePressed me) || meDrag me = noEff ed   -- act on presses only, not releases/drags
  | meButton me /= MBLeft = noEff ed
  | edShowMenu ed && row == loMenuRow lo =
      case menuHitTest col of
        Just i  -> noEff ed { edFocus = FMenu
                            , edMenu = MenuState i True (firstSelectable (entriesFor ed i)) }
        Nothing -> noEff (leaveMenu ed)
  | msOpen (edMenu ed) =
      let (y0, x0, h, innerW) = dropdownGeom ed
          entries = entriesFor ed (msMenuIx (edMenu ed))
          itemRow = row - (y0 + 1)
          inBox = row >= y0 + 1 && row < y0 + h - 1
                  && col >= x0 && col <= x0 + innerW + 1
      in if inBox && itemRow >= 0 && itemRow < length entries
           then case entries !! itemRow of
                  MEItem _ _ act -> runAction act (ed { edMenu = (edMenu ed) { msItemIx = itemRow } })
                  MESep          -> noEff ed
           else noEff (leaveMenu ed)
  | otherwise = noEff (leaveMenu ed)
  where
    lo = computeLayout ed
    row = meRow me
    col = meCol me

------------------------------------------------------------------------------
-- File browser (the Open dialog)

-- Box geometry for the browser: (y, x, height, width).
browserBox :: Editor -> (Int, Int, Int, Int)
browserBox ed =
  let (rows, cols) = edSize ed
      w = max 30 (min (cols - 4) 80)
      h = max 8 (min (rows - 2) 30)
      x = max 0 ((cols - w) `div` 2)
      y = max 1 ((rows - h) `div` 2)
  in (y, x, h, w)

-- Number of visible tree rows (height minus borders, header and footer).
browserTreeHeight :: Editor -> Int
browserTreeHeight ed = let (_, _, h, _) = browserBox ed in max 1 (h - 4)

-- Open the file browser, requesting a listing of the starting directory. In
-- pick-folder mode (@pick@), Enter on a directory opens it as the workspace.
openBrowser :: Editor -> (Editor, [Effect])
openBrowser = openBrowserWith False

openBrowserWith :: Bool -> Editor -> (Editor, [Effect])
openBrowserWith pick ed =
  ( (leaveMenu ed) { edFocus = FBrowser, edBrowser = Nothing, edDialog = Nothing
                   , edBrowserPick = pick, edStatus = "" }
  , [EffBrowse hint] )
  where
    -- Start at the open folder (if any), else near the active file.
    hint = firstJust [ fnPath . brRoot <$> edExplorer ed, takeDirectory <$> edPath ed ]

-- | Driver callback: the starting (or re-rooted) directory has been listed.
startBrowser :: FilePath -> [Entry] -> Editor -> Editor
startBrowser dir entries ed =
  ed { edBrowser = Just (Br.scrollInto (browserTreeHeight ed) (Br.mkBrowser dir entries))
     , edFocus = FBrowser }

-- | Driver callback: a directory was listed in response to an expand request.
browserLoaded :: FilePath -> [Entry] -> Editor -> Editor
browserLoaded path entries ed = case edBrowser ed of
  Nothing -> ed
  Just br -> ed { edBrowser = Just (Br.scrollInto (browserTreeHeight ed)
                                      (mergeKeepSel path entries br)) }

-- Install a fresh listing, keeping loaded/expanded subtrees and re-anchoring
-- the selection to the same path (a refresh may add or remove rows above it).
mergeKeepSel :: FilePath -> [Entry] -> Browser -> Browser
mergeKeepSel path entries br =
  let br' = Br.mergeChildren path entries br
  in case Br.selectedNode br of
       Just n  -> Br.selectPath (fnPath n) br'
       Nothing -> br'

closeBrowser :: Editor -> Editor
closeBrowser ed = ed { edBrowser = Nothing, edBrowserPick = False, edFocus = FEdit, edStatus = "" }

handleBrowserKey :: Key -> Editor -> (Editor, [Effect])
handleBrowserKey key ed = case edBrowser ed of
  Nothing -> case key of
    KEsc -> noEff (closeBrowser ed)
    _    -> noEff ed                         -- still loading
  Just br ->
    let th = browserTreeHeight ed
        upd b = noEff ed { edBrowser = Just (Br.scrollInto th b) }
    in case key of
         KEsc            -> noEff (closeBrowser ed)
         KArrow DUp _    -> upd (Br.moveSel (-1) br)
         KArrow DDown _  -> upd (Br.moveSel 1 br)
         KPageUp _       -> upd (Br.moveSel (negate th) br)
         KPageDown _     -> upd (Br.moveSel th br)
         KHome _         -> upd (Br.setSel 0 br)
         KEnd _          -> upd (Br.setSel (Br.rowCount br - 1) br)
         KArrow DRight _ -> browserExpand br ed
         KArrow DLeft _  -> browserCollapse br ed
         KEnter          -> browserActivate br ed
         KBackspace      -> rerootBrowser (Br.parentDir br) ed
         KChar '.'       -> upd (Br.toggleHidden br)
         KChar c         -> upd (Br.typeAhead c br)
         KMouse me       -> browserMouse me br ed
         _               -> noEff ed

-- Expand the selected directory (loading it if necessary), enter a "..", or
-- do nothing for a file.
browserExpand :: Browser -> Editor -> (Editor, [Effect])
browserExpand br ed = case Br.selectedNode br of
  Just n
    | fnParent n -> rerootBrowser (fnPath n) ed
    -- Always re-list on expand (cached children show instantly; the fresh
    -- listing merges in) so a directory that changed behind our back is
    -- correct the moment it is opened.
    | fnIsDir n ->
        ( ed { edBrowser = Just (Br.scrollInto (browserTreeHeight ed) (Br.expandSelected br)) }
        , [EffListDir (fnPath n)] )
    | otherwise -> noEff ed
  Nothing -> noEff ed

-- Collapse an expanded directory, otherwise move to the parent row (or re-root
-- upward when already at the top level).
browserCollapse :: Browser -> Editor -> (Editor, [Effect])
browserCollapse br ed = case Br.selectedNode br of
  Just n
    | fnIsDir n && fnExpanded n ->
        noEff ed { edBrowser = Just (Br.scrollInto (browserTreeHeight ed) (Br.collapseSelected br)) }
    | otherwise ->
        let parentPath = takeDirectory (fnPath n)
            rows = Br.visibleRows br
        in case [ i | (i, (_, m)) <- zip [0 ..] rows, fnPath m == parentPath ] of
             (i : _) -> noEff ed { edBrowser = Just (Br.scrollInto (browserTreeHeight ed) (Br.setSel i br)) }
             []      -> rerootBrowser (Br.parentDir br) ed
  Nothing -> rerootBrowser (Br.parentDir br) ed

-- Enter: open a file, toggle a directory, or follow "..". In pick-folder mode,
-- Enter on a directory opens it as the workspace folder instead of expanding.
browserActivate :: Browser -> Editor -> (Editor, [Effect])
browserActivate br ed = case Br.selectedNode br of
  Just n
    | fnParent n -> rerootBrowser (fnPath n) ed
    | edBrowserPick ed && fnIsDir n -> (closeBrowser ed, [EffExplorerOpen (fnPath n)])
    | fnIsDir n && fnExpanded n ->
        noEff ed { edBrowser = Just (Br.scrollInto (browserTreeHeight ed) (Br.collapseSelected br)) }
    | fnIsDir n ->   -- expand: always re-list so the listing is never stale
        ( ed { edBrowser = Just (Br.scrollInto (browserTreeHeight ed) (Br.expandSelected br)) }
        , [EffListDir (fnPath n)] )
    | otherwise -> (pushNavIfFar (Just (fnPath n)) origin (closeBrowser ed), [EffOpen (fnPath n)])
  Nothing -> noEff ed

rerootBrowser :: FilePath -> Editor -> (Editor, [Effect])
rerootBrowser dir ed = (ed, [EffBrowse (Just dir)])

browserMouse :: MouseEvent -> Browser -> Editor -> (Editor, [Effect])
browserMouse me br ed
  | meButton me == MBWheelUp   = noEff ed { edBrowser = Just (Br.scrollInto th (Br.moveSel (-3) br)) }
  | meButton me == MBWheelDown = noEff ed { edBrowser = Just (Br.scrollInto th (Br.moveSel 3 br)) }
  | mePressed me && meButton me == MBLeft =
      let (y, _, _, _) = browserBox ed
          treeTop = y + 2
          row = brTop br + (meRow me - treeTop)
      in if meRow me >= treeTop && meRow me < treeTop + th && row < Br.rowCount br
           then if row == brSelected br
                  then browserActivate br ed
                  else noEff ed { edBrowser = Just (Br.scrollInto th (Br.setSel row br)) }
           else noEff ed
  | otherwise = noEff ed
  where th = browserTreeHeight ed

------------------------------------------------------------------------------
-- File explorer panel (the persistent workspace sidebar)

-- | Driver callback: a folder was opened (or re-rooted). Builds the panel tree,
-- focuses it, and drops the modal browser if it was up.
explorerStart :: FilePath -> [Entry] -> Editor -> Editor
explorerStart dir entries ed =
  let ed1 = ed { edExplorer = Just (Br.mkBrowserNoParent dir entries)
               , edExpCollapsed = False, edFocus = FExplorer
               , edBrowser = Nothing, edBrowserPick = False
               , edStatus = T.pack ("Folder: " ++ takeFileName dir) }
      th  = explorerTreeHeight ed1
  in relayout ed1 { edExplorer = fmap (Br.scrollInto th) (edExplorer ed1) }

-- | Driver callback: a directory in the panel was listed (lazy expand).
explorerLoaded :: FilePath -> [Entry] -> Editor -> Editor
explorerLoaded path entries ed = case edExplorer ed of
  Nothing -> ed
  Just br -> ed { edExplorer = Just (Br.scrollInto (explorerTreeHeight ed)
                                       (mergeKeepSel path entries br)) }

-- | Ctrl+B / View ▸ Explorer: open a folder if none is open, otherwise toggle
-- the panel between expanded-and-focused, expanded-and-blurred, and collapsed.
toggleExplorer :: Editor -> (Editor, [Effect])
toggleExplorer ed
  | Nothing <- edExplorer ed = openBrowserWith True ed
  | edExpCollapsed ed        = noEff (expandExplorer ed)
  | edFocus ed == FExplorer  = noEff (relayout ed { edFocus = contentFocus ed })
  | otherwise                = noEff ed { edFocus = FExplorer }

expandExplorer :: Editor -> Editor
expandExplorer ed = relayout ed { edExpCollapsed = False, edFocus = FExplorer, edStatus = "" }

collapseExplorer :: Editor -> Editor
collapseExplorer ed = relayout ed { edExpCollapsed = True, edFocus = contentFocus ed
                                  , edSidebarDrag = False, edStatus = "Explorer collapsed" }

-- Prompt before discarding the open folder (the panel's ✕ button / menu).
closeFolderFlow :: Editor -> (Editor, [Effect])
closeFolderFlow ed
  | isJust (edExplorer ed) =
      noEff (openDialog (mkConfirm DKConfirmCloseFolder "Close Folder"
        (T.pack ("Stop using " ++ explorerRootName ed ++ " as the open folder?"))
        ["Close Folder", "Cancel"]) ed)
  | otherwise = noEff ed { edStatus = "No folder is open" }

closeFolder :: Editor -> Editor
closeFolder ed = relayout ed { edExplorer = Nothing, edExpCollapsed = False
                             , edSidebarDrag = False, edFocus = contentFocus ed
                             , edStatus = "Folder closed" }

handleExplorerKey :: Key -> Editor -> (Editor, [Effect])
handleExplorerKey key ed = case edExplorer ed of
  Nothing -> noEff ed { edFocus = FEdit }
  Just br ->
    let th = explorerTreeHeight ed
        upd b = noEff ed { edExplorer = Just (Br.scrollInto th b) }
    in case key of
         KEsc            -> noEff ed { edFocus = contentFocus ed, edStatus = "" }
         KArrow DUp _    -> upd (Br.moveSel (-1) br)
         KArrow DDown _  -> upd (Br.moveSel 1 br)
         KPageUp _       -> upd (Br.moveSel (negate th) br)
         KPageDown _     -> upd (Br.moveSel th br)
         KHome _         -> upd (Br.setSel 0 br)
         KEnd _          -> upd (Br.setSel (Br.rowCount br - 1) br)
         KArrow DRight _ -> explorerExpand br ed
         KArrow DLeft _  -> explorerCollapseSel br ed
         KEnter          -> explorerActivate br ed
         -- File management: create / rename / delete the selected entry.
         KInsert _       -> noEff (explorerNewPrompt ed)
         KCtrlChar 'n'   -> noEff (explorerNewPrompt ed)
         KFn 2 _         -> noEff (explorerRenamePrompt ed)
         KDelete _       -> noEff (explorerDeletePrompt ed)
         KChar '.'       -> upd (Br.toggleHidden br)
         KChar c         -> upd (Br.typeAhead c br)
         KMouse me
           | inExplorerRegion ed me -> explorerMouse me ed
           | menuBarPress ed me     -> noEff (mouseMenuBar (meCol me) (ed { edFocus = contentFocus ed }))
           | otherwise              -> dispatchKey key (ed { edFocus = contentFocus ed })
         -- Ctrl+B from within the panel: expand a collapsed strip (focus
         -- stays here), else blur back to the editor (runAction's leaveMenu
         -- would otherwise reset focus before toggleExplorer sees it).
         KCtrlChar 'b'
           | edExpCollapsed ed -> noEff (expandExplorer ed)
           | otherwise -> noEff (relayout ed { edFocus = contentFocus ed, edStatus = "" })
         -- Global shortcuts keep working from the panel; route them through the
         -- active document's own handler so CSV/image keys (e.g. undo) are right.
         KCtrlChar _      -> delegateActive key ed
         KCtrlShiftChar _ -> delegateActive key ed
         KAltChar _       -> delegateActive key ed
         KFn _ _          -> delegateActive key ed
         _                -> noEff ed

-- Hand a key to whichever handler the active document uses (text / CSV / image).
delegateActive :: Key -> Editor -> (Editor, [Effect])
delegateActive key ed = case edImage ed of
  Just idoc -> handleImageKey key idoc ed
  Nothing   -> case edCsv ed of
                 Just v  -> handleCsvKey key v ed
                 Nothing -> handleEditKey key ed

-- Right arrow / expand: show the (possibly cached) children immediately and
-- always request a fresh listing — the merge on arrival keeps loaded subtrees,
-- so a directory that changed on disk is correct the moment it is opened.
explorerExpand :: Browser -> Editor -> (Editor, [Effect])
explorerExpand br ed = case Br.selectedNode br of
  Just n
    | fnIsDir n ->
        ( ed { edExplorer = Just (Br.scrollInto (explorerTreeHeight ed) (Br.expandSelected br)) }
        , [EffExplorerList (fnPath n)] )
    | otherwise -> noEff ed
  Nothing -> noEff ed

-- Left arrow: collapse an expanded directory, else jump to the parent row.
explorerCollapseSel :: Browser -> Editor -> (Editor, [Effect])
explorerCollapseSel br ed = case Br.selectedNode br of
  Just n
    | fnIsDir n && fnExpanded n ->
        noEff ed { edExplorer = Just (Br.scrollInto (explorerTreeHeight ed) (Br.collapseSelected br)) }
    | otherwise ->
        let parentPath = takeDirectory (fnPath n)
            rows = Br.visibleRows br
        in case [ i | (i, (_, m)) <- zip [0 ..] rows, fnPath m == parentPath ] of
             (i : _) -> noEff ed { edExplorer = Just (Br.scrollInto (explorerTreeHeight ed) (Br.setSel i br)) }
             []      -> noEff ed
  Nothing -> noEff ed

-- Enter / activate: toggle a directory, or open a file. Focus follows the
-- loaded document (text/CSV hands focus to the editor via 'setLoaded'); an
-- image keeps the focus here in the panel ('imageLoaded'), so we don't force
-- FEdit at activation time — the load callback decides once the type is known.
explorerActivate :: Browser -> Editor -> (Editor, [Effect])
explorerActivate br ed = case Br.selectedNode br of
  Just n
    | fnIsDir n && fnExpanded n ->
        noEff ed { edExplorer = Just (Br.scrollInto (explorerTreeHeight ed) (Br.collapseSelected br)) }
    | fnIsDir n ->   -- expand: always re-list so the listing is never stale
        ( ed { edExplorer = Just (Br.scrollInto (explorerTreeHeight ed) (Br.expandSelected br)) }
        , [EffExplorerList (fnPath n)] )
    | otherwise -> ( pushNavIfFar (Just (fnPath n)) origin ed
                   , [EffOpen (fnPath n)] )
  Nothing -> noEff ed

------------------------------------------------------------------------------
-- Explorer file management (New / Rename / Delete)

-- The directory a new entry belongs in: the selected directory, the selected
-- file's parent, or the workspace root.
explorerTargetDir :: Editor -> Maybe FilePath
explorerTargetDir ed = do
  br <- edExplorer ed
  pure $ case Br.selectedNode br of
    Just n | fnIsDir n -> fnPath n
           | otherwise -> takeDirectory (fnPath n)
    Nothing -> fnPath (brRoot br)

explorerSelectedNode :: Editor -> Maybe FileNode
explorerSelectedNode ed = edExplorer ed >>= Br.selectedNode

-- | Insert / Ctrl+N in the explorer: prompt for a new file (or folder, with a
-- trailing @/@) inside the selected directory.
explorerNewPrompt :: Editor -> Editor
explorerNewPrompt ed = case explorerTargetDir ed of
  Nothing  -> ed
  Just dir -> openDialog (mkNewPath (T.pack dir)) ed

-- | F2 in the explorer: rename the selected entry.
explorerRenamePrompt :: Editor -> Editor
explorerRenamePrompt ed = case explorerSelectedNode ed of
  Nothing -> ed { edStatus = "Nothing selected to rename" }
  Just n  -> openDialog (mkRename (T.pack (takeFileName (fnPath n)))) ed

-- | Delete in the explorer: confirm, then remove the selected entry.
explorerDeletePrompt :: Editor -> Editor
explorerDeletePrompt ed = case explorerSelectedNode ed of
  Nothing -> ed { edStatus = "Nothing selected to delete" }
  Just n  ->
    let what = takeFileName (fnPath n)
        msg | fnIsDir n = "Delete the folder " ++ what ++ " and everything in it?"
            | otherwise = "Delete " ++ what ++ "?"
    in openDialog (setFocus 1 (mkConfirm DKConfirmDelete "Delete"
         (T.pack (msg ++ "\nThis cannot be undone.")) ["Delete", "Cancel"])) ed

-- Cancel/close for the file-management dialogs returns focus to the panel.
backToExplorer :: Editor -> Editor
backToExplorer ed
  | isJust (edExplorer ed) = ed { edFocus = FExplorer }
  | otherwise              = ed

-- | Driver callback after a create/rename/delete: report it and put focus
-- back on the explorer.
fileOpDone :: String -> Editor -> Editor
fileOpDone msg = backToExplorer . setStatus (T.pack msg)

-- | Re-anchor the explorer selection onto a (created/renamed) path.
selectInExplorer :: FilePath -> Editor -> Editor
selectInExplorer path ed = case edExplorer ed of
  Nothing -> ed
  Just br -> ed { edExplorer = Just (Br.scrollInto (explorerTreeHeight ed)
                                       (Br.selectPath path br)) }

-- | After a rename on disk, rewrite the paths of open documents (and the
-- recents) that pointed at the old name — including everything under a
-- renamed directory.
renamePaths :: FilePath -> FilePath -> Editor -> Editor
renamePaths old new ed = ed
  { edPath = fmap rew (edPath ed)
  , edBefore = map rewDoc (edBefore ed)
  , edAfter = map rewDoc (edAfter ed)
  , edRecent = [ e { rePath = rew (rePath e) } | e <- edRecent ed ]
  , edNavBack = [ s { nsPath = fmap rew (nsPath s) } | s <- edNavBack ed ]
  , edNavFwd  = [ s { nsPath = fmap rew (nsPath s) } | s <- edNavFwd ed ]
  }
  where
    rew p | p == old = new
          | (old ++ "/") `isPrefixOf` p = new ++ drop (length old) p
          | otherwise = p
    rewDoc d = d { docPath = fmap rew (docPath d) }

-- Is a mouse event inside the panel's on-screen region (rows of the text area,
-- columns of the sidebar)?
inExplorerRegion :: Editor -> MouseEvent -> Bool
inExplorerRegion ed me =
  isJust (edExplorer ed) &&
  let lo = computeLayout ed
  in meRow me >= loTextTop lo && meRow me < loTextTop lo + loTextHeight lo
     && meCol me >= 0 && meCol me < loContentLeft lo

-- A mouse event known to be over the panel: scroll, drag the divider, hit a
-- header button, focus, expand a collapsed strip, or select/open a tree row.
explorerMouse :: MouseEvent -> Editor -> (Editor, [Effect])
explorerMouse me ed = case edExplorer ed of
  Nothing -> noEff ed
  Just br
    | meButton me == MBWheelUp   -> noEff ed { edExplorer = Just (Br.scrollInto th (Br.moveSel (-3) br)) }
    | meButton me == MBWheelDown -> noEff ed { edExplorer = Just (Br.scrollInto th (Br.moveSel 3 br)) }
    | not (mePressed me) || meButton me /= MBLeft -> noEff ed
    | edExpCollapsed ed          -> noEff (expandExplorer ed)
    | meCol me == cl - 1         -> noEff ed { edSidebarDrag = True, edFocus = FExplorer }
    | meRow me == ptop ->
        if meCol me == explorerCloseCol lo then closeFolderFlow ed
        else if meCol me == explorerCollapseCol lo then noEff (collapseExplorer ed)
        else noEff ed { edFocus = FExplorer }
    | otherwise ->
        let idx = brTop br + (meRow me - (ptop + 1))
        in if meRow me >= ptop + 1 && idx >= 0 && idx < Br.rowCount br
             then let br' = Br.setSel idx br
                  in explorerActivate br' (ed { edExplorer = Just br', edFocus = FExplorer })
             else noEff ed { edFocus = FExplorer }
  where
    lo   = computeLayout ed
    cl   = loContentLeft lo
    ptop = loTextTop lo
    th   = explorerTreeHeight ed

-- Update the panel width / collapse state from a divider drag.
sidebarDragMove :: MouseEvent -> Editor -> Editor
sidebarDragMove me ed
  | not (mePressed me) = ed { edSidebarDrag = False }       -- release ends the drag
  | meCol me <= 2      = collapseExplorer ed                -- dragged to the far left
  | otherwise = relayout ed { edExpWidth = clampExplorerWidth (edSize ed) (meCol me + 1)
                            , edExpCollapsed = False }

handleSearchKey :: Key -> Editor -> (Editor, [Effect])
handleSearchKey key ed = case edSearch ed of
  Nothing -> noEff ed { edFocus = FEdit }
  Just ss ->
    let onField = isJust (S.focusedField ss)
        onButton = S.focusedReplaceAll ss          -- the Replace All button is focused
        h = searchResultsHeight ss ed
        setSS f = noEff ed { edSearch = Just (S.scrollInto h (f ss)) }
        editField g = case S.focusedField ss of
          Just SFFind    -> setSS (\s -> s { ssFind    = g (ssFind s) })
          Just SFReplace -> setSS (\s -> s { ssReplace = g (ssReplace s) })
          Just SFInclude -> setSS (\s -> s { ssInclude = g (ssInclude s) })
          Just SFExclude -> setSS (\s -> s { ssExclude = g (ssExclude s) })
          Nothing        -> noEff ed
    in case key of
      KEsc            -> noEff (closeSearchView ed)
      KFn 4 _            -> openSearchPanel False ed
      KFn 6 _            -> openSearchPanel True ed   -- reveal the Replace row
      KCtrlShiftChar 'f' -> openSearchPanel False ed
      KCtrlShiftChar 'h' -> openSearchPanel True ed
      KCtrlChar 's'   -> runAction MASave ed
      KCtrlChar 'q'   -> runAction MAExit ed
      KCtrlChar 'p'   -> runAction MAQuickOpen ed
      KCtrlChar 'b'   -> toggleExplorer ed
      -- Open the menu bar over the search view (it stays drawn behind).
      KFn 10 _        -> noEff (enterMenu ed)
      KFn 1 _         -> runAction MAHelp ed

      KTab            -> setSS (searchNextField 1)
      KBackTab        -> setSS (searchNextField (-1))
      KArrow DUp _    -> setSS (S.moveCursor (-1))
      KArrow DDown _  -> setSS (S.moveCursor 1)
      KPageUp _       -> setSS (S.moveCursor (negate h))
      KPageDown _     -> setSS (S.moveCursor h)

      KArrow DLeft _
        | onField     -> editField S.fieldLeft
        | otherwise   -> setSS searchCollapseOrParent
      KArrow DRight _
        | onField     -> editField S.fieldRight
        | otherwise   -> setSS searchExpandOrInto

      KHome _ | onField   -> editField S.fieldHome
              | otherwise -> setSS (S.setCursorResultRow 0)
      KEnd _  | onField   -> editField S.fieldEnd
              | otherwise -> setSS (S.setCursorResultRow (length (S.resultRows ss) - 1))

      KBackspace | onField -> editField S.fieldBackspace
      KCtrlChar 'h' | onField -> editField S.fieldDeleteWordLeft
      KDelete _ | onField    -> editField S.fieldDelete
                | otherwise  -> setSS searchDismissSelected

      KChar ' ' | onButton    -> runReplaceAll ed         -- Space activates the focused button
                | not onField -> searchActivate ed ss     -- Space toggles/opens like Enter
      KChar c   | onField     -> editField (S.fieldInsert c)
      KPaste s  | onField     -> editField (\f -> foldl (flip S.fieldInsert) f (filter (/= '\n') (T.unpack s)))

      KAltChar 'c' -> runSearch ed { edSearch = Just (S.clampCursor ss { ssCase = not (ssCase ss) }) }
      KAltChar 'w' -> runSearch ed { edSearch = Just (S.clampCursor ss { ssWord = not (ssWord ss) }) }
      KAltChar 'x' -> runSearch ed { edSearch = Just (S.clampCursor ss { ssRegex = not (ssRegex ss) }) }
      KAltChar 'r' | ssShowReplace ss -> runReplaceAll ed
      KAltChar 'h' -> noEff ed { edSearch = Just (S.clampCursor ss { ssShowReplace = not (ssShowReplace ss) }) }
      -- Any other Alt+letter opens its menu (over the search view, which stays up).
      KAltChar c   -> case menuAccelFor c of
                        Just i  -> noEff ed { edFocus = FMenu
                                            , edMenu = MenuState i True (firstSelectable (entriesFor ed i)) }
                        Nothing -> noEff ed

      -- Ctrl/Shift+Enter: replace all (from a field/button) or just this file
      -- (from a result row) — so you can apply the replacement file-by-file.
      KModEnter
        | not (ssShowReplace ss) -> runSearch ed
        | onField || onButton    -> runReplaceAll ed
        | otherwise              -> runReplaceFile ed
      KEnter
        | onButton  -> runReplaceAll ed       -- Enter on the focused Replace All button
        | onField   -> runSearch ed
        | otherwise -> searchActivate ed ss

      KMouse me    -> handleSearchMouse me ed
      _            -> noEff ed

-- Move focus to the next/previous header control (Tab): the input fields and,
-- when shown, the Replace All button. From a result row, Tab returns to them.
-- The header items are the prefix of 'focusItems', so their positions coincide
-- with the cursor index.
searchNextField :: Int -> SearchState -> SearchState
searchNextField dir ss =
  let n   = length (S.headerItems ss)
      cur = if ssCursor ss < n then ssCursor ss else 0
      nxt = ((cur + dir) `mod` n + n) `mod` n
  in ss { ssCursor = nxt }

-- Left on a result row: collapse an expanded file, or hop to the file header.
searchCollapseOrParent :: SearchState -> SearchState
searchCollapseOrParent ss = case S.selectedRow ss of
  Just (SRFile fi) | maybe False (not . frCollapsed) (Seq.lookup fi (ssResults ss)) -> S.toggleFileCollapsed fi ss
  Just (SRMatch fi _) -> fileHeaderRow fi ss
  _ -> ss

-- Right on a result row: expand a collapsed file, or step into its first match.
searchExpandOrInto :: SearchState -> SearchState
searchExpandOrInto ss = case S.selectedRow ss of
  Just (SRFile fi) -> case Seq.lookup fi (ssResults ss) of
    Just fr | frCollapsed fr           -> S.toggleFileCollapsed fi ss
            | not (null (frMatches fr)) -> moveToRow (SRMatch fi 0) ss
    _ -> ss
  _ -> ss

fileHeaderRow :: Int -> SearchState -> SearchState
fileHeaderRow fi = moveToRow (SRFile fi)

moveToRow :: SRow -> SearchState -> SearchState
moveToRow r ss = case findIndex (== r) (S.resultRows ss) of
  Just k  -> S.setCursorResultRow k ss
  Nothing -> ss

-- Enter/Space on a result row: toggle a file, or open a match.
searchActivate :: Editor -> SearchState -> (Editor, [Effect])
searchActivate ed ss = case S.selectedRow ss of
  Just (SRFile fi) -> noEff ed { edSearch = Just (S.clampCursor (S.toggleFileCollapsed fi ss)) }
  Just (SRMatch fi mi) ->
    case Seq.lookup fi (ssResults ss) of
      Just fr -> case drop mi (frMatches fr) of
        (m : _) -> let (c, len) = case mCols m of ((c0, l0) : _) -> (c0, l0); [] -> (0, 0)
                   in openMatch (frPath fr) (mLine m) c len ed
        _ -> noEff ed
      _ -> noEff ed
  Nothing -> noEff ed

-- Delete on a result row: drop that file (or match line) from the results view.
searchDismissSelected :: SearchState -> SearchState
searchDismissSelected ss = case S.selectedRow ss of
  Just (SRFile fi) ->
    let fr = Seq.lookup fi (ssResults ss)
    in S.clampCursor ss { ssResults = Seq.deleteAt fi (ssResults ss)
                        , ssTotal = ssTotal ss - maybe 0 S.fileMatchCount fr }
  Just (SRMatch fi mi) ->
    case Seq.lookup fi (ssResults ss) of
      Just fr | mi < length (frMatches fr) ->
        let lost = length (mCols (frMatches fr !! mi))
            ms'  = dropIndex mi (frMatches fr)
        in S.clampCursor ss
             { ssResults = if null ms' then Seq.deleteAt fi (ssResults ss)
                                       else Seq.adjust (\f -> f { frMatches = ms' }) fi (ssResults ss)
             , ssTotal = ssTotal ss - lost }
      _ -> ss
  Nothing -> ss

dropIndex :: Int -> [a] -> [a]
dropIndex i xs = [ x | (j, x) <- zip [0 ..] xs, j /= i ]

------------------------------------------------------------------------------
-- Search-view mouse handling

handleSearchMouse :: MouseEvent -> Editor -> (Editor, [Effect])
handleSearchMouse me ed = case edSearch ed of
  Nothing -> noEff ed
  Just ss
    -- Shift+wheel / a horizontal wheel pans the (clipped) result snippets.
    | meButton me == MBWheelUp, hasShift (meMods me)
        -> noEff ed { edSearch = Just ss { ssLeft = max 0 (ssLeft ss - 8) } }
    | meButton me == MBWheelDown, hasShift (meMods me)
        -> noEff ed { edSearch = Just ss { ssLeft = min maxPan (ssLeft ss + 8) } }
    | meButton me == MBWheelLeft  -> noEff ed { edSearch = Just ss { ssLeft = max 0 (ssLeft ss - 8) } }
    | meButton me == MBWheelRight -> noEff ed { edSearch = Just ss { ssLeft = min maxPan (ssLeft ss + 8) } }
    | meButton me == MBWheelUp   -> noEff ed { edSearch = Just ss { ssTop = max 0 (ssTop ss - 3) } }
    | meButton me == MBWheelDown ->
        let maxTop = max 0 (length (S.resultRows ss) - 1)
        in noEff ed { edSearch = Just ss { ssTop = min maxTop (ssTop ss + 3) } }
    | not (mePressed me) || meButton me /= MBLeft || meDrag me -> noEff ed
    | otherwise ->
        let (top, left, h, w) = searchRegion (computeLayout ed)
            relRow = meRow me - top
            hls = S.headerLines ss
            hh = length hls
        in if relRow < 0 || relRow >= h then noEff ed
           else if relRow < hh
             then headerClick (hls !! relRow) (meCol me) left w ss ed
             else let k = ssTop ss + (relRow - hh)
                  in if k >= 0 && k < length (S.resultRows ss)
                       then searchActivate ed (S.setCursorResultRow k ss)
                       -- A click on the empty area below the results leaves
                       -- the search view and returns to the document (like
                       -- Esc; the results are kept for next time).
                       else noEff (closeSearchView ed)
  where
    maxPan = 2000   -- snippets are clipped anyway; just bound the pan

-- A click on one of the fixed header lines.
headerClick :: HLine -> Int -> Int -> Int -> SearchState -> Editor -> (Editor, [Effect])
headerClick hl clickCol left w ss ed = case hl of
  HLFind -> case findCtlAt clickCol left w of
    Just CtlCase       -> runSearch ed { edSearch = Just (ss { ssCase = not (ssCase ss) }) }
    Just CtlWord       -> runSearch ed { edSearch = Just (ss { ssWord = not (ssWord ss) }) }
    Just CtlRegex      -> runSearch ed { edSearch = Just (ss { ssRegex = not (ssRegex ss) }) }
    Just CtlReplToggle -> noEff ed { edSearch = Just (S.clampCursor ss { ssShowReplace = not (ssShowReplace ss) }) }
    _                  -> focusFieldAt SFFind clickCol left ss ed
  HLReplace  -> if clickReplaceAll clickCol left w
                  then runReplaceAll ed
                  else focusFieldAt SFReplace clickCol left ss ed
  HLInclude  -> focusFieldAt SFInclude clickCol left ss ed
  HLExclude  -> focusFieldAt SFExclude clickCol left ss ed
  _          -> noEff ed

findCtlAt :: Int -> Int -> Int -> Maybe SearchCtl
findCtlAt clickCol left w =
  case [ ctl | (c, s, ctl) <- findLineCtls left w, clickCol >= c, clickCol < c + length s ] of
    (ctl : _) -> Just ctl
    []        -> Nothing

clickReplaceAll :: Int -> Int -> Int -> Bool
clickReplaceAll clickCol left w =
  let start = left + w - length replaceAllLabel
  in clickCol >= start && clickCol < start + length replaceAllLabel

focusFieldAt :: SearchField -> Int -> Int -> SearchState -> Editor -> (Editor, [Effect])
focusFieldAt f clickCol left ss ed =
  let valStart = left + searchFieldValueCol
      cur = max 0 (clickCol - valStart)
      place fld = let n = T.length (sfText fld) in fld { sfCur = min n cur }
      ss1 = S.setCursorField f ss
      ss2 = case f of
              SFFind    -> ss1 { ssFind    = place (ssFind ss1) }
              SFReplace -> ss1 { ssReplace = place (ssReplace ss1) }
              SFInclude -> ss1 { ssInclude = place (ssInclude ss1) }
              SFExclude -> ss1 { ssExclude = place (ssExclude ss1) }
  in noEff ed { edSearch = Just (S.clampCursor ss2) }

------------------------------------------------------------------------------
-- Go to Definition (F12 / Ctrl+Click / Find menu)

-- | Look up where the identifier at @pos@ is defined, across every supported
-- language in the workspace. Opens the picker dialog immediately — seeded from
-- the open documents' (possibly unsaved) buffers — and streams definition
-- sites found on disk into it as the background scan reports them.
goToDefinition :: Pos -> Editor -> (Editor, [Effect])
goToDefinition pos ed
  | isJust (edImage ed) || isJust (edCsv ed) =
      noEff ed { edStatus = "Go to Definition works in text files" }
  | otherwise =
      let (a, b) = wordRangeAt pos (edBuffer ed)
          name = textInRange a b (edBuffer ed)
      in if not (isIdentName name)
           then noEff ed { edStatus = "No identifier here to look up" }
           else
             let gen  = edDefGen ed + 1
                 root = guessRoot ed
                 dp   = D.dpAddItems (defSeedItems name ed) (D.newDefPick gen name root)
             in ( ed { edDefGen = gen, edDefPick = Just dp
                     , edFocus = FDefPick, edMenu = closedMenu, edStatus = "" }
                , [EffFindDefs (DefReq gen name root (openPathsList ed))] )

-- A lookup target must look like an identifier (not a number or punctuation).
isIdentName :: Text -> Bool
isIdentName t = case T.uncons t of
  Just (c, _) -> isAlpha c || c == '_'
  Nothing     -> False

-- Definition sites in the open documents' buffers, so unsaved edits are seen
-- (the background scan skips these paths — they are handed to it as dfSkip).
defSeedItems :: Text -> Editor -> [DefItem]
defSeedItems name ed =
  [ DefItem p ln c l (T.copy (T.take 2000 line))
  | d <- allOpenDocs ed
  , isPlainDoc d
  , Just p  <- [docPath d]
  , Just lg <- [D.langOf p]
  , (ln, line) <- zip [0 ..] (T.lines (bufferToText LF False (docBuffer d)))
  , (c, l) <- take 1 (D.defLineCols lg name line)
  ]

-- | Driver callback: one file's definition sites arrived from the scan.
defFound :: Int -> FileResult -> Editor -> Editor
defFound gen fr ed = case edDefPick ed of
  Just dp | dpGen dp == gen ->
    let items = [ DefItem (frPath fr) (mLine m) c l (mText m)
                | m <- frMatches fr, (c, l) <- take 1 (mCols m) ]
    in ed { edDefPick = Just (D.dpAddItems items dp) }
  _ -> ed

-- | Driver callback: the definition scan finished.
defDone :: Int -> Editor -> Editor
defDone gen ed = case edDefPick ed of
  Just dp | dpGen dp == gen -> ed { edDefPick = Just dp { dpRunning = False } }
  _ -> ed

-- | Geometry of the picker dialog: @(top, left, height, width)@. The list
-- grows with the streamed results up to most of the screen. Shared by the
-- renderer and mouse hit-testing.
defPickGeom :: Editor -> (Int, Int, Int, Int)
defPickGeom ed =
  let (rows, cols) = edSize ed
      n  = maybe 0 (length . dpItems) (edDefPick ed)
      w  = max 40 (min 96 (cols - 4))
      vh = max 1 (min (max 1 n) (max 3 (rows - 9)))
      h  = vh + 3   -- title border row + list + footer + bottom border
      x  = max 0 ((cols - w) `div` 2)
      y  = max 1 ((rows - h) `div` 2)
  in (y, x, h, w)

-- Rows available for the item list inside the picker box.
defPickViewH :: Editor -> Int
defPickViewH ed = let (_, _, h, _) = defPickGeom ed in max 1 (h - 3)

closeDefPick :: Editor -> Editor
closeDefPick ed = ed { edDefPick = Nothing, edFocus = FEdit }

-- Open the selected definition site (via the same pending-jump machinery as a
-- search result, so it works for open, closed and still-loading files).
defPickOpen :: DefPick -> Editor -> (Editor, [Effect])
defPickOpen dp ed = case drop (dpSel dp) (dpItems dp) of
  (it : _) -> openMatch (diPath it) (diLine it) (diCol it) (diLen it) (closeDefPick ed)
  []       -> noEff (closeDefPick ed)   -- Enter with no results dismisses

handleDefPickKey :: Key -> Editor -> (Editor, [Effect])
handleDefPickKey key ed = case edDefPick ed of
  Nothing -> noEff ed { edFocus = FEdit }
  Just dp ->
    let vh = defPickViewH ed
        put d = noEff ed { edDefPick = Just d }
    in case key of
      KEsc           -> noEff (closeDefPick ed)
      KEnter         -> defPickOpen dp ed
      KArrow DUp _   -> put (D.dpMoveSel (-1) vh dp)
      KArrow DDown _ -> put (D.dpMoveSel 1 vh dp)
      KPageUp _      -> put (D.dpMoveSel (negate vh) vh dp)
      KPageDown _    -> put (D.dpMoveSel vh vh dp)
      KHome _        -> put (D.dpSelTo 0 vh dp)
      KEnd _         -> put (D.dpSelTo (length (dpItems dp) - 1) vh dp)
      KMouse me      -> defPickMouse me dp ed
      _              -> noEff ed

-- Wheel scrolls the list, a click on a row opens that site, a click anywhere
-- off the box dismisses the picker (like the single-button dialogs).
defPickMouse :: MouseEvent -> DefPick -> Editor -> (Editor, [Effect])
defPickMouse me dp ed
  | meButton me == MBWheelUp   = scrollBy (-3)
  | meButton me == MBWheelDown = scrollBy 3
  | not (mePressed me) || meButton me /= MBLeft || meDrag me = noEff ed
  | insideList =
      let k = dpTop dp + (meRow me - listTop)
      in if k < length (dpItems dp)
           then defPickOpen dp { dpSel = k } ed
           else noEff ed
  | insideBox = noEff ed
  | otherwise = noEff (closeDefPick ed)
  where
    (y, x, h, w) = defPickGeom ed
    vh = max 1 (h - 3)
    listTop = y + 1
    insideBox  = meRow me >= y && meRow me < y + h && meCol me >= x && meCol me < x + w
    insideList = insideBox && meRow me >= listTop && meRow me < listTop + vh
    scrollBy d =
      let maxTop = max 0 (length (dpItems dp) - vh)
      in noEff ed { edDefPick = Just dp { dpTop = max 0 (min maxTop (dpTop dp + d)) } }

handleCompleteKey :: Key -> Complete -> Editor -> (Editor, [Effect])
handleCompleteKey key cp ed = case key of
  KEsc           -> noEff (closeComplete ed)
  KEnter         -> accept
  KTab           -> accept
  KArrow DUp _   -> noEff (moveSel (-1))
  KArrow DDown _ -> noEff (moveSel 1)
  KCtrlChar ' '  -> noEff (moveSel 1)          -- repeat Ctrl+Space cycles
  KChar ch | isWordCh ch -> noEff (renarrowComplete (typeChar ch ed))
  KBackspace     -> noEff (renarrowComplete (backspace ed))
  KMouse me      -> completeMouse me cp ed
  -- Anything else dismisses the popup and applies as a normal key.
  _              -> handleEditKey key (closeComplete ed)
  where
    accept = case drop (cpSel cp) (cpItems cp) of
      (w : _) -> noEff (acceptWord (cpStart cp) w ed)
      []      -> noEff (closeComplete ed)
    moveSel d =
      let n = length (cpItems cp)
          sel = (cpSel cp + d + n) `mod` max 1 n
          vh = min completeVisRows n
          top | sel < cpTop cp = sel
              | sel >= cpTop cp + vh = sel - vh + 1
              | otherwise = cpTop cp
      in ed { edComplete = Just cp { cpSel = sel, cpTop = top } }

completeMouse :: MouseEvent -> Complete -> Editor -> (Editor, [Effect])
completeMouse me cp ed
  | meButton me == MBWheelUp || meButton me == MBWheelDown =
      let d = if meButton me == MBWheelUp then -3 else 3
          n = length (cpItems cp)
          vh = min completeVisRows n
          maxTop = max 0 (n - vh)
      in noEff ed { edComplete = Just cp { cpTop = max 0 (min maxTop (cpTop cp + d)) } }
  | mePressed me && not (meDrag me) && meButton me == MBLeft =
      let (y, x, h, w) = completeGeom ed cp
          inside = meRow me >= y && meRow me < y + h && meCol me >= x && meCol me < x + w
          k = cpTop cp + (meRow me - y)
      in if inside && k < length (cpItems cp)
           then noEff (acceptWord (cpStart cp) (cpItems cp !! k) ed)
           else handleEditKey (KMouse me) (closeComplete ed)
  | otherwise = noEff ed

handleCsvKey :: Key -> CsvView -> Editor -> (Editor, [Effect])
handleCsvKey key v ed
  | Csv.isEditing v = handleCsvEdit key v ed
  | otherwise       = handleCsvNav key v ed

handleCsvNav :: Key -> CsvView -> Editor -> (Editor, [Effect])
handleCsvNav key v ed = case key of
  KCtrlChar 'q' -> runAction MAExit ed
  KCtrlChar 's' -> save ed
  KCtrlChar 'o' -> runAction MAOpen ed
  KCtrlChar 'n' -> runAction MANew ed
  KCtrlChar 'p' -> runAction MAQuickOpen ed
  KCtrlChar 'w' -> runAction MACloseFile ed
  KFn 10 _      -> noEff (enterMenu ed)
  KFn 1 _       -> runAction MAHelp ed
  KCtrlChar 'f' -> runAction MAFind ed
  KCtrlChar 'r' -> runAction MAReplace ed  -- shows a "use text mode" message in table view
  KCtrlChar 'b' -> runAction MAToggleExplorer ed
  KFn 3 m | hasShift m -> runAction MAFindPrev ed
          | otherwise  -> runAction MAFindNext ed
  KFn 4 _ -> runAction MAFindInFiles ed
  KFn 6 _ -> runAction MAReplaceInFiles ed
  KCtrlChar 'z' -> noEff (csvModUndo (Csv.undo v) ed)
  KCtrlChar 'y' -> noEff (csvModUndo (Csv.redo v) ed)
  -- Copy/cut use the rectangular selection (a mini-CSV) or just the cell.
  KCtrlChar 'c' -> (csvPut v ed, [EffCopy (Csv.copyText v)])
  KCtrlChar 'x' -> (csvMod (Csv.clearSelCells v) ed, [EffCopy (Csv.copyText v)])
  KCtrlChar 'v' -> (csvPut v ed, [EffPaste])
  -- Shift+nav grows a rectangular selection; plain/ctrl nav collapses it.
  KArrow d m | hasAlt m  -> noEff (csvStruct d (Csv.clearSel v) ed)
             | hasCtrl m -> noEff (nav m (csvJump d))
             | otherwise -> noEff (nav m (Csv.moveCursor d))
  KTab          -> noEff (csvPut (Csv.clearSel (Csv.nextCellTab False v)) ed)
  KBackTab      -> noEff (csvPut (Csv.clearSel (Csv.nextCellTab True v)) ed)
  KHome m | hasCtrl m -> noEff (nav m (Csv.moveToTop . Csv.moveToHomeRow))
          | otherwise -> noEff (nav m Csv.moveToHomeRow)
  KEnd m  | hasCtrl m -> noEff (nav m (Csv.moveToBottom . Csv.moveToEndRow))
          | otherwise -> noEff (nav m Csv.moveToEndRow)
  KPageUp m     -> noEff (nav m (Csv.pageMove (negate (csvPageSize ed))))
  KPageDown m   -> noEff (nav m (Csv.pageMove (csvPageSize ed)))
  KEnter        -> noEff (csvPut (Csv.beginEdit v) ed)
  KModEnter    -> noEff (csvPut (Csv.beginEdit v) ed)
  KFn 2 _       -> noEff (csvPut (Csv.beginEdit v) ed)
  KDelete m | hasCtrl m -> noEff (csvMod (Csv.clearSel (Csv.deleteRow v)) ed)
            | otherwise -> noEff (csvMod (Csv.clearSelCells v) ed)   -- clear selected cell(s)
  KBackspace    -> noEff (csvMod (Csv.clearSelCells v) ed)
  KAltChar '\DEL' -> noEff (csvMod (Csv.clearSel (Csv.deleteCol v)) ed)  -- Alt+Backspace: delete column
  KChar ch      -> noEff (csvMod (Csv.beginEditFresh ch v) ed)   -- typing changes the cell now
  KAltChar c    -> handleAlt c ed
  KMouse me     -> noEff (csvMouse me v ed)
  KEsc          -> noEff ((csvPut (Csv.clearSel v) ed) { edStatus = "" })
  _             -> noEff ed
  where
    -- Apply a movement, extending the selection with Shift, else collapsing it.
    nav m move = csvPut (if hasShift m then Csv.withSel move v else Csv.clearSel (move v)) ed

handleCsvEdit :: Key -> CsvView -> Editor -> (Editor, [Effect])
handleCsvEdit key v ed = case key of
  KEsc           -> noEff (csvPut (Csv.cancelEdit v) ed)
  KEnter         -> noEff (csvMod (Csv.moveCursor DDown (Csv.commitEdit v)) ed)
  KModEnter     -> noEff (csvMod (Csv.editInsert '\n' v) ed)   -- newline within the cell
  KTab           -> noEff (csvMod (Csv.nextCellTab False (Csv.commitEdit v)) ed)
  KBackTab       -> noEff (csvMod (Csv.nextCellTab True (Csv.commitEdit v)) ed)
  -- Within a multi-line cell, Up/Down move between its lines; at the top/bottom
  -- line they commit and move to the cell above/below.
  KArrow DUp _   -> case Csv.editLineUp v of
                      Just v' -> noEff (csvPut v' ed)
                      Nothing -> noEff (csvMod (Csv.moveCursor DUp (Csv.commitEdit v)) ed)
  KArrow DDown _ -> case Csv.editLineDown v of
                      Just v' -> noEff (csvPut v' ed)
                      Nothing -> noEff (csvMod (Csv.moveCursor DDown (Csv.commitEdit v)) ed)
  KArrow DLeft _ -> noEff (csvPut (Csv.editLeft v) ed)
  KArrow DRight _ -> noEff (csvPut (Csv.editRight v) ed)
  KHome _        -> noEff (csvPut (Csv.editHome v) ed)
  KEnd _         -> noEff (csvPut (Csv.editEnd v) ed)
  KBackspace     -> noEff (csvMod (Csv.editBackspace v) ed)
  KDelete _      -> noEff (csvMod (Csv.editDelete v) ed)
  KChar ch       -> noEff (csvMod (Csv.editInsert ch v) ed)
  KPaste s       -> noEff (csvMod (T.foldl' (\vv c -> if c == '\n' then vv else Csv.editInsert c vv) v s) ed)
  -- Global shortcuts still work mid-edit; the grid is already live, so a save
  -- captures the in-progress edit even without pressing Enter first.
  KFn 10 _       -> noEff (enterMenu ed)
  KCtrlChar 's'  -> save ed
  KCtrlChar 'q'  -> runAction MAExit ed
  KCtrlChar 'w'  -> runAction MACloseFile ed
  KCtrlChar 'b'  -> runAction MAToggleExplorer ed
  KAltChar c     -> handleAlt c ed        -- Alt+T toggle / Alt+letter menus (grid is live)
  KMouse me      -> noEff (csvMouse me v ed)
  _              -> noEff ed

-- Map a mouse click to a cell.
csvMouse :: MouseEvent -> CsvView -> Editor -> Editor
csvMouse me v ed
  -- Shift+wheel / a horizontal wheel steps the cell cursor across columns.
  | meButton me == MBWheelUp, hasShift (meMods me)   = csvPut (Csv.clearSel (Csv.moveCursor DLeft v)) ed
  | meButton me == MBWheelDown, hasShift (meMods me) = csvPut (Csv.clearSel (Csv.moveCursor DRight v)) ed
  | meButton me == MBWheelLeft  = csvPut (Csv.clearSel (Csv.moveCursor DLeft v)) ed
  | meButton me == MBWheelRight = csvPut (Csv.clearSel (Csv.moveCursor DRight v)) ed
  | meButton me == MBWheelUp   = csvPut (Csv.pageMove (-3) v) ed
  | meButton me == MBWheelDown = csvPut (Csv.pageMove 3 v) ed
  | mePressed me && meButton me == MBLeft =
      let lo = computeLayout ed
          cl = loContentLeft lo
          gut = csvGutterWidthFor v
          top = loTextTop lo + 1
          off = meRow me - top
          frozen = edFreezeHeader ed && Csv.nRows v > 0
          h0 = if frozen then Csv.rowHeight v 0 else 0
          row | frozen && off < h0 = 0                              -- clicked the pinned row
              | otherwise          = Csv.rowAtLineOffset v (off - h0)
          col   = csvColAtX v gut (meCol me - cl)
          valid = meRow me >= top && meRow me < loTextTop lo + loTextHeight lo
                    && meCol me >= cl + gut && row < Csv.nRows v
      in if not valid then ed
         else if meDrag me
           then csvPut (Csv.withSel (Csv.setCursor row col) v) ed     -- drag grows the selection
           else let v' = Csv.clearSel (Csv.setCursor row col (Csv.commitEdit v))
                in csvPut (if meClicks me >= 2 then Csv.beginEdit v' else v') ed  -- double-click edits
  | otherwise = ed

-- Which column index a screen x falls in, given the gutter width.
csvColAtX :: CsvView -> Int -> Int -> Int
csvColAtX v gut x =
  let ws = Csv.columnWidths v
      go c acc
        | c >= length ws = max 0 (length ws - 1)
        | x < gut + acc + (ws !! c) + 1 = c
        | otherwise = go (c + 1) (acc + (ws !! c) + 1)
  in go (csvLeft v) 0

------------------------------------------------------------------------------
-- Quick open key handling (kept in the hub: picking a command runs runAction)

------------------------------------------------------------------------------
-- Quick open (Ctrl+P / File ▸ Go to File…)

-- | Open (or toggle away) the fuzzy go-to-file picker over the workspace.
openQuickOpen :: Editor -> (Editor, [Effect])
openQuickOpen ed
  | isJust (edQuickOpen ed), edFocus ed == FQuickOpen = noEff (closeQuickOpen ed)
  | otherwise =
      let gen = edQuickGen ed + 1
          root = guessRoot ed
      in ( (leaveMenu ed) { edQuickGen = gen
                          , edQuickOpen = Just (Q.newQuickOpen gen root [] (paletteCommands ed))
                          , edFocus = FQuickOpen, edMenu = closedMenu, edStatus = "" }
         , [EffQuickOpen gen root] )

-- | Ctrl+Shift+P / Help ▸ Command Palette: quick open pre-seeded with the
-- @>@ prefix, so it starts in command mode.
openPalette :: Editor -> (Editor, [Effect])
openPalette ed =
  let (ed1, effs) = openQuickOpen ed
  in ( ed1 { edQuickOpen = fmap (Q.qoEditField (S.fieldInsert '>')) (edQuickOpen ed1) }
     , effs )

-- | Every command reachable from the menus, as searchable palette rows:
-- "Menu: Item (accel)". Built from 'entriesFor', so context pruning and the
-- dynamic labels (line endings, theme, recents…) carry over.
paletteCommands :: Editor -> [(Text, MenuAction)]
paletteCommands ed =
  [ (row, act)
  | (mi, m) <- zip [0 ..] menuBar
  , MEItem lbl accel act <- entriesFor ed mi
  , act `notElem` [MANop, MAQuickOpen, MAPalette]   -- opening the palette from itself is noise
  , let title = menuTitleDisp m
        disp  = fst (parseMnemonic lbl)
        row   = title <> ": " <> disp
                  <> (if T.null accel then "" else "  (" <> accel <> ")")
  ]

-- Open the selected match (through EffOpen, so already-open files switch and
-- the recents cursor restore applies) — or, in command mode, run the picked
-- menu action.
quickOpenPick :: QuickOpen -> Editor -> (Editor, [Effect])
quickOpenPick qo ed
  | Q.qoCommandMode qo = case Q.qoPickedCommand qo of
      Just act -> runAction act (closeQuickOpen ed)
      Nothing  -> noEff (closeQuickOpen ed)
quickOpenPick qo ed = case drop (qoSel qo) (qoMatches qo) of
  ((_, rel, _) : _) ->
    let p = qoRoot qo ++ "/" ++ T.unpack rel
    in (pushNavIfFar (Just p) origin (closeQuickOpen ed), [EffOpen p])
  [] -> noEff (closeQuickOpen ed)

handleQuickOpenKey :: Key -> Editor -> (Editor, [Effect])
handleQuickOpenKey key ed = case edQuickOpen ed of
  Nothing -> noEff ed { edFocus = contentFocus ed }
  Just qo ->
    let vh = quickOpenViewH ed
        put q = noEff ed { edQuickOpen = Just q }
        edit f = put (Q.qoEditField f qo)
    in case key of
      KEsc            -> noEff (closeQuickOpen ed)
      KCtrlChar 'p'   -> noEff (closeQuickOpen ed)          -- Ctrl+P toggles
      KEnter          -> quickOpenPick qo ed
      KArrow DUp _    -> put (Q.qoMoveSel (-1) vh qo)
      KArrow DDown _  -> put (Q.qoMoveSel 1 vh qo)
      KPageUp _       -> put (Q.qoMoveSel (negate vh) vh qo)
      KPageDown _     -> put (Q.qoMoveSel vh vh qo)
      KArrow DLeft _  -> put qo { qoField = S.fieldLeft (qoField qo) }
      KArrow DRight _ -> put qo { qoField = S.fieldRight (qoField qo) }
      KHome _         -> put qo { qoField = S.fieldHome (qoField qo) }
      KEnd _          -> put qo { qoField = S.fieldEnd (qoField qo) }
      KBackspace      -> edit S.fieldBackspace
      KCtrlChar 'h'   -> edit S.fieldDeleteWordLeft
      KDelete _       -> edit S.fieldDelete
      KChar c         -> edit (S.fieldInsert c)
      KPaste s        -> edit (\f -> foldl (flip S.fieldInsert) f (filter (/= '\n') (T.unpack s)))
      KMouse me       -> quickOpenMouse me qo ed
      _               -> noEff ed

-- Wheel scrolls, a click on a row opens it, a click on the box focuses it,
-- and a click anywhere off the box dismisses (like the definition picker).
quickOpenMouse :: MouseEvent -> QuickOpen -> Editor -> (Editor, [Effect])
quickOpenMouse me qo ed
  | meButton me == MBWheelUp   = scrollBy (-3)
  | meButton me == MBWheelDown = scrollBy 3
  | not (mePressed me) || meButton me /= MBLeft || meDrag me = noEff ed
  | insideList =
      let k = qoTop qo + (meRow me - listTop)
      in if k < length (qoMatches qo)
           then quickOpenPick qo { qoSel = k } ed
           else noEff ed
  | insideBox = noEff ed
  | otherwise = noEff (closeQuickOpen ed)
  where
    (y, x, h, w) = quickOpenGeom ed
    vh = max 1 (h - 4)
    listTop = y + 2
    insideBox  = meRow me >= y && meRow me < y + h && meCol me >= x && meCol me < x + w
    insideList = insideBox && meRow me >= listTop && meRow me < listTop + vh
    scrollBy d =
      let maxTop = max 0 (length (qoMatches qo) - vh)
      in noEff ed { edQuickOpen = Just qo { qoTop = max 0 (min maxTop (qoTop qo + d)) } }

