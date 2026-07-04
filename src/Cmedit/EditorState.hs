-- | The editor model itself: the state records, configuration,
-- effects, layout and the small pure queries over them. The lowest
-- layer of the editor logic (everything else imports it).
module Cmedit.EditorState where


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
import Cmedit.HelpCard (helpCanvasMinW, helpDialogText)
import Cmedit.Clipboard (CopyOutcome(..))
import Cmedit.Image (Image(..), ImgMode(..), renderImage, viewFit)
import Cmedit.Syntax (HlCache, CommentSyntax(..), langComment, langForPath)


------------------------------------------------------------------------------
-- State

-- | Which part of the UI currently has keyboard focus.
data Focus = FEdit | FMenu | FDialog | FBrowser | FExplorer | FSearch | FDefPick | FQuickOpen
  deriving (Eq, Show)

-- | Per-file decoration shown in the explorer panel: whether the file is the
-- active document, has unsaved edits, or has changed on disk since it was
-- loaded. A file with no open document yields 'Nothing' from 'fileMarkFor'.
data FileMark = FileMark
  { fmActive      :: !Bool
  , fmModified    :: !Bool
  , fmDiskChanged :: !Bool
  } deriving (Eq, Show)

-- | The kind of the most recent edit, used to coalesce undo steps.
-- 'EKMoveLine' coalesces too, so holding Alt+Up/Down to walk a line across the
-- file undoes in one step.
data EditKind = EKNone | EKType | EKDelete | EKMoveLine | EKOther
  deriving (Eq, Show)

-- | One remembered location in the Alt+Left/Right navigation history.
-- 'Nothing' path = an untitled buffer (only revisitable while it is active).
data NavStop = NavStop
  { nsPath :: !(Maybe FilePath)
  , nsPos  :: !Pos
  } deriving (Eq, Show)

-- | The Ctrl+Space word-completion popup: the prefix being completed (kept in
-- step as the user types), where it starts, and the candidate words.
data Complete = Complete
  { cpPrefix :: !Text
  , cpStart  :: !Pos
  , cpItems  :: ![Text]
  , cpSel    :: !Int
  , cpTop    :: !Int
  } deriving (Eq, Show)

-- | A single undo checkpoint.
data UndoState = UndoState
  { usBuffer :: !Buffer
  , usCursor :: !Pos
  , usAnchor :: !(Maybe Pos)
  } deriving (Show)

-- | A saved, inactive document. The active document lives directly in the
-- 'Editor' fields; switching files swaps these in and out so that all of the
-- editing logic can keep operating on the active fields unchanged.
data Document = Document
  { docBuffer       :: !Buffer
  , docSavedBuffer  :: !Buffer          -- ^ Buffer as last saved/loaded (for the modified flag).
  , docCursor       :: !Pos
  , docSelAnchor    :: !(Maybe Pos)
  , docDesiredCol   :: !Int
  , docTop          :: !Int
  , docLeft         :: !Int
  , docPath         :: !(Maybe FilePath)
  , docModified     :: !Bool
  , docDiskMtime    :: !(Maybe EpochTime)  -- ^ On-disk mtime when last loaded/saved.
  , docDiskChanged  :: !Bool               -- ^ File on disk is newer than 'docDiskMtime'.
  , docLineEnding   :: !LineEnding
  , docSavedEol     :: !LineEnding         -- ^ Line ending as last loaded/saved (the modified flag tracks divergence).
  , docEncoding     :: !Encoding
  , docSavedEnc     :: !Encoding           -- ^ Encoding (BOM) as last loaded/saved.
  , docFinalNewline :: !Bool
  , docReadOnly     :: !Bool
  , docUndo         :: ![UndoState]
  , docRedo         :: ![UndoState]
  , docLastEdit     :: !EditKind
  , docOverwrite    :: !Bool
  , docDiscard      :: !Bool
  , docCsv          :: !(Maybe CsvView)
  , docCsvStash      :: !(Maybe CsvView)  -- ^ Table model kept while in plain-text view (preserves CSV undo).
  , docImage         :: !(Maybe ImageDoc) -- ^ Image-view model when this doc is an image.
  , docHlCache       :: !(Maybe HlCache)  -- ^ Cached syntax-highlight lexer states (perf only; self-validating).
  } deriving (Show)

-- | A read-only image-view document: the decoded image, the current paint mode,
-- and a cache of the last rendered cell grid (keyed by size + mode) so we only
-- re-scale on resize or a mode toggle. This is a wholly separate view mode to
-- both plain-text and CSV — nothing here flows through the line buffer.
data ImageDoc = ImageDoc
  { idImage :: !Image
  , idMode  :: !ImgMode
  , idCrop  :: !(Maybe (Int, Int, Int, Int))  -- ^ Zoomed view rect in source px (x,y,w,h); Nothing = whole image.
  , idDrag  :: !(Maybe (Int, Int, Int, Int))  -- ^ In-progress drag rect in text-area cells (aRow,aCol,curRow,curCol).
    -- The cache key carries the cell pixel geometry ('cellPxKey') as well as
    -- size/mode/crop: a font-size change alters the fitted shape even when the
    -- cell grid dimensions stay the same.
  , idCache :: !(Maybe (Int, Int, ImgMode, (Int, Int, Int, Int), (Int, Int), Array (Int, Int) Cell))
  } deriving (Show)

data Editor = Editor
  { edBuffer        :: !Buffer
  , edSavedBuffer   :: !Buffer          -- ^ Buffer content as last saved/loaded; the modified flag tracks divergence from it.
  , edCursor        :: !Pos
  , edSelAnchor     :: !(Maybe Pos)
  , edDesiredCol    :: !Int
  , edTop           :: !Int
  , edLeft          :: !Int
  , edPath          :: !(Maybe FilePath)
  , edModified      :: !Bool
  , edDiskMtime     :: !(Maybe EpochTime)  -- ^ On-disk mtime when the active file was last loaded/saved.
  , edDiskChanged   :: !Bool               -- ^ Active file on disk is newer than 'edDiskMtime' (refreshed when a menu opens).
  , edLineEnding    :: !LineEnding
  , edSavedEol      :: !LineEnding         -- ^ Line ending as last loaded/saved; a switched EOL keeps the file "modified" even if the text matches.
  , edEncoding      :: !Encoding
  , edSavedEnc      :: !Encoding           -- ^ Encoding (BOM) as last loaded/saved.
  , edFinalNewline  :: !Bool
  , edReadOnly      :: !Bool
  , edUndo          :: ![UndoState]
  , edRedo          :: ![UndoState]
  , edLastEdit      :: !EditKind
  , edSize          :: !(Int, Int)         -- (rows, cols)
  , edStatus        :: !Text
  , edFocus         :: !Focus
  , edMenu          :: !MenuState
  , edDialog        :: !(Maybe Dialog)
  , edConfig        :: !Config
  , edClipboard     :: !Text               -- internal mirror / paste fallback
  , edSearchTerm    :: !Text
  , edReplaceTerm   :: !Text
  , edSearchCase    :: !Bool
  , edSearchWord    :: !Bool
  , edQuit          :: !Bool
  , edQuitting      :: !Bool   -- ^ A multi-file quit-save sequence is in progress.
  , edDiscard       :: !Bool   -- ^ Active file marked "don't save" during quit.
  , edPendingClose  :: !Bool
  , edWordWrap      :: !Bool
  , edShowLineNumbers :: !Bool
  , edShowWhitespace  :: !Bool
  , edFreezeHeader  :: !Bool   -- ^ Keep the first table row pinned while scrolling (CSV view).
  , edShowMenu      :: !Bool
  , edShowStatus    :: !Bool
  , edShowHints     :: !Bool
  , edMouseSelecting :: !Bool
  , edOverwrite     :: !Bool
  , edBefore        :: ![Document]    -- ^ Open files before the active one.
  , edAfter         :: ![Document]    -- ^ Open files after the active one.
  , edBrowser       :: !(Maybe Browser) -- ^ The Open-file tree browser, when active.
  , edBrowserPick   :: !Bool            -- ^ The modal browser is choosing a folder (not a file).
  , edExplorer      :: !(Maybe Browser) -- ^ The workspace file-explorer panel tree, when a folder is open.
  , edExpWidth      :: !Int             -- ^ Desired expanded width (cells) of the explorer panel.
  , edExpCollapsed  :: !Bool            -- ^ The panel is collapsed to a single-column strip.
  , edSidebarDrag   :: !Bool            -- ^ A panel-width drag (on the divider) is in progress.
  , edScrollDrag    :: !Bool            -- ^ A scrollbar-thumb drag is in progress (swallows mouse until release).
  , edLoading       :: !(Maybe (String, Int)) -- ^ A file is loading in the background: (display name, spinner frame).
  , edAboutTick     :: !Int             -- ^ Frame counter of the About-box animation (reset when the dialog opens).
  , edSearch        :: !(Maybe SearchState) -- ^ Workspace-wide find/replace panel state (global, not per-document).
  , edSearchMode    :: !Bool                 -- ^ The search view occupies the main content area (drawn instead of the document); stays set while a menu/dialog overlays it.
  , edPendingJump   :: !(Maybe (FilePath, Int, Int, Int)) -- ^ After opening a search result: (path, line, col, len) to move the cursor to once loaded.
  , edDefPick       :: !(Maybe DefPick) -- ^ The go-to-definition picker dialog, when open.
  , edDefGen        :: !Int             -- ^ Monotonic id of the newest definition scan (stale results are dropped).
  , edQuickOpen     :: !(Maybe QuickOpen) -- ^ The Ctrl+P go-to-file picker, when open.
  , edQuickGen      :: !Int             -- ^ Monotonic id of the newest quick-open walk.
  , edNavBack       :: ![NavStop]       -- ^ Locations to go back to (Alt+Left), most recent first.
  , edNavFwd        :: ![NavStop]       -- ^ Locations undone by Go Back (Alt+Right re-visits them).
  , edFindHist      :: ![Text]          -- ^ Previous search terms, newest first (Up in the Find field recalls them).
  , edReplHist      :: ![Text]          -- ^ Previous replacement terms.
  , edHistPos       :: !(Maybe Int)     -- ^ Index being browsed in the focused field's history, if any.
  , edHistStash     :: !Text            -- ^ The in-progress field text stashed while browsing history.
  , edComplete      :: !(Maybe Complete) -- ^ The Ctrl+Space word-completion popup, when open.
  , edCsv           :: !(Maybe CsvView) -- ^ Table view, when the active doc is in CSV mode.
  , edCsvStash      :: !(Maybe CsvView) -- ^ Table model retained while viewing a CSV doc as plain text (keeps its undo across toggles).
  , edImage         :: !(Maybe ImageDoc) -- ^ Image view, when the active doc is an image (a separate mode to text/CSV).
  , edHlCache       :: !(Maybe HlCache) -- ^ Cached syntax-highlight lexer states for the active doc (perf only; self-validating against the buffer).
  , edRecent        :: ![RecentEntry]   -- ^ Recently-opened files, most recent first (global; persisted by the driver).
  , edDetectedDark  :: !(Maybe Bool)    -- ^ OSC 11 verdict: the terminal background is dark (drives @theme = auto@; Nothing until the terminal answers).
  , edCellPx        :: !(Maybe (Int, Int)) -- ^ One character cell's (width, height) in pixels, when the terminal reported it (image aspect ratio).
  } deriving (Show)

-- | A fresh editor for the given terminal size and config.
newEditor :: (Int, Int) -> Config -> Editor
newEditor size cfg = Editor
  { edBuffer        = emptyBuffer
  , edSavedBuffer   = emptyBuffer
  , edCursor        = origin
  , edSelAnchor     = Nothing
  , edDesiredCol    = 0
  , edTop           = 0
  , edLeft          = 0
  , edPath          = Nothing
  , edModified      = False
  , edDiskMtime     = Nothing
  , edDiskChanged   = False
  , edLineEnding    = LF
  , edSavedEol      = LF
  , edEncoding      = Utf8
  , edSavedEnc      = Utf8
  , edFinalNewline  = True
  , edReadOnly      = False
  , edUndo          = []
  , edRedo          = []
  , edLastEdit      = EKNone
  , edSize          = size
  , edStatus        = ""
  , edFocus         = FEdit
  , edMenu          = closedMenu
  , edDialog        = Nothing
  , edConfig        = cfg
  , edClipboard     = ""
  , edSearchTerm    = ""
  , edReplaceTerm   = ""
  , edSearchCase    = False
  , edSearchWord    = False
  , edQuit          = False
  , edQuitting      = False
  , edDiscard       = False
  , edPendingClose  = False
  , edWordWrap      = cfgWordWrap cfg
  , edShowLineNumbers = cfgLineNumbers cfg
  , edShowWhitespace  = cfgShowWhitespace cfg
  , edFreezeHeader  = False
  , edShowMenu      = True
  , edShowStatus    = True
  , edShowHints     = True
  , edMouseSelecting = False
  , edOverwrite     = False
  , edBefore        = []
  , edAfter         = []
  , edBrowser       = Nothing
  , edBrowserPick   = False
  , edExplorer      = Nothing
  , edExpWidth      = defaultExplorerWidth
  , edExpCollapsed  = False
  , edSidebarDrag   = False
  , edScrollDrag    = False
  , edLoading       = Nothing
  , edAboutTick     = 0
  , edSearch        = Nothing
  , edSearchMode    = False
  , edPendingJump   = Nothing
  , edDefPick       = Nothing
  , edDefGen        = 0
  , edQuickOpen     = Nothing
  , edQuickGen      = 0
  , edNavBack       = []
  , edNavFwd        = []
  , edFindHist      = []
  , edReplHist      = []
  , edHistPos       = Nothing
  , edHistStash     = ""
  , edComplete      = Nothing
  , edCsv           = Nothing
  , edCsvStash      = Nothing
  , edImage         = Nothing
  , edHlCache       = Nothing
  , edRecent        = []
  , edDetectedDark  = Nothing
  , edCellPx        = Nothing
  }

-- | The theme to draw with this frame: an explicit config choice wins; with
-- @theme = auto@ the detected terminal background decides, defaulting to dark
-- until (unless) the terminal answers the driver's OSC 11 query.
resolvedTheme :: Editor -> ThemeName
resolvedTheme ed = case cfgTheme (edConfig ed) of
  ThemeAuto -> case edDetectedDark ed of
                 Just False -> ThemeLight
                 _          -> ThemeDark
  t         -> t

-- | Record the OSC 11 verdict (driver callback).
setDetectedDark :: Bool -> Editor -> Editor
setDetectedDark dark ed = ed { edDetectedDark = Just dark }

-- | Sub-pixel aspect ratio for the image view: the height of one half-cell in
-- units of the cell width. 1.0 — the classic "a cell is twice as tall as it
-- is wide" assumption — when the terminal's pixel geometry is unknown, and
-- clamped so a nonsense reply can never distort the picture badly.
cellAspect :: Editor -> Double
cellAspect ed = case edCellPx ed of
  Just (w, h) | w > 0 && h > 0 ->
    min 1.6 (max 0.7 (fromIntegral h / (2 * fromIntegral w)))
  _ -> 1.0

-- | The cell pixel geometry as it participates in the image render-cache key
-- ((0,0) when unknown).
cellPxKey :: Editor -> (Int, Int)
cellPxKey ed = fromMaybe (0, 0) (edCellPx ed)

-- | The mouse-pointer shape to suggest for a screen cell (the OSC 22 hint the
-- driver emits on hover): an I-beam over editable text, a hand over the
-- clickable chrome (menu bar, status zones, explorer rows, search results),
-- a crosshair over the image view's zoom-drag area, and the default arrow
-- elsewhere. Purely advisory — terminals without OSC 22 ignore it.
pointerShapeFor :: Editor -> Int -> Int -> String
pointerShapeFor ed row col
  | isJust (edLoading ed)   = "wait"
  | edFocus ed `elem` [FDialog, FBrowser, FDefPick, FQuickOpen, FMenu] = "default"
  | edShowMenu ed   && row == loMenuRow lo   = "pointer"
  | edShowStatus ed && row == loStatusRow lo = "pointer"
  | edShowHints ed  && row == loHintRow lo   = "default"
  | col >= loCols lo - 1                     = "default"   -- scrollbar
  | inSidebar                                = "pointer"
  | edSearchMode ed && inContent             = "pointer"   -- result rows / fields
  | isJust (edImage ed) && inContent         = "crosshair" -- drag-zoom
  | isJust (edCsv ed) && inContent           = "default"
  | inText                                   = "text"
  | otherwise                                = "default"
  where
    lo = computeLayout ed
    inRows    = row >= loTextTop lo && row < loTextTop lo + loTextHeight lo
    inSidebar = loContentLeft lo > 0 && col < loContentLeft lo - 1 && inRows
    inContent = inRows && col >= loContentLeft lo
    inText    = inRows && col >= loTextLeft lo

tabWidthOf :: Editor -> Int
tabWidthOf = cfgTabWidth . edConfig

currentLine :: Editor -> Text
currentLine ed = getLine' (posLine (edCursor ed)) (edBuffer ed)

-- | A title string for the terminal window.
windowTitle :: Editor -> String
windowTitle ed =
  let name = maybe "untitled" takeFileName (edPath ed)
      dirty = if edModified ed then "* " else ""
  in dirty ++ name ++ " - CMeDit"

------------------------------------------------------------------------------
-- Layout

data Layout = Layout
  { loRows       :: !Int
  , loCols       :: !Int
  , loMenuRow    :: !Int
  , loTextTop    :: !Int
  , loTextHeight :: !Int
  , loTextLeft   :: !Int        -- ^ Absolute column where the document content begins (sidebar + gutter).
  , loTextWidth  :: !Int
  , loGutter     :: !Int
  , loContentLeft :: !Int       -- ^ Absolute column where the document area (gutter included) begins = sidebar width.
  , loStatusRow  :: !Int
  , loHintRow    :: !Int
  } deriving (Show)

computeLayout :: Editor -> Layout
computeLayout ed =
  let (rows, cols) = edSize ed
      menuH   = if edShowMenu ed then 1 else 0
      statusH = if edShowStatus ed then 1 else 0
      hintH   = if edShowHints ed then 1 else 0
      nLines  = lineCount (edBuffer ed)
      sideW   = sidebarWidth ed
      avail   = max 1 (cols - sideW)
      gutter  = if isJust (edImage ed) then 0      -- image view uses the full width
                else if edShowLineNumbers ed
                  then max 4 (length (show nLines) + 2)
                  else 0
      textTop = menuH
      textH   = max 1 (rows - menuH - statusH - hintH)
      -- The terminal's rightmost column is reserved for the scrollbar (drawn
      -- only when the content overflows, but reserved unconditionally so the
      -- word-wrap width can't oscillate with the content height).
      textW   = max 1 (avail - gutter - 1)
  in Layout
       { loRows = rows, loCols = cols
       , loMenuRow = 0
       , loTextTop = textTop
       , loTextHeight = textH
       , loTextLeft = sideW + gutter
       , loTextWidth = textW
       , loGutter = gutter
       , loContentLeft = sideW
       , loStatusRow = menuH + textH
       , loHintRow = menuH + textH + statusH
       }

pageSize :: Editor -> Int
pageSize ed = max 1 (loTextHeight (computeLayout ed) - 1)

------------------------------------------------------------------------------
-- Scrollbar geometry (shared by the renderer and mouse hit-testing)

-- | The vertical scrollbar for the current main view, when its content
-- overflows: @(column, top row, height, total rows, window start)@. The bar
-- lives in the terminal's reserved rightmost column. Buffer lines stand in
-- for visual rows under word wrap (an approximation that keeps huge wrapped
-- files O(1) here).
scrollBarInfo :: Editor -> Maybe (Int, Int, Int, Int, Int)
scrollBarInfo ed
  | isJust (edImage ed) = Nothing
  | otherwise =
      let lo = computeLayout ed
          h = loTextHeight lo
          bar total win
            | total > h = Just (loCols lo - 1, loTextTop lo, h, total, win)
            | otherwise = Nothing
      in if searchViewActive ed
           then case edSearch ed of
                  Just ss -> bar (length (S.resultRows ss)) (ssTop ss)
                  Nothing -> Nothing
           else case edCsv ed of
                  Just v  -> bar (Csv.nRows v) (csvTop v)
                  Nothing -> bar (lineCount (edBuffer ed)) (edTop ed)

-- | Thumb placement within a bar: @(thumbTop, thumbLen)@ for a @h@-row track
-- showing a @total@-row document scrolled to @win@.
scrollThumb :: Int -> Int -> Int -> (Int, Int)
scrollThumb h total win =
  let thumbLen = max 1 (min h (h * h `div` max 1 total))
      maxTop = max 1 (total - h)
      pos = (win * (h - thumbLen) + maxTop `div` 2) `div` maxTop
  in (max 0 (min (h - thumbLen) pos), thumbLen)

-- | Map a click at track offset @rel@ back to a window start (the inverse of
-- 'scrollThumb', so clicking where the thumb would be for row N goes to N).
scrollTrackTarget :: Int -> Int -> Int -> Int
scrollTrackTarget h total rel =
  let thumbLen = max 1 (min h (h * h `div` max 1 total))
      denom = max 1 (h - thumbLen)
      maxTop = max 0 (total - h)
  in max 0 (min maxTop (rel * maxTop `div` denom))

------------------------------------------------------------------------------
-- File explorer panel geometry

-- Default expanded width of the explorer panel (cells), before clamping.
defaultExplorerWidth :: Int
defaultExplorerWidth = 30

-- Narrowest the panel may be dragged before it should collapse instead.
minExplorerWidth :: Int
minExplorerWidth = 16

-- | Current on-screen width (cells) of the explorer sidebar: 0 when no folder
-- is open, 1 when collapsed to a strip, else the clamped expanded width. The
-- rightmost column is the draggable divider.
sidebarWidth :: Editor -> Int
sidebarWidth ed = case edExplorer ed of
  Nothing -> 0
  Just _
    | edExpCollapsed ed -> min 1 (max 0 (cols - 1))
    | otherwise         -> min (max 1 (cols - 8)) (clampExplorerWidth (edSize ed) (edExpWidth ed))
  where (_, cols) = edSize ed

-- Keep an expanded width within sensible bounds for the terminal size, leaving
-- room for the text area.
clampExplorerWidth :: (Int, Int) -> Int -> Int
clampExplorerWidth (_, cols) w =
  let maxw = max minExplorerWidth (cols - 20)
  in max minExplorerWidth (min maxw w)

-- Visible tree rows in the panel (height minus the one-row header).
explorerTreeHeight :: Editor -> Int
explorerTreeHeight ed = max 1 (loTextHeight (computeLayout ed) - 1)

-- Column of the close (✕) button on the panel header, and the collapse («).
explorerCloseCol :: Layout -> Int
explorerCloseCol lo = loContentLeft lo - 2

explorerCollapseCol :: Layout -> Int
explorerCollapseCol lo = loContentLeft lo - 4

-- Display name for the open folder (its base name, or the path for the FS root).
explorerRootName :: Editor -> String
explorerRootName ed = case edExplorer ed of
  Nothing -> ""
  Just br -> let p = fnPath (brRoot br); nm = takeFileName p in if null nm then p else nm

-- | The decoration for a file path, if a document for it is open: whether it is
-- the active document, has unsaved edits, and/or changed on disk. Used by the
-- explorer panel to mark open/dirty/stale files.
fileMarkFor :: Editor -> FilePath -> Maybe FileMark
fileMarkFor ed path
  | edPath ed == Just path = Just (FileMark True (edModified ed) (edDiskChanged ed))
  | otherwise = firstJust (map fromDoc (edBefore ed ++ edAfter ed))
  where
    fromDoc d
      | docPath d == Just path = Just (FileMark False (docModified d) (docDiskChanged d))
      | otherwise              = Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr orElse Nothing where orElse (Just x) _ = Just x; orElse Nothing y = y

------------------------------------------------------------------------------
-- Background file loading (spinner shown while a big file loads off-thread)

-- | Braille spinner frames cycled by the driver's tick while loading.
spinnerFrames :: [Char]
spinnerFrames = "\x280b\x2819\x2839\x2838\x283c\x2834\x2826\x2827\x2807\x280f"

-- | Enter the "loading" state for a file (shows the spinner overlay). The name
-- is what is displayed; the frame counter starts at 0.
beginLoading :: String -> Editor -> Editor
beginLoading name ed = ed { edLoading = Just (name, 0), edStatus = "" }

-- | Advance the spinner animation one frame (driver tick).
-- | Advance the About-box wordmark animation one frame (a no-op once it has
-- settled, so the event loop's tick timer stops re-arming).
tickAbout :: Editor -> Editor
tickAbout ed
  | aboutAnimating ed = ed { edAboutTick = edAboutTick ed + 1 }
  | otherwise         = ed

-- | The About dialog is open and its animation has not finished yet.
aboutAnimating :: Editor -> Bool
aboutAnimating ed =
  maybe False ((== DKAbout) . dlgKind) (edDialog ed) && edAboutTick ed < aboutTotalFrames

tickLoading :: Editor -> Editor
tickLoading ed = case edLoading ed of
  Just (nm, f) -> ed { edLoading = Just (nm, f + 1) }
  Nothing      -> ed

-- | Leave the loading state (the load finished or failed).
endLoading :: Editor -> Editor
endLoading ed = ed { edLoading = Nothing }

-- | Hard cap on the size of a file we will open. Larger files are refused (they
-- can't be edited without hanging / exhausting memory). Binary files of any size
-- are refused separately by 'Cmedit.TextBuffer.looksBinary'.
maxOpenBytes :: Integer
maxOpenBytes = 100 * 1024 * 1024

-- | At or above this size a file shows its size in the explorer tree.
sizeLabelThreshold :: Integer
sizeLabelThreshold = 1024 * 1024

-- | Compact byte size for the explorer tree, e.g. @"512"@, @"3K"@, @"52M"@,
-- @"1.2G"@ (kept to at most 4 characters).
shortSize :: Integer -> String
shortSize n
  | n < 1024            = show n
  | n < 1024 * 1024     = show (n `div` 1024) ++ "K"
  | n < 1024*1024*1024  = mag (1024*1024) "M"
  | otherwise           = mag (1024*1024*1024) "G"
  where
    mag unit suf =
      let whole = n `div` unit
          tenth = (n * 10 `div` unit) `mod` 10
      in if whole < 10 then show whole ++ "." ++ show tenth ++ suf
                       else show whole ++ suf

-- | Human-readable byte size for messages, e.g. @"52.4 MB"@.
humanSize :: Integer -> String
humanSize n
  | n < 1024            = show n ++ " B"
  | n < 1024 * 1024     = oneDp 1024 ++ " KB"
  | n < 1024*1024*1024  = oneDp (1024*1024) ++ " MB"
  | otherwise           = oneDp (1024*1024*1024) ++ " GB"
  where oneDp unit = let whole = n `div` unit
                         tenth = (n * 10 `div` unit) `mod` 10
                     in show whole ++ "." ++ show tenth

------------------------------------------------------------------------------
-- Effects

data Effect
  = EffCopy !Text          -- ^ Place text on the system clipboard.
  | EffPaste               -- ^ Request the clipboard contents (driver replies via 'applyPaste').
  | EffSaveTo !FilePath    -- ^ Write the buffer to a path.
  | EffOpen !FilePath      -- ^ Load a file (driver replies via 'setLoaded' or 'setError').
  | EffRevert !FilePath    -- ^ Reload a file in place, discarding edits (driver replies via 'revertLoaded').
  | EffStatFile !FilePath  -- ^ Stat a path to refresh the stale-on-disk flag (driver replies via 'noteDiskMtime').
  | EffSetTitle !String    -- ^ Update the terminal title.
  | EffBell                -- ^ Ring the terminal bell.
  | EffBrowse !(Maybe FilePath) -- ^ Open the file browser near a path (driver replies via 'startBrowser').
  | EffListDir !FilePath   -- ^ List a directory for the browser (driver replies via 'browserLoaded').
  | EffExplorerOpen !FilePath -- ^ Open a directory as the workspace folder (driver replies via 'explorerStart').
  | EffExplorerList !FilePath -- ^ List a directory for the explorer panel (driver replies via 'explorerLoaded').
  | EffCreatePath !FilePath   -- ^ Create a file (or, with a trailing @/@, a directory); the driver refreshes the explorer.
  | EffRenamePath !FilePath !FilePath -- ^ Rename/move a file or directory on disk.
  | EffDeletePath !FilePath   -- ^ Delete a file (or a directory recursively).
  | EffStartSearch !SearchReq -- ^ Kick off a background workspace search (driver streams results back).
  | EffFindDefs !DefReq    -- ^ Kick off a background go-to-definition scan (driver streams sites back via 'defFound'/'defDone').
  | EffQuickOpen !Int !FilePath -- ^ Start a quick-open file walk (gen, root); driver replies via 'quickOpenSeed'/'quickFilesFound'/'quickDone'.
  | EffReplaceOnDisk !ReplaceReq -- ^ Rewrite closed files on disk for a large Replace All (driver replies via 'replaceDone').
  | EffStageReplace !ReplaceReq -- ^ Open the closed files, apply the replacement in-buffer (unsaved), reveal them (driver replies via 'stageReplaceDone').
  | EffSaveAll               -- ^ Save every open document that has unsaved changes (driver replies via 'savedAll').
  deriving (Show)

-- | The closed-file part of a workspace Replace All: which files to rewrite on
-- disk (open files are edited in their buffers by the pure layer first).
data ReplaceReq = ReplaceReq
  { rrGen       :: !Int
  , rrTerm      :: !Text
  , rrRepl      :: !Text
  , rrCase      :: !Bool
  , rrWord      :: !Bool
  , rrRegex     :: !Bool
  , rrPaths     :: ![FilePath]  -- ^ closed files to rewrite on disk.
  , rrOpenCount :: !Int         -- ^ occurrences already replaced in open buffers (for the summary).
  } deriving (Show)

noEff :: Editor -> (Editor, [Effect])
noEff e = (e, [])

-- | Is this a CSV/TSV file (by extension)?
isCsvPath :: FilePath -> Bool
isCsvPath p = map toLower (takeExtension p) `elem` [".csv", ".tsv"]

isCsvFile :: Editor -> Bool
isCsvFile ed = maybe False isCsvPath (edPath ed)

csvDelimForPath :: FilePath -> Char
csvDelimForPath p = if map toLower (takeExtension p) == ".tsv" then '\t' else ','

csvDelimOf :: Editor -> Char
csvDelimOf ed = maybe ',' csvDelimForPath (edPath ed)

setStatus :: Text -> Editor -> Editor
setStatus s ed = ed { edStatus = s }

-- | The active document's cursor as a text position (a CSV table maps its
-- current cell back to the underlying text), for recording into the recents.
activeCursorPos :: Editor -> Pos
activeCursorPos ed = case edCsv ed of
  Just v  -> let (l, c) = Csv.cellTextPos v (Csv.csvCurRow v) (Csv.csvCurCol v) in Pos l c
  Nothing -> edCursor ed

docCursorPos :: Document -> Pos
docCursorPos d = case docCsv d of
  Just v  -> let (l, c) = Csv.cellTextPos v (Csv.csvCurRow v) (Csv.csvCurCol v) in Pos l c
  Nothing -> docCursor d

openAbout :: Editor -> Editor
openAbout ed = openDialog (mkAbout aboutText) ed { edAboutTick = 0 }

openHelp :: Editor -> Editor
openHelp ed = openDialog (mkHelp helpDialogText) ed

openDialog :: Dialog -> Editor -> Editor
openDialog d ed = ed { edDialog = Just d, edFocus = FDialog, edMenu = closedMenu }

-- Closing a menu returns to the editor (menu actions operate on the document),
-- leaving the search view if it was showing behind the dropdown.
leaveMenu :: Editor -> Editor
leaveMenu ed = ed { edFocus = FEdit, edMenu = closedMenu, edSearchMode = False }

-- Closing a dialog returns to whatever was underneath it: the search view when a
-- dialog was raised over it (e.g. the Replace All confirmation), else the editor.
closeDialog :: Editor -> Editor
closeDialog ed = ed { edDialog = Nothing, edFocus = if edSearchMode ed then FSearch else FEdit }

------------------------------------------------------------------------------
-- Dialog body layout (shared with the renderer and used for mouse hit-testing)

-- A flat description of the dialog body, one entry per row.
-- A field row carries (fieldIndex, lineWithinField, visibleHeight): a multi-line
-- Find/Replace value occupies 'fieldVisH' consecutive rows and scrolls within
-- them, just like a tall cell in the table view.
data DRow = DRMsg Text | DRField !Int !Int !Int | DROption Int | DRBlank | DRButtons

-- On-screen height of a field: its line count capped at the table view's cap, so
-- a multi-line value shows up to 'Csv.maxCellLines' rows (taller ones scroll).
fieldVisH :: Field -> Int
fieldVisH f = max 1 (min Csv.maxCellLines (Csv.cellLineCount (fText f)))

dialogRows :: Dialog -> [DRow]
dialogRows d = msgRows ++ fieldRows ++ optionRows ++ [DRBlank, DRButtons]
  where
    msgLines = if T.null (T.strip (dlgMessage d)) then [] else T.splitOn "\n" (dlgMessage d)
    msgRows  = map DRMsg msgLines
    fieldRows = if null (dlgFields d) then []
                else DRBlank : concat
                       [ let h = fieldVisH f in [ DRField i li h | li <- [0 .. h - 1] ]
                       | (i, f) <- zip [0 ..] (dlgFields d) ]
    optionRows = map DROption [0 .. length (dlgOptions d) - 1]

-- Row index (within dialogRows) at which field @fi@'s first line is drawn.
fieldRowIndex :: Dialog -> Int -> Int
fieldRowIndex d fi =
  let msgLines = if T.null (T.strip (dlgMessage d)) then [] else T.splitOn "\n" (dlgMessage d)
      above    = sum [ fieldVisH f | f <- take fi (dlgFields d) ]
  in length msgLines + 1 + above   -- +1 for the DRBlank preceding the fields

-- (y, x, height, width) of the dialog box on screen.
dialogGeom :: Editor -> Dialog -> Layout -> (Int, Int, Int, Int)
dialogGeom _ d lo =
  let cols = loCols lo; rows = loRows lo
      contentW = maximum (20 : map rowWidth (dialogRows d)
                            ++ [T.length (dlgTitle d) + 2]
                            ++ [aboutCanvasMinW | dlgKind d == DKAbout]
                            ++ [helpCanvasMinW | dlgKind d == DKHelp])
      w = min (cols - 2) (max 30 (contentW + 4))
      rs = dialogRows d
      h = min (rows - 2) (length rs + 2)
      x = (cols - w) `div` 2
      y = max 1 ((rows - h) `div` 2)
  in (y, x, h, w)
  where
    rowWidth (DRMsg m) = T.length m
    rowWidth (DRField i 0 _) = fieldLineWidth (dlgFields d !! i)  -- size once, on line 0
    rowWidth (DRField {})    = 0
    rowWidth (DROption i) = T.length (fst (dlgOptions d !! i)) + 4
    rowWidth DRBlank = 0
    rowWidth DRButtons = sum [ T.length b + 4 | b <- dlgButtons d ] + 2

-- The field box grows to fit its widest line; longer lines scroll horizontally.
fieldLineWidth :: Field -> Int
fieldLineWidth (Field lbl t _) =
  let widest = maximum (0 : map T.length (T.splitOn (T.pack "\n") t))
  in T.length lbl + 1 + max 20 (widest + 2)

-- The leading blank lines are the canvas the animated wordmark is drawn on
-- (the renderer overlays 'Cmedit.About.aboutFrameCells' there); the version
-- and copyright lines are hand-centred under it.
aboutText :: Text
aboutText = T.intercalate "\n" $
  replicate aboutCanvasH "" ++
  [ ""
  , center ("version " <> versionText)
  , ""
  , "A fast, modeless terminal editor: menus, mouse and"
  , "the real system clipboard, find & replace in files,"
  , "CSV tables and syntax highlighting \x2014 and it saves"
  , "files back exactly as it found them, no surprises."
  , ""
  , center "\x00a9 2026 Benjamin Marsh \x2014 GPL-3.0-only"
  ]
  where center t = T.replicate (max 0 ((51 - T.length t) `div` 2)) " " <> t

versionText :: Text
versionText = "0.3.0"

------------------------------------------------------------------------------
-- Search

normCase :: Bool -> Text -> Text
normCase cs = if cs then id else T.toLower

isWordCh :: Char -> Bool
isWordCh c = isAlphaNum c || c == '_'

hasNewline :: Text -> Bool
hasNewline = T.any (== '\n')

-- Collapse newlines to a glyph so a multi-line search term fits the one-line
-- status bar (a raw '\n' would corrupt the rendered row).
flat1 :: Text -> Text
flat1 = T.replace (T.pack "\n") (T.pack "\x21B5")

-- Render a non-negative count with thousands separators (12999 -> "12,999").
groupThousands :: Int -> String
groupThousands n = reverse (intercalate "," (chunk3 (reverse (show (max 0 n)))))
  where chunk3 [] = []
        chunk3 xs = let (a, b) = splitAt 3 xs in a : chunk3 b

------------------------------------------------------------------------------
-- Workspace-wide find / replace (the "Search" view; F4 / F6)
--
-- The panel occupies the main content area (to the right of the explorer, if
-- one is open). It is drawn whenever 'edSearchMode' is set — which stays true
-- while a menu or dialog overlays it, so the menu bar and the Replace All
-- confirmation appear *over* the results; interaction is gated on focus FSearch.
-- 'edSearch' persists across hide/show so opening a result and returning finds
-- the results intact. The
-- directory walk / file reads are IO (in "Cmedit.App"), which streams results
-- back through 'searchSeed' / 'searchFileFound' / 'searchProgress' / 'searchDone'
-- — the same effect/callback round-trip used for the browser and file loads.

-- | Is the search view the active main view (drawn instead of the document)?
-- Keyed off 'edSearchMode' rather than focus, so it stays drawn while a menu or
-- dialog overlays it (menus and confirm dialogs then appear *over* the results).
searchViewActive :: Editor -> Bool
searchViewActive ed = edSearchMode ed && isJust (edSearch ed)

-- | Where focus should land when a panel (explorer, dialog…) hands control back
-- to the main content area: the search view when it is the one showing, else
-- the editor. Focusing FEdit while the search view covers the document would
-- leave keystrokes editing an invisible buffer.
contentFocus :: Editor -> Focus
contentFocus ed = if searchViewActive ed then FSearch else FEdit

-- | Is a background search currently running (used to animate the spinner)?
searchRunning :: Editor -> Bool
searchRunning ed = maybe False ssRunning (edSearch ed)

-- | (top, left, height, width) of the search view's on-screen region.
searchRegion :: Layout -> (Int, Int, Int, Int)
searchRegion lo = (loTextTop lo, loContentLeft lo, loTextHeight lo, max 1 (loCols lo - loContentLeft lo - 1))

-- Best-guess root directory to search: the open folder, else the active file's
-- directory, else the current directory (the driver canonicalises it).
guessRoot :: Editor -> FilePath
guessRoot ed = case edExplorer ed of
  Just br -> fnPath (brRoot br)
  Nothing -> case edPath ed of
    Just p  -> takeDirectory p
    Nothing -> "."

-- Relative path of @p@ under canonical root @root@, if it is under it.
relativeTo :: FilePath -> FilePath -> Maybe FilePath
relativeTo root p =
  let r = if "/" `isSuffixOf` root then root else root ++ "/"
  in if r `isPrefixOf` p then Just (drop (length r) p)
     else if root == p then Just (takeFileName p) else Nothing

docText :: Document -> Text
docText = bufferToText LF False . docBuffer

isPlainDoc :: Document -> Bool
isPlainDoc d = isNothing (docCsv d) && isNothing (docImage d)

plural :: Int -> String
plural n = if n == 1 then "" else "s"

------------------------------------------------------------------------------
-- Navigation history (Alt+Left / Alt+Right)

maxNavStops :: Int
maxNavStops = 100

-- A jump within the same file only earns a history stop when it moves at
-- least this many lines — F3-stepping through nearby matches shouldn't flood
-- the trail, but Ctrl+End across a big file should be undoable.
navFarLines :: Int
navFarLines = 20

currentStop :: Editor -> NavStop
currentStop ed = NavStop (edPath ed) (activeCursorPos ed)

-- | Record the current location before jumping to @(tpath, tpos)@ — but only
-- when the jump is "far" (a different file, or 'navFarLines'+ lines away).
-- Starting a new jump invalidates the forward trail, like a browser.
pushNavIfFar :: Maybe FilePath -> Pos -> Editor -> Editor
pushNavIfFar tpath tpos ed
  | not isFar = ed
  | (s : _) <- edNavBack ed, s == cur = ed { edNavFwd = [] }
  | otherwise = ed { edNavBack = take maxNavStops (cur : edNavBack ed), edNavFwd = [] }
  where
    cur = currentStop ed
    isFar = tpath /= edPath ed
              || abs (posLine tpos - posLine (activeCursorPos ed)) >= navFarLines

------------------------------------------------------------------------------
-- IO-callback used after a system copy attempt

-- | Adjust the status line based on how a copy actually went.
confirmCopyOutcome :: CopyOutcome -> Editor -> Editor
confirmCopyOutcome outcome ed = case outcome of
  CopiedSystem -> ed
  UseOsc52     -> ed   -- the driver emits OSC52; keep the existing status
  CopyFailed   -> ed { edStatus = "Clipboard unavailable (copied internally)" }
