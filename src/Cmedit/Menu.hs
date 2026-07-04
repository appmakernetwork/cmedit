-- | The menu-bar definition and its navigation state. This module is pure
-- data: it knows what the menus look like and which 'MenuAction' each item
-- triggers, but not how to perform them. "Cmedit.Editor" interprets the
-- actions.
module Cmedit.Menu
  ( MenuAction(..)
  , MenuEntry(..)
  , Menu(..)
  , menuBar
  , MenuState(..)
  , closedMenu
  , menuAt
  , menuTitleDisp
  , entriesOf
  , selectableIndices
  , firstSelectable
  , menuAccelFor
  , parseMnemonic
  , mnemonicChar
  , mnemonicItem
  , mnemonicItemIn
  , isWindowMenu
  , isFileMenu
  ) where

import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | Every command reachable from the menus (and from keyboard shortcuts).
data MenuAction
  = MANew | MAOpen | MAOpenFolder | MACloseFolder | MASave | MASaveAs | MASaveAll | MARevert | MACloseFile | MAExit
  | MAQuickOpen                -- ^ The fuzzy go-to-file picker (Ctrl+P).
  | MAPalette                  -- ^ The command palette (quick open in '>' mode).
  | MAUndo | MARedo
  | MACut | MACopy | MAPaste | MADelete | MASelectAll
  | MADuplicateLine | MAMoveLineUp | MAMoveLineDown | MADeleteLine | MAJoinLines
  | MAToggleComment
  | MAFind | MAFindNext | MAFindPrev | MAReplace | MAGoToLine
  | MAFindInFiles | MAReplaceInFiles   -- ^ Workspace-wide (multi-file) find / replace.
  | MAGoToDef                  -- ^ Go to the definition of the identifier at the cursor.
  | MAGoToBracket              -- ^ Jump to the bracket matching the one at the cursor.
  | MANavBack | MANavFwd       -- ^ Navigation history: go back / forward (Alt+Left / Alt+Right).
  | MAToggleWordWrap | MAToggleLineNumbers | MAToggleWhitespace
  | MAToggleExplorer           -- ^ Show/focus/hide the file-explorer panel.
  | MAToggleCsv                -- ^ Toggle CSV table view (for .csv files).
  | MAToggleFreezeHeader       -- ^ Freeze the first table row while scrolling.
  | MASortColumn               -- ^ Sort the table by the current column (toggles asc/desc).
  | MACycleLineEnding          -- ^ Switch the saved line ending (LF ⇄ CRLF).
  | MAToggleBom                -- ^ Toggle the UTF-8 BOM written on save.
  | MAToggleTheme              -- ^ Switch the colour theme (dark ⇄ light).
  | MASwitchFile !Int          -- ^ Switch to the open file at this index.
  | MARecentFile !Int          -- ^ Open the k-th entry of the File menu's recent-files list.
  | MANextFile | MAPrevFile
  | MAAbout | MAHelp
  | MAManual                   -- ^ Open the built-in manual as a read-only document.
  | MANop
  deriving (Eq, Show)

-- | One row in a dropdown: either a command or a separator.
data MenuEntry
  = MEItem !Text !Text !MenuAction   -- ^ label, accelerator hint, action
  | MESep
  deriving (Eq, Show)

-- | A top-level menu: a title plus its dropdown entries.
data Menu = Menu
  { menuTitle   :: !Text
  , menuEntries :: ![MenuEntry]
  } deriving (Eq, Show)

-- | The full menu bar, in display order.
-- Item labels use an ampersand to mark the mnemonic key (e.g. @E&xit@ ->
-- press @x@); 'parseMnemonic' strips it for display.
menuBar :: [Menu]
menuBar =
  [ Menu "&File"
      [ MEItem "&New"            "Ctrl+N" MANew
      , MEItem "&Open\x2026"     "Ctrl+O" MAOpen
      , MEItem "Open &Folder\x2026" "" MAOpenFolder
      , MEItem "Go to F&ile\x2026" "Ctrl+P" MAQuickOpen
      , MEItem "&Save"           "Ctrl+S" MASave
      , MEItem "Save &As\x2026"  "Ctrl+Shift+S" MASaveAs
      , MEItem "Save A&ll"       "" MASaveAll
      , MEItem "Re&vert"         "" MARevert
      , MESep
      , MEItem "&Close File"     "Ctrl+W" MACloseFile
      , MEItem "Close Fol&der"   "" MACloseFolder
      , MEItem "E&xit"           "Ctrl+Q" MAExit
      ]
  , Menu "&Edit"
      [ MEItem "&Undo"           "Ctrl+Z" MAUndo
      , MEItem "&Redo"           "Ctrl+Y" MARedo
      , MESep
      , MEItem "Cu&t"            "Ctrl+X" MACut
      , MEItem "&Copy"           "Ctrl+C" MACopy
      , MEItem "&Paste"          "Ctrl+V" MAPaste
      , MEItem "&Delete"         "Del"    MADelete
      , MESep
      , MEItem "Dup&licate Line" "Ctrl+D" MADuplicateLine
      , MEItem "&Move Line Up"   "Alt+\x2191" MAMoveLineUp
      , MEItem "Mo&ve Line Down" "Alt+\x2193" MAMoveLineDown
      , MEItem "D&elete Line"    "Ctrl+Shift+K" MADeleteLine
      , MEItem "&Join Lines"     "Alt+J"  MAJoinLines
      , MEItem "Toggle C&omment" "Ctrl+/" MAToggleComment
      , MESep
      , MEItem "Select &All"     "Ctrl+A" MASelectAll
      ]
  , Menu "F&ind"
      [ MEItem "&Find\x2026"     "Ctrl+F" MAFind
      , MEItem "Find &Next"      "F3"     MAFindNext
      , MEItem "Find &Previous"  "Shift+F3" MAFindPrev
      , MEItem "&Replace\x2026"  "Ctrl+R" MAReplace
      , MESep
      , MEItem "Find in Fi&les\x2026"    "F4" MAFindInFiles
      , MEItem "Replace in Files\x2026"  "F6" MAReplaceInFiles
      , MESep
      , MEItem "Go to &Definition" "F12" MAGoToDef
      , MEItem "&Go to Line\x2026" "Ctrl+G" MAGoToLine
      , MEItem "Go to &Bracket"  "Ctrl+]" MAGoToBracket
      , MESep
      , MEItem "Go Bac&k"        "Alt+\x2190" MANavBack
      , MEItem "Go For&ward"     "Alt+\x2192" MANavFwd
      ]
  , Menu "&View"
      [ MEItem "&Table View (CSV)" "Alt+T" MAToggleCsv
      , MEItem "&Freeze Header Row" "" MAToggleFreezeHeader
      , MEItem "S&ort by Column"   "Alt+S" MASortColumn
      , MESep
      , MEItem "&Word Wrap"       "Alt+Z" MAToggleWordWrap
      , MEItem "&Line Numbers"    "Alt+L" MAToggleLineNumbers
      , MEItem "White&space"      ""      MAToggleWhitespace
      , MEItem "&Explorer"        "Ctrl+B" MAToggleExplorer
      , MESep
      -- Labels are rewritten per document to show the current value
      -- (Cmedit.Editor.relabelEntries).
      , MEItem "Line E&ndings: LF" "" MACycleLineEnding
      , MEItem "&UTF-8 BOM: off"   "" MAToggleBom
      , MEItem "The&me: dark"      "" MAToggleTheme
      ]
  , Menu "&Window"
      [ MEItem "&Next File"       "Alt+." MANextFile
      , MEItem "&Previous File"   "Alt+," MAPrevFile
      ]
  , Menu "&Help"
      [ MEItem "&Command Palette\x2026" "Ctrl+Shift+P" MAPalette
      , MEItem "&Keyboard Help"   "F1"    MAHelp
      , MEItem "&Manual"          ""      MAManual
      , MEItem "&About CMeDit"     ""      MAAbout
      ]
  ]

-- | True if menu @i@ is the Window menu, whose entries are generated from the
-- open-files list rather than the static definition above.
isWindowMenu :: Int -> Bool
isWindowMenu i = fmap menuTitleDisp (menuAt i) == Just "Window"

-- | True if menu @i@ is the File menu, which gets a dynamic recent-files
-- section spliced in above Exit.
isFileMenu :: Int -> Bool
isFileMenu i = fmap menuTitleDisp (menuAt i) == Just "File"

-- | Navigation state for the menu bar.
data MenuState = MenuState
  { msMenuIx :: !Int        -- ^ Highlighted top-level menu.
  , msOpen   :: !Bool       -- ^ Is the dropdown open (vs. just bar focus)?
  , msItemIx :: !Int        -- ^ Highlighted entry within the open dropdown.
  } deriving (Eq, Show)

closedMenu :: MenuState
closedMenu = MenuState 0 False 0

menuAt :: Int -> Maybe Menu
menuAt i
  | i >= 0 && i < length menuBar = Just (menuBar !! i)
  | otherwise                    = Nothing

entriesOf :: Int -> [MenuEntry]
entriesOf i = maybe [] menuEntries (menuAt i)

-- | Indices of entries that can be highlighted (skipping separators).
selectableIndices :: [MenuEntry] -> [Int]
selectableIndices es = [ i | (i, e) <- zip [0 ..] es, selectable e ]
  where selectable MESep = False
        selectable _     = True

firstSelectable :: [MenuEntry] -> Int
firstSelectable es = case selectableIndices es of
  (i : _) -> i
  []      -> 0

-- | Find the menu action whose title starts with the given accelerator
-- letter (used for Alt+letter menu access).
-- | The on-screen menu title (mnemonic ampersand stripped). The raw 'menuTitle'
-- keeps the @&@ so a menu can mark a mnemonic other than its first letter
-- (e.g. @F&ind@ -> @i@, leaving @f@ free for File).
menuTitleDisp :: Menu -> Text
menuTitleDisp = fst . parseMnemonic . menuTitle

menuAccelFor :: Char -> Maybe Int
menuAccelFor ch =
  let c = toLower ch
  in listToMaybe [ i | (i, m) <- zip [0 ..] menuBar, mnemonicChar (menuTitle m) == Just c ]

-- | Strip the @&@ mnemonic marker from a label, returning the display text and
-- the index (into the display text) of the mnemonic character, or -1 if none.
parseMnemonic :: Text -> (Text, Int)
parseMnemonic t =
  let (before, rest) = T.breakOn (T.pack "&") t
  in if T.null rest
       then (t, -1)
       else (before <> T.drop 1 rest, T.length before)

-- | The mnemonic character of a label (lower-cased), if it has one.
mnemonicChar :: Text -> Maybe Char
mnemonicChar t =
  let (disp, i) = parseMnemonic t
  in if i >= 0 && i < T.length disp then Just (toLower (T.index disp i)) else Nothing

-- | Index of the entry in menu @mi@ whose mnemonic matches @c@ (case
-- insensitive), if any.
mnemonicItem :: Int -> Char -> Maybe Int
mnemonicItem mi = mnemonicItemIn (entriesOf mi)

-- | As 'mnemonicItem' but over an explicit entry list (for dynamic menus).
-- Falls back to matching the first letter of the display label.
mnemonicItemIn :: [MenuEntry] -> Char -> Maybe Int
mnemonicItemIn entries c =
  let target = toLower c
      byMnemonic = [ i | (i, MEItem lbl _ _) <- zip [0 ..] entries
                       , mnemonicChar lbl == Just target ]
      byFirst    = [ i | (i, MEItem lbl _ _) <- zip [0 ..] entries
                       , let d = fst (parseMnemonic lbl)
                       , not (T.null d), toLower (T.head d) == target ]
  in listToMaybe (byMnemonic ++ byFirst)
