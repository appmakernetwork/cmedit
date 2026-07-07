-- | Modal dialogs: open/save prompts, find, replace, go-to-line, the about
-- box and confirmation popups. This module holds the dialog data and pure
-- helpers for editing the input fields and moving focus. "Cmedit.Editor"
-- decides what a dialog /means/ when it is confirmed.
module Cmedit.Dialog
  ( DialogKind(..)
  , Field(..)
  , Choice(..)
  , Dialog(..)
    -- * Constructors
  , mkOpen
  , mkSaveAs
  , mkFind
  , mkReplace
  , mkGoToLine
  , mkNewPath
  , mkRename
  , mkConfirm
  , mkAbout
  , mkMessage
  , mkHelp
  , mkTheme
    -- * Focus
  , focusCount
  , focusedField
  , focusedChoice
  , focusIsButton
  , focusedButton
  , focusNext
  , focusPrev
  , setFocus
  , cycleChoice
  , setChoiceIx
    -- * Field editing
  , fieldInsert
  , fieldBackspace
  , fieldDelete
  , fieldLeft
  , fieldRight
  , fieldHome
  , fieldEnd
  , fieldLineUp
  , fieldLineDown
  , fieldSetCursorLineCol
  , fieldDeleteWordLeft
  , setFieldText
  , toggleOption
    -- * Queries
  , fieldValue
  , optionValue
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

data DialogKind
  = DKOpen
  | DKSaveAs
  | DKFind
  | DKReplace
  | DKGoToLine
  | DKNewPath        -- ^ Explorer: create a file (or folder, with a trailing @/@).
  | DKRename         -- ^ Explorer: rename the selected file/folder.
  | DKConfirmDelete  -- ^ Explorer: delete the selected file/folder.
  | DKConfirmQuit
  | DKConfirmQuitAll
  | DKConfirmSaveAll
  | DKConfirmClose
  | DKConfirmCloseFolder
  | DKConfirmRevert
  | DKConfirmOverwrite
  | DKConfirmReplaceAll
  | DKAbout
  | DKHelp           -- ^ The F1 keyboard card (Manual button + styled overlay).
  | DKTheme          -- ^ View ▸ Theme: one button per colour theme, live-previewed.
  | DKSettings       -- ^ File ▸ Settings: a choice row per config key, applied live.
  | DKMessage
  deriving (Eq, Show)

-- | A single editable text input.
data Field = Field
  { fLabel :: !Text
  , fText  :: !Text
  , fCur   :: !Int    -- ^ Cursor as a character index into 'fText'.
  } deriving (Eq, Show)

-- | One settings-style row: a label and a value cycled through a fixed list.
data Choice = Choice
  { chLabel  :: !Text
  , chValues :: ![Text]        -- ^ non-empty; shown one at a time
  , chIx     :: !Int           -- ^ current value index
  , chHeader :: !(Maybe Text)  -- ^ section header drawn above this row, if it starts a group
  , chHint   :: !Text          -- ^ one-line contextual help shown while the row is focused
  , chNote   :: !(Maybe Text)  -- ^ an always-visible dimmed line drawn under the row (e.g. a linter's "✗ not installed — pip install ruff" availability note).
  } deriving (Eq, Show)

-- | A modal dialog. Focus ranges over fields, then options (checkboxes),
-- then choices (value pickers), then buttons, in that order.
data Dialog = Dialog
  { dlgKind    :: !DialogKind
  , dlgTitle   :: !Text
  , dlgFields  :: ![Field]
  , dlgOptions :: ![(Text, Bool)]
  , dlgChoices :: ![Choice]
  , dlgButtons :: ![Text]
  , dlgFocus   :: !Int
  , dlgMessage :: !Text
  , dlgPristine :: !Bool  -- ^ The seeded first-field text is untouched: the next typed character replaces it (like a selected value).
  } deriving (Eq, Show)

------------------------------------------------------------------------------
-- Constructors

field :: Text -> Text -> Field
field lbl t = Field lbl t (T.length t)

mkOpen :: Text -> Dialog
mkOpen path = Dialog DKOpen "Open File"
  [field "Path:" path] [] [] ["Open", "Cancel"] 0 "" False

mkSaveAs :: Text -> Dialog
mkSaveAs path = Dialog DKSaveAs "Save As"
  [field "Path:" path] [] [] ["Save", "Cancel"] 0 "" False

mkFind :: Text -> Bool -> Bool -> Dialog
mkFind term caseSens wholeWord = Dialog DKFind "Find"
  [field "Find:" term]
  [("Match case", caseSens), ("Whole word", wholeWord)]
  []
  ["Find", "Close"] 0 "" True

mkReplace :: Text -> Text -> Bool -> Dialog
mkReplace term repl caseSens = Dialog DKReplace "Replace"
  [field "Find:" term, field "Replace:" repl]
  [("Match case", caseSens)]
  []
  ["Replace", "Replace All", "Close"] 0 "" True

mkGoToLine :: Dialog
mkGoToLine = Dialog DKGoToLine "Go to Line"
  [field "Line:" ""] [] [] ["Go", "Cancel"] 0 "" False

-- | Explorer "new file/folder" prompt. @where_@ names the directory it will
-- be created in (display only; the caller re-derives it on confirm).
mkNewPath :: Text -> Dialog
mkNewPath where_ = Dialog DKNewPath "New File / Folder"
  [field "Name:" ""] [] [] ["Create", "Cancel"] 0
  ("In " <> where_ <> "\nEnd the name with / to create a folder.") False

-- | Explorer rename prompt, seeded with the current name.
mkRename :: Text -> Dialog
mkRename oldName = Dialog DKRename "Rename"
  [field "New name:" oldName] [] [] ["Rename", "Cancel"] 0
  ("Renaming " <> oldName) False   -- renames tweak the old name, so keep it editable

mkConfirm :: DialogKind -> Text -> Text -> [Text] -> Dialog
mkConfirm kind title msg buttons = Dialog kind title [] [] [] buttons 0 msg False

mkAbout :: Text -> Dialog
mkAbout msg = Dialog DKAbout "About CMeDit" [] [] [] ["OK"] 0 msg False

mkMessage :: Text -> Text -> Dialog
mkMessage title msg = Dialog DKMessage title [] [] [] ["OK"] 0 msg False

-- | The keyboard help card. Close (not Manual) starts focused, so Enter
-- dismisses; the message is the card's blank canvas plus its footer
-- ("Cmedit.HelpCard"), overlaid with styled cells by the renderer.
mkHelp :: Text -> Dialog
mkHelp msg = Dialog DKHelp "Keyboard Shortcuts" [] [] [] ["Manual", "Close"] 1 msg False

-- | View ▸ Theme: pick the colour theme directly — one button per mode,
-- focus starting on the current one (@cur@ indexes the theme buttons in the
-- same order as @Cmedit.EditorState.themeChoices@). Moving the focus
-- live-previews the theme ('resolvedTheme' consults this dialog's focused
-- button); Esc or Cancel restores what you came in with.
mkTheme :: Int -> Dialog
mkTheme cur = Dialog DKTheme "Theme"
  [] [] []
  [ "Auto", "Dark Terminal", "Light Terminal"
  , "Cherry Blossom", "Flashbang", "Midnight", "Cancel" ] cur
  "Applies for this session; set theme = ... in the config to keep it."
  False

------------------------------------------------------------------------------
-- Focus

-- | Total number of focusable elements (fields + options + choices + buttons).
focusCount :: Dialog -> Int
focusCount d = nFields d + nOptions d + nChoices d + length (dlgButtons d)

nFields :: Dialog -> Int
nFields = length . dlgFields

nOptions :: Dialog -> Int
nOptions = length . dlgOptions

nChoices :: Dialog -> Int
nChoices = length . dlgChoices

-- | If a field is focused, its index; otherwise Nothing.
focusedField :: Dialog -> Maybe Int
focusedField d
  | dlgFocus d < nFields d = Just (dlgFocus d)
  | otherwise              = Nothing

-- | If an option is focused, its index; otherwise Nothing.
focusedOption :: Dialog -> Maybe Int
focusedOption d =
  let i = dlgFocus d - nFields d
  in if i >= 0 && i < nOptions d then Just i else Nothing

-- | If a choice row is focused, its index (into 'dlgChoices'); otherwise Nothing.
focusedChoice :: Dialog -> Maybe Int
focusedChoice d =
  let i = dlgFocus d - nFields d - nOptions d
  in if i >= 0 && i < nChoices d then Just i else Nothing

-- | Is a button currently focused?
focusIsButton :: Dialog -> Bool
focusIsButton d = dlgFocus d >= nFields d + nOptions d + nChoices d

-- | The focused button index (into 'dlgButtons'), if any.
focusedButton :: Dialog -> Maybe Int
focusedButton d =
  let i = dlgFocus d - nFields d - nOptions d - nChoices d
  in if i >= 0 && i < length (dlgButtons d) then Just i else Nothing

focusNext :: Dialog -> Dialog
focusNext d
  | focusCount d == 0 = d
  | otherwise         = d { dlgFocus = (dlgFocus d + 1) `mod` focusCount d }

focusPrev :: Dialog -> Dialog
focusPrev d
  | focusCount d == 0 = d
  | otherwise         = d { dlgFocus = (dlgFocus d - 1) `mod` focusCount d }

setFocus :: Int -> Dialog -> Dialog
setFocus i d
  | focusCount d == 0 = d
  | otherwise         = d { dlgFocus = max 0 (min i (focusCount d - 1)) }

------------------------------------------------------------------------------
-- Field editing (operates on the focused field, if any)

onFocusedField :: (Field -> Field) -> Dialog -> Dialog
onFocusedField f d = case focusedField d of
  Nothing -> d
  Just i  -> d { dlgFields = adjust i f (dlgFields d) }

adjust :: Int -> (a -> a) -> [a] -> [a]
adjust i f xs = [ if j == i then f x else x | (j, x) <- zip [0 ..] xs ]

fieldInsert :: Char -> Dialog -> Dialog
fieldInsert ch = onFocusedField $ \(Field l t c) ->
  Field l (T.take c t <> T.singleton ch <> T.drop c t) (c + 1)

fieldBackspace :: Dialog -> Dialog
fieldBackspace = onFocusedField $ \(Field l t c) ->
  if c > 0 then Field l (T.take (c - 1) t <> T.drop c t) (c - 1)
           else Field l t c

fieldDelete :: Dialog -> Dialog
fieldDelete = onFocusedField $ \(Field l t c) ->
  if c < T.length t then Field l (T.take c t <> T.drop (c + 1) t) c
                    else Field l t c

fieldLeft :: Dialog -> Dialog
fieldLeft = onFocusedField $ \(Field l t c) -> Field l t (max 0 (c - 1))

fieldRight :: Dialog -> Dialog
fieldRight = onFocusedField $ \(Field l t c) -> Field l t (min (T.length t) (c + 1))

-- Home/End act on the current visual line (so they behave on a multi-line field
-- and on a single-line one they are start/end of the whole value, as before).
fieldHome :: Dialog -> Dialog
fieldHome = onFocusedField $ \f@(Field l t _) ->
  let (ln, _) = fieldLineCol f in Field l t (lineColCur t ln 0)

fieldEnd :: Dialog -> Dialog
fieldEnd = onFocusedField $ \f@(Field l t _) ->
  let (ln, _)  = fieldLineCol f
      lineWidth = T.length (T.splitOn (T.pack "\n") t !! ln)
  in Field l t (lineColCur t ln lineWidth)

-- | Move the cursor up/down one visual line within the focused field, keeping
-- the column. 'Nothing' when there is no focused field or the cursor is already
-- on the first/last line — the caller then shifts focus to another control.
fieldLineUp :: Dialog -> Maybe Dialog
fieldLineUp = fieldLineMove (-1)

fieldLineDown :: Dialog -> Maybe Dialog
fieldLineDown = fieldLineMove 1

fieldLineMove :: Int -> Dialog -> Maybe Dialog
fieldLineMove dir d = do
  i <- focusedField d
  let f@(Field _ t _) = dlgFields d !! i
      (ln, col)       = fieldLineCol f
      ln'             = ln + dir
      nlines          = length (T.splitOn (T.pack "\n") t)
  if ln' < 0 || ln' >= nlines
    then Nothing
    else Just d { dlgFields = adjust i (setCur (lineColCur t ln' col)) (dlgFields d) }

setCur :: Int -> Field -> Field
setCur c (Field l t _) = Field l t (max 0 (min (T.length t) c))

-- | Place the focused field's cursor at a (line, column), clamped to the text.
-- The editor uses this for mouse clicks, mapping the clicked screen cell to a
-- line/column (accounting for the field's vertical/horizontal scroll).
fieldSetCursorLineCol :: Int -> Int -> Dialog -> Dialog
fieldSetCursorLineCol line col = onFocusedField $ \(Field l t _) ->
  Field l t (lineColCur t line col)

-- (line, column) of the cursor within a (possibly multi-line) field value.
fieldLineCol :: Field -> (Int, Int)
fieldLineCol (Field _ t c) =
  let before = T.take c t
  in (T.count nl before, T.length (last (T.splitOn nl before)))
  where nl = T.pack "\n"

-- Character index of a (line, column), clamped to the text.
lineColCur :: Text -> Int -> Int -> Int
lineColCur t line col =
  let ls    = T.splitOn (T.pack "\n") t
      line' = max 0 (min (length ls - 1) line)
      col'  = max 0 (min col (T.length (ls !! line')))
  in sum (map ((+ 1) . T.length) (take line' ls)) + col'

-- | Delete the word before the cursor (Ctrl+Backspace in a field).
fieldDeleteWordLeft :: Dialog -> Dialog
fieldDeleteWordLeft = onFocusedField $ \(Field l t c) ->
  let before = reverse (T.unpack (T.take c t))
      kept   = reverse (dropWhile (not . isSpace) (dropWhile isSpace before))
  in Field l (T.pack kept <> T.drop c t) (length kept)

-- | Replace field @i@'s text outright (cursor at the end) — history recall.
setFieldText :: Int -> Text -> Dialog -> Dialog
setFieldText i t d = d { dlgFields = adjust i (\(Field l _ _) -> Field l t (T.length t)) (dlgFields d) }

-- | Toggle the focused checkbox option, if one is focused.
toggleOption :: Dialog -> Dialog
toggleOption d = case focusedOption d of
  Nothing -> d
  Just i  -> d { dlgOptions = adjust i (\(t, b) -> (t, not b)) (dlgOptions d) }

-- | Advance choice row @ci@'s value by @dir@ (+1 forward, -1 back), wrapping
-- around its (non-empty) value list. A no-op for an out-of-range index or an
-- empty value list.
cycleChoice :: Int -> Int -> Dialog -> Dialog
cycleChoice ci dir d
  | ci < 0 || ci >= nChoices d = d
  | otherwise = d { dlgChoices = adjust ci step (dlgChoices d) }
  where step c = let n = length (chValues c)
                 in if n == 0 then c else c { chIx = (chIx c + dir) `mod` n }

-- | Set choice row @ci@'s value index directly, clamped to its value list.
setChoiceIx :: Int -> Int -> Dialog -> Dialog
setChoiceIx ci ix d
  | ci < 0 || ci >= nChoices d = d
  | otherwise = d { dlgChoices = adjust ci put (dlgChoices d) }
  where put c = let n = length (chValues c)
                in if n == 0 then c else c { chIx = max 0 (min (n - 1) ix) }

------------------------------------------------------------------------------
-- Queries

fieldValue :: Int -> Dialog -> Text
fieldValue i d = case drop i (dlgFields d) of
  (Field _ t _ : _) -> t
  _                 -> ""

optionValue :: Int -> Dialog -> Bool
optionValue i d = case drop i (dlgOptions d) of
  ((_, b) : _) -> b
  _            -> False
