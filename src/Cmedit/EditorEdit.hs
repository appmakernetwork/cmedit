-- | Core buffer editing: selection, scrolling, wrap geometry,
-- movement, undo, the editing primitives, line operations, comment
-- toggling, bracket matching, save-time fixups, word completion and
-- the file-property (EOL/BOM/status-bar) logic.
module Cmedit.EditorEdit where


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
import Cmedit.Clipboard (CopyOutcome(..))
import Cmedit.Image (Image(..), ImgMode(..), renderImage, viewFit)
import Cmedit.Syntax (HlCache, CommentSyntax(..), langComment, langForPath)

import Cmedit.EditorState


------------------------------------------------------------------------------
-- Selection helpers

-- | The current selection range (normalised), if any non-empty selection.
getSelection :: Editor -> Maybe (Pos, Pos)
getSelection ed = case edSelAnchor ed of
  Nothing -> Nothing
  Just a
    | a == edCursor ed -> Nothing
    | posLE a (edCursor ed) -> Just (a, edCursor ed)
    | otherwise             -> Just (edCursor ed, a)

hasSelection :: Editor -> Bool
hasSelection = isJust . getSelection

clearSel :: Editor -> Editor
clearSel ed = ed { edSelAnchor = Nothing }

------------------------------------------------------------------------------
-- Scrolling

ensureVisible :: Editor -> Editor
ensureVisible ed
  | edWordWrap ed = ensureVisibleWrap ed
ensureVisible ed =
  let lo = computeLayout ed
      th = loTextHeight lo
      tw = loTextWidth lo
      Pos l c = edCursor ed
      top0 = edTop ed
      top1 | l < top0          = l
           | l >= top0 + th    = l - th + 1
           | otherwise         = top0
      dcol = colToDisplay (tabWidthOf ed) c (getLine' l (edBuffer ed))
      left0 = edLeft ed
      left1 | dcol < left0      = dcol
            | dcol >= left0 + tw = dcol - tw + 1
            | otherwise         = left0
  in ed { edTop = max 0 top1, edLeft = max 0 left1 }

curDisplayCol :: Editor -> Int
curDisplayCol ed
  | edWordWrap ed =
      let Pos l c = edCursor ed
          segs = lineSegs ed l
          (s, _) = segs !! segIndexOf segs c
          line = getLine' l (edBuffer ed)
      in colToDisplay (tabWidthOf ed) c line - colToDisplay (tabWidthOf ed) s line
  | otherwise =
      colToDisplay (tabWidthOf ed) (posCol (edCursor ed)) (currentLine ed)

------------------------------------------------------------------------------
-- Word wrap geometry (used only when edWordWrap is on)

-- Wrapped segments (startCol, endCol) of buffer line @li@.
lineSegs :: Editor -> Int -> [(Int, Int)]
lineSegs ed li =
  wrapLine (tabWidthOf ed) (max 1 (loTextWidth (computeLayout ed))) (getLine' li (edBuffer ed))

-- Index of the segment containing character column @c@.
segIndexOf :: [(Int, Int)] -> Int -> Int
segIndexOf segs c = go 0 segs
  where
    go _ [] = max 0 (length segs - 1)
    go i ((_, e) : rest)
      | c < e     = i
      | null rest = i
      | otherwise = go (i + 1) rest

-- Position within a segment closest to the desired visual column @want@.
posInSeg :: Editor -> Int -> (Int, Int) -> Int -> Pos
posInSeg ed li (s, e) want =
  let line = getLine' li (edBuffer ed)
      tabw = tabWidthOf ed
      absDisp = colToDisplay tabw s line + want
      c = displayToCol tabw absDisp line
  in Pos li (max s (min e c))

-- One visual-row step up (dir<0) or down (dir>0).
visualStep :: Int -> Int -> Editor -> Pos
visualStep dir want ed =
  let buf = edBuffer ed
      Pos l c = edCursor ed
      segs = lineSegs ed l
      i = segIndexOf segs c
  in if dir > 0
       then if i < length segs - 1
              then posInSeg ed l (segs !! (i + 1)) want
              else if l < lineCount buf - 1
                     then posInSeg ed (l + 1) (head (lineSegs ed (l + 1))) want
                     else edCursor ed
       else if i > 0
              then posInSeg ed l (segs !! (i - 1)) want
              else if l > 0
                     then let segs' = lineSegs ed (l - 1) in posInSeg ed (l - 1) (last segs') want
                     else edCursor ed

moveVisual :: Bool -> Int -> Editor -> Editor
moveVisual extend delta ed =
  let want = edDesiredCol ed
      go 0 e = e
      go k e = go (k - 1) (e { edCursor = clampPos (visualStep (signum delta) want e) (edBuffer e) })
      ed1 = go (abs delta) ed
  in moveTo extend (edCursor ed1) ed

-- Number of visual rows between the top of line @top@ and the cursor's row.
visualOffset :: Editor -> Int -> Pos -> Int
visualOffset ed top (Pos l c) =
  sum [ length (lineSegs ed li) | li <- [top .. l - 1] ]
    + segIndexOf (lineSegs ed l) c

ensureVisibleWrap :: Editor -> Editor
ensureVisibleWrap ed =
  let th = loTextHeight (computeLayout ed)
      Pos l c = edCursor ed
      t0 = max 0 (min (edTop ed) l)
      -- Smallest top that keeps the cursor within @th@ visual rows, found by
      -- walking BACKWARD from the cursor accumulating wrapped line heights —
      -- at most a screenful of lines, however far the cursor jumped. (Stepping
      -- the top forward one row at a time, re-measuring the whole span each
      -- step, made Ctrl+End / go-to-line O(distance²) — minutes on a large
      -- wrapped file.)
      back t acc
        | t <= 0 = 0
        | otherwise =
            let h = length (lineSegs ed (t - 1))
            in if acc + h < th then back (t - 1) (acc + h) else t
      tStar = back l (segIndexOf (lineSegs ed l) c)
  in ed { edTop = max t0 tStar, edLeft = 0 }

------------------------------------------------------------------------------
-- Cursor movement

moveTo :: Bool -> Pos -> Editor -> Editor
moveTo extend newPos ed =
  let anchor = if extend
                 then Just (fromMaybe (edCursor ed) (edSelAnchor ed))
                 else Nothing
      cur = clampPos newPos (edBuffer ed)
  in ensureVisible ed { edCursor = cur, edSelAnchor = anchor, edLastEdit = EKNone }

moveHoriz :: Bool -> (Editor -> Pos) -> Editor -> Editor
moveHoriz extend f ed =
  let ed' = moveTo extend (f ed) ed
  in ed' { edDesiredCol = curDisplayCol ed' }

moveVert :: Bool -> Int -> Editor -> Editor
moveVert extend delta ed
  | edWordWrap ed = moveVisual extend delta ed
  | otherwise =
      let buf = edBuffer ed
          Pos l _ = edCursor ed
          l' = max 0 (min (lineCount buf - 1) (l + delta))
          col = displayToCol (tabWidthOf ed) (edDesiredCol ed) (getLine' l' buf)
      in moveTo extend (Pos l' col) ed

------------------------------------------------------------------------------
-- Undo / redo

maxUndo :: Int
maxUndo = 1000

snapshot :: Editor -> UndoState
snapshot ed = UndoState (edBuffer ed) (edCursor ed) (edSelAnchor ed)

-- Does a buffer differ from the last saved/loaded content? The O(1) size
-- checks (line count, then the buffer's carried character count) short-circuit
-- nearly every edit; the element-wise comparison only runs when the buffer is
-- exactly the saved size — the only case where it could have returned to
-- unmodified — and is itself pointer-accelerated per line, so typing back to
-- the original content clears the flag without a full content diff.
bufModified :: Editor -> Buffer -> Bool
bufModified ed buf =
  let saved = edSavedBuffer ed
  in lineCount buf /= lineCount saved
       || bufChars buf /= bufChars saved
       || buf /= saved

-- Begin an edit of the given kind, pushing an undo checkpoint unless it can be
-- coalesced with the previous edit of the same kind.
beginEdit :: EditKind -> Editor -> Editor
beginEdit kind ed
  | coalesces && kind == edLastEdit ed = ed { edRedo = [] }
  | otherwise = ed { edUndo = take maxUndo (snapshot ed : edUndo ed)
                   , edRedo = []
                   , edLastEdit = kind }
  where coalesces = kind == EKType || kind == EKDelete || kind == EKMoveLine

undo :: Editor -> Editor
undo ed = case edUndo ed of
  [] -> ed { edStatus = "Nothing to undo" }
  (u : us) ->
    ensureVisible ed
      { edUndo = us
      , edRedo = snapshot ed : edRedo ed
      , edBuffer = usBuffer u
      , edCursor = clampPos (usCursor u) (usBuffer u)
      , edSelAnchor = usAnchor u
      , edLastEdit = EKNone
      , edModified = metaModified ed || bufModified ed (usBuffer u)
      , edStatus = ""
      }

redo :: Editor -> Editor
redo ed = case edRedo ed of
  [] -> ed { edStatus = "Nothing to redo" }
  (u : us) ->
    ensureVisible ed
      { edRedo = us
      , edUndo = snapshot ed : edUndo ed
      , edBuffer = usBuffer u
      , edCursor = clampPos (usCursor u) (usBuffer u)
      , edSelAnchor = usAnchor u
      , edLastEdit = EKNone
      , edModified = metaModified ed || bufModified ed (usBuffer u)
      , edStatus = ""
      }

------------------------------------------------------------------------------
-- Editing primitives

-- Remove the current selection (if any). Does not push undo itself.
removeSelection :: Editor -> Editor
removeSelection ed = case getSelection ed of
  Nothing -> ed
  Just (a, b) ->
    let (buf', cur') = deleteRange a b (edBuffer ed)
    in ed { edBuffer = buf', edCursor = cur', edSelAnchor = Nothing }

-- Does the file's save form differ from disk in ways the buffer text can't
-- show — a switched line ending or a toggled BOM? Composed into every
-- modified-flag computation so "undo back to the original text" cannot clear
-- the flag while an EOL/BOM change is still pending.
metaModified :: Editor -> Bool
metaModified ed = edLineEnding ed /= edSavedEol ed || edEncoding ed /= edSavedEnc ed

afterEdit :: Editor -> Editor
afterEdit ed =
  ensureVisible ed { edModified = metaModified ed || bufModified ed (edBuffer ed), edStatus = "" }

typeChar :: Char -> Editor -> Editor
typeChar ch ed0
  | edReadOnly ed0 = ed0 { edStatus = "File is read-only" }
  | otherwise =
      let kind = if hasSelection ed0 then EKOther else EKType
          ed1  = removeSelection (beginEdit kind ed0)
          Pos l c = edCursor ed1
          overwrite = edOverwrite ed1 && not (hasSelection ed0) && c < lineLen l (edBuffer ed1)
          (buf', cur')
            | overwrite = overwriteChar (edCursor ed1) ch (edBuffer ed1)
            | otherwise = insertChar (edCursor ed1) ch (edBuffer ed1)
          ed2 = ed1 { edBuffer = buf', edCursor = cur' }
      in setDesired (afterEdit ed2)

insertRaw :: Text -> Editor -> Editor
insertRaw txt ed1 =
  let (buf', cur') = insertText (edCursor ed1) txt (edBuffer ed1)
  in ed1 { edBuffer = buf', edCursor = cur' }

setDesired :: Editor -> Editor
setDesired ed = ed { edDesiredCol = curDisplayCol ed }

newline :: Editor -> Editor
newline ed0
  | edReadOnly ed0 = ed0 { edStatus = "File is read-only" }
  | otherwise =
      let ed1 = removeSelection (beginEdit EKOther ed0)
          indent = if cfgAutoIndent (edConfig ed1)
                     then T.takeWhile isSpace (getLine' (posLine (edCursor ed1)) (edBuffer ed1))
                     else ""
          (buf', cur') = splitLineAt (edCursor ed1) (edBuffer ed1)
          ed2 = ed1 { edBuffer = buf', edCursor = cur' }
          ed3 = if T.null indent then ed2 else insertRaw indent ed2
      in setDesired (afterEdit ed3)

insertTab :: Editor -> Editor
insertTab ed0
  | edReadOnly ed0 = ed0 { edStatus = "File is read-only" }
  | hasSelection ed0 = indentSelection ed0
  | cfgTabsToSpaces (edConfig ed0) =
      let dcol = curDisplayCol ed0
          tw   = tabWidthOf ed0
          n    = tw - (dcol `mod` tw)
      in foldr (\_ e -> typeChar ' ' e) ed0 (replicate n ())
  | otherwise = typeChar '\t' ed0

-- Indent every line touched by the selection by one tab stop.
indentSelection :: Editor -> Editor
indentSelection ed0 = case getSelection ed0 of
  Nothing -> ed0
  Just (a, b) ->
    let ed1 = beginEdit EKOther ed0
        tw  = tabWidthOf ed1
        pad = if cfgTabsToSpaces (edConfig ed1) then T.replicate tw " " else "\t"
        shift = T.length pad
        ls  = [posLine a .. posLine b]
        buf' = foldl (\bf li -> fst (insertText (Pos li 0) pad bf)) (edBuffer ed1) ls
        adj (Pos l col) = Pos l (col + shift)
    in afterEdit ed1
         { edBuffer = buf'
         , edCursor = adj (edCursor ed1)
         , edSelAnchor = fmap adj (edSelAnchor ed1)
         }

backspace :: Editor -> Editor
backspace ed0
  | edReadOnly ed0 = ed0 { edStatus = "File is read-only" }
  | hasSelection ed0 = setDesired (afterEdit (removeSelection (beginEdit EKOther ed0)))
  | otherwise =
      let ed1 = beginEdit EKDelete ed0
          (buf', cur') = deleteBackward (edCursor ed1) (edBuffer ed1)
          ed2 = ed1 { edBuffer = buf', edCursor = cur' }
      in setDesired (afterEdit ed2)

deleteForwardOrSel :: Editor -> Editor
deleteForwardOrSel ed0
  | edReadOnly ed0 = ed0 { edStatus = "File is read-only" }
  | hasSelection ed0 = setDesired (afterEdit (removeSelection (beginEdit EKOther ed0)))
  | otherwise =
      let ed1 = beginEdit EKDelete ed0
          (buf', cur') = deleteForward (edCursor ed1) (edBuffer ed1)
          ed2 = ed1 { edBuffer = buf', edCursor = cur' }
      in setDesired (afterEdit ed2)

deleteWordLeft :: Editor -> Editor
deleteWordLeft ed0
  | edReadOnly ed0 = ed0
  | hasSelection ed0 = backspace ed0
  | otherwise =
      let ed1 = beginEdit EKOther ed0
          target = wordLeft (edCursor ed1) (edBuffer ed1)
          (buf', cur') = deleteRange target (edCursor ed1) (edBuffer ed1)
      in setDesired (afterEdit ed1 { edBuffer = buf', edCursor = cur' })

deleteWordRight :: Editor -> Editor
deleteWordRight ed0
  | edReadOnly ed0 = ed0
  | hasSelection ed0 = deleteForwardOrSel ed0
  | otherwise =
      let ed1 = beginEdit EKOther ed0
          target = wordRight (edCursor ed1) (edBuffer ed1)
          (buf', cur') = deleteRange (edCursor ed1) target (edBuffer ed1)
      in setDesired (afterEdit ed1 { edBuffer = buf', edCursor = cur' })

------------------------------------------------------------------------------
-- Clipboard actions

copy :: Editor -> (Editor, [Effect])
copy ed = case getSelection ed of
  Just (a, b) ->
    let txt = textInRange a b (edBuffer ed)
    in (ed { edClipboard = txt, edStatus = "Copied" }, [EffCopy txt])
  Nothing ->
    let l   = posLine (edCursor ed)
        txt = getLine' l (edBuffer ed) <> "\n"
    in (ed { edClipboard = txt, edStatus = "Copied line" }, [EffCopy txt])

cut :: Editor -> (Editor, [Effect])
cut ed0
  | edReadOnly ed0 = (ed0 { edStatus = "File is read-only" }, [])
  | otherwise = case getSelection ed0 of
      Just (a, b) ->
        let txt = textInRange a b (edBuffer ed0)
            ed1 = setDesired (afterEdit (removeSelection (beginEdit EKOther ed0)))
        in (ed1 { edClipboard = txt, edStatus = "Cut" }, [EffCopy txt])
      Nothing ->
        -- Cut the whole current line.
        let l   = posLine (edCursor ed0)
            txt = getLine' l (edBuffer ed0) <> "\n"
            ed1 = beginEdit EKOther ed0
            buf = edBuffer ed1
            ed2 = if lineCount buf == 1
                    then ed1 { edBuffer = emptyBuffer, edCursor = origin }
                    else let (b', c') = deleteRange (Pos l 0) (Pos (l + 1) 0) buf
                         in ed1 { edBuffer = b', edCursor = clampPos c' b' }
        in (setDesired (afterEdit ed2) { edClipboard = txt, edStatus = "Cut line" }, [EffCopy txt])

selectAll :: Editor -> Editor
selectAll ed =
  ensureVisible ed { edSelAnchor = Just origin
                   , edCursor = endPos (edBuffer ed)
                   , edLastEdit = EKNone }

------------------------------------------------------------------------------
-- Save-time fixups (trim trailing whitespace / ensure final newline)

-- | Apply the configured save-time cleanups to the active document, just
-- before its buffer is written: strip trailing whitespace (as an undoable
-- edit) and/or turn on the final newline. CSV tables and images are left
-- alone — whitespace can be data there.
applySaveFixups :: Editor -> Editor
applySaveFixups ed0 = ensureNl (trimStep ed0)
  where
    cfg = edConfig ed0
    plainDoc = isNothing (edCsv ed0) && isNothing (edImage ed0)
    trimStep ed
      | cfgTrimTrailingWs cfg && plainDoc && not (edReadOnly ed) =
          case trimTrailingWs (edBuffer ed) of
            Nothing -> ed
            Just buf' ->
              let edU = beginEdit EKOther ed
              in setDesired (afterEdit edU
                   { edBuffer = buf'
                   , edCursor = clampPos (edCursor edU) buf'
                   , edSelAnchor = fmap (`clampPos` buf') (edSelAnchor edU) })
      | otherwise = ed
    ensureNl ed
      | cfgEnsureFinalNl cfg && plainDoc && not (edFinalNewline ed) =
          ed { edFinalNewline = True }
      | otherwise = ed

-- | The Save All variant: fix up the active document and every modified,
-- writable plain-text document in the zipper (those are the ones a Save All
-- writes), each with its own undo checkpoint.
applySaveFixupsAll :: Editor -> Editor
applySaveFixupsAll ed0 =
  let ed1 = applySaveFixups ed0
      cfg = edConfig ed0
      fixDoc d
        | not (docModified d) || isJust (docCsv d) || isJust (docImage d) || docReadOnly d = d
        | otherwise =
            let d1 = case trimTrailingWs (docBuffer d) of
                       Just b' | cfgTrimTrailingWs cfg ->
                         d { docBuffer = b'
                           , docCursor = clampPos (docCursor d) b'
                           , docSelAnchor = fmap (`clampPos` b') (docSelAnchor d)
                           , docUndo = take maxUndo
                               (UndoState (docBuffer d) (docCursor d) (docSelAnchor d) : docUndo d)
                           , docRedo = [], docLastEdit = EKNone }
                       _ -> d
            in if cfgEnsureFinalNl cfg && not (docFinalNewline d1)
                 then d1 { docFinalNewline = True }
                 else d1
  in ed1 { edBefore = map fixDoc (edBefore ed1), edAfter = map fixDoc (edAfter ed1) }

------------------------------------------------------------------------------
-- File properties (line ending / BOM) and the interactive status bar

eolName :: LineEnding -> String
eolName LF = "LF"; eolName CRLF = "CRLF"; eolName CR = "CR"

-- Recompute the modified flag after a metadata (EOL/BOM) change, respecting
-- whichever model is live (the CSV grid or the text buffer).
recomputeModified :: Editor -> Editor
recomputeModified ed = case edCsv ed of
  Just v  -> ed { edModified = Csv.isModified v || metaModified ed }
  Nothing -> ed { edModified = metaModified ed || bufModified ed (edBuffer ed) }

-- | Switch the line ending the file will be saved with (LF ⇄ CRLF; legacy CR
-- files convert to LF). Applies on the next save.
cycleLineEnding :: Editor -> Editor
cycleLineEnding ed
  | isJust (edImage ed) = ed { edStatus = "Not available in image view" }
  | edReadOnly ed       = ed { edStatus = "File is read-only" }
  | otherwise =
      let new = case edLineEnding ed of LF -> CRLF; _ -> LF
          ed1 = recomputeModified ed { edLineEnding = new }
      in ed1 { edStatus = T.pack ("Line endings: " ++ eolName new ++ " (written on save)") }

-- | Toggle the UTF-8 byte-order mark the file will be saved with.
toggleBom :: Editor -> Editor
toggleBom ed
  | isJust (edImage ed) = ed { edStatus = "Not available in image view" }
  | edReadOnly ed       = ed { edStatus = "File is read-only" }
  | otherwise =
      let new = case edEncoding ed of Utf8 -> Utf8Bom; Utf8Bom -> Utf8
          ed1 = recomputeModified ed { edEncoding = new }
      in ed1 { edStatus = if new == Utf8Bom then "UTF-8 BOM will be written on save"
                                            else "UTF-8 BOM will be removed on save" }

-- | View ▸ Theme: switch the colour palette for this session (the config
-- file's @theme =@ key sets the startup default). From @auto@ the toggle
-- flips away from whatever the detection resolved to; explicit dark/light
-- then toggle between each other (only the config returns you to auto).
toggleTheme :: Editor -> Editor
toggleTheme ed =
  let cfg = edConfig ed
      new = case resolvedTheme ed of ThemeDark -> ThemeLight; _ -> ThemeDark
  in ed { edConfig = cfg { cfgTheme = new }
        , edStatus = T.pack ("Theme: " ++ themeLabel new
                             ++ " (set theme = " ++ themeLabel new ++ " in the config to keep it)") }

themeLabel :: ThemeName -> String
themeLabel ThemeDark = "dark"; themeLabel ThemeLight = "light"; themeLabel ThemeAuto = "auto"

-- | The clickable regions of the status bar's right side.
data StatusZone = SZGoTo | SZOverwrite | SZEncoding | SZLineEnding
  deriving (Eq, Show)

-- | The status bar's right-hand text and its clickable zones as
-- @(startCol, length, zone)@ offsets *within that text* (it is drawn
-- right-aligned). One builder shared by the renderer and mouse hit-testing so
-- they can never disagree.
statusRightInfo :: Editor -> (String, [(Int, Int, StatusZone)])
statusRightInfo ed = flatten segs
  where
    Pos l c = edCursor ed
    selInfo = case getSelection ed of
      Just (a, b) -> "  Sel " ++ show (T.length (textInRange a b (edBuffer ed)))
      Nothing -> ""
    enc = case edEncoding ed of Utf8 -> "UTF-8"; Utf8Bom -> "UTF-8-BOM"
    eol = eolName (edLineEnding ed)
    ovr = if edOverwrite ed then "OVR" else "INS"
    plain s = (s, Nothing)
    zone z s = (s, Just z)
    segs = case edImage ed of
      Just idoc ->
        let img = idImage idoc
            m   = case idMode idoc of HalfBlock -> "colour"; Ascii -> "ASCII"
        in [ plain (imgFmt img ++ "  " ++ show (imgW img) ++ "\xd7" ++ show (imgH img)
                    ++ "   IMAGE/" ++ m ++ " ") ]
      Nothing -> case edCsv ed of
        Just v ->
          let (r0, c0, r1, c1) = Csv.selRect v
              cellRef = "Cell " ++ Csv.colLabel (csvCurCol v) ++ show (csvCurRow v + 1)
              selRef  = if Csv.hasSelection v
                          then cellRef ++ "  " ++ show (r1 - r0 + 1)
                                 ++ "\xd7" ++ show (c1 - c0 + 1) ++ " sel"
                          else cellRef
          in [ plain (selRef ++ "   " ++ show (Csv.nRows v) ++ " rows x "
                      ++ show (Csv.nCols v) ++ " cols   TABLE  ")
             , zone SZLineEnding eol, plain " " ]
        Nothing ->
          [ zone SZGoTo ("Ln " ++ show (l + 1) ++ ", Col " ++ show (c + 1))
          , plain selInfo, plain "   "
          , zone SZOverwrite ovr, plain "  "
          , zone SZEncoding enc, plain "  "
          , zone SZLineEnding eol, plain " " ]
    flatten = go 0 [] []
      where
        go _ accS accZ [] = (concat (reverse accS), reverse accZ)
        go col accS accZ ((s, mz) : rest) =
          let len = length s
              accZ' = case mz of Just z -> (col, len, z) : accZ; Nothing -> accZ
          in go (col + len) (s : accS) accZ' rest

-- A left press on the status bar row (mirrors 'menuBarPress').
statusBarPress :: Editor -> MouseEvent -> Bool
statusBarPress ed me =
  edShowStatus ed && mePressed me && not (meDrag me) && meButton me == MBLeft
    && meRow me == loStatusRow (computeLayout ed)

-- Dispatch a click on one of the status bar's zones.
statusClick :: Int -> Editor -> (Editor, [Effect])
statusClick col ed =
  let lo = computeLayout ed
      (txt, zones) = statusRightInfo ed
      rel = col - max 0 (loCols lo - length txt)
  in case [ z | (s, len, z) <- zones, rel >= s, rel < s + len ] of
       (SZGoTo : _)       -> noEff (openGoTo ed)
       (SZOverwrite : _)  -> noEff ed { edOverwrite = not (edOverwrite ed)
                                      , edStatus = if edOverwrite ed then "Insert mode" else "Overwrite mode" }
       (SZEncoding : _)   -> noEff (toggleBom ed)
       (SZLineEnding : _) -> noEff (cycleLineEnding ed)
       _                  -> noEff ed

openGoTo :: Editor -> Editor
openGoTo ed = openDialog mkGoToLine ed

-- | The bracket pair to highlight this frame: the bracket at/before the cursor
-- and its partner. Empty outside plain-text view or when nothing matches.
bracketPair :: Editor -> [Pos]
bracketPair ed
  | isJust (edCsv ed) || isJust (edImage ed) = []
  | otherwise = case matchBracket (edCursor ed) (edBuffer ed) of
      Just (a, b) -> [a, b]
      Nothing     -> []

-- | Ctrl+] / Find ▸ Go to Bracket: jump to the matching bracket.
gotoBracket :: Editor -> Editor
gotoBracket ed
  | isJust (edImage ed) = ed { edStatus = "Not available in image view" }
  | isJust (edCsv ed)   = ed { edStatus = "Bracket jump works in text view (Alt+T to switch)" }
  | otherwise = case matchBracket (edCursor ed) (edBuffer ed) of
      Just (_, p) -> moveHoriz False (const p) (pushNavIfFar (edPath ed) p ed)
      Nothing     -> ed { edStatus = "No matching bracket here" }

-- Home toggles between first non-blank and column 0 (a common nicety).
smartHome :: Editor -> Pos
smartHome ed =
  let Pos l c = edCursor ed
      line = getLine' l (edBuffer ed)
      firstNonBlank = T.length (T.takeWhile isSpace line)
  in if c == firstNonBlank then Pos l 0 else Pos l firstNonBlank

scrollLine :: Int -> Editor -> Editor
scrollLine delta ed =
  let nLines = lineCount (edBuffer ed)
      top' = max 0 (min (nLines - 1) (edTop ed + delta))
      th = loTextHeight (computeLayout ed)
      Pos l c = edCursor ed
      l' | l < top'           = top'
         | l >= top' + th     = top' + th - 1
         | otherwise          = l
  in ed { edTop = top', edCursor = clampPos (Pos l' c) (edBuffer ed) }

-- | Pan the view horizontally (Shift+wheel / a horizontal wheel), pulling the
-- cursor along so it stays on screen — the horizontal twin of 'scrollLine'.
-- A no-op under word wrap, where there is no horizontal scroll.
scrollCol :: Int -> Editor -> Editor
scrollCol delta ed
  | edWordWrap ed = ed
  | otherwise =
      let left' = max 0 (edLeft ed + delta)
          tw = loTextWidth (computeLayout ed)
          tabw = tabWidthOf ed
          Pos l c = edCursor ed
          line = getLine' l (edBuffer ed)
          dcol = colToDisplay tabw c line
          dcol' | dcol < left'           = left'
                | dcol >= left' + tw     = left' + tw - 1
                | otherwise              = dcol
          c' = displayToCol tabw dcol' line
          ed1 = ed { edLeft = left', edCursor = clampPos (Pos l c') (edBuffer ed) }
      in ed1 { edDesiredCol = curDisplayCol ed1 }

outdentSelection :: Editor -> Editor
outdentSelection ed0 =
  let tw = tabWidthOf ed0
      (a, b) = fromMaybe (edCursor ed0, edCursor ed0) (getSelection ed0)
      ls = [posLine a .. posLine b]
      ed1 = beginEdit EKOther ed0
      stripOne li bf =
        let line = getLine' li bf
            ws   = T.length (T.takeWhile (== ' ') (T.take tw line))
            n    = if not (T.null line) && T.head line == '\t' then 1 else ws
        in fst (deleteRange (Pos li 0) (Pos li n) bf)
      buf' = foldl (flip stripOne) (edBuffer ed1) ls
  in afterEdit ed1 { edBuffer = buf'
                   , edCursor = clampPos (edCursor ed1) buf'
                   , edSelAnchor = fmap (`clampPos` buf') (edSelAnchor ed1) }

------------------------------------------------------------------------------
-- Line operations (duplicate / move / delete / join)

-- Why these are refused outside the plain-text view (the table's grid — not
-- 'edBuffer' — is the live model there; images are read-only).
lineOpBlocked :: Editor -> Maybe Text
lineOpBlocked ed
  | isJust (edImage ed) = Just "Not available in image view"
  | isJust (edCsv ed)   = Just "Line operations work in text view (Alt+T to switch)"
  | edReadOnly ed       = Just "File is read-only"
  | otherwise           = Nothing

-- The inclusive range of buffer lines the selection touches (or the cursor's
-- line). A selection ending at column 0 does not include that final line, so
-- selecting two whole lines (Shift+Down twice, or triple-click) operates on
-- exactly those two.
selLineSpan :: Editor -> (Int, Int)
selLineSpan ed = case getSelection ed of
  Just (a, b) | posLine b > posLine a && posCol b == 0 -> (posLine a, posLine b - 1)
              | otherwise                              -> (posLine a, posLine b)
  Nothing -> let l = posLine (edCursor ed) in (l, l)

-- | Duplicate the selected lines (or the cursor line) below themselves. With
-- @down@ the cursor (and selection) move onto the new lower copy; without, they
-- stay on the upper one — matching Copy Line Down / Up.
duplicateLinesDir :: Bool -> Editor -> Editor
duplicateLinesDir down ed0 = case lineOpBlocked ed0 of
  Just msg -> ed0 { edStatus = msg }
  Nothing ->
    let ed1 = beginEdit EKOther ed0
        (a, b) = selLineSpan ed1
        buf = edBuffer ed1
        block = T.intercalate "\n" [ getLine' i buf | i <- [a .. b] ]
        buf' = fst (insertText (Pos b (lineLen b buf)) (T.cons '\n' block) buf)
        n = b - a + 1
        shift (Pos l c) = if down then Pos (l + n) c else Pos l c
    in setDesired (afterEdit ed1 { edBuffer = buf'
                                 , edCursor = shift (edCursor ed1)
                                 , edSelAnchor = fmap shift (edSelAnchor ed1) })

-- | Move the selected lines (or the cursor line) one line up (@dir@ < 0) or
-- down, carrying the cursor and selection with the block. A no-op (and no undo
-- checkpoint) at the top/bottom of the file.
moveLines :: Int -> Editor -> Editor
moveLines dir ed0 = case lineOpBlocked ed0 of
  Just msg -> ed0 { edStatus = msg }
  Nothing ->
    let (a, b) = selLineSpan ed0
        buf0 = edBuffer ed0
    in if (dir < 0 && a == 0) || (dir > 0 && b >= lineCount buf0 - 1)
         then ed0
         else
           let ed1 = beginEdit EKMoveLine ed0
               buf = edBuffer ed1
               shift (Pos l c) = Pos (l + signum dir) c
               buf' =
                 if dir > 0
                   -- Pull the line below the block out and drop it above.
                   then let below = getLine' (b + 1) buf
                            bufA  = fst (deleteRange (Pos b (lineLen b buf))
                                                     (Pos (b + 1) (lineLen (b + 1) buf)) buf)
                        in fst (insertText (Pos a 0) (below <> "\n") bufA)
                   -- Pull the line above the block out and drop it below.
                   else let above = getLine' (a - 1) buf
                            bufA  = fst (deleteRange (Pos (a - 1) 0) (Pos a 0) buf)
                        in fst (insertText (Pos (b - 1) (lineLen (b - 1) bufA))
                                           (T.cons '\n' above) bufA)
           in setDesired (afterEdit ed1 { edBuffer = buf'
                                        , edCursor = shift (edCursor ed1)
                                        , edSelAnchor = fmap shift (edSelAnchor ed1) })

-- | Delete the selected lines (or the cursor line) outright (no clipboard —
-- Ctrl+K cuts). The cursor keeps its column on the line that moves up.
deleteLines :: Editor -> Editor
deleteLines ed0 = case lineOpBlocked ed0 of
  Just msg -> ed0 { edStatus = msg }
  Nothing ->
    let ed1 = beginEdit EKOther ed0
        (a, b) = selLineSpan ed1
        buf = edBuffer ed1
        n = lineCount buf
        col = posCol (edCursor ed1)
        buf' | b < n - 1 = fst (deleteRange (Pos a 0) (Pos (b + 1) 0) buf)
             | a > 0     = fst (deleteRange (Pos (a - 1) (lineLen (a - 1) buf))
                                            (Pos b (lineLen b buf)) buf)
             | otherwise = emptyBuffer
        cur = clampPos (Pos a col) buf'
    in setDesired (afterEdit ed1 { edBuffer = buf', edCursor = cur, edSelAnchor = Nothing })

-- | Join the selected lines into one (or the cursor line with the next):
-- trailing/leading whitespace at each seam collapses to a single space, and the
-- cursor lands on the (last) seam.
joinLines :: Editor -> Editor
joinLines ed0 = case lineOpBlocked ed0 of
  Just msg -> ed0 { edStatus = msg }
  Nothing ->
    let (a, b) = selLineSpan ed0
        lastL = lineCount (edBuffer ed0) - 1
        nJoins = min (max 1 (b - a)) (lastL - a)
    in if a >= lastL
         then ed0
         else
           let ed1 = beginEdit EKOther ed0
               go 0 buf cur = (buf, cur)
               go k buf _   = let (buf', cur') = joinOnce a buf in go (k - 1 :: Int) buf' cur'
               (buf2, cur2) = go nJoins (edBuffer ed1) (edCursor ed1)
           in setDesired (afterEdit ed1 { edBuffer = buf2, edCursor = cur2, edSelAnchor = Nothing })

-- Join line l with line l+1, collapsing the seam whitespace to one space
-- (none when either side is empty). Returns the buffer and the seam position.
joinOnce :: Int -> Buffer -> (Buffer, Pos)
joinOnce l buf =
  let cur  = getLine' l buf
      nxt  = getLine' (l + 1) buf
      curR = T.stripEnd cur
      nxtL = T.stripStart nxt
      sep  = if T.null curR || T.null nxtL then "" else " "
      buf1 = fst (deleteRange (Pos l 0) (Pos (l + 1) (T.length nxt)) buf)
      buf2 = fst (insertText (Pos l 0) (curR <> sep <> nxtL) buf1)
  in (buf2, Pos l (T.length curR + T.length sep))

------------------------------------------------------------------------------
-- Toggle comment (Ctrl+/)

-- | Comment or uncomment the selected lines (or the cursor line), using the
-- active file's language syntax. Line-comment languages toggle per line;
-- block-only languages (HTML, CSS…) wrap/unwrap the whole span.
toggleComment :: Editor -> Editor
toggleComment ed0 = case lineOpBlocked ed0 of
  Just msg -> ed0 { edStatus = msg }
  Nothing -> case langForPath (edPath ed0) >>= langComment of
    Nothing -> ed0 { edStatus = "No comment syntax known for this file type" }
    Just (LineComment pfx)    -> toggleLineComments pfx ed0
    Just (BlockComment op cl) -> toggleBlockComment op cl ed0

-- Line comments: if every non-blank line in the span is already commented,
-- uncomment them all; otherwise comment them, aligned at the span's minimum
-- indentation (blank lines are skipped, unless the span is a single line).
toggleLineComments :: Text -> Editor -> Editor
toggleLineComments pfx ed0 =
  let (a, b) = selLineSpan ed0
      lns = [ (i, getLine' i (edBuffer ed0)) | i <- [a .. b] ]
      isBlank = T.null . T.strip
      targets | a == b    = lns
              | otherwise = filter (not . isBlank . snd) lns
      commented t = pfx `T.isPrefixOf` T.stripStart t
  in case targets of
       [] -> ed0
       _ | all (commented . snd) targets && not (all (isBlank . snd) targets) ->
             applyLineEdit (uncommentLine pfx) targets ed0
         | otherwise ->
             let col = minimum [ T.length (T.takeWhile isSpace t) | (_, t) <- targets ]
             in applyLineEdit (commentLine pfx col) targets ed0

-- Rewrite whole lines through an undoable edit; @f@ returns the new line text
-- plus a column-adjustment so the cursor/selection stay on their characters.
applyLineEdit :: (Text -> (Text, Int -> Int)) -> [(Int, Text)] -> Editor -> Editor
applyLineEdit f targets ed0 =
  let ed1 = beginEdit EKOther ed0
      step (buf, adjs) (i, old) =
        let (new, adj) = f old
            buf1 = fst (deleteRange (Pos i 0) (Pos i (T.length old)) buf)
            buf2 = fst (insertText (Pos i 0) new buf1)
        in (buf2, (i, adj) : adjs)
      (buf', adjs) = foldl step (edBuffer ed1, []) targets
      fix p@(Pos l c) = case lookup l adjs of
        Just adj -> clampPos (Pos l (max 0 (adj c))) buf'
        Nothing  -> clampPos p buf'
  in setDesired (afterEdit ed1 { edBuffer = buf'
                               , edCursor = fix (edCursor ed1)
                               , edSelAnchor = fmap fix (edSelAnchor ed1) })

commentLine :: Text -> Int -> Text -> (Text, Int -> Int)
commentLine pfx col t =
  let ins = pfx <> " "
      at = min col (T.length t)
  in ( T.take at t <> ins <> T.drop at t
     , \c -> if c >= at then c + T.length ins else c )

uncommentLine :: Text -> Text -> (Text, Int -> Int)
uncommentLine pfx t =
  let ind = T.length (T.takeWhile isSpace t)
      rest = T.drop ind t
  in if pfx `T.isPrefixOf` rest
       then let afterP = T.drop (T.length pfx) rest
                extra = if T.take 1 afterP == " " then 1 else 0
                n = T.length pfx + extra
            in ( T.take ind t <> T.drop n rest
               , \c -> if c > ind then max ind (c - n) else c )
       else (t, id)

-- Block comments: wrap the span (open before the first line's content, close
-- at the end of the last line), or unwrap it when already wrapped.
toggleBlockComment :: Text -> Text -> Editor -> Editor
toggleBlockComment op cl ed0 =
  let (a, b) = selLineSpan ed0
      buf0 = edBuffer ed0
      firstT = getLine' a buf0
      lastT = getLine' b buf0
      indent = T.length (T.takeWhile isSpace firstT)
      wrapped = op `T.isPrefixOf` T.stripStart firstT && cl `T.isSuffixOf` T.stripEnd lastT
      ed1 = beginEdit EKOther ed0
      buf = edBuffer ed1
      buf' | wrapped =
               let restA = T.drop indent (getLine' a buf)
                   afterOp = T.drop (T.length op) restA
                   extraA = if T.take 1 afterOp == " " then 1 else 0
                   buf1 = fst (deleteRange (Pos a indent)
                                           (Pos a (indent + T.length op + extraA)) buf)
                   lineB = getLine' b buf1
                   endC = T.length (T.stripEnd lineB)
                   startC = endC - T.length cl
                   extraB = if startC > 0 && T.index lineB (startC - 1) == ' ' then 1 else 0
               in fst (deleteRange (Pos b (startC - extraB)) (Pos b endC) buf1)
           | otherwise =
               let buf1 = fst (insertText (Pos b (lineLen b buf)) (" " <> cl) buf)
               in fst (insertText (Pos a indent) (op <> " ") buf1)
  in setDesired (afterEdit ed1 { edBuffer = buf'
                               , edCursor = clampPos (edCursor ed1) buf'
                               , edSelAnchor = fmap (`clampPos` buf') (edSelAnchor ed1) })

------------------------------------------------------------------------------
-- Word completion (Ctrl+Space)

-- Caps that keep a completion press cheap on huge buffers: lines scanned
-- across all open documents, and candidates kept.
maxCompleteLines :: Int
maxCompleteLines = 100000

maxCompleteItems :: Int
maxCompleteItems = 100

completeVisRows :: Int
completeVisRows = 8

-- | Ctrl+Space: complete the identifier prefix before the cursor from the
-- words of every open buffer, nearest occurrences first. A single candidate
-- inserts immediately; several open the popup.
startComplete :: Editor -> Editor
startComplete ed
  | isJust (edCsv ed) || isJust (edImage ed) = ed { edStatus = "Completion works in text view" }
  | edReadOnly ed = ed { edStatus = "File is read-only" }
  | otherwise =
      let Pos l c = edCursor ed
          line = getLine' l (edBuffer ed)
          pre = T.take c line
          plen = T.length (T.takeWhile isWordCh (T.reverse pre))
          start = Pos l (c - plen)
          prefix = T.drop (c - plen) pre
          items = collectCandidates prefix ed
      in case items of
           []  -> ed { edStatus = if T.null prefix then "No words to complete"
                                  else "No completions for \x2018" <> prefix <> "\x2019" }
           [w] -> acceptWord start w ed
           _   -> ed { edComplete = Just (Complete prefix start items 0 0), edStatus = "" }

-- Candidate words with the prefix, from the active buffer (scanning outward
-- from the cursor, so nearby words rank first) then the other open documents.
collectCandidates :: Text -> Editor -> [Text]
collectCandidates prefix ed = go maxCompleteLines Set.empty srcLines []
  where
    buf = edBuffer ed
    Pos cl _ = edCursor ed
    n = lineCount buf
    activeLines = [ getLine' li buf
                  | li <- cl : concat [ [cl - k, cl + k] | k <- [1 .. n] ]
                  , li >= 0, li < n ]
    otherLines = [ ln | d <- edBefore ed ++ edAfter ed, isPlainDoc d
                      , ln <- toList (bufLines (docBuffer d)) ]
    srcLines = activeLines ++ otherLines
    keep w = not (T.null w)
             && (T.null prefix || prefix `T.isPrefixOf` w)
             && w /= prefix
             && not (isDigit (T.head w))
    go _ _ [] acc = reverse acc
    go budget seen (ln : rest) acc
      | budget <= 0 || length acc >= maxCompleteItems = reverse acc
      | otherwise =
          let ws = [ w | w <- T.split (not . isWordCh) ln, keep w, not (Set.member w seen) ]
              fresh = nubQuick ws
              seen' = foldr Set.insert seen fresh
          in go (budget - 1) seen' rest (reverse fresh ++ acc)
    nubQuick = foldr (\w acc -> if w `elem` acc then acc else w : acc) []

-- Replace [start .. cursor) with the chosen word (one undo step).
acceptWord :: Pos -> Text -> Editor -> Editor
acceptWord start w ed0 =
  let ed1 = beginEdit EKOther ed0
      buf1 = fst (deleteRange start (edCursor ed1) (edBuffer ed1))
      (buf2, cur2) = insertText start w buf1
  in setDesired (afterEdit ed1 { edBuffer = buf2, edCursor = cur2
                               , edSelAnchor = Nothing, edComplete = Nothing })

closeComplete :: Editor -> Editor
closeComplete ed = ed { edComplete = Nothing }

-- Narrow (or dismiss) the popup after the prefix changed by typing/backspace.
renarrowComplete :: Editor -> Editor
renarrowComplete ed = case edComplete ed of
  Nothing -> ed
  Just cp ->
    let Pos l c = edCursor ed
        startC = posCol (cpStart cp)
    in if posLine (edCursor ed) /= posLine (cpStart cp) || c < startC
         then closeComplete ed
         else
           let prefix = T.take (c - startC) (T.drop startC (getLine' l (edBuffer ed)))
               items = filter (\w -> prefix `T.isPrefixOf` w && w /= prefix) (cpItems cp)
           in if null items
                then closeComplete ed
                else ed { edComplete = Just cp { cpPrefix = prefix, cpItems = items
                                               , cpSel = 0, cpTop = 0 } }

-- | Popup geometry @(top, left, height, width)@, anchored under the prefix
-- start (above it when there's no room below). Shared with mouse hit-testing.
completeGeom :: Editor -> Complete -> (Int, Int, Int, Int)
completeGeom ed cp =
  let lo = computeLayout ed
      Pos l s = cpStart cp
      line = getLine' l (edBuffer ed)
      tabw = tabWidthOf ed
      (row, colBase)
        | edWordWrap ed =
            let vrow = visualOffset ed (edTop ed) (cpStart cp)
                segs = lineSegs ed l
                (segS, _) = segs !! segIndexOf segs s
            in ( loTextTop lo + vrow
               , loTextLeft lo + colToDisplay tabw s line - colToDisplay tabw segS line )
        | otherwise =
            ( loTextTop lo + (l - edTop ed)
            , loTextLeft lo + colToDisplay tabw s line - edLeft ed )
      h = min completeVisRows (length (cpItems cp))
      w = min (max 12 (2 + maximum (1 : map T.length (take 50 (cpItems cp)))))
              (max 12 (loCols lo - 2))
      below = row + 1
      top | below + h <= loTextTop lo + loTextHeight lo = below
          | otherwise = max (loTextTop lo) (row - h)
      left = max 0 (min colBase (loCols lo - w))
  in (top, left, h, w)
