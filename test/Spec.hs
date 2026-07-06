-- | Hand-rolled test suite (no external test framework is available offline).
-- Exercises the pure core: text buffer, width mapping, input parsing and the
-- editor update function.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Bits (shiftR, (.&.))
import Data.IORef
import Data.Word (Word8)
import Data.Either (isLeft)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, tails)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Array (bounds)
import qualified Data.Array as A
import Data.Array.Unboxed ((!))

import Cmedit.Types
import Cmedit.Link (filePathUri, urlSpans, linkIdOf)
import Cmedit.About (aboutCanvasH, aboutTotalFrames, aboutFrameCells)
import Cmedit.HelpCard (helpCanvasH, helpCanvasMinW, helpFrameCells)
import Cmedit.Manual (manualPath)
import Cmedit.TextBuffer
import Cmedit.Width
import Cmedit.Input
import Cmedit.Editor
import Cmedit.ConfigFile
  ( parseConfigText, RecentEntry(..), parseRecentText, renderRecentText
  , parseHistoryText, renderHistoryText )
import Cmedit.QuickOpen (QuickOpen(..))
import qualified Cmedit.QuickOpen as Q
import Cmedit.Menu (MenuAction(..), MenuEntry(..), MenuState(..))
import Cmedit.Dialog (fieldValue, Field(..), Dialog(..), DialogKind(..), mkFind, fieldSetCursorLineCol, focusedButton)
import Cmedit.Browser (Browser(..), FileNode(..))
import qualified Cmedit.Browser as Br
import Cmedit.Search (SearchState(..), SField(..), SearchField(..), FileResult(..), Match(..), SRow(..))
import qualified Cmedit.Search as S
import Cmedit.Definition (DefLang(..), DefPick(..), DefItem(..), DefReq(..))
import qualified Cmedit.Definition as D
import qualified Cmedit.Regex as Rx
import qualified Data.Sequence as Seq
import Cmedit.Csv
import Cmedit.Image (Image(..), ImgMode(..), decodeImage, decodeFrames, decodeGIFFrames, sniffImage, renderImage, viewFit, scaleRGBA)
import Cmedit.Render (renderEditor, renderFrame, scrollPlan, Screen(..), ScrollHint(..), Theme(..), defaultTheme, lightTheme, themeFor, FileKind(..), fileKind)
import Cmedit.ConfigFile (ThemeName(..), defaultConfig)
import Cmedit.Ansi (styleSgr, styleSgrWith)
import Cmedit.Caps
import Cmedit.Gfx (base64B, sixelEncode, gfxFit, kittyPlace)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Char (isAlphaNum, isDigit, isSpace, toLower)
import Cmedit.Syntax (Lang(..), Tok(..), HlState(..), langForPath, initialState, lexLine,
                      refreshHlCache, hlStateBefore, hlCoverage)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- Build an ordered 'DiskTime' from a small integer, for the stale-file tests.
mt :: Int -> DiskTime
mt = posixSecondsToUTCTime . fromIntegral

main :: IO ()
main = do
  results <- newIORef (0 :: Int, 0 :: Int)
  let check name cond = do
        (p, f) <- readIORef results
        if cond then writeIORef results (p + 1, f)
                else do putStrLn ("FAIL: " ++ name); writeIORef results (p, f + 1)
      checkEq name a b = check (name ++ " (got " ++ show a ++ ")") (a == b)

  -- TextBuffer ---------------------------------------------------------------
  let b0 = fromText (T.pack "hello\nworld")
  checkEq "lineCount" (lineCount b0) 2
  checkEq "getLine0" (getLine' 0 b0) (T.pack "hello")
  checkEq "roundtrip" (bufferToText LF False b0) (T.pack "hello\nworld")

  let (b1, p1) = insertChar (Pos 0 5) '!' b0
  checkEq "insertChar buf" (getLine' 0 b1) (T.pack "hello!")
  checkEq "insertChar pos" p1 (Pos 0 6)

  let (b2, p2) = insertText (Pos 0 0) (T.pack "AB\nCD") b0
  checkEq "insertText lines" (lineCount b2) 3
  checkEq "insertText l0" (getLine' 0 b2) (T.pack "AB")
  checkEq "insertText l1" (getLine' 1 b2) (T.pack "CDhello")
  checkEq "insertText pos" p2 (Pos 1 2)

  let (b3, p3) = splitLineAt (Pos 0 2) b0
  checkEq "split l0" (getLine' 0 b3) (T.pack "he")
  checkEq "split l1" (getLine' 1 b3) (T.pack "llo")
  checkEq "split pos" p3 (Pos 1 0)

  let (b4, p4) = deleteBackward (Pos 1 0) b0   -- join "world" onto "hello"
  checkEq "joinback" (getLine' 0 b4) (T.pack "helloworld")
  checkEq "joinback pos" p4 (Pos 0 5)

  let (b5, _) = deleteRange (Pos 0 2) (Pos 1 2) b0
  checkEq "deleteRange" (getLine' 0 b5) (T.pack "herld")
  checkEq "deleteRange count" (lineCount b5) 1

  checkEq "textInRange" (textInRange (Pos 0 2) (Pos 1 2) b0) (T.pack "llo\nwo")

  checkEq "wordRight" (wordRight (Pos 0 0) (fromText (T.pack "foo bar"))) (Pos 0 4)
  checkEq "wordLeft" (wordLeft (Pos 0 7) (fromText (T.pack "foo bar"))) (Pos 0 4)

  -- Double-click word range / triple-click line range.
  let wb = fromText (T.pack "hello world foo")
  checkEq "wordRangeAt mid-word" (wordRangeAt (Pos 0 8) wb) (Pos 0 6, Pos 0 11)  -- 'r' in "world"
  checkEq "wordRangeAt on space" (wordRangeAt (Pos 0 5) wb) (Pos 0 5, Pos 0 6)
  let lb = fromText (T.pack "aa\nbb\ncc")
  checkEq "lineRangeAt middle" (lineRangeAt (Pos 1 1) lb) (Pos 1 0, Pos 2 0)     -- includes newline
  checkEq "lineRangeAt last"   (lineRangeAt (Pos 2 0) lb) (Pos 2 0, Pos 2 2)

  checkEq "detect CRLF" (detectLineEnding (T.pack "a\r\nb")) CRLF
  checkEq "detect LF" (detectLineEnding (T.pack "a\nb")) LF

  -- Buffer character count: kept incrementally by every edit --------------------
  let countOf b = sum (map T.length [ getLine' i b | i <- [0 .. lineCount b - 1] ])
      charsOk name b = checkEq ("bufChars " ++ name) (bufChars b) (countOf b)
  charsOk "fromText" b0
  charsOk "empty" emptyBuffer
  charsOk "insertChar" b1
  charsOk "insertText multi" b2
  charsOk "splitLineAt" b3
  charsOk "join backward" b4
  charsOk "deleteRange" b5
  charsOk "overwrite" (fst (overwriteChar (Pos 0 1) 'X' b0))
  charsOk "overwrite at eol" (fst (overwriteChar (Pos 0 5) 'X' b0))
  charsOk "deleteForward join" (fst (deleteForward (Pos 0 5) b0))
  charsOk "insertText single" (fst (insertText (Pos 1 2) (T.pack "zz") b0))
  charsOk "deleteRange multiline"
          (fst (deleteRange (Pos 0 1) (Pos 2 1) (fromText (T.pack "abc\ndef\nghi"))))

  -- Width --------------------------------------------------------------------
  checkEq "tab display" (colToDisplay 4 1 (T.pack "\tx")) 4
  checkEq "ascii display" (colToDisplay 4 3 (T.pack "abc")) 3
  checkEq "wide width" (charWidth '\x4e00') 2     -- CJK
  checkEq "combining width" (charWidth '\x0301') 0
  checkEq "displayToCol tab" (displayToCol 4 4 (T.pack "\tx")) 1

  -- Input parser -------------------------------------------------------------
  kUp <- parseBytes [0x1b, 0x5b, 0x41]
  checkEq "arrow up" kUp (KArrow DUp noMods)
  kCtrlA <- parseBytes [0x01]
  checkEq "ctrl-a" kCtrlA (KCtrlChar 'a')
  kCtrlRight <- parseBytes [0x1b, 0x5b, 0x31, 0x3b, 0x35, 0x43]
  checkEq "ctrl-right" kCtrlRight (KArrow DRight (Mods False False True))
  kShiftUp <- parseBytes [0x1b, 0x5b, 0x31, 0x3b, 0x32, 0x41]
  checkEq "shift-up" kShiftUp (KArrow DUp (Mods True False False))
  kEsc <- parseBytes [0x1b]
  checkEq "bare esc" kEsc KEsc
  kEnter <- parseBytes [0x0d]
  checkEq "enter" kEnter KEnter
  kDel <- parseBytes [0x1b, 0x5b, 0x33, 0x7e]
  checkEq "delete" kDel (KDelete noMods)
  kHome <- parseBytes [0x1b, 0x5b, 0x48]
  checkEq "home" kHome (KHome noMods)

  -- Shift / Ctrl modified Home/End/PageUp/PageDown in their common encodings.
  let bytesOf = map (fromIntegral . fromEnum)
      shiftMods = Mods True False False
      ctrlShift = Mods True False True
  kSHomeA <- parseBytes (bytesOf "\ESC[1;2H")
  checkEq "shift-home 1;2H" kSHomeA (KHome shiftMods)
  kSEndA <- parseBytes (bytesOf "\ESC[1;2F")
  checkEq "shift-end 1;2F" kSEndA (KEnd shiftMods)
  kSHomeB <- parseBytes (bytesOf "\ESC[7;2~")
  checkEq "shift-home 7;2~" kSHomeB (KHome shiftMods)
  kSEndB <- parseBytes (bytesOf "\ESC[8;2~")
  checkEq "shift-end 8;2~" kSEndB (KEnd shiftMods)
  kSPgUp <- parseBytes (bytesOf "\ESC[5;2~")
  checkEq "shift-pageup 5;2~" kSPgUp (KPageUp shiftMods)
  kSPgDn <- parseBytes (bytesOf "\ESC[6;2~")
  checkEq "shift-pagedown 6;2~" kSPgDn (KPageDown shiftMods)
  kCSHome <- parseBytes (bytesOf "\ESC[1;6H")
  checkEq "ctrl-shift-home 1;6H" kCSHome (KHome ctrlShift)
  kEuro <- parseBytes [0xe2, 0x82, 0xac]   -- € U+20AC
  checkEq "utf8 euro" kEuro (KChar '\x20ac')
  -- Ctrl+Enter in both common encodings (must NOT be mistaken for EOF/F3).
  kCEother <- parseBytes (bytesOf "\ESC[27;5;13~")   -- xterm modifyOtherKeys
  checkEq "ctrl-enter modifyOtherKeys" kCEother KModEnter
  kCEcsiu <- parseBytes (bytesOf "\ESC[13;5u")       -- CSI u
  checkEq "ctrl-enter csi-u" kCEcsiu KModEnter
  kF3tilde <- parseBytes (bytesOf "\ESC[13~")        -- plain 13~ is still F3
  checkEq "f3 via 13~" kF3tilde (KFn 3 noMods)
  kUnkTilde <- parseBytes (bytesOf "\ESC[99~")       -- unknown tilde must not be KUnknown []
  check "unknown tilde is not the EOF sentinel" (kUnkTilde /= KUnknown [])
  kSEother <- parseBytes (bytesOf "\ESC[27;2;13~")   -- Shift+Enter modifyOtherKeys
  checkEq "shift-enter modifyOtherKeys" kSEother KModEnter
  kSEcsiu <- parseBytes (bytesOf "\ESC[13;2u")       -- Shift+Enter CSI u
  checkEq "shift-enter csi-u" kSEcsiu KModEnter
  -- Once the Kitty protocol is enabled these arrive disambiguated as CSI-u and
  -- must map back to the same keys their legacy bytes produced.
  kAltT  <- parseBytes (bytesOf "\ESC[116;3u")       -- Alt+t
  checkEq "alt-t csi-u" kAltT (KAltChar 't')
  kCtrlS <- parseBytes (bytesOf "\ESC[115;5u")       -- Ctrl+s
  checkEq "ctrl-s csi-u" kCtrlS (KCtrlChar 's')
  kEscU  <- parseBytes (bytesOf "\ESC[27u")          -- Esc
  checkEq "esc csi-u" kEscU KEsc
  kEnterU <- parseBytes (bytesOf "\ESC[13u")         -- plain Enter (no mods) stays KEnter
  checkEq "plain enter csi-u" kEnterU KEnter
  kMouse <- parseBytes (map (fromIntegral . fromEnum) "\ESC[<0;10;5M")
  case kMouse of
    KMouse me -> do
      checkEq "mouse col" (meCol me) 9
      checkEq "mouse row" (meRow me) 4
      check "mouse pressed" (mePressed me)
    _ -> check "mouse parse" False

  -- Editor update ------------------------------------------------------------
  let ed0 = newEditor (24, 80) defaultConfig
      ed1 = fst (update (KChar 'i') (fst (update (KChar 'h') ed0)))
  checkEq "type chars" (getLine' 0 (edBuffer ed1)) (T.pack "hi")
  let edSel = fst (update (KCtrlChar 'a') ed1)
  check "select all" (case getSelection edSel of Just _ -> True; Nothing -> False)
  let (edCopy, effs) = update (KCtrlChar 'c') edSel
  check "copy effect" (not (null effs))
  -- Opening Replace with a (single-line) selection seeds the Find field.
  let edReplSel = fst (update (KCtrlChar 'r') edSel)
  checkEq "replace seeds find from selection"
    (maybe (T.pack "?") (fieldValue 0) (edDialog edReplSel)) (T.pack "hi")
  -- With no selection the Find field stays empty (the last search term).
  let edReplNone = fst (update (KCtrlChar 'r') ed1)
  checkEq "replace without selection leaves find empty"
    (maybe (T.pack "?") (fieldValue 0) (edDialog edReplNone)) (T.pack "")

  -- Multi-line find / replace (Shift+Enter enters a newline in the dialog field).
  let feed = foldl (\e k -> fst (update k e))   -- Editor -> [Key] -> Editor
  -- Shift/Ctrl+Enter inserts a newline into the focused field; paste keeps them.
  let dlgNL = feed (fst (update (KCtrlChar 'f') ed0)) [KChar 'a', KModEnter, KChar 'b']
  checkEq "shift-enter inserts newline in field"
    (maybe (T.pack "?") (fieldValue 0) (edDialog dlgNL)) (T.pack "a\nb")
  let dlgPaste = fst (update (KPaste "x\ny") (fst (update (KCtrlChar 'f') ed0)))
  checkEq "paste keeps newlines in field"
    (maybe (T.pack "?") (fieldValue 0) (edDialog dlgPaste)) (T.pack "x\ny")
  -- Ctrl+V routes the clipboard into the focused field via applyPaste.
  let dlgClip = applyPaste (T.pack "p\nq") (fst (update (KCtrlChar 'f') ed0))
  checkEq "ctrl-v pastes clipboard into focused field"
    (maybe (T.pack "?") (fieldValue 0) (edDialog dlgClip)) (T.pack "p\nq")

  -- Multi-line dialog fields: visible-height cap (like a tall CSV cell),
  -- line-relative Home, and vertical navigation within the field.
  let mkField s = Field (T.pack "Find:") (T.pack s) 0
  checkEq "field visible height caps at 3" (fieldVisH (mkField "a\nb\nc\nd")) 3
  checkEq "single-line field height is 1" (fieldVisH (mkField "abc")) 1
  let curOf d = fCur (head (dlgFields d))
      edFld   = feed (fst (update (KCtrlChar 'f') ed0))
                  [KChar 'a', KChar 'b', KModEnter, KChar 'c', KChar 'd']  -- "ab\ncd", cur=5
  checkEq "field cursor at end of multi-line value"
    (maybe (-1) curOf (edDialog edFld)) 5
  checkEq "Home goes to start of the current line, not the whole field"
    (maybe (-1) curOf (edDialog (fst (update (KHome noMods) edFld)))) 3
  checkEq "Up moves to the previous line keeping the column"
    (maybe (-1) curOf (edDialog (fst (update (KArrow DUp noMods) edFld)))) 2

  -- fieldSetCursorLineCol maps a (line, col) to a clamped character index.
  let fd3 = mkFind (T.pack "ab\ncd\nef") False False   -- field focused by default
  checkEq "set cursor by line/col -> char index"
    (curOf (fieldSetCursorLineCol 1 1 fd3)) 4          -- col 1 of line 1 ("cd")
  checkEq "set cursor by line/col clamps past the end"
    (curOf (fieldSetCursorLineCol 9 9 fd3)) 8          -- clamps to end of "ef"

  -- A click on a multi-line field focuses it AND lands the cursor on the clicked
  -- cell. Compute the real on-screen geometry and click the 'd' cell (line 1).
  let edClk = feed (fst (update (KCtrlChar 'f') ed0))
                [ KChar 'a', KChar 'b', KModEnter, KChar 'c', KChar 'd'
                , KModEnter, KChar 'e', KChar 'f' ]   -- "ab\ncd\nef"
  case edDialog edClk of
    Nothing -> check "click test: find dialog present" False
    Just dClk -> do
      let loClk = computeLayout edClk
          (yClk, xClk, _, _) = dialogGeom edClk dClk loClk
          valStartClk = (xClk + 2) + (T.length (T.pack "Find:") + 1)
          rowClk = (yClk + 1) + fieldRowIndex dClk 0 + 1   -- field line index 1 ("cd")
          meClk  = MouseEvent MBLeft (valStartClk + 1) rowClk True False noMods 1
          edClk2 = fst (update (KMouse meClk) edClk)
      checkEq "click lands cursor on the clicked cell (line 1, col 1)"
        (maybe (-1) curOf (edDialog edClk2)) 4
  -- Find a term that spans a line break: type "AB", Shift+Enter, "CD".
  let mlBuf  = setLoadedText (T.pack "xxAB\nCDyy\nAB\nCD") ed0
      mlFind = fst (update KEnter
                 (feed (fst (update (KCtrlChar 'f') mlBuf))
                       [KChar 'A', KChar 'B', KModEnter, KChar 'C', KChar 'D']))
  checkEq "multiline find selects across the line break"
    (getSelection mlFind) (Just (Pos 0 2, Pos 1 2))
  checkEq "multiline find selection text"
    (maybe (T.pack "") (\(a, b) -> textInRange a b (edBuffer mlFind)) (getSelection mlFind))
    (T.pack "AB\nCD")
  -- Replace All over a multi-line term rewrites every match across line breaks.
  let mlRepl = feed (fst (update (KCtrlChar 'r') (setLoadedText (T.pack "AB\nCD\nAB\nCD") ed0)))
                 [ KChar 'A', KChar 'B', KModEnter, KChar 'C', KChar 'D'  -- Find: "AB\nCD"
                 , KTab, KChar 'Z'                                        -- Replace: "Z"
                 , KTab, KTab, KTab, KEnter ]                             -- focus "Replace All", run
  checkEq "multiline replace-all collapses matches" (lineCount (edBuffer mlRepl)) 2
  checkEq "multiline replace-all line 0" (getLine' 0 (edBuffer mlRepl)) (T.pack "Z")
  checkEq "multiline replace-all line 1" (getLine' 1 (edBuffer mlRepl)) (T.pack "Z")

  -- replaceAllText: the linear-time replace-all engine (case / whole-word aware).
  -- Returns (count, result, Just offset-just-past-the-last-replacement).
  let raCS = replaceAllText True False (T.pack "ab") (T.pack "X") (T.pack "ab_ab_AB")
  checkEq "replace-all case-sensitive" raCS (2, T.pack "X_X_AB", Just 3)
  let raCI = replaceAllText False False (T.pack "ab") (T.pack "X") (T.pack "ab_Ab_AB")
  checkEq "replace-all case-insensitive keeps surrounding text" raCI (3, T.pack "X_X_X", Just 5)
  let raWW = replaceAllText True True (T.pack "cat") (T.pack "DOG") (T.pack "cat category cat")
  checkEq "replace-all whole-word skips substrings" raWW (2, T.pack "DOG category DOG", Just 16)
  let raML = replaceAllText False False (T.pack "a\nb") (T.pack "Z") (T.pack "a\nb\na\nB")
  checkEq "replace-all multi-line term" raML (2, T.pack "Z\nZ", Just 3)
  checkEq "replace-all with empty replacement deletes"
    (replaceAllText True False (T.pack "x") (T.pack "") (T.pack "axbxc")) (2, T.pack "abc", Just 2)
  checkEq "replace-all no match reports Nothing"
    (replaceAllText False False (T.pack "zz") (T.pack "Q") (T.pack "abc"))
    (0, T.pack "abc", Nothing)
  -- Linear time: a large buffer must finish promptly (guards against O(n^2)).
  let (bigN, bigT, _) = replaceAllText False False (T.pack "foo") (T.pack "barbar")
                          (T.replicate 50000 (T.pack "foo "))
  checkEq "replace-all large-input count" bigN 50000
  checkEq "replace-all large-input length" (T.length bigT) (50000 * 7)
  -- Friendly status wording, pluralised and comma-grouped.
  checkEq "replace-all status: many" (replaceAllStatus 12999 (T.pack "x"))
    (T.pack "Replaced 12,999 matches")
  checkEq "replace-all status: one" (replaceAllStatus 1 (T.pack "x")) (T.pack "Replaced 1 match")

  -- End-to-end Replace All: closes the dialog, jumps the cursor to the last
  -- replacement, and shows the friendly count in the status bar.
  let raEnd = feed (fst (update (KCtrlChar 'r') (setLoadedText (T.pack "aXa\naXa") ed0)))
                [ KChar 'X'                    -- Find: "X"
                , KTab, KChar 'Y', KChar 'Y'   -- Replace: "YY"
                , KTab, KTab, KTab, KEnter ]   -- focus "Replace All", run
  check "replace-all closes the dialog" (edDialog raEnd == Nothing)
  checkEq "replace-all moves cursor to the last replacement" (edCursor raEnd) (Pos 1 3)
  checkEq "replace-all sets the friendly status" (edStatus raEnd) (T.pack "Replaced 2 matches")
  checkEq "replace-all applied to the buffer" (getLine' 1 (edBuffer raEnd)) (T.pack "aYYa")
  let edUndo = fst (update (KCtrlChar 'z') ed1)
  check "undo shrinks" (T.length (getLine' 0 (edBuffer edUndo)) < 2)

  -- The modified flag clears when undo returns to the saved/opened content.
  check "modified after typing" (edModified ed1)
  let edBack = fst (update (KCtrlChar 'z') ed1)   -- undo the (coalesced) "hi"
  check "not modified after undo to opened state" (not (edModified edBack))
  check "modified again after redo" (edModified (fst (update (KCtrlChar 'y') edBack)))
  -- Manually deleting back to the original content also clears the flag.
  let edTyped = fst (update (KChar 'q') ed0)
      edErased = fst (update KBackspace edTyped)
  check "modified after typing q" (edModified edTyped)
  check "not modified after deleting back to original" (not (edModified edErased))

  -- A multi-line buffer + go-to-line via the editor's gotoLine path.
  let edBig = setLoadedText (T.pack (unlines (map show [1 .. 50 :: Int]))) ed0
      edMoved = moveDown 10 edBig
  check "vertical move" (posLine (edCursor edMoved) == 10)

  -- Revert availability + the File-menu Revert entry -------------------------
  let mkLR t = LoadResult (fromText (T.pack t)) LF Utf8 True False Nothing
      hasRevert e = any (\en -> case en of MEItem _ _ MARevert -> True; _ -> False)
                        (entriesFor e 0)
      edA = setLoaded "a.txt" (mkLR "aaa") ed0
  check "revert hidden on a clean just-loaded file" (not (hasRevert edA))
  check "revert hidden on an untitled buffer" (not (hasRevert ed0))
  check "revert shown after an edit" (hasRevert (fst (update (KChar 'x') edA)))
  -- A file that changed on disk offers Revert even with no local edits.
  let edDisk = noteDiskMtime (Just (mt 100)) edA { edDiskMtime = Just (mt 50) }
  check "noteDiskMtime flags a newer file" (edDiskChanged edDisk)
  check "revert shown when the file changed on disk" (hasRevert edDisk)
  check "noteDiskMtime: same mtime is unchanged"
        (not (edDiskChanged (noteDiskMtime (Just (mt 50)) edA { edDiskMtime = Just (mt 50) })))
  check "noteDiskMtime: missing file is treated as unchanged"
        (not (edDiskChanged (noteDiskMtime Nothing edA { edDiskMtime = Just (mt 50) })))
  -- Saving re-baselines the on-disk time and clears the changed flag.
  let (edSaved, _) = onSaved 3 (Just (mt 200)) edDisk
  check "onSaved clears disk-changed" (not (edDiskChanged edSaved))
  checkEq "onSaved updates disk mtime" (edDiskMtime edSaved) (Just (mt 200))
  -- Opening the menu requests a stat so the flag is fresh; untitled files don't.
  check "opening the menu requests a disk stat"
        (any (\e -> case e of EffStatFile "a.txt" -> True; _ -> False)
             (snd (update (KFn 10 noMods) edA)))
  check "no stat request for an untitled buffer"
        (not (any (\e -> case e of EffStatFile _ -> True; _ -> False)
                  (snd (update (KFn 10 noMods) ed0))))

  -- Re-opening an already-open file switches to it instead of duplicating -----
  let edTwo     = setLoadedNew "b.txt" (mkLR "bbb") edA        -- a backgrounded, b active
      edReopenA = setLoadedNew "a.txt" (mkLR "aaa-reloaded") edTwo
  checkEq "two files open" (fileCount edTwo) 2
  checkEq "re-opening does not add a copy" (fileCount edReopenA) 2
  checkEq "re-opening switches to the file" (edPath edReopenA) (Just "a.txt")
  checkEq "re-opening keeps the existing buffer (ignores the reload)"
          (getLine' 0 (edBuffer edReopenA)) (T.pack "aaa")
  checkEq "re-opening the active file is a no-op"
          (fileCount (setLoadedNew "b.txt" (mkLR "x") edTwo)) 2

  -- CSV table model ----------------------------------------------------------
  let v1 = mkCsvView ',' (T.pack "a,b,c\n1,2,3\n")
  checkEq "csv rows" (nRows v1) 2
  checkEq "csv cols" (nCols v1) 3
  checkEq "csv cell(1,1)" (cellAt 1 1 v1) (T.pack "2")
  checkEq "csv roundtrip" (csvToText v1) (T.pack "a,b,c\n1,2,3")
  let v2 = mkCsvView ',' (T.pack "\"x,y\",\"line\nbreak\",z\n")
  checkEq "csv quoted comma" (cellAt 0 0 v2) (T.pack "x,y")
  checkEq "csv quoted newline" (cellAt 0 1 v2) (T.pack "line\nbreak")
  checkEq "csv requote" (csvToText v2) (T.pack "\"x,y\",\"line\nbreak\",z")
  let v3 = commitEdit (editInsert 'Z' (beginEditFresh 'Q' v1))
  checkEq "csv edit cell" (cellAt 0 0 v3) (T.pack "QZ")
  let v4 = insertRowBelow v1
  checkEq "csv insert row" (nRows v4) 3
  let v5 = deleteCol (moveCursor DRight v1)   -- delete column B
  checkEq "csv delete col" (cellAt 0 1 v5) (T.pack "c")
  let v6 = redo (undo v3)
  checkEq "csv undo/redo" (cellAt 0 0 v6) (T.pack "QZ")

  -- Multi-line cells: rows grow with embedded newlines (capped), and the
  -- char-index <-> (line,col) mapping used for in-cell up/down navigation.
  let vml = mkCsvView ',' (T.pack "a,\"x\ny\nz\"\n")   -- B1 = "x\ny\nz"
  checkEq "csv cell line count" (cellLineCount (cellAt 0 1 vml)) 3
  checkEq "csv row height" (rowHeight vml 0) 3
  let vml5 = mkCsvView ',' (T.pack "\"1\n2\n3\n4\n5\"\n")
  checkEq "csv row height capped" (rowHeight vml5 0) maxCellLines
  checkEq "csv cursorLineCol" (cursorLineCol (T.pack "ab\ncde") 5) (1, 2)

  -- The table's modified flag clears when undo returns to the saved grid.
  let vmodA = mkCsvView ',' (T.pack "a,b\n1,2\n")
      vmodB = commitEdit (beginEditFresh 'Z' vmodA)   -- A1 -> "Z"
  check "csv not modified initially" (not (isModified vmodA))
  check "csv modified after edit" (isModified vmodB)
  check "csv not modified after undo to saved" (not (isModified (undo vmodB)))
  check "csv modified again after redo" (isModified (redo (undo vmodB)))

  -- Rectangular cell selection: copy (mini-CSV) and grid-aware paste.
  let vg   = mkCsvView ',' (T.pack "a,b,c\nd,e,f\ng,h,i\n")
      vsel = withSel (moveCursor DDown) (withSel (moveCursor DRight) vg)  -- A1:B2
  check "csv hasSelection" (hasSelection vsel)
  checkEq "csv selRect" (selRect vsel) (0, 0, 1, 1)
  checkEq "csv copyText box" (copyText vsel) (T.pack "a,b\nd,e")
  let (vfill, _) = pasteClip (T.pack "X") vsel        -- scalar fills the box
  checkEq "csv fill A1" (cellAt 0 0 vfill) (T.pack "X")
  checkEq "csv fill B2" (cellAt 1 1 vfill) (T.pack "X")
  let (vspread, _) = pasteClip (T.pack "P,Q\nR,S") vg -- grid spreads from one cell
  checkEq "csv spread B2" (cellAt 1 1 vspread) (T.pack "S")
  let (vmismatch, _) = pasteClip (T.pack "1,2,3") vsel  -- 1x3 into a 2x2: rejected
  checkEq "csv mismatch unchanged" (cellAt 0 0 vmismatch) (T.pack "a")
  checkEq "csv clearSelCells" (cellAt 1 1 (clearSelCells vsel)) (T.pack "")

  -- File explorer panel ------------------------------------------------------
  let expEntries = [ ("/w/a.txt", False, Just 3), ("/w/sub", True, Nothing)
                   , ("/w/b.txt", False, Just 3) ]
      edExp = explorerStart "/w" expEntries ed0
  check "explorer opens focused" (edFocus edExp == FExplorer)
  check "explorer has a tree" (maybe False (const True) (edExplorer edExp))
  -- The sidebar shifts the text area right by exactly its width.
  let loExp = computeLayout edExp
  checkEq "sidebar width == content left" (loContentLeft loExp) (sidebarWidth edExp)
  check "sidebar takes real width" (sidebarWidth edExp > 1)
  checkEq "text left == sidebar + gutter" (loTextLeft loExp) (loContentLeft loExp + loGutter loExp)
  -- Collapsed -> a single-column strip; closed -> no sidebar.
  checkEq "collapsed strip is one column" (sidebarWidth (edExp { edExpCollapsed = True })) 1
  checkEq "no folder -> no sidebar" (sidebarWidth ed0) 0
  -- Selection order is directories first (sub), then files (a.txt, b.txt).
  let selOf e = maybe (-1) brSelected (edExplorer e)
      edDown  = fst (update (KArrow DDown noMods) edExp)
  checkEq "explorer down moves selection" (selOf edDown) 1
  -- Enter on a file emits EffOpen for it (and returns focus to the editor).
  let (edOpenF, openEffs) = update KEnter edDown
  check "explorer Enter opens the file"
        (any (\e -> case e of EffOpen "/w/a.txt" -> True; _ -> False) openEffs)
  -- Focus follows the loaded document, not the keypress: the panel keeps focus
  -- until the file loads, then a text/CSV file hands focus to the editor (an
  -- image keeps it in the panel — checked with the image fixtures below).
  check "opening a file keeps panel focus until it loads" (edFocus edOpenF == FExplorer)
  check "loading text from the panel blurs it"
        (edFocus (setLoadedNew "/w/a.txt" (mkLR "aaa") edOpenF) == FEdit)
  -- Right arrow on a directory requests its listing (lazy load).
  let (_, expandEffs) = update (KArrow DRight noMods) edExp
  check "explorer expand lists the directory"
        (any (\e -> case e of EffExplorerList "/w/sub" -> True; _ -> False) expandEffs)
  -- explorerLoaded fills the directory's children; the child becomes navigable.
  let edChild  = explorerLoaded "/w/sub" [("/w/sub/inner.txt", False, Just 5)]
                   (fst (update (KArrow DRight noMods) edExp))
      (_, kidEffs) = update KEnter (fst (update (KArrow DDown noMods) edChild))
  check "explorer opens a loaded child"
        (any (\e -> case e of EffOpen "/w/sub/inner.txt" -> True; _ -> False) kidEffs)
  -- Ctrl+B toggles focus between the panel and the editor.
  checkEq "Ctrl+B blurs the focused panel" (edFocus (fst (update (KCtrlChar 'b') edExp))) FEdit
  checkEq "Ctrl+B refocuses a blurred panel"
          (edFocus (fst (update (KCtrlChar 'b') edExp { edFocus = FEdit }))) FExplorer
  -- Decorations: an open & modified file is marked; an unopened one is not.
  let edMarked = fst (update (KChar 'x') (setLoaded "/w/a.txt" (mkLR "aaa") edExp))
  checkEq "open active modified file is marked"
          (fmap fmModified (fileMarkFor edMarked "/w/a.txt")) (Just True)
  check "active flag set on the open file"
        (maybe False fmActive (fileMarkFor edMarked "/w/a.txt"))
  checkEq "unopened file has no mark" (fileMarkFor edMarked "/w/b.txt") Nothing
  -- The panel renders a divider column and the directory marker.
  let scrExp = renderEditor edExp
      cellRC scr r c = scrCells scr A.! (r * scrW scr + c)
      cl = sidebarWidth edExp
  checkEq "panel divider drawn" (cellChar (cellRC scrExp 1 (cl - 1))) '\x2502'
  checkEq "directory marker drawn" (cellChar (cellRC scrExp 2 1)) '\x25b8'
  -- A file that changed on disk since loading is marked with a diamond (◆).
  let edDisk = noteDiskMtime (Just (mt 100))
                 ((setLoaded "/w/a.txt" (mkLR "aaa") edExp) { edDiskMtime = Just (mt 50) })
      scrDisk = renderEditor edDisk
  checkEq "disk-changed file shows a diamond"
          (cellChar (cellRC scrDisk 3 (sidebarWidth edDisk - 2))) '\x25c6'

  -- Background refresh: re-list on expand, merge-preserving listings ----------
  -- Expanding a directory always requests a fresh listing (even when its
  -- children are cached), so an externally-changed dir is correct on open.
  let edSelSub = fst (update (KHome noMods) edExp)      -- select "sub" (dirs sort first)
      (edExpand, expandEffs) = update (KArrow DRight noMods) edSelSub
  checkEq "expand emits a listing request"
          [ p | EffExplorerList p <- expandEffs ] ["/w/sub"]
  let edSub = explorerLoaded "/w/sub" [("/w/sub/inner.txt", False, Just 1)] edExpand
      (_, expandEffs2) = update (KArrow DRight noMods)
                           (fst (update (KArrow DLeft noMods) edSub))
  checkEq "re-expand of an already-loaded dir re-lists it"
          [ p | EffExplorerList p <- expandEffs2 ] ["/w/sub"]
  -- A fresh root listing (the poll found a new file) merges in: the expanded
  -- subdir keeps its loaded subtree, the selection stays on the same path,
  -- and the new file appears.
  let edMerged = explorerLoaded "/w" (expEntries ++ [("/w/new.txt", False, Just 9)]) edSub
  check "merge keeps the subdir expanded with its children"
        (case edExplorer edMerged >>= Br.nodeAt "/w/sub" of
           Just n  -> fnExpanded n && maybe False (const True) (fnChildren n)
           Nothing -> False)
  check "merge picks up the new file"
        (any ((== T.pack "new.txt") . fnName . snd)
             (maybe [] Br.visibleRows (edExplorer edMerged)))
  check "merge keeps the selection on the same path"
        ((fnPath <$> (edExplorer edMerged >>= Br.selectedNode))
           == (fnPath <$> (edExplorer edSub >>= Br.selectedNode)))
  checkEq "expandedDirPaths lists root and expanded subdir"
          (maybe [] Br.expandedDirPaths (edExplorer edSub)) ["/w", "/w/sub"]
  -- Ctrl+B on a collapsed panel expands it and focuses it, from any mode.
  let ctrlB e = fst (update (KCtrlChar 'b') e)
      colFoc e = (edExpCollapsed e, edFocus e)
  checkEq "Ctrl+B expands a collapsed panel (editor focus)"
          (colFoc (ctrlB (edExp { edExpCollapsed = True, edFocus = FEdit }))) (False, FExplorer)
  checkEq "Ctrl+B expands a collapsed panel (panel focus)"
          (colFoc (ctrlB (edExp { edExpCollapsed = True, edFocus = FExplorer }))) (False, FExplorer)
  let edCsvExp = setLoaded "/w/t.csv" (mkLR "a,b\n1,2") (edExp { edFocus = FEdit })
  checkEq "Ctrl+B works from CSV table view"
          (colFoc (ctrlB (edCsvExp { edExpCollapsed = True }))) (False, FExplorer)
  -- Terminal focus reports parse and are inert in the pure model.
  kFocIn  <- parseBytes [0x1b, 0x5b, 0x49]
  kFocOut <- parseBytes [0x1b, 0x5b, 0x4f]
  checkEq "focus-in parses"  kFocIn  (KFocus True)
  checkEq "focus-out parses" kFocOut (KFocus False)
  check "focus events are inert in the model"
        (let (e', effs) = update (KFocus True) edExp
         in null effs && edFocus e' == edFocus edExp
            && fmap brSelected (edExplorer e') == fmap brSelected (edExplorer edExp))
  -- noteDiskMtimes: stale-on-disk flags for open docs, active or backgrounded.
  let edA100 = (setLoaded "/w/a.txt" (mkLR "aaa") ed0) { edDiskMtime = Just (mt 100) }
      edAB   = setLoadedNew "/w/b.txt" (mkLR "bbb") edA100   -- a.txt joins edBefore
      edPolled = noteDiskMtimes [("/w/a.txt", Just (mt 200)), ("/w/b.txt", Nothing)] edAB
  check "background doc flagged when newer on disk" (any docDiskChanged (edBefore edPolled))
  check "doc without a baseline stays unflagged" (not (edDiskChanged edPolled))
  let edStale = noteDiskMtimes [("/w/a.txt", Just (mt 200))] (edA100 { edStatus = T.empty })
  check "active doc flagged when newer on disk" (edDiskChanged edStale)
  check "stale active doc shows a notice"
        (T.pack "changed on disk" `T.isInfixOf` edStatus edStale)

  -- Large / binary files: detection and display --------------------------------
  check "looksBinary on text" (not (looksBinary (TE.encodeUtf8 (T.pack "hello world\nplain text"))))
  check "looksBinary on NUL bytes" (looksBinary (BS.pack [0x7f,0x45,0x4c,0x46,0x00,0x01,0x02]))
  checkEq "shortSize bytes" (shortSize 512) "512"
  checkEq "shortSize KB" (shortSize (3 * 1024)) "3K"
  checkEq "shortSize MB" (shortSize (52 * 1024 * 1024)) "52M"
  check "humanSize MB has a decimal" ("MB" `T.isInfixOf` T.pack (humanSize (2 * 1024 * 1024)))
  -- A huge file in the tree is dimmed and labelled with its size.
  let bigEntries = [("/w/huge.bin", False, Just (maxOpenBytes + 1))]
      edBig = explorerStart "/w" bigEntries ed0
      scrBig = renderEditor edBig
      cellRC2 scr r c = scrCells scr A.! (r * scrW scr + c)
      row2 = [ cellChar (cellRC2 scrBig 2 c) | c <- [0 .. sidebarWidth edBig - 2] ]
  check "oversize file shows a size label" (shortSize (maxOpenBytes + 1) `isInfixOf` row2)
  -- Background-load (spinner) state: input is swallowed while loading.
  let edLoad = beginLoading "big.log" ed0
  check "beginLoading sets the flag" (maybe False (const True) (edLoading edLoad))
  check "keys are ignored while loading"
        (edCursor (fst (update (KChar 'x') edLoad)) == edCursor edLoad
         && not (edModified (fst (update (KChar 'x') edLoad))))
  check "endLoading clears the flag" (maybe True (const False) (edLoading (endLoading edLoad)))
  check "tickLoading advances the frame"
        (maybe (-1) snd (edLoading (tickLoading edLoad)) == 1)
  -- The spinner overlay renders while loading.
  let scrLoad = renderEditor edLoad
      loadText = [ cellChar (cellRC2 scrLoad r c) | r <- [0 .. scrH scrLoad - 1], c <- [0 .. scrW scrLoad - 1] ]
  check "spinner overlay shows Loading" ("Loading" `isInfixOf` loadText)

  -- Image decoding -----------------------------------------------------------
  -- Hand-built fixtures keep these self-contained; real-file decoding (PNG
  -- DEFLATE, GIF LZW, baseline JPEG) is validated pixel-exact by the offline
  -- PTY/harness against PIL ground truth.
  let pic = [(255,0,0),(0,255,0),(0,0,255),(255,255,0)]  -- 2x2: red, green / blue, yellow
      bmp = mkBMP 2 2 pic
      ppm = mkPPM 2 2 pic
      png = mkPNG 2 2 pic
      s   = map (fromIntegral . fromEnum)
  checkEq "sniff BMP"  (sniffImage bmp) (Just "BMP")
  checkEq "sniff PNM"  (sniffImage ppm) (Just "PNM")
  checkEq "sniff PNG"  (sniffImage png) (Just "PNG")
  checkEq "sniff JPEG" (sniffImage (BS.pack [0xFF,0xD8,0xFF,0xE0])) (Just "JPEG")
  checkEq "sniff GIF"  (sniffImage (BS.pack (s "GIF89a..."))) (Just "GIF")
  checkEq "sniff WebP" (sniffImage mkWebPLL) (Just "WebP")
  checkEq "sniff none" (sniffImage (BS.pack (s "hello, world"))) Nothing
  forM_ [("bmp", bmp), ("ppm", ppm), ("png", png), ("webp", mkWebPLL)] $ \(nm, bs) ->
    case decodeImage bs of
      Left e   -> check (nm ++ " decode: " ++ e) False
      Right im -> do
        checkEq (nm ++ " dims") (imgW im, imgH im) (2, 2)
        checkEq (nm ++ " TL red")    (pixelAt im 0 0) (255,0,0,255)
        checkEq (nm ++ " TR green")  (pixelAt im 1 0) (0,255,0,255)
        checkEq (nm ++ " BL blue")   (pixelAt im 0 1) (0,0,255,255)
        checkEq (nm ++ " BR yellow") (pixelAt im 1 1) (255,255,0,255)
  -- WebP lossless alpha survives exactly.
  case decodeImage mkWebPLLA of
    Left e   -> check ("webp alpha decode: " ++ e) False
    Right im -> do
      checkEq "webp alpha TL" (pixelAt im 0 0) (255,0,0,255)
      checkEq "webp alpha TR" (pixelAt im 1 0) (0,255,0,128)
      checkEq "webp alpha BL" (pixelAt im 0 1) (0,0,255,64)
      checkEq "webp alpha BR" (pixelAt im 1 1) (255,255,0,0)
  -- Lossy VP8: the decoder is bit-exact against libwebp, so the expected
  -- pixels below are libwebp's own output for these fixtures.
  case decodeImage mkWebPLossy of
    Left e   -> check ("webp lossy decode: " ++ e) False
    Right im -> do
      checkEq "webp lossy dims" (imgW im, imgH im) (8, 8)
      checkEq "webp lossy TL" (pixelAt im 0 0) (198,51,33,255)
      checkEq "webp lossy TR" (pixelAt im 7 0) (18,62,197,255)
      checkEq "webp lossy BL" (pixelAt im 0 7) (200,50,33,255)
      checkEq "webp lossy BR" (pixelAt im 7 7) (19,61,197,255)
  case decodeImage mkWebPLossyA of
    Left e   -> check ("webp lossy+alpha decode: " ++ e) False
    Right im -> do
      checkEq "webp ALPH left"  (pixelAt im 0 0) (198,51,33,255)
      checkEq "webp ALPH right" (pixelAt im 7 0) (18,62,197,40)
  check "truncated WebP -> error" (isLeft (decodeImage (BS.take 30 mkWebPLossy)))
  -- A PNG with an invalid zlib header is reported as an error, not rendered.
  let badPng = BS.pack ([137,80,78,71,13,10,26,10]
                 ++ [0,0,0,13] ++ s "IHDR" ++ be32b 2 ++ be32b 2 ++ [8,2,0,0,0] ++ [0,0,0,0]
                 ++ [0,0,0,4]  ++ s "IDAT" ++ [0xde,0xad,0xbe,0xef] ++ [0,0,0,0]
                 ++ [0,0,0,0]  ++ s "IEND" ++ [0,0,0,0])
  check "corrupt PNG -> error" (isLeft (decodeImage badPng))
  -- Animated GIF: the full frame sequence, with sub-rectangle composition,
  -- transparency, disposal methods and the delay clamp.
  case decodeFrames mkGIFAnim of
    Left e -> check ("gif anim decode: " ++ e) False
    Right frames -> do
      checkEq "gif anim frame count" (length frames) 3
      checkEq "gif anim delays (50cs, clamp 0 -> 100ms, 30cs)"
              (map snd frames) [500, 100, 300]
      case map fst frames of
        [f1, f2, f3] -> do
          checkEq "gif f1 TL red"    (pixelAt f1 0 0) (255,0,0,255)
          checkEq "gif f1 TR green"  (pixelAt f1 1 0) (0,255,0,255)
          checkEq "gif f1 BL blue"   (pixelAt f1 0 1) (0,0,255,255)
          checkEq "gif f1 BR yellow" (pixelAt f1 1 1) (255,255,0,255)
          -- Frame 2 paints yellow over TL; its transparent TR pixel leaves
          -- frame 1's green showing through; the bottom row is untouched.
          checkEq "gif f2 TL painted"     (pixelAt f2 0 0) (255,255,0,255)
          checkEq "gif f2 TR sees f1"     (pixelAt f2 1 0) (0,255,0,255)
          checkEq "gif f2 BL untouched"   (pixelAt f2 0 1) (0,0,255,255)
          -- Frame 2's disposal 2 clears its top-row rectangle before frame 3
          -- (which draws only a transparent pixel); the bottom row survives.
          checkEq "gif f3 TL cleared"     (pixelAt f3 0 0) (0,0,0,0)
          checkEq "gif f3 TR cleared"     (pixelAt f3 1 0) (0,0,0,0)
          checkEq "gif f3 BL survives"    (pixelAt f3 0 1) (0,0,255,255)
          checkEq "gif f3 BR survives"    (pixelAt f3 1 1) (255,255,0,255)
        _ -> check "gif anim: three frames" False
  -- decodeImage still yields just the first frame (the cheap still path).
  case decodeImage mkGIFAnim of
    Left e   -> check ("gif first frame: " ++ e) False
    Right im -> checkEq "gif decodeImage = frame 1" (pixelAt im 0 0) (255,0,0,255)
  -- The frame cap truncates a long animation instead of decoding unboundedly.
  checkEq "gif frame cap truncates"
          (either (const 0) length (decodeGIFFrames 2 mkGIFAnim)) 2
  -- A GIF cut off mid-stream keeps the frames already decoded.
  checkEq "gif truncated keeps whole frames"
          (either (const 0) length
             (decodeFrames (BS.pack (gifAnimHeader ++ gifAnimF1 ++ take 4 gifAnimF2)))) 1
  -- Nonsense dimensions are refused up front, never allocated.
  check "gif huge header refused"
        (isLeft (decodeFrames (BS.pack (map (fromIntegral . fromEnum) "GIF89a"
                                        ++ le16b 30000 ++ le16b 30000 ++ [0x91,0,0]))))
  -- Rendering produces a grid of exactly the requested size.
  case decodeImage bmp of
    Right im -> do
      checkEq "render grid bounds"
        (bounds (renderImage 1.0 Nothing Ascii 10 4 (0,0,imgW im,imgH im) im)) ((0,0),(3,9))
      -- Opening an image from the file-explorer panel keeps the selection focus
      -- in the panel (no keystroke editing in the read-only image view);
      -- opened any other way it takes the editor focus like a normal document.
      check "image opened from the panel keeps panel focus"
            (edFocus (imageLoadedNew "/w/pic.bmp" [(im, 0)] edExp) == FExplorer)
      check "image opened elsewhere focuses the view"
            (edFocus (imageLoadedNew "/w/pic.bmp" [(im, 0)] ed0) == FEdit)
      -- The half-block cell picture is drawn only when no pixel placement will
      -- cover it; with graphics caps present the renderer blanks the area so the
      -- blocky fallback / checkerboard can't bleed through the overlay.
      let edImgE = setGfxCaps True False False (imageLoaded "/w/pic.bmp" [(im, 0)] ed0)   -- caps + editor
      check "gfx overlay off without caps"
            (not (imageOverlayActive (imageLoaded "/w/pic.bmp" [(im, 0)] ed0)))
      check "gfx overlay on with caps (image focused)"
            (imageOverlayActive edImgE)
      check "gfx overlay on with caps (panel focused)"
            (imageOverlayActive (setGfxCaps True False False (imageLoaded "/w/pic.bmp" [(im, 0)] edExp)))
      check "gfx overlay off when the search view obscures the image"
            (not (imageOverlayActive (edImgE { edSearchMode = True })))
      -- Animation scheduling: who steps the frames depends on the terminal.
      let anim2 = [(im, 500), (im, 100)]
          edAnim = imageLoaded "/w/anim.gif" anim2 ed0          -- no gfx caps
          edAnimK = setGfxCaps True True False edAnim           -- real kitty (native animation)
          edAnimS = setGfxCaps False False True edAnim          -- sixel
          edAnimG = setGfxCaps True False False edAnim          -- static kitty protocol (Ghostty/WezTerm)
      check "still image never ticks"
            (imageTickUs (imageLoaded "/w/pic.bmp" [(im, 0)] ed0) == Nothing)
      check "cell fallback ticks at the frame delay"
            (imageTickUs edAnim == Just 500000)
      check "cell fallback clamps tiny delays to 50ms"
            (imageTickUs (tickImage edAnim) == Just 100000
             && imageTickUs (tickImage edAnim { edImage = fmap (\d -> d { idFrames = [(im, 500), (im, 5)] }) (edImage edAnim) }) == Just 50000)
      check "kitty animates natively (no editor tick)"
            (imageTickUs edAnimK == Nothing && imageKittyAnim edAnimK)
      check "kitty + zoom crop freezes (still no tick)"
            (let edCrop = edAnimK { edImage = fmap (\d -> d { idCrop = Just (0,0,1,1) }) (edImage edAnimK) }
             in imageTickUs edCrop == Nothing && not (imageKittyAnim edCrop))
      check "sixel steps with a 100ms floor"
            (maybe False (>= 100000) (imageTickUs edAnimS) && not (imageKittyAnim edAnimS))
      -- A terminal that answers the kitty-graphics probe but is not real
      -- kitty (Ghostty, WezTerm, Konsole) must not be trusted to animate:
      -- the editor steps it via cheap placement swaps at the cell floor.
      check "static-kitty terminal is stepped by the editor"
            (imageTickUs edAnimG == Just 500000 && not (imageKittyAnim edAnimG))
      check "static-kitty steps clamp to 50ms (not the sixel floor)"
            (imageTickUs (edAnimG { edImage = fmap (\d -> d { idFrames = [(im, 500), (im, 5)], idFrame = 1 }) (edImage edAnimG) }) == Just 50000)
      check "static-kitty + zoom crop freezes like real kitty"
            (let edCropG = edAnimG { edImage = fmap (\d -> d { idCrop = Just (0,0,1,1) }) (edImage edAnimG) }
             in imageTickUs edCropG == Nothing)
      check "static-kitty tickImage advances (editor owns playback)"
            (maybe (-1) idFrame (edImage (tickImage edAnimG)) == 1)
      check "kitty animation whitelist: real kitty only"
            (supportsKittyAnim "kitty(0.32.2)" && supportsKittyAnim "KiTTY(0.40.0)"
             && not (supportsKittyAnim "ghostty 1.1.3")
             && not (supportsKittyAnim "WezTerm 20240203-110809-5046fc22")
             && not (supportsKittyAnim "Konsole 24.08.0"))
      check "tickImage advances and wraps"
            (let f = maybe (-1) idFrame . edImage
             in f edAnim == 0 && f (tickImage edAnim) == 1
                && f (tickImage (tickImage edAnim)) == 0)
      check "tickImage is a no-op when kitty owns playback"
            (maybe (-1) idFrame (edImage (tickImage edAnimK)) == 0)
    Left _   -> check "render decode" False
  -- The image view is read-only and cursor-less: the terminal cursor must be
  -- hidden over a focused image (a text document still shows one).
  case decodeImage bmp of
    Right im -> do
      checkEq "image view hides the terminal cursor"
              (scrCursor (renderEditor (imageLoaded "pic.bmp" [(im, 0)] ed0))) Nothing
      check "text view still places the cursor"
            (isJust' (scrCursor (renderEditor (setLoaded "t.txt" (mkLR "hi") ed0))))
    Left _ -> check "image cursor decode" False

  -- Syntax highlighting (Haskell) --------------------------------------------
  checkEq "langForPath .hs" (langForPath (Just "Foo.hs")) (Just Haskell)
  let hsLex s = fst (lexLine Haskell initialState (T.pack s))
      hsTokAt s i = hsLex s !! i
  -- Keyword, type (upper-case), function (lower-case) get distinct tokens.
  checkEq "hs keyword"  (hsTokAt "module Main where" 0) TkKeyword
  checkEq "hs type"     (hsTokAt "data Foo = Bar" 5) TkType
  checkEq "hs builtin"  (hsTokAt "map f xs" 0) TkBuiltin
  -- A line comment covers the rest of the line, but @-->@ stays an operator.
  checkEq "hs line comment" (hsTokAt "x = 1 -- note" 6) TkComment
  checkEq "hs arrow not comment" (hsTokAt "a --> b" 2) TkText
  -- Strings and char literals.
  checkEq "hs string" (hsTokAt "s = \"hi\"" 4) TkString
  checkEq "hs char"   (hsTokAt "c = 'x'" 4) TkString
  -- A trailing prime is part of the identifier, not a char literal.
  checkEq "hs prime ident" (length (takeWhile (== TkBuiltin) (drop 0 (hsLex "foldl' f")))) 6
  -- Block comments nest and carry their depth across lines.
  let (_, st1) = lexLine Haskell initialState (T.pack "{- outer {- inner")
  checkEq "hs nested comment state" st1 (StNestComment 2)
  let (toks2, st2) = lexLine Haskell st1 (T.pack "still -} closed -} x")
  checkEq "hs comment closes to normal" st2 StNormal
  checkEq "hs code after comment" (last toks2) TkText
  -- A pragma on one line is coloured as a decorator.
  checkEq "hs pragma" (hsTokAt "{-# LANGUAGE OverloadedStrings #-}" 0) TkDecorator

  -- Highlight-state cache (HlCache) ---------------------------------------------
  -- Brute-force reference: state before line i is entry i of this scan.
  let bruteStates lang lns = scanl (\st ln -> snd (lexLine lang st ln)) initialState lns
      -- Every state the cache claims to cover must agree with the brute force.
      agreesAll lang lns c =
        and (zipWith (==) (take (hlCoverage c + 1) (bruteStates lang lns))
                          [ hlStateBefore c i | i <- [0 .. hlCoverage c] ])
  -- A block comment opened on line 0 and never closed: every later line is
  -- inside it. The old renderer's bounded look-back mis-lexed this past 2000
  -- lines; the cache must be right arbitrarily deep.
  let deepN = 2500 :: Int
      deepLines = T.pack "select /* open" : [ T.pack ("line " ++ show i) | i <- [1 .. deepN - 1] ]
      deepSeq = Seq.fromList deepLines
      deepC = refreshHlCache SQL deepSeq 2400 Nothing
  checkEq "hlcache: block-comment state survives past the old 2000-line cap"
          (hlStateBefore deepC 2400) StBlock
  check "hlcache: deep states agree with brute force" (agreesAll SQL deepLines deepC)
  checkEq "hlcache: refresh of unchanged buffer keeps coverage"
          (hlCoverage (refreshHlCache SQL deepSeq 2400 (Just deepC))) (hlCoverage deepC)
  -- In-place single-line edit that leaves the line's end state alone: the old
  -- tail re-converges, so even a small refresh target snaps coverage back to
  -- the whole file instead of re-lexing it.
  let fullC = refreshHlCache SQL deepSeq (deepN - 1) (Just deepC)
      editedLines = take 5 deepLines ++ [T.pack "edited!"] ++ drop 6 deepLines
      editC = refreshHlCache SQL (Seq.fromList editedLines) 10 (Just fullC)
  checkEq "hlcache: re-convergence adopts the old tail" (hlCoverage editC) deepN
  check "hlcache: adopted states agree with brute force" (agreesAll SQL editedLines editC)
  -- An edit that changes the state (closing the comment) reflows everything below.
  let closedLines = take 5 editedLines ++ [T.pack "still */ closed"] ++ drop 6 editedLines
      closedC = refreshHlCache SQL (Seq.fromList closedLines) 2400 (Just editC)
  check "hlcache: closing the comment reflows the states below"
        (hlStateBefore closedC 2400 == StNormal && agreesAll SQL closedLines closedC)
  -- Structural edits (insert / delete a line) keep the prefix and stay correct.
  let insLines = take 100 closedLines ++ [T.pack "again /* new"] ++ drop 100 closedLines
      insC = refreshHlCache SQL (Seq.fromList insLines) 2400 (Just closedC)
      delLines = take 100 insLines ++ drop 101 insLines
      delC = refreshHlCache SQL (Seq.fromList delLines) 2400 (Just insC)
  check "hlcache: line insert stays correct"
        (hlStateBefore insC 2400 == StBlock && agreesAll SQL insLines insC)
  check "hlcache: line delete stays correct"
        (hlStateBefore delC 2400 == StNormal && agreesAll SQL delLines delC)
  -- Randomized cross-check: a deterministic stream of replace/insert/delete
  -- edits with varying refresh targets must always agree with the brute force
  -- (Python docstrings make the cross-line state flip often).
  let lcg s = (s * 1103515245 + 12345) `mod` 2147483648 :: Int
      pyLineFor r = case r `mod` 5 of
        0 -> T.pack "x = 1"
        1 -> T.pack "\"\"\""
        2 -> T.pack "# comment"
        3 -> T.pack "s = \"str\""
        _ -> T.pack "def f():"
      applyOp r lns
        | null lns = [pyLineFor r]
        | otherwise =
            let i = (r `div` 7) `mod` length lns
            in case r `mod` 3 of
                 0 -> take i lns ++ [pyLineFor (r `div` 11)] ++ drop (i + 1) lns  -- replace
                 1 -> take i lns ++ [pyLineFor (r `div` 11)] ++ drop i lns        -- insert
                 _ | length lns > 1 -> take i lns ++ drop (i + 1) lns             -- delete
                   | otherwise      -> lns
      fuzzStep (lns, mc, s, ok) _ =
        let s1 = lcg s
            lns' = applyOp s1 lns
            s2 = lcg s1
            tgt = s2 `mod` (length lns' + 2)
            c = refreshHlCache Python (Seq.fromList lns') tgt mc
            ok' = ok && agreesAll Python lns' c
                     && hlCoverage c >= min (tgt + 1) (length lns')
        in (lns', Just c, s2, ok')
      fuzzLines0 = [ pyLineFor i | i <- [0 .. 59] ]
      fuzzC0 = refreshHlCache Python (Seq.fromList fuzzLines0) 59 Nothing
      (_, _, _, fuzzOk) = foldl fuzzStep (fuzzLines0, Just fuzzC0, 42, True) [1 .. 300 :: Int]
  check "hlcache: 300 randomized edits all agree with brute force" fuzzOk
  -- End to end: the renderer paints a line deep inside the block comment in
  -- the comment colour — impossible with the old bounded look-back.
  let edSqlDeep = (setLoaded "deep.sql" (mkLR (intercalate "\n" (map T.unpack deepLines))) ed0)
                    { edTop = 2400 }
      scrSql = renderEditor edSqlDeep
      loSql = computeLayout edSqlDeep
      sqlCell = scrCells scrSql A.! (loTextTop loSql * scrW scrSql + loTextLeft loSql)
  checkEq "render: comment style survives past the old look-back cap"
          (styleFg (cellStyle sqlCell)) BrightBlack

  -- Frame diffing: replaying the emitted escape stream over the previous
  -- screen must reproduce the new screen exactly (characters and styles), so
  -- the changed-span emitter can never drop or misplace an update ---------------
  let builderStr b = T.unpack (TE.decodeUtf8 (BSL.toStrict (BB.toLazyByteString b)))
      sgrOf st = builderStr (styleSgr st)
      -- The canonical (char, style) grid of a screen; wide-glyph continuation
      -- cells are absent (covered by their base glyph), like on a terminal.
      seedGrid scr = M.fromList
        [ ((r, c), (cellChar cell, sgrOf (cellStyle cell)))
        | r <- [0 .. scrH scr - 1], c <- [0 .. scrW scr - 1]
        , let cell = scrCells scr A.! (r * scrW scr + c)
        , cellChar cell /= '\0' ]
      -- A tiny VT: CUP, SGR, 2J, DECSTBM + SU/SD (hardware scrolling), REP
      -- and printable placement with wide-char widths. Rows a scroll exposes
      -- have no grid entry (blank with default style), like a real terminal's
      -- erased cells; compare with 'normEq' when a stream may scroll.
      vtRun g0 s0 = go g0 (0, 0) "" Nothing ' ' s0
        where
          go g _ _ _ _ [] = g
          -- OSC/DCS/APC strings (hyperlinks, titles, …) place no cells:
          -- skip to the BEL or ST terminator like a real terminal.
          go g pos sgr mg lch ('\ESC' : c0 : rest)
            | c0 `elem` ("]P_" :: String) = skipStr rest
            where
              skipStr ('\BEL' : r') = go g pos sgr mg lch r'
              skipStr ('\ESC' : '\\' : r') = go g pos sgr mg lch r'
              skipStr (_ : r') = skipStr r'
              skipStr [] = g
          go g (r, c) sgr mg lch ('\ESC' : '[' : rest) =
            let (params, rest1) = span (\x -> isDigit x || x `elem` (";?<>=" :: String)) rest
            in case rest1 of
                 (cmd : rest2) -> case cmd of
                   'H' -> let (rr, cc) = parseRC params in go g (rr - 1, cc - 1) sgr mg lch rest2
                   'm' -> go g (r, c) ("\ESC[" ++ params ++ "m") mg lch rest2
                   'J' -> go M.empty (r, c) sgr mg lch rest2
                   'r' -> let mg' = if null params then Nothing
                                    else let (t, b) = parseRC params in Just (t - 1, b - 1)
                          in go g (0, 0) sgr mg' lch rest2
                   'S' -> go (vscroll g mg (numOf 1 params)) (r, c) sgr mg lch rest2
                   'T' -> go (vscroll g mg (negate (numOf 1 params))) (r, c) sgr mg lch rest2
                   'b' -> let n = numOf 1 params
                              g' = foldl (\gg k -> M.insert (r, c + k) (lch, sgr) gg) g [0 .. n - 1]
                          in go g' (r, c + n) sgr mg lch rest2
                   _   -> go g (r, c) sgr mg lch rest2
                 [] -> g
          go g (r, c) sgr mg _ (ch : rest) =
            let wdt = max 1 (charWidth ch)
                g1 = M.insert (r, c) (ch, sgr) g
                g2 = if wdt == 2 then M.delete (r, c + 1) g1 else g1
            in go g2 (r, c + wdt) sgr mg ch rest
          -- SU by n within the margin band: an entry at row R lands at R-n
          -- (dropped if that leaves the band); rows outside are untouched.
          vscroll g mg n =
            let (mt, mb) = maybe (0, maxBound `div` 2) id mg
            in M.fromList
                 [ ((r', c), v)
                 | ((r, c), v) <- M.toList g
                 , r' <- if r < mt || r > mb
                           then [r]
                           else [ r - n | r - n >= mt, r - n <= mb ] ]
          numOf d s = case reads s of [(n, _)] -> n; _ -> d
          parseRC s = case break (== ';') s of
            (a, ';' : b) -> (read a, read b)
            (a, _)       -> (read a :: Int, 1)
      -- Grid equality where a missing entry means a blank default cell.
      normEq w h a b = all (\k -> lk a k == lk b k)
                           [ (r, c) | r <- [0 .. h - 1], c <- [0 .. w - 1] ]
        where lk g k = M.findWithDefault (' ', sgrOf defaultStyle) k g
      replayOk prev next =
        vtRun (seedGrid prev) (builderStr (renderFrame plainCaps (Just prev) next)) == seedGrid next
  let edD0 = setLoaded "d.txt" (mkLR "hello world\nsecond line here\nthird") ed0
      edD1 = fst (update (KChar 'x') edD0)
      edD2 = fst (update (KArrow DDown noMods) edD1)
      edD3 = fst (update (KEnd noMods) edD2)
      edDm = fst (update (KFn 10 noMods) edD3)          -- open the menu (overlay)
      edW0 = setLoaded "w.txt" (mkLR "\27721\23383 wide \28450\23383\nabc") ed0
      edW1 = fst (update (KChar 'y') edW0)              -- shifts wide glyphs right
      scrsD = map renderEditor [edD0, edD1, edD2, edD3, edDm]
      scrsW = map renderEditor [edW0, edW1]
  check "framediff: text edits replay exactly"
        (and (zipWith replayOk scrsD (tail scrsD)))
  check "framediff: menu overlay open/close replays exactly"
        (replayOk (scrsD !! 4) (scrsD !! 3) && replayOk (scrsD !! 3) (scrsD !! 4))
  check "framediff: wide glyphs shifted by an edit replay exactly"
        (and (zipWith replayOk scrsW (tail scrsW)))
  check "framediff: identical screens emit no cell updates"
        (vtRun (seedGrid (head scrsD)) (builderStr (renderFrame plainCaps (Just (head scrsD)) (head scrsD)))
          == seedGrid (head scrsD))
  -- Full redraw (no previous screen) starts from clear and matches too.
  check "framediff: full redraw replays exactly"
        (vtRun M.empty (builderStr (renderFrame plainCaps Nothing (head scrsD))) == seedGrid (head scrsD))
  -- A cursor-only move must now cost a handful of bytes, not whole rows.
  check "framediff: cursor move emits a small diff"
        (length (builderStr (renderFrame plainCaps (Just (renderEditor edD1)) (renderEditor edD2))) < 400)

  -- Hardware scrolling: consecutive frames that are the same text shifted one
  -- row must go out as a scroll-region op plus a small repaint, and replaying
  -- the stream through the (scroll-aware) VT must reproduce the new frame ------
  -- Distinct line contents, so repainting the shifted band would be expensive
  -- and the scroll genuinely wins the savings comparison.
  let lngLine i = show (i :: Int) ++ ": "
                    ++ take (20 + (i * 13) `mod` 40)
                            (drop ((i * 7) `mod` 20) (cycle "lorem ipsum dolor sit amet "))
      edLng = setLoaded "lng.txt"
                (mkLR (intercalate "\n" [ lngLine i | i <- [1 .. 200] ])) ed0
      edLa = edLng { edTop = 20, edCursor = Pos 30 0 }
      edLb = edLng { edTop = 21, edCursor = Pos 31 0 }
      scrLa = renderEditor edLa
      scrLb = renderEditor edLb
      scrollStream = builderStr (renderFrame plainCaps (Just scrLa) scrLb)
  check "hwscroll: plan fires for a one-line scroll"
        (isJust' (scrollPlan scrLa scrLb))
  check "hwscroll: emits SU inside a scroll region, then resets it"
        ("\ESC[1S" `isInfixOf` scrollStream && "\ESC[r" `isInfixOf` scrollStream)
  check "hwscroll: scroll stream is much smaller than the band repaint"
        (length scrollStream < length (builderStr (renderFrame plainCaps Nothing scrLb)) `div` 4)
  check "hwscroll: scroll-aware replay reproduces the frame"
        (normEq (scrW scrLb) (scrH scrLb) (vtRun (seedGrid scrLa) scrollStream) (seedGrid scrLb))
  check "hwscroll: reverse scroll (SD) replays exactly"
        (normEq (scrW scrLa) (scrH scrLa)
                (vtRun (seedGrid scrLb) (builderStr (renderFrame plainCaps (Just scrLb) scrLa)))
                (seedGrid scrLa))
  -- A jump farther than the band height cannot scroll; it must still replay.
  let scrLc = renderEditor (edLng { edTop = 90, edCursor = Pos 90 0 })
  check "hwscroll: page jump falls back to the plain diff"
        (not (isJust' (scrollPlan scrLa scrLc))
         && normEq (scrW scrLc) (scrH scrLc)
                   (vtRun (seedGrid scrLa) (builderStr (renderFrame plainCaps (Just scrLa) scrLc)))
                   (seedGrid scrLc))
  -- Identical frames must not scroll (delta 0).
  check "hwscroll: no plan for identical frames" (not (isJust' (scrollPlan scrLa scrLa)))

  -- REP run compression: gated on the probe; with it on, streams shrink and
  -- the (REP-aware) VT replay still reproduces every cell --------------------
  let repCaps = plainCaps { rcRep = True }
      repFull  = builderStr (renderFrame repCaps Nothing scrLa)
      slowFull = builderStr (renderFrame plainCaps Nothing scrLa)
  check "rep: full redraw replays exactly"
        (normEq (scrW scrLa) (scrH scrLa) (vtRun M.empty repFull) (seedGrid scrLa))
  check "rep: compressed redraw is smaller" (length repFull < length slowFull)
  check "rep: diff replay with REP still exact"
        (normEq (scrW scrLb) (scrH scrLb)
                (vtRun (seedGrid scrLa) (builderStr (renderFrame repCaps (Just scrLa) scrLb)))
                (seedGrid scrLb))

  -- OSC 8 hyperlinks: URI building, URL recognition, and emission -------------
  checkEq "link: file uri percent-encodes"
          (filePathUri "/tmp/a b.txt") (Just "file:///tmp/a%20b.txt")
  checkEq "link: file uri unicode"
          (filePathUri "/tmp/caf\233.txt") (Just "file:///tmp/caf%C3%A9.txt")
  checkEq "link: relative path has no uri" (filePathUri "d.txt") Nothing
  checkEq "link: pseudo path has no uri" (filePathUri "cmedit://Manual.md") Nothing
  checkEq "link: windows drive path"
          (filePathUri "C:\\dir\\f.txt") (Just "file:///C:/dir/f.txt")
  checkEq "link: url span with trailing dot trimmed"
          (urlSpans "see https://example.com/x. end")
          [(4, 25, "https://example.com/x")]
  checkEq "link: parenthesised url drops the closer"
          (urlSpans "(https://a.b/c)") [(1, 14, "https://a.b/c")]
  checkEq "link: balanced wiki parens survive"
          (urlSpans "https://en.wikipedia.org/wiki/Foo_(bar)")
          [(0, 39, "https://en.wikipedia.org/wiki/Foo_(bar)")]
  checkEq "link: bare scheme is not a link" (urlSpans "http:// nope") []
  checkEq "link: plain text has none" (urlSpans "nothing to see here") []
  check "link: two urls, both found"
        (length (urlSpans "https://a.b/1 and http://c.d/2") == 2)
  check "link: id is stable and hex"
        (linkIdOf "https://a.b/1" == linkIdOf "https://a.b/1"
         && all (`elem` ("0123456789abcdef" :: String)) (linkIdOf "https://a.b/1"))
  -- A document line containing a URL emits an OSC 8 open around it and a
  -- close after — and the replay (which skips OSC strings) is still exact.
  let edUrl  = setLoaded "u.txt" (mkLR "docs at https://example.com/guide today\nplain") ed0
      scrUrl = renderEditor edUrl
      urlStream = builderStr (renderFrame plainCaps Nothing scrUrl)
  check "link: url in text emits OSC 8 open with id"
        ("\ESC]8;id=" `isInfixOf` urlStream)
  check "link: emission closes the link"
        ("\ESC]8;;\ESC\\" `isInfixOf` urlStream)
  check "link: full redraw with links replays exactly"
        (vtRun M.empty urlStream == seedGrid scrUrl)
  -- No link, no OSC 8 bytes at all (portable stream unchanged).
  check "link: linkless frame emits no OSC 8"
        (not ("\ESC]8" `isInfixOf` builderStr (renderFrame plainCaps Nothing (head scrsD))))
  -- The status bar links an absolute file path.
  let edAbs = setLoaded "/tmp/abs.txt" (mkLR "hello") ed0
      absStream = builderStr (renderFrame plainCaps Nothing (renderEditor edAbs))
  check "link: status bar links absolute paths"
        ("file:///tmp/abs.txt" `isInfixOf` absStream)
  check "link: status bar replay exact"
        (vtRun M.empty absStream == seedGrid (renderEditor edAbs))
  -- REP never merges a run across a link boundary: two half-rows of the
  -- same glyph with different targets must open two separate links.
  let linkCell u = CellL 'x' defaultStyle (Just u)
      lrow = [ linkCell (if c < 10 then "https://a.example/" else "https://b.example/")
             | c <- [0 .. (19 :: Int)] ]
      lscr = Screen { scrW = 20, scrH = 1
                    , scrCells = A.listArray (0, 19) lrow
                    , scrCursor = Nothing, scrHint = Nothing }
      lstream = builderStr (renderFrame (plainCaps { rcRep = True }) Nothing lscr)
      countInfix pat s = length [ () | t <- tails s, pat `isPrefixOf` t ]
  check "link: REP run breaks at a link boundary"
        (countInfix "\ESC]8;id=" lstream == 2)
  check "link: REP-linked row replays exactly"
        (vtRun M.empty lstream == seedGrid lscr)

  -- Styled underline emission: colon form only under the capability.
  checkEq "undercurl: colon form when supported"
          (builderStr (styleSgrWith (RenderCaps True False) (Style Default Default attrUndercurl)))
          "\ESC[0;4:3m"
  checkEq "undercurl: plain underline fallback"
          (builderStr (styleSgrWith plainCaps (Style Default Default attrUndercurl)))
          "\ESC[0;4m"

  -- Terminal capability plumbing ----------------------------------------------
  checkEq "osc color: 16-bit rgb" (parseOscColor "rgb:1e1e/2a2a/3b3b") (Just (30, 42, 59))
  checkEq "osc color: 8-bit rgb"  (parseOscColor "rgb:ff/80/00") (Just (255, 128, 0))
  checkEq "osc color: hash form"  (parseOscColor "#102030") (Just (16, 32, 48))
  checkEq "osc color: junk"       (parseOscColor "cmyk:1/2/3") Nothing
  check "bg luminance verdicts" (isDarkRgb 30 42 59 && not (isDarkRgb 250 250 240))
  checkEq "rep probe: col 5 = supported"   (repProbeResult 1 5) (Just True)
  checkEq "rep probe: col 3 = ignored"     (repProbeResult 1 3) (Just False)
  checkEq "rep probe: unrelated CPR"       (repProbeResult 12 40) Nothing
  let capsSteps = foldl (flip applyReply) defaultCaps
        [ TrDA1 [64, 4, 22], TrTermVersion "kitty(0.31.0)"
        , TrKittyGfx True, TrCursorPos 1 5 ]
  check "caps fold: sixel + undercurl + kitty gfx + rep"
        (tcSixel capsSteps && tcUndercurl capsSteps && tcKittyGfx capsSteps && tcRep capsSteps)
  check "caps: unknown terminal keeps portable defaults"
        (let mystery = applyReply (TrTermVersion "MysteryTerm 1.0") defaultCaps
         in not (tcUndercurl mystery) && not (tcSixel mystery)
              && not (tcKittyGfx mystery) && not (tcRep mystery))

  -- Reply parsing (the byte streams real terminals answer with) --------------
  kBg <- parseBytes (bytesOf "\ESC]11;rgb:1e1e/2a2a/3b3b\a")
  checkEq "parse OSC 11 reply (BEL)" kBg (KReply (TrBgColor 30 42 59))
  kBg2 <- parseBytes (bytesOf "\ESC]11;rgb:ffff/ffff/ffff\ESC\\")
  checkEq "parse OSC 11 reply (ST)" kBg2 (KReply (TrBgColor 255 255 255))
  kDa <- parseBytes (bytesOf "\ESC[?62;4;22c")
  checkEq "parse DA1 reply" kDa (KReply (TrDA1 [62, 4, 22]))
  kCpr <- parseBytes (bytesOf "\ESC[?1;5R")
  checkEq "parse DECXCPR reply" kCpr (KReply (TrCursorPos 1 5))
  kF3 <- parseBytes (bytesOf "\ESC[1;2R")
  checkEq "modified F3 is still a key" kF3 (KFn 3 shiftOnly)
  kCell <- parseBytes (bytesOf "\ESC[6;18;9t")
  checkEq "parse cell-size reply" kCell (KReply (TrCellPx 18 9))
  kTxt <- parseBytes (bytesOf "\ESC[4;720;1280t")
  checkEq "parse text-area-px reply" kTxt (KReply (TrTextPx 720 1280))
  kVer <- parseBytes (bytesOf "\ESCP>|WezTerm 20240203\ESC\\")
  checkEq "parse XTVERSION reply" kVer (KReply (TrTermVersion "WezTerm 20240203"))
  kGfx <- parseBytes (bytesOf "\ESC_Gi=31;OK\ESC\\")
  checkEq "parse kitty graphics OK" kGfx (KReply (TrKittyGfx True))
  kGfxNo <- parseBytes (bytesOf "\ESC_Gi=31;ENOTSUPPORTED\ESC\\")
  checkEq "kitty graphics error = unsupported" kGfxNo (KReply (TrKittyGfx False))
  kAltBr <- parseBytes (bytesOf "\ESC]")
  checkEq "bare ESC ] stays Alt+]" kAltBr (KAltChar ']')
  kAltP <- parseBytes (bytesOf "\ESCP")
  checkEq "bare ESC P stays Alt+Shift+P" kAltP (KAltChar 'P')

  -- Theme resolution (theme=auto follows the detected background) ------------
  checkEq "config: theme = auto parses" (cfgTheme (fst (parseConfigText "theme = auto" defaultConfig))) ThemeAuto
  checkEq "theme auto defaults dark" (resolvedTheme ed0) ThemeDark
  checkEq "theme auto follows a light background" (resolvedTheme (setDetectedDark False ed0)) ThemeLight
  checkEq "explicit theme beats detection"
          (resolvedTheme ((setDetectedDark False ed0) { edConfig = (edConfig ed0) { cfgTheme = ThemeDark } }))
          ThemeDark

  -- Cell-aspect-aware image fit ----------------------------------------------
  checkEq "viewFit: classic 2:1 cells" (viewFit 1.0 Nothing 20 10 100 100) (20, 20, 0, 0)
  checkEq "viewFit: taller cells shorten the fitted height" (viewFit 1.25 Nothing 20 10 100 100) (20, 16, 0, 2)
  -- With a native-size cap a small image is pinned at 1:1 and centred rather
  -- than enlarged to fill the canvas.
  checkEq "viewFit: cap pins a small image at native, centred"
          (viewFit 1.0 (Just 0.125) 80 22 32 32) (4, 4, 38, 20)
  checkEq "viewFit: cap does not shrink an image that already fits"
          (viewFit 1.0 (Just 10.0) 20 10 100 100) (20, 20, 0, 0)
  checkEq "cellAspect: unknown geometry = 1.0" (cellAspect ed0) 1.0
  check "cellAspect: reported geometry is clamped sane"
        (let a = cellAspect (setCellPx (9, 22) ed0) in a > 1.0 && a <= 1.6)

  -- Explorer file-type classification -----------------------------------------
  checkEq "fileKind: png is a displayable image" (fileKind "a/b/logo.png") FKImage
  checkEq "fileKind: JPEG image (case-insensitive)" (fileKind "Photo.JPG") FKImage
  checkEq "fileKind: source code we highlight" (fileKind "src/Main.hs") FKCode
  checkEq "fileKind: python source" (fileKind "run.py") FKCode
  checkEq "fileKind: markdown is markup" (fileKind "README.md") FKMarkup
  checkEq "fileKind: html is markup" (fileKind "index.html") FKMarkup
  checkEq "fileKind: json is data" (fileKind "pkg.json") FKData
  checkEq "fileKind: csv is data" (fileKind "rows.csv") FKData
  checkEq "fileKind: binary blob we cannot open" (fileKind "app.wasm") FKBinary
  checkEq "fileKind: svg stays markup, not a displayable image" (fileKind "icon.svg") FKMarkup
  checkEq "fileKind: unknown extension is plain" (fileKind "notes.txt") FKPlain
  checkEq "fileKind: no extension is plain" (fileKind "Makefile") FKPlain

  -- Pointer-shape hints --------------------------------------------------------
  let edPtr = setLoaded "p.txt" (mkLR "hello world") ed0
  checkEq "pointer: text area is a beam" (pointerShapeFor edPtr 5 10) "text"
  checkEq "pointer: menu bar is a hand" (pointerShapeFor edPtr 0 3) "pointer"
  checkEq "pointer: scrollbar column is default" (pointerShapeFor edPtr 5 79) "default"

  -- Pixel-graphics encoders ----------------------------------------------------
  checkEq "base64: RFC vector" (builderStr (base64B (BS.pack (map (fromIntegral . fromEnum) ("Man" :: String))))) "TWFu"
  checkEq "base64: padding" (builderStr (base64B (BS.pack [77]))) "TQ=="
  let redPx = BS.pack (concat (replicate 4 [255, 0, 0, 255]))
      sixRed = builderStr (sixelEncode 2 2 redPx)
  check "sixel: DCS..ST framing with raster attributes"
        ("\ESCP0;1;0q" `isPrefixOf` sixRed && "\ESC\\" `isSuffixOf` sixRed
           && "\"1;1;2;2" `isInfixOf` sixRed && "#" `isInfixOf` sixRed)
  checkEq "gfxFit: aspect-true centred box (upscale to fill)"
          (gfxFit (10, 20) (1, 0, 80, 22) (100, 100) True) (1, 18, 44, 22, 100, 100)
  -- Without upscaling a small image is placed at native size, centred, and the
  -- pixel payload is 1:1 with the source (matches the cell view's imageFitCap).
  checkEq "gfxFit: no-upscale pins a small image at native, centred"
          (gfxFit (8, 16) (0, 0, 80, 24) (32, 32) False) (11, 38, 4, 2, 32, 32)
  case decodeImage bmp of
    Right im -> checkEq "scaleRGBA: exact payload size"
                        (BS.length (scaleRGBA im (0, 0, imgW im, imgH im) 5 3)) 60
    Left _   -> check "scaleRGBA decode" False
  let kitB = builderStr (kittyPlace (2, 3) (10, 5) (2, 2) (BS.replicate 16 0))
  check "kitty: delete-all, then a display transmit"
        ("\ESC_Ga=d,d=A\ESC\\" `isPrefixOf` kitB
           && "a=T,f=32" `isInfixOf` kitB && "m=0" `isInfixOf` kitB)

  -- CSV column-width cache ------------------------------------------------------
  -- The cache maintained by withRows/undo/redo must always equal a fresh
  -- recomputation (serialise -> reparse is the ground truth), across cell
  -- edits, multi-line cells, row/column inserts/deletes and undo/redo.
  let widthsOk v = columnWidths v == columnWidths (mkCsvView (csvDelim v) (csvToText v))
      csvOp r v = case r `mod` 10 of
        0 -> setCurrentCell (T.pack (replicate (1 + r `mod` 40) 'x')) v
        1 -> setCurrentCell (T.pack "s") v
        2 -> insertRowBelow v
        3 -> deleteRow v
        4 -> insertColRight v
        5 -> deleteCol v
        6 -> Cmedit.Csv.undo v
        7 -> Cmedit.Csv.redo v
        8 -> commitEdit (editInsert 'q' (editInsert '\n' (beginEditFresh 'w' v)))
        _ -> setCursor (r `mod` (nRows v + 1)) ((r `div` 7) `mod` (nCols v + 1)) v
      csvFuzzStep (v, s, ok) _ =
        let s' = lcg s
            v' = csvOp s' v
        in (v', s', ok && widthsOk v'
                       -- pointer-accelerated modified flag == plain equality
                       && Cmedit.Csv.isModified v' == (csvRows v' /= csvSaved v'))
      vw0 = mkCsvView ',' (T.pack "a,bb,ccc\ndddd,e,f\ng,hh,i")
      (_, _, csvWidthsOk) = foldl csvFuzzStep (vw0, 7, True) [1 .. 250 :: Int]
  check "csv width cache correct at load" (widthsOk vw0)
  check "csv width cache survives 250 random ops" csvWidthsOk
  -- scrollLeft agrees with the (cubic) reference it replaced.
  let scrollLeftRef width v =
        let ws = columnWidths v
            cc = csvCurCol v
            fits l = sum [ ws !! c + 1 | c <- [l .. cc], c < length ws ] <= width
            go l | l >= cc = cc
                 | fits l = l
                 | otherwise = go (l + 1)
        in go (max 0 (min (csvLeft v) cc))
      vWide = mkCsvView ',' (T.intercalate (T.pack "\n")
                [ T.intercalate (T.pack ",")
                    [ T.replicate (1 + (r * 31 + c * 7) `mod` 12) (T.pack "y")
                    | c <- [0 .. 59 :: Int] ]
                | r <- [0 .. 3 :: Int] ])
      slCase s =
        let s1 = lcg s
            v1 = (setCursor (s1 `mod` 4) (s1 `mod` 60) vWide) { csvLeft = (s1 `div` 5) `mod` 60 }
            width = 10 + (s1 `div` 11) `mod` 70
        in csvLeft (Cmedit.Csv.ensureVisible 5 0 width v1) == scrollLeftRef width v1
  check "csv scrollLeft matches the reference" (all slCase [1 .. 120])
  -- Perf tripwire: navigating a 200k-row table must not rescan every cell per
  -- keystroke (this regresses to many seconds if the width cache is bypassed).
  let vHuge = mkCsvView ',' (T.intercalate (T.pack "\n")
                (replicate 200000 (T.pack "aa,bb,cc,dd")))
      navHuge = foldl (\vv _ -> Cmedit.Csv.ensureVisible 20 0 78 (moveCursor DDown vv))
                      vHuge [1 .. 25 :: Int]
  checkEq "csv huge-table navigation is cheap" (csvCurRow navHuge) 25
  -- The modified flag is exact at any table size (the 50k-row cutoff is gone)
  -- and cheap per keystroke even editing the END of a huge table: typing a
  -- character sets it, undoing clears it again.
  let edCsvHuge = setLoaded "h.csv" (mkLR (T.unpack (T.intercalate (T.pack "\n")
                    (replicate 60000 (T.pack "aa,bb,cc"))))) ed0
      edHE1 = fst (update (KChar 'x') (fst (update (KEnd ctrlOnly) edCsvHuge)))
      edHE2 = fst (update (KCtrlChar 'z') (fst (update KEsc edHE1)))
  check "huge csv: typing at the end sets modified" (edModified edHE1)
  check "huge csv: undo clears modified exactly" (not (edModified edHE2))

  -- Browser type-ahead ----------------------------------------------------------
  let taNames = ["alpha", "beta", "apple", "cherry", "avocado", "berry"]
      taBr = Br.mkBrowserNoParent "/t" [ ("/t/" ++ nm, False, Just 1) | nm <- taNames ]
      -- the quadratic reference it replaced
      taRef ch br =
        let rows = Br.visibleRows br
            starts (_, nn) = not (T.null (fnName nn)) && T.head (fnName nn) == ch
            n = length rows
            order = [ (brSelected br + 1 + k) `mod` n | k <- [0 .. n - 1] ]
        in case [ i | i <- order, i < n, starts (rows !! i) ] of
             (i : _) -> br { brSelected = i }
             []      -> br
      taCase s =
        let s1 = lcg s
            br' = Br.setSel (s1 `mod` length taNames) taBr
            ch = "abcz" !! ((s1 `div` 3) `mod` 4)
        in brSelected (Br.typeAhead ch br') == brSelected (taRef ch br')
  check "typeAhead matches the reference" (all taCase [1 .. 80 :: Int])
  checkEq "typeAhead wraps past the end"
          (brSelected (Br.typeAhead 'a' (Br.setSel 4 taBr))) 0
  checkEq "typeAhead miss keeps the selection"
          (brSelected (Br.typeAhead 'z' (Br.setSel 2 taBr))) 2

  -- Word hops / double-click word range: linear on huge single-line tokens ----
  -- References: the old T.index-stepping implementations they replaced.
  let clsOf ch = (isSpace ch, isAlphaNum ch || ch == '_')
      wordLeftRef line c0 =
        let skipSp i | i > 0 && isSpace (T.index line (i - 1)) = skipSp (i - 1)
                     | otherwise = i
            i1 = skipSp c0
        in if i1 == 0 then 0
           else let k = clsOf (T.index line (i1 - 1))
                    skipC i | i > 0 && clsOf (T.index line (i - 1)) == k = skipC (i - 1)
                            | otherwise = i
                in skipC i1
      wordRightRef line c0 =
        let n = T.length line
            k = clsOf (T.index line c0)
            skipC i | i < n && clsOf (T.index line i) == k = skipC (i + 1)
                    | otherwise = i
            i1 = if fst k then c0 else skipC c0
            skipSp i | i < n && isSpace (T.index line i) = skipSp (i + 1)
                     | otherwise = i
        in skipSp i1
      wordRangeRef line c =
        let n = T.length line
            anchor | c < n = Just (clsOf (T.index line c))
                   | c > 0 = Just (clsOf (T.index line (c - 1)))
                   | otherwise = Nothing
        in case anchor of
             Nothing -> (c, c)
             Just k ->
               let goL i | i > 0 && clsOf (T.index line (i - 1)) == k = goL (i - 1)
                         | otherwise = i
                   goR i | i < n && clsOf (T.index line i) == k = goR (i + 1)
                         | otherwise = i
               in (goL c, goR c)
      wordAlphabet = "ab _.,!\t\27721 __ zz"
      wordFuzzLine s0 = T.pack [ wordAlphabet !! (s' `mod` length wordAlphabet)
                               | s' <- take (3 + s0 `mod` 50) (iterate lcg (lcg s0)) ]
      wordCase s =
        let s1 = lcg s
            line = wordFuzzLine s1
            n = T.length line
            wb = fromText line
            c = s1 `div` 3 `mod` (n + 1)
            cL = max 1 (min n (s1 `div` 5 `mod` (n + 1)))   -- wordLeft needs c > 0
        in wordRight (Pos 0 c) wb
             == (if c >= n then Pos 0 c else Pos 0 (wordRightRef line c))
           && wordLeft (Pos 0 cL) wb == Pos 0 (wordLeftRef line cL)
           && wordRangeAt (Pos 0 c) wb
                == (let (a, b') = wordRangeRef line (min c n) in (Pos 0 a, Pos 0 b'))
  check "word hops match the reference" (all wordCase [1 .. 300 :: Int])
  -- Tripwire: quadratic word hops take ~minutes on a 300k-char token.
  let megaTok = fromText (T.replicate 300000 (T.pack "a"))
  checkEq "wordRight across a 300k token" (wordRight (Pos 0 0) megaTok) (Pos 0 300000)
  checkEq "wordLeft across a 300k token" (wordLeft (Pos 0 300000) megaTok) (Pos 0 0)
  checkEq "double-click range on a 300k token"
          (wordRangeAt (Pos 0 150000) megaTok) (Pos 0 0, Pos 0 300000)

  -- Whole-word search: boundary checks must not index from the line start ----
  let lmRef cs ww term line =    -- the old wordBoundary-indexing lineMatches
        let nterm = if cs then term else T.toLower term
            nline = if cs then line else T.toLower line
            len = T.length term
            nlen = T.length nterm
            bound i = (i == 0 || not (isW (T.index line (i - 1))))
                      && (i + len >= T.length line || not (isW (T.index line (i + len))))
            isW ch = isAlphaNum ch || ch == '_'
            go off t =
              let (pre, rest) = T.breakOn nterm t
              in if T.null rest then []
                 else let i = off + T.length pre
                      in if not ww || bound i
                           then (i, len) : go (i + nlen) (T.drop nlen rest)
                           else go (i + 1) (T.drop 1 rest)
        in if T.null term then [] else go 0 nline
      lmAlphabet = "fo bar_ FO,x "
      lmLine s0 = T.pack [ lmAlphabet !! (s' `mod` length lmAlphabet)
                         | s' <- take (5 + s0 `mod` 60) (iterate lcg (lcg s0)) ]
      lmCase s =
        let s1 = lcg s
            line = lmLine s1
            term = T.pack (["fo", "o", "bar", "x", "FO"] !! (s1 `div` 7 `mod` 5))
            cs = even (s1 `div` 11)
            ww = even (s1 `div` 13)
        in S.lineMatches cs ww term line == lmRef cs ww term line
  check "whole-word lineMatches matches the reference" (all lmCase [1 .. 300 :: Int])
  -- Tripwire: this took ~40 seconds with per-candidate indexed boundary checks.
  checkEq "whole-word search over a 440KB line is linear"
          (length (S.lineMatches True True (T.pack "foo")
                     (T.replicate 40000 (T.pack "foo bar1 x ")))) 40000

  -- Wrap mode: long jumps recompute the top in O(screen), not O(distance²) ---
  let refWrapTop ed =            -- the old one-row-at-a-time adjust loop
        let th = loTextHeight (computeLayout ed)
            Pos l _ = edCursor ed
            adjust top | top < l && visualOffset ed top (edCursor ed) >= th = adjust (top + 1)
                       | otherwise = top
        in max 0 (adjust (min (edTop ed) l))
      wrapLines = T.intercalate (T.pack "\n")
        [ T.replicate (1 + (i * 13) `mod` 4) (T.pack "words go here and wrap about ")
        | i <- [0 .. 299 :: Int] ]
      edWrapBase = (setLoaded "w.txt" (mkLR (T.unpack wrapLines)) ed0) { edWordWrap = True }
      wrapCase s =
        let s1 = lcg s
            l = s1 `mod` 300
            c = (s1 `div` 7) `mod` (T.length (getLine' l (edBuffer edWrapBase)) + 1)
            ed' = edWrapBase { edCursor = Pos l c, edTop = (s1 `div` 11) `mod` 300 }
        in edTop (resize (24, 80) ed') == refWrapTop ed'
  check "wrap-mode scroll matches the reference" (all wrapCase [1 .. 150 :: Int])
  -- Tripwire: Ctrl+End on a big wrapped file locked up for minutes before.
  let edWrapBig = (setLoaded "big.txt" (mkLR (unlines (replicate 30000 "some words that wrap around the place here from time to time ok yes"))) ed0)
                    { edWordWrap = True }
      edWrapEnd = fst (update (KEnd ctrlOnly) edWrapBig)
  check "wrap Ctrl+End lands at the end instantly"
        (posLine (edCursor edWrapEnd) == 29999 && edTop edWrapEnd > 29900
         && visualOffset edWrapEnd (edTop edWrapEnd) (edCursor edWrapEnd)
              < loTextHeight (computeLayout edWrapEnd))

  -- The modified flag is exact on huge files too (no size cutoff): typing a
  -- character and deleting it again must clear the flag on a 60k-line buffer.
  let hugeTxt = T.intercalate (T.pack "\n") (replicate 60000 (T.pack "line of text"))
      edHuge = setLoaded "huge.txt" (LoadResult (fromText hugeTxt) LF Utf8 True False Nothing) ed0
      edHuge1 = fst (update (KChar 'x') edHuge)
      edHuge2 = fst (update KBackspace edHuge1)
  check "huge file: typing sets modified" (edModified edHuge1)
  check "huge file: deleting back clears modified" (not (edModified edHuge2))

  -- Long single lines --------------------------------------------------------
  -- wrapLine must agree with the (quadratic) reference implementation it
  -- replaced, across tabs, wide chars, control chars and break points.
  let wrapRef tabw width line
        | width <= 0  = [(0, T.length line)]
        | T.null line = [(0, 0)]
        | otherwise   = goW 0
        where
          n = T.length line
          goW start
            | start >= n = []
            | otherwise =
                let hardEnd = fitEnd start
                in if hardEnd >= n
                     then [(start, n)]
                     else let e = preferSpace start hardEnd
                          in (start, e) : goW e
          fitEnd start =
            let base = colToDisplay tabw start line
                loop e
                  | e < n && (colToDisplay tabw (e + 1) line - base) <= width = loop (e + 1)
                  | otherwise = e
            in loop (start + 1)
          preferSpace start hardEnd =
            case [ j | j <- [hardEnd, hardEnd - 1 .. start + 1]
                     , isSpace (T.index line (j - 1)) ] of
              (j : _) -> j
              []      -> max (start + 1) hardEnd
      wrapAlphabet = "ab \tc 汉x  y\x01z 字 w"
      wrapFuzzLine s0 = T.pack (go1 s0 (12 + s0 `mod` 50))
        where go1 _ 0 = []
              go1 s k = let s' = lcg s
                        in wrapAlphabet !! (s' `mod` length wrapAlphabet) : go1 s' (k - 1)
      wrapOK = and [ wrapLine 4 w ln == wrapRef 4 w ln
                   | s <- [1 .. 150], let ln = wrapFuzzLine s
                   , w <- [1, 2, 3, 5, 8, 13, 21, 34] ]
  check "wrapLine matches the reference on random lines" wrapOK
  -- Linear-time tripwire: this took minutes with the old quadratic wrapLine.
  check "wrapLine is linear on a 200k-char line"
        (not (null (wrapLine 4 78 (T.replicate 40000 (T.pack "ab cd ")))))
  -- Megalines are rendered unstyled but thread the lexer state through, so
  -- highlighting below them stays sane.
  let megaLines2 = [ T.pack "select /* open"
                   , T.replicate 30000 (T.pack "x")
                   , T.pack "line two" ]
      megaC = refreshHlCache SQL (Seq.fromList megaLines2) 2 Nothing
  checkEq "megaline threads lexer state through" (hlStateBefore megaC 2) StBlock
  checkEq "megaline itself is unstyled"
          (fst (lexLine SQL StBlock (T.replicate 30000 (T.pack "x")))) []
  -- Deep horizontal scroll expands only the window and shows the right slice.
  let longAscii = T.concat (replicate 10000 (T.pack "0123456789"))
      edLong = (setLoaded "long.txt" (mkLR (T.unpack longAscii)) ed0) { edLeft = 50000 }
      scrLong = renderEditor edLong
      loLong = computeLayout edLong
      cellL r c = scrCells scrLong A.! (r * scrW scrLong + c)
      sliceChars = [ cellChar (cellL (loTextTop loLong) (loTextLeft loLong + k)) | k <- [0 .. 9] ]
  checkEq "windowed expand: deep h-scroll shows the right slice" sliceChars "0123456789"
  -- A wide glyph straddling the left edge keeps its continuation sentinel at
  -- the boundary (same cells the unwindowed expansion produced).
  let wideLine = T.replicate 200 (T.pack "\27721")     -- 200 wide glyphs, 2 cells each
      edWide = (setLoaded "wide.txt" (mkLR (T.unpack wideLine)) ed0) { edLeft = 101 }
      scrWide = renderEditor edWide
      loWide = computeLayout edWide
      cellW k = cellChar (scrCells scrWide A.! (loTextTop loWide * scrW scrWide + loTextLeft loWide + k))
  checkEq "windowed expand: straddling wide glyph leaves its cont cell" (cellW 0) '\0'
  checkEq "windowed expand: next wide glyph starts after the boundary" (cellW 1) '\27721'

  -- Workspace search: pure matching --------------------------------------------
  checkEq "lineMatches basic" (S.lineMatches True False (T.pack "foo") (T.pack "a foo foo")) [(2,3),(6,3)]
  checkEq "lineMatches case-insensitive"
          (S.lineMatches False False (T.pack "foo") (T.pack "FOO foo")) [(0,3),(4,3)]
  checkEq "lineMatches case-sensitive skips"
          (S.lineMatches True False (T.pack "foo") (T.pack "FOO foo")) [(4,3)]
  checkEq "lineMatches whole-word"
          (S.lineMatches True True (T.pack "foo") (T.pack "foo food foo")) [(0,3),(9,3)]
  checkEq "lineMatches non-overlapping"
          (S.lineMatches True False (T.pack "aa") (T.pack "aaaa")) [(0,2),(2,2)]
  checkEq "lineMatches whole-word retries overlap"
          (S.lineMatches True True (T.pack "foo") (T.pack "foofoo foo")) [(7,3)]
  -- Regression: lineMatches must stay LINEAR in line length. Minified JS and
  -- .eps files carry multi-megabyte single lines; the old per-position scan was
  -- O(n²) there (days of CPU — an effective hang of the workspace search).
  -- This line is instant when linear and takes minutes if the quadratic scan
  -- ever comes back.
  let hugeLine = T.replicate 250000 (T.pack "ab") <> T.pack "needle"
                   <> T.replicate 250000 (T.pack "ba")
  checkEq "lineMatches is linear on a huge single line"
          (S.lineMatches False False (T.pack "needle") hugeLine) [(500000, 6)]
  let (fm, ftrunc, fcnt) = S.fileMatches True False (T.pack "x") (T.pack "x here\nno match\nx and x")
  checkEq "fileMatches lines" (map mLine fm) [0, 2]
  checkEq "fileMatches count" fcnt 3
  check "fileMatches not truncated" (not ftrunc)
  checkEq "fileMatches empty term" (let (a,_,c) = S.fileMatches True False (T.pack "") (T.pack "x") in (length a, c)) (0, 0)
  checkEq "fileMatches multiline term ignored"
          (let (a,_,_) = S.fileMatches True False (T.pack "a\nb") (T.pack "a\nb") in length a) 0

  -- Go to Definition ------------------------------------------------------------
  let defs lg nm ln = D.defLineCols lg (T.pack nm) (T.pack ln)
  -- Python
  checkEq "py def"          (defs LPython "helper" "def helper(x):") [(4,6)]
  checkEq "py async def"    (defs LPython "helper" "    async def helper(x):") [(14,6)]
  checkEq "py class"        (defs LPython "Helper" "class Helper(Base):") [(6,6)]
  checkEq "py call is not a def" (defs LPython "helper" "y = helper(x)") []
  checkEq "py prefixed name is not a match" (defs LPython "helper" "def my_helper(x):") []
  -- SQL (case-insensitive, schema-qualified)
  checkEq "sql create function" (defs LSql "member_award" "CREATE OR REPLACE FUNCTION member_award(mid INT)") [(27,12)]
  checkEq "sql qualified"       (defs LSql "member_award" "create function public.member_award(mid int)") [(23,12)]
  checkEq "sql case-folded name" (defs LSql "MEMBER_AWARD" "create or replace function member_award()") [(27,12)]
  checkEq "sql procedure"       (defs LSql "do_thing" "CREATE PROCEDURE do_thing()") [(17,8)]
  checkEq "sql select is not a def" (defs LSql "member_award" "SELECT member_award(1);") []
  checkEq "sql drop is not a def"   (defs LSql "member_award" "DROP FUNCTION member_award;") []
  -- JavaScript / TypeScript
  checkEq "js function"      (defs LJs "render" "export async function render(props) {") [(22,6)]
  checkEq "js const arrow"   (defs LJs "render" "const render = (props) => {") [(6,6)]
  checkEq "js object key fn" (defs LJs "render" "  render: async (e) => {") [(2,6)]
  checkEq "js class method"  (defs LJs "render" "  render(props) {") [(2,6)]
  checkEq "js class"         (defs LJs "Widget" "class Widget extends Base {") [(6,6)]
  checkEq "js call is not a def"    (defs LJs "render" "  render(props);") []
  checkEq "js compare is not a def" (defs LJs "render" "if (render === (a)) {") []
  checkEq "js keyword stmt is not a def" (defs LJs "if" "  if (x) {") []
  -- Haskell / shell
  checkEq "hs signature"   (defs LHaskell "update" "update :: Key -> Editor") [(0,6)]
  checkEq "hs equation"    (defs LHaskell "update" "update key ed = go") [(0,6)]
  checkEq "hs data"        (defs LHaskell "Editor" "data Editor = Editor") [(5,6)]
  checkEq "hs use is not a def" (defs LHaskell "update" "  let r = update k e") []
  checkEq "sh function"    (defs LShell "deploy" "deploy() {") [(0,6)]
  checkEq "langOf sql"     (D.langOf "/x/pl-member_award.sql") (Just LSql)
  checkEq "langOf tsx"     (D.langOf "/x/App.tsx") (Just LJs)
  checkEq "langOf none"    (D.langOf "/x/notes.txt") Nothing

  -- The picker flow: F12 on a call seeds from the open buffer, streams disk
  -- results in, and Enter jumps to the chosen definition.
  let mkLRd t = LoadResult (fromText (T.pack t)) LF Utf8 True False Nothing
      edWd = explorerStart "/proj" [("/proj/util.py", False, Just 3)] ed0
      edPy = (setLoaded "/proj/util.py" (mkLRd "def helper(x):\n    return x\n\nhelper(1)\n") edWd)
               { edPath = Just "/proj/util.py" }
      edOnCall = edPy { edCursor = Pos 3 2 }   -- cursor inside the call "helper(1)"
      (edDp, dpEffs) = update (KFn 12 noMods) edOnCall
      dpReqs = [ r | EffFindDefs r <- dpEffs ]
  checkEq "F12 emits a definition scan" (map dfName dpReqs) [T.pack "helper"]
  checkEq "F12 focuses the picker" (edFocus edDp) FDefPick
  checkEq "picker seeded from the open buffer"
          (maybe [] (map diLine . dpItems) (edDefPick edDp)) [0]
  let gen2 = maybe 0 dpGen (edDefPick edDp)
      frSql = FileResult "/proj/pl-helper.sql"
                [Match 12 [(27,6)] (T.pack "CREATE OR REPLACE FUNCTION helper()")] False False
      edDp2 = defFound gen2 frSql edDp
  checkEq "streamed definition appended"
          (maybe [] (map diPath . dpItems) (edDefPick edDp2)) ["/proj/util.py", "/proj/pl-helper.sql"]
  check "stale-gen definition dropped"
        (maybe 0 (length . dpItems) (edDefPick (defFound (gen2 - 1) frSql edDp2)) == 2)
  let edDp3 = defDone gen2 edDp2
  check "defDone clears running" (maybe True (not . dpRunning) (edDefPick edDp3))
  -- Down + Enter opens the second (SQL) definition.
  let edSel = fst (update (KArrow DDown noMods) edDp3)
      (edJump, jEffs) = update KEnter edSel
  check "Enter on a closed file emits EffOpen"
        (any (\e -> case e of EffOpen "/proj/pl-helper.sql" -> True; _ -> False) jEffs)
  check "picker closed after opening" (edDefPick edJump == Nothing)
  check "jump target recorded" (edPendingJump edJump == Just ("/proj/pl-helper.sql", 12, 27, 6))
  -- Esc dismisses; F12 on whitespace reports rather than opening a picker.
  checkEq "Esc closes the picker" (edFocus (fst (update KEsc edDp3))) FEdit
  let edBlank = edPy { edCursor = Pos 2 0 }    -- an empty line
      (edNoId, noIdEffs) = update (KFn 12 noMods) edBlank
  check "F12 with no identifier stays in the editor" (edFocus edNoId == FEdit && null [ () | EffFindDefs _ <- noIdEffs ])

  -- Globs / scope --------------------------------------------------------------
  -- The memoised glob must agree with the exponential backtracker it replaced
  -- (kept here as the reference — with the same path normalisation the
  -- 'globMatch' wrapper applies — on inputs small enough to terminate).
  let globRef pat path
        | '/' `elem` pat = goG (normG pat) (normG path)
        | otherwise      = goG (normG pat) (baseG (normG path))
        where
          normG = map toLower . dropSl
          dropSl ('/' : r) = dropSl r
          dropSl s' = s'
          baseG = reverse . takeWhile (/= '/') . reverse
          goG [] [] = True
          goG ('*' : '*' : ps) cs =
            goG ('*' : dropStars ps) cs || anyTail (goG ps) cs
            where dropStars ('*' : r) = dropStars r
                  dropStars r = r
          goG ('*' : ps) cs =
            goG ps cs || case cs of
                           (c : cs') | c /= '/' -> goG ('*' : ps) cs'
                           _                    -> False
          goG ('?' : ps) (c : cs) | c /= '/' = goG ps cs
          goG ('/' : ps) ('/' : cs) = goG ps cs
          goG (p : ps) (c : cs) | p == c = goG ps cs
          goG _ _ = False
          anyTail f cs = f cs || case cs of { (_ : cs') -> anyTail f cs'; [] -> False }
      globPatAlpha = "ab*?/*"
      globPathAlpha = "aab/b"
      mkStr alpha len s0 = [ alpha !! (s' `mod` length alpha)
                           | s' <- take len (iterate lcg (lcg s0)) ]
      globCase s =
        let s1 = lcg s
            pat = mkStr globPatAlpha (1 + s1 `mod` 9) s1
            pth = mkStr globPathAlpha (1 + (s1 `div` 7) `mod` 12) (lcg s1)
        in S.globMatch pat pth == globRef pat pth
  check "glob matches the reference on 400 random cases" (all globCase [1 .. 400 :: Int])
  -- Tripwire: exponential backtracking took seconds-to-minutes on these.
  check "glob with many stars is linear (miss)"
        (not (S.globMatch "*a*a*a*a*a*a*a*a*a*b" (replicate 60 'a' ++ ".txt")))
  check "glob with many stars is linear (hit)"
        (S.globMatch "*a*a*a*a*a*a*a*a*a*b" (replicate 60 'a' ++ "b"))
  check "glob *.hs matches basename" (S.globMatch "*.hs" "src/Foo.hs")
  check "glob *.hs rejects .js" (not (S.globMatch "*.hs" "src/Foo.js"))
  check "glob ** spans segments" (S.globMatch "src/**/*.hs" "src/a/b/Foo.hs")
  check "glob ? single char" (S.globMatch "a?c.txt" "a-c.txt")
  check "glob dir name" (S.globMatch "node_modules" "node_modules")
  checkEq "parseGlobs splits" (S.parseGlobs (T.pack "*.hs, src/**")) ["*.hs", "src/**"]
  check "pathIncluded default-excludes node_modules"
        (not (S.pathIncluded [] [] "node_modules/x.js"))
  check "pathIncluded honours include" (S.pathIncluded ["*.hs"] [] "a/b/Foo.hs")
  check "pathIncluded rejects non-include" (not (S.pathIncluded ["*.hs"] [] "a/b/Foo.js"))
  check "pathIncluded honours exclude" (not (S.pathIncluded [] ["*.min.js"] "a/b.min.js"))
  check "dirPruned prunes .git" (S.dirPruned [] ".git")
  check "dirPruned prunes dotdirs" (S.dirPruned [] ".cache")
  check "dirPruned keeps src" (not (S.dirPruned [] "src"))
  -- The walker's skip-without-opening filter for well-known binary formats.
  check "binaryExtension skips images" (S.binaryExtension "photo.PNG")
  check "binaryExtension skips archives" (S.binaryExtension "backup.tar.gz")
  check "binaryExtension skips objects" (S.binaryExtension "Editor.o")
  check "binaryExtension keeps source files" (not (S.binaryExtension "Editor.hs"))
  check "binaryExtension keeps extensionless files" (not (S.binaryExtension "Makefile"))
  check "binaryExtension keeps dotfiles" (not (S.binaryExtension ".gitignore"))

  -- Editor integration: opening the panel & running a search -------------------
  let mkLR2 t = LoadResult (fromText (T.pack t)) LF Utf8 True False Nothing
      edW = explorerStart "/proj" [("/proj/a.txt", False, Just 3)] ed0
      edWF = (setLoaded "/proj/a.txt" (mkLR2 "hello world\nfind me here\nhello") edW) { edPath = Just "/proj/a.txt" }
      (edFind, _) = update (KCtrlShiftChar 'f') edWF
  check "Ctrl+Shift+F opens the search view" (searchViewActive edFind)
  check "search panel state exists" (maybe False (const True) (edSearch edFind))
  -- Typing into the Find field then Enter fires a background search effect.
  let edTyped = feed edFind [KChar 'h', KChar 'e', KChar 'l', KChar 'l', KChar 'o']
      (edRun, runEffs) = update KEnter edTyped
      startReq = [ r | EffStartSearch r <- runEffs ]
  checkEq "Enter starts one search" (length startReq) 1
  check "search marked running" (maybe False ssRunning (edSearch edRun))
  checkEq "find term captured" (map S.sqTerm startReq) [T.pack "hello"]

  -- Seeding open docs finds in-memory matches (using the started request).
  case startReq of
    (req : _) -> do
      let seeded = searchOpenDocs "/proj" req edRun
      checkEq "open-doc seed finds the file" (map frPath seeded) ["/proj/a.txt"]
      checkEq "open-doc seed match count" (sum (map S.fileMatchCount seeded)) 2
    [] -> check "no request produced" False

  -- Streaming disk results in, then finishing.
  let gen1 = maybe 0 ssGen (edSearch edRun)
      fr1  = FileResult "/proj/b.hs" [Match 4 [(0,5)] (T.pack "hello there")] False False
      edGot = searchFileFound gen1 fr1 edRun
      edDoneS = searchDone gen1 False edGot
  checkEq "streamed result inserted" (maybe [] S.resultPaths (edSearch edGot)) ["/proj/b.hs"]
  check "search done clears running" (maybe True (not . ssRunning) (edSearch edDoneS))
  check "done message summarises" (maybe False (\ss -> T.pack "result" `T.isInfixOf` ssMessage ss) (edSearch edDoneS))
  -- Stale-generation updates are ignored.
  check "stale gen dropped"
        (maybe True (\ss -> length (ssResults ss) == 1)
          (edSearch (searchFileFound (gen1 - 1) fr1 edDoneS)))

  -- Navigating results and opening a match jumps into the file.
  let edResults = edDoneS
      -- row 1 = the match under the file header (row 0 = the file header itself)
      edOnMatch = edResults { edSearch = fmap (S.setCursorResultRow 1) (edSearch edResults) }
      (edOpened, openEffs2) = update KEnter edOnMatch
  check "Enter on a match opens its file"
        (any (\eff -> case eff of EffOpen "/proj/b.hs" -> True; _ -> False) openEffs2)
  check "opening a match records a pending jump"
        (maybe False (const True) (edPendingJump edOpened))

  -- applyPendingJump moves the cursor once the file is active.
  let edPend = (setLoadedText (T.pack "line0\nline1\nline2\nline3\nhello there") ed0)
                 { edPath = Just "/proj/b.hs", edPendingJump = Just ("/proj/b.hs", 4, 0, 5) }
      edJumped = applyPendingJump edPend
  checkEq "pending jump lands on the match line" (posLine (edCursor edJumped)) 4
  check "pending jump cleared" (edPendingJump edJumped == Nothing)
  check "pending jump selects the match" (maybe False (const True) (edSelAnchor edJumped))

  -- Replace across the workspace: open docs edited in-buffer, closed on disk.
  let edRepl0 = (setLoaded "/proj/a.txt" (mkLR2 "foo bar foo") edW) { edPath = Just "/proj/a.txt" }
      ssR = (S.newSearchState "/proj")
              { ssFind = S.mkField (T.pack "foo"), ssReplace = S.mkField (T.pack "X")
              , ssShowReplace = True
              , ssResults = Seq.fromList
                            [ FileResult "/proj/a.txt" [Match 0 [(0,3),(8,3)] (T.pack "foo bar foo")] False False
                            , FileResult "/proj/closed.txt" [Match 0 [(0,3)] (T.pack "foo")] False False ] }
      edReplReady = edRepl0 { edSearch = Just ssR, edFocus = FSearch }
      (edRepld, replEffs) = update (KAltChar 'r') edReplReady   -- Alt+R = Replace All
      -- A small replace (<= 50 files) stages the closed files as unsaved tabs.
      stageReq = [ r | EffStageReplace r <- replEffs ]
  checkEq "open doc replaced in buffer" (getLine' 0 (edBuffer edRepld)) (T.pack "X bar X")
  check "active doc marked modified after replace" (edModified edRepld)
  -- The replace is undoable on the active document (after leaving the panel).
  checkEq "workspace replace is undoable"
          (getLine' 0 (edBuffer (fst (update (KCtrlChar 'z') (fst (update KEsc edRepld))))))
          (T.pack "foo bar foo")
  checkEq "closed file staged (opened) for replace" (map rrPaths stageReq) [["/proj/closed.txt"]]
  checkEq "staged replace carries open count" (map rrOpenCount stageReq) [2]

  -- Per-file replace: Ctrl+Enter on one result row touches only that file.
  let edOnFile = edReplReady { edSearch = fmap (S.setCursorResultRow 0) (edSearch edReplReady) }
      (edRF, rfEffs) = update KModEnter edOnFile   -- Ctrl/Shift+Enter on the a.txt row
      rfStage = [ r | EffStageReplace r <- rfEffs ]
  checkEq "per-file replace edits the selected open file" (getLine' 0 (edBuffer edRF)) (T.pack "X bar X")
  checkEq "per-file replace leaves other files alone (no closed paths here)"
          (map rrPaths rfStage) [[]]

  -- The Replace All button is keyboard-focusable: Tab from the Replace field
  -- lands on it, and Enter/Space there triggers Replace All.
  let edOnRepl  = edReplReady { edSearch = fmap (S.setCursorField SFReplace) (edSearch edReplReady) }
      edTabbed  = fst (update KTab edOnRepl)
  check "Tab from Replace field lands on the Replace All button"
        (maybe False S.focusedReplaceAll (edSearch edTabbed))
  let (_, btnEffs) = update KEnter edTabbed
  check "Enter on the focused button runs Replace All" (not (null [ () | EffStageReplace _ <- btnEffs ]))
  let (_, spcEffs) = update (KChar ' ') edTabbed
  check "Space on the focused button runs Replace All" (not (null [ () | EffStageReplace _ <- spcEffs ]))
  -- With Replace hidden there is no button in the focus ring.
  check "no Replace All button when replace is hidden"
        (maybe True (not . S.focusedReplaceAll)
          (edSearch (edReplReady { edSearch = fmap (\s -> s { ssShowReplace = False, ssCursor = 2 })
                                              (edSearch edReplReady) })))

  -- Ctrl+Shift+H shows the Replace row; Ctrl+Shift+F hides it again (so going
  -- "back to find" can't leave a Replace All primed by accident).
  let edShowR = fst (update (KCtrlShiftChar 'h') edWF)
      edHideR = fst (update (KCtrlShiftChar 'f') edShowR)
  check "Ctrl+Shift+H shows the replace row" (maybe False ssShowReplace (edSearch edShowR))
  check "Ctrl+Shift+F hides the replace row"  (maybe True (not . ssShowReplace) (edSearch edHideR))

  -- Leaving the search view gracefully: making a document the active view must
  -- dismiss the panel (or keystrokes would edit a buffer hidden behind it), and
  -- a click on the empty area below the results closes it like Esc.
  check "search view is up before opening a file" (searchViewActive edFind)
  let edOpenB = setLoadedNew "/proj/b.hs" (mkLR2 "module B where") edFind
  check "opening a new file dismisses the search view" (not (searchViewActive edOpenB))
  checkEq "opening a new file focuses the editor" (edFocus edOpenB) FEdit
  -- a.txt is still open, so this goes through switch-to-open / restoreDoc.
  let edSwitch = setLoadedNew "/proj/a.txt" (mkLR2 "ignored") (fst (update (KCtrlShiftChar 'f') edOpenB))
  check "switching to an open file dismisses the search view" (not (searchViewActive edSwitch))
  -- Esc from the explorer returns focus to the search view while it is the one
  -- showing (a second Esc there closes it), not to the hidden document.
  let edEscExp = fst (update KEsc edFind { edFocus = FExplorer })
  checkEq "Esc in the explorer refocuses the visible search view" (edFocus edEscExp) FSearch
  check "which stays drawn" (searchViewActive edEscExp)
  checkEq "Esc in the explorer returns to the editor when no search is up"
          (edFocus (fst (update KEsc edFind { edSearchMode = False, edFocus = FExplorer }))) FEdit
  -- A left click below the last result row leaves the search view.
  let loS = computeLayout edFind
      deadClick = KMouse (MouseEvent MBLeft (loContentLeft loS + 5)
                           (loTextTop loS + loTextHeight loS - 2) True False noMods 1)
      edDeadClk = fst (update deadClick edFind)
  check "click on the empty results area closes the search view" (not (searchViewActive edDeadClk))
  checkEq "and returns focus to the editor" (edFocus edDeadClk) FEdit

  -- Replace All over more than 10 files asks for confirmation first (no immediate
  -- on-disk effect); confirming it then performs the replace.
  let manyR = [ FileResult ("/proj/f" ++ show i ++ ".txt") [Match 0 [(0,3)] (T.pack "foo")] False False
              | i <- [1 .. 11 :: Int] ]
      ssMany = (S.newSearchState "/proj") { ssFind = S.mkField (T.pack "foo")
                 , ssReplace = S.mkField (T.pack "X"), ssShowReplace = True, ssResults = Seq.fromList manyR }
      edMany = ed0 { edSearch = Just ssMany, edFocus = FSearch, edSearchMode = True }
      (edCfm, cfmEffs) = update (KAltChar 'r') edMany   -- Alt+R
  check "big replace defers (no immediate on-disk effect)" (null [ () | EffReplaceOnDisk _ <- cfmEffs ])
  check "big replace opens a confirm dialog"
        (maybe False ((== DKConfirmReplaceAll) . dlgKind) (edDialog edCfm))
  let (_, cfmDo) = update KEnter edCfm    -- Enter = the default "Replace All" button
  checkEq "confirming stages all 11 files" (length [ rrPaths r | EffStageReplace r <- cfmDo, length (rrPaths r) == 11 ]) 1

  -- A very large replace (> 50 files) falls back to a direct on-disk rewrite.
  let bigR = [ FileResult ("/proj/g" ++ show i ++ ".txt") [Match 0 [(0,3)] (T.pack "foo")] False False
             | i <- [1 .. 60 :: Int] ]
      ssBig = (S.newSearchState "/proj") { ssFind = S.mkField (T.pack "foo")
                , ssReplace = S.mkField (T.pack "X"), ssShowReplace = True, ssResults = Seq.fromList bigR }
      edBig2 = ed0 { edSearch = Just ssBig, edFocus = FSearch, edSearchMode = True }
      (edBigC, _) = update (KAltChar 'r') edBig2     -- confirm dialog (60 > 10)
      (_, bigDo) = update KEnter edBigC              -- confirm
  check "very large replace (>50 files) writes to disk, not staged"
        (not (null [ () | EffReplaceOnDisk _ <- bigDo ]) && null [ () | EffStageReplace _ <- bigDo ])

  -- Staging a closed file opens it as an unsaved doc with the change applied;
  -- Save All then marks it saved.
  let substFooBar = replaceSubst False False False (T.pack "foo") (T.pack "BAR")
      (edStg, stgN) = addStagedDoc "/proj/new.txt" (mkLR2 "foo and foo") substFooBar ed0
  checkEq "addStagedDoc replacement count" stgN 2
  check "staged doc is added and modified" (any docModified (edAfter edStg))
  checkEq "staged doc has the replacement"
          (map (getLine' 0 . docBuffer) (filter ((== Just "/proj/new.txt") . docPath) (edAfter edStg)))
          [T.pack "BAR and BAR"]
  let edSavedAll = savedAll [("/proj/new.txt", Just (mt 100))] edStg
  check "Save All clears the staged doc's modified flag" (not (any docModified (edAfter edSavedAll)))
  checkEq "modifiedDocsToSave lists dirty titled docs"
          (map (\(p,_,_,_,_) -> p) (modifiedDocsToSave edStg)) ["/proj/new.txt"]

  -- Informational (single-button) dialogs dismiss on a click off the box; a
  -- multi-button confirm stays modal.
  let outsideClick = KMouse (MouseEvent MBLeft 0 0 True False noMods 1)
      edWarn = setError "blob.bin: binary file \x2014 cannot be edited" ed0
      edWarnDismissed = fst (update outsideClick edWarn)
  check "warning dialog present" (edDialog edWarn /= Nothing)
  check "single-button dialog dismissed by outside click" (edDialog edWarnDismissed == Nothing)
  let edDirty = fst (update (KChar 'z') ed0)             -- make the buffer modified
      (edQuit', _) = update (KCtrlChar 'q') edDirty      -- Ctrl+Q -> unsaved-changes confirm
      edQuitClicked = fst (update outsideClick edQuit')
  check "multi-button confirm present" (edDialog edQuit' /= Nothing)
  check "multi-button confirm NOT dismissed by outside click" (edDialog edQuitClicked /= Nothing)

  -- Regex engine ---------------------------------------------------------------
  let rxLM ci pat line = case Rx.compile ci (T.pack pat) of
        Right r -> Rx.lineMatches r (T.pack line); Left _ -> [(-1,-1)]
      rxRL ci pat tmpl line = case Rx.compile ci (T.pack pat) of
        Right r -> let (n,o) = Rx.replaceLine r (T.pack tmpl) (T.pack line) in (n, T.unpack o)
        Left _  -> (-1, "")
  checkEq "rx literal"      (rxLM False "foo" "a foo foo") [(2,3),(6,3)]
  checkEq "rx dot-star"     (rxLM False "a.*b" "axxb yb") [(0,7)]   -- greedy: longest
  checkEq "rx digit+"       (rxLM False "\\d+" "ab12cd345") [(2,2),(6,3)]
  checkEq "rx class range"  (rxLM False "[a-c]+" "aXbc") [(0,1),(2,2)]
  checkEq "rx neg class"    (rxLM False "[^0-9 ]+" "ab 12 cd") [(0,2),(6,2)]
  checkEq "rx anchors"      (rxLM False "^foo$" "foo") [(0,3)]
  checkEq "rx anchors miss" (rxLM False "^foo$" "xfoo") []
  checkEq "rx alternation"  (rxLM False "cat|dog" "cat dog") [(0,3),(4,3)]
  checkEq "rx group+"       (rxLM False "(ab)+" "ababc") [(0,4)]
  checkEq "rx optional"     (rxLM False "colou?r" "color colour") [(0,5),(6,6)]
  checkEq "rx wordbound"    (rxLM False "\\bfoo\\b" "foo food foo") [(0,3),(9,3)]
  checkEq "rx brace"        (rxLM False "a{2,3}" "a aa aaaa") [(2,2),(5,3)]
  checkEq "rx case-insens"  (rxLM True "foo" "FOO Foo") [(0,3),(4,3)]
  checkEq "rx lazy"         (rxLM False "<.*?>" "<a><b>") [(0,3),(3,3)]
  check   "rx invalid"      (isLeft (Rx.compile False (T.pack "a(")))
  -- The Pike VM is linear-time: a catastrophically-backtracking pattern is
  -- instant, and — unlike the old step-budgeted backtracker — its matches are
  -- never silently dropped. (A backtracker would need ~2^100 steps here.)
  checkEq "rx (a+)+b finds its match (no budget loss)"
          (rxLM False "(a+)+b" (replicate 100 'a' ++ "b")) [(0, 101)]
  checkEq "rx (a+)+b no-match is instant"
          (rxLM False "(a+)+b" (replicate 100 'a')) []
  -- Long minified lines are searchable now (the old 20k-char cap skipped them).
  checkEq "rx long line searched"
          (rxLM False "nee+dle" (replicate 50000 'x' ++ "needle")) [(50000, 6)]
  -- Priority semantics preserved from the backtracker.
  checkEq "rx first alternative wins" (rxLM False "ab|a" "ab") [(0,2)]
  checkEq "rx lazy brace" (rxLM False "a{2,4}?" "aaaa") [(0,2),(2,2)]
  checkEq "rx big brace expands and matches"
          (rxLM False "a{5000}" (replicate 5000 'a')) [(0, 5000)]
  check   "rx absurd brace nesting rejected"
          (isLeft (Rx.compile False (T.pack "(a{9999}){9999}")))
  checkEq "rx capture in repetition keeps last" (rxRL False "(\\w)+" "$1" "abc x") (2, "c x")
  checkEq "rx replace grp"  (rxRL False "(\\w+)@(\\w+)" "$2.$1" "user@host x") (1, "host.user x")
  checkEq "rx replace all"  (rxRL False "\\d" "#" "a1b2") (2, "a#b#")
  -- Regex wired through the file search + replace path.
  let (rms, _, rcnt) = S.fileMatchesM
        (either (error "bad") id (S.compileMatcher True False True (T.pack "f\\w+")))
        (T.pack "foo bar\nno\nfizz here")
  checkEq "regex fileMatchesM lines" (map mLine rms) [0, 2]
  checkEq "regex fileMatchesM count" rcnt 2
  checkEq "regex whole-text replace"
          (S.regexReplaceText (either (error "bad") id (Rx.compile False (T.pack "\\d+")))
             (T.pack "N") (T.pack "a1\r\nb22\r\n"))
          (2, T.pack "aN\r\nbN\r\n")
  -- A regex search in the editor runs and finds; a bad regex is reported.
  let edRx0 = (setLoaded "/proj/a.txt" (mkLR2 "val = foo123\nno match\nbar456 x") edW) { edPath = Just "/proj/a.txt" }
      (edRxOpen, _) = update (KCtrlShiftChar 'f') edRx0
      -- toggle regex on (Alt+X), type a pattern, Enter
      edRxOn = fst (update (KAltChar 'x') edRxOpen)
      edRxTyped = feed edRxOn [KChar '\\', KChar 'w', KChar '+', KChar '\\', KChar 'd', KChar '+']
      (edRxRun, rxEffs) = update KEnter edRxTyped
      rxReq = [ r | EffStartSearch r <- rxEffs ]
  check "regex toggle set" (maybe False ssRegex (edSearch edRxOn))
  checkEq "regex search dispatched with regex flag" (map S.sqRegex rxReq) [True]
  case rxReq of
    (r : _) -> checkEq "regex seed finds identifiers"
                 (sum (map S.fileMatchCount (searchOpenDocs "/proj" r edRxRun))) 2
    [] -> check "regex req produced" False
  -- A malformed regex is reported, and no search effect is emitted.
  let edBad = fst (update KEnter (feed (fst (update (KAltChar 'x') (fst (update (KCtrlShiftChar 'f') edRx0))))
                                       [KChar 'a', KChar '(']))
  check "bad regex reported" (maybe False (\ss -> T.pack "Invalid regex" `T.isInfixOf` ssMessage ss) (edSearch edBad))

  -- Bulk quit: > 8 unsaved files ask once (Save All / Discard All), not per-file.
  let addDirty i e = fst (addStagedDoc ("/proj/f" ++ show i ++ ".txt") (mkLR2 "x") (\tx -> (1, tx)) e)
      edMany8  = foldl (flip addDirty) ed0 [1 .. 8 :: Int]
      edMany9  = foldl (flip addDirty) ed0 [1 .. 9 :: Int]
      (edQ8, _) = update (KCtrlChar 'q') edMany8
      (edQ9, _) = update (KCtrlChar 'q') edMany9
  check "8 unsaved files -> per-file prompt"
        (maybe False ((== DKConfirmQuit) . dlgKind) (edDialog edQ8))
  check "9 unsaved files -> single bulk prompt"
        (maybe False ((== DKConfirmQuitAll) . dlgKind) (edDialog edQ9))
  check "bulk quit dialog reports the count"
        (maybe False (\dl -> T.pack "9 files" `T.isInfixOf` dlgMessage dl) (edDialog edQ9))
  -- Discard All (2nd button) quits immediately; Save All emits a batch save.
  let edDiscardAll = fst (update KEnter (fst (update KTab edQ9)))   -- Tab to "Discard All", Enter
  check "Discard All quits" (edQuit edDiscardAll)
  let (_, saveAllEffs) = update KEnter edQ9                         -- default button = "Save All"
  check "Save All emits EffSaveAll" (not (null [ () | EffSaveAll <- saveAllEffs ]))
  -- Cancel (3rd button) aborts the quit.
  let edCancelQuit = fst (update KEnter (feed edQ9 [KTab, KTab]))
  check "Cancel keeps the editor open" (not (edQuit edCancelQuit) && edDialog edCancelQuit == Nothing)

  -- Save All: only in the menu with >1 file open + unsaved changes, and it asks
  -- before writing.
  let hasSaveAll e = any (\me -> case me of MEItem _ _ MASaveAll -> True; _ -> False) (entriesFor e 0)
      ed1mod = fst (update (KChar 'z') ed0)                                   -- 1 file, modified
      ed2mod = fst (addStagedDoc "/proj/n2.txt" (mkLR2 "x") (\tx -> (1, tx)) ed1mod)  -- 2 files
  check "Save All hidden with a single file" (not (hasSaveAll ed1mod))
  check "Save All shown with >1 file and unsaved changes" (hasSaveAll ed2mod)
  check "Save All hidden when nothing is modified" (not (hasSaveAll ed0))
  let (edSAd, saEffs0) = saveAll ed2mod
  check "Save All prompts before writing" (maybe False ((== DKConfirmSaveAll) . dlgKind) (edDialog edSAd))
  check "Save All prompt emits no effect yet" (null saEffs0)
  let (_, saEffs1) = update KEnter edSAd
  check "confirming Save All emits the batch save" (not (null [ () | EffSaveAll <- saEffs1 ]))
  let (edSAcancel, _) = update KEnter (fst (update KTab edSAd))   -- Tab to Cancel, Enter
  check "cancelling Save All writes nothing" (edDialog edSAcancel == Nothing)

  -- F4 / F6 open workspace find / replace (the menu accelerators for them).
  check "F4 opens Find in Files" (searchViewActive (fst (update (KFn 4 noMods) edW)))
  check "F6 opens Replace in Files"
        (let (e6, _) = update (KFn 6 noMods) edW
         in searchViewActive e6 && maybe False ssShowReplace (edSearch e6))

  -- Image view: the in-file find options are hidden (no text to search); the
  -- workspace Find/Replace in Files stay. Their keyboard shortcuts are inert too.
  case decodeImage (mkBMP 2 2 [(255,0,0),(0,255,0),(0,0,255),(255,255,0)]) of
    Left _   -> check "image fixture decodes for find-menu test" False
    Right im -> do
      let edImg = imageLoaded "/pic.png" [(im, 0)] ed0
          findActs = [ a | MEItem _ _ a <- entriesFor edImg 2 ]   -- Find menu is index 2
      check "image view hides in-file Find/Replace/GoTo"
            (all (`notElem` findActs) [MAFind, MAFindNext, MAFindPrev, MAReplace, MAGoToLine])
      check "image view keeps Find/Replace in Files"
            (MAFindInFiles `elem` findActs && MAReplaceInFiles `elem` findActs)
      check "image view Find menu has no dangling separators"
            (findActs == [MAFindInFiles, MAReplaceInFiles, MANavBack, MANavFwd])
      check "image view: Ctrl+F does not open a Find dialog"
            (let (e, _) = update (KCtrlChar 'f') edImg in edDialog e == Nothing && not (searchViewActive e))
      check "image view: Ctrl+G does not open Go to Line"
            (edDialog (fst (update (KCtrlChar 'g') edImg)) == Nothing)
      -- A normal text file still shows all the find options.
      let edTxt = setLoaded "/x.txt" (mkLR2 "hi") ed0
          txtActs = [ a | MEItem _ _ a <- entriesFor edTxt 2 ]
      check "text view keeps in-file Find" (MAFind `elem` txtActs && MAGoToLine `elem` txtActs)

  -- Input parsing: Ctrl+Shift+F/H arrive as CSI u under the Kitty protocol.
  kcsF <- parseBytes (csiU 102 6)   -- 'f' with shift(1)+ctrl(4) -> mods param 6
  checkEq "CSI u ctrl+shift+f" kcsF (KCtrlShiftChar 'f')
  kcsH <- parseBytes (csiU 104 6)
  checkEq "CSI u ctrl+shift+h" kcsH (KCtrlShiftChar 'h')
  kcF <- parseBytes (csiU 102 5)    -- 'f' with just ctrl(4) -> mods param 5
  checkEq "CSI u ctrl+f (no shift) stays Ctrl" kcF (KCtrlChar 'f')

  -- The search view renders the SEARCH header.
  let scrSearch = renderEditor edRun
      searchText = [ cellChar (scrCells scrSearch A.! (r * scrW scrSearch + c))
                   | r <- [0 .. scrH scrSearch - 1], c <- [0 .. scrW scrSearch - 1] ]
  check "search view shows SEARCH header" ("SEARCH" `isInfixOf` searchText)

  -- About-box animation --------------------------------------------------------
  do
    let aw = 51
        inBounds w' ((r, c), _) = r >= 0 && r < aboutCanvasH && c >= 0 && c < w'
    check "about frames stay in the canvas"
      (all (all (inBounds aw) . aboutFrameCells aw) [0 .. aboutTotalFrames + 3])
    -- The animation is static once it has settled, and ends with the wordmark.
    let final = aboutFrameCells aw aboutTotalFrames
    check "about animation settles" (final == aboutFrameCells aw (aboutTotalFrames + 50))
    check "about final wordmark spans the canvas"
      (not (null final) &&
       maximum (map (snd . fst) final) - minimum (map (snd . fst) final) >= 30)
    -- A narrow canvas clips cells rather than emitting out-of-bounds ones.
    check "about narrow canvas clips"
      (all (all (inBounds 20) . aboutFrameCells 20) [0 .. aboutTotalFrames + 3])
    -- Opening About resets and animates; ticking stops at the last frame.
    let edAb = openAbout (newEditor (24, 80) defaultConfig)
    check "openAbout starts animating" (aboutAnimating edAb && edAboutTick edAb == 0)
    let edEnd = iterate tickAbout edAb !! (aboutTotalFrames + 10)
    check "about tick stops at the end"
      (edAboutTick edEnd == aboutTotalFrames && not (aboutAnimating edEnd))
    -- The About text reserves the blank canvas rows the overlay draws on.
    case edDialog edAb of
      Just dab -> check "aboutText reserves the canvas rows"
                    (all T.null (take aboutCanvasH (T.splitOn (T.pack "\n") (dlgMessage dab))))
      Nothing  -> check "openAbout opens a dialog" False

  -- Keyboard help card & the manual ---------------------------------------------
  do
    let ed0 = newEditor (30, 100) defaultConfig
        edH = fst (update (KFn 1 noMods) ed0)
    checkEq "F1 opens the help card" (dlgKind <$> edDialog edH) (Just DKHelp)
    checkEq "help preselects Close" (focusedButton =<< edDialog edH) (Just 1)
    case edDialog edH of
      Just dh -> check "help message reserves the canvas rows"
                   (all T.null (take helpCanvasH (T.splitOn (T.pack "\n") (dlgMessage dh))))
      Nothing -> check "openHelp opens a dialog" False
    -- The card's cells stay inside the canvas, even at a clipped width.
    let inCard w' ((r, c), _) = r >= 0 && r < helpCanvasH && c >= 0 && c < w'
    check "help card has content" (not (null (helpFrameCells helpCanvasMinW)))
    check "help card cells stay in bounds"
      (all (inCard helpCanvasMinW) (helpFrameCells helpCanvasMinW))
    check "help card narrow width clips" (all (inCard 40) (helpFrameCells 40))
    check "help card emits single-width glyphs only"
      (all ((== 1) . charWidth . cellChar . snd) (helpFrameCells helpCanvasMinW))
    -- Enter on the fresh card just closes it (Close is focused).
    let edClosed = fst (update KEnter edH)
    check "Enter closes the help card" (edDialog edClosed == Nothing)
    checkEq "Enter alone does not open the manual" (edPath edClosed) Nothing
    -- Tab reaches the Manual button; Enter there opens the manual read-only.
    let edMan = fst (update KEnter (fst (update KTab edH)))
    checkEq "Manual button opens the manual" (edPath edMan) (Just manualPath)
    check "manual is read-only" (edReadOnly edMan)
    check "manual has content" (not (isEmptyBuffer (edBuffer edMan)))
    check "manual closed the dialog" (edDialog edMan == Nothing)
    -- Editing is refused; the buffer and modified flag stay untouched.
    let edTyped = fst (update (KChar 'x') edMan)
    checkEq "manual refuses edits"
      (bufferToText LF False (edBuffer edTyped)) (bufferToText LF False (edBuffer edMan))
    check "manual stays unmodified" (not (edModified edTyped))
    -- Re-opening switches to the open copy rather than duplicating it.
    let edNew  = fst (update (KCtrlChar 'n') edMan)
        edBack = openManual edNew
    checkEq "re-opening the manual switches, not duplicates" (fileCount edBack) 2
    checkEq "re-opening lands on the manual" (edPath edBack) (Just manualPath)
    -- Closing the manual leaves no trace in the recent-files list.
    let edGone = fst (update (KCtrlChar 'w') edMan)
    check "manual leaves no recents entry" (null (edRecent edGone))

  -- Config file ----------------------------------------------------------------
  do
    let parsed txt = parseConfigText (T.pack txt) defaultConfig
    checkEq "config defaults untouched by empty" (fst (parsed "")) defaultConfig
    let (c1, w1) = parsed "tab-width = 8\nindent = spaces\nword-wrap = yes\n# comment\n\nline-numbers = on"
    checkEq "config tab-width" (cfgTabWidth c1) 8
    checkEq "config indent spaces" (cfgTabsToSpaces c1) True
    checkEq "config word-wrap" (cfgWordWrap c1) True
    checkEq "config line-numbers" (cfgLineNumbers c1) True
    checkEq "config no warnings" w1 []
    let (c2, w2) = parsed "tab-width = 99\nbogus = 1\nauto-indent = false\nnonsense"
    checkEq "config bad value keeps default" (cfgTabWidth c2) 4
    checkEq "config later keys still apply" (cfgAutoIndent c2) False
    checkEq "config warning count" (length w2) 3
    check "config warnings carry line numbers"
      (any ("line 1:" `isInfixOf`) w2 && any ("line 2:" `isInfixOf`) w2
       && any ("line 4:" `isInfixOf`) w2)
    let (c3, _) = parsed "whitespace = true  # trailing comment"
    checkEq "config inline comment" (cfgShowWhitespace c3) True
    -- Config flows into the fresh editor.
    let edC = newEditor (24, 80) c1
    check "config drives editor toggles"
      (edWordWrap edC && edShowLineNumbers edC && tabWidthOf edC == 8)

  -- Recent files ---------------------------------------------------------------
  do
    let rt = T.pack "12:5:/tmp/a.txt\n1:1:/tmp/b.hs\nbroken line\n0:0:\n3:9:/tmp/with:colon.txt\n"
        rs = parseRecentText rt
    checkEq "recent parse count" (length rs) 3
    checkEq "recent parse entry" (head rs) (RecentEntry "/tmp/a.txt" 11 4)
    checkEq "recent path may contain colons" (rePath (rs !! 2)) "/tmp/with:colon.txt"
    checkEq "recent roundtrip" (parseRecentText (renderRecentText rs)) rs
    check "recent list is capped"
      (length (parseRecentText (T.unlines
        [ T.pack ("1:1:/f" ++ show i) | i <- [1 .. 200 :: Int] ])) == 50)

    -- touch/record ordering and the cursor-restore on load.
    let ed0 = newEditor (24, 80) defaultConfig
        edR = recordRecent "/tmp/b.hs" (Pos 2 3)
                (recordRecent "/tmp/a.txt" (Pos 9 1) ed0)
    checkEq "recent most-recent-first" (map rePath (edRecent edR)) ["/tmp/b.hs", "/tmp/a.txt"]
    let edR2 = touchRecent "/tmp/a.txt" edR
    checkEq "touch moves to front, keeps pos"
      (head (edRecent edR2)) (RecentEntry "/tmp/a.txt" 9 1)
    let lr = loadFromBytes False Nothing (TE.encodeUtf8 (T.unlines (replicate 30 (T.pack "line here"))))
        edL = setLoaded "/tmp/a.txt" lr edR2
    checkEq "setLoaded restores remembered cursor" (edCursor edL) (Pos 9 1)
    checkEq "setLoaded touches recents front" (rePath (head (edRecent edL))) "/tmp/a.txt"
    -- A position beyond the (new, shorter) file clamps instead of vanishing.
    let lrShort = loadFromBytes False Nothing (TE.encodeUtf8 (T.pack "one\ntwo"))
        edL2 = setLoaded "/tmp/a.txt" lrShort edR2
    checkEq "restored cursor clamps to buffer" (edCursor edL2) (Pos 1 1)

    -- Closing records the position; the File menu offers the closed file.
    let edClosed = fst (update (KCtrlChar 'w') edL)
    checkEq "close records cursor into recents"
      (take 1 [ (reLine e, reCol e) | e <- edRecent edClosed, rePath e == "/tmp/a.txt" ])
      [(9, 1)]
    let fileEntries = entriesFor edClosed 0
        recentActs = [ a | MEItem _ _ a@(MARecentFile _) <- fileEntries ]
    check "File menu lists closed recents" (not (null recentActs))
    -- Open files are not offered again (the Window menu covers them).
    check "open files are not offered as recents"
      ("/tmp/a.txt" `notElem` recentMenuPaths edL)
    -- Activating a recent entry asks the driver to open it.
    let (_, effs) = update KEnter edClosed { edFocus = FMenu
                                           , edMenu = menuStateFor edClosed 0 (MARecentFile 0) }
    check "recent entry emits EffOpen"
      (any (\case EffOpen p -> p == "/tmp/a.txt"; _ -> False) effs)
    -- Persisting overlays the live cursor of open files.
    let edMoved = moveDown 3 edL2
    checkEq "recentsForPersist uses live cursor"
      (take 1 [ reLine e | e <- recentsForPersist edMoved, rePath e == "/tmp/a.txt" ])
      [posLine (edCursor edMoved)]

  -- Line operations --------------------------------------------------------------
  do
    let ed0 = (newEditor (24, 80) defaultConfig)
                { edBuffer = fromText (T.pack "alpha\nbravo\ncharlie\ndelta") }
        bufLinesOf e = [ getLine' i (edBuffer e) | i <- [0 .. lineCount (edBuffer e) - 1] ]
        at l c e = e { edCursor = Pos l c, edSelAnchor = Nothing }
        key k e = fst (update k e)

    -- Duplicate: Ctrl+D copies the line below and moves onto the copy.
    let edDup = key (KCtrlChar 'd') (at 1 3 ed0)
    checkEq "dup line content" (bufLinesOf edDup)
      (map T.pack ["alpha", "bravo", "bravo", "charlie", "delta"])
    checkEq "dup cursor follows down" (edCursor edDup) (Pos 2 3)
    -- Shift+Alt+Up duplicates but keeps the cursor on the upper copy.
    let edDupUp = key (KArrow DUp (Mods True True False)) (at 1 3 ed0)
    checkEq "dup-up content" (bufLinesOf edDupUp) (bufLinesOf edDup)
    checkEq "dup-up cursor stays" (edCursor edDupUp) (Pos 1 3)
    -- Duplicating a selection copies the whole block and keeps the selection.
    let edSel = (at 1 1 ed0) { edSelAnchor = Just (Pos 2 2) }
        edDupSel = key (KCtrlChar 'd') edSel
    checkEq "dup selection block" (bufLinesOf edDupSel)
      (map T.pack ["alpha", "bravo", "charlie", "bravo", "charlie", "delta"])
    checkEq "dup selection moves selection" (edSelAnchor edDupSel, edCursor edDupSel)
      (Just (Pos 4 2), Pos 3 1)

    -- Move line down / up, carrying the cursor; no-ops at the edges.
    let edMv = key (KArrow DDown (Mods False True False)) (at 1 2 ed0)
    checkEq "move down content" (bufLinesOf edMv)
      (map T.pack ["alpha", "charlie", "bravo", "delta"])
    checkEq "move down cursor" (edCursor edMv) (Pos 2 2)
    let edMvUp = key (KArrow DUp (Mods False True False)) edMv
    checkEq "move up restores" (bufLinesOf edMvUp) (bufLinesOf ed0)
    let edTop = key (KArrow DUp (Mods False True False)) (at 0 0 ed0)
    checkEq "move up at top is a no-op" (bufLinesOf edTop) (bufLinesOf ed0)
    checkEq "edge no-op pushes no undo" (undoDepth edTop) 0
    -- Held moves coalesce into a single undo step.
    let edMv2 = key (KArrow DDown (Mods False True False))
                  (key (KArrow DDown (Mods False True False)) (at 0 1 ed0))
    checkEq "two moves, one undo step" (undoDepth edMv2) 1
    checkEq "undo restores both moves" (bufLinesOf (key (KCtrlChar 'z') edMv2)) (bufLinesOf ed0)

    -- Delete line: middle, last, and only line.
    let edDel = key (KCtrlShiftChar 'k') (at 1 4 ed0)
    checkEq "delete middle line" (bufLinesOf edDel) (map T.pack ["alpha", "charlie", "delta"])
    checkEq "delete keeps column" (edCursor edDel) (Pos 1 4)
    let edDelLast = key (KCtrlShiftChar 'k') (at 3 0 ed0)
    checkEq "delete last line" (bufLinesOf edDelLast) (map T.pack ["alpha", "bravo", "charlie"])
    let edOnly = key (KCtrlShiftChar 'k')
                   ed0 { edBuffer = fromText (T.pack "solo"), edCursor = Pos 0 2 }
    checkEq "delete only line empties" (bufLinesOf edOnly) [T.pack ""]

    -- Join: seam whitespace collapses to one space, cursor on the seam.
    let edJ0 = ed0 { edBuffer = fromText (T.pack "foo   \n   bar\nbaz"), edCursor = Pos 0 0 }
        edJ = key (KAltChar 'j') edJ0
    checkEq "join collapses seam" (bufLinesOf edJ) (map T.pack ["foo bar", "baz"])
    checkEq "join cursor at seam" (edCursor edJ) (Pos 0 4)
    -- Joining with an empty side adds no stray space.
    let edJE = key (KAltChar 'j') ed0 { edBuffer = fromText (T.pack "\nxyz"), edCursor = Pos 0 0 }
    checkEq "join with empty line" (bufLinesOf edJE) [T.pack "xyz"]
    -- Join on the last line is a no-op.
    let edJL = key (KAltChar 'j') (at 3 0 ed0)
    checkEq "join at eof no-op" (bufLinesOf edJL) (bufLinesOf ed0)

    -- Blocked outside plain text: CSV mode leaves the grid alone and explains.
    let edCsvMode = setLoaded "/tmp/t.csv"
                      (loadFromBytes False Nothing (TE.encodeUtf8 (T.pack "a,b\nc,d")))
                      (newEditor (24, 80) defaultConfig)
        -- Ctrl+D is swallowed by the CSV handler (its own key set); the menu
        -- fallback (and Alt+J, which routes through runAction) explain instead.
        edCsvTry = key (KAltChar 'j') edCsvMode
    check "line ops blocked in table view"
      (T.pack "text view" `T.isInfixOf` edStatus edCsvTry)
    check "blocked op leaves the grid alone"
      ((csvToText <$> edCsv edCsvTry) == (csvToText <$> edCsv edCsvMode))
    -- The Edit menu hides the group in table view but shows it in text.
    let editIx = 1
    check "menu shows line ops in text"
      (any (\case MEItem _ _ MADuplicateLine -> True; _ -> False) (entriesFor ed0 editIx))
    check "menu hides line ops in table view"
      (not (any (\case MEItem _ _ MADuplicateLine -> True; _ -> False)
                (entriesFor edCsvMode editIx)))

  -- Toggle comment ---------------------------------------------------------------
  do
    let mkAt path txt l c = (newEditor (24, 80) defaultConfig)
          { edBuffer = fromText (T.pack txt), edPath = Just path, edCursor = Pos l c }
        bufLinesOf e = [ getLine' i (edBuffer e) | i <- [0 .. lineCount (edBuffer e) - 1] ]
        key k e = fst (update k e)
        ctrlSlash = key (KCtrlChar '_')   -- what a legacy terminal sends for Ctrl+/

    -- Python: comment, cursor shifts with its character; toggle back.
    let edPy = ctrlSlash (mkAt "/x/t.py" "def f():\n    return 1" 0 4)
    checkEq "comment python line" (bufLinesOf edPy) (map T.pack ["# def f():", "    return 1"])
    checkEq "comment shifts cursor" (edCursor edPy) (Pos 0 6)
    let edPy2 = ctrlSlash edPy
    checkEq "uncomment restores" (bufLinesOf edPy2) (map T.pack ["def f():", "    return 1"])
    checkEq "uncomment shifts back" (edCursor edPy2) (Pos 0 4)

    -- Selection: aligned at minimum indent, blank lines skipped; mixed
    -- commented/uncommented spans get commented (VS Code semantics).
    let src = "    a = 1\n\n        b = 2\n    # c = 3"
        edSel = (mkAt "/x/t.py" src 0 0) { edSelAnchor = Just (Pos 3 9), edCursor = Pos 0 0 }
        edC = ctrlSlash edSel
    checkEq "block comment aligned + blank skipped" (bufLinesOf edC)
      (map T.pack ["    # a = 1", "", "    #     b = 2", "    # # c = 3"])
    -- All-commented span uncomments.
    let edU = ctrlSlash edC { edSelAnchor = Just (Pos 3 11), edCursor = Pos 0 0 }
    checkEq "uncomment whole span" (bufLinesOf edU)
      (map T.pack ["    a = 1", "", "        b = 2", "    # c = 3"])

    -- Block-comment language (HTML): wrap then unwrap.
    let edH = ctrlSlash (mkAt "/x/p.html" "  <p>hello</p>" 0 5)
    checkEq "html wraps in block comment" (bufLinesOf edH) [T.pack "  <!-- <p>hello</p> -->"]
    checkEq "html unwrap restores" (bufLinesOf (ctrlSlash edH)) [T.pack "  <p>hello</p>"]

    -- SQL uses --; unknown file types explain themselves.
    checkEq "sql comment prefix" (bufLinesOf (ctrlSlash (mkAt "/x/q.sql" "select 1" 0 0)))
      [T.pack "-- select 1"]
    let edTxt = ctrlSlash (mkAt "/x/notes.txt" "plain" 0 0)
    check "txt reports no comment syntax" (T.pack "No comment syntax" `T.isInfixOf` edStatus edTxt)
    checkEq "txt buffer untouched" (bufLinesOf edTxt) [T.pack "plain"]

    -- The Kitty-protocol form of Ctrl+/ decodes to the same key family.
    kSlash <- parseBytes ([0x1b] ++ map (fromIntegral . fromEnum) "[47;5u")
    checkEq "kitty Ctrl+/ decodes" kSlash (KCtrlChar '/')
    kLegacy <- parseBytes [0x1f]
    checkEq "legacy Ctrl+/ decodes" kLegacy (KCtrlChar '_')

  -- Find live highlight + counters -------------------------------------------------
  do
    let key k e = fst (update k e)
        typeAll :: String -> Editor -> Editor
        typeAll s e = foldl (\acc c -> key' (KChar c) acc) e s
          where key' k x = fst (update k x)
        edBase = (newEditor (24, 80) defaultConfig)
                   { edBuffer = fromText (T.pack "cat dog cat\nbird\ncatalog") }
        -- Ctrl+F opens Find; type a fresh term over the (empty) seeded one.
        edF = typeAll "cat" (key (KCtrlChar 'f') edBase)
    case edDialog edF of
      Nothing -> check "find dialog open" False
      Just d  -> do
        checkEq "live match count in dialog" (dlgMessage d) (T.pack "3 matches")
        -- Whole-word only counts the standalone "cat"s once toggled on... via spans:
        checkEq "live spans on a line" (liveMatchSpans edF (T.pack "cat dog cat"))
          [(0, 3), (8, 11)]
    let edNo = typeAll "zebra" (key (KCtrlChar 'f') edBase)
    check "no-match message" ((dlgMessage <$> edDialog edNo) == Just (T.pack "No matches"))
    -- The rendered screen paints every match with the find-match style.
    let scrF = renderEditor edF
        matchCells = [ () | i <- [0 .. scrW scrF * scrH scrF - 1]
                          , cellStyle (scrCells scrF A.! i) == Style Black Yellow attrNone ]
    check "matches highlighted on screen" (length matchCells >= 6)  -- 2 visible "cat"s + "cat" in catalog
    -- Confirming the find reports the ordinal.
    let edGo = key KEnter edF
    checkEq "match ordinal in status" (edStatus edGo) (T.pack "Match 1 of 3")
    let edGo2 = key (KFn 3 noMods) edGo
    checkEq "F3 advances the ordinal" (edStatus edGo2) (T.pack "Match 2 of 3")

  -- Bracket matching ---------------------------------------------------------------
  do
    let bb = fromText (T.pack "f(a, [b,\n {c}]\n) end")
    -- On the opening paren: partner is the ')' two lines down.
    checkEq "bracket ( to )" (matchBracket (Pos 0 1) bb) (Just (Pos 0 1, Pos 2 0))
    -- On the closing paren, backwards across lines.
    checkEq "bracket ) to (" (matchBracket (Pos 2 0) bb) (Just (Pos 2 0, Pos 0 1))
    -- Nested same-kind brackets skip the inner pair.
    checkEq "bracket [ nests" (matchBracket (Pos 0 5) bb) (Just (Pos 0 5, Pos 1 4))
    -- The character *before* the cursor is used when the one at it is not a bracket.
    checkEq "bracket before cursor" (matchBracket (Pos 0 2) bb) (Just (Pos 0 1, Pos 2 0))
    -- No bracket near the cursor / unmatched bracket.
    checkEq "no bracket here" (matchBracket (Pos 2 3) bb) Nothing
    checkEq "unmatched open" (matchBracket (Pos 0 0) (fromText (T.pack "(abc"))) Nothing

    -- Ctrl+] jumps; the pair is exposed to the renderer.
    let edB = (newEditor (24, 80) defaultConfig) { edBuffer = bb, edCursor = Pos 0 1 }
        edJmp = fst (update (KCtrlChar ']') edB)
    checkEq "Ctrl+] jumps to partner" (edCursor edJmp) (Pos 2 0)
    checkEq "bracketPair highlights both" (bracketPair edB) [Pos 0 1, Pos 2 0]
    let edNoB = fst (update (KCtrlChar ']') edB { edCursor = Pos 2 3 })
    check "Ctrl+] reports no match" (T.pack "No matching bracket" `T.isInfixOf` edStatus edNoB)

  -- Line ending / BOM switching + status bar clicks -----------------------------
  do
    let key k e = fst (update k e)
        ed0 = (newEditor (24, 80) defaultConfig)
                { edBuffer = fromText (T.pack "one\ntwo"), edSavedBuffer = fromText (T.pack "one\ntwo")
                , edPath = Just "/tmp/eol.txt" }
        viewIx = 3
        actEntries e = [ (lbl, a) | MEItem lbl _ a <- entriesFor e viewIx ]
        runMenu a e = fst (update KEnter e { edFocus = FMenu, edMenu = menuStateFor e viewIx a })

    -- Menu shows the current value; activating switches it and dirties the file.
    check "menu shows LF" (any ((== T.pack "Line E&ndings: LF") . fst) (actEntries ed0))
    let edCr = runMenu MACycleLineEnding ed0
    checkEq "cycle to CRLF" (edLineEnding edCr) CRLF
    check "EOL change marks modified" (edModified edCr)
    check "menu shows CRLF" (any ((== T.pack "Line E&ndings: CRLF") . fst) (actEntries edCr))
    -- A text edit + undo must NOT clear the pending EOL change.
    let edTyped = key KBackspace (key (KChar 'x') edCr)
    check "undo keeps EOL-modified flag" (edModified edTyped)
    -- Saving records the new baseline and clears the flag.
    let (edSaved, _) = onSaved 8 Nothing edTyped
    check "save clears EOL-modified" (not (edModified edSaved) && edSavedEol edSaved == CRLF)
    -- Cycling back home on a clean file un-dirties it.
    let edBack = runMenu MACycleLineEnding (runMenu MACycleLineEnding ed0)
    check "cycling back is clean again" (not (edModified edBack))

    -- BOM toggle mirrors the same rules.
    let edBom = runMenu MAToggleBom ed0
    check "BOM toggle marks modified" (edEncoding edBom == Utf8Bom && edModified edBom)

    -- Status bar zones: clicking the LF cell switches the line ending, the INS
    -- cell toggles overwrite, and Ln/Col opens Go To Line.
    let (txt, zones) = statusRightInfo ed0
        start = 80 - length txt
        statusRow = 22   -- menu row + 21 text rows
        clickAt col e = key (KMouse (MouseEvent MBLeft col statusRow True False noMods 1)) e
        colOf z = head [ start + s | (s, _, zz) <- zones, zz == z ]
    checkEq "click LF switches EOL" (edLineEnding (clickAt (colOf SZLineEnding) ed0)) CRLF
    check "click INS toggles overwrite" (edOverwrite (clickAt (colOf SZOverwrite) ed0))
    check "click Ln/Col opens Go To"
      ((dlgKind <$> edDialog (clickAt (colOf SZGoTo) ed0)) == Just DKGoToLine)
    checkEq "click BOM zone toggles encoding" (edEncoding (clickAt (colOf SZEncoding) ed0)) Utf8Bom

  -- Save-time fixups (trim trailing whitespace / final newline) --------------------
  do
    let bufLinesOf e = [ getLine' i (edBuffer e) | i <- [0 .. lineCount (edBuffer e) - 1] ]
        cfgOn = defaultConfig { cfgTrimTrailingWs = True, cfgEnsureFinalNl = True }
        edD = (newEditor (24, 80) cfgOn)
                { edBuffer = fromText (T.pack "keep\ntrail   \n\ttabbed\t\t")
                , edFinalNewline = False, edCursor = Pos 1 8 }
        edFixed = applySaveFixups edD
    checkEq "trim strips trailing ws" (bufLinesOf edFixed)
      (map T.pack ["keep", "trail", "\ttabbed"])
    check "final newline forced on" (edFinalNewline edFixed)
    checkEq "cursor clamps after trim" (edCursor edFixed) (Pos 1 5)
    -- Undoable: one Ctrl+Z brings the whitespace back.
    let edUndone = fst (update (KCtrlChar 'z') edFixed)
    checkEq "trim is undoable" (bufLinesOf edUndone)
      (map T.pack ["keep", "trail   ", "\ttabbed\t\t"])
    -- Off by default: nothing changes.
    let edOff = applySaveFixups edD { edConfig = defaultConfig }
    checkEq "fixups off by default" (bufLinesOf edOff) (bufLinesOf edD)
    check "final newline untouched by default" (not (edFinalNewline edOff))
    -- No trailing ws: no undo checkpoint is pushed.
    let edClean = (newEditor (24, 80) cfgOn) { edBuffer = fromText (T.pack "clean") }
    checkEq "clean buffer pushes no undo" (undoDepth (applySaveFixups edClean)) 0
    -- CSV documents are left alone even with the options on.
    let edCsvD = setLoaded "/tmp/x.csv"
                   (loadFromBytes False Nothing (TE.encodeUtf8 (T.pack "a ,b\nc,d ")))
                   (newEditor (24, 80) cfgOn)
    check "csv exempt from trim"
      ((csvToText <$> edCsv (applySaveFixups edCsvD)) == (csvToText <$> edCsv edCsvD))
    -- Save All fixes background documents too (with their own undo step).
    let dirtyDoc = edD { edPath = Just "/tmp/a.txt", edModified = True }
        edMulti = (fst (update (KCtrlChar 'n') dirtyDoc)) { edConfig = cfgOn }
        edAllFixed = applySaveFixupsAll edMulti
    check "save-all trims zipper docs"
      (all (\d -> all (\ln -> T.stripEnd ln == ln)
                      [ getLine' i (docBuffer d) | i <- [0 .. lineCount (docBuffer d) - 1] ])
           (edBefore edAllFixed ++ edAfter edAllFixed))

  -- Quick open (Ctrl+P) -------------------------------------------------------------
  do
    let key k e = fst (update k e)
    -- Fuzzy matcher basics.
    check "fuzzy: subsequence matches" (isJust' (Q.fuzzyMatch "edi" "src/Editor.hs"))
    check "fuzzy: non-subsequence fails" (Q.fuzzyMatch "zzz" "src/Editor.hs" == Nothing)
    check "fuzzy: case-insensitive" (isJust' (Q.fuzzyMatch "EDI" "src/editor.hs"))
    -- Basename match beats a scattered directory match.
    let score q p = maybe (-1) fst (Q.fuzzyMatch (T.pack q) (T.pack p))
    check "fuzzy: basename beats scatter"
      (score "edit" "src/Editor.hs" > score "edit" "extra/dir/notes.txt")
    check "fuzzy: consecutive beats gaps"
      (score "app" "src/App.hs" > score "app" "a/p/p/x.txt")
    -- Positions returned for highlighting are the matched characters.
    case Q.fuzzyMatch "cfg" "src/ConfigFile.hs" of
      Just (_, ps) -> checkEq "fuzzy positions count" (length ps) 3
      Nothing      -> check "fuzzy cfg matches" False

    -- Model: streaming batches merge incrementally; query re-ranks.
    let qo0 = Q.newQuickOpen 1 "/w" [T.pack "recent.txt"] []
        qo1 = Q.qoAddFiles (map T.pack ["src/App.hs", "src/Editor.hs", "README.md"]) qo0
    checkEq "empty query: recents lead"
      (take 1 [ p | (_, p, _) <- qoMatches qo1 ]) [T.pack "recent.txt"]
    let qo2 = Q.qoEditField (\f -> foldl (flip S.fieldInsert) f ("edit" :: String)) qo1
    checkEq "query ranks the editor first"
      (take 1 [ p | (_, p, _) <- qoMatches qo2 ]) [T.pack "src/Editor.hs"]
    let qo3 = Q.qoAddFiles [T.pack "docs/editing.md"] qo2
    check "late batch merges into ranking"
      (T.pack "docs/editing.md" `elem` [ p | (_, p, _) <- qoMatches qo3 ])
    checkEq "total counts all matches" (qoTotal qo3) 2   -- Editor.hs + editing.md

    -- Editor wiring: Ctrl+P opens the picker and requests the walk; Enter
    -- opens the selection through EffOpen.
    let edW = (newEditor (24, 80) defaultConfig) { edPath = Just "/w/x.txt" }
        (edP, effsP) = update (KCtrlChar 'p') edW
    check "Ctrl+P opens the picker" (edFocus edP == FQuickOpen && isJust' (edQuickOpen edP))
    check "Ctrl+P requests the walk"
      (any (\case EffQuickOpen _ _ -> True; _ -> False) effsP)
    let gen = maybe 0 qoGen (edQuickOpen edP)
        edSeeded = quickFilesFound gen (map T.pack ["a.txt", "b/c.hs"]) (quickOpenSeed gen "/w" edP)
        (edPicked, effs2) = update KEnter edSeeded
    check "Enter opens the top match"
      (any (\case EffOpen p -> p == "/w/a.txt"; _ -> False) effs2)
    check "picker closes on pick" (edQuickOpen edPicked == Nothing)
    -- Esc dismisses; Ctrl+P toggles closed.
    check "Esc dismisses picker" (edQuickOpen (key KEsc edSeeded) == Nothing)
    check "Ctrl+P toggles closed" (edQuickOpen (key (KCtrlChar 'p') edSeeded) == Nothing)

  -- Navigation history (Alt+Left/Right) ---------------------------------------------
  do
    let key k e = fst (update k e)
        typeIn :: String -> Editor -> Editor
        typeIn s e = foldl (\acc c -> fst (update (KChar c) acc)) e s
        bigBuf = fromText (T.unlines [ T.pack ("line " ++ show i) | i <- [1 .. 200 :: Int] ])
        ed0 = (newEditor (24, 80) defaultConfig)
                { edBuffer = bigBuf, edPath = Just "/w/big.txt" }
        altL = KArrow DLeft (Mods False True False)
        altR = KArrow DRight (Mods False True False)

    -- Go to Line pushes the origin; Alt+Left returns; Alt+Right re-jumps.
    let edGoto = key KEnter (typeIn "150" (key (KCtrlChar 'g') ed0))
    checkEq "goto moved" (posLine (edCursor edGoto)) 149
    checkEq "goto pushed one stop" (map nsPos (edNavBack edGoto)) [Pos 0 0]
    let (edBack, _) = update altL edGoto
    checkEq "Alt+Left returns to origin" (edCursor edBack) (Pos 0 0)
    checkEq "forward trail recorded" (map nsPos (edNavFwd edBack)) [Pos 149 0]
    let (edFwdE, _) = update altR edBack
    checkEq "Alt+Right re-visits" (posLine (edCursor edFwdE)) 149
    -- A short jump does not pollute the history.
    let edNear = key KEnter (typeIn "152" (key (KCtrlChar 'g') edFwdE))
    checkEq "near jump not recorded" (length (edNavBack edNear)) (length (edNavBack edFwdE))
    -- Ctrl+End pushes too, and empty stacks report politely.
    let edEnd = key (KEnd ctrlOnly) ed0
    check "Ctrl+End pushes a stop" (not (null (edNavBack edEnd)))
    let (edNoB, _) = update altL ed0
    check "empty back reports" (T.pack "No earlier" `T.isInfixOf` edStatus edNoB)
    -- Cross-file: switching files (Alt+1) records the origin; Alt+Left returns.
    let edTwo = setLoadedNew "/w/other.txt"
                  (loadFromBytes False Nothing (TE.encodeUtf8 (T.pack "abc")))
                  edGoto
        edSw = key (KAltChar '1') edTwo
    checkEq "Alt+1 switched to first file" (edPath edSw) (Just "/w/big.txt")
    let (edBack2, _) = update altL edSw
    checkEq "Alt+Left returns across files" (edPath edBack2) (Just "/w/other.txt")

  -- Explorer file management ---------------------------------------------------------
  do
    let key k e = fst (update k e)
        typeIn :: String -> Editor -> Editor
        typeIn s e = foldl (\acc c -> fst (update (KChar c) acc)) e s
        ents = [("/w/adir", True, Nothing), ("/w/file.txt", False, Just 3)]
        edX = explorerStart "/w" ents (newEditor (24, 80) defaultConfig)

    -- Insert on a selected directory prompts for a new entry inside it.
    let edNew = key (KInsert noMods) edX
    checkEq "new prompt opens" (dlgKind <$> edDialog edNew) (Just DKNewPath)
    check "new prompt names the target dir"
      (maybe False (T.isInfixOf (T.pack "/w/adir") . dlgMessage) (edDialog edNew))
    let (edC, effsC) = update KEnter (typeIn "notes.md" edNew)
    check "create emits the effect"
      (any (\case EffCreatePath p -> p == "/w/adir/notes.md"; _ -> False) effsC)
    checkEq "create returns to explorer" (edFocus edC) FExplorer
    -- A trailing slash flows through for folder creation.
    let (_, effsD) = update KEnter (typeIn "sub/" (key (KInsert noMods) edX))
    check "trailing slash kept for folders"
      (any (\case EffCreatePath p -> p == "/w/adir/sub/"; _ -> False) effsD)

    -- F2 renames the selected file (seeded with its name).
    let edSel = key (KArrow DDown noMods) edX      -- move to file.txt
        edRen = key (KFn 2 noMods) edSel
    checkEq "rename prompt opens" (dlgKind <$> edDialog edRen) (Just DKRename)
    checkEq "rename seeded with the old name"
      (fieldValue 0 <$> edDialog edRen) (Just (T.pack "file.txt"))
    let (_, effsR) = update KEnter (typeIn "2" edRen)
    check "rename emits old -> new"
      (any (\case EffRenamePath o n -> o == "/w/file.txt" && n == "/w/file.txt2"
                  _ -> False) effsR)
    -- Renaming to the same name is a no-op.
    let (_, effsSame) = update KEnter edRen
    check "unchanged rename is inert"
      (not (any (\case EffRenamePath _ _ -> True; _ -> False) effsSame))

    -- Delete asks first, with Cancel preselected so a stray Enter is safe.
    let edDel = key (KDelete noMods) edSel
    checkEq "delete asks" (dlgKind <$> edDialog edDel) (Just DKConfirmDelete)
    checkEq "cancel is preselected" (focusedButton =<< edDialog edDel) (Just 1)
    let (_, effsP) = update KEnter edDel
    check "enter on the fresh dialog deletes nothing"
      (not (any (\case EffDeletePath _ -> True; _ -> False) effsP))
    let (_, effsX) = update KEnter (key KTab edDel)
    check "delete emits the effect"
      (any (\case EffDeletePath p -> p == "/w/file.txt"; _ -> False) effsX)
    -- Esc cancels back to the panel, nothing emitted.
    let (edEsc, effsE) = update KEsc edDel
    check "cancel emits nothing" (null effsE)
    checkEq "cancel returns to explorer" (edFocus edEsc) FExplorer

    -- renamePaths rewrites open documents, recents and history under a moved dir.
    let edDocs = (newEditor (24, 80) defaultConfig)
                   { edPath = Just "/w/adir/deep/a.hs"
                   , edRecent = [RecentEntry "/w/adir/deep/a.hs" 3 1, RecentEntry "/w/other" 0 0]
                   , edNavBack = [NavStop (Just "/w/adir/deep/a.hs") (Pos 1 0)] }
        edRw = renamePaths "/w/adir" "/w/bdir" edDocs
    checkEq "rename rewrites active path" (edPath edRw) (Just "/w/bdir/deep/a.hs")
    checkEq "rename rewrites recents"
      (map rePath (edRecent edRw)) ["/w/bdir/deep/a.hs", "/w/other"]
    checkEq "rename rewrites nav stops"
      (map nsPath (edNavBack edRw)) [Just "/w/bdir/deep/a.hs"]

  -- Find/replace input history --------------------------------------------------------
  do
    let key k e = fst (update k e)
        typeIn :: String -> Editor -> Editor
        typeIn s e = foldl (\acc c -> fst (update (KChar c) acc)) e s
        up = KArrow DUp noMods
        down = KArrow DDown noMods
        ed0 = (newEditor (24, 80) defaultConfig)
                { edBuffer = fromText (T.pack "alpha beta gamma") }
        -- Run two searches so the history has two entries.
        find t e = key KEnter (typeIn t (key (KCtrlChar 'f') e))
        ed2 = find "beta" (find "alpha" ed0)
    checkEq "history records searches, newest first"
      (edFindHist ed2) (map T.pack ["beta", "alpha"])
    -- Up recalls: newest, then older; Down comes back; typing resumes fresh.
    let edDlg = key (KCtrlChar 'f') ed2      -- Find field seeded with "beta"
        edU1 = key up edDlg
    checkEq "Up recalls newest" (fieldValue 0 <$> edDialog edU1) (Just (T.pack "beta"))
    let edU2 = key up edU1
    checkEq "Up again recalls older" (fieldValue 0 <$> edDialog edU2) (Just (T.pack "alpha"))
    let edD1 = key down edU2
    checkEq "Down returns newer" (fieldValue 0 <$> edDialog edD1) (Just (T.pack "beta"))
    let edD2 = key down edD1
    checkEq "Down past newest restores the draft" (fieldValue 0 <$> edDialog edD2)
      (Just (T.pack "beta"))    -- the stash was the seeded term
    check "browse state ends" (edHistPos edD2 == Nothing)
    -- With no history, Up still moves dialog focus (old behaviour).
    let edFresh = key up (key (KCtrlChar 'g') ed0)   -- Go To Line has no history
    check "no-history Up moves focus"
      ((dlgFocus <$> edDialog edFresh) == Just 2)    -- wrapped to the last button
    -- Round-trips through the persisted format, multi-line terms included.
    let fh = map T.pack ["multi\nline", "plain"]
        rh = [T.pack "repl one"]
    checkEq "history file round-trip" (parseHistoryText (renderHistoryText fh rh)) (fh, rh)

    -- A seeded Find term is "pristine": the first keystroke replaces it, but
    -- an arrow first means the user wants to edit it in place.
    let edSeeded = key (KCtrlChar 'f') ed2      -- seeded with "beta"
    checkEq "typing replaces a seeded term"
      (fieldValue 0 <$> edDialog (key (KChar 'x') edSeeded)) (Just (T.pack "x"))
    checkEq "after an arrow, typing edits in place"
      (fieldValue 0 <$> edDialog (key (KChar 'x') (key (KArrow DLeft noMods) edSeeded)))
      (Just (T.pack "betxa"))

  -- Word completion (Ctrl+Space) ---------------------------------------------------
  do
    let key k e = fst (update k e)
        lineAt i e = getLine' i (edBuffer e)
        edW = (newEditor (24, 80) defaultConfig)
                { edBuffer = fromText (T.pack "banana band bandit\nba")
                , edCursor = Pos 1 2 }
        popup = key (KCtrlChar ' ') edW
    case edComplete popup of
      Nothing -> check "completion popup opens" False
      Just cp -> do
        checkEq "candidates nearest-first, deduped" (cpItems cp)
          (map T.pack ["banana", "band", "bandit"])
        checkEq "prefix captured" (cpPrefix cp) (T.pack "ba")
    -- Down + Enter accepts the second candidate.
    let edPick = key KEnter (key (KArrow DDown noMods) popup)
    checkEq "accept replaces the prefix" (lineAt 1 edPick) (T.pack "band")
    check "popup closes on accept" (edComplete edPick == Nothing)
    -- One undo restores the typed prefix.
    checkEq "completion is undoable" (lineAt 1 (key (KCtrlChar 'z') edPick)) (T.pack "ba")
    -- Typing narrows; the sole survivor is accepted with Tab.
    let edNarrow = key (KChar 'd') (key (KChar 'n') popup)
    checkEq "typing narrows the list"
      (cpItems <$> edComplete edNarrow) (Just [T.pack "bandit"])
    checkEq "Tab accepts the survivor" (lineAt 1 (key KTab edNarrow)) (T.pack "bandit")
    -- A unique prefix completes immediately, no popup.
    let edUniq = key (KCtrlChar ' ')
                   edW { edBuffer = fromText (T.pack "banana band\nbana"), edCursor = Pos 1 4 }
    checkEq "single candidate inserts directly" (lineAt 1 edUniq) (T.pack "banana")
    check "no popup for a single candidate" (edComplete edUniq == Nothing)
    -- Esc keeps the buffer as typed; words from other open docs are offered.
    let edEsc = key KEsc popup
    check "Esc dismisses" (edComplete edEsc == Nothing)
    checkEq "Esc leaves the text alone" (lineAt 1 edEsc) (T.pack "ba")
    -- The banana words live in the (now background) main.py document.
    let edMulti = setLoadedNew "/w/lib.py"
                    (loadFromBytes False Nothing (TE.encodeUtf8 (T.pack "xylophone = 1")))
                    edW { edPath = Just "/w/main.py" }
        edMulti2 = edMulti { edBuffer = fromText (T.pack "bana"), edCursor = Pos 0 4 }
        edXy = key (KCtrlChar ' ') edMulti2
    checkEq "other open buffers contribute words"
      (getLine' 0 (edBuffer edXy)) (T.pack "banana")
    -- No candidates: friendly status, no popup.
    let edNone = key (KCtrlChar ' ') edW { edCursor = Pos 1 2
                                         , edBuffer = fromText (T.pack "zz\nqq") }
    check "no-completions status"
      (T.pack "No completions" `T.isInfixOf` edStatus edNone)

  -- Themes ---------------------------------------------------------------------------
  do
    let key k e = fst (update k e)
        edT = (newEditor (24, 80) defaultConfig)
                { edBuffer = fromText (T.pack "x = 42"), edPath = Just "/w/a.py" }
        viewIx = 3
    -- Config selects the theme; the parser validates it.
    let (cL, wL) = parseConfigText (T.pack "theme = light") defaultConfig
    checkEq "config theme=light" (cfgTheme cL) ThemeLight
    checkEq "config theme parse clean" wL []
    check "bad theme warns"
      (not (null (snd (parseConfigText (T.pack "theme = solarized") defaultConfig))))
    -- The palettes actually differ where it matters (numbers on light bg).
    check "light tokens differ from dark"
      (thTokens defaultTheme TkNumber /= thTokens lightTheme TkNumber)
    checkEq "themeFor maps names" (thTokens (themeFor ThemeLight) TkNumber)
      (thTokens lightTheme TkNumber)
    -- The rendered screen uses the configured palette for the number token.
    let scrD = renderEditor edT
        scrL = renderEditor edT { edConfig = (edConfig edT) { cfgTheme = ThemeLight } }
        styleAtCol c scr = cellStyle (scrCells scr A.! (1 * scrW scr + c))
    check "render follows the theme" (styleAtCol 4 scrD /= styleAtCol 4 scrL)
    -- View ▸ Theme toggles per-session, with a live label.
    let edTog = fst (update KEnter edT { edFocus = FMenu
                                       , edMenu = menuStateFor edT viewIx MAToggleTheme })
    checkEq "menu toggles theme" (cfgTheme (edConfig edTog)) ThemeLight
    check "menu label shows the value"
      (any (\case MEItem lbl _ MAToggleTheme -> T.pack "light" `T.isInfixOf` lbl; _ -> False)
           (entriesFor edTog viewIx))
    checkEq "toggle back" (cfgTheme (edConfig (key KEnter edTog { edFocus = FMenu
                             , edMenu = menuStateFor edTog viewIx MAToggleTheme }))) ThemeDark

  -- Horizontal mouse scrolling --------------------------------------------------------
  do
    let key k e = fst (update k e)
        wide = T.pack (replicate 300 'x')
        edH = (newEditor (24, 80) defaultConfig)
                { edBuffer = fromText (T.unlines (replicate 5 wide)) }
        wheel b sh = KMouse (MouseEvent b 40 10 True False (Mods sh False False) 1)
    -- SGR horizontal wheel buttons decode.
    kWL <- parseBytes (map (fromIntegral . fromEnum) "\x1b[<66;5;5M")
    kWR <- parseBytes (map (fromIntegral . fromEnum) "\x1b[<67;5;5M")
    check "wheel-left decodes" (case kWL of KMouse m -> meButton m == MBWheelLeft; _ -> False)
    check "wheel-right decodes" (case kWR of KMouse m -> meButton m == MBWheelRight; _ -> False)
    -- Shift+wheel pans; the cursor is pulled along; plain wheel still scrolls lines.
    let edPan = key (wheel MBWheelDown True) edH
    checkEq "shift+wheel pans right" (edLeft edPan) 6
    check "pan pulls the cursor into view" (posCol (edCursor edPan) >= 6)
    let edPanBack = key (wheel MBWheelUp True) edPan
    checkEq "shift+wheel pans back" (edLeft edPanBack) 0
    checkEq "plain wheel scrolls lines" (edTop (key (wheel MBWheelDown False) edH)) 3
    -- Horizontal wheel buttons pan too; word wrap makes it a no-op.
    checkEq "wheel-right pans" (edLeft (key (wheel MBWheelRight False) edH)) 6
    checkEq "wrap mode ignores pan" (edLeft (key (wheel MBWheelRight False) edH { edWordWrap = True })) 0
    -- CSV: shift+wheel steps the cell cursor across columns.
    let edCsvH = setLoaded "/tmp/w.csv"
                   (loadFromBytes False Nothing (TE.encodeUtf8 (T.pack "a,b,c\n1,2,3")))
                   (newEditor (24, 80) defaultConfig)
        colOf e = maybe (-1) csvCurCol (edCsv e)
    checkEq "csv shift+wheel moves column" (colOf (key (wheel MBWheelDown True) edCsvH)) 1
    checkEq "csv wheel-left moves back"
      (colOf (key (wheel MBWheelLeft False) (key (wheel MBWheelDown True) edCsvH))) 0

  -- Scrollbar --------------------------------------------------------------------------
  do
    let key k e = fst (update k e)
        big = (newEditor (24, 80) defaultConfig)
                { edBuffer = fromText (T.unlines [ T.pack ("l" ++ show i) | i <- [1 .. 300 :: Int] ]) }
        small = (newEditor (24, 80) defaultConfig) { edBuffer = fromText (T.pack "hi") }
    check "no bar when content fits" (scrollBarInfo small == Nothing)
    case scrollBarInfo big of
      Nothing -> check "bar appears on overflow" False
      Just (x, top, h, total, win) -> do
        checkEq "bar in the reserved column" x 79
        checkEq "bar spans the text area" (top, h) (1, 21)
        checkEq "bar totals the buffer" (total, win) (300, 0)
        let (tt, tl) = scrollThumb h total win
        check "thumb at top, sane size" (tt == 0 && tl >= 1 && tl < h)
    -- Click near the bottom of the track jumps deep into the file; the bar
    -- follows; dragging to the top comes back; release ends the drag.
    let press r = KMouse (MouseEvent MBLeft 79 r True False noMods 1)
        dragTo r = KMouse (MouseEvent MBLeft 79 r True True noMods 1)
        release r = KMouse (MouseEvent MBLeft 79 r False False noMods 1)
        edJ = key (press 20) big
    check "click jumps deep" (edTop edJ > 200)
    check "click starts a drag" (edScrollDrag edJ)
    let edD = key (dragTo 1) edJ
    checkEq "drag to the top returns" (edTop edD) 0
    let edR = key (release 1) edD
    check "release ends the drag" (not (edScrollDrag edR))
    -- The rendered screen shows the thumb/track glyphs in the last column.
    let scrB = renderEditor big
        lastCol = [ cellChar (scrCells scrB A.! (r * scrW scrB + 79)) | r <- [1 .. 21] ]
    check "track+thumb drawn" ('\x2588' `elem` lastCol && '\x2502' `elem` lastCol)
    -- CSV: the bar tracks the table and a click moves the cell cursor.
    let csvBig = T.unlines (T.pack "h1,h2" : [ T.pack (show i ++ "," ++ show i) | i <- [1 .. 200 :: Int] ])
        edCsvB = setLoaded "/tmp/big.csv" (loadFromBytes False Nothing (TE.encodeUtf8 csvBig))
                   (newEditor (24, 80) defaultConfig)
    check "csv overflow shows a bar" (scrollBarInfo edCsvB /= Nothing)
    let edCsvJ = key (press 20) edCsvB
    check "csv click jumps rows" (maybe 0 csvCurRow (edCsv edCsvJ) > 100)

  -- CSV column sort ---------------------------------------------------------------------
  do
    let key k e = fst (update k e)
        mkCsv txt = setLoaded "/tmp/s.csv" (loadFromBytes False Nothing (TE.encodeUtf8 (T.pack txt)))
                      (newEditor (24, 80) defaultConfig)
        col0 e = maybe [] (\v -> [ cellAt r 0 v | r <- [0 .. nRows v - 1] ]) (edCsv e)
        col1 e = maybe [] (\v -> [ cellAt r 1 v | r <- [0 .. nRows v - 1] ]) (edCsv e)
        edS0 = mkCsv "name,n\nbravo,2\nalpha,10\ncharlie,9"
        -- move to column n (col 1) and pin the header
        edS = fst (update (KArrow DRight noMods) (fst (runAction' MAToggleFreezeHeader edS0)))
        runAction' a e = update KEnter e { edFocus = FMenu, edMenu = menuStateFor e 3 a }

    -- Alt+S sorts numerically ascending, header pinned; again flips to descending.
    let edAsc = key (KAltChar 's') edS
    checkEq "numeric ascending, header pinned" (col1 edAsc)
      (map T.pack ["n", "2", "9", "10"])
    check "sort marks modified" (edModified edAsc)
    let edDesc = key (KAltChar 's') edAsc
    checkEq "second sort flips to descending" (col1 edDesc)
      (map T.pack ["n", "10", "9", "2"])
    -- The cursor follows its row: put it on "alpha" (row 2 originally).
    let edCur = edS { edCsv = fmap (setCursor 2 1) (edCsv edS) }
        edCurSorted = key (KAltChar 's') edCur
    checkEq "cursor follows its row"
      (maybe (-1) csvCurRow (edCsv edCurSorted)) 3    -- alpha,10 sorts last ascending
    -- One undo restores the original order.
    let edUndo2 = key (KCtrlChar 'z') edAsc
    checkEq "sort is undoable" (col0 edUndo2)
      (map T.pack ["name", "bravo", "alpha", "charlie"])
    -- Text column sorts case-insensitively; empties go last.
    let edT = key (KAltChar 's')
                ((mkCsv "h\nBeta\n\nalpha") { edFreezeHeader = True })
    checkEq "text sort, empties last" (col0 edT) (map T.pack ["h", "alpha", "Beta", ""])
    -- Outside table view it just explains itself.
    let edPlain = key (KAltChar 's') ((newEditor (24, 80) defaultConfig)
                    { edBuffer = fromText (T.pack "x") })
    check "plain text explains sort"
      (T.pack "table view" `T.isInfixOf` edStatus edPlain)

  -- Command palette ('>' in quick open) -----------------------------------------------
  do
    let key k e = fst (update k e)
        typeIn :: String -> Editor -> Editor
        typeIn s e = foldl (\acc c -> fst (update (KChar c) acc)) e s
        edP0 = (newEditor (24, 80) defaultConfig)
                 { edBuffer = fromText (T.pack "hello"), edPath = Just "/w/a.txt" }
        (edQ, _) = update (KCtrlChar 'p') edP0
        edCmd = typeIn ">" edQ
    case edQuickOpen edCmd of
      Nothing -> check "palette mode reachable" False
      Just qo -> do
        check "'>' switches to command mode" (Q.qoCommandMode qo)
        check "bare '>' lists all commands" (qoTotal qo > 30)
        check "commands look like Menu: Item"
          (case qoMatches qo of ((_, lbl, _) : _) -> T.pack "File: " `T.isPrefixOf` lbl; _ -> False)
    -- Fuzzy-filter to Word Wrap and run it (observable state change).
    let edWW = typeIn "word wrap" edCmd
    case edQuickOpen edWW of
      Nothing -> check "palette filtered" False
      Just qo -> check "query filters to the command"
        (case qoMatches qo of ((_, lbl, _) : _) -> T.pack "Word Wrap" `T.isInfixOf` lbl; _ -> False)
    let edRun = key KEnter edWW
    check "Enter runs the command" (edWordWrap edRun)
    check "palette closes after running" (edQuickOpen edRun == Nothing)
    -- Ctrl+Shift+P opens straight into command mode.
    let (edCsp, _) = update (KCtrlShiftChar 'p') edP0
    check "Ctrl+Shift+P preseeds '>'"
      (maybe False Q.qoCommandMode (edQuickOpen edCsp))
    -- Deleting the '>' drops back to file mode.
    let edBack = key KBackspace edCmd
    check "deleting '>' returns to files"
      (maybe True (not . Q.qoCommandMode) (edQuickOpen edBack))

  -- Report -------------------------------------------------------------------
  (passed, failed) <- readIORef results
  putStrLn ("Passed " ++ show passed ++ ", failed " ++ show failed)
  if failed == 0 then exitSuccess else exitFailure

-- Helpers --------------------------------------------------------------------

parseBytes :: [Word8] -> IO Key
parseBytes ws = do
  src <- listSource ws
  nextKey src

-- Build a Kitty/fixterms "CSI code ; mods u" byte sequence.
csiU :: Int -> Int -> [Word8]
csiU code mods = [0x1b, 0x5b] ++ digits code ++ [0x3b] ++ digits mods ++ [0x75]
  where digits n = map (fromIntegral . fromEnum) (show n)

listSource :: [Word8] -> IO ByteSource
listSource ws0 = do
  ref <- newIORef ws0
  let next = do
        xs <- readIORef ref
        case xs of
          []       -> pure Nothing
          (b : bs) -> writeIORef ref bs >> pure (Just b)
  pure (ByteSource next (const next))

-- Undo-stack depth (a local `edUndo` binding in main shadows the selector).
undoDepth :: Editor -> Int
undoDepth = length . edUndo

isJust' :: Maybe a -> Bool
isJust' = maybe False (const True)

-- A menu state with the entry carrying the given action highlighted (menu mi
-- open); lets a test "press Enter on" a specific dynamic menu item.
menuStateFor :: Editor -> Int -> MenuAction -> MenuState
menuStateFor ed mi act =
  let es = entriesFor ed mi
      ix = head ([ i | (i, MEItem _ _ a) <- zip [0 ..] es, a == act ] ++ [0])
  in MenuState mi True ix

-- Apply N Down-arrow presses.
moveDown :: Int -> Editor -> Editor
moveDown 0 e = e
moveDown n e = moveDown (n - 1) (fst (update (KArrow DDown noMods) e))

-- Load text directly into an editor (test convenience).
setLoadedText :: T.Text -> Editor -> Editor
setLoadedText t e = e { edBuffer = fromText t }

-- Image fixture helpers ------------------------------------------------------

-- Decoded RGBA at (x,y).
pixelAt :: Image -> Int -> Int -> (Int,Int,Int,Int)
pixelAt im x y =
  let p = imgPix im; i = (y * imgW im + x) * 4
  in (fromIntegral (p!i), fromIntegral (p!(i+1)), fromIntegral (p!(i+2)), fromIntegral (p!(i+3)))

le16b, le32b, be32b :: Int -> [Word8]
le16b n = [fromIntegral (n .&. 255), fromIntegral ((n `shiftR` 8) .&. 255)]
le32b n = [fromIntegral ((n `shiftR` (8*k)) .&. 255) | k <- [0..3]]
be32b n = [fromIntegral ((n `shiftR` (8*k)) .&. 255) | k <- [3,2,1,0]]

-- An animated 2x2 GIF89a in three parts (so tests can slice it): frame 1 is a
-- full red/green/blue/yellow canvas at 50cs, frame 2 paints a 2x1 top-row
-- sub-rectangle (yellow + a transparent pixel) with delay 0 and disposal 2,
-- and frame 3 is a transparent 1x1 at 30cs — together exercising
-- sub-rectangle composition, transparency, disposal 1/2 and the delay clamp.
gifAnimHeader, gifAnimF1, gifAnimF2, gifAnimF3 :: [Word8]
gifAnimHeader =
     map (fromIntegral . fromEnum) "GIF89a" ++ le16b 2 ++ le16b 2 ++ [0x91, 0, 0]
  ++ concat [[255,0,0],[0,255,0],[0,0,255],[255,255,0]]   -- GCT: red green blue yellow
gifAnimF1 = [0x21,0xF9,4, 0x04] ++ le16b 50 ++ [0, 0]     -- disposal 1, 500ms
         ++ [0x2C] ++ le16b 0 ++ le16b 0 ++ le16b 2 ++ le16b 2 ++ [0]
         ++ gifLzw 2 [0,1,2,3]
gifAnimF2 = [0x21,0xF9,4, 0x09] ++ le16b 0 ++ [0, 0]      -- disposal 2 + transp 0, 0cs
         ++ [0x2C] ++ le16b 0 ++ le16b 0 ++ le16b 2 ++ le16b 1 ++ [0]
         ++ gifLzw 2 [3,0]
gifAnimF3 = [0x21,0xF9,4, 0x01] ++ le16b 30 ++ [0, 0]     -- transp 0, 300ms
         ++ [0x2C] ++ le16b 0 ++ le16b 0 ++ le16b 1 ++ le16b 1 ++ [0]
         ++ gifLzw 2 [0]

mkGIFAnim :: BS.ByteString
mkGIFAnim = BS.pack (gifAnimHeader ++ gifAnimF1 ++ gifAnimF2 ++ gifAnimF3 ++ [0x3B])

-- Real LZW for tiny GIF fixtures: a clear code before every literal keeps the
-- code width fixed at minCode+1 bits (the decoder accepts clears anywhere), so
-- the packer needs no dictionary.
gifLzw :: Int -> [Int] -> [Word8]
gifLzw minCode pixels =
  fromIntegral minCode : fromIntegral (length packed) : packed ++ [0]
  where
    clear = 2 ^ minCode
    codes = concat [ [clear, p] | p <- pixels ] ++ [clear + 1]
    packed = packBitsLSB (minCode + 1) codes

-- Pack codes LSB-first at a fixed bit width (GIF's LZW bit order).
packBitsLSB :: Int -> [Int] -> [Word8]
packBitsLSB width = go 0 0
  where
    go acc n cs
      | n >= 8 = fromIntegral (acc .&. 255) : go (acc `shiftR` 8) (n - 8) cs
      | otherwise = case cs of
          []      -> [fromIntegral (acc .&. 255) | n > 0]
          (c : t) -> go (acc + c * 2 ^ n) (n + width) t

-- A 24-bit uncompressed BMP from row-major (top-to-bottom) RGB pixels.
mkBMP :: Int -> Int -> [(Word8,Word8,Word8)] -> BS.ByteString
mkBMP w h pix = BS.pack (hdr ++ dib ++ pixels)
  where
    rowBytes = ((w*24 + 31) `div` 32) * 4
    pad      = replicate (rowBytes - w*3) 0
    rowOf y  = concat [ let (r,g,b) = pix !! (y*w+x) in [b,g,r] | x <- [0..w-1] ] ++ pad
    pixels   = concat [ rowOf y | y <- [h-1, h-2 .. 0] ]   -- BMP is bottom-up
    dataOff  = 54
    hdr = map (fromIntegral . fromEnum) "BM" ++ le32b (dataOff + length pixels)
            ++ le32b 0 ++ le32b dataOff
    dib = le32b 40 ++ le32b w ++ le32b h ++ le16b 1 ++ le16b 24 ++ le32b 0
            ++ le32b (length pixels) ++ le32b 2835 ++ le32b 2835 ++ le32b 0 ++ le32b 0

-- A binary PPM (P6) from row-major RGB pixels.
mkPPM :: Int -> Int -> [(Word8,Word8,Word8)] -> BS.ByteString
mkPPM w h pix = BS.pack (header ++ body)
  where header = map (fromIntegral . fromEnum) ("P6\n" ++ show w ++ " " ++ show h ++ "\n255\n")
        body   = concat [ [r,g,b] | (r,g,b) <- pix ]

-- An RGB PNG using a single uncompressed (stored) DEFLATE block; exercises the
-- inflate stored-block path, scanline unfiltering and RGB conversion. CRCs are
-- not checked by the decoder, so they are left zero.
mkPNG :: Int -> Int -> [(Word8,Word8,Word8)] -> BS.ByteString
mkPNG w h pix = BS.pack (sig ++ chunk "IHDR" ihdr ++ chunk "IDAT" idat ++ chunk "IEND" [])
  where
    sig  = [137,80,78,71,13,10,26,10]
    ihdr = be32b w ++ be32b h ++ [8,2,0,0,0]                   -- 8-bit RGB, no interlace
    scan = concat [ 0 : concat [ let (r,g,b) = pix !! (y*w+x) in [r,g,b] | x <- [0..w-1] ]
                  | y <- [0..h-1] ]                             -- filter byte 0 (None) per row
    n    = length scan
    idat = [0x78,0x01]                                          -- zlib header
             ++ [0x01] ++ le16b n ++ le16b (0xFFFF - n) ++ scan -- one final stored block
             ++ [0,0,0,0]                                       -- (adler32, unchecked)
    chunk t d = be32b (length d) ++ map (fromIntegral . fromEnum) t ++ d ++ [0,0,0,0]

-- WebP fixtures, produced by libwebp (via PIL) and checked in as bytes. The
-- decoder was validated bit-exact against libwebp over a large corpus; these
-- keep the entry points covered offline.

-- 2x2 lossless (VP8L): red, green / blue, yellow.
mkWebPLL :: BS.ByteString
mkWebPLL = BS.pack
  [ 82,73,70,70,44,0,0,0,87,69,66,80,86,80
  , 56,76,31,0,0,0,47,1,64,0,0,31,32,16
  , 72,218,31,122,141,249,23,16,20,249,63,218,252,7
  , 95,36,224,7,8,17,253,15,1,0
  ]

-- 2x2 lossless with per-pixel alpha 255/128/64/0.
mkWebPLLA :: BS.ByteString
mkWebPLLA = BS.pack
  [ 82,73,70,70,52,0,0,0,87,69,66,80,86,80
  , 56,76,40,0,0,0,47,1,64,0,16,31,32,16
  , 72,222,31,58,13,1,65,145,255,163,9,8,138,252
  , 31,77,32,155,44,179,251,75,21,109,116,35,184,1
  , 67,68,255,35
  ]

-- 8x8 lossy (VP8): left half (200,50,30), right half (20,60,200).
mkWebPLossy :: BS.ByteString
mkWebPLossy = BS.pack
  [ 82,73,70,70,76,0,0,0,87,69,66,80,86,80
  , 56,32,64,0,0,0,144,2,0,157,1,42,8,0
  , 8,0,2,0,52,37,168,2,116,186,1,64,3,236
  , 2,191,255,112,0,28,24,0,254,241,175,249,149,127
  , 7,249,5,121,157,202,135,127,243,128,67,188,165,223
  , 239,95,243,53,213,158,243,241,177,70,214,57,108,0
  ]

-- The same 8x8 lossy frame in a VP8X container with an ALPH chunk
-- (alpha 255 on the left half, 40 on the right).
mkWebPLossyA :: BS.ByteString
mkWebPLossyA = BS.pack
  [ 82,73,70,70,114,0,0,0,87,69,66,80,86,80
  , 56,88,10,0,0,0,16,0,0,0,7,0,0,7
  , 0,0,65,76,80,72,12,0,0,0,1,15,240,148
  , 255,136,136,80,248,136,254,7,86,80,56,32,64,0
  , 0,0,144,2,0,157,1,42,8,0,8,0,2,0
  , 52,37,168,2,116,186,1,64,3,236,2,191,255,112
  , 0,28,24,0,254,241,175,249,149,127,7,249,5,121
  , 157,202,135,127,243,128,67,188,165,223,239,95,243,53
  , 213,158,243,241,177,70,214,57,108,0
  ]
