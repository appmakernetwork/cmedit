-- | Hand-rolled test suite (no external test framework is available offline).
-- Exercises the pure core: text buffer, width mapping, input parsing and the
-- editor update function.
module Main (main) where

import Control.Monad (forM_, unless, void)
import Data.Bits (shiftR, (.&.))
import Data.Foldable (toList)
import Data.IORef
import Data.Word (Word8)
import Data.Either (isLeft)
import Data.List (foldl', intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, tails)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Control.Exception (SomeException, evaluate, try)
import System.Exit (exitFailure, exitSuccess)
import GHC.Clock (getMonotonicTime)
import GHC.Stats (getRTSStats, getRTSStatsEnabled, gc, gcdetails_live_bytes)
import System.Mem (performMajorGC)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Array (bounds)
import qualified Data.Array as A
import Data.Array.Unboxed ((!), listArray)

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
  ( parseConfigText, updateConfigText, RecentEntry(..), parseRecentText
  , renderRecentText, parseHistoryText, renderHistoryText
  , Session(..), parseSessionText, renderSessionText
  , RestorePlan(..), planRestore )
import Cmedit.QuickOpen (QuickOpen(..))
import qualified Cmedit.QuickOpen as Q
import Cmedit.Menu (MenuAction(..), MenuEntry(..), MenuState(..))
import Cmedit.Dialog (fieldValue, Field(..), Choice(..), Dialog(..), DialogKind(..), mkFind, mkTheme, fieldSetCursorLineCol, focusedButton, focusedChoice, focusedField, focusIsButton, focusNext, focusPrev, cycleChoice, setChoiceIx, focusCount)
import Cmedit.Browser (Browser(..), FileNode(..))
import qualified Cmedit.Browser as Br
import Cmedit.Search (SearchState(..), SField(..), SearchField(..), FileResult(..), Match(..), SRow(..))
import qualified Cmedit.Search as S
import qualified Cmedit.DocText as DT
import Cmedit.Definition (DefLang(..), DefPick(..), DefItem(..), DefReq(..))
import qualified Cmedit.Definition as D
import qualified Cmedit.Regex as Rx
import qualified Data.Sequence as Seq
import Cmedit.Pager (PagerDoc(..))
import qualified Cmedit.Pager as Pg
import Cmedit.Journal (Journal(..), RecoveryCase(..))
import qualified Cmedit.Journal as J
import qualified Cmedit.Zip as Z
import qualified Cmedit.Xml as X
import qualified Cmedit.Docx as Docx
import qualified Cmedit.Epub as Epub
import qualified Cmedit.Xlsx as Xlsx
import qualified Cmedit.Formula as Fm
import qualified Cmedit.Odf as Odf
import Cmedit.App (convertPath)
import Cmedit.Rtf (RtfDoc(..))
import Cmedit.Pdf (PdfDoc(..))
import qualified Cmedit.Pdf as Pdf
import qualified Cmedit.Rtf as Rtf
import Cmedit.Csv
import Cmedit.Image (Image(..), ImgMode(..), decodeImage, decodeFrames, decodeGIFFrames, sniffImage, renderImage, viewFit, scaleRGBA)
import Cmedit.Render (renderEditor, renderFrame, scrollPlan, Screen(..), ScrollHint(..), Theme(..), defaultTheme, lightTheme, themeFor, FileKind(..), fileKind, expandLineCells)
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
import Cmedit.Lint
  ( LinterId(..), Linter(..), Severity(..), Diag(..)
  , linters, linterById, lintersForPath
  , parseLintOutput, diagSpans, diagAt, maxDiagsPerFile )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | Live heap in MB after a major collection (needs the -T RTS flag, which the
-- Makefile's test target sets). Used by the retention guards below.
liveMB :: IO Double
liveMB = do
  performMajorGC
  s <- getRTSStats
  pure (fromIntegral (gcdetails_live_bytes (gc s)) / 1048576)

-- | Retention guard for plan 0014. A 'Data.Text' value is a slice of a shared
-- byte array, so a clipboard entry cut from a buffer would pin that buffer's
-- whole file for the rest of the session unless it is 'detach'ed. Copies one
-- 100-character line out of a ~4 MB document, drops the document, and reports
-- what is still live. Returns (liveAfter, documentSizeMB).
clipboardRetentionMB :: IO (Double, Double)
clipboardRetentionMB = do
  let nLines = 20000
      lineW  = 200
      big    = T.unlines (replicate nLines (T.replicate lineW (T.pack "x")))
      docMB  = fromIntegral (nLines * (lineW + 1)) / 1048576
      ed0    = setLoaded "big.txt" (emptyLoadResult { lrBuffer = fromText big })
                         (newEditor (40, 120) defaultConfig)
      ed1    = ed0 { edSelAnchor = Just (Pos 0 0), edCursor = Pos 0 100 }
      (ed2, _) = update (KCtrlChar 'c') ed1
      clip   = edClipboard ed2
  T.length clip `seq` pure ()
  live <- liveMB                    -- only @clip@ is reachable from here on
  pure (live + fromIntegral (T.length clip) * 0, docMB)

-- | Retention guard for plan 0001: how much does the *undo history* keep alive
-- after @n@ snapshot-pushing edits? Every snapshot's lines are forced first, so
-- what is measured is the history structure itself rather than unevaluated
-- thunks (an unoptimised build leaves plenty of those, and they swamp the
-- signal). With the old lazy @take maxUndo@ bound this grew linearly with @n@ —
-- the cap was never applied, so every snapshot ever pushed stayed reachable;
-- with the structural 'pushHist' bound it is flat at the cap.
undoRetentionMB :: Int -> IO (Double, Int)
undoRetentionMB n = do
  let ed0 = setLoaded "big.txt"
              (emptyLoadResult { lrBuffer = fromText (T.unlines (replicate 200 (T.pack "some line of text"))) })
              (newEditor (40, 120) defaultConfig)
      -- foldl' because the real editor applies each keystroke before the next
      -- arrives; a lazy fold would measure its own thunk chain instead.
      step e i = fst (update (if even (i :: Int) then KChar 'x' else KBackspace) e)
      edN = foldl' step ed0 [1 .. n]
      !forced = sum [ T.length t | u <- toList (edUndo edN)
                                 , t <- toList (bufLines (usBuffer u)) ]
  forced `seq` pure ()
  live <- liveMB                 -- edN is still reachable (used just below)
  pure (live, Seq.length (edUndo edN))

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
  -- Emoji + variation selector: the terminal folds ℹ️ into a 2-cell glyph,
  -- and our sizing must agree with the renderer's own cell emission
  -- (which forces at least one grid cell per code point). If either side
  -- undercounts, CSV columns to the right of an emoji cell drift left by
  -- one cell per occurrence — the row-1236 bug.
  checkEq "colToDisplay info+VS16"
    (colToDisplay 4 2 (T.pack "\x2139\xFE0Fx")) 2
  checkEq "colToDisplay past info+VS16"
    (colToDisplay 4 3 (T.pack "\x2139\xFE0Fx")) 3
  checkEq "cellWidth info+VS16"
    (cellWidth (T.pack "\x2139\xFE0F")) 2
  checkEq "cellWidth mixed emoji row"
    (cellWidth
      (T.pack "\x1F44D Reviews \x1F4CC Map \x2139\xFE0F Information"))
    (2 + 1 + 7 + 1 + 2 + 1 + 3 + 1 + 2 + 1 + 11)
  -- Emoji_Presentation code points in the BMP misc-symbols/dingbats block:
  -- these render as two-cell emoji by default (no VS16 selector) and used to
  -- be missing from the 'wide' table, drifting CSV columns by one cell per
  -- glyph.
  checkEq "sparkles width"        (charWidth '\x2728') 2   -- ✨
  checkEq "watch width"           (charWidth '\x231A') 2   -- ⌚
  checkEq "hourglass sand width"  (charWidth '\x23F3') 2   -- ⏳
  checkEq "check mark button"     (charWidth '\x2705') 2   -- ✅
  checkEq "high voltage width"    (charWidth '\x26A1') 2   -- ⚡
  checkEq "large red square"      (charWidth '\x1F7E5') 2  -- 🟥 (existing range)
  checkEq "cellWidth sparkles cell"
    (cellWidth (T.pack "\x2728The Portal\x2728 Mount Shasta"))
    (2 + 10 + 2 + 1 + 12)

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
  -- Kitty reports numpad keys with distinct KP_* functional codes; every one
  -- must fold onto the same key its non-numpad twin produces (Enter, digits,
  -- arrows, Home/End/…) or it lands as a PUA character.
  kKpEnter <- parseBytes (bytesOf "\ESC[57414u")     -- KP_ENTER
  checkEq "numpad enter csi-u" kKpEnter KEnter
  kKpEnterC <- parseBytes (bytesOf "\ESC[57414;5u")  -- Ctrl+KP_ENTER
  checkEq "ctrl numpad enter csi-u" kKpEnterC KModEnter
  kKp5 <- parseBytes (bytesOf "\ESC[57404u")         -- KP_5 -> '5'
  checkEq "numpad 5 csi-u" kKp5 (KChar '5')
  kKpDot <- parseBytes (bytesOf "\ESC[57409u")       -- KP_DECIMAL -> '.'
  checkEq "numpad . csi-u" kKpDot (KChar '.')
  kKpUp <- parseBytes (bytesOf "\ESC[57419u")        -- KP_UP -> Arrow Up
  checkEq "numpad up csi-u" kKpUp (KArrow DUp noMods)
  kKpHome <- parseBytes (bytesOf "\ESC[57423u")      -- KP_HOME
  checkEq "numpad home csi-u" kKpHome (KHome noMods)
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
  -- Dialog choice rows: focus traversal with all four element kinds present;
  -- Left/Right/Space/Enter cycling (with wrap); Enter must not confirm; and a
  -- rendered choice shows its label, current value and the guillemets, plus the
  -- focused row's hint line.
  let chTheme = Choice (T.pack "Theme") [T.pack "auto", T.pack "dark", T.pack "light"] 0
                       (Just (T.pack "Appearance")) (T.pack "Editor colour theme") Nothing
      chTabs  = Choice (T.pack "Tab width") [T.pack "2", T.pack "4", T.pack "8"] 1
                       Nothing (T.pack "Spaces per tab") Nothing
      -- A dialog with a field, an option, a choice and buttons (all four kinds).
      dAll = Dialog DKMessage (T.pack "Settings")
               [Field (T.pack "Name:") (T.pack "") 0]
               [(T.pack "Wrap", False)]
               [chTheme]
               [T.pack "OK", T.pack "Cancel"] 0 (T.pack "") False
  let screenLines scr = [ [ cellChar (scrCells scr A.! (r * scrW scr + c)) | c <- [0 .. scrW scr - 1] ]
                        | r <- [0 .. scrH scr - 1] ]
  checkEq "choice focus order: 1 field + 1 option + 1 choice + 2 buttons"
    (focusCount dAll) 5
  check "focus 0 is the field"  (isJust' (focusedField dAll) && (focusedChoice dAll == Nothing))
  let dAll1 = focusNext dAll; dAll2 = focusNext dAll1; dAll3 = focusNext dAll2
  check "focus 1 is the option"  ((focusedField dAll1 == Nothing) && (focusedChoice dAll1 == Nothing) && not (focusIsButton dAll1))
  check "focus 2 is the choice"  (focusedChoice dAll2 == Just 0 && not (focusIsButton dAll2))
  check "focus 3 is a button"    (focusIsButton dAll3 && focusedButton dAll3 == Just 0)
  check "focusPrev from the choice returns to the option"
    ((focusedChoice (focusPrev dAll2) == Nothing) && (focusedField (focusPrev dAll2) == Nothing))

  -- Pure cycling with wrap in both directions.
  checkEq "cycle forward advances the value index" (chIx (head (dlgChoices (cycleChoice 0 1 dAll)))) 1
  checkEq "cycle forward wraps at the end"
    (chIx (head (dlgChoices (cycleChoice 0 1 (setChoiceIx 0 2 dAll))))) 0
  checkEq "cycle backward wraps at the start"
    (chIx (head (dlgChoices (cycleChoice 0 (-1) dAll)))) 2

  -- End-to-end through 'update': choice focused, keys cycle the value.
  let choDlg = Dialog DKMessage (T.pack "Settings") [] []
                 [chTheme, chTabs] [T.pack "OK"] 0 (T.pack "") False
      edCho  = ed0 { edDialog = Just choDlg, edFocus = FDialog }
      choIx d k = chIx (dlgChoices d !! k)
      afterKeys ks = maybe choDlg id (edDialog (feed edCho ks))
  checkEq "Right arrow cycles the focused choice forward"
    (choIx (afterKeys [KArrow DRight noMods]) 0) 1
  checkEq "Left arrow cycles the focused choice backward (wraps)"
    (choIx (afterKeys [KArrow DLeft noMods]) 0) 2
  checkEq "Space cycles the focused choice forward"
    (choIx (afterKeys [KChar ' ']) 0) 1
  checkEq "Enter cycles the focused choice forward"
    (choIx (afterKeys [KEnter]) 0) 1
  check "Enter on a choice row does NOT close the dialog"
    (isJust' (edDialog (feed edCho [KEnter])))
  -- Tab past the choices reaches the button; only then does Enter confirm.
  check "Tab/Down moves focus off the choices to the button"
    (maybe False focusIsButton (edDialog (feed edCho [KTab, KTab])))
  check "Enter on the button closes the dialog"
    (edDialog (feed edCho [KTab, KTab, KEnter]) == Nothing)

  -- Render: the label, current value and single guillemets appear on one row,
  -- and the focused choice's hint appears (message-area style) below.
  let choLines  = screenLines (renderEditor edCho)
      hasLine s = any (s `isInfixOf`) choLines
  check "choice row shows the label"          (hasLine "Theme")
  check "choice row shows the current value"   (hasLine "auto")
  check "choice row shows the left guillemet"  (any ('\x2039' `elem`) choLines)
  check "choice row shows the right guillemet" (any ('\x203a' `elem`) choLines)
  check "label, value and guillemets share one row"
    (any (\l -> "Theme" `isInfixOf` l && "auto" `isInfixOf` l
                && ['\x2039'] `isInfixOf` l && ['\x203a'] `isInfixOf` l) choLines)
  check "focused choice's hint line is shown" (hasLine "Editor colour theme")
  check "section header is shown above the choice" (hasLine "Appearance")
  -- Cycling the value updates what is rendered.
  let choLines2 = screenLines (renderEditor (feed edCho [KArrow DRight noMods]))
  check "cycling re-renders the new value" (any ("dark" `isInfixOf`) choLines2)

  -- Existing dialogs still render and confirm unchanged.
  let findLines = screenLines (renderEditor (fst (update (KCtrlChar 'f') (setLoadedText (T.pack "hello") ed0))))
  check "Find dialog still renders its title" (any ("Find" `isInfixOf`) findLines)
  let edGoto = feed (fst (update (KCtrlChar 'g') (setLoadedText (T.pack "a\nb\nc\nd") ed0))) [KChar '3', KEnter]
  check "Go-to-line confirm still works (dialog closed, cursor moved)"
    ((edDialog edGoto == Nothing) && edCursor edGoto == Pos 2 0)

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

  statsOn <- getRTSStatsEnabled

  -- Live session counters on the status bar (plan 0006) ----------------------
  do
    let edPlain = setLoaded "s.txt" (mkLR "hello") (newEditor (24, 100) defaultConfig)
        edWith  = setStatsLine (Just (T.pack "f 1.2/9ms 42MB j1")) edPlain
        (txtOff, zonesOff) = statusRightInfo edPlain
        (txtOn,  zonesOn)  = statusRightInfo edWith
    check "no stats segment when the driver has not set one"
          (not ("42MB" `isInfixOf` txtOff))
    check "stats segment appears when set" ("f 1.2/9ms 42MB j1" `isInfixOf` txtOn)
    -- The click zones must still land on the right columns: the segment is
    -- plain text, but it shifts everything after it.
    checkEq "status click zones survive the stats segment"
            (map (\(_, _, z) -> z) zonesOn) (map (\(_, _, z) -> z) zonesOff)
    let shift = length txtOn - length txtOff
    checkEq "stats segment shifts the zones by its own width"
            [ c - shift | (c, _, _) <- zonesOn ] [ c | (c, _, _) <- zonesOff ]
    -- Config key round-trips like any other.
    checkEq "debug-stats parses"
            (cfgDebugStats (fst (parseConfigText "debug-stats = on" defaultConfig))) True
    check "debug-stats is off by default" (not (cfgDebugStats defaultConfig))

  -- JPEG decoding (plan 0018) ------------------------------------------------
  -- Structure worth checking: hard colour edges (JPEG ringing), a gradient
  -- region, a 1-pixel checkerboard (the worst case for chroma subsampling) and
  -- the image corners, which exercise the upsampler's edge clamping.
  case decodeImage jpegGray8 of
    Left e -> check ("grayscale JPEG: " ++ e) False
    Right im -> do
      checkEq "jpeg gray: dimensions" (imgW im, imgH im) (32, 24)
      checkEq "jpeg gray: format" (imgFmt im) "JPEG"
      -- Grayscale decodes to equal R=G=B.
      let (r0,g0,b0,a0) = pixelAt im 4 4
      checkEq "jpeg gray: neutral pixel" (r0 == g0 && g0 == b0, a0) (True, 255)
      checkEq "jpeg gray: red block luma"   (pixelAt im 4 4)   (76,76,76,255)
      checkEq "jpeg gray: green block luma" (pixelAt im 12 6)  (150,150,150,255)
      checkEq "jpeg gray: corner"           (pixelAt im 31 23) (0,0,0,255)
  case decodeImage jpegColour420 of
    Left e -> check ("colour JPEG: " ++ e) False
    Right im -> do
      checkEq "jpeg 4:2:0: dimensions" (imgW im, imgH im) (32, 24)
      -- Saturated blocks survive the round trip through YCbCr and chroma
      -- upsampling; these are the exact values the decoder produced before the
      -- IDCT was rewritten, so they pin the arithmetic as well as the plumbing.
      checkEq "jpeg 4:2:0: red block"    (pixelAt im 0 0)   (253,0,1,255)
      checkEq "jpeg 4:2:0: red block b"  (pixelAt im 4 4)   (252,1,0,255)
      checkEq "jpeg 4:2:0: green block"  (pixelAt im 12 6)  (2,255,0,255)
      checkEq "jpeg 4:2:0: gradient"     (pixelAt im 20 12) (158,121,121,255)
      checkEq "jpeg 4:2:0: corner"       (pixelAt im 31 23) (0,0,0,255)
      checkEq "jpeg 4:2:0: checkerboard" (pixelAt im 28 9)  (255,255,255,255)
      -- Every pixel is opaque and in range (a decoder that walks off a plane
      -- tends to produce zeros or garbage alpha).
      let alphas = [ a | y <- [0 .. imgH im - 1], x <- [0 .. imgW im - 1]
                       , let (_,_,_,a) = pixelAt im x y ]
      checkEq "jpeg 4:2:0: fully opaque" (all (== 255) alphas, length alphas) (True, 32*24)
  check "truncated JPEG -> error" (isLeft (decodeImage (BS.take 120 jpegColour420)))

  -- Paged read-only view of huge files (plan 0012) ---------------------------
  -- The index is the whole basis of the view: if an offset is wrong, the file
  -- is shown incorrectly with no way for the user to tell. Check it against a
  -- brute-force split for every awkward shape.
  do
    tmpDir <- getTemporaryDirectory
    let tmp = tmpDir </> "cmedit-pager-test.bin"
        cases =
          [ ("plain lf",            "a\nb\nc\n")
          , ("no final newline",    "a\nb\nc")
          , ("crlf",                "a\r\nb\r\nc\r\n")
          , ("cr only",             "a\rb\rc\r")
          , ("blank lines",         "\n\n\nx\n\n")
          , ("single line no nl",   "only one line")
          , ("empty file",          "")
          , ("bom + lf",            "\239\187\191a\nb\n")
          , ("utf8 multibyte",      "\27979\35797\n\128512 emoji\ncaf\233\n")
          , ("trailing blank",      "a\n\n")
          ]
    forM_ cases $ \(nm, content) -> do
      BS.writeFile tmp (TE.encodeUtf8 (T.pack content))
      sz <- fromIntegral . BS.length <$> BS.readFile tmp
      r <- Pg.buildPagerIndex tmp sz
      case r of
        Left e -> check ("pager index " ++ nm ++ ": " ++ e) False
        Right pg -> do
          -- Reference: how the ordinary loader would split the same text.
          -- Mirror TextBuffer.splitContent exactly: normalise the WHOLE text
          -- first, then strip one trailing newline, then split.
          let refLines = let t = T.pack content
                             norm = T.replace (T.pack "\r") (T.pack "\n")
                                      (T.replace (T.pack "\r\n") (T.pack "\n") t)
                             body = if not (T.null norm) && T.last norm == '\n'
                                      then T.init norm else norm
                         in if T.null t then [T.empty] else T.splitOn (T.pack "\n") body
          checkEq ("pager index " ++ nm ++ ": line count")
                  (pgLineCount pg) (max 1 (length refLines))
          -- Every line must read back exactly, through the index+seek path.
          w <- Pg.readPagerWindow pg 0 (pgLineCount pg)
          checkEq ("pager window " ++ nm ++ ": content")
                  (toList w) (take (pgLineCount pg) refLines)
    -- A file big enough to need several index entries: every line must be
    -- reachable by seeking, including across stride boundaries.
    let nBig = Pg.pagerStride * 3 + 7
    BS.writeFile tmp (TE.encodeUtf8 (T.pack (unlines [ "line " ++ show i | i <- [1 .. nBig] ])))
    szBig <- fromIntegral . BS.length <$> BS.readFile tmp
    rBig <- Pg.buildPagerIndex tmp szBig
    case rBig of
      Left e -> check ("pager big index: " ++ e) False
      Right pg -> do
        checkEq "pager index: line count across strides" (pgLineCount pg) nBig
        forM_ [0, 1, Pg.pagerStride - 1, Pg.pagerStride, Pg.pagerStride + 1
              , 2 * Pg.pagerStride, nBig - 1] $ \ln -> do
          w <- Pg.readPagerWindow pg ln 1
          checkEq ("pager seek to line " ++ show ln)
                  (toList w) [T.pack ("line " ++ show (ln + 1))]
        -- Windows that straddle a stride boundary come back in order.
        w2 <- Pg.readPagerWindow pg (Pg.pagerStride - 2) 4
        checkEq "pager window across a stride boundary" (toList w2)
                [ T.pack ("line " ++ show i) | i <- [Pg.pagerStride - 1 .. Pg.pagerStride + 2] ]
        -- Movement clamps and keeps the cursor on screen.
        let h = 10
            pgEnd = Pg.pagerBottom h pg
        checkEq "pager: Ctrl+End lands on the last line" (pgCursor pgEnd) (nBig - 1)
        check "pager: the last line is visible after Ctrl+End"
              (pgCursor pgEnd >= pgTop pgEnd && pgCursor pgEnd < pgTop pgEnd + h)
        checkEq "pager: movement clamps at the top" (pgCursor (Pg.pagerMoveBy h (-999) pg)) 0
        -- A viewport outside the loaded window asks for a refill; one inside
        -- does not (that is what keeps scrolling from re-reading constantly).
        check "pager: empty window needs a fill" (isJust' (Pg.pagerNeedsFill h pg))
        w3 <- Pg.readPagerWindow pg 0 (3 * h + 2 * Pg.pagerStride)
        let pgLoaded = Pg.pagerFilled 0 w3 pg
        check "pager: loaded window needs no fill" (not (isJust' (Pg.pagerNeedsFill h pgLoaded)))
        check "pager: scrolling far away needs a fill"
              (isJust' (Pg.pagerNeedsFill h (Pg.pagerMoveTo h (nBig - 1) pgLoaded)))
    -- A file with NO separator at all: the window reader must not concatenate
    -- the whole thing looking for one. Before the cap, a 120 MB single-line
    -- file drove the editor to 51 GB resident; the guard here is that reading
    -- one line of a no-newline file yields at most maxPagerLine characters and
    -- returns promptly.
    let hugeLine = 400000 :: Int
    BS.writeFile tmp (BS.replicate hugeLine 120)      -- 'x' repeated, no newline
    szOne <- fromIntegral . BS.length <$> BS.readFile tmp
    rOne <- Pg.buildPagerIndex tmp szOne
    case rOne of
      Left e -> check ("pager one-line index: " ++ e) False
      Right pg -> do
        checkEq "pager: a file with no newline is one line" (pgLineCount pg) 1
        w <- Pg.readPagerWindow pg 0 1
        checkEq "pager: one window line returned" (Seq.length w) 1
        check ("pager: an enormous line is capped (got "
               ++ show (maybe 0 T.length (Seq.lookup 0 w)) ++ " chars)")
              (maybe 0 T.length (Seq.lookup 0 w) <= Pg.maxPagerLine)
    _ <- try (removeFile tmp) :: IO (Either SomeException ())
    pure ()

  -- RTF formatted view --------------------------------------------------------
  -- The parser's job is to render the document and ignore everything else, so
  -- the tests are mostly "did the markup disappear and the text survive".
  do
    let rtfDoc = T.pack $ concat
          [ "{\\rtf1\\ansi\\ansicpg1252\\deff0"
          , "{\\fonttbl{\\f0\\froman Times;}}"
          , "{\\colortbl;\\red0\\green0\\blue0;\\red200\\green0\\blue0;}"
          , "{\\*\\generator Riched20;}{\\info{\\title Secret}}"
          , "\\pard\\qc\\b Heading\\b0\\par\n"
          , "\\pard\\ql Plain \\i slanted\\i0  and \\cf2 red\\cf0  and \\ul under\\ulnone .\\par\n"
          , "\\pard\\v hidden\\v0 shown\\par\n"
          , "\\pard Caf\\'e9 \\u233?X \\ldblquote q\\rdblquote \\emdash end\\par\n"
          , "}" ]
        pars = toList (Rtf.parseRtf rtfDoc)
        parText p = T.concat (map Rtf.rrText (Rtf.rpRuns p))
        allText = T.intercalate (T.pack "\n") (map parText pars)
    check "rtf looksLikeRtf" (Rtf.looksLikeRtf rtfDoc)
    check "rtf not looksLikeRtf on plain text" (not (Rtf.looksLikeRtf (T.pack "hello")))
    -- Destinations that are not document text must leave no trace at all.
    forM_ ["fonttbl", "Times", "colortbl", "Riched20", "Secret", "\\par", "\\pard"] $ \s ->
      check ("rtf drops " ++ s) (not (s `isInfixOf` T.unpack allText))
    checkEq "rtf paragraph count" (length pars) 4
    checkEq "rtf heading text" (parText (head pars)) (T.pack "Heading")
    checkEq "rtf heading centred" (Rtf.rpAlign (head pars)) Rtf.AlignCenter
    -- \v hidden text is dropped the way a word processor hides it.
    checkEq "rtf hidden text dropped" (parText (pars !! 2)) (T.pack "shown")
    -- cp1252 \'hh, \uN with its fallback char skipped, and the shorthands.
    checkEq "rtf escapes" (parText (pars !! 3))
      (T.pack "Caf\233 \233X \8220q\8221\8212end")
    -- Character formatting lands on the right runs.
    let body = pars !! 1
        runOf t = [ r | r <- Rtf.rpRuns body, Rtf.rrText r == T.pack t ]
    check "rtf italic run" (all (Rtf.rfItalic . Rtf.rrFmt) (runOf "slanted"))
    check "rtf underline run" (all (Rtf.rfUnder . Rtf.rrFmt) (runOf "under"))
    check "rtf colour run" (all ((== Just (ColorRGB 200 0 0)) . Rtf.rfColor . Rtf.rrFmt) (runOf "red"))
    check "rtf plain run has no formatting"
      (all ((== Rtf.defaultFmt) . Rtf.rrFmt) (runOf "Plain "))
    -- The bold heading must not leak into the following paragraph.
    check "rtf bold ends at \\b0" (not (any (Rtf.rfBold . Rtf.rrFmt) (Rtf.rpRuns body)))

    -- Layout: every laid-out line fits the width it was laid out for, and the
    -- spans stay inside their line's text (the renderer indexes with them).
    forM_ [20, 40, 80] $ \w -> do
      let ls = toList (Rtf.layoutRtf 8 w (Seq.fromList pars))
      check ("rtf layout fits width " ++ show w)
        (all (\l -> Rtf.rlPad l + lineDisplayWidth 8 (Rtf.rlText l) <= w) ls)
      check ("rtf layout spans in range " ++ show w)
        (all (\l -> all (\(s, e, _) -> s >= 0 && e <= T.length (Rtf.rlText l) && s <= e)
                        (Rtf.rlSpans l)) ls)
      check ("rtf layout keeps all text " ++ show w)
        (T.filter (not . isSpace) (T.concat (map Rtf.rlText ls))
           == T.filter (not . isSpace) (T.concat (map parText pars)))
    -- Narrower wrapping can only produce more lines, never fewer.
    check "rtf narrower wraps to more lines"
      (Seq.length (Rtf.layoutRtf 8 20 (Seq.fromList pars)) >= Seq.length (Rtf.layoutRtf 8 80 (Seq.fromList pars)))

    -- Alignment places the text, since the view has no other way to show it.
    let ctr = head (toList (Rtf.layoutRtf 8 40 (Seq.fromList (take 1 pars))))
    checkEq "rtf centred pad" (Rtf.rlPad ctr) ((40 - 7) `div` 2)

    -- \ucN says how many fallback characters follow each \uN, and getting it
    -- wrong either duplicates text or eats the document after it. A control
    -- word, a \'hh escape and a plain character each count as one.
    let ucText n s = T.concat (map Rtf.rrText (Rtf.rpRuns (head (toList
                       (Rtf.parseRtf (T.pack ("{\\rtf1\\uc" ++ show (n :: Int) ++ " " ++ s ++ "}")))))))
    checkEq "rtf uc1 skips one char" (ucText 1 "[\\u233?]") (T.pack "[\233]")
    checkEq "rtf uc2 skips two chars" (ucText 2 "[\\u233??]") (T.pack "[\233]")
    checkEq "rtf uc0 skips nothing" (ucText 0 "[\\u233]x") (T.pack "[\233]x")
    checkEq "rtf uc skips a \\'hh fallback" (ucText 1 "[\\u233\\'e9]") (T.pack "[\233]")
    checkEq "rtf uc skips a control-word fallback" (ucText 1 "[\\u9731\\loch ]") (T.pack "[\9731]")
    checkEq "rtf uc never eats a group delimiter"
      (ucText 2 "[\\u233{}]") (T.pack "[\233]")
    -- A negative \u is a code point above 32767 written as signed 16 bits.
    checkEq "rtf negative \\u wraps to 16 bits" (ucText 0 "[\\u-1]") (T.pack "[\65535]")
    -- Lone surrogates are not characters; they must not reach 'chr'.
    checkEq "rtf lone surrogate becomes U+FFFD" (ucText 0 "[\\u55357]") (T.pack "[\65533]")
    -- A nonsense parameter must still yield exactly one character, with none
    -- of its digits leaking out as document text.
    checkEq "rtf absurd \\u yields one char, no leaked digits"
      (T.length (ucText 0 "[\\u111411200000000]")) 3

    -- Malformed input must degrade, not diverge.
    forM_ [ "{\\rtf1", "{\\rtf1\\b", "}}}}", "{\\rtf1 \\u", "{\\rtf1 \\'z", "{\\rtf1 \\'"
          , "{\\rtf1 {\\*\\unknown", "{\\rtf1 \\uc99 \\u65?x" ] $ \s ->
      check ("rtf survives " ++ s)
        (Seq.length (Rtf.parseRtf (T.pack s)) >= 0)

    -- The view: entering it derives from the buffer and never writes back;
    -- toggling out leaves the buffer byte-identical.
    let ed0 = setLoaded "doc.rtf"
                (emptyLoadResult { lrBuffer = fromText rtfDoc })
                (newEditor (24, 80) defaultConfig)
    check "rtf opens in the formatted view" (isJust' (edRtf ed0))
    checkEq "rtf buffer untouched on open" (bufferToText LF False (edBuffer ed0)) rtfDoc
    let edRaw = toggleRtf ed0
        edFmt = toggleRtf edRaw
    check "rtf Alt+T leaves the formatted view" (not (isJust' (edRtf edRaw)))
    check "rtf Alt+T returns to it" (isJust' (edRtf edFmt))
    checkEq "rtf round-trip preserves the buffer exactly"
      (bufferToText LF False (edBuffer edFmt)) rtfDoc
    -- Read-only: an editing keystroke must not reach the buffer.
    let edTyped = fst (update (KChar 'X') edFmt)
    checkEq "rtf typing does not edit the buffer"
      (bufferToText LF False (edBuffer edTyped)) rtfDoc
    check "rtf typing is not treated as a modification" (not (edModified edTyped))
    -- ...but the raw view is an ordinary editable buffer.
    let edRawTyped = fst (update (KChar 'X') edRaw)
    check "rtf raw view accepts edits" (edModified edRawTyped)
    -- Only .rtf files offer the view.
    let edTxt = setLoaded "notes.txt"
                  (emptyLoadResult { lrBuffer = fromText rtfDoc })
                  (newEditor (24, 80) defaultConfig)
    check "non-rtf does not open formatted" (not (isJust' (edRtf edTxt)))
    check "non-rtf refuses the toggle" (not (isJust' (edRtf (toggleRtf edTxt))))
    -- Opening the file to land on a position in it (a workspace search hit, a
    -- definition site) matched the *markup*, which the formatted view neither
    -- shows nor has a cursor for — so that open stays in the raw view.
    let edJump = setLoaded "doc.rtf"
                   (emptyLoadResult { lrBuffer = fromText rtfDoc })
                   (newEditor (24, 80) defaultConfig)
                     { edPendingJump = Just ("doc.rtf", 1, 0, 3) }
    check "rtf open-with-pending-jump stays raw" (not (isJust' (edRtf edJump)))
    let edJumpOther = setLoaded "doc.rtf"
                        (emptyLoadResult { lrBuffer = fromText rtfDoc })
                        (newEditor (24, 80) defaultConfig)
                          { edPendingJump = Just ("other.txt", 1, 0, 3) }
    check "rtf open with a jump pending elsewhere still formats"
      (isJust' (edRtf edJumpOther))

    -- The derived view re-parses when the buffer moves under it. Every real
    -- buffer mutation bumps 'edEditSeq', which is what the view watches.
    let edEdited = refreshRtf edFmt { edBuffer = fromText (T.pack "{\\rtf1 One\\par Two\\par}")
                                    , edEditSeq = edEditSeq edFmt + 1 }
    checkEq "rtf view re-derives from a changed buffer"
      (fmap (Seq.length . rdPars) (edRtf edEdited)) (Just 2)
    check "rtf view does not re-derive when nothing was edited"
      (not (Rtf.rtfStale (edEditSeq edFmt) (maybe (error "no view") id (edRtf edFmt))))
    -- Scrolling must not re-parse. This is a *timing* test because the bug it
    -- guards was invisible to every structural check: the view looked right,
    -- it was simply rebuilt from scratch on each keystroke (a 1.6 MB document
    -- cost ~170 ms per arrow key). Compare a scroll against the one-time parse
    -- of the same document — the operation being accidentally repeated.
    do
      let bigRtf = T.concat
            [ T.pack "{\\rtf1\\ansi{\\colortbl;\\red0\\green0\\blue0;}"
            , T.concat [ T.pack ("\\pard\\ql Paragraph " ++ show i
                                 ++ " with \\b some bold\\b0  and \\i italic\\i0"
                                 ++ " text in it to wrap.\\par\n")
                       | i <- [1 :: Int .. 2000] ]
            , T.pack "}" ]
          edBig = setLoaded "big.rtf"
                    (emptyLoadResult { lrBuffer = fromText bigRtf })
                    (newEditor (30, 100) defaultConfig)
          scroll e = fst (update (KArrow DDown noMods) e)
      -- Force the view so the parse is not still a thunk when we time scrolls.
      _ <- evaluate (maybe 0 Rtf.rtfLineCount (edRtf edBig))
      tParse <- do
        t0 <- getMonotonicTime
        _ <- evaluate (Seq.length (Rtf.parseRtf bigRtf))
        t1 <- getMonotonicTime
        pure (t1 - t0)
      tScroll <- do
        t0 <- getMonotonicTime
        e <- evaluate (foldl' (\e' _ -> scroll e') edBig [1 :: Int .. 100])
        _ <- evaluate (maybe 0 rdTop (edRtf e))
        t1 <- getMonotonicTime
        pure (t1 - t0)
      -- 100 scrolls that each re-parsed would be ~100x one parse. Anything
      -- near or below a single parse means the cache is doing its job.
      check ("rtf 100 scrolls cost less than one parse (parse "
             ++ show (round (tParse * 1000) :: Int) ++ " ms, 100 scrolls "
             ++ show (round (tScroll * 1000) :: Int) ++ " ms)")
            (tScroll < max 0.05 tParse)

    -- No cursor in the formatted view: it has no position of its own, so the
    -- buffer's cursor would blink at an arbitrary spot in the rendered text.
    check "rtf formatted view shows no cursor"
      (not (isJust' (scrCursor (renderEditor edFmt))))
    check "rtf raw view does show a cursor"
      (isJust' (scrCursor (renderEditor edRaw)))

    -- The vertical scrollbar must drive the view the bar is measuring. It is
    -- drawn from 'scrollBarInfo' but moved by 'scrollBarTo', and those are two
    -- separate view splits: teaching only the first about a new mode gives a
    -- bar that tracks correctly and does nothing when you drag it.
    do
      let manyPars = T.concat
            [ T.pack "{\\rtf1\\ansi"
            , T.concat [ T.pack ("\\pard Paragraph number " ++ show i ++ ".\\par\n")
                       | i <- [1 :: Int .. 400] ]
            , T.pack "}" ]
          edS = setLoaded "bar.rtf"
                  (emptyLoadResult { lrBuffer = fromText manyPars })
                  (newEditor (24, 80) defaultConfig)
          topOf e = maybe (-1) rdTop (edRtf e)
          click row col pressed dragging =
            KMouse (MouseEvent MBLeft col row pressed dragging noMods 1)
      case scrollBarInfo edS of
        Nothing -> check "rtf scrollbar is showing for a long document" False
        Just (bx, btop, bh, total, _) -> do
          check "rtf scrollbar measures laid-out rows"
            (total == maybe 0 Rtf.rtfLineCount (edRtf edS))
          -- A press two-thirds down the track jumps roughly two-thirds in.
          let edMid = fst (update (click (btop + (2 * bh) `div` 3) bx True False) edS)
          check ("rtf scrollbar click scrolls the formatted view (top "
                 ++ show (topOf edMid) ++ " of " ++ show total ++ ")")
            (topOf edMid > total `div` 3)
          -- Dragging the thumb keeps moving it, and a release ends the drag.
          let edDrag = fst (update (click (btop + bh - 1) bx True True) edMid)
          check "rtf scrollbar drag keeps scrolling" (topOf edDrag > topOf edMid)
          let edUp = fst (update (click btop bx True True) edDrag)
          check "rtf scrollbar drag scrolls back up" (topOf edUp < topOf edDrag)
          checkEq "rtf scrollbar click never moves the hidden buffer cursor"
            (edCursor edMid) (edCursor edS)
          -- A click on the rendered document starts a selection in the *view*
          -- and never touches the buffer underneath it.
          let edText = fst (update (click 5 5 True False) edS)
          check "rtf text click starts a selection drag" (edMouseSelecting edText)
          checkEq "rtf text click leaves the buffer cursor alone"
            (edCursor edText) (edCursor edS)
          checkEq "rtf text click does not scroll" (topOf edText) (topOf edS)
          -- ...and releasing it ends the drag, so the scrollbar guard reopens.
          let edUpText = fst (update (click 5 5 False False) edText)
          check "rtf text release ends the selection drag"
            (not (edMouseSelecting edUpText))

    -- Scrolling is clamped to the laid-out document.
    let Just rd = edRtf edFmt
        h = 5
    checkEq "rtf scroll clamps at the top" (rdTop (Rtf.rtfScroll h (-99) rd)) 0
    check "rtf scroll clamps at the bottom"
      (rdTop (Rtf.rtfScroll h 999 rd) <= max 0 (Rtf.rtfLineCount rd - h))

  -- PDF reading view -----------------------------------------------------------
  -- The fixtures are built here rather than read from disk so the tests are
  -- hermetic: a PDF is text markup around a handful of streams, and a stored
  -- (uncompressed) DEFLATE block is a legal zlib stream, so even the
  -- FlateDecode path can be exercised without a compressor.
  do
    let ascii = BS.pack . map (fromIntegral . fromEnum)
        obj n body = ascii (show (n :: Int) ++ " 0 obj\n") <> body <> ascii "\nendobj\n"
        mkPdf objs = ascii "%PDF-1.4\n" <> BS.concat [ obj n b | (n, b) <- objs ]
                       <> ascii "trailer\n<< /Size 9 /Root 1 0 R >>\n%%EOF\n"
        stream dict body =
          ascii ("<< " ++ dict ++ " /Length " ++ show (BS.length body) ++ " >>\nstream\n")
            <> body <> ascii "\nendstream"
        -- A zlib stream whose single DEFLATE block is "stored": BFINAL=1,
        -- BTYPE=00, then the length, its complement, and the bytes.
        zlibStored raw =
          let n = BS.length raw
              lo v = fromIntegral (v .&. 0xff) :: Word8
              hi v = fromIntegral ((v `shiftR` 8) .&. 0xff) :: Word8
          in BS.pack [0x78, 0x01, 0x01, lo n, hi n, lo (65535 - n), hi (65535 - n)]
               <> raw <> BS.pack [0, 0, 0, 1]
        -- One page whose content stream is `body`, with `fontName` as /F1.
        onePage fontName body = mkPdf
          [ (1, ascii "<< /Type /Catalog /Pages 2 0 R >>")
          , (2, ascii "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
          , (3, ascii ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792]"
                       ++ " /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
          , (4, stream "" (ascii body))
          , (5, ascii ("<< /Type /Font /Subtype /Type1 /BaseFont /" ++ fontName
                       ++ " /Encoding /WinAnsiEncoding >>"))
          ]
        textOf pd = T.intercalate (T.pack "\n")
          [ T.concat (map Pdf.psText (Pdf.ppRuns par))
          | pg <- toList (pdPages pd), par <- Pdf.pgPars pg ]
        hex2 :: Char -> String
        hex2 c = let n = fromEnum c
                     d k = "0123456789ABCDEF" !! k
                 in [d (n `div` 16), d (n `mod` 16)]
        parse label bs = case Pdf.parsePdf bs of
          Left e   -> Nothing <$ check ("pdf parses " ++ label ++ " (" ++ e ++ ")") False
          Right pd -> pure (Just pd)

    check "pdf sniffs its header" (Pdf.sniffPdf (ascii "%PDF-1.7\n..."))
    -- Producers are allowed junk before the header, and a viewer that insisted
    -- on offset zero would refuse files every other reader opens.
    check "pdf sniffs a header after junk" (Pdf.sniffPdf (ascii (replicate 200 'x' ++ "%PDF-1.4")))
    check "pdf does not sniff plain text" (not (Pdf.sniffPdf (ascii "hello, world")))
    check "pdf does not sniff a late header"
      (not (Pdf.sniffPdf (ascii (replicate 4000 'x' ++ "%PDF-1.4"))))

    -- Text extraction, and the space that is not in the file. PDF records no
    -- word breaks: "Hello" ends at x=99.3 (Helvetica's own metrics at 12pt)
    -- and "world" starts at 110, and the gap is what has to become a space.
    Just pd1 <- parse "a one-page document" (onePage "Helvetica"
      "BT /F1 12 Tf 1 0 0 1 72 700 Tm (Hello) Tj 1 0 0 1 110 700 Tm (world) Tj ET")
    checkEq "pdf page count" (Pdf.pdfPageCount pd1) 1
    checkEq "pdf infers a space from the gap" (textOf pd1) (T.pack "Hello world")

    -- ...but a kerning adjustment is not a word break. TJ moves the next glyph
    -- by thousandths of an em; treating that as a gap would space out every
    -- kerned pair in the document.
    Just pd2 <- parse "a kerned string" (onePage "Helvetica"
      "BT /F1 12 Tf 1 0 0 1 72 700 Tm [(Hel) -20 (lo) -15 (,) 0 ( there)] TJ ET")
    checkEq "pdf kerning does not become a space" (textOf pd2) (T.pack "Hello, there")

    -- WinAnsi is the encoding nearly every simple font declares, and its
    -- 0x80..0x9F range is where the curly quotes and dashes live.
    Just pd3 <- parse "cp1252 bytes" (onePage "Helvetica"
      "BT /F1 12 Tf 1 0 0 1 72 700 Tm (\\223q\\224\\227\\351) Tj ET")
    checkEq "pdf decodes WinAnsi" (textOf pd3) (T.pack "\8220q\8221\8212\233")

    -- Weight and slope come from the font's name, since PDF has no way to say
    -- "the bold version of this run".
    Just pdB <- parse "a bold font" (onePage "Helvetica-BoldOblique"
      "BT /F1 12 Tf 1 0 0 1 72 700 Tm (Heavy) Tj ET")
    check "pdf reads bold and italic off the font name"
      (all (\s -> Pdf.pfBold (Pdf.psFmt s) && Pdf.pfItalic (Pdf.psFmt s))
           [ s | pg <- toList (pdPages pdB), par <- Pdf.pgPars pg, s <- Pdf.ppRuns par ])

    -- Ligatures are what a font honestly reports and what no terminal font
    -- has; spelling them out is the difference between "first" and a hole.
    Just pdL <- parse "a ligature" (mkPdf
      [ (1, ascii "<< /Type /Catalog /Pages 2 0 R >>")
      , (2, ascii "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
      , (3, ascii ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792]"
                   ++ " /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
      , (4, stream "" (ascii "BT /F1 12 Tf 1 0 0 1 72 700 Tm (\\001rst) Tj ET"))
      , (5, ascii ("<< /Type /Font /Subtype /Type1 /BaseFont /AAAAAA+Minion"
                   ++ " /ToUnicode 6 0 R >>"))
      , (6, stream "" (ascii ("begincodespacerange <00> <ff> endcodespacerange\n"
                             ++ "1 beginbfchar <01> <FB01> endbfchar")))
      ])
    checkEq "pdf expands the fi ligature" (textOf pdL) (T.pack "first")

    -- FlateDecode is how essentially every real content stream arrives.
    let flatePdf = mkPdf
          [ (1, ascii "<< /Type /Catalog /Pages 2 0 R >>")
          , (2, ascii "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
          , (3, ascii ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792]"
                       ++ " /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
          , (4, stream "/Filter /FlateDecode"
                  (zlibStored (ascii "BT /F1 12 Tf 1 0 0 1 72 700 Tm (Compressed) Tj ET")))
          , (5, ascii "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
          ]
    Just pdF <- parse "a compressed content stream" flatePdf
    checkEq "pdf inflates a content stream" (textOf pdF) (T.pack "Compressed")

    -- ASCIIHexDecode, and a filter chain, since /Filter may be an array.
    Just pdH <- parse "a hex-encoded stream" (mkPdf
      [ (1, ascii "<< /Type /Catalog /Pages 2 0 R >>")
      , (2, ascii "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
      , (3, ascii ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792]"
                   ++ " /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
      , (4, stream "/Filter [/ASCIIHexDecode]"
              (ascii (concatMap hex2 ("BT /F1 12 Tf 1 0 0 1 72 700 Tm (Hex) Tj ET" :: String) ++ ">")))
      , (5, ascii "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
      ])
    checkEq "pdf decodes an ASCIIHex stream" (textOf pdH) (T.pack "Hex")

    -- Objects inside a compressed object stream are invisible to the scan that
    -- finds every other object, so they are expanded separately — and every
    -- PDF written since 1.5 puts its catalog there.
    let objStmBody = ascii "1 0 " <> ascii "<< /Type /Catalog /Pages 2 0 R >>"
    Just pdO <- parse "an object stream" (mkPdf
      [ (7, stream "/Type /ObjStm /N 1 /First 4 /Filter /FlateDecode" (zlibStored objStmBody))
      , (2, ascii "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
      , (3, ascii ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792]"
                   ++ " /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
      , (4, stream "" (ascii "BT /F1 12 Tf 1 0 0 1 72 700 Tm (Streamed) Tj ET"))
      , (5, ascii "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
      ])
    checkEq "pdf finds a catalog inside an object stream" (textOf pdO) (T.pack "Streamed")

    -- A form XObject is a nested content stream with its own matrix; text
    -- drawn inside one is document text like any other.
    Just pdX <- parse "a form xobject" (mkPdf
      [ (1, ascii "<< /Type /Catalog /Pages 2 0 R >>")
      , (2, ascii "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
      , (3, ascii ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792]"
                   ++ " /Resources << /Font << /F1 5 0 R >> /XObject << /X1 6 0 R >> >>"
                   ++ " /Contents 4 0 R >>"))
      , (4, stream "" (ascii "/X1 Do"))
      , (5, ascii "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
      , (6, stream "/Type /XObject /Subtype /Form /BBox [0 0 612 792]"
              (ascii "BT /F1 12 Tf 1 0 0 1 72 700 Tm (Inside) Tj ET"))
      ])
    checkEq "pdf runs a form xobject" (textOf pdX) (T.pack "Inside")

    -- /Rotate turns the page for display, and a landscape page normally draws
    -- its text rotated to match — so the two compose, and getting either the
    -- direction or the endpoints wrong collapses every line of such a page
    -- into one. The text matrix here advances along +y.
    Just pdR <- parse "a rotated landscape page" (mkPdf
      [ (1, ascii "<< /Type /Catalog /Pages 2 0 R >>")
      , (2, ascii "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
      , (3, ascii ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Rotate 90"
                   ++ " /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
      , (4, stream "" (ascii (concat
              [ "BT /F1 12 Tf 0 1 -1 0 " ++ show (100 + 16 * i :: Int)
                ++ " 100 Tm (Landscape " ++ show i ++ ") Tj ET\n"
              | i <- [0 .. 3 :: Int] ])))
      , (5, ascii "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
      ])
    check "pdf reads a rotated page in order"
      (T.pack "Landscape 0" `T.isInfixOf` textOf pdR
         && T.pack "Landscape 3" `T.isInfixOf` textOf pdR
         && T.count (T.pack "Landscape") (textOf pdR) == 4)
    check "pdf keeps a rotated page's lines apart"
      (not (T.pack "Landscape 0Landscape" `T.isInfixOf` textOf pdR))

    -- An /Encrypt entry means the streams are ciphertext. Saying so beats
    -- showing a document made of noise.
    let encPdf = ascii "%PDF-1.4\n"
          <> obj 1 (ascii "<< /Type /Catalog /Pages 2 0 R >>")
          <> ascii "trailer\n<< /Size 9 /Root 1 0 R /Encrypt 8 0 R >>\n%%EOF\n"
    check "pdf reports an encrypted file" $ case Pdf.parsePdf encPdf of
      Left e  -> "encrypted" `isInfixOf` e
      Right _ -> False

    -- Malformed input must degrade, not diverge or throw.
    forM_ [ "%PDF-1.4", "%PDF-1.4\n1 0 obj\n<< /Type", "%PDF-1.4\n1 0 obj\nstream\n"
          , "%PDF-1.4\n<< >>\ntrailer", "%PDF-1.4\n0 0 obj\n(", "%PDF-1.4\n1 0 obj\n[[[[" ] $ \s -> do
      r <- try (evaluate (either length (const 0) (Pdf.parsePdf (ascii s))))
             :: IO (Either SomeException Int)
      check ("pdf survives " ++ show s) (either (const False) (const True) r)

    -- A multi-page document, which is what page navigation moves between.
    -- Long enough that it overflows a terminal window, since the interactive
    -- assertions below are about scrolling as well as paging.
    let pageBody n = ascii (concat
          [ "BT /F1 11 Tf 1 0 0 1 72 " ++ show (700 - 14 * i :: Int)
            ++ " Tm (Page " ++ show (n :: Int) ++ " line " ++ show i
            ++ " with several more words on it so that the line is a long one.) Tj ET\n"
          | i <- [0 .. 19 :: Int] ])
        nPages = 3 :: Int
        manyPages = mkPdf $
          [ (1, ascii "<< /Type /Catalog /Pages 2 0 R >>")
          , (2, ascii ("<< /Type /Pages /Count " ++ show nPages ++ " /Kids ["
                       ++ concat [ show (10 + k) ++ " 0 R " | k <- [0 .. nPages - 1] ] ++ "] >>"))
          , (5, ascii "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
          ] ++ concat
          [ [ (10 + k, ascii ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792]"
                              ++ " /Resources << /Font << /F1 5 0 R >> >> /Contents "
                              ++ show (20 + k) ++ " 0 R >>"))
            , (20 + k, stream "" (pageBody (k + 1))) ]
          | k <- [0 .. nPages - 1] ]
    Just pdP <- parse "a multi-page document" manyPages
    checkEq "pdf counts every page" (Pdf.pdfPageCount pdP) nPages

    -- Layout: every laid-out line fits the width it was laid out for, and the
    -- spans stay inside their line's text (the renderer indexes with them).
    forM_ [20, 40, 80] $ \w -> do
      let (ls, starts) = Pdf.layoutPdf 8 w (pdPages pdP)
      check ("pdf layout fits width " ++ show w)
        (all (\l -> Pdf.plPad l + lineDisplayWidth 8 (Pdf.plText l) <= w) (toList ls))
      check ("pdf layout spans in range " ++ show w)
        (all (\l -> all (\(s, e, _) -> s >= 0 && e <= T.length (Pdf.plText l) && s <= e)
                        (Pdf.plSpans l)) (toList ls))
      checkEq ("pdf layout records one start per page at width " ++ show w)
        (Seq.length starts) nPages
      check ("pdf page starts are ordered and in range at width " ++ show w)
        (and [ a < b | (a, b) <- zip (toList starts) (drop 1 (toList starts)) ]
           && all (< Seq.length ls) (toList starts))
    -- Narrower wrapping can only produce more lines, never fewer.
    check "pdf narrower wraps to more lines"
      (Seq.length (fst (Pdf.layoutPdf 8 30 (pdPages pdP)))
         >= Seq.length (fst (Pdf.layoutPdf 8 80 (pdPages pdP))))

    -- Page navigation.
    let laidOut = Pdf.pdfRelayout 8 60 10 pdP
        h = 10
    checkEq "pdf starts on page 1" (Pdf.pdfCurrentPage laidOut) 1
    checkEq "pdf ] turns the page" (Pdf.pdfCurrentPage (Pdf.pdfNextPage h laidOut)) 2
    checkEq "pdf [ turns back"
      (Pdf.pdfCurrentPage (Pdf.pdfPrevPage h (Pdf.pdfNextPage h laidOut))) 1
    checkEq "pdf go-to-page clamps above the last page"
      (Pdf.pdfCurrentPage (Pdf.pdfGoToPage h 99 laidOut)) nPages
    checkEq "pdf go-to-page clamps below the first"
      (Pdf.pdfCurrentPage (Pdf.pdfGoToPage h (-3) laidOut)) 1
    -- Scrolled into the middle of a page, "back" means the top of this one.
    let onTwo = Pdf.pdfGoToPage h 2 laidOut
        midway = Pdf.pdfScroll h 1 onTwo
    check "pdf scrolling within a page stays on it" (Pdf.pdfCurrentPage midway == 2)
    checkEq "pdf [ from mid-page returns to that page's top"
      (pdTop (Pdf.pdfPrevPage h midway)) (pdTop onTwo)
    checkEq "pdf scroll clamps at the top" (pdTop (Pdf.pdfScroll h (-99) laidOut)) 0
    check "pdf scroll clamps at the bottom"
      (pdTop (Pdf.pdfScroll h 999 laidOut) <= max 0 (Pdf.pdfLineCount laidOut - h))
    -- Re-laying out at the same width must not rebuild: scrolling a long
    -- document would otherwise re-wrap it on every keypress.
    let scrolled = Pdf.pdfScroll h 3 laidOut
    checkEq "pdf relayout at the same width is a no-op"
      (pdTop (Pdf.pdfRelayout 8 60 h scrolled)) (pdTop scrolled)

    -- The view in the editor: read-only, no cursor, and Go To means pages.
    let edPdfDoc = pdfLoaded "paper.pdf" laidOut (newEditor (24, 80) defaultConfig)
    check "pdf opens in the reading view" (isJust' (edPdf edPdfDoc))
    check "pdf document is read-only" (edReadOnly edPdfDoc)
    check "pdf view has no buffer" (lineCount (edBuffer edPdfDoc) <= 1)
    check "pdf view shows no cursor" (not (isJust' (scrCursor (renderEditor edPdfDoc))))
    let edTypedPdf = fst (update (KChar 'X') edPdfDoc)
    check "pdf typing does not modify anything" (not (edModified edTypedPdf))
    checkEq "pdf typing leaves the page where it was"
      (fmap pdTop (edPdf edTypedPdf)) (fmap pdTop (edPdf edPdfDoc))
    -- ] and [ are the page keys, and they must not fall through to the
    -- read-only note that swallows every other printable character.
    let edNext = feed edPdfDoc [KChar ']']
        edBack = feed edNext [KChar '[']
    checkEq "pdf ] turns to page 2 in the editor"
      (fmap Pdf.pdfCurrentPage (edPdf edNext)) (Just 2)
    checkEq "pdf [ turns back in the editor"
      (fmap Pdf.pdfCurrentPage (edPdf edBack)) (Just 1)
    -- Go To is worded and read as pages here, not lines.
    let edGoTo = fst (update (KCtrlChar 'g') edPdfDoc)
    checkEq "pdf Ctrl+G opens Go to Page"
      (fmap dlgTitle (edDialog edGoTo)) (Just (T.pack "Go to Page"))
    let edPage2 = feed edGoTo [KChar '2', KEnter]
    checkEq "pdf go-to-page jumps"
      (fmap Pdf.pdfCurrentPage (edPdf edPage2)) (Just 2)
    -- Save would have to write a PDF, which this view cannot do.
    let edSavePdf = feed edPdfDoc [KCtrlChar 's']
    check "pdf refuses to save" (T.unpack (edStatus edSavePdf) /= "")
    -- ...and neither can anything else that writes.
    forM_ [KCtrlChar 'v', KCtrlChar 'x'] $ \k ->
      check ("pdf refuses " ++ show k)
        (fmap Pdf.pdfLineCount (edPdf (feed edPdfDoc [k]))
           == fmap Pdf.pdfLineCount (edPdf edPdfDoc))

    -- Selection, which exists so that a passage can be copied out.
    let firstLine = maybe (T.pack "") (\p -> Pdf.pdfLineText p 0) (edPdf edPdfDoc)
        selAll = feed edPdfDoc [KCtrlChar 'a']
    check "pdf select all covers the document"
      (case edPdf selAll >>= Pdf.pdfSelection of
         Just (Pos 0 0, Pos l _) -> l == maybe 0 (subtract 1 . Pdf.pdfLineCount) (edPdf selAll)
         _ -> False)
    check "pdf select-all text starts with the first line"
      (T.isPrefixOf firstLine (maybe (T.pack "") Pdf.pdfSelText (edPdf selAll)))
    -- Copy puts the selected text on the clipboard and emits the effect the
    -- driver turns into an OSC 52 / xclip write.
    let (edCopied, copyEffs) = update (KCtrlChar 'c') selAll
    check "pdf copy emits a clipboard effect" (not (null copyEffs))
    check "pdf copy fills the clipboard with the selection"
      (edClipboard edCopied == maybe (T.pack "") Pdf.pdfSelText (edPdf selAll))
    check "pdf copy leaves the document alone" (not (edModified edCopied))
    -- With nothing selected there is nothing to copy: the text view's
    -- "copy the current line" fallback needs a cursor this view has not got.
    let (edCopyNone, copyNoneEffs) = update (KCtrlChar 'c') edPdfDoc
    check "pdf copy with no selection copies nothing" (null copyNoneEffs)
    check "pdf copy with no selection says so" (T.unpack (edStatus edCopyNone) /= "")
    -- No selection means no cursor; making one puts the cursor at its end.
    check "pdf shows no cursor without a selection"
      (not (isJust' (scrCursor (renderEditor edPdfDoc))))
    check "pdf Esc drops the selection"
      (not (isJust' (edPdf (feed selAll [KEsc]) >>= Pdf.pdfSelection)))
    -- Shift+movement extends from the caret; plain movement scrolls the
    -- window and must leave the selection alone (you scroll to see the rest
    -- of what you highlighted).
    let edShifted = feed edPdfDoc [KArrow DRight shiftMods, KArrow DRight shiftMods]
    check "pdf shows a cursor while selecting"
      (isJust' (scrCursor (renderEditor edShifted)))
    -- Select All does not move the window, so its caret (the last line) is
    -- off screen and there is nothing on screen to point at.
    check "pdf shows no cursor for an off-screen caret"
      (not (isJust' (scrCursor (renderEditor selAll))))
    checkEq "pdf shift+right selects two characters"
      (T.length (maybe (T.pack "") Pdf.pdfSelText (edPdf edShifted))) 2
    let edScrolled = feed edShifted [KArrow DDown noMods, KArrow DDown noMods]
    checkEq "pdf plain scrolling keeps the selection"
      (fmap Pdf.pdfSelText (edPdf edScrolled)) (fmap Pdf.pdfSelText (edPdf edShifted))
    check "pdf plain scrolling still scrolls"
      (fmap pdTop (edPdf edScrolled) == Just 2)
    -- Mouse: a drag across the first line selects it; a plain click leaves no
    -- selection (and so no cursor) behind.
    let mouseAt r c pressed dragging =
          KMouse (MouseEvent MBLeft c r pressed dragging noMods 1)
        edDragged = feed edPdfDoc [ mouseAt 1 0 True False, mouseAt 1 12 True True
                                  , mouseAt 1 12 False False ]
    checkEq "pdf mouse drag selects what it crossed"
      (maybe (T.pack "") Pdf.pdfSelText (edPdf edDragged)) (T.take 12 firstLine)
    let edClicked = feed edPdfDoc [ mouseAt 1 5 True False, mouseAt 1 5 False False ]
    check "pdf a plain click selects nothing"
      (not (isJust' (edPdf edClicked >>= Pdf.pdfSelection)))
    let edDoubled = feed edPdfDoc [KMouse (MouseEvent MBLeft 2 1 True False noMods 2)]
    check "pdf double-click takes a word"
      (let t = maybe (T.pack "") Pdf.pdfSelText (edPdf edDoubled)
       in not (T.null t) && T.all (\c -> c /= ' ') t)

    -- Find, over the laid-out text: it is the only text this view has, and
    -- the found range becomes the selection so Find and copy compose.
    let edFound = feed edPdfDoc [KCtrlChar 'f', KChar 'P', KChar 'a'
                                , KChar 'g', KChar 'e', KEnter]
    check "pdf Ctrl+F opens the Find dialog"
      (isJust' (edDialog (feed edPdfDoc [KCtrlChar 'f'])))
    checkEq "pdf find selects the match"
      (maybe (T.pack "") Pdf.pdfSelText (edPdf edFound)) (T.pack "Page")
    check "pdf find reports which match it is"
      ("Match 1 of " `isPrefixOf` T.unpack (edStatus edFound))
    -- F3 advances, and the two hits are not the same one.
    let edNext2 = feed edFound [KFn 3 noMods]
    check "pdf find next advances"
      (fmap pdCaret (edPdf edNext2) /= fmap pdCaret (edPdf edFound))
    checkEq "pdf find next still selects the term"
      (maybe (T.pack "") Pdf.pdfSelText (edPdf edNext2)) (T.pack "Page")
    -- Searching backwards from there returns to the first hit.
    let edPrev = feed edNext2 [KFn 3 shiftMods]
    check "pdf find previous goes back"
      (fmap pdCaret (edPdf edPrev) == fmap pdCaret (edPdf edFound))
    -- A term that is not there says so and leaves the view where it was.
    let edMiss = feed edPdfDoc [KCtrlChar 'f', KChar 'z', KChar 'q', KChar 'x', KEnter]
    check "pdf find reports a miss" ("Not found" `isPrefixOf` T.unpack (edStatus edMiss))
    check "pdf a missed find selects nothing"
      (not (isJust' (edPdf edMiss >>= Pdf.pdfSelection)))
    -- A find scrolls its hit into view rather than leaving it off screen.
    let edFar = feed edPdfDoc [KCtrlChar 'f', KChar 'S', KChar 'e', KChar 'c'
                              , KChar 'o', KChar 'n', KChar 'd', KEnter]
    check "pdf find scrolls the hit into view"
      (case edPdf edFar of
         Just p -> let Pos l _ = pdCaret p
                   in l >= pdTop p && l < pdTop p + pdfHeight edFar
         Nothing -> False)
    -- The selection has to be *painted*, not merely held: a highlight the
    -- renderer does not draw is a selection nobody can see they have.
    do
      let scr = renderEditor edShifted
          loSel = computeLayout edShifted
          cellRC r c = scrCells scr A.! (r * scrW scr + c)
          pad = maybe 0 Pdf.plPad (edPdf edShifted >>= \p -> Seq.lookup 0 (Pdf.pdfLines p))
          row = loTextTop loSel
          selSty = thSelection (themeFor (resolvedTheme edShifted))
      check "pdf paints the selected cells"
        (all (\c -> cellStyle (cellRC row (loTextLeft loSel + pad + c)) == selSty) [0, 1])
      check "pdf leaves unselected cells alone"
        (cellStyle (cellRC row (loTextLeft loSel + pad + 2)) /= selSty)
    -- Every match highlights while the Find dialog is open, the way it does
    -- in the text view.
    do
      let edLive = feed edPdfDoc [KCtrlChar 'f', KChar 'P', KChar 'a', KChar 'g', KChar 'e']
          scr = renderEditor edLive
          loLive = computeLayout edLive
          cellRC r c = scrCells scr A.! (r * scrW scr + c)
          matchSty = thFindMatch (themeFor (resolvedTheme edLive))
          rowCells r = [ cellStyle (cellRC r c) | c <- [0 .. scrW scr - 1] ]
      check "pdf highlights live find matches"
        (any (\r -> matchSty `elem` rowCells r)
             [loTextTop loLive .. loTextTop loLive + loTextHeight loLive - 1])

    -- Re-wrapping invalidates line/character indices, so the selection goes
    -- rather than pointing at whatever now sits there.
    check "pdf a re-wrap drops the selection"
      (not (isJust' (Pdf.pdfSelection (Pdf.pdfRelayout 8 41 10 (Pdf.pdfSelectAll laidOut)))))
    -- The scrollbar must drive the view it is measuring.
    let edWide = pdfLoaded "paper.pdf" (Pdf.pdfRelayout 8 79 20 pdP)
                   (newEditor (24, 80) defaultConfig)
    case scrollBarInfo edWide of
      Nothing -> check "pdf scrollbar is showing for a long document" False
      Just (bx, btop, bh, total, _) -> do
        checkEq "pdf scrollbar measures laid-out rows"
          total (maybe 0 Pdf.pdfLineCount (edPdf edWide))
        let clickAt row = KMouse (MouseEvent MBLeft bx row True False noMods 1)
            edMid = fst (update (clickAt (btop + bh - 1)) edWide)
        check "pdf scrollbar click scrolls the view"
          (maybe 0 pdTop (edPdf edMid) > 0)

  -- ZIP archive listing -------------------------------------------------------
  -- The archives are built here rather than read from disk so the tests are
  -- hermetic and portable: a stored (uncompressed) archive is a handful of
  -- fixed-layout records, which is exactly what the reader parses. Nothing is
  -- decompressed by the reader, so stored members exercise all of it.
  do
    let le16, le32 :: Int -> BS.ByteString
        le16 n = BS.pack [fromIntegral (n .&. 0xff), fromIntegral ((n `shiftR` 8) .&. 0xff)]
        le32 n = le16 (n .&. 0xffff) <> le16 ((n `shiftR` 16) .&. 0xffff)
        utf8 = TE.encodeUtf8 . T.pack
        -- (name, contents, general-purpose flag word)
        mkZip :: [(BS.ByteString, BS.ByteString, Int)] -> BS.ByteString -> BS.ByteString
        mkZip items comment =
          let local (nm, dat, fl) =
                BS.concat [ le32 0x04034b50, le16 20, le16 fl, le16 0, le16 0, le16 0
                          , le32 0, le32 (BS.length dat), le32 (BS.length dat)
                          , le16 (BS.length nm), le16 0, nm, dat ]
              locals  = map local items
              offsets = scanl (+) 0 (map BS.length locals)
              central (off, (nm, dat, fl)) =
                BS.concat [ le32 0x02014b50, le16 20, le16 20, le16 fl, le16 0
                          , le16 0, le16 0, le32 0
                          , le32 (BS.length dat), le32 (BS.length dat)
                          , le16 (BS.length nm), le16 0, le16 0, le16 0, le16 0
                          , le32 0, le32 off, nm ]
              cds   = map central (zip offsets items)
              cdOff = sum (map BS.length locals)
              cdSz  = sum (map BS.length cds)
              eocd  = BS.concat [ le32 0x06054b50, le16 0, le16 0
                                , le16 (length items), le16 (length items)
                                , le32 cdSz, le32 cdOff
                                , le16 (BS.length comment), comment ]
          in BS.concat (locals ++ cds ++ [eocd])
        -- What the driver does: find the end record in the tail, then read the
        -- central directory it points at.
        readZip bs =
          case Z.findEocd bs 0 of
            Left e   -> Left e
            Right ec -> fmap ((,) ec) (Z.parseCentral (Z.ecPrefix ec)
              (BS.take (fromInteger (Z.ecCdSize ec)) (BS.drop (fromInteger (Z.ecCdOff ec)) bs)))

        plain nm dat = (utf8 nm, utf8 dat, 0x800)   -- bit 11: the name is UTF-8
        archive = mkZip [ plain "README.md" "hello"
                        , plain "src/" ""
                        , plain "src/main.c" "int main(void){}"
                        , plain "src/util/str.h" "x"
                        , (utf8 "secret.txt", utf8 "\0\0\0\0", 0x801)  -- bit 0: encrypted
                        ] (utf8 "an archive comment")

    check "zip magic recognises a local header" (Z.zipMagic archive)
    check "zip magic rejects text" (not (Z.zipMagic (utf8 "PKZIP is not a zip")))
    check "zip magic rejects a short file" (not (Z.zipMagic (BS.take 3 archive)))

    case readZip archive of
      Left e -> check ("zip parses the archive we built (" ++ e ++ ")") False
      Right (ec, es) -> do
        checkEq "zip entry count" (length es) 5
        checkEq "zip comment" (Z.ecComment ec) (T.pack "an archive comment")
        checkEq "zip member names"
          (map Z.zeName es)
          (map T.pack ["README.md", "src/", "src/main.c", "src/util/str.h", "secret.txt"])
        checkEq "zip member size" (map Z.zeSize (take 1 es)) [5]
        check "zip trailing slash is a directory" (Z.zeDir (es !! 1))
        check "zip a file is not a directory" (not (Z.zeDir (head es)))
        check "zip general-purpose bit 0 is encryption" (Z.zeEncrypted (last es))
        check "zip an unflagged member is not encrypted" (not (any Z.zeEncrypted (init es)))

        -- The listing is the whole user-visible surface, so assert on it.
        let listing = Z.zipListing "demo.zip" (toInteger (BS.length archive))
                        es (Z.ecComment ec)
            lns = T.lines listing
            has s = any (T.isInfixOf (T.pack s)) lns
        check "zip listing names the archive" (has "demo.zip")
        check "zip listing counts files and folders" (has "4 files in 2 folders")
        check "zip listing reports the comment" (has "an archive comment")
        check "zip listing flags encrypted members" (has "encrypted")
        check "zip listing draws a tree" (has "\x251c\x2500\x2500 " && has "\x2514\x2500\x2500 ")
        check "zip listing marks directories with a slash" (has "src/")
        -- A directory only implied by its members still gets a row: plenty of
        -- archivers write no entry for one.
        check "zip listing synthesises an implied directory" (has "util/")
        check "zip listing shows every member"
          (all (\n -> has n) ["README.md", "main.c", "str.h", "secret.txt"])
        -- The listing is a description, never the archive's bytes.
        check "zip listing is not the archive" (not (has "PK"))

    -- A self-extracting archive is a stub followed by an ordinary ZIP, whose
    -- recorded offsets are relative to the ZIP rather than to the file.
    case readZip (BS.replicate 5000 0x58 <> archive) of
      Left e -> check ("zip reads past a self-extracting stub (" ++ e ++ ")") False
      Right (_, es) -> checkEq "zip reads past a self-extracting stub" (length es) 5

    -- An archive with no members is valid: the end record is the whole file.
    case readZip (mkZip [] BS.empty) of
      Left e -> check ("zip reads an empty archive (" ++ e ++ ")") False
      Right (_, es) -> do
        checkEq "zip empty archive has no members" (length es) 0
        check "zip empty archive says so"
          (T.isInfixOf (T.pack "empty") (Z.zipListing "e.zip" 22 [] T.empty))

    -- Names are CP437 unless bit 11 says UTF-8; guessing wrong mangles them.
    case readZip (mkZip [(BS.pack [0x63, 0x61, 0x66, 0x82], BS.empty, 0)] BS.empty) of
      Left e -> check ("zip decodes a CP437 name (" ++ e ++ ")") False
      Right (_, es) -> checkEq "zip decodes a CP437 name"
        (map Z.zeName es) [T.pack "caf\233"]

    -- Malformed input must be a Left, not a crash or a hang: these bytes come
    -- from a file we did not write.
    forM_ [ ("truncated", BS.take 40 archive)
          , ("headless", BS.drop 40 archive)
          , ("empty", BS.empty)
          , ("tiny", utf8 "PK\3\4")
          , ("no end record", BS.replicate 200 0x41)
          ] $ \(what, bs) ->
      check ("zip survives " ++ what)
        (case readZip bs of Left _ -> True; Right (_, es) -> length es >= 0)

  -- Cmedit.Xml (plan 0021) ----------------------------------------------------
  -- The shared parser under DOCX, XLSX and EPUB. It is non-validating by
  -- design, so most of what is asserted here is that malformed input still
  -- yields the text it looks like rather than nothing at all.
  do
    let px = X.parseXml . T.pack
        texts es = [ t | X.XText t <- es ]
        names es = [ n | X.XStart n _ <- es ]
        allText es = T.concat (texts es)

    checkEq "xml elements and text"
      (px "<a><b>hi</b></a>") [X.XStart "a" [], X.XStart "b" [], X.XText "hi", X.XEnd "b", X.XEnd "a"]
    -- Self-closing tags produce both halves, so a consumer with a stack never
    -- has to special-case them.
    checkEq "xml self-closing emits both halves"
      (px "<br/>") [X.XStart "br" [], X.XEnd "br"]
    -- Namespace prefixes are dropped: OOXML producers disagree about them and
    -- every consumer here matches on the local name.
    checkEq "xml matches on the local name"
      (names (px "<w:p><w:r/></w:p>")) ["p", "r"]
    checkEq "xml attribute names are local too"
      (px "<c r=\"A1\" t=\"s\"/>")
      [X.XStart "c" [("r", "A1"), ("t", "s")], X.XEnd "c"]
    checkEq "xml single-quoted attributes"
      (px "<a b='x y'/>") [X.XStart "a" [("b", "x y")], X.XEnd "a"]
    -- XML forbids both of these; real documents contain both.
    checkEq "xml unquoted attribute"
      (px "<a b=x/>") [X.XStart "a" [("b", "x")], X.XEnd "a"]
    checkEq "xml valueless attribute"
      (px "<a hidden/>") [X.XStart "a" [("hidden", "")], X.XEnd "a"]

    checkEq "xml resolves the five built-in entities"
      (allText (px "<a>&amp;&lt;&gt;&quot;&apos;</a>")) (T.pack "&<>\"'")
    checkEq "xml resolves numeric entities"
      (allText (px "<a>&#65;&#x42;&#8212;</a>")) (T.pack "AB\x2014")
    checkEq "xml resolves the html entities xhtml uses without a dtd"
      (allText (px "<a>&nbsp;&mdash;&rsquo;</a>")) (T.pack "\xa0\x2014\x2019")
    -- An unknown reference is shown, not swallowed: deleting text silently is
    -- worse than printing something odd.
    checkEq "xml leaves an unknown entity alone"
      (allText (px "<a>a&foo;b</a>")) (T.pack "a&foo;b")
    checkEq "xml text with no ampersand is untouched"
      (allText (px "<a>plain text</a>")) (T.pack "plain text")

    checkEq "xml keeps CDATA verbatim"
      (allText (px "<a><![CDATA[x < y & z]]></a>")) (T.pack "x < y & z")
    checkEq "xml skips comments" (allText (px "<a>x<!-- <b>no</b> -->y</a>")) (T.pack "xy")
    checkEq "xml skips processing instructions"
      (allText (px "<?xml version=\"1.0\"?><a>x</a>")) (T.pack "x")
    checkEq "xml skips a doctype with an internal subset"
      (allText (px "<!DOCTYPE a [<!ENTITY x \"y\">]><a>hi</a>")) (T.pack "hi")

    -- Malformed input: never an exception, never empty.
    checkEq "xml survives an unclosed element" (names (px "<a><b>text")) ["a", "b"]
    checkEq "xml survives a stray close" (names (px "</b><a/>")) ["a"]
    checkEq "xml renders a bare less-than as itself"
      (allText (px "<a>3 < 4</a>")) (T.pack "3 < 4")
    check "xml survives a truncated tag" (length (px "<a b=\"c") >= 0)
    check "xml survives an unterminated comment" (px "<a><!-- x" == [X.XStart "a" []])

    -- Nesting is bounded: a file built to nest a million elements must stop.
    let deep n = T.concat (replicate n (T.pack "<x>")) <> T.pack "deep"
    check "xml bounds nesting depth"
      (length (X.parseXml (deep (X.maxXmlDepth + 50))) <= X.maxXmlDepth + 1)

    checkEq "xml elemText gathers a subtree's text"
      (fst (X.elemText (drop 1 (px "<si><r><t>a</t></r><t>b</t></si>")))) (T.pack "ab")
    checkEq "xml elemText stops at the matching close"
      (names (snd (X.elemText (drop 1 (px "<si><t>a</t></si><z/>"))))) ["z"]

    checkEq "xml decodes a utf-8 BOM"
      (X.decodeXmlBytes (BS.pack [0xEF, 0xBB, 0xBF, 0x68, 0x69])) (T.pack "hi")
    checkEq "xml decodes utf-16 by its BOM"
      (X.decodeXmlBytes (BS.pack [0xFF, 0xFE, 0x68, 0x00, 0x69, 0x00])) (T.pack "hi")
    checkEq "xml reads an integer attribute"
      (X.xAttrInt "n" [("n", "1440")]) (Just 1440)
    checkEq "xml reads a negative integer attribute"
      (X.xAttrInt "n" [("n", "-360")]) (Just (-360))
    checkEq "xml rejects a non-numeric attribute"
      (X.xAttrInt "n" [("n", "auto")]) Nothing

  -- ZIP member extraction (plan 0021) ------------------------------------------
  -- The listing never decompresses; the container reading views do. Stored
  -- members exercise the header walk, and one hand-made DEFLATE stream
  -- exercises the other branch.
  do
    let le16, le32 :: Int -> BS.ByteString
        le16 n = BS.pack [fromIntegral (n .&. 0xff), fromIntegral ((n `shiftR` 8) .&. 0xff)]
        le32 n = le16 (n .&. 0xffff) <> le16 ((n `shiftR` 16) .&. 0xffff)
        utf8 = TE.encodeUtf8 . T.pack
        -- (name, stored bytes, raw bytes, method, flags, local extra field).
        -- The local extra field is deliberately a different length from the
        -- central one: a reader that assumed they matched would land in the
        -- middle of the data.
        mk items =
          let local (nm, _usz, dat, meth, fl, ex) =
                BS.concat [ le32 0x04034b50, le16 20, le16 fl, le16 meth, le16 0, le16 0
                          , le32 0, le32 (BS.length dat), le32 (BS.length dat)
                          , le16 (BS.length nm), le16 (BS.length ex), nm, ex, dat ]
              locals  = map local items
              offsets = scanl (+) 0 (map BS.length locals)
              central (off, (nm, usz, dat, meth, fl, _ex)) =
                BS.concat [ le32 0x02014b50, le16 20, le16 20, le16 fl, le16 meth
                          , le16 0, le16 0, le32 0
                          , le32 (BS.length dat), le32 usz
                          , le16 (BS.length nm), le16 0, le16 0, le16 0, le16 0
                          , le32 0, le32 off, nm ]
              cds   = map central (zip offsets items)
              eocd  = BS.concat [ le32 0x06054b50, le16 0, le16 0
                                , le16 (length items), le16 (length items)
                                , le32 (sum (map BS.length cds)), le32 (sum (map BS.length locals))
                                , le16 0 ]
          in BS.concat (locals ++ cds ++ [eocd])
        readDir bs = case Z.findEocd bs 0 of
          Left e   -> Left e
          Right ec -> Z.parseCentral (Z.ecPrefix ec)
                        (BS.take (fromInteger (Z.ecCdSize ec))
                                 (BS.drop (fromInteger (Z.ecCdOff ec)) bs))
        -- Produced by zlib at wbits=-15; expands to the 61-byte string below.
        deflated = BS.pack [203,72,205,201,201,87,72,73,77,203,73,44,73,85,40,207
                           ,47,202,73,209,81,200,32,82,16,0]
        plainTxt = utf8 "hello deflate world, hello deflate world, hello deflate world"
        stored nm dat = (utf8 nm, BS.length dat, dat, 0, 0x800, BS.empty)
        arc = mk [ stored "a.txt" (utf8 "first member")
                 , (utf8 "b.txt", BS.length plainTxt, deflated, 8, 0x800, utf8 "EXTRA-LOCAL")
                 , (utf8 "enc.txt", 4, utf8 "\0\0\0\0", 0, 0x801, BS.empty)
                 , (utf8 "lzma.txt", 4, utf8 "xxxx", 14, 0x800, BS.empty)
                 ]
        -- What the driver does: seek to the local header, read its own name
        -- and extra lengths, then take the data span.
        extract bs e = case Z.localDataOffset (BS.take Z.localHeaderBytes
                                                (BS.drop (fromInteger (Z.zeOffset e)) bs)) of
          Left err   -> Left err
          Right skip -> Z.memberBytes e
                          (BS.take (fromInteger (Z.zePacked e))
                                   (BS.drop (fromInteger (Z.zeOffset e) + skip) bs))

    case readDir arc of
      Left e -> check ("zip extraction: parses the archive (" ++ e ++ ")") False
      Right es -> do
        checkEq "zip records each member's local header offset"
          (map Z.zeOffset es == [] ) False
        check "zip member offsets are ascending"
          (and [ a < b | (a, b) <- zip (map Z.zeOffset es) (drop 1 (map Z.zeOffset es)) ])
        checkEq "zip extracts a stored member"
          (extract arc (es !! 0)) (Right (utf8 "first member"))
        -- The local extra field is 11 bytes and the central one is 0; reading
        -- the local header is the only way to land on the data.
        checkEq "zip extracts a deflated member past a longer local extra field"
          (extract arc (es !! 1)) (Right plainTxt)
        check "zip refuses an encrypted member"
          (case extract arc (es !! 2) of Left m -> "encrypted" `isInfixOf` m; _ -> False)
        check "zip refuses an unsupported compression method"
          (case extract arc (es !! 3) of Left m -> "lzma" `isInfixOf` m; _ -> False)
        checkEq "zip finds a member by name"
          (fmap Z.zeName (Z.findEntry (T.pack "b.txt") es)) (Just (T.pack "b.txt"))
        check "zip reports a missing member" (not (Z.hasEntry (T.pack "nope") es))
    -- A self-extracting stub shifts every member, not just the directory.
    case readDir (BS.replicate 777 0x5a <> arc) of
      Left e -> check ("zip extraction past a stub (" ++ e ++ ")") False
      Right es ->
        checkEq "zip corrects member offsets past a self-extracting stub"
          (extract (BS.replicate 777 0x5a <> arc) (head es)) (Right (utf8 "first member"))
    check "zip local header rejects a foreign signature"
      (isLeft (Z.localDataOffset (BS.replicate 30 0x41)))
    check "zip local header rejects a truncated read"
      (isLeft (Z.localDataOffset (BS.replicate 10 0x50)))

  -- DOCX mapping (plan 0021) ---------------------------------------------------
  -- Asserted on the mapped paragraphs rather than on rendered cells, so a
  -- change to layout or theming does not churn these.
  do
    let doc body = TE.encodeUtf8 (T.pack
          ("<?xml version=\"1.0\"?><w:document xmlns:w=\"x\"><w:body>" ++ body ++ "</w:body></w:document>"))
        pars body = toList (fst (Docx.docxPars (doc body)))
        parText p = T.concat (map Rtf.rrText (Rtf.rpRuns p))
        allTexts body = map parText (pars body)

    checkEq "docx reads a paragraph"
      (allTexts "<w:p><w:r><w:t>hello</w:t></w:r></w:p>") [T.pack "hello"]
    checkEq "docx joins a paragraph's runs"
      (allTexts "<w:p><w:r><w:t>a</w:t></w:r><w:r><w:t>b</w:t></w:r></w:p>") [T.pack "ab"]
    -- Empty paragraphs are dropped and every real one is spaced instead: a
    -- .docx carries its spacing in a style sheet this reader does not resolve,
    -- so honouring only the manual blank paragraphs gives an uneven document.
    checkEq "docx drops empty paragraphs"
      (allTexts "<w:p><w:r><w:t>a</w:t></w:r></w:p><w:p/><w:p><w:r><w:t>b</w:t></w:r></w:p>")
      [T.pack "a", T.pack "b"]
    check "docx spaces ordinary paragraphs"
      (all Rtf.rpSpace (pars "<w:p><w:r><w:t>a</w:t></w:r></w:p>"))

    -- Character formatting comes from a run's own w:rPr, and OOXML's on/off
    -- attribute means "on" when absent — getting that backwards makes every
    -- <w:b/> a no-op.
    let fmtOf body = map Rtf.rrFmt (concatMap Rtf.rpRuns (pars body))
    check "docx reads bold"
      (all Rtf.rfBold (fmtOf "<w:p><w:r><w:rPr><w:b/></w:rPr><w:t>x</w:t></w:r></w:p>"))
    check "docx honours an explicit w:val=0"
      (not (any Rtf.rfBold (fmtOf "<w:p><w:r><w:rPr><w:b w:val=\"0\"/></w:rPr><w:t>x</w:t></w:r></w:p>")))
    check "docx reads italic, underline and strike"
      (case fmtOf "<w:p><w:r><w:rPr><w:i/><w:u w:val=\"single\"/><w:strike/></w:rPr><w:t>x</w:t></w:r></w:p>" of
         (f : _) -> Rtf.rfItalic f && Rtf.rfUnder f && Rtf.rfStrike f
         _       -> False)
    check "docx treats w:u w:val=none as no underline"
      (not (any Rtf.rfUnder (fmtOf "<w:p><w:r><w:rPr><w:u w:val=\"none\"/></w:rPr><w:t>x</w:t></w:r></w:p>")))
    checkEq "docx reads a run colour"
      (map Rtf.rfColor (fmtOf "<w:p><w:r><w:rPr><w:color w:val=\"CC0000\"/></w:rPr><w:t>x</w:t></w:r></w:p>"))
      [Just (ColorRGB 0xCC 0 0)]
    checkEq "docx treats w:color auto as the theme's colour"
      (map Rtf.rfColor (fmtOf "<w:p><w:r><w:rPr><w:color w:val=\"auto\"/></w:rPr><w:t>x</w:t></w:r></w:p>"))
      [Nothing]
    -- Formatting does not leak between runs: each w:r carries its own rPr.
    check "docx formatting does not leak into the next run"
      (case fmtOf "<w:p><w:r><w:rPr><w:b/></w:rPr><w:t>a</w:t></w:r><w:r><w:t>b</w:t></w:r></w:p>" of
         [f1, f2] -> Rtf.rfBold f1 && not (Rtf.rfBold f2)
         _        -> False)

    -- A heading's size lives in its style, not on its runs, which is the only
    -- reason a terminal can tell it is a heading at all.
    check "docx gives a heading style a heading size"
      (case fmtOf "<w:p><w:pPr><w:pStyle w:val=\"Heading1\"/></w:pPr><w:r><w:t>T</w:t></w:r></w:p>" of
         (f : _) -> Rtf.rfSize f >= 28
         _       -> False)
    -- A run that sets its own size keeps it.
    checkEq "docx honours an explicit w:sz"
      (map Rtf.rfSize (fmtOf "<w:p><w:r><w:rPr><w:sz w:val=\"20\"/></w:rPr><w:t>x</w:t></w:r></w:p>")) [20]

    -- Twips are OOXML's unit and RTF's alike, which is why the layout's
    -- twipsToCols transfers unchanged.
    checkEq "docx reads alignment and indentation"
      (map (\p -> (Rtf.rpAlign p, Rtf.rpLeft p, Rtf.rpFirst p))
           (pars "<w:p><w:pPr><w:jc w:val=\"center\"/><w:ind w:left=\"720\" w:firstLine=\"360\"/></w:pPr><w:r><w:t>x</w:t></w:r></w:p>"))
      [(Rtf.AlignCenter, 720, 360)]
    checkEq "docx reads a hanging indent as a negative first line"
      (map Rtf.rpFirst
           (pars "<w:p><w:pPr><w:ind w:left=\"720\" w:hanging=\"360\"/></w:pPr><w:r><w:t>x</w:t></w:r></w:p>"))
      [-360]
    -- A table's own w:jc must not centre the document: the element stack is
    -- what tells the two apart.
    checkEq "docx ignores a table's alignment"
      (map Rtf.rpAlign
           (pars "<w:tbl><w:tblPr><w:jc w:val=\"center\"/></w:tblPr><w:tr><w:tc><w:p><w:r><w:t>x</w:t></w:r></w:p></w:tc></w:tr></w:tbl>"))
      [Rtf.AlignLeft]

    -- A list item gets a bullet because the numbering definitions are not
    -- resolved; no marks at all would be worse.
    check "docx marks a list item"
      (case allTexts "<w:p><w:pPr><w:numPr><w:ilvl w:val=\"0\"/></w:numPr></w:pPr><w:r><w:t>item</w:t></w:r></w:p>" of
         (t : _) -> T.pack "\x2022 " `T.isPrefixOf` t
         _       -> False)

    -- A table row is one paragraph and its cells are its tab stops; ending a
    -- paragraph per cell would lose the table's shape entirely.
    checkEq "docx lays a table row out on tab stops"
      (allTexts ("<w:tbl><w:tr><w:tc><w:p><w:r><w:t>a</w:t></w:r></w:p></w:tc>"
                 ++ "<w:tc><w:p><w:r><w:t>b</w:t></w:r></w:p></w:tc></w:tr></w:tbl>"))
      [T.pack "a\tb"]

    checkEq "docx reads w:tab and w:br"
      (allTexts "<w:p><w:r><w:t>a</w:t><w:tab/><w:t>b</w:t><w:br/><w:t>c</w:t></w:r></w:p>")
      [T.pack "a\tb\nc"]
    -- Skipped subtrees: none of these is document text.
    checkEq "docx skips field instructions, deletions and drawings"
      (allTexts ("<w:p><w:r><w:t>keep</w:t></w:r>"
                 ++ "<w:r><w:instrText>PAGE</w:instrText></w:r>"
                 ++ "<w:del><w:r><w:delText>gone</w:delText></w:r></w:del>"
                 ++ "<w:r><w:drawing><wp:docPr name=\"pic\"/></w:drawing></w:r></w:p>"))
      [T.pack "keep"]
    checkEq "docx skips hidden text"
      (allTexts "<w:p><w:r><w:t>a</w:t></w:r><w:r><w:rPr><w:vanish/></w:rPr><w:t>SECRET</w:t></w:r></w:p>")
      [T.pack "a"]
    -- mc:Fallback duplicates the mc:Choice beside it; reading both prints a
    -- text box twice.
    checkEq "docx skips an alternate-content fallback"
      (allTexts ("<w:p><mc:AlternateContent><mc:Choice><w:r><w:t>once</w:t></w:r></mc:Choice>"
                 ++ "<mc:Fallback><w:r><w:t>once</w:t></w:r></mc:Fallback></mc:AlternateContent></w:p>"))
      [T.pack "once"]
    -- A page break belongs between two paragraphs, so the rule is drawn after
    -- the one before it.
    check "docx puts a page break's rule before the following text"
      (case pars "<w:p><w:r><w:t>a</w:t></w:r></w:p><w:p><w:r><w:br w:type=\"page\"/></w:r><w:r><w:t>b</w:t></w:r></w:p>" of
         [p1, p2] -> Rtf.rpBreak p1 && not (Rtf.rpBreak p2) && parText p2 == T.pack "b"
         _        -> False)
    checkEq "docx resolves entities in body text"
      (allTexts "<w:p><w:r><w:t>a &amp; b &lt; c</w:t></w:r></w:p>") [T.pack "a & b < c"]

    -- Detection is by member name, so a renamed container still reads and a
    -- template with no body still falls through to the listing.
    checkEq "docx is detected by its body member"
      (Docx.docxBodyMember (map T.pack ["[Content_Types].xml", "word/document.xml"]))
      (Just (T.pack "word/document.xml"))
    checkEq "docx detection declines an archive with no body"
      (Docx.docxBodyMember (map T.pack ["a.txt", "b/c.txt"])) Nothing
    -- Damaged input yields no paragraphs, which the driver turns into the
    -- archive listing plus a note rather than an error.
    checkEq "docx yields nothing from garbage"
      (Seq.length (fst (Docx.docxPars (TE.encodeUtf8 (T.pack "<<<not xml &&&"))))) 0

  -- XLSX mapping (plan 0021) ---------------------------------------------------
  do
    let sheet body = TE.encodeUtf8 (T.pack ("<worksheet><sheetData>" ++ body ++ "</sheetData></worksheet>"))
        strs = Seq.fromList (map T.pack ["Region", "Sales", "North"])
        grid body = let (g, _, _) = Xlsx.sheetGrid strs (sheet body)
                    in fmap toList (toList g)

    checkEq "xlsx reads an A1 reference" (Xlsx.cellRef (T.pack "A1")) (Just (0, 0))
    checkEq "xlsx reads a two-letter column" (Xlsx.cellRef (T.pack "AB12")) (Just (27, 11))
    checkEq "xlsx rejects a reference that is not one" (Xlsx.cellRef (T.pack "hello")) Nothing

    checkEq "xlsx reads inline numbers"
      (grid "<row r=\"1\"><c r=\"A1\"><v>3.5</v></c></row>") [[T.pack "3.5"]]
    checkEq "xlsx resolves a shared string"
      (grid "<row r=\"1\"><c r=\"A1\" t=\"s\"><v>2</v></c></row>") [[T.pack "North"]]
    checkEq "xlsx reads an inline string"
      (grid "<row r=\"1\"><c r=\"A1\" t=\"inlineStr\"><is><t>hi</t></is></c></row>") [[T.pack "hi"]]
    checkEq "xlsx reads a boolean"
      (grid "<row r=\"1\"><c r=\"A1\" t=\"b\"><v>1</v></c><c r=\"B1\" t=\"b\"><v>0</v></c></row>")
      [[T.pack "TRUE", T.pack "FALSE"]]
    -- A formula shows its cached value; this is not a spreadsheet engine and
    -- must never print the formula source in a cell.
    checkEq "xlsx shows a formula's cached value, not its source"
      (grid "<row r=\"1\"><c r=\"A1\"><f>SUM(B1:B9)</f><v>42</v></c></row>") [[T.pack "42"]]

    -- A spreadsheet's shape is part of its meaning, so gaps must materialise.
    checkEq "xlsx materialises a column gap"
      (grid "<row r=\"1\"><c r=\"A1\"><v>1</v></c><c r=\"C1\"><v>3</v></c></row>")
      [[T.pack "1", T.empty, T.pack "3"]]
    checkEq "xlsx materialises a missing row"
      (grid "<row r=\"1\"><c r=\"A1\"><v>1</v></c></row><row r=\"3\"><c r=\"A3\"><v>3</v></c></row>")
      [[T.pack "1"], [T.empty], [T.pack "3"]]
    checkEq "xlsx pads every row to the widest"
      (map length (grid "<row r=\"1\"><c r=\"A1\"><v>1</v></c><c r=\"B1\"><v>2</v></c></row><row r=\"2\"><c r=\"A2\"><v>3</v></c></row>"))
      [2, 2]
    -- A single bad reference must not materialise a million blank cells: past
    -- the format's own column limit the cell is dropped, not filled up to.
    checkEq "xlsx ignores a column reference past the format's limit"
      (grid "<row r=\"1\"><c r=\"A1\"><v>1</v></c><c r=\"ZZZ1\"><v>x</v></c></row>")
      [[T.pack "1"]]
    -- A reference that is not a reference at all falls back to the next
    -- column, which is what a producer that omits @r@ entirely relies on.
    checkEq "xlsx places an unreadable reference sequentially"
      (grid "<row r=\"1\"><c r=\"A1\"><v>1</v></c><c><v>2</v></c></row>")
      [[T.pack "1", T.pack "2"]]

    let wbXml = TE.encodeUtf8 (T.pack
          ("<workbook><sheets><sheet name=\"Data\" r:id=\"rId1\"/>"
           ++ "<sheet name=\"Extra\" r:id=\"rId2\"/></sheets></workbook>"))
        relsXml = TE.encodeUtf8 (T.pack
          ("<Relationships><Relationship Id=\"rId1\" Target=\"worksheets/sheet1.xml\"/>"
           ++ "<Relationship Id=\"rId2\" Target=\"/xl/worksheets/other.xml\"/></Relationships>"))
        refs = Xlsx.sheetRefs wbXml
        rels = Xlsx.relTargets relsXml
    -- Tab order, not creation order: a workbook whose tabs have been dragged
    -- around has them in neither the member names' order nor the manifest's.
    checkEq "xlsx reads the sheets in tab order"
      (map fst refs) (map T.pack ["Data", "Extra"])
    checkEq "xlsx resolves a sheet through the relationship table"
      (Xlsx.sheetMemberPath refs rels 0) (T.pack "xl/worksheets/sheet1.xml")
    checkEq "xlsx resolves an absolute relationship target"
      (Xlsx.sheetMemberPath refs rels 1) (T.pack "xl/worksheets/other.xml")
    -- Producers that ship no relationship table are common; the conventional
    -- naming is right for all of them.
    checkEq "xlsx falls back to the conventional sheet name"
      (Xlsx.sheetMemberPath refs [] 1) (T.pack "xl/worksheets/sheet2.xml")

    checkEq "xlsx reads the shared-string table"
      (toList (Xlsx.sharedStrings (TE.encodeUtf8 (T.pack
        "<sst><si><t>a</t></si><si><r><t>b</t></r><r><t>c</t></r></si></sst>"))))
      [T.pack "a", T.pack "bc"]
    checkEq "xlsx workbook detection" (Xlsx.isXlsx [Xlsx.workbookPath]) True
    checkEq "xlsx declines an archive with no workbook"
      (Xlsx.isXlsx (map T.pack ["a.txt"])) False

    -- The open workbook: the showing sheet is the live table view, and
    -- switching stores it back so returning finds it where it was left.
    let wb0 = Xlsx.mkWorkbook [ (T.pack "One", Seq.fromList [Seq.fromList [T.pack "a"]])
                              , (T.pack "Two", Seq.fromList [Seq.fromList [T.pack "b"]]) ]
                              (T.pack "note")
    checkEq "workbook counts its sheets" (Xlsx.wbCount wb0) 2
    checkEq "workbook names the showing sheet" (Xlsx.wbName wb0) (T.pack "One")
    case Xlsx.wbView wb0 of
      Nothing -> check "workbook has a view for its first sheet" False
      Just v0 -> do
        let moved = setCursor 0 0 v0
            (wb1, v1) = Xlsx.wbNext moved wb0
            (wb2, v2) = Xlsx.wbPrev v1 wb1
        checkEq "workbook turns to the next sheet" (Xlsx.wbIdx wb1) 1
        checkEq "workbook shows the next sheet's grid" (cellAt 0 0 v1) (T.pack "b")
        checkEq "workbook turns back" (Xlsx.wbIdx wb2) 0
        checkEq "workbook restores the sheet it left" (cellAt 0 0 v2) (T.pack "a")
        checkEq "workbook clamps past the last sheet"
          (Xlsx.wbIdx (fst (Xlsx.wbGoTo v0 99 wb0))) 1
        checkEq "workbook clamps before the first"
          (Xlsx.wbIdx (fst (Xlsx.wbGoTo v0 (-5) wb0))) 0
        check "workbook status names the sheet"
          ("Sheet 1/2" `isInfixOf` Xlsx.wbStatus wb0)
        check "and names it when the name is one cell per character"
          ("One" `isInfixOf` Xlsx.wbStatus wb0)
        -- The status bar's right block is measured in characters and its click
        -- zones with it, so a name in a wide script would shift every zone
        -- after it. The number survives; the name does not.
        let wbWide = Xlsx.mkWorkbook
              [ (T.pack "\x58f2\x4e0a", Seq.fromList [Seq.fromList [T.pack "x"]]) ] T.empty
        check "a wide sheet name is kept out of the clickable status block"
          ("Sheet 1/1" `isInfixOf` Xlsx.wbStatus wbWide
             && not ("\x58f2" `isInfixOf` Xlsx.wbStatus wbWide))
        checkEq "so the status block stays one cell per character"
          (lineDisplayWidth 8 (T.pack (Xlsx.wbStatus wbWide)))
          (length (Xlsx.wbStatus wbWide))

  -- EPUB mapping (plan 0021) ---------------------------------------------------
  do
    let bytes = TE.encodeUtf8 . T.pack
        html body = bytes ("<html><head><title>T</title></head><body>" ++ body ++ "</body></html>")
        pars body = toList (Epub.htmlPars (html body))
        parText p = T.concat (map Rtf.rrText (Rtf.rpRuns p))
        texts body = map parText (pars body)

    -- Container plumbing.
    checkEq "epub finds the package document"
      (Epub.epubOpfPath (bytes "<container><rootfiles><rootfile full-path=\"OEBPS/content.opf\"/></rootfiles></container>"))
      (Just (T.pack "OEBPS/content.opf"))
    checkEq "epub declines a container with no root file"
      (Epub.epubOpfPath (bytes "<container/>")) Nothing
    checkEq "epub is detected by its container member"
      (Epub.isEpub [Epub.containerPath]) True

    let opf = bytes (concat
          [ "<package><metadata><dc:title>A Book</dc:title></metadata><manifest>"
          , "<item id=\"c1\" href=\"ch1.xhtml\" media-type=\"application/xhtml+xml\"/>"
          , "<item id=\"c2\" href=\"sub/ch%202.xhtml\" media-type=\"application/xhtml+xml\"/>"
          , "<item id=\"css\" href=\"s.css\" media-type=\"text/css\"/>"
          , "</manifest><spine><itemref idref=\"c2\"/><itemref idref=\"c1\"/>"
          , "<itemref idref=\"css\"/><itemref idref=\"gone\"/></spine></package>" ])
    -- Reading order comes from the spine, not the manifest, and only documents
    -- are chapters.
    checkEq "epub reads the spine in reading order"
      (Epub.epubSpine (T.pack "OEBPS") opf)
      (map T.pack ["OEBPS/sub/ch 2.xhtml", "OEBPS/ch1.xhtml"])
    checkEq "epub reads the book's title" (Epub.epubTitle opf) (T.pack "A Book")

    -- Href resolution is one function because getting any part of it wrong
    -- loses a chapter silently.
    checkEq "epub resolves an href against the package directory"
      (Epub.resolveHref (T.pack "OEBPS") (T.pack "text/ch1.xhtml")) (T.pack "OEBPS/text/ch1.xhtml")
    checkEq "epub resolves an href at the archive root"
      (Epub.resolveHref T.empty (T.pack "ch1.xhtml")) (T.pack "ch1.xhtml")
    checkEq "epub percent-decodes an href"
      (Epub.resolveHref (T.pack "a") (T.pack "b%20c.xhtml")) (T.pack "a/b c.xhtml")
    checkEq "epub drops a fragment"
      (Epub.resolveHref (T.pack "a") (T.pack "b.xhtml#part2")) (T.pack "a/b.xhtml")
    checkEq "epub folds away dot segments"
      (Epub.resolveHref (T.pack "a/b") (T.pack "../c.xhtml")) (T.pack "a/c.xhtml")
    checkEq "epub treats a leading slash as the archive root"
      (Epub.resolveHref (T.pack "a") (T.pack "/x/y.xhtml")) (T.pack "x/y.xhtml")

    -- Chapters.
    checkEq "epub reads a chapter's title" (Epub.htmlTitle (html "<p>x</p>")) (T.pack "T")
    checkEq "epub reads paragraphs" (texts "<p>one</p><p>two</p>") [T.pack "one", T.pack "two"]
    -- Whitespace collapses as HTML says, so a pretty-printed chapter does not
    -- come out with a ragged left margin.
    checkEq "epub collapses whitespace"
      (texts "<p>\n   one    two\n</p>") [T.pack "one two "]
    checkEq "epub keeps the space between inline elements"
      (texts "<p><em>a</em> <em>b</em></p>") [T.pack "a b"]
    checkEq "epub keeps whitespace inside pre"
      (texts "<pre>  a   b</pre>") [T.pack "  a   b"]
    -- Inline formatting nests, which a single "current format" cannot express.
    check "epub nests inline formatting"
      (case concatMap Rtf.rpRuns (pars "<p><b>a<i>b</i>c</b></p>") of
         [r1, r2, r3] -> Rtf.rfBold (Rtf.rrFmt r1)
                       && Rtf.rfBold (Rtf.rrFmt r2) && Rtf.rfItalic (Rtf.rrFmt r2)
                       && Rtf.rfBold (Rtf.rrFmt r3) && not (Rtf.rfItalic (Rtf.rrFmt r3))
         _ -> False)
    check "epub gives a heading a heading size"
      (case concatMap Rtf.rpRuns (pars "<h1>Title</h1>") of
         (r : _) -> Rtf.rfSize (Rtf.rrFmt r) >= 28
         _       -> False)
    checkEq "epub numbers an ordered list and bullets an unordered one"
      (texts "<ul><li>a</li></ul><ol><li>x</li><li>y</li></ol>")
      [T.pack "\x2022 a", T.pack "1. x", T.pack "2. y"]
    check "epub sets list items tight and paragraphs spaced"
      (case pars "<p>p</p><ul><li>a</li><li>b</li></ul>" of
         [p0, l1, l2] -> Rtf.rpSpace p0 && not (Rtf.rpSpace l1) && not (Rtf.rpSpace l2)
         _            -> False)
    -- A blockquote's indent outlives the paragraphs inside it, so it cannot
    -- live on the paragraph being built.
    check "epub indents a blockquote's paragraphs"
      (case pars "<blockquote><p>quoted</p></blockquote>" of
         (p : _) -> Rtf.rpLeft p > 0
         _       -> False)
    checkEq "epub lays a table row out on tab stops"
      (texts "<table><tr><td>a</td><td>b</td></tr></table>") [T.pack "a\tb"]
    checkEq "epub skips scripts and styles"
      (texts "<p>keep</p><script>var x=1</script><style>p{}</style>") [T.pack "keep"]
    checkEq "epub shows an image's alternative text"
      (texts "<p><img src=\"x.png\" alt=\"a cat\"/></p>") [T.pack "[a cat]"]
    checkEq "epub reads a line break"
      (texts "<p>a<br/>b</p>") [T.pack "a\nb"]
    -- A wrapper element that holds nothing is not a blank line in the book.
    checkEq "epub drops empty blocks"
      (length (pars "<div><div></div><p>only</p><div/></div>")) 1
    -- Non-validating by design: stray markup degrades to the text it looks
    -- like (what a browser does), and a chapter with no text yields nothing —
    -- which is what makes the driver fall back to the archive listing.
    checkEq "epub degrades broken markup to its text"
      (texts "<p>a <b>b</p> c</b>") [T.pack "a b", T.pack "c"]
    checkEq "epub yields nothing from a chapter with no text"
      (Seq.length (Epub.htmlPars (bytes "<html><body><div/><span/></body></html>"))) 0

  -- Container reading views in the editor (plan 0021) ---------------------------
  -- The views are the RTF and CSV ones with a different origin, so what is
  -- asserted here is the difference: no buffer, nothing writable, and the
  -- units each is divided into.
  do
    let mkPar t = Rtf.defaultPar { Rtf.rpRuns = [Rtf.RtfRun (T.pack t) Rtf.defaultFmt] }
        bookPars = Seq.fromList (map mkPar
          ["Chapter one text", "more", "Chapter two text", "and more", "the end"])
        sects = Seq.fromList [(0, T.pack "One"), (2, T.pack "Two")]
        rdBook = Rtf.mkRtfDocFrom (RtfFromContainer (T.pack "EPUB")) sects T.empty bookPars
        rdDoc  = Rtf.mkRtfDocFrom (RtfFromContainer (T.pack "DOCX")) Seq.empty T.empty bookPars
        edBase = newEditor (24, 80) defaultConfig
        edBook = containerDocLoaded "/tmp/x.epub" rdBook edBase
        edDocx = containerDocLoaded "/tmp/x.docx" rdDoc edBase
        wbTest = Xlsx.mkWorkbook [ (T.pack "S1", Seq.fromList [Seq.fromList [T.pack "a"]])
                                 , (T.pack "S2", Seq.fromList [Seq.fromList [T.pack "b"]]) ] T.empty
        edWb   = workbookLoaded "/tmp/x.xlsx" wbTest edBase

    -- A container view is derived from a binary file: read-only, no buffer,
    -- no cursor to draw.
    check "container doc is read-only" (edReadOnly edBook)
    check "container doc has an empty buffer" (isEmptyBuffer (edBuffer edBook))
    check "container doc is not a plain document"
      (not (isPlainDoc (captureDoc edBook)))
    check "workbook is not a plain document" (not (isPlainDoc (captureDoc edWb)))
    check "container view is recognised" (containerViewActive edBook && containerViewActive edWb)
    -- A plain .rtf is *not* a container view: its buffer is live and Save
    -- writes it.
    check "an rtf file is not a container view"
      (not (containerViewActive (enterRtf (setLoaded "/tmp/x.rtf"
              (emptyLoadResult { lrBuffer = fromText (T.pack "{\\rtf1 hi}") }) edBase))))
    -- Nothing can move a binary file under its view, so it never re-parses.
    check "a container view never goes stale" (not (Rtf.rtfStale 999 rdBook))

    -- Save is refused rather than writing an empty buffer over the container.
    forM_ [("doc", edBook), ("workbook", edWb)] $ \(what, e) ->
      forM_ [MASave, MASaveAll, MARevert, MAPaste, MACut] $ \a ->
        check (what ++ " refuses " ++ show a)
          (let (e', effs) = runAction a e in null effs && edStatus e' /= T.empty)
    -- A workbook's grid must never be serialised into the buffer and saved as
    -- CSV: it is one sheet of someone's workbook, not their workbook.
    check "syncCsvToBuffer leaves a workbook alone"
      (isEmptyBuffer (edBuffer (syncCsvToBuffer edWb)))

    -- Each view is divided into something, and Go To means that thing.
    check "an e-book's Go To is titled for chapters"
      (case edDialog (openGoTo edBook) of
         Just d  -> T.pack "Chapter" `T.isInfixOf` dlgTitle d
         Nothing -> False)
    check "a workbook's Go To is titled for sheets"
      (case edDialog (openGoTo edWb) of
         Just d  -> T.pack "Sheet" `T.isInfixOf` dlgTitle d
         Nothing -> False)
    -- A .docx has no chapters, so Go To is honestly absent there.
    check "a docx has no Go To"
      (let (e', _) = runAction MAGoToLine edDocx in edDialog e' == Nothing)

    checkEq "go to sheet switches the showing grid"
      (fmap (cellAt 0 0) (edCsv (goToSheetIn 1 edWb))) (Just (T.pack "b"))
    checkEq "go to sheet clamps" (fmap Xlsx.wbIdx (edSheets (goToSheetIn 9 edWb))) (Just 1)
    checkEq "go to chapter moves the viewport"
      (fmap rdTop (edRtf (gotoLine (T.pack "2") edBook)))
      (fmap (\rd -> rdTop (Rtf.rtfGoToSection (rtfHeight edBook) 2 rd)) (edRtf edBook))

    -- Alt+T offers the archive from the rendered view and the document back
    -- from the listing.
    check "Alt+T from a container view asks for the archive"
      (case runAction MAArchiveView edBook of
         (_, [EffContainerView p True]) -> p == "/tmp/x.epub"
         _                              -> False)
    let edListing = setLoaded "/tmp/x.epub"
                      (emptyLoadResult { lrBuffer = fromText (T.pack "listing")
                                       , lrReadOnly = True }) edBase
    check "the archive listing offers the document back"
      (containerListing edListing)
    check "Alt+T from the listing asks for the document"
      (case runAction MAArchiveView edListing of
         (_, [EffContainerView _ False]) -> True
         _                               -> False)

    -- Menus: what is pruned, and what deliberately is not.
    let viewItems e = [ a | MEItem _ _ a <- entriesFor e 3 ]
        findItems e = [ a | MEItem _ _ a <- entriesFor e 2 ]
    check "the container views offer the archive entry"
      (MAArchiveView `elem` viewItems edBook && MAArchiveView `elem` viewItems edWb)
    check "a plain document does not" (MAArchiveView `notElem` viewItems edBase)
    check "a workbook drops the sort and table-toggle entries"
      (MASortColumn `notElem` viewItems edWb && MAToggleCsv `notElem` viewItems edWb)
    check "a workbook keeps the freeze-header entry"
      (MAToggleFreezeHeader `elem` viewItems edWb)
    check "an e-book keeps Go To in the Find menu"
      (MAGoToLine `elem` findItems edBook)
    check "a docx does not" (MAGoToLine `notElem` findItems edDocx)

    -- Section navigation, which is what makes [ and ] mean anything.
    checkEq "sections are counted" (Rtf.rtfSectionCount rdBook) 2
    -- A one-line viewport, so the clamp cannot hide the movement (a document
    -- shorter than the window never scrolls, which is correct and untestable).
    let laid = Rtf.rtfRelayout 4 40 1 rdBook
    checkEq "the first section is section one" (Rtf.rtfSectionAt laid) 1
    checkEq "turning forward reaches the second"
      (Rtf.rtfSectionAt (Rtf.rtfNextSection 1 laid)) 2
    checkEq "turning forward again stays at the last"
      (Rtf.rtfSectionAt (Rtf.rtfNextSection 1 (Rtf.rtfNextSection 1 laid))) 2
    checkEq "turning back returns to the first"
      (rdTop (Rtf.rtfPrevSection 1 (Rtf.rtfNextSection 1 laid)))
      (rdTop (Rtf.rtfGoToSection 1 1 laid))
    -- Back from *within* a section goes to that section's start first, which
    -- is what every reader's page-back key does.
    checkEq "turning back from mid-section returns to its start"
      (rdTop (Rtf.rtfPrevSection 1 (Rtf.rtfScroll 1 1 (Rtf.rtfGoToSection 1 2 laid))))
      (rdTop (Rtf.rtfGoToSection 1 2 laid))
    checkEq "the section title follows the viewport"
      (Rtf.rtfSectionTitle (Rtf.rtfNextSection 1 laid)) (T.pack "Two")
    checkEq "a document with no sections reports none" (Rtf.rtfSectionCount rdDoc) 0
    check "the status bar names the container format"
      ("EPUB" `isInfixOf` Rtf.rtfStatus laid)
    -- Same rule for a chapter title as for a sheet name.
    let wideSect = Seq.fromList [(0, T.pack "\x7ae0")]
        laidWide = Rtf.rtfRelayout 4 40 1
          (Rtf.mkRtfDocFrom (RtfFromContainer (T.pack "EPUB")) wideSect T.empty bookPars)
    checkEq "a wide chapter title keeps the status block one cell per character"
      (lineDisplayWidth 8 (T.pack (Rtf.rtfStatus laidWide)))
      (length (Rtf.rtfStatus laidWide))
    check "and the chapter number still shows"
      ("Ch 1/1" `isInfixOf` Rtf.rtfStatus laidWide)

  -- Selection in the formatted view (RTF / DOCX / EPUB) -----------------------
  -- The text view's caret-and-anchor model over laid-out (line, character)
  -- coordinates, minus everything that writes. Ported from the PDF view, so
  -- these mirror its assertions.
  do
    let mkPar t = Rtf.defaultPar { Rtf.rpRuns = [Rtf.RtfRun (T.pack t) Rtf.defaultFmt] }
        doc0 = Rtf.mkRtfDocFrom (RtfFromContainer (T.pack "DOCX")) Seq.empty T.empty
                 (Seq.fromList (map mkPar ["alpha beta", "gamma delta", "epsilon"]))
        -- Laid out wide enough that each paragraph is one line.
        doc = Rtf.rtfRelayout 4 40 3 doc0
        at l c = Pos l c

    checkEq "formatted view lays each paragraph on one line" (Rtf.rtfLineCount doc) 3
    checkEq "formatted view reads a laid-out line"
      (Rtf.rtfLineTextAt doc 1) (T.pack "gamma delta")

    -- An untouched document has no selection and so no caret to draw.
    checkEq "a fresh formatted view has no selection" (Rtf.rtfSelection doc) Nothing
    checkEq "a fresh formatted view has no anchor" (rdAnchor doc) Nothing
    checkEq "a fresh formatted view copies nothing" (Rtf.rtfSelText doc) T.empty

    -- Within one line, and across several.
    let sel1 = Rtf.rtfSelectRange (at 0 6) (at 0 10) doc
    checkEq "selecting within a line" (Rtf.rtfSelText sel1) (T.pack "beta")
    let sel2 = Rtf.rtfSelectRange (at 0 6) (at 2 3) doc
    checkEq "selecting across lines joins them with newlines"
      (Rtf.rtfSelText sel2) (T.pack "beta\ngamma delta\neps")
    -- Backwards drags are the same selection.
    checkEq "a backwards selection is the same text"
      (Rtf.rtfSelText (Rtf.rtfSelectRange (at 2 3) (at 0 6) doc)) (T.pack "beta\ngamma delta\neps")
    checkEq "an empty range is no selection"
      (Rtf.rtfSelection (Rtf.rtfSelectRange (at 1 2) (at 1 2) doc)) Nothing

    checkEq "select all takes the whole document"
      (Rtf.rtfSelText (Rtf.rtfSelectAll doc)) (T.pack "alpha beta\ngamma delta\nepsilon")
    checkEq "clearing drops the selection"
      (Rtf.rtfSelection (Rtf.rtfClearSel sel1)) Nothing

    -- Positions are clamped onto the laid-out document, always.
    checkEq "a position past the end clamps" (Rtf.rtfClampPos doc (at 99 99)) (at 2 7)
    checkEq "a negative position clamps" (Rtf.rtfClampPos doc (at (-3) (-3))) (at 0 0)

    -- Double- and triple-click.
    checkEq "double-click takes the word under it"
      (Rtf.rtfSelText (uncurry Rtf.rtfSelectRange (Rtf.rtfWordRange (at 1 8) doc) doc))
      (T.pack "delta")
    checkEq "double-click off a word selects nothing"
      (Rtf.rtfWordRange (at 0 5) doc) (at 0 5, at 0 5)
    checkEq "triple-click takes the line"
      (Rtf.rtfSelText (uncurry Rtf.rtfSelectRange (Rtf.rtfLineRange (at 1 4) doc) doc))
      (T.pack "gamma delta")

    -- Shift+movement: the anchor is planted where the caret was.
    let ext f d = Rtf.rtfExtendTo f d
        moved = ext Rtf.rtfCaretRight (ext Rtf.rtfCaretRight doc)
    checkEq "shift+movement starts a selection from the caret"
      (Rtf.rtfSelText moved) (T.pack "al")
    checkEq "extending again grows the same selection"
      (Rtf.rtfSelText (ext Rtf.rtfCaretRight moved)) (T.pack "alp")
    checkEq "shift+end takes to the end of the line"
      (Rtf.rtfSelText (ext Rtf.rtfCaretEnd doc)) (T.pack "alpha beta")
    checkEq "shift+ctrl+end takes to the end of the document"
      (Rtf.rtfSelText (ext Rtf.rtfCaretBottom doc))
      (T.pack "alpha beta\ngamma delta\nepsilon")
    -- Caret movement wraps between lines, as in the text view.
    checkEq "the caret moves down a line"
      (rdCaret (Rtf.rtfCaretDown doc)) (at 1 0)
    checkEq "the caret at a line's end steps to the next"
      (rdCaret (Rtf.rtfCaretRight (Rtf.rtfCaretEnd doc))) (at 1 0)
    checkEq "the caret at a line's start steps back to the previous"
      (rdCaret (Rtf.rtfCaretLeft (Rtf.rtfSelectRange (at 1 0) (at 1 0) doc))) (at 0 10)

    -- Mouse mapping. A line's leading pad is indent and alignment, not text,
    -- so a click inside it lands at the start of the line rather than partway
    -- into it.
    let indented = Rtf.rtfRelayout 4 40 3
          (Rtf.mkRtfDocFrom RtfFromBuffer Seq.empty T.empty
             (Seq.fromList [ (mkPar "indented text") { Rtf.rpLeft = 720 } ]))
    check "the laid-out line carries its pad"
      (maybe False ((> 0) . Rtf.rlPad) (Seq.lookup 0 (Rtf.rtfLines indented)))
    checkEq "a click in a line's indent lands at its first character"
      (Rtf.rtfPosAtCell 4 0 1 indented) (at 0 0)
    checkEq "a click past the pad lands on the character under it"
      (Rtf.rtfPosAtCell 4 0 (6 + 3) indented) (at 0 3)
    checkEq "the cell of a position undoes the mapping"
      (Rtf.rtfCellOfPos 4 (at 0 3) indented) (6 + 3)

    -- A re-wrap replaces every laid-out line, so the selection has to go
    -- rather than point at whatever now sits at those indices.
    checkEq "a re-wrap drops the selection"
      (Rtf.rtfSelection (Rtf.rtfRelayout 4 20 3 (Rtf.rtfSelectAll doc))) Nothing
    -- Re-laying out to the *same* width is a no-op and must keep it.
    checkEq "re-laying out to the same width keeps the selection"
      (Rtf.rtfSelText (Rtf.rtfRelayout 4 40 3 (Rtf.rtfSelectAll doc)))
      (T.pack "alpha beta\ngamma delta\nepsilon")

    -- Scrolling to the caret moves as little as it can.
    let tall = Rtf.rtfRelayout 4 40 1 doc
    checkEq "scrolling to a caret below the window brings it on"
      (rdTop (Rtf.rtfScrollToCaret 1 (Rtf.rtfSelectRange (at 2 0) (at 2 3) tall))) 2
    checkEq "scrolling to a caret already on the window does nothing"
      (rdTop (Rtf.rtfScrollToCaret 1 (Rtf.rtfSelectRange (at 0 0) (at 0 3) tall))) 0

    -- Through the editor. The view has to be laid out for the editor's own
    -- width first: a re-wrap drops the selection, so selecting against a
    -- differently-laid-out copy would assert on something the first repaint
    -- throws away.
    let edLaid = refreshRtf ((newEditor (24, 80) defaultConfig) { edRtf = Just doc0 })
        laid   = maybe doc0 id (edRtf edLaid)
        edSel  = edLaid { edRtf = Just (Rtf.rtfSelectRange (at 0 6) (at 0 10) laid) }
        (edC, effs) = runAction MACopy edSel
    checkEq "Ctrl+C copies the formatted view's selection"
      [ t | EffCopy t <- effs ] [T.pack "beta"]
    checkEq "the copied text is on the editor's clipboard" (edClipboard edC) (T.pack "beta")
    let edNone = edLaid
        (edN, effsN) = runAction MACopy edNone
    check "copying nothing says so rather than copying"
      (null [ () | EffCopy _ <- effsN ] && T.pack "Nothing selected" `T.isInfixOf` edStatus edN)
    checkEq "Select All works in the formatted view"
      (fmap Rtf.rtfSelText (edRtf (fst (runAction MASelectAll edNone))))
      (Just (T.pack "alpha beta\ngamma delta\nepsilon"))
    -- Copy and Select All are the two actions the view deliberately keeps, so
    -- they survive into the Edit menu while Cut, Paste and Delete do not.
    let editItems e = [ act | MEItem _ _ act <- entriesFor e 1 ]
    check "the formatted view offers Copy and Select All"
      (MACopy `elem` editItems edNone && MASelectAll `elem` editItems edNone)
    check "the formatted view still offers nothing that writes"
      (all (`notElem` editItems edNone) [MACut, MAPaste])
    -- The renderer paints it. Asserted on the cell grid rather than on a
    -- terminal round trip, since the selection is a *style* and the diff would
    -- not tell us which cells carried it.
    let scrSel = renderEditor edSel
        cellAtRC scr r c = scrCells scr A.! (r * scrW scr + c)
        -- "alpha beta" is laid out on the first text row; columns 6..9 are the
        -- selected "beta".
        row0 = 1
        selStyle = cellStyle (cellAtRC scrSel row0 6)
        unselStyle = cellStyle (cellAtRC scrSel row0 2)
    checkEq "the selected cell holds the selected character"
      (cellChar (cellAtRC scrSel row0 6)) 'b'
    check "the selection is painted" (selStyle /= unselStyle)
    check "the character before it is not" (unselStyle == cellStyle (cellAtRC scrSel row0 0))
    check "the character after it is not"
      (cellStyle (cellAtRC scrSel row0 10) == unselStyle)
    -- The caret is shown only while a selection is being made; an untouched
    -- document still has no cursor at all.
    check "the formatted view shows a caret while selecting"
      (scrCursor scrSel /= Nothing)
    check "an untouched formatted view shows no cursor"
      (scrCursor (renderEditor edLaid) == Nothing)

    -- Esc clears; plain arrows scroll and leave the selection alone, because
    -- scrolling to see the rest of what you highlighted must not destroy it.
    let edWithSel = edSel
    checkEq "Esc clears the formatted view's selection"
      (fmap Rtf.rtfSelection (edRtf (fst (update KEsc edWithSel)))) (Just Nothing)
    check "a plain arrow scrolls without disturbing the selection"
      (case edRtf (fst (update (KArrow DDown noMods) edWithSel)) of
         Just rd -> Rtf.rtfSelText rd == T.pack "beta"
         Nothing -> False)

  -- Formula evaluation (Cmedit.Formula) ---------------------------------------
  -- The whole point of this module is the case a workbook does *not* answer
  -- for itself, so the first thing pinned is that it never touches the case a
  -- workbook does.
  do
    let row vs = Seq.fromList (map T.pack vs ++ replicate (10 - length vs) T.empty)
        grid = Seq.fromList
          [ row ["10", "row1"], row ["20", "row2"], row ["30", "row3"]
          , row ["40", "row4"], row ["5",  "row5"], row [] ]
        other = Seq.fromList [ row ["99"] ]
        -- Evaluate one formula, placed in a scratch cell (J6) out of the way
        -- of everything the fixtures reference — including the whole-row and
        -- whole-column ranges, which would otherwise take it in and be
        -- correctly reported as circular.
        run src =
          let sheets = [ Fm.SheetIn (T.pack "Calc") grid (M.singleton (5, 9) (T.pack src))
                       , Fm.SheetIn (T.pack "Other") other M.empty ]
              (out, comp, unsup) = Fm.evalWorkbook sheets
              cell = case out of
                ((_, g) : _) -> maybe T.empty id (Seq.lookup 5 g >>= Seq.lookup 9)
                []           -> T.empty
          in (T.unpack cell, comp, unsup)
        calc src = let (t, _, _) = run src in t
        cellOf g r c = maybe T.empty id (Seq.lookup r g >>= Seq.lookup c)
        eq name src want = checkEq (name ++ " (" ++ src ++ ")") (calc src) want

    -- Arithmetic, precedence and coercion.
    eq "sum of a range"      "=SUM(A1:A5)"        "105"
    eq "precedence"          "=(2+3)*4^2-10/4"    "77.5"
    eq "left-associative ^"  "=2^3^2"             "64"
    eq "unary minus"         "=-A1+5"             "-5"
    eq "percent"             "=50%*A2"            "10"
    eq "concatenation"       "=\"a\"&1&TRUE"      "a1TRUE"
    eq "comparison"          "=A1<A2"             "TRUE"
    eq "text compares case-insensitively" "=\"AB\"=\"ab\"" "TRUE"
    eq "a blank is zero"     "=J5+1"              "1"
    eq "text that looks like a number coerces" "=\"3\"+1" "4"
    eq "text that does not is an error" "=\"three\"+1" "#VALUE!"

    -- Aggregates. Text and blanks inside a range are skipped, as in Excel.
    eq "average"    "=AVERAGE(A1:A5)"   "21"
    eq "min and max" "=MIN(A1:A5)&\"/\"&MAX(A1:A5)" "5/40"
    eq "count is numbers only"  "=COUNT(A1:B5)"  "5"
    eq "counta is non-blank"    "=COUNTA(A1:B5)" "10"
    eq "median"     "=MEDIAN(A1:A5)"    "20"
    eq "product"    "=PRODUCT(A1:A3)"   "6000"
    eq "a range skips text"     "=SUM(A1:B5)"    "105"
    eq "sumproduct" "=SUMPRODUCT(A1:A3,A1:A3)"   "1400"

    -- Conditionals.
    eq "if true"    "=IF(A1>5,\"yes\",\"no\")"  "yes"
    eq "if false"   "=IF(A1>50,\"yes\",\"no\")" "no"
    eq "if with no else" "=IF(A1>50,\"yes\")"   "FALSE"
    eq "nested if"  "=IF(SUM(A1:A5)>100,ROUND(AVERAGE(A1:A5),1),0)" "21"
    eq "and/or"     "=AND(A1>5,A2>5)&\"|\"&OR(A1>500,A2>500)" "TRUE|FALSE"
    eq "not"        "=NOT(A1>5)"        "FALSE"
    eq "iferror catches" "=IFERROR(A1/0,\"oops\")" "oops"
    eq "iferror passes through" "=IFERROR(A1+1,\"oops\")" "11"
    -- IF must not evaluate the branch it does not take, or a guard against
    -- division by zero would still divide by zero.
    eq "if short-circuits" "=IF(A1=0,A2/A1,\"safe\")" "safe"

    -- Criteria: comparisons, exact text and wildcards.
    eq "sumif"      "=SUMIF(A1:A5,\">=20\")"  "90"
    eq "countif"    "=COUNTIF(A1:A5,\">15\")" "3"
    eq "countif on text" "=COUNTIF(B1:B5,\"row3\")" "1"
    eq "countif wildcard" "=COUNTIF(B1:B5,\"row*\")" "5"
    eq "sumif with a separate sum range" "=SUMIF(B1:B5,\"row3\",A1:A5)" "30"
    eq "averageif" "=AVERAGEIF(A1:A5,\">=20\")" "30"

    -- Maths and rounding. Excel rounds half away from zero, not to even.
    eq "round"      "=ROUND(10/3,2)"    "3.33"
    eq "round half away from zero" "=ROUND(2.5,0)" "3"
    eq "round negative half away"  "=ROUND(-2.5,0)" "-3"
    eq "roundup"    "=ROUNDUP(3.01,1)"  "3.1"
    eq "rounddown"  "=ROUNDDOWN(3.99,1)" "3.9"
    eq "round to tens" "=ROUND(1234,-2)" "1200"
    eq "abs and mod" "=MOD(17,5)&\"|\"&ABS(-4.5)" "2|4.5"
    eq "int truncates downward" "=INT(-1.5)" "-2"
    eq "power and sqrt" "=POWER(2,10)&\"|\"&SQRT(16)" "1024|4"
    eq "mod by zero"    "=MOD(1,0)"     "#DIV/0!"
    eq "sqrt of a negative" "=SQRT(-1)" "#NUM!"

    -- Text.
    eq "upper/left/len" "=UPPER(LEFT(\"hello world\",5))&\"-\"&LEN(\"abc\")" "HELLO-3"
    eq "right and mid"  "=RIGHT(\"abcdef\",2)&MID(\"abcdef\",2,3)" "efbcd"
    eq "trim collapses" "=TRIM(\"  a   b  \")" "a b"
    eq "concatenate"    "=CONCATENATE(\"a\",1,\"b\")" "a1b"
    eq "substitute"     "=SUBSTITUTE(\"a-b-c\",\"-\",\"+\")" "a+b+c"
    eq "textjoin skips blanks" "=TEXTJOIN(\",\",TRUE,B1:B3)" "row1,row2,row3"

    -- Lookups.
    eq "vlookup exact"  "=VLOOKUP(30,A1:B5,2,FALSE)" "row3"
    eq "vlookup missing" "=VLOOKUP(31,A1:B5,2,FALSE)" "#N/A"
    eq "match"          "=MATCH(30,A1:A5,0)" "3"
    eq "index"          "=INDEX(B1:B5,2)"    "row2"
    eq "is-functions"   "=ISNUMBER(A1)&ISTEXT(B1)&ISBLANK(J5)" "TRUETRUETRUE"

    -- References: cell to cell, whole columns, other sheets.
    eq "a whole-column range" "=SUM(A:A)" "105"
    eq "a whole-row range"    "=SUM(1:1)" "10"
    eq "a cross-sheet reference" "=Other!A1+1" "100"
    eq "an unknown sheet"     "=Nope!A1"  "#REF!"
    eq "dollar signs are ignored" "=$A$1+1" "11"

    -- Errors are answers, and are shown.
    eq "division by zero" "=A1/0" "#DIV/0!"
    eq "an error propagates through arithmetic" "=A1/0+1" "#DIV/0!"
    eq "an error literal in the source" "=NA()+1" ""     -- NA() is not implemented

    -- A formula this reader does not understand leaves the cell exactly as it
    -- was and is counted, rather than showing a guess.
    let (t1, c1, u1) = run "=XIRR(A1:A5,A1:A5)"
    checkEq "an unsupported function leaves the cell blank" t1 ""
    checkEq "an unsupported function is not counted as computed" c1 0
    checkEq "an unsupported function is counted as unsupported" u1 1
    let (t2, c2, u2) = run "=SUM(A1:A5"           -- unbalanced
    checkEq "a formula that does not parse leaves the cell blank" t2 ""
    checkEq "a formula that does not parse is counted" (c2, u2) (0, 1)
    let (_, c3, u3) = run "=SUM(A1:A5)"
    checkEq "a formula that works is counted as computed" (c3, u3) (1, 0)
    check "the supported set covers what generated workbooks use"
      (all (`elem` Fm.supportedFunctions)
           (map T.pack ["SUM", "AVERAGE", "IF", "COUNT", "MIN", "MAX", "ROUND"
                       ,"SUMIF", "COUNTIF", "VLOOKUP", "CONCATENATE", "IFERROR"]))
    -- Volatile and date functions are deliberately absent: this module is pure
    -- and has no clock, and a wrong date is worse than a blank.
    check "no clock-dependent functions are claimed"
      (all (`notElem` Fm.supportedFunctions) (map T.pack ["TODAY", "NOW", "RAND"]))

    -- Chains, cycles and depth. A cycle must be reported, not followed.
    let chain = [ Fm.SheetIn (T.pack "Calc") grid
                    (M.fromList [ ((0, 9), T.pack "=SUM(A1:A5)")
                                , ((1, 9), T.pack "=J1*2") ])
                , Fm.SheetIn (T.pack "Other") other M.empty ]
        (chainOut, _, _) = Fm.evalWorkbook chain
    checkEq "a formula that reads another formula's cell"
      (case chainOut of ((_, g) : _) -> cellOf g 1 9; _ -> T.empty) (T.pack "210")
    let cyc = [ Fm.SheetIn (T.pack "Calc") grid
                  (M.fromList [ ((0, 9), T.pack "=J2"), ((1, 9), T.pack "=J1") ])
              , Fm.SheetIn (T.pack "Other") other M.empty ]
        (cycOut, _, _) = Fm.evalWorkbook cyc
    checkEq "a circular reference is reported, not followed"
      (case cycOut of ((_, g) : _) -> cellOf g 0 9; _ -> T.empty) (T.pack "#CYCLE!")
    -- A long chain is not a cycle, and the two must not be confused: a running
    -- total written upward (each row reading the one below) cannot be resolved
    -- by evaluation order and really does recurse the length of the column.
    let deep n =
          [ Fm.SheetIn (T.pack "Up")
              (Seq.fromList (replicate n (Seq.fromList [T.pack "1", T.empty])))
              (M.fromList [ ((r, 1), T.pack (if r == n - 1 then "A" ++ show (r + 1)
                                             else "B" ++ show (r + 2) ++ "+A" ++ show (r + 1)))
                          | r <- [0 .. n - 1] ]) ]
        (deepOut, deepC, _) = Fm.evalWorkbook (deep 500)
    checkEq "a 500-deep reference chain resolves"
      (case deepOut of ((_, g) : _) -> cellOf g 0 1; _ -> T.empty) (T.pack "500")
    checkEq "every link of it is computed" deepC 500

    checkEq "a cell that refers to itself is circular too"
      (calc "=J6") "#CYCLE!"
    -- ...and so is a range that quietly contains the formula.
    checkEq "a range that includes the formula's own cell is circular"
      (calc "=SUM(J1:J6)") "#CYCLE!"

    -- Number rendering matches how the file writes its own cached values:
    -- plain, no separators, no format applied.
    checkEq "an integer result has no decimal point" (Fm.showNumber 30) (T.pack "30")
    checkEq "a fraction keeps its digits" (Fm.showNumber 3.25) (T.pack "3.25")
    checkEq "float noise is not shown" (Fm.showNumber (0.1 + 0.2)) (T.pack "0.3")
    checkEq "zero" (Fm.showNumber 0) (T.pack "0")
    checkEq "a negative" (Fm.showNumber (-1.5)) (T.pack "-1.5")

    -- The parser, on its own.
    check "a formula parses with or without a leading ="
      (Fm.parseFormula (T.pack "SUM(A1)") == Fm.parseFormula (T.pack "=SUM(A1)"))
    checkEq "an unknown function makes the whole formula unsupported"
      (Fm.parseFormula (T.pack "=1+XIRR(A1)")) Nothing
    checkEq "a bare short name is not a cell reference"
      (Fm.parseFormula (T.pack "=VAT*2")) Nothing
    check "a range parses as an area"
      (case Fm.parseFormula (T.pack "=A1:B2") of Just Fm.EArea{} -> True; _ -> False)

  -- Formulas through the workbook reader (Cmedit.Xlsx) --------------------------
  do
    let sheet body = TE.encodeUtf8 (T.pack ("<worksheet><sheetData>" ++ body ++ "</sheetData></worksheet>"))
        readSheet body = let (g, fs, _) = Xlsx.sheetGrid Seq.empty (sheet body)
                         in (fmap toList (toList g), M.toList fs)

    -- THE invariant: a formula that came with its value is not recorded for
    -- evaluation at all, so nothing this editor computes can ever contradict
    -- the program that wrote the file.
    checkEq "a formula with a cached value is shown and never recomputed"
      (readSheet "<row r=\"1\"><c r=\"A1\"><f>SUM(B1:C1)</f><v>999</v></c></row>")
      ([[T.pack "999"]], [])
    -- ...and one without is recorded, which is the only case that is computed.
    checkEq "a formula with no cached value is recorded for evaluation"
      (readSheet "<row r=\"1\"><c r=\"A1\"><f>SUM(B1:C1)</f></c></row>")
      ([[T.empty]], [((0, 0), T.pack "SUM(B1:C1)")])
    -- A shared formula's followers carry an empty <f>; they cannot be computed
    -- and are recorded so the count says so rather than staying silent.
    checkEq "an empty formula element is still recorded"
      (readSheet "<row r=\"1\"><c r=\"A1\"><f t=\"shared\" si=\"0\"/></c></row>")
      ([[T.empty]], [((0, 0), T.empty)])
    -- The formula source must never leak into the cell.
    checkEq "the formula source is never displayed"
      (fst (readSheet "<row r=\"1\"><c r=\"A1\"><f>SUM(B1:C1)</f><v>7</v></c></row>"))
      [[T.pack "7"]]

    -- End to end through the workbook: cached wins, uncached is computed.
    let both = [ (T.pack "S", Seq.fromList [Seq.fromList (map T.pack ["10", "20", "999", ""])]
                 , M.fromList [ ((0, 2), T.pack "SUM(A1:B1)")     -- has a cached 999 alongside
                              , ((0, 3), T.pack "SUM(A1:B1)") ]) ]
        (outSheets, nComp, nUn) = Xlsx.resolveFormulas both
        firstRow = case outSheets of
          ((_, g) : _) -> maybe [] toList (Seq.lookup 0 g)
          []           -> []
    -- (Both are in the formula map here only because the fixture puts them
    -- there; the reader would not have recorded the first.)
    checkEq "resolveFormulas fills the cells it is given"
      firstRow (map T.pack ["10", "20", "30", "30"])
    checkEq "resolveFormulas counts what it did" (nComp, nUn) (2, 0)
    checkEq "a workbook with no uncached formulas is untouched"
      (Xlsx.resolveFormulas [(T.pack "S", Seq.fromList [Seq.fromList [T.pack "a"]], M.empty)])
      ([(T.pack "S", Seq.fromList [Seq.fromList [T.pack "a"]])], 0, 0)

  -- Save As in a view with no buffer: export, not save --------------------------
  -- These views are read-only because nothing can write their format back.
  -- That is an argument against writing an .xlsx, not against writing
  -- anything, so Save As exports what is on screen — and, crucially, leaves
  -- the open document exactly where it was.
  do
    let ed0 = newEditor (24, 80) defaultConfig
        wb  = Xlsx.mkWorkbook
                [ (T.pack "Data",  Seq.fromList [Seq.fromList (map T.pack ["a", "b,c"])])
                , (T.pack "Q1/Q2", Seq.fromList [Seq.fromList [T.pack "z"]]) ] T.empty
        edWb = workbookLoaded "/tmp/book.xlsx" wb ed0
        mkPar t = Rtf.defaultPar { Rtf.rpRuns = [Rtf.RtfRun (T.pack t) Rtf.defaultFmt]
                                 , Rtf.rpSpace = True }
        rdDoc = Rtf.mkRtfDocFrom (RtfFromContainer (T.pack "DOCX")) Seq.empty T.empty
                  (Seq.fromList (map mkPar ["First para", "Second para"]))
        edDoc = containerDocLoaded "/tmp/report.docx" rdDoc ed0

    -- A workbook exports the *showing* sheet, and says which one in the name.
    checkEq "a workbook exports the showing sheet as CSV"
      (fmap fst (exportSuggestion edWb)) (Just "/tmp/book-Data.csv")
    checkEq "the exported text is the sheet, quoted as CSV"
      (fmap snd (exportSuggestion edWb)) (Just (T.pack "a,\"b,c\""))
    -- Turning to another sheet exports that one instead.
    checkEq "turning the sheet changes what is exported"
      (fmap fst (exportSuggestion (goToSheetIn 1 edWb))) (Just "/tmp/book-Q1_Q2.csv")
    checkEq "a sheet name that is not a filename is sanitised"
      (fmap snd (exportSuggestion (goToSheetIn 1 edWb))) (Just (T.pack "z"))

    -- A container document exports its paragraphs, unwrapped: a file wrapped
    -- to whatever width the terminal happened to be is a poor artifact.
    checkEq "a docx exports as text"
      (fmap fst (exportSuggestion edDoc)) (Just "/tmp/report.txt")
    checkEq "the exported text is the paragraphs, not the laid-out lines"
      (fmap snd (exportSuggestion edDoc))
      (Just (T.pack "First para\n\nSecond para\n\n"))

    -- A plain .rtf has a real buffer, so Save As stays an ordinary save of it.
    let edRtfPlain = enterRtf (setLoaded "/tmp/x.rtf"
          (emptyLoadResult { lrBuffer = fromText (T.pack "{\\rtf1 hi}") }) ed0)
    checkEq "a plain rtf file is saved, not exported" (exportSuggestion edRtfPlain) Nothing
    checkEq "an ordinary document is saved, not exported"
      (exportSuggestion (setLoadedText (T.pack "hello") ed0)) Nothing

    -- Views with no text refuse rather than writing an empty file, which is
    -- what Save As did before this existed.
    let edImg = imageLoaded "/tmp/x.png"
                  [(Image 1 1 "PNG" (listArray (0, 3) [0, 0, 0, 255]), 0)] ed0
    checkEq "an image has nothing to export" (exportSuggestion edImg) Nothing
    check "and says so rather than offering a dialog"
      (case saveAsRefusal edImg of Just m -> "no text" `isInfixOf` m; Nothing -> False)
    check "Save As on an image opens no dialog"
      (edDialog (saveAsDialogFlow edImg) == Nothing)

    -- The dialog, and the effect it produces.
    let edDlg = saveAsDialogFlow edWb
    check "the export dialog says it is an export"
      (case edDialog edDlg of
         Just d  -> T.pack "Export" `T.isInfixOf` dlgTitle d
         Nothing -> False)
    let (edAfter, effs) = update KEnter edDlg
    checkEq "confirming exports rather than saving"
      [ (p, t) | EffExportTo p t <- effs ] [("/tmp/book-Data.csv", T.pack "a,\"b,c\"")]
    checkEq "an export emits no save" (length [ () | EffSaveTo _ <- effs ]) 0
    -- The point of the whole distinction: the document is still the workbook.
    checkEq "the open document keeps its own path" (edPath edAfter) (Just "/tmp/book.xlsx")
    check "and is still the workbook view" (maybe False (const True) (edSheets edAfter))
    -- An ordinary document's Save As is unchanged: it writes and retitles.
    let edTxt = setLoaded "/tmp/a.txt" (emptyLoadResult { lrBuffer = fromText (T.pack "hi") }) ed0
        (edTxt2, effsTxt) = update KEnter (saveAsDialogFlow edTxt)
    checkEq "an ordinary Save As still saves" (length [ () | EffSaveTo _ <- effsTxt ]) 1
    checkEq "an ordinary Save As still retitles" (edPath edTxt2) (Just "/tmp/a.txt")

    -- Exporting over the file the view came from would destroy it, and Save As
    -- does not confirm before overwriting.
    let edSelf = case edDialog edDlg of
          Just d  -> edDlg { edDialog = Just d
                       { dlgFields = [ fl { fText = T.pack "/tmp/book.xlsx" }
                                     | fl <- dlgFields d ] } }
          Nothing -> edDlg
        (edSelfAfter, effsSelf) = update KEnter edSelf
    checkEq "exporting over the source archive is refused" (length effsSelf) 0
    check "and says why" (T.pack "came from" `T.isInfixOf` edStatus edSelfAfter)

    -- The menu entry says which of the two it is.
    let fileItems e = [ lbl | MEItem lbl _ MASaveAs <- entriesFor e 0 ]
    check "the workbook's File menu offers a CSV export"
      (any (T.isInfixOf (T.pack "CSV")) (fileItems edWb))
    check "the document's File menu offers a text export"
      (any (T.isInfixOf (T.pack "Text")) (fileItems edDoc))
    check "an ordinary document's File menu still says Save As"
      (any (T.isInfixOf (T.pack "Save")) (fileItems edTxt))

  -- OpenDocument (Cmedit.Odf) ---------------------------------------------------
  -- The third container format, onto the same two targets. What is asserted
  -- here is what makes ODF different from OOXML rather than what they share:
  -- formatting comes from a style table, lengths are CSS lengths, and a
  -- spreadsheet is padded out to its full width with repeat counts.
  do
    let bytes = TE.encodeUtf8 . T.pack
        content styles body = bytes (concat
          [ "<office:document-content>"
          , "<office:automatic-styles>", styles, "</office:automatic-styles>"
          , "<office:body>", body, "</office:body></office:document-content>" ])
        doc styles body = content styles ("<office:text>" ++ body ++ "</office:text>")
        sheet body = content "" ("<office:spreadsheet>" ++ body ++ "</office:spreadsheet>")
        pars styles body = toList (fst (Odf.odfPars (doc styles body)))
        texts styles body = map (T.concat . map Rtf.rrText . Rtf.rpRuns) (pars styles body)
        fmts styles body = map Rtf.rrFmt (concatMap Rtf.rpRuns (pars styles body))

    -- Detection: the manifest says it is ODF, the body says which kind.
    checkEq "odf is detected by content and manifest"
      (Odf.isOdf (map T.pack ["content.xml", "META-INF/manifest.xml", "styles.xml"])) True
    checkEq "an archive with no manifest is not odf"
      (Odf.isOdf (map T.pack ["content.xml"])) False
    checkEq "a text body is a document"
      (Odf.odfKind (doc "" "<text:p>x</text:p>")) (Just Odf.OdfText)
    checkEq "a spreadsheet body is a spreadsheet"
      (Odf.odfKind (sheet "<table:table/>")) (Just Odf.OdfSheet)
    -- A presentation is neither, and falls back to the archive listing rather
    -- than being rendered as something it is not.
    checkEq "a presentation has no reading view"
      (Odf.odfKind (content "" "<office:presentation/>")) Nothing

    -- CSS lengths, which OOXML did not need: it writes bare twips.
    checkEq "an inch is 1440 twips" (Odf.lengthToTwips (T.pack "1in")) (Just 1440)
    checkEq "half an inch" (Odf.lengthToTwips (T.pack "0.5in")) (Just 720)
    checkEq "a centimetre" (Odf.lengthToTwips (T.pack "1cm")) (Just 567)
    checkEq "a point" (Odf.lengthToTwips (T.pack "12pt")) (Just 240)
    checkEq "a negative length" (Odf.lengthToTwips (T.pack "-0.25in")) (Just (-360))
    checkEq "not a length" (Odf.lengthToTwips (T.pack "auto")) Nothing

    -- The style table. This is the whole difference from a .docx: matching on
    -- elements alone would see no formatting at all.
    let st1 = "<style:style style:name=\"T1\" style:family=\"text\">"
              ++ "<style:text-properties fo:font-weight=\"bold\"/></style:style>"
              ++ "<style:style style:name=\"T2\" style:family=\"text\" style:parent-style-name=\"T1\">"
              ++ "<style:text-properties fo:font-style=\"italic\" fo:color=\"#cc0000\"/></style:style>"
              ++ "<style:style style:name=\"T3\" style:family=\"text\">"
              ++ "<style:text-properties style:text-underline-style=\"solid\""
              ++ " style:text-line-through-style=\"solid\" fo:font-size=\"14pt\"/></style:style>"
              ++ "<style:style style:name=\"P1\" style:family=\"paragraph\">"
              ++ "<style:paragraph-properties fo:text-align=\"center\""
              ++ " fo:margin-left=\"0.5in\" fo:text-indent=\"-0.25in\"/></style:style>"
    check "a span's style makes it bold"
      (all Rtf.rfBold (fmts st1 "<text:p><text:span text:style-name=\"T1\">x</text:span></text:p>"))
    -- Style inheritance: T2's parent is T1, so it is bold *and* italic.
    check "a style inherits from its parent"
      (case fmts st1 "<text:p><text:span text:style-name=\"T2\">x</text:span></text:p>" of
         (f : _) -> Rtf.rfBold f && Rtf.rfItalic f && Rtf.rfColor f == Just (ColorRGB 0xCC 0 0)
         _       -> False)
    -- ODF spells underline and strike as *line styles*, so anything but "none"
    -- is on — a reader looking for a boolean sees nothing.
    check "underline and strike are line styles"
      (case fmts st1 "<text:p><text:span text:style-name=\"T3\">x</text:span></text:p>" of
         (f : _) -> Rtf.rfUnder f && Rtf.rfStrike f && Rtf.rfSize f == 28
         _       -> False)
    check "an unknown style name changes nothing"
      (fmts st1 "<text:p><text:span text:style-name=\"NOPE\">x</text:span></text:p>"
         == [Rtf.defaultFmt])
    -- Spans nest, and formatting does not leak past the one it applies to.
    check "spans nest"
      (case fmts st1 ("<text:p>a<text:span text:style-name=\"T1\">b"
                      ++ "<text:span text:style-name=\"T2\">c</text:span>d</text:span>e</text:p>") of
         [a, b, c, d, e] -> not (Rtf.rfBold a) && Rtf.rfBold b
                          && Rtf.rfBold c && Rtf.rfItalic c
                          && Rtf.rfBold d && not (Rtf.rfItalic d)
                          && not (Rtf.rfBold e)
         _ -> False)
    -- A paragraph style carries block properties, in CSS lengths.
    checkEq "a paragraph style sets alignment and indents"
      (map (\p -> (Rtf.rpAlign p, Rtf.rpLeft p, Rtf.rpFirst p))
           (pars st1 "<text:p text:style-name=\"P1\">x</text:p>"))
      [(Rtf.AlignCenter, 720, -360)]

    -- Text content.
    checkEq "paragraphs" (texts "" "<text:p>one</text:p><text:p>two</text:p>")
      [T.pack "one", T.pack "two"]
    -- A heading names its own level, which is why styles.xml never has to be
    -- read to recognise one.
    check "a heading gets a heading size"
      (case fmts "" "<text:h text:outline-level=\"1\">Title</text:h>" of
         (f : _) -> Rtf.rfSize f >= 28
         _       -> False)
    checkEq "empty paragraphs are dropped and the rest are spaced"
      (texts "" "<text:p>a</text:p><text:p/><text:p>b</text:p>") [T.pack "a", T.pack "b"]
    check "paragraphs are spaced" (all Rtf.rpSpace (pars "" "<text:p>a</text:p>"))
    -- ODF collapses whitespace like HTML and writes real runs of spaces as
    -- <text:s>, so both halves have to be handled or the document loses them.
    checkEq "whitespace collapses" (texts "" "<text:p>a\n   b</text:p>") [T.pack "a b"]
    checkEq "text:s is a run of real spaces"
      (texts "" "<text:p>a<text:s text:c=\"4\"/>b</text:p>") [T.pack "a    b"]
    checkEq "tabs and line breaks"
      (texts "" "<text:p>a<text:tab/>b<text:line-break/>c</text:p>") [T.pack "a\tb\nc"]
    checkEq "entities are resolved" (texts "" "<text:p>a &amp; b</text:p>") [T.pack "a & b"]
    -- A list item's content *is* a text:p, so the item's bullet and indent
    -- have to survive that paragraph starting.
    checkEq "list items are bulleted"
      (texts "" ("<text:list><text:list-item><text:p>one</text:p></text:list-item>"
                 ++ "<text:list-item><text:p>two</text:p></text:list-item></text:list>"))
      [T.pack "\x2022 one", T.pack "\x2022 two"]
    check "and indented, and set tight"
      (case pars "" "<text:list><text:list-item><text:p>one</text:p></text:list-item></text:list>" of
         (p : _) -> Rtf.rpLeft p > 0 && Rtf.rpFirst p < 0 && not (Rtf.rpSpace p)
         _       -> False)
    -- A table row is one paragraph and its cells are tab stops, as in a .docx.
    checkEq "a table row lays out on tab stops"
      (texts "" ("<table:table><table:table-row>"
                 ++ "<table:table-cell><text:p>a</text:p></table:table-cell>"
                 ++ "<table:table-cell><text:p>b</text:p></table:table-cell>"
                 ++ "</table:table-row></table:table>"))
      [T.pack "a\tb"]
    -- Marginalia is not body text.
    checkEq "footnotes and annotations are skipped"
      (texts "" ("<text:p>keep<text:note><text:note-body><text:p>gone</text:p>"
                 ++ "</text:note-body></text:note></text:p>"))
      [T.pack "keep"]
    checkEq "an odt with no text yields nothing"
      (Seq.length (fst (Odf.odfPars (doc "" "")))) 0

    -- Spreadsheets.
    let cell v t = "<table:table-cell office:value=\"" ++ v ++ "\"><text:p>" ++ t
                   ++ "</text:p></table:table-cell>"
        table nm rows = "<table:table table:name=\"" ++ nm ++ "\">" ++ rows ++ "</table:table>"
        rowOf cs = "<table:table-row>" ++ cs ++ "</table:table-row>"
        sheets body = let (ss, _) = Odf.odfSheets (sheet body)
                      in [ (n, fmap toList (toList g), M.toList f) | (n, g, f) <- ss ]

    -- The display text wins over the raw value: this is where an .ods reads
    -- better than an .xlsx, because the number format is already applied.
    checkEq "a cell shows its displayed text, not its raw value"
      (map (\(_, g, _) -> g) (sheets (table "S" (rowOf (cell "1234.5" "$1,234.50")))))
      [[[T.pack "$1,234.50"]]]
    checkEq "a cell with no displayed text falls back to its value"
      (map (\(_, g, _) -> g)
           (sheets (table "S" (rowOf "<table:table-cell office:value=\"7\"/>"))))
      [[[T.pack "7"]]]
    checkEq "sheets are named" (map (\(n, _, _) -> n) (sheets (table "Sales" "" ++ table "Costs" "")))
      (map T.pack ["Sales", "Costs"])

    -- The repeat counts, which are not optional to handle: LibreOffice pads
    -- every row out to 1024 columns and every sheet to 1048576 rows.
    checkEq "a repeated cell is expanded"
      (map (\(_, g, _) -> g)
        (sheets (table "S" (rowOf (cell "1" "a"
                 ++ "<table:table-cell table:number-columns-repeated=\"3\""
                 ++ " office:value=\"2\"><text:p>b</text:p></table:table-cell>"
                 ++ cell "3" "c")))))
      [[map T.pack ["a", "b", "b", "b", "c"]]]
    checkEq "a trailing run of empty cells is dropped, not materialised"
      (map (\(_, g, _) -> map length g)
        (sheets (table "S" (rowOf (cell "1" "a"
                 ++ "<table:table-cell table:number-columns-repeated=\"1023\"/>")))))
      [[1]]
    checkEq "and so is a trailing run of empty rows"
      (map (\(_, g, _) -> length g)
        (sheets (table "S" (rowOf (cell "1" "a")
                 ++ "<table:table-row table:number-rows-repeated=\"1048575\">"
                 ++ "<table:table-cell table:number-columns-repeated=\"1024\"/>"
                 ++ "</table:table-row>"))))
      [1]
    checkEq "a repeated row with content is expanded"
      (map (\(_, g, _) -> length g)
        (sheets (table "S" ("<table:table-row table:number-rows-repeated=\"3\">"
                 ++ cell "1" "x" ++ "</table:table-row>"))))
      [3]

    -- Formulas: recorded only where the file gave no displayed value, which
    -- for an .ods is rare — and a formula cell must survive the trailing trim
    -- precisely because it looks empty.
    checkEq "a calculated cell records no formula"
      (map (\(_, _, f) -> f)
        (sheets (table "S" (rowOf ("<table:table-cell table:formula=\"of:=SUM([.A1:.B1])\""
                 ++ " office:value=\"3\"><text:p>3</text:p></table:table-cell>")))))
      [[]]
    checkEq "an uncalculated one is recorded, and is not trimmed away"
      (map (\(_, g, f) -> (map length g, f))
        (sheets (table "S" (rowOf (cell "1" "a"
                 ++ "<table:table-cell table:formula=\"of:=SUM([.A1:.B1])\"/>")))))
      [([2], [((0, 1), T.pack "SUM(A1:B1)")])]

    -- ODF's formula syntax is not Excel's: every reference is bracketed and a
    -- same-sheet one is prefixed with a dot.
    checkEq "a same-sheet reference loses its brackets and dot"
      (Odf.odfFormula (T.pack "of:=SUM([.A1:.B9])")) (T.pack "SUM(A1:B9)")
    checkEq "a cross-sheet reference becomes an Excel one"
      (Odf.odfFormula (T.pack "of:=[Sheet2.A1]*2")) (T.pack "Sheet2!A1*2")
    checkEq "the oooc namespace is handled too"
      (Odf.odfFormula (T.pack "oooc:=[.A1]+1")) (T.pack "A1+1")
    checkEq "a bare = is handled" (Odf.odfFormula (T.pack "=[.A1]")) (T.pack "A1")
    checkEq "nothing at all is nothing" (Odf.odfFormula T.empty) T.empty
    -- And the translation feeds straight into the evaluator.
    checkEq "a translated formula evaluates"
      (let g = Seq.fromList [Seq.fromList (map T.pack ["7", "6", ""])]
           (out, c, _) = Fm.evalWorkbook
             [ Fm.SheetIn (T.pack "S") g
                 (M.singleton (0, 2) (Odf.odfFormula (T.pack "of:=SUM([.A1:.B1])"))) ]
       in (case out of ((_, g') : _) -> maybe T.empty id (Seq.lookup 0 g' >>= Seq.lookup 2)
                       _ -> T.empty, c))
      (T.pack "13", 1)

  -- Command-line conversion (cmedit FILE > out.txt) ---------------------------
  -- Every reading view already turns an awkward format into text, so this
  -- asserts the decision layer on top: what each format converts *to*, and the
  -- cases that refuse rather than emitting something useless.
  do
    tmpDir <- getTemporaryDirectory
    let cfg = defaultConfig
        write name t = do
          let p = tmpDir </> name
          BS.writeFile p t
          pure p
        le16, le32 :: Int -> BS.ByteString
        le16 n = BS.pack [fromIntegral (n .&. 0xff), fromIntegral ((n `shiftR` 8) .&. 0xff)]
        le32 n = le16 (n .&. 0xffff) <> le16 ((n `shiftR` 16) .&. 0xffff)
        u8t = TE.encodeUtf8 . T.pack
        -- A minimal stored-member ZIP, as the archive tests build one.
        zipOf items =
          let local (nm, dat) =
                BS.concat [ le32 0x04034b50, le16 20, le16 0x800, le16 0, le16 0, le16 0
                          , le32 0, le32 (BS.length dat), le32 (BS.length dat)
                          , le16 (BS.length nm), le16 0, nm, dat ]
              locals = map local items
              offs = scanl (+) 0 (map BS.length locals)
              central (off, (nm, dat)) =
                BS.concat [ le32 0x02014b50, le16 20, le16 20, le16 0x800, le16 0
                          , le16 0, le16 0, le32 0
                          , le32 (BS.length dat), le32 (BS.length dat)
                          , le16 (BS.length nm), le16 0, le16 0, le16 0, le16 0
                          , le32 0, le32 off, nm ]
              cds = map central (zip offs items)
          in BS.concat (locals ++ cds ++
               [ BS.concat [ le32 0x06054b50, le16 0, le16 0
                           , le16 (length items), le16 (length items)
                           , le32 (sum (map BS.length cds))
                           , le32 (sum (map BS.length locals)), le16 0 ] ])

    -- An .rtf is text on disk, so it arrives as a buffer of *markup* — and
    -- markup is the one thing nobody redirecting this wants.
    rtfP <- write "cmedit-conv.rtf" (u8t "{\\rtf1\\ansi Hello \\b there\\b0 .\\par Second.\\par}")
    rc <- convertPath cfg 1 rtfP
    checkEq "an rtf converts to its document text, not its markup"
      (fmap fst rc) (Right (T.pack "Hello there.\nSecond.\n"))
    check "and the description says what it read"
      (case rc of Right (_, d) -> "RTF document" `isInfixOf` d; _ -> False)

    -- A workbook converts one sheet, because a CSV holds one table.
    let sheetXml n = u8t ("<worksheet><sheetData><row r=\"1\">"
                          ++ "<c r=\"A1\" t=\"inlineStr\"><is><t>" ++ n ++ "</t></is></c>"
                          ++ "<c r=\"B1\"><v>1</v></c></row></sheetData></worksheet>")
    xlsxP <- write "cmedit-conv.xlsx" (zipOf
      [ (u8t "xl/workbook.xml", u8t ("<workbook xmlns:r=\"x\"><sheets>"
          ++ "<sheet name=\"One\" r:id=\"rId1\"/><sheet name=\"Two\" r:id=\"rId2\"/>"
          ++ "</sheets></workbook>"))
      , (u8t "xl/_rels/workbook.xml.rels", u8t ("<Relationships>"
          ++ "<Relationship Id=\"rId1\" Target=\"worksheets/a.xml\"/>"
          ++ "<Relationship Id=\"rId2\" Target=\"worksheets/b.xml\"/></Relationships>"))
      , (u8t "xl/worksheets/a.xml", sheetXml "first")
      , (u8t "xl/worksheets/b.xml", sheetXml "second") ])
    wc <- convertPath cfg 1 xlsxP
    checkEq "a workbook converts to CSV" (fmap fst wc) (Right (T.pack "first,1"))
    check "and says which sheet, and that there are others"
      (case wc of Right (_, d) -> "sheet 1" `isInfixOf` d && "--sheet" `isInfixOf` d
                  _            -> False)
    wc2 <- convertPath cfg 2 xlsxP
    checkEq "--sheet picks another one" (fmap fst wc2) (Right (T.pack "second,1"))
    -- Out of range clamps rather than failing: the file is still convertible.
    wc9 <- convertPath cfg 9 xlsxP
    checkEq "a sheet number past the end clamps" (fmap fst wc9) (Right (T.pack "second,1"))

    -- An archive with nothing recognisable inside converts to its listing.
    zipP <- write "cmedit-conv.zip" (zipOf [(u8t "readme.txt", u8t "hi")])
    zc <- convertPath cfg 1 zipP
    check "an archive converts to its listing"
      (case zc of Right (t, d) -> T.pack "readme.txt" `T.isInfixOf` t
                                  && "archive listing" `isInfixOf` d
                  _ -> False)

    -- Plain text converts to itself, which is what makes the rule "everything
    -- it can open, it can convert" true without exceptions.
    txtP <- write "cmedit-conv.txt" (u8t "one\ntwo\n")
    tc <- convertPath cfg 1 txtP
    checkEq "plain text converts to itself" (fmap fst tc) (Right (T.pack "one\ntwo\n"))

    -- The refusals. Each one exists because the alternative is emitting
    -- something useless into whatever the user redirected into.
    imgP <- write "cmedit-conv.bmp" (mkBMP 1 1 [(255, 0, 0)])
    ic <- convertPath cfg 1 imgP
    check "an image refuses, rather than converting to nothing"
      (case ic of Left m -> "image" `isInfixOf` m; _ -> False)
    -- A missing path is a *new buffer* to the editor and an error here.
    mc <- convertPath cfg 1 (tmpDir </> "cmedit-conv-does-not-exist.pdf")
    check "a missing file refuses rather than converting an empty buffer"
      (case mc of Left m -> "no such file" `isInfixOf` m; _ -> False)
    dc <- convertPath cfg 1 tmpDir
    check "a directory refuses" (case dc of Left m -> "directory" `isInfixOf` m; _ -> False)

    forM_ ["cmedit-conv.rtf", "cmedit-conv.xlsx", "cmedit-conv.zip"
          , "cmedit-conv.txt", "cmedit-conv.bmp"] $ \f -> void
      (try (removeFile (tmpDir </> f)) :: IO (Either SomeException ()))

  -- Plain-text export of the reading views (shared with Save As) --------------
  do
    let mkPar t = Rtf.defaultPar { Rtf.rpRuns = [Rtf.RtfRun (T.pack t) Rtf.defaultFmt]
                                 , Rtf.rpSpace = True }
        rd = Rtf.mkRtfDocFrom RtfFromBuffer Seq.empty T.empty
               (Seq.fromList [mkPar "First", mkPar "Second"])
    -- The paragraphs, not the laid-out lines: a file wrapped to whatever width
    -- the terminal happened to be is a poor artifact, and this one has not
    -- even been laid out.
    checkEq "the document exports as unwrapped paragraphs"
      (Rtf.rtfPlainText rd) (T.pack "First\n\nSecond\n\n")
    checkEq "an empty document exports as nothing"
      (Rtf.rtfPlainText (Rtf.mkRtfDocFrom RtfFromBuffer Seq.empty T.empty Seq.empty))
      T.empty

  -- Find in the table view -----------------------------------------------------
  -- Ctrl+F searches *cells* and moves the cell cursor, which is what a grid
  -- can do with a hit; the text view's character ranges have nowhere to go.
  do
    let csvText = T.pack "name,city,note\nAlice,Paris,red\nBob,Rome,green\nCarol,Paris,blue\n"
        ed0 = setLoaded "/tmp/find.csv"
                (emptyLoadResult { lrBuffer = fromText csvText })
                (newEditor (24, 80) defaultConfig)
        cellOfCur e = case edCsv e of
          Just v  -> (csvCurRow v, csvCurCol v)
          Nothing -> (-1, -1)
        findFor t e = doFind e { edSearchTerm = T.pack t }
        nextIn e = findAgain True e
        prevIn e = findAgain False e

    check "a .csv opens in the table view" (isJust' (edCsv ed0))
    -- The hit becomes the cell cursor, so Find and \"look at it\" are one step.
    let f1 = findFor "Paris" ed0
    checkEq "find moves the cell cursor to the matching cell" (cellOfCur f1) (1, 1)
    checkEq "and says which cell, and which match" (edStatus f1) (T.pack "Match 1 of 2 in B2")
    let f2 = nextIn f1
    checkEq "find next advances to the next matching cell" (cellOfCur f2) (3, 1)
    checkEq "and counts up" (edStatus f2) (T.pack "Match 2 of 2 in B4")
    -- Wrapping, in both directions.
    checkEq "find next wraps at the end" (cellOfCur (nextIn f2)) (1, 1)
    checkEq "find previous goes back" (cellOfCur (prevIn f2)) (1, 1)
    checkEq "find previous wraps at the start" (cellOfCur (prevIn f1)) (3, 1)
    -- A substring of a cell counts: cells are not matched whole.
    checkEq "a substring matches" (cellOfCur (findFor "rol" ed0)) (3, 0)   -- "Carol"
    checkEq "a miss says so and moves nothing"
      (let e = findFor "zzz" ed0 in (cellOfCur e, T.pack "Not found" `T.isPrefixOf` edStatus e))
      ((0, 0), True)
    -- Case folding follows the dialog's option, as everywhere else.
    checkEq "find is case-insensitive by default" (cellOfCur (findFor "paris" ed0)) (1, 1)
    checkEq "and case-sensitive when asked"
      (let e = doFind ed0 { edSearchTerm = T.pack "paris", edSearchCase = True }
       in T.pack "Not found" `T.isPrefixOf` edStatus e) True

    -- The live count in the dialog, which the grid counts in *cells* because a
    -- cell is what Find Next steps through.
    let dlgOf e = openFind e { edSearchTerm = T.pack "Paris" }
    check "the dialog shows a live count of matching cells"
      (case edDialog (dlgOf ed0) of
         Just d  -> dlgMessage d == T.pack "2 matching cells"
         Nothing -> False)
    check "and says so in the singular"
      (case edDialog (openFind ed0 { edSearchTerm = T.pack "Rome" }) of
         Just d  -> dlgMessage d == T.pack "1 matching cell"
         Nothing -> False)
    check "and reports no matches"
      (case edDialog (openFind ed0 { edSearchTerm = T.pack "zzz" }) of
         Just d  -> dlgMessage d == T.pack "No matches"
         Nothing -> False)

    -- The live highlight: every matching cell is lit while the dialog is open,
    -- below the cursor and above the selection. Asserted on the cell grid,
    -- since it is a style and the frame diff would not say which cells got it.
    let lit e = let scr = renderEditor e
                    fm = thFindMatch (themeFor (resolvedTheme e))
                in length [ () | i <- [0 .. scrW scr * scrH scr - 1]
                          , cellStyle (scrCells scr A.! i) == fm ]
    -- "Paris" is five cells wide and appears in two cells of the grid.
    check "matching cells are lit while the Find dialog is open"
      (lit (openFind ed0 { edSearchTerm = T.pack "Paris" }) >= 10)
    check "a grid with no matches lights nothing"
      (lit (openFind ed0 { edSearchTerm = T.pack "zzz" }) == 0)
    check "and nothing is lit with no dialog open" (lit ed0 == 0)

    -- Both bounds exist because the count is a full scan whenever the matches
    -- are sparse, and it runs on every keystroke in the dialog. Past the bound
    -- the count goes; *finding* never does.
    let bigGrid = Seq.fromList [ Seq.fromList [ T.pack (show (r * c))
                                              | c <- [0 .. 19 :: Int] ]
                               | r <- [0 .. 2000 :: Int] ]
        edBig = ed0 { edCsv = Just (mkCsvGrid ',' bigGrid) }
    check "a grid past the live-count bound shows no count"
      (case edDialog (openFind edBig { edSearchTerm = T.pack "1" }) of
         Just d  -> dlgMessage d == T.empty
         Nothing -> False)
    check "but still finds, and still says where"
      (let e = doFind edBig { edSearchTerm = T.pack "3999" }
       in T.pack "Found in " `T.isPrefixOf` edStatus e)

  -- Save/load round-trip matrix (plan 0013) ----------------------------------
  -- Saving is the one operation where a bug silently corrupts the user's file,
  -- so pin the exact bytes for every combination of line ending, BOM and final
  -- newline BEFORE touching the writer.
  do
    tmpDir <- getTemporaryDirectory
    let tmp = tmpDir </> "cmedit-save-roundtrip.txt"
        cases =
          [ ("lf, final nl",        LF,   Utf8,    True,  ["alpha", "beta"])
          , ("lf, no final nl",     LF,   Utf8,    False, ["alpha", "beta"])
          , ("crlf, final nl",      CRLF, Utf8,    True,  ["alpha", "beta"])
          , ("crlf, no final nl",   CRLF, Utf8,    False, ["alpha", "beta"])
          , ("cr, final nl",        CR,   Utf8,    True,  ["alpha", "beta"])
          , ("bom + lf",            LF,   Utf8Bom, True,  ["alpha", "beta"])
          , ("bom + crlf",          CRLF, Utf8Bom, True,  ["alpha", "beta"])
          , ("empty buffer",        LF,   Utf8,    True,  [""])
          , ("single line, no nl",  LF,   Utf8,    False, ["only"])
          , ("trailing empty line", LF,   Utf8,    True,  ["a", ""])
          , ("blank lines",         LF,   Utf8,    True,  ["", "", "x", ""])
          , ("unicode + wide",      LF,   Utf8,    True,  ["\27979\35797 \128512", "caf\233"])
          , ("embedded cr",         LF,   Utf8,    True,  ["a\rb"])
          , ("long line",           LF,   Utf8,    True,  [T.unpack (T.replicate 5000 (T.pack "x"))])
          ]
    forM_ cases $ \(nm, le, enc, fin, lns) -> do
      let buf = fromText (T.intercalate (lineEndingText le) (map T.pack lns))
      res <- saveFile tmp enc le fin buf
      case res of
        Left e -> check ("save " ++ nm ++ ": " ++ e) False
        Right (n, _) -> do
          raw <- BS.readFile tmp
          checkEq ("save " ++ nm ++ ": byte count matches") n (BS.length raw)
          -- The BOM is present iff requested, and exactly once.
          checkEq ("save " ++ nm ++ ": BOM") (BS.take 3 raw == BS.pack [0xEF,0xBB,0xBF])
                  (enc == Utf8Bom)
          -- Reloading reproduces the buffer, the line ending and the final-newline flag.
          lr <- loadFile tmp
          case lr of
            Left e -> check ("reload " ++ nm ++ ": " ++ e) False
            Right r -> do
              checkEq ("reload " ++ nm ++ ": encoding") (lrEncoding r) enc
              checkEq ("reload " ++ nm ++ ": final newline") (lrFinalNewline r) fin
              -- Content survives. Compared in LF form against the buffer that
              -- was actually saved: the loader normalises CR/CRLF to LF, so an
              -- embedded CR comes back as a line break.
              checkEq ("reload " ++ nm ++ ": content")
                      (bufferToText LF False (lrBuffer r))
                      (T.replace (T.pack "\r") (T.pack "\n") (bufferToText LF False buf))
              -- Re-saving what was loaded must reproduce the same bytes exactly.
              res2 <- saveFile tmp (lrEncoding r) (lrLineEnding r) (lrFinalNewline r) (lrBuffer r)
              raw2 <- BS.readFile tmp
              check ("re-save " ++ nm ++ ": byte-identical")
                    (either (const False) (const True) res2 && raw2 == raw)
    _ <- try (removeFile tmp) :: IO (Either SomeException ())
    pure ()

  -- Case-insensitive literal search: pre-filter must not change results (0019)
  -- The optimisation is only safe if it is invisible, so compare it against the
  -- straightforward fold-everything implementation over an awkward corpus.
  let refMatches cs ww term line
        | T.null term = []
        | otherwise =
            let norm t = if cs then t else T.toLower t
                len = T.length term
            in [ (i, len) | i <- S.scanMatches False ww (norm term) (norm line) ]
      corpusLines = map T.pack
        [ "hello world", "HELLO WORLD", "HeLLo", "no match here", ""
        , "ends with hell", "hellhello", "\1052\1086\1089\1082\1074\1072 Moscow"
        , "\223 sharp s", "SS sharp", "i\775 dotted", "\304stanbul", "istanbul"
        , "tab\there", "\27581\26412\35486 hello", "emoji \128512 hello", "  hello  " ]
      corpusTerms = map T.pack
        [ "hello", "HELLO", "Hello", "hell", "o w", "\1052\1086\1089", "\223", "SS"
        , "\304", "i", "x", " ", "\128512" ]
  forM_ corpusTerms $ \term -> forM_ corpusLines $ \ln -> forM_ [False, True] $ \ww ->
    forM_ [False, True] $ \cs ->
      checkEq ("lineMatches agrees with the reference (term " ++ show term
               ++ ", line " ++ show ln ++ ", cs=" ++ show cs ++ ", ww=" ++ show ww ++ ")")
              (S.lineMatches cs ww term ln) (refMatches cs ww term ln)

  -- Bracketed paste: bulk-scanned, straddle-safe, capped (plan 0017) ---------
  let pasteStart = [0x1b, 0x5b, 0x32, 0x30, 0x30, 0x7e]      -- ESC[200~
      pasteEnd   = [0x1b, 0x5b, 0x32, 0x30, 0x31, 0x7e]      -- ESC[201~
      bytesOf    = BS.unpack . TE.encodeUtf8 . T.pack
      wrapped p  = pasteStart ++ bytesOf p ++ pasteEnd
  -- The terminator can land at any offset within a chunk, including split
  -- across two: run every chunk size from 1 (byte at a time) upwards.
  forM_ [1, 2, 3, 4, 5, 6, 7, 13, 4096] $ \k -> do
    kp <- parseBytesChunked k (wrapped "hello\nworld")
    checkEq ("paste reassembles across chunk size " ++ show k) kp (KPaste (T.pack "hello\nworld"))
  -- Bytes after the terminator must not be swallowed: typing right after a
  -- paste is the regression this guards.
  forM_ [1, 3, 6, 4096] $ \k -> do
    (kp, nxt) <- parseTwoChunked k (wrapped "abc" ++ [0x7a])   -- 'z'
    checkEq ("paste then key, chunk " ++ show k) (kp, nxt) (KPaste (T.pack "abc"), KChar 'z')
  -- A payload containing a partial terminator is not mistaken for the end.
  do kp <- parseBytesChunked 4096 (wrapped "a\ESC[201b")
     checkEq "partial terminator inside the payload" kp (KPaste (T.pack "a\ESC[201b"))
  -- Unterminated paste ends at EOF with what it has.
  do kp <- parseBytesChunked 4096 (pasteStart ++ bytesOf "tail")
     checkEq "unterminated paste ends at EOF" kp (KPaste (T.pack "tail"))
  -- Invalid UTF-8 decodes leniently, exactly as before.
  do kp <- parseBytesChunked 4096 (pasteStart ++ [0xff, 0x41] ++ pasteEnd)
     checkEq "invalid UTF-8 in a paste is replaced"
             (case kp of KPaste t -> T.unpack t; _ -> "?") "\65533A"
  -- Empty paste.
  do kp <- parseBytesChunked 4096 (pasteStart ++ pasteEnd)
     checkEq "empty paste" kp (KPaste T.empty)
  -- KPasteTruncated is normalised to a paste plus a status note, so every
  -- consumer keeps working.
  let edTrunc = fst (update (KPasteTruncated (T.pack "abc")) (newEditor (24, 80) defaultConfig))
  checkEq "truncated paste still inserts" (getLine' 0 (edBuffer edTrunc)) (T.pack "abc")
  check "truncated paste says so" (T.isInfixOf (T.pack "too large") (edStatus edTrunc))

  -- Bounded histories really are bounded (plan 0001) -------------------------
  -- The cap must be structural: `take n (x:xs)` retains everything it claims to
  -- drop, so undo history grew for the whole session.
  let manyEdits n e = foldl (\acc i -> fst (update (if even (i :: Int) then KChar 'q'
                                                     else KBackspace) acc)) e [1 .. n]
      edDeep = manyEdits (maxUndo + 500) (newEditor (24, 80) defaultConfig)
  -- (`undoDepth` is used because a local `edUndo` binding above shadows the
  -- record selector for the rest of `main`.)
  checkEq "undo stack is capped at maxUndo" (undoDepth edDeep) maxUndo
  -- pushHist itself: capped, newest first, and the tail is really gone.
  checkEq "pushHist caps" (Seq.length (foldl (flip (pushHist 3)) Seq.empty [1 .. 10 :: Int])) 3
  checkEq "pushHist keeps newest first"
          (toList (foldl (flip (pushHist 3)) Seq.empty [1 .. 10 :: Int])) [10, 9, 8]
  -- Nav history and input history use the same bound.
  -- Nav history uses the same bound: Ctrl+Home/End jumps across a long file are
  -- "far", so alternating them fills the trail.
  let edFar0 = setLoaded "/f.txt"
                 (emptyLoadResult { lrBuffer = fromText (T.unlines (replicate 500 (T.pack "x"))) })
                 (newEditor (24, 80) defaultConfig)
      edNav = foldl (\e i -> fst (update (if even (i :: Int) then KEnd (Mods False True False)
                                                              else KHome (Mods False True False)) e))
                    edFar0 [1 .. 2 * (maxNavStops + 50)]
  check ("nav history is capped (" ++ show (Seq.length (edNavBack edNav)) ++ " stops)")
        (Seq.length (edNavBack edNav) <= maxNavStops)
  if not statsOn
    then check "undo retention guard skipped (build with -with-rtsopts=-T)" True
    else do
      (liveSmall, depthSmall) <- undoRetentionMB 4000
      (liveBig,   depthBig)   <- undoRetentionMB 40000
      checkEq "undo depth is the cap at 4k edits"  depthSmall maxUndo
      checkEq "undo depth is the cap at 40k edits" depthBig maxUndo
      -- 10x the edits must not mean 10x the retained history: the cap is the
      -- same in both runs, so the heap must be too. Without the structural
      -- bound the 40k run keeps 40 000 snapshots instead of 1 000.
      check ("undo history does not grow with session length (live "
             ++ show (round liveSmall :: Int) ++ " MB at 4k edits, "
             ++ show (round liveBig :: Int) ++ " MB at 40k)")
            (liveBig < liveSmall * 2 + 2)

  -- Text slices must not pin the buffers they came from (plan 0014) ----------
  if not statsOn
    then check "clipboard retention guard skipped (build with -with-rtsopts=-T)" True
    else do
      (live, docMB) <- clipboardRetentionMB
      -- Without 'detach' the 100-character clipboard entry keeps the whole
      -- ~4 MB document alive. Half the document is a generous ceiling that
      -- still fails loudly if the copy ever goes back to being a slice.
      check ("clipboard does not pin its source buffer (live " ++ show (round live :: Int)
             ++ " MB after copying one line out of " ++ show (round docMB :: Int) ++ " MB)")
            (live < docMB / 2)

  -- Lexer cost is LINEAR in the line length -----------------------------------
  -- 'lexWith' used to clamp each step's count with @min n (length cs)@, walking
  -- the whole remainder per token: O(line²). A 3000-column minified line then
  -- cost ~800ms *per frame* (the renderer re-lexes every visible line). This is
  -- a ratio guard, not an absolute one, so it survives any machine: linear
  -- gives ~4x for 4x the input, the old quadratic gave ~16x.
  let lexTime lang txt = do
        let go 0 !acc = pure acc
            go k !acc = do
              t0 <- getMonotonicTime
              let (ts, st) = lexLine lang initialState txt
              length ts `seq` st `seq` pure ()
              t1 <- getMonotonicTime
              go (k - 1 :: Int) (min acc (t1 - t0))
        go 3 (1 / 0)
      longLine n = T.pack (take n (cycle "var alpha = beta + 12; foo(bar, baz); "))
  forM_ [("JS", JS), ("Python", Python), ("SQL", SQL)] $ \(nm, lang) -> do
    tSmall <- lexTime lang (longLine 2000)
    tBig   <- lexTime lang (longLine 8000)
    -- 4x the input: linear ~4x, quadratic ~16x. 10x is the dividing line.
    check ("lexer is not quadratic in line length (" ++ nm ++ ", ratio "
           ++ show (round (tBig / max 1e-9 tSmall) :: Int) ++ "x for 4x input)")
          (tBig < tSmall * 10)
  -- The clamp it replaced still holds: exactly one token per character.
  forM_ [ "x = 'abc' + \"def\";  // trailing"
        , "\t\tif (a && b) { return c; }"
        , "select a, b from t where x = 1 -- c" ] $ \src ->
    forM_ [JS, Python, SQL, Haskell, YAML] $ \lang ->
      checkEq "lexLine emits one token per character"
              (length (fst (lexLine lang initialState (T.pack src)))) (length src)

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
  -- Interactive links: terminals with mouse reporting on rarely forward
  -- link clicks to their own OSC 8 handling, so the editor opens its links
  -- itself — Ctrl+Click or right-click, with a hand pointer and a status
  -- hint on hover.
  let edUrl = setLoaded "u.txt" (mkLR "see https://a.b/c end") ed0
      loU = computeLayout edUrl
      urlEvt btn mods press drag =
        MouseEvent btn (loTextLeft loU + 6) (loTextTop loU) press drag mods 1
      ctrlM = Mods False False True
      hasOpen effs = [ u | EffOpenUrl u <- effs ] == ["https://a.b/c"]
  check "link: ctrl+click on a url opens it"
        (hasOpen (snd (update (KMouse (urlEvt MBLeft ctrlM True False)) edUrl)))
  check "link: ctrl+click beside the url goes to definition instead"
        (null [ u | EffOpenUrl u <- snd (update
                 (KMouse (MouseEvent MBLeft (loTextLeft loU + 1) (loTextTop loU)
                            True False ctrlM 1)) edUrl) ])
  check "link: right-click on a url opens it"
        (hasOpen (snd (update (KMouse (urlEvt MBRight noMods True False)) edUrl)))
  check "link: hover sets the status hint and the hand pointer"
        (let hov = urlEvt MBNone noMods False True
             edH = fst (update (KMouse hov) edUrl)
         in edHoverUrl edH == Just "https://a.b/c"
            && pointerShapeAt hov edUrl == "pointer")
  check "link: a keystroke clears the hover hint"
        (let edH = fst (update (KMouse (urlEvt MBNone noMods False True)) edUrl)
         in edHoverUrl (fst (update (KChar 'x') edH)) == Nothing)
  check "link: hover hint is rendered on the status bar"
        (let edH = fst (update (KMouse (urlEvt MBNone noMods False True)) edUrl)
             scr = renderEditor edH
             row = [ cellChar (scrCells scr A.! (loStatusRow loU * scrW scr + c))
                   | c <- [0 .. scrW scr - 1] ]
         in "Ctrl+Click to open" `isInfixOf` row)
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
  -- The explorer divider and a CSV column border are the same width-drag
  -- gesture, so they hint the same shape — hovering the handle and mid-drag.
  let edPanel = explorerStart "/w" [("/w/a.txt", False, Just 3)] edPtr
      divCol  = loContentLeft (computeLayout edPanel) - 1
      panelRow = loTextTop (computeLayout edPanel) + 1
  checkEq "pointer: explorer divider offers col-resize"
          (pointerShapeFor edPanel panelRow divCol) "col-resize"
  checkEq "pointer: explorer rows stay a hand"
          (pointerShapeFor edPanel panelRow (divCol - 2)) "pointer"
  checkEq "pointer: a panel-width drag holds col-resize anywhere"
          (pointerShapeFor edPanel { edSidebarDrag = True } 5 60) "col-resize"
  checkEq "pointer: the collapsed strip is a hand, not a resize handle"
          (pointerShapeFor edPanel { edExpCollapsed = True } panelRow 0) "pointer"

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

  -- CSV parser: Text version must equal the String version it replaced (0016)
  -- Compared against the previous String implementation, kept as an oracle.
  -- Cases where a closing quote is followed by stray text are excluded here and
  -- pinned separately below: the old parser *reversed* the field in that path
  -- (a latent bug), so agreeing with it there would be wrong.
  let csvCorpus =
        [ "a,b,c", "a,b,c\n1,2,3", "", "\n", "a", "a,", ",a", ",,"
        , "\"quoted\",b", "\"with,delim\",b", "\"with\nnewline\",b"
        , "\"doubled\"\"quote\",b", "\"unterminated"
        , "\"\",x", "a\r\nb", "a\rb", "a\n\nb", "x,y\n", "x,y\n\n"
        , "\"a\"\"\",\"b\"", "ragged,row\nshort", "one\ntwo,three,four"
        , "\27979,\35797\n\128512,x", "  spaced  ,  cells  "
        , "\"multi\nline\ncell\",z", "tab\tinside,b" ]
  forM_ csvCorpus $ \src ->
    checkEq ("csvParse matches the reference (" ++ show src ++ ")")
            (csvParse ',' (T.pack src)) (csvParseRef ',' (T.pack src))
  -- Other delimiters, with data that actually uses them.
  forM_ [ (';', "a;b\n\"q;q\";c"), ('\t', "a\tb\n\"q\tq\"\tc"), ('|', "a|b|\"c\"") ] $
    \(d, src) ->
      checkEq ("csvParse matches the reference (delim " ++ show d ++ ")")
              (csvParse d (T.pack src)) (csvParseRef d (T.pack src))
  -- Stray text after a closing quote: tolerated (not rejected), and now
  -- appended in order. The old parser emitted it reversed — "tyartsail".
  checkEq "stray text after a closing quote is appended, not reversed"
          (csvParse ',' (T.pack "\"stray\"tail,b"))
          (Seq.fromList [Seq.fromList [T.pack "straytail", T.pack "b"]])
  checkEq "escaped quote at the end of a field"
          (csvParse ',' (T.pack "\"a\"\"\",\"b\""))
          (Seq.fromList [Seq.fromList [T.pack "a\"", T.pack "b"]])
  -- Parsing is idempotent once serialised (a trailing empty record collapses on
  -- the first pass, so compare the second and third generations).
  forM_ csvCorpus $ \src ->
    let once = csvToText (mkCsvView ',' (T.pack src))
        twice = csvToText (mkCsvView ',' once)
    in checkEq ("csv text is stable after one normalisation (" ++ show src ++ ")")
               (csvToText (mkCsvView ',' twice)) twice

  -- CSV parser: the line parser and the text parser are one engine (0026) ------
  -- The load path parses a buffer's *lines* ('csvParseLines') rather than
  -- re-joining them into one Text first, so the two must agree exactly — and
  -- both must agree with the parser that shipped before, kept below as
  -- 'csvParsePrev'. Randomised over tokens picked to hit every quirk: bare and
  -- doubled quotes, stray text after a closing quote, lone CR, CRLF, embedded
  -- newlines in quoted cells, ragged rows and multi-byte characters.
  let csvTok r = T.pack (case r `mod` 16 of
        0 -> "a"     ; 1  -> "bb,cc"      ; 2  -> ","      ; 3  -> "\""
        4 -> "\n"    ; 5  -> "\r\n"       ; 6  -> "\r"     ; 7  -> "\"q\""
        8 -> "\"a,b\""; 9 -> "\"x\ny\""   ; 10 -> "\"\"\"\""
        11 -> "z\"stray\"t"               ; 12 -> " "      ; 13 -> "\t"
        14 -> "\233"                      ; _  -> "\26085")
      csvGen s = T.concat [ csvTok r | r <- take (1 + s `mod` 14) (iterate lcg (lcg s)) ]
      csvSrcs = [ csvGen s | s <- take 400 (iterate lcg 991) ]
      -- The production equivalence: what the editor loads (a buffer's lines,
      -- CR already normalised by 'fromText') against what it used to load (the
      -- same buffer serialised back into one Text).
      lineBad = [ src | src <- csvSrcs
                , let buf = fromText src
                , csvParseLines ',' (bufLines buf)
                    /= csvParse ',' (bufferToText LF False buf) ]
      prevBad = [ (d, src) | d <- ",;\t|" :: String, src <- csvSrcs
                , csvParse d src /= csvParsePrev d src ]
      viewBad = [ src | src <- csvSrcs
                , let buf = fromText src
                      va  = mkCsvLines ',' (bufLines buf)
                      vb  = mkCsvView ',' (bufferToText LF False buf)
                , csvRows va /= csvRows vb || columnWidths va /= columnWidths vb ]
  checkEq "csvParseLines agrees with csvParse over 400 random inputs"
          (take 1 lineBad) []
  checkEq "csvParse agrees with the pre-0026 parser, 4 delimiters x 400 inputs"
          (take 1 prevBad) []
  checkEq "mkCsvLines agrees with mkCsvView (grid and widths)" (take 1 viewBad) []
  -- Edge cases of the line cursor, pinned by hand: the implicit newline
  -- between lines is where every one of them lives.
  let pl = Seq.fromList . map T.pack
      grid rs = Seq.fromList [ Seq.fromList (map T.pack r) | r <- rs ]
  checkEq "csvParseLines: an empty buffer is an empty grid"
          (csvParseLines ',' (pl [""])) Seq.empty
  checkEq "csvParseLines: two empty lines are one empty record"
          (csvParseLines ',' (pl ["", ""])) (grid [[""]])
  checkEq "csvParseLines: a trailing empty line is not a record"
          (csvParseLines ',' (pl ["a,b", ""])) (grid [["a", "b"]])
  checkEq "csvParseLines: a quoted cell spanning three lines"
          (csvParseLines ',' (pl ["a,\"x", "y", "z\",b"]))
          (grid [["a", "x\ny\nz", "b"]])
  checkEq "csvParseLines: an unterminated quote swallows the rest of the file"
          (csvParseLines ',' (pl ["a,\"x", "y"])) (grid [["a", "x\ny"]])
  checkEq "csvParseLines: doubled quotes inside a spanning cell"
          (csvParseLines ',' (pl ["\"a\"\"b", "c\""])) (grid [["a\"b\nc"]])
  checkEq "csvParseLines: ragged rows keep their own lengths"
          (csvParseLines ',' (pl ["a,b,c", "d", "e,f"]))
          (grid [["a", "b", "c"], ["d"], ["e", "f"]])
  checkEq "csvParseLines: stray text after a closing quote is appended"
          (csvParseLines ',' (pl ["\"stray\"tail,b"])) (grid [["straytail", "b"]])
  -- A CR inside a line ends the record mid-line (only reachable by editing —
  -- 'fromText' normalises the CRs a file arrives with — but the text parser
  -- has always done it, so the line parser must too).
  checkEq "csvParseLines: a lone CR ends the record mid-line"
          (csvParseLines ',' (pl ["a\rb,c"])) (grid [["a"], ["b", "c"]])
  checkEq "csvParseLines: a CR at the end of a line eats the line break"
          (csvParseLines ',' (pl ["a\r", "b"])) (grid [["a"], ["b"]])
  -- Cell width: the strict fold must equal the splitOn/unpack version it
  -- replaced, including multi-line cells, wide glyphs, variation selectors and
  -- truly invisible formatting characters.
  let widthSrcs = csvSrcs ++ map T.pack
        [ "", "abc", "ab\ncdef\ng", "\n", "ab\n", "\nabc", "\26085\26412"
        , "\8505\65039", "a\8203b", "\9888\65039x", "\t", "\1", "e\769" ]
      cwBad = [ t | t <- widthSrcs, cellWidth t /= cellWidthRef t ]
  checkEq "cellWidth agrees with the splitOn/unpack reference" (take 1 cwBad) []
  checkEq "cellWidth of a multi-line cell is its widest line"
          (cellWidth (T.pack "ab\ncdef\ng")) 4
  checkEq "cellWidth counts a wide glyph as two cells"
          (cellWidth (T.pack "\26085\26412")) 4
  checkEq "cellWidth ignores an invisible format character"
          (cellWidth (T.pack "a\8203b")) 2

  -- CSV column-width cache ------------------------------------------------------
  -- The cache maintained by withRows/undo/redo must always equal a fresh
  -- recomputation (serialise -> reparse is the ground truth), across cell
  -- edits, multi-line cells, row/column inserts/deletes and undo/redo.
  let widthsOk v = columnWidths v == columnWidths (mkCsvView (csvDelim v) (csvToText v))
      -- The dirty state from outside the module: plain structural comparison,
      -- no pointer tricks and no incremental anything (plan 0028).
      shapeRef g = map Seq.length (toList g)
      dirtyRef v
        | shapeRef (csvRows v) /= shapeRef (csvSaved v) = DirtyShape
        | otherwise = DirtyCells (length
            [ () | (r, s) <- zip (toList (csvRows v)) (toList (csvSaved v))
                 , (a, b) <- zip (toList r) (toList s), a /= b ])
      -- The embedded-newline map from outside the module (plan 0029), built
      -- only out of 'csvToText' — the serialisation 'cellTextPos' mirrors — so
      -- it shares no code at all with the cache it checks. One row through
      -- 'mkCsvGrid'/'csvToText' *is* that row's serialised form.
      nlT = T.pack "\n"
      serRow v row = csvToText (mkCsvGrid (csvDelim v) (Seq.singleton row))
      nlRef v = M.fromList
        [ (i, k)
        | (i, row) <- zip [0 ..] (toList (csvRows v))
        , let k = T.count nlT (serRow v row), k > 0 ]
      -- 'cellTextPos' as it was written before the cache: re-serialise every
      -- row above the target. This is the definition; the cache is the claim.
      cellTextPosRef v r c =
        let dl   = T.singleton (csvDelim v)
            row  = Seq.index (csvRows v) r
            base = r + sum [ T.count nlT (serRow v (Seq.index (csvRows v) i))
                           | i <- [0 .. r - 1] ]
            pre0 = serRow v (Seq.take c row)
            pre  = if c > 0 then pre0 <> dl else pre0
        in (base + T.count nlT pre, T.length (last (T.splitOn nlT pre)))
      -- ...and 'textPosCell' as it was: a running scan for the row whose
      -- serialised text contains the target line. The binary search that
      -- replaced it must answer identically, including off both ends. Note the
      -- fields have to come from the serialiser, not from splitting the row's
      -- serialised text on the delimiter: a quoted field may contain one.
      textPosCellRef v line col =
        let n      = nRows v
            starts = scanl (\acc i -> acc + 1 + T.count nlT
                                        (serRow v (Seq.index (csvRows v) i))) 0 [0 .. n - 1]
            r      = clampT 0 (n - 1) (length (takeWhile (<= line) starts) - 1)
            dl     = T.singleton (csvDelim v)
            fs     = [ serRow v (Seq.singleton f)
                     | f <- toList (Seq.index (csvRows v) r) ]
            colSt  = scanl (\acc f -> acc + T.length f + 1) 0 fs
            c      = clampT 0 (max 0 (length fs - 1))
                            (length (takeWhile (<= col) (initSafe colSt)) - 1)
        in dl `seq` (r, c)
      clampT lo hi = max lo . min hi
      initSafe [] = []
      initSafe xs = init xs
      -- Both mappings, at seeded positions plus each end of the grid. Sampled
      -- rather than exhaustive because the *reference* is O(rows) per call and
      -- the fuzz grid grows; every cell of a small multi-line grid is checked
      -- exhaustively in the 0029 block below.
      posMapsOk s v =
        let n  = nRows v
            rs = nub [0, n - 1, s `mod` n]
            wide row = [0 .. Seq.length row]
        in and [ cellTextPos v r c == cellTextPosRef v r c
               | r <- rs, c <- wide (Seq.index (csvRows v) r) ]
           && and [ textPosCell v l c == textPosCellRef v l c
                  | l <- nub [-1, 0, n - 1, n, n + 2, s `mod` (n + 4)]
                  , c <- [-1, 0, 3, 9] ]
      -- Every mutating operation in the module, so a site that moves csvRows
      -- or csvSaved without carrying the caches gets caught here.
      csvOp r v = case r `mod` 21 of
        0 -> setCurrentCell (T.pack (replicate (1 + r `mod` 40) 'x')) v
        1 -> setCurrentCell (T.pack "s") v
        2 -> insertRowBelow v
        3 -> deleteRow v
        4 -> insertColRight v
        5 -> deleteCol v
        6 -> Cmedit.Csv.undo v
        7 -> Cmedit.Csv.redo v
        8 -> commitEdit (editInsert 'q' (editInsert '\n' (beginEditFresh 'w' v)))
        -- Typing into a cell, one keystroke at a time and NOT committing
        -- between them: the per-keystroke width-cache path (plan 0016).
        9 -> foldl (\acc c -> editInsert c acc) (beginEdit v) ("widen" :: String)
        -- Shrinking a cell that may have been its column's widest: the one
        -- case where the cache cannot be updated in O(1).
        10 -> commitEdit (editBackspace (editBackspace (beginEdit v)))
        11 -> cancelEdit (editInsert 'z' (beginEdit v))
        12 -> insertRowAbove v
        13 -> insertColLeft v
        -- Wholesale rewrites and the shape-preserving permutation (sort), the
        -- three grid changes that cannot be described as "one row moved".
        14 -> mapCells (\t -> if T.null t then T.pack "f" else T.init t) v
        15 -> sortByColumn (r `mod` max 1 (nCols v)) (even r) (odd r) v
        16 -> clearSelCells (withSel (moveCursor DDown) (withSel (moveCursor DRight) v))
        17 -> fst (pasteClip (T.pack "P,Q\nR,S") v)
        18 -> fst (pasteClip (T.pack "one") (withSel (moveCursor DRight) v))
        -- A new saved point mid-history: everything above must re-baseline.
        19 -> markSaved v
        _ -> setCursor (r `mod` (nRows v + 1)) ((r `div` 7) `mod` (nCols v + 1)) v
      csvFuzzStep (v, s, ok, nlOk) _ =
        let s' = lcg s
            v' = csvOp s' v
        in (v', s'
           , ok && widthsOk v'
                       -- the maintained modified flag == plain equality
                       && Cmedit.Csv.isModified v' == (csvRows v' /= csvSaved v')
                       -- ...and the state behind it is exact, not merely
                       -- right-about-zero: a sign error that keeps the boolean
                       -- correct today would drift on the next edit.
                       && csvDirty v' == dirtyRef v'
                       -- the from-scratch path agrees with the incremental one
                       && dirtyFrom (csvSaved v') (csvRows v') == dirtyRef v'
             -- ...and, tracked separately so a failure names itself, the
             -- embedded-newline map (plan 0029): against the module's own
             -- recompute, against an oracle built only out of 'csvToText',
             -- and through the two mappings it exists to make cheap.
           , nlOk && csvNl v' == computeNl (csvDelim v') (csvRows v')
                  && csvNl v' == nlRef v'
                  && posMapsOk s' v')
      vw0 = mkCsvView ',' (T.pack "a,bb,ccc\ndddd,e,f\ng,hh,i")
      (_, _, csvWidthsOk, csvNlOk0) = foldl csvFuzzStep (vw0, 7, True, True) [1 .. 600 :: Int]
      -- A second run over a wider table with a different seed: more columns
      -- means more shape-preserving states, which is where the count lives.
      vw1 = mkCsvView ',' (T.pack "k,a,bb,3,x\n1,2,3,4,5\nz,,q,,w\nm,n,o,p,q")
      (_, _, csvDirtyOk, csvNlOk1) = foldl csvFuzzStep (vw1, 1234567, True, True) [1 .. 600 :: Int]
      -- A third, for 0029 specifically: a table that *starts* full of embedded
      -- newlines, so the map is non-empty from the first operation rather than
      -- only once the script happens to type one.
      vw2 = mkCsvView ',' (T.pack "a,\"b\nc\"\n\"d\ne\nf\",g\nh,i\n\"j\nk\",\"l\nm\"")
      (_, _, csvWidthsOk2, csvNlOk2) = foldl csvFuzzStep (vw2, 99, True, True) [1 .. 600 :: Int]
  check "csv width cache correct at load" (widthsOk vw0)
  check "csv width cache survives 600 random ops" csvWidthsOk
  check "csv dirty state survives 600 random ops on a wider table" csvDirtyOk
  check "csv width/dirty caches survive 600 ops on a multi-line table" csvWidthsOk2
  check "0029: csv newline map and text mappings survive 600 random ops" csvNlOk0
  check "0029: ...and 600 more on a wider table" csvNlOk1
  check "0029: ...and 600 more on a table full of multi-line cells" csvNlOk2
  checkEq "csv dirty state at load" (csvDirty vw0) (DirtyCells 0)

  -- The CSV modified flag as maintained state (plan 0028) -----------------------
  -- isModified is O(1) and exact: 'csvDirty' says whether the grid's shape
  -- differs from the saved grid's, and if not, how many cells do. Everything
  -- here is a way for that count to go wrong while the boolean still looks
  -- plausible on the next keystroke.
  let vm0 = mkCsvView ',' (T.pack "a,b,c\nd,e,f\ng,h,i")
      cellTo r c t = setCurrentCell (T.pack t) (setCursor r c vm0)
  checkEq "0028: a loaded table is clean" (csvDirty vm0) (DirtyCells 0)
  check "0028: a loaded table is not modified" (not (isModified vm0))
  -- Edit a cell, then edit it back to the value it was saved with: the count
  -- must come back to zero, not merely stop growing.
  let vmEdit = cellTo 1 1 "E"
      vmBack = setCurrentCell (T.pack "e") vmEdit
  checkEq "0028: one changed cell is one dirty cell" (csvDirty vmEdit) (DirtyCells 1)
  check "0028: one changed cell is modified" (isModified vmEdit)
  checkEq "0028: editing a cell back to its saved value is clean again"
          (csvDirty vmBack) (DirtyCells 0)
  check "0028: editing a cell back to its saved value clears the flag"
          (not (isModified vmBack))
  -- Two cells dirty, one put back: still modified. (A count that saturated at
  -- one, or a boolean that latched, would pass the case above and fail here.)
  let vmTwo  = setCurrentCell (T.pack "C") (setCursor 2 2 vmEdit)
      vmOne  = setCurrentCell (T.pack "i") (setCursor 2 2 vmTwo)
  checkEq "0028: two changed cells" (csvDirty vmTwo) (DirtyCells 2)
  checkEq "0028: one of two put back" (csvDirty vmOne) (DirtyCells 1)
  check "0028: still modified with one cell outstanding" (isModified vmOne)
  -- Typing character by character inside one cell, without committing: the
  -- per-keystroke path, which must not re-derive the count from the grid.
  let vmTyped = foldl (\acc ch -> editInsert ch acc) (beginEdit (setCursor 0 0 vm0)) ("bc" :: String)
  checkEq "0028: uncommitted typing is one dirty cell" (csvDirty vmTyped) (DirtyCells 1)
  checkEq "0028: cancelling that edit is clean again"
          (csvDirty (cancelEdit vmTyped)) (DirtyCells 0)
  -- Shape changes: modified with no cell comparison at all, and — the case an
  -- index-keyed design gets wrong — restoring the shape must re-derive the
  -- count rather than trust whatever the indices used to mean.
  let vmIns    = insertRowBelow vm0
      vmInsDel = deleteRow (setCursor 1 0 vmIns)
      vmCol    = insertColRight vm0
      vmColDel = deleteCol vmCol
  checkEq "0028: an inserted row is a shape change" (csvDirty vmIns) DirtyShape
  check "0028: an inserted row is modified" (isModified vmIns)
  checkEq "0028: deleting it again restores the shape and the count"
          (csvDirty vmInsDel) (DirtyCells 0)
  check "0028: shape change then revert is not modified" (not (isModified vmInsDel))
  checkEq "0028: an inserted column is a shape change" (csvDirty vmCol) DirtyShape
  checkEq "0028: deleting it again restores the shape and the count"
          (csvDirty vmColDel) (DirtyCells 0)
  -- A shape change on top of a dirty cell: reverting the shape must leave the
  -- cell still counted.
  let vmBoth = deleteRow (setCursor 3 0 (insertRowBelow (setCursor 2 0 vmEdit)))
  checkEq "0028: shape reverted over an outstanding cell edit"
          (csvDirty vmBoth) (DirtyCells 1)
  check "0028: still modified after the shape reverts" (isModified vmBoth)
  -- Undo back to clean is what the journal sweep relies on to drop a journal:
  -- an editor that stayed "modified" after undoing to the saved grid would keep
  -- offering to recover a file the user has not changed.
  let vmU = Cmedit.Csv.undo (commitEdit (beginEditFresh 'Z' (setCursor 1 1 vm0)))
  checkEq "0028: undo to the saved grid is clean" (csvDirty vmU) (DirtyCells 0)
  check "0028: undo to the saved grid is not modified" (not (isModified vmU))
  check "0028: redo is modified again" (isModified (Cmedit.Csv.redo vmU))
  -- Undo across a structural edit, both ways.
  let vmSU = Cmedit.Csv.undo (deleteRow (setCursor 1 0 vm0))
  checkEq "0028: undo of a row delete is clean" (csvDirty vmSU) (DirtyCells 0)
  checkEq "0028: redo of a row delete is a shape change"
          (csvDirty (Cmedit.Csv.redo vmSU)) DirtyShape
  -- Saving re-baselines: the grid on screen becomes the saved grid, and an
  -- edit after that counts from there.
  let vmSaved = markSaved vmTwo
  checkEq "0028: markSaved is clean" (csvDirty vmSaved) (DirtyCells 0)
  checkEq "0028: an edit after saving counts from the new baseline"
          (csvDirty (setCurrentCell (T.pack "q") (setCursor 0 0 vmSaved))) (DirtyCells 1)
  checkEq "0028: undoing past a save is dirty again"
          (csvDirty (Cmedit.Csv.undo vmSaved)) (DirtyCells 1)
  -- Replace-all over every cell, and back.
  let vmAll = mapCells T.toUpper vm0
  checkEq "0028: replace-all dirties every non-empty cell" (csvDirty vmAll) (DirtyCells 9)
  checkEq "0028: undoing replace-all is clean"
          (csvDirty (Cmedit.Csv.undo vmAll)) (DirtyCells 0)
  -- Sort: a shape-preserving permutation, so the count is the number of cells
  -- that ended up somewhere else.
  let vmSort = sortByColumn 0 False False vm0
  checkEq "0028: a descending sort of 3 distinct rows moves 6 cells"
          (csvDirty vmSort) (DirtyCells 6)
  checkEq "0028: sorting back is clean"
          (csvDirty (sortByColumn 0 True False vmSort)) (DirtyCells 0)
  -- Paste: a same-shaped overwrite, and the ragged case where a paste grows
  -- the grid (which is a shape change, not a count).
  let vmPaste = fst (pasteClip (T.pack "X,Y\nZ,W")
                      (withSel (moveCursor DDown) (withSel (moveCursor DRight) vm0)))
  checkEq "0028: a 2x2 paste over a 2x2 selection dirties four cells"
          (csvDirty vmPaste) (DirtyCells 4)
  let vmGrow = fst (pasteClip (T.pack "X,Y\nZ,W") (setCursor 2 2 vm0))
  checkEq "0028: a paste that grows the grid is a shape change"
          (csvDirty vmGrow) DirtyShape
  -- The grid a mode toggle rebases onto keeps the old saved point, so its dirty
  -- state has to be derived from scratch against it.
  let vmReb = rebaseHistory vmEdit (mkCsvView ',' (csvToText vmEdit))
  check "0028: a rebased view is modified against the old saved point"
        (isModified vmReb)
  checkEq "0028: a rebased view's count is exact" (csvDirty vmReb) (dirtyRef vmReb)
  let vmReb0 = rebaseHistory vm0 (mkCsvView ',' (csvToText vm0))
  check "0028: a rebased view with no text edit is clean" (not (isModified vmReb0))

  -- The embedded-newline map (plan 0029) ---------------------------------------
  -- 'cellTextPos' answers "which line of the serialised file does this cell
  -- start on?", which the recents, the session file, the crash journal and the
  -- nav history all ask. It used to re-serialise every row above the cursor —
  -- 383 ms and 1 651 MB at the last row of a 223 209-row table — and the
  -- session-shape check reached it from every keystroke. 'csvNl' is the sparse
  -- row -> embedded-newline-count map that replaces the walk; the invariant is
  --   Map.findWithDefault 0 i (csvNl v) == (newlines in row i's serialised form)
  -- and everything here is a way for it to drift while still looking plausible.
  let vnl0 = mkCsvView ',' (T.pack "a,b\nc,d\ne,f")
      -- A grid whose row 1 holds a two-line cell and row 3 a three-line one.
      vnlM = mkCsvView ',' (T.pack "a,b\n\"x\ny\",c\nd,e\n\"p\nq\nr\",s\nt,u")
  checkEq "0029: a table with no embedded newline has an empty map"
          (csvNl vnl0) M.empty
  checkEq "0029: rows with embedded newlines, and only those, are in the map"
          (csvNl vnlM) (M.fromList [(1, 1), (3, 2)])
  checkEq "0029: the map at load equals a from-scratch recompute"
          (csvNl vnlM) (computeNl ',' (csvRows vnlM))
  checkEq "0029: rowNl counts a row's newlines"
          (map (rowNl ',') (toList (csvRows vnlM))) [0, 1, 0, 2, 0]
  checkEq "0029: linesBefore is the running total, and 0 above row 0"
          (map (linesBefore vnlM) [0, 1, 2, 3, 4, 5]) [0, 0, 1, 1, 3, 3]
  -- The point of the whole cache: the line a row starts on.
  checkEq "0029: cellTextPos skips the lines an earlier cell added"
          (map (\r -> fst (cellTextPos vnlM r 0)) [0 .. 4]) [0, 1, 3, 4, 7]
  checkEq "0029: cellTextPos of a later field is offset by the earlier ones"
          (cellTextPos vnlM 0 1) (0, 2)
  -- ...and within a multi-line cell, the field after it starts on a later line.
  checkEq "0029: a field after a multi-line cell lands on that cell's last line"
          (cellTextPos vnlM 1 1) (2, 3)   -- "x\ny" serialises as "x NL y" over 2 lines
  -- Exhaustive both-ways check on the multi-line grid: every cell's position
  -- against the pre-cache definition, and every line back to its row.
  checkEq "0029: cellTextPos agrees with the pre-cache definition everywhere"
          [ (r, c) | r <- [0 .. nRows vnlM - 1]
                   , c <- [0 .. Seq.length (Seq.index (csvRows vnlM) r)]
                   , cellTextPos vnlM r c /= cellTextPosRef vnlM r c ] []
  checkEq "0029: textPosCell agrees with the scan it replaced, on and off the ends"
          [ (l, c) | l <- [-2 .. nRows vnlM + 4], c <- [-1, 0, 1, 2, 3, 7, 40]
                   , textPosCell vnlM l c /= textPosCellRef vnlM l c ] []
  -- Typing a newline into a cell and taking it out again: the O(log rows)
  -- incremental path in 'withCell', including the "back to zero drops the
  -- entry" direction that a saturating counter would pass without.
  let vnlT = commitEdit (editInsert 'z' (editInsert '\n' (beginEditFresh 'w' (setCursor 2 0 vnl0))))
  checkEq "0029: typing a newline into a cell adds its row"
          (csvNl vnlT) (M.fromList [(2, 1)])
  checkEq "0029: and the row below moves down a line"
          (fst (cellTextPos vnlT 2 0)) 2
  let vnlT2 = setCurrentCell (T.pack "plain") (setCursor 2 0 vnlT)
  checkEq "0029: removing it again empties the map (sparse, not zero-valued)"
          (csvNl vnlT2) M.empty
  checkEq "0029: two newlines in one cell count twice"
          (csvNl (setCurrentCell (T.pack "a\nb\nc") (setCursor 0 1 vnl0)))
          (M.fromList [(0, 2)])
  -- Structural edits move the indices, so they recompute rather than shift.
  checkEq "0029: inserting a row above shifts the entries"
          (csvNl (insertRowAbove (setCursor 0 0 vnlM))) (M.fromList [(2, 1), (4, 2)])
  checkEq "0029: deleting a row above shifts them back"
          (csvNl (deleteRow (setCursor 0 0 vnlM))) (M.fromList [(0, 1), (2, 2)])
  checkEq "0029: deleting the multi-line row itself drops its entry"
          (csvNl (deleteRow (setCursor 1 0 vnlM))) (M.fromList [(2, 2)])
  checkEq "0029: deleting a column drops what that column contributed"
          (csvNl (deleteCol (setCursor 0 0 vnlM))) M.empty
  -- Undo and redo restore whole grids, so the map is carried across them.
  let vnlU = Cmedit.Csv.undo (deleteRow (setCursor 1 0 vnlM))
  checkEq "0029: undo restores the map" (csvNl vnlU) (csvNl vnlM)
  checkEq "0029: redo restores it again"
          (csvNl (Cmedit.Csv.redo vnlU)) (M.fromList [(2, 2)])
  -- A sort permutes the rows, so the entries have to move with them.
  checkEq "0029: a sort moves the entries with their rows"
          (csvNl (sortByColumn 0 False False vnlM))
          (computeNl ',' (csvRows (sortByColumn 0 False False vnlM)))
  -- A paste that grows the grid changes every later index.
  checkEq "0029: a paste that grows the grid recomputes"
          (csvNl (fst (pasteClip (T.pack "X,Y\nZ,W") (setCursor 4 1 vnlM))))
          (computeNl ',' (csvRows (fst (pasteClip (T.pack "X,Y\nZ,W") (setCursor 4 1 vnlM)))))
  -- A grid handed in by a non-CSV producer (Xlsx/Odf go through mkCsvGrid).
  checkEq "0029: mkCsvGrid builds the map for a grid it was handed"
          (csvNl (mkCsvGrid ',' (Seq.fromList
                    [ Seq.fromList [T.pack "a", T.pack "b\nc"]
                    , Seq.fromList [T.pack "d", T.pack "e"] ])))
          (M.fromList [(0, 1)])
  -- A tab-delimited table: the delimiter is not a newline, so it contributes
  -- nothing, and the counts are the cells' own.
  checkEq "0029: the map is delimiter-independent for ordinary delimiters"
          (csvNl (mkCsvView '\t' (T.pack "a\tb\n\"x\ny\"\tc")))
          (M.fromList [(1, 1)])
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
  -- User column widths (header-border drag) --------------------------------
  -- An override replaces the content-fitted width, sticks across cell edits,
  -- clamps to a sane range, follows its column across inserts/deletes, and
  -- resets back to the content width.
  let vcw0 = mkCsvView ',' (T.pack "a,bb,ccc\ndddd,e,f")   -- widths [4,3,3]
      vcw1 = setColWidth 1 20 vcw0
  checkEq "csv setColWidth overrides one column" (columnWidths vcw1) [4, 20, 3]
  checkEq "csv setColWidth survives a content edit"
          (columnWidths (setCurrentCell (T.replicate 30 (T.pack "z")) (setCursor 0 1 vcw1)))
          [4, 20, 3]
  checkEq "csv setColWidth clamps narrow and wide"
          (columnWidths (setColWidth 0 1 (setColWidth 2 999 vcw0))) [2, 3, 200]
  checkEq "csv setColWidth ignores an out-of-range column"
          (columnWidths (setColWidth 7 20 vcw0)) [4, 3, 3]
  checkEq "csv resetColWidth restores the content width"
          (columnWidths (resetColWidth 1 vcw1)) [4, 3, 3]
  checkEq "csv width override follows the column past an insert"
          (columnWidths (insertColLeft vcw1)) [3, 4, 20, 3]   -- cursor col 0: insert before
  checkEq "csv width override follows the column past a delete"
          (columnWidths (deleteCol vcw1)) [20, 3]             -- delete col 0
  checkEq "csv deleting the sized column drops its override"
          (columnWidths (deleteCol (setCursor 0 1 vcw1))) [4, 3]
  -- Border hit-test / geometry shared by pointer hint and resize drag: with
  -- widths [4,20,3] and a 3-cell gutter, borders sit at 3+4=7, 7+1+20=28, 32.
  checkEq "csv csvBorderColAt finds each header border"
          (map (csvBorderColAt vcw1) [7, 28, 32]) [Just 0, Just 1, Just 2]
  checkEq "csv csvBorderColAt misses cell interiors"
          (map (csvBorderColAt vcw1) [2, 6, 8, 33]) [Nothing, Nothing, Nothing, Nothing]
  checkEq "csv csvColStartX honours the horizontal scroll"
          (map (csvColStartX vcw1 { csvLeft = 1 }) [0, 1, 2]) [Nothing, Just 3, Just 24]
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

  -- CSV row rendering with wide (2-cell) glyphs -------------------------------
  -- Regression for cali_gyms.csv row 17 (Description "\x2728The Portal\x2728…"):
  -- the CSV row draw path was string-indexed, so a wide-glyph char consumed
  -- one grid cell instead of two and every column right of that cell shifted
  -- left by one per glyph. After the fix, the column separator (U+2502) sits
  -- at the same screen column on every row, regardless of the cell's content.
  let edEmojiCsv = setLoaded "/tmp/e.csv"
                     (loadFromBytes False Nothing
                       (TE.encodeUtf8
                         (T.pack "hdr,x\n\x2728\&ab,1\ncdxx,2\n")))
                     (newEditor (24, 80) defaultConfig)
      scrEC     = renderEditor edEmojiCsv
      wEC       = scrW scrEC
      cellAtEC r c = scrCells scrEC A.! (r * wEC + c)
      sepAtEC r = [ c | c <- [0 .. wEC - 1]
                      , cellChar (cellAtEC r c) == '\x2502' ]
      -- First separator column on each row identifies where the first data
      -- column ends. Rows 3 and 4 hold "✨ab,1" and "cdxx,2" respectively.
      sep3 = sepAtEC 3
      sep4 = sepAtEC 4
  check "csv wide-glyph row has a column separator"
    (not (null sep3))
  check "csv separator column matches non-wide row"
    (not (null sep3) && not (null sep4) && head sep3 == head sep4)
  -- The wide glyph itself must be followed by a contChar continuation so the
  -- diff renderer skips one cell and the terminal's own 2-cell emoji lands
  -- exactly where cmedit's grid expects the next char.
  let sparkleCol = [ c | c <- [0 .. wEC - 1]
                       , cellChar (cellAtEC 3 c) == '\x2728' ]
  check "wide glyph followed by contChar"
    (case sparkleCol of
       (c : _) | c + 1 < wEC -> cellChar (cellAtEC 3 (c + 1)) == '\0'
       _                     -> False)
  -- Zero-width formatting controls (U+200B in cali_gyms.csv row 25) are
  -- truly invisible: no glyph, no cursor advance. Counting them as one cell
  -- would drift every column right of the cell by one — the row-25 shift.
  checkEq "cellWidth ignores ZWSP"
    (cellWidth (T.pack "\x200B\&Power Plant Fitness")) 19
  checkEq "cellWidth ignores BOM"
    (cellWidth (T.pack "\xFEFF\&hello")) 5
  -- VS16 must still count so ℹ️ measures as two cells (kept in step with the
  -- terminal folding the pair into a wide emoji).
  checkEq "cellWidth keeps VS16"
    (cellWidth (T.pack "\x2139\xFE0F")) 2
  -- Render integration: a ZWSP-prefixed cell aligns its column separator
  -- with a plain-ASCII cell in the same table.
  let edZwspCsv = setLoaded "/tmp/z.csv"
                    (loadFromBytes False Nothing
                      (TE.encodeUtf8
                        (T.pack "hdr,x\n\x200B\&Power,1\ncdxxx,2\n")))
                    (newEditor (24, 80) defaultConfig)
      scrZW   = renderEditor edZwspCsv
      wZW     = scrW scrZW
      cellAtZW r c = scrCells scrZW A.! (r * wZW + c)
      sepAtZW r = [ c | c <- [0 .. wZW - 1]
                      , cellChar (cellAtZW r c) == '\x2502' ]
  check "csv separator aligned across ZWSP row"
    (let s3 = sepAtZW 3; s4 = sepAtZW 4
     in not (null s3) && not (null s4) && head s3 == head s4)

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
      frSql = S.plainResult "/proj/pl-helper.sql"
                [S.plainMatch 12 [(27,6)] (T.pack "CREATE OR REPLACE FUNCTION helper()")] False False
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
      fr1  = S.plainResult "/proj/b.hs" [S.plainMatch 4 [(0,5)] (T.pack "hello there")] False False
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
                            [ S.plainResult "/proj/a.txt" [S.plainMatch 0 [(0,3),(8,3)] (T.pack "foo bar foo")] False False
                            , S.plainResult "/proj/closed.txt" [S.plainMatch 0 [(0,3)] (T.pack "foo")] False False ] }
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
  let manyR = [ S.plainResult ("/proj/f" ++ show i ++ ".txt") [S.plainMatch 0 [(0,3)] (T.pack "foo")] False False
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
  let bigR = [ S.plainResult ("/proj/g" ++ show i ++ ".txt") [S.plainMatch 0 [(0,3)] (T.pack "foo")] False False
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
  let (edSavedAll, _) = savedAll [("/proj/new.txt", Just (mt 100))] edStg
  check "Save All clears the staged doc's modified flag" (not (any docModified (edAfter edSavedAll)))
  checkEq "modifiedDocsToSave lists dirty titled docs"
          (map (\(p,_,_,_,_) -> p) (modifiedDocsToSave edStg)) ["/proj/new.txt"]

  -- A staged replace in a .csv opens with no table view and no stash, so
  -- Alt+T builds the table's saved baseline from the buffer — which is dirty.
  -- Adopting it as the baseline made the next recompute through 'csvMod'
  -- (a bare Ctrl+Z will do) call the document clean, drop its journal and let
  -- Ctrl+Q leave without asking: the staged change lost, silently. The table
  -- carries its own baseline, so the dirtiness has to be said twice.
  -- (Review of plan 0028; same guard as the crash-recovery installer.)
  let (edCsvStg, _) = addStagedDoc "/proj/t.csv" (mkLR2 "a,foo\nc,d") substFooBar ed0
      edCsvAct = fst (update (KAltChar '2') edCsvStg)      -- make it active
      edCsvTab = fst (update (KAltChar 't') edCsvAct)      -- plain -> table
      edCsvUz  = fst (update (KCtrlChar 'z') edCsvTab)     -- recompute via csvMod
  check "a staged .csv replace starts modified with no table view"
        (edModified edCsvAct && null (fmap (const ()) (edCsv edCsvAct)))
  check "the table view of a staged replace is itself modified"
        (fmap isModified (edCsv edCsvTab) == Just True)
  check "a staged .csv replace stays modified across a no-op undo"
        (edModified edCsvUz)
  checkEq "a staged .csv replace keeps its journal across a no-op undo"
          (journalLiveKeys edCsvUz) (journalLiveKeys edCsvTab)
  -- The guard must not latch: a clean .csv toggled into the table is clean.
  let edCleanCsv = setLoaded "/proj/u.csv"
                     (LoadResult (fromText (T.pack "a,b\nc,d")) LF Utf8 True False Nothing) ed0
      edCleanTab = fst (update (KCtrlChar 'z') edCleanCsv)
  check "a clean .csv in the table view is not modified"
        (not (edModified edCleanTab) && fmap isModified (edCsv edCleanTab) == Just False)

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

  -- Writing the config back --------------------------------------------------
  do
    let roundtrips desired src =
          fst (parseConfigText (updateConfigText desired (T.pack src)) defaultConfig)
            == desired
        want = defaultConfig { cfgTabWidth = 8, cfgWordWrap = True
                             , cfgTabsToSpaces = True, cfgTheme = ThemeLight
                             , cfgTrimTrailingWs = True }
    -- Round-trip: update then parse gives the desired config, for several inputs.
    check "config write roundtrip on empty" (roundtrips want "")
    check "config write roundtrip on pristine defaults"
      (roundtrips want "# my settings\ntab-width = 4\n")
    check "config write roundtrip all defaults keeps empty"
      (roundtrips defaultConfig "")
    -- A pristine (all-default) target must not spam a pristine file with keys.
    checkEq "config write appends nothing for defaults"
      (updateConfigText defaultConfig (T.pack "# header\n")) (T.pack "# header\n")
    -- Comment preservation: header, an inline comment, an unknown key and a
    -- malformed line all survive; only the value of a present key changes.
    let src = "# header comment\nword-wrap = off # keep wrapped\nbogus = 1\nnonsense\ntheme = dark\n"
        out = T.unpack (updateConfigText want (T.pack src))
    check "config write preserves header" ("# header comment" `isInfixOf` out)
    check "config write preserves inline comment" ("# keep wrapped" `isInfixOf` out)
    check "config write rewrites value in place" ("word-wrap = on # keep wrapped" `isInfixOf` out)
    check "config write preserves unknown key" ("bogus = 1" `isInfixOf` out)
    check "config write preserves malformed line" ("nonsense" `isInfixOf` out)
    check "config write rewrites present theme" ("theme = light" `isInfixOf` out)
    check "config write roundtrips through comments" (roundtrips want src)
    -- Duplicate keys: a later line wins in the parser, so every occurrence must
    -- be updated (otherwise the stale last line would override).
    let dup = "word-wrap = off\nword-wrap = off\n"
        dupOut = updateConfigText (defaultConfig { cfgWordWrap = True }) (T.pack dup)
    checkEq "config write updates duplicate keys"
      (cfgWordWrap (fst (parseConfigText dupOut defaultConfig))) True
    check "config write updated both duplicates"
      (length (filter (isInfixOf "word-wrap = on") (lines (T.unpack dupOut))) == 2)
    -- Append only non-default missing keys; separated by a blank line.
    let appended = T.unpack (updateConfigText
                     (defaultConfig { cfgWordWrap = True }) (T.pack "# a config\n"))
    check "config write appends the changed key" ("word-wrap = on" `isInfixOf` appended)
    check "config write does not append unchanged keys"
      (not ("tab-width" `isInfixOf` appended))
    check "config write keeps original content when appending"
      ("# a config" `isInfixOf` appended)

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
    checkEq "goto pushed one stop" (map nsPos (toList (edNavBack edGoto))) [Pos 0 0]
    let (edBack, _) = update altL edGoto
    checkEq "Alt+Left returns to origin" (edCursor edBack) (Pos 0 0)
    checkEq "forward trail recorded" (map nsPos (toList (edNavFwd edBack))) [Pos 149 0]
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
                   , edNavBack = Seq.fromList [NavStop (Just "/w/adir/deep/a.hs") (Pos 1 0)] }
        edRw = renamePaths "/w/adir" "/w/bdir" edDocs
    checkEq "rename rewrites active path" (edPath edRw) (Just "/w/bdir/deep/a.hs")
    checkEq "rename rewrites recents"
      (map rePath (edRecent edRw)) ["/w/bdir/deep/a.hs", "/w/other"]
    checkEq "rename rewrites nav stops"
      (map nsPath (toList (edNavBack edRw))) [Just "/w/bdir/deep/a.hs"]

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
      (toList (edFindHist ed2)) (map T.pack ["beta", "alpha"])
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
    -- CSV header freeze: on by default, config key turns it off.
    check "freeze-header defaults on" (cfgFreezeHeader defaultConfig)
    checkEq "config freeze-header = off"
            (cfgFreezeHeader (fst (parseConfigText (T.pack "freeze-header = off") defaultConfig)))
            False
    check "newEditor seeds edFreezeHeader from the config"
      (edFreezeHeader (newEditor (24, 80) defaultConfig)
       && not (edFreezeHeader (newEditor (24, 80) defaultConfig { cfgFreezeHeader = False })))
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
    -- View ▸ Theme opens a picker dialog: buttons for every theme, focus
    -- starting on the current one, live preview while the focus moves, and
    -- nothing committed until Enter.
    let edDlg = fst (update KEnter edT { edFocus = FMenu
                                       , edMenu = menuStateFor edT viewIx MAToggleTheme })
    check "menu opens the theme picker"
      (maybe False ((== DKTheme) . dlgKind) (edDialog edDlg))
    checkEq "picker focus starts on the current theme (auto)"
            (fmap dlgFocus (edDialog edDlg)) (Just (themeIndex ThemeAuto))
    let edPrev = key (KArrow DRight noMods)
                     (key (KArrow DRight noMods) edDlg)   -- Auto -> Dark -> Light
    check "moving focus previews without committing"
      (resolvedTheme edPrev == ThemeLight
       && cfgTheme (edConfig edPrev) == ThemeAuto)
    let edPick = key KEnter edPrev
    checkEq "Enter applies the focused theme" (cfgTheme (edConfig edPick)) ThemeLight
    check "picker closes after choosing" (edDialog edPick == Nothing)
    check "menu label shows the value"
      (any (\case MEItem lbl _ MAToggleTheme -> T.pack "light" `T.isInfixOf` lbl; _ -> False)
           (entriesFor edPick viewIx))
    let edEsc = key KEsc edPrev
    check "Esc drops the preview and keeps the old theme"
      (cfgTheme (edConfig edEsc) == ThemeAuto && resolvedTheme edEsc == ThemeDark
       && edDialog edEsc == Nothing)
    checkEq "the Cancel button keeps the old theme too"
            (cfgTheme (edConfig (key KEnter
              (edDlg { edDialog = fmap (\d -> d { dlgFocus = length themeChoices })
                                       (edDialog edDlg) }))))
            ThemeAuto
    checkEq "picking Cherry Blossom applies it"
            (cfgTheme (edConfig (key KEnter
              (edDlg { edDialog = fmap (\d -> d { dlgFocus = 3 }) (edDialog edDlg) }))))
            ThemeCherryBlossom

    -- Cherry blossom: config spellings parse, and the rendered frame carries
    -- an explicit RGB background on every cell (nothing falls through to the
    -- terminal's own colours).
    checkEq "config theme=cherry-blossom"
            (cfgTheme (fst (parseConfigText (T.pack "theme = cherry-blossom") defaultConfig)))
            ThemeCherryBlossom
    checkEq "config theme=cherry shorthand"
            (cfgTheme (fst (parseConfigText (T.pack "theme = cherry") defaultConfig)))
            ThemeCherryBlossom
    let scrC = renderEditor edT { edConfig = (edConfig edT) { cfgTheme = ThemeCherryBlossom } }
        isRGB c = case c of ColorRGB{} -> True; _ -> False
    check "cherry blossom forces an RGB background on every cell"
      (all (isRGB . styleBg . cellStyle) (A.elems (scrCells scrC)))
    check "cherry blossom keyword colour differs from light theme"
      (thTokens (themeFor ThemeCherryBlossom) TkKeyword /= thTokens lightTheme TkKeyword)

    -- Flashbang & Midnight: the other two forced-background themes. Config
    -- words parse (including the renamed terminal themes and their legacy
    -- spellings), every theme's label round-trips through the parser, and
    -- both palettes carry an explicit RGB background on every cell.
    checkEq "config theme=flashbang"
            (cfgTheme (fst (parseConfigText (T.pack "theme = flashbang") defaultConfig)))
            ThemeFlashbang
    checkEq "config theme=midnight"
            (cfgTheme (fst (parseConfigText (T.pack "theme = midnight") defaultConfig)))
            ThemeMidnight
    checkEq "config theme=dark-terminal"
            (cfgTheme (fst (parseConfigText (T.pack "theme = dark-terminal") defaultConfig)))
            ThemeDark
    checkEq "legacy theme=dark still parses"
            (cfgTheme (fst (parseConfigText (T.pack "theme = dark") defaultConfig)))
            ThemeDark
    check "every theme label parses back to its theme"
      (and [ cfgTheme (fst (parseConfigText (T.pack ("theme = " ++ themeLabel t)) defaultConfig)) == t
           | t <- themeChoices ])
    let scrF = renderEditor edT { edConfig = (edConfig edT) { cfgTheme = ThemeFlashbang } }
        scrM = renderEditor edT { edConfig = (edConfig edT) { cfgTheme = ThemeMidnight } }
    check "flashbang forces an RGB background on every cell"
      (all (isRGB . styleBg . cellStyle) (A.elems (scrCells scrF)))
    check "midnight forces an RGB background on every cell"
      (all (isRGB . styleBg . cellStyle) (A.elems (scrCells scrM)))
    check "flashbang and midnight keyword colours differ"
      (thTokens (themeFor ThemeFlashbang) TkKeyword /= thTokens (themeFor ThemeMidnight) TkKeyword)
    checkEq "picking Flashbang applies it"
            (cfgTheme (edConfig (key KEnter
              (edDlg { edDialog = fmap (\d -> d { dlgFocus = 4 }) (edDialog edDlg) }))))
            ThemeFlashbang
    checkEq "picking Midnight applies it"
            (cfgTheme (edConfig (key KEnter
              (edDlg { edDialog = fmap (\d -> d { dlgFocus = 5 }) (edDialog edDlg) }))))
            ThemeMidnight
    checkEq "picking Graphite applies it"
            (cfgTheme (edConfig (key KEnter
              (edDlg { edDialog = fmap (\d -> d { dlgFocus = 6 }) (edDialog edDlg) }))))
            ThemeGraphite

    -- Graphite: the neutral-grey forced-background theme. Its config word
    -- parses, it paints every cell, its palette is its own, and — the detail
    -- that makes it read like an IDE rather than a grey Midnight — its
    -- chrome sits *below* the page rather than above it.
    checkEq "config theme=graphite"
            (cfgTheme (fst (parseConfigText (T.pack "theme = graphite") defaultConfig)))
            ThemeGraphite
    let scrG = renderEditor edT { edConfig = (edConfig edT) { cfgTheme = ThemeGraphite } }
    check "graphite forces an RGB background on every cell"
      (all (isRGB . styleBg . cellStyle) (A.elems (scrCells scrG)))
    check "graphite and midnight keyword colours differ"
      (thTokens (themeFor ThemeGraphite) TkKeyword /= thTokens (themeFor ThemeMidnight) TkKeyword)
    let lum c = case c of ColorRGB r g b -> Just (fromIntegral r * 0.3
                                                  + fromIntegral g * 0.59
                                                  + fromIntegral b * 0.11 :: Double)
                          _ -> Nothing
        thG = themeFor ThemeGraphite
    check "graphite's bars sit below its page"
      (case (lum (styleBg (thMenuBar thG)), lum (styleBg (thText thG))) of
         (Just bar, Just page) -> bar < page
         _ -> False)
    check "graphite's page is neutral (no colour cast)"
      (case styleBg (thText thG) of
         ColorRGB r g b -> maximum [r, g, b] - minimum [r, g, b] <= 4
         _ -> False)
    -- The eight-button picker no longer fits one 80-column row: its buttons
    -- wrap ('buttonRows'), every button lands on exactly one row, and the
    -- box still fits the terminal.
    checkEq "theme picker buttons wrap to two rows"
            (length [ () | DRButtons _ <- dialogRows (mkTheme 0) ]) 2
    check "a blank line separates the wrapped button rows"
      (case [ i | (i, DRButtons _) <- zip [0 :: Int ..] (dialogRows (mkTheme 0)) ] of
         [a, b] -> b == a + 2
                   && (case dialogRows (mkTheme 0) !! (a + 1) of DRBlank -> True; _ -> False)
         _      -> False)
    checkEq "every picker button lands on exactly one row"
            (concatMap (map fst) (buttonRows (mkTheme 0)))
            [0 .. length (dlgButtons (mkTheme 0)) - 1]
    let (_, _, _, wTheme) = dialogGeom edT (mkTheme 0) (computeLayout edT)
    check "theme picker fits an 80-column terminal" (wTheme <= 78)

  -- Settings dialog (File ▸ Settings…) ---------------------------------------
  do
    let key k e = fst (update k e)
        edP = setLoaded "/tmp/s.txt" (mkLR "hello world") (newEditor (24, 80) defaultConfig)
        focusRow k e = e { edDialog = fmap (\d -> d { dlgFocus = k }) (edDialog e) }
        edS = fst (update KEnter edP { edFocus = FMenu
                                     , edMenu = menuStateFor edP 0 MASettings })
    check "settingsSpec: every default index is in range"
      (and [ let ix = ixOf defaultConfig in ix >= 0 && ix < length vals
           | (_, _, vals, ixOf, _) <- settingsSpec ])
    check "settings opens from the File menu"
      (maybe False ((== DKSettings) . dlgKind) (edDialog edS))
    check "Ctrl+, opens settings"
      (maybe False ((== DKSettings) . dlgKind) (edDialog (key (KCtrlChar ',') edP)))
    check "settings has a row per spec entry and Save/Cancel"
      (maybe False (\d -> length (dlgChoices d) == length settingsSpec
                          && dlgButtons d == ["Save", "Cancel"]) (edDialog edS))
    -- Row 5 = Line numbers: cycling applies live, behind the dialog.
    let edLn = key (KArrow DRight noMods) (focusRow 5 edS)
    check "cycling a row applies live (line numbers)"
      (edShowLineNumbers edLn && cfgLineNumbers (edConfig edLn)
       && not (edShowLineNumbers edS))
    -- Row 3 = Theme: applies live too (no separate preview machinery needed).
    checkEq "theme row applies live"
            (cfgTheme (edConfig (key (KArrow DRight noMods) (focusRow 3 edS))))
            ThemeDark
    -- Esc reverts every live change to the state captured at open.
    let edEsc = key KEsc edLn
    check "Esc reverts live changes and closes"
      (not (edShowLineNumbers edEsc) && not (cfgLineNumbers (edConfig edEsc))
       && edDialog edEsc == Nothing && edSettingsStash edEsc == Nothing)
    -- The Cancel button does the same.
    let edCanc = key KEnter (focusRow (length settingsSpec + 1) edLn)
    check "Cancel button reverts too"
      (not (edShowLineNumbers edCanc) && edDialog edCanc == Nothing)
    -- Save persists the on-screen values via EffSaveConfig.
    let (edSaved, effsS) = update KEnter (focusRow (length settingsSpec) edLn)
    check "Save emits EffSaveConfig carrying the live config"
      (case effsS of [EffSaveConfig c] -> cfgLineNumbers c; _ -> False)
    check "Save closes, keeps the setting, drops the stash"
      (edDialog edSaved == Nothing && edShowLineNumbers edSaved
       && edSettingsStash edSaved == Nothing)
    -- Rows show session-effective values: wrap toggled via Alt+Z reads as on.
    let edW = key (KCtrlChar ',') (key (KAltChar 'z') edP)
    check "rows show session-effective values (word wrap)"
      (maybe False (\d -> chIx (dlgChoices d !! 4) == 1) (edDialog edW))
    -- …and Save from that state persists the session value.
    let (_, effsW) = update KEnter (focusRow (length settingsSpec) edW)
    check "Save persists session-effective values"
      (case effsW of [EffSaveConfig c] -> cfgWordWrap c; _ -> False)

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
        -- The text area is 21 rows tall minus the reserved horizontal-scrollbar
        -- row = 20 (see 'computeLayout').
        checkEq "bar spans the text area" (top, h) (1, 20)
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
        lastCol = [ cellChar (scrCells scrB A.! (r * scrW scrB + 79)) | r <- [1 .. 20] ]
    check "track+thumb drawn" ('\x2588' `elem` lastCol && '\x2502' `elem` lastCol)
    -- CSV: the bar tracks the table and a click moves the cell cursor.
    let csvBig = T.unlines (T.pack "h1,h2" : [ T.pack (show i ++ "," ++ show i) | i <- [1 .. 200 :: Int] ])
        edCsvB = setLoaded "/tmp/big.csv" (loadFromBytes False Nothing (TE.encodeUtf8 csvBig))
                   (newEditor (24, 80) defaultConfig)
    check "csv overflow shows a bar" (scrollBarInfo edCsvB /= Nothing)
    let edCsvJ = key (press 20) edCsvB
    check "csv click jumps rows" (maybe 0 csvCurRow (edCsv edCsvJ) > 100)

  -- Horizontal scrollbar -----------------------------------------------------------------
  do
    let key k e = fst (update k e)

    -- Csv.hScrollTo: an independent reference over every offset from -5 up to
    -- past the total width, including the documented "past the end" fallback
    -- (target >= totalWidth resets csvXOff to 0 rather than clamping to the
    -- last valid offset; real callers never reach it because scrollTrackTarget
    -- always hands hScrollTo a value < totalWidth).
    let vHT = setColWidth 1 20 (mkCsvView ',' (T.pack "a,bb,ccc\ndddd,e,f")) -- widths [4,20,3]
        effsHT = columnWidths vHT
        nHT = length effsHT
        startsHT = scanl (+) 0 (map (+ 1) effsHT)               -- [0, 5, 26]
        totalHT = last startsHT                                  -- 30 (scanl's final running sum)
        refHT x =
          let clamped = max 0 x
          in case [ c | c <- [0 .. nHT - 1]
                      , clamped >= startsHT !! c, clamped <= startsHT !! c + effsHT !! c ] of
               (c : _) -> (c, clamped - startsHT !! c)
               []      -> (nHT - 1, 0)                          -- past the end
        htGot x = let v' = hScrollTo x vHT in (csvLeft v', csvXOff v')
    check "csv hScrollTo matches an independent reference across every offset"
      (all (\x -> htGot x == refHT x) [-5 .. totalHT + 3])
    checkEq "csv hScrollTo X=0 is the origin" (htGot 0) (0, 0)
    checkEq "csv hScrollTo negative clamps to 0" (htGot (-40)) (0, 0)
    check "csv hScrollTo keeps the invariant 0 <= xoff <= effWidth(csvLeft)"
      (all (\x -> let (l, off) = htGot x in off >= 0 && off <= effsHT !! l) [-5 .. totalHT + 3])
    check "csv hScrollTo inverse relation: prefix-sum(widths+1)+xoff == X up to the last valid offset"
      (all (\x -> let v' = hScrollTo x vHT
                      effs = columnWidths v'
                  in sum (map (+ 1) (take (csvLeft v') effs)) + csvXOff v' == x)
           [0 .. totalHT - 1])

    -- Csv.ensureVisible keep-or-snap: a scrollbar-produced (csvLeft, csvXOff)
    -- survives when the cursor cell is already fully visible; otherwise it
    -- snaps csvXOff back to 0 and recomputes csvLeft the way 'scrollLeft'
    -- would (reference copied from the "csv scrollLeft matches the reference"
    -- test above, since 'scrollLeft' itself isn't exported).
    let scrollLeftRef2 width v =
          let ws = columnWidths v
              cc = csvCurCol v
              fits l = sum [ ws !! c + 1 | c <- [l .. cc], c < length ws ] <= width
              go l | l >= cc = cc
                   | fits l = l
                   | otherwise = go (l + 1)
          in go (max 0 (min (csvLeft v) cc))
        vKS0 = mkCsvView ',' (T.pack "a,bb,ccc\ndddd,e,f")        -- widths [4,3,3]
        vKeep = (setCursor 1 2 vKS0) { csvLeft = 1, csvXOff = 3 } -- col2 (w=3) fully visible in width 6
        vSnap = (setCursor 1 2 vKS0) { csvLeft = 0, csvXOff = 5 } -- col2 NOT fully visible in width 6
        vKeep' = Cmedit.Csv.ensureVisible 5 0 6 vKeep
        vSnap' = Cmedit.Csv.ensureVisible 5 0 6 vSnap
    checkEq "ensureVisible keeps a scrollbar sub-column offset when the cell is fully visible"
      (csvLeft vKeep', csvXOff vKeep') (1, 3)
    checkEq "ensureVisible snaps to (scrollLeft, 0) when the cell is not fully visible"
      (csvLeft vSnap', csvXOff vSnap') (scrollLeftRef2 6 vSnap, 0)

    -- hScrollBarInfo: plain text view. A tab-filled line keeps the char count
    -- low while the display width (colToDisplay) overflows, so the test only
    -- passes if hScrollBarInfo measures display cells, not characters.
    let tabw = cfgTabWidth defaultConfig
        tabLine = T.replicate 30 (T.pack "\t")                  -- display width 30*4 = 120
        edShort = (newEditor (24, 80) defaultConfig)
                    { edBuffer = fromText (T.pack "hi\nbye") }
        edOverflow = (newEditor (24, 80) defaultConfig) { edBuffer = fromText tabLine }
        edLefty = edShort { edLeft = 5 }
    check "no overflow, no scroll -> Nothing" (hScrollBarInfo edShort == Nothing)
    case hScrollBarInfo edOverflow of
      Nothing -> check "overflowing tab line shows a bar" False
      Just (row, x0, trackWidth, total, offset) -> do
        let loOv = computeLayout edOverflow
        checkEq "hbar row is loHBarRow" (Just row) (loHBarRow loOv)
        checkEq "hbar x0 is loTextLeft" x0 (loTextLeft loOv)
        checkEq "hbar spans to the vertical bar" trackWidth (loCols loOv - loVBarW loOv - x0)
        check "hbar total reflects colToDisplay, not char count"
          (total == colToDisplay tabw (T.length tabLine) tabLine && total > T.length tabLine)
        checkEq "hbar offset is edLeft" offset 0
    check "edLeft > 0 with short lines still shows a bar" (hScrollBarInfo edLefty /= Nothing)
    case hScrollBarInfo edLefty of
      Just (_, _, _, _, offset) -> checkEq "hbar offset with edLeft > 0" offset 5
      Nothing -> check "edLeft>0 bar present" False
    -- Disabling the vertical bar reclaims its column for the horizontal track.
    let edOverflowNoV = edOverflow { edConfig = (edConfig edOverflow) { cfgScrollBarV = False } }
    case (hScrollBarInfo edOverflow, hScrollBarInfo edOverflowNoV) of
      (Just (_, _, twOn, _, _), Just (_, _, twOff, _, _)) ->
        checkEq "hbar track extends one column when the vertical bar is off" twOff (twOn + 1)
      _ -> check "hbar present in both V-bar configurations" False

    -- hScrollBarInfo: CSV view. total is the sum of effWidth+1 (including the
    -- user override); after a scrollbar-style hScrollTo the offset carries
    -- through unchanged.
    let vCsvHT = hScrollTo 10 vHT
        edCsvHT = (newEditor (24, 80) defaultConfig) { edCsv = Just vCsvHT }
        loCsvHT = computeLayout edCsvHT
        gutHT = csvGutterWidthFor vCsvHT
    case hScrollBarInfo edCsvHT of
      Nothing -> check "csv hbar present after a nonzero xoff" False
      Just (row, x0, trackWidth, total, offset) -> do
        checkEq "csv hbar row is loHBarRow" (Just row) (loHBarRow loCsvHT)
        checkEq "csv hbar x0 is loContentLeft+gutter" x0 (loContentLeft loCsvHT + gutHT)
        checkEq "csv hbar trackWidth stops short of the vertical bar"
          trackWidth (loCols loCsvHT - loVBarW loCsvHT - x0)
        checkEq "csv hbar total is the sum of effWidth+1, including the override"
          total (sum (map (+ 1) (columnWidths vCsvHT)))
        checkEq "csv hbar offset carries the hScrollTo target through" offset 10

    -- Mouse end-to-end (text): a press on the bar jumps + starts a drag, a
    -- drag moves it again, and a release ends the drag without moving further.
    -- 'scrollTrackTarget' isn't exported (like the vertical bar's twin), so
    -- its trivial formula is reproduced here to predict the target exactly,
    -- the same way 'scrollLeftRef'/'scrollLeftRef2' stand in for 'scrollLeft'.
    let scrollTrackTargetRef h total rel =
          let thumbLen = max 1 (min h (h * h `div` max 1 total))
              denom = max 1 (h - thumbLen)
              maxTop = max 0 (total - h)
          in max 0 (min maxTop (rel * maxTop `div` denom))
        wideLine = T.pack (replicate 300 'x')
        edWide = (newEditor (24, 80) defaultConfig) { edBuffer = fromText wideLine }
    case hScrollBarInfo edWide of
      Nothing -> check "wide text line shows an hbar" False
      Just (hbarRow, hx0, htrack, htotal, _) -> do
        let pressH c = KMouse (MouseEvent MBLeft c hbarRow True False noMods 1)
            dragH c = KMouse (MouseEvent MBLeft c hbarRow True True noMods 1)
            releaseH c = KMouse (MouseEvent MBLeft c hbarRow False False noMods 1)
            rel1 = max 0 (min (htrack - 1) 40)
            rel2 = max 0 (min (htrack - 1) 60)
            expect1 = scrollTrackTargetRef htrack htotal rel1
            expect2 = scrollTrackTargetRef htrack htotal rel2
            edWP = key (pressH (hx0 + 40)) edWide
        checkEq "hbar press jumps edLeft to the track target" (edLeft edWP) expect1
        check "hbar press starts a drag" (edHScrollDrag edWP)
        let edWD = key (dragH (hx0 + 60)) edWP
        checkEq "hbar drag moves edLeft again" (edLeft edWD) expect2
        check "hbar drag actually changed the offset" (expect2 /= expect1)
        let edWR = key (releaseH (hx0 + 60)) edWD
        check "hbar release ends the drag" (not (edHScrollDrag edWR))
        checkEq "hbar release doesn't move the scroll further" (edLeft edWR) expect2
        -- The rendered row shows the track/thumb glyphs, like the vertical bar.
        let scrHT = renderEditor edWide
            hbarCells = [ cellChar (scrCells scrHT A.! (hbarRow * scrW scrHT + c))
                        | c <- [0 .. scrW scrHT - 1] ]
        check "hbar track+thumb drawn" ('\x2588' `elem` hbarCells && '\x2500' `elem` hbarCells)

    -- Mouse end-to-end (CSV): the same press/drag/release, cross-checked
    -- against calling Csv.hScrollTo directly on the pre-drag view, confirming
    -- the CSV path bypasses ensureVisible/csvPut just as documented.
    let mkWide ch = T.replicate 34 (T.pack [ch])
        wideCsvTxt = T.intercalate (T.pack ",") [mkWide 'A', mkWide 'B', mkWide 'C']
                       `T.append` T.pack "\n1,2,3"
        edCsvW = setLoaded "/tmp/wide.csv" (loadFromBytes False Nothing (TE.encodeUtf8 wideCsvTxt))
                   (newEditor (24, 80) defaultConfig)
    case edCsv edCsvW of
      Nothing -> check "wide csv loads as a table" False
      Just vBefore -> case hScrollBarInfo edCsvW of
        Nothing -> check "wide csv triggers horizontal overflow" False
        Just (hbarRowC, hx0C, htrackC, htotalC, _) -> do
          let pressC c = KMouse (MouseEvent MBLeft c hbarRowC True False noMods 1)
              dragC c = KMouse (MouseEvent MBLeft c hbarRowC True True noMods 1)
              releaseC c = KMouse (MouseEvent MBLeft c hbarRowC False False noMods 1)
              rel1 = max 0 (min (htrackC - 1) 5)
              rel2 = max 0 (min (htrackC - 1) 60)
              expect1 = scrollTrackTargetRef htrackC htotalC rel1
              expect2 = scrollTrackTargetRef htrackC htotalC rel2
              vAfterPure1 = hScrollTo expect1 vBefore
              edCP = key (pressC (hx0C + 5)) edCsvW
          check "csv hbar press starts a drag" (edHScrollDrag edCP)
          checkEq "csv hbar press matches Csv.hScrollTo applied directly"
            (maybe (-1, -1) (\v -> (csvLeft v, csvXOff v)) (edCsv edCP))
            (csvLeft vAfterPure1, csvXOff vAfterPure1)
          check "csv hbar press produces a nonzero csvXOff"
            (maybe False ((> 0) . csvXOff) (edCsv edCP))
          case edCsv edCP of
            Nothing -> check "csv view still active after a press" False
            Just vAfter1 ->
              checkEq "csvColStartX for the scrolled-to column reflects the new xoff"
                (csvColStartX vAfter1 (csvLeft vAfter1))
                (Just (csvGutterWidthFor vAfter1 - csvXOff vAfter1))
          let edCD = key (dragC (hx0C + 60)) edCP
              vAfterPure2 = hScrollTo expect2 vBefore
          checkEq "csv hbar drag matches Csv.hScrollTo applied directly"
            (maybe (-1, -1) (\v -> (csvLeft v, csvXOff v)) (edCsv edCD))
            (csvLeft vAfterPure2, csvXOff vAfterPure2)
          let edCR = key (releaseC (hx0C + 60)) edCD
          check "csv hbar release ends the drag" (not (edHScrollDrag edCR))

    -- Layout: cfgScrollBarH off drops the reserved row (its space returns to
    -- the text area); word wrap does the same in the text view; a CSV view
    -- keeps the bar even under word wrap.
    let edPlainH = newEditor (24, 80) defaultConfig
        edNoH = edPlainH { edConfig = (edConfig edPlainH) { cfgScrollBarH = False } }
        loOn = computeLayout edPlainH
        loOff = computeLayout edNoH
    check "hbar on by default for a plain text view" (loHBarRow loOn /= Nothing)
    checkEq "cfgScrollBarH off drops the reserved row" (loHBarRow loOff) Nothing
    checkEq "the row returns to the text area" (loTextHeight loOff) (loTextHeight loOn + 1)
    checkEq "word wrap also drops the horizontal bar"
      (loHBarRow (computeLayout (edPlainH { edWordWrap = True }))) Nothing
    let edCsvMode = edPlainH { edCsv = Just (mkCsvView ',' (T.pack "a,b\n1,2")) }
    check "CSV view keeps the horizontal bar even under word wrap"
      (loHBarRow (computeLayout (edCsvMode { edWordWrap = True })) /= Nothing)

    -- cfgScrollBarV off reclaims its column.
    let edVOff = edPlainH { edConfig = (edConfig edPlainH) { cfgScrollBarV = False } }
    checkEq "cfgScrollBarV off -> loVBarW 0" (loVBarW (computeLayout edVOff)) 0
    check "cfgScrollBarV off -> scrollBarInfo Nothing"
      (scrollBarInfo (edVOff { edBuffer = fromText (T.unlines [ T.pack (show i) | i <- [1 .. 300 :: Int] ]) })
        == Nothing)
    checkEq "cfgScrollBarV off widens the text area by one column"
      (loTextWidth (computeLayout edVOff)) (loTextWidth (computeLayout edPlainH) + 1)

    -- applySettingRow rows 7 (Vertical scroll bar) / 8 (Horizontal scroll bar)
    -- apply live, the same way every other settings row does.
    check "applySettingRow 7 toggles cfgScrollBarV live"
      (not (cfgScrollBarV (edConfig (applySettingRow 7 0 edPlainH))))
    check "applySettingRow 7 back on restores it"
      (cfgScrollBarV (edConfig (applySettingRow 7 1 edPlainH)))
    check "applySettingRow 8 toggles cfgScrollBarH live"
      (not (cfgScrollBarH (edConfig (applySettingRow 8 0 edPlainH))))
    check "computeLayout reflects a row-7 toggle immediately"
      (loVBarW (computeLayout (applySettingRow 7 0 edPlainH)) == 0)
    check "computeLayout reflects a row-8 toggle immediately"
      (loHBarRow (computeLayout (applySettingRow 8 0 edPlainH)) == Nothing)

    -- openSettings -> toggle rows 7 & 8 -> Cancel restores the original
    -- config (mirrors the existing "Esc reverts live changes" settings test).
    let edSP = setLoaded "/tmp/hb.txt" (loadFromBytes False Nothing (TE.encodeUtf8 (T.pack "hello")))
                 (newEditor (24, 80) defaultConfig)
        edSSettings = key KEnter edSP { edFocus = FMenu, edMenu = menuStateFor edSP 0 MASettings }
        focusRowH k e = e { edDialog = fmap (\d -> d { dlgFocus = k }) (edDialog e) }
        edSV = key (KArrow DRight noMods) (focusRowH 7 edSSettings)
        edSVH = key (KArrow DRight noMods) (focusRowH 8 edSV)
    check "toggling settings rows 7/8 applies live"
      (not (cfgScrollBarV (edConfig edSV)) && not (cfgScrollBarH (edConfig edSVH)))
    let edSEsc = key KEsc edSVH
    check "Esc restores the original scrollbar config"
      (cfgScrollBarV (edConfig edSEsc) && cfgScrollBarH (edConfig edSEsc)
       && edDialog edSEsc == Nothing)

    -- Config file: the scrollbar keys parse and round-trip like any other key.
    let (cSB, wSB) = parseConfigText
                       (T.pack "scrollbar-vertical = off\nscrollbar-horizontal = off\n") defaultConfig
    check "scrollbar-vertical = off parses" (not (cfgScrollBarV cSB))
    check "scrollbar-horizontal = off parses" (not (cfgScrollBarH cSB))
    checkEq "scrollbar config keys parse without warnings" wSB []
    let wantSB = defaultConfig { cfgScrollBarV = False, cfgScrollBarH = False }
        outSB = updateConfigText wantSB (T.pack "")
    check "scrollbar config round-trips through write+parse"
      (fst (parseConfigText outSB defaultConfig) == wantSB)

  -- CSV column sort ---------------------------------------------------------------------
  do
    let key k e = fst (update k e)
        mkCsv txt = setLoaded "/tmp/s.csv" (loadFromBytes False Nothing (TE.encodeUtf8 (T.pack txt)))
                      (newEditor (24, 80) defaultConfig)
        col0 e = maybe [] (\v -> [ cellAt r 0 v | r <- [0 .. nRows v - 1] ]) (edCsv e)
        col1 e = maybe [] (\v -> [ cellAt r 1 v | r <- [0 .. nRows v - 1] ]) (edCsv e)
        edS0 = mkCsv "name,n\nbravo,2\nalpha,10\ncharlie,9"
        -- move to column n (col 1); the header row is pinned by default now
        -- (config key freeze-header, on unless turned off)
        edS = fst (update (KArrow DRight noMods) edS0)
    check "header row is frozen by default" (edFreezeHeader edS0)

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

    -- Typed-column sorts: money/percent/thousands-numeric/date/time all sort
    -- by their true value rather than alphabetically, and blanks sink to the
    -- bottom. Mixed formats or a stray non-matching cell fall back to alpha.
    let sortCol0 s = col0 (key (KAltChar 's')
                              ((mkCsv s) { edFreezeHeader = True }))
    -- Values that contain the CSV field separator must be quoted so they
    -- parse as a single cell.
    checkEq "thousands-grouped numeric sorts numerically"
      (sortCol0 "h\n\"1,000\"\n500\n\"2,500\"\n50")
      (map T.pack ["h", "50", "500", "1,000", "2,500"])
    checkEq "money column sorts by value, blanks last"
      (sortCol0 "h\n$100\n\"$1,000\"\n\n$50")
      (map T.pack ["h", "$50", "$100", "$1,000", ""])
    checkEq "percent column sorts by value"
      (sortCol0 "h\n50%\n5%\n12.5%")
      (map T.pack ["h", "5%", "12.5%", "50%"])
    checkEq "ISO date column sorts chronologically"
      (sortCol0 "h\n2024-01-15\n2023-12-01\n2024-02-01")
      (map T.pack ["h", "2023-12-01", "2024-01-15", "2024-02-01"])
    checkEq "DD/MM/YYYY column sorts chronologically"
      (sortCol0 "h\n15/01/2024\n01/12/2023\n01/02/2024")
      (map T.pack ["h", "01/12/2023", "15/01/2024", "01/02/2024"])
    -- Column disambiguates as MDY (13/xx wouldn't parse as DMY, so DMY fails
    -- across the column and MDY wins).
    checkEq "MM/DD/YYYY column sorts chronologically"
      (sortCol0 "h\n01/13/2024\n12/01/2023\n02/01/2024")
      (map T.pack ["h", "12/01/2023", "01/13/2024", "02/01/2024"])
    checkEq "time column sorts by clock, 12h supported"
      (sortCol0 "h\n2:30 PM\n9:00 AM\n11:59 PM")
      (map T.pack ["h", "9:00 AM", "2:30 PM", "11:59 PM"])
    checkEq "ISO timestamp column sorts chronologically"
      (sortCol0 "h\n2024-01-15T10:00:00\n2024-01-15T09:59:59\n2024-01-14T23:00:00")
      (map T.pack ["h", "2024-01-14T23:00:00", "2024-01-15T09:59:59"
                  , "2024-01-15T10:00:00"])
    -- One rogue cell disqualifies the type and we fall back to alpha, but
    -- the sort never errors. Start unsorted so Alt+S ascends.
    checkEq "mixed-format column falls back to alpha"
      (sortCol0 "h\nnot a date\n2024-02-01\n2024-01-15")
      (map T.pack ["h", "2024-01-15", "2024-02-01", "not a date"])
    -- The toggle survives typed detection: sorting a date column twice
    -- reverses (rather than re-sorting ascending under alpha semantics).
    let edD1 = key (KAltChar 's')
                 ((mkCsv "h\n2024-01-15\n2023-12-01\n2024-02-01")
                    { edFreezeHeader = True })
        edD2 = key (KAltChar 's') edD1
    checkEq "second sort on typed date column flips to descending"
      (col0 edD2)
      (map T.pack ["h", "2024-02-01", "2024-01-15", "2023-12-01"])

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

  -- Lint ---------------------------------------------------------------------
  do
    -- ruff (concise): code + [*] fixable marker skipped, E999 → error, noise
    -- lines (summary / blank) skipped, 1-based → 0-based.
    let ruffOut = T.pack $ unlines
          [ "Found 2 errors."
          , ""
          , "foo.py:3:8: F401 [*] 'os' imported but unused"
          , "foo.py:10:1: E999 SyntaxError: invalid syntax" ]
        ruffD = parseLintOutput LRuff "foo.py" ruffOut
    checkEq "ruff count" (length ruffD) 2
    case ruffD of
      (d : _) -> do
        checkEq "ruff line 0-based" (dgLine d) 2
        checkEq "ruff col 0-based" (dgCol d) 7
        checkEq "ruff code" (dgCode d) (T.pack "F401")
        checkEq "ruff sev" (dgSev d) SevWarning
        checkEq "ruff [*] stripped" (dgMsg d) (T.pack "'os' imported but unused")
      _ -> check "ruff parsed" False
    checkEq "ruff E999 error" (map dgSev ruffD) [SevWarning, SevError]

    -- flake8: E9xx → error, others warning, F401 stays a warning.
    let flakeOut = T.pack $ unlines
          [ "foo.py:1:1: E999 IndentationError: unexpected indent"
          , "foo.py:2:5: F401 'os' imported but unused" ]
        flakeD = parseLintOutput LFlake8 "foo.py" flakeOut
    checkEq "flake8 sev list" (map dgSev flakeD) [SevError, SevWarning]
    checkEq "flake8 codes" (map dgCode flakeD) (map T.pack ["E999", "F401"])

    -- eslint (stylish): header path line and "N problems" summary are skipped;
    -- rows carry line:col, a severity word, the message, and usually a
    -- trailing rule id (absent for parsing errors, which also report 0:0 —
    -- clamped to the buffer start). A double space inside the message must not
    -- promote its tail to a rule id unless it actually looks like one.
    let eslintOut = T.pack $ unlines
          [ "/w/foo.js"
          , "   1:10  error    Unexpected var  no-var"
          , "  12:3   warning  'y' is assigned a value but never used. Allowed unused vars must match /^_/u  @typescript-eslint/no-unused-vars"
          , "   0:0   error    Parsing error: Unexpected token )"
          , ""
          , "\x2716 3 problems (2 errors, 1 warning)"
          , "  1 error and 0 warnings potentially fixable with the `--fix` option." ]
        eslintD = parseLintOutput LEslint "foo.js" eslintOut
    checkEq "eslint count" (length eslintD) 3
    case eslintD of
      [d0, d1, d2] -> do
        checkEq "eslint parse-error pos" (dgLine d0, dgCol d0) (0, 0)
        checkEq "eslint parse-error code" (dgCode d0) (T.pack "")
        checkEq "eslint parse-error msg" (dgMsg d0) (T.pack "Parsing error: Unexpected token )")
        checkEq "eslint pos" (dgLine d1, dgCol d1) (0, 9)
        checkEq "eslint code" (dgCode d1) (T.pack "no-var")
        checkEq "eslint sev" (dgSev d1) SevError
        checkEq "eslint msg" (dgMsg d1) (T.pack "Unexpected var")
        checkEq "eslint plugin code" (dgCode d2) (T.pack "@typescript-eslint/no-unused-vars")
        checkEq "eslint plugin sev" (dgSev d2) SevWarning
      _ -> check "eslint parsed" False

    -- stylelint (unix): severity in [ ], optional trailing (rule) → code.
    let styOut = T.pack $ unlines
          [ "a.scss:2:3: Unexpected unit [error]"
          , "a.scss:4:5: Unexpected thing (unit-no-unknown) [error]" ]
        styD = parseLintOutput LStylelint "a.scss" styOut
    checkEq "stylelint sev" (map dgSev styD) [SevError, SevError]
    checkEq "stylelint codes" (map dgCode styD) (map T.pack ["", "unit-no-unknown"])
    checkEq "stylelint msg no paren" (map dgMsg styD)
      (map T.pack ["Unexpected unit", "Unexpected thing"])

    -- shellcheck (gcc): note → info; path field is "-"; code from [SCnnnn].
    let scOut = T.pack $ unlines
          [ "-:2:1: warning: foo unused [SC2034]"
          , "-:5:3: note: minor thing [SC2086]"
          , "-:7:1: error: bad [SC1009]" ]
        scD = parseLintOutput LShellcheck "-" scOut
    checkEq "shellcheck sev" (map dgSev scD) [SevWarning, SevInfo, SevError]
    checkEq "shellcheck codes" (map dgCode scD) (map T.pack ["SC2034", "SC2086", "SC1009"])

    -- pyright: own parser, leading spaces, " - " sep, (reportXxx) → code.
    let pyOut = T.pack $ unlines
          [ "  /path/foo.py:12:5 - error: Something wrong (reportGeneralTypeIssues)"
          , "  /path/foo.py:3:1 - warning: mild (reportUnusedImport)"
          , "1 error, 1 warning, 0 informations " ]
        pyD = parseLintOutput LPyright "/path/foo.py" pyOut
    checkEq "pyright count" (length pyD) 2
    case [ d | d <- pyD, dgSev d == SevError ] of
      (d : _) -> do
        checkEq "pyright pos" (dgLine d, dgCol d) (11, 4)
        checkEq "pyright code" (dgCode d) (T.pack "reportGeneralTypeIssues")
        checkEq "pyright msg" (dgMsg d) (T.pack "Something wrong")
      _ -> check "pyright error diag" False

    -- Windows-ish path with a drive-letter colon: the numeric-colon scan wins.
    let winD = parseLintOutput LFlake8 "C:\\x\\foo.py"
                 (T.pack "C:\\x\\foo.py:1:2: E501 line too long\n")
    checkEq "windows path parses" (map (\d -> (dgLine d, dgCol d, dgCode d)) winD)
      [(0, 1, T.pack "E501")]

    -- Cap: 600 lines → 500 diags.
    let capOut = T.pack (concat (replicate 600 "foo.py:1:1: E501 x\n"))
    checkEq "lint cap" (length (parseLintOutput LFlake8 "foo.py" capOut)) maxDiagsPerFile

    -- diagSpans: identifier run, non-ident width 1, past-EOL, empty, overlap order.
    let dg c sv = Diag 0 c sv (T.pack "") (T.pack "") LRuff
    checkEq "span identifier run" (diagSpans (T.pack "foo bar") [dg 0 SevWarning])
      [(0, 3, SevWarning)]
    checkEq "span non-ident width 1" (diagSpans (T.pack "a+b") [dg 1 SevWarning])
      [(1, 2, SevWarning)]
    checkEq "span past EOL" (diagSpans (T.pack "abc") [dg 10 SevWarning])
      [(2, 3, SevWarning)]
    checkEq "span empty line" (diagSpans (T.pack "") [dg 0 SevWarning])
      [(0, 0, SevWarning)]
    checkEq "span overlap severity order"
      (map (\(_, _, s) -> s) (diagSpans (T.pack "foobar") [dg 0 SevWarning, dg 0 SevError]))
      [SevError, SevWarning]

    -- diagAt: prefer the more severe diag covering the position.
    case diagAt 0 2 (T.pack "foobar") [dg 0 SevWarning, dg 0 SevError] of
      Just d -> checkEq "diagAt severity" (dgSev d) SevError
      Nothing -> check "diagAt hit" False
    check "diagAt miss on other line" (diagAt 1 2 (T.pack "foobar") [dg 0 SevError] == Nothing)

    -- lintersForPath: case-insensitive extension routing.
    checkEq "lintersForPath .PY" (map linId (lintersForPath "a.PY"))
      [LRuff, LFlake8, LPyright]
    checkEq "lintersForPath .tsx" (map linId (lintersForPath "a.tsx")) [LEslint]
    checkEq "lintersForPath none" (map linId (lintersForPath "Makefile")) []
    checkEq "linterById total" (linId (linterById LShellcheck)) LShellcheck
    checkEq "linters count" (length linters) 6

  -- Lint settings/config -----------------------------------------------------
  do
    let mkLR t = LoadResult (fromText (T.pack t)) LF Utf8 True False Nothing
        ed0 = newEditor (24, 80) defaultConfig
        availAll = [ (linId l, Just ("/bin/" ++ linName l, linName l ++ " 9.9")) | l <- linters ]
        setOff lid = map (\(i, b) -> if i == lid then (i, False) else (i, b))
        runsIds req = map (\(i, _, _, _) -> i) (lrRuns req)
        -- a plain .py document with every linter detected as installed
        edPy = (setLoaded "/w/a.py" (mkLR "import os\n") ed0) { edLintAvail = availAll }

    -- settingsSpec / applySettingRow position sync (extends the settings tests).
    checkEq "settingsSpec has editing + master + per-linter rows"
      (length settingsSpec) (15 + length linters)
    check "applySettingRow round-trips every spec row to its default"
      (and [ edConfig (applySettingRow k (ixOf defaultConfig) ed0) == defaultConfig
           | (k, (_, _, _, ixOf, _)) <- zip [0 ..] settingsSpec ])
    -- Row 12 = the crash-recovery journal: config only (journalling is
    -- driver-side, so there is no session mirror to check).
    check "applySettingRow 12 toggles cfgJournal"
      (not (cfgJournal (edConfig (applySettingRow 12 0 ed0)))
       && cfgJournal (edConfig (applySettingRow 12 1 ed0)))
    -- Row 13 = restore session: likewise config-only (it is read at startup).
    check "applySettingRow 13 toggles cfgRestoreSession"
      (cfgRestoreSession (edConfig (applySettingRow 13 1 ed0))
       && not (cfgRestoreSession (edConfig (applySettingRow 13 0 ed0))))

    -- Config: parsing, per-linter keys, unknown-key warning, round-trip.
    let (cl, wl) = parseConfigText (T.pack "lint-pyright = on\nlint = off\n") defaultConfig
    checkEq "lint master parses off" (cfgLint cl) False
    checkEq "lint-pyright = on parses" (lookup LPyright (cfgLintOn cl)) (Just True)
    checkEq "lint config no warnings" wl []
    let (_, wb) = parseConfigText (T.pack "lint-foo = on\n") defaultConfig
    checkEq "unknown lint- key warns" (length wb) 1
    let wantLint = defaultConfig { cfgLint = False
                                 , cfgLintOn = [ (linId l, linId l == LPyright) | l <- linters ] }
        writtenLint = updateConfigText wantLint (T.pack "")
    check "lint config round-trips through the writer"
      (fst (parseConfigText writtenLint defaultConfig) == wantLint)
    check "writer renders lint keys"
      ("lint = off" `isInfixOf` T.unpack writtenLint
       && "lint-ruff = off" `isInfixOf` T.unpack writtenLint
       && "lint-pyright = on" `isInfixOf` T.unpack writtenLint)

    -- lintRequest gating.
    check "master off -> no lint" (lintRequest False edPy { edConfig = (edConfig edPy) { cfgLint = False } } == Nothing)
    checkEq "edit-time runs ruff, drops superseded flake8"
      (fmap runsIds (lintRequest False edPy)) (Just [LRuff])
    -- pyright is default-off; enable it to check the save-time path includes it.
    let edPyAll = edPy { edConfig = (edConfig edPy) { cfgLintOn = [ (linId l, True) | l <- linters ] } }
    checkEq "edit-time excludes save-time-only pyright"
      (fmap runsIds (lintRequest False edPyAll)) (Just [LRuff])
    checkEq "save-time also runs pyright"
      (fmap runsIds (lintRequest True edPyAll)) (Just [LRuff, LPyright])
    let edNoRuff = edPy { edConfig = (edConfig edPy) { cfgLintOn = setOff LRuff (cfgLintOn (edConfig edPy)) } }
    checkEq "disabling ruff keeps flake8"
      (fmap runsIds (lintRequest False edNoRuff)) (Just [LFlake8])
    let edCsvDoc = (setLoaded "/w/t.csv" (mkLR "a,b\n1,2") ed0) { edLintAvail = availAll }
    check "csv doc -> no lint" (lintRequest False edCsvDoc == Nothing)
    let img1 = Image 1 1 "test" (listArray (0, 3) [0, 0, 0, 255])
        edImg = edPy { edImage = Just (mkImageDoc [(img1, 0)]) }
    check "image doc -> no lint" (lintRequest False edImg == Nothing)
    check "untitled -> no lint"
      (lintRequest False (ed0 { edLintAvail = availAll, edBuffer = fromText (T.pack "x=1\n") }) == Nothing)
    check "cmedit:// pseudo-path -> no lint"
      (lintRequest False (edPy { edPath = Just "cmedit://x.py" }) == Nothing)

    -- edEditSeq bumps on edits/undo/redo, not on plain navigation.
    let edPlain = setLoaded "/w/n.txt" (mkLR "hello\nworld") ed0
        seqOf = edEditSeq
        edType = fst (update (KChar 'z') edPlain)
        edUndo1 = fst (update (KCtrlChar 'z') edType)
        edRedo1 = fst (update (KCtrlChar 'y') edUndo1)
        edNav  = fst (update (KArrow DDown noMods) edType)
    check "edEditSeq bumps on insert" (seqOf edType > seqOf edPlain)
    check "edEditSeq bumps on undo"   (seqOf edUndo1 > seqOf edType)
    check "edEditSeq bumps on redo"   (seqOf edRedo1 > seqOf edUndo1)
    checkEq "edEditSeq unchanged by navigation" (seqOf edNav) (seqOf edType)

    -- diagCounts / diagUnderCursor / jumpNextDiag.
    let d0 = Diag 0 0 SevError (T.pack "E1") (T.pack "bad") LRuff
        d2 = Diag 2 0 SevWarning (T.pack "W1") (T.pack "meh") LRuff
        d2b = Diag 2 4 SevInfo (T.pack "") (T.pack "note") LRuff
        edD = edPlain { edBuffer = fromText (T.pack "aaa\nbbb\nccccc")
                      , edDiags = [d0, d2, d2b], edCursor = Pos 0 0 }
    checkEq "diagCounts (errors, warns+info)" (diagCounts edD) (1, 2)
    check "diagUnderCursor at (0,0)" (fmap dgSev (diagUnderCursor edD) == Just SevError)
    let edJ1 = jumpNextDiag edD                       -- from (0,0) -> next after it = line 2
    checkEq "jumpNextDiag advances" (posLine (edCursor edJ1)) 2
    let edJ2 = jumpNextDiag (edD { edCursor = Pos 2 4 }) -- past the last -> wraps to line 0
    checkEq "jumpNextDiag wraps to first" (posLine (edCursor edJ2)) 0
    check "jumpNextDiag on empty is a no-op with a note"
      (edCursor (jumpNextDiag (edD { edDiags = [] })) == Pos 0 0)

    -- diagToolNames / the status bar's tool prefix.
    let dPyr = Diag 1 0 SevWarning (T.pack "reportX") (T.pack "hm") LPyright
    checkEq "diagToolNames in catalogue order"
      (diagToolNames (edD { edDiags = [dPyr, d0] })) ["ruff", "pyright"]
    check "status bar names the tool before the counts"
      ("ruff: 1E 2W" `isInfixOf` fst (statusRightInfo edD))
    check "status bar joins multiple tools"
      ("ruff+pyright: " `isInfixOf` fst (statusRightInfo (edD { edDiags = [d0, dPyr] })))

    -- Next Problem is pruned from the Find menu unless linting could produce
    -- (or has produced) diagnostics for the active document.
    let findActs e = [ a | MEItem _ _ a <- entriesFor e 2 ]   -- Find menu is index 2
    check "no linter for file type -> Next Problem hidden"
      (MANextProblem `notElem` findActs edPlain)
    check "active linter for file type -> Next Problem shown"
      (MANextProblem `elem` findActs edPy)
    check "existing diags keep Next Problem shown"
      (MANextProblem `elem` findActs edD)
    check "master lint off without diags hides Next Problem"
      (MANextProblem `notElem` findActs
        (edPy { edConfig = (edConfig edPy) { cfgLint = False } }))

    -- lintResults targets a zipper doc by path.
    let edA  = setLoaded "/w/a.py" (mkLR "aaa") ed0
        edTwo = setLoadedNew "/w/b.py" (mkLR "bbb") edA   -- b active, a backgrounded
        edLR = lintResults "/w/a.py" [d0] edTwo
    check "lintResults sets a zipper doc's docDiags"
      (any (\dd -> docPath dd == Just "/w/a.py" && docDiags dd == [d0])
           (edBefore edLR ++ edAfter edLR))
    check "lintResults leaves the active doc alone" (null (edDiags edLR))

    -- Master toggle off clears diagnostics everywhere (no future pass will).
    let edDirty = edLR { edDiags = [d0] }                 -- active + zipper both carry diags
        edMOff  = applySettingRow 14 0 edDirty            -- row 14 = Linting master, 0 = off
    check "master lint off clears active diags" (null (edDiags edMOff))
    check "master lint off clears zipper diags"
      (all (null . docDiags) (edBefore edMOff ++ edAfter edMOff))
    check "master lint on keeps diags" (edDiags (applySettingRow 14 1 edDirty) == [d0])

    -- lintersDetected refreshes an open Settings dialog's per-linter notes.
    let edSettingsNone = openSettings ed0                 -- edLintAvail all Nothing
        edSettingsAv = lintersDetected availAll edSettingsNone
        ruffRow d = dlgChoices d !! 15                     -- master at 14, ruff (linters!!0) at 15
    check "settings shows not-installed note before detection"
      (maybe False (\d -> maybe False (T.isInfixOf (T.pack "\x2717")) (chNote (ruffRow d)))
             (edDialog edSettingsNone))
    check "lintersDetected refreshes the note in place"
      (maybe False (\d -> maybe False (T.isInfixOf (T.pack "\x2713")) (chNote (ruffRow d)))
             (edDialog edSettingsAv))
    check "lintersDetected preserves dialog focus and choice indices"
      (maybe False (\d -> dlgFocus d == 0 && chIx (ruffRow d) == chIx (ruffRow (maybe d id (edDialog edSettingsNone))))
             (edDialog edSettingsAv))

    -- ✓ / ✗ notes use single-width glyphs (else fall back per the spec).
    checkEq "check glyph width" (charWidth '\x2713') 1
    checkEq "cross glyph width" (charWidth '\x2717') 1

    -- dialogScroll: fits -> 0; overflow with focus on buttons -> clamped positive
    -- and keeps the buttons row reachable.
    let Just sdlg = edDialog (openSettings edPy)
        dbtn = sdlg { dlgFocus = focusCount sdlg - 1 }    -- focus on the last button
        totalFit = length (dialogRows sdlg)
        totalBtn = length (dialogRows dbtn)
    checkEq "dialogScroll fits -> 0" (dialogScroll (totalFit + 5) sdlg) 0
    check "dialogScroll overflow scrolls to reach the buttons"
      (let s = dialogScroll 8 dbtn in s == totalBtn - 8 && s > 0)

  -- Lint render ----------------------------------------------------------------
  do
    -- Style/StyleU pattern synonym: the three-field synonym equals the
    -- four-field constructor with a default underline colour, and record
    -- updates on a synonym-built value preserve the underline-colour field.
    check "Style synonym == StyleU with default ul"
      (Style Red Blue attrBold == StyleU Red Blue attrBold Default)
    checkEq "synonym-built styleUl defaults to Default"
      (styleUl (Style Red Blue attrBold)) Default
    checkEq "record update preserves styleUl (default)"
      (styleUl ((Style Red Blue attrBold) { styleFg = Green })) Default
    checkEq "record update preserves styleUl (set)"
      (styleUl (((Style Red Blue attrBold) { styleUl = Green }) { styleFg = Yellow })) Green

    -- SGR emission of undercurl + underline colour, gated on rcUndercurl.
    let sgrStr caps st = map (toEnum . fromIntegral)
                           (BSL.unpack (BB.toLazyByteString (styleSgrWith caps st))) :: String
        ulStyle = StyleU Default Default attrUndercurl (ColorRGB 200 40 40)
        onCaps  = plainCaps { rcUndercurl = True }
        sOn  = sgrStr onCaps ulStyle
        sOff = sgrStr plainCaps ulStyle
    check "undercurl caps on: emits 4:3"  ("4:3" `isInfixOf` sOn)
    check "undercurl caps on: emits 58:"  ("58:" `isInfixOf` sOn)
    check "undercurl caps off: plain ;4"  (";4" `isInfixOf` sOff)
    check "undercurl caps off: no 4:3"    (not ("4:3" `isInfixOf` sOff))
    check "undercurl caps off: no 58"     (not ("58" `isInfixOf` sOff))
    -- A named/indexed underline colour uses the 58:5:n form.
    check "named underline colour -> 58:5:1"
      ("58:5:1" `isInfixOf` sgrStr onCaps (StyleU Default Default attrUndercurl Red))

    -- expandLineCells with a diagOver span: covered cells gain attrUndercurl and
    -- keep the base fg + a set styleUl; uncovered cells are untouched.
    let baseSty = Style Red Default attrNone
        cells1  = expandLineCells 4 False (const baseSty) (Style White Blue attrNone)
                    defaultStyle Nothing False [] [(1, 3, Green)] [] (T.pack "abcd")
        cellAt d cs = head [ c | (dd, c) <- cs, dd == d ]
        styD d = cellStyle (cellAt d cells1)
    check "diag: covered cell gains attrUndercurl" (hasAttr attrUndercurl (styleAttr (styD 1)))
    checkEq "diag: covered cell underline colour"  (styleUl (styD 1)) Green
    checkEq "diag: covered cell keeps base fg"      (styleFg (styD 1)) Red
    check "diag: uncovered cell has no undercurl"   (not (hasAttr attrUndercurl (styleAttr (styD 0))))
    checkEq "diag: uncovered cell keeps default ul" (styleUl (styD 0)) Default

    -- Wide-glyph continuation cells keep alignment and carry the head's squiggle.
    let cells2 = expandLineCells 4 False (const baseSty) (Style White Blue attrNone)
                   defaultStyle Nothing False [] [(0, 1, Green)] [] (T.pack "\x4E2Dx")
        headC = cellAt 0 cells2
        contC = cellAt 1 cells2
        tailC = cellAt 2 cells2
    checkEq "wide glyph head char"              (cellChar headC) '\x4E2D'
    checkEq "wide glyph continuation sentinel"  (cellChar contC) '\NUL'
    checkEq "wide glyph tail char"              (cellChar tailC) 'x'
    check "wide glyph head has undercurl"          (hasAttr attrUndercurl (styleAttr (cellStyle headC)))
    check "wide glyph continuation has undercurl"  (hasAttr attrUndercurl (styleAttr (cellStyle contC)))
    checkEq "wide glyph continuation shares underline colour"
      (styleUl (cellStyle contC)) Green


  -- Search in documents ------------------------------------------------------
  --
  -- The engine half: a reading-view model flattened to searchable text, and
  -- each match addressed by the unit its view can navigate to rather than by a
  -- line number, which in a reflowed view depends on the terminal width.
  do
    let mkPar t = Rtf.defaultPar { Rtf.rpRuns = [Rtf.RtfRun (T.pack t) Rtf.defaultFmt] }
        pars = Seq.fromList (map mkPar
                 ["Chapter one text", "alpha here", "Chapter two text", "alpha again"])
        sects = Seq.fromList [(0, T.pack "One"), (2, T.pack "Two")]
        rdBook = Rtf.mkRtfDocFrom (RtfFromContainer (T.pack "EPUB")) sects T.empty pars
        rdDoc  = Rtf.mkRtfDocFrom (RtfFromContainer (T.pack "DOCX")) Seq.empty T.empty pars
        lit t = S.matcherLine (either (error "bad matcher") id
                                (S.compileMatcher False False False (T.pack t)))

    -- An e-book is addressed by chapter; a document with no sections falls
    -- back to its paragraph index, which is the remaining intrinsic address.
    let (bookMs, _, _) = DT.docMatches (lit "alpha") (DT.extractRtf S.DKBook rdBook)
    checkEq "epub search finds both" (length bookMs) 2
    checkEq "epub match 1 is in chapter 1" (fmap fst (mUnit (head bookMs))) (Just 1)
    checkEq "epub match 1 label"           (fmap snd (mUnit (head bookMs))) (Just (T.pack "ch.1"))
    checkEq "epub match 2 is in chapter 2" (fmap fst (mUnit (bookMs !! 1))) (Just 2)

    let (docMs, _, _) = DT.docMatches (lit "alpha") (DT.extractRtf S.DKWord rdDoc)
    checkEq "docx match 1 is paragraph 2" (fmap fst (mUnit (head docMs))) (Just 2)
    checkEq "docx match 2 is paragraph 4" (fmap fst (mUnit (docMs !! 1))) (Just 4)
    checkEq "docx label is a paragraph mark"
      (fmap snd (mUnit (head docMs))) (Just (T.pack "\x00b6\&2"))

    -- A workbook is addressed by sheet, and labelled by cell: one extracted
    -- line per non-empty cell, because a cell is what the grid can put a
    -- cursor on.
    let wb = Xlsx.mkWorkbook
               [ (T.pack "S1", Seq.fromList [ Seq.fromList [T.pack "x", T.pack "find me"]
                                            , Seq.fromList [T.pack "", T.pack "y"] ])
               , (T.pack "S2", Seq.fromList [ Seq.fromList [T.pack "find me too"] ]) ]
               T.empty
        (wbMs, _, _) = DT.docMatches (lit "find me") (DT.extractBook wb)
    checkEq "workbook finds both sheets" (length wbMs) 2
    checkEq "workbook match 1 sheet"  (fmap fst (mUnit (head wbMs))) (Just 1)
    checkEq "workbook match 1 cell"   (fmap snd (mUnit (head wbMs))) (Just (T.pack "B1"))
    checkEq "workbook match 2 sheet"  (fmap fst (mUnit (wbMs !! 1))) (Just 2)
    checkEq "workbook match 2 cell"   (fmap snd (mUnit (wbMs !! 1))) (Just (T.pack "A1"))

    -- A term spanning two columns is deliberately not found: those are two
    -- values that happen to be adjacent, not a phrase.
    let (spanMs, _, _) = DT.docMatches (lit "x find") (DT.extractBook wb)
    checkEq "workbook does not match across cells" (length spanMs) 0

    -- cellName is the inverse of cellRef, bijective base-26 and all.
    checkEq "cellName A1"   (Xlsx.cellName 0 0)    (T.pack "A1")
    checkEq "cellName Z1"   (Xlsx.cellName 0 25)   (T.pack "Z1")
    checkEq "cellName AA1"  (Xlsx.cellName 0 26)   (T.pack "AA1")
    checkEq "cellName AB12" (Xlsx.cellName 11 27)  (T.pack "AB12")
    check "cellName round-trips through cellRef"
      (and [ Xlsx.cellRef (Xlsx.cellName r c) == Just (c, r)
           | (r, c) <- [(0,0), (0,25), (0,26), (11,27), (999,701), (5,702)] ])

    -- Only the document extensions are decoded, and they are exactly the ones
    -- that would otherwise be skipped as binary.
    check "documentExtension accepts a pdf"  (S.documentExtension "paper.pdf")
    check "documentExtension accepts a docx" (S.documentExtension "/a/b/Report.DOCX")
    check "documentExtension rejects source" (not (S.documentExtension "Main.hs"))
    check "documentExtension rejects a zip"  (not (S.documentExtension "x.zip"))
    check "documents are binary without the option"
      (all S.binaryExtension ["a.pdf", "a.docx", "a.xlsx", "a.epub", "a.ods", "a.odt"])
    -- .rtf is the exception, and deliberately: it is text on disk, so with the
    -- option off it is searched as the markup it is.
    check "rtf stays searchable as text" (not (S.binaryExtension "a.rtf"))

    -- Replace can never reach a document result, and the panel can say how
    -- many it is leaving alone.
    let docFr = (S.plainResult "/p/a.pdf" [S.plainMatch 0 [(0,3)] (T.pack "foo")] False False)
                  { frDoc = Just S.DKPdf }
        txtFr = S.plainResult "/p/b.txt" [S.plainMatch 0 [(0,3)] (T.pack "foo")] False False
        ssMix = (S.newSearchState "/p") { ssResults = Seq.fromList [docFr, txtFr] }
    checkEq "resultPaths lists everything"
      (S.resultPaths ssMix) ["/p/a.pdf", "/p/b.txt"]
    checkEq "replaceablePaths excludes documents"
      (S.replaceablePaths ssMix) ["/p/b.txt"]
    checkEq "docResultCount counts them" (S.docResultCount ssMix) 1
    check "a document result is not replaceable" (not (S.replaceable docFr))
    check "a text result is replaceable"         (S.replaceable txtFr)

    -- A search whose every hit is in a document has nothing to replace, and
    -- says why rather than reporting an empty result.
    let ssAllDocs = (S.newSearchState "/p") { ssResults = Seq.fromList [docFr]
                                            , ssFind = S.mkField (T.pack "foo")
                                            , ssShowReplace = True }
        edAllDocs = (newEditor (24, 80) defaultConfig) { edSearch = Just ssAllDocs }
        (edNo, effNo) = runReplaceAll edAllDocs
    checkEq "all-document replace emits no effect" (length effNo) 0
    check "all-document replace explains itself"
      (maybe False (T.isInfixOf (T.pack "cannot write") . ssMessage) (edSearch edNo))

  -- In-file Find inside the formatted reading view --------------------------
  --
  -- Enabled when the workspace search learned to look inside documents: a hit
  -- you cannot then open in the document is worth much less.
  do
    let mkPar t = Rtf.defaultPar { Rtf.rpRuns = [Rtf.RtfRun (T.pack t) Rtf.defaultFmt] }
        pars = Seq.fromList (map mkPar ["alpha beta", "gamma delta", "beta again"])
        rd0  = Rtf.mkRtfDocFrom (RtfFromContainer (T.pack "DOCX")) Seq.empty T.empty pars
        edD  = containerDocLoaded "/tmp/f.docx" rd0 (newEditor (24, 80) defaultConfig)
        edT  = edD { edSearchTerm = T.pack "beta" }

    check "the formatted view no longer disables Find"
      (MAFind `notElem` rtfDisabledActions)
    check "the formatted view still disables Replace"
      (MAReplace `elem` rtfDisabledActions)

    let edFound = doFind (refreshRtf edT)
    check "find in a formatted document selects the match"
      (maybe False (not . T.null . Rtf.rtfSelText) (edRtf edFound))
    checkEq "find in a formatted document selects the term"
      (maybe T.empty Rtf.rtfSelText (edRtf edFound)) (T.pack "beta")

    -- Find Next advances rather than re-finding the match it is sitting on.
    let edNext = findAgain True edFound
    check "find next advances"
      (maybe False (\rd -> Rtf.rtfSelection rd /= maybe Nothing Rtf.rtfSelection (edRtf edFound))
        (edRtf edNext))

    -- Landing on a workspace hit: go to the unit, then find the term there.
    let edLand = applyPendingDoc
                   edD { edPendingDoc = Just ("/tmp/f.docx", 3, T.pack "beta") }
    checkEq "landing clears the pending jump" (edPendingDoc edLand) Nothing
    checkEq "landing selects the term in that paragraph"
      (maybe T.empty Rtf.rtfSelText (edRtf edLand)) (T.pack "beta")
    -- Unit 3 is the paragraph "beta again", so landing must select *that*
    -- "beta" and not the one in paragraph 1 that a search from the top would
    -- have hit first. (The window is taller than this three-line document, so
    -- rdTop legitimately stays 0 — the selection is the assertion that bites.)
    checkEq "landing selects the match in the named unit"
      (fmap (posLine . fst) (edRtf edLand >>= Rtf.rtfSelection)) (Just 2)

  -- Crash-safe edit journal (plan 0011) ---------------------------------------
  --
  -- Everything that can go wrong with a journal is either "it did not survive
  -- the round trip" or "recovery drew the wrong conclusion", and both are pure.
  -- The format is deliberately conservative: exact text, exact baseline,
  -- forward-compatible headers, and nothing that could reach the real file.
  do
    let jMt   = posixSecondsToUTCTime 1721890123.456789
        jBase = Journal { jPath         = Just "/home/ben/work/x.py"
                        , jMtime        = Just jMt
                        , jEol          = LF
                        , jEnc          = Utf8
                        , jFinalNewline = True
                        , jReadOnly     = False
                        , jCursor       = Pos 412 7
                        , jText         = T.pack "alpha\nbeta" }
        roundTrips lbl j = checkEq ("journal round-trip: " ++ lbl)
                             (J.parseJournal (J.serializeJournal j)) (Just j)
        rawJournal = TE.encodeUtf8 . T.pack

    roundTrips "plain" jBase
    roundTrips "crlf + BOM + no final newline"
      jBase { jEol = CRLF, jEnc = Utf8Bom, jFinalNewline = False }
    roundTrips "untitled buffer" jBase { jPath = Nothing, jMtime = Nothing }
    roundTrips "read-only document" jBase { jReadOnly = True }
    roundTrips "named file that does not exist yet" jBase { jMtime = Nothing }
    roundTrips "empty buffer" jBase { jText = T.empty }
    -- A buffer whose last line is blank is exactly the case a courtesy
    -- trailing newline in the body would erase.
    roundTrips "buffer ending in a blank line" jBase { jText = T.pack "a\n" }
    roundTrips "buffer of only blank lines" jBase { jText = T.pack "\n\n" }
    -- A CR inside a line must stay a character, not become a line break.
    roundTrips "CR embedded in a line" jBase { jText = T.pack "a\rb\r\nc" }
    roundTrips "very long line"
      jBase { jText = T.replicate 200000 (T.pack "x") }
    roundTrips "non-ASCII text" jBase { jText = T.pack "caf\233 \12354 \128169" }
    -- A POSIX filename may contain a newline; unescaped it would split the
    -- header block and desynchronise the parse.
    roundTrips "path containing a newline and quotes"
      jBase { jPath = Just "/tmp/we\nird \"name\".txt" }

    -- Format guard: the first line is what identifies the file and governs the
    -- meaning of everything after it.
    check "journal starts with its version line"
      (BS.pack (map (fromIntegral . fromEnum) "cmedit-journal 1\n")
         `BS.isPrefixOf` J.serializeJournal jBase)

    -- The baseline must compare EQUAL after a round trip, or the clean-recovery
    -- case never fires on a filesystem with sub-second timestamps.
    checkEq "journal mtime survives exactly"
      (J.parseJournal (J.serializeJournal jBase) >>= jMtime) (Just jMt)
    checkEq "picosecond timestamp round-trips"
      (J.picosToDiskTime (J.diskTimeToPicos jMt)) jMt

    -- Rejections.
    check "a journal with a NUL in the body is rejected"
      (J.parseJournal (BS.concat [J.serializeJournal jBase, BS.pack [0]]) == Nothing)
    check "an unknown journal version is rejected"
      (J.parseJournal (rawJournal "cmedit-journal 2\neol: lf\n--\nhi\n") == Nothing)
    check "a file that is not a journal is rejected"
      (J.parseJournal (rawJournal "hello there\n") == Nothing)
    check "a journal with no separator is rejected"
      (J.parseJournal (rawJournal "cmedit-journal 1\neol: lf\n") == Nothing)

    -- Forward compatibility: a key this version has never heard of must not
    -- make the journal unreadable, and absent keys take their defaults.
    let fwd = J.parseJournal (rawJournal "cmedit-journal 1\nfuture-key: 9\neol: cr\n--\nhi")
    checkEq "unknown header keys are ignored" (fmap jText fwd) (Just (T.pack "hi"))
    checkEq "known keys still read" (fmap jEol fwd) (Just CR)
    checkEq "absent keys default" (fmap jFinalNewline fwd) (Just True)
    checkEq "absent path means untitled" (fmap jPath fwd) (Just Nothing)

    -- Malformed UTF-8 in the body decodes leniently rather than throwing.
    let malformed = BS.concat [ rawJournal "cmedit-journal 1\n--\n"
                              , BS.pack [0x61, 0xff, 0xfe, 0x62] ]
    checkEq "malformed UTF-8 in the body becomes replacement chars"
      (fmap jText (J.parseJournal malformed)) (Just (T.pack "a\65533\65533b"))

    -- Buffer <-> body. The interesting cases are the ones where a line count is
    -- ambiguous unless the body is treated as "lines joined with LF".
    forM_ ["", "a", "a\n", "a\n\n", "a\nb", "\n"] $ \s -> do
      let b = fromText (T.pack s)
          j = jBase { jText = J.bufferJournalText b }
      checkEq ("journal buffer round-trip " ++ show s)
        (toList (bufLines (J.journalBuffer j))) (toList (bufLines b))

    -- Naming. The hash identifies the file; the basename is only there so a
    -- human can tell the journals apart in ~/.cache.
    checkEq "journal name for the same path is stable"
      (J.journalFileName (Just "/home/ben/work/x.py") 0)
      (J.journalFileName (Just "/home/ben/work/x.py") 7)
    check "journal names differ per path"
      (J.journalFileName (Just "/a/x.py") 0 /= J.journalFileName (Just "/b/x.py") 0)
    check "journal name ends in the journal extension"
      (J.isJournalFileName (J.journalFileName (Just "/home/ben/work/x.py") 0))
    check "journal name keeps a readable basename"
      ("-x.py.cmj" `isSuffixOf` J.journalFileName (Just "/home/ben/work/x.py") 0)
    check "journal name sanitises the basename"
      (all (\c -> isAlphaNum c || c `elem` (".-_" :: String))
           (J.journalFileName (Just "/tmp/we ird*na/me?.txt") 0))
    checkEq "untitled journals are numbered"
      (J.journalFileName Nothing 3) "untitled-3.cmj"
    check "an unrelated cache file is not a journal"
      (not (J.isJournalFileName "recent"))

    -- The recovery decision, as a table over (has a path?, had a baseline?,
    -- is there a file now?).
    let tNow   = posixSecondsToUTCTime 200
        untit  = jBase { jPath = Nothing, jMtime = Nothing }
        nobase = jBase { jMtime = Nothing }
        recCases =
          [ ("untitled buffer",              untit,  Just tNow, RecoverUntitled)
          , ("untitled, nothing on disk",    untit,  Nothing,   RecoverUntitled)
          , ("baseline matches",             jBase,  Just jMt,  RecoverClean)
          , ("file changed on disk",         jBase,  Just tNow, RecoverChanged)
          , ("file is gone",                 jBase,  Nothing,   RecoverMissing)
          -- A file that never existed and still does not is not "missing".
          , ("never created, still absent",  nobase, Nothing,   RecoverClean)
          , ("never created, appeared since", nobase, Just tNow, RecoverChanged)
          ]
    forM_ recCases $ \(lbl, j, now, want) ->
      checkEq ("classifyJournal: " ++ lbl) (J.classifyJournal j now) want

    -- Recovery must never offer to write back where the session could not.
    check "recovery may write back a writable named file" (J.canWriteBack jBase)
    check "recovery must not write back a read-only file"
      (not (J.canWriteBack jBase { jReadOnly = True }))
    check "recovery must not write back an untitled buffer"
      (not (J.canWriteBack untit))
    check "a clean recovery carries no caveat"
      (J.recoveryNote RecoverClean == Nothing)
    check "a changed or missing file is flagged"
      (J.recoveryNote RecoverChanged /= Nothing && J.recoveryNote RecoverMissing /= Nothing)

    -- The untitled index has to survive the round trip through the filename,
    -- or a recovered untitled buffer would write itself to a second journal
    -- and leave the first for the next startup to offer again.
    checkEq "untitled journal names round-trip their index"
      (J.untitledIndexOf (J.journalFileName Nothing 7)) (Just 7)
    checkEq "a titled journal name has no untitled index"
      (J.untitledIndexOf (J.journalFileName (Just "/a/x.py") 7)) Nothing
    checkEq "a non-journal name has no untitled index"
      (J.untitledIndexOf "untitled-3.txt") Nothing

  -- The journal write-behind's selection rule, over the open-files zipper.
  -- Everything here is "which documents would a crash lose, and has each one
  -- moved since its journal was written".
  do
    let jcfg  = defaultConfig
        edJ0  = newEditor (24, 80) jcfg
        mkLRj t = LoadResult (fromText (T.pack t)) LF Utf8 True False Nothing
        keysOf = map (\(k, _, _) -> k) . journalRequests
        -- What the driver hands back when a pass completes: the journal key
        -- with the edit counter that write captured.
        wroteOf = map (\(k, s, _) -> (k, s)) . journalRequests
        typed e = fst (update (KChar 'x') e)
        edTxt  = setLoaded "/w/a.txt" (mkLRj "aaa") edJ0

    check "an unmodified document is not journalled"
      (null (journalRequests edTxt))
    check "an edited document is journalled"
      (length (journalRequests (typed edTxt)) == 1)
    check "journalling off writes nothing"
      (null (journalRequests (typed edTxt) { edConfig = jcfg { cfgJournal = False } }))
    -- Turning the key off must also take away what was already written: the
    -- point of journal = off is that no copy is left lying in the cache.
    check "journalling off drops the existing journals"
      (null (journalLiveKeys (typed edTxt) { edConfig = jcfg { cfgJournal = False } }))

    -- The whole point of the per-document counter: recording the write must
    -- settle it, and typing in one file must not re-stale another's.
    let edDirty  = typed edTxt
        edNoted  = journalsWritten (wroteOf edDirty) edDirty
    check "a written journal is not written again"
      (null (journalRequests edNoted))
    check "a further edit makes it stale again"
      (length (journalRequests (typed edNoted)) == 1)
    let edTwo    = typed (setLoadedNew "/w/b.txt" (mkLRj "bbb") edNoted)
        edTwoOk  = journalsWritten (wroteOf edTwo) edTwo
        edTypeB  = typed edTwoOk
    checkEq "editing one file does not restale another's journal"
      (keysOf edTypeB) [journalKeyOf (captureDoc edTypeB)]

    -- The buffer under a CSV table is stale by construction, so journalling it
    -- would journal the pre-table text. (Alt+T is the same serialisation.)
    let edCsvJ = fst (update (KChar 'Z')
                   (setLoaded "/w/t.csv" (mkLRj "a,b\n1,2") edJ0))
    check "a CSV table edit is journalled" (length (journalRequests edCsvJ) == 1)
    check "a journalled CSV holds the table, not the stale line buffer"
      (case journalRequests edCsvJ of
         [(_, _, j)] -> T.pack "Z" `T.isInfixOf` jText j
         _        -> False)

    -- Views with no buffer under them, and things that are not files.
    let pix1 = Image 1 1 "png" (listArray (0, 3) [0, 0, 0, 255])
        edImg = imageLoaded "/w/p.png" [(pix1, 0)] edJ0
    check "an image document is never journalled"
      (null (journalRequests (edImg { edModified = True })))
    check "the manual is never journalled"
      (null (journalRequests (openManual edJ0) { edModified = True }))
    check "a read-only ZIP listing is not journalled"
      (null (journalRequests
               (setLoaded "/w/x.zip" (mkLRj "listing") edJ0) { edModified = True }))

    -- The live-key set is what the driver deletes against: a saved or closed
    -- document must drop out of it, with no save/close path saying so.
    let keyA = journalKeyOf (captureDoc edDirty)
    check "a modified document is a live journal key"
      (keyA `elem` journalLiveKeys edDirty)
    check "saving drops the journal key"
      (keyA `notElem` journalLiveKeys (fst (onSaved 3 Nothing edDirty)))
    check "closing drops the journal key"
      (keyA `notElem` journalLiveKeys (doClose edDirty))
    check "Save All drops every journal key"
      (null (journalLiveKeys
               (fst (savedAll [ (p, Nothing) | p <- modifiedDocPaths edTwo ] edTwo))))
    -- Undo back to the original content is a drop too, and nothing announces it.
    check "undoing back to unmodified drops the journal key"
      (null (journalLiveKeys (fst (update (KCtrlChar 'z') edDirty))))

    -- Two untitled buffers must not share a journal name, however the zipper
    -- is arranged — which is what the per-document id is for.
    let edU1 = typed edJ0
        edU2 = typed (fst (runAction MANew edU1))
    check "two untitled buffers get distinct journal names"
      (length (journalLiveKeys edU2) == 2
         && length (nub (journalLiveKeys edU2)) == 2)
    check "a kept untitled journal cannot be renumbered over"
      (journalKeyOf (captureDoc (typed (fst (runAction MANew (seedJournalIds [9] edJ0)))))
         /= J.journalFileName Nothing 9)

  ---------------------------------------------------------------------------
  -- BEGIN adaptive journal write-behind (plan 0027)
  --
  -- The interval the driver spaces write-behind passes by, and the size
  -- estimate that drives it. A journal is a whole buffer, so the traffic a
  -- session generates is (buffer size / interval); a fixed 2 s meant ~10 MB/s
  -- for a 40 MB buffer under editing (measured through a PTY). These pin the
  -- shape of the curve rather than any one number, plus the two ends, which
  -- are promises: never slower than 2 s for anything ordinary, and never
  -- slower than 30 s for anything at all.
  do
    let mb n = n * 1024 * 1024
        sizes = [0, 1, 100, mb 1, mb 4, mb 10, mb 40, mb 100, mb 1000]

    checkEq "journalDelayUs: the floor is the shipped 2 s debounce"
      (journalDelayUs 0) 2000000
    check "journalDelayUs: an ordinary file is untouched by any of this"
      (all (\n -> journalDelayUs n == journalMinDelayUs)
           [0, 1, 1000, 100 * 1024, mb 1, mb 2, mb 4])
    check "journalDelayUs: monotonic in the bytes to write"
      (and [ journalDelayUs a <= journalDelayUs b
           | (a, b) <- zip sizes (tail sizes) ])
    -- Including sizes no buffer can reach: the arithmetic must not overflow
    -- into a *short* interval, which is the failure that would matter.
    check "journalDelayUs: never below the floor, never above the ceiling"
      (all (\n -> journalDelayUs n >= journalMinDelayUs
                    && journalDelayUs n <= journalMaxDelayUs)
           (sizes ++ [-1, maxBound `div` 2, maxBound]))
    check "journalDelayUs: an absurd size still lands on the ceiling"
      (journalDelayUs maxBound == journalMaxDelayUs
         && journalDelayUs (mb 1000) == journalMaxDelayUs)
    -- The band a big buffer must land in: 40 MB at the 2 MB/s budget is 20 s,
    -- which is what turns ~10 MB/s of write traffic into ~2.
    check "journalDelayUs: a 40 MB buffer lands at ~20 s"
      (journalDelayUs (mb 40) >= 18000000 && journalDelayUs (mb 40) <= 22000000)
    -- The budget is what the interval *means*, so state it as traffic: no size
    -- may exceed the budget except by the deliberate ceiling, and at the
    -- ceiling the overshoot is bounded (100 MB is the largest file that opens
    -- as text at all — see maxOpenBytes).
    check "journalDelayUs: traffic stays within the budget until the ceiling"
      (all (\n -> journalDelayUs n == journalMaxDelayUs
                    || n * 1000000 `div` journalDelayUs n <= journalBudgetBps)
           [mb 5, mb 10, mb 40, mb 60])
    check "journalDelayUs: at the ceiling a 100 MB buffer still costs < 4 MB/s"
      (mb 100 * 1000000 `div` journalDelayUs (mb 100) < 4 * 1024 * 1024)

    -- The size the interval is computed from is the *stale* journals' size:
    -- what a pass would actually write, not what is open.
    let jcfg2 = defaultConfig
        edB0  = newEditor (24, 80) jcfg2
        big n = T.replicate n (T.pack "abcdefghij\n")
        mkLR2 t = LoadResult (fromText t) LF Utf8 True False Nothing
        edBig = setLoaded "/w/big.txt" (mkLR2 (big 100000)) edB0
        typed2 e = fst (update (KChar 'x') e)
    checkEq "journalPendingBytes: nothing modified, nothing pending"
      (journalPendingBytes edBig) 0
    check "journalPendingBytes: an edited buffer is measured, cheaply"
      (let n = journalPendingBytes (typed2 edBig)
       in n > 1000000 && n < 1300000)
    check "journalPendingBytes: a 1 MB buffer still journals on the 2 s floor"
      (journalDelayUs (journalPendingBytes (typed2 edBig)) == journalMinDelayUs)
    check "journalPendingBytes: journalling off means no pending bytes"
      (journalPendingBytes (typed2 edBig) { edConfig = jcfg2 { cfgJournal = False } } == 0)
    -- Two stale documents cost one pass between them, so the interval is
    -- driven by their sum.
    let edBig2 = typed2 (setLoadedNew "/w/big2.txt" (mkLR2 (big 100000)) (typed2 edBig))
    check "journalPendingBytes: sums the documents a pass would write"
      (journalPendingBytes edBig2 >= 2 * journalPendingBytes (typed2 edBig) - 4)

    -- The counter that travels with an asynchronous write. Recording the
    -- version that was *written* (not the document's version now) is what
    -- keeps an edit made while the write was in flight from being lost: the
    -- document stays stale and the next pass writes it.
    let edW    = typed2 edBig
        pass   = journalRequests edW          -- what a pass captures
        edW2   = typed2 edW                   -- ...and an edit that lands mid-write
        edDone = journalsWritten [ (k, s) | (k, s, _) <- pass ] edW2
    check "journalsWritten: an edit during the write leaves the journal stale"
      (length (journalRequests edDone) == 1)
    check "journalsWritten: recording the current version settles it"
      (null (journalRequests
               (journalsWritten [ (k, s) | (k, s, _) <- journalRequests edW2 ] edW2)))
    check "journalsWritten: a key nobody wrote is left alone"
      (length (journalRequests (journalsWritten [("no-such.cmj", 99)] edW)) == 1)
  -- END adaptive journal write-behind (plan 0027)
  ---------------------------------------------------------------------------

  -- Recovery: what the dialog installs. Nothing here writes to disk — a
  -- recovered document is an unsaved buffer, which is what makes recovering
  -- always safe to try.
  do
    let recMt  = posixSecondsToUTCTime 500
        recJ p = Journal { jPath = p, jMtime = Just recMt, jEol = CRLF
                         , jEnc = Utf8Bom, jFinalNewline = False
                         , jReadOnly = False, jCursor = Pos 1 2
                         , jText = T.pack "recovered\ntext" }
        -- The key is the name the journal file really has on disk, which for
        -- a titled document is derived from its path.
        item c p = RecoverItem (J.journalFileName p 0) c (recJ p)
        keyFor p = J.journalFileName p (0 :: Int)
        edR0 = newEditor (24, 80) defaultConfig
        edOffer = openRecoverDialog
                    [item RecoverChanged (Just "/w/a.txt")] edR0
        edRec = recoverJournals edOffer

    check "found journals open the recovery prompt"
      (fmap dlgKind (edDialog edOffer) == Just DKRecover)
    checkEq "the prompt offers Recover / Discard / Keep for later"
      (maybe [] dlgButtons (edDialog edOffer))
      [T.pack "Recover", T.pack "Discard", T.pack "Keep for later"]
    check "the prompt names the affected file and its caveat"
      (case edDialog edOffer of
         Just d -> T.pack "a.txt" `T.isInfixOf` dlgMessage d
                     && T.pack "changed on disk" `T.isInfixOf` dlgMessage d
         Nothing -> False)

    checkEq "a recovered document holds the journalled text"
      (bufferToText LF False (edBuffer edRec)) (T.pack "recovered\ntext")
    check "a recovered document is modified" (edModified edRec)
    checkEq "a recovered document restores its path"
      (edPath edRec) (Just "/w/a.txt")
    checkEq "a recovered document restores its line ending" (edLineEnding edRec) CRLF
    checkEq "a recovered document restores its BOM" (edEncoding edRec) Utf8Bom
    check "a recovered document restores its final-newline setting"
      (not (edFinalNewline edRec))
    checkEq "a recovered document restores the cursor" (edCursor edRec) (Pos 1 2)
    checkEq "a recovered document keeps the journal's disk baseline"
      (edDiskMtime edRec) (Just recMt)
    check "a recovered file that changed on disk says so" (edDiskChanged edRec)
    check "recovery closes the prompt and empties the offer"
      (edDialog edRec == Nothing && null (edRecover edRec))
    -- The recovered document keeps the journal's own name, or the write-behind
    -- would write a second file and leave the first behind.
    checkEq "a recovered document adopts its journal's key"
      (journalLiveKeys edRec) [keyFor (Just "/w/a.txt")]
    check "a recovered document does not immediately rewrite its journal"
      (null (journalRequests edRec))
    -- Editing it must still be journalled, against the same key.
    checkEq "editing a recovered document restales its own journal"
      (map (\(k, _, _) -> k) (journalRequests (fst (update (KChar 'x') edRec))))
      [keyFor (Just "/w/a.txt")]

    -- An untitled journal comes back as an untitled buffer under its own name.
    let edU = recoverJournals (openRecoverDialog
                [RecoverItem "untitled-4.cmj" RecoverUntitled (recJ Nothing)] edR0)
    check "an untitled journal recovers as an untitled buffer"
      (edPath edU == Nothing && edModified edU)
    checkEq "an untitled recovery keeps its journal number"
      (journalLiveKeys edU) ["untitled-4.cmj"]

    -- A journal for a file that is already open must patch that document
    -- rather than open a second copy of it.
    let edOpen = setLoaded "/w/a.txt"
                   (LoadResult (fromText (T.pack "on disk")) LF Utf8 True False Nothing) edR0
        edPatched = recoverJournals (openRecoverDialog
                      [item RecoverClean (Just "/w/a.txt")] edOpen)
    checkEq "recovering an already-open file does not open it twice"
      (fileCount edPatched) 1
    checkEq "recovering an already-open file applies the journal"
      (bufferToText LF False (edBuffer edPatched)) (T.pack "recovered\ntext")

    -- Recovery must never propose writing back where the session could not.
    check "a read-only recovery is still offered but not writable"
      (not (J.canWriteBack (recJ (Just "/w/a.txt")) { jReadOnly = True }))

    -- A recovered .csv lands in the table view, which carries its *own* saved
    -- baseline — so the empty 'docSavedBuffer' above does not protect it. If
    -- the table adopted the recovered grid as its saved point, recomputing the
    -- flag through 'csvMod' would call the document clean, and since 0028 that
    -- recomputation is exact and instant: one Ctrl+Z, with no undo step to pop,
    -- sweeps away the journal that is the only copy of the work and lets the
    -- next Ctrl+Q leave without asking. (Review of plan 0028.)
    let csvJ = (recJ (Just "/w/a.csv")) { jText = T.pack "a,b\nc,RECOVERED" }
        edCsvRec = recoverJournals (openRecoverDialog
                     [RecoverItem (keyFor (Just "/w/a.csv")) RecoverChanged csvJ] edR0)
        edCsvZ = fst (update (KCtrlChar 'z') edCsvRec)
    check "a recovered .csv opens in the table view"
      (fmap csvToText (edCsv edCsvRec) == Just (T.pack "a,b\nc,RECOVERED"))
    check "a recovered table is modified" (edModified edCsvRec)
    check "a recovered table's grid is itself modified"
      (fmap isModified (edCsv edCsvRec) == Just True)
    check "a recovered table stays modified across a no-op undo" (edModified edCsvZ)
    checkEq "a recovered table keeps its journal across a no-op undo"
      (journalLiveKeys edCsvZ) [keyFor (Just "/w/a.csv")]
    -- ...and an edit-then-undo cycle, which does have a step to pop.
    let edCsvEdit = fst (update (KChar 'q') edCsvRec)
        edCsvUndo = fst (update (KCtrlChar 'z') edCsvEdit)
    check "a recovered table stays modified after editing and undoing"
      (edModified edCsvUndo)
    checkEq "a recovered table keeps its journal after editing and undoing"
      (journalLiveKeys edCsvUndo) [keyFor (Just "/w/a.csv")]
    -- Saving is what resolves a recovery: the baseline moves to the grid on
    -- screen and the flag clears normally, so the guard is not a latch.
    check "saving a recovered table clears the flag normally"
      (case edCsv edCsvRec of
         Just v  -> not (isModified (markSaved v))
         Nothing -> False)

  -- Full session restore (plan 0011 section 6) --------------------------------
  --
  -- The file format is the recents' own encoding under a version line, so the
  -- part that can actually be wrong is the arithmetic: which document is active
  -- once the files that have since vanished are dropped, and which documents
  -- are worth recording at all.
  do
    let sess = Session (Just "/home/ben/my work")
                 [ RecentEntry "/w/a.txt" 0 0
                 , RecentEntry "/w/b:c:d.txt" 11 4        -- colons belong to the path
                 , RecentEntry "/w/spaced name.md" 3 7 ] 1
        roundTrips lbl s = checkEq ("session round-trip: " ++ lbl)
                             (parseSessionText (renderSessionText s)) (Just s)
    roundTrips "folder, files, active" sess
    roundTrips "no folder" sess { seFolder = Nothing }
    roundTrips "no files" (Session (Just "/w") [] 0)
    roundTrips "nothing at all" (Session Nothing [] 0)

    -- A version we do not know is "no session", not a guess: a later format's
    -- lines would arrive here looking exactly like malformed ones.
    check "an unknown session version is no session"
      (parseSessionText (T.pack "cmedit-session 2\nactive: 0\n1:1:/w/a.txt\n") == Nothing)
    check "a file that is not a session is rejected"
      (parseSessionText (T.pack "1:1:/w/a.txt\n") == Nothing)
    check "an empty session file is no session"
      (parseSessionText T.empty == Nothing)

    -- Tolerance: CRLF line endings, blank lines and lines from a future
    -- version are all skipped rather than taking the session down with them.
    checkEq "CRLF, blanks and unknown keys are tolerated"
      (parseSessionText (T.pack ("cmedit-session 1\r\nfolder: /w\r\nactive: 1\r\n"
                                 ++ "1:1:/w/a.txt\r\n\r\nlayout: split\r\n12:5:/w/b.txt\r\n")))
      (Just (Session (Just "/w")
               [RecentEntry "/w/a.txt" 0 0, RecentEntry "/w/b.txt" 11 4] 1))
    checkEq "an absent active line reads as the first file"
      (fmap seActive (parseSessionText (T.pack "cmedit-session 1\n1:1:/w/a.txt\n"))) (Just 0)
    checkEq "an empty folder value is no folder"
      (fmap seFolder (parseSessionText (T.pack "cmedit-session 1\nfolder:\n"))) (Just Nothing)

    -- planRestore: the index arithmetic, which is the whole of the risk.
    let s3 = Session Nothing [ RecentEntry "/w/a" 0 0
                             , RecentEntry "/w/b" 1 0
                             , RecentEntry "/w/c" 2 0 ] 2
        plan fs s = planRestore fs s
    checkEq "restore keeps the session's order"
      (map rePath (rpFiles (plan [True, True, True] s3))) ["/w/a", "/w/b", "/w/c"]
    checkEq "restore keeps the recorded active file"
      (rpActive (plan [True, True, True] s3)) 2
    checkEq "restore skips the files that are gone"
      (map rePath (rpFiles (plan [True, False, True] s3))) ["/w/a", "/w/c"]
    checkEq "a missing file ahead of the active one shifts it down"
      (rpActive (plan [True, False, True] s3)) 1
    checkEq "two missing files ahead of it shift it twice"
      (rpActive (plan [False, False, True] s3)) 0
    checkEq "a missing active file lands on the next survivor"
      (rpActive (plan [True, False, True] s3 { seActive = 1 })) 1
    checkEq "a missing active file at the end clamps to the last survivor"
      (rpActive (plan [True, False, False] s3)) 0
    checkEq "an out-of-range active index is clamped"
      (rpActive (plan [True, True, True] s3 { seActive = 99 })) 2
    checkEq "the recorded count is reported for the 'N of M' note"
      (rpRecorded (plan [True, False, True] s3)) 3
    check "nothing surviving is an empty plan"
      (null (rpFiles (plan [False, False, False] s3))
       && rpActive (plan [False, False, False] s3) == 0)
    check "a short existence list means the rest are gone"
      (map rePath (rpFiles (plan [True] s3)) == ["/w/a"])

    -- What the editor records. Untitled buffers are absent by construction:
    -- there is no path to reopen, and their content is the journal's job.
    let mkLRs t = LoadResult (fromText (T.pack t)) LF Utf8 True False Nothing
        edS0 = newEditor (24, 80) defaultConfig
        edSa = setLoaded "/w/a.txt" (mkLRs "aaa") edS0
        edSab = setLoadedNew "/w/b.txt" (mkLRs "bbb\nbbb") edSa   -- a backgrounded, b active
        edSabU = fst (update (KCtrlChar 'n') edSab)               -- + an untitled buffer, active
    checkEq "the session records open files in order"
      (map rePath (seFiles (sessionForPersist edSab))) ["/w/a.txt", "/w/b.txt"]
    checkEq "the session records which file is active"
      (seActive (sessionForPersist edSab)) 1
    checkEq "an untitled buffer is not recorded"
      (map rePath (seFiles (sessionForPersist edSabU))) ["/w/a.txt", "/w/b.txt"]
    checkEq "an untitled active buffer records the nearest recorded document"
      (seActive (sessionForPersist edSabU)) 1
    checkEq "the manual's pseudo-path is not recorded"
      (map rePath (seFiles (sessionForPersist (openManual edSab)))) ["/w/a.txt", "/w/b.txt"]
    checkEq "the session records the open folder"
      (seFolder (sessionForPersist (explorerStart "/w" [("/w/a.txt", False, Just 3)] edSab)))
      (Just "/w")
    checkEq "the session records live cursor positions"
      (seFiles (sessionForPersist edSab { edCursor = Pos 1 2 }))
      [RecentEntry "/w/a.txt" 0 0, RecentEntry "/w/b.txt" 1 2]
    -- ...but a cursor move is not a change of *shape*, so it must not make the
    -- driver rewrite the file (the recents' rule, for the recents' reason).
    checkEq "a cursor move is not a session change"
      (sessionShape edSab) (sessionShape edSab { edCursor = Pos 1 2 })
    check "opening a file is a session change"
      (sessionShape edSa /= sessionShape edSab)
    check "switching files is a session change"
      (sessionShape edSab /= sessionShape (switchToFile 0 edSab))
    -- The shape must still be exactly what the persisted session says, or the
    -- driver would decide to rewrite (or not) on a different question from the
    -- one the file answers.
    checkEq "the shape is the persisted session's folder, paths and active index"
      (sessionShape (explorerStart "/w" [("/w/a.txt", False, Just 3)] edSab))
      (let s = sessionForPersist (explorerStart "/w" [("/w/a.txt", False, Just 3)] edSab)
       in (seFolder s, map rePath (seFiles s), seActive s))

    -- 0029: the driver evaluates 'sessionShape' after *every* key batch, so it
    -- must never ask a document where its cursor is. For a table document that
    -- question is 'Csv.cellTextPos', and it used to be forced anyway: the shape
    -- was projected out of a whole 'sessionForPersist', and 'RecentEntry' has
    -- strict fields, so building one evaluated the position the projection was
    -- about to drop. That put a walk over every row above the cursor on each
    -- keystroke -- 390 ms at the last row of a 223 209-row CSV.
    --
    -- Pinned with a grid whose rows are bottom: 'Seq' is spine-strict but
    -- element-lazy, so the view is perfectly well-formed until something
    -- reaches for a row. Nothing structural can see this property; only the
    -- bomb can.
    let boom = error "0029: sessionShape forced a table cursor position"
        vBomb = (Cmedit.Csv.mkCsvView ',' (T.pack "a,b\nc,d\ne,f"))
                  { csvRows = Seq.fromList [Seq.fromList [T.pack "a", T.pack "b"], boom, boom]
                  , csvCurRow = 2, csvCurCol = 1 }
        edBomb = (setLoaded "/w/t.csv" (mkLRs "a,b\nc,d\ne,f") edS0)
                   { edCsv = Just vBomb }
    shapeOk <- try (evaluate (length (concat (let (_, ps, _) = sessionShape edBomb in ps))))
                 :: IO (Either SomeException Int)
    check "0029: sessionShape does not force a table document's cursor position"
      (either (const False) (> 0) shapeOk)
    posBoom <- try (evaluate (length (concatMap rePath
                                (seFiles (sessionForPersist edBomb)))))
                 :: IO (Either SomeException Int)
    check "0029: ...and the bomb is armed (sessionForPersist does force it)"
      (either (const True) (const False) posBoom)

    -- Seeding a restored cursor: clamped, because the file may have shrunk
    -- since the session was written, and reachable in the zipper, because a
    -- restore opens several files before any of them is looked at.
    let edLong = setLoaded "/w/long.txt" (mkLRs "one\ntwo\nthree\n") edS0
    checkEq "a restored cursor is seeded from the session"
      (edCursor (seedSessionPos "/w/long.txt" (Pos 2 3) edLong)) (Pos 2 3)
    checkEq "a restored cursor is clamped to the file as it is now"
      (edCursor (seedSessionPos "/w/long.txt" (Pos 900 900) edLong))
      (clampPos (Pos 900 900) (edBuffer edLong))
    let edZ  = setLoadedNew "/w/other.txt" (mkLRs "x") edLong
        edZS = seedSessionPos "/w/long.txt" (Pos 1 1) edZ
    check "a backgrounded document's cursor is seeded too"
      (any (\d -> docPath d == Just "/w/long.txt" && docCursor d == Pos 1 1)
           (edBefore edZS ++ edAfter edZS))
    let edZN = seedSessionPos "/w/nope.txt" (Pos 1 1) edZ
    check "seeding a path that is not open moves no cursor"
      (edCursor edZN == edCursor edZ
       && map docCursor (edBefore edZN ++ edAfter edZN)
            == map docCursor (edBefore edZ ++ edAfter edZ))

    -- The config key behind the Settings row (row 13, pinned above).
    let (cR, wR) = parseConfigText (T.pack "restore-session = on\n") defaultConfig
    checkEq "restore-session = on parses" (cfgRestoreSession cR) True
    checkEq "restore-session parses without warnings" wR []
    checkEq "restore-session round-trips through updateConfigText"
      (fst (parseConfigText (updateConfigText cR T.empty) defaultConfig)) cR
    checkEq "restore-session is off by default" (cfgRestoreSession defaultConfig) False

  -- Report -------------------------------------------------------------------
  (passed, failed) <- readIORef results
  putStrLn ("Passed " ++ show passed ++ ", failed " ++ show failed)
  if failed == 0 then exitSuccess else exitFailure

-- Helpers --------------------------------------------------------------------

parseBytes :: [Word8] -> IO Key
parseBytes ws = do
  src <- listSource ws
  nextKey src

-- Parse a byte stream where the source hands out at most @k@ bytes at a time.
parseBytesChunked :: Int -> [Word8] -> IO Key
parseBytesChunked k ws = chunkedSource k ws >>= nextKey

-- Parse two keys in a row from one stream (paste, then whatever followed it).
parseTwoChunked :: Int -> [Word8] -> IO (Key, Key)
parseTwoChunked k ws = do
  src <- chunkedSource k ws
  a <- nextKey src
  b <- nextKey src
  pure (a, b)

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
      -- Hand the whole remainder over at once, so the paste path's chunk
      -- scanner is exercised; push-back returns bytes to the front.
      chunk = do
        xs <- readIORef ref
        writeIORef ref []
        pure (BS.pack xs)
      pushBack bs = modifyIORef' ref (BS.unpack bs ++)
  pure (ByteSource next (const next) chunk pushBack)

-- | A source that hands out at most @k@ bytes per chunk, so a paste terminator
-- straddling a chunk boundary is exercised at every offset.
chunkedSource :: Int -> [Word8] -> IO ByteSource
chunkedSource k ws0 = do
  ref <- newIORef ws0
  let next = do
        xs <- readIORef ref
        case xs of
          []       -> pure Nothing
          (b : bs) -> writeIORef ref bs >> pure (Just b)
      chunk = do
        xs <- readIORef ref
        let (a, b) = splitAt (max 1 k) xs
        writeIORef ref b
        pure (BS.pack a)
      pushBack bs = modifyIORef' ref (BS.unpack bs ++)
  pure (ByteSource next (const next) chunk pushBack)

-- | The CSV parser as it stood between plans 0016 and 0026 — a 'Text' parser
-- over the whole file at once — kept as the oracle for the line-cursor parser
-- that replaced it. The engine is shared now ('csvParse' splits and calls
-- 'csvParseLines'), so this is the only thing standing between a rewrite of it
-- and a silent change of dialect.
csvParsePrev :: Char -> T.Text -> Seq.Seq (Seq.Seq T.Text)
csvParsePrev delim = Seq.fromList . rows
  where
    rows t
      | T.null t = []
      | otherwise = let (row, rest, more) = oneRow t
                    in row : if more then rows rest else []
    oneRow t = collect t []
    collect t acc =
      let (f, rest, term) = field t
      in case term of
           0 -> collect rest (f : acc)
           1 -> (Seq.fromList (reverse (f : acc)), rest, not (T.null rest))
           _ -> (Seq.fromList (reverse (f : acc)), rest, False)
    isSep c = c == delim || c == '\n' || c == '\r'
    field t = case T.uncons t of
      Just ('"', cs) -> quoted cs []
      _              -> unquoted t
    unquoted t =
      let (val, rest) = T.break isSep t
      in case T.uncons rest of
           Nothing -> (val, T.empty, 2 :: Int)
           Just (c, cs)
             | c == delim -> (val, cs, 0)
             | c == '\n'  -> (val, cs, 1)
             | otherwise  -> (val, dropLF cs, 1)
    quoted t acc =
      let (seg, rest) = T.break (== '"') t
      in case T.uncons rest of
           Nothing -> (joinSegs (seg : acc), T.empty, 2)
           Just (_, r2) -> case T.uncons r2 of
             Just ('"', r3) -> quoted r3 (T.singleton '"' : seg : acc)
             _              -> close r2 (joinSegs (seg : acc))
    joinSegs [seg] = seg
    joinSegs segs  = T.concat (reverse segs)
    close r val = case T.uncons r of
      Nothing -> (val, T.empty, 2)
      Just (c, cs)
        | c == delim -> (val, cs, 0)
        | c == '\n'  -> (val, cs, 1)
        | c == '\r'  -> (val, dropLF cs, 1)
        | otherwise  -> let (v2, rest2, term2) = unquoted cs
                        in (val <> T.singleton c <> v2, rest2, term2)
    dropLF t = case T.uncons t of
      Just ('\n', cs) -> cs
      _               -> t

-- | 'Cmedit.Csv.cellWidth' as it was before plan 0026 made it a strict fold:
-- the widest of the cell's newline-separated lines, via 'T.splitOn' and
-- 'T.unpack'. Kept as the oracle for the fold.
cellWidthRef :: T.Text -> Int
cellWidthRef = maximum . (0 :) . map lineW . T.splitOn (T.pack "\n")
  where
    lineW = sum . map effW . T.unpack
    effW c | isInvisibleFormat c = 0
           | otherwise           = max 1 (charWidth c)

-- | The CSV parser as it was before plan 0016 replaced it (String-based), kept
-- as an oracle so the Text version can be proved identical on a corpus.
csvParseRef :: Char -> T.Text -> Seq.Seq (Seq.Seq T.Text)
csvParseRef delim = Seq.fromList . map Seq.fromList . rows . T.unpack
  where
    rows [] = []
    rows str = let (row, rest, more) = oneRow str
               in row : if more then rows rest else []
    oneRow str = collect str []
    collect str acc =
      let (f, rest, term) = fld str
      in case term of
           0 -> collect rest (f : acc)
           1 -> (reverse (f : acc), rest, not (null rest))
           _ -> (reverse (f : acc), rest, False)
    fld ('"' : cs) = quoted cs []
    fld cs         = unquoted cs []
    quoted ('"' : '"' : cs) acc = quoted cs ('"' : acc)
    quoted ('"' : cs) acc       = close cs (reverse acc)
    quoted (c : cs) acc         = quoted cs (c : acc)
    quoted [] acc               = (T.pack (reverse acc), [], 2 :: Int)
    close (c : cs) val
      | c == delim = (T.pack val, cs, 0)
      | c == '\n'  = (T.pack val, cs, 1)
      | c == '\r'  = (T.pack val, dropLF cs, 1)
      | otherwise  = unquoted cs (reverse (c : reverse val))
    close [] val = (T.pack val, [], 2)
    unquoted (c : cs) acc
      | c == delim = (T.pack (reverse acc), cs, 0)
      | c == '\n'  = (T.pack (reverse acc), cs, 1)
      | c == '\r'  = (T.pack (reverse acc), dropLF cs, 1)
      | otherwise  = unquoted cs (c : acc)
    unquoted [] acc = (T.pack (reverse acc), [], 2)
    dropLF ('\n' : cs) = cs
    dropLF cs          = cs

-- Two real JPEGs, 32x24, produced by an independent encoder: a baseline
-- grayscale one and a 4:2:0 colour one. The decoder had NO test coverage at
-- all before the performance work of plan 0018 — which meant a decoder rewrite
-- had nothing to check itself against. Expected pixels below were captured
-- from the decoder as it stood before that work.
jpegGray8 :: BS.ByteString
jpegGray8 = BS.pack $
  [ 255,216,255,224,0,16,74,70,73,70,0,1,1,0,0,1
  , 0,1,0,0,255,219,0,67,0,5,3,4,4,4,3,5
  , 4,4,4,5,5,5,6,7,12,8,7,7,7,7,15,11
  , 11,9,12,17,15,18,18,17,15,17,17,19,22,28,23,19
  , 20,26,21,17,17,24,33,24,26,29,29,31,31,31,19,23
  , 34,36,34,30,36,28,30,31,30,255,192,0,11,8,0,24
  , 0,32,1,1,17,0,255,196,0,31,0,0,1,5,1,1
  , 1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4
  , 5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3
  , 3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0
  , 4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50
  , 129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114
  , 130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53
  , 54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85
  , 86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117
  , 118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148
  , 149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178
  , 179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201
  , 202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230
  , 231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,218
  , 0,8,1,1,0,0,63,0,242,202,247,106,241,43,13,19
  , 167,201,250,87,164,255,0,199,191,253,59,249,31,246,203,202
  , 217,255,0,126,124,189,159,101,255,0,166,27,62,203,255,0
  , 46,191,100,255,0,137,31,150,215,187,87,59,97,162,116,249
  , 41,191,241,239,255,0,78,254,71,253,178,242,182,127,223,159
  , 47,103,217,127,233,134,207,178,255,0,203,175,217,63,226,71
  , 229,181,238,213,212,216,232,157,62,74,243,47,248,247,255,0
  , 167,127,35,254,217,121,91,63,239,207,151,179,236,191,244,195
  , 103,217,127,229,215,236,159,241,35,255,217
  ]

jpegColour420 :: BS.ByteString
jpegColour420 = BS.pack $
  [ 255,216,255,224,0,16,74,70,73,70,0,1,1,0,0,1
  , 0,1,0,0,255,219,0,67,0,5,3,4,4,4,3,5
  , 4,4,4,5,5,5,6,7,12,8,7,7,7,7,15,11
  , 11,9,12,17,15,18,18,17,15,17,17,19,22,28,23,19
  , 20,26,21,17,17,24,33,24,26,29,29,31,31,31,19,23
  , 34,36,34,30,36,28,30,31,30,255,219,0,67,1,5,5
  , 5,7,6,7,14,8,8,14,30,20,17,20,30,30,30,30
  , 30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30
  , 30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30
  , 30,30,30,30,30,30,30,30,30,30,30,30,30,30,255,192
  , 0,17,8,0,24,0,32,3,1,34,0,2,17,1,3,17
  , 1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0
  , 0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9
  , 10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5
  , 5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33
  , 49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35
  , 66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23
  , 24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58
  , 67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90
  , 99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122
  , 131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153
  , 154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183
  , 184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213
  , 214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241
  , 242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3
  , 1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1
  , 2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0
  , 2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0
  , 1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19
  , 34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21
  , 98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39
  , 40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73
  , 74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105
  , 106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136
  , 137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166
  , 167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196
  , 197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226
  , 227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249
  , 250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,242
  , 202,247,106,240,154,247,106,252,103,62,255,0,151,127,63,208
  , 244,62,149,95,243,42,255,0,184,255,0,251,132,241,43,13
  , 19,167,201,250,87,164,255,0,199,191,253,59,249,31,246,203
  , 202,217,255,0,126,124,189,159,101,255,0,166,27,62,203,255
  , 0,46,191,100,255,0,137,27,172,52,78,159,37,55,254,61
  , 255,0,233,223,200,255,0,182,94,86,207,251,243,229,236,251
  , 47,253,48,217,246,95,249,117,251,39,252,72,255,0,112,197
  , 99,62,179,47,67,183,55,199,253,114,149,45,118,191,227,99
  , 203,107,221,168,162,191,15,207,191,229,223,207,244,56,190,149
  , 95,243,42,255,0,184,255,0,251,132,234,108,116,78,159,37
  , 121,151,252,123,255,0,211,191,145,255,0,108,188,173,159,247
  , 231,203,217,246,95,250,97,179,236,191,242,235,246,79,248,145
  , 148,87,233,185,77,89,84,149,78,111,47,212,249,252,182,180
  , 234,210,247,186,31,255,217
  ]

-- Undo-stack depth (a local `edUndo` binding in main shadows the selector).
undoDepth :: Editor -> Int
undoDepth = Seq.length . edUndo

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
