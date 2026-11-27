{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | Differential oracle, randomized direct-write parity, cache-hit proof,
-- theme-remap contract, and memory-retention tests for row-level render
-- caching (plan 0042).
--
-- Every interactive transition is rendered with BOTH 'renderEditor' (uncached)
-- and 'renderEditorCached' (threading 'RenderCache' across frames the way
-- 'Cmedit.App.renderNow' does) and checked for bit-for-bit equality of
-- dimensions, cursor, scroll hint, and every cell in the grid.
module RenderCacheSpec (renderCacheSpec) where

import Control.Monad (forM_)
import Data.Array ((!))
import Data.Bits ((.|.))
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Stats (getRTSStats, getRTSStatsEnabled, gc, gcdetails_live_bytes)
import System.Mem (performMajorGC)

import Cmedit.ConfigFile (Config(..), ThemeName(..), defaultConfig)
import Cmedit.Dialog (mkTheme)
import Cmedit.Editor
import Cmedit.Link (urlSpans)
import Cmedit.Lint (Diag(..), LinterId(..), Severity(..))
import qualified Cmedit.Pager as Pg
import qualified Cmedit.Pdf as Pdf
import Cmedit.Render
import qualified Cmedit.Rtf as Rtf
import Cmedit.Syntax (Tok(..), TokRun(..))
import Cmedit.TextBuffer (Encoding(..), LineEnding(..), LoadResult(..), emptyLoadResult, fromText)
import Cmedit.Types
import Cmedit.Width (charWidth, colToDisplay)

renderCacheSpec :: (String -> Bool -> IO ()) -> IO ()
renderCacheSpec check = do
  themeRemapContractTests check
  randomizedDirectWriteVsReference check
  directWriteVsExpandLineCellsOracle check
  cacheHitPointerProof check
  plainTypingAndScrollOracle check
  multilineCommentSyntaxOracle check
  selectionFindBracketOracle check
  themesAndRemapOracle check
  diagnosticsAndOverlaysOracle check
  unicodeWideTabsUrlsOracle check
  wordWrapOracle check
  viewSwitchAndResizeOracle check
  pagerRtfPdfOracle check
  memoryRetentionTests check

------------------------------------------------------------------------------
-- Bit-for-bit oracle harness

screensMatch :: Screen -> Screen -> Either String ()
screensMatch a b
  | scrW a /= scrW b || scrH a /= scrH b =
      Left $ "size mismatch: " ++ show (scrW a, scrH a) ++ " vs " ++ show (scrW b, scrH b)
  | scrCursor a /= scrCursor b =
      Left $ "cursor mismatch: " ++ show (scrCursor a) ++ " vs " ++ show (scrCursor b)
  | scrHint a /= scrHint b =
      Left $ "hint mismatch: " ++ show (scrHint a) ++ " vs " ++ show (scrHint b)
  | otherwise = go 0
  where
    !w = scrW a
    !total = w * scrH a
    !ca = scrCells a
    !cb = scrCells b
    go !i
      | i >= total = Right ()
      | ca ! i == cb ! i = go (i + 1)
      | otherwise =
          let (r, c) = i `divMod` w
          in Left $ "cell mismatch at (" ++ show r ++ "," ++ show c ++ "): oracle="
                    ++ show (ca ! i) ++ " cached=" ++ show (cb ! i)

-- Run a sequence of (label, step) editor transitions starting from @ed0@,
-- threading 'RenderCache' across all steps and comparing every single frame
-- against uncached 'renderEditor'.
runOracle
  :: (String -> Bool -> IO ())
  -> String
  -> Editor
  -> [(String, Editor -> Editor)]
  -> IO (Editor, RenderCache)
runOracle check suite ed0 steps = do
  let e0 = refreshHighlight ed0
      (s0, rc0) = renderEditorCached emptyRenderCache e0
      o0 = renderEditor e0
  case screensMatch o0 s0 of
    Right () -> check (suite ++ ": initial frame") True
    Left err -> check (suite ++ ": initial frame (" ++ err ++ ")") False
  let go !ed !rc [] = pure (ed, rc)
      go !ed !rc ((lbl, f) : rest) = do
        let ed' = refreshHighlight (f ed)
            (sc, rc') = renderEditorCached rc ed'
            so = renderEditor ed'
        case screensMatch so sc of
          Right () -> check (suite ++ ": " ++ lbl) True
          Left err -> check (suite ++ ": " ++ lbl ++ " (" ++ err ++ ")") False
        go ed' rc' rest
  go e0 rc0 steps

key :: Key -> Editor -> Editor
key k e = fst (update k e)

keys :: [Key] -> Editor -> Editor
keys ks e = foldl' (\acc k -> fst (update k acc)) e ks

mkDoc :: (Int, Int) -> Config -> FilePath -> Text -> Editor
mkDoc sz cfg path txt =
  setLoaded path (emptyLoadResult { lrBuffer = fromText txt }) (newEditor sz cfg)

------------------------------------------------------------------------------
-- A. Theme remap contract (M1): every built-in theme remap must be idempotent,
--    pass exact (RGB/256) pairs through unchanged, and always output exact
--    fg/bg colours.

themeRemapContractTests :: (String -> Bool -> IO ()) -> IO ()
themeRemapContractTests check = do
  let isExact ColorRGB{} = True
      isExact Color256{} = True
      isExact _          = False
      allNamed =
        [ Default, Black, Red, Green, Yellow, Blue, Magenta, Cyan, White
        , BrightBlack, BrightRed, BrightGreen, BrightYellow, BrightBlue
        , BrightMagenta, BrightCyan, BrightWhite ]
      sampleExact = [ ColorRGB 12 34 56, ColorRGB 255 255 255, Color256 42, Color256 231 ]
      allCols = allNamed ++ sampleExact
      attrs   = [ attrNone, attrBold, attrItalic .|. attrUndercurl ]
      uls     = [ Default, ColorRGB 200 50 50 ]
      themes  = [ ThemeDark, ThemeLight, ThemeCherryBlossom
                , ThemeFlashbang, ThemeMidnight, ThemeGraphite ]
  forM_ themes $ \tn -> case thRemap (themeFor tn) of
    Nothing -> check ("theme remap contract (" ++ show tn ++ ": no remap)") True
    Just f  -> do
      let th = themeFor tn
          styles = [ StyleU fg bg a ul | fg <- allCols, bg <- allCols, a <- attrs, ul <- uls ]
          allToks =
            [ TkText, TkKeyword, TkType, TkString, TkComment, TkNumber
            , TkFunction, TkBuiltin, TkDecorator, TkTag, TkAttr
            , TkHeading, TkEmph, TkStrong, TkCode, TkLink, TkPunct, TkProperty ]
          textAreaStyles =
            [ cellStyle blankCell, thText th, thGutter th, thGutterCur th
            , thGutterDiag th, thSelection th, thWhitespace th, thFindMatch th
            , thBracket th ] ++ map (thTokens th) allToks
          exactOk = all (\st -> not (isExact (styleFg st) && isExact (styleBg st)) || f st == st) styles
          idemOk = all (\st -> f (f st) == f st) styles
          textExact = all (\st -> let r = f st in isExact (styleFg r) && isExact (styleBg r)) textAreaStyles
      check ("theme remap exact pass-through (" ++ show tn ++ ")") exactOk
      check ("theme remap idempotent (" ++ show tn ++ ")") idemOk
      check ("theme remap emits exact fg/bg on text-area styles (" ++ show tn ++ ")") textExact

------------------------------------------------------------------------------
-- B. Randomized differential oracle (M2): expandLineCellsDirect (which invokes
--    expandLineCellsInto) vs the original list-based expandLineCellsFrom.

-- Minimal deterministic SplitMix64 PRNG (zero external deps).
nextRng :: Word64 -> (Word64, Word64)
nextRng s =
  let !s' = s + 0x9e3779b97f4a7c15
      !z1 = (s' `xorW` (s' `div` 0x40000000)) * 0xbf58476d1ce4e5b9
      !z2 = (z1 `xorW` (z1 `div` 0x8000000))  * 0x94d049bb133111eb
      !z3 = z2 `xorW` (z2 `div` 0x80000000)
  in (z3, s')
  where
    xorW :: Word64 -> Word64 -> Word64
    xorW a b = go 0 1 a b
      where
        go !acc !_ 0 0 = acc
        go !acc !bit x y =
          let bx = x `mod` 2; by = y `mod` 2
              acc' = if bx /= by then acc + bit else acc
          in go acc' (bit * 2) (x `div` 2) (y `div` 2)

rngInt :: Int -> Word64 -> (Int, Word64)
rngInt bound s
  | bound <= 1 = (0, s)
  | otherwise  = let (r, s') = nextRng s in (fromIntegral (r `mod` fromIntegral bound), s')

randomizedDirectWriteVsReference :: (String -> Bool -> IO ()) -> IO ()
randomizedDirectWriteVsReference check = do
  let th = defaultTheme
      alphabet =
        [ 'a', 'z', ' ', '0', ';', '\t'
        , '\x00', '\x01', '\x1b', '\x7f', '\x85'
        , '\x0301', '\x0308'           -- combining marks
        , '\x4e00', '\x65e5'           -- CJK wide
        , '\x200d', '\xfe0f', '\x200b' -- ZWJ, VS16, ZWSP
        , '\x2728', '\x2139'           -- emoji
        ]
      alphaN = length alphabet
      allToks = [ TkText, TkKeyword, TkString, TkComment, TkNumber, TkFunction, TkType, TkPunct ]
      tokN = length allToks
      mkLine !len !st0 = go len st0 []
        where
          go 0 !st acc = (T.pack (reverse acc), st)
          go k !st acc =
            let (pick, st1) = rngInt 10 st
            in if pick == 0 && k >= 10
                 -- Inject a run of > maxComb (10) combining marks!
                 then go (k - 10) st1 (replicate 10 '\x0301' ++ acc)
                 else let (ix, st2) = rngInt alphaN st1
                      in go (k - 1) st2 (alphabet !! ix : acc)
      mkRuns !n !st0 = go 0 st0 []
        where
          go !pos !st acc
            | pos >= n = (reverse acc, st)
            | otherwise =
                let (rl, st1) = rngInt 6 st          -- includes 0-length runs!
                    (ti, st2) = rngInt tokN st1
                    !rlen = min (n - pos) rl
                    !tok  = allToks !! ti
                in go (pos + max 1 rlen) st2 (TokRun rlen tok : acc)
      runsToBaseAt runs i = go 0 runs
        where
          go _ [] = thText th
          go !pos (TokRun rl tk : rs)
            | i < pos + rl = thTokens th tk
            | otherwise    = go (pos + rl) rs
      mkIntervals !n !cnt !st0 = go cnt st0 []
        where
          go 0 !st acc = (reverse acc, st)
          go k !st acc =
            let (a, st1) = rngInt (max 1 (n + 1)) st
                (l, st2) = rngInt 8 st1
            in go (k - 1) st2 ((a, a + l) : acc)
      oneCase :: Int -> Word64 -> (Either String (), Word64)
      oneCase cid st0 =
        let (len, st1)      = rngInt 45 st0
            (fullLine, st2) = mkLine len st1
            (tabIx, st3)    = rngInt 3 st2
            tabw            = [2, 4, 8] !! tabIx
            (wsB, st4)      = rngInt 2 st3
            showWS          = wsB == 1
            (i0, st5)       = rngInt (max 1 (len + 1)) st4
            d0              = colToDisplay tabw i0 fullLine
            subLine         = T.drop i0 fullLine
            (offMin, st6)   = rngInt 8 st5
            (winW, st7)     = rngInt 35 st6
            dMin            = d0 + offMin
            dEnd            = dMin + winW
            (useRunsB, st8) = rngInt 3 st7
            (runs, st9)     = mkRuns len st8
            (baseSpec, baseAt) = case useRunsB of
              0 -> (Right [], const (thText th))
              1 -> (Right runs, runsToBaseAt runs)
              _ -> (Left (\i -> if even i then thTokens th TkKeyword else thText th),
                    \i -> if even i then thTokens th TkKeyword else thText th)
            (hasSelB, st10) = rngInt 2 st9
            (sA, st11)      = rngInt (max 1 (len + 1)) st10
            (sB, st12)      = rngInt (max 1 (len + 1)) st11
            msel            = if hasSelB == 1 then Just (min sA sB, max sA sB) else Nothing
            (eolB, st13)    = rngInt 2 st12
            selEOL          = eolB == 1
            (ovRaw, st14)   = mkIntervals len 3 st13
            overlays        = [ (a, b, thFindMatch th) | (a, b) <- ovRaw ]
            (dgRaw, st15)   = mkIntervals len 2 st14
            diagOver        = [ (a, b, thDiagErr th) | (a, b) <- dgRaw ]
            (lkRaw, st16)   = mkIntervals len 2 st15
            links           = [ (a, b, "https://x.test/" <> T.pack (show k))
                              | (k, (a, b)) <- zip [0 :: Int ..] lkRaw ]
            got = expandLineCellsDirect th dMin dEnd tabw showWS baseSpec
                    (thSelection th) (thWhitespace th) msel selEOL overlays diagOver links
                    i0 d0 subLine
            refPairs = expandLineCellsFrom tabw showWS baseAt
                         (thSelection th) (thWhitespace th) msel selEOL overlays diagOver links
                         i0 d0 subLine
            -- Mirror STArray write semantics: last write at a given display col wins
            -- (identical to sequential writeArray), default blankCell.
            refInWin = [ (d, c) | (d, c) <- refPairs, d >= dMin, d < dEnd ]
            expected = [ fromMaybe blankCell (lastMaybe [ c | (d, c) <- refInWin, d == col ])
                       | col <- [dMin .. dEnd - 1] ]
            res = if got == expected
                    then Right ()
                    else Left $ "seed case #" ++ show cid
                                ++ " (len=" ++ show len ++ " i0=" ++ show i0
                                ++ " d0=" ++ show d0 ++ " dMin=" ++ show dMin
                                ++ " dEnd=" ++ show dEnd ++ " tabw=" ++ show tabw
                                ++ " ws=" ++ show showWS ++ ")"
        in (res, st16)
      lastMaybe [] = Nothing
      lastMaybe xs = Just (last xs)
      runAll !cid !st !fails
        | cid >= 250 = pure fails
        | otherwise  =
            let (r, st') = oneCase cid st
            in case r of
                 Right () -> runAll (cid + 1) st' fails
                 Left msg -> runAll (cid + 1) st' (msg : fails)
  fails <- runAll 0 0x42cafe0042 []
  check ("randomized expandLineCellsDirect vs expandLineCellsFrom (250 cases"
         ++ (if null fails then ")" else ", first fail: " ++ head (reverse fails) ++ ")"))
        (null fails)

------------------------------------------------------------------------------
-- C. Direct-write expandLineCellsInto vs reference expandLineCells oracle (full screen)

directWriteVsExpandLineCellsOracle :: (String -> Bool -> IO ()) -> IO ()
directWriteVsExpandLineCellsOracle check = do
  let th = defaultTheme
      cfg = defaultConfig
        { cfgLineNumbers = False
        , cfgScrollBarV  = False
        , cfgScrollBarH  = False
        , cfgWordWrap    = False
        }
      cases =
        [ ("tabs+ascii",       "\tabc\tdef  g",                      Nothing,        False, False)
        , ("tabs+showWS",      "\tabc\tdef  g",                      Nothing,        False, True)
        , ("wide+comb+ctrl",   T.pack "A\x4e00\x65e5e\x0301\x01Z",   Just (1, 5),    False, False)
        , ("leading-comb",     T.pack "\x0301\x0302xy",              Nothing,        False, False)
        , ("url+selEOL",       "see https://example.com/a/b ok",     Just (0, 30),   True,  False)
        ]
  forM_ cases $ \(nm, ln, msel, selEOL, showWS) ->
    forM_ [0, 1, 2, 3, 5] $ \left -> do
      let bufTxt = if selEOL then ln <> "\nnext" else ln
          ed0 = (mkDoc (10, 30) cfg "/tmp/ref.txt" bufTxt)
                  { edShowWhitespace = showWS
                  , edLeft = left
                  , edSelAnchor = case msel of
                      Nothing -> Nothing
                      Just (s, _) -> Just (Pos 0 s)
                  , edCursor = case msel of
                      Nothing -> Pos 0 0
                      Just (_, e) -> if selEOL then Pos 1 0 else Pos 0 e
                  }
          lo = computeLayout ed0
          tw = loTextWidth lo
          scr = renderEditor ed0
          sr = loTextTop lo
          tl = loTextLeft lo
          got = [ scrCells scr ! (sr * scrW scr + tl + k) | k <- [0 .. tw - 1] ]
          links = [ (s, e, u) | (s, e, u) <- urlSpans ln ]
          refMap = expandLineCells 4 showWS (const (thText th)) (thSelection th) (thWhitespace th)
                     msel selEOL [] [] links ln
          rawRef = [ maybe blankCell id (lookup (left + k) refMap) | k <- [0 .. tw - 1] ]
          sanRef = sanitizeRow rawRef
      check ("directWrite vs expandLineCells (" ++ nm ++ " left=" ++ show left ++ ")")
            (got == sanRef)
  where
    sanitizeRow :: [Cell] -> [Cell]
    sanitizeRow [] = []
    sanitizeRow (c : rest)
      | cellChar c == '\0' = c { cellChar = ' ', cellComb = T.empty } : sanitizeRow rest
      | charWidth (cellChar c) == 2 = case rest of
          (c2 : rest') | cellChar c2 == '\0' -> c : c2 : sanitizeRow rest'
          _                                  -> c { cellChar = ' ', cellComb = T.empty } : sanitizeRow rest
      | otherwise = c : sanitizeRow rest

------------------------------------------------------------------------------
-- D. Cache-hit proof (M3): untouched rows share the exact same Cell pointers
--    across consecutive frames in plain (Dark + Graphite) and word-wrap modes.

cacheHitPointerProof :: (String -> Bool -> IO ()) -> IO ()
cacheHitPointerProof check = do
  let txt = T.unlines
        [ "const a = 1;"
        , "const b = \"untouched cached row with enough text to wrap nicely\";"
        , "const c = 3;"
        ]
      verifyHit lbl cfg = do
        let ed0 = refreshHighlight (mkDoc (14, 30) cfg "/tmp/hit.js" txt)
            (s1, rc1) = renderEditorCached emptyRenderCache ed0
            ed1 = refreshHighlight (key (KChar 'x') ed0) -- modifies line 0 only
            (s2, _rc2) = renderEditorCached rc1 ed1
            lo = computeLayout ed1
            sr = loTextTop lo + 1  -- line 1 is untouched!
            tl = loTextLeft lo
            w  = scrW s1
            allPtrEq = and
              [ let !c1 = scrCells s1 ! (sr * w + c)
                    !c2 = scrCells s2 ! (sr * w + c)
                in ptrEq c1 c2
              | c <- [tl .. tl + 10] ]
        check ("cache hit pointer reuse (" ++ lbl ++ ")") allPtrEq
  verifyHit "plain ThemeDark"      defaultConfig
  verifyHit "plain ThemeGraphite"  defaultConfig { cfgTheme = ThemeGraphite }
  verifyHit "wrap ThemeDark"       defaultConfig { cfgWordWrap = True }
  verifyHit "wrap ThemeGraphite"   defaultConfig { cfgWordWrap = True, cfgTheme = ThemeGraphite }

------------------------------------------------------------------------------
-- 1. Plain-text typing, scrolling, horizontal scrollbar, undo/redo, toggles

plainTypingAndScrollOracle :: (String -> Bool -> IO ()) -> IO ()
plainTypingAndScrollOracle check = do
  let ls = [ T.pack ("const line" ++ show i ++ " = " ++ show (i * 7)
                     ++ "; // " ++ replicate (if i == 15 then 140 else 20) 'x')
           | i <- [1 .. 80 :: Int] ]
      ed0 = mkDoc (20, 60) defaultConfig "/tmp/test.js" (T.unlines ls)
  _ <- runOracle check "plain-edit-scroll" ed0
    [ ("type a", key (KChar 'a'))
    , ("type b", key (KChar 'b'))
    , ("enter splits line", key KEnter)
    , ("backspace rejoins", key KBackspace)
    , ("move down 1", key (KArrow DDown noMods))
    , ("move down 5", keys (replicate 5 (KArrow DDown noMods)))
    , ("scroll down via PageDown", key (KPageDown noMods))
    , ("scroll down 1 line (top changes)", \e -> e { edTop = edTop e + 1 })
    , ("scroll up 1 line", \e -> e { edTop = max 0 (edTop e - 1) })
    , ("move to long line 15 & End (hscroll)", \e ->
        let e1 = e { edTop = 10, edCursor = Pos 14 0 }
        in key (KEnd noMods) e1)
    , ("type at end of long line (widest grows)", keys [KChar '!', KChar '!', KChar '!'])
    , ("backspace at end of long line (widest shrinks)", keys [KBackspace, KBackspace, KBackspace])
    , ("Home resets hscroll", key (KHome noMods))
    , ("undo", key (KCtrlChar 'z'))
    , ("redo", key (KCtrlChar 'y'))
    , ("toggle line numbers off", \e -> e { edShowLineNumbers = False })
    , ("toggle line numbers on",  \e -> e { edShowLineNumbers = True })
    , ("toggle whitespace on",    \e -> e { edShowWhitespace = True })
    , ("toggle whitespace off",   \e -> e { edShowWhitespace = False })
    , ("toggle h-scrollbar off",  \e -> e { edConfig = (edConfig e) { cfgScrollBarH = False } })
    , ("toggle h-scrollbar on",   \e -> e { edConfig = (edConfig e) { cfgScrollBarH = True } })
    , ("toggle v-scrollbar off",  \e -> e { edConfig = (edConfig e) { cfgScrollBarV = False } })
    , ("toggle v-scrollbar on",   \e -> e { edConfig = (edConfig e) { cfgScrollBarV = True } })
    , ("change tab width to 2",   \e -> e { edConfig = (edConfig e) { cfgTabWidth = 2 } })
    , ("change tab width to 8",   \e -> e { edConfig = (edConfig e) { cfgTabWidth = 8 } })
    ]
  pure ()

------------------------------------------------------------------------------
-- 2. Multi-line block comment changing syntax on later unmodified lines

multilineCommentSyntaxOracle :: (String -> Bool -> IO ()) -> IO ()
multilineCommentSyntaxOracle check = do
  let txt = T.unlines
        [ "function demo(x) {"
        , "  const a = 42;"
        , "  const s = \"hello\";"
        , "  return a + x;"
        , "}"
        ]
      ed0 = mkDoc (15, 60) defaultConfig "/tmp/comment.js" txt
  _ <- runOracle check "multiline-comment" ed0
    [ ("type / at start of line 0", key (KChar '/'))
    , ("type * opens block comment over lines 1..4", key (KChar '*'))
    , ("move down into commented line 2", keys [KArrow DDown noMods, KArrow DDown noMods])
    , ("insert */ at line 2 closes comment for lines 3..4", keys [KChar '*', KChar '/'])
    , ("undo removes */", key (KCtrlChar 'z'))
    , ("go back to top and remove /*", \e ->
        let e1 = e { edCursor = Pos 0 2 }
        in keys [KBackspace, KBackspace] e1)
    ]
  pure ()

------------------------------------------------------------------------------
-- 3. Selection (single + multi-line selEOL), live Find/Replace, brackets

selectionFindBracketOracle :: (String -> Bool -> IO ()) -> IO ()
selectionFindBracketOracle check = do
  let txt = T.unlines
        [ "function foo(alpha, beta) {"
        , "  if (alpha > 0) {"
        , "    return foo(alpha - 1, beta);"
        , "  }"
        , "  return beta;"
        , "}"
        ]
      ed0 = mkDoc (18, 60) defaultConfig "/tmp/sel.js" txt
  _ <- runOracle check "sel-find-bracket" ed0
    [ ("select right 5 chars", keys (replicate 5 (KArrow DRight shiftOnly)))
    , ("extend selection down 2 lines (multi-line selEOL)", keys (replicate 2 (KArrow DDown shiftOnly)))
    , ("shrink selection up 1 line", key (KArrow DUp shiftOnly))
    , ("clear selection with Right", key (KArrow DRight noMods))
    , ("select all (Ctrl+A)", key (KCtrlChar 'a'))
    , ("clear select all", key (KHome noMods))
    , ("move onto opening ( at (0,12)", \e -> e { edCursor = Pos 0 12 })
    , ("move onto opening { at (0,26) -> matches line 5 }", \e -> e { edCursor = Pos 0 26 })
    , ("move onto inner { at (1,17) -> matches line 3 }", \e -> e { edCursor = Pos 1 17 })
    , ("move off bracket", \e -> e { edCursor = Pos 2 4 })
    , ("open Find dialog (Ctrl+F)", key (KCtrlChar 'f'))
    , ("type 'alpha' in Find", keys [KChar 'a', KChar 'l', KChar 'p', KChar 'h', KChar 'a'])
    , ("edit Find term to 'beta'", keys (replicate 5 KBackspace ++ [KChar 'b', KChar 'e', KChar 't', KChar 'a']))
    , ("close Find (Esc)", key KEsc)
    , ("open Replace dialog (Ctrl+R)", key (KCtrlChar 'r'))
    , ("type 'return' in Replace Find field", keys (map KChar "return"))
    , ("close Replace (Esc)", key KEsc)
    ]
  pure ()

------------------------------------------------------------------------------
-- 4. All 6 themes + theme-picker live preview (L5)

themesAndRemapOracle :: (String -> Bool -> IO ()) -> IO ()
themesAndRemapOracle check = do
  let txt = T.unlines
        [ "// comment with https://example.com/docs"
        , "const greeting = \"hello\";"
        , "function add(a, b) {"
        , "  return a + b;"
        , "}"
        ]
      setTheme t e = e { edConfig = (edConfig e) { cfgTheme = t } }
      ed0 = mkDoc (18, 70) defaultConfig "/tmp/themes.js" txt
  _ <- runOracle check "themes-remap" ed0
    [ ("switch to ThemeGraphite", setTheme ThemeGraphite)
    , ("type in ThemeGraphite (cache hits on lines 1..4)", key (KChar ' '))
    , ("move onto bracket in ThemeGraphite", \e -> e { edCursor = Pos 2 19 })
    , ("select across lines in ThemeGraphite", keys [KArrow DDown shiftOnly, KArrow DRight shiftOnly])
    , ("open Find in ThemeGraphite", keys [KCtrlChar 'f', KChar 'a'])
    , ("close Find in ThemeGraphite", key KEsc)
    , ("switch to ThemeMidnight", setTheme ThemeMidnight)
    , ("type in ThemeMidnight", key (KChar 'x'))
    , ("switch to ThemeCherryBlossom", setTheme ThemeCherryBlossom)
    , ("move down in ThemeCherryBlossom", key (KArrow DDown noMods))
    , ("switch to ThemeFlashbang", setTheme ThemeFlashbang)
    , ("type in ThemeFlashbang", key (KChar 'y'))
    , ("switch to ThemeLight", setTheme ThemeLight)
    , ("type in ThemeLight", key (KChar 'z'))
    , ("switch back to ThemeDark", setTheme ThemeDark)
    , ("open Theme picker dialog (live preview)", \e ->
        e { edDialog = Just (mkTheme 0), edFocus = FDialog })
    , ("cycle theme preview right x3 (CherryBlossom)", keys (replicate 3 (KArrow DRight noMods)))
    , ("cycle theme preview right x2 (Graphite)", keys (replicate 2 (KArrow DRight noMods)))
    , ("cancel Theme picker (restores Dark)", key KEsc)
    , ("menu open over ThemeGraphite", \e -> key (KFn 10 noMods) (setTheme ThemeGraphite e))
    , ("menu close over ThemeGraphite", key KEsc)
    ]
  pure ()

------------------------------------------------------------------------------
-- 5. Diagnostics & UI overlays (menus, explorer wider than window, quick-open)

diagnosticsAndOverlaysOracle :: (String -> Bool -> IO ()) -> IO ()
diagnosticsAndOverlaysOracle check = do
  let txt = T.unlines
        [ "x = 1"
        , "y = undefined_var + 2"
        , "z = x + y"
        , "print(z)"
        ]
      dErr  = Diag 1 4 SevError   "F821" "undefined name"  LRuff
      dWarn = Diag 2 0 SevWarning "F841" "unused variable" LRuff
      dInfo = Diag 3 0 SevInfo    "I001" "info note"       LRuff
      ed0   = mkDoc (18, 70) defaultConfig { cfgTheme = ThemeGraphite } "/tmp/diag.py" txt
      expEntries = [ ("/w/a.py", False, Just 10), ("/w/sub", True, Nothing) ]
  _ <- runOracle check "diags-overlays" ed0
    [ ("diagnostics arrive", \e -> e { edDiags = [dErr, dWarn, dInfo] })
    , ("cursor moves to diagnostic line", \e -> e { edCursor = Pos 1 5 })
    , ("type on line 0 while diags sit on lines 1..3", \e -> key (KChar '#') (e { edCursor = Pos 0 0 }))
    , ("update diagnostic severity", \e -> e { edDiags = [dErr { dgSev = SevWarning }] })
    , ("clear diagnostics", \e -> e { edDiags = [] })
    , ("open explorer sidebar", explorerStart "/w" expEntries)
    , ("explorer wider than window (L5)", \e -> e { edExpWidth = 80, edSize = (16, 30) })
    , ("restore normal size after wide explorer", \e -> e { edExpWidth = 24, edSize = (18, 70) })
    , ("collapse explorer sidebar", \e -> e { edExpCollapsed = True })
    , ("expand explorer sidebar", \e -> e { edExpCollapsed = False })
    , ("open top menu bar", key (KFn 10 noMods))
    , ("close menu bar", key KEsc)
    , ("open QuickOpen", key (KCtrlChar 'p'))
    , ("close QuickOpen", key KEsc)
    ]
  pure ()

------------------------------------------------------------------------------
-- 6. Tricky Unicode: CJK wide glyphs, combining marks, tabs, controls, URLs

unicodeWideTabsUrlsOracle :: (String -> Bool -> IO ()) -> IO ()
unicodeWideTabsUrlsOracle check = do
  let stacked = T.pack ("e" ++ replicate 12 '\x0301' ++ " end")
      txt = T.unlines
        [ "\tindented\twith\ttabs and spaces"
        , "日本語とASCII混在 https://example.com/path?a=1&b=2 末尾"
        , T.pack "\x0301leading combining mark and e\x0301 acute"
        , stacked
        , T.pack "ctrl:\x01\x07\x1b invis:\x200b\xfeff emoji:\x2728\x2139\xfe0f ok"
        , "plain tail line"
        ]
      ed0 = mkDoc (16, 40) defaultConfig "/tmp/uni.txt" txt
  _ <- runOracle check "unicode-wide-tabs-urls" ed0
    [ ("showWhitespace = True", \e -> e { edShowWhitespace = True })
    , ("hscroll left=1 (splits tab/wide)", \e -> e { edLeft = 1 })
    , ("hscroll left=2 (splits CJK wide)", \e -> e { edLeft = 2 })
    , ("hscroll left=3", \e -> e { edLeft = 3 })
    , ("hscroll left=10 (mid-URL)", \e -> e { edLeft = 10 })
    , ("hscroll left=20 (mid-URL)", \e -> e { edLeft = 20 })
    , ("hscroll left=0 + showWhitespace=False", \e -> e { edLeft = 0, edShowWhitespace = False })
    , ("select across CJK & combining lines", \e ->
        e { edSelAnchor = Just (Pos 1 2), edCursor = Pos 3 4 })
    , ("ThemeCherryBlossom on unicode+selection", \e ->
        e { edConfig = (edConfig e) { cfgTheme = ThemeCherryBlossom } })
    , ("type CJK char into cached line", \e ->
        key (KChar '漢') (e { edSelAnchor = Nothing, edCursor = Pos 1 0 }))
    ]
  pure ()

------------------------------------------------------------------------------
-- 7. Word-wrapped mode (Phase 2), including bottom-clipped cwrComplete=False,
--    chrome height toggles (L5), and v-scrollbar off (L5).

wordWrapOracle :: (String -> Bool -> IO ()) -> IO ()
wordWrapOracle check = do
  let longA = T.pack (concat (replicate 8 "alpha beta gamma delta "))
      longB = T.pack ("\t日本語 " ++ concat (replicate 7 "https://example.org/x foo(bar) "))
      ls    = [ "short 0", "short 1", "short 2", "short 3", "short 4"
              , longA
              , longB
              , "tail 7", "tail 8"
              ]
      cfg   = defaultConfig { cfgWordWrap = True }
      -- Small height (12 rows -> ~9 text rows) so longA/longB clip at the bottom!
      ed0   = mkDoc (12, 32) cfg "/tmp/wrap.js" (T.unlines ls)
  _ <- runOracle check "word-wrap" ed0
    [ ("type on short 0", key (KChar '!'))
    , ("scroll down so longA is bottom-clipped (cwrComplete=False)", \e -> e { edTop = 2 })
    , ("hide menu+status+hints at same edSize (textHeight grows -> must rebuild clipped longA)", \e ->
        e { edShowMenu = False, edShowStatus = False, edShowHints = False })
    , ("restore menu+status+hints", \e ->
        e { edShowMenu = True, edShowStatus = True, edShowHints = True })
    , ("scroll down 1 more -> exposes rest of longA (must rebuild!)", \e -> e { edTop = 3 })
    , ("scroll down 1 more -> longB bottom-clipped", \e -> e { edTop = 4 })
    , ("scroll down 1 more -> exposes rest of longB", \e -> e { edTop = 5 })
    , ("scroll down into longB", \e -> e { edTop = 6 })
    , ("select multi-line across wrapped lines (selEOL)", \e ->
        e { edTop = 5, edSelAnchor = Just (Pos 5 10), edCursor = Pos 6 15 })
    , ("wrap with vertical scrollbar off (L5)", \e ->
        e { edConfig = (edConfig e) { cfgScrollBarV = False } })
    , ("wrap with vertical scrollbar back on", \e ->
        e { edConfig = (edConfig e) { cfgScrollBarV = True } })
    , ("clear selection & edit inside wrapped line", \e ->
        key (KChar 'Z') (e { edSelAnchor = Nothing, edCursor = Pos 5 5 }))
    , ("toggle showWhitespace in wrap mode", \e -> e { edShowWhitespace = True })
    , ("switch to ThemeGraphite in wrap mode", \e ->
        e { edConfig = (edConfig e) { cfgTheme = ThemeGraphite } })
    , ("type in ThemeGraphite wrap mode", key (KChar 'W'))
    , ("open Find in wrap mode", keys [KCtrlChar 'f', KChar 'b', KChar 'e', KChar 't', KChar 'a'])
    , ("close Find in wrap mode", key KEsc)
    , ("resize narrower in wrap mode", \e -> e { edSize = (12, 20) })
    , ("toggle wrap off then back on", \e -> e { edWordWrap = False })
    , ("toggle wrap back on", \e -> e { edWordWrap = True })
    ]
  pure ()

------------------------------------------------------------------------------
-- 8. View switches (CSV, multiple files) & tiny terminal sizes

viewSwitchAndResizeOracle :: (String -> Bool -> IO ()) -> IO ()
viewSwitchAndResizeOracle check = do
  let csvTxt = T.unlines [ "a,b,c", "1,2,3", "4,5,6" ]
      ed0    = mkDoc (18, 60) defaultConfig "/tmp/data.csv" csvTxt
  _ <- runOracle check "view-switch-resize" ed0
    [ ("toggle CSV table -> text (Alt+T)", key (KAltChar 't'))
    , ("type in CSV text view", key (KChar '9'))
    , ("toggle text -> CSV table (Alt+T)", key (KAltChar 't'))
    , ("toggle CSV table -> text again", key (KAltChar 't'))
    , ("open second file", setLoadedNew "/tmp/second.hs"
        (emptyLoadResult { lrBuffer = fromText "main :: IO ()\nmain = putStrLn \"hi\"\n" }))
    , ("type in second file", key (KChar ' '))
    , ("resize to tiny 5x10", \e -> e { edSize = (5, 10) })
    , ("resize to 1x1", \e -> e { edSize = (1, 1) })
    , ("resize back to 24x80", \e -> e { edSize = (24, 80) })
    ]
  pure ()

------------------------------------------------------------------------------
-- 9. Pager, RTF, and PDF frames (L5: direct-write parity + cache drop)

pagerRtfPdfOracle :: (String -> Bool -> IO ()) -> IO ()
pagerRtfPdfOracle check = do
  let pg0 = Pg.pagerFilled 0 (Seq.fromList ["const a = 1;", "const b = \"hi\";", "\ttabbed\tline"])
              (Pg.mkPagerDoc "/tmp/big.js" 1024 3 [0] LF Utf8)
      rtfDoc = Rtf.rtfRelayout 4 50 12
                 (Rtf.mkRtfDoc 0 (Seq.fromList ["{\\rtf1\\ansi {\\b Bold} and {\\i italic} https://example.com\\par Second line}"]))
      pdfLines = Seq.fromList
        [ Pdf.PdfLine 2 "PDF Heading" [(0, 11, Pdf.defaultPdfFmt { Pdf.pfBold = True, Pdf.pfSize = 18 })] False 1
        , Pdf.PdfLine 0 "" [] True 1
        , Pdf.PdfLine 0 "Body text with https://example.org/p" [] False 2
        ]
      pdfDoc = Pdf.PdfDoc
        { Pdf.pdPages  = Seq.empty
        , Pdf.pdTop    = 0
        , Pdf.pdCache  = Just (50, 4, pdfLines, Seq.fromList [0, 2])
        , Pdf.pdCaret  = origin
        , Pdf.pdAnchor = Just (Pos 0 4)
        , Pdf.pdTitle  = "Test"
        , Pdf.pdVer    = "1.4"
        , Pdf.pdNote   = ""
        , Pdf.pdSource = error "unused in reflow render"
        }
      ed0 = mkDoc (16, 60) defaultConfig "/tmp/start.txt" "plain text\nline 2\n"
  _ <- runOracle check "pager-rtf-pdf" ed0
    [ ("enter Pager view", \e -> e { edPager = Just pg0, edPath = Just "/tmp/big.js" })
    , ("scroll Pager right", \e -> e { edPager = Just pg0 { Pg.pgLeft = 3, Pg.pgCursor = 1 } })
    , ("enter RTF view", \e -> e { edPager = Nothing, edRtf = Just rtfDoc })
    , ("enter PDF view", \e -> e { edRtf = Nothing, edPdf = Just pdfDoc })
    , ("return to plain text", \e -> e { edPdf = Nothing, edPath = Just "/tmp/start.txt" })
    ]
  pure ()

------------------------------------------------------------------------------
-- 10. Memory retention guards (H1):
--     (a) Switching away from a 7.5 MB buffer drops all cached rows.
--     (b) Reverting/reloading identical text into a fresh buffer on the SAME
--         edDocId re-anchors crLine/crToks so the old decoded file is NOT
--         pinned alongside the new one.

memoryRetentionTests :: (String -> Bool -> IO ()) -> IO ()
memoryRetentionTests check = do
  rtsOn <- getRTSStatsEnabled
  if not rtsOn
    then check "render cache memory retention (RTS -T disabled, skipped)" True
    else do
      -- (a) Switching to a tiny buffer drops the 7.5 MB buffer.
      baseMB <- liveMB
      let nLines = 30000
          lineW  = 250
          big    = T.unlines [ T.pack (show i) <> T.replicate lineW "x" | i <- [1 .. nLines :: Int] ]
          edBig  = refreshHighlight $
            mkDoc (40, 120) defaultConfig "/tmp/big.txt" big
          (sBig, rc1) = renderEditorCached emptyRenderCache edBig
      _ <- pure $! (scrW sBig + fromEnum (cellChar (scrCells sBig ! 0)))
      let edSmall = refreshHighlight $
            mkDoc (40, 120) defaultConfig "/tmp/small.txt" "hello\n"
          (sSmall, rc2) = renderEditorCached rc1 edSmall
      _ <- pure $! (scrW sSmall + fromEnum (cellChar (scrCells sSmall ! 0)))
      afterMB <- liveMB
      let retained = afterMB - baseMB
          (sCheck, _) = renderEditorCached rc2 edSmall
          !alive = scrW sCheck + scrH sSmall
      check ("render cache drops closed 7.5 MB buffer (retained "
             ++ show (round (retained * 10) `div` (10 :: Int)) ++ " MB, keepalive="
             ++ show alive ++ ")")
            (retained < 3.0)

      -- (b) Revert/reload same content into a fresh buffer (same edDocId,
      --     different Text backing array via T.copy): cache hits without
      --     pinning the previous buffer's decoded text (H1).
      base2 <- liveMB
      let mkBigDoc copyFlag =
            let raw = T.unlines [ T.pack (show i) <> T.replicate lineW "y" | i <- [1 .. nLines :: Int] ]
                txt = if copyFlag then T.copy raw else raw
            in refreshHighlight (mkDoc (40, 120) defaultConfig "/tmp/revert.txt" txt)
          edRev1 = mkBigDoc False
          (sRev1, rcRev1) = renderEditorCached emptyRenderCache edRev1
      _ <- pure $! (scrW sRev1 + fromEnum (cellChar (scrCells sRev1 ! 0)))
      oneCopyMB <- liveMB
      -- Reload identical text into a fresh backing array (same edDocId = 0).
      let edRev2 = (mkBigDoc True) { edDocId = edDocId edRev1 }
          (sRev2, rcRev2) = renderEditorCached rcRev1 edRev2
      _ <- pure $! (scrW sRev2 + fromEnum (cellChar (scrCells sRev2 ! 0)))
      -- Only edRev2, sRev2, and rcRev2 remain reachable here; edRev1 must be GC'd!
      afterRevMB <- liveMB
      let oneCopySize = max 1.0 (oneCopyMB - base2)
          afterRevDelta = afterRevMB - base2
          (sRevCheck, _) = renderEditorCached rcRev2 edRev2
          !alive2 = scrW sRevCheck + scrH sRev2
      -- If edRev1 were pinned, afterRevDelta would be ~2 * oneCopySize (~16 MB).
      -- With H1 fixed, it stays ~1 * oneCopySize (~8-9 MB).
      check ("render cache Revert/reload does not pin old buffer (1-copy="
             ++ show (round oneCopySize :: Int) ++ " MB, after-reload="
             ++ show (round afterRevDelta :: Int) ++ " MB, keepalive="
             ++ show alive2 ++ ")")
            (afterRevDelta < oneCopySize * 1.45)

liveMB :: IO Double
liveMB = do
  performMajorGC
  s <- getRTSStats
  pure (fromIntegral (gcdetails_live_bytes (gc s)) / 1048576)
