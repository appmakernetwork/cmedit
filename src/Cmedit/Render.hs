-- | Rendering. A pure function turns the 'Editor' into a 'Screen' (a grid of
-- styled cells), and a diff against the previously displayed screen produces
-- the minimal stream of escape sequences to update the terminal. Wide glyphs
-- occupy a cell plus a continuation sentinel so the diff keeps columns aligned.
module Cmedit.Render
  ( Screen(..)
  , ScrollHint(..)
  , renderEditor
  , renderFrame
  , scrollPlan
  , refreshHighlight
  , Theme(..)
  , defaultTheme
  , lightTheme
  , cherryBlossomTheme
  , themeFor
  , FileKind(..)
  , fileKind
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.|.))
import Data.Array (Array, (!), listArray)
import Data.Array.MArray (freeze, newArray, readArray, writeArray)
import Data.Array.ST (STArray)
import Data.ByteString.Builder (Builder, charUtf8, intDec, string7)
import qualified Data.Map.Strict as Map
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Cmedit.About (aboutFrameCells)
import Cmedit.HelpCard (helpFrameCells)
import Cmedit.Ansi
import Cmedit.Browser (Browser(..), FileNode(..))
import qualified Cmedit.Browser as Br
import Cmedit.Search
  ( SearchState(..), SField(..), SearchField(..), FileResult(..), Match(..)
  , SRow(..), HLine(..) )
import qualified Cmedit.Search as S
import qualified Data.Sequence as Seq
import Cmedit.Csv (CsvView(..))
import Cmedit.Link (filePathUri, urlSpans)
import qualified Cmedit.Csv as Csv
import Cmedit.ConfigFile (ThemeName(..))
import Cmedit.Definition (DefPick(..), DefItem(..))
import Cmedit.QuickOpen (QuickOpen(..))
import qualified Cmedit.QuickOpen as Q
import Cmedit.Dialog
import Cmedit.Editor
import System.FilePath (makeRelative, takeExtension)
import Cmedit.Menu
import Cmedit.Syntax
import Cmedit.TextBuffer
import Cmedit.Types
import Cmedit.Image (Image(..), ImgMode(..), renderImage)
import Cmedit.Width (charWidth, colToDisplay, controlCaret, isControlChar, windowStart)

-- | A rendered frame: a flat row-major grid plus the desired cursor position.
-- 'scrHint' describes where this frame's scrollable band sat, so the diff can
-- recognise two consecutive frames as "the same content, shifted" and emit a
-- hardware scroll instead of repainting the band ('scrollPlan').
data Screen = Screen
  { scrW      :: !Int
  , scrH      :: !Int
  , scrCells  :: !(Array Int Cell)
  , scrCursor :: !(Maybe (Int, Int))
  , scrHint   :: !(Maybe ScrollHint)
  }

-- | The scroll identity of a frame's text band: candidate frames must agree
-- on geometry and 'shKey' (which folds in the document index and horizontal
-- offsets); the difference of their 'shPos' is the candidate shift. The hint
-- is only ever a /candidate/ — 'scrollPlan' verifies against the actual cell
-- content before any scroll escape is emitted, so a wrong or colliding hint
-- costs nothing.
data ScrollHint = ScrollHint
  { shTop    :: !Int   -- ^ First row of the scrollable band.
  , shHeight :: !Int   -- ^ Band height in rows.
  , shKey    :: !Int   -- ^ Identity of the content stream (doc, h-scroll, layout).
  , shPos    :: !Int   -- ^ Scroll position in rows (e.g. the top buffer line).
  } deriving (Eq, Show)

-- Continuation sentinel placed in the trailing cell(s) of a wide glyph.
contChar :: Char
contChar = '\0'

------------------------------------------------------------------------------
-- Theme

data Theme = Theme
  { thText         :: !Style
  , thGutter       :: !Style
  , thGutterCur    :: !Style
  , thMenuBar      :: !Style
  , thMenuActive   :: !Style
  , thMenuItem     :: !Style
  , thMenuSel      :: !Style
  , thMenuAccel    :: !Style
  , thStatus       :: !Style
  , thStatusKey    :: !Style
  , thHint         :: !Style
  , thHintKey      :: !Style
  , thSelection    :: !Style
  , thDialog       :: !Style
  , thDialogTitle  :: !Style
  , thField        :: !Style
  , thFieldFocus   :: !Style
  , thButton       :: !Style
  , thButtonFocus  :: !Style
  , thWhitespace   :: !Style
  , thFindMatch    :: !Style   -- ^ Every-match highlight while the Find dialog is open.
  , thBracket      :: !Style   -- ^ The bracket pair enclosing/under the cursor.
  , thScrollbar    :: !Style   -- ^ The right-edge scrollbar track and thumb.
  , thTokens       :: !(Tok -> Style) -- ^ Syntax-token palette (differs between dark and light).
  , thRemap        :: !(Maybe (Style -> Style))
    -- ^ Applied to every finished cell before the frame is frozen. Themes
    -- that paint their own background (Cherry Blossom) use this to remap the
    -- 16 ANSI colours — including the hardcoded styles in views like the
    -- explorer, search and quick-open — onto their palette, so no cell ever
    -- falls through to the terminal's own colours. 'Nothing' skips the pass.
  }

defaultTheme :: Theme
defaultTheme = Theme
  { thText        = Style Default Default attrNone
  , thGutter      = Style BrightBlack Default attrNone
  , thGutterCur   = Style BrightYellow Default attrNone
  , thMenuBar     = Style BrightWhite Blue attrNone
  , thMenuActive  = Style Black White attrNone
  , thMenuItem    = Style Black White attrNone
  , thMenuSel     = Style BrightWhite Blue attrNone
  , thMenuAccel   = Style BrightBlack White attrNone
  , thStatus      = Style BrightWhite Blue attrNone
  , thStatusKey   = Style BrightYellow Blue attrBold
  , thHint        = Style Default Default attrNone
  , thHintKey     = Style Black Cyan attrNone
  , thSelection   = Style BrightWhite Blue attrNone
  , thDialog      = Style Black White attrNone
  , thDialogTitle = Style BrightWhite Blue attrBold
  , thField       = Style Black BrightWhite attrNone
  , thFieldFocus  = Style BrightWhite Blue attrNone
  , thButton      = Style Black White attrNone
  , thButtonFocus = Style BrightWhite Blue attrBold
  , thWhitespace  = Style BrightBlack Default attrNone
  , thFindMatch   = Style Black Yellow attrNone
    -- Undercurl where the terminal has it ('rcUndercurl'); the emitter falls
    -- back to the plain underline this always used elsewhere.
  , thBracket     = Style BrightCyan Default (attrBold .|. attrUndercurl)
  , thScrollbar   = Style BrightBlack Default attrNone
  , thTokens      = darkTokens
  , thRemap       = Nothing
  }

-- | The light variant: chrome (menus, dialogs, status) is already explicit
-- black-on-white/blue and carries over; what changes are the colours that sit
-- on the terminal's own background — syntax tokens, the current-line gutter
-- number and the bracket highlight — which trade the bright-on-dark hues for
-- ones readable on white.
lightTheme :: Theme
lightTheme = defaultTheme
  { thGutterCur = Style Blue Default attrBold
  , thBracket   = Style Blue Default (attrBold .|. attrUndercurl)
  , thTokens    = lightTokens
  }

-- | Cherry Blossom: a light 24-bit theme with (soft pinks with lilac/purple accents)
-- toned down for day-long work:
-- a near-white pink page, dark plum-grey text, and muted accents that keep
-- readable contrast on the tinted ground. The chrome fields below are set in
-- explicit RGB, and 'cherryRemap' catches everything else, so every cell
-- carries its own colours and the terminal palette never shows through.
cherryBlossomTheme :: Theme
cherryBlossomTheme = Theme
  { thText        = txt
  , thGutter      = Style (rgb 184 143 174) cbBase attrNone
  , thGutterCur   = Style cbRaspberry cbBase attrBold
  , thMenuBar     = Style cbInk cbBarPink attrNone
  , thMenuActive  = Style cbInkDeep cbLilac attrNone
  , thMenuItem    = Style cbInk cbDropPink attrNone
  , thMenuSel     = Style cbInkDeep cbLilac attrNone
  , thMenuAccel   = Style (rgb 147 100 139) cbDropPink attrNone
  , thStatus      = Style cbInk cbBarPink attrNone
  , thStatusKey   = Style (rgb 122 21 96) cbBarPink attrBold
  , thHint        = Style (rgb 92 67 86) cbBase attrNone
  , thHintKey     = Style cbInkDeep (rgb 242 153 231) attrNone
  , thSelection   = Style cbInkDeep cbSelLav attrNone
  , thDialog      = Style cbInk cbDlgPink attrNone
  , thDialogTitle = Style cbInkDeep cbLilac attrBold
  , thField       = Style cbInk (rgb 255 255 255) attrNone
  , thFieldFocus  = Style cbInkDeep cbSelLav attrNone
  , thButton      = Style cbInk cbDropPink attrNone
  , thButtonFocus = Style (rgb 255 255 255) cbPurple attrBold
  , thWhitespace  = Style (rgb 224 179 212) cbBase attrNone
  , thFindMatch   = Style cbInkDeep (rgb 247 208 112) attrNone
  , thBracket     = Style cbRaspberry cbBase (attrBold .|. attrUndercurl)
  , thScrollbar   = Style (rgb 216 168 207) cbBase attrNone
  , thTokens      = cherryTokens
  , thRemap       = Just cherryRemap
  }
  where
    rgb = ColorRGB
    txt = Style cbText cbBase attrNone

-- The Cherry Blossom palette. The pinks are: navbar #ffb6f6,
-- dropdown #ffdcfc, modal #ffe9fc, active-gradient lilac #d29cf1, primary
-- purple #6d67c5; page and inks are toned for contrast on the pink ground.
cbBase, cbText, cbInk, cbInkDeep, cbBarPink, cbDropPink, cbDlgPink,
  cbLilac, cbSelLav, cbPurple, cbRaspberry :: Color
cbBase      = ColorRGB 255 240 250   -- page: barely-pink white
cbText      = ColorRGB 45 37 48      -- body text: dark warm grey
cbInk       = ColorRGB 58 36 55      -- chrome text: dark plum-grey
cbInkDeep   = ColorRGB 42 22 51
cbBarPink   = ColorRGB 255 182 246   -- menu/status bars
cbDropPink  = ColorRGB 255 220 252   -- dropdowns, buttons
cbDlgPink   = ColorRGB 255 233 252   -- dialogs
cbLilac     = ColorRGB 210 156 241   -- highlighted menu items, titles
cbSelLav    = ColorRGB 220 194 245   -- text selection
cbPurple    = ColorRGB 109 103 197   -- strong accent (focused button)
cbRaspberry = ColorRGB 163 18 95

-- Remap one finished cell style onto the Cherry Blossom palette. Cells the
-- theme fields already coloured are pure RGB and pass through unchanged; what
-- this really handles are the ANSI-named styles hardcoded in the explorer,
-- search view, quick-open, CSV table, browser and About wordmark. Named
-- colours never reach the terminal, so the forced background stays intact.
cherryRemap :: Style -> Style
cherryRemap (Style fg bg at) = Style fg' (mapBg bg) at
  where
    -- Blue and Black backgrounds are the dark chrome (selections, table
    -- headers); they keep light foregrounds. Everything else sits on a light
    -- pink ground and gets the dark on-base palette.
    fg' = if bg == Blue || bg == Black then lightFg fg else darkFg fg
    mapBg c = case c of
      Default     -> cbBase
      Black       -> cbInk
      Blue        -> cbPurple
      Cyan        -> cbLilac
      White       -> cbDropPink
      BrightWhite -> ColorRGB 255 255 255
      BrightBlack -> ColorRGB 185 162 180      -- unfocused selection
      Yellow      -> ColorRGB 247 208 112      -- find-match amber
      Green       -> ColorRGB 182 236 205      -- mint
      Red         -> ColorRGB 232 160 164
      Magenta     -> ColorRGB 242 153 231
      other       -> other                     -- RGB / 256 pass through
    -- Light tints for dark (purple) chrome.
    lightFg c = case c of
      BrightWhite   -> ColorRGB 255 255 255
      White         -> ColorRGB 243 227 240
      Yellow        -> ColorRGB 255 227 179
      BrightYellow  -> ColorRGB 255 227 179
      BrightBlack   -> ColorRGB 217 198 230
      Green         -> ColorRGB 196 240 216
      BrightGreen   -> ColorRGB 196 240 216
      Red           -> ColorRGB 255 196 200
      BrightRed     -> ColorRGB 255 196 200
      Cyan          -> ColorRGB 201 232 242
      BrightCyan    -> ColorRGB 201 232 242
      Magenta       -> ColorRGB 247 201 239
      BrightMagenta -> ColorRGB 247 201 239
      Blue          -> ColorRGB 212 208 245
      BrightBlue    -> ColorRGB 212 208 245
      ColorRGB{}    -> c
      Color256{}    -> c
      _             -> ColorRGB 255 255 255    -- Default / Black
    -- Dark hues readable on the pink page and light chrome.
    darkFg c = case c of
      Default       -> cbText
      Black         -> cbInkDeep
      White         -> ColorRGB 125 107 119
      BrightWhite   -> cbInkDeep               -- "emphasized" on dark terminals
      BrightBlack   -> ColorRGB 150 112 139
      Red           -> ColorRGB 181 72 77
      BrightRed     -> ColorRGB 196 64 74
      Green         -> ColorRGB 46 125 84
      BrightGreen   -> ColorRGB 46 143 94
      Yellow        -> ColorRGB 178 101 0
      BrightYellow  -> ColorRGB 178 101 0
      Blue          -> ColorRGB 94 85 184
      BrightBlue    -> cbPurple
      Magenta       -> cbRaspberry
      BrightMagenta -> ColorRGB 192 57 154
      Cyan          -> ColorRGB 14 116 144
      BrightCyan    -> ColorRGB 14 116 144
      _             -> c                       -- RGB / 256 pass through

-- | Pick the palette the editor's config asks for.
themeFor :: ThemeName -> Theme
themeFor ThemeDark  = defaultTheme
themeFor ThemeLight = lightTheme
themeFor ThemeCherryBlossom = cherryBlossomTheme
themeFor ThemeAuto  = defaultTheme   -- resolved before we get here ('resolvedTheme'); dark is the fallback

darkTokens :: Tok -> Style
darkTokens t = case t of
  TkText      -> Style Default Default attrNone
  TkPunct     -> Style Default Default attrNone
  TkKeyword   -> Style Magenta Default attrBold
  TkType      -> Style Cyan Default attrNone
  TkString    -> Style Green Default attrNone
  TkComment   -> Style BrightBlack Default attrItalic
  TkNumber    -> Style Yellow Default attrNone
  TkFunction  -> Style BrightYellow Default attrNone
  TkBuiltin   -> Style Cyan Default attrNone
  TkDecorator -> Style BrightYellow Default attrNone
  TkTag       -> Style Blue Default attrNone
  TkAttr      -> Style Cyan Default attrNone
  TkHeading   -> Style BrightYellow Default attrBold
  TkEmph      -> Style Default Default attrItalic
  TkStrong    -> Style Default Default attrBold
  TkCode      -> Style Green Default attrNone
  TkLink      -> Style Blue Default attrUnderline
  TkProperty  -> Style BrightBlue Default attrNone

-- Darker hues for light terminal backgrounds (bright yellow/cyan wash out).
lightTokens :: Tok -> Style
lightTokens t = case t of
  TkText      -> Style Default Default attrNone
  TkPunct     -> Style Default Default attrNone
  TkKeyword   -> Style Magenta Default attrBold
  TkType      -> Style Blue Default attrNone
  TkString    -> Style Green Default attrNone
  TkComment   -> Style BrightBlack Default attrItalic
  TkNumber    -> Style Red Default attrNone
  TkFunction  -> Style Blue Default attrBold
  TkBuiltin   -> Style Blue Default attrNone
  TkDecorator -> Style Magenta Default attrNone
  TkTag       -> Style Blue Default attrNone
  TkAttr      -> Style Cyan Default attrNone
  TkHeading   -> Style Magenta Default attrBold
  TkEmph      -> Style Default Default attrItalic
  TkStrong    -> Style Default Default attrBold
  TkCode      -> Style Green Default attrNone
  TkLink      -> Style Blue Default attrUnderline
  TkProperty  -> Style Blue Default attrNone

-- Hand-tuned hues for the Cherry Blossom page (the Default backgrounds are
-- turned into the pink base by 'cherryRemap'): raspberry keywords, plum
-- functions, deep green strings, mauve comments — all picked for contrast on
-- the tinted near-white ground.
cherryTokens :: Tok -> Style
cherryTokens t = case t of
  TkText      -> Style Default Default attrNone
  TkPunct     -> Style Default Default attrNone
  TkKeyword   -> on cbRaspberry attrBold
  TkType      -> on (ColorRGB 94 85 184) attrNone
  TkString    -> on (ColorRGB 46 125 84) attrNone
  TkComment   -> on (ColorRGB 151 107 139) attrItalic
  TkNumber    -> on (ColorRGB 181 72 77) attrNone
  TkFunction  -> on (ColorRGB 156 61 146) attrNone
  TkBuiltin   -> on (ColorRGB 124 80 200) attrNone
  TkDecorator -> on (ColorRGB 178 101 0) attrNone
  TkTag       -> on (ColorRGB 94 85 184) attrNone
  TkAttr      -> on (ColorRGB 140 86 200) attrNone
  TkHeading   -> on cbRaspberry attrBold
  TkEmph      -> Style Default Default attrItalic
  TkStrong    -> Style Default Default attrBold
  TkCode      -> on (ColorRGB 46 125 84) attrNone
  TkLink      -> on (ColorRGB 124 80 200) attrUnderline
  TkProperty  -> on (ColorRGB 94 85 184) attrNone
  where on fg = Style fg Default

------------------------------------------------------------------------------
-- Pure render to a cell grid

type Surf s = STArray s Int Cell

newSurf :: Int -> ST s (Surf s)
newSurf n = newArray (0, max 0 (n - 1)) blankCell

renderEditor :: Editor -> Screen
renderEditor ed = runST $ do
  let lo = computeLayout ed
      rows = loRows lo
      cols = loCols lo
      th = themeFor (resolvedTheme ed)
  arr <- newSurf (rows * cols)
  if searchViewActive ed
    then maybe (pure ()) (\ss -> drawSearch th ss lo arr) (edSearch ed)
    else case edImage ed of
      Just idoc -> drawImage ed idoc lo arr
      Nothing   -> maybe (drawTextArea th ed lo arr) (\v -> drawCsvTable th ed v lo arr) (edCsv ed)
  maybe (pure ()) (drawVScroll th arr cols rows) (scrollBarInfo ed)
  when (isJust (edExplorer ed)) $ drawExplorer th ed lo arr
  when (edShowMenu ed)   $ drawMenuBar th ed lo arr
  when (edShowStatus ed) $ drawStatus th ed lo arr
  when (edShowHints ed)  $ drawHints th ed lo arr
  when (edFocus ed == FMenu && msOpen (edMenu ed)) $ drawDropdown th ed lo arr
  maybe (pure ()) (\d -> drawDialog th ed lo d arr) (edDialog ed)
  when (edFocus ed == FBrowser) $ drawBrowser th ed arr
  when (edFocus ed == FDefPick) $ maybe (pure ()) (\dp -> drawDefPick th ed dp arr) (edDefPick ed)
  when (edFocus ed == FQuickOpen) $ maybe (pure ()) (\qo -> drawQuickOpen th ed qo arr) (edQuickOpen ed)
  when (edFocus ed == FEdit) $ maybe (pure ()) (\cp -> drawComplete th ed cp arr) (edComplete ed)
  maybe (pure ()) (\ld -> drawLoading th ed lo ld arr) (edLoading ed)
  -- Forced-background themes remap every cell onto their palette (see
  -- 'thRemap'); dark/light skip this entirely.
  case thRemap th of
    Nothing -> pure ()
    Just f  -> forM_ [0 .. rows * cols - 1] $ \i -> do
      cell <- readArray arr i
      writeArray arr i cell { cellStyle = f (cellStyle cell) }
  frozen <- freeze arr
  pure (Screen cols rows frozen (computeCursor ed lo) (scrollHintFor ed lo))

-- The scroll identity of this frame, when the main content is the plain
-- (non-wrapped) text view — the one path whose vertical scroll moves whole
-- rows uniformly. Overlays (menus, dialogs) may still cover part of the band;
-- 'scrollPlan' verifies content before scrolling, so the hint stays valid.
scrollHintFor :: Editor -> Layout -> Maybe ScrollHint
scrollHintFor ed lo
  | edSearchMode ed || isJust (edImage ed) || isJust (edCsv ed) || edWordWrap ed = Nothing
  | loTextHeight lo < 4 = Nothing
  | otherwise = Just ScrollHint
      { shTop    = loTextTop lo
      , shHeight = loTextHeight lo
      , shKey    = fileIndex ed * 7919 + edLeft ed * 131
                     + loTextLeft lo * 17 + loContentLeft lo
      , shPos    = edTop ed
      }

-- Low-level cell writers ----------------------------------------------------

putCell :: Surf s -> Int -> Int -> Int -> Int -> Cell -> ST s ()
putCell arr cols rows r c cell =
  when (r >= 0 && r < rows && c >= 0 && c < cols) $
    writeArray arr (r * cols + c) cell

drawStr :: Surf s -> Int -> Int -> Int -> Int -> Style -> String -> ST s ()
drawStr arr cols rows r c0 st s =
  forM_ (zip [0 ..] s) $ \(k, ch) -> putCell arr cols rows r (c0 + k) (Cell ch st)

-- | 'drawStr' with an OSC 8 hyperlink target attached to every cell
-- ('Nothing' is a plain 'drawStr', so callers can pass a computed target).
drawStrL :: Surf s -> Int -> Int -> Int -> Int -> Style -> Maybe Text -> String -> ST s ()
drawStrL arr cols rows r c0 st mlnk s =
  forM_ (zip [0 ..] s) $ \(k, ch) -> putCell arr cols rows r (c0 + k) (CellL ch st mlnk)

-- Like 'drawStr' but underlines the character at index @ui@ (the menu
-- mnemonic). A negative @ui@ underlines nothing.
drawStrU :: Surf s -> Int -> Int -> Int -> Int -> Style -> Int -> String -> ST s ()
drawStrU arr cols rows r c0 st ui s =
  forM_ (zip [0 ..] s) $ \(k, ch) ->
    let st' = if k == ui then st { styleAttr = styleAttr st .|. attrUnderline } else st
    in putCell arr cols rows r (c0 + k) (Cell ch st')

fillRow :: Surf s -> Int -> Int -> Int -> Style -> ST s ()
fillRow arr cols rows r st =
  forM_ [0 .. cols - 1] $ \c -> putCell arr cols rows r c (Cell ' ' st)

-- The right-edge vertical scrollbar: a track with a proportional thumb.
drawVScroll :: Theme -> Surf s -> Int -> Int -> (Int, Int, Int, Int, Int) -> ST s ()
drawVScroll th arr cols rows (x, top, h, total, win) = do
  let (tt, tl) = scrollThumb h total win
  forM_ [0 .. h - 1] $ \i ->
    putCell arr cols rows (top + i) x
      (Cell (if i >= tt && i < tt + tl then '\x2588' else '\x2502') (thScrollbar th))

fillRect :: Surf s -> Int -> Int -> Int -> Int -> Int -> Int -> Style -> ST s ()
fillRect arr cols rows r0 c0 h w st =
  forM_ [r0 .. r0 + h - 1] $ \r ->
    forM_ [c0 .. c0 + w - 1] $ \c -> putCell arr cols rows r c (Cell ' ' st)

------------------------------------------------------------------------------
-- Text area

drawTextArea :: Theme -> Editor -> Layout -> Surf s -> ST s ()
drawTextArea th ed lo arr
  | edWordWrap ed = drawTextAreaWrapped th ed lo arr
  | otherwise = do
      let cols = loCols lo
          rows = loRows lo
          buf = edBuffer ed
          n = lineCount buf
          sel = getSelection ed
          tabw = tabWidthOf ed
          gut = loGutter lo
          cl = loContentLeft lo
          tw = loTextWidth lo
          left = edLeft ed
          hlmap = visibleHighlight ed (loTextHeight lo)
          brs = bracketPair ed
      forM_ [0 .. loTextHeight lo - 1] $ \row -> do
        let bl = edTop ed + row
            sr = loTextTop lo + row
        when (bl < n) $ do
          -- gutter / line number
          when (gut > 0) $ do
            let numStr = show (bl + 1)
                pad = gut - 1 - length numStr
                gstyle = if bl == posLine (edCursor ed) then thGutterCur th else thGutter th
            drawStr arr cols rows sr (cl + max 0 pad) gstyle numStr
          -- line content: expand cells only from just before the horizontal
          -- scroll offset, so a view deep into a huge single line stays cheap.
          let line = getLine' bl buf
              msel = lineSelInterval sel bl (T.length line)
              (selRange, selEOL) = case msel of
                                     Just (s, e, eol) -> (Just (s, e), eol)
                                     Nothing          -> (Nothing, False)
              baseAt = mkBaseAt th (Map.findWithDefault [] bl hlmap)
              (startCol, startDisp) = windowStart tabw left line
              overlays = [ (s, e, thFindMatch th) | (s, e) <- liveMatchSpans ed line ]
                         ++ [ (bc, bc + 1, thBracket th) | Pos brl bc <- brs, brl == bl ]
              cells = expandLineCellsFrom tabw (edShowWhitespace ed)
                        baseAt (thSelection th) (thWhitespace th)
                        selRange selEOL overlays (urlLinks line)
                        startCol startDisp (T.drop startCol line)
              visible = takeWhile (\(d, _) -> d < left + tw)
                          (dropWhile (\(d, _) -> d < left) cells)
          forM_ visible $ \(d, cell) ->
            putCell arr cols rows sr (loTextLeft lo + (d - left)) cell

-- Word-wrapped text rendering: each buffer line occupies one or more visual
-- rows. Tab stops and selection styling are taken from the full line so they
-- stay consistent with the cursor math in "Cmedit.Editor".
drawTextAreaWrapped :: Theme -> Editor -> Layout -> Surf s -> ST s ()
drawTextAreaWrapped th ed lo arr = loop (edTop ed) 0
  where
    cols = loCols lo; rows = loRows lo
    buf = edBuffer ed; n = lineCount buf
    sel = getSelection ed; tabw = tabWidthOf ed
    gut = loGutter lo; cl = loContentLeft lo; th' = loTextHeight lo
    hlmap = visibleHighlight ed (loTextHeight lo)
    brs = bracketPair ed
    loop li vrow
      | vrow >= th' || li >= n = pure ()
      | otherwise = do
          let line = getLine' li buf
              segs = lineSegs ed li
              msel = lineSelInterval sel li (T.length line)
              (selRange, eolFlag) = case msel of
                                      Just (s, e, eol) -> (Just (s, e), eol)
                                      Nothing          -> (Nothing, False)
              baseAt = mkBaseAt th (Map.findWithDefault [] li hlmap)
              overlays = [ (s, e, thFindMatch th) | (s, e) <- liveMatchSpans ed line ]
                         ++ [ (bc, bc + 1, thBracket th) | Pos brl bc <- brs, brl == li ]
              cells = expandLineCells tabw (edShowWhitespace ed)
                        baseAt (thSelection th) (thWhitespace th)
                        selRange False overlays (urlLinks line) line
          vrow' <- drawSegs li line cells eolFlag segs 0 vrow
          loop (li + 1) vrow'
    drawSegs _ _ _ _ [] _ vrow = pure vrow
    drawSegs li line cells eolFlag ((s, e) : rest) si vrow
      | vrow >= th' = pure vrow
      | otherwise = do
          let sr = loTextTop lo + vrow
              ds = colToDisplay tabw s line
              de = colToDisplay tabw e line
              isLast = null rest
          when (gut > 0 && si == 0) $ do
            let numStr = show (li + 1)
                pad = gut - 1 - length numStr
                gstyle = if li == posLine (edCursor ed) then thGutterCur th else thGutter th
            drawStr arr cols rows sr (cl + max 0 pad) gstyle numStr
          forM_ [ (d - ds, cell) | (d, cell) <- cells, d >= ds, d < de ] $ \(off, cell) ->
            putCell arr cols rows sr (loTextLeft lo + off) cell
          when (isLast && eolFlag) $
            putCell arr cols rows sr (loTextLeft lo + (de - ds)) (Cell ' ' (thSelection th))
          drawSegs li line cells eolFlag rest (si + 1) (vrow + 1)

-- Selection columns for a given buffer line: (startCol, endCol, includesEOL).
lineSelInterval :: Maybe (Pos, Pos) -> Int -> Int -> Maybe (Int, Int, Bool)
lineSelInterval Nothing _ _ = Nothing
lineSelInterval (Just (Pos sl sc, Pos el ec)) bl len
  | bl < sl || bl > el = Nothing
  | sl == el           = Just (sc, ec, False)
  | bl == sl           = Just (sc, len, True)
  | bl == el           = Just (0, ec, False)
  | otherwise          = Just (0, len, True)

-- Expand a line into (displayColumn, Cell) pairs, applying selection styling,
-- find-match highlights and whitespace markers, with wide glyphs followed by
-- continuation cells.
expandLineCells
  :: Int -> Bool -> (Int -> Style) -> Style -> Style
  -> Maybe (Int, Int) -> Bool -> [(Int, Int, Style)] -> [(Int, Int, Text)]
  -> Text -> [(Int, Cell)]
expandLineCells tabw showWS baseAt selSty wsSty msel selEOL overlays links line =
  expandLineCellsFrom tabw showWS baseAt selSty wsSty msel selEOL overlays links 0 0 line

-- | 'expandLineCells' starting from character index @i0@ (whose display
-- column is @d0@) with the leading characters already dropped from @line@ —
-- so a view deep into a huge single line only expands the window, not the
-- whole prefix. @overlays@ are absolute character intervals painted with the
-- given style (find matches, the bracket pair); the selection wins where they
-- overlap, and the first covering overlay wins among themselves. @links@ are
-- absolute character intervals whose cells carry an OSC 8 hyperlink target
-- (independent of styling; a wide glyph's continuation cells carry it too).
expandLineCellsFrom
  :: Int -> Bool -> (Int -> Style) -> Style -> Style
  -> Maybe (Int, Int) -> Bool -> [(Int, Int, Style)] -> [(Int, Int, Text)]
  -> Int -> Int -> Text -> [(Int, Cell)]
expandLineCellsFrom tabw showWS baseAt selSty wsSty msel selEOL overlays links i0 d0 line =
  go d0 i0 (T.unpack line)
  where
    inSel i = case msel of Just (s, e) -> i >= s && i < e; Nothing -> False
    overlayAt i = case [ sty | (s, e, sty) <- overlays, i >= s, i < e ] of
                    (sty : _) -> Just sty
                    []        -> Nothing
    styAt i | inSel i = selSty
            | otherwise = fromMaybe (baseAt i) (overlayAt i)
    wsAt i  = if inSel i then selSty else wsSty
    lnkAt i = case [ u | (s, e, u) <- links, i >= s, i < e ] of
                (u : _) -> Just u
                []      -> Nothing
    mkC i ch s = CellL ch s (lnkAt i)
    go dcol _ [] = [ (dcol, Cell ' ' selSty) | selEOL ]
    go dcol i (c : cs)
      | c == '\t' =
          let w = tabw - dcol `mod` tabw
              s = styAt i
              cells = if showWS
                        then (dcol, mkC i '\8594' (wsAt i))
                               : [ (dcol + k, mkC i ' ' s) | k <- [1 .. w - 1] ]
                        else [ (dcol + k, mkC i ' ' s) | k <- [0 .. w - 1] ]
          in cells ++ go (dcol + w) (i + 1) cs
      | isControlChar c =
          let s = styAt i
              cc = take 2 (controlCaret c ++ "  ")
          in (dcol, mkC i (cc !! 0) s) : (dcol + 1, mkC i (cc !! 1) s)
               : go (dcol + 2) (i + 1) cs
      | otherwise =
          let w = max 1 (charWidth c)
              s = styAt i
              c' = if showWS && c == ' ' then '\183' else c
              cont = [ (dcol + k, mkC i contChar s) | k <- [1 .. w - 1] ]
          in (dcol, mkC i c' s) : cont ++ go (dcol + w) (i + 1) cs

-- | The URL hyperlink spans of a document line, with the same length guard
-- as highlighting so a megabyte-long minified line can't dominate a frame.
urlLinks :: Text -> [(Int, Int, Text)]
urlLinks line
  | T.length line > maxHlLine = []
  | otherwise = urlSpans line

-- Map a syntax token to a display style (per-theme palette).
tokStyle :: Theme -> Tok -> Style
tokStyle = thTokens

-- A per-character base-style lookup for one line's tokens.
mkBaseAt :: Theme -> [Tok] -> (Int -> Style)
mkBaseAt th toks
  | null toks = const (thText th)
  | otherwise =
      let n = length toks
          arr = listArray (0, n - 1) toks :: Array Int Tok
      in \i -> if i >= 0 && i < n then tokStyle th (arr ! i) else thText th

-- Tokenise lines [top .. top+count) for highlighting. Line-start lexer states
-- come from the 'HlCache' (refreshed here if the editor's copy is stale), so
-- multi-line constructs are correct from the top of the file — there is no
-- look-back cap — while each frame only lexes the visible window.
highlightMap :: Lang -> Editor -> Int -> Int -> Map.Map Int [Tok]
highlightMap lang ed top count =
  let buf   = edBuffer ed
      lastL = min (lineCount buf - 1) (top + count - 1)
      cache = refreshHlCache lang (bufLines buf) lastL (edHlCache ed)
      go _ li acc
        | li > lastL = acc
      go st li acc =
        let (toks, st') = lexLine lang st (getLine' li buf)
        in go st' (li + 1) (Map.insert li toks acc)
  in go (hlStateBefore cache top) (max 0 top) Map.empty

-- The highlight map for the visible region (empty when no language applies).
visibleHighlight :: Editor -> Int -> Map.Map Int [Tok]
visibleHighlight ed count = case langForPath (edPath ed) of
  Just lang -> highlightMap lang ed (edTop ed) count
  Nothing   -> Map.empty

-- | Bring the editor's highlight-state cache up to date for the current
-- viewport so the render that follows finds it fresh. The driver calls this
-- before each repaint and keeps the returned editor, which is what carries
-- the lexer states across frames; rendering stays correct without it, just
-- slower (the cache would be rebuilt from scratch every frame).
refreshHighlight :: Editor -> Editor
refreshHighlight ed
  | edSearchMode ed || isJust (edImage ed) || isJust (edCsv ed) = ed
  | otherwise = case langForPath (edPath ed) of
      Nothing -> ed
      Just lang ->
        let lo    = computeLayout ed
            buf   = edBuffer ed
            lastL = min (lineCount buf - 1) (edTop ed + loTextHeight lo - 1)
        in ed { edHlCache = Just (refreshHlCache lang (bufLines buf) lastL (edHlCache ed)) }

------------------------------------------------------------------------------
-- Menu bar

drawMenuBar :: Theme -> Editor -> Layout -> Surf s -> ST s ()
drawMenuBar th ed lo arr = do
  let cols = loCols lo; rows = loRows lo; r = loMenuRow lo
  fillRow arr cols rows r (thMenuBar th)
  go 1 (zip [0 ..] menuBar)
  where
    cols = loCols lo; rows = loRows lo; r = loMenuRow lo
    active i = edFocus ed == FMenu && msMenuIx (edMenu ed) == i
    go _ [] = pure ()
    go col ((i, m) : rest) = do
      let (disp, mIdx) = parseMnemonic (menuTitle m)
          title = T.unpack disp
          len = length title
          st = if active i then thMenuActive th else thMenuBar th
      putCell arr cols rows r col (Cell ' ' st)
      drawStrU arr cols rows r (col + 1) st mIdx title   -- underline the mnemonic
      putCell arr cols rows r (col + 1 + len) (Cell ' ' st)
      go (col + len + 3) rest

------------------------------------------------------------------------------
-- Dropdown

drawDropdown :: Theme -> Editor -> Layout -> Surf s -> ST s ()
drawDropdown th ed lo arr = do
  let cols = loCols lo; rows = loRows lo
      ms = edMenu ed
      mi = msMenuIx ms
      entries = entriesFor ed mi
      accelW = maximum (0 : [ T.length a | MEItem _ a _ <- entries ])
      -- Geometry shared with the mouse hit-testing in Cmedit.Editor.
      (y0, x0, h, innerW) = dropdownGeom ed
  -- box background
  fillRect arr cols rows y0 x0 h (innerW + 2) (thMenuItem th)
  drawBox th arr cols rows y0 x0 h (innerW + 2) (thMenuItem th)
  forM_ (zip [0 ..] entries) $ \(j, e) -> do
    let r = y0 + 1 + j
    case e of
      MESep ->
        drawStr arr cols rows r x0 (thMenuItem th)
          ('\9500' : replicate innerW '\9472' ++ "\9508")
      MEItem lbl accel _ -> do
        let selected = msItemIx ms == j
            st = if selected then thMenuSel th else thMenuItem th
            ast = if selected then thMenuSel th else thMenuAccel th
            (disp, mIdx) = parseMnemonic lbl
        fillRect arr cols rows r (x0 + 1) 1 innerW st
        drawStrU arr cols rows r (x0 + 2) st mIdx (T.unpack disp)
        drawStr arr cols rows r (x0 + 1 + innerW - accelW - 1) ast (T.unpack accel)

drawBox :: Theme -> Surf s -> Int -> Int -> Int -> Int -> Int -> Int -> Style -> ST s ()
drawBox _ arr cols rows y0 x0 h w st = do
  let r1 = y0; r2 = y0 + h - 1
      c1 = x0; c2 = x0 + w - 1
  putCell arr cols rows r1 c1 (Cell '\9484' st)   -- ┌
  putCell arr cols rows r1 c2 (Cell '\9488' st)   -- ┐
  putCell arr cols rows r2 c1 (Cell '\9492' st)   -- └
  putCell arr cols rows r2 c2 (Cell '\9496' st)   -- ┘
  forM_ [c1 + 1 .. c2 - 1] $ \c -> do
    putCell arr cols rows r1 c (Cell '\9472' st)
    putCell arr cols rows r2 c (Cell '\9472' st)
  forM_ [r1 + 1 .. r2 - 1] $ \r -> do
    putCell arr cols rows r c1 (Cell '\9474' st)
    putCell arr cols rows r c2 (Cell '\9474' st)

------------------------------------------------------------------------------
-- Status bar

drawStatus :: Theme -> Editor -> Layout -> Surf s -> ST s ()
drawStatus th ed lo arr = do
  let cols = loCols lo; rows = loRows lo; r = loStatusRow lo
  fillRow arr cols rows r (thStatus th)
  let name = maybe "untitled" id (fmap shortName (edPath ed))
      modi = if edModified ed then "\9679 " else "  "   -- ●
      ro   = if edReadOnly ed then " [RO]" else ""
      tabs = if fileCount ed > 1
               then "[" ++ show (fileIndex ed) ++ "/" ++ show (fileCount ed) ++ "] "
               else ""
      prefix = " " ++ modi ++ tabs
      left = prefix ++ name ++ ro
      -- The file name is an OSC 8 hyperlink to the file itself (absolute
      -- real paths only — untitled buffers and cmedit:// pseudo-paths give
      -- 'Nothing' and draw plain).
      nameLink = filePathUri =<< edPath ed
      -- The right side (and its clickable zones) comes from the shared
      -- builder in Cmedit.Editor so mouse hit-testing can never disagree.
      right = fst (statusRightInfo ed)
      -- The link-hover hint temporarily overlays the message slot while the
      -- pointer is on a URL (hovering is the *current* action; any status
      -- message is older news and comes back when the pointer moves off).
      status = case edHoverUrl ed of
        Just u  -> "Ctrl+Click to open " ++ T.unpack u
        Nothing -> T.unpack (edStatus ed)
  drawStr arr cols rows r 0 (thStatus th) prefix
  drawStrL arr cols rows r (length prefix) (thStatus th) nameLink name
  drawStr arr cols rows r (length prefix + length name) (thStatus th) ro
  -- transient status message just after the filename
  when (not (null status)) $
    drawStr arr cols rows r (length left + 2) (thStatusKey th) status
  drawStr arr cols rows r (max 0 (cols - length right)) (thStatus th) right

shortName :: FilePath -> String
shortName = reverse . takeWhile (/= '/') . reverse

------------------------------------------------------------------------------
-- Hint bar (nano-style)

drawHints :: Theme -> Editor -> Layout -> Surf s -> ST s ()
drawHints th ed lo arr = do
  let cols = loCols lo; rows = loRows lo; r = loHintRow lo
  fillRow arr cols rows r (thHint th)
  let hints
       | edFocus ed == FSearch =
         maybe id (\ss -> if ssShowReplace ss
                            then (++ [("A+R", "ReplaceAll"), ("^\x21b5", "ReplaceFile")]) else id) (edSearch ed)
         [ ("\x21b5", "Search/Open"), ("Tab", "Field"), ("\x2191\x2193", "Move")
         , ("\x2190\x2192", "Fold"), ("A+C", "Case"), ("A+W", "Word")
         , ("A+X", "Regex"), ("A+H", "Replace"), ("Esc", "Editor") ]
       | edFocus ed == FExplorer =
         [ ("\x2191\x2193", "Move"), ("\x21b5", "Open"), ("Ins", "New")
         , ("F2", "Rename"), ("Del", "Delete"), (".", "Hidden")
         , ("^B", "Editor"), ("F10", "Menu") ]
       | otherwise = case edImage ed of
       Just _ ->
         [ ("a", "ASCII/Colour"), ("^O", "Open"), ("^W", "Close")
         , ("^PgUp", "PrevFile"), ("^Q", "Quit"), ("F10", "Menu") ]
       Nothing -> case edCsv ed of
        Just _ ->
          [ ("Enter", "Edit"), ("Tab", "Next"), ("A+\x2191\x2193", "Row")
          , ("A+\x2190\x2192", "Col"), ("^Del", "DelRow"), ("A+Bksp", "DelCol")
          , ("^S", "Save"), ("A+T", "Text") ]
        Nothing ->
          [ ("^S", "Save"), ("^O", "Open"), ("^F", "Find"), ("^G", "Go To")
          , ("^Z", "Undo"), ("^X", "Cut"), ("^V", "Paste"), ("^Q", "Quit")
          , ("F10", "Menu") ]
  go 1 hints
  where
    cols = loCols lo; rows = loRows lo; r = loHintRow lo
    go _ [] = pure ()
    go col ((k, lbl) : rest) = do
      drawStr arr cols rows r col (thHintKey th) k
      let col2 = col + length k + 1
      drawStr arr cols rows r col2 (thHint th) lbl
      go (col2 + length lbl + 2) rest

------------------------------------------------------------------------------
-- Dialog

-- The dialog body layout (DRow, dialogRows, dialogGeom, fieldRowIndex,
-- fieldLineWidth) lives in Cmedit.Editor so the editor can hit-test mouse
-- clicks against it; the renderer just consumes it.

drawDialog :: Theme -> Editor -> Layout -> Dialog -> Surf s -> ST s ()
drawDialog th ed lo d arr = do
  let cols = loCols lo; rows = loRows lo
      (y, x, h, w) = dialogGeom ed d lo
  fillRect arr cols rows y x h w (thDialog th)
  drawBox th arr cols rows y x h w (thDialog th)
  -- Title centred on the top border; the About box colours its wordmark.
  if dlgKind d == DKAbout
    then drawAboutTitle arr cols rows y x w
    else do
      let title = " " ++ T.unpack (dlgTitle d) ++ " "
          tx = x + (w - length title) `div` 2
      drawStr arr cols rows y tx (thDialogTitle th) title
  let rs = dialogRows d
  forM_ (zip [0 ..] rs) $ \(j, dr) -> do
    let r = y + 1 + j
    drawDRow th ed d arr cols rows r (x + 2) (w - 4) dr
  -- The About box's animated wordmark, overlaid on the blank canvas rows at
  -- the top of its body (frame counter ticked by the event loop).
  when (dlgKind d == DKAbout) $
    forM_ (aboutFrameCells (w - 4) (edAboutTick ed)) $ \((rr, cc), cell) ->
      putCell arr cols rows (y + 1 + rr) (x + 2 + cc) cell
  -- The keyboard card, likewise overlaid on the help dialog's blank canvas.
  when (dlgKind d == DKHelp) $
    forM_ (helpFrameCells (w - 4)) $ \((rr, cc), cell) ->
      putCell arr cols rows (y + 1 + rr) (x + 2 + cc) cell

-- "About CMeDit" on the top border, with the CMeDit wordmark in its brand
-- colours: C / M / D blue, e / t red, i grey (on the dialog's light border).
drawAboutTitle :: Surf s -> Int -> Int -> Int -> Int -> Int -> ST s ()
drawAboutTitle arr cols rows r x0 w =
  let blue = Style Blue White attrBold
      red  = Style Red White attrBold
      grey = Style BrightBlack White attrBold
      lbl  = Style Black White attrBold
      brand = [ ('C', blue), ('M', blue), ('e', red)
              , ('D', blue), ('i', grey), ('t', red) ]
      pre = " About "
      total = length pre + length brand + 1
      sc = x0 + max 0 ((w - total) `div` 2)
  in do
    drawStr arr cols rows r sc lbl pre
    forM_ (zip [0 ..] brand) $ \(i, (ch, st)) ->
      putCell arr cols rows r (sc + length pre + i) (Cell ch st)
    putCell arr cols rows r (sc + length pre + length brand) (Cell ' ' lbl)

drawDRow :: Theme -> Editor -> Dialog -> Surf s -> Int -> Int -> Int -> Int -> Int -> DRow -> ST s ()
drawDRow th _ed d arr cols rows r x innerW dr = case dr of
  DRBlank -> pure ()
  DRMsg m -> drawStr arr cols rows r x (thDialog th) (T.unpack m)
  DRField i li visH -> do
    -- One visual line of a (possibly multi-line) field. The value box reuses the
    -- table view's 'cellDisplay': the focused field scrolls vertically to keep the
    -- cursor line shown and horizontally to keep the cursor column shown.
    let Field lbl t cur = dlgFields d !! i
        focused  = dlgFocus d == i
        fst'     = if focused then thFieldFocus th else thField th
        labelW   = T.length lbl + 1
        valStart = x + labelW
        valW     = max 1 (innerW - labelW)
        dispLines = cellDisplay valW visH (if focused then Just cur else Nothing) t
        content   = if li < length dispLines then dispLines !! li else ""
    when (li == 0) $ drawStr arr cols rows r x (thDialog th) (T.unpack lbl ++ " ")
    fillRect arr cols rows r valStart 1 valW fst'
    drawStr arr cols rows r valStart fst' (take valW (content ++ repeat ' '))
    -- Mirror the table view's '+': flag a field with more lines than fit.
    when (li == visH - 1 && Csv.cellLineCount t > visH) $
      drawStr arr cols rows r x (thDialog th) "+"
    -- cursor handled separately
    pure ()
  DROption i -> do
    let (lbl, on) = dlgOptions d !! i
        focused = dlgFocus d == length (dlgFields d) + i
        st = if focused then thButtonFocus th else thDialog th
        box = if on then "[x] " else "[ ] "
    drawStr arr cols rows r x st (box ++ T.unpack lbl)
  DRButtons -> do
    let btns = dlgButtons d
        baseFocus = length (dlgFields d) + length (dlgOptions d)
        total = sum [ T.length b + 4 | b <- btns ] + (length btns - 1)
        start = x + max 0 ((innerW - total) `div` 2)
    drawButtons th d arr cols rows r start baseFocus (zip [0 ..] btns)

drawButtons :: Theme -> Dialog -> Surf s -> Int -> Int -> Int -> Int -> Int -> [(Int, Text)] -> ST s ()
drawButtons _ _ _ _ _ _ _ _ [] = pure ()
drawButtons th d arr cols rows r col baseFocus ((i, b) : rest) = do
  let focused = dlgFocus d == baseFocus + i
      st = if focused then thButtonFocus th else thButton th
      label = "  " ++ T.unpack b ++ "  "
  drawStr arr cols rows r col st label
  drawButtons th d arr cols rows r (col + length label + 1) baseFocus rest

------------------------------------------------------------------------------
-- File browser

drawBrowser :: Theme -> Editor -> Surf s -> ST s ()
drawBrowser th ed arr = do
  let (rows, cols) = edSize ed
      (y, x, h, w) = browserBox ed
      dirSty  = Style Blue White attrBold
      selSty  = Style BrightWhite Blue attrNone
      selDir  = Style BrightWhite Blue attrBold
      hdrSty  = Style Black White attrBold
  fillRect arr cols rows y x h w (thDialog th)
  drawBox th arr cols rows y x h w (thDialog th)
  let pick = edBrowserPick ed
      title = if pick then " Open Folder " else " Open File "
      tx = x + (w - length title) `div` 2
  drawStr arr cols rows y tx (thDialogTitle th) title
  case edBrowser ed of
    Nothing -> drawStr arr cols rows (y + 2) (x + 2) (thDialog th) "Loading\x2026"
    Just br -> do
      -- Header: the current root directory path (clipped on the left).
      drawStr arr cols rows (y + 1) (x + 2) hdrSty (ellipsizeLeft (w - 4) (fnPath (brRoot br)))
      -- Tree rows.
      let treeTop = y + 2
          vh = browserTreeHeight ed
          allRows = Br.visibleRows br
          shown = take vh (drop (brTop br) allRows)
      forM_ (zip [0 ..] shown) $ \(i, (depth, node)) -> do
        let r = treeTop + i
            idx = brTop br + i
            selected = idx == brSelected br
            indent = depth * 2
            marker
              | fnParent node                   = "\x2191 "    -- up arrow for ".."
              | fnIsDir node && fnExpanded node = "\x25be "    -- down triangle
              | fnIsDir node                    = "\x25b8 "    -- right triangle
              | otherwise                       = "  "
            suffix  = if fnIsDir node && not (fnParent node) then "/" else ""
            nameStr = T.unpack (fnName node) ++ suffix
            rowSty  = if selected then selSty else thDialog th
            nameSty
              | selected   = if fnIsDir node then selDir else selSty
              | fnIsDir node = dirSty
              | otherwise  = thDialog th
            startCol = x + 2 + indent
            avail = w - 4 - indent - length marker
        fillRect arr cols rows r (x + 1) 1 (w - 2) rowSty
        drawStr arr cols rows r startCol rowSty marker
        drawStr arr cols rows r (startCol + length marker) nameSty (take (max 0 avail) nameStr)
      -- Footer hint.
      let act = if edBrowserPick ed then "\x21b5 open folder" else "\x21b5 open"
          hint = " \x2191\x2193 move   \x2192 expand   \x2190 up   " ++ act
                 ++ "   . hidden   Esc cancel "
      drawStr arr cols rows (y + h - 2) (x + 2) hdrSty (take (w - 4) hint)

-- Clip a string to width, keeping the right-hand (most specific) end.
ellipsizeLeft :: Int -> String -> String
ellipsizeLeft maxw s
  | length s <= maxw = s
  | maxw <= 1        = take maxw s
  | otherwise        = '\x2026' : drop (length s - maxw + 1) s

------------------------------------------------------------------------------
-- Go-to-definition picker

-- The modal list of definition sites ('defPickGeom' supplies the box; the
-- Editor's mouse hit-testing uses the same geometry).
drawDefPick :: Theme -> Editor -> DefPick -> Surf s -> ST s ()
drawDefPick th ed dp arr = do
  let (rows, cols) = edSize ed
      (y, x, h, w) = defPickGeom ed
      vh = max 1 (h - 3)
      selSty = Style BrightWhite Blue attrNone
      dimSty = Style BrightBlack White attrNone
  fillRect arr cols rows y x h w (thDialog th)
  drawBox th arr cols rows y x h w (thDialog th)
  let title = " Definitions of \x2018" ++ T.unpack (dpName dp) ++ "\x2019 "
      tx = x + max 1 ((w - length title) `div` 2)
  drawStr arr cols rows y tx (thDialogTitle th) (take (w - 2) title)
  let items = dpItems dp
      shown = take vh (drop (dpTop dp) items)
      innerW = w - 2
      pathW  = min 44 (max 16 (innerW * 2 `div` 5))
  if null items
    then drawStr arr cols rows (y + 1) (x + 2) (thDialog th)
           (if dpRunning dp then "Searching\x2026" else "No definitions found")
    else forM_ (zip [0 ..] shown) $ \(i, it) -> do
      let r = y + 1 + i
          selected = dpTop dp + i == dpSel dp
          sty = if selected then selSty else thDialog th
          loc = makeRelative (dpRoot dp) (diPath it) ++ ":" ++ show (diLine it + 1)
          snippet = T.unpack (T.map detab (T.strip (diText it)))
          row = " " ++ padTo pathW (ellipsizeLeft pathW loc) ++ " \x2502 " ++ snippet
      drawStr arr cols rows r (x + 1) sty (padTo innerW (take innerW row))
  let n = length items
      more | dpRunning dp = "searching\x2026  "
           | otherwise    = show n ++ " definition" ++ (if n == 1 then "" else "s") ++ "  "
      hint = " " ++ more ++ "\x21b5 open   \x2191\x2193 move   Esc cancel "
  drawStr arr cols rows (y + h - 2) (x + 1) dimSty (padTo (w - 2) (take (w - 2) hint))
  where
    detab c = if c == '\t' then ' ' else c
    padTo n s = s ++ replicate (n - length s) ' '

-- The Ctrl+P quick-open palette: a query input over a ranked file list, with
-- the fuzzy-matched characters emphasised.
drawQuickOpen :: Theme -> Editor -> QuickOpen -> Surf s -> ST s ()
drawQuickOpen th ed qo arr = do
  let (rows, cols) = edSize ed
      (y, x, h, w) = quickOpenGeom ed
      vh = max 1 (h - 4)
      selSty = Style BrightWhite Blue attrNone
      dimSty = Style BrightBlack White attrNone
      innerW = w - 2
  fillRect arr cols rows y x h w (thDialog th)
  drawBox th arr cols rows y x h w (thDialog th)
  let title = if Q.qoCommandMode qo then " Command Palette " else " Go to File "
      tx = x + max 1 ((w - length title) `div` 2)
  drawStr arr cols rows y tx (thDialogTitle th) title
  -- Query input: "> query", scrolled so the cursor stays visible.
  let SField qtext qcur = qoField qo
      valW = max 1 (w - 6)
      off = if qcur >= valW then qcur - valW + 1 else 0
      shown = T.unpack (T.take valW (T.drop off qtext))
  drawStr arr cols rows (y + 1) (x + 2) (thDialog th) ("> " ++ shown
    ++ replicate (max 0 (valW - length shown)) ' ')
  -- Ranked results; matched characters bold+underlined for scannability.
  let items = take vh (drop (qoTop qo) (qoMatches qo))
  if null (qoMatches qo)
    then drawStr arr cols rows (y + 2) (x + 2) dimSty
           (if qoRunning qo then "Scanning\x2026" else "No matching files")
    else forM_ (zip [0 ..] items) $ \(i, (_, path, ps)) -> do
      let r = y + 2 + i
          selected = qoTop qo + i == qoSel qo
          sty = if selected then selSty else thDialog th
          emph = sty { styleAttr = styleAttr sty .|. attrBold .|. attrUnderline }
          disp = ' ' : T.unpack path
      forM_ (zip [0 ..] (take innerW (disp ++ repeat ' '))) $ \(k, ch) ->
        let s = if (k - 1) `elem` ps then emph else sty
        in putCell arr cols rows r (x + 1 + k) (Cell ch s)
  -- Footer: match count / walk progress + key hints.
  let n = qoTotal qo
      cmdMode = Q.qoCommandMode qo
      state | cmdMode      = show n ++ " command" ++ (if n == 1 then "" else "s") ++ "  "
            | qoRunning qo = "scanning\x2026  "
            | qoTrunc qo   = show n ++ "+ files  "
            | otherwise    = show n ++ " match" ++ (if n == 1 then "" else "es") ++ "  "
      hint = " " ++ state ++ (if cmdMode then "\x21b5 run " else "\x21b5 open ")
               ++ "  \x2191\x2193 move   Esc cancel "
  drawStr arr cols rows (y + h - 2) (x + 1) dimSty
    (take innerW (hint ++ repeat ' '))

-- The Ctrl+Space completion popup: a compact list anchored at the prefix,
-- selected row highlighted, prefix part dimmed for scannability.
drawComplete :: Theme -> Editor -> Complete -> Surf s -> ST s ()
drawComplete th ed cp arr = do
  let (rows, cols) = edSize ed
      (y, x, h, w) = completeGeom ed cp
      items = take h (drop (cpTop cp) (cpItems cp))
      plen = T.length (cpPrefix cp)
  forM_ (zip [0 ..] items) $ \(i, wtxt) -> do
    let r = y + i
        selected = cpTop cp + i == cpSel cp
        sty = if selected then thMenuSel th else thMenuItem th
        preSty = sty { styleAttr = styleAttr sty .|. attrBold }
        disp = ' ' : T.unpack wtxt
    forM_ (zip [0 ..] (take w (disp ++ repeat ' '))) $ \(k, ch) ->
      let s = if k >= 1 && k <= plen then preSty else sty
      in putCell arr cols rows r (x + k) (Cell ch s)

------------------------------------------------------------------------------
-- File explorer panel (workspace sidebar)

drawExplorer :: Theme -> Editor -> Layout -> Surf s -> ST s ()
drawExplorer th ed lo arr
  | cl <= 0   = pure ()
  | edExpCollapsed ed = drawExplorerStrip th ed lo arr
  | otherwise = drawExplorerPanel th ed lo arr
  where cl = loContentLeft lo

-- The collapsed single-column strip: click anywhere to expand.
drawExplorerStrip :: Theme -> Editor -> Layout -> Surf s -> ST s ()
drawExplorerStrip th _ed lo arr = do
  let cols = loCols lo; rows = loRows lo
      ptop = loTextTop lo; ph = loTextHeight lo
      sty = thMenuBar th
  forM_ [ptop .. ptop + ph - 1] $ \r -> putCell arr cols rows r 0 (Cell ' ' sty)
  putCell arr cols rows ptop 0 (Cell '\xbb' sty)   -- » : expand

-- | A coarse file-type classification (by extension) used to tint file names
-- in the explorer and flag the images we can display. Purely a display hint;
-- actual openability is still decided by content when a file is opened.
data FileKind
  = FKImage    -- ^ a raster image the viewer can display
  | FKCode     -- ^ a source language we highlight/lint
  | FKMarkup   -- ^ Markdown / HTML / XML
  | FKData     -- ^ JSON / YAML / TOML / INI / CSV and friends
  | FKBinary   -- ^ a known opaque/binary blob we cannot open as text
  | FKPlain    -- ^ anything else (treated as plain text)
  deriving (Eq, Show)

-- | Extensions of raster images the viewer can decode ('Cmedit.Image'). Kept in
-- step with the magic-byte sniffer there — this is only the explorer's hint.
imageExtensions :: [String]
imageExtensions =
  ["png","jpg","jpeg","gif","bmp","webp","ppm","pgm","pbm","pnm"]

fileKind :: FilePath -> FileKind
fileKind path
  | ext `elem` imageExtensions            = FKImage
  | Just lang <- langForPath (Just path)  = case lang of
      Markdown -> FKMarkup
      HTML     -> FKMarkup
      JSON     -> FKData
      YAML     -> FKData
      TOML     -> FKData
      INI      -> FKData
      CSV      -> FKData
      _        -> FKCode
  | S.binaryExtension path                = FKBinary
  | otherwise                             = FKPlain
  where ext = map toLower (drop 1 (takeExtension path))

-- | Name colour for a file kind against the panel background (unopened files;
-- open/active/modified files keep their state colour). Binary blobs are dimmed
-- so they read as "nothing to open here".
fileKindStyle :: Theme -> FileKind -> Style
fileKindStyle th k = case k of
  FKImage  -> Style Magenta Default attrNone
  FKCode   -> Style Green   Default attrNone
  FKMarkup -> Style Cyan    Default attrNone
  FKData   -> Style Yellow  Default attrNone
  FKBinary -> Style BrightBlack Default attrNone
  FKPlain  -> thText th

-- | The leading glyph for a file kind (only images get one for now), or a space.
fileKindIcon :: FileKind -> Char
fileKindIcon FKImage = '\x274f'   -- ❏ : a displayable image
fileKindIcon _       = ' '

-- The expanded panel: a header (folder name + collapse/close buttons), the
-- directory tree with open/modified/disk-changed decorations, and a divider.
drawExplorerPanel :: Theme -> Editor -> Layout -> Surf s -> ST s ()
drawExplorerPanel th ed lo arr = do
  let cols = loCols lo; rows = loRows lo
      cl   = loContentLeft lo
      ptop = loTextTop lo; ph = loTextHeight lo
      dividerCol = cl - 1
      pcw  = cl - 1                         -- panel content width (excl. divider)
      hdrSty   = thMenuBar th
      btnSty   = (thMenuBar th) { styleAttr = attrBold }
      divSty   = Style BrightBlack Default attrNone
      focused  = edFocus ed == FExplorer
      closeCol = explorerCloseCol lo
      collCol  = explorerCollapseCol lo
  -- Divider down the full height of the panel.
  forM_ [ptop .. ptop + ph - 1] $ \r -> putCell arr cols rows r dividerCol (Cell '\x2502' divSty)
  -- Header bar: folder name + « collapse + ✕ close.
  fillRect arr cols rows ptop 0 1 pcw hdrSty
  drawStr arr cols rows ptop 1 hdrSty (take (max 0 (collCol - 2)) (explorerRootName ed))
  putCell arr cols rows ptop collCol  (Cell '\xab' btnSty)    -- «
  putCell arr cols rows ptop closeCol (Cell '\x2715' btnSty)  -- ✕
  -- Tree.
  case edExplorer ed of
    Nothing -> pure ()
    Just br -> do
      let treeTop = ptop + 1
          vh = explorerTreeHeight ed
          allRows = Br.visibleRows br
          shown = take vh (drop (brTop br) allRows)
      forM_ (zip [0 ..] shown) $ \(i, (depth, node)) -> do
        let r = treeTop + i
            idx = brTop br + i
            selected = idx == brSelected br
            isDir = fnIsDir node
            indent = depth * 2
            path = fnPath node
            kind = if isDir then FKPlain else fileKind path
            fmk  = if isDir then Nothing else fileMarkFor ed path
            open = isJust fmk
            modi = maybe False fmModified fmk
            disk = maybe False fmDiskChanged fmk
            active = maybe False fmActive fmk
            markCh | isDir && fnExpanded node = '\x25be'   -- ▾
                   | isDir                    = '\x25b8'   -- ▸
                   | otherwise                = ' '
            nameStr = T.unpack (fnName node) ++ (if isDir then "/" else "")
            tooBig  = not isDir && maybe False (> maxOpenBytes) (fnSize node)
            selSty | focused   = Style BrightWhite Blue attrNone
                   | otherwise = Style BrightWhite BrightBlack attrNone
            rowSty = if selected then selSty else thText th
            nameSty
              | selected   = selSty { styleAttr = if isDir || active then attrBold else attrNone }
              | tooBig     = Style BrightBlack Default attrNone   -- too large to edit: dimmed
              | isDir      = Style Blue Default attrBold
              | active     = Style BrightWhite Default attrBold
              | modi       = Style Yellow Default attrNone
              | open       = Style BrightWhite Default attrNone
              | otherwise  = fileKindStyle th kind   -- tint unopened files by type
            statusCh | disk = '\x25c6'   -- ◆ changed on disk since opened
                     | modi = '\x25cf'   -- ● unsaved edits
                     | otherwise = ' '
            statusSty = case () of
              _ | selected  -> selSty
                | disk      -> Style Cyan Default attrNone
                | modi      -> Style Yellow Default attrNone
                | otherwise -> thText th
            -- Size label for large files, right-aligned before the status char.
            sizeStr = case fnSize node of
                        Just sz | not isDir && sz >= sizeLabelThreshold -> shortSize sz
                        _ -> ""
            statusCol = pcw - 1
            sizeCol = if null sizeStr then statusCol else statusCol - 1 - length sizeStr
            sizeSty | selected  = selSty
                    | tooBig    = Style Red Default attrNone
                    | otherwise = Style BrightBlack Default attrNone
            startCol = indent + 1
            nameCol  = startCol + 2
            avail    = max 0 (sizeCol - nameCol)
        fillRect arr cols rows r 0 1 pcw rowSty
        putCell arr cols rows r startCol (Cell markCh rowSty)
        -- Type glyph in the leading indicator column (where a directory's
        -- arrow goes), so image rows read like "❏ name" beside "▸ folder".
        -- Currently only displayable images get one.
        let iconCh  = fileKindIcon kind
            iconSty | selected  = selSty
                    | otherwise = fileKindStyle th kind
        when (iconCh /= ' ' && startCol < sizeCol) $
          putCell arr cols rows r startCol (Cell iconCh iconSty)
        -- The name is an OSC 8 hyperlink to the file/folder, so a
        -- terminal that supports links lets Ctrl+Click open it in the OS.
        drawStrL arr cols rows r nameCol nameSty (filePathUri path) (take avail nameStr)
        drawStr arr cols rows r sizeCol sizeSty sizeStr
        putCell arr cols rows r statusCol (Cell statusCh statusSty)

------------------------------------------------------------------------------
-- Workspace search view (find/replace in files)

drawSearch :: Theme -> SearchState -> Layout -> Surf s -> ST s ()
drawSearch th ss lo arr = do
  let cols = loCols lo; rows = loRows lo
      (top, left, h, w) = searchRegion lo
      baseSty  = thText th
      hls      = S.headerLines ss
      hh       = length hls
  -- Blank the whole region first.
  fillRect arr cols rows top left h w baseSty
  -- Header lines.
  forM_ (zip [0 ..] hls) $ \(i, hl) ->
    drawSearchHeader th ss arr cols rows (top + i) left w hl
  -- Results (scrolling).
  let resTop = top + hh
      resH   = max 0 (h - hh)
      allRows = S.resultRows ss
      shown  = take resH (drop (ssTop ss) allRows)
      curRow = S.cursorRowInResults ss
  forM_ (zip [0 ..] shown) $ \(i, srow) -> do
    let r = resTop + i
        idx = ssTop ss + i
        selected = curRow == Just idx
    drawResultRow th ss arr cols rows r left w srow selected

-- One fixed header line of the search view.
drawSearchHeader :: Theme -> SearchState -> Surf s -> Int -> Int -> Int -> Int -> Int -> HLine -> ST s ()
drawSearchHeader th ss arr cols rows r left w hl = case hl of
  HLScope -> do
    let sty = thStatus th
        spin | ssRunning ss = [spinnerFrames !! (ssSpin ss `mod` length spinnerFrames), ' ']
             | otherwise    = "\x2315 "         -- ⌕
        rootNm = let p = ssRoot ss; nm = shortName p in if null nm then p else nm
        title = " " ++ spin ++ "SEARCH  " ++ rootNm
    fillRect arr cols rows r left 1 w sty
    drawStr arr cols rows r left sty (take w (title ++ repeat ' '))
  HLFind    -> drawSearchField th ss arr cols rows r left w SFFind "Find"
  HLReplace -> drawSearchField th ss arr cols rows r left w SFReplace "Replace"
  HLInclude -> drawSearchField th ss arr cols rows r left w SFInclude "Files"
  HLExclude -> drawSearchField th ss arr cols rows r left w SFExclude "Excl"
  HLSummary -> do
    let msg | ssRunning ss = T.unpack (ssMessage ss) ++ "searching\x2026 (" ++ show (ssScanned ss) ++ " scanned)"
            | T.null (ssMessage ss) && not (ssSearched ss) = "Enter a term and press Enter"
            | otherwise    = T.unpack (ssMessage ss)
        sty = Style BrightBlack Default attrNone
    drawStr arr cols rows r left sty (take w (" " ++ msg))
  HLDivider ->
    forM_ [left .. left + w - 1] $ \c -> putCell arr cols rows r c (Cell '\x2500' (Style BrightBlack Default attrNone))

-- Draw a labelled, editable field row (with focus highlight, controls, scroll).
drawSearchField :: Theme -> SearchState -> Surf s -> Int -> Int -> Int -> Int -> Int -> SearchField -> String -> ST s ()
drawSearchField th ss arr cols rows r left w fld label = do
  let labelSty = Style BrightBlack Default attrNone
      focused  = S.focusedField ss == Just fld
      fst'     = if focused then thFieldFocus th else thField th
      (SField t cur) = fieldOf fld ss
      vcol     = left + searchFieldValueCol
      (valW, ctls) = fieldValueWidth ss left w fld
      -- horizontal scroll so the cursor stays visible when focused
      off      = if focused && cur >= valW then cur - valW + 1 else 0
      shownTxt = T.unpack (T.take valW (T.drop off t))
      placeholder = case fld of
        SFFind    | T.null t -> "text to find \x2014 press Enter to search"
        SFReplace | T.null t -> "replacement text \x2014 Alt+R replaces all"
        SFInclude | T.null t -> "include, e.g. *.hs, src/**"
        SFExclude | T.null t -> "exclude, e.g. *.min.js, docs/**"
        _ -> ""
  -- label
  drawStr arr cols rows r (left + 1) labelSty (take (searchFieldValueCol - 1) (label ++ repeat ' '))
  -- value box
  fillRect arr cols rows r vcol 1 valW fst'
  if null shownTxt && not (null placeholder)
    then drawStr arr cols rows r vcol (fst' { styleFg = BrightBlack }) (take valW placeholder)
    else drawStr arr cols rows r vcol fst' (take valW (shownTxt ++ repeat ' '))
  -- controls (Find line: Aa/W/toggle; Replace line: [Replace All])
  mapM_ (\(c, s, ctl) -> drawStr arr cols rows r c (ctlStyle th ss ctl) s) ctls

-- The controls to draw on a field row, and the value-box width left over.
fieldValueWidth :: SearchState -> Int -> Int -> SearchField -> (Int, [(Int, String, SearchCtl)])
fieldValueWidth _ left w SFFind =
  let ctls = findLineCtls left w
      firstC = case ctls of ((c, _, _) : _) -> c; [] -> left + w
  in (max 4 (firstC - 1 - (left + searchFieldValueCol)), ctls)
fieldValueWidth ss left w SFReplace =
  let start = left + w - length replaceAllLabel
      ctls  = [(start, replaceAllLabel, CtlReplaceAll)]
      shown = if ssShowReplace ss then ctls else []
      right = if ssShowReplace ss then start else left + w
  in (max 4 (right - 1 - (left + searchFieldValueCol)), shown)
fieldValueWidth _ left w _ = (max 4 (left + w - 1 - (left + searchFieldValueCol)), [])

ctlStyle :: Theme -> SearchState -> SearchCtl -> Style
ctlStyle _ ss ctl = case ctl of
  CtlCase       -> if ssCase ss then onSty else offSty
  CtlWord       -> if ssWord ss then onSty else offSty
  CtlRegex      -> if ssRegex ss then onSty else offSty
  CtlReplToggle -> if ssShowReplace ss then onSty else offSty
  -- The Replace All button reverses to blue when it holds keyboard focus.
  CtlReplaceAll -> if S.focusedReplaceAll ss then Style BrightWhite Blue attrBold
                                             else Style Black Cyan attrBold
  where onSty  = Style Black Cyan attrBold
        offSty = Style BrightBlack Default attrNone

fieldOf :: SearchField -> SearchState -> SField
fieldOf SFFind    = ssFind
fieldOf SFReplace = ssReplace
fieldOf SFInclude = ssInclude
fieldOf SFExclude = ssExclude

-- A results row: a file header or a matching line.
drawResultRow :: Theme -> SearchState -> Surf s -> Int -> Int -> Int -> Int -> Int -> SRow -> Bool -> ST s ()
drawResultRow th ss arr cols rows r left w srow selected = do
  let selSty = Style BrightWhite Blue attrNone
      rowBg  = if selected then selSty else thText th
  fillRect arr cols rows r left 1 w rowBg
  case srow of
    SRFile fi -> case Seq.lookup fi (ssResults ss) of
      Just fr -> do
        let chev = if frCollapsed fr then '\x25b8' else '\x25be'   -- ▸ / ▾
            nm   = shortName (frPath fr)
            dir  = relPathTo (ssRoot ss) (frPath fr)
            cnt  = S.fileMatchCount fr
            cntS = show cnt ++ (if frTruncated fr then "+" else "")
            nmSty  = if selected then selSty { styleAttr = attrBold } else Style BrightWhite Default attrBold
            dirSty = if selected then selSty else Style BrightBlack Default attrNone
            cntSty = if selected then selSty else Style BrightBlack Default attrNone
            cntCol = left + w - length cntS - 1
        putCell arr cols rows r (left + 1) (Cell chev rowBg)
        -- The file header is an OSC 8 hyperlink to the matched file.
        drawStrL arr cols rows r (left + 3) nmSty (filePathUri (frPath fr))
                 (take (max 0 (cntCol - (left + 3) - 1)) nm)
        let dirCol = left + 3 + length nm + 1
        when (dirCol < cntCol) $
          drawStr arr cols rows r dirCol dirSty (take (max 0 (cntCol - dirCol - 1)) dir)
        drawStr arr cols rows r cntCol cntSty cntS
      _ -> pure ()
    SRMatch fi mi -> case Seq.lookup fi (ssResults ss) of
      Just fr -> case drop mi (frMatches fr) of
        (m : _) -> do
          let lnS   = show (mLine m + 1)
              gutW  = 6
              lnCol = left + 4
              lnSty = if selected then selSty else Style BrightBlack Default attrNone
              txtCol = left + 4 + gutW
              availT = max 1 (w - (txtCol - left) - 1)
              cols0  = mCols m
              firstC = case cols0 of ((c, _) : _) -> c; [] -> 0
              -- auto-scroll to the first match, plus the user's manual pan
              scroll = ssLeft ss + (if firstC >= availT then firstC - availT `div` 2 else 0)
              snip   = displaySnippet (mText m)
              seg    = take availT (drop scroll snip)
              hiSty  = if selected then selSty { styleAttr = attrBold, styleFg = BrightYellow }
                                   else Style Black Yellow attrNone
              inMatch col = any (\(c, l) -> col >= c && col < c + l) cols0
          drawStr arr cols rows r lnCol lnSty (take (gutW - 1) (lnS ++ repeat ' '))
          forM_ (zip [0 ..] seg) $ \(k, ch) ->
            let srcCol = scroll + k
                cst = if inMatch srcCol then hiSty else rowBg
            in putCell arr cols rows r (txtCol + k) (Cell ch cst)
        _ -> pure ()
      _ -> pure ()

-- Replace control chars (tabs etc.) with spaces so a snippet stays 1 char/cell
-- and match columns keep lining up.
displaySnippet :: Text -> String
displaySnippet = map (\c -> if c < ' ' then ' ' else c) . T.unpack

-- Path of a file relative to the search root (for the dimmed directory hint).
relPathTo :: FilePath -> FilePath -> String
relPathTo root p =
  let r = if not (null root) && last root == '/' then root else root ++ "/"
      rel = if r `isPrefixList` p then drop (length r) p else p
      d = reverse (dropWhile (/= '/') (reverse rel))
  in if null d then "" else init d   -- directory part, without trailing slash
  where isPrefixList a b = take (length a) b == a

------------------------------------------------------------------------------
-- Loading spinner (shown while a large file loads off the main thread)

drawLoading :: Theme -> Editor -> Layout -> (String, Int) -> Surf s -> ST s ()
drawLoading th _ed lo (name, frame) arr = do
  let cols = loCols lo; rows = loRows lo
      spin = spinnerFrames !! (frame `mod` length spinnerFrames)
      label = spin : "  Loading " ++ name ++ "\x2026"
      w = min (cols - 2) (max 24 (length label + 4))
      h = 3
      x = max 0 ((cols - w) `div` 2)
      y = max 0 ((rows - h) `div` 2)
      sty = thDialog th
  fillRect arr cols rows y x h w sty
  drawBox th arr cols rows y x h w sty
  drawStr arr cols rows (y + 1) (x + 2) sty (take (w - 4) label)

------------------------------------------------------------------------------
-- CSV table view

-- Columns visible starting at @left@, each taking width+1 (separator), within
-- @avail@ display columns: returns @(colIndex, xOffset)@ pairs.
visibleCsvCols :: Int -> [Int] -> Int -> [(Int, Int)]
visibleCsvCols left ws avail = go left 0
  where
    n = length ws
    go c x
      | c >= n = []
      | x + colW c + 1 > avail && c > left = []
      | otherwise = (c, x) : go (c + 1) (x + colW c + 1)
    colW c = ws !! c

padCenter :: Int -> String -> String
padCenter w s =
  let pad = max 0 (w - length s); l = pad `div` 2
  in replicate l ' ' ++ s ++ replicate (pad - l) ' '

-- Paint the active image into the text area. Uses the cached, pre-scaled cell
-- grid when it matches the current size/mode; otherwise scales on the fly
-- (so a frame is never wrong, only the cache makes it cheap).
drawImage :: Editor -> ImageDoc -> Layout -> Surf s -> ST s ()
drawImage ed idoc lo arr = do
  let cols = loCols lo; rows = loRows lo
      top  = loTextTop lo; left = loTextLeft lo
      th   = loTextHeight lo; tw = loTextWidth lo
      crop = imageCrop idoc
      pxk  = cellPxKey ed
  if imageOverlayActive ed
    then
      -- A real pixel placement will cover this area, so leave the cells blank:
      -- painting the half-block picture underneath lets its blocky edges and
      -- transparency checkerboard bleed through the overlay's transparent
      -- pixels (see 'imageOverlayActive'). Blank = terminal background, which
      -- is exactly what the overlay composites its own transparency over.
      forM_ [0 .. th-1] $ \r -> forM_ [0 .. tw-1] $ \c ->
        putCell arr cols rows (top+r) (left+c) blankCell
    else do
      let grid = case idCache idoc of
                   Just (c, r, m, cr, px, fr, g)
                     | c == tw && r == th && m == idMode idoc && cr == crop && px == pxk
                         && fr == idFrame idoc -> g
                   _ -> renderImage (cellAspect ed) (imageFitCap ed idoc) (idMode idoc) tw th crop (idImage idoc)
      forM_ [0 .. th-1] $ \r -> forM_ [0 .. tw-1] $ \c ->
        putCell arr cols rows (top+r) (left+c) (grid ! (r, c))
  -- While dragging, highlight the border of the selection rectangle.
  case idDrag idoc of
    Nothing -> pure ()
    Just (ar, ac, br, bc) -> do
      let r0 = min ar br; r1 = max ar br; c0 = min ac bc; c1 = max ac bc
      forM_ [r0 .. r1] $ \r -> forM_ [c0 .. c1] $ \c ->
        when (r == r0 || r == r1 || c == c0 || c == c1) $ do
          let rr = top + r; cc = left + c
          when (rr >= 0 && rr < rows && cc >= 0 && cc < cols) $ do
            cell <- readArray arr (rr*cols + cc)
            writeArray arr (rr*cols + cc)
              cell { cellStyle = (cellStyle cell)
                       { styleAttr = styleAttr (cellStyle cell) .|. attrReverse } }

drawCsvTable :: Theme -> Editor -> CsvView -> Layout -> Surf s -> ST s ()
drawCsvTable th ed v lo arr = do
  let cols = loCols lo; rows = loRows lo
      cl = loContentLeft lo
      gut = csvGutterWidthFor v
      headerRow = loTextTop lo
      ws = Csv.columnWidths v
      avail = cols - cl - gut
      visCols = visibleCsvCols (csvLeft v) ws avail
      curRow = csvCurRow v; curCol = csvCurCol v
      (sr0, sc0, sr1, sc1) = Csv.selRect v       -- highlighted rectangle
      inSelRow r = r >= sr0 && r <= sr1
      inSelCol c = c >= sc0 && c <= sc1
      hdr    = Style BrightWhite Blue attrBold
      hdrSel = Style Black Cyan attrBold
      gutS   = Style BrightBlack Default attrNone
      gutSel = Style BrightYellow Default attrBold
      cellS  = thText th
      cellSel = Style BrightWhite Blue attrNone     -- active cell
      cellSelExt = Style Black Cyan attrNone        -- other selected cells
      headRow0 = (thText th) { styleAttr = attrBold }
      sepS   = Style BrightBlack Default attrNone
  -- Column-header row (selected columns lit).
  fillRect arr cols rows headerRow cl 1 (cols - cl) hdr
  forM_ visCols $ \(c, x) -> do
    let w = ws !! c
        st = if inSelCol c then hdrSel else hdr
    drawStr arr cols rows headerRow (cl + gut + x) st (take w (padCenter w (Csv.colLabel c) ++ repeat ' '))
    putCell arr cols rows headerRow (cl + gut + x + w) (Cell '\x2502' hdr)
  -- Data rows (each can span several screen lines when a cell has newlines).
  forM_ (csvRowLayout (edFreezeHeader ed) v lo) $ \(r, sl, visH) -> do
    let numStr = show (r + 1)
        gstyle = if inSelRow r then gutSel else gutS
    drawStr arr cols rows sl (cl + max 0 (gut - 1 - length numStr)) gstyle numStr
    -- A row taller than the cap shows a '+' on its last visible gutter line.
    when (Csv.rowLineCount v r > Csv.maxCellLines && visH >= 2) $
      drawStr arr cols rows (sl + visH - 1) (cl + max 0 (gut - 2)) gstyle "+"
    forM_ visCols $ \(c, x) -> do
      let w = ws !! c
          isCursor = r == curRow && c == curCol
          editingHere = isCursor && Csv.isEditing v
          st | isCursor              = cellSel
             | inSelRow r && inSelCol c = cellSelExt
             | r == 0                = headRow0
             | otherwise             = cellS
          mCur = if editingHere then Just (maybe 0 fst (csvEdit v)) else Nothing
          dispLines = cellDisplay w visH mCur (Csv.cellAt r c v)
      forM_ (zip [0 ..] dispLines) $ \(li, s) -> do
        drawStr arr cols rows (sl + li) (cl + gut + x) st (take w (s ++ repeat ' '))
        putCell arr cols rows (sl + li) (cl + gut + x + w) (Cell '\x2502' sepS)

-- Screen layout of the visible table rows: (tableRow, firstScreenLine, height).
-- Rows have variable height (capped); the last one may be clipped at the bottom.
-- When @freeze@ is on, row 0 is pinned directly under the column header and the
-- scrolling rows (>= 1) flow beneath it.
csvRowLayout :: Bool -> CsvView -> Layout -> [(Int, Int, Int)]
csvRowLayout freeze v lo =
  let dataTop = loTextTop lo + 1
      dataEnd = loTextTop lo + loTextHeight lo
      walk sl r
        | sl >= dataEnd     = []
        | r >= Csv.nRows v  = []
        | otherwise         = let h = Csv.rowHeight v r
                              in (r, sl, min h (dataEnd - sl)) : walk (sl + h) (r + 1)
  in if freeze && Csv.nRows v > 0
       then let h0 = Csv.rowHeight v 0
            in (0, dataTop, min h0 (dataEnd - dataTop)) : walk (dataTop + h0) (max 1 (csvTop v))
       else walk dataTop (csvTop v)

-- The @visH@ display strings (each ≤ w wide) for a cell. When editing (mCur is
-- the in-cell cursor), the cell scrolls vertically to keep the cursor line shown
-- and that line scrolls horizontally to keep the cursor column shown.
cellDisplay :: Int -> Int -> Maybe Int -> Text -> [String]
cellDisplay w visH mCur cellText =
  let rawLines = map (map sani . T.unpack) (T.splitOn (T.pack "\n") cellText)
      n = length rawLines
      mLineCol = fmap (Csv.cursorLineCol cellText) mCur
      winTop = case mLineCol of
        Just (cl, _) | cl >= visH -> cl - visH + 1
        _                         -> 0
      lineAt idx
        | idx < 0 || idx >= n = replicate w ' '
        | otherwise = case mLineCol of
            Just (cl, cc) | cl == idx ->
              let off = if cc < w then 0 else cc - w + 1 in drop off (rawLines !! idx)
            _ -> clipStr w (rawLines !! idx)
  in [ lineAt (winTop + li) | li <- [0 .. visH - 1] ]
  where sani ch = if ch == '\t' then ' ' else ch

clipStr :: Int -> String -> String
clipStr w s = if length s <= w then s else take (max 0 (w - 1)) s ++ "\x2026"

-- Cursor for the cell being edited in CSV mode (on the right line + column of a
-- multi-line cell). Mirrors the windowing done by 'cellDisplay'.
csvCursor :: Editor -> CsvView -> Layout -> Maybe (Int, Int)
csvCursor ed v lo
  | not (Csv.isEditing v) = Nothing
  | otherwise = do
      let cleft = loContentLeft lo
          gut = csvGutterWidthFor v
          ws = Csv.columnWidths v
          avail = loCols lo - cleft - gut
          visCols = visibleCsvCols (csvLeft v) ws avail
          w = ws !! csvCurCol v
          (cl, cc) = Csv.cursorLineCol (Csv.currentCellText v) (maybe 0 fst (csvEdit v))
      x <- lookup (csvCurCol v) visCols
      (_, sl, visH) <- find (\(r, _, _) -> r == csvCurRow v) (csvRowLayout (edFreezeHeader ed) v lo)
      let winTop = if cl >= visH then cl - visH + 1 else 0
          li = cl - winTop
          off = if cc < w then 0 else cc - w + 1
          srow = sl + li
          scol = cleft + gut + x + (cc - off)
      if li >= 0 && li < visH && srow < loTextTop lo + loTextHeight lo && scol < loCols lo
        then Just (srow, scol) else Nothing

------------------------------------------------------------------------------
-- Cursor position

computeCursor :: Editor -> Layout -> Maybe (Int, Int)
computeCursor ed _ | isJust (edLoading ed) = Nothing
computeCursor ed lo = case edFocus ed of
  FMenu -> Nothing
  FBrowser -> Nothing
  FExplorer -> Nothing
  FDefPick -> Nothing
  FQuickOpen -> edQuickOpen ed >>= \qo ->
    let (y, x, _, w) = quickOpenGeom ed
        cur = sfCur (qoField qo)
        valW = max 1 (w - 6)
        off = if cur >= valW then cur - valW + 1 else 0
    in Just (y + 1, x + 4 + (cur - off))
  FSearch -> edSearch ed >>= searchCursor lo
  FDialog -> edDialog ed >>= dialogCursor ed lo
  -- The image view is read-only and has no cursor interaction: falling
  -- through to the text path would blink a cursor over the picture at the
  -- stale buffer's position, so hide it.
  FEdit | isJust (edImage ed) -> Nothing
  FEdit | Just v <- edCsv ed -> csvCursor ed v lo
  FEdit | edWordWrap ed ->
    let Pos l c = edCursor ed
        vrow = visualOffset ed (edTop ed) (edCursor ed)
        segs = lineSegs ed l
        (s, _) = segs !! segIndexOf segs c
        line = getLine' l (edBuffer ed)
        vcol = colToDisplay (tabWidthOf ed) c line - colToDisplay (tabWidthOf ed) s line
        sr = loTextTop lo + vrow
        sc = loTextLeft lo + vcol
    in if sr >= loTextTop lo && sr < loTextTop lo + loTextHeight lo && sc < loCols lo
         then Just (sr, sc) else Nothing
  FEdit ->
    let Pos l c = edCursor ed
        dcol = colToDisplay (tabWidthOf ed) c (currentLine ed)
        sr = loTextTop lo + (l - edTop ed)
        sc = loTextLeft lo + (dcol - edLeft ed)
    in if sr >= loTextTop lo && sr < loTextTop lo + loTextHeight lo
          && sc >= loTextLeft lo && sc < loCols lo
         then Just (sr, sc) else Nothing

-- Cursor in the focused search-panel input field (only when editing a field).
searchCursor :: Layout -> SearchState -> Maybe (Int, Int)
searchCursor lo ss = do
  fld <- S.focusedField ss
  let (top, left, _, w) = searchRegion lo
      hls = S.headerLines ss
      hlOf SFFind = HLFind; hlOf SFReplace = HLReplace; hlOf SFInclude = HLInclude; hlOf SFExclude = HLExclude
  ri <- lookupIndex (hlOf fld) hls
  let SField _ cur = fieldOf fld ss
      (valW, _) = fieldValueWidth ss left w fld
      vcol = left + searchFieldValueCol
      off  = if cur >= valW then cur - valW + 1 else 0
  Just (top + ri, vcol + (cur - off))
  where lookupIndex x xs = case [ i | (i, y) <- zip [0 ..] xs, y == x ] of (i : _) -> Just i; [] -> Nothing

dialogCursor :: Editor -> Layout -> Dialog -> Maybe (Int, Int)
dialogCursor ed lo d = case focusedField d of
  Nothing -> Nothing
  Just fi ->
    let (y, x, _, w) = dialogGeom ed d lo
        innerW = w - 4
        j = fieldRowIndex d fi
        f@(Field lbl t cur) = dlgFields d !! fi
        visH     = fieldVisH f
        labelW   = T.length lbl + 1
        valStart = (x + 2) + labelW
        valW     = max 1 (innerW - labelW)
        (cl, cc) = Csv.cursorLineCol t cur
        winTop   = if cl >= visH then cl - visH + 1 else 0
        off      = if cc < valW then 0 else cc - valW + 1
    in Just (y + 1 + j + (cl - winTop), valStart + (cc - off))

------------------------------------------------------------------------------
-- Diff to escape sequences

-- | Produce the escape-sequence stream to turn the previous screen into the
-- current one. Only the changed cell spans of each row are emitted (a size
-- change forces a clear and full redraw); unchanged regions emit nothing.
-- When two frames are the same content vertically shifted ('scrollPlan'), a
-- hardware scroll moves the band and only the newly exposed rows (plus any
-- overlay damage) are painted — scroll regions are VT100-core, so this needs
-- no capability gate.
renderFrame :: RenderCaps -> Maybe Screen -> Screen -> Builder
renderFrame caps mprev cur =
  let h = scrH cur
      sameSize = case mprev of
        Just p -> scrW p == scrW cur && scrH p == h
        Nothing -> False
      (body, esEnd) = case mprev of
        Just p | sameSize ->
          case scrollPlan p cur of
            Just (top, bot, delta, predScr) ->
              -- Blank rows exposed by SU/SD are erased with the current
              -- background: reset SGR first so they match 'blankCell'.
              let (d, es) = diffRows caps predScr cur
              in ( resetSgr <> setScrollRegion top bot
                     <> (if delta > 0 then scrollUp delta else scrollDown (negate delta))
                     <> resetScrollRegion
                     <> d
                 , es )
            Nothing -> diffRows caps p cur
        _ -> let (d, es) = foldl (\(acc, s) r -> let (b, s') = drawRow caps cur r s
                                                 in (acc <> b, s'))
                                 (mempty, emitState0) [0 .. h - 1]
             in (clearScreen <> d, es)
      curPart = case scrCursor cur of
        Just (r, c) -> moveTo r c <> showCursor
        Nothing -> hideCursor
      -- A frame never leaves a hyperlink open: whatever goes out next (the
      -- title, graphics escapes, the next frame) must not join the link.
      linkPart = case esLink esEnd of
        Just _  -> linkClose
        Nothing -> mempty
  in hideCursor <> body <> linkPart <> resetSgr <> curPart

-- | Decide whether to turn this frame transition into a hardware scroll:
-- when both frames carry matching hints with a plausible shift, build the
-- /predicted/ post-scroll screen (rows shifted within the band, exposed rows
-- blank) and count the band cells each strategy would repaint. Only when the
-- scroll genuinely saves work does it return @(top, bottom, delta, predicted)@;
-- the caller then diffs against the predicted screen, which repairs any part
-- of the prediction that was wrong (overlays, the scrollbar thumb, a sidebar)
-- — so this is an optimisation with no correctness surface.
scrollPlan :: Screen -> Screen -> Maybe (Int, Int, Int, Screen)
scrollPlan p cur = do
  hp <- scrHint p
  hc <- scrHint cur
  let delta = shPos hc - shPos hp
  if shTop hp /= shTop hc || shHeight hp /= shHeight hc || shKey hp /= shKey hc
       || delta == 0 || abs delta >= shHeight hc
    then Nothing
    else do
      let top = shTop hc
          hgt = shHeight hc
          predScr = shiftScreen p top hgt delta
          plainCost = bandDiffCells p cur top hgt
          predCost  = bandDiffCells predScr cur top hgt
      -- ~24 cells' worth of escape bytes for region setup + scroll + reset.
      if predCost + 24 < plainCost
        then Just (top, top + hgt - 1, delta, predScr)
        else Nothing

-- The screen as a terminal would show it after scrolling rows [top..top+h-1]
-- by delta (positive = content moves up): shifted rows inside the band,
-- blanks where rows were exposed, everything else untouched.
shiftScreen :: Screen -> Int -> Int -> Int -> Screen
shiftScreen p top hgt delta =
  let w = scrW p
      cellFor i =
        let (r, c) = i `divMod` w
        in if r < top || r >= top + hgt
             then scrCells p ! i
             else let src = r + delta
                  in if src >= top && src < top + hgt
                       then scrCells p ! (src * w + c)
                       else blankCell
  in p { scrCells = listArray (0, w * scrH p - 1)
                      [ cellFor i | i <- [0 .. w * scrH p - 1] ] }

-- How many cells inside the band differ between two same-size screens.
bandDiffCells :: Screen -> Screen -> Int -> Int -> Int
bandDiffCells a b top hgt =
  let w = scrW a
      lo = top * w
      hi = min (scrW a * scrH a) ((top + hgt) * w) - 1
  in length [ () | i <- [lo .. hi], scrCells a ! i /= scrCells b ! i ]

-- Repositioning the cursor costs ~8 bytes plus an SGR re-sync, while
-- re-emitting an unchanged cell costs a few, so runs of changed cells
-- separated by an unchanged gap up to this long are emitted as one run.
bridgeGap :: Int
bridgeGap = 12

-- | The SGR and OSC 8 hyperlink state threaded across a frame's emission
-- (the terminal keeps both across cursor moves), so a run only emits what
-- actually changes. 'Nothing' for the style means "unknown, emit before the
-- next cell"; 'Nothing' for the link means "no link open" — every frame both
-- starts and ends with no link open ('renderFrame' closes a trailing one).
data EmitState = EmitState
  { esStyle :: !(Maybe Style)
  , esLink  :: !(Maybe Text)
  }

emitState0 :: EmitState
emitState0 = EmitState Nothing Nothing

-- The escape prefix needed before painting a cell in the given state, and
-- the state afterwards. Opening a different link implicitly replaces the
-- previous one, so only a link→no-link transition emits a close.
cellPre :: RenderCaps -> Cell -> EmitState -> (Builder, EmitState)
cellPre caps cell es =
  let st = cellStyle cell
      lnk = cellLink cell
      preS = if esStyle es == Just st then mempty else styleSgrWith caps st
      preL | esLink es == lnk = mempty
           | otherwise        = maybe linkClose linkOpen lnk
  in (preS <> preL, EmitState (Just st) lnk)

-- Emit only the changed cell runs of each row, threading the SGR state across
-- the whole frame (the terminal keeps it across cursor moves), so a cursor
-- move or single-character edit costs a handful of bytes instead of full rows.
diffRows :: RenderCaps -> Screen -> Screen -> (Builder, EmitState)
diffRows caps p cur = foldl step (mempty, emitState0) [0 .. scrH cur - 1]
  where
    step (acc, es) r =
      let (b, es') = drawSpans caps cur r (rowSpans p cur r) es
      in (acc <> b, es')

-- The changed column spans of a row, bridging small unchanged gaps and
-- widening over wide-glyph continuation sentinels so glyphs repaint whole.
rowSpans :: Screen -> Screen -> Int -> [(Int, Int)]
rowSpans p cur r = go 0
  where
    w = scrW cur
    changed c = cellAt p w r c /= cellAt cur w r c
    go c
      | c >= w = []
      | not (changed c) = go (c + 1)
      | otherwise = let e  = grow (c + 1) c
                        a' = widenL c
                        e' = widenR e
                    in (a', e') : go (e' + 1)
    grow j lastHit
      | j >= w || j - lastHit > bridgeGap = lastHit
      | changed j = grow (j + 1) j
      | otherwise = grow (j + 1) lastHit
    widenL a
      | a > 0 && cellChar (cellAt cur w r a) == contChar = widenL (a - 1)
      | otherwise = a
    widenR e
      | e + 1 < w && cellChar (cellAt cur w r (e + 1)) == contChar = widenR (e + 1)
      | otherwise = e

-- Emit the changed runs of one row, carrying the frame's SGR state through.
-- With REP confirmed ('rcRep'), a run of identical ASCII cells collapses to
-- one character plus CSI n b.
drawSpans :: RenderCaps -> Screen -> Int -> [(Int, Int)] -> EmitState -> (Builder, EmitState)
drawSpans caps cur r spans es0 = foldl one (mempty, es0) spans
  where
    w = scrW cur
    one (acc, es) (a, e) = go (acc <> moveTo r a) es a
      where
        go acc' es' c
          | c > e = (acc', es')
          | otherwise =
              let cell = cellAt cur w r c
              in if cellChar cell == contChar
                   then go acc' es' (c + 1)   -- covered by the preceding wide glyph
                   else
                     let (pre, es'') = cellPre caps cell es'
                         n = cellRun caps cur w r c e cell
                     in go (acc' <> pre <> emitRun cell n) es'' (c + n)

cellAt :: Screen -> Int -> Int -> Int -> Cell
cellAt scr w r c = scrCells scr ! (r * w + c)

-- The length of the repeatable run starting at column c (bounded by e):
-- identical printable-ASCII cells only, and only worth 1 unless REP works.
cellRun :: RenderCaps -> Screen -> Int -> Int -> Int -> Int -> Cell -> Int
cellRun caps cur w r c e cell
  | not (rcRep caps) = 1
  | ch < ' ' || ch > '~' = 1
  | otherwise = go (c + 1)
  where
    ch = cellChar cell
    go j | j <= e && cellAt cur w r j == cell = go (j + 1)
         | otherwise = j - c

-- One cell, repeated: literal for short runs, REP for long ones (the emitted
-- char is the "preceding graphic character" REP repeats).
emitRun :: Cell -> Int -> Builder
emitRun cell n
  | n >= 4    = charUtf8 (cellChar cell) <> repChar (n - 1)
  | otherwise = mconcat (replicate n (charUtf8 (cellChar cell)))

-- Redraw a whole row, skipping wide-glyph continuation sentinels so physical
-- columns stay aligned. Emits SGR / link changes only at transitions; the
-- state threads across rows on the full-redraw path.
drawRow :: RenderCaps -> Screen -> Int -> EmitState -> (Builder, EmitState)
drawRow caps cur r es0 = go (moveTo r 0) es0 0
  where
    w = scrW cur
    go acc es c
      | c >= w = (acc, es)
      | otherwise =
          let cell = cellAt cur w r c
          in if cellChar cell == contChar
               then go acc es (c + 1)    -- covered by the preceding wide glyph
               else
                 let (pre, es') = cellPre caps cell es
                     n = cellRun caps cur w r c (w - 1) cell
                 in go (acc <> pre <> emitRun cell n) es' (c + n)
