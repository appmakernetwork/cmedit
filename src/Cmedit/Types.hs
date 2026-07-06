-- | Core shared types: keyboard/mouse events, colours, styles and screen
-- positions. Kept dependency-free so every other module can import it.
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
module Cmedit.Types
  ( -- * Geometry
    Pos(..)
  , origin
    -- * Cheap identity
  , ptrEq
    -- * Input events
  , Key(..)
  , TermReply(..)
  , Dir(..)
  , Mods(..)
  , noMods
  , shiftOnly
  , ctrlOnly
  , hasShift
  , hasCtrl
  , hasAlt
  , MouseEvent(..)
  , MouseButton(..)
    -- * Colours and styles
  , Color(..)
  , Attr
  , attrNone
  , attrBold
  , attrUnderline
  , attrReverse
  , attrItalic
  , attrDim
  , attrUndercurl
  , hasAttr
  , Style(..)
  , defaultStyle
  , Cell(Cell, CellL, cellChar, cellStyle, cellLink)
  , withLink
  , blankCell
    -- * Negotiated emitter capabilities
  , RenderCaps(..)
  , plainCaps
  ) where

import Data.Bits (Bits, (.&.), (.|.))
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

-- | Same heap object? GC can move objects between comparisons, so a mismatch
-- proves nothing (callers fall back to (==)) — but a match is a sound "equal".
-- Used to skip comparing shared substructure (undo snapshots, cache
-- validation) in O(1).
ptrEq :: a -> a -> Bool
ptrEq a b = isTrue# (reallyUnsafePtrEquality# a b)

-- | A position in the text buffer: 0-based line and 0-based column. Columns
-- count /characters/ (code points), not display cells; the renderer maps
-- character columns to display columns using 'Cmedit.Width'.
data Pos = Pos
  { posLine :: !Int
  , posCol  :: !Int
  } deriving (Eq, Ord, Show)

origin :: Pos
origin = Pos 0 0

-- | Direction for arrow-style navigation.
data Dir = DUp | DDown | DLeft | DRight
  deriving (Eq, Ord, Show)

-- | Keyboard modifier flags carried by navigation keys.
data Mods = Mods
  { modShift :: !Bool
  , modAlt   :: !Bool
  , modCtrl  :: !Bool
  } deriving (Eq, Ord, Show)

noMods :: Mods
noMods = Mods False False False

shiftOnly :: Mods
shiftOnly = Mods True False False

ctrlOnly :: Mods
ctrlOnly = Mods False False True

hasShift, hasCtrl, hasAlt :: Mods -> Bool
hasShift = modShift
hasCtrl  = modCtrl
hasAlt   = modAlt

-- | A fully decoded input event. The parser in "Cmedit.Input" produces these;
-- 'KResize' is injected by the SIGWINCH handler rather than the byte parser.
data Key
  = KChar !Char           -- ^ A printable character (already UTF-8 decoded).
  | KCtrlChar !Char       -- ^ Ctrl + the given (lower-case) letter or symbol.
  | KCtrlShiftChar !Char  -- ^ Ctrl+Shift + a (lower-case) letter (needs the Kitty protocol to be distinguishable from Ctrl+letter).
  | KAltChar !Char        -- ^ Alt/Meta + a printable character.
  | KFn !Int !Mods        -- ^ Function key F1..F12.
  | KArrow !Dir !Mods     -- ^ Arrow key with modifiers.
  | KHome !Mods
  | KEnd !Mods
  | KPageUp !Mods
  | KPageDown !Mods
  | KInsert !Mods
  | KDelete !Mods
  | KEnter
  | KModEnter            -- ^ Enter with a modifier (Ctrl/Shift/Alt): a newline, and a literal newline inside a CSV cell.
  | KTab
  | KBackTab              -- ^ Shift+Tab.
  | KBackspace
  | KEsc
  | KMouse !MouseEvent
  | KPaste !Text          -- ^ Bracketed-paste payload (Text: giant pastes must not materialise as a char list).
  | KResize               -- ^ Terminal was resized (injected, not parsed).
  | KFocus !Bool          -- ^ Terminal gained (True) / lost focus (CSI I / CSI O).
  | KReply !TermReply     -- ^ A terminal reply to one of our queries (driver-consumed, like 'KFocus').
  | KUnknown ![Word8]     -- ^ Bytes we could not interpret.
  deriving (Eq, Show)

-- | A decoded reply from the terminal to one of the queries the driver sends
-- at startup (colours, capabilities, pixel geometry). Produced by the input
-- parser, consumed by the driver in its key loop; the pure model ignores them
-- the same way it ignores 'KFocus'.
data TermReply
  = TrBgColor !Word8 !Word8 !Word8 -- ^ OSC 11 reply: the terminal's background colour (8-bit per channel).
  | TrDA1 ![Int]                   -- ^ Primary device attributes (CSI ? .. c): advertised feature params.
  | TrTermVersion !String          -- ^ XTVERSION reply (DCS > | text ST): terminal name/version.
  | TrCellPx !Int !Int             -- ^ XTWINOPS 16 reply (CSI 6;h;w t): one cell's (height, width) in pixels.
  | TrTextPx !Int !Int             -- ^ XTWINOPS 14 reply (CSI 4;h;w t): the text area's (height, width) in pixels.
  | TrCursorPos !Int !Int          -- ^ DECXCPR reply (CSI ? row ; col R), 1-based: used by the REP probe.
  | TrKittyGfx !Bool               -- ^ Kitty graphics probe reply: True when it came back "OK".
  deriving (Eq, Show)

data MouseButton = MBLeft | MBMiddle | MBRight
                 | MBWheelUp | MBWheelDown | MBWheelLeft | MBWheelRight
                 | MBNone
  deriving (Eq, Ord, Show)

-- | A decoded SGR mouse event. Coordinates are 0-based (col, row) into the
-- screen grid.
data MouseEvent = MouseEvent
  { meButton  :: !MouseButton
  , meCol     :: !Int
  , meRow     :: !Int
  , mePressed :: !Bool     -- ^ True on press, False on release.
  , meDrag    :: !Bool     -- ^ True if this is a motion (drag) report.
  , meMods    :: !Mods
  , meClicks  :: !Int      -- ^ Click count for a press (1 single, 2 double, 3 triple); set by the driver.
  } deriving (Eq, Show)

-- | A terminal colour. We support the 16 ANSI colours, the 256-colour cube and
-- direct 24-bit RGB, plus a "use the terminal default" sentinel.
data Color
  = Default
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  | BrightBlack | BrightRed | BrightGreen | BrightYellow
  | BrightBlue | BrightMagenta | BrightCyan | BrightWhite
  | Color256 !Word8
  | ColorRGB !Word8 !Word8 !Word8
  deriving (Eq, Ord, Show)

-- | Text attributes packed into a bitmask.
type Attr = Int

attrNone, attrBold, attrUnderline, attrReverse, attrItalic, attrDim, attrUndercurl :: Attr
attrNone      = 0
attrBold      = 1
attrUnderline = 2
attrReverse   = 4
attrItalic    = 8
attrDim       = 16
-- | Curly (undercurl) underline. Terminals that support the SGR 4:3 colon
-- form render a squiggle; elsewhere the renderer falls back to a plain
-- underline, so styles carrying this attr degrade gracefully.
attrUndercurl = 32

hasAttr :: Attr -> Attr -> Bool
hasAttr flag a = (a .&. flag) /= 0

-- | The visual style of a single cell.
data Style = Style
  { styleFg   :: !Color
  , styleBg   :: !Color
  , styleAttr :: !Attr
  } deriving (Eq, Show)

defaultStyle :: Style
defaultStyle = Style Default Default attrNone

-- | One character cell in the screen back-buffer. 'cellLink' is an OSC 8
-- hyperlink target: the diff emitter opens/closes the link around runs of
-- cells that carry it, and terminals without hyperlink support skip the OSC
-- string bytes entirely, so it needs no capability gate. Wide-glyph
-- continuation cells must carry the same link as their head cell.
data Cell = CellL
  { cellChar  :: !Char
  , cellStyle :: !Style
  , cellLink  :: !(Maybe Text)
  } deriving (Eq, Show)

-- | The common no-link cell, in constructor position: almost every cell has
-- no link target, so construction and matching stay two-field ('cellLink'
-- participates in Eq regardless, which is what the frame diff needs).
pattern Cell :: Char -> Style -> Cell
pattern Cell c s <- CellL c s _ where
  Cell c s = CellL c s Nothing
{-# COMPLETE Cell #-}

-- | Attach (or clear) a hyperlink target.
withLink :: Maybe Text -> Cell -> Cell
withLink l c = c { cellLink = l }

blankCell :: Cell
blankCell = Cell ' ' defaultStyle

-- | What the connected terminal is known to support, as far as the emitted
-- escape stream cares. Everything defaults to off ('plainCaps'); the driver
-- upgrades fields from probe replies, so an unknown terminal always gets the
-- portable byte stream.
data RenderCaps = RenderCaps
  { rcUndercurl :: !Bool   -- ^ SGR 4:3 curly underline (colon sub-parameters).
  , rcRep       :: !Bool   -- ^ REP (CSI n b) repeat-character, probe-confirmed.
  } deriving (Eq, Show)

plainCaps :: RenderCaps
plainCaps = RenderCaps False False
