-- | ANSI / VT100 escape-sequence builders. Everything is produced as a
-- 'Builder' so a whole frame can be assembled and flushed in one syscall.
module Cmedit.Ansi
  ( -- * Builders
    esc
  , csi
  , osc
  , oscSt
    -- * Hyperlinks (OSC 8)
  , linkOpen
  , linkClose
    -- * Screen / cursor control
  , clearScreen
  , clearToEol
  , clearLine
  , moveTo
  , hideCursor
  , showCursor
  , cursorStyleBar
  , cursorStyleBlock
  , saveCursor
  , restoreCursor
    -- * Modes (setup / teardown strings)
  , enterAltScreen
  , leaveAltScreen
  , enableMouse
  , disableMouse
  , enableBracketedPaste
  , disableBracketedPaste
  , enableKittyKeys
  , disableKittyKeys
  , enableFocusEvents
  , disableFocusEvents
    -- * Synchronized output
  , beginSync
  , endSync
    -- * Scroll region (hardware scrolling)
  , setScrollRegion
  , resetScrollRegion
  , scrollUp
  , scrollDown
    -- * Repeat-character
  , repChar
    -- * Title stack
  , pushTitle
  , popTitle
    -- * Terminal queries (answered via 'Cmedit.Types.TermReply')
  , queryBg
  , queryCellPx
  , queryTextPx
  , queryVersion
  , queryDA1
  , queryCursorPos
  , repProbe
    -- * Pointer / cursor appearance / notifications
  , setPointerShape
  , setCursorColor
  , resetCursorColor
  , notify
    -- * Styling
  , RenderCaps(..)
  , plainCaps
  , resetSgr
  , styleSgr
  , styleSgrWith
  , setTitle
  ) where

import Data.ByteString.Builder (Builder, char7, intDec, string7, word8HexFixed)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Word (Word8)
import Cmedit.Link (linkIdOf)
import Cmedit.Types

-- | The ESC byte.
esc :: Builder
esc = char7 '\ESC'

-- | Control Sequence Introducer (@ESC[@) followed by the given body.
csi :: Builder -> Builder
csi b = esc <> char7 '[' <> b

-- | Operating System Command: @ESC ]@ body @BEL@. Terminals ignore OSC codes
-- they do not know, which is what makes the appearance hints below safe to
-- emit unconditionally.
osc :: Builder -> Builder
osc b = esc <> char7 ']' <> b <> char7 '\BEL'

-- | Like 'osc' but terminated with ST (@ESC \\@) instead of BEL — the
-- terminator the OSC 8 hyperlink spec prescribes. Unknown-OSC skipping in
-- terminals handles either framing, so this is equally safe unconditionally.
oscSt :: Builder -> Builder
oscSt b = esc <> char7 ']' <> b <> esc <> char7 '\\'

-- | Open an OSC 8 hyperlink: every printable cell emitted until 'linkClose'
-- (or the next 'linkOpen') becomes part of the link. The @id=@ parameter is
-- derived from the URI so a link spanning several runs, rows or frames
-- hovers as a single unit. The caller is responsible for the URI containing
-- no control bytes ("Cmedit.Link" produces percent-encoded targets).
linkOpen :: Text -> Builder
linkOpen uri =
  oscSt (string7 "8;id=" <> string7 (linkIdOf uri) <> char7 ';'
           <> encodeUtf8Builder uri)

-- | Close the open hyperlink (OSC 8 with an empty target).
linkClose :: Builder
linkClose = oscSt (string7 "8;;")

-- | Clear the whole screen.
clearScreen :: Builder
clearScreen = csi (string7 "2J")

-- | Erase from the cursor to the end of the line.
clearToEol :: Builder
clearToEol = csi (string7 "K")

-- | Erase the entire current line.
clearLine :: Builder
clearLine = csi (string7 "2K")

-- | Move the cursor to a 0-based (row, col), emitting the 1-based escape.
moveTo :: Int -> Int -> Builder
moveTo row col = csi (intDec (row + 1) <> char7 ';' <> intDec (col + 1) <> char7 'H')

hideCursor :: Builder
hideCursor = csi (string7 "?25l")

showCursor :: Builder
showCursor = csi (string7 "?25h")

-- | Steady vertical bar cursor (DECSCUSR 6).
cursorStyleBar :: Builder
cursorStyleBar = csi (string7 "6 q")

-- | Steady block cursor (DECSCUSR 2).
cursorStyleBlock :: Builder
cursorStyleBlock = csi (string7 "2 q")

saveCursor :: Builder
saveCursor = esc <> char7 '7'

restoreCursor :: Builder
restoreCursor = esc <> char7 '8'

-- | Switch to the alternate screen buffer and clear it.
enterAltScreen :: Builder
enterAltScreen = csi (string7 "?1049h")

-- | Restore the primary screen buffer.
leaveAltScreen :: Builder
leaveAltScreen = csi (string7 "?1049l")

-- | Turn on button + any-motion + SGR mouse reporting.
enableMouse :: Builder
enableMouse =
  csi (string7 "?1000h") <> csi (string7 "?1002h") <>
  csi (string7 "?1003h") <> csi (string7 "?1006h")

disableMouse :: Builder
disableMouse =
  csi (string7 "?1006l") <> csi (string7 "?1003l") <>
  csi (string7 "?1002l") <> csi (string7 "?1000l")

enableBracketedPaste :: Builder
enableBracketedPaste = csi (string7 "?2004h")

disableBracketedPaste :: Builder
disableBracketedPaste = csi (string7 "?2004l")

-- Kitty keyboard protocol: push the "disambiguate escape codes" flag so the
-- terminal reports keys like Shift+Enter / Ctrl+Enter as unambiguous CSI-u
-- sequences. Terminals that don't support it ignore these. Popped on exit.
enableKittyKeys :: Builder
enableKittyKeys = csi (string7 ">1u")

disableKittyKeys :: Builder
disableKittyKeys = csi (string7 "<u")

enableFocusEvents :: Builder
enableFocusEvents = csi (string7 "?1004h")

disableFocusEvents :: Builder
disableFocusEvents = csi (string7 "?1004l")

-- Synchronized output (mode 2026): the terminal buffers everything between
-- begin and end and commits it as one atomic update, so a frame can never be
-- displayed half-painted. Terminals without the mode ignore both.
beginSync :: Builder
beginSync = csi (string7 "?2026h")

endSync :: Builder
endSync = csi (string7 "?2026l")

-- | DECSTBM: restrict scrolling to rows @top..bot@ (0-based here; the escape
-- is 1-based). Side effect per the standard: the cursor homes.
setScrollRegion :: Int -> Int -> Builder
setScrollRegion top bot = csi (intDec (top + 1) <> char7 ';' <> intDec (bot + 1) <> char7 'r')

resetScrollRegion :: Builder
resetScrollRegion = csi (char7 'r')

-- | SU: move the content of the scroll region up @n@ rows (revealing blank
-- rows at the bottom, erased with the current background).
scrollUp :: Int -> Builder
scrollUp n = csi (intDec n <> char7 'S')

-- | SD: move the content of the scroll region down @n@ rows.
scrollDown :: Int -> Builder
scrollDown n = csi (intDec n <> char7 'T')

-- | REP: repeat the preceding graphic character @n@ more times. Only emitted
-- when the startup probe confirmed support ('rcRep'), since a terminal that
-- ignores it would silently drop cells.
repChar :: Int -> Builder
repChar n = csi (intDec n <> char7 'b')

-- Title stack (XTWINOPS): save the user's window title on entry and restore
-- it on exit so our OSC 0 title never clobbers the shell's.
pushTitle :: Builder
pushTitle = csi (string7 "22;0t")

popTitle :: Builder
popTitle = csi (string7 "23;0t")

------------------------------------------------------------------------------
-- Queries. Each is answered (by terminals that support it) with a sequence
-- the input parser decodes to a 'TermReply'; unsupported queries go
-- unanswered, so every consumer must treat "no reply" as the fallback.

-- | OSC 11 with a @?@ payload: report the terminal background colour.
queryBg :: Builder
queryBg = osc (string7 "11;?")

-- | XTWINOPS 16: report one character cell's size in pixels.
queryCellPx :: Builder
queryCellPx = csi (string7 "16t")

-- | XTWINOPS 14: report the text area's size in pixels.
queryTextPx :: Builder
queryTextPx = csi (string7 "14t")

-- | XTVERSION: report the terminal name and version (DCS > | .. ST reply).
queryVersion :: Builder
queryVersion = csi (string7 ">0q")

-- | Primary device attributes. Every terminal answers this, so it doubles as
-- a fence: replies to the queries sent before it arrive before its reply.
queryDA1 :: Builder
queryDA1 = csi (char7 'c')

-- | DECXCPR (@CSI ?6n@): report the cursor position with a @?@-prefixed reply,
-- which keeps it distinguishable from a modified-F3 keypress (plain @CSI ..R@).
queryCursorPos :: Builder
queryCursorPos = csi (string7 "?6n")

-- | Probe REP support empirically: home, print two spaces, ask the terminal
-- to repeat the last one twice, then ask where the cursor is. Column 5 means
-- REP worked, column 3 means it was ignored ('Cmedit.Caps.repProbeResult').
-- Runs on the freshly-cleared alternate screen, so the spaces are invisible
-- and the first full repaint wipes any trace.
repProbe :: Builder
repProbe = moveTo 0 0 <> string7 "  " <> repChar 2 <> queryCursorPos

------------------------------------------------------------------------------
-- Appearance hints (all ignored by terminals that lack them)

-- | OSC 22: the mouse pointer shape ("text", "default", "pointer", ...).
setPointerShape :: String -> Builder
setPointerShape shape = osc (string7 "22;" <> string7 shape)

-- | OSC 12: set the text cursor colour.
setCursorColor :: (Word8, Word8, Word8) -> Builder
setCursorColor (r, g, b) =
  osc (string7 "12;rgb:" <> word8HexFixed r <> char7 '/'
         <> word8HexFixed g <> char7 '/' <> word8HexFixed b)

-- | OSC 112: restore the terminal's own cursor colour.
resetCursorColor :: Builder
resetCursorColor = osc (string7 "112")

-- | OSC 9: post a desktop notification (iTerm2 convention, honoured by
-- kitty/WezTerm/Ghostty and friends; silently ignored elsewhere).
notify :: String -> Builder
notify msg = osc (string7 "9;" <> string7 (filter (\c -> c >= ' ' && c /= '\DEL') msg))

-- | Reset all styling to the terminal default.
resetSgr :: Builder
resetSgr = csi (string7 "0m")

-- | 'styleSgrWith' for a terminal with no negotiated extras.
styleSgr :: Style -> Builder
styleSgr = styleSgrWith plainCaps

-- | Emit a full SGR sequence that establishes exactly the given style
-- (it always starts from a reset so it is independent of previous state).
-- 'attrUndercurl' renders as the curly 4:3 form only when the caps allow it,
-- and as a plain underline otherwise.
styleSgrWith :: RenderCaps -> Style -> Builder
styleSgrWith caps (Style fg bg attr) =
  csi (string7 "0" <> attrCodes caps attr <> fgCodes fg <> bgCodes bg <> char7 'm')

attrCodes :: RenderCaps -> Attr -> Builder
attrCodes caps a =
  optc attrBold      ";1" <>
  optc attrDim       ";2" <>
  optc attrItalic    ";3" <>
  underlinePart      <>
  optc attrReverse   ";7"
  where
    optc flag s = if hasAttr flag a then string7 s else mempty
    underlinePart
      | hasAttr attrUndercurl a =
          string7 (if rcUndercurl caps then ";4:3" else ";4")
      | hasAttr attrUnderline a = string7 ";4"
      | otherwise               = mempty

fgCodes :: Color -> Builder
fgCodes c = case c of
  Default        -> mempty
  Black          -> n 30; Red     -> n 31; Green   -> n 32; Yellow -> n 33
  Blue           -> n 34; Magenta -> n 35; Cyan    -> n 36; White  -> n 37
  BrightBlack    -> n 90; BrightRed     -> n 91; BrightGreen  -> n 92
  BrightYellow   -> n 93; BrightBlue    -> n 94; BrightMagenta -> n 95
  BrightCyan     -> n 96; BrightWhite   -> n 97
  Color256 i     -> string7 ";38;5;" <> intDec (fromIntegral i)
  ColorRGB r g b -> string7 ";38;2;" <> intDec (fromIntegral r) <> char7 ';'
                      <> intDec (fromIntegral g) <> char7 ';' <> intDec (fromIntegral b)
  where n k = char7 ';' <> intDec k

bgCodes :: Color -> Builder
bgCodes c = case c of
  Default        -> mempty
  Black          -> n 40; Red     -> n 41; Green   -> n 42; Yellow -> n 43
  Blue           -> n 44; Magenta -> n 45; Cyan    -> n 46; White  -> n 47
  BrightBlack    -> n 100; BrightRed     -> n 101; BrightGreen  -> n 102
  BrightYellow   -> n 103; BrightBlue    -> n 104; BrightMagenta -> n 105
  BrightCyan     -> n 106; BrightWhite   -> n 107
  Color256 i     -> string7 ";48;5;" <> intDec (fromIntegral i)
  ColorRGB r g b -> string7 ";48;2;" <> intDec (fromIntegral r) <> char7 ';'
                      <> intDec (fromIntegral g) <> char7 ';' <> intDec (fromIntegral b)
  where n k = char7 ';' <> intDec k

-- | Set the terminal window title via OSC 0.
setTitle :: String -> Builder
setTitle t = esc <> char7 ']' <> char7 '0' <> char7 ';' <> string7 t <> char7 '\BEL'
