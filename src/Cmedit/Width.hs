-- | Character display widths (a compact @wcwidth@) and the mapping between
-- character columns (indices into a line of text) and display columns (cells
-- on screen), accounting for tab stops and wide/zero-width characters.
module Cmedit.Width
  ( charWidth
  , isControlChar
  , controlCaret
  , isInvisibleFormat
  , isCombining
  , glyphCells
  , cellAdvance
  , strAdvance
  , strCells
  , textCells
  , takeCells
  , fitCells
  , dropCellsEnd
  , inputWindow
  , colToDisplay
  , displayToCol
  , windowStart
  , lineDisplayWidth
  , takeDisplay
  , wrapLine
  ) where

import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text (Text)

-- | The number of terminal cells a single character occupies when rendered.
-- Control characters return 0 here (callers render them via 'controlCaret',
-- which is two cells, or handle tabs specially) so that the pure width of a
-- code point is well defined; tab handling lives in the column-mapping
-- functions below.
charWidth :: Char -> Int
charWidth c
  | c == '\t'        = 0           -- handled by the tab-aware mappers
  | isControlChar c  = 0           -- rendered as ^X by the renderer
  | n < 0x0300       = 1           -- fast path: ASCII + Latin-1 letters
  | inRanges zeroWidth n = 0
  | inRanges wide n      = 2
  | otherwise            = 1
  where n = fromEnum c

-- | C0/C1 control characters (excluding tab, which is special).
isControlChar :: Char -> Bool
isControlChar c =
  let n = fromEnum c
  in (n < 0x20 && c /= '\t') || (n >= 0x7f && n < 0xa0)

-- | Formatting characters that are truly invisible: no glyph AND no modifier
-- effect on a neighbour. Terminals emit nothing for these and the cursor
-- does not advance, so the CSV table sizer / renderer treats them as zero
-- cells (unlike VS16 or ZWJ, which change how a neighbouring glyph is drawn
-- and keep a cell each, and combining marks, which 'isCombining' puts on
-- their glyph's cell). Kept small on purpose:
-- ZWSP, LTR/RTL marks, the four bidirectional embeddings and overrides,
-- the word joiner and the invisible arithmetic operators, and the BOM.
isInvisibleFormat :: Char -> Bool
isInvisibleFormat c =
  let n = fromEnum c
  in    n == 0x200B                             -- ZERO WIDTH SPACE
     || (n >= 0x200E && n <= 0x200F)            -- LTR / RTL MARK
     || (n >= 0x202A && n <= 0x202E)            -- LRE/RLE/PDF/LRO/RLO
     || (n >= 0x2060 && n <= 0x2064)            -- WJ + invisible ops
     || n == 0xFEFF                             -- BOM / ZWNBSP

-- | Caret notation for a control character, e.g. @\\NUL@ -> @"^@"@,
-- DEL -> @"^?"@.
controlCaret :: Char -> String
controlCaret c =
  let n = fromEnum c
  in if n == 0x7f then "^?"
     else if n < 0x20 then ['^', toEnum (n + 0x40)]
     else if n >= 0x80 && n < 0xa0 then ['^', '[']  -- approximate
     else [c]

-- | A combining mark: a character the terminal draws over the glyph before
-- it without advancing the cursor (accents, Thai and Hebrew vowel signs,
-- enclosing marks). These take no cell of their own; the renderer stores them
-- on the cell of the glyph they modify. The zero-width characters this
-- deliberately leaves out keep a cell each, as before: the invisible format
-- controls ('isInvisibleFormat'), the joiners, and the variation selectors,
-- which either draw nothing at all or change how wide a terminal draws their
-- neighbour, and which an editor has to be able to put a cursor on.
isCombining :: Char -> Bool
isCombining c =
  n >= 0x0300 && inRanges zeroWidth n
    && not (n >= 0x200B && n <= 0x200F) && not (n >= 0x202A && n <= 0x202E)
    && not (n >= 0x2060 && n <= 0x2064) && not (n >= 0xFE00 && n <= 0xFEFF)
  where n = fromEnum c

-- | Cells a printable character takes when it starts at display column
-- @disp@ of its line. A combining mark takes none, except at the very start
-- of a line, where there is no glyph for it to sit on and it is drawn on a
-- blank cell of its own; every other code point takes at least one. This is
-- the one rule the cursor maths here and the renderer's cell emission share.
glyphCells :: Int -> Char -> Int
glyphCells disp c
  | n < 0x0300    = 1
  | isCombining c = if disp == 0 then 1 else 0
  | otherwise     = max 1 (charWidth c)
  where n = fromEnum c
{-# INLINE glyphCells #-}

-- | 'glyphCells' for the text view, where a control character is drawn as a
-- two-cell caret pair.
cellAdvance :: Int -> Char -> Int
cellAdvance disp c
  | isControlChar c = 2
  | otherwise       = glyphCells disp c
{-# INLINE cellAdvance #-}

------------------------------------------------------------------------------
-- Strings outside the text view (menus, dialogs, panels, the status bar, table
-- cells). One convention for all of them, shared by the draw routines in
-- "Cmedit.Render": an invisible format control takes no cell and is not
-- drawn, a combining mark joins the glyph before it, a wide glyph takes two.
-- Clipping or positioning such a string by its /character/ count is what let
-- a CJK file name run past the edge of its panel, so these are what the
-- chrome measures and cuts with.

-- | Cells a character of such a string takes, starting at cell @disp@ of it.
strAdvance :: Int -> Char -> Int
strAdvance disp c
  | fromEnum c < 0x0300 = 1
  | isInvisibleFormat c = 0
  | otherwise           = glyphCells disp c
{-# INLINE strAdvance #-}

-- | Display width of a chrome string.
strCells :: String -> Int
strCells = go 0
  where go !d []       = d
        go !d (c : cs) = go (d + strAdvance d c) cs

-- | 'strCells' of a 'Text', without unpacking it.
textCells :: Text -> Int
textCells = T.foldl' (\d c -> d + strAdvance d c) 0

-- | The longest prefix that fits in @w@ cells. A wide glyph that would
-- straddle the boundary is left out whole; the marks and invisible controls
-- that follow the last glyph kept stay with it.
takeCells :: Int -> String -> String
takeCells w = go 0
  where
    go _ [] = []
    go !d (c : cs)
      | d + a > w = []
      | otherwise = c : go (d + a) cs
      where a = strAdvance d c

-- | 'takeCells', padded with spaces to exactly @w@ cells.
fitCells :: Int -> String -> String
fitCells w s = let t = takeCells w s in t ++ replicate (max 0 w - strCells t) ' '

-- | The longest /suffix/ that fits in @w@ cells (for paths, whose specific
-- end is the right-hand one). Never starts on a combining mark.
dropCellsEnd :: Int -> String -> String
dropCellsEnd w s = dropWhile isCombining (go (strCells s) s)
  where
    go !total rest@(c : cs)
      | total > w = go (total - strAdvance 1 c) cs
      | otherwise = rest
    go _ [] = []

-- | The horizontal window of a one-line input @w@ cells wide whose cursor is
-- before character @cur@: @(off, cx)@, the index of the first character to
-- draw and the cursor's cell within the window. The text scrolls only once
-- the cursor would leave the box, and then by just enough to keep it in the
-- last cell, measured in cells so wide text cannot push the cursor outside.
inputWindow :: Int -> Int -> String -> (Int, Int)
inputWindow w cur s
  | before < w = (0, before)
  | otherwise  = go 0 before s
  where
    before = strCells (take cur s)
    go !off !rest (c : cs)
      | rest > max 0 (w - 1) || (off < cur && isCombining c) = go (off + 1) (rest - strAdvance 1 c) cs
    go off rest _ = (off, max 0 rest)

-- | Display column at which character index @col@ begins, given a tab width.
-- Equivalent to the rendered width of @take col line@. A strict fold over the
-- text (no intermediate list) — this runs on every cursor move and repaint,
-- including on multi-megabyte single-line files. Uses 'cellAdvance' per
-- glyph to agree with the renderer's own cell emission
-- (@Render.expandLineCells@): at least one cell per code point (which keeps
-- the emoji variation selector U+FE0F accounted for as one column, matching
-- how terminals then fold it into a wide glyph), except a combining mark,
-- which shares its glyph's cell.
colToDisplay :: Int -> Int -> Text -> Int
colToDisplay tabw col line = T.foldl' step 0 (T.take col line)
  where
    step !disp c
      | c == '\t' = disp + (tabw - disp `mod` tabw)
      | otherwise = disp + cellAdvance disp c

-- | Character index whose start lies at or just past display column @target@.
-- Used to translate mouse clicks and to preserve a desired column during
-- vertical movement.
displayToCol :: Int -> Int -> Text -> Int
displayToCol tabw target line = go 0 0 (T.unpack line)
  where
    -- A position between a glyph and its combining marks is addressable with
    -- the arrow keys but is not somewhere a click or a vertical move should
    -- land: typing there would move the marks onto the new character.
    go !disp !i cs | disp >= target =
      if disp > 0 then i + length (takeWhile isCombining cs) else i
    go _     !i []                 = i
    go !disp !i (c:cs)
      | c == '\t' =
          let disp' = disp + (tabw - disp `mod` tabw)
          in if disp' > target then i else go disp' (i + 1) cs
      | otherwise =
          let w     = cellAdvance disp c
              disp' = disp + w
          in if disp' > target then i else go disp' (i + 1) cs

-- | Where to start expanding a horizontally-scrolled line: one character
-- before the one whose start reaches display column @target@ (so a glyph
-- straddling the left edge is included), as @(charIndex, itsDisplayColumn)@.
-- One scan, shared by the renderer instead of a displayToCol+colToDisplay
-- pair.
windowStart :: Int -> Int -> Text -> (Int, Int)
windowStart tabw target line = go 0 0 (0, 0) (T.unpack line)
  where
    go !disp _ prev _ | disp >= target = prev
    go _     _ prev []                 = prev
    go !disp !i prev (c:cs)
      | c == '\t' =
          let disp' = disp + (tabw - disp `mod` tabw)
          in if disp' > target then (i, disp) else go disp' (i + 1) (i, disp) cs
      | otherwise =
          let w     = cellAdvance disp c
              disp' = disp + w
          -- A combining mark is never the window's first character: its
          -- glyph is the one that may straddle the edge.
          in if disp' > target then (i, disp)
             else go disp' (i + 1) (if w == 0 then prev else (i, disp)) cs

-- | Total display width of a line.
lineDisplayWidth :: Int -> Text -> Int
lineDisplayWidth tabw line = colToDisplay tabw (T.length line) line

-- | Take as many characters from a line as fit within @width@ display cells,
-- returning the prefix text and its actual display width.
takeDisplay :: Int -> Int -> Text -> (Text, Int)
takeDisplay tabw width line = go 0 0 (T.unpack line) []
  where
    go !disp _ [] acc = (T.pack (reverse acc), disp)
    go !disp !_ (c:cs) acc
      | c == '\t' =
          let disp' = disp + (tabw - disp `mod` tabw)
          in if disp' > width then (T.pack (reverse acc), disp)
             else go disp' 0 cs (c:acc)
      | otherwise =
          let w     = cellAdvance disp c
              disp' = disp + w
          in if disp' > width then (T.pack (reverse acc), disp)
             else go disp' 0 cs (c:acc)

-- | Greedy word wrap. Split a line into a list of @(startCol, endCol)@
-- character-index ranges, each fitting within @width@ display cells, breaking
-- at spaces where possible and hard-breaking long words. Always returns at
-- least one (possibly empty) segment. Single pass over the line (each
-- character is re-walked at most once after a space break), so it stays
-- linear on huge lines; display columns are absolute so tab stops agree with
-- the unwrapped renderer.
wrapLine :: Int -> Int -> Text -> [(Int, Int)]
wrapLine tabw width line
  | width <= 0  = [(0, T.length line)]
  | T.null line = [(0, 0)]
  | otherwise   = go 0 0 (T.unpack line)
  where
    n = T.length line
    go start dispStart cs
      | start >= n = []
      | otherwise  = seg start dispStart Nothing start dispStart cs
    -- Walk one segment; @lastSp@ remembers the position just after the most
    -- recent space (break index, its display column, the chars after it).
    seg start dispStart lastSp !i !disp rest =
      case rest of
        [] -> [(start, n)]
        (c : cs) ->
          let w = if c == '\t' then tabw - disp `mod` tabw else cellAdvance disp c
              disp' = disp + w
          in if disp' - dispStart > width && i > start
               then case lastSp of
                      Just (j, dj, csJ) | j > start -> (start, j) : go j dj csJ
                      _                             -> (start, i) : go i disp rest
               else
                 let lastSp' = if isSpace c then Just (i + 1, disp', cs) else lastSp
                 in seg start dispStart lastSp' (i + 1) disp' cs

-- | Membership test over a sorted, non-overlapping list of inclusive ranges.
inRanges :: [(Int, Int)] -> Int -> Bool
inRanges rs n = go rs
  where
    go [] = False
    go ((lo, hi) : rest)
      | n < lo    = False      -- ranges are sorted; no later range can match
      | n <= hi   = True
      | otherwise = go rest

-- Zero-width: combining marks, zero-width spaces, joiners, BOM.
zeroWidth :: [(Int, Int)]
zeroWidth =
  [ (0x0300, 0x036F), (0x0483, 0x0489), (0x0591, 0x05BD), (0x05BF, 0x05BF)
  , (0x05C1, 0x05C2), (0x05C4, 0x05C5), (0x0610, 0x061A), (0x064B, 0x065F)
  , (0x0670, 0x0670), (0x06D6, 0x06DC), (0x06DF, 0x06E4), (0x06E7, 0x06E8)
  , (0x06EA, 0x06ED), (0x0711, 0x0711), (0x0730, 0x074A), (0x07A6, 0x07B0)
  , (0x07EB, 0x07F3), (0x0816, 0x0819), (0x081B, 0x0823), (0x0825, 0x0827)
  , (0x0829, 0x082D), (0x0859, 0x085B), (0x08E3, 0x0902), (0x093A, 0x093A)
  , (0x093C, 0x093C), (0x0941, 0x0948), (0x094D, 0x094D), (0x0951, 0x0957)
  , (0x0962, 0x0963), (0x0981, 0x0981), (0x09BC, 0x09BC), (0x09C1, 0x09C4)
  , (0x09CD, 0x09CD), (0x0A00, 0x0A02), (0x0A3C, 0x0A3C), (0x0A41, 0x0A51)
  , (0x0A70, 0x0A71), (0x0E31, 0x0E31), (0x0E34, 0x0E3A), (0x0E47, 0x0E4E)
  , (0x200B, 0x200F), (0x202A, 0x202E), (0x2060, 0x2064), (0xFE00, 0xFE0F)
  , (0xFEFF, 0xFEFF), (0x1AB0, 0x1AFF), (0x1DC0, 0x1DFF), (0x20D0, 0x20FF)
  ]

-- Wide (2-cell) ranges: CJK, Hangul, fullwidth forms, and every code point
-- with Emoji_Presentation=Yes (i.e. renders as a two-cell emoji by default,
-- with no U+FE0F selector). The BMP entries between 0x231A and 0x2B55 are the
-- misc-symbols/dingbats emoji (⌚ ⏳ ✨ ❌ ⛔ …); without them any CSV cell
-- containing one drifts the row's right-hand columns left by one cell per
-- glyph, because terminals render them wide but the sizer would count them
-- as narrow. Kept sorted for 'inRanges' early-exit.
wide :: [(Int, Int)]
wide =
  [ (0x1100, 0x115F)
  , (0x231A, 0x231B), (0x2329, 0x232A)
  , (0x23E9, 0x23EC), (0x23F0, 0x23F0), (0x23F3, 0x23F3)
  , (0x25FD, 0x25FE)
  , (0x2614, 0x2615), (0x2648, 0x2653), (0x267F, 0x267F), (0x2693, 0x2693)
  , (0x26A1, 0x26A1), (0x26AA, 0x26AB), (0x26BD, 0x26BE), (0x26C4, 0x26C5)
  , (0x26CE, 0x26CE), (0x26D4, 0x26D4), (0x26EA, 0x26EA), (0x26F2, 0x26F3)
  , (0x26F5, 0x26F5), (0x26FA, 0x26FA), (0x26FD, 0x26FD)
  , (0x2705, 0x2705), (0x270A, 0x270B), (0x2728, 0x2728)
  , (0x274C, 0x274C), (0x274E, 0x274E), (0x2753, 0x2755), (0x2757, 0x2757)
  , (0x2795, 0x2797), (0x27B0, 0x27B0), (0x27BF, 0x27BF)
  , (0x2B1B, 0x2B1C), (0x2B50, 0x2B50), (0x2B55, 0x2B55)
  , (0x2E80, 0x303E), (0x3041, 0x33FF)
  , (0x3400, 0x4DBF), (0x4E00, 0x9FFF), (0xA000, 0xA4CF), (0xA960, 0xA97F)
  , (0xAC00, 0xD7A3), (0xF900, 0xFAFF), (0xFE10, 0xFE19), (0xFE30, 0xFE6F)
  , (0xFF00, 0xFF60), (0xFFE0, 0xFFE6), (0x16FE0, 0x16FE4), (0x17000, 0x187FF)
  , (0x18800, 0x18AFF), (0x1B000, 0x1B16F), (0x1F004, 0x1F004)
  , (0x1F0CF, 0x1F0CF), (0x1F18E, 0x1F18E), (0x1F191, 0x1F19A)
  , (0x1F200, 0x1F320), (0x1F32D, 0x1F3FA), (0x1F400, 0x1F64F)
  , (0x1F680, 0x1F6FF), (0x1F7E0, 0x1F7EB), (0x1F7F0, 0x1F7F0)
  , (0x1F900, 0x1F9FF), (0x1FA70, 0x1FAFF)
  , (0x20000, 0x3FFFD)
  ]
