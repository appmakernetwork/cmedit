-- | The F1 keyboard quick-reference card: structured shortcut data plus the
-- pure layout that turns it into positioned styled cells. The renderer
-- overlays those cells on the blank canvas lines reserved at the top of the
-- help dialog's message -- the same mechanism as the About-box wordmark
-- ('Cmedit.About.aboutFrameCells'), so the dialog machinery itself stays
-- unstyled. Content is deliberately curated: the exhaustive reference lives
-- in the Manual ("Cmedit.Manual"), one button away.
--
-- Only single-width glyphs are emitted (no 'contChar' continuation concerns),
-- and the module must stay dependency-free (imports "Cmedit.Types" only)
-- since both Editor and Render import it.
module Cmedit.HelpCard
  ( helpCanvasH
  , helpCanvasMinW
  , helpDialogText
  , helpFrameCells
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Cmedit.Types

-- | A titled group of (key, action) rows.
type Section = (Text, [(Text, Text)])

-- The two side-by-side columns of the reference card. Keep keys and actions
-- short (they set the card's width) and stick to single-width characters.
leftSections :: [Section]
leftSections =
  [ ("Files",
      [ ("Ctrl+N",      "New file")
      , ("Ctrl+O",      "Open file\x2026")
      , ("Ctrl+P",      "Go to file\x2026")
      , ("Ctrl+,",      "Settings\x2026")
      , ("Ctrl+S",      "Save")
      , ("Ctrl+W",      "Close file")
      , ("Ctrl+Q",      "Quit")
      ])
  , ("Edit",
      [ ("Ctrl+Z/Y",    "Undo / redo")
      , ("Ctrl+X/C/V",  "Cut / copy / paste")
      , ("Ctrl+A",      "Select all")
      , ("Ctrl+D",      "Duplicate line")
      , ("Alt+\x2191/\x2193", "Move line")
      , ("Ctrl+/",      "Toggle comment")
      , ("Ctrl+Space",  "Complete word")
      ])
  , ("View",
      [ ("Alt+Z",       "Word wrap")
      , ("Alt+L",       "Line numbers")
      , ("Alt+T",       "CSV table view")
      , ("F10",         "Open the menus")
      ])
  ]

rightSections :: [Section]
rightSections =
  [ ("Find & Go",
      [ ("Ctrl+F",      "Find")
      , ("F3, Shift+F3", "Next / previous")
      , ("F8",          "Next problem")
      , ("Ctrl+R",      "Replace")
      , ("Ctrl+G",      "Go to line")
      , ("F4",          "Find in files")
      , ("F6",          "Replace in files")
      , ("F12",         "Go to definition")
      , ("Ctrl+]",      "Go to bracket")
      , ("Alt+\x2190/\x2192", "Back / forward")
      ])
  , ("Explorer",
      [ ("Ctrl+B",      "Show / focus panel")
      , ("Ins",         "New file / folder")
      , ("F2",          "Rename")
      , ("Del",         "Delete")
      ])
  , ("Windows",
      [ ("Alt+. / Alt+,", "Next / previous file")
      , ("Alt+1\x2026\&9", "Go to file 1-9")
      ])
  ]

-- Gap between the two columns, and the entries' indent under their header.
colGap, keyIndent :: Int
colGap = 4
keyIndent = 1

-- | One positioned styled run of text within a row.
data Run = Run !Int !Style !Text

-- All on the dialog's white chrome, like the About-box wordmark.
hdrSty, ruleSty, keySty, actSty :: Style
hdrSty  = Style Blue White attrBold
ruleSty = Style BrightBlack White attrNone
keySty  = Style Black White attrBold
actSty  = Style Black White attrNone

-- Width of a column's key field (keys are padded to it so actions align).
colKeyW :: [Section] -> Int
colKeyW ss = maximum (0 : [ T.length k | (_, es) <- ss, (k, _) <- es ])

colWidth :: [Section] -> Int
colWidth ss =
  let kw = colKeyW ss
  in maximum ( [ keyIndent + kw + 2 + T.length a | (_, es) <- ss, (_, a) <- es ]
            ++ [ T.length t + 2 | (t, _) <- ss ] )

-- One column as rows of runs: a ruled header per section, one row per entry,
-- a blank row between sections.
colRuns :: [Section] -> [[Run]]
colRuns ss =
  let kw = colKeyW ss
      w  = colWidth ss
      header t = [ Run 0 hdrSty t
                 , Run (T.length t + 1) ruleSty
                     (T.replicate (max 0 (w - T.length t - 1)) "\x2500") ]
      entry (k, a) = [ Run keyIndent keySty (T.justifyLeft kw ' ' k)
                     , Run (keyIndent + kw + 2) actSty a ]
      section (t, es) = header t : map entry es
  in concat (zipWith (\i s -> [ [] | i > (0 :: Int) ] ++ section s) [0 ..] ss)

-- | Height of the blank canvas the dialog reserves for the card.
helpCanvasH :: Int
helpCanvasH = max (length (colRuns leftSections)) (length (colRuns rightSections))

-- | The card's natural width; 'dialogGeom' widens the help dialog to it.
helpCanvasMinW :: Int
helpCanvasMinW = colWidth leftSections + colGap + colWidth rightSections

-- | The help dialog's message: the blank canvas the card is overlaid on,
-- plus a centred plain-text footer line (also the one non-blank line that
-- keeps 'dialogRows' from dropping the message entirely).
helpDialogText :: Text
helpDialogText = T.intercalate "\n" (replicate helpCanvasH "" ++ [footer])
  where
    footer = T.replicate (max 0 ((helpCanvasMinW - T.length msg) `div` 2)) " " <> msg
    msg    = "Full guide: the Manual button, or Help \x25b8 Manual"

-- | The card as positioned styled cells, centred in a body of width @w@
-- (cells that would not fit are clipped rather than escaping the box).
helpFrameCells :: Int -> [((Int, Int), Cell)]
helpFrameCells w =
  let lw = colWidth leftSections
      ox = max 0 ((w - helpCanvasMinW) `div` 2)
      place x0 rows = [ ((r, x), Cell ch sty)
                      | (r, runs) <- zip [0 ..] rows
                      , Run dx sty txt <- runs
                      , (i, ch) <- zip [0 ..] (T.unpack txt)
                      , let x = x0 + dx + i, x < w ]
  in place ox (colRuns leftSections)
  ++ place (ox + lw + colGap) (colRuns rightSections)
