{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the page view's text selection: positioned per-character text
-- ('Pdf.pdfPageGlyphs') and turning a rectangle into copyable text and
-- highlight rectangles ('Pdf.pdfGlyphsIn', 'Pdf.pdfGlyphsText',
-- 'Pdf.pdfGlyphRects').
--
-- Spec.hs calls 'pdfSelectSpec' with its own @check@. The PDFs are tiny
-- documents written here (the object scan needs no xref). Positions are
-- checked against the page renderer ("Cmedit.PdfRender") through a Type 3
-- font, whose glyphs come out of the renderer as one 'CTransform' per glyph
-- with its origin at the pen position, so no font program is needed.
module PdfSelectSpec (pdfSelectSpec) where

import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (sortOn)
import qualified Data.Text as T
import System.Timeout (timeout)

import Cmedit.Geom
import Cmedit.Pdf
import Cmedit.PdfObj (Dict, PdfFile, fuelDefault, parsePdfFile, pdfPages)
import Cmedit.PdfRender (defaultRenderEnv, renderPage)
import Cmedit.Scene (Scene(..), Cmd(..))

pdfSelectSpec :: (String -> Bool -> IO ()) -> IO ()
pdfSelectSpec check = do
  basicTests check
  geometryTests check
  rectTests check
  spacingTests check
  columnTests check
  ligatureTests check
  hostileTests check

------------------------------------------------------------------------------
-- Fixtures

-- | A one-page document: extra page attributes, content, and extra objects
-- from 10 up. Object 5 is Helvetica (no /Widths: the built-in metrics), 6 a
-- Type 3 font whose codes A and B are 600 units wide under a 0.001 font
-- matrix, 7 Helvetica with a /ToUnicode map (object 8) sending A to U+FB03.
pdfDoc :: String -> String -> [(Int, String)] -> BS.ByteString
pdfDoc pageExtra content extra = BC.pack (concat
  ([ "%PDF-1.7\n" ]
   ++ [ show n ++ " 0 obj\n" ++ body ++ "\nendobj\n" | (n, body) <- objs ]
   ++ [ "trailer\n<< /Root 1 0 R >>\n%%EOF\n" ]))
  where
    stream = streamObj
    objs =
      [ (1, "<< /Type /Catalog /Pages 2 0 R >>")
      , (2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
      , (3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] "
            ++ "/Resources << /Font << /F1 5 0 R /T 6 0 R /L 7 0 R >> /XObject << /X 9 0 R >> >> "
            ++ "/Contents 4 0 R " ++ pageExtra ++ " >>")
      , (4, stream "" content)
      , (5, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
      , (6, "<< /Type /Font /Subtype /Type3 /FontBBox [0 0 1000 1000] /FontMatrix [0.001 0 0 0.001 0 0] "
            ++ "/CharProcs << /A 11 0 R /B 11 0 R >> /Encoding << /Type /Encoding /Differences [65 /A /B] >> "
            ++ "/FirstChar 65 /LastChar 66 /Widths [600 600] >>")
      , (11, stream "" "600 0 0 0 600 700 d1 0 0 600 700 re f")
      , (7, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /ToUnicode 8 0 R >>")
      , (8, stream "" "/CIDInit /ProcSet findresource begin 12 dict begin begincmap\n1 begincodespacerange <00> <FF> endcodespacerange\n1 beginbfchar\n<41> <FB03>\nendbfchar\nendcmap end end")
      ] ++ extra

streamObj :: String -> String -> String
streamObj dict dat = "<< " ++ dict ++ " /Length " ++ show (length dat) ++ " >>\nstream\n" ++ dat ++ "\nendstream"

-- | The file and its first page, or a failed check's worth of nothing.
page1 :: BS.ByteString -> Maybe (PdfFile, Dict)
page1 b = case parsePdfFile b of
  Right f | (p : _) <- pdfPages f -> Just (f, p)
  _ -> Nothing

glyphsOf :: BS.ByteString -> [PdfGlyph]
glyphsOf b = maybe [] (uncurry pdfPageGlyphs) (page1 b)

textOf :: BS.ByteString -> T.Text
textOf = pdfGlyphsText . glyphsOf

-- | The renderer's pen positions (scene space), in drawing order.
penPositions :: BS.ByteString -> [Pt]
penPositions b = case page1 b of
  Just (f, p) -> [ applyA m (Pt 0 0) | CTransform m _ <- allCmds (scCmds (fst (renderPage defaultRenderEnv f fuelDefault p))) ]
  Nothing -> []
  where
    allCmds = concatMap one
    one c = c : case c of
      CClip _ k -> allCmds k
      CGroup _ k -> allCmds k
      CTransform _ k -> allCmds k
      _ -> []

near :: Double -> Double -> Double -> Bool
near tol a b = abs (a - b) <= tol

-- Inside the glyph's box, give or take half a point.
onBox :: PdfGlyph -> Pt -> Bool
onBox g (Pt x y) = x >= pgX0 g - 0.5 && x <= pgX1 g + 0.5 && y >= pgTop g - 0.5 && y <= pgBot g + 0.5

------------------------------------------------------------------------------

-- "Hello world" in 12pt Helvetica at (72, 700): scene baseline y = 92.
-- Hello is 27.336pt wide, the space 3.336, world 28.668.
helloDoc :: BS.ByteString
helloDoc = pdfDoc "" "BT /F1 12 Tf 72 700 Td (Hello world) Tj 0 -14 Td (Second line) Tj ET" []

basicTests :: (String -> Bool -> IO ()) -> IO ()
basicTests check = do
  let gs = glyphsOf helloDoc
  check "pdfselect: one glyph per character, spaces included" (length gs == 22)
  check "pdfselect: glyph texts in content order" (T.concat (map pgText gs) == "Hello worldSecond line")
  check "pdfselect: pgSeq counts content order" (map pgSeq gs == [0 .. 21])
  check "pdfselect: whole page text keeps the line break" (pdfGlyphsText gs == "Hello world\nSecond line")
  check "pdfselect: boxes are ordered and abut along a word" (case gs of
    (h : e : _) -> pgX0 h <= pgX1 h && pgTop h < pgBot h && near 1e-9 (pgX1 h) (pgX0 e)
    _ -> False)
  check "pdfselect: Helvetica advance and baseline" (case gs of
    (h : _) -> near 1e-6 (pgX0 h) 72 && near 1e-6 (pgX1 h) (72 + 8.664) && near 1e-6 (pgBase h) 92
               && near 1e-6 (pgSize h) 12 && pgTop h < 92 && pgBot h > 92
    _ -> False)
  check "pdfselect: empty set gives empty text and no rects" (pdfGlyphsText [] == "" && null (pdfGlyphRects []))
  check "pdfselect: a page with no text has no glyphs" (null (glyphsOf (pdfDoc "" "0 0 m 10 10 l S" [])))
  check "pdfselect: the reflow extraction of the same page is unchanged"
    (either (const False) (\d -> pdfPlainText d == "Hello world Second line\n\n") (parsePdf helloDoc))

geometryTests :: (String -> Bool -> IO ()) -> IO ()
geometryTests check = do
  let content = "BT /T 10 Tf 20 50 Td (ABAB) Tj 0 -20 Td [(AB) -400 (BA)] TJ ET"
      agree name extra strong = do
        let d = pdfDoc extra content []
            gs = glyphsOf d
            pens = penPositions d
        check ("pdfselect: " ++ name ++ ": one glyph per renderer glyph") (length gs == 8 && length pens == 8)
        check ("pdfselect: " ++ name ++ ": renderer pen positions sit on the glyph boxes")
          (length gs == length pens && and (zipWith onBox gs pens))
        check ("pdfselect: " ++ name ++ ": exact pen origin") (not strong || (length gs == length pens
          && and (zipWith (\g (Pt x y) -> near 1e-6 (pgX0 g) x && near 1e-6 (pgBase g) y) gs pens)))
  agree "rotate 0" "" True
  agree "crop box offset" "/CropBox [10 30 500 700]" True
  agree "rotate 90" "/Rotate 90" False
  agree "rotate 90 with crop box" "/Rotate 90 /CropBox [10 30 500 700]" False
  agree "rotate 180" "/Rotate 180" False
  agree "rotate 270 with crop box" "/Rotate 270 /CropBox [10 30 500 700]" False
  -- Crop box [10 30 ...] on a 792 page whose crop top is 700: the pen at
  -- PDF (20, 50) is scene (10, 650).
  case glyphsOf (pdfDoc "/CropBox [10 30 500 700]" content []) of
    (g : _) -> check "pdfselect: crop box origin" (near 1e-6 (pgX0 g) 10 && near 1e-6 (pgBase g) 650 && near 1e-6 (pgX1 g) 16)
    [] -> check "pdfselect: crop box origin" False
  -- Rotated 90 clockwise, the line runs down the page: x is constant and the
  -- glyphs advance in y.
  let r90 = glyphsOf (pdfDoc "/Rotate 90" content [])
  check "pdfselect: rotate 90 advances downward" (case r90 of
    (a : b : _) -> pgTop b > pgTop a && near 1e-6 (pgX0 a) (pgX0 b) && near 1e-6 (pgX1 a - pgX0 a) 10
    _ -> False)
  -- Whatever the rotation, the copied text reads the same.
  let readsAs extra = pdfGlyphsText (glyphsOf (pdfDoc extra content [])) == "ABAB\nAB BA"
  check "pdfselect: rotated pages read the same" (all readsAs ["", "/Rotate 90", "/Rotate 180", "/Rotate 270 /CropBox [10 30 500 700]"])
  check "pdfselect: direction is recorded" (map pgDir (take 1 r90) == [1]
    && map pgDir (take 1 (glyphsOf (pdfDoc "/Rotate 270" content []))) == [3])
  -- Text set sideways on an upright page is its own block, read upward.
  let sideways = pdfDoc "" "BT /F1 12 Tf 72 700 Td (Body text) Tj ET BT /F1 12 Tf 0 1 -1 0 40 400 Tm (Label) Tj ET" []
  check "pdfselect: sideways text is a block of its own" (textOf sideways == "Body text\n\nLabel")
  check "pdfselect: sideways highlight is tall and narrow" (case pdfGlyphRects (filter ((== 3) . pgDir) (glyphsOf sideways)) of
    [Rect x0 y0 x1 y1] -> y1 - y0 > 20 && x1 - x0 < 13 && near 1e-6 y1 392
    _ -> False)
  -- The text rise moves the box, not the baseline.
  case glyphsOf (pdfDoc "" "BT /F1 10 Tf 72 700 Td (a) Tj 4 Ts (b) Tj ET" []) of
    [a, b] -> check "pdfselect: rise lifts the box" (near 1e-6 (pgBase a) (pgBase b) && near 1e-6 (pgTop a - pgTop b) 4)
    _ -> check "pdfselect: rise lifts the box" False

rectTests :: (String -> Bool -> IO ()) -> IO ()
rectTests check = do
  let gs = glyphsOf helloDoc
      sel r = pdfGlyphsText (pdfGlyphsIn r gs)
  check "pdfselect: a rectangle over one word takes the word" (sel (Rect 103 80 135 95) == "world")
  check "pdfselect: either corner first" (sel (Rect 135 95 103 80) == "world")
  check "pdfselect: half a word takes the half" (sel (Rect 72 80 86 95) == "He")
  check "pdfselect: two lines" (sel (Rect 60 80 200 110) == "Hello world\nSecond line")
  check "pdfselect: part of each of two lines" (sel (Rect 103 80 200 110) == "world\nd line")
  check "pdfselect: an empty rectangle takes nothing" (sel (Rect 100 90 100 90) == "" && null (pdfGlyphsIn (Rect 100 90 100 90) gs))
  check "pdfselect: a rectangle off the text takes nothing" (sel (Rect 300 300 400 400) == "")
  let rs = pdfGlyphRects (pdfGlyphsIn (Rect 60 80 200 110) gs)
  check "pdfselect: one highlight rectangle per line" (length rs == 2)
  check "pdfselect: highlight is the union of the line's boxes" (case rs of
    (Rect x0 y0 x1 y1 : Rect _ y0' _ _ : _) -> near 1e-6 x0 72 && near 1e-3 x1 131.34 && y0 < 92 && y1 > 92 && y0' > y0
    _ -> False)
  let rsWord = pdfGlyphRects (pdfGlyphsIn (Rect 98 80 135 95) gs)
  check "pdfselect: a selected leading space does not widen the highlight" (case rsWord of
    [Rect x0 _ _ _] -> near 1e-3 x0 102.672
    _ -> False)

spacingTests :: (String -> Bool -> IO ()) -> IO ()
spacingTests check = do
  check "pdfselect: a gap between two Tj runs is a space"
    (textOf (pdfDoc "" "BT /F1 12 Tf 72 700 Td (Hello) Tj 33 0 Td (world) Tj ET" []) == "Hello world")
  check "pdfselect: a kerning adjustment in TJ is not a space"
    (textOf (pdfDoc "" "BT /F1 12 Tf 72 700 Td [(Hel) -20 (lo)] TJ ET" []) == "Hello")
  check "pdfselect: a word-sized TJ adjustment is a space"
    (textOf (pdfDoc "" "BT /F1 12 Tf 72 700 Td [(Hello) -300 (world)] TJ ET" []) == "Hello world")
  check "pdfselect: an all-space string still separates words"
    (textOf (pdfDoc "" "BT /F1 12 Tf 72 700 Td (Hello) Tj ( ) Tj (world) Tj ET" []) == "Hello world")
  check "pdfselect: a superscript stays on its line"
    (textOf (pdfDoc "" "BT /F1 12 Tf 72 700 Td (x) Tj 3 3 Td /F1 7 Tf (2) Tj ET" []) == "x2")
  check "pdfselect: the same glyph drawn twice a hair apart is copied once"
    (textOf (pdfDoc "" "BT /F1 12 Tf 72 700 Td (Bold) Tj ET BT /F1 12 Tf 72.3 700 Td (Bold) Tj ET" []) == "Bold")
  check "pdfselect: lines come out top to bottom whatever the content order"
    (textOf (pdfDoc "" "BT /F1 12 Tf 72 600 Td (lower) Tj ET BT /F1 12 Tf 72 700 Td (upper) Tj ET" []) == "upper\nlower")

columnTests :: (String -> Bool -> IO ()) -> IO ()
columnTests check = do
  -- Two columns drawn row by row across the page: content order interleaves
  -- them, reading order must not.
  let row k = "BT /F1 10 Tf 72 " ++ show (700 - 14 * k) ++ " Td (Left line " ++ show k ++ ") Tj ET\n"
              ++ "BT /F1 10 Tf 330 " ++ show (700 - 14 * k) ++ " Td (Right line " ++ show k ++ ") Tj ET\n"
      d = pdfDoc "" (concatMap row [1 .. 8 :: Int]) []
      want = T.intercalate "\n" [ T.pack ("Left line " ++ show k) | k <- [1 .. 8 :: Int] ]
             <> "\n\n" <> T.intercalate "\n" [ T.pack ("Right line " ++ show k) | k <- [1 .. 8 :: Int] ]
  check "pdfselect: two columns come out column by column" (textOf d == want)
  check "pdfselect: a highlight per line per column" (length (pdfGlyphRects (glyphsOf d)) == 16)
  -- A rectangle over the left column alone is one column.
  let leftOnly = pdfGlyphsText (pdfGlyphsIn (Rect 60 0 200 792) (glyphsOf d))
  check "pdfselect: one column selected" (leftOnly == T.intercalate "\n" [ T.pack ("Left line " ++ show k) | k <- [1 .. 8 :: Int] ])

ligatureTests :: (String -> Bool -> IO ()) -> IO ()
ligatureTests check = do
  -- Code A is 667 units of Helvetica (8.004pt at 12pt) and maps to U+FB03.
  let gs = glyphsOf (pdfDoc "" "BT /L 12 Tf 72 700 Td (A) Tj ET" [])
  check "pdfselect: a ligature is spelled out, one glyph per letter" (map pgText gs == ["f", "f", "i"])
  check "pdfselect: a ligature's advance is split evenly" (case gs of
    [a, b, c] -> near 1e-6 (pgX0 a) 72 && near 1e-6 (pgX1 c) 80.004 && near 1e-6 (pgX1 a) (pgX0 b)
                 && near 1e-6 (pgX1 a - pgX0 a) (pgX1 b - pgX0 b)
    _ -> False)
  check "pdfselect: a third of a ligature" (pdfGlyphsText (pdfGlyphsIn (Rect 70 80 75 95) gs) == "f")
  -- An unmapped code draws no glyph (and its advance is still a gap).
  check "pdfselect: an unmapped code has no glyph"
    (map pgText (glyphsOf (pdfDoc "" "BT /F1 12 Tf 72 700 Td (A\\001B) Tj ET" [])) == ["A", "B"])

hostileTests :: (String -> Bool -> IO ()) -> IO ()
hostileTests check = do
  let forced d = do
        r <- timeout 20000000 (evaluate (let gs = glyphsOf d
                                             t = pdfGlyphsText gs
                                         in length gs + T.length t + length (pdfGlyphRects gs)))
        pure r
  -- A form that draws itself eight times: nesting and operator caps stop it.
  let selfForm = pdfDoc "" "/X Do"
        [ (9, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> /XObject << /X 9 0 R >> >>"
                        "BT /F1 12 Tf 72 700 Td (AAAA) Tj ET /X Do /X Do /X Do /X Do /X Do /X Do /X Do /X Do") ]
  r1 <- forced selfForm
  check "pdfselect: a self-drawing form terminates" (maybe False (> 0) r1)
  check "pdfselect: glyph count is capped" (length (glyphsOf selfForm) <= maxSelGlyphs)
  -- Garbage and unbalanced operators.
  r2 <- forced (pdfDoc "" "BT /F1 12 Tf [(((( ] TJ <<<< >> ] ) Tj Q Q Q q q 1 2 3 cm Tj Td Tm ET ET" [])
  check "pdfselect: a malformed content stream terminates" (r2 /= Nothing)
  -- A degenerate text matrix and a giant one.
  r3 <- forced (pdfDoc "" "BT /F1 12 Tf 0 0 0 0 0 0 Tm (AB) Tj 99999999999999999999 0 0 99999999999999999999 1 1 Tm (CD) Tj ET" [])
  check "pdfselect: degenerate matrices terminate" (r3 /= Nothing)
  let gs3 = glyphsOf (pdfDoc "" "BT /F1 12 Tf 99999999999999999999 0 0 99999999999999999999 1 1 Tm (CD) Tj ET" [])
  check "pdfselect: coordinates are finite" (all (\g -> all (\v -> not (isNaN v || isInfinite v)) [pgX0 g, pgX1 g, pgTop g, pgBot g, pgBase g]) gs3)
  -- A long string (the selection layout is not quadratic in a line).
  let long = pdfDoc "" ("BT /F1 1 Tf 0 400 Td (" ++ replicate 60000 'x' ++ ") Tj ET") []
  r4 <- forced long
  check "pdfselect: a long line is cheap" (r4 /= Nothing)
  -- Sorted output is stable whatever order the glyphs arrive in.
  let gs = glyphsOf helloDoc
  check "pdfselect: text does not depend on the order glyphs are given"
    (pdfGlyphsText (reverse gs) == pdfGlyphsText gs && pdfGlyphsText (sortOn pgX0 gs) == pdfGlyphsText gs)
