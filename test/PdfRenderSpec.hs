-- | Tests for the vector stack (plan 0034): PdfRender.
--
-- Spec.hs calls 'pdfRenderSpec' with its own @check@, so a failure here is
-- counted and reported exactly like every other test. Fixtures are built
-- in code or checked in as bytes: the suite never shells out.
--
-- The PDFs are tiny documents written here (the object scan needs no xref),
-- and the one font program is a three-glyph TrueType built here too, used
-- both embedded (@FontFile2@) and as the "system" substitute the
-- environment hands back. Everything is checked structurally on the
-- 'Scene' (positions of glyphs and transforms, clip nesting, paints), never
-- through pixels: the rasteriser has its own spec.
module PdfRenderSpec (pdfRenderSpec) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.IntMap.Strict as IM
import Data.List (isInfixOf, nub)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Word (Word8)
import System.Timeout (timeout)
import GHC.Stats (getRTSStatsEnabled, getRTSStats, RTSStats(..))

import qualified Data.Array.Unboxed as U

import Cmedit.Font (Font, parseFont)
import Cmedit.Image (Image(..))
import Cmedit.FontSys (FontRequest(..))
import Cmedit.Geom
import Cmedit.PdfObj
import Cmedit.PdfRender
import qualified Cmedit.IntervalMap as IV
import qualified Cmedit.Pdf as Pdf
import Cmedit.PdfRenderFont (parseCMapBytes, splitCodes, cmapCid)
import Cmedit.Scene

pdfRenderSpec :: (String -> Bool -> IO ()) -> IO ()
pdfRenderSpec check = do
  intervalSpec check
  hostileSpec check
  geometrySpec check
  textSpec check
  fontSpec' check
  graphicsSpec check
  structureSpec check
  boundsSpec check

------------------------------------------------------------------------------
-- Fixtures

bs :: String -> BS.ByteString
bs = BC.pack

pdfOf :: [(Int, BS.ByteString)] -> BS.ByteString
pdfOf objs = BS.concat $
  [bs "%PDF-1.7\n"]
  ++ [ BS.concat [bs (show n ++ " 0 obj\n"), body, bs "\nendobj\n"] | (n, body) <- objs ]
  ++ [bs "trailer\n<< /Root 1 0 R >>\n%%EOF\n"]

streamObj :: String -> BS.ByteString -> BS.ByteString
streamObj dict dat = BS.concat [bs ("<< " ++ dict ++ " /Length " ++ show (BS.length dat) ++ " >>\nstream\n"), dat, bs "\nendstream"]

-- A one-page document: page attributes, resources, content, extra objects
-- from 10 up, and extra catalog entries.
docFull :: String -> String -> String -> String -> [(Int, BS.ByteString)] -> BS.ByteString
docFull catExtra pageExtra res content extra = pdfOf $
  [ (1, bs ("<< /Type /Catalog /Pages 2 0 R " ++ catExtra ++ " >>"))
  , (2, bs "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
  , (3, bs ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 200 100] /Resources << " ++ res ++ " >> /Contents 4 0 R " ++ pageExtra ++ " >>"))
  , (4, streamObj "" (bs content)) ]
  ++ extra

doc :: String -> String -> [(Int, BS.ByteString)] -> BS.ByteString
doc = docFull "" ""

render :: RenderEnv -> BS.ByteString -> Scene
render env b = case parsePdfFile b of
  Left _ -> Scene (Rect 0 0 0 0) [] ["parse failed"]
  Right f -> case pdfPages f of
    (p : _) -> fst (renderPage env f fuelDefault p)
    [] -> Scene (Rect 0 0 0 0) [] ["no pages"]

renderD :: BS.ByteString -> Scene
renderD = render defaultRenderEnv

-- Every command, containers included, pre-order.
allCmds :: [Cmd] -> [Cmd]
allCmds = concatMap one
  where
    one c = c : case c of
      CClip _ k -> allCmds k
      CGroup _ k -> allCmds k
      CTransform _ k -> allCmds k
      _ -> []

-- Every leaf with its accumulated transform (to scene space).
leaves :: Affine -> [Cmd] -> [(Affine, Cmd)]
leaves m = concatMap one
  where
    one c = case c of
      CClip _ k -> leaves m k
      CGroup _ k -> leaves m k
      CTransform t k -> leaves (composeA m t) k
      _ -> [(m, c)]

glyphsOf :: Scene -> [(Affine, Glyph)]
glyphsOf sc = [ (composeA m ca, g) | (m, CGlyphs ca _ gs) <- leaves identityA (scCmds sc), g <- gs ]

transformsOf :: Scene -> [Affine]
transformsOf sc = [ m | CTransform m _ <- allCmds (scCmds sc) ]

near :: Double -> Double -> Bool
near a b = abs (a - b) < 1e-6

nearP :: Pt -> Pt -> Bool
nearP (Pt a b) (Pt c d) = abs (a - c) < 1e-4 && abs (b - d) < 1e-4

origin :: Affine -> Pt
origin m = applyA m (Pt 0 0)

pts :: [Pt] -> [Pt] -> Bool
pts a b = length a == length b && and (zipWith nearP a b)

hasNote :: String -> Scene -> Bool
hasNote s sc = any (s `isInfixOf`) (scNotes sc)

------------------------------------------------------------------------------
-- A three-glyph TrueType font: glyph 1 a 500x700 box, 2 a triangle, 3 a
-- narrow bar; advances 600, 700 and 700 (glyph 0 has 500; glyph 3 shares
-- the last metric).
-- cmap (3,1) maps 'A'..'C' to 1..3; (3,0) maps 0xF041..0xF043 to 1..3.

be16, be32 :: Int -> [Word8]
be16 v = [fromIntegral (v `shiftR` 8), fromIntegral v]
be32 v = [fromIntegral (v `shiftR` 24), fromIntegral (v `shiftR` 16), fromIntegral (v `shiftR` 8), fromIntegral v]

testFontBytes :: BS.ByteString
testFontBytes = BS.concat (header : dir : map (pad . snd) tables)
  where
    pad b = b <> BS.replicate ((4 - BS.length b `mod` 4) `mod` 4) 0
    tables = [ ("cmap", cmap), ("glyf", glyf), ("head", headT), ("hhea", hhea), ("hmtx", hmtx)
             , ("loca", loca), ("maxp", maxp), ("post", post) ]
    n = length tables
    header = BS.pack ([0, 1, 0, 0] ++ be16 n ++ be16 0 ++ be16 0 ++ be16 0)
    offs = scanl (+) (12 + 16 * n) [ BS.length (pad t) | (_, t) <- tables ]
    dir = BS.concat [ bs tag <> BS.pack (be32 0 ++ be32 o ++ be32 (BS.length t)) | ((tag, t), o) <- zip tables offs ]
    glyph pts = BS.pack (be16 1 ++ concatMap be16 [minimum xs, minimum ys, maximum xs, maximum ys]
                         ++ be16 (length pts - 1) ++ be16 0 ++ replicate (length pts) 1
                         ++ concatMap be16 (deltas xs) ++ concatMap be16 (deltas ys))
      where xs = map fst pts; ys = map snd pts
            deltas vs = map (.&. 0xFFFF) (zipWith (-) vs (0 : vs))
    glyphs = map even' [ BS.empty, glyph [(0, 0), (500, 0), (500, 700), (0, 700)]
             , glyph [(0, 0), (600, 0), (300, 600)], glyph [(100, 0), (200, 0), (200, 700), (100, 700)] ]
    even' g = if odd (BS.length g) then g <> BS.singleton 0 else g
    glyf = BS.concat glyphs
    loca = BS.pack (concatMap (be16 . (`div` 2)) (scanl (+) 0 (map BS.length glyphs)))
    headT = BS.pack (be32 0x00010000 ++ be32 0x00010000 ++ be32 0 ++ be32 0x5F0F3CF5 ++ be16 0
                     ++ be16 1000 ++ replicate 16 0 ++ concatMap be16 [0, (-200) .&. 0xFFFF, 1000, 800]
                     ++ be16 0 ++ be16 8 ++ be16 2 ++ be16 0 ++ be16 0)
    maxp = BS.pack (be32 0x00005000 ++ be16 4)
    hhea = BS.pack (be32 0x00010000 ++ be16 800 ++ be16 ((-200) .&. 0xFFFF) ++ replicate 26 0 ++ be16 3)
    hmtx = BS.pack (concat [ be16 a ++ be16 0 | a <- [500, 600, 700] ] ++ be16 0)
    f4 starts ends' firstG =
      let segs = length starts + 1
          deltas = [ (firstG - s + k) .&. 0xFFFF | (s, k) <- zip starts [0, 0 ..] ] ++ [1]
          body = be16 (segs * 2) ++ be16 2 ++ be16 0 ++ be16 0 ++ concatMap be16 (ends' ++ [0xFFFF]) ++ be16 0
                 ++ concatMap be16 (starts ++ [0xFFFF]) ++ concatMap be16 deltas ++ concatMap be16 (replicate segs 0)
      in be16 4 ++ be16 (6 + length body) ++ be16 0 ++ body
    sub31 = f4 [0x41] [0x43] 1
    sub30 = f4 [0xF041] [0xF043] 1
    cmap = BS.pack (be16 0 ++ be16 2 ++ be16 3 ++ be16 0 ++ be32 20 ++ be16 3 ++ be16 1 ++ be32 (20 + length sub30)
                    ++ sub30 ++ sub31)
    post = BS.pack (be32 0x00030000 ++ replicate 28 0)

testFont :: Font
testFont = either (error . ("test font: " ++)) id (parseFont testFontBytes)

-- An environment whose only "system font" is the test font.
fontEnv :: RenderEnv
fontEnv = defaultRenderEnv { reFonts = const (Just (7, testFont)) }

------------------------------------------------------------------------------

geometrySpec :: (String -> Bool -> IO ()) -> IO ()
geometrySpec check = do
  let page extra = docFull "" ("/CropBox [10 10 190 90] " ++ extra) "" "1 0 0 rg 20 20 10 5 re f" []
      rectAt sc = [ map (applyA m) [ p | LineTo p <- path ] ++ [ applyA m p | MoveTo p <- path ]
                  | (m0, CFill m1 _ (PSolid (Color 1 0 0 1)) path) <- leaves identityA (scCmds sc), let m = composeA m0 m1 ]
      bboxOf ps = let xs = [ x | Pt x _ <- ps ]; ys = [ y | Pt _ y <- ps ] in (minimum xs, minimum ys, maximum xs, maximum ys)
  let s0 = renderD (page "")
  check "pdfrender: crop box sets the scene bounds" (scBounds s0 == Rect 0 0 180 80)
  check "pdfrender: first command is the paper" (case scCmds s0 of
    (CFill _ _ (PSolid c) _ : _) -> c == white
    _ -> False)
  check "pdfrender: content is clipped to the page" (case scCmds s0 of
    [_, CClip [ClipShape _ _ p] _] -> pathBounds p == Just (Rect 0 0 180 80)
    _ -> False)
  -- The rectangle (20,20)-(30,25) in PDF space, crop origin (10,10), page
  -- height 80 after cropping: y-down scene coordinates.
  check "pdfrender: rotate 0 maps y down" (map bboxOf (rectAt s0) == [(10, 65, 20, 70)])
  let s90 = renderD (page "/Rotate 90")
  check "pdfrender: rotate 90 swaps the bounds" (scBounds s90 == Rect 0 0 80 180)
  check "pdfrender: rotate 90 places content clockwise" (map bboxOf (rectAt s90) == [(10, 10, 15, 20)])
  let s180 = renderD (page "/Rotate 180")
  check "pdfrender: rotate 180" (scBounds s180 == Rect 0 0 180 80 && map bboxOf (rectAt s180) == [(160, 10, 170, 15)])
  let s270 = renderD (page "/Rotate 270")
  check "pdfrender: rotate 270" (scBounds s270 == Rect 0 0 80 180 && map bboxOf (rectAt s270) == [(65, 160, 70, 170)])
  let sBad = renderD (page "/Rotate 45")
  check "pdfrender: a rotation that is not a multiple of 90 is ignored" (scBounds sBad == Rect 0 0 180 80)

------------------------------------------------------------------------------
-- Text arithmetic through a Type 3 font (no font program needed): each
-- glyph is a CTransform whose origin is the pen position.

type3Res :: String
type3Res = "/Font << /T 10 0 R >>"

type3Objs :: [(Int, BS.ByteString)]
type3Objs =
  [ (10, bs "<< /Type /Font /Subtype /Type3 /FontBBox [0 0 1000 1000] /FontMatrix [0.001 0 0 0.001 0 0] \
            \/CharProcs << /a 11 0 R /sp 12 0 R >> /Encoding << /Differences [32 /sp 65 /a] >> \
            \/FirstChar 32 /LastChar 65 /Widths [250 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1000] >>")
  , (11, streamObj "" (bs "1000 0 0 0 1000 1000 d1 0 1 0 rg 0 0 1000 1000 re f"))
  , (12, streamObj "" (bs "250 0 d0 1 0 0 rg 0 0 10 10 re f")) ]

-- The font requests of a document's first page (one bogus request when the
-- fixture does not parse, so a broken fixture cannot pass a "no needs" test).
needsOf :: BS.ByteString -> [FontRequest]
needsOf b = case parsePdfFile b of
  Right f | (p : _) <- pdfPages f -> fst (pageFontNeeds f fuelDefault p)
  _ -> [FontRequest T.empty False False False False False]

penXs :: String -> [Double]
penXs content =
  let sc = render defaultRenderEnv (doc type3Res content type3Objs)
  in [ x | m <- transformsOf sc, let Pt x _ = origin m ]

textSpec :: (String -> Bool -> IO ()) -> IO ()
textSpec check = do
  let xsTj = penXs "BT /T 10 Tf 20 50 Td (AA) Tj ET"
  check "pdfrender: Tj positions" (xsTj == [20, 30])
  check "pdfrender: Tc adds per glyph" (penXs "BT /T 10 Tf 2 Tc 20 50 Td (AAA) Tj ET" == [20, 32, 44])
  check "pdfrender: Tw applies to code 32 only" (penXs "BT /T 10 Tf 5 Tw 20 50 Td (A A) Tj ET" == [20, 30, 37.5])
  check "pdfrender: Tz scales the advance" (penXs "BT /T 10 Tf 50 Tz 20 50 Td (AAA) Tj ET" == [20, 25, 30])
  check "pdfrender: TJ adjustments are thousandths of the font size" (penXs "BT /T 10 Tf 20 50 Td [(A) -500 (A) 1000 (A)] TJ ET" == [20, 35, 35])
  check "pdfrender: TJ adjustment scales with Tz" (penXs "BT /T 10 Tf 50 Tz 20 50 Td [(A) -1000 (A)] TJ ET" == [20, 30])
  let ys = [ y | m <- transformsOf (render defaultRenderEnv (doc type3Res "BT /T 10 Tf 12 TL 20 50 Td (A) Tj T* (A) Tj 3 Ts (A) Tj ET" type3Objs))
               , let Pt _ y = origin m ]
  -- Scene y = 100 - PDF y: baseline 50, next line 38, rise 3 on that line.
  check "pdfrender: T* and Ts move the baseline" (ys == [50, 62, 59])
  check "pdfrender: Tm sets the matrix" (penXs "BT /T 10 Tf 1 0 0 1 100 20 Tm (A) Tj ET" == [100])
  check "pdfrender: ' and \" show on the next line" (length (penXs "BT /T 10 Tf 10 TL 20 50 Td (A) ' 1 2 (A) \" ET") == 2)
  -- The glyph procedure's square is drawn in glyph space: the transform is
  -- the font size times the font matrix.
  let ms = transformsOf (renderD (doc type3Res "BT /T 20 Tf 20 50 Td (A) Tj ET" type3Objs))
  check "pdfrender: Type 3 glyph transform includes size and FontMatrix" (case ms of
    [Affine a b c d _ _] -> near a 0.02 && near b 0 && near c 0 && near d (-0.02)
    _ -> False)
  -- d1 glyphs take the current fill colour; d0 glyphs keep their own.
  let sc = renderD (doc type3Res "BT /T 10 Tf 0 0 1 rg 20 50 Td (A ) Tj ET" type3Objs)
      fills = [ c | CFill _ _ (PSolid c) _ <- concatMap kidsOf (transformsWithKids sc) ]
  check "pdfrender: d1 glyph uses the fill colour, d0 its own" (fills == [Color 0 0 1 1, Color 1 0 0 1])
  -- Sharing: ten copies of a glyph cost the procedure's tokens once.
  let spent content = case parsePdfFile (doc type3Res content type3Objs) of
        Right f | (p : _) <- pdfPages f -> fuTokens fuelDefault - fuTokens (snd (renderPage defaultRenderEnv f fuelDefault p))
        _ -> -1
  check "pdfrender: Type 3 procedures are interpreted once per glyph" (spent "BT /T 10 Tf (A) Tj ET" == spent "BT /T 10 Tf (AAAAAAAAAA) Tj ET")
  let tsc = renderD (doc type3Res "BT /T 10 Tf (AAA) Tj ET" type3Objs)
  check "pdfrender: Type 3 copies share one sub-scene" (length (nub (map (show . kidsOf) (transformsWithKids tsc))) == 1
                                                         && length (transformsWithKids tsc) == 3)
  -- Render mode 7 clips what follows to the glyphs.
  let csc = renderD (doc type3Res "BT /T 10 Tf 7 Tr 20 50 Td (AAA) Tj ET 1 0 0 rg 0 0 200 100 re f" type3Objs)
  check "pdfrender: Tr 3 and 7 draw nothing for Type 3 glyphs" (null (transformsOf csc))
  where
    transformsWithKids sc = [ c | c@(CTransform _ _) <- allCmds (scCmds sc) ]
    kidsOf c = case c of CTransform _ k -> allCmds k; _ -> []

------------------------------------------------------------------------------
-- Font programs and substitutes.

ttDescr :: String -> String
ttDescr extra = "<< /Type /FontDescriptor /FontName /Test /Flags " ++ extra ++ " /FontBBox [0 -200 1000 800] /ItalicAngle 0 /Ascent 800 /Descent -200 /CapHeight 700 /StemV 80 >>"

fontSpec' :: (String -> Bool -> IO ()) -> IO ()
fontSpec' check = do
  -- Embedded TrueType, non-symbolic: code -> name -> Unicode -> (3,1).
  let embedded = doc "/Font << /F 10 0 R >>" "BT /F 10 Tf 20 50 Td (ABCA) Tj ET"
        [ (10, bs "<< /Type /Font /Subtype /TrueType /BaseFont /ABCDEF+Test /FirstChar 65 /LastChar 67 /Widths [500 600 700] /FontDescriptor 11 0 R /Encoding /WinAnsiEncoding >>")
        , (11, bs (init (ttDescr "32") ++ " /FontFile2 12 0 R >>"))
        , (12, streamObj "" testFontBytes) ]
      gsE = glyphsOf (renderD embedded)
      keys = [ k | (_, Glyph _ k _) <- gsE ]
      pos = [ origin (composeA m ga) | (m, Glyph ga _ _) <- gsE ]
  check "pdfrender: embedded TrueType draws every glyph" (length gsE == 4)
  check "pdfrender: glyph positions follow /Widths" (pos == [Pt 20 50, Pt 25 50, Pt 31 50, Pt 38 50] || map (\(Pt x _) -> x) pos == [20, 25, 31, 38])
  check "pdfrender: glyph keys are per (font, GID)" (case keys of
    [a, b, c, d] -> a == d && length (nub [a, b, c]) == 3 && all (>= 0) keys
    _ -> False)
  check "pdfrender: glyph matrix is size x font matrix, y flipped" (case gsE of
    ((m, Glyph ga _ _) : _) -> let Affine a _ _ d _ _ = composeA m ga in near a 0.01 && near d (-0.01)
    _ -> False)
  -- Symbolic: (3,0) with 0xF000 + code.
  let symbolic = doc "/Font << /F 10 0 R >>" "BT /F 10 Tf 20 50 Td (AB) Tj ET"
        [ (10, bs "<< /Type /Font /Subtype /TrueType /BaseFont /Sym /FirstChar 65 /LastChar 66 /Widths [500 600] /FontDescriptor 11 0 R >>")
        , (11, bs (init (ttDescr "4") ++ " /FontFile2 12 0 R >>"))
        , (12, streamObj "" testFontBytes) ]
  check "pdfrender: symbolic TrueType goes through (3,0)" (length (glyphsOf (renderD symbolic)) == 2)
  -- Not embedded: asked for, substituted, stretched to the PDF's widths.
  let subst = doc "/Font << /F 10 0 R >>" "BT /F 10 Tf 20 50 Td (AB) Tj ET"
        [ (10, bs "<< /Type /Font /Subtype /TrueType /BaseFont /Arial,Bold /FirstChar 65 /LastChar 66 /Widths [1000 600] /FontDescriptor 11 0 R >>")
        , (11, bs (ttDescr "262176")) ]
      needs = needsOf subst
  check "pdfrender: pageFontNeeds lists the unembedded font" (map frFamily needs == [T.pack "Arial,Bold"])
  check "pdfrender: font request carries the descriptor flags" (case needs of
    [r] -> frBold r && not (frItalic r) && not (frSymbolic r)
    _ -> False)
  let gsS = glyphsOf (render fontEnv subst)
  check "pdfrender: a substitute draws the text" (length gsS == 2)
  check "pdfrender: substitute glyphs are stretched to the PDF width" (case gsS of
    [(m1, Glyph g1 _ _), (m2, Glyph g2 _ _)] ->
      let Affine a1 _ _ _ _ _ = composeA m1 g1; Affine a2 _ _ _ _ _ = composeA m2 g2
      in near a1 (0.01 * 1000 / 600) && near a2 (0.01 * 600 / 700) && origin (composeA m2 g2) == Pt 30 50
    _ -> False)
  check "pdfrender: an embedded font needs no substitute" (null (needsOf embedded))
  let missing = renderD subst
  check "pdfrender: no substitute: text skipped with a note" (null (glyphsOf missing) && hasNote "Arial,Bold" missing)
  -- A broken embedded program is asked for and substituted.
  let broken = doc "/Font << /F 10 0 R >>" "BT /F 10 Tf (A) Tj ET"
        [ (10, bs "<< /Type /Font /Subtype /TrueType /BaseFont /Broken /FirstChar 65 /LastChar 65 /Widths [500] /FontDescriptor 11 0 R >>")
        , (11, bs (init (ttDescr "32") ++ " /FontFile2 12 0 R >>"))
        , (12, streamObj "" (bs "not a font")) ]
  check "pdfrender: an unreadable embedded font is a need" (length (needsOf broken) == 1)
  check "pdfrender: an unreadable embedded font is substituted" (length (glyphsOf (render fontEnv broken)) == 1)
  -- Composite font: Identity-H, CIDToGIDMap stream, /W.
  let cid = doc "/Font << /F 10 0 R >>" "BT /F 10 Tf 20 50 Td <00010002> Tj ET"
        [ (10, bs "<< /Type /Font /Subtype /Type0 /BaseFont /T-Identity-H /Encoding /Identity-H /DescendantFonts [13 0 R] >>")
        , (13, bs "<< /Type /Font /Subtype /CIDFontType2 /BaseFont /T /CIDSystemInfo << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> /FontDescriptor 11 0 R /W [1 [400 800]] /DW 1000 /CIDToGIDMap 14 0 R >>")
        , (11, bs (init (ttDescr "4") ++ " /FontFile2 12 0 R >>"))
        , (12, streamObj "" testFontBytes)
        , (14, streamObj "" (BS.pack [0, 0, 0, 3, 0, 2])) ]
      gsC = glyphsOf (renderD cid)
  check "pdfrender: CID font through Identity-H and CIDToGIDMap" (length gsC == 2)
  check "pdfrender: CID widths from /W" (map (\(m, Glyph ga _ _) -> origin (composeA m ga)) gsC == [Pt 20 50, Pt 24 50])
  -- Vertical writing: glyphs hang from the pen (DW2's default position
  -- vector), and the pen moves down by w1y.
  let vert = doc "/Font << /F 10 0 R >>" "BT /F 10 Tf 20 50 Td <00010001> Tj ET"
        [ (10, bs "<< /Type /Font /Subtype /Type0 /BaseFont /T /Encoding /Identity-V /DescendantFonts [13 0 R] >>")
        , (13, bs "<< /Type /Font /Subtype /CIDFontType2 /BaseFont /T /FontDescriptor 11 0 R >>")
        , (11, bs (init (ttDescr "4") ++ " /FontFile2 12 0 R >>"))
        , (12, streamObj "" testFontBytes) ]
  check "pdfrender: vertical writing (Identity-V, default DW2)"
    (map (\(m, Glyph ga _ _) -> origin (composeA m ga)) (glyphsOf (renderD vert)) `pts` [Pt 15 58.8, Pt 15 68.8])
  -- CMap parsing.
  let cmapSrc = bs "/CIDInit /ProcSet findresource begin 12 dict begin begincmap\n\
                   \2 begincodespacerange <00> <7F> <8140> <9FFC> endcodespacerange\n\
                   \2 begincidrange <20> <7E> 1 <8140> <817E> 633 endcidrange\n\
                   \1 begincidchar <8150> 9999 endcidchar endcmap end end"
      (cm, _) = parseCMapBytes 100000 cmapSrc
      codes = splitCodes cm (BS.pack [0x41, 0x81, 0x41, 0x81, 0x50])
  check "pdfrender: CMap codespace splits mixed 1/2-byte codes" (codes == [(0x41, 1), (0x8141, 2), (0x8150, 2)])
  check "pdfrender: CMap cidrange and cidchar" (map (uncurry (cmapCid cm)) codes == [34, 634, 9999])
  check "pdfrender: unmapped code reads as CID 0" (cmapCid cm 0x9000 2 == 0)

------------------------------------------------------------------------------

graphicsSpec :: (String -> Bool -> IO ()) -> IO ()
graphicsSpec check = do
  -- Clipping applies after the painting operator and nests with q/Q.
  let sc = renderD (doc "" "q 10 10 50 50 re W n 1 0 0 rg 0 0 200 100 re f 0 0 1 rg 0 0 5 5 re f Q 0 1 0 rg 0 0 1 1 re f" [])
      top = case scCmds sc of [_, CClip _ k] -> k; _ -> []
  check "pdfrender: consecutive objects share a clip" (case top of
    [CClip [_] [CFill {}, CFill {}], CFill {}] -> True
    _ -> False)
  -- Alpha: a solid fill folds it into the colour; a blend wraps a group and
  -- makes the page an isolated group.
  let tsc = renderD (doc "/ExtGState << /A << /ca 0.5 >> /M << /BM /Multiply >> >>"
                         "/A gs 1 0 0 rg 0 0 10 10 re f /M gs 0 0 10 10 re f" [])
  check "pdfrender: ca folds into a solid colour" (any isHalfRed (allCmds (scCmds tsc)))
  check "pdfrender: a blend mode is a one-leaf group" (not (null [ () | CGroup o [CFill {}] <- allCmds (scCmds tsc), goBlend o == BlendMultiply ]))
  check "pdfrender: Multiply needs no page group" (case scCmds tsc of
    [_, CClip _ _] -> True
    _ -> False)
  let screen = renderD (doc "/ExtGState << /M << /BM /Screen >> >>" "/M gs 1 0 0 rg 0 0 10 10 re f" [])
  check "pdfrender: a page that blends against the paper is isolated" (case scCmds screen of
    [_, CGroup o _] -> goIsolated o
    _ -> False)
  -- B with alpha paints fill and stroke as two objects.
  let bsc = renderD (doc "/ExtGState << /A << /ca 0.5 /CA 0.5 >> >>" "/A gs 10 10 50 50 re B" [])
  check "pdfrender: B is a fill and a stroke" (length [ () | CFill {} <- allCmds (scCmds bsc) ] == 2
                                                && length [ () | CStroke {} <- allCmds (scCmds bsc) ] == 1)
  -- Soft mask: objects wrapped with a luminosity mask drawn at the CTM of gs.
  let msc = renderD (doc "/ExtGState << /S << /SMask << /S /Luminosity /G 10 0 R /BC [0.5] >> >> >>"
                         "2 0 0 2 0 0 cm /S gs 1 0 0 rg 0 0 10 10 re f"
                         [(10, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 50 50] /Group << /S /Transparency /CS /DeviceGray >>" (bs "1 g 0 0 20 20 re f"))])
  check "pdfrender: soft mask is luminosity with its backdrop" (case [ m | CGroup o _ <- allCmds (scCmds msc), Just m <- [goMask o] ] of
    [m] -> smKind m == MaskLuminosity && smBackdrop m == Color 0.5 0.5 0.5 1
           && case smCmds m of [CTransform t _] -> let Affine a _ _ d _ _ = t in near a 2 && near d (-2); _ -> False
    _ -> False)
  -- Stencil masks paint with the fill colour.
  let ssc = renderD (doc "/XObject << /M 10 0 R >>" "0 1 0 rg 10 0 0 10 0 0 cm /M Do"
                         [(10, streamObj "/Type /XObject /Subtype /Image /Width 8 /Height 1 /ImageMask true /BitsPerComponent 1" (BS.pack [0x0F]))])
  check "pdfrender: an image mask is a stencil in the fill colour" (case [ (mkW mk, p) | CStencil _ mk p _ <- allCmds (scCmds ssc) ] of
    [(8, PSolid c)] -> c == Color 0 1 0 1
    _ -> False)
  check "pdfrender: image space is flipped into the unit square" (case [ a | CStencil a _ _ _ <- allCmds (scCmds ssc) ] of
    [a] -> nearP (applyA a (Pt 0 0)) (Pt 0 90) && nearP (applyA a (Pt 1 1)) (Pt 10 100)
    _ -> False)
  -- Image memory: an image is reduced to 600 dpi of its placed size (never
  -- enlarged), stencils too, and a second, larger use decodes it again.
  let big = BS.pack (concat [ [fromIntegral x, fromIntegral y, 128] | y <- [0 .. 199 :: Int], x <- [0 .. 199 :: Int] ])
      imgDoc content = doc "/XObject << /I 10 0 R /M 11 0 R >>" content
        [ (10, streamObj "/Type /XObject /Subtype /Image /Width 200 /Height 200 /ColorSpace /DeviceRGB /BitsPerComponent 8" big)
        , (11, streamObj "/Type /XObject /Subtype /Image /Width 160 /Height 160 /ImageMask true /BitsPerComponent 1" (BS.replicate (20 * 160) 0x0F)) ]
      sizes sc' = [ (imgW i, imgH i) | CImage _ i _ <- allCmds (scCmds sc') ]
      msizes sc' = [ (mkW mk, mkH mk) | CStencil _ mk _ _ <- allCmds (scCmds sc') ]
  check "pdfrender: an image is reduced to 600 dpi of its placement"
    (sizes (renderD (imgDoc "q 9 0 0 4.5 0 0 cm /I Do Q")) == [(75, 38)])
  check "pdfrender: an image is never enlarged" (sizes (renderD (imgDoc "q 200 0 0 100 0 0 cm /I Do Q")) == [(200, 200)])
  check "pdfrender: a stencil is reduced the same way" (msizes (renderD (imgDoc "q 9 0 0 9 0 0 cm /M Do Q")) == [(75, 75)])
  check "pdfrender: a larger second use decodes again"
    (sizes (renderD (imgDoc "q 9 0 0 9 0 0 cm /I Do Q q 18 0 0 18 0 0 cm /I Do Q q 9 0 0 9 0 0 cm /I Do Q")) == [(75, 75), (150, 150), (150, 150)])
  check "pdfrender: reduction averages the pixels" (case [ i | CImage _ i _ <- allCmds (scCmds (renderD (imgDoc "q 0.96 0 0 0.96 0 0 cm /I Do Q"))) ] of
    [i] -> imgW i == 8 && let px k = fromIntegral (imgPix i U.! k) :: Int in abs (px 0 - 12) <= 1 && abs (px 2 - 128) <= 1
    _ -> False)
  -- Shadings: Extend → Spread.
  let shade ext = renderD (doc ("/Shading << /S << /ShadingType 2 /ColorSpace /DeviceRGB /Coords [0 0 100 0] " ++ ext
                                ++ " /Function << /FunctionType 2 /Domain [0 1] /C0 [1 0 0] /C1 [0 0 1] /N 1 >> >> >>") "/S sh" [])
      spreadOf sc' = [ sp | CFill _ _ (PLinear _ _ _ _ sp) _ <- allCmds (scCmds sc') ]
  check "pdfrender: Extend [true false] is SpreadPadStart" (spreadOf (shade "/Extend [true false]") == [SpreadPadStart])
  check "pdfrender: Extend [false true] is SpreadPadEnd" (spreadOf (shade "/Extend [false true]") == [SpreadPadEnd])
  check "pdfrender: no Extend is SpreadNone" (spreadOf (shade "") == [SpreadNone])
  check "pdfrender: a linear function needs two stops" (case [ st | CFill _ _ (PLinear _ _ _ st _) _ <- allCmds (scCmds (shade "")) ] of
    [st] -> length st == 2
    _ -> False)
  let rsc = renderD (doc "/Shading << /S << /ShadingType 3 /ColorSpace /DeviceGray /Coords [50 50 0 50 50 40] /Extend [false true] /Function << /FunctionType 2 /Domain [0 1] /C0 [0] /C1 [1] /N 1 >> >> >>" "/S sh" [])
  check "pdfrender: radial shading" (case [ sp | CFill _ _ (PRadial _ _ r0 _ r1 _ sp) _ <- allCmds (scCmds rsc), r0 == 0, r1 == 40 ] of
    [SpreadPadEnd] -> True
    _ -> False)
  -- A free-form mesh: one triangle.
  let mesh = BS.pack [0, 0, 0, 255, 0, 0,  0, 255, 0, 0, 255, 0,  0, 0, 255, 0, 0, 255]
      gsc = renderD (doc "/Shading << /S 10 0 R >>" "/S sh"
                         [(10, streamObj "/ShadingType 4 /ColorSpace /DeviceRGB /BitsPerCoordinate 8 /BitsPerComponent 8 /BitsPerFlag 8 /Decode [0 255 0 255 0 1 0 1 0 1]" (BS.take 18 (mesh <> BS.replicate 3 0)))])
  check "pdfrender: type 4 mesh is a Gouraud triangle" (case [ ts | CGouraud _ ts <- allCmds (scCmds gsc) ] of
    [[GouraudTri _ c0 _ _ _ _]] -> c0 == Color 1 0 0 1
    _ -> False)
  -- Tiling patterns: the matrix is relative to the page's default space,
  -- not to the CTM at the time of painting.
  let psc = renderD (doc "/Pattern << /P 10 0 R >>" "2 0 0 2 0 0 cm /Pattern cs /P scn 0 0 10 10 re f"
                         [(10, streamObj "/PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 5 5] /XStep 5 /YStep 5 /Resources << >>" (bs "0 0 1 1 re f"))])
  check "pdfrender: pattern matrix ignores the CTM" (case [ m | CFill _ _ (PTiling _ m _ _ _ _) _ <- allCmds (scCmds psc) ] of
    [Affine a b c d e f] -> near a 0.5 && near b 0 && near c 0 && near d 0.5 && near e 0 && near f 0
    _ -> False)
  where
    isHalfRed c = case c of
      CFill _ _ (PSolid (Color 1 0 0 a)) _ -> near a 0.5
      _ -> False

------------------------------------------------------------------------------

structureSpec :: (String -> Bool -> IO ()) -> IO ()
structureSpec check = do
  -- A form is a CTransform (matrix, BBox clip), interpreted once for two uses.
  let form = doc "/XObject << /F 10 0 R >>" "q 1 0 0 1 10 10 cm /F Do Q q 1 0 0 1 50 10 cm /F Do Q"
               [(10, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 20 20] /Matrix [1 0 0 1 5 0]" (bs "0 0 1 rg 0 0 30 30 re f"))]
      fsc = renderD form
      ts = [ (m, k) | CTransform m k <- allCmds (scCmds fsc) ]
  check "pdfrender: forms are CTransforms" (map (origin . fst) ts == [Pt 15 90, Pt 55 90])
  check "pdfrender: a form is clipped to its BBox" (case ts of
    ((_, [CClip [ClipShape _ _ p] _]) : _) -> pathBounds p == Just (Rect 0 0 20 20)
    _ -> False)
  check "pdfrender: repeated forms share their content" (case ts of
    [(_, a), (_, b)] -> show a == show b
    _ -> False)
  let spentF content = case parsePdfFile (doc "/XObject << /F 10 0 R >>" content [(10, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 20 20]" (bs "0 0 1 rg 0 0 30 30 re f 1 0 0 rg 1 1 2 2 re f"))]) of
        Right f | (p : _) <- pdfPages f -> fuTokens fuelDefault - fuTokens (snd (renderPage defaultRenderEnv f fuelDefault p))
        _ -> -1
  check "pdfrender: a form is interpreted once" (spentF "/F Do /F Do /F Do" - spentF "/F Do" == 4)
  -- A form that draws itself is drawn once, not forever.
  let selfRef = renderD (doc "/XObject << /F 10 0 R >>" "/F Do"
                          [(10, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 20 20] /Resources << /XObject << /F 10 0 R >> >>" (bs "0 0 1 1 re f /F Do"))])
  check "pdfrender: a self-referencing form terminates" (length [ () | CTransform {} <- allCmds (scCmds selfRef) ] == 1)
  -- Transparency group: one CGroup carrying the knockout flag and the alpha.
  let grp = renderD (doc "/XObject << /G 10 0 R >> /ExtGState << /A << /ca 0.5 >> >>" "/A gs /G Do"
                      [(10, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 20 20] /Group << /S /Transparency /K true /I true >>" (bs "0 0 1 rg 0 0 10 10 re f"))])
  check "pdfrender: a transparency group is a CGroup" (case [ o | CGroup o _ <- allCmds (scCmds grp) ] of
    [o] -> goKnockout o && goIsolated o && near (goOpacity o) 0.5
    _ -> False)
  check "pdfrender: a group's content starts with reset alpha" (case [ c | CFill _ _ (PSolid c) _ <- allCmds (scCmds grp) ] of
    (_ : cs) -> Color 0 0 1 1 `elem` cs
    _ -> False)
  -- Optional content off by default.
  let oc = docFull "/OCProperties << /OCGs [10 0 R 11 0 R] /D << /OFF [10 0 R] >> >>" ""
             "/Properties << /Off 10 0 R /On 11 0 R /MD 13 0 R >> /XObject << /X 12 0 R >>"
             "/OC /Off BDC 1 0 0 rg 0 0 10 10 re f EMC /OC /On BDC 0 1 0 rg 0 0 10 10 re f EMC /X Do /OC /MD BDC 0 0 1 rg 0 0 5 5 re f EMC"
             [ (10, bs "<< /Type /OCG /Name (off) >>"), (11, bs "<< /Type /OCG /Name (on) >>")
             , (12, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 9 9] /OC 10 0 R" (bs "1 0 1 rg 0 0 9 9 re f"))
             , (13, bs "<< /Type /OCMD /OCGs [10 0 R 11 0 R] /P /AllOn >>") ]
      osc = renderD oc
      colours = [ c | CFill _ _ (PSolid c) _ <- allCmds (scCmds osc) ]
  check "pdfrender: optional content that is off is not drawn" (colours == [white, Color 0 1 0 1])
  -- Annotations: the appearance's BBox (after its Matrix) is fitted into /Rect.
  let annot = docFull "" "/Annots [10 0 R 12 0 R 13 0 R]" "" ""
                [ (10, bs "<< /Type /Annot /Subtype /Square /Rect [100 20 140 40] /AP << /N 11 0 R >> >>")
                , (11, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 10 20] /Matrix [0 1 -1 0 0 0]" (bs "1 0 0 rg 0 0 10 20 re f"))
                , (12, bs "<< /Type /Annot /Subtype /Square /F 2 /Rect [0 0 10 10] /AP << /N 11 0 R >> >>")
                , (13, bs "<< /Type /Annot /Subtype /Widget /Rect [0 0 10 10] /AS /Off /AP << /N << /On 11 0 R >> >> >>") ]
      asc = renderD annot
      corners = [ map (applyA m) [Pt 0 0, Pt 10 20] | CTransform m _ <- allCmds (scCmds asc) ]
  check "pdfrender: annotation appearance fitted into its Rect" (case corners of
    [[p0, p1]] -> nearP p0 (Pt 140 80) && nearP p1 (Pt 100 60)
    _ -> False)
  check "pdfrender: hidden annotations and missing states are skipped" (length corners == 1)
  check "pdfrender: annotations can be turned off"
    (null [ () | CTransform {} <- allCmds (scCmds (render defaultRenderEnv { reAnnotations = False } annot)) ])
  -- An annotation with no appearance stream that poppler draws anyway.
  let links = renderD (docFull "" "/Annots [10 0 R 11 0 R]" "" ""
                [ (10, bs "<< /Type /Annot /Subtype /Link /Rect [10 10 50 30] /Border [0 0 2] /C [1 0 0] >>")
                , (11, bs "<< /Type /Annot /Subtype /Link /Rect [60 10 90 30] /Border [0 0 0] /C [0 1 0] >>") ])
  check "pdfrender: link borders are drawn from /Border and /C" (case [ (ssWidth st, c) | CStroke _ st (PSolid c) _ <- allCmds (scCmds links) ] of
    [(2, c)] -> c == Color 1 0 0 1
    _ -> False)
  -- Unsupported image codecs: a placeholder and a counted note.
  let ph = renderD (doc "/XObject << /J 10 0 R >>" "q 50 0 0 30 10 10 cm /J Do Q q 50 0 0 30 100 10 cm /J Do Q"
                     [(10, streamObj "/Type /XObject /Subtype /Image /Width 4 /Height 4 /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /JPXDecode" (bs "junk"))])
  check "pdfrender: JPEG 2000 images are placeholders" (length [ () | CFill _ _ (PSolid (Color r _ _ _)) _ <- allCmds (scCmds ph), r < 0.9, r > 0.8 ] == 2)
  check "pdfrender: placeholders are counted in one note" (hasNote "2 JPEG 2000 images not shown" ph)

------------------------------------------------------------------------------

boundsSpec :: (String -> Bool -> IO ()) -> IO ()
boundsSpec check = do
  -- Fuel exhaustion: the page stops, says so, and keeps what it drew.
  let content = concat (replicate 200 "1 0 0 rg 0 0 10 10 re f ")
  case parsePdfFile (doc "" content []) of
    Right f | (p : _) <- pdfPages f -> do
      r <- try (evaluate (let sc = fst (renderPage defaultRenderEnv f (fuel0 maxPdfDecoded 300) p) in forceScene sc `seq` sc))
      check "pdfrender: fuel exhaustion stops the page with a note" (case r of
        Right sc -> hasNote "truncated" sc && not (null [ () | CFill {} <- allCmds (scCmds sc) ])
        Left (_ :: SomeException) -> False)
    _ -> check "pdfrender: fuel fixture parses" False
  -- An unbalanced Q and a q flood are harmless.
  let qs = renderD (doc "" (concat (replicate 2000 "q ") ++ "Q Q Q 1 0 0 rg 0 0 1 1 re f" ++ concat (replicate 3000 " Q")) [])
  check "pdfrender: q/Q floods are bounded" (length (allCmds (scCmds qs)) >= 3)
  -- Fuzz: truncations and bit flips of two fixtures never throw.
  let fixtures = [ doc type3Res "BT /T 10 Tf 20 50 Td (AA A) Tj ET q 10 10 50 50 re W n 0 0 1 rg 0 0 200 100 re f Q" type3Objs
                 , doc "/Font << /F 10 0 R >> /Shading << /S 20 0 R >> /XObject << /X 21 0 R >> /ExtGState << /A << /ca 0.5 /SMask << /S /Alpha /G 21 0 R >> >> >>"
                       "/A gs BT /F 10 Tf 20 50 Td <00010002> Tj ET /S sh /X Do"
                       [ (10, bs "<< /Type /Font /Subtype /Type0 /BaseFont /T /Encoding /Identity-H /DescendantFonts [13 0 R] >>")
                       , (13, bs "<< /Type /Font /Subtype /CIDFontType2 /BaseFont /T /FontDescriptor 11 0 R /W [1 [400 800]] >>")
                       , (11, bs (init (ttDescr "4") ++ " /FontFile2 12 0 R >>"))
                       , (12, streamObj "" testFontBytes)
                       , (20, bs "<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [0 0 100 0] /Function << /FunctionType 2 /Domain [0 1] /C0 [1 0 0] /C1 [0 0 1] /N 1 >> >>")
                       , (21, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 20 20]" (bs "0 0 1 rg 0 0 10 10 re f")) ] ]
      variants = concat
        [ [ BS.take k fx | k <- [0, 7 .. BS.length fx] ]
          ++ [ flipBit fx i | i <- take 300 [ (j * 7919) `mod` (8 * BS.length fx) | j <- [1 ..] ] ]
        | fx <- fixtures ]
  r <- try (evaluate (sum (map (forceScene . render fontEnv) variants)))
  check "pdfrender: fuzzed documents never throw" (case r of
    Right n -> n >= 0
    Left (_ :: SomeException) -> False)
  where
    flipBit b i = let k = i `div` 8 in
      if k >= BS.length b then b
      else BS.concat [BS.take k b, BS.singleton (BS.index b k `xor` (1 `shiftL'` (i `mod` 8))), BS.drop (k + 1) b]
    shiftL' a n = a * (2 ^ n)

-- Force a scene completely.
forceScene :: Scene -> Int
forceScene sc = length (show (scCmds sc)) + length (concat (scNotes sc))

------------------------------------------------------------------------------
-- Regression tests from the release review's hostile files (/tmp/rr34),
-- generated here.

-- The commands a full walk of a scene visits, shared bodies counted once
-- per use, stopping past a limit (the walk the rasteriser and the driver
-- make).
walkCount :: Int -> [Cmd] -> Int
walkCount lim = go 0
  where
    go n [] = n
    go n (c : cs)
      | n > lim = n
      | otherwise = case c of
          CClip _ k -> go (go (n + 1) k) cs
          CGroup o k -> go (go (maybe (n + 1) (go (n + 1) . smCmds) (goMask o)) k) cs
          CTransform _ k -> go (go (n + 1) k) cs
          CFill _ _ (PTiling _ _ _ _ _ k) _ -> go (go (n + 1) k) cs
          _ -> go (n + 1) cs

-- Render with a wall-clock guard: Nothing when it takes over 20 s.
renderTimed :: RenderEnv -> BS.ByteString -> IO (Maybe Scene)
renderTimed env b = timeout 20000000 $ do
  let sc = render env b
  _ <- evaluate (forceScene sc)
  pure sc

hostileSpec :: (String -> Bool -> IO ()) -> IO ()
hostileSpec check = do
  -- B1: sixteen levels of forms, each drawing the level below ten times.
  -- The scene is tiny either way; its walk is what must be bounded.
  let levels = 16 :: Int
      formObj i = (20 + i, streamObj ("/Type /XObject /Subtype /Form /BBox [0 0 10 10] /Resources << /XObject << /F "
                                      ++ show (20 + i - 1) ++ " 0 R >> >>")
                                     (bs (if i == 0 then "0 0 1 1 re f" else concat (replicate 10 "/F Do "))))
      bomb = doc ("/XObject << /F " ++ show (20 + levels - 1) ++ " 0 R >>") "/F Do" [ formObj i | i <- [0 .. levels - 1] ]
  mb <- renderTimed defaultRenderEnv bomb
  check "pdfrender: a form bomb stops quickly as too complex" (maybe False (hasNote "too complex") mb)
  check "pdfrender: a form bomb's expanded walk is bounded"
    (maybe False (\sc -> walkCount 5000000 (scCmds sc) <= 2 * maxPageCmds) mb)
  -- B1: Type 3 fonts whose glyphs show 200 glyphs of the font below.
  let t3 i = (40 + 2 * i, bs ("<< /Type /Font /Subtype /Type3 /FontBBox [0 0 1000 1000] /FontMatrix [0.001 0 0 0.001 0 0] \
                              \/CharProcs << /a " ++ show (41 + 2 * i) ++ " 0 R >> /Encoding << /Differences [65 /a] >> \
                              \/FirstChar 65 /LastChar 65 /Widths [1000] /Resources << /Font << /T "
                              ++ show (40 + 2 * (i - 1)) ++ " 0 R >> >> >>"))
      t3p i = (41 + 2 * i, streamObj "" (bs (if i == 0 then "1000 0 d0 0 0 500 500 re f"
                                              else "1000 0 d0 BT /T 1 Tf (" ++ replicate 200 'A' ++ ") Tj ET")))
      t3bomb = doc ("/Font << /T " ++ show (40 + 2 * 15) ++ " 0 R >>") "BT /T 10 Tf (A) Tj ET"
                 (concat [ [t3 i, t3p i] | i <- [0 .. 15] ])
  mt <- renderTimed defaultRenderEnv t3bomb
  check "pdfrender: a Type 3 bomb stops quickly as too complex" (maybe False (hasNote "too complex") mt)
  check "pdfrender: a Type 3 bomb's expanded walk is bounded"
    (maybe False (\sc -> walkCount 5000000 (scCmds sc) <= 2 * maxPageCmds) mt)
  -- An honest shared form still draws every copy.
  let honest = doc "/XObject << /F 10 0 R >>" (concat (replicate 50 "/F Do "))
                 [(10, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 10 10] /Resources << >>" (bs "0 0 1 1 re f 0 0 2 2 re f"))]
  check "pdfrender: an ordinary shared form is not truncated"
    (let sc = renderD honest in not (hasNote "too complex" sc) && length [ () | CTransform {} <- allCmds (scCmds sc) ] == 50)
  -- B2: /W and /W2 ranges spanning millions of CIDs cost an entry each.
  let ranges = concat [ show (k * 100) ++ " 4000000000 " ++ show (500 + k) ++ " " | k <- [0 .. 199 :: Int] ]
      w2 = concat [ show (k * 100) ++ " 4000000000 -900 250 800 " | k <- [0 .. 199 :: Int] ]
      cidFont i = [ (60 + 2 * i, bs ("<< /Type /Font /Subtype /Type0 /BaseFont /X /Encoding /Identity-H /DescendantFonts [" ++ show (61 + 2 * i) ++ " 0 R] >>"))
                  , (61 + 2 * i, bs ("<< /Type /Font /Subtype /CIDFontType2 /BaseFont /X /W [" ++ ranges ++ "] /W2 [" ++ w2 ++ "] >>")) ]
      fontRes = "/Font << /T 10 0 R " ++ concat [ "/F" ++ show i ++ " " ++ show (60 + 2 * i) ++ " 0 R " | i <- [0 .. 19 :: Int] ] ++ ">>"
      shows' = concat [ "/F" ++ show i ++ " 10 Tf <0005> Tj " | i <- [0 .. 19 :: Int] ]
      cidw = doc fontRes ("BT 0 50 Td " ++ shows' ++ "/T 10 Tf (A) Tj ET")
               (type3Objs ++ concat [ cidFont i | i <- [0 .. 19] ])
  mw <- renderTimed defaultRenderEnv cidw
  -- Twenty CID 5 advances: the last range covering 5 starts at 0 (width 500).
  check "pdfrender: /W ranges are intervals (fast, and still honoured)"
    (case mw of
       Just sc -> case transformsOf sc of
         [m] -> nearP (origin m) (Pt 100 50)
         _ -> False
       Nothing -> False)
  -- Non-blocking: clips without q, and alternating clips, stay linear.
  let clips = concat (replicate 20000 "0 0 100 100 re W n 1 0 0 rg 1 1 2 2 re f ")
  mc <- renderTimed defaultRenderEnv (doc "" clips [])
  check "pdfrender: a clip chain is capped and fast" (case mc of
    Just sc -> hasNote "clipping ignored" sc && depth (scCmds sc) <= 300
    Nothing -> False)
  -- Memo keys: masks sharing /G but not /BC are different masks.
  let masks = doc "/ExtGState << /A << /SMask << /S /Luminosity /G 10 0 R /BC [0] >> >> /B << /SMask << /S /Luminosity /G 10 0 R /BC [1] >> >> >>"
                "q /A gs 0 0 1 1 re f Q q /B gs 0 0 1 1 re f Q"
                [(10, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 50 50] /Group << /S /Transparency /CS /DeviceGray >> /Resources << >>" (bs "0.5 g 0 0 9 9 re f"))]
  check "pdfrender: soft masks differing in /BC get different keys"
    (length (nub [ smKey m | CGroup o _ <- allCmds (scCmds (renderD masks)), Just m <- [goMask o] ]) == 2)
  -- A form without /Resources draws with the resources it inherits.
  let inherit = doc "/XObject << /F 10 0 R /G 11 0 R >> /ColorSpace << /C [/Indexed /DeviceRGB 0 <FF0000>] >>" "/F Do /G Do"
                  [ (10, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 10 10]" (bs "/C cs 0 sc 0 0 1 1 re f"))
                  , (11, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 10 10] /Resources << /XObject << /F 10 0 R >> /ColorSpace << /C [/Indexed /DeviceRGB 0 <00FF00>] >> >>" (bs "/F Do")) ]
  check "pdfrender: a form without resources is not reused under other resources"
    ([ c | CFill _ _ (PSolid c) _ <- allCmds (scCmds (renderD inherit)) ] == [white, Color 1 0 0 1, Color 0 1 0 1])
  -- A form inside an uncoloured pattern cannot set its own colour.
  let locked = doc "/Pattern << /P 10 0 R >> /ColorSpace << /U [/Pattern /DeviceRGB] >>" "/U cs 0 0 1 /P scn 0 0 50 50 re f"
                 [ (10, streamObj "/PatternType 1 /PaintType 2 /TilingType 1 /BBox [0 0 5 5] /XStep 5 /YStep 5 /Resources << /XObject << /F 11 0 R >> >>" (bs "/F Do"))
                 , (11, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 5 5] /Resources << >>" (bs "1 0 0 rg 0 0 1 1 re f")) ]
  check "pdfrender: forms in an uncoloured pattern keep the pattern's colour"
    ([ c | CFill _ _ (PTiling _ _ _ _ _ k) _ <- allCmds (scCmds (renderD locked)), CFill _ _ (PSolid c) _ <- allCmds k ] == [Color 0 0 1 1])
  -- Two fonts written inline do not share a form's memo entry.
  let inlineT3 glyph = "<< /Type /Font /Subtype /Type3 /FontBBox [0 0 1000 1000] /FontMatrix [0.001 0 0 0.001 0 0] /CharProcs << /a "
                       ++ glyph ++ " 0 R >> /Encoding << /Differences [65 /a] >> /FirstChar 65 /LastChar 65 /Widths [1000] >>"
      inl = doc ("/XObject << /F 10 0 R >> /Font << /I1 " ++ inlineT3 "11" ++ " /I2 " ++ inlineT3 "12" ++ " >>")
              "BT /I1 10 Tf ET /F Do BT /I2 10 Tf ET /F Do"
              [ (10, streamObj "/Type /XObject /Subtype /Form /BBox [0 0 100 100] /Resources << >>" (bs "BT (A) Tj ET"))
              , (11, streamObj "" (bs "1000 0 d0 0 0 100 100 re f"))
              , (12, streamObj "" (bs "1000 0 d0 0 0 900 900 re f")) ]
  check "pdfrender: inline fonts do not collide in form memo keys"
    (length (nub [ show p | CFill _ _ _ p <- allCmds (scCmds (renderD inl)) ]) == 3)
  -- Mesh: thousands of patches are held to the per-shading budget.
  let patch k = BS.pack (0 : concat [ [fromIntegral ((k * 37 + j * 91) `mod` 256), fromIntegral ((k * 53 + j * 17) `mod` 256)] | j <- [0 .. 11 :: Int] ]
                         ++ concat (replicate 4 [255, fromIntegral (k `mod` 256), 0]))
      mesh = BS.concat (map patch [0 .. 3999])
      meshDoc = doc "/Shading << /S 10 0 R >>" "/S sh"
                  [(10, streamObj "/ShadingType 6 /ColorSpace /DeviceRGB /BitsPerCoordinate 8 /BitsPerComponent 8 /BitsPerFlag 8 /Decode [0 200 0 100 0 1 0 1 0 1]" mesh)]
  mm <- renderTimed defaultRenderEnv meshDoc
  check "pdfrender: a mesh of thousands of patches is bounded"
    (case mm of
       Just sc -> let n = sum [ length ts | CGouraud _ ts <- allCmds (scCmds sc) ] in n > 0 && n <= 40000
       Nothing -> False)
  where
    depth = maximum . (0 :) . map d
    d c = case c of
      CClip _ k -> 1 + depth k
      CGroup _ k -> 1 + depth k
      CTransform _ k -> 1 + depth k
      _ -> 0 :: Int

------------------------------------------------------------------------------
-- Width tables as intervals ("Cmedit.IntervalMap"), shared by the reflow
-- extractor and the page renderer.

intervalSpec :: (String -> Bool -> IO ()) -> IO ()
intervalSpec check = do
  -- The painter is exactly the expanded map: last write wins key by key.
  let ops = take 400 [ (lo, lo + len, v) | k <- [1 :: Int ..]
                     , let r = (k * 7919 + 13) `mod` 1009; lo = r `mod` 200 - 20; len = (r * 31) `mod` 37 - 3; v = k ]
      painted = foldl (\m (lo, hi, v) -> IV.insertRange lo hi v m) IV.empty ops
      expanded = foldl (\m (lo, hi, v) -> foldl (\m' kk -> IM.insert kk v m') m [lo .. hi]) IM.empty ops
  check "intervals: lookups equal the expanded map"
    (all (\kk -> IV.lookupKey kk painted == IM.lookup kk expanded) [-40 .. 260])
  check "intervals: the covered count is the expanded map's size" (IV.coveredCount painted == IM.size expanded)
  check "intervals: a wide range is one entry"
    (IV.intervalCount (IV.insertRange 0 4000000000 (1 :: Int) IV.empty) == 1)
  -- The reflow view keeps its output on an ordinary CID font (widths decide
  -- its word breaks), with arrays, a range and an overriding entry.
  let tu = bs "/CIDInit /ProcSet findresource begin 12 dict begin begincmap 1 begincodespacerange <0000> <FFFF> endcodespacerange 2 beginbfrange <0001> <0005> <0041> <000A> <000C> <0061> endbfrange endcmap end end"
      cidDoc w = pdfOf [ (1, bs "<< /Type /Catalog /Pages 2 0 R >>"), (2, bs "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                       , (3, bs "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 300 100] /Resources << /Font << /F 10 0 R >> >> /Contents 4 0 R >>")
                       , (4, streamObj "" (bs "BT /F 10 Tf 20 50 Td <000100020003> Tj 30 0 Td <0004000A> Tj 0 -12 Td <000B000C0005> Tj ET"))
                       , (10, bs "<< /Type /Font /Subtype /Type0 /BaseFont /X /Encoding /Identity-H /DescendantFonts [11 0 R] /ToUnicode 12 0 R >>")
                       , (11, bs ("<< /Type /Font /Subtype /CIDFontType2 /BaseFont /X /W [" ++ w ++ "] /DW 600 >>"))
                       , (12, streamObj "" tu) ]
  check "reflow: text from an ordinary CID font is unchanged"
    (fmap Pdf.pdfPlainText (Pdf.parsePdf (cidDoc "1 [400 800] 3 4 900 10 [300 300 300] 1 1 250")) == Right (T.pack "ABC Da\nbcE\n\n"))
  -- cidw20: twenty CID fonts whose /W ranges each name millions of CIDs.
  -- Opening it used to build a million-entry map per font (2 GB resident
  -- in the editor; this test allocated 19.8 GB in the -O0 suite), now about
  -- 0.4 GB here and nothing measurable at -O2.
  let ranges = concat [ show (k * 100) ++ " 4000000000 " ++ show (500 + k) ++ " " | k <- [0 .. 199 :: Int] ]
      fonts = [ o | i <- [0 .. 19 :: Int]
                  , o <- [ (20 + 2 * i, bs ("<< /Type /Font /Subtype /Type0 /BaseFont /X /Encoding /Identity-H /DescendantFonts [" ++ show (21 + 2 * i) ++ " 0 R] >>"))
                         , (21 + 2 * i, bs ("<< /Type /Font /Subtype /CIDFontType2 /BaseFont /X /W [" ++ ranges ++ "] >>")) ] ]
      bombDoc = pdfOf ([ (1, bs "<< /Type /Catalog /Pages 2 0 R >>"), (2, bs "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                       , (3, bs ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 300 100] /Resources << /Font << "
                                 ++ concat [ "/F" ++ show i ++ " " ++ show (20 + 2 * i) ++ " 0 R " | i <- [0 .. 19 :: Int] ] ++ ">> >> /Contents 4 0 R >>"))
                       , (4, streamObj "" (bs (concat [ "BT /F" ++ show i ++ " 10 Tf <0005> Tj ET " | i <- [0 .. 19 :: Int] ]))) ] ++ fonts)
  enabled <- getRTSStatsEnabled
  _ <- evaluate (BS.length bombDoc)
  a0 <- if enabled then allocated_bytes <$> getRTSStats else pure 0
  r <- timeout 20000000 (evaluate (either length (T.length . Pdf.pdfPlainText) (Pdf.parsePdf bombDoc)))
  a1 <- if enabled then allocated_bytes <$> getRTSStats else pure 0
  check "reflow: /W ranges over millions of CIDs open in bounded memory"
    (isJustInt r && (not enabled || a1 - a0 < 2 * 1000 * 1000 * 1000))
  where
    isJustInt = maybe False (>= 0)
