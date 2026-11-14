-- | Font programs to glyph outlines: TrueType, OpenType (TrueType or CFF
-- outlines), TrueType collections, bare CFF (name-keyed and CID-keyed) and
-- PostScript Type 1. The outlines feed the SVG and PDF page renderers
-- (plan 0034), which draw them through "Cmedit.Raster".
--
-- __What a 'Font' is.__ One record per face, whatever the format: a font
-- matrix (glyph space → text space, 1 unit = 1 em), a glyph count, and
-- per-glyph closures that decode one glyph program on demand. Opening a
-- font parses offsets and small per-glyph tables only (see each reader's
-- header); 'glyphPath' does the real work, so callers memoise it per
-- (font, GID). Glyph space is the font's own, y-up, exactly as the font
-- program draws it; the caller composes 'fontMatrix' with its text matrix.
--
-- __Glyph addressing is the caller's half of the job.__ A PDF simple font
-- reaches a glyph by code through its encoding and @/Differences@ (names,
-- so 'glyphByName' and 'fontBuiltinEncoding'), or for a symbolic TrueType
-- through a specific cmap subtable ('fontCmaps', 'cmapLookup'); a CID font
-- through 'glyphForCID' or a @CIDToGIDMap@; SVG and substituted fonts by
-- character ('glyphForChar'). This module exposes every one of those
-- tables and decides none of the PDF rules, which belong to the renderer
-- that knows the font dictionary.
--
-- __Never an exception.__ Every reader is total over arbitrary bytes:
-- malformed tables give 'Left' at load time or an empty glyph later, every
-- interpreter is fuelled (subroutine depth, operators per glyph, composite
-- depth, point budgets), and 'glyphPath' drops an outline with a
-- non-finite or absurd coordinate rather than hand one to the rasteriser.
module Cmedit.Font
  ( -- * Fonts
    Font
  , FontFormat(..)
  , FontHint(..)
  , parseFont
  , parseFontAs
  , parseFontFace
  , fontFaceCount
    -- * Face properties
  , fontFormat
  , fontMatrix
  , fontUnitsPerEm
  , fontNumGlyphs
  , fontFamily
  , fontStyle
  , fontFullName
  , fontPSName
  , fontBold
  , fontItalic
  , fontFixedPitch
  , fontWeight
  , fontItalicAngle
  , fontAscent
  , fontDescent
  , fontBBox
  , fontIsCID
    -- * Glyphs
  , glyphPath
  , glyphAdvance
  , glyphName
  , glyphByName
  , glyphForChar
  , glyphForCID
  , glyphKerning
    -- * Encodings and character maps
  , fontCmaps
  , cmapLookup
  , fontBuiltinEncoding
  , standardEncodingNames
  , expertEncodingNames
  , macRomanEncodingNames
  , winAnsiEncodingNames
  , macGlyphNames
  , macRomanChars
  , aglNameToChar
  , aglCharToNames
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import Cmedit.Geom
import Cmedit.FontCore
import Cmedit.FontTT (parseSfnt, sfntFaceCount)
import Cmedit.FontCFF (parseCFF)
import Cmedit.FontT1 (parseType1)

-- | What the container says the bytes are. PDF knows: @FontFile@ is Type 1
-- (with its @Length1@\/@Length2@), @FontFile2@ TrueType, @FontFile3@ by
-- @/Subtype@ (@Type1C@ and @CIDFontType0C@ bare CFF, @OpenType@ an sfnt).
-- A hint is tried first and the sniffer second, because producers mislabel
-- (an OpenType font under @/Type1C@ is common).
data FontHint
  = HintType1 (Maybe (Int, Int))   -- ^ PDF @Length1@, @Length2@ when known.
  | HintTrueType
  | HintCFF
  | HintOpenType
  deriving (Eq, Show)

-- | Parse the first face of any supported font, by sniffing.
parseFont :: BS.ByteString -> Either String Font
parseFont = parseFontFace 0

-- | Parse with a container hint (see 'FontHint'), falling back to sniffing.
parseFontAs :: FontHint -> BS.ByteString -> Either String Font
parseFontAs hint bs = case hinted of
  Right f -> Right f
  Left e -> case parseFont bs of
    Right f -> Right f
    Left _ -> Left e
  where
    hinted = case hint of
      HintType1 lens -> parseType1 lens bs
      HintTrueType -> parseSfnt 0 bs
      HintCFF -> parseCFF bs
      HintOpenType -> parseSfnt 0 bs

-- | Faces in the file: the collection size for a TTC, else 1 for any font
-- this module can sniff, else 0.
fontFaceCount :: BS.ByteString -> Int
fontFaceCount bs = case sfntFaceCount bs of
  0 | sniff bs /= Unknown -> 1
  n -> n

-- | Face @k@ of a collection (0 for single-face files). Pick a face by name
-- by comparing 'fontFamily'\/'fontStyle' across @[0 .. fontFaceCount - 1]@.
parseFontFace :: Int -> BS.ByteString -> Either String Font
parseFontFace k bs = case sniff bs of
  Sfnt -> parseSfnt k bs
  Cff | k == 0 -> parseCFF bs
  T1 | k == 0 -> parseType1 Nothing bs
  Unknown -> Left "unrecognised font format"
  _ -> Left "face index out of range"

data Kind = Sfnt | Cff | T1 | Unknown deriving Eq

sniff :: BS.ByteString -> Kind
sniff bs
  | sfntFaceCount bs > 0 = Sfnt
  | BS.length bs >= 4 && u8 bs 0 == 0x80 && u8 bs 1 == 1 = T1
  | BC.pack "%!" `BS.isPrefixOf` bs = T1
  | BS.length bs >= 4 && u8 bs 0 == 1 && u8 bs 1 == 0 && u8 bs 2 >= 4 && u8 bs 3 >= 1 && u8 bs 3 <= 4 = Cff
  | not (BS.null (snd (BS.breakSubstring (BC.pack "eexec") (BS.take 65536 bs)))) = T1
  | otherwise = Unknown

-- | The outline of a glyph in glyph space (y-up), every subpath closed.
-- Empty for a missing glyph, a blank one (space), or one the reader had to
-- give up on.
glyphPath :: Font -> Int -> Path
glyphPath f g
  | g < 0 || g >= fontNumGlyphs f = []
  | all segOK p = p
  | otherwise = []
  where
    p = fnGlyph f g
    segOK s = case s of
      MoveTo a -> ok a
      LineTo a -> ok a
      QuadTo a b -> ok a && ok b
      CubicTo a b c -> ok a && ok b && ok c
      ClosePath -> True
    ok (Pt x y) = fin x && fin y
    fin v = not (isNaN v) && abs v < 1e7

-- | Advance width in glyph-space units (multiply by 'fontMatrix' for ems).
glyphAdvance :: Font -> Int -> Double
glyphAdvance f g
  | g < 0 || g >= fontNumGlyphs f = 0
  | isNaN w || isInfinite w = 0
  | otherwise = w
  where w = fnAdvance f g

-- | The glyph's PostScript name (@post@ table, CFF charset or Type 1
-- CharStrings key); 'Nothing' for CID-keyed fonts and @post@ format 3.
glyphName :: Font -> Int -> Maybe String
glyphName = fnName

-- | The GID drawing a glyph name.
glyphByName :: Font -> String -> Maybe Int
glyphByName = fnByName

-- | The GID for a character: the best Unicode cmap (then the symbol and
-- Mac Roman subtables) for sfnt fonts; the Adobe Glyph List name (or
-- @uniXXXX@) through the glyph names for CFF and Type 1.
glyphForChar :: Font -> Char -> Maybe Int
glyphForChar f c = case fnForChar f c of
  -- A sparse (or missing) cmap may still leave the glyph reachable by name.
  Nothing | fontFormat f `elem` [FmtTrueType, FmtOpenTypeCFF, FmtCFFCID] ->
    firstJust [ fnByName f n | n <- aglCharToNames c ]
  r -> r
  where firstJust (Just x : _) = Just x
        firstJust (_ : xs) = firstJust xs
        firstJust [] = Nothing

-- | CID → GID for a CID-keyed CFF (through its charset); the identity
-- (within range) for every other font, which is PDF's @Identity@ reading.
glyphForCID :: Font -> Int -> Maybe Int
glyphForCID = fnForCID

-- | Pair kerning between two GIDs, glyph-space units.
glyphKerning :: Font -> Int -> Int -> Double
glyphKerning = fontKerning

-- | A cmap subtable by (platform, encoding), e.g. @(3,1)@ Windows Unicode
-- BMP, @(3,0)@ Windows Symbol, @(1,0)@ Mac Roman, @(3,10)@ full Unicode.
cmapLookup :: Font -> Int -> Int -> Maybe (Int -> Maybe Int)
cmapLookup f p e = lookup (p, e) (fontCmaps f)
