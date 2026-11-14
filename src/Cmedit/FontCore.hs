{-# LANGUAGE BangPatterns #-}
-- | The shared floor of the font readers ("Cmedit.FontTT", "Cmedit.FontCFF",
-- "Cmedit.FontT1"), re-exported through "Cmedit.Font".
--
-- Three things live here because more than one reader needs them:
--
-- * __The 'Font' record.__ Every format answers the same questions (an
--   outline per glyph, an advance, names, cmaps), so a parsed font is a
--   record of /closures/ over its own tables. The tables are parsed once
--   into arrays when the font is read; the closures decode one glyph
--   program on demand. That split is the cost model: opening a CJK font
--   with 65 000 glyphs reads its offset tables and nothing else, and a page
--   that draws thousands of glyphs pays per glyph it actually draws (the
--   caller memoises per (font, gid)).
--
-- * __Total byte readers.__ Every read past the end of the input returns 0
--   rather than throwing. Font files are hostile input, and a reader that
--   cannot throw on a bad offset turns "check every offset" into "bound every
--   loop", which is the property that actually matters: a garbage count
--   costs at most a bounded amount of work and produces an empty glyph.
--
-- * __The 'PathB' builder__ the two charstring interpreters (Type 1 and
--   Type 2) share: relative moves, lines and cubics, with a pending move so a
--   @moveto@ followed by another @moveto@ never emits an empty subpath, and
--   every subpath closed, because a glyph is always filled and a fill closes
--   implicitly anyway.
--
-- Plus the standard tables: the CFF standard strings (whose first 150 entries
-- are also Adobe StandardEncoding's glyph names, in code order), the 258
-- Macintosh glyph names TrueType's @post@ table indexes, Mac Roman, and a
-- Latin/Greek/symbol subset of the Adobe Glyph List for name ↔ Unicode.
module Cmedit.FontCore
  ( -- * The font record
    Font(..)
  , FontFormat(..)
  , emptyFontWith
  , weightFromName
    -- * Byte readers (total: 0 past the end)
  , u8, u16, i16, u24, u32, i32, fixed1616, f2dot14
  , slice
    -- * Charstring path builder
  , PathB
  , pbEmpty, pbMove, pbLine, pbCurve, pbClose, pbFinish, pbPoint, pbSegCount
    -- * Standard tables
  , cffStandardStrings
  , standardEncoding
  , standardEncodingNames
  , expertEncodingNames
  , macGlyphNames
  , macRomanChars
  , macRomanEncodingNames
  , winAnsiEncodingNames
  , aglNameToChar
  , aglCharToNames
  ) where

import Data.Array (Array, listArray)
import qualified Data.Array as A
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import Data.Char (chr, isHexDigit, digitToInt, ord)
import Data.Int (Int16, Int32)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16, Word32)

import Cmedit.Geom

------------------------------------------------------------------------------
-- The font record

-- | Which reader produced the font. 'FmtCFFCID' is a CID-keyed CFF (bare or
-- inside an OpenType wrapper), whose glyphs are addressed by CID through
-- 'Cmedit.Font.glyphForCID'.
data FontFormat
  = FmtTrueType       -- ^ sfnt with @glyf@ outlines.
  | FmtOpenTypeCFF    -- ^ sfnt (@OTTO@) wrapping a name-keyed CFF.
  | FmtCFF            -- ^ Bare name-keyed CFF (PDF @FontFile3 /Type1C@).
  | FmtCFFCID         -- ^ CID-keyed CFF, bare or in an OpenType wrapper.
  | FmtType1          -- ^ PostScript Type 1 (PFA, PFB, PDF @FontFile@).
  deriving (Eq, Show)

-- | A parsed font. The public face of this record is the accessor functions
-- "Cmedit.Font" exports; the @fn*@ closures are the reader's own and are
-- reached through 'Cmedit.Font.glyphPath' and friends.
data Font = Font
  { fontFormat     :: !FontFormat
  , fontMatrix     :: !Affine
      -- ^ Glyph space to text space, where one text-space unit is one em:
      -- @1\/unitsPerEm@ for TrueType, the @FontMatrix@ for CFF and Type 1.
  , fontUnitsPerEm :: !Double
      -- ^ Nominal units per em (@1 \/ a@ of 'fontMatrix' for CFF\/Type 1).
  , fontNumGlyphs  :: !Int
  , fontFamily     :: !Text
  , fontStyle      :: !Text   -- ^ Subfamily ("Bold Italic"), or the Weight.
  , fontFullName   :: !Text
  , fontPSName     :: !Text
  , fontBold       :: !Bool
  , fontItalic     :: !Bool
  , fontFixedPitch :: !Bool
  , fontWeight     :: !Int    -- ^ CSS-style 100–900 (400 when unknown).
  , fontItalicAngle :: !Double
  , fontAscent     :: !Double -- ^ In ems (positive, above the baseline).
  , fontDescent    :: !Double -- ^ In ems (negative, below the baseline).
  , fontBBox       :: !Rect   -- ^ The font's declared box, glyph space.
  , fontIsCID      :: !Bool
  , fontCmaps      :: [((Int, Int), Int -> Maybe Int)]
      -- ^ TrueType\/OpenType character maps by (platform, encoding), in file
      -- order, each a code → GID lookup. Empty for bare CFF and Type 1.
  , fontBuiltinEncoding :: Maybe (IM.IntMap String)
      -- ^ Code → glyph name, for Type 1 and name-keyed CFF.
  , fontKerning    :: Int -> Int -> Double
      -- ^ Pair kerning in glyph-space units (TrueType @kern@ format 0); 0
      -- when the font has none.
  , fnGlyph        :: Int -> Path
  , fnAdvance      :: Int -> Double
  , fnName         :: Int -> Maybe String
  , fnByName       :: String -> Maybe Int
  , fnForCID       :: Int -> Maybe Int
  , fnForChar      :: Char -> Maybe Int
  }

instance Show Font where
  show f = "Font{" ++ show (fontFormat f) ++ " " ++ show (fontFamily f) ++ " "
        ++ show (fontStyle f) ++ " glyphs=" ++ show (fontNumGlyphs f) ++ "}"

-- | A font with every field at its "knows nothing" value, for readers to
-- fill in with record update.
emptyFontWith :: FontFormat -> Font
emptyFontWith fmt = Font
  { fontFormat = fmt, fontMatrix = scaleA 0.001 0.001, fontUnitsPerEm = 1000
  , fontNumGlyphs = 0, fontFamily = T.empty, fontStyle = T.empty
  , fontFullName = T.empty, fontPSName = T.empty
  , fontBold = False, fontItalic = False, fontFixedPitch = False
  , fontWeight = 400, fontItalicAngle = 0
  , fontAscent = 0.8, fontDescent = -0.2, fontBBox = Rect 0 0 0 0
  , fontIsCID = False, fontCmaps = [], fontBuiltinEncoding = Nothing
  , fontKerning = \_ _ -> 0
  , fnGlyph = const [], fnAdvance = const 0, fnName = const Nothing
  , fnByName = const Nothing, fnForCID = const Nothing, fnForChar = const Nothing
  }

-- | A CSS weight from the words of a PostScript @Weight@ or font name
-- (lower-cased): Type 1 and CFF have no numeric weight, and a substitute
-- chosen by weight needs one.
weightFromName :: Text -> Int
weightFromName t
  | has ["thin", "hairline"] = 100
  | has ["extralight", "ultralight"] = 200
  | has ["semibold", "demibold", "demi"] = 600
  | has ["extrabold", "ultrabold", "heavy"] = 800
  | has ["black"] = 900
  | has ["bold"] = 700
  | has ["medium"] = 500
  | has ["light"] = 300
  | otherwise = 400
  where has = any (`T.isInfixOf` t)

------------------------------------------------------------------------------
-- Byte readers

u8 :: BS.ByteString -> Int -> Int
u8 bs i
  | i >= 0 && i < BS.length bs = fromIntegral (BU.unsafeIndex bs i)
  | otherwise = 0
{-# INLINE u8 #-}

u16 :: BS.ByteString -> Int -> Int
u16 bs i
  | i >= 0 && i + 1 < BS.length bs =
      (fromIntegral (BU.unsafeIndex bs i) `shiftL` 8) .|. fromIntegral (BU.unsafeIndex bs (i + 1))
  | otherwise = (u8 bs i `shiftL` 8) .|. u8 bs (i + 1)
{-# INLINE u16 #-}

i16 :: BS.ByteString -> Int -> Int
i16 bs i = fromIntegral (fromIntegral (u16 bs i) :: Int16)
{-# INLINE i16 #-}

u24 :: BS.ByteString -> Int -> Int
u24 bs i = (u8 bs i `shiftL` 16) .|. (u8 bs (i + 1) `shiftL` 8) .|. u8 bs (i + 2)

u32 :: BS.ByteString -> Int -> Int
u32 bs i = (u16 bs i `shiftL` 16) .|. u16 bs (i + 2)
{-# INLINE u32 #-}

i32 :: BS.ByteString -> Int -> Int
i32 bs i = fromIntegral (fromIntegral (u32 bs i) :: Int32)

fixed1616 :: BS.ByteString -> Int -> Double
fixed1616 bs i = fromIntegral (i32 bs i) / 65536

f2dot14 :: BS.ByteString -> Int -> Double
f2dot14 bs i = fromIntegral (i16 bs i) / 16384

-- | @slice off len@, clamped to the input (never throws).
slice :: Int -> Int -> BS.ByteString -> BS.ByteString
slice off len bs
  | off < 0 || len <= 0 || off >= BS.length bs = BS.empty
  | otherwise = BS.take len (BS.drop off bs)


------------------------------------------------------------------------------
-- Path builder

-- | Charstring outline accumulator: segments in reverse, the current point,
-- whether a subpath is open, and the count of segments emitted (the
-- interpreters' fuel reads it).
data PathB = PathB ![Seg] !Double !Double !Bool !Int

pbEmpty :: PathB
pbEmpty = PathB [] 0 0 False 0

pbPoint :: PathB -> (Double, Double)
pbPoint (PathB _ x y _ _) = (x, y)

pbSegCount :: PathB -> Int
pbSegCount (PathB _ _ _ _ n) = n

-- | Move to an absolute point. Closes the open subpath; the new one is only
-- started (a 'MoveTo' emitted) when something is drawn from it.
pbMove :: Double -> Double -> PathB -> PathB
pbMove x y (PathB segs _ _ open n)
  | open      = PathB (ClosePath : segs) x y False (n + 1)
  | otherwise = PathB segs x y False n

begin :: PathB -> PathB
begin pb@(PathB segs x y open n)
  | open      = pb
  | otherwise = PathB (MoveTo (Pt x y) : segs) x y True (n + 1)

pbLine :: Double -> Double -> PathB -> PathB
pbLine x y pb = let PathB segs _ _ _ n = begin pb
                in PathB (LineTo (Pt x y) : segs) x y True (n + 1)

pbCurve :: Double -> Double -> Double -> Double -> Double -> Double -> PathB -> PathB
pbCurve x1 y1 x2 y2 x3 y3 pb =
  let PathB segs _ _ _ n = begin pb
  in PathB (CubicTo (Pt x1 y1) (Pt x2 y2) (Pt x3 y3) : segs) x3 y3 True (n + 1)

-- | Close the open subpath without moving the current point (Type 1's
-- @closepath@: the next relative move is from where the outline ended, the
-- reading FreeType and Ghostscript take).
pbClose :: PathB -> PathB
pbClose pb@(PathB segs x y open n)
  | open      = PathB (ClosePath : segs) x y False (n + 1)
  | otherwise = pb

pbFinish :: PathB -> Path
pbFinish (PathB segs _ _ open _) = reverse (if open then ClosePath : segs else segs)

------------------------------------------------------------------------------
-- Standard tables

-- | The 391 CFF standard strings (SIDs 0–390).
cffStandardStrings :: Array Int String
cffStandardStrings = listArray (0, length ss - 1) ss
  where
    ss =
      [ ".notdef", "space", "exclam", "quotedbl", "numbersign", "dollar", "percent"
      , "ampersand", "quoteright", "parenleft", "parenright", "asterisk", "plus"
      , "comma", "hyphen", "period", "slash", "zero", "one", "two", "three", "four"
      , "five", "six", "seven", "eight", "nine", "colon", "semicolon", "less"
      , "equal", "greater", "question", "at" ]
      ++ map (: []) ['A' .. 'Z'] ++
      [ "bracketleft", "backslash", "bracketright", "asciicircum", "underscore"
      , "quoteleft" ]
      ++ map (: []) ['a' .. 'z'] ++
      [ "braceleft", "bar", "braceright", "asciitilde", "exclamdown", "cent"
      , "sterling", "fraction", "yen", "florin", "section", "currency"
      , "quotesingle", "quotedblleft", "guillemotleft", "guilsinglleft"
      , "guilsinglright", "fi", "fl", "endash", "dagger", "daggerdbl"
      , "periodcentered", "paragraph", "bullet", "quotesinglbase", "quotedblbase"
      , "quotedblright", "guillemotright", "ellipsis", "perthousand"
      , "questiondown", "grave", "acute", "circumflex", "tilde", "macron", "breve"
      , "dotaccent", "dieresis", "ring", "cedilla", "hungarumlaut", "ogonek"
      , "caron", "emdash", "AE", "ordfeminine", "Lslash", "Oslash", "OE"
      , "ordmasculine", "ae", "dotlessi", "lslash", "oslash", "oe", "germandbls"
      , "onesuperior", "logicalnot", "mu", "trademark", "Eth", "onehalf"
      , "plusminus", "Thorn", "onequarter", "divide", "brokenbar", "degree"
      , "thorn", "threequarters", "twosuperior", "registered", "minus", "eth"
      , "multiply", "threesuperior", "copyright", "Aacute", "Acircumflex"
      , "Adieresis", "Agrave", "Aring", "Atilde", "Ccedilla", "Eacute"
      , "Ecircumflex", "Edieresis", "Egrave", "Iacute", "Icircumflex"
      , "Idieresis", "Igrave", "Ntilde", "Oacute", "Ocircumflex", "Odieresis"
      , "Ograve", "Otilde", "Scaron", "Uacute", "Ucircumflex", "Udieresis"
      , "Ugrave", "Yacute", "Ydieresis", "Zcaron", "aacute", "acircumflex"
      , "adieresis", "agrave", "aring", "atilde", "ccedilla", "eacute"
      , "ecircumflex", "edieresis", "egrave", "iacute", "icircumflex"
      , "idieresis", "igrave", "ntilde", "oacute", "ocircumflex", "odieresis"
      , "ograve", "otilde", "scaron", "uacute", "ucircumflex", "udieresis"
      , "ugrave", "yacute", "ydieresis", "zcaron", "exclamsmall"
      , "Hungarumlautsmall", "dollaroldstyle", "dollarsuperior", "ampersandsmall"
      , "Acutesmall", "parenleftsuperior", "parenrightsuperior", "twodotenleader"
      , "onedotenleader", "zerooldstyle", "oneoldstyle", "twooldstyle"
      , "threeoldstyle", "fouroldstyle", "fiveoldstyle", "sixoldstyle"
      , "sevenoldstyle", "eightoldstyle", "nineoldstyle", "commasuperior"
      , "threequartersemdash", "periodsuperior", "questionsmall", "asuperior"
      , "bsuperior", "centsuperior", "dsuperior", "esuperior", "isuperior"
      , "lsuperior", "msuperior", "nsuperior", "osuperior", "rsuperior"
      , "ssuperior", "tsuperior", "ff", "ffi", "ffl", "parenleftinferior"
      , "parenrightinferior", "Circumflexsmall", "hyphensuperior", "Gravesmall" ]
      ++ [ c : "small" | c <- ['A' .. 'Z'] ] ++
      [ "colonmonetary", "onefitted", "rupiah", "Tildesmall", "exclamdownsmall"
      , "centoldstyle", "Lslashsmall", "Scaronsmall", "Zcaronsmall"
      , "Dieresissmall", "Brevesmall", "Caronsmall", "Dotaccentsmall"
      , "Macronsmall", "figuredash", "hypheninferior", "Ogoneksmall", "Ringsmall"
      , "Cedillasmall", "questiondownsmall", "oneeighth", "threeeighths"
      , "fiveeighths", "seveneighths", "onethird", "twothirds", "zerosuperior"
      , "foursuperior", "fivesuperior", "sixsuperior", "sevensuperior"
      , "eightsuperior", "ninesuperior", "zeroinferior", "oneinferior"
      , "twoinferior", "threeinferior", "fourinferior", "fiveinferior"
      , "sixinferior", "seveninferior", "eightinferior", "nineinferior"
      , "centinferior", "dollarinferior", "periodinferior", "commainferior"
      , "Agravesmall", "Aacutesmall", "Acircumflexsmall", "Atildesmall"
      , "Adieresissmall", "Aringsmall", "AEsmall", "Ccedillasmall"
      , "Egravesmall", "Eacutesmall", "Ecircumflexsmall", "Edieresissmall"
      , "Igravesmall", "Iacutesmall", "Icircumflexsmall", "Idieresissmall"
      , "Ethsmall", "Ntildesmall", "Ogravesmall", "Oacutesmall"
      , "Ocircumflexsmall", "Otildesmall", "Odieresissmall", "OEsmall"
      , "Oslashsmall", "Ugravesmall", "Uacutesmall", "Ucircumflexsmall"
      , "Udieresissmall", "Yacutesmall", "Thornsmall", "Ydieresissmall"
      , "001.000", "001.001", "001.002", "001.003", "Black", "Bold", "Book"
      , "Light", "Medium", "Regular", "Roman", "Semibold" ]

-- | Adobe StandardEncoding as code → glyph name. Its non-empty codes run
-- through the standard strings in SID order, which is how it is built.
standardEncodingNames :: IM.IntMap String
standardEncodingNames = IM.fromList
  [ (code, cffStandardStrings A.! sid) | (code, sid) <- standardEncodingSIDs ]

-- | Code → SID for StandardEncoding (also CFF's predefined encoding 0).
standardEncoding :: [(Int, Int)]
standardEncoding = standardEncodingSIDs

standardEncodingSIDs :: [(Int, Int)]
standardEncodingSIDs =
  zip [32 .. 126] [1 .. 95] ++
  zip [161 .. 175] [96 .. 110] ++
  zip [177 .. 180] [111 .. 114] ++
  zip [182 .. 189] [115 .. 122] ++
  [ (191, 123) ] ++
  zip [193 .. 200] [124 .. 131] ++
  [ (202, 132), (203, 133) ] ++
  zip [205 .. 208] [134 .. 137] ++
  [ (225, 138), (227, 139), (232, 140), (233, 141), (234, 142), (235, 143)
  , (241, 144), (245, 145), (248, 146), (249, 147), (250, 148), (251, 149) ]

-- | Adobe ExpertEncoding (CFF's predefined encoding 1), code → glyph name.
expertEncodingNames :: IM.IntMap String
expertEncodingNames = IM.fromList (filter (not . null . snd) (zip [32 ..] names))
  where
    names =
      [ "space", "exclamsmall", "Hungarumlautsmall", "", "dollaroldstyle"
      , "dollarsuperior", "ampersandsmall", "Acutesmall", "parenleftsuperior"
      , "parenrightsuperior", "twodotenleader", "onedotenleader", "comma"
      , "hyphen", "period", "fraction", "zerooldstyle", "oneoldstyle"
      , "twooldstyle", "threeoldstyle", "fouroldstyle", "fiveoldstyle"
      , "sixoldstyle", "sevenoldstyle", "eightoldstyle", "nineoldstyle", "colon"
      , "semicolon", "commasuperior", "threequartersemdash", "periodsuperior"
      , "questionsmall", "", "asuperior", "bsuperior", "centsuperior"
      , "dsuperior", "esuperior", "", "", "", "isuperior", "", "", "lsuperior"
      , "msuperior", "nsuperior", "osuperior", "", "", "rsuperior", "ssuperior"
      , "tsuperior", "", "ff", "fi", "fl", "ffi", "ffl", "parenleftinferior", ""
      , "parenrightinferior", "Circumflexsmall", "hyphensuperior", "Gravesmall" ]
      ++ [ c : "small" | c <- ['A' .. 'Z'] ] ++
      [ "colonmonetary", "onefitted", "rupiah", "Tildesmall" ]
      ++ replicate 34 "" ++                                   -- 127–160
      [ "exclamdownsmall", "centoldstyle", "Lslashsmall", "", "", "Scaronsmall"
      , "Zcaronsmall", "Dieresissmall", "Brevesmall", "Caronsmall", ""
      , "Dotaccentsmall", "", "", "Macronsmall", "", "", "figuredash"
      , "hypheninferior", "", "", "Ogoneksmall", "Ringsmall", "Cedillasmall"
      , "", "", "", "onequarter", "onehalf", "threequarters", "questiondownsmall"
      , "oneeighth", "threeeighths", "fiveeighths", "seveneighths", "onethird"
      , "twothirds", "", "", "zerosuperior", "onesuperior", "twosuperior"
      , "threesuperior", "foursuperior", "fivesuperior", "sixsuperior"
      , "sevensuperior", "eightsuperior", "ninesuperior", "zeroinferior"
      , "oneinferior", "twoinferior", "threeinferior", "fourinferior"
      , "fiveinferior", "sixinferior", "seveninferior", "eightinferior"
      , "nineinferior", "centinferior", "dollarinferior", "periodinferior"
      , "commainferior", "Agravesmall", "Aacutesmall", "Acircumflexsmall"
      , "Atildesmall", "Adieresissmall", "Aringsmall", "AEsmall"
      , "Ccedillasmall", "Egravesmall", "Eacutesmall", "Ecircumflexsmall"
      , "Edieresissmall", "Igravesmall", "Iacutesmall", "Icircumflexsmall"
      , "Idieresissmall", "Ethsmall", "Ntildesmall", "Ogravesmall"
      , "Oacutesmall", "Ocircumflexsmall", "Otildesmall", "Odieresissmall"
      , "OEsmall", "Oslashsmall", "Ugravesmall", "Uacutesmall"
      , "Ucircumflexsmall", "Udieresissmall", "Yacutesmall", "Thornsmall"
      , "Ydieresissmall" ]

-- | The 258 standard Macintosh glyph names (TrueType @post@ formats 1 and 2).
macGlyphNames :: Array Int String
macGlyphNames = listArray (0, length ns - 1) ns
  where
    ns =
      [ ".notdef", ".null", "nonmarkingreturn", "space", "exclam", "quotedbl"
      , "numbersign", "dollar", "percent", "ampersand", "quotesingle"
      , "parenleft", "parenright", "asterisk", "plus", "comma", "hyphen"
      , "period", "slash", "zero", "one", "two", "three", "four", "five", "six"
      , "seven", "eight", "nine", "colon", "semicolon", "less", "equal"
      , "greater", "question", "at" ]
      ++ map (: []) ['A' .. 'Z'] ++
      [ "bracketleft", "backslash", "bracketright", "asciicircum", "underscore"
      , "grave" ]
      ++ map (: []) ['a' .. 'z'] ++
      [ "braceleft", "bar", "braceright", "asciitilde", "Adieresis", "Aring"
      , "Ccedilla", "Eacute", "Ntilde", "Odieresis", "Udieresis", "aacute"
      , "agrave", "acircumflex", "adieresis", "atilde", "aring", "ccedilla"
      , "eacute", "egrave", "ecircumflex", "edieresis", "iacute", "igrave"
      , "icircumflex", "idieresis", "ntilde", "oacute", "ograve", "ocircumflex"
      , "odieresis", "otilde", "uacute", "ugrave", "ucircumflex", "udieresis"
      , "dagger", "degree", "cent", "sterling", "section", "bullet", "paragraph"
      , "germandbls", "registered", "copyright", "trademark", "acute"
      , "dieresis", "notequal", "AE", "Oslash", "infinity", "plusminus"
      , "lessequal", "greaterequal", "yen", "mu", "partialdiff", "summation"
      , "product", "pi", "integral", "ordfeminine", "ordmasculine", "Omega"
      , "ae", "oslash", "questiondown", "exclamdown", "logicalnot", "radical"
      , "florin", "approxequal", "Delta", "guillemotleft", "guillemotright"
      , "ellipsis", "nonbreakingspace", "Agrave", "Atilde", "Otilde", "OE"
      , "oe", "endash", "emdash", "quotedblleft", "quotedblright", "quoteleft"
      , "quoteright", "divide", "lozenge", "ydieresis", "Ydieresis", "fraction"
      , "currency", "guilsinglleft", "guilsinglright", "fi", "fl", "daggerdbl"
      , "periodcentered", "quotesinglbase", "quotedblbase", "perthousand"
      , "Acircumflex", "Ecircumflex", "Aacute", "Edieresis", "Egrave", "Iacute"
      , "Icircumflex", "Idieresis", "Igrave", "Oacute", "Ocircumflex", "apple"
      , "Ograve", "Uacute", "Ucircumflex", "Ugrave", "dotlessi", "circumflex"
      , "tilde", "macron", "breve", "dotaccent", "ring", "cedilla"
      , "hungarumlaut", "ogonek", "caron", "Lslash", "lslash", "Scaron"
      , "scaron", "Zcaron", "zcaron", "brokenbar", "Eth", "eth", "Yacute"
      , "yacute", "Thorn", "thorn", "minus", "multiply", "onesuperior"
      , "twosuperior", "threesuperior", "onehalf", "onequarter"
      , "threequarters", "franc", "Gbreve", "gbreve", "Idotaccent", "Scedilla"
      , "scedilla", "Cacute", "cacute", "Ccaron", "ccaron", "dcroat" ]

-- | Mac OS Roman bytes 0x80–0xFF as Unicode (0xDB is the euro sign, as in
-- every Mac OS since 8.5).
macRomanChars :: Array Int Char
macRomanChars = listArray (128, 255)
  "\x00C4\x00C5\x00C7\x00C9\x00D1\x00D6\x00DC\x00E1\x00E0\x00E2\x00E4\x00E3\x00E5\x00E7\x00E9\x00E8\
  \\x00EA\x00EB\x00ED\x00EC\x00EE\x00EF\x00F1\x00F3\x00F2\x00F4\x00F6\x00F5\x00FA\x00F9\x00FB\x00FC\
  \\x2020\x00B0\x00A2\x00A3\x00A7\x2022\x00B6\x00DF\x00AE\x00A9\x2122\x00B4\x00A8\x2260\x00C6\x00D8\
  \\x221E\x00B1\x2264\x2265\x00A5\x00B5\x2202\x2211\x220F\x03C0\x222B\x00AA\x00BA\x03A9\x00E6\x00F8\
  \\x00BF\x00A1\x00AC\x221A\x0192\x2248\x2206\x00AB\x00BB\x2026\x00A0\x00C0\x00C3\x00D5\x0152\x0153\
  \\x2013\x2014\x201C\x201D\x2018\x2019\x00F7\x25CA\x00FF\x0178\x2044\x20AC\x2039\x203A\xFB01\xFB02\
  \\x2021\x00B7\x201A\x201E\x2030\x00C2\x00CA\x00C1\x00CB\x00C8\x00CD\x00CE\x00CF\x00CC\x00D3\x00D4\
  \\xF8FF\x00D2\x00DA\x00DB\x00D9\x0131\x02C6\x02DC\x00AF\x02D8\x02D9\x02DA\x00B8\x02DD\x02DB\x02C7"

-- | PDF's MacRomanEncoding, code → glyph name (the printable ASCII range
-- plus the high half; note PDF's table differs from Mac OS Roman at a few
-- codes, e.g. 0xDB is @currency@, and has no glyph at 0xF0).
macRomanEncodingNames :: IM.IntMap String
macRomanEncodingNames = IM.fromList (asciiNames ++ hi)
  where
    hi = filter (not . null . snd) $ zip [128 ..]
      [ "Adieresis", "Aring", "Ccedilla", "Eacute", "Ntilde", "Odieresis"
      , "Udieresis", "aacute", "agrave", "acircumflex", "adieresis", "atilde"
      , "aring", "ccedilla", "eacute", "egrave", "ecircumflex", "edieresis"
      , "iacute", "igrave", "icircumflex", "idieresis", "ntilde", "oacute"
      , "ograve", "ocircumflex", "odieresis", "otilde", "uacute", "ugrave"
      , "ucircumflex", "udieresis", "dagger", "degree", "cent", "sterling"
      , "section", "bullet", "paragraph", "germandbls", "registered"
      , "copyright", "trademark", "acute", "dieresis", "notequal", "AE"
      , "Oslash", "infinity", "plusminus", "lessequal", "greaterequal", "yen"
      , "mu", "partialdiff", "summation", "product", "pi", "integral"
      , "ordfeminine", "ordmasculine", "Omega", "ae", "oslash", "questiondown"
      , "exclamdown", "logicalnot", "radical", "florin", "approxequal", "Delta"
      , "guillemotleft", "guillemotright", "ellipsis", "space", "Agrave"
      , "Atilde", "Otilde", "OE", "oe", "endash", "emdash", "quotedblleft"
      , "quotedblright", "quoteleft", "quoteright", "divide", "lozenge"
      , "ydieresis", "Ydieresis", "fraction", "currency", "guilsinglleft"
      , "guilsinglright", "fi", "fl", "daggerdbl", "periodcentered"
      , "quotesinglbase", "quotedblbase", "perthousand", "Acircumflex"
      , "Ecircumflex", "Aacute", "Edieresis", "Egrave", "Iacute", "Icircumflex"
      , "Idieresis", "Igrave", "Oacute", "Ocircumflex", "", "Ograve"
      , "Uacute", "Ucircumflex", "Ugrave", "dotlessi", "circumflex", "tilde"
      , "macron", "breve", "dotaccent", "ring", "cedilla", "hungarumlaut"
      , "ogonek", "caron" ]

-- | PDF's WinAnsiEncoding, code → glyph name.
winAnsiEncodingNames :: IM.IntMap String
winAnsiEncodingNames = IM.fromList (asciiNames ++ mid ++ latin1)
  where
    mid = filter (not . null . snd) $ zip [128 ..]
      [ "Euro", "", "quotesinglbase", "florin", "quotedblbase", "ellipsis"
      , "dagger", "daggerdbl", "circumflex", "perthousand", "Scaron"
      , "guilsinglleft", "OE", "", "Zcaron", "", "", "quoteleft", "quoteright"
      , "quotedblleft", "quotedblright", "bullet", "endash", "emdash", "tilde"
      , "trademark", "scaron", "guilsinglright", "oe", "", "zcaron"
      , "Ydieresis" ]
    latin1 = [ (c, n) | c <- [160 .. 255], Just (n : _) <- [M.lookup (chr c) charToNames]
                      , n /= "nbspace" ]
             ++ [(160, "space"), (173, "hyphen")]

-- | Codes 32–126 with their ASCII glyph names (quotesingle/grave, not the
-- StandardEncoding curly quotes at 39 and 96).
asciiNames :: [(Int, String)]
asciiNames = [ (c, n) | c <- [32 .. 126], Just (n : _) <- [M.lookup (chr c) charToNames] ]

-- | A glyph name to the character it stands for: the @uniXXXX@ and
-- @uXXXX[XX]@ conventions, then the Adobe Glyph List subset below. A suffix
-- after a period (@a.sc@, @one.oldstyle@) names a variant of the same
-- character.
aglNameToChar :: String -> Maybe Char
aglNameToChar nm0 = case nm of
  ('u' : 'n' : 'i' : hx) | length hx >= 4, length hx `mod` 4 == 0, all isHexDigit hx -> hexChar (take 4 hx)
  ('u' : hx) | length hx >= 4, length hx <= 6, all isHexDigit hx -> hexChar hx
  _ -> M.lookup nm nameToChar
  where
    nm = takeWhile (/= '.') nm0
    hexChar hx = let v = foldl (\a c -> a * 16 + digitToInt c) 0 hx
                 in if v <= 0x10FFFF && (v < 0xD800 || v > 0xDFFF) then Just (chr v) else Nothing

-- | The glyph names a character is commonly drawn by, best first, ending
-- with the @uniXXXX@ (or @uXXXXX@) spelling every font tool understands.
aglCharToNames :: Char -> [String]
aglCharToNames c = M.findWithDefault [] c charToNames ++ [uniName]
  where
    v = ord c
    uniName | v <= 0xFFFF = "uni" ++ hex4 v
            | otherwise   = "u" ++ hexN v
    hex4 x = [ hexDigit ((x `div` (16 ^ k)) `mod` 16) | k <- [3, 2, 1, 0 :: Int] ]
    hexN x = let s = go x in replicate (4 - length s) '0' ++ s
      where go 0 = ""
            go y = go (y `div` 16) ++ [hexDigit (y `mod` 16)]
    hexDigit d = "0123456789ABCDEF" !! d

nameToChar :: M.Map String Char
nameToChar = M.fromListWith (\_new old -> old) aglPairs

charToNames :: M.Map Char [String]
charToNames = M.fromListWith (flip (++)) [ (c, [n]) | (n, c) <- aglPairs ]

-- | A subset of the Adobe Glyph List: ASCII, Latin-1, Latin Extended-A, the
-- Windows-1252 and Mac Roman extras, Greek (Symbol's names), and the common
-- punctuation, math and arrow names. For a name that is in neither this
-- table nor the @uni@ form, the caller still has the font's own name lookup.
-- Where two names share a character the preferred one comes first.
aglPairs :: [(String, Char)]
aglPairs =
  [ ("space", ' '), ("exclam", '!'), ("quotedbl", '"'), ("numbersign", '#')
  , ("dollar", '$'), ("percent", '%'), ("ampersand", '&'), ("quotesingle", '\'')
  , ("parenleft", '('), ("parenright", ')'), ("asterisk", '*'), ("plus", '+')
  , ("comma", ','), ("hyphen", '-'), ("period", '.'), ("slash", '/')
  , ("zero", '0'), ("one", '1'), ("two", '2'), ("three", '3'), ("four", '4')
  , ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')
  , ("colon", ':'), ("semicolon", ';'), ("less", '<'), ("equal", '=')
  , ("greater", '>'), ("question", '?'), ("at", '@'), ("bracketleft", '[')
  , ("backslash", '\\'), ("bracketright", ']'), ("asciicircum", '^')
  , ("underscore", '_'), ("grave", '`'), ("braceleft", '{'), ("bar", '|')
  , ("braceright", '}'), ("asciitilde", '~') ]
  ++ [ ([c], c) | c <- ['A' .. 'Z'] ++ ['a' .. 'z'] ] ++
  [ ("nbspace", '\xa0'), ("exclamdown", '\xa1'), ("cent", '\xa2')
  , ("sterling", '\xa3'), ("currency", '\xa4'), ("yen", '\xa5')
  , ("brokenbar", '\xa6'), ("section", '\xa7'), ("dieresis", '\xa8')
  , ("copyright", '\xa9'), ("ordfeminine", '\xaa'), ("guillemotleft", '\xab')
  , ("logicalnot", '\xac'), ("sfthyphen", '\xad'), ("registered", '\xae')
  , ("macron", '\xaf'), ("degree", '\xb0'), ("plusminus", '\xb1')
  , ("twosuperior", '\xb2'), ("threesuperior", '\xb3'), ("acute", '\xb4')
  , ("mu", '\xb5'), ("paragraph", '\xb6'), ("periodcentered", '\xb7')
  , ("cedilla", '\xb8'), ("onesuperior", '\xb9'), ("ordmasculine", '\xba')
  , ("guillemotright", '\xbb'), ("onequarter", '\xbc'), ("onehalf", '\xbd')
  , ("threequarters", '\xbe'), ("questiondown", '\xbf'), ("Agrave", '\xc0')
  , ("Aacute", '\xc1'), ("Acircumflex", '\xc2'), ("Atilde", '\xc3')
  , ("Adieresis", '\xc4'), ("Aring", '\xc5'), ("AE", '\xc6'), ("Ccedilla", '\xc7')
  , ("Egrave", '\xc8'), ("Eacute", '\xc9'), ("Ecircumflex", '\xca')
  , ("Edieresis", '\xcb'), ("Igrave", '\xcc'), ("Iacute", '\xcd')
  , ("Icircumflex", '\xce'), ("Idieresis", '\xcf'), ("Eth", '\xd0')
  , ("Ntilde", '\xd1'), ("Ograve", '\xd2'), ("Oacute", '\xd3')
  , ("Ocircumflex", '\xd4'), ("Otilde", '\xd5'), ("Odieresis", '\xd6')
  , ("multiply", '\xd7'), ("Oslash", '\xd8'), ("Ugrave", '\xd9'), ("Uacute", '\xda')
  , ("Ucircumflex", '\xdb'), ("Udieresis", '\xdc'), ("Yacute", '\xdd')
  , ("Thorn", '\xde'), ("germandbls", '\xdf'), ("agrave", '\xe0'), ("aacute", '\xe1')
  , ("acircumflex", '\xe2'), ("atilde", '\xe3'), ("adieresis", '\xe4')
  , ("aring", '\xe5'), ("ae", '\xe6'), ("ccedilla", '\xe7'), ("egrave", '\xe8')
  , ("eacute", '\xe9'), ("ecircumflex", '\xea'), ("edieresis", '\xeb')
  , ("igrave", '\xec'), ("iacute", '\xed'), ("icircumflex", '\xee')
  , ("idieresis", '\xef'), ("eth", '\xf0'), ("ntilde", '\xf1'), ("ograve", '\xf2')
  , ("oacute", '\xf3'), ("ocircumflex", '\xf4'), ("otilde", '\xf5')
  , ("odieresis", '\xf6'), ("divide", '\xf7'), ("oslash", '\xf8')
  , ("ugrave", '\xf9'), ("uacute", '\xfa'), ("ucircumflex", '\xfb')
  , ("udieresis", '\xfc'), ("yacute", '\xfd'), ("thorn", '\xfe')
  , ("ydieresis", '\xff')
  -- Latin Extended-A
  , ("Amacron", '\x100'), ("amacron", '\x101'), ("Abreve", '\x102'), ("abreve", '\x103')
  , ("Aogonek", '\x104'), ("aogonek", '\x105'), ("Cacute", '\x106'), ("cacute", '\x107')
  , ("Ccircumflex", '\x108'), ("ccircumflex", '\x109'), ("Cdotaccent", '\x10a')
  , ("cdotaccent", '\x10b'), ("Ccaron", '\x10c'), ("ccaron", '\x10d')
  , ("Dcaron", '\x10e'), ("dcaron", '\x10f'), ("Dcroat", '\x110'), ("dcroat", '\x111')
  , ("Emacron", '\x112'), ("emacron", '\x113'), ("Ebreve", '\x114'), ("ebreve", '\x115')
  , ("Edotaccent", '\x116'), ("edotaccent", '\x117'), ("Eogonek", '\x118')
  , ("eogonek", '\x119'), ("Ecaron", '\x11a'), ("ecaron", '\x11b')
  , ("Gcircumflex", '\x11c'), ("gcircumflex", '\x11d'), ("Gbreve", '\x11e')
  , ("gbreve", '\x11f'), ("Gdotaccent", '\x120'), ("gdotaccent", '\x121')
  , ("Gcommaaccent", '\x122'), ("gcommaaccent", '\x123'), ("Hcircumflex", '\x124')
  , ("hcircumflex", '\x125'), ("Hbar", '\x126'), ("hbar", '\x127')
  , ("Itilde", '\x128'), ("itilde", '\x129'), ("Imacron", '\x12a'), ("imacron", '\x12b')
  , ("Ibreve", '\x12c'), ("ibreve", '\x12d'), ("Iogonek", '\x12e'), ("iogonek", '\x12f')
  , ("Idotaccent", '\x130'), ("dotlessi", '\x131'), ("IJ", '\x132'), ("ij", '\x133')
  , ("Jcircumflex", '\x134'), ("jcircumflex", '\x135'), ("Kcommaaccent", '\x136')
  , ("kcommaaccent", '\x137'), ("kgreenlandic", '\x138'), ("Lacute", '\x139')
  , ("lacute", '\x13a'), ("Lcommaaccent", '\x13b'), ("lcommaaccent", '\x13c')
  , ("Lcaron", '\x13d'), ("lcaron", '\x13e'), ("Ldot", '\x13f'), ("ldot", '\x140')
  , ("Lslash", '\x141'), ("lslash", '\x142'), ("Nacute", '\x143'), ("nacute", '\x144')
  , ("Ncommaaccent", '\x145'), ("ncommaaccent", '\x146'), ("Ncaron", '\x147')
  , ("ncaron", '\x148'), ("napostrophe", '\x149'), ("Eng", '\x14a'), ("eng", '\x14b')
  , ("Omacron", '\x14c'), ("omacron", '\x14d'), ("Obreve", '\x14e'), ("obreve", '\x14f')
  , ("Ohungarumlaut", '\x150'), ("ohungarumlaut", '\x151'), ("OE", '\x152')
  , ("oe", '\x153'), ("Racute", '\x154'), ("racute", '\x155'), ("Rcommaaccent", '\x156')
  , ("rcommaaccent", '\x157'), ("Rcaron", '\x158'), ("rcaron", '\x159')
  , ("Sacute", '\x15a'), ("sacute", '\x15b'), ("Scircumflex", '\x15c')
  , ("scircumflex", '\x15d'), ("Scedilla", '\x15e'), ("scedilla", '\x15f')
  , ("Scaron", '\x160'), ("scaron", '\x161'), ("Tcommaaccent", '\x162')
  , ("tcommaaccent", '\x163'), ("Tcaron", '\x164'), ("tcaron", '\x165')
  , ("Tbar", '\x166'), ("tbar", '\x167'), ("Utilde", '\x168'), ("utilde", '\x169')
  , ("Umacron", '\x16a'), ("umacron", '\x16b'), ("Ubreve", '\x16c'), ("ubreve", '\x16d')
  , ("Uring", '\x16e'), ("uring", '\x16f'), ("Uhungarumlaut", '\x170')
  , ("uhungarumlaut", '\x171'), ("Uogonek", '\x172'), ("uogonek", '\x173')
  , ("Wcircumflex", '\x174'), ("wcircumflex", '\x175'), ("Ycircumflex", '\x176')
  , ("ycircumflex", '\x177'), ("Ydieresis", '\x178'), ("Zacute", '\x179')
  , ("zacute", '\x17a'), ("Zdotaccent", '\x17b'), ("zdotaccent", '\x17c')
  , ("Zcaron", '\x17d'), ("zcaron", '\x17e'), ("longs", '\x17f'), ("florin", '\x192')
  , ("Scommaaccent", '\x218'), ("scommaaccent", '\x219')
  , ("Tcommaaccent", '\x21a'), ("tcommaaccent", '\x21b'), ("dotlessj", '\x237')
  -- Spacing modifiers
  , ("circumflex", '\x2c6'), ("caron", '\x2c7'), ("breve", '\x2d8')
  , ("dotaccent", '\x2d9'), ("ring", '\x2da'), ("ogonek", '\x2db'), ("tilde", '\x2dc')
  , ("hungarumlaut", '\x2dd')
  -- Greek (the Symbol font's names)
  , ("Alpha", '\x391'), ("Beta", '\x392'), ("Gamma", '\x393'), ("Delta", '\x394')
  , ("Epsilon", '\x395'), ("Zeta", '\x396'), ("Eta", '\x397'), ("Theta", '\x398')
  , ("Iota", '\x399'), ("Kappa", '\x39a'), ("Lambda", '\x39b'), ("Mu", '\x39c')
  , ("Nu", '\x39d'), ("Xi", '\x39e'), ("Omicron", '\x39f'), ("Pi", '\x3a0')
  , ("Rho", '\x3a1'), ("Sigma", '\x3a3'), ("Tau", '\x3a4'), ("Upsilon", '\x3a5')
  , ("Phi", '\x3a6'), ("Chi", '\x3a7'), ("Psi", '\x3a8'), ("Omega", '\x3a9')
  , ("alpha", '\x3b1'), ("beta", '\x3b2'), ("gamma", '\x3b3'), ("delta", '\x3b4')
  , ("epsilon", '\x3b5'), ("zeta", '\x3b6'), ("eta", '\x3b7'), ("theta", '\x3b8')
  , ("iota", '\x3b9'), ("kappa", '\x3ba'), ("lambda", '\x3bb'), ("mu", '\x3bc')
  , ("nu", '\x3bd'), ("xi", '\x3be'), ("omicron", '\x3bf'), ("pi", '\x3c0')
  , ("rho", '\x3c1'), ("sigma1", '\x3c2'), ("sigma", '\x3c3'), ("tau", '\x3c4')
  , ("upsilon", '\x3c5'), ("phi", '\x3c6'), ("chi", '\x3c7'), ("psi", '\x3c8')
  , ("omega", '\x3c9'), ("theta1", '\x3d1'), ("Upsilon1", '\x3d2'), ("phi1", '\x3d5')
  , ("omega1", '\x3d6')
  -- Punctuation
  , ("figuredash", '\x2012'), ("endash", '\x2013'), ("emdash", '\x2014')
  , ("quoteleft", '\x2018'), ("quoteright", '\x2019'), ("quotesinglbase", '\x201a')
  , ("quotereversed", '\x201b'), ("quotedblleft", '\x201c'), ("quotedblright", '\x201d')
  , ("quotedblbase", '\x201e'), ("dagger", '\x2020'), ("daggerdbl", '\x2021')
  , ("bullet", '\x2022'), ("onedotenleader", '\x2024'), ("twodotenleader", '\x2025')
  , ("ellipsis", '\x2026'), ("perthousand", '\x2030'), ("minute", '\x2032')
  , ("second", '\x2033'), ("guilsinglleft", '\x2039'), ("guilsinglright", '\x203a')
  , ("exclamdbl", '\x203c'), ("fraction", '\x2044'), ("colonmonetary", '\x20a1')
  , ("franc", '\x20a3'), ("lira", '\x20a4'), ("peseta", '\x20a7'), ("dong", '\x20ab')
  , ("Euro", '\x20ac'), ("trademark", '\x2122'), ("Ifraktur", '\x2111')
  , ("weierstrass", '\x2118'), ("Rfraktur", '\x211c'), ("aleph", '\x2135')
  , ("afii61289", '\x2113'), ("afii61352", '\x2116'), ("estimated", '\x212e')
  , ("onethird", '\x2153'), ("twothirds", '\x2154'), ("oneeighth", '\x215b')
  , ("threeeighths", '\x215c'), ("fiveeighths", '\x215d'), ("seveneighths", '\x215e')
  -- Arrows
  , ("arrowleft", '\x2190'), ("arrowup", '\x2191'), ("arrowright", '\x2192')
  , ("arrowdown", '\x2193'), ("arrowboth", '\x2194'), ("arrowupdn", '\x2195')
  , ("carriagereturn", '\x21b5'), ("arrowdblleft", '\x21d0'), ("arrowdblup", '\x21d1')
  , ("arrowdblright", '\x21d2'), ("arrowdbldown", '\x21d3'), ("arrowdblboth", '\x21d4')
  -- Mathematics
  , ("universal", '\x2200'), ("partialdiff", '\x2202'), ("existential", '\x2203')
  , ("emptyset", '\x2205'), ("increment", '\x2206'), ("gradient", '\x2207')
  , ("element", '\x2208'), ("notelement", '\x2209'), ("suchthat", '\x220b')
  , ("product", '\x220f'), ("summation", '\x2211'), ("minus", '\x2212')
  , ("asteriskmath", '\x2217'), ("radical", '\x221a'), ("proportional", '\x221d')
  , ("infinity", '\x221e'), ("angle", '\x2220'), ("logicaland", '\x2227')
  , ("logicalor", '\x2228'), ("intersection", '\x2229'), ("union", '\x222a')
  , ("integral", '\x222b'), ("therefore", '\x2234'), ("similar", '\x223c')
  , ("congruent", '\x2245'), ("approxequal", '\x2248'), ("notequal", '\x2260')
  , ("equivalence", '\x2261'), ("lessequal", '\x2264'), ("greaterequal", '\x2265')
  , ("propersubset", '\x2282'), ("propersuperset", '\x2283'), ("notsubset", '\x2284')
  , ("reflexsubset", '\x2286'), ("reflexsuperset", '\x2287'), ("circleplus", '\x2295')
  , ("circlemultiply", '\x2297'), ("perpendicular", '\x22a5'), ("dotmath", '\x22c5')
  , ("integraltp", '\x2320'), ("integralbt", '\x2321'), ("angleleft", '\x2329')
  , ("angleright", '\x232a')
  -- Shapes and suits
  , ("lozenge", '\x25ca'), ("spade", '\x2660'), ("club", '\x2663')
  , ("heart", '\x2665'), ("diamond", '\x2666'), ("filledbox", '\x25a0')
  , ("H22073", '\x25a1'), ("circle", '\x25cb'), ("H18533", '\x25cf')
  , ("triagup", '\x25b2'), ("triagdn", '\x25bc'), ("dotlessj", '\x237')
  -- Ligatures (presentation forms)
  , ("ff", '\xfb00'), ("fi", '\xfb01'), ("fl", '\xfb02'), ("ffi", '\xfb03')
  , ("ffl", '\xfb04')
  ]
