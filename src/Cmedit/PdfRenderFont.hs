{-# LANGUAGE BangPatterns #-}
-- | PDF fonts for the page renderer ("Cmedit.PdfRender"): from a font
-- dictionary to the three things drawing text needs per code: how many
-- bytes the code takes, how far it advances, and which outline it draws.
--
-- "Cmedit.Font" reads font /programs/ and decides none of PDF's rules;
-- this module is those rules (PDF 32000 §9.6–9.10), kept pure so it can be
-- tested without a single system font:
--
--   * __Simple fonts__ map a one-byte code to a glyph /name/ (the base
--     encoding: the dictionary's @\/Encoding@ or @\/BaseEncoding@, else the
--     program's built-in encoding for an embedded Type 1 or CFF, else
--     Symbol or ZapfDingbats for those two, else WinAnsi for TrueType and
--     Standard for the rest; then @\/Differences@ over it), and the name to
--     a GID. An embedded __TrueType__ program follows §9.6.6.4 instead,
--     because its glyphs are reached through cmaps, not names: a
--     non-symbolic font goes name → Unicode → @(3,1)@, or name → Mac Roman
--     code → @(1,0)@; a symbolic one tries @(3,0)@ with the code and with
--     @0xF000@, @0xF100@, @0xF200@ added, then @(1,0)@; either then falls
--     back to the @post@ names and finally to GID = code, which is what
--     subset producers that write neither rely on.
--
--   * __Composite fonts__ split the string through the CMap's codespace
--     ranges (§9.7.6.2: one byte at a time until a range of that length
--     matches) and map each code to a CID (@Identity-H\/V@ natively, an
--     embedded CMap's @cidrange@\/@cidchar@\/@notdefrange@ with @usecmap@
--     followed through embedded streams), then a CID to a GID through the
--     CFF charset or @\/CIDToGIDMap@. The predefined CJK CMaps
--     (@UniJIS-UCS2-H@ and friends) cannot be bundled: a UCS-2/UTF-16 one
--     is read as "the code is the character" (exactly true, and what a
--     substitute font needs), any other is read as two-byte Identity and
--     noted.
--
--   * __A font with no usable program is drawn with a substitute__ that the
--     caller found ('fontNeed' is the request; the page renderer's
--     environment answers it). Codes reach the substitute by glyph name,
--     then by Unicode (the name through the Adobe Glyph List, then
--     @\/ToUnicode@, then the code as Latin-1), and each glyph is
--     __stretched horizontally so its advance equals the PDF's width__: the
--     document's widths position every glyph whatever face draws it, and a
--     substitute's own advance only decides how the glyph fills its slot
--     (clamped to a quarter and four times, past which the "substitute" is
--     plainly the wrong design and stretching would only make it worse). A
--     standard-14 font written without @\/Widths@ keeps the substitute's own
--     advances, which are the standard metrics when the substitute is the
--     URW clone FontSys prefers.
--
-- Glyph keys are unique per (font, GID) for the rasteriser's glyph cache:
-- @(fontId + 1) * 2^17 + gid@, with a substitute's face key moved into its
-- own range (bit 52) so two PDF fonts drawn by one substitute share
-- entries, and a font that is not an object of its own (no stable id) gets
-- negative keys (never cached). Outlines are decoded lazily and once per
-- font: a code's glyph is a thunk in a per-font table.
--
-- Bounds: CMaps are charged to the document's entry budget and a range
-- covers at most 65536 codes. @\/W@ and @\/W2@ are kept as /intervals/
-- (a range is one entry whatever it spans; expanding them let 300 bytes of
-- ranges build a million-entry map per font, 1.1 GB for twenty fonts), and
-- what their arrays cost is charged to the document's entry and token
-- budgets, at most 'widthEntriesPerFont' per table. A font program is
-- decoded under the document's byte budget. Nothing here throws.
module Cmedit.PdfRenderFont
  ( PFont(..)
  , FontKind(..)
  , GlyphRef(..)
  , T3Font(..)
  , loadPdfFont
  , fontNeed
  , fontRequestFor
  , CMap(..)
  , parseCMapBytes
  , identityCMap
  , splitCodes
  , cmapCid
  , toUnicodeFirst
  ) where

import Data.Array (Array, listArray, (!), bounds)
import Data.Bits (shiftL, shiftR, (.&.), (.|.), testBit)
import Data.Char (chr, ord, isDigit, isHexDigit, digitToInt, toLower)
import Data.List (foldl', isSuffixOf, isInfixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Text as T
import Data.Word (Word8)

import Cmedit.Font (Font, FontFormat(..), FontHint(..))
import qualified Cmedit.Font as F
import Cmedit.FontSys (FontRequest(..))
import Cmedit.Geom
import qualified Cmedit.IntervalMap as IV
import Cmedit.PdfObj

data FontKind = FkSimple | FkCID | FkType3
  deriving (Eq, Show)

-- | One drawable glyph: the cache key, the outline (glyph space, lazily
-- decoded), and glyph space → text space in ems (the font matrix, and a
-- substitute's stretch).
data GlyphRef = GlyphRef
  { grKey  :: !Int
  , grPath :: Path
  , grMat  :: !Affine
  }

-- | A Type 3 font: glyph procedures run by the renderer.
data T3Font = T3Font
  { t3Matrix :: !Affine
  , t3Res    :: Maybe Dict
  , t3BBox   :: Maybe Rect
  , t3Proc   :: Int -> Maybe Obj   -- ^ Code → its CharProcs stream (as the file has it: usually a reference).
  }

data PFont = PFont
  { pfName      :: String               -- ^ @\/BaseFont@, for notes.
  , pfKind      :: !FontKind
  , pfCodes     :: BS.ByteString -> [(Int, Int)]
      -- ^ A string's codes with their byte lengths.
  , pfWidth     :: Int -> Int -> Double -- ^ Code, byte length → horizontal displacement @w0@ in text space (ems).
  , pfVertical  :: !Bool
  , pfVMetric   :: Int -> Int -> (Double, Double, Double)
      -- ^ Vertical writing: (@w1y@, @vx@, @vy@) in ems.
  , pfGlyph     :: Int -> Int -> Maybe GlyphRef
  , pfDrawable  :: !Bool                -- ^ False: no program and no substitute (text not shown).
  , pfSubst     :: !Bool                -- ^ Drawn with a substitute.
  , pfType3     :: Maybe T3Font
  , pfNotes     :: [String]
  }

------------------------------------------------------------------------------
-- What to ask the font provider for

-- | The substitute request for a font dictionary: its family as written and
-- the descriptor's flags. 'Nothing' when the font needs none (Type 3, or an
-- embedded program that parses).
fontNeed :: PdfFile -> Fuel -> Dict -> (Maybe FontRequest, Fuel)
fontNeed f fu d
  | subtype == BC.pack "Type3" = (Nothing, fu)
  | otherwise = case embeddedProgram f fu (descriptorOf f d) of
      (Just (Right _), fu') -> (Nothing, fu')
      (_, fu') -> (Just (fontRequestFor f d), fu')
  where subtype = nameOf (lookD f d "Subtype")

-- | The request for a font, embedded or not (the renderer asks for exactly
-- this when an embedded program turns out unusable).
fontRequestFor :: PdfFile -> Dict -> FontRequest
fontRequestFor f d = FontRequest
  { frFamily = T.pack (BC.unpack fam)
  , frBold = testBit flags 18 || weight >= 600 || stemV >= 120
  , frItalic = testBit flags 6 || italicAngle /= 0
  , frMono = testBit flags 0
  , frSerif = testBit flags 1
  , frSymbolic = testBit flags 2 && not (testBit flags 5)
  }
  where
    isType0 = nameOf (lookD f d "Subtype") == BC.pack "Type0"
    desc = descendantOf f d
    fam0 = nameOf (lookD f (if isType0 then desc else d) "BaseFont")
    fam = if BS.null fam0 then nameOf (lookD f d "BaseFont") else fam0
    descr = descriptorOf f d
    flags = intOf f (M.lookup (BC.pack "Flags") descr) :: Int
    weight = numOf f (M.lookup (BC.pack "FontWeight") descr)
    stemV = numOf f (M.lookup (BC.pack "StemV") descr)
    italicAngle = numOf f (M.lookup (BC.pack "ItalicAngle") descr)

descendantOf :: PdfFile -> Dict -> Dict
descendantOf f d = case arrOf f (lookD f d "DescendantFonts") of
  (x : _) -> dictOf f x
  [] -> M.empty

descriptorOf :: PdfFile -> Dict -> Dict
descriptorOf f d
  | nameOf (lookD f d "Subtype") == BC.pack "Type0" = dictOf f (lookD f (descendantOf f d) "FontDescriptor")
  | otherwise = dictOf f (lookD f d "FontDescriptor")

-- The embedded font program: Nothing when there is none.
embeddedProgram :: PdfFile -> Fuel -> Dict -> (Maybe (Either String Font), Fuel)
embeddedProgram f fu descr = case candidates of
  [] -> (Nothing, fu)
  ((o, hint) : _) -> case resolve f o of
    OStream sd raw -> case decodeStreamF f fu sd raw of
      (Left e, fu') -> (Just (Left e), fu')
      (Right bytes, fu') ->
        let h = hint sd
        in (Just (F.parseFontAs h bytes), spendTokens (BS.length bytes `div` 64 + 1) fu')
    _ -> (Just (Left "font file is not a stream"), fu)
  where
    candidates = [ (o, h) | (k, h) <- [("FontFile", t1Hint), ("FontFile2", const HintTrueType), ("FontFile3", ff3Hint)]
                          , Just o <- [M.lookup (BC.pack k) descr] ]
    t1Hint sd = let l1 = intOf f (M.lookup (BC.pack "Length1") sd)
                    l2 = intOf f (M.lookup (BC.pack "Length2") sd)
                in HintType1 (if l1 > 0 && l2 > 0 then Just (l1, l2) else Nothing)
    ff3Hint sd = case BC.unpack (nameOf (lookD f sd "Subtype")) of
      "OpenType" -> HintOpenType
      _ -> HintCFF

------------------------------------------------------------------------------
-- Loading

-- | Load a font dictionary. @fid@ is the font's object number (or a
-- negative number for a font with none: its glyphs are then never cached);
-- @lookupSub@ answers a substitute request.
loadPdfFont :: PdfFile -> Fuel -> (FontRequest -> Maybe (Int, Font)) -> Int -> Dict -> (PFont, Fuel)
loadPdfFont f fu0 lookupSub fid d
  | subtype == BC.pack "Type3" = type3Font f fu0 d
  | subtype == BC.pack "Type0" = compositeFont f fu0 lookupSub fid d
  | otherwise = simpleFont f fu0 lookupSub fid d
  where subtype = nameOf (lookD f d "Subtype")

baseName :: PdfFile -> Dict -> String
baseName f d = BC.unpack (BS.take 80 (nameOf (lookD f d "BaseFont")))

-- The program to draw with: embedded, else a substitute.
chooseProgram :: PdfFile -> Fuel -> (FontRequest -> Maybe (Int, Font)) -> Dict -> Dict
              -> ((Maybe (Int, Font, Bool)), [String], Fuel)
chooseProgram f fu lookupSub d descr = case embeddedProgram f fu descr of
  (Just (Right fnt), fu') -> (Just (-1, fnt, False), [], fu')
  (Just (Left _), fu') -> substitute ["embedded font " ++ baseName f d ++ " could not be read; substituted"] fu'
  (Nothing, fu') -> substitute [] fu'
  where
    substitute notes fu' = case lookupSub (fontRequestFor f d) of
      Just (k, fnt) -> (Just (k, fnt, True), notes, fu')
      Nothing -> (Nothing, notes, fu')

glyphKey :: Int -> Bool -> Int -> Int -> Int
glyphKey fid isSub subKey gid
  | isSub = (1 `shiftL` 52) .|. ((subKey + 1) `shiftL` 17) .|. (gid .&. 0x1ffff)
  | fid < 0 = -1
  | otherwise = ((fid + 1) `shiftL` 17) .|. (gid .&. 0x1ffff)

-- A table of lazily decoded outlines, one per GID.
pathTable :: Font -> Int -> Path
pathTable fnt = \g -> if g >= 0 && g < n then tbl ! g else []
  where
    n = min 65536 (F.fontNumGlyphs fnt)
    tbl :: Array Int Path
    tbl = listArray (0, max 0 (n - 1)) (if n > 0 then [ F.glyphPath fnt g | g <- [0 .. n - 1] ] else [[]])

-- Glyph advance in ems.
advanceEm :: Font -> Int -> Double
advanceEm fnt g = let Affine a _ _ _ _ _ = F.fontMatrix fnt in F.glyphAdvance fnt g * a

-- The stretch that makes a substitute's glyph fill the PDF's width.
stretchFor :: Double -> Double -> Double
stretchFor w0 adv
  | w0 > 0 && adv > 0 = let s = w0 / adv in if abs (s - 1) < 0.01 then 1 else max 0.25 (min 4 s)
  | otherwise = 1

------------------------------------------------------------------------------
-- Simple fonts (Type1, MMType1, TrueType)

simpleFont :: PdfFile -> Fuel -> (FontRequest -> Maybe (Int, Font)) -> Int -> Dict -> (PFont, Fuel)
simpleFont f fu0 lookupSub fid d = (pf, fu2)
  where
    descr = descriptorOf f d
    (prog, notes0, fu1) = chooseProgram f fu0 lookupSub d descr
    (toUni, fu2) = toUnicodeOf f fu1 d
    subtype = nameOf (lookD f d "Subtype")
    isTT = subtype == BC.pack "TrueType"
    flags = intOf f (M.lookup (BC.pack "Flags") descr) :: Int
    symbolic = testBit flags 2 && not (testBit flags 5)
    bname = baseName f d
    lowName = map toLower bname
    encObj = lookD f d "Encoding"
    explicitBase = case encObj of
      OName n -> encodingByName n
      ODict ed -> case lookD f ed "BaseEncoding" of
        OName n -> encodingByName n
        _ -> Nothing
      _ -> Nothing
    progFont = fmap (\(_, x, _) -> x) prog
    embedded = case prog of Just (_, _, False) -> True; _ -> False
    builtin = progFont >>= F.fontBuiltinEncoding >>= \m -> Just (IM.map BC.pack m)
    symbolName
      | "symbol" `isInfixOf` lowName = encodingByName (BC.pack "Symbol")
      | "dingbat" `isInfixOf` lowName = encodingByName (BC.pack "ZapfDingbats")
      | otherwise = Nothing
    baseNames = case explicitBase of
      Just t -> t
      Nothing
        | embedded, not isTT, Just b <- builtin -> b
        | not embedded, Just t <- symbolName -> t
        | isTT -> winAnsiEncodingNames
        | otherwise -> standardEncodingNames
    names = differences f encObj baseNames
    hasDiffs = case encObj of ODict _ -> True; _ -> False
    nameAt c = IM.lookup c names

    -- Widths.
    firstChar = intOf f (M.lookup (BC.pack "FirstChar") d)
    widthsList = arrOf f (lookD f d "Widths")
    widths = IM.fromList (zip [firstChar ..] (map (numOf f . Just) (take 256 widthsList)))
    missingW = numOf f (M.lookup (BC.pack "MissingWidth") descr)
    pdfWidth c = case IM.lookup c widths of
      Just w -> Just (w / 1000)
      Nothing | null widthsList -> Nothing
              | otherwise -> Just (missingW / 1000)

    gidFor :: Font -> Bool -> Int -> Maybe Int
    gidFor fnt isSub c
      | not isSub && F.fontFormat fnt == FmtTrueType = ttGid fnt c
      | not isSub = nameGid fnt c
      | otherwise = subGid fnt c
    validG fnt g = if g > 0 && g < F.fontNumGlyphs fnt then Just g else Nothing
    byName fnt n = F.glyphByName fnt (BC.unpack n) >>= validG fnt
    charOfName n = case glyphUnicode n of
      Just u -> Just (chr' u)
      Nothing -> F.aglNameToChar (BC.unpack n)
    chr' u = if u >= 0 && u <= 0x10ffff then chr u else '\xfffd'
    -- Embedded Type 1 / CFF: by name, then the program's own encoding,
    -- then by Unicode.
    nameGid fnt c = firstJ
      [ nameAt c >>= byName fnt
      , builtin >>= IM.lookup c >>= byName fnt
      , nameAt c >>= charOfName >>= F.glyphForChar fnt >>= validG fnt
      , IM.lookup c toUni >>= F.glyphForChar fnt >>= validG fnt
      , if F.fontFormat fnt == FmtCFFCID then F.glyphForCID fnt c >>= validG fnt else Nothing ]
    -- Embedded TrueType: PDF 32000 §9.6.6.4.
    ttGid fnt c =
      let cm p e = F.cmapLookup fnt p e
          via p e k = cm p e >>= \lk -> lk k >>= validG fnt
          has p e = isJust (cm p e)
          symbolicPath = firstJ ([ via 3 0 (b + c) | b <- [0, 0xF000, 0xF100, 0xF200] ] ++ [via 1 0 c])
          nonSymbolic = firstJ
            [ nameAt c >>= charOfName >>= \ch -> via 3 1 (ord ch)
            , nameAt c >>= \n -> M.lookup n macRomanCodeOf >>= via 1 0
            , if not hasDiffs && not (isJust explicitBase) then via 1 0 c else Nothing ]
          useSym = symbolic || not (has 3 1)
      in firstJ
           [ if useSym then firstJ [symbolicPath, nonSymbolic] else firstJ [nonSymbolic, symbolicPath]
           , via 3 1 c
           , nameAt c >>= byName fnt
           , IM.lookup c toUni >>= F.glyphForChar fnt >>= validG fnt
           , validG fnt c ]
    -- A substitute: by name, then by Unicode.
    subGid fnt c = firstJ
      [ nameAt c >>= byName fnt
      , nameAt c >>= charOfName >>= F.glyphForChar fnt >>= validG fnt
      , IM.lookup c toUni >>= F.glyphForChar fnt >>= validG fnt
      , if c >= 32 && c < 256 && not (isJust (nameAt c)) then F.glyphForChar fnt (chr c) >>= validG fnt else Nothing ]

    glyphTable :: Array Int (Maybe GlyphRef)
    glyphTable = listArray (0, 255) [ mk c | c <- [0 .. 255] ]
    mk c = case prog of
      Nothing -> Nothing
      Just (sk, fnt, isSub) -> do
        g <- gidFor fnt isSub c
        let fm = F.fontMatrix fnt
            st = if isSub then maybe 1 (\w -> stretchFor w (advanceEm fnt g)) (pdfWidth c) else 1
            m = if st == 1 then fm else composeA (scaleA st 1) fm
        Just (GlyphRef (glyphKey fid isSub sk g) (paths g) m)
    paths = case prog of
      Just (_, fnt, _) -> pathTable fnt
      Nothing -> const []
    widthOf c = case pdfWidth c of
      Just w -> w
      Nothing -> case prog of
        Just (_, fnt, isSub) -> maybe 0 (advanceEm fnt) (gidFor fnt isSub c)
        Nothing -> 0.5
    widthTable :: Array Int Double
    widthTable = listArray (0, 255) [ widthOf c | c <- [0 .. 255] ]
    pf = PFont
      { pfName = bname
      , pfKind = FkSimple
      , pfCodes = \s -> [ (fromIntegral b, 1) | b <- BS.unpack s ]
      , pfWidth = \c _ -> if c >= 0 && c <= 255 then widthTable ! c else 0
      , pfVertical = False
      , pfVMetric = \_ _ -> (0, 0, 0)
      , pfGlyph = \c _ -> if c >= 0 && c <= 255 then glyphTable ! c else Nothing
      , pfDrawable = isJust prog
      , pfSubst = case prog of Just (_, _, s) -> s; Nothing -> False
      , pfType3 = Nothing
      , pfNotes = notes0
      }

-- Mac Roman: glyph name → code, for the (1,0) cmap.
macRomanCodeOf :: M.Map BS.ByteString Int
macRomanCodeOf = M.fromList [ (n, c) | (c, n) <- IM.toList macRomanEncodingNames ]

firstJ :: [Maybe a] -> Maybe a
firstJ (Just x : _) = Just x
firstJ (_ : r) = firstJ r
firstJ [] = Nothing

------------------------------------------------------------------------------
-- Type 3

type3Font :: PdfFile -> Fuel -> Dict -> (PFont, Fuel)
type3Font f fu d = (pf, spendTokens (M.size procs + 1) fu)
  where
    fm = fromMaybe (Affine 0.001 0 0 0.001 0 0) (matrixOf f (lookD f d "FontMatrix"))
    Affine fa _ _ _ _ _ = fm
    procs = dictOf f (lookD f d "CharProcs")
    names = differences f (lookD f d "Encoding") IM.empty
    firstChar = intOf f (M.lookup (BC.pack "FirstChar") d)
    widths = IM.fromList (zip [firstChar ..] (map (numOf f . Just) (take 256 (arrOf f (lookD f d "Widths")))))
    res = case lookD f d "Resources" of
      ODict r -> Just r
      _ -> Nothing
    pf = PFont
      { pfName = case baseName f d of "" -> "Type 3"; n -> n
      , pfKind = FkType3
      , pfCodes = \s -> [ (fromIntegral b, 1) | b <- BS.unpack s ]
      , pfWidth = \c _ -> IM.findWithDefault 0 c widths * fa
      , pfVertical = False
      , pfVMetric = \_ _ -> (0, 0, 0)
      , pfGlyph = \_ _ -> Nothing
      , pfDrawable = True
      , pfSubst = False
      , pfType3 = Just T3Font
          { t3Matrix = fm
          , t3Res = res
          , t3BBox = rectOf f (lookD f d "FontBBox")
          , t3Proc = \c -> IM.lookup c names >>= \n -> M.lookup n procs }
      , pfNotes = []
      }

------------------------------------------------------------------------------
-- Composite fonts

compositeFont :: PdfFile -> Fuel -> (FontRequest -> Maybe (Int, Font)) -> Int -> Dict -> (PFont, Fuel)
compositeFont f fu0 lookupSub fid d = (pf, fu6)
  where
    desc = descendantOf f d
    descr = descriptorOf f d
    (cmap, cmNotes, fu1) = cmapFor f fu0 (M.findWithDefault ONull (BC.pack "Encoding") d)
    (prog, notes0, fu2) = chooseProgram f fu1 lookupSub d descr
    (toUni, fu3) = toUnicodeOf f fu2 d
    isType2 = nameOf (lookD f desc "Subtype") == BC.pack "CIDFontType2"
    (cidToGid, fu4) = case lookD f desc "CIDToGIDMap" of
      OStream sd raw -> case decodeStreamF f fu3 sd raw of
        (Right b, fu') -> (Just b, fu')
        (Left _, fu') -> (Nothing, fu')
      _ -> (Nothing, fu3)
    -- Widths are stored as intervals, so a three-number range costs one
    -- entry however many CIDs it spans; what explicit arrays cost is
    -- charged to the document's entry and token budgets, and a font may
    -- take no more than what is left of them.
    wLimit fu = max 0 (min widthEntriesPerFont (fuEntries fu))
    (wmap, wCost) = cidW f (wLimit fu4) (lookD f desc "W")
    fu5 = spendTokens wCost (spendEntries wCost fu4)
    (w2map, w2Cost) = cidW2 f (wLimit fu5) (lookD f desc "W2")
    fu6 = spendTokens w2Cost (spendEntries w2Cost fu5)
    dw = case lookD f desc "DW" of ONum v -> v; _ -> 1000
    (dw2y, dw2w) = case numsOf f (lookD f desc "DW2") of
      [a, b] -> (a, b)
      _ -> (880, -1000)
    cidOf code n = cmapCid cmap code n
    w0cid cid = fromMaybe dw (IV.lookupKey cid wmap) / 1000
    gidOf :: Font -> Bool -> Int -> Int -> Maybe Int
    gidOf fnt isSub code cid
      | isSub = firstJ
          [ IM.lookup code toUni >>= F.glyphForChar fnt >>= validG
          , if cmUnicode cmap then F.glyphForChar fnt (chr' code) >>= validG else Nothing ]
      | isType2 = case cidToGid of
          Just b -> let i = 2 * cid
                    in if i + 1 < BS.length b
                         then validG (fromIntegral (BSU.unsafeIndex b i) * 256 + fromIntegral (BSU.unsafeIndex b (i + 1)))
                         else Nothing
          Nothing -> validG cid
      | otherwise = F.glyphForCID fnt cid >>= validG
      where validG g = if g > 0 && g < F.fontNumGlyphs fnt then Just g else Nothing
    chr' u = if u >= 0 && u <= 0x10ffff && not (u >= 0xd800 && u <= 0xdfff) then chr u else '\xfffd'
    paths = case prog of
      Just (_, fnt, _) -> pathTable fnt
      Nothing -> const []
    glyph code n = do
      (sk, fnt, isSub) <- prog
      let cid = cidOf code n
      g <- gidOf fnt isSub code cid
      let fm = F.fontMatrix fnt
          st = if isSub then stretchFor (w0cid cid) (advanceEm fnt g) else 1
          m = if st == 1 then fm else composeA (scaleA st 1) fm
      Just (GlyphRef (glyphKey fid isSub sk g) (paths g) m)
    pf = PFont
      { pfName = baseName f d
      , pfKind = FkCID
      , pfCodes = splitCodes cmap
      , pfWidth = \code n -> w0cid (cidOf code n)
      , pfVertical = cmVertical cmap
      , pfVMetric = \code n ->
          let cid = cidOf code n
          in case IV.lookupKey cid w2map of
               Just (w1, vx, vy) -> (w1 / 1000, vx / 1000, vy / 1000)
               Nothing -> (dw2w / 1000, w0cid cid / 2, dw2y / 1000)
      , pfGlyph = glyph
      , pfDrawable = isJust prog
      , pfSubst = case prog of Just (_, _, s) -> s; Nothing -> False
      , pfType3 = Nothing
      , pfNotes = cmNotes ++ notes0
      }

-- | Entries one font's @\/W@ (and again its @\/W2@) may hold.
widthEntriesPerFont :: Int
widthEntriesPerFont = 1048576

-- | @\/W@: @[c [w …] cFirst cLast w …]@, as "Cmedit.IntervalMap" intervals
-- (an array element is an interval of one; a later entry wins). At most @limit@ entries; the second result is what
-- was spent (entries written plus one per item read).
cidW :: PdfFile -> Int -> Obj -> (IV.IntervalMap Double, Int)
cidW f limit o = go limit (arrOf f o) IV.empty
  where
    go left _ acc | left <= 0 = (acc, limit)
    go left (a : rest) acc = case (resolve f a, rest) of
      (ONum c, b : more) | ok c -> case resolve f b of
        OArr ws -> let ws' = take left ws
                       c0 = truncate c
                   in go (left - length ws' - 1) more
                         (foldl' (\m (i, v) -> IV.insertKey i (numOf f (Just v)) m) acc (zip [c0 ..] ws'))
        ONum c2 | ok c2 -> case more of
          (wv : more') | truncate c2 >= (truncate c :: Int) ->
            go (left - 1) more' (IV.insertRange (truncate c) (truncate c2) (numOf f (Just wv)) acc)
          (_ : more') -> go (left - 1) more' acc
          [] -> (acc, limit - left)
        _ -> go (left - 1) more acc
      _ -> (acc, limit - left)
    go left [] acc = (acc, limit - left)
    ok v = v >= 0 && v < 4294967296

-- | @\/W2@: @[c [w1y vx vy …] cFirst cLast w1y vx vy …]@, likewise.
cidW2 :: PdfFile -> Int -> Obj -> (IV.IntervalMap (Double, Double, Double), Int)
cidW2 f limit o = go limit (arrOf f o) IV.empty
  where
    go left _ acc | left <= 0 = (acc, limit)
    go left (a : rest) acc = case (resolve f a, rest) of
      (ONum c, b : more) | ok c -> case resolve f b of
        OArr ws -> let trip = take left (triples (map (numOf f . Just) (take (3 * left) ws)))
                       c0 = truncate c
                   in go (left - length trip - 1) more
                         (foldl' (\m (i, v) -> IV.insertKey i v m) acc (zip [c0 ..] trip))
        ONum c2 | ok c2 -> case more of
          (x : y : z : more') | truncate c2 >= (truncate c :: Int) ->
            go (left - 1) more' (IV.insertRange (truncate c) (truncate c2) (numOf f (Just x), numOf f (Just y), numOf f (Just z)) acc)
          (_ : _ : _ : more') -> go (left - 1) more' acc
          _ -> (acc, limit - left)
        _ -> go (left - 1) more acc
      _ -> (acc, limit - left)
    go left [] acc = (acc, limit - left)
    triples (x : y : z : r) = (x, y, z) : triples r
    triples _ = []
    ok v = v >= 0 && v < 4294967296

------------------------------------------------------------------------------
-- CMaps

-- | A CMap: codespace ranges (byte length, per-byte bounds) and code → CID
-- ranges keyed by @(length << 32) | low@.
data CMap = CMap
  { cmSpaces   :: [(Int, [(Word8, Word8)])]
  , cmRanges   :: IM.IntMap (Int, Int)     -- ^ Key → (high code, CID of the low code).
  , cmNotdef   :: IM.IntMap (Int, Int)
  , cmIdentity :: !Bool                   -- ^ CID = code (no table).
  , cmVertical :: !Bool
  , cmUnicode  :: !Bool                   -- ^ A predefined UCS-2/UTF-16 CMap: the code is the character.
  }

identityCMap :: Bool -> CMap
identityCMap v = CMap [(2, [(0, 255), (0, 255)])] IM.empty IM.empty True v False

-- The CMap a Type 0 font's @\/Encoding@ names or embeds.
cmapFor :: PdfFile -> Fuel -> Obj -> (CMap, [String], Fuel)
cmapFor f fu0 o0 = go (8 :: Int) IS.empty fu0 o0
  where
    go 0 _ fu _ = (identityCMap False, [], fu)
    go k seen fu o = case resolve f o of
      OName n -> (named n, namedNote n, fu)
      OStream sd raw
        | Just r <- refOf o, r `IS.member` seen -> (identityCMap False, [], fu)
        | otherwise -> case decodeStreamF f fu sd raw of
            (Left _, fu') -> (identityCMap False, ["a CMap could not be read; read as Identity"], fu')
            (Right b, fu') ->
              let parentObj = M.lookup (BC.pack "UseCMap") sd
                  wv = intOf f (M.lookup (BC.pack "WMode") sd) == 1
                  (parent, pnotes, fu'') = case parentObj of
                    Just p -> go (k - 1) (maybe seen (`IS.insert` seen) (refOf o)) fu' p
                    Nothing -> case useCMapName b of
                      Just n -> (named n, namedNote n, fu')
                      Nothing -> (CMap [] IM.empty IM.empty False False False, [], fu')
                  (cm, used) = parseCMapBytes (fuEntries fu'') b
                  merged = cm { cmSpaces = if null (cmSpaces cm) then cmSpaces parent else cmSpaces cm
                              , cmRanges = IM.union (cmRanges cm) (cmRanges parent)
                              , cmNotdef = IM.union (cmNotdef cm) (cmNotdef parent)
                              , cmIdentity = IM.null (cmRanges cm) && cmIdentity parent
                              , cmVertical = cmVertical cm || wv || (null (cmSpaces cm) && cmVertical parent) }
                  final = if null (cmSpaces merged) then merged { cmSpaces = cmSpaces (identityCMap False) } else merged
              in (final, pnotes, spendEntries used fu'')
      _ -> (identityCMap False, [], fu)
    named n
      | s == "Identity-H" = identityCMap False
      | s == "Identity-V" = identityCMap True
      | "UCS2" `isInfixOf` s || "UTF16" `isInfixOf` s = (identityCMap vert) { cmIdentity = True, cmUnicode = True }
      | otherwise = identityCMap vert
      where s = BC.unpack n
            vert = "-V" `isSuffixOf` s
    namedNote n
      | s `elem` ["Identity-H", "Identity-V"] = []
      | "UCS2" `isInfixOf` s || "UTF16" `isInfixOf` s = []
      | otherwise = ["predefined CMap " ++ take 40 s ++ " read as Identity"]
      where s = BC.unpack n
    useCMapName b =
      let toks = contentTokens (BS.take 4096 b)
          find' (TObj (OName n) : TOp u : _) | u == BC.pack "usecmap" = Just n
          find' (_ : r) = find' r
          find' [] = Nothing
      in find' toks

-- | Parse a CMap's body: codespace, cid and notdef ranges and @\/WMode@,
-- holding at most @limit@ entries. The second result is how many entries
-- it cost.
parseCMapBytes :: Int -> BS.ByteString -> (CMap, Int)
parseCMapBytes limit0 b = (CMap (reverse spaces) ranges notdefs False vert False, limit - left)
  where
    limit = max 0 (min limit0 1048576)
    toks = contentTokens b
    (spaces, ranges, notdefs, vert, left) = go toks [] IM.empty IM.empty False limit
    go [] sp rg nd v l = (sp, rg, nd, v, l)
    go (TObj (OName w) : TObj (ONum x) : rest) sp rg nd _ l | w == BC.pack "WMode" = go rest sp rg nd (x == 1) l
    go (TOp op : rest) sp rg nd v l
      | l <= 0 = (sp, rg, nd, v, l)
      | op == BC.pack "begincodespacerange" =
          let (body, rest') = section (BC.pack "endcodespacerange") rest
              sp' = [ (BS.length lo, zip (BS.unpack lo) (BS.unpack hi))
                    | [OStr lo, OStr hi] <- chunks 2 body, BS.length lo == BS.length hi, BS.length lo >= 1, BS.length lo <= 4 ]
          in go rest' (reverse sp' ++ sp) rg nd v (l - length sp')
      | op == BC.pack "begincidrange" =
          let (body, rest') = section (BC.pack "endcidrange") rest
              (rg', l') = addRanges body rg l
          in go rest' sp rg' nd v l'
      | op == BC.pack "begincidchar" =
          let (body, rest') = section (BC.pack "endcidchar") rest
              items = [ [lo, lo, c] | [lo, c] <- chunks 2 body ]
              (rg', l') = addRanges (concat items) rg l
          in go rest' sp rg' nd v l'
      | op == BC.pack "beginnotdefrange" =
          let (body, rest') = section (BC.pack "endnotdefrange") rest
              (nd', l') = addRanges body nd l
          in go rest' sp rg nd' v l'
    go (_ : rest) sp rg nd v l = go rest sp rg nd v l
    section end ts =
      let (body, after) = break isEnd ts
          isEnd (TOp o) = o == end
          isEnd _ = False
      in ([ o | TObj o <- take 400000 body ], drop 1 after)
    addRanges body m l = foldl' add1 (m, l) (chunks 3 body)
    add1 (m, l) [OStr lo, OStr hi, ONum c]
      | l <= 0 = (m, l)
      | BS.length lo >= 1, BS.length lo <= 4, BS.length hi == BS.length lo =
          let n = BS.length lo
              a = beInt lo; z = beInt hi
          in if z < a then (m, l)
             else (IM.insert ((n `shiftL` 32) .|. a) (min z (a + 65535), truncate c) m, l - 1 - (min z (a + 65535) - a) `div` 256)
    add1 st _ = st
    chunks k xs = case splitAt k xs of
      (c, rest) | length c == k -> c : chunks k rest
      _ -> []

beInt :: BS.ByteString -> Int
beInt = BS.foldl' (\a c -> a * 256 + fromIntegral c) 0

-- | A string's codes and their byte lengths under a CMap's codespace.
splitCodes :: CMap -> BS.ByteString -> [(Int, Int)]
splitCodes cm s = go 0
  where
    n = BS.length s
    spaces = cmSpaces cm
    minLen = case spaces of [] -> 2; xs -> minimum (map fst xs)
    byte i = BSU.unsafeIndex s i
    matches k i = any (\(len, bnds) -> len == k && and [ lo <= byte (i + j) && byte (i + j) <= hi | (j, (lo, hi)) <- zip [0 ..] bnds ]) spaces
    go i
      | i >= n = []
      | otherwise =
          let lens = [ k | k <- [1 .. 4], i + k <= n, matches k i ]
              k = case lens of
                    (x : _) -> x
                    [] -> min (n - i) minLen
              code = beInt (BS.take k (BS.drop i s))
          in (code, k) : go (i + k)

-- | The CID of a code of a given byte length.
cmapCid :: CMap -> Int -> Int -> Int
cmapCid cm code n
  | cmIdentity cm && IM.null (cmRanges cm) = code
  | otherwise = case find' (cmRanges cm) of
      Just (c0, lo) -> c0 + code - lo
      Nothing -> case find' (cmNotdef cm) of
        Just (c0, _) -> c0
        Nothing -> if cmIdentity cm then code else 0
  where
    key = (n `shiftL` 32) .|. code
    find' m = look (8 :: Int) key
      where
        look 0 _ = Nothing
        look k kk = case IM.lookupLE kk m of
          Just (lo, (hi, c0))
            | lo `shiftR` 32 == n ->
                let lo' = lo .&. 0xffffffff
                in if code <= hi then Just (c0, lo') else look (k - 1) (lo - 1)
          _ -> Nothing

------------------------------------------------------------------------------
-- ToUnicode (first character of each mapping: enough to find a glyph)

toUnicodeOf :: PdfFile -> Fuel -> Dict -> (IM.IntMap Char, Fuel)
toUnicodeOf f fu d = case lookD f d "ToUnicode" of
  OStream sd raw -> case decodeStreamF f fu sd raw of
    (Right b, fu') -> let m = toUnicodeFirst (fuEntries fu') b
                      in (m, spendEntries (IM.size m) fu')
    (Left _, fu') -> (IM.empty, fu')
  _ -> (IM.empty, fu)

-- | The first character of every @bfchar@/@bfrange@ mapping, at most
-- @limit@ of them.
toUnicodeFirst :: Int -> BS.ByteString -> IM.IntMap Char
toUnicodeFirst limit0 b = go (contentTokens b) IM.empty limit
  where
    limit = max 0 (min limit0 1048576)
    go _ m l | l <= 0 = m
    go (TOp op : rest) m l
      | op == BC.pack "beginbfchar" =
          let (body, rest') = section (BC.pack "endbfchar") rest
              m' = foldl' (\acc p -> case p of
                             [OStr a, OStr t] | Just c <- utf16First t -> IM.insert (beInt a) c acc
                             _ -> acc) m (chunks 2 body)
          in go rest' m' (l - IM.size m' + IM.size m - 1)
      | op == BC.pack "beginbfrange" =
          let (body, rest') = section (BC.pack "endbfrange") rest
              (m', l') = foldl' range (m, l) (chunks 3 body)
          in go rest' m' l'
    go (_ : rest) m l = go rest m l
    go [] m _ = m
    range (m, l) [OStr a, OStr z, dst]
      | l <= 0 = (m, l)
      | otherwise =
          let lo = beInt a; hi = min (beInt z) (lo + 65535); k = max 0 (min l (hi - lo + 1))
          in case dst of
               OStr t | Just c <- utf16First t ->
                 (foldl' (\acc i -> IM.insert (lo + i) (bump c i) acc) m [0 .. k - 1], l - k)
               OArr ds -> (foldl' (\acc (i, v) -> case v of
                                      OStr t | Just c <- utf16First t -> IM.insert (lo + i) c acc
                                      _ -> acc) m (zip [0 .. k - 1] ds), l - k)
               _ -> (m, l)
    range st _ = st
    bump c i = let v = ord c + i in if v <= 0x10ffff && not (v >= 0xd800 && v <= 0xdfff) then chr v else '\xfffd'
    section end ts =
      let (body, after) = break isEnd ts
          isEnd (TOp o) = o == end
          isEnd _ = False
      in ([ o | TObj o <- take 400000 body ], drop 1 after)
    chunks k xs = case splitAt k xs of
      (c, rest) | length c == k -> c : chunks k rest
      _ -> []
    utf16First t = case BS.unpack (BS.take 4 t) of
      (a : bb : rest) ->
        let u = fromIntegral a * 256 + fromIntegral bb :: Int
        in if u >= 0xd800 && u <= 0xdbff
             then case rest of
               (c : e : _) -> let lo = fromIntegral c * 256 + fromIntegral e :: Int
                              in if lo >= 0xdc00 && lo <= 0xdfff
                                   then Just (chr (0x10000 + ((u - 0xd800) `shiftL` 10) + (lo - 0xdc00)))
                                   else Nothing
               _ -> Nothing
             else if u >= 0xdc00 && u <= 0xdfff then Nothing else Just (chr u)
      [a] -> Just (chr (fromIntegral a))
      _ -> Nothing
