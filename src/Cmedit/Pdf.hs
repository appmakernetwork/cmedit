{-# LANGUAGE BangPatterns #-}
-- | The PDF reading view: a page-description file shown as a document.
--
-- PDF is binary, so unlike the RTF view ("Cmedit.Rtf") this is not a
-- projection of a text buffer that Alt+T can toggle away from — there is no
-- buffer at all. It is structured like the image view instead: sniffed by
-- magic bytes, decoded from raw bytes into a read-only per-document model,
-- and never written back. Nothing here touches IO.
--
-- Three things make a text view of PDF tractable without a rasteriser:
--
--   * __Objects can be found without the cross-reference table.__ Every
--     indirect object in the file is introduced by @N G obj@, so a single
--     scan recovers the object graph. That is deliberately not "parse
--     @startxref@ and follow the chain": broken, patched and
--     linearised-then-appended files are common in the wild, and the scan
--     reads them all. Incremental updates fall out for free — a later
--     definition of an object number wins, which is exactly the semantics an
--     appended update has. Objects hidden inside compressed object streams
--     (@\/ObjStm@, universal in PDF 1.5+) are the one thing a scan cannot
--     see, so those are expanded afterwards, ordered by the same rule.
--
--   * __The content stream is a tiny stack machine.__ Of its ~70 operators
--     only the text ones and the two matrix ones carry text position, so the
--     interpreter here is small; everything else is skipped by arity-free
--     operand clearing, the same way an RTF reader skips markup it does not
--     model.
--
--   * __Reading order is recoverable, but only heuristically.__ A PDF has no
--     paragraphs, no lines and no reading order — just glyphs at coordinates.
--     'assemblePage' puts them back: columns from a vertical gap in the
--     x-occupancy, lines from y-clustering, spaces from the gap between one
--     glyph run's end and the next one's start measured in ems, and
--     paragraphs from lines that reach the right margin under a consistent
--     leading. These are judgement calls, and they are all in one place so
--     they can be tuned against real files.
--
-- What is deliberately absent: encrypted files (reported, not decrypted),
-- embedded images, vector graphics and any attempt at page fidelity. The
-- view reflows to the terminal width, which is the point — it is for reading
-- a document, not for reproducing its typesetting.
--
-- The object layer (lexer, object scan, filters, the page tree, the content
-- tokenizer, encodings, 'Fuel') lives in "Cmedit.PdfObj", shared with the
-- page renderer; this module is text extraction and the reading view. A
-- 'PdfDoc' keeps the parsed file and its page dictionaries ('pdSource') so a
-- page view can render the same document without parsing it again.
module Cmedit.Pdf
  ( -- * The document model
    PdfDoc(..)
  , PdfPage(..)
  , PdfPar(..)
  , PdfSpan(..)
  , PdfFmt(..)
  , PdfKind(..)
  , PdfAlign(..)
  , defaultPdfFmt
    -- * The parsed file (for a page renderer)
  , PdfSource(..)
  , pdfPageSource
    -- * Parsing
  , parsePdf
  , sniffPdf
  , maxPdfBytes
  , maxPdfPages
  , maxPdfDecoded
  , maxPdfTokens
  , parsePdfWithin
    -- * The view
  , pdfRelayout
  , pdfLines
  , pdfLineCount
  , pdfPageCount
  , pdfPageLine
    -- * Laid-out lines (what the renderer draws)
  , PdfLine(..)
  , layoutPdf
    -- * Selection (read-only: it exists to be copied, never edited)
  , pdfSelection
  , pdfSelText
  , pdfSelectRange
  , pdfSelectAll
  , pdfClearSel
  , pdfSetCaret
  , pdfExtendTo
  , pdfLineText
  , pdfClampPos
  , pdfPosAtCell
  , pdfCellOfPos
  , pdfWordRange
  , pdfLineRange
  , pdfCaretLeft
  , pdfCaretRight
  , pdfCaretUp
  , pdfCaretDown
  , pdfCaretHome
  , pdfCaretEnd
  , pdfCaretTop
  , pdfCaretBottom
  , pdfScrollToCaret
    -- * Movement (all clamped; the view is read-only)
  , pdfScroll
  , pdfGoTop
  , pdfGoBottom
  , pdfClamp
  , pdfGoToPage
  , pdfNextPage
  , pdfPrevPage
  , pdfCurrentPage
    -- * Presentation
  , pdfStatus
  , pdfPlainText
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, isDigit, isHexDigit, digitToInt, ord, toLower)
import Data.Foldable (toList)
import Data.List (foldl', sortOn, isInfixOf)
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Array.ST (newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, (!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M

import Cmedit.Geom (Affine(..), identityA, composeA, translateA)
import qualified Cmedit.IntervalMap as IV
import Cmedit.PdfObj
import Cmedit.Types (Pos(..), origin)
import Cmedit.Width (colToDisplay, displayToCol, lineDisplayWidth, wrapLine)

------------------------------------------------------------------------------
-- The document model

-- | Character formatting, as much of it as a terminal can show. Bold and
-- italic are inferred from the font (its name and descriptor flags), not
-- declared: PDF has no notion of "the bold version of this run", only a
-- different font resource.
data PdfFmt = PdfFmt
  { pfBold   :: !Bool
  , pfItalic :: !Bool
  , pfMono   :: !Bool
  , pfSize   :: !Double  -- ^ Effective font size in points, after the text and current transformation matrices.
  } deriving (Eq, Show)

defaultPdfFmt :: PdfFmt
defaultPdfFmt = PdfFmt False False False 0

data PdfAlign = PAlignLeft | PAlignCenter | PAlignRight
  deriving (Eq, Show)

-- | What a reconstructed block of text is.
data PdfKind
  = PKFlow    -- ^ Ordinary prose: re-wrapped to the terminal width.
  | PKFixed   -- ^ A line whose horizontal positions carry meaning (a table row, a code listing): placed by column, never re-wrapped.
  | PKBlank   -- ^ Vertical space between blocks.
  | PKRule    -- ^ A page boundary, drawn as a rule.
  deriving (Eq, Show)

-- | A run of text in one format. @psX@ is where it started on the page, as a
-- fraction of its column's width — meaningful for 'PKFixed' lines (which are
-- laid out by it) and for the first run of a paragraph (which sets the
-- indent); ignored otherwise.
data PdfSpan = PdfSpan
  { psX    :: !Double
  , psText :: !Text
  , psFmt  :: !PdfFmt
  } deriving (Eq, Show)

-- | One reconstructed block: a paragraph, a table row, a blank, or a rule.
data PdfPar = PdfPar
  { ppKind   :: !PdfKind
  , ppAlign  :: !PdfAlign
  , ppMargin :: !Double    -- ^ Left margin as a fraction of the column width.
  , ppFirst  :: !Double    -- ^ Extra first-line indent, as a fraction (negative for a hanging indent).
  , ppRuns   :: ![PdfSpan]
  } deriving (Eq, Show)

data PdfPage = PdfPage
  { pgNumber :: !Int
  , pgPars   :: ![PdfPar]
  } deriving (Eq, Show)

-- | One laid-out line. Same shape as "Cmedit.Rtf"'s 'Cmedit.Rtf.RtfLine' —
-- leading pad, text, and character ranges carrying the formatting — because
-- the renderer feeds both to the same @expandLineCellsFrom@ the plain text
-- view uses. @plPage@ is what the status bar and page navigation read.
data PdfLine = PdfLine
  { plPad   :: !Int
  , plText  :: !Text
  , plSpans :: ![(Int, Int, PdfFmt)]
  , plRule  :: !Bool
  , plPage  :: !Int
  } deriving (Eq, Show)

-- | The PDF view of a document: the extracted pages, the scroll position, and
-- the laid-out lines cached against the width they were laid out for.
--
-- Extraction happens once, when the file is opened (it is the expensive part:
-- inflating every content stream and interpreting it). Layout re-runs on a
-- resize only, and scrolling never re-lays out — the same bargain 'Cmedit.Rtf'
-- and the image view make.
data PdfDoc = PdfDoc
  { pdPages  :: !(Seq PdfPage)
  , pdTop    :: !Int    -- ^ First visible laid-out line.
  , pdCache  :: !(Maybe (Int, Int, Seq PdfLine, Seq Int))
      -- ^ (width, tab width, lines, first line index of each page).
  , pdCaret  :: !Pos
      -- ^ The end of the selection that moves, in laid-out (line, character)
      -- coordinates. Shown as the terminal cursor only while 'pdAnchor' is
      -- set: with no selection this view has no position worth pointing at.
  , pdAnchor :: !(Maybe Pos)
      -- ^ The fixed end of the selection; 'Nothing' when there is none.
      --
      -- Both are indices into the *laid-out* lines, so they mean nothing after
      -- a re-wrap — 'pdfRelayout' drops the selection when the width changes,
      -- which is also what a reader expects from reflowing the document.
  , pdTitle  :: !Text   -- ^ @\/Info \/Title@, when the file names one.
  , pdVer    :: !Text   -- ^ The @%PDF-1.x@ header version.
  , pdNote   :: !Text   -- ^ A warning to show on opening (truncation, no extractable text), or empty.
  , pdSource :: PdfSource
      -- ^ The parsed file and its pages, for a page renderer. Deliberately
      -- lazy: it is the very object graph extraction walked, so building it
      -- costs nothing, but it does keep the file's bytes alive (every stream
      -- is a slice of them) for as long as the document is open.
  } deriving (Show)

-- | A PDF's object graph and page dictionaries (inherited attributes merged
-- in, in reading order: what 'Cmedit.PdfObj.pdfPages' returns). Page @k@ of
-- the text view (@pgNumber k@) is @srcPages !! (k - 1)@; there may be more
-- page dictionaries than extracted pages when extraction stopped on a budget.
data PdfSource = PdfSource
  { srcFile  :: PdfFile
  , srcPages :: Seq Dict
  }

instance Show PdfSource where
  show _ = "PdfSource"

-- | The file and page dictionary of page @k@ (1-based, as 'pdfCurrentPage'
-- counts), for rendering it.
pdfPageSource :: PdfDoc -> Int -> Maybe (PdfFile, Dict)
pdfPageSource pd k = do
  let src = pdSource pd
  d <- Seq.lookup (k - 1) (srcPages src)
  Just (srcFile src, d)

------------------------------------------------------------------------------
-- Limits.
--
-- Every one of these bounds work that a *malformed or hostile* file could
-- otherwise make unbounded; none of them is reached by an ordinary document.

-- | Most characters extracted across the whole document.
maxPdfChars :: Int
maxPdfChars = 4 * 1024 * 1024

-- | Content-stream operators executed per page, and form-XObject nesting.
-- A form that draws itself would otherwise not terminate.
maxOpsPerPage, maxFormDepth :: Int
maxOpsPerPage = 400000
maxFormDepth  = 8

-- | Entries in one @\/ToUnicode@ CMap, and in one CID font's @\/W@ table.
-- A range is three short tokens that can stand for 65536 entries, so twenty
-- kilobytes could otherwise ask for hundreds of millions. No real font maps
-- more than a few tens of thousands.
maxCMapEntries :: Int
maxCMapEntries = 1048576

-- | Operands kept for one operator. Every operator this view implements reads
-- at most its first six, so the ones dropped are ones nothing looks at —
-- while a stream of operands with no operator would otherwise grow the list
-- without limit.
maxOperands :: Int
maxOperands = 64

-- | Characters one glyph may stand for through a @\/ToUnicode@ map. The
-- format's own limit is 512 bytes of UTF-16; without one a single code can
-- map to megabytes, and every glyph drawn with it repeats them.
maxGlyphChars :: Int
maxGlyphChars = 256

w2c :: Word8 -> Char
w2c = chr . fromIntegral

------------------------------------------------------------------------------
-- Fonts
--
-- Everything here exists to answer two questions per glyph code: what
-- character is this, and how wide is it. The first decides what the view
-- shows; the second decides where words break, since PDF does not record
-- spaces between positioned runs — they have to be inferred from the gap.

data FontInfo = FontInfo
  { fiBold    :: !Bool
  , fiItalic  :: !Bool
  , fiMono    :: !Bool
  , fiTwoByte :: !Bool                 -- ^ Composite font with 2-byte codes (Identity-H and friends).
  , fiToUni   :: !(IM.IntMap Text)     -- ^ From a @\/ToUnicode@ CMap: authoritative when present.
  , fiEnc     :: !(IM.IntMap Char)     -- ^ Simple-font encoding, base plus @\/Differences@.
  , fiWidths  :: !(IV.IntervalMap Double)
      -- ^ Glyph widths, in the units 'fiWScale' converts. Intervals: a CID
      -- font's @cFirst cLast w@ range is one entry, not one per CID.
  , fiDefW    :: !Double
  , fiWScale  :: !Double
      -- ^ What one unit of 'fiWidths' is worth in text space. A thousandth of
      -- an em for every font except Type3, whose widths are in its own glyph
      -- space and must go through its @\/FontMatrix@ — miss that and a
      -- Type3 document's advances are wrong by orders of magnitude, which
      -- shows up as text with every space missing (the gaps this view infers
      -- word breaks from all collapse to zero).
  }

emptyFont :: FontInfo
emptyFont = FontInfo False False False False IM.empty winAnsiMap IV.empty 500 0.001

-- | What the document as a whole may still spend ('Fuel', the budgets
-- "Cmedit.PdfObj" defines), and what it has already worked out and need not
-- work out again. Threaded through every page, so the limits it carries are
-- on the file and not on any one part of it.
--
-- The three caches are what make the budget safe for real documents rather
-- than only tight for hostile ones. A font is selected (@Tf@) hundreds of
-- times a page and its @\/ToUnicode@ map used to be inflated and parsed on
-- every one; a letterhead or watermark form is drawn (@Do@) on every page.
-- Charging those per use would spend an ordinary document's budget on
-- repetition, so each is charged once, when it is first read.
data XFuel = XFuel
  { xfFuel    :: !Fuel
  , fuFonts   :: !(IM.IntMap FontInfo)
      -- ^ Fonts already loaded, by object number.
  , fuCMaps   :: !(IM.IntMap (IM.IntMap Text))
      -- ^ @\/ToUnicode@ maps already parsed, by the stream's object number: a
      -- font written inline in a resource dictionary has no number of its
      -- own to be remembered by, but its CMap is a stream and always does.
  , fuPlain   :: !(IM.IntMap Int)
      -- ^ Form XObjects known to draw no text and no other form, by object
      -- number, with the operator count each adds. Running one again can
      -- change nothing but that count, so it is not run again.
  }

xfuel0 :: Int -> Int -> XFuel
xfuel0 bytes tokens = XFuel (fuel0 bytes tokens) IM.empty IM.empty IM.empty

-- | Change the budgets, keeping the caches.
onFuel :: (Fuel -> Fuel) -> XFuel -> XFuel
onFuel g x = x { xfFuel = g (xfFuel x) }

xBytes :: XFuel -> Int
xBytes = fuBytes . xfFuel

-- | Has the document spent any of its budgets?
xSpent :: XFuel -> Bool
xSpent = fuelSpent . xfFuel


-- | The font a resource dictionary's entry names, loading it if this is the
-- first time it has been asked for.
--
-- Only a font that is an object of its own can be remembered here; one
-- written inline in its resource dictionary is the caller's to remember (see
-- 'isFonts'). Either way a load is charged to the token budget by the size of
-- what it had to read, because the tables are the file's: a width array may
-- hold a million entries, and a three-number @\/W@ range stands for 65536.
fontFor :: PdfFile -> XFuel -> Obj -> (FontInfo, XFuel)
fontFor f fu o = case o of
  ORef r -> case IM.lookup r (fuFonts fu) of
    Just fi -> (fi, fu)
    Nothing -> let (fi, fu') = load
               in (fi, fu' { fuFonts = IM.insert r fi (fuFonts fu') })
  _ -> load
  where
    d = dictOf f o
    load = let (fi, fu') = loadFont f fu d
               cost = objWeight f (ODict d) + IV.coveredCount (fiWidths fi)
           in (fi, onFuel (spendTokens cost) fu')

-- | Roughly how many values an object holds, following references a few
-- levels (a font's @\/Widths@ and @\/Encoding@ are usually objects of their
-- own). Counting stops at a ceiling; this is a price, not a measurement.
objWeight :: PdfFile -> Obj -> Int
objWeight f o0 = ceiling' - go (3 :: Int) ceiling' o0
  where
    ceiling' = 1048576 :: Int
    -- Returns what is left of the allowance after counting @o@.
    go :: Int -> Int -> Obj -> Int
    go _ left _ | left <= 0 = 0
    go hops left o = case o of
      ODict m  -> M.foldl' (go hops) (left - 1) m
      OArr xs  -> foldl' (go hops) (left - 1) xs
      ORef r | hops > 0 -> go (hops - 1) (left - 1) (IM.findWithDefault ONull r (pfObjs f))
      _        -> left - 1

loadFont :: PdfFile -> XFuel -> Dict -> (FontInfo, XFuel)
loadFont f fu0 d = (FontInfo
  { fiBold = bold, fiItalic = italic, fiMono = mono
  , fiTwoByte = twoByte
  , fiToUni = toUni
  , fiEnc = encMap
  , fiWidths = widths
  , fiDefW = defW
  , fiWScale = wScale
  }, onFuel (spendTokens (2 * wWork)) fu1)
  where
    subtype = nameOf (lookD f d "Subtype")
    isType3 = subtype == BC.pack "Type3"
    wScale
      | isType3 = case map (numOf f . Just) (arrOf f (lookD f d "FontMatrix")) of
                    (a : _) | a /= 0 -> a
                    _ -> 0.001
      | otherwise = 0.001
    baseFont = BC.unpack (nameOf (lookD f d "BaseFont"))
    -- A subset-embedded font is named "ABCDEF+Real-Name"; the tag says
    -- nothing about the face.
    faceName = map toLower (drop 1 (dropWhile (/= '+') baseFont ++ "+" ++ baseFont))
    isType0 = subtype == BC.pack "Type0"
    descendant = case arrOf f (lookD f d "DescendantFonts") of
                   (x : _) -> dictOf f x
                   []      -> M.empty
    fdSource = if isType0 then descendant else d
    descr = dictOf f (lookD f fdSource "FontDescriptor")
    flags = intOf f (M.lookup (BC.pack "Flags") descr)
    italicAngle = numOf f (M.lookup (BC.pack "ItalicAngle") descr)
    stemV = numOf f (M.lookup (BC.pack "StemV") descr)
    has s = s `isInfixOf` faceName
    bold = has "bold" || has "black" || has "heavy" || has "semib"
             || (flags .&. 0x40000) /= 0 || stemV >= 120
    italic = has "italic" || has "oblique" || (flags .&. 0x40) /= 0 || italicAngle /= 0
    mono = has "mono" || has "courier" || has "consol" || (flags .&. 0x1) /= 0

    (toUni, fu1) = case (M.lookup (BC.pack "ToUnicode") d, lookD f d "ToUnicode") of
      (Just (ORef r), _) | Just m <- IM.lookup r (fuCMaps fu0) -> (m, fu0)
      (ref, OStream sd raw) ->
        let (res, spent) = decodeStream f (xBytes fu0) sd raw
            m = either (const IM.empty) (parseToUnicode (fuEntries (xfFuel fu0))) res
            fu = onFuel (spendEntries (IM.size m) . spendBytes spent) fu0
        in case ref of
             Just (ORef r) -> (m, fu { fuCMaps = IM.insert r m (fuCMaps fu) })
             _             -> (m, fu)
      _ -> (IM.empty, fu0)

    -- Composite fonts are 2-byte in every encoding this view meets; a
    -- one-byte CMap is legal but essentially unused outside CJK legacy files.
    twoByte = isType0

    -- Simple-font encoding: a named base, or a dictionary carrying a base and
    -- a /Differences list that renames individual codes.
    encMap
      | isType0 = IM.empty
      | otherwise = applyDifferences f (lookD f d "Encoding") baseMap
    baseMap = case lookD f d "Encoding" of
      OName n -> namedEncoding (BC.unpack n)
      o@(ODict _) -> case lookD f (dictOf f o) "BaseEncoding" of
                       OName n -> namedEncoding (BC.unpack n)
                       _       -> defaultBase
      _ -> defaultBase
    -- Symbolic fonts with no encoding of their own are anyone's guess; the
    -- WinAnsi assumption is right far more often than it is wrong, and a
    -- /ToUnicode map (which most such fonts carry) overrides it anyway.
    defaultBase = winAnsiMap

    (widths, wWork)
      | isType0 = cidWidths f (lookD f descendant "W")
      | otherwise = (simpleWidths, 0)
    simpleWidths =
          let first = intOf f (M.lookup (BC.pack "FirstChar") d)
              ws = [ numOf f (Just o) | o <- arrOf f (lookD f d "Widths") ]
          in if null ws
               -- Only the standard 14 have metrics to fall back on; a Type3
               -- font's glyphs are procedures we do not run, so an unlisted
               -- code simply does not advance.
               then (if isType3 then IV.empty else IV.fromKeys (IM.toList (builtinWidths faceName)))
               else IV.fromKeys (zip [first ..] ws)
    defW
      | isType0 = case lookD f descendant "DW" of
                    ONum v | v > 0 -> v
                    _              -> 1000
      | isType3 = 0
      | otherwise = case lookD f descr "MissingWidth" of
                      ONum v | v > 0 -> v
                      _ | mono       -> 600
                        | otherwise  -> 500

-- | @\/W@ for a CID font: @[c [w …] cFirst cLast w …]@, mixing both forms.
--
-- Budgeted like a CMap and for the same reason: a range is three numbers that
-- stand for up to 65536 entries, and nothing stops a file repeating one.
--
-- The second result is how many entries were written, which is the work done
-- and not the size of the result: a repeated range rewrites the same keys.
cidWidths :: PdfFile -> Obj -> (IV.IntervalMap Double, Int)
cidWidths f o = go maxCMapEntries (arrOf f o) IV.empty
  where
    -- The budget is still spent per CID covered, exactly as when every CID
    -- was its own map entry (the extractor's output, and when it stops,
    -- are unchanged); only the storage is per range.
    go :: Int -> [Obj] -> IV.IntervalMap Double -> (IV.IntervalMap Double, Int)
    go left _ acc | left <= 0 = (acc, maxCMapEntries)
    go left (a : rest) acc = case (resolve f a, rest) of
      (ONum c, (b : more)) -> case resolve f b of
        OArr ws ->
          let ws' = take left ws
          in go (left - length ws') more
                (foldl' (\m (i, v) -> IV.insertKey i (numOf f (Just v)) m)
                        acc (zip [truncate c ..] ws'))
        ONum c2 -> case more of
          (wv : more') ->
            let lo = truncate c :: Int
                hi = min (truncate c2) (lo + 65535)
                taken = min left (max 0 (hi - lo + 1))
            in go (left - max 1 taken) more'
                  (if taken > 0 then IV.insertRange lo (lo + taken - 1) (numOf f (Just wv)) acc else acc)
          [] -> (acc, maxCMapEntries - left)
        _ -> go (left - 1) more acc
      _ -> (acc, maxCMapEntries - left)
    go left [] acc = (acc, maxCMapEntries - left)

-- | Parse a @\/ToUnicode@ CMap: the @bfchar@ and @bfrange@ sections, which map
-- glyph codes to UTF-16BE strings. The rest of the CMap language (codespace
-- ranges, usecmap) is not needed to read one.
--
-- @limit@ is how many entries the document may still have ('maxPdfCMapEntries'
-- less what its other fonts took); one map is held to 'maxCMapEntries' too.
parseToUnicode :: Int -> BS.ByteString -> IM.IntMap Text
parseToUnicode limit bs
  | IM.size whole <= limit = whole
  | otherwise = IM.fromDistinctAscList (take (max 0 limit) (IM.toAscList whole))
  where
    whole = IM.union (chars) (ranges)
    chars = sections "beginbfchar" "endbfchar" bfChar
    ranges = sections "beginbfrange" "endbfrange" bfRange
    -- Budgeted across every section: see 'maxCMapEntries'.
    sections open close h = fst (foldl' add (IM.empty, max 0 (min limit maxCMapEntries)) (slices open close))
      where add (m, left) body
              | left <= 0 = (m, left)
              | otherwise = let m' = IM.fromList (take left (h body))
                            in (IM.union m m', left - IM.size m')
    slices open close = go 0
      where
        go i =
          let hay = BS.drop i bs
              (pre, post) = BS.breakSubstring (BC.pack open) hay
          in if BS.null post then []
             else
               let s = i + BS.length pre + length open
                   (body, after) = BS.breakSubstring (BC.pack close) (BS.drop s bs)
               in body : (if BS.null after then [] else go (s + BS.length body + length close))
    bfChar body =
      [ (codeOf a, utf16 b) | [a, b] <- pairsOf (objsIn body) ]
    bfRange body = concatMap one (triplesOf (objsIn body))
      where
        one [OStr a, OStr b, OStr dst] =
          let lo = beInt a; hi = min (beInt b) (lo + 65535)
              base = utf16 (OStr dst)
          in [ (c, bumpLast base (c - lo)) | c <- [lo .. hi] ]
        one [OStr a, OStr _, OArr ds] =
          [ (beInt a + k, utf16 v) | (k, v) <- zip [0 ..] ds ]
        one _ = []
    codeOf o = case o of OStr s -> beInt s; ONum v -> truncate v; _ -> 0
    beInt = BS.foldl' (\a c -> a * 256 + fromIntegral c) 0
    -- A bfrange's destination increments in its *last* code unit.
    bumpLast t k
      | T.null t = t
      | otherwise = T.init t `T.snoc` safeChr (fromEnum (T.last t) + k)
    safeChr n = if n >= 0 && n <= 0x10ffff && not (n >= 0xd800 && n <= 0xdfff)
                  then chr n else '\xfffd'
    -- 'maxGlyphChars': two bytes a character, four for a surrogate pair.
    utf16 (OStr s) = T.take maxGlyphChars (decodeUtf16BE (BS.take (4 * maxGlyphChars) s))
    utf16 _ = T.empty
    objsIn body = unfoldObjs body 0
    unfoldObjs b i = case parseObj b i of
      Just (o, j) | j > i -> o : unfoldObjs b j
      _ -> []
    pairsOf (a : b : r) = [a, b] : pairsOf r
    pairsOf _ = []
    triplesOf (a : b : c : r) = [a, b, c] : triplesOf r
    triplesOf _ = []

decodeUtf16BE :: BS.ByteString -> Text
decodeUtf16BE s = T.pack (go (BS.unpack s))
  where
    go (a : b : rest) =
      let u = fromIntegral a * 256 + fromIntegral b :: Int
      in if u >= 0xd800 && u <= 0xdbff
           then case rest of
             (c : d : rest') ->
               let lo = fromIntegral c * 256 + fromIntegral d :: Int
               in if lo >= 0xdc00 && lo <= 0xdfff
                    then chr (0x10000 + ((u - 0xd800) `shiftL` 10) + (lo - 0xdc00)) : go rest'
                    else go rest'
             _ -> []
           else if u >= 0xdc00 && u <= 0xdfff then go rest
           else chr u : go rest
    go _ = []


-- | Approximate metrics for the standard 14 fonts, which a file may use
-- without supplying @\/Widths@ at all. Getting these roughly right matters
-- more than it sounds: word breaks in this view are inferred from the gap
-- between one run's computed end and the next run's start, so a font whose
-- advance is guessed badly produces text with the spaces in the wrong places.
builtinWidths :: String -> IM.IntMap Double
builtinWidths face
  | "courier" `isInfixOf` face = IM.fromList [ (i, 600) | i <- [32 .. 255] ]
  | "times" `isInfixOf` face || "roman" `isInfixOf` face || "serif" `isInfixOf` face =
      IM.fromList (zip [32 ..] timesWidths)
  | otherwise = IM.fromList (zip [32 ..] helvWidths)

-- Advance widths for ASCII 32..126, in 1/1000 em (Adobe's AFM values).
helvWidths :: [Double]
helvWidths =
  [ 278,278,355,556,556,889,667,191,333,333,389,584,278,333,278,278
  , 556,556,556,556,556,556,556,556,556,556,278,278,584,584,584,556
  , 1015,667,667,722,722,667,611,778,722,278,500,667,556,833,722,778
  , 667,778,722,667,611,722,667,944,667,667,611,278,278,278,469,556
  , 333,556,556,500,556,556,278,556,556,222,222,500,222,833,556,556
  , 556,556,333,500,278,556,500,722,500,500,500,334,260,334,584 ]

timesWidths :: [Double]
timesWidths =
  [ 250,333,408,500,500,833,778,180,333,333,500,564,250,333,250,278
  , 500,500,500,500,500,500,500,500,500,500,278,278,564,564,564,444
  , 921,722,667,667,722,611,556,722,722,333,389,722,611,889,722,722
  , 556,722,667,556,611,722,722,944,722,722,611,333,278,333,469,500
  , 333,444,500,444,500,444,333,500,500,278,278,500,278,778,500,500
  , 500,500,333,389,278,500,500,722,500,500,444,480,200,480,541 ]

-- | Decode one PDF string into glyph codes paired with the text they draw.
decodeGlyphs :: FontInfo -> BS.ByteString -> [(Int, Text)]
decodeGlyphs fi s
  | fiTwoByte fi = map two (pairsOf (BS.unpack s))
  | otherwise = map one (BS.unpack s)
  where
    pairsOf (a : b : r) = (fromIntegral a * 256 + fromIntegral b) : pairsOf r
    pairsOf [a] = [fromIntegral a * 256]
    pairsOf [] = []
    two c = (c, expandLigatures (glyphText c))
    one b = let c = fromIntegral b in (c, expandLigatures (glyphText c))
    glyphText c = case IM.lookup c (fiToUni fi) of
      Just t  -> t
      Nothing -> case IM.lookup c (fiEnc fi) of
        Just ch -> T.singleton ch
        -- A composite font with no /ToUnicode is unreadable by construction
        -- (its codes are glyph indices in an embedded font programme); an
        -- unmapped simple code below 256 is far more likely to be Latin-1.
        Nothing | fiTwoByte fi -> T.empty
                | c >= 32 && c < 256 -> T.singleton (chr c)
                | otherwise -> T.empty

glyphWidth :: FontInfo -> Int -> Double
glyphWidth fi c = fromMaybe (fiDefW fi) (IV.lookupKey c (fiWidths fi))

-- | Spell the Latin typographic ligatures out.
--
-- A typeset document is full of them and they are what a @\/ToUnicode@ map
-- honestly reports, but they are also outside every terminal font's usual
-- coverage — leaving them in means a reader sees @rst@ with a hole in it
-- rather than @first@. The letters are what the document says; the ligature
-- was a typesetting decision for a page width this view is not using anyway.
expandLigatures :: Text -> Text
expandLigatures t
  | T.all (< '\xfb00') t = t
  | otherwise = T.concatMap one t
  where
    one c = case c of
      '\xfb00' -> "ff"
      '\xfb01' -> "fi"
      '\xfb02' -> "fl"
      '\xfb03' -> "ffi"
      '\xfb04' -> "ffl"
      '\xfb05' -> "st"
      '\xfb06' -> "st"
      _        -> T.singleton c

------------------------------------------------------------------------------
-- The content-stream interpreter

-- | @[a b c d e f]@, the affine matrix PDF writes as a six-element array.
type Mat = Affine

identityMat :: Mat
identityMat = identityA

-- | @matMul m n@ is @m@ then @n@: PDF's row-vector product @m × n@, which
-- is why @cm@ reads @matMul new ctm@. (The same floating-point operations in
-- the same order as the tuple version this view had before the split.)
matMul :: Mat -> Mat -> Mat
matMul m n = composeA n m


-- | A positioned run of text.
--
-- Both ends are kept, not a start and a width, because the advance direction
-- is whatever the text matrix says: a landscape page usually draws its text
-- rotated, and a run that advances along @y@ has no horizontal extent at all
-- to measure. 'rotateRun' maps both points into display space and normalises
-- them; everything downstream reads @rX0 <= rX1@ along the line and @rY@
-- growing downward.
data Run = Run
  { rX0   :: !Double
  , rY    :: !Double
  , rX1   :: !Double
  , rY1   :: !Double
  , rSize :: !Double
  , rTxt  :: !Text
  , rFmt  :: !PdfFmt
  }

data GState = GState
  { gCTM    :: !Mat
  , gFont   :: !(Maybe FontInfo)
  , gSize   :: !Double
  , gChar   :: !Double
  , gWord   :: !Double
  , gHScale :: !Double
  , gLead   :: !Double
  , gRise   :: !Double
  }

gInit :: GState
gInit = GState identityMat Nothing 0 0 0 1 0 0

data IState = IState
  { isG     :: !GState
  , isStack :: ![GState]
  , isTm    :: !Mat
  , isTlm   :: !Mat
  , isRuns  :: ![Run]        -- ^ Reversed.
  , isOps   :: !Int
  , isChars :: !Int
  , isFuel  :: !XFuel      -- ^ The document's budgets and caches, passing through.
  , isFonts :: !(M.Map BS.ByteString FontInfo)
      -- ^ Inline fonts of the resource dictionary in force, by name: they
      -- have no object number for 'fuFonts' to know them by, and a page
      -- selects the same one hundreds of times.
  }


-- | Run a page's content, collecting positioned text runs, and hand back
-- what the document has left to spend.
runContent :: PdfFile -> XFuel -> Dict -> BS.ByteString -> ([Run], XFuel)
runContent f fuIn res0 dat = (reverse (isRuns end), isFuel end)
  where
    end = exec 0 res0 (contentTokens dat) st0 0 []
    st0 = IState gInit [] identityMat identityMat [] 0 0 fuIn M.empty

    -- @k@ is how many operands the pending operator has been given, which is
    -- not the length of @ops@: only the first 'maxOperands' are kept.
    exec :: Int -> Dict -> [Tok] -> IState -> Int -> [Obj] -> IState
    exec _ _ [] st _ _ = st
    -- ('contentTokens' never produces an inline image; it skips them.)
    exec depth res (TInline _ _ : ts) st !k ops = exec depth res ts st k ops
    exec depth res (TObj o : ts) st !k ops
      | k < maxOperands = exec depth res ts st (k + 1) (o : ops)
      -- Operands nothing will read. They are still charged, a block at a
      -- time, or a stream of them with no operator would cost nothing.
      | k .&. 4095 /= 0 = exec depth res ts st (k + 1) ops
      | xSpent (isFuel st) = st
      | otherwise =
          let fu = isFuel st
          in exec depth res ts st { isFuel = onFuel (spendTokens 4096) fu } (k + 1) ops
    exec depth res (TOp op : ts) st !k ops
      | isOps st > maxOpsPerPage || isChars st > maxPdfChars || xSpent fu = st
      | otherwise =
          let st1 = st { isOps = isOps st + 1
                       , isFuel = onFuel (\x -> x { fuTokens = fuTokens x - min k maxOperands - weight }) fu }
              st' = apply depth res (BC.unpack op) (reverse ops) st1
          in exec depth res ts st' 0 []
      where
        fu = isFuel st
        -- What an operator costs beyond being a token. Selecting a font and
        -- drawing a form each do several dictionary lookups, and a page can
        -- be nothing but 400 000 of either.
        weight | op == BC.pack "Do" = 32
               | op == BC.pack "Tf" = 8
               | otherwise          = 1

    num (ONum v) = v
    num _ = 0

    apply depth res op args st = case op of
      "q"  -> st { isStack = isG st : isStack st }
      "Q"  -> case isStack st of
                (g : gs) -> st { isG = g, isStack = gs }
                []       -> st
      "cm" | [a, b, c, d, e, ff] <- map num (take 6 args) ->
               st { isG = g { gCTM = matMul (Affine a b c d e ff) (gCTM g) } }
      "BT" -> st { isTm = identityMat, isTlm = identityMat }
      "ET" -> st
      "Tf" -> case args of
        [OName fn, sz] ->
          let (fi, st') = fontNamed res fn st
          in st' { isG = g { gFont = Just fi, gSize = num sz } }
        _ -> st
      "Td" | [tx, ty] <- map num (take 2 args) -> setLine (matMul (translateA tx ty) (isTlm st)) st
      "TD" | [tx, ty] <- map num (take 2 args) ->
               setLine (matMul (translateA tx ty) (isTlm st)) st { isG = g { gLead = -ty } }
      "Tm" | [a, b, c, d, e, ff] <- map num (take 6 args) -> setLine (Affine a b c d e ff) st
      "T*" -> nextLine st
      "TL" | (v : _) <- args -> st { isG = g { gLead = num v } }
      "Tc" | (v : _) <- args -> st { isG = g { gChar = num v } }
      "Tw" | (v : _) <- args -> st { isG = g { gWord = num v } }
      "Tz" | (v : _) <- args -> st { isG = g { gHScale = num v / 100 } }
      "Ts" | (v : _) <- args -> st { isG = g { gRise = num v } }
      "Tj" | (OStr s : _) <- args -> showStr s st
      "'"  | (OStr s : _) <- args -> showStr s (nextLine st)
      "\"" | [aw, ac, OStr s] <- args ->
               showStr s (nextLine st { isG = g { gWord = num aw, gChar = num ac } })
      "TJ" | (OArr xs : _) <- args -> foldl' tjItem st xs
      "Do" | (OName xn : _) <- args -> doXObject depth res xn st
      _ -> st
      where g = isG st

    setLine m st = st { isTm = m, isTlm = m }
    nextLine st = setLine (matMul (translateA 0 (negate (gLead (isG st)))) (isTlm st)) st

    tjItem st (OStr s) = showStr s st
    tjItem st (ONum v) =
      let gs = isG st
          tx = negate v / 1000 * gSize gs * gHScale gs
      in st { isTm = matMul (translateA tx 0) (isTm st) }
    tjItem st _ = st

    -- Draw a string: one run, positioned by where the text matrix starts and
    -- ends. The advance is the sum of each glyph's width plus the character
    -- and word spacing, all scaled by the size and the horizontal scale —
    -- exactly the formula the spec gives, and the reason this view can tell
    -- an inter-word gap from a kerning adjustment.
    showStr s st = case gFont (isG st) of
      Nothing -> st
      Just fi ->
        let gs = isG st
            glyphs = capGlyphs (decodeGlyphs fi s)
            txt = T.concat (map snd glyphs)
            adv = sum [ (glyphWidth fi c * fiWScale fi * gSize gs
                          + gChar gs
                          + (if c == 32 && not (fiTwoByte fi) then gWord gs else 0))
                        * gHScale gs
                      | (c, _) <- glyphs ]
            base = matMul (isTm st) (gCTM gs)
            Affine a b c' d e ff = base
            scaleY = sqrt (abs (c' * c' + d * d))
            scaleX = sqrt (abs (a * a + b * b))
            x0 = e; y0 = ff
            tm' = matMul (translateA adv 0) (isTm st)
            Affine _ _ _ _ e2 f2 = matMul tm' (gCTM gs)
            fmt = PdfFmt (fiBold fi) (fiItalic fi) (fiMono fi) (gSize gs * scaleY)
            run = Run x0 y0 e2 f2 (max 0.01 (gSize gs * scaleY)) txt fmt
        in if T.null txt || T.all (== ' ') txt || scaleX <= 0
             then st { isTm = tm' }
             else st { isTm = tm'
                     , isRuns = run : isRuns st
                     , isChars = isChars st + T.length txt }

    -- One string's glyphs, up to the point where they alone would fill the
    -- document's character budget: the page stops after an operator that
    -- crosses it, but the operator itself has to be finite first. A glyph
    -- that draws nothing still counts one, so a string of them is bounded
    -- too.
    capGlyphs = go (0 :: Int)
      where
        go !n (gl@(_, t) : gs) | n <= maxPdfChars = gl : go (n + max 1 (T.length t)) gs
        go _ _ = []

    -- A form XObject is a nested content stream with its own matrix and
    -- resources; images and anything else are skipped.
    doXObject depth res xn st
      | depth >= maxFormDepth = st
      | otherwise =
          let xobjs = dictOf f (lookD f res "XObject")
              entry = M.findWithDefault ONull xn xobjs
              key = case entry of ORef r -> Just r; _ -> Nothing
              fu = isFuel st
              known = key >>= \r -> IM.lookup r (fuPlain fu)
          in case known of
               -- Seen before, and it draws no text: all it can do is count.
               Just nOps | nOps >= 0 -> st { isOps = isOps st + nOps }
               _ -> case resolve f entry of
                 OStream sd raw
                   | nameOf (lookD f sd "Subtype") == BC.pack "Form" ->
                       case decodeStream f (xBytes fu) sd raw of
                         (Left _, spent) -> st { isFuel = onFuel (spendBytes spent) fu }
                         (Right body, spent) ->
                           runForm depth res key (known == Nothing) sd body
                                   st { isFuel = onFuel (spendBytes spent) fu }
                 _ -> st

    runForm depth res key unseen sd body st
      -- First sight of this form: if nothing in it shows text or draws
      -- another form, running it changes only the operator count, so that is
      -- recorded and the form is never decoded again. Otherwise that is
      -- recorded instead (as -1), so the question is asked once.
      | unseen, Just r <- key = case plainOps (contentTokens body) 0 0 of
          Just (nOps, nToks) ->
            st { isOps = isOps st + nOps
               , isFuel = (onFuel (spendTokens nToks) fu)
                             { fuPlain = IM.insert r nOps (fuPlain fu) } }
          Nothing -> run st { isFuel = fu { fuPlain = IM.insert r (-1) (fuPlain fu) } }
      | otherwise = run st
      where
        fu = isFuel st
        run st1 =
          let mtx = case map (numOf f . Just) (arrOf f (lookD f sd "Matrix")) of
                      [a, b, c, d, e, ff] -> Affine a b c d e ff
                      _ -> identityMat
              (inner, own) = case lookD f sd "Resources" of
                        ODict rd -> (rd, True)
                        _        -> (res, False)
              stIn = st1 { isG = (isG st1) { gCTM = matMul mtx (gCTM (isG st1)) }
                         , isStack = []
                         -- its own resources are its own names
                         , isFonts = if own then M.empty else isFonts st1 }
              stOut = exec (depth + 1) inner (contentTokens body) stIn 0 []
          in st1 { isRuns = isRuns stOut
                 , isOps = isOps stOut
                 , isChars = isChars stOut
                 , isFuel = isFuel stOut }

    -- The operators and tokens in a stream that shows no text and draws no
    -- form; 'Nothing' at the first operator that does either.
    plainOps :: [Tok] -> Int -> Int -> Maybe (Int, Int)
    plainOps [] !nOps !nToks = Just (nOps, nToks)
    plainOps (TInline _ _ : ts) !nOps !nToks = plainOps ts nOps nToks
    plainOps (TObj _ : ts) !nOps !nToks = plainOps ts nOps (nToks + 1)
    plainOps (TOp op : ts) !nOps !nToks
      | op `elem` drawing = Nothing
      | otherwise = plainOps ts (nOps + 1) (nToks + 1)
    drawing = map BC.pack ["Tj", "TJ", "'", "\"", "Do"]

    fontNamed res fn st =
      let fonts = dictOf f (lookD f res "Font")
      in case M.lookup fn fonts of
           Just o@(ORef _) -> let (fi, fu') = fontFor f (isFuel st) o
                              in (fi, st { isFuel = fu' })
           Just o -> case M.lookup fn (isFonts st) of
             Just fi -> (fi, st)
             Nothing -> let (fi, fu') = fontFor f (isFuel st) o
                        in (fi, st { isFuel = fu', isFonts = M.insert fn fi (isFonts st) })
           Nothing -> (emptyFont, st)

------------------------------------------------------------------------------
-- Putting the page back together
--
-- This is the heuristic half. A PDF page is a bag of positioned glyph runs
-- with no lines, no paragraphs, no columns and no reading order; everything
-- below is about recovering those, and every threshold is expressed in ems so
-- it holds at any point size.

-- | One reconstructed line, before it becomes a paragraph.
data PLine = PLine
  { lY     :: !Double
  , lX0    :: !Double
  , lX1    :: !Double
  , lSize  :: !Double
  , lSpans :: ![PdfSpan]     -- ^ @psX@ still absolute at this stage.
  , lFixed :: !Bool          -- ^ Wide internal gaps: a table row, not prose.
  }

-- | Turn a page's runs into blocks, given the page box and its rotation.
assemblePage :: (Double, Double, Double, Double) -> Int -> [Run] -> [PdfPar]
assemblePage box rot runs0
  | null runs = []
  | otherwise = concatMap column columns
  where
    runs = map (rotateRun box rot) (filter (not . T.null . rTxt) runs0)
    (pw, ph) = pageSize box rot
    columns = splitColumns pw runs
    column rs =
      let ls = buildLines rs
          x0 = minimum (map lX0 ls)
          x1 = maximum (map lX1 ls)
          cw = max 1 (x1 - x0)
      in groupPars x0 cw (sortOn lY ls)
    _unused = ph

-- | Page width and height after rotation.
pageSize :: (Double, Double, Double, Double) -> Int -> (Double, Double)
pageSize (bx0, by0, bx1, by1) rot
  | rot == 90 || rot == 270 = (h, w)
  | otherwise = (w, h)
  where w = abs (bx1 - bx0); h = abs (by1 - by0)

-- | Move a run into a top-left origin, undoing @\/Rotate@ so a landscape page
-- reads in the right order. @rY@ afterwards grows downward.
--
-- Both endpoints go through the same point transform and are normalised
-- afterwards, so a run that advanced along @y@ in page space comes out with
-- the horizontal extent the assembly needs — see 'Run'.
rotateRun :: (Double, Double, Double, Double) -> Int -> Run -> Run
rotateRun (bx0, by0, bx1, by1) rot r =
  r { rX0 = min sx ex, rX1 = max sx ex, rY = sy, rY1 = ey }
  where
    w = abs (bx1 - bx0); h = abs (by1 - by0)
    (sx, sy) = place (rX0 r) (rY r)
    (ex, ey) = place (rX1 r) (rY1 r)
    -- Unrotated display coordinates: x from the left edge, y down from the
    -- top. /Rotate then turns that picture clockwise.
    place x y = let dx = x - bx0; dy = by1 - y in case rot of
      90  -> (h - dy, dx)
      180 -> (w - dx, h - dy)
      270 -> (dy, w - dx)
      _   -> (dx, dy)

-- | Split a page into columns at a vertical band no text crosses.
--
-- Runs wider than 60% of the page are left out of the occupancy test and
-- placed afterwards by their centre: a title spanning both columns would
-- otherwise fill the gutter and hide it, which is the failure mode that makes
-- naive column detection useless on real papers.
splitColumns :: Double -> [Run] -> [[Run]]
splitColumns pw runs = go 0 runs
  where
    nbins = 100 :: Int
    go :: Int -> [Run] -> [[Run]]
    go depth rs
      | depth >= 2 || length rs < 12 = [rs]
      | otherwise = case gapAt rs of
          Nothing -> [rs]
          Just cut ->
            let left = [ r | r <- rs, mid r < cut ]
                right = [ r | r <- rs, mid r >= cut ]
            in if length left < 5 || length right < 5
                 || length left * 5 < length rs || length right * 5 < length rs
                 then [rs]
                 else go (depth + 1) left ++ go (depth + 1) right
    mid r = (rX0 r + rX1 r) / 2
    gapAt rs =
      let narrow = [ r | r <- rs, rX1 r - rX0 r < 0.6 * pw ]
          xs = concatMap (\r -> [rX0 r, rX1 r]) narrow
      in if length narrow < 10 then Nothing else
        let lo = minimum xs; hi = maximum xs
            span' = max 1 (hi - lo)
            bin v = max 0 (min (nbins - 1) (floor ((v - lo) / span' * fromIntegral nbins)))
            filled = foldl' (\m r -> foldl' (flip (`IM.insert` True)) m
                                            [bin (rX0 r) .. bin (rX1 r)])
                            IM.empty narrow
            empties = [ i | i <- [0 .. nbins - 1], not (IM.member i filled) ]
            runsOf = groupRuns empties
            wide = [ g | g <- runsOf, length g >= 5
                   , head g > 8, last g < nbins - 9 ]
        in case wide of
             [] -> Nothing
             gs -> let g = last (sortOn length gs)
                       centre = fromIntegral (head g + last g) / 2
                   in Just (lo + centre / fromIntegral nbins * span')
    groupRuns [] = []
    groupRuns (x : xs) = let (a, b) = span2 (x + 1) xs in (x : a) : groupRuns b
      where span2 _ [] = ([], [])
            span2 k (y : ys) | y == k = let (p, q) = span2 (k + 1) ys in (y : p, q)
                             | otherwise = ([], y : ys)

-- | Cluster runs into lines by their baseline, then order each line by x and
-- join it into spans, inserting the spaces PDF does not record.
buildLines :: [Run] -> [PLine]
buildLines rs = map mkLine (cluster (sortOn rY rs))
  where
    cluster [] = []
    cluster (r : rest) = go [r] (rY r) (rSize r) rest
      where
        go acc y sz (x : xs)
          -- A baseline within about half an em of the running one belongs to
          -- the same line: superscripts, subscripts, inline maths and the
          -- separately-drawn halves of an accented glyph all sit off it.
          -- Comfortably under a line of leading either way, which is what
          -- stops this from swallowing the next line.
          | abs (rY x - y) <= max 0.5 (0.45 * sz) = go (x : acc) y (max sz (rSize x)) xs
          | otherwise = reverse acc : cluster (x : xs)
        go acc _ _ [] = [reverse acc]
    mkLine grp =
      let srt = sortOn rX0 grp
          sz = maximum (map rSize srt)
          (spans, fixed) = joinRuns sz srt
      in PLine { lY = minimum (map rY srt)
               , lX0 = minimum (map rX0 srt)
               , lX1 = maximum (map rX1 srt)
               , lSize = sz
               , lSpans = spans
               , lFixed = fixed }

-- | Join a line's runs, deciding at each junction between nothing (kerning),
-- one space (a word break) and a column boundary (which keeps the runs apart
-- and marks the line as positioned rather than prose).
joinRuns :: Double -> [Run] -> ([PdfSpan], Bool)
joinRuns sz (r : rest) = go (PdfSpan (rX0 r) (rTxt r) (rFmt r)) (rX1 r) rest [] False
  where
    wordGap = 0.22 * sz
    colGap = 1.6 * sz
    go cur _ [] acc fixed = (reverse (cur : acc), fixed)
    go cur end (x : xs) acc fixed
      | gap >= colGap =
          go (PdfSpan (rX0 x) (rTxt x) (rFmt x)) (rX1 x) xs (cur : acc) True
      | rFmt x /= psFmt cur =
          -- A format change ends the span, but the space (if any) belongs to
          -- the text that precedes it.
          let cur' = if needSpace then cur { psText = psText cur `T.snoc` ' ' } else cur
          in go (PdfSpan (rX0 x) (rTxt x) (rFmt x)) (rX1 x) xs (cur' : acc) fixed
      | otherwise =
          let sep = if needSpace then T.singleton ' ' else T.empty
          in go cur { psText = psText cur <> sep <> rTxt x } (max end (rX1 x)) xs acc fixed
      where
        gap = rX0 x - end
        needSpace = gap >= wordGap
                      && not (endsSpace (psText cur)) && not (startsSpace (rTxt x))
    endsSpace t = not (T.null t) && T.last t == ' '
    startsSpace t = not (T.null t) && T.head t == ' '
joinRuns _ [] = ([], False)

-- | Group lines into paragraphs and blank space.
--
-- A line joins the paragraph above it when the two are set the same way, the
-- leading between them is ordinary, and the line above ran to the right
-- margin — the last of which is what distinguishes wrapped prose from a
-- heading, a caption or the last line of a paragraph.
groupPars :: Double -> Double -> [PLine] -> [PdfPar]
groupPars colX0 colW ls0 = go ls0
  where
    -- Where the column's right margin is. A high percentile of where lines
    -- end, deliberately not the maximum: a page number or a running header
    -- sits out at the true page edge, and taking the maximum would mean no
    -- body line ever counts as "reaching the margin" — which is the one test
    -- that distinguishes a wrapped line from the last line of a paragraph.
    rightEdge = percentile 0.85 (map lX1 ls0)
    percentile p xs
      | null xs = 0
      | otherwise = let s = sortOn id xs
                        k = min (length s - 1) (floor (p * fromIntegral (length s)))
                    in s !! k
    frac v = (v - colX0) / colW
    -- The column's ordinary line spacing: the *most common* step between
    -- consecutive baselines, which is a far better signal for where a
    -- paragraph ends than anything derived from the font size. It has to be
    -- the mode and not the median — a page carrying several headings and
    -- paragraph breaks has enough wide steps to drag a median up past the
    -- body leading, and then every paragraph on it merges into one.
    leading =
      let dys = [ d | (a, b) <- zip ls0 (drop 1 ls0), let d = lY b - lY a, d > 0.5 ]
          tally = IM.fromListWith (+) [ (round d, 1 :: Int) | d <- dys ]
      in case sortOn (\(v, c) -> (negate c, v)) (IM.toList tally) of
           ((v, _) : _) -> fromIntegral v
           []           -> 0
    go [] = []
    go (l : ls)
      | lFixed l = fixedPar l : gap l ls
      | otherwise =
          let (body, rest) = grab l [l] ls
          in flowPar body : gap (last body) rest
    -- Collect the lines that continue this paragraph.
    grab prev acc (x : xs)
      | not (lFixed x)
      , dy > 0
      , sizeClose (lSize prev) (lSize x)
      , psFmtBold prev == psFmtBold x
      , lX0 x <= lX0 prev + 0.02 * colW
        -- Both signals must agree: the spacing is the column's ordinary
        -- leading (rather than the wider step a new paragraph is set with),
        -- and the line above ran to the right margin (rather than stopping
        -- short, the way the last line of a paragraph does).
      , dy <= leadTol (max (lSize prev) (lSize x))
      , lX1 prev >= rightEdge - 0.12 * colW
      = grab x (acc ++ [x]) xs
      where dy = lY x - lY prev
    grab _ acc rest = (acc, rest)
    -- A column of one or two lines has no leading to measure, so fall back
    -- to the point size there.
    leadTol sz = if leading > 0 then leading * 1.15 else 1.35 * sz
    sizeClose a b = abs (a - b) <= 0.15 * max a b
    psFmtBold l = case lSpans l of
      (s : _) -> pfBold (psFmt s)
      []      -> False
    -- Blank space between blocks, when the gap is noticeably more than one
    -- line's leading.
    gap prev rest = case rest of
      (x : _) | lY x - lY prev > max (leading * 1.6) (1.9 * max (lSize prev) (lSize x)) ->
        PdfPar PKBlank PAlignLeft 0 0 [] : go rest
      _ -> go rest

    fixedPar l = PdfPar PKFixed PAlignLeft 0 0
      [ s { psX = max 0 (min 0.98 (frac (psX s))) } | s <- lSpans l ]

    flowPar body =
      let firstL = head body
          margin = minimum (map lX0 body)
          runs = concatRuns body
          -- Centred and right-aligned blocks are recognised per line, and
          -- every line has to agree: a two-line paragraph whose short last
          -- line happens to balance is ordinary prose, not a centred block.
          align
            | all balanced body = PAlignCenter
            | length body == 1, leftPadOf firstL > 0.15, rightPadOf firstL < 0.03 = PAlignRight
            | otherwise = PAlignLeft
          leftPadOf l = frac (lX0 l)
          rightPadOf l = (rightEdge - lX1 l) / colW
          balanced l = leftPadOf l > 0.08 && rightPadOf l > 0.08
                         && abs (leftPadOf l - rightPadOf l) < 0.07
      in PdfPar { ppKind = PKFlow
                , ppAlign = align
                , ppMargin = clampFrac (if align == PAlignLeft then frac margin else 0)
                , ppFirst = if align == PAlignLeft
                              then clampFrac (frac (lX0 firstL)) - clampFrac (frac margin)
                              else 0
                , ppRuns = runs }
    clampFrac v = max 0 (min 0.35 v)

-- | Concatenate a paragraph's lines, restoring the space the line break stood
-- for — and undoing hyphenation, which is a line-breaking artefact of a page
-- width this view is not using.
concatRuns :: [PLine] -> [PdfSpan]
concatRuns = merge . foldr joinLine []
  where
    joinLine l acc = case acc of
      [] -> lSpans l
      _  -> lSpans l `bridge` acc
    bridge as bs = case (reverse as, bs) of
      (aLast : aRev, bFirst : bRest)
        | hyphenated (psText aLast) (psText bFirst) ->
            reverse (aLast { psText = T.init (psText aLast) } : aRev) ++ (bFirst : bRest)
        | otherwise ->
            reverse (aLast { psText = psText aLast `T.snoc` ' ' } : aRev) ++ bs
      _ -> as ++ bs
    hyphenated a b =
      not (T.null a) && T.last a == '-'
        && T.length a >= 2 && isLetter (T.index a (T.length a - 2))
        && not (T.null b) && isLetter (T.head b)
    isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c > '\xff'
    -- Adjacent spans that ended up with the same formatting are one span.
    merge (a : b : rest)
      | psFmt a == psFmt b = merge (a { psText = psText a <> psText b } : rest)
      | otherwise = a : merge (b : rest)
    merge xs = xs

------------------------------------------------------------------------------
-- Parsing a whole file

-- | Extract a readable document from PDF bytes.
--
-- The result is 'Left' only for a file this view cannot read at all (not a
-- PDF, encrypted, no pages); a file it reads *partially* comes back as a
-- document carrying a note, because half a document on screen beats an error
-- message.
parsePdf :: BS.ByteString -> Either String PdfDoc
parsePdf = parsePdfWithin maxPdfDecoded maxPdfTokens

-- | 'parsePdf' under a decoding budget and a token budget of the caller's
-- choosing, in place of 'maxPdfDecoded' and 'maxPdfTokens'. The limits are
-- sized so that no real document meets them, which also means no /test/ can
-- meet them in reasonable time; this is how the tests reach the code that
-- runs when one is met.
parsePdfWithin :: Int -> Int -> BS.ByteString -> Either String PdfDoc
parsePdfWithin decodeBudget tokenBudget bs = case openPdfFile decodeBudget bs of
  Left e -> Left e
  Right (file, bytesLeft) -> extractPdf tokenBudget bs file bytesLeft

-- | The text view of an opened file, starting from what its object streams
-- left of the decoding budget.
extractPdf :: Int -> BS.ByteString -> PdfFile -> Int -> Either String PdfDoc
extractPdf tokenBudget bs file bytesLeft
  | null pageDicts = Left "PDF has no pages"
  | otherwise = Right PdfDoc
      { pdPages = Seq.fromList pages
      , pdTop = 0
      , pdCache = Nothing
      , pdCaret = origin
      , pdAnchor = Nothing
      , pdTitle = title
      , pdVer = version
      , pdNote = note
      , pdSource = PdfSource file (Seq.fromList pageDicts)
      }
  where
    trailer = pfTrailer file
    pageDicts = collectPages file
    truncated = length pageDicts >= maxPdfPages

    (rawPages, fuEnd) = goPages 1 0 (xfuel0 bytesLeft tokenBudget) pageDicts
    pages = markHeadings rawPages
    -- Did the document run out of budget ('maxPdfDecoded', 'maxPdfTokens')
    -- before it ran out of pages?
    spent = xSpent fuEnd
    goPages :: Int -> Int -> XFuel -> [Dict] -> ([PdfPage], XFuel)
    goPages _ _ fu [] = ([], fu)
    goPages !k !chars fu (d : ds)
      | chars > maxPdfChars || xSpent fu = ([], fu)
      | otherwise =
          let (pars, fu') = pageOf fu d
              n = sum [ T.length (psText s) | p <- pars, s <- ppRuns p ]
              (rest, cut) = goPages (k + 1) (chars + n) fu' ds
          in (PdfPage k pars : rest, cut)

    pageOf fu d =
      let res = dictOf file (M.findWithDefault ONull (BC.pack "Resources") d)
          box = mediaBox file d
          rot = ((intOf file (M.lookup (BC.pack "Rotate") d) `mod` 360) + 360) `mod` 360
          (dat, core1) = contentsOf file (xfFuel fu) d
          fu1 = fu { xfFuel = core1 }
          (runs, fu2) = runContent file fu1 res dat
      in (assemblePage box rot runs, fu2)

    -- Body size is the size most of the document's characters are set in;
    -- anything meaningfully larger is a heading, which a terminal can only
    -- show as bold. (The same trick "Cmedit.Rtf" plays with \fs.) Measured
    -- before the marking pass, not after — reading it off the marked pages
    -- would be circular.
    body = bodySize rawPages
    markHeadings ps =
      [ p { pgPars = map markPar (pgPars p) } | p <- ps ]
      where
        markPar p = p { ppRuns = map markRun (ppRuns p) }
        markRun s
          | pfSize (psFmt s) >= body * 1.15 && pfSize (psFmt s) > body + 0.5 =
              s { psFmt = (psFmt s) { pfBold = True } }
          | otherwise = s

    title = case lookD file trailer "Info" of
      ODict info -> case lookD file info "Title" of
        OStr s -> textFromPdfString s
        _      -> T.empty
      _ -> T.empty

    version =
      let (_, post) = BS.breakSubstring (BC.pack "%PDF-") (BS.take 1024 bs)
      in T.pack (BC.unpack (BS.take 3 (BS.drop 5 post)))

    note
      | spent = T.pack ("this file asks for far more decoding than a document needs \x2014 only the first "
                        ++ show (length pages) ++ (if length pages == 1 then " page was" else " pages were")
                        ++ " read")
      | truncated = T.pack ("only the first " ++ show maxPdfPages ++ " pages are shown")
      | all (null . pgPars) pages =
          "no extractable text \x2014 the pages are images or use fonts without a Unicode map"
      | otherwise = T.empty

-- | A PDF text string: UTF-16BE when it carries the byte-order mark that says
-- so, PDFDocEncoding (near enough to Latin-1) otherwise.
textFromPdfString :: BS.ByteString -> Text
textFromPdfString s
  | BS.length s >= 2 && BS.index s 0 == 0xfe && BS.index s 1 == 0xff =
      decodeUtf16BE (BS.drop 2 s)
  | otherwise = T.pack (map w2c (BS.unpack s))

-- | The size most of the document's text is set in, weighted by how much text
-- is set in it. Rounded to the nearest half point so that a body face used at
-- 9.96pt and 10.0pt counts once.
bodySize :: [PdfPage] -> Double
bodySize ps
  | IM.null tally = 10
  | otherwise = fromIntegral (fst (last (sortOn snd (IM.toList tally)))) / 2
  where
    tally = foldl' add IM.empty
      [ (pfSize (psFmt s), T.length (psText s))
      | p <- ps, par <- pgPars p, s <- ppRuns par ]
    add m (sz, n) | sz <= 0 = m
                  | otherwise = IM.insertWith (+) (round (sz * 2)) n m


------------------------------------------------------------------------------
-- Layout

-- | Lay the extracted pages out for a terminal width, returning the lines and
-- the index at which each page starts (which is what page navigation moves
-- between).
--
-- Cost is linear in the document and it runs once per width — a resize — not
-- per frame or per scroll.
layoutPdf :: Int -> Int -> Seq PdfPage -> (Seq PdfLine, Seq Int)
layoutPdf tabw width pages
  | width <= 0 = (Seq.empty, Seq.empty)
  | otherwise = foldl' onePage (Seq.empty, Seq.empty) (toList pages)
  where
    onePage (acc, starts) pg =
      let sep = if Seq.null acc then Seq.empty
                  else Seq.fromList [ PdfLine 0 T.empty [] True (pgNumber pg) ]
          acc1 = acc <> sep
          body = Seq.fromList (concatMap (parLines (pgNumber pg)) (pgPars pg))
      in (acc1 <> body, starts |> Seq.length acc1)

    clampPad = max 0 . min (max 0 (width - 8))

    parLines page p = case ppKind p of
      PKBlank -> [ PdfLine 0 T.empty [] False page ]
      PKRule  -> [ PdfLine 0 T.empty [] True page ]
      PKFixed -> [ fixedLine page p ]
      PKFlow  -> flowLines page p

    -- A positioned line: each span goes where its fraction of the width says,
    -- never overlapping the one before it.
    fixedLine page p =
      let (txt, spans) = place 0 T.empty [] (ppRuns p)
      in PdfLine 0 txt spans False page
      where
        place _ txt spans [] = (txt, reverse spans)
        place !col txt spans (s : ss) =
          let want = max col (round (psX s * fromIntegral width))
              padN = max (if col == 0 then 0 else 1) (want - col)
              txt' = txt <> T.replicate padN (T.singleton ' ') <> psText s
              st = T.length txt + padN
              en = st + T.length (psText s)
          in if st >= width then (txt, reverse spans)
             else place en txt' ((st, min width en, psFmt s) : spans) ss

    flowLines page p =
      let (txt, spans) = flattenRuns (ppRuns p)
          margin = clampPad (round (ppMargin p * fromIntegral width))
          firstIn = clampPad (margin + round (ppFirst p * fromIntegral width))
          avail = max 1 (width - margin)
          segs = wrapLine tabw avail txt
      in [ mkLine page p (if k == (0 :: Int) then firstIn else margin) avail txt spans s e
         | (k, (s, e)) <- zip [0 ..] segs ]

    mkLine page p pad avail txt spans s e =
      let seg = T.take (e - s) (T.drop s txt)
          sp = sliceSpans s e spans
          wdt = lineDisplayWidth tabw seg
          extra = case ppAlign p of
                    PAlignCenter -> max 0 (avail - wdt) `div` 2
                    PAlignRight  -> max 0 (avail - wdt)
                    PAlignLeft   -> 0
      in PdfLine (pad + extra) seg sp False page

-- | Concatenate a paragraph's runs into one text plus the character ranges
-- each run occupies (the shape the renderer's per-character style lookup
-- wants).
flattenRuns :: [PdfSpan] -> (Text, [(Int, Int, PdfFmt)])
flattenRuns runs = (T.concat (map psText runs), go 0 runs)
  where
    go _ [] = []
    go !i (r : rs) =
      let n = T.length (psText r)
      in if n == 0 then go i rs else (i, i + n, psFmt r) : go (i + n) rs

sliceSpans :: Int -> Int -> [(Int, Int, PdfFmt)] -> [(Int, Int, PdfFmt)]
sliceSpans s e spans =
  [ (max 0 (a - s), min (e - s) (b - s), f)
  | (a, b, f) <- spans, b > s, a < e ]

------------------------------------------------------------------------------
-- The view

-- | Lay the document out for this width and tab size if that is not what the
-- cache already holds, and re-clamp the scroll position to the result.
pdfRelayout :: Int -> Int -> Int -> PdfDoc -> PdfDoc
pdfRelayout tabw width height pd
  | Just (w, tw, _, _) <- pdCache pd, w == width, tw == tabw = pd
  | width <= 0 = pd
  | otherwise =
      let (ls, starts) = layoutPdf tabw width (pdPages pd)
      in pdfClamp height pd { pdCache = Just (width, tabw, ls, starts)
                            -- A selection is a pair of indices into the old
                            -- layout; after a re-wrap they point at different
                            -- text, so they go rather than lie.
                            , pdCaret = origin, pdAnchor = Nothing }

pdfLines :: PdfDoc -> Seq PdfLine
pdfLines pd = case pdCache pd of
  Just (_, _, ls, _) -> ls
  Nothing            -> Seq.empty

pdfPageStarts :: PdfDoc -> Seq Int
pdfPageStarts pd = case pdCache pd of
  Just (_, _, _, st) -> st
  Nothing            -> Seq.empty

pdfLineCount :: PdfDoc -> Int
pdfLineCount = Seq.length . pdfLines

-- | First laid-out line of page @n@ (1-based).
--
-- Not @pdTop@ after a 'pdfGoToPage': that is the scroll position, which
-- 'pdfClamp' pulls back on the last pages of a document. See
-- 'Cmedit.Rtf.rtfSectionLine' for the same distinction.
pdfPageLine :: Int -> PdfDoc -> Int
pdfPageLine n pd =
  fromMaybe 0 (Seq.lookup (max 0 (n - 1)) (pdfPageStarts pd))

pdfPageCount :: PdfDoc -> Int
pdfPageCount = Seq.length . pdPages

------------------------------------------------------------------------------
-- Selection
--
-- The model is the text view's, minus everything that writes: a caret and an
-- optional anchor. It exists so that a passage can be copied out — looking
-- something up in a manual and pasting the answer somewhere else is most of
-- what a PDF gets opened for — and for the Find command to have somewhere to
-- put a hit. Nothing here can modify the document; there is no document to
-- modify, only the extracted text.

-- | The active selection as an ordered pair, or 'Nothing' when the caret and
-- anchor coincide (or there is no anchor at all).
pdfSelection :: PdfDoc -> Maybe (Pos, Pos)
pdfSelection pd = case pdAnchor pd of
  Just a | a /= pdCaret pd -> Just (if a <= pdCaret pd then (a, pdCaret pd) else (pdCaret pd, a))
  _ -> Nothing

pdfLineText :: PdfDoc -> Int -> Text
pdfLineText pd i = maybe T.empty plText (Seq.lookup i (pdfLines pd))

-- | Clamp a position onto the laid-out document.
pdfClampPos :: PdfDoc -> Pos -> Pos
pdfClampPos pd (Pos l c) =
  let n = pdfLineCount pd
      l' = max 0 (min (max 0 (n - 1)) l)
  in Pos l' (max 0 (min (T.length (pdfLineText pd l')) c))

-- | The selected text: the visible lines, sliced and joined.
--
-- What you see is what you copy — these are the reflowed lines on screen, not
-- the source paragraph, because the line breaks on screen are the only ones
-- this view can honestly claim the document has.
pdfSelText :: PdfDoc -> Text
pdfSelText pd = case pdfSelection pd of
  Nothing -> T.empty
  Just (Pos sl sc, Pos el ec)
    | sl == el  -> slice sl sc ec
    | otherwise -> T.intercalate (T.singleton '\n')
        ( slice sl sc maxBound
        : [ pdfLineText pd i | i <- [sl + 1 .. el - 1] ]
          ++ [ slice el 0 ec ] )
  where slice i a b = let t = pdfLineText pd i
                      in T.take (max 0 (min (T.length t) b - a)) (T.drop a t)

pdfSetCaret :: Pos -> PdfDoc -> PdfDoc
pdfSetCaret p pd = pd { pdCaret = pdfClampPos pd p, pdAnchor = Nothing }

-- | Select @[a, b)@, leaving the caret at @b@.
pdfSelectRange :: Pos -> Pos -> PdfDoc -> PdfDoc
pdfSelectRange a b pd = pd { pdAnchor = Just (pdfClampPos pd a), pdCaret = pdfClampPos pd b }

pdfSelectAll :: PdfDoc -> PdfDoc
pdfSelectAll pd
  | pdfLineCount pd == 0 = pd
  | otherwise =
      let l = pdfLineCount pd - 1
      in pd { pdAnchor = Just origin, pdCaret = Pos l (T.length (pdfLineText pd l)) }

pdfClearSel :: PdfDoc -> PdfDoc
pdfClearSel pd = pd { pdAnchor = Nothing }

-- | Move the caret, starting a selection from where it was if there is not
-- one already — the Shift+movement rule, in one place.
pdfExtendTo :: (PdfDoc -> PdfDoc) -> PdfDoc -> PdfDoc
pdfExtendTo move pd =
  let anchored = case pdAnchor pd of
                   Just _  -> pd
                   Nothing -> pd { pdAnchor = Just (pdCaret pd) }
  in move anchored

-- | The position under a cell, given the line it falls on and a display
-- column measured from the left edge of the text area. The line's leading pad
-- (its indent and alignment) is not text, so a click in it lands at column 0.
pdfPosAtCell :: Int -> Int -> Int -> PdfDoc -> Pos
pdfPosAtCell tabw line dcol pd =
  let Pos l _ = pdfClampPos pd (Pos line 0)
      pad = maybe 0 plPad (Seq.lookup l (pdfLines pd))
      txt = pdfLineText pd l
  in Pos l (displayToCol tabw (max 0 (dcol - pad)) txt)

-- | The inverse: which display column a position sits at, pad included. The
-- renderer and the cursor placement share it.
pdfCellOfPos :: Int -> Pos -> PdfDoc -> Int
pdfCellOfPos tabw (Pos l c) pd =
  let pad = maybe 0 plPad (Seq.lookup l (pdfLines pd))
  in pad + colToDisplay tabw c (pdfLineText pd l)

-- | The word around a position (a double-click), or the position twice when
-- it is not on one.
pdfWordRange :: Pos -> PdfDoc -> (Pos, Pos)
pdfWordRange p@(Pos l c) pd
  | T.null txt = (p, p)
  | c >= T.length txt || not (wordChar (T.index txt (min c (T.length txt - 1)))) = (p, p)
  | otherwise = (Pos l a, Pos l b)
  where
    txt = pdfLineText pd l
    a = c - length (takeWhile wordChar (reverse (T.unpack (T.take c txt))))
    b = c + length (takeWhile wordChar (T.unpack (T.drop c txt)))

pdfLineRange :: Pos -> PdfDoc -> (Pos, Pos)
pdfLineRange (Pos l _) pd = (Pos l 0, Pos l (T.length (pdfLineText pd l)))

wordChar :: Char -> Bool
wordChar ch = ch == '_' || ch > '\x7f'
                || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
                || (ch >= '0' && ch <= '9')

-- Caret movement. Each moves only the caret; the caller decides whether the
-- anchor comes along ('pdfExtendTo') or is dropped.
pdfCaretLeft, pdfCaretRight, pdfCaretUp, pdfCaretDown :: PdfDoc -> PdfDoc
pdfCaretLeft pd = case pdCaret pd of
  Pos l 0 | l > 0 -> pd { pdCaret = Pos (l - 1) (T.length (pdfLineText pd (l - 1))) }
  Pos l c -> pd { pdCaret = Pos l (max 0 (c - 1)) }
pdfCaretRight pd = case pdCaret pd of
  Pos l c | c >= T.length (pdfLineText pd l), l < pdfLineCount pd - 1 -> pd { pdCaret = Pos (l + 1) 0 }
          | otherwise -> pd { pdCaret = pdfClampPos pd (Pos l (c + 1)) }
pdfCaretUp pd = let Pos l c = pdCaret pd in pd { pdCaret = pdfClampPos pd (Pos (l - 1) c) }
pdfCaretDown pd = let Pos l c = pdCaret pd in pd { pdCaret = pdfClampPos pd (Pos (l + 1) c) }

pdfCaretHome, pdfCaretEnd, pdfCaretTop, pdfCaretBottom :: PdfDoc -> PdfDoc
pdfCaretHome pd = let Pos l _ = pdCaret pd in pd { pdCaret = Pos l 0 }
pdfCaretEnd pd = let Pos l _ = pdCaret pd
                 in pd { pdCaret = Pos l (T.length (pdfLineText pd l)) }
pdfCaretTop pd = pd { pdCaret = origin }
pdfCaretBottom pd = pdfClampCaret pd { pdCaret = Pos (max 0 (pdfLineCount pd - 1)) maxBound }

pdfClampCaret :: PdfDoc -> PdfDoc
pdfClampCaret pd = pd { pdCaret = pdfClampPos pd (pdCaret pd) }

-- | Scroll the window so the caret is on it, moving as little as possible.
pdfScrollToCaret :: Int -> PdfDoc -> PdfDoc
pdfScrollToCaret height pd
  | l < pdTop pd = pdfClamp height pd { pdTop = l }
  | l >= pdTop pd + h = pdfClamp height pd { pdTop = l - h + 1 }
  | otherwise = pd
  where Pos l _ = pdCaret pd
        h = max 1 height

------------------------------------------------------------------------------

-- | Scroll by @d@ lines, keeping at least one line on screen.
pdfScroll :: Int -> Int -> PdfDoc -> PdfDoc
pdfScroll height d pd = pdfClamp height pd { pdTop = pdTop pd + d }

pdfGoTop :: PdfDoc -> PdfDoc
pdfGoTop pd = pd { pdTop = 0 }

pdfGoBottom :: Int -> PdfDoc -> PdfDoc
pdfGoBottom height pd = pdfClamp height pd { pdTop = pdfLineCount pd }

pdfClamp :: Int -> PdfDoc -> PdfDoc
pdfClamp height pd =
  pd { pdTop = max 0 (min (max 0 (pdfLineCount pd - max 1 height)) (pdTop pd)) }

-- | Which page the top of the window is showing (1-based).
pdfCurrentPage :: PdfDoc -> Int
pdfCurrentPage pd =
  let starts = pdfPageStarts pd
      before = Seq.length (Seq.takeWhileL (<= pdTop pd) starts)
  in max 1 (min (max 1 (pdfPageCount pd)) before)

-- | Scroll so page @n@ (1-based) starts at the top of the window.
pdfGoToPage :: Int -> Int -> PdfDoc -> PdfDoc
pdfGoToPage height n pd =
  let k = max 0 (min (pdfPageCount pd - 1) (n - 1))
  in case Seq.lookup k (pdfPageStarts pd) of
       Just top -> pdfClamp height pd { pdTop = top }
       Nothing  -> pd

pdfNextPage :: Int -> PdfDoc -> PdfDoc
pdfNextPage height pd = pdfGoToPage height (pdfCurrentPage pd + 1) pd

-- | Back one page — or to the top of the current one when the window has
-- scrolled into its middle, which is what a reader means by "back".
pdfPrevPage :: Int -> PdfDoc -> PdfDoc
pdfPrevPage height pd =
  let cur = pdfCurrentPage pd
      start = fromMaybe 0 (Seq.lookup (cur - 1) (pdfPageStarts pd))
  in if pdTop pd > start then pdfGoToPage height cur pd
     else pdfGoToPage height (cur - 1) pd

-- | The status-bar text: where you are in the document, and that this is a
-- read-only rendering rather than the file's bytes.
pdfStatus :: PdfDoc -> String
pdfStatus pd =
  "Page " ++ show (pdfCurrentPage pd) ++ " of " ++ show (pdfPageCount pd)
    ++ "   Ln " ++ show (pdTop pd + 1) ++ " of " ++ show (pdfLineCount pd)
    ++ "   PDF "

-- | The whole document as plain text: one line per reconstructed block, in
-- reading order, with pages separated by a blank line.
--
-- The paragraphs rather than the laid-out lines, for the reason
-- 'Cmedit.Rtf.rtfPlainText' gives: a file wrapped to the width the terminal
-- happened to be is a worse artifact than one the reader can wrap itself.
pdfPlainText :: PdfDoc -> Text
pdfPlainText pd = T.unlines (concatMap pageLines (toList (pdPages pd)))
  where
    pageLines pg = map parLine (pgPars pg) ++ [T.empty]
    parLine p | ppKind p == PKBlank = T.empty
              | otherwise = T.concat (map psText (ppRuns p))
