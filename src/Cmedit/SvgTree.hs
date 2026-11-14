{-# LANGUAGE BangPatterns #-}
-- | The document side of "Cmedit.Svg": bytes to an element tree, the CSS
-- cascade, and computed styles. Internal; "Cmedit.Svg" re-exports what a
-- caller needs.
--
-- __Namespaces are resolved here, not by prefix-stripping.__ The XML
-- reader's usual local-name matching would let @sodipodi:cx@ answer for
-- @cx@ and an Inkscape @\<sodipodi:guide\>@ be taken for something SVG, so
-- this module reads qualified names ('parseXmlQualified') and keeps an
-- element only when its namespace is SVG's. An element in no namespace at
-- all is taken to be SVG too: hand-written files routinely omit the
-- declaration, and refusing them would help nobody. Foreign subtrees
-- (RDF metadata, XHTML in a @foreignObject@) are dropped whole while
-- building, so they cost nothing later. Attributes keep SVG's unprefixed
-- ones plus @xlink:href@ (folded into @href@, which wins when both are
-- present, as SVG 2 says) and @xml:space@.
--
-- __Specified once, computed per use.__ The cascade (presentation
-- attributes < style sheet by specificity and order < @style=\"\"@, with
-- @!important@ above all) depends only on where an element sits in the
-- document, so 'specified' is computed once per element. The /computed/
-- style depends on what it inherits from, and that differs between an
-- element drawn in place and the same element instanced by a @\<use\>@, so
-- 'computeStyle' takes the parent style as an argument. Every candidate
-- value is kept in priority order and the first one that parses wins: an
-- invalid declaration is ignored, exactly as CSS says, rather than
-- shadowing a valid one below it. The in-place styles that paint servers,
-- clip paths, masks and markers need (their contents inherit from /their/
-- ancestors, never from the element that references them) are a lazy array,
-- so each is computed at most once and only if something asks.
module Cmedit.SvgTree
  ( -- * Documents
    Doc(..)
  , El(..)
  , Node(..)
  , buildDoc
  , decodeSvgBytes
  , attr
  , elOf
  , lookupId
  , refId
  , urlRef
  , elemKids
  , maxSvgElements
  , maxSvgBytes
    -- * Styles
  , Style(..)
  , PaintSpec(..)
  , PaintPart(..)
  , Anchor(..)
  , Baseline(..)
  , FontStyle(..)
  , FilterSpec(..)
  , CssFilterFn(..)
  , rootStyle
  , computeStyle
  , inPlaceStyle
    -- * Lengths
  , Len(..)
  , Unit(..)
  , Axis(..)
  , parseLength
  , resolveLen
  , lenAttr
  , parseAngle
  , numOrPct
    -- * Colour matrices
  , saturateM
  , hueRotateM
  , linM
  , parsePaint
  , blendMode
  ) where

import Data.Array
import Data.Bits (testBit)
import Data.Char (isSpace)
import Data.List (foldl', sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (comparing, Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Cmedit.Scene
import Cmedit.SvgCss
import Cmedit.Geom (Path, Rect(..), pathBoundsExact)
import Cmedit.SvgPath (PCmd(..), parseNumber, parseNumberList, parsePathData, parsePoints, cmdsToPath)
import Cmedit.Xml (XmlEvent(..), parseXmlQualified, decodeXmlBytes)
import Cmedit.Inflate (inflateDyn)

------------------------------------------------------------------------------
-- Bounds

-- | Most elements a document may contain; the rest are dropped (and noted).
maxSvgElements :: Int
maxSvgElements = 200000

-- | Largest decompressed document (an @.svgz@ is a gzip stream, and a gzip
-- stream names no size it can be held to).
maxSvgBytes :: Int
maxSvgBytes = 64 * 1024 * 1024

------------------------------------------------------------------------------
-- Decoding

-- | Bytes to text: gunzip an @.svgz@, then the BOM decides (UTF-8, UTF-16
-- either way), and a BOM-less UTF-16 document is recognised by its first
-- @<@. 'Left' only for a gzip stream that is not one.
decodeSvgBytes :: BS.ByteString -> Either String Text
decodeSvgBytes bs0 = do
  bs <- if BS.take 2 bs0 == BS.pack [0x1f, 0x8b] then gunzip bs0 else Right bs0
  pure $ case BS.unpack (BS.take 2 bs) of
    [0x3c, 0x00] -> TE.decodeUtf16LEWith TEE.lenientDecode bs
    [0x00, 0x3c] -> TE.decodeUtf16BEWith TEE.lenientDecode bs
    _            -> decodeXmlBytes bs

-- RFC 1952: a fixed ten-byte header, then optional extra / name / comment /
-- header-CRC fields named by the flag byte, then raw DEFLATE.
gunzip :: BS.ByteString -> Either String BS.ByteString
gunzip bs
  | BS.length bs < 18 = Left "truncated gzip stream"
  | BS.index bs 2 /= 8 = Left "gzip stream is not deflate"
  | otherwise =
      let flg = BS.index bs 3
          o1 = 10
          o2 = if testBit flg 2
                 then o1 + 2 + fromIntegral (BS.index bs o1) + 256 * fromIntegral (BS.index bs (o1 + 1))
                 else o1
          zstr o = case BS.elemIndex 0 (BS.drop o bs) of
                     Just k  -> o + k + 1
                     Nothing -> BS.length bs
          o3 = if testBit flg 3 then zstr o2 else o2
          o4 = if testBit flg 4 then zstr o3 else o3
          o5 = if testBit flg 1 then o4 + 2 else o4
      in if o5 >= BS.length bs then Left "truncated gzip stream"
         else inflateDyn bs o5 maxSvgBytes

------------------------------------------------------------------------------
-- The element tree

-- | A node of an element's content.
data Node = NE !Int | NT !Text
  deriving (Show)

-- | An SVG element. Attribute names are unprefixed except @xml:space@.
data El = El
  { elName   :: !Text
  , elAttrs  :: !(M.Map Text Text)
  , elKids   :: [Node]
  , elParent :: !Int        -- ^ -1 for the root.
  } deriving (Show)

data Doc = Doc
  { dEls     :: Array Int El
  , dIds     :: M.Map Text Int
  , dSpec    :: Array Int (M.Map Text [Text])   -- ^ Candidate values per property, highest priority first.
  , dInPlace :: Array Int Style                 -- ^ Lazy: computed in document position.
  , dGeom    :: Array Int (Maybe ([PCmd], Path, Maybe Rect))
    -- ^ Lazy: the parsed geometry of each path, polyline and polygon (the
    -- shapes whose geometry is independent of style), with its exact bounds.
    -- Parsed once however many times a @\<use\>@ instances it, and every
    -- instance shares the same 'Path' value.
  , dRoot    :: Style                           -- ^ What the root inherits.
  , dDropped :: !Bool                           -- ^ Elements were dropped at 'maxSvgElements'.
  }

attr :: El -> Text -> Maybe Text
attr e k = M.lookup k (elAttrs e)
{-# INLINE attr #-}

elOf :: Doc -> Int -> El
elOf d i = dEls d ! i

-- | Element children, in order.
elemKids :: El -> [Int]
elemKids e = [ i | NE i <- elKids e ]

lookupId :: Doc -> Text -> Maybe Int
lookupId d k = M.lookup k (dIds d)

-- | @#id@ (an href) to the id.
refId :: Text -> Maybe Text
refId t = case T.uncons (T.strip t) of
  Just ('#', r) | not (T.null r) -> Just r
  _ -> Nothing

-- | @url(#id)@ (optionally quoted) to the id and whatever follows (a paint
-- fallback).
urlRef :: Text -> Maybe (Text, Text)
urlRef t0 = do
  r <- T.stripPrefix "url(" (T.stripStart t0)
  let (inner, post) = T.breakOn ")" r
      unq = T.dropAround (\c -> c == '"' || c == '\'' || isSpace c) inner
  if T.null post then Nothing else do
    i <- refId unq
    Just (i, T.strip (T.drop 1 post))

svgNS, xlinkNS :: Text
svgNS = "http://www.w3.org/2000/svg"
xlinkNS = "http://www.w3.org/1999/xlink"

-- Build state: open elements (id, name, attrs, kids reversed, ns scope),
-- finished elements, next id, foreign-skip depth.
data Open = Open !Int !Text !(M.Map Text Text) [Node] !(M.Map Text Text) !Text

-- The style-independent geometry of an element (see 'dGeom').
geomOf :: El -> Maybe ([PCmd], Path, Maybe Rect)
geomOf e = case elName e of
  "path" -> Just (mk (parsePathData (maybe T.empty id (attr e "d"))))
  "polyline" -> poly False
  "polygon" -> poly True
  _ -> Nothing
  where
    mk pcs = let p = cmdsToPath pcs in (pcs, p, pathBoundsExact p)
    poly closed = case parsePoints (maybe T.empty id (attr e "points")) of
      (p : ps) -> Just (mk (PMove p : map PLine ps ++ [ PClose | closed ]))
      [] -> Nothing

-- | Parse a document's text into a 'Doc'. 'Left' when there is no SVG root.
buildDoc :: Style -> Text -> Either String Doc
buildDoc root0 src =
  let (els, dropped) = build (parseXmlQualified src)
      n = length els
  in if n == 0 then Left "not an SVG document (no <svg> element)"
     else
       let arr = array (0, n - 1) els
       in if elName (arr ! 0) /= "svg" then Left "not an SVG document (root is not <svg>)"
          else Right (finish arr dropped)
  where
    finish arr dropped =
      let ids = M.fromListWith (\_ old -> old)
                  [ (v, i) | (i, e) <- assocs arr, Just v <- [attr e "id"] ]
          spec = cascade arr
          doc = Doc arr ids spec inplace geom root0 dropped
          geom = fmap geomOf arr
          inplace = listArray (bounds arr)
                      [ computeStyle doc par i
                      | i <- range (bounds arr)
                      , let p = elParent (arr ! i)
                            par = if p < 0 then root0 else inplace ! p ]
      in doc

    build :: [XmlEvent] -> ([(Int, El)], Bool)
    build evs0 = go evs0 [] [] 0 (0 :: Int) False
      where
        -- stack, done, next id, skip depth, dropped flag
        go [] stack done _ _ dr = (closeAll stack done, dr)
        go (ev : evs) stack done !nid !skip dr = case ev of
          XStart q as
            | skip > 0 -> go evs stack done nid (skip + 1) dr
            | otherwise ->
                let scope0 = case stack of (Open _ _ _ _ sc _ : _) -> sc; [] -> M.empty
                    scope = foldl' nsDecl scope0 as
                    (pre, local) = splitQ q
                    ns = M.lookup pre scope
                    isSvg = case ns of
                      Nothing -> T.null pre || pre == "svg"   -- undeclared default (or a bare svg: prefix)
                      Just u  -> u == svgNS
                    rootOk = not (null stack) || local == "svg"
                in if not isSvg || not rootOk || (null stack && not (null done))
                     then go evs stack done nid 1 dr
                     else if nid >= maxSvgElements
                       then go evs stack done nid 1 True
                       else go evs (Open nid local (attrMap scope as) [] scope q : stack) done (nid + 1) 0 dr
          XEnd q
            | skip > 0 -> go evs stack done nid (skip - 1) dr
            | otherwise ->
                -- Close up to the matching open element; a closer that
                -- matches nothing is ignored.
                if any (\(Open _ _ _ _ _ oq) -> oq == q) stack
                  then let (stack', done') = closeTo q stack done in go evs stack' done' nid 0 dr
                  else go evs stack done nid 0 dr
          XText t
            | skip > 0 -> go evs stack done nid skip dr
            | otherwise -> case stack of
                (Open i nm as ks sc oq : rest) | keepsText nm ->
                  go evs (Open i nm as (NT t : ks) sc oq : rest) done nid 0 dr
                _ -> go evs stack done nid 0 dr
        closeTo q (o@(Open _ _ _ _ _ oq) : rest) done =
          let (rest', done') = pop o rest done
          in if oq == q then (rest', done') else closeTo q rest' done'
        closeTo _ [] done = ([], done)
        pop (Open i nm as ks _ _) rest done =
          let parent = case rest of (Open p _ _ _ _ _ : _) -> p; [] -> -1
              el = El nm as (reverse ks) parent
              rest' = case rest of
                (Open p pn pa pk ps pq : more) -> Open p pn pa (NE i : pk) ps pq : more
                [] -> []
          in (rest', (i, el) : done)
        closeAll [] done = done
        closeAll (o : rest) done = let (r', d') = pop o rest done in closeAll r' d'

    keepsText nm = nm `elem` ["text", "tspan", "textPath", "a", "style", "tref"]
    splitQ q = case T.breakOn ":" q of
      (a, b) | T.null b -> ("", a)
             | otherwise -> (a, T.drop 1 b)
    nsDecl sc (k, v)
      | k == "xmlns" = M.insert "" v sc
      | Just p <- T.stripPrefix "xmlns:" k = M.insert p v sc
      | otherwise = sc
    attrMap scope as =
      let plain = [ (k, v) | (k, v) <- as, not (T.any (== ':') k), k /= "xmlns" ]
          pref  = [ (local, v) | (k, v) <- as, let (p, local) = splitQ k, not (T.null p)
                               , p /= "xmlns", M.lookup p scope == Just xlinkNS || (p == "xlink" && M.notMember p scope) ]
          xmlA  = [ (k, v) | (k, v) <- as, "xml:" `T.isPrefixOf` k ]
          base = M.fromList (xmlA ++ reverse plain)
          withHref = case lookup "href" pref of
            Just h | M.notMember "href" base -> M.insert "href" h base
            _ -> base
      in withHref

------------------------------------------------------------------------------
-- The cascade

-- Priority: (important, origin, specificity, order). Origin 0 = presentation
-- attribute, 1 = style sheet, 2 = style attribute.
type Prio = (Bool, Int, Specificity, Int)

cascade :: Array Int El -> Array Int (M.Map Text [Text])
cascade arr = fmap specOf infosIx
  where
    infosIx = listArray (bounds arr) (range (bounds arr)) :: Array Int Int
    sheets = [ t | e <- elems arr, elName e == "style"
                 , maybe True (\ty -> let ty' = T.toLower (T.strip ty) in T.null ty' || ty' == "text/css") (attr e "type")
                 , let t = T.concat [ s | NT s <- elKids e ] ]
    rules = take maxCssRules (concat (numberSheets 0 sheets))
    numberSheets _ [] = []
    numberSheets !o (s : ss) = let rs = parseSheet o s in rs : numberSheets (o + length rs) ss
    ix = indexRules rules
    -- Sibling structure for selectors.
    -- accumArray, not array: each index is set at most once here, but
    -- accumArray is defined for any association list, array is not.
    prevSib = accumArray (\_ v -> v) (-1) (bounds arr)
                [ (k, p) | e <- elems arr, let ks = elemKids e, (p, k) <- zip ks (drop 1 ks) ] :: Array Int Int
    isLast = accumArray (\_ v -> v) False (bounds arr)
                ((0, True) : [ (k, True) | e <- elems arr, Just k <- [lastMaybe (elemKids e)] ]) :: Array Int Bool
    lastMaybe [] = Nothing
    lastMaybe xs = Just (last xs)
    infos = listArray (bounds arr) [ mkInfo i | i <- range (bounds arr) ] :: Array Int ElemInfo
    mkInfo i = let e = arr ! i
                   p = elParent e
                   pv = prevSib ! i
               in ElemInfo (elName e) (\k -> attr e k)
                           (if p < 0 then Nothing else Just (infos ! p))
                           (if pv < 0 then Nothing else Just (infos ! pv))
                           (isLast ! i)
    specOf i =
      let e = arr ! i
          pres = [ ((False, 0, (0, 0, 0), 0), d)
                 | (k, v) <- M.toList (elAttrs e), isProperty k
                 , d <- expandShorthand (Decl k (T.strip v) False) ]
          sheet = [ ((dImportant d, 1, ruleSpec r, ruleOrder r), d)
                  | not (null rules)
                  , r <- candidateRules ix (elName e) (attr e "id") (maybe [] T.words (attr e "class"))
                  , matches (ruleSel r) (infos ! i)
                  , d0 <- ruleDecls r, d <- expandShorthand d0 ]
          inline = [ ((dImportant d, 2, (0, 0, 0), k), d)
                   | Just s <- [attr e "style"], (k, d0) <- zip [0 ..] (parseDecls s)
                   , d <- expandShorthand d0 ]
          allD = sortBy (comparing (Down . fst)) (pres ++ sheet ++ inline) :: [(Prio, Decl)]
      in M.fromListWith (flip (++)) [ (dProp d, [dValue d]) | (_, d) <- allD ]

-- Shorthands we expand: marker (style sheets only in SVG 1.1, harmless as
-- an attribute) and font.
expandShorthand :: Decl -> [Decl]
expandShorthand d@(Decl p v imp) = case p of
  "marker" -> [ Decl q v imp | q <- ["marker-start", "marker-mid", "marker-end"] ]
  "font"   -> fontShorthand
  _        -> [d]
  where
    fontShorthand =
      let ws = T.words v
          (pre, rest) = break (\w -> isJust (sizeTok w)) ws
          styleW = [ w | w <- pre, T.toLower w `elem` ["italic", "oblique"] ]
          weightW = [ w | w <- pre, T.toLower w `elem` ["bold", "bolder", "lighter"] || T.all (`elem` ['0' .. '9']) w ]
      in case rest of
           (sz : fam) ->
             let (s, _) = T.breakOn "/" sz
             in [ Decl "font-style" (fromMaybe "normal" (listToMaybe styleW)) imp
                , Decl "font-weight" (fromMaybe "normal" (listToMaybe weightW)) imp
                , Decl "font-size" s imp ]
                ++ [ Decl "font-family" (T.unwords fam) imp | not (null fam) ]
           [] -> []
    sizeTok w = let (s, _) = T.breakOn "/" w in parseLength s >>= \l -> if lenUnit l == UNone && lenVal l /= 0 then Nothing else Just ()

-- The properties that may appear as presentation attributes (SVG 1.1's
-- list plus the SVG 2 / CSS ones exporters write as attributes).
isProperty :: Text -> Bool
isProperty k = M.member k propSet

propSet :: M.Map Text ()
propSet = M.fromList [ (p, ()) | p <-
  [ "fill", "fill-opacity", "fill-rule", "stroke", "stroke-width", "stroke-opacity"
  , "stroke-linecap", "stroke-linejoin", "stroke-miterlimit", "stroke-dasharray"
  , "stroke-dashoffset", "opacity", "color", "display", "visibility", "clip-path"
  , "clip-rule", "mask", "filter", "marker-start", "marker-mid", "marker-end"
  , "font-family", "font-size", "font-weight", "font-style", "font", "text-anchor"
  , "letter-spacing", "word-spacing", "stop-color", "stop-opacity", "flood-color"
  , "flood-opacity", "overflow", "image-rendering", "paint-order", "vector-effect"
  , "mask-type", "mix-blend-mode", "isolation", "color-interpolation-filters"
  , "dominant-baseline", "alignment-baseline", "baseline-shift", "text-decoration"
  , "white-space", "font-variant", "shape-rendering", "text-rendering", "direction"
  , "lighting-color", "color-interpolation", "writing-mode", "font-stretch" ] ]

------------------------------------------------------------------------------
-- Lengths

data Unit = UNone | UPx | UPt | UPc | UMm | UCm | UIn | UEm | UEx | UPct
  deriving (Eq, Show)

data Len = Len { lenVal :: !Double, lenUnit :: !Unit }
  deriving (Eq, Show)

-- | Which viewport dimension a percentage refers to: width, height, or the
-- normalised diagonal @sqrt((w² + h²) / 2)@.
data Axis = AxX | AxY | AxD
  deriving (Eq, Show)

parseLength :: Text -> Maybe Len
parseLength t = do
  (v, r) <- parseNumber t
  u <- case T.toLower (T.strip r) of
    ""   -> Just UNone; "px" -> Just UPx; "pt" -> Just UPt; "pc" -> Just UPc
    "mm" -> Just UMm;   "cm" -> Just UCm; "in" -> Just UIn; "em" -> Just UEm
    "ex" -> Just UEx;   "%"  -> Just UPct
    "rem" -> Just UEm   -- no root font size distinct from ours worth tracking
    _    -> Nothing
  Just (Len v u)

-- | A length in user units, given the font size and the viewport.
resolveLen :: Double -> (Double, Double) -> Axis -> Len -> Double
resolveLen fs (vw, vh) ax (Len v u) = case u of
  UNone -> v; UPx -> v; UPt -> v * 4 / 3; UPc -> v * 16; UMm -> v / 25.4 * 96
  UCm -> v / 2.54 * 96; UIn -> v * 96; UEm -> v * fs; UEx -> v * fs / 2
  UPct -> v / 100 * case ax of
    AxX -> vw
    AxY -> vh
    AxD -> sqrt ((vw * vw + vh * vh) / 2)

-- | An attribute length resolved against a style's font size and viewport.
lenAttr :: Style -> Axis -> El -> Text -> Maybe Double
lenAttr st ax e k = resolveLen (stFontSize st) (stVp st) ax <$> (attr e k >>= parseLength)

-- | A number or a percentage, as a fraction for percentages.
numOrPct :: Text -> Maybe Double
numOrPct t = case parseNumber t of
  Just (v, r) | T.strip r == "%" -> Just (v / 100)
              | T.null (T.strip r) -> Just v
  _ -> Nothing

-- | An angle in degrees (@deg@, @rad@, @grad@, @turn@ or a bare number).
parseAngle :: Text -> Maybe Double
parseAngle t = case parseNumber t of
  Just (v, r) -> case T.toLower (T.strip r) of
    ""     -> Just v
    "deg"  -> Just v
    "rad"  -> Just (v * 180 / pi)
    "grad" -> Just (v * 0.9)
    "turn" -> Just (v * 360)
    _      -> Nothing
  Nothing -> Nothing

------------------------------------------------------------------------------
-- Computed style

data PaintSpec
  = PSNone
  | PSColor !ColorSpec
  | PSUrl !Text !(Maybe PaintSpec)     -- ^ Paint server id and fallback.
  | PSContextFill
  | PSContextStroke
  deriving (Eq, Show)

data PaintPart = PPFill | PPStroke | PPMarkers
  deriving (Eq, Show)

data Anchor = AnchorStart | AnchorMiddle | AnchorEnd
  deriving (Eq, Show)

data Baseline = BLAlphabetic | BLMiddle | BLCentral | BLHanging | BLTop | BLBottom | BLMath | BLIdeographic
  deriving (Eq, Show)

data FontStyle = FontNormal | FontItalic | FontOblique
  deriving (Eq, Show)

-- | A CSS filter function (the shorthand forms of @filter@).
data CssFilterFn
  = CFBlur !Double
  | CFDropShadow !Double !Double !Double !ColorSpec   -- ^ dx dy blur colour
  | CFMatrix [Double]                                 -- ^ A colour matrix (grayscale, sepia, …).
  | CFOpacity !Double
  deriving (Eq, Show)

data FilterSpec
  = FSNone
  | FSList [Either Text CssFilterFn]     -- ^ url(#id) references and functions, applied in order.
  deriving (Eq, Show)

-- | A computed style. Lengths are already in user units.
data Style = Style
  { stFill         :: !PaintSpec
  , stFillOpacity  :: !Double
  , stFillRule     :: !FillRule
  , stStroke       :: !PaintSpec
  , stStrokeOpacity :: !Double
  , stStrokeWidth  :: !Double
  , stCap          :: !LineCap
  , stJoin         :: !LineJoin
  , stMiter        :: !Double
  , stDash         :: [Double]
  , stDashOffset   :: !Double
  , stColor        :: !Color
  , stVisible      :: !Bool
  , stDisplayNone  :: !Bool          -- not inherited
  , stOpacity      :: !Double        -- not inherited
  , stClipRule     :: !FillRule
  , stClipPath     :: !(Maybe Text)  -- not inherited
  , stMask         :: !(Maybe Text)  -- not inherited
  , stFilter       :: !FilterSpec    -- not inherited
  , stMarkerStart  :: !(Maybe Text)
  , stMarkerMid    :: !(Maybe Text)
  , stMarkerEnd    :: !(Maybe Text)
  , stFontFamily   :: [Text]
  , stFontSize     :: !Double
  , stFontWeight   :: !Int
  , stFontStyle    :: !FontStyle
  , stAnchor       :: !Anchor
  , stLetterSp     :: !Double
  , stWordSp       :: !Double
  , stPreserve     :: !Bool          -- xml:space / white-space
  , stStopColor    :: !ColorSpec     -- not inherited
  , stStopOpacity  :: !Double        -- not inherited
  , stFloodColor   :: !ColorSpec     -- not inherited
  , stFloodOpacity :: !Double        -- not inherited
  , stOverflowVis  :: !Bool          -- not inherited
  , stPixelated    :: !Bool
  , stPaintOrder   :: [PaintPart]
  , stNonScaling   :: !Bool          -- not inherited
  , stMaskAlpha    :: !Bool          -- not inherited
  , stBlend        :: !Blend         -- not inherited
  , stIsolate      :: !Bool          -- not inherited
  , stLinearRGB    :: !Bool          -- color-interpolation-filters
  , stBaseline     :: !Baseline      -- not inherited (dominant/alignment-baseline)
  , stBaselineShift :: !Double       -- not inherited; y-down offset in user units
  , stDecoration   :: [Text]         -- not inherited (propagates to descendants' text)
  , stVp           :: !(Double, Double)   -- the viewport this element's lengths use
  , stInnerVp      :: !(Double, Double)   -- the viewport its children use
  } deriving (Show)

-- | What the root element inherits: CSS initial values, the caller's
-- default colour, font size and viewport.
rootStyle :: Color -> Double -> (Double, Double) -> Style
rootStyle col fs vp = Style
  { stFill = PSColor (CSColor black), stFillOpacity = 1, stFillRule = NonZero
  , stStroke = PSNone, stStrokeOpacity = 1, stStrokeWidth = 1
  , stCap = CapButt, stJoin = JoinMiter, stMiter = 4, stDash = [], stDashOffset = 0
  , stColor = col, stVisible = True, stDisplayNone = False, stOpacity = 1
  , stClipRule = NonZero, stClipPath = Nothing, stMask = Nothing, stFilter = FSNone
  , stMarkerStart = Nothing, stMarkerMid = Nothing, stMarkerEnd = Nothing
  , stFontFamily = [], stFontSize = fs, stFontWeight = 400, stFontStyle = FontNormal
  , stAnchor = AnchorStart, stLetterSp = 0, stWordSp = 0, stPreserve = False
  , stStopColor = CSColor black, stStopOpacity = 1
  , stFloodColor = CSColor black, stFloodOpacity = 1, stOverflowVis = False
  , stPixelated = False, stPaintOrder = [PPFill, PPStroke, PPMarkers]
  , stNonScaling = False, stMaskAlpha = False, stBlend = BlendNormal, stIsolate = False
  , stLinearRGB = True, stBaseline = BLAlphabetic, stBaselineShift = 0, stDecoration = []
  , stVp = vp, stInnerVp = vp }

-- | Reset the non-inherited properties to their initial values.
resetNonInherited :: Style -> Style
resetNonInherited p = p
  { stDisplayNone = False, stOpacity = 1, stClipPath = Nothing, stMask = Nothing
  , stFilter = FSNone, stStopColor = CSColor black, stStopOpacity = 1
  , stFloodColor = CSColor black, stFloodOpacity = 1, stOverflowVis = False
  , stNonScaling = False, stMaskAlpha = False, stBlend = BlendNormal, stIsolate = False
  , stBaseline = BLAlphabetic, stBaselineShift = 0
  , stVp = stInnerVp p }

-- | The computed style of element @i@ when it inherits from @par@.
computeStyle :: Doc -> Style -> Int -> Style
computeStyle doc par i =
  let e = elOf doc i
      spec = dSpec doc ! i
      base0 = resetNonInherited par
      base = base0 { stPreserve = case attr e "xml:space" of
                                    Just "preserve" -> True
                                    Just "default"  -> False
                                    _               -> stPreserve par }
      -- font-size first: every other em length depends on it.
      withFs = applyNamed par base spec "font-size"
      s1 = foldl' (\s k -> applyNamed par s spec k) withFs
             [ k | k <- M.keys spec, k /= "font-size" ]
      inner = if elName e == "svg" then svgInnerVp s1 e else stVp s1
  in s1 { stInnerVp = inner }
  where
    applyNamed p s spec k = case M.lookup k spec of
      Nothing -> s
      Just vs -> fromMaybe s (firstJust [ applyProp p s k v | v <- vs ])
    firstJust xs = case [ x | Just x <- xs ] of (x : _) -> Just x; [] -> Nothing

-- The viewport a nested (or root) <svg> establishes for its children.
svgInnerVp :: Style -> El -> (Double, Double)
svgInnerVp s e = case attr e "viewBox" >>= vb4 of
  Just (_, _, w, h) | w > 0 && h > 0 -> (w, h)
  _ -> ( fromMaybe (fst (stVp s)) (lenAttr s AxX e "width")
       , fromMaybe (snd (stVp s)) (lenAttr s AxY e "height") )
  where vb4 t = case parseNumberList t of
                  [a, b, c, d] -> Just (a, b, c, d)
                  _ -> Nothing

-- | The in-place style of an element (its document position).
inPlaceStyle :: Doc -> Int -> Style
inPlaceStyle d i = dInPlace d ! i

-- One declaration. 'Nothing' = invalid (the caller tries the next one).
applyProp :: Style -> Style -> Text -> Text -> Maybe Style
applyProp par s name v0
  | kw == "inherit" = Just (copyProp name par s)
  | kw == "initial" = Just (copyProp name initialS s)
  | kw == "unset"   = Just (if inheritedProp name then copyProp name par s else copyProp name initialS s)
  | otherwise = case name of
      "fill"              -> (\p -> s { stFill = p }) <$> parsePaint v
      "stroke"            -> (\p -> s { stStroke = p }) <$> parsePaint v
      "fill-opacity"      -> (\o -> s { stFillOpacity = o }) <$> opac
      "stroke-opacity"    -> (\o -> s { stStrokeOpacity = o }) <$> opac
      "opacity"           -> (\o -> s { stOpacity = o }) <$> opac
      "stop-opacity"      -> (\o -> s { stStopOpacity = o }) <$> opac
      "flood-opacity"     -> (\o -> s { stFloodOpacity = o }) <$> opac
      "fill-rule"         -> (\r -> s { stFillRule = r }) <$> rule
      "clip-rule"         -> (\r -> s { stClipRule = r }) <$> rule
      "stroke-width"      -> len AxD >>= \w -> if w < 0 then Nothing else Just s { stStrokeWidth = w }
      "stroke-linecap"    -> (\c -> s { stCap = c }) <$> case kw of
                               "butt" -> Just CapButt; "round" -> Just CapRound; "square" -> Just CapSquare; _ -> Nothing
      "stroke-linejoin"   -> (\j -> s { stJoin = j }) <$> case kw of
                               "miter" -> Just JoinMiter; "miter-clip" -> Just JoinMiter; "arcs" -> Just JoinMiter
                               "round" -> Just JoinRound; "bevel" -> Just JoinBevel; _ -> Nothing
      "stroke-miterlimit" -> case parseNumber v of
                               Just (m, r) | T.null (T.strip r), m >= 1 -> Just s { stMiter = m }
                               _ -> Nothing
      "stroke-dasharray"  -> if kw == "none" then Just s { stDash = [] } else
                               (\ds -> s { stDash = ds }) <$> dashes
      "stroke-dashoffset" -> (\o -> s { stDashOffset = o }) <$> len AxD
      "color"             -> case parseColor v of
                               Just (CSColor c) -> Just s { stColor = c }
                               Just CSCurrent   -> Just s { stColor = stColor par }
                               Nothing          -> Nothing
      "display"           -> Just s { stDisplayNone = kw == "none" }
      "visibility"        -> case kw of
                               "visible" -> Just s { stVisible = True }
                               "hidden" -> Just s { stVisible = False }
                               "collapse" -> Just s { stVisible = False }
                               _ -> Nothing
      "clip-path"         -> if kw == "none" then Just s { stClipPath = Nothing }
                             else (\(r, _) -> s { stClipPath = Just r }) <$> urlRef v
      "mask"              -> if kw == "none" then Just s { stMask = Nothing }
                             else (\(r, _) -> s { stMask = Just r }) <$> urlRef v
      "marker-start"      -> (\m -> s { stMarkerStart = m }) <$> marker
      "marker-mid"        -> (\m -> s { stMarkerMid = m }) <$> marker
      "marker-end"        -> (\m -> s { stMarkerEnd = m }) <$> marker
      "filter"            -> if kw == "none" then Just s { stFilter = FSNone }
                             else (\f -> s { stFilter = f }) <$> parseFilterList v
      "font-family"       -> let fs = fontFamilies v in if null fs then Nothing else Just s { stFontFamily = fs }
      "font-size"         -> fontSize >>= \fs -> if fs < 0 then Nothing else Just s { stFontSize = fs }
      "font-weight"       -> (\w -> s { stFontWeight = w }) <$> weight
      "font-style"        -> (\f -> s { stFontStyle = f }) <$> case kw of
                               "normal" -> Just FontNormal; "italic" -> Just FontItalic
                               _ | "oblique" `T.isPrefixOf` kw -> Just FontOblique
                                 | otherwise -> Nothing
      "text-anchor"       -> (\a -> s { stAnchor = a }) <$> case kw of
                               "start" -> Just AnchorStart; "middle" -> Just AnchorMiddle; "end" -> Just AnchorEnd
                               _ -> Nothing
      "letter-spacing"    -> if kw == "normal" then Just s { stLetterSp = 0 } else (\l -> s { stLetterSp = l }) <$> len AxX
      "word-spacing"      -> if kw == "normal" then Just s { stWordSp = 0 } else (\l -> s { stWordSp = l }) <$> len AxX
      "stop-color"        -> (\c -> s { stStopColor = c }) <$> parseColor v
      "flood-color"       -> (\c -> s { stFloodColor = c }) <$> parseColor v
      "overflow"          -> Just s { stOverflowVis = kw == "visible" || kw == "auto" }
      "image-rendering"   -> Just s { stPixelated = kw `elem` ["optimizespeed", "pixelated", "crisp-edges", "-moz-crisp-edges"] }
      "paint-order"       -> (\o -> s { stPaintOrder = o }) <$> paintOrder
      "vector-effect"     -> Just s { stNonScaling = kw == "non-scaling-stroke" }
      "mask-type"         -> case kw of
                               "alpha" -> Just s { stMaskAlpha = True }
                               "luminance" -> Just s { stMaskAlpha = False }
                               _ -> Nothing
      "mix-blend-mode"    -> (\b -> s { stBlend = b }) <$> blendMode kw
      "isolation"         -> Just s { stIsolate = kw == "isolate" }
      "color-interpolation-filters" -> case kw of
                               "srgb" -> Just s { stLinearRGB = False }
                               "linearrgb" -> Just s { stLinearRGB = True }
                               "auto" -> Just s { stLinearRGB = True }
                               _ -> Nothing
      "dominant-baseline" -> baseline
      "alignment-baseline" -> baseline
      "baseline-shift"    -> case kw of
                               "baseline" -> Just s { stBaselineShift = 0 }
                               "sub"   -> Just s { stBaselineShift = stBaselineShift par + 0.2 * stFontSize s }
                               "super" -> Just s { stBaselineShift = stBaselineShift par - 0.4 * stFontSize s }
                               _ -> (\l -> s { stBaselineShift = stBaselineShift par - l })
                                      <$> (parseLength v >>= \l -> Just (case lenUnit l of
                                             UPct -> lenVal l / 100 * stFontSize s
                                             _ -> resolveLen (stFontSize s) (stVp s) AxY l))
      "text-decoration"   -> Just s { stDecoration = if kw == "none" then [] else
                                        filter (`elem` ["underline", "overline", "line-through"]) (T.words kw) }
      "white-space"       -> Just s { stPreserve = kw `elem` ["pre", "pre-wrap", "break-spaces"] }
      _ -> Just s
  where
    v = T.strip v0
    kw = T.toLower v
    initialS = rootStyle black 16 (stVp s)
    opac = numOrPct v >>= \o -> Just (max 0 (min 1 o))
    rule = case kw of "nonzero" -> Just NonZero; "evenodd" -> Just EvenOdd; _ -> Nothing
    len ax = resolveLen (stFontSize s) (stVp s) ax <$> parseLength v
    marker = if kw == "none" then Just Nothing else (\(r, _) -> Just r) <$> urlRef v
    dashes =
      let parts = filter (not . T.null) (T.split (\c -> c == ',' || isSpace c) v)
      in mapM (\p -> resolveLen (stFontSize s) (stVp s) AxD <$> parseLength p) parts >>= \ds ->
           if any (< 0) ds then Nothing else Just ds
    fontSize = case kw of
      "xx-small" -> Just 9; "x-small" -> Just 10; "small" -> Just 13; "medium" -> Just 16
      "large" -> Just 18; "x-large" -> Just 24; "xx-large" -> Just 32; "xxx-large" -> Just 48
      "larger" -> Just (stFontSize par * 1.2); "smaller" -> Just (stFontSize par / 1.2)
      _ -> parseLength v >>= \l -> Just $ case lenUnit l of
             UPct -> lenVal l / 100 * stFontSize par
             UEm  -> lenVal l * stFontSize par
             UEx  -> lenVal l * stFontSize par / 2
             _    -> resolveLen (stFontSize par) (stVp s) AxY l
    weight = case kw of
      "normal" -> Just 400; "bold" -> Just 700
      "bolder" -> Just (let w = stFontWeight par in if w < 350 then 400 else if w < 550 then 700 else 900)
      "lighter" -> Just (let w = stFontWeight par in if w < 100 then w else if w < 550 then 100 else if w < 750 then 400 else 700)
      _ -> case parseNumber v of
             Just (w, r) | T.null r, w >= 1, w <= 1000 -> Just (round w)
             _ -> Nothing
    paintOrder
      | kw == "normal" = Just [PPFill, PPStroke, PPMarkers]
      | otherwise =
          let toks = map tok (T.words kw)
              tok w = case w of "fill" -> Just PPFill; "stroke" -> Just PPStroke; "markers" -> Just PPMarkers; _ -> Nothing
          in if any (== Nothing) toks || null toks then Nothing
             else let given = [ t | Just t <- toks ]
                  in Just (given ++ [ p | p <- [PPFill, PPStroke, PPMarkers], p `notElem` given ])
    baseline = Just s { stBaseline = case kw of
      "middle" -> BLMiddle; "central" -> BLCentral; "hanging" -> BLHanging
      "text-before-edge" -> BLTop; "text-top" -> BLTop; "before-edge" -> BLTop
      "text-after-edge" -> BLBottom; "text-bottom" -> BLBottom; "after-edge" -> BLBottom
      "mathematical" -> BLMath; "ideographic" -> BLIdeographic
      _ -> BLAlphabetic }

inheritedProp :: Text -> Bool
inheritedProp n = n `notElem`
  [ "display", "opacity", "clip-path", "mask", "filter", "stop-color", "stop-opacity"
  , "flood-color", "flood-opacity", "overflow", "vector-effect", "mask-type"
  , "mix-blend-mode", "isolation", "dominant-baseline", "alignment-baseline"
  , "baseline-shift", "text-decoration" ]

-- Copy one property's value from @from@ into @to@.
copyProp :: Text -> Style -> Style -> Style
copyProp n from to = case n of
  "fill" -> to { stFill = stFill from }
  "stroke" -> to { stStroke = stStroke from }
  "fill-opacity" -> to { stFillOpacity = stFillOpacity from }
  "stroke-opacity" -> to { stStrokeOpacity = stStrokeOpacity from }
  "opacity" -> to { stOpacity = stOpacity from }
  "stop-opacity" -> to { stStopOpacity = stStopOpacity from }
  "flood-opacity" -> to { stFloodOpacity = stFloodOpacity from }
  "fill-rule" -> to { stFillRule = stFillRule from }
  "clip-rule" -> to { stClipRule = stClipRule from }
  "stroke-width" -> to { stStrokeWidth = stStrokeWidth from }
  "stroke-linecap" -> to { stCap = stCap from }
  "stroke-linejoin" -> to { stJoin = stJoin from }
  "stroke-miterlimit" -> to { stMiter = stMiter from }
  "stroke-dasharray" -> to { stDash = stDash from }
  "stroke-dashoffset" -> to { stDashOffset = stDashOffset from }
  "color" -> to { stColor = stColor from }
  "display" -> to { stDisplayNone = stDisplayNone from }
  "visibility" -> to { stVisible = stVisible from }
  "clip-path" -> to { stClipPath = stClipPath from }
  "mask" -> to { stMask = stMask from }
  "marker-start" -> to { stMarkerStart = stMarkerStart from }
  "marker-mid" -> to { stMarkerMid = stMarkerMid from }
  "marker-end" -> to { stMarkerEnd = stMarkerEnd from }
  "filter" -> to { stFilter = stFilter from }
  "font-family" -> to { stFontFamily = stFontFamily from }
  "font-size" -> to { stFontSize = stFontSize from }
  "font-weight" -> to { stFontWeight = stFontWeight from }
  "font-style" -> to { stFontStyle = stFontStyle from }
  "text-anchor" -> to { stAnchor = stAnchor from }
  "letter-spacing" -> to { stLetterSp = stLetterSp from }
  "word-spacing" -> to { stWordSp = stWordSp from }
  "stop-color" -> to { stStopColor = stStopColor from }
  "flood-color" -> to { stFloodColor = stFloodColor from }
  "overflow" -> to { stOverflowVis = stOverflowVis from }
  "image-rendering" -> to { stPixelated = stPixelated from }
  "paint-order" -> to { stPaintOrder = stPaintOrder from }
  "vector-effect" -> to { stNonScaling = stNonScaling from }
  "mask-type" -> to { stMaskAlpha = stMaskAlpha from }
  "mix-blend-mode" -> to { stBlend = stBlend from }
  "isolation" -> to { stIsolate = stIsolate from }
  "color-interpolation-filters" -> to { stLinearRGB = stLinearRGB from }
  "dominant-baseline" -> to { stBaseline = stBaseline from }
  "alignment-baseline" -> to { stBaseline = stBaseline from }
  "baseline-shift" -> to { stBaselineShift = stBaselineShift from }
  "text-decoration" -> to { stDecoration = stDecoration from }
  "white-space" -> to { stPreserve = stPreserve from }
  _ -> to

-- | A paint: @none@, a colour, @url(#id) [fallback]@, @context-fill@,
-- @context-stroke@.
parsePaint :: Text -> Maybe PaintSpec
parsePaint v
  | kw == "none" = Just PSNone
  | kw == "context-fill" = Just PSContextFill
  | kw == "context-stroke" = Just PSContextStroke
  | Just (r, rest) <- urlRef v =
      if T.null rest then Just (PSUrl r Nothing)
      else Just (PSUrl r (parsePaint rest))
  | otherwise = PSColor <$> parseColor v
  where kw = T.toLower (T.strip v)

-- | @font-family@: a comma list of quoted strings or identifier sequences.
fontFamilies :: Text -> [Text]
fontFamilies = filter (not . T.null) . map clean . T.splitOn ","
  where clean = T.unwords . T.words . T.dropAround (\c -> c == '"' || c == '\'' || isSpace c)

-- | A @filter@ value: url() references and CSS filter functions.
parseFilterList :: Text -> Maybe FilterSpec
parseFilterList v0 = FSList <$> go (T.strip v0) (0 :: Int)
  where
    go t n
      | T.null t = Just []
      | n > 32 = Just []
      | otherwise = do
          let (fn, r0) = T.break (== '(') t
              fname = T.toLower (T.strip fn)
          r1 <- T.stripPrefix "(" r0
          let (inner, r2) = parenBody r1
          rest <- T.stripPrefix ")" r2
          item <- if fname == "url"
                    then (\(i, _) -> Left i) <$> urlRef (T.concat ["url(", inner, ")"])
                    else Right <$> cssFn fname (T.strip inner)
          (item :) <$> go (T.strip rest) (n + 1)
    parenBody t = let (a, b) = T.span (/= ')') t in (a, b)
    amount d s = if T.null s then Just d else case parseNumber s of
      Just (x, r) | T.strip r == "%" -> Just (x / 100)
                  | T.null (T.strip r) -> Just x
      _ -> Nothing
    lenPx s = resolveLen 16 (0, 0) AxX <$> parseLength s
    cssFn name inner = case name of
      "blur" -> (\b -> CFBlur (max 0 b)) <$> (if T.null inner then Just 0 else lenPx inner)
      "opacity" -> (\a -> CFOpacity (clamp a)) <$> amount 1 inner
      "grayscale" -> (\a -> CFMatrix (saturateM (1 - clamp a))) <$> amount 1 inner
      "saturate" -> (\a -> CFMatrix (saturateM (max 0 a))) <$> amount 1 inner
      "sepia" -> (\a -> CFMatrix (sepiaM (clamp a))) <$> amount 1 inner
      "hue-rotate" -> (\a -> CFMatrix (hueRotateM a)) <$> (if T.null inner then Just 0 else parseAngle inner)
      "invert" -> (\a -> CFMatrix (linM (1 - 2 * clamp a) (clamp a))) <$> amount 1 inner
      "brightness" -> (\a -> CFMatrix (linM (max 0 a) 0)) <$> amount 1 inner
      "contrast" -> (\a -> CFMatrix (linM (max 0 a) (0.5 - 0.5 * max 0 a))) <$> amount 1 inner
      "drop-shadow" ->
        -- Lengths and an optional colour in either order.
        let toks = colorAwareWords inner
            (cols, lens) = foldr (\w (cs, ls) -> case lenPx w of
                                     Just l -> (cs, l : ls)
                                     Nothing -> (w : cs, ls)) ([], []) toks
            col = case cols of
              [] -> Just CSCurrent
              [c] -> parseColor c
              _ -> Nothing
        in case (lens, col) of
             ([dx, dy], Just c) -> Just (CFDropShadow dx dy 0 c)
             ([dx, dy, b], Just c) -> Just (CFDropShadow dx dy (max 0 b) c)
             _ -> Nothing
      _ -> Nothing
    clamp = max 0 . min 1
    -- Split on spaces outside parentheses (so "rgb(0, 0, 0)" stays one word).
    colorAwareWords t = filter (not . T.null) (map T.strip (splitOutside t))
    splitOutside t = go2 (0 :: Int) [] [] (T.unpack t)
      where go2 _ cur acc [] = reverse (T.pack (reverse cur) : acc)
            go2 d cur acc (c : cs)
              | c == '(' = go2 (d + 1) (c : cur) acc cs
              | c == ')' = go2 (max 0 (d - 1)) (c : cur) acc cs
              | isSpace c && d == 0 = go2 d [] (T.pack (reverse cur) : acc) cs
              | otherwise = go2 d (c : cur) acc cs

-- Colour matrices (row-major 4×5) for the CSS filter functions; also used
-- by feColorMatrix's named types.
saturateM :: Double -> [Double]
saturateM s =
  [ 0.213 + 0.787 * s, 0.715 - 0.715 * s, 0.072 - 0.072 * s, 0, 0
  , 0.213 - 0.213 * s, 0.715 + 0.285 * s, 0.072 - 0.072 * s, 0, 0
  , 0.213 - 0.213 * s, 0.715 - 0.715 * s, 0.072 + 0.928 * s, 0, 0
  , 0, 0, 0, 1, 0 ]

hueRotateM :: Double -> [Double]
hueRotateM deg =
  let a = deg * pi / 180; c = cos a; s = sin a
  in [ 0.213 + c * 0.787 - s * 0.213, 0.715 - c * 0.715 - s * 0.715, 0.072 - c * 0.072 + s * 0.928, 0, 0
     , 0.213 - c * 0.213 + s * 0.143, 0.715 + c * 0.285 + s * 0.140, 0.072 - c * 0.072 - s * 0.283, 0, 0
     , 0.213 - c * 0.213 - s * 0.787, 0.715 - c * 0.715 + s * 0.715, 0.072 + c * 0.928 + s * 0.072, 0, 0
     , 0, 0, 0, 1, 0 ]

sepiaM :: Double -> [Double]
sepiaM a =
  let k = 1 - a
  in [ 0.393 + 0.607 * k, 0.769 - 0.769 * k, 0.189 - 0.189 * k, 0, 0
     , 0.349 - 0.349 * k, 0.686 + 0.314 * k, 0.168 - 0.168 * k, 0, 0
     , 0.272 - 0.272 * k, 0.534 - 0.534 * k, 0.131 + 0.869 * k, 0, 0
     , 0, 0, 0, 1, 0 ]

-- Per-channel @slope * c + intercept@ on RGB, alpha untouched.
linM :: Double -> Double -> [Double]
linM m b = [ m, 0, 0, 0, b,  0, m, 0, 0, b,  0, 0, m, 0, b,  0, 0, 0, 1, 0 ]

-- | A CSS blend-mode keyword (lower case).
blendMode :: Text -> Maybe Blend
blendMode k = case k of
  "normal" -> Just BlendNormal; "multiply" -> Just BlendMultiply; "screen" -> Just BlendScreen
  "overlay" -> Just BlendOverlay; "darken" -> Just BlendDarken; "lighten" -> Just BlendLighten
  "color-dodge" -> Just BlendColorDodge; "color-burn" -> Just BlendColorBurn
  "hard-light" -> Just BlendHardLight; "soft-light" -> Just BlendSoftLight
  "difference" -> Just BlendDifference; "exclusion" -> Just BlendExclusion
  "hue" -> Just BlendHue; "saturation" -> Just BlendSaturation; "color" -> Just BlendColor
  "luminosity" -> Just BlendLuminosity
  _ -> Nothing
