{-# LANGUAGE BangPatterns #-}
-- | SVG documents to "Cmedit.Scene" display lists (plan 0034, workstream D).
--
-- 'parseSvg' reads static SVG 1.1 plus the SVG 2 and CSS features that real
-- files use, and produces a 'Scene' whose bounds are the root viewport in CSS
-- px. Nothing here knows about pixels: the scene is drawn by
-- "Cmedit.Raster" at whatever size and crop the viewer asks for, which is
-- what lets a zoomed SVG re-rasterise sharply.
--
-- The work is split three ways: "Cmedit.SvgPath" (number, path-data and
-- transform micro-syntaxes), "Cmedit.SvgCss" (style sheets, selectors,
-- colours) and "Cmedit.SvgTree" (namespaces, the element tree, the cascade
-- and computed styles). This module is the renderer: it walks the tree and
-- emits commands. Decisions worth knowing before changing it:
--
-- __Every element renders to commands in its parent's space plus a
-- bounding box.__ The box is the SVG /object bounding box/ (geometry only:
-- no stroke, no markers, clipping ignored) in the parent's coordinates, and
-- it is computed compositionally on the way back up, so @objectBoundingBox@
-- units on a group's clip path, mask, filter or gradient need no second walk.
-- An element's own transform wraps its commands in a 'CTransform'; its
-- effects nest inside that, in the order SVG composites them: filter
-- innermost, then clip, then mask and opacity (one group), so a blurred
-- element is clipped after blurring.
--
-- __References are instanced, never resolved by copying the tree.__ A
-- @\<use\>@, a pattern tile, a marker, a mask or a clip path renders the
-- referenced element afresh under the right parent style (a @\<use\>@'s
-- content inherits from the @\<use\>@; a marker's, pattern's or mask's from
-- its own ancestors, via the in-place styles of "Cmedit.SvgTree"). Each
-- instancing pushes the element on a stack that is checked for cycles, and is
-- bounded in depth ('maxRefDepth'); a global fuel counter ('maxFuel') bounds
-- the total, so a document built to expand exponentially (nested @\<use\>@
-- fans) stops instead of hanging. A marker's content is rendered once and
-- shared by every vertex through 'CTransform'.
--
-- __Invalid references follow the reference renderer (resvg).__ A
-- @clip-path@ or @mask@ naming nothing is ignored; a @filter@ naming
-- nothing hides the element (SVG 1.1's rule, and resvg's); a paint server
-- naming nothing uses the fallback colour or @none@.
--
-- __Opacity on one leaf is folded, not grouped.__ A solid fill's alpha, a
-- gradient's stop alphas and an image's 'ioOpacity' absorb opacity exactly,
-- so the common translucent icon does not cost an offscreen layer; anything
-- with two parts (fill and stroke) gets the 'CGroup' it needs so overlaps do
-- not double up.
--
-- __Text uses a font the caller supplies.__ 'Face' is a deliberately small
-- interface (outline, advance, kerning, metrics per character) that the
-- integration adapts "Cmedit.Font" to; this module does layout only:
-- @x@/@y@/@dx@/@dy@/@rotate@ lists, chunks and @text-anchor@, letter and word
-- spacing, @xml:space@, baselines, per-character font fallback through the
-- family list, and @textPath@ (each glyph's midpoint on the path, turned to
-- its tangent; glyphs past either end are hidden). With no face the text is
-- omitted and the scene says so.
--
-- What is not modelled, and is said in 'scNotes' when it costs something
-- visible: @foreignObject@, filter primitives outside the
-- contract (turbulence, lighting, morphology, …; the element is drawn
-- unfiltered), @BackgroundImage@ inputs, external (non-fragment) references.
-- Animation elements are ignored, which is SVG's static base value.
module Cmedit.Svg
  ( -- * Environment
    SvgEnv(..)
  , defaultSvgEnv
  , Face(..)
  , FontQuery(..)
  , FontStyle(..)
    -- * Parsing
  , parseSvg
  , parseSvgText
  , svgIntrinsicSize
  , sniffSvg
  , sniffSvgText
    -- * Pre-pass (what the caller must load before 'parseSvgText')
  , svgFontQueries
  , svgExternalRefs
    -- * Bounds
  , maxFuel
  , maxRefDepth
  , maxTextGlyphs
  , maxFilterPrims
  ) where

import Control.Monad (forM)
import Control.Monad.State.Strict
import Data.Array ((!))
import Data.Bits (shiftR, xor, (.&.), shiftL, (.|.))
import qualified Data.Array
import qualified Data.ByteString as BS
import Data.Char (isSpace, isHexDigit, digitToInt, ord)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe, catMaybes, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64, Word8)

import Cmedit.Geom
import Cmedit.Image (Image(..), decodeImage)
import Cmedit.Scene
import Cmedit.SvgCss (ColorSpec(..))
import Cmedit.SvgPath
import Cmedit.SvgTree
import Cmedit.Xml (XmlEvent(..), parseXmlQualified)

------------------------------------------------------------------------------
-- The environment

-- | A font request: the @font-family@ list as written (generic families
-- such as @serif@ and @monospace@ included, unmapped), the numeric weight and
-- the style.
data FontQuery = FontQuery
  { fqFamilies :: [Text]
  , fqWeight   :: !Int
  , fqStyle    :: !FontStyle
  } deriving (Eq, Show)

-- | A font face as the layout needs it. Glyph space is the font's own:
-- units of 'faceUnitsPerEm', y up, origin on the baseline.
data Face = Face
  { faceKey        :: !Int   -- ^ Unique per face (part of the glyph cache keys); negative = do not cache.
  , faceUnitsPerEm :: !Double
  , faceAscent     :: !Double   -- ^ Above the baseline, positive.
  , faceDescent    :: !Double   -- ^ Below the baseline, negative (OpenType convention).
  , faceGlyph      :: Char -> Maybe (Int, Path, Double)
    -- ^ Glyph id (also the cache key base), outline, advance width.
  , faceKern       :: Int -> Int -> Double   -- ^ Pair adjustment in glyph units (0 when none).
  }

-- | What 'parseSvg' needs from its caller.
data SvgEnv = SvgEnv
  { seFonts    :: FontQuery -> Maybe Face
  , seResolve  :: Text -> Maybe BS.ByteString
    -- ^ The bytes of an external image reference (an @href@ that is not a
    -- @data:@ URI), or 'Nothing' when not available.
  , seViewport :: (Double, Double)
    -- ^ The size used when the root has neither width/height nor a viewBox
    -- (and the reference for root percentages without a viewBox).
  , seColor    :: Color    -- ^ The initial @color@, i.e. what @currentColor@ means at the root.
  , seFontSize :: Double   -- ^ The initial @font-size@ (CSS @medium@ is 16).
  }

-- | No fonts, no external resources, a 100×100 fallback viewport, black.
defaultSvgEnv :: SvgEnv
defaultSvgEnv = SvgEnv (const Nothing) (const Nothing) (100, 100) black 16

------------------------------------------------------------------------------
-- Bounds

-- | Total work units for one document: one per element instance plus one
-- per 16 segments of its path, counted /logically/ (a shared sub-scene is
-- charged at every use, see 'memoised'), so it bounds what a consumer
-- walking the scene pays, while sharing bounds the memory. A 200 000-element
-- map fits; a use fan built to expand does not. Exhausting it truncates the
-- drawing (with a note).
maxFuel :: Int
maxFuel = 300000

-- | Deepest nesting of references being instanced (use, pattern, marker,
-- mask, clip path, nested image).
maxRefDepth :: Int
maxRefDepth = 32

-- | Most glyphs one document lays out.
maxTextGlyphs :: Int
maxTextGlyphs = 200000

-- | Most primitives one filter may hold.
maxFilterPrims :: Int
maxFilterPrims = 64

-- Deepest nesting of SVG documents inside <image>.
maxImageDepth :: Int
maxImageDepth = 3

-- Most marker vertices drawn on one path.
maxMarkerVerts :: Int
maxMarkerVerts = 20000

------------------------------------------------------------------------------
-- The render monad

data RS = RS
  { rsFuel   :: !Int
  , rsNotes  :: !(S.Set String)
  , rsGlyphs :: !Int
  , rsOut    :: !Bool        -- ^ Fuel ran out.
  , rsMemo   :: !(M.Map Int ([Cmd], Maybe Rect, Int))
    -- ^ Instanced sub-scenes by content key: commands, bounds, and the fuel
    -- the first rendering cost (see 'memoised').
  }

type R = State RS

note :: String -> R ()
note n = modify' (\s -> s { rsNotes = S.insert n (rsNotes s) })

-- | Spend fuel; 'False' once it is gone.
tick :: Int -> R Bool
tick n = do
  s <- get
  if rsFuel s < n
    then do put s { rsFuel = 0, rsOut = True }; pure False
    else do put s { rsFuel = rsFuel s - n }; pure True

data Ctx = Ctx
  { cxDoc     :: Doc
  , cxEnv     :: SvgEnv
  , cxCtm     :: !Affine         -- ^ Current user space → scene space.
  , cxStack   :: [Int]           -- ^ Elements being instanced (cycle guard).
  , cxImgDep  :: !Int
  , cxCtxFill :: Maybe Paint     -- ^ context-fill inside a marker.
  , cxCtxStroke :: Maybe Paint
  , cxNonScaling :: !Bool       -- ^ The document uses vector-effect (so a sub-scene depends on the CTM).
  }

-- | Render a sub-scene once per content key and share it. A @\<use\>@ fan
-- (ten uses of a group of ten uses of …) then costs one rendering per level
-- in memory, because every instance holds the /same/ command list, and the
-- scene is a DAG under 'CTransform'. Sharing alone would leave the scene's
-- /logical/ size exponential, and anything that walks it (the rasteriser, a
-- 'show') would pay for that, so a hit is charged the fuel its first
-- rendering cost: the logical size stays bounded by 'maxFuel' while the
-- memory is that of the distinct sub-scenes.
memoised :: Int -> R ([Cmd], Maybe Rect) -> R ([Cmd], Maybe Rect)
memoised key body = do
  m <- gets rsMemo
  case M.lookup key m of
    Just (cs, bb, cost) -> do
      ok <- tick cost
      pure (if ok then (cs, bb) else ([], bb))
    Nothing -> do
      f0 <- gets rsFuel
      (cs, bb) <- body
      f1 <- gets rsFuel
      modify' (\st -> st { rsMemo = M.insert key (cs, bb, max 1 (f0 - f1)) (rsMemo st) })
      pure (cs, bb)

-- A key for a sub-scene rendered under a context: everything that can change
-- what it renders to (the inherited style, the context paints for markers,
-- and the CTM when a non-scaling stroke might be inside).
contextKey :: Ctx -> Style -> [Int] -> Int
contextKey cx st extra =
  mixKey ([4, hashString (show st), hashString (show (cxCtxFill cx, cxCtxStroke cx))]
          ++ (if cxNonScaling cx then qdA (cxCtm cx) else []) ++ extra)

hashString :: String -> Int
hashString = fromIntegral . (.&. 0x3fffffffffffffff) . foldl' (\h c -> (h `xor` fromIntegral (ord c)) * 1099511628211) (14695981039346656037 :: Word64)

------------------------------------------------------------------------------
-- Entry points

-- | Does this look like an SVG document (after optional gzip, BOM, XML
-- declaration, comments and doctype)? Cheap: looks for an @\<svg@ start tag
-- in the first few KiB.
sniffSvg :: BS.ByteString -> Bool
sniffSvg bs
  | BS.take 2 bs == BS.pack [0x1f, 0x8b] = case decodeSvgBytes (BS.take (256 * 1024) bs) of
      Right t -> looks t
      Left _  -> False
  | otherwise = case decodeSvgBytes (BS.take 8192 bs) of
      Right t -> looks t
      Left _ -> False
  where
    looks t = case [ q | XStart q _ <- take 64 (parseXmlQualified t) ] of
      (q : _) -> q == "svg" || ":svg" `T.isSuffixOf` q
      []      -> False

-- | The root's intrinsic size in CSS px from its @width@/@height@ and
-- @viewBox@ (a missing dimension follows the viewBox's aspect ratio), or
-- 'Nothing' when the document states none of them (or is not SVG).
svgIntrinsicSize :: BS.ByteString -> Maybe (Double, Double)
svgIntrinsicSize bs = case decodeSvgBytes bs of
  Left _ -> Nothing
  Right t -> case [ as | XStart q as <- take 256 (parseXmlQualified t), q == "svg" || ":svg" `T.isSuffixOf` q ] of
    (as : _) ->
      let m = M.fromList as
      in if any (`M.member` m) ["width", "height", "viewBox"]
           then case rootSize defaultSvgEnv m of
                  (w, h) | w > 0 && h > 0 -> Just (w, h)
                  _ -> Nothing
           else Nothing
    [] -> Nothing

-- | 'sniffSvg' for text already decoded (an editor buffer): is the first
-- element an @\<svg\>@? Looks at the first 8 KiB only, so it is cheap
-- enough to run when a file is opened.
sniffSvgText :: Text -> Bool
sniffSvgText t =
  case [ q | XStart q _ <- take 64 (parseXmlQualified (T.take 8192 t)) ] of
    (q : _) -> q == "svg" || ":svg" `T.isSuffixOf` q
    []      -> False

-- | The font queries this document's text will make, in the order the
-- layout tries them ('fontTries' over every text element's in-place style),
-- deduplicated and capped at 64. 'parseSvgText' is pure, so a caller that
-- loads fonts from disk does it /before/ parsing, for exactly these queries,
-- and hands the result over as 'seFonts'. Text instanced by a @\<use\>@
-- inherits from the use site and may ask for a family not listed here; the
-- generic families are always in the list, so it falls back to one of them.
-- Empty for a document with no text (checked by substring first, so the
-- common icon costs no second tree build).
svgFontQueries :: SvgEnv -> Text -> [FontQuery]
svgFontQueries env txt
  | not (any (`T.isInfixOf` txt) ["<text", ":text"]) = []
  | otherwise = case buildDoc (rootStyle (seColor env) (seFontSize env) (seViewport env)) txt of
      Left _ -> []
      Right doc -> take 64 (dedup S.empty
        [ q | (i, e) <- assocsEls doc, elName e `elem` ["text", "tspan", "textPath"]
            , q <- fontTries (inPlaceStyle doc i) ])
  where
    dedup _ [] = []
    dedup seen (q : qs)
      | key q `S.member` seen = dedup seen qs
      | otherwise = q : dedup (S.insert (key q) seen) qs
    key q = (fqFamilies q, fqWeight q, show (fqStyle q))

-- | The external (non-@data:@, non-fragment) image references in the
-- document, as written, deduplicated and capped at 64. The caller resolves
-- them (relative to the document, local files only) and answers
-- 'seResolve' from what it read.
svgExternalRefs :: Text -> [Text]
svgExternalRefs txt
  | not (any (`T.isInfixOf` txt) ["<image", ":image"]) = []
  | otherwise = case buildDoc (rootStyle black 16 (100, 100)) txt of
      Left _ -> []
      Right doc -> take 64 (S.toList (S.fromList
        [ h | (_, e) <- assocsEls doc, elName e == "image"
            , Just h0 <- [attr e "href"], let h = T.strip h0
            , not (T.null h), not ("data:" `T.isPrefixOf` h), not ("#" `T.isPrefixOf` h) ]))

assocsEls :: Doc -> [(Int, El)]
assocsEls doc = Data.Array.assocs (dEls doc)

-- | Parse an SVG (or gzip-compressed @.svgz@) document into a scene.
-- 'Left' only when the input is not an SVG document at all or has no area;
-- everything wrong /inside/ a document degrades as SVG prescribes.
parseSvg :: SvgEnv -> BS.ByteString -> Either String Scene
parseSvg env bs = parseSvgDepth env 0 bs

-- | 'parseSvg' for a document that is already text (an editor buffer):
-- the same result as encoding it and calling 'parseSvg', without the round
-- trip through bytes.
parseSvgText :: SvgEnv -> Text -> Either String Scene
parseSvgText env = parseSvgTextDepth env 0

parseSvgDepth :: SvgEnv -> Int -> BS.ByteString -> Either String Scene
parseSvgDepth env imgDepth bs = decodeSvgBytes bs >>= parseSvgTextDepth env imgDepth

parseSvgTextDepth :: SvgEnv -> Int -> Text -> Either String Scene
parseSvgTextDepth env imgDepth txt = do
  let root0 = rootStyle (seColor env) (seFontSize env) (seViewport env)
  doc <- buildDoc root0 txt
  let rootEl = elOf doc 0
      (w, h) = rootSize env (elAttrs rootEl)
  if not (w > 0 && h > 0) || isNaN w || isNaN h || isInfinite w || isInfinite h
    then Left "SVG has no area (zero width or height)"
    else do
      let nonScaling = any (M.member "vector-effect") (elemsA (dSpec doc))
          elemsA = foldr (:) []
          cx = Ctx doc env identityA [0] imgDepth Nothing Nothing nonScaling
          (cmds, rs) = runState (renderRoot cx (w, h)) (RS maxFuel S.empty 0 False M.empty)
          notes = S.toList (rsNotes rs)
                  ++ [ "drawing truncated: too complex" | rsOut rs ]
                  ++ [ "some elements dropped: too many" | dDropped doc ]
      Right (Scene (Rect 0 0 w h) cmds notes)

-- The root's size (see 'svgIntrinsicSize').
rootSize :: SvgEnv -> M.Map Text Text -> (Double, Double)
rootSize env m =
  let vb = M.lookup "viewBox" m >>= parseViewBox
      ref = case vb of
        Just (Rect x0 y0 x1 y1) -> (x1 - x0, y1 - y0)
        Nothing -> seViewport env
      dim ax k = M.lookup k m >>= \v -> if T.toLower (T.strip v) == "auto" then Nothing else parseLength v
                   >>= \l -> let r = resolveLen (seFontSize env) ref ax l in if r < 0 then Nothing else Just r
      mw = dim AxX "width"; mh = dim AxY "height"
  in case (mw, mh, vb) of
       (Just w, Just h, _) -> (w, h)
       (Just w, Nothing, Just r) | rectW r > 0 -> (w, w * rectH r / rectW r)
       (Nothing, Just h, Just r) | rectH r > 0 -> (h * rectW r / rectH r, h)
       (Just w, Nothing, _) -> (w, snd (seViewport env))
       (Nothing, Just h, _) -> (fst (seViewport env), h)
       (Nothing, Nothing, Just r) -> (rectW r, rectH r)
       _ -> seViewport env

------------------------------------------------------------------------------
-- Viewports

parseViewBox :: Text -> Maybe Rect
parseViewBox t = case parseNumberList t of
  [x, y, w, h] | w > 0 && h > 0 -> Just (Rect x y (x + w) (y + h))
  _ -> Nothing

-- | preserveAspectRatio: (alignment fractions, or Nothing for @none@; slice).
data PAR = PAR !(Maybe (Double, Double)) !Bool

parsePAR :: Maybe Text -> PAR
parsePAR Nothing = PAR (Just (0.5, 0.5)) False
parsePAR (Just t) =
  let ws = filter (/= "defer") (T.words t)
  in case ws of
       (a : rest) ->
         let slice = rest == ["slice"]
         in case alignOf a of
              Just al -> PAR al slice
              Nothing -> PAR (Just (0.5, 0.5)) False
       [] -> PAR (Just (0.5, 0.5)) False
  where
    alignOf a
      | a == "none" = Just Nothing
      | T.length a == 8, Just x <- frac (T.take 4 a), Just y <- frac (T.drop 4 a) = Just (Just (x, y))
      | otherwise = Nothing
    frac s = case s of
      "xMin" -> Just 0; "xMid" -> Just 0.5; "xMax" -> Just 1
      "YMin" -> Just 0; "YMid" -> Just 0.5; "YMax" -> Just 1
      _ -> Nothing

-- | The transform taking a viewBox onto a @w × h@ viewport at the origin.
viewBoxTransform :: Rect -> PAR -> Double -> Double -> Affine
viewBoxTransform (Rect vx vy vx1 vy1) (PAR al slice) w h =
  let vw = vx1 - vx; vh = vy1 - vy
      sx = w / vw; sy = h / vh
  in case al of
       Nothing -> Affine sx 0 0 sy (-vx * sx) (-vy * sy)
       Just (ax, ay) ->
         let s = if slice then max sx sy else min sx sy
             tx = -vx * s + ax * (w - vw * s)
             ty = -vy * s + ay * (h - vh * s)
         in Affine s 0 0 s tx ty

------------------------------------------------------------------------------
-- The walk

renderRoot :: Ctx -> (Double, Double) -> R [Cmd]
renderRoot cx (w, h) = do
  let doc = cxDoc cx
      e = elOf doc 0
      st0 = inPlaceStyle doc 0
      vb = attr e "viewBox" >>= parseViewBox
      vbM = maybe identityA (\r -> viewBoxTransform r (parsePAR (attr e "preserveAspectRatio")) w h) vb
      t = fromMaybe identityA (attr e "transform" >>= parseTransform)
      m = composeA t vbM
      st = st0 { stInnerVp = maybe (w, h) (\r -> (rectW r, rectH r)) vb, stVp = (w, h) }
  if stDisplayNone st then pure [] else do
    let cx' = cx { cxCtm = m }
    (kids, bb) <- renderKids cx' st (elemKids e)
    out <- effects cx' st bb kids
    pure [ CClip [ClipShape identityA NonZero (rectPath 0 0 w h)] [ CTransform m c | Just c <- [out] ] ]

renderKids :: Ctx -> Style -> [Int] -> R ([Cmd], Maybe Rect)
renderKids cx st ks = do
  rs <- mapM (renderEl cx st) ks
  pure (concatMap fst rs, unionAll (map snd rs))

unionAll :: [Maybe Rect] -> Maybe Rect
unionAll = foldl' u Nothing
  where u Nothing r = r
        u a Nothing = a
        u (Just a) (Just b) = Just (rectUnion a b)

elTransform :: El -> Affine
elTransform e = fromMaybe identityA (attr e "transform" >>= parseTransform)

-- | Render element @i@ inheriting from @par@: commands and bounding box,
-- both in the parent's user space.
renderEl :: Ctx -> Style -> Int -> R ([Cmd], Maybe Rect)
renderEl cx par i = do
  ok <- tick 1
  let doc = cxDoc cx
      e = elOf doc i
      st = computeStyle doc par i
  if not ok || stDisplayNone st then pure ([], Nothing) else
    case elName e of
      "g"       -> withTransform cx st e $ \cx' -> renderKids cx' st (elemKids e)
      "a"       -> withTransform cx st e $ \cx' -> renderKids cx' st (elemKids e)
      "switch"  -> withTransform cx st e $ \cx' -> case filter (switchOk doc) (elemKids e) of
                     (k : _) -> renderEl cx' st k
                     []      -> pure ([], Nothing)
      "svg"     -> withTransform cx st e $ \cx' -> nestedViewport cx' st e e Nothing Nothing
      "use"     -> renderUse cx st e i
      "text"    -> withTransform cx st e $ \cx' -> renderText cx' st i
      "image"   -> withTransform cx st e $ \cx' -> renderImage cx' st e
      "foreignObject" -> do note "foreignObject content not shown"; pure ([], Nothing)
      n | n `elem` shapeNames -> withTransform cx st e $ \cx' -> renderShape cx' st i
        | otherwise -> pure ([], Nothing)

shapeNames :: [Text]
shapeNames = ["path", "rect", "circle", "ellipse", "line", "polyline", "polygon"]

-- The child a <switch> renders: the first graphics element whose
-- conditional attributes hold.
switchOk :: Doc -> Int -> Bool
switchOk doc k =
  let e = elOf doc k
  in elName e `elem` (["g", "svg", "use", "text", "image", "switch", "a", "foreignObject"] ++ shapeNames)
     && maybe True (const False) (attr e "requiredExtensions")
     && maybe True langOk (attr e "systemLanguage")
  where
    langOk t = any (\l -> let l' = T.toLower (T.strip l) in l' == "en" || "en-" `T.isPrefixOf` l')
                   (T.splitOn "," t)

-- Apply an element's transform around its content and effects.
withTransform :: Ctx -> Style -> El -> (Ctx -> R ([Cmd], Maybe Rect)) -> R ([Cmd], Maybe Rect)
withTransform cx st e body = withMatrix cx st (elTransform e) body

withMatrix :: Ctx -> Style -> Affine -> (Ctx -> R ([Cmd], Maybe Rect)) -> R ([Cmd], Maybe Rect)
withMatrix cx st t body
  | isNothingA (invertA t) = pure ([], Nothing)
  | otherwise = do
      let cx' = cx { cxCtm = composeA (cxCtm cx) t }
      (cmds, bb) <- body cx'
      out <- effects cx' st bb cmds
      let bbOut = transformRect t <$> bb
      pure $ case out of
        Nothing -> ([], bbOut)
        Just cs | null cs -> ([], bbOut)
                | t == identityA -> (cs, bbOut)
                | otherwise -> ([CTransform t cs], bbOut)
  where isNothingA = maybe True (const False)

-- | An element's filter, clip path, mask, opacity and blend mode, around
-- its content (in its own user space). 'Nothing' = not rendered.
effects :: Ctx -> Style -> Maybe Rect -> [Cmd] -> R (Maybe [Cmd])
effects _ st _ [] | stFilter st == FSNone = pure (Just [])
effects cx st bb cmds0 = do
  fr <- resolveFilters cx st bb
  case fr of
    Nothing -> pure Nothing
    Just filters -> do
      let filtered = foldl' (\cs f -> [CGroup defaultGroup { goFilter = Just f, goIsolated = True } cs]) cmds0 filters
      cr <- maybe (pure (Just [])) (\r -> resolveClip cx r bb) (stClipPath st)
      case cr of
        Nothing -> pure Nothing
        Just layers -> do
          mr <- maybe (pure (Right Nothing)) (\r -> resolveMask cx r bb) (stMask st)
          case mr of
            Left () -> pure Nothing
            Right msk -> do
              let op = stOpacity st
                  needGroup = isJust msk || stBlend st /= BlendNormal || stIsolate st
                  grouped
                    | needGroup = [CGroup defaultGroup { goOpacity = op, goBlend = stBlend st
                                                       , goMask = msk, goIsolated = True } filtered]
                    | otherwise = applyOpacity op filtered
              pure (Just (foldr applyLayer grouped layers))

data ClipLayer = LShapes [ClipShape] | LMask SoftMask

applyLayer :: ClipLayer -> [Cmd] -> [Cmd]
applyLayer (LShapes ss) cs = [CClip ss cs]
applyLayer (LMask sm) cs = [CGroup defaultGroup { goMask = Just sm, goIsolated = True } cs]

-- | Opacity over commands: folded into a single leaf's paint where that is
-- exact, otherwise an isolated group.
applyOpacity :: Double -> [Cmd] -> [Cmd]
applyOpacity op cs
  | op >= 1 = cs
  | op <= 0 = []
  | otherwise = case cs of
      [c] | Just c' <- fold1 c -> [c']
      _ -> [CGroup defaultGroup { goOpacity = op, goIsolated = True } cs]
  where
    fold1 c = case c of
      CFill m r p path -> (\p' -> CFill m r p' path) <$> fadePaint op p
      CStroke m s p path -> (\p' -> CStroke m s p' path) <$> fadePaint op p
      CGlyphs m p gs -> (\p' -> CGlyphs m p' gs) <$> fadePaint op p
      CImage m img o -> Just (CImage m img o { ioOpacity = ioOpacity o * op })
      CTransform m [x] -> (\x' -> CTransform m [x']) <$> fold1 x
      _ -> Nothing

-- A paint with its alpha scaled, where that is exact.
fadePaint :: Double -> Paint -> Maybe Paint
fadePaint op p = case p of
  PSolid c -> Just (PSolid (fade c))
  PLinear m a b ss sp -> Just (PLinear m a b (map fs ss) sp)
  PRadial m a ra b rb ss sp -> Just (PRadial m a ra b rb (map fs ss) sp)
  _ -> Nothing
  where fade (Color r g b a) = Color r g b (a * op)
        fs (Stop o c) = Stop o (fade c)

------------------------------------------------------------------------------
-- Nested viewports: <svg> and instanced <symbol>

-- | Render the children of @content@ (an svg or symbol) in a new viewport
-- whose geometry comes from @geom@ (the svg itself, or the referencing use),
-- with optional width/height overrides.
nestedViewport :: Ctx -> Style -> El -> El -> Maybe Double -> Maybe Double -> R ([Cmd], Maybe Rect)
nestedViewport cx st geom content mw mh = do
  let x = fromMaybe 0 (lenAttr st AxX geom "x")
      y = fromMaybe 0 (lenAttr st AxY geom "y")
      (vw, vh) = stVp st
      w = fromMaybe (fromMaybe vw (lenAttr st AxX content "width")) mw
      h = fromMaybe (fromMaybe vh (lenAttr st AxY content "height")) mh
      vb = attr content "viewBox" >>= parseViewBox
      vbM = maybe identityA (\r -> viewBoxTransform r (parsePAR (attr content "preserveAspectRatio")) w h) vb
      inner = st { stInnerVp = maybe (w, h) (\r -> (rectW r, rectH r)) vb }
  if w <= 0 || h <= 0 then pure ([], Nothing) else do
    let cx' = cx { cxCtm = cxCtm cx `composeA` translateA x y `composeA` vbM }
    (kids, bb) <- renderKids cx' inner (elemKids content)
    let clipped = if stOverflowVis st then kids
                  else [CClip [ClipShape identityA NonZero (rectPath 0 0 w h)] [CTransform vbM kids]]
        body = if stOverflowVis st then [CTransform vbM kids] else clipped
        m = translateA x y
    pure ( if null kids then [] else [CTransform m body]
         , transformRect (composeA m vbM) <$> bb )

renderUse :: Ctx -> Style -> El -> Int -> R ([Cmd], Maybe Rect)
renderUse cx st e i = do
  let doc = cxDoc cx
      -- A use may not reference itself or an element containing it.
      ancestors k = k : (let p = elParent (elOf doc k) in if p < 0 then [] else ancestors p)
  case attr e "href" >>= refId >>= lookupId doc of
    Nothing -> pure ([], Nothing)
    Just j
      | j `elem` cxStack cx || j `elem` ancestors i || length (cxStack cx) > maxRefDepth -> do
          note "recursive <use> reference ignored"; pure ([], Nothing)
      | otherwise -> do
          let x = fromMaybe 0 (lenAttr st AxX e "x")
              y = fromMaybe 0 (lenAttr st AxY e "y")
              t = elTransform e `composeA` translateA x y
              target = elOf doc j
              cxR = cx { cxStack = j : cxStack cx }
              key = contextKey cxR st ([j] ++ map (maybe (-1) qd . lenAttr st AxX e) ["width", "height"])
          withMatrix cxR st t $ \cx' -> memoised key $ case elName target of
            "symbol" -> do
              let sst = computeStyle doc st j
              if stDisplayNone sst then pure ([], Nothing) else do
                let mw = lenAttr st AxX e "width"
                    mh = lenAttr st AxY e "height"
                (cs, bb) <- nestedViewport cx' sst target target
                              (Just (fromMaybe (fst (stVp st)) mw)) (Just (fromMaybe (snd (stVp st)) mh))
                out <- effects cx' sst bb cs
                pure (fromMaybe [] out, bb)
            "svg" -> do
              let sst = computeStyle doc st j
              if stDisplayNone sst then pure ([], Nothing) else do
                (cs, bb) <- nestedViewport cx' sst target target
                              (lenAttr st AxX e "width") (lenAttr st AxY e "height")
                let tt = elTransform target
                out <- effects cx' sst bb cs
                pure (case out of
                        Just o | not (null o) -> if tt == identityA then o else [CTransform tt o]
                        _ -> [], transformRect tt <$> bb)
            _ -> renderEl cx' st j

------------------------------------------------------------------------------
-- Shapes

-- The geometry of a basic shape as path commands, and whether markers apply.
shapeCmds :: Style -> El -> Maybe ([PCmd], Bool)
shapeCmds st e = case elName e of
  "path" -> Just (parsePathData (fromMaybe "" (attr e "d")), True)
  "rect" -> do
    let x = num AxX "x"; y = num AxY "y"
    w <- lenAttr st AxX e "width"
    h <- lenAttr st AxY e "height"
    if w <= 0 || h <= 0 then Nothing else do
      let rx0 = autoLen AxX "rx"; ry0 = autoLen AxY "ry"
          (rx1, ry1) = case (rx0, ry0) of
            (Just a, Just b) -> (a, b)
            (Just a, Nothing) -> (a, a)
            (Nothing, Just b) -> (b, b)
            _ -> (0, 0)
          rx = max 0 (min (w / 2) rx1); ry = max 0 (min (h / 2) ry1)
      Just (roundRect x y w h rx ry, False)
  "circle" -> do
    r <- lenAttr st AxD e "r"
    if r <= 0 then Nothing else Just (ellipseCmds (num AxX "cx") (num AxY "cy") r r, False)
  "ellipse" -> do
    let rx0 = autoLen AxX "rx"; ry0 = autoLen AxY "ry"
    if negative "rx" || negative "ry" then Nothing else pure ()
    (rx, ry) <- case (rx0, ry0) of
      (Just a, Just b) -> Just (a, b)
      (Just a, Nothing) -> Just (a, a)
      (Nothing, Just b) -> Just (b, b)
      _ -> Nothing
    if rx <= 0 || ry <= 0 then Nothing else Just (ellipseCmds (num AxX "cx") (num AxY "cy") rx ry, False)
  "line" -> Just ([PMove (Pt (num AxX "x1") (num AxY "y1")), PLine (Pt (num AxX "x2") (num AxY "y2"))], True)
  "polyline" -> poly False
  "polygon" -> poly True
  _ -> Nothing
  where
    num ax k = fromMaybe 0 (lenAttr st ax e k)
    negative k = maybe False (< 0) (lenAttr st AxX e k)
    autoLen ax k = case attr e k of
      Just v | T.toLower (T.strip v) == "auto" -> Nothing
      _ -> lenAttr st ax e k >>= \v -> if v < 0 then Nothing else Just v
    poly closed = case parsePoints (fromMaybe "" (attr e "points")) of
      (p : ps) -> Just (PMove p : map PLine ps ++ [ PClose | closed ], True)
      [] -> Nothing

-- Quarter-ellipse Bézier constant.
kappa :: Double
kappa = 0.5522847498307936

ellipseCmds :: Double -> Double -> Double -> Double -> [PCmd]
ellipseCmds cx cy rx ry =
  let kx = kappa * rx; ky = kappa * ry
  in [ PMove (Pt (cx + rx) cy)
     , PCubic (Pt (cx + rx) (cy + ky)) (Pt (cx + kx) (cy + ry)) (Pt cx (cy + ry))
     , PCubic (Pt (cx - kx) (cy + ry)) (Pt (cx - rx) (cy + ky)) (Pt (cx - rx) cy)
     , PCubic (Pt (cx - rx) (cy - ky)) (Pt (cx - kx) (cy - ry)) (Pt cx (cy - ry))
     , PCubic (Pt (cx + kx) (cy - ry)) (Pt (cx + rx) (cy - ky)) (Pt (cx + rx) cy)
     , PClose ]

roundRect :: Double -> Double -> Double -> Double -> Double -> Double -> [PCmd]
roundRect x y w h rx ry
  | rx <= 0 || ry <= 0 =
      [PMove (Pt x y), PLine (Pt (x + w) y), PLine (Pt (x + w) (y + h)), PLine (Pt x (y + h)), PClose]
  | otherwise =
      let kx = kappa * rx; ky = kappa * ry
          x1 = x + w; y1 = y + h
      in [ PMove (Pt (x + rx) y), PLine (Pt (x1 - rx) y)
         , PCubic (Pt (x1 - rx + kx) y) (Pt x1 (y + ry - ky)) (Pt x1 (y + ry))
         , PLine (Pt x1 (y1 - ry))
         , PCubic (Pt x1 (y1 - ry + ky)) (Pt (x1 - rx + kx) y1) (Pt (x1 - rx) y1)
         , PLine (Pt (x + rx) y1)
         , PCubic (Pt (x + rx - kx) y1) (Pt x (y1 - ry + ky)) (Pt x (y1 - ry))
         , PLine (Pt x (y + ry))
         , PCubic (Pt x (y + ry - ky)) (Pt (x + rx - kx) y) (Pt (x + rx) y)
         , PClose ]

-- | A shape's geometry: commands, whether markers apply, the path and its
-- exact bounds. Paths and point lists come from the document's memo
-- ('dGeom'), so every instance of one shares one parsed 'Path'; the basic
-- shapes depend on the style (percentages, em) and are a few segments each.
shapeGeom :: Doc -> Int -> Style -> Maybe ([PCmd], Bool, Path, Maybe Rect)
shapeGeom doc i st = case dGeom doc ! i of
  Just (pcs, p, bb) -> Just (pcs, True, p, bb)
  Nothing -> case shapeCmds st (elOf doc i) of
    Just (pcs, mk) -> let p = cmdsToPath pcs in Just (pcs, mk, p, pathBoundsExact p)
    Nothing -> Nothing

renderShape :: Ctx -> Style -> Int -> R ([Cmd], Maybe Rect)
renderShape cx st i = case shapeGeom (cxDoc cx) i st of
  Nothing -> pure ([], Nothing)
  Just (pcs, markersOk, path, bb) -> do
    _ <- tick (length path `div` 16)
    if null path then pure ([], Nothing) else
      if not (stVisible st) then pure ([], bb) else do
        parts <- forM (stPaintOrder st) $ \part -> case part of
          PPFill -> fillPart cx st path bb
          PPStroke -> strokePart cx st path bb
          PPMarkers -> if markersOk then renderMarkers cx st pcs else pure []
        pure (concat parts, bb)

fillPart :: Ctx -> Style -> Path -> Maybe Rect -> R [Cmd]
fillPart cx st path bb
  | maybe True rectEmpty bb = pure []   -- an open line or a degenerate path covers nothing
  | otherwise = do
      mp <- resolvePaint cx st (stFill st) (stFillOpacity st) bb
      pure $ case mp of
        Nothing -> []
        Just (p, gop) -> applyOpacity gop [CFill identityA (stFillRule st) p path]

strokeStyleOf :: Style -> Maybe StrokeStyle
strokeStyleOf st
  | stStrokeWidth st <= 0 = Nothing
  | otherwise =
      let ds = stDash st
          ds' | null ds || all (== 0) ds = []
              | odd (length ds) = ds ++ ds
              | otherwise = ds
      in Just StrokeStyle { ssWidth = stStrokeWidth st, ssCap = stCap st, ssJoin = stJoin st
                          , ssMiterLimit = stMiter st, ssDash = ds', ssDashPhase = stDashOffset st
                          , ssAdjust = False }

strokePart :: Ctx -> Style -> Path -> Maybe Rect -> R [Cmd]
strokePart cx st path bb = case strokeStyleOf st of
  Nothing -> pure []
  Just ss -> do
    mp <- resolvePaint cx st (stStroke st) (stStrokeOpacity st) bb
    pure $ case mp of
      Nothing -> []
      Just (p, gop)
        | stNonScaling st, Just inv <- invertA (cxCtm cx) ->
            -- Stroke in scene space: the pen is not scaled by any transform.
            let m = cxCtm cx
            in applyOpacity gop [CStroke inv ss (transformPaint m p) (transformPath m path)]
        | otherwise -> applyOpacity gop [CStroke identityA ss p path]

-- | Re-express a paint for a command whose user space is @m@ applied to the
-- old one.
transformPaint :: Affine -> Paint -> Paint
transformPaint m p = case p of
  PSolid _ -> p
  PLinear a s e ss sp -> PLinear (composeA m a) s e ss sp
  PRadial a c0 r0 c1 r1 ss sp -> PRadial (composeA m a) c0 r0 c1 r1 ss sp
  PSampled a img sp -> PSampled (composeA m a) img sp
  PTiling k a cell xs ys cs -> PTiling k (composeA m a) cell xs ys cs

------------------------------------------------------------------------------
-- Paint servers

colorOf :: Style -> ColorSpec -> Color
colorOf st cs = case cs of
  CSColor c -> c
  CSCurrent -> stColor st

withAlpha :: Double -> Color -> Color
withAlpha op (Color r g b a) = Color r g b (a * op)

-- | Resolve a paint: the paint and the group opacity still to apply (1 unless
-- the paint is a pattern with a fill/stroke opacity below 1).
resolvePaint :: Ctx -> Style -> PaintSpec -> Double -> Maybe Rect -> R (Maybe (Paint, Double))
resolvePaint cx st ps op bb = case ps of
  PSNone -> pure Nothing
  PSColor c -> pure (Just (PSolid (withAlpha op (colorOf st c)), 1))
  PSContextFill -> pure (ctxPaint (cxCtxFill cx))
  PSContextStroke -> pure (ctxPaint (cxCtxStroke cx))
  PSUrl ref fb ->
    let doc = cxDoc cx
        fallback = case fb of
          Just f -> resolvePaint cx st f op bb
          Nothing -> pure Nothing
    in case lookupId doc ref of
         Nothing -> fallback
         -- A server that cannot paint this element (no stops, a zero-size
         -- bounding box in objectBoundingBox units, an empty tile) uses the
         -- fallback when there is one, as resvg and librsvg both do.
         Just j -> case elName (elOf doc j) of
           n | n == "linearGradient" || n == "radialGradient" -> gradientPaint cx st j op bb >>= orFallback
             | n == "pattern" -> patternPaint cx st j op bb >>= orFallback
             | otherwise -> fallback
           where orFallback r = maybe fallback (pure . Just) r
  where
    ctxPaint mp = case mp of
      Just p -> Just (maybe (p, op) (\p' -> (p', 1)) (fadePaint op p))
      Nothing -> Nothing

-- The href chain of a paint server (itself first), cycle-free and bounded.
hrefChain :: Doc -> [Text] -> Int -> [Int]
hrefChain doc kinds j0 = go [j0] j0
  where
    go seen j = j : case attr (elOf doc j) "href" >>= refId >>= lookupId doc of
      Just k | k `notElem` seen, length seen < 16, elName (elOf doc k) `elem` kinds -> go (k : seen) k
      _ -> []

chainAttr :: Doc -> [Int] -> Text -> Maybe Text
chainAttr doc ch k = listToMaybe (mapMaybe (\j -> attr (elOf doc j) k) ch)

bboxMatrix :: Rect -> Affine
bboxMatrix r@(Rect x y _ _) = Affine (rectW r) 0 0 (rectH r) x y

-- A coordinate of a paint server, mask, filter or pattern: in
-- objectBoundingBox units a number is a fraction and a percentage is /100;
-- in user space it is a length against the viewport.
unitCoord :: Style -> Bool -> Axis -> Maybe Text -> Double -> Double
unitCoord st obb ax mt dflt = case mt >>= parseLength of
  Nothing -> dflt
  Just l | obb -> case lenUnit l of
                    UPct -> lenVal l / 100
                    _ -> resolveLen (stFontSize st) (stVp st) ax l
         | otherwise -> resolveLen (stFontSize st) (stVp st) ax l

gradientPaint :: Ctx -> Style -> Int -> Double -> Maybe Rect -> R (Maybe (Paint, Double))
gradientPaint cx st j op bb = do
  let doc = cxDoc cx
      kinds = ["linearGradient", "radialGradient"]
      ch = hrefChain doc kinds j
      kind = elName (elOf doc j)
      same = [ k | k <- ch, elName (elOf doc k) == kind ]
      ga = chainAttr doc ch
      sa = chainAttr doc same
      obb = ga "gradientUnits" /= Just "userSpaceOnUse"
      gt = fromMaybe identityA (ga "gradientTransform" >>= parseTransform)
      spread = case ga "spreadMethod" of
        Just "reflect" -> SpreadReflect
        Just "repeat" -> SpreadRepeat
        _ -> SpreadPad
      stopsOwner = listToMaybe [ k | k <- ch, any (\s -> elName (elOf doc s) == "stop") (elemKids (elOf doc k)) ]
      stops = case stopsOwner of
        Nothing -> []
        Just k -> mkStops [ s | s <- elemKids (elOf doc k), elName (elOf doc s) == "stop" ]
      mkStops ss = reverse (snd (foldl' step (0, []) ss))
      step (prev, acc) s =
        let se = elOf doc s
            sst = inPlaceStyle doc s
            off = max prev (max 0 (min 1 (fromMaybe 0 (attr se "offset" >>= numOrPct))))
            c = withAlpha (stStopOpacity sst * op) (colorOf sst (stStopColor sst))
        in (off, Stop off c : acc)
      coord ax k d = unitCoord st obb ax (sa k) d
      bbOk = case bb of
        Just r -> rectW r > 0 && rectH r > 0
        Nothing -> False
  case stops of
    [] -> pure Nothing
    [Stop _ c] -> pure (Just (PSolid c, 1))
    _ | obb && not bbOk -> pure Nothing
      | otherwise -> do
          let base = if obb then maybe identityA bboxMatrix bb else identityA
              m = composeA base gt
              Stop _ lastC = last stops
          if kind == "linearGradient"
            then do
              let x1 = coord AxX "x1" 0; y1 = coord AxY "y1" 0
                  x2 = coord AxX "x2" (if obb then 1 else fst (stVp st)); y2 = coord AxY "y2" 0
              pure $ if x1 == x2 && y1 == y2 then Just (PSolid lastC, 1)
                     else Just (PLinear m (Pt x1 y1) (Pt x2 y2) stops spread, 1)
            else do
              let half ax = if obb then 0.5 else 0.5 * (case ax of AxX -> fst (stVp st); _ -> snd (stVp st))
                  cxv = coord AxX "cx" (half AxX); cyv = coord AxY "cy" (half AxY)
                  r = coord AxD "r" (if obb then 0.5 else 0.5 * diag)
                  diag = let (w, h) = stVp st in sqrt ((w * w + h * h) / 2)
                  fx = coord AxX "fx" cxv; fy = coord AxY "fy" cyv
                  fr = coord AxD "fr" 0
              pure $ if r <= 0 then Just (PSolid lastC, 1)
                     else Just (PRadial m (Pt fx fy) (max 0 fr) (Pt cxv cyv) r stops spread, 1)

-- A 64-bit mix of integers, for content-derived cache keys.
mixKey :: [Int] -> Int
mixKey = fromIntegral . (.&. 0x3fffffffffffffff) . foldl' (\h x -> splitmix (h `xor` fromIntegral x)) (0x9e3779b97f4a7c15 :: Word64)
  where
    splitmix z0 =
      let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
          z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
      in z2 `xor` (z2 `shiftR` 31)

qd :: Double -> Int
qd x | isNaN x || isInfinite x = 0
     | otherwise = round (max (-1e12) (min 1e12 x) * 4096)

qdA :: Affine -> [Int]
qdA (Affine a b c d e f) = map qd [a, b, c, d, e, f]

qdR :: Maybe Rect -> [Int]
qdR Nothing = [0]
qdR (Just (Rect a b c d)) = map qd [a, b, c, d]

patternPaint :: Ctx -> Style -> Int -> Double -> Maybe Rect -> R (Maybe (Paint, Double))
patternPaint cx st j op bb
  | j `elem` cxStack cx || length (cxStack cx) > maxRefDepth = do
      note "recursive pattern ignored"; pure Nothing
  | otherwise = do
      let doc = cxDoc cx
          ch = hrefChain doc ["pattern"] j
          pa = chainAttr doc ch
          obb = pa "patternUnits" /= Just "userSpaceOnUse"
          cobb = pa "patternContentUnits" == Just "objectBoundingBox"
          pt = fromMaybe identityA (pa "patternTransform" >>= parseTransform)
          owner = listToMaybe [ k | k <- ch, not (null (elemKids (elOf doc k))) ]
          vb = pa "viewBox" >>= parseViewBox
          bbOk = maybe False (\r -> rectW r > 0 && rectH r > 0) bb
          Rect bx by _ _ = fromMaybe (Rect 0 0 1 1) bb
          bw = maybe 1 rectW bb; bh = maybe 1 rectH bb
          fx = unitCoord st obb AxX (pa "x") 0; fy = unitCoord st obb AxY (pa "y") 0
          fw = unitCoord st obb AxX (pa "width") 0; fh = unitCoord st obb AxY (pa "height") 0
          (x, y, w, h) = if obb then (bx + fx * bw, by + fy * bh, fw * bw, fh * bh) else (fx, fy, fw, fh)
      if (obb || (cobb && isNothing' vb)) && not bbOk then pure Nothing
      else if w <= 0 || h <= 0 then pure Nothing
      else case owner of
        Nothing -> pure Nothing
        Just o -> do
          let contentM = case vb of
                Just r -> viewBoxTransform r (parsePAR (pa "preserveAspectRatio")) w h
                Nothing | cobb -> Affine bw 0 0 bh 0 0
                        | otherwise -> identityA
              m = translateA x y `composeA` contentM
              ost = inPlaceStyle doc o
              ost' = ost { stInnerVp = maybe (stInnerVp ost) (\r -> (rectW r, rectH r)) vb }
              cx' = cx { cxStack = j : cxStack cx, cxCtm = cxCtm cx `composeA` pt `composeA` m }
          let key = mixKey ([1, j] ++ map qd [x, y, w, h] ++ qdA contentM
                            ++ (if cxNonScaling cx then qdA (cxCtm cx') else []))
          (kids, _) <- memoised key (renderKids cx' ost' (elemKids (elOf doc o)))
          let paint = PTiling key pt (Rect x y (x + w) (y + h)) w h [CTransform m kids]
          pure (if null kids then Nothing else Just (paint, op))
  where isNothing' = maybe True (const False)

------------------------------------------------------------------------------
-- Clip paths

-- | The clip layers for @clip-path: url(#ref)@, outermost first; 'Nothing'
-- hides the element; @Just []@ means no clipping.
resolveClip :: Ctx -> Text -> Maybe Rect -> R (Maybe [ClipLayer])
resolveClip cx ref bb = do
  let doc = cxDoc cx
  case lookupId doc ref of
    Just j | elName (elOf doc j) == "clipPath" -> clipLayers cx j bb
    _ -> pure (Just [])

clipLayers :: Ctx -> Int -> Maybe Rect -> R (Maybe [ClipLayer])
clipLayers cx j bb
  | j `elem` cxStack cx || length (cxStack cx) > maxRefDepth = do
      note "recursive clip path ignored"; pure Nothing
  | otherwise = do
      let doc = cxDoc cx
          e = elOf doc j
          cst = inPlaceStyle doc j
          obb = attr e "clipPathUnits" == Just "objectBoundingBox"
          bbOk = maybe False (\r -> rectW r > 0 && rectH r > 0) bb
          cm = elTransform e `composeA` (if obb then maybe identityA bboxMatrix bb else identityA)
          cx' = cx { cxStack = j : cxStack cx }
      if obb && not bbOk then pure Nothing else do
        -- The clip path's own clip-path intersects it.
        outer <- case stClipPath cst of
          Just r | Just k <- lookupId doc r, elName (elOf doc k) == "clipPath" -> clipLayers cx' k bb
          _ -> pure (Just [])
        case outer of
          Nothing -> pure Nothing
          Just outerLs -> do
            kids <- fmap catMaybes $ forM (elemKids e) $ \k -> clipChild cx' cst cm k
            let simple = all (\(_, ls) -> null ls) kids
            if simple
              then pure (Just (outerLs ++ [LShapes (concatMap fst kids)]))
              else do
                -- A child with its own clip path: an alpha mask of the
                -- children, each clipped, expresses the union of intersections.
                let fillOf (ClipShape m r p) = CFill m r (PSolid white) p
                    cmds = concat [ foldr applyLayer (map fillOf ss) ls | (ss, ls) <- kids ]
                    sm = SoftMask (-1) MaskAlpha transparent cmds Nothing
                pure (Just (outerLs ++ [LMask sm]))

-- One child of a clipPath: its shapes (in the clip path's user space after
-- @cm@) and any clip layers of its own.
clipChild :: Ctx -> Style -> Affine -> Int -> R (Maybe ([ClipShape], [ClipLayer]))
clipChild cx cst cm k = do
  let doc = cxDoc cx
      ke = elOf doc k
      kst = computeStyle doc cst k
  if stDisplayNone kst then pure Nothing else
    case elName ke of
      "use" -> case attr ke "href" >>= refId >>= lookupId doc of
        Just t | elName (elOf doc t) `elem` ("text" : shapeNames), t `notElem` cxStack cx -> do
          let x = fromMaybe 0 (lenAttr kst AxX ke "x")
              y = fromMaybe 0 (lenAttr kst AxY ke "y")
              um = cm `composeA` elTransform ke `composeA` translateA x y
          r <- clipChild cx { cxStack = t : cxStack cx } kst um t
          own <- ownLayers kst
          pure (fmap (\(ss, ls) -> (ss, own ++ ls)) r)
        _ -> pure Nothing
      n | n `elem` shapeNames || n == "text" -> do
            if not (stVisible kst) && n /= "text" then pure Nothing else do
              let m = cm `composeA` elTransform ke
              ss <- if n == "text"
                      then do
                        gl <- textClipShapes cx kst k
                        pure [ ClipShape (m `composeA` gm) NonZero p | (gm, p) <- gl ]
                      else pure $ case shapeGeom doc k kst of
                        Just (_, _, p, _) -> [ClipShape m (stClipRule kst) p]
                        Nothing -> []
              own <- ownLayers kst
              -- The child's own clip path lives in the child's user space.
              let own' = map (shiftLayer m) own
              pure (Just (ss, own'))
        | otherwise -> pure Nothing
  where
    ownLayers kst = case stClipPath kst of
      Just r -> do
        -- objectBoundingBox on a clip child's clip uses the child's bbox;
        -- clip children are rare enough that user space is assumed.
        res <- resolveClip cx r Nothing
        pure (fromMaybe [LShapes []] res)
      Nothing -> pure []
    shiftLayer m l = case l of
      LShapes ss -> LShapes [ ClipShape (composeA m a) r p | ClipShape a r p <- ss ]
      LMask sm -> LMask sm { smCmds = [CTransform m (smCmds sm)] }

------------------------------------------------------------------------------
-- Masks

-- | @Left ()@ hides the element; @Right Nothing@ means no mask.
resolveMask :: Ctx -> Text -> Maybe Rect -> R (Either () (Maybe SoftMask))
resolveMask cx ref bb = do
  let doc = cxDoc cx
  case lookupId doc ref of
    Just j | elName (elOf doc j) == "mask" -> maskOf cx j bb
    _ -> pure (Right Nothing)

maskOf :: Ctx -> Int -> Maybe Rect -> R (Either () (Maybe SoftMask))
maskOf cx j bb
  | j `elem` cxStack cx || length (cxStack cx) > maxRefDepth = do
      note "recursive mask ignored"; pure (Left ())
  | otherwise = do
      let doc = cxDoc cx
          e = elOf doc j
          mst = inPlaceStyle doc j
          obb = attr e "maskUnits" /= Just "userSpaceOnUse"
          cobb = attr e "maskContentUnits" == Just "objectBoundingBox"
          bbOk = maybe False (\r -> rectW r > 0 && rectH r > 0) bb
          Rect bx by _ _ = fromMaybe (Rect 0 0 1 1) bb
          bw = maybe 1 rectW bb; bh = maybe 1 rectH bb
          (vw, vh) = stVp mst
          fx = unitCoord mst obb AxX (attr e "x") (if obb then -0.1 else -0.1 * vw)
          fy = unitCoord mst obb AxY (attr e "y") (if obb then -0.1 else -0.1 * vh)
          fw = unitCoord mst obb AxX (attr e "width") (if obb then 1.2 else 1.2 * vw)
          fh = unitCoord mst obb AxY (attr e "height") (if obb then 1.2 else 1.2 * vh)
          (x, y, w, h) = if obb then (bx + fx * bw, by + fy * bh, fw * bw, fh * bh) else (fx, fy, fw, fh)
          contentM = if cobb then maybe identityA bboxMatrix bb else identityA
          cx' = cx { cxStack = j : cxStack cx, cxCtm = cxCtm cx `composeA` contentM }
      if (obb || cobb) && not bbOk then pure (Left ())
      else if w <= 0 || h <= 0 then pure (Left ())
      else do
        (kids, _) <- renderKids cx' mst (elemKids e)
        let body = [CClip [ClipShape identityA NonZero (rectPath x y w h)]
                      [ if contentM == identityA then c else CTransform contentM [c] | c <- kids ]]
        -- A mask may itself be masked.
        inner <- case stMask mst of
          Just r -> resolveMask cx' { cxCtm = cxCtm cx } r bb
          Nothing -> pure (Right Nothing)
        let cmds = case inner of
              Right (Just sm2) -> [CGroup defaultGroup { goMask = Just sm2, goIsolated = True } body]
              _ -> body
            kind = if stMaskAlpha mst then MaskAlpha else MaskLuminance
            key = mixKey ([2, j] ++ qdR bb)
        pure (Right (Just (SoftMask key kind transparent cmds Nothing)))

------------------------------------------------------------------------------
-- Markers

-- A path vertex: position and the incoming and outgoing directions.
data Vertex = Vertex !Pt !(Maybe Pt) !(Maybe Pt)

-- | The marker vertices of a path, per SVG's path-vertex rules: one per
-- command end point (an arc is one) plus each subpath start; a closed
-- subpath's start and close vertices take the closing segment as their
-- incoming direction and the first segment as their outgoing one.
markerVertices :: [PCmd] -> [Vertex]
markerVertices = go (Pt 0 0) (Pt 0 0) Nothing Nothing
  where
    -- go: subpath start, current point, end direction of the previous
    -- segment, start direction of the subpath's first segment.
    go _ _ _ _ [] = []
    go _ _ _ _ (PMove p : rest) =
      Vertex p (closingIn p rest) (firstDir p rest) : go p p Nothing (firstDir p rest) rest
    go s cur lastEnd firstOut (PClose : rest) =
      Vertex s (nz (ptSub s cur) `orD` lastEnd) firstOut : go s s Nothing firstOut rest
    go s cur _ firstOut (c : rest) =
      let (_, ed, p) = dirs cur c
          next = case rest of
            (n : _) | isDraw n -> let (sd', _, _) = dirs p n in sd'
            (PClose : _) -> nz (ptSub s p) `orD` firstOut
            _ -> Nothing
      in Vertex p ed next : go s p ed firstOut rest
    isDraw x = case x of PMove _ -> False; PClose -> False; _ -> True
    firstDir p rest = case rest of
      (n : _) | isDraw n -> let (sd, _, _) = dirs p n in sd
      _ -> Nothing
    -- The start vertex of a closed subpath: incoming = the closing segment
    -- (or the last drawn segment when the close is degenerate).
    closingIn s rest =
      let (body, after) = break (not . isDraw) rest
      in case after of
           (PClose : _) | not (null body) ->
             let (endP, lastD) = foldl' (\(cp, _) x -> let (_, ed, p) = dirs cp x in (p, ed)) (s, Nothing) body
             in nz (ptSub s endP) `orD` lastD
           _ -> Nothing
    orD (Just a) _ = Just a
    orD Nothing b = b
    nz v = if ptLen v < 1e-12 then Nothing else Just v
    -- start direction, end direction, end point
    dirs cur c = case c of
      PLine p -> let d = nz (ptSub p cur) in (d, d, p)
      PQuad a p -> (nz (ptSub a cur) `orD` nz (ptSub p cur), nz (ptSub p a) `orD` nz (ptSub p cur), p)
      PCubic a b p -> ( nz (ptSub a cur) `orD` nz (ptSub b cur) `orD` nz (ptSub p cur)
                      , nz (ptSub p b) `orD` nz (ptSub p a) `orD` nz (ptSub p cur), p)
      PArc cs p -> case (cs, reverse cs) of
        ((a, _, _) : _, (_, b, e) : more) ->
          let prevEnd = case more of ((_, _, pe) : _) -> pe; [] -> cur
          in (nz (ptSub a cur) `orD` nz (ptSub p cur), nz (ptSub e b) `orD` nz (ptSub e prevEnd), p)
        _ -> let d = nz (ptSub p cur) in (d, d, p)
      PMove p -> (Nothing, Nothing, p)
      PClose -> (Nothing, Nothing, cur)

angleOf :: Pt -> Double
angleOf (Pt x y) = atan2 y x * 180 / pi

-- The bisector of the incoming and outgoing directions, in degrees.
vertexAngle :: Vertex -> Double
vertexAngle (Vertex _ i o) = case (i, o) of
  (Just a, Just b) ->
    let ta = angleOf a; tb = angleOf b
        m = (ta + tb) / 2
    in if abs (ta - tb) > 180 then m + 180 else m
  (Just a, Nothing) -> angleOf a
  (Nothing, Just b) -> angleOf b
  _ -> 0

renderMarkers :: Ctx -> Style -> [PCmd] -> R [Cmd]
renderMarkers cx st pcs
  | all (== Nothing) [stMarkerStart st, stMarkerMid st, stMarkerEnd st] = pure []
  | otherwise = do
      let vs = take maxMarkerVerts (markerVertices pcs)
          n = length vs
          idx = zip [0 :: Int ..] vs
          sel k = case k of
            0 -> [ (v, True) | (i, v) <- idx, i == 0 ]
            1 -> [ (v, False) | (i, v) <- idx, i > 0, i < n - 1 ]
            _ -> [ (v, False) | (i, v) <- idx, i == n - 1 ]
      fp <- resolvePaint cx st (stFill st) (stFillOpacity st) Nothing
      sp <- resolvePaint cx st (stStroke st) (stStrokeOpacity st) Nothing
      let cx' = cx { cxCtxFill = fst <$> fp, cxCtxStroke = fst <$> sp }
      fmap concat $ forM (zip [0 :: Int ..] [stMarkerStart st, stMarkerMid st, stMarkerEnd st]) $ \(k, mref) ->
        case mref >>= lookupId (cxDoc cx) of
          Just j | elName (elOf (cxDoc cx) j) == "marker", not (null (sel k)) -> markerAt cx' st j (sel k)
          _ -> pure []

markerAt :: Ctx -> Style -> Int -> [(Vertex, Bool)] -> R [Cmd]
markerAt cx st j verts
  | j `elem` cxStack cx || length (cxStack cx) > maxRefDepth = pure []
  | otherwise = do
      let doc = cxDoc cx
          e = elOf doc j
          mst0 = inPlaceStyle doc j
          mw = fromMaybe 3 (lenAttr mst0 AxX e "markerWidth")
          mh = fromMaybe 3 (lenAttr mst0 AxY e "markerHeight")
          vb = attr e "viewBox" >>= parseViewBox
          vbM = maybe identityA (\r -> viewBoxTransform r (parsePAR (attr e "preserveAspectRatio")) mw mh) vb
          mst = mst0 { stInnerVp = maybe (mw, mh) (\r -> (rectW r, rectH r)) vb }
          refX = fromMaybe 0 (lenAttr mst AxX e "refX")
          refY = fromMaybe 0 (lenAttr mst AxY e "refY")
          Pt rx ry = applyA vbM (Pt refX refY)
          sw = if attr e "markerUnits" == Just "userSpaceOnUse" then 1 else stStrokeWidth st
          orient = fromMaybe "0" (attr e "orient")
          cx' = cx { cxStack = j : cxStack cx }
      if mw <= 0 || mh <= 0 || sw <= 0 then pure [] else do
        (kids, _) <- renderKids cx' mst (elemKids e)
        if null kids then pure [] else do
          let clipR = Rect 0 0 mw mh
              body = if stOverflowVis mst then [CTransform vbM kids]
                     else [CClip [ClipShape identityA NonZero (rectPath (rectX clipR) (rectY clipR) mw mh)] [CTransform vbM kids]]
              inner = [CTransform (translateA (-rx) (-ry)) body]
          pure [ CTransform (translateA px py `composeA` rotateA (ang * pi / 180) `composeA` scaleA sw sw) inner
               | (v@(Vertex (Pt px py) _ _), isStart) <- verts
               , let ang = case T.strip orient of
                       "auto" -> vertexAngle v
                       "auto-start-reverse" -> vertexAngle v + (if isStart then 180 else 0)
                       o -> fromMaybe 0 (parseAngle o) ]
  where rectX (Rect x _ _ _) = x
        rectY (Rect _ y _ _) = y

------------------------------------------------------------------------------
-- Images

renderImage :: Ctx -> Style -> El -> R ([Cmd], Maybe Rect)
renderImage cx st e = case attr e "href" of
  Nothing -> pure ([], Nothing)
  Just href -> do
    let bytes = case dataUri href of
          Just b -> Just b
          Nothing | "#" `T.isPrefixOf` T.strip href -> Nothing
                  | otherwise -> seResolve (cxEnv cx) (T.strip href)
    case bytes of
      Nothing -> do
        if "data:" `T.isPrefixOf` T.strip href then note "an embedded image could not be read"
          else note "external image not available"
        pure ([], Nothing)
      Just bs -> place bs
  where
    x = fromMaybe 0 (lenAttr st AxX e "x")
    y = fromMaybe 0 (lenAttr st AxY e "y")
    dimW = case attr e "width" of
      Just v | T.toLower (T.strip v) /= "auto" -> lenAttr st AxX e "width"
      _ -> Nothing
    dimH = case attr e "height" of
      Just v | T.toLower (T.strip v) /= "auto" -> lenAttr st AxY e "height"
      _ -> Nothing
    par = parsePAR (attr e "preserveAspectRatio")
    sizes iw ih = case (dimW, dimH) of
      (Just w, Just h) -> (w, h)
      (Just w, Nothing) -> (w, if iw > 0 then w * ih / iw else 0)
      (Nothing, Just h) -> (if ih > 0 then h * iw / ih else 0, h)
      _ -> (iw, ih)
    clipTo w h cs = case par of
      PAR _ True -> [CClip [ClipShape identityA NonZero (rectPath x y w h)] cs]
      _ -> cs
    place bs
      | sniffSvg bs = if cxImgDep cx >= maxImageDepth
          then do note "nested SVG images too deep"; pure ([], Nothing)
          else case parseSvgDepth (cxEnv cx) (cxImgDep cx + 1) bs of
            Left _ -> do note "an embedded SVG image could not be read"; pure ([], Nothing)
            Right sc -> do
              mapM_ note (scNotes sc)
              let Rect _ _ iw ih = scBounds sc
                  (w, h) = sizes iw ih
              if w <= 0 || h <= 0 || iw <= 0 || ih <= 0 then pure ([], Nothing) else do
                let m = translateA x y `composeA` viewBoxTransform (Rect 0 0 iw ih) par w h
                    cs = [CTransform m [CClip [ClipShape identityA NonZero (rectPath 0 0 iw ih)] (scCmds sc)]]
                pure (if stVisible st then clipTo w h cs else [], Just (Rect x y (x + w) (y + h)))
      | otherwise = case decodeImage bs of
          Left _ -> do note "an embedded image could not be decoded"; pure ([], Nothing)
          Right img -> do
            let iw = fromIntegral (imgW img); ih = fromIntegral (imgH img)
                (w, h) = sizes iw ih
            if w <= 0 || h <= 0 || iw <= 0 || ih <= 0 then pure ([], Nothing) else do
              let m = translateA x y `composeA` viewBoxTransform (Rect 0 0 iw ih) par w h `composeA` scaleA iw ih
                  opts = defaultImageOpts { ioInterp = if stPixelated st then InterpNearest else InterpSmooth }
              pure (if stVisible st then clipTo w h [CImage m img opts] else [], Just (Rect x y (x + w) (y + h)))

-- | The bytes of a @data:@ URI (base64 or percent-encoded).
dataUri :: Text -> Maybe BS.ByteString
dataUri t0 = do
  r <- T.stripPrefix "data:" (T.strip t0)
  let (hdr, body0) = T.breakOn "," r
  if T.null body0 then Nothing else do
    let body = T.drop 1 body0
        isB64 = "base64" `elem` map (T.toLower . T.strip) (T.splitOn ";" hdr)
    Just (if isB64 then base64 body else percentDecode body)

base64 :: Text -> BS.ByteString
base64 t = BS.pack (go (mapMaybe val (T.unpack t)))
  where
    val c | c >= 'A' && c <= 'Z' = Just (ord c - 65)
          | c >= 'a' && c <= 'z' = Just (ord c - 71)
          | c >= '0' && c <= '9' = Just (ord c + 4)
          | c == '+' || c == '-' = Just 62
          | c == '/' || c == '_' = Just 63
          | otherwise = Nothing
    go (a : b : c : d : r) = let n = a `shiftL` 18 .|. b `shiftL` 12 .|. c `shiftL` 6 .|. d
                             in w8 (n `shiftR` 16) : w8 (n `shiftR` 8) : w8 n : go r
    go [a, b, c] = let n = a `shiftL` 18 .|. b `shiftL` 12 .|. c `shiftL` 6 in [w8 (n `shiftR` 16), w8 (n `shiftR` 8)]
    go [a, b] = let n = a `shiftL` 18 .|. b `shiftL` 12 in [w8 (n `shiftR` 16)]
    go _ = []
    w8 :: Int -> Word8
    w8 n = fromIntegral (n .&. 255)

percentDecode :: Text -> BS.ByteString
percentDecode = BS.concat . go . T.unpack
  where
    go [] = []
    go ('%' : a : b : r) | isHexDigit a && isHexDigit b = BS.singleton (fromIntegral (digitToInt a * 16 + digitToInt b)) : go r
    go (c : r) = encodeChar c : go r
    encodeChar c = let n = ord c in
      if n < 0x80 then BS.singleton (fromIntegral n)
      else BS.pack (map fromIntegral (utf8 n))
    utf8 n | n < 0x800 = [0xc0 .|. (n `shiftR` 6), 0x80 .|. (n .&. 0x3f)]
           | n < 0x10000 = [0xe0 .|. (n `shiftR` 12), 0x80 .|. ((n `shiftR` 6) .&. 0x3f), 0x80 .|. (n .&. 0x3f)]
           | otherwise = [0xf0 .|. (n `shiftR` 18), 0x80 .|. ((n `shiftR` 12) .&. 0x3f), 0x80 .|. ((n `shiftR` 6) .&. 0x3f), 0x80 .|. (n .&. 0x3f)]

------------------------------------------------------------------------------
-- Text

-- One addressable character before layout.
data TChar = TChar
  { tcChar  :: !Char
  , tcOwner :: !Int        -- ^ The element whose text node it came from.
  , tcStyle :: Style
  , tcPath  :: [Int]       -- ^ Its text-content ancestors, innermost first (the text element last).
  }

-- A laid-out glyph.
data PGlyph = PGlyph
  { pgOwner :: !Int
  , pgStyle :: Style
  , pgAff   :: !Affine
  , pgKey   :: !Int
  , pgPath  :: Path
  , pgX0    :: !Double     -- ^ Pen position before and after (for decorations).
  , pgX1    :: !Double
  , pgY     :: !Double
  , pgChunk :: !Int
  , pgOnPath :: Maybe (PathTab, Double)
    -- ^ On a textPath: the path and the glyph's advance. Then 'pgX0' is the
    -- distance along the path, 'pgY' the offset across it, and 'pgAff' only
    -- the glyph's own rotation and scale, until 'placeOnPath'.
  }

-- | A flattened path for textPath: total length and, keyed by the distance
-- at which each piece starts, its start point and unit direction.
data PathTab = PathTab !Double (M.Map Double (Pt, Pt))

mkPathTab :: Path -> Maybe PathTab
mkPathTab path =
  let pts = flatten Nothing Nothing path
      segs = zip pts (drop 1 pts)
      go !_ [] = []
      go !d ((a, b) : r) = let l = ptLen (ptSub b a)
                           in if l <= 0 then go d r else (d, (a, ptScale (1 / l) (ptSub b a)), l) : go (d + l) r
      pieces = go 0 segs
      total = sum [ l | (_, _, l) <- pieces ]
  in if null pieces then Nothing else Just (PathTab total (M.fromList [ (d, v) | (d, v, _) <- pieces ]))
  where
    -- Only the first subpath, as SVG specifies for text on a path.
    flatten _ _ [] = []
    flatten cur _ (MoveTo p : r) = case cur of
      Nothing -> p : flatten (Just p) (Just p) r
      Just _ -> []
    flatten cur st (seg : r) = let c = fromMaybe (Pt 0 0) cur in case seg of
      LineTo p -> p : flatten (Just p) st r
      QuadTo a p -> [ q t c a p | t <- steps ] ++ flatten (Just p) st r
      CubicTo a b p -> [ cub t c a b p | t <- steps ] ++ flatten (Just p) st r
      ClosePath -> maybe [] (\s0 -> s0 : flatten (Just s0) st r) st
    steps = [ fromIntegral k / 16 | k <- [1 .. 16 :: Int] ]
    q t a b c = ptAdd (ptScale ((1 - t) * (1 - t)) a) (ptAdd (ptScale (2 * t * (1 - t)) b) (ptScale (t * t) c))
    cub t a b c d = let u = 1 - t in
      ptAdd (ptScale (u * u * u) a) (ptAdd (ptScale (3 * u * u * t) b) (ptAdd (ptScale (3 * u * t * t) c) (ptScale (t * t * t) d)))

-- Point and unit direction at a distance along the path.
pathAt :: PathTab -> Double -> Maybe (Pt, Pt)
pathAt (PathTab total m) d
  | d < 0 || d > total = Nothing
  | otherwise = case M.lookupLE d m of
      Just (d0, (p, dir)) -> Just (ptAdd p (ptScale (d - d0) dir), dir)
      Nothing -> Nothing

-- | Final glyph transform for a glyph on a path: its midpoint sits on the
-- path, rotated to the tangent; a midpoint off either end hides it.
placeOnPath :: PGlyph -> Maybe PGlyph
placeOnPath g = case pgOnPath g of
  Nothing -> Just g
  Just (tab, adv) -> do
    let mid = pgX0 g + adv / 2
    (Pt px py, Pt dx dy) <- pathAt tab mid
    let rot = Affine dx dy (-dy) dx 0 0
        m = translateA px py `composeA` rot `composeA` translateA (-adv / 2) (pgY g) `composeA` pgAff g
    Just g { pgAff = m }

-- | Collect the characters of a text element, styles computed, whitespace
-- handled (see 'collapseSpace').
textChars :: Ctx -> Style -> Int -> R [TChar]
textChars cx st0 i0 = do
  raw <- go st0 [i0] i0
  pure (collapseSpace raw)
  where
    doc = cxDoc cx
    go st path i = do
      let e = elOf doc i
      fmap concat $ forM (elKids e) $ \nd -> case nd of
        NT t -> pure [ TChar c i st path | c <- T.unpack t ]
        NE k -> do
          let ke = elOf doc k
              kst = computeStyle doc st k
          if stDisplayNone kst then pure [] else case elName ke of
            "tspan" -> go kst (k : path) k
            "a" -> go kst (k : path) k
            "textPath" -> go kst (k : path) k
            _ -> pure []

-- | SVG whitespace handling. In the default mode newlines and tabs become
-- spaces, runs of spaces collapse across element boundaries, and leading and
-- trailing spaces of the whole text go (CSS @white-space: normal@, which is
-- what browsers and resvg implement for @xml:space=\"default\"@); in
-- @preserve@ mode newlines and tabs become spaces and nothing collapses.
collapseSpace :: [TChar] -> [TChar]
collapseSpace = trimEnd . go True
  where
    go _ [] = []
    go prevSpace (c : cs)
      | stPreserve (tcStyle c) =
          let ch = if tcChar c `elem` ("\n\r\t" :: String) then ' ' else tcChar c
          in c { tcChar = ch } : go (ch == ' ') cs
      | otherwise =
          let ch = if tcChar c `elem` ("\n\r\t" :: String) then ' ' else tcChar c
          in if ch == ' ' && prevSpace then go True cs
             else c { tcChar = ch } : go (ch == ' ') cs
    trimEnd xs = reverse (dropWhile (\c -> tcChar c == ' ' && not (stPreserve (tcStyle c))) (reverse xs))

-- Per-character positional attributes: x, y, dx, dy, rotate.
data PosAttrs = PosAttrs
  { paX, paY, paDX, paDY, paRot :: IM.IntMap Double }

positionAttrs :: Doc -> Style -> [TChar] -> PosAttrs
positionAttrs doc _ chars =
  let indexed = zip [0 :: Int ..] chars
      -- Each element's first character index and its char count.
      ranges = M.fromListWith (\(a, n) (b, m) -> (min a b, n + m))
                 [ (el, (k, 1 :: Int)) | (k, c) <- indexed, el <- tcPath c ]
      -- Elements in document order (= by first index, outer before inner at ties).
      els = [ (el, start, cnt) | (el, (start, cnt)) <- M.toList ranges ]
      ordered = sortOn (\(el, start, _) -> (start, depthOf el)) els
      depthOf el = length (takeWhile (>= 0) (iterate (\k -> elParent (elOf doc k)) el))
      lens st ax k e = case attr e k of
        Nothing -> []
        Just v -> mapMaybe (fmap (resolveLen (stFontSize st) (stVp st) ax) . parseLength)
                    (filter (not . T.null) (T.split (\c -> c == ',' || isSpace c) v))
      styleOf el = case [ tcStyle c | c <- chars, tcOwner c == el ] of
        (s : _) -> s
        [] -> inPlaceStyle doc el
      assign ax k = foldl' (\im (el, start, cnt) ->
        let vs = take cnt (lens (styleOf el) ax k (elOf doc el))
        in foldl' (\m (o, v) -> IM.insert (start + o) v m) im (zip [0 ..] vs)) IM.empty ordered
      assignRot im0 = foldl' (\im (el, start, cnt) ->
        case attr (elOf doc el) "rotate" of
          Nothing -> im
          Just v -> let vs = parseNumberList v
                    in if null vs then im
                       else let full = take cnt (vs ++ repeat (last vs))
                            in foldl' (\m (o, r) -> IM.insert (start + o) r m) im (zip [0 ..] full)) im0 ordered
  in PosAttrs (assign AxX "x") (assign AxY "y") (assign AxX "dx") (assign AxY "dy") (assignRot IM.empty)


-- | Lay out a text element: glyphs in the text element's user space.
layoutText :: Ctx -> Style -> Int -> R ([PGlyph], Bool, Int)
layoutText cx st i = do
  chars <- textChars cx st i
  let doc = cxDoc cx
      pos = positionAttrs doc st chars
      env = cxEnv cx
      faces = M.fromList [ (tcOwner c, facesFor env (tcStyle c)) | c <- chars ]
      lookupGlyph c = do
        fs <- M.lookup (tcOwner c) faces
        listToMaybe [ (f, g) | f <- fs, Just g <- [faceGlyph f (tcChar c)] ]
      anyFace = not (null (concat (M.elems faces)))
  budget <- gets rsGlyphs
  let n = length chars
  if n == 0 then pure ([], True, 0) else
    if budget + n > maxTextGlyphs then do note "text truncated: too many glyphs"; pure ([], True, n) else do
      modify' (\s -> s { rsGlyphs = rsGlyphs s + n })
      let tpOf c = listToMaybe [ t | t <- tcPath c, elName (elOf doc t) == "textPath" ]
          tabs = M.fromList [ (t, textPathTab cx (tcStyle c) t) | c <- chars, Just t <- [tpOf c] ]
          step (acc, penX0, penY0, chunk, prev, prevTp) (k, c) =
            let tp = tpOf c
                tab = tp >>= \t -> M.findWithDefault Nothing t tabs
                entering = tp /= prevTp && isJust tp
                -- Leaving a path: continue from where the text left it.
                (penX, penY)
                  | entering = (startOffset c tp tab, 0)
                  | tp /= prevTp, Just pt <- prevTp, Just pt' <- M.findWithDefault Nothing pt tabs
                  , Just (Pt ex ey, _) <- pathAt pt' (min penX0 (tabLen pt')) = (ex, ey)
                  | otherwise = (penX0, penY0)
                onPath = isJust tp
                ax = if onPath then Nothing else IM.lookup k (paX pos)
                ay = if onPath then Nothing else IM.lookup k (paY pos)
                newChunk = k > 0 && (isJust ax || isJust ay || entering)
                chunk' = if newChunk then chunk + 1 else chunk
                cst = tcStyle c
                fs = stFontSize cst
                mg = lookupGlyph c
                kern = case (prev, mg) of
                  (Just (pf, pg), Just (f, (g, _, _))) | faceKey pf == faceKey f, not newChunk
                                                       , isNothingI (IM.lookup k (paDX pos)) ->
                    faceKern f pg g * fs / faceUnitsPerEm f
                  _ -> 0
                x0 = fromMaybe (penX + kern) ax + IM.findWithDefault 0 k (paDX pos)
                y0 = fromMaybe penY ay + IM.findWithDefault 0 k (paDY pos)
                rot = IM.findWithDefault 0 k (paRot pos)
                spacing = stLetterSp cst + (if tcChar c == ' ' then stWordSp cst else 0)
            in case mg of
                 Just (f, (gid, gpath, adv)) ->
                   let s = fs / faceUnitsPerEm f
                       shift = baselineShift cst f s
                       local = rotateA (rot * pi / 180) `composeA` scaleA s (-s)
                       aff = if onPath then local else translateA x0 (y0 + shift) `composeA` local
                       x1 = x0 + adv * s + spacing
                       key = if faceKey f < 0 then -1 else mixKey [3, faceKey f, gid]
                       pg = PGlyph (tcOwner c) cst aff key gpath x0 x1 (y0 + shift) chunk'
                              (if onPath then fmap (\t -> (t, adv * s)) tab else Nothing)
                       keep = not onPath || isJust tab
                   in (if keep then pg : acc else acc, x1, y0, chunk', Just (f, gid), tp)
                 Nothing ->
                   let x1 = x0 + 0.5 * fs + spacing
                   in (acc, x1, y0, chunk', Nothing, tp)
          (gsRev, _, _, _, _, _) = foldl' step ([], 0, 0, 0 :: Int, Nothing, Nothing) (zip [0 ..] chars)
          gs = reverse gsRev
          -- Chunk extents (from the pen, not the ink) and anchors.
          chunkInfo = M.fromListWith (\(a0, a1, an) (b0, b1, _) -> (min a0 b0, max a1 b1, an))
                        [ (pgChunk g, (pgX0 g, pgX1 g, stAnchor (pgStyle g))) | g <- gsRev ]
          anchored = [ g''
                     | g <- gs
                     , let (c0, c1, an) = M.findWithDefault (0, 0, AnchorStart) (pgChunk g) chunkInfo
                           w = c1 - c0
                           dx = case an of
                             AnchorStart -> 0
                             AnchorMiddle -> -w / 2
                             AnchorEnd -> -w
                           g' = if isJust (pgOnPath g)
                                  then g { pgX0 = pgX0 g + dx, pgX1 = pgX1 g + dx }
                                  else g { pgAff = translateA dx 0 `composeA` pgAff g, pgX0 = pgX0 g + dx, pgX1 = pgX1 g + dx }
                     , Just g'' <- [placeOnPath g'] ]
      pure (anchored, anyFace, n)
  where
    isNothingI = maybe True (const False)
    tabLen (PathTab l _) = l
    startOffset c tp tab = case (tp, tab) of
      (Just t, Just (PathTab l _)) -> case attr (elOf (cxDoc cx) t) "startOffset" >>= parseLength of
        Just (Len v UPct) -> v / 100 * l
        Just len -> resolveLen (stFontSize (tcStyle c)) (stVp (tcStyle c)) AxX len
        Nothing -> 0
      _ -> 0

-- The path a textPath follows (its href: a path, or any basic shape), in
-- the text element's user space.
textPathTab :: Ctx -> Style -> Int -> Maybe PathTab
textPathTab cx _ t = do
  let doc = cxDoc cx
      te = elOf doc t
  j <- attr te "href" >>= refId >>= lookupId doc
  let pe = elOf doc j
  (_, _, p, _) <- shapeGeom doc j (inPlaceStyle doc j)
  mkPathTab (transformPath (elTransform pe) p)

-- The y-down offset of a glyph's baseline for dominant-baseline and
-- baseline-shift, from the face's metrics.
baselineShift :: Style -> Face -> Double -> Double
baselineShift st f s =
  let asc = faceAscent f * s; desc = faceDescent f * s
      b = case stBaseline st of
        BLAlphabetic -> 0
        BLMiddle -> 0.25 * faceUnitsPerEm f * s     -- half an assumed x-height of 0.5 em
        BLCentral -> (asc + desc) / 2
        BLHanging -> asc * 0.8
        BLTop -> asc
        BLBottom -> desc
        BLMath -> asc * 0.5
        BLIdeographic -> desc
  in b + stBaselineShift st

-- The faces to try for a style, primary first: the whole family list, then
-- each family alone (per-character fallback), then the generic families.
facesFor :: SvgEnv -> Style -> [Face]
facesFor env st = dedupFaces (mapMaybe (seFonts env) (fontTries st))
  where dedupFaces = go S.empty
          where go _ [] = []
                go seen (f : fs) | faceKey f >= 0 && S.member (faceKey f) seen = go seen fs
                                 | otherwise = f : go (S.insert (faceKey f) seen) fs

-- | The queries 'facesFor' makes for a style, primary first: the whole
-- family list, then each family alone (per-character fallback), then the
-- generic families. Shared with the 'svgFontQueries' pre-pass, so the two
-- cannot drift apart.
fontTries :: Style -> [FontQuery]
fontTries st =
  let fams = if null (stFontFamily st) then ["serif"] else stFontFamily st
      q fs = FontQuery fs (stFontWeight st) (stFontStyle st)
  in q fams : [ q [f] | f <- drop 1 fams ] ++ [ q [g] | g <- ["sans-serif", "serif", "monospace"], g `notElem` fams ]

renderText :: Ctx -> Style -> Int -> R ([Cmd], Maybe Rect)
renderText cx st i = do
  (gs, anyFace, nChars) <- layoutText cx st i
  if null gs then do
    if nChars > 0 && not anyFace then note "text not shown: no font available" else pure ()
    pure ([], Nothing)
  else do
    let outlines = [ (g, transformPath (pgAff g) (pgPath g)) | g <- gs ]
        bb = unionAll [ pathBoundsExact p | (_, p) <- outlines ]
        runs = groupRuns gs
    cmds <- fmap concat $ forM runs $ \run -> do
      let rst = pgStyle (head run)
          visible = stVisible rst
          glyphs = [ Glyph (pgAff g) (pgKey g) (pgPath g) | g <- run, not (null (pgPath g)) ]
          strokePath = concat [ transformPath (pgAff g) (pgPath g) | g <- run ]
          decos = if any (isJust . pgOnPath) run then [] else decorations rst run
      if not visible then pure [] else do
        parts <- forM (stPaintOrder rst) $ \part -> case part of
          PPFill -> do
            mp <- resolvePaint cx rst (stFill rst) (stFillOpacity rst) bb
            pure $ case mp of
              Nothing -> []
              Just (p, gop) -> applyOpacity gop (CGlyphs identityA p glyphs : [ CFill identityA NonZero p d | d <- decos ])
          PPStroke -> case strokeStyleOf rst of
            Nothing -> pure []
            Just ss -> do
              mp <- resolvePaint cx rst (stStroke rst) (stStrokeOpacity rst) bb
              pure $ case mp of
                Nothing -> []
                Just (p, gop) -> applyOpacity gop [CStroke identityA ss p (strokePath ++ concat decos)]
          PPMarkers -> pure []
        let owner = pgOwner (head run)
            ownOp = if owner == i then 1 else stOpacity rst
        pure (applyOpacity ownOp (concat parts))
    pure (cmds, bb)
  where
    groupRuns [] = []
    groupRuns (g : rest) = let (same, other) = span (\h -> pgOwner h == pgOwner g) rest
                           in (g : same) : groupRuns other
    -- Underline / overline / line-through as filled rectangles under the
    -- run's advance, from the run's font size.
    decorations rst run =
      [ rectPath x0 (y + off) (x1 - x0) th
      | d <- stDecoration rst
      , let fs = stFontSize rst
            th = fs / 16
            x0 = minimum (map pgX0 run); x1 = maximum (map pgX1 run)
            y = pgY (head run)
            off = case d of
              "underline" -> fs * 0.1
              "overline" -> -fs * 0.8
              _ -> -fs * 0.3
      , x1 > x0 ]

-- Text as clip geometry: glyph outlines in the text element's user space.
textClipShapes :: Ctx -> Style -> Int -> R [(Affine, Path)]
textClipShapes cx st i = do
  (gs, _, _) <- layoutText cx st i
  pure [ (pgAff g, pgPath g) | g <- gs, stVisible (pgStyle g) ]

------------------------------------------------------------------------------
-- Filters

-- | The filters to apply, innermost first; 'Nothing' hides the element.
-- An unsupported filter draws the element unfiltered (with a note).
resolveFilters :: Ctx -> Style -> Maybe Rect -> R (Maybe [Filter])
resolveFilters cx st bb = case stFilter st of
  FSNone -> pure (Just [])
  FSList items -> do
    rs <- mapM item items
    if any (== FHide) rs then pure Nothing
    else if any (== FUnsup) rs then pure (Just [])
    else pure (Just [ f | FOk f <- rs ])
  where
    item (Left ref) = filterElement cx st ref bb
    item (Right fn) = pure (cssFilter st fn bb)

data FRes = FOk Filter | FHide | FUnsup

instance Eq FRes where
  FHide == FHide = True
  FUnsup == FUnsup = True
  _ == _ = False

-- A CSS filter function as a one- or two-primitive filter over the element's
-- bounding box (grown by the blur's reach).
cssFilter :: Style -> CssFilterFn -> Maybe Rect -> FRes
cssFilter st fn bb = case bb of
  Nothing -> FHide
  Just r0 ->
    let reach = case fn of
          CFBlur s -> 3 * s
          CFDropShadow dx dy s _ -> 3 * s + max (abs dx) (abs dy)
          _ -> 0
        r = rectInflate reach (inflatePct r0)
        prim op ins lin = FilterPrim op ins Nothing lin 0
    in FOk $ Filter r identityA $ case fn of
         CFBlur s -> [prim (FeBlur s s) [SourceGraphic] False]
         CFDropShadow dx dy s c -> [prim (FeDropShadow (s / 2) (s / 2) dx dy (colorOf st c)) [SourceGraphic] False]
         CFMatrix m -> [prim (FeColorMatrix m) [SourceGraphic] False]
         CFOpacity a -> [prim (FeColorMatrix [1,0,0,0,0, 0,1,0,0,0, 0,0,1,0,0, 0,0,0,a,0]) [SourceGraphic] False]
  where
    inflatePct r@(Rect x0 y0 x1 y1) = let dx = 0.1 * rectW r; dy = 0.1 * rectH r
                                      in Rect (x0 - dx) (y0 - dy) (x1 + dx) (y1 + dy)

filterElement :: Ctx -> Style -> Text -> Maybe Rect -> R FRes
filterElement cx st ref bb = do
  let doc = cxDoc cx
  case lookupId doc ref of
    Just j | elName (elOf doc j) == "filter" -> do
      let ch = hrefChain doc ["filter"] j
          fa = chainAttr doc ch
          obb = fa "filterUnits" /= Just "userSpaceOnUse"
          pobb = fa "primitiveUnits" == Just "objectBoundingBox"
          bbOk = maybe False (\r -> rectW r > 0 && rectH r > 0) bb
          Rect bx by _ _ = fromMaybe (Rect 0 0 1 1) bb
          bw = maybe 1 rectW bb; bh = maybe 1 rectH bb
          (vw, vh) = stVp st
          fx = unitCoord st obb AxX (fa "x") (if obb then -0.1 else -0.1 * vw)
          fy = unitCoord st obb AxY (fa "y") (if obb then -0.1 else -0.1 * vh)
          fw = unitCoord st obb AxX (fa "width") (if obb then 1.2 else 1.2 * vw)
          fh = unitCoord st obb AxY (fa "height") (if obb then 1.2 else 1.2 * vh)
          (x, y, w, h) = if obb then (bx + fx * bw, by + fy * bh, fw * bw, fh * bh) else (fx, fy, fw, fh)
          owner = listToMaybe [ k | k <- ch, not (null (elemKids (elOf doc k))) ]
          region = Rect x y (x + w) (y + h)
      if (obb || pobb) && not bbOk then pure FHide
      else if w <= 0 || h <= 0 then pure FHide
      else case owner of
        Nothing -> pure FHide
        Just o -> do
          let prims0 = take maxFilterPrims (elemKids (elOf doc o))
              pctx = PCtx doc st pobb (bx, by, bw, bh) region
          res <- buildPrims pctx prims0
          case res of
            Left msg -> do note msg; pure FUnsup
            Right [] -> pure FHide
            Right ps -> pure (FOk (Filter region identityA ps))
    _ -> pure FHide

data PCtx = PCtx
  { pcDoc :: Doc, pcStyle :: Style, pcObb :: Bool
  , pcBox :: (Double, Double, Double, Double), pcRegion :: Rect }

-- Primitives in order, results named by index; 'Left' = unsupported.
buildPrims :: PCtx -> [Int] -> R (Either String [FilterPrim])
buildPrims pc ks = go 0 M.empty [] ks
  where
    doc = pcDoc pc
    go :: Int -> M.Map Text Int -> [FilterPrim] -> [Int] -> R (Either String [FilterPrim])
    go _ _ acc [] = pure (Right (reverse acc))
    go n names acc (k : rest) = do
      let e = elOf doc k
          pst = inPlaceStyle doc k
          nm = elName e
      if not ("fe" `T.isPrefixOf` nm) then go n names acc rest else
        case primOp pc names e pst of
          Left msg -> pure (Left msg)
          Right (op, ins) -> do
            let names' = case attr e "result" of
                  Just r | not (T.null (T.strip r)) -> M.insert (T.strip r) n names
                  _ -> names
                fp = FilterPrim op ins (subregion pc e) (stLinearRGB pst) n
            go (n + 1) names' (fp : acc) rest

-- The primitive subregion, when any of x/y/width/height is given.
subregion :: PCtx -> El -> Maybe Rect
subregion pc e
  | not (any (isJust . attr e) ["x", "y", "width", "height"]) = Nothing
  | otherwise =
      let Rect rx ry rx1 ry1 = pcRegion pc
          (bx, by, bw, bh) = pcBox pc
          st = pcStyle pc
          obb = pcObb pc
          coord ax k d off sc = case attr e k of
            Nothing -> d
            Just _ -> let v = unitCoord st obb ax (attr e k) 0 in if obb then off + v * sc else v
          x = coord AxX "x" rx bx bw
          y = coord AxY "y" ry by bh
          w = case attr e "width" of
            Nothing -> rx1 - x
            Just _ -> let v = unitCoord st obb AxX (attr e "width") 0 in if obb then v * bw else v
          h = case attr e "height" of
            Nothing -> ry1 - y
            Just _ -> let v = unitCoord st obb AxY (attr e "height") 0 in if obb then v * bh else v
      in Just (Rect x y (x + max 0 w) (y + max 0 h))

primOp :: PCtx -> M.Map Text Int -> El -> Style -> Either String (FilterOp, [FilterIn])
primOp pc names e pst = do
  in1 <- input "in"
  case elName e of
    "feGaussianBlur" -> do
      let (sx, sy) = pair (attr e "stdDeviation") 0
      Right (FeBlur (max 0 (sx * sclX)) (max 0 (sy * sclY)), [in1])
    "feOffset" -> Right (FeOffset (n "dx" 0 * sclX) (n "dy" 0 * sclY), [in1])
    "feFlood" -> Right (FeFlood (withAlpha (stFloodOpacity pst) (colorOf pst (stFloodColor pst))), [])
    "feDropShadow" -> do
      let (sx, sy) = pair (attr e "stdDeviation") 2
      Right (FeDropShadow (max 0 (sx * sclX)) (max 0 (sy * sclY)) (n "dx" 2 * sclX) (n "dy" 2 * sclY)
               (withAlpha (stFloodOpacity pst) (colorOf pst (stFloodColor pst))), [in1])
    "feComposite" -> do
      in2 <- input "in2"
      let op = case attr e "operator" of
            Just "in" -> CompIn; Just "out" -> CompOut; Just "atop" -> CompAtop
            Just "xor" -> CompXor
            Just "arithmetic" -> CompArithmetic (n "k1" 0) (n "k2" 0) (n "k3" 0) (n "k4" 0)
            _ -> CompOver
      Right (FeComposite op, [in1, in2])
    "feBlend" -> do
      in2 <- input "in2"
      Right (FeBlend (fromMaybe BlendNormal (attr e "mode" >>= blendMode . T.toLower . T.strip)), [in1, in2])
    "feMerge" -> do
      ins <- mapM (\k -> inputOf (attr (elOf (pcDoc pc) k) "in"))
               [ k | k <- elemKids e, elName (elOf (pcDoc pc) k) == "feMergeNode" ]
      Right (FeMerge, ins)
    "feColorMatrix" -> do
      let vals = parseNumberList (fromMaybe "" (attr e "values"))
          ident = [1,0,0,0,0, 0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0]
          m = case fromMaybe "matrix" (attr e "type") of
            "saturate" -> saturateM (case vals of (v : _) -> max 0 v; [] -> 1)
            "hueRotate" -> hueRotateM (case vals of (v : _) -> v; [] -> 0)
            "luminanceToAlpha" -> [0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0.2125,0.7154,0.0721,0,0]
            _ -> if length vals == 20 then vals else ident
      Right (FeColorMatrix m, [in1])
    "feComponentTransfer" -> do
      fs <- mapM transferFn [ (elName ke, ke) | k <- elemKids e, let ke = elOf (pcDoc pc) k ]
      let get c = fromMaybe (1, 0) (lookup c [ (ch, v) | Just (ch, v) <- fs ])
          (rs, ri) = get 'R'; (gs, gi) = get 'G'; (bs, bi) = get 'B'; (as, ai) = get 'A'
      Right (FeColorMatrix [rs,0,0,0,ri, 0,gs,0,0,gi, 0,0,bs,0,bi, 0,0,0,as,ai], [in1])
    other -> Left ("filter primitive " ++ T.unpack other ++ " not supported (drawn unfiltered)")
  where
    (_, _, bw, bh) = pcBox pc
    sclX = if pcObb pc then bw else 1
    sclY = if pcObb pc then bh else 1
    n k d = case attr e k >>= parseNumber of
      Just (v, _) -> v
      Nothing -> d
    pair mt d = case maybe [] parseNumberList mt of
      [a] -> (a, a)
      (a : b : _) -> (a, b)
      [] -> (d, d)
    input k = inputOf (attr e k)
    inputOf mt = case fmap T.strip mt of
      Nothing -> Right Previous
      Just "" -> Right Previous
      Just "SourceGraphic" -> Right SourceGraphic
      Just "SourceAlpha" -> Right SourceAlpha
      Just v | v `elem` ["BackgroundImage", "BackgroundAlpha", "FillPaint", "StrokePaint"] ->
                 Left ("filter input " ++ T.unpack v ++ " not supported (drawn unfiltered)")
             | Just r <- M.lookup v names -> Right (Result r)
             | otherwise -> Right Previous
    -- A transfer function that is a straight line: (slope, intercept).
    transferFn (nm, ke) = case nm of
      _ | Just ch <- T.stripPrefix "feFunc" nm, T.length ch == 1 ->
            let c = T.head ch
                num k d = case attr ke k >>= parseNumber of Just (v, _) -> v; Nothing -> d
            in case fromMaybe "identity" (attr ke "type") of
                 "identity" -> Right (Just (c, (1, 0)))
                 "linear" -> Right (Just (c, (num "slope" 1, num "intercept" 0)))
                 "table" -> case parseNumberList (fromMaybe "" (attr ke "tableValues")) of
                   [] -> Right (Just (c, (1, 0)))
                   [a, b] -> Right (Just (c, (b - a, a)))
                   _ -> Left "feComponentTransfer table not supported (drawn unfiltered)"
                 "gamma" | num "exponent" 1 == 1 -> Right (Just (c, (num "amplitude" 1, num "offset" 0)))
                 t -> Left ("feComponentTransfer " ++ T.unpack t ++ " not supported (drawn unfiltered)")
        | otherwise -> Right Nothing
