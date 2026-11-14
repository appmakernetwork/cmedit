{-# LANGUAGE BangPatterns #-}
-- | A PDF page as a "Cmedit.Scene" display list: the page view's front-end
-- (plan 0034 §E). Text in its real fonts, vector graphics, images,
-- shadings, transparency and annotation appearances, held against
-- @pdftoppm@ (see @docs\/plans\/bench\/pdfrendercheck.sh@).
--
-- __Pure, with fonts as a pre-pass.__ Substitute fonts live on disk and
-- loading them is IO, so the page is asked first what it needs
-- ('pageFontNeeds': every font that is not embedded or whose program does
-- not parse, as a "Cmedit.FontSys" request carrying the descriptor's
-- flags), the driver answers each through "Cmedit.FontProvider", and
-- 'renderPage' runs against a pure lookup ('reFonts'). The shape is
-- "Cmedit.Svg"'s @svgFontQueries@ + @SvgEnv@.
--
-- __Scene space__ is the displayed page: y-down PDF points over the crop
-- box (clipped to the media box), after @\/Rotate@, so @scBounds@ is
-- @Rect 0 0 w h@ and the first command is the paper. 'pageGeometry' is the
-- PDF-default-space → scene matrix every page starts its CTM from.
--
-- The load-bearing decisions:
--
--   * __Clips are tracked, not replayed.__ PDF's clip is graphics state
--     (intersected by @W n@, restored by @Q@), the scene's is a container.
--     Each clip level gets a unique id when it is created, the builder keeps
--     the chain of clips currently open, and emitting a command under a
--     given chain closes and opens only the levels that differ (compared by
--     id, outermost first). Consecutive objects under one clip therefore
--     share one 'CClip', and @q W n … Q@ needs no bookkeeping at all: the
--     restored state simply names a shorter chain.
--
--   * __Every sub-stream is built in its own coordinates and shared__
--     through 'CTransform': a form XObject (memoised per object and
--     inherited state, so a letterhead on every page or a form drawn per
--     table cell is interpreted once), a Type 3 glyph procedure (memoised per
--     font, code and inherited state, which is what makes TeX's bitmap fonts
--     cheap), a tiling pattern's cell, a soft mask's group. A sub-context
--     starts with an identity CTM and an empty clip chain, and whatever of
--     the inherited state is positional is re-expressed in its space by
--     'mapGsInto' (a soft mask's commands, a pattern's base matrix).
--
--   * __A pattern's matrix is relative to the default space of the stream
--     it is used in__, not to the CTM: the page's default space at page
--     level, the form's space inside a form (§8.7.3.1). 'cxPatBase' is that
--     space in the context's root coordinates, and a paint's matrix (which
--     the scene defines relative to the user space of the command using it)
--     is @inverse(command matrix) ∘ base ∘ \/Matrix@.
--
--   * __Transparency is per object, as PDF defines it__: @ca@/@CA@, a blend
--     mode or a soft mask wrap each painted object in its own one-leaf
--     'CGroup' (which the rasteriser paints directly, without a layer); a
--     solid paint with only an alpha folds it into the colour instead. A
--     @B@ with alpha is two objects, fill then stroke, exactly as poppler
--     paints it. A transparency group XObject is one 'CGroup' (isolated and
--     knockout from its @\/Group@) carrying the gstate's alpha, blend and
--     mask, its content starting from reset ones (§11.6.6). A soft mask's
--     group is drawn with the CTM in force when @gs@ set it.
--
--   * __Text positioning is PDF's arithmetic exactly__ (§9.4.4:
--     @tx = ((w0 − Tj\/1000)·Tfs + Tc + Tw)·Th@, @Tw@ only for a one-byte
--     code 32), with the widths from the font dictionary, never the
--     program's, so text lands where @pdftoppm@ puts it whatever face draws
--     it. Glyphs go out as one 'CGlyphs' run per string with keys unique per
--     (font, GID); render modes 4–7 collect glyph outlines into a clip that
--     @ET@ applies. The font rules are "Cmedit.PdfRenderFont".
--
-- __Never a crash, always a bound.__ One 'Fuel' (the document's) pays for
-- every token and every decode; the page also stops at 'maxPageCmds'
-- commands and 'maxPageGlyphs' glyphs. __A shared body is charged its
-- expanded weight on every use__ ('chargeReuse'): sharing makes the scene
-- small, but whatever consumes it walks each use, so sixteen nested forms
-- or Type 3 fonts each used K times are a K^16 walk in a 4 KB file unless
-- the cap counts the expansion; such a page stops with "drawing truncated:
-- too complex". The clip chain is capped at the rasteriser's nesting
-- ('maxClipDepth'), image memory per image and per page ("Image memory"
-- below), and the page nests forms, patterns, Type 3 glyphs
-- and masks at most 'maxNest' deep with a cycle guard, and keeps at most
-- 'maxGsDepth' saved states. Malformed constructs are skipped; a page that
-- stops early keeps what it drew and says why in 'scNotes'. Optional
-- content that is off by default (§8.11) is not drawn; annotations with an
-- appearance stream are (unless hidden), fitted to their @\/Rect@ by
-- §12.5.5's algorithm.
module Cmedit.PdfRender
  ( RenderEnv(..)
  , defaultRenderEnv
  , renderPage
  , renderPagePrint
  , pageFontNeeds
  , pageGeometry
    -- * Limits
  , maxPageCmds
  , maxPageGlyphs
  , maxNest
  , maxGsDepth
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.State.Strict
import Data.Bits (testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (foldl', nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Array.Unboxed (listArray)

import Cmedit.Font (Font)
import qualified Cmedit.Font as F
import Cmedit.FontSys (FontRequest(..), fontRequest)
import Cmedit.Geom
import Cmedit.Image (Image(..))
import Cmedit.PdfFunc (parseFunction, evalFunction)
import Cmedit.PdfImage
import Cmedit.PdfObj
import Cmedit.PdfRenderFont
import Cmedit.PdfRenderShade
import Cmedit.RasterGeom (strokeToPath)
import Cmedit.Raster (prescaleRGBA, prescaleMask)
import Cmedit.Scene

------------------------------------------------------------------------------
-- Public API

data RenderEnv = RenderEnv
  { reFonts       :: FontRequest -> Maybe (Int, Font)
      -- ^ A substitute for a font the page does not embed (or embeds
      -- unreadably): the "Cmedit.FontProvider" answer to one of
      -- 'pageFontNeeds', with its face key.
  , reAnnotations :: Bool    -- ^ Draw annotations (appearance streams, and the few poppler generates).
  }

-- | No fonts, annotations shown.
defaultRenderEnv :: RenderEnv
defaultRenderEnv = RenderEnv (const Nothing) True

maxPageCmds, maxPageGlyphs, maxNest, maxGsDepth, maxPageTris :: Int
-- | Drawing commands one page may emit.
maxPageCmds = 1000000
-- | Glyphs one page may draw.
maxPageGlyphs = 2000000
-- | Nesting of forms, patterns, Type 3 glyphs and soft masks.
maxNest = 16
-- | Saved graphics states (@q@ without @Q@).
maxGsDepth = 256
-- | Mesh triangles one page may draw.
maxPageTris = 200000

-- | The displayed page and the PDF-default-space → scene matrix: crop box
-- (clipped to the media box) and @\/Rotate@ (clockwise).
pageGeometry :: PdfFile -> Dict -> (Rect, Affine)
pageGeometry f page = (Rect 0 0 w h, m)
  where
    Rect x0 y0 x1 y1 = pageCropBox f page
    cw = x1 - x0; ch = y1 - y0
    rot = pageRotation f page
    (w, h) = if rot == 90 || rot == 270 then (ch, cw) else (cw, ch)
    m = case rot of
      90  -> Affine 0 1 1 0 (-y0) (-x0)
      180 -> Affine (-1) 0 0 1 x1 (-y0)
      270 -> Affine 0 (-1) (-1) 0 y1 x1
      _   -> Affine 1 0 0 (-1) (-x0) y1

------------------------------------------------------------------------------
-- Interpreter state

data ClipLevel = ClipLevel !Int [ClipShape]

data PatRef = PatRef
  { prObj   :: Obj       -- ^ The pattern as the resource dictionary holds it.
  , prKey   :: !Int      -- ^ Its object number, or -1.
  , prBase  :: !Affine   -- ^ Pattern default space → the context's root.
  , prColor :: Maybe Color  -- ^ An uncoloured pattern's colour.
  }

data PaintSpec = PNone | PColor !Color | PPat PatRef

data GS = GS
  { gCtm      :: !Affine
  , gClip     :: [ClipLevel]          -- ^ Innermost first.
  , gFillCS   :: ColorSpace
  , gStrokeCS :: ColorSpace
  , gFill     :: PaintSpec
  , gStroke   :: PaintSpec
  , gLW       :: !Double
  , gCap      :: !LineCap
  , gJoin     :: !LineJoin
  , gMiter    :: !Double
  , gDash     :: [Double]
  , gPhase    :: !Double
  , gSA       :: !Bool
  , gFillA    :: !Double
  , gStrokeA  :: !Double
  , gBlend    :: !Blend
  , gMask     :: Maybe SoftMask
  , gFont     :: Maybe (Int, PFont)    -- ^ Identity (for memo keys) and font.
  , gFs, gTc, gTw, gTh, gTL, gTs :: !Double
  , gTr       :: !Int
  }

initGS :: Affine -> GS
initGS m = GS
  { gCtm = m, gClip = [], gFillCS = CSGray, gStrokeCS = CSGray
  , gFill = PColor black, gStroke = PColor black
  , gLW = 1, gCap = CapButt, gJoin = JoinMiter, gMiter = 10, gDash = [], gPhase = 0, gSA = False
  , gFillA = 1, gStrokeA = 1, gBlend = BlendNormal, gMask = Nothing
  , gFont = Nothing, gFs = 0, gTc = 0, gTw = 0, gTh = 1, gTL = 0, gTs = 0, gTr = 0 }

data OpenClip = OpenClip !Int [ClipShape] [Cmd]

-- One content stream's interpretation context.
data Ctx = Ctx
  { cxGS       :: GS
  , cxStack    :: [GS]
  , cxDepthQ   :: !Int
  , cxPath     :: [Seg]           -- ^ Reversed.
  , cxCur      :: Maybe Pt
  , cxStart    :: Maybe Pt
  , cxClipNext :: Maybe FillRule
  , cxOpen     :: [OpenClip]      -- ^ Innermost first.
  , cxRoot     :: [Cmd]           -- ^ Reversed.
  , cxRes      :: Dict
  , cxRegion   :: Rect            -- ^ What the stream can paint, root coordinates.
  , cxPatBase  :: !Affine
  , cxToScene  :: Maybe Affine    -- ^ Root → scene, when known (resolution-dependent choices only: image reduction, interpolation). Unknown inside a tiling cell, whose placement is the paint's.
  , cxTm       :: !Affine
  , cxTlm      :: !Affine
  , cxTextClip :: Maybe [ClipShape]
  , cxMC       :: [Bool]          -- ^ Marked-content levels: True = hidden.
  , cxHidden   :: !Int
  , cxLocked   :: !Bool           -- ^ Colour operators ignored (d1 glyphs, uncoloured patterns).
  , cxNest     :: !Int
  , cxVisit    :: IS.IntSet
  , cxCS       :: M.Map BS.ByteString ColorSpace
  , cxResKey   :: !Int
      -- ^ Names the resource dictionary in force, for memo keys: a body that
      -- inherits its resources draws differently under different ones.
  }

newCtx :: GS -> Dict -> Rect -> Affine -> Maybe Affine -> Int -> IS.IntSet -> Ctx
newCtx gs res region patBase toScene nest visit = Ctx
  { cxGS = gs, cxStack = [], cxDepthQ = 0, cxPath = [], cxCur = Nothing, cxStart = Nothing
  , cxClipNext = Nothing, cxOpen = [], cxRoot = [], cxRes = res, cxRegion = region
  , cxPatBase = patBase, cxToScene = toScene, cxTm = identityA, cxTlm = identityA
  , cxTextClip = Nothing, cxMC = [], cxHidden = 0, cxLocked = False
  , cxNest = nest, cxVisit = visit, cxCS = M.empty, cxResKey = 0 }

data NoteK
  = NUnsupImage String
  | NCorruptImage
  | NFont String
  | NMisc String
  deriving (Eq, Ord)

data RS = RS
  { rsFuel    :: !Fuel
  , rsCtx     :: Ctx
  , rsFile    :: PdfFile
  , rsEnv     :: RenderEnv
  , rsOC      :: OCConfig
  , rsFonts   :: IM.IntMap PFont
  , rsImages  :: IM.IntMap (Either ImageFailure PdfImg, (Int, Int))
      -- ^ Decoded (and reduced) images by object number, with the size they were reduced for.
  , rsImgPixels :: !Int     -- ^ Image pixels the page holds so far ('maxPageImagePixels').
  , rsShades  :: IM.IntMap (Either String Shading)
  , rsForms   :: M.Map (Int, String) (Int, [Cmd])          -- ^ Each body with its expanded weight ('chargeReuse').
  , rsT3      :: M.Map (Int, Int, String) (Int, [Cmd])
  , rsTiles   :: M.Map (Int, String) (Int, Int, [Cmd])     -- ^ Tile key, weight, cell.
  , rsMasks   :: M.Map Int (Int, [Cmd])
  , rsMaskKey :: M.Map (Int, String) Int
  , rsNext    :: !Int
  , rsCmds    :: !Int
  , rsGlyphs  :: !Int
  , rsTris    :: !Int
  , rsNotes   :: M.Map NoteK Int
  , rsStop    :: Maybe String
  , rsBlended :: !Bool      -- ^ Some object blends non-normally.
  , rsPrint   :: !Bool      -- ^ Print mode (annotation visibility).
  }

type R = State RS

getCtx :: R Ctx
getCtx = gets rsCtx

modCtx :: (Ctx -> Ctx) -> R ()
modCtx g = modify' (\s -> s { rsCtx = g (rsCtx s) })

getGS :: R GS
getGS = gets (cxGS . rsCtx)

modGS :: (GS -> GS) -> R ()
modGS g = modCtx (\c -> c { cxGS = g (cxGS c) })

file :: R PdfFile
file = gets rsFile

note :: NoteK -> R ()
note k = modify' (\s -> s { rsNotes = M.insertWith (+) k 1 (rsNotes s) })

stopWith :: String -> R ()
stopWith why = modify' (\s -> s { rsStop = Just (fromMaybe why (rsStop s)) })

newKey :: R Int
newKey = do
  s <- get
  put s { rsNext = rsNext s + 1 }
  pure (rsNext s)

-- Run a fuel-charged pure step.
withFuel :: (Fuel -> (a, Fuel)) -> R a
withFuel g = do
  s <- get
  let (a, fu) = g (rsFuel s)
  put s { rsFuel = fu }
  pure a

------------------------------------------------------------------------------
-- The builder

-- Emit one command under the current clip chain (dropped when hidden by
-- optional content or after the page stopped).
emit :: Cmd -> R ()
emit cmd = do
  s <- get
  let c = rsCtx s
  if cxHidden c > 0 || isJust (rsStop s) then pure () else
    if rsCmds s >= maxPageCmds
      then stopWith tooComplex
      else do
        let chain = gClip (cxGS c)
            -- A level's id names its whole chain (a level is only ever
            -- created on top of one parent), so equal innermost ids mean
            -- equal chains: the common case costs O(1).
            same = case (chain, cxOpen c) of
              ([], []) -> True
              (ClipLevel a _ : _, OpenClip b _ _ : _) -> a == b
              _ -> False
            target = reverse chain       -- outermost first
            open = reverse (cxOpen c)
            k = commonPrefix target open
            c2 | same = c
               | otherwise = foldl' openLevel (closeN (length open - k) c) (drop k target)
            c3 = case cxOpen c2 of
                   (OpenClip i sh kids : rest) -> c2 { cxOpen = OpenClip i sh (cmd : kids) : rest }
                   [] -> c2 { cxRoot = cmd : cxRoot c2 }
        put s { rsCtx = c3, rsCmds = rsCmds s + 1 }
  where
    commonPrefix (ClipLevel a _ : as) (OpenClip b _ _ : bs) | a == b = 1 + commonPrefix as bs
    commonPrefix _ _ = 0 :: Int
    openLevel c (ClipLevel i sh) = c { cxOpen = OpenClip i sh [] : cxOpen c }

closeN :: Int -> Ctx -> Ctx
closeN n c | n <= 0 = c
closeN n c = case cxOpen c of
  (OpenClip _ sh kids : rest) ->
    let cmd = CClip sh (reverse kids)
        c' = case rest of
          (OpenClip i sh' kids' : rest') -> c { cxOpen = OpenClip i sh' (cmd : kids') : rest' }
          [] -> c { cxOpen = [], cxRoot = cmd : cxRoot c }
    in closeN (n - 1) c'
  [] -> c

-- | What a page says when its drawing is cut short by the command cap.
tooComplex :: String
tooComplex = "drawing truncated: too complex"

-- Build a sub-stream's body, returning it with its /expanded weight/: the
-- commands it charged, including what its own shared sub-bodies charged.
buildWeighted :: Ctx -> R () -> R (Int, [Cmd])
buildWeighted inner act = do
  n0 <- gets rsCmds
  cmds <- subContext inner act
  n1 <- gets rsCmds
  pure (max 0 (n1 - n0), cmds)

-- Charge a reused body's expanded weight. A shared body is one value in the
-- scene, but every consumer of the scene (the rasteriser, the driver's
-- forcing) walks it once per use, so sixteen nested levels each used K
-- times are K^16 commands however small the scene is; charging the weight
-- per use is what makes 'maxPageCmds' bound that walk. False once over.
chargeReuse :: Int -> R Bool
chargeReuse w = do
  s <- get
  if isJust (rsStop s) then pure False
  else if rsCmds s + w > maxPageCmds then stopWith tooComplex >> pure False
  else put s { rsCmds = rsCmds s + w } >> pure True

-- Push a clip level, unless the chain is already as deep as the
-- rasteriser will nest (it would skip what lies deeper anyway).
pushClip :: [ClipShape] -> R ()
pushClip shapes = do
  gs <- getGS
  if length (take maxClipDepth (gClip gs)) >= maxClipDepth
    then note (NMisc "some clipping ignored (nested too deeply)")
    else do
      k <- newKey
      modGS (\g -> g { gClip = ClipLevel k shapes : gClip g })

-- The resource key for a sub-stream: fresh when it brings its own
-- resources, inherited otherwise; and the tag its memo key carries.
innerResKey :: Maybe Dict -> R Int
innerResKey own = case own of
  Just _ -> newKey
  Nothing -> gets (cxResKey . rsCtx)

resTag :: Maybe Dict -> Ctx -> String
resTag own c = case own of
  Just _ -> ""
  Nothing -> 'R' : show (cxResKey c)

-- | Clip levels kept in one chain: the rasteriser's 'maxNesting'.
maxClipDepth :: Int
maxClipDepth = 256

finishCtx :: Ctx -> [Cmd]
finishCtx c = reverse (cxRoot (closeN (length (cxOpen c)) c))

-- Run an action in a fresh context, returning what it drew.
subContext :: Ctx -> R () -> R [Cmd]
subContext inner act = do
  outer <- getCtx
  modify' (\s -> s { rsCtx = inner })
  act
  done <- getCtx
  modify' (\s -> s { rsCtx = outer })
  pure (finishCtx done)

-- Re-express inherited positional state in a sub-context whose root maps
-- to the current root by @n@.
mapGsInto :: Affine -> GS -> R GS
mapGsInto n gs = case invertA n of
  Nothing -> pure gs { gMask = Nothing }
  Just inv -> do
    mask' <- case gMask gs of
      Nothing -> pure Nothing
      Just sm -> do
        k <- newKey
        pure (Just sm { smKey = k, smCmds = [CTransform inv (smCmds sm)] })
    let ps (PPat pr) = PPat pr { prBase = composeA inv (prBase pr) }
        ps p = p
    pure gs { gMask = mask', gFill = ps (gFill gs), gStroke = ps (gStroke gs) }

-- A signature of the inherited state a sub-stream could depend on, for
-- memoising its content.
gsSig :: GS -> String
gsSig gs = show ( psSig (gFill gs), psSig (gStroke gs)
                , (gLW gs, show (gCap gs), show (gJoin gs), gMiter gs, gDash gs, gPhase gs, gSA gs)
                , (gFillA gs, gStrokeA gs, show (gBlend gs))
                , (fmap fst (gFont gs), gFs gs, gTc gs, gTw gs, gTh gs, gTL gs, gTs gs, gTr gs)
                , (take 24 (show (gFillCS gs)), take 24 (show (gStrokeCS gs))) )
  where
    psSig PNone = "n"
    psSig (PColor c) = show c
    psSig (PPat pr) = show (prKey pr, prBase pr, prColor pr)

------------------------------------------------------------------------------
-- Optional content

data OCConfig = OCConfig
  { ocOff     :: IS.IntSet
  , ocOn      :: IS.IntSet
  , ocBaseOff :: !Bool
  }

ocConfig :: PdfFile -> OCConfig
ocConfig f = OCConfig (refs "OFF") (refs "ON") (nameOf (lookD f dflt "BaseState") == BC.pack "OFF")
  where
    root = dictOf f (M.findWithDefault ONull (BC.pack "Root") (pfTrailer f))
    props = dictOf f (lookD f root "OCProperties")
    dflt = dictOf f (lookD f props "D")
    refs k = IS.fromList (mapMaybe refOf (take 100000 (arrOf f (lookD f dflt k))))

-- | Is an /OC entry (an OCG or OCMD, usually by reference) visible?
ocVisible :: PdfFile -> OCConfig -> Obj -> Bool
ocVisible f cfg o0 = go (8 :: Int) o0
  where
    ocgOn o = case refOf o of
      Just r | r `IS.member` ocOff cfg -> False
             | ocBaseOff cfg -> r `IS.member` ocOn cfg
      _ -> True
    go 0 _ = True
    go k o = case resolve f o of
      ODict d -> case BC.unpack (nameOf (lookD f d "Type")) of
        "OCMD" -> case M.lookup (BC.pack "VE") d of
          Just ve -> expr k ve
          Nothing ->
            let ocgs = case M.lookup (BC.pack "OCGs") d of
                  Just x -> case resolve f x of
                    OArr xs -> xs
                    ONull -> []
                    _ -> [x]
                  Nothing -> []
                states = map ocgOn (take 1000 ocgs)
            in if null states then True else case BC.unpack (nameOf (lookD f d "P")) of
                 "AllOn"  -> and states
                 "AnyOff" -> not (and states)
                 "AllOff" -> not (or states)
                 _        -> or states
        _ -> ocgOn o
      _ -> True
    expr 0 _ = True
    expr k e = case resolve f e of
      OArr (op : args) -> case BC.unpack (nameOf (resolve f op)) of
        "And" -> all (expr (k - 1)) (take 100 args)
        "Or"  -> any (expr (k - 1)) (take 100 args)
        "Not" -> case args of (a : _) -> not (expr (k - 1) a); [] -> True
        _ -> True
      _ -> ocgOn e

-- Does an XObject's or annotation's /OC hide it?
ocHides :: Dict -> R Bool
ocHides d = case M.lookup (BC.pack "OC") d of
  Nothing -> pure False
  Just o -> do
    f <- file
    cfg <- gets rsOC
    pure (not (ocVisible f cfg o))

------------------------------------------------------------------------------
-- Entry points

-- | Render one page (a dictionary from 'pdfPages', inherited attributes
-- merged). Never throws; a page that could not be fully drawn says why in
-- 'scNotes'.
renderPage :: RenderEnv -> PdfFile -> Fuel -> Dict -> (Scene, Fuel)
renderPage = renderPageMode False

-- | 'renderPage' for print: only annotations flagged Print are drawn
-- (display mode draws every annotation not flagged Hidden or NoView).
renderPagePrint :: RenderEnv -> PdfFile -> Fuel -> Dict -> (Scene, Fuel)
renderPagePrint = renderPageMode True

renderPageMode :: Bool -> RenderEnv -> PdfFile -> Fuel -> Dict -> (Scene, Fuel)
renderPageMode printing env f fu0 page = (Scene bounds cmds notes, rsFuel final)
  where
    (bounds, pageM) = pageGeometry f page
    Rect _ _ w h = bounds
    res = dictOf f (lookD f page "Resources")
    ctx0 = newCtx (initGS pageM) res bounds pageM (Just identityA) 0 IS.empty
    st0 = RS { rsFuel = fu0, rsCtx = ctx0, rsFile = f, rsEnv = env, rsOC = ocConfig f
             , rsFonts = IM.empty, rsImages = IM.empty, rsImgPixels = 0, rsShades = IM.empty
             , rsForms = M.empty, rsT3 = M.empty, rsTiles = M.empty, rsMasks = M.empty
             , rsMaskKey = M.empty, rsNext = 1, rsCmds = 0, rsGlyphs = 0, rsTris = 0
             , rsNotes = M.empty, rsStop = Nothing, rsBlended = False, rsPrint = printing }
    final = execState body st0
    body = do
      content <- withFuel (\fu -> contentsOf f fu page)
      runContent content
      env' <- gets rsEnv
      when (reAnnotations env') (drawAnnotations page pageM)
    pageCmds = finishCtx (rsCtx final)
    box = rectPath 0 0 w h
    -- Blending happens against what the page has drawn, not against the
    -- paper (poppler, Ghostscript and cairo all agree: Screen over an empty
    -- page shows the source colour), so a page that blends is an isolated
    -- group over the paper. One that does not needs no layer, and neither
    -- does one whose blends are all the same against white ('noteBlend').
    clipped = CClip [ClipShape identityA NonZero box] pageCmds
    cmds = CFill identityA NonZero (PSolid white) box
         : [ if rsBlended final then CGroup defaultGroup { goIsolated = True } [clipped] else clipped
           | not (null pageCmds) ]
    -- Notes carry names out of the file (a font's /BaseFont): no control
    -- character reaches the status line.
    notes = map (map printable) (map fmt (M.toList (rsNotes final)) ++ maybe [] (: []) (rsStop final))
    printable ch = if ch < ' ' || ch == '\DEL' then '?' else ch
    fmt (k, n) = case k of
      NUnsupImage what -> show n ++ " " ++ what ++ (if n == 1 then " image" else " images") ++ " not shown"
      NCorruptImage -> show n ++ (if n == 1 then " image" else " images") ++ " could not be decoded"
      NFont nm -> "text in font " ++ nm ++ " not shown (no font available)"
      NMisc m -> m

-- | The substitute fonts a page needs: every font reachable from its
-- resources (forms, patterns, Type 3 fonts, soft masks and annotation
-- appearances included) that is not embedded or whose embedded program does
-- not parse, as requests to "Cmedit.FontProvider". A page with an image this
-- renderer cannot decode also asks for Helvetica, which labels the
-- placeholder.
pageFontNeeds :: PdfFile -> Fuel -> Dict -> ([FontRequest], Fuel)
pageFontNeeds f fu0 page = (nub (reverse reqs ++ [ fontRequest (T.pack "Helvetica") | needLabel ]), fuEnd)
  where
    (reqs, needLabel, fuEnd, _, _) =
      foldl' visitRes ([], False, fu0, IS.empty, 0 :: Int) (pageRes : annotRes)
    pageRes = M.findWithDefault ONull (BC.pack "Resources") page
    annotRes = concat [ apStreams (lookD f a "AP") | ao <- take 2000 (arrOf f (lookD f page "Annots"))
                                                   , let a = dictOf f ao ]
    apStreams ap = concat [ case resolve f (lookD f (dictOf f ap) k) of
                              OStream sd _ -> [M.findWithDefault ONull (BC.pack "Resources") sd]
                              ODict sd -> [ M.findWithDefault ONull (BC.pack "Resources") (dictOf f v) | v <- M.elems sd ]
                              _ -> []
                          | k <- ["N"] ]
    visitRes st@(acc, lab, fu, seen, n) ro
      | n > 4000 = st
      | Just r <- refOf ro, r `IS.member` seen = st
      | otherwise =
          let seen' = maybe seen (`IS.insert` seen) (refOf ro)
              res = dictOf f ro
              sub k = dictOf f (lookD f res k)
              fonts = M.elems (sub "Font")
              (acc1, fu1, seen1) = foldl' visitFont (acc, fu, seen') fonts
              xobjs = [ (o, sd) | o <- M.elems (sub "XObject"), OStream sd _ <- [resolve f o] ]
              lab' = lab || any (\(_, sd) -> nameOf (lookD f sd "Subtype") == BC.pack "Image"
                                   && any (\(fl, _) -> fl `elem` map BC.pack ["JPXDecode", "JBIG2Decode"]) (streamFilters f sd)) xobjs
              childRes = [ M.findWithDefault ONull (BC.pack "Resources") sd
                         | (_, sd) <- xobjs, nameOf (lookD f sd "Subtype") == BC.pack "Form" ]
                      ++ [ M.findWithDefault ONull (BC.pack "Resources") (dictOf f p) | p <- M.elems (sub "Pattern") ]
                      ++ [ M.findWithDefault ONull (BC.pack "Resources") (dictOf f fo)
                         | fo <- fonts, nameOf (lookD f (dictOf f fo) "Subtype") == BC.pack "Type3" ]
                      ++ [ M.findWithDefault ONull (BC.pack "Resources") (dictOf f g)
                         | gso <- M.elems (sub "ExtGState"), let gd = dictOf f gso
                         , ODict sm <- [lookD f gd "SMask"], let g = M.findWithDefault ONull (BC.pack "G") sm ]
              gsFonts = [ fo | gso <- M.elems (sub "ExtGState"), (fo : _) <- [arrOf f (lookD f (dictOf f gso) "Font")] ]
              (acc2, fu2, seen2) = foldl' visitFont (acc1, fu1, seen1) gsFonts
          in foldl' visitRes (acc2, lab', fu2, seen2, n + 1) childRes
    visitFont (acc, fu, seen) fo
      | Just r <- refOf fo, r `IS.member` seen = (acc, fu, seen)
      | IS.size seen > 20000 = (acc, fu, seen)
      | otherwise =
          let seen' = maybe seen (`IS.insert` seen) (refOf fo)
              (need, fu') = fontNeed f fu (dictOf f fo)
          in (maybe acc (: acc) need, fu', seen')

------------------------------------------------------------------------------
-- The content-stream loop

maxOperands :: Int
maxOperands = 64

runContent :: BS.ByteString -> R ()
runContent bytes = go (contentTokensInline bytes) [] 0
  where
    go [] _ _ = pure ()
    go (t : ts) ops !n = do
      s <- get
      if isJust (rsStop s) then pure ()
      else if fuelSpent (rsFuel s) then stopWith "page truncated: the document's budget is spent"
      else do
        put s { rsFuel = spendTokens 1 (rsFuel s) }
        case t of
          TObj o -> if n >= maxOperands then go ts ops n else go ts (o : ops) (n + 1)
          TInline d raw -> inlineImage d raw >> go ts [] 0
          TOp op -> operator op (reverse ops) >> go ts [] 0

num :: PdfFile -> Obj -> Double
num f o = case resolve f o of
  ONum v | not (isNaN v || isInfinite v) -> v
  _ -> 0

operator :: BS.ByteString -> [Obj] -> R ()
operator op args = do
  f <- file
  let nums = map (num f) args
      n6 = case nums of [a, b, c, d, e, g] -> Just (Affine a b c d e g); _ -> Nothing
  case BC.unpack op of
    -- Graphics state
    "q" -> modCtx (\c -> if cxDepthQ c >= maxGsDepth then c
                         else c { cxStack = cxGS c : cxStack c, cxDepthQ = cxDepthQ c + 1 })
    "Q" -> modCtx (\c -> case cxStack c of
                           (g : rest) -> c { cxGS = (g { gFont = gFont g }), cxStack = rest, cxDepthQ = cxDepthQ c - 1 }
                           [] -> c)
    "cm" -> forM_ n6 $ \m -> modGS (\g -> g { gCtm = composeA (gCtm g) m })
    "w" -> case nums of [v] -> modGS (\g -> g { gLW = abs v }); _ -> pure ()
    "J" -> case nums of [v] -> modGS (\g -> g { gCap = capOf v }); _ -> pure ()
    "j" -> case nums of [v] -> modGS (\g -> g { gJoin = joinOf v }); _ -> pure ()
    "M" -> case nums of [v] -> modGS (\g -> g { gMiter = max 1 v }); _ -> pure ()
    "d" -> case args of
      [a, p] -> modGS (\g -> g { gDash = dashOf (map (num f) (arrOf f a)), gPhase = num f p })
      _ -> pure ()
    "ri" -> pure ()
    "i" -> pure ()
    "gs" -> case args of [OName nm] -> extGState nm; _ -> pure ()
    -- Paths
    "m" -> case nums of [x, y] -> moveTo (Pt x y); _ -> pure ()
    "l" -> case nums of [x, y] -> lineTo (Pt x y); _ -> pure ()
    "c" -> case nums of [a, b, c, d, e, g] -> curveTo (Just (Pt a b)) (Pt c d) (Pt e g); _ -> pure ()
    "v" -> case nums of [c, d, e, g] -> curveTo Nothing (Pt c d) (Pt e g); _ -> pure ()
    "y" -> case nums of [a, b, e, g] -> curveTo (Just (Pt a b)) (Pt e g) (Pt e g); _ -> pure ()
    "h" -> closePath
    "re" -> case nums of
      [x, y, w, h] -> do
        modCtx (\c -> c { cxPath = reverse (rectPath x y w h) ++ cxPath c, cxCur = Just (Pt x y), cxStart = Just (Pt x y) })
      _ -> pure ()
    "S" -> paintPath False True Nothing
    "s" -> closePath >> paintPath False True Nothing
    "f" -> paintPath True False (Just NonZero)
    "F" -> paintPath True False (Just NonZero)
    "f*" -> paintPath True False (Just EvenOdd)
    "B" -> paintPath True True (Just NonZero)
    "B*" -> paintPath True True (Just EvenOdd)
    "b" -> closePath >> paintPath True True (Just NonZero)
    "b*" -> closePath >> paintPath True True (Just EvenOdd)
    "n" -> paintPath False False Nothing
    "W" -> modCtx (\c -> c { cxClipNext = Just NonZero })
    "W*" -> modCtx (\c -> c { cxClipNext = Just EvenOdd })
    -- Colour
    "CS" -> case args of [o] -> setCS True o; _ -> pure ()
    "cs" -> case args of [o] -> setCS False o; _ -> pure ()
    "SC" -> setColor True args
    "SCN" -> setColor True args
    "sc" -> setColor False args
    "scn" -> setColor False args
    "G" -> deviceColor True "DeviceGray" CSGray nums
    "g" -> deviceColor False "DeviceGray" CSGray nums
    "RG" -> deviceColor True "DeviceRGB" CSRGB nums
    "rg" -> deviceColor False "DeviceRGB" CSRGB nums
    "K" -> deviceColor True "DeviceCMYK" CSCMYK nums
    "k" -> deviceColor False "DeviceCMYK" CSCMYK nums
    -- Shadings, XObjects
    "sh" -> case args of [OName nm] -> shadeOp nm; _ -> pure ()
    "Do" -> case args of [OName nm] -> doXObject nm; _ -> pure ()
    -- Text
    "BT" -> modCtx (\c -> c { cxTm = identityA, cxTlm = identityA, cxTextClip = Nothing })
    "ET" -> endText
    "Tc" -> case nums of [v] -> modGS (\g -> g { gTc = v }); _ -> pure ()
    "Tw" -> case nums of [v] -> modGS (\g -> g { gTw = v }); _ -> pure ()
    "Tz" -> case nums of [v] -> modGS (\g -> g { gTh = v / 100 }); _ -> pure ()
    "TL" -> case nums of [v] -> modGS (\g -> g { gTL = v }); _ -> pure ()
    "Tr" -> case nums of [v] -> modGS (\g -> g { gTr = max 0 (min 7 (truncate v)) }); _ -> pure ()
    "Ts" -> case nums of [v] -> modGS (\g -> g { gTs = v }); _ -> pure ()
    "Tf" -> case args of
      [OName nm, sz] -> selectFontByName nm (num f sz)
      _ -> pure ()
    "Td" -> case nums of [x, y] -> textMove x y; _ -> pure ()
    "TD" -> case nums of [x, y] -> modGS (\g -> g { gTL = -y }) >> textMove x y; _ -> pure ()
    "Tm" -> forM_ n6 $ \m -> modCtx (\c -> c { cxTm = m, cxTlm = m })
    "T*" -> nextLine
    "Tj" -> case args of [OStr s] -> showText s; _ -> pure ()
    "'" -> case args of [OStr s] -> nextLine >> showText s; _ -> pure ()
    "\"" -> case args of
      [aw, ac, OStr s] -> do
        modGS (\g -> g { gTw = num f aw, gTc = num f ac })
        nextLine >> showText s
      _ -> pure ()
    "TJ" -> case args of [OArr xs] -> showArray xs; _ -> pure ()
    -- Type 3 glyph metrics
    "d0" -> pure ()
    "d1" -> modCtx (\c -> c { cxLocked = True })
    -- Marked content
    "BMC" -> pushMC False
    "BDC" -> case args of
      [OName tag, props] | tag == BC.pack "OC" -> do
        hidden <- ocProps props
        pushMC hidden
      _ -> pushMC False
    "EMC" -> modCtx (\c -> case cxMC c of
                             (h : rest) -> c { cxMC = rest, cxHidden = cxHidden c - (if h then 1 else 0) }
                             [] -> c)
    _ -> pure ()
  where
    capOf v = case (truncate v :: Int) of 1 -> CapRound; 2 -> CapSquare; _ -> CapButt
    joinOf v = case (truncate v :: Int) of 1 -> JoinRound; 2 -> JoinBevel; _ -> JoinMiter
    pushMC h = modCtx (\c -> if length (cxMC c) > 1000 then c
                             else c { cxMC = h : cxMC c, cxHidden = cxHidden c + (if h then 1 else 0) })

dashOf :: [Double] -> [Double]
dashOf xs
  | null xs || any (< 0) xs || all (== 0) xs = []
  | otherwise = take 64 xs

ocProps :: Obj -> R Bool
ocProps props = do
  f <- file
  c <- getCtx
  cfg <- gets rsOC
  let o = case props of
        OName nm -> M.findWithDefault ONull nm (dictOf f (lookD f (cxRes c) "Properties"))
        x -> x
  pure (not (ocVisible f cfg o))

------------------------------------------------------------------------------
-- Paths

moveTo, lineTo :: Pt -> R ()
moveTo p = modCtx (\c -> c { cxPath = MoveTo p : cxPath c, cxCur = Just p, cxStart = Just p })
lineTo p = modCtx (\c -> case cxCur c of
  Nothing -> c { cxPath = MoveTo p : cxPath c, cxCur = Just p, cxStart = Just p }
  Just _ -> c { cxPath = LineTo p : cxPath c, cxCur = Just p })

curveTo :: Maybe Pt -> Pt -> Pt -> R ()
curveTo mc1 c2 p = modCtx (\c -> case cxCur c of
  Nothing -> c
  Just cur -> let c1 = fromMaybe cur mc1
              in c { cxPath = CubicTo c1 c2 p : cxPath c, cxCur = Just p })

closePath :: R ()
closePath = modCtx (\c -> case cxCur c of
  Nothing -> c
  Just _ -> c { cxPath = ClosePath : cxPath c, cxCur = cxStart c })

strokeStyle :: GS -> StrokeStyle
strokeStyle g = StrokeStyle (gLW g) (gCap g) (gJoin g) (gMiter g) (gDash g) (gPhase g) (gSA g)

-- Paint the current path (fill rule when filling), then apply a pending
-- clip, then clear the path.
paintPath :: Bool -> Bool -> Maybe FillRule -> R ()
paintPath doFill doStroke rule = do
  c <- getCtx
  let path = reverse (cxPath c)
      gs = cxGS c
      ctm = gCtm gs
  unless (null path) $ do
    when doFill $ fillShape False (gFill gs) (fromMaybe NonZero rule) ctm path
    when doStroke $ strokeShape gs ctm path
  case cxClipNext c of
    Just r -> pushClip [ClipShape ctm r path]
    Nothing -> pure ()
  modCtx (\c' -> c' { cxPath = [], cxCur = Nothing, cxStart = Nothing, cxClipNext = Nothing })

fillShape :: Bool -> PaintSpec -> FillRule -> Affine -> Path -> R ()
fillShape _ spec rule ctm path = do
  pp <- resolvePaint False spec ctm
  case pp of
    PaintNone -> pure ()
    PaintIs p bg -> do
      forM_ bg $ \b -> emitPainted False (PSolid b) (\q -> CFill ctm rule q path)
      emitPainted False p (\q -> CFill ctm rule q path)
    PaintMesh m body bg -> do
      forM_ bg $ \b -> emitPainted False (PSolid b) (\q -> CFill ctm rule q path)
      meshInto False [ClipShape ctm rule path] m body

strokeShape :: GS -> Affine -> Path -> R ()
strokeShape gs ctm path = do
  pp <- resolvePaint True (gStroke gs) ctm
  let ss = strokeStyle gs
  case pp of
    PaintNone -> pure ()
    PaintIs p _ -> emitPainted True p (\q -> CStroke ctm ss q path)
    PaintMesh m body _ ->
      let outline = strokeToPath ss 0.05 path
      in unless (null outline) $ meshInto True [ClipShape ctm NonZero outline] m body

-- Emit a command painted with a paint under the gstate's alpha, blend and
-- soft mask.
emitPainted :: Bool -> Paint -> (Paint -> Cmd) -> R ()
emitPainted stroke paint mk = do
  gs <- getGS
  let a = if stroke then gStrokeA gs else gFillA gs
  case (paint, gMask gs, gBlend gs) of
    (PSolid (Color r g b ca), Nothing, BlendNormal) ->
      when (ca * a > 0) $ emit (mk (PSolid (Color r g b (ca * a))))
    _ -> emitWrapped a (mk paint)

-- Emit a command under the gstate's transparency with an explicit alpha.
emitWrapped :: Double -> Cmd -> R ()
emitWrapped a cmd = do
  gs <- getGS
  if a >= 1 && isNothing (gMask gs) && gBlend gs == BlendNormal
    then emit cmd
    else when (a > 0 || gBlend gs /= BlendNormal) $ do
      noteBlend (gBlend gs)
      emit (CGroup defaultGroup { goOpacity = max 0 (min 1 a), goBlend = gBlend gs, goMask = gMask gs } [cmd])

-- Record a blend that would come out differently against the paper than
-- against an empty page (see 'renderPage'). Multiply and Darken with white
-- are the source itself, and they are most of the blending real files do,
-- so they do not cost the page a layer.
noteBlend :: Blend -> R ()
noteBlend b = unless (b `elem` [BlendNormal, BlendMultiply, BlendDarken]) $
  modify' (\st -> st { rsBlended = True })

-- A mesh pattern painted through clip shapes.
meshInto :: Bool -> [ClipShape] -> Affine -> ShadeBody -> R ()
meshInto stroke shapes m body = do
  gs <- getGS
  let a = if stroke then gStrokeA gs else gFillA gs
  mc <- meshCmd m body
  forM_ mc $ \cmd -> emitWrapped a (CClip shapes [cmd])

meshCmd :: Affine -> ShadeBody -> R (Maybe Cmd)
meshCmd m body = do
  s <- get
  let n = case body of ShTris ts -> length ts; ShRamp _ ts -> length ts; _ -> 0
  if rsTris s + n > maxPageTris
    then note (NMisc "some shaded meshes not drawn (too many triangles)") >> pure Nothing
    else do
      put s { rsTris = rsTris s + n }
      pure $ case body of
        ShTris ts -> Just (CGouraud m ts)
        ShRamp st ts -> Just (CGouraudRamp m st ts)
        ShPaint p -> Just (CFill m NonZero p [])
        ShNothing -> Nothing

------------------------------------------------------------------------------
-- Colour

data Resolved
  = PaintNone
  | PaintIs Paint (Maybe Color)                 -- ^ A paint for the command, and a pattern's background.
  | PaintMesh Affine ShadeBody (Maybe Color)    -- ^ A mesh in the root's coordinates, painted through a clip.

-- A paint spec as a paint for a command whose matrix is @cmdM@.
resolvePaint :: Bool -> PaintSpec -> Affine -> R Resolved
resolvePaint _ PNone _ = pure PaintNone
resolvePaint _ (PColor c) _ = pure (PaintIs (PSolid c) Nothing)
resolvePaint _ (PPat pr) cmdM = do
  f <- file
  let pd = dictOf f (prObj pr)
      pm = fromMaybe identityA (matrixOf f (lookD f pd "Matrix"))
      toRoot = composeA (prBase pr) pm
  case invertA cmdM of
    Nothing -> pure PaintNone
    Just inv -> do
      let toUser = composeA inv toRoot
      case intOf f (M.lookup (BC.pack "PatternType") pd) of
        1 -> do
          mt <- tilingPaint pr pd
          pure (maybe PaintNone (\(k, cell, xs, ys, cmds) -> PaintIs (PTiling k toUser cell xs ys cmds) Nothing) mt)
        2 -> do
          let sho = M.findWithDefault ONull (BC.pack "Shading") pd
          sh <- shadingOf sho
          case sh of
            Left _ -> pure PaintNone
            Right s -> case shBody s of
              ShPaint p -> pure (PaintIs (retargetPaint toUser p) (shBackground s))
              ShNothing -> pure PaintNone
              body -> pure (PaintMesh toRoot body (shBackground s))
        _ -> pure PaintNone

shadingOf :: Obj -> R (Either String Shading)
shadingOf o = do
  f <- file
  s <- get
  let res = cxRes (rsCtx s)
  case refOf o of
    Just r | Just v <- IM.lookup r (rsShades s) -> pure v
    mr -> do
      v <- withFuel (\fu -> parseShading f fu res o)
      forM_ mr $ \r -> modify' (\st -> st { rsShades = IM.insert r v (rsShades st) })
      case v of
        Left e -> note (NMisc ("a shading was not drawn: " ++ take 80 e))
        Right _ -> pure ()
      pure v

-- A tiling pattern's cell content: (key, cell, xstep, ystep, commands).
tilingPaint :: PatRef -> Dict -> R (Maybe (Int, Rect, Double, Double, [Cmd]))
tilingPaint pr pd = do
  f <- file
  c <- getCtx
  let bbox = rectOf f (lookD f pd "BBox")
      xs = num f (lookD f pd "XStep")
      ys = num f (lookD f pd "YStep")
      uncoloured = intOf f (M.lookup (BC.pack "PaintType") pd) == 2
      colourSig = if uncoloured then show (prColor pr) else ""
      memoKey = (prKey pr, colourSig)
  case bbox of
    Nothing -> pure Nothing
    Just cell
      | xs == 0 || ys == 0 || rectEmpty cell -> pure Nothing
      | cxNest c >= maxNest -> note (NMisc "patterns nested too deeply") >> pure Nothing
      | Just r <- refOf (prObj pr), r `IS.member` cxVisit c -> pure Nothing
      | otherwise -> do
          memo <- gets rsTiles
          let ownRes = case lookD f pd "Resources" of
                ODict r -> Just r
                _ -> Nothing
              memoable = prKey pr >= 0
              memoKey' = (fst memoKey, snd memoKey ++ resTag ownRes c)
          case (memoable, M.lookup memoKey' memo) of
            (True, Just (k, w, cmds)) -> do
              ok <- chargeReuse w
              pure (if ok then Just (k, cell, abs xs, abs ys, cmds) else Nothing)
            _ -> do
              bytes <- streamBytes (prObj pr)
              let res = fromMaybe (cxRes c) ownRes
                  col = fromMaybe black (prColor pr)
                  gs0 = (initGS identityA) { gFill = PColor col, gStroke = PColor col }
                  visit = maybe (cxVisit c) (`IS.insert` cxVisit c) (refOf (prObj pr))
              rk <- innerResKey ownRes
              let inner = (newCtx gs0 res cell identityA Nothing (cxNest c + 1) visit) { cxLocked = uncoloured, cxResKey = rk }
              (w, cmds) <- buildWeighted inner (runContent bytes)
              k <- newKey
              when memoable $ modify' (\s -> s { rsTiles = M.insert memoKey' (k, w, cmds) (rsTiles s) })
              pure (Just (k, cell, abs xs, abs ys, cmds))

-- A stream's decoded bytes (empty on failure), charged to fuel.
streamBytes :: Obj -> R BS.ByteString
streamBytes o = do
  f <- file
  case resolve f o of
    OStream d raw -> do
      r <- withFuel (\fu -> decodeStreamF f fu d raw)
      pure (either (const BS.empty) id r)
    _ -> pure BS.empty

lookupCS :: Obj -> R (Maybe ColorSpace)
lookupCS o = do
  f <- file
  c <- getCtx
  case o of
    OName nm | Just cs <- M.lookup nm (cxCS c) -> pure (Just cs)
    _ -> do
      r <- withFuel (\fu -> parseColorSpace f fu (cxRes c) o)
      case (r, o) of
        (Right cs, OName nm) -> modCtx (\c' -> c' { cxCS = M.insert nm cs (cxCS c') }) >> pure (Just cs)
        (Right cs, _) -> pure (Just cs)
        (Left _, _) -> pure Nothing

setCS :: Bool -> Obj -> R ()
setCS stroke o = do
  c <- getCtx
  unless (cxLocked c) $ do
    mcs <- lookupCS o
    forM_ mcs $ \cs -> do
      let spec = case cs of
            CSPattern _ -> PNone
            _ | csInvisible cs -> PNone
              | otherwise -> PColor (paintColor cs (initialColor cs) 1)
      modGS (\g -> if stroke then g { gStrokeCS = cs, gStroke = spec } else g { gFillCS = cs, gFill = spec })

setColor :: Bool -> [Obj] -> R ()
setColor stroke args = do
  c <- getCtx
  f <- file
  unless (cxLocked c) $ do
    let gs = cxGS c
        cs = if stroke then gStrokeCS gs else gFillCS gs
        nums = [ num f a | a <- args, isNum (resolve f a) ]
        isNum (ONum _) = True
        isNum _ = False
    spec <- case cs of
      CSPattern under -> case [ nm | OName nm <- args ] of
        (nm : _) -> do
          let pats = dictOf f (lookD f (cxRes c) "Pattern")
              po = M.findWithDefault ONull nm pats
              col = fmap (\u -> paintColor u nums 1) under
          pure (if resolve f po == ONull then PNone
                else PPat (PatRef po (fromMaybe (-1) (refOf po)) (cxPatBase c) col))
        [] -> pure PNone
      _ | csInvisible cs -> pure PNone
        | otherwise -> pure (PColor (paintColor cs nums 1))
    modGS (\g -> if stroke then g { gStroke = spec } else g { gFill = spec })

-- g/rg/k: the device space, or the scope's Default* replacement for it.
deviceColor :: Bool -> String -> ColorSpace -> [Double] -> R ()
deviceColor stroke name dflt nums = do
  c <- getCtx
  f <- file
  unless (cxLocked c) $ do
    let named = dictOf f (lookD f (cxRes c) "ColorSpace")
        defKey = BC.pack (case name of "DeviceGray" -> "DefaultGray"; "DeviceRGB" -> "DefaultRGB"; _ -> "DefaultCMYK")
    cs <- if M.member defKey named
            then fromMaybe dflt <$> lookupCS (OName (BC.pack name))
            else pure dflt
    let spec = PColor (paintColor cs nums 1)
    modGS (\g -> if stroke then g { gStrokeCS = cs, gStroke = spec } else g { gFillCS = cs, gFill = spec })

------------------------------------------------------------------------------
-- ExtGState

extGState :: BS.ByteString -> R ()
extGState nm = do
  f <- file
  c <- getCtx
  let d = dictOf f (M.findWithDefault ONull nm (dictOf f (lookD f (cxRes c) "ExtGState")))
      has k = M.lookup (BC.pack k) d
      numK k = fmap (num f) (has k)
  forM_ (numK "LW") $ \v -> modGS (\g -> g { gLW = abs v })
  forM_ (numK "LC") $ \v -> modGS (\g -> g { gCap = case (truncate v :: Int) of 1 -> CapRound; 2 -> CapSquare; _ -> CapButt })
  forM_ (numK "LJ") $ \v -> modGS (\g -> g { gJoin = case (truncate v :: Int) of 1 -> JoinRound; 2 -> JoinBevel; _ -> JoinMiter })
  forM_ (numK "ML") $ \v -> modGS (\g -> g { gMiter = max 1 v })
  forM_ (has "D") $ \o -> case arrOf f o of
    [a, p] -> modGS (\g -> g { gDash = dashOf (map (num f) (arrOf f a)), gPhase = num f p })
    _ -> pure ()
  forM_ (numK "CA") $ \v -> modGS (\g -> g { gStrokeA = max 0 (min 1 v) })
  forM_ (numK "ca") $ \v -> modGS (\g -> g { gFillA = max 0 (min 1 v) })
  forM_ (has "SA") $ \o -> forM_ (boolOf f o) $ \b -> modGS (\g -> g { gSA = b })
  forM_ (has "BM") $ \o -> modGS (\g -> g { gBlend = blendOf f o })
  forM_ (has "Font") $ \o -> case arrOf f o of
    [fo, sz] -> selectFont fo (num f sz)
    _ -> pure ()
  forM_ (has "SMask") $ \o -> case resolve f o of
    ODict sm -> do
      m <- softMask sm
      modGS (\g -> g { gMask = m })
    _ -> modGS (\g -> g { gMask = Nothing })

blendOf :: PdfFile -> Obj -> Blend
blendOf f o = case mapMaybe known (map (nameOf . resolve f) (arrOf f o)) of
  (b : _) -> b
  [] -> BlendNormal
  where
    known n = case BC.unpack n of
      "Normal" -> Just BlendNormal; "Compatible" -> Just BlendNormal
      "Multiply" -> Just BlendMultiply; "Screen" -> Just BlendScreen
      "Overlay" -> Just BlendOverlay; "Darken" -> Just BlendDarken
      "Lighten" -> Just BlendLighten; "ColorDodge" -> Just BlendColorDodge
      "ColorBurn" -> Just BlendColorBurn; "HardLight" -> Just BlendHardLight
      "SoftLight" -> Just BlendSoftLight; "Difference" -> Just BlendDifference
      "Exclusion" -> Just BlendExclusion; "Hue" -> Just BlendHue
      "Saturation" -> Just BlendSaturation; "Color" -> Just BlendColor
      "Luminosity" -> Just BlendLuminosity
      _ -> Nothing

-- A soft mask dictionary, drawn with the CTM in force now.
softMask :: Dict -> R (Maybe SoftMask)
softMask sm = do
  f <- file
  c <- getCtx
  let go = M.findWithDefault ONull (BC.pack "G") sm
      gd = dictOf f go
      kind = if nameOf (lookD f sm "S") == BC.pack "Alpha" then MaskAlpha else MaskLuminosity
      mm = fromMaybe identityA (matrixOf f (lookD f gd "Matrix"))
      bbox = rectOf f (lookD f gd "BBox")
      ctm = gCtm (cxGS c)
      gref = refOf go
  case (resolve f go, bbox) of
    (OStream _ _, Just bb)
      | cxNest c >= maxNest -> pure Nothing
      | Just r <- gref, r `IS.member` cxVisit c -> pure Nothing
      | otherwise -> do
          memo <- gets rsMasks
          let ownRes = case lookD f gd "Resources" of
                ODict r -> Just r
                _ -> Nothing
          let mk r = if isJust ownRes then r else r * 1000003 + cxResKey c
          content <- case gref >>= \r -> M.lookup (mk r) memo of
            Just (w, cmds) -> do
              ok <- chargeReuse w
              pure (if ok then cmds else [])
            Nothing -> do
              bytes <- streamBytes go
              let res = fromMaybe (cxRes c) ownRes
                  visit = maybe (cxVisit c) (`IS.insert` cxVisit c) gref
              rk <- innerResKey ownRes
              let inner = (newCtx (initGS identityA) res bb identityA (fmap (`composeA` composeA ctm mm) (cxToScene c)) (cxNest c + 1) visit) { cxResKey = rk }
              (w, cmds) <- buildWeighted inner (runContent bytes)
              forM_ gref $ \r -> modify' (\s -> s { rsMasks = M.insert (mk r) (w, cmds) (rsMasks s) })
              pure cmds
          -- The backdrop, in the group's colour space.
          let grp = dictOf f (lookD f gd "Group")
              csObj = M.findWithDefault (OName (BC.pack "DeviceRGB")) (BC.pack "CS") grp
          bcCs <- lookupCSNoRes csObj
          let bc = case numsOf f (lookD f sm "BC") of
                [] -> black
                xs -> paintColor (fromMaybe CSRGB bcCs) xs 1
          lut <- case M.lookup (BC.pack "TR") sm of
            Nothing -> pure Nothing
            Just tro -> case resolve f tro of
              OName _ -> pure Nothing
              _ -> do
                r <- withFuel (\fu -> parseFunction f fu tro)
                pure $ case r of
                  Right fn -> Just (listArray (0, 255)
                    [ q (head (evalFunction fn [fromIntegral i / 255] ++ [0])) | i <- [0 .. 255 :: Int] ])
                  Left _ -> Nothing
          -- Everything that changes the mask's coverage is in the key: two
          -- masks sharing a /G with different /S, /BC or /TR are different
          -- masks, and the rasteriser caches by this key.
          let sig = show (ctm, mm, show kind, bc, fmap show (M.lookup (BC.pack "TR") sm)) ++ resTag ownRes c
              mkey = (fromMaybe (-1) gref, sig)
          key <- do
            ks <- gets rsMaskKey
            case M.lookup mkey ks of
              Just k | fst mkey >= 0 -> pure k
              _ -> do
                k <- newKey
                modify' (\s -> s { rsMaskKey = M.insert mkey k (rsMaskKey s) })
                pure k
          let body = [CTransform (composeA ctm mm) [CClip [ClipShape identityA NonZero (rectPathR bb)] content]]
          pure (Just (SoftMask key kind bc body lut))
    _ -> pure Nothing
  where
    q v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int))) :: Word8

lookupCSNoRes :: Obj -> R (Maybe ColorSpace)
lookupCSNoRes o = do
  f <- file
  r <- withFuel (\fu -> parseColorSpaceObj f fu o)
  pure (either (const Nothing) Just r)

rectPathR :: Rect -> Path
rectPathR (Rect x0 y0 x1 y1) = rectPath x0 y0 (x1 - x0) (y1 - y0)

------------------------------------------------------------------------------
-- Shadings (sh)

shadeOp :: BS.ByteString -> R ()
shadeOp nm = do
  f <- file
  c <- getCtx
  let o = M.findWithDefault ONull nm (dictOf f (lookD f (cxRes c) "Shading"))
      gs = cxGS c
      ctm = gCtm gs
  sh <- shadingOf o
  case (sh, invertA ctm) of
    (Right s, Just inv) -> do
      let region = case shBBox s of
            Just bb -> rectPathR bb
            Nothing -> let Rect x0 y0 x1 y1 = cxRegion c
                           ps = map (applyA inv) [Pt x0 y0, Pt x1 y0, Pt x1 y1, Pt x0 y1]
                       in case ps of
                            (p : rest) -> MoveTo p : map LineTo rest ++ [ClosePath]
                            [] -> []
      case shBody s of
        ShPaint p -> emitPainted False p (\q -> CFill ctm NonZero q region)
        ShNothing -> pure ()
        body -> do
          mc <- meshCmd ctm body
          forM_ mc $ \cmd -> case shBBox s of
            Just bb -> emitWrapped (gFillA gs) (CClip [ClipShape ctm NonZero (rectPathR bb)] [cmd])
            Nothing -> emitWrapped (gFillA gs) cmd
    _ -> pure ()

------------------------------------------------------------------------------
-- XObjects

doXObject :: BS.ByteString -> R ()
doXObject nm = do
  f <- file
  c <- getCtx
  let xo = dictOf f (lookD f (cxRes c) "XObject")
      raw = M.findWithDefault ONull nm xo
  case resolve f raw of
    OStream d bytes -> do
      hidden <- ocHides d
      unless hidden $ case BC.unpack (nameOf (lookD f d "Subtype")) of
        "Image" -> drawImage (refOf raw) d bytes False
        "Form" -> drawForm (refOf raw) d bytes
        _ -> pure ()
    _ -> pure ()

inlineImage :: Dict -> BS.ByteString -> R ()
inlineImage d raw = drawImage Nothing d raw True

drawImage :: Maybe Int -> Dict -> BS.ByteString -> Bool -> R ()
drawImage mref d raw inline = do
  f <- file
  c <- getCtx
  s0 <- get
  let gs = cxGS c
      aff = composeA (gCtm gs) (Affine 1 0 0 (-1) 0 1)
      placed = fmap (`composeA` aff) (cxToScene c)
      want = placedPixels placed
  unless (cxHidden c > 0) $ do
    r <- case mref >>= \k -> IM.lookup k (rsImages s0) of
      -- Reused when it was reduced for at least this size (or not reduced).
      Just (v, (rw, rh)) | fits v rw rh want -> pure v
      _ -> do
        v0 <- withFuel (\fu -> (if inline then decodeInlineImage else decodePdfImage) f fu (cxRes c) d raw)
        used <- gets rsImgPixels
        let (v, px) = reduceImage want used v0
        modify' (\s -> s { rsImgPixels = rsImgPixels s + px })
        forM_ mref $ \k -> modify' (\s -> s { rsImages = IM.insert k (v, want) (rsImages s) })
        pure v
    case r of
      Right (PdfColorImage img interp _) -> do
        let ip = interpFor interp (srcW d img) (srcH d img) placed
        case (gMask gs, gBlend gs) of
          (Nothing, BlendNormal) -> when (gFillA gs > 0) $ emit (CImage aff img (ImageOpts ip (gFillA gs)))
          _ -> emitWrapped (gFillA gs) (CImage aff img (ImageOpts ip 1))
      Right (PdfStencil mask interp) -> do
        let ip = interpFor interp (srcWm d mask) (srcHm d mask) placed
        pp <- resolvePaint False (gFill gs) aff
        case pp of
          PaintNone -> pure ()
          PaintIs p _ -> emitPainted False p (\q -> CStencil aff mask q ip)
          PaintMesh _ body _ -> emitPainted False (PSolid (meshMean body)) (\q -> CStencil aff mask q ip)
      Left (ImgUnsupported what) -> do
        note (NUnsupImage what)
        placeholder aff (what ++ " image")
      Left (ImgCorrupt _) -> note NCorruptImage
  where
    -- The interpolation choice is about the image's own pixels, so it is
    -- judged on the declared size, not the reduced one.
    declared k dflt = case M.lookup (BC.pack k) d of
      Just (ONum v) | v >= 1 -> truncate v
      _ -> case M.lookup (BC.pack (take 1 k)) d of
        Just (ONum v) | v >= 1 -> truncate v
        _ -> dflt
    srcW _ img = declared "Width" (imgW img)
    srcH _ img = declared "Height" (imgH img)
    srcWm _ m = declared "Width" (mkW m)
    srcHm _ m = declared "Height" (mkH m)
    fits v rw rh (ww, wh) = case v of
      Right _ -> (rw >= ww && rh >= wh) || not (reduced v)
      Left _ -> True
    reduced v = case v of
      Right (PdfColorImage img _ _) -> imgFmt img == reducedTag
      Right (PdfStencil m _) -> mkW m < declared "Width" (mkW m) || mkH m < declared "Height" (mkH m)
      _ -> False

------------------------------------------------------------------------------
-- Image memory
--
-- A decoded image used to stay at full resolution in the scene, and a page
-- view holding the current page and its neighbours then held every photo of
-- three pages as full RGBA: a scanned 16-page document peaked at 574 MB.
-- Nothing on screen can use more pixels than the image's placed size times
-- the deepest zoom anyone reads at, so images are reduced (area-averaged,
-- the rasteriser's own streaming 'prescaleRGBA'/'prescaleMask') to:
--
--   * 'imageDpi' (600) dots per inch of their placed size, per axis, never
--     enlarged. A page that fills a window is shown at roughly 80–130 dpi,
--     so 600 dpi is a 5–7× zoom with every displayed pixel still backed by
--     its own source pixel; past that a reader is looking at the picture's
--     grain, and scans and photos are rarely finer than 300–600 dpi anyway
--     (a 300 dpi scan is never reduced).
--   * 'maxImagePixels' (8 MP, 32 MB of RGBA) per image, whatever its
--     placement: enough to fill a 4K screen (8.3 MP) with the image alone
--     at one source pixel per device pixel, which is more than a page view
--     ever shows of one picture; the rasteriser caps a whole render at 16 MP.
--   * 'maxPageImagePixels' (32 MP, 128 MB of RGBA) for all of a page's
--     images together: an image that would overflow it gets what is left,
--     down to a floor of 1 MP. Images are decoded in content order, so the
--     budget is first come, first served; four full-cap photos fit before
--     it bites at all. The page view keeps the current page and both
--     neighbours, so this is what bounds its image memory (3 × 128 MB at
--     worst, 3 × 32 MB for a scanned book).
--
-- Inside a tiling cell the placement is the paint's, not known here, so
-- only the two absolute caps apply. An image used twice at different sizes
-- is decoded again when the second use needs more pixels than the first
-- was reduced to. Interpolation is still judged on the declared size.

imageDpi :: Double
imageDpi = 600

maxImagePixels, maxPageImagePixels, minBudgetPixels :: Int
maxImagePixels = 8 * 1000 * 1000
maxPageImagePixels = 32 * 1000 * 1000
minBudgetPixels = 1000 * 1000

-- Marks an RGBA image that was reduced.
reducedTag :: String
reducedTag = "pdf-reduced"

-- The pixels an image placed by this matrix (unit square → scene points)
-- can use per axis; unbounded when the placement is unknown.
placedPixels :: Maybe Affine -> (Int, Int)
placedPixels Nothing = (maxBound `div` 4, maxBound `div` 4)
placedPixels (Just m) =
  let k = imageDpi / 72
      ax v = let x = v * k in if isNaN x || isInfinite x || x > 1e7 then 10000000 else max 1 (ceiling x)
  in (ax (ptLen (applyLinearA m (Pt 1 0))), ax (ptLen (applyLinearA m (Pt 0 1))))

-- Reduce a decoded image to the placement and the caps; the second result
-- is the pixels it now holds (charged to the page).
reduceImage :: (Int, Int) -> Int -> Either ImageFailure PdfImg -> (Either ImageFailure PdfImg, Int)
reduceImage (ww, wh) used v = case v of
  Right (PdfColorImage img interp alpha) ->
    let (w', h') = target (imgW img) (imgH img)
    in if w' < imgW img || h' < imgH img
         then (Right (PdfColorImage (Image w' h' reducedTag (prescaleRGBA (imgW img) (imgH img) (imgPix img) w' h')) interp alpha), w' * h')
         else (v, imgW img * imgH img)
  Right (PdfStencil m interp) ->
    let (w', h') = target (mkW m) (mkH m)
    in if w' < mkW m || h' < mkH m
         -- A quarter of the RGBA cost: charged as such.
         then (Right (PdfStencil (Mask w' h' (prescaleMask (mkW m) (mkH m) (mkPix m) w' h')) interp), (w' * h') `div` 4)
         else (v, (mkW m * mkH m) `div` 4)
  Left _ -> (v, 0)
  where
    target w h =
      let tw = max 1 (min w ww); th = max 1 (min h wh)
          cap = min maxImagePixels (max minBudgetPixels (maxPageImagePixels - used))
          n = tw * th
      in if n <= cap then (tw, th)
         else let s = sqrt (fromIntegral cap / fromIntegral n) :: Double
              in (max 1 (floor (fromIntegral tw * s)), max 1 (floor (fromIntegral th * s)))

-- PDF @\/Interpolate false@ means hard pixels when magnified; poppler
-- nevertheless smooths an image enlarged less than four times. The scene
-- has no device, so judge at about 100 dpi.
interpFor :: Bool -> Int -> Int -> Maybe Affine -> ImgInterp
interpFor True _ _ _ = InterpSmooth
interpFor False _ _ Nothing = InterpSmooth
interpFor False w h (Just m) =
  let sx = ptLen (applyLinearA m (Pt 1 0)) / fromIntegral (max 1 w)
      sy = ptLen (applyLinearA m (Pt 0 1)) / fromIntegral (max 1 h)
  in if min sx sy * (100 / 72) >= 4 then InterpNearest else InterpSmooth

meshMean :: ShadeBody -> Color
meshMean body = case body of
  ShTris (GouraudTri _ c _ _ _ _ : _) -> c
  ShRamp (Stop _ c : _) _ -> c
  _ -> black

-- A labelled light-grey box where an image could not be drawn.
placeholder :: Affine -> String -> R ()
placeholder aff label = do
  env <- gets rsEnv
  let unit = rectPath 0 0 1 1
      w = ptLen (applyLinearA aff (Pt 1 0))
      h = ptLen (applyLinearA aff (Pt 0 1))
      grey = PSolid (Color 0.86 0.86 0.86 1)
      border = CStroke aff defaultStroke { ssWidth = 0 } (PSolid (Color 0.55 0.55 0.55 1)) unit
      textCmds = case reFonts env (fontRequest (T.pack "Helvetica")) of
        Just (_, fnt) | w > 4, h > 4 ->
          let size0 = min 10 (h * 0.4)
              advs = [ (g, F.glyphAdvance fnt g * fa fnt) | ch <- label, Just g <- [F.glyphForChar fnt ch] ]
              total = sum (map snd advs) * size0
              size = if total > w * 0.9 && total > 0 then size0 * w * 0.9 / total else size0
              totalW = sum (map snd advs) * size
              x0 = (w - totalW) / 2
              y0 = (h + size * 0.7) / 2
              xs = scanl (+) x0 (map ((* size) . snd) advs)
              glyphs = [ Glyph (composeA (Affine (size / w) 0 0 (-size / h) (x / w) (y0 / h)) (F.fontMatrix fnt)) (-1) (F.glyphPath fnt g)
                       | ((g, _), x) <- zip advs xs ]
          in [ CGlyphs aff (PSolid (Color 0.35 0.35 0.35 1)) glyphs | not (null glyphs) ]
        _ -> []
  emit (CFill aff NonZero grey unit)
  emit border
  mapM_ emit textCmds
  where fa fnt = let Affine a _ _ _ _ _ = F.fontMatrix fnt in a

drawForm :: Maybe Int -> Dict -> BS.ByteString -> R ()
drawForm mref d raw = do
  f <- file
  c <- getCtx
  let gs = cxGS c
      m = fromMaybe identityA (matrixOf f (lookD f d "Matrix"))
      bbox = fromMaybe (Rect (-1e5) (-1e5) 1e5 1e5) (rectOf f (lookD f d "BBox"))
      ownRes = case lookD f d "Resources" of
        ODict r -> Just r
        _ -> Nothing
      res = fromMaybe (cxRes c) ownRes
      grp = dictOf f (lookD f d "Group")
      isGroup = nameOf (lookD f grp "S") == BC.pack "Transparency"
      isolated = boolOf f (lookD f grp "I") == Just True
      knockout = boolOf f (lookD f grp "K") == Just True
      n = composeA (gCtm gs) m
  when (cxHidden c == 0) $
    if cxNest c >= maxNest then note (NMisc "forms nested too deeply; some not drawn")
    else if maybe False (`IS.member` cxVisit c) mref then pure ()
    else case invertA n of
      Nothing -> pure ()
      Just _ -> do
        gs1 <- mapGsInto n gs
        let gs2 = (if isGroup then gs1 { gFillA = 1, gStrokeA = 1, gBlend = BlendNormal, gMask = Nothing } else gs1)
                    { gCtm = identityA, gClip = [] }
            -- A form with no /Resources of its own draws with whatever it
            -- inherits, so the key names the resources in force.
            sig = gsSig gs2 ++ (if cxLocked c then "L" else "") ++ resTag ownRes c
            memoable = isJust mref && isNothing (gMask gs2)
            memoKey = (fromMaybe (-1) mref, sig)
        memo <- gets rsForms
        kids <- case M.lookup memoKey memo of
          Just (w, k) | memoable -> do
            ok <- chargeReuse w
            pure (if ok then k else [])
          _ -> do
            r <- withFuel (\fu -> decodeStreamF f fu d raw)
            let bytes = either (const BS.empty) id r
                visit = maybe (cxVisit c) (`IS.insert` cxVisit c) mref
            rk <- innerResKey ownRes
            let inner = (newCtx gs2 res bbox identityA (fmap (`composeA` n) (cxToScene c)) (cxNest c + 1) visit)
                          { cxLocked = cxLocked c, cxResKey = rk }
            (w, k) <- buildWeighted inner (runContent bytes)
            when memoable $ modify' (\s -> s { rsForms = M.insert memoKey (w, k) (rsForms s) })
            pure k
        unless (null kids) $ do
          let body = CTransform n [CClip [ClipShape identityA NonZero (rectPathR bbox)] kids]
          if isGroup
            then do
              let opts = defaultGroup { goOpacity = gFillA gs, goBlend = gBlend gs, goMask = gMask gs
                                      , goIsolated = isolated, goKnockout = knockout }
              noteBlend (gBlend gs)
              if goOpacity opts >= 1 && gBlend gs == BlendNormal && isNothing (gMask gs) && not knockout
                then emit body
                else emit (CGroup opts [body])
            else emit body

------------------------------------------------------------------------------
-- Text

textMove :: Double -> Double -> R ()
textMove x y = modCtx (\c -> let m = composeA (cxTlm c) (translateA x y) in c { cxTm = m, cxTlm = m })

nextLine :: R ()
nextLine = do
  gs <- getGS
  textMove 0 (-(gTL gs))

endText :: R ()
endText = do
  c <- getCtx
  case cxTextClip c of
    Just shapes -> pushClip shapes
    Nothing -> pure ()
  modCtx (\c' -> c' { cxTextClip = Nothing })

selectFontByName :: BS.ByteString -> Double -> R ()
selectFontByName nm size = do
  f <- file
  c <- getCtx
  let fo = M.findWithDefault ONull nm (dictOf f (lookD f (cxRes c) "Font"))
  selectFont fo size

selectFont :: Obj -> Double -> R ()
selectFont fo size = do
  f <- file
  s <- get
  let d = dictOf f fo
  if M.null d then modGS (\g -> g { gFont = Nothing, gFs = size }) else do
    (fid, pf) <- case refOf fo >>= \r -> fmap ((,) r) (IM.lookup r (rsFonts s)) of
      Just hit -> pure hit
      Nothing -> do
        -- A font written inline has no object number: it gets a fresh
        -- negative id, so two such fonts never share a memo key.
        fid <- case refOf fo of
          Just r -> pure r
          Nothing -> negate <$> newKey
        p <- withFuel (\fu -> loadPdfFont f fu (reFonts (rsEnv s)) fid d)
        forM_ (refOf fo) $ \r -> modify' (\st -> st { rsFonts = IM.insert r p (rsFonts st) })
        mapM_ (note . NMisc) (pfNotes p)
        pure (fid, p)
    modGS (\g -> g { gFont = Just (fid, pf), gFs = size })

showArray :: [Obj] -> R ()
showArray xs = do
  f <- file
  forM_ (take 100000 xs) $ \x -> case resolve f x of
    OStr s -> showText s
    ONum v | not (isNaN v || isInfinite v) -> do
      gs <- getGS
      let vertical = maybe False (pfVertical . snd) (gFont gs)
          adj = -(v / 1000) * gFs gs
      modCtx (\c -> c { cxTm = composeA (cxTm c) (if vertical then translateA 0 adj else translateA (adj * gTh gs) 0) })
    _ -> pure ()

showText :: BS.ByteString -> R ()
showText s = do
  c <- getCtx
  let gs = cxGS c
  case gFont gs of
    Nothing -> pure ()
    Just (fid, pf) -> do
      let fs = gFs gs; th = gTh gs; tc = gTc gs; tw = gTw gs; rise = gTs gs
          mode = gTr gs
          scaleM = Affine (fs * th) 0 0 fs 0 rise
          codes = pfCodes pf s
          -- Walk the codes: glyph placements and the final text matrix.
          step (tm, acc) (code, nb) =
            let w0 = pfWidth pf code nb
                spaceW = if nb == 1 && code == 32 then tw else 0
                textM = composeA tm scaleM
            in if pfVertical pf
                 then let (w1, vx, vy) = pfVMetric pf code nb
                          gm = composeA textM (translateA (-vx) (-vy))
                          ty = w1 * fs + tc + spaceW
                      in (composeA tm (translateA 0 ty), (code, nb, gm) : acc)
                 else let tx = (w0 * fs + tc + spaceW) * th
                      in (composeA tm (translateA tx 0), (code, nb, textM) : acc)
          (tmEnd, placed0) = foldl' step (cxTm c, []) codes
          placed = reverse placed0
      modCtx (\c' -> c' { cxTm = tmEnd })
      when (cxHidden c == 0 && mode /= 3 || mode >= 4) $
        case pfType3 pf of
          Just t3 -> when (mode /= 3 && mode /= 7) $ mapM_ (\(code, _, gm) -> type3Glyph fid pf t3 code gm) placed
          Nothing -> do
            let glyphs = [ Glyph (composeA gm (grMat g)) (grKey g) (grPath g)
                         | (code, nb, gm) <- placed, Just g <- [pfGlyph pf code nb], not (null (grPath g)) ]
            when (not (pfDrawable pf) && not (null codes)) $ note (NFont (pfName pf))
            st <- get
            if rsGlyphs st + length glyphs > maxPageGlyphs
              then note (NMisc "some text not drawn (too many glyphs on the page)")
              else do
                put st { rsGlyphs = rsGlyphs st + length glyphs }
                unless (null glyphs) $ drawGlyphs gs mode glyphs

drawGlyphs :: GS -> Int -> [Glyph] -> R ()
drawGlyphs gs mode glyphs = do
  let ctm = gCtm gs
      fillIt = mode `elem` [0, 2, 4, 6]
      strokeIt = mode `elem` [1, 2, 5, 6]
      clipIt = mode >= 4
  c <- getCtx
  when (cxHidden c == 0) $ do
    when fillIt $ do
      pp <- resolvePaint False (gFill gs) ctm
      case pp of
        PaintNone -> pure ()
        PaintIs p _ -> emitPainted False p (\q -> CGlyphs ctm q glyphs)
        PaintMesh m body _ -> meshInto False [ ClipShape (composeA ctm ga) NonZero p | Glyph ga _ p <- glyphs ] m body
    when strokeIt $
      forM_ glyphs $ \(Glyph ga _ p) -> strokeShape gs ctm (transformPath ga p)
  when clipIt $
    modCtx (\c' -> c' { cxTextClip = Just ([ ClipShape (composeA ctm ga) NonZero p | Glyph ga _ p <- glyphs ]
                                           ++ fromMaybe [] (cxTextClip c')) })

-- One Type 3 glyph: its procedure, built once per (font, code, inherited
-- state) and placed with a transform.
type3Glyph :: Int -> PFont -> T3Font -> Int -> Affine -> R ()
type3Glyph fid _ t3 code gm = do
  c <- getCtx
  let gs = cxGS c
      n = composeA (gCtm gs) (composeA gm (t3Matrix t3))
  case t3Proc t3 code of
    Nothing -> pure ()
    Just po
      | cxNest c >= maxNest -> note (NMisc "Type 3 glyphs nested too deeply")
      | Just r <- refOf po, r `IS.member` cxVisit c -> pure ()
      | otherwise -> case invertA n of
          Nothing -> pure ()
          Just _ -> do
            gs1 <- mapGsInto n gs
            let gs2 = gs1 { gCtm = identityA, gClip = [] }
                memoable = fid >= 0 && isNothing (gMask gs2)
                key = (fid, code, gsSig gs2 ++ (if cxLocked c then "L" else "") ++ resTag (t3Res t3) c)
            memo <- gets rsT3
            kids <- case M.lookup key memo of
              Just (w, k) | memoable -> do
                ok <- chargeReuse w
                pure (if ok then k else [])
              _ -> do
                bytes <- streamBytes po
                let res = fromMaybe (cxRes c) (t3Res t3)
                    region = fromMaybe (Rect (-1e4) (-1e4) 1e4 1e4) (t3BBox t3)
                    visit = maybe (cxVisit c) (`IS.insert` cxVisit c) (refOf po)
                rk <- innerResKey (t3Res t3)
                let inner = (newCtx gs2 res region identityA (fmap (`composeA` n) (cxToScene c)) (cxNest c + 1) visit)
                              { cxLocked = cxLocked c, cxResKey = rk }
                (w, k) <- buildWeighted inner (runContent bytes)
                when memoable $ modify' (\s -> s { rsT3 = M.insert key (w, k) (rsT3 s) })
                pure k
            st <- get
            unless (null kids) $
              if rsGlyphs st >= maxPageGlyphs then note (NMisc "some text not drawn (too many glyphs on the page)")
              else do
                put st { rsGlyphs = rsGlyphs st + 1 }
                emit (CTransform n kids)

------------------------------------------------------------------------------
-- Annotations

drawAnnotations :: Dict -> Affine -> R ()
drawAnnotations page pageM = do
  f <- file
  printing <- gets rsPrint
  forM_ (take 5000 (arrOf f (lookD f page "Annots"))) $ \ao -> do
    let a = dictOf f ao
        flags = intOf f (M.lookup (BC.pack "F") a) :: Int
        sub = nameOf (lookD f a "Subtype")
        hidden = testBit flags 1 || testBit flags 5 || (printing && not (testBit flags 2))
        ap = dictOf f (lookD f a "AP")
        stream = case lookD f ap "N" of
          s@(OStream _ _) -> Just (M.findWithDefault ONull (BC.pack "N") ap, s)
          ODict states ->
            let st = nameOf (lookD f a "AS")
            in case M.lookup st states of
                 Just o | OStream _ _ <- resolve f o -> Just (o, resolve f o)
                 _ -> Nothing
          _ -> Nothing
    ocHidden <- ocHides a
    stop <- gets (isJust . rsStop)
    unless (hidden || ocHidden || stop || sub == BC.pack "Popup") $ case (stream, rectOf f (lookD f a "Rect")) of
      (Just (ref, OStream sd raw), Just (Rect rx0 ry0 rx1 ry1)) -> do
        let m = fromMaybe identityA (matrixOf f (lookD f sd "Matrix"))
        case rectOf f (lookD f sd "BBox") of
          Nothing -> pure ()
          Just bb -> do
            let Rect bx0 by0 bx1 by1 = transformRect m bb
                bw = bx1 - bx0; bh = by1 - by0
                sx = if bw > 1e-9 then (rx1 - rx0) / bw else 1
                sy = if bh > 1e-9 then (ry1 - ry0) / bh else 1
                aM = Affine sx 0 0 sy (rx0 - sx * bx0) (ry0 - sy * by0)
            modCtx (\c -> c { cxGS = initGS (composeA pageM aM), cxStack = [], cxPath = [], cxMC = [], cxHidden = 0 })
            drawForm (refOf ref) sd raw
      (Nothing, Just r) | not (M.member (BC.pack "AP") a) -> do
        modCtx (\c -> c { cxGS = initGS pageM, cxStack = [], cxPath = [], cxMC = [], cxHidden = 0 })
        generatedAppearance a r
      _ -> pure ()

-- Annotations without an appearance stream that poppler draws anyway (it
-- generates one): link borders, squares and circles, text markup, lines
-- and ink. Their look is the annotation's own data: /C (colour), /IC
-- (interior), /Border or /BS (width, dash, underline style), /CA (opacity).
generatedAppearance :: Dict -> Rect -> R ()
generatedAppearance a (Rect x0 y0 x1 y1) = do
  f <- file
  gs <- getGS
  let sub = BC.unpack (nameOf (lookD f a "Subtype"))
      colourOf k = case numsOf f (lookD f a k) of
        [g] -> Just (Color g g g 1)
        [r, g, b] -> Just (Color r g b 1)
        [c, m, y, k'] -> Just (paintColor CSCMYK [c, m, y, k'] 1)
        _ -> Nothing
      opacity = case lookD f a "CA" of ONum v -> max 0 (min 1 v); _ -> 1
      bs = dictOf f (lookD f a "BS")
      width = case (lookD f bs "W", numsOf f (lookD f a "Border")) of
        (ONum w, _) -> w
        (_, (_ : _ : w : _)) -> w
        _ -> 1
      style = BC.unpack (nameOf (lookD f bs "S"))
      dash = if style == "D" then (case numsOf f (lookD f bs "D") of [] -> [3]; ds -> ds) else []
      ctm = gCtm gs
      ss = defaultStroke { ssWidth = width, ssDash = dashOf dash }
      fade c = let Color r g b al = c in Color r g b (al * opacity)
      hw = width / 2
      quads = chunk (numsOf f (lookD f a "QuadPoints"))
      chunk (ax : ay : bx : by : cx : cy : dx : dy : rest) = (Pt ax ay, Pt bx by, Pt cx cy, Pt dx dy) : chunk rest
      chunk _ = []
      ellipse = let cx = (x0 + x1) / 2; cy = (y0 + y1) / 2; rx = (x1 - x0) / 2 - hw; ry = (y1 - y0) / 2 - hw; k = 0.5523
                in [ MoveTo (Pt (cx + rx) cy), CubicTo (Pt (cx + rx) (cy + k * ry)) (Pt (cx + k * rx) (cy + ry)) (Pt cx (cy + ry))
                   , CubicTo (Pt (cx - k * rx) (cy + ry)) (Pt (cx - rx) (cy + k * ry)) (Pt (cx - rx) cy)
                   , CubicTo (Pt (cx - rx) (cy - k * ry)) (Pt (cx - k * rx) (cy - ry)) (Pt cx (cy - ry))
                   , CubicTo (Pt (cx + k * rx) (cy - ry)) (Pt (cx + rx) (cy - k * ry)) (Pt (cx + rx) cy), ClosePath ]
      inset = rectPath (x0 + hw) (y0 + hw) (x1 - x0 - width) (y1 - y0 - width)
      stroke c path = when (width > 0) $ emit (CStroke ctm ss (PSolid (fade c)) path)
      fill c path = emit (CFill ctm NonZero (PSolid (fade c)) path)
  case (sub, colourOf "C") of
    ("Link", Just c)
      | style == "U" -> stroke c [MoveTo (Pt x0 (y0 + hw)), LineTo (Pt x1 (y0 + hw))]
      | otherwise -> stroke c inset
    ("Square", mc) -> do
      forM_ (colourOf "IC") $ \ic -> fill ic inset
      forM_ mc $ \c -> stroke c inset
    ("Circle", mc) -> do
      forM_ (colourOf "IC") $ \ic -> fill ic ellipse
      forM_ mc $ \c -> stroke c ellipse
    ("Highlight", Just c) ->
      forM_ quads $ \(p1, p2, p3, p4) ->
        emit (CGroup defaultGroup { goBlend = BlendMultiply, goOpacity = opacity }
                [CFill ctm NonZero (PSolid c) [MoveTo p1, LineTo p2, LineTo p4, LineTo p3, ClosePath]])
    (markup, Just c) | markup `elem` ["Underline", "StrikeOut"] ->
      forM_ quads $ \(p1, p2, p3, p4) -> do
        let h = ptLen (ptSub p1 p3)
            t = if markup == "Underline" then 1 / 7 else 0.5
            lw = max 0.5 (h / 14)
            q = ptLerp t p3 p1; q' = ptLerp t p4 p2
        emit (CStroke ctm defaultStroke { ssWidth = lw } (PSolid (fade c)) [MoveTo q, LineTo q'])
    ("Line", Just c) -> case numsOf f (lookD f a "L") of
      [ax, ay, bx, by] -> stroke c [MoveTo (Pt ax ay), LineTo (Pt bx by)]
      _ -> pure ()
    ("Ink", Just c) ->
      forM_ (take 1000 (arrOf f (lookD f a "InkList"))) $ \stroke' ->
        case pairs (numsOf f stroke') of
          (p : rest) -> when (width > 0) $
            emit (CStroke ctm ss { ssCap = CapRound, ssJoin = JoinRound } (PSolid (fade c)) (MoveTo p : map LineTo rest))
          [] -> pure ()
    _ -> pure ()
  where
    pairs (u : v : r) = Pt u v : pairs r
    pairs _ = []
