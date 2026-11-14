-- | The display list: what a vector front-end ("Cmedit.Svg",
-- "Cmedit.PdfRender") produces and what the rasteriser ("Cmedit.Raster")
-- consumes.
--
-- __Why a display list and not a canvas API.__ A front-end parses its file
-- once into a 'Scene' (pure data), and the rasteriser can then draw that
-- scene at /any/ resolution and crop: that is what makes zooming a vector
-- picture re-rasterise sharply instead of upscaling pixels, and it means the
-- expensive half (parsing a PDF content stream, loading its fonts) is paid
-- once per page, not once per zoom step. It also keeps both front-ends free
-- of any pixel or resolution concept: nothing in this module mentions a
-- device, and nothing a front-end builds may depend on one.
--
-- __Coordinates.__ Scene space is y-DOWN with the origin at the top-left of
-- the picture and one unit per CSS px (SVG) or per PDF point (PDF; the PDF
-- front-end flips y and applies @/Rotate@ itself). Every command's 'Affine'
-- maps the command's /user space/ to its /parent space/: scene space at the
-- top level, or the space established by the nearest enclosing
-- 'CTransform'. Groups and clips do not change the coordinate system. That
-- is what lets one sub-scene (a Type 3 glyph, a form XObject, an SVG
-- @\<use\>@ or marker) be built once and shared by every use. Paths stay in
-- user space so strokes are computed where the pen is defined: a stroke under
-- a non-uniform scale must come out elliptical.
--
-- __Colours__ are straight (non-premultiplied) sRGB with alpha, each
-- component in @[0,1]@. Front-ends convert their colour spaces (CMYK, Lab,
-- ICC-by-component-count, Separation via its tint transform) before building
-- the scene, so the rasteriser knows nothing about colour spaces. Shadings
-- whose colour is a function of a parameter are /sampled/ into 'Stop's by the
-- front-end (256 samples is plenty), which is the same mechanism gradients
-- use.
--
-- __Scoping.__ Clips, groups and transforms /contain/ their children rather
-- than being push/pop pairs, so an unbalanced scene is unrepresentable (PDF's
-- @q@/@Q@ is balanced by the front-end while it builds the tree).
--
-- __Opacity on one object.__ PDF's @ca@/@CA@ and SVG's @fill-opacity@ on a
-- gradient or pattern paint, and any single object with a blend mode, are
-- expressed as a 'CGroup' around that one leaf. The rasteriser must give
-- @CGroup g [leaf]@ with a normal blend, no mask, no filter and no knockout a
-- direct path (coverage × opacity, no layer): one leaf is painted as one
-- shape, so this is exact, not an approximation.
--
-- __Cache keys.__ 'Glyph', 'SoftMask' and 'PTiling' carry an @Int@ key that
-- the front-end assigns: equal keys promise equal content (so the rasteriser
-- may cache flattened outlines, glyph coverage, mask layers and tile
-- renderings across uses and across zoom steps); a negative key means "do not
-- cache".
--
-- Every type derives 'Show' so front-ends can be tested (and golden-tested)
-- without the rasteriser.
--
-- Leaf module: imports "Cmedit.Geom" and "Cmedit.Image" (for the RGBA
-- 'Image' carried by image commands) and nothing else from the project.
module Cmedit.Scene
  ( -- * Scenes
    Scene(..)
  , emptyScene
  , Cmd(..)
  , ClipShape(..)
  , Glyph(..)
    -- * Colour and paint
  , Color(..)
  , black
  , white
  , transparent
  , Paint(..)
  , Stop(..)
  , Spread(..)
  , GouraudTri(..)
  , RampTri(..)
    -- * Filling and stroking
  , FillRule(..)
  , StrokeStyle(..)
  , LineCap(..)
  , LineJoin(..)
  , defaultStroke
    -- * Images and masks
  , ImageOpts(..)
  , defaultImageOpts
  , ImgInterp(..)
  , Mask(..)
    -- * Groups
  , GroupOpts(..)
  , defaultGroup
  , Blend(..)
  , SoftMask(..)
  , MaskKind(..)
    -- * Filters
  , Filter(..)
  , FilterPrim(..)
  , FilterIn(..)
  , FilterOp(..)
  , CompOp(..)
  ) where

import Data.Array.Unboxed (UArray)
import Data.Word (Word8)

import Cmedit.Geom (Affine, Path, Pt, Rect)
import Cmedit.Image (Image)

------------------------------------------------------------------------------
-- Scenes

-- | A complete picture. 'scBounds' is the picture's own extent in scene
-- space (the SVG viewport, or the PDF page's crop box after rotation); the
-- viewer fits /that/, not the ink bounds, so a page with a small drawing in
-- one corner still looks like a page. A front-end that wants an opaque
-- background (PDF's paper white) draws it as its first command.
data Scene = Scene
  { scBounds :: !Rect
  , scCmds   :: [Cmd]      -- ^ Painted in order, first at the bottom.
  , scNotes  :: [String]   -- ^ Honest notes for the status line ("2 JPEG 2000 images not shown", ...). Deduplicated by the front-end.
  } deriving (Show)

emptyScene :: Rect -> Scene
emptyScene r = Scene r [] []

-- | One drawing command. Every 'Affine' maps user space to parent space (see
-- the module header).
data Cmd
  = CFill   !Affine !FillRule Paint Path
    -- ^ Fill a path (user space) under a fill rule.
  | CStroke !Affine StrokeStyle Paint Path
    -- ^ Stroke a path. Width, dashes and miter limit are in user units. The
    -- stroke is painted as /one/ shape (its overlaps do not double up).
  | CGlyphs !Affine Paint [Glyph]
    -- ^ Fill a run of glyph outlines with one paint, painted as one object
    -- (so a knockout group treats a text run as a single shape, and a
    -- translucent run's overlapping glyphs do not double up). Each glyph is
    -- filled under 'NonZero' /individually/ and the results are unioned: a
    -- TrueType and a CFF outline wind in opposite directions, so a combined
    -- non-zero fill of overlapping glyphs would punch holes.
  | CImage  !Affine Image ImageOpts
    -- ^ Draw an RGBA image into the unit square @[0,1]×[0,1]@ of user space,
    -- with image row 0 at user @y = 0@ (front-ends fold PDF's flipped image
    -- space into the matrix).
  | CStencil !Affine Mask Paint !ImgInterp
    -- ^ A coverage mask painted with a paint, mapped like 'CImage'. PDF's
    -- @/ImageMask@ (a 1-bit scan is stored one byte per pixel, 0 or 255).
  | CGouraud !Affine [GouraudTri]
    -- ^ Smoothly shaded triangles. Colours interpolate linearly in sRGB.
  | CGouraudRamp !Affine [Stop] [RampTri]
    -- ^ Shaded triangles carrying a parameter @t@ per vertex; @t@ is
    -- interpolated and then looked up in the stop ramp (PDF mesh shadings with
    -- a @/Function@, where interpolating colours would be wrong).
  | CClip   [ClipShape] [Cmd]
    -- ^ Draw the children clipped to the /union/ of the shapes, intersected
    -- with the enclosing clip. An empty shape list clips everything away.
  | CGroup  GroupOpts [Cmd]
    -- ^ Draw the children into an offscreen layer, then composite it with the
    -- group's opacity, blend mode, soft mask and filter.
  | CTransform !Affine [Cmd]
    -- ^ Draw the children in a coordinate system mapped to the parent's by
    -- the matrix. The only way to share a sub-scene.
  deriving (Show)

-- | One part of a clip: a path under a fill rule, in its own user space.
data ClipShape = ClipShape !Affine !FillRule Path
  deriving (Show)

-- | One glyph of a 'CGlyphs' run: glyph space → the run's user space, the
-- front-end's outline key (equal keys promise equal 'Path's; negative = no
-- caching), and the outline itself.
data Glyph = Glyph !Affine !Int Path
  deriving (Show)

-- | A triangle with a colour at each corner.
data GouraudTri = GouraudTri {-# UNPACK #-} !Pt !Color {-# UNPACK #-} !Pt !Color {-# UNPACK #-} !Pt !Color
  deriving (Show)

-- | A triangle with a ramp parameter at each corner.
data RampTri = RampTri {-# UNPACK #-} !Pt {-# UNPACK #-} !Double
                       {-# UNPACK #-} !Pt {-# UNPACK #-} !Double
                       {-# UNPACK #-} !Pt {-# UNPACK #-} !Double
  deriving (Show)

------------------------------------------------------------------------------
-- Colour and paint

-- | Straight-alpha sRGB, components in @[0,1]@.
data Color = Color {-# UNPACK #-} !Double {-# UNPACK #-} !Double
                   {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

black, white, transparent :: Color
black = Color 0 0 0 1
white = Color 1 1 1 1
transparent = Color 0 0 0 0

-- | What fills the inside of a path, a stroke, a glyph run or a stencil.
-- Every paint's own matrix maps its paint space to the /user space of the
-- command it is used by/.
data Paint
  = PSolid !Color
  | PLinear !Affine !Pt !Pt [Stop] !Spread
    -- ^ Gradient space → user space, start and end points (gradient space).
    -- @t@ is the projection onto start→end, 0 at start, 1 at end.
  | PRadial !Affine !Pt !Double !Pt !Double [Stop] !Spread
    -- ^ Two-point conical gradient: start circle (centre, radius) at @t = 0@
    -- and end circle at @t = 1@, in gradient space; the circle at @t@
    -- interpolates centre and radius linearly. A point's colour comes from the
    -- __largest__ @t@ whose circle passes through it with radius @r(t) >= 0@,
    -- among the @t@ the spread allows ('SpreadNone': @[0,1]@,
    -- 'SpreadPadStart': @(-∞,1]@, 'SpreadPadEnd': @[0,∞)@, everything else:
    -- all @t@, mapped back into @[0,1]@ by pad/reflect/repeat). No such @t@ ⇒
    -- transparent. This is PDF shading type 3 and covers SVG's focal radial
    -- gradients (start circle = focal point with radius @fr@).
  | PSampled !Affine Image !Spread
    -- ^ An image as a paint (PDF function-based shading pre-sampled by the
    -- front-end): unit square of paint space → user space, like 'CImage'.
  | PTiling !Int !Affine !Rect !Double !Double [Cmd]
    -- ^ A repeating tile: cache key, pattern space → user space, the tile's
    -- cell (pattern space), x step, y step, and the tile's own commands
    -- (drawn in pattern space). The rasteriser renders the cell at device
    -- resolution once and repeats it. PDF tiling patterns and SVG
    -- @\<pattern\>@.
  deriving (Show)

-- | A gradient stop: offset in @[0,1]@ and colour.
data Stop = Stop {-# UNPACK #-} !Double !Color
  deriving (Eq, Show)

-- | What a gradient does beyond its ends.
data Spread
  = SpreadPad      -- ^ Extend the end colours (PDF @/Extend [true true]@, SVG @pad@).
  | SpreadReflect
  | SpreadRepeat
  | SpreadNone     -- ^ Transparent beyond both ends (PDF @/Extend [false false]@, the default).
  | SpreadPadStart -- ^ Extend only before the start (PDF @[true false]@).
  | SpreadPadEnd   -- ^ Extend only after the end (PDF @[false true]@).
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Filling and stroking

data FillRule = NonZero | EvenOdd
  deriving (Eq, Show)

data LineCap = CapButt | CapRound | CapSquare
  deriving (Eq, Show)

data LineJoin = JoinMiter | JoinRound | JoinBevel
  deriving (Eq, Show)

-- | A stroke, in user units. A width of 0 means PDF's "thinnest line the
-- device can draw": the rasteriser draws it one device pixel wide.
data StrokeStyle = StrokeStyle
  { ssWidth      :: !Double
  , ssCap        :: !LineCap
  , ssJoin       :: !LineJoin
  , ssMiterLimit :: !Double
  , ssDash       :: [Double]  -- ^ On/off lengths; empty = solid. An all-zero or negative array is treated as solid.
  , ssDashPhase  :: !Double
  , ssAdjust     :: !Bool     -- ^ Stroke adjustment (PDF @SA@): never thinner than one device pixel, so a 0.1 pt table rule stays visible at any zoom.
  } deriving (Eq, Show)

-- | PDF's initial graphics state (and SVG's initial stroke properties apart
-- from SVG's miter limit of 4, which its front-end sets).
defaultStroke :: StrokeStyle
defaultStroke = StrokeStyle 1 CapButt JoinMiter 10 [] 0 False

------------------------------------------------------------------------------
-- Images and masks

data ImgInterp
  = InterpSmooth   -- ^ Bilinear when magnifying, area-averaged when minifying.
  | InterpNearest  -- ^ Hard pixels when magnifying (PDF @/Interpolate false@ on a small image, SVG @image-rendering: pixelated@). Minification is still area-averaged.
  deriving (Eq, Show)

data ImageOpts = ImageOpts
  { ioInterp  :: !ImgInterp
  , ioOpacity :: !Double
  } deriving (Eq, Show)

defaultImageOpts :: ImageOpts
defaultImageOpts = ImageOpts InterpSmooth 1

-- | A single-channel coverage image, one byte per pixel (0 = none, 255 =
-- full), row-major, row 0 first. A quarter of an RGBA 'Image', which matters
-- for 600 dpi bilevel scans.
data Mask = Mask
  { mkW   :: !Int
  , mkH   :: !Int
  , mkPix :: !(UArray Int Word8)   -- ^ Length @mkW*mkH@.
  } deriving (Show)

------------------------------------------------------------------------------
-- Groups

-- | PDF and CSS blend modes. Anything a front-end cannot map becomes
-- 'BlendNormal'.
data Blend
  = BlendNormal | BlendMultiply | BlendScreen | BlendOverlay | BlendDarken
  | BlendLighten | BlendColorDodge | BlendColorBurn | BlendHardLight
  | BlendSoftLight | BlendDifference | BlendExclusion
  | BlendHue | BlendSaturation | BlendColor | BlendLuminosity
  deriving (Eq, Show)

data MaskKind
  = MaskLuminosity  -- ^ PDF: coverage = luminosity (0.3, 0.59, 0.11) of the mask's rendering composited over 'smBackdrop'.
  | MaskLuminance   -- ^ SVG/CSS: coverage = luminance (0.2125, 0.7154, 0.0721) of the mask's rendering over 'smBackdrop' (transparent black for SVG).
  | MaskAlpha       -- ^ Coverage = alpha of the mask's rendering.
  deriving (Eq, Show)

-- | A soft mask: render 'smCmds' (in the coordinate system of the group it
-- masks), reduce it to one coverage channel and multiply the group by it.
data SoftMask = SoftMask
  { smKey      :: !Int         -- ^ Cache key (see the module header).
  , smKind     :: !MaskKind
  , smBackdrop :: !Color       -- ^ PDF @/BC@; ignored for 'MaskAlpha'. SVG masks use transparent black.
  , smCmds     :: [Cmd]
  , smTransfer :: Maybe (UArray Int Word8)
    -- ^ PDF @/TR@ as a 256-entry lookup table applied to the coverage.
  } deriving (Show)

data GroupOpts = GroupOpts
  { goOpacity  :: !Double
  , goBlend    :: !Blend
  , goMask     :: Maybe SoftMask
  , goFilter   :: Maybe Filter
  , goIsolated :: !Bool          -- ^ PDF isolated group. SVG groups are isolated.
  , goKnockout :: !Bool          -- ^ PDF knockout group: each child composites against the group's initial backdrop, not against earlier children.
  } deriving (Show)

-- | An opaque, normally blended, /non-isolated/, non-knockout group with no
-- mask or filter. Only such a group may be flattened into its parent (an
-- isolated one may not, if any descendant blends non-normally).
defaultGroup :: GroupOpts
defaultGroup = GroupOpts 1 BlendNormal Nothing Nothing False False

------------------------------------------------------------------------------
-- Filters (SVG)

-- | A filter applied to a group's rendering before it is composited. Data,
-- not a function, so the rasteriser runs it on its own premultiplied float
-- layer at device resolution over just the region it needs (the filter
-- region intersected with the crop, plus whatever margin the primitives read),
-- and so scenes stay 'Show'able.
data Filter = Filter
  { fRegion :: !Rect          -- ^ Filter region, in the group's user space (already resolved from units by the front-end).
  , fAffine :: !Affine        -- ^ That user space → the group's parent space (the space the group is drawn in).
  , fPrims  :: [FilterPrim]   -- ^ In order; the last one's output is the result.
  } deriving (Show)

data FilterPrim = FilterPrim
  { fpOp        :: FilterOp
  , fpIns       :: [FilterIn]   -- ^ Inputs (most ops take one; composite and blend take two; merge any number).
  , fpSubregion :: Maybe Rect   -- ^ Primitive subregion in user space; 'Nothing' = the filter region.
  , fpLinearRGB :: !Bool        -- ^ @color-interpolation-filters: linearRGB@ (SVG's default) vs @sRGB@.
  , fpResult    :: !Int         -- ^ Name of this result, for later 'Result' references (front-end-assigned).
  } deriving (Show)

data FilterIn
  = SourceGraphic
  | SourceAlpha
  | Previous          -- ^ The previous primitive's result (SourceGraphic for the first).
  | Result !Int
  deriving (Eq, Show)

data FilterOp
  = FeBlur !Double !Double                -- ^ stdDeviation x, y in user units.
  | FeOffset !Double !Double              -- ^ dx, dy in user units.
  | FeFlood !Color
  | FeColorMatrix [Double]                -- ^ 20 values, row-major 4×5 (front-end expands saturate/hueRotate/luminanceToAlpha).
  | FeComposite !CompOp
  | FeMerge
  | FeBlend !Blend
  | FeDropShadow !Double !Double !Double !Double !Color  -- ^ stdDeviation x, y, dx, dy, flood colour (with opacity).
  deriving (Show)

-- | @feComposite@ operators; arithmetic carries k1..k4.
data CompOp = CompOver | CompIn | CompOut | CompAtop | CompXor
            | CompArithmetic !Double !Double !Double !Double
  deriving (Eq, Show)
