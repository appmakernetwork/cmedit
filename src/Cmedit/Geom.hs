{-# LANGUAGE BangPatterns #-}
-- | Plane geometry shared by the vector stack: the rasteriser
-- ("Cmedit.Raster"), the font readers ("Cmedit.Font") and the two scene
-- front-ends ("Cmedit.Svg", "Cmedit.PdfRender").
--
-- A leaf module (base only), so every one of those can import it without a
-- cycle. It deliberately holds nothing but points, affine matrices and path
-- segments: anything with an opinion about painting lives in "Cmedit.Scene".
--
-- __Matrix convention.__ 'Affine' is the PDF/SVG/PostScript matrix
-- @[a b c d e f]@, which maps a point by
--
-- > x' = a*x + c*y + e
-- > y' = b*x + d*y + f
--
-- so PDF's @cm@ operands and SVG's @matrix(a,b,c,d,e,f)@ can be passed
-- straight through. 'composeA' reads like function composition:
-- @applyA (composeA outer inner) p == applyA outer (applyA inner p)@.
-- PDF's @cm@ (which /pre/-multiplies the CTM) is therefore
-- @ctm' = composeA ctm m@.
module Cmedit.Geom
  ( -- * Points
    Pt(..)
  , ptAdd
  , ptSub
  , ptScale
  , ptLen
  , ptDot
  , ptLerp
    -- * Affine matrices
  , Affine(..)
  , identityA
  , composeA
  , applyA
  , applyLinearA
  , invertA
  , translateA
  , scaleA
  , rotateA
  , skewXA
  , skewYA
  , determinantA
  , expansionA
    -- * Rectangles
  , Rect(..)
  , rectW
  , rectH
  , rectUnion
  , rectIntersect
  , rectEmpty
  , rectInflate
  , transformRect
    -- * Paths
  , Seg(..)
  , Path
  , transformPath
  , pathBounds
  , pathBoundsExact
  , rectPath
  ) where

import Data.List (foldl')

------------------------------------------------------------------------------
-- Points

-- | A point (or vector) in some coordinate space.
data Pt = Pt {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

ptAdd, ptSub :: Pt -> Pt -> Pt
ptAdd (Pt a b) (Pt c d) = Pt (a + c) (b + d)
ptSub (Pt a b) (Pt c d) = Pt (a - c) (b - d)
{-# INLINE ptAdd #-}
{-# INLINE ptSub #-}

ptScale :: Double -> Pt -> Pt
ptScale k (Pt a b) = Pt (k * a) (k * b)
{-# INLINE ptScale #-}

ptLen :: Pt -> Double
ptLen (Pt a b) = sqrt (a * a + b * b)
{-# INLINE ptLen #-}

ptDot :: Pt -> Pt -> Double
ptDot (Pt a b) (Pt c d) = a * c + b * d
{-# INLINE ptDot #-}

-- | Linear interpolation: @t = 0@ gives the first point.
ptLerp :: Double -> Pt -> Pt -> Pt
ptLerp t (Pt a b) (Pt c d) = Pt (a + t * (c - a)) (b + t * (d - b))
{-# INLINE ptLerp #-}

------------------------------------------------------------------------------
-- Affine matrices

-- | @Affine a b c d e f@: see the module header for the convention.
data Affine = Affine {-# UNPACK #-} !Double {-# UNPACK #-} !Double
                     {-# UNPACK #-} !Double {-# UNPACK #-} !Double
                     {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

identityA :: Affine
identityA = Affine 1 0 0 1 0 0

-- | @composeA outer inner@ applies @inner@ first.
composeA :: Affine -> Affine -> Affine
composeA (Affine a b c d e f) (Affine a' b' c' d' e' f') =
  Affine (a * a' + c * b')     (b * a' + d * b')
         (a * c' + c * d')     (b * c' + d * d')
         (a * e' + c * f' + e) (b * e' + d * f' + f)
{-# INLINE composeA #-}

applyA :: Affine -> Pt -> Pt
applyA (Affine a b c d e f) (Pt x y) = Pt (a * x + c * y + e) (b * x + d * y + f)
{-# INLINE applyA #-}

-- | Apply only the linear part (for direction vectors, not positions).
applyLinearA :: Affine -> Pt -> Pt
applyLinearA (Affine a b c d _ _) (Pt x y) = Pt (a * x + c * y) (b * x + d * y)
{-# INLINE applyLinearA #-}

determinantA :: Affine -> Double
determinantA (Affine a b c d _ _) = a * d - b * c

-- | The inverse, or 'Nothing' for a (numerically) singular matrix. A singular
-- CTM is legal in PDF and simply means "draws nothing", so callers treat
-- 'Nothing' as an empty result rather than an error.
invertA :: Affine -> Maybe Affine
invertA m@(Affine a b c d e f)
  | abs det < 1e-300 || isNaN det || isInfinite det = Nothing
  | otherwise =
      let ia = d / det; ib = -b / det; ic = -c / det; id' = a / det
      in Just (Affine ia ib ic id' (-(ia * e + ic * f)) (-(ib * e + id' * f)))
  where det = determinantA m

translateA :: Double -> Double -> Affine
translateA x y = Affine 1 0 0 1 x y

scaleA :: Double -> Double -> Affine
scaleA sx sy = Affine sx 0 0 sy 0 0

-- | Rotation by an angle in /radians/, counter-clockwise in a y-up space
-- (clockwise on a y-down screen, which is SVG's reading of @rotate()@).
rotateA :: Double -> Affine
rotateA t = let c = cos t; s = sin t in Affine c s (-s) c 0 0

skewXA, skewYA :: Double -> Affine
skewXA t = Affine 1 0 (tan t) 1 0 0
skewYA t = Affine 1 (tan t) 0 1 0 0

-- | The largest factor by which the matrix stretches any unit vector (the
-- larger singular value). This is what turns a device-space flattening
-- tolerance into a user-space one, and a user-space line width into an
-- approximate device width.
expansionA :: Affine -> Double
expansionA (Affine a b c d _ _) =
  let s1 = a * a + b * b + c * c + d * d
      s2 = (a * d - b * c)
      disc = sqrt (max 0 (s1 * s1 - 4 * s2 * s2))
  in sqrt (max 0 ((s1 + disc) / 2))

------------------------------------------------------------------------------
-- Rectangles

-- | An axis-aligned rectangle, @Rect x0 y0 x1 y1@ with @x0 <= x1@ and
-- @y0 <= y1@ when non-empty.
data Rect = Rect {-# UNPACK #-} !Double {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

rectW, rectH :: Rect -> Double
rectW (Rect x0 _ x1 _) = x1 - x0
rectH (Rect _ y0 _ y1) = y1 - y0

-- | No interior. Note this is 'True' for the bounds of a horizontal or
-- vertical line, so never cull a /stroke/ by its path's bounds without
-- 'rectInflate'-ing them by the stroke's extent first.
rectEmpty :: Rect -> Bool
rectEmpty (Rect x0 y0 x1 y1) = not (x1 > x0 && y1 > y0)

-- | Grow by a margin on every side.
rectInflate :: Double -> Rect -> Rect
rectInflate m (Rect x0 y0 x1 y1) = Rect (x0 - m) (y0 - m) (x1 + m) (y1 + m)

-- | The smallest rectangle containing both (there is no empty identity: fold
-- over a 'Maybe' or a non-empty list).
rectUnion :: Rect -> Rect -> Rect
rectUnion (Rect a b c d) (Rect a' b' c' d') =
  Rect (min a a') (min b b') (max c c') (max d d')

rectIntersect :: Rect -> Rect -> Rect
rectIntersect (Rect a b c d) (Rect a' b' c' d') =
  Rect (max a a') (max b b') (min c c') (min d d')

-- | The bounding box of a transformed rectangle.
transformRect :: Affine -> Rect -> Rect
transformRect m (Rect x0 y0 x1 y1) =
  let ps = map (applyA m) [Pt x0 y0, Pt x1 y0, Pt x0 y1, Pt x1 y1]
      xs = [ x | Pt x _ <- ps ]; ys = [ y | Pt _ y <- ps ]
  in Rect (minimum xs) (minimum ys) (maximum xs) (maximum ys)

------------------------------------------------------------------------------
-- Paths

-- | One path segment. Subpaths begin with 'MoveTo'; a segment that arrives
-- with no current point is treated as starting at its own first point (the
-- lenient reading every real renderer takes). 'ClosePath' returns to the
-- subpath's start and leaves the current point there.
data Seg
  = MoveTo  {-# UNPACK #-} !Pt
  | LineTo  {-# UNPACK #-} !Pt
  | QuadTo  {-# UNPACK #-} !Pt {-# UNPACK #-} !Pt                     -- ^ Control point, end point.
  | CubicTo {-# UNPACK #-} !Pt {-# UNPACK #-} !Pt {-# UNPACK #-} !Pt  -- ^ Two control points, end point.
  | ClosePath
  deriving (Eq, Show)

-- | A path is a plain list: front-ends build them incrementally and the
-- rasteriser consumes them once, front to back.
type Path = [Seg]

-- | Transform every point of a path. Béziers are affine-invariant, so this
-- is exact.
transformPath :: Affine -> Path -> Path
transformPath m = map go
  where
    go (MoveTo p)        = MoveTo (applyA m p)
    go (LineTo p)        = LineTo (applyA m p)
    go (QuadTo p q)      = QuadTo (applyA m p) (applyA m q)
    go (CubicTo p q r)   = CubicTo (applyA m p) (applyA m q) (applyA m r)
    go ClosePath         = ClosePath

-- | The bounding box of a path's control polygon (a conservative bound of
-- the curve itself), or 'Nothing' for a path with no points.
pathBounds :: Path -> Maybe Rect
pathBounds = foldl' step Nothing . concatMap pts
  where
    pts (MoveTo p)      = [p]
    pts (LineTo p)      = [p]
    pts (QuadTo p q)    = [p, q]
    pts (CubicTo p q r) = [p, q, r]
    pts ClosePath       = []
    step Nothing (Pt x y)                  = Just (Rect x y x y)
    step (Just (Rect a b c d)) (Pt x y)    = Just (Rect (min a x) (min b y) (max c x) (max d y))

-- | The exact bounding box of the curve (Bézier extrema, not the control
-- polygon), or 'Nothing' for a path with no points. This is the box SVG's
-- @objectBoundingBox@ units are defined against.
pathBoundsExact :: Path -> Maybe Rect
pathBoundsExact segs = foldl' step Nothing (go Nothing Nothing segs)
  where
    -- go: current point, subpath start
    go _ _ [] = []
    go _ _ (MoveTo p : r) = p : go (Just p) (Just p) r
    go cur st (LineTo p : r) = p : go (Just p) (st `orJ` cur `orJ` Just p) r
    go cur st (QuadTo c p : r) =
      let p0 = maybe c id cur
      in quadExt p0 c p ++ [p] ++ go (Just p) (st `orJ` Just p0) r
    go cur st (CubicTo c1 c2 p : r) =
      let p0 = maybe c1 id cur
      in cubicExt p0 c1 c2 p ++ [p] ++ go (Just p) (st `orJ` Just p0) r
    go _ st (ClosePath : r) = go st st r
    orJ (Just a) _ = Just a
    orJ Nothing b  = b
    step Nothing (Pt x y)               = Just (Rect x y x y)
    step (Just (Rect a b c d)) (Pt x y) = Just (Rect (min a x) (min b y) (max c x) (max d y))
    quadExt p0@(Pt x0 y0) (Pt x1 y1) p2@(Pt x2 y2) =
      [ p0 ] ++
      [ evalQ t | t <- [rootQ x0 x1 x2, rootQ y0 y1 y2], t > 0, t < 1 ]
      where evalQ t = let u = 1 - t
                          Pt ax ay = p0; Pt bx by = p2
                      in Pt (u*u*ax + 2*u*t*x1 + t*t*bx) (u*u*ay + 2*u*t*y1 + t*t*by)
            rootQ a b c = let den = a - 2 * b + c in if abs den < 1e-12 then -1 else (a - b) / den
    cubicExt p0@(Pt x0 y0) (Pt x1 y1) (Pt x2 y2) (Pt x3 y3) =
      p0 : [ evalC t | t <- rootsC x0 x1 x2 x3 ++ rootsC y0 y1 y2 y3, t > 0, t < 1 ]
      where evalC t = let u = 1 - t
                          b0 = u*u*u; b1 = 3*u*u*t; b2 = 3*u*t*t; b3 = t*t*t
                      in Pt (b0*x0 + b1*x1 + b2*x2 + b3*x3) (b0*y0 + b1*y1 + b2*y2 + b3*y3)
            -- derivative / 3: a t^2 + b t + c
            rootsC a0 a1 a2 a3 =
              let a = -a0 + 3*a1 - 3*a2 + a3; b = 2 * (a0 - 2*a1 + a2); c = a1 - a0
              in if abs a < 1e-12
                   then (if abs b < 1e-12 then [] else [-c / b])
                   else let disc = b*b - 4*a*c
                        in if disc < 0 then []
                           else let sq = sqrt disc in [(-b + sq) / (2*a), (-b - sq) / (2*a)]

-- | A closed rectangle path (PDF's @re@ operator: @x y w h@, drawn
-- counter-clockwise in a y-up space).
rectPath :: Double -> Double -> Double -> Double -> Path
rectPath x y w h =
  [ MoveTo (Pt x y), LineTo (Pt (x + w) y), LineTo (Pt (x + w) (y + h))
  , LineTo (Pt x (y + h)), ClosePath ]
