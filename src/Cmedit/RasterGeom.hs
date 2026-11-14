{-# LANGUAGE BangPatterns #-}
-- | Pure path geometry for the rasteriser ("Cmedit.Raster"): curve
-- flattening, dashing and the stroker. Nothing here knows about pixels; the
-- only device concept is the flattening /tolerance/, which the caller
-- converts into whatever space it is working in.
--
-- __The stroker produces outlines whose non-zero fill is the stroke, and
-- that is the whole correctness argument.__ Each subpath becomes one closed
-- polygon (two for a closed subpath): the left offset forward, the end cap,
-- the right offset backward, the start cap. At a vertex the /outer/ side
-- gets the join (miter, round or bevel) and the /inner/ side is routed
-- through the vertex itself (Skia's "inner join through the pivot"). Seen
-- as a chain, that outline is exactly the sum of the boundaries of every
-- segment's rectangle, every outer join wedge and every cap, all with the
-- same orientation, because the edges they share cancel pairwise. The
-- winding number at a point is therefore the number of those pieces that
-- cover it: never negative, and non-zero exactly inside their union. So a
-- self-overlapping stroke, a hairpin turn, a segment shorter than the pen,
-- a flattened curve tighter than its own half-width all come out right
-- without a single intersection test, and the rasteriser's coverage clamp
-- makes a stroke "one shape" (its overlaps do not double up) for free.
-- Every generated piece, dots included, is built with the same (negative,
-- in y-up shoelace terms) orientation; a dot built the other way round
-- would cancel the stroke it overlaps.
--
-- __Curves are flattened before stroking__, in the space the pen lives in,
-- and the vertices a curve contributes are marked as smooth: they always get
-- a round join (adaptively subdivided, so a gentle bend adds no points at
-- all) whatever the stroke's join style, which makes the outline the exact
-- Minkowski sum of the flattened centre line and the pen. The offset error
-- is then the centre line's flattening error and nothing more, and a miter
-- spike can never appear in the middle of a curve.
--
-- Flattening uses Wang's formula: a degree-@d@ Bézier split into @n@ equal
-- parameter steps deviates from its chords by at most
-- @d(d-1)/8 · max|Δ²P| / n²@, so @n@ is chosen to meet the tolerance with
-- no recursion and no per-curve estimate loop. The step count is capped
-- ('maxCurveSteps'): a curve with absurd control points costs a bounded
-- number of segments rather than an unbounded one.
--
-- Dashing works on the flattened polylines (dash lengths are arc lengths in
-- the pen's space), restarts the pattern at every subpath as PDF and SVG
-- both require, joins the last dash of a closed subpath to its first when
-- the pattern is "on" across the closing point, and turns a zero-length
-- "on" interval into a zero-length polyline that the stroker draws as a dot
-- (round or square caps). A pattern that would produce more than
-- 'maxDashes' dashes strokes solid instead: the alternative is unbounded
-- work for a picture that would look like a solid line anyway.
module Cmedit.RasterGeom
  ( -- * Polylines
    Vtx(..)
  , PolyLine(..)
  , flattenPolylines
  , quadSteps
  , cubicSteps
  , maxCurveSteps
    -- * Dashing
  , dashPolylines
  , validDash
  , maxDashes
    -- * Stroking
  , strokePolyline
  , strokePolylines
  , strokeOutlines
  , strokeToPath
  , dashPath
  , flattenPath
  , polygonsToPath
  , strokeExtent
  , arcPoints
  ) where


import Cmedit.Geom
import Cmedit.Scene (StrokeStyle(..), LineCap(..), LineJoin(..))

------------------------------------------------------------------------------
-- Flattening

-- | A polyline vertex: position, and whether it is a real corner of the
-- source path ('True') or an interior point of a flattened curve ('False',
-- which always gets a round join).
data Vtx = Vtx {-# UNPACK #-} !Double {-# UNPACK #-} !Double !Bool
  deriving (Eq, Show)

-- | One flattened subpath. A closed polyline does not repeat its first
-- point; the closing segment is implied. 'plDir' is the direction caps use
-- when the polyline has no extent at all (a dot).
data PolyLine = PolyLine
  { plPts    :: [Vtx]
  , plClosed :: !Bool
  , plDir    :: !Pt
  } deriving (Show)

-- | Upper bound on the chords one curve is split into.
maxCurveSteps :: Int
maxCurveSteps = 4096

steps :: Double -> Double -> Int
steps k tol
  | not (k > 0) = 1
  | otherwise =
      let t = if tol > 0 then tol else 0.1
          n = sqrt (k / t)
      in if n < fromIntegral maxCurveSteps then max 1 (ceiling n) else maxCurveSteps
{-# INLINE steps #-}

-- | Chords needed for a quadratic within the tolerance (Wang's formula).
quadSteps :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Int
quadSteps tol x0 y0 x1 y1 x2 y2 =
  let dx = x0 - 2 * x1 + x2; dy = y0 - 2 * y1 + y2
  in steps (0.25 * sqrt (dx * dx + dy * dy)) tol
{-# INLINE quadSteps #-}

-- | Chords needed for a cubic within the tolerance (Wang's formula).
cubicSteps :: Double -> Double -> Double -> Double -> Double -> Double -> Double
           -> Double -> Double -> Int
cubicSteps tol x0 y0 x1 y1 x2 y2 x3 y3 =
  let ax = x0 - 2 * x1 + x2; ay = y0 - 2 * y1 + y2
      bx = x1 - 2 * x2 + x3; by = y1 - 2 * y2 + y3
      m = max (ax * ax + ay * ay) (bx * bx + by * by)
  in steps (0.75 * sqrt m) tol
{-# INLINE cubicSteps #-}

-- | Flatten a path into polylines within a tolerance (same units as the
-- path). A subpath with no segments (a bare moveto) is dropped; one whose
-- segments all have zero length survives as a single-point polyline, which
-- strokes as a dot.
flattenPolylines :: Double -> Path -> [PolyLine]
flattenPolylines tol = go Nothing [] False (Pt 0 0)
  where
    -- go: current point, reversed vertices of the open subpath, whether it
    -- has any segment, subpath start
    go :: Maybe Pt -> [Vtx] -> Bool -> Pt -> Path -> [PolyLine]
    go _ acc hasSeg _ [] = flush acc hasSeg False []
    go _ acc hasSeg _ (MoveTo p : r) =
      flush acc hasSeg False (go (Just p) [vt p] False p r)
    go Nothing _ _ _ (LineTo p : r) = go (Just p) [vt p] True p r
    go (Just _) acc _ st (LineTo p : r) = go (Just p) (vt p : acc) True st r
    go cur acc hasSeg st (QuadTo c p : r) =
      let (p0, acc0, st0) = start cur acc st c
          Pt x0 y0 = p0; Pt x1 y1 = c; Pt x2 y2 = p
          n = quadSteps tol x0 y0 x1 y1 x2 y2
          inner = [ let t = fromIntegral k / fromIntegral n; u = 1 - t
                    in Vtx (u * u * x0 + 2 * u * t * x1 + t * t * x2)
                           (u * u * y0 + 2 * u * t * y1 + t * t * y2) False
                  | k <- [1 .. n - 1] ]
      in hasSeg `seq` go (Just p) (vt p : reverse inner ++ acc0) True st0 r
    go cur acc hasSeg st (CubicTo c1 c2 p : r) =
      let (p0, acc0, st0) = start cur acc st c1
          Pt x0 y0 = p0; Pt x1 y1 = c1; Pt x2 y2 = c2; Pt x3 y3 = p
          n = cubicSteps tol x0 y0 x1 y1 x2 y2 x3 y3
          inner = [ let t = fromIntegral k / fromIntegral n; u = 1 - t
                        b0 = u * u * u; b1 = 3 * u * u * t; b2 = 3 * u * t * t; b3 = t * t * t
                    in Vtx (b0 * x0 + b1 * x1 + b2 * x2 + b3 * x3)
                           (b0 * y0 + b1 * y1 + b2 * y2 + b3 * y3) False
                  | k <- [1 .. n - 1] ]
      in hasSeg `seq` go (Just p) (vt p : reverse inner ++ acc0) True st0 r
    go Nothing acc hasSeg st (ClosePath : r) = go Nothing acc hasSeg st r
    go (Just _) acc _ st (ClosePath : r) =
      flush acc True True (go (Just st) [vt st] False st r)

    start Nothing _ _ c = (c, [vt c], c)
    start (Just p) acc st _ = (p, acc, st)

    vt (Pt x y) = Vtx x y True

    flush acc hasSeg closed rest
      | not hasSeg = rest
      | otherwise =
          let pts = reverse acc
              pts' = if closed then dropClosingDup pts else pts
          in PolyLine pts' closed (firstDir pts') : rest

    dropClosingDup ps@(Vtx x0 y0 _ : _ : _) =
      case last ps of
        Vtx x y _ | x == x0 && y == y0 -> init ps
        _ -> ps
    dropClosingDup ps = ps

firstDir :: [Vtx] -> Pt
firstDir (Vtx x0 y0 _ : rest) =
  case [ Pt (x - x0) (y - y0) | Vtx x y _ <- rest, x /= x0 || y /= y0 ] of
    (d : _) -> d
    [] -> Pt 1 0
firstDir [] = Pt 1 0

------------------------------------------------------------------------------
-- Dashing

-- | Upper bound on the dashes one dashing operation may produce.
maxDashes :: Int
maxDashes = 100000

-- | The pattern a dash array really means, or 'Nothing' for "solid": PDF and
-- SVG both treat an empty, all-zero or negative array as no dashing, and an
-- odd-length array as itself repeated twice.
validDash :: [Double] -> Maybe [Double]
validDash ds
  | null ds = Nothing
  | any (\d -> not (d >= 0) || isInfinite d) ds = Nothing
  | sum ds <= 0 = Nothing
  | odd (length ds) = Just (ds ++ ds)
  | otherwise = Just ds

-- | Apply a dash pattern (already validated, see 'validDash') with a phase.
dashPolylines :: [Double] -> Double -> [PolyLine] -> [PolyLine]
dashPolylines pat phase pls
  | null pat = pls
  | estimate > fromIntegral maxDashes = pls
  | otherwise = concatMap dashOne pls
  where
    period = sum pat
    n = length pat
    totalLen = sum (map polyLen pls)
    estimate = totalLen / period * fromIntegral n + fromIntegral (length pls * n)
    patAt i = pat !! i
    -- Where the phase lands: index into the pattern and length left in it.
    (i0, rem0) =
      let ph0 = phase - period * fromIntegral (floor (phase / period) :: Integer)
          ph = if isNaN ph0 || ph0 < 0 || ph0 >= period then 0 else ph0
          walk i p | i >= n = (0, patAt 0)
                   | p >= patAt i && not (p == 0 && patAt i == 0) = walk (i + 1) (p - patAt i)
                   | otherwise = (i, patAt i - p)
      in walk 0 ph

    dashOne (PolyLine vs closed dir0) =
      let startOn = even i0
          initCur = if startOn then take 1 vs else []
          (dashes, onEnd, toggled) =
            walk (polySegs vs closed) i0 rem0 startOn initCur [] False dir0
      in if not closed then dashes
         else if startOn && onEnd && not toggled then [PolyLine vs True dir0]
         else if startOn && onEnd then
           case dashes of
             (firstD : rest@(_ : _)) ->
               let lastD = last rest
                   merged = PolyLine (plPts lastD ++ drop 1 (plPts firstD)) False (plDir lastD)
               in merged : init rest
             _ -> dashes
         else dashes

    -- walk: segments, pattern index, length left in this interval, whether
    -- it is "on", the current dash's vertices (reversed), the dashes so far
    -- (reversed), whether the state ever changed, the last direction.
    walk :: [(Vtx, Vtx)] -> Int -> Double -> Bool -> [Vtx] -> [PolyLine] -> Bool -> Pt
         -> ([PolyLine], Bool, Bool)
    walk [] _ _ on cur out tog d =
      (reverse (if on && not (null cur) then PolyLine (reverse cur) False d : out else out), on, tog)
    walk ((a@(Vtx ax ay _), b@(Vtx bx by _)) : rest) idx0 rm0 on0 cur0 out0 tog0 d0 =
      let dx = bx - ax; dy = by - ay
          len = sqrt (dx * dx + dy * dy)
          d = Pt dx dy
          seg !pos !idx !rm on cur out tog
            | len - pos > rm =
                let pos' = pos + rm
                    t = pos' / len
                    q = Vtx (ax + dx * t) (ay + dy * t) True
                    idx' = if idx + 1 >= n then 0 else idx + 1
                in if on
                     then seg pos' idx' (patAt idx') False [] (PolyLine (reverse (q : cur)) False d : out) True
                     else seg pos' idx' (patAt idx') True [q] out True
            | otherwise =
                walk rest idx (rm - (len - pos)) on (if on then b : cur else cur) out tog d
      in if not (len > 0) || isInfinite len
           then walk rest idx0 rm0 on0 (if on0 then b : cur0 else cur0) out0 tog0 d0
           else a `seq` seg 0 idx0 rm0 on0 cur0 out0 tog0

    polySegs vs closed = zip vs (drop 1 vs) ++
      (if closed then case vs of
                        (v : _ : _) -> [(last vs, v)]
                        _ -> []
       else [])

polyLen :: PolyLine -> Double
polyLen (PolyLine vs closed _) =
  sum [ let dx = bx - ax; dy = by - ay in sqrt (dx * dx + dy * dy)
      | (Vtx ax ay _, Vtx bx by _) <- zip vs (drop 1 vs) ++ closing ]
  where closing = if closed then case vs of
                                   (v : _ : _) -> [(last vs, v)]
                                   _ -> []
                  else []

------------------------------------------------------------------------------
-- Stroking

-- | How far a stroke can reach beyond its path, in the pen's units: half
-- the width, scaled by the miter limit for miter joins and by √2 for square
-- caps.
strokeExtent :: StrokeStyle -> Double
strokeExtent ss =
  let h = abs (ssWidth ss) / 2
      j = case ssJoin ss of
            JoinMiter -> max 1 (ssMiterLimit ss)
            _ -> 1
      c = case ssCap ss of
            CapSquare -> sqrt 2
            _ -> 1
  in h * max j c

-- | Points on an arc of the given radius about a centre, starting at angle
-- @a0@ and sweeping @sweep@ radians, excluding the start point and
-- including the end; subdivided so the chords stay within the tolerance.
arcPoints :: Pt -> Double -> Double -> Double -> Double -> [Pt]
arcPoints (Pt cx cy) r a0 sweep tol =
  -- A quarter of the tolerance: an inscribed polygon always loses area, and
  -- a round cap or dot is nothing but arc, so the full tolerance would show.
  let tolA = tol * 0.25
      dth | tolA < r = 2 * acos (1 - tolA / r)
          | otherwise = pi / 2
      nRaw = abs sweep / max 1e-3 dth
      n | not (nRaw < 512) = 512
        | otherwise = max 1 (ceiling nRaw) :: Int
  in [ let a = a0 + sweep * fromIntegral k / fromIntegral n
       in Pt (cx + r * cos a) (cy + r * sin a)
     | k <- [1 .. n] ]

-- | Stroke one polyline: half-width, cap, join, miter limit, tolerance.
-- Returns closed polygons (implicitly closed point lists) whose non-zero
-- union is the stroke.
strokePolyline :: Double -> LineCap -> LineJoin -> Double -> Double -> PolyLine -> [[Pt]]
strokePolyline h cap join ml tol (PolyLine vs0 closed dir0)
  | not (h > 0) || isInfinite h = []
  | otherwise =
      case dedupe vs0 of
        [] -> []
        [Vtx x y _] -> dot (Pt x y)
        vs | closed -> closedOutline vs
           | otherwise -> [openOutline vs]
  where
    -- Clamped: a limit near 1e155 squares to infinity, limit2 becomes 0, and
    -- an exact reversal (1 + dt = 0) would then take the miter branch and
    -- divide by zero. A ratio of a million is already "never bevel".
    limit2 = let m = min 1e6 (max 1 ml) in 2 / (m * m)

    dedupe = closeDup . go
      where
        go (a@(Vtx ax ay ac) : b@(Vtx bx by bc) : r)
          | near ax ay bx by = go (Vtx ax ay (ac || bc) : r)
          | otherwise = a : go (b : r)
        go r = r
        closeDup ps@(Vtx ax ay _ : _ : _)
          | closed, Vtx lx ly _ <- last ps, near ax ay lx ly = init ps
        closeDup ps = ps
    near ax ay bx by =
      let dx = bx - ax; dy = by - ay
          s = 1e-9 * (1 + abs ax + abs ay)
      in dx * dx + dy * dy <= s * s

    unitDir (Vtx ax ay _) (Vtx bx by _) =
      let dx = bx - ax; dy = by - ay; l = sqrt (dx * dx + dy * dy)
      in (Pt (dx / l) (dy / l), l)
    normalOf (Pt dx dy) = Pt (-dy * h) (dx * h)

    dot p@(Pt px py) = case cap of
      CapButt -> []
      CapRound -> [arcPoints p h 0 (-2 * pi) tol]
      CapSquare ->
        let Pt ddx ddy = dir0
            l = sqrt (ddx * ddx + ddy * ddy)
            (ux, uy) = if l > 0 && not (isInfinite l) then (ddx / l, ddy / l) else (1, 0)
            dx = ux * h; dy = uy * h
            nx = -uy * h; ny = ux * h
        in [[ Pt (px - dx + nx) (py - dy + ny), Pt (px + dx + nx) (py + dy + ny)
            , Pt (px + dx - nx) (py + dy - ny), Pt (px - dx - nx) (py - dy - ny) ]]

    pt (Vtx x y _) = Pt x y
    offs s (Vtx x y _) (Pt nx ny) = Pt (x + s * nx) (y + s * ny)

    -- Points on side s at a vertex between a segment with direction dA and
    -- one with dB (and their lengths): from the end of the first offset to
    -- the start of the second, inclusive.
    --
    -- The inner side meets at the offset lines' intersection when both
    -- segments are long enough to be cut back that far (by at most half
    -- their length, so the cuts from a segment's two ends never cross), and
    -- is routed through the vertex otherwise. Both outlines have the same
    -- non-zero union; the pivot route covers a small kite twice, which
    -- exact-area coverage would count twice in the boundary pixel beside it,
    -- and on a finely flattened curve that is every vertex of the inside.
    joinSide :: Double -> Vtx -> (Pt, Double) -> (Pt, Double) -> [Pt]
    joinSide s v@(Vtx vx vy corner) (dA@(Pt ax ay), lenA) (dB@(Pt bx by), lenB) =
      let nA = normalOf dA; nB = normalOf dB
          cr = ax * by - ay * bx
          dt = ax * bx + ay * by
          pA = offs s v nA; pB = offs s v nB
          reversal = cr == 0 && dt < 0
          outer | reversal = s > 0
                | otherwise = s * cr < 0
          theta | reversal = -pi
                | otherwise = atan2 cr dt
          style = if corner then join else JoinRound
          meet = let Pt nax nay = nA; Pt nbx nby = nB; k = s / (1 + dt)
                 in Pt (vx + k * (nax + nbx)) (vy + k * (nay + nby))
          cutBack = h * abs cr / (1 + dt)
      in if cr == 0 && dt > 0 then [pA]
         else if not outer then
           (if not reversal && 1 + dt > 1e-9 && cutBack <= 0.5 * lenA && cutBack <= 0.5 * lenB
              then [meet] else [pA, Pt vx vy, pB])
         else case style of
           JoinBevel -> [pA, pB]
           JoinMiter
             | 1 + dt >= limit2 -> [pA, meet, pB]
             | otherwise -> [pA, pB]
           JoinRound ->
             let Pt sx sy = Pt (s * fst' nA) (s * snd' nA)
                 a0 = atan2 sy sx
             in pA : init' (arcPoints (Pt vx vy) h a0 theta tol) ++ [pB]
    fst' (Pt x _) = x
    snd' (Pt _ y) = y
    init' [] = []
    init' xs = init xs

    -- Cap points strictly between p+n and p-n, for a cap facing direction d.
    capPts :: Pt -> Pt -> [Pt]
    capPts p@(Pt px py) d@(Pt dx dy) =
      let Pt nx ny = normalOf d
      in case cap of
           CapButt -> []
           CapSquare -> [ Pt (px + nx + dx * h) (py + ny + dy * h)
                        , Pt (px - nx + dx * h) (py - ny + dy * h) ]
           CapRound -> init' (arcPoints p h (atan2 ny nx) (-pi) tol)

    openOutline vs =
      let dirs = zipWith unitDir vs (drop 1 vs)
          p0 = head vs; pE = last vs
          d0 = fst (head dirs); dE = fst (last dirs)
          interior = zip3 (drop 1 vs) dirs (drop 1 dirs)
          side s = offs s p0 (normalOf d0)
                   : concat [ joinSide s v a b | (v, a, b) <- interior ]
                   ++ [offs s pE (normalOf dE)]
          Pt d0x d0y = d0
      in side 1 ++ capPts (pt pE) dE ++ reverse (side (-1))
         ++ capPts (pt p0) (Pt (-d0x) (-d0y))

    closedOutline vs =
      let m = length vs
          nexts = drop 1 vs ++ take 1 vs
          dirs = zipWith unitDir vs nexts          -- segment i: vs!!i -> vs!!(i+1)
          prevDirs = last dirs : init dirs         -- segment arriving at vertex i
          side s = concat [ joinSide s v a b | (v, a, b) <- zip3 vs prevDirs dirs ]
      in if m < 2 then [] else [side 1, reverse (side (-1))]

-- | Stroke polylines with a style (width, cap, join, miter limit; the dash
-- fields are ignored here, see 'dashPolylines').
strokePolylines :: StrokeStyle -> Double -> [PolyLine] -> [[Pt]]
strokePolylines ss tol =
  concatMap (strokePolyline (ssWidth ss / 2) (ssCap ss) (ssJoin ss) (ssMiterLimit ss) tol)

-- | Flatten, dash and stroke a path in its own space: the polygons whose
-- non-zero fill is the stroke. A width of zero or less produces nothing
-- (a hairline is a device concept; the rasteriser handles it).
strokeOutlines :: StrokeStyle -> Double -> Path -> [[Pt]]
strokeOutlines ss tol path =
  let pls = flattenPolylines tol path
      pls' = case validDash (ssDash ss) of
               Just pat -> dashPolylines pat (ssDashPhase ss) pls
               Nothing -> pls
  in strokePolylines ss tol pls'

-- | The stroke of a path as a path to fill under 'Cmedit.Scene.NonZero',
-- with curves flattened to the given tolerance (in the path's own units).
strokeToPath :: StrokeStyle -> Double -> Path -> Path
strokeToPath ss tol = polygonsToPath . strokeOutlines ss tol

-- | The dashes of a path, as open polyline subpaths.
dashPath :: [Double] -> Double -> Double -> Path -> Path
dashPath pat phase tol path =
  let pls = flattenPolylines tol path
      pls' = case validDash pat of
               Just p -> dashPolylines p phase pls
               Nothing -> pls
  in concatMap toPath pls'
  where
    toPath (PolyLine vs closed _) = case vs of
      [] -> []
      (Vtx x y _ : rest) ->
        MoveTo (Pt x y) : [ LineTo (Pt a b) | Vtx a b _ <- rest ]
        ++ (if null rest && not closed then [LineTo (Pt x y)] else [])
        ++ [ ClosePath | closed ]

-- | A path flattened to polylines: points and a closed flag per subpath.
flattenPath :: Double -> Path -> [([Pt], Bool)]
flattenPath tol path =
  [ ([ Pt x y | Vtx x y _ <- vs ], c) | PolyLine vs c _ <- flattenPolylines tol path ]

-- | Closed polygons as a path.
polygonsToPath :: [[Pt]] -> Path
polygonsToPath = concatMap one
  where
    one [] = []
    one (p : ps) = MoveTo p : map LineTo ps ++ [ClosePath]
