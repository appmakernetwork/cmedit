{-# LANGUAGE BangPatterns #-}
-- | PDF shadings (PDF 32000 §8.7.4.5) as "Cmedit.Scene" paints and meshes,
-- for the page renderer ("Cmedit.PdfRender").
--
-- A shading is the same object whether it is painted by the @sh@ operator
-- or used as a pattern's paint, so this module knows nothing about how it is
-- used: it returns a 'Shading' in /shading space/ and the caller composes
-- the matrix that puts shading space where it belongs (the CTM for @sh@, the
-- pattern matrix for a shading pattern).
--
-- Three decisions carry it:
--
--   * __A function of one parameter becomes stops__ (axial, radial, and the
--     meshes that carry a @\/Function@). The function is sampled at 256
--     points over its @\/Domain@ and the samples that a straight line through
--     their neighbours already reproduces (to a quarter of a code value) are
--     dropped, so a two-colour linear ramp costs two stops while a stitched
--     rainbow keeps every bend. Interpolating /colours/ along a mesh edge
--     would be wrong when the function is not linear in its parameter, which
--     is why the meshes with a function become 'CGouraudRamp' (the parameter
--     is interpolated and looked up) and only the ones with per-vertex
--     colours become 'CGouraud'.
--
--   * __A function of two parameters becomes an image__ (type 1): sampled on
--     a grid over its @\/Domain@ into a 'PSampled' paint, transparent outside
--     the domain because a type 1 shading paints nothing there.
--
--   * __Patches become triangles here, at a resolution chosen in shading
--     space__, not per device: the scene is resolution-independent, so a
--     patch is cut into an @n×n@ grid with @n@ from its control polygon's
--     extent (one cell per 24 units, at most 12 a side), and a shading's
--     patches share 'patchTriangles' between them, @n@ lowered uniformly
--     when there are many. Coons patches (type 6) are converted to tensor patches by the
--     standard interior-point formula, so one evaluator serves both.
--
-- The mesh stream reader follows poppler (the oracle) where the spec is
-- quiet: every type 4 and type 5 vertex, and every type 6 and 7 patch,
-- starts on a byte boundary. A truncated stream ends the mesh where the data
-- ends; an edge flag with no previous patch or triangle to continue from
-- skips that element. Nothing here throws.
module Cmedit.PdfRenderShade
  ( Shading(..)
  , ShadeBody(..)
  , parseShading
  , sampleStops
  , retargetPaint
  , extendSpread
  , maxMeshTriangles
  , maxMeshVertices
  , paintColor
  , cmykToRGB
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BSU
import Data.Array.Unboxed (listArray)
import qualified Data.Map.Strict as M
import Data.Word (Word8)

import Cmedit.Geom
import Cmedit.Image (Image(..))
import Cmedit.PdfFunc (PdfFunction, parseFunction, evalFunction)
import Cmedit.PdfImage (ColorSpace(..), parseColorSpace, csComponents, defaultDecode, toColor, cmykToRGB)
import Data.Array.Unboxed (UArray, bounds, (!))
import Cmedit.PdfObj
import Cmedit.Scene

-- | A parsed shading, in shading space.
data Shading = Shading
  { shBody       :: ShadeBody
  , shBBox       :: Maybe Rect    -- ^ @\/BBox@, shading space: painting is clipped to it.
  , shBackground :: Maybe Color   -- ^ @\/Background@: only a shading /pattern/ paints it.
  } deriving (Show)

data ShadeBody
  = ShPaint Paint                 -- ^ Axial, radial, function-based: a paint over shading space.
  | ShTris [GouraudTri]           -- ^ A mesh with per-vertex colours.
  | ShRamp [Stop] [RampTri]       -- ^ A mesh whose vertices carry the function's parameter (normalised to [0,1]).
  | ShNothing                     -- ^ Degenerate: paints nothing.
  deriving (Show)

-- | A colour operand as the renderer paints it: "Cmedit.PdfImage"'s
-- 'toColor', whose DeviceCMYK conversion is poppler's sixteen-corner blend
-- ('cmykToRGB', also re-exported from here). It lives there and only there,
-- so a vector fill and an image of the same CMYK colour agree.
paintColor :: ColorSpace -> [Double] -> Double -> Color
paintColor = toColor

-- | Triangles one mesh shading may produce (free-form and lattice meshes
-- list theirs; patch meshes are held to 'patchTriangles').
maxMeshTriangles :: Int
maxMeshTriangles = 100000

-- | Triangles the patches of one shading are cut into, in all. A patch's
-- grid follows its size (a 24 pt patch is two triangles, a page-sized one
-- at most 12×12 cells), and many patches share this budget evenly, so a
-- file of 20 000 random patches costs 40 000 triangles, not millions.
patchTriangles :: Int
patchTriangles = 40000

-- | Vertices (or patches) read from one mesh stream.
maxMeshVertices :: Int
maxMeshVertices = 400000

-- | PDF @\/Extend [a b]@ as a 'Spread'.
extendSpread :: Bool -> Bool -> Spread
extendSpread a b = case (a, b) of
  (True, True)   -> SpreadPad
  (True, False)  -> SpreadPadStart
  (False, True)  -> SpreadPadEnd
  (False, False) -> SpreadNone

-- | Re-express a paint built over shading space in some other space: the
-- paint's matrix becomes @outer ∘ m@.
retargetPaint :: Affine -> Paint -> Paint
retargetPaint o p = case p of
  PSolid _ -> p
  PLinear m a b st sp -> PLinear (composeA o m) a b st sp
  PRadial m a ra b rb st sp -> PRadial (composeA o m) a ra b rb st sp
  PSampled m img sp -> PSampled (composeA o m) img sp
  PTiling k m cell xs ys cmds -> PTiling k (composeA o m) cell xs ys cmds

-- | Parse a shading dictionary or stream (an object as the @\/Shading@
-- resource or a shading pattern's @\/Shading@ holds it). Charged to the
-- document's fuel: functions and mesh streams are decoded.
parseShading :: PdfFile -> Fuel -> Dict -> Obj -> (Either String Shading, Fuel)
parseShading f fu0 res o
  | fuelSpent fu0 = (Left "the document's budget is spent", fu0)
  | otherwise = case parseColorSpace f fu0 res csObj of
      (Left e, fu1) -> (Left ("shading colour space: " ++ e), fu1)
      (Right cs, fu1) ->
        let bbox = rectOf f (lookD f d "BBox")
            bg = case numsOf f (lookD f d "Background") of
                   [] -> Nothing
                   xs -> Just (paintColor cs xs 1)
            (fnE, fu2) = case M.lookup (BC.pack "Function") d of
              Nothing -> (Nothing, fu1)
              Just fo -> case parseFunction f fu1 fo of
                (Right fn, fu') -> (Just (Right fn), fu')
                (Left e, fu')   -> (Just (Left e), fu')
            wrap (Left e, fu') = (Left e, fu')
            wrap (Right b, fu') = (Right (Shading b bbox bg), fu')
        in case fnE of
             Just (Left e) -> (Left ("shading function: " ++ e), fu2)
             _ -> let fn = case fnE of Just (Right x) -> Just x; _ -> Nothing
                  in wrap (body cs fn fu2)
  where
    d = dictOf f o
    raw = case resolve f o of
      OStream _ r -> Just r
      _ -> Nothing
    csObj = M.findWithDefault ONull (BC.pack "ColorSpace") d
    sty = intOf f (M.lookup (BC.pack "ShadingType") d)
    extend = case map (boolOf f) (arrOf f (lookD f d "Extend")) of
      [Just a, Just b] -> (a, b)
      _ -> (False, False)
    domain1 = case numsOf f (lookD f d "Domain") of
      [a, b] -> (a, b)
      _ -> (0, 1)
    colourOf cs fn t = case fn of
      Just g -> paintColor cs (evalFunction g [t]) 1
      Nothing -> paintColor cs [t] 1
    body cs fn fu = case sty of
      1 -> case fn of
        Nothing -> (Left "function-based shading without a function", fu)
        Just g -> (Right (functionShading cs g), spendTokens (sampleN * sampleN `div` 8) fu)
      2 -> case (numsOf f (lookD f d "Coords"), fn) of
        ([x0, y0, x1, y1], Just _)
          | abs (x1 - x0) + abs (y1 - y0) < 1e-12 -> (Right ShNothing, fu)
          | otherwise ->
              let stops = sampleStops (colourOf cs fn) domain1
              in (Right (ShPaint (PLinear identityA (Pt x0 y0) (Pt x1 y1) stops (uncurry extendSpread extend))), spendTokens 256 fu)
        _ -> (Left "axial shading without coordinates or a function", fu)
      3 -> case (numsOf f (lookD f d "Coords"), fn) of
        ([x0, y0, r0, x1, y1, r1], Just _)
          | r0 < 0 || r1 < 0 -> (Right ShNothing, fu)
          | otherwise ->
              let stops = sampleStops (colourOf cs fn) domain1
              in (Right (ShPaint (PRadial identityA (Pt x0 y0) r0 (Pt x1 y1) r1 stops (uncurry extendSpread extend))), spendTokens 256 fu)
        _ -> (Left "radial shading without coordinates or a function", fu)
      t | t >= 4 && t <= 7 -> case raw of
        Nothing -> (Left "mesh shading without a data stream", fu)
        Just bytes -> case decodeStreamF f fu d bytes of
          (Left e, fu') -> (Left e, fu')
          (Right dat, fu') ->
            let r = meshShading f d t cs fn dat
            in (r, spendTokens (BS.length dat `div` 4 + 1) fu')
      t -> (Left ("shading type " ++ show t ++ " is not supported"), fu)

    -- Type 1: sample the two-input function over its domain into an image.
    functionShading cs g =
      let (x0, x1, y0, y1) = case numsOf f (lookD f d "Domain") of
            [a, b, c, e] -> (a, b, c, e)
            _ -> (0, 1, 0, 1)
          m = maybe identityA id (matrixOf f (lookD f d "Matrix"))
          n = sampleN
          px i j = let u = (fromIntegral i + 0.5) / fromIntegral n
                       v = (fromIntegral j + 0.5) / fromIntegral n
                       Color r gg b _ = paintColor cs (evalFunction g [x0 + u * (x1 - x0), y0 + v * (y1 - y0)]) 1
                   in [q r, q gg, q b, 255]
          q c = fromIntegral (max 0 (min 255 (round (c * 255) :: Int))) :: Word8
          pix = listArray (0, n * n * 4 - 1) (concat [ px i j | j <- [0 .. n - 1], i <- [0 .. n - 1] ])
          img = Image n n "shading" pix
          toDomain = Affine (x1 - x0) 0 0 (y1 - y0) x0 y0
      in if x1 == x0 || y1 == y0 then ShNothing
         else ShPaint (PSampled (composeA m toDomain) img SpreadNone)

-- Grid resolution for a function-based shading.
sampleN :: Int
sampleN = 128

-- | Sample a colour function of one parameter over a domain into stops
-- (offsets 0..1 along the domain), dropping samples a straight line
-- through their neighbours reproduces.
sampleStops :: (Double -> Color) -> (Double, Double) -> [Stop]
sampleStops col (t0, t1) = simplify samples
  where
    n = 256 :: Int
    samples = [ let s = fromIntegral i / fromIntegral (n - 1) in Stop s (col (t0 + s * (t1 - t0))) | i <- [0 .. n - 1] ]
    tol = 0.25 / 255
    -- Greedy: keep a stop when the line from the last kept stop to the next
    -- candidate misses any sample in between.
    simplify [] = []
    simplify [s] = [s]
    simplify (s0 : rest) = s0 : go s0 [] rest
    go _ _ [] = []
    go _ _ [l] = [l]
    go kept between (c : more@(nxt : _)) =
      if all (onLine kept nxt) (c : between)
        then go kept (c : between) more
        else c : go c [] more
    onLine (Stop a ca) (Stop b cb) (Stop s cs)
      | b <= a = True
      | otherwise =
          let u = (s - a) / (b - a)
              Color r0 g0 b0 a0 = ca; Color r1 g1 b1 a1 = cb; Color r g bb aa = cs
              near x y z = abs (x + u * (y - x) - z) <= tol
          in near r0 r1 r && near g0 g1 g && near b0 b1 bb && near a0 a1 aa

------------------------------------------------------------------------------
-- Meshes

-- A big-endian bit reader over the mesh stream: position in bits.
data BitR = BitR !BS.ByteString !Int

bitsLeft :: BitR -> Int
bitsLeft (BitR b p) = BS.length b * 8 - p

readBitsR :: Int -> BitR -> (Integer, BitR)
readBitsR k (BitR b p) = (go 0 p k, BitR b (p + k))
  where
    go !acc _ 0 = acc
    go !acc q left =
      let byteI = q `shiftR` 3
          bit = q .&. 7
          byte = if byteI < BS.length b then fromIntegral (BSU.unsafeIndex b byteI) else 0 :: Int
          avail = 8 - bit
          take' = min avail left
          v = (byte `shiftR` (avail - take')) .&. ((1 `shiftL` take') - 1)
      in go ((acc `shiftL` take') .|. fromIntegral v) (q + take') (left - take')

alignR :: BitR -> BitR
alignR (BitR b p) = BitR b ((p + 7) .&. complement7)
  where complement7 = -8

-- One decoded vertex: position (shading space) and either colour
-- components or the function parameter.
data V = V !Pt [Double]

meshShading :: PdfFile -> Dict -> Int -> ColorSpace -> Maybe PdfFunction -> BS.ByteString -> Either String ShadeBody
meshShading f d ty cs fn dat
  | bpc `notElem` [1, 2, 4, 8, 12, 16, 24, 32] = Left "mesh shading: bad /BitsPerCoordinate"
  | bpcomp `notElem` [1, 2, 4, 8, 12, 16] = Left "mesh shading: bad /BitsPerComponent"
  | ty /= 5 && bpf `notElem` [2, 4, 8] = Left "mesh shading: bad /BitsPerFlag"
  | length dec < 4 + 2 * nc = Left "mesh shading: short /Decode"
  | ty == 5 && vpr < 2 = Left "mesh shading: bad /VerticesPerRow"
  | otherwise = Right $ case ty of
      4 -> finish (freeForm (BitR dat 0))
      5 -> finish (lattice (BitR dat 0))
      _ -> finish (patches (BitR dat 0))
  where
    bpc = intOf f (M.lookup (BC.pack "BitsPerCoordinate") d)
    bpcomp = intOf f (M.lookup (BC.pack "BitsPerComponent") d)
    bpf = intOf f (M.lookup (BC.pack "BitsPerFlag") d)
    vpr = intOf f (M.lookup (BC.pack "VerticesPerRow") d)
    nc = case fn of Just _ -> 1; Nothing -> csComponents cs
    dec = numsOf f (lookD f d "Decode")
    pairsD (a : b : r) = (a, b) : pairsD r
    pairsD _ = []
    decPairs = pairsD dec
    (xr, yr) = case decPairs of (a : b : _) -> (a, b); _ -> ((0, 1), (0, 1))
    cr = take nc (drop 2 decPairs)
    (t0, t1) = case cr of ((a, b) : _) -> (a, b); _ -> (0, 1)
    scaleTo :: Int -> (Double, Double) -> Integer -> Double
    scaleTo bits (lo, hi) v =
      let mx = fromIntegral ((2 :: Integer) ^ bits - 1) :: Double
      in lo + fromIntegral v * (hi - lo) / mx
    vertexBits = 2 * bpc + nc * bpcomp
    readV r0 =
      let (xv, r1) = readBitsR bpc r0
          (yv, r2) = readBitsR bpc r1
          (cs', r3) = comps nc cr r2 []
      in (V (Pt (scaleTo bpc xr xv) (scaleTo bpc yr yv)) cs', r3)
    comps 0 _ r acc = (reverse acc, r)
    comps k rngs r acc =
      let (v, r') = readBitsR bpcomp r
          rg = case rngs of (x : _) -> x; [] -> (0, 1)
      in comps (k - 1 :: Int) (drop 1 rngs) r' (scaleTo bpcomp rg v : acc)
    readCoord r0 = let (xv, r1) = readBitsR bpc r0
                       (yv, r2) = readBitsR bpc r1
                   in (Pt (scaleTo bpc xr xv) (scaleTo bpc yr yv), r2)

    -- Type 4: a flag per vertex; 0 starts a triangle, 1 and 2 continue
    -- from the previous one's edge (bc or ac).
    freeForm r0 = go r0 (0 :: Int) [] Nothing
      where
        go r !k acc st
          | k >= maxMeshVertices || bitsLeft r < bpf + vertexBits = acc
          | otherwise =
              let (flag, r1) = readBitsR bpf r
                  (v, r2) = readV r1
                  r3 = alignR r2
              in case (flag, st) of
                   (0, _) -> collect r3 (k + 1) acc [v]
                   (1, Just (_, b, c)) -> go r3 (k + 1) (tri b c v : acc) (Just (b, c, v))
                   (2, Just (a, _, c)) -> go r3 (k + 1) (tri a c v : acc) (Just (a, c, v))
                   _ -> go r3 (k + 1) acc st
        -- After a 0 flag the next two vertices complete the triangle
        -- whatever their flags say.
        collect r !k acc vs
          | length vs == 3 = case vs of
              [c, b, a] -> go r k (tri a b c : acc) (Just (a, b, c))
              _ -> acc
          | k >= maxMeshVertices || bitsLeft r < bpf + vertexBits = acc
          | otherwise =
              let (_, r1) = readBitsR bpf r
                  (v, r2) = readV r1
              in collect (alignR r2) (k + 1) acc (v : vs)

    -- Type 5: rows of VerticesPerRow vertices, each adjacent pair of rows a
    -- strip of quads.
    lattice r0 =
      let rows = readRows r0 (0 :: Int) []
          strips (a : b : rest) = concat (zipWith4' quad a (drop 1 a) b (drop 1 b)) ++ strips (b : rest)
          strips _ = []
          quad p q s t = [tri p q s, tri q t s]
      in strips rows
      where
        readRows r !k acc
          | k >= maxMeshVertices = reverse acc
          | otherwise = case readRow r vpr [] of
              Just (row, r') -> readRows r' (k + vpr) (row : acc)
              Nothing -> reverse acc
        readRow r 0 acc = Just (reverse acc, r)
        readRow r m acc
          | bitsLeft r < vertexBits = Nothing
          | otherwise = let (v, r') = readV r in readRow (alignR r') (m - 1) (v : acc)

    -- Types 6 and 7: patches, as 16 control points p_ij (i along u, j
    -- along v) and four corner colours, then cut into triangles.
    patches r0 =
      let ps = readPatches r0 (0 :: Int) Nothing []
          np = length ps
          perSide = max 1 (floor (sqrt (fromIntegral patchTriangles / (2 * fromIntegral (max 1 np)) :: Double)))
      in concatMap (patchTris perSide) ps
      where
        nNew f0 = if ty == 6 then (if f0 == 0 then 12 else 8) else (if f0 == 0 then 16 else 12)
        readPatches r !k prev acc
          | k >= maxMeshVertices `div` 16 = reverse acc
          | bitsLeft r < bpf = reverse acc
          | otherwise =
              let (flag, r1) = readBitsR bpf r
                  fl = fromIntegral flag :: Int
                  np' = nNew fl
                  nCol = if fl == 0 then 4 else 2
                  need = np' * 2 * bpc + nCol * nc * bpcomp
              in if bitsLeft r1 < need then reverse acc
                 else
                   let (pts, r2) = readN np' r1 []
                       (cols, r3) = readCols nCol r2 []
                       r4 = alignR r3
                   in case assemble fl prev pts cols of
                        Just p -> readPatches r4 (k + 1) (Just p) (p : acc)
                        Nothing -> readPatches r4 (k + 1) prev acc
        readN 0 r acc = (reverse acc, r)
        readN m r acc = let (p, r') = readCoord r in readN (m - 1 :: Int) r' (p : acc)
        readCols 0 r acc = (reverse acc, r)
        readCols m r acc = let (c, r') = comps nc cr r [] in readCols (m - 1 :: Int) r' (c : acc)

    -- Boundary order of a patch's points as the stream lists them, then the
    -- four interior ones (tensor patches only).
    boundaryIdx = [(0,0),(0,1),(0,2),(0,3),(1,3),(2,3),(3,3),(3,2),(3,1),(3,0),(2,0),(1,0)]
    interiorIdx = [(1,1),(1,2),(2,2),(2,1)]
    assemble :: Int -> Maybe Patch -> [Pt] -> [[Double]] -> Maybe Patch
    assemble fl prev pts cols = case fl of
      0 -> let named = zip (boundaryIdx ++ (if ty == 7 then interiorIdx else [])) pts
           in case cols of
                [c00, c03, c33, c30] -> Just (complete named (c00, c03, c33, c30))
                _ -> Nothing
      _ -> do
        Patch pm (pc00, pc03, pc33, pc30) <- prev
        let at ij = M.findWithDefault (Pt 0 0) ij pm
            (shared, sc0, sc1) = case fl of
              1 -> (map at [(0,3),(1,3),(2,3),(3,3)], pc03, pc33)
              2 -> (map at [(3,3),(3,2),(3,1),(3,0)], pc33, pc30)
              _ -> (map at [(3,0),(2,0),(1,0),(0,0)], pc30, pc00)
            named = zip (boundaryIdx ++ (if ty == 7 then interiorIdx else [])) (shared ++ pts)
        case cols of
          [c33, c30] -> Just (complete named (sc0, sc1, c33, c30))
          _ -> Nothing
    complete named cs4 =
      let m0 = M.fromList named
          at ij = M.findWithDefault (Pt 0 0) ij m0
          coons = M.fromList
            [ ((1,1), comb [(-4, at (0,0)), (6, at (0,1)), (6, at (1,0)), (-2, at (0,3)), (-2, at (3,0)), (3, at (3,1)), (3, at (1,3)), (-1, at (3,3))])
            , ((1,2), comb [(-4, at (0,3)), (6, at (0,2)), (6, at (1,3)), (-2, at (0,0)), (-2, at (3,3)), (3, at (3,2)), (3, at (1,0)), (-1, at (3,0))])
            , ((2,1), comb [(-4, at (3,0)), (6, at (3,1)), (6, at (2,0)), (-2, at (3,3)), (-2, at (0,0)), (3, at (0,1)), (3, at (2,3)), (-1, at (0,3))])
            , ((2,2), comb [(-4, at (3,3)), (6, at (3,2)), (6, at (2,3)), (-2, at (3,0)), (-2, at (0,3)), (3, at (0,2)), (3, at (2,0)), (-1, at (0,0))]) ]
          m1 = if ty == 6 then M.union m0 coons else m0
      in Patch m1 cs4
    comb ws = let Pt x y = foldr (\(w, p) acc -> ptAdd acc (ptScale w p)) (Pt 0 0) ws in Pt (x / 9) (y / 9)

    patchTris cap (Patch pm (c00, c03, c33, c30)) =
      let at ij = M.findWithDefault (Pt 0 0) ij pm
          ctrl = [ [ at (i, j) | j <- [0 .. 3] ] | i <- [0 .. 3 :: Int] ]
          ext = case pathBounds [ MoveTo p | row <- ctrl, p <- row ] of
                  Just r -> max (rectW r) (rectH r)
                  Nothing -> 0
          n = max 1 (min cap (min 12 (ceiling (ext / 24))))
          bern t = let s = 1 - t in [s * s * s, 3 * s * s * t, 3 * s * t * t, t * t * t]
          surf u v =
            let bu = bern u; bv = bern v
            in foldr ptAdd (Pt 0 0)
                 [ ptScale (wu * wv) p | (wu, row) <- zip bu ctrl, (wv, p) <- zip bv row ]
          colAt u v = [ (1 - u) * (1 - v) * a + (1 - u) * v * b + u * v * c + u * (1 - v) * e
                      | (a, b, c, e) <- zip4' c00 c03 c33 c30 ]
          grid = [ [ let u = fromIntegral i / fromIntegral n; v = fromIntegral j / fromIntegral n
                     in V (surf u v) (colAt u v) | j <- [0 .. n] ] | i <- [0 .. n] ]
          quads (a : b : rest) = concat (zipWith4' (\p q s t -> [tri p q s, tri q t s]) a (drop 1 a) b (drop 1 b)) ++ quads (b : rest)
          quads _ = []
      in quads grid

    tri a b c = (a, b, c)
    finish :: [(V, V, V)] -> ShadeBody
    finish tris0 =
      let tris = take maxMeshTriangles tris0
      in case fn of
           Just g ->
             let norm t = if t1 == t0 then 0 else (t - t0) / (t1 - t0)
                 tv (V p cs') = (p, norm (case cs' of (x : _) -> x; [] -> t0))
                 stops = sampleStops (\t -> paintColor cs (evalFunction g [t]) 1) (t0, t1)
             in ShRamp stops [ RampTri pa ta pb tb pc tc | (a, b, c) <- tris
                             , let (pa, ta) = tv a; (pb, tb) = tv b; (pc, tc) = tv c ]
           Nothing ->
             let cv (V p cs') = (p, paintColor cs cs' 1)
             in ShTris [ GouraudTri pa ca pb cb pc cc | (a, b, c) <- tris
                       , let (pa, ca) = cv a; (pb, cb) = cv b; (pc, cc) = cv c ]

data Patch = Patch (M.Map (Int, Int) Pt) ([Double], [Double], [Double], [Double])

zipWith4' :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4' g (a : as) (b : bs) (c : cs) (e : es) = g a b c e : zipWith4' g as bs cs es
zipWith4' _ _ _ _ _ = []

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' = zipWith4' (,,,)
