{-# LANGUAGE BangPatterns #-}
-- | SVG filter primitives over premultiplied float layers, for
-- "Cmedit.Raster".
--
-- A filter runs at device resolution over one box (the caller's filter
-- region intersected with the visible area plus whatever margin the
-- primitives read, see 'filterMargin'), with every intermediate result a
-- buffer of that box's geometry. Results are tagged with the colour space
-- they are in, and an input is converted only when the consuming primitive
-- wants the other one (@color-interpolation-filters@), so a chain of
-- linearRGB primitives pays for one conversion in and one out, and an
-- offset or a merge never converts at all. The output is always sRGB.
--
-- __Parameters are scaled the way resvg scales them__, because resvg is the
-- reference this module is held to: a blur's standard deviations and an
-- offset's distances are multiplied by the matrix's x and y scale (the
-- lengths of its columns) rather than transformed as vectors, and an offset
-- is truncated to whole device pixels. Under rotation or skew that is an
-- approximation (the spec would rotate the blur's axes), and it is the one
-- every renderer we compare against makes. Gaussian blur follows resvg as
-- well: a true Gaussian kernel for small deviations, and for larger ones
-- three box blurs whose widths are chosen so their variance matches
-- (Kovesi's construction), which is what keeps a large blur linear in the
-- area rather than in area × radius.
module Cmedit.RasterFilter
  ( runFilter
  , filterMargin
  , blurBuffer
  ) where

import Control.Monad (forM_, when, foldM)
import Control.Monad.ST (ST)
import Data.Array.Base (numElements)
import Data.Array.Unboxed (UArray, listArray)
import Data.Array.ST (STUArray, newArray)
import qualified Data.Map.Strict as M

import Cmedit.Geom
import Cmedit.RasterCore
import Cmedit.Scene

type Buf s = STUArray s Int Float

-- | Device pixels a chain of primitives may read beyond what it writes.
filterMargin :: Affine -> [FilterPrim] -> Double
filterMargin m prims = sum (map (one . fpOp) prims) + 2
  where
    (sx, sy) = axisScales m
    blurM a b = 3 * max (a * sx) (b * sy) + 3
    one op = case op of
      FeBlur a b -> blurM (abs a) (abs b)
      FeOffset dx dy -> offM dx dy
      FeDropShadow a b dx dy _ -> blurM (abs a) (abs b) + offM dx dy
      _ -> 0
    offM dx dy = let Pt x y = applyLinearA m (Pt dx dy) in abs x + abs y + 1

-- | An offset vector in the filter's user space as whole device pixels: the
-- full linear part of the matrix, so a rotated or flipped element's shadow
-- falls where the spec (and the browsers) put it. Truncated toward zero
-- like resvg's, which only matters for fractional results; non-finite or
-- absurd offsets become no shift.
deviceOffset :: Affine -> Double -> Double -> (Int, Int)
deviceOffset m dx dy =
  let Pt x y = applyLinearA m (Pt dx dy)
      -- (a hair of slack, so rotate(90)'s cos 90° = 6e-17 does not turn
      -- an exact -4 into -3.9999999, which truncates to -3)
      t v = if abs v < 1e9 then truncate (v + signum v * 1e-9) else 0
  in (t x, t y)

axisScales :: Affine -> (Double, Double)
axisScales (Affine a b c d _ _) = (sqrt (a * a + b * b), sqrt (c * c + d * d))

-- | Run a filter: the matrix from the filter's user space to device, the
-- working box, the filter region's pixel box, the primitives, and the
-- source graphic (a layer over the working box). Returns a layer over the
-- working box holding the result, transparent outside the region.
runFilter :: Env s -> Affine -> IBox -> IBox -> [FilterPrim] -> Layer s -> ST s (Layer s)
runFilter env m fb@(IBox fx0 fy0 fx1 fy1) region prims src = do
  let w = fx1 - fx0; h = fy1 - fy0; n = w * h
      srcBuf = lyPix src
      newBuf = newArray (0, max 1 (4 * n) - 1) 0 :: ST s (Buf s)
  alphaRef <- newBuf
  forM_ [0 .. n - 1] $ \i -> rdA srcBuf (4 * i + 3) >>= wrA alphaRef (4 * i + 3)
  let sourceG = (srcBuf, False)
      sourceA = (alphaRef, False)
      resolve prev results inp = case inp of
        SourceGraphic -> sourceG
        SourceAlpha -> sourceA
        Previous -> prev
        Result k -> M.findWithDefault prev k results
      step acc@(prev, results) prim = do
        charge env (n * 4)
        ok <- budgetLeft env
        if not ok then return acc else stepPrim prev results prim
      stepPrim prev results prim = do
        let lin = fpLinearRGB prim
            sub = iboxIntersect fb (iboxIntersect region (case fpSubregion prim of
                    Nothing -> region
                    Just r -> rectToIBox (transformRect m r)))
            ins = map (resolve prev results) (fpIns prim)
            in1 = case ins of (a : _) -> a; [] -> prev
            in2 = case ins of (_ : b : _) -> b; _ -> prev
            inSp (b, l) = toSpace env n lin b l
        out <- newBuf
        case fpOp prim of
          FeBlur sx sy -> do
            b <- inSp in1
            copyBuf b out n
            let (kx, ky) = axisScales m
            blurBuffer out w h (max 0 sx * kx) (max 0 sy * ky)
          FeOffset dx dy -> do
            b <- inSp in1
            let (ox, oy) = deviceOffset m dx dy
            shiftBuf b out w h ox oy
          FeFlood (Color r g bl a) -> do
            let conv v = if lin then toLinear v else v
                a' = realToFrac (clamp01 a) :: Float
                C4 pr pg pb pa = C4 (realToFrac (conv (clamp01 r)) * a') (realToFrac (conv (clamp01 g)) * a')
                                    (realToFrac (conv (clamp01 bl)) * a') a'
            fillBox out w fb sub pr pg pb pa
          FeColorMatrix vs -> do
            b <- inSp in1
            colorMatrix b out n (if length vs == 20 then map realToFrac vs else identityCM)
          FeComposite op -> do
            a <- inSp in1
            b <- inSp in2
            composite op a b out n
          FeMerge -> do
            bufs <- mapM inSp ins
            forM_ bufs $ \b -> overInto b out n
          FeBlend bm -> do
            a <- inSp in1
            b <- inSp in2
            forM_ [0 .. n - 1] $ \i -> do
              let j = 4 * i
              sr <- rdA a j; sg <- rdA a (j + 1); sb <- rdA a (j + 2); sa <- rdA a (j + 3)
              dr <- rdA b j; dg <- rdA b (j + 1); db <- rdA b (j + 2); da <- rdA b (j + 3)
              let C4 r g bb aa = blendOver bm sr sg sb sa dr dg db da
              wrA out j r; wrA out (j + 1) g; wrA out (j + 2) bb; wrA out (j + 3) aa
          FeDropShadow sx sy dx dy (Color r g bl a) -> do
            b <- inSp in1
            sh <- newBuf
            forM_ [0 .. n - 1] $ \i -> rdA b (4 * i + 3) >>= wrA sh (4 * i + 3)
            let (kx, ky) = axisScales m
            blurBuffer sh w h (max 0 sx * kx) (max 0 sy * ky)
            let conv v = if lin then toLinear v else v
                a' = realToFrac (clamp01 a) :: Float
                fr = realToFrac (conv (clamp01 r)); fg = realToFrac (conv (clamp01 g)); fbl = realToFrac (conv (clamp01 bl))
            forM_ [0 .. n - 1] $ \i -> do
              al <- rdA sh (4 * i + 3)
              let k = al * a'
              wrA sh (4 * i) (fr * k); wrA sh (4 * i + 1) (fg * k)
              wrA sh (4 * i + 2) (fbl * k); wrA sh (4 * i + 3) k
            let (ox, oy) = deviceOffset m dx dy
            shiftBuf sh out w h ox oy
            overInto b out n
        clearOutside out w fb sub
        let r = (out, lin)
        return (r, M.insert (fpResult prim) r results)
  (final, _) <- foldM step (sourceG, M.empty) prims
  res <- toSpace env n False (fst final) (snd final)
  -- Keep the result inside the filter region.
  out <- if res == srcBuf then do b <- newBuf; copyBuf res b n; return b else return res
  clearOutside out w fb (iboxIntersect fb region)
  return (Layer fx0 fy0 w h out Nothing Nothing)
  where
    clamp01 :: Double -> Double
    clamp01 v = if v <= 0 then 0 else if v >= 1 then 1 else v
    identityCM = [1,0,0,0,0, 0,1,0,0,0, 0,0,1,0,0, 0,0,0,1,0]

copyBuf :: Buf s -> Buf s -> Int -> ST s ()
copyBuf a b n = let go !i | i >= 4 * n = return ()
                          | otherwise = rdA a i >>= wrA b i >> go (i + 1)
                in go 0

-- Convert between sRGB and linearRGB (premultiplied in, premultiplied out),
-- returning the input itself when no conversion is needed.
toSpace :: Env s -> Int -> Bool -> Buf s -> Bool -> ST s (Buf s)
toSpace _ n want b have
  | want == have = return b
  | otherwise = do
      out <- newArray (0, max 1 (4 * n) - 1) 0
      let f = if want then toLinear else fromLinear
      forM_ [0 .. n - 1] $ \i -> do
        let j = 4 * i
        a <- rdA b (j + 3)
        if a <= 0 then return () else do
          r <- rdA b j; g <- rdA b (j + 1); bl <- rdA b (j + 2)
          let cv v = realToFrac (f (realToFrac (min 1 (v / a)))) * a
          wrA out j (cv r); wrA out (j + 1) (cv g); wrA out (j + 2) (cv bl)
          wrA out (j + 3) a
      return out

toLinear :: Double -> Double
toLinear c | c <= 0.04045 = c / 12.92
           | otherwise = ((c + 0.055) / 1.055) ** 2.4

fromLinear :: Double -> Double
fromLinear c | c <= 0.0031308 = c * 12.92
             | otherwise = 1.055 * c ** (1 / 2.4) - 0.055

-- Zero everything outside a sub-box (both in device coordinates).
clearOutside :: Buf s -> Int -> IBox -> IBox -> ST s ()
clearOutside b w (IBox fx0 fy0 fx1 fy1) (IBox sx0 sy0 sx1 sy1) = do
  let h = fy1 - fy0
  forM_ [0 .. h - 1] $ \r -> do
    let y = fy0 + r
        inside = y >= sy0 && y < sy1
        base = r * w
        zeroR lo hi = forM_ [lo .. hi - 1] $ \c -> do
          let j = 4 * (base + c)
          wrA b j 0; wrA b (j + 1) 0; wrA b (j + 2) 0; wrA b (j + 3) 0
    if not inside then zeroR 0 w else do
      zeroR 0 (max 0 (min w (sx0 - fx0)))
      zeroR (max 0 (min w (sx1 - fx0))) w
  where _ = fx1

fillBox :: Buf s -> Int -> IBox -> IBox -> Float -> Float -> Float -> Float -> ST s ()
fillBox b w (IBox fx0 fy0 _ _) (IBox sx0 sy0 sx1 sy1) r g bl a =
  forM_ [sy0 .. sy1 - 1] $ \y -> forM_ [sx0 .. sx1 - 1] $ \x -> do
    let j = 4 * ((y - fy0) * w + (x - fx0))
    wrA b j r; wrA b (j + 1) g; wrA b (j + 2) bl; wrA b (j + 3) a

-- Integer shift: out(x, y) = in(x - dx, y - dy).
shiftBuf :: Buf s -> Buf s -> Int -> Int -> Int -> Int -> ST s ()
shiftBuf a out w h dx dy =
  forM_ [0 .. h - 1] $ \y -> do
    let sy = y - dy
    when (sy >= 0 && sy < h) $
      forM_ [max 0 dx .. min w (w + dx) - 1] $ \x -> do
        let s = 4 * (sy * w + (x - dx)); d = 4 * (y * w + x)
        rdA a s >>= wrA out d
        rdA a (s + 1) >>= wrA out (d + 1)
        rdA a (s + 2) >>= wrA out (d + 2)
        rdA a (s + 3) >>= wrA out (d + 3)

colorMatrix :: Buf s -> Buf s -> Int -> [Float] -> ST s ()
colorMatrix a out n ms = do
  let mv = listArray (0, 19) (take 20 (ms ++ repeat 0)) :: UArray Int Float
      mm k = atA mv k
      m0 = mm 0; m1 = mm 1; m2 = mm 2; m3 = mm 3; m4 = mm 4
      m5 = mm 5; m6 = mm 6; m7 = mm 7; m8 = mm 8; m9 = mm 9
      m10 = mm 10; m11 = mm 11; m12 = mm 12; m13 = mm 13; m14 = mm 14
      m15 = mm 15; m16 = mm 16; m17 = mm 17; m18 = mm 18; m19 = mm 19
      cl v = if v < 0 then 0 else if v > 1 then 1 else v
  forM_ [0 .. n - 1] $ \i -> do
    let j = 4 * i
    pa <- rdA a (j + 3)
    pr <- rdA a j; pg <- rdA a (j + 1); pb <- rdA a (j + 2)
    let ua = if pa > 0 then 1 / pa else 0
        r = cl (pr * ua); g = cl (pg * ua); b = cl (pb * ua); al = pa
        r' = cl (m0 * r + m1 * g + m2 * b + m3 * al + m4)
        g' = cl (m5 * r + m6 * g + m7 * b + m8 * al + m9)
        b' = cl (m10 * r + m11 * g + m12 * b + m13 * al + m14)
        a' = cl (m15 * r + m16 * g + m17 * b + m18 * al + m19)
    wrA out j (r' * a'); wrA out (j + 1) (g' * a')
    wrA out (j + 2) (b' * a'); wrA out (j + 3) a'

composite :: CompOp -> Buf s -> Buf s -> Buf s -> Int -> ST s ()
composite op a b out n =
  forM_ [0 .. n - 1] $ \i -> do
    let j = 4 * i
    aa <- rdA a (j + 3)
    ba <- rdA b (j + 3)
    forM_ [0 .. 3] $ \k -> do
      av <- rdA a (j + k)
      bv <- rdA b (j + k)
      let v = case op of
            CompOver -> av + bv * (1 - aa)
            CompIn -> av * ba
            CompOut -> av * (1 - ba)
            CompAtop -> av * ba + bv * (1 - aa)
            CompXor -> av * (1 - ba) + bv * (1 - aa)
            CompArithmetic k1 k2 k3 k4 ->
              let r = realToFrac k1 * av * bv + realToFrac k2 * av + realToFrac k3 * bv + realToFrac k4
              in if r < 0 then 0 else if r > 1 then 1 else r
      wrA out (j + k) v
    case op of
      CompArithmetic {} -> do
        al <- rdA out (j + 3)
        forM_ [0 .. 2] $ \k -> do
          v <- rdA out (j + k)
          when (v > al) $ wrA out (j + k) al
      _ -> return ()

overInto :: Buf s -> Buf s -> Int -> ST s ()
overInto a out n =
  forM_ [0 .. n - 1] $ \i -> do
    let j = 4 * i
    sa <- rdA a (j + 3)
    when (sa > 0) $ do
      let ia = 1 - sa
      forM_ [0 .. 3] $ \k -> do
        s <- rdA a (j + k)
        d <- rdA out (j + k)
        wrA out (j + k) (s + d * ia)

-- | Gaussian blur in place (premultiplied RGBA, @w × h@) with device
-- standard deviations; a zero deviation leaves that axis alone.
blurBuffer :: Buf s -> Int -> Int -> Double -> Double -> ST s ()
blurBuffer b w h sx sy = do
  let longest = max w h
  tmp <- newArray (0, 4 * (longest + 2) - 1) 0 :: ST s (Buf s)
  let rows !y | y >= h = return ()
              | otherwise = blurLine b tmp (4 * y * w) 4 w sx >> rows (y + 1)
      cols !x | x >= w = return ()
              | otherwise = blurLine b tmp (4 * x) (4 * w) h sy >> cols (x + 1)
  when (sx > 0 && w > 1) $ rows 0
  when (sy > 0 && h > 1) $ cols 0

-- One line (start offset, stride between pixels, length) blurred in place.
blurLine :: Buf s -> Buf s -> Int -> Int -> Int -> Double -> ST s ()
blurLine b tmp off stride len sigma
  | sigma > 2 = mapM_ (boxPass b tmp off stride len) (boxRadii sigma)
  | otherwise = gaussPass b tmp off stride len (gaussKernel sigma)

-- Box widths whose three-fold convolution approximates the Gaussian.
boxRadii :: Double -> [Int]
boxRadii sigma =
  let n = 3 :: Int
      nd = fromIntegral n
      wIdeal = sqrt (12 * sigma * sigma / nd + 1)
      wl0 = if wIdeal < 1e9 then floor wIdeal else 1000000000 :: Int
      wl = if even wl0 then wl0 - 1 else wl0
      wu = wl + 2
      wlD = fromIntegral wl
      mIdeal = (12 * sigma * sigma - nd * wlD * wlD - 4 * nd * wlD - 3 * nd) / (-4 * wlD - 4)
      m = if abs mIdeal < 10 then round mIdeal else 0 :: Int
  in [ (if i < m then wl else wu) `div` 2 | i <- [0 .. n - 1] ]

-- Copy one line out to the scratch row (4 floats per pixel).
loadLine :: Buf s -> Buf s -> Int -> Int -> Int -> ST s ()
loadLine b tmp off stride len = go 0
  where
    go !i | i >= len = return ()
          | otherwise = do
              let s = off + i * stride; d = 4 * i
              rdA b s >>= wrA tmp d
              rdA b (s + 1) >>= wrA tmp (d + 1)
              rdA b (s + 2) >>= wrA tmp (d + 2)
              rdA b (s + 3) >>= wrA tmp (d + 3)
              go (i + 1)

-- A running-sum box blur of radius r, zero outside the line.
boxPass :: Buf s -> Buf s -> Int -> Int -> Int -> Int -> ST s ()
boxPass b tmp off stride len r
  | r <= 0 = return ()
  | otherwise = do
      loadLine b tmp off stride len
      let inv = 1 / fromIntegral (2 * r + 1) :: Float
          at !k !i = if i < 0 || i >= len then return 0 else rdA tmp (4 * i + k)
          chan !k
            | k >= 4 = return ()
            | otherwise = do
                -- Only the part of the window inside the line contributes,
                -- so a radius longer than the line costs the line.
                let initSum !i !acc | i > min (len - 1) r = return acc
                                    | otherwise = do v <- rdA tmp (4 * i + k); initSum (i + 1) (acc + v)
                s0 <- initSum 0 0
                let go !i !s
                      | i >= len = return ()
                      | otherwise = do
                          wrA b (off + i * stride + k) (s * inv)
                          a <- at k (i + r + 1)
                          d <- at k (i - r)
                          go (i + 1) (s + a - d)
                go 0 s0
                chan (k + 1)
      chan 0

-- Normalised Gaussian weights, radius ceil(3σ).
gaussKernel :: Double -> UArray Int Float
gaussKernel sigma =
  let r = max 1 (ceiling (3 * sigma)) :: Int
      ws = [ exp (-(fromIntegral (i * i)) / (2 * sigma * sigma)) | i <- [-r .. r] ] :: [Double]
      tot = sum ws
  in listArray (0, 2 * r) (map (\v -> realToFrac (v / tot)) ws)

gaussPass :: Buf s -> Buf s -> Int -> Int -> Int -> UArray Int Float -> ST s ()
gaussPass b tmp off stride len ker = do
  loadLine b tmp off stride len
  let r = (numElements ker - 1) `div` 2
      px !i
        | i >= len = return ()
        | otherwise = do
            let lo = max (-r) (-i); hi = min r (len - 1 - i)
                acc !d !sr !sg !sb !sa
                  | d > hi = do
                      let o = off + i * stride
                      wrA b o sr
                      wrA b (o + 1) sg
                      wrA b (o + 2) sb
                      wrA b (o + 3) sa
                  | otherwise = do
                      let wt = atA ker (d + r); j = 4 * (i + d)
                      vr <- rdA tmp j
                      vg <- rdA tmp (j + 1)
                      vb <- rdA tmp (j + 2)
                      va <- rdA tmp (j + 3)
                      acc (d + 1) (sr + vr * wt) (sg + vg * wt) (sb + vb * wt) (sa + va * wt)
            acc lo 0 0 0 0
            px (i + 1)
  px 0
