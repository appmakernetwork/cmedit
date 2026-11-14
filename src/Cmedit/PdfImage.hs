{-# LANGUAGE BangPatterns #-}
-- | PDF colour spaces and image XObjects (PDF 32000 §8.6, §8.9): everything
-- needed to turn an @\/Image@ stream or an inline image into pixels, and a
-- colour operand into sRGB.
--
-- __Colour.__ 'ColorSpace' models every family the format has. Conversion
-- to sRGB ('toRGB') follows what the reference renderers do /without/ colour
-- management, because that is what the oracle (and every reader's eye) is
-- calibrated against:
--
--   * DeviceCMYK is poppler's (and xpdf's) profile-less conversion,
--     'cmykToRGB': a multilinear blend of the sixteen corner colours of the
--     CMYK hypercube, measured from a press. It is the one conversion for
--     everything that ends in DeviceCMYK, vector colours ("Cmedit.PdfRender")
--     and images alike, so a fill and an image of the same CMYK colour agree,
--     and both match @pdftoppm@. (The naive @1 - min(1, C + K)@ turns a dark
--     navy @1 .77 .46 .46@ into black.)
--   * ICCBased spaces are their @\/Alternate@ (or the device space with as
--     many components): profiles are not interpreted.
--   * CalGray, CalRGB and Lab are converted properly, through CIE XYZ with a
--     Bradford adaptation from their white point to D65 and the sRGB transfer
--     curve, as poppler does (measured: within a few code values of
--     @pdfimages@). Lab drawn as a device space would mean nothing at all,
--     and a CalGray with the default gamma of 1 drawn as DeviceGray is far
--     too dark.
--   * Indexed looks up its palette; Separation and DeviceN run their tint
--     transform ("Cmedit.PdfFunc") into the alternate space; the colorant
--     @All@ is no exception (every viewer draws it that way), while @None@
--     is never painted ('csInvisible').
--
-- __Images__ ('decodePdfImage', 'decodeInlineImage') come back as straight
-- RGBA ('PdfColorImage') or as a coverage stencil ('PdfStencil', for
-- @\/ImageMask@), in image space: row 0 is the image's first row (the top
-- of the unit square once the renderer applies PDF's image matrix). The
-- pipeline is: the stream's filters up to its codec (PdfObj), the codec
-- (raw samples, DCT through "Cmedit.Image"'s JPEG decoder including CMYK and
-- YCCK, fax through "Cmedit.Ccitt" as an ordinary filter), sample unpacking
-- at 1, 2, 4, 8 or 16 bits with rows padded to bytes, @\/Decode@, the colour
-- space, then alpha: a soft mask (@\/SMask@, with @\/Matte@ un-premultiplied),
-- an explicit stencil @\/Mask@ (which may be a different size: both are
-- resampled to the larger, nearest-neighbour), or a colour-key @\/Mask@ range.
--
-- The common cases are table lookups, not per-pixel colour conversion: a
-- one-component image of any space is a lookup into a table over its sample
-- values, DeviceRGB is a per-component table, DeviceCMYK a table of the
-- hypercube's corners folded over each K value (then a trilinear blend per
-- pixel), and anything
-- else (Lab, DeviceN, CalRGB) is memoised on the sample tuple.
--
-- __Failure is typed__ ('ImageFailure'): a JPEG 2000 or JBIG2 image is
-- 'ImgUnsupported' (the renderer draws a labelled placeholder and notes it),
-- anything else that cannot be drawn is 'ImgCorrupt'. Short data is not a
-- failure: missing samples read as zero, as every viewer draws them. Sizes
-- are bounded by "Cmedit.Image"'s 'maxPixels' and decoding by the document's
-- 'Fuel'.
module Cmedit.PdfImage
  ( -- * Colour spaces
    ColorSpace(..)
  , parseColorSpace
  , parseColorSpaceObj
  , csComponents
  , csInvisible
  , defaultDecode
  , initialColor
  , toRGB
  , toColor
  , cmykToRGB
    -- * Images
  , PdfImg(..)
  , ImageFailure(..)
  , decodePdfImage
  , decodeInlineImage
  , expandInlineDict
  , maxImageDim
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, newArray, runSTUArray)
import Data.Array.Unboxed (UArray, listArray, bounds)
import qualified Data.Array.Unboxed as U
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Array.MArray (thaw, freeze)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Word (Word8)

import Cmedit.Image (Image(..), JpegSamples(..), decodeJPEGSamples, maxPixels)
import Cmedit.PdfFunc (PdfFunction, parseFunction, evalFunction)
import Cmedit.PdfObj
import Cmedit.Scene (Color(..), Mask(..))

------------------------------------------------------------------------------
-- Colour spaces

data ColorSpace
  = CSGray
  | CSRGB
  | CSCMYK
  | CSCalGray !Double                         -- ^ Gamma (white point does not change a neutral).
  | CSCalRGB !(Double, Double, Double) [Double] !(Double, Double, Double)
      -- ^ White point, @\/Matrix@ (9 numbers, column-major as PDF writes it), gammas.
  | CSLab !(Double, Double, Double) !(Double, Double, Double, Double)
      -- ^ White point and the @a*@/@b*@ range @(amin, amax, bmin, bmax)@.
  | CSICC !Int [(Double, Double)] ColorSpace
      -- ^ Component count, @\/Range@, and the space it is drawn as.
  | CSIndexed ColorSpace !Int !(UArray Int Word8)
      -- ^ Base space, @hival@, and the palette (@(hival + 1) × n@ bytes).
  | CSSeparation !BS.ByteString ColorSpace PdfFunction
  | CSDeviceN [BS.ByteString] ColorSpace PdfFunction
  | CSNone !Int                                -- ^ A @None@ separation or all-@None@ DeviceN: never painted.
  | CSPattern (Maybe ColorSpace)               -- ^ Pattern, with the space of an uncoloured pattern's colour.
  deriving (Show)

-- | How many colour operands (or image samples per pixel) a space takes.
csComponents :: ColorSpace -> Int
csComponents cs = case cs of
  CSGray -> 1
  CSRGB -> 3
  CSCMYK -> 4
  CSCalGray _ -> 1
  CSCalRGB {} -> 3
  CSLab {} -> 3
  CSICC n _ _ -> n
  CSIndexed {} -> 1
  CSSeparation {} -> 1
  CSDeviceN ns _ _ -> length ns
  CSNone n -> n
  CSPattern (Just b) -> csComponents b
  CSPattern Nothing -> 0

-- | Paints nothing at all (a @None@ separation): the renderer skips the
-- object, and an image in it is fully transparent.
csInvisible :: ColorSpace -> Bool
csInvisible (CSNone _) = True
csInvisible _ = False

-- | The default @\/Decode@ for an image with this many bits per component.
defaultDecode :: ColorSpace -> Int -> [(Double, Double)]
defaultDecode cs bpc = case cs of
  CSIndexed {} -> [(0, fromIntegral ((2 :: Int) ^ min 16 (max 1 bpc) - 1))]
  CSLab _ (a0, a1, b0, b1) -> [(0, 100), (a0, a1), (b0, b1)]
  CSICC n rng _ -> take n (rng ++ repeat (0, 1))
  _ -> replicate (csComponents cs) (0, 1)

-- | The colour a @CS@/@cs@ operator selects before any @SC@.
initialColor :: ColorSpace -> [Double]
initialColor cs = case cs of
  CSCMYK -> [0, 0, 0, 1]
  CSLab _ (a0, a1, b0, b1) -> [0, clampD a0 a1 0, clampD b0 b1 0]
  CSICC n rng _ -> [ clampD lo hi 0 | (lo, hi) <- take n (rng ++ repeat (0, 1)) ]
  CSIndexed {} -> [0]
  CSSeparation {} -> [1]
  CSDeviceN ns _ _ -> map (const 1) ns
  CSNone n -> replicate n 1
  CSPattern _ -> []
  _ -> replicate (csComponents cs) 0

-- | A colour in a space, as sRGB components in @[0,1]@. Missing operands
-- read as 0, out-of-range ones are clamped, and a NaN never escapes.
toRGB :: ColorSpace -> [Double] -> (Double, Double, Double)
toRGB cs xs0 = clamp3 $ case cs of
  CSGray -> let g = c 0 in (g, g, g)
  CSRGB -> (c 0, c 1, c 2)
  CSCMYK -> cmykToRGB (c 0) (c 1) (c 2) (c 3)
  CSCalGray gm -> let y = srgbEncode (c 0 ** gm) in (y, y, y)
  CSCalRGB wp m gs -> calRGB wp m gs (c 0, c 1, c 2)
  CSLab wp (a0, a1, b0, b1) -> labToRGB wp (clampD 0 100 (x 0)) (clampD a0 a1 (x 1)) (clampD b0 b1 (x 2))
  CSICC n rng alt -> toRGB alt [ clampD lo hi v | ((lo, hi), v) <- zip (take n (rng ++ repeat (0, 1))) xs ]
  CSIndexed base hival pal ->
    let i = max 0 (min hival (round' (x 0)))
        n = csComponents base
        rngs = defaultDecode base 8
        comp k = let (lo, hi) = rngs !! k
                     b = palByte pal (i * n + k)
                 in lo + fromIntegral b / 255 * (hi - lo)
    in toRGB base (map comp [0 .. n - 1])
  CSSeparation _ alt fn -> toRGB alt (evalFunction fn [c 0])
  CSDeviceN ns alt fn -> toRGB alt (evalFunction fn (map c [0 .. length ns - 1]))
  CSNone _ -> (1, 1, 1)
  CSPattern (Just b) -> toRGB b xs
  CSPattern Nothing -> (0, 0, 0)
  where
    xs = xs0
    x k = case drop k xs of (v : _) | not (isNaN v) -> v; _ -> 0
    c k = clampD 0 1 (x k)
    round' v = if isNaN v then 0 else floor (v + 0.5) :: Int

-- | 'toRGB' as a "Cmedit.Scene" colour with the given alpha.
toColor :: ColorSpace -> [Double] -> Double -> Color
toColor cs xs a = let (r, g, b) = toRGB cs xs in Color r g b (clampD 0 1 a)

-- | DeviceCMYK to sRGB the way xpdf and poppler do without an output
-- profile: a multilinear (four-way linear) blend of the sixteen corner
-- colours of the CMYK hypercube. Components are clamped to @[0,1]@.
cmykToRGB :: Double -> Double -> Double -> Double -> (Double, Double, Double)
cmykToRGB c0 m0 y0 k0 = (cl r, cl g, cl b)
  where
    c = cl c0; m = cl m0; y = cl y0; k = cl k0
    c1 = 1 - c; m1 = 1 - m; y1 = 1 - y; k1 = 1 - k
    terms =
      [ (c1 * m1 * y1 * k1, 1, 1, 1)
      , (c1 * m1 * y1 * k, 0.1373, 0.1216, 0.1255)
      , (c1 * m1 * y * k1, 1, 0.9490, 0)
      , (c1 * m1 * y * k, 0.1098, 0.1020, 0)
      , (c1 * m * y1 * k1, 0.9255, 0, 0.5490)
      , (c1 * m * y1 * k, 0.1412, 0, 0)
      , (c1 * m * y * k1, 0.9294, 0.1098, 0.1412)
      , (c1 * m * y * k, 0.1333, 0, 0)
      , (c * m1 * y1 * k1, 0, 0.6784, 0.9373)
      , (c * m1 * y1 * k, 0, 0.0588, 0.1412)
      , (c * m1 * y * k1, 0, 0.6510, 0.3137)
      , (c * m1 * y * k, 0, 0.0745, 0)
      , (c * m * y1 * k1, 0.1804, 0.1922, 0.5725)
      , (c * m * y1 * k, 0, 0, 0.0078)
      , (c * m * y * k1, 0.2118, 0.2119, 0.2235) ]
    r = sum [ x * v | (x, v, _, _) <- terms ]
    g = sum [ x * v | (x, _, v, _) <- terms ]
    b = sum [ x * v | (x, _, _, v) <- terms ]
    cl v = if isNaN v then 0 else max 0 (min 1 v)

-- The hypercube's corner colours: index @c*8 + m*4 + y*2 + k@ (each 0 or 1),
-- three channels each. 'cmykToRGB' is exactly the multilinear interpolation
-- of these, which is what lets an image evaluate it as nested blends.
cmykCorners :: UArray Int Double
cmykCorners = listArray (0, 47) $ concat
  [ [1, 1, 1], [0.1373, 0.1216, 0.1255], [1, 0.9490, 0], [0.1098, 0.1020, 0]
  , [0.9255, 0, 0.5490], [0.1412, 0, 0], [0.9294, 0.1098, 0.1412], [0.1333, 0, 0]
  , [0, 0.6784, 0.9373], [0, 0.0588, 0.1412], [0, 0.6510, 0.3137], [0, 0.0745, 0]
  , [0.1804, 0.1922, 0.5725], [0, 0, 0.0078], [0.2118, 0.2119, 0.2235], [0, 0, 0] ]

palByte :: UArray Int Word8 -> Int -> Word8
palByte pal i = let (lo, hi) = bounds pal in if i >= lo && i <= hi then pal `unsafeAt` (i - lo) else 0

clampD :: Double -> Double -> Double -> Double
clampD lo hi v
  | isNaN v = lo
  | otherwise = max (min lo hi) (min (max lo hi) v)

clamp3 :: (Double, Double, Double) -> (Double, Double, Double)
clamp3 (r, g, b) = (clampD 0 1 r, clampD 0 1 g, clampD 0 1 b)

-- The sRGB transfer curve, from linear light.
srgbEncode :: Double -> Double
srgbEncode v
  | isNaN v || v <= 0 = 0
  | v <= 0.0031308 = 12.92 * v
  | v >= 1 = 1
  | otherwise = 1.055 * v ** (1 / 2.4) - 0.055

-- CIE XYZ relative to white point @wp@ → sRGB, via a Bradford adaptation to
-- D65.
xyzToRGB :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
xyzToRGB (xw, yw, zw) (x, y, z) =
  let (rw, gw, bw) = brad (xw, yw, zw)
      (rd, gd, bd) = brad (0.95047, 1, 1.08883)
      (r, g, b) = brad (x, y, z)
      sc a b' = if b' == 0 then 1 else a / b'
      (x', y', z') = bradInv (r * sc rd rw, g * sc gd gw, b * sc bd bw)
      lr = 3.2404542 * x' - 1.5371385 * y' - 0.4985314 * z'
      lg = -0.9692660 * x' + 1.8760108 * y' + 0.0415560 * z'
      lb = 0.0556434 * x' - 0.2040259 * y' + 1.0572252 * z'
  in (srgbEncode lr, srgbEncode lg, srgbEncode lb)
  where
    brad (a, b', c) = ( 0.8951 * a + 0.2664 * b' - 0.1614 * c
                      , -0.7502 * a + 1.7135 * b' + 0.0367 * c
                      , 0.0389 * a - 0.0685 * b' + 1.0296 * c )
    bradInv (a, b', c) = ( 0.9869929 * a - 0.1470543 * b' + 0.1599627 * c
                         , 0.4323053 * a + 0.5183603 * b' + 0.0492912 * c
                         , -0.0085287 * a + 0.0400428 * b' + 0.9684867 * c )

labToRGB :: (Double, Double, Double) -> Double -> Double -> Double -> (Double, Double, Double)
labToRGB wp@(xw, yw, zw) l a b =
  let fy = (l + 16) / 116
      fx = fy + a / 500
      fz = fy - b / 200
      g t = if t > 6 / 29 then t * t * t else 3 * (6 / 29) * (6 / 29) * (t - 4 / 29)
  in xyzToRGB wp (xw * g fx, yw * g fy, zw * g fz)

calRGB :: (Double, Double, Double) -> [Double] -> (Double, Double, Double) -> (Double, Double, Double)
       -> (Double, Double, Double)
calRGB wp m (gr, gg, gb) (a, b, c) =
  let la = a ** gr; lb = b ** gg; lc = c ** gb
      mm = if length m == 9 then m else [1, 0, 0, 0, 1, 0, 0, 0, 1]
      at k = mm !! k
      -- PDF's Matrix lists X, Y, Z for A, then for B, then for C.
      x = at 0 * la + at 3 * lb + at 6 * lc
      y = at 1 * la + at 4 * lb + at 7 * lc
      z = at 2 * la + at 5 * lb + at 8 * lc
  in xyzToRGB wp (x, y, z)

-- | A colour space by name or array, looking named ones up in the resource
-- dictionary's @\/ColorSpace@ (and honouring @\/DefaultGray@, @\/DefaultRGB@
-- and @\/DefaultCMYK@ there, which replace the device spaces in that scope).
-- Charged to fuel: an Indexed palette and a tint transform may be streams.
parseColorSpace :: PdfFile -> Fuel -> Dict -> Obj -> (Either String ColorSpace, Fuel)
parseColorSpace f fu res o = parseCS f res IS.empty (0 :: Int) True fu o

-- | 'parseColorSpace' with no resources (an object found somewhere a name
-- cannot appear, e.g. a shading's or a soft mask group's @\/CS@ after it has
-- been resolved).
parseColorSpaceObj :: PdfFile -> Fuel -> Obj -> (Either String ColorSpace, Fuel)
parseColorSpaceObj f fu = parseColorSpace f fu M.empty

parseCS :: PdfFile -> Dict -> IS.IntSet -> Int -> Bool -> Fuel -> Obj -> (Either String ColorSpace, Fuel)
parseCS f res seen depth useDefaults fu o
  | depth > 8 = (Left "colour spaces nested too deeply", fu)
  | Just r <- refOf o, r `IS.member` seen = (Left "a colour space that contains itself", fu)
  | otherwise = case resolve f o of
      OName n -> byName n
      OArr (x : rest) -> case resolve f x of
        OName fam -> family fam rest
        _ -> bad
      OArr [] -> bad
      _ -> bad
  where
    seen' = maybe seen (`IS.insert` seen) (refOf o)
    bad = (Left "not a colour space", fu)
    sub = parseCS f res seen' (depth + 1)
    named = dictOf f (lookD f res "ColorSpace")
    -- A device name, replaced by the scope's default space when it has one
    -- (but not inside that default, which would recurse).
    deviceOr dflt key
      | useDefaults, Just v <- M.lookup (BC.pack key) named =
          case sub False fu v of
            (Right cs, fu') | csComponents cs == csComponents dflt -> (Right cs, fu')
            _ -> (Right dflt, fu)
      | otherwise = (Right dflt, fu)
    byName n = case BC.unpack n of
      "DeviceGray" -> deviceOr CSGray "DefaultGray"
      "DeviceRGB"  -> deviceOr CSRGB "DefaultRGB"
      "DeviceCMYK" -> deviceOr CSCMYK "DefaultCMYK"
      "G"          -> deviceOr CSGray "DefaultGray"
      "RGB"        -> deviceOr CSRGB "DefaultRGB"
      "CMYK"       -> deviceOr CSCMYK "DefaultCMYK"
      "Pattern"    -> (Right (CSPattern Nothing), fu)
      "CalRGB"     -> (Right CSRGB, fu)     -- a bare family name, as some producers write
      "CalGray"    -> (Right CSGray, fu)
      _ -> case M.lookup n named of
        Just v -> sub useDefaults fu v
        Nothing -> (Left ("unknown colour space " ++ BC.unpack (BS.take 40 n)), fu)
    num' d k dflt = case lookD f d k of ONum v -> v; _ -> dflt
    triple d k dflt = case numsOf f (lookD f d k) of
      [a, b, c] -> (a, b, c)
      _ -> dflt
    family fam rest = case BC.unpack fam of
      "DeviceGray" -> byName fam
      "DeviceRGB" -> byName fam
      "DeviceCMYK" -> byName fam
      "G" -> byName fam
      "RGB" -> byName fam
      "CMYK" -> byName fam
      "CalGray" -> let d = arg0 in (Right (CSCalGray (positive (num' d "Gamma" 1))), fu)
      "CalRGB" ->
        let d = arg0
        in (Right (CSCalRGB (triple d "WhitePoint" (0.9505, 1, 1.089))
                            (numsOf f (lookD f d "Matrix"))
                            (let (a, b, c) = triple d "Gamma" (1, 1, 1) in (positive a, positive b, positive c))), fu)
      "Lab" ->
        let d = arg0
            (a0, a1, b0, b1) = case numsOf f (lookD f d "Range") of
              [p, q, r, s] -> (p, q, r, s)
              _ -> (-100, 100, -100, 100)
        in (Right (CSLab (triple d "WhitePoint" (0.9505, 1, 1.089)) (a0, a1, b0, b1)), fu)
      "ICCBased" -> case rest of
        (s : _) ->
          let d = dictOf f s
              n0 = truncate (num' d "N" 0) :: Int
              altFor k = case k of 1 -> CSGray; 3 -> CSRGB; 4 -> CSCMYK; _ -> CSRGB
              (alt, fu') = case M.lookup (BC.pack "Alternate") d of
                Just a -> case sub useDefaults fu a of
                  (Right cs, fu1) | n0 == 0 || csComponents cs == n0 -> (cs, fu1)
                  (_, fu1) -> (altFor n0, fu1)
                Nothing -> (altFor n0, fu)
              n = if n0 `elem` [1, 3, 4] then n0 else csComponents alt
              rng = pairs (numsOf f (lookD f d "Range"))
          in if n0 /= 0 && n0 `notElem` [1, 3, 4] && M.notMember (BC.pack "Alternate") d
               then (Left "ICCBased space with an unusual component count and no alternate", fu')
               else (Right (CSICC n rng alt), fu')
        [] -> bad
      "Indexed" -> indexed rest
      "I" -> indexed rest
      "Separation" -> case rest of
        (nm : alt : fn : _) -> case nameOf (resolve f nm) of
          -- (All is drawn through its tint transform like any other
          -- colorant, as every viewer draws it; only None is special.)
          n | n == BC.pack "None" -> (Right (CSNone 1), fu)
            | otherwise -> withAlt alt fn (CSSeparation n)
        _ -> bad
      "DeviceN" -> case rest of
        (nms : alt : fn : _) ->
          let ns = map (nameOf . resolve f) (arrOf f nms)
          in if null ns then bad
             else if all (== BC.pack "None") ns then (Right (CSNone (length ns)), fu)
             else withAlt alt fn (CSDeviceN ns)
        _ -> bad
      "Pattern" -> case rest of
        (b : _) -> case sub useDefaults fu b of
          (Right cs, fu') -> (Right (CSPattern (Just cs)), fu')
          (Left _, fu') -> (Right (CSPattern Nothing), fu')
        [] -> (Right (CSPattern Nothing), fu)
      other -> (Left ("unsupported colour space family " ++ take 40 other), fu)
      where arg0 = case rest of (d : _) -> dictOf f d; [] -> M.empty
    positive v = if v > 0 && not (isInfinite v) then v else 1
    withAlt alt fn k = case sub useDefaults fu alt of
      (Left e, fu') -> (Left e, fu')
      (Right altCS, fu') -> case parseFunction f fu' fn of
        (Left e, fu'') -> (Left ("tint transform: " ++ e), fu'')
        (Right func, fu'') -> (Right (k altCS func), fu'')
    indexed rest = case rest of
      (b : hv : lk : _) -> case sub useDefaults fu b of
        (Left e, fu') -> (Left e, fu')
        (Right base, fu') ->
          let hival = max 0 (min 255 (intOf f (Just hv)))
              n = csComponents base
              want = (hival + 1) * n
              (bytes, fu'') = case resolve f lk of
                OStr s -> (s, fu')
                OStream sd raw -> case decodeStreamF f fu' sd raw of
                  (Right s, fu3) -> (s, fu3)
                  (Left _, fu3) -> (BS.empty, fu3)
                _ -> (BS.empty, fu')
              pal = listArray (0, max 0 (want - 1))
                      [ if i < BS.length bytes then BS.index bytes i else 0 | i <- [0 .. want - 1] ]
          in if n == 0 then (Left "Indexed space with a pattern base", fu'')
             else (Right (CSIndexed base hival pal), fu'')
      _ -> bad

pairs :: [Double] -> [(Double, Double)]
pairs (a : b : rest) = (a, b) : pairs rest
pairs _ = []

------------------------------------------------------------------------------
-- Images

-- | A decoded image, in image space (row 0 first).
data PdfImg
  = PdfColorImage
      { piImage       :: Image    -- ^ Straight RGBA.
      , piInterpolate :: !Bool    -- ^ @\/Interpolate@.
      , piHasAlpha    :: !Bool    -- ^ Whether any alpha source applied (an opaque image can skip blending).
      }
  | PdfStencil
      { piMask        :: Mask     -- ^ Coverage: 255 where the current fill colour paints.
      , piInterpolate :: !Bool
      }
  deriving (Show)

data ImageFailure
  = ImgUnsupported String   -- ^ A codec this reader does not have ("JPEG 2000", "JBIG2", an unknown filter): draw a placeholder.
  | ImgCorrupt String       -- ^ Anything else that stops the image being drawn.
  deriving (Eq, Show)

-- | Largest width or height accepted (the pixel count is bounded by
-- 'maxPixels' too).
maxImageDim :: Int
maxImageDim = 65536

-- | Decode an image XObject: its dictionary (with references, as the file
-- has it), its raw stream bytes, and the resources it is drawn under (for
-- named colour spaces).
decodePdfImage :: PdfFile -> Fuel -> Dict -> Dict -> BS.ByteString -> (Either ImageFailure PdfImg, Fuel)
decodePdfImage f fu0 res d raw = decodeWith f fu0 res d raw True

-- | Decode an inline image (@BI … ID … EI@, as 'Cmedit.PdfObj.contentTokensInline'
-- returns it): abbreviated keys and names are expanded first.
decodeInlineImage :: PdfFile -> Fuel -> Dict -> Dict -> BS.ByteString -> (Either ImageFailure PdfImg, Fuel)
decodeInlineImage f fu0 res d raw = decodeWith f fu0 res (expandInlineDict d) raw False

-- | An inline image dictionary with its abbreviations (PDF 32000 Tables 92
-- and 93) spelled out: keys, filter names and device colour-space names.
expandInlineDict :: Dict -> Dict
expandInlineDict = M.fromList . map one . M.toList
  where
    one (k, v) = let k' = key k in (k', value k' v)
    key k = case BC.unpack k of
      "BPC" -> BC.pack "BitsPerComponent"
      "CS"  -> BC.pack "ColorSpace"
      "D"   -> BC.pack "Decode"
      "DP"  -> BC.pack "DecodeParms"
      "F"   -> BC.pack "Filter"
      "H"   -> BC.pack "Height"
      "IM"  -> BC.pack "ImageMask"
      "I"   -> BC.pack "Interpolate"
      "W"   -> BC.pack "Width"
      "L"   -> BC.pack "Length"
      _     -> k
    value k v
      | k == BC.pack "Filter" = case v of
          OName n -> OName (canonicalFilter n)
          OArr xs -> OArr [ case x of OName n -> OName (canonicalFilter n); _ -> x | x <- xs ]
          _ -> v
      | k == BC.pack "ColorSpace" = csName v
      | otherwise = v
    csName v = case v of
      OName n -> OName (csAbbrev n)
      OArr (OName fam : rest) -> OArr (OName (csAbbrev fam) : map csName (take 1 rest) ++ drop 1 rest)
      _ -> v
    csAbbrev n = case BC.unpack n of
      "G" -> BC.pack "DeviceGray"
      "RGB" -> BC.pack "DeviceRGB"
      "CMYK" -> BC.pack "DeviceCMYK"
      "I" -> BC.pack "Indexed"
      _ -> n

-- The codecs that end a filter chain, and the names 'decodeStreamPrefix'
-- understands before them.
knownFilters :: [BS.ByteString]
knownFilters = map BC.pack
  [ "FlateDecode", "LZWDecode", "ASCIIHexDecode", "ASCII85Decode", "RunLengthDecode"
  , "CCITTFaxDecode", "Crypt", "DCTDecode", "JPXDecode", "JBIG2Decode" ]

-- The source of an image's samples, after its filters.
data Src
  = SrcPacked !BS.ByteString !Int !Int !Int   -- ^ Bytes, bits per component, components, row stride in bytes.
  | SrcJpeg !(UArray Int Word8)                -- ^ Four bytes per pixel ('JpegSamples').

decodeWith :: PdfFile -> Fuel -> Dict -> Dict -> BS.ByteString -> Bool -> (Either ImageFailure PdfImg, Fuel)
decodeWith f fu0 res d raw allowMasks
  | fuelSpent fu0 = (Left (ImgCorrupt "the document's budget is spent"), fu0)
  | Just bad <- unknownFilter = (Left (ImgUnsupported bad), fu0)
  | w < 1 || h < 1 = (Left (ImgCorrupt "image has no size"), fu0)
  | w > maxImageDim || h > maxImageDim || w * h > maxPixels =
      (Left (ImgCorrupt ("image too large (" ++ show w ++ "x" ++ show h ++ ")")), fu0)
  | otherwise = case decodeStreamPrefix f fu0 d raw of
      (Left e, fu1) -> (Left (ImgCorrupt e), fu1)
      (Right (dat, rest), fu1) ->
        let fu2 = spendTokens (w * h `div` 16 + 1) fu1
        in case rest of
          [] -> sampled fu2 dat
          [(codec, parms)]
            | codec == BC.pack "DCTDecode" -> jpeg fu2 dat parms
            | codec == BC.pack "JPXDecode" -> (Left (ImgUnsupported "JPEG 2000"), fu2)
            | codec == BC.pack "JBIG2Decode" -> (Left (ImgUnsupported "JBIG2"), fu2)
          ((codec, _) : _) -> (Left (ImgUnsupported (BC.unpack codec ++ " followed by more filters")), fu2)
  where
    w = dimOf "Width"
    h = dimOf "Height"
    dimOf k = case lookD f d k of
      ONum v | v >= 1 && v < 1e9 -> truncate v
      _ -> 0
    unknownFilter = case [ nm | (nm, _) <- streamFilters f d, nm `notElem` knownFilters ] of
      (nm : _) -> Just (BC.unpack (BS.take 40 nm))
      [] -> Nothing
    isMask = boolOf f (lookD f d "ImageMask") == Just True
    interp = boolOf f (lookD f d "Interpolate") == Just True
    bpcOf = case lookD f d "BitsPerComponent" of
      ONum v -> truncate v
      _ -> if isMask then 1 else 8
    decodeArr = pairs (numsOf f (lookD f d "Decode"))

    -- Raw samples (possibly via CCITT): a stencil or a colour image.
    sampled fu dat
      | isMask =
          let inv = case decodeArr of ((a, b) : _) -> a > b; _ -> False
          in (Right (PdfStencil (stencilFrom dat inv) interp), fu)
      | bpcOf `notElem` [1, 2, 4, 8, 16] = (Left (ImgCorrupt ("unsupported BitsPerComponent " ++ show bpcOf)), fu)
      | otherwise = case colourSpace fu of
          (Left e, fu') -> (Left (ImgCorrupt e), fu')
          (Right cs, fu') ->
            let n = csComponents cs
                stride = (w * n * bpcOf + 7) `div` 8
            in if n < 1 || n > 32 then (Left (ImgCorrupt "image colour space has no components"), fu')
               else finish fu' cs bpcOf (SrcPacked dat bpcOf n stride)

    jpeg fu dat parms =
      let override = case lookD f parms "ColorTransform" of
            ONum v -> Just (v /= 0)
            _ -> Nothing
      in case decodeJPEGSamples override dat of
        Left e -> (Left (ImgCorrupt ("JPEG: " ++ e)), fu)
        Right js ->
          let jw = jsWidth js; jh = jsHeight js
          in if isMask
               then -- A DCT stencil is legal and bizarre: threshold the samples.
                    let inv = case decodeArr of ((a, b) : _) -> a > b; _ -> False
                        pix = jsData js
                        m = Mask w h (listArray (0, w * h - 1)
                              [ let v = jpegAt pix jw jh x y 0 >= 128 in if v == inv then 255 else 0
                              | y <- [0 .. h - 1], x <- [0 .. w - 1] ])
                    in (Right (PdfStencil m interp), fu)
             else
               let (csE, fu') = colourSpace fu
                   deviceFor k = case k of 1 -> CSGray; 4 -> CSCMYK; _ -> CSRGB
                   cs = case csE of
                     Right c | csComponents c == jsComps js -> c
                     _ -> deviceFor (jsComps js)
               in if jw /= w || jh /= h
                    -- The JPEG is the authority on its own size; a dictionary
                    -- that disagrees is wrong, and the image is drawn as coded.
                    then finishSized fu' cs jw jh (SrcJpeg (jsData js))
                    else finish fu' cs 8 (SrcJpeg (jsData js))

    colourSpace fu = case M.lookup (BC.pack "ColorSpace") d of
      Just o -> parseColorSpace f fu res o
      Nothing -> (Right CSGray, fu)    -- (only JPX may omit it; gray is the safe guess)

    stencilFrom dat inv =
      let stride = (w + 7) `div` 8
      in Mask w h $ runSTUArray $ do
           out <- newArray (0, w * h - 1) 0
           -- A byte (eight pixels) at a time: a 600 dpi page is 34 million.
           let row !y | y >= h = pure ()
                      | otherwise = col y 0 >> row (y + 1)
               col !y !x
                 | x >= w = pure ()
                 | otherwise = do
                     let b = byteOf dat (y * stride + (x `shiftR` 3))
                         -- Decode [0 1]: a 0 sample paints. [1 0] inverts.
                         paints = if inv then b else 255 - b
                         o = y * w + x
                         bitAt k | x + k >= w = pure ()
                                 | (paints `shiftR` (7 - k)) .&. 1 /= 0 = unsafeWrite out (o + k) 255
                                 | otherwise = pure ()
                     when (paints /= 0) $ mapM_ bitAt [0 .. 7]
                     col y (x + 8)
           row 0
           pure out

    finish fu cs bpc src = finishWith fu cs bpc w h src
    finishSized fu cs jw jh src = finishWith fu cs 8 jw jh src

    finishWith fu cs bpc iw ih src =
      let dec = case decodeArr of
                  ds | length ds >= csComponents cs -> take (csComponents cs) ds
                  _ -> defaultDecode cs bpc
          rgba = convertImage cs bpc dec iw ih src
          colourKey = case lookD f d "Mask" of
            OArr ks | allowMasks -> Just (map (truncate . numOf f . Just) ks :: [Int])
            _ -> Nothing
          base = Image iw ih "PDF image" (applyKey colourKey bpc iw ih src rgba)
          invisible = csInvisible cs
      in if invisible
           then (Right (PdfColorImage base { imgPix = U.amap (const 0) (imgPix base) } interp True), fu)
           else if not allowMasks then (Right (PdfColorImage base interp (colourKey /= Nothing)), fu)
           else withAlpha fu base (colourKey /= Nothing)

    -- Soft mask, else explicit stencil mask.
    withAlpha fu img keyed = case lookD f d "SMask" of
      OStream sd sraw -> case decodeWith f fu res sd sraw False of
        (Right (PdfColorImage simg _ _), fu') ->
          let matte = case numsOf f (lookD f sd "Matte") of
                [] -> Nothing
                ms -> Just ms
              csE = fst (colourSpace fu')
              matteRGB = case (matte, csE) of
                (Just ms, Right cs) -> Just (toRGB cs ms)
                (Just ms, Left _) -> Just (toRGB (deviceN (length ms)) ms)
                _ -> Nothing
          in (Right (PdfColorImage (applySoftMask img simg matteRGB) interp True), fu')
        (_, fu') -> (Right (PdfColorImage img interp keyed), fu')
      _ -> case lookD f d "Mask" of
        OStream md mraw -> stencilMask fu img md mraw keyed
        _ -> (Right (PdfColorImage img interp keyed), fu)
    stencilMask fu img md mraw keyed =
      case decodeWith f fu res (M.insert (BC.pack "ImageMask") (OBool True) md) mraw False of
        (Right (PdfStencil m _), fu') -> (Right (PdfColorImage (applyStencil img m) interp True), fu')
        (_, fu') -> (Right (PdfColorImage img interp keyed), fu')
    deviceN k = case k of 1 -> CSGray; 4 -> CSCMYK; _ -> CSRGB

byteOf :: BS.ByteString -> Int -> Int
byteOf bs i = if i >= 0 && i < BS.length bs then fromIntegral (BSU.unsafeIndex bs i) else 0
{-# INLINE byteOf #-}

jpegAt :: UArray Int Word8 -> Int -> Int -> Int -> Int -> Int -> Int
jpegAt pix jw jh x y c
  | x >= jw || y >= jh = 0
  | otherwise = let i = (y * jw + x) * 4 + c
                    (_, hi) = bounds pix
                in if i <= hi then fromIntegral (pix `unsafeAt` i) else 0

-- One sample: pixel (x, y), component c, as an integer 0 .. 2^bpc - 1.
sampleAt :: Src -> Int -> Int -> Int -> Int -> Int -> Int
sampleAt src iw ih x y c = case src of
  SrcJpeg pix -> jpegAt pix iw ih x y c
  SrcPacked bs bpc n stride ->
    let bit = (x * n + c) * bpc
        o = y * stride + (bit `shiftR` 3)
    in case bpc of
         8  -> byteOf bs o
         16 -> byteOf bs o * 256 + byteOf bs (o + 1)
         _  -> (byteOf bs o `shiftR` (8 - (bit .&. 7) - bpc)) .&. ((1 `shiftL` bpc) - 1)
{-# INLINE sampleAt #-}

-- Samples → straight RGBA (alpha 255).
convertImage :: ColorSpace -> Int -> [(Double, Double)] -> Int -> Int -> Src -> UArray Int Word8
convertImage cs bpc dec iw ih src = runSTUArray $ do
  out <- newArray (0, max 0 (iw * ih * 4 - 1)) 255
  let n = csComponents cs
      maxV = (1 `shiftL` bpc) - 1 :: Int
      -- Per component: sample value → decoded operand.
      decTab k = let (lo, hi) = if k < length dec then dec !! k else (0, 1)
                 in listArray (0, maxV) [ lo + fromIntegral s * (hi - lo) / fromIntegral maxV | s <- [0 .. maxV] ]
                      :: UArray Int Double
      tabs = map decTab [0 .. n - 1]
      byte v = fromIntegral (max 0 (min 255 (floor (v * 255 + 0.5) :: Int))) :: Word8
      put o (r, g, b) = do
        unsafeWrite out o (byte r); unsafeWrite out (o + 1) (byte g); unsafeWrite out (o + 2) (byte b)
      pixels body =
        let row !y | y >= ih = pure ()
                   | otherwise = col y 0 >> row (y + 1)
            col !y !x | x >= iw = pure ()
                      | otherwise = body x y ((y * iw + x) * 4) >> col y (x + 1)
        in row 0
      {-# INLINE pixels #-}
      s k x y = sampleAt src iw ih x y k
  -- An ICCBased space drawn as its device alternate takes that space's fast
  -- path (ICC RGB is the commonest image space in modern PDFs).
  let fastCS = case cs of
        CSICC _ rng alt | all (== (0, 1)) rng -> alt
        _ -> cs
  case fastCS of
    _ | n == 1 -> do
          -- Any one-component space: a table over every sample value.
          let t0 = head tabs
              lut = listArray (0, 3 * maxV + 2)
                      (concat [ let (r, g, b) = toRGB cs [t0 `unsafeAt` v] in [byte r, byte g, byte b]
                              | v <- [0 .. maxV] ]) :: UArray Int Word8
          pixels $ \x y o -> do
            let v = min maxV (s 0 x y) * 3
            unsafeWrite out o (lut `unsafeAt` v)
            unsafeWrite out (o + 1) (lut `unsafeAt` (v + 1))
            unsafeWrite out (o + 2) (lut `unsafeAt` (v + 2))
    CSRGB -> do
      let lt k = listArray (0, maxV) [ byte ((tabs !! k) `unsafeAt` v) | v <- [0 .. maxV] ] :: UArray Int Word8
          l0 = lt 0; l1 = lt 1; l2 = lt 2
      pixels $ \x y o -> do
        unsafeWrite out o (l0 `unsafeAt` min maxV (s 0 x y))
        unsafeWrite out (o + 1) (l1 `unsafeAt` min maxV (s 1 x y))
        unsafeWrite out (o + 2) (l2 `unsafeAt` min maxV (s 2 x y))
    CSCMYK -> do
      -- 'cmykToRGB' as nested blends: K is folded into the eight C/M/Y
      -- corners once per K sample value (a table of (maxV+1) × 24), so each
      -- pixel is a trilinear blend, 21 multiply-adds, with no allocation.
      let dk k = listArray (0, maxV) [ clampD 0 1 ((tabs !! k) `unsafeAt` v) | v <- [0 .. maxV] ] :: UArray Int Double
          dc = dk 0; dm = dk 1; dy = dk 2; dkk = dk 3
          kTab = listArray (0, 24 * (maxV + 1) - 1)
                   [ (1 - kv) * cmykCorners `unsafeAt` (j * 6 + ch) + kv * cmykCorners `unsafeAt` (j * 6 + 3 + ch)
                   | v <- [0 .. maxV], let kv = dkk `unsafeAt` v, j <- [0 .. 7], ch <- [0 .. 2] ] :: UArray Int Double
          toByte v = fromIntegral (max 0 (min 255 (floor (v * 255 + 0.5) :: Int))) :: Word8
      pixels $ \x y o -> do
        let !cv = dc `unsafeAt` min maxV (s 0 x y)
            !mv = dm `unsafeAt` min maxV (s 1 x y)
            !yv = dy `unsafeAt` min maxV (s 2 x y)
            !kb = 24 * min maxV (s 3 x y)
            -- corner j = c*4 + m*2 + y, channel ch
            at j ch = kTab `unsafeAt` (kb + j * 3 + ch)
            chan ch =
              let ly j = at j ch + yv * (at (j + 1) ch - at j ch)
                  lm j = ly j + mv * (ly (j + 2) - ly j)
                  l0 = lm 0
              in l0 + cv * (lm 4 - l0)
            {-# INLINE chan #-}
        unsafeWrite out o (toByte (chan 0))
        unsafeWrite out (o + 1) (toByte (chan 1))
        unsafeWrite out (o + 2) (toByte (chan 2))
    _ -> do
      -- Everything else: memoised on the sample tuple (packed into an Int
      -- when it fits, which it does for up to 3 components at 16 bits or
      -- 7 at 8).
      -- (The entry count is kept beside the map: 'IM.size' walks the whole
      -- map, and asking it per pixel made a megapixel ICC JPEG take minutes.)
      memo <- newSTRef (IM.empty :: IM.IntMap (Double, Double, Double), 0 :: Int)
      let fits = n * bpc <= 60
      pixels $ \x y o -> do
        let ss = [ min maxV (s k x y) | k <- [0 .. n - 1] ]
            conv = toRGB cs [ (tabs !! k) `unsafeAt` v | (k, v) <- zip [0 ..] ss ]
        if fits
          then do
            let key = foldl (\a v -> (a `shiftL` bpc) .|. v) 0 ss
            (m, cnt) <- readSTRef memo
            case IM.lookup key m of
              Just rgb -> put o rgb
              Nothing -> do
                let !rgb = conv
                put o rgb
                when (cnt < 65536) $ writeSTRef memo (IM.insert key rgb m, cnt + 1)
          else put o conv
  pure out

-- Colour-key masking: a pixel whose every raw sample lies in its component's
-- [min, max] pair is transparent.
applyKey :: Maybe [Int] -> Int -> Int -> Int -> Src -> UArray Int Word8 -> UArray Int Word8
applyKey Nothing _ _ _ _ pix = pix
applyKey (Just ks) _ iw ih src pix
  | null rangesK = pix
  | otherwise = runST $ do
      out <- thawU pix
      forM_ [0 .. ih - 1] $ \y -> forM_ [0 .. iw - 1] $ \x ->
        when (and [ let v = sampleAt src iw ih x y k in v >= lo && v <= hi | (k, (lo, hi)) <- zip [0 ..] rangesK ]) $
          unsafeWrite out ((y * iw + x) * 4 + 3) 0
      freezeU out
  where
    rangesK = pairsI ks
    pairsI (a : b : r) = (a, b) : pairsI r
    pairsI _ = []

-- Combine an image with a soft mask's grey (its red channel), resampling
-- both to the larger of the two sizes; @\/Matte@ un-premultiplies.
applySoftMask :: Image -> Image -> Maybe (Double, Double, Double) -> Image
applySoftMask img sm matte =
  let (ow, oh) = outSize (imgW img, imgH img) (imgW sm, imgH sm)
      base = resizeImage ow oh img
      mask = resizeImage ow oh sm
      pix = runSTUArray $ do
        out <- thawU (imgPix base)
        forM_ [0 .. ow * oh - 1] $ \i -> do
          let a = fromIntegral (imgPix mask `unsafeAt` (i * 4)) :: Int
              o = i * 4
          a0 <- unsafeRead out (o + 3)
          case matte of
            Just (mr, mg, mb) | a > 0 -> do
              let un k mv = do
                    v <- unsafeRead out (o + k)
                    let m255 = mv * 255
                        c = m255 + (fromIntegral v - m255) * 255 / fromIntegral a
                    unsafeWrite out (o + k) (fromIntegral (max 0 (min 255 (floor (c + 0.5) :: Int))))
              un 0 mr; un 1 mg; un 2 mb
            _ -> pure ()
          unsafeWrite out (o + 3) (fromIntegral ((fromIntegral a0 * a + 127) `div` 255))
        pure out
  in base { imgPix = pix }

-- Explicit masking: transparent wherever the stencil does not paint.
applyStencil :: Image -> Mask -> Image
applyStencil img m =
  let (ow, oh) = outSize (imgW img, imgH img) (mkW m, mkH m)
      base = resizeImage ow oh img
      cov = resizeMask ow oh m
      pix = runSTUArray $ do
        out <- thawU (imgPix base)
        forM_ [0 .. ow * oh - 1] $ \i ->
          when (cov `unsafeAt` i == 0) $ unsafeWrite out (i * 4 + 3) 0
        pure out
  in base { imgPix = pix }

-- The larger of two sizes, falling back to the first when the larger would
-- exceed the pixel limit.
outSize :: (Int, Int) -> (Int, Int) -> (Int, Int)
outSize (w1, h1) (w2, h2) =
  let w = max w1 w2; h = max h1 h2
  in if w * h <= maxPixels then (w, h) else (w1, h1)

resizeImage :: Int -> Int -> Image -> Image
resizeImage ow oh img
  | ow == iw && oh == ih = img
  | otherwise = img { imgW = ow, imgH = oh, imgPix = runSTUArray $ do
      out <- newArray (0, max 0 (ow * oh * 4 - 1)) 0
      forM_ [0 .. oh - 1] $ \y -> do
        let sy = min (ih - 1) (y * ih `div` oh)
        forM_ [0 .. ow - 1] $ \x -> do
          let sx = min (iw - 1) (x * iw `div` ow)
              si = (sy * iw + sx) * 4
              o = (y * ow + x) * 4
          forM_ [0 .. 3] $ \k -> unsafeWrite out (o + k) (src `at` (si + k))
      pure out }
  where
    iw = max 1 (imgW img); ih = max 1 (imgH img)
    src = imgPix img
    at a i = let (_, hi) = bounds a in if i <= hi then a `unsafeAt` i else 0

resizeMask :: Int -> Int -> Mask -> UArray Int Word8
resizeMask ow oh m
  | ow == mkW m && oh == mkH m = mkPix m
  | otherwise = runSTUArray $ do
      out <- newArray (0, max 0 (ow * oh - 1)) 0
      forM_ [0 .. oh - 1] $ \y -> do
        let sy = min (mh - 1) (y * mh `div` oh)
        forM_ [0 .. ow - 1] $ \x -> do
          let sx = min (mw - 1) (x * mw `div` ow)
              i = sy * mw + sx
              (_, hi) = bounds (mkPix m)
          unsafeWrite out (y * ow + x) (if i <= hi then mkPix m `unsafeAt` i else 0)
      pure out
  where mw = max 1 (mkW m); mh = max 1 (mkH m)

thawU :: UArray Int Word8 -> ST s (STUArray s Int Word8)
thawU = thaw

freezeU :: STUArray s Int Word8 -> ST s (UArray Int Word8)
freezeU = freeze
