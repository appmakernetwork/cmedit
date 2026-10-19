{-# LANGUAGE RankNTypes, MagicHash, UnboxedTuples #-}
-- | Self-contained raster-image support, written from first principles using
-- only GHC boot libraries (no @JuicyPixels@, no @zlib@ — the DEFLATE, LZW,
-- Huffman and IDCT machinery below is all hand-rolled). The editor uses this to
-- offer a read-only image *view* mode (see 'Cmedit.Editor' @edImage@), the way
-- CSV files get a table view — handy for glancing at an image over SSH where a
-- real graphical viewer is not available.
--
-- The module decodes BMP, PNM (PPM/PGM/PBM), GIF, PNG, baseline JPEG and WebP
-- (both lossless VP8L and lossy VP8, including the ALPH alpha channel), plus
-- JPEG XL via "Cmedit.Jxl", into a
-- common RGBA 'Image', then 'renderImage' scales that to fit a character grid
-- and paints it with Unicode half-block glyphs (two vertical pixels per cell,
-- 24-bit colour) or an ASCII luminance ramp.
--
-- It depends only on "Cmedit.Types" (for 'Cell'/'Color'/'Style'); nothing here
-- touches IO or the rest of the editor, so it adds no cost unless an image is
-- actually opened.
module Cmedit.Image
  ( Image(..)
  , ImgMode(..)
  , decodeImage
  , decodeFrames
  , decodeGIFFrames
  , sniffImage
  , renderImage
  , viewFit
  , scaleRGBA
  ) where

import Control.Monad (when, unless, forM, forM_, foldM)
import Cmedit.Jxl (isJxl, decodeJxl)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, listArray)
import qualified Data.Array as A
import Data.Array.ST (STArray, STUArray, newArray, readArray, writeArray, runSTUArray)
import Data.Array.MArray (freeze, thaw)
import Data.Array.Base (STUArray(..), unsafeRead, unsafeWrite, unsafeAt)
import GHC.Exts (Int(I#), shrinkMutableByteArray#, copyMutableByteArray#)
import GHC.ST (ST(..))
import Data.Array.Unsafe (castSTUArray, unsafeFreeze)
import Data.Array.Unboxed (UArray, (!), bounds)
import qualified Data.Array.Unboxed as U
import Data.Bits
import Data.Char (ord)
import Data.STRef
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int16)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Cmedit.Inflate (Huff(..), buildHuff, inflateInto, InflateHuff(..), buildInflateHuffN, decodeSymSlow)
import Cmedit.Types (Cell(..), Style(..), Color(..), attrNone)

------------------------------------------------------------------------------
-- Core image type

-- | A decoded image: row-major, top-down, 4 bytes (R,G,B,A) per pixel.
data Image = Image
  { imgW   :: !Int
  , imgH   :: !Int
  , imgFmt :: !String                 -- ^ Human-readable source format, for the status line.
  , imgPix :: !(UArray Int Word8)     -- ^ Length @imgW*imgH*4@, RGBA.
  } deriving (Show)

-- | How to paint an image into the character grid.
data ImgMode = HalfBlock | Ascii
  deriving (Eq, Show)

-- Largest image we will decode, as a guard against pathological dimensions
-- (a 4096x4096 RGBA image is already 64 MiB).
maxDim :: Int
maxDim = 20000

maxPixels :: Int
maxPixels = 40 * 1000 * 1000   -- 40 megapixels

------------------------------------------------------------------------------
-- Construction helpers

-- | Build an image by filling a mutable RGBA buffer. Indices are
-- @(y*w + x)*4 + channel@.
buildImage :: String -> Int -> Int
           -> (forall s. STUArray s Int Word8 -> ST s ())
           -> Image
buildImage fmt w h fill =
  Image w h fmt $ runSTUArray $ do
    a <- newArray (0, max 0 (w * h * 4 - 1)) 0
    fill a
    pure a

checkDims :: Int -> Int -> Either String ()
checkDims w h
  | w <= 0 || h <= 0           = Left "image has zero or negative dimensions"
  | w > maxDim || h > maxDim   = Left ("image too large (" ++ show w ++ "x" ++ show h ++ ")")
  | w * h > maxPixels          = Left "image exceeds the size limit"
  | otherwise                  = Right ()

------------------------------------------------------------------------------
-- Little byte helpers over a ByteString (all O(1) indexing)

at :: BS.ByteString -> Int -> Int
at bs i = fromIntegral (BS.index bs i)

-- Bounds-checked byte read; Nothing past the end.
atM :: BS.ByteString -> Int -> Maybe Int
atM bs i | i >= 0 && i < BS.length bs = Just (fromIntegral (BS.index bs i))
         | otherwise                  = Nothing

le16 :: BS.ByteString -> Int -> Int
le16 bs i = at bs i .|. (at bs (i+1) `shiftL` 8)

le32 :: BS.ByteString -> Int -> Int
le32 bs i = at bs i .|. (at bs (i+1) `shiftL` 8)
        .|. (at bs (i+2) `shiftL` 16) .|. (at bs (i+3) `shiftL` 24)

be16 :: BS.ByteString -> Int -> Int
be16 bs i = (at bs i `shiftL` 8) .|. at bs (i+1)

be32 :: BS.ByteString -> Int -> Int
be32 bs i = (at bs i `shiftL` 24) .|. (at bs (i+1) `shiftL` 16)
        .|. (at bs (i+2) `shiftL` 8) .|. at bs (i+3)

-- Interpret a 4-byte little-endian field as signed 32-bit.
le32s :: BS.ByteString -> Int -> Int
le32s bs i = let v = le32 bs i in if v >= 0x80000000 then v - 0x100000000 else v

------------------------------------------------------------------------------
-- Format detection + dispatch

pngSig :: BS.ByteString
pngSig = BS.pack [137,80,78,71,13,10,26,10]

-- | Does this byte string look like a supported image? Returns the format name.
-- Used by the driver to decide whether to open a file in image-view mode (we
-- sniff magic bytes rather than trusting the extension).
sniffImage :: BS.ByteString -> Maybe String
sniffImage bs
  | BS.length bs < 4                         = Nothing
  | bsTake 8 == pngSig                        = Just "PNG"
  | at bs 0 == 0xFF && at bs 1 == 0xD8        = Just "JPEG"
  | bsTake 4 == BS.pack (map (fromIntegral . ord) "GIF8") = Just "GIF"
  | at bs 0 == 0x42 && at bs 1 == 0x4D        = Just "BMP"
  | BS.length bs >= 12 && bsTake 4 == fourCC "RIFF"
      && BS.take 4 (BS.drop 8 bs) == fourCC "WEBP" = Just "WebP"
  | isJxl bs                                  = Just "JPEG XL"
  | at bs 0 == ord' 'P' && (at bs 1 >= ord' '1' && at bs 1 <= ord' '6') = Just "PNM"
  | otherwise                                 = Nothing
  where bsTake n = BS.take n bs
        ord' = ord

fourCC :: String -> BS.ByteString
fourCC = BS.pack . map (fromIntegral . ord)

-- | Decode a recognised image, or explain why we cannot. (Unrecognised input is
-- a 'Left' too, so the caller can surface a clear up-front error.)
decodeImage :: BS.ByteString -> Either String Image
decodeImage bs = case sniffImage bs of
  Just "PNG"  -> decodePNG bs
  Just "JPEG" -> decodeJPEG bs
  Just "GIF"  -> decodeGIF bs
  Just "BMP"  -> decodeBMP bs
  Just "PNM"  -> decodePNM bs
  Just "WebP" -> decodeWebP bs
  Just "JPEG XL" -> decodeJXL bs
  _           -> Left "unrecognised image format"

-- | Decode a recognised image into its full frame sequence: every frame is a
-- fully-composed canvas-sized RGBA image paired with its display time in
-- milliseconds. Static formats yield a single frame; an animated GIF yields
-- them all (up to the 'maxAnimBytes' memory budget). Never returns an empty
-- list on 'Right'.
decodeFrames :: BS.ByteString -> Either String [(Image, Int)]
decodeFrames bs = case sniffImage bs of
  Just "GIF" -> decodeGIFAnim bs
  _          -> (\img -> [(img, 0)]) <$> decodeImage bs

------------------------------------------------------------------------------
-- JPEG XL (the decoder lives in "Cmedit.Jxl"; this is only the adapter)

decodeJXL :: BS.ByteString -> Either String Image
decodeJXL bs = do
  (w, h, px) <- decodeJxl maxPixels bs
  checkDims w h
  pure (Image w h "JPEG XL" px)

------------------------------------------------------------------------------
-- BMP (Windows bitmap)

decodeBMP :: BS.ByteString -> Either String Image
decodeBMP bs = do
  when' (BS.length bs < 54) "truncated BMP header"
  let dataOff = le32 bs 10
      dibSize = le32 bs 14
      w       = le32s bs 18
      hRaw    = le32s bs 22
      topDown = hRaw < 0
      h       = abs hRaw
      bpp     = le16 bs 28
      comp    = le32 bs 30
  checkDims w h
  unless (comp == 0 || comp == 3) $
    Left ("unsupported BMP compression " ++ show comp)
  unless (bpp `elem` [1,4,8,24,32]) $
    Left ("unsupported BMP bit depth " ++ show bpp)
  -- Palette (for <= 8bpp): entries are BGRA, starts right after the DIB header.
  let palOff   = 14 + dibSize          -- colour table follows the DIB header
      palAt k  = let o = palOff + k*4
                 in if o + 2 < BS.length bs
                      then (at bs (o+2), at bs (o+1), at bs o)   -- R,G,B from BGRA
                      else (0,0,0)
      rowBytes = ((w * bpp + 31) `div` 32) * 4   -- padded to 4 bytes
  when' (dataOff + rowBytes * h > BS.length bs) "truncated BMP pixel data"
  let srcRow y = dataOff + rowBytes * (if topDown then y else h-1-y)
      -- Every index a pixel can hold, resolved once ('palAt', so an index
      -- past the colour table still reads whatever bytes follow it), as
      -- opaque RGBA words: built as bytes and viewed through 'asWords', so
      -- the byte order is the canvas's, and a pixel is one read and one write.
      -- (The word view keeps the byte bounds; it is read only below 256.)
      pal = runST $ do
        bytes <- newBytes (4*256)
        forLoop 0 255 $ \k -> do
          let (r,g,b) = palAt k
          unsafeWrite bytes (k*4)     (fromIntegral r)
          unsafeWrite bytes (k*4 + 1) (fromIntegral g)
          unsafeWrite bytes (k*4 + 2) (fromIntegral b)
          unsafeWrite bytes (k*4 + 3) 255
        asWords bytes >>= unsafeFreeze
        :: UArray Int Word32
      byteAt i = BSU.unsafeIndex bs i
  -- The raster's extent was checked above, so each row's reads are in range.
  pure $ buildImage "BMP" w h $ \a -> do
   aw <- asWords a
   forLoop 0 (h-1) $ \y -> do
      let base = srcRow y
          oRow = y*w*4
          putPal x idx = unsafeWrite aw (y*w + x) (pal `unsafeAt` idx)
          putBGR x i = do
            let o = oRow + x*4
            unsafeWrite a o       (byteAt (i + 2))
            unsafeWrite a (o + 1) (byteAt (i + 1))
            unsafeWrite a (o + 2) (byteAt i)
            unsafeWrite a (o + 3) 255
      case bpp of
        24 -> forLoop 0 (w-1) $ \x -> putBGR x (base + x*3)
        32 -> forLoop 0 (w-1) $ \x -> putBGR x (base + x*4)
        8  -> forLoop 0 (w-1) $ \x -> putPal x (fromIntegral (byteAt (base + x)))
        4  -> forLoop 0 (w-1) $ \x -> do
                let byte = fromIntegral (byteAt (base + (x `shiftR` 1))) :: Int
                putPal x (if even x then byte `shiftR` 4 else byte .&. 0x0F)
        1  -> forLoop 0 (w-1) $ \x -> do
                let byte = fromIntegral (byteAt (base + (x `shiftR` 3))) :: Int
                putPal x ((byte `shiftR` (7 - (x .&. 7))) .&. 1)
        _  -> forLoop 0 (w-1) $ \x -> do
                let o = oRow + x*4
                unsafeWrite a o 0; unsafeWrite a (o+1) 0; unsafeWrite a (o+2) 0
                unsafeWrite a (o+3) 255

-- Guarded index helpers shared by decoders.
putRGBA :: STUArray s Int Word8 -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s ()
putRGBA a w x y r g b al = do
  let i = (y * w + x) * 4
  writeArray a i     (fromIntegral r)
  writeArray a (i+1) (fromIntegral g)
  writeArray a (i+2) (fromIntegral b)
  writeArray a (i+3) (fromIntegral al)

when' :: Bool -> String -> Either String ()
when' c msg = if c then Left msg else Right ()

------------------------------------------------------------------------------
-- PNM (Netpbm: PBM/PGM/PPM, ASCII or binary)

decodePNM :: BS.ByteString -> Either String Image
decodePNM bs = do
  when' (BS.length bs < 2 || at bs 0 /= ord 'P') "not a PNM file"
  let kind = at bs 1 - ord '0'
  unless (kind >= 1 && kind <= 6) $ Left "unsupported PNM type"
  -- Parse whitespace/comment-separated header tokens after the magic.
  let needMax = kind `elem` [2,3,5,6]
      nTok    = if needMax then 3 else 2
  (toks, pos) <- readTokens bs 2 nTok
  let w = toks !! 0
      h = toks !! 1
      maxv = if needMax then toks !! 2 else 1
  checkDims w h
  unless (maxv >= 1 && maxv <= 65535) $ Left "bad PNM maxval"
  case kind of
    6 -> binaryPPM bs pos w h maxv False
    5 -> binaryPPM bs pos w h maxv True
    4 -> binaryPBM bs pos w h
    3 -> asciiPNM bs pos w h maxv False
    2 -> asciiPNM bs pos w h maxv True
    1 -> asciiPBM bs pos w h
    _ -> Left "unsupported PNM type"

-- Read @n@ whitespace-separated decimal tokens starting at @i@, skipping
-- @#@ comments. Returns the tokens and the index just past the final token's
-- single trailing whitespace byte (the start of binary raster data).
readTokens :: BS.ByteString -> Int -> Int -> Either String ([Int], Int)
readTokens bs i0 n = go i0 n []
  where
    len = BS.length bs
    go i 0 acc = Right (reverse acc, i)          -- i already points past last delim
    go i k acc =
      let i1 = skipWsC i
      in if i1 >= len then Left "truncated PNM header"
         else let (v, i2) = readNum i1 0
              in if not (hadDigit i1 i2) then Left "malformed PNM header"
                 else go (i2 + 1) (k-1) (v : acc)  -- consume exactly one trailing delim
    hadDigit a b = b > a
    skipWsC i
      | i >= len = i
      | c == ord '#' = skipWsC (skipLine i)
      | isWs c       = skipWsC (i+1)
      | otherwise    = i
      where c = at bs i
    skipLine i | i >= len = i
               | at bs i == 10 = i+1
               | otherwise = skipLine (i+1)
    readNum i acc
      | i < len && isDig (at bs i) = readNum (i+1) (acc*10 + (at bs i - ord '0'))
      | otherwise = (acc, i)
    isDig c = c >= ord '0' && c <= ord '9'
    isWs c = c == 32 || c == 9 || c == 10 || c == 13 || c == 11 || c == 12

-- Samples are scaled from maxval to 255 as v * 255 `div` maxval, truncated
-- to a byte (maxval >= 1 is checked by the caller). Off the 8-bit/255 fast
-- paths that is a table over every value a sample can hold (256 entries, or
-- 65536 for 16-bit samples; a corrupt sample can exceed maxval), holding the
-- very bytes the division gave: a division per sample was most of the time.
binaryPPM :: BS.ByteString -> Int -> Int -> Int -> Int -> Bool
          -> Either String Image
binaryPPM bs pos w h maxv gray = do
  let bytesPer = if maxv > 255 then 2 else 1
      scaleTab = runSTUArray $ do
        t <- newArray (0, 256^bytesPer - 1) 0
        forLoop 0 (256^bytesPer - 1) $ \v ->
          unsafeWrite t v (fromIntegral ((v * 255) `div` maxv) :: Word8)
        pure t
      scale v = scaleTab `unsafeAt` v
      chans    = if gray then 1 else 3
      need     = w * h * chans * bytesPer
  when' (pos + need > BS.length bs) "truncated PNM raster"
  -- The raster's extent is checked, so its reads are unchecked.
  let byteAt i = fromIntegral (BSU.unsafeIndex bs i) :: Int
      rd o = if bytesPer == 2 then (byteAt o `shiftL` 8) .|. byteAt (o+1) else byteAt o
  pure $ buildImage "PNM" w h $ \a -> do
    let n = w*h
        wr o v = unsafeWrite a o (v :: Word8)
    if | bytesPer == 1 && maxv == 255 && gray ->
           forLoop 0 (n-1) $ \i -> do
             let v = BSU.unsafeIndex bs (pos + i); o = 4*i
             unsafeWrite a o v; unsafeWrite a (o+1) v; unsafeWrite a (o+2) v
             unsafeWrite a (o+3) 255
       | bytesPer == 1 && maxv == 255 ->
           forLoop 0 (n-1) $ \i -> do
             let s = pos + 3*i; o = 4*i
             unsafeWrite a o       (BSU.unsafeIndex bs s)
             unsafeWrite a (o + 1) (BSU.unsafeIndex bs (s + 1))
             unsafeWrite a (o + 2) (BSU.unsafeIndex bs (s + 2))
             unsafeWrite a (o + 3) 255
       | gray ->
           forLoop 0 (n-1) $ \i -> do
             let v = scale (rd (pos + i*bytesPer)); o = 4*i
             wr o v; wr (o+1) v; wr (o+2) v; wr (o+3) 255
       | otherwise ->
           forLoop 0 (n-1) $ \i -> do
             let s = pos + i*3*bytesPer; o = 4*i
             wr o (scale (rd s))
             wr (o+1) (scale (rd (s + bytesPer)))
             wr (o+2) (scale (rd (s + 2*bytesPer)))
             wr (o+3) 255

binaryPBM :: BS.ByteString -> Int -> Int -> Int -> Either String Image
binaryPBM bs pos w h = do
  let rowBytes = (w + 7) `div` 8
  when' (pos + rowBytes*h > BS.length bs) "truncated PBM raster"
  pure $ buildImage "PNM" w h $ \a -> do
    -- White and black (1 = black in PBM) as RGBA words, built as bytes so
    -- the byte order is the canvas's; then eight pixels per input byte.
    bw <- newBytes 8
    forLoop 0 3 $ \i -> unsafeWrite bw i 255
    unsafeWrite bw 7 255
    pal <- asWords bw
    white <- unsafeRead pal 0
    black <- unsafeRead pal 1
    aw <- asWords a
    forLoop 0 (h-1) $ \y -> do
      let src = pos + y*rowBytes
          o = y*w
          px x byte b = unsafeWrite aw (o + x) (if testBit byte b then black else white)
          full = w `shiftR` 3
      forLoop 0 (full - 1) $ \q -> do
        let byte = BSU.unsafeIndex bs (src + q); x = q*8
        px x byte 7; px (x+1) byte 6; px (x+2) byte 5; px (x+3) byte 4
        px (x+4) byte 3; px (x+5) byte 2; px (x+6) byte 1; px (x+7) byte 0
      forLoop (full*8) (w-1) $ \x ->
        px x (BSU.unsafeIndex bs (src + (x `shiftR` 3))) (7 - (x .&. 7))

-- ASCII PNM (P2/P3, and P1 via 'asciiPBM'): w*h*chans whitespace-separated
-- samples, parsed straight into the image instead of into a list and a boxed
-- array first (a 640x507 P3 held 199 MB that way). The tokenizer is the old
-- one exactly: whitespace and # comments are skipped, a sample is a run of
-- digits, and a stray non-digit yields a 0 sample without advancing; the
-- raster ending early is still an error.
-- The sample scaling is written out here (it is 'decodePNM''s @scale@) rather
-- than passed in: as a function argument it boxed every sample's result.
asciiPNM :: BS.ByteString -> Int -> Int -> Int -> Int -> Bool
         -> Either String Image
asciiPNM bs pos w h maxv gray = asciiRaster bs pos w h (if gray then 1 else 3) $ \_ v ->
  if maxv == 255 then v else if maxv <= 0 then 0 else (v * 255) `div` maxv

asciiPBM :: BS.ByteString -> Int -> Int -> Int -> Either String Image
asciiPBM bs pos w h = asciiRaster bs pos w h 1 $ \_ v -> if v == 1 then 0 else 255

-- Fill an image from ASCII samples, @chans@ per pixel (1 = the sample is
-- written to R, G and B). @val c v@ gives output channel c's byte.
{-# INLINE asciiRaster #-}
asciiRaster :: BS.ByteString -> Int -> Int -> Int -> Int -> (Int -> Int -> Int)
            -> Either String Image
asciiRaster bs pos w h chans val = runST $ do
  a <- newBytes (w*h*4)
  let len = BS.length bs
      byteAt i = fromIntegral (BSU.unsafeIndex bs i) :: Int
      isWs c = c==32||c==9||c==10||c==13||c==11||c==12
      skipLine i | i >= len = i | byteAt i == 10 = i+1 | otherwise = skipLine (i+1)
      skip i | i >= len = i
             | c == 35 = skip (skipLine i)              -- '#'
             | isWs c = skip (i+1)
             | otherwise = i
        where c = byteAt i
      total = w*h*chans
      -- go and digits only call each other in tail position, so both are
      -- join points: a sample costs no allocation.
      go !k !i
        | k >= total = pure True
        | otherwise =
            let i1 = skip i in
            if i1 >= len then pure False else digits k i1 0
      digits !k !i !v
        | i < len, let c = byteAt i, c >= 48 && c <= 57 = digits k (i+1) (v*10 + c - 48)
        | otherwise = do
            let px = k `quot` chans
                o = 4*px
            if chans == 1
              then do
                unsafeWrite a o (fromIntegral (val 0 v))
                unsafeWrite a (o+1) (fromIntegral (val 1 v))
                unsafeWrite a (o+2) (fromIntegral (val 2 v))
                unsafeWrite a (o+3) 255
              else do
                let c = k - px*chans
                unsafeWrite a (o + c) (fromIntegral (val c v))
                when (c == 2) $ unsafeWrite a (o+3) 255
            go (k+1) i
  ok <- go 0 pos
  if ok then Right . Image w h "PNM" <$> unsafeFreeze a
        else pure (Left "truncated ASCII PNM raster")

------------------------------------------------------------------------------
-- Stubs filled in by later slices (kept here so the module type-checks while
-- the harder decoders are written).

------------------------------------------------------------------------------
-- GIF (87a/89a) — LZW-compressed, palette-indexed. Every frame is decoded and
-- composed onto the logical-screen canvas (honouring each frame's
-- sub-rectangle, local palette, transparency and disposal method), so an
-- animation comes out as a list of full-canvas RGBA frames plus per-frame
-- delays. 'decodeGIF' keeps the cheap single-image path (it stops after the
-- first frame).

-- Memory budget for a decoded animation (RGBA bytes across all frames): a
-- longer GIF is truncated to the frames that fit rather than exhausting
-- memory. 256 MiB holds e.g. ~340 full-canvas frames of a 640x480 GIF.
maxAnimBytes :: Int
maxAnimBytes = 256 * 1024 * 1024

decodeGIF :: BS.ByteString -> Either String Image
decodeGIF = fmap (fst . head) . decodeGIFFrames 1

-- All frames within the memory budget.
decodeGIFAnim :: BS.ByteString -> Either String [(Image, Int)]
decodeGIFAnim bs = do
  when' (BS.length bs < 13) "truncated GIF header"
  let w = le16 bs 6; h = le16 bs 8
  checkDims w h
  decodeGIFFrames (max 1 (maxAnimBytes `div` max 1 (w * h * 4))) bs

-- The most recent Graphic Control Extension's fields, as they apply to the
-- frame that follows it.
data GifGce = GifGce
  { gceTrans    :: !(Maybe Int)  -- ^ Transparent palette index.
  , gceDelayMs  :: !Int          -- ^ Display time, clamped ('gifDelayMs').
  , gceDisposal :: !Int          -- ^ 0/1 leave, 2 clear to transparent, 3 restore previous.
  }

gceDefault :: GifGce
gceDefault = GifGce Nothing (gifDelayMs 0) 0

-- Centiseconds -> milliseconds; a zero or tiny delay means 100ms, the
-- convention browsers settled on for GIFs authored with delay 0.
gifDelayMs :: Int -> Int
gifDelayMs cs = let ms = cs * 10 in if ms < 20 then 100 else ms

-- One image descriptor, validated, with the GCE in effect when it appeared.
data GifFrame = GifFrame
  { gfLeft, gfTop, gfW, gfH :: !Int
  , gfInterlace :: !Bool
  , gfPalOff    :: !Int          -- ^ Colour-table offset in the file (local wins over global).
  , gfMinCode   :: !Int
  , gfDataOff   :: !Int          -- ^ Offset of the LZW minimum-code byte's first sub-block.
  , gfGce       :: !GifGce
  }

-- | Decode at most @cap@ frames (the animation entry point computes the cap
-- from the memory budget; 'decodeGIF' passes 1). A malformed *first* frame is
-- an error; damage after at least one good frame truncates the animation
-- instead — a partly-downloaded GIF still shows what it has.
decodeGIFFrames :: Int -> BS.ByteString -> Either String [(Image, Int)]
decodeGIFFrames cap bs = do
  when' (BS.length bs < 13) "truncated GIF header"
  let w = le16 bs 6
      h = le16 bs 8
      packed = at bs 10
      gctFlag = testBit packed 7
      gctSize = 1 `shiftL` ((packed .&. 7) + 1)
      p0 = 13 + (if gctFlag then 3*gctSize else 0)
  checkDims w h
  case walkFrames bs gctFlag cap p0 gceDefault of
    (frames, merr)
      | null frames -> Left (maybe "GIF has no image data" id merr)
      | otherwise   -> Right (composeGifFrames bs w h frames)

-- Walk the top-level block stream collecting frame descriptors; returns the
-- frames found and the error that stopped the walk early, if any.
walkFrames :: BS.ByteString -> Bool -> Int -> Int -> GifGce
           -> ([GifFrame], Maybe String)
walkFrames bs gctFlag cap0 pos0 gce0 = go cap0 pos0 gce0
  where
    go cap pos gce
      | cap <= 0            = ([], Nothing)
      | pos >= BS.length bs = ([], Just "GIF ended before any image")
      | otherwise = case at bs pos of
          0x3B -> ([], Just "GIF has no image data")
          0x21 ->                                   -- extension
            let label = atSafe bs (pos+1)
                -- Bounds-guarded: a GIF cut off inside a GCE truncates the
                -- animation instead of indexing past the end.
                gce' | label == 0xF9 && atSafe bs (pos+2) == 4
                       && pos + 7 <= BS.length bs =
                         let p2 = at bs (pos+3)
                         in GifGce
                              { gceTrans = if testBit p2 0 then Just (at bs (pos+6)) else Nothing
                              , gceDelayMs = gifDelayMs (le16 bs (pos+4))
                              , gceDisposal = (p2 `shiftR` 2) .&. 7
                              }
                     | otherwise = gce
            in go cap (skipSubBlocks bs (pos+2)) gce'
          0x2C -> case frameAt bs gctFlag pos gce of
            Left err        -> ([], Just err)
            Right (f, pos') -> let (fs, merr) = go (cap-1) pos' gceDefault
                               in (f : fs, merr)
          b    -> ([], Just ("unexpected GIF block 0x" ++ show b))

-- Validate one image descriptor at pos; also returns the position just past
-- its pixel data.
frameAt :: BS.ByteString -> Bool -> Int -> GifGce -> Either String (GifFrame, Int)
frameAt bs gctFlag pos gce = do
  let left = le16 bs (pos+1)
      top  = le16 bs (pos+3)
      iw   = le16 bs (pos+5)
      ih   = le16 bs (pos+7)
      ipacked = at bs (pos+9)
      lctFlag  = testBit ipacked 7
      interlace = testBit ipacked 6
      lctSize  = 1 `shiftL` ((ipacked .&. 7) + 1)
      lctOff   = pos + 10
      dataOff0 = pos + 10 + (if lctFlag then 3*lctSize else 0)
      palOff
        | lctFlag   = lctOff                 -- local colour table takes priority
        | gctFlag   = 13
        | otherwise = -1
  when' (pos + 10 > BS.length bs) "truncated GIF image descriptor"
  when' (iw <= 0 || ih <= 0) "GIF image has zero size"
  when' (not lctFlag && not gctFlag) "GIF has no colour table"
  let minCode = atSafe bs dataOff0
  when' (minCode < 1 || minCode > 11) "bad GIF LZW code size"
  pure ( GifFrame { gfLeft = left, gfTop = top, gfW = iw, gfH = ih
                  , gfInterlace = interlace, gfPalOff = palOff
                  , gfMinCode = minCode, gfDataOff = dataOff0
                  , gfGce = gce }
       , skipSubBlocks bs (dataOff0 + 1) )

-- Compose the frame sequence onto a shared canvas. Each displayed frame is
-- the canvas after drawing; the working canvas for the next frame follows the
-- previous frame's disposal method (0/1 leave, 2 clear the frame's rectangle
-- to transparent, 3 restore the canvas from before the frame was drawn).
-- The next frame's base is carried as a canvas plus an optional rectangle to
-- clear, so disposal 2 costs nothing beyond the copy the next frame makes
-- anyway; and the first frame draws onto a fresh transparent canvas rather
-- than copying an empty one.
composeGifFrames :: BS.ByteString -> Int -> Int -> [GifFrame] -> [(Image, Int)]
composeGifFrames bs w h = go Nothing Nothing
  where
    go _ _ [] = []
    go base clr (f:fs) =
      let drawn = drawGifFrame bs w h base clr f
          (base', clr') = case gceDisposal (gfGce f) of
                   2 -> (Just drawn, Just f)
                   3 -> (base, clr)
                   _ -> (Just drawn, Nothing)
      in (Image w h "GIF" drawn, gceDelayMs (gfGce f)) : go base' clr' fs

-- Draw one frame's (possibly offset, possibly interlaced) pixels over a copy
-- of the canvas (with a disposal-2 rectangle cleared first); transparent-index
-- pixels leave the canvas showing through.
drawGifFrame :: BS.ByteString -> Int -> Int -> Maybe (UArray Int Word8) -> Maybe GifFrame
             -> GifFrame -> UArray Int Word8
drawGifFrame bs w h mbase mclear f
  -- An opaque, non-interlaced frame covering the whole canvas replaces every
  -- pixel, so it needs no copy of the canvas below and no index buffer: its
  -- LZW output is written straight into a fresh canvas as colours. Positions
  -- the stream never reaches get palette entry 0, which is what the zeroed
  -- index buffer gave them.
  | gfLeft f == 0 && gfTop f == 0 && gfW f == w && gfH f == h
  , not (gfInterlace f), Nothing <- gceTrans (gfGce f) = runSTUArray $ do
      a <- newBytes (w*h*4)
      pal <- gifPalette bs (gfPalOff f)
      aw <- asWords a
      let (lzwData, _) = collectSubBlocksBS bs (gfDataOff f + 1)
          n = w*h
      done <- lzwDecodeWith (gfMinCode f) lzwData n $ \p k ->
                unsafeRead pal k >>= unsafeWrite aw p
      when (done < n) $ do
        c0 <- unsafeRead pal 0
        forLoop done (n-1) $ \p -> unsafeWrite aw p c0
      pure a
  | otherwise = runSTUArray $ do
  a <- case mbase of
         Nothing   -> newBytes (w*h*4)
         Just base -> thaw base
  case mclear of
    Nothing -> pure ()
    Just c  -> forLoop (max 0 (gfTop c)) (min (h-1) (gfTop c + gfH c - 1)) $ \y ->
                 forLoop (max 0 (gfLeft c)) (min (w-1) (gfLeft c + gfW c - 1)) $ \x -> do
                   let o = (y*w + x)*4
                   unsafeWrite a o 0; unsafeWrite a (o+1) 0
                   unsafeWrite a (o+2) 0; unsafeWrite a (o+3) 0
  let iw = gfW f; ih = gfH f
      (lzwData, _) = collectSubBlocksBS bs (gfDataOff f + 1)
      idxRaw = lzwDecode (gfMinCode f) lzwData (iw*ih)
      -- LZW output rows are in storage order; for an interlaced image that is
      -- the 4-pass sequence, so map each actual raster row to its stored row.
      invRow :: UArray Int Int
      invRow | gfInterlace f = U.array (0, ih-1) (zip (gifInterlaceOrder ih) [0 ..])
             | otherwise     = U.listArray (0, ih-1) [0 .. ih-1]
      palOff = gfPalOff f
      trans = maybe (-1) id (gceTrans (gfGce f))
      x0 = max 0 (gfLeft f); x1 = min (w-1) (gfLeft f + iw - 1)
  pal <- gifPalette bs palOff
  aw  <- asWords a
  forLoop (max 0 (gfTop f)) (min (h-1) (gfTop f + ih - 1)) $ \y -> do
    let iy = y - gfTop f
        d = invRow `unsafeAt` iy
    forLoop x0 x1 $ \x -> do
      let ix = x - gfLeft f
          k = if d < ih then fromIntegral (idxRaw `unsafeAt` (d*iw + ix)) else 0
      when (k /= trans) $
        unsafeRead pal k >>= unsafeWrite aw (y*w + x)
  pure a

-- The colour of every index LZW can produce, as opaque RGBA quads viewed as
-- words like the canvas, so a pixel is one read and one write. Built as
-- bytes and viewed through 'asWords', so the byte order is the canvas's on
-- any platform. As before, an index past the colour table reads whatever file
-- bytes follow it, and one past the end of the file is black.
gifPalette :: BS.ByteString -> Int -> ST s (STUArray s Int Word32)
gifPalette bs palOff = do
  palB <- newBytes (4*4096)
  forLoop 0 4095 $ \k -> do
    let o = palOff + k*3
        ok = palOff >= 0 && o+2 < BS.length bs
        q = 4*k
    unsafeWrite palB q       (if ok then BSU.unsafeIndex bs o else 0)
    unsafeWrite palB (q + 1) (if ok then BSU.unsafeIndex bs (o+1) else 0)
    unsafeWrite palB (q + 2) (if ok then BSU.unsafeIndex bs (o+2) else 0)
    unsafeWrite palB (q + 3) 255
  asWords palB

atSafe :: BS.ByteString -> Int -> Int
atSafe bs i = maybe (-1) id (atM bs i)

-- pos points at the first sub-block length byte; returns the index just past
-- the 0x00 terminator.
skipSubBlocks :: BS.ByteString -> Int -> Int
skipSubBlocks bs = go
  where go p = case atM bs p of
                 Nothing -> BS.length bs
                 Just 0  -> p+1
                 Just n  -> go (p + 1 + n)

-- The raster rows in GIF interlace storage order (4 passes).
gifInterlaceOrder :: Int -> [Int]
gifInterlaceOrder h =
  concat [ [r | r <- [s0, s0+step .. h-1]] | (s0,step) <- [(0,8),(4,8),(2,4),(1,2)] ]

-- Gather GIF sub-blocks into one ByteString of LZW data; returns it and the
-- position past the terminator.
collectSubBlocksBS :: BS.ByteString -> Int -> (BS.ByteString, Int)
collectSubBlocksBS bs = go []
  where go acc p = case atM bs p of
          Nothing -> (BS.concat (reverse acc), BS.length bs)
          Just 0  -> (BS.concat (reverse acc), p+1)
          Just n  -> go (BS.take n (BS.drop (p+1) bs) : acc) (p + 1 + n)

-- GIF LZW decompression to a flat array of palette indices (length == count).
-- Indices are 16-bit: a minimum code size up to 11 is accepted, so a base
-- code can exceed 255.
lzwDecode :: Int -> BS.ByteString -> Int -> UArray Int Word16
lzwDecode minCode dat count = runSTUArray $ do
  out <- newArray (0, max 0 (count-1)) 0 :: ST s (STUArray s Int Word16)
  _ <- lzwDecodeWith minCode dat count (\p v -> unsafeWrite out p (fromIntegral v))
  pure out

-- The decoder proper, handing each index to @sink@ at its output position
-- (always < count); returns how many positions were produced. Inlined into
-- each caller so the sink is a known function there.
--
-- The dictionary records each code's prefix code, last symbol, first symbol
-- and length, so a code's string is written straight into place from its end
-- backwards (no stack) and the first symbol a new entry needs is one read.
-- That is exact for every entry whose chain is intact, which is every entry
-- of a valid stream. A corrupt stream can break one in exactly one way: a
-- code past the next free entry is accepted (as the KwKwK case), and the
-- entry made after it takes that code as its prefix, naming an entry that
-- does not exist yet or is left over from before the last clear. Such an
-- entry is given an impossible length, which every entry built on it
-- inherits, and codes like that (or the stale code itself) are decoded by the
-- exact chain walk the decoder always used ('lzwSlowEmit').
{-# INLINE lzwDecodeWith #-}
lzwDecodeWith :: Int -> BS.ByteString -> Int -> (Int -> Int -> ST s ()) -> ST s Int
lzwDecodeWith minCode dat count sink = do
  prefix <- newArray (0, 4095) (-1) :: ST s (STUArray s Int Int)
  suffix <- newArray (0, 4095) 0    :: ST s (STUArray s Int Int)
  first  <- newArray (0, 4095) 0    :: ST s (STUArray s Int Int)
  len    <- newArray (0, 4095) 1    :: ST s (STUArray s Int Int)
  stack  <- newArray (0, 4096) 0    :: ST s (STUArray s Int Int)
  root   <- newArray (0, 0) 0       :: ST s (STUArray s Int Int)
  let clear = 1 `shiftL` minCode
      end   = clear + 1
      dlen  = BS.length dat
      suspect = 100000 :: Int       -- longer than any real chain
  forLoop 0 (clear-1) $ \c -> do   -- base codes are literals
    unsafeWrite suffix c c
    unsafeWrite first c c
  let nbits = 8 * dlen
      byteAt i = if i < dlen then fromIntegral (BSU.unsafeIndex dat i) else 0 :: Int
      readCode bitpos size =
        let byteOff = bitpos `shiftR` 3
            raw | byteOff + 2 < dlen =
                    fromIntegral (BSU.unsafeIndex dat byteOff)
                    .|. (fromIntegral (BSU.unsafeIndex dat (byteOff+1)) `shiftL` 8)
                    .|. (fromIntegral (BSU.unsafeIndex dat (byteOff+2)) `shiftL` 16)
                | otherwise = byteAt byteOff .|. (byteAt (byteOff+1) `shiftL` 8)
                                             .|. (byteAt (byteOff+2) `shiftL` 16)
        in (raw `shiftR` (bitpos .&. 7)) .&. ((1 `shiftL` size) - 1)
      -- Write an intact code's string at outPos (dropping what falls past
      -- count) and return the next position.
      emit c l outPos = do
        let walk !j !cc
              | j < 0 = pure ()
              | otherwise = do
                  when (outPos + j < count) $ unsafeRead suffix cc >>= sink (outPos + j)
                  unsafeRead prefix cc >>= walk (j - 1)
        walk (l - 1) c
        pure (outPos + l)
  let loop !bitpos !size !next !prev !outPos
        | outPos >= count = pure count
        | bitpos + size > nbits = pure outPos
        | otherwise = do
            let code = readCode bitpos size
                bp'  = bitpos + size
            if code == clear
              then loop bp' (minCode+1) (clear+2) (-1) outPos
            else if code == end
              then pure (min count outPos)
            else if prev < 0
              then do
                -- first code: must be a base entry
                when (outPos < count) $ sink outPos code
                loop bp' size next code (outPos+1)
            else do
              -- The string to emit (code's, or prev's for the special case)
              -- through the fast walk when its entry is intact, else the
              -- exact chain walk; -1 back means a cyclic chain: stop there.
              let special = code >= next
                  src = if special then prev else code
              l <- unsafeRead len src
              let intact = src < next && l <= 4096
              p1 <- if intact then emit src l outPos
                              else lzwSlowEmit prefix suffix stack root count sink src outPos
              k <- if intact then unsafeRead first src else unsafeRead root 0
              if p1 < 0 then pure (min count outPos) else do
                -- special case: code == next ; entry = seq[prev]+first(prev)
                outPos' <- if special
                             then do when (p1 < count) $ sink p1 k
                                     pure (p1 + 1)
                             else pure p1
                -- add new entry seq[prev] + k
                if next <= 4095
                  then do
                    unsafeWrite prefix next prev
                    unsafeWrite suffix next k
                    if prev >= next
                      then unsafeWrite len next suspect
                      else do unsafeRead len prev >>= unsafeWrite len next . (+ 1)
                              unsafeRead first prev >>= unsafeWrite first next
                    let next' = next+1
                        size' = if next' == (1 `shiftL` size) && size < 12 then size+1 else size
                    loop bp' size' next' code outPos'
                  else loop bp' size next code outPos'
  loop 0 (minCode+1) (clear+2) (-1) 0

-- The original stack-based chain walk, for chains the fast walk found
-- inconsistent (only a corrupt stream makes one); out of line, as it is
-- never on the hot path. A chain longer than the dictionary can only be
-- cyclic; the old walk then indexed past its stack (or, finding the first
-- symbol, spun forever), and an exception escaping a pure decoder takes the
-- editor down with it. It now ends the stream there instead (-1), keeping
-- what was decoded.
{-# NOINLINE lzwSlowEmit #-}
lzwSlowEmit :: STUArray s Int Int -> STUArray s Int Int -> STUArray s Int Int
            -> STUArray s Int Int -> Int -> (Int -> Int -> ST s ()) -> Int -> Int
            -> ST s Int
lzwSlowEmit prefix suffix stack root count sink c outPos = do
  let push !depth cc
        | cc < 0 = pure depth
        | depth > 4096 = pure (-1)
        | otherwise = do
            fb <- unsafeRead suffix cc
            unsafeWrite stack depth fb
            pr <- unsafeRead prefix cc
            push (depth+1) pr
  d <- push 0 c
  if d < 0 then pure (-1) else do
    let popOut !j !p
          | j < 0 = pure p
          | otherwise = do
              v <- unsafeRead stack j
              when (p < count) $ sink p v
              popOut (j-1) (p+1)
    p' <- popOut (d-1) outPos
    unsafeRead stack (d-1) >>= unsafeWrite root 0
    pure p'

------------------------------------------------------------------------------
-- PNG — chunked, zlib/DEFLATE-compressed, scanline-filtered.

decodePNG :: BS.ByteString -> Either String Image
decodePNG bs = do
  when' (BS.length bs < 8 || BS.take 8 bs /= pngSig) "not a PNG file"
  (ihdr, plte, trns, idat) <- pngChunks bs 8 Nothing BS.empty BS.empty []
  let (w,h,bd,ct,interlace) = ihdr
  checkDims w h
  chans <- pngChannels ct
  unless (validDepth ct bd) $
    Left ("unsupported PNG bit depth " ++ show bd ++ " for colour type " ++ show ct)
  when (ct == 3 && BS.null plte) $ Left "PNG palette image without PLTE"
  when' (BS.null idat) "PNG has no image data"
  let bitsPP   = chans * bd
      bpp      = max 1 ((bitsPP + 7) `div` 8)
      rowBytes pw = (bitsPP * pw + 7) `div` 8
      passes   = if interlace == 1 then adam7Passes w h
                                    else [(0,0,1,1,w,h)]
      rawSize  = sum [ ph * (1 + rowBytes pw)
                     | (_,_,_,_,pw,ph) <- passes, pw > 0, ph > 0 ]
  -- Validate the zlib header (CM=8 deflate, FCHECK), then inflate.
  let zlibOk = BS.length idat >= 2 && (at idat 0 .&. 0x0f) == 8
               && ((at idat 0 * 256 + at idat 1) `mod` 31) == 0
  unless zlibOk $ Left "corrupt PNG (bad zlib header)"
  let -- Eight-bit samples already have the output range; avoid a division
      -- for every colour/alpha component in the common RGB/RGBA path.
      scale v | bd == 8 = v
              | otherwise = v * 255 `div` ((1 `shiftL` bd) - 1)
      palAt i  = let o = i*3 in if o+2 < BS.length plte
                                  then (at plte o, at plte (o+1), at plte (o+2))
                                  else (0,0,0)
      palAlpha i = if i < BS.length trns then at trns i else 255
      grayTrans  = if ct == 0 && BS.length trns >= 2 then Just (be16 trns 0) else Nothing
      rgbTrans   = if ct == 2 && BS.length trns >= 6
                     then Just (be16 trns 0, be16 trns 2, be16 trns 4) else Nothing
      -- The palette as RGBA quads, so a palette pixel is one indexed read
      -- per channel (sub-byte depths index the same table).
      palQuads = U.listArray (0, 1023)
                   [ fromIntegral v | i <- [0 .. 255]
                   , let (r,g,b) = palAt i
                   , v <- [r, g, b, palAlpha i] ] :: UArray Int Word8
      outSize = w*h*4
      -- A non-interlaced image is decoded in place: one buffer of
      -- max outSize rawSize bytes, the stream inflated into its tail, each
      -- row unfiltered into the scratch rows and then converted to its
      -- final position at the front. That never overwrites a row still to
      -- be read. With k = 4w - (1 + rowBytes) (output minus raw bytes per
      -- row), output row y ends at 4w(y+1) and raw row y+1 starts at
      -- bufSize - h(1+rb) + (y+1)(1+rb); the first is no later exactly when
      -- (y+1)k <= hk + (bufSize - outSize), which holds for every y < h:
      -- for k >= 0 with no slack, and for k < 0 (RGBA8 is one byte per row
      -- over; 16-bit layouts more) because the slack is -hk. The buffer is
      -- then shrunk to the image. It saves a raw-sized allocation: 8 MB for
      -- a 1920x1080 RGBA screenshot. Interlaced passes scatter their output
      -- across the image, so they keep a separate raw buffer.
      inPlace = interlace /= 1
      bufSize = if inPlace then max outSize rawSize else outSize
  px <- runST $ do
   a <- newBytes bufSize
   (rawArr, base) <- if inPlace then pure (a, bufSize - rawSize)
                                 else do r <- newBytes rawSize; pure (r, 0)
   ierr <- inflateInto idat 2 rawArr base rawSize   -- skip the 2-byte zlib header
   case ierr of
    Just e -> pure (Left e)
    Nothing -> do
     -- One row converter per image, chosen once: (recon row offset, output
     -- byte offset of the row's first pixel, output stride per pixel, pixels).
     -- The common layouts get their own loops; anything else takes the
     -- general per-pixel path.
     let conv = case (ct, bd) of
           -- RGBA8 rows are already the output's bytes: one block copy,
           -- unless Adam7 scatters the pixels (stride > 4).
           (6, 8) -> \recon ro o0 st n -> if st == 4 then copyBytesU8 recon ro a o0 (4*n) else forLoop 0 (n-1) $ \px -> do
                       let i = ro + 4*px; o = o0 + px*st
                       unsafeRead recon i       >>= unsafeWrite a o
                       unsafeRead recon (i + 1) >>= unsafeWrite a (o + 1)
                       unsafeRead recon (i + 2) >>= unsafeWrite a (o + 2)
                       unsafeRead recon (i + 3) >>= unsafeWrite a (o + 3)
           (2, 8) | Nothing <- rgbTrans ->
                     \recon ro o0 st n -> forLoop 0 (n-1) $ \px -> do
                       let i = ro + 3*px; o = o0 + px*st
                       unsafeRead recon i       >>= unsafeWrite a o
                       unsafeRead recon (i + 1) >>= unsafeWrite a (o + 1)
                       unsafeRead recon (i + 2) >>= unsafeWrite a (o + 2)
                       unsafeWrite a (o + 3) 255
           (0, 8) | Nothing <- grayTrans ->
                     \recon ro o0 st n -> forLoop 0 (n-1) $ \px -> do
                       g <- unsafeRead recon (ro + px)
                       let o = o0 + px*st
                       unsafeWrite a o g; unsafeWrite a (o + 1) g; unsafeWrite a (o + 2) g
                       unsafeWrite a (o + 3) 255
           (4, 8) -> \recon ro o0 st n -> forLoop 0 (n-1) $ \px -> do
                       g <- unsafeRead recon (ro + 2*px)
                       al <- unsafeRead recon (ro + 2*px + 1)
                       let o = o0 + px*st
                       unsafeWrite a o g; unsafeWrite a (o + 1) g; unsafeWrite a (o + 2) g
                       unsafeWrite a (o + 3) al
           (0, 16) | Nothing <- grayTrans ->
                     \recon ro o0 st n -> forLoop 0 (n-1) $ \px -> do
                       g <- s16 recon (ro + 2*px)
                       let o = o0 + px*st
                       unsafeWrite a o g; unsafeWrite a (o + 1) g; unsafeWrite a (o + 2) g
                       unsafeWrite a (o + 3) 255
           (2, 16) | Nothing <- rgbTrans ->
                     \recon ro o0 st n -> forLoop 0 (n-1) $ \px -> do
                       let i = ro + 6*px; o = o0 + px*st
                       s16 recon i       >>= unsafeWrite a o
                       s16 recon (i + 2) >>= unsafeWrite a (o + 1)
                       s16 recon (i + 4) >>= unsafeWrite a (o + 2)
                       unsafeWrite a (o + 3) 255
           (6, 16) -> \recon ro o0 st n -> forLoop 0 (n-1) $ \px -> do
                       let i = ro + 8*px; o = o0 + px*st
                       s16 recon i       >>= unsafeWrite a o
                       s16 recon (i + 2) >>= unsafeWrite a (o + 1)
                       s16 recon (i + 4) >>= unsafeWrite a (o + 2)
                       s16 recon (i + 6) >>= unsafeWrite a (o + 3)
           (4, 16) -> \recon ro o0 st n -> forLoop 0 (n-1) $ \px -> do
                       g <- s16 recon (ro + 4*px)
                       al <- s16 recon (ro + 4*px + 2)
                       let o = o0 + px*st
                       unsafeWrite a o g; unsafeWrite a (o + 1) g; unsafeWrite a (o + 2) g
                       unsafeWrite a (o + 3) al
           (0, _) | bd <= 8 -> \recon ro o0 st n -> forLoop 0 (n-1) $ \px -> do
                       v <- rawSampleU recon ro bd px
                       let g = fromIntegral (scale v)
                           al = if grayTrans == Just v then 0 else 255
                           o = o0 + px*st
                       unsafeWrite a o g; unsafeWrite a (o+1) g; unsafeWrite a (o+2) g
                       unsafeWrite a (o+3) al
           (3, _) -> \recon ro o0 st n -> forLoop 0 (n-1) $ \px -> do
                       idx <- rawSampleU recon ro bd px
                       let q = 4*idx; o = o0 + px*st
                       unsafeWrite a o       (palQuads `unsafeAt` q)
                       unsafeWrite a (o + 1) (palQuads `unsafeAt` (q + 1))
                       unsafeWrite a (o + 2) (palQuads `unsafeAt` (q + 2))
                       unsafeWrite a (o + 3) (palQuads `unsafeAt` (q + 3))
           _ -> \recon ro o0 st n -> forLoop 0 (n-1) $ \px -> do
                  (r,g,b,al) <- pngPixel recon ro bd ct chans scale palAt palAlpha
                                         grayTrans rgbTrans px
                  let o = o0 + px*st
                  unsafeWrite a o       (fromIntegral r)
                  unsafeWrite a (o + 1) (fromIntegral g)
                  unsafeWrite a (o + 2) (fromIntegral b)
                  unsafeWrite a (o + 3) (fromIntegral al)
     _ <- foldM (\off (x0,y0,dx,dy,pw,ph) ->
             if pw <= 0 || ph <= 0 then pure off else do
               let rb = rowBytes pw
               -- Only this row and its predecessor are needed. Start each
               -- Adam7 pass with a zero predecessor, then alternate the slots.
               recon <- newArray (0, 2*rb - 1) 0 :: ST s (STUArray s Int Word8)
               forLoop 0 (ph-1) $ \ry -> do
                 let lineOff = off + ry*(1+rb)
                     rowOff = (ry .&. 1)*rb
                 unfilterPngRow rawArr (base + lineOff) bpp rb recon rowOff (rb-rowOff)
                 -- Convert before reusing the row's scratch slot.
                 conv recon rowOff (((y0 + ry*dy)*w + x0)*4) (4*dx) pw
               pure (off + ph*(1+rb)))
           0 passes
     a' <- if bufSize > outSize then shrinkBytes a outSize else pure a
     Right <$> unsafeFreeze a'
  pure (Image w h "PNG" px)

-- Dispatch once per row and read only the neighbours used by that filter.
-- Each loop is split at bpp: before it there is no left neighbour, so Sub
-- and None copy, Average halves the byte above, and Paeth (whose predictor
-- with left = upper-left = 0 is always the byte above) takes the byte above.
-- Reads of the inflated stream are unchecked: it is exactly as long as the
-- passes' rows, which is what 'inflate' was asked for.
{-# INLINE unfilterPngRow #-}
unfilterPngRow :: STUArray s Int Word8 -> Int -> Int -> Int
               -> STUArray s Int Word8 -> Int -> Int -> ST s ()
unfilterPngRow raw lineOff bpp rb recon rowOff prevOff = do
  ftype <- unsafeRead raw lineOff
  let lead = min bpp rb
      left i = (fromIntegral :: Word8 -> Int) <$> unsafeRead recon (rowOff+i-bpp)
      above i = (fromIntegral :: Word8 -> Int) <$> unsafeRead recon (prevOff+i)
      put i predictor = do
        v <- (fromIntegral :: Word8 -> Int) <$> unsafeRead raw (lineOff+1+i)
        unsafeWrite recon (rowOff+i) (fromIntegral (v + predictor))
  case ftype of
    1 -> do forLoop 0 (lead-1) $ \i -> put i 0
            forLoop lead (rb-1) $ \i -> left i >>= put i
    2 -> forLoop 0 (rb-1) $ \i -> above i >>= put i
    3 -> do forLoop 0 (lead-1) $ \i -> do b <- above i; put i (b `shiftR` 1)
            forLoop lead (rb-1) $ \i -> do
              a <- left i; b <- above i
              put i ((a+b) `shiftR` 1)
    4 -> do forLoop 0 (lead-1) $ \i -> above i >>= put i
            forLoop lead (rb-1) $ \i -> do
              a <- left i; b <- above i; c <- above (i-bpp)
              put i (paethPredict 4 a b c)
    _ -> copyBytesU8 raw (lineOff+1) recon rowOff rb   -- None (and unknown): a copy

-- Convert one pixel (component samples) from an unfiltered scanline to RGBA.
{-# INLINE pngPixel #-}
pngPixel :: STUArray s Int Word8 -> Int -> Int -> Int -> Int
         -> (Int -> Int) -> (Int -> (Int,Int,Int)) -> (Int -> Int)
         -> Maybe Int -> Maybe (Int,Int,Int) -> Int
         -> ST s (Int,Int,Int,Int)
pngPixel recon rowOff bd ct chans scale palAt palAlpha grayTrans rgbTrans px = do
  let samp c = rawSampleST recon rowOff bd chans px c
  case ct of
    0 -> do s <- samp 0
            let g = scale s; al = if grayTrans == Just s then 0 else 255
            pure (g,g,g,al)
    2 -> do r <- samp 0; g <- samp 1; b <- samp 2
            let al = if rgbTrans == Just (r,g,b) then 0 else 255
            pure (scale r, scale g, scale b, al)
    3 -> do i <- samp 0
            let (r,g,b) = palAt i
            pure (r,g,b, palAlpha i)
    4 -> do g <- samp 0; al <- samp 1
            let gg = scale g in pure (gg,gg,gg, scale al)
    6 -> do r <- samp 0; g <- samp 1; b <- samp 2; al <- samp 3
            pure (scale r, scale g, scale b, scale al)
    _ -> pure (0,0,0,255)

-- Raw sample value (unscaled) from a scanline, handling 1/2/4/8/16-bit depths.
{-# INLINE rawSampleST #-}
rawSampleST :: STUArray s Int Word8 -> Int -> Int -> Int -> Int -> Int -> ST s Int
rawSampleST recon rowOff bd chans x c
  | bd == 8  = fromIntegral <$> rd recon (rowOff + x*chans + c)
  | bd == 16 = do hi <- rd recon (rowOff + 2*(x*chans+c))
                  lo <- rd recon (rowOff + 2*(x*chans+c) + 1)
                  pure (fromIntegral hi * 256 + fromIntegral lo)
  | otherwise = do                          -- sub-byte (gray/palette, chans==1)
      let bitIndex = x * bd
          byteI    = rowOff + bitIndex `div` 8
          within   = bitIndex `mod` 8
          shiftAmt = 8 - bd - within
          mask     = (1 `shiftL` bd) - 1
      byte <- rd recon byteI
      pure ((fromIntegral byte `shiftR` shiftAmt) .&. mask)

-- A big-endian 16-bit sample scaled to 8 bits as 'pngPixel' scales it:
-- (hi*256 + lo) * 255 `div` 65535 is exactly hi, less one when lo < hi
-- (checked for all 65536 inputs), so no division is needed.
{-# INLINE s16 #-}
s16 :: STUArray s Int Word8 -> Int -> ST s Word8
s16 recon i = do
  hi <- unsafeRead recon i
  lo <- unsafeRead recon (i + 1)
  pure (if lo < hi then hi - 1 else hi)

-- A one-channel sample (palette index) at depth 1/2/4/8, unchecked: the
-- converters only ask for pixels inside the row.
{-# INLINE rawSampleU #-}
rawSampleU :: STUArray s Int Word8 -> Int -> Int -> Int -> ST s Int
rawSampleU recon rowOff bd x
  | bd == 8 = fromIntegral <$> unsafeRead recon (rowOff + x)
  | otherwise = do
      let bitIndex = x * bd
          shiftAmt = 8 - bd - (bitIndex .&. 7)
      byte <- unsafeRead recon (rowOff + (bitIndex `shiftR` 3))
      pure ((fromIntegral byte `shiftR` shiftAmt) .&. ((1 `shiftL` bd) - 1))

rd :: STUArray s Int Word8 -> Int -> ST s Word8
rd = readArray

paethPredict :: Int -> Int -> Int -> Int -> Int
paethPredict ft a b c = case ft of
  0 -> 0
  1 -> a
  2 -> b
  3 -> (a + b) `div` 2
  -- The textbook form compares |p-a|, |p-b|, |p-c| for p = a+b-c. With
  -- lo/hi the smaller/larger of a and b and t = 3c-a-b, it reduces to two
  -- comparisons (checked equal on all 2^24 byte triples).
  4 -> let lo = min a b; hi = max a b; t = 3*c - a - b
       in if hi <= t then lo else if t <= lo then hi else c
  _ -> 0

pngChannels :: Int -> Either String Int
pngChannels ct = case ct of
  0 -> Right 1; 2 -> Right 3; 3 -> Right 1; 4 -> Right 2; 6 -> Right 4
  _ -> Left ("unsupported PNG colour type " ++ show ct)

validDepth :: Int -> Int -> Bool
validDepth ct bd = case ct of
  0 -> bd `elem` [1,2,4,8,16]
  3 -> bd `elem` [1,2,4,8]
  _ -> bd `elem` [8,16]

-- Adam7 interlace passes: (xStart, yStart, xStep, yStep, passWidth, passHeight).
adam7Passes :: Int -> Int -> [(Int,Int,Int,Int,Int,Int)]
adam7Passes w h =
  [ (x0,y0,dx,dy, ceilDiv (w - x0) dx, ceilDiv (h - y0) dy)
  | (x0,y0,dx,dy) <- [(0,0,8,8),(4,0,8,8),(0,4,4,8),(2,0,4,4),(0,2,2,4),(1,0,2,2),(0,1,1,2)] ]
  where ceilDiv a b = if a <= 0 then 0 else (a + b - 1) `div` b

-- Walk PNG chunks, accumulating IHDR fields, PLTE, tRNS and concatenated IDAT.
pngChunks :: BS.ByteString -> Int
          -> Maybe (Int,Int,Int,Int,Int) -> BS.ByteString -> BS.ByteString -> [BS.ByteString]
          -> Either String ((Int,Int,Int,Int,Int), BS.ByteString, BS.ByteString, BS.ByteString)
pngChunks bs pos mihdr plte trns idats
  | pos + 8 > BS.length bs = finish
  | otherwise =
      let clen  = be32 bs pos
          ctype = BS.take 4 (BS.drop (pos+4) bs)
          dstart = pos + 8
          cdata = BS.take clen (BS.drop dstart bs)
          next  = dstart + clen + 4
      in if dstart + clen > BS.length bs
           then finish
           else case map (chr8) (BS.unpack ctype) of
             "IHDR" -> let w = be32 cdata 0; h = be32 cdata 4
                           bd = at cdata 8; ct = at cdata 9; il = at cdata 12
                       in pngChunks bs next (Just (w,h,bd,ct,il)) plte trns idats
             "PLTE" -> pngChunks bs next mihdr cdata trns idats
             "tRNS" -> pngChunks bs next mihdr plte cdata idats
             "IDAT" -> pngChunks bs next mihdr plte trns (cdata : idats)
             "IEND" -> finishWith
             _      -> pngChunks bs next mihdr plte trns idats
  where
    chr8 = toEnum . fromIntegral :: Word8 -> Char
    finish = finishWith
    finishWith = case mihdr of
      Nothing -> Left "PNG missing IHDR"
      Just ih -> Right (ih, plte, trns, BS.concat (reverse idats))

------------------------------------------------------------------------------
-- DEFLATE lives in "Cmedit.Inflate" (a leaf module), which PNG shares with the
-- PDF view's content streams. 'Huff'/'buildHuff' come from there too: JPEG's
-- scan reader builds the same canonical tables from its own DHT segments.
------------------------------------------------------------------------------
-- JPEG: baseline AND progressive sequential DCT (Huffman). Arithmetic and
-- lossless are rejected. Both modes decode their scan(s) into a per-component
-- DCT-coefficient buffer; the picture is reconstructed (dequantise + IDCT +
-- upsample) only after every scan, so progressive's multi-pass coefficient
-- refinement and baseline's single full scan share one reconstruction path.

data JComp = JComp { jcId :: !Int, jcH :: !Int, jcV :: !Int, jcTq :: !Int }

data JFrame = JFrame { jfW :: !Int, jfH :: !Int, jfComps :: ![JComp] }

emptyHuff :: Huff
emptyHuff = Huff (A.listArray (0,16) (replicate 17 0)) (A.listArray (0,0) [0]) 16

emptyJHuff :: JHuff
emptyJHuff = mkJHuff emptyHuff

decodeJPEG :: BS.ByteString -> Either String Image
decodeJPEG bs = do
  when' (BS.length bs < 2 || at bs 0 /= 0xFF || at bs 1 /= 0xD8) "not a JPEG file"
  fr <- findFrame bs 2
  let w = jfW fr; h = jfH fr; comps = jfComps fr
  checkDims w h
  when' (null comps) "JPEG missing frame header"
  when' (not (length comps `elem` [1,3])) "unsupported JPEG component count"
  Right (decodeJpegImage bs w h comps)

-- Walk markers to the frame (SOF) header, rejecting unsupported variants.
findFrame :: BS.ByteString -> Int -> Either String JFrame
findFrame bs pos
  | pos + 1 >= BS.length bs = Left "JPEG: no frame header"
  | at bs pos /= 0xFF = findFrame bs (pos+1)
  | otherwise =
      let m = at bs (pos+1) in
      if m == 0xFF then findFrame bs (pos+1)
      else if m == 0xD8 then findFrame bs (pos+2)
      else if m == 0xD9 then Left "JPEG ended before a frame"
      else if m >= 0xD0 && m <= 0xD7 then findFrame bs (pos+2)
      else
        let len = be16 bs (pos+2); seg = pos+4; next = pos+2+len in
        -- C0 baseline, C1 extended, C2 progressive: all sequential DCT/Huffman,
        -- decoded by the same coefficient-buffer path.
        if m == 0xC0 || m == 0xC1 || m == 0xC2 then Right (frameAt seg)
        else if m == 0xC3 then Left "lossless JPEG is not supported"
        else if m `elem` [0xC5,0xC6,0xC7,0xC9,0xCA,0xCB,0xCD,0xCE,0xCF]
               then Left "unsupported JPEG SOF variant"
        else findFrame bs next
  where
    frameAt seg =
      let nf = at bs (seg+5)
          comp i = let o = seg+6+3*i; hv = at bs (o+1)
                   in JComp (at bs o) (hv `shiftR` 4) (hv .&. 15) (at bs (o+2))
      in JFrame (be16 bs (seg+3)) (be16 bs (seg+1)) (map comp [0 .. nf-1])

-- Slots of the entropy reader's unboxed state array (see 'decodeJpegImage').
iPos, iAcc, iNb, iMk, iEob, iRst :: Int
iPos = 0   -- byte offset of the next byte to load
iAcc = 1   -- bit accumulator: the low iNb bits are the unread stream, MSB first
iNb  = 2   -- valid bits in the accumulator
iMk  = 3   -- marker that stopped the entropy stream (0 = none)
iEob = 4   -- progressive AC: blocks left in the current end-of-band run
iRst = 5   -- MCUs since the last restart marker

-- The reader keeps up to 64 bits of the entropy stream in an accumulator,
-- unstuffed (0xFF00 is a literal 0xFF) and loaded a byte at a time. It never
-- loads past a marker, and reads beyond one (or beyond EOF) see zero bits:
-- exactly what the old bit-at-a-time reader returned once it hit a marker.
-- Since no marker is ever skipped, iPos stays a valid place to look for the
-- next RST or segment marker ('findRST', 'syncToMarker') even though it can
-- run ahead of the bits consumed.
fillJ :: BS.ByteString -> STUArray s Int Int -> ST s ()
fillJ bs st = do
  mk <- unsafeRead st iMk
  when (mk == 0) $ do
    p0   <- unsafeRead st iPos
    acc0 <- unsafeRead st iAcc
    nb0  <- unsafeRead st iNb
    let len = BS.length bs
        byte i = fromIntegral (BSU.unsafeIndex bs i) :: Int
        done !p !acc !nb !m = do
          unsafeWrite st iPos p; unsafeWrite st iAcc acc; unsafeWrite st iNb nb
          when (m /= 0) $ unsafeWrite st iMk m
        go !p !acc !nb
          | nb > 56   = done p acc nb 0
          | p >= len  = done p acc nb 0xD9
          | otherwise =
              let b = byte p in
              if b /= 0xFF then go (p+1) ((acc `shiftL` 8) .|. b) (nb+8)
              else if p + 1 >= len then done p acc nb 0xD9
              else let n = byte (p+1) in
                   if n == 0 then go (p+2) ((acc `shiftL` 8) .|. 0xFF) (nb+8)
                   else done p acc nb n
    go p0 acc0 nb0

-- The next n (1..16) bits without consuming them, zero-padded at a marker.
{-# INLINE peekJ #-}
peekJ :: BS.ByteString -> STUArray s Int Int -> Int -> ST s Int
peekJ bs st n = do
  nb0 <- unsafeRead st iNb
  when (nb0 < n) $ fillJ bs st
  nb  <- unsafeRead st iNb
  acc <- unsafeRead st iAcc
  let m = (1 `shiftL` n) - 1
  pure (if nb >= n then (acc `shiftR` (nb - n)) .&. m
                   else (acc `shiftL` (n - nb)) .&. m)

{-# INLINE consumeJ #-}
consumeJ :: STUArray s Int Int -> Int -> ST s ()
consumeJ st n = do
  nb <- unsafeRead st iNb
  unsafeWrite st iNb (max 0 (nb - n))

{-# INLINE nextBitJ #-}
nextBitJ :: BS.ByteString -> STUArray s Int Int -> ST s Int
nextBitJ bs st = do v <- peekJ bs st 1; consumeJ st 1; pure v

-- n bits MSB first. Corrupt data can ask for more than 16 (a DC magnitude
-- category is a whole Huffman symbol); those are read in 16-bit pieces,
-- which wraps exactly like the old one-bit-at-a-time shift.
{-# INLINE getBitsJ #-}
getBitsJ :: BS.ByteString -> STUArray s Int Int -> Int -> ST s Int
getBitsJ bs st n
  | n <= 0  = pure 0
  | n <= 16 = do v <- peekJ bs st n; consumeJ st n; pure v
  | otherwise = getBitsLongJ bs st n

getBitsLongJ :: BS.ByteString -> STUArray s Int Int -> Int -> ST s Int
getBitsLongJ bs st = go 0
  where go !acc k | k <= 0 = pure acc
                  | otherwise = do
                      let c = min 16 k
                      v <- peekJ bs st c
                      consumeJ st c
                      go ((acc `shiftL` c) .|. v) (k - c)

{-# INLINE recvJ #-}
recvJ :: BS.ByteString -> STUArray s Int Int -> Int -> ST s Int
recvJ bs st s
  | s == 0 = pure 0
  | otherwise = do v <- getBitsJ bs st s
                   pure (if v < (1 `shiftL` (s-1)) then v - (1 `shiftL` s) + 1 else v)

-- A DHT table plus a 9-bit lookahead: entry @symbolIndex << 4 .|. len@ for
-- every 9-bit prefix whose code is at most 9 bits long, 0 otherwise. Each
-- entry is produced by running the canonical walk itself on that prefix, so
-- even a malformed table decodes exactly as the walk would (a prefix whose
-- walk would index past the symbol list is left to the slow path, which
-- fails the same way the walk did, and only if that code actually occurs).
-- The entry holds the symbol's *index*, not the symbol: a DHT's symbols are
-- read lazily from the file, and on a truncated one, forcing a symbol the
-- stream never uses would fail where the walk did not.
data JHuff = JHuff !Huff !(UArray Int Int)

mkJHuff :: Huff -> JHuff
mkJHuff hf@(Huff counts syms _) = JHuff hf (U.listArray (0, 511) (map entry [0 .. 511]))
  where
    (_, symHi) = A.bounds syms
    entry v = go 1 0 0 0
      where go len code first idx
              | len > 9 = 0
              | otherwise =
                  let b = (v `shiftR` (9 - len)) .&. 1
                      code1 = (code `shiftL` 1) .|. b
                      cnt = counts A.! len
                      k = idx + (code1 - first)
                  in if code1 - first < cnt
                       then (if k >= 0 && k <= symHi then (k `shiftL` 4) .|. len else 0)
                       else go (len+1) code1 ((first+cnt) `shiftL` 1) (idx+cnt)

{-# INLINE decodeHuffJ #-}
decodeHuffJ :: BS.ByteString -> STUArray s Int Int -> JHuff -> ST s Int
decodeHuffJ bs st (JHuff hf@(Huff _ syms _) tbl) = do
  v <- peekJ bs st 9
  let e = tbl `unsafeAt` v
  if e /= 0 then do consumeJ st (e .&. 15); pure (syms A.! (e `shiftR` 4))
            else decodeHuffSlowJ bs st hf

-- The canonical walk over a 16-bit peek, for codes longer than 9 bits. An
-- invalid code consumes 16 bits and decodes as 0, as the walk always did.
decodeHuffSlowJ :: BS.ByteString -> STUArray s Int Int -> Huff -> ST s Int
decodeHuffSlowJ bs st (Huff counts syms _) = do
  v <- peekJ bs st 16
  let go len code first idx
        | len > 16 = (0, 16)
        | otherwise =
            let b = (v `shiftR` (16 - len)) .&. 1
                code1 = (code `shiftL` 1) .|. b
                cnt = counts A.! len
            in if code1 - first < cnt then (syms A.! (idx + (code1 - first)), len)
               else go (len+1) code1 ((first+cnt) `shiftL` 1) (idx+cnt)
      (sym, used) = go 1 0 0 0
  consumeJ st used
  pure sym

-- The natural-order index for each zig-zag position.
zigzag :: UArray Int Int
zigzag = U.listArray (0,63)
  [ 0, 1, 8,16, 9, 2, 3,10,17,24,32,25,18,11, 4, 5
  ,12,19,26,33,40,48,41,34,27,20,13, 6, 7,14,21,28
  ,35,42,49,56,57,50,43,36,29,22,15,23,30,37,44,51
  ,58,59,52,45,38,31,39,46,53,60,61,54,47,55,62,63]

-- IDCT cosine factors: ctK (k*8+u) = C(u)*cos((2k+1)*u*pi/16), as literals.
-- In the unrolled IDCT kernels every index is a compile-time constant, so
-- each factor folds to an immediate (as a table, each was a load through a
-- CAF). The literals are 'show' of the values computed by that formula,
-- which read back as the identical Doubles, so the IDCT output is unchanged.
{-# INLINE ctK #-}
ctK :: Int -> Double
ctK i = case i of
  0 -> 0.7071067811865475
  1 -> 0.9807852804032304
  2 -> 0.9238795325112867
  3 -> 0.8314696123025452
  4 -> 0.7071067811865476
  5 -> 0.5555702330196023
  6 -> 0.38268343236508984
  7 -> 0.19509032201612833
  8 -> 0.7071067811865475
  9 -> 0.8314696123025452
  10 -> 0.38268343236508984
  11 -> -0.1950903220161282
  12 -> -0.7071067811865475
  13 -> -0.9807852804032304
  14 -> -0.9238795325112868
  15 -> -0.5555702330196022
  16 -> 0.7071067811865475
  17 -> 0.5555702330196023
  18 -> -0.3826834323650897
  19 -> -0.9807852804032304
  20 -> -0.7071067811865477
  21 -> 0.1950903220161283
  22 -> 0.9238795325112865
  23 -> 0.8314696123025455
  24 -> 0.7071067811865475
  25 -> 0.19509032201612833
  26 -> -0.9238795325112867
  27 -> -0.5555702330196022
  28 -> 0.7071067811865474
  29 -> 0.8314696123025455
  30 -> -0.3826834323650899
  31 -> -0.9807852804032307
  32 -> 0.7071067811865475
  33 -> -0.1950903220161282
  34 -> -0.9238795325112868
  35 -> 0.5555702330196018
  36 -> 0.7071067811865477
  37 -> -0.8314696123025451
  38 -> -0.38268343236509056
  39 -> 0.9807852804032304
  40 -> 0.7071067811865475
  41 -> -0.555570233019602
  42 -> -0.38268343236509034
  43 -> 0.9807852804032304
  44 -> -0.7071067811865467
  45 -> -0.19509032201612803
  46 -> 0.9238795325112867
  47 -> -0.831469612302545
  48 -> 0.7071067811865475
  49 -> -0.8314696123025453
  50 -> 0.38268343236509
  51 -> 0.19509032201612878
  52 -> -0.7071067811865471
  53 -> 0.9807852804032307
  54 -> -0.9238795325112864
  55 -> 0.5555702330196015
  56 -> 0.7071067811865475
  57 -> -0.9807852804032304
  58 -> 0.9238795325112865
  59 -> -0.8314696123025451
  60 -> 0.7071067811865466
  61 -> -0.5555702330196015
  62 -> 0.38268343236508956
  _ -> -0.19509032201612858

decodeJpegImage :: BS.ByteString -> Int -> Int -> [JComp] -> Image
decodeJpegImage bs w h comps =
  buildImage "JPEG" w h $ \img -> do
    let hmax = maximum (map jcH comps)
        vmax = maximum (map jcV comps)
        mcuW = 8*hmax; mcuH = 8*vmax
        mpr  = (w + mcuW - 1) `div` mcuW       -- MCUs per row / col
        mpc  = (h + mcuH - 1) `div` mcuH
        ncomp = length comps
        -- per component: (blocksPerRow, blocksPerCol) on the MCU-padded grid,
        -- and (blocksWide, blocksHigh) for its true (non-interleaved) extent.
        compInfo = [ (mpr*jcH c, mpc*jcV c, (cw+7)`div`8, (ch+7)`div`8)
                   | c <- comps
                   , let cw = (w*jcH c + hmax-1) `div` hmax
                   , let ch = (h*jcV c + vmax-1) `div` vmax ]
        infoOf ci = compInfo !! ci
        planeDims c = (mpr*jcH c*8, mpc*jcV c*8)
    -- One DCT-coefficient buffer and one sample plane per component.
    -- Int16, not Int: these buffers hold every coefficient of the image at
    -- once (31 MB of Int for a 1280x1014 4:4:4 photo; 7.8 MB as Int16), and
    -- a DCT coefficient fits in 16 bits at every sample precision JPEG
    -- defines (8 and 12 bits), successive-approximation shifts included. Only
    -- a corrupt stream can produce one that does not, and then it wraps:
    -- different garbage from what an Int buffer would show, never a crash.
    coefs  <- mapM (\(bpr,bpc,_,_) -> newCoefs (bpr*bpc*64)) compInfo
    planes <- mapM (\c -> let (pw,ph)=planeDims c
                          in do a <- newArray (0, max 0 (pw*ph-1)) 0 :: ST s (STUArray s Int Word8)
                                pure (pw,ph,a)) comps
    -- Mutable decode tables (DQT/DHT may appear or change between scans).
    quantT <- newArray (0,3) (U.listArray (0,63) (replicate 64 1))
                :: ST s (STArray s Int (UArray Int Int))
    dcT    <- newArray (0,3) emptyJHuff :: ST s (STArray s Int JHuff)
    acT    <- newArray (0,3) emptyJHuff :: ST s (STArray s Int JHuff)
    driRef <- newSTRef 0
    -- Entropy bit reader.
    --
    -- The four hot state slots live in ONE unboxed array rather than four
    -- 'STRef Int's. An STRef holds a boxed value, so every write allocates a
    -- fresh Int on the heap — and this state is written several times per
    -- *bit* of the entropy stream. Measured in isolation, the STRef shape
    -- costs ~110 bytes per iteration (522 MB over 5M iterations) against zero
    -- for the unboxed array; decoding a 603 KiB JPEG churned 830 MB, which at
    -- GHC's allocation rate was most of its 679 ms.
    st <- newArray (0, 5) 0 :: ST s (STUArray s Int Int)
    writeArray st iPos 2
    -- After a scan, advance to the next real marker (skipping the entropy
    -- stream's stuffed 0xFF00 and any RST markers).
    let syncToMarker = do
          p0 <- readArray st iPos
          let go p | p+1 >= BS.length bs = BS.length bs
                   | at bs p == 0xFF && at bs (p+1) /= 0
                     && not (at bs (p+1) >= 0xD0 && at bs (p+1) <= 0xD7) = p
                   | otherwise = go (p+1)
          writeArray st iPos (go p0)
    -- DQT / DHT into the mutable tables.
    let applyDQT seg end = goD seg
          where goD p | p >= end = pure ()
                      | otherwise = do
                          let pqtq = at bs p; pq = pqtq `shiftR` 4; tq = pqtq .&. 15
                              readVal i = if pq == 0 then at bs (p+1+i) else be16 bs (p+1+2*i)
                              tbl = U.array (0,63) [ (zigzag ! i, readVal i) | i <- [0..63] ]
                                      :: UArray Int Int
                          writeArray quantT tq tbl
                          goD (p + 1 + (if pq == 0 then 64 else 128))
        applyDHT seg end = goH seg
          where goH p | p >= end = pure ()
                      | otherwise = do
                          let tcth = at bs p; tc = tcth `shiftR` 4; th = tcth .&. 15
                              counts = [ at bs (p+1+i) | i <- [0..15] ]
                              nsym = sum counts
                              syms = [ at bs (p+17+i) | i <- [0..nsym-1] ]
                              hf = Huff (A.listArray (0,16) (0:counts))
                                        (A.listArray (0, max 0 (nsym-1)) (syms++[0])) 16
                          if tc == 0 then writeArray dcT th (mkJHuff hf)
                                     else writeArray acT th (mkJHuff hf)
                          goH (p+17+nsym)
    -- Decode one scan (its band [Ss,Se] at successive-approximation Ah/Al) into
    -- the coefficient buffers. Handles baseline, DC first/refine and AC
    -- first/refine; the AC routines maintain an EOB-run counter.
    let decodeScan seg next = do
          let ns = at bs seg
              ents = [ (at bs o, at bs (o+1) `shiftR` 4, at bs (o+1) .&. 15)
                     | i <- [0..ns-1], let o = seg+1+2*i ]
              hdr = seg + 1 + 2*ns
              ss = at bs hdr; se = at bs (hdr+1); ahal = at bs (hdr+2)
              ah = ahal `shiftR` 4; al = ahal .&. 15
              scanComps = [ (ci, td, ta) | (cs,td,ta) <- ents
                                         , (ci,c) <- zip [0..] comps, jcId c == cs ]
          writeArray st iPos next
          writeArray st iAcc 0; writeArray st iNb 0; writeArray st iMk 0
          preds  <- newArray (0, max 0 (ncomp-1)) 0 :: ST s (STUArray s Int Int)
          writeArray st iEob 0
          ri <- readSTRef driRef
          let zpos boff k = boff + (zigzag ! k)
              acFirst coef boff actbl k0 = do
                eob <- readArray st iEob
                if eob > 0 then writeArray st iEob (eob-1) else loop k0
                where loop k | k > se = pure ()
                             | otherwise = do
                                 rs <- decodeHuffJ bs st actbl
                                 let r = rs `shiftR` 4; s = rs .&. 15
                                 if s == 0
                                   then if r < 15
                                          then do extra <- getBitsJ bs st r
                                                  writeArray st iEob ((1 `shiftL` r) - 1 + extra)
                                          else loop (k+16)         -- ZRL: 16 zeros
                                   else do let k' = k + r
                                           if k' > se then pure () else do
                                             v <- recvJ bs st s
                                             wrC coef (zpos boff k') (v `shiftL` al)
                                             loop (k'+1)
              -- Successive-approximation AC refinement. Every local function
              -- is called only in tail position (outer -> adv -> place ->
              -- outer, ending in finish), so they compile to join points: the
              -- previous form allocated four STRefs per Huffman symbol.
              acRefine coef boff actbl k0 = do
                let p1 = 1 `shiftL` al
                    m1 = negate (1 `shiftL` al)
                    outer !k
                      | k > se = finish k
                      | otherwise = do
                          rs <- decodeHuffJ bs st actbl
                          let r = rs `shiftR` 4; s = rs .&. 15
                          if s == 0
                            then if r < 15
                                   then do                      -- start an EOB run
                                     extra <- getBitsJ bs st r
                                     writeArray st iEob ((1 `shiftL` r) + extra)
                                     finish k
                                   else adv k r 0               -- ZRL
                            else do b <- nextBitJ bs st         -- one new +-1 coefficient
                                    adv k r (if b /= 0 then p1 else m1)
                    -- Skip r zero-valued coefficients, refining the non-zero
                    -- ones passed on the way.
                    adv !k !rr !nv
                      | k > se = place k nv
                      | otherwise = do
                          v <- rdC coef (zpos boff k)
                          if v /= 0
                            then do b <- nextBitJ bs st
                                    when (b /= 0 && (v .&. p1) == 0) $
                                      wrC coef (zpos boff k) (if v > 0 then v+p1 else v+m1)
                                    adv (k+1) rr nv
                            else if rr == 0 then place k nv
                                 else adv (k+1) (rr-1) nv
                    place !k !nv = do
                      when (nv /= 0 && k <= se) $ wrC coef (zpos boff k) nv
                      outer (k+1)
                    -- Inside an EOB run: refine the rest of the band.
                    finish !k = do
                      eob1 <- readArray st iEob
                      when (eob1 > 0) $ do
                        forLoop k se $ \kk -> do
                          v <- rdC coef (zpos boff kk)
                          when (v /= 0) $ do
                            b <- nextBitJ bs st
                            when (b /= 0 && (v .&. p1) == 0) $
                              wrC coef (zpos boff kk) (if v > 0 then v+p1 else v+m1)
                        writeArray st iEob (eob1 - 1)
                eob0 <- readArray st iEob
                if eob0 == 0 then outer k0 else finish k0
              -- One block of one component, its buffer and tables already
              -- resolved for the scan (see @resolved@ below).
              decodeOneBlock !ci coef !bpr dctbl actbl !bx !by = do
                let boff = (by*bpr + bx)*64
                when (ss == 0) $
                  if ah == 0
                    then do t <- decodeHuffJ bs st dctbl                  -- DC first
                            diff <- recvJ bs st t
                            pv <- readArray preds ci
                            let dc = pv + diff
                            writeArray preds ci dc
                            wrC coef boff (dc `shiftL` al)
                    else do b <- nextBitJ bs st                            -- DC refine
                            when (b /= 0) $ do
                              v <- rdC coef boff
                              wrC coef boff (v .|. (1 `shiftL` al))
                let acStart = max ss 1
                when (acStart <= se) $
                  if ah == 0 then acFirst  coef boff actbl acStart
                             else acRefine coef boff actbl acStart
          writeArray st iRst 0
          let doRestart = do
                writeArray st iAcc 0; writeArray st iNb 0; writeArray st iMk 0
                p <- readArray st iPos; writeArray st iPos (findRST bs p)
                forM_ [0..ncomp-1] $ \i -> writeArray preds i 0
                writeArray st iEob 0
              tick = when (ri > 0) $ do
                n <- readArray st iRst
                if n+1 == ri then doRestart >> writeArray st iRst 0
                else writeArray st iRst (n+1)
          -- Each scan component's coefficient buffer, geometry and Huffman
          -- tables, looked up once per scan rather than once per block.
          resolved <- forM scanComps $ \(ci,td,ta) -> do
            dctbl <- readArray dcT td
            actbl <- readArray acT ta
            let (bpr,_,bw,bh) = infoOf ci
                c = comps !! ci
            pure (ci, coefs !! ci, bpr, bw, bh, jcH c, jcV c, dctbl, actbl)
          -- Interleaved (>1 scan component) iterates MCUs; a single-component
          -- scan iterates that component's blocks in raster order.
          case resolved of
            [(ci, coef, bpr, bw, bh, _, _, dctbl, actbl)] ->
              forLoop 0 (bh-1) $ \by -> forLoop 0 (bw-1) $ \bx -> do
                decodeOneBlock ci coef bpr dctbl actbl bx by
                tick
            _ ->
              forLoop 0 (mpc-1) $ \my -> forLoop 0 (mpr-1) $ \mx -> do
                forM_ resolved $ \(ci, coef, bpr, _, _, ch, cv, dctbl, actbl) ->
                  forLoop 0 (cv-1) $ \by -> forLoop 0 (ch-1) $ \bx ->
                    decodeOneBlock ci coef bpr dctbl actbl (mx*ch+bx) (my*cv+by)
                tick
          syncToMarker
    -- Marker walk: apply tables and decode every scan until EOI.
    let walk = do
          p <- readArray st iPos
          if p + 1 >= BS.length bs then pure ()
          else if at bs p /= 0xFF then writeArray st iPos (p+1) >> walk
          else let m = at bs (p+1) in
            if m == 0xFF then writeArray st iPos (p+1) >> walk
            else if m == 0xD9 then pure ()                            -- EOI
            else if m == 0xD8 || (m >= 0xD0 && m <= 0xD7) then writeArray st iPos (p+2) >> walk
            else do
              let len = be16 bs (p+2); seg = p+4; next = p+2+len
              if      m == 0xDB then applyDQT seg next >> writeArray st iPos next >> walk
              else if m == 0xC4 then applyDHT seg next >> writeArray st iPos next >> walk
              else if m == 0xDD then writeSTRef driRef (be16 bs seg) >> writeArray st iPos next >> walk
              else if m == 0xDA then decodeScan seg next >> walk      -- leaves iPos at the next marker
              else writeArray st iPos next >> walk                     -- APPn / COM / SOF (already parsed)
    walk
    -- Reconstruct: dequantise + IDCT every block into its component plane.
    --
    -- The IDCT is separable: a 2-D transform is a pass over rows followed by a
    -- pass over columns, 2x8x8 multiply-adds per block instead of the 8^4 the
    -- direct double sum needs. The two scratch buffers are allocated once and
    -- reused for every block.
    --
    -- Rows of coefficients that are all zero are skipped in both passes, and
    -- that is exact, not an approximation: such a row's row-pass output is
    -- +0.0 throughout, so its terms in a column sum are all ±0.0, and adding
    -- ±0.0 to an accumulator that starts at +0.0 can never change it (it is
    -- never -0.0: only an exact cancellation reaches zero, and that rounds to
    -- +0.0). Most blocks of a photo have one or two non-zero rows.
    blk <- newArray (0, 63) 0 :: ST s (STUArray s Int Double)   -- dequantised coefficients
    tmp <- newArray (0, 63) 0 :: ST s (STUArray s Int Double)   -- output of the row pass
    forM_ (zip3 [0..] comps compInfo) $ \(ci, c, (bpr,bpc,_,_)) -> do
      let coef = coefs !! ci
          (pw,ph,plane) = planes !! ci
      qtbl <- readArray quantT (jcTq c)
      forLoop 0 (bpc-1) $ \by -> forLoop 0 (bpr-1) $ \bx -> do
        let boff = (by*bpr+bx)*64
            -- Dequantise, collecting a bit per row that has a non-zero.
            deq !k !mask
              | k > 63 = pure mask
              | otherwise = do
                  v <- rdC coef (boff+k)
                  writeArray blk k (fromIntegral (v * (qtbl ! k)))
                  deq (k+1) (if v == 0 then mask
                             else mask .|. (1 `shiftL` (k `shiftR` 3))
                                       .|. (if k > 0 && k < 8 then 0x100 else 0))
        mask <- deq 0 (0 :: Int)
        if mask == 1
          then do
            -- DC only. Every row-pass output is then 0 + ct[0]*dc (the
            -- other terms are ±0.0) and ct[k*8] is the same for every k, so
            -- all 64 samples are one value, computed by the same expressions.
            dc <- readArray blk 0
            let t = 0 + ctK 0 * dc
                sm = 0 + ctK 0 * t
                px = fromIntegral (clamp8 (roundD (0.25 * sm + 128)))
            forLoop 0 7 $ \yy -> do
              let gy = by*8 + yy
              when (gy < ph) $ forLoop 0 7 $ \xx -> do
                let gx = bx*8 + xx
                when (gx < pw) $ unsafeWrite plane (gy*pw+gx) px
          else do
            jIdctRows blk tmp mask
            jIdctCols tmp plane pw ph (bx*8) (by*8)
    -- Upsample (centred bilinear) + YCbCr->RGB into the RGBA image.
    --
    -- Every coordinate a component's samples depend on (the two source
    -- columns and the fraction for each output x, likewise for each output y)
    -- is computed once into a table, by the same expressions in the same
    -- order, so the per-pixel work is four reads and the interpolation
    -- itself. A full-resolution component (luma always) needs no
    -- interpolation at all: it is a single read.
    let tabsFor ((pw,ph,_), c) =
          let ch = jcH c; cv = jcV c
              axis n lim sub tot =
                let f i = (fromIntegral i + 0.5) * fromIntegral sub / fromIntegral tot - 0.5 :: Double
                    i0s = [ clampI 0 (lim-1) (floor (f i)) | i <- [0 .. n-1] ]
                in ( U.listArray (0, n-1) i0s :: UArray Int Int
                   , U.listArray (0, n-1) [ clampI 0 (lim-1) (i0 + 1) | i0 <- i0s ] :: UArray Int Int
                   , U.listArray (0, n-1) [ let fi = f i in fi - fromIntegral (floor fi :: Int)
                                          | i <- [0 .. n-1] ] :: UArray Int Double )
              (xa, xb, xd) = axis w pw ch hmax
              (ya, yb, yd) = axis h ph cv vmax
          in (ch == hmax && cv == vmax, xa, xb, xd, ya, yb, yd)
        compTabs = map tabsFor (zip planes comps)
        tabsOf ci = (planes !! ci, compTabs !! ci)
        ((pw0,ph0,pl0), (f0,xa0,xb0,xd0,ya0,yb0,yd0)) = tabsOf 0
        ((pw1,ph1,pl1), (f1,xa1,xb1,xd1,ya1,yb1,yd1)) = tabsOf (min 1 (ncomp-1))
        ((pw2,ph2,pl2), (f2,xa2,xb2,xd2,ya2,yb2,yd2)) = tabsOf (min 2 (ncomp-1))
    let ycc !o !yf !cbv !crv = do
          let cb = cbv - 128; cr = crv - 128
              r = clamp8 (roundD (yf + 1.402*cr))
              g = clamp8 (roundD (yf - 0.344136*cb - 0.714136*cr))
              b = clamp8 (roundD (yf + 1.772*cb))
          unsafeWrite img o       (fromIntegral r)
          unsafeWrite img (o + 1) (fromIntegral g)
          unsafeWrite img (o + 2) (fromIntegral b)
          unsafeWrite img (o + 3) 255
        {-# INLINE ycc #-}
        -- A full-resolution plane's row offset for output row y, as
        -- 'jSample' computes it (the planes are MCU-padded, so the column
        -- clamp never binds for x < w).
        rowOf ph pw y = min (ph-1) y * pw
    if | ncomp == 1 && f0 ->
           -- A full-resolution gray sample is already a clamped byte, which
           -- the Double round trip would hand back unchanged.
           forLoop 0 (h-1) $ \y -> do
             let ro = rowOf ph0 pw0 y
             forLoop 0 (w-1) $ \x -> do
               g <- unsafeRead pl0 (ro + min (pw0-1) x)
               let o = (y*w + x)*4
               unsafeWrite img o g; unsafeWrite img (o+1) g; unsafeWrite img (o+2) g
               unsafeWrite img (o+3) 255
       | ncomp == 3 && f0 && f1 && f2 ->
           forLoop 0 (h-1) $ \y -> do
             let r0 = rowOf ph0 pw0 y; r1 = rowOf ph1 pw1 y; r2 = rowOf ph2 pw2 y
             forLoop 0 (w-1) $ \x -> do
               yf  <- rdDU pl0 (r0 + min (pw0-1) x)
               cbv <- rdDU pl1 (r1 + min (pw1-1) x)
               crv <- rdDU pl2 (r2 + min (pw2-1) x)
               ycc ((y*w + x)*4) yf cbv crv
       | ncomp == 3 && f0 && not f1 && not f2 -> do
           -- Subsampled chroma: the horizontal half of each bilinear sample
           -- depends only on the source row, so each source row is
           -- interpolated across once into a row buffer (two per component,
           -- reused as the output rows advance) and the per-pixel work is
           -- the vertical blend. Same expressions, same operands, so the
           -- samples are bit-identical to 'jSample'.
           cache1 <- newArray (0, 2*w - 1) 0 :: ST s (STUArray s Int Double)
           cache2 <- newArray (0, 2*w - 1) 0 :: ST s (STUArray s Int Double)
           tags   <- newArray (0, 3) (-1) :: ST s (STUArray s Int Int)
           let hrow pl pw xa xb xd cache r slot =
                 forLoop 0 (w-1) $ \x -> do
                   let !x0 = xa `unsafeAt` x; !x1 = xb `unsafeAt` x; !dx = xd `unsafeAt` x
                   p0 <- rdDU pl (r*pw + x0); p1 <- rdDU pl (r*pw + x1)
                   unsafeWrite cache (slot*w + x) (p0 + (p1-p0)*dx)
               -- The slot (0/1) holding source row r, interpolating it into
               -- the slot that does not hold @keep@ if neither has it.
               slotFor pl pw xa xb xd cache t r keep = do
                 a0 <- unsafeRead tags t; a1 <- unsafeRead tags (t+1)
                 if | a0 == r -> pure 0
                    | a1 == r -> pure 1
                    | otherwise -> do
                        let sl = if a0 == keep then 1 else 0
                        hrow pl pw xa xb xd cache r sl
                        unsafeWrite tags (t+sl) r
                        pure sl
           forLoop 0 (h-1) $ \y -> do
             let r0 = rowOf ph0 pw0 y
                 ya1' = ya1 `unsafeAt` y; yb1' = yb1 `unsafeAt` y; dy1 = yd1 `unsafeAt` y
                 ya2' = ya2 `unsafeAt` y; yb2' = yb2 `unsafeAt` y; dy2 = yd2 `unsafeAt` y
             t1 <- slotFor pl1 pw1 xa1 xb1 xd1 cache1 0 ya1' yb1'
             b1 <- slotFor pl1 pw1 xa1 xb1 xd1 cache1 0 yb1' ya1'
             t2 <- slotFor pl2 pw2 xa2 xb2 xd2 cache2 2 ya2' yb2'
             b2 <- slotFor pl2 pw2 xa2 xb2 xd2 cache2 2 yb2' ya2'
             let o1t = t1*w; o1b = b1*w; o2t = t2*w; o2b = b2*w
             forLoop 0 (w-1) $ \x -> do
               yf <- rdDU pl0 (r0 + min (pw0-1) x)
               tc <- unsafeRead cache1 (o1t + x); bc <- unsafeRead cache1 (o1b + x)
               tr <- unsafeRead cache2 (o2t + x); br <- unsafeRead cache2 (o2b + x)
               ycc ((y*w + x)*4) yf (tc + (bc-tc)*dy1) (tr + (br-tr)*dy2)
       | ncomp == 1 ->
           forLoop 0 (h-1) $ \y -> forLoop 0 (w-1) $ \x -> do
             yv <- jSample pw0 ph0 pl0 f0 xa0 xb0 xd0 ya0 yb0 yd0 x y
             let g = clamp8 (roundD yv) in putRGBA img w x y g g g 255
       | otherwise ->
           forLoop 0 (h-1) $ \y -> forLoop 0 (w-1) $ \x -> do
             yf  <- jSample pw0 ph0 pl0 f0 xa0 xb0 xd0 ya0 yb0 yd0 x y
             cbv <- jSample pw1 ph1 pl1 f1 xa1 xb1 xd1 ya1 yb1 yd1 x y
             crv <- jSample pw2 ph2 pl2 f2 xa2 xb2 xd2 ya2 yb2 yd2 x y
             ycc ((y*w + x)*4) yf cbv crv

-- One component sample at output (x, y) from its precomputed tables (see
-- the upsampling note in 'decodeJpegImage'). Top-level and inlined, with
-- every input an argument: called per component per pixel, a closure here
-- would return a boxed Double each time.
{-# INLINE jSample #-}
jSample :: Int -> Int -> STUArray s Int Word8 -> Bool
        -> UArray Int Int -> UArray Int Int -> UArray Int Double
        -> UArray Int Int -> UArray Int Int -> UArray Int Double
        -> Int -> Int -> ST s Double
jSample !pw !ph !plane !full !xa !xb !xd !ya !yb !yd !x !y
  | full = rdD plane (min (ph-1) y * pw + min (pw-1) x)
  | otherwise = do
      let !x0 = xa `unsafeAt` x; !x1 = xb `unsafeAt` x
          !y0 = ya `unsafeAt` y; !y1 = yb `unsafeAt` y
          !dx = xd `unsafeAt` x; !dy = yd `unsafeAt` y
      !p00 <- rdD plane (y0*pw+x0); !p10 <- rdD plane (y0*pw+x1)
      !p01 <- rdD plane (y1*pw+x0); !p11 <- rdD plane (y1*pw+x1)
      let !top = p00 + (p10-p00)*dx; !bot = p01 + (p11-p01)*dx
      pure (top + (bot-top)*dy)

-- The two IDCT passes, straight-line over each row/column so no per-sample
-- loop closure is left behind. Every sum adds its terms in the original
-- order starting from 0, so the result is bit-identical to the loop form.
-- Rows: tmp[v*8+xx] = sum_u ct[xx*8+u] * blk[v*8+u]. A row outside @mask@
-- is all zero, and its outputs are +0.0 exactly (0 plus terms of ±0.0), so
-- it is written as zeros without the arithmetic. The eight outputs of a row
-- are unrolled: as an inner loop, its closure (holding the eight inputs) was
-- allocated afresh for every row.
jIdctRows :: STUArray s Int Double -> STUArray s Int Double -> Int -> ST s ()
jIdctRows blk tmp mask = forLoop 0 7 $ \v -> do
  let o = v*8
  if not (testBit mask v)
    then forLoop o (o+7) $ \i -> unsafeWrite tmp i 0
    else do
      b0 <- unsafeRead blk o;       b1 <- unsafeRead blk (o+1)
      b2 <- unsafeRead blk (o+2);   b3 <- unsafeRead blk (o+3)
      b4 <- unsafeRead blk (o+4);   b5 <- unsafeRead blk (o+5)
      b6 <- unsafeRead blk (o+6);   b7 <- unsafeRead blk (o+7)
      let out xx =
            let c i = ctK (xx*8+i)
            in unsafeWrite tmp (o+xx)
                 (0 + c 0*b0 + c 1*b1 + c 2*b2 + c 3*b3 + c 4*b4 + c 5*b5 + c 6*b6 + c 7*b7)
          {-# INLINE out #-}
      out 0; out 1; out 2; out 3; out 4; out 5; out 6; out 7

-- Columns: out[yy][xx] = 0.25 * sum_v ct[yy*8+v] * tmp[v*8+xx], written into
-- the plane where it lies inside it; the eight outputs are unrolled for the
-- same reason as the rows. (Dropping the trailing zero rows' terms is exact
-- too, but four specialised copies measured slower: the call sites boxed.)
jIdctCols :: STUArray s Int Double -> STUArray s Int Word8 -> Int -> Int
          -> Int -> Int -> ST s ()
jIdctCols tmp plane pw ph gx0 gy0 = forLoop 0 7 $ \xx -> do
  let gx = gx0 + xx
  when (gx < pw) $ do
    t0 <- unsafeRead tmp xx;        t1 <- unsafeRead tmp (8+xx)
    t2 <- unsafeRead tmp (16+xx);   t3 <- unsafeRead tmp (24+xx)
    t4 <- unsafeRead tmp (32+xx);   t5 <- unsafeRead tmp (40+xx)
    t6 <- unsafeRead tmp (48+xx);   t7 <- unsafeRead tmp (56+xx)
    let out yy =
          let gy = gy0 + yy
              c v = ctK (yy*8+v)
              sm = 0 + c 0*t0 + c 1*t1 + c 2*t2 + c 3*t3 + c 4*t4 + c 5*t5 + c 6*t6 + c 7*t7
          in when (gy < ph) $
               unsafeWrite plane (gy*pw+gx) (fromIntegral (clamp8 (roundD (0.25 * sm + 128))))
        {-# INLINE out #-}
    out 0; out 1; out 2; out 3; out 4; out 5; out 6; out 7

-- 'round' (to nearest, ties to even) for Double -> Int, inline: the library
-- version is an out-of-line call per sample, and the decoder rounds about
-- four times per pixel. Exact for any |x| < 2^52, where x - truncate x is
-- computed without error; JPEG samples are a few thousand at most.
{-# INLINE roundD #-}
roundD :: Double -> Int
roundD x =
  let n = truncate x :: Int
      r = x - fromIntegral n
  in if | r > 0.5   -> n + 1
        | r < -0.5  -> n - 1
        | r == 0.5  -> if even n then n else n + 1
        | r == -0.5 -> if even n then n else n - 1
        | otherwise -> n

newCoefs :: Int -> ST s (STUArray s Int Int16)
newCoefs n = newArray (0, max 0 (n - 1)) 0

{-# INLINE rdC #-}
rdC :: STUArray s Int Int16 -> Int -> ST s Int
rdC a i = fromIntegral <$> readArray a i

{-# INLINE wrC #-}
wrC :: STUArray s Int Int16 -> Int -> Int -> ST s ()
wrC a i v = writeArray a i (fromIntegral v)

clamp8 :: Int -> Int
clamp8 v = max 0 (min 255 v)

clampI :: Int -> Int -> Int -> Int
clampI lo hi v = max lo (min hi v)

-- | A strict counted loop. @forM_ [lo..hi]@ builds a list and a closure per
-- element; in the JPEG reconstruct path that runs ~200 times per 8x8 block
-- over tens of thousands of blocks, which is real allocation. This compiles to
-- a plain tail-recursive loop.
forLoop :: Int -> Int -> (Int -> ST s ()) -> ST s ()
forLoop lo hi act = go lo
  where go !i | i > hi = pure ()
              | otherwise = act i >> go (i + 1)
{-# INLINE forLoop #-}

rdD :: STUArray s Int Word8 -> Int -> ST s Double
rdD a i = fromIntegral <$> readArray a i

-- The unchecked, inlined form for the conversion loops, whose indices are
-- in range by construction: called out of line, each sample came back as a
-- boxed Double.
{-# INLINE rdDU #-}
rdDU :: STUArray s Int Word8 -> Int -> ST s Double
rdDU a i = fromIntegral <$> unsafeRead a i

-- Find and skip past the next restart marker (0xFF 0xD0..0xD7).
findRST :: BS.ByteString -> Int -> Int
findRST bs = go
  where len = BS.length bs
        go p | p+1 >= len = len
             | at bs p == 0xFF && at bs (p+1) >= 0xD0 && at bs (p+1) <= 0xD7 = p+2
             | otherwise = go (p+1)

------------------------------------------------------------------------------
-- WebP: a RIFF container holding either a VP8L (lossless) or VP8 (lossy)
-- bitstream, optionally with a separate ALPH alpha plane, or an animation
-- whose ANMF frames hold the same. Both codecs are decoded from first
-- principles below; for an animation we render the first frame (like GIF).

le24 :: BS.ByteString -> Int -> Int
le24 bs i = at bs i .|. (at bs (i+1) `shiftL` 8) .|. (at bs (i+2) `shiftL` 16)

decodeWebP :: BS.ByteString -> Either String Image
decodeWebP bs = do
  when' (BS.length bs < 20) "truncated WebP file"
  wpDecodeChunks bs (wpChunks bs 12 (BS.length bs))

-- (tag, payload offset, payload size) for each chunk between two offsets.
-- Chunk payloads are padded to even sizes; a truncated final chunk is clamped.
wpChunks :: BS.ByteString -> Int -> Int -> [(BS.ByteString, Int, Int)]
wpChunks bs = go
  where
    go p end
      | p + 8 > end || end > BS.length bs = []
      | otherwise =
          let tag = BS.take 4 (BS.drop p bs)
              sz  = min (le32 bs (p+4)) (end - (p+8))
          in (tag, p+8, sz) : go (p + 8 + sz + (sz .&. 1)) end

wpDecodeChunks :: BS.ByteString -> [(BS.ByteString, Int, Int)] -> Either String Image
wpDecodeChunks bs chunks
  | Just (o,n) <- findC "VP8L" = decodeVP8L (sub o n)
  | Just (o,n) <- findC "VP8 " =
      case findC "ALPH" of
        Nothing      -> decodeVP8 (sub o n) Nothing
        Just (ao,an) -> do
          (w,h)  <- vp8Dims (sub o n)
          alpha  <- decodeALPH (sub ao an) w h
          decodeVP8 (sub o n) (Just alpha)
  | Just (o,n) <- findC "ANMF" = do
      when' (n < 16) "truncated WebP animation frame"
      frame <- wpDecodeChunks bs (wpChunks bs (o+16) (o+n))
      let fx = 2 * le24 bs o
          fy = 2 * le24 bs (o+3)
      pure (wpPlaceFrame (findC "VP8X") bs fx fy frame)
  | otherwise = Left "WebP has no image data"
  where
    findC t = case [ (o,n) | (tag,o,n) <- chunks, tag == fourCC t ] of
                (c:_) -> Just c
                []    -> Nothing
    sub o n = BS.take n (BS.drop o bs)

-- Composite an animation's first frame onto the VP8X canvas at its offset
-- (transparent elsewhere), when the canvas is sane; otherwise show the frame.
wpPlaceFrame :: Maybe (Int,Int) -> BS.ByteString -> Int -> Int -> Image -> Image
wpPlaceFrame mvp8x bs fx fy frame = case mvp8x of
  Just (co,cn)
    | cn >= 10
    , cw <- le24 bs (co+4) + 1
    , ch <- le24 bs (co+7) + 1
    , cw /= imgW frame || ch /= imgH frame
    , cw > 0 && ch > 0 && cw <= maxDim && ch <= maxDim && cw*ch <= maxPixels
    -> buildImage (imgFmt frame) cw ch $ \a ->
         forM_ [0 .. ch-1] $ \y -> forM_ [0 .. cw-1] $ \x -> do
           let ix = x - fx; iy = y - fy
           if ix >= 0 && ix < imgW frame && iy >= 0 && iy < imgH frame
             then do let o = (iy * imgW frame + ix) * 4
                         p k = fromIntegral (imgPix frame ! (o+k)) :: Int
                     putRGBA a cw x y (p 0) (p 1) (p 2) (p 3)
             else putRGBA a cw x y 0 0 0 0
  _ -> frame

-- ALPH chunk: the alpha plane for a lossy frame. One header byte (bits 0-1
-- compression, 2-3 filter), then either raw bytes or a headerless VP8L image
-- stream whose green channel carries the alpha values.
decodeALPH :: BS.ByteString -> Int -> Int -> Either String (UArray Int Word8)
decodeALPH bs w h = do
  when' (BS.null bs) "truncated WebP alpha"
  let hdr  = at bs 0
      comp = hdr .&. 3
      filt = (hdr `shiftR` 2) .&. 3
      body = BS.drop 1 bs
      n    = w*h
  -- The raw plane is written into the result and defiltered there in place.
  src <- case comp of
    0 -> do when' (BS.length body < n) "truncated WebP alpha"
            pure Nothing
    1 -> Just <$> vlDecodeStream body w h
    _ -> Left "unsupported WebP alpha compression"
  pure $ runSTUArray $ do
    a <- newArray (0, n-1) 0
    case src of
      Nothing -> forLoop 0 (n-1) $ \i -> unsafeWrite a i (BSU.unsafeIndex body i)
      Just px -> forLoop 0 (n-1) $ \i ->
                   unsafeWrite a i (fromIntegral ((px `unsafeAt` i) `shiftR` 8))
    when (filt /= 0) $ alphaDefilter filt w h a
    pure a

-- Undo ALPH scanline filtering (1 horizontal, 2 vertical, 3 gradient) in
-- place: every predictor reads only neighbours (left, above, above-left)
-- that are already reconstructed, and the residual at i before writing it.
alphaDefilter :: Int -> Int -> Int -> STUArray s Int Word8 -> ST s ()
alphaDefilter filt w h a = do
  let rd8 i = (fromIntegral :: Word8 -> Int) <$> unsafeRead a i
  forLoop 0 (h-1) $ \y -> forLoop 0 (w-1) $ \x -> do
    let i = y*w + x
    p <- if | x == 0 && y == 0 -> pure 0
            | filt == 1 -> if x == 0 then rd8 (i-w) else rd8 (i-1)
            | filt == 2 -> if y == 0 then rd8 (i-1) else rd8 (i-w)
            | x == 0    -> rd8 (i-w)
            | y == 0    -> rd8 (i-1)
            | otherwise -> do l <- rd8 (i-1); t <- rd8 (i-w); tl <- rd8 (i-w-1)
                              pure (clamp8 (l + t - tl))
    r <- rd8 i
    unsafeWrite a i (fromIntegral (r + p))

------------------------------------------------------------------------------
-- VP8L (WebP lossless): an LSB-first bit stream of canonical prefix codes.
-- Pixels are ARGB Word32s produced by literals, LZ77 back-references over the
-- flattened pixel array (with a 2-D short-distance map) and a small hashed
-- colour cache; up to four invertible transforms (predictor, colour,
-- subtract-green, palette with pixel bundling) are undone afterwards.

-- The reader's state: slot 0 of an unboxed array is the bit position (an
-- 'STRef Int' would box on every write, which is every symbol). Slot 0 is
-- also where "Cmedit.Inflate"'s 'decodeSymSlow' keeps it, so the two
-- readers share the canonical walk.
type VLSt s = STUArray s Int Int

newVLSt :: Int -> ST s (VLSt s)
newVLSt bitPos = do
  st <- newArray (0, 0) 0
  unsafeWrite st 0 bitPos
  pure st

-- 32 bits starting at byte @off@, LSB-first, zero past the end. Every read
-- but the last few takes the first branch.
{-# INLINE vlPeek #-}
vlPeek :: BS.ByteString -> Int -> Int
vlPeek bs off
  | off >= 0 && off + 3 < BS.length bs =
      u off .|. (u (off+1) `shiftL` 8) .|. (u (off+2) `shiftL` 16)
            .|. (u (off+3) `shiftL` 24)
  | otherwise = b off .|. (b (off+1) `shiftL` 8) .|. (b (off+2) `shiftL` 16)
                      .|. (b (off+3) `shiftL` 24)
  where u i = fromIntegral (BSU.unsafeIndex bs i)
        b i = if i >= 0 && i < BS.length bs then u i else 0

-- Read n bits (n <= 24) LSB-first.
{-# INLINE vlBits #-}
vlBits :: BS.ByteString -> VLSt s -> Int -> ST s Int
vlBits bs st n = do
  p <- unsafeRead st 0
  unsafeWrite st 0 (p + n)
  pure ((vlPeek bs (p `shiftR` 3) `shiftR` (p .&. 7)) .&. ((1 `shiftL` n) - 1))

-- A prefix code: a code with a single used symbol consumes no bits at all.
-- Otherwise DEFLATE's table: VP8L's canonical codes are read exactly like
-- DEFLATE's (first stream bit = code MSB), so the prefix table and its
-- canonical-walk fallback are "Cmedit.Inflate"'s own, built 'vlTableBits'
-- wide.
data VHuff = VSingle !Int | VTable {-# UNPACK #-} !(UArray Int Int) !Huff

mkVHuff :: [Int] -> VHuff
mkVHuff lens = case [ s | (s,l) <- zip [0..] lens, l > 0 ] of
  [s] -> VSingle s
  _   -> case buildInflateHuffN vlTableBits lens of
           InflateHuff hf table -> VTable table hf

vlTableBits :: Int
vlTableBits = 9

-- Decode one symbol; -1 on an invalid code.
{-# INLINE vlSym #-}
vlSym :: BS.ByteString -> VLSt s -> VHuff -> ST s Int
vlSym _ _ (VSingle s) = pure s
vlSym bs st (VTable table hf) = do
  p <- unsafeRead st 0
  let e = table `unsafeAt`
            ((vlPeek bs (p `shiftR` 3) `shiftR` (p .&. 7))
               .&. ((1 `shiftL` vlTableBits) - 1))
      l = e .&. 15
  if l == 0 then vlSymSlow bs st hf
            else do unsafeWrite st 0 (p + l); pure (e `shiftR` 4)

-- Codes longer than the table, or missing from it: rare, so one shared copy
-- of the canonical walk rather than one inlined at every symbol read. (Its
-- boxed result makes the fast path box too; routing the symbol through a
-- state slot instead removed 8 MB of churn on a photo but measured 10% slower.)
{-# NOINLINE vlSymSlow #-}
vlSymSlow :: BS.ByteString -> VLSt s -> Huff -> ST s Int
vlSymSlow = decodeSymSlow

vlCLOrder :: [Int]
vlCLOrder = [17,18,0,1,2,3,4,5,16,6,7,8,9,10,11,12,13,14,15]

-- Read one prefix code: the "simple" 1/2-symbol form, or code lengths that
-- are themselves prefix-coded (with 16/17/18 repeat codes, as in DEFLATE
-- except 16 repeats the previous *non-zero* length, initially 8).
vlReadCode :: BS.ByteString -> VLSt s -> STRef s (Maybe String) -> Int
           -> ST s VHuff
vlReadCode bs st errR alphaSize = do
  simple <- vlBits bs st 1
  if simple == 1
    then do
      two  <- vlBits bs st 1
      wide <- vlBits bs st 1
      s0   <- vlBits bs st (if wide == 1 then 8 else 1)
      ss   <- if two == 1 then do s1 <- vlBits bs st 8; pure [s0,s1]
                          else pure [s0]
      if any (>= alphaSize) ss
        then do writeSTRef errR (Just "corrupt WebP (bad prefix code)")
                pure (VSingle 0)
        else pure (mkVHuff [ if s `elem` ss then 1 else 0 | s <- [0..alphaSize-1] ])
    else do
      nCodes <- (+4) <$> vlBits bs st 4
      clRaw  <- mapM (const (vlBits bs st 3)) [1 .. min 19 nCodes]
      let clLens = A.elems (A.accumArray (\_ x -> x) 0 (0,18)
                             (zip vlCLOrder clRaw)) :: [Int]
          clHuff = mkVHuff clLens
      useMax <- vlBits bs st 1
      maxSym <- if useMax == 1
                  then do nb <- vlBits bs st 3
                          v  <- vlBits bs st (2 + 2*nb)
                          pure (2 + v)
                  else pure alphaSize
      lens <- vlReadLens bs st errR clHuff alphaSize maxSym
      pure (mkVHuff lens)

vlReadLens :: BS.ByteString -> VLSt s -> STRef s (Maybe String) -> VHuff
           -> Int -> Int -> ST s [Int]
vlReadLens bs st errR clHuff total maxSym0 = go 0 8 maxSym0 []
  where
    go n prev maxSym acc
      | n >= total  = pure (reverse acc)
      | maxSym == 0 = pure (reverse acc ++ replicate (total - n) 0)
      | otherwise = do
          s <- vlSym bs st clHuff
          if s < 0 || s > 18
            then do writeSTRef errR (Just "corrupt WebP (bad prefix code)")
                    pure (reverse acc ++ replicate (total - n) 0)
          else if s < 16
            then go (n+1) (if s /= 0 then s else prev) (maxSym-1) (s:acc)
          else do
            (rep, val) <- case s of
              16 -> do r <- vlBits bs st 2; pure (r+3, prev)
              17 -> do r <- vlBits bs st 3; pure (r+3, 0)
              _  -> do r <- vlBits bs st 7; pure (r+11, 0)
            let k = min rep (total - n)
            go (n+k) prev (maxSym-1) (replicate k val ++ acc)

-- Ceiling-divide a dimension by a power-of-two tile size.
vlSub :: Int -> Int -> Int
vlSub s bits = (s + (1 `shiftL` bits) - 1) `shiftR` bits

vlWidthBits :: Int -> Int
vlWidthBits n | n <= 2 = 3 | n <= 4 = 2 | n <= 16 = 1 | otherwise = 0

-- Per-channel helpers on ARGB words.
chan :: Word32 -> Int -> Int
chan v sh = fromIntegral ((v `shiftR` sh) .&. 0xff)
{-# INLINE chan #-}

pack32 :: Int -> Int -> Int -> Int -> Word32
pack32 a r g b = (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16)
             .|. (fromIntegral g `shiftL` 8)  .|. fromIntegral b
{-# INLINE pack32 #-}

-- Per-channel add mod 256 (SWAR on the two interleaved lane pairs).
addPix32 :: Word32 -> Word32 -> Word32
addPix32 a b = (((a .&. 0xff00ff00) + (b .&. 0xff00ff00)) .&. 0xff00ff00)
           .|. (((a .&. 0x00ff00ff) + (b .&. 0x00ff00ff)) .&. 0x00ff00ff)
{-# INLINE addPix32 #-}

-- Per-channel floor average.
avg2 :: Word32 -> Word32 -> Word32
avg2 a b = (a .&. b) + (((a `xor` b) .&. 0xfefefefe) `shiftR` 1)
{-# INLINE avg2 #-}

-- Every transform records the pixel width it operates at: the palette
-- transform narrows the entropy-coded image, so a transform read after it
-- works on the bundled width.
data VLTransform
  = VLPredictor !Int !(UArray Int Word32) !Int   -- tile bits, tiles, width
  | VLColor     !Int !(UArray Int Word32) !Int
  | VLSubGreen  !Int
  | VLPalette   !(UArray Int Word32) !Int        -- palette, unpacked width

-- Decode a whole VP8L image stream without the 5-byte container header
-- (this exact form is also how ALPH lossless alpha is stored).
vlDecodeStream :: BS.ByteString -> Int -> Int -> Either String (UArray Int Word32)
vlDecodeStream body w h = runST $ do
  st   <- newVLSt 0
  errR <- newSTRef Nothing
  out  <- newArray (0, max 0 (w*h-1)) 0
  vlMainInto body st errR w h out
  err  <- readSTRef errR
  case err of
    Just e  -> pure (Left e)
    Nothing -> Right <$> unsafeFreeze out

-- The whole image lives in one buffer from the first symbol to the last
-- byte: allocated RGBA-sized as bytes and viewed as ARGB words, it receives
-- the entropy-coded pixels, every inverse transform runs over it in place,
-- and the final swizzle to RGBA rewrites each word as its own four bytes.
-- One image-sized allocation, against one per transform plus the RGBA copy.
decodeVP8L :: BS.ByteString -> Either String Image
decodeVP8L bs = do
  when' (BS.length bs < 5) "truncated WebP lossless data"
  when' (at bs 0 /= 0x2f) "corrupt WebP (bad lossless signature)"
  let v = le32 bs 1
      w = (v .&. 0x3fff) + 1
      h = ((v `shiftR` 14) .&. 0x3fff) + 1
      ver = (v `shiftR` 29) .&. 7
  when' (ver /= 0) "unsupported WebP lossless version"
  checkDims w h
  px <- runST $ do
    st    <- newVLSt 40                -- past signature + size header
    errR  <- newSTRef Nothing
    bytes <- newBytes (w*h*4)
    out   <- asWords bytes
    vlMainInto bs st errR w h out
    err   <- readSTRef errR
    case err of
      Just e  -> pure (Left e)
      Nothing -> do
        forLoop 0 (w*h - 1) $ \i -> do
          p <- unsafeRead out i
          let o = 4*i
          unsafeWrite bytes o       (fromIntegral (p `shiftR` 16))
          unsafeWrite bytes (o + 1) (fromIntegral (p `shiftR` 8))
          unsafeWrite bytes (o + 2) (fromIntegral p)
          unsafeWrite bytes (o + 3) (fromIntegral (p `shiftR` 24))
        Right <$> unsafeFreeze bytes
  pure (Image w h "WebP" px)

-- The top-level image: transforms, then the entropy-coded pixels (into the
-- first @w'*h@ words of @out@, which must hold @w*h@), then the inverse
-- transforms in reverse read order, all in place.
vlMainInto :: BS.ByteString -> VLSt s -> STRef s (Maybe String) -> Int -> Int
           -> STUArray s Int Word32 -> ST s ()
vlMainInto bs st errR w h out = do
  (transforms, w') <- vlReadTransforms bs st errR h w
  vlEntropyInto bs st errR True w' h out
  err <- readSTRef errR
  when (err == Nothing) $ forM_ transforms $ \t -> case t of
    VLSubGreen tw          -> vlInvGreen (tw*h) out
    VLColor bits tiles tw  -> vlInvColor bits tiles tw h out
    VLPredictor bits tiles tw -> vlInvPredictor bits tiles tw h out
    VLPalette pal tw       -> vlInvPalette pal tw h out

-- A sub-image (transform data, the meta prefix image, a palette): entropy
-- coded with no transforms of its own and no meta image.
vlSubImage :: BS.ByteString -> VLSt s -> STRef s (Maybe String) -> Int -> Int
           -> ST s (UArray Int Word32)
vlSubImage bs st errR w h = do
  buf <- newArray (0, max 0 (w*h-1)) 0
  vlEntropyInto bs st errR False w h buf
  unsafeFreeze buf

-- Transforms, most recently read first (= inverse application order).
vlReadTransforms :: BS.ByteString -> VLSt s -> STRef s (Maybe String)
                 -> Int -> Int -> ST s ([VLTransform], Int)
vlReadTransforms bs st errR h = go []
  where
    go acc w = do
      more <- vlBits bs st 1
      if more == 0 then pure (acc, w) else do
        t <- vlBits bs st 2
        case t of
          0 -> do bits <- (+2) <$> vlBits bs st 3
                  timg <- vlSubImage bs st errR (vlSub w bits) (vlSub h bits)
                  go (VLPredictor bits timg w : acc) w
          1 -> do bits <- (+2) <$> vlBits bs st 3
                  timg <- vlSubImage bs st errR (vlSub w bits) (vlSub h bits)
                  go (VLColor bits timg w : acc) w
          2 -> go (VLSubGreen w : acc) w
          _ -> do n <- (+1) <$> vlBits bs st 8
                  praw <- vlSubImage bs st errR n 1
                  let pal = U.listArray (0, n-1)
                              (scanl1 addPix32 (U.elems praw)) :: UArray Int Word32
                  go (VLPalette pal w : acc) (vlSub w (vlWidthBits n))

vlInvGreen :: Int -> STUArray s Int Word32 -> ST s ()
vlInvGreen n out = forLoop 0 (n-1) $ \i -> do
  v <- unsafeRead out i
  let g = (v `shiftR` 8) .&. 0xff
      r = ((v `shiftR` 16) + g) .&. 0xff
      b = (v + g) .&. 0xff
  unsafeWrite out i ((v .&. 0xff00ff00) .|. (r `shiftL` 16) .|. b)

-- Spatial predictor transform: add the per-tile predictor to each residual,
-- scanning in raster order. In place is exact: every neighbour a predictor
-- reads (left, top-left, top, top-right) precedes the pixel being written.
vlInvPredictor :: Int -> UArray Int Word32 -> Int -> Int -> STUArray s Int Word32
               -> ST s ()
vlInvPredictor bits tiles w h out = when (w > 0 && h > 0) $ do
  let tw = vlSub w bits
  -- Row 0: the first pixel predicts opaque black, the rest their left.
  p0 <- unsafeRead out 0
  unsafeWrite out 0 (addPix32 p0 0xff000000)
  forLoop 1 (w-1) $ \x -> do
    l <- unsafeRead out (x-1); s <- unsafeRead out x
    unsafeWrite out x (addPix32 s l)
  forLoop 1 (h-1) $ \y -> do
    let row = y*w
        trow = (y `shiftR` bits) * tw
    -- Column 0 predicts from the pixel above.
    t0 <- unsafeRead out (row - w); s0 <- unsafeRead out row
    unsafeWrite out row (addPix32 s0 t0)
    -- The rest of the row in tile runs: one mode per run, dispatched once.
    forLoop 0 (tw-1) $ \tx -> do
      let x0 = max 1 (tx `shiftL` bits)
          x1 = min (w-1) (((tx + 1) `shiftL` bits) - 1)
          mode = fromIntegral ((tiles `unsafeAt` (trow + tx) `shiftR` 8) .&. 0xff)
      when (x0 <= x1) $ vlPredRun mode out w (row + x0) (row + x1)

-- One run of pixels under a single predictor mode. Each mode gets its own
-- loop (via the inlined 'vlRun'), so the per-pixel work is the arithmetic
-- alone. For the last column i-w+1 is the first pixel of the current row:
-- that is the format's actual top-right rule, not an accident.
vlPredRun :: Int -> STUArray s Int Word32 -> Int -> Int -> Int -> ST s ()
vlPredRun mode out w i0 i1 = case mode of
  0  -> vlRun out w i0 i1 (\_ _ _ _ -> 0xff000000)
  1  -> vlRun out w i0 i1 (\l _ _ _ -> l)
  2  -> vlRun out w i0 i1 (\_ t _ _ -> t)
  3  -> vlRun out w i0 i1 (\_ _ _ tr -> tr)
  4  -> vlRun out w i0 i1 (\_ _ tl _ -> tl)
  5  -> vlRun out w i0 i1 (\l t _ tr -> avg2 (avg2 l tr) t)
  6  -> vlRun out w i0 i1 (\l _ tl _ -> avg2 l tl)
  7  -> vlRun out w i0 i1 (\l t _ _ -> avg2 l t)
  8  -> vlRun out w i0 i1 (\_ t tl _ -> avg2 tl t)
  9  -> vlRun out w i0 i1 (\_ t _ tr -> avg2 t tr)
  10 -> vlRun out w i0 i1 (\l t tl tr -> avg2 (avg2 l tl) (avg2 t tr))
  11 -> vlRun out w i0 i1 (\l t tl _ -> vlPredict 11 l t tl 0)
  12 -> vlRun out w i0 i1 (\l t tl _ -> vlPredict 12 l t tl 0)
  13 -> vlRun out w i0 i1 (\l t tl _ -> vlPredict 13 l t tl 0)
  _  -> vlRun out w i0 i1 (\l _ _ _ -> l)

{-# INLINE vlRun #-}
vlRun :: STUArray s Int Word32 -> Int -> Int -> Int
      -> (Word32 -> Word32 -> Word32 -> Word32 -> Word32) -> ST s ()
vlRun out w i0 i1 f = do
  l0 <- unsafeRead out (i0 - 1)
  let go !i !l
        | i > i1 = pure ()
        | otherwise = do
            t  <- unsafeRead out (i - w)
            tl <- unsafeRead out (i - w - 1)
            tr <- unsafeRead out (i - w + 1)
            s  <- unsafeRead out i
            let v = addPix32 s (f l t tl tr)
            unsafeWrite out i v
            go (i + 1) v
  go i0 l0

{-# INLINE vlPredict #-}
vlPredict :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32
vlPredict m l t tl tr = case m of
  0  -> 0xff000000
  1  -> l
  2  -> t
  3  -> tr
  4  -> tl
  5  -> avg2 (avg2 l tr) t
  6  -> avg2 l tl
  7  -> avg2 l t
  8  -> avg2 tl t
  9  -> avg2 t tr
  10 -> avg2 (avg2 l tl) (avg2 t tr)
  11 -> let d sh = abs (chan l sh - chan tl sh) - abs (chan t sh - chan tl sh)
        in if d 24 + d 16 + d 8 + d 0 <= 0 then t else l
  12 -> let f sh = clamp8 (chan l sh + chan t sh - chan tl sh)
        in pack32 (f 24) (f 16) (f 8) (f 0)
  13 -> let av = avg2 l t
            f sh = clamp8 (chan av sh + (chan av sh - chan tl sh) `quot` 2)
        in pack32 (f 24) (f 16) (f 8) (f 0)
  _  -> l

-- Cross-colour transform inverse: signed fixed-point green/red corrections.
vlInvColor :: Int -> UArray Int Word32 -> Int -> Int -> STUArray s Int Word32
           -> ST s ()
vlInvColor bits tiles w h out = forLoop 0 (h-1) $ \y -> do
  let row = y*w
      trow = (y `shiftR` bits) * tw
  -- One tile run at a time, its three signed multipliers decoded once.
  forLoop 0 (tw-1) $ \tx -> do
    let cte = tiles `unsafeAt` (trow + tx)
        g2r = s8 (chan cte 0); g2b = s8 (chan cte 8); r2b = s8 (chan cte 16)
        x0 = tx `shiftL` bits
        x1 = min (w-1) (((tx + 1) `shiftL` bits) - 1)
    forLoop (row + x0) (row + x1) $ \i -> do
      v <- unsafeRead out i
      let g  = s8 (chan v 8)
          r' = (chan v 16 + dlt g2r g) .&. 0xff
          b' = (chan v 0 + dlt g2b g + dlt r2b (s8 r')) .&. 0xff
      unsafeWrite out i
        ((v .&. 0xff00ff00) .|. (fromIntegral r' `shiftL` 16) .|. fromIntegral b')
  where tw = vlSub w bits
        s8 n = if n >= 128 then n - 256 else n
        dlt a c = (a * c) `shiftR` 5

-- Palette lookup, unbundling packed sub-byte indices when the palette is
-- small enough that several pixels share one green byte. The bundled image
-- occupies the front of the buffer, so it is expanded back to front: a
-- packed source index never exceeds the destination being written, and every
-- index already written is larger than any still to be read.
vlInvPalette :: UArray Int Word32 -> Int -> Int -> STUArray s Int Word32
             -> ST s ()
vlInvPalette pal w h out = go (h-1) (w-1)
  where
    n   = snd (bounds pal) + 1
    wb  = vlWidthBits n
    bpp = 8 `shiftR` wb
    pw  = vlSub w wb
    xm  = (1 `shiftL` wb) - 1
    im  = (1 `shiftL` bpp) - 1
    go !y !x
      | y < 0 = pure ()
      | x < 0 = go (y-1) (w-1)
      | otherwise = do
          packed <- unsafeRead out (y*pw + (x `shiftR` wb))
          let idx = (chan packed 8 `shiftR` ((x .&. xm) * bpp)) .&. im
          unsafeWrite out (y*w + x) (if idx < n then pal `unsafeAt` idx else 0)
          go y (x-1)

data VGroup = VGroup !VHuff !VHuff !VHuff !VHuff !VHuff

-- The entropy-coded pixels: optional colour cache, optional meta prefix-code
-- image (top level only), then per-pixel symbols: literal / back-reference /
-- cache hit. Writes the first @w*h@ words of @out@.
vlEntropyInto :: BS.ByteString -> VLSt s -> STRef s (Maybe String) -> Bool
              -> Int -> Int -> STUArray s Int Word32 -> ST s ()
vlEntropyInto bs st errR level0 w h out = do
  ccBit <- vlBits bs st 1
  cacheBits <- if ccBit == 1 then vlBits bs st 4 else pure 0
  when (ccBit == 1 && (cacheBits < 1 || cacheBits > 11)) $
    writeSTRef errR (Just "corrupt WebP (bad colour cache)")
  let cacheSize = if ccBit == 1 && cacheBits >= 1 && cacheBits <= 11
                    then 1 `shiftL` cacheBits else 0
  (metaBits, metaImg) <- if level0
    then do useMeta <- vlBits bs st 1
            if useMeta == 1
              then do mb <- (+2) <$> vlBits bs st 3
                      mi <- vlSubImage bs st errR (vlSub w mb) (vlSub h mb)
                      pure (mb, Just mi)
              else pure (0, Nothing)
    else pure (0, Nothing)
  let nGroups = case metaImg of
        Nothing -> 1
        Just mi -> 1 + maximum
                     (0 : [ fromIntegral ((v `shiftR` 8) .&. 0xffff) | v <- U.elems mi ])
      rc = vlReadCode bs st errR
  groups <- mapM (\_ -> VGroup <$> rc (256 + 24 + cacheSize) <*> rc 256
                               <*> rc 256 <*> rc 256 <*> rc 40) [1 .. nGroups]
  let garr = A.listArray (0, nGroups-1) groups :: Array Int VGroup
      g0 = garr A.! 0
      mtw = vlSub w metaBits
      total = w * h
      groupAt x y = case metaImg of
        Nothing -> g0
        Just mi ->
          let gi = fromIntegral ((mi `unsafeAt` ((y `shiftR` metaBits)*mtw
                                                 + (x `shiftR` metaBits))
                                   `shiftR` 8) .&. 0xffff)
          in if gi < nGroups then garr A.! gi else g0
  cache <- newArray (0, max 0 cacheSize) 0 :: ST s (STUArray s Int Word32)
  let insertC !px = when (cacheSize > 0) $
        unsafeWrite cache
          (fromIntegral ((0x1e35a7bd * px) `shiftR` (32 - cacheBits))) px
      bail m = writeSTRef errR (Just m)
      next !pos !x !y | x + 1 == w = loop (pos+1) 0 (y+1)
                      | otherwise  = loop (pos+1) (x+1) y
      loop !pos !x !y
        | pos >= total = pure ()
        | otherwise = do
            let !(VGroup gT rT bT aT dT) = groupAt x y
            s <- vlSym bs st gT
            if s < 0 then bail "corrupt WebP (bad prefix code)"
            else if s < 256 then do
              r <- vlSym bs st rT
              b <- vlSym bs st bT
              a <- vlSym bs st aT
              if r < 0 || b < 0 || a < 0
                then bail "corrupt WebP (bad prefix code)"
                else do let px = pack32 a r s b
                        unsafeWrite out pos px
                        insertC px
                        next pos x y
            else if s < 280 then do
              len <- vlPrefixVal bs st (s - 256)
              dc  <- vlSym bs st dT
              if dc < 0 || dc > 39 then bail "corrupt WebP (bad distance)"
              else do
                dv <- vlPrefixVal bs st dc
                let dist = vlPlaneDist w dv
                if dist > pos || len <= 0
                  then bail "corrupt WebP (bad back-reference)"
                  else do
                    let k = min len (total - pos)
                        copy !j | j >= k = pure ()
                                | otherwise = do
                                    v <- unsafeRead out (pos - dist + j)
                                    unsafeWrite out (pos + j) v
                                    insertC v
                                    copy (j + 1)
                    copy 0
                    let pos' = pos + k
                        x' = x + k
                    if x' < w then loop pos' x' y
                              else let (q, r) = pos' `quotRem` w in loop pos' r q
            else do
              let ci = s - 280
              if ci >= cacheSize then bail "corrupt WebP (bad cache index)"
              else do v <- unsafeRead cache ci
                      unsafeWrite out pos v
                      next pos x y
  err0 <- readSTRef errR
  when (err0 == Nothing) $ loop 0 0 0

newBytes :: Int -> ST s (STUArray s Int Word8)
newBytes n = newArray (0, max 0 (n - 1)) 0

-- memcpy between two 0-based byte buffers (STUArray indices are byte
-- offsets).
{-# INLINE copyBytesU8 #-}
copyBytesU8 :: STUArray s Int Word8 -> Int -> STUArray s Int Word8 -> Int -> Int -> ST s ()
copyBytesU8 (STUArray _ _ _ src#) (I# so#) (STUArray _ _ _ dst#) (I# do#) (I# n#) =
  ST $ \s# -> case copyMutableByteArray# src# so# dst# do# n# s# of s1# -> (# s1#, () #)

-- Shrink a byte buffer in place to its first n > 0 bytes (no copy).
shrinkBytes :: STUArray s Int Word8 -> Int -> ST s (STUArray s Int Word8)
shrinkBytes (STUArray _ _ _ marr) n@(I# n#) =
  ST $ \s -> case shrinkMutableByteArray# marr n# s of
               s' -> (# s', STUArray 0 (n - 1) n marr #)

-- A word view of a byte buffer. Its bounds are still the byte bounds, so it
-- is only ever accessed with 'unsafeRead'/'unsafeWrite' inside the first
-- quarter.
asWords :: STUArray s Int Word8 -> ST s (STUArray s Int Word32)
asWords = castSTUArray

-- LZ77 length/distance prefix values: 24 codes, low four literal.
{-# INLINE vlPrefixVal #-}
vlPrefixVal :: BS.ByteString -> VLSt s -> Int -> ST s Int
vlPrefixVal bs st code
  | code < 4 = pure (code + 1)
  | otherwise = do
      let eb = (code - 2) `shiftR` 1
      extra <- vlBits bs st eb
      pure (((2 + (code .&. 1)) `shiftL` eb) + extra + 1)

-- Map a distance prefix value to a linear pixel distance: the first 120
-- codes are a 2-D neighbourhood (see 'vlDistMap'), the rest are literal.
vlPlaneDist :: Int -> Int -> Int
vlPlaneDist w code
  | code > 120 = code - 120
  | otherwise  = let v  = vlDistMap ! (code - 1)
                     dy = v `shiftR` 4
                     dx = 8 - (v .&. 0xf)
                 in max 1 (dy * w + dx)

------------------------------------------------------------------------------
-- VP8 (WebP lossy): a single intra-only key frame (RFC 6386). A boolean
-- arithmetic decoder drives everything: the frame header, per-macroblock
-- intra modes, and the DCT token partitions. Reconstruction is intra
-- prediction + dequantise + 4x4 IDCT (plus a WHT for the luma DC plane),
-- followed by the in-loop deblocking filter and 4:2:0 chroma upsampling.

vpBands :: UArray Int Int
vpBands = U.listArray (0,16) [0,1,2,3,6,4,5,6,6,6,6,6,6,6,6,7,0]

vpZigzag :: UArray Int Int
vpZigzag = U.listArray (0,15) [0,1,4,8,5,2,3,6,9,12,13,10,7,11,14,15]

-- Extra-bit probabilities for the large-coefficient categories 3..6, flat:
-- category k's run is [vpCatStart!k, vpCatStart!(k+1)).
vpCatProbs, vpCatStart :: UArray Int Int
vpCatProbs = U.listArray (0, 22)
  [ 173,148,140
  , 176,155,140,135
  , 180,157,141,134,130
  , 254,254,243,230,196,177,153,140,133,130,129 ]
vpCatStart = U.listArray (0, 4) [0, 3, 7, 12, 23]

-- Boolean arithmetic decoder (RFC 6386 section 7), in libwebp's formulation:
-- the same arithmetic as the RFC's bit-at-a-time loop, but with up to 56 bits
-- prefetched and each renormalisation done as one computed shift. State lives
-- in an unboxed array: 0 byte position, 1 value window, 2 bits in the window
-- below the active byte (negative = refill due), 3 range minus one. Reads
-- past the end of the partition yield zero bytes, as the format prescribes.
data VPBool s = VPBool !BS.ByteString !Int !(STUArray s Int Int)
                       -- buffer, end, state

vpNewBool :: BS.ByteString -> Int -> Int -> ST s (VPBool s)
vpNewBool bs off len = do
  let end = min (BS.length bs) (max off (off + len))
  st <- newArray (0, 3) 0
  unsafeWrite st 0 off
  unsafeWrite st 2 (-8)
  unsafeWrite st 3 254
  vpLoad bs end st
  pure (VPBool bs end st)

-- Refill: seven bytes at once when they are there, else one (or a zero).
-- The window holds fewer than 8 bits whenever this runs, so 56 more fit.
vpLoad :: BS.ByteString -> Int -> STUArray s Int Int -> ST s ()
vpLoad bs end st = do
  pos   <- unsafeRead st 0
  value <- unsafeRead st 1
  bits  <- unsafeRead st 2
  if pos >= 0 && pos + 7 <= end
    then do
      let b i = fromIntegral (BSU.unsafeIndex bs (pos + i)) :: Int
          v = (b 0 `shiftL` 48) .|. (b 1 `shiftL` 40) .|. (b 2 `shiftL` 32)
              .|. (b 3 `shiftL` 24) .|. (b 4 `shiftL` 16) .|. (b 5 `shiftL` 8) .|. b 6
      unsafeWrite st 1 ((value `shiftL` 56) .|. v)
      unsafeWrite st 2 (bits + 56)
      unsafeWrite st 0 (pos + 7)
    else do
      let byte = if pos >= 0 && pos < end then at bs pos else 0
      unsafeWrite st 1 ((value `shiftL` 8) .|. byte)
      unsafeWrite st 2 (bits + 8)
      unsafeWrite st 0 (pos + 1)

-- One exit, with the bit and the new range/value picked as pure values:
-- two monadic exits let case-of-case copy each call site's continuation into
-- both, and this is inlined at every token, which multiplied the frame
-- decoder's code size many times over.
{-# INLINE vpGetBool #-}
vpGetBool :: VPBool s -> Int -> ST s Int
vpGetBool (VPBool bs end st) prob = do
  bits0 <- unsafeRead st 2
  when (bits0 < 0) $ vpLoad bs end st
  value <- unsafeRead st 1
  bits  <- unsafeRead st 2
  rng   <- unsafeRead st 3
  let split = (rng * prob) `shiftR` 8
      big   = (split + 1) `shiftL` bits
      bit   = if value >= big then 1 else 0   -- (value >> bits) > split
      r     = if bit == 1 then rng - split else split + 1
      -- Doublings that bring range r (1..255) back to at least 128.
      sh    = countLeadingZeros r - (finiteBitSize r - 8)
  unsafeWrite st 1 (value - big * bit)
  unsafeWrite st 2 (bits - sh)
  unsafeWrite st 3 ((r `shiftL` sh) - 1)
  pure bit

vpFlag :: VPBool s -> ST s Int
vpFlag bd = vpGetBool bd 128

-- An n-bit unsigned literal, most significant bit first.
vpLit :: VPBool s -> Int -> ST s Int
vpLit bd n = foldM (\acc _ -> do b <- vpFlag bd; pure (acc*2 + b)) 0 [1..n]

-- A flagged, sign-and-magnitude delta (0 when the flag is clear).
vpFlagged :: VPBool s -> Int -> ST s Int
vpFlagged bd n = do
  f <- vpFlag bd
  if f == 0 then pure 0 else do
    v <- vpLit bd n
    s <- vpFlag bd
    pure (if s == 1 then negate v else v)

-- Frame dimensions from the uncompressed 10-byte VP8 frame header.
vp8Dims :: BS.ByteString -> Either String (Int, Int)
vp8Dims bs = do
  when' (BS.length bs < 10) "truncated WebP lossy data"
  when' (le24 bs 0 .&. 1 /= 0) "WebP lossy frame is not a key frame"
  when' (not (at bs 3 == 0x9d && at bs 4 == 0x01 && at bs 5 == 0x2a))
        "corrupt WebP (bad VP8 start code)"
  pure (le16 bs 6 .&. 0x3fff, le16 bs 8 .&. 0x3fff)

decodeVP8 :: BS.ByteString -> Maybe (UArray Int Word8) -> Either String Image
decodeVP8 bs malpha = do
  (w, h) <- vp8Dims bs
  checkDims w h
  let part0 = le24 bs 0 `shiftR` 5
  when' (part0 <= 0 || 10 + part0 > BS.length bs) "truncated WebP lossy data"
  let (yP, uP, vP) = runST (vpDecodeFrame bs part0 w h)
      mbW = (w + 15) `div` 16
  pure (vpToImage w h (mbW*16) (mbW*8) yP uP vP malpha)

-- The whole frame decode: header, modes, residuals+reconstruction, filter.
vpDecodeFrame :: BS.ByteString -> Int -> Int -> Int
              -> ST s (UArray Int Word8, UArray Int Word8, UArray Int Word8)
vpDecodeFrame bs part0 w h = do
  let mbW = (w + 15) `div` 16
      mbH = (h + 15) `div` 16
      yW = mbW*16; yH = mbH*16
      cW = mbW*8;  cH = mbH*8
      nMB = mbW * mbH
  bd0 <- vpNewBool bs 10 part0

  -- Frame header ------------------------------------------------------------
  _colourSpace <- vpFlag bd0
  _clamping    <- vpFlag bd0
  -- Segmentation.
  segEnabled <- vpFlag bd0
  (segUpdMap, segAbs, segQ, segLf, segProbs) <-
    if segEnabled == 1
      then do
        updMap  <- vpFlag bd0
        updData <- vpFlag bd0
        (sAbs, sq, sl) <-
          if updData == 1
            then do sAbs <- vpFlag bd0
                    sq <- mapM (const (vpFlagged bd0 7)) [1..4::Int]
                    sl <- mapM (const (vpFlagged bd0 6)) [1..4::Int]
                    pure (sAbs, sq, sl)
            else pure (0, [0,0,0,0], [0,0,0,0])
        probs <- if updMap == 1
                   then mapM (\_ -> do f <- vpFlag bd0
                                       if f == 1 then vpLit bd0 8 else pure 255)
                             [1..3::Int]
                   else pure [255,255,255]
        pure (updMap, sAbs, sq, sl, probs)
      else pure (0, 0, [0,0,0,0], [0,0,0,0], [255,255,255])
  -- Loop filter.
  fSimple <- vpFlag bd0
  fLevel  <- vpLit bd0 6
  fSharp  <- vpLit bd0 3
  lfDelta <- vpFlag bd0
  (refDelta0, modeDelta0) <-
    if lfDelta == 1
      then do upd <- vpFlag bd0
              if upd == 1
                then do rs <- mapM (const (vpFlagged bd0 6)) [1..4::Int]
                        ms <- mapM (const (vpFlagged bd0 6)) [1..4::Int]
                        pure (head rs, head ms)
                else pure (0, 0)
      else pure (0, 0)
  -- Token partitions.
  nPartsL2 <- vpLit bd0 2
  let nParts = 1 `shiftL` nPartsL2
      sizesOff = 10 + part0
      dataOff  = sizesOff + 3*(nParts-1)
      fileLen  = BS.length bs
      partAt i off
        | i >= nParts = []
        | i == nParts - 1 = [(off, max 0 (fileLen - off))]
        | otherwise =
            let sz = if sizesOff + 3*i + 3 <= fileLen then le24 bs (sizesOff + 3*i) else 0
            in (off, sz) : partAt (i+1) (off + sz)
      parts = partAt 0 dataOff
  partBds <- mapM (\(o,n) -> vpNewBool bs o n) parts
  let partArr = A.listArray (0, nParts-1) partBds
  -- Quantisers.
  qBase  <- vpLit bd0 7
  qYdc   <- vpFlagged bd0 4
  qY2dc  <- vpFlagged bd0 4
  qY2ac  <- vpFlagged bd0 4
  qUVdc  <- vpFlagged bd0 4
  qUVac  <- vpFlagged bd0 4
  let dcq i = vpDcQ ! clampI 0 127 i
      acq i = vpAcQ ! clampI 0 127 i
      segQIndex s | segEnabled == 1 = clampI 0 127 (if segAbs == 1 then segQ !! s
                                                    else qBase + segQ !! s)
                  | otherwise = qBase
      quants = A.listArray (0, 3)
        [ let q = segQIndex s
          in ( dcq (q + qYdc), acq q
             , dcq (q + qY2dc) * 2, max 8 (acq (q + qY2ac) * 155 `div` 100)
             , min 132 (dcq (q + qUVdc)), acq (q + qUVac) )
        | s <- [0..3] ] :: Array Int (Int,Int,Int,Int,Int,Int)
  -- Coefficient probabilities (defaults, then per-frame updates).
  _refreshEntropy <- vpFlag bd0
  probs <- newArray (0, 1055) 0 :: ST s (STUArray s Int Int)
  forLoop 0 1055 $ \i -> writeArray probs i (vpCoeffProbs ! i)
  forLoop 0 1055 $ \i -> do
    u <- vpGetBool bd0 (vpCoeffUpdate ! i)
    when (u == 1) $ writeArray probs i =<< vpLit bd0 8
  useSkip  <- vpFlag bd0
  skipProb <- if useSkip == 1 then vpLit bd0 8 else pure 0

  -- Pass A: per-MB modes (rest of the first partition) -----------------------
  segIds  <- newArray (0, max 0 (nMB-1)) 0    :: ST s (STUArray s Int Int)
  skips   <- newArray (0, max 0 (nMB-1)) 0    :: ST s (STUArray s Int Int)
  ymodes  <- newArray (0, max 0 (nMB-1)) 0    :: ST s (STUArray s Int Int)
  uvmodes <- newArray (0, max 0 (nMB-1)) 0    :: ST s (STUArray s Int Int)
  bmodes  <- newArray (0, max 0 (nMB*16-1)) 0 :: ST s (STUArray s Int Int)
  aboveB  <- newArray (0, max 0 (mbW*4-1)) 0  :: ST s (STUArray s Int Int)
  leftB   <- newArray (0, 3) 0                :: ST s (STUArray s Int Int)
  let spr k = segProbs !! k
  forLoop (0) (mbH-1) $ \mby -> do
    forLoop 0 3 $ \i -> writeArray leftB i 0
    forLoop (0) (mbW-1) $ \mbx -> do
      let mi = mby*mbW + mbx
      when (segUpdMap == 1) $ do
        b0 <- vpGetBool bd0 (spr 0)
        s  <- if b0 == 0 then do b <- vpGetBool bd0 (spr 1); pure b
                         else do b <- vpGetBool bd0 (spr 2); pure (2 + b)
        writeArray segIds mi s
      when (useSkip == 1) $ writeArray skips mi =<< vpGetBool bd0 skipProb
      -- Key-frame luma mode (fixed probabilities).
      b145 <- vpGetBool bd0 145
      ym <- if b145 == 0 then pure 4 else do          -- 4 = B_PRED
              b156 <- vpGetBool bd0 156
              if b156 == 0
                then do b163 <- vpGetBool bd0 163
                        pure (if b163 == 0 then 0 else 1)   -- DC / V
                else do b128 <- vpGetBool bd0 128
                        pure (if b128 == 0 then 2 else 3)   -- H / TM
      writeArray ymodes mi ym
      if ym == 4
        then forLoop 0 15 $ \j -> do
               let r = j `shiftR` 2; c = j .&. 3
               a <- if r == 0 then rdI aboveB (mbx*4 + c)
                              else rdI bmodes (mi*16 + j - 4)
               l <- if c == 0 then rdI leftB r
                              else rdI bmodes (mi*16 + j - 1)
               m <- vpReadBMode bd0 ((a*10 + l)*9)
               writeArray bmodes (mi*16 + j) m
               when (r == 3) $ writeArray aboveB (mbx*4 + c) m
               when (c == 3) $ writeArray leftB r m
        else do
          -- Implied sub-modes give the neighbours' contexts.
          let imp = case ym of 0 -> 0; 1 -> 2; 2 -> 3; _ -> 1
          forLoop 0 15 $ \j -> writeArray bmodes (mi*16 + j) imp
          forLoop 0 3 $ \i -> do writeArray aboveB (mbx*4 + i) imp
                                 writeArray leftB i imp
      -- Chroma mode.
      b142 <- vpGetBool bd0 142
      uvm <- if b142 == 0 then pure 0 else do
               b114 <- vpGetBool bd0 114
               if b114 == 0 then pure 1 else do
                 b183 <- vpGetBool bd0 183
                 pure (if b183 == 0 then 2 else 3)
      writeArray uvmodes mi uvm

  -- Pass B: residuals + reconstruction ---------------------------------------
  yPl <- newArray (0, max 0 (yW*yH-1)) 0 :: ST s (STUArray s Int Word8)
  uPl <- newArray (0, max 0 (cW*cH-1)) 0 :: ST s (STUArray s Int Word8)
  vPl <- newArray (0, max 0 (cW*cH-1)) 0 :: ST s (STUArray s Int Word8)
  coeffs  <- newArray (0, 25*16-1) 0 :: ST s (STUArray s Int Int)
  nCoefs  <- newArray (0, 24) 0      :: ST s (STUArray s Int Int)
  aboveNZ <- newArray (0, max 0 (mbW*9-1)) 0 :: ST s (STUArray s Int Int)
  leftNZ  <- newArray (0, 8) 0               :: ST s (STUArray s Int Int)
  fLevels <- newArray (0, max 0 (nMB-1)) 0   :: ST s (STUArray s Int Int)
  fInner  <- newArray (0, max 0 (nMB-1)) 0   :: ST s (STUArray s Int Int)
  tmp16   <- newArray (0, 15) 0              :: ST s (STUArray s Int Int)
  edge    <- newArray (0, 12) 0              :: ST s (STUArray s Int Int)
  trMB    <- newArray (0, 3) 0               :: ST s (STUArray s Int Int)

  forLoop (0) (mbH-1) $ \mby -> do
    forLoop 0 8 $ \i -> writeArray leftNZ i 0
    forLoop (0) (mbW-1) $ \mbx -> do
      let mi = mby*mbW + mbx
          bd = partArr A.! (mby `mod` nParts)
      ym   <- rdI ymodes mi
      uvm  <- rdI uvmodes mi
      seg  <- rdI segIds mi
      skip <- rdI skips mi
      let hasY2 = ym /= 4
          (y1dc, y1ac, y2dc, y2ac, uvdc, uvac) = quants A.! seg
      forLoop (0) (25*16-1) $ \i -> writeArray coeffs i 0
      forLoop 0 24 $ \i -> writeArray nCoefs i 0
      hasCoeff <-
        if skip == 1
          then do
            forLoop 0 3 $ \i -> do writeArray aboveNZ (mbx*9 + i) 0
                                   writeArray leftNZ i 0
            forLoop 4 7 $ \i -> do writeArray aboveNZ (mbx*9 + i) 0
                                   writeArray leftNZ i 0
            when hasY2 $ do writeArray aboveNZ (mbx*9 + 8) 0
                            writeArray leftNZ 8 0
            pure False
          else do
            -- Y2 (luma DC) block.
            (firstY, tyY, dcNz) <-
              if hasY2
                then do
                  aY2 <- rdI aboveNZ (mbx*9 + 8)
                  lY2 <- rdI leftNZ 8
                  vpBlockCoeffs bd probs 1 (aY2 + lY2) 0 (y2dc, y2ac) coeffs (24*16)
                                nCoefs 24
                  n <- rdI nCoefs 24
                  writeArray aboveNZ (mbx*9 + 8) (if n > 0 then 1 else 0)
                  writeArray leftNZ 8 (if n > 0 then 1 else 0)
                  -- Inverse WHT scatters the DC values into the 16 Y blocks.
                  nz <- vpWhtScatter tmp16 coeffs (24*16)
                  pure (1, 0, nz)
                else pure (0, 3, False)
            -- Y blocks.
            let anyBlocks lo hi body = go lo False
                  where go !b !acc | b > hi = pure acc
                                   | otherwise = do nz <- body b; go (b + 1) (acc || nz)
            yNz <- anyBlocks 0 15 (\b -> do
                     let bx = b .&. 3; by = b `shiftR` 2
                     aN <- rdI aboveNZ (mbx*9 + bx)
                     lN <- rdI leftNZ by
                     vpBlockCoeffs bd probs tyY (aN + lN) firstY (y1dc, y1ac)
                                   coeffs (b*16) nCoefs b
                     n <- rdI nCoefs b
                     let nz = if n > firstY then 1 else 0
                     writeArray aboveNZ (mbx*9 + bx) nz
                     writeArray leftNZ by nz
                     pure (nz == 1))
            -- Chroma blocks (U then V).
            uvNz <- anyBlocks 16 23 (\b -> do
                     let j = b - 16
                         pl = j `shiftR` 2         -- 0 = U, 1 = V
                         bx = j .&. 1; by = (j `shiftR` 1) .&. 1
                         ni = 4 + pl*2
                     aN <- rdI aboveNZ (mbx*9 + ni + bx)
                     lN <- rdI leftNZ (ni + by)
                     vpBlockCoeffs bd probs 2 (aN + lN) 0 (uvdc, uvac)
                                   coeffs (b*16) nCoefs b
                     n <- rdI nCoefs b
                     let nz = if n > 0 then 1 else 0
                     writeArray aboveNZ (mbx*9 + ni + bx) nz
                     writeArray leftNZ (ni + by) nz
                     pure (nz == 1))
            pure (yNz || uvNz || dcNz)
      -- Reconstruction.
      let x0 = mbx*16; y0 = mby*16
          idctIf pl pw px py b = do
            n  <- rdI nCoefs b
            dc <- rdI coeffs (b*16)
            if n > 1 then vpIdctAdd tmp16 pl pw px py coeffs (b*16)
            else when (dc /= 0) $ vpIdctDcAdd pl pw px py dc
      if ym == 4
        then do
          -- Above-MB-row pixels right of this MB: every right-column
          -- subblock's top-right predictor (the format's rule).
          forLoop 0 3 $ \i -> vpRd yPl yW (x0 + 16 + i) (y0 - 1) >>= writeArray trMB i
          forLoop 0 15 $ \j -> do
            let r = j `shiftR` 2; c = j .&. 3
                bx = x0 + c*4; by = y0 + r*4
            m <- rdI bmodes (mi*16 + j)
            vpRd yPl yW (bx-1) (by-1) >>= unsafeWrite edge 4
            forLoop 0 3 $ \i -> do
              vpRd yPl yW (bx+i) (by-1) >>= unsafeWrite edge (5+i)
              vpRd yPl yW (bx-1) (by+i) >>= unsafeWrite edge (3-i)
              if c == 3 then unsafeRead trMB i >>= unsafeWrite edge (9+i)
                        else vpRd yPl yW (bx+4+i) (by-1) >>= unsafeWrite edge (9+i)
            vpPredB edge yPl yW bx by m
            idctIf yPl yW bx by j
        else do
          vpPredBlock yPl yW x0 y0 16 ym
          forLoop 0 15 $ \j ->
            idctIf yPl yW (x0 + (j .&. 3)*4) (y0 + (j `shiftR` 2)*4) j
      let cx0 = mbx*8; cy0 = mby*8
      vpPredBlock uPl cW cx0 cy0 8 uvm
      forLoop 16 19 $ \b -> do
        let j = b - 16
        idctIf uPl cW (cx0 + (j .&. 1)*4) (cy0 + ((j `shiftR` 1) .&. 1)*4) b
      vpPredBlock vPl cW cx0 cy0 8 uvm
      forLoop 20 23 $ \b -> do
        let j = b - 20
        idctIf vPl cW (cx0 + (j .&. 1)*4) (cy0 + ((j `shiftR` 1) .&. 1)*4) b
      -- Loop-filter strength for this MB.
      let lvl0 = if segEnabled == 1
                   then clampI 0 63 (if segAbs == 1 then segLf !! seg
                                     else fLevel + segLf !! seg)
                   else fLevel
          lvl1 = if lfDelta == 1
                   then clampI 0 63 (lvl0 + refDelta0
                                     + (if ym == 4 then modeDelta0 else 0))
                   else lvl0
      writeArray fLevels mi lvl1
      writeArray fInner mi (if ym == 4 || hasCoeff then 1 else 0)

  -- In-loop deblocking filter -------------------------------------------------
  when (fLevel > 0 && fSimple <= 1) $                    -- 0 = normal, 1 = simple
    vpFilterFrame yPl uPl vPl yW cW mbW mbH fLevels fInner fSharp (fSimple == 1)
  -- Nothing writes the planes after this point, so they need no copy.
  yF <- unsafeFreeze yPl
  uF <- unsafeFreeze uPl
  vF <- unsafeFreeze vPl
  pure (yF, uF, vF)

rdI :: STUArray s Int Int -> Int -> ST s Int
rdI = readArray

-- Key-frame 4x4 sub-mode, context-coded against the above/left sub-modes.
-- Mode numbering follows the tree-leaf order 0..9 = DC TM VE HE RD VR LD VL
-- HD HU (libwebp's), because 'vpBModeProbs' is indexed by it — this differs
-- from RFC 6386's enum, which swaps LD in front of RD/VR.
vpReadBMode :: VPBool s -> Int -> ST s Int
vpReadBMode bd base = do
  let pr i = vpGetBool bd (vpBModeProbs ! (base + i))
  b0 <- pr 0
  if b0 == 0 then pure 0 else do
    b1 <- pr 1
    if b1 == 0 then pure 1 else do
      b2 <- pr 2
      if b2 == 0 then pure 2 else do
        b3 <- pr 3
        if b3 == 0
          then do b4 <- pr 4
                  if b4 == 0 then pure 3
                  else do b5 <- pr 5; pure (if b5 == 0 then 4 else 5)
          else do b6 <- pr 6
                  if b6 == 0 then pure 6
                  else do b7 <- pr 7
                          if b7 == 0 then pure 7
                          else do b8 <- pr 8; pure (if b8 == 0 then 8 else 9)

-- Decode one 4x4 block's DCT tokens into dequantised coefficients (natural
-- order via the zig-zag); returns the position the scan stopped at.
-- The scan position the block stopped at is written to @res!ri@ rather than
-- returned: a non-inlined 'ST' function boxes an 'Int' result on every call.
-- The token loop is a local function called only in tail position, so it
-- compiles to a join point and allocates nothing. (Carrying the decoder
-- state in loop arguments instead of the state array was tried: no faster,
-- and its continuations did not all become join points.)
vpBlockCoeffs :: VPBool s -> STUArray s Int Int -> Int -> Int -> Int
              -> (Int, Int) -> STUArray s Int Int -> Int
              -> STUArray s Int Int -> Int -> ST s ()
vpBlockCoeffs bd probs ty ctx0 first (dqDc, dqAc) out boff res ri =
  loop True first (vpPIdx ty first ctx0)
  where
    -- @mayEob@ is False straight after a zero token, which the format
    -- follows without an end-of-block check.
    loop !mayEob !n !p = do
      let prB i = do pv <- unsafeRead probs (p + i); vpGetBool bd pv
      e <- if mayEob then prB 0 else pure 1
      if e == 0 then unsafeWrite res ri n else do
        z <- prB 1
        if z == 0
          then do
            let n1 = n + 1
            if n1 == 16 then unsafeWrite res ri 16
                        else loop False n1 (vpPIdx ty n1 0)
          else do
            one <- prB 2
            v <- if one == 0 then pure 1 else vpLargeValue bd probs p
            sgn <- vpFlag bd
            let v' = if sgn == 1 then negate v else v
                dq = if n > 0 then dqAc else dqDc
            unsafeWrite out (boff + vpZigzag `unsafeAt` n) (v' * dq)
            let n1 = n + 1
            if n1 >= 16 then unsafeWrite res ri 16
                        else loop True n1 (vpPIdx ty n1 (if one == 0 then 1 else 2))

-- Probability-row index for a token at scan position @n@ in context @ctx@.
{-# INLINE vpPIdx #-}
vpPIdx :: Int -> Int -> Int -> Int
vpPIdx ty n ctx = ((ty*8 + vpBands `unsafeAt` n)*3 + ctx)*11

-- Coefficient magnitudes above 4: category trees with fixed extra-bit probs.
vpLargeValue :: VPBool s -> STUArray s Int Int -> Int -> ST s Int
vpLargeValue bd probs p = do
  let prB i = do pv <- rdI probs (p + i); vpGetBool bd pv
  b3 <- prB 3
  if b3 == 0
    then do b4 <- prB 4
            if b4 == 0 then pure 2
                       else do b5 <- prB 5; pure (3 + b5)
    else do
      b6 <- prB 6
      if b6 == 0
        then do b7 <- prB 7
                if b7 == 0
                  then do e <- vpGetBool bd 159; pure (5 + e)
                  else do e1 <- vpGetBool bd 165
                          e0 <- vpGetBool bd 145
                          pure (7 + 2*e1 + e0)
        else do
          bit1 <- prB 8
          bit0 <- prB (9 + bit1)
          let cat = 2*bit1 + bit0
              lo = vpCatStart `unsafeAt` cat
              hi = vpCatStart `unsafeAt` (cat + 1)
              extra !k !acc | k >= hi = pure acc
                            | otherwise = do
                                b <- vpGetBool bd (vpCatProbs `unsafeAt` k)
                                extra (k + 1) (2*acc + b)
          v <- extra lo 0
          pure (v + 3 + (8 `shiftL` cat))

-- The exact integer 4x4 inverse DCT (RFC 6386 section 14.3), added onto the
-- prediction already in the plane. Columns go through a 16-entry scratch
-- buffer; rows are written straight into the plane.
vpIdctAdd :: STUArray s Int Int -> STUArray s Int Word8 -> Int -> Int -> Int
          -> STUArray s Int Int -> Int -> ST s ()
vpIdctAdd tmp pl pw px py coeffs boff = do
  forLoop 0 3 $ \i -> do
    i0  <- unsafeRead coeffs (boff + i)
    i4  <- unsafeRead coeffs (boff + i + 4)
    i8  <- unsafeRead coeffs (boff + i + 8)
    i12 <- unsafeRead coeffs (boff + i + 12)
    let a1 = i0 + i8; b1 = i0 - i8
        c1 = vpM2 i4 - vpM1 i12
        d1 = vpM1 i4 + vpM2 i12
    unsafeWrite tmp i        (a1 + d1)
    unsafeWrite tmp (4 + i)  (b1 + c1)
    unsafeWrite tmp (8 + i)  (b1 - c1)
    unsafeWrite tmp (12 + i) (a1 - d1)
  forLoop 0 3 $ \r -> do
    t0 <- unsafeRead tmp (r*4)
    t1 <- unsafeRead tmp (r*4 + 1)
    t2 <- unsafeRead tmp (r*4 + 2)
    t3 <- unsafeRead tmp (r*4 + 3)
    let a1 = t0 + t2; b1 = t0 - t2
        c1 = vpM2 t1 - vpM1 t3
        d1 = vpM1 t1 + vpM2 t3
        o = (py + r)*pw + px
    vpAddPx pl o       ((a1 + d1 + 4) `shiftR` 3)
    vpAddPx pl (o + 1) ((b1 + c1 + 4) `shiftR` 3)
    vpAddPx pl (o + 2) ((b1 - c1 + 4) `shiftR` 3)
    vpAddPx pl (o + 3) ((a1 - d1 + 4) `shiftR` 3)

-- The same transform when only the DC coefficient can be non-zero: every
-- column but the first is zero, so all sixteen outputs are (dc + 4) >> 3.
vpIdctDcAdd :: STUArray s Int Word8 -> Int -> Int -> Int -> Int -> ST s ()
vpIdctDcAdd pl pw px py dc = do
  let d = (dc + 4) `shiftR` 3
  forLoop 0 3 $ \r -> do
    let o = (py + r)*pw + px
    vpAddPx pl o d; vpAddPx pl (o + 1) d; vpAddPx pl (o + 2) d; vpAddPx pl (o + 3) d

{-# INLINE vpM1 #-}
vpM1, vpM2 :: Int -> Int
vpM1 x = x + ((x * 20091) `shiftR` 16)
{-# INLINE vpM2 #-}
vpM2 x = (x * 35468) `shiftR` 16

{-# INLINE vpAddPx #-}
vpAddPx :: STUArray s Int Word8 -> Int -> Int -> ST s ()
vpAddPx pl o d = do
  old <- unsafeRead pl o
  unsafeWrite pl o (fromIntegral (clamp8 (fromIntegral old + d)))

-- Inverse Walsh-Hadamard transform for the luma DC plane (section 14.3):
-- reads the Y2 block at @boff@ and scatters one DC value into each of the
-- sixteen Y blocks' coefficient 0. Returns whether any of them is non-zero.
vpWhtScatter :: STUArray s Int Int -> STUArray s Int Int -> Int -> ST s Bool
vpWhtScatter tmp coeffs boff = do
  forLoop 0 3 $ \i -> do
    i0  <- unsafeRead coeffs (boff + i)
    i4  <- unsafeRead coeffs (boff + i + 4)
    i8  <- unsafeRead coeffs (boff + i + 8)
    i12 <- unsafeRead coeffs (boff + i + 12)
    let a1 = i0 + i12; b1 = i4 + i8
        c1 = i4 - i8;  d1 = i0 - i12
    unsafeWrite tmp i        (a1 + b1)
    unsafeWrite tmp (4 + i)  (c1 + d1)
    unsafeWrite tmp (8 + i)  (a1 - b1)
    unsafeWrite tmp (12 + i) (d1 - c1)
  let row !r !nz
        | r > 3 = pure nz
        | otherwise = do
            t0 <- unsafeRead tmp (r*4)
            t1 <- unsafeRead tmp (r*4 + 1)
            t2 <- unsafeRead tmp (r*4 + 2)
            t3 <- unsafeRead tmp (r*4 + 3)
            let a1 = t0 + t3; b1 = t1 + t2
                c1 = t1 - t2; d1 = t0 - t3
                v0 = (a1 + b1 + 3) `shiftR` 3
                v1 = (c1 + d1 + 3) `shiftR` 3
                v2 = (a1 - b1 + 3) `shiftR` 3
                v3 = (d1 - c1 + 3) `shiftR` 3
                b = r*4
            unsafeWrite coeffs (b*16)       v0
            unsafeWrite coeffs ((b+1)*16)   v1
            unsafeWrite coeffs ((b+2)*16)   v2
            unsafeWrite coeffs ((b+3)*16)   v3
            row (r + 1) (nz || v0 /= 0 || v1 /= 0 || v2 /= 0 || v3 /= 0)
  row 0 False

-- A reconstructed sample with the frame-border rules: the row above the
-- frame reads 127, the column left of it 129, and reads past the right edge
-- clamp to the last column. A top-level INLINE function rather than a
-- closure, which would box every sample it returned.
{-# INLINE vpRd #-}
vpRd :: STUArray s Int Word8 -> Int -> Int -> Int -> ST s Int
vpRd pl pw x y
  | y < 0     = pure 127
  | x < 0     = pure 129
  | otherwise = fromIntegral <$> unsafeRead pl (y*pw + min x (pw-1))

-- Whole-block intra prediction (16x16 luma or 8x8 chroma): DC / V / H / TM.
vpPredBlock :: STUArray s Int Word8 -> Int
            -> Int -> Int -> Int -> Int -> ST s ()
vpPredBlock pl pw x0 y0 sz mode = case mode of
  0 -> do
    let hasA = y0 > 0; hasL = x0 > 0
        lg = if sz == 16 then 4 else 3
        sumOf f = let go !i !acc | i >= sz = pure acc
                                 | otherwise = do v <- f i; go (i + 1) (acc + v)
                  in go 0 0
    a <- if hasA then sumOf (\i -> vpRd pl pw (x0+i) (y0-1)) else pure 0
    l <- if hasL then sumOf (\i -> vpRd pl pw (x0-1) (y0+i)) else pure 0
    let v | hasA && hasL = (a + l + sz) `shiftR` (lg + 1)
          | hasA         = (a + sz `div` 2) `shiftR` lg
          | hasL         = (l + sz `div` 2) `shiftR` lg
          | otherwise    = 128
        v8 = fromIntegral v :: Word8
    forLoop 0 (sz-1) $ \y -> do
      let o = (y0+y)*pw + x0
      forLoop 0 (sz-1) $ \x -> unsafeWrite pl (o + x) v8
  1 -> forLoop 0 (sz-1) $ \x -> do
         v <- vpRd pl pw (x0+x) (y0-1)
         let v8 = fromIntegral v :: Word8
         forLoop 0 (sz-1) $ \y -> unsafeWrite pl ((y0+y)*pw + x0+x) v8
  2 -> forLoop 0 (sz-1) $ \y -> do
         v <- vpRd pl pw (x0-1) (y0+y)
         let v8 = fromIntegral v :: Word8
             o = (y0+y)*pw + x0
         forLoop 0 (sz-1) $ \x -> unsafeWrite pl (o + x) v8
  _ -> do
    p <- vpRd pl pw (x0-1) (y0-1)
    forLoop 0 (sz-1) $ \y -> do
      lv <- vpRd pl pw (x0-1) (y0+y)
      let o = (y0+y)*pw + x0
      forLoop 0 (sz-1) $ \x -> do
        av <- vpRd pl pw (x0+x) (y0-1)
        unsafeWrite pl (o + x) (fromIntegral (clamp8 (lv + av - p)))

-- 4x4 luma sub-block prediction: the ten B_* modes (section 12.3). The
-- thirteen edge pixels are in @edge@: 0..3 the left column bottom-up
-- (L3 L2 L1 L0), 4 the corner, 5..8 the row above, 9..12 above-right.
vpPredB :: STUArray s Int Int -> STUArray s Int Word8 -> Int -> Int -> Int -> Int
        -> ST s ()
-- The mode is dispatched once per block, each case its own copy of the
-- sample loop (via the inlined 'vpPredBRun'), and B_DC's eight-sample sum is
-- taken once rather than per sample (its average of bytes needs no clamp).
vpPredB edge pl pw x0 y0 mode = case mode of
  0 -> do
    a <- vpE edge 0; b <- vpE edge 1; c <- vpE edge 2; d <- vpE edge 3
    e <- vpE edge 5; f <- vpE edge 6; g <- vpE edge 7; h <- vpE edge 8
    let v = fromIntegral ((a+b+c+d+e+f+g+h+4) `shiftR` 3)
    forLoop 0 15 $ \k -> unsafeWrite pl ((y0+(k `shiftR` 2))*pw + x0 + (k .&. 3)) v
  1 -> vpPredBRun edge pl pw x0 y0 1
  2 -> vpPredBRun edge pl pw x0 y0 2
  3 -> vpPredBRun edge pl pw x0 y0 3
  4 -> vpPredBRun edge pl pw x0 y0 4
  5 -> vpPredBRun edge pl pw x0 y0 5
  6 -> vpPredBRun edge pl pw x0 y0 6
  7 -> vpPredBRun edge pl pw x0 y0 7
  8 -> vpPredBRun edge pl pw x0 y0 8
  _ -> vpPredBRun edge pl pw x0 y0 9

-- One flat loop over the sixteen samples: nested inside another loop's body
-- the inner counter was compiled boxed, one 'I#' per sample.
{-# INLINE vpPredBRun #-}
vpPredBRun :: STUArray s Int Int -> STUArray s Int Word8 -> Int -> Int -> Int -> Int -> ST s ()
vpPredBRun edge pl pw x0 y0 mode =
  forLoop 0 15 $ \k -> do
    let row = k `shiftR` 2; c = k .&. 3
    v <- vpBPix edge mode row c
    unsafeWrite pl ((y0+row)*pw + x0 + c) (fromIntegral (clampI 0 255 v))

-- One predicted sample, read straight from the edge buffer and inlined into
-- the write loop. As local functions over captured edge values these
-- allocated a set of closures on every 4x4 block, and as a separate function
-- returning through 'ST' it boxed every sample.
{-# INLINE vpBPix #-}
vpBPix :: STUArray s Int Int -> Int -> Int -> Int -> ST s Int
vpBPix edge mode row c = case mode of
  0 -> do s <- sumE 0 3; t <- sumE 5 8; pure ((s + t + 4) `shiftR` 3)   -- B_DC
  1 -> do l <- e (3-row); a <- e (5+c); p <- e 4                        -- B_TM
          pure (clamp8 (l + a - p))
  2 -> a3 (4+c) (5+c) (6+c)                                             -- B_VE
  3 -> if row == 3 then a3 1 0 0 else a3 (4-row) (3-row) (2-row)        -- B_HE
  4 -> let d = c - row in a3 (d+3) (d+4) (d+5)                          -- B_RD
  5 -> case row of                                                      -- B_VR
         0 -> a2 (4+c) (5+c)
         1 -> a3 (3+c) (4+c) (5+c)
         2 -> if c == 0 then a3 2 3 4 else a2 (3+c) (4+c)
         _ -> if c == 0 then a3 1 2 3 else a3 (2+c) (3+c) (4+c)
  6 -> let k = row + c                                                  -- B_LD
       in if k < 6 then a3 (5+k) (6+k) (7+k) else a3 11 12 12
  7 -> case row of                                                      -- B_VL
         0 -> a2 (5+c) (6+c)
         1 -> a3 (5+c) (6+c) (7+c)
         2 -> if c < 3 then a2 (6+c) (7+c) else a3 9 10 11
         _ -> if c < 3 then a3 (6+c) (7+c) (8+c) else a3 10 11 12
  -- B_HD and B_HU repeat their own values two columns over, one row down
  -- (HD) or up (HU).
  8 | c >= 2 && row >= 1 -> hd (row-1) (c-2)                          -- B_HD
    | otherwise -> hd row c
  _ | c >= 2 && row < 3 -> hu (row+1) (c-2)                            -- B_HU
    | otherwise -> hu row c
  where
    e = vpE edge
    a3 = vpA3 edge
    a2 = vpA2 edge
    -- At most one step of that repetition is ever needed (c <= 3), so the
    -- two kernels are written without recursion, which keeps this function
    -- inlinable.
    hd r' c' | r' == 0 = case c' of
                 0 -> a2 3 4
                 1 -> a3 3 4 5
                 2 -> a3 4 5 6
                 _ -> a3 5 6 7
             | c' == 0 = a2 (3-r') (4-r')
             | otherwise = a3 (3-r') (4-r') (5-r')
    hu r' c' | r' == 3 || (r' == 2 && c' >= 2) = e 0
             | r' == 2 = if c' == 0 then a2 1 0 else a3 1 0 0
             | c' == 0 = a2 (3-r') (2-r')
             | otherwise = a3 (3-r') (2-r') (1-r')
    sumE lo hi = let go !i !acc | i > hi = pure acc
                                | otherwise = do v <- e i; go (i + 1) (acc + v)
                 in go lo 0

-- The edge readers for 'vpBPix', top-level so that inlining them leaves no
-- closure behind (as @where@-bound helpers they were allocated per sample).
{-# INLINE vpE #-}
vpE :: STUArray s Int Int -> Int -> ST s Int
vpE edge i = unsafeRead edge i

{-# INLINE vpA3 #-}
vpA3 :: STUArray s Int Int -> Int -> Int -> Int -> ST s Int
vpA3 edge i j k = do
  x <- unsafeRead edge i; y <- unsafeRead edge j; z <- unsafeRead edge k
  pure ((x + 2*y + z + 2) `shiftR` 2)

{-# INLINE vpA2 #-}
vpA2 :: STUArray s Int Int -> Int -> Int -> ST s Int
vpA2 edge i j = do
  x <- unsafeRead edge i; y <- unsafeRead edge j
  pure ((x + y + 1) `shiftR` 1)

-- In-loop deblocking: per macroblock, left edge, interior vertical edges,
-- top edge, interior horizontal edges (RFC 6386 section 15). The simple
-- variant touches only luma; the normal variant also filters chroma and the
-- wider macroblock edges.
vpFilterFrame :: STUArray s Int Word8 -> STUArray s Int Word8
              -> STUArray s Int Word8 -> Int -> Int -> Int -> Int
              -> STUArray s Int Int -> STUArray s Int Int -> Int -> Bool
              -> ST s ()
vpFilterFrame yPl uPl vPl yW cW mbW mbH fLevels fInner sharp simple =
  forLoop 0 (mbH-1) $ \mby -> forLoop 0 (mbW-1) $ \mbx -> do
    let mi = mby*mbW + mbx
    level <- rdI fLevels mi
    inner <- rdI fInner mi
    when (level > 0) $ do
      let ilev0 = if sharp > 0
                    then min (9 - sharp)
                             (level `shiftR` (if sharp > 4 then 2 else 1))
                    else level
          ilev  = max 1 ilev0
          mbLim  = 2*level + ilev + 4
          subLim = 2*level + ilev
          hev    = if level >= 40 then 2 else if level >= 15 then 1 else 0
          doInner = inner == 1
          x0 = mbx*16; y0 = mby*16
      if simple
        then do
          when (mbx > 0) $ forLoop 0 15 $ \r ->
            vpSimpleEdge yPl ((y0+r)*yW + x0) 1 mbLim
          when doInner $ forLoop 1 3 $ \k -> forLoop 0 15 $ \r ->
            vpSimpleEdge yPl ((y0+r)*yW + x0 + 4*k) 1 subLim
          when (mby > 0) $ forLoop 0 15 $ \c ->
            vpSimpleEdge yPl (y0*yW + x0+c) yW mbLim
          when doInner $ forLoop 1 3 $ \k -> forLoop 0 15 $ \c ->
            vpSimpleEdge yPl ((y0 + 4*k)*yW + x0+c) yW subLim
        else do
          let cx0 = mbx*8; cy0 = mby*8
          when (mbx > 0) $ do
            forLoop 0 15 $ \r -> vpEdge True yPl ((y0+r)*yW + x0) 1 mbLim ilev hev
            forLoop 0 7 $ \r -> do
              vpEdge True uPl ((cy0+r)*cW + cx0) 1 mbLim ilev hev
              vpEdge True vPl ((cy0+r)*cW + cx0) 1 mbLim ilev hev
          when doInner $ do
            forLoop 1 3 $ \k -> forLoop 0 15 $ \r ->
              vpEdge False yPl ((y0+r)*yW + x0 + 4*k) 1 subLim ilev hev
            forLoop 0 7 $ \r -> do
              vpEdge False uPl ((cy0+r)*cW + cx0+4) 1 subLim ilev hev
              vpEdge False vPl ((cy0+r)*cW + cx0+4) 1 subLim ilev hev
          when (mby > 0) $ do
            forLoop 0 15 $ \c -> vpEdge True yPl (y0*yW + x0+c) yW mbLim ilev hev
            forLoop 0 7 $ \c -> do
              vpEdge True uPl (cy0*cW + cx0+c) cW mbLim ilev hev
              vpEdge True vPl (cy0*cW + cx0+c) cW mbLim ilev hev
          when doInner $ do
            forLoop 1 3 $ \k -> forLoop 0 15 $ \c ->
              vpEdge False yPl ((y0 + 4*k)*yW + x0+c) yW subLim ilev hev
            forLoop 0 7 $ \c -> do
              vpEdge False uPl ((cy0+4)*cW + cx0+c) cW subLim ilev hev
              vpEdge False vPl ((cy0+4)*cW + cx0+c) cW subLim ilev hev

clampS :: Int -> Int
clampS v = clampI (-128) 127 v
{-# INLINE clampS #-}

-- The 2-tap adjustment shared by every filter (common_adjust with outer
-- taps), given the four pixels around the edge.
{-# INLINE vpAdjust2 #-}
vpAdjust2 :: STUArray s Int Word8 -> Int -> Int -> Int -> Int -> Int -> Int -> ST s ()
vpAdjust2 pl o step p1 p0 q0 q1 = do
  let a  = clampS (3*(q0 - p0) + clampS (p1 - q1))
      f  = clampS (a + 4) `shiftR` 3
      e' = clampS (a + 3) `shiftR` 3
  unsafeWrite pl o (fromIntegral (clamp8 (q0 - f)))
  unsafeWrite pl (o-step) (fromIntegral (clamp8 (p0 + e')))

{-# INLINE rdP8 #-}
rdP8 :: STUArray s Int Word8 -> Int -> ST s Int
rdP8 pl i = fromIntegral <$> unsafeRead pl i

vpSimpleEdge :: STUArray s Int Word8 -> Int -> Int -> Int -> ST s ()
vpSimpleEdge pl o step lim = do
  p1 <- rdP8 pl (o-2*step); p0 <- rdP8 pl (o-step)
  q0 <- rdP8 pl o;          q1 <- rdP8 pl (o+step)
  when (4*abs (p0-q0) + abs (p1-q1) <= 2*lim + 1) $ vpAdjust2 pl o step p1 p0 q0 q1

-- Normal filter for one edge position: macroblock edges use the wide 6-tap
-- smoothing, interior edges the 4-tap one; high edge variance falls back to
-- the 2-tap adjustment in both.
vpEdge :: Bool -> STUArray s Int Word8 -> Int -> Int -> Int -> Int -> Int
       -> ST s ()
vpEdge mbEdge pl o step lim ilim hevT = do
  p1 <- rdP8 pl (o-2*step); p0 <- rdP8 pl (o-step)
  q0 <- rdP8 pl o;          q1 <- rdP8 pl (o+step)
  -- The cheap edge-difference test rejects most positions before the outer
  -- taps are read.
  when (4*abs (p0-q0) + abs (p1-q1) <= 2*lim + 1) $ do
    p3 <- rdP8 pl (o-4*step); p2 <- rdP8 pl (o-3*step)
    q2 <- rdP8 pl (o+2*step); q3 <- rdP8 pl (o+3*step)
    let ok = abs (p3-p2) <= ilim && abs (p2-p1) <= ilim && abs (p1-p0) <= ilim
             && abs (q3-q2) <= ilim && abs (q2-q1) <= ilim && abs (q1-q0) <= ilim
        isHev = abs (p1-p0) > hevT || abs (q1-q0) > hevT
    when ok $
      if isHev
        then vpAdjust2 pl o step p1 p0 q0 q1
        else if mbEdge
          then do
            let wv = clampS (clampS (p1 - q1) + 3*(q0 - p0))
                a1 = (27*wv + 63) `shiftR` 7
                a2 = (18*wv + 63) `shiftR` 7
                a3 = (9*wv + 63) `shiftR` 7
            unsafeWrite pl (o-3*step) (fromIntegral (clamp8 (p2 + a3)))
            unsafeWrite pl (o-2*step) (fromIntegral (clamp8 (p1 + a2)))
            unsafeWrite pl (o-step)   (fromIntegral (clamp8 (p0 + a1)))
            unsafeWrite pl o          (fromIntegral (clamp8 (q0 - a1)))
            unsafeWrite pl (o+step)   (fromIntegral (clamp8 (q1 - a2)))
            unsafeWrite pl (o+2*step) (fromIntegral (clamp8 (q2 - a3)))
          else do
            let a  = clampS (3*(q0 - p0))
                a1 = clampS (a + 4) `shiftR` 3
                a2 = clampS (a + 3) `shiftR` 3
                a3 = (a1 + 1) `shiftR` 1
            unsafeWrite pl (o-2*step) (fromIntegral (clamp8 (p1 + a3)))
            unsafeWrite pl (o-step)   (fromIntegral (clamp8 (p0 + a2)))
            unsafeWrite pl o          (fromIntegral (clamp8 (q0 - a1)))
            unsafeWrite pl (o+step)   (fromIntegral (clamp8 (q1 - a3)))

-- YUV 4:2:0 planes to RGBA: "fancy" bilinear chroma upsampling and the
-- BT.601 studio-range conversion, in libwebp's fixed-point arithmetic. The
-- two chroma rows and the clamped column pair are resolved once per row and
-- per pixel respectively; the samples feeding each output pixel are unchanged.
vpToImage :: Int -> Int -> Int -> Int -> UArray Int Word8 -> UArray Int Word8
          -> UArray Int Word8 -> Maybe (UArray Int Word8) -> Image
vpToImage w h yW cW yP uP vP malpha =
  Image w h "WebP" $ runSTUArray $ do
    a <- newBytes (w*h*4)
    let chW = (w + 1) `shiftR` 1; chH = (h + 1) `shiftR` 1
        c8 pl i = fromIntegral (pl `unsafeAt` i) :: Int
    forLoop 0 (h-1) $ \y -> do
      let cy = y `shiftR` 1
          dy = if odd y then 1 else -1
          rA = clampI 0 (chH-1) cy * cW
          rB = clampI 0 (chH-1) (cy + dy) * cW
          yRow = y*yW
          oRow = y*w
      forLoop 0 (w-1) $ \x -> do
        let cx = x `shiftR` 1
            dx = if odd x then 1 else -1
            xA = clampI 0 (chW-1) cx
            xB = clampI 0 (chW-1) (cx + dx)
            up pl = (9 * c8 pl (rA + xA) + 3 * c8 pl (rA + xB)
                     + 3 * c8 pl (rB + xA) + c8 pl (rB + xB) + 8) `shiftR` 4
            yv = c8 yP (yRow + x)
            uv = up uP
            vv = up vP
            mh val co = (val * co) `shiftR` 8
            clip v = if v < 0 then 0 else if v >= 16384 then 255 else v `shiftR` 6
            yy = mh yv 19077
            o = (oRow + x) * 4
        unsafeWrite a o       (fromIntegral (clip (yy + mh vv 26149 - 14234)))
        unsafeWrite a (o + 1) (fromIntegral (clip (yy - mh uv 6419 - mh vv 13320 + 8708)))
        unsafeWrite a (o + 2) (fromIntegral (clip (yy + mh uv 33050 - 17685)))
        unsafeWrite a (o + 3) (case malpha of
                                 Nothing -> 255
                                 Just ap -> ap `unsafeAt` (oRow + x))
    pure a

------------------------------------------------------------------------------
-- Scaling + painting to a character grid

-- | The placement of a @cropW x cropH@ source rectangle inside a @cols x rows@
-- cell grid (sub-pixel canvas is @cols x 2*rows@, since each cell stacks two
-- pixels). Returns @(outW, outH, offX, offY)@ — the fitted size and centring
-- offsets, in sub-pixel/column units. Shared by 'renderImage' and the mouse
-- code that maps a drag rectangle back to source pixels, so the two agree.
--
-- @aspect@ is one sub-pixel's height in units of the cell width
-- ('Cmedit.EditorState.cellAspect'): 1.0 assumes the classic 2:1 cell; when
-- the terminal reports its real cell pixel size the fit compensates, so
-- pictures keep their true proportions in any font.
--
-- @maxScale@ optionally caps the fit scale (in the same cell-widths-per-source-
-- pixel units): @Nothing@ scales the crop to fill the canvas as before, while
-- @Just m@ refuses to enlarge past @m@ — used to pin an image at native size
-- (centred, not blown up) when the terminal's real pixel geometry is known.
viewFit :: Double -> Maybe Double -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
viewFit aspect maxScale cols rows cropW cropH =
  let cols' = max 1 cols
      subH  = max 1 rows * 2
      cw = max 1 cropW; ch = max 1 cropH
      a  = if aspect > 0 then aspect else 1
      -- Uniform physical scale (cell-widths per source pixel), bounded by the
      -- canvas in both directions; a sub-pixel is 1 wide and @a@ tall.
      scFit = min (fromIntegral cols' / fromIntegral cw)
                  (fromIntegral subH * a / fromIntegral ch) :: Double
      sc = case maxScale of Nothing -> scFit; Just m -> min m scFit
      outW = max 1 (min cols' (round (fromIntegral cw * sc)))
      outH = max 1 (min subH  (round (fromIntegral ch * sc / a)))
  in (outW, outH, (cols' - outW) `div` 2, (subH - outH) `div` 2)

-- | Render a sub-rectangle @(cropX, cropY, cropW, cropH)@ (in source pixels) of
-- an image into a @rows x cols@ grid of styled cells, scaled to fit while
-- preserving aspect ratio and centred (@maxScale@ as in 'viewFit' — pass
-- @Just@ to keep a small image at native size instead of enlarging it). Pass
-- the whole image rectangle for the default (unzoomed) view. In 'HalfBlock'
-- mode each cell is a @▀@ glyph whose
-- foreground is the top sub-pixel and background the bottom, doubling vertical
-- resolution; in 'Ascii' mode each cell is a luminance-ramp character tinted
-- with the average colour. The grid is indexed @(row, col)@.
renderImage :: Double -> Maybe Double -> ImgMode -> Int -> Int
            -> (Int, Int, Int, Int) -> Image
            -> Array (Int, Int) Cell
renderImage aspect maxScale mode cols rows (cropX, cropY, cropW, cropH) img =
  listArray ((0,0),(rows-1,cols-1))
    [ cellAt r c | r <- [0 .. rows-1], c <- [0 .. cols-1] ]
  where
    iw = imgW img
    cw = max 1 cropW; ch = max 1 cropH
    (outW, outH, offX, offY) = viewFit aspect maxScale cols rows cw ch
    pix  = imgPix img

    -- Average source RGBA over the box that output sub-pixel (sx,sy) maps from
    -- (within the crop rectangle).
    sample sx sy =
      let x0 = cropX + (sx * cw) `div` outW
          x1 = max (x0+1) (cropX + ((sx+1) * cw) `div` outW)
          y0 = cropY + (sy * ch) `div` outH
          y1 = max (y0+1) (cropY + ((sy+1) * ch) `div` outH)
          (n, sr, sg, sb, sa) = boxSum x0 x1 y0 y1
      in if n == 0 then (0,0,0,0)
         else (sr `div` n, sg `div` n, sb `div` n, sa `div` n)
    boxSum x0 x1 y0 y1 = goY y0 (0,0,0,0,0)
      where
        goY y acc@(n,r,g,b,a)
          | y >= y1   = acc
          | otherwise = goY (y+1) (goX x0 y (n,r,g,b,a))
        goX x y acc@(n,r,g,b,a)
          | x >= x1   = acc
          | otherwise =
              let o = (y*iw + x)*4
              in goX (x+1) y ( n+1
                             , r + fromIntegral (pix ! o)
                             , g + fromIntegral (pix ! (o+1))
                             , b + fromIntegral (pix ! (o+2))
                             , a + fromIntegral (pix ! (o+3)) )

    -- Colour of a sub-pixel at output position (sx,sy), or Nothing if outside
    -- the placed image. Composites alpha over a checkerboard so transparency
    -- shows. Coordinates here are in canvas space (0..cols', 0..subH).
    subColor cx cy =
      let sx = cx - offX; sy = cy - offY
      in if sx < 0 || sx >= outW || sy < 0 || sy >= outH
           then Nothing
           else let (r,g,b,a) = sample sx sy
                    (br,bg,bb) = checker cx cy
                    comp v bgc = (v*a + bgc*(255-a)) `div` 255
                in Just (comp r br, comp g bg, comp b bb)
    checker cx cy = if ((cx `div` 8) + (cy `div` 8)) .&. 1 == 0
                      then (60,60,60) else (40,40,40)

    cellAt r c = case mode of
      HalfBlock ->
        let top = subColor c (2*r)
            bot = subColor c (2*r+1)
        in case (top, bot) of
             (Nothing, Nothing) -> blank
             (Just t, Nothing)  -> Cell '\x2580' (Style (rgb t) Default attrNone)        -- ▀ top only
             (Nothing, Just b)  -> Cell '\x2584' (Style (rgb b) Default attrNone)        -- ▄ bottom only
             (Just t, Just b)   -> Cell '\x2580' (Style (rgb t) (rgb b) attrNone)
      Ascii ->
        case (subColor c (2*r), subColor c (2*r+1)) of
          (Nothing, Nothing) -> blank
          (mt, mb) ->
            let cols2 = [v | Just v <- [mt, mb]]
                (r',g',b') = avgC cols2
                lum = (r'*30 + g'*59 + b'*11) `div` 100
                ch  = ramp lum
            in Cell ch (Style (rgb (r',g',b')) Default attrNone)
    blank = Cell ' ' (Style Default Default attrNone)
    rgb (r,g,b) = ColorRGB (fromIntegral r) (fromIntegral g) (fromIntegral b)
    avgC [] = (0,0,0)
    avgC vs = let n = length vs
                  (sr,sg,sb) = foldr (\(a,b,c) (x,y,z) -> (a+x,b+y,c+z)) (0,0,0) vs
              in (sr `div` n, sg `div` n, sb `div` n)

-- | Area-average a source crop rectangle @(cropX, cropY, cropW, cropH)@ down
-- to exactly @outW x outH@ raw RGBA bytes — the pixel payload for the
-- kitty-graphics / sixel display paths ("Cmedit.Gfx"). The same box-average
-- sampling as 'renderImage', so both views agree on what a pixel looks like.
scaleRGBA :: Image -> (Int, Int, Int, Int) -> Int -> Int -> BS.ByteString
scaleRGBA img (cropX, cropY, cropW, cropH) outW0 outH0 =
  -- Written straight into the result buffer. Building the output as a list of
  -- boxed Word8s and packing it cost 63 MB and 66 ms per 800x600 placement —
  -- paid on every resize, every zoom and every sixel animation frame. The box
  -- sums use unboxed accumulators for the same reason: the previous lazy
  -- 5-tuple allocated once per source pixel.
  fst (BS.unfoldrN (outW * outH * 4) step 0)
  where
    outW = max 1 outW0; outH = max 1 outH0
    cw = max 1 cropW; ch = max 1 cropH
    iw = imgW img
    ih = imgH img
    pix = imgPix img

    step :: Int -> Maybe (Word8, Int)
    step !i =
      let (px, chan) = i `quotRem` 4
          (sy, sx)   = px `quotRem` outW
      in Just (sample sx sy chan, i + 1)

    sample !sx !sy !chan =
      let x0 = cropX + (sx * cw) `div` outW
          x1 = max (x0 + 1) (cropX + ((sx + 1) * cw) `div` outW)
          y0 = cropY + (sy * ch) `div` outH
          y1 = max (y0 + 1) (cropY + ((sy + 1) * ch) `div` outH)
      in boxAvg x0 x1 y0 y1 chan

    -- Average one channel over the source box. Kept per-channel (rather than
    -- computing all four at once) so the result can be produced by unfoldrN
    -- without an intermediate; the box is typically 1-4 pixels.
    boxAvg !x0 !x1 !y0 !y1 !chan = goY y0 0 0
      where
        goY !y !n !acc
          | y >= y1 = if n == 0 then 0 else fromIntegral (acc `div` n)
          | otherwise = let (n', acc') = goX x0 y n acc in goY (y + 1) n' acc'
        goX !x !y !n !acc
          | x >= x1 = (n, acc)
          | x < 0 || x >= iw || y < 0 || y >= ih = goX (x + 1) y n acc
          | otherwise =
              let o = (y * iw + x) * 4 + chan
              in goX (x + 1) y (n + 1) (acc + fromIntegral (pix ! o))

-- ASCII luminance ramp, darkest -> brightest (more ink == brighter pixel on a
-- dark terminal).
rampChars :: String
rampChars = " .:-=+*#%@"

ramp :: Int -> Char
ramp lum =
  let n = length rampChars
      i = (lum * (n-1)) `div` 255
  in rampChars !! max 0 (min (n-1) i)
-- Default DCT-token probabilities, [type][band][ctx][11] flattened.
vpCoeffProbs :: UArray Int Int
vpCoeffProbs = U.listArray (0,1055)
  [ 128,128,128,128,128,128,128,128,128,128,128
  , 128,128,128,128,128,128,128,128,128,128,128
  , 128,128,128,128,128,128,128,128,128,128,128
  , 253,136,254,255,228,219,128,128,128,128,128
  , 189,129,242,255,227,213,255,219,128,128,128
  , 106,126,227,252,214,209,255,255,128,128,128
  , 1,98,248,255,236,226,255,255,128,128,128
  , 181,133,238,254,221,234,255,154,128,128,128
  , 78,134,202,247,198,180,255,219,128,128,128
  , 1,185,249,255,243,255,128,128,128,128,128
  , 184,150,247,255,236,224,128,128,128,128,128
  , 77,110,216,255,236,230,128,128,128,128,128
  , 1,101,251,255,241,255,128,128,128,128,128
  , 170,139,241,252,236,209,255,255,128,128,128
  , 37,116,196,243,228,255,255,255,128,128,128
  , 1,204,254,255,245,255,128,128,128,128,128
  , 207,160,250,255,238,128,128,128,128,128,128
  , 102,103,231,255,211,171,128,128,128,128,128
  , 1,152,252,255,240,255,128,128,128,128,128
  , 177,135,243,255,234,225,128,128,128,128,128
  , 80,129,211,255,194,224,128,128,128,128,128
  , 1,1,255,128,128,128,128,128,128,128,128
  , 246,1,255,128,128,128,128,128,128,128,128
  , 255,128,128,128,128,128,128,128,128,128,128
  , 198,35,237,223,193,187,162,160,145,155,62
  , 131,45,198,221,172,176,220,157,252,221,1
  , 68,47,146,208,149,167,221,162,255,223,128
  , 1,149,241,255,221,224,255,255,128,128,128
  , 184,141,234,253,222,220,255,199,128,128,128
  , 81,99,181,242,176,190,249,202,255,255,128
  , 1,129,232,253,214,197,242,196,255,255,128
  , 99,121,210,250,201,198,255,202,128,128,128
  , 23,91,163,242,170,187,247,210,255,255,128
  , 1,200,246,255,234,255,128,128,128,128,128
  , 109,178,241,255,231,245,255,255,128,128,128
  , 44,130,201,253,205,192,255,255,128,128,128
  , 1,132,239,251,219,209,255,165,128,128,128
  , 94,136,225,251,218,190,255,255,128,128,128
  , 22,100,174,245,186,161,255,199,128,128,128
  , 1,182,249,255,232,235,128,128,128,128,128
  , 124,143,241,255,227,234,128,128,128,128,128
  , 35,77,181,251,193,211,255,205,128,128,128
  , 1,157,247,255,236,231,255,255,128,128,128
  , 121,141,235,255,225,227,255,255,128,128,128
  , 45,99,188,251,195,217,255,224,128,128,128
  , 1,1,251,255,213,255,128,128,128,128,128
  , 203,1,248,255,255,128,128,128,128,128,128
  , 137,1,177,255,224,255,128,128,128,128,128
  , 253,9,248,251,207,208,255,192,128,128,128
  , 175,13,224,243,193,185,249,198,255,255,128
  , 73,17,171,221,161,179,236,167,255,234,128
  , 1,95,247,253,212,183,255,255,128,128,128
  , 239,90,244,250,211,209,255,255,128,128,128
  , 155,77,195,248,188,195,255,255,128,128,128
  , 1,24,239,251,218,219,255,205,128,128,128
  , 201,51,219,255,196,186,128,128,128,128,128
  , 69,46,190,239,201,218,255,228,128,128,128
  , 1,191,251,255,255,128,128,128,128,128,128
  , 223,165,249,255,213,255,128,128,128,128,128
  , 141,124,248,255,255,128,128,128,128,128,128
  , 1,16,248,255,255,128,128,128,128,128,128
  , 190,36,230,255,236,255,128,128,128,128,128
  , 149,1,255,128,128,128,128,128,128,128,128
  , 1,226,255,128,128,128,128,128,128,128,128
  , 247,192,255,128,128,128,128,128,128,128,128
  , 240,128,255,128,128,128,128,128,128,128,128
  , 1,134,252,255,255,128,128,128,128,128,128
  , 213,62,250,255,255,128,128,128,128,128,128
  , 55,93,255,128,128,128,128,128,128,128,128
  , 128,128,128,128,128,128,128,128,128,128,128
  , 128,128,128,128,128,128,128,128,128,128,128
  , 128,128,128,128,128,128,128,128,128,128,128
  , 202,24,213,235,186,191,220,160,240,175,255
  , 126,38,182,232,169,184,228,174,255,187,128
  , 61,46,138,219,151,178,240,170,255,216,128
  , 1,112,230,250,199,191,247,159,255,255,128
  , 166,109,228,252,211,215,255,174,128,128,128
  , 39,77,162,232,172,180,245,178,255,255,128
  , 1,52,220,246,198,199,249,220,255,255,128
  , 124,74,191,243,183,193,250,221,255,255,128
  , 24,71,130,219,154,170,243,182,255,255,128
  , 1,182,225,249,219,240,255,224,128,128,128
  , 149,150,226,252,216,205,255,171,128,128,128
  , 28,108,170,242,183,194,254,223,255,255,128
  , 1,81,230,252,204,203,255,192,128,128,128
  , 123,102,209,247,188,196,255,233,128,128,128
  , 20,95,153,243,164,173,255,203,128,128,128
  , 1,222,248,255,216,213,128,128,128,128,128
  , 168,175,246,252,235,205,255,255,128,128,128
  , 47,116,215,255,211,212,255,255,128,128,128
  , 1,121,236,253,212,214,255,255,128,128,128
  , 141,84,213,252,201,202,255,219,128,128,128
  , 42,80,160,240,162,185,255,205,128,128,128
  , 1,1,255,128,128,128,128,128,128,128,128
  , 244,1,255,128,128,128,128,128,128,128,128
  , 238,1,255,128,128,128,128,128,128,128,128 ]

-- Probability that each token probability is updated in the header.
vpCoeffUpdate :: UArray Int Int
vpCoeffUpdate = U.listArray (0,1055)
  [ 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 176,246,255,255,255,255,255,255,255,255,255
  , 223,241,252,255,255,255,255,255,255,255,255
  , 249,253,253,255,255,255,255,255,255,255,255
  , 255,244,252,255,255,255,255,255,255,255,255
  , 234,254,254,255,255,255,255,255,255,255,255
  , 253,255,255,255,255,255,255,255,255,255,255
  , 255,246,254,255,255,255,255,255,255,255,255
  , 239,253,254,255,255,255,255,255,255,255,255
  , 254,255,254,255,255,255,255,255,255,255,255
  , 255,248,254,255,255,255,255,255,255,255,255
  , 251,255,254,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,253,254,255,255,255,255,255,255,255,255
  , 251,254,254,255,255,255,255,255,255,255,255
  , 254,255,254,255,255,255,255,255,255,255,255
  , 255,254,253,255,254,255,255,255,255,255,255
  , 250,255,254,255,254,255,255,255,255,255,255
  , 254,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 217,255,255,255,255,255,255,255,255,255,255
  , 225,252,241,253,255,255,254,255,255,255,255
  , 234,250,241,250,253,255,253,254,255,255,255
  , 255,254,255,255,255,255,255,255,255,255,255
  , 223,254,254,255,255,255,255,255,255,255,255
  , 238,253,254,254,255,255,255,255,255,255,255
  , 255,248,254,255,255,255,255,255,255,255,255
  , 249,254,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,253,255,255,255,255,255,255,255,255,255
  , 247,254,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,253,254,255,255,255,255,255,255,255,255
  , 252,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,254,254,255,255,255,255,255,255,255,255
  , 253,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,254,253,255,255,255,255,255,255,255,255
  , 250,255,255,255,255,255,255,255,255,255,255
  , 254,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 186,251,250,255,255,255,255,255,255,255,255
  , 234,251,244,254,255,255,255,255,255,255,255
  , 251,251,243,253,254,255,254,255,255,255,255
  , 255,253,254,255,255,255,255,255,255,255,255
  , 236,253,254,255,255,255,255,255,255,255,255
  , 251,253,253,254,254,255,255,255,255,255,255
  , 255,254,254,255,255,255,255,255,255,255,255
  , 254,254,254,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,254,255,255,255,255,255,255,255,255,255
  , 254,254,255,255,255,255,255,255,255,255,255
  , 254,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 254,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 248,255,255,255,255,255,255,255,255,255,255
  , 250,254,252,254,255,255,255,255,255,255,255
  , 248,254,249,253,255,255,255,255,255,255,255
  , 255,253,253,255,255,255,255,255,255,255,255
  , 246,253,253,255,255,255,255,255,255,255,255
  , 252,254,251,254,254,255,255,255,255,255,255
  , 255,254,252,255,255,255,255,255,255,255,255
  , 248,254,253,255,255,255,255,255,255,255,255
  , 253,255,254,254,255,255,255,255,255,255,255
  , 255,251,254,255,255,255,255,255,255,255,255
  , 245,251,254,255,255,255,255,255,255,255,255
  , 253,253,254,255,255,255,255,255,255,255,255
  , 255,251,253,255,255,255,255,255,255,255,255
  , 252,253,254,255,255,255,255,255,255,255,255
  , 255,254,255,255,255,255,255,255,255,255,255
  , 255,252,255,255,255,255,255,255,255,255,255
  , 249,255,254,255,255,255,255,255,255,255,255
  , 255,255,254,255,255,255,255,255,255,255,255
  , 255,255,253,255,255,255,255,255,255,255,255
  , 250,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255
  , 254,255,255,255,255,255,255,255,255,255,255
  , 255,255,255,255,255,255,255,255,255,255,255 ]

-- Key-frame B-mode probabilities, [above][left][9] flattened.
vpBModeProbs :: UArray Int Int
vpBModeProbs = U.listArray (0,899)
  [ 231,120,48,89,115,113,120,152,112
  , 152,179,64,126,170,118,46,70,95
  , 175,69,143,80,85,82,72,155,103
  , 56,58,10,171,218,189,17,13,152
  , 114,26,17,163,44,195,21,10,173
  , 121,24,80,195,26,62,44,64,85
  , 144,71,10,38,171,213,144,34,26
  , 170,46,55,19,136,160,33,206,71
  , 63,20,8,114,114,208,12,9,226
  , 81,40,11,96,182,84,29,16,36
  , 134,183,89,137,98,101,106,165,148
  , 72,187,100,130,157,111,32,75,80
  , 66,102,167,99,74,62,40,234,128
  , 41,53,9,178,241,141,26,8,107
  , 74,43,26,146,73,166,49,23,157
  , 65,38,105,160,51,52,31,115,128
  , 104,79,12,27,217,255,87,17,7
  , 87,68,71,44,114,51,15,186,23
  , 47,41,14,110,182,183,21,17,194
  , 66,45,25,102,197,189,23,18,22
  , 88,88,147,150,42,46,45,196,205
  , 43,97,183,117,85,38,35,179,61
  , 39,53,200,87,26,21,43,232,171
  , 56,34,51,104,114,102,29,93,77
  , 39,28,85,171,58,165,90,98,64
  , 34,22,116,206,23,34,43,166,73
  , 107,54,32,26,51,1,81,43,31
  , 68,25,106,22,64,171,36,225,114
  , 34,19,21,102,132,188,16,76,124
  , 62,18,78,95,85,57,50,48,51
  , 193,101,35,159,215,111,89,46,111
  , 60,148,31,172,219,228,21,18,111
  , 112,113,77,85,179,255,38,120,114
  , 40,42,1,196,245,209,10,25,109
  , 88,43,29,140,166,213,37,43,154
  , 61,63,30,155,67,45,68,1,209
  , 100,80,8,43,154,1,51,26,71
  , 142,78,78,16,255,128,34,197,171
  , 41,40,5,102,211,183,4,1,221
  , 51,50,17,168,209,192,23,25,82
  , 138,31,36,171,27,166,38,44,229
  , 67,87,58,169,82,115,26,59,179
  , 63,59,90,180,59,166,93,73,154
  , 40,40,21,116,143,209,34,39,175
  , 47,15,16,183,34,223,49,45,183
  , 46,17,33,183,6,98,15,32,183
  , 57,46,22,24,128,1,54,17,37
  , 65,32,73,115,28,128,23,128,205
  , 40,3,9,115,51,192,18,6,223
  , 87,37,9,115,59,77,64,21,47
  , 104,55,44,218,9,54,53,130,226
  , 64,90,70,205,40,41,23,26,57
  , 54,57,112,184,5,41,38,166,213
  , 30,34,26,133,152,116,10,32,134
  , 39,19,53,221,26,114,32,73,255
  , 31,9,65,234,2,15,1,118,73
  , 75,32,12,51,192,255,160,43,51
  , 88,31,35,67,102,85,55,186,85
  , 56,21,23,111,59,205,45,37,192
  , 55,38,70,124,73,102,1,34,98
  , 125,98,42,88,104,85,117,175,82
  , 95,84,53,89,128,100,113,101,45
  , 75,79,123,47,51,128,81,171,1
  , 57,17,5,71,102,57,53,41,49
  , 38,33,13,121,57,73,26,1,85
  , 41,10,67,138,77,110,90,47,114
  , 115,21,2,10,102,255,166,23,6
  , 101,29,16,10,85,128,101,196,26
  , 57,18,10,102,102,213,34,20,43
  , 117,20,15,36,163,128,68,1,26
  , 102,61,71,37,34,53,31,243,192
  , 69,60,71,38,73,119,28,222,37
  , 68,45,128,34,1,47,11,245,171
  , 62,17,19,70,146,85,55,62,70
  , 37,43,37,154,100,163,85,160,1
  , 63,9,92,136,28,64,32,201,85
  , 75,15,9,9,64,255,184,119,16
  , 86,6,28,5,64,255,25,248,1
  , 56,8,17,132,137,255,55,116,128
  , 58,15,20,82,135,57,26,121,40
  , 164,50,31,137,154,133,25,35,218
  , 51,103,44,131,131,123,31,6,158
  , 86,40,64,135,148,224,45,183,128
  , 22,26,17,131,240,154,14,1,209
  , 45,16,21,91,64,222,7,1,197
  , 56,21,39,155,60,138,23,102,213
  , 83,12,13,54,192,255,68,47,28
  , 85,26,85,85,128,128,32,146,171
  , 18,11,7,63,144,171,4,4,246
  , 35,27,10,146,174,171,12,26,128
  , 190,80,35,99,180,80,126,54,45
  , 85,126,47,87,176,51,41,20,32
  , 101,75,128,139,118,146,116,128,85
  , 56,41,15,176,236,85,37,9,62
  , 71,30,17,119,118,255,17,18,138
  , 101,38,60,138,55,70,43,26,142
  , 146,36,19,30,171,255,97,27,20
  , 138,45,61,62,219,1,81,188,64
  , 32,41,20,117,151,142,20,21,163
  , 112,19,12,61,195,128,48,4,24 ]

vpDcQ :: UArray Int Int
vpDcQ = U.listArray (0,127)
  [ 4,5,6,7,8,9,10,10,11,12,13,14,15,16,17,17
  , 18,19,20,20,21,21,22,22,23,23,24,25,25,26,27,28
  , 29,30,31,32,33,34,35,36,37,37,38,39,40,41,42,43
  , 44,45,46,46,47,48,49,50,51,52,53,54,55,56,57,58
  , 59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74
  , 75,76,76,77,78,79,80,81,82,83,84,85,86,87,88,89
  , 91,93,95,96,98,100,101,102,104,106,108,110,112,114,116,118
  , 122,124,126,128,130,132,134,136,138,140,143,145,148,151,154,157 ]

vpAcQ :: UArray Int Int
vpAcQ = U.listArray (0,127)
  [ 4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
  , 20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35
  , 36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51
  , 52,53,54,55,56,57,58,60,62,64,66,68,70,72,74,76
  , 78,80,82,84,86,88,90,92,94,96,98,100,102,104,106,108
  , 110,112,114,116,119,122,125,128,131,134,137,140,143,146,149,152
  , 155,158,161,164,167,170,173,177,181,185,189,193,197,201,205,209
  , 213,217,221,225,229,234,239,245,249,254,259,264,269,274,279,284 ]

-- VP8L short-distance map, packed (dy<<4)|(8-dx).
vlDistMap :: UArray Int Int
vlDistMap = U.listArray (0,119)
  [ 24,7,23,25,40,6,39,41,22,26,38,42,56,5,55,57
  , 21,27,54,58,37,43,72,4,71,73,20,28,53,59,70,74
  , 36,44,88,69,75,52,60,3,87,89,19,29,86,90,35,45
  , 68,76,85,91,51,61,104,2,103,105,18,30,102,106,34,46
  , 84,92,67,77,101,107,50,62,120,1,119,121,83,93,17,31
  , 100,108,66,78,118,122,33,47,117,123,49,63,99,109,82,94
  , 0,116,124,65,79,16,32,98,110,48,115,125,81,95,64,114
  , 126,97,111,80,113,127,96,112 ]
