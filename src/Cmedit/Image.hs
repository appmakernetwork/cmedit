{-# LANGUAGE RankNTypes #-}
-- | Self-contained raster-image support, written from first principles using
-- only GHC boot libraries (no @JuicyPixels@, no @zlib@ — the DEFLATE, LZW,
-- Huffman and IDCT machinery below is all hand-rolled). The editor uses this to
-- offer a read-only image *view* mode (see 'Cmedit.Editor' @edImage@), the way
-- CSV files get a table view — handy for glancing at an image over SSH where a
-- real graphical viewer is not available.
--
-- The module decodes BMP, PNM (PPM/PGM/PBM), GIF, PNG, baseline JPEG and WebP
-- (both lossless VP8L and lossy VP8, including the ALPH alpha channel) into a
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
  , sniffImage
  , renderImage
  , viewFit
  , scaleRGBA
  ) where

import Control.Monad (when, unless, forM_, foldM)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, listArray)
import qualified Data.Array as A
import Data.Array.ST (STArray, STUArray, newArray, readArray, writeArray, runSTUArray)
import Data.Array.MArray (freeze)
import Data.Array.Unboxed (UArray, (!), bounds)
import qualified Data.Array.Unboxed as U
import Data.Bits
import Data.Char (ord)
import Data.STRef
import Data.Word (Word8, Word16, Word32)
import qualified Data.ByteString as BS

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
  _           -> Left "unrecognised image format"

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
      px x y =
        let base = srcRow y in case bpp of
          24 -> let o = base + x*3 in (at bs (o+2), at bs (o+1), at bs o, 255)
          32 -> let o = base + x*4 in (at bs (o+2), at bs (o+1), at bs o, 255)
          8  -> let idx = at bs (base + x); (r,g,b) = palAt idx in (r,g,b,255)
          4  -> let byte = at bs (base + (x `div` 2))
                    idx  = if even x then byte `shiftR` 4 else byte .&. 0x0F
                    (r,g,b) = palAt idx in (r,g,b,255)
          1  -> let byte = at bs (base + (x `div` 8))
                    idx  = (byte `shiftR` (7 - (x `mod` 8))) .&. 1
                    (r,g,b) = palAt idx in (r,g,b,255)
          _  -> (0,0,0,255)
  pure $ buildImage "BMP" w h $ \a ->
    forM_ [0 .. h-1] $ \y -> forM_ [0 .. w-1] $ \x -> do
      let (r,g,b,al) = px x y
      putRGBA a w x y r g b al

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
  let scale v = if maxv == 255 then v
                else if maxv <= 0 then 0
                else (v * 255) `div` maxv
  case kind of
    6 -> binaryPPM bs pos w h maxv scale False
    5 -> binaryPPM bs pos w h maxv scale True
    4 -> binaryPBM bs pos w h
    3 -> asciiPNM bs pos w h scale False
    2 -> asciiPNM bs pos w h scale True
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

binaryPPM :: BS.ByteString -> Int -> Int -> Int -> Int -> (Int -> Int) -> Bool
          -> Either String Image
binaryPPM bs pos w h maxv scale gray = do
  let bytesPer = if maxv > 255 then 2 else 1
      chans    = if gray then 1 else 3
      need     = w * h * chans * bytesPer
  when' (pos + need > BS.length bs) "truncated PNM raster"
  let rd o = if bytesPer == 2 then be16 bs o else at bs o
  pure $ buildImage "PNM" w h $ \a ->
    forM_ [0 .. h-1] $ \y -> forM_ [0 .. w-1] $ \x -> do
      let o = pos + (y*w + x) * chans * bytesPer
      if gray
        then do let v = scale (rd o) in putRGBA a w x y v v v 255
        else do let r = scale (rd o)
                    g = scale (rd (o + bytesPer))
                    b = scale (rd (o + 2*bytesPer))
                putRGBA a w x y r g b 255

binaryPBM :: BS.ByteString -> Int -> Int -> Int -> Either String Image
binaryPBM bs pos w h = do
  let rowBytes = (w + 7) `div` 8
  when' (pos + rowBytes*h > BS.length bs) "truncated PBM raster"
  pure $ buildImage "PNM" w h $ \a ->
    forM_ [0 .. h-1] $ \y -> forM_ [0 .. w-1] $ \x -> do
      let byte = at bs (pos + y*rowBytes + x `div` 8)
          bit  = (byte `shiftR` (7 - x `mod` 8)) .&. 1
          v    = if bit == 1 then 0 else 255   -- 1 = black in PBM
      putRGBA a w x y v v v 255

-- ASCII PNM (P2/P3): read w*h*chans whitespace-separated samples.
asciiPNM :: BS.ByteString -> Int -> Int -> Int -> (Int -> Int) -> Bool
         -> Either String Image
asciiPNM bs pos w h scale gray = do
  let chans = if gray then 1 else 3
      total = w * h * chans
  vals <- readAsciiSamples bs pos total
  let arr = listArray (0, total-1) (map scale vals) :: Array Int Int
  pure $ buildImage "PNM" w h $ \a ->
    forM_ [0 .. h-1] $ \y -> forM_ [0 .. w-1] $ \x -> do
      let o = (y*w + x) * chans
      if gray then do let v = arr A.! o in putRGBA a w x y v v v 255
              else putRGBA a w x y (arr A.! o) (arr A.! (o+1)) (arr A.! (o+2)) 255

asciiPBM :: BS.ByteString -> Int -> Int -> Int -> Either String Image
asciiPBM bs pos w h = do
  let total = w*h
  vals <- readAsciiSamples bs pos total
  let arr = listArray (0, total-1) vals :: Array Int Int
  pure $ buildImage "PNM" w h $ \a ->
    forM_ [0 .. h-1] $ \y -> forM_ [0 .. w-1] $ \x -> do
      let v = if (arr A.! (y*w+x)) == 1 then 0 else 255
      putRGBA a w x y v v v 255

readAsciiSamples :: BS.ByteString -> Int -> Int -> Either String [Int]
readAsciiSamples bs pos n = go pos n []
  where
    len = BS.length bs
    go _ 0 acc = Right (reverse acc)
    go i k acc =
      let i1 = skip i
      in if i1 >= len then Left "truncated ASCII PNM raster"
         else let (v,i2) = num i1 0 in go i2 (k-1) (v:acc)
    skip i | i >= len = i
           | c == ord '#' = skip (skipLine i)
           | isWs c = skip (i+1)
           | otherwise = i
      where c = at bs i
    skipLine i | i >= len = i | at bs i == 10 = i+1 | otherwise = skipLine (i+1)
    num i acc | i < len && d (at bs i) = num (i+1) (acc*10 + at bs i - ord '0')
              | otherwise = (acc,i)
    d c = c >= ord '0' && c <= ord '9'
    isWs c = c==32||c==9||c==10||c==13||c==11||c==12

------------------------------------------------------------------------------
-- Stubs filled in by later slices (kept here so the module type-checks while
-- the harder decoders are written).

------------------------------------------------------------------------------
-- GIF (87a/89a) — LZW-compressed, palette-indexed. We render the first frame.

decodeGIF :: BS.ByteString -> Either String Image
decodeGIF bs = do
  when' (BS.length bs < 13) "truncated GIF header"
  let w = le16 bs 6
      h = le16 bs 8
      packed = at bs 10
      gctFlag = testBit packed 7
      gctSize = 1 `shiftL` ((packed .&. 7) + 1)
      gctOff  = 13
      p0 = 13 + (if gctFlag then 3*gctSize else 0)
      gct = (gctFlag, gctOff)
  checkDims w h
  parseBlocks bs w h gct p0 Nothing

-- Walk top-level GIF blocks until the first image; thread the most recent
-- Graphic Control Extension's transparent-colour index.
parseBlocks :: BS.ByteString -> Int -> Int -> (Bool, Int) -> Int -> Maybe Int
            -> Either String Image
parseBlocks bs w h gct pos mtrans
  | pos >= BS.length bs = Left "GIF ended before any image"
  | otherwise = case at bs pos of
      0x3B -> Left "GIF has no image data"
      0x21 ->                                   -- extension
        let label = atSafe bs (pos+1)
        in if label == 0xF9 && atSafe bs (pos+2) == 4
             then let p2 = at bs (pos+3)
                      trans = if testBit p2 0 then Just (at bs (pos+6)) else Nothing
                  in parseBlocks bs w h gct (skipSubBlocks bs (pos+2)) trans
             else parseBlocks bs w h gct (skipSubBlocks bs (pos+2)) mtrans
      0x2C -> decodeGifImage bs w h gct pos mtrans
      _    -> Left ("unexpected GIF block 0x" ++ show (at bs pos))

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

decodeGifImage :: BS.ByteString -> Int -> Int -> (Bool, Int) -> Int -> Maybe Int
               -> Either String Image
decodeGifImage bs sw sh (gctFlag, gctOff) pos mtrans = do
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
        | gctFlag   = gctOff
        | otherwise = -1
  when' (iw <= 0 || ih <= 0) "GIF image has zero size"
  when' (not lctFlag && not gctFlag) "GIF has no colour table"
  let minCode = at bs dataOff0
  when' (minCode < 1 || minCode > 11) "bad GIF LZW code size"
  let (lzwData, _) = collectSubBlocksBS bs (dataOff0 + 1)
      idxRaw = lzwDecode minCode lzwData (iw*ih)
      -- LZW output rows are in storage order; for an interlaced image that is
      -- the 4-pass sequence, so map each actual raster row to its stored row.
      rowOrder = if interlace then gifInterlaceOrder ih else [0 .. ih-1]
      invRow   = A.array (0, ih-1) (zip rowOrder [0 ..]) :: A.Array Int Int
      idxAt x y = let d = invRow A.! y
                  in if x < iw && d < ih then idxRaw ! (d*iw + x) else 0
      colAt k = let o = palOff + k*3
                in if palOff >= 0 && o+2 < BS.length bs
                     then (at bs o, at bs (o+1), at bs (o+2)) else (0,0,0)
  -- Composite the (possibly offset) frame onto the logical screen.
  pure $ buildImage "GIF" sw sh $ \a ->
    forM_ [0 .. sh-1] $ \y -> forM_ [0 .. sw-1] $ \x -> do
      let ix = x - left; iy = y - top
      if ix >= 0 && ix < iw && iy >= 0 && iy < ih
        then do let k = idxAt ix iy
                    (r,g,b) = colAt k
                    al = if mtrans == Just k then 0 else 255
                putRGBA a sw x y r g b al
        else putRGBA a sw x y 0 0 0 0

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
lzwDecode :: Int -> BS.ByteString -> Int -> UArray Int Int
lzwDecode minCode dat count = runSTUArray $ do
  out    <- newArray (0, max 0 (count-1)) 0 :: ST s (STUArray s Int Int)
  prefix <- newArray (0, 4095) (-1)         :: ST s (STUArray s Int Int)
  firstB <- newArray (0, 4095) 0            :: ST s (STUArray s Int Int)
  stack  <- newArray (0, 4096) 0            :: ST s (STUArray s Int Int)
  let clear = 1 `shiftL` minCode
      end   = clear + 1
  forM_ [0 .. clear-1] $ \c -> writeArray firstB c c   -- base codes are literals
  let _ = ()
      nbits = 8 * BS.length dat
      readCode bitpos size =
        let byteOff = bitpos `shiftR` 3
            bitOff  = bitpos .&. 7
            b k = maybe 0 id (atM dat (byteOff+k))
            raw = b 0 .|. (b 1 `shiftL` 8) .|. (b 2 `shiftL` 16)
        in (raw `shiftR` bitOff) .&. ((1 `shiftL` size) - 1)
      -- emit the sequence for `code`; returns its first index (the base value)
      emit out' code outPos = do
        let push depth c
              | c < 0 = pure depth
              | otherwise = do
                  fb <- readArray firstB c
                  writeArray stack depth fb
                  pr <- readArray prefix c
                  push (depth+1) pr
        d <- push 0 code
        -- stack[d-1] is the base (first) index; pop in reverse to output
        let popOut j p
              | j < 0 = pure p
              | otherwise = do
                  v <- readArray stack j
                  when (p < count) $ writeArray out' p v
                  popOut (j-1) (p+1)
        p' <- popOut (d-1) outPos
        base <- readArray stack (d-1)
        pure (p', base)
  let loop bitpos size next prev outPos
        | outPos >= count = pure ()
        | bitpos + size > nbits = pure ()
        | otherwise = do
            let code = readCode bitpos size
                bp'  = bitpos + size
            if code == clear
              then loop bp' (minCode+1) (clear+2) (-1) outPos
            else if code == end
              then pure ()
            else if prev < 0
              then do
                -- first code: must be a base entry
                when (outPos < count) $ writeArray out outPos code
                loop bp' size next code (outPos+1)
            else do
              (outPos', k) <-
                if code < next
                  then emit out code outPos
                  else do
                    -- special case: code == next ; entry = seq[prev]+firstIndex(prev)
                    kp <- firstIndexOf prefix firstB prev
                    (p1,_) <- emit out prev outPos
                    let p2 = p1
                    when (p2 < count) $ writeArray out p2 kp
                    pure (p2+1, kp)
              -- add new entry seq[prev] + k
              if next <= 4095
                then do
                  writeArray prefix next prev
                  writeArray firstB next k
                  let next' = next+1
                      size' = if next' == (1 `shiftL` size) && size < 12 then size+1 else size
                  loop bp' size' next' code outPos'
                else loop bp' size next code outPos'
  loop 0 (minCode+1) (clear+2) (-1) 0
  pure out

-- First index (base value) of a dictionary code, by following prefix links.
firstIndexOf :: STUArray s Int Int -> STUArray s Int Int -> Int -> ST s Int
firstIndexOf prefix firstB = go
  where go c = do
          pr <- readArray prefix c
          if pr < 0 then readArray firstB c else go pr

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
  raw <- inflate idat 2 rawSize        -- skip the 2-byte zlib header
  let scale v  = v * 255 `div` ((1 `shiftL` bd) - 1)
      palAt i  = let o = i*3 in if o+2 < BS.length plte
                                  then (at plte o, at plte (o+1), at plte (o+2))
                                  else (0,0,0)
      palAlpha i = if i < BS.length trns then at trns i else 255
      grayTrans  = if ct == 0 && BS.length trns >= 2 then Just (be16 trns 0) else Nothing
      rgbTrans   = if ct == 2 && BS.length trns >= 6
                     then Just (be16 trns 0, be16 trns 2, be16 trns 4) else Nothing
  pure $ buildImage "PNG" w h $ \a -> do
    _ <- foldM (\off (x0,y0,dx,dy,pw,ph) ->
            if pw <= 0 || ph <= 0 then pure off else do
              let rb = rowBytes pw
              recon <- newArray (0, max 0 (ph*rb - 1)) 0 :: ST s (STUArray s Int Word8)
              -- Unfilter every scanline of this pass.
              forM_ [0 .. ph-1] $ \ry -> do
                let lineOff = off + ry*(1+rb)
                    ft = ux raw lineOff
                forM_ [0 .. rb-1] $ \i -> do
                  let x = ux raw (lineOff+1+i)
                  av <- if i >= bpp then fromIntegral <$> rd recon (ry*rb + i - bpp) else pure 0
                  bv <- if ry > 0   then fromIntegral <$> rd recon ((ry-1)*rb + i) else pure 0
                  cv <- if ry > 0 && i >= bpp then fromIntegral <$> rd recon ((ry-1)*rb + i - bpp) else pure (0 :: Int)
                  let v = (x + paethPredict ft av bv cv) .&. 0xff
                  writeArray recon (ry*rb + i) (fromIntegral v)
              -- Convert + place each pixel of this pass into the image.
              forM_ [0 .. ph-1] $ \ry -> forM_ [0 .. pw-1] $ \px -> do
                (r,g,b,al) <- pngPixel recon (ry*rb) bd ct chans scale palAt palAlpha
                                       grayTrans rgbTrans px
                putRGBA a w (x0 + px*dx) (y0 + ry*dy) r g b al
              pure (off + ph*(1+rb)))
          0 passes
    pure ()

-- Convert one pixel (component samples) from an unfiltered scanline to RGBA.
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

rd :: STUArray s Int Word8 -> Int -> ST s Word8
rd = readArray

-- Safe read from the inflated raw byte array (0 past the end).
ux :: UArray Int Word8 -> Int -> Int
ux a i = if i >= 0 && i <= hi then fromIntegral (a ! i) else 0
  where hi = snd (bounds a)

paethPredict :: Int -> Int -> Int -> Int -> Int
paethPredict ft a b c = case ft of
  0 -> 0
  1 -> a
  2 -> b
  3 -> (a + b) `div` 2
  4 -> let p  = a + b - c
           pa = abs (p - a); pb = abs (p - b); pc = abs (p - c)
       in if pa <= pb && pa <= pc then a else if pb <= pc then b else c
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
-- DEFLATE / inflate (RFC 1951), hand-rolled. Reused by PNG (and available to
-- anything else needing zlib decompression).

-- Canonical Huffman table built from per-symbol code lengths.
data Huff = Huff !(Array Int Int) !(Array Int Int) !Int  -- counts[len], symbols, maxLen

buildHuff :: [Int] -> Huff
buildHuff lens =
  let maxLen = maximum (0 : lens)
      counts = A.accumArray (+) 0 (0, max 1 maxLen) [(l,1) | l <- lens, l > 0]
      syms   = [ s | l <- [1..maxLen], (s,ll) <- zip [0..] lens, ll == l ]
      nsym   = length syms
  in Huff counts (A.listArray (0, max 0 (nsym-1)) (syms ++ [0])) maxLen

-- | Inflate a raw DEFLATE stream starting at byte @startByte@, producing up to
-- @outSize@ bytes (the caller knows the size; PNG does). A hard error (invalid
-- Huffman code or block type) yields @Left@ so corrupt input is reported up
-- front rather than rendered as garbage; a merely-short stream is tolerated
-- (zero-padded) so slightly-truncated-but-valid files still display.
inflate :: BS.ByteString -> Int -> Int -> Either String (UArray Int Word8)
inflate dat startByte outSize = runST $ do
  out    <- newArray (0, max 0 (outSize-1)) 0 :: ST s (STUArray s Int Word8)
  bitRef <- newSTRef (startByte * 8)
  outRef <- newSTRef 0
  doneRef <- newSTRef False
  errRef  <- newSTRef (Nothing :: Maybe String)
  let fail' msg = writeSTRef errRef (Just msg) >> writeSTRef doneRef True
      getBit = do
        p <- readSTRef bitRef
        let byte = maybe 0 id (atM dat (p `shiftR` 3))
            b    = (byte `shiftR` (p .&. 7)) .&. 1
        writeSTRef bitRef (p+1)
        pure b
      getBits n = go 0 0
        where go i acc | i >= n = pure acc
                       | otherwise = do b <- getBit; go (i+1) (acc .|. (b `shiftL` i))
      putByte w = do
        o <- readSTRef outRef
        when (o < outSize) $ writeArray out o w
        writeSTRef outRef (o+1)
      copyBack dist len = forM_ [1..len] $ \_ -> do
        o <- readSTRef outRef
        v <- if o - dist >= 0 && o - dist < outSize then readArray out (o-dist) else pure 0
        putByte v
      decodeSym (Huff counts syms maxLen) = go 1 0 0 0
        where go len code first index
                | len > maxLen = pure (-1)
                | otherwise = do
                    b <- getBit
                    let code1 = code .|. b
                        cnt   = counts A.! len
                    if code1 - first < cnt
                      then pure (syms A.! (index + (code1 - first)))
                      else go (len+1) (code1 `shiftL` 1) ((first+cnt) `shiftL` 1) (index+cnt)
      huffBlock lit dist = loop
        where loop = do
                o <- readSTRef outRef
                if o >= outSize then pure () else do
                  sym <- decodeSym lit
                  if sym < 0 then fail' "invalid Huffman code"
                  else if sym < 256 then putByte (fromIntegral sym) >> loop
                  else if sym == 256 then pure ()
                  else do
                    let li = sym - 257
                    if li >= length lenBase then fail' "invalid length code" else do
                      extra <- getBits (lenExtra !! li)
                      let len = lenBase !! li + extra
                      dsym <- decodeSym dist
                      if dsym < 0 || dsym >= length distBase then fail' "invalid distance code" else do
                        dextra <- getBits (distExtra !! dsym)
                        let d = distBase !! dsym + dextra
                        copyBack d len
                        loop
      storedBlock = do
        p <- readSTRef bitRef
        writeSTRef bitRef ((p + 7) .&. complement 7)   -- align to byte
        len <- getBits 16
        _nlen <- getBits 16
        forM_ [1..len] $ \_ -> do v <- getBits 8; putByte (fromIntegral v)
      readDynamic = do
        hlit  <- getBits 5
        hdist <- getBits 5
        hclen <- getBits 4
        let nlit = hlit + 257; ndist = hdist + 1; nclen = hclen + 4
        clRaw <- mapM (const (getBits 3)) [1..nclen]
        let clLens = A.elems (A.accumArray (\_ x -> x) 0 (0,18)
                       (zip (take nclen clOrder) clRaw)) :: [Int]
            clHuff = buildHuff clLens
        allLens <- decodeLens clHuff (nlit + ndist)
        let (litLens, distLens) = splitAt nlit allLens
        pure (buildHuff litLens, buildHuff distLens)
      decodeLens clHuff total = reverse <$> go [] 0
        where go acc n
                | n >= total = pure acc
                | otherwise = do
                    s <- decodeSym clHuff
                    if s < 0 then pure acc
                    else if s < 16 then go (s:acc) (n+1)
                    else if s == 16 then do
                      r <- getBits 2
                      let prev = case acc of (p:_) -> p; [] -> 0
                          k = r + 3
                      go (replicate k prev ++ acc) (n+k)
                    else if s == 17 then do
                      r <- getBits 3; let k = r + 3 in go (replicate k 0 ++ acc) (n+k)
                    else do
                      r <- getBits 7; let k = r + 11 in go (replicate k 0 ++ acc) (n+k)
      blockLoop = do
        done <- readSTRef doneRef
        o <- readSTRef outRef
        if done || o >= outSize then pure () else do
          bfinal <- getBit
          btype  <- getBits 2
          case btype of
            0 -> storedBlock
            1 -> huffBlock fixedLit fixedDist
            2 -> do (lit,dist) <- readDynamic; huffBlock lit dist
            _ -> fail' "invalid DEFLATE block type"
          err <- readSTRef errRef
          if bfinal == 1 || err /= Nothing then pure () else blockLoop
  blockLoop
  err <- readSTRef errRef
  frozen <- freeze out
  pure (maybe (Right frozen) Left err)

-- Fixed Huffman tables (RFC 1951 §3.2.6).
fixedLit :: Huff
fixedLit = buildHuff ([8 | _ <- [0..143]] ++ [9 | _ <- [144..255]]
                   ++ [7 | _ <- [256..279]] ++ [8 | _ <- [280..287]])

fixedDist :: Huff
fixedDist = buildHuff (replicate 30 5)

clOrder :: [Int]
clOrder = [16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15]

lenBase, lenExtra, distBase, distExtra :: [Int]
lenBase   = [3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258]
lenExtra  = [0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0]
distBase  = [1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769
            ,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577]
distExtra = [0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13]

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

-- The natural-order index for each zig-zag position.
zigzag :: UArray Int Int
zigzag = U.listArray (0,63)
  [ 0, 1, 8,16, 9, 2, 3,10,17,24,32,25,18,11, 4, 5
  ,12,19,26,33,40,48,41,34,27,20,13, 6, 7,14,21,28
  ,35,42,49,56,57,50,43,36,29,22,15,23,30,37,44,51
  ,58,59,52,45,38,31,39,46,53,60,61,54,47,55,62,63]

-- Precomputed IDCT cosine factors: ct[k*8+u] = C(u)*cos((2k+1)*u*pi/16).
ct :: A.Array Int Double
ct = A.listArray (0,63)
  [ cu u * cos ((fromIntegral (2*k+1) * fromIntegral u * pi) / 16)
  | k <- [0..7], u <- [0..7] ]
  where cu u = if u == 0 then 1 / sqrt 2 else 1

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
    coefs  <- mapM (\(bpr,bpc,_,_) -> newArray (0, max 0 (bpr*bpc*64-1)) 0
                       :: ST s (STUArray s Int Int)) compInfo
    planes <- mapM (\c -> let (pw,ph)=planeDims c
                          in do a <- newArray (0, max 0 (pw*ph-1)) 0 :: ST s (STUArray s Int Word8)
                                pure (pw,ph,a)) comps
    -- Mutable decode tables (DQT/DHT may appear or change between scans).
    quantT <- newArray (0,3) (U.listArray (0,63) (replicate 64 1))
                :: ST s (STArray s Int (UArray Int Int))
    dcT    <- newArray (0,3) emptyHuff :: ST s (STArray s Int Huff)
    acT    <- newArray (0,3) emptyHuff :: ST s (STArray s Int Huff)
    driRef <- newSTRef 0
    -- Entropy bit reader.
    posRef  <- newSTRef 2
    bbufRef <- newSTRef 0; bcntRef <- newSTRef 0; mkRef <- newSTRef (0 :: Int)
    let loadByte = do
          mk <- readSTRef mkRef
          if mk /= 0 then pure () else do
            p <- readSTRef posRef
            case atM bs p of
              Nothing -> writeSTRef mkRef 0xD9
              Just b
                | b == 0xFF -> case atM bs (p+1) of
                    Just 0  -> do writeSTRef posRef (p+2); writeSTRef bbufRef 0xFF; writeSTRef bcntRef 8
                    Just nn -> writeSTRef mkRef nn
                    Nothing -> writeSTRef mkRef 0xD9
                | otherwise -> do writeSTRef posRef (p+1); writeSTRef bbufRef b; writeSTRef bcntRef 8
        nextBit = do
          c0 <- readSTRef bcntRef
          c  <- if c0 == 0 then loadByte >> readSTRef bcntRef else pure c0
          if c == 0 then pure 0
          else do buf <- readSTRef bbufRef; writeSTRef bcntRef (c-1); pure ((buf `shiftR` (c-1)) .&. 1)
        getBitsJ n = go 0 n where go acc 0 = pure acc
                                  go acc k = do b <- nextBit; go ((acc `shiftL` 1) .|. b) (k-1)
        recv s | s == 0 = pure 0
               | otherwise = do v <- getBitsJ s
                                pure (if v < (1 `shiftL` (s-1)) then v - (1 `shiftL` s) + 1 else v)
        decodeHuffJ (Huff counts syms _) = go 1 0 0 0
          where go len code first idx
                  | len > 16 = pure 0
                  | otherwise = do
                      b <- nextBit
                      let code1 = (code `shiftL` 1) .|. b; cnt = counts A.! len
                      if code1 - first < cnt then pure (syms A.! (idx + (code1 - first)))
                      else go (len+1) code1 ((first+cnt) `shiftL` 1) (idx+cnt)
        -- After a scan, advance posRef to the next real marker (skipping the
        -- entropy stream's stuffed 0xFF00 and any RST markers).
        syncToMarker = do
          p0 <- readSTRef posRef
          let go p | p+1 >= BS.length bs = BS.length bs
                   | at bs p == 0xFF && at bs (p+1) /= 0
                     && not (at bs (p+1) >= 0xD0 && at bs (p+1) <= 0xD7) = p
                   | otherwise = go (p+1)
          writeSTRef posRef (go p0)
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
                          if tc == 0 then writeArray dcT th hf else writeArray acT th hf
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
          writeSTRef posRef next
          writeSTRef bbufRef 0; writeSTRef bcntRef 0; writeSTRef mkRef 0
          preds  <- newArray (0, max 0 (ncomp-1)) 0 :: ST s (STUArray s Int Int)
          eobRef <- newSTRef 0
          ri <- readSTRef driRef
          let zpos boff k = boff + (zigzag ! k)
              acFirst coef boff actbl k0 = do
                eob <- readSTRef eobRef
                if eob > 0 then writeSTRef eobRef (eob-1) else loop k0
                where loop k | k > se = pure ()
                             | otherwise = do
                                 rs <- decodeHuffJ actbl
                                 let r = rs `shiftR` 4; s = rs .&. 15
                                 if s == 0
                                   then if r < 15
                                          then do extra <- getBitsJ r
                                                  writeSTRef eobRef ((1 `shiftL` r) - 1 + extra)
                                          else loop (k+16)         -- ZRL: 16 zeros
                                   else do let k' = k + r
                                           if k' > se then pure () else do
                                             v <- recv s
                                             writeArray coef (zpos boff k') (v `shiftL` al)
                                             loop (k'+1)
              acRefine coef boff actbl k0 = do
                let p1 = 1 `shiftL` al
                    m1 = negate (1 `shiftL` al)
                    refineNZ k = do
                      v <- readArray coef (zpos boff k)
                      if v /= 0
                        then do b <- nextBit
                                when (b /= 0 && (v .&. p1) == 0) $
                                  writeArray coef (zpos boff k) (if v > 0 then v+p1 else v+m1)
                                pure True
                        else pure False
                kRef <- newSTRef k0
                eob0 <- readSTRef eobRef
                when (eob0 == 0) $ do
                  let outer = do
                        k <- readSTRef kRef
                        when (k <= se) $ do
                          rs <- decodeHuffJ actbl
                          let r = rs `shiftR` 4; s = rs .&. 15
                          newRef <- newSTRef 0
                          rRef   <- newSTRef r
                          brk    <- newSTRef False
                          if s == 0
                            then when (r < 15) $ do                 -- start an EOB run
                                   extra <- getBitsJ r
                                   writeSTRef eobRef ((1 `shiftL` r) + extra)
                                   writeSTRef brk True
                            else do b <- nextBit                    -- one new +-1 coefficient
                                    writeSTRef newRef (if b /= 0 then p1 else m1)
                          stop <- readSTRef brk
                          if stop then pure () else do
                            -- skip r zero-valued coefficients, refining nonzeros we pass
                            let adv = do
                                  k2 <- readSTRef kRef
                                  when (k2 <= se) $ do
                                    wasNZ <- refineNZ k2
                                    if wasNZ then writeSTRef kRef (k2+1) >> adv
                                    else do rr <- readSTRef rRef
                                            if rr == 0 then pure ()
                                            else do writeSTRef rRef (rr-1)
                                                    writeSTRef kRef (k2+1); adv
                            adv
                            nv <- readSTRef newRef
                            k3 <- readSTRef kRef
                            when (nv /= 0 && k3 <= se) $ writeArray coef (zpos boff k3) nv
                            writeSTRef kRef (k3+1)
                            outer
                  outer
                eob1 <- readSTRef eobRef
                when (eob1 > 0) $ do                                -- refine the rest of the band
                  let refLoop = do
                        k <- readSTRef kRef
                        when (k <= se) $ do _ <- refineNZ k; writeSTRef kRef (k+1); refLoop
                  refLoop
                  writeSTRef eobRef (eob1 - 1)
              decodeOneBlock ci td ta bx by = do
                let (bpr,_,_,_) = infoOf ci
                    coef = coefs !! ci
                    boff = (by*bpr + bx)*64
                dctbl <- readArray dcT td
                actbl <- readArray acT ta
                when (ss == 0) $
                  if ah == 0
                    then do t <- decodeHuffJ dctbl                  -- DC first
                            diff <- recv t
                            pv <- readArray preds ci
                            let dc = pv + diff
                            writeArray preds ci dc
                            writeArray coef boff (dc `shiftL` al)
                    else do b <- nextBit                            -- DC refine
                            when (b /= 0) $ do
                              v <- readArray coef boff
                              writeArray coef boff (v .|. (1 `shiftL` al))
                let acStart = max ss 1
                when (acStart <= se) $
                  if ah == 0 then acFirst  coef boff actbl acStart
                             else acRefine coef boff actbl acStart
          cntRef <- newSTRef 0
          let doRestart = do
                writeSTRef bcntRef 0; writeSTRef mkRef 0
                p <- readSTRef posRef; writeSTRef posRef (findRST bs p)
                forM_ [0..ncomp-1] $ \i -> writeArray preds i 0
                writeSTRef eobRef 0
              tick = when (ri > 0) $ do
                n <- readSTRef cntRef
                if n+1 == ri then doRestart >> writeSTRef cntRef 0
                else writeSTRef cntRef (n+1)
          -- Interleaved (>1 scan component) iterates MCUs; a single-component
          -- scan iterates that component's blocks in raster order.
          if length scanComps > 1
            then forM_ [0..mpc-1] $ \my -> forM_ [0..mpr-1] $ \mx -> do
                   forM_ scanComps $ \(ci,td,ta) -> do
                     let c = comps !! ci
                     forM_ [0..jcV c-1] $ \by -> forM_ [0..jcH c-1] $ \bx ->
                       decodeOneBlock ci td ta (mx*jcH c+bx) (my*jcV c+by)
                   tick
            else case scanComps of
                   ((ci,td,ta):_) -> do
                     let (_,_,bw,bh) = infoOf ci
                     forM_ [0..bh-1] $ \by -> forM_ [0..bw-1] $ \bx -> do
                       decodeOneBlock ci td ta bx by
                       tick
                   [] -> pure ()
          syncToMarker
    -- Marker walk: apply tables and decode every scan until EOI.
    let walk = do
          p <- readSTRef posRef
          if p + 1 >= BS.length bs then pure ()
          else if at bs p /= 0xFF then writeSTRef posRef (p+1) >> walk
          else let m = at bs (p+1) in
            if m == 0xFF then writeSTRef posRef (p+1) >> walk
            else if m == 0xD9 then pure ()                            -- EOI
            else if m == 0xD8 || (m >= 0xD0 && m <= 0xD7) then writeSTRef posRef (p+2) >> walk
            else do
              let len = be16 bs (p+2); seg = p+4; next = p+2+len
              if      m == 0xDB then applyDQT seg next >> writeSTRef posRef next >> walk
              else if m == 0xC4 then applyDHT seg next >> writeSTRef posRef next >> walk
              else if m == 0xDD then writeSTRef driRef (be16 bs seg) >> writeSTRef posRef next >> walk
              else if m == 0xDA then decodeScan seg next >> walk      -- leaves posRef at next marker
              else writeSTRef posRef next >> walk                     -- APPn / COM / SOF (already parsed)
    walk
    -- Reconstruct: dequantise + IDCT every block into its component plane.
    forM_ (zip3 [0..] comps compInfo) $ \(ci, c, (bpr,bpc,_,_)) -> do
      let coef = coefs !! ci
          (pw,ph,plane) = planes !! ci
      qtbl <- readArray quantT (jcTq c)
      forM_ [0..bpc-1] $ \by -> forM_ [0..bpr-1] $ \bx -> do
        let boff = (by*bpr+bx)*64
        cf <- mapM (\k -> do v <- readArray coef (boff+k); pure (v * (qtbl ! k))) [0..63]
        let cfA = U.listArray (0,63) cf :: UArray Int Int
        forM_ [0..7] $ \yy -> forM_ [0..7] $ \xx -> do
          let sm = 0.25 * sum [ (ct A.!(xx*8+u)) * (ct A.!(yy*8+v)) * fromIntegral (cfA ! (v*8+u))
                              | u <- [0..7], v <- [0..7] ]
              gx = bx*8+xx; gy = by*8+yy
          when (gx < pw && gy < ph) $
            writeArray plane (gy*pw+gx) (fromIntegral (clamp8 (round (sm+128))))
    -- Upsample (centred bilinear) + YCbCr->RGB into the RGBA image.
    let sampleAt ci x y = do
          let (pw,ph,plane) = planes !! ci
              c = comps !! ci
              fx = (fromIntegral x + 0.5) * fromIntegral (jcH c) / fromIntegral hmax - 0.5 :: Double
              fy = (fromIntegral y + 0.5) * fromIntegral (jcV c) / fromIntegral vmax - 0.5
              x0 = clampI 0 (pw-1) (floor fx); x1 = clampI 0 (pw-1) (x0+1)
              y0 = clampI 0 (ph-1) (floor fy); y1 = clampI 0 (ph-1) (y0+1)
              dx = fx - fromIntegral (floor fx :: Int); dy = fy - fromIntegral (floor fy :: Int)
          p00 <- rdD plane (y0*pw+x0); p10 <- rdD plane (y0*pw+x1)
          p01 <- rdD plane (y1*pw+x0); p11 <- rdD plane (y1*pw+x1)
          let top = p00 + (p10-p00)*dx; bot = p01 + (p11-p01)*dx
          pure (top + (bot-top)*dy)
    forM_ [0 .. h-1] $ \y -> forM_ [0 .. w-1] $ \x ->
      if ncomp == 1
        then do yv <- sampleAt 0 x y
                let g = clamp8 (round yv) in putRGBA img w x y g g g 255
        else do
          yf  <- sampleAt 0 x y
          cbv <- sampleAt 1 x y
          crv <- sampleAt 2 x y
          let cb = cbv - 128; cr = crv - 128
              r = clamp8 (round (yf + 1.402*cr))
              g = clamp8 (round (yf - 0.344136*cb - 0.714136*cr))
              b = clamp8 (round (yf + 1.772*cb))
          putRGBA img w x y r g b 255

clamp8 :: Int -> Int
clamp8 v = max 0 (min 255 v)

clampI :: Int -> Int -> Int -> Int
clampI lo hi v = max lo (min hi v)

rdD :: STUArray s Int Word8 -> Int -> ST s Double
rdD a i = fromIntegral <$> readArray a i

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
  raw <- case comp of
    0 -> do when' (BS.length body < w*h) "truncated WebP alpha"
            pure (U.listArray (0, w*h-1) (BS.unpack (BS.take (w*h) body)))
    1 -> do px <- vlDecodeStream body w h
            pure (U.listArray (0, w*h-1)
                    [ fromIntegral ((px U.! i) `shiftR` 8) | i <- [0 .. w*h-1] ])
    _ -> Left "unsupported WebP alpha compression"
  pure (if filt == 0 then raw else alphaDefilter filt w h raw)

-- Undo ALPH scanline filtering (1 horizontal, 2 vertical, 3 gradient).
alphaDefilter :: Int -> Int -> Int -> UArray Int Word8 -> UArray Int Word8
alphaDefilter filt w h src = runSTUArray $ do
  a <- newArray (0, max 0 (w*h-1)) 0
  forM_ [0 .. h-1] $ \y -> forM_ [0 .. w-1] $ \x -> do
    l  <- if x > 0 then fromIntegral <$> rd a (y*w + x-1)     else pure (0::Int)
    t  <- if y > 0 then fromIntegral <$> rd a ((y-1)*w + x)   else pure 0
    tl <- if x > 0 && y > 0 then fromIntegral <$> rd a ((y-1)*w + x-1) else pure 0
    let p | x == 0 && y == 0 = 0
          | filt == 1 = if x == 0 then t else l
          | filt == 2 = if y == 0 then l else t
          | otherwise = if x == 0 then t else if y == 0 then l
                        else clamp8 (l + t - tl)
    writeArray a (y*w+x)
      (fromIntegral ((fromIntegral (src U.! (y*w+x)) + p) .&. 0xff))
  pure a

------------------------------------------------------------------------------
-- VP8L (WebP lossless): an LSB-first bit stream of canonical prefix codes.
-- Pixels are ARGB Word32s produced by literals, LZ77 back-references over the
-- flattened pixel array (with a 2-D short-distance map) and a small hashed
-- colour cache; up to four invertible transforms (predictor, colour,
-- subtract-green, palette with pixel bundling) are undone afterwards.

-- Read n bits (n <= 18) LSB-first at the STRef bit position.
vlBits :: BS.ByteString -> STRef s Int -> Int -> ST s Int
vlBits bs posR n = do
  p <- readSTRef posR
  writeSTRef posR (p + n)
  let off = p `shiftR` 3
      b k = maybe 0 id (atM bs (off + k))
      raw = b 0 .|. (b 1 `shiftL` 8) .|. (b 2 `shiftL` 16) .|. (b 3 `shiftL` 24)
  pure ((raw `shiftR` (p .&. 7)) .&. ((1 `shiftL` n) - 1))

-- A prefix code: a code with a single used symbol consumes no bits at all.
data VHuff = VSingle !Int | VTable !Huff

mkVHuff :: [Int] -> VHuff
mkVHuff lens = case [ s | (s,l) <- zip [0..] lens, l > 0 ] of
  [s] -> VSingle s
  _   -> VTable (buildHuff lens)

-- Decode one symbol (canonical codes, first stream bit = code MSB, exactly
-- like DEFLATE); -1 on an invalid code.
vlSym :: BS.ByteString -> STRef s Int -> VHuff -> ST s Int
vlSym _ _ (VSingle s) = pure s
vlSym bs posR (VTable (Huff counts syms maxLen)) = go 1 0 0 0
  where go len code first index
          | len > maxLen = pure (-1)
          | otherwise = do
              b <- vlBits bs posR 1
              let code1 = code .|. b
                  cnt   = counts A.! len
              if code1 - first < cnt
                then pure (syms A.! (index + (code1 - first)))
                else go (len+1) (code1 `shiftL` 1) ((first+cnt) `shiftL` 1) (index+cnt)

vlCLOrder :: [Int]
vlCLOrder = [17,18,0,1,2,3,4,5,16,6,7,8,9,10,11,12,13,14,15]

-- Read one prefix code: the "simple" 1/2-symbol form, or code lengths that
-- are themselves prefix-coded (with 16/17/18 repeat codes, as in DEFLATE
-- except 16 repeats the previous *non-zero* length, initially 8).
vlReadCode :: BS.ByteString -> STRef s Int -> STRef s (Maybe String) -> Int
           -> ST s VHuff
vlReadCode bs posR errR alphaSize = do
  simple <- vlBits bs posR 1
  if simple == 1
    then do
      two  <- vlBits bs posR 1
      wide <- vlBits bs posR 1
      s0   <- vlBits bs posR (if wide == 1 then 8 else 1)
      ss   <- if two == 1 then do s1 <- vlBits bs posR 8; pure [s0,s1]
                          else pure [s0]
      if any (>= alphaSize) ss
        then do writeSTRef errR (Just "corrupt WebP (bad prefix code)")
                pure (VSingle 0)
        else pure (mkVHuff [ if s `elem` ss then 1 else 0 | s <- [0..alphaSize-1] ])
    else do
      nCodes <- (+4) <$> vlBits bs posR 4
      clRaw  <- mapM (const (vlBits bs posR 3)) [1 .. min 19 nCodes]
      let clLens = A.elems (A.accumArray (\_ x -> x) 0 (0,18)
                             (zip vlCLOrder clRaw)) :: [Int]
          clHuff = mkVHuff clLens
      useMax <- vlBits bs posR 1
      maxSym <- if useMax == 1
                  then do nb <- vlBits bs posR 3
                          v  <- vlBits bs posR (2 + 2*nb)
                          pure (2 + v)
                  else pure alphaSize
      lens <- vlReadLens bs posR errR clHuff alphaSize maxSym
      pure (mkVHuff lens)

vlReadLens :: BS.ByteString -> STRef s Int -> STRef s (Maybe String) -> VHuff
           -> Int -> Int -> ST s [Int]
vlReadLens bs posR errR clHuff total maxSym0 = go 0 8 maxSym0 []
  where
    go n prev maxSym acc
      | n >= total  = pure (reverse acc)
      | maxSym == 0 = pure (reverse acc ++ replicate (total - n) 0)
      | otherwise = do
          s <- vlSym bs posR clHuff
          if s < 0 || s > 18
            then do writeSTRef errR (Just "corrupt WebP (bad prefix code)")
                    pure (reverse acc ++ replicate (total - n) 0)
          else if s < 16
            then go (n+1) (if s /= 0 then s else prev) (maxSym-1) (s:acc)
          else do
            (rep, val) <- case s of
              16 -> do r <- vlBits bs posR 2; pure (r+3, prev)
              17 -> do r <- vlBits bs posR 3; pure (r+3, 0)
              _  -> do r <- vlBits bs posR 7; pure (r+11, 0)
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

pack32 :: Int -> Int -> Int -> Int -> Word32
pack32 a r g b = (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16)
             .|. (fromIntegral g `shiftL` 8)  .|. fromIntegral b

-- Per-channel add mod 256 (SWAR on the two interleaved lane pairs).
addPix32 :: Word32 -> Word32 -> Word32
addPix32 a b = (((a .&. 0xff00ff00) + (b .&. 0xff00ff00)) .&. 0xff00ff00)
           .|. (((a .&. 0x00ff00ff) + (b .&. 0x00ff00ff)) .&. 0x00ff00ff)

-- Per-channel floor average.
avg2 :: Word32 -> Word32 -> Word32
avg2 a b = (a .&. b) + (((a `xor` b) .&. 0xfefefefe) `shiftR` 1)

data VLTransform
  = VLPredictor !Int !(UArray Int Word32) !Int   -- tile bits, tiles, width
  | VLColor     !Int !(UArray Int Word32) !Int
  | VLSubGreen
  | VLPalette   !(UArray Int Word32) !Int        -- palette, unpacked width

-- Decode a whole VP8L image stream without the 5-byte container header
-- (this exact form is also how ALPH lossless alpha is stored).
vlDecodeStream :: BS.ByteString -> Int -> Int -> Either String (UArray Int Word32)
vlDecodeStream body w h = runST $ do
  posR <- newSTRef 0
  errR <- newSTRef Nothing
  px   <- vlImage body posR errR True w h
  err  <- readSTRef errR
  pure (maybe (Right px) Left err)

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
    posR <- newSTRef 40                       -- past signature + size header
    errR <- newSTRef Nothing
    p <- vlImage bs posR errR True w h
    err <- readSTRef errR
    pure (maybe (Right p) Left err)
  pure $ buildImage "WebP" w h $ \a ->
    forM_ [0 .. h-1] $ \y -> forM_ [0 .. w-1] $ \x -> do
      let v' = px U.! (y*w + x)
      putRGBA a w x y (chan v' 16) (chan v' 8) (chan v' 0) (chan v' 24)

-- One spatially-coded image: optional transforms (top level only), then the
-- entropy-coded pixels, then the inverse transforms in reverse read order.
vlImage :: BS.ByteString -> STRef s Int -> STRef s (Maybe String) -> Bool
        -> Int -> Int -> ST s (UArray Int Word32)
vlImage bs posR errR level0 w h = do
  (transforms, w') <- if level0 then vlReadTransforms bs posR errR h w
                                else pure ([], w)
  px <- vlEntropyImage bs posR errR level0 w' h
  pure (foldl (\p t -> vlInverse h t p) px transforms)

-- Transforms, most recently read first (= inverse application order).
vlReadTransforms :: BS.ByteString -> STRef s Int -> STRef s (Maybe String)
                 -> Int -> Int -> ST s ([VLTransform], Int)
vlReadTransforms bs posR errR h = go []
  where
    go acc w = do
      more <- vlBits bs posR 1
      if more == 0 then pure (acc, w) else do
        t <- vlBits bs posR 2
        case t of
          0 -> do bits <- (+2) <$> vlBits bs posR 3
                  timg <- vlImage bs posR errR False (vlSub w bits) (vlSub h bits)
                  go (VLPredictor bits timg w : acc) w
          1 -> do bits <- (+2) <$> vlBits bs posR 3
                  timg <- vlImage bs posR errR False (vlSub w bits) (vlSub h bits)
                  go (VLColor bits timg w : acc) w
          2 -> go (VLSubGreen : acc) w
          _ -> do n <- (+1) <$> vlBits bs posR 8
                  praw <- vlImage bs posR errR False n 1
                  let pal = U.listArray (0, n-1)
                              (scanl1 addPix32 (U.elems praw)) :: UArray Int Word32
                  go (VLPalette pal w : acc) (vlSub w (vlWidthBits n))

vlInverse :: Int -> VLTransform -> UArray Int Word32 -> UArray Int Word32
vlInverse h t px = case t of
  VLSubGreen            -> U.amap unGreen px
  VLColor bits tiles w  -> vlInvColor bits tiles w h px
  VLPredictor bits tiles w -> vlInvPredictor bits tiles w h px
  VLPalette pal w       -> vlInvPalette pal w h px
  where
    unGreen v = let g = (v `shiftR` 8) .&. 0xff
                    r = ((v `shiftR` 16) + g) .&. 0xff
                    b = (v + g) .&. 0xff
                in (v .&. 0xff00ff00) .|. (r `shiftL` 16) .|. b

-- Spatial predictor transform: add the per-tile predictor to each residual,
-- scanning in raster order over already-reconstructed neighbours.
vlInvPredictor :: Int -> UArray Int Word32 -> Int -> Int -> UArray Int Word32
               -> UArray Int Word32
vlInvPredictor bits tiles w h src = runSTUArray $ do
  out <- newArray (0, max 0 (w*h-1)) 0
  let tw = vlSub w bits
  forM_ [0 .. h-1] $ \y -> forM_ [0 .. w-1] $ \x -> do
    let i = y*w + x
    l  <- if x > 0 then readArray out (i-1) else pure 0
    t  <- if y > 0 then readArray out (i-w) else pure 0
    tl <- if x > 0 && y > 0 then readArray out (i-w-1) else pure 0
    -- For the last column i-w+1 is the first pixel of the current row: that
    -- is the format's actual top-right rule, not an accident.
    tr <- if y > 0 then readArray out (i-w+1) else pure 0
    let mode | x == 0 && y == 0 = 0
             | y == 0 = 1
             | x == 0 = 2
             | otherwise = fromIntegral
                 ((tiles U.! ((y `shiftR` bits)*tw + (x `shiftR` bits))
                    `shiftR` 8) .&. 0xff)
        p = vlPredict mode l t tl tr
    writeArray out i (addPix32 (src U.! i) p)
  pure out

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
        in if sum [d sh | sh <- [24,16,8,0]] <= 0 then t else l
  12 -> let f sh = clamp8 (chan l sh + chan t sh - chan tl sh)
        in pack32 (f 24) (f 16) (f 8) (f 0)
  13 -> let av = avg2 l t
            f sh = clamp8 (chan av sh + (chan av sh - chan tl sh) `quot` 2)
        in pack32 (f 24) (f 16) (f 8) (f 0)
  _  -> l

-- Cross-colour transform inverse: signed fixed-point green/red corrections.
vlInvColor :: Int -> UArray Int Word32 -> Int -> Int -> UArray Int Word32
           -> UArray Int Word32
vlInvColor bits tiles w h src =
  U.listArray (0, max 0 (w*h-1))
    [ let v   = src U.! (y*w + x)
          cte = tiles U.! ((y `shiftR` bits)*tw + (x `shiftR` bits))
          s8 n = if n >= 128 then n - 256 else n
          dlt a c = (a * c) `shiftR` 5
          g  = s8 (chan v 8)
          r' = (chan v 16 + dlt (s8 (chan cte 0)) g) .&. 0xff
          b' = (chan v 0 + dlt (s8 (chan cte 8)) g
                         + dlt (s8 (chan cte 16)) (s8 r')) .&. 0xff
      in (v .&. 0xff00ff00) .|. (fromIntegral r' `shiftL` 16) .|. fromIntegral b'
    | y <- [0 .. h-1], x <- [0 .. w-1] ]
  where tw = vlSub w bits

-- Palette lookup, unbundling packed sub-byte indices when the palette is
-- small enough that several pixels share one green byte.
vlInvPalette :: UArray Int Word32 -> Int -> Int -> UArray Int Word32
             -> UArray Int Word32
vlInvPalette pal w h src =
  U.listArray (0, max 0 (w*h-1))
    [ let packed = src U.! (y*pw + (x `shiftR` wb))
          idx = (chan packed 8 `shiftR` ((x .&. ((1 `shiftL` wb) - 1)) * bpp))
                .&. ((1 `shiftL` bpp) - 1)
      in if idx < n then pal U.! idx else 0
    | y <- [0 .. h-1], x <- [0 .. w-1] ]
  where
    n   = snd (bounds pal) + 1
    wb  = vlWidthBits n
    bpp = 8 `shiftR` wb
    pw  = vlSub w wb

-- The entropy-coded pixels: optional colour cache, optional meta prefix-code
-- image (top level only), then per-pixel symbols: literal / back-reference /
-- cache hit.
vlEntropyImage :: BS.ByteString -> STRef s Int -> STRef s (Maybe String) -> Bool
               -> Int -> Int -> ST s (UArray Int Word32)
vlEntropyImage bs posR errR level0 w h = do
  ccBit <- vlBits bs posR 1
  cacheBits <- if ccBit == 1 then vlBits bs posR 4 else pure 0
  when (ccBit == 1 && (cacheBits < 1 || cacheBits > 11)) $
    writeSTRef errR (Just "corrupt WebP (bad colour cache)")
  let cacheSize = if ccBit == 1 && cacheBits >= 1 && cacheBits <= 11
                    then 1 `shiftL` cacheBits else 0
  (metaBits, metaImg) <- if level0
    then do useMeta <- vlBits bs posR 1
            if useMeta == 1
              then do mb <- (+2) <$> vlBits bs posR 3
                      mi <- vlImage bs posR errR False (vlSub w mb) (vlSub h mb)
                      pure (mb, Just mi)
              else pure (0, Nothing)
    else pure (0, Nothing)
  let nGroups = case metaImg of
        Nothing -> 1
        Just mi -> 1 + maximum
                     (0 : [ fromIntegral ((v `shiftR` 8) .&. 0xffff) | v <- U.elems mi ])
      sizes = [256 + 24 + cacheSize, 256, 256, 256, 40]
  groups <- mapM (\_ -> mapM (vlReadCode bs posR errR) sizes) [1 .. nGroups]
  let garr = A.listArray (0, nGroups-1)
               [ case g of [a,b,c,d,e] -> (a,b,c,d,e)
                           _           -> (VSingle 0,VSingle 0,VSingle 0,VSingle 0,VSingle 0)
               | g <- groups ]
      mtw = vlSub w metaBits
      total = w * h
  out   <- newArray (0, max 0 (total-1)) 0 :: ST s (STUArray s Int Word32)
  cache <- newArray (0, max 0 cacheSize)  0 :: ST s (STUArray s Int Word32)
  let groupAt pos = case metaImg of
        Nothing -> garr A.! 0
        Just mi ->
          let y = pos `div` w; x = pos - y*w
              gi = fromIntegral ((mi U.! ((y `shiftR` metaBits)*mtw
                                          + (x `shiftR` metaBits))
                                   `shiftR` 8) .&. 0xffff)
          in if gi < nGroups then garr A.! gi else garr A.! 0
      insertC px = when (cacheSize > 0) $
        writeArray cache
          (fromIntegral ((0x1e35a7bd * px) `shiftR` (32 - cacheBits))) px
      bail m = writeSTRef errR (Just m)
      loop pos
        | pos >= total = pure ()
        | otherwise = do
            err <- readSTRef errR
            case err of
              Just _  -> pure ()
              Nothing -> do
                let (gT, rT, bT, aT, dT) = groupAt pos
                s <- vlSym bs posR gT
                if s < 0 then bail "corrupt WebP (bad prefix code)"
                else if s < 256 then do
                  r <- vlSym bs posR rT
                  b <- vlSym bs posR bT
                  a <- vlSym bs posR aT
                  if r < 0 || b < 0 || a < 0
                    then bail "corrupt WebP (bad prefix code)"
                    else do let px = pack32 a r s b
                            writeArray out pos px
                            insertC px
                            loop (pos+1)
                else if s < 280 then do
                  len <- vlPrefixVal bs posR (s - 256)
                  dc  <- vlSym bs posR dT
                  if dc < 0 || dc > 39 then bail "corrupt WebP (bad distance)"
                  else do
                    dv <- vlPrefixVal bs posR dc
                    let dist = vlPlaneDist w dv
                    if dist > pos || len <= 0
                      then bail "corrupt WebP (bad back-reference)"
                      else do
                        let k = min len (total - pos)
                        forM_ [0 .. k-1] $ \j -> do
                          v <- readArray out (pos - dist + j)
                          writeArray out (pos+j) v
                          insertC v
                        loop (pos + k)
                else do
                  let ci = s - 280
                  if ci >= cacheSize then bail "corrupt WebP (bad cache index)"
                  else do v <- readArray cache ci
                          writeArray out pos v
                          loop (pos+1)
  loop 0
  freezeU32 out

freezeU32 :: STUArray s Int Word32 -> ST s (UArray Int Word32)
freezeU32 = freeze

freezeU8 :: STUArray s Int Word8 -> ST s (UArray Int Word8)
freezeU8 = freeze

-- LZ77 length/distance prefix values: 24 codes, low four literal.
vlPrefixVal :: BS.ByteString -> STRef s Int -> Int -> ST s Int
vlPrefixVal bs posR code
  | code < 4 = pure (code + 1)
  | otherwise = do
      let eb = (code - 2) `shiftR` 1
      extra <- vlBits bs posR eb
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

-- Extra-bit probabilities for the large-coefficient categories 3..6.
vpCatProbs :: [[Int]]
vpCatProbs =
  [ [173,148,140]
  , [176,155,140,135]
  , [180,157,141,134,130]
  , [254,254,243,230,196,177,153,140,133,130,129] ]

-- Boolean arithmetic decoder (RFC 6386 section 7). Reads past the end of
-- the partition yield zero bytes, as the format prescribes.
data VPBool s = VPBool !BS.ByteString !Int !(STRef s Int) !(STRef s Int)
                       !(STRef s Int) !(STRef s Int)
                       -- buffer, end, pos, value, range, bit count

vpNewBool :: BS.ByteString -> Int -> Int -> ST s (VPBool s)
vpNewBool bs off len = do
  let end = min (BS.length bs) (max off (off + len))
      b i = if i >= 0 && i < end then at bs i else 0
  posR <- newSTRef (off + 2)
  valR <- newSTRef ((b off `shiftL` 8) .|. b (off+1))
  rngR <- newSTRef 255
  cntR <- newSTRef 0
  pure (VPBool bs end posR valR rngR cntR)

vpGetBool :: VPBool s -> Int -> ST s Int
vpGetBool (VPBool bs end posR valR rngR cntR) prob = do
  rng <- readSTRef rngR
  val <- readSTRef valR
  let split  = 1 + (((rng - 1) * prob) `shiftR` 8)
      sSplit = split `shiftL` 8
      (ret, rng1, val1) = if val >= sSplit then (1, rng - split, val - sSplit)
                                           else (0, split, val)
      norm r v
        | r >= 128 = do writeSTRef rngR r; writeSTRef valR v
        | otherwise = do
            c <- readSTRef cntR
            if c == 7
              then do
                p <- readSTRef posR
                let byte = if p < end then at bs p else 0
                writeSTRef posR (p+1)
                writeSTRef cntR 0
                norm (r `shiftL` 1) ((v `shiftL` 1) .|. byte)
              else do
                writeSTRef cntR (c+1)
                norm (r `shiftL` 1) (v `shiftL` 1)
  norm rng1 val1
  pure ret

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
  forM_ [0..1055] $ \i -> writeArray probs i (vpCoeffProbs ! i)
  forM_ [0..1055] $ \i -> do
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
  forM_ [0 .. mbH-1] $ \mby -> do
    forM_ [0..3] $ \i -> writeArray leftB i 0
    forM_ [0 .. mbW-1] $ \mbx -> do
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
        then forM_ [0..15] $ \j -> do
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
          forM_ [0..15] $ \j -> writeArray bmodes (mi*16 + j) imp
          forM_ [0..3] $ \i -> do writeArray aboveB (mbx*4 + i) imp
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
  let rdPlane pl pw border127 x y
        | y < 0     = pure 127
        | x < 0     = pure (if border127 then 127 else 129)
        | otherwise = fromIntegral <$> rd pl (y*pw + min x (pw-1))
      yRead  = rdPlane yPl yW False
      uRead  = rdPlane uPl cW False
      vRead  = rdPlane vPl cW False
  forM_ [0 .. mbH-1] $ \mby -> do
    forM_ [0..8] $ \i -> writeArray leftNZ i 0
    forM_ [0 .. mbW-1] $ \mbx -> do
      let mi = mby*mbW + mbx
          bd = partArr A.! (mby `mod` nParts)
      ym   <- rdI ymodes mi
      uvm  <- rdI uvmodes mi
      seg  <- rdI segIds mi
      skip <- rdI skips mi
      let hasY2 = ym /= 4
          (y1dc, y1ac, y2dc, y2ac, uvdc, uvac) = quants A.! seg
      forM_ [0 .. 25*16-1] $ \i -> writeArray coeffs i 0
      forM_ [0 .. 24] $ \i -> writeArray nCoefs i 0
      hasCoeff <-
        if skip == 1
          then do
            forM_ [0..3] $ \i -> do writeArray aboveNZ (mbx*9 + i) 0
                                    writeArray leftNZ i 0
            forM_ [4..7] $ \i -> do writeArray aboveNZ (mbx*9 + i) 0
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
                  n <- vpBlockCoeffs bd probs 1 (aY2 + lY2) 0 (y2dc, y2ac) coeffs (24*16)
                  writeArray aboveNZ (mbx*9 + 8) (if n > 0 then 1 else 0)
                  writeArray leftNZ 8 (if n > 0 then 1 else 0)
                  -- Inverse WHT scatters the DC values into the 16 Y blocks.
                  ws <- mapM (\i -> rdI coeffs (24*16 + i)) [0..15]
                  let dcs = vpWht ws
                  forM_ (zip [0..] dcs) $ \(b, dc) -> writeArray coeffs (b*16) dc
                  pure (1, 0, any (/= 0) dcs)
                else pure (0, 3, False)
            -- Y blocks.
            yNz <- foldM (\acc b -> do
                     let bx = b .&. 3; by = b `shiftR` 2
                     aN <- rdI aboveNZ (mbx*9 + bx)
                     lN <- rdI leftNZ by
                     n <- vpBlockCoeffs bd probs tyY (aN + lN) firstY (y1dc, y1ac)
                                        coeffs (b*16)
                     writeArray nCoefs b n
                     let nz = if n > firstY then 1 else 0
                     writeArray aboveNZ (mbx*9 + bx) nz
                     writeArray leftNZ by nz
                     pure (acc || nz == 1)) False [0..15]
            -- Chroma blocks (U then V).
            uvNz <- foldM (\acc b -> do
                     let j = b - 16
                         pl = j `shiftR` 2         -- 0 = U, 1 = V
                         bx = j .&. 1; by = (j `shiftR` 1) .&. 1
                         ni = 4 + pl*2
                     aN <- rdI aboveNZ (mbx*9 + ni + bx)
                     lN <- rdI leftNZ (ni + by)
                     n <- vpBlockCoeffs bd probs 2 (aN + lN) 0 (uvdc, uvac)
                                        coeffs (b*16)
                     writeArray nCoefs b n
                     let nz = if n > 0 then 1 else 0
                     writeArray aboveNZ (mbx*9 + ni + bx) nz
                     writeArray leftNZ (ni + by) nz
                     pure (acc || nz == 1)) False [16..23]
            pure (yNz || uvNz || dcNz)
      -- Reconstruction.
      let x0 = mbx*16; y0 = mby*16
          idctIf pl pw px py b = do
            n  <- rdI nCoefs b
            dc <- rdI coeffs (b*16)
            when (n > 0 || dc /= 0) $ vpIdctAdd pl pw px py coeffs (b*16)
      if ym == 4
        then do
          -- Above-MB-row pixels right of this MB: every right-column
          -- subblock's top-right predictor (the format's rule).
          trMB <- mapM (\i -> yRead (x0 + 16 + i) (y0 - 1)) [0..3]
          forM_ [0..15] $ \j -> do
            let r = j `shiftR` 2; c = j .&. 3
                bx = x0 + c*4; by = y0 + r*4
            m <- rdI bmodes (mi*16 + j)
            tr <- if c == 3 then pure trMB
                            else mapM (\i -> yRead (bx + 4 + i) (by - 1)) [0..3]
            vpPredB yRead yPl yW bx by m tr
            idctIf yPl yW bx by j
        else do
          vpPredBlock yRead yPl yW x0 y0 16 ym
          forM_ [0..15] $ \j ->
            idctIf yPl yW (x0 + (j .&. 3)*4) (y0 + (j `shiftR` 2)*4) j
      let cx0 = mbx*8; cy0 = mby*8
      vpPredBlock uRead uPl cW cx0 cy0 8 uvm
      forM_ [16..19] $ \b -> do
        let j = b - 16
        idctIf uPl cW (cx0 + (j .&. 1)*4) (cy0 + ((j `shiftR` 1) .&. 1)*4) b
      vpPredBlock vRead vPl cW cx0 cy0 8 uvm
      forM_ [20..23] $ \b -> do
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
  yF <- freezeU8 yPl
  uF <- freezeU8 uPl
  vF <- freezeU8 vPl
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
vpBlockCoeffs :: VPBool s -> STUArray s Int Int -> Int -> Int -> Int
              -> (Int, Int) -> STUArray s Int Int -> Int -> ST s Int
vpBlockCoeffs bd probs ty ctx0 first (dqDc, dqAc) out boff = do
  let pIdx band ctx = ((ty*8 + band)*3 + ctx)*11
      prB p i = do pv <- rdI probs (p + i); vpGetBool bd pv
      go n p = do
        e <- prB p 0
        if e == 0 then pure n else notEob n p
      notEob n p = do
        z <- prB p 1
        if z == 0
          then do
            let n1 = n + 1
            if n1 == 16 then pure 16
                        else notEob n1 (pIdx (vpBands ! n1) 0)
          else do
            one <- prB p 2
            (v, nctx) <- if one == 0 then pure (1, 1)
                                     else do v <- vpLargeValue bd probs p
                                             pure (v, 2)
            sgn <- vpFlag bd
            let v' = if sgn == 1 then negate v else v
                dq = if n > 0 then dqAc else dqDc
            writeArray out (boff + (vpZigzag ! n)) (v' * dq)
            let n1 = n + 1
            if n1 >= 16 then pure 16
                        else go n1 (pIdx (vpBands ! n1) nctx)
  go first (pIdx (vpBands ! first) ctx0)

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
          v <- foldM (\acc pp -> do b <- vpGetBool bd pp; pure (2*acc + b))
                     0 (vpCatProbs !! cat)
          pure (v + 3 + (8 `shiftL` cat))

-- The exact integer 4x4 inverse DCT (RFC 6386 section 14.3), added onto the
-- prediction already in the plane.
vpIdctAdd :: STUArray s Int Word8 -> Int -> Int -> Int -> STUArray s Int Int
          -> Int -> ST s ()
vpIdctAdd pl pw px py coeffs boff = do
  cs <- mapM (\i -> rdI coeffs (boff + i)) [0..15]
  let out = vpIdct cs
  forM_ (zip [0..] out) $ \(i, d) -> do
    let x = px + (i .&. 3); y = py + (i `shiftR` 2); o = y*pw + x
    old <- rd pl o
    writeArray pl o (fromIntegral (clamp8 (fromIntegral old + d)))

vpIdct :: [Int] -> [Int]
vpIdct cs = concatMap row [0..3]
  where
    ip = U.listArray (0,15) cs :: UArray Int Int
    m1 x = x + ((x * 20091) `shiftR` 16)
    m2 x = (x * 35468) `shiftR` 16
    col i = let i0 = ip ! i; i4 = ip ! (i+4); i8 = ip ! (i+8); i12 = ip ! (i+12)
                a1 = i0 + i8; b1 = i0 - i8
                c1 = m2 i4 - m1 i12
                d1 = m1 i4 + m2 i12
            in (a1 + d1, b1 + c1, b1 - c1, a1 - d1)
    cols = map col [0..3]
    tmp r i = case cols !! i of (v0,v1,v2,v3) -> [v0,v1,v2,v3] !! r
    row r = let t0 = tmp r 0; t1 = tmp r 1; t2 = tmp r 2; t3 = tmp r 3
                a1 = t0 + t2; b1 = t0 - t2
                c1 = m2 t1 - m1 t3
                d1 = m1 t1 + m2 t3
            in [ (a1 + d1 + 4) `shiftR` 3, (b1 + c1 + 4) `shiftR` 3
               , (b1 - c1 + 4) `shiftR` 3, (a1 - d1 + 4) `shiftR` 3 ]

-- Inverse Walsh-Hadamard transform for the luma DC plane (section 14.3).
vpWht :: [Int] -> [Int]
vpWht cs = concatMap row [0..3]
  where
    ip = U.listArray (0,15) cs :: UArray Int Int
    col i = let a1 = ip ! i + ip ! (i+12)
                b1 = ip ! (i+4) + ip ! (i+8)
                c1 = ip ! (i+4) - ip ! (i+8)
                d1 = ip ! i - ip ! (i+12)
            in (a1 + b1, c1 + d1, a1 - b1, d1 - c1)
    cols = map col [0..3]
    tmp r i = case cols !! i of (v0,v1,v2,v3) -> [v0,v1,v2,v3] !! r
    row r = let t0 = tmp r 0; t1 = tmp r 1; t2 = tmp r 2; t3 = tmp r 3
                a1 = t0 + t3; b1 = t1 + t2
                c1 = t1 - t2; d1 = t0 - t3
            in [ (a1 + b1 + 3) `shiftR` 3, (c1 + d1 + 3) `shiftR` 3
               , (a1 - b1 + 3) `shiftR` 3, (d1 - c1 + 3) `shiftR` 3 ]

-- Whole-block intra prediction (16x16 luma or 8x8 chroma): DC / V / H / TM.
vpPredBlock :: (Int -> Int -> ST s Int) -> STUArray s Int Word8 -> Int
            -> Int -> Int -> Int -> Int -> ST s ()
vpPredBlock rdP pl pw x0 y0 sz mode = case mode of
  0 -> do
    let hasA = y0 > 0; hasL = x0 > 0
        lg = if sz == 16 then 4 else 3
    a <- if hasA then sum <$> mapM (\i -> rdP (x0+i) (y0-1)) [0..sz-1] else pure 0
    l <- if hasL then sum <$> mapM (\i -> rdP (x0-1) (y0+i)) [0..sz-1] else pure 0
    let v | hasA && hasL = (a + l + sz) `shiftR` (lg + 1)
          | hasA         = (a + sz `div` 2) `shiftR` lg
          | hasL         = (l + sz `div` 2) `shiftR` lg
          | otherwise    = 128
    forM_ [0..sz-1] $ \y -> forM_ [0..sz-1] $ \x ->
      writeArray pl ((y0+y)*pw + x0+x) (fromIntegral v)
  1 -> forM_ [0..sz-1] $ \x -> do
         v <- rdP (x0+x) (y0-1)
         forM_ [0..sz-1] $ \y -> writeArray pl ((y0+y)*pw + x0+x) (fromIntegral v)
  2 -> forM_ [0..sz-1] $ \y -> do
         v <- rdP (x0-1) (y0+y)
         forM_ [0..sz-1] $ \x -> writeArray pl ((y0+y)*pw + x0+x) (fromIntegral v)
  _ -> do
    p <- rdP (x0-1) (y0-1)
    as <- mapM (\i -> rdP (x0+i) (y0-1)) [0..sz-1]
    ls <- mapM (\i -> rdP (x0-1) (y0+i)) [0..sz-1]
    forM_ (zip [0..] ls) $ \(y, lv) ->
      forM_ (zip [0..] as) $ \(x, av) ->
        writeArray pl ((y0+y)*pw + x0+x) (fromIntegral (clamp8 (lv + av - p)))

-- 4x4 luma sub-block prediction: the ten B_* modes (section 12.3).
vpPredB :: (Int -> Int -> ST s Int) -> STUArray s Int Word8 -> Int
        -> Int -> Int -> Int -> [Int] -> ST s ()
vpPredB rdP pl pw x0 y0 mode tr = do
  p  <- rdP (x0-1) (y0-1)
  as <- mapM (\i -> rdP (x0+i) (y0-1)) [0..3]
  ls <- mapM (\i -> rdP (x0-1) (y0+i)) [0..3]
  let aa i = (as ++ tr) !! i                          -- above + above-right
      e i  = ([ls !! 3, ls !! 2, ls !! 1, ls !! 0, p] ++ as) !! i
      l r  = ls !! r
      avg3 x y z = (x + 2*y + z + 2) `shiftR` 2
      avg2' x y = (x + y + 1) `shiftR` 1
      -- B_VE / B_HE smooth across the corner pixel.
      ve = [ avg3 (if c == 0 then p else as !! (c-1)) (as !! c)
                  (if c == 3 then head tr else as !! (c+1)) | c <- [0..3] ]
      he r = avg3 (if r == 0 then p else l (r-1)) (l r) (l (min 3 (r+1)))
      vr0 = [ avg2' p (aa 0), avg2' (aa 0) (aa 1), avg2' (aa 1) (aa 2)
            , avg2' (aa 2) (aa 3) ]
      vr1 = [ avg3 (l 0) p (aa 0), avg3 p (aa 0) (aa 1)
            , avg3 (aa 0) (aa 1) (aa 2), avg3 (aa 1) (aa 2) (aa 3) ]
      vl0 = [ avg2' (aa c) (aa (c+1)) | c <- [0..3] ]
      vl1 = [ avg3 (aa c) (aa (c+1)) (aa (c+2)) | c <- [0..3] ]
      hd0 = [ avg2' (l 0) p, avg3 (l 0) p (aa 0)
            , avg3 p (aa 0) (aa 1), avg3 (aa 0) (aa 1) (aa 2) ]
      hdR r prev = [ avg2' (l r) (l (r-1)), avg3 (l r) (l (r-1)) (e (5-r))
                   , head prev, prev !! 1 ]
      hd1 = hdR 1 hd0; hd2 = hdR 2 hd1; hd3 = hdR 3 hd2
      hu0 = [ avg2' (l 0) (l 1), avg3 (l 0) (l 1) (l 2)
            , avg2' (l 1) (l 2), avg3 (l 1) (l 2) (l 3) ]
      hu1 = [ hu0 !! 2, hu0 !! 3, avg2' (l 2) (l 3), avg3 (l 2) (l 3) (l 3) ]
      rows = case mode of
        0 -> let v = (sum as + sum ls + 4) `shiftR` 3
             in replicate 4 (replicate 4 v)
        1 -> [ [ clamp8 (l r + as !! c - p) | c <- [0..3] ] | r <- [0..3] ]
        2 -> replicate 4 ve
        3 -> [ replicate 4 (he r) | r <- [0..3] ]
        4 -> [ [ let d = c - r
                 in avg3 (e (d+3)) (e (d+4)) (e (d+5))
               | c <- [0..3] ] | r <- [0..3] ]                    -- B_RD
        5 -> [ vr0, vr1
             , avg3 (l 1) (l 0) p : take 3 vr0
             , avg3 (l 2) (l 1) (l 0) : take 3 vr1 ]              -- B_VR
        6 -> [ [ let k = r + c
                 in if k < 6 then avg3 (aa k) (aa (k+1)) (aa (k+2))
                             else avg3 (aa 6) (aa 7) (aa 7)
               | c <- [0..3] ] | r <- [0..3] ]                    -- B_LD
        7 -> [ vl0, vl1
             , tail vl0 ++ [avg3 (aa 4) (aa 5) (aa 6)]
             , tail vl1 ++ [avg3 (aa 5) (aa 6) (aa 7)] ]
        8 -> [ hd0, hd1, hd2, hd3 ]
        _ -> [ hu0, hu1
             , [ hu1 !! 2, hu1 !! 3, l 3, l 3 ]
             , replicate 4 (l 3) ]
  forM_ (zip [0..] rows) $ \(r, rowVs) ->
    forM_ (zip [0..] rowVs) $ \(c, v) ->
      writeArray pl ((y0+r)*pw + x0+c) (fromIntegral (clampI 0 255 v))

-- In-loop deblocking: per macroblock, left edge, interior vertical edges,
-- top edge, interior horizontal edges (RFC 6386 section 15). The simple
-- variant touches only luma; the normal variant also filters chroma and the
-- wider macroblock edges.
vpFilterFrame :: STUArray s Int Word8 -> STUArray s Int Word8
              -> STUArray s Int Word8 -> Int -> Int -> Int -> Int
              -> STUArray s Int Int -> STUArray s Int Int -> Int -> Bool
              -> ST s ()
vpFilterFrame yPl uPl vPl yW cW mbW mbH fLevels fInner sharp simple =
  forM_ [0 .. mbH-1] $ \mby -> forM_ [0 .. mbW-1] $ \mbx -> do
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
      if simple
        then do
          let x0 = mbx*16; y0 = mby*16
          when (mbx > 0) $ forM_ [0..15] $ \r ->
            vpSimpleEdge yPl (( y0+r)*yW + x0) 1 mbLim
          when doInner $ forM_ [4,8,12] $ \dx -> forM_ [0..15] $ \r ->
            vpSimpleEdge yPl ((y0+r)*yW + x0+dx) 1 subLim
          when (mby > 0) $ forM_ [0..15] $ \c ->
            vpSimpleEdge yPl (y0*yW + x0+c) yW mbLim
          when doInner $ forM_ [4,8,12] $ \dy -> forM_ [0..15] $ \c ->
            vpSimpleEdge yPl ((y0+dy)*yW + x0+c) yW subLim
        else do
          let x0 = mbx*16; y0 = mby*16
              cx0 = mbx*8; cy0 = mby*8
          when (mbx > 0) $ do
            forM_ [0..15] $ \r -> vpEdge True yPl ((y0+r)*yW + x0) 1 mbLim ilev hev
            forM_ [0..7] $ \r -> do
              vpEdge True uPl ((cy0+r)*cW + cx0) 1 mbLim ilev hev
              vpEdge True vPl ((cy0+r)*cW + cx0) 1 mbLim ilev hev
          when doInner $ do
            forM_ [4,8,12] $ \dx -> forM_ [0..15] $ \r ->
              vpEdge False yPl ((y0+r)*yW + x0+dx) 1 subLim ilev hev
            forM_ [0..7] $ \r -> do
              vpEdge False uPl ((cy0+r)*cW + cx0+4) 1 subLim ilev hev
              vpEdge False vPl ((cy0+r)*cW + cx0+4) 1 subLim ilev hev
          when (mby > 0) $ do
            forM_ [0..15] $ \c -> vpEdge True yPl (y0*yW + x0+c) yW mbLim ilev hev
            forM_ [0..7] $ \c -> do
              vpEdge True uPl (cy0*cW + cx0+c) cW mbLim ilev hev
              vpEdge True vPl (cy0*cW + cx0+c) cW mbLim ilev hev
          when doInner $ do
            forM_ [4,8,12] $ \dy -> forM_ [0..15] $ \c ->
              vpEdge False yPl ((y0+dy)*yW + x0+c) yW subLim ilev hev
            forM_ [0..7] $ \c -> do
              vpEdge False uPl ((cy0+4)*cW + cx0+c) cW subLim ilev hev
              vpEdge False vPl ((cy0+4)*cW + cx0+c) cW subLim ilev hev

clampS :: Int -> Int
clampS v = clampI (-128) 127 v

-- The 2-tap adjustment shared by every filter (common_adjust with outer
-- taps); returns nothing extra since callers re-read pixels as needed.
vpAdjust2 :: STUArray s Int Word8 -> Int -> Int -> ST s ()
vpAdjust2 pl o step = do
  p1 <- rdP8 pl (o-2*step); p0 <- rdP8 pl (o-step)
  q0 <- rdP8 pl o;          q1 <- rdP8 pl (o+step)
  let a  = clampS (3*(q0 - p0) + clampS (p1 - q1))
      f  = clampS (a + 4) `shiftR` 3
      e' = clampS (a + 3) `shiftR` 3
  writeArray pl o (fromIntegral (clamp8 (q0 - f)))
  writeArray pl (o-step) (fromIntegral (clamp8 (p0 + e')))

rdP8 :: STUArray s Int Word8 -> Int -> ST s Int
rdP8 pl i = fromIntegral <$> readArray pl i

vpSimpleEdge :: STUArray s Int Word8 -> Int -> Int -> Int -> ST s ()
vpSimpleEdge pl o step lim = do
  p1 <- rdP8 pl (o-2*step); p0 <- rdP8 pl (o-step)
  q0 <- rdP8 pl o;          q1 <- rdP8 pl (o+step)
  when (4*abs (p0-q0) + abs (p1-q1) <= 2*lim + 1) $ vpAdjust2 pl o step

-- Normal filter for one edge position: macroblock edges use the wide 6-tap
-- smoothing, interior edges the 4-tap one; high edge variance falls back to
-- the 2-tap adjustment in both.
vpEdge :: Bool -> STUArray s Int Word8 -> Int -> Int -> Int -> Int -> Int
       -> ST s ()
vpEdge mbEdge pl o step lim ilim hevT = do
  p3 <- rdP8 pl (o-4*step); p2 <- rdP8 pl (o-3*step)
  p1 <- rdP8 pl (o-2*step); p0 <- rdP8 pl (o-step)
  q0 <- rdP8 pl o;          q1 <- rdP8 pl (o+step)
  q2 <- rdP8 pl (o+2*step); q3 <- rdP8 pl (o+3*step)
  let ok = 4*abs (p0-q0) + abs (p1-q1) <= 2*lim + 1
           && abs (p3-p2) <= ilim && abs (p2-p1) <= ilim && abs (p1-p0) <= ilim
           && abs (q3-q2) <= ilim && abs (q2-q1) <= ilim && abs (q1-q0) <= ilim
      isHev = abs (p1-p0) > hevT || abs (q1-q0) > hevT
  when ok $
    if isHev
      then vpAdjust2 pl o step
      else if mbEdge
        then do
          let wv = clampS (clampS (p1 - q1) + 3*(q0 - p0))
              a1 = (27*wv + 63) `shiftR` 7
              a2 = (18*wv + 63) `shiftR` 7
              a3 = (9*wv + 63) `shiftR` 7
          writeArray pl (o-3*step) (fromIntegral (clamp8 (p2 + a3)))
          writeArray pl (o-2*step) (fromIntegral (clamp8 (p1 + a2)))
          writeArray pl (o-step)   (fromIntegral (clamp8 (p0 + a1)))
          writeArray pl o          (fromIntegral (clamp8 (q0 - a1)))
          writeArray pl (o+step)   (fromIntegral (clamp8 (q1 - a2)))
          writeArray pl (o+2*step) (fromIntegral (clamp8 (q2 - a3)))
        else do
          let a  = clampS (3*(q0 - p0))
              a1 = clampS (a + 4) `shiftR` 3
              a2 = clampS (a + 3) `shiftR` 3
              a3 = (a1 + 1) `shiftR` 1
          writeArray pl (o-2*step) (fromIntegral (clamp8 (p1 + a3)))
          writeArray pl (o-step)   (fromIntegral (clamp8 (p0 + a2)))
          writeArray pl o          (fromIntegral (clamp8 (q0 - a1)))
          writeArray pl (o+step)   (fromIntegral (clamp8 (q1 - a3)))

-- YUV 4:2:0 planes to RGBA: "fancy" bilinear chroma upsampling and the
-- BT.601 studio-range conversion, in libwebp's fixed-point arithmetic.
vpToImage :: Int -> Int -> Int -> Int -> UArray Int Word8 -> UArray Int Word8
          -> UArray Int Word8 -> Maybe (UArray Int Word8) -> Image
vpToImage w h yW cW yP uP vP malpha =
  buildImage "WebP" w h $ \a -> do
    let chW = (w + 1) `shiftR` 1; chH = (h + 1) `shiftR` 1
        cAt pl cx cy = fromIntegral
          (pl U.! (clampI 0 (chH-1) cy * cW + clampI 0 (chW-1) cx)) :: Int
        up pl x y =
          let cx = x `shiftR` 1; cy = y `shiftR` 1
              dx = if odd x then 1 else -1
              dy = if odd y then 1 else -1
          in (9 * cAt pl cx cy + 3 * cAt pl (cx+dx) cy
              + 3 * cAt pl cx (cy+dy) + cAt pl (cx+dx) (cy+dy) + 8) `shiftR` 4
    forM_ [0 .. h-1] $ \y -> forM_ [0 .. w-1] $ \x -> do
      let yv = fromIntegral (yP U.! (y*yW + x)) :: Int
          uv = up uP x y
          vv = up vP x y
          (r,g,b) = vpYuvRgb yv uv vv
          al = case malpha of
                 Nothing -> 255
                 Just ap -> fromIntegral (ap U.! (y*w + x))
      putRGBA a w x y r g b al

vpYuvRgb :: Int -> Int -> Int -> (Int, Int, Int)
vpYuvRgb y u v =
  let mh val co = (val * co) `shiftR` 8
      clip x = if x < 0 then 0 else if x >= 16384 then 255 else x `shiftR` 6
      yy = mh y 19077
  in ( clip (yy + mh v 26149 - 14234)
     , clip (yy - mh u 6419 - mh v 13320 + 8708)
     , clip (yy + mh u 33050 - 17685) )

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
viewFit :: Double -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
viewFit aspect cols rows cropW cropH =
  let cols' = max 1 cols
      subH  = max 1 rows * 2
      cw = max 1 cropW; ch = max 1 cropH
      a  = if aspect > 0 then aspect else 1
      -- Uniform physical scale (cell-widths per source pixel), bounded by the
      -- canvas in both directions; a sub-pixel is 1 wide and @a@ tall.
      sc = min (fromIntegral cols' / fromIntegral cw)
               (fromIntegral subH * a / fromIntegral ch) :: Double
      outW = max 1 (min cols' (round (fromIntegral cw * sc)))
      outH = max 1 (min subH  (round (fromIntegral ch * sc / a)))
  in (outW, outH, (cols' - outW) `div` 2, (subH - outH) `div` 2)

-- | Render a sub-rectangle @(cropX, cropY, cropW, cropH)@ (in source pixels) of
-- an image into a @rows x cols@ grid of styled cells, scaled to fit while
-- preserving aspect ratio and centred. Pass the whole image rectangle for the
-- default (unzoomed) view. In 'HalfBlock' mode each cell is a @▀@ glyph whose
-- foreground is the top sub-pixel and background the bottom, doubling vertical
-- resolution; in 'Ascii' mode each cell is a luminance-ramp character tinted
-- with the average colour. The grid is indexed @(row, col)@.
renderImage :: Double -> ImgMode -> Int -> Int -> (Int, Int, Int, Int) -> Image
            -> Array (Int, Int) Cell
renderImage aspect mode cols rows (cropX, cropY, cropW, cropH) img =
  listArray ((0,0),(rows-1,cols-1))
    [ cellAt r c | r <- [0 .. rows-1], c <- [0 .. cols-1] ]
  where
    iw = imgW img
    cw = max 1 cropW; ch = max 1 cropH
    (outW, outH, offX, offY) = viewFit aspect cols rows cw ch
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
  BS.pack [ chan | sy <- [0 .. outH - 1], sx <- [0 .. outW - 1]
                 , chan <- sample sx sy ]
  where
    outW = max 1 outW0; outH = max 1 outH0
    cw = max 1 cropW; ch = max 1 cropH
    iw = imgW img
    pix = imgPix img
    sample sx sy =
      let x0 = cropX + (sx * cw) `div` outW
          x1 = max (x0 + 1) (cropX + ((sx + 1) * cw) `div` outW)
          y0 = cropY + (sy * ch) `div` outH
          y1 = max (y0 + 1) (cropY + ((sy + 1) * ch) `div` outH)
          (n, sr, sg, sb, sa) = boxSum x0 x1 y0 y1
      in if n == 0 then [0, 0, 0, 0]
         else map (fromIntegral . (`div` n)) [sr, sg, sb, sa]
    boxSum x0 x1 y0 y1 = goY y0 (0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int)
      where
        goY y acc
          | y >= y1   = acc
          | otherwise = goY (y + 1) (goX x0 y acc)
        goX x y acc@(n, r, g, b, a)
          | x >= x1   = acc
          | x < 0 || x >= iw || y < 0 || y >= imgH img = goX (x + 1) y acc
          | otherwise =
              let o = (y * iw + x) * 4
              in goX (x + 1) y ( n + 1
                               , r + fromIntegral (pix ! o)
                               , g + fromIntegral (pix ! (o + 1))
                               , b + fromIntegral (pix ! (o + 2))
                               , a + fromIntegral (pix ! (o + 3)) )

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
