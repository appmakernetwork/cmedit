-- | Pixel-graphics output: the kitty graphics protocol and sixel, both
-- hand-rolled on boot libraries (a base64 encoder and a run-length sixel
-- encoder). The image view upgrades to true pixels when the terminal
-- advertised one of these ('Cmedit.Caps'); the half-block cell renderer stays
-- underneath as the universal fallback, so terminals without either see
-- exactly what they saw before.
--
-- Pure: everything here builds escape byte streams from an 'Image'; the
-- driver decides when to emit them.
module Cmedit.Gfx
  ( -- * Capability probe / teardown
    kittyGfxProbe
  , kittyGfxDeleteAll
    -- * Placement
  , GfxKind(..)
  , gfxFit
  , maxGfxPixels
  , kittyPlace
  , sixelPlace
    -- * Encoders (exposed for tests)
  , base64B
  , sixelEncode
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString.Builder (Builder, char7, intDec, string7, word8)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (UArray, (!))
import Data.Array.ST (runSTUArray)
import qualified Data.Array.Unboxed as U
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)

import Cmedit.Ansi (csi, esc, moveTo)
import Cmedit.Image (Image(..))

------------------------------------------------------------------------------
-- Kitty graphics protocol

apc :: Builder -> Builder
apc b = esc <> char7 '_' <> b <> esc <> char7 '\\'

-- | Query support: transmit a 1x1 RGB probe with the query action. A
-- supporting terminal answers @APC G i=31;OK ST@ (decoded to 'TrKittyGfx');
-- everything else ignores the APC entirely.
kittyGfxProbe :: Builder
kittyGfxProbe = apc (string7 "Gi=31,s=1,v=1,a=q,t=d,f=24;AAAA")

-- | Delete every visible kitty-graphics placement (also safe to emit on
-- terminals that never showed one).
kittyGfxDeleteAll :: Builder
kittyGfxDeleteAll = apc (string7 "Ga=d,d=A")

-- | Which pixel protocol a placement uses.
data GfxKind = GfxKitty | GfxSixel
  deriving (Eq, Show)

-- | Don't ship more pixels than this per placement: it bounds the encode +
-- transmit cost (a placement is re-sent only when the view actually changes).
maxGfxPixels :: Int
maxGfxPixels = 1200 * 1000

-- | Fit a @cw x ch@ source-pixel crop into the text area (rows/cols
-- @top,left,tw,th@) given the cell pixel size, preserving aspect: returns
-- @(row, col, cols, rows, pxW, pxH)@ — the placement cell box (centred) and
-- the pixel resolution to encode at ('maxGfxPixels'-capped, and never more
-- than the source has). When @allowUpscale@ is 'False' the crop is never
-- enlarged past native (1 device pixel per source pixel), so a small image is
-- shown at 1:1 and centred rather than blown up to fill; a user zoom crop
-- passes 'True' so a selected region still fills the view. Kept in step with
-- the cell-renderer cap in 'Cmedit.EditorState.imageFitCap'.
gfxFit :: (Int, Int) -> (Int, Int, Int, Int) -> (Int, Int) -> Bool
       -> (Int, Int, Int, Int, Int, Int)
gfxFit (pw0, ph0) (top, left, tw, th) (cw0, ch0) allowUpscale =
  let pw = max 1 pw0; ph = max 1 ph0
      cw = max 1 cw0; ch = max 1 ch0
      -- Uniform physical scale bounded by the box in both directions, capped
      -- at native resolution unless upscaling is allowed.
      scFit = min (fromIntegral (tw * pw) / fromIntegral cw)
                  (fromIntegral (th * ph) / fromIntegral ch) :: Double
      sc = if allowUpscale then scFit else min scFit 1.0
      cols = max 1 (min tw (round (fromIntegral cw * sc / fromIntegral pw)))
      rows = max 1 (min th (round (fromIntegral ch * sc / fromIntegral ph)))
      row = top + (th - rows) `div` 2
      col = left + (tw - cols) `div` 2
      -- Encode at the displayed resolution, capped, and never upscaled past
      -- the source (the terminal scales the payload into the cell box).
      dispW = cols * pw; dispH = rows * ph
      capSc = minimum [ 1.0
                      , sqrt (fromIntegral maxGfxPixels / fromIntegral (max 1 (dispW * dispH)))
                      , fromIntegral cw / fromIntegral dispW
                      , fromIntegral ch / fromIntegral dispH ] :: Double
      pxW = max 1 (round (fromIntegral dispW * capSc))
      pxH = max 1 (round (fromIntegral dispH * capSc))
  in (row, col, cols, rows, pxW, pxH)

-- | Place raw RGBA pixels (@pxW x pxH@) into a cell box at @(row, col)@ via
-- the kitty protocol: delete the previous placement, transmit in base64
-- chunks, display scaled into @cols x rows@ cells. @q=2@ suppresses the OK
-- chatter a display action would otherwise send back.
kittyPlace :: (Int, Int) -> (Int, Int) -> (Int, Int) -> BS.ByteString -> Builder
kittyPlace (row, col) (cols, rows) (pxW, pxH) rgba =
  kittyGfxDeleteAll
    <> moveTo row col
    <> chunks True rgba
  where
    header first m =
      if first
        then string7 "Ga=T,f=32,i=1,q=2,s=" <> intDec pxW <> string7 ",v=" <> intDec pxH
               <> string7 ",c=" <> intDec cols <> string7 ",r=" <> intDec rows
               <> string7 ",m=" <> intDec m
        else string7 "Gm=" <> intDec m
    chunks first bs =
      let (now, rest) = BS.splitAt 3072 bs   -- 4096 base64 chars per chunk
          m = if BS.null rest then 0 else 1
          part = apc (header first m <> char7 ';' <> base64B now)
      in if BS.null rest then part else part <> chunks False rest

-- | Place raw RGBA pixels as a sixel graphic at @(row, col)@. The terminal
-- paints the pixels over that region at its own cell raster; the caller
-- chooses the pixel size to match the cell box ('gfxFit').
sixelPlace :: (Int, Int) -> (Int, Int) -> BS.ByteString -> Builder
sixelPlace (row, col) (pxW, pxH) rgba =
  moveTo row col <> sixelEncode pxW pxH rgba

------------------------------------------------------------------------------
-- Base64 (Builder over strict ByteString input)

base64B :: BS.ByteString -> Builder
base64B = go
  where
    go bs = case BS.length bs of
      0 -> mempty
      1 -> let a = BS.index bs 0
               n = fromIntegral a `shiftL` 16 :: Int
           in enc (n `shiftR` 18) <> enc ((n `shiftR` 12) .&. 63)
                <> char7 '=' <> char7 '='
      2 -> let a = BS.index bs 0; b = BS.index bs 1
               n = (fromIntegral a `shiftL` 16) .|. (fromIntegral b `shiftL` 8) :: Int
           in enc (n `shiftR` 18) <> enc ((n `shiftR` 12) .&. 63)
                <> enc ((n `shiftR` 6) .&. 63) <> char7 '='
      _ -> let a = BS.index bs 0; b = BS.index bs 1; c = BS.index bs 2
               n = (fromIntegral a `shiftL` 16) .|. (fromIntegral b `shiftL` 8)
                     .|. fromIntegral c :: Int
           in enc (n `shiftR` 18) <> enc ((n `shiftR` 12) .&. 63)
                <> enc ((n `shiftR` 6) .&. 63) <> enc (n .&. 63)
                <> go (BS.drop 3 bs)
    enc i = word8 (b64tab `BS.index` i)
    b64tab = BS.pack (map (fromIntegral . fromEnum)
               "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

------------------------------------------------------------------------------
-- Sixel

-- The fixed 6x7x6 RGB palette (252 registers): small enough for any sixel
-- terminal, dense enough that area-averaged photos read well.
sixelColors :: Int
sixelColors = 6 * 7 * 6

sixelIndex :: Word8 -> Word8 -> Word8 -> Int
sixelIndex r g b =
  let ri = (fromIntegral r * 5 + 127) `div` 255
      gi = (fromIntegral g * 6 + 127) `div` 255
      bi = (fromIntegral b * 5 + 127) `div` 255
  in (ri * 7 + gi) * 6 + bi

-- Palette entry i as 0-100 RGB percentages (the sixel colour space).
sixelRgb :: Int -> (Int, Int, Int)
sixelRgb i =
  let (rg, bi) = i `divMod` 6
      (ri, gi) = rg `divMod` 7
  in (ri * 100 `div` 5, gi * 100 `div` 6, bi * 100 `div` 5)

-- | Encode @w x h@ raw RGBA pixels as a complete sixel sequence (DCS .. ST).
-- Pixels with alpha < 128 are left unset; P2=1 keeps them transparent, so the
-- terminal background shows through exactly like the cell renderer's blanks.
sixelEncode :: Int -> Int -> BS.ByteString -> Builder
sixelEncode w h rgba =
  esc <> string7 "P0;1;0q"
    <> string7 "\"1;1;" <> intDec w <> char7 ';' <> intDec h
    <> palette
    <> bands 0
    <> esc <> char7 '\\'
  where
    -- Quantized colour index per pixel, -1 for transparent.
    idx :: UArray Int Int
    idx = runSTUArray $ do
      a <- newArray (0, max 0 (w * h - 1)) (-1)
      forM_ [0 .. w * h - 1] $ \p -> do
        let o = p * 4
        when (BS.index rgba (o + 3) >= 128) $
          writeArray a p (sixelIndex (BS.index rgba o)
                                     (BS.index rgba (o + 1))
                                     (BS.index rgba (o + 2)))
      pure a

    used :: UArray Int Bool
    used = runSTUArray $ do
      a <- newArray (0, sixelColors - 1) False
      forM_ [0 .. w * h - 1] $ \p -> do
        let c = idx ! p
        when (c >= 0) (writeArray a c True)
      pure a

    palette = mconcat
      [ char7 '#' <> intDec c <> string7 ";2;" <> intDec r <> char7 ';'
          <> intDec g <> char7 ';' <> intDec b
      | c <- [0 .. sixelColors - 1], used ! c
      , let (r, g, b) = sixelRgb c ]

    bands y0
      | y0 >= h = mempty
      | otherwise = band y0 <> (if y0 + 6 < h then char7 '-' else mempty) <> bands (y0 + 6)

    -- One 6-row band: per colour present in the band, a run-length-encoded
    -- bitmask row ('$' rewinds the band between colours).
    band y0 = runST $ do
      bits <- newBandBits (sixelColors * w - 1)
      seen <- newSeen (sixelColors - 1)
      forM_ [0 .. min 5 (h - 1 - y0)] $ \k ->
        forM_ [0 .. w - 1] $ \x -> do
          let c = idx ! ((y0 + k) * w + x)
          when (c >= 0) $ do
            old <- readArray bits (c * w + x)
            writeArray bits (c * w + x) (old .|. (1 `shiftL` k))
            writeArray seen c True
      let rowFor c = rle c 0 (-1) 0 mempty
          -- Run-length emit one colour's band row.
          rle c x lastB n acc
            | x >= w = pure (acc <> flush lastB n)
            | otherwise = do
                b <- readArray bits (c * w + x)
                let ch = fromIntegral b :: Int
                if ch == lastB
                  then rle c (x + 1) lastB (n + 1) acc
                  else rle c (x + 1) ch 1 (acc <> flush lastB n)
          flush b n
            | n <= 0 || b < 0 = mempty
            | n <= 3 = mconcat (replicate n (sixChar b))
            | otherwise = char7 '!' <> intDec n <> sixChar b
          sixChar b = word8 (fromIntegral (63 + b))
          loop c first acc
            | c >= sixelColors = pure acc
            | otherwise = do
                s <- readArray seen c
                if not s
                  then loop (c + 1) first acc
                  else do
                    row <- rowFor c
                    let sep = if first then mempty else char7 '$'
                    loop (c + 1) False (acc <> sep <> char7 '#' <> intDec c <> row)
      loop 0 True mempty

    newBandBits :: Int -> ST s (STUArray s Int Word8)
    newBandBits n = newArray (0, max 0 n) 0

    newSeen :: Int -> ST s (STUArray s Int Bool)
    newSeen n = newArray (0, max 0 n) False
