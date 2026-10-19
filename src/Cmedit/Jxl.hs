{-# LANGUAGE BangPatterns, RankNTypes, ScopedTypeVariables #-}
-- | JPEG XL decoding (ISO/IEC 18181-1): the container, the codestream
-- headers, frames and their sections, and conversion to 8-bit RGBA.
--
-- Pure and dependency-free like the other decoders: 'decodeJxl' takes the
-- file's bytes and returns the first displayed frame as RGBA. The heavy
-- lifting lives in "Cmedit.JxlEntropy" (the entropy coder),
-- "Cmedit.JxlModular" (lossless and LF data) and "Cmedit.JxlVarDct"
-- (lossy DCT data); this module is the orchestration.
module Cmedit.Jxl
  ( decodeJxl
  , isJxl
  ) where

import Control.Monad (forM, forM_, unless, when, replicateM, foldM)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Array.Base (STUArray(..), unsafeAt, unsafeRead, unsafeWrite, unsafeFreeze)
import Data.Array.ST (newArray)
import Data.Array.Unboxed (UArray, listArray)
import qualified Data.Array.Unboxed as U
import Data.Bits
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Word (Word8, Word32, Word64)

import Cmedit.JxlBits
import Cmedit.JxlEntropy
import Cmedit.JxlModular
import Cmedit.JxlPar
import Cmedit.JxlVarDct
import Data.Array (Array)
import qualified Data.Array as A
import Data.Array.Unsafe (castSTUArray)

-- | Does this look like JPEG XL (a bare codestream or the box container)?
isJxl :: BS.ByteString -> Bool
isJxl bs = BS.take 2 bs == BS.pack [0xff, 0x0a] || BS.take 12 bs == jxlBoxSig

jxlBoxSig :: BS.ByteString
jxlBoxSig = BS.pack [0, 0, 0, 0x0c, 0x4a, 0x58, 0x4c, 0x20, 0x0d, 0x0a, 0x87, 0x0a]

-- | Decode the first displayed frame: (width, height, RGBA pixels).
decodeJxl :: Int -> BS.ByteString -> Either String (Int, Int, UArray Int Word8)
decodeJxl maxPixels bs = do
  cs <- codestream bs
  let buf = mkBuf cs
  runST (runJ (decodeCodestream maxPixels buf))

------------------------------------------------------------------------------
-- The container (§18181-2): concatenate jxlc / jxlp payloads.

codestream :: BS.ByteString -> Either String BS.ByteString
codestream bs
  | BS.take 2 bs == BS.pack [0xff, 0x0a] = Right bs
  | BS.take 12 bs == jxlBoxSig = boxes (BS.drop 12 bs) [] False
  | otherwise = Left "not a JPEG XL file"
  where
    be32 b i = foldl (\a k -> (a `shiftL` 8) .|. fromIntegral (BS.index b (i + k))) 0 [0 .. 3] :: Int
    boxes b acc seenC
      | BS.length b < 8 = finish acc
      | otherwise =
          let sz32 = be32 b 0
              ty = BS.take 4 (BS.drop 4 b)
              (hdr, size)
                | sz32 == 1 = if BS.length b < 16 then (16, -1)
                              else (16, be32 b 8 `shiftL` 32 .|. be32 b 12)
                | sz32 == 0 = (8, BS.length b)
                | otherwise = (8, sz32)
          in if size < hdr then finish acc
             else
               let payload = BS.take (size - hdr) (BS.drop hdr b)
                   rest = BS.drop size b
               in if ty == BS.pack [0x6a, 0x78, 0x6c, 0x63]          -- jxlc
                    then if seenC then Left "JPEG XL: duplicate codestream box"
                         else boxes rest (payload : acc) True
                  else if ty == BS.pack [0x6a, 0x78, 0x6c, 0x70]     -- jxlp
                    then boxes rest (BS.drop 4 payload : acc) seenC
                  else boxes rest acc seenC
    finish [] = Left "JPEG XL: container has no codestream"
    finish acc = Right (BS.concat (reverse acc))

------------------------------------------------------------------------------
-- Image header (§A.6 metadata)

data ECInfo = ECInfo
  { ecType, ecBpp, ecExp, ecShift :: !Int
  , ecAssoc :: !Bool
  , ecSpot :: [Float]    -- ^ spot colour: red, green, blue, solidity
  }

data ImgHdr = ImgHdr
  { ihW, ihH, ihOrient :: !Int
  , ihBpp, ihExp :: !Int
  , ihAnim, ihTimecodes :: !Bool
  , ihEC :: [ECInfo]
  , ihXyb :: !Bool
  , ihGrey :: !Bool
  , ihWantIcc :: !Bool
  , ihPreview :: Maybe (Int, Int)
  , ihOpsin :: [Double]       -- ^ 9 matrix entries, 3 biases, 3 quant biases, numerator
  , ihIntensity :: !Double
  , ihUp2, ihUp4, ihUp8 :: Maybe [Double]
  }

f16J :: BR s -> J s Double
f16J br = liftJ (f16 br) >>= maybe (jfail "JPEG XL: invalid float") pure

sizeHeader :: BR s -> J s (Int, Int)
sizeHeader br = liftJ $ do
  div8 <- readBool br
  h <- if div8 then (\v -> (v + 1) * 8) <$> readBits br 5
       else u32 br (1,9) (1,13) (1,18) (1,30)
  ratio <- readBits br 3
  w <- if ratio /= 0 then pure (ratioWidth ratio h)
       else if div8 then (\v -> (v + 1) * 8) <$> readBits br 5
       else u32 br (1,9) (1,13) (1,18) (1,30)
  pure (w, h)

ratioWidth :: Int -> Int -> Int
ratioWidth r h = case r of
  1 -> h; 2 -> h * 12 `div` 10; 3 -> h * 4 `div` 3; 4 -> h * 3 `div` 2
  5 -> h * 16 `div` 9; 6 -> h * 5 `div` 4; _ -> h * 2

bitDepth :: BR s -> J s (Int, Int)
bitDepth br = do
  fl <- liftJ (readBool br)
  if fl then do
    bpp <- liftJ (u32 br (32,0) (16,0) (24,0) (1,6))
    e <- liftJ ((+ 1) <$> readBits br 4)
    let mant = bpp - e - 1
    jguard (mant >= 2 && mant <= 23 && e >= 2 && e <= 8) "JPEG XL: bad float depth"
    pure (bpp, e)
  else do
    bpp <- liftJ (u32 br (8,0) (10,0) (12,0) (1,6))
    jguard (bpp >= 1 && bpp <= 31) "JPEG XL: bad bit depth"
    pure (bpp, 0)

skipName :: BR s -> J s ()
skipName br = do
  len <- liftJ (u32 br (0,0) (0,4) (16,5) (48,10))
  liftJ (skipBits br (8 * len))

extensions :: BR s -> J s ()
extensions br = do
  ext <- liftJ (u64 br)
  lens <- forM [0 .. 63 :: Int] $ \i ->
    if testBit ext i then liftJ (u64 br) else pure 0
  let total = sum lens
  jguard (total >= 0 && total < (1 `shiftL` 40)) "JPEG XL: bad extensions"
  liftJ (skipBits br total)

imageHeader :: BR s -> J s ImgHdr
imageHeader br = do
  sig <- liftJ (readBits br 16)
  jguard (sig == 0x0aff) "not a JPEG XL codestream"
  (w, h) <- sizeHeader br
  allDefault <- liftJ (readBool br)
  let base = ImgHdr { ihW = w, ihH = h, ihOrient = 1, ihBpp = 8, ihExp = 0
                    , ihAnim = False, ihTimecodes = False, ihEC = [], ihXyb = True
                    , ihGrey = False, ihWantIcc = False, ihPreview = Nothing
                    , ihOpsin = defaultOpsin, ihIntensity = 255
                    , ihUp2 = Nothing, ihUp4 = Nothing, ihUp8 = Nothing }
  hdr <- if allDefault then pure base else do
    extra <- liftJ (readBool br)
    (orient, preview, anim, tcs) <- if not extra then pure (1, Nothing, False, False) else do
      o <- liftJ ((+ 1) <$> readBits br 3)
      intr <- liftJ (readBool br)
      when intr (() <$ sizeHeader br)
      hasPrev <- liftJ (readBool br)
      pv <- if not hasPrev then pure Nothing else Just <$> previewHeader br
      hasAnim <- liftJ (readBool br)
      (a, t) <- if not hasAnim then pure (False, False) else liftJ $ do
        _ <- u32 br (100,0) (1000,0) (1,10) (1,30)
        _ <- u32 br (1,0) (1001,0) (1,8) (1,10)
        _ <- u32 br (0,0) (0,3) (0,16) (0,32)
        tc <- readBool br
        pure (True, tc)
      pure (o, pv, a, t)
    (bpp, ex) <- bitDepth br
    _m16 <- liftJ (readBool br)
    nec <- liftJ (u32 br (0,0) (1,0) (2,4) (1,12))
    jguard (nec <= 256) "JPEG XL: too many extra channels"
    ecs <- replicateM nec $ do
      dAlpha <- liftJ (readBool br)
      if dAlpha then pure (ECInfo 0 8 0 0 False []) else do
        ty <- liftJ (readEnum br)
        (b, e) <- bitDepth br
        ds <- liftJ (u32 br (0,0) (3,0) (4,0) (1,3))
        skipName br
        (assoc, spot) <- case ty of
          0 -> (\a -> (a, [])) <$> liftJ (readBool br)
          2 -> do sp <- replicateM_' 4 (f16J br); pure (False, map realToFrac sp)
          5 -> do _ <- liftJ (u32 br (1,0) (0,2) (3,4) (19,8)); pure (False, [])
          _ | ty <= 7 -> pure (False, [])
            | otherwise -> jfail "JPEG XL: unknown extra channel type"
        pure (ECInfo ty b e ds assoc spot)
    xyb <- liftJ (readBool br)
    allDefCE <- liftJ (readBool br)
    (wantIcc, grey) <- if allDefCE then pure (False, False) else do
      wi <- liftJ (readBool br)
      csp <- liftJ (readEnum br)
      jguard (csp <= 3) "JPEG XL: bad colour space"
      unless wi $ do
        when (csp /= 2) $ do
          wp <- liftJ (readEnum br)
          case wp of
            2 -> () <$ replicateM_' 2 (liftJ (u32 br (0,19) (0x80000,19) (0x100000,20) (0x200000,21)))
            _ | wp `elem` [1, 10, 11] -> pure ()
              | otherwise -> jfail "JPEG XL: bad white point"
          when (csp /= 1) $ do
            pr <- liftJ (readEnum br)
            case pr of
              2 -> () <$ replicateM_' 6 (liftJ (u32 br (0,19) (0x80000,19) (0x100000,20) (0x200000,21)))
              _ | pr `elem` [1, 9, 11] -> pure ()
                | otherwise -> jfail "JPEG XL: bad primaries"
        hasGamma <- liftJ (readBool br)
        if hasGamma then () <$ liftJ (readBits br 24) else do
          tf <- liftJ (readEnum br)
          jguard (tf `elem` [1, 2, 8, 13, 16, 17, 18]) "JPEG XL: bad transfer function"
        ri <- liftJ (readEnum br)
        jguard (ri <= 3) "JPEG XL: bad rendering intent"
      pure (wi, csp == 1)
    intensity <- if not extra then pure 255 else do
      allDefTM <- liftJ (readBool br)
      if allDefTM then pure 255 else do
        it <- f16J br
        _ <- f16J br
        _ <- liftJ (readBool br)
        _ <- f16J br
        pure it
    extensions br
    pure base { ihOrient = orient, ihPreview = preview, ihAnim = anim, ihTimecodes = tcs
              , ihBpp = bpp, ihExp = ex, ihEC = ecs, ihXyb = xyb, ihWantIcc = wantIcc
              , ihGrey = grey, ihIntensity = intensity }
  defaultM <- liftJ (readBool br)
  if defaultM then pure hdr else do
    opsin <- if ihXyb hdr then replicateM 16 (f16J br) else pure defaultOpsin
    cw <- liftJ (readBits br 3)
    u2 <- if testBit cw 0 then Just <$> replicateM 15 (f16J br) else pure Nothing
    u4 <- if testBit cw 1 then Just <$> replicateM 55 (f16J br) else pure Nothing
    u8 <- if testBit cw 2 then Just <$> replicateM 210 (f16J br) else pure Nothing
    pure hdr { ihOpsin = opsin, ihUp2 = u2, ihUp4 = u4, ihUp8 = u8 }
  where
    replicateM_' n act = replicateM n act

previewHeader :: BR s -> J s (Int, Int)
previewHeader br = liftJ $ do
  div8 <- readBool br
  h <- if div8 then (* 8) <$> u32 br (16,0) (32,0) (1,5) (33,9)
       else u32 br (1,6) (65,8) (321,10) (1345,12)
  ratio <- readBits br 3
  w <- if ratio /= 0 then pure (ratioWidth ratio h)
       else if div8 then (* 8) <$> u32 br (16,0) (32,0) (1,5) (33,9)
       else u32 br (1,6) (65,8) (321,10) (1345,12)
  pure (w, h)

defaultOpsin :: [Double]
defaultOpsin =
  [ 11.031566901960783, -9.866943921568629, -0.16462299647058826
  , -3.254147380392157, 4.418770392156863, -0.16462299647058826
  , -3.6588512862745097, 2.7129230470588235, 1.9459282392156863
  , -0.0037930732552754493, -0.0037930732552754493, -0.0037930732552754493
  , 1 - 0.05465007330715401, 1 - 0.07005449891748593, 1 - 0.049935103337343655
  , 0.145 ]

-- | Decode (and discard) an embedded ICC profile: it is entropy coded, so
-- the only way past it is through it.
skipIcc :: BR s -> J s ()
skipIcc br = do
  encSize <- liftJ (u64 br)
  jguard (encSize <= (1 `shiftL` 24)) "JPEG XL: ICC profile too large"
  code <- readCode br 41
  st <- liftJ (newCodeSt br code encSize)
  liftJ $ do
    let go !i !p1 !p2
          | i >= encSize = pure ()
          | otherwise = do
              let ctx | i <= 128 = 0
                      | otherwise = iccCtx p1 p2
              b <- readSym code st br ctx 0
              go (i + 1) (b .&. 0xff) p1
    go 0 0 0
  finishCode code st
  where
    iccCtx p1 p2 =
      let c1 | p1 < 16 = if p1 < 2 then p1 + 3 else 5
             | p1 > 240 = 6 + (if p1 == 255 then 1 else 0)
             | isAlpha p1 = 1
             | isNum p1 = 2
             | otherwise = 8
          c2 | p2 < 16 = 2
             | p2 > 240 = 3
             | isAlpha p2 = 0
             | isNum p2 = 1
             | otherwise = 4
      in c1 + 8 * c2
    isAlpha c = let l = c .|. 32 in l >= 97 && l <= 122
    isNum c = c == 44 || c == 46 || (c >= 48 && c < 58)

------------------------------------------------------------------------------
-- Frame header (§C.2? FrameHeader)

data Blend = Blend { blMode, blAlpha :: !Int, blClamp :: !Bool, blSource :: !Int }

data FrameHdr = FrameHdr
  { fhType :: !Int
  , fhModular :: !Bool
  , fhFlags :: !Int
  , fhYCbCr :: !Bool
  , fhJpegUp :: [Int]
  , fhUpsampling :: !Int
  , fhEcUp :: [Int]
  , fhGroupShift :: !Int
  , fhXqm, fhBqm :: !Int
  , fhPasses :: !Int
  , fhPassShifts :: [(Int, (Int, Int))]
  , fhPassShiftK :: [Int]
  , fhLfLevel :: !Int
  , fhX0, fhY0, fhW, fhH :: !Int
  , fhBlend :: Blend
  , fhEcBlend :: [Blend]
  , fhDuration :: !Int
  , fhIsLast :: !Bool
  , fhSaveAsRef :: !Int
  , fhSaveBeforeCT :: !Bool
  , fhGab :: Maybe [Double]
  , fhEpfIters :: !Int
  , fhEpfSharp :: [Double]
  , fhEpfScale :: [Double]
  , fhEpfSigma :: (Double, Double, Double, Double)   -- quant mul, pass0, pass2, border SAD
  , fhEpfModular :: !Double
  }

frameHeader :: BR s -> ImgHdr -> J s FrameHdr
frameHeader br ih = do
  liftJ (zeroPad br)
  allDefault <- liftJ (readBool br)
  let nec = length (ihEC ih)
  if allDefault then pure (defaultFrame ih) else do
    ftype <- liftJ (readBits br 2)
    modular <- liftJ (readBool br)
    flags <- liftJ (u64 br)
    ycbcr <- if ihXyb ih then pure False else liftJ (readBool br)
    let useLf = testBit flags 5
    jup <- if ycbcr && not useLf then liftJ (replicateM 3 (readBits br 2)) else pure [0, 0, 0]
    ups <- if useLf then pure 1 else liftJ (u32 br (1,0) (2,0) (4,0) (8,0))
    ecup <- if useLf then pure (replicate nec 1) else liftJ (replicateM nec (u32 br (1,0) (2,0) (4,0) (8,0)))
    gshift <- if modular then liftJ ((+ 7) <$> readBits br 2) else pure 8
    (xqm, bqm) <- if ihXyb ih && not modular then liftJ ((,) <$> readBits br 3 <*> readBits br 3)
                  else pure (if ihXyb ih && not modular then 3 else 2, 2)
    (npass, pshifts, shk) <- if ftype == 2 then pure (1, [(0, (0, 3))], [0]) else do
      np <- liftJ (u32 br (1,0) (2,0) (3,0) (4,3))
      if np == 1 then pure (1, [(0, (0, 3))], [0]) else do
        nds <- liftJ (u32 br (0,0) (1,0) (2,0) (3,1))
        jguard (nds < np) "JPEG XL: bad passes"
        shifts <- liftJ (replicateM (np - 1) (readBits br 2))
        ds <- liftJ (replicateM nds (u32 br (1,0) (2,0) (4,0) (8,0)))
        lp <- liftJ (replicateM nds (u32 br (0,0) (1,0) (2,0) (0,3)))
        jguard (and (zipWith (<) lp (drop 1 lp)) && all (< np) lp) "JPEG XL: bad passes"
        let mk [] _ = []
            mk ((d, p) : rest) mx = let mn = countTrailingZeros d in (p, (mn, mx)) : mk rest mn
            listed = mk (zip ds lp) 3
            lastMax = if null listed then 3 else fst (snd (last listed))
            table = listed ++ [(np - 1, (0, lastMax))]
        pure (np, table, shifts ++ [0])
    lfl <- if ftype == 1 then liftJ ((+ 1) <$> readBits br 2) else pure 0
    crop <- if ftype /= 1 then liftJ (readBool br) else pure False
    (x0, y0) <- if crop && ftype /= 2 then liftJ $ do
                  a <- u32 br (0,8) (256,11) (2304,14) (18688,30)
                  b <- u32 br (0,8) (256,11) (2304,14) (18688,30)
                  pure (unpackSigned a, unpackSigned b)
                else pure (0, 0)
    (fw, fh) <- if crop then liftJ $ do
                  a <- u32 br (0,8) (256,11) (2304,14) (18688,30)
                  b <- u32 br (0,8) (256,11) (2304,14) (18688,30)
                  pure (a, b)
                else pure (ihW ih, ihH ih)
    let normal = ftype == 0 || ftype == 3
        full = x0 <= 0 && y0 <= 0 && fw + x0 >= ihW ih && fh + y0 >= ihH ih
        partial = crop && not full
        readBlend = do
          mode <- liftJ (u32 br (0,0) (1,0) (2,0) (3,2))
          jguard (mode <= 4) "JPEG XL: bad blend mode"
          let usesAlpha = nec > 0 && (mode == 2 || mode == 3)
          a <- if usesAlpha then liftJ (u32 br (0,0) (1,0) (2,0) (3,3)) else pure 0
          c <- if usesAlpha || mode == 4 then liftJ (readBool br) else pure False
          s <- if mode /= 0 || partial then liftJ (readBits br 2) else pure 0
          pure (Blend mode a c s)
    (bl, ecbl) <- if normal then (,) <$> readBlend <*> replicateM nec readBlend
                  else pure (Blend 0 0 False 0, replicate nec (Blend 0 0 False 0))
    dur <- if normal && ihAnim ih then liftJ (u32 br (0,0) (1,0) (0,8) (0,32)) else pure 0
    when (normal && ihTimecodes ih) $ () <$ liftJ (readBits br 32)
    isLast <- if normal then liftJ (readBool br) else pure (ftype == 0)
    sar <- if ftype /= 1 && not isLast then liftJ (readBits br 2) else pure 0
    let resets = blMode bl == 0 && not partial
    sbct <- if ftype == 2 || (resets && not isLast && (dur == 0 || sar /= 0) && ftype /= 1)
              then liftJ (readBool br) else pure (not normal)
    skipName br
    -- restoration filters
    rfDefault <- liftJ (readBool br)
    (gab, epfIters, sharp, scale, sigma, sigMod) <-
      if rfDefault then pure (Just gabDefault, 2, sharpDefault, [40, 5, 3.5], sigmaDefault, 1) else do
        gabOn <- liftJ (readBool br)
        g <- if not gabOn then pure Nothing else do
          custom <- liftJ (readBool br)
          if custom then Just <$> replicateM 6 (f16J br) else pure (Just gabDefault)
        iters <- liftJ (readBits br 2)
        if iters == 0 then do
          extensions br
          pure (g, 0, sharpDefault, [40, 5, 3.5], sigmaDefault, 1)
        else do
          sc <- if not modular then liftJ (readBool br) else pure False
          sh <- if sc then replicateM 8 (f16J br) else pure sharpDefault
          wc <- liftJ (readBool br)
          sl <- if wc then do v <- replicateM 3 (f16J br); liftJ (skipBits br 32); pure v
                else pure [40, 5, 3.5]
          sgc <- liftJ (readBool br)
          sg <- if not sgc then pure sigmaDefault else do
            qm <- if not modular then f16J br else pure 0.46
            p0 <- f16J br
            p2 <- f16J br
            bs <- f16J br
            pure (qm, p0, p2, bs)
          sm <- if modular then f16J br else pure 1
          extensions br
          pure (g, iters, sh, sl, sg, sm)
    extensions br
    pure FrameHdr
      { fhType = ftype, fhModular = modular, fhFlags = flags, fhYCbCr = ycbcr
      , fhJpegUp = jup, fhUpsampling = ups, fhEcUp = ecup, fhGroupShift = gshift
      , fhXqm = xqm, fhBqm = bqm, fhPasses = npass, fhPassShifts = pshifts
      , fhPassShiftK = shk, fhLfLevel = lfl
      , fhX0 = x0, fhY0 = y0, fhW = fw, fhH = fh, fhBlend = bl, fhEcBlend = ecbl
      , fhDuration = dur, fhIsLast = isLast, fhSaveAsRef = sar, fhSaveBeforeCT = sbct
      , fhGab = gab, fhEpfIters = epfIters, fhEpfSharp = sharp, fhEpfScale = scale
      , fhEpfSigma = sigma, fhEpfModular = sigMod }

gabDefault, sharpDefault :: [Double]
gabDefault = concat (replicate 3 [0.115169525, 0.061248592])
sharpDefault = [ fromIntegral i / 7 | i <- [0 .. 7 :: Int] ]

sigmaDefault :: (Double, Double, Double, Double)
sigmaDefault = (0.46, 0.9, 6.5, 2 / 3)

defaultFrame :: ImgHdr -> FrameHdr
defaultFrame ih = FrameHdr
  { fhType = 0, fhModular = False, fhFlags = 0, fhYCbCr = False, fhJpegUp = [0, 0, 0]
  , fhUpsampling = 1, fhEcUp = map (const 1) (ihEC ih), fhGroupShift = 8
  , fhXqm = if ihXyb ih then 3 else 2, fhBqm = 2, fhPasses = 1
  , fhPassShifts = [(0, (0, 3))], fhPassShiftK = [0], fhLfLevel = 0
  , fhX0 = 0, fhY0 = 0, fhW = ihW ih, fhH = ihH ih
  , fhBlend = Blend 0 0 False 0, fhEcBlend = map (const (Blend 0 0 False 0)) (ihEC ih)
  , fhDuration = 0, fhIsLast = True, fhSaveAsRef = 0, fhSaveBeforeCT = False
  , fhGab = Just gabDefault, fhEpfIters = 2, fhEpfSharp = sharpDefault
  , fhEpfScale = [40, 5, 3.5], fhEpfSigma = sigmaDefault, fhEpfModular = 1 }

-- Sample dimensions of a frame's colour channels.
frameDims :: FrameHdr -> (Int, Int)
frameDims fh =
  let u = fhUpsampling fh
      w1 = (fhW fh + u - 1) `div` u
      h1 = (fhH fh + u - 1) `div` u
      sh = 3 * fhLfLevel fh
      d x = (x + (1 `shiftL` sh) - 1) `shiftR` sh
  in (d w1, d h1)

------------------------------------------------------------------------------
-- The table of contents: section byte ranges in logical order.

data Toc = Toc { tocSingle :: !Bool, tocSec :: [(Int, Int)], tocEnd :: !Int }

readToc :: BR s -> FrameHdr -> J s Toc
readToc br fh = do
  let (w, h) = frameDims fh
      gdim = 1 `shiftL` fhGroupShift fh
      ngroups = ceilDiv w gdim * ceilDiv h gdim
      nlf = ceilDiv w (8 * gdim) * ceilDiv h (8 * gdim)
      npass = fhPasses fh
      n = if ngroups == 1 && npass == 1 then 1 else 1 + nlf + 1 + npass * ngroups
  jguard (n <= 65536) "JPEG XL: too many sections"
  permuted <- liftJ (readBool br)
  perm <- if not permuted then pure Nothing else do
    code <- readCode br 8
    st <- liftJ (newCodeSt br code 65536)
    p <- readPermutation br code st n 0
    finishCode code st
    pure (Just p)
  liftJ (zeroPad br)
  sizes <- liftJ (replicateM n (u32 br (0,10) (1024,14) (17408,22) (4211712,30)))
  liftJ (zeroPad br)
  start <- liftJ ((`shiftR` 3) <$> brPos br)
  let offs = scanl (+) start sizes
      ranges = zip offs sizes
      -- logical section k is stored at bitstream position perm[k]
      rarr = listArray (0, n - 1) (map fst ranges) :: UArray Int Int
      sarr = listArray (0, n - 1) sizes :: UArray Int Int
      logical = case perm of
        Nothing -> ranges
        Just p -> [ (rarr `unsafeAt` j, sarr `unsafeAt` j) | k <- [0 .. n - 1], let j = p `unsafeAt` k ]
  pure (Toc (n == 1) logical (last offs))

ceilDiv :: Int -> Int -> Int
ceilDiv a b = (a + b - 1) `div` b

------------------------------------------------------------------------------
-- The codestream

data Decoded s = DecodedModular [Chan s] Int

decodeCodestream :: Int -> Buf -> J s (Int, Int, UArray Int Word8)
decodeCodestream maxPix buf = do
  br <- liftJ (newBR buf 0)
  ih <- imageHeader br
  jguard (ihW ih > 0 && ihH ih > 0 && ihW ih * ihH ih <= maxPix) "JPEG XL: image too large"
  when (ihWantIcc ih) (skipIcc br)
  -- a preview frame comes first; skip it whole
  case ihPreview ih of
    Nothing -> pure ()
    Just (pw, ph) -> do
      fh <- frameHeader br ih { ihW = pw, ihH = ph }
      toc <- readToc br fh
      liftJ (brSetPos br (8 * tocEnd toc))
  frames br ih (Refs (replicate 4 Nothing) (replicate 4 Nothing) 0 0)

-- | A rendered frame, or a canvas: float planes holding the colour
-- channels (XYB, or the image's colour space scaled to [0, 1]) and the
-- extra channels (scaled to [0, 1]), all w x h with one row stride.
data Img s = Img
  { imW, imH, imStride :: !Int
  , imColour :: [FPlane s]
  , imExtra  :: [ExtraPlane s]
  , imDomain :: !Int         -- ^ colour domain: domEncoded, domXyb or domYCbCr
  }

-- Extra channels may stay constant after inverse squeeze. Keep the ordinary
-- float path for nonconstants and for interpolation; do not assume floating
-- point weighted sums preserve constant bits.
data ExtraPlane s = ConstantExtra !Float | DenseExtra !(FPlane s)

{-# INLINE readExtra #-}
readExtra :: ExtraPlane s -> Int -> ST s Float
readExtra (ConstantExtra v) _ = pure v
readExtra (DenseExtra p) i = unsafeRead p i

materializeExtra :: Int -> Int -> Int -> ExtraPlane s -> ST s (FPlane s)
materializeExtra _ _ _ (DenseExtra p) = pure p
materializeExtra w h st (ConstantExtra v) = do
  p <- newArray (0, max 0 (st * h - 1)) 0
  forLoopJ 0 (h - 1) $ \y -> forLoopJ 0 (w - 1) $ \x ->
    unsafeWrite p (y * st + x) v
  pure p

-- Colour domains of an 'Img': the image's own colour space (scaled to
-- [0, 1], not clamped), XYB, or (Cb, Y, Cr).
domEncoded, domXyb, domYCbCr :: Int
domEncoded = 0
domXyb = 1
domYCbCr = 2

-- | Reference slots (0..3) for blending and patches, and the LF frames
-- (by level) that later frames take their LF image from.
data Refs s = Refs
  { rfSlots :: [Maybe (Img s)]
  , rfLf :: [Maybe (Img s)]
  , rfVisible, rfInvisible :: !Int   -- ^ frames before this one (seed the noise)
  }

-- | The per-frame features signalled in LfGlobal.
data Features = Features
  { ftPatches :: [PatchRef]
  , ftSplines :: Maybe ([QSpline], Int)    -- ^ quantised splines, quant adjust
  , ftNoise :: Maybe (UArray Int Float)   -- ^ the noise LUT (9 entries)
  , ftCorr :: (Float, Float)              -- ^ base correlations of X and B
  }

-- | A patch: a rectangle of a reference frame and where it is pasted, with
-- per-channel-group blending (mode, alpha channel, clamp).
data PatchRef = PatchRef
  { prRef, prX0, prY0, prW, prH :: !Int
  , prTargets :: [(Int, Int, [(Int, Int, Bool)])]
  }

frames :: BR s -> ImgHdr -> Refs s -> J s (Int, Int, UArray Int Word8)
frames br ih refs = do
  fh <- frameHeader br ih
  toc <- readToc br fh
  jguard (tocEnd toc <= bufLen (brBuf br)) "JPEG XL: truncated file"
  let normal = fhType fh == 0 || fhType fh == 3
      shown = normal && (fhIsLast fh || fhDuration fh /= 0)
      canRef = not (fhIsLast fh) && (fhDuration fh == 0 || fhSaveAsRef fh /= 0) && fhType fh /= 1
      lfFrame = fhType fh == 1
      next refs' = do
        jguard (not (fhIsLast fh)) "JPEG XL: no displayable frame"
        liftJ (brSetPos br (8 * tocEnd toc))
        -- the noise seed counts displayed frames, and normal frames that
        -- are not displayed (measured against libjxl: reference-only and
        -- LF frames count as neither)
        frames br ih (if shown then refs' { rfVisible = rfVisible refs' + 1 }
                      else if normal then refs' { rfInvisible = rfInvisible refs' + 1 }
                      else refs')
      (sw, sh) = frameDims fh
  if not (shown || canRef || lfFrame) then next refs else do
    jguard (sw * sh <= 4 * ihW ih * ihH ih + 65536) "JPEG XL: frame too large"
    (img0, feats) <-
      if fhModular fh then decodeModularFrame br ih fh toc
      else do
        -- a frame at LF level n takes its LF image from the level n + 1 frame
        let lfImg = if testBit (fhFlags fh) 5 then rfLf refs !! min 3 (fhLfLevel fh) else Nothing
        jguard (not (testBit (fhFlags fh) 5) || isJust lfImg) "JPEG XL: missing LF frame"
        decodeVarDctFrame br ih fh toc lfImg
    img1 <- applyPatches ih refs (ftPatches feats) img0
    case ftSplines feats of
      Just (qs, qa) | length (imColour img1) == 3 -> do
        jguard (splineArea qs qa (ftCorr feats) <= min (1 `shiftL` 42) (1024 * fhW fh * fhH fh + (1 `shiftL` 32)))
          "JPEG XL: spline area too large"
        liftJ (renderSplines fh qs qa (ftCorr feats) img1)
      _ -> pure ()
    img2 <- liftJ (upsampleFrame ih fh img1)
    -- noise comes after upsampling (libjxl's pipeline order, measured)
    case ftNoise feats of
      Just lut | length (imColour img2) == 3 ->
        -- libjxl's frame indices count the frame being decoded itself
        liftJ (applyNoise fh (rfVisible refs + fromEnum shown) (rfInvisible refs + fromEnum (normal && not shown))
                 lut (ftCorr feats) img2)
      _ -> pure ()
    if lfFrame
      then next refs { rfLf = setAt (fhLfLevel fh - 1) (Just img2) (rfLf refs) }
      else do
        -- a frame not saved before the colour transform is blended and
        -- kept in the image's own colour space (only converted when a
        -- blend or a reference will actually see it)
        let fullReplace = fhX0 fh == 0 && fhY0 fh == 0 && imW img2 == ihW ih && imH img2 == ihH ih
                          && all ((== 0) . blMode) (fhBlend fh : fhEcBlend fh)
            srcDomains = [ imDomain r | b <- fhBlend fh : fhEcBlend fh, Just r <- [rfSlots refs !! blSource b] ]
            convert = normal && imDomain img2 /= domEncoded
                      && ((not (fhSaveBeforeCT fh) && (not fullReplace || canRef))
                          || (not fullReplace && any (/= imDomain img2) srcDomains))
        img3 <- if convert then liftJ (toEncoded ih img2) else pure img2
        canvas <- if normal then blendFrame ih fh refs img3 else pure img3
        let refs' = if canRef then refs { rfSlots = setAt (fhSaveAsRef fh) (Just canvas) (rfSlots refs) }
                    else refs
        if shown then liftJ (renderImg ih canvas) else next refs'

setAt :: Int -> a -> [a] -> [a]
setAt i v xs = take i xs ++ [v] ++ drop (i + 1) xs

-- A reader positioned at a section; single-section frames share one.
sectionReader :: BR s -> Toc -> Int -> J s (BR s, Int)
sectionReader br toc k
  | tocSingle toc = pure (br, snd (head (tocSec toc)) + fst (head (tocSec toc)))
  | otherwise = do
      let (off, size) = tocSec toc !! k
      jguard (off + size <= bufLen (brBuf br)) "JPEG XL: truncated file"
      r <- liftJ (newBR (brBuf br) (8 * off))
      pure (r, off + size)

checkEnd :: BR s -> Int -> J s ()
checkEnd r end = do
  over <- liftJ (brOverrun r end)
  jguard (not over) "JPEG XL: truncated or corrupt section"

------------------------------------------------------------------------------
-- The global modular image: every channel of a Modular frame, or the
-- extra channels (alpha and so on) of a VarDCT one.

data GM s = GM
  { gmTree  :: Maybe MATree
  , gmHdr   :: Maybe ModHeader
  , gmChans :: [Chan s]
  , gmRest  :: [Chan s]   -- ^ channels left to the LF and pass groups
  }

-- | GlobalModular: an optional global tree, then the header and the
-- channels small enough for the global stream.
readGlobalModular :: BR s -> Int -> Int -> Int -> [ChInfo] -> J s (GM s)
readGlobalModular r w h gdim chans0 = do
  hasTree <- liftJ (readBool r)
  gtree <- if not hasTree then pure Nothing else
    Just <$> readTree r (min (1 `shiftL` 22) (1024 + w * h * length chans0 `div` 16))
  if null chans0 then pure (GM gtree Nothing [] []) else do
    mh <- readModHeader r chans0
    let tchans = mhChannels mh
        nbMeta = mhNbMeta mh
        gmCount = length (takeWhile id [ i < nbMeta || (ciW c <= gdim && ciH c <= gdim)
                                       | (i, c) <- zip [0 :: Int ..] tchans ])
    chans <- liftJ (mapM newChan tchans)
    tree0 <- treeFor r gtree mh tchans
    decodeChannels r tree0 (mhWP mh) 0 chans gmCount
    pure (GM gtree (Just mh) chans (drop gmCount chans))

-- The part of an LF group section holding the global image's channels
-- that are shifted by at least 3 both ways.
gmLfGroup :: BR s -> GM s -> Int -> Int -> Int -> Int -> Int -> J s ()
gmLfGroup r gm bd gdim lfcols nlf g = do
  let gx = g `mod` lfcols
      gy = g `div` lfcols
      sel = [ (c, rect) | c <- gmRest gm, chHS c >= 3, chVS c >= 3
            , let gw = (8 * gdim) `shiftR` chHS c
                  gh = (8 * gdim) `shiftR` chVS c
                  rect = clipRect c (gx * gw) (gy * gh) gw gh
            , nonEmptyRect rect ]
  unless (null sel) $ groupStream r (gmTree gm) bd (1 + nlf + g) sel

-- The part of a pass group section holding the remaining channels whose
-- shift falls in the pass's range.
gmPassGroup :: BR s -> GM s -> Int -> Int -> Int -> (Int, Int) -> Int -> Int -> J s ()
gmPassGroup r gm bd gdim gcols (mn, mx) sidx g = do
  let gx = g `mod` gcols
      gy = g `div` gcols
      sel = [ (c, rect) | c <- gmRest gm, chHS c < 3 || chVS c < 3
            , let s = min (chHS c) (chVS c)
            , s >= mn && s < mx
            , let gw = gdim `shiftR` chHS c
                  gh = gdim `shiftR` chVS c
                  rect = clipRect c (gx * gw) (gy * gh) gw gh
            , nonEmptyRect rect ]
  unless (null sel) $ groupStream r (gmTree gm) bd sidx sel

-- The pass groups of one pass as independent jobs (Astra's observation
-- that each group's sub-bitstream is self-contained: its own reader,
-- header, tree-or-global, channels and inverse transforms over the
-- immutable file bytes), decoded on worker threads and pasted into the
-- frame's channels in group order on this thread, with a bounded number
-- of results ahead of the paste (Fable's rolling lookahead). Output and
-- error messages are those of the sequential loop: the same bytes are
-- written to the same cells, and the first failing group in index order
-- is the one reported.
passGroupsPar :: forall s. Int -> BR s -> Toc -> GM s -> Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int) -> J s ()
passGroupsPar nworkers br toc gm bd gdim gcols ngroups nlf p (mn, mx) = do
  let buf = brBuf br
      secs = A.listArray (0, length (tocSec toc) - 1) (tocSec toc) :: Array Int (Int, Int)
      selFor g =
        let gx = g `mod` gcols
            gy = g `div` gcols
        in [ (c, rect) | c <- gmRest gm, chHS c < 3 || chVS c < 3
           , let s = min (chHS c) (chVS c)
           , s >= mn && s < mx
           , let gw = gdim `shiftR` chHS c
                 gh = gdim `shiftR` chVS c
                 rect = clipRect c (gx * gw) (gy * gh) gw gh
           , nonEmptyRect rect ]
      -- Only each group's channel selection is kept for the whole pass. The
      -- decoded result is built afresh inside the job, so once it has been
      -- pasted nothing refers to it: memoising the results in this array
      -- instead retained every group's output until the pass ended (+300 MiB
      -- on a 4096x4096 lossless RGBA image).
      sels = A.listArray (0, ngroups - 1) (map selFor [0 .. ngroups - 1])
      runGroup g =
        let sel = sels A.! g
            infos = [ ChInfo cw ch (chHS c) (chVS c) | (c, (_, _, cw, ch)) <- sel ]
            (off, size) = secs A.! (1 + nlf + 1 + p * ngroups + g)
            sidx = 1 + 3 * nlf + 17 + p * ngroups + g
        in passGroupJob buf off size (gmTree gm) bd sidx infos
      paste :: [(Chan s, (Int, Int, Int, Int))] -> [UArray Int Int32] -> ST s ()
      paste sel outs = forM_ (zip sel outs) $ \((dst, (x0, y0, cw, ch)), src) ->
        forLoopJ 0 (ch - 1) $ \y -> do
          let so = y * cw
              d0 = (y0 + y) * chW dst + x0
          forLoopJ 0 (cw - 1) $ \x ->
            unsafeWrite (chBuf dst) (d0 + x) (src `unsafeAt` (so + x))
  failed <- liftJ $ unsafeIOToST $ do
    err <- newIORef Nothing
    parOrdered nworkers (2 * nworkers) ngroups
      (\g -> pure (runGroup g))
      (\g r -> case r of
          Left e -> writeIORef err (Just e) >> pure False
          Right outs -> unsafeSTToIO (paste (sels A.! g) outs) >> pure True)
    readIORef err
  maybe (pure ()) jfail failed

-- One pass group's sub-stream decoded on its own: a pure value of the
-- file bytes and the group's parameters, so it may be computed on any
-- thread. The guards are 'sectionReader' and 'checkEnd' verbatim.
passGroupJob :: Buf -> Int -> Int -> Maybe MATree -> Int -> Int -> [ChInfo] -> Either String [UArray Int Int32]
passGroupJob buf off size gtree bd sidx infos
  | off + size > bufLen buf = Left "JPEG XL: truncated file"
  | otherwise = runST $ runJ $ do
      r <- liftJ (newBR buf (8 * off))
      outs <- if null infos then pure [] else modularStream r gtree bd sidx infos
      checkEnd r (off + size)
      liftJ (mapM (unsafeFreeze . chBuf) outs)


gmFinish :: GM s -> Int -> J s [CompactChan s]
gmFinish gm bd = case gmHdr gm of
  Nothing -> pure []
  Just mh -> inverseTransformsCompact bd (mhWP mh) (mhTransforms mh) (gmChans gm)

clipRect :: Chan s -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
clipRect c x0 y0 gw gh =
  let x1 = min (chW c) (x0 + gw)
      y1 = min (chH c) (y0 + gh)
  in (x0, y0, max 0 (x1 - x0), max 0 (y1 - y0))

nonEmptyRect :: (Int, Int, Int, Int) -> Bool
nonEmptyRect (_, _, a, b) = a > 0 && b > 0

treeFor :: BR s -> Maybe MATree -> ModHeader -> [ChInfo] -> J s MATree
treeFor r gtree mh tchans
  | mhGlobalTree mh = maybe (jfail "JPEG XL: global MA tree missing") pure gtree
  | otherwise = readTree r (min (1 `shiftL` 20) (1024 + sum [ ciW c * ciH c | c <- tchans ]))

-- | A self-contained modular sub-bitstream (header, tree, channels, inverse
-- transforms): LF coefficients, HF metadata, raw dequantisation matrices.
modularStream :: BR s -> Maybe MATree -> Int -> Int -> [ChInfo] -> J s [Chan s]
modularStream r gtree bd sidx infos = do
  mh <- readModHeader r infos
  let tchans = mhChannels mh
  chans <- liftJ (mapM newChan tchans)
  tree <- treeFor r gtree mh tchans
  decodeChannels r tree (mhWP mh) sidx chans (length chans)
  outs <- inverseTransforms bd (mhWP mh) (mhTransforms mh) chans
  jguard (length outs == length infos) "JPEG XL: transform channel mismatch"
  pure outs

-- One group's modular sub-stream: decode cropped channels, undo its own
-- transforms, paste the result into the frame's channels.
groupStream :: BR s -> Maybe MATree -> Int -> Int -> [(Chan s, (Int, Int, Int, Int))] -> J s ()
groupStream r gtree bd sidx sel = do
  let infos = [ ChInfo cw ch (chHS c) (chVS c) | (c, (_, _, cw, ch)) <- sel ]
  outs <- modularStream r gtree bd sidx infos
  liftJ $ forM_ (zip sel outs) $ \((dst, (x0, y0, cw, ch)), src) ->
    forLoopJ 0 (ch - 1) $ \y -> do
      let so = y * cw
          d0 = (y0 + y) * chW dst + x0
      forLoopJ 0 (cw - 1) $ \x ->
        unsafeRead (chBuf src) (so + x) >>= unsafeWrite (chBuf dst) (d0 + x)

-- | Run @f a b@ over disjoint row bands [a, b) of @h@ rows, one band per
-- capability, on worker threads ('Cmedit.JxlPar.parOrdered': joined before
-- this returns, killed and joined if the caller is interrupted). Sound
-- when each band writes only its own rows of arrays private to the
-- enclosing runST and reads nothing another band writes: the result is
-- then the sequential one. One capability (or too few rows) runs the
-- sequential code.
parRows :: Int -> (Int -> Int -> ST s ()) -> ST s ()
parRows h f = do
  n <- unsafeIOToST parWorkers
  let nb = max 1 (min (min 4 n) (h `div` 64))
      band k = (k * h `div` nb, (k + 1) * h `div` nb)
  if nb <= 1 then f 0 h else unsafeIOToST $
    parOrdered nb nb nb (\k -> let (a, b) = band k in unsafeSTToIO (f a b)) (\_ _ -> pure True)

{-# INLINE forLoopStep' #-}
forLoopStep' :: Int -> Int -> Int -> (Int -> ST s ()) -> ST s ()
forLoopStep' lo hi step f = go lo
  where go !i | i > hi = pure ()
              | otherwise = f i >> go (i + step)

{-# INLINE forLoopJ #-}
forLoopJ :: Int -> Int -> (Int -> ST s ()) -> ST s ()
forLoopJ lo hi f = go lo
  where go !i | i > hi = pure ()
              | otherwise = f i >> go (i + 1)

-- | The features at the head of LfGlobal (patches; splines and noise are
-- refused before we get here).
readFeatures :: BR s -> ImgHdr -> FrameHdr -> J s Features
readFeatures r ih fh = do
  ps <- readPatches r ih fh
  sp <- if testBit (fhFlags fh) 4 then Just <$> readSplines r fh else pure Nothing
  noise <- if not (testBit (fhFlags fh) 0) then pure Nothing else liftJ $ do
    vs <- replicateM 8 (readBits r 10)
    let lut = map (\v -> fromIntegral v / 1024) vs
    pure (Just (listArray (0, 8) (lut ++ [last lut])))
  pure (Features ps sp noise (0, 1))

-- | A quantised spline: control points, colour DCTs (X, Y, B) and sigma DCT.
data QSpline = QSpline { qsPoints :: [(Int, Int)], qsManhattan :: !Int, qsXyb :: [[Int]], qsSigma :: [Int] }

readSplines :: BR s -> FrameHdr -> J s ([QSpline], Int)
readSplines r fh = do
  code <- readCode r 6
  st <- liftJ (newCodeSt r code 65536)
  let rd ctx = liftJ (readSym code st r ctx 0)
      npix = fhW fh * fhH fh
  n0 <- rd 2
  jguard (n0 < min (1 `shiftL` 24) (npix `div` 4)) "JPEG XL: too many splines"
  let n = n0 + 1
  x0 <- rd 1
  y0 <- rd 1
  rest <- replicateM (n - 1) ((,) <$> (unpackSigned <$> rd 1) <*> (unpackSigned <$> rd 1))
  let starts = scanl (\(a, b) (dx, dy) -> (a + dx, b + dy)) (x0, y0) rest
  qa <- unpackSigned <$> rd 0
  let one acc start = do
        np <- rd 3
        jguard (acc + np <= min (1 `shiftL` 20) (npix `div` 2)) "JPEG XL: too many spline points"
        let pts !k cur dlt man ps
              | k >= np = pure (reverse ps, man)
              | otherwise = do
                  dx <- unpackSigned <$> rd 4
                  dy <- unpackSigned <$> rd 4
                  let dlt' = (fst dlt + dx, snd dlt + dy)
                      cur' = (fst cur + fst dlt', snd cur + snd dlt')
                  jguard (cur' /= cur) "JPEG XL: repeated spline point"
                  pts (k + 1) cur' dlt' (man + abs (fst dlt') + abs (snd dlt')) (cur' : ps)
        (ps, man) <- pts (0 :: Int) start (0, 0) 0 [start]
        xyb <- replicateM 3 (replicateM 32 (unpackSigned <$> rd 5))
        sig <- replicateM 32 (unpackSigned <$> rd 5)
        pure (QSpline ps man xyb sig, acc + np)
      loop _ [] out = pure (reverse out)
      loop acc (s0 : ss) out = do
        (q, acc') <- one acc s0
        loop acc' ss (q : out)
  qs <- loop 0 starts []
  finishCode code st
  pure (qs, qa)

readPatches :: BR s -> ImgHdr -> FrameHdr -> J s [PatchRef]
readPatches r ih fh
  | not (testBit (fhFlags fh) 1) = pure []
  | otherwise = do
      code <- readCode r 10
      st <- liftJ (newCodeSt r code 65536)
      let rd ctx = liftJ (readSym code st r ctx 0)
          nec = length (ihEC ih)
          alphas = [ i | (i, e) <- zip [0 ..] (ihEC ih), ecType e == 0 ]
          maxRefs = min (1 `shiftL` 24) (fhW fh * fhH fh `div` 16)
      nrefs <- rd 0
      jguard (nrefs <= maxRefs) "JPEG XL: too many patches"
      let refLoop 0 _ acc = pure (reverse acc)
          refLoop k total acc = do
            ref <- rd 1
            jguard (ref < 4) "JPEG XL: bad patch reference"
            x0 <- rd 3
            y0 <- rd 3
            pw' <- (+ 1) <$> rd 2
            ph' <- (+ 1) <$> rd 2
            count <- (+ 1) <$> rd 7
            jguard (total + count <= 4 * maxRefs) "JPEG XL: too many patches"
            let tgtLoop 0 _ tacc = pure (reverse tacc)
                tgtLoop n prev tacc = do
                  (x, y) <- case prev of
                    Nothing -> (,) <$> rd 4 <*> rd 4
                    Just (px, py) -> do
                      dx <- unpackSigned <$> rd 6
                      dy <- unpackSigned <$> rd 6
                      pure (px + dx, py + dy)
                  bl <- replicateM (nec + 1) $ do
                    mode <- rd 5
                    jguard (mode <= 7) "JPEG XL: bad patch blend mode"
                    a <- if mode >= 4 && length alphas >= 2 then rd 8
                         else pure (case alphas of { (a0 : _) -> a0; [] -> 0 })
                    c <- if mode >= 3 then (/= 0) <$> rd 9 else pure False
                    pure (mode, a, c)
                  tgtLoop (n - 1 :: Int) (Just (x, y)) ((x, y, bl) : tacc)
            tgts <- tgtLoop count Nothing []
            refLoop (k - 1) (total + count) (PatchRef ref x0 y0 pw' ph' tgts : acc)
      ps <- refLoop nrefs 0 []
      finishCode code st
      pure ps

-- | LfChannelDequantization: the LF multipliers (m_x, m_y, m_b; the
-- modular XYB path divides them by 128).
lfDequant :: BR s -> J s [Float]
lfDequant r = do
  dflt <- liftJ (readBool r)
  if dflt then pure [1 / 32, 1 / 4, 1 / 2]
  else map realToFrac <$> replicateM 3 (f16J r)

------------------------------------------------------------------------------
-- Modular frames

decodeModularFrame :: BR s -> ImgHdr -> FrameHdr -> Toc -> J s (Img s, Features)
decodeModularFrame br ih fh toc = do
  let (w, h) = frameDims fh
      gdim = 1 `shiftL` fhGroupShift fh
      gcols = ceilDiv w gdim
      ngroups = gcols * ceilDiv h gdim
      lfcols = ceilDiv w (8 * gdim)
      nlf = lfcols * ceilDiv h (8 * gdim)
      ncolour = if not (fhYCbCr fh) && not (ihXyb ih) && ihGrey ih then 1 else 3
      chans0 = replicate ncolour (ChInfo w h 0 0) ++ ecInfos ih fh w h
      bd = ihBpp ih
  jguard (not (fhYCbCr fh)) "JPEG XL: YCbCr modular frames are not supported yet"
  (r0, end0) <- sectionReader br toc 0
  feats <- readFeatures r0 ih fh
  mlf <- lfDequant r0
  gm <- readGlobalModular r0 w h gdim chans0
  unless (tocSingle toc) (checkEnd r0 end0)
  forM_ [0 .. nlf - 1] $ \g -> do
    (r, end) <- sectionReader br toc (1 + g)
    gmLfGroup r gm bd gdim lfcols nlf g
    unless (tocSingle toc) (checkEnd r end)
  -- HfGlobal is empty for modular frames
  nworkers <- liftJ (unsafeIOToST (min 4 <$> parWorkers))
  if tocSingle toc || nworkers <= 1
    then forM_ (fhPassShifts fh) $ \(p, shifts) ->
      forM_ [0 .. ngroups - 1] $ \g -> do
        (r, end) <- sectionReader br toc (1 + nlf + 1 + p * ngroups + g)
        gmPassGroup r gm bd gdim gcols shifts (1 + 3 * nlf + 17 + p * ngroups + g) g
        unless (tocSingle toc) (checkEnd r end)
    else forM_ (fhPassShifts fh) $ \(p, shifts) ->
      passGroupsPar nworkers br toc gm bd gdim gcols ngroups nlf p shifts
  when (tocSingle toc) (checkEnd br (tocEnd toc))
  final <- gmFinish gm bd
  jguard (length final == ncolour + length (ihEC ih)) "JPEG XL: channel count mismatch"
  colourCh <- liftJ (mapM materializeChan (take ncolour final))
  colour <- liftJ $
    if not (ihXyb ih)
      then do
        let sc = 1 / fromIntegral ((1 `shiftL` bd) - 1 :: Int) :: Float
        forM colourCh $ \c -> intsToFloats c sc
      else do
        -- lossy modular: the channels are quantised XYB (Y, X, B - Y)
        let [cy, cx, cb] = colourCh
            [mx, my, mb] = map (/ 128) mlf
        planes <- mapM (castSTUArray . chBuf) [cx, cy, cb]
        let [px, py, pb] = planes
        forLoopJ 0 (w * h - 1) $ \i -> do
          yv <- rdC (chBuf cy) i
          xv <- rdC (chBuf cx) i
          bv <- rdC (chBuf cb) i
          unsafeWrite px i (fromIntegral xv * mx)
          unsafeWrite py i (fromIntegral yv * my)
          unsafeWrite pb i (fromIntegral (bv + yv) * mb)
        sigma <- newArray (0, ceilDiv w 8 * ceilDiv h 8) (realToFrac (fhEpfModular fh))
        restoration fh planes w w h sigma (ceilDiv w 8)
        pure planes
  extra <- liftJ (extraToFloats ih fh w h w (drop ncolour final))
  pure (Img w h w colour extra (if ihXyb ih then domXyb else domEncoded), feats)

-- Consume a final integer channel, scaling its samples in place. Call only
-- after all inverse transforms and integer-channel consumers have finished.
-- Int32 and Float occupy the same four bytes; the Chan's extra sentinel is
-- retained but is outside the image. No integer view may be read afterwards.
intsToFloats :: Chan s -> Float -> ST s (FPlane s)
intsToFloats c sc = do
  let n = chW c * chH c
  out <- castSTUArray (chBuf c)
  forLoopJ 0 (n - 1) $ \i -> do
    v <- rdC (chBuf c) i
    unsafeWrite out i (fromIntegral v * sc)
  pure out

-- Extra channels to float planes at the colour channels' resolution
-- (w x h, row stride @stride@), upsampling any decoded smaller.
extraToFloats :: ImgHdr -> FrameHdr -> Int -> Int -> Int -> [CompactChan s] -> ST s [ExtraPlane s]
extraToFloats ih fh w h stride chans =
  forM (zip3 chans (ihEC ih) (ecShifts ih fh)) $ \(cc, e, sh) -> do
    let sc = 1 / fromIntegral ((1 `shiftL` ecBpp e) - 1 :: Int) :: Float
    case cc of
      ConstantChan ci v | sh == 0 && ciW ci > 0 && ciH ci > 0 ->
        pure (ConstantExtra (fromIntegral v * sc))
      _ -> do
        c <- materializeChan cc
        small <- intsToFloats c sc
        (big, bw, bh) <- upsampleBy (upWeights ih) sh small (chW c) (chH c) (chW c)
        if bw == w && bh == h && stride == w
          then pure (DenseExtra big)
          else do
            out <- newArray (0, max 0 (stride * h - 1)) 0
            forLoopJ 0 (h - 1) $ \y -> forLoopJ 0 (w - 1) $ \x ->
              unsafeRead big (min (chH c `shiftL` sh - 1) y * bw + min (bw - 1) x) >>= unsafeWrite out (y * stride + x)
            pure (DenseExtra out)

-- Extra channels' shifts relative to the colour channels' samples.
ecShifts :: ImgHdr -> FrameHdr -> [Int]
ecShifts ih fh = [ max 0 (countTrailingZeros u + ecShift e - countTrailingZeros (fhUpsampling fh))
                 | (e, u) <- zip (ihEC ih) (fhEcUp fh) ]

ecInfos :: ImgHdr -> FrameHdr -> Int -> Int -> [ChInfo]
ecInfos ih fh w h = [ ChInfo (up w s) (up h s) s s | s <- ecShifts ih fh ]
  where up x s = (x + (1 `shiftL` s) - 1) `shiftR` s

-- Gaborish then the edge-preserving filter, as the frame asks.
restoration :: FrameHdr -> [FPlane s] -> Int -> Int -> Int -> FPlane s -> Int -> ST s ()
restoration fh planes pw w h sigma sw = do
  let iters = fhEpfIters fh
      (_, p0, p2, bm) = fhEpfSigma fh
      scale = map realToFrac (fhEpfScale fh)
      params = (realToFrac p0, realToFrac p2, realToFrac bm)
      serial = do
        maybe (pure ()) (gaborish planes pw w h) (fhGab fh)
        when (iters > 0) (epf planes pw w h sigma sw iters scale params)
  -- The serial path is unchanged at -N1. Tiling is restricted to large
  -- noisy frames: the baseline's group-height noise scratch leaves room
  -- for the bounded source band and at most four private filter tiles.
  if w >= 2048 && h >= 2048 && testBit (fhFlags fh) 0 && iters > 0 then do
    n <- unsafeIOToST parWorkers
    if n <= 1 then serial else
      filterBands (min 4 n) planes pw w h sigma sw (fhGab fh) iters scale params
  else serial

------------------------------------------------------------------------------
-- VarDCT frames

data HfPass = HfPass (Array Int (UArray Int Int)) Code

decodeVarDctFrame :: BR s -> ImgHdr -> FrameHdr -> Toc -> Maybe (Img s) -> J s (Img s, Features)
decodeVarDctFrame br ih fh toc lfImg = do
  let (w, h) = frameDims fh
      gdim = 256
      gcols = ceilDiv w gdim
      ngroups = gcols * ceilDiv h gdim
      lfdim = 8 * gdim
      lfcols = ceilDiv w lfdim
      nlf = lfcols * ceilDiv h lfdim
      -- chroma subsampling (JPEG recompression): per-channel shifts, as
      -- libjxl's ChromaSubsampling derives them from the channel modes
      ju = fhJpegUp fh
      hsc = any (\v -> v == 1 || v == 2) ju
      vsc = any (\v -> v == 1 || v == 3) ju
      b2i b = if b then 1 else 0 :: Int
      shifts = [ case m of { 0 -> (b2i hsc, b2i vsc); 1 -> (0, 0); 2 -> (0, b2i vsc); _ -> (b2i hsc, 0) }
               | m <- ju ]
      subsampled = any (/= (0, 0)) shifts
      roundEven b n = if b then 2 * ceilDiv n 2 else n
      bw = roundEven hsc (ceilDiv w 8)
      bh = roundEven vsc (ceilDiv h 8)
      cbw = [ bw `shiftR` hs | (hs, _) <- shifts ]
      cbh = [ bh `shiftR` vs | (_, vs) <- shifts ]
      cpw = map (* 8) cbw
      cw64 = ceilDiv w 64
      ch64 = ceilDiv h 64
      bd = ihBpp ih
      npass = fhPasses fh
  ---------------------------------------------------------------- LfGlobal
  (r0, end0) <- sectionReader br toc 0
  feats <- readFeatures r0 ih fh
  mlf <- lfDequant r0
  gscale <- liftJ (u32 r0 (1,11) (2049,11) (4097,12) (8193,16))
  qlf <- liftJ (u32 r0 (16,0) (1,5) (1,8) (1,16))
  (bmap, nbc, lfThr, qfThr) <- readHfBlockCtx r0
  (cfac, bcx, bcb, xflf, bflf) <- do
    dflt <- liftJ (readBool r0)
    if dflt then pure (84, 0, 1, 128, 128) else do
      cf <- liftJ (u32 r0 (84,0) (256,0) (2,8) (258,16))
      bx <- f16J r0
      bb <- f16J r0
      xf <- liftJ (readBits r0 8)
      bf <- liftJ (readBits r0 8)
      pure (cf, bx, bb, xf, bf)
  gm <- readGlobalModular r0 w h gdim (ecInfos ih fh w h)
  unless (tocSingle toc) (checkEnd r0 end0)
  let feats' = feats { ftCorr = (realToFrac bcx, realToFrac bcb) }
      lfMul = product [ length t + 1 | t <- lfThr ]
      hc = HfCtx bmap nbc (listArray (0, length qfThr - 1) qfThr) lfMul
      (qmul, _, _, _) = fhEpfSigma fh
      sharpLut = map realToFrac (fhEpfSharp fh) :: [Float]
      epfOn = fhEpfIters fh > 0
      fgs = fromIntegral gscale :: Float
  jguard (not subsampled || testBit (fhFlags fh) 7 || isJust lfImg)
    "JPEG XL: LF smoothing of subsampled chroma is not supported yet"
  ---------------------------------------------------------------- buffers
  planes <- liftJ $ forM (zip cpw cbh) $ \(p, bhc) -> newArray (0, p * bhc * 8 - 1) 0 :: ST s (FPlane s)
  planesI <- liftJ (mapM castSTUArray planes)
  lf <- liftJ $ forM (zip cbw cbh) $ \(a, b) -> newArray (0, a * b - 1) 0 :: ST s (FPlane s)
  lfIdx <- liftJ (newArray (0, bw * bh - 1) 0 :: ST s (STUArray s Int Int32))
  blk <- liftJ (newArray (0, bw * bh - 1) (-1) :: ST s (STUArray s Int Int32))
  sigma <- liftJ (newArray (0, bw * bh - 1) 0 :: ST s (FPlane s))
  xfy <- liftJ (newArray (0, cw64 * ch64 - 1) 0 :: ST s (STUArray s Int Int32))
  bfy <- liftJ (newArray (0, cw64 * ch64 - 1) 0 :: ST s (STUArray s Int Int32))
  ---------------------------------------------------------------- LF groups
  forM_ [0 .. nlf - 1] $ \g -> do
    (r, end) <- sectionReader br toc (1 + g)
    let lfx0 = (g `mod` lfcols) * lfdim
        lfy0 = (g `div` lfcols) * lfdim
        lfw = min lfdim (w - lfx0)
        lfh = min lfdim (h - lfy0)
        bwl0 = ceilDiv lfw 8
        bhl0 = ceilDiv lfh 8
        bwl = roundEven hsc bwl0
        bhl = roundEven vsc bhl0
        bx0 = lfx0 `div` 8
        by0 = lfy0 `div` 8
        -- a channel's size in blocks within this LF group
        chDim n has sh = if has then (if sh == 1 then ceilDiv n 2 else 2 * ceilDiv n 2) else n
        lfDims = [ (chDim bwl0 hsc hs, chDim bhl0 vsc vs) | (hs, vs) <- shifts ]
    -- LF coefficients (modular channels in Y, X, B order), unless an LF
    -- frame supplies the LF image
    when (isNothing lfImg) $ do
     ep <- liftJ (readBits r 2)
     let [dX, dY, dB] = lfDims
     lfq <- modularStream r (gmTree gm) bd (1 + g)
              [ ChInfo (fst d) (snd d) 0 0 | d <- [dY, dX, dB] ]
     let [qy, qx, qb] = lfq
         qXyb = [qx, qy, qb]
     liftJ $ forM_ (zip3 [0 :: Int ..] qXyb mlf) $ \(c, q, m) -> do
      let sc = m * fromIntegral ((1 :: Int) `shiftL` (9 - ep)) / (fgs * fromIntegral qlf)
          dst = lf !! c
          (hs, vs) = shifts !! c
          stride = cbw !! c
          qw = chW q
          ox = bx0 `shiftR` hs
          oy = by0 `shiftR` vs
          nx = min qw (stride - ox)
          ny = min (chH q) ((cbh !! c) - oy)
      forLoopJ 0 (ny - 1) $ \y -> forLoopJ 0 (nx - 1) $ \x -> do
        v <- rdC (chBuf q) (y * qw + x)
        unsafeWrite dst ((oy + y) * stride + ox + x) (fromIntegral v * sc)
     when (lfMul > 1) $ liftJ $
      forLoopJ 0 (bhl - 1) $ \y -> forLoopJ 0 (bwl - 1) $ \x -> do
        let at c q = let (hs, vs) = shifts !! c
                     in rdC (chBuf q) (min (chH q - 1) (y `shiftR` vs) * chW q + min (chW q - 1) (x `shiftR` hs))
        vX <- at 0 qx
        vB <- at 2 qb
        vY <- at 1 qy
        let idx = foldl (\acc (v, thr) -> acc * (length thr + 1) + length (filter (v >) thr)) 0
                    (zip [vX, vB, vY] [lfThr !! 0, lfThr !! 2, lfThr !! 1])
        when (by0 + y < bh && bx0 + x < bw) $
          unsafeWrite lfIdx ((by0 + y) * bw + bx0 + x) (fromIntegral idx)
     pure ()
    -- modular LF group
    gmLfGroup r gm bd gdim lfcols nlf g
    -- HF metadata
    nbBlocks <- liftJ ((+ 1) <$> readBits r (ceilLog2 (bwl * bhl - 1)))
    hfm <- modularStream r (gmTree gm) bd (1 + 2 * nlf + g)
      [ ChInfo (ceilDiv lfw 64) (ceilDiv lfh 64) 0 0, ChInfo (ceilDiv lfw 64) (ceilDiv lfh 64) 0 0
      , ChInfo nbBlocks 2 0 0, ChInfo bwl bhl 0 0 ]
    let (cxf, cbf, cinfo, csharp) = case hfm of
          [a, b, c, d] -> (a, b, c, d)
          _ -> error "unreachable: modularStream checks the channel count"
    liftJ $ forLoopJ 0 (ceilDiv lfh 64 - 1) $ \y -> forLoopJ 0 (ceilDiv lfw 64 - 1) $ \x -> do
      let si = y * ceilDiv lfw 64 + x
          di = (lfy0 `div` 64 + y) * cw64 + lfx0 `div` 64 + x
      unsafeRead (chBuf cxf) si >>= unsafeWrite xfy di
      unsafeRead (chBuf cbf) si >>= unsafeWrite bfy di
    placed <- liftJ $ placeVarblocks blk sigma bw bx0 by0 bwl bhl nbBlocks cinfo csharp
                (if epfOn then Just (realToFrac qmul * 65536 / fgs, sharpLut) else Nothing)
    either jfail pure placed
    unless (tocSingle toc) (checkEnd r end)
  -- LF chroma from luma, then adaptive smoothing; an LF frame's image is
  -- final already
  case lfImg of
    Just li -> liftJ $ forM_ (zip3 lf (imColour li) (zip cbw cbh)) $ \(dst, src, (a, b)) ->
      forLoopJ 0 (b - 1) $ \y -> forLoopJ 0 (a - 1) $ \x ->
        unsafeRead src (min (imH li - 1) y * imStride li + min (imW li - 1) x)
          >>= unsafeWrite dst (y * a + x)
    Nothing -> pure ()
  let kxLf = realToFrac bcx + fromIntegral (xflf - 128) / fromIntegral cfac :: Float
      kbLf = realToFrac bcb + fromIntegral (bflf - 128) / fromIntegral cfac :: Float
      [lx, ly, lb] = lf
  when (isNothing lfImg && not subsampled) $ liftJ $ forLoopJ 0 (bw * bh - 1) $ \i -> do
    yv <- unsafeRead ly i
    xv <- unsafeRead lx i
    bv <- unsafeRead lb i
    unsafeWrite lx i (xv + kxLf * yv)
    unsafeWrite lb i (bv + kbLf * yv)
  unless (testBit (fhFlags fh) 7 || isJust lfImg) $ liftJ $
    adaptiveLfSmoothing lf bw bh [ 512 * m / (fgs * fromIntegral qlf) | m <- mlf ]
  ---------------------------------------------------------------- HfGlobal
  (rh, endh) <- sectionReader br toc (1 + nlf)
  dqDefault <- liftJ (readBool rh)
  dqps <- if dqDefault then pure [ defaultDqParams p | p <- [0 .. 16] ] else
    forM [0 .. 16] $ \p -> readDqParams rh p $ \mw mh -> do
      chans <- modularStream rh (gmTree gm) bd (1 + 3 * nlf + p) (replicate 3 (ChInfo mw mh 0 0))
      liftJ $ forM chans $ \c -> forM [0 .. mw * mh - 1] (rdC (chBuf c))
  nhfp <- liftJ ((+ 1) <$> readBits rh (ceilLog2 (ngroups - 1)))
  passes <- replicateM npass $ do
    used <- liftJ (u32 rh (0x5f,0) (0x13,0) (0,0) (0,13))
    perms <- if used == 0 then pure [] else do
      code <- readCode rh 8
      st <- liftJ (newCodeSt rh code 65536)
      ps <- forM [ o | o <- [0 .. 12], testBit used o ] $ \o -> do
        let size = (orderBW U.! o) * (orderBH U.! o)
            nat = naturalOrder o
        forM [0 .. 2] $ \c -> do
          perm <- readPermutation rh code st size (size `div` 64)
          pure (o * 3 + c, listArray (0, size - 1) [ nat U.! (perm U.! k) | k <- [0 .. size - 1] ] :: UArray Int Int)
      finishCode code st
      pure (concat ps)
    hcode <- readCode rh (495 * nhfp * nbc)
    let orders = A.listArray (0, 38) [ maybe (naturalOrder (k `div` 3)) id (lookup k perms) | k <- [0 .. 38] ]
    pure (HfPass orders hcode)
  unless (tocSingle toc) (checkEnd rh endh)
  ---------------------------------------------------------------- pass groups
  forM_ (zip3 [0 ..] passes (fhPassShiftK fh)) $ \(p, HfPass orders hcode, shift) ->
    forM_ [0 .. ngroups - 1] $ \g -> do
      (r, end) <- sectionReader br toc (1 + nlf + 1 + p * ngroups + g)
      let gx = g `mod` gcols
          gy = g `div` gcols
          gbx = gx * 32
          gby = gy * 32
          nbx = min 32 (bw - gbx)
          nby = min 32 (bh - gby)
      hfp <- liftJ (readBits r (ceilLog2 (nhfp - 1)))
      jguard (hfp < nhfp) "JPEG XL: HF preset out of range"
      cs <- liftJ (newCodeSt r hcode (nbx * nby * 64 * 3))
      decodeHfGroup r hcode cs (495 * nbc * hfp) hc orders blk lfIdx bw planesI cpw shifts
        gbx gby nbx nby shift
      finishCode hcode cs
      let pshifts = maybe (0, 0) id (lookup p (fhPassShifts fh))
      gmPassGroup r gm bd gdim gcols pshifts (1 + 3 * nlf + 17 + p * ngroups + g) g
      unless (tocSingle toc) (checkEnd r end)
  when (tocSingle toc) (checkEnd br (tocEnd toc))
  ---------------------------------------------------------------- reconstruct
  let mats = A.listArray (0, 16) [ buildMatrix p ps | (p, ps) <- zip [0 ..] dqps ]
               :: Array Int (Either String [UArray Int Float])
      qmScale = [0.8 ** fromIntegral (fhXqm fh - 2), 1, 0.8 ** fromIntegral (fhBqm fh - 2)] :: [Float]
      qbias = map realToFrac (take 3 (drop 12 (ihOpsin ih))) :: [Float]
      qbn = realToFrac (ihOpsin ih !! 15) :: Float
  -- every matrix a block uses must be valid
  used <- liftJ $ do
    flags <- newArray (0, 16) False :: ST s (STUArray s Int Bool)
    forLoopJ 0 (bw * bh - 1) $ \i -> do
      e <- unsafeRead blk i
      when (e >= 0) $ unsafeWrite flags (dctParam U.! (fromIntegral e .&. 0xff)) True
    forM [0 .. 16] (unsafeRead flags)
  forM_ (zip [0 ..] used) $ \(p, u) -> when u $ either jfail (const (pure ())) (mats A.! p)
  let matOf p = either (const []) id (mats A.! p)
      -- the varblocks a channel holds: with a shift, those starting at a
      -- multiple of it where a varblock also starts at the shifted spot
      channelBlocks c k = forLoopJ 0 (bh - 1) $ \by -> forLoopJ 0 (bw - 1) $ \bx -> do
        e <- fromIntegral <$> unsafeRead blk (by * bw + bx)
        when (e >= 0) $ do
          let (hs, vs) = shifts !! c
              sbx = bx `shiftR` hs
              sby = by `shiftR` vs
          ok <- if hs == 0 && vs == 0 then pure True
                else if (sbx `shiftL` hs) /= bx || (sby `shiftL` vs) /= by then pure False
                else (>= 0) <$> unsafeRead blk (sby * bw + sbx)
          when ok (k e sbx sby)
  sc <- liftJ newScratch
  liftJ $ do
    -- dequantise
    forM_ (zip3 [0 ..] (zip planesI planes) (zip qmScale qbias)) $ \(c, (ip, fp), (qs, qb)) ->
      channelBlocks c $ \e sbx sby -> do
        let t = e .&. 0xff
            hfm = e `shiftR` 8
            bwp = 8 * dctW8 U.! t
            bhp = 8 * dctH8 U.! t
            p = dctParam U.! t
            mw = matrixW U.! p
            tr = dctTranspose t
            pw = cpw !! c
            origin = sby * 8 * pw + sbx * 8
            mat = matOf p !! c
            mul = 65536 / (fgs * fromIntegral hfm) * qs
        forLoopJ 0 (bhp - 1) $ \y -> forLoopJ 0 (bwp - 1) $ \x -> do
          let pos = origin + y * pw + x
              m = if tr then mat `unsafeAt` (x * mw + y) else mat `unsafeAt` (y * mw + x)
          q <- unsafeRead ip pos
          when (q /= 0) $ do
            let qf = fromIntegral q :: Float
                v = if abs qf <= 1 then qf * qb else qf - qbn / qf
            unsafeWrite fp pos (v * m * mul)
    -- chroma from luma (not with subsampled chroma)
    unless subsampled $ do
      let [px, py, pb] = planes
          pw = head cpw
          ph = 8 * bh
      forLoopJ 0 (ph - 1) $ \y -> forLoopJ 0 (pw - 1) $ \x -> do
        let ci = min (ch64 - 1) (y `shiftR` 6) * cw64 + min (cw64 - 1) (x `shiftR` 6)
            i = y * pw + x
        xf <- unsafeRead xfy ci
        bf <- unsafeRead bfy ci
        yv <- unsafeRead py i
        when (yv /= 0) $ do
          let kx = realToFrac bcx + fromIntegral xf / fromIntegral cfac :: Float
              kb = realToFrac bcb + fromIntegral bf / fromIntegral cfac :: Float
          xv <- unsafeRead px i
          bv <- unsafeRead pb i
          unsafeWrite px i (xv + kx * yv)
          unsafeWrite pb i (bv + kb * yv)
    -- LLF from the LF image, then the inverse transforms
    forM_ (zip3 [0 ..] planes lf) $ \(c, fp, lfp) ->
      channelBlocks c $ \e sbx sby -> do
        let t = e .&. 0xff
            bw8 = dctW8 U.! t
            bh8 = dctH8 U.! t
            pw = cpw !! c
            lstride = cbw !! c
            origin = sby * 8 * pw + sbx * 8
        if bw8 == 1 && bh8 == 1
          then unsafeRead lfp (sby * lstride + sbx) >>= unsafeWrite fp origin
          else llfFromLf sc lfp (sby * lstride + sbx) lstride fp origin pw bw8 bh8
        idctBlock sc t fp origin pw
  -- subsampled chroma is upsampled (as JPEG decoders do) before filtering
  (outPlanes, stride) <- liftJ $ if not subsampled then pure (planes, head cpw) else do
    ups <- forM (zip3 planes cpw shifts) $ \(p, pw, (hs, vs)) -> jpegUpsample p pw hs vs w h
    pure (ups, w)
  liftJ (restoration fh outPlanes stride w h sigma bw)
  extraCh <- gmFinish gm bd
  extra <- liftJ (extraToFloats ih fh w h stride extraCh)
  pure (Img w h stride outPlanes extra (if fhYCbCr fh then domYCbCr else if ihXyb ih then domXyb else domEncoded), feats')

-- | JPEG-style chroma upsampling (the "fancy" triangle filter libjxl uses
-- for recompressed JPEGs) of a plane shifted by (hs, vs) to w x h.
jpegUpsample :: FPlane s -> Int -> Int -> Int -> Int -> Int -> ST s (FPlane s)
jpegUpsample p pw hs vs w h = do
  let sw = (w + (1 `shiftL` hs) - 1) `shiftR` hs
      sh = (h + (1 `shiftL` vs) - 1) `shiftR` vs
  out <- newArray (0, w * h - 1) 0 :: ST s (FPlane s)
  -- horizontal pass into rows 0 .. sh-1 of a w-wide buffer
  tmp <- newArray (0, w * sh - 1) 0 :: ST s (FPlane s)
  forLoopJ 0 (sh - 1) $ \y ->
    if hs == 0
      then forLoopJ 0 (w - 1) $ \x -> unsafeRead p (y * pw + x) >>= unsafeWrite tmp (y * w + x)
      else forLoopJ 0 (sw - 1) $ \x -> do
        c <- unsafeRead p (y * pw + x)
        l <- unsafeRead p (y * pw + max 0 (x - 1))
        r <- unsafeRead p (y * pw + min (sw - 1) (x + 1))
        when (2 * x < w) $ unsafeWrite tmp (y * w + 2 * x) (0.25 * l + 0.75 * c)
        when (2 * x + 1 < w) $ unsafeWrite tmp (y * w + 2 * x + 1) (0.75 * c + 0.25 * r)
  forLoopJ 0 (sh - 1) $ \y ->
    if vs == 0
      then forLoopJ 0 (w - 1) $ \x -> unsafeRead tmp (y * w + x) >>= unsafeWrite out (y * w + x)
      else forLoopJ 0 (w - 1) $ \x -> do
        c <- unsafeRead tmp (y * w + x)
        u <- unsafeRead tmp (max 0 (y - 1) * w + x)
        d <- unsafeRead tmp (min (sh - 1) (y + 1) * w + x)
        when (2 * y < h) $ unsafeWrite out ((2 * y) * w + x) (0.75 * c + 0.25 * u)
        when (2 * y + 1 < h) $ unsafeWrite out ((2 * y + 1) * w + x) (0.75 * c + 0.25 * d)
  pure out

-- HfBlockContext: (block context map, number of contexts, LF thresholds
-- per XYB channel, HfMul thresholds).
readHfBlockCtx :: BR s -> J s (UArray Int Int, Int, [[Int]], [Int])
readHfBlockCtx r = do
  dflt <- liftJ (readBool r)
  if dflt
    then pure ( listArray (0, 38) [ 0, 1, 2, 2, 3, 3, 4, 5, 6, 6, 6, 6, 6, 7, 8, 9, 9, 10, 11, 12, 13
                                  , 14, 14, 14, 14, 14, 7, 8, 9, 9, 10, 11, 12, 13, 14, 14, 14, 14, 14 ]
              , 15, [[], [], []], [] )
    else do
      lfThr <- replicateM 3 $ do
        n <- liftJ (readBits r 4)
        replicateM n (liftJ (unpackSigned <$> u32 r (0,4) (16,8) (272,16) (65808,32)))
      nq <- liftJ (readBits r 4)
      qfThr <- replicateM nq (liftJ ((+ 1) <$> u32 r (0,2) (4,3) (12,5) (44,8)))
      let bsize = product (map ((+ 1) . length) lfThr) * (nq + 1)
      jguard (bsize <= 64) "JPEG XL: HF block context too large"
      (ncl, cmap) <- readClusters r (bsize * 39)
      jguard (ncl <= 16) "JPEG XL: too many HF block contexts"
      pure (cmap, ncl, lfThr, qfThr)

-- Place an LF group's varblocks on the block grid (§I.4? in raster order,
-- each into the first free block), recording type and HfMul, and the EPF
-- sigma of every 8x8 block they cover.
placeVarblocks :: STUArray s Int Int32 -> FPlane s -> Int -> Int -> Int -> Int -> Int -> Int
               -> Chan s -> Chan s -> Maybe (Float, [Float]) -> ST s (Either String ())
placeVarblocks blk sigma bw bx0 by0 bwl bhl nb cinfo csharp epfp = go 0 0 0
  where
    sharpArr = listArray (0, 7) (maybe (replicate 8 0) snd epfp) :: UArray Int Float
    go !y !x !di
      | y >= bhl = pure (Right ())
      | x >= bwl = go (y + 1) 0 di
      | otherwise = do
          e <- unsafeRead blk ((by0 + y) * bw + bx0 + x)
          if e /= -1 then go y (x + 1) di else
            if di >= nb then pure (Left "JPEG XL: varblocks do not fill the LF group") else do
              t <- rdC (chBuf cinfo) di
              m <- rdC (chBuf cinfo) (nb + di)
              let hfm = m + 1
              if t < 0 || t > 26 || hfm <= 0 || hfm >= (1 `shiftL` 23)
                then pure (Left "JPEG XL: bad varblock") else do
                let dw = dctW8 U.! t
                    dh = dctH8 U.! t
                if (x `mod` 32) + dw > 32 || (y `mod` 32) + dh > 32 || x + dw > bwl || y + dh > bhl
                  then pure (Left "JPEG XL: varblock crosses a group") else do
                  clash <- or <$> sequence [ (/= -1) <$> unsafeRead blk ((by0 + y + dy) * bw + bx0 + x + dx)
                                           | dy <- [0 .. dh - 1], dx <- [0 .. dw - 1] ]
                  if clash then pure (Left "JPEG XL: varblocks overlap") else do
                    okSharp <- fmap and $ forM [ (dx, dy) | dy <- [0 .. dh - 1], dx <- [0 .. dw - 1] ] $ \(dx, dy) -> do
                      let bi = (by0 + y + dy) * bw + bx0 + x + dx
                      unsafeWrite blk bi (if dx == 0 && dy == 0 then fromIntegral (t .|. (hfm `shiftL` 8)) else -2)
                      case epfp of
                        Nothing -> pure True
                        Just (base, _) -> do
                          sh <- rdC (chBuf csharp) ((y + dy) * bwl + x + dx)
                          if sh < 0 || sh > 7 then pure False else do
                            unsafeWrite sigma bi (base / fromIntegral hfm * (sharpArr `unsafeAt` sh))
                            pure True
                    if okSharp then go y (x + dw) (di + 1) else pure (Left "JPEG XL: bad EPF sharpness")

------------------------------------------------------------------------------
-- Output

-- | A frame or canvas to sRGB RGBA: XYB through the opsin inverse, or the
-- image's own colour space as is, both with libjxl's ordered dither on the
-- way to 8 bits; alpha from the first alpha extra channel.
renderImg :: ImgHdr -> Img s -> ST s (Int, Int, UArray Int Word8)
renderImg ih img0
  | any (\e -> ecType e == 2) (ihEC ih) && length (imColour img0) == 3 = do
      -- spot colours are mixed into the colour channels in the image's own
      -- colour space, as libjxl renders them by default
      img <- toEncoded ih img0
      let st = imStride img
          n = imW img * imH img
      forM_ [ (p, e) | (p, e) <- zip (imExtra img) (ihEC ih), ecType e == 2, length (ecSpot e) == 4 ] $ \(sp, e) -> do
        let [cr, cg, cbl, solid] = ecSpot e
        forM_ (zip (imColour img) [cr, cg, cbl]) $ \(p, col) ->
          forLoopJ 0 (imH img - 1) $ \y -> forLoopJ 0 (imW img - 1) $ \x -> do
            let i = y * st + x
            sv <- readExtra sp i
            v <- unsafeRead p i
            let mix = sv * solid
            unsafeWrite p i (mix * col + (1 - mix) * v)
      n `seq` renderImg' ih img
  | otherwise = renderImg' ih img0

renderImg' :: ImgHdr -> Img s -> ST s (Int, Int, UArray Int Word8)
renderImg' ih img = do
  let !tab = srgbTab
      w = imW img
      h = imH img
      st = imStride img
      op = U.listArray (0, 11) (map realToFrac (take 12 (ihOpsin ih))) :: UArray Int Float
      !m00 = op `unsafeAt` 0
      !m01 = op `unsafeAt` 1
      !m02 = op `unsafeAt` 2
      !m10 = op `unsafeAt` 3
      !m11 = op `unsafeAt` 4
      !m12 = op `unsafeAt` 5
      !m20 = op `unsafeAt` 6
      !m21 = op `unsafeAt` 7
      !m22 = op `unsafeAt` 8
      !b0 = op `unsafeAt` 9
      !b1 = op `unsafeAt` 10
      !b2 = op `unsafeAt` 11
      !cb0 = cbrtF b0
      !cb1 = cbrtF b1
      !cb2 = cbrtF b2
      !its = 255 / realToFrac (ihIntensity ih) :: Float
      !grey = ihGrey ih
      alpha = [ p | (p, e) <- zip (imExtra img) (ihEC ih), ecType e == 0 ]
      -- libjxl's 8-bit output: sRGB scaled to 255 plus an ordered 8x8
      -- (Bayer) dither in (-0.5, 0.5), then rounded
      {-# INLINE enc #-}
      enc d v = fromIntegral (truncate (srgb255 tab v + d + 0.5) :: Int) :: Word8
      direct d v = let u = v * 255 + d
                   in fromIntegral (truncate (max 0 (min 255 u) + 0.5) :: Int) :: Word8
  -- renderImg is terminal: frames returns this first displayed frame and
  -- never reads its canvas/reference planes again. Each output pixel occupies
  -- exactly one Float. With st >= w, a forward raster scan writes only over
  -- samples already read, even when the source stride contains padding.
  out <- case imColour img of
    (p : _) -> do
      STUArray _ _ _ bytes <- castSTUArray p
      pure (STUArray 0 (w * h * 4 - 1) (w * h * 4) bytes)
    [] -> newArray (0, w * h * 4 - 1) 255
  case imColour img of
    [px, py, pb] | imDomain img == domXyb ->
      -- Row bands run in parallel when every output pixel's four bytes
      -- overwrite exactly its own source Float (st == w): then a band
      -- reads and writes only its own rows. A padded stride keeps the
      -- sequential forward scan, whose safety argument is the one above.
      (if st == w then parRows h else \f -> f 0 h) $ \ya yb -> do
      lin <- newArray (0, 3 * 32 - 1) 0 :: ST s (FPlane s)
      forLoopJ ya (yb - 1) $ \y -> forLoopStep' 0 (w - 1) 32 $ \x0 -> do
        let !n = min 32 (w - x0)
            !rowI = y * st + x0
            !rowO = (y * w + x0) * 4
            !dRow = (y .&. 7) `shiftL` 3
        -- phase A: XYB -> linear RGB for the chunk (few live values)
        forLoopJ 0 (n - 1) $ \k -> do
          let i = rowI + k
          xv <- unsafeRead px i
          yv <- unsafeRead py i
          bv <- unsafeRead pb i
          let gl = yv + xv - cb0
              gm = yv - xv - cb1
              gs = bv - cb2
              l = (gl * gl * gl + b0) * its
              m = (gm * gm * gm + b1) * its
              s' = (gs * gs * gs + b2) * its
          unsafeWrite lin (3 * k) (m00 * l + m01 * m + m02 * s')
          unsafeWrite lin (3 * k + 1) (m10 * l + m11 * m + m12 * s')
          unsafeWrite lin (3 * k + 2) (m20 * l + m21 * m + m22 * s')
        -- phase B: transfer curve, dither, bytes
        if grey
          then forLoopJ 0 (n - 1) $ \k -> do
            let d = bayer `unsafeAt` (dRow .|. ((x0 + k) .&. 3))
                o = rowO + 4 * k
            g <- unsafeRead lin (3 * k + 1)
            let v = enc d g
            unsafeWrite out o v; unsafeWrite out (o + 1) v; unsafeWrite out (o + 2) v
          else forLoopJ 0 (n - 1) $ \k -> do
            let d = bayer `unsafeAt` (dRow .|. ((x0 + k) .&. 3))
                o = rowO + 4 * k
            r <- unsafeRead lin (3 * k)
            g <- unsafeRead lin (3 * k + 1)
            b <- unsafeRead lin (3 * k + 2)
            unsafeWrite out o (enc d r); unsafeWrite out (o + 1) (enc d g); unsafeWrite out (o + 2) (enc d b)
    [pcb, pyy, pcr] | imDomain img == domYCbCr ->
      forLoopJ 0 (h - 1) $ \y -> forLoopJ 0 (w - 1) $ \x -> do
        let i = y * st + x
            d = bayer `unsafeAt` (((y .&. 7) `shiftL` 3) .|. (x .&. 3))
            o = (y * w + x) * 4
        cb <- unsafeRead pcb i
        yv <- (+ (128 / 255)) <$> unsafeRead pyy i
        cr <- unsafeRead pcr i
        let r = yv + 1.402 * cr
            g = yv - (0.114 * 1.772 / 0.587) * cb - (0.299 * 1.402 / 0.587) * cr
            b = yv + 1.772 * cb
        if ihGrey ih
          then do let v = direct d yv
                  unsafeWrite out o v; unsafeWrite out (o + 1) v; unsafeWrite out (o + 2) v
          else do unsafeWrite out o (direct d r); unsafeWrite out (o + 1) (direct d g)
                  unsafeWrite out (o + 2) (direct d b)
    [pr, pg, pb] ->
      forLoopJ 0 (h - 1) $ \y -> forLoopJ 0 (w - 1) $ \x -> do
        let i = y * st + x
            d = bayer `unsafeAt` (((y .&. 7) `shiftL` 3) .|. (x .&. 3))
            o = (y * w + x) * 4
        unsafeRead pr i >>= unsafeWrite out o . direct d
        unsafeRead pg i >>= unsafeWrite out (o + 1) . direct d
        unsafeRead pb i >>= unsafeWrite out (o + 2) . direct d
    (pg : _) ->
      forLoopJ 0 (h - 1) $ \y -> forLoopJ 0 (w - 1) $ \x -> do
        let d = bayer `unsafeAt` (((y .&. 7) `shiftL` 3) .|. (x .&. 3))
            o = (y * w + x) * 4
        v <- direct d <$> unsafeRead pg (y * st + x)
        unsafeWrite out o v; unsafeWrite out (o + 1) v; unsafeWrite out (o + 2) v
    [] -> pure ()
  case alpha of
    (ConstantExtra v : _) -> do
      let a = fromIntegral (truncate (max 0 (min 255 (v * 255)) + 0.5) :: Int)
      forLoopJ 0 (w * h - 1) $ \i -> unsafeWrite out (i * 4 + 3) a
    (DenseExtra a : _) -> forLoopJ 0 (h - 1) $ \y -> forLoopJ 0 (w - 1) $ \x -> do
      v <- unsafeRead a (y * st + x)
      unsafeWrite out ((y * w + x) * 4 + 3)
        (fromIntegral (truncate (max 0 (min 255 (v * 255)) + 0.5) :: Int))
    [] -> forLoopJ 0 (w * h - 1) $ \i -> unsafeWrite out (i * 4 + 3) 255
  px' <- unsafeFreeze out
  pure (orient (ihOrient ih) w h px')

------------------------------------------------------------------------------
-- Frame features, upsampling and blending

-- | Convert an image's colour planes to the image's own colour space
-- (sRGB transfer, unclamped: the transfer is extended sign-symmetrically).
toEncoded :: ImgHdr -> Img s -> ST s (Img s)
toEncoded ih img
  | imDomain img == domEncoded = pure img
  | otherwise = do
      let w = imW img
          h = imH img
          st = imStride img
          op = map realToFrac (ihOpsin ih) :: [Float]
          [m00, m01, m02, m10, m11, m12, m20, m21, m22] = take 9 op
          [b0, b1, b2] = take 3 (drop 9 op)
          cb0 = cbrtF b0
          cb1 = cbrtF b1
          cb2 = cbrtF b2
          its = 255 / realToFrac (ihIntensity ih) :: Float
          tf v = let a = abs v
                     e = if a <= 0.0031308 then 12.92 * a else 1.055 * a ** (1 / 2.4) - 0.055
                 in if v < 0 then negate e else e
      outs <- replicateM 3 (newArray (0, max 0 (w * h - 1)) 0 :: ST s (FPlane s))
      let [o0, o1, o2] = outs
          [p0, p1, p2] = take 3 (imColour img ++ repeat (head (imColour img)))
      forLoopJ 0 (h - 1) $ \y -> forLoopJ 0 (w - 1) $ \x -> do
        let i = y * st + x
            o = y * w + x
        a <- unsafeRead p0 i
        b <- unsafeRead p1 i
        c <- unsafeRead p2 i
        let (r, g, bl)
              | imDomain img == domXyb =
                  let gl = b + a - cb0
                      gm = b - a - cb1
                      gs = c - cb2
                      l = (gl * gl * gl + b0) * its
                      m = (gm * gm * gm + b1) * its
                      s = (gs * gs * gs + b2) * its
                  in ( tf (m00 * l + m01 * m + m02 * s), tf (m10 * l + m11 * m + m12 * s)
                     , tf (m20 * l + m21 * m + m22 * s) )
              | otherwise =
                  let yv = b + 128 / 255
                  in ( yv + 1.402 * c, yv - (0.114 * 1.772 / 0.587) * a - (0.299 * 1.402 / 0.587) * c
                     , yv + 1.772 * a )
        unsafeWrite o0 o r
        unsafeWrite o1 o g
        unsafeWrite o2 o bl
      -- extra channels keep their planes but adopt the new stride
      extras <- forM (imExtra img) $ \ep -> case ep of
        ConstantExtra _ -> pure ep
        DenseExtra p -> if st == w then pure ep else do
          q <- newArray (0, max 0 (w * h - 1)) 0
          forLoopJ 0 (h - 1) $ \y -> forLoopJ 0 (w - 1) $ \x -> unsafeRead p (y * st + x) >>= unsafeWrite q (y * w + x)
          pure (DenseExtra q)
      let colour = if ihGrey ih then [o1] else outs
      pure img { imStride = w, imColour = colour, imExtra = extras, imDomain = domEncoded }

-- | Paste patches from the reference slots (§K.3? PatchDictionary).
applyPatches :: ImgHdr -> Refs s -> [PatchRef] -> Img s -> J s (Img s)
applyPatches _ _ [] img = pure img
applyPatches ih refs ps img0 = do
  -- Patch targets are mutable; each constant target gets its own plane.
  extras <- liftJ (mapM (materializeExtra (imW img0) (imH img0) (imStride img0)) (imExtra img0))
  let img = img0 { imExtra = map DenseExtra extras }
  -- Materialise each referenced slot's constants once, not once per patch.
  -- Keep the dense blend loop: a sum-type read inside that loop allocates.
  sources <- liftJ $ forM (zip [0 ..] (rfSlots refs)) $ \(slot, mr) -> case mr of
    Just ref | any ((== slot) . prRef) ps -> do
      es <- mapM (materializeExtra (imW ref) (imH ref) (imStride ref)) (imExtra ref)
      pure (Just (ref, es))
    _ -> pure Nothing
  forM_ ps $ \pr -> do
    (ref, refExtra) <- maybe (jfail "JPEG XL: patch from an empty reference slot") pure (sources !! prRef pr)
    jguard (length (imColour ref) == length (imColour img)
            && length (imExtra ref) == length (imExtra img)) "JPEG XL: patch reference mismatch"
    let ncol = length (imColour img)
        bases = imColour img ++ extras
        news = imColour ref ++ refExtra
    forM_ (prTargets pr) $ \(tx, ty, bl) ->
      forM_ (zip3 [0 ..] bases news) $ \(idx, base, new) -> do
        let (mode, aidx, clampA) = if idx < ncol then head bl else bl !! (idx - ncol + 1)
            -- clip the target to the frame and the source to the reference
            x0 = max 0 tx
            y0 = max 0 ty
            x1 = minimum [imW img, tx + prW pr, tx + imW ref - prX0 pr]
            y1 = minimum [imH img, ty + prH pr, ty + imH ref - prY0 pr]
            sxOf x = prX0 pr + (x - tx)
            syOf y = prY0 pr + (y - ty)
            isAlphaCh = idx == ncol + aidx
            premul = case drop aidx (ihEC ih) of { (e : _) -> ecAssoc e; [] -> False }
            alphaB = drop aidx extras
            alphaN = drop aidx refExtra
        when (x1 > x0 && y1 > y0 && sxOf x0 >= 0 && syOf y0 >= 0) $
          liftJ $ blendRegion mode clampA isAlphaCh premul base (imStride img) new (imStride ref)
            (listToMaybe alphaB) (listToMaybe alphaN) x0 y0 (sxOf x0) (syOf y0) (x1 - x0) (y1 - y0)
  pure img

-- Blend one channel's rectangle of @new@ into @base@ with a patch blend
-- mode (0 none, 1 replace, 2 add, 3 mul, 4/5 blend above/below, 6/7 mul-add
-- above/below).
blendRegion :: Int -> Bool -> Bool -> Bool -> FPlane s -> Int -> FPlane s -> Int
            -> Maybe (FPlane s) -> Maybe (FPlane s) -> Int -> Int -> Int -> Int -> Int -> Int -> ST s ()
blendRegion mode clampA isAlphaCh premul base bst new nst mba mna bx by nx ny w h =
  forLoopJ 0 (h - 1) $ \dy -> forLoopJ 0 (w - 1) $ \dx -> do
    let bi = (by + dy) * bst + bx + dx
        ni = (ny + dy) * nst + nx + dx
        clampV v = if clampA then max 0 (min 1 v) else v
    b <- unsafeRead base bi
    n <- unsafeRead new ni
    ba <- maybe (pure 0) (\a -> unsafeRead a bi) mba
    na <- maybe (pure 0) (\a -> unsafeRead a ni) mna
    let swapped = mode == 5 || mode == 7
        (bS, nS, bA, nA) = if swapped then (n, b, na, ba) else (b, n, ba, na)
        res = case mode of
          1 -> Just n
          2 -> Just (b + n)
          3 -> Just (b * clampV n)
          _ | mode == 4 || mode == 5 ->
                if isAlphaCh then Just (bS + clampV nS * (1 - bS))
                else if premul then Just (nS + bS * (1 - clampV nA))
                else let nA' = clampV nA
                         mix = 1 - (1 - nA') * (1 - bA)
                     in Just (if mix > 0 then (nA' * nS + bA * bS * (1 - nA')) / mix else 0)
            | mode == 6 || mode == 7 ->
                if isAlphaCh then (if swapped then Just n else Nothing)
                else Just (bS + clampV nA * nS)
            | otherwise -> Nothing
    case res of
      Just v -> unsafeWrite base bi v
      Nothing -> pure ()

-- | libjxl's bound on the work splines may ask for (an estimate of the
-- area they paint), checked before rendering any.
splineArea :: [QSpline] -> Int -> (Float, Float) -> Int
splineArea qs qa (cx, cb) = sum (map one qs)
  where
    divQa d | qa >= 0 = (8 * d + 7 + qa) `div` (8 + qa)
            | otherwise = d + ((d * negate qa) + 7) `div` 8
    corrX = ceiling (abs cx) :: Int
    corrB = ceiling (abs cb) :: Int
    one q =
      let [cX, cY, cB] = [ sum (map (divQa . abs) c) | c <- qsXyb q ]
          logColor = ceilLog2 (maximum [cX + corrX * cY, cY, cB + corrB * cY])
          widthEst = sum [ let wt = 1 + divQa (abs s) in wt * wt * logColor | s <- qsSigma q ]
      in widthEst * qsManhattan q

-- | Paint the splines (§K.? splines) onto XYB colour planes: each one's
-- Catmull-Rom curve is resampled at unit arc length, and every sample adds
-- a Gaussian-profile stroke whose colour and width are continuous IDCTs
-- along the curve.
renderSplines :: FrameHdr -> [QSpline] -> Int -> (Float, Float) -> Img s -> ST s ()
renderSplines fh qs qa (corrX, corrB) img = forM_ qs $ \q -> do
  let inv = if qa >= 0 then 1 / (1 + fromIntegral qa / 8) else 1 - fromIntegral qa / 8 :: Float
      wts = [0.0042, 0.075, 0.07] :: [Float]
      [dx0, dy0, db0] = [ [ fromIntegral v * wc * inv | v <- c ] | (c, wc) <- zip (qsXyb q) wts ]
      dX = zipWith (\a y -> a + corrX * y) dx0 dy0
      dB = zipWith (\a y -> a + corrB * y) db0 dy0
      dSig = [ fromIntegral v * 0.3333 * inv | v <- qsSigma q ]
      pts = [ (fromIntegral a, fromIntegral b) | (a, b) <- qsPoints q ] :: [(Float, Float)]
      samples = splineSamples pts
      arcLen = fromIntegral (length samples) - 2 + snd (last samples)
      w = imW img
      h = imH img
      st = imStride img
      planes = imColour img
  forM_ (zip [0 :: Int ..] samples) $ \(i, ((px, py), len)) -> do
    let t = 31 * min 1 (fromIntegral i / arcLen)
        sigma = cidct dSig t
        invS = 1 / sigma
        vals = [ cidct dX t * len, cidct dy0 t * len, cidct dB t * len ]
        maxColor = max 0.01 (maximum vals)
        maxDist = sqrt (2 * (log 10 * 3 + maxColor)) * abs sigma
        xb = max 0 (floor (px - maxDist + 0.5))
        xe = min (min (fhW fh) w) (floor (px + maxDist + 1.5))
        yb = max 0 (floor (py - maxDist + 0.5))
        ye = min (min (fhH fh) h) (floor (py + maxDist + 1.5))
    forM_ (zip planes vals) $ \(p, v) ->
      forLoopJ yb (ye - 1) $ \y -> forLoopJ xb (xe - 1) $ \x -> do
        let ddx = fromIntegral x - px
            ddy = fromIntegral y - py
            dist = sqrt (ddx * ddx + ddy * ddy)
            fct = erfF ((0.5 * dist + 0.35355338) * invS) - erfF ((0.5 * dist - 0.35355338) * invS)
            o = y * st + x
        old <- unsafeRead p o
        unsafeWrite p o (old + 0.25 * v * sigma * fct * fct)

cidct :: [Float] -> Float -> Float
cidct (c0 : cs) t = c0 + sum [ 1.4142135 * c * cos (fromIntegral k * (pi / 32) * (t + 0.5))
                              | (k, c) <- zip [1 :: Int ..] cs ]
cidct [] _ = 0

erfF :: Float -> Float
erfF x =
  let ax = abs x
      d1 = ax * 7.77394369e-02 + 2.05260015e-04
      d2 = d1 * ax + 2.32120216e-01
      d3 = d2 * ax + 2.77820801e-01
      d4 = d3 * ax + 1
      d5 = d4 * d4
      i5 = 1 / d5
      r = 1 - i5 * i5
  in if x < 0 then negate r else r

-- Unit-arc-length samples of a spline: (point, arc length it stands for).
splineSamples :: [(Float, Float)] -> [((Float, Float), Float)]
splineSamples pts = go0 (upsampled pts)
  where
    sub (a, b) (c, d) = (a - c, b - d)
    add (a, b) (c, d) = (a + c, b + d)
    scale (a, b) k = (a * k, b * k)
    norm (a, b) = sqrt (a * a + b * b)
    go0 [] = []
    go0 (p0 : rest) = (p0, 1) : walk p0 rest
    -- step along the polyline, emitting a point every unit of arc length
    walk _ [] = []
    walk cur ups = seg cur 0 ups
      where
        seg prev arc [] = [(prev, arc)]
        seg prev arc (nxt : more) =
          let d = norm (sub nxt prev)
          in if arc + d >= 1
               then let c = add prev (scale (sub nxt prev) ((1 - arc) / d))
                    in (c, 1) : walk c (nxt : more)
               else seg nxt (arc + d) more
    upsampled [p] = [p]
    upsampled s =
      let mirror a c = (fst c + fst c - fst a, snd c + snd c - snd a)
          ext = [mirror (s !! 1) (head s)] ++ s ++ [mirror (s !! (length s - 2)) (last s)]
          quads = [ take 4 (drop i ext) | i <- [0 .. length ext - 4] ]
          seg4 [p0, p1, p2, p3] =
            let ps = [p0, p1, p2, p3]
                ts = scanl (\acc (a, b) -> acc + (let (dx, dy) = sub b a in (dx * dx + dy * dy) ** 0.25)) 0
                       (zip ps (tail ps))
                [t0, t1, t2, t3] = ts
                tt = [t0, t1, t2, t3]
                at k = (tt !! k)
                pt k = (ps !! k)
                step stp =
                  let knot = t1 + (fromIntegral stp / 16) * (t2 - t1)
                      a k = add (pt k) (scale (sub (pt (k + 1)) (pt k)) ((knot - at k) / (at (k + 1) - at k)))
                      as = map a [0, 1, 2]
                      b k = add (as !! k) (scale (sub (as !! (k + 1)) (as !! k)) ((knot - at k) / (at (k + 2) - at k)))
                      bs = map b [0, 1]
                  in add (bs !! 0) (scale (sub (bs !! 1) (bs !! 0)) ((knot - t1) / (t2 - t1)))
            in p1 : map step [1 .. 15 :: Int]
          seg4 _ = []
      in concatMap seg4 quads ++ [last s]

-- | Add the frame's synthetic noise (§K.4? noise synthesis) to XYB
-- colour planes. The noise field is drawn per group from xorshift128+
-- seeded by the frame's position in the file and the group's origin, then
-- high-passed with a 5x5 kernel that reads across group borders and
-- mirrors at the frame's edges; groups tile the frame exactly, so that is a
-- convolution over one frame-wide field.
applyNoise :: FrameHdr -> Int -> Int -> UArray Int Float -> (Float, Float) -> Img s -> ST s ()
applyNoise fh visible invisible lut corr img =
  let h = min (imH img) (fhH fh)
      rows = noiseRows fh visible invisible lut corr img
  in if min (imW img) (fhW fh) * h >= 1048576
       then parRows h rows
       else rows 0 h

-- | A disjoint output band, seeded at its first required raw row. Its halo
-- is regenerated from the RNG, never read from another band's image output.
-- Each worker owns its RNG table, raw rows and sum rings. Jump offsets count
-- whole sixteen-sample batches, including the discarded partial-row tail.
noiseRows :: FrameHdr -> Int -> Int -> UArray Int Float -> (Float, Float)
          -> Img s -> Int -> Int -> ST s ()
noiseRows fh visible invisible lut (corrX, corrB) img ya yb = do
  let !fw = fhW fh
      !fhh = fhH fh
      !gdim = 1 `shiftL` fhGroupShift fh
      !gcols = ceilDiv fw gdim
      !seed0 = (fromIntegral visible `shiftL` 32) + fromIntegral invisible :: Word64
  -- The random field is produced one row at a time rather than one group
  -- at a time, so only eight rows of each field are ever live (five for
  -- the high-pass cross plus the two generated ahead of it for the
  -- horizontal sums). The spec generates a group's three channels as one
  -- continuous xorshift stream, X then Y then B, so each channel's
  -- starting state is the group's seed advanced by the whole rows before
  -- it: `xsJump` is that advance, exactly, through the GF(2) matrix powers
  -- of the (linear) state transition. The stream each channel draws is
  -- the same as before; only when its rows are drawn has changed.
  let nr = min fhh 8
      first = max 0 (ya - 2)
  fields <- replicateM 3 (newArray (0, max 0 (fw * nr - 1)) 0 :: ST s (FPlane s))
  -- Write the random Float representation as Word32 bits. This avoids
  -- a castWord32ToFloat runtime call per sample; later passes only use
  -- the Float view, after generation has finished writing the needed rows.
  fieldsW <- mapM castSTUArray fields
  rings <- replicateM 3 (newArray (0, 8 * fw - 1) 0 :: ST s (FPlane s))
  -- per group column, the three channels' sixteen-word xorshift states
  states <- newArray (0, max 0 (3 * 16 * gcols - 1)) 0 :: ST s (STUArray s Int Word64)
  let seedRow y = forLoopJ 0 (gcols - 1) $ \gx -> do
        let y0 = y - y `mod` gdim
            x0 = gx * gdim
            gw = min gdim (fw - x0)
            gh = min gdim (fhh - y0)
            seed1 = (fromIntegral x0 `shiftL` 32) + fromIntegral y0 :: Word64
            -- a channel's rows are whole batches of sixteen, the tail of
            -- the last batch discarded: one state step per batch
            batches = (gw + 15) `div` 16
            steps = gh * batches
            skip = (y - y0) * batches
            bX = gx * 16
            bY = (gcols + gx) * 16
            bB = (2 * gcols + gx) * 16
        seedXorShift states bX seed0 seed1
        forLoopJ 0 7 $ \lane -> do
          a <- unsafeRead states (bX + lane)
          b <- unsafeRead states (bX + 8 + lane)
          let (a', b') = xsJump skip a b
              (c, d) = xsJump steps a' b'
              (e, f) = xsJump steps c d
          unsafeWrite states (bX + lane) a'
          unsafeWrite states (bX + 8 + lane) b'
          unsafeWrite states (bY + lane) c
          unsafeWrite states (bY + 8 + lane) d
          unsafeWrite states (bB + lane) e
          unsafeWrite states (bB + 8 + lane) f
      generateRow y = do
        when (y == first || y `mod` gdim == 0) (seedRow y)
        let !rowBase = (y `mod` nr) * fw
        forLoopJ 0 (gcols - 1) $ \gx -> do
          let !x0 = gx * gdim
              !gw = min gdim (fw - x0)
              !full = gw `div` 16
              !tailN = gw `mod` 16
          forM_ (zip [0 :: Int ..] fieldsW) $ \(c, fld) -> do
            let !sb = (c * gcols + gx) * 16
            forLoopJ 0 (full - 1) $ \k -> xsFillFull states sb fld (rowBase + x0 + k * 16)
            when (tailN /= 0) $ xsFill states sb fld (rowBase + x0 + full * 16) tailN
  let mirror n i | i < 0 = mirror n (negate i - 1)
                 | i >= n = mirror n (2 * n - 1 - i)
                 | otherwise = i
  let hsumRow r = forM_ (zip fields rings) $ \(fld, ring) -> do
        let src = (r `mod` nr) * fw
            dst = (r .&. 7) * fw
            rd x = unsafeRead fld (src + mirror fw x)
            edge x = do
              a <- rd (x - 2); b' <- rd (x - 1); c <- rd x; d <- rd (x + 1); e <- rd (x + 2)
              unsafeWrite ring (dst + x) ((((a + b') + c) + d) + e)
        if fw < 5 then forLoopJ 0 (fw - 1) edge else do
          edge 0; edge 1
          forLoopJ 2 (fw - 3) $ \x -> do
            a <- unsafeRead fld (src + x - 2)
            b' <- unsafeRead fld (src + x - 1)
            c <- unsafeRead fld (src + x)
            d <- unsafeRead fld (src + x + 1)
            e <- unsafeRead fld (src + x + 2)
            unsafeWrite ring (dst + x) ((((a + b') + c) + d) + e)
          edge (fw - 2); edge (fw - 1)
  let [nX, nY, nB] = fields
      [sX, sY, sB] = rings
      !w = imW img
      !h = imH img
      !st = imStride img
      [px, py, pb] = imColour img
      !corrX' = corrX
      !corrB' = corrB
      !lut' = lut
      lutAt v = let sc = max 0 (v * 3)
                    i = min 7 (truncate sc) :: Int
                    f = sc - fromIntegral i
                in (lut' `unsafeAt` (i + 1) - lut' `unsafeAt` i) * f + lut' `unsafeAt` i
      {-# INLINE lutAt #-}
  -- per-chunk scratch for the three high-pass values (phase A), so the
  -- apply loop (phase B) carries few live values
  hp <- newArray (0, 3 * 64 - 1) 0 :: ST s (FPlane s)
  forLoopJ first (min (ya + 1) (fhh - 1)) $ \y -> generateRow y >> hsumRow y
  forLoopJ ya (yb - 1) $ \y -> do
    let next = y + 2
    when (next < fhh) $ do
      generateRow next
      hsumRow next
    let !r0 = (mirror fhh (y - 2) .&. 7) * fw
        !r1 = (mirror fhh (y - 1) .&. 7) * fw
        !r2 = (y .&. 7) * fw
        !r3 = (mirror fhh (y + 1) .&. 7) * fw
        !r4 = (mirror fhh (y + 2) .&. 7) * fw
        !base = (y `mod` nr) * fw
        high fld ring x = do
          a <- unsafeRead ring (r0 + x)
          b' <- unsafeRead ring (r1 + x)
          c <- unsafeRead ring (r2 + x)
          d <- unsafeRead ring (r3 + x)
          e <- unsafeRead ring (r4 + x)
          v <- unsafeRead fld (base + x)
          pure (0.16 * ((((a + b') + c) + d) + e) - v * 4)
        {-# INLINE high #-}
        !wq = min w fw
    forLoopStep' 0 (wq - 1) 64 $ \x0 -> do
      let !n = min 64 (wq - x0)
      forLoopJ 0 (n - 1) $ \k -> do
        gx <- high nX sX (x0 + k)
        unsafeWrite hp (3 * k) gx
      forLoopJ 0 (n - 1) $ \k -> do
        gy <- high nY sY (x0 + k)
        unsafeWrite hp (3 * k + 1) gy
      forLoopJ 0 (n - 1) $ \k -> do
        gb <- high nB sB (x0 + k)
        unsafeWrite hp (3 * k + 2) gb
      forLoopJ 0 (n - 1) $ \k -> do
        gx <- unsafeRead hp (3 * k)
        gy <- unsafeRead hp (3 * k + 1)
        gb <- unsafeRead hp (3 * k + 2)
        let i = y * st + x0 + k
        xv <- unsafeRead px i
        yv <- unsafeRead py i
        bv <- unsafeRead pb i
        let sx = lutAt (xv + yv)
            sy = lutAt (yv - xv)
            nx = 0.22 * sx * (0.0078125 * gx + 0.9921875 * gb)
            ny = 0.22 * sy * (0.0078125 * gy + 0.9921875 * gb)
        unsafeWrite px i (xv + corrX' * (nx + ny) + nx - ny)
        unsafeWrite py i (yv + nx + ny)
        unsafeWrite pb i (bv + corrB' * (nx + ny))

-- Eight-lane xorshift128+ as libjxl seeds it: the sixteen words at @sb@
-- of @st@ (lane i's halves at @sb + i@ and @sb + 8 + i@).
seedXorShift :: STUArray s Int Word64 -> Int -> Word64 -> Word64 -> ST s ()
seedXorShift st sb seed0 seed1 = do
  let mix z0 = let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
                   z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
               in z2 `xor` (z2 `shiftR` 31)
      s0s = take 8 (iterate mix (mix (seed0 + 0x9E3779B97F4A7C15)))
      s1s = take 8 (iterate mix (mix (seed1 + 0x9E3779B97F4A7C15)))
  forM_ (zip [0 ..] (zip s0s s1s)) $ \(i, (a, b)) -> do
    unsafeWrite st (sb + i) a
    unsafeWrite st (sb + 8 + i) b

-- One batch: sixteen 32-bit values, each lane's low half then high half,
-- the first @n@ of them stored at @off@ as floats in [1, 2).
{-# INLINE xsFill #-}
xsFill :: STUArray s Int Word64 -> Int -> STUArray s Int Word32 -> Int -> Int -> ST s ()
xsFill st !sb out !off !n = forLoopJ 0 7 $ \i -> do
  s1a <- unsafeRead st (sb + i)
  s0 <- unsafeRead st (sb + 8 + i)
  let ret = s1a + s0
      s1b = s1a `xor` (s1a `shiftL` 23)
      toF :: Word64 -> Word32
      toF v = (fromIntegral v `shiftR` 9) .|. 0x3f800000
  unsafeWrite st (sb + i) s0
  unsafeWrite st (sb + 8 + i) (s1b `xor` (s0 `xor` (s1b `shiftR` 18) `xor` (s0 `shiftR` 5)))
  when (2 * i < n) $ unsafeWrite out (off + 2 * i) (toF (ret .&. 0xffffffff))
  when (2 * i + 1 < n) $ unsafeWrite out (off + 2 * i + 1) (toF (ret `shiftR` 32))

-- The full-batch caller guarantees space for all sixteen values. Keeping
-- this loop separate removes lane tests that GHC retains for xsFill 16.
{-# INLINE xsFillFull #-}
xsFillFull :: STUArray s Int Word64 -> Int -> STUArray s Int Word32 -> Int -> ST s ()
xsFillFull st !sb out !off = forLoopJ 0 7 $ \i -> do
  s1a <- unsafeRead st (sb + i)
  s0 <- unsafeRead st (sb + 8 + i)
  let ret = s1a + s0
      s1b = s1a `xor` (s1a `shiftL` 23)
      toF :: Word64 -> Word32
      toF v = (fromIntegral v `shiftR` 9) .|. 0x3f800000
  unsafeWrite st (sb + i) s0
  unsafeWrite st (sb + 8 + i) (s1b `xor` (s0 `xor` (s1b `shiftR` 18) `xor` (s0 `shiftR` 5)))
  unsafeWrite out (off + 2 * i) (toF (ret .&. 0xffffffff))
  unsafeWrite out (off + 2 * i + 1) (toF (ret `shiftR` 32))

-- One lane's xorshift128+ state transition, as 'xsFill' performs it.
xsStep :: Word64 -> Word64 -> (Word64, Word64)
xsStep a b = let t = a `xor` (a `shiftL` 23)
             in (b, t `xor` (b `xor` (t `shiftR` 18) `xor` (b `shiftR` 5)))

-- The transition is shifts and xors only, so it is a linear map over
-- GF(2) on the 128 state bits, and advancing a lane @n@ steps is applying
-- the map's n-th power. The table holds the powers M^(2^k), k < 31, each
-- as 128 rows (the image of each state bit) of two words; applying one is
-- an xor of the rows selected by the state's set bits.
xsJumpTable :: UArray Int Word64
xsJumpTable = listArray (0, 31 * 256 - 1) (concatMap flat (take 31 (iterate square m1)))
  where
    m1 = [ if k < 64 then xsStep (bit k) 0 else xsStep 0 (bit (k - 64)) | k <- [0 .. 127 :: Int] ]
    flat = concatMap (\(a, b) -> [a, b])
    square m = let t = listArray (0, 255) (flat m) :: UArray Int Word64
               in [ applyRows t 0 a b | (a, b) <- m ]
    applyRows t off a b = let (x, y) = xorRows t off a 0 0 in xorRows t (off + 128) b x y

-- xor into (x, y) the rows of the matrix at @off@ selected by @w@'s bits
xorRows :: UArray Int Word64 -> Int -> Word64 -> Word64 -> Word64 -> (Word64, Word64)
xorRows t !off !w !x !y
  | w == 0 = (x, y)
  | otherwise = let k = countTrailingZeros w
                    r = off + 2 * k
                in xorRows t off (w .&. (w - 1)) (x `xor` (t `unsafeAt` r)) (y `xor` (t `unsafeAt` (r + 1)))

-- | A lane's state after @n@ transitions (n < 2^31).
xsJump :: Int -> Word64 -> Word64 -> (Word64, Word64)
xsJump n0 a0 b0 = go n0 0 a0 b0
  where
    go !n !k !a !b
      | n == 0 || k >= 31 = (a, b)
      | testBit n 0 = let off = k * 256
                          (x, y) = xorRows xsJumpTable off a 0 0
                          (a', b') = xorRows xsJumpTable (off + 128) b x y
                      in go (n `shiftR` 1) (k + 1) a' b'
      | otherwise = go (n `shiftR` 1) (k + 1) a b

-- | The frame's non-separable upsampling (§K.2), colour and extra channels
-- alike, cropped to the frame's size.
upsampleFrame :: ImgHdr -> FrameHdr -> Img s -> ST s (Img s)
upsampleFrame ih fh img
  | fhUpsampling fh <= 1 = pure img
  | otherwise = do
      let k = countTrailingZeros (fhUpsampling fh)
          ws = upWeights ih
          up p = do (q, _, _) <- upsampleBy ws k p (imW img) (imH img) (imStride img); pure q
      cs <- mapM up (imColour img)
      es <- forM (imExtra img) $ \e -> do
        p <- materializeExtra (imW img) (imH img) (imStride img) e
        DenseExtra <$> up p
      let nw = imW img `shiftL` k
          nh = imH img `shiftL` k
      pure (Img (min nw (fhW fh)) (min nh (fhH fh)) nw cs es (imDomain img))

-- The upsampling kernels: up2, up4, up8 weights (header or defaults).
upWeights :: ImgHdr -> (UArray Int Float, UArray Int Float, UArray Int Float)
upWeights ih = ( arr 15 (maybe defUp2 (map realToFrac) (ihUp2 ih))
               , arr 55 (maybe defUp4 (map realToFrac) (ihUp4 ih))
               , arr 210 (maybe defUp8 (map realToFrac) (ihUp8 ih)) )
  where arr n xs = listArray (0, n - 1) xs

-- | Upsample a plane by 2^k (k = 0 is the identity), returning the new
-- plane with its width and height (and so its stride, the width).
upsampleBy :: (UArray Int Float, UArray Int Float, UArray Int Float) -> Int -> FPlane s -> Int -> Int -> Int
           -> ST s (FPlane s, Int, Int)
upsampleBy (u2, u4, u8) k p0 w0 h0 st0
  | k <= 0 = pure (p0, st0, h0)
  | otherwise = do
      let steps = replicate (k `div` 3) (8, u8) ++ (case k `mod` 3 of { 1 -> [(2, u2)]; 2 -> [(4, u4)]; _ -> [] })
          go p w h st [] = pure (p, w, h, st)
          go p w h st ((kk, ws) : rest) = do
            q <- upsampleOnce kk ws p w h st
            go q (w * kk) (h * kk) (w * kk) rest
      (p, w, _, _) <- go p0 w0 h0 st0 steps
      pure (p, w, h0 `shiftL` k)

upsampleOnce :: Int -> UArray Int Float -> FPlane s -> Int -> Int -> Int -> ST s (FPlane s)
upsampleOnce kk ws p w h st = do
  let matN = kk `div` 2
      -- the kernel quarter for each sub-position, symmetric in x and y
      quarter = U.accumArray (\_ v -> v) 0 (0, matN * matN * 25 - 1)
        (concat [ [ ((my * matN + mx) * 25 + ky * 5 + kx, v), ((mx * matN + my) * 25 + kx * 5 + ky, v) ]
                | (wi, (y, x)) <- zip [0 ..] [ (y, x) | y <- [0 .. 5 * matN - 1], x <- [y .. 5 * matN - 1] ]
                , let (my, ky) = y `divMod` 5
                      (mx, kx) = x `divMod` 5
                      v = ws `unsafeAt` wi ]) :: UArray Int Float
      mirror n i | i < 0 = mirror n (negate i - 1)
                 | i >= n = mirror n (2 * n - 1 - i)
                 | otherwise = i
      ow = w * kk
      oh = h * kk
  out <- newArray (0, max 0 (ow * oh - 1)) 0
  -- a mirrored window of 5 rows is read per output row
  forLoopJ 0 (oh - 1) $ \y -> do
    let ry = y `div` kk
        sy = y `mod` kk
        my = min sy (kk - sy - 1)
        flipV = sy >= matN
    forLoopJ 0 (ow - 1) $ \x -> do
      let rx = x `div` kk
          sx = x `mod` kk
          mx = min sx (kk - sx - 1)
          flipH = sx >= matN
          kb = (my * matN + mx) * 25
          acc !iy !sm !mn !mxv
            | iy > 4 = pure (sm, mn, mxv)
            | otherwise = do
                let ky = if flipV then 4 - iy else iy
                    row = mirror h (ry + iy - 2) * st
                    accx !ix !s2 !n2 !x2
                      | ix > 4 = pure (s2, n2, x2)
                      | otherwise = do
                          let kx = if flipH then 4 - ix else ix
                          v <- unsafeRead p (row + mirror w (rx + ix - 2))
                          accx (ix + 1) (s2 + (quarter `unsafeAt` (kb + ky * 5 + kx)) * v) (min n2 v) (max x2 v)
                (s2, n2, x2) <- accx 0 sm mn mxv
                acc (iy + 1) s2 n2 x2
      (sm, mn, mxv) <- acc 0 0 (1 / 0) (-1 / 0)
      unsafeWrite out (y * ow + x) (max mn (min mxv sm))
  pure out

-- | Blend a normal frame onto the canvas (§F? frame blending): each
-- channel starts from its blending source's reference (or zero) and the
-- frame is placed at its offset with its mode.
blendFrame :: ImgHdr -> FrameHdr -> Refs s -> Img s -> J s (Img s)
blendFrame ih fh refs img = do
  let iw = ihW ih
      ihh = ihH ih
      ncol = length (imColour img)
      infos = replicate ncol (fhBlend fh) ++ fhEcBlend fh
      full = fhX0 fh == 0 && fhY0 fh == 0 && imW img == iw && imH img == ihh
  if full && all ((== 0) . blMode) infos then pure img else do
    frameExtra <- liftJ (mapM (materializeExtra (imW img) (imH img) (imStride img)) (imExtra img))
    let chans = imColour img ++ frameExtra
    canvas <- liftJ $ forM (zip [0 ..] infos) $ \(idx, b) -> do
      out <- newArray (0, iw * ihh - 1) 0 :: ST s (FPlane s)
      case rfSlots refs !! blSource b of
        Just r0 | imW r0 == iw && imH r0 == ihh -> do
          r <- if imDomain r0 /= imDomain img && idx < ncol then toEncoded ih r0 else pure r0
          src <- if idx < ncol then pure (imColour r !! idx)
                 else materializeExtra (imW r) (imH r) (imStride r) (imExtra r !! (idx - ncol))
          forLoopJ 0 (ihh - 1) $ \y -> forLoopJ 0 (iw - 1) $ \x ->
            unsafeRead src (y * imStride r + x) >>= unsafeWrite out (y * iw + x)
        _ -> pure ()
      pure out
    let x0 = max 0 (fhX0 fh)
        y0 = max 0 (fhY0 fh)
        x1 = min iw (fhX0 fh + imW img)
        y1 = min ihh (fhY0 fh + imH img)
        canvasExtra = drop ncol canvas
    when (x1 > x0 && y1 > y0) $ liftJ $ forM_ (zip3 [0 ..] infos (zip canvas chans)) $ \(idx, b, (base, new)) -> do
      let usesAlpha = blMode b == 2 || blMode b == 3
          isAlphaCh = usesAlpha && idx == ncol + blAlpha b
          premul = case drop (blAlpha b) (ihEC ih) of { (e : _) -> ecAssoc e; [] -> False }
          -- frame modes 0..4 as patch modes: replace, add, blend, mul-add, mul
          pmode = case blMode b of { 0 -> 1; 1 -> 2; 2 -> 4; 3 -> 6; _ -> 3 }
          ba = if usesAlpha then listToMaybe (drop (blAlpha b) canvasExtra) else Nothing
          na = if usesAlpha then listToMaybe (drop (blAlpha b) frameExtra) else Nothing
      blendRegion pmode (blClamp b) isAlphaCh premul base iw new (imStride img) ba na
        x0 y0 (x0 - fhX0 fh) (y0 - fhY0 fh) (x1 - x0) (y1 - y0)
    pure (Img iw ihh iw (take ncol canvas) (map DenseExtra (drop ncol canvas)) (imDomain img))

defUp2, defUp4, defUp8 :: [Float]
defUp2 = [ -0.01716200, -0.03452303, -0.04022174, -0.02921014, -0.00624645
         , 0.14111091, 0.28896755, 0.00278718, -0.01610267, 0.56661550
         , 0.03777607, -0.01986694, -0.03144731, -0.01185068, -0.00213539 ]
defUp4 = [ -0.02419067, -0.03491987, -0.03693351, -0.03094285, -0.00529785
         , -0.01663432, -0.03556863, -0.03888905, -0.03516850, -0.00989469
         , 0.23651958, 0.33392945, -0.01073543, -0.01313181, -0.03556694
         , 0.13048175, 0.40103025, 0.03951150, -0.02077584, 0.46914198
         , -0.00209270, -0.01484589, -0.04064806, 0.18942530, 0.56279892
         , 0.06674400, -0.02335494, -0.03551682, -0.00754830, -0.02267919
         , -0.02363578, 0.00315804, -0.03399098, -0.01359519, -0.00091653
         , -0.00335467, -0.01163294, -0.01610294, -0.00974088, -0.00191622
         , -0.01095446, -0.03198464, -0.04455121, -0.02799790, -0.00645912
         , 0.06390599, 0.22963888, 0.00630981, -0.01897349, 0.67537268
         , 0.08483369, -0.02534994, -0.02205197, -0.01667999, -0.00384443 ]
defUp8 = [ -0.02928613, -0.03706353, -0.03783812, -0.03324558, -0.00447632
         , -0.02519406, -0.03752601, -0.03901508, -0.03663285, -0.00646649
         , -0.02066407, -0.03838633, -0.04002101, -0.03900035, -0.00901973
         , -0.01626393, -0.03954148, -0.04046620, -0.03979621, -0.01224485
         , 0.29895328, 0.35757708, -0.02447552, -0.01081748, -0.04314594
         , 0.23903219, 0.41119301, -0.00573046, -0.01450239, -0.04246845
         , 0.17567618, 0.45220643, 0.02287757, -0.01936783, -0.03583255
         , 0.11572472, 0.47416733, 0.06284440, -0.02685066, 0.42720050
         , -0.02248939, -0.01155273, -0.04562755, 0.28689496, 0.49093869
         , -0.00007891, -0.01545926, -0.04562659, 0.21238920, 0.53980934
         , 0.03369474, -0.02070211, -0.03866988, 0.14229550, 0.56593398
         , 0.08045181, -0.02888298, -0.03680918, -0.00542229, -0.02920477
         , -0.02788574, -0.02118180, -0.03942402, -0.00775547, -0.02433614
         , -0.03193943, -0.02030828, -0.04044014, -0.01074016, -0.01930822
         , -0.03620399, -0.01974125, -0.03919545, -0.01456093, -0.00045072
         , -0.00360110, -0.01020207, -0.01231907, -0.00638988, -0.00071592
         , -0.00279122, -0.00957115, -0.01288327, -0.00730937, -0.00107783
         , -0.00210156, -0.00890705, -0.01317668, -0.00813895, -0.00153491
         , -0.02128481, -0.04173044, -0.04831487, -0.03293190, -0.00525260
         , -0.01720322, -0.04052736, -0.05045706, -0.03607317, -0.00738030
         , -0.01341764, -0.03965629, -0.05151616, -0.03814886, -0.01005819
         , 0.18968273, 0.33063684, -0.01300105, -0.01372950, -0.04017465
         , 0.13727832, 0.36402234, 0.01027890, -0.01832107, -0.03365072
         , 0.08734506, 0.38194295, 0.04338228, -0.02525993, 0.56408126
         , 0.00458352, -0.01648227, -0.04887868, 0.24585519, 0.62026135
         , 0.04314807, -0.02213737, -0.04158014, 0.16637289, 0.65027023
         , 0.09621636, -0.03101388, -0.04082742, -0.00904519, -0.02790922
         , -0.02117818, 0.00798662, -0.03995711, -0.01243427, -0.02231705
         , -0.02946266, 0.00992055, -0.03600283, -0.01684920, -0.00111684
         , -0.00411204, -0.01297130, -0.01723725, -0.01022545, -0.00165306
         , -0.00313110, -0.01218016, -0.01763266, -0.01125620, -0.00231663
         , -0.01374149, -0.03797620, -0.05142937, -0.03117307, -0.00581914
         , -0.01064003, -0.03608089, -0.05272168, -0.03375670, -0.00795586
         , 0.09628104, 0.27129991, -0.00353779, -0.01734151, -0.03153981
         , 0.05686230, 0.28500998, 0.02230594, -0.02374955, 0.68214326
         , 0.05018048, -0.02320852, -0.04383616, 0.18459474, 0.71517975
         , 0.10805613, -0.03263677, -0.03637639, -0.01394373, -0.02511203
         , -0.01728636, 0.05407331, -0.02867568, -0.01893131, -0.00240854
         , -0.00446511, -0.01636187, -0.02377053, -0.01522848, -0.00333334
         , -0.00819975, -0.02964169, -0.04499287, -0.02745350, -0.00612408
         , 0.02727416, 0.19446600, 0.00159832, -0.02232473, 0.74982506
         , 0.11452620, -0.03348048, -0.01605681, -0.02070339, -0.00458223 ]

cbrtF :: Float -> Float
cbrtF v = if v < 0 then negate (negate v ** (1 / 3)) else v ** (1 / 3)

-- | Linear light in [0, 1] to sRGB scaled to [0, 255]: a 4096-interval
-- table of the transfer curve, linearly interpolated (under 0.01 of a code
-- value from the exact curve anywhere).
{-# INLINE srgb255 #-}
srgb255 :: UArray Int Float -> Float -> Float
srgb255 tab v0 =
  let v = max 0 (min 1 v0) * 4096
      i = min 4095 (truncate v) :: Int
      f = v - fromIntegral i
      a = tab `unsafeAt` i
      b = tab `unsafeAt` (i + 1)
  in a + f * (b - a)

srgbTab :: UArray Int Float
srgbTab = listArray (0, 4096) [ realToFrac (enc (fromIntegral i / 4096)) | i <- [0 .. 4096 :: Int] ]
  where enc v = 255 * (if v <= 0.0031308 then 12.92 * v else 1.055 * v ** (1 / 2.4) - 0.055) :: Double

-- libjxl's ordered dither for 8-bit output: the 8x8 Bayer matrix, centred.
bayer :: UArray Int Float
bayer = listArray (0, 63) [ (fromIntegral v + 0.5) / 64 - 0.5 | v <- m ]
  where m = [ 0, 32, 8, 40, 2, 34, 10, 42, 48, 16, 56, 24, 50, 18, 58, 26
            , 12, 44, 4, 36, 14, 46, 6, 38, 60, 28, 52, 20, 62, 30, 54, 22
            , 3, 35, 11, 43, 1, 33, 9, 41, 51, 19, 59, 27, 49, 17, 57, 25
            , 15, 47, 7, 39, 13, 45, 5, 37, 63, 31, 55, 23, 61, 29, 53, 21 :: Int ]

-- Apply the EXIF-style orientation (1 = as stored).
orient :: Int -> Int -> Int -> UArray Int Word8 -> (Int, Int, UArray Int Word8)
orient o w h px
  | o <= 1 || o > 8 = (w, h, px)
  | otherwise =
      let transposed = o >= 5
          (ow, oh) = if transposed then (h, w) else (w, h)
          src ox oy = case o of
            2 -> (w - 1 - ox, oy)
            3 -> (w - 1 - ox, h - 1 - oy)
            4 -> (ox, h - 1 - oy)
            5 -> (oy, ox)
            6 -> (oy, h - 1 - ox)
            7 -> (w - 1 - oy, h - 1 - ox)
            _ -> (w - 1 - oy, ox)
          arr = runST $ do
            out <- newArray (0, ow * oh * 4 - 1) 0 :: ST s (STUArray s Int Word8)
            forM_ [0 .. oh - 1] $ \oy -> forM_ [0 .. ow - 1] $ \ox -> do
              let (sx, sy) = src ox oy
                  si = (sy * w + sx) * 4
                  di = (oy * ow + ox) * 4
              forM_ [0 .. 3] $ \k -> unsafeWrite out (di + k) (px `unsafeAt` (si + k))
            unsafeFreeze out
      in (ow, oh, arr)
