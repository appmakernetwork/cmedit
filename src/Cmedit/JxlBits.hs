{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
-- | The JPEG XL bit reader and field decoders (ISO/IEC 18181-1 §B, §9.2).
--
-- JPEG XL packs everything LSB-first. The codestream is copied once into a
-- zero-padded byte array so that any bit position can be peeked as one
-- unaligned 64-bit load ('indexWord8ArrayAsWord64#'); that is the whole
-- speed story of this module. Reading past the end yields zeros rather than
-- failing, so a reader never needs a bounds branch beyond the one cheap
-- comparison in 'peekAt'; truncation is caught afterwards by comparing the
-- final position with the section's end ('brOverrun'). Every loop in the
-- decoder is bounded by counts it read up front, which is what makes "zeros
-- forever" safe instead of a hang.
--
-- The bit position lives in slot 0 of an unboxed array, not an 'STRef': an
-- 'STRef Int' would box on every write, and every symbol writes it.
module Cmedit.JxlBits
  ( Buf, mkBuf, bufLen, bufByte
  , BR(..), newBR, brPos, brSetPos, brOverrun
  , peekAt, readBits, skipBits, readBool
  , u32, u64, f16, readEnum, zeroPad
  , unpackSigned, ceilLog2, floorLog2
  , J, jfail, jguard, liftJ, runJ
  ) where

import Control.Monad.ST (ST)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Array.Base (STUArray(..), unsafeRead, unsafeWrite, newArray)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word64, byteSwap64)
import GHC.Exts
import GHC.IO (IO(..), unsafeDupablePerformIO)
import GHC.Word (Word64(..))
import GHC.ByteOrder (ByteOrder(..), targetByteOrder)

-- | A codestream: the bytes plus 16 zero bytes of padding, so an 8-byte
-- load at any byte index up to @len + 8@ stays inside the allocation.
data Buf = Buf ByteArray# !Int

bufLen :: Buf -> Int
bufLen (Buf _ n) = n

bufByte :: Buf -> Int -> Int
bufByte (Buf ba n) (I# i)
  | I# i < n  = I# (word2Int# (word8ToWord# (indexWord8Array# ba i)))
  | otherwise = 0

mkBuf :: BS.ByteString -> Buf
mkBuf bs = unsafeDupablePerformIO $ BSU.unsafeUseAsCStringLen bs $ \(Ptr a, I# n) ->
  IO $ \s0 -> case newByteArray# (n +# 16#) s0 of
    (# s1, mba #) -> case setByteArray# mba 0# (n +# 16#) 0# s1 of
      s2 -> case copyAddrToByteArray# a mba 0# n s2 of
        s3 -> case unsafeFreezeByteArray# mba s3 of
          (# s4, ba #) -> (# s4, Buf ba (I# n) #)

-- | 64 bits starting at bit @p@ (at least 57 of them meaningful), zero past
-- the end of the data.
{-# INLINE peekAt #-}
peekAt :: Buf -> Int -> Int
peekAt (Buf ba n) p =
  let !b@(I# b#) = p `unsafeShiftR` 3
  in if b <= n + 8
       then let w = W64# (indexWord8ArrayAsWord64# ba b#)
                w' = case targetByteOrder of
                       LittleEndian -> w
                       BigEndian    -> byteSwap64 w
            in fromIntegral (w' `unsafeShiftR` (p .&. 7))
       else 0

-- | A reader over one codestream. Slot 0 of 'brSt' is the bit position.
data BR s = BR { brBuf :: !Buf, brSt :: !(STUArray s Int Int) }

newBR :: Buf -> Int -> ST s (BR s)
newBR buf bitPos = do
  st <- newArray (0, 0) 0
  unsafeWrite st 0 bitPos
  pure (BR buf st)

{-# INLINE brPos #-}
brPos :: BR s -> ST s Int
brPos br = unsafeRead (brSt br) 0

{-# INLINE brSetPos #-}
brSetPos :: BR s -> Int -> ST s ()
brSetPos br = unsafeWrite (brSt br) 0

-- | Has the reader consumed bits past byte offset @endByte@?
brOverrun :: BR s -> Int -> ST s Bool
brOverrun br endByte = do
  p <- brPos br
  pure (p > 8 * endByte)

-- | Read @n@ bits, @0 <= n <= 56@.
{-# INLINE readBits #-}
readBits :: BR s -> Int -> ST s Int
readBits (BR buf st) n = do
  p <- unsafeRead st 0
  unsafeWrite st 0 (p + n)
  pure (peekAt buf p .&. ((1 `unsafeShiftL` n) - 1))

{-# INLINE skipBits #-}
skipBits :: BR s -> Int -> ST s ()
skipBits (BR _ st) n = do
  p <- unsafeRead st 0
  unsafeWrite st 0 (p + n)

{-# INLINE readBool #-}
readBool :: BR s -> ST s Bool
readBool br = (/= 0) <$> readBits br 1

-- | @U32(d0, d1, d2, d3)@: a 2-bit selector picks one distribution, each an
-- @(offset, bits)@ pair; @bits = 0@ is a constant.
u32 :: BR s -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> ST s Int
u32 br d0 d1 d2 d3 = do
  sel <- readBits br 2
  let (o, nb) = case sel of { 0 -> d0; 1 -> d1; 2 -> d2; _ -> d3 }
  if nb == 0 then pure o else (+ o) <$> readBits br nb

u64 :: BR s -> ST s Int
u64 br = do
  sel <- readBits br 2
  case sel of
    0 -> pure 0
    1 -> (+ 1) <$> readBits br 4
    2 -> (+ 17) <$> readBits br 8
    _ -> do
      v0 <- readBits br 12
      let go !v !sh = do
            more <- readBits br 1
            if more == 0 then pure v
            else if sh == 60 then do
              b <- readBits br 4
              pure (v .|. (b `shiftL` sh))
            else do
              b <- readBits br 8
              go (v .|. (b `shiftL` sh)) (sh + 8)
      go v0 12

-- | A binary16 float; Nothing for NaN/infinity (invalid in a codestream).
f16 :: BR s -> ST s (Maybe Double)
f16 br = do
  v <- readBits br 16
  let neg = v .&. 0x8000 /= 0
      m = v .&. 0x3ff
      e = (v `shiftR` 10) .&. 0x1f
      sgn x = if neg then negate x else x
  pure $ if e == 0x1f then Nothing
         else if e == 0 then Just (sgn (fromIntegral m / 1024 / 16384))
         else Just (sgn ((1 + fromIntegral m / 1024) * 2 ^^ (e - 15)))

readEnum :: BR s -> ST s Int
readEnum br = u32 br (0,0) (1,0) (2,4) (18,6)

-- | ZeroPadToByte (the padding's value is not checked).
zeroPad :: BR s -> ST s ()
zeroPad br = do
  p <- brPos br
  brSetPos br ((p + 7) .&. complement 7)

{-# INLINE unpackSigned #-}
unpackSigned :: Int -> Int
unpackSigned u = (u `unsafeShiftR` 1) `xor` negate (u .&. 1)

-- | ceil(log2(x + 1)): the bits needed to hold 0..x.
ceilLog2 :: Int -> Int
ceilLog2 x | x <= 0 = 0
           | otherwise = finiteBitSize x - countLeadingZeros x

floorLog2 :: Int -> Int
floorLog2 x | x <= 0 = -1
            | otherwise = finiteBitSize x - 1 - countLeadingZeros x

-- | The structural decoder's monad. Hot loops run in plain 'ST' (via
-- 'liftJ') and report through state; only header-level code pays for the
-- 'Either' plumbing.
type J s = ExceptT String (ST s)

jfail :: String -> J s a
jfail = throwError

{-# INLINE jguard #-}
jguard :: Bool -> String -> J s ()
jguard ok msg = if ok then pure () else throwError msg

{-# INLINE liftJ #-}
liftJ :: ST s a -> J s a
liftJ = lift

runJ :: J s a -> ST s (Either String a)
runJ = runExceptT
