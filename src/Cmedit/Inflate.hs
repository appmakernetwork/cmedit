{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
-- | DEFLATE decompression (RFC 1951), hand-rolled.
--
-- This started life inside "Cmedit.Image" as PNG's decompressor and moved out
-- when a second caller appeared: PDF stores nearly every content stream under
-- @\/FlateDecode@ ("Cmedit.Pdf"). It is a leaf module — 'Cmedit.Types' is not
-- even needed — so anything may depend on it.
--
-- The two entry points differ only in whether the caller knows the output
-- size. PNG does (image dimensions give it exactly), so 'inflate' writes into
-- one pre-sized buffer and hands back the unboxed array the unfilter pass
-- wants. PDF does not — a compressed stream declares its /compressed/ length
-- and nothing else — so 'inflateDyn' grows its buffer geometrically and
-- returns a 'BS.ByteString'. Both share one decoder ('inflateWith'); only the
-- sink differs.
--
-- 'Huff' and 'buildHuff' live here rather than in a decoder because JPEG's
-- scan reader builds the same canonical-Huffman tables from its own DHT
-- segments and walks them the same way.
module Cmedit.Inflate
  ( -- * Decompression
    inflate
  , inflateDyn
  , inflateInto
  , inflateIntoSegs
  , inflateIntoSegsW
    -- * Canonical Huffman tables (shared with JPEG)
  , Huff(..)
  , buildHuff
    -- * The prefix-table decoder (shared with WebP lossless, whose
    -- bit order and canonical codes are DEFLATE's)
  , InflateHuff(..)
  , buildInflateHuff
  , buildInflateHuffN
  , decodeSymSlow
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Array (Array)
import qualified Data.Array as A
import Data.Array.ST (STUArray, newArray, readArray, writeArray, runSTUArray)
import Data.Array.MArray (freeze)
import Data.Array.Unboxed (UArray, (!))
import qualified Data.Array.Unboxed as U
import Data.Bits
import Data.STRef
import Data.Word (Word8, Word16, Word32, Word64, byteSwap32, byteSwap64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BSU
import Data.Array.Base (STUArray(..), unsafeRead, unsafeWrite, unsafeAt)
import GHC.ByteOrder (ByteOrder(..), targetByteOrder)
import GHC.Exts (Addr#, Int(I#), Int#, MutableByteArray#, State#, Word64#, copyAddrToByteArray#, copyMutableByteArray#, gtAddr#, indexWord64OffAddr#, isTrue#, (<#), minusAddr#, plusAddr#, readIntArray#, readWord64OffAddr#, readWord8Array#, readWord8ArrayAsWord16#, readWord8ArrayAsWord32#, readWord8ArrayAsWord64#, runRW#, setByteArray#, touch#, writeWord8Array#, writeWord8ArrayAsWord32#, writeWord8ArrayAsWord64#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents)
import GHC.IO (IO(..))
import GHC.ST (ST(..))
import GHC.Word (Word8(W8#), Word16(W16#), Word32(W32#), Word64(W64#))

-- Canonical Huffman table built from per-symbol code lengths.
-- Boxed arrays here, deliberately, and measured: switching these two tables to
-- UArray made the decoder allocate MORE (504 MB against 485 MB on the same
-- image and tree). The elements are already-evaluated Ints, so a boxed lookup
-- hands back a pointer to a box that already exists, while an unboxed lookup
-- has to build a fresh box to return the Int through the ST monad. Unboxing
-- only pays where the read fuses into arithmetic — as in the IDCT's cosine
-- table, which measured the other way.
data Huff = Huff !(Array Int Int) !(Array Int Int) !Int  -- counts[len], symbols, maxLen

buildHuff :: [Int] -> Huff
buildHuff lens =
  let maxLen = maximum (0 : lens)
      counts = A.accumArray (+) 0 (0, max 1 maxLen) [(l,1) | l <- lens, l > 0]
      nsym   = sum [ counts A.! l | l <- [1 .. maxLen] ]
      -- Symbols ordered by (length, symbol): a counting sort, one pass over
      -- the lengths, instead of one pass per length (this runs for every
      -- dynamic DEFLATE block, twice).
      sorted = runSTUArray $ do
        out  <- newArray (0, max 0 (nsym - 1)) 0
        next <- newArray (0, max 1 maxLen) 0 :: ST s (STUArray s Int Int)
        let start !l !acc | l > maxLen = pure ()
                          | otherwise = do writeArray next l acc
                                           start (l + 1) (acc + counts A.! l)
        start 1 0
        let place !_ [] = pure ()
            place !sym (l : rest) = do
              when (l > 0) $ do
                k <- readArray next l
                writeArray out k sym
                writeArray next l (k + 1)
              place (sym + 1) rest
        place 0 lens
        pure out
  in Huff counts (A.listArray (0, max 0 (nsym-1))
                    ((if nsym > 0 then U.elems sorted else []) ++ [0])) maxLen

-- DEFLATE alone uses a bounded prefix table ('inflateBits'). Keep the shared JPEG Huff
-- representation unchanged. Longer/incomplete codes use the canonical walk.
data InflateHuff = InflateHuff !Huff !(UArray Int Int)

buildInflateHuff :: [Int] -> InflateHuff
buildInflateHuff = buildInflateHuffN inflateBits

-- DEFLATE's prefix-table width: 10 bits measured faster than 9 (fewer codes
-- fall to the canonical walk) without the table build dominating, as 11 began
-- to on streams with many small dynamic blocks.
inflateBits :: Int
inflateBits = 10

-- | The same table at a chosen prefix width (entries pack @sym << 4 | len@,
-- so widths up to 15 bits). DEFLATE keeps 9; WebP lossless, whose codes run
-- longer and whose alphabets are larger, uses a wider one.
buildInflateHuffN :: Int -> [Int] -> InflateHuff
buildInflateHuffN bits lens =
  let hf@(Huff counts syms maxLen) = buildHuff lens
      size = 1 `shiftL` bits
      -- Do not introduce out-of-bounds table writes for malformed trees.
      valid len slots
        | len > maxLen = True
        | otherwise = let next = slots*2 - counts A.! len
                      in next >= 0 && valid (len+1) next
      reverseCode len code = go 0 0
        where go !i !acc | i >= len = acc
                         | otherwise = go (i+1) ((acc `shiftL` 1) .|. ((code `shiftR` i) .&. 1))
      -- Every code of length <= bits fills the 2^(bits-len) slots whose low
      -- len bits are the code reversed (the stream is LSB first).
      table = runSTUArray $ do
        t <- newArray (0, size-1) 0
        let fill !len !first !index
              | len > min bits maxLen = pure ()
              | otherwise = do
                  let cnt = counts A.! len
                  forLoop 0 (cnt-1) $ \j -> do
                    let code = reverseCode len (first+j)
                        e = ((syms A.! (index+j)) `shiftL` 4) .|. len
                    forLoop 0 ((1 `shiftL` (bits-len)) - 1) $ \extra ->
                      writeArray t (code .|. (extra `shiftL` len)) e
                  fill (len+1) ((first+cnt) `shiftL` 1) (index+cnt)
        when (valid 1 1) $ fill 1 0 0
        pure t
  in InflateHuff hf table

-- Slots of the inflate reader's unboxed state array.
iBit, iOut :: Int
iBit = 0   -- bit position in the compressed stream
iOut = 1   -- next output byte index

-- Inline the bit operations into their callers: a local ST action otherwise
-- allocates at every bit boundary in this shared PNG/PDF/archive decoder.
-- 24 bits of the stream from byte @i@, LSB first, zero past the end: the
-- unchecked read is every read but the last few of a stream. 24 is enough
-- for every caller, which needs at most 16 bits past a sub-byte offset of
-- up to 7 (23 bits); a fourth byte measured 4% slower. A future caller
-- wanting more than 17 bits needs a wider window.
{-# INLINE peek24I #-}
peek24I :: BS.ByteString -> Int -> Int
peek24I dat i
  | i >= 0 && i + 2 < BS.length dat =
      u i .|. (u (i+1) `shiftL` 8) .|. (u (i+2) `shiftL` 16)
  | otherwise = b i .|. (b (i+1) `shiftL` 8) .|. (b (i+2) `shiftL` 16)
  where u j = fromIntegral (BSU.unsafeIndex dat j)
        b j = if j >= 0 && j < BS.length dat then u j else 0

{-# INLINE getBitI #-}
getBitI :: BS.ByteString -> STUArray s Int Int -> ST s Int
getBitI dat st = do
  p <- unsafeRead st iBit
  let i    = p `shiftR` 3
      byte = if i >= 0 && i < BS.length dat then fromIntegral (BSU.unsafeIndex dat i) else 0
      b    = (byte `shiftR` (p .&. 7)) .&. 1
  unsafeWrite st iBit (p+1)
  pure b

-- All callers request at most 16 bits, which with the sub-byte offset fit
-- in one 24-bit window; past the end the window is zero-padded, exactly as
-- reading byte by byte was.
{-# INLINE getBitsI #-}
getBitsI :: BS.ByteString -> STUArray s Int Int -> Int -> ST s Int
getBitsI dat st n = do
  p <- unsafeRead st iBit
  let !v | n <= 0 = 0
         | otherwise = (peek24I dat (p `shiftR` 3) `shiftR` (p .&. 7)) .&. ((1 `shiftL` n) - 1)
  unsafeWrite st iBit (p + max 0 n)
  pure v

{-# INLINE decodeSymI #-}
decodeSymI :: BS.ByteString -> STUArray s Int Int -> InflateHuff -> ST s Int
decodeSymI dat st (InflateHuff hf table) = do
  p <- unsafeRead st iBit
  let prefix = (peek24I dat (p `shiftR` 3) `shiftR` (p .&. 7)) .&. ((1 `shiftL` inflateBits) - 1)
      entry = table `unsafeAt` prefix
      len = entry .&. 15
  if len == 0 then decodeSymSlowI dat st hf
  else unsafeWrite st iBit (p+len) >> pure (entry `shiftR` 4)

-- The canonical walk, out of line: codes longer than the table are rare,
-- and inlined it was copied into every symbol read.
{-# NOINLINE decodeSymSlowI #-}
decodeSymSlowI :: BS.ByteString -> STUArray s Int Int -> Huff -> ST s Int
decodeSymSlowI = decodeSymSlow

{-# INLINE decodeSymSlow #-}
decodeSymSlow :: BS.ByteString -> STUArray s Int Int -> Huff -> ST s Int
decodeSymSlow dat st (Huff counts syms maxLen) = go 1 0 0 0
  where go !len !code !first !index
          | len > maxLen = pure (-1)
          | otherwise = do
              b <- getBitI dat st
              let code1 = code .|. b
                  cnt   = counts A.! len
              if code1 - first < cnt
                then pure (syms A.! (index + (code1 - first)))
                else go (len+1) (code1 `shiftL` 1) ((first+cnt) `shiftL` 1) (index+cnt)

-- | Inflate a raw DEFLATE stream starting at byte @startByte@, producing up to
-- @outSize@ bytes (the caller knows the size; PNG does). A hard error (invalid
-- Huffman code or block type) yields @Left@ so corrupt input is reported up
-- front rather than rendered as garbage; a merely-short stream is tolerated
-- (zero-padded) so slightly-truncated-but-valid files still display.
inflate :: BS.ByteString -> Int -> Int -> Either String (UArray Int Word8)
inflate dat startByte outSize = runST $ do
  arr <- newArray (0, max 0 (outSize - 1)) 0 :: ST s (STUArray s Int Word8)
  err <- inflateFixed 0 (mkSegs [dat]) [] startByte arr 0 outSize
  frozen <- freeze arr
  pure (maybe (Right frozen) Left err)

-- | 'inflate' into a caller's buffer: up to @outSize@ bytes written from
-- index @base@ on, which the buffer must hold. Bytes before @base@ are not
-- part of the stream: a distance reaching past it reads zeros, exactly as one
-- reaching past the start of 'inflate''s own buffer does. This lets PNG
-- inflate into the tail of its output image and decode in place.
inflateInto :: BS.ByteString -> Int -> STUArray s Int Word8 -> Int -> Int
            -> ST s (Maybe String)
inflateInto dat startByte arr base outSize =
  inflateFixed 0 (mkSegs [dat]) [] startByte arr base outSize

-- | 'inflateInto' over a list of contiguous byte slices, avoiding an upfront
-- concatenation copy (e.g. multi-IDAT PNGs). Semantics are identical to
-- @inflateInto (BS.concat chunks)@.
--
-- The list is consumed as a stream: the decoder holds a window of about
-- 'streamWindow' bytes ahead of its read position and lets go of every slice
-- behind it, so a lazily produced list (PNG IDAT payloads read from the file
-- as the decode reaches them) is never resident as a whole.
inflateIntoSegs :: [BS.ByteString] -> Int -> STUArray s Int Word8 -> Int -> Int
                -> ST s (Maybe String)
inflateIntoSegs = inflateIntoSegsW streamWindow

-- | 'inflateIntoSegs' with the window size given (tests use small ones, so
-- that blocks straddle windows).
inflateIntoSegsW :: Int -> [BS.ByteString] -> Int -> STUArray s Int Word8 -> Int -> Int
                 -> ST s (Maybe String)
inflateIntoSegsW win chunks startByte arr base outSize =
  let !w = max minWindow win
      (segs, rest) = advanceWindow w (mkSegsAt 0 [] True) chunks 0 (startByte + w)
  in inflateFixed w segs rest startByte arr base outSize

-- Input held ahead of the reader. Large enough that the window is rebuilt a
-- few times a megabyte and a block rarely has to be resumed; small next to
-- the file it saves holding.
streamWindow :: Int
streamWindow = 256 * 1024

-- The least window: a block header plus its dynamic code tables (at most
-- about 600 bytes) and the bit buffer's lookahead always fit in half of it.
minWindow :: Int
minWindow = 4096

-- Slide a window forward: keep the slices that end after @lo@, then append
-- from @rest@ until the window reaches @hi@ or the stream ends. The slices
-- behind @lo@ are no longer referenced, which is what lets a lazily produced
-- stream be collected as it is decoded.
advanceWindow :: Int -> Segs -> [BS.ByteString] -> Int -> Int -> (Segs, [BS.ByteString])
advanceWindow _ (Segs n tot starts arr _) rest lo hi =
  let firstKept = head ([ j | j <- [0 .. n - 1], starts `unsafeAt` (j + 1) > lo ] ++ [n])
      kept      = [ arr A.! j | j <- [firstKept .. n - 1] ]
      base      = starts `unsafeAt` firstKept
      grab !acc !end rs
        | end >= hi = (reverse acc, rs)
        | otherwise = case rs of
            []       -> (reverse acc, [])
            (b : bs) -> grab (b : acc) (end + BS.length b) bs
      (added, rest') = grab [] tot rest
  in (mkSegsAt base (kept ++ added) (not (null rest')), rest')

-- The status a Huffman block returns when its window ran out mid-block.
needInput :: String
needInput = "\0need input"

--------------------------------------------------------------------------------
-- Fast known-size DEFLATE decoder ('inflateFixed')
--
-- Keeps the LSB-first 64-bit bit buffer (bytePos, bitBuf, bitCnt) and the
-- output position in unboxed loop arguments rather than a mutable state array.
-- Invariant: streamBitPos == bytePos * 8 - bitCnt, 0 <= bitCnt <= 63, and all
-- bits in bitBuf at positions >= bitCnt are either 0 or the exact stream bits
-- at bytePos*8 + (k - bitCnt), so OR-ing a fresh 64-bit load shifted left by
-- bitCnt is always idempotent.
--------------------------------------------------------------------------------

{-# INLINE le64Raw #-}
le64Raw :: Addr# -> ForeignPtrContents -> Int -> Word64
le64Raw addr# fpc (I# pos#) =
  let !w = W64# (runRW# $ \s0# ->
        case readWord64OffAddr# (plusAddr# addr# pos#) 0# s0# of
          (# s1#, w# #) -> case touch# fpc s1# of
            _ -> w#)
  in case targetByteOrder of
       LittleEndian -> w
       BigEndian    -> byteSwap64 w

-- Non-empty input slices with cumulative byte offsets, padded with one
-- trailing sentinel so @segStarts ! (k + 1)@ is always in bounds for @k <= n@.
-- The last field says more input follows the last slice: the value is then a
-- window onto a longer stream ('inflateIntoSegs'), and a refill that reaches
-- its end stops short instead of supplying the zero padding that only the
-- true end of the stream gets.
data Segs = Segs !Int !Int !(UArray Int Int) !(Array Int BS.ByteString) !Bool

{-# INLINE segTotal #-}
segTotal :: Segs -> Int
segTotal (Segs _ tot _ _ _) = tot

mkSegs :: [BS.ByteString] -> Segs
mkSegs raw = mkSegsAt 0 raw False

-- | Slices starting at stream offset @base@ (a window when @more@).
mkSegsAt :: Int -> [BS.ByteString] -> Bool -> Segs
mkSegsAt base raw more =
  let nonEmpty = filter (not . BS.null) raw
      !n       = length nonEmpty
      starts   = scanl (\acc b -> acc + BS.length b) base nonEmpty
      !tot     = last starts
      !sArr    = U.listArray (0, n + 1) (starts ++ [tot])
      !bArr    = A.listArray (0, n) (nonEmpty ++ [BS.empty])
  in Segs n tot sArr bArr more

-- Binary search for the segment containing byte @pos@ (0 if pos <= 0,
-- @segCount@ if pos >= @segTotal@).
{-# INLINE seekSeg #-}
seekSeg :: Segs -> Int -> Int
seekSeg (Segs !n !tot !starts _ _) !pos
  | pos <= 0   = 0
  | pos >= tot = n
  | otherwise  = go 0 n
  where
    go !lo !hi
      | lo + 1 >= hi = lo
      | otherwise =
          let !mid = (lo + hi) `unsafeShiftR` 1
          in if starts `unsafeAt` mid <= pos then go mid hi else go lo mid

-- Advance segment index forward to contain @pos@ (assumes @pos >= starts ! k@
-- whenever @k < n@ and @pos >= 0@).
{-# INLINE advSeg #-}
advSeg :: Segs -> Int -> Int -> Int
advSeg (Segs !n _ !starts _ _) !k0 !pos = go k0
  where
    go !k
      | k < n && pos >= starts `unsafeAt` (k + 1) = go (k + 1)
      | otherwise = k

{-# INLINE segByteAt #-}
segByteAt :: Segs -> Int -> Int
segByteAt segs@(Segs _ !tot !starts !arr _) !idx
  | idx < 0 || idx >= tot = 0
  | otherwise =
      let !k  = seekSeg segs idx
          !s0 = starts `unsafeAt` k
      in fromIntegral (BSU.unsafeIndex (arr A.! k) (idx - s0))

{-# NOINLINE copyStoredSegs #-}
copyStoredSegs :: Segs -> Int -> Int -> STUArray s Int Word8 -> Int -> ST s ()
copyStoredSegs segs@(Segs !n !tot !starts !arr _) !srcStart !toCopy !(STUArray _ _ _ dst#) !dstOff
  | toCopy <= 0 = pure ()
  | otherwise = go (seekSeg segs srcStart) srcStart dstOff toCopy
  where
    go !k !pos !o !remN
      | remN <= 0 = pure ()
      | pos < 0 = do
          let !z = min remN (negate pos)
          zeroOut o z
          go 0 0 (o + z) (remN - z)
      | pos >= tot || k >= n = zeroOut o remN
      | otherwise =
          let !s0 = starts `unsafeAt` k
              !s1 = starts `unsafeAt` (k + 1)
              !avail = s1 - pos
              !chunk = min remN avail
          in if chunk <= 0
               then go (k + 1) pos o remN
               else case arr A.! k of
                 BI.BS (ForeignPtr addr# fpc) _ -> do
                   copyFromAddr addr# fpc (pos - s0) o chunk
                   go (k + 1) (pos + chunk) (o + chunk) (remN - chunk)
    zeroOut (I# off#) (I# len#) = ST $ \s# ->
      case setByteArray# dst# off# len# 0# s# of s1# -> (# s1#, () #)
    copyFromAddr addr# fpc (I# sOff#) (I# dOff#) (I# len#) = do
      ST $ \s# ->
        case copyAddrToByteArray# (plusAddr# addr# sOff#) dst# dOff# len# s# of
          s1# -> (# s1#, () #)
      unsafeIOToST (IO $ \s# -> case touch# fpc s# of s1# -> (# s1#, () #))

-- Refill the 64-bit bit buffer to 56..63 valid bits from segmented input.
{-# INLINE refillSeg #-}
refillSeg :: Segs -> Int -> Int -> Word64 -> Int -> (# Int#, Int#, Word64#, Int# #)
refillSeg segs@(Segs !n _ !starts !arr _) !k !p !bb !bc
  | k < n
  , let !s0 = starts `unsafeAt` k
        !s1 = starts `unsafeAt` (k + 1)
  , p >= s0 && p + 8 <= s1 =
      case arr A.! k of
        BI.BS (ForeignPtr addr# fpc) _ ->
          let !w   = le64Raw addr# fpc (p - s0)
              !bb' = bb .|. (w `unsafeShiftL` bc)
              !p'  = p + ((63 - bc) `unsafeShiftR` 3)
              !bc' = bc .|. 56
          in case (k, p', bb', bc') of
               (I# k#, I# p#, W64# bb#, I# bc#) -> (# k#, p#, bb#, bc# #)
  | otherwise =
      case (k, p, bb, bc) of
        (I# k#, I# p#, W64# bb#, I# bc#) -> refillSlowSeg segs k# p# bb# bc#

{-# OPAQUE refillSlowSeg #-}
refillSlowSeg :: Segs -> Int# -> Int# -> Word64# -> Int# -> (# Int#, Int#, Word64#, Int# #)
refillSlowSeg segs@(Segs !n _ !starts !arr more) k0# p0# bb0# bc0# =
  go (I# k0#) (I# p0#) (W64# bb0#) (I# bc0#)
  where
    go !k !p !bb !bc
      | bc >= 56 = case (k, p, bb, bc) of
          (I# k#, I# p#, W64# bb#, I# bc#) -> (# k#, p#, bb#, bc# #)
      | p < 0 = go k (p + 1) bb (bc + 8)
      | otherwise =
          let !k1 = advSeg segs k p
          in if k1 < n
               then
                 let !s0 = starts `unsafeAt` k1
                     !b  = fromIntegral (BSU.unsafeIndex (arr A.! k1) (p - s0)) :: Word64
                 in go k1 (p + 1) (bb .|. (b `unsafeShiftL` bc)) (bc + 8)
               else if more
                 then case (k1, p, bb, bc) of   -- end of a window: stop short
                   (I# k#, I# p#, W64# bb#, I# bc#) -> (# k#, p#, bb#, bc# #)
                 else go k1 (p + 1) bb (bc + 8)

-- 10-bit bit-reversal table for O(1) prefix-table construction.
rev10 :: UArray Int Int
rev10 = U.listArray (0, 1023) [rev i | i <- [0 .. 1023]]
  where
    rev x = go 0 0
      where
        go !k !acc | k >= 10   = acc
                   | otherwise = go (k + 1) ((acc `unsafeShiftL` 1) .|. ((x `unsafeShiftR` k) .&. 1))

clOrderU :: UArray Int Int
clOrderU = U.listArray (0, 18) clOrder

fixedLitLensU :: UArray Int Int
fixedLitLensU = U.listArray (0, 287)
  ([8 | _ <- [0..143 :: Int]] ++ [9 | _ <- [144..255 :: Int]]
   ++ [7 | _ <- [256..279 :: Int]] ++ [8 | _ <- [280..287 :: Int]])

fixedDistLensU :: UArray Int Int
fixedDistLensU = U.listArray (0, 29) (replicate 30 5)

{-# INLINE zeroWord32Arr #-}
zeroWord32Arr :: STUArray s Int Word32 -> Int -> ST s ()
zeroWord32Arr (STUArray _ _ _ mba#) !n =
  let !(I# bytes#) = n `unsafeShiftL` 2
  in ST $ \s# -> case setByteArray# mba# 0# bytes# 0# s# of s1# -> (# s1#, () #)

{-# INLINE zeroIntArr #-}
zeroIntArr :: STUArray s Int Int -> Int -> ST s ()
zeroIntArr (STUArray _ _ _ mba#) !n =
  let !(I# bytes#) = n * (finiteBitSize (0 :: Int) `unsafeShiftR` 3)
  in ST $ \s# -> case setByteArray# mba# 0# bytes# 0# s# of s1# -> (# s1#, () #)

-- Reusable scratch buffers for allocation-free dynamic Huffman decoding
-- across all blocks in a stream (~16 KB total, allocated once per stream).
-- 2-level combined lookup table sizes (libdeflate-style).
litRootBits, litRootMask, litTblCap :: Int
litRootBits = 10
litRootMask = (1 `unsafeShiftL` litRootBits) - 1
litTblCap   = 2048

distRootBits, distRootMask, distTblCap :: Int
distRootBits = 9
distRootMask = (1 `unsafeShiftL` distRootBits) - 1
distTblCap   = 1024

{-# INLINE zeroIntRange #-}
zeroIntRange :: STUArray s Int Int -> Int -> Int -> ST s ()
zeroIntRange (STUArray _ _ _ mba#) !off !n =
  let !sh = if finiteBitSize (0 :: Int) == 64 then 3 else 2
      !(I# offB#) = off `unsafeShiftL` sh
      !(I# lenB#) = n `unsafeShiftL` sh
  in ST $ \s# -> case setByteArray# mba# offB# lenB# 0# s# of s1# -> (# s1#, () #)

distTblOff :: Int
distTblOff = litTblCap

-- Reusable scratch buffers for allocation-free dynamic Huffman decoding
-- across all blocks in a stream (~28 KB total, allocated once per stream).
-- Lit/len (0..litTblCap-1) and dist (distTblOff..distTblOff+distTblCap-1)
-- share one table array to save a register in huffFastLoop.
data InflateScratch s = InflateScratch
  { scClLens     :: !(STUArray s Int Int)    -- 19
  , scClTable    :: !(STUArray s Int Word32) -- 128 (7-bit)
  , scClCounts   :: !(STUArray s Int Int)    -- 16
  , scClSyms     :: !(STUArray s Int Int)    -- 19
  , scAllLens    :: !(STUArray s Int Int)    -- 464
  , scOffs       :: !(STUArray s Int Int)    -- 16
  , scTable      :: !(STUArray s Int Int)    -- 3072 (lit 0..2047 + dist 2048..3071)
  , scLitCounts  :: !(STUArray s Int Int)    -- 16
  , scLitSyms    :: !(STUArray s Int Int)    -- 288
  , scDistCounts :: !(STUArray s Int Int)    -- 16
  , scDistSyms   :: !(STUArray s Int Int)    -- 464
  , scShortLits  :: !(STUArray s Int Int)    -- 256 (packed rev<<16 | sym<<8 | len)
  }

newInflateScratch :: ST s (InflateScratch s)
newInflateScratch = InflateScratch
  <$> newArray (0, 18) 0
  <*> newArray (0, 127) 0
  <*> newArray (0, 15) 0
  <*> newArray (0, 18) 0
  <*> newArray (0, 463) 0
  <*> newArray (0, 15) 0
  <*> newArray (0, litTblCap + distTblCap - 1) 0
  <*> newArray (0, 15) 0
  <*> newArray (0, 287) 0
  <*> newArray (0, 15) 0
  <*> newArray (0, 463) 0
  <*> newArray (0, 255) 0

-- Build the 7-bit code-length Huffman table into reusable STUArrays.
{-# NOINLINE buildClTableST #-}
buildClTableST :: STUArray s Int Int -> STUArray s Int Int -> STUArray s Int Int
               -> STUArray s Int Int -> STUArray s Int Word32 -> ST s Int
buildClTableST !lens !counts !offs !syms !table = do
  zeroIntArr counts 16
  let countLoop !i !ml
        | i >= 19 = pure ml
        | otherwise = do
            !l <- unsafeRead lens i
            if l > 0
              then do
                !c <- unsafeRead counts l
                unsafeWrite counts l (c + 1)
                countLoop (i + 1) (if l > ml then l else ml)
              else countLoop (i + 1) ml
  !maxLen <- countLoop 0 0
  let offLoop !l !acc !slots !ok
        | l > maxLen = pure ok
        | otherwise = do
            !c <- unsafeRead counts l
            unsafeWrite offs l acc
            let !nextSlots = slots * 2 - c
            offLoop (l + 1) (acc + c) nextSlots (ok && nextSlots >= 0)
  !valid <- offLoop 1 0 1 True
  let sortLoop !i
        | i >= 19 = pure ()
        | otherwise = do
            !l <- unsafeRead lens i
            when (l > 0) $ do
              !k <- unsafeRead offs l
              unsafeWrite syms k i
              unsafeWrite offs l (k + 1)
            sortLoop (i + 1)
  sortLoop 0
  zeroWord32Arr table 128
  when valid $ do
    let !lim = min 7 maxLen
        fillLen !len !first !index
          | len > lim = pure ()
          | otherwise = do
              !cnt <- unsafeRead counts len
              let !step  = 1 `unsafeShiftL` len
                  !shift = 10 - len
                  fillSyms !j
                    | j >= cnt = pure ()
                    | otherwise = do
                        !sym <- unsafeRead syms (index + j)
                        let !rev   = rev10 `unsafeAt` ((first + j) `unsafeShiftL` shift)
                            !entry = fromIntegral ((sym `unsafeShiftL` 4) .|. len) :: Word32
                            fillSlots !idx
                              | idx >= 128 = pure ()
                              | otherwise  = unsafeWrite table idx entry >> fillSlots (idx + step)
                        fillSlots rev
                        fillSyms (j + 1)
              fillSyms 0
              fillLen (len + 1) ((first + cnt) `unsafeShiftL` 1) (index + cnt)
    fillLen 1 0 0
  pure ((maxLen `unsafeShiftL` 1) .|. (if valid then 1 else 0))

-- Combined 2-level lit/len table entry encoding (signed Int):
--   e < 0                         : LITERAL  (minBound | bytes<<8 | cnt<<4 | totLen)
--   0 < e < 0x40000000            : LENGTH   (lenBase<<16 | extraMask<<8 | (len+extraBits)<<4 | len)
--   0x40000000 <= e < 0x50000000  : EOB      (0x40000000 | len)
--   0x50000000 <= e < 0x60000000  : SUBTABLE (0x50000000 | subMask<<16 | subOff<<4 | litRootBits)
--   e >= 0x60000000               : INVALID  (0x60000000 | len)
--   e == 0                        : slow canonical walk fallback (incomplete/oversubscribed)
litFlag :: Int
litFlag = minBound

{-# INLINE mkLitEntry #-}
mkLitEntry :: Int -> Int -> Int
mkLitEntry !sym !len
  | sym < 256  = litFlag .|. (sym `unsafeShiftL` 8) .|. (1 `unsafeShiftL` 4) .|. len
  | sym == 256 = 0x40000000 .|. len
  | otherwise  =
      let !li = sym - 257
      in if li < nLenCodes
           then
             let !lb = lenBase `unsafeAt` li
                 !ex = lenExtra `unsafeAt` li
                 !em = (1 `unsafeShiftL` ex) - 1
             in (lb `unsafeShiftL` 16) .|. (em `unsafeShiftL` 8) .|. ((len + ex) `unsafeShiftL` 4) .|. len
           else 0x60000000 .|. len

-- Combined 2-level distance table entry encoding (signed Int):
--   de > 0                        : VALID    (dmask<<32 [64-bit] | distBase<<16 | (len+extraBits)<<4 | len)
--   de < 0 && de .&. 0x40000000==0: SUBTABLE (minBound | subMask<<16 | subOff<<4 | distRootBits)
--   de < 0 && de .&. 0x40000000/=0: INVALID  (minBound | 0x40000000 | len)
--   de == 0                       : slow canonical walk fallback
{-# INLINE mkDistEntry #-}
mkDistEntry :: Int -> Int -> Int
mkDistEntry !sym !len
  | sym < nDistCodes =
      let !db = distBase `unsafeAt` sym
          !ex = distExtra `unsafeAt` sym
          !dm = (1 `unsafeShiftL` ex) - 1
          !hi = if finiteBitSize (0 :: Int) == 64 then dm `unsafeShiftL` 32 else 0
      in hi .|. (db `unsafeShiftL` 16) .|. ((len + ex) `unsafeShiftL` 4) .|. len
  | otherwise = litFlag .|. 0x40000000 .|. len

{-# NOINLINE buildLitTableST #-}
buildLitTableST :: STUArray s Int Int -> Int -> Int
                -> STUArray s Int Int -> STUArray s Int Int -> STUArray s Int Int
                -> STUArray s Int Int -> STUArray s Int Int -> ST s Int
buildLitTableST !lens !off !n !counts !offs !syms !table !shortLits = do
  zeroIntArr counts 16
  let countLoop !i !ml
        | i >= n = pure ml
        | otherwise = do
            !l <- unsafeRead lens (off + i)
            if l > 0
              then do
                !c <- unsafeRead counts l
                unsafeWrite counts l (c + 1)
                countLoop (i + 1) (if l > ml then l else ml)
              else countLoop (i + 1) ml
  !maxLen <- countLoop 0 0
  let offLoop !l !acc !slots !ok
        | l > maxLen = pure (ok, slots)
        | otherwise = do
            !c <- unsafeRead counts l
            unsafeWrite offs l acc
            let !nextSlots = slots * 2 - c
            offLoop (l + 1) (acc + c) nextSlots (ok && nextSlots >= 0)
  (!valid, !finalSlots) <- offLoop 1 0 1 True
  let sortLoop !i
        | i >= n = pure ()
        | otherwise = do
            !l <- unsafeRead lens (off + i)
            when (l > 0) $ do
              !k <- unsafeRead offs l
              unsafeWrite syms k i
              unsafeWrite offs l (k + 1)
            sortLoop (i + 1)
  sortLoop 0
  let !rootSize = 1 `unsafeShiftL` litRootBits
      !complete = valid && maxLen > 0 && finalSlots == 0
  when (not complete) $ zeroIntArr table rootSize
  when valid $ do
    let !lim = min litRootBits maxLen
        fillRoot !len !first !index !nShort !minLit
          | len > lim = pure (first, index, nShort, minLit)
          | otherwise = do
              !cnt <- unsafeRead counts len
              let !step  = 1 `unsafeShiftL` len
                  !shift = 10 - len
                  fillSyms !j !ns !ml
                    | j >= cnt = pure (ns, ml)
                    | otherwise = do
                        !sym <- unsafeRead syms (index + j)
                        let !rev   = rev10 `unsafeAt` ((first + j) `unsafeShiftL` shift)
                            !entry = mkLitEntry sym len
                            fillSlots !idx
                              | idx >= rootSize = pure ()
                              | otherwise       = unsafeWrite table idx entry >> fillSlots (idx + step)
                        fillSlots rev
                        if sym < 256 && len + ml <= litRootBits
                          then do
                            let !ml' = if ns == 0 then len else ml
                            if len + ml' <= litRootBits
                              then do
                                unsafeWrite shortLits ns ((rev `unsafeShiftL` 16) .|. (sym `unsafeShiftL` 8) .|. len)
                                fillSyms (j + 1) (ns + 1) ml'
                              else fillSyms (j + 1) ns ml'
                          else fillSyms (j + 1) ns ml
              (!ns', !ml') <- fillSyms 0 nShort minLit
              fillRoot (len + 1) ((first + cnt) `unsafeShiftL` 1) (index + cnt) ns' ml'
    (!firstLong, !idxLong, !nShort, !minLit) <- fillRoot 1 0 0 0 1
    -- Symbol-driven multi-literal packing (up to 4 short literals whose combined
    -- length <= litRootBits): iterates only over short literals sorted by length,
    -- avoiding all root-table reads and branch mispredictions.
    when (finiteBitSize (0 :: Int) == 64 && nShort > 0 && minLit + minLit <= litRootBits) $ do
      let fillStride !ent !step !idx
            | idx >= rootSize = pure ()
            | otherwise       = unsafeWrite table idx ent >> fillStride ent step (idx + step)
          pack4 !rev3 !b3 !l3 !k4
            | k4 >= nShort = pure ()
            | otherwise = do
                !p4 <- unsafeRead shortLits k4
                let !len4 = p4 .&. 15
                    !l4   = l3 + len4
                when (l4 <= litRootBits) $ do
                  let !s4   = (p4 `unsafeShiftR` 8) .&. 255
                      !rev4 = rev3 .|. ((p4 `unsafeShiftR` 16) `unsafeShiftL` l3)
                      !b4   = b3 .|. (s4 `unsafeShiftL` 24)
                      !ent4 = litFlag .|. (b4 `unsafeShiftL` 8) .|. (4 `unsafeShiftL` 4) .|. l4
                  fillStride ent4 (1 `unsafeShiftL` l4) rev4
                  pack4 rev3 b3 l3 (k4 + 1)
          pack3 !rev2 !b2 !l2 !k3
            | k3 >= nShort = pure ()
            | otherwise = do
                !p3 <- unsafeRead shortLits k3
                let !len3 = p3 .&. 15
                    !l3   = l2 + len3
                when (l3 <= litRootBits) $ do
                  let !s3   = (p3 `unsafeShiftR` 8) .&. 255
                      !rev3 = rev2 .|. ((p3 `unsafeShiftR` 16) `unsafeShiftL` l2)
                      !b3   = b2 .|. (s3 `unsafeShiftL` 16)
                      !ent3 = litFlag .|. (b3 `unsafeShiftL` 8) .|. (3 `unsafeShiftL` 4) .|. l3
                  fillStride ent3 (1 `unsafeShiftL` l3) rev3
                  when (l3 + minLit <= litRootBits) $ pack4 rev3 b3 l3 0
                  pack3 rev2 b2 l2 (k3 + 1)
          pack2 !rev1 !s1 !l1 !k2
            | k2 >= nShort = pure ()
            | otherwise = do
                !p2 <- unsafeRead shortLits k2
                let !len2 = p2 .&. 15
                    !l2   = l1 + len2
                when (l2 <= litRootBits) $ do
                  let !s2   = (p2 `unsafeShiftR` 8) .&. 255
                      !rev2 = rev1 .|. ((p2 `unsafeShiftR` 16) `unsafeShiftL` l1)
                      !b2   = s1 .|. (s2 `unsafeShiftL` 8)
                      !ent2 = litFlag .|. (b2 `unsafeShiftL` 8) .|. (2 `unsafeShiftL` 4) .|. l2
                  fillStride ent2 (1 `unsafeShiftL` l2) rev2
                  when (l2 + minLit <= litRootBits) $ pack3 rev2 b2 l2 0
                  pack2 rev1 s1 l1 (k2 + 1)
          pack1 !k1
            | k1 >= nShort = pure ()
            | otherwise = do
                !p1 <- unsafeRead shortLits k1
                let !l1   = p1 .&. 15
                    !s1   = (p1 `unsafeShiftR` 8) .&. 255
                    !rev1 = p1 `unsafeShiftR` 16
                pack2 rev1 s1 l1 0
                pack1 (k1 + 1)
      pack1 0
    when (maxLen > litRootBits) $ do
      -- Pass A: record max subBits for each root prefix needing a subtable.
      let markSub !len !first
            | len > maxLen = pure ()
            | otherwise = do
                !cnt <- unsafeRead counts len
                let !subBits = len - litRootBits
                    markJ !j
                      | j >= cnt = pure ()
                      | otherwise = do
                          let !rIdx = rev10 `unsafeAt` ((first + j) `unsafeShiftR` subBits)
                          unsafeWrite table rIdx subBits
                          markJ (j + 1)
                markJ 0
                markSub (len + 1) ((first + cnt) `unsafeShiftL` 1)
      markSub (litRootBits + 1) firstLong
      -- Pass B: allocate subtables and fill their entries.
      let fillSub !len !first !index !nextSub
            | len > maxLen = pure ()
            | otherwise = do
                !cnt <- unsafeRead counts len
                let !subLen  = len - litRootBits
                    !subStep = 1 `unsafeShiftL` subLen
                    !subMaskC = subStep - 1
                    !subShift = 10 - subLen
                    fillJ !j !ns
                      | j >= cnt = pure ns
                      | otherwise = do
                          let !code = first + j
                              !rIdx = rev10 `unsafeAt` (code `unsafeShiftR` subLen)
                          !rEnt <- unsafeRead table rIdx
                          (!subOff, !subSize, !ns') <-
                            if rEnt > 0 && rEnt < 16
                              then
                                let !sz = 1 `unsafeShiftL` rEnt
                                in if ns + sz <= litTblCap
                                     then do
                                       zeroIntRange table ns sz
                                       let !ptrEnt = 0x50000000 .|. ((sz - 1) `unsafeShiftL` 16) .|. (ns `unsafeShiftL` 4) .|. litRootBits
                                       unsafeWrite table rIdx ptrEnt
                                       pure (ns, sz, ns + sz)
                                     else do
                                       unsafeWrite table rIdx 0
                                       pure (0, 0, ns)
                              else if rEnt >= 0x50000000 && rEnt < 0x60000000
                                then pure ((rEnt `unsafeShiftR` 4) .&. 0xFFF, ((rEnt `unsafeShiftR` 16) .&. 31) + 1, ns)
                                else pure (0, 0, ns)
                          when (subSize > 0) $ do
                            !sym <- unsafeRead syms (index + j)
                            let !subRev = rev10 `unsafeAt` ((code .&. subMaskC) `unsafeShiftL` subShift)
                                !entry  = mkLitEntry sym subLen
                                !subEnd = subOff + subSize
                                fillS !idx
                                  | idx >= subEnd = pure ()
                                  | otherwise     = unsafeWrite table idx entry >> fillS (idx + subStep)
                            fillS (subOff + subRev)
                          fillJ (j + 1) ns'
                !nextSub' <- fillJ 0 nextSub
                fillSub (len + 1) ((first + cnt) `unsafeShiftL` 1) (index + cnt) nextSub'
      fillSub (litRootBits + 1) firstLong idxLong rootSize
  pure maxLen

{-# NOINLINE buildDistTableST #-}
buildDistTableST :: STUArray s Int Int -> Int -> Int
                 -> STUArray s Int Int -> STUArray s Int Int -> STUArray s Int Int
                 -> STUArray s Int Int -> ST s Int
buildDistTableST !lens !off !n !counts !offs !syms !table = do
  zeroIntArr counts 16
  let countLoop !i !ml
        | i >= n = pure ml
        | otherwise = do
            !l <- unsafeRead lens (off + i)
            if l > 0
              then do
                !c <- unsafeRead counts l
                unsafeWrite counts l (c + 1)
                countLoop (i + 1) (if l > ml then l else ml)
              else countLoop (i + 1) ml
  !maxLen <- countLoop 0 0
  let offLoop !l !acc !slots !ok
        | l > maxLen = pure (ok, slots)
        | otherwise = do
            !c <- unsafeRead counts l
            unsafeWrite offs l acc
            let !nextSlots = slots * 2 - c
            offLoop (l + 1) (acc + c) nextSlots (ok && nextSlots >= 0)
  (!valid, !finalSlots) <- offLoop 1 0 1 True
  let sortLoop !i
        | i >= n = pure ()
        | otherwise = do
            !l <- unsafeRead lens (off + i)
            when (l > 0) $ do
              !k <- unsafeRead offs l
              unsafeWrite syms k i
              unsafeWrite offs l (k + 1)
            sortLoop (i + 1)
  sortLoop 0
  let !rootSize = 1 `unsafeShiftL` distRootBits
  when (not (valid && maxLen > 0 && finalSlots == 0)) $ zeroIntRange table distTblOff rootSize
  when valid $ do
    let !lim = min distRootBits maxLen
        fillRoot !len !first !index
          | len > lim = pure (first, index)
          | otherwise = do
              !cnt <- unsafeRead counts len
              let !step  = 1 `unsafeShiftL` len
                  !shift = 10 - len
                  fillSyms !j
                    | j >= cnt = pure ()
                    | otherwise = do
                        !sym <- unsafeRead syms (index + j)
                        let !rev   = rev10 `unsafeAt` ((first + j) `unsafeShiftL` shift)
                            !entry = mkDistEntry sym len
                            fillSlots !idx
                              | idx >= rootSize = pure ()
                              | otherwise       = unsafeWrite table (distTblOff + idx) entry >> fillSlots (idx + step)
                        fillSlots rev
                        fillSyms (j + 1)
              fillSyms 0
              fillRoot (len + 1) ((first + cnt) `unsafeShiftL` 1) (index + cnt)
    (!firstLong, !idxLong) <- fillRoot 1 0 0
    when (maxLen > distRootBits) $ do
      let !rootShift = 10 - distRootBits
          markSub !len !first
            | len > maxLen = pure ()
            | otherwise = do
                !cnt <- unsafeRead counts len
                let !subBits = len - distRootBits
                    markJ !j
                      | j >= cnt = pure ()
                      | otherwise = do
                          let !rIdx = rev10 `unsafeAt` (((first + j) `unsafeShiftR` subBits) `unsafeShiftL` rootShift)
                          unsafeWrite table (distTblOff + rIdx) subBits
                          markJ (j + 1)
                markJ 0
                markSub (len + 1) ((first + cnt) `unsafeShiftL` 1)
      markSub (distRootBits + 1) firstLong
      let fillSub !len !first !index !nextSub
            | len > maxLen = pure ()
            | otherwise = do
                !cnt <- unsafeRead counts len
                let !subLen   = len - distRootBits
                    !subStep  = 1 `unsafeShiftL` subLen
                    !subMaskC = subStep - 1
                    !subShift = 10 - subLen
                    fillJ !j !ns
                      | j >= cnt = pure ns
                      | otherwise = do
                          let !code = first + j
                              !rIdx = distTblOff + (rev10 `unsafeAt` ((code `unsafeShiftR` subLen) `unsafeShiftL` rootShift))
                          !rEnt <- unsafeRead table rIdx
                          (!subOff, !subSize, !ns') <-
                            if rEnt > 0 && rEnt < 16
                              then
                                let !sz = 1 `unsafeShiftL` rEnt
                                in if ns + sz <= distTblCap
                                     then do
                                       let !absOff = distTblOff + ns
                                       zeroIntRange table absOff sz
                                       let !ptrEnt = litFlag .|. ((sz - 1) `unsafeShiftL` 16) .|. (absOff `unsafeShiftL` 4) .|. distRootBits
                                       unsafeWrite table rIdx ptrEnt
                                       pure (absOff, sz, ns + sz)
                                     else do
                                       unsafeWrite table rIdx 0
                                       pure (0, 0, ns)
                              else if rEnt < 0 && (rEnt .&. 0x40000000) == 0
                                then pure ((rEnt `unsafeShiftR` 4) .&. 0xFFF, ((rEnt `unsafeShiftR` 16) .&. 63) + 1, ns)
                                else pure (0, 0, ns)
                          when (subSize > 0) $ do
                            !sym <- unsafeRead syms (index + j)
                            let !subRev = rev10 `unsafeAt` ((code .&. subMaskC) `unsafeShiftL` subShift)
                                !entry  = mkDistEntry sym subLen
                                !subEnd = subOff + subSize
                                fillS !idx
                                  | idx >= subEnd = pure ()
                                  | otherwise     = unsafeWrite table idx entry >> fillS (idx + subStep)
                            fillS (subOff + subRev)
                          fillJ (j + 1) ns'
                !nextSub' <- fillJ 0 nextSub
                fillSub (len + 1) ((first + cnt) `unsafeShiftL` 1) (index + cnt) nextSub'
      fillSub (distRootBits + 1) firstLong idxLong rootSize
  pure maxLen

-- Canonical walk over the 64-bit bit buffer using reusable STUArrays.
-- OPAQUE keeps the slow path out of line and prevents spilling its arrays.
{-# OPAQUE decodeSymSlowM #-}
decodeSymSlowM :: Word64# -> Int# -> MutableByteArray# s -> MutableByteArray# s -> Int#
               -> State# s -> (# State# s, Int#, Word64#, Int# #)
decodeSymSlowM bb0# bc0# cnts# syms# maxLen# s0# = go 1 0 0 0 (W64# bb0#) (I# bc0#) s0#
  where
    !maxLen = I# maxLen#
    go !len !code !first !index !bb !bc s#
      | len > maxLen =
          case (bb, bc) of (W64# b#, I# c#) -> (# s#, -1#, b#, c# #)
      | otherwise =
          let !b     = fromIntegral (bb .&. 1) :: Int
              !bb'   = bb `unsafeShiftR` 1
              !bc'   = bc - 1
              !code1 = code .|. b
              !(I# len#) = len
          in case readIntArray# cnts# len# s# of
               (# s1#, cnt# #) ->
                 let !cnt = I# cnt#
                 in if code1 - first < cnt
                      then
                        let !(I# idx#) = index + (code1 - first)
                        in case readIntArray# syms# idx# s1# of
                             (# s2#, sym# #) ->
                               case (bb', bc') of (W64# b#, I# c#) -> (# s2#, sym#, b#, c# #)
                      else go (len + 1) (code1 `unsafeShiftL` 1) ((first + cnt) `unsafeShiftL` 1) (index + cnt) bb' bc' s1#

data BlockRes = BlockRes !Int !Int !Word64 !Int !Int !(Maybe String)
data FastRes  = FastRes  !Int Addr# !Word64 !Int !Int

{-# INLINE le64Ptr #-}
le64Ptr :: Addr# -> Word64
le64Ptr ptr# =
  let !w = W64# (indexWord64OffAddr# ptr# 0#)
  in case targetByteOrder of
       LittleEndian -> w
       BigEndian    -> byteSwap64 w

{-# INLINE readU8Raw #-}
readU8Raw :: MutableByteArray# s -> Int -> ST s Word8
readU8Raw mba# (I# off#) = ST $ \s# ->
  case readWord8Array# mba# off# s# of (# s1#, w# #) -> (# s1#, W8# w# #)

{-# INLINE writeU8Raw #-}
writeU8Raw :: MutableByteArray# s -> Int -> Word8 -> ST s ()
writeU8Raw mba# (I# off#) (W8# w#) = ST $ \s# ->
  case writeWord8Array# mba# off# w# s# of s1# -> (# s1#, () #)

{-# INLINE readU16Raw #-}
readU16Raw :: MutableByteArray# s -> Int -> ST s Word16
readU16Raw mba# (I# off#) = ST $ \s# ->
  case readWord8ArrayAsWord16# mba# off# s# of (# s1#, w# #) -> (# s1#, W16# w# #)

{-# INLINE readU32Raw #-}
readU32Raw :: MutableByteArray# s -> Int -> ST s Word32
readU32Raw mba# (I# off#) = ST $ \s# ->
  case readWord8ArrayAsWord32# mba# off# s# of (# s1#, w# #) -> (# s1#, W32# w# #)

{-# INLINE writeU32LERaw #-}
writeU32LERaw :: MutableByteArray# s -> Int -> Int -> ST s ()
writeU32LERaw mba# (I# off#) !v =
  let !w32 = case targetByteOrder of
               LittleEndian -> fromIntegral v :: Word32
               BigEndian    -> byteSwap32 (fromIntegral v)
  in case w32 of
       W32# w# -> ST $ \s# ->
         case writeWord8ArrayAsWord32# mba# off# w# s# of s1# -> (# s1#, () #)

{-# INLINE readU64Raw #-}
readU64Raw :: MutableByteArray# s -> Int -> ST s Word64
readU64Raw mba# (I# off#) = ST $ \s# ->
  case readWord8ArrayAsWord64# mba# off# s# of (# s1#, w# #) -> (# s1#, W64# w# #)

{-# INLINE writeU64Raw #-}
writeU64Raw :: MutableByteArray# s -> Int -> Word64 -> ST s ()
writeU64Raw mba# (I# off#) (W64# w#) = ST $ \s# ->
  case writeWord8ArrayAsWord64# mba# off# w# s# of s1# -> (# s1#, () #)

-- Leaf fast loop for combined 2-level Huffman tables.
-- Takes raw MutableByteArray# for output and makes ZERO out-of-line calls
-- (including inline 64-bit word match copies), so GHC keeps the entire loop
-- register-resident with no STG stack spills. Exits with a status code:
--   0 = EOB
--   1 = boundary or slow path needed (caller handles one symbol or tail)
--   2 = invalid length code
--   3 = invalid distance code
--   >= 4 = slow distance walk needed with mlen = st - 4
{-# NOINLINE huffFastLoop #-}
huffFastLoop
  :: Addr# -> Addr#
  -> MutableByteArray# s -> Int -> Int
  -> STUArray s Int Int
  -> Word64 -> Int -> Int
  -> ST s FastRes
huffFastLoop cur0# endPtr# arr# !base !fastOutEnd !tbl !bb0 !bc0 !o0 =
  loop cur0# bb0 bc0 o0
  where
    ret !st cur# !bb !bc !o = pure (FastRes st cur# bb bc o)
    loop cur# !bb !bc !o
      | o >= fastOutEnd || isTrue# (gtAddr# cur# endPtr#) = ret 1 cur# bb bc o
      | otherwise = do
          let !w      = le64Ptr cur#
              !bb1    = bb .|. (w `unsafeShiftL` bc)
              !(I# adv#) = (63 - bc) `unsafeShiftR` 3
              cur1#   = plusAddr# cur# adv#
              !bc1    = bc .|. 56
          !e <- unsafeRead tbl (fromIntegral bb1 .&. litRootMask)
          if e < 0
            then do
              if finiteBitSize (0 :: Int) == 64
                then writeU32LERaw arr# o (e `unsafeShiftR` 8)
                else writeU8Raw arr# o (fromIntegral (e `unsafeShiftR` 8))
              let !len1 = e .&. 15
                  !cnt1 = (e `unsafeShiftR` 4) .&. 7
                  !bb2  = bb1 `unsafeShiftR` len1
                  !bc2  = bc1 - len1
                  !o2   = o + cnt1
              !e2 <- unsafeRead tbl (fromIntegral bb2 .&. litRootMask)
              if e2 < 0
                then do
                  if finiteBitSize (0 :: Int) == 64
                    then writeU32LERaw arr# o2 (e2 `unsafeShiftR` 8)
                    else writeU8Raw arr# o2 (fromIntegral (e2 `unsafeShiftR` 8))
                  let !len2 = e2 .&. 15
                      !cnt2 = (e2 `unsafeShiftR` 4) .&. 7
                  loop cur1# (bb2 `unsafeShiftR` len2) (bc2 - len2) (o2 + cnt2)
                else if e2 > 0 && e2 < 0x40000000
                  then
                    let !llen  = e2 .&. 15
                        !ltot  = (e2 `unsafeShiftR` 4) .&. 15
                        !lmask = (e2 `unsafeShiftR` 8) .&. 31
                        !mlen  = (e2 `unsafeShiftR` 16) + (fromIntegral (bb2 `unsafeShiftR` llen) .&. lmask)
                    in decodeDist cur1# (bb2 `unsafeShiftR` ltot) (bc2 - ltot) o2 mlen
                  else loop cur1# bb2 bc2 o2
            else if e > 0 && e < 0x40000000
              then
                let !llen  = e .&. 15
                    !ltot  = (e `unsafeShiftR` 4) .&. 15
                    !lmask = (e `unsafeShiftR` 8) .&. 31
                    !mlen  = (e `unsafeShiftR` 16) + (fromIntegral (bb1 `unsafeShiftR` llen) .&. lmask)
                in decodeDist cur1# (bb1 `unsafeShiftR` ltot) (bc1 - ltot) o mlen
              else if e >= 0x40000000
                then if e < 0x50000000
                  then let !len = e .&. 15 in ret 0 cur1# (bb1 `unsafeShiftR` len) (bc1 - len) o
                  else if e < 0x60000000
                    then do
                      let !bb2    = bb1 `unsafeShiftR` litRootBits
                          !bc2    = bc1 - litRootBits
                          !subOff = (e `unsafeShiftR` 4) .&. 0xFFF
                          !subMsk = (e `unsafeShiftR` 16) .&. 31
                      !se <- unsafeRead tbl (subOff + (fromIntegral bb2 .&. subMsk))
                      if se < 0
                        then do
                          writeU8Raw arr# o (fromIntegral (se `unsafeShiftR` 8))
                          let !len = se .&. 15
                          loop cur1# (bb2 `unsafeShiftR` len) (bc2 - len) (o + 1)
                        else if se > 0 && se < 0x40000000
                          then
                            let !llen  = se .&. 15
                                !ltot  = (se `unsafeShiftR` 4) .&. 15
                                !lmask = (se `unsafeShiftR` 8) .&. 31
                                !mlen  = (se `unsafeShiftR` 16) + (fromIntegral (bb2 `unsafeShiftR` llen) .&. lmask)
                            in decodeDist cur1# (bb2 `unsafeShiftR` ltot) (bc2 - ltot) o mlen
                          else if se >= 0x40000000
                            then if se < 0x50000000
                              then let !len = se .&. 15 in ret 0 cur1# (bb2 `unsafeShiftR` len) (bc2 - len) o
                              else let !len = se .&. 15 in ret 2 cur1# (bb2 `unsafeShiftR` len) (bc2 - len) o
                            else ret 1 cur1# bb1 bc1 o
                    else let !len = e .&. 15 in ret 2 cur1# (bb1 `unsafeShiftR` len) (bc1 - len) o
                else ret 1 cur1# bb1 bc1 o
    decodeDist cur# !bb !bc !o !mlen = do
      !de <- unsafeRead tbl (distTblOff + (fromIntegral bb .&. distRootMask))
      if de > 0
        then
          let !dlen  = de .&. 15
              !dtot  = (de `unsafeShiftR` 4) .&. 31
              !dmask = if finiteBitSize (0 :: Int) == 64 then de `unsafeShiftR` 32 else (1 `unsafeShiftL` (dtot - dlen)) - 1
              !d     = ((de `unsafeShiftR` 16) .&. 0x7FFF) + (fromIntegral (bb `unsafeShiftR` dlen) .&. dmask)
          in doCopy cur# (bb `unsafeShiftR` dtot) (bc - dtot) o mlen d
        else if de < 0
          then if (de .&. 0x40000000) == 0
            then do
              let !bb1    = bb `unsafeShiftR` distRootBits
                  !bc1    = bc - distRootBits
                  !subOff = (de `unsafeShiftR` 4) .&. 0xFFF
                  !subMsk = (de `unsafeShiftR` 16) .&. 63
              !sde <- unsafeRead tbl (subOff + (fromIntegral bb1 .&. subMsk))
              if sde > 0
                then
                  let !dlen  = sde .&. 15
                      !dtot  = (sde `unsafeShiftR` 4) .&. 31
                      !dmask = if finiteBitSize (0 :: Int) == 64 then sde `unsafeShiftR` 32 else (1 `unsafeShiftL` (dtot - dlen)) - 1
                      !d     = ((sde `unsafeShiftR` 16) .&. 0x7FFF) + (fromIntegral (bb1 `unsafeShiftR` dlen) .&. dmask)
                  in doCopy cur# (bb1 `unsafeShiftR` dtot) (bc1 - dtot) o mlen d
                else if sde < 0
                  then let !dlen = sde .&. 15 in ret 3 cur# (bb1 `unsafeShiftR` dlen) (bc1 - dlen) o
                  else ret (4 + mlen) cur# bb bc o
            else let !dlen = de .&. 15 in ret 3 cur# (bb `unsafeShiftR` dlen) (bc - dlen) o
          else ret (4 + mlen) cur# bb bc o
    -- Word-sized (64-bit) sloppy match copies; safe because fastOutEnd = limit - 270
    -- guarantees o + mlen + 7 < limit. Any trailing sloppy bytes on truncated/corrupt
    -- streams are zeroed at inflateFixed exit.
    copy8 cur# !bb !bc !end !src !dst
      | dst >= end = loop cur# bb bc end
      | otherwise = do
          !w <- readU64Raw arr# src
          writeU64Raw arr# dst w
          copy8 cur# bb bc end (src + 8) (dst + 8)
    fill8 cur# !bb !bc !end !dst !w
      | dst >= end = loop cur# bb bc end
      | otherwise = do
          writeU64Raw arr# dst w
          fill8 cur# bb bc end (dst + 8) w
    doCopy cur# !bb !bc !o !mlen !d
      | o - d < base = do
          let !end = o + mlen
              zLoop !i
                | i >= end  = loop cur# bb bc end
                | otherwise = do
                    !v <- if i - d >= base then readU8Raw arr# (i - d) else pure 0
                    writeU8Raw arr# i v
                    zLoop (i + 1)
          zLoop o
      | d >= 8 = do
          !w0 <- readU64Raw arr# (o - d)
          writeU64Raw arr# o w0
          let !end = o + mlen
          if mlen <= 8
            then loop cur# bb bc end
            else copy8 cur# bb bc end (o - d + 8) (o + 8)
      | d == 1 = do
          !b <- readU8Raw arr# (o - 1)
          fill8 cur# bb bc (o + mlen) o (fromIntegral b * (0x0101010101010101 :: Word64))
      | d == 4 = do
          !w32 <- readU32Raw arr# (o - 4)
          let !w64 = fromIntegral w32 :: Word64
          fill8 cur# bb bc (o + mlen) o (w64 .|. (w64 `unsafeShiftL` 32))
      | d == 2 = do
          !w16 <- readU16Raw arr# (o - 2)
          fill8 cur# bb bc (o + mlen) o (fromIntegral w16 * (0x0001000100010001 :: Word64))
      | otherwise = do
          let !end = o + mlen
          if mlen <= 16
            then do
              let bLoop !i
                    | i >= end  = loop cur# bb bc end
                    | otherwise = readU8Raw arr# (i - d) >>= writeU8Raw arr# i >> bLoop (i + 1)
              bLoop o
            else do
              let !d' = if d == 3 then 9 else d + d
                  !prepEnd = o + d' - d
                  prep !i
                    | i >= prepEnd = pure ()
                    | otherwise    = readU8Raw arr# (i - d) >>= writeU8Raw arr# i >> prep (i + 1)
              prep o
              copy8 cur# bb bc end (o - d) prepEnd

{-# NOINLINE huffBlockST #-}
huffBlockST :: Segs -> STUArray s Int Word8 -> Int -> Int
            -> STUArray s Int Int
            -> STUArray s Int Int -> STUArray s Int Int -> Int
            -> STUArray s Int Int -> STUArray s Int Int -> Int
            -> Int -> Int -> Word64 -> Int -> Int
            -> ST s BlockRes
huffBlockST !segs@(Segs !nSeg _ !starts !segArr _) !arr@(STUArray _ _ _ arr#) !base !limit
            !tbl
            !(STUArray _ _ _ litCnt#) !(STUArray _ _ _ litSym#) !(I# litMax#)
            !(STUArray _ _ _ distCnt#) !(STUArray _ _ _ distSym#) !(I# distMax#)
            !k0 !p0 !bb0 !bc0 !o0 =
  driver k0 p0 bb0 bc0 o0
  where
    !fastOutEnd = limit - 274
    driver !k !p !bb !bc !o
      | o >= limit = pure (BlockRes k p bb bc o Nothing)
      | o < fastOutEnd && k < nSeg
      , let !s0 = starts `unsafeAt` k
            !s1 = starts `unsafeAt` (k + 1)
      , p >= s0 && p <= s1 - 8 = do
          let !(BI.BS (ForeignPtr segAddr# segFpc) _) = segArr A.! k
              !(I# pOff#) = p - s0
              !(I# eOff#) = s1 - s0 - 8
          FastRes st cur1# bb1 bc1 o1 <-
            huffFastLoop (plusAddr# segAddr# pOff#) (plusAddr# segAddr# eOff#)
                         arr# base fastOutEnd tbl bb bc o
          unsafeIOToST (IO $ \s# -> case touch# segFpc s# of s1# -> (# s1#, () #))
          let !p1 = s0 + I# (minusAddr# cur1# segAddr#)
          case st of
            0 -> pure (BlockRes k p1 bb1 bc1 o1 Nothing)
            2 -> pure (BlockRes k p1 bb1 bc1 o1 (Just "invalid length code"))
            3 -> pure (BlockRes k p1 bb1 bc1 o1 (Just "invalid distance code"))
            1 -> slowStep k p1 bb1 bc1 o1
            _ -> walkDist k p1 bb1 bc1 o1 (st - 4)
      | otherwise = slowStep k p bb bc o
    slowStep !k !p !bb !bc !o
      | o >= limit = pure (BlockRes k p bb bc o Nothing)
      | otherwise =
          case refillSeg segs k p bb bc of
            (# k1#, p1#, bb1#, bc1# #)
              -- only a window's end leaves fewer than 56 bits: resume there
              | isTrue# (bc1# <# 56#) -> pure (BlockRes (I# k1#) (I# p1#) (W64# bb1#) (I# bc1#) o (Just needInput))
              | otherwise -> decodeLitSlow (I# k1#) (I# p1#) (W64# bb1#) (I# bc1#) o
    decodeLitSlow !k !p !bb !bc !o = do
      !e <- unsafeRead tbl (fromIntegral bb .&. litRootMask)
      if e < 0
        then do
          let !len = e .&. 15
              !cnt = (e `unsafeShiftR` 4) .&. 7
              !nwr = min cnt (limit - o)
              writeBytes !j !v
                | j >= nwr  = pure ()
                | otherwise = unsafeWrite arr (o + j) (fromIntegral v) >> writeBytes (j + 1) (v `unsafeShiftR` 8)
          writeBytes 0 (e `unsafeShiftR` 8)
          driver k p (bb `unsafeShiftR` len) (bc - len) (o + cnt)
        else if e > 0 && e < 0x40000000
          then
            let !llen  = e .&. 15
                !ltot  = (e `unsafeShiftR` 4) .&. 15
                !lmask = (e `unsafeShiftR` 8) .&. 31
                !mlen  = (e `unsafeShiftR` 16) + (fromIntegral (bb `unsafeShiftR` llen) .&. lmask)
            in decodeDistSlow k p (bb `unsafeShiftR` ltot) (bc - ltot) o mlen
          else if e >= 0x40000000
            then if e < 0x50000000
              then let !len = e .&. 15 in pure (BlockRes k p (bb `unsafeShiftR` len) (bc - len) o Nothing)
              else if e < 0x60000000
                then do
                  let !bb1    = bb `unsafeShiftR` litRootBits
                      !bc1    = bc - litRootBits
                      !subOff = (e `unsafeShiftR` 4) .&. 0xFFF
                      !subMsk = (e `unsafeShiftR` 16) .&. 31
                  !se <- unsafeRead tbl (subOff + (fromIntegral bb1 .&. subMsk))
                  if se < 0
                    then do
                      unsafeWrite arr o (fromIntegral (se `unsafeShiftR` 8))
                      let !len = se .&. 15
                      driver k p (bb1 `unsafeShiftR` len) (bc1 - len) (o + 1)
                    else if se > 0 && se < 0x40000000
                      then
                        let !llen  = se .&. 15
                            !ltot  = (se `unsafeShiftR` 4) .&. 15
                            !lmask = (se `unsafeShiftR` 8) .&. 31
                            !mlen  = (se `unsafeShiftR` 16) + (fromIntegral (bb1 `unsafeShiftR` llen) .&. lmask)
                        in decodeDistSlow k p (bb1 `unsafeShiftR` ltot) (bc1 - ltot) o mlen
                      else if se >= 0x40000000
                        then if se < 0x50000000
                          then let !len = se .&. 15 in pure (BlockRes k p (bb1 `unsafeShiftR` len) (bc1 - len) o Nothing)
                          else let !len = se .&. 15 in pure (BlockRes k p (bb1 `unsafeShiftR` len) (bc1 - len) o (Just "invalid length code"))
                        else walkLit k p bb bc o
                else let !len = e .&. 15 in pure (BlockRes k p (bb `unsafeShiftR` len) (bc - len) o (Just "invalid length code"))
            else walkLit k p bb bc o
    walkLit !k !p !bb !bc !o =
      case (bb, bc) of
        (W64# bb#, I# bc#) -> ST $ \s# ->
          case decodeSymSlowM bb# bc# litCnt# litSym# litMax# s# of
            (# s1#, sym#, bb2#, bc2# #) ->
              case handleSymWalk k p (W64# bb2#) (I# bc2#) o (I# sym#) of
                ST f -> f s1#
    handleSymWalk !k !p !bb !bc !o !sym
      | sym < 0    = pure (BlockRes k p bb bc o (Just "invalid Huffman code"))
      | sym < 256  = do
          unsafeWrite arr o (fromIntegral sym)
          driver k p bb bc (o + 1)
      | sym == 256 = pure (BlockRes k p bb bc o Nothing)
      | otherwise  =
          let !li = sym - 257
          in if li >= nLenCodes
               then pure (BlockRes k p bb bc o (Just "invalid length code"))
               else
                 let !lex   = lenExtra `unsafeAt` li
                     !extra = fromIntegral bb .&. ((1 `unsafeShiftL` lex) - 1)
                     !mlen  = lenBase `unsafeAt` li + extra
                 in decodeDistSlow k p (bb `unsafeShiftR` lex) (bc - lex) o mlen
    decodeDistSlow !k !p !bb !bc !o !mlen = do
      !de <- unsafeRead tbl (distTblOff + (fromIntegral bb .&. distRootMask))
      if de > 0
        then
          let !dlen  = de .&. 15
              !dtot  = (de `unsafeShiftR` 4) .&. 31
              !dmask = if finiteBitSize (0 :: Int) == 64 then de `unsafeShiftR` 32 else (1 `unsafeShiftL` (dtot - dlen)) - 1
              !d     = ((de `unsafeShiftR` 16) .&. 0x7FFF) + (fromIntegral (bb `unsafeShiftR` dlen) .&. dmask)
          in doCopySlow k p (bb `unsafeShiftR` dtot) (bc - dtot) o mlen d
        else if de < 0
          then if (de .&. 0x40000000) == 0
            then do
              let !bb1    = bb `unsafeShiftR` distRootBits
                  !bc1    = bc - distRootBits
                  !subOff = (de `unsafeShiftR` 4) .&. 0xFFF
                  !subMsk = (de `unsafeShiftR` 16) .&. 63
              !sde <- unsafeRead tbl (subOff + (fromIntegral bb1 .&. subMsk))
              if sde > 0
                then
                  let !dlen  = sde .&. 15
                      !dtot  = (sde `unsafeShiftR` 4) .&. 31
                      !dmask = if finiteBitSize (0 :: Int) == 64 then sde `unsafeShiftR` 32 else (1 `unsafeShiftL` (dtot - dlen)) - 1
                      !d     = ((sde `unsafeShiftR` 16) .&. 0x7FFF) + (fromIntegral (bb1 `unsafeShiftR` dlen) .&. dmask)
                  in doCopySlow k p (bb1 `unsafeShiftR` dtot) (bc1 - dtot) o mlen d
                else if sde < 0
                  then let !dlen = sde .&. 15 in pure (BlockRes k p (bb1 `unsafeShiftR` dlen) (bc1 - dlen) o (Just "invalid distance code"))
                  else walkDist k p bb bc o mlen
            else let !dlen = de .&. 15 in pure (BlockRes k p (bb `unsafeShiftR` dlen) (bc - dlen) o (Just "invalid distance code"))
          else walkDist k p bb bc o mlen
    walkDist !k !p !bb !bc !o !mlen =
      case (bb, bc) of
        (W64# bb#, I# bc#) -> ST $ \s# ->
          case decodeSymSlowM bb# bc# distCnt# distSym# distMax# s# of
            (# s1#, dsym#, bb3#, bc3# #) ->
              case handleDistWalk k p (W64# bb3#) (I# bc3#) o mlen (I# dsym#) of
                ST f -> f s1#
    handleDistWalk !k !p !bb !bc !o !mlen !dsym
      | dsym < 0 || dsym >= nDistCodes = pure (BlockRes k p bb bc o (Just "invalid distance code"))
      | otherwise =
          let !dex = distExtra `unsafeAt` dsym
              !d   = distBase `unsafeAt` dsym + (fromIntegral bb .&. ((1 `unsafeShiftL` dex) - 1))
          in doCopySlow k p (bb `unsafeShiftR` dex) (bc - dex) o mlen d
    doCopySlow !k !p !bb !bc !o !mlen !d = do
      let !end = min limit (o + mlen) - 1
      if o - d >= base
        then copyMatch arr o d (end - o + 1)
        else forLoop o end $ \i -> do
               v <- if i - d >= base then unsafeRead arr (i - d) else pure 0
               unsafeWrite arr i v
      driver k p bb bc (o + mlen)

data DynRes = DynRes !Int !Int !Int !Int !Word64 !Int

{-# NOINLINE readDynamicST #-}
readDynamicST :: Segs -> InflateScratch s -> Int -> Int -> Word64 -> Int -> ST s DynRes
readDynamicST !segs !sc !k0 !p0 !bb0 !bc0 =
  case refillSeg segs k0 p0 bb0 bc0 of
    (# k1#, p1#, bb1#, bc1# #) -> do
      let !bb1   = W64# bb1#
          !hlit  = fromIntegral (bb1 .&. 31) :: Int
          !hdist = fromIntegral ((bb1 `unsafeShiftR` 5) .&. 31) :: Int
          !hclen = fromIntegral ((bb1 `unsafeShiftR` 10) .&. 15) :: Int
          !nlit  = hlit + 257
          !ndist = hdist + 1
          !nclen = hclen + 4
          !clLens = scClLens sc
          !clTbl  = scClTable sc
          !clCnt  = scClCounts sc
          !clSym  = scClSyms sc
          !allLens = scAllLens sc
          !offs   = scOffs sc
      zeroIntArr clLens 19
      let readCL !i !k !p !bb !bc
            | i >= nclen = pure (k, p, bb, bc)
            | bc < 3 = case refillSeg segs k p bb bc of
                (# k'#, p'#, bb'#, bc'# #) -> readCL i (I# k'#) (I# p'#) (W64# bb'#) (I# bc'#)
            | otherwise = do
                let !v = fromIntegral (bb .&. 7) :: Int
                unsafeWrite clLens (clOrderU `unsafeAt` i) v
                readCL (i + 1) k p (bb `unsafeShiftR` 3) (bc - 3)
      (!k2, !p2, !bb2, !bc2) <- readCL 0 (I# k1#) (I# p1#) (bb1 `unsafeShiftR` 14) (I# bc1# - 14)
      !clMeta <- buildClTableST clLens clCnt offs clSym clTbl
      let !clMax   = clMeta `unsafeShiftR` 1
          !clValid = clMeta .&. 1
          !total   = nlit + ndist
          !(STUArray _ _ _ clCnt#) = clCnt
          !(STUArray _ _ _ clSym#) = clSym
          !(I# clMax#) = clMax
          fillVal !start !cnt !val = go 0
            where go !j | j >= cnt  = pure ()
                        | otherwise = unsafeWrite allLens (start + j) val >> go (j + 1)
          decodeLens !n !prev !k !p !bb !bc
            | n >= total = pure (n, k, p, bb, bc)
            | bc < 22 = case refillSeg segs k p bb bc of
                (# k'#, p'#, bb'#, bc'# #) -> decodeLens n prev (I# k'#) (I# p'#) (W64# bb'#) (I# bc'#)
            | clValid == 1 = do
                !e32 <- unsafeRead clTbl (fromIntegral bb .&. 127)
                let !e   = fromIntegral e32 :: Int
                    !len = e .&. 15
                if len == 0
                  then pure (n, k, p, bb `unsafeShiftR` clMax, bc - clMax)
                  else step n prev (e `unsafeShiftR` 4) k p (bb `unsafeShiftR` len) (bc - len)
            | otherwise = case (bb, bc) of
                (W64# bb#, I# bc#) -> ST $ \s# ->
                  case decodeSymSlowM bb# bc# clCnt# clSym# clMax# s# of
                    (# s1#, sSym#, bb'#, bc'# #) ->
                      case step n prev (I# sSym#) k p (W64# bb'#) (I# bc'#) of
                        ST f -> f s1#
          step !n !prev !s !k !p !bb !bc
            | s < 0  = pure (n, k, p, bb, bc)
            | s < 16 = do
                unsafeWrite allLens n s
                decodeLens (n + 1) s k p bb bc
            | s == 16 = do
                let !cnt = fromIntegral (bb .&. 3) + 3
                fillVal n cnt prev
                decodeLens (n + cnt) prev k p (bb `unsafeShiftR` 2) (bc - 2)
            | s == 17 = do
                let !cnt = fromIntegral (bb .&. 7) + 3
                fillVal n cnt 0
                decodeLens (n + cnt) 0 k p (bb `unsafeShiftR` 3) (bc - 3)
            | otherwise = do
                let !cnt = fromIntegral (bb .&. 127) + 11
                fillVal n cnt 0
                decodeLens (n + cnt) 0 k p (bb `unsafeShiftR` 7) (bc - 7)
      (!actualTotal, !k3, !p3, !bb3, !bc3) <- decodeLens 0 0 k2 p2 bb2 bc2
      !litMax  <- buildLitTableST allLens 0 (min nlit actualTotal)
                    (scLitCounts sc) offs (scLitSyms sc) (scTable sc) (scShortLits sc)
      !distMax <- buildDistTableST allLens nlit (max 0 (actualTotal - nlit))
                    (scDistCounts sc) offs (scDistSyms sc) (scTable sc)
      pure (DynRes litMax distMax k3 p3 bb3 bc3)

{-# NOINLINE inflateFixed #-}
inflateFixed :: Int -> Segs -> [BS.ByteString] -> Int -> STUArray s Int Word8 -> Int -> Int
             -> ST s (Maybe String)
inflateFixed !win !segs0 rest0 !startByte arr !base !size = do
  sc <- newInflateScratch
  let !limit   = base + size
      finish !o !err = do
        when (o < limit) $ do
          let !zEnd = min limit (o + 16)
              zeroTail !i
                | i >= zEnd = pure ()
                | otherwise = unsafeWrite arr i 0 >> zeroTail (i + 1)
          zeroTail o
        pure err
      -- A window onto a longer stream (only 'inflateIntoSegs' makes one)
      -- is moved on at a block boundary once less than half of it lies
      -- ahead; until the stream's true end is in it, its end is never
      -- taken for the end of the data.
      slide !segs rest !lo !hi = advanceWindow win segs rest lo hi
      blockLoop segs@(Segs _ _ _ _ more) rest !k !p !bb !bc !o
        | more && segTotal segs - p < win `unsafeShiftR` 1 =
            let (segs', rest') = slide segs rest (p - 16) (p + win)
            in blockLoop segs' rest' (seekSeg segs' p) p bb bc o
        | o >= limit || p * 8 - bc >= 8 * segTotal segs = finish o Nothing
        | bc < 3 = case refillSeg segs k p bb bc of
            (# k'#, p'#, bb'#, bc'# #) -> blockLoop segs rest (I# k'#) (I# p'#) (W64# bb'#) (I# bc'#) o
        | otherwise =
            let !bfinal = fromIntegral (bb .&. 1) :: Int
                !btype  = fromIntegral ((bb `unsafeShiftR` 1) .&. 3) :: Int
                !bb1    = bb `unsafeShiftR` 3
                !bc1    = bc - 3
            in case btype of
                 0 -> do
                   let !curBitPos   = p * 8 - bc1
                       !alignedByte = ((curBitPos + 7) .&. complement 7) `unsafeShiftR` 3
                       !len         = segByteAt segs alignedByte .|. (segByteAt segs (alignedByte + 1) `unsafeShiftL` 8)
                       !srcStart    = alignedByte + 4
                       !toCopy      = min len (limit - o)
                       -- the whole payload in the window before copying it
                       (segsS, restS)
                         | more && srcStart + toCopy > segTotal segs =
                             slide segs rest (alignedByte - 16) (srcStart + toCopy + win)
                         | otherwise = (segs, rest)
                   copyStoredSegs segsS srcStart toCopy arr o
                   if bfinal == 1 then finish (o + toCopy) Nothing
                   else let !nextP = srcStart + len
                        in blockLoop segsS restS (seekSeg segsS nextP) nextP 0 0 (o + len)
                 1 -> do
                   forLoop 0 287 $ \i -> unsafeWrite (scAllLens sc) i (fixedLitLensU `unsafeAt` i)
                   forLoop 0 29  $ \i -> unsafeWrite (scAllLens sc) (288 + i) (fixedDistLensU `unsafeAt` i)
                   !litMax  <- buildLitTableST (scAllLens sc) 0 288
                                 (scLitCounts sc) (scOffs sc) (scLitSyms sc) (scTable sc) (scShortLits sc)
                   !distMax <- buildDistTableST (scAllLens sc) 288 30
                                 (scDistCounts sc) (scOffs sc) (scDistSyms sc) (scTable sc)
                   huffBlock segs rest litMax distMax bfinal k p bb1 bc1 o
                 2 -> do
                   DynRes litMax distMax k2 p2 bb2 bc2 <- readDynamicST segs sc k p bb1 bc1
                   huffBlock segs rest litMax distMax bfinal k2 p2 bb2 bc2 o
                 _ -> finish o (Just "invalid DEFLATE block type")
      -- A Huffman block, resumed with the window moved on whenever it
      -- stops at the window's end ('needInput'): the tables are still in
      -- the scratch arrays, and nothing was decoded from bits it lacked.
      huffBlock segs rest !litMax !distMax !bfinal !k !p !bb !bc !o = do
        BlockRes k' p' bb' bc' o' err <- huffBlockST segs arr base limit
          (scTable sc)
          (scLitCounts sc) (scLitSyms sc) litMax
          (scDistCounts sc) (scDistSyms sc) distMax
          k p bb bc o
        case err of
          Just e
            | e == needInput ->
                let (segs', rest') = slide segs rest (p' - 16) (p' + win)
                in huffBlock segs' rest' litMax distMax bfinal (seekSeg segs' p') p' bb' bc' o'
            | otherwise -> finish o' err
          Nothing -> if bfinal == 1 then finish o' Nothing else blockLoop segs rest k' p' bb' bc' o'
  blockLoop segs0 rest0 (seekSeg segs0 startByte) startByte 0 0 base

-- | Inflate a stream of unknown output size, stopping at @maxOut@ bytes.
--
-- The bound is not a nicety: a compressed stream names no expanded size, so a
-- hostile or corrupt one could otherwise ask for unbounded memory — the same
-- reasoning as "Cmedit.Pager"'s per-line cap. Hitting it truncates rather than
-- failing, since a partial content stream still renders most of a page.
inflateDyn :: BS.ByteString -> Int -> Int -> Either String BS.ByteString
inflateDyn dat startByte maxOut = runST $ do
  (arrRef, n, err) <- inflateWith dat startByte maxOut True 0 Nothing
  arr <- readSTRef arrRef
  frozen <- freeze arr
  let ua = frozen :: UArray Int Word8
      out = fst (BS.unfoldrN n (\i -> Just (ua ! i, i + 1)) 0)
  -- A truncated-but-decodable stream is worth what it produced, so the bytes
  -- come back even alongside an error; the caller decides (see 'Cmedit.Pdf').
  pure $ case err of
    Just e | BS.null out -> Left e
    _                    -> Right out

-- The decoder proper. @limit@ bounds the output either way; @grow@ says
-- whether the buffer starts small and doubles (unknown size) or is allocated
-- once at full size and silently drops any overflow (known size, PNG's case,
-- where a stream longer than the image is corrupt anyway).
--
-- Returns the buffer, the number of bytes written, and any hard error.
--
-- With a caller's buffer the output occupies indices [base, base+size): the
-- output position, @limit@ and the capacity are all absolute, and only the
-- known-size mode supports it (growing always starts at 0 in a fresh buffer).
inflateWith
  :: BS.ByteString -> Int -> Int -> Bool -> Int -> Maybe (STUArray s Int Word8)
  -> ST s (STRef s (STUArray s Int Word8), Int, Maybe String)
inflateWith dat startByte size grow base given = do
  let limit = base + size
      cap0 | grow      = min limit (max 4096 (4 * BS.length dat))
           | otherwise = limit
  arr0   <- case given of
              Just g  -> pure g
              Nothing -> newArray (0, max 0 (cap0 - 1)) 0 :: ST s (STUArray s Int Word8)
  arrRef <- newSTRef arr0
  capRef <- newSTRef (max 1 cap0)
  -- Bit position and output position share one unboxed array: they are read
  -- and written on every bit and every output byte respectively, and an
  -- 'STRef Int' allocates a fresh box on each write.
  st     <- newArray (0, 1) 0 :: ST s (STUArray s Int Int)
  writeArray st iBit (startByte * 8)
  writeArray st iOut base
  doneRef <- newSTRef False
  errRef  <- newSTRef (Nothing :: Maybe String)
  let fail' msg = writeSTRef errRef (Just msg) >> writeSTRef doneRef True
      -- Double the buffer, copying what is already in it. Geometric, so the
      -- copying costs O(n) over the whole stream.
      growTo !need = do
        cap <- readSTRef capRef
        when (need > cap) $ do
          let cap' = min limit (max need (cap * 2))
          old <- readSTRef arrRef
          new <- newArray (0, max 0 (cap' - 1)) 0
          n <- readArray st iOut
          forLoop 0 (min n cap - 1) $ \i -> readArray old i >>= writeArray new i
          writeSTRef arrRef new
          writeSTRef capRef cap'
      putByte !w = do
        !o <- unsafeRead st iOut
        cap <- readSTRef capRef
        when (grow && o >= cap && o < limit) (growTo (o + 1))
        cap' <- readSTRef capRef
        when (o < cap') $ do
          a <- readSTRef arrRef
          unsafeWrite a o w
        unsafeWrite st iOut (o+1)
      copyBack !dist !len = do
        o <- unsafeRead st iOut
        cap <- readSTRef capRef
        when (grow && o+len > cap && o < limit) (growTo (min limit (o+len)))
        cap' <- readSTRef capRef
        a <- readSTRef arrRef
        -- Copy forwards: overlapping matches must see bytes just written.
        -- Capacity checks and mutable sink lookup happen once per match, and
        -- so does the source check: only a distance reaching before the
        -- start of the output (corrupt) needs it per byte.
        let end = min cap' (o+len) - 1
        if o - dist >= base
          then forLoop o end $ \i -> unsafeRead a (i-dist) >>= unsafeWrite a i
          else forLoop o end $ \i -> do
                 v <- if i-dist >= base then unsafeRead a (i-dist) else pure 0
                 unsafeWrite a i v
        unsafeWrite st iOut (o+len)
      -- Known-size mode: the buffer and its capacity never change, so the
      -- block loop writes straight into it (the general loop below reads
      -- both through STRefs on every byte, for the growing case). Same
      -- bounds as 'putByte'/'copyBack': here the capacity is the limit.
      huffBlockFixed lit dist = loop
        where cap = max 1 limit
              loop = do
                o <- unsafeRead st iOut
                if o >= limit then pure () else do
                  sym <- decodeSymI dat st lit
                  if sym < 0 then fail' "invalid Huffman code"
                  else if sym < 256 then do
                    unsafeWrite arr0 o (fromIntegral sym)
                    unsafeWrite st iOut (o+1)
                    loop
                  else if sym == 256 then pure ()
                  else do
                    let li = sym - 257
                    if li >= nLenCodes then fail' "invalid length code" else do
                      extra <- getBitsI dat st (lenExtra `unsafeAt` li)
                      let len = lenBase `unsafeAt` li + extra
                      dsym <- decodeSymI dat st dist
                      if dsym < 0 || dsym >= nDistCodes then fail' "invalid distance code" else do
                        dextra <- getBitsI dat st (distExtra `unsafeAt` dsym)
                        let d = distBase `unsafeAt` dsym + dextra
                            end = min cap (o+len) - 1
                        if o - d >= base
                          then copyMatch arr0 o d (end-o+1)
                          else forLoop o end $ \i -> do
                                 v <- if i-d >= base then unsafeRead arr0 (i-d) else pure 0
                                 unsafeWrite arr0 i v
                        unsafeWrite st iOut (o+len)
                        loop
      huffBlock lit dist
        | not grow = huffBlockFixed lit dist
        | otherwise = loop
        -- Unknown output size: the input is the only thing that can end a
        -- truncated stream. Past it the reader supplies zero bits, which
        -- decode as symbols for ever, so a stream cut mid-block used to
        -- "inflate" to the full cap (64 MiB of garbage per PDF stream, each
        -- then parsed). Stop at the last real bit and keep what came out.
        -- (The known-size loop keeps its zero-padding tolerance: its output
        -- is bounded by a size the file declared.)
        where endBits = 8 * BS.length dat
              loop = do
                o <- unsafeRead st iOut
                p <- unsafeRead st iBit
                if o >= limit then pure ()
                else if p > endBits then fail' "truncated DEFLATE stream" else do
                  sym <- decodeSymI dat st lit
                  if sym < 0 then fail' "invalid Huffman code"
                  else if sym < 256 then putByte (fromIntegral sym) >> loop
                  else if sym == 256 then pure ()
                  else do
                    let li = sym - 257
                    if li >= nLenCodes then fail' "invalid length code" else do
                      extra <- getBitsI dat st (lenExtra ! li)
                      let len = lenBase ! li + extra
                      dsym <- decodeSymI dat st dist
                      if dsym < 0 || dsym >= nDistCodes then fail' "invalid distance code" else do
                        dextra <- getBitsI dat st (distExtra ! dsym)
                        let d = distBase ! dsym + dextra
                        copyBack d len
                        loop
      storedBlock = do
        p <- readArray st iBit
        writeArray st iBit ((p + 7) .&. complement 7)   -- align to byte
        len <- getBitsI dat st 16
        _nlen <- getBitsI dat st 16
        bytePos <- (`shiftR` 3) <$> readArray st iBit
        o <- readArray st iOut
        cap <- readSTRef capRef
        when (grow && o+len > cap && o < limit) (growTo (min limit (o+len)))
        cap' <- readSTRef capRef
        a <- readSTRef arrRef
        -- Stored blocks are already byte-aligned. Copy their bytes directly,
        -- retaining the reader's zero padding when the input is truncated.
        forLoop 0 (min len (cap'-o) - 1) $ \i -> do
          let pos = bytePos+i
              v = if pos >= 0 && pos < BS.length dat then BS.index dat pos else 0
          writeArray a (o+i) v
        writeArray st iBit ((bytePos+len)*8)
        writeArray st iOut (o+len)
      readDynamic = do
        hlit  <- getBitsI dat st 5
        hdist <- getBitsI dat st 5
        hclen <- getBitsI dat st 4
        let nlit = hlit + 257; ndist = hdist + 1; nclen = hclen + 4
        clRaw <- mapM (const (getBitsI dat st 3)) [1..nclen]
        let clLens = A.elems (A.accumArray (\_ x -> x) 0 (0,18)
                       (zip (take nclen clOrder) clRaw)) :: [Int]
            clHuff = buildInflateHuff clLens
        allLens <- decodeLens clHuff (nlit + ndist)
        let (litLens, distLens) = splitAt nlit allLens
        pure (buildInflateHuff litLens, buildInflateHuff distLens)
      decodeLens clHuff total = reverse <$> go [] 0
        where go acc n
                | n >= total = pure acc
                | otherwise = do
                    s <- decodeSymI dat st clHuff
                    if s < 0 then pure acc
                    else if s < 16 then go (s:acc) (n+1)
                    else if s == 16 then do
                      r <- getBitsI dat st 2
                      let prev = case acc of (p:_) -> p; [] -> 0
                          k = r + 3
                      go (replicate k prev ++ acc) (n+k)
                    else if s == 17 then do
                      r <- getBitsI dat st 3; let k = r + 3 in go (replicate k 0 ++ acc) (n+k)
                    else do
                      r <- getBitsI dat st 7; let k = r + 11 in go (replicate k 0 ++ acc) (n+k)
      -- A block never starts in the zero padding past the end of the
      -- data: there a header reads as a non-final stored block of length 0,
      -- forever, and a stream truncated at a block boundary used to spin
      -- here without end. Every other path makes output progress, so this
      -- changes nothing for any input that ever finished.
      blockLoop = do
        done <- readSTRef doneRef
        o <- readArray st iOut
        p <- readArray st iBit
        if done || o >= limit || p >= 8 * BS.length dat then pure () else do
          bfinal <- getBitI dat st
          btype  <- getBitsI dat st 2
          case btype of
            0 -> storedBlock
            1 -> huffBlock fixedLit fixedDist
            2 -> do (lit,dist) <- readDynamic; huffBlock lit dist
            _ -> fail' "invalid DEFLATE block type"
          err <- readSTRef errRef
          if bfinal == 1 || err /= Nothing then pure () else blockLoop
  blockLoop
  err <- readSTRef errRef
  written <- readArray st iOut
  cap <- readSTRef capRef
  pure (arrRef, max 0 (min cap written - base), err)

-- | A strict counted loop; @forM_ [lo..hi]@ builds a list and a closure per
-- element, which is real allocation in a per-byte loop.
forLoop :: Int -> Int -> (Int -> ST s ()) -> ST s ()
forLoop lo hi act = go lo
  where go !i | i > hi = pure ()
              | otherwise = act i >> go (i + 1)
{-# INLINE forLoop #-}

-- Fixed Huffman tables (RFC 1951 §3.2.6).
fixedLit :: InflateHuff
fixedLit = buildInflateHuff ([8 | _ <- [0..143 :: Int]] ++ [9 | _ <- [144..255 :: Int]]
                   ++ [7 | _ <- [256..279 :: Int]] ++ [8 | _ <- [280..287 :: Int]])

fixedDist :: InflateHuff
fixedDist = buildInflateHuff (replicate 30 5)

clOrder :: [Int]
clOrder = [16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15]

-- DEFLATE's length/distance tables. Unboxed arrays, not lists: these are
-- indexed once per compressed match, and (!!) walks the list every time.
lenBase, lenExtra, distBase, distExtra :: UArray Int Int
lenBase   = U.listArray (0, nLenCodes - 1)
  [3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258]
lenExtra  = U.listArray (0, nLenCodes - 1)
  [0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0]
distBase  = U.listArray (0, nDistCodes - 1)
  [1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769
  ,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577]
distExtra = U.listArray (0, nDistCodes - 1)
  [0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13]

nLenCodes, nDistCodes :: Int
nLenCodes  = 29
nDistCodes = 30

-- A back-reference of @n@ bytes at distance @d@ ending output position @o@,
-- with its whole source inside the output. Short ones byte by byte; long
-- ones (flat runs in screenshots are thousands of bytes) as block copies,
-- out of line (inlined, it slowed the photo cases). A match longer than its
-- distance repeats its last d bytes: copying them, then the 2d bytes now
-- available, then 4d, and so on gives exactly what the byte-by-byte forward
-- copy gives, with every block's source wholly before its destination.
{-# INLINE copyMatch #-}
copyMatch :: STUArray s Int Word8 -> Int -> Int -> Int -> ST s ()
copyMatch a o d n
  | n < 16 = forLoop o (o+n-1) $ \i -> unsafeRead a (i-d) >>= unsafeWrite a i
  | otherwise = copyLong a o d n

{-# NOINLINE copyLong #-}
copyLong :: STUArray s Int Word8 -> Int -> Int -> Int -> ST s ()
copyLong a o d n = go o d n
  where
    go !pos !distance !remaining
      | remaining <= 0 = pure ()
      | otherwise = do
          let chunk = min distance remaining
          copyBytes a (pos-distance) pos chunk
          go (pos+chunk) (distance+chunk) (remaining-chunk)

-- memcpy within one 0-based byte array (STUArray indices are byte offsets).
{-# INLINE copyBytes #-}
copyBytes :: STUArray s Int Word8 -> Int -> Int -> Int -> ST s ()
copyBytes (STUArray _ _ _ a#) (I# src#) (I# dst#) (I# n#) = ST $ \s# ->
  case copyMutableByteArray# a# src# a# dst# n# s# of s1# -> (# s1#, () #)
