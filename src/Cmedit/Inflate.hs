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
import Data.Array (Array)
import qualified Data.Array as A
import Data.Array.ST (STUArray, newArray, readArray, writeArray, runSTUArray)
import Data.Array.MArray (freeze)
import Data.Array.Unboxed (UArray, (!))
import qualified Data.Array.Unboxed as U
import Data.Bits
import Data.STRef
import Data.Word (Word8, Word32, Word64, byteSwap64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BSU
import Data.Array.Base (STUArray(..), unsafeRead, unsafeWrite, unsafeAt)
import GHC.ByteOrder (ByteOrder(..), targetByteOrder)
import GHC.Exts (Addr#, Int(I#), Int#, MutableByteArray#, State#, Word64#, copyMutableByteArray#, plusAddr#, readIntArray#, readWord64OffAddr#, runRW#, setByteArray#, touch#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents)
import GHC.ST (ST(..))
import GHC.Word (Word64(W64#))

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
  err <- inflateFixed dat startByte arr 0 outSize
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
  inflateFixed dat startByte arr base outSize

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

-- Refill the 64-bit bit buffer to 56..63 valid bits.
{-# INLINE refill #-}
refill :: BS.ByteString -> Int -> Word64 -> Int -> (# Int#, Word64#, Int# #)
refill dat@(BI.BS (ForeignPtr addr# fpc) datLen) !p !bb !bc
  | p >= 0 && p + 8 <= datLen =
      let !w   = le64Raw addr# fpc p
          !bb' = bb .|. (w `unsafeShiftL` bc)
          !p'  = p + ((63 - bc) `unsafeShiftR` 3)
          !bc' = bc .|. 56
      in case (p', bb', bc') of (I# p#, W64# bb#, I# bc#) -> (# p#, bb#, bc# #)
  | otherwise = case (p, bb, bc) of (I# p#, W64# bb#, I# bc#) -> refillSlow dat p# bb# bc#

{-# OPAQUE refillSlow #-}
refillSlow :: BS.ByteString -> Int# -> Word64# -> Int# -> (# Int#, Word64#, Int# #)
refillSlow dat p0# bb0# bc0# = go (I# p0#) (W64# bb0#) (I# bc0#)
  where
    !datLen = BS.length dat
    go !p !bb !bc
      | bc >= 56 = case (p, bb, bc) of (I# p#, W64# bb#, I# bc#) -> (# p#, bb#, bc# #)
      | p >= 0 && p < datLen =
          let !b = fromIntegral (BSU.unsafeIndex dat p) :: Word64
          in go (p + 1) (bb .|. (b `unsafeShiftL` bc)) (bc + 8)
      | otherwise =
          go (p + 1) bb (bc + 8)

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
data InflateScratch s = InflateScratch
  { scClLens     :: !(STUArray s Int Int)    -- 19
  , scClTable    :: !(STUArray s Int Word32) -- 128 (7-bit)
  , scClCounts   :: !(STUArray s Int Int)    -- 16
  , scClSyms     :: !(STUArray s Int Int)    -- 19
  , scAllLens    :: !(STUArray s Int Int)    -- 464
  , scOffs       :: !(STUArray s Int Int)    -- 16
  , scLitTable   :: !(STUArray s Int Word32) -- 1024 (10-bit)
  , scLitCounts  :: !(STUArray s Int Int)    -- 16
  , scLitSyms    :: !(STUArray s Int Int)    -- 288
  , scDistTable  :: !(STUArray s Int Word32) -- 1024 (10-bit)
  , scDistCounts :: !(STUArray s Int Int)    -- 16
  , scDistSyms   :: !(STUArray s Int Int)    -- 464
  }

newInflateScratch :: ST s (InflateScratch s)
newInflateScratch = InflateScratch
  <$> newArray (0, 18) 0
  <*> newArray (0, 127) 0
  <*> newArray (0, 15) 0
  <*> newArray (0, 18) 0
  <*> newArray (0, 463) 0
  <*> newArray (0, 15) 0
  <*> newArray (0, (1 `unsafeShiftL` inflateBits) - 1) 0
  <*> newArray (0, 15) 0
  <*> newArray (0, 287) 0
  <*> newArray (0, (1 `unsafeShiftL` inflateBits) - 1) 0
  <*> newArray (0, 15) 0
  <*> newArray (0, 463) 0

-- Build a canonical Huffman table + @bits@-wide prefix lookup table into
-- reusable STUArrays, allocating nothing. Returns @(maxLen, valid)@ packed as
-- @maxLen * 2 + (if valid then 1 else 0)@.
{-# NOINLINE buildTableST #-}
buildTableST :: Int -> STUArray s Int Int -> Int -> Int
             -> STUArray s Int Int -> STUArray s Int Int -> STUArray s Int Int
             -> STUArray s Int Word32 -> ST s Int
buildTableST !bits !lens !off !n !counts !offs !syms !table = do
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
  -- Compute starting offsets and validate Kraft inequality (non-oversubscribed).
  let offLoop !l !acc !slots !ok
        | l > maxLen = pure ok
        | otherwise = do
            !c <- unsafeRead counts l
            unsafeWrite offs l acc
            let !nextSlots = slots * 2 - c
            offLoop (l + 1) (acc + c) nextSlots (ok && nextSlots >= 0)
  !valid <- offLoop 1 0 1 True
  -- Counting-sort symbols by (code length, symbol index).
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
  let !size = 1 `unsafeShiftL` bits
  zeroWord32Arr table size
  when valid $ do
    let !lim = min bits maxLen
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
                              | idx >= size = pure ()
                              | otherwise   = unsafeWrite table idx entry >> fillSlots (idx + step)
                        fillSlots rev
                        fillSyms (j + 1)
              fillSyms 0
              fillLen (len + 1) ((first + cnt) `unsafeShiftL` 1) (index + cnt)
    fillLen 1 0 0
  pure ((maxLen `unsafeShiftL` 1) .|. (if valid then 1 else 0))

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

data BlockRes = BlockRes !Int !Word64 !Int !Int !(Maybe String)

{-# NOINLINE huffBlockST #-}
huffBlockST :: BS.ByteString -> STUArray s Int Word8 -> Int -> Int
            -> STUArray s Int Word32 -> STUArray s Int Int -> STUArray s Int Int -> Int
            -> STUArray s Int Word32 -> STUArray s Int Int -> STUArray s Int Int -> Int
            -> Int -> Word64 -> Int -> Int
            -> ST s BlockRes
huffBlockST !dat@(BI.BS (ForeignPtr addr# fpc) !datLen) !arr !base !limit
            !litTbl !(STUArray _ _ _ litCnt#) !(STUArray _ _ _ litSym#) !(I# litMax#)
            !distTbl !(STUArray _ _ _ distCnt#) !(STUArray _ _ _ distSym#) !(I# distMax#)
            !p0 !bb0 !bc0 !o0 =
  loop p0 bb0 bc0 o0
  where
    !mask    = (1 `unsafeShiftL` inflateBits) - 1
    !fastEnd = datLen - 8
    loop !p !bb !bc !o
      | o >= limit = pure (BlockRes p bb bc o Nothing)
      | p >= 0 && p <= fastEnd =
          let !w   = le64Raw addr# fpc p
              !bb1 = bb .|. (w `unsafeShiftL` bc)
              !p1  = p + ((63 - bc) `unsafeShiftR` 3)
              !bc1 = bc .|. 56
          in decodeLit p1 bb1 bc1 o
      | otherwise =
          case (p, bb, bc) of
            (I# p#, W64# bb#, I# bc#) -> case refillSlow dat p# bb# bc# of
              (# p1#, bb1#, bc1# #) -> decodeLit (I# p1#) (W64# bb1#) (I# bc1#) o
    decodeLit !p !bb !bc !o = do
      !e32 <- unsafeRead litTbl (fromIntegral bb .&. mask)
      let !e   = fromIntegral e32 :: Int
          !len = e .&. 15
      if len /= 0
        then handleSym p (bb `unsafeShiftR` len) (bc - len) o (e `unsafeShiftR` 4)
        else case (bb, bc) of
          (W64# bb#, I# bc#) -> ST $ \s# ->
            case decodeSymSlowM bb# bc# litCnt# litSym# litMax# s# of
              (# s1#, sym#, bb2#, bc2# #) ->
                case handleSym p (W64# bb2#) (I# bc2#) o (I# sym#) of
                  ST f -> f s1#
    handleSym !p !bb !bc !o !sym
      | sym < 0    = pure (BlockRes p bb bc o (Just "invalid Huffman code"))
      | sym < 256  = do
          unsafeWrite arr o (fromIntegral sym)
          loop p bb bc (o + 1)
      | sym == 256 = pure (BlockRes p bb bc o Nothing)
      | otherwise  =
          let !li = sym - 257
          in if li >= nLenCodes
               then pure (BlockRes p bb bc o (Just "invalid length code"))
               else do
                 let !lextra = lenExtra `unsafeAt` li
                     !extra  = fromIntegral bb .&. ((1 `unsafeShiftL` lextra) - 1)
                     !mlen   = lenBase `unsafeAt` li + extra
                     !bb2    = bb `unsafeShiftR` lextra
                     !bc2    = bc - lextra
                 !de32 <- unsafeRead distTbl (fromIntegral bb2 .&. mask)
                 let !de   = fromIntegral de32 :: Int
                     !dlen = de .&. 15
                 if dlen /= 0
                   then handleDist p (bb2 `unsafeShiftR` dlen) (bc2 - dlen) o mlen (de `unsafeShiftR` 4)
                   else case (bb2, bc2) of
                     (W64# bb2#, I# bc2#) -> ST $ \s# ->
                       case decodeSymSlowM bb2# bc2# distCnt# distSym# distMax# s# of
                         (# s1#, dsym#, bb3#, bc3# #) ->
                           case handleDist p (W64# bb3#) (I# bc3#) o mlen (I# dsym#) of
                             ST f -> f s1#
    handleDist !p !bb !bc !o !mlen !dsym
      | dsym < 0 || dsym >= nDistCodes = pure (BlockRes p bb bc o (Just "invalid distance code"))
      | otherwise = do
          let !dextraB = distExtra `unsafeAt` dsym
              !dextra  = fromIntegral bb .&. ((1 `unsafeShiftL` dextraB) - 1)
              !d       = distBase `unsafeAt` dsym + dextra
              !bb2     = bb `unsafeShiftR` dextraB
              !bc2     = bc - dextraB
              !end     = min limit (o + mlen) - 1
          if o - d >= base
            then copyMatch arr o d (end - o + 1)
            else forLoop o end $ \i -> do
                   v <- if i - d >= base then unsafeRead arr (i - d) else pure 0
                   unsafeWrite arr i v
          loop p bb2 bc2 (o + mlen)

data DynRes = DynRes !Int !Int !Int !Word64 !Int

{-# NOINLINE readDynamicST #-}
readDynamicST :: BS.ByteString -> InflateScratch s -> Int -> Word64 -> Int -> ST s DynRes
readDynamicST !dat !sc !p0 !bb0 !bc0 =
  case refill dat p0 bb0 bc0 of
    (# p1#, bb1#, bc1# #) -> do
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
      let readCL !i !p !bb !bc
            | i >= nclen = pure (p, bb, bc)
            | bc < 3 = case refill dat p bb bc of
                (# p'#, bb'#, bc'# #) -> readCL i (I# p'#) (W64# bb'#) (I# bc'#)
            | otherwise = do
                let !v = fromIntegral (bb .&. 7) :: Int
                unsafeWrite clLens (clOrderU `unsafeAt` i) v
                readCL (i + 1) p (bb `unsafeShiftR` 3) (bc - 3)
      (!p2, !bb2, !bc2) <- readCL 0 (I# p1#) (bb1 `unsafeShiftR` 14) (I# bc1# - 14)
      !clMeta <- buildTableST 7 clLens 0 19 clCnt offs clSym clTbl
      let !clMax   = clMeta `unsafeShiftR` 1
          !clValid = clMeta .&. 1
          !total   = nlit + ndist
          !(STUArray _ _ _ clCnt#) = clCnt
          !(STUArray _ _ _ clSym#) = clSym
          !(I# clMax#) = clMax
          fillVal !start !k !val = go 0
            where go !j | j >= k    = pure ()
                        | otherwise = unsafeWrite allLens (start + j) val >> go (j + 1)
          decodeLens !n !prev !p !bb !bc
            | n >= total = pure (n, p, bb, bc)
            | bc < 22 = case refill dat p bb bc of
                (# p'#, bb'#, bc'# #) -> decodeLens n prev (I# p'#) (W64# bb'#) (I# bc'#)
            | clValid == 1 = do
                !e32 <- unsafeRead clTbl (fromIntegral bb .&. 127)
                let !e   = fromIntegral e32 :: Int
                    !len = e .&. 15
                if len == 0
                  then pure (n, p, bb `unsafeShiftR` clMax, bc - clMax)
                  else step n prev (e `unsafeShiftR` 4) p (bb `unsafeShiftR` len) (bc - len)
            | otherwise = case (bb, bc) of
                (W64# bb#, I# bc#) -> ST $ \s# ->
                  case decodeSymSlowM bb# bc# clCnt# clSym# clMax# s# of
                    (# s1#, sSym#, bb'#, bc'# #) ->
                      case step n prev (I# sSym#) p (W64# bb'#) (I# bc'#) of
                        ST f -> f s1#
          step !n !prev !s !p !bb !bc
            | s < 0  = pure (n, p, bb, bc)
            | s < 16 = do
                unsafeWrite allLens n s
                decodeLens (n + 1) s p bb bc
            | s == 16 = do
                let !k = fromIntegral (bb .&. 3) + 3
                fillVal n k prev
                decodeLens (n + k) prev p (bb `unsafeShiftR` 2) (bc - 2)
            | s == 17 = do
                let !k = fromIntegral (bb .&. 7) + 3
                fillVal n k 0
                decodeLens (n + k) 0 p (bb `unsafeShiftR` 3) (bc - 3)
            | otherwise = do
                let !k = fromIntegral (bb .&. 127) + 11
                fillVal n k 0
                decodeLens (n + k) 0 p (bb `unsafeShiftR` 7) (bc - 7)
      (!actualTotal, !p3, !bb3, !bc3) <- decodeLens 0 0 p2 bb2 bc2
      !litMeta  <- buildTableST inflateBits allLens 0 (min nlit actualTotal)
                     (scLitCounts sc) offs (scLitSyms sc) (scLitTable sc)
      !distMeta <- buildTableST inflateBits allLens nlit (max 0 (actualTotal - nlit))
                     (scDistCounts sc) offs (scDistSyms sc) (scDistTable sc)
      pure (DynRes (litMeta `unsafeShiftR` 1) (distMeta `unsafeShiftR` 1) p3 bb3 bc3)

{-# NOINLINE inflateFixed #-}
inflateFixed :: BS.ByteString -> Int -> STUArray s Int Word8 -> Int -> Int
             -> ST s (Maybe String)
inflateFixed dat !startByte arr !base !size = do
  sc <- newInflateScratch
  let !limit   = base + size
      !datLen  = BS.length dat
      !endBits = 8 * datLen
      blockLoop !p !bb !bc !o
        | o >= limit || p * 8 - bc >= endBits = pure Nothing
        | bc < 3 = case refill dat p bb bc of
            (# p'#, bb'#, bc'# #) -> blockLoop (I# p'#) (W64# bb'#) (I# bc'#) o
        | otherwise =
            let !bfinal = fromIntegral (bb .&. 1) :: Int
                !btype  = fromIntegral ((bb `unsafeShiftR` 1) .&. 3) :: Int
                !bb1    = bb `unsafeShiftR` 3
                !bc1    = bc - 3
            in case btype of
                 0 -> do
                   let !curBitPos   = p * 8 - bc1
                       !alignedByte = ((curBitPos + 7) .&. complement 7) `unsafeShiftR` 3
                       byteAt !idx  = if idx >= 0 && idx < datLen then fromIntegral (BSU.unsafeIndex dat idx) else 0 :: Int
                       !len         = byteAt alignedByte .|. (byteAt (alignedByte + 1) `unsafeShiftL` 8)
                       !srcStart    = alignedByte + 4
                       !toCopy      = min len (limit - o)
                   forLoop 0 (toCopy - 1) $ \i -> do
                     let !pos = srcStart + i
                         !v   = if pos >= 0 && pos < datLen then BSU.unsafeIndex dat pos else 0
                     unsafeWrite arr (o + i) v
                   if bfinal == 1 then pure Nothing
                   else blockLoop (srcStart + len) 0 0 (o + len)
                 1 -> do
                   forLoop 0 287 $ \i -> unsafeWrite (scAllLens sc) i (fixedLitLensU `unsafeAt` i)
                   forLoop 0 29  $ \i -> unsafeWrite (scAllLens sc) (288 + i) (fixedDistLensU `unsafeAt` i)
                   !litMeta  <- buildTableST inflateBits (scAllLens sc) 0 288
                                  (scLitCounts sc) (scOffs sc) (scLitSyms sc) (scLitTable sc)
                   !distMeta <- buildTableST inflateBits (scAllLens sc) 288 30
                                  (scDistCounts sc) (scOffs sc) (scDistSyms sc) (scDistTable sc)
                   BlockRes p' bb' bc' o' err <- huffBlockST dat arr base limit
                     (scLitTable sc) (scLitCounts sc) (scLitSyms sc) (litMeta `unsafeShiftR` 1)
                     (scDistTable sc) (scDistCounts sc) (scDistSyms sc) (distMeta `unsafeShiftR` 1)
                     p bb1 bc1 o
                   case err of
                     Just _  -> pure err
                     Nothing -> if bfinal == 1 then pure Nothing else blockLoop p' bb' bc' o'
                 2 -> do
                   DynRes litMax distMax p2 bb2 bc2 <- readDynamicST dat sc p bb1 bc1
                   BlockRes p' bb' bc' o' err <- huffBlockST dat arr base limit
                     (scLitTable sc) (scLitCounts sc) (scLitSyms sc) litMax
                     (scDistTable sc) (scDistCounts sc) (scDistSyms sc) distMax
                     p2 bb2 bc2 o
                   case err of
                     Just _  -> pure err
                     Nothing -> if bfinal == 1 then pure Nothing else blockLoop p' bb' bc' o'
                 _ -> pure (Just "invalid DEFLATE block type")
  blockLoop startByte 0 0 base

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
