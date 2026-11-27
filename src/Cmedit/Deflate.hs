{-# LANGUAGE BangPatterns #-}
-- | Fast RFC 1950 zlib / RFC 1951 DEFLATE compression, hand-rolled on GHC
-- boot libraries.
--
-- Designed for the kitty graphics @o=z@ transport (plan 0041): a single
-- DEFLATE block with a constant Huffman code (BTYPE=10 and a precomputed
-- header; see the code tables) driven by an L1-resident 1-probe
-- hash-table LZ77 matcher (4-byte hashes, greedy, no hash chains) with an
-- early incompressibility bailout. On flat or structured images (PDF pages,
-- SVG diagrams, screenshots) it reaches 10–30x compression in a few
-- milliseconds; on noisy photographs it bails out after 64 KiB in < 0.3 ms
-- so the caller can send the raw bytes without wasting CPU or bandwidth.
--
-- Leaf module: no @Cmedit.*@ imports.
module Cmedit.Deflate
  ( zlibCompress
  , zlibCompressFast
  , adler32
  ) where

import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newArray)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits
  ( (.&.), (.|.), countLeadingZeros, shiftL, shiftR, testBit )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IM
import Data.List (mapAccumL)
import Data.Word (Word8, Word32, Word64)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

------------------------------------------------------------------------------
-- Public API

-- | Compress @src@ into a valid RFC 1950 zlib stream (2-byte header,
-- one RFC 1951 DEFLATE block, 4-byte big-endian Adler-32 trailer).
-- Always succeeds; output is right-sized so it never pins a larger buffer.
zlibCompress :: BS.ByteString -> BS.ByteString
zlibCompress src =
  let n = BS.length src
      -- At most 9 bits/literal + 2 header + the block header + 7 EOB
      -- + 7 align + 4 Adler-32 = n + ceil(n/8) + blockHeaderLen + 10 bytes.
      -- Add 32 bytes of headroom for 32-bit word flushes.
      cap = n + (n `shiftR` 3) + blockHeaderLen + 32
  in unsafeDupablePerformIO $ BI.createAndTrim cap $ \outPtr ->
       deflateInto False src outPtr cap

-- | Fast zlib compression with early bailout for incompressible payloads.
-- Returns 'Nothing' when @src@ is too small (< 64 bytes), looks
-- incompressible after the first 64 KiB (output >= 95% of input; noise
-- under this code runs ~103–106%), or the compressed stream is not strictly
-- smaller than @src@.
zlibCompressFast :: BS.ByteString -> Maybe BS.ByteString
zlibCompressFast src
  | n < 64 = Nothing
  | otherwise =
      -- Only allocate enough for an output strictly smaller than @src@
      -- (plus the block header, which is written before the first limit
      -- check, and 32 bytes flush headroom); anything larger aborts early.
      let cap = n + blockHeaderLen + 32
          bs = unsafeDupablePerformIO $ BI.createAndTrim cap $ \outPtr ->
                 deflateInto True src outPtr cap
      in if BS.null bs || BS.length bs >= n then Nothing else Just bs
  where
    n = BS.length src

------------------------------------------------------------------------------
-- Adler-32 (RFC 1950 §8.2)

-- | RFC 1950 Adler-32 checksum. Processes up to 5552 bytes per modulo step so
-- the 32-bit accumulators never overflow.
adler32 :: BS.ByteString -> Word32
adler32 src = go 0 1 0
  where
    !n = BS.length src
    go !i !s1 !s2
      | i >= n = (s2 `shiftL` 16) .|. s1
      | otherwise =
          let !end = min n (i + 5552)
          in chunk i end s1 s2
    chunk !i !end !s1 !s2
      | i >= end = go i (s1 `rem` 65521) (s2 `rem` 65521)
      | otherwise =
          let !b   = fromIntegral (BU.unsafeIndex src i) :: Word32
              !s1' = s1 + b
              !s2' = s2 + s1'
          in chunk (i + 1) end s1' s2'

------------------------------------------------------------------------------
-- Huffman code tables (RFC 1951 §3.2.2, §3.2.5 – §3.2.7)
--
-- The block is a /dynamic/ one (BTYPE=10) whose code is a constant, declared
-- by the precomputed 'blockHeader'. It is the fixed code of §3.2.6 adjusted
-- only as far as a dynamic header requires (286 literal/length and 30
-- distance symbols, both codes complete), so the encoder is still a table
-- lookup per symbol and the header costs 'blockHeaderLen' bytes per stream.
--
-- It is not a fixed block (BTYPE=01) because Ghostty 1.3.1 segfaults on one:
-- any fixed-Huffman stream with matches that inflates past a few tens of
-- kilobytes kills the terminal, stock zlib's @Z_FIXED@ output included, while
-- dynamic blocks over the same data are fine. Nearly every encoder in use
-- emits dynamic blocks, so that is the path terminals are tested on. A Spec
-- test pins the block type.

-- Reverse the lowest @k@ bits of @v@.
revBits :: Int -> Word32 -> Word32
revBits !k !v = go 0 0
  where
    go !i !acc
      | i >= k = acc
      | otherwise =
          let !bit = if testBit v i then 1 `shiftL` (k - 1 - i) else 0
          in go (i + 1) (acc .|. bit)

-- Pack (bitCount, lsbFirstBits) into a single Word32:
-- bits 24..31 = bitCount (<= 24), bits 0..23 = bits.
packCode :: Int -> Word32 -> Word32
packCode !nb !bits = (fromIntegral nb `shiftL` 24) .|. bits

-- Canonical Huffman codes for a list of code lengths (§3.2.2), one
-- @(length, bit-reversed code)@ per symbol, ready for LSB-first emission.
-- A length of 0 means the symbol is unused.
canonCodes :: [Int] -> [(Int, Word32)]
canonCodes lens = snd (mapAccumL assign firstCodes lens)
  where
    count l = length (filter (== l) lens)
    firstCodes :: IM.IntMap Word32
    firstCodes = IM.fromList (go 1 0)
      where
        go :: Int -> Word32 -> [(Int, Word32)]
        go l prev
          | l > 15    = []
          | otherwise =
              let c = (prev + (if l == 1 then 0 else fromIntegral (count (l - 1))))
                        `shiftL` 1
              in (l, c) : go (l + 1) c
    assign next 0 = (next, (0, 0))
    assign next l =
      let c = IM.findWithDefault 0 l next
      in (IM.insert l (c + 1) next, (l, revBits l c))

-- Literal/length code lengths, symbols 0..285: the fixed code's, with
-- literals 144..147 shortened to 8 bits to make up for the two symbols
-- (286, 287) a dynamic header cannot declare. Kraft sum exactly 1.
litLenLens :: [Int]
litLenLens = [ len s | s <- [0 .. 285 :: Int] ]
  where
    len s
      | s <= 147  = 8
      | s <= 255  = 9
      | s <= 279  = 7
      | otherwise = 8

-- Distance code lengths, symbols 0..29: 5 bits as in the fixed code, except
-- distances 1 and 4 (a flat byte run, a flat RGBA run) at 4 bits, which is
-- what makes 30 symbols a complete code.
distLens :: [Int]
distLens = [ if s == 0 || s == 3 then 4 else 5 | s <- [0 .. 29 :: Int] ]

litLenCodes :: UArray Int Word32
litLenCodes = listArray (0, 285) [ packCode l c | (l, c) <- canonCodes litLenLens ]

-- Distance symbol codes 0..29, packed like 'packCode'.
distSymTbl :: UArray Int Word32
distSymTbl = listArray (0, 29) [ packCode l c | (l, c) <- canonCodes distLens ]

-- The dynamic block header (§3.2.7) as whole bytes plus the leftover
-- @(bit count, bits)@ that seed the encoder's accumulator: BFINAL=1,
-- BTYPE=10, HLIT, HDIST, HCLEN, the code-length code, then the 316 code
-- lengths, each written as a plain code-length symbol (no repeat codes; the
-- lengths used are 4, 5, 7, 8 and 9).
blockHeader :: (BS.ByteString, Int, Word64)
blockHeader =
  ( BS.pack [ fromIntegral (big `shiftR` (8 * k)) | k <- [0 .. nBytes - 1] ]
  , remBits
  , fromIntegral (big `shiftR` (8 * nBytes)) )
  where
    -- Code-length alphabet: lengths for the symbols we use.
    clLens = [ clLen s | s <- [0 .. 18 :: Int] ]
    clLen s = case s of { 8 -> 1; 9 -> 2; 7 -> 3; 5 -> 4; 4 -> 4; _ -> 0 }
    clCodes = canonCodes clLens
    clOrder = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
    nCl = 12   -- up to and including symbol 4, the last one in use
    fields :: [(Int, Integer)]
    fields =
      [ (1, 1), (2, 2), (5, 286 - 257), (5, 30 - 1), (4, fromIntegral (nCl - 4)) ]
      ++ [ (3, fromIntegral (clLens !! s)) | s <- take nCl clOrder ]
      ++ [ (l, fromIntegral c)
         | len <- litLenLens ++ distLens, let (l, c) = clCodes !! len ]
    (total, big) = foldl (\(pos, acc) (nb, v) -> (pos + nb, acc .|. (v `shiftL` pos)))
                         (0, 0 :: Integer) fields
    nBytes  = total `div` 8
    remBits = total `mod` 8

-- | Whole bytes of 'blockHeader' (the rest of it rides in the accumulator).
blockHeaderLen :: Int
blockHeaderLen = let (bs, _, _) = blockHeader in BS.length bs

-- Literal/EOB table for symbols 0..256.
litCodeTbl :: UArray Int Word32
litCodeTbl = listArray (0, 256) [ unsafeAt litLenCodes s | s <- [0 .. 256] ]

-- Length table for match lengths 3..258: combines the length
-- symbol code (257..285) with its extra bits into a single LSB-first bitfield
-- (at most 8 + 5 = 13 bits).
lenCodeTbl :: UArray Int Word32
lenCodeTbl = listArray (3, 258) [ mk l | l <- [3 .. 258] ]
  where
    bases :: [(Int, Int, Int)]  -- (sym, baseLen, extraBits)
    bases =
      [ (257, 3, 0),   (258, 4, 0),   (259, 5, 0),   (260, 6, 0)
      , (261, 7, 0),   (262, 8, 0),   (263, 9, 0),   (264, 10, 0)
      , (265, 11, 1),  (266, 13, 1),  (267, 15, 1),  (268, 17, 1)
      , (269, 19, 2),  (270, 23, 2),  (271, 27, 2),  (272, 31, 2)
      , (273, 35, 3),  (274, 43, 3),  (275, 51, 3),  (276, 59, 3)
      , (277, 67, 4),  (278, 83, 4),  (279, 99, 4),  (280, 115, 4)
      , (281, 131, 5), (282, 163, 5), (283, 195, 5), (284, 227, 5)
      , (285, 258, 0)
      ]
    symCode s =
      let e = unsafeAt litLenCodes s
      in (fromIntegral (e `shiftR` 24), e .&. 0xFFFFFF)
    findSym 258 _ = (285, 258, 0)
    findSym l ((s, b, eb) : rest)
      | s < 285, l >= b, l < b + (1 `shiftL` eb) = (s, b, eb)
      | otherwise = findSym l rest
    findSym _ [] = (285, 258, 0)
    mk l =
      let (s, b, eb) = findSym l bases
          (clen, cbits) = symCode s
          extra = fromIntegral (l - b) :: Word32
      in packCode (clen + eb) (cbits .|. (extra `shiftL` clen))

-- Precomputed combined (distance code + extra bits) for distances 1..512.
distSmallTbl :: UArray Int Word32
distSmallTbl = listArray (1, 512) [ distCodeSlow d | d <- [1 .. 512] ]

{-# INLINE distCodeSlow #-}
distCodeSlow :: Int -> Word32
distCodeSlow !dist
  | dist <= 4 = unsafeAt distSymTbl (dist - 1)
  | otherwise =
      let !d     = dist - 1
          !msb   = 63 - countLeadingZeros (fromIntegral d :: Word64)
          !ebits = msb - 1
          !half  = (d `shiftR` ebits) .&. 1
          !sym   = 2 * msb + half
          !extra = fromIntegral (d .&. ((1 `shiftL` ebits) - 1)) :: Word32
          !e     = unsafeAt distSymTbl sym
          !clen  = fromIntegral (e `shiftR` 24) :: Int
      in packCode (clen + ebits) ((e .&. 0xFFFFFF) .|. (extra `shiftL` clen))

{-# INLINE distCode #-}
distCode :: Int -> Word32
distCode !dist
  | dist <= 512 = unsafeAt distSmallTbl (dist - 1)
  | otherwise   = distCodeSlow dist

------------------------------------------------------------------------------
-- Core LZ77 + Huffman encoder

hashBits :: Int
hashBits = 13

hashSize :: Int
hashSize = 1 `shiftL` hashBits  -- 8192 entries * 4 bytes = 32 KiB (L1-resident)

hashMask :: Int
hashMask = hashSize - 1

{-# INLINE read4 #-}
read4 :: BS.ByteString -> Int -> Word32
read4 !src !i =
      (fromIntegral (BU.unsafeIndex src i))
  .|. (fromIntegral (BU.unsafeIndex src (i + 1)) `shiftL` 8)
  .|. (fromIntegral (BU.unsafeIndex src (i + 2)) `shiftL` 16)
  .|. (fromIntegral (BU.unsafeIndex src (i + 3)) `shiftL` 24)

{-# INLINE hash4 #-}
hash4 :: Word32 -> Int
hash4 !w = fromIntegral ((w * 2654435761) `shiftR` (32 - hashBits)) .&. hashMask

-- Write 4 little-endian bytes from the low 32 bits of @acc@.
{-# INLINE writeWord32LE #-}
writeWord32LE :: Ptr Word8 -> Int -> Word64 -> IO ()
writeWord32LE !p !o !acc = do
  pokeByteOff p o       (fromIntegral acc :: Word8)
  pokeByteOff p (o + 1) (fromIntegral (acc `shiftR` 8) :: Word8)
  pokeByteOff p (o + 2) (fromIntegral (acc `shiftR` 16) :: Word8)
  pokeByteOff p (o + 3) (fromIntegral (acc `shiftR` 24) :: Word8)

-- Write RFC 1950 zlib stream into @outPtr@ (capacity @cap@), returning the
-- exact byte count written, or 0 if @bailFast@ aborted due to
-- incompressibility.
deflateInto :: Bool -> BS.ByteString -> Ptr Word8 -> Int -> IO Int
deflateInto !bailFast !src !outPtr !cap = do
  let !n = BS.length src
      -- Bailout ceiling: for bailFast, abort if outPos reaches n - 6
      -- (because we still need EOB + align + 4-byte Adler-32).
      !hardLimit = if bailFast then n - 6 else cap - 16
      !checkAt   = 65536
  -- RFC 1950 zlib header: CMF = 0x78 (deflate, 32K), FLG = 0x01 (fastest).
  -- (0x78 * 256 + 0x01) `mod` 31 == 0.
  pokeByteOff outPtr 0 (0x78 :: Word8)
  pokeByteOff outPtr 1 (0x01 :: Word8)

  (finalOut, finalAcc, finalBits, aborted) <- runLz77 bailFast src n outPtr hardLimit checkAt
  if aborted
    then pure 0
    else do
      -- Append End-of-Block symbol (256: 7 zero bits).
      let !eobEntry = unsafeAt litCodeTbl 256
          !eobNb    = fromIntegral (eobEntry `shiftR` 24) :: Int
          !eobBits  = fromIntegral (eobEntry .&. 0xFFFFFF) :: Word64
          !acc1     = finalAcc .|. (eobBits `shiftL` finalBits)
          !nb1      = finalBits + eobNb
      -- Flush all remaining bits (byte-aligned with trailing zeros).
      o1 <- flushTail outPtr finalOut acc1 nb1
      if bailFast && o1 + 4 >= n
        then pure 0
        else do
          -- Append big-endian Adler-32 trailer (computed only when we did
          -- not bail out).
          let !chk = adler32 src
          pokeByteOff outPtr o1       (fromIntegral (chk `shiftR` 24) :: Word8)
          pokeByteOff outPtr (o1 + 1) (fromIntegral (chk `shiftR` 16) :: Word8)
          pokeByteOff outPtr (o1 + 2) (fromIntegral (chk `shiftR` 8)  :: Word8)
          pokeByteOff outPtr (o1 + 3) (fromIntegral chk               :: Word8)
          pure (o1 + 4)

flushTail :: Ptr Word8 -> Int -> Word64 -> Int -> IO Int
flushTail !p !o !acc !nb
  | nb <= 0   = pure o
  | otherwise = do
      pokeByteOff p o (fromIntegral acc :: Word8)
      flushTail p (o + 1) (acc `shiftR` 8) (nb - 8)

-- Core LZ77 + Huffman loop over a 32 KiB hash table.
runLz77 :: Bool -> BS.ByteString -> Int -> Ptr Word8 -> Int -> Int
        -> IO (Int, Word64, Int, Bool)
runLz77 !bailFast !src !n !outPtr !hardLimit !checkAt = do
  ht <- newHashTable
  -- The block header: its whole bytes go straight out after the two zlib
  -- header bytes, its last few bits start the accumulator.
  let (hdrBytes, hdrBits, hdrAcc) = blockHeader
      !o0 = 2 + BS.length hdrBytes
  mapM_ (\k -> pokeByteOff outPtr (2 + k) (BU.unsafeIndex hdrBytes k))
        [0 .. BS.length hdrBytes - 1]
  let !lim4 = n - 4
      -- Guard against Int32 position wrap on absurd >2 GiB inputs.
      !maxPos = 0x7FFFFE00 :: Int
      go !i !o !acc !nb !nextChk !prevI !prevO
        | bailFast && o >= hardLimit = pure (o, acc, nb, True)
        | bailFast && i >= nextChk =
            let !di = i - prevI
                !do_ = o - prevO
                -- Bail out if cumulative output >= 95% of input, OR if a later
                -- 64 KiB window expanded (>= 98%) while cumulative output is
                -- already >= 50% (catches noisy photos with a flat top border
                -- without aborting flat slides that contain a small photo).
                !badCum = o * 100 >= i * 95
                !badWin = prevI > 0 && o * 2 >= i && do_ * 100 >= di * 98
            in if badCum || badWin
                 then pure (o, acc, nb, True)
                 else go i o acc nb (i + checkAt) i o
        | i <= lim4 && i < maxPos = do
            let !w32 = read4 src i
                !h   = hash4 w32
            !prev32 <- unsafeRead ht h
            unsafeWrite ht h (fromIntegral i :: Int32)
            let !prev = fromIntegral prev32 :: Int
                !dist = i - prev
            if prev >= 0 && dist >= 1 && dist <= 32768 && read4 src prev == w32
              then do
                let !maxL = min 258 (n - i)
                    !mlen = extendMatch src prev i 4 maxL
                    !lc   = unsafeAt lenCodeTbl (mlen - 3)
                    !lnb  = fromIntegral (lc `shiftR` 24) :: Int
                    !lbt  = fromIntegral (lc .&. 0xFFFFFF) :: Word64
                    !dc   = distCode dist
                    !dnb  = fromIntegral (dc `shiftR` 24) :: Int
                    !dbt  = fromIntegral (dc .&. 0xFFFFFF) :: Word64
                    -- nb <= 31, lnb <= 13, dnb <= 18 => total <= 62 < 64.
                    !acc1 = acc .|. (lbt `shiftL` nb) .|. (dbt `shiftL` (nb + lnb))
                    !nb1  = nb + lnb + dnb
                    !nextI = i + mlen
                -- Refresh hash entries at the tail of the match so consecutive
                -- flat 1-byte or 4-byte RGBA runs reference distance 1 or 4
                -- (4-bit code, 0 extra bits) rather than distance 258.
                if mlen > 4 && nextI - 4 <= lim4
                  then unsafeWrite ht (hash4 (read4 src (nextI - 4)))
                                      (fromIntegral (nextI - 4) :: Int32)
                  else pure ()
                if nextI - 1 <= lim4
                  then unsafeWrite ht (hash4 (read4 src (nextI - 1)))
                                      (fromIntegral (nextI - 1) :: Int32)
                  else pure ()
                if nb1 >= 32
                  then do
                    writeWord32LE outPtr o acc1
                    go nextI (o + 4) (acc1 `shiftR` 32) (nb1 - 32) nextChk prevI prevO
                  else
                    go nextI o acc1 nb1 nextChk prevI prevO
              else do
                let !b    = fromIntegral (BU.unsafeIndex src i) :: Int
                    !lc   = unsafeAt litCodeTbl b
                    !lnb  = fromIntegral (lc `shiftR` 24) :: Int
                    !lbt  = fromIntegral (lc .&. 0xFFFFFF) :: Word64
                    !acc1 = acc .|. (lbt `shiftL` nb)
                    !nb1  = nb + lnb
                if nb1 >= 32
                  then do
                    writeWord32LE outPtr o acc1
                    go (i + 1) (o + 4) (acc1 `shiftR` 32) (nb1 - 32) nextChk prevI prevO
                  else
                    go (i + 1) o acc1 nb1 nextChk prevI prevO
        | i < n = do
            -- Trailing 1..3 bytes (or beyond Int32 window): emit as literals.
            let !b    = fromIntegral (BU.unsafeIndex src i) :: Int
                !lc   = unsafeAt litCodeTbl b
                !lnb  = fromIntegral (lc `shiftR` 24) :: Int
                !lbt  = fromIntegral (lc .&. 0xFFFFFF) :: Word64
                !acc1 = acc .|. (lbt `shiftL` nb)
                !nb1  = nb + lnb
            if nb1 >= 32
              then do
                writeWord32LE outPtr o acc1
                go (i + 1) (o + 4) (acc1 `shiftR` 32) (nb1 - 32) nextChk prevI prevO
              else
                go (i + 1) o acc1 nb1 nextChk prevI prevO
        | otherwise = pure (o, acc, nb, False)
  go 0 o0 hdrAcc hdrBits checkAt 0 o0

-- Extend a match that already agrees on its first @m@ bytes (4) up to @maxL@.
{-# INLINE extendMatch #-}
extendMatch :: BS.ByteString -> Int -> Int -> Int -> Int -> Int
extendMatch !src !prev !i !m0 !maxL = go4 m0
  where
    go4 !m
      | m + 4 <= maxL && read4 src (prev + m) == read4 src (i + m) = go4 (m + 4)
      | otherwise = go1 m
    go1 !m
      | m < maxL && BU.unsafeIndex src (prev + m) == BU.unsafeIndex src (i + m) = go1 (m + 1)
      | otherwise = m

newHashTable :: IO (IOUArray Int Int32)
newHashTable = newArray (0, hashSize - 1) (-1)
