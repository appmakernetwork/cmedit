{-# LANGUAGE BangPatterns #-}
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
    -- * Canonical Huffman tables (shared with JPEG)
  , Huff(..)
  , buildHuff
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Array (Array)
import qualified Data.Array as A
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Array.MArray (freeze)
import Data.Array.Unboxed (UArray, (!))
import qualified Data.Array.Unboxed as U
import Data.Bits
import Data.STRef
import Data.Word (Word8)
import qualified Data.ByteString as BS

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
      syms   = [ s | l <- [1..maxLen], (s,ll) <- zip [0..] lens, ll == l ]
      nsym   = length syms
  in Huff counts (A.listArray (0, max 0 (nsym-1)) (syms ++ [0])) maxLen

-- Slots of the inflate reader's unboxed state array.
iBit, iOut :: Int
iBit = 0   -- bit position in the compressed stream
iOut = 1   -- next output byte index

-- | Inflate a raw DEFLATE stream starting at byte @startByte@, producing up to
-- @outSize@ bytes (the caller knows the size; PNG does). A hard error (invalid
-- Huffman code or block type) yields @Left@ so corrupt input is reported up
-- front rather than rendered as garbage; a merely-short stream is tolerated
-- (zero-padded) so slightly-truncated-but-valid files still display.
inflate :: BS.ByteString -> Int -> Int -> Either String (UArray Int Word8)
inflate dat startByte outSize = runST $ do
  (arrRef, _, err) <- inflateWith dat startByte outSize False
  arr <- readSTRef arrRef
  frozen <- freeze arr
  pure (maybe (Right frozen) Left err)

-- | Inflate a stream of unknown output size, stopping at @maxOut@ bytes.
--
-- The bound is not a nicety: a compressed stream names no expanded size, so a
-- hostile or corrupt one could otherwise ask for unbounded memory — the same
-- reasoning as "Cmedit.Pager"'s per-line cap. Hitting it truncates rather than
-- failing, since a partial content stream still renders most of a page.
inflateDyn :: BS.ByteString -> Int -> Int -> Either String BS.ByteString
inflateDyn dat startByte maxOut = runST $ do
  (arrRef, n, err) <- inflateWith dat startByte maxOut True
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
inflateWith
  :: BS.ByteString -> Int -> Int -> Bool
  -> ST s (STRef s (STUArray s Int Word8), Int, Maybe String)
inflateWith dat startByte limit grow = do
  let cap0 | grow      = min limit (max 4096 (4 * BS.length dat))
           | otherwise = limit
  arr0   <- newArray (0, max 0 (cap0 - 1)) 0 :: ST s (STUArray s Int Word8)
  arrRef <- newSTRef arr0
  capRef <- newSTRef (max 1 cap0)
  -- Bit position and output position share one unboxed array: they are read
  -- and written on every bit and every output byte respectively, and an
  -- 'STRef Int' allocates a fresh box on each write.
  st     <- newArray (0, 1) 0 :: ST s (STUArray s Int Int)
  writeArray st iBit (startByte * 8)
  doneRef <- newSTRef False
  errRef  <- newSTRef (Nothing :: Maybe String)
  let fail' msg = writeSTRef errRef (Just msg) >> writeSTRef doneRef True
      -- Direct bounds check rather than a Maybe-returning helper: this runs
      -- once per bit of the compressed stream (tens of millions of times for a
      -- photo-sized PNG), so the Just was a heap allocation per bit.
      datLen = BS.length dat
      getBit = do
        p <- readArray st iBit
        let i    = p `shiftR` 3
            byte = if i >= 0 && i < datLen then fromIntegral (BS.index dat i) else 0
            b    = (byte `shiftR` (p .&. 7)) .&. 1
        writeArray st iBit (p+1)
        pure b
      getBits n = go 0 0
        where go !i !acc | i >= n = pure acc
                         | otherwise = do b <- getBit; go (i+1) (acc .|. (b `shiftL` i))
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
        !o <- readArray st iOut
        cap <- readSTRef capRef
        when (grow && o >= cap && o < limit) (growTo (o + 1))
        cap' <- readSTRef capRef
        when (o < cap') $ do
          a <- readSTRef arrRef
          writeArray a o w
        writeArray st iOut (o+1)
      copyBack !dist !len = forLoop 1 len $ \_ -> do
        !o <- readArray st iOut
        cap <- readSTRef capRef
        !v <- if o - dist >= 0 && o - dist < cap
                then do a <- readSTRef arrRef; readArray a (o-dist)
                else pure 0
        putByte v
      decodeSym (Huff counts syms maxLen) = go 1 0 0 0
        where go !len !code !first !index
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
                o <- readArray st iOut
                if o >= limit then pure () else do
                  sym <- decodeSym lit
                  if sym < 0 then fail' "invalid Huffman code"
                  else if sym < 256 then putByte (fromIntegral sym) >> loop
                  else if sym == 256 then pure ()
                  else do
                    let li = sym - 257
                    if li >= nLenCodes then fail' "invalid length code" else do
                      extra <- getBits (lenExtra ! li)
                      let len = lenBase ! li + extra
                      dsym <- decodeSym dist
                      if dsym < 0 || dsym >= nDistCodes then fail' "invalid distance code" else do
                        dextra <- getBits (distExtra ! dsym)
                        let d = distBase ! dsym + dextra
                        copyBack d len
                        loop
      storedBlock = do
        p <- readArray st iBit
        writeArray st iBit ((p + 7) .&. complement 7)   -- align to byte
        len <- getBits 16
        _nlen <- getBits 16
        forLoop 1 len $ \_ -> do v <- getBits 8; putByte (fromIntegral v)
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
        o <- readArray st iOut
        if done || o >= limit then pure () else do
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
  written <- readArray st iOut
  cap <- readSTRef capRef
  pure (arrRef, max 0 (min cap written), err)

-- | A strict counted loop; @forM_ [lo..hi]@ builds a list and a closure per
-- element, which is real allocation in a per-byte loop.
forLoop :: Int -> Int -> (Int -> ST s ()) -> ST s ()
forLoop lo hi act = go lo
  where go !i | i > hi = pure ()
              | otherwise = act i >> go (i + 1)
{-# INLINE forLoop #-}

-- Fixed Huffman tables (RFC 1951 §3.2.6).
fixedLit :: Huff
fixedLit = buildHuff ([8 | _ <- [0..143 :: Int]] ++ [9 | _ <- [144..255 :: Int]]
                   ++ [7 | _ <- [256..279 :: Int]] ++ [8 | _ <- [280..287 :: Int]])

fixedDist :: Huff
fixedDist = buildHuff (replicate 30 5)

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
