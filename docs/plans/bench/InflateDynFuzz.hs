{-# LANGUAGE BangPatterns #-}
-- Differential fuzz harness for Cmedit.Inflate.inflateDyn against the
-- original 4f3c80e implementation (InflateRef4f3c80e.inflateDyn).
-- Verifies bit-for-bit identical Either String ByteString results on:
--   - valid streams (text, binaries, levels 0..9, stored/fixed/dynamic)
--   - every truncation of small streams
--   - bit flips at many offsets
--   - random maxOut caps (including mid-match and buffer-growth boundaries)
--   - random startBytes (negative, 0, mid-stream, >= len)
--   - handcrafted edge cases (short packed literals near EOF, distances < 0,
--     stored blocks, invalid codes/trees, random garbage)
module Main where

import Control.Monad (forM_, when)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.Word (Word64, Word8)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

import qualified Cmedit.Inflate as New
import qualified InflateRef4f3c80e as Ref

lcg :: Word64 -> Word64
lcg x = x * 6364136223846793005 + 1442695040888963407

rngSeq :: Word64 -> [Word64]
rngSeq seed = tail (iterate lcg (seed * 2654435761 + 1))

flipBit :: BS.ByteString -> Int -> Int -> BS.ByteString
flipBit bs byteIdx bitIdx
  | byteIdx < 0 || byteIdx >= BS.length bs = bs
  | otherwise =
      let b  = BS.index bs byteIdx
          b' = b `xor` (1 `shiftL` (bitIdx .&. 7))
      in BS.concat [BS.take byteIdx bs, BS.singleton b', BS.drop (byteIdx + 1) bs]

firstDiff :: BS.ByteString -> BS.ByteString -> Maybe (Int, Maybe Word8, Maybe Word8)
firstDiff a b = go 0
  where
    la = BS.length a
    lb = BS.length b
    m  = min la lb
    go !i
      | i >= m = if la == lb then Nothing
                 else Just (i, if i < la then Just (BS.index a i) else Nothing
                             , if i < lb then Just (BS.index b i) else Nothing)
      | BS.index a i /= BS.index b i = Just (i, Just (BS.index a i), Just (BS.index b i))
      | otherwise = go (i + 1)

check :: IORef Int -> String -> BS.ByteString -> Int -> Int -> IO ()
check cntRef label bs startByte maxOut = do
  modifyIORef' cntRef (+ 1)
  let rRef = Ref.inflateDyn bs startByte maxOut
      rNew = New.inflateDyn bs startByte maxOut
  when (rRef /= rNew) $ do
    putStrLn $ "MISMATCH [" ++ label ++ "] len=" ++ show (BS.length bs)
            ++ " startByte=" ++ show startByte ++ " maxOut=" ++ show maxOut
    case (rRef, rNew) of
      (Left e1, Left e2) -> do
        putStrLn $ "  Ref: Left " ++ show e1
        putStrLn $ "  New: Left " ++ show e2
      (Left e1, Right b2) -> do
        putStrLn $ "  Ref: Left " ++ show e1
        putStrLn $ "  New: Right (len=" ++ show (BS.length b2) ++ ") head=" ++ show (BS.unpack (BS.take 32 b2))
      (Right b1, Left e2) -> do
        putStrLn $ "  Ref: Right (len=" ++ show (BS.length b1) ++ ") head=" ++ show (BS.unpack (BS.take 32 b1))
        putStrLn $ "  New: Left " ++ show e2
      (Right b1, Right b2) -> do
        putStrLn $ "  Ref: Right len=" ++ show (BS.length b1)
        putStrLn $ "  New: Right len=" ++ show (BS.length b2)
        putStrLn $ "  First diff: " ++ show (firstDiff b1 b2)
    hFlush stdout
    exitFailure

-- Build LSB-first bitstream from (value, nbits) pairs.
packBits :: [(Int, Int)] -> BS.ByteString
packBits = BS.pack . finish . foldl step (0 :: Int, 0 :: Int, [])
  where
    step (!acc, !nb, !out) (val, k) = drain (acc .|. ((val .&. ((1 `shiftL` k) - 1)) `shiftL` nb)) (nb + k) out
    drain !acc !nb !out
      | nb >= 8   = drain (acc `shiftR` 8) (nb - 8) (fromIntegral (acc .&. 255) : out)
      | otherwise = (acc, nb, out)
    finish (!acc, !nb, !out)
      | nb > 0    = reverse (fromIntegral (acc .&. 255) : out)
      | otherwise = reverse out

revBits :: Int -> Int -> Int
revBits len code = go 0 0 code
  where
    go !i !acc !c
      | i >= len  = acc
      | otherwise = go (i + 1) ((acc `shiftL` 1) .|. (c .&. 1)) (c `shiftR` 1)

-- Handcrafted fixed-Huffman stream with repeated byte / pattern matches to test
-- mid-match maxOut caps and distance-before-0 behavior.
mkFixedMatchStream :: Int -> Int -> Int -> BS.ByteString
mkFixedMatchStream prefixLen matchLenCode distCode =
  let litBits sym
        | sym <= 143 = (revBits 8 (0x30 + sym), 8)
        | otherwise  = (revBits 9 (0x190 + (sym - 144)), 9)
      lenBits sym
        | sym <= 279 = (revBits 7 (sym - 256), 7)
        | otherwise  = (revBits 8 (0xC0 + (sym - 280)), 8)
      lits = [ litBits (65 + (i `mod` 26)) | i <- [0 .. prefixLen - 1] ]
      eob  = (revBits 7 0, 7)
  in packBits $
       [(1, 1), (1, 2)] -- bfinal=1, btype=1 (fixed)
       ++ lits
       ++ [lenBits matchLenCode, (revBits 5 distCode, 5)]
       ++ [eob]

-- Handcrafted dynamic block with 2-bit literals (triggers 4-literal multi-symbol
-- packing in the 10-bit root table) to stress-test truncation at every bit/byte!
mkShortLitDynamic :: Int -> BS.ByteString
mkShortLitDynamic count =
  -- Define lit/len lengths: sym 0..2 get len=2 (codes 00, 01, 10), sym 256 gets len=2 (code 11)
  -- Actually: 3 symbols at len=2 (00, 01, 10) + EOB (256) at len=2 (11) = complete tree!
  -- Code-length tree: sym 0 (len=0) -> cl=2, sym 2 (len=2) -> cl=2, sym 18 (repeat 0) -> cl=1
  -- Wait: sym 18: 1 bit (0), sym 0: 2 bits (10), sym 2: 2 bits (11) -> complete!
  -- clOrder = [16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15]
  -- Need up to index 15 (sym 2) -> nclen = 16 -> hclen = 12
  -- nlit = 257 (hlit = 0), ndist = 1 (hdist = 0)
  let clLens = [0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2] -- 16 entries
      -- cl codes: sym 18 -> 0 (1b), sym 0 -> 10 (2b, LSB 01), sym 2 -> 11 (2b, LSB 11)
      cl18 = (0, 1)
      cl0  = (revBits 2 2, 2)
      cl2  = (revBits 2 3, 2)
      -- Emit allLens (258 entries):
      -- sym 0,1,2 -> 2 (3x cl2)
      -- sym 3..255 -> 253 zeros = 138 (cl18, 127) + 115 (cl18, 104)
      -- sym 256 -> 2 (cl2)
      -- dist 0 -> 0 (cl0)
      lensBits = [cl2, cl2, cl2, cl18, (127, 7), cl18, (104, 7), cl2, cl0]
      -- Lit codes (len=2): sym 0 -> 00, sym 1 -> 01 (rev 10), sym 2 -> 10 (rev 01), sym 256 -> 11
      dataBits = [ (revBits 2 (i `mod` 3), 2) | i <- [0 .. count - 1] ] ++ [(3, 2)]
  in packBits $
       [(1, 1), (2, 2), (0, 5), (0, 5), (12, 4)]
       ++ [(c, 3) | c <- clLens]
       ++ lensBits
       ++ dataBits

main :: IO ()
main = do
  args <- getArgs
  (corpusDir, nFuzz) <- case args of
    [d]    -> pure (d, 60)
    [d, n] -> pure (d, read n)
    _      -> do
      putStrLn "Usage: InflateDynFuzz CORPUS_DIR [SEED_COUNT]"
      exitFailure

  cntRef <- newIORef (0 :: Int)
  files <- sort <$> listDirectory corpusDir

  -- 1. Valid corpus streams: full, edge maxOut, random maxOut, random startBytes
  forM_ files $ \fname -> do
    bs <- BS.readFile (corpusDir </> fname)
    let isZlib = ".z" `isSuffixOf` fname
        s0     = if isZlib then 2 else 0
        len    = BS.length bs
        full   = Ref.inflateDyn bs s0 (64 * 1024 * 1024)
        expLen = either (const 0) BS.length full
        rs     = rngSeq (fromIntegral (sum (map fromIntegral (BS.unpack (BS.take 16 bs))) + len * 31))

    -- Full decode and exact-size boundaries
    check cntRef (fname ++ ":full") bs s0 (64 * 1024 * 1024)
    forM_ [-5, -1, 0, 1, 2, 3, 4, 7, 8, 15, 16, 255, 257, 258, 259,
           273, 274, 275, 4095, 4096, 4097,
           4096 - 275, 4096 - 274, 4096 - 258, 8192 - 274, 8192 - 258, 8192,
           expLen - 1, expLen, expLen + 1,
           4 * len - 275, 4 * len - 258, 4 * len, 4 * len + 1] $ \mo ->
      check cntRef (fname ++ ":edge-maxOut=" ++ show mo) bs s0 mo

    -- Random maxOut caps (many hitting mid-match or mid-growth)
    forM_ (take 25 (zip [0 :: Int ..] rs)) $ \(idx, r) -> do
      let spanLen = max 32 (expLen + 64)
          mo = fromIntegral (r `mod` fromIntegral spanLen)
      check cntRef (fname ++ ":rand-maxOut#" ++ show idx) bs s0 mo

    -- Random and boundary startBytes
    let edgeStarts = [-17, -5, -1, 0, 1, 2, 3, len - 2, len - 1, len, len + 1, len + 37]
        randStarts = [ fromIntegral (r `mod` fromIntegral (max 1 (len + 16))) - 8
                     | r <- take 15 (drop 30 rs) ]
    forM_ (edgeStarts ++ randStarts) $ \sb ->
      check cntRef (fname ++ ":startByte=" ++ show sb) bs sb 65536

    -- Seeded multi-bit flips (+ optional truncation + random maxOut + random startByte)
    forM_ [1 .. nFuzz] $ \seed -> do
      let mrs = rngSeq (fromIntegral seed * 1000003 + fromIntegral len)
          nFlips = 1 + fromIntegral (head mrs `mod` 6)
          flipped = foldl (\b r ->
                      if len == 0 then b
                      else let bi = fromIntegral ((r `shiftR` 16) `mod` fromIntegral len)
                               bt = fromIntegral ((r `shiftR` 8) .&. 7)
                           in flipBit b bi bt)
                    bs (take nFlips (tail mrs))
          cutMode = mrs !! 10 `mod` 3
          mut = if cutMode == 0 || len == 0 then flipped
                else BS.take (fromIntegral ((mrs !! 11) `mod` fromIntegral (len + 1))) flipped
          sb = if (mrs !! 12 `mod` 5) == 0
                 then fromIntegral ((mrs !! 13) `mod` fromIntegral (max 1 (BS.length mut + 8))) - 4
                 else s0
          mo = case mrs !! 14 `mod` 4 of
                 0 -> 65536
                 1 -> fromIntegral ((mrs !! 15) `mod` 1024)
                 2 -> fromIntegral ((mrs !! 15) `mod` 16384)
                 _ -> fromIntegral ((mrs !! 15) `mod` 262144)
      check cntRef (fname ++ ":fuzz#" ++ show seed) mut sb mo

  -- 2. Exhaustive truncations and bit-flips on small streams
  let smallFiles = filter (".small." `isInfixOf`) files
  forM_ smallFiles $ \fname -> do
    bs <- BS.readFile (corpusDir </> fname)
    let isZlib = ".z" `isSuffixOf` fname
        s0     = if isZlib then 2 else 0
        len    = BS.length bs
    -- Every truncation 0 .. len
    forM_ [0 .. len] $ \t -> do
      let tr = BS.take t bs
      check cntRef (fname ++ ":trunc=" ++ show t) tr s0 262144
      when (t `mod` 7 == 0) $
        forM_ [0, 1, 3, 17, 258, 4096] $ \mo ->
          check cntRef (fname ++ ":trunc=" ++ show t ++ ":mo=" ++ show mo) tr s0 mo
    -- Single-bit flips across many offsets (every bit of first 48 bytes + stride across rest)
    let offsets = [0 .. min (len - 1) 47]
               ++ [ i | i <- [48, 55 .. len - 1] ]
               ++ [ max 0 (len - 16) .. len - 1 ]
    forM_ offsets $ \bi ->
      forM_ [0 .. 7] $ \bt ->
        check cntRef (fname ++ ":bitflip@" ++ show bi ++ "." ++ show bt)
              (flipBit bs bi bt) s0 65536

  -- 3. Handcrafted adversarial & boundary cases
  -- 3a. Empty & all 256 single-byte streams
  check cntRef "empty:0" BS.empty 0 1024
  check cntRef "empty:-3" BS.empty (-3) 1024
  check cntRef "empty:1" BS.empty 1 1024
  forM_ [0 .. 255 :: Word8] $ \b ->
    forM_ [0, 1, 1024] $ \mo ->
      check cntRef ("1byte:" ++ show b ++ ":mo=" ++ show mo) (BS.singleton b) 0 mo

  -- 3b. Stored block edge cases (truncated, oversized len, multi-block, negative startByte)
  let storedZero    = BS.pack [0x01, 0x00, 0x00, 0xFF, 0xFF]
      storedHuge    = BS.pack [0x01, 0xFF, 0xFF, 0x00, 0x00, 0x41, 0x42, 0x43]
      storedTwo     = BS.pack [0x00, 0x03, 0x00, 0xFC, 0xFF, 65, 66, 67,
                               0x01, 0x04, 0x00, 0xFB, 0xFF, 68, 69, 70, 71]
      storedNonFin  = BS.pack [0x00, 0x05, 0x00, 0xFA, 0xFF, 1, 2]
      invalidBtype3 = BS.pack [0x07, 0x00, 0x00]
      invalidBtype3After = BS.pack [0x00, 0x02, 0x00, 0xFD, 0xFF, 65, 66, 0x07]
  forM_ [("storedZero", storedZero), ("storedHuge", storedHuge), ("storedTwo", storedTwo),
         ("storedNonFin", storedNonFin), ("btype3", invalidBtype3),
         ("btype3After", invalidBtype3After)] $ \(nm, bs) -> do
    forM_ [0 .. BS.length bs] $ \t ->
      forM_ [-2, 0, 1, 2, 5, 4095, 4096, 4097, 70000] $ \mo ->
        check cntRef (nm ++ ":t=" ++ show t ++ ":mo=" ++ show mo) (BS.take t bs) 0 mo

  -- 3c. Fixed match streams: mid-match maxOut caps & distances reaching before 0
  let matchCases =
        [ ("distBefore0-empty", mkFixedMatchStream 0 265 4)   -- match at o=0, d=5
        , ("distBefore0-short", mkFixedMatchStream 2 285 5)   -- match at o=2, d=7, len=258
        , ("rle-d1-len258",     mkFixedMatchStream 1 285 0)   -- d=1, len=258
        , ("rle-d2-len258",     mkFixedMatchStream 2 285 1)   -- d=2, len=258
        , ("rle-d3-len258",     mkFixedMatchStream 3 285 2)   -- d=3, len=258
        , ("rle-d4-len258",     mkFixedMatchStream 4 285 3)   -- d=4, len=258
        , ("invLen286-o0",      mkFixedMatchStream 0 286 0)   -- invalid length code at o=0
        , ("invLen286-o3",      mkFixedMatchStream 3 286 0)   -- invalid length code at o=3
        , ("invDist30-o0",      mkFixedMatchStream 0 257 30)  -- invalid dist code at o=0
        , ("invDist30-o3",      mkFixedMatchStream 3 257 30)  -- invalid dist code at o=3
        ]
  forM_ matchCases $ \(nm, bs) -> do
    forM_ [0 .. BS.length bs] $ \t ->
      check cntRef (nm ++ ":t=" ++ show t) (BS.take t bs) 0 4096
    forM_ [0, 1, 2, 3, 4, 5, 7, 8, 9, 15, 16, 17, 100, 257, 258, 259, 260, 262, 300] $ \mo ->
      check cntRef (nm ++ ":mo=" ++ show mo) bs 0 mo

  -- 3d. Short 2-bit literals dynamic block (multi-literal packing) truncated at EVERY byte
  -- and with single-bit flips at the tail!
  forM_ [1, 2, 3, 4, 5, 7, 8, 15, 16, 31, 32, 33, 64, 300, 4100] $ \cnt -> do
    let bs = mkShortLitDynamic cnt
        len = BS.length bs
    forM_ [0 .. len] $ \t ->
      check cntRef ("shortLit(" ++ show cnt ++ "):t=" ++ show t) (BS.take t bs) 0 16384
    forM_ [max 0 (len - 6) .. len - 1] $ \bi ->
      forM_ [0 .. 7] $ \bt ->
        check cntRef ("shortLit(" ++ show cnt ++ "):flip@" ++ show bi ++ "." ++ show bt)
              (flipBit bs bi bt) 0 16384
    forM_ [0, 1, 2, 3, 4, 5, cnt - 2, cnt - 1, cnt, cnt + 1] $ \mo ->
      check cntRef ("shortLit(" ++ show cnt ++ "):mo=" ++ show mo) bs 0 mo

  -- 3e. Pure random garbage streams
  forM_ [1 .. 400 :: Int] $ \seed -> do
    let rs = rngSeq (fromIntegral seed * 999983 + 17)
        len = fromIntegral (head rs `mod` 512)
        bs = BS.pack [ fromIntegral ((r `shiftR` 24) .&. 255) | r <- take len (tail rs) ]
        sb = if seed `mod` 10 == 0 then -3 else 0
        mo = fromIntegral ((rs !! 3) `mod` 8192)
    check cntRef ("garbage#" ++ show seed) bs sb mo

  -- 3f. Repeated all-0x00 and all-0xFF streams
  forM_ [1, 2, 3, 4, 7, 8, 9, 15, 16, 32, 64] $ \len -> do
    check cntRef ("zeros:" ++ show len) (BS.replicate len 0x00) 0 8192
    check cntRef ("ones:" ++ show len)  (BS.replicate len 0xFF) 0 8192

  total <- readIORef cntRef
  putStrLn $ "PASS: " ++ show total ++ " differential inflateDyn checks matched 4f3c80e identically."
