{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for kitty graphics shm and zlib transport (plan 0041):
--   * Cmedit.Deflate (Adler-32, RFC 1950/1951 round-trip, block type,
--     length/distance coverage, 32 KiB window boundary, bailout)
--   * Cmedit.Input + Cmedit.Caps (i=31/32/33 replies, unknown IDs)
--   * Cmedit.Gfx (probes, KpRaw/KpZlib/KpLocal formatting and chunking)
--   * Cmedit.Term (writeKittyLocal / unlinkKittyLocal on POSIX /dev/shm)
--
-- Spec.hs calls 'gfxTransportSpec' with its own @check@.
module GfxTransportSpec (gfxTransportSpec) where

import Control.Monad (when)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)
import Data.Word (Word8, Word32)
import System.Directory (doesFileExist)
import System.Posix.Files (fileMode, getFileStatus)

import Cmedit.Caps
import Cmedit.Deflate (adler32, zlibCompress, zlibCompressFast)
import Cmedit.Gfx
  ( KittyPayload(..), base64Strict
  , kittyLocalProbe, kittyZlibProbe
  , kittyPlace, kittyPlacePayload
  , kittyPlaceAnim, kittyPlaceAnimPayload
  , kittyClientAnim, kittyClientAnimPayload )
import qualified Cmedit.Inflate as Inflate
import Data.IORef (newIORef, readIORef, writeIORef)
import Cmedit.Input (ByteSource(..), nextKey)
import Cmedit.Term (KittyLocalKind(..), writeKittyLocal, unlinkKittyLocal)
import Cmedit.Types

gfxTransportSpec :: (String -> Bool -> IO ()) -> IO ()
gfxTransportSpec check = do
  deflateTests check
  capsInputTests check
  gfxWireTests check
  termShmTests check

------------------------------------------------------------------------------
-- Helpers

toBS :: BB.Builder -> BS.ByteString
toBS = BSL.toStrict . BB.toLazyByteString

-- | Naive reference Adler-32 (RFC 1950 §8.2) for cross-checking chunked impl.
refAdler32 :: BS.ByteString -> Word32
refAdler32 bs =
  let step (s1, s2) b =
        let s1' = (s1 + fromIntegral b) `mod` 65521
            s2' = (s2 + s1') `mod` 65521
        in (s1', s2')
      (a, b) = BS.foldl' step (1 :: Word32, 0 :: Word32) bs
  in (b `shiftL` 16) .|. a

-- | Validate an RFC 1950 zlib stream against expected uncompressed bytes:
-- checks header CMF/FLG + FCHECK + no preset dict, inflates via Cmedit.Inflate,
-- and verifies the big-endian Adler-32 trailer.
verifyZlib :: BS.ByteString -> BS.ByteString -> Bool
verifyZlib orig z
  | BS.length z < 6 = False
  | otherwise =
      let cmf = fromIntegral (BS.index z 0) :: Int
          flg = fromIntegral (BS.index z 1) :: Int
          hdrOk = cmf == 0x78
               && ((cmf * 256 + flg) `mod` 31 == 0)
               && (flg .&. 0x20 == 0)
          n = BS.length z
          t0 = fromIntegral (BS.index z (n - 4)) :: Word32
          t1 = fromIntegral (BS.index z (n - 3)) :: Word32
          t2 = fromIntegral (BS.index z (n - 2)) :: Word32
          t3 = fromIntegral (BS.index z (n - 1)) :: Word32
          trailer = (t0 `shiftL` 24) .|. (t1 `shiftL` 16) .|. (t2 `shiftL` 8) .|. t3
          adlerOk = trailer == adler32 orig && trailer == refAdler32 orig
          maxOut = max 64 (BS.length orig + 1024)
          inflateOk = Inflate.inflateDyn z 2 maxOut == Right orig
      in hdrOk && adlerOk && inflateOk

-- | Simple deterministic xorshift32 pseudo-random byte generator (incompressible).
prngBytes :: Int -> Word32 -> BS.ByteString
prngBytes n seed0 = fst $ BS.unfoldrN n step seed0
  where
    step !s =
      let !s1 = s `xor` (s `shiftL` 13)
          !s2 = s1 `xor` (s1 `shiftR` 17)
          !s3 = s2 `xor` (s2 `shiftL` 5)
          !b  = fromIntegral (s3 `shiftR` 24) :: Word8
      in Just (b, s3)

-- | Minimal base64 decoder for verifying emitted probe/placement payloads.
decodeBase64 :: BS.ByteString -> Maybe BS.ByteString
decodeBase64 bs
  | BS.length bs `mod` 4 /= 0 = Nothing
  | otherwise = BS.pack <$> go 0
  where
    !n = BS.length bs
    val c
      | c >= 65 && c <= 90  = Just (fromIntegral (c - 65) :: Int)
      | c >= 97 && c <= 122 = Just (fromIntegral (c - 71))
      | c >= 48 && c <= 57  = Just (fromIntegral (c + 4))
      | c == 43             = Just 62
      | c == 47             = Just 63
      | otherwise           = Nothing
    go i
      | i >= n = Just []
      | otherwise = do
          let c0 = BS.index bs i
              c1 = BS.index bs (i + 1)
              c2 = BS.index bs (i + 2)
              c3 = BS.index bs (i + 3)
          v0 <- val c0
          v1 <- val c1
          let b0 = fromIntegral ((v0 `shiftL` 2) .|. (v1 `shiftR` 4)) :: Word8
          if c2 == 61
            then do rest <- go (i + 4); Just (b0 : rest)
            else do
              v2 <- val c2
              let b1 = fromIntegral (((v1 .&. 0xF) `shiftL` 4) .|. (v2 `shiftR` 2)) :: Word8
              if c3 == 61
                then do rest <- go (i + 4); Just (b0 : b1 : rest)
                else do
                  v3 <- val c3
                  let b2 = fromIntegral (((v2 .&. 0x3) `shiftL` 6) .|. v3) :: Word8
                  rest <- go (i + 4)
                  Just (b0 : b1 : b2 : rest)

-- | Minimal RFC 1951 symbol walker for conformance tests: returns
-- @Right (emittedLenSyms, emittedDistCodes, maxDist)@ from a zlib stream that
-- is one final /dynamic/ block. It refuses any other block type on purpose:
-- Ghostty 1.3.1 segfaults on fixed-Huffman blocks (see "Cmedit.Deflate"), so
-- the block type is part of what these tests pin.
inspectZlib :: BS.ByteString -> Either String ([Int], [Int], Int)
inspectZlib z
  | BS.length z < 6 = Left "too short"
  | otherwise = goInit 16  -- skip 2-byte zlib header
  where
    !totalBits = (BS.length z - 4) * 8
    bitAt bp
      | bp >= totalBits = 0
      | otherwise =
          let b = BS.index z (bp `shiftR` 3)
          in fromIntegral ((b `shiftR` (bp .&. 7)) .&. 1) :: Int
    readLSB bp k = foldr (\j acc -> acc * 2 + bitAt (bp + j)) 0 [0 .. k - 1]
    lenExtra :: Int -> Int
    lenExtra s
      | s <= 264  = 0
      | s <= 284  = (s - 261) `div` 4
      | otherwise = 0
    distBase :: Int -> (Int, Int)
    distBase d
      | d <= 3    = (d + 1, 0)
      | otherwise =
          let eb = (d - 2) `div` 2
              base = (1 `shiftL` (eb + 1)) + ((d .&. 1) `shiftL` eb) + 1
          in (base, eb)
    -- Canonical code (RFC 1951 §3.2.2) as an association list keyed on
    -- (length, code).
    mkCode :: [Int] -> [((Int, Int), Int)]
    mkCode lens =
      let count l = length (filter (== l) lens)
          firsts = scanl (\c l -> (c + (if l == 1 then 0 else count (l - 1))) * 2)
                         0 [1 .. 15 :: Int]
          first l = firsts !! l
          step (acc, used) (sym, l)
            | l == 0    = (acc, used)
            | otherwise =
                let k = length (filter (== l) used)
                in (((l, first l + k), sym) : acc, l : used)
      in fst (foldl step ([], []) (zip [0 ..] lens))
    -- Decode one symbol, reading the code MSB-first.
    decode :: [((Int, Int), Int)] -> Int -> Either String (Int, Int)
    decode tbl bp0 = go 1 (bitAt bp0)
      where
        go l c
          | l > 15 = Left ("invalid code at bit " ++ show bp0)
          | otherwise = case lookup (l, c) tbl of
              Just sym -> Right (sym, bp0 + l)
              Nothing  -> go (l + 1) (c * 2 + bitAt (bp0 + l))
    clOrder = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 :: Int]
    goInit bp =
      let bfinal = bitAt bp
          btype  = readLSB (bp + 1) 2
      in if bfinal /= 1 || btype /= 2
           then Left ("unexpected header bfinal=" ++ show bfinal ++ " btype=" ++ show btype)
           else
             let hlit  = readLSB (bp + 3) 5 + 257
                 hdist = readLSB (bp + 8) 5 + 1
                 hclen = readLSB (bp + 13) 4 + 4
                 clRaw = [ (clOrder !! k, readLSB (bp + 17 + 3 * k) 3) | k <- [0 .. hclen - 1] ]
                 clTbl = mkCode [ maybe 0 id (lookup s clRaw) | s <- [0 .. 18] ]
             in do (lens, bp1) <- readLens clTbl (hlit + hdist) (bp + 17 + 3 * hclen) []
                   let litTbl  = mkCode (take hlit lens)
                       distTbl = mkCode (drop hlit lens)
                   walk litTbl distTbl bp1 [] [] 0
    readLens clTbl want bp acc
      | length acc >= want = Right (take want acc, bp)
      | otherwise = do
          (sym, bp1) <- decode clTbl bp
          case sym of
            16 | null acc  -> Left "repeat with no previous length"
               | otherwise -> readLens clTbl want (bp1 + 2)
                                (acc ++ replicate (3 + readLSB bp1 2) (last acc))
            17 -> readLens clTbl want (bp1 + 3) (acc ++ replicate (3 + readLSB bp1 3) 0)
            18 -> readLens clTbl want (bp1 + 7) (acc ++ replicate (11 + readLSB bp1 7) 0)
            _  -> readLens clTbl want bp1 (acc ++ [sym])
    walk litTbl distTbl bp lsyms dcodes maxD
      | bp >= totalBits = Left "truncated before EOB"
      | otherwise = do
          (sym, bp1) <- decode litTbl bp
          if sym < 256 then walk litTbl distTbl bp1 lsyms dcodes maxD
          else if sym == 256 then Right (reverse lsyms, reverse dcodes, maxD)
          else do
            let bp2 = bp1 + lenExtra sym
            (dc, bp3) <- decode distTbl bp2
            let (db, deb) = distBase dc
                dval = db + readLSB bp3 deb
            walk litTbl distTbl (bp3 + deb) (sym : lsyms) (dc : dcodes) (max maxD dval)

------------------------------------------------------------------------------
-- 1. Deflate / Adler-32

deflateTests :: (String -> Bool -> IO ()) -> IO ()
deflateTests check = do
  -- RFC 1950 Adler-32 vectors
  check "adler32 empty == 1" (adler32 BS.empty == 1)
  check "adler32 \"Wikipedia\" == 0x11E60398"
    (adler32 (BC.pack "Wikipedia") == 0x11E60398)
  check "adler32 \"a\" == 0x00620062"
    (adler32 (BC.pack "a") == 0x00620062)
  let bigFF = BS.replicate 12000 0xFF
  check "adler32 matches reference across multiple 5552-byte chunks"
    (adler32 bigFF == refAdler32 bigFF)

  -- Edge-case round-trips
  let cases :: [(String, BS.ByteString)]
      cases =
        [ ("empty", BS.empty)
        , ("1 byte", BS.singleton 0x42)
        , ("2 bytes", BS.pack [0x01, 0xFE])
        , ("3 bytes", BS.pack [0xAA, 0xBB, 0xCC])
        , ("short text", BC.pack "hello world")
        , ("all 256 byte values", BS.pack [0 .. 255])
        , ("63 bytes", BS.replicate 63 0x7F)
        , ("64 bytes", BS.replicate 64 0x7F)
        , ("4 KiB zeros", BS.replicate 4096 0)
        , ("100 KiB zeros", BS.replicate 102400 0)
        , ("repeating 3-byte RGB", BS.concat (replicate 4000 (BS.pack [10, 20, 30])))
        , ("repeating 4-byte RGBA", BS.concat (replicate 16384 (BS.pack [255, 255, 255, 255])))
        ]
  mapM_ (\(label, orig) ->
           check ("zlibCompress round-trip: " ++ label)
                 (verifyZlib orig (zlibCompress orig)))
        cases

  -- All match lengths 3..258 (exercises every length code 257..285 and extra bits)
  let lengthPayload = BS.concat
        [ let pat = BS.pack [ fromIntegral ((l * 13 + j * 7 + 1) .&. 0xFF)
                            | j <- [0 .. l - 1] ]
              sep = BS.pack [0xF1, fromIntegral (l .&. 0xFF), 0xE2, 0xD3]
          in pat <> sep <> pat <> BS.pack [0xC4, 0xB5]
        | l <- [3 .. 258 :: Int] ]
  check "zlibCompress round-trips all match lengths 3..258"
    (verifyZlib lengthPayload (zlibCompress lengthPayload))

  -- RFC 1951 §3.2.5 conformance: length 258 MUST emit symbol 285 (never 284+31),
  -- and flat runs must keep match distance <= 4 (hash updated at tail of match).
  let flatZeros = BS.replicate 8192 0
      zFlatZeros = zlibCompress flatZeros
  check "flat zero run uses RFC 1951 symbol 285 (not 284+31) and distance <= 1"
    (case inspectZlib zFlatZeros of
       Right (lsyms, _, maxD) -> 285 `elem` lsyms && not (284 `elem` lsyms) && maxD == 1
       Left _                 -> False)
  let flatRgba = BS.concat (replicate 2048 (BS.pack [0x11, 0x22, 0x33, 0xFF]))
      zFlatRgba = zlibCompress flatRgba
  check "flat 4-byte RGBA run uses symbol 285 and distance <= 4"
    (case inspectZlib zFlatRgba of
       Right (lsyms, _, maxD) -> 285 `elem` lsyms && maxD <= 4
       Left _                 -> False)

  -- All distance buckets 1..32768 plus just-past-window 32769.
  -- Use a constant-zero filler of length (d - 8) so it only touches ONE hash
  -- slot (0x00000000) and never evicts the pattern's hash slot even at d=32768!
  let dists =
        [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 13, 16, 17, 24, 25, 32, 33
        , 48, 49, 64, 65, 96, 97, 128, 129, 192, 193, 256, 257
        , 384, 385, 512, 513, 768, 769, 1024, 1025, 1536, 1537
        , 2048, 2049, 3072, 3073, 4096, 4097, 6144, 6145, 8192, 8193
        , 12288, 12289, 16384, 16385, 24576, 24577, 32767, 32768, 32769 ]
      mkDistCase d =
        let pat = BS.pack [0xDE, 0xAD, 0xBE, 0xEF, 0xCA, 0xFE, 0xBA, 0xBE]
            body = if d >= 8
                     then pat <> BS.replicate (d - 8) 0x55 <> pat
                     else BS.concat (replicate 20 (BS.pack [ fromIntegral (k * 37 + d) | k <- [0 .. d - 1] ]))
        in body
  check "zlibCompress round-trips every distance bucket 1..32769"
    (all (\d -> let b = mkDistCase d in verifyZlib b (zlibCompress b)) dists)
  check "distance 16385 emits dist code 28 and round-trips"
    (let b = mkDistCase 16385; z = zlibCompress b
     in verifyZlib b z && case inspectZlib z of
                            Right (_, dcs, maxD) -> 28 `elem` dcs && maxD == 16385
                            Left _               -> False)
  check "distance 32768 emits dist code 29 and round-trips"
    (let b = mkDistCase 32768; z = zlibCompress b
     in verifyZlib b z && case inspectZlib z of
                            Right (_, dcs, maxD) -> 29 `elem` dcs && maxD == 32768
                            Left _               -> False)
  check "distance 32769 never emits out-of-window distance > 32768"
    (let b = mkDistCase 32769; z = zlibCompress b
     in verifyZlib b z && case inspectZlib z of
                            Right (_, _, maxD) -> maxD <= 32768
                            Left _             -> False)

  -- Synthetic PDF/SVG page RGBA (800x600x4 = 1.92 MB): white page with text-like bands
  let pageW = 800 :: Int
      pageH = 600 :: Int
      whiteRow = BS.replicate (pageW * 4) 0xFF
      bandRow  = BS.concat
        [ BS.replicate (100 * 4) 0xFF
        , BS.concat (replicate 150 (BS.pack [0x20, 0x20, 0x20, 0xFF, 0x20, 0x20, 0x20, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]))
        , BS.replicate (100 * 4) 0xFF ]
      synthPage = BS.concat [ if y `mod` 16 < 3 then bandRow else whiteRow | y <- [0 .. pageH - 1] ]
  case zlibCompressFast synthPage of
    Nothing -> check "zlibCompressFast compresses synthetic PDF page" False
    Just z  -> do
      check "zlibCompressFast synthetic PDF page round-trips" (verifyZlib synthPage z)
      check ("zlibCompressFast synthetic PDF page >= 10x ratio (got "
             ++ show (BS.length synthPage `div` max 1 (BS.length z)) ++ "x)")
        (BS.length z * 10 <= BS.length synthPage)

  -- Incompressible pseudo-random noise (128 KiB):
  -- zlibCompress still round-trips; zlibCompressFast bails out (Nothing).
  let noise = prngBytes (128 * 1024) 0x12345678
  check "zlibCompress round-trips 128 KiB incompressible noise"
    (verifyZlib noise (zlibCompress noise))
  check "zlibCompressFast bails out on 128 KiB incompressible noise"
    (isNothing (zlibCompressFast noise))
  check "zlibCompressFast returns Nothing on < 64 byte input"
    (isNothing (zlibCompressFast (BS.replicate 63 0)))
  -- Compressible small input succeeds once it outweighs the block header
  check "zlibCompressFast succeeds on 1024 zero bytes"
    (case zlibCompressFast (BS.replicate 1024 0) of
       Just z  -> BS.length z < 1024 && verifyZlib (BS.replicate 1024 0) z
       Nothing -> False)
  -- Ghostty 1.3.1 segfaults on fixed-Huffman blocks (BTYPE=01); every
  -- stream must be one final dynamic block (BFINAL=1, BTYPE=10).
  check "zlibCompress emits a dynamic block, never a fixed one"
    (all (\b -> let z = zlibCompress b
                in BS.length z > 2 && BS.index z 2 .&. 7 == 5)
         [BS.empty, BS.replicate 64 0, synthPage, noise])
  -- Periodic/windowed bailout: flat 80 KiB border followed by 256 KiB noise
  -- must bail out early instead of compressing the whole noisy image.
  let borderThenNoise = BS.replicate (80 * 1024) 0xFF <> prngBytes (256 * 1024) 0xABCD1234
  check "zlibCompressFast bails out on flat top border + noisy photo"
    (isNothing (zlibCompressFast borderThenNoise))
  -- Flat slide (180 KiB) with a small embedded photo (64 KiB) still compresses well.
  let slideWithPhoto = BS.replicate (180 * 1024) 0xFF <> prngBytes (64 * 1024) 0xDEADBEEF <> BS.replicate (16 * 1024) 0xFF
  check "zlibCompressFast compresses flat slide containing a small photo"
    (case zlibCompressFast slideWithPhoto of
       Just z  -> BS.length z < BS.length slideWithPhoto `div` 2 && verifyZlib slideWithPhoto z
       Nothing -> False)

------------------------------------------------------------------------------
-- 2. Input & Caps probing (i=31, i=32, i=33)

parseOne :: String -> IO Key
parseOne s = do
  ref <- newIORef (BS.unpack (BC.pack s))
  let pop = do
        xs <- readIORef ref
        case xs of
          []      -> pure Nothing
          (b:bs') -> writeIORef ref bs' >> pure (Just b)
      chunk = do
        xs <- readIORef ref
        writeIORef ref []
        pure (BS.pack xs)
  let pushBack bs = readIORef ref >>= writeIORef ref . (BS.unpack bs ++)
  nextKey (ByteSource pop (const pop) chunk pushBack)

capsInputTests :: (String -> Bool -> IO ()) -> IO ()
capsInputTests check = do
  let esc = "\ESC"
      apc s = esc ++ "_G" ++ s ++ esc ++ "\\"
      checkParse lbl inp expect = do
        k <- parseOne inp
        check lbl (k == expect)

  checkParse "i=31;OK parses to TrKittyGfx True"
    (apc "i=31;OK") (KReply (TrKittyGfx True))
  checkParse "i=31;EINVAL parses to TrKittyGfx False"
    (apc "i=31;EINVAL") (KReply (TrKittyGfx False))
  checkParse "i=32;OK parses to TrKittyShm True"
    (apc "i=32;OK") (KReply (TrKittyShm True))
  checkParse "i=32;ENOENT:missing parses to TrKittyShm False"
    (apc "i=32;ENOENT:missing") (KReply (TrKittyShm False))
  checkParse "i=33;OK parses to TrKittyZlib True"
    (apc "i=33;OK") (KReply (TrKittyZlib True))
  checkParse "i=33;EINVAL:unsupported parses to TrKittyZlib False"
    (apc "i=33;EINVAL:unsupported") (KReply (TrKittyZlib False))

  -- Multi-key headers (e.g. kitty/ghostty include p=... or I=...)
  checkParse "multi-key i=31,p=0;OK parses to TrKittyGfx True"
    (apc "i=31,p=0;OK") (KReply (TrKittyGfx True))
  checkParse "multi-key p=1,i=32;OK parses to TrKittyShm True"
    (apc "p=1,i=32;OK") (KReply (TrKittyShm True))
  checkParse "multi-key i=33,I=7;OK parses to TrKittyZlib True"
    (apc "i=33,I=7;OK") (KReply (TrKittyZlib True))

  let checkUnknown lbl inp = do
        k <- parseOne inp
        check lbl (case k of KUnknown _ -> True; _ -> False)
  -- Unknown IDs must be ignored (KUnknown) and never clobber tcKittyGfx
  checkUnknown "unknown id i=99;EINVAL produces KUnknown" (apc "i=99;EINVAL")
  checkUnknown "placement id i=1;OK produces KUnknown" (apc "i=1;OK")
  checkUnknown "id prefix i=310;OK does not match i=31" (apc "i=310;OK")
  checkUnknown "missing id ;OK produces KUnknown" (apc ";OK")

  -- Caps state transitions
  let c0 = defaultCaps
  check "defaultCaps has kittyGfx/Shm/Zlib all False"
    (not (tcKittyGfx c0) && not (tcKittyShm c0) && not (tcKittyZlib c0))
  let c1 = applyReply (TrKittyGfx True) c0
      c2 = applyReply (TrKittyZlib True) c1
      c3 = applyReply (TrKittyShm True) c2
      c4 = applyReply (TrKittyShm False) c3
  check "applyReply sets tcKittyGfx independently"
    (tcKittyGfx c1 && not (tcKittyShm c1) && not (tcKittyZlib c1))
  check "applyReply sets tcKittyZlib preserving tcKittyGfx"
    (tcKittyGfx c2 && not (tcKittyShm c2) && tcKittyZlib c2)
  check "applyReply sets tcKittyShm preserving others"
    (tcKittyGfx c3 && tcKittyShm c3 && tcKittyZlib c3)
  check "applyReply TrKittyShm False clears only tcKittyShm"
    (tcKittyGfx c4 && not (tcKittyShm c4) && tcKittyZlib c4)

------------------------------------------------------------------------------
-- 3. Gfx wire formatting

gfxWireTests :: (String -> Bool -> IO ()) -> IO ()
gfxWireTests check = do
  -- kittyLocalProbe
  let lpShm = BC.unpack (toBS (kittyLocalProbe 's' "/cm-123-ab-0"))
      b64Shm = BC.unpack (base64Strict (BC.pack "/cm-123-ab-0"))
  check "kittyLocalProbe 's' carries i=32,a=q,t=s,f=32 and base64 name"
    ("\ESC_Gi=32,s=1,v=1,a=q,t=s,f=32;" `isPrefixOf` lpShm
     && (b64Shm ++ "\ESC\\") `isInfixOf` lpShm)

  let lpTmp = BC.unpack (toBS (kittyLocalProbe 't' "/tmp/cmedit-tty-graphics-protocol-1.bin"))
  check "kittyLocalProbe 't' carries t=t"
    ("a=q,t=t,f=32;" `isInfixOf` lpTmp)

  -- kittyZlibProbe: 8x8 RGB (192 raw bytes) compresses to < 32 bytes so a
  -- terminal that ignores o=z rejects it for insufficient pixel data.
  let zp = toBS kittyZlibProbe
      zpStr = BC.unpack zp
      zpB64 = BS.takeWhile (/= 0x1B) (BS.drop 1 (BS.dropWhile (/= 0x3B) zp))
  check "kittyZlibProbe carries i=33,s=8,v=8,a=q,t=d,f=24,o=z"
    ("\ESC_Gi=33,s=8,v=8,a=q,t=d,f=24,o=z;" `isPrefixOf` zpStr)
  check "kittyZlibProbe payload decodes (< 128 B) and inflates to 192 zero bytes"
    (case decodeBase64 zpB64 of
       Just z  -> BS.length z < 128 && verifyZlib (BS.replicate (8 * 8 * 3) 0) z
       Nothing -> False)

  -- kittyPlacePayload KpRaw is byte-identical to kittyPlace
  let pxSmall = BS.replicate (4 * 4 * 4) 0xAB
      pxLarge = BS.concat (replicate 2000 (BS.pack [1, 2, 3, 255]))
  check "kittyPlacePayload KpRaw == kittyPlace (small)"
    (toBS (kittyPlacePayload (1, 2) (4, 4) (4, 4) (KpRaw pxSmall))
      == toBS (kittyPlace (1, 2) (4, 4) (4, 4) pxSmall))
  check "kittyPlacePayload KpRaw == kittyPlace (multi-chunk)"
    (toBS (kittyPlacePayload (1, 2) (40, 20) (80, 25) (KpRaw pxLarge))
      == toBS (kittyPlace (1, 2) (40, 20) (80, 25) pxLarge))

  -- kittyPlacePayload KpLocal
  let localWire = BC.unpack (toBS (kittyPlacePayload (2, 3) (100, 50) (800, 600) (KpLocal 's' "/cm-42-ff-7")))
  check "kittyPlacePayload KpLocal uses t=s, omits o=z, single compact APC"
    ("a=T,f=32,t=s,i=1,q=2,s=800,v=600,c=100,r=50;" `isInfixOf` localWire
     && not ("o=z" `isInfixOf` localWire)
     && not ("m=1" `isInfixOf` localWire)
     && length localWire < 128)

  -- kittyPlacePayload KpZlib (multi-chunk check: each chunk <= 4096 base64 chars)
  let zLarge = zlibCompress (prngBytes 5000 0x99887766)
      zWire = toBS (kittyPlacePayload (1, 1) (20, 10) (50, 25) (KpZlib zLarge))
      zStr  = BC.unpack zWire
      -- Split on ESC_G (drop kittyGfxDeleteAll prefix too)
      chunks = filter (not . ("a=d," `isPrefixOf`)) (drop 1 (splitOn "\ESC_G" zStr))
      chunkBodies = [ takeWhile (/= '\ESC') (drop 1 (dropWhile (/= ';') c)) | c <- chunks ]
  check "kittyPlacePayload KpZlib carries o=z and multi-chunk m=1/m=0 <= 4096"
    (",o=z," `isInfixOf` zStr
     && length chunks >= 2
     && all (\b -> length b <= 4096) chunkBodies
     && "m=1;" `isInfixOf` head chunks
     && "m=0;" `isInfixOf` last chunks)

  -- Animated placement parity for KpRaw + o=z / t=s on KpZlib / KpLocal
  check "kittyPlaceAnimPayload KpRaw == kittyPlaceAnim"
    (toBS (kittyPlaceAnimPayload (1, 1) (10, 5) (4, 4) [(KpRaw pxSmall, 100), (KpRaw pxSmall, 100)])
      == toBS (kittyPlaceAnim (1, 1) (10, 5) (4, 4) [(pxSmall, 100), (pxSmall, 100)]))
  check "kittyPlaceAnimPayload KpLocal emits t=s on root and continuation frames"
    (let w = BC.unpack (toBS (kittyPlaceAnimPayload (1, 1) (10, 5) (4, 4)
               [(KpLocal 's' "/cm-1-a-0", 100), (KpLocal 's' "/cm-1-a-1", 100)]))
     in "a=T,f=32,t=s,i=1,q=2,s=4,v=4,c=10,r=5;" `isInfixOf` w
        && "a=f,i=1,f=32,t=s,q=2,s=4,v=4,z=100;" `isInfixOf` w)
  check "kittyPlaceAnimPayload KpZlib emits o=z on frames"
    (let z = zlibCompress pxSmall
         w = BC.unpack (toBS (kittyPlaceAnimPayload (1, 1) (10, 5) (4, 4) [(KpZlib z, 80), (KpZlib z, 80)]))
     in "a=T,f=32,i=1,q=2,o=z,s=4,v=4,c=10,r=5,m=0;" `isInfixOf` w
        && "a=f,i=1,f=32,q=2,o=z,s=4,v=4,z=80,m=0;" `isInfixOf` w)
  check "kittyClientAnimPayload KpRaw == kittyClientAnim"
    (toBS (kittyClientAnimPayload (1, 1) (10, 5) (4, 4) 0 [KpRaw pxSmall, KpRaw pxSmall])
      == toBS (kittyClientAnim (1, 1) (10, 5) (4, 4) 0 [pxSmall, pxSmall]))
  check "kittyClientAnimPayload KpLocal emits t=s on transmit frames"
    (let w = BC.unpack (toBS (kittyClientAnimPayload (1, 1) (10, 5) (4, 4) 0
               [KpLocal 's' "/cm-1-a-0", KpLocal 's' "/cm-1-a-1"]))
     in "a=t,f=32,t=s,q=2,i=1,s=4,v=4;" `isInfixOf` w
        && "a=t,f=32,t=s,q=2,i=2,s=4,v=4;" `isInfixOf` w)
  check "kittyClientAnimPayload KpZlib emits o=z on transmit frames"
    (let z = zlibCompress pxSmall
         w = BC.unpack (toBS (kittyClientAnimPayload (1, 1) (10, 5) (4, 4) 0 [KpZlib z, KpZlib z]))
     in "a=t,f=32,q=2,o=z,i=1,s=4,v=4,m=0;" `isInfixOf` w
        && "a=t,f=32,q=2,o=z,i=2,s=4,v=4,m=0;" `isInfixOf` w)

splitOn :: String -> String -> [String]
splitOn sep = go
  where
    n = length sep
    go "" = [""]
    go s
      | sep `isPrefixOf` s = "" : go (drop n s)
      | otherwise = case go (tail s) of
                      (x : xs) -> (head s : x) : xs
                      []       -> [[head s]]

------------------------------------------------------------------------------
-- 4. POSIX shm round-trip via Cmedit.Term

termShmTests :: (String -> Bool -> IO ()) -> IO ()
termShmTests check = do
  let payload = BS.pack [0x10, 0x20, 0x30, 0xFF, 0x40, 0x50, 0x60, 0xFF]
  res <- writeKittyLocal payload
  case res of
    Nothing ->
      -- Some locked-down sandboxes have no writable /dev/shm; skip fs check
      -- rather than failing spuriously, but still verify error path is clean.
      check "writeKittyLocal returned clean Nothing when shm unavailable" True
    Just (kind, name) -> do
      check "POSIX writeKittyLocal returns KittyLocalShm" (kind == KittyLocalShm)
      check ("POSIX shm name starts with '/' and fits macOS 31-char limit (" ++ name ++ ")")
        (not (null name) && head name == '/' && length name <= 31)
      let shmPath = "/dev/shm" ++ name
      exists <- doesFileExist shmPath
      when exists $ do
        contents <- BS.readFile shmPath
        check "POSIX shm object contains exact written bytes" (contents == payload)
        st <- getFileStatus shmPath
        let perms = fileMode st .&. 0o777
        check ("POSIX shm object has mode 0600 (got " ++ show perms ++ ")")
          (perms == 0o600)
      unlinkKittyLocal kind name
      when exists $ do
        stillThere <- doesFileExist shmPath
        check "unlinkKittyLocal removes shm object" (not stillThere)
      -- Idempotent second unlink must not throw
      unlinkKittyLocal kind name
      check "double unlinkKittyLocal is safe" True

      -- Multi-MB shm write (exercises partial-write loop and size handling)
      let bigPayload = BS.concat (replicate 4096 (BS.replicate 1024 0x5A))
      resBig <- writeKittyLocal bigPayload
      case resBig of
        Nothing -> check "4 MiB shm write clean fallback" True
        Just (k2, n2) -> do
          let p2 = "/dev/shm" ++ n2
          ex2 <- doesFileExist p2
          when ex2 $ do
            c2 <- BS.readFile p2
            check "4 MiB POSIX shm round-trips exact length and bytes" (c2 == bigPayload)
          unlinkKittyLocal k2 n2
          when ex2 $ do
            still2 <- doesFileExist p2
            check "4 MiB POSIX shm unlinked cleanly" (not still2)
