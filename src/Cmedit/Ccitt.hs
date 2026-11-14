{-# LANGUAGE BangPatterns #-}
-- | CCITT fax decoding (ITU-T T.4 and T.6), as PDF's @CCITTFaxDecode@ filter
-- defines it: Group 3 one-dimensional (Modified Huffman, @K = 0@), Group 3
-- two-dimensional (Modified READ, @K > 0@) and Group 4 (MMR, @K < 0@).
--
-- A leaf module (base, bytestring, array): the PDF object layer runs it as an
-- ordinary filter, so a scanned page's bilevel image and the rare stream
-- that is fax-encoded for no good reason both go through it.
--
-- __The algorithm__ is the one xpdf, poppler and pdf.js share, because it is
-- the one that survives real files: each row is a list of /changing
-- elements/ (the columns where the colour flips), the previous row's list is
-- the reference line the two-dimensional modes are relative to, and a row is
-- decoded by appending to its list. Codes are read through 13-bit lookahead
-- tables built once from the T.4 code lists below.
--
-- __Robustness.__ Fax data in the wild is damaged, padded, truncated and
-- mislabelled, and the right response to all of it is to keep going: an
-- invalid run code consumes one bit and counts as a one-pixel run, an invalid
-- mode code ends the row (white to the margin) and counts it as damaged, and
-- with @EndOfLine@ the decoder resynchronises on the next EOL marker. Every
-- loop consumes input or ends the row, the output is capped by the caller's
-- byte limit, and running out of data ends the image with the rows decoded so
-- far. @DamagedRowsBeforeError@ is reported, not enforced: every PDF viewer
-- shows a damaged scan rather than refusing it, and so does this one.
module Cmedit.Ccitt
  ( CcittParams(..)
  , defaultCcitt
  , CcittResult(..)
  , decodeCcitt
  , maxCcittColumns
    -- * Code tables (exported for tests)
  , whiteCodes
  , blackCodes
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)
import Data.Array.ST (STUArray, newArray, runSTUArray)
import Data.Array.Unboxed (UArray, accumArray)
import qualified Data.Array.Unboxed as U
import Data.Bits (shiftL, shiftR, (.&.), (.|.), xor, complement)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import Data.Array.Unsafe (unsafeFreeze)
import Data.Word (Word8)
import Control.Monad.ST (runST)

-- | The filter's @\/DecodeParms@.
data CcittParams = CcittParams
  { ccK           :: !Int    -- ^ < 0 Group 4; 0 Group 3 1-D; > 0 Group 3 mixed 1-D/2-D.
  , ccColumns     :: !Int    -- ^ Pixels per row (default 1728).
  , ccRows        :: !Int    -- ^ Rows expected; 0 = until the data ends.
  , ccBlackIs1    :: !Bool   -- ^ Output 1 bits for black (default: 0 is black).
  , ccByteAlign   :: !Bool   -- ^ @EncodedByteAlign@: every row starts on a byte boundary.
  , ccEndOfLine   :: !Bool   -- ^ EOL markers are required before rows.
  , ccEndOfBlock  :: !Bool   -- ^ The data ends with an EOFB/RTC marker (default true).
  , ccDamagedRows :: !Int    -- ^ @DamagedRowsBeforeError@ (reported only; see the module header).
  } deriving (Eq, Show)

defaultCcitt :: CcittParams
defaultCcitt = CcittParams 0 1728 0 False False False True 0

-- | What came out: packed rows (@(columns + 7) div 8@ bytes each, MSB
-- first), how many, and how many of them were damaged.
data CcittResult = CcittResult
  { crRows    :: !Int
  , crData    :: !BS.ByteString
  , crDamaged :: !Int
  } deriving (Show)

-- | Widest row accepted (a 600 dpi scan of a 4-metre banner is 94 000).
maxCcittColumns :: Int
maxCcittColumns = 1000000

------------------------------------------------------------------------------
-- Code tables (ITU-T T.4, tables 2 and 3)

-- | Terminating (0–63) and make-up (64–1728) white codes, plus the extended
-- make-up codes (1792–2560) both colours share: @(run length, code)@ with
-- the code written as its bit string.
whiteCodes :: [(Int, String)]
whiteCodes =
  zip [0 ..]
    [ "00110101", "000111", "0111", "1000", "1011", "1100", "1110", "1111"
    , "10011", "10100", "00111", "01000", "001000", "000011", "110100", "110101"
    , "101010", "101011", "0100111", "0001100", "0001000", "0010111", "0000011", "0000100"
    , "0101000", "0101011", "0010011", "0100100", "0011000", "00000010", "00000011", "00011010"
    , "00011011", "00010010", "00010011", "00010100", "00010101", "00010110", "00010111", "00101000"
    , "00101001", "00101010", "00101011", "00101100", "00101101", "00000100", "00000101", "00001010"
    , "00001011", "01010010", "01010011", "01010100", "01010101", "00100100", "00100101", "01011000"
    , "01011001", "01011010", "01011011", "01001010", "01001011", "00110010", "00110011", "00110100" ]
  ++ zip [64, 128 ..]
    [ "11011", "10010", "010111", "0110111", "00110110", "00110111", "01100100", "01100101"
    , "01101000", "01100111", "011001100", "011001101", "011010010", "011010011", "011010100", "011010101"
    , "011010110", "011010111", "011011000", "011011001", "011011010", "011011011", "010011000", "010011001"
    , "010011010", "011000", "010011011" ]
  ++ extendedMakeup

blackCodes :: [(Int, String)]
blackCodes =
  zip [0 ..]
    [ "0000110111", "010", "11", "10", "011", "0011", "0010", "00011"
    , "000101", "000100", "0000100", "0000101", "0000111", "00000100", "00000111", "000011000"
    , "0000010111", "0000011000", "0000001000", "00001100111", "00001101000", "00001101100", "00000110111", "00000101000"
    , "00000010111", "00000011000", "000011001010", "000011001011", "000011001100", "000011001101", "000001101000", "000001101001"
    , "000001101010", "000001101011", "000011010010", "000011010011", "000011010100", "000011010101", "000011010110", "000011010111"
    , "000001101100", "000001101101", "000011011010", "000011011011", "000001010100", "000001010101", "000001010110", "000001010111"
    , "000001100100", "000001100101", "000001010010", "000001010011", "000000100100", "000000110111", "000000111000", "000000100111"
    , "000000101000", "000001011000", "000001011001", "000000101011", "000000101100", "000001011010", "000001100110", "000001100111" ]
  ++ zip [64, 128 ..]
    [ "0000001111", "000011001000", "000011001001", "000001011011", "000000110011", "000000110100", "000000110101", "0000001101100"
    , "0000001101101", "0000001001010", "0000001001011", "0000001001100", "0000001001101", "0000001110010", "0000001110011", "0000001110100"
    , "0000001110101", "0000001110110", "0000001110111", "0000001010010", "0000001010011", "0000001010100", "0000001010101", "0000001011010"
    , "0000001011011", "0000001100100", "0000001100101" ]
  ++ extendedMakeup

extendedMakeup :: [(Int, String)]
extendedMakeup = zip [1792, 1856 ..]
  [ "00000001000", "00000001100", "00000001101", "000000010010", "000000010011", "000000010100", "000000010101"
  , "000000010110", "000000010111", "000000011100", "000000011101", "000000011110", "000000011111" ]

-- Two-dimensional mode codes.
mPass, mHoriz, mV0, mVR1, mVR2, mVR3, mVL1, mVL2, mVL3 :: Int
mPass = 1; mHoriz = 2; mV0 = 3; mVR1 = 4; mVR2 = 5; mVR3 = 6; mVL1 = 7; mVL2 = 8; mVL3 = 9

modeCodes :: [(Int, String)]
modeCodes =
  [ (mPass, "0001"), (mHoriz, "001"), (mV0, "1"), (mVR1, "011"), (mVR2, "000011")
  , (mVR3, "0000011"), (mVL1, "010"), (mVL2, "000010"), (mVL3, "0000010") ]

-- A 13-bit lookahead table: every 13-bit window whose prefix is a code maps
-- to @(length << 16) .|. (value + 1)@; 0 means no code.
lookahead :: [(Int, String)] -> UArray Int Int
lookahead codes = accumArray (\_ v -> v) 0 (0, 8191)
  [ (prefix `shiftL` (13 - len) + k, (len `shiftL` 16) .|. (v + 1))
  | (v, bits) <- codes
  , let len = length bits
        prefix = foldl (\a c -> a * 2 + (if c == '1' then 1 else 0)) 0 bits
  , k <- [0 .. (1 `shiftL` (13 - len)) - 1] ]

whiteTable, blackTable, modeTable :: UArray Int Int
whiteTable = lookahead whiteCodes
blackTable = lookahead blackCodes
modeTable = lookahead modeCodes
{-# NOINLINE whiteTable #-}
{-# NOINLINE blackTable #-}
{-# NOINLINE modeTable #-}

------------------------------------------------------------------------------
-- Decoding

-- | Decode fax data, producing at most @limit@ bytes of rows.
decodeCcitt :: CcittParams -> Int -> BS.ByteString -> CcittResult
decodeCcitt p limit dat
  | columns < 1 || columns > maxCcittColumns || limit < rowBytes = CcittResult 0 BS.empty 0
  | otherwise = runST $ do
      let maxRows0 = limit `div` rowBytes
          maxRows = if ccRows p > 0 then min (ccRows p) maxRows0 else maxRows0
      -- Rows are written into a buffer that grows by doubling, up to the cap.
      outRef <- newSTRef =<< newArray (0, rowBytes * min maxRows 64 - 1) 0
      capRef <- newSTRef (min maxRows 64)
      coding <- newArray (0, columns + 8) 0 :: ST s (STUArray s Int Int)
      ref    <- newArray (0, columns + 8) 0 :: ST s (STUArray s Int Int)
      unsafeWrite coding 0 columns           -- the line before the first: all white
      st <- newSTRef (Dec 0 False False 0 (ccK p < 0))
      -- Leading fill and an initial EOL.
      skipZeros st
      d0 <- readSTRef st
      when (look12 (dPos d0) == 1) $ eat st 12
      when (ccK p > 0) $ readTag st

      let loop !row
            | row >= maxRows = pure row
            | otherwise = do
                d <- readSTRef st
                if dEof d || dPos d >= nbits then pure row else do
                  -- G4's end-of-block where a row would start.
                  if ccK p < 0 && look 24 (dPos d) == 0x001001
                    then pure row
                    else do
                      decodeRow st coding ref (dNext2D d)
                      cap <- readSTRef capRef
                      when (row >= cap) $ do
                        let cap' = min maxRows (2 * cap)
                        old <- readSTRef outRef
                        new <- newArray (0, rowBytes * cap' - 1) 0
                        let copy i | i >= rowBytes * cap = pure ()
                                   | otherwise = unsafeRead old i >>= unsafeWrite new i >> copy (i + 1)
                        copy 0
                        writeSTRef outRef new
                        writeSTRef capRef cap'
                      out <- readSTRef outRef
                      emitRow coding out (row * rowBytes)
                      afterRow st row
                      loop (row + 1)
      nRows <- loop 0
      out <- readSTRef outRef
      bytes <- freezeBytes out (nRows * rowBytes)
      d <- readSTRef st
      pure (CcittResult nRows bytes (dDamaged d))
  where
    columns = ccColumns p
    rowBytes = (columns + 7) `div` 8
    nbits = 8 * BS.length dat
    whiteByte = if ccBlackIs1 p then 0 else 0xff :: Word8

    byteAt i = if i < BS.length dat then fromIntegral (BSU.unsafeIndex dat i) else 0 :: Int
    -- The next @n@ (1..24) bits at bit position @pos@, zero past the end.
    look :: Int -> Int -> Int
    look n pos =
      let b = pos `shiftR` 3
          w = (byteAt b `shiftL` 24) .|. (byteAt (b + 1) `shiftL` 16)
              .|. (byteAt (b + 2) `shiftL` 8) .|. byteAt (b + 3)
      in (w `shiftR` (32 - (pos .&. 7) - n)) .&. ((1 `shiftL` n) - 1)
    -- 12 bits, or -1 at the end of the data (xpdf's EOF).
    look12 pos = if pos >= nbits then -1 else look 12 pos

    eat st n = modifySTRef' st (\d -> d { dPos = dPos d + n })

    skipZeros st = do
      d <- readSTRef st
      let go pos | pos < nbits && look 12 pos == 0 = go (pos + 1)
                 | otherwise = pos
      writeSTRef st d { dPos = go (dPos d) }

    readTag st = do
      d <- readSTRef st
      if dPos d >= nbits then writeSTRef st d { dEof = True }
      else writeSTRef st d { dPos = dPos d + 1, dNext2D = look 1 (dPos d) == 0 }

    -- A run code of either colour: the run length, or -1 at the end of the
    -- data. An invalid code eats one bit and is a run of one, so the caller
    -- always makes progress (xpdf's rule).
    runCode :: STRefDec s -> UArray Int Int -> ST s Int
    runCode st tbl = do
      d <- readSTRef st
      let pos = dPos d
      if pos >= nbits then pure (-1) else do
        let e = tbl `unsafeAt` look 13 pos
        if e == 0
          then do writeSTRef st d { dPos = pos + 1, dErr = True }; pure 1
          else do writeSTRef st d { dPos = pos + (e `shiftR` 16) }; pure ((e .&. 0xffff) - 1)

    -- A whole run: make-up codes until a terminating one.
    fullRun st black = go 0 (0 :: Int)
      where
        go !acc !k = do
          c <- runCode st (if black then blackTable else whiteTable)
          if c < 0 then pure (acc, True)
          else if c >= 64 && k < 100000 then go (acc + c) (k + 1)
          else pure (acc + c, False)

    -- xpdf's addPixels / addPixelsNeg over the coding line.
    addPixels coding posRef a1' black = do
      cp <- readSTRef posRef
      cur <- unsafeRead coding cp
      when (a1' > cur) $ do
        let a1 = min columns a1'
        let cp' = if ((cp .&. 1) `xor` black) /= 0 then cp + 1 else cp
        when (cp' <= columns + 4) $ do
          unsafeWrite coding cp' a1
          writeSTRef posRef cp'
    addPixelsNeg coding posRef a1' black = do
      cp <- readSTRef posRef
      cur <- unsafeRead coding cp
      if a1' > cur then addPixels coding posRef a1' black
      else when (a1' < cur) $ do
        let a1 = max 0 a1'
            back k = if k > 0 then do prev <- unsafeRead coding (k - 1)
                                      if a1 < prev then back (k - 1) else pure k
                     else pure k
        k <- back cp
        unsafeWrite coding k a1
        writeSTRef posRef k

    decodeRow st coding ref twoD = do
      posRef <- newSTRef (0 :: Int)
      if twoD then do
        -- The previous row becomes the reference line, terminated twice.
        let copy i = do v <- unsafeRead coding i
                        if v < columns && i < columns + 2
                          then unsafeWrite ref i v >> copy (i + 1)
                          else do unsafeWrite ref i columns; unsafeWrite ref (i + 1) columns
        copy 0
        unsafeWrite coding 0 0
        refPos <- newSTRef (0 :: Int)
        blackRef <- newSTRef (0 :: Int)
        let rd i = if i < 0 || i > columns + 4 then pure columns else unsafeRead ref i
            curA = readSTRef posRef >>= unsafeRead coding
            -- Advance refPos past a0, two entries at a time (same colour).
            settle = do
              a0 <- curA
              let go rp = do v <- rd rp
                             if v <= a0 && v < columns then go (rp + 2) else pure rp
              rp <- readSTRef refPos
              go rp >>= writeSTRef refPos
            vert d = do
              rp <- readSTRef refPos
              b1 <- rd rp
              bp <- readSTRef blackRef
              if d >= 0 then addPixels coding posRef (b1 + d) bp
                        else addPixelsNeg coding posRef (b1 + d) bp
              writeSTRef blackRef (bp `xor` 1)
              a0 <- curA
              when (a0 < columns) $ do
                if d > 0 || d == 0 then writeSTRef refPos (rp + 1)
                else writeSTRef refPos (if rp > 0 then rp - 1 else rp + 1)
                settle
            rowLoop !guard = do
              a0 <- curA
              when (a0 < columns && guard > 0) $ do
                d <- readSTRef st
                let pos = dPos d
                if pos >= nbits
                  then do addPixels coding posRef columns 0; writeSTRef st d { dEof = True }
                  else do
                    let e = modeTable `unsafeAt` look 13 pos
                    if e == 0
                      then do
                        -- Not a mode code: an EOL, damage, or the end. The
                        -- row ends white to the margin, and so does the
                        -- image unless EOL markers give a place to resume
                        -- (xpdf and pdf.js both stop here).
                        addPixels coding posRef columns 0
                        writeSTRef st (if ccEndOfLine p then d { dErr = True } else d { dErr = True, dEof = True })
                      else do
                        writeSTRef st d { dPos = pos + (e `shiftR` 16) }
                        let m = (e .&. 0xffff) - 1
                        bp <- readSTRef blackRef
                        if | m == mPass -> do
                               rp <- readSTRef refPos
                               b2 <- rd (rp + 1)
                               addPixels coding posRef b2 bp
                               when (b2 < columns) $ writeSTRef refPos (rp + 2)
                           | m == mHoriz -> do
                               (r1, e1) <- fullRun st (bp == 1)
                               (r2, e2) <- fullRun st (bp /= 1)
                               a0' <- curA
                               addPixels coding posRef (a0' + max 0 r1) bp
                               a1 <- curA
                               when (a1 < columns) $ addPixels coding posRef (a1 + max 0 r2) (bp `xor` 1)
                               when (e1 || e2) $ do
                                 addPixels coding posRef columns 0
                                 modifySTRef' st (\x -> x { dEof = True })
                               settle
                           | m == mV0 -> vert 0
                           | m == mVR1 -> vert 1
                           | m == mVR2 -> vert 2
                           | m == mVR3 -> vert 3
                           | m == mVL1 -> vert (-1)
                           | m == mVL2 -> vert (-2)
                           | otherwise -> vert (-3)
                        rowLoop (guard - 1)
        rowLoop (4 * columns + 64 :: Int)
        -- A row that ran out of codes (the guard) ends white to the margin.
        addPixels coding posRef columns 0
      else do
        unsafeWrite coding 0 0
        let rowLoop !black !guard = do
              cp <- readSTRef posRef
              a0 <- unsafeRead coding cp
              when (a0 < columns && guard > 0) $ do
                (r, end) <- fullRun st (black == 1)
                addPixels coding posRef (a0 + max 0 r) black
                if end
                  then do addPixels coding posRef columns 0
                          modifySTRef' st (\x -> x { dEof = True })
                  else rowLoop (black `xor` 1) (guard - 1)
        rowLoop 0 (2 * columns + 64 :: Int)
        addPixels coding posRef columns 0
      -- A damaged row is counted once.
      d <- readSTRef st
      when (dErr d) $ writeSTRef st d { dDamaged = dDamaged d + 1 }

    -- Everything between rows: EOLs, byte alignment, the 2-D tag, the end
    -- of block, and resynchronisation after damage. xpdf's sequence.
    afterRow st row = do
      d0 <- readSTRef st
      let eob = ccEndOfBlock p; eol = ccEndOfLine p; align = ccByteAlign p
      if not eob && ccRows p > 0 && row == ccRows p - 1
        then writeSTRef st d0 { dEof = True }
        else do
          gotEOL <- if eol || not align
            then do
              let pos0 = dPos d0
                  scan pos | eol = if pos < nbits && look12 pos /= 1 then scan (pos + 1) else pos
                           | otherwise = if pos < nbits && look12 pos == 0 then scan (pos + 1) else pos
                  pos1 = scan pos0
              if look12 pos1 == 1
                then do writeSTRef st d0 { dPos = pos1 + 12 }; pure True
                else do writeSTRef st d0 { dPos = pos1 }; pure False
            else pure False
          when (align && not gotEOL) $
            modifySTRef' st (\d -> d { dPos = (dPos d + 7) .&. complement 7 })
          d1 <- readSTRef st
          when (dPos d1 >= nbits) $ writeSTRef st d1 { dEof = True }
          d2 <- readSTRef st
          when (not (dEof d2) && ccK p > 0) $ readTag st
          gotEOL' <- if eob && not eol && align
            then do
              d3 <- readSTRef st
              if look 24 (dPos d3) == 0x001001
                then do writeSTRef st d3 { dPos = dPos d3 + 12 }; pure True
                else pure gotEOL
            else pure gotEOL
          d4 <- readSTRef st
          if eob && gotEOL'
            then when (look12 (dPos d4) == 1) $
                   -- End of block (G4 EOFB, G3 RTC): the image is complete.
                   writeSTRef st d4 { dEof = True }
            else when (dErr d4 && eol) $ do
              -- Resynchronise on the next EOL after a damaged row.
              let find pos | pos >= nbits = Nothing
                           | look 12 pos == 1 = Just pos
                           | otherwise = find (pos + 1)
              case find (dPos d4) of
                Nothing -> writeSTRef st d4 { dEof = True }
                Just pos -> do
                  let pos' = pos + 12
                  if ccK p > 0
                    then writeSTRef st d4 { dPos = pos' + 1, dNext2D = look 1 pos' == 0 }
                    else writeSTRef st d4 { dPos = pos' }
          modifySTRef' st (\d -> d { dErr = False })

    -- Paint the coding line into packed bits: white everywhere, then the
    -- black runs (the odd-indexed intervals).
    emitRow coding out base = do
      let fill i | i >= rowBytes = pure ()
                 | otherwise = unsafeWrite out (base + i) whiteByte >> fill (i + 1)
      fill 0
      let spans !k !prev = do
            e <- unsafeRead coding k
            let e' = min columns e
            when (odd k && e' > prev) $ paint prev e'
            when (e' < columns && k < columns + 4) $ spans (k + 1) e'
          -- Whole bytes at a time, with a mask for the partial ones at
          -- either end.
          paint x0 x1
            | x0 >= x1 = pure ()
            | x0 .&. 7 == 0 && x1 - x0 >= 8 = do
                unsafeWrite out (base + (x0 `shiftR` 3)) (complement whiteByte)
                paint (x0 + 8) x1
            | otherwise = do
                let i = base + (x0 `shiftR` 3)
                    hi = min x1 ((x0 .|. 7) + 1)          -- end of this byte's span
                    nb = hi - x0
                    mask = fromIntegral ((((0xff :: Int) `shiftL` (8 - nb)) .&. 0xff) `shiftR` (x0 .&. 7)) :: Word8
                v <- unsafeRead out i
                unsafeWrite out i (if ccBlackIs1 p then v .|. mask else v .&. complement mask)
                paint hi x1
      spans 0 0

-- Decoder state between rows.
data Dec = Dec
  { dPos     :: !Int
  , dEof     :: !Bool
  , dErr     :: !Bool
  , dDamaged :: !Int
  , dNext2D  :: !Bool
  }

type STRefDec s = STRef s Dec

freezeBytes :: STUArray s Int Word8 -> Int -> ST s BS.ByteString
freezeBytes arr n = do
  frozen <- freezeU arr
  pure (fst (BS.unfoldrN n (\i -> Just (frozen `unsafeAt` i, i + 1)) 0))

freezeU :: STUArray s Int Word8 -> ST s (UArray Int Word8)
freezeU = unsafeFreeze
