{-# LANGUAGE BangPatterns #-}
-- | CCITT fax decoding (ITU-T T.4 and T.6), as PDF's @CCITTFaxDecode@ filter
-- defines it: Group 3 one-dimensional (Modified Huffman, @K = 0@), Group 3
-- two-dimensional (Modified READ, @K > 0@) and Group 4 (MMR, @K < 0@).
--
-- A leaf module (base, bytestring, array): the PDF object layer runs it as an
-- ordinary filter, so a scanned page's bilevel image and the rare stream
-- that is fax-encoded for no good reason both go through it.
--
-- __Written from the Recommendations__ (ITU-T T.4 §4.1 and §4.2, T.6 §2.2,
-- and the filter's parameters in ISO 32000-1 §7.4.6), in their own terms. A
-- line is the list of its /changing elements/: the columns where a run
-- ends, white first, so an even entry ends a white run and an odd one a
-- black run, and the list always finishes at the right margin. The
-- two-dimensional modes are coded against the line above, and T.4 defines
-- everything they need on it: @b1@, the first changing element to the right
-- of the coding position @a0@ that ends a run of the colour being coded, and
-- @b2@, the one after it. 'findB1' is that definition; pass mode moves @a0@
-- under @b2@, vertical mode ends the current run at @b1 + d@, horizontal
-- mode reads two run lengths. Codes are read through 13-bit lookahead
-- tables built once from the T.4 code lists below.
--
-- __Between lines__ the Recommendations and the PDF parameters leave four
-- things to look for, and 'lineStart' looks for them in the order they can
-- occur: fill bits and an EOL (always accepted, as PDF requires, and sought
-- through anything when @EndOfLine@ says they are there), byte alignment
-- (@EncodedByteAlign@; an EOL is its own synchronisation, so a line
-- introduced by one is not aligned again), the tag bit that says whether a
-- Group 3 line is one- or two-dimensional (@K > 0@), and the end of the
-- block, which in every variant is an EOL directly after an EOL.
--
-- __Robustness.__ Fax data in the wild is damaged, padded, truncated and
-- mislabelled, and the right response to all of it is to keep going. Two
-- kinds of damage are told apart. A run that would leave the line (or end
-- before it starts) is /clamped/: the decoder is still in step with the
-- data, so it carries on. A bit pattern that is no code at all means step
-- is lost: the line ends there, white to the margin, and decoding resumes
-- after the next EOL if there is one, which is what T.4 put EOL in the
-- format for. With none ahead, a one-dimensional stream tries again from
-- the next bit (its lines are independent) and a two-dimensional one ends,
-- since every later line would be coded against a line it does not have.
-- Every line consumes input, the output is capped by the caller's byte
-- limit, and running out of data ends the image with the lines decoded so
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
import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)
import Data.Array.ST (STUArray, newArray, runSTUArray)
import Data.Array.Unboxed (UArray, accumArray)
import qualified Data.Array.Unboxed as U
import Data.Bits (shiftL, shiftR, (.&.), (.|.), complement, countTrailingZeros)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import Data.Array.Unsafe (unsafeFreeze)
import Data.Word (Word8)

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
      -- Two lines of changing elements, swapped after every row. A line has
      -- at most columns + 1 of them (strictly increasing after a leading 0).
      lineA <- newArray (0, columns + 2) 0 :: ST s (STUArray s Int Int)
      lineB <- newArray (0, columns + 2) 0 :: ST s (STUArray s Int Int)
      unsafeWrite lineA 0 columns            -- the imaginary line above the first: one white run
      rd <- Reader <$> newSTRef 0 <*> newSTRef False <*> newSTRef False <*> newSTRef False
      damagedRef <- newSTRef (0 :: Int)

      let rows !row ref !nref cur !afterResync
            | row >= maxRows = pure row
            | otherwise = do
                kind <- lineStart rd (row == 0) afterResync
                if kind == noMoreLines then pure row else do
                  begun <- readSTRef (rdPos rd)
                  writeSTRef (rdLost rd) False
                  writeSTRef (rdClamped rd) False
                  (n, complete) <- if kind == twoDimensional then line2D rd ref nref cur
                                                             else line1D rd cur
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
                  emitRow cur n out (row * rowBytes)
                  lost <- readSTRef (rdLost rd)
                  clamped <- readSTRef (rdClamped rd)
                  when (lost || clamped) $ modifySTRef' damagedRef (+ 1)
                  if | not complete -> pure (row + 1)          -- the data ended inside this line
                     | not lost -> rows (row + 1) cur n ref False
                     | otherwise -> do
                         -- Step is lost. T.4's remedy is the next EOL.
                         here <- readSTRef (rdPos rd)
                         found <- if ccK p >= 0 then nextEol rd here else pure (-1)
                         if | found >= 0 -> do
                                writeSTRef (rdPos rd) found
                                rows (row + 1) cur n ref True
                            | ccK p /= 0 -> pure (row + 1)     -- later lines depend on this one
                            | otherwise -> do
                                writeSTRef (rdPos rd) (max here (begun + 1))
                                rows (row + 1) cur n ref False
      nRows <- rows 0 lineA 1 lineB False
      out <- readSTRef outRef
      bytes <- freezeBytes out (nRows * rowBytes)
      damaged <- readSTRef damagedRef
      pure (CcittResult nRows bytes damaged)
  where
    columns = ccColumns p
    rowBytes = (columns + 7) `div` 8
    nbits = 8 * BS.length dat
    whiteByte = if ccBlackIs1 p then 0 else 0xff :: Word8
    eolCode = 1 :: Int                       -- 000000000001

    byteAt i = if i < BS.length dat then fromIntegral (BSU.unsafeIndex dat i) else 0 :: Int
    -- The bit position of the data's last 1 bit (-1 for none), found once:
    -- every position after it is trailing padding.
    lastOne = case BS.findIndexEnd (/= 0) dat of
      Nothing -> -1
      Just b -> 8 * b + 7 - countTrailingZeros (BSU.unsafeIndex dat b)
    -- The next @n@ (1..24) bits at bit position @pos@, zero past the end.
    look :: Int -> Int -> Int
    look n pos =
      let b = pos `shiftR` 3
          w = (byteAt b `shiftL` 24) .|. (byteAt (b + 1) `shiftL` 16)
              .|. (byteAt (b + 2) `shiftL` 8) .|. byteAt (b + 3)
      in (w `shiftR` (32 - (pos .&. 7) - n)) .&. ((1 `shiftL` n) - 1)

    -- The position of the next EOL at or after @from@, or -1. A failed
    -- search is remembered: the data does not change, so there is none
    -- after any later position either, and a stream of garbage is searched
    -- once rather than once per line.
    nextEol rd from = do
      none <- readSTRef (rdNoEol rd)
      if none then pure (-1) else do
        let go pos | pos + 12 > nbits = -1
                   | look 12 pos == eolCode = pos
                   | otherwise = go (pos + 1)
            found = go from
        when (found < 0) $ writeSTRef (rdNoEol rd) True
        pure found

    -- Everything that can stand between two lines. Leaves the reader on the
    -- line's first code and says what kind of line it is.
    lineStart rd first afterResync = do
      pos0 <- readSTRef (rdPos rd)
      let align = ccByteAlign p
          eolAt pos = pos + 12 <= nbits && look 12 pos == eolCode
          fill pos | pos < nbits && look 12 pos == 0 = fill (pos + 1)
                   | otherwise = pos
      -- Where EOLs are promised (or one was just found by a resync) the next
      -- one is sought through whatever lies before it. A stream that
      -- promises them and has none left is mislabelled or at its end; either
      -- way only fill is skipped.
      promised <- if (ccEndOfLine p && not first) || afterResync then nextEol rd pos0 else pure (-1)
      let -- With byte alignment and no promise of EOLs, the padding of one
          -- line and the leading zeros of the next can spell an EOL between
          -- them, so fill is not skipped there.
          skipping = first || not align || ccEndOfLine p
          pos1 | promised >= 0 = promised
               | skipping = fill pos0
               | otherwise = pos0
          pos2 | (promised >= 0 || skipping) && eolAt pos1 = pos1
               | align && not first = (pos1 + 7) .&. complement 7
               | otherwise = pos1
          -- (After aligning, an EOL standing whole at the boundary is
          -- unambiguous: no line begins with eleven zeros.)
          gotEol = eolAt pos2
          pos3 = if gotEol then pos2 + 12 else pos2
          tagged = ccK p > 0
          pos4 = if tagged then pos3 + 1 else pos3
          -- The end of the block is an EOL straight after an EOL: six of
          -- them (RTC) in Group 3, two (EOFB) in Group 4, and never anything
          -- else. A Group 4 line cannot begin with one, so there it ends the
          -- image whatever @EndOfBlock@ says.
          blockEnd = gotEol && eolAt pos4
      if -- Every code has a 1 in it, so nothing but zeros to the end is
         -- padding, however many bytes of it there are (with alignment on
         -- and no EOLs, each byte would otherwise start a damaged line).
         | pos3 > lastOne -> pure noMoreLines
         | blockEnd && (ccEndOfBlock p || ccK p < 0) -> pure noMoreLines
         | otherwise -> do
             writeSTRef (rdPos rd) pos4
             pure (if ccK p < 0 || (tagged && look 1 pos3 == 0) then twoDimensional else oneDimensional)

    -- One run length of the given colour: make-up codes, then a terminating
    -- code. The status is 'runOk', 'runNoCode' (the bits are not a code;
    -- nothing is consumed) or 'runNoData'.
    runLength rd black = go 0
      where
        table = if black then blackTable else whiteTable
        go !acc = do
          pos <- readSTRef (rdPos rd)
          if pos >= nbits then pure (acc, runNoData) else do
            let e = table `unsafeAt` look 13 pos
            if e == 0 then pure (acc, runNoCode) else do
              writeSTRef (rdPos rd) (pos + (e `shiftR` 16))
              let len = (e .&. 0xffff) - 1
                  acc' = min (2 * maxCcittColumns) (acc + len)
              if len >= 64 then go acc' else pure (acc', runOk)

    -- End the current run at column @x@: append a changing element. @lo@ is
    -- the least column it may be (the coding position); a run asked to end
    -- outside [lo, columns] is clamped and the line marked. An empty run in
    -- mid-line is no change of colour at all, so instead of recording it the
    -- run before it is reopened, which keeps a line's elements strictly
    -- increasing and its parity equal to the colour being coded. Returns
    -- the new element count and the column actually used.
    endRun rd cur !n !lo !x = do
      let x' = max lo (min columns x)
      when (x' /= x) $ writeSTRef (rdClamped rd) True
      prev <- if n > 0 then unsafeRead cur (n - 1) else pure (-1)
      if x' <= prev
        then pure (n - 1, prev)
        else do unsafeWrite cur n x'; pure (n + 1, x')

    -- Close a line. One that reached the margin through its codes ends with
    -- the run then being coded; one that was cut short is white from its
    -- last changing element on.
    closeLine cur !n reached = do
      prev <- if n > 0 then unsafeRead cur (n - 1) else pure (-1)
      if | prev >= columns -> pure n
         | reached || even n -> do unsafeWrite cur n columns; pure (n + 1)
         | otherwise -> do unsafeWrite cur (n - 1) columns; pure n

    -- A one-dimensional line (T.4 §4.1): alternating runs, white first.
    line1D rd cur = go 0 0 (0 :: Int)
      where
        go !a0 !n !colour
          | a0 >= columns = do n' <- closeLine cur n True; pure (n', True)
          | otherwise = do
              (len, status) <- runLength rd (colour == 1)
              if status == runOk
                then do (n', a1) <- endRun rd cur n a0 (a0 + len)
                        go a1 n' (1 - colour)
                else do when (status == runNoCode) $ writeSTRef (rdLost rd) True
                        n' <- closeLine cur n False
                        pure (n', status /= runNoData)

    -- T.4 §4.2.1.3.1: b1 is the first changing element of the reference
    -- line to the right of a0 that changes to the colour opposite the one
    -- being coded, that is, one that ends a run of the coded colour: an
    -- element whose index has the colour's parity. @hint@ is where the last
    -- search ended; a0 only ever moves right, but b1 may have been passed
    -- over by a vertical code to its left, hence the step back. Past the
    -- end of the line every element is the margin.
    findB1 ref !nref !a0 !colour !hint = do
      let at i = if i < nref then unsafeRead ref i else pure columns
          back i = if i <= 0 then pure 0 else do
                     v <- at (i - 1)
                     if v > a0 then back (i - 1) else pure i
          fwd i = if i >= nref then pure i else do
                    v <- at i
                    if v <= a0 then fwd (i + 1) else pure i
      i <- back hint >>= fwd
      pure (if i .&. 1 == colour then i else i + 1)

    -- A two-dimensional line (T.4 §4.2, T.6 §2.2), coded against @ref@.
    -- a0 starts on an imaginary element just left of the line.
    line2D rd ref nref cur = go (-1) 0 (0 :: Int) 0
      where
        refAt i = if i < nref then unsafeRead ref i else pure columns
        stop n reached complete = do n' <- closeLine cur n reached; pure (n', complete)
        go !a0 !n !colour !hint
          | a0 >= columns = stop n True True
          | otherwise = do
              pos <- readSTRef (rdPos rd)
              let e = modeTable `unsafeAt` look 13 pos
              if | pos >= nbits -> stop n False False
                 | e == 0 -> do writeSTRef (rdLost rd) True; stop n False True
                 | otherwise -> do
                     writeSTRef (rdPos rd) (pos + (e `shiftR` 16))
                     let mode = (e .&. 0xffff) - 1
                     i <- findB1 ref nref a0 colour hint
                     if | mode == mPass -> do
                            -- the run continues under b2
                            b2 <- refAt (i + 1)
                            go b2 n colour i
                        | mode == mHoriz -> do
                            let from = max 0 a0
                            (len1, st1) <- runLength rd (colour == 1)
                            if st1 /= runOk then failed n st1 else do
                              (n1, a1) <- endRun rd cur n from (from + len1)
                              (len2, st2) <- runLength rd (colour /= 1)
                              if st2 /= runOk then failed n1 st2 else do
                                (n2, a2) <- endRun rd cur n1 a1 (a1 + len2)
                                go a2 n2 colour i
                        | otherwise -> do
                            b1 <- refAt i
                            (n', a1) <- endRun rd cur n (max 0 a0) (b1 + verticalOffset mode)
                            go a1 n' (1 - colour) i
        failed n status = do
          when (status == runNoCode) $ writeSTRef (rdLost rd) True
          stop n False (status /= runNoData)

    -- Paint a line's changing elements into packed bits: white everywhere,
    -- then the black runs (each ends at an odd-indexed element).
    emitRow cur n out base = do
      let fill i | i >= rowBytes = pure ()
                 | otherwise = unsafeWrite out (base + i) whiteByte >> fill (i + 1)
      fill 0
      let spans !k
            | k >= n = pure ()
            | otherwise = do
                x0 <- unsafeRead cur (k - 1)
                x1 <- unsafeRead cur k
                paint x0 x1
                spans (k + 2)
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
      spans 1

-- What 'lineStart' found.
noMoreLines, oneDimensional, twoDimensional :: Int
noMoreLines = -1; oneDimensional = 0; twoDimensional = 1

-- How a run length ended.
runOk, runNoCode, runNoData :: Int
runOk = 0; runNoCode = 1; runNoData = 2

-- a1 - b1 for the seven vertical modes.
verticalOffset :: Int -> Int
verticalOffset m
  | m == mV0 = 0
  | m == mVR1 = 1
  | m == mVR2 = 2
  | m == mVR3 = 3
  | m == mVL1 = -1
  | m == mVL2 = -2
  | otherwise = -3

-- The bit reader and the state of the line being decoded.
data Reader s = Reader
  { rdPos     :: !(STRef s Int)    -- ^ Bit position in the data.
  , rdLost    :: !(STRef s Bool)   -- ^ This line met bits that are no code.
  , rdClamped :: !(STRef s Bool)   -- ^ This line had a run clamped to the line.
  , rdNoEol   :: !(STRef s Bool)   -- ^ A search for an EOL has reached the end.
  }

freezeBytes :: STUArray s Int Word8 -> Int -> ST s BS.ByteString
freezeBytes arr n = do
  frozen <- freezeU arr
  pure (fst (BS.unfoldrN n (\i -> Just (frozen `unsafeAt` i, i + 1)) 0))

freezeU :: STUArray s Int Word8 -> ST s (UArray Int Word8)
freezeU = unsafeFreeze
