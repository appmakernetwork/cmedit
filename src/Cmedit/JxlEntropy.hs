{-# LANGUAGE BangPatterns #-}
-- | JPEG XL entropy decoding (ISO/IEC 18181-1 Annex C): the hybrid-integer
-- coder shared by every stream in the format.
--
-- A stream is configured once ('readCode') with LZ77 parameters, a map from
-- contexts to clusters, a hybrid-uint split per cluster, and per-cluster
-- distributions coded either as Brotli-style prefix codes or as 12-bit rANS
-- with an alias table. Then symbols are read one at a time ('readSym' by
-- context, 'readSymCl' by an already-resolved cluster).
--
-- Representation, chosen for the per-sample read in the Modular decoder:
--
--  * every table is a flat 'UArray' of packed 'Int's indexed by
--    @cluster << bits | index@, so a symbol costs one or two unboxed loads;
--  * the ANS alias table is JPEG XL's own (§C.2.6): the state-to-symbol map
--    is normative, so a generic cumulative-frequency rANS table would decode
--    garbage. Each bucket packs symbol, cutoff, both distributions and the
--    alias offset into one word, so the read is branch-light;
--  * mutable per-stream state (the ANS state, the LZ77 copy cursor) lives in
--    a small unboxed array, never an 'STRef'.
module Cmedit.JxlEntropy
  ( Code(..), CodeSt
  , readCode, newCodeSt, readSym, readSymCl, readToken, finishCode
  , singleToken, clusterOf, withSymReader, SymReader, readWith
  , readClusters, readPermutation
  ) where

import Control.Monad (forM, forM_, replicateM, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, newArray, runSTUArray, getElems)
import Data.Array.Unboxed (UArray, listArray, bounds, elems, accumArray)
import qualified Data.Array.Unboxed as U
import Data.Bits
import Data.List (foldl')

import Cmedit.JxlBits

-- | A configured entropy stream (immutable; see 'CodeSt' for its state).
data Code = Code
  { cdClusters  :: !(UArray Int Int)  -- ^ context -> cluster
  , cdNClusters :: !Int
  , cdPrefix    :: !Bool
  , cdLogA      :: !Int               -- ^ ANS: log2 of the bucket count
  , cdAns       :: !(UArray Int Int)  -- ^ ANS buckets, @cl << logA | i@
  , cdCfg       :: !(UArray Int Int)  -- ^ cluster -> packed hybrid-uint config
  , cdPfxTop    :: !(UArray Int Int)  -- ^ all clusters' top-level tables
  , cdPfxInfo   :: !(UArray Int Int)  -- ^ cluster -> offset << 4 | top bits
  , cdPfxSub    :: !(UArray Int Int)  -- ^ second-level tables
  , cdSingle    :: !(UArray Int Int)  -- ^ cluster -> its only symbol, or -1
  , cdLz        :: !Bool
  , cdLzMinSym  :: !Int
  , cdLzMinLen  :: !Int
  , cdLzLenCfg  :: !Int
  , cdLzDistCl  :: !Int
  }

-- | Mutable state of one stream: slot 0 ANS state, 1 LZ77 copies pending,
-- 2 copy cursor, 3 symbols decoded, 4 error flag; plus the LZ77 window.
data CodeSt s = CodeSt !(STUArray s Int Int) !(STUArray s Int Int) !Int

-- Packed hybrid-uint config: split exponent | msb << 8 | lsb << 16.
cfgPack :: Int -> Int -> Int -> Int
cfgPack se msb lsb = se .|. (msb `shiftL` 8) .|. (lsb `shiftL` 16)

readIntCfg :: BR s -> Int -> J s Int
readIntCfg br logA = do
  se <- liftJ (readBits br (ceilLog2 logA))
  if se == logA then pure (cfgPack se 0 0) else do
    msb <- liftJ (readBits br (ceilLog2 se))
    jguard (msb <= se) "JPEG XL: bad hybrid integer config"
    lsb <- liftJ (readBits br (ceilLog2 (se - msb)))
    jguard (msb + lsb <= se) "JPEG XL: bad hybrid integer config"
    pure (cfgPack se msb lsb)

-- | Read a stream's configuration for @numDist@ contexts.
readCode :: BR s -> Int -> J s Code
readCode br numDist = do
  lz <- liftJ (readBool br)
  (minSym, minLen, lenCfg) <-
    if not lz then pure (0, 0, 0) else do
      ms <- liftJ (u32 br (224,0) (512,0) (4096,0) (8,15))
      ml <- liftJ (u32 br (3,0) (4,0) (5,2) (9,8))
      lc <- readIntCfg br 8
      pure (ms, ml, lc)
  readCodeInner br (if lz then numDist + 1 else numDist) lz minSym minLen lenCfg

readCodeInner :: BR s -> Int -> Bool -> Int -> Int -> Int -> J s Code
readCodeInner br nd lz minSym minLen lenCfg = do
  (ncl, clusters) <- readClusters br nd
  prefix <- liftJ (readBool br)
  logA <- if prefix then pure 15 else liftJ ((+ 5) <$> readBits br 2)
  cfgs <- replicateM ncl (readIntCfg br logA)
  let cfgArr = listArray (0, ncl - 1) cfgs :: UArray Int Int
  if prefix
    then do
      counts <- replicateM ncl $ do
        b <- liftJ (readBool br)
        c <- if not b then pure 1 else liftJ $ do
               n <- readBits br 4
               r <- readBits br n
               pure (1 + (1 `shiftL` n) + r)
        jguard (c <= 32768) "JPEG XL: prefix alphabet too large"
        pure c
      tabs <- forM counts (readPrefixLengths br)
      let built = map buildPrefix tabs
          tops = map (\(_, t, _) -> t) built
          offs = scanl (+) 0 (map length tops)
          subOffs = scanl (+) 1 (map (\(_, _, s) -> length s) built)
          -- second-level entries point at absolute offsets in the shared array
          topAbs = [ [ if e .&. 0x80 /= 0
                         then (((e `shiftR` 8) + so) `shiftL` 8) .|. (e .&. 0xff)
                         else e
                     | e <- t ]
                   | ((_, t, _), so) <- zip built subOffs ]
          info = [ (o `shiftL` 4) .|. tb | ((tb, _, _), o) <- zip built offs ]
          singles = [ singleOf lens | lens <- tabs ]
      pure Code
        { cdClusters = clusters, cdNClusters = ncl, cdPrefix = True, cdLogA = 0
        , cdAns = emptyArr, cdCfg = cfgArr
        , cdPfxTop = listArr (concat topAbs)
        , cdPfxInfo = listArr info
        , cdPfxSub = listArr (0 : concatMap (\(_, _, s) -> s) built)
        , cdSingle = listArr singles
        , cdLz = lz, cdLzMinSym = minSym, cdLzMinLen = minLen, cdLzLenCfg = lenCfg
        , cdLzDistCl = clusters `unsafeAt` (nd - 1)
        }
    else do
      hs <- replicateM ncl (readAnsHist br logA)
      let buckets = concatMap fst hs
      pure Code
        { cdClusters = clusters, cdNClusters = ncl, cdPrefix = False, cdLogA = logA
        , cdAns = listArr buckets, cdCfg = cfgArr
        , cdPfxTop = emptyArr, cdPfxInfo = emptyArr, cdPfxSub = emptyArr
        , cdSingle = listArr (map snd hs)
        , cdLz = lz, cdLzMinSym = minSym, cdLzMinLen = minLen, cdLzLenCfg = lenCfg
        , cdLzDistCl = clusters `unsafeAt` (nd - 1)
        }
  where
    singleOf lens = case [ s | (s, l) <- zip [0..] lens, l > 0 ] of
      []  -> 0          -- the alphabet-of-one code: symbol 0, no bits
      [s] -> s
      _   -> -1

emptyArr :: UArray Int Int
emptyArr = listArray (0, -1) []

listArr :: [Int] -> UArray Int Int
listArr xs = listArray (0, length xs - 1) xs

-- | The cluster map (§C.2.4).
readClusters :: BR s -> Int -> J s (Int, UArray Int Int)
readClusters br nd
  | nd == 1 = pure (1, listArray (0, 0) [0])
  | otherwise = do
      simple <- liftJ (readBool br)
      cs <- if simple
        then liftJ $ do
          nbits <- readBits br 2
          replicateM nd (readBits br nbits)
        else do
          useMtf <- liftJ (readBool br)
          code <- if nd <= 2
            then do lz <- liftJ (readBool br)
                    jguard (not lz) "JPEG XL: LZ77 in a small cluster map"
                    readCodeInner br 1 False 0 0 0
            else readCode br 1
          st <- liftJ (newCodeSt br code 0)
          vs <- liftJ (replicateM nd (readSym code st br 0 0))
          finishCode code st
          jguard (all (< 256) vs) "JPEG XL: cluster index out of range"
          pure (if useMtf then mtf vs else vs)
      let ncl = maximum cs + 1
          seen = accumArray (\_ b -> b) False (0, ncl - 1) [ (c, True) | c <- cs ]
                   :: UArray Int Bool
      jguard (and (elems seen)) "JPEG XL: cluster map has a hole"
      pure (ncl, listArray (0, nd - 1) cs)
  where
    mtf = go [0 .. 255]
      where go _ [] = []
            go tbl (i : rest) =
              let v = tbl !! i
              in v : go (v : take i tbl ++ drop (i + 1) tbl) rest

------------------------------------------------------------------------------
-- Prefix codes (Brotli, RFC 7932 §3.4–3.5)

readPrefixLengths :: BR s -> Int -> J s [Int]
readPrefixLengths br alpha
  | alpha == 1 = pure [0]
  | otherwise = do
      hskip <- liftJ (readBits br 2)
      if hskip == 1 then simpleCode else complexCode hskip
  where
    abits = ceilLog2 (alpha - 1)
    place assigns = do
      jguard (all (\(s, _) -> s < alpha) assigns) "JPEG XL: bad prefix code"
      pure (U.elems (accumArray (\_ l -> l) 0 (0, alpha - 1) assigns :: UArray Int Int))
    simpleCode = do
      nsym <- liftJ ((+ 1) <$> readBits br 2)
      syms <- liftJ (replicateM nsym (readBits br abits))
      case syms of
        [s] -> do jguard (s < alpha) "JPEG XL: bad prefix code"
                  pure (singleMarker alpha s)
        [a, b] -> place [(a, 1), (b, 1)]
        [a, b, c] -> place [(a, 1), (b, 2), (c, 2)]
        [a, b, c, d] -> do
          sel <- liftJ (readBool br)
          place (if sel then [(a,1),(b,2),(c,3),(d,3)] else [(a,2),(b,2),(c,2),(d,2)])
        _ -> jfail "JPEG XL: bad prefix code"
    complexCode hskip = do
      let order = drop hskip [1,2,3,4,0,5,17,6,16,7,8,9,10,11,12,13,14,15]
      clcl <- liftJ (readClcl order)
      let nonzero = [ (s, l) | (s, l) <- clcl, l /= 0 ]
          acc = sum [ 32 `shiftR` l | (_, l) <- nonzero ] :: Int
      jguard (not (null nonzero)) "JPEG XL: empty code length code"
      jguard (length nonzero == 1 || acc == 32) "JPEG XL: bad code length code"
      let lens18 = U.elems (accumArray (\_ l -> l) 0 (0, 17) clcl :: UArray Int Int)
          clTab = tableArrays $ case nonzero of
                    [(s, _)] -> (0, [s `shiftL` 8], [])
                    _        -> buildPrefix lens18
      r <- liftJ (readLengths clTab)
      case r of
        Nothing -> jfail "JPEG XL: bad prefix code lengths"
        Just ls -> pure ls
    -- the code lengths of the code-length code, stopping once complete
    readClcl = go (0 :: Int) []
      where
        go _ acc [] = pure (reverse acc)
        go !bitacc acc (i : rest) = do
          base <- u32 br (0,0) (4,0) (3,0) (8,0)
          len <- if base /= 8 then pure base else do
                   b1 <- readBool br
                   if not b1 then pure 2 else do
                     b2 <- readBool br
                     pure (if b2 then 5 else 1)
          let acc' = (i, len) : acc
          if len == 0 then go bitacc acc' rest else do
            let ba = bitacc + (32 `shiftR` len)
            if ba >= 32 then pure (reverse acc') else go ba acc' rest
    readLengths clTab = do
      out <- newIntArr alpha
      let go !i !prevSym !lastNZ !lastRep !bitacc
            | i >= alpha = pure (bitacc == 32768)
            | otherwise = do
                sym <- prefixSymOf clTab br
                if sym <= 15 then do
                  unsafeWrite out i sym
                  let ba = if sym /= 0 then bitacc + (32768 `shiftR` sym) else bitacc
                  if ba > 32768 then pure False
                  else if ba == 32768 then pure True
                  else go (i + 1) sym (if sym /= 0 then sym else lastNZ) lastRep ba
                else do
                  extra <- readBits br (if sym == 16 then 2 else 3)
                  let base = extra + 3
                      (rc, lr') = if prevSym == sym
                                    then let r = base + lastRep * (if sym == 16 then 3 else 7)
                                                       - (if sym == 16 then 8 else 16)
                                         in (r, lastRep + r)
                                    else (base, base)
                      rsym = if sym == 16 then lastNZ else 0
                  if rc <= 0 || i + rc > alpha then pure False else do
                    forM_ [i .. i + rc - 1] $ \k -> unsafeWrite out k rsym
                    let ba = if rsym /= 0 then bitacc + rc * (32768 `shiftR` rsym) else bitacc
                    if ba > 32768 then pure False
                    else if ba == 32768 then pure True
                    else go (i + rc) sym lastNZ lr' ba
      ok <- go 0 8 8 0 0
      if ok then Just <$> getElems out else pure Nothing

-- A single-symbol simple code: all lengths zero but the symbol's, which the
-- table builder treats as a zero-bit code.
singleMarker :: Int -> Int -> [Int]
singleMarker alpha s = [ if k == s then -1 else 0 | k <- [0 .. alpha - 1] ]

-- | A two-level decoding table: (top bits, top table, second-level table).
-- Top entries are @sym << 8 | len@ (len <= top bits), or
-- @suboffset << 8 | 0x80 | subbits@; second-level entries @sym << 8 | len@.
buildPrefix :: [Int] -> (Int, [Int], [Int])
buildPrefix lens
  | [s] <- [ s | (s, l) <- zip [0..] lens, l < 0 ] = (0, [s `shiftL` 8], [])
  | otherwise =
  let maxLen = maximum (0 : lens)
      tb = min maxLen 10
      blCount = accumArray (+) 0 (0, 15) [ (l, 1) | l <- lens, l > 0 ] :: UArray Int Int
      firstCode = listArray (0, 16)
        (scanl (\c l -> (c + blCount U.! l) `shiftL` 1) 0 [0 .. 15]) :: UArray Int Int
      -- canonical codes, assigned in symbol order within each length
      coded = assign lens 0 firstCode
      assign [] _ _ = []
      assign (l : ls) s fc
        | l <= 0 = assign ls (s + 1) fc
        | otherwise =
            let c = fc U.! l
            in (s, l, rev l c) : assign ls (s + 1) (fc U.// [(l, c + 1)])
      rev l c = foldl' (\a i -> (a `shiftL` 1) .|. ((c `shiftR` i) .&. 1)) 0 [0 .. l - 1]
      topSize = 1 `shiftL` tb
      longs = [ x | x@(_, l, _) <- coded, l > tb ]
      subMax = accumArray max 0 (0, topSize - 1)
                 [ (r .&. (topSize - 1), l) | (_, l, r) <- longs ] :: UArray Int Int
      subSizes = [ if m > 0 then 1 `shiftL` (m - tb) else 0 | m <- U.elems subMax ]
      subOff = listArray (0, topSize - 1) (scanl (+) 0 subSizes) :: UArray Int Int
      topEntries =
        [ (r + (k `shiftL` l), (s `shiftL` 8) .|. l)
        | (s, l, r) <- coded, l <= tb, k <- [0 .. (1 `shiftL` (tb - l)) - 1] ]
        ++ [ (p, ((subOff U.! p) `shiftL` 8) .|. 0x80 .|. (m - tb))
           | (p, m) <- zip [0 ..] (U.elems subMax), m > 0 ]
      top = accumArray (\_ e -> e) 0 (0, topSize - 1) topEntries :: UArray Int Int
      subTotal = sum subSizes
      subEntries =
        [ (subOff U.! p + (r `shiftR` tb) + (k `shiftL` (l - tb)), (s `shiftL` 8) .|. l)
        | (s, l, r) <- longs
        , let p = r .&. (topSize - 1)
              m = subMax U.! p
        , k <- [0 .. (1 `shiftL` (m - l)) - 1] ]
      sub = accumArray (\_ e -> e) 0 (0, subTotal - 1) subEntries :: UArray Int Int
  in (tb, U.elems top, if subTotal > 0 then U.elems sub else [])

tableArrays :: (Int, [Int], [Int]) -> (Int, UArray Int Int, UArray Int Int)
tableArrays (tb, top, sub) = (tb, listArr top, listArr sub)

-- Decode with a stand-alone table (only the code-length code uses this).
prefixSymOf :: (Int, UArray Int Int, UArray Int Int) -> BR s -> ST s Int
prefixSymOf (tb, topA, subA) br = do
  p <- brPos br
  let v = peekAt (brBuf br) p
      e = topA `unsafeAt` (v .&. ((1 `shiftL` tb) - 1))
  if e .&. 0x80 == 0
    then do brSetPos br (p + (e .&. 0x7f)); pure (e `shiftR` 8)
    else do
      let e2 = subA `unsafeAt` ((e `shiftR` 8) + ((v `shiftR` tb) .&. ((1 `shiftL` (e .&. 0x7f)) - 1)))
      brSetPos br (p + (e2 .&. 0x7f))
      pure (e2 `shiftR` 8)

newIntArr :: Int -> ST s (STUArray s Int Int)
newIntArr n = newArray (0, max 0 (n - 1)) 0

------------------------------------------------------------------------------
-- ANS distributions (§C.2.5) and the alias table (§C.2.6)

readU8 :: BR s -> ST s Int
readU8 br = do
  b <- readBool br
  if not b then pure 0 else do
    n <- readBits br 3
    r <- readBits br n
    pure ((1 `shiftL` n) + r)

-- The log-count prefix code of the compressed histogram form.
readLogCount :: BR s -> ST s Int
readLogCount br = do
  v <- readBits br 3
  case v of
    0 -> pure 10
    1 -> pickFirst [4, 0, 11, 13]
    2 -> pure 7
    3 -> do b <- readBool br; pure (if b then 1 else 3)
    4 -> pure 6
    5 -> pure 8
    6 -> pure 9
    _ -> do b <- readBool br; pure (if b then 2 else 5)
  where pickFirst [] = pure 12
        pickFirst (x : xs) = do b <- readBool br
                                if b then pure x else pickFirst xs

ansBias :: Int
ansBias = 1 `shiftL` 20

-- | One cluster's buckets plus its single symbol (or -1).
readAnsHist :: BR s -> Int -> J s ([Int], Int)
readAnsHist br logA = do
  let tableSize = 1 `shiftL` logA
  dist <- readDist br logA tableSize
  let logB = 12 - logA
      bsize = 1 `shiftL` logB
  case [ i | (i, d) <- zip [0..] dist, d == 4096 ] of
    (s : _) -> pure
      ( [ packB s 0 d 4096 (bsize * i) | (i, d) <- zip [0..] dist ]
      , s )
    [] -> pure (aliasTable dist tableSize bsize, -1)
  where
    packB sym cut d da off =
      sym .|. (cut `shiftL` 8) .|. (d `shiftL` 16) .|. (da `shiftL` 29)
          .|. ((off + ansBias) `shiftL` 42)

    aliasTable dist tableSize bsize = runST $ do
      dA  <- newListArr dist
      sym <- newIntArr tableSize
      off <- newIntArr tableSize
      cut <- newIntArr tableSize
      forM_ (zip [0 ..] dist) $ \(i, d) -> do
        unsafeWrite sym i i
        unsafeWrite cut i d
      let under0 = reverse [ i | (i, d) <- zip [0..] dist, d < bsize ]
          over0  = reverse [ i | (i, d) <- zip [0..] dist, d > bsize ]
          loop (o : os) (u : us) = do
            cu <- unsafeRead cut u
            co <- unsafeRead cut o
            let by = bsize - cu
                co' = co - by
            unsafeWrite cut o co'
            unsafeWrite sym u o
            unsafeWrite off u co'
            case compare co' bsize of
              LT -> loop os (o : us)
              EQ -> loop os us
              GT -> loop (o : os) us
          loop _ _ = pure ()
      loop over0 under0
      forM [0 .. tableSize - 1] $ \i -> do
        d <- unsafeRead dA i
        c <- unsafeRead cut i
        if c == bsize then pure (packB i 0 d d 0) else do
          s <- unsafeRead sym i
          o <- unsafeRead off i
          ds <- unsafeRead dA s
          pure (packB s c d ds (o - c))

-- | The 4096-total distribution of one cluster.
readDist :: BR s -> Int -> Int -> J s [Int]
readDist br logA tableSize = do
  b1 <- liftJ (readBool br)
  if b1 then do
    b2 <- liftJ (readBool br)
    if b2 then do
      v0 <- liftJ (readU8 br)
      v1 <- liftJ (readU8 br)
      jguard (v0 /= v1 && max v0 v1 < tableSize) "JPEG XL: bad ANS histogram"
      prob <- liftJ (readBits br 12)
      pure [ if i == v0 then prob else if i == v1 then 4096 - prob else 0
           | i <- [0 .. tableSize - 1] ]
    else do
      v <- liftJ (readU8 br)
      jguard (v < tableSize) "JPEG XL: bad ANS histogram"
      pure [ if i == v then 4096 else 0 | i <- [0 .. tableSize - 1] ]
  else do
    b2 <- liftJ (readBool br)
    if b2 then do
      a <- liftJ ((+ 1) <$> readU8 br)
      jguard (a <= tableSize) "JPEG XL: bad ANS histogram"
      let base = 4096 `div` a
          left = 4096 `mod` a
      pure [ if i < left then base + 1 else if i < a then base else 0
           | i <- [0 .. tableSize - 1] ]
    else do
      len <- liftJ (let go l | l >= 3 = pure l
                             | otherwise = do b <- readBool br
                                              if b then go (l + 1) else pure l
                    in go (0 :: Int))
      shift <- liftJ ((\v -> v + (1 `shiftL` len) - 1) <$> readBits br len)
      jguard (shift <= 13) "JPEG XL: bad ANS histogram"
      alpha <- liftJ ((+ 3) <$> readU8 br)
      jguard (alpha <= tableSize) "JPEG XL: bad ANS histogram"
      -- log counts, with run-length "same" markers (libjxl's ReadHistogram)
      let readLogs !i acc
            | i >= alpha = pure (reverse acc)
            | otherwise = do
                lc <- readLogCount br
                if lc == 13 then do
                  rle <- readU8 br
                  let run = rle + 4
                  readLogs (i + run) (replicate (run - 1) (-2) ++ [-1 - run] ++ acc)
                else readLogs (i + 1) (lc : acc)
      logs0 <- liftJ (readLogs 0 [])
      let logs = take alpha logs0
      jguard (length logs0 == alpha) "JPEG XL: bad ANS histogram"
      -- the largest log count (first occurrence) is omitted and implied
      let cands = [ (lc, i) | (i, lc) <- zip [0 :: Int ..] logs, lc >= 0 ]
      jguard (not (null cands)) "JPEG XL: bad ANS histogram"
      let omitLog = maximum (map fst cands)
          omitPos = head [ i | (lc, i) <- cands, lc == omitLog ]
      jguard (omitPos + 1 >= alpha || not (isRunStart (logs !! (omitPos + 1))))
        "JPEG XL: bad ANS histogram"
      let go _ _ [] acc = pure (reverse acc)
          go !i !prev (lc : rest) acc
            | isRunStart lc || lc == -2 = go (i + 1) prev rest (prev : acc)
            | i == omitPos = go (i + 1) 0 rest (0 : acc)
            | lc == 0 = go (i + 1) 0 rest (0 : acc)
            | lc == 1 = go (i + 1) 1 rest (1 : acc)
            | otherwise = do
                let zeros = lc - 1
                    bc = max 0 (min zeros (shift - ((12 - zeros) `shiftR` 1)))
                r <- readBits br bc
                let v = (1 `shiftL` zeros) + (r `shiftL` (zeros - bc))
                go (i + 1) v rest (v : acc)
      counts <- liftJ (go 0 0 logs [])
      let total = sum counts
          omitC = 4096 - total
      jguard (omitC > 0) "JPEG XL: bad ANS histogram"
      pure (take tableSize
              ([ if i == omitPos then omitC else c | (i, c) <- zip [0..] counts ]
               ++ repeat 0))
  where
    isRunStart lc = lc <= -5

newListArr :: [Int] -> ST s (STUArray s Int Int)
newListArr xs = do
  a <- newArray (0, length xs - 1) 0
  forM_ (zip [0..] xs) $ \(i, x) -> unsafeWrite a i x
  pure a

------------------------------------------------------------------------------
-- Reading symbols

-- | Start reading a stream: ANS streams begin with a 32-bit state. The hint
-- bounds how many symbols the stream can hold, sizing the LZ77 window.
newCodeSt :: BR s -> Code -> Int -> ST s (CodeSt s)
newCodeSt br c hint = do
  st <- newArray (0, 7) 0
  unless (cdPrefix c) $ do
    s0 <- readBits br 32
    unsafeWrite st 0 s0
  let wsize = if cdLz c then pow2 (min (1 `shiftL` 20) (max 16 hint)) else 1
      pow2 n = until (>= n) (* 2) 1
  win <- newArray (0, wsize - 1) 0
  pure (CodeSt st win (wsize - 1))

-- | Check the stream ended cleanly (ANS final state, no LZ77 misuse).
finishCode :: Code -> CodeSt s -> J s ()
finishCode c (CodeSt st _ _) = do
  e <- liftJ (unsafeRead st 4)
  jguard (e == 0) "JPEG XL: invalid LZ77 symbol"
  unless (cdPrefix c) $ do
    s <- liftJ (unsafeRead st 0)
    jguard (s == 0x130000) "JPEG XL: corrupt entropy-coded stream"

{-# INLINE clusterOf #-}
clusterOf :: Code -> Int -> Int
clusterOf c ctx = cdClusters c `unsafeAt` ctx

-- | The value a cluster always decodes to, when it can only produce one
-- (and LZ77 is off), else -1.
singleToken :: Code -> Int -> Int
singleToken c cl
  | cdLz c = -1
  | otherwise =
      let s = cdSingle c `unsafeAt` cl
          se = (cdCfg c `unsafeAt` cl) .&. 0xff
      in if s >= 0 && s < (1 `shiftL` se) then s else -1

{-# INLINE readSym #-}
readSym :: Code -> CodeSt s -> BR s -> Int -> Int -> ST s Int
readSym c cs br ctx dm = readSymCl c cs br (cdClusters c `unsafeAt` ctx) dm

-- | Read a value with an already-resolved cluster and LZ77 distance
-- multiplier.
{-# INLINE readSymCl #-}
readSymCl :: Code -> CodeSt s -> BR s -> Int -> Int -> ST s Int
readSymCl c cs br cl dm
  | cdLz c = readLz c cs br cl dm
  | otherwise = do
      t <- readToken c cs br cl
      readUint br (cdCfg c `unsafeAt` cl) t

-- | A symbol reader for one stream. Built by 'withSymReader' as a known
-- constructor and consumed by the INLINE 'readWith', so each call site in a
-- loop inlines the right coder statically: the common configuration (ANS,
-- no LZ77) gets its tables bound once, outside the loop, and the rest go
-- out of line. (A reader passed as a lambda is not inlined into a loop that
-- calls it from more than one place: it stays a closure with a boxed
-- argument and result, 88 bytes a symbol.)
data SymReader s
  = SRAns !(UArray Int Int) !Int !(UArray Int Int) !(STUArray s Int Int) !Buf !(STUArray s Int Int)
  | SRGen !Code !(CodeSt s) !(BR s) !Int

{-# INLINE withSymReader #-}
withSymReader :: Code -> CodeSt s -> BR s -> Int -> (SymReader s -> ST s a) -> ST s a
withSymReader c cs@(CodeSt st _ _) br dm k
  | cdLz c || cdPrefix c = k (SRGen c cs br dm)
  | otherwise = k (SRAns (cdAns c) (cdLogA c) (cdCfg c) st (brBuf br) (brSt br))

{-# INLINE readWith #-}
readWith :: SymReader s -> Int -> ST s Int
readWith (SRAns ans la cfg st buf bst) cl = ansRead ans la cfg st buf bst cl
readWith (SRGen c cs br dm) cl = readSymGen c cs br cl dm

{-# NOINLINE readSymGen #-}
readSymGen :: Code -> CodeSt s -> BR s -> Int -> Int -> ST s Int
readSymGen = readSymCl

{-# INLINE ansRead #-}
ansRead :: UArray Int Int -> Int -> UArray Int Int -> STUArray s Int Int -> Buf
        -> STUArray s Int Int -> Int -> ST s Int
ansRead ans la cfgs st buf bst cl = do
  s <- unsafeRead st 0
  p <- unsafeRead bst 0
  let lb = 12 - la
      idx = s .&. 0xfff
      i = idx `unsafeShiftR` lb
      pos = idx .&. ((1 `unsafeShiftL` lb) - 1)
      b = ans `unsafeAt` ((cl `unsafeShiftL` la) + i)
      cut = (b `unsafeShiftR` 8) .&. 0xff
      alias = pos >= cut
      !t = if alias then b .&. 0xff else i
      d = if alias then (b `unsafeShiftR` 29) .&. 0x1fff else (b `unsafeShiftR` 16) .&. 0x1fff
      off = if alias then (b `unsafeShiftR` 42) - ansBias + pos else pos
      next = (s `unsafeShiftR` 12) * d + off
      !refill = next < 0x10000
      !ra = if refill then 16 else 0
      !v = peekAt buf p .&. ((1 `unsafeShiftL` ra) - 1)
      !p1 = p + ra
      cfg = cfgs `unsafeAt` cl
      !se = cfg .&. 0xff
      !msb = (cfg `unsafeShiftR` 8) .&. 0xff
      !lsb = (cfg `unsafeShiftR` 16) .&. 0xff
      !split = 1 `unsafeShiftL` se
      !ml = msb + lsb
      !big = t >= split
      !nb = if big then (se - ml + ((t - split) `unsafeShiftR` ml)) .&. 31 else 0
      !rest = (peekAt buf p1) .&. ((1 `unsafeShiftL` nb) - 1)
      !low = t .&. ((1 `unsafeShiftL` lsb) - 1)
      !hi = ((t `unsafeShiftR` lsb) .&. ((1 `unsafeShiftL` msb) - 1)) .|. (1 `unsafeShiftL` msb)
  unsafeWrite st 0 (if refill then (next `unsafeShiftL` 16) .|. v else next)
  unsafeWrite bst 0 (p1 + nb)
  pure (if big then (((hi `unsafeShiftL` nb) .|. rest) `unsafeShiftL` lsb) .|. low else t)

-- | One raw token from a cluster's distribution. Both coders are written
-- single-exit (a 0-bit read where a branch would go), so the sample loop
-- that inlines this is not copied once per exit.
{-# INLINE readToken #-}
readToken :: Code -> CodeSt s -> BR s -> Int -> ST s Int
readToken c (CodeSt st _ _) br cl
  | cdPrefix c = do
      p <- brPos br
      let info = cdPfxInfo c `unsafeAt` cl
          tb = info .&. 15
          v = peekAt (brBuf br) p
          e = cdPfxTop c `unsafeAt` ((info `unsafeShiftR` 4) + (v .&. ((1 `unsafeShiftL` tb) - 1)))
          e2 = if e .&. 0x80 == 0 then e
               else cdPfxSub c `unsafeAt`
                      ((e `unsafeShiftR` 8)
                         + ((v `unsafeShiftR` tb) .&. ((1 `unsafeShiftL` (e .&. 0x7f)) - 1)))
      brSetPos br (p + (e2 .&. 0x7f))
      pure (e2 `unsafeShiftR` 8)
  | otherwise = do
      s <- unsafeRead st 0
      let la = cdLogA c
          lb = 12 - la
          idx = s .&. 0xfff
          i = idx `unsafeShiftR` lb
          pos = idx .&. ((1 `unsafeShiftL` lb) - 1)
          b = cdAns c `unsafeAt` ((cl `unsafeShiftL` la) + i)
          cut = (b `unsafeShiftR` 8) .&. 0xff
          alias = pos >= cut
          sym = if alias then b .&. 0xff else i
          d = if alias then (b `unsafeShiftR` 29) .&. 0x1fff else (b `unsafeShiftR` 16) .&. 0x1fff
          off = if alias then (b `unsafeShiftR` 42) - ansBias + pos else pos
          next = (s `unsafeShiftR` 12) * d + off
          refill = next < 0x10000
      v <- readBits br (if refill then 16 else 0)
      unsafeWrite st 0 (if refill then (next `unsafeShiftL` 16) .|. v else next)
      pure sym

-- | The hybrid-uint tail of a token (§C.3.1).
{-# INLINE readUint #-}
readUint :: BR s -> Int -> Int -> ST s Int
readUint br cfg t = do
  let !se = cfg .&. 0xff
      !msb = (cfg `unsafeShiftR` 8) .&. 0xff
      !lsb = (cfg `unsafeShiftR` 16) .&. 0xff
      !split = 1 `unsafeShiftL` se
      !ml = msb + lsb
      !nb = if t >= split then (se - ml + ((t - split) `unsafeShiftR` ml)) .&. 31 else 0
  rest <- readBits br nb
  let !low = t .&. ((1 `unsafeShiftL` lsb) - 1)
      !hi = ((t `unsafeShiftR` lsb) .&. ((1 `unsafeShiftL` msb) - 1)) .|. (1 `unsafeShiftL` msb)
  pure (if t >= split then (((hi `unsafeShiftL` nb) .|. rest) `unsafeShiftL` lsb) .|. low else t)

-- The LZ77 path: out of line, it is the rare configuration.
{-# NOINLINE readLz #-}
readLz :: Code -> CodeSt s -> BR s -> Int -> Int -> ST s Int
readLz c cs@(CodeSt st win mask) br cl dm = do
  ntc <- unsafeRead st 1
  if ntc > 0 then do
    cp <- unsafeRead st 2
    r <- unsafeRead win (cp .&. mask)
    unsafeWrite st 2 (cp + 1)
    unsafeWrite st 1 (ntc - 1)
    push r
  else do
    tok <- readToken c cs br cl
    if tok >= cdLzMinSym c then do
      nd <- unsafeRead st 3
      if nd == 0 then do unsafeWrite st 4 1; push 0 else do
        n <- readUint br (cdLzLenCfg c) (tok - cdLzMinSym c)
        let dcl = cdLzDistCl c
        dtok <- readToken c cs br dcl
        d0 <- readUint br (cdCfg c `unsafeAt` dcl) dtok
        let d1 | dm == 0 = d0
               | d0 < 120 = let o = specialOff `unsafeAt` d0
                                k = specialDist `unsafeAt` d0
                            in max 0 (o + dm * k - 1)
               | otherwise = d0 - 120
            dist = min (min (1 `shiftL` 20 - 1) d1 + 1) nd
            cp = nd - dist
        r <- unsafeRead win (cp .&. mask)
        unsafeWrite st 2 (cp + 1)
        unsafeWrite st 1 (n + cdLzMinLen c - 1)
        push r
    else readUint br (cdCfg c `unsafeAt` cl) tok >>= push
  where
    push r = do
      nd <- unsafeRead st 3
      unsafeWrite win (nd .&. mask) r
      unsafeWrite st 3 (nd + 1)
      pure r

specialPairs :: [(Int, Int)]
specialPairs =
  [ (0,1),(1,0),(1,1),(-1,1),(0,2),(2,0),(1,2),(-1,2),(2,1),(-2,1)
  , (2,2),(-2,2),(0,3),(3,0),(1,3),(-1,3),(3,1),(-3,1),(2,3),(-2,3)
  , (3,2),(-3,2),(0,4),(4,0),(1,4),(-1,4),(4,1),(-4,1),(3,3),(-3,3)
  , (2,4),(-2,4),(4,2),(-4,2),(0,5),(3,4),(-3,4),(4,3),(-4,3),(5,0)
  , (1,5),(-1,5),(5,1),(-5,1),(2,5),(-2,5),(5,2),(-5,2),(4,4),(-4,4)
  , (3,5),(-3,5),(5,3),(-5,3),(0,6),(6,0),(1,6),(-1,6),(6,1),(-6,1)
  , (2,6),(-2,6),(6,2),(-6,2),(4,5),(-4,5),(5,4),(-5,4),(3,6),(-3,6)
  , (6,3),(-6,3),(0,7),(7,0),(1,7),(-1,7),(5,5),(-5,5),(7,1),(-7,1)
  , (4,6),(-4,6),(6,4),(-6,4),(2,7),(-2,7),(7,2),(-7,2),(3,7),(-3,7)
  , (7,3),(-7,3),(5,6),(-5,6),(6,5),(-6,5),(8,0),(4,7),(-4,7),(7,4)
  , (-7,4),(8,1),(8,2),(6,6),(-6,6),(8,3),(5,7),(-5,7),(7,5),(-7,5)
  , (8,4),(6,7),(-6,7),(7,6),(-7,6),(8,5),(7,7),(-7,7),(8,6),(8,7) ]

specialOff, specialDist :: UArray Int Int
specialOff = listArr (map fst specialPairs)
specialDist = listArr (map snd specialPairs)

------------------------------------------------------------------------------
-- Permutations (§C.3.2? Lehmer codes for the TOC and coefficient orders)

-- | Read a permutation of @[0, size)@ whose first @skip@ entries are fixed.
readPermutation :: BR s -> Code -> CodeSt s -> Int -> Int -> J s (UArray Int Int)
readPermutation br c cs size skip = do
  end <- liftJ (readSym c cs br (ctxOf size) 0)
  jguard (end <= size - skip) "JPEG XL: invalid permutation"
  lehmer <- liftJ (let go !i !prev acc
                         | i >= end = pure (Just (reverse acc))
                         | otherwise = do
                             v <- readSym c cs br (ctxOf prev) 0
                             if v >= size - skip - i then pure Nothing
                               else go (i + 1) v (v : acc)
                   in go 0 0 [])
  case lehmer of
    Nothing -> jfail "JPEG XL: invalid permutation"
    Just ls -> pure (applyLehmer size skip ls)
  where ctxOf x = min 7 (ceilLog2 x)

-- Decode a Lehmer code with a Fenwick tree over the remaining elements:
-- O(n log n), where removing from a list would be quadratic in the TOC or a
-- 256x256 coefficient order.
applyLehmer :: Int -> Int -> [Int] -> UArray Int Int
applyLehmer size skip ls = runSTUArray $ do
  let n = size - skip
  out <- newArray (0, max 0 (size - 1)) 0
  forM_ [0 .. skip - 1] $ \i -> unsafeWrite out i i
  fen <- newIntArr n
  -- every remaining slot starts present (count 1)
  forM_ [1 .. n] $ \i -> do
    let addUp j = when (j <= n) $ do
          v <- unsafeRead' fen j
          writeF fen j (v + 1)
          addUp (j + (j .&. negate j))
    addUp i
  used <- newIntArr n
  let logN = until (\b -> (1 `shiftL` b) > n) (+ 1) 0
      kth k = do -- smallest index with prefix sum k+1 (0-based k)
        let go !pos !rem' !b
              | b < 0 = pure pos
              | otherwise = do
                  let np = pos + (1 `shiftL` b)
                  if np <= n then do
                    v <- unsafeRead' fen np
                    if v <= rem' then go np (rem' - v) (b - 1) else go pos rem' (b - 1)
                  else go pos rem' (b - 1)
        p <- go 0 k logN
        pure p -- 0-based slot index
      remove i = let goR j = when (j <= n) $ do
                       v <- unsafeRead' fen j
                       writeF fen j (v - 1)
                       goR (j + (j .&. negate j))
                 in goR (i + 1)
  let place !k [] = do
        -- the rest in order
        let fill !slot !o | slot >= n = pure ()
                          | otherwise = do
                              u <- unsafeRead used slot
                              if u /= 0 then fill (slot + 1) o else do
                                unsafeWrite out o (slot + skip)
                                fill (slot + 1) (o + 1)
        fill 0 k
      place !k (v : vs) = do
        slot <- kth v
        unsafeWrite used slot 1
        remove slot
        unsafeWrite out k (slot + skip)
        place (k + 1) vs
  place skip ls
  pure out
  where
    unsafeRead' a i = Data.Array.Base.unsafeRead a (i - 1)
    writeF a i v = Data.Array.Base.unsafeWrite a (i - 1) v
