{-# LANGUAGE BangPatterns #-}
-- | PDF functions (PDF 32000 §7.10): the maps from parameters to colour
-- components that shadings, Separation/DeviceN tint transforms, transfer
-- functions and soft-mask transfers are written in.
--
-- Four types, all evaluated by one total function 'evalFunction':
--
--   * __Type 0, sampled.__ A table of samples over an @m@-dimensional grid,
--     interpolated /multilinearly/ in every dimension (the @2^m@ corners of
--     the cell the input falls in). @\/Order 3@ asks for cubic spline
--     interpolation; it is read and evaluated linearly, which is what most
--     viewers do and differs from the spline by far less than a colour step
--     on any table a producer actually writes.
--   * __Type 2, exponential.__ @C0 + x^N (C1 - C0)@.
--   * __Type 3, stitching.__ A one-input function made of others over
--     sub-domains, each re-mapped by @\/Encode@. Nested to any depth the file
--     likes, so parsing is depth-limited and reference cycles are cut.
--   * __Type 4, PostScript calculator.__ A small stack language with no
--     loops: parsed once into a tree ('PsCode', with @if@/@ifelse@ resolved
--     at parse time), then run per evaluation under a stack limit (100, the
--     spec's) and an operation budget. A run-time error (underflow, a type
--     error, division by zero) yields the low end of the range rather than an
--     exception: a shading that draws one wrong colour is better than one that
--     stops the page.
--
-- Inputs are clipped to @\/Domain@ and outputs to @\/Range@ (when present),
-- as the spec requires, and a NaN never escapes: 'evalFunction' replaces one
-- with the range's low end (or 0). Parsing is charged to the document's
-- 'Fuel' (a sampled function's table is a stream to decode), and a function
-- whose table would be absurdly large is refused ('maxFuncSamples').
module Cmedit.PdfFunc
  ( PdfFunction(..)
  , PsCode(..)
  , PsOp(..)
  , PsVal(..)
  , parseFunction
  , parseFunctionArray
  , evalFunction
  , funcInputs
  , funcOutputs
  , maxFuncSamples
  , maxPsOps
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.), complement, xor)
import Data.Char (isDigit, isSpace)
import Data.List (foldl')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Array.Unboxed (UArray, listArray, (!), bounds)
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M

import Cmedit.PdfObj

-- | A parsed function. Domains and ranges are lists of @(lo, hi)@ pairs, one
-- per input or output.
data PdfFunction
  = FSampled
      { fsDomain :: [(Double, Double)]
      , fsRange  :: [(Double, Double)]
      , fsSize   :: [Int]
      , fsEncode :: [(Double, Double)]
      , fsOutN   :: !Int
      , fsTable  :: !(UArray Int Double)
          -- ^ Samples already mapped through @\/Decode@, row-major with the
          -- first input varying fastest, @fsOutN@ values per grid point.
      }
  | FExponential
      { feDomain :: [(Double, Double)]
      , feRange  :: Maybe [(Double, Double)]
      , feC0     :: [Double]
      , feC1     :: [Double]
      , feN      :: !Double
      }
  | FStitching
      { ftDomain :: (Double, Double)
      , ftRange  :: Maybe [(Double, Double)]
      , ftFuncs  :: [PdfFunction]
      , ftBounds :: [Double]
      , ftEncode :: [(Double, Double)]
      }
  | FPostScript
      { fpDomain :: [(Double, Double)]
      , fpRange  :: [(Double, Double)]
      , fpCode   :: PsCode
      }
  | FArray [PdfFunction]
      -- ^ Several functions of the same input whose outputs are concatenated:
      -- a shading's or tint transform's @\/Function@ may be an array of
      -- one-output functions, one per colour component.
  deriving (Show)

-- | A PostScript calculator program.
newtype PsCode = PsCode [PsOp]
  deriving (Show)

data PsOp
  = PsPush !PsVal
  | PsOp !BS.ByteString         -- ^ A named operator.
  | PsIf PsCode                 -- ^ @{…} if@
  | PsIfElse PsCode PsCode      -- ^ @{…} {…} ifelse@
  deriving (Show)

data PsVal = PsInt !Int | PsReal !Double | PsBool !Bool
  deriving (Eq, Show)

-- | Most grid points × outputs in one sampled function's table (four
-- million: 32 MB of doubles, far more than any shading needs, a few hundred
-- bytes of file in a hostile one).
maxFuncSamples :: Int
maxFuncSamples = 4 * 1024 * 1024

-- | Most operators in a PostScript calculator program, and so the most a
-- single evaluation can run (the language has no loops).
maxPsOps :: Int
maxPsOps = 100000

-- Function nesting (stitching inside stitching) and functions in one array.
maxFuncDepth, maxFuncArray :: Int
maxFuncDepth = 16
maxFuncArray = 64

-- | How many inputs a function takes (from its domain).
funcInputs :: PdfFunction -> Int
funcInputs fn = case fn of
  FSampled { fsDomain = d } -> length d
  FExponential { feDomain = d } -> length d
  FStitching {} -> 1
  FPostScript { fpDomain = d } -> length d
  FArray (f : _) -> funcInputs f
  FArray [] -> 0

-- | How many outputs it produces.
funcOutputs :: PdfFunction -> Int
funcOutputs fn = case fn of
  FSampled { fsOutN = n } -> n
  FExponential { feC0 = c0, feC1 = c1 } -> max (length c0) (length c1)
  FStitching { ftFuncs = (f : _) } -> funcOutputs f
  FStitching {} -> 0
  FPostScript { fpRange = r } -> length r
  FArray fs -> sum (map funcOutputs fs)

------------------------------------------------------------------------------
-- Parsing

-- | Parse a function object (a dictionary or a stream), or an array of them
-- (see 'FArray'). Charged to the document's fuel: a sampled or PostScript
-- function's stream is decoded.
parseFunction :: PdfFile -> Fuel -> Obj -> (Either String PdfFunction, Fuel)
parseFunction f fu0 o = case resolve f o of
  OArr xs -> parseFunctionArray f fu0 xs
  _       -> parseOne f IS.empty 0 fu0 o

-- | An array of functions, as one 'FArray'.
parseFunctionArray :: PdfFile -> Fuel -> [Obj] -> (Either String PdfFunction, Fuel)
parseFunctionArray f fu0 xs
  | null xs = (Left "empty function array", fu0)
  | otherwise = go fu0 [] (take maxFuncArray xs)
  where
    go fu acc [] = (Right (FArray (reverse acc)), fu)
    go fu acc (x : rest) = case parseOne f IS.empty 0 fu x of
      (Right fn, fu') -> go fu' (fn : acc) rest
      (Left e, fu')   -> (Left e, fu')

parseOne :: PdfFile -> IS.IntSet -> Int -> Fuel -> Obj -> (Either String PdfFunction, Fuel)
parseOne f seen depth fu o
  | depth > maxFuncDepth = (Left "functions nested too deeply", fu)
  | Just r <- refOf o, r `IS.member` seen = (Left "a function that contains itself", fu)
  | fuelSpent fu = (Left "the document's budget is spent", fu)
  | otherwise = case resolve f o of
      ODict d -> byType d Nothing
      OStream d raw -> byType d (Just raw)
      _ -> (Left "not a function", fu)
  where
    seen' = maybe seen (`IS.insert` seen) (refOf o)
    byType d raw = case intOf f (M.lookup (BC.pack "FunctionType") d) of
      0 -> withStream d raw (sampled d)
      2 -> (exponential d, spendTokens 1 fu)
      3 -> stitching d
      4 -> withStream d raw (postscript d)
      t -> (Left ("function type " ++ show t ++ " is not defined"), fu)
    withStream d raw k = case raw of
      Nothing -> (Left "function has no data stream", fu)
      Just bytes -> case decodeStreamF f fu d bytes of
        (Left e, fu')    -> (Left e, fu')
        (Right dat, fu') -> k dat fu'

    domainOf d = pairs (numsOf f (lookD f d "Domain"))
    rangeOf d = case numsOf f (lookD f d "Range") of
      [] -> Nothing
      xs -> Just (pairs xs)

    exponential d =
      let dom = domainOf d
          c0 = case numsOf f (lookD f d "C0") of [] -> [0]; xs -> xs
          c1 = case numsOf f (lookD f d "C1") of [] -> [1]; xs -> xs
          n = numOf f (Just (lookD f d "N"))
      in if null dom then Left "exponential function without a domain"
         else Right (FExponential (take 1 dom) (rangeOf d) c0 c1 n)

    stitching d =
      let dom = domainOf d
          subs = take maxFuncArray (arrOf f (lookD f d "Functions"))
          bnds = numsOf f (lookD f d "Bounds")
          enc = pairs (numsOf f (lookD f d "Encode"))
          go fuel acc [] = (Right (reverse acc), fuel)
          go fuel acc (x : rest) = case parseOne f seen' (depth + 1) fuel x of
            (Right fn, fuel') -> go fuel' (fn : acc) rest
            (Left e, fuel')   -> (Left e, fuel')
      in case dom of
           [] -> (Left "stitching function without a domain", fu)
           (d0 : _) | null subs -> (Left "stitching function with no functions", fu)
                    | otherwise -> case go (spendTokens 1 fu) [] subs of
               (Left e, fu') -> (Left e, fu')
               (Right fns, fu') ->
                 let k = length fns
                     -- Missing Encode pairs default to the natural [0 1].
                     enc' = take k (enc ++ repeat (0, 1))
                 in (Right (FStitching d0 (rangeOf d) fns (take (k - 1) bnds) enc'), fu')

    sampled d dat fuel =
      let dom = domainOf d
          rng = maybe [] id (rangeOf d)
          m = length dom
          nOut = length rng
          size = map (max 1 . truncate) (numsOf f (lookD f d "Size")) :: [Int]
          bps = intOf f (M.lookup (BC.pack "BitsPerSample") d)
          enc = case pairs (numsOf f (lookD f d "Encode")) of
                  e | length e >= m -> take m e
                  _ -> [ (0, fromIntegral (s - 1)) | s <- size ]
          dec = case pairs (numsOf f (lookD f d "Decode")) of
                  e | length e >= nOut -> take nOut e
                  _ -> rng
          points = foldl' (\a s -> if a > maxFuncSamples then a else a * s) 1 size
          total = points * nOut
      in if m == 0 || nOut == 0 then (Left "sampled function without a domain or range", fuel)
         else if length size /= m then (Left "sampled function /Size does not match its domain", fuel)
         else if bps `notElem` [1, 2, 4, 8, 12, 16, 24, 32] then (Left "unsupported /BitsPerSample", fuel)
         else if total > maxFuncSamples || total <= 0 then (Left "sampled function table too large", fuel)
         else
           let maxV = 2 ^ bps - 1 :: Double
               decodeAt i =
                 let (lo, hi) = dec !! (i `mod` nOut)
                 in lo + readBits dat bps i * (hi - lo) / maxV
               table = listArray (0, total - 1) [ decodeAt i | i <- [0 .. total - 1] ]
           in (Right (FSampled dom rng size enc nOut table), spendTokens (total `div` 16 + 1) fuel)

    postscript d dat fuel =
      let dom = domainOf d
          rng = maybe [] id (rangeOf d)
      in if null dom || null rng then (Left "PostScript function without a domain or range", fuel)
         else case parsePs dat of
           Left e -> (Left e, fuel)
           Right (code, nOps) -> (Right (FPostScript dom rng code), spendTokens nOps fuel)

pairs :: [Double] -> [(Double, Double)]
pairs (a : b : rest) = (a, b) : pairs rest
pairs _ = []

-- The @i@-th @bps@-bit unsigned sample of a big-endian bit stream; zero past
-- the end (short data is not an error: the missing samples are black).
readBits :: BS.ByteString -> Int -> Int -> Double
readBits dat bps i
  | bps == 8 = byte i
  | bps == 16 = byte (2 * i) * 256 + byte (2 * i + 1)
  | bps == 24 = (byte (3 * i) * 256 + byte (3 * i + 1)) * 256 + byte (3 * i + 2)
  | bps == 32 = ((byte (4 * i) * 256 + byte (4 * i + 1)) * 256 + byte (4 * i + 2)) * 256 + byte (4 * i + 3)
  | otherwise =
      -- 1, 2, 4 or 12 bits: at most two bytes hold the sample.
      let bit = i * bps
          b0 = bit `shiftR` 3
          two = (ibyte b0 `shiftL` 8) .|. ibyte (b0 + 1)
          sh = 16 - (bit .&. 7) - bps
      in fromIntegral ((two `shiftR` sh) .&. ((1 `shiftL` bps) - 1))
  where
    byte k = fromIntegral (ibyte k) :: Double
    ibyte k = if k >= 0 && k < BS.length dat then fromIntegral (BS.index dat k) else 0 :: Int

------------------------------------------------------------------------------
-- Evaluation

-- | Evaluate a function. Total: missing inputs read as 0, extra ones are
-- ignored, and the result always has 'funcOutputs' entries, each finite.
evalFunction :: PdfFunction -> [Double] -> [Double]
evalFunction fn xs0 = case fn of
  FSampled dom rng size enc nOut table ->
    let xs = clipAll dom xs0
        es = [ clampD 0 (fromIntegral (s - 1)) (interp x d0 d1 e0 e1)
             | (x, (d0, d1), (e0, e1), s) <- zip4 xs dom enc size ]
    in finite rng (clipRange rng (sampleAt size nOut table es))
  FExponential dom rng c0 c1 n ->
    let x = head (clipAll dom xs0 ++ [0])
        k = max (length c0) (length c1)
        xn = x ** n
        ys = [ a + xn * (b - a) | (a, b) <- take k (zip (c0 ++ repeat 0) (c1 ++ repeat 1)) ]
        ys' = if isBad xn then take k (c0 ++ repeat 0) else ys
    in finite (maybe [] id rng) (maybe ys' (`clipRange` ys') rng)
  FStitching (d0, d1) rng fns bnds enc ->
    let x = clampD d0 d1 (head (xs0 ++ [0]))
        k = length fns
        -- The sub-domain x falls in: Bounds are ascending, and x at a bound
        -- belongs to the function on its right (except at the domain's end).
        i = length (takeWhile (<= x) bnds)
        i' = min (k - 1) i
        lo = if i' == 0 then d0 else bnds !! (i' - 1)
        hi = if i' >= length bnds then d1 else bnds !! i'
        (e0, e1) = enc !! i'
        x' = if hi == lo then e0 else interp x lo hi e0 e1
        ys = if k == 0 then [] else evalFunction (fns !! i') [x']
    in finite (maybe [] id rng) (maybe ys (`clipRange` ys) rng)
  FPostScript dom rng code ->
    let xs = clipAll dom xs0
        n = length rng
        start = reverse [ v | v <- map PsReal xs ]   -- top of stack is the last input
        ys = case runPs code start of
          Just st | length st >= n -> map psNum (reverse (take n st))
          _ -> map fst rng
    in finite rng (clipRange rng ys)
  FArray fs -> concatMap (`evalFunction` xs0) fs

-- Replace anything non-finite by the range's low end (or 0).
finite :: [(Double, Double)] -> [Double] -> [Double]
finite rng ys = [ if isBad y then lo else y | (y, lo) <- zip ys (map fst rng ++ repeat 0) ]

isBad :: Double -> Bool
isBad v = isNaN v || isInfinite v

clampD :: Double -> Double -> Double -> Double
clampD lo hi v
  | isNaN v = lo
  | otherwise = max (min lo hi) (min (max lo hi) v)

-- Inputs: clipped per domain pair; missing ones read as the domain's low
-- end clipped (0 clipped), extra ones dropped.
clipAll :: [(Double, Double)] -> [Double] -> [Double]
clipAll dom xs = [ clampD lo hi x | ((lo, hi), x) <- zip dom (xs ++ repeat 0) ]

clipRange :: [(Double, Double)] -> [Double] -> [Double]
clipRange rng ys = [ clampD lo hi y | ((lo, hi), y) <- zip rng ys ]

-- Linear map of x from [a, b] to [c, d].
interp :: Double -> Double -> Double -> Double -> Double -> Double
interp x a b c d
  | b == a = c
  | otherwise = c + (x - a) * (d - c) / (b - a)

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []

-- Multilinear interpolation at fractional grid coordinates @es@: the
-- weighted sum over the 2^m corners of the enclosing cell. A coordinate that
-- sits exactly on a grid line (or a dimension of size 1) contributes one
-- corner, not two, so the common one-input table costs two lookups.
sampleAt :: [Int] -> Int -> UArray Int Double -> [Double] -> [Double]
sampleAt size nOut table es =
  [ sum [ w * at (base + off) j | (off, w) <- corners ] | j <- [0 .. nOut - 1] ]
  where
    strides = scanl (*) 1 size
    -- Per dimension: the lower index, the fraction, and that dimension's stride.
    dims = [ (i0, t, st)
           | (e, s, st) <- zip3' es size strides
           , let i0 = min (s - 1) (max 0 (floor e))
                 t = if i0 >= s - 1 then 0 else e - fromIntegral i0 ]
    base = sum [ i0 * st | (i0, _, st) <- dims ]
    corners = foldl' step [(0, 1)] dims
    step acc (_, t, st)
      | t <= 0 = acc
      | otherwise = [ (o, w * (1 - t)) | (o, w) <- acc ] ++ [ (o + st, w * t) | (o, w) <- acc ]
    (_, hiIx) = bounds table
    at p j = let k = p * nOut + j in if k >= 0 && k <= hiIx then table ! k else 0
    zip3' (a : as) (b : bs) (c : cs) = (a, b, c) : zip3' as bs cs
    zip3' _ _ _ = []

------------------------------------------------------------------------------
-- The PostScript calculator

psNum :: PsVal -> Double
psNum (PsInt i) = fromIntegral i
psNum (PsReal d) = d
psNum (PsBool b) = if b then 1 else 0

-- | Parse a calculator program: @{ … }@, procedures nested for @if@ and
-- @ifelse@. Returns the code and its operator count (bounded by 'maxPsOps').
parsePs :: BS.ByteString -> Either String (PsCode, Int)
parsePs src = case dropWhile (/= "{") (psTokens src) of
  ("{" : rest) -> case block rest 0 (0 :: Int) of
    Right (code, _, n) -> Right (code, n)
    Left e -> Left e
  _ -> Left "PostScript function: no program"
  where
    -- Parse up to the matching '}', resolving `{p} if` and `{p} {q} ifelse`.
    block :: [BS.ByteString] -> Int -> Int -> Either String (PsCode, [BS.ByteString], Int)
    block toks0 n0 depth
      | depth > 64 = Left "PostScript function nested too deeply"
      | otherwise = go toks0 n0 []
      where
        go [] _ _ = Left "PostScript function: unterminated program"
        go (t : ts) n acc
          | n > maxPsOps = Left "PostScript function too long"
          | t == "}" = Right (PsCode (reverse acc), ts, n)
          | t == "{" = case block ts n (depth + 1) of
              Left e -> Left e
              Right (p, ts', n') -> case ts' of
                ("if" : more) -> go more (n' + 1) (PsIf p : acc)
                ("{" : more) -> case block more n' (depth + 1) of
                  Left e -> Left e
                  Right (q, ("ifelse" : more'), n'') -> go more' (n'' + 1) (PsIfElse p q : acc)
                  Right _ -> Left "PostScript function: two procedures without ifelse"
                _ -> Left "PostScript function: a procedure without if"
          | Just v <- psNumber t = go ts (n + 1) (PsPush v : acc)
          | t == "true" = go ts (n + 1) (PsPush (PsBool True) : acc)
          | t == "false" = go ts (n + 1) (PsPush (PsBool False) : acc)
          | t `elem` psOperators = go ts (n + 1) (PsOp t : acc)
          | otherwise = Left ("PostScript function: unknown operator " ++ BC.unpack (BS.take 32 t))

psOperators :: [BS.ByteString]
psOperators = map BC.pack
  [ "abs", "add", "atan", "ceiling", "cos", "cvi", "cvr", "div", "exp", "floor"
  , "idiv", "ln", "log", "mod", "mul", "neg", "round", "sin", "sqrt", "sub"
  , "truncate", "and", "bitshift", "eq", "ge", "gt", "le", "lt", "ne", "not"
  , "or", "xor", "copy", "dup", "exch", "index", "pop", "roll" ]

-- Braces are tokens of their own; everything else splits on whitespace and
-- PostScript delimiters. Comments run to the end of the line.
psTokens :: BS.ByteString -> [BS.ByteString]
psTokens s0 = go s0
  where
    go s = case BC.uncons (BC.dropWhile isSpace s) of
      Nothing -> []
      Just (c, rest)
        | c == '{' || c == '}' -> BC.singleton c : go rest
        | c == '%' -> go (BC.dropWhile (\x -> x /= '\n' && x /= '\r') rest)
        | otherwise ->
            let (tok, more) = BC.span (\x -> not (isSpace x) && x `notElem` ("{}%()<>[]/" :: String)) (BC.cons c rest)
            in if BS.null tok then go rest else tok : go more

-- An integer, a real (with optional exponent) or a radix number (@16#FF@).
psNumber :: BS.ByteString -> Maybe PsVal
psNumber t
  | BS.null t = Nothing
  | Just (b, ds) <- radix = PsInt <$> digitsIn b ds
  | isIntTok = Just (PsInt (fromIntegral (clampInt (readSignedInt t))))
  | isRealTok = Just (PsReal (readReal t))
  | otherwise = Nothing
  where
    s = BC.unpack t
    body = case s of ('+' : r) -> r; ('-' : r) -> r; r -> r
    isIntTok = not (null body) && all isDigit body
    isRealTok =
      let (mant, ex) = break (\c -> c == 'e' || c == 'E') body
          (ip, fp) = break (== '.') mant
          okMant = not (null mant) && all isDigit ip && (null fp || all isDigit (drop 1 fp))
                   && (not (null ip) || length fp > 1)
          okEx = case ex of
                   [] -> True
                   (_ : r) -> let r' = case r of ('+' : q) -> q; ('-' : q) -> q; q -> q
                              in not (null r') && all isDigit r' && length r' <= 4
      in okMant && okEx
    radix = case break (== '#') s of
      (b, '#' : ds) | not (null b), all isDigit b, length b <= 2 -> Just (read b :: Int, ds)
      _ -> Nothing
    digitsIn b ds
      | b < 2 || b > 36 || null ds || length ds > 32 = Nothing
      | otherwise = foldl' (\acc c -> do a <- acc; v <- digitVal c; if v < b then Just (a * b + v) else Nothing) (Just 0) ds
    digitVal c | isDigit c = Just (fromEnum c - 48)
               | c >= 'a' && c <= 'z' = Just (fromEnum c - 87)
               | c >= 'A' && c <= 'Z' = Just (fromEnum c - 55)
               | otherwise = Nothing
    clampInt :: Integer -> Integer
    clampInt v = max (-2147483648) (min 2147483647 v)
    readSignedInt :: BS.ByteString -> Integer
    readSignedInt x = case BC.unpack x of
      ('-' : r) -> negate (digitsI r)
      ('+' : r) -> digitsI r
      r -> digitsI r
    digitsI r = foldl' (\a c -> a * 10 + fromIntegral (fromEnum c - 48)) 0 (take 20 r)
    readReal x =
      let str = BC.unpack x
          (sgn, r) = case str of ('-' : q) -> (-1, q); ('+' : q) -> (1, q); q -> (1, q)
          (mant, ex) = break (\c -> c == 'e' || c == 'E') r
          (ip, fp) = break (== '.') mant
          whole = foldl' (\a c -> a * 10 + fromIntegral (fromEnum c - 48)) 0 (take 20 ip) :: Double
          frac = foldr (\c a -> (a + fromIntegral (fromEnum c - 48)) / 10) 0 (take 20 (drop 1 fp)) :: Double
          e = case ex of
                [] -> 0
                (_ : q) -> case q of
                  ('-' : d) -> negate (read d)
                  ('+' : d) -> read d
                  d -> read d :: Int
      in sgn * (whole + frac) * 10 ^^ e

-- Run a program on a stack (head = top). 'Nothing' on any run-time error.
runPs :: PsCode -> [PsVal] -> Maybe [PsVal]
runPs code st0 = fmap fst (exec code st0 0)
  where
    exec :: PsCode -> [PsVal] -> Int -> Maybe ([PsVal], Int)
    exec (PsCode ops) st n = foldl' (\acc op -> acc >>= \(s, k) -> step op s k) (Just (st, n)) ops

    step :: PsOp -> [PsVal] -> Int -> Maybe ([PsVal], Int)
    step op st !n
      | n > maxPsOps = Nothing
      | otherwise = case op of
          PsPush v -> push v st (n + 1)
          PsIf p -> case st of
            (PsBool b : rest) -> if b then exec p rest (n + 1) else Just (rest, n + 1)
            _ -> Nothing
          PsIfElse p q -> case st of
            (PsBool b : rest) -> exec (if b then p else q) rest (n + 1)
            _ -> Nothing
          PsOp name -> do
            st' <- prim (BC.unpack name) st
            if length st' > 100 then Nothing else Just (st', n + 1)

    push v st n = if length st >= 100 then Nothing else Just (v : st, n)

    prim :: String -> [PsVal] -> Maybe [PsVal]
    prim name st = case (name, st) of
      ("abs", PsInt a : r) -> Just (intOrReal (abs (toInteger a)) : r)
      ("abs", PsReal a : r) -> Just (PsReal (abs a) : r)
      ("neg", PsInt a : r) -> Just (intOrReal (negate (toInteger a)) : r)
      ("neg", PsReal a : r) -> Just (PsReal (negate a) : r)
      ("add", b : a : r) -> arith (+) (+) a b r
      ("sub", b : a : r) -> arith (-) (-) a b r
      ("mul", b : a : r) -> arith (*) (*) a b r
      ("div", b : a : r) -> do x <- num a; y <- num b
                               if y == 0 then Nothing else Just (PsReal (x / y) : r)
      ("idiv", PsInt b : PsInt a : r) -> if b == 0 then Nothing else Just (intOrReal (toInteger a `quot` toInteger b) : r)
      ("mod", PsInt b : PsInt a : r) -> if b == 0 then Nothing else Just (PsInt (a `rem` b) : r)
      ("ceiling", v : r) -> rounding ceiling v r
      ("floor", v : r) -> rounding floor v r
      ("round", v : r) -> rounding (\x -> floor (x + 0.5)) v r
      ("truncate", v : r) -> rounding truncate v r
      ("sqrt", v : r) -> do x <- num v; if x < 0 then Nothing else Just (PsReal (sqrt x) : r)
      ("sin", v : r) -> do x <- num v; Just (PsReal (sin (x * pi / 180)) : r)
      ("cos", v : r) -> do x <- num v; Just (PsReal (cos (x * pi / 180)) : r)
      ("atan", d : nn : r) -> do
        y <- num nn; x <- num d
        if x == 0 && y == 0 then Nothing else
          let a = atan2 y x * 180 / pi in Just (PsReal (if a < 0 then a + 360 else a) : r)
      ("exp", e : b : r) -> do x <- num b; y <- num e
                               let v = x ** y
                               if isBad v then Nothing else Just (PsReal v : r)
      ("ln", v : r) -> do x <- num v; if x <= 0 then Nothing else Just (PsReal (log x) : r)
      ("log", v : r) -> do x <- num v; if x <= 0 then Nothing else Just (PsReal (logBase 10 x) : r)
      ("cvi", v : r) -> do x <- num v
                           if isBad x || abs x > 2147483647 then Nothing else Just (PsInt (truncate x) : r)
      ("cvr", v : r) -> do x <- num v; Just (PsReal x : r)
      ("eq", b : a : r) -> Just (PsBool (psEq a b) : r)
      ("ne", b : a : r) -> Just (PsBool (not (psEq a b)) : r)
      ("gt", b : a : r) -> cmp (>) a b r
      ("ge", b : a : r) -> cmp (>=) a b r
      ("lt", b : a : r) -> cmp (<) a b r
      ("le", b : a : r) -> cmp (<=) a b r
      ("and", b : a : r) -> logic (&&) (.&.) a b r
      ("or", b : a : r) -> logic (||) (.|.) a b r
      ("xor", b : a : r) -> logic (/=) xor a b r
      ("not", PsBool a : r) -> Just (PsBool (not a) : r)
      ("not", PsInt a : r) -> Just (PsInt (complement a) : r)
      ("bitshift", PsInt s : PsInt a : r)
        | s >= 32 || s <= -32 -> Just (PsInt 0 : r)
        | s >= 0 -> Just (PsInt (wrap32 (a `shiftL` s)) : r)
        | otherwise -> Just (PsInt (a `shiftR` negate s) : r)
      ("dup", a : r) -> Just (a : a : r)
      ("pop", _ : r) -> Just r
      ("exch", b : a : r) -> Just (a : b : r)
      ("copy", PsInt k : r) | k >= 0 && k <= length r -> Just (take k r ++ r)
      ("index", PsInt k : r) | k >= 0 && k < length r -> Just (r !! k : r)
      ("roll", PsInt j : PsInt k : r) | k >= 0 && k <= length r ->
        let (top, rest) = splitAt k r
            -- PostScript lists the stack bottom-to-top; 'top' is top-first.
            bottomFirst = reverse top
            j' = if k == 0 then 0 else j `mod` k
            rolled = drop (k - j') bottomFirst ++ take (k - j') bottomFirst
        in Just (reverse rolled ++ rest)
      _ -> Nothing

    num (PsInt i) = Just (fromIntegral i)
    num (PsReal d) = Just d
    num (PsBool _) = Nothing

    intOrReal :: Integer -> PsVal
    intOrReal v | v >= -2147483648 && v <= 2147483647 = PsInt (fromIntegral v)
                | otherwise = PsReal (fromIntegral v)

    wrap32 :: Int -> Int
    wrap32 v = let w = v .&. 0xffffffff in if w >= 0x80000000 then w - 0x100000000 else w

    arith :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> PsVal -> PsVal -> [PsVal] -> Maybe [PsVal]
    arith fi fd a b r = case (a, b) of
      (PsInt x, PsInt y) -> Just (intOrReal (fi (toInteger x) (toInteger y)) : r)
      _ -> do x <- num a; y <- num b; Just (PsReal (fd x y) : r)

    rounding :: (Double -> Integer) -> PsVal -> [PsVal] -> Maybe [PsVal]
    rounding g v r = case v of
      PsInt i -> Just (PsInt i : r)
      PsReal d | isBad d -> Nothing
               | abs d > 1e300 -> Just (PsReal d : r)
               | otherwise -> Just (PsReal (fromInteger (g d)) : r)
      PsBool _ -> Nothing

    psEq a b = case (a, b) of
      (PsBool x, PsBool y) -> x == y
      (PsBool _, _) -> False
      (_, PsBool _) -> False
      _ -> num a == num b

    cmp :: (Double -> Double -> Bool) -> PsVal -> PsVal -> [PsVal] -> Maybe [PsVal]
    cmp op a b r = do x <- num a; y <- num b; Just (PsBool (op x y) : r)

    logic :: (Bool -> Bool -> Bool) -> (Int -> Int -> Int) -> PsVal -> PsVal -> [PsVal] -> Maybe [PsVal]
    logic fb fi a b r = case (a, b) of
      (PsBool x, PsBool y) -> Just (PsBool (fb x y) : r)
      (PsInt x, PsInt y) -> Just (PsInt (fi x y) : r)
      _ -> Nothing
