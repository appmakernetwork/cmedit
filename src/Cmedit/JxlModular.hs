{-# LANGUAGE BangPatterns, MagicHash #-}
-- | JPEG XL Modular mode (ISO/IEC 18181-1 Annex H): integer channels coded
-- by context-modelled prediction, plus the reversible transforms (RCT,
-- palette, squeeze) applied over them.
--
-- Every channel is a flat row-major 'Int32' plane. The per-sample decode
-- ('decodeChannel') is the hot loop of every lossless JPEG XL, and of the
-- LF and metadata of every lossy one, so the meta-adaptive tree is first
-- specialised to the channel ('specialise': the channel index, the stream
-- index and the availability of previous channels are constants per channel,
-- so every decision on them is resolved before the first sample), and the
-- common shapes then get loops of their own:
--
--  * a single leaf with the zero predictor and a one-symbol distribution
--    fills the channel without reading a bit;
--  * a single gradient leaf (libjxl's fast lossless and effort 1–2) runs a
--    loop that computes no properties at all;
--  * everything else walks the specialised tree per sample, and pays for the
--    weighted predictor only when a reachable leaf or decision needs it.
module Cmedit.JxlModular
  ( Chan(..), ChInfo(..), newChan, chanInfo
  , MATree, readTree
  , WPP, Transform, ModHeader(..), readModHeader
  , decodeChannels, inverseTransforms
  , CompactChan(..), inverseTransformsCompact, materializeChan
  , rdC, wrC
  ) where

import Control.Monad (forM, forM_, unless, when, foldM)
import Control.Monad.ST (ST)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, newArray)
import Data.Array.Unboxed (UArray, listArray)
import qualified Data.Array.Unboxed as U
import GHC.Exts (Int(I#), (<#))
import Data.Bits
import Data.Int (Int32)
import Data.List (foldl')

import Cmedit.JxlBits
import Cmedit.JxlEntropy

-- | A channel: geometry, shifts (-1 for the unshiftable palette meta
-- channel) and a row-major plane.
data Chan s = Chan
  { chW, chH, chHS, chVS :: !Int
  , chBuf :: !(STUArray s Int Int32)
  }

data ChInfo = ChInfo { ciW, ciH, ciHS, ciVS :: !Int }
  deriving (Eq, Show)

-- | A zeroed channel. The plane has one cell more than it needs: index
-- @w * h@ stays 0 forever, and stands in for every missing neighbour.
newChan :: ChInfo -> ST s (Chan s)
newChan (ChInfo w h hs vs) = Chan w h hs vs <$> newArray (0, max 0 (w * h)) 0

chanInfo :: Chan s -> ChInfo
chanInfo c = ChInfo (chW c) (chH c) (chHS c) (chVS c)

{-# INLINE rdC #-}
rdC :: STUArray s Int Int32 -> Int -> ST s Int
rdC a i = fromIntegral <$> unsafeRead a i

{-# INLINE wrC #-}
wrC :: STUArray s Int Int32 -> Int -> Int -> ST s ()
wrC a i v = unsafeWrite a i (fromIntegral v)

------------------------------------------------------------------------------
-- The meta-adaptive tree (§H.4.2)

-- | Decision nodes have a property >= 0, a split value and a first child
-- (the "greater than" side; the other child is the next node). Leaves have
-- property -1 and carry a cluster, predictor, offset and multiplier.
data MATree = MATree
  { mtProp, mtVal, mtKid :: !(UArray Int Int)
  , mtCl, mtPred, mtOff, mtMul :: !(UArray Int Int)
  , mtCode :: !Code
  }

readTree :: BR s -> Int -> J s MATree
readTree br maxNodes = do
  tc <- readCode br 6
  st <- liftJ (newCodeSt br tc 4096)
  let loop !i !left !ctx acc
        | left <= 0 = pure (Right (reverse acc, ctx))
        | i >= maxNodes = pure (Left "JPEG XL: MA tree too large")
        | otherwise = do
            let left' = left - 1
            p <- readSym tc st br 1 0
            if p > 0 then do
              v <- unpackSigned <$> readSym tc st br 0 0
              -- children take the next two free indices after the queue
              loop (i + 1) (left' + 2) ctx ((p - 1, v, i + left' + 1, 0, 0, 0) : acc)
            else do
              pr <- readSym tc st br 2 0
              off <- unpackSigned <$> readSym tc st br 3 0
              sh <- readSym tc st br 4 0
              mv <- readSym tc st br 5 0
              if pr > 13 || sh > 30 || ((mv + 1) `shiftR` (31 - sh)) /= 0
                then pure (Left "JPEG XL: bad MA tree leaf")
                else loop (i + 1) left' (ctx + 1)
                       ((-1, ctx, pr, off, (mv + 1) `shiftL` sh, 0) : acc)
  r <- liftJ (loop 0 1 0 [])
  (nodes, nctx) <- either jfail pure r
  finishCode tc st
  code <- readCode br nctx
  let n = length nodes
      arr f = listArray (0, n - 1) (map f nodes) :: UArray Int Int
      isLeaf (p, _, _, _, _, _) = p < 0
  pure MATree
    { mtProp = arr (\(p, _, _, _, _, _) -> p)
    , mtVal  = arr (\t@(_, v, _, _, _, _) -> if isLeaf t then 0 else v)
    , mtKid  = arr (\t@(_, _, k, _, _, _) -> if isLeaf t then 0 else k)
    , mtCl   = arr (\t@(_, c, _, _, _, _) -> if isLeaf t then clusterOf code c else 0)
    , mtPred = arr (\t@(_, _, pr, _, _, _) -> if isLeaf t then pr else 0)
    , mtOff  = arr (\t@(_, _, _, o, _, _) -> if isLeaf t then o else 0)
    , mtMul  = arr (\t@(_, _, _, _, m, _) -> if isLeaf t then m else 0)
    , mtCode = code
    }

-- | A tree specialised to one channel: same layout, with every decision on
-- the channel index, stream index or a missing previous channel resolved.
data FTree = FTree
  { ftProp, ftVal, ftKid :: !(UArray Int Int)
  , ftCl, ftPred, ftOff, ftMul :: !(UArray Int Int)
  }

specialise :: MATree -> Int -> Int -> Int -> FTree
specialise t chan sidx nprev =
  let -- follow static decisions from node i
      resolve !i =
        let p = mtProp t `unsafeAt` i
            v = mtVal t `unsafeAt` i
            k = mtKid t `unsafeAt` i
            go val = resolve (if val > v then k else k + 1)
        in if p == 0 then go chan
           else if p == 1 then go sidx
           else if p >= 16 && (p - 16) `shiftR` 2 >= nprev then go 0
           else i
      -- breadth-first copy of the reachable dynamic tree
      build [] _ acc = reverse acc
      build (i : queue) !next acc =
        let p = mtProp t `unsafeAt` i
        in if p < 0
             then build queue next ((-1, 0, 0, mtCl t `unsafeAt` i, mtPred t `unsafeAt` i
                                     , mtOff t `unsafeAt` i, mtMul t `unsafeAt` i) : acc)
             else let k = mtKid t `unsafeAt` i
                      l = resolve k
                      r = resolve (k + 1)
                  in build (queue ++ [l, r]) (next + 2)
                       ((p, mtVal t `unsafeAt` i, next, 0, 0, 0, 0) : acc)
      nodes = build [resolve 0] 1 []
      n = length nodes
      arr f = listArray (0, n - 1) (map f nodes) :: UArray Int Int
  in FTree (arr (\(a,_,_,_,_,_,_) -> a)) (arr (\(_,a,_,_,_,_,_) -> a))
           (arr (\(_,_,a,_,_,_,_) -> a)) (arr (\(_,_,_,a,_,_,_) -> a))
           (arr (\(_,_,_,_,a,_,_) -> a)) (arr (\(_,_,_,_,_,a,_) -> a))
           (arr (\(_,_,_,_,_,_,a) -> a))

------------------------------------------------------------------------------
-- Modular header and transform bookkeeping (§H.2, §H.6)

-- | Weighted-predictor parameters: p1, p2, p3a..p3e, w0..w3.
type WPP = UArray Int Int

data SqParam = SqParam { sqHor, sqInPlace :: !Bool, sqBegin, sqNum :: !Int }

data Transform
  = TrRct !Int !Int                      -- begin, type
  | TrPalette !Int !Int !Int !Int !Int   -- begin, numC, colours, deltas, predictor
  | TrSqueeze [SqParam]

data ModHeader = ModHeader
  { mhGlobalTree :: !Bool
  , mhWP         :: !WPP
  , mhTransforms :: [Transform]
  , mhChannels   :: [ChInfo]   -- ^ after the transforms
  , mhNbMeta     :: !Int
  }

readModHeader :: BR s -> [ChInfo] -> J s ModHeader
readModHeader br chans = do
  useGlobal <- liftJ (readBool br)
  dflt <- liftJ (readBool br)
  wpp <- if dflt then pure (listArray (0, 10) [16, 10, 7, 7, 7, 0, 0, 13, 12, 12, 12])
         else liftJ $ do
           ps <- mapM (readBits br) [5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4]
           pure (listArray (0, 10) ps)
  nbt <- liftJ (u32 br (0,0) (1,0) (2,4) (18,8))
  jguard (nbt <= 512) "JPEG XL: too many transforms"
  let step (cs, nbMeta, trs) _ = do
        tid <- liftJ (readBits br 2)
        case tid of
          0 -> do
            b <- liftJ (u32 br (0,3) (8,6) (72,10) (1096,13))
            ty <- liftJ (u32 br (6,0) (0,2) (2,4) (10,6))
            jguard (ty < 42 && b + 3 <= length cs) "JPEG XL: bad RCT"
            let [c0, c1, c2] = take 3 (drop b cs)
            jguard (sameSize c0 c1 && sameSize c0 c2) "JPEG XL: bad RCT"
            pure (cs, nbMeta, TrRct b ty : trs)
          1 -> do
            b <- liftJ (u32 br (0,3) (8,6) (72,10) (1096,13))
            nc <- liftJ (u32 br (1,0) (3,0) (4,0) (1,13))
            ncol <- liftJ (u32 br (0,8) (256,10) (1280,12) (5376,16))
            nd <- liftJ (u32 br (0,0) (1,8) (257,10) (1281,16))
            dp <- liftJ (readBits br 4)
            let e = b + nc
            jguard (dp < 14 && e <= length cs) "JPEG XL: bad palette"
            jguard (b >= nbMeta || e <= nbMeta) "JPEG XL: bad palette"
            let grp = take nc (drop b cs)
            jguard (all (sameSize (head grp)) grp) "JPEG XL: bad palette"
            let nbMeta' = if b < nbMeta then nbMeta + 2 - nc else nbMeta + 1
                cs' = ChInfo ncol nc (-1) (-1) : take (b + 1) cs ++ drop e cs
            pure (cs', nbMeta', TrPalette b nc ncol nd dp : trs)
          2 -> do
            nsq <- liftJ (u32 br (0,0) (1,4) (9,6) (41,8))
            sps0 <- liftJ $ forM [1 .. nsq] $ \_ -> do
              h <- readBool br
              ip <- readBool br
              b <- u32 br (0,3) (8,6) (72,10) (1096,13)
              n <- u32 br (1,0) (2,0) (3,0) (4,4)
              pure (SqParam h ip b n)
            let sps = if null sps0 then defaultSqueeze cs nbMeta else sps0
            (cs', nbMeta') <- foldM squeezeInfo (cs, nbMeta) sps
            pure (cs', nbMeta', TrSqueeze sps : trs)
          _ -> jfail "JPEG XL: unknown transform"
  (cs, nbMeta, trs) <- foldM step (chans, 0, []) [1 .. nbt]
  jguard (length cs <= 65536) "JPEG XL: too many channels"
  pure (ModHeader useGlobal wpp (reverse trs) cs nbMeta)
  where
    sameSize a b = ciW a == ciW b && ciH a == ciH b

-- libjxl's default squeeze script, when the transform gives no parameters.
defaultSqueeze :: [ChInfo] -> Int -> [SqParam]
defaultSqueeze cs first
  | first >= length cs = []
  | otherwise =
  let c0 = cs !! first
      w0 = ciW c0
      h0 = ciH c0
      chroma = if length cs - first >= 3 && ciW (cs !! (first + 1)) == w0 && ciH (cs !! (first + 1)) == h0
                 then [SqParam True False (first + 1) 2, SqParam False False (first + 1) 2]
                 else []
      base hor = SqParam hor True first (length cs - first)
      (pre, h1) = if h0 >= w0 && h0 > 8 then ([base False], (h0 + 1) `div` 2) else ([], h0)
      go w h | w <= 8 && h <= 8 = []
             | otherwise =
                 let (a, w') = if w > 8 then ([base True], (w + 1) `div` 2) else ([], w)
                     (b, h') = if h > 8 then ([base False], (h + 1) `div` 2) else ([], h)
                 in a ++ b ++ go w' h'
  in chroma ++ pre ++ go w0 h1

squeezeInfo :: ([ChInfo], Int) -> SqParam -> J s ([ChInfo], Int)
squeezeInfo (cs, nbMeta) (SqParam hor inPlace b n) = do
  let e = b + n
  jguard (e <= length cs) "JPEG XL: bad squeeze"
  nbMeta' <- if b < nbMeta
    then do jguard (inPlace && e <= nbMeta) "JPEG XL: bad squeeze"
            pure (nbMeta + n)
    else pure nbMeta
  let targets = take n (drop b cs)
  jguard (all (\c -> ciW c > 0 && ciH c > 0 && ciHS c <= 30 && ciVS c <= 30) targets)
    "JPEG XL: bad squeeze"
  let halve c | hor = ( c { ciW = (ciW c + 1) `div` 2, ciHS = bumpS (ciHS c) }
                      , c { ciW = ciW c `div` 2, ciHS = bumpS (ciHS c) } )
              | otherwise = ( c { ciH = (ciH c + 1) `div` 2, ciVS = bumpS (ciVS c) }
                            , c { ciH = ciH c `div` 2, ciVS = bumpS (ciVS c) } )
      bumpS s = if s >= 0 then s + 1 else s
      pairs = map halve targets
      kept = take b cs ++ map fst pairs
      resid = map snd pairs
      cs' = if inPlace then kept ++ resid ++ drop e cs
                       else kept ++ drop e cs ++ resid
  pure (cs', nbMeta')

------------------------------------------------------------------------------
-- Channel decoding (§H.5)

-- | Decode channels @[0, n)@ of a stream (the rest are decoded elsewhere,
-- by groups). The entropy stream starts here and is finished here.
decodeChannels :: BR s -> MATree -> WPP -> Int -> [Chan s] -> Int -> J s ()
decodeChannels br tree wpp sidx chans n = do
  let todo = take n chans
      dm = maximum (0 : [ chW c | c <- todo ])
      code = mtCode tree
      hint = sum [ chW c * chH c | c <- todo ]
  cs <- liftJ (newCodeSt br code hint)
  let go _ [] = pure ()
      go i (c : rest) = do
        let prevs = [ p | p <- reverse (take i todo), compatible p c ]
            ft = specialise tree i sidx (length prevs)
        unless (chW c == 0 || chH c == 0) $
          liftJ (decodeChannel br code cs wpp ft dm prevs c)
        go (i + 1) rest
  go 0 todo
  finishCode code cs
  where
    compatible p c = chW p == chW c && chH p == chH c && chHS p == chHS c && chVS p == chVS c

decodeChannel :: BR s -> Code -> CodeSt s -> WPP -> FTree -> Int -> [Chan s] -> Chan s -> ST s ()
decodeChannel br code cs wpp ft dm prevs ch
  | singleLeaf && pr0 == 0 && singleToken code cl0 >= 0 = do
      let v = unpackSigned (singleToken code cl0) * mul0 + off0
      forLoop 0 (w * h - 1) $ \i -> wrC buf i v
  | singleLeaf && pr0 == 5 && off0 == 0 && mul0 == 1 =
      withSymReader code cs br dm $ \sr -> gradientLoop sr (\_ _ _ -> cl0) ch
  | Just (!p, !lo, !tab, !pr, !off, !mul) <- simpleTable ft =
      let leafOf v = tab `unsafeAt` max 0 (min (snd (U.bounds tab)) (v - lo))
          clOf v = ftCl ft `unsafeAt` leafOf v
      in if p == 9 && pr == 5 && off == 0 && mul == 1
           then withSymReader code cs br dm $ \sr ->
                  gradientLoop sr (\wv nv nwv -> clOf (wv + nv - nwv)) ch
           else if p == 15 && pr == 6 && off == 0 && mul == 1
             then
               let {-# INLINE run #-}
                   run sr = wpLoop sr wpp ch clOf (const 6) (const 0) (const 1) $ \_ _ _ _ _ _ _ _ _ maxErr k ->
                     k maxErr
               in withSymReader code cs br dm run
           else if p == 15 || pr == 6
             then
               let {-# INLINE run #-}
                   run sr = wpLoop sr wpp ch clOf (const pr) (const off) (const mul) $ \x y wv nv nwv nev nnv wwv nwwv maxErr k ->
                     k (propPure p x y wv nv nwv nev nnv wwv nwwv maxErr)
               in withSymReader code cs br dm run
             else withSymReader code cs br dm $ \sr -> tableLoop sr p clOf pr off mul ch
  -- A common mixed gradient/WP tree uses only these five properties.
  -- Check the complete tree once; all other shapes keep the general walk.
  | needWP && localTree =
      let {-# INLINE run #-}
          run sr = wpLoop sr wpp ch (\leaf -> ftCl ft `unsafeAt` leaf)
                     (\leaf -> ftPred ft `unsafeAt` leaf) (const 0) (const 1) $
            \_ _ wv nv nwv nev _ _ _ maxErr k ->
              let !v9 = wv + nv - nwv
                  !v10 = wv - nwv
                  !v11 = nwv - nv
                  !v12 = nv - nev
                  walk !i =
                    let p = ftProp ft `unsafeAt` i
                    in if p < 0 then k i else
                         let v = case p of
                               9 -> v9
                               10 -> v10
                               11 -> v11
                               12 -> v12
                               _ -> maxErr
                             kid = ftKid ft `unsafeAt` i
                         in walk (if v > ftVal ft `unsafeAt` i then kid else kid + 1)
              in walk 0
      in withSymReader code cs br dm run
  | needWP = withSymReader code cs br dm $ \sr ->
      wpLoop sr wpp ch (\leaf -> ftCl ft `unsafeAt` leaf) (\leaf -> ftPred ft `unsafeAt` leaf)
        (\leaf -> ftOff ft `unsafeAt` leaf) (\leaf -> ftMul ft `unsafeAt` leaf) $ \x y wv nv nwv nev nnv wwv nwwv maxErr k ->
        let walk !i = do
              let p = ftProp ft `unsafeAt` i
              if p < 0 then k i else do
                v <- if p >= 16 then prevProp prevs (p - 16) x y
                     else pure (propPure p x y wv nv nwv nev nnv wwv nwwv maxErr)
                let kid = ftKid ft `unsafeAt` i
                walk (if v > ftVal ft `unsafeAt` i then kid else kid + 1)
        in walk 0
  | otherwise = withSymReader code cs br dm $ \sr -> generalLoop sr ft prevs ch
  where
    w = chW ch
    h = chH ch
    buf = chBuf ch
    singleLeaf = ftProp ft `unsafeAt` 0 < 0
    cl0 = ftCl ft `unsafeAt` 0
    pr0 = ftPred ft `unsafeAt` 0
    off0 = ftOff ft `unsafeAt` 0
    mul0 = ftMul ft `unsafeAt` 0
    nn = snd (U.bounds (ftProp ft)) + 1
    localTree = all eligible [0 .. nn - 1]
      where eligible i =
              let p = ftProp ft `unsafeAt` i
              in if p >= 0 then p == 9 || p == 10 || p == 11 || p == 12 || p == 15
                   else (ftPred ft `unsafeAt` i == 5 || ftPred ft `unsafeAt` i == 6)
                        && ftOff ft `unsafeAt` i == 0 && ftMul ft `unsafeAt` i == 1
    needWP = or [ (ftProp ft `unsafeAt` i == 15) || (ftProp ft `unsafeAt` i < 0 && ftPred ft `unsafeAt` i == 6)
                | i <- [0 .. nn - 1] ]

-- | A tree that decides on one property only, with every leaf sharing its
-- predictor, offset and multiplier, is a lookup from that property's value
-- to a cluster (libjxl writes these at low efforts: a table on the gradient
-- property, or on the weighted predictor's error). Values at or below the
-- smallest split all behave alike, as do those above the largest, so the
-- table spans [lo, hi + 1] and lookups clamp.
simpleTable :: FTree -> Maybe (Int, Int, UArray Int Int, Int, Int, Int)
simpleTable ft
  | n < 2 = Nothing
  | not (all (== p0) dprops) = Nothing
  | p0 < 2 || p0 > 15 = Nothing
  | not (all (== head leaves) leaves) = Nothing
  | hi - lo > 1024 = Nothing
  | otherwise =
      let (pr, off, mul) = head leaves
          tab = listArray (0, hi - lo + 1) [ leafFor v | v <- [lo .. hi + 1] ] :: UArray Int Int  -- leaf nodes
      in Just (p0, lo, tab, pr, off, mul)
  where
    n = snd (U.bounds (ftProp ft)) + 1
    idx = [0 .. n - 1]
    dnodes = [ i | i <- idx, ftProp ft `unsafeAt` i >= 0 ]
    dprops = [ ftProp ft `unsafeAt` i | i <- dnodes ]
    p0 = ftProp ft `unsafeAt` 0
    leaves = [ (ftPred ft `unsafeAt` i, ftOff ft `unsafeAt` i, ftMul ft `unsafeAt` i)
             | i <- idx, ftProp ft `unsafeAt` i < 0 ]
    vals = [ ftVal ft `unsafeAt` i | i <- dnodes ]
    lo = minimum vals
    hi = maximum vals
    leafFor v = go 0
      where go i | ftProp ft `unsafeAt` i < 0 = i
                 | otherwise = let k = ftKid ft `unsafeAt` i
                               in go (if v > ftVal ft `unsafeAt` i then k else k + 1)

-- A gradient-predicted channel with no offset or scale, the cluster chosen
-- from (W, N, NW): a single gradient leaf (libjxl's fast lossless) or a
-- table on the gradient property (effort 1–2). Only three neighbours are
-- read and no other property is computed.
{-# INLINE gradientLoop #-}
gradientLoop :: SymReader s -> (Int -> Int -> Int -> Int) -> Chan s -> ST s ()
gradientLoop sr clOf ch = do
  let w = chW ch
      h = chH ch
      buf = chBuf ch
      row0 !x !wv | x >= w = pure ()
                  | otherwise = do
                      t <- readWith sr (clOf wv wv wv)
                      let !v = unpackSigned t + wv
                      wrC buf x v
                      row0 (x + 1) v
  row0 0 0
  let rows !y | y >= h = pure ()
              | otherwise = do
                  let rb = y * w
                      pb = rb - w
                  n0 <- rdC buf pb
                  t0 <- readWith sr (clOf n0 n0 n0)
                  let !v0 = unpackSigned t0 + n0
                  wrC buf rb v0
                  let cols !x !wv !nw
                        | x >= w = rows (y + 1)
                        | otherwise = do
                            n <- rdC buf (pb + x)
                            t <- readWith sr (clOf wv n nw)
                            let !v = unpackSigned t + gradient wv n nw
                            wrC buf (rb + x) v
                            cols (x + 1) v n
                  cols 1 v0 n0
  rows 1

-- A single-property table with a fixed predictor (no weighted predictor):
-- the general loop minus the tree walk.
{-# INLINE tableLoop #-}
tableLoop :: SymReader s -> Int -> (Int -> Int) -> Int -> Int -> Int -> Chan s -> ST s ()
tableLoop sr prop clOf pr off mul ch = do
  let w = chW ch
      h = chH ch
      buf = chBuf ch
      cols !y !x
        | x >= w = if y + 1 < h then cols (y + 1) 0 else pure ()
        | otherwise = do
            Nb wv nv nwv nev nnv neev wwv nwwv <- neighbours buf w h x y
            let !pv0 = propPure prop x y wv nv nwv nev nnv wwv nwwv 0
            t <- readWith sr (clOf pv0)
            let !v = unpackSigned t * mul + off + predictWith pr wv nv nwv nev nnv neev wwv
            wrC buf (y * w + x) v
            cols y (x + 1)
  when (h > 0) (cols 0 0)

-- Channels that need the weighted predictor: its prediction, or its error
-- as property 15. The predictor's state is carried incrementally, as
-- libjxl and jxl-oxide do: the error sums over each sample's neighbourhood
-- (N, W, NW, WW, NE, the right edge counting W twice) are rolled forward in
-- loop variables, and the previous row's errors live in one row array that
-- is read one sample ahead (NE) and overwritten one behind, so a sample
-- costs five reads and five writes rather than thirty.
--
-- @findLeaf@ chooses a lookup key (a leaf index for a walk, or a property
-- value for a table) and continues with it, keeping walks as join points.
-- The four leaf-field functions inline here: table paths supply constants,
-- so the known WP/error-table case drops unused neighbours and dispatch.
{-# INLINE wpLoop #-}
wpLoop :: SymReader s -> WPP -> Chan s
       -> (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> (Int -> Int)
       -> (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> (Int -> ST s ()) -> ST s ())
       -> ST s ()
wpLoop sr wpp ch cluster predictor offset multiplier findLeaf = do
  let w = chW ch
      h = chH ch
      buf = chBuf ch
      !p1 = wpp `unsafeAt` 0
      !p2 = wpp `unsafeAt` 1
      !p3a = wpp `unsafeAt` 2
      !p3b = wpp `unsafeAt` 3
      !p3c = wpp `unsafeAt` 4
      !p3d = wpp `unsafeAt` 5
      !p3e = wpp `unsafeAt` 6
      !ww0 = wpp `unsafeAt` 7
      !ww1 = wpp `unsafeAt` 8
      !ww2 = wpp `unsafeAt` 9
      !ww3 = wpp `unsafeAt` 10
      -- bound once: a top-level table is a CAF, entered on every use
      !d24 = div24
      weight maxw es =
        let sh = max (flg (es + 1) - 5) 0
        in 4 + ((maxw * (d24 `unsafeAt` (es `unsafeShiftR` sh))) `unsafeShiftR` sh)
  -- previous row's errors, five per sample, padded so the look-ahead read
  -- at x + 2 never leaves the array
  row <- newArray (0, 5 * w + 9) 0 :: ST s (STUArray s Int Int)
  let rowStart !y
        | y >= h = pure ()
        | otherwise = do
            n0 <- unsafeRead row 0
            n1 <- unsafeRead row 1
            n2 <- unsafeRead row 2
            n3 <- unsafeRead row 3
            t0 <- unsafeRead row 4
            let j = if w > 1 then 5 else 0
            e0 <- unsafeRead row j
            e1 <- unsafeRead row (j + 1)
            e2 <- unsafeRead row (j + 2)
            e3 <- unsafeRead row (j + 3)
            te <- unsafeRead row (j + 4)
            pixel y 0 0 t0 t0 te n0 n1 n2 n3 n0 n1 n2 n3 e0 e1 e2 e3
      -- a = NW + WW, b = N + W, c = NE: the sums' three rolling parts
      pixel !y !x !tw !tn !tnw !tne !a0 !a1 !a2 !a3 !b0 !b1 !b2 !b3 !c0 !c1 !c2 !c3
        | x >= w = rowStart (y + 1)
        | otherwise = do
            Nb pw pn pnw pne pnn pnee pww pnww <- neighbours buf w h x y
            let !pr0 = (pw + pne - pn) * 8
                !pr1 = pn * 8 - (((tw + tn + tne) * p1) `shiftR` 5)
                !pr2 = pw * 8 - (((tw + tn + tnw) * p2) `shiftR` 5)
                !pr3 = pn * 8 - ((tnw * p3a + tn * p3b + tne * p3c
                                  + (pnn - pn) * 8 * p3d + (pnw - pw) * 8 * p3e) `shiftR` 5)
                !w0 = weight ww0 (a0 + b0 + c0)
                !w1 = weight ww1 (a1 + b1 + c1)
                !w2 = weight ww2 (a2 + b2 + c2)
                !w3 = weight ww3 (a3 + b3 + c3)
                !logw = flg (w0 + w1 + w2 + w3) - 4
                !v0 = w0 `unsafeShiftR` logw
                !v1 = w1 `unsafeShiftR` logw
                !v2 = w2 `unsafeShiftR` logw
                !v3 = w3 `unsafeShiftR` logw
                !wsum = v0 + v1 + v2 + v3
                !ssum = pr0 * v0 + pr1 * v1 + pr2 * v2 + pr3 * v3
                !p4a = ((ssum + (wsum `unsafeShiftR` 1) - 1) * (d24 `unsafeAt` (wsum - 1))) `shiftR` 24
                !p4 = if ((tn `xor` tw) .|. (tn `xor` tnw)) <= 0
                        then maxFast (minFast pw (minFast pn pne) * 8) (minFast (maxFast pw (maxFast pn pne) * 8) p4a)
                        else p4a
                !mx1 = selectLT (absFast tw) (absFast tn) tn tw
                !mx2 = selectLT (absFast mx1) (absFast tnw) tnw mx1
                !maxErr = selectLT (absFast mx2) (absFast tne) tne mx2
            findLeaf x y pw pn pnw pne pnn pww pnww maxErr $ \leaf -> do
              t <- readWith sr (cluster leaf)
              let !pr = predictor leaf
                  !pv = if pr == 6 then (p4 + 3) `shiftR` 3
                        else predictWith pr pw pn pnw pne pnn pnee pww
                  !v = unpackSigned t * (multiplier leaf) + (offset leaf) + pv
                  !v8 = v * 8
                  !e0 = (absFast (pr0 - v8) + 3) `shiftR` 3
                  !e1 = (absFast (pr1 - v8) + 3) `shiftR` 3
                  !e2 = (absFast (pr2 - v8) + 3) `shiftR` 3
                  !e3 = (absFast (pr3 - v8) + 3) `shiftR` 3
                  !te = p4 - v8
                  !o = 5 * x
                  !has2 = x + 2 < w
              wrC buf (y * w + x) v
              m0 <- unsafeRead row (o + 10)
              m1 <- unsafeRead row (o + 11)
              m2 <- unsafeRead row (o + 12)
              m3 <- unsafeRead row (o + 13)
              mt <- unsafeRead row (o + 14)
              unsafeWrite row o e0
              unsafeWrite row (o + 1) e1
              unsafeWrite row (o + 2) e2
              unsafeWrite row (o + 3) e3
              unsafeWrite row (o + 4) te
              let !b0' = c0 + e0
                  !b1' = c1 + e1
                  !b2' = c2 + e2
                  !b3' = c3 + e3
              pixel y (x + 1) te tne tn (if has2 then mt else tne)
                    b0 b1 b2 b3 b0' b1' b2' b3'
                    (if has2 then m0 else b0') (if has2 then m1 else b1')
                    (if has2 then m2 else b2') (if has2 then m3 else b3')
  rowStart 0

-- The eight neighbours of (x, y), each one read from an index that falls
-- back exactly as the spec's neighbour does (missing ones end at the zero
-- cell). One interior fast path avoids all edge tests. The fallback uses
-- pure index arithmetic, avoiding a monadic branch per neighbour.
data Nb = Nb !Int !Int !Int !Int !Int !Int !Int !Int

{-# INLINE neighbours #-}
neighbours :: STUArray s Int Int32 -> Int -> Int -> Int -> Int -> ST s Nb
neighbours buf w h x y
  | x >= 2 && x + 2 < w && y >= 2 = do
      let i = y * w + x
      a <- rdC buf (i - 1)
      b <- rdC buf (i - w)
      c <- rdC buf (i - w - 1)
      d <- rdC buf (i - w + 1)
      e <- rdC buf (i - 2 * w)
      f <- rdC buf (i - w + 2)
      g <- rdC buf (i - 2)
      k <- rdC buf (i - w - 2)
      pure (Nb a b c d e f g k)
  | otherwise = do
    let rb = y * w
        pb = rb - w
        hasW = x > 0
        hasN = y > 0
        z = w * h
        !iW = if hasW then rb + x - 1 else if hasN then pb + x else z
        !iN = if hasN then pb + x else iW
        !iNW = if hasW && hasN then pb + x - 1 else iW
        !iNE = if x + 1 < w && hasN then pb + x + 1 else iN
        !iNN = if y > 1 then pb - w + x else iN
        !iNEE = if x + 2 < w && hasN then pb + x + 2 else iNE
        !iWW = if x > 1 then rb + x - 2 else iW
        !iNWW = if x > 1 && hasN then pb + x - 2 else iWW
    a <- rdC buf iW
    b <- rdC buf iN
    c <- rdC buf iNW
    d <- rdC buf iNE
    e <- rdC buf iNN
    f <- rdC buf iNEE
    g <- rdC buf iWW
    k <- rdC buf iNWW
    pure (Nb a b c d e f g k)

-- Properties 2..15 (the static ones and previous-channel ones are handled
-- elsewhere).
{-# INLINE propPure #-}
propPure :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
propPure p x y wv nv nwv nev nnv wwv nwwv maxErr = case p of
  2 -> y
  3 -> x
  4 -> absFast nv
  5 -> absFast wv
  6 -> nv
  7 -> wv
  -- W minus the previous sample's property 9 (libjxl keeps that in the
  -- property vector; it is 0 at a row start)
  8 -> wv - (if x > 1 then (if y > 0 then wwv + nwv - nwwv else wwv)
             else if x == 1 && y > 0 then nwv else 0)
  9 -> wv + nv - nwv
  10 -> wv - nwv
  11 -> nwv - nv
  12 -> nv - nev
  13 -> nv - nnv
  14 -> wv - wwv
  15 -> maxErr
  _ -> 0

{-# INLINE gradient #-}
gradient :: Int -> Int -> Int -> Int
gradient w n nw =
  let lo = minFast w n
      hi = maxFast w n
      g = w + n - nw
  in maxFast lo (minFast hi g)

-- The general path: walk the specialised tree for every sample.
--
-- Written so that GHC compiles the whole pixel step to straight-line code
-- plus jumps: every neighbour is read unconditionally from a clamped index
-- and selected with a pure 'if' (a monadic branch per neighbour would copy
-- the rest of the step into each branch, exponentially), and the tree walk
-- ends by running the rest of the step itself, so it is a join point rather
-- than a closure allocated per sample.
{-# INLINE generalLoop #-}
generalLoop :: SymReader s -> FTree -> [Chan s] -> Chan s -> ST s ()
generalLoop sr ft prevs ch = do
  let w = chW ch
      h = chH ch
      buf = chBuf ch
      props = ftProp ft
      vals = ftVal ft
      kids = ftKid ft
      cols !y !x
        | x >= w = if y + 1 < h then cols (y + 1) 0 else pure ()
        | otherwise = do
            Nb wv nv nwv nev nnv neev wwv nwwv <- neighbours buf w h x y
            let walk !i = do
                  let p = props `unsafeAt` i
                  if p < 0 then leaf i else do
                    v <- if p >= 16 then prevProp prevs (p - 16) x y
                         else pure (propPure p x y wv nv nwv nev nnv wwv nwwv 0)
                    let k = kids `unsafeAt` i
                    walk (if v > vals `unsafeAt` i then k else k + 1)
                leaf !i = do
                  t <- readWith sr (ftCl ft `unsafeAt` i)
                  let !v = unpackSigned t * (ftMul ft `unsafeAt` i) + (ftOff ft `unsafeAt` i)
                           + predictWith (ftPred ft `unsafeAt` i) wv nv nwv nev nnv neev wwv
                  wrC buf (y * w + x) v
                  cols y (x + 1)
            walk 0
  when (h > 0) (cols 0 0)

-- Previous-channel properties (16 + 4k + j): rare, so out of line.
{-# NOINLINE prevProp #-}
prevProp :: [Chan s] -> Int -> Int -> Int -> ST s Int
prevProp prevs !q !x !y = case drop (q `shiftR` 2) prevs of
  [] -> pure 0
  (pc : _) -> do
    let w = chW pc
        b = chBuf pc
        j = q .&. 3
    c <- rdC b (y * w + x)
    if j < 2 then pure (if j == 0 then abs c else c) else do
      g <- if x == 0 && y == 0 then pure 0
           else if x == 0 then rdC b ((y - 1) * w)
           else if y == 0 then rdC b (x - 1)
           else do nw <- rdC b ((y - 1) * w + x - 1)
                   n <- rdC b ((y - 1) * w + x)
                   wv <- rdC b (y * w + x - 1)
                   pure (gradient wv n nw)
      pure (if j == 2 then abs (c - g) else c - g)

-- The fixed predictors (all but 6, the weighted one).
{-# INLINE predictWith #-}
predictWith :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
predictWith pr w n nw ne nn nee ww = case pr of
  0 -> 0
  1 -> w
  2 -> n
  3 -> (w + n) `quot` 2
  4 -> selectLT (absFast (n - nw)) (absFast (w - nw)) w n
  5 -> gradient w n nw
  7 -> ne
  8 -> nw
  9 -> ww
  10 -> (w + nw) `quot` 2
  11 -> (n + nw) `quot` 2
  12 -> (n + ne) `quot` 2
  13 -> (6 * n - 2 * nn + 7 * w + ww + nee + 3 * ne + 8) `quot` 16
  _ -> 0

------------------------------------------------------------------------------
-- The weighted ("self-correcting") predictor (§H.5.2)

-- State: two rows of five errors per sample (four sub-predictor errors and
-- the signed true error), one all-zero block standing in for every missing
-- neighbour, plus a scratch of the current sub-predictions.
data WPState s = WPState !(STUArray s Int Int) !(STUArray s Int Int)

newWPState :: Int -> ST s (WPState s)
newWPState w = WPState <$> newArray (0, 2 * w * 5 + 4) 0 <*> newArray (0, 7) 0

div24 :: UArray Int Int
div24 = listArray (0, 63) [ (1 `shiftL` 24) `div` (i + 1) | i <- [0 .. 63] ]

{-# INLINE flg #-}
flg :: Int -> Int
flg v = 63 - countLeadingZeros v

{-# INLINE sum6 #-}
sum6 :: STUArray s Int Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s Int
sum6 e a b c d f g k = do
  va <- unsafeRead e (a + k)
  vb <- unsafeRead e (b + k)
  vc <- unsafeRead e (c + k)
  vd <- unsafeRead e (d + k)
  vf <- unsafeRead e (f + k)
  vg <- unsafeRead e (g + k)
  pure (va + vb + vc + vd + vf + vg)

{-# INLINE wpWeight #-}
wpWeight :: Int -> Int -> Int
wpWeight maxw es =
  let sh = max (flg (es + 1) - 5) 0
  in 4 + ((maxw * (div24 `unsafeAt` (es `unsafeShiftR` sh))) `unsafeShiftR` sh)

-- Compute the sub-predictions and the weighted prediction for (x, y),
-- leaving them in the scratch; returns the max-error property. The
-- neighbours' error blocks are addressed once, missing ones pointing at the
-- zero block, so the twenty-odd reads below are straight-line code.
{-# INLINE wpPredict #-}
wpPredict :: WPP -> WPState s -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> ST s Int
wpPredict wpp (WPState errs sc) w x y pw pn pnw pne pnn = do
  let z = 2 * w * 5
      cur = (y .&. 1) * w * 5
      prv = ((y + 1) .&. 1) * w * 5
      iN = if y > 0 then prv + x * 5 else z
      iW = if x > 0 then cur + (x - 1) * 5 else z
      iNW = if x > 0 && y > 0 then prv + (x - 1) * 5 else iN
      iWW = if x > 1 then cur + (x - 2) * 5 else z
      iNE = if x + 1 < w && y > 0 then prv + (x + 1) * 5 else iN
      iW2 = if x + 1 < w then z else iW
  tw <- unsafeRead errs (iW + 4)
  tn <- unsafeRead errs (iN + 4)
  tnw <- unsafeRead errs (iNW + 4)
  tne <- unsafeRead errs (iNE + 4)
  s0 <- sum6 errs iN iW iNW iWW iNE iW2 0
  s1 <- sum6 errs iN iW iNW iWW iNE iW2 1
  s2 <- sum6 errs iN iW iNW iWW iNE iW2 2
  s3 <- sum6 errs iN iW iNW iWW iNE iW2 3
  let pr0 = (pw + pne - pn) * 8
      pr1 = pn * 8 - (((tw + tn + tne) * (wpp `unsafeAt` 0)) `shiftR` 5)
      pr2 = pw * 8 - (((tw + tn + tnw) * (wpp `unsafeAt` 1)) `shiftR` 5)
      pr3 = pn * 8 - ((tnw * (wpp `unsafeAt` 2) + tn * (wpp `unsafeAt` 3)
                        + tne * (wpp `unsafeAt` 4) + (pnn - pn) * 8 * (wpp `unsafeAt` 5)
                        + (pnw - pw) * 8 * (wpp `unsafeAt` 6)) `shiftR` 5)
      w0 = wpWeight (wpp `unsafeAt` 7) s0
      w1 = wpWeight (wpp `unsafeAt` 8) s1
      w2 = wpWeight (wpp `unsafeAt` 9) s2
      w3 = wpWeight (wpp `unsafeAt` 10) s3
      logw = flg (w0 + w1 + w2 + w3) - 4
      v0 = w0 `unsafeShiftR` logw
      v1 = w1 `unsafeShiftR` logw
      v2 = w2 `unsafeShiftR` logw
      v3 = w3 `unsafeShiftR` logw
      wsum = v0 + v1 + v2 + v3
      ssum = pr0 * v0 + pr1 * v1 + pr2 * v2 + pr3 * v3
      p4a = ((ssum + (wsum `unsafeShiftR` 1) - 1) * (div24 `unsafeAt` (wsum - 1))) `shiftR` 24
      p4 = if ((tn `xor` tw) .|. (tn `xor` tnw)) <= 0
             then let lo = min pw (min pn pne) * 8
                      hi = max pw (max pn pne) * 8
                  in max lo (min hi p4a)
             else p4a
      mx1 = if abs tw < abs tn then tn else tw
      mx2 = if abs mx1 < abs tnw then tnw else mx1
      mx3 = if abs mx2 < abs tne then tne else mx2
  unsafeWrite sc 0 pr0
  unsafeWrite sc 1 pr1
  unsafeWrite sc 2 pr2
  unsafeWrite sc 3 pr3
  unsafeWrite sc 4 p4
  pure mx3

{-# INLINE wpPrediction #-}
wpPrediction :: WPState s -> ST s Int
wpPrediction (WPState _ sc) = do
  p4 <- unsafeRead sc 4
  pure ((p4 + 3) `shiftR` 3)

{-# INLINE wpUpdate #-}
wpUpdate :: WPState s -> Int -> Int -> Int -> Int -> ST s ()
wpUpdate (WPState errs sc) w x y v = do
  let base = ((y .&. 1) * w + x) * 5
      v8 = v * 8
  forLoop 0 3 $ \k -> do
    p <- unsafeRead sc k
    unsafeWrite errs (base + k) ((abs (p - v8) + 3) `shiftR` 3)
  p4 <- unsafeRead sc 4
  unsafeWrite errs (base + 4) (p4 - v8)

------------------------------------------------------------------------------
-- Inverse transforms (§H.6)

-- | Undo a stream's transforms, last first, turning the decoded channel list
-- back into the one the header started from.
inverseTransforms :: Int -> WPP -> [Transform] -> [Chan s] -> J s [Chan s]
inverseTransforms bitDepth wpp trs chans0 = do
  cs <- inverseTransformsCompact bitDepth wpp trs chans0
  liftJ (mapM materializeChan cs)

-- Constants live only after decoding. Before a transform that mutates or
-- needs a dense plane they are materialised, so no cached fact can go stale.
data CompactChan s = ConstantChan !ChInfo !Int32 | DenseChan !(Chan s)

materializeChan :: CompactChan s -> ST s (Chan s)
materializeChan (DenseChan c) = pure c
materializeChan (ConstantChan ci v) = filledChan ci v

compactChan :: CompactChan s -> ST s (CompactChan s)
compactChan c@(ConstantChan _ _) = pure c
compactChan c@(DenseChan p) = do
  v <- constantChan p
  pure (maybe c (ConstantChan (chanInfo p)) v)

inverseTransformsCompact :: Int -> WPP -> [Transform] -> [Chan s] -> J s [CompactChan s]
inverseTransformsCompact bitDepth wpp trs chans0 = foldM undo (map DenseChan chans0) (reverse trs)
  where
    undo cs (TrRct b ty) = do
      dense <- liftJ (mapM materializeChan cs)
      map DenseChan <$> liftJ (inverseRct b ty dense)
    undo cs (TrPalette b nc ncol nd dp) = do
      dense <- liftJ (mapM materializeChan cs)
      map DenseChan <$> inversePalette bitDepth wpp b nc ncol nd dp dense
    undo cs (TrSqueeze sps) = foldM inverseSqueezeCompact cs (reverse sps)

inverseRct :: Int -> Int -> [Chan s] -> ST s [Chan s]
inverseRct b ty cs = do
  let [c0, c1, c2] = take 3 (drop b cs)
      a0 = chBuf c0
      a1 = chBuf c1
      a2 = chBuf c2
      n = chW c0 * chH c0
  case ty `mod` 7 of
    0 -> pure ()
    1 -> forLoop 0 (n - 1) $ \i -> do
           p0 <- rdC a0 i; p2 <- rdC a2 i; wrC a2 i (p2 + p0)
    2 -> forLoop 0 (n - 1) $ \i -> do
           p0 <- rdC a0 i; p1 <- rdC a1 i; wrC a2 i (p1 + p0)
    3 -> forLoop 0 (n - 1) $ \i -> do
           p0 <- rdC a0 i; p1 <- rdC a1 i; p2 <- rdC a2 i
           wrC a1 i (p1 + p0); wrC a2 i (p2 + p0)
    4 -> forLoop 0 (n - 1) $ \i -> do
           p0 <- rdC a0 i; p1 <- rdC a1 i; p2 <- rdC a2 i
           wrC a1 i (p1 + ((p0 + p2) `shiftR` 1))
    5 -> forLoop 0 (n - 1) $ \i -> do
           p0 <- rdC a0 i; p1 <- rdC a1 i; p2 <- rdC a2 i
           wrC a1 i (p1 + p0 + (p2 `shiftR` 1)); wrC a2 i (p2 + p0)
    _ -> forLoop 0 (n - 1) $ \i -> do
           p0 <- rdC a0 i; p1 <- rdC a1 i; p2 <- rdC a2 i
           let tmp = p0 - (p2 `shiftR` 1)
               q1 = p2 + tmp
               q2 = tmp - (p1 `shiftR` 1)
           wrC a0 i (q2 + p1); wrC a1 i q1; wrC a2 i q2
  -- channel permutation
  let perm = case ty `div` 7 of
        0 -> [0, 1, 2]; 1 -> [1, 2, 0]; 2 -> [2, 0, 1]
        3 -> [0, 2, 1]; 4 -> [1, 0, 2]; _ -> [2, 1, 0]
      src = [c0, c1, c2]
      placed = [ snd (head [ (k, s) | (k, s) <- zip perm src, k == j ]) | j <- [0, 1, 2] ]
  pure (take b cs ++ placed ++ drop (b + 3) cs)

deltaPalette :: UArray Int Int
deltaPalette = listArray (0, 215) $ concat
  [ [0,0,0],[4,4,4],[11,0,0],[0,0,-13],[0,-12,0],[-10,-10,-10]
  , [-18,-18,-18],[-27,-27,-27],[-18,-18,0],[0,0,-32],[-32,0,0],[-37,-37,-37]
  , [0,-32,-32],[24,24,45],[50,50,50],[-45,-24,-24],[-24,-45,-45],[0,-24,-24]
  , [-34,-34,0],[-24,0,-24],[-45,-45,-24],[64,64,64],[-32,0,-32],[0,-32,0]
  , [-32,0,32],[-24,-45,-24],[45,24,45],[24,-24,-45],[-45,-24,24],[80,80,80]
  , [64,0,0],[0,0,-64],[0,-64,-64],[-24,-24,45],[96,96,96],[64,64,0]
  , [45,-24,-24],[34,-34,0],[112,112,112],[24,-45,-45],[45,45,-24],[0,-32,32]
  , [24,-24,45],[0,96,96],[45,-24,24],[24,-45,-24],[-24,-45,24],[0,-64,0]
  , [96,0,0],[128,128,128],[64,0,64],[144,144,144],[96,96,0],[-36,-36,36]
  , [45,-24,-45],[45,-45,-24],[0,0,-96],[0,128,128],[0,96,0],[45,24,-45]
  , [-128,0,0],[24,-45,24],[-45,24,-45],[64,0,-64],[64,-64,-64],[96,0,96]
  , [45,-45,24],[24,45,-45],[64,64,-64],[128,128,0],[0,0,-128],[-24,45,-45] ]

inversePalette :: Int -> WPP -> Int -> Int -> Int -> Int -> Int -> [Chan s] -> J s [Chan s]
inversePalette bd wpp b nc ncol nd dp cs = do
  jguard (length cs >= b + 2) "JPEG XL: bad palette"
  let pal = head cs
      rest = tail cs
      idxC = rest !! b
      w = chW idxC
      h = chH idxC
      n = w * h
      pb = chBuf pal
      pw = chW pal
  outs <- liftJ $ forM [0 .. nc - 1] $ \c ->
    if c == 0 then pure idxC else newChan (chanInfo idxC)
  -- the delta mask must be taken before channel 0 overwrites the indices
  mask <- liftJ $ if nd > 0
    then do m <- newChan (chanInfo idxC)
            forLoop 0 (n - 1) $ \i -> rdC (chBuf idxC) i >>= wrC (chBuf m) i
            pure (Just m)
    else pure Nothing
  -- channel 0 last: it is the index channel, overwritten in place
  liftJ $ forM_ (reverse (zip [0 ..] outs)) $ \(c, oc) -> do
    let ob = chBuf oc
        ib = chBuf idxC
    forLoop 0 (n - 1) $ \i -> do
      ix <- rdC ib i
      v <- if ix >= 0 && ix < ncol then
             (if c < chH pal then rdC pb (c * pw + ix) else pure 0)
           else if ix >= ncol then
             let k = ix - ncol
             in if k < 64
                  then pure (((k `shiftR` (2 * c)) `mod` 4) * ((1 `shiftL` bd) - 1) `div` 4
                             + (1 `shiftL` max 0 (bd - 3)))
                  else pure (((k - 64) `div` (5 ^ c)) `mod` 5 * ((1 `shiftL` bd) - 1) `div` 4)
           else if c >= 3 then pure 0
           else do
             let k = (negate (ix + 1)) `mod` 143
                 t0 = deltaPalette `unsafeAt` (((k + 1) `shiftR` 1) * 3 + c)
                 t1 = if k .&. 1 == 0 then negate t0 else t0
             pure (if bd > 8 then t1 `shiftL` (min bd 24 - 8) else t1)
      wrC ob i v
  -- delta entries are predicted from already-reconstructed neighbours
  case mask of
    Nothing -> pure ()
    Just m -> liftJ $ forM_ outs $ \oc -> do
      let ob = chBuf oc
          mb = chBuf m
          useWP = dp == 6
      wps <- newWPState (if useWP then w else 0)
      forLoop 0 (h - 1) $ \y -> forLoop 0 (w - 1) $ \x -> do
        let rb = y * w
            pb' = rb - w
            i = rb + x
        wv <- if x > 0 then rdC ob (i - 1) else if y > 0 then rdC ob pb' else pure 0
        nv <- if y > 0 then rdC ob (pb' + x) else pure wv
        nwv <- if x > 0 && y > 0 then rdC ob (pb' + x - 1) else pure wv
        nev <- if x + 1 < w && y > 0 then rdC ob (pb' + x + 1) else pure nv
        nnv <- if y > 1 then rdC ob (pb' - w + x) else pure nv
        neev <- if x + 2 < w && y > 0 then rdC ob (pb' + x + 2) else pure nev
        wwv <- if x > 1 then rdC ob (i - 2) else pure wv
        when useWP $ () <$ wpPredict wpp wps w x y wv nv nwv nev nnv
        s <- rdC ob i
        ix <- rdC mb i
        s' <- if ix < nd
                then do pv <- if useWP then wpPrediction wps
                              else pure (predictWith dp wv nv nwv nev nnv neev wwv)
                        pure (s + pv)
                else pure s
        when (s' /= s) $ wrC ob i s'
        when useWP $ wpUpdate wps w x y s'
  pure (take b rest ++ outs ++ drop (b + 1) rest)

inverseSqueezeCompact :: [CompactChan s] -> SqParam -> J s [CompactChan s]
inverseSqueezeCompact cs (SqParam hor inPlace b n) = do
  let e = b + n
  jguard (length cs >= e + n) "JPEG XL: bad squeeze"
  let (resid, cs1) = if inPlace
        then (take n (drop e cs), take e cs ++ drop (e + n) cs)
        else (drop (length cs - n) cs, take (length cs - n) cs)
  merged <- liftJ $ forM (zip (take n (drop b cs1)) resid) $ \(a, r) -> do
    let fallback = do
          -- Use the original dense arrays when available, even if a scan
          -- classified them as constant. Do not allocate them again.
          ad <- materializeChan a
          rd <- materializeChan r
          DenseChan <$> (if hor then unsqueezeH ad rd else unsqueezeV ad rd)
    ac <- compactChan a
    case ac of
      DenseChan _ -> fallback
      ConstantChan ai v -> do
        zero <- case r of
          ConstantChan _ rv -> pure (rv == 0)
          DenseChan rc -> allChan rc 0
        if zero
          -- tendency v v v = 0: (4*v - 3*v - v + 6) quot 12 = 0,
          -- and both clamps leave zero unchanged. With residual zero, first
          -- and second are v. Induction covers each row/column; an odd tail
          -- copies v. Zero residuals ALONE do not establish this invariant.
          then let ri = case r of { ConstantChan ci _ -> ci; DenseChan rc -> chanInfo rc }
               in pure (ConstantChan (squeezeResult hor ai ri) v)
          else fallback
  pure (take b cs1 ++ merged ++ drop e cs1)

-- Scan only real samples, never the zero sentinel. Stop at the first mismatch.
constantChan :: Chan s -> ST s (Maybe Int32)
constantChan c
  | chW c * chH c <= 0 = pure (Just 0)
  | otherwise = do
      v <- unsafeRead (chBuf c) 0
      same <- allChan c v
      pure (if same then Just v else Nothing)

allChan :: Chan s -> Int32 -> ST s Bool
allChan c v = go 0
  where
    n = chW c * chH c
    go !i | i >= n = pure True
          | otherwise = do
              x <- unsafeRead (chBuf c) i
              if x == v then go (i + 1) else pure False

filledChan :: ChInfo -> Int32 -> ST s (Chan s)
filledChan (ChInfo w h hs vs) v = do
  a <- newArray (0, max 0 (w * h)) v
  unsafeWrite a (max 0 (w * h)) 0
  pure (Chan w h hs vs a)

squeezeResult :: Bool -> ChInfo -> ChInfo -> ChInfo
squeezeResult hor (ChInfo w h hs vs) (ChInfo rw rh _ _)
  | hor = ChInfo (w + rw) h (hs - fromEnum (hs > 0)) vs
  | otherwise = ChInfo w (h + rh) hs (vs - fromEnum (vs > 0))

{-# INLINE tendency #-}
tendency :: Int -> Int -> Int -> Int
tendency a b c
  | a >= b && b >= c =
      let x0 = (4 * a - 3 * c - b + 6) `quot` 12
          x1 = if x0 - (x0 .&. 1) > 2 * (a - b) then 2 * (a - b) + 1 else x0
      in if x1 + (x1 .&. 1) > 2 * (b - c) then 2 * (b - c) else x1
  | a <= b && b <= c =
      let x0 = (4 * a - 3 * c - b - 6) `quot` 12
          x1 = if x0 + (x0 .&. 1) < 2 * (a - b) then 2 * (a - b) - 1 else x0
      in if x1 - (x1 .&. 1) < 2 * (b - c) then 2 * (b - c) else x1
  | otherwise = 0

unsqueezeH :: Chan s -> Chan s -> ST s (Chan s)
unsqueezeH a r = do
  let aw = chW a
      rw = chW r
      h = chH a
      w = aw + rw
  out <- newChan (ChInfo w h (chHS a - (if chHS a > 0 then 1 else 0)) (chVS a))
  let ab = chBuf a
      rb = chBuf r
      ob = chBuf out
  forLoop 0 (h - 1) $ \y -> do
    let ar = y * aw
        rr = y * rw
        orow = y * w
    a0 <- rdC ab ar
    let go !x !avg !left
          | x >= rw = when (odd w) $ rdC ab (ar + aw - 1) >>= wrC ob (orow + w - 1)
          | otherwise = do
              res <- rdC rb (rr + x)
              nxt <- if x + 1 < aw then rdC ab (ar + x + 1) else pure avg
              let diff = res + tendency left avg nxt
                  first = avg + diff `quot` 2
                  second = first - diff
              wrC ob (orow + 2 * x) first
              wrC ob (orow + 2 * x + 1) second
              go (x + 1) nxt second
    go 0 a0 a0
  pure out

unsqueezeV :: Chan s -> Chan s -> ST s (Chan s)
unsqueezeV a r = do
  let ah = chH a
      rh = chH r
      w = chW a
      h = ah + rh
  out <- newChan (ChInfo w h (chHS a) (chVS a - (if chVS a > 0 then 1 else 0)))
  let ab = chBuf a
      rb = chBuf r
      ob = chBuf out
  -- Independent columns share the same recurrence. Reconstruct a whole
  -- pair of rows at a time so large power-of-two strides do not send every
  -- access to the same cache set. Retain Int recurrence values separately:
  -- the Int32 output store can truncate extreme intermediate values.
  tops <- newArray (0, w - 1) 0 :: ST s (STUArray s Int Int)
  forLoop 0 (rh - 1) $ \y -> do
    let ar = y * w
        nr = min (ah - 1) (y + 1) * w
        dst = 2 * y * w
    forLoop 0 (w - 1) $ \x -> do
      avg <- rdC ab (ar + x)
      res <- rdC rb (ar + x)
      nxt <- rdC ab (nr + x)
      top <- if y == 0 then pure avg else unsafeRead tops x
      let diff = res + tendency top avg nxt
          first = avg + diff `quot` 2
          second = first - diff
      wrC ob (dst + x) first
      wrC ob (dst + w + x) second
      unsafeWrite tops x second
  when (odd h) $ forLoop 0 (w - 1) $ \x ->
    rdC ab ((ah - 1) * w + x) >>= wrC ob ((h - 1) * w + x)
  pure out

{-# INLINE forLoop #-}
forLoop :: Int -> Int -> (Int -> ST s ()) -> ST s ()
forLoop lo hi f = go lo
  where go !i | i > hi = pure ()
              | otherwise = f i >> go (i + 1)

{-# INLINE absFast #-}
absFast :: Int -> Int
absFast v = let m = v `unsafeShiftR` (finiteBitSize v - 1) in (v `xor` m) - m

{-# INLINE selectLT #-}
selectLT :: Int -> Int -> Int -> Int -> Int
selectLT (I# a) (I# b) x y = let m = negate (I# (a <# b)) in y `xor` ((x `xor` y) .&. m)
{-# INLINE minFast #-}
minFast :: Int -> Int -> Int
minFast a b = selectLT a b a b
{-# INLINE maxFast #-}
maxFast :: Int -> Int -> Int
maxFast a b = selectLT a b b a
