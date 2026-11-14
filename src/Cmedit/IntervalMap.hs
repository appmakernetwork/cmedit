{-# LANGUAGE BangPatterns #-}
-- | A map from 'Int' keys to values, stored as disjoint closed intervals:
-- what a PDF CID font's @\/W@ and @\/W2@ tables are (@cFirst cLast w@ gives
-- one width to every CID in a range), shared by the reflow extractor
-- ("Cmedit.Pdf") and the page renderer ("Cmedit.PdfRenderFont").
--
-- __Why intervals.__ Expanding a range into one map entry per key let three
-- numbers stand for a million entries: twenty CID fonts in a 21 KB file
-- held two gigabytes. Here a range is one entry however wide it is.
--
-- __It is exactly the expanded map.__ 'insertRange' paints: the new range
-- replaces whatever it overlaps, splitting the intervals it cuts, so the
-- last write wins key by key, as repeated 'Data.IntMap.insert's would have
-- it; 'lookupKey' is therefore the old map's lookup, and 'coveredCount' its
-- 'Data.IntMap.size'. The reflow view's output depends on both (widths
-- place its word breaks, and the count is charged to the document's
-- budget), and it is held byte-identical across the change.
--
-- Leaf module: @containers@ only.
module Cmedit.IntervalMap
  ( IntervalMap
  , empty
  , insertRange
  , insertKey
  , fromKeys
  , lookupKey
  , coveredCount
  , intervalCount
  ) where

import Data.List (foldl')
import qualified Data.IntMap.Strict as IM

-- | Disjoint intervals keyed by their first key, each holding its last key
-- and its value, plus the number of keys covered.
data IntervalMap a = IntervalMap !(IM.IntMap (Int, a)) !Int

instance Show a => Show (IntervalMap a) where
  show (IntervalMap m _) = "IntervalMap " ++ show (IM.toList m)

empty :: IntervalMap a
empty = IntervalMap IM.empty 0

-- | Give every key in @[lo, hi]@ the value (nothing when @hi < lo@).
insertRange :: Int -> Int -> a -> IntervalMap a -> IntervalMap a
insertRange lo hi v im@(IntervalMap m n)
  | hi < lo = im
  | otherwise =
      let -- An interval starting before lo that reaches into the range keeps
          -- its part before lo (and its part after hi, when it spans both).
          (m1, before) = case IM.lookupLT lo m of
            Just (s, (e, w)) | e >= lo ->
              let kept = IM.insert s (lo - 1, w) (IM.delete s m)
                  tailPart = if e > hi then [(hi + 1, (e, w))] else []
              in (foldl' (\acc (k, x) -> IM.insert k x acc) kept tailPart, (e - lo + 1) `min` (hi - lo + 1))
            _ -> (m, 0)
          -- Intervals starting inside the range are removed; the last may
          -- run past hi and keeps its remainder.
          insideL = collect lo
          collect k = case IM.lookupGE k m1 of
            Just (st, x) | st <= hi -> (st, x) : (if st == maxBound then [] else collect (st + 1))
            _ -> []
          removed = sum [ min e hi - s + 1 | (s, (e, _)) <- insideL ]
          m2 = foldl' (\acc (s, _) -> IM.delete s acc) m1 insideL
          m3 = case reverse insideL of
            ((_, (e, w)) : _) | e > hi -> IM.insert (hi + 1) (e, w) m2
            _ -> m2
          m4 = IM.insert lo (hi, v) m3
      in IntervalMap m4 (n - before - removed + (hi - lo + 1))

-- | One key.
insertKey :: Int -> a -> IntervalMap a -> IntervalMap a
insertKey k = insertRange k k

-- | From keys and values, later ones winning (an expanded map's @fromList@).
fromKeys :: [(Int, a)] -> IntervalMap a
fromKeys = foldl' (\acc (k, v) -> insertKey k v acc) empty

lookupKey :: Int -> IntervalMap a -> Maybe a
lookupKey k (IntervalMap m _) = case IM.lookupLE k m of
  Just (_, (e, v)) | k <= e -> Just v
  _ -> Nothing

-- | How many keys have a value (the expanded map's size).
coveredCount :: IntervalMap a -> Int
coveredCount (IntervalMap _ n) = n

-- | How many intervals are stored (what the map actually costs).
intervalCount :: IntervalMap a -> Int
intervalCount (IntervalMap m _) = IM.size m
