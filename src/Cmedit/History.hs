-- | Bounded history stacks (undo/redo, navigation trail, input history).
--
-- A leaf module — it imports nothing from Cmedit — so both the editor state
-- and the CSV table model can use it without a cycle.
module Cmedit.History
  ( pushHist
  ) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | Push onto a bounded history stack, dropping the oldest entry past the cap.
--
-- The bound is *structural*, which is the whole point. @take n (x : xs)@ does
-- not discard anything: forced to WHNF it is one cons cell plus a thunk that
-- still holds the entire tail, so the cap only takes effect if something walks
-- the list that far — and nothing in the editor ever does (undo pops one
-- element; capture/restore move the field wholesale). Histories bounded that
-- way grew for the whole session: measured 51 MB live after 200 000 edits
-- against a nominal 1 000-entry cap, with one @THUNK_1_1@ per retained
-- snapshot visible in a @+RTS -hT@ heap census.
--
-- @Seq.length@ is O(1) and @Seq.take@ is O(log n) on a finger tree, and the
-- discarded part becomes unreachable at push time.
pushHist :: Int -> a -> Seq a -> Seq a
pushHist n x s
  | n <= 0            = Seq.empty
  | Seq.length s >= n = x Seq.<| Seq.take (n - 1) s
  | otherwise         = x Seq.<| s
