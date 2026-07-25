# 0009 — Undo as an edit log instead of buffer snapshots

**Theme:** memory model; a design change, not a bug fix
**Status:** ⛔ **CLOSED — not needed.** The gate this plan set for itself was
never met. 2026-07-26

## Outcome

This plan was explicit about its own precondition:

> **If, after `0001`, a realistic session's undo history is a few MB, do not do
> this plan.**

It is. Measured after `0001` landed:

- **The soak** (`make soak`, 60 000 scripted operations including continuous
  typing, undo/redo, line duplication and CSV edits): live heap **0.3 MB → 0.4
  MB**, flat.
- **The benchmark**: 200 000 snapshot-pushing edits leave the undo stack at its
  1 000-entry cap with a live heap that does not grow with session length.
- **The heaviest case this plan worried about** — whole-buffer operations on a
  big document — measured on a 300 001-row CSV: 500 retained grid snapshots and
  20 full sorts cost **+0 MB**, because `Seq` shares every untouched row.

Snapshots are correct by construction and, with structural sharing, cheap.
Replacing them with an operation log would trade that for a correctness surface
on the feature users trust most — every mutating path would have to record its
ops faithfully, and a single unrecorded path corrupts undo silently.

**Do not implement this without a new measurement showing undo memory actually
matters.** If that day comes, the hybrid in §3 (ops only for the three
whole-buffer operations, snapshots everywhere else) is still the right shape,
and the round-trip property test in §5 is still the thing that would make it
trustworthy.

The plan below is the original analysis.

---

---

## 1. The current model and what it costs

`UndoState` snapshots the whole buffer:

```haskell
data UndoState = UndoState
  { usBuffer :: !Buffer, usCursor :: !Pos, usAnchor :: !(Maybe Pos) }
```

This is a genuinely good design for a `Seq`-of-lines buffer: a single-line edit
produces a new spine that shares every untouched line, so a snapshot costs
O(log n) new finger-tree nodes plus the one changed `Text`. That is why 1 000
snapshots of a large file are affordable at all.

It stops being cheap exactly where `Seq` sharing stops:

| Operation | Snapshot cost |
|---|---|
| Type a character | O(log n) — excellent |
| Delete a selection spanning k lines | O(log n + k) |
| Move a line (Alt+↑/↓) | O(log n), coalesced into one step |
| **Replace All across the buffer** | O(n) — a whole new spine |
| **Save-time fixups** (trim trailing whitespace on every line) | O(n) |
| **Paste of a large block** | O(n) |
| **Reindent / toggle comment over a big selection** | O(changed lines + log n) |

A session that runs Replace All a dozen times over a 300 000-line file retains
a dozen full spines. Each is ~n/8 finger-tree nodes; at 300k lines that is on
the order of tens of MB *per snapshot*.

## 2. The alternative

Store **operations**, not states:

```haskell
data EditOp
  = OpInsert !Pos !Text          -- ^ text inserted at Pos
  | OpDelete !Pos !Text          -- ^ text removed at Pos (kept for the inverse)
  | OpBatch ![EditOp]            -- ^ one user-visible step (Replace All, a paste)
  deriving Show

data UndoStep = UndoStep
  { usOps      :: ![EditOp]      -- ^ applied in order; undone in reverse
  , usCursorBefore, usCursorAfter :: !Pos
  , usAnchorBefore :: !(Maybe Pos)
  }
```

Undo applies the inverses in reverse; redo re-applies. Memory is proportional
to the *text changed*, not to the file. A Replace All of 5 000 occurrences of a
10-character term costs ~100 KB, not a spine.

### Why this is not obviously the right call

- **Every edit primitive must produce its ops.** `TextBuffer` currently has
  clean `insertChar` / `deleteRange` / `insertText` primitives, so the ops fall
  out naturally — but the *editor-level* operations (line move, join, toggle
  comment, save fixups, CSV sync-to-buffer) all edit through those primitives
  in ways that would need to record their ops. That is the bulk of the work
  and the bulk of the risk.
- **Undo must restore exact state, not equivalent state.** Snapshots are
  trivially correct. An op log is correct only if every mutating path records
  faithfully; a single unrecorded path corrupts undo silently, and the user
  discovers it at the worst moment.
- **Coalescing gets more subtle.** Today `beginEdit` coalesces by `EditKind`;
  with ops, coalescing means *merging adjacent inserts*, which is nicer but
  another correctness surface.

## 3. Recommended path: hybrid, and only if measured

A hybrid keeps almost all of the benefit with a fraction of the risk:

1. **Keep snapshots as the model** (they are correct by construction).
2. **Add a size guard**: when a snapshot's buffer does not share the previous
   one's spine — detectable cheaply, since `Seq` sharing means pointer identity
   of subtrees, or simply by flagging the *operations* that are known to be
   whole-buffer (Replace All, save fixups, paste over a threshold) — record an
   `OpBatch` entry instead of a snapshot for those specific operations.
3. That gives an op log exactly where snapshots are expensive, and snapshots
   everywhere they are cheap, with a small, enumerable list of paths to get
   right (`applySaveFixups`, `replaceAllInBuffer`, large `applyPaste`).

```haskell
data UndoEntry = USnap !UndoState | UOps !UndoStep
```

`undo`/`redo` become a two-case dispatch. The number of code paths that must
produce ops drops from "all of them" to "three".

## 4. Prerequisites

- `0001` must land first: with the lazy-`take` bug in place, no measurement of
  undo memory means anything.
- `0005`'s soak harness should record undo-stack live bytes across a scripted
  session including Replace All, so the decision to do this at all is made on
  numbers. **If, after `0001`, a realistic session's undo history is a few MB,
  do not do this plan.**

## 5. Testing (if pursued)

- **Round-trip property.** For a random script of edits, assert that undoing
  every step returns the buffer to `bufferToText`-equality with the original,
  and that redoing every step returns it to the final state. Run it over
  thousands of random scripts — this is the single test that makes an op log
  trustworthy.
- **Cursor and selection fidelity** after undo/redo, which snapshots give for
  free and ops must reconstruct.
- **Interaction with the modified flag**: `metaModified` composition must still
  hold (EOL/BOM changes keep a file dirty regardless of text undo).
- **Interaction with `HlCache`**: the cache is self-validating against the line
  `Seq`, so an op-based undo produces a new `Seq` exactly as a snapshot restore
  does — no change needed, but assert it.
