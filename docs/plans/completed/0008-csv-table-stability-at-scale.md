# 0008 — CSV table view: memory and cost at spreadsheet scale

**Theme:** long-session memory; the heaviest per-edit state in the editor
**Status:** ✅ **CLOSED** — §2.1 shipped with `0001`; §2.2–§2.5 measured and
found unnecessary. 2026-07-26

## Outcome

**§2.1 (the retention bug) is fixed** — `csvUndo`/`csvRedo` became bounded
`Seq`s under `Cmedit.History.pushHist` as part of `0001`, and `0016` made cell
edits O(1) rather than O(rows).

**Everything else in this plan was premised on grid snapshots being expensive.
They are not.** Measured on the 32 MB / 300 001-row table, with the undo entries
genuinely forced (150 million rows summed across the stack, so nothing is hiding
in a thunk):

| | Live heap |
|---|---|
| after parsing | 221 MB |
| after 600 cell edits (500 snapshots retained) | 222 MB (**+0**) |
| after 20 full column sorts | 221 MB (**+0**) |

`Seq`'s structural sharing covers both cases: a cell edit shares every untouched
row, and — the case this plan doubted — **a sort shares every row object too**,
since reordering rebuilds only the spine. The permutation-entry design (§2.2)
would have added a second undo representation, with the ordering-sensitivity
that implies, to save nothing measurable.

- **§2.2 (permutation undo entries)** — not needed; see above.
- **§2.3 (byte budget for undo)** — not needed. 500 entries cost ~0 on the
  largest table that fits under `maxOpenBytes`.
- **§2.4 (`csvWidths` under bulk changes)** — covered instead by `0016`, whose
  fuzz test now asserts the cache equals a fresh recomputation after every one
  of 600 random operations including sorts, inserts, deletes and per-keystroke
  edits.
- **§2.5 (the modified flag at scale)** — already healthy: `isModified` measures
  0.04 ms per call on the 300 001-row table, because `sameGrid`'s pointer
  shortcuts short-circuit almost immediately. The proposed `csvDirtySeq`
  fingerprint would optimise something that costs nothing.

Kept as a record of what was checked, so the same ideas are not re-derived.

The plan below is the original analysis.

---

---

## 1. Why CSV is the sharpest case

`Cmedit.Csv` keeps its own undo history, and each entry is a **whole grid**:

```haskell
csvUndo :: ![Grid]
snapshot v = v { csvUndo = take maxUndo (csvRows v : csvUndo v), csvRedo = [] }
```

Three properties compound:

1. **The lazy-`take` retention bug of `0001` applies here too** (three push
   sites: `Csv.hs:389`, `:641`, `:1208`), so the 500-entry cap never actually
   discards anything.
2. **Each entry is a full grid**, not a delta. Structural sharing via the
   persistent `Seq` keeps the *unchanged rows* shared, so a single-cell edit
   costs a spine delta, not a copy — but a **row insert/delete or a sort**
   (`sortByColumn`, Alt+S) rebuilds the row sequence, so those snapshots share
   nothing and each one is a full grid's worth of spine.
3. **Sorting a large table is a snapshot per sort.** Alt+S toggling asc/desc
   on a 200 000-row table pushes a fresh, unshared grid each time. Ten toggles
   while exploring data is ten full grids retained.

So the worst realistic case is not typing: it is a data-exploration session —
open a large CSV, sort by a few columns, insert/delete some rows, undo a bit —
which is precisely what the table view exists for.

## 2. Work items

### 2.1 Fix the retention (do with `0001`)

Convert `csvUndo`/`csvRedo` to `Seq Grid` with the structural `pushHist`. This
is a prerequisite for everything below: without it, the cap is fiction and any
measurement of the model's cost is meaningless.

### 2.2 Make sort snapshots share

`sortByColumn` currently snapshots the pre-sort grid and produces a reordered
one. A permutation-aware representation would make it O(rows) pointers rather
than a new spine, but the simpler and better win is to store, for *reordering
operations only*, an **inverse permutation** instead of a grid:

```haskell
data CsvUndoEntry
  = UGrid !Grid                 -- ^ arbitrary change (cell edits, shape changes)
  | UPerm !(UArray Int Int)     -- ^ row reordering: apply the inverse to undo
```

An `UArray Int Int` for 200 000 rows is 1.6 MB and shares every row `Text`;
the grid it replaces is a fresh finger-tree spine over the same rows. Undo
becomes "permute back", which is also faster than restoring a whole grid.

Keep `UGrid` for everything else — the point is to special-case the one
operation that defeats sharing, not to build a general delta system (that is
`0009`'s territory, and the same argument applies).

### 2.3 Bound the undo *bytes*, not just the entries

500 entries is the wrong unit when entries differ by four orders of magnitude
in size. Add a cheap size estimate (rows × 1 word for the spine is a good
enough proxy; `Seq.length` is O(1)) and cap on a **byte budget** as well as a
count:

```haskell
maxUndoBytes :: Int      -- e.g. 64 MB across a document's history
```

Push drops the oldest entries until the budget is met. This is the standard
editor behaviour (VS Code, Emacs both bound undo by size) and it makes the
worst case predictable regardless of table size. Apply the same idea to the
text undo stack in `0001` as a follow-up.

### 2.4 Check `csvWidths` maintenance under bulk changes

`columnWidths` must never rescan the grid (documented invariant), and
`withRows`/`syncWidths` uphold it via pointer-diffing. Two things to verify
under scale, ideally in the soak harness:

- `sortByColumn` reorders rows without changing widths — confirm `syncWidths`
  takes the cheap path there rather than the "shape changed → recompute" path,
  since a sort changes no widths at all.
- `csvUserW` (sparse per-column overrides) shifts on `insertColAt`/`deleteCol`;
  confirm the shift is O(overrides), not O(columns).

### 2.5 The modified flag at scale

`Csv.isModified` (`sameGrid`, run per keystroke via `csvMod`) is documented as
exact with pointer shortcuts. After 2.2 introduces permutation entries, a
sorted-then-unsorted table must still compare equal to `csvSaved` — the
pointer shortcut per row still applies, but the *row order* differs, so the
comparison walks every row. That is O(rows) pointer compares per keystroke on
a sorted table. Add a cheap guard: track a `csvDirtySeq :: !Int` bumped by
every mutating operation and compare it against the value at save time first;
only when they *could* have returned to equality does the full compare run.
(This mirrors `bufChars` in `TextBuffer` — an O(1) fingerprint that
short-circuits the O(n) check.)

## 3. Testing

- Extend the existing `csvWidths` fuzz test in `Spec.hs` to also assert
  undo/redo round-trips after random operation scripts that include sorts.
- Add a residency assertion (via `0005`'s soak harness): 10 000 random table
  operations on a 50 000-row grid must not grow live bytes beyond a ratio of
  the 1 000-operation figure.
- Assert `undo` after `sortByColumn` restores the exact original row order,
  including with a frozen header row and with `csvUserW` overrides set.

## 4. Risks

- The `UPerm` entry changes what undo *means* for a sort followed by a cell
  edit: the permutation must be applied to the grid as it exists at undo time,
  which is correct only if entries are undone in order — which they are, since
  it is a stack. Document this invariant next to the type.
- A byte budget that silently drops history is a user-visible behaviour
  change. Surface it: when history is trimmed for budget, the status line can
  say so once (the codebase already has the "shown once" idiom for config
  warnings).
