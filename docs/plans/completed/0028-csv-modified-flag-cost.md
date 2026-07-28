# 0028 — The CSV modified flag: maintain it, don't recompute it

**Theme:** per-keystroke cost in the table view
**Status:** ✅ **RESOLVED** — implemented 2026-07-27
**Risk (as shipped):** low — the flag is exact by construction and the exactness
is pinned twice: against plain structural equality *and* against an
independently-computed dirty count, after every one of 1 200 random operations

## Resolved

`Csv.isModified` used to compare the grid against the saved grid on every
keystroke. It is now a field read. On the 32 MB / 223 209-row × 12-column table
from `0026`'s generator:

| 100 operations on a 223 209-row table | Before | After |
|---|---|---|
| **cell edits at the LAST row** (begin/insert/commit **+ the flag**) | **245 ms, 1 420 MB** | **0 ms, 0 MB** |
| the same edits *without* the flag (0016's guard) | 0 ms, 0 MB | 0 ms, 0 MB |
| **`isModified` alone, grid MODIFIED at the last row** | **246 ms, 1 417 MB** | **0 ms, 0 MB** |
| **`isModified` alone, grid UNMODIFIED** | **355 ms, 1 421 MB** | **0 ms, 0 MB** |
| `isModified` alone, grid modified at row 0 | 0 ms, 0 MB | 0 ms, 0 MB |
| undo/redo pairs, with the flag | 4 029 ms, 8 773 MB | 4 195 ms, 9 226 MB |
| undo/redo pairs, without the flag | 3 505 ms, 5 931 MB | 4 153 ms, 9 226 MB |
| `columnWidths` alone (0016's other guard) | 0 ms, 0 MB | 0 ms, 0 MB |
| open the table (`mkCsvView`) | 824 ms, 903 MB | 835 ms, 903 MB |

Per keystroke that is **2.5 ms and 14 MB → 0**, and the cost no longer depends
on *where* in the file you are typing. Undo/redo is the one thing that got
worse; it is measured, attributed and argued below.

### Review found: two documents whose dirtiness the table parsed away

An adversarial review of this plan re-derived the invariants against an
independent oracle (≈11 000 further random operations over ragged, one-column,
single-cell, blank and wide-glyph tables, plus `markUnsaved` baselines) and
found the module's own biconditional intact — no producer, blessed site or
`ptrEq` in `Cmedit.Csv` is wrong. It found two real defects, both *outside* the
module and both the same shape, on the attack surface this plan's own "undo to
clean drops the journal" argument creates.

**The general fault: a `CsvView` carries its own saved baseline, so a document
that is dirty for a reason the buffer knows about has to say so twice.**
`Csv.mkCsvLines` adopts whatever grid it is handed as `csvSaved`, which is
right for a file just read off disk and wrong everywhere else. Two installers
handed it a buffer that was *already* dirty, and in both the next recompute
through `csvMod` — a bare Ctrl+Z will do, with no undo step to pop — declared
the document clean, and the journal sweep deleted the only copy of the work
while Ctrl+Q stopped asking.

**1. Crash recovery.** `recoveredDoc` (EditorDoc) is careful to give a recovered document
`docSavedBuffer = emptyBuffer`, with a comment saying exactly why: a recovered
buffer differs from disk by definition, so seeding the baseline with the
recovered text would let the first edit-and-undo declare it clean. The `docCsv`
built two lines below it was not given the same treatment, so a recovered
`.csv` opened with `Csv.isModified == False` underneath a document flagged
modified:

```
CSV recovered:  modified=True   table=False  liveKeys=["…-a.csv.cmj"]
CSV + Ctrl+Z:   modified=False  table=False  liveKeys=[]
CSV Ctrl+Q:     dialog=Nothing            -- quits without asking
```

**2. A staged workspace replace.** `stagedDoc` (EditorFind) sets `docCsv` and
`docCsvStash` to `Nothing` and a buffer that differs from disk, so a `.csv`
staged by a Replace All opens as plain text. **Alt+T** then reaches
`plainToCsv`'s no-stash branch, which parses the *dirty* buffer into a fresh
baseline — and the same bare Ctrl+Z discards the staged change and its journal.
(The stash branch was never wrong: `rebaseHistory` keeps the old saved point,
which is exactly the guard the fresh parse lacked.)

Pre-0028 both holes existed (`sameGrid` compared the same two fields), so
neither is a regression — but the recompute they turn on is now *instant and
exact* where it used to be an O(rows) comparison, which is precisely the kind of
consequence this plan makes reachable.

Fixed with a fourth blessed producer, `Csv.markUnsaved`, at both sites — beside
the empty `docSavedBuffer` in `recoveredDoc`, and in `plainToCsv` gated on
`bufModified || metaModified` so a clean document is untouched. It sets
`csvSaved` to the **empty grid**, which is an exact baseline rather than a
sentinel and therefore needs no special case anywhere: `mkCsvGrid` guarantees at
least one row, so the shapes differ, `DirtyShape` is the biconditional's own
answer, no cell write can restore a zero-row shape, and every dirty path stays
O(1) on it (the row-count test answers first). `markSaved` re-baselines normally
on the save that resolves either case, so the guard is not a latch — pinned as
such at both sites. Thirteen checks were added (bare Ctrl+Z, edit-then-undo, the
save that clears it, and a clean `.csv` that must stay clean); six of them fail
against the unfixed installers. Suite **3 127 → 3 140**.

### What was actually wrong, which was worse than reported

`0026` found this and described it as "free while the grid *is* the saved grid,
O(rows) after the first edit". The first half of that is not true, and finding
out why is the useful part of this plan.

```haskell
sameGrid a b = ptrEq a b || (Seq.length a == Seq.length b && and (zipWith sameRow …))
```

That top-level `ptrEq` — the thing the whole design leaned on — **never fires**.
Directly probed on an `-O2` build, `ptrEq (csvRows v) (csvSaved v)` returns
`False` for a view whose two fields were assigned from the same binding in
`mkCsvGrid` a moment earlier. The row-level tests inside it, on elements of two
`toList`s, *do* fire. So the real pre-0028 behaviour was:

* **an unmodified table:** a full walk of every row, every keystroke — the most
  expensive case, and the one 0016 and 0026 both recorded as free;
* **a table modified near the top:** free, because `and` stops at the first
  differing row;
* **a table modified near the bottom:** a full walk.

`0016` measured 0.04 ms and concluded "healthy". That number is an artefact of a
loop-invariant probe: `Csv.isModified v` in a repeat loop is floated out by full
laziness, so the harness timed one call and 99 comparisons of a `Bool`. The
probe in `Bench.hs` now moves the cursor first (an O(1) record update that
touches neither grid), which is enough to keep the call inside the loop. Both
mistakes have the same shape — *a cheap answer that was never actually
computed* — and between them they hid a per-keystroke cost through two plans.

This is CLAUDE.md's `ptrEq` rule (written for `Cmedit.Rtf`'s `rtfStale`) in a
second place: **`ptrEq` is only sound as a fast path in front of a real
comparison.** The new code obeys it — every `ptrEq` in the dirty machinery is in
front of a real `==`, and a spurious `False` costs time and never correctness —
but the deeper lesson is that a *cost* argument may not rest on one either. If
the affordability of a design depends on a pointer test firing, measure the
pointer test.

## The design

`CsvView` gains one field, `csvDirty :: !CsvDirty`, maintained by exactly the
functions already allowed to move `csvRows` or `csvSaved` — the same discipline
`csvWidths` has had since `0016`:

```haskell
data CsvDirty = DirtyShape | DirtyCells !Int
```

**The invariant is a biconditional, and both directions are load-bearing.**

* `DirtyShape` holds **exactly when** the two grids have different shapes: a
  different row count, or some row index at which the two rows have different
  lengths.
* `DirtyCells n` holds **exactly when** the shapes are equal and exactly `n`
  cell positions hold different text.

Hence `isModified v = csvRows v /= csvSaved v`, in O(1), exactly, at any size.

Three producers, and nothing else may construct a `CsvDirty`:

| | when | cost |
|---|---|---|
| `dirtyCell` | one cell written (`withCell`) | **O(log rows)** |
| `syncDirty` | a grid change (`withRows`, undo, redo) | O(rows) pointer hops + the changed rows |
| `dirtyFrom` | a grid arrives wholesale (`rebaseHistory`), or the state cannot be carried | O(grid), pointer-accelerated |

### Why cells and not rows

The obvious index-keyed state is a set of dirty *rows*. A count of dirty
**cells** is strictly better here, and the reason is what makes the typing path
O(log rows) instead of O(rows):

**a cell write can compute its own delta.** `withCell` knows the cell's old
text, its new text and — one `Seq.lookup` away — its saved text, which is
everything needed to move the count by ±1 without consulting anything else:

```haskell
DirtyCells (n - diffBit old saved + diffBit new saved)
```

Editing a cell **back to its saved value** therefore un-dirties it by
construction rather than by a special case, which is the property the whole
feature exists for (and is pinned by hand and by fuzz). A row-keyed set needs
the same lookup *plus* a comparison of the whole row, and an `IntSet` that grows
with the number of edited rows; the count is one machine word whether one cell
or two million are dirty.

### Why shape is a separate constructor

Two reasons, and the second is the one that matters.

**It is free.** If the shapes differ the grids differ, with no cell comparison
at all — so a row insert or delete is answered by the row-count test in O(1),
and a column insert or delete by the first row the walk looks at.

**It cannot be carried by index.** Any index-keyed state is invalidated by a
shape change: after inserting a row, "cell (7, 2) differs" addresses a different
cell. Rather than fix up indices, `DirtyShape` says *the shape is wrong*, which
no cell write can change — that is the forward direction of the biconditional,
and it is what lets `withCell` keep the state without looking at the grid. When
a change *restores* the shape (delete the row you inserted; undo a column
delete), the reverse direction forbids guessing, so `syncDirty` pays one full
recompute. A shape change is rare next to typing, and is already an O(grid)
operation in the width cache.

### Undo/redo: recomputed, not snapshotted

Undo and redo restore whole grids, so they carry the state through `syncDirty`
against the *current* rows — one pointer-accelerated walk, exactly like the
`syncWidths` call sitting beside them.

The alternative — storing the dirty state alongside each undo snapshot — was
rejected on correctness before cost. A stored state is computed against a
particular `csvSaved`, and `markSaved` moves that mid-history, so every stored
state would need a guard proving the baseline had not changed. The only cheap
guard available is a pointer test on the saved grid, and the measurement at the
top of this document is that such a test does not fire under `-O2`. A guard that
never fires is a dead branch and a full recompute anyway, so the honest version
is the one that recomputes.

The cost of that decision is the one regression here: an undo/redo pair on the
223 209-row table goes from 88 MB to 92 MB and from 40.3 ms to 42.0 ms
(**+5 % allocation, +4 % time**), because `syncDirty`'s walk is slightly dearer
than the `sameGrid` walk it replaced — it also indexes the saved grid and counts
cells in the rows that changed. Undo is not a per-keystroke path, and the
attribution is exact: before, `isModified` was 5.2 ms and 28 MB of the pair;
after, `syncDirty` is 6.9 ms and 33 MB. The remaining ~35 ms and ~59 MB of an
undo/redo pair is `syncWidths` and the history `Seq`s, which is unchanged by
this plan and is where the prize is if anyone wants it: `syncWidths` and
`syncDirty` walk the same two grids one after the other, and fusing them would
make undo cheaper than it was before this plan. Not taken here — `syncWidths` is
`0016`'s most invariant-critical function ("a wrong width is a visibly broken
table"), and this plan should not be the thing that touches it.

### The blessed sites

`csvDirty` is set in exactly eight places, which are exactly the places
`csvWidths` is set plus the two that move `csvSaved`:

| site | what it does |
|---|---|
| `mkCsvGrid` | `DirtyCells 0` — the grid *is* the saved grid |
| `withCell` | `dirtyCell` (the typing path) |
| `withRows` | `syncDirty` |
| `undo` / `redo` | `syncDirty` |
| `markSaved` | `DirtyCells 0` — new baseline |
| `markUnsaved` | `DirtyShape` against the empty grid — no trustworthy baseline (crash recovery, a staged replace; see the review note above) |
| `rebaseHistory` | `dirtyFrom` — a new grid against an old saved point |

Every mutating operation in the module funnels through `withCell` or
`withRows`, so `pasteClip`, `clearSelCells`, `fillSelCells`, `mapCells`,
`setCells`, `sortByColumn` and the row/column inserts and deletes need no code
of their own — and the fuzz test below exercises all of them so that a future
site added outside the funnel is caught rather than assumed.

## Guards (38 new checks)

**The fuzz test is the guard, extended twice over.** The existing 600-operation
`csvWidths` fuzz test (0016, strengthened by 0026) gained eight operations —
`insertRowAbove`, `insertColLeft`, `mapCells`, `sortByColumn`, `clearSelCells`,
a shaped `pasteClip`, a scalar `pasteClip` over a selection, and `markSaved`
mid-history — so the random script now reaches every mutating function in the
module, and a second 600-operation run over a wider (5-column, ragged, blank-
bearing) table with a different seed was added. After **every** step, four
things are asserted:

* `columnWidths v == columnWidths (fresh reparse)` — 0016's invariant, now over
  a wider operation set (sort and paste through the width cache are new
  coverage in their own right);
* `isModified v == (csvRows v /= csvSaved v)` — the boolean, against plain
  structural equality;
* `csvDirty v == dirtyRef v`, where `dirtyRef` is computed **in the test** by
  plain comparison with no pointer tricks and nothing incremental. This is the
  assertion that catches a sign error which happens to keep the boolean right
  today and drifts on the next keystroke;
* `dirtyFrom (csvSaved v) (csvRows v) == dirtyRef v` — the from-scratch producer
  against the same oracle, so the recompute paths cannot rot behind the
  incremental one.

Plus 36 hand-pinned cases, each of which is a way for the count to be wrong
while the boolean still looks plausible:

* **edit a cell back to its saved value** → `DirtyCells 0` and not modified;
* **two cells dirty, one put back** → `DirtyCells 1`, still modified (a count
  that saturated at one, or a boolean that latched, passes the case above and
  fails this one);
* uncommitted per-keystroke typing → one dirty cell; cancelling it → clean;
* **shape change then revert of the shape** (insert a row then delete it;
  insert a column then delete it) → clean again, both ways;
* the same **over an outstanding cell edit** → the cell is still counted;
* **undo to clean** → `DirtyCells 0` and not modified, which is what the journal
  sweep relies on to drop a journal — an editor that stayed "modified" after
  undoing to the saved grid would keep offering to recover a file the user has
  not changed;
* undo/redo across a *structural* edit → clean / `DirtyShape`;
* `markSaved` mid-history, an edit after it, and an undo *past* it;
* replace-all over every cell and its undo; a descending sort and sorting back;
* a same-shaped paste (four cells) and a paste that grows the grid (a shape
  change);
* `rebaseHistory` onto an old saved point, with and without a text edit.

The full suite is **3 089 → 3 127**, all passing, including 0026's parser
oracle and the serialisation corpus unchanged. `make windows-check` passes and
`docs/plans/bench/pty_journal.py` is 4/4 (the modified flag is what
`journalLiveKeys` filters on, so it is the journal's input as much as the title
bar's).

## The measurements

`bench csv` on the corpus from `docs/plans/bench/gen_csv.py`
(32 MB, 223 209 rows × 12 columns, quoted fields, embedded delimiters, doubled
quotes, multi-line cells, ragged rows), `-O2` with `-T`, before and after built
from the same tree with only this change reverted. Two probes were added and one
was fixed:

* the repeat-loop fix described above (a floated-out call measured nothing);
* `isModified` on a **modified** grid, at the last row and at row 0, which is
  the difference between 246 ms and 0 ms and is the shape of the whole defect;
* `sameGrid` itself, kept in the harness as `0026` kept `csvParsePrev` — run
  once with its pointer shortcuts and once without, because whether they fire
  under `-O2` is not something the source can be read to decide:

| 100 pre-0028 comparisons, modified grid | Time | Allocated |
|---|---|---|
| pointer shortcuts on | 239 ms | 1 385 MB |
| pointer shortcuts **off** | 4 539 ms | 16 017 MB |
| on, grid built from the buffer's lines (the editor's own path) | 236 ms | 1 381 MB |

The middle row is what the comparison costs when *no* shortcut fires — 45 ms per
keystroke — and is the margin the row-level tests were buying. The third row
confirms the load path 0026 introduced makes no difference to any of this.

## Found along the way, not fixed here

**Typing into the last row of a 223 209-row CSV costs ~390 ms per keystroke in
the shipped binary, and this plan does not change it.** Driven over a PTY
against the real `./cmedit`, 30 keystrokes into the last row after Ctrl+End:

| | median | p95 | bytes emitted per keystroke |
|---|---|---|---|
| at row 0, either build | 1.7 ms | 3.6 ms | 32 939 |
| at the last row, before this plan | 390.1 ms | 411.2 ms | 6 028 |
| at the last row, after this plan | 393.9 ms | 479.9 ms | 6 028 |

It is not the modified flag (identical in both builds), not the journal
(`journal = off` changes nothing) and not the emit (6 KB). And the pure model
does not reproduce it: `update` + `refreshHighlight` + `renderEditor` over
exactly that state — cursor on the last row, `csvTop` scrolled to match — costs
**0.6 ms** per keystroke in-process. So it lives in the driver, and it wants a
plan of its own.

Two traps in measuring it are worth recording, because each produced a confident
wrong answer first. **A run leaves a journal behind, and the next start
*recovers* it** — which opens a modified plain-text buffer instead of the table,
so the probe measures a different document and reports 1.6 ms; the journal must
be removed before *and* after every run. And **the jump to the last row is
occasionally swallowed**, so the probe must read `Cell L223209` back off the
status bar and retry rather than assume the cursor moved — a silently unmoved
cursor measures row 0, which is the fast case. The first "after" number this
plan produced was 1.6 ms against 390 ms, and it was wrong in both traps at once.

## What was tried and not taken

**A dirty-row `IntSet`.** The obvious index-keyed state. Rejected for the reason
in the design above: a cell write would have to compare a whole row where the
count compares one cell, and the set grows with the number of edited rows where
the count does not grow at all. Both are exact; the count is exact more cheaply.

**Snapshotting the dirty state with each undo entry.** See the undo section: the
guard it needs is a pointer test on the saved grid, and this plan's headline
measurement is that such a test does not fire.

**Fusing `syncWidths` and `syncDirty` into one walk.** Would make undo/redo
cheaper than before this plan rather than 4 % dearer, and would help
`clearSelCells` too. Deliberately out of scope: it is a change to `0016`'s width
cache, which is the one thing in this module whose failure mode is a visibly
broken table rather than a slow one.

**Anything approximate.** A big-table cutoff or a "close enough" flag is not
available here at any price: the flag drives the title bar, the quit
confirmation, Save All, `edDocSeq` and therefore the crash journal, and the
journal sweep that deletes journals for documents that are no longer modified.
Getting it wrong loses work. Every path in the new code that cannot carry the
state exactly falls back to a full recompute, and the two defensive `Nothing`
branches (a saved cell that should exist and does not) recompute rather than
guess.
