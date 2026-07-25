# 0001 — Strict bounded history stacks (undo, redo, nav, input history)

**Theme:** long-session memory stability
**Status:** ✅ **RESOLVED** — implemented 2026-07-26
**Risk (as shipped):** low — pure refactor, no interface or behaviour change

## Resolved

New leaf module **`Cmedit.History`** provides `pushHist`, a *structurally*
bounded push over `Data.Sequence`. Six histories converted from
`[a]` + lazy `take` to `Seq a` + `pushHist`:

| Field | Module |
|---|---|
| `edUndo`, `edRedo`, `docUndo`, `docRedo` | `EditorState` / `EditorEdit` / `EditorDoc` / `EditorFind` |
| `csvUndo`, `csvRedo` (whole grids) | `Csv` |
| `edNavBack`, `edNavFwd` | `EditorState` / `EditorDoc` / `Editor` |
| `edFindHist`, `edReplHist` | `EditorFind` (list at the persistence boundary only) |

This also fixed the two pushes in `undo`/`redo` that had **no cap at all**
(`EditorEdit.hs:247, 263`), and replaced the nested `take n (t : filter (/= t) …)`
in the input history with a strict `Seq.filter`.

Measured with the same harness that found the bug (`bench undo N`, `-O2`):

| Snapshot-pushing edits | Live heap before | Live heap after |
|---|---|---|
| 5 000 | 2 MB | **~0 MB** |
| 20 000 | 5 MB | **~0 MB** |
| 80 000 | 21 MB | **~0 MB** |
| 200 000 | 51 MB | **~0 MB** |

Flat at every session length, depth pinned at the 1 000 cap. Per-keystroke
timings are unchanged (2.0 ms at 120-char lines, same as before).

### A second bug found while verifying

The unoptimised (`-O0`) test build still showed linear growth. Chasing it with
`GHC.Exts.Heap` showed the retained objects were **thunks holding previous
versions of the edited line**, one per snapshot: `Data.Sequence` is spine-strict
but *element-lazy*, so a line stored unforced chains to the line it replaced.
`-O2`'s simplifier removes the chain, which is why it never showed up in a real
build. Buffer writes are now explicitly strict (`Seq.adjust'` plus bangs on the
constructed lines in `TextBuffer`), so the space behaviour no longer depends on
the optimiser. Residual `-O0`-only laziness remains in the surrounding code and
is noted in the test.

### Guards added (`test/Spec.hs`, suite now 1164 passing)

- undo depth is exactly `maxUndo` after `maxUndo + 500` edits;
- `pushHist` caps, keeps newest-first ordering, and drops the tail;
- the navigation trail is capped when driven through the real key handlers;
- a retention measurement (`undoRetentionMB`, needs `-with-rtsopts=-T`, which
  the test target now sets) comparing 4 k against 40 k edits.

Removing the bound makes five of these fail, so they discriminate.

The plan below is the original analysis, kept for the record.

---

---

## 1. The problem

Every bounded history in the editor is bounded with a *lazy* `take`:

| Site | Code |
|---|---|
| `src/Cmedit/EditorEdit.hs:236` | `edUndo = take maxUndo (snapshot ed : edUndo ed)` |
| `src/Cmedit/EditorEdit.hs:247` | `edRedo = snapshot ed : edRedo ed` (no cap at all) |
| `src/Cmedit/EditorEdit.hs:263` | `edUndo = snapshot ed : edUndo ed` (no cap at all) |
| `src/Cmedit/EditorEdit.hs:482` | `docUndo = take maxUndo …` (per-document push) |
| `src/Cmedit/EditorFind.hs:813` | `docUndo = take maxUndo (snap : docUndo d)` |
| `src/Cmedit/Csv.hs:389, 641, 1208` | `csvUndo = take maxUndo (csvRows v : csvUndo v)` |
| `src/Cmedit/EditorState.hs:1276` | `edNavBack = take maxNavStops (cur : edNavBack ed)` |
| `src/Cmedit/EditorDoc.hs:871, 881` | `edNavFwd/edNavBack = take maxNavStops …` |
| `src/Cmedit/EditorFind.hs:495, 500` | `edFindHist`/`edReplHist = take maxHistoryEntries (t : filter (/= t) …)` |

`take n xs` does not discard anything. Forced to WHNF it yields
`x : take (n-1) xs'` — a **cons cell plus a thunk that still holds `xs'` in
full**. Because the record field is strict, each edit forces exactly one cons
cell and re-suspends the rest. After *k* edits the field is a chain of *k*
nested `take` thunks, and every snapshot ever pushed is still reachable from
the youngest one. The cap only takes effect if something walks the list past
element 1000 — and nothing in the editor ever does. `undo` pattern-matches one
element; `captureDoc`/`restoreDoc` move the field wholesale; nothing calls
`length`.

Net effect: **undo history is unbounded in a long session**, and the same
applies to the CSV grid history (whose elements are whole tables) and, more
mildly, to nav stops and find history.

This is exactly the failure mode the goal calls out: nothing looks wrong for
the first ten minutes, and after six hours of editing the process is hundreds
of megabytes larger than it should be, with major GCs that get slower as the
live set grows.

## 2. Evidence

Measured with a harness built against `src/` (`ghc --make … -O2 -with-rtsopts=-T`),
alternating `KChar 'x'` / `KBackspace` through `Cmedit.Editor.update` so every
edit pushes a snapshot (the two kinds do not coalesce), on a 2000-line Python
buffer. Live bytes are `GHC.Stats.gcdetails_live_bytes` after
`performMajorGC`, with the editor still reachable:

| Snapshot-pushing edits | Live heap (spine unforced) | After forcing the spine | Depth |
|---|---|---|---|
| 5 000 | 2 MB | ~0 MB | 1000 |
| 20 000 | 5 MB | ~0 MB | 1000 |
| 80 000 | 21 MB | ~0 MB | 1000 |
| 200 000 | 51 MB | ~0 MB | 1000 |

Growth is linear in the number of edits, not flat at the cap. ~0.26 KB
retained per edit *on a small file*; per-snapshot cost scales with the
`Seq` spine delta, so on a 400k-line file, or in CSV mode where each entry is
a whole `Grid`, the per-edit retention is several times larger.

The "after forcing" column is the cap working as intended: the memory is
recoverable, it is simply never reclaimed because nothing walks the list.

### A heap census names the culprit

`+RTS -hT` (a heap profile by closure type) needs **no profiling build** — it
works on the ordinary `-O2` binary, so this recipe applies to the shipped
editor as much as to the harness. A census taken mid-run at 80 000 edits:

```
    2589360  containers:Data.Sequence.Internal.Deep
    2135424  text:Data.Text.Internal.Text
    2071552  containers:Data.Sequence.Internal.Three
    2071328  main:Cmedit.EditorState.UndoState      <-- ~64 700 live UndoStates
    2071328  THUNK_1_1                              <-- exactly one thunk each
    1896056  ARR_WORDS
    1553472  main:Cmedit.Types.Pos
```

`UndoState` and `THUNK_1_1` are present in identical byte counts: one suspended
`take (n-1) …` per retained snapshot, ~64 700 of them where the cap says 1 000.
That is the lazy-`take` chain, visible in the heap.

## 3. Design

Replace the list-with-lazy-`take` idiom with a structure whose bound is
*structural*, so the cap cannot be forgotten and costs no extra traversal.

`Data.Sequence` is already a dependency and already the buffer's line store.
`Seq.take` on a finger tree is O(log n) **and drops the discarded part
immediately** — the spine is strict, so nothing beyond the cap stays
reachable.

```haskell
-- Cmedit.EditorState (or a new leaf module Cmedit.History)

-- | Push onto a bounded history, dropping the oldest entry past the cap.
-- Unlike `take n (x : xs)` this bound is *structural*: the discarded tail
-- becomes unreachable at push time instead of staying alive behind a lazy
-- `take` thunk.
pushHist :: Int -> a -> Seq a -> Seq a
pushHist n x s
  | Seq.length s >= n = x Seq.<| Seq.take (n - 1) s
  | otherwise         = x Seq.<| s
```

`Seq.length` is O(1) on a finger tree, so the guard is free.

Field changes:

```haskell
edUndo, edRedo   :: !(Seq UndoState)      -- was ![UndoState]
docUndo, docRedo :: !(Seq UndoState)
csvUndo, csvRedo :: !(Seq Grid)
edNavBack, edNavFwd :: !(Seq NavStop)
edFindHist, edReplHist :: !(Seq Text)
```

Pattern matches become `Seq.viewl` (or the `:<|` pattern synonym, available in
containers ≥ 0.5.8 — GHC 9.6's boot containers is 0.6.7, so `x :<| rest`
works and reads exactly like the current list match).

`undo`/`redo` become:

```haskell
undo ed = case edUndo ed of
  Seq.Empty -> ed { edStatus = "Nothing to undo" }
  (u :<| us) -> ensureVisible ed
    { edUndo = us
    , edRedo = pushHist maxUndo (snapshot ed) (edRedo ed)   -- now capped too
    , … }
```

Note this also fixes the *uncapped* redo/undo pushes in `undo`/`redo`
(`EditorEdit.hs:247,263`): today a long undo/redo ping-pong grows the opposite
stack without any bound at all.

For `edFindHist`, keep the de-duplication but do it against a bounded
structure so the nested-`filter` chain disappears as well:

```haskell
-- Cmedit.EditorFind, replacing pushFindHist / pushReplHist (:492, :497)
pushFindHist t ed
  | T.null t  = ed
  | otherwise = ed { edFindHist = pushHist maxHistoryEntries t
                                    (Seq.filter (/= t) (edFindHist ed)) }
```

`Seq.filter` is strict, so unlike today's `take n (t : filter (/= t) old)` there
is no stack of N nested filters left suspended for a later read to re-run.

## 4. Implementation steps

1. **Add `pushHist`** (and `histToList`, `histFromList` for the persistence
   boundaries) to `Cmedit.EditorState`. Two exports, no new module needed —
   though a tiny `Cmedit.History` leaf module is equally fine and keeps
   `EditorState` from growing.
2. **Flip `edUndo`/`edRedo`/`docUndo`/`docRedo`** to `Seq UndoState`. Touch
   points: `EditorState.hs` (records + `newEditor`), `EditorDoc.hs`
   (`captureDoc`/`restoreDoc`/`setLoaded`/`doNew` — all of which just move or
   reset the field, so `Seq.empty` replaces `[]`), `EditorEdit.hs`
   (`beginEdit`/`undo`/`redo` + the per-document push at :482),
   `EditorFind.hs:813`.
3. **Flip `csvUndo`/`csvRedo`** in `Cmedit.Csv` (records, `newCsvView`,
   `snapshot`, the two other push sites, `undo`/`redo`). CSV is where the
   payoff is largest — each entry is a whole grid.
4. **Flip `edNavBack`/`edNavFwd`** (`EditorState.hs:1276`, `EditorDoc.hs:865-881`,
   plus the prefix-rewrite in `Editor.hs:1842`, which becomes `fmap`).
5. **Flip `edFindHist`/`edReplHist`**, and convert at the persistence boundary
   only (`App.hs:96` and `saveHistoryFile`, `recentsForPersist`) with
   `Data.Foldable.toList`.
6. **Leave `edRecent` alone** or convert it for consistency: it is already
   effectively bounded because `maybePersistRecents` forces the whole list
   (`map rePath (edRecent ed)`) after every key batch. Converting it removes
   that accidental dependency, which is worth doing so a future refactor of
   the persistence check cannot silently re-introduce the leak.
7. **Grep for `take max`** afterwards and assert the idiom is gone:
   `grep -rn "take max" src/` should return nothing.

## 5. Testing

Add to `test/Spec.hs`:

1. **Structural cap.** Push `maxUndo + 500` snapshots through `beginEdit`
   (alternating `EKType`/`EKDelete` so nothing coalesces) and assert
   `Seq.length (edUndo ed) == maxUndo`. This is the test that would fail
   today, since the list-based version *reports* 1000 only because `length`
   forces the cap into existence — so word the test to run through the
   editor's own API, and add the retention check below to catch the real bug.
2. **Retention.** A residency assertion is possible without extra
   dependencies: `GHC.Stats` is in `base`, and the test binary can be built
   with `-with-rtsopts=-T`. Build an editor, run 100k snapshot-pushing edits,
   `performMajorGC`, read `gcdetails_live_bytes`, and assert it is under a
   generous ceiling (say 3× the same measurement after 10k edits). Keep the
   ceiling loose — this test is guarding an order of magnitude, not a byte
   count.
   *If* adding `-with-rtsopts=-T` to the test target is unwelcome, the
   fallback is the structural test plus a `Seq.length` assertion at each of
   the six push sites, which is weaker but still catches a regression to the
   lazy idiom.
3. **Behavioural parity.** Existing undo/redo, CSV undo, nav-history and
   find-history tests must pass unchanged; they exercise the pattern matches
   that change shape.

## 6. Risks and notes

- `Seq` pattern synonyms (`:<|`, `Empty`) need `{-# LANGUAGE PatternSynonyms #-}`
  or `ViewPatterns`; the codebase's shared `EXTS` list in the Makefile does not
  include it, so either add `PatternSynonyms` per-module with a pragma (cleanest
  — it stays local) or use `Seq.viewl`.
- `Show` instances: `Document`/`Editor` derive `Show`; `Seq` has one, so this
  is transparent.
- Serialisation boundaries (`saveHistoryFile`, `recentsForPersist`) are the
  only places that need `toList`.
- This is a pure-refactor change; no `Effect`, driver or renderer code is
  touched.

## 7. Follow-on

Once the histories are honestly bounded, the *cost model* of undo becomes
worth revisiting: 1000 full-buffer snapshots is a coarse design (it works only
because `Seq` shares unchanged lines). A future plan could replace snapshots
with an operation log (insert/delete deltas) so undo depth costs O(edit size)
rather than O(log file size) per step — see `0009-undo-as-edit-log.md`.
