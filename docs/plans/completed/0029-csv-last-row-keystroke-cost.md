# 0029 — 390 ms per keystroke at the bottom of a CSV: the shape check that asked for a cursor

**Theme:** per-keystroke cost in the table view — the open mystery `0028` left behind
**Status:** ✅ **RESOLVED** — implemented 2026-07-28
**Risk (as shipped):** low — the new cache is a pure function of the grid, checked
after every one of 1 800 random operations against an oracle that shares no code
with it, and both mappings it feeds are checked against the from-scratch
versions they replaced

## Resolved

Typing into the **last** row of the 32 MB / 223 209-row table from `0026`'s
generator cost **390 ms per keystroke in the shipped binary**; at row 0 it cost
1.7 ms. It now costs 1.8 ms. Driven over a PTY against the real `./cmedit`,
30 keystrokes at each position, each run in a throwaway `HOME`:

| cursor (223 209-row × 12-col CSV) | before | after | bytes emitted/key |
|---|---|---|---|
| row 1 (Ctrl+Home) | 2.36 ms (p95 3.1) | **2.37 ms** (p95 4.7) | 11 901 |
| row 110 558 (mid-file) | 195.76 ms (p95 216.7) | **3.39 ms** (p95 23.8) | 11 902 |
| row 223 209 (Ctrl+End) | 386.26 ms (p95 417.6) | **1.80 ms** (p95 2.6) | 1 977 |

The middle row is the shape of the whole defect: the cost was *exactly linear in
how far down the file the cursor was*, and half-way down it was half the price.
It is now position-independent, which is the property being bought.

## The mystery, as `0028` left it

`0028` found this while measuring something else and could not explain it. What
it had established:

* it is **not** the modified flag — the two builds `0028` compared differ in
  exactly that, and both measured 390 ms;
* it is **not** the crash journal — `journal = off` changes nothing;
* it is **not** the emitted bytes — 6 KB per keystroke at the last row against
  33 KB at row 0, i.e. the *slow* case emits the *least*;
* and it is **not reproducible in the pure model** — `update` +
  `refreshHighlight` + `renderEditor` over exactly that state measured **0.6 ms**
  per keystroke in-process.

So the first thing to explain is not the 390 ms. It is the 0.6 ms.

## Diagnosis

### 1. The in-process 0.6 ms was measured on a `Screen` that was never rendered

The probe that produced it ended each iteration with `scrW s`, where `s` is the
`Screen` and `scrW` is its width field. That is `bench/README`'s **trap 2**
exactly: `Screen`'s cells are boxed and lazy, so forcing anything but the cells
renders nothing at all. The probe timed `update` plus the construction of a
thunk.

Re-run with the frame forced the way `bench decomp` and `bench frame` do it —
`renderFrame` against the previous screen, with the resulting `Builder`
serialised, which is what forces the cells — on the same states, cursor and
`csvTop` settled by a real `update`:

| 20 keystrokes, full driver cycle, cells forced | per key |
|---|---|
| row 0 (`csvTop = 1`) | 1.82 ms |
| row 111 604 (`csvTop = 111 561`) | 2.97 ms |
| row 223 208 (`csvTop = 223 165`) | 2.90 ms |
| Ctrl+End's actual state — last row *and* last column | 1.02 ms |

So the corrected in-process number is 1.8–3.0 ms, not 0.6 ms — but the
conclusion `0028` drew from it survives the correction: **the pure model really
does not reproduce the 390 ms.** Rendering and geometry are exonerated with the
cells forced, not merely with them unbuilt. (The last row is checked with the
cursor in the last *column* too, because that is where Ctrl+End actually leaves
it, and a column-dependent cost would have looked identical from the outside.)

That also disposes of the first family of leads. The CSV vertical-geometry
family — `rowHeight`, `csvRowLayout`, `rowAtLineOffset`, `scrollTop`,
`ensureVisible` — was the obvious suspect, since variable row heights make
"sum the heights from row 0" O(rows-above). It is not guilty and never was:
every one of those measures **from `csvTop`**, not from row 0, and the
scrollbar uses `Csv.nRows` (a `Seq.length`) rather than a height sum. That
discipline was already correct and is untouched by this plan.

### 2. Instrumenting the driver puts it in one stage

Temporary `getMonotonicTime` marks around each stage of the `GotKey` branch of
the event loop, and inside `renderNow`, with the editor's stderr redirected to a
file so the PTY screen stays readable. Typing at the last row:

```
STAGE batch=0.01  pager=0.00  persist=388.79  render=1.06  lint=0.00  journal=0.01
  RENDER pre-gfx=0.0  gfx=0.0  mkbuilder=0.0  serialise=0.94  emit=0.01  flush=0.01  bytes=137
```

The whole pure update is 0.01 ms; the entire frame — diff, serialise, write,
flush — is 1 ms. The 390 ms is in `persist`, which is two calls:
`maybePersistRecents` and `maybePersistSession`. Splitting them:

```
STAGE batch=0.01  pager=0.00  recents=0.00  session=392.30  render=1.00  lint=0.00  journal=0.01
```

**`maybePersistSession`, every key batch, 392 ms.** It writes nothing — the
session file's *shape* has not changed since startup. All it does is evaluate
`sessionShape ed` and compare it with the last one.

### 3. What `sessionShape` was actually asking

```haskell
sessionShape ed = (seFolder s, map rePath (seFiles s), seActive s)
  where s = sessionForPersist ed
```

Its own comment says it is "everything the file records **apart from cursor
positions**", and that is exactly what it returns. Reading it, the positions
look free: they are computed inside `sessionForPersist` and then projected away,
and Haskell does not evaluate what nobody looks at.

Except that `seFiles` is a list of `RecentEntry`, and

```haskell
data RecentEntry = RecentEntry { rePath :: !FilePath, reLine :: !Int, reCol :: !Int }
```

has **strict fields**. Forcing `rePath e` — which the driver's `shape /= old`
does, character by character, on every key batch — forces the `RecentEntry`
constructor application, which forces `reLine` and `reCol`, which is
`docCursorPos d`, which for a table document is `Csv.cellTextPos`:

```haskell
cellTextPos v r c =
  let baseLine = r + sum [ nlCount (rowSerial v i) | i <- [0 .. r - 1] ]
```

`rowSerial` builds the serialised text of a whole row — quoting every field,
intercalating the delimiter. At row 223 208 that is 223 208 full row
serialisations, per keystroke, to discover a number that is then thrown away.

Measured directly, in-process, on the same corpus (`bench csv`, both traps
avoided — the result is a *tuple*, so `pure $!` computes neither component, and
the call is loop-invariant, so it floats out of a repeat loop):

| per call, 223 209-row table | before |
|---|---|
| `sessionShape` at the last row | **392.6 ms, 1 651 MB** |
| `sessionShape` at row 0 | ~0 ms, ~0 MB |
| `cellTextPos` at the last row | 372 ms, 1 651 MB |
| `cellTextPos` at the middle | 186 ms, 825 MB |
| `cellTextPos` at row 0 | ~0 ms |
| `textPosCell` at the last line | 373 ms, 1 675 MB |

392.6 ms in-process against 386–390 ms through the PTY. That is the whole of it,
and it is why the "not the emitted bytes" observation was a red herring twice
over: the expensive keystroke emits *less* because the cursor is on the bottom
row of a settled screen, and the cost has nothing to do with drawing.

### Two faults, and each hides the other

Neither half is wrong on its own terms, which is why this survived two plans:

1. **`sessionShape` asks for data it discards.** It is entitled to assume
   laziness will collect the difference, and a strict field three modules away
   is what defeats it. This is `0028`'s lesson in a second dress — that plan's
   headline was that a *cost* argument resting on `ptrEq` firing must be
   measured, and this one is that a cost argument resting on **laziness** must
   be measured too. Reading either function tells you nothing.
2. **`cellTextPos` is O(rows above the cursor) and allocates a serialisation of
   all of them.** That is defensible for a question asked once per document
   event — and it is asked by the recents, the session file, the crash journal's
   `jCursor` and the nav history — but it is a 390 ms landmine sitting where any
   future per-batch caller would step on it. Fix only fault 1 and the landmine
   stays armed for exit, for every file switch, and for every journal
   write-behind pass.

Both are fixed.

## The fix

### `sessionShape` computes the shape

`sessionForPersist` and `sessionShape` now share `sessionDocs`, which yields
`(zipper index, path, Document)` — the *documents*, not finished
`RecentEntry`s — so which documents are recorded cannot drift between the two
while only one of them ever asks a `Document` for a cursor position.
`sessionActiveIx` is likewise shared. `sessionShape` touches folder, paths and
the active index and nothing else.

### `csvNl`: the sparse map of rows that carry embedded newlines

`CsvView` gains a third cache on the discipline `csvWidths` established in
`0016` and `csvDirty` inherited in `0028`:

```haskell
csvNl :: !(Map Int Int)   -- row index -> newlines that row contributes; absent = 0
```

The invariant is an equality: for every row index `i`,
`Map.findWithDefault 0 i (csvNl v)` is the number of newlines in row `i`'s
serialised form. That holds because **serialisation cannot change a newline
count** — `quoteField` only wraps a field in quotes and doubles the quotes
inside it, and the joins contribute one delimiter each (which is a newline only
in a degenerate case `rowNl` accounts for explicitly). So the map can be
maintained from the *cells*, without ever building the text.

`linesBefore v r` is then `Map.split` plus a fold over the left part:
**O(log rows + multi-line rows above r)**, and O(log rows) flat for a table with
no embedded newlines at all — which is most tables. The benchmark corpus is
deliberately not one of those: it has 446 multi-line rows out of 223 209
(892 extra lines), and summing 446 `Int`s is 0.005 ms.

**Sparse, and that is the design.** A dense per-row array or a prefix-sum tree
would both be exact, and both would cost per row rather than per *interesting*
row. The map's size is the number of rows a human put a newline inside, which
does not grow with the table.

Three producers, and nothing else may construct one:

| | when | cost |
|---|---|---|
| `withCell` | one cell written (the typing path) | **O(log rows)** — the row's count moves by `- nlCount old + nlCount new`, looking at nothing outside the row |
| `syncNl` | a grid change (`withRows`, undo, redo) | O(rows) pointer hops + the changed rows — the twin of `syncWidths`/`syncDirty` |
| `computeNl` | a grid arrives wholesale (`mkCsvGrid`), or a row count changes | O(grid) |

Set in exactly the five places `csvWidths` is set: `mkCsvGrid`, `withCell`,
`withRows`, `undo`, `redo`. `rebaseHistory` deliberately sets neither, and for
the same reason: it keeps `new`'s grid, and both caches are functions of the
grid alone, so `new` already built them correctly. Every mutating operation in
the module funnels through `withCell` or `withRows`, so `pasteClip`,
`sortByColumn`, `clearSelCells`, `mapCells` and the row/column inserts and
deletes need no code of their own.

**Why a row count change recomputes.** Inserting or deleting a row shifts every
later index, so an index-keyed map cannot be carried across it — the same
argument `0028` makes for `DirtyShape`. `syncNl` answers a differing row count
with a full recompute rather than a fix-up, and `withCell` does the same in the
one case where `ensureCell` had to pad.

### Both mappings read it

`cellTextPos` (cell → text position) replaces its walk with `linesBefore`.

`textPosCell` (text position → cell), its inverse, used to `scanl` down the
file serialising each row until it passed the target line. Row `i` starts at
line `i + linesBefore v i`, which is strictly increasing in `i`, so it is now a
**binary search** — O(log rows) probes, each O(log rows + multi-line rows
above). The column half of it is unchanged, including its pre-existing
approximation for rows that contain a newline; the fuzz test pins the new search
against the old scan rather than against an idealisation of it.

`rowSerial` — the "serialise a whole row" helper both mappings used to walk
through — is gone, because the only thing either needed from those rows was how
many lines each takes up.

### What was deliberately not touched

The CSV **display** geometry: `rowHeight`, `csvRowLayout`, `rowAtLineOffset`,
`scrollTop`, `ensureVisible`, `csvGutterWidthFor`, `scrollBarInfo`. Those are
the functions whose failure mode is a table that scrolls wrong or a mouse click
that lands on the wrong row, they were already measured from `csvTop` rather
than from row 0, and this plan does not go near them. `csvNl` addresses the
*serialised file*, which is a different coordinate system from the screen and is
never used to draw anything.

## The measurements

`bench csv` on `docs/plans/bench/gen_csv.py`'s 32 MB corpus (223 209 rows × 12
columns, quoted fields, embedded delimiters, doubled quotes, multi-line cells,
ragged rows), `-O2` with `-T`, before and after built from the same tree with
only this change reverted. Five probes were added and are kept:

| per call unless noted | Before | After |
|---|---|---|
| **`sessionShape` at the LAST row** (the per-keystroke path) | **392.6 ms, 1 651 MB** | **0 ms, 0 MB** |
| `sessionShape` at row 0 | 0 ms, 0 MB | 0 ms, 0 MB |
| **`cellTextPos` at the LAST row** | **372 ms, 1 651 MB** | **0 ms, 0 MB** |
| `cellTextPos` at the middle | 186 ms, 825 MB | 0 ms, 0 MB |
| `cellTextPos` at row 0 | 0 ms | 0 ms |
| **`textPosCell` at the LAST line** | **373 ms, 1 675 MB** | **0 ms, 0 MB** |
| `textPosCell` at the middle | 186 ms, 838 MB | 0 ms, 0 MB |
| `computeNl` over the whole grid (new; load path only) | — | 126 ms, 64 MB |
| `mkCsvLines` — the editor's own load path | 584–599 ms, 852 MB | 749–760 ms, 919 MB |
| `mkCsvView` (whole-text load path) | 827–832 ms, 903 MB | 948–997 ms, 971 MB |
| one undo/redo pair | 39.6 ms, 92 MB | 43.6 ms, 121 MB |
| 100 cell edits at the last row, `isModified` included | 0 ms, 0 MB | 0 ms, 0 MB |
| `columnWidths`, `isModified`, the `0028` probes | unchanged | unchanged |
| live heap / RSS with the table open (`bench csvlive … lines`) | 192 MB / 417 MB | 192 MB / 417 MB |

The `sessionShape` probe is forced the way the driver forces it — by walking
the paths, which is what `shape /= old` does. `length ps` alone forces only the
list spine and leaves every path a thunk, and the entire defect is one level
further in; the first version of that probe reported 0 ms for the slow case and
222 ms for the fast one, which is trap 2 for the third time in three plans.

### The two costs this buys it with, measured and argued

**Opening a big table is ~28 % slower: `mkCsvLines` 590 ms → 755 ms.**
`computeNl` is one extra pass over every cell of the grid (126 ms, 64 MB on
32 MB of CSV). It is not reducible by counting differently — `T.count`,
`T.foldl'`, `T.isInfixOf`-then-count and `T.split` were all measured on the full
corpus and land within 12 % of each other (152 / 141 / 135 / 136 ms), because
the cost is touching 32 MB of text and 2.7 million `Seq` elements, not the
counting. `T.count` allocates least, so it is what ships. The only way to make
it free is to fuse it into a pass that already exists (see below). The trade is
a one-off 165 ms at open against 390 ms on every keystroke, and for any table
small enough to edit interactively both numbers are invisible.

**An undo/redo pair is ~10 % slower: 39.6 ms → 43.6 ms, 92 MB → 121 MB.**
`syncNl` walks the two grids beside `syncWidths` and `syncDirty` (1.9 ms and
14 MB per call). This is the same trade `0028` made for `syncDirty` and it
compounds with it: there are now three pointer-diff walks over the same two
grids where `0016` had one. Undo is not a per-keystroke path. Fusing all three
into one walk is the standing prize, and is still not taken here for `0028`'s
reason.

## Guards (31 new checks, suite 3 140 → 3 171)

**The fuzz test is the guard, extended a third time.** The 600-operation script
that already reaches every mutating function in `Cmedit.Csv` now asserts three
more things after **every** step, tracked separately so a failure names itself:

* `csvNl v == computeNl (csvDelim v) (csvRows v)` — the incremental state
  against the module's own from-scratch producer;
* `csvNl v == nlRef v`, where `nlRef` is computed **in the test** out of
  `csvToText` alone — one row through `mkCsvGrid`/`csvToText` *is* that row's
  serialised form, so the oracle shares no code with the thing it checks;
* `cellTextPos` and `textPosCell` against `cellTextPosRef` and
  `textPosCellRef`, which are the pre-cache implementations written out again in
  the test over that same oracle, sampled at both ends of the grid and at a
  seeded row/line (the references are O(rows) per call and the fuzz grid grows).
  `textPosCellRef` takes its fields from the serialiser rather than by splitting
  a serialised row on the delimiter — a quoted field may contain one, and the
  first version of that reference was wrong for exactly that reason.

A **third** 600-operation run was added over a table that *starts* full of
multi-line cells, so the map is non-empty from the first operation rather than
only once the random script happens to type a newline. 1 800 operations in all.

Plus hand-pinned cases, each a way for the map to be wrong while still looking
plausible: an ordinary table's map is *empty* (not zero-valued); the rows with
newlines and only those appear; `linesBefore` is the running total and is 0
above row 0; `cellTextPos` of each row of a grid with a two-line and a
three-line cell; a field *after* a multi-line cell lands on that cell's last
line; typing a newline adds the row and moves the row below it down; removing it
again **empties** the map rather than leaving a zero; two newlines in one cell
count twice; row insert/delete above shifts the entries and back; deleting the
multi-line row drops its entry; deleting a column drops what that column
contributed; undo and redo restore the map; a sort moves the entries with their
rows; a paste that grows the grid recomputes; `mkCsvGrid` builds it for a grid
handed in by a non-CSV producer (`Xlsx`/`Odf`); and a tab-delimited table counts
only the cells' own newlines. `cellTextPos` and `textPosCell` are additionally
checked *exhaustively* — every cell, and every line from off one end to off the
other — on the multi-line grid.

**And the per-keystroke path is pinned with a bomb.** Nothing structural can
observe "`sessionShape` does not force a cursor position": both fixes are
individually sufficient for the *timing*, so a timing test would pass with
either one reverted. What can observe it is a grid whose rows are `error` — a
`Seq` is spine-strict but element-**lazy**, so such a view is perfectly
well-formed until something reaches for a row. The test asserts that
`sessionShape` returns the paths, and that `sessionForPersist` on the same
editor throws, so the bomb is proved armed rather than merely unexercised.

Every new guard was checked to **fail** when its fix is reverted, one sabotage
at a time:

| sabotage | fails |
|---|---|
| `sessionShape` back to projecting `sessionForPersist` | the bomb test |
| `withCell` stops updating `csvNl` | all three fuzz runs + 2 pinned cases |
| `undo`/`redo` stop carrying it | all three fuzz runs + "undo restores the map" |
| `withRows` stops carrying it | all three fuzz runs + 5 pinned cases |
| the binary search's `<=` weakened to `<` | all three fuzz runs + the exhaustive `textPosCell` check |

`make test` 3 171 passing, `make windows-check` clean,
`docs/plans/bench/pty_journal.py` 4/4 and `docs/plans/bench/pty_session.py` 5/5
(the second is the one that matters here — `sessionShape` decides when the
session file is rewritten, and its four scenarios pin both persist paths).

Behavioural cover for the two mappings beyond the unit tests: the PTY probe's
mid-file target reaches row 110 558 by toggling **Alt+T** out to the markup
view, using Ctrl+G to reach *line* 111 000, and toggling back — which exercises
`cellTextPos` and `textPosCell` end to end through the real binary. The 442-line
discrepancy between the two numbers is the embedded newlines above that row,
independently derived.

## Found along the way, not fixed here

**Ctrl+G does nothing in the CSV table view, which still advertises it.**
Reaching a mid-file row for the measurements above turned out to need a detour
through Alt+T, because Ctrl+G in a table opens no dialog: `handleCsvNav` has no
`KCtrlChar 'g'` case and falls through to its catch-all — even though the
read-only *sheet* view next door binds it (`handleSheetKey`, reinterpreted as
"Go to Sheet"), as do the text, PDF, EPUB and workbook views. Confirmed through
the PTY: the key produces no dialog and no cursor movement, while the Find menu
carries on showing "Go to Line…  Ctrl+G". And the menu item is not a working
substitute either — with `edCsv` set, `EditorDoc.gotoLine` falls through to its
final clause, which moves `edCursor` in the line buffer that the table view is
not displaying. Noted rather than fixed: "Go to row" in a table wants a decision
about what the number means for a grid with a frozen header and multi-line rows,
and this plan is about a cost.

## What was tried and not taken

**Fixing only `sessionShape`.** It is enough for the headline number — the PTY
median at the last row goes to 1.8 ms either way — and it leaves `cellTextPos`
as a 390 ms O(rows) walk reachable from the exit path, from every file switch
that changes the session's shape, from every recents rewrite and from the crash
journal's `jCursor` on each write-behind pass. A per-keystroke defect that took
two plans to find is not a good reason to leave the mechanism that made it
possible in place.

**A dense prefix-sum structure** (a Fenwick tree, or a checkpoint array of
partial sums every K rows). Exact and O(log rows) in the worst case, where the
sparse map is O(multi-line rows above). But the worst case is a table in which
*every* row contains an embedded newline, and there the map is 223 209 `Int`
additions — about 1–2 ms, still 200× better than the walk — while the common
case is a table with none, where the map is empty and the answer is zero without
looking at anything. `containers` also gives the map for free where a Fenwick
tree would be hand-rolled state with its own invariant.

**Snapshotting `csvNl` with each undo entry.** This would remove the undo/redo
regression entirely, and — unlike the same idea for `csvDirty`, which `0028`
rejected — it is *sound*: `csvNl` is a function of `csvRows` alone, with no
moving baseline like `csvSaved` to invalidate it, so a snapshot stored beside a
grid is unconditionally valid. Not taken because it changes the type of the undo
history (`Seq Grid` → `Seq (Grid, Map Int Int)`), which is load-bearing state in
the module whose failure mode is losing a user's edits, to save 4 ms on an
operation nobody performs at typing speed.

**Fusing `computeNl` into `computeWidths`.** This would make the cache free at
load: `cellWidth` already visits every character of every cell and already tests
`c == '\n'` (that is how it finds a multi-line cell's widest line), so the
newline counts are sitting in a pass that already happens. Deliberately out of
scope for the reason `0028` gave when it declined to fuse `syncWidths` and
`syncDirty`: `computeWidths` is `0016`'s most invariant-critical function, and a
wrong width is a visibly broken table rather than a slow one. This plan should
not be the thing that rewrites it. The prize is real and is now worth more than
it was — one fused walk would pay for `syncWidths`, `syncDirty` *and* `syncNl`
at once, on load and on every structural edit.

**Making `csvNl` a lazy field**, so the load pays nothing. The first keystroke
then pays the whole 126 ms instead, because `withCell` must read the map to
update it — and a chain of unforced `syncNl` thunks is exactly the retention
pattern `0001` and `0014` were about. Strict at load is the honest version.
