# 0016 — CSV table mode: O(1) cell edits and a Text-based parser

**Theme:** interactive latency on large tables; open-time cost
**Status:** ✅ **RESOLVED** — implemented 2026-07-26
**Risk (as shipped):** low — landed behind the extended `csvWidths` fuzz test
and a parser oracle

## Resolved

Three changes, on a 32 MB / 300 001-row × 12-column table:

| | Before | After |
|---|---|---|
| One keystroke in a cell (`editInsert`) | **7.4 ms, 22 MB** | **~0 ms, ~0 MB** |
| `commitEdit` (Tab between cells) | 7.4 ms, 22 MB | ~0 ms |
| `csvParse` alone | ~4 300 ms, 8 813 MB | **520 ms, 763 MB** |
| `mkCsvView` (parse + width cache) | 4 340 ms, 8 813 MB | **1 077 ms, 3 863 MB** |
| Live heap after parse | 289 MB | 221 MB |
| RSS of the real editor with the file open | 2 506 MB | **640 MB** |

1. **`Csv.withCell`** — single-cell writes tell the width cache which cell
   changed instead of making `syncWidths` walk every row to discover it. O(1)
   when the cell widened or was not its column's widest; only a genuine shrink
   of the widest cell costs a column rescan. `putCellCursor`, `beginEditFresh`,
   `cancelEdit`, `clearCell` and `setCurrentCell` route through it; structural
   edits keep `withRows`.
2. **A `Text` parser.** The unquoted field — the overwhelming majority — is one
   `T.break` yielding a slice with no copy; only fields containing quotes are
   rebuilt. All quirks preserved: doubled `""`, CR/CRLF/LF records, ragged rows.
3. **`computeWidths` accumulates into an unboxed array** instead of doing a
   `Seq.adjust'` per cell (3.6 M spine rebuilds on load).

### A latent bug found and fixed

The old parser *reversed the field* when stray text followed a closing quote:
`"stray"tail,b` parsed as `tyartsail`. The new one appends in order
(`straytail`). This is why the oracle test excludes that path — agreeing with
the old behaviour there would have been wrong. Both cases are now pinned
explicitly.

### Guards (suite 2151 → 2208)

- The `csvWidths` fuzz test gained three operations — typing several characters
  into a cell without committing, backspacing a cell that may be its column's
  widest (the one O(rows) branch), and cancelling an edit — and runs 600 random
  operations instead of 250, asserting after **every** step that the cache
  equals a fresh recomputation and that the pointer-accelerated modified flag
  equals plain equality.
- A 26-case parser corpus compared against the previous `String` implementation,
  kept verbatim in the suite as an oracle, plus other delimiters and the two
  quirk cases pinned by hand, plus serialisation stability.

**Not taken:** moving the CSV parse onto the async load thread (§3). At 1.1 s
for 32 MB it is no longer the dominant open cost, and the `OutCsv` outcome it
needs would add a branch to every load path. Worth revisiting only if someone
routinely opens tables larger than this.

The plan below is the original analysis, kept for the record.

---

---

## 1. Measurements

A 32 MB CSV, 300 001 rows × 12 columns, through the real `Cmedit.Csv` API:

| Operation | Time | Allocated |
|---|---|---|
| `mkCsvView` (parse + build the width cache) | **4 340 ms** | **8 813 MB** |
| live heap after parse | — | 289 MB (9× the file) |
| **one `editInsert` keystroke inside a cell** | **7.4 ms** | **22 MB** |
| `commitEdit` (Tab to the next cell) | 7.4 ms | 22 MB |
| `isModified` (runs every keystroke) | 0.04 ms | 0.2 MB |
| `columnWidths` (runs every repaint) | ~0 ms | ~0 MB |

Two of these are healthy and two are not.

**Healthy:** `isModified` and `columnWidths`. The pointer-shortcut design in
`sameGrid` and the width cache both do exactly what their comments claim.

**Not healthy:** typing one character into a cell of a large table costs
7.4 ms and 22 MB — before any rendering. At 300k rows that is a perceptible
lag on every keystroke and ~330 MB/s of garbage while typing. And opening the
file takes 4.3 seconds during which the editor is blocked (CSV parsing happens
on the load path).

**And the real editor is worse than the harness suggests.** Driving the actual
`./cmedit` binary through a PTY (50×200), opening the same 32 MB CSV and then
sitting idle:

```
big.csv  RSS MB over time: 1s:701  3s:2503  5s:2504  …  25s:2504
big.txt  RSS MB over time: 1s:163  3s:163   5s:163   …  25s:163
```

**2 504 MB of resident memory for a 32 MB file — and it never comes back**,
because once the editor is idle it stops allocating, so the RTS has no reason
to run another major GC and never returns the pages. (The live set after the
parse is only ~289 MB; the rest is heap the collector has released internally
but not to the OS. See `0007` for that half of the problem.) A 100 MB CSV —
well inside `maxOpenBytes` — would need something like 8 GB and would be an
OOM on most machines. The equivalent 49 MB *text* file sits at a stable
163 MB.

## 2. Root cause 1 — `syncWidths` walks every row on every cell edit

`editInsert` → `putCellCursor` → `withRows (setCell r c t)` → `syncWidths`:

```haskell
syncWidths old ws new
  | ptrEq old new = ws
  | Seq.length old /= Seq.length new = computeWidths new
  | otherwise = case rowsDiff (toList old) (toList new) [] of …
```

`rowsDiff` walks **all** rows (with a cheap `ptrEq` per row, plus the `toList`
allocation) to *discover* which cell changed. The caller already knows: it is
`(csvCurRow v, csvCurCol v)`.

The design comment is honest about this — "a cell edit costs O(rows) pointer
hops plus the one changed cell" — it was simply never measured at 300k rows,
where O(rows) pointer hops is 7 ms.

### Fix: tell `withRows` what changed

Add a targeted sibling and use it from every single-cell path:

```haskell
-- | Replace one cell, updating the width cache in O(1) for the common case.
-- The general 'withRows' (which discovers the change by diffing) stays for
-- structural edits — inserts, deletes, sorts, paste, mapCells.
withCell :: Int -> Int -> Text -> CsvView -> CsvView
withCell r c t v =
  let old  = cellAt r c v
      rows' = setCell r c t (csvRows v)
      wNew = clampW (cellWidth t)
      wOld = clampW (cellWidth old)
      cur  = Seq.index (csvWidths v) c
      ws'  | c >= Seq.length (csvWidths v) = computeWidths rows'   -- new column
           | wNew >= cur     = Seq.update c wNew (csvWidths v)     -- widened: O(1)
           | wOld <  cur     = csvWidths v                         -- wasn't the widest: O(1)
           | otherwise       = Seq.update c (colWidth rows' c) (csvWidths v)  -- may have shrunk: O(rows), rare
  in v { csvRows = rows', csvWidths = ws' }
```

Only the last branch is O(rows), and it fires only when the edited cell *was*
the column's widest **and** got narrower — i.e. essentially never while typing
(typing widens). `colWidth` already exists for exactly this case.

Call sites to convert: `putCellCursor`, `beginEditFresh`, `cancelEdit`,
`clearCell`, `setCurrentCell`. `setCells`/`mapCells`/row & column
insert/delete/sort keep the general `withRows`, where a full diff or
recomputation is the right answer.

Expected: 7.4 ms → single-digit microseconds per keystroke, and 22 MB → ~0.

## 3. Root cause 2 — `csvParse` runs on `String`

```haskell
csvParse delim = Seq.fromList . map Seq.fromList . rows . T.unpack
```

The whole file is unpacked to a `String` (24 bytes per character), each field
is accumulated as a reversed `[Char]` and then `T.pack`ed. That is the 8.8 GB
of allocation and most of the 4.3 s.

### Fix: a `Text` parser with a fast path

The overwhelming majority of CSV fields are unquoted and contain no delimiter,
so the fast path is a single `T.break`:

```haskell
-- Unquoted field: everything up to the next delimiter / CR / LF.
field t
  | Just ('"', rest) <- T.uncons t = quotedField rest
  | otherwise =
      let (val, rest) = T.break (\c -> c == delim || c == '\n' || c == '\r') t
      in (val, rest, terminatorOf rest)
```

`T.break` is a slice — no copy — so an unquoted field costs nothing but the
scan, and the resulting cells share the file's array (which is the same
sharing the plain-text buffer already relies on; see `0014` for when that
matters).

The quoted path (`""` escapes) still needs to build, but only for fields that
actually contain quotes: accumulate with `T.breakOn "\""` segments and
`T.concat` the pieces, rather than character-by-character.

Expected: an order of magnitude on both time and allocation — which is what
brings the 2.5 GB peak down, since the peak is driven by the `String`
intermediates, not by the final grid — and a correspondingly lower live heap (289 MB for a 32 MB file is largely the
`String`-era per-cell `Text` objects; slices would cut the per-cell cost to a
4-word header).

A second, independent win: `mkCsvView` currently runs on the **load path**, so
a large CSV blocks the UI. Once parsing is fast this matters less, but the
existing async-load machinery (`asyncThresholdBytes`, `beginLoading`/`loadQ`)
should carry the CSV parse too — the outcome type just needs a
`OutCsv path grid` variant, which is a small addition to `classifyFile`.

## 4. Testing

- **The existing `csvWidths` fuzz test is the key guard** — extend it so the
  random operation script includes single-cell edits through the new
  `withCell` path, and assert after every step that `csvWidths` equals
  `computeWidths (csvRows v)`. That is precisely the invariant this plan risks.
- **Parser equivalence.** A corpus test asserting the new `Text` parser
  produces a grid identical to the old `String` one, over: quoted fields with
  embedded delimiters, newlines and doubled quotes; stray text after a closing
  quote (the current parser tolerates it — preserve that); CRLF and lone CR;
  empty fields; a trailing newline vs none; a file with no trailing newline in
  the middle of a quoted field. Keep the old implementation in the test as the
  oracle for one release.
- **Round trip.** `csvToText . csvParse` must be a fixpoint on already-normalised
  input, and `syncCsvToBuffer` → save must produce the same bytes as today for
  the corpus.
- **Performance guard.** Assert 1 000 `editInsert`s on a 100 000-row table
  complete in well under a second (today: ~7 s).

## 5. Risks

- The width cache is load-bearing for rendering and for mouse hit-testing
  (`csvColStartX`, `csvBorderColAt`). A wrong width is a visibly broken table,
  not a subtle slowdown — hence the invariant test above being non-negotiable.
- `T.break`-based cells share the source array. For a CSV that is opened,
  heavily edited and left open, that is fine (same as the text buffer). It does
  mean `0014`'s "copy at escape boundaries" rule applies to CSV copy/cut
  (`copyText` → clipboard) as well.
- Tolerance quirks of the current parser (stray characters after a closing
  quote) are behaviour users may depend on; the corpus test is what preserves
  them.
