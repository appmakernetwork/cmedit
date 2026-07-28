# 0022 — SQLite database browser

**Theme:** capability — the file class nobody's terminal editor opens
**Status:** proposal
**Estimated effort:** 4–6 days
**Risk:** medium (a binary format parsed by hand — but read-only, bounded,
and with the refusal path as the floor, so the failure mode is "shows
nothing", never corruption)

---

## 1. The observation

`.db` / `.sqlite` files are everywhere — application state, browser profiles,
package caches, data science hand-offs — and the editor currently refuses
them as binary. The file format is stable, fully documented, and *designed*
for bounded random access: fixed-size pages, b-trees, a self-describing
schema table on page 1. Every architectural idea needed to read one already
shipped in this codebase:

- **Seeking bounded reads:** `App.readAt` (built for the ZIP central
  directory) — a 10 GB database costs only the pages actually visited.
- **Memory independent of file size:** the pager's philosophy (sparse index +
  bounded window + per-item cap) transfers verbatim, with b-tree pages in
  place of newline-delimited lines.
- **The display surface:** `Cmedit.Csv.CsvView` as a read-only grid — the
  exact mechanism plan `0021` §4.2 builds for XLSX (whichever plan lands
  first builds it; the other reuses it).
- **Read-only by construction:** no serialiser exists or could, the RTF/ZIP
  bargain, so the database can never be damaged by its own view.

## 2. Detection

The 16-byte magic `"SQLite format 3\0"` at offset 0, sniffed in
`classifyFileWith` **before** the size check and binary refusal, exactly like
`zipMagic` and `sniffImage`. `maxOpenBytes` never applies. It arrives as its
own `LoadOutcome` (the PDF pattern: binary, so no buffer, no Alt+T, Save
refused; re-opening switches to the open copy).

## 3. `Cmedit.Sqlite` — a pure page-level parser

A leaf module (like `Zip`: `Cmedit.Width`/`Types` at most). All functions
take page *bytes* and return parsed structure; **no IO** — the driver does
the reads. What it models:

- **The header** (page size, text encoding UTF-8/16le/16be, the change
  counter — see §6, reserved-bytes-per-page which shrinks usable page size).
- **Varints and serial types** — the record format: NULL, ints, floats,
  text, blob. Blobs render as a bounded hex-ish preview, never raw bytes.
- **B-tree pages** (table interior/leaf; index pages ignored in v1): cell
  pointer arrays, interior child/key pairs, leaf rowid+payload cells, and
  **overflow chains** for large payloads — followed but capped at
  `maxCellBytes` (64 KiB, the `maxPagerLine` lesson: without a per-item cap,
  one pathological row un-bounds everything).
- **The schema:** page 1 is itself a table b-tree holding `sqlite_schema` —
  names, types, root pages, and the `CREATE` SQL. Column names come from a
  deliberately tiny parse of `CREATE TABLE`'s parenthesised column list
  (identifiers only, quoting/brackets/backticks handled, constraints
  skipped); fallback is `c0…cN`. This is not a SQL parser and must not grow
  into one.

Everything unmodelled is *skipped*: views and triggers list as schema
entries showing their SQL; index b-trees are never walked in v1; `WITHOUT
ROWID` tables (index-shaped leaves) are listed but open as their DDL with a
"not yet supported" note rather than a wrong guess at their key layout.

## 4. The view

**Table picker first:** opening a database shows its schema — a modal picker
in the `DefPick`/quick-open mould (name, type, row-count *estimate* — counts
are not stored in the format, so show nothing rather than walk a huge tree
at open). Enter opens a table; the picker reopens via the PDF `[`/`]` idiom
or a relabelled Go To ("Go to Table…", the `relabelEntry` trick).

**Table content = the read-only grid** (`CsvView` per `0021` §4.2): frozen
header row of column names, cell selection, copy (a selection copies as
mini-CSV, the existing `copyText` behaviour), both scrollbars (taught in
both places — `scrollBarInfo` *and* `scrollBarTo`, the twice-taught rule).

**Windowed rows, pager-style.** The grid never holds the table; it holds a
window of decoded rows around the viewport plus a **sparse ordinal index**:
one streaming in-order leaf walk records the *rowid* at every
`dbStride` (1000) ordinals — the exact `buildPagerIndex` shape. Seeking to
"row 400 000" is then: nearest checkpoint's rowid → O(log n) b-tree descent
(interior pages carry key ranges, so descent by rowid is cheap even though
descent by ordinal is impossible) → decode forward ≤ stride rows. Every
fill is bounded three ways (stride, window count, `maxCellBytes`) and
round-trips as an effect (`EffDbFill` → `dbFilled`, the `EffPagerFill`
pattern), with the driver-side direct path (`fillPagerNow`'s twin) for
startup/resize/restore. The index build itself is bounded per fill and
continues in the background over `loadQ` for huge tables, with the
scrollbar honestly reflecting "indexed so far" (the pager precedent).

## 5. What is deliberately absent

- **No SQL.** No query box, no filtering, no sorting — v1 is "open the file
  and *see* it", which is the entire gap being filled. A WHERE-less
  reading view has no failure modes; a query engine is nothing but.
- **No index walking, no foreign-key chasing, no BLOB export.**
- **No editing** — structurally impossible (no serialiser), stated in the
  status bar like the other read-only views.

## 6. Concurrency honesty (the one genuinely sharp edge)

Reading pages without SQLite's locking protocol is safe against *stale* data
but not against a writer mid-transaction:

- **WAL databases:** the main file is always a consistent (if stale)
  snapshot — pages only reach it at checkpoint. If a non-empty `-wal` file
  sits next to the database, show a status note ("recent changes in WAL not
  shown") rather than merging WAL frames (a whole second format; explicitly
  out of scope for v1, and the honest note costs one `statEntry`).
- **Rollback-journal databases mid-write:** pages can genuinely tear. The
  defence: re-read the header's **change counter** on every `EffDbFill`; if
  it moved, drop the window and sparse index and rebuild from the new
  snapshot (cheap — the index is sparse), with a "database changed —
  reloaded" status. A torn *individual* read manifests as a cell-parse
  failure, which renders as an error cell, never a crash — every parse in
  `Cmedit.Sqlite` is total.

## 7. Testing

- **Pure:** varint/serial-type/record decoding against hand-built byte
  fixtures; b-tree descent on synthetic trees (built by the test as bytes —
  the format is simple enough to *write* a minimal single-table database in
  the Spec, which is the offline answer to having no `sqlite3` binary);
  overflow-chain reassembly and its cap; the `CREATE TABLE` column-name
  parse over a corpus of quoting styles.
- **Property:** for generated databases, the streamed ordinal walk and the
  checkpoint-descent seek must agree on every row (the invariant the whole
  scrolling model rests on).
- **Real files:** a small corpus checked in (a browser-profile-shaped db, a
  UTF-16 one, a >4 KiB-row overflow case, a WAL pair) with golden schema and
  first/last-row assertions.
- **Integration (PTY):** open, pick a table, page to the bottom, copy a
  selection; assert bounded RSS on a large generated database (the pager's
  measurement discipline: the number goes in this file when taken).
- **`make windows-check`:** `Cmedit.Sqlite` is pure; the driver uses
  `readAt`, already portable.

## 8. Risks

- **Format edge cases** (ancient page sizes, reserved bytes, pointer-map
  pages in auto-vacuum files) — mitigated by parsing *totally* (every
  refusal is a typed error surfacing as the binary-refusal dialog, today's
  behaviour) and by the corpus. Auto-vacuum's ptrmap pages just mean some
  page numbers aren't b-tree pages — the descent never visits them.
- **Effort honesty:** the b-tree + record layer is ~2 days; the grid/picker
  wiring ~1 (less if `0021`'s read-only grid landed); index/seek/refresh
  ~1–2; corpus and tests ~1.
