# CMeDit improvement plans

Work-plan proposals for performance, long-session stability and capability.
Each file is self-contained: problem, evidence, design, implementation steps,
test plan, risks.

Written 2026-07-25 after a read of the driver (`Cmedit.App`), the pure model
(`EditorState` → `EditorEdit` → `EditorDoc` → `EditorFind` → `Editor`), the
renderer, `TextBuffer`, `Syntax`, `Csv` and `Search`, together with a
purpose-built benchmark harness compiled against `src/` at `-O2`
([`bench/Bench.hs`](bench/Bench.hs) — see [`bench/README.md`](bench/README.md)
for how to build it, which mode produced which number, and the two
laziness traps that make naive measurements of this codebase wrong in
*both* directions).

## Status — 17 of 20 closed (v0.4.1)

**Every confirmed defect is fixed**, every non-capability plan is closed —
either implemented, or closed on a measurement showing it was unnecessary — and
the huge-file paged view (`0012`) has landed. Two capability plans remain, plus
one partially-landed renderer plan.

| | Before | After |
|---|---|---|
| Live heap after 200 000 edits | 51 MB, growing linearly | **flat** |
| Per keystroke, 3 000-char `.js` lines | 452 ms | **28.9 ms** |
| Per keystroke, 3 000-char plain lines | 7.9 ms | **2.2 ms** |
| RSS after opening *and closing* a 32 MB CSV | 2 507 MB, permanent | **33 MB** |
| Heap held by copying one line out of a 49 MB file | 49 MB | **~0** |
| Saving a 49 MB file | 117 ms, 440 MB allocated | **35 ms, 67 MB** |
| One keystroke in a 300 001-row CSV cell | 7.4 ms, 22 MB | **~0** |
| Opening that CSV | 4 340 ms, 8.8 GB allocated | **1 077 ms, 3.9 GB** |
| A 4 MiB paste | 592 ms, 1 142 MB | **8 ms, 22 MB** |
| `scaleRGBA` per image placement | 66 ms, 63 MB | **17 ms, 21 MB** |
| Concurrent linter processes while typing | 5 | **1** |
| Opening a 281 MB log (4 M lines) | refused | **opens, 32 MB RSS** |
| Opening a 120 MB single-line file | refused | **opens, 30 MB RSS** |

Verification: `make test` 1 138 → **2 214 passing**, `make soak` (new) flat on
all three axes, `make windows-check` clean. Every guard was checked to *fail*
when its fix is reverted.

Three plans were closed **without** implementing them, because measurement said
not to: `0009` (undo memory is flat, so an edit log buys nothing), `0008`
(grid snapshots cost ~0 thanks to `Seq` sharing) and `0019` (both proposed
search optimisations measured *worse*). Those write-ups are the most useful
thing in this directory for anyone tempted to retry them.

## Read this first: what the measurements actually showed

Six things are genuinely broken (four now fixed), several things that looked
broken are fine, and the rest is design work. Everything below was measured,
not inferred.

**Broken 1 (✅ fixed — `0001`) — history stacks never release memory.** Every bounded history
(`edUndo`, `edRedo`, `csvUndo`, `csvRedo`, nav stops, find history) is bounded
with a *lazy* `take`, which retains everything it promises to drop. Measured
live heap after N snapshot-pushing edits on a small file: 2 MB (5k edits),
5 MB (20k), 21 MB (80k), 51 MB (200k) — linear, where the cap should have made
it flat. → **`0001`**, and **`0008`** for the CSV variant where each entry is a
whole grid.

**Broken 2 (✅ fixed — `0002`) — the syntax lexer is quadratic in line length.** `lexWith` calls
`length` on the remaining line once per token. `lexLine` on 8 000 characters:
81 ms. A full frame over 3 000-character lines: **819 ms**. A three-line fix
(prototyped and measured) takes that frame to 56 ms and the 8 000-char lex to
2 ms. → **`0002`**.

**Broken 3 (found later, same class as 1) — saving allocates 9× the file
size**, because the whole file is materialised as a `Text`, then a
`ByteString`, then copied again for the BOM: 440 MB of allocation to write a
49 MB file. → **`0013`**.

**Broken 4 (✅ fixed — `0014`) — a `Text` slice pins the array it was cut from.** Ten lines
(680 characters) retained out of a 49 MB file keep **49 MB** live. The
clipboard, the find/replace terms and the persisted search history all hold
uncopied slices, so copying one line out of a big file and closing it retains
the file for the session. `Cmedit.Search` already guards against exactly this
for snippets — the guard just is not applied at the other boundaries.
→ **`0014`**.

**Broken 5 (✅ memory half fixed — `0007`; the CSV parse/edit cost remains — `0016`) — opening a 32 MB CSV takes 2.5 GB of RSS, permanently — even after
you close it.** Measured on the real binary through a PTY: RSS climbs to
2 504 MB, and is still 2 504 MB after Ctrl+W closes the document and 2 511 MB
ten seconds later. An idle editor never allocates, so it never collects, so the
RTS never hands the pages back. **A long session's RSS is therefore the
high-water mark of everything it has ever opened, and only ratchets upward.** The cause is `csvParse` running
on a `String` (4.3 s and 8.8 GB of allocation for the parse); the reason it is
*permanent* — and survives closing the file — is the missing idle collection
(`0007` §2b: one `performMajorGC` after 30 s of no input). Also, one keystroke in a large CSV
cell costs 7.4 ms and 22 MB:
`syncWidths` walks every row of the table on every cell edit to *discover*
which cell changed, when the caller already knows.
→ **`0016`** for both, and **`0007` §2b** for the memory-return half.

**Broken 6 — bracketed paste allocates 279 bytes per pasted byte**, with no
size cap at all: a 4 MiB paste costs 592 ms and 1.1 GB. The payload is built
as a `[Word8]` with a six-element list comparison per byte. → **`0017`**.

**Broken 7 — opening a 603 KiB JPEG freezes the editor for over a second.**
The async-load threshold is 2 MiB of *file*, but an image's cost is its pixel
count: this one decodes in 697 ms and its first scale takes another 578 ms, all
on the main thread with no spinner. `scaleRGBA` also builds a cons cell per
output byte (63 MB, 66 ms per call) and runs on every resize, zoom and sixel
animation frame. → **`0018`**.

**And the two findings that are not defects but shape the priorities:**

**The pure editing model is fast and file-size independent.** `update` costs
6 µs per keystroke on a 1 000-line file and 46 µs on a 400 000-line one (that
is `Seq`'s O(log n), not a scan); the whole driver cycle is 2.23 ms/keystroke
on a 200 000-line file vs 2.21 ms on a 5 000-line one. The `Seq`-of-`Text`
buffer with its incremental `bufChars` and pointer-accelerated comparison is a
good design and is not the bottleneck.

**The renderer is essentially the entire per-keystroke cost** — 2.27 ms and
6 MB allocated per frame at 50×200 — and part of it is proportional to whole
line lengths rather than to the visible window. → **`0003`**.

**Status convention:** plans live in `incomplete/` until they are implemented,
tested and verified, then move to `completed/` with a "Resolved" header
recording what shipped and the before/after measurement.

## The plans

| # | Title | Theme | Effort | Priority |
|---|---|---|---|---|
| [0001](completed/0001-strict-bounded-history-stacks.md) | Strict bounded history stacks | memory leak | ✅ **DONE** | — |
| [0002](completed/0002-quadratic-lexer-line-cost.md) | Remove the quadratic in the lexer's step driver | latency | ✅ **DONE** | — |
| [0003](incomplete/0003-frame-cost-and-per-line-work.md) | Bound per-line frame work to the visible window | latency + allocation | 🟡 **Stage 1 done**; Stage 3 reverted (regressed large files) | 1–2 days |
| [0004](completed/0004-lint-single-flight-and-process-supervision.md) | Single-flight linting; stop process pile-up | resource stability | ✅ **DONE** (group-kill rejected) | — |
| [0005](completed/0005-long-session-soak-harness.md) | A long-session soak harness | proving stability | ✅ **DONE** (in-process; PTY soak deferred) | — |
| [0006](completed/0006-session-observability.md) | Session observability (`debug-stats`, `--stats-on-exit`) | diagnosis | ✅ **DONE** (log mode + Settings row dropped) | — |
| [0007](completed/0007-rts-and-gc-tuning.md) | Idle collection so memory returns to the OS; `-rtsopts`/`-T` | memory held | ✅ **DONE** (nursery tuning dropped; capabilities → `0005`) | — |
| [0008](completed/0008-csv-table-stability-at-scale.md) | CSV table memory and cost at spreadsheet scale | memory | ✅ **CLOSED** — retention fixed via `0001`; rest measured unnecessary | — |
| [0009](completed/0009-undo-as-edit-log.md) | Undo as an edit log instead of snapshots | memory (design change) | ⛔ **CLOSED** — its own gate not met (undo memory is flat) | — |
| [0010](completed/0010-background-work-supervision.md) | Supervising background threads and cancellation | resource stability | ✅ **DONE** (load-outcome gen → noted) | — |
| [0011](incomplete/0011-crash-safe-journal-and-session-restore.md) | Crash-safe edit journal and session restore | capability | 3–4 days | medium |
| [0012](completed/0012-huge-file-read-only-paging.md) | Huge files: paged read-only view | capability | ✅ **DONE** (in-file search deferred) | — |
| [0013](completed/0013-streaming-save-and-lower-copy-load.md) | Streaming save; lower-copy load | memory spike | ✅ **DONE** (per-line decode → `0005`) | — |
| [0014](completed/0014-text-slice-pinning.md) | Text slice pinning at escape boundaries | memory leak | ✅ **DONE** (§4.1; compaction deferred to `0005`) | — |
| [0015](incomplete/0015-incremental-workspace-index.md) | Incremental workspace index (quick open, definitions, symbols) | capability + amortised perf | 3–4 days | medium |
| [0016](completed/0016-csv-parse-and-edit-cost.md) | CSV: O(1) cell edits, Text-based parser | latency | ✅ **DONE** | — |
| [0017](completed/0017-bracketed-paste-throughput.md) | Bracketed paste: bulk reads and a cap | burst stability | ✅ **DONE** | — |
| [0018](completed/0018-image-pipeline-cost.md) | Image decode off the main thread; list-free scaler | UI stall | ✅ **DONE** (decoders left, §2.4) | — |
| [0019](completed/0019-case-insensitive-search-throughput.md) | Case-insensitive search: stop lowercasing every line | throughput | ⛔ **CLOSED** — both fixes measured worse; tests + notes kept | — |
| [0020](completed/0020-performance-invariants-in-the-docs.md) | Codify the performance invariants in the docs | recurrence prevention | ✅ **DONE** (+ `make lint-invariants`) | — |

## Checked and found healthy (do not "fix" these)

Recording the negative results matters as much as the positive ones — each of
these looked like a problem when reading the code and was not one when
measured:

- **Editing cost is independent of file size.** 1 000 keystrokes through
  `update` cost 46 ms on a 400 000-line file and 6 ms on a 1 000-line one —
  and that difference is `Seq`'s O(log n), not a scan. The modified-flag
  comparison (`bufModified`) short-circuits on `bufChars` as designed.
- **`HlCache` sync is not O(file) per edit in practice.** `update +
  refreshHighlight` costs the same as `update` alone at every file size
  measured, up to 400 000 lines.
- **Word wrap is not a slow path.** Same content, wrap on vs off, over
  keystrokes and page-downs: within ~20% either way, and *faster* with wrap on
  for long lines (fewer buffer lines are visible). The `ensureVisibleWrap`
  backward-walk design holds up.
- **The frame diff is efficient on the wire.** ~400 bytes emitted per
  keystroke frame, ~300 for plain text. The scroll/REP/diff machinery is doing
  its job; the cost is in *building* the frame, not sending it.
- **Search snippets and definition snippets already `T.copy`.** The two places
  that needed slice detaching have it, with a comment explaining why —
  which is how `0014` was identified.
- **The regex engine is fast.** The from-scratch Thompson-NFA/Pike-VM matches a
  49 MB corpus in 16–35 ms — *faster* than the literal path. No change wanted.
  (The literal case-insensitive path is the slow one — `0019`, and it is a
  micro-optimisation, not a bottleneck.)
- **The search walker's structure is sound.** Bounded `TBQueue`, ≤4 grep
  workers, per-file and global caps, binary sniffing before the bulk read: the
  aggregate throughput is ~1 GB/s. The generation counter correctly discards
  superseded results (what it does *not* do is stop the work — `0010`).
- **The explorer tree is lazy in the right way.** `Br.visibleRows` flattens the
  whole expanded tree, but the renderer's `take vh (drop brTop …)` means only
  the visible slice is ever forced: frame cost is identical with 200 and with
  20 000 entries expanded (4.7 ms either way).
- **Nursery size and parallel GC are not levers here.** 4 MB to 32 MB, serial
  vs parallel, all within noise on the realistic typing workload (`0007` §2).
  An earlier contrary result came from a benchmark dominated by the lexer bug —
  a useful reminder to fix algorithms before tuning runtimes. (The *other* RTS
  question, returning memory to the OS, turned out to matter a great deal —
  `0007` §2b.)
- **The pixel-placement encoders are good.** The hand-rolled sixel encoder is
  2.8 ms and 542 KiB per 800×600 placement — faster *and* 4.6× more compact
  than the kitty base64 payload. Neither is the animation bottleneck (that is
  `scaleRGBA` — `0018`).
- **Startup is quick.** Measured on the real binary through a PTY:
  time-to-first-frame is 60 ms with no file, 404 ms for a 49 MB text file
  (163 MB RSS, stable), 735 ms for a 603 KiB JPEG (that one is `0018`).
- **`runToolCapture` does not leak processes or fds.** `withCreateProcess`
  reaps on every path including timeout; the stdin/stderr helper threads are
  the right design. (The *number of concurrent* runs is the problem — `0004`.)

## Suggested sequence

1. **`0001` + `0002` + `0014` + `0007` §2b** — one to two days for all four, all
   confirmed defects, none changing an interface. `0007` §2b (one
   `performMajorGC` after 30 s idle) is a handful of lines and is what stops any
   transient spike becoming permanent, so it belongs with the leak fixes rather
   than with the RTS tuning it sits next to.
2. **`0020`** — two hours, and it is what stops the three recurring idioms
   behind five of these seven defects from recurring again.
3. **`0005`** — the soak harness, so step 1 is *proven* rather than asserted,
   and every later change has a baseline.
4. **`0013` + `0016` + `0017`** — confirmed, a day or two each, each touching one
   module. They need their test matrices written first (the save round-trip, the
   `csvWidths` invariant, the paste-parser boundary cases), which is why they are
   not in step 1.
5. **`0004` + `0010`** — the resource-supervision pair; they share the
   cancellation machinery and are best done together.
6. **`0003`** — the biggest remaining latency/allocation win, staged.
7. **`0006`** (+ the rest of `0007`) — instrument the session, then revisit any
   remaining tuning with evidence rather than intuition.
8. **`0008`**, then **`0009`** only if the soak says undo memory still matters.
   **`0018`** and **`0019`** are independent and can be picked up at any point.
9. **`0011` / `0012` / `0015`** — capability work, independent of all the above.
   `0015` is the one that also pays back as performance (F12 currently reads the
   whole workspace on every press).

## Principles these plans try to honour

- **Measure before proposing.** Two of the ideas that looked obvious from
  reading the code (an O(n)-per-keystroke modified-flag comparison; an
  O(n)-per-edit highlight-cache sync) turned out to be false when measured, and
  are not in this directory. Two others turned out to be worse than they
  looked.
- **Keep the architecture.** No new dependencies (boot libraries only), no TUI
  framework, pure core / thin IO shell, effects as `Effect` constructors, no
  new import cycles. Every plan states where its code goes in the existing
  layering.
- **Fail gracefully.** Nothing here may regress a dumb terminal, and nothing
  may make data loss more likely.
- **Bound everything.** Any structure that grows with session length needs a
  *structural* bound, not a promise of one — that is the lesson of `0001` and
  it is worth applying to anything added later.
