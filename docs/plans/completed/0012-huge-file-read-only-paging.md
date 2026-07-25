# 0012 — Huge files: a paged read-only view instead of a refusal

**Theme:** capability, with a hard memory-safety rationale
**Status:** ✅ **RESOLVED** — implemented 2026-07-26
**Risk (as shipped):** medium — a fourth view mode, read-only by construction

## Resolved

A file over `maxOpenBytes` now opens in a read-only paged view instead of being
refused. Measured against the real binary through a PTY:

| File | Result |
|---|---|
| 281 MB log, 4 000 000 lines | opens, **32 MB** RSS; 44 MB after 40 page-downs |
| Go To Line 2 000 000 in that file | lands exactly on the right line, 37 MB RSS |
| 120 MB file that is **one single line** | opens, **30 MB** RSS |

**`Cmedit.Pager`** owns the model and the two file operations (the
`TextBuffer` precedent for load/save living beside the format knowledge):

- `buildPagerIndex` — one streaming pass in 64 KiB blocks recording a byte
  offset every `pagerStride` (1000) lines, and sniffing the BOM and line ending.
  `pgEol` then *selects the byte the index and reader split on*, so a CR-only
  file pages correctly rather than appearing as one enormous line.
- `readPagerWindow` — seek to the nearest index entry, decode forward. Bounded
  three ways: the stride skipped, the `count` decoded, and `maxPagerLine`
  (64 KiB) per line.
- the pure view state: window, viewport, cursor line, movement, status.

Wiring: `edPager`/`docPager` (captured and restored by the zipper),
`EffPagerFill` → `pagerFilled` for key-driven scrolling, plus `fillPagerNow`
for the paths that never reach the key handler (startup, a background load
landing, a resize). `Render.drawPager` draws the window with the gutter and
horizontal scroll; a line not yet loaded shows `…` rather than blank, so
scrolling never looks like data loss. `paged-view = off` restores the old
refusal.

Read-only is enforced, not assumed: editing keys are swallowed with a status
note, Save/Save All/Revert are refused, and the in-file Find/definition menu
entries are pruned as they are for images — **except Go To Line**, which is the
entire point of a log viewer.

### Two bugs the testing caught

Both in the same place, and both would have been catastrophic in the field:

1. **The window reader made no progress without a separator.** For a file with
   no newlines it concatenated block after block looking for one — a 120 MB
   single-line file drove the editor to **51 GB resident** before it was fixed.
   It now caps each line and stops early once the last line it needs has
   reached the cap, so that file costs one block read.
2. **The index pass carried an unbounded partial line** for the same reason.

`test/Spec.hs` gained 40+ assertions: the index and window checked against a
brute-force split for LF/CRLF/CR-only/BOM/empty/no-final-newline/blank-line/
multi-byte-UTF-8 files; seeks to lines either side of stride boundaries in a
3000-line file; windows straddling a boundary; movement clamping; the
refill predicate firing only when the viewport leaves the window; and the
no-separator case as a regression guard for (1).

### Deliberately not done

- **In-file search** (§ "What works"). It needs a streaming matcher over the
  file rather than a buffer scan; until that exists the Find entries are
  *absent* in this mode rather than quietly broken. Go To Line covers the
  common need.
- **Word wrap and CSV table mode** in the paged view, as the plan already
  scoped out.
- **Re-indexing on growth** (a live log). The index is a snapshot; a file that
  grows keeps showing its old extent until reopened. The hook exists (`pollFs`
  already notices mtime changes) but follow-mode is a feature of its own.
- **The `activeView` refactor** suggested under Risks. There are now four modes
  and the dispatch is four `Maybe` checks deep; worth doing before a fifth, but
  bundling it with this change would have made the diff much harder to review.

The plan below is the original analysis, kept for the record.

---

---

## 1. Today's behaviour and why it exists

`App.classifyFile` refuses anything over `maxOpenBytes`, and
`TextBuffer.looksBinary` refuses anything with a NUL in the first 8 KiB. That
guard is correct and load-bearing: without it a 2 GB log decodes into tens of
millions of `Text` lines and the process dies. The refusal is the *right*
default.

But the common case behind that refusal is benign and useful: a large log, a
database dump, a CSV export, a JSON blob. The user wants to *look* at it —
search it, jump to a line, follow the tail — not edit it. cmedit already has
the concept of a read-only view mode that does not go through the line buffer
at all (the image view), and already has a windowed renderer.

Memory relevance: this is the difference between "the editor refuses" and
"someone raises `maxOpenBytes` and the editor dies", which is the failure mode
a long-session stability effort should pre-empt.

## 2. Design sketch

A fourth view mode, `edPager :: Maybe PagerDoc` (with the `docPager` twin in
`Document`, per the zipper rule), sitting alongside `edCsv` and `edImage` in
the `update` dispatch and in `computeLayout`.

```haskell
data PagerDoc = PagerDoc
  { pdPath    :: !FilePath
  , pdSize    :: !Integer
  , pdIndex   :: !(UArray Int Int64)   -- ^ byte offset of every Nth line (sparse index)
  , pdStride  :: !Int                  -- ^ lines per index entry
  , pdLines   :: !Int                  -- ^ total line count (from the index pass)
  , pdWindow  :: !(Int, Seq Text)      -- ^ (first line number, the decoded window)
  , pdEncoding, pdEol :: …
  }
```

- **Index pass.** One streaming read counting newlines and recording every
  1 000th offset. For a 2 GB file that is 2 M entries at stride 1 000 → an
  `UArray Int Int64` of ~16 KB. The pass is IO-bound and runs on a background
  thread with the existing spinner/`edLoading` machinery; it can also stream
  partial results ("indexed 400 MB of 2 GB") over the search queue like the
  walkers do.
- **Window.** Reading around a target line means seeking to the nearest index
  entry and decoding forward at most `stride` lines. Keep 3–4 screens' worth
  decoded; drop the rest. Live memory is O(window), independent of file size.
- **What works:** scroll, page, Ctrl-Home/End, go-to-line, mouse selection and
  copy within the window, syntax highlighting (the `HlCache` needs a "state at
  window start" story — simplest is to lex from the window start with
  `initialState` and accept that a block comment opened off-window is not
  detected; document it, as the manual already documents other honest limits).
- **What does not:** editing (read-only, like the image view — `handleEditKey`
  swallows edits), word wrap initially, CSV table mode.
- **Find within the pager** streams the file rather than searching a buffer:
  this is the same code shape as `Search.fileMatchesWith`, applied to one file
  with progressive results. Workspace find already skips these files
  (`maxFileBytesToSearch`), which stays true.

## 3. Entry points

- `classifyFile` gains a third outcome: `OutPaged path size` for a file over
  `maxOpenBytes` that is **not** binary (sniff the first block: no NULs, and it
  decodes as text). Today's error message becomes an offer: the status line
  reads "x.log is 2.1 GB — opened read-only (paged view)".
- A truly binary large file keeps today's refusal.
- A config key `paged-view = on|off` and a threshold key, so the behaviour is
  opt-out.

## 4. Testing

- **Pure:** index building over synthetic byte strings — CRLF, LF, no final
  newline, a file that is one enormous line, multi-byte UTF-8 split across the
  read boundary (the classic bug: never decode a chunk boundary mid-codepoint;
  decode with a carry-over of up to 3 bytes).
- **Pure:** window extraction — line N is the same whether reached by scroll,
  go-to-line, or jump-to-end.
- **Integration:** open a generated 1 GB file under a memory ceiling
  (`+RTS -M256m`) and assert scrolling to the end, searching, and closing all
  succeed. This is the test that proves the point of the whole plan.
- **Soak:** scroll the pager continuously for thousands of iterations and
  assert flat residency (windows must be dropped, not accumulated).

## 5. Risks

- **Mode proliferation.** There are already three view modes; a fourth
  multiplies the "which mode is active" checks in `computeLayout`, the mouse
  hit-tests and the menus. Before writing it, consider factoring the mode
  dispatch into a single `activeView :: Editor -> View` sum type — that
  refactor is worth doing anyway and would make this plan additive rather than
  multiplicative.
- **Encoding.** Only UTF-8/ASCII with the existing BOM handling; anything else
  is lenient-decoded exactly as `loadFromBytes` does today. Do not open the
  encoding-detection can of worms here.
- **The index is a snapshot.** If the file grows (a live log), the index goes
  stale. Either re-index on the existing `pollFs` mtime change, or add a
  follow-mode later; state which, and keep the first version simple
  (re-index on mtime change, preserving the view position by byte offset).
