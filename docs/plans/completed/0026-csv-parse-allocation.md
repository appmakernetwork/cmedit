# 0026 — Opening a CSV: parse the buffer's lines, and stop unpacking every cell

**Theme:** open-time cost of the table view
**Status:** ✅ **RESOLVED** — implemented 2026-07-27
**Risk (as shipped):** low — the parser rewrite landed behind a randomised
oracle test against the implementation it replaces, and the existing
`csvWidths` fuzz test covers the width path

## Resolved

A 32 MB CSV (223 209 rows × 12 columns, quoted fields, embedded commas,
multi-line quoted cells, ragged rows) through the real load path:

| | Before | After |
|---|---|---|
| **open the table** (join + parse + width cache) | **1 090 ms, 3 719 MB** | **794 ms, 852 MB** |
| — the re-join (`bufferToText LF False`) | 33 ms, 125 MB | *gone* |
| — the parse | 760 ms, 908 MB | 484 ms, 773 MB |
| — the width cache (`computeWidths`) | 481 ms, 2 697 MB | **244 ms, 95 MB** |
| live heap, buffer **and** grid retained | 224 MB | **192 MB** |
| RSS of the real editor with the file open | 578 MB | **444 MB** |
| time to first frame, real editor over a PTY | 1 311 ms | **864 ms** |
| 100 cell edits at the last row (guard) | 234 ms, 1 416 MB | 234 ms, 1 416 MB |
| 100 `columnWidths` calls (guard) | ~0 ms, ~0 MB | ~0 ms, ~0 MB |

The same file written **without quoting** — what most CSVs actually look like,
and what the parser's new fast path is for — 32 MB, 241 394 rows × 12 columns:

| | Before | After |
|---|---|---|
| open the table | **1 084 ms, 3 666 MB** | **684 ms, 425 MB** |
| — the parse | 661 ms, 684 MB | 325 ms, 337 MB |
| — the width cache | 487 ms, 2 864 MB | 240 ms, 99 MB |
| live heap, buffer and grid | 228 MB | **196 MB** |
| RSS of the real editor with the file open | 587 MB | **240 MB** |
| time to first frame, real editor | 1 253 ms | **674 ms** |

And the same file with **CRLF** line endings (32 MB, 221 738 rows): 1 085 ms /
3 691 MB → 794 ms / 844 MB, live 223 → 191 MB. (CRLF costs 51 MB more in
`loadFromBytes`, before any of this, because `normalizeNewlines` runs; that is
unchanged.)

Allocation to open is now **27× the file size** for the quoted table and
**13×** for the plain one, against 116× and 115× before; the number that made
this worth doing — the width cache's 2.7 GB — is gone entirely.

Three changes:

1. **`cellWidth` is a strict fold.** It was
   `maximum . map (sum . map effW . T.unpack) . T.splitOn "\n"`: a `String` (a
   cons cell and a boxed `Char` per character) plus a list of slices, **per
   cell**, and `computeWidths` calls it on all 2.7 million of them when a table
   opens. One `T.foldl'` carrying `(widest line, current line)` in a strict
   pair does the same job and allocates nothing. This alone was 2 602 MB of the
   2 867 MB saved on the quoted table.
2. **The load path parses the buffer's lines** (`Csv.csvParseLines` /
   `Csv.mkCsvLines`) instead of `Csv.mkCsvView (bufferToText LF False buf)`.
   The old spelling rebuilt the whole file as a second `Text` — along exactly
   the newlines the buffer had just been split on — purely so the parser could
   take it apart again. Worse, the grid's cells were then slices of *that*
   array, so an open CSV document held two complete copies of the file: 32 MB
   of the 224 MB live heap, and the difference between 578 MB and 444 MB of RSS.
3. **One parser, with a fast path.** The parser is now a cursor into a list of
   lines — the remainder of the current line plus the lines after it — with the
   newline between them implicit, never materialised and never scanned for.
   `csvParse` (still used by `pasteClip` and the tests) is `T.split (== '\n')`
   followed by the same engine, so there is no second implementation to drift.
   A line containing no quote and no CR skips the field machinery entirely: the
   record is one strict `T.break` loop over the delimiter, and every cell is a
   slice. The **strictness of that loop is worth 111 MB on its own** (448 MB →
   337 MB on the unquoted 32 MB file): without the bang, each cell reaching the
   row's `Seq` is a selector thunk on the `T.break` pair, and `Seq` is
   element-lazy, so a grid of 2.9 million thunks is what gets built. This is
   the hazard CLAUDE.md's "buffer writes are explicitly strict" bullet is
   about, in a new place.

### Guards (17 new checks)

- **A randomised oracle test.** The parser that shipped between 0016 and this
  plan is kept verbatim in `test/Spec.hs` as `csvParsePrev`, and 400 generated
  inputs — built from tokens chosen to hit bare and doubled quotes, stray text
  after a closing quote, lone CR, CRLF, newlines inside quoted cells, ragged
  rows and multi-byte characters — are checked three ways: `csvParse` against
  `csvParsePrev` for four delimiters, `csvParseLines (bufLines buf)` against
  `csvParse (bufferToText LF False buf)` (the production equivalence, including
  the CR normalisation `fromText` applies), and `mkCsvLines` against
  `mkCsvView` for both the grid *and* the column widths. 372 of the 400
  contain a quote, 337 span records, 289 contain a CR, 199 contain multi-byte
  text; feeding the comparison a deliberately wrong newline (lines joined with
  nothing) is caught in 343 of them, so the test discriminates.
- **Ten hand-pinned edge cases of the line cursor** — an empty buffer, two
  empty lines, a trailing empty line, a quoted cell spanning three lines, an
  unterminated quote at EOF, doubled quotes inside a spanning cell, ragged
  rows, stray text after a closing quote, a lone CR mid-line and a CR at a line
  end (which eats the line break). Every one of them is a question about the
  implicit newline, which is where this design's whole risk lives.
- **`cellWidth` against the `splitOn`/`unpack` version it replaced**, over the
  same 400 inputs plus wide glyphs, a variation selector, a combining mark, a
  ZWSP and a control character.
- The existing 26-case `csvParseRef` corpus (the pre-0016 `String` parser),
  the serialisation-stability corpus and the 600-operation `csvWidths` fuzz
  test all still pass unchanged.

The win is **not an `-O2` artefact**: rebuilt at `-O1` the same file goes
1 188 ms / 3 783 MB → 747 ms / 908 MB.

## What was tried and not taken

**Slicing the line by byte offset** (`Data.Text.Internal.Text arr off len` plus
`Data.Text.Array.unsafeIndex`) instead of the `T.break` loop — the strongest
form of "parse straight off the decoded content". Splitting every line of the
32 MB quote-free file into fields:

| | Time | Allocated |
|---|---|---|
| `T.break` loop (shipped) | 139 ms | 309 MB |
| `T.splitOn` (public API) | 151 ms | 461 MB |
| `T.split` (public API) | 150 ms | 461 MB |
| byte-offset slicing | **92 ms** | 302 MB |

Not shipped, for two reasons that only became clear in this order. It
hard-codes `text`'s internal representation: in text ≥ 2.0 those offsets are
UTF-8 **bytes** and the array element is a `Word8`; in text 1.2 they are UTF-16
code units and a `Word16`. `cmedit.cabal` asks for `base >= 4.14` — GHC 8.10,
which ships text 1.2 — and the `text` bound is open, so the module would stop
compiling on half the range the project claims to support. And once the `T.break`
loop was made strict (change 3 above, which the first version of this
comparison did not have), **the allocation advantage disappeared**: 302 MB
against 309 MB. What is left is 47 ms on a 32 MB file, for an internal API. The
public-API alternatives were measured at the same time and are both worse than
the loop already in place, so there is nothing else to take here.

The wider lesson is worth recording: the first run of this table said the
internal version saved 26% of the allocation, and that number was an artefact
of comparing against a *lazy* competitor. Fix the competitor first.

**Walking the grid `Seq`s directly in `computeWidths`** rather than through
`toList`. Three spellings were measured on the 32 MB file: `toList` per row
(241 ms / 91 MB), `Data.Foldable.foldlM` (238 ms / 123 MB) and an explicit
`Seq.index` loop (288 ms / 4 MB). The list cells are the cheapest thing in the
loop — they are bump-allocated and die in the nursery — while `Seq.index`'s
O(log n) costs 45 ms. `toList` was kept, and the `maximum . map . toList` that
computed the column count was replaced by a `foldl'`.

## Found along the way, not fixed here

**`isModified` costs 2.3 ms and 14 MB per keystroke once a large table has been
modified.** The cell edit itself is genuinely free — 100 `beginEdit` /
`editInsert` / `commitEdit` cycles at the last row of the 223 000-row table
allocate **0 MB** and take **0 ms**, which is 0016's `withCell` working exactly
as advertised — but the `isModified` call that follows each of them costs
14 MB. `sameGrid`'s pointer shortcut fires at the top level only while the grid
*is* the saved grid; after the first edit it falls through to
`zipWith sameRow (toList a) (toList b)`, which walks (and lists) all 223 000
rows on every subsequent keystroke. 0016 measured this at 0.04 ms because it
measured an unmodified grid. It wants an incrementally-maintained flag rather
than a comparison, which is a change to what `csvSaved` means, so it is a plan
of its own rather than a rider on this one.

## The measurements

Corpus built rather than found, deterministic and seeded, by
`docs/plans/bench/gen_csv.py` (added by this plan): 12 columns, `id,name,email,city,word,quoted,with_comma,with_quote,amount,date,notes,tail`,
where column 5 is a quoted plain field, column 6 a quoted field containing the
delimiter, column 7 a quoted field containing doubled quotes, column 10 a
**multi-line** quoted cell every 500th row, and every 997th row is short
(ragged). Three variants: LF, CRLF, and one with no quoting at all. 32 MB each.

```python
f = [str(i), "%s %s" % (w, w2), "%s%d@example.com" % (w, i), city, w,
     '"%s %s"' % (w, city), '"%s, %s"' % (city, w),
     '"he said ""%s"" loudly"' % w, "%.2f" % amount, "20%02d-%02d-%02d" % ymd,
     ('"line one%sline two%sline three"' % (eol, eol)) if i % 500 == 0 else w,
     str(n)]
if i % 997 == 0: f = f[:8]          # ragged row
```

Timing and allocation come from `GHC.Stats` on an `-O2` build with `-T`, in the
harness described in `docs/plans/bench/README.md` — `bench csv FILE` for the
split between parse and width cache, and `bench csvlive FILE lines|text` (added
by this plan) for the live heap, which cannot be compared in one process
because whichever grid is built first is still reachable when the second is
measured. Both of its traps apply and
were handled: the buffer is built and forced *before* the timed block (trap 1),
and the grid is consumed by summing `T.length` over every cell — forcing a
`Seq` to WHNF parses nothing (trap 2). The live-heap figures keep **both** the
buffer and the grid reachable past the `performMajorGC`, which is what the
editor does; an earlier version of the probe reported 43 MB for both variants
because the grid was dead by the time it was measured. RSS and time-to-first
frame are from the real `cmedit` binary driven over a PTY, old and new built
from the same tree with only these changes reverted.

## Where the remaining allocation goes

852 MB for the quoted file, of which ~773 MB is the parse. Per row of twelve
fields that is ~3.5 KB: roughly 1 KB of `T.break` results (a lazy pair and two
`Text` headers per field, the item the byte-slicing experiment above would
remove), ~400 B for the row's `Seq` (`Data.Sequence` wraps each element in an
`Elem`), ~600 B of list cells for the field accumulator and its `reverse`, and
one `Fld` per field. The live 192 MB is the grid itself: 2.7 million four-word
`Text` headers is 86 MB before any structure around them. Cutting either
meaningfully means changing what a row *is*, not how it is parsed.
