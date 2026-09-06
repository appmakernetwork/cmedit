# 0031 — Compact CSV rows without edit or save regressions

**Theme:** retained memory, open/save latency, bounded wide-row behaviour  
**Status:** ✅ **RESOLVED 2026-09-05**

## Problem

After the parser and cache work in `0016`, `0026`, `0028`, and `0029`, a
loaded table still retained a `Seq Text` for every row. On the 32 MB benchmark
file that means 2.68 million persistent `Text` slice headers plus the inner
finger trees, even though almost every cell is an unchanged span of one
physical input line.

A boxed array alone was not sufficient: it reduced some structure but copied a
whole exceptionally wide row for every cell edit. An early source-span
prototype reduced live memory but increased load allocation and made saving
slower, so it did not meet the acceptance gate.

## Resolution

`Row` now has two internal representations:

- `PackedRow` retains one physical source line, an unboxed array of
  `(start, length+width)` descriptors, a bounded sparse list for decoded
  exceptions, and a sparse edit overlay.
- `OwnedRow` is a boxed array used for multiline records, structural results,
  pasted/workbook grids, and deliberately adversarial rows.

The descriptor's second `Int` packs the character length and the clamped
display width. This keeps the mandatory width-cache build from materialising a
`Text` slice for every cell. A single-line packed row contains no embedded
newlines, so the newline cache also avoids scanning its cells. Cell writes
change only the overlay; reverting to the source value removes the overlay
entry. Row/column structural operations already have whole-row semantics and
convert to `OwnedRow`.

Saving does not blindly reuse input spelling: `csvToText` makes one strict scan
of an untouched packed source row to determine whether canonical CSV spelling
would differ. It reuses the whole source for canonical rows, patches a sole
noncanonical field, and uses the established serializer for multiple changes,
edited rows, multiline rows, and producer-owned rows. Redundant quotes, bare
quotes, doubled quotes, quoted delimiters, empty quoted fields, and tolerated
post-quote text retain the previous normalization behaviour.

## Bounds and fallbacks

- Packed rows are capped at 4,096 raw fields. Wider or delimiter-heavy rows use
  `OwnedRow`, avoiding descriptor over-allocation from delimiters inside quotes.
  A row also falls back when the raw-delimiter capacity exceeds the parsed
  field count by more than 4× (with a 32-field grace window).
- Descriptor packing is used only while `lineLength <= maxBound / 64`, so the
  length multiplication cannot overflow on either platform word size.
- More than 32 decoded exceptions in one row switches it to `OwnedRow`; sparse
  list lookup therefore cannot become quadratic on an attacker-controlled wide
  row.
- Quotes spanning physical lines and lone-CR record boundaries stay on the
  existing general parser. The compact path is never asked to guess them.
- `Row` equality is semantic across representations. `mkCsvGrid` retains its
  existing `Seq (Seq Text)` producer API, so XLSX and tests do not inherit the
  storage representation.

## Measurements

GHC 9.0.2, `-O2`, 32 MB / 223,209-row benchmark corpus:

| Operation | Before | After |
|---|---:|---:|
| `mkCsvLines` (real editor open path) | 1,045 ms / 919 MB allocated | **697 ms / 796 MB** |
| live heap / RSS with table open | 192 MB / 418 MB | **146 MB / 310 MB** |
| full `csvToText` save materialisation | 339 ms / 1,762 MB | **273 ms / 518 MB** |
| 100 undo/redo pairs | 4,970 ms / 12,064 MB | **2,789 ms / 11,896 MB** |
| full `computeNl` | 135 ms / 64 MB | **3 ms / 12 MB** |
| last-row cell edits | ~0 ms / 0 MB | **unchanged** |

The compact representation therefore removes 46 MB (24%) from the retained
heap while improving, rather than trading away, each production open, save,
edit, cache, and history path measured by the existing harness.

## Verification

- The parser is compared with both historical implementations across the fixed
  dialect corpus and 1,600 randomized delimiter/input combinations.
- Compact token/save-specific tests pin redundant/missing quotes, canonical
  quoting, doubled quotes, and tolerated post-quote text.
- Existing 600-operation CSV mutation fuzzing checks widths, dirty state,
  embedded-newline counts, position mappings, undo/redo, and serialization
  after every operation.
- `make test`, `make windows-check`, and `make soak` cover correctness,
  portability, and long-session bounds; `bench csv`, `bench csvlive`, and the
  dedicated before/after save probe provide the figures above.
