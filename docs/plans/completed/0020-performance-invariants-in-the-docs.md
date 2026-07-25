# 0020 — Codify the performance invariants where contributors will read them

**Theme:** preventing recurrence (the cheapest plan here, and possibly the
highest-leverage)
**Status:** ✅ **RESOLVED** — implemented 2026-07-26
**Risk:** none

## Resolved

- **`CLAUDE.md`** gained the invariants as first-class architecture notes,
  alongside the existing design rules: structural history bounds
  (`Cmedit.History`), `detach` at buffer-escape boundaries, explicit strictness
  when storing into a `Seq`, and the event loop's idle collection with the RTS
  options that make it visible.
- **`CONTRIBUTING.md`** gained a "Performance invariants" section — seven rules,
  each with the measurement that justifies it — and a "Measuring" section
  covering the harness, the two laziness traps that produced wrong answers
  *during this very investigation*, and the `+RTS -hT` / `+RTS -s` recipe that
  works on the shipped binary (which is now built with `-rtsopts`).
- **`make test` enforces invariant #1.** A new `lint-invariants` target fails
  the build if a history field is ever assigned a lazy `take` again. Verified to
  fire: re-introducing `edUndo = take maxUndo …` fails with a pointer to the
  rule. It matches the exact idiom rather than `take max` generally, which had
  false positives (`take maxw` in a display-width truncation).

Two rules gained enforcement beyond the docs during the other plans: the
`csvWidths` fuzz test (invariant #4) and the retention guards in the suite
(#1, #2).

The plan below is the original analysis, kept for the record.

---

---

## 1. Why

Of the seven confirmed defects in this directory, **five are instances of three
recurring idioms**, not one-off mistakes:

| Idiom | Where it recurred |
|---|---|
| `take n (x : xs)` used as a bound | 9 sites (`0001`), incl. the CSV grid history (`0008`) |
| A `Text`/`ByteString` slice outliving its parent | clipboard, find terms, history (`0014`) — guarded correctly in only 2 places |
| Whole-collection work where the caller knows the change | `syncWidths` per cell edit (`0016`), whole-line scans per frame (`0003`) |

Plus two measurement traps that made *this very investigation* produce two
wrong answers before it produced right ones:

| Trap | Effect |
|---|---|
| Timing a lazily-constructed input | Charges construction to the first measurement — produced a false "editing is O(file size)" result |
| Forcing a lazily-consumed output to WHNF only | Measures nothing — `Screen`'s boxed cells meant a "render benchmark" that did no rendering |

`CLAUDE.md` is already an unusually good architecture document — it explains
the *design* invariants (the zipper, the two coordinate systems, the capability
gates, the `csvWidths` cache, the `HlCache` self-validation) with the reasons
attached. What it does not yet carry is the *performance* invariants. Adding
them costs an afternoon and is the only thing here that prevents the next
instance.

## 2. What to add

### 2.1 To `CLAUDE.md`, a new "Performance invariants" section

Short, imperative, each with the reason and the evidence:

1. **A bound must be structural, not a lazy `take`.** `take n (x : xs)` retains
   everything it promises to drop; the cap only applies if something walks the
   list that far, and nothing does. Use a `Seq` with `Seq.take` (O(log n) and
   genuinely strict) or force the spine. Measured: 51 MB retained after 200 000
   edits against a nominal 1 000-entry cap.
2. **A `Text` that outlives its buffer must be `T.copy`'d.** A slice pins the
   whole array it was cut from. Measured: ten lines (680 characters) of a 49 MB
   file keep 49 MB live. `Cmedit.Search`'s snippet clipping is the model.
3. **Per-frame work is per-*visible*-cell work.** Anything the renderer does
   per line must be proportional to the visible window, not the line length —
   the horizontal-scroll path already gets this right for cell expansion
   (`windowStart`) and wrong for URL scanning and tokenisation.
4. **When the caller knows what changed, tell the callee.** Discovering the
   change by diffing is O(collection): `syncWidths` costs 7.4 ms per keystroke
   on a 300 000-row CSV to rediscover the cell the caller just edited.
5. **Bound anything that grows with session length**, and say what the bound is
   in a comment: undo, redo, nav stops, input history, search results, the
   explorer's mtime map, in-flight background jobs, paste payloads.
6. **One in flight per kind.** Background work (lint passes, walkers, loads)
   supersedes rather than accumulates; a generation counter that only discards
   *results* still pays for the work.

### 2.2 To `CONTRIBUTING.md`, a "How to measure this codebase" section

- Build a harness against `src/` with `ghc --make -isrc -iplatform/posix`
  (`docs/plans/bench/README.md` has the exact command line) — the test suite
  builds at `-O0`, which is right for correctness and wrong for performance.
- **Force your inputs before timing** and **force your outputs meaningfully**:
  `Screen`'s cells are lazy, so a render benchmark must run the real
  `renderFrame` diff; a buffer benchmark must force every line first. Both
  traps produce plausible numbers, in opposite directions.
- Diagnose a real session with `+RTS -hT -i5` (heap census by closure type —
  works on the ordinary `-O2` build; the profiling libraries are not required
  and, in this environment, not installed) and `+RTS -s`. Two identical byte
  counts for a data constructor and `THUNK_*` is the "lazy accumulator"
  signature.

### 2.3 A one-line rule in the review checklist

> Does this change add anything whose size depends on how long the session has
> been running? If so, where is its structural bound?

## 3. Testing

None to write, but three of the invariants above can be *enforced* cheaply, and
should be:

- `grep -rn "take max" src/` must return nothing (invariant 1). A two-line
  check in `make test` is enough, with a comment pointing at `0001`.
- The `Spec.hs` `csvWidths` fuzz test already enforces invariant 4's safety
  side; extend it as `0016` describes.
- The soak harness (`0005`) is the general enforcement mechanism for
  invariant 5.

## 4. Note

This plan is deliberately last in the numbering and first in value-per-hour.
The measurements in this directory will go stale; the invariants will not.
