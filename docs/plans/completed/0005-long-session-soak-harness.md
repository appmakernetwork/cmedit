# 0005 — A long-session soak harness (the regression guard the others need)

**Theme:** proving stability, not asserting it
**Status:** ✅ **RESOLVED** (harness A shipped; harness B — the PTY soak —
deliberately deferred, see below) — implemented 2026-07-26
**Risk:** none — test-only, nothing ships in the binary

## Resolved

**`soak/Soak.hs`** drives the pure model through a scripted, deterministic
session (an LCG seed, printed and reproducible) and samples the heap as it runs.
The operation mix deliberately touches everything that *accumulates*: typing,
deleting, undo/redo, select-all, copy (clipboard retention), far jumps
(Ctrl+Home/End, which fill the navigation trail), paste, line duplication,
resizes, a three-document zipper, and a CSV view edited in parallel so the
table's own undo history and width cache grow too.

Assertions are ratios between the first and last quarter of the run, so they
survive any machine: live heap flat, allocation per operation flat, time per
operation flat.

```
make soak        # 60 000 operations, ~40 samples
make soak-long   # 600 000
```

Current output, on the tree with plans 0001/0002/0013/0014/0016/0017 landed:

```
PASS  live heap is flat  (0.3 MB -> 0.4 MB)
PASS  allocation per operation is flat  (5.2 MB -> 7.0 MB per sample)
PASS  time per operation is flat  (2 ms -> 2 ms per sample)
```

**It discriminates.** Removing the bound from `Cmedit.History.pushHist` makes it
fail:

```
FAIL  live heap is flat  (0.7 MB -> 4.0 MB)
```

That check was tightened for exactly this reason: the first version's additive
slack (`liveA * 2 + 4` MB) *swallowed* the leak it exists to catch and reported
PASS. A guard that cannot fail is worse than no guard, so it is now
`liveA * 2 + 1.5` and verified in both directions.

**Not part of `make test`.** It takes tens of seconds; a slow `make test` is a
test people stop running. It is its own target, documented in `CONTRIBUTING.md`.

## Deferred

- **Harness B (the PTY session soak)** — RSS/fd/thread sampling against the real
  binary. The individual probes exist and were used throughout this work
  (`docs/plans/bench/pty_startup.py`, `pty_rss.py`, `pty_ratchet.py`, and the
  fake-linter concurrency check in `0004`); what is missing is a single
  long-running driver that ties them together. The pieces are there for whoever
  wants it.
- The two questions parked here by other plans (§3c) — buffer compaction from
  `0014` and the capability count from `0007` — still need this harness extended
  with a memory-shape script and a pause-distribution measurement respectively.

The plan below is the original analysis, kept for the record.

---

---

## 1. Why

Every other plan in this directory claims a stability property — bounded
history, bounded processes, flat allocation. None of those claims survive
contact with a refactor unless something checks them repeatedly. The existing
suite (`test/Spec.hs`, one hand-rolled program, no framework) is excellent at
*correctness* and says nothing about *behaviour over time*.

The undo leak in `0001` is the proof: it is not a subtle bug, it is a one-line
idiom repeated at nine sites, and it has been invisible because nothing has
ever measured the editor's heap after ten thousand edits.

## 2. Two harnesses, different jobs

### A. In-process soak (`soak/Soak.hs`, part of `make test`)

Drives the **pure model** — no terminal, no PTY, no timing sensitivity — for a
long scripted session, sampling `GHC.Stats` as it goes.

```haskell
-- Built with -with-rtsopts=-T so getRTSStats works.
data Sample = Sample { sIter :: !Int, sLive :: !Word64, sAlloc :: !Word64 }

soak :: Int -> Editor -> IO [Sample]
```

The script must exercise the state that *accumulates*:

| Behaviour | Why it belongs in the soak |
|---|---|
| Type / delete / undo / redo cycles | undo & redo stacks (`0001`) |
| Open, switch, close many documents | the zipper, `edRecent`, `docHlCache`, `docDiags` |
| Repeated Find / Replace with new terms | `edFindHist`, `edReplHist` |
| Jump around (go-to-line, F12, results) | `edNavBack` / `edNavFwd` |
| Toggle CSV view, edit a table, undo | `csvUndo` grids, `csvWidths` |
| Explorer expand/collapse over a deep tree | the `Browser` tree, `drvDirMtimes` |
| Word-wrap on/off, resize, theme changes | layout + `HlCache` invalidation |
| Render a frame every N iterations | `HlCache`, per-frame allocation |

Assertions, all of them *ratios* rather than absolutes so they survive on any
machine:

1. `live(100k iterations) < 2 × live(10k iterations)` — the shape of a bounded
   system. A leak makes this linear and the test fails.
2. `alloc-per-iteration` in the last 10% of the run is within 1.5× of the
   first 10% — catches "it gets slower as it goes" (a growing list being
   re-walked, a cache that degrades into a scan).
3. Wall-clock per iteration in the last 10% within 2× of the first 10%.

Sample every 1 000 iterations, `performMajorGC` before each sample, and dump
the series to `soak.tsv` on failure so the shape is inspectable.

Runtime target: under 60 s in the default `make test`, with `make soak-long`
running a 10× script for release checks.

### B. PTY session soak (`soak/pty_soak.py`, run manually / in CI nightly)

The project already documents the PTY technique for verifying interactive
behaviour (see `CLAUDE.md`). Extend it into a soak driver:

- `openpty`, `TIOCSWINSZ`, exec `./cmedit` with the slave as stdin/stdout;
- answer the capability probes both ways (one run silent, one run replying
  OSC 11 / DA1 / XTVERSION / kitty / REP) so both the fallback and the upgrade
  paths get soaked;
- feed a long randomised keystroke script (weighted to real usage: mostly
  printable characters, then arrows, then Ctrl-combos from a safe list that
  excludes quit);
- inject `SIGWINCH` resizes and focus in/out (`CSI I` / `CSI O`) periodically;
- sample `/proc/<pid>/status` (VmRSS, Threads), `/proc/<pid>/fd` count, and
  the child-process count every few seconds;
- assert at the end: RSS growth over the last two thirds of the run is under a
  threshold, thread count is flat, fd count is flat, no orphaned children.

The fd and thread checks are the ones an in-process soak cannot do, and they
are exactly what `0004` (linter processes) and `0010` (walker threads) need.

## 3. Implementation notes

- Keep both harnesses out of the shipped binary: a `soak/` directory and two
  Makefile targets (`soak`, `soak-long`), with `test` depending on the short
  in-process one.
- `getRTSStats` requires the `-T` RTS flag. Add `-with-rtsopts=-T` to the
  *soak* target only, not to `cmedit` itself (see `0007` for the separate
  question of the shipped RTS options).
- Determinism: seed the keystroke generator explicitly and print the seed, so
  a failure is reproducible.
- The in-process soak should build at `-O2` (the test suite builds `-O0`
  today, which is right for a correctness suite and wrong for a performance
  one). This is why it wants its own target rather than living inside
  `Spec.hs`.

## 3b. When an assertion fires, what next?

A ratio assertion tells you *that* something grew, not *what*. Pair the soak
with a heap census by closure type — `+RTS -hT`, which works on an ordinary
`-O2` build with no profiling libraries (they are not installed in this
environment, so do not build the workflow around `-prof`). Dump a census every
few seconds during the soak and, on failure, print the top ten closure types of
the last sample. That is exactly how the undo leak in `0001` was pinned down:
`UndoState` and `THUNK_1_1` in identical byte counts is an unmistakable
"lazy accumulator" signature.

## 3c. Two questions deferred here from completed plans

- **Buffer compaction** (`0014` §4.2): deleting most of a large file still
  retains the original array, because the surviving lines are slices of it.
  Compacting (rewriting survivors with `T.copy`) breaks the pointer-identity
  fast paths in `HlCache`, `Csv.sameGrid` and `bufModified`, so the trigger
  threshold has to be justified by a soak measurement rather than guessed.
- **Capability count** (`0007` §4): `runTui` pins `min 4 (cores-1)` capabilities
  for the search pool, so every GC is a parallel GC for the 99.9% of a session
  with one busy thread. Whether `-qn2`, or raising capabilities only around a
  search, is better shows up in the pause distribution — which needs the soak,
  not a microbenchmark.

## 4. What this unlocks

- `0001` gets a real assertion instead of a structural one.
- `0002`/`0003` get a stable place to record before/after numbers, so future
  work can be compared against a committed baseline (`docs/plans/baseline.tsv`).
- `0004`/`0010` get their process/thread/fd invariants checked.
- Any future feature that adds per-document state gets it exercised for free,
  because the soak script opens and closes documents.

## 5. Risks

- A soak test that is flaky is worse than none. Every assertion must be a
  ratio with generous headroom, and the failure output must include the
  series so a human can tell "leak" from "noisy machine" in five seconds.
- CI time: keep the default soak at ~60 s and gate the long one behind a
  separate target.
