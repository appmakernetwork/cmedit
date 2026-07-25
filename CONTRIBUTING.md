# Contributing to CMeDit

Thanks for your interest in improving CMeDit! Contributions of all kinds — bug
reports, fixes, features, docs — are welcome.

## Contributor License Agreement (required)

CMeDit uses a **licensing Contributor License Agreement (CLA)**. Before your
first contribution can be merged, you must agree to the
[Individual CLA](CLA.md).

You keep the copyright to your work. The CLA grants the project owner
(Benjamin Marsh) a broad license to your contribution — including the right to
distribute it under the GPL and, if ever needed, to re-license the project (for
example, to offer a commercial license). This keeps CMeDit's copyright
consistent and under single stewardship.

**How signing works:** when you open your first pull request, the
[CLA-assistant](https://github.com/contributor-assistant/github-action) bot will
comment with a link to the CLA and ask you to confirm. Reply on the PR with:

> I have read the CLA Document and I hereby sign the CLA

Your agreement is recorded once and applies to future contributions. (No paper,
no email needed.)

## Licensing of contributions

CMeDit is licensed under the **GNU General Public License, version 3**
(`GPL-3.0-only`). All contributions are made under that license (in addition to
the rights granted by the CLA above).

## Development

CMeDit is written from first principles with **no TUI framework** and depends
only on libraries that ship with GHC. Please keep it that way:

- **Build with `make`, not `cabal`** — this project targets offline machines
  with no Hackage index. `make` drives `ghc --make` directly.
  - `make` — build the optimized `./cmedit`
  - `make test` — build and run the test suite (`./cmedit-test`)
  - `make run` — build and launch
- **No new dependencies** unless they ship with GHC (base, bytestring, text,
  containers, array, unix, process, stm, directory, filepath, mtl).
- **Add tests** for pure logic in `test/Spec.hs` (a hand-rolled suite — there's
  no external framework offline). Interactive/TUI behaviour is verified with a
  PTY harness; see `CLAUDE.md` for the approach.
- **Match the surrounding style**: keep the pure core (`Cmedit.Editor`,
  rendering, buffer) free of IO, and route side effects through `Effect`
  constructors handled in `Cmedit.App`. See `README.md` and `CLAUDE.md` for the
  architecture.
- Run `make test` and make sure it prints `failed 0` before opening a PR.

### Performance invariants

CMeDit is meant to stay fast in a session that has been open all day. A handful
of rules cover almost everything that has gone wrong here before; each one cost
real memory or real milliseconds when it was broken.

1. **A bound must be structural, never a lazy `take`.** `take n (x : xs)`
   retains everything it promises to drop — the cap only applies if something
   walks the list that far, and nothing does. Use `Cmedit.History.pushHist`
   (a `Seq` with `Seq.take`). *Measured when broken: 51 MB of undo history live
   after 200 000 edits against a nominal 1 000-entry cap.*
2. **A `Text` that outlives its buffer must be `detach`ed**
   (`Cmedit.EditorState.detach`). `Data.Text` values are slices of a shared
   array and a buffer's lines are slices of the whole file, so one escaped slice
   pins the entire file. Applies to the clipboard, search terms, persisted
   history, completion candidates and result snippets. *Measured when broken:
   ten retained lines of a 49 MB file held 49 MB.*
3. **Per-frame work is per-*visible*-cell work.** Anything the renderer does per
   line must be proportional to the visible window, not the line length.
   *Measured when broken: 7.9 ms per keystroke on 3 000-character lines with no
   highlighting at all.*
4. **When the caller knows what changed, tell the callee.** Rediscovering it by
   diffing is O(collection). *Measured when broken: 7.4 ms and 22 MB per
   keystroke on a 300 000-row CSV, spent working out which cell had just been
   edited.*
5. **Bound anything that grows with session length**, and say what the bound is
   in a comment: undo/redo, navigation stops, input history, search results,
   paste payloads, in-flight background jobs.
6. **One background job of each kind in flight.** A generation counter that only
   discards stale *results* still pays for the work.
7. **Storage into a `Seq` is element-lazy.** `Seq.update i x` is
   `adjust (const x) i` and stores a thunk capturing the old element; use
   `Seq.adjust'` and force what you store, or versions chain and none can be
   collected.

### Soak testing

`make soak` runs 60 000 scripted editor operations and fails if live heap,
allocation per operation or time per operation grows with session length.
`make soak-long` runs ten times as many. It is not part of `make test` (it takes
tens of seconds), but run it before merging anything that touches editing state,
caches or background work.

### Measuring

The test suite builds at `-O0`, which is right for correctness and wrong for
performance. Build a harness against `src/` instead — `docs/plans/bench/`
has one, its exact command line, and the PTY probes that measure the real
binary's startup, RSS and idle behaviour.

Two traps in this codebase produce plausible numbers that are simply wrong, in
opposite directions:

- **Force your inputs before timing.** Building the test buffer is O(file) and
  is otherwise charged to whatever you measure first.
- **Force your outputs meaningfully.** `Screen`'s cells are lazy, so forcing a
  `Screen` to WHNF renders nothing; a render benchmark must run the real
  `renderFrame` diff. Likewise `length . map` over `Text` fuses into a count
  that never builds the result.

To diagnose a real session, `cmedit +RTS -hT -i5` gives a heap census by closure
type and `+RTS -s` a summary — both work on the shipped binary (it is built with
`-rtsopts`), no profiling libraries needed. A data constructor and `THUNK_*`
appearing in identical byte counts is the "lazy accumulator" signature.

## Reporting bugs

Open an issue with the terminal you're using, the steps to reproduce, and what
you expected versus what happened. For rendering glitches, the `$TERM` value and
terminal size help a lot.
