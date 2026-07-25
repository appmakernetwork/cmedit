# 0010 — Supervising background work: threads, queues and cancellation

**Theme:** long-session resource stability
**Status:** ✅ **RESOLVED** — implemented 2026-07-26
**Risk (as shipped):** low–medium — driver-only; see the verification caveat

## Resolved

**§2.1 — a job supervisor.** `Drv` carries `drvJobs :: IORef (Map JobKind
ThreadId)` with `JobKind = JSearch | JDefs | JQuickOpen | JLoad | JReplace`.
`startJob` kills the previous job of the same kind under `mask_` and the job
removes itself on completion (only if the slot still names it). The search,
definition and quick-open walkers and the async file load all run through it.
The generation counters stay as the correctness backstop — a killed thread may
already have queued a message.

**`runScan`'s worker pool is now bracketed.** This was the trap the plan warned
about: `runScan` forks its own grep workers, so killing the walker alone would
have left them blocked on the queue forever *and* left the end-of-walk wait
unsatisfiable. `bracket (forM … forkIO worker) (mapM_ killThread)` ties them to
the walk.

**§2.2 — the replace paths are off the main thread.** `EffReplaceOnDisk` forks
the whole rewrite; `EffStageReplace` forks the *reads* and posts `SMStaged`,
whose handler does the (pure) staging and the explorer reveal in the event loop.
Both show the loading overlay while they run. A replace job that dies for any
reason still posts its completion message (`replaceFailed`), because that
message is what clears the overlay — an overlay outliving its job would leave
the editor swallowing input.

**§2.4 — drag coalescing.** `coalesceDrags` collapses a run of consecutive
same-button drag events in a batch to its last one. A terminal reporting motion
can deliver dozens per batch; only the final position is ever displayed.

**Not taken: §2.3 cancellable loads beyond the supervisor.** `JLoad` is
supervised (a newer open cancels an in-flight one), but the outcome queue has no
generation check, so a just-completed-but-superseded load can still install. It
is harmless — the newer load installs after it — and adding a generation to
`LoadOutcome` touches every open path. Noted rather than done.

**Not taken: §2.5 counters.** They belong to `0006`.

### Verification caveat

The suite (2 208 passing), `make windows-check` and code review cover the
change; the *UI-responsiveness during a large staged Replace All* was **not**
verified end-to-end. Driving the search view's replace flow through the PTY
harness needs the right function-key encodings for F4/F6/Ctrl+Enter and my
attempts did not reach it. The structural property — `perform` returns
`beginLoading …` without touching the files — is evident in the code, but
someone should confirm it interactively (open a folder, F4, F6, Replace All over
a few hundred files, and check the spinner animates).

The plan below is the original analysis, kept for the record.

---

---

## 1. Current shape

The driver forks unsupervised threads in seven places:

| Site | Thread | Lifetime |
|---|---|---|
| `readerLoop` | input parser | process lifetime (intended) |
| `EffOpen` (large file) | `classifyFile` → `loadQ` | until the read finishes |
| `EffStartSearch` | `runWalker` + up to 4 grep workers | until the walk finishes or is superseded |
| `EffFindDefs` | `runDefWalker` + workers | same |
| `EffQuickOpen` | `runQuickWalker` | same |
| `EffStageReplace` / `EffReplaceOnDisk` | inline in `perform` (blocking!) | — |
| `forkDetectLinters`, `startLintRun` | linter probe / runner | see `0004` |

The generation-counter pattern (`drvSearchGen`, `drvDefGen`, `drvQuickGen`,
`drvLintGen`) is a good design: a superseded worker notices and bails. But it
is **cooperative cancellation with coarse checkpoints**, and there is no
accounting of what is alive.

Concrete consequences over a long session:

1. **Superseded walkers keep working.** `runScan`'s workers check `scAlive`
   per file and the walk checks per directory entry — good — but each worker
   can still be inside a multi-megabyte `BS.hGetContents` + decode + match for
   a single file when superseded. Retype a search term ten times over a large
   repo and you have up to ten walks' worth of workers finishing their current
   file each. Bounded, but wasteful and invisible.
2. **`EffStageReplace` and `EffReplaceOnDisk` run on the main thread.** They
   read and rewrite up to `maxStageReplaceFiles` files inside `perform`, which
   is called synchronously from `applyBatch`. The UI is frozen for the
   duration — no spinner, no repaint, no Ctrl-C. This is the one place the
   otherwise-strict "IO goes to a background thread" discipline is broken.
3. **No thread accounting.** There is no way to know how many walkers are
   alive; the PTY soak in `0005` is the only way to notice a leak, and only
   after the fact.
4. **The load queue has no cancellation at all.** Open a 400 MB file by
   accident and the read runs to completion even if the user immediately opens
   something else; the outcome is then applied (`applyOutcome` on a stale
   editor is harmless, but the work and the memory spike are not).

## 2. Proposal

### 2.1 A tiny supervisor in `Drv`

```haskell
data Job = Job { jobKind :: !JobKind, jobGen :: !Int, jobTid :: !ThreadId }
, drvJobs :: !(IORef (Map JobKind Job))   -- at most one job per kind
```

`JobKind` = `JSearch | JDefs | JQuickOpen | JLint | JLoad`. Starting a job of a
kind kills the previous one of the same kind (`killThread`) and records the new
one; the job's `finally` removes itself. This is ~40 lines and it converts
cooperative cancellation into immediate cancellation, with the cooperative
generation check retained as the correctness backstop (a killed thread might
already have queued a message).

Safety: every worker's IO is either STM, a `BS.readFile`, or a
`withCreateProcess` bracket — all interruptible or bracket-protected on the
threaded RTS. The one thing to verify is that a killed grep worker cannot
leave `doneVar` short, wedging `runScan`'s end-of-walk wait: fix by making the
walker's wait `orElse` an "alive" check, or by killing the whole scan (walker
+ workers) as a unit via a thread group.

### 2.2 Move the replace paths off the main thread

`EffStageReplace` / `EffReplaceOnDisk` should follow the pattern that already
exists for search: fork, stream progress over `searchQ`, and let
`stageReplaceDone` / `replaceDone` land as messages. The editor already has a
spinner (`edLoading`) and a "Replace All finished" notification path; this is
mostly plumbing, and it removes the only unbounded main-thread stall in the
program.

### 2.3 Cancellable loads

Give `JLoad` the same treatment: opening a different file while a large one is
loading kills the reader. Guard the queue with a generation so a
just-completed-but-superseded outcome is dropped rather than installed.

### 2.4 Bound the input queue's worst case

`readerLoop` writes to an unbounded `TQueue`. The main loop drains it fully per
iteration, so it cannot grow without bound in normal use — but a bracketed
paste of a very large payload arrives as a single `KPaste !Text` (good, that
was designed for), while a *terminal replaying* a huge scroll of mouse events
can queue tens of thousands of `KMouse` events. Coalescing already handles the
throughput; consider collapsing runs of drag events in `applyBatch` (keep only
the last of a consecutive drag run) so the pure model does not process 10 000
intermediate drags.

### 2.5 Counters

Every fork/kill bumps a counter exposed by `0006`. "Threads alive: 1" on the
status line is the cheapest possible proof that this plan works.

## 3. Testing

- **Cancellation.** Start a search over a large synthetic tree, immediately
  supersede it 20 times, and assert (via `0006`'s counters or a test hook)
  that at most one walker is alive shortly afterwards.
- **No wedge.** Assert `runScan` always reaches `scDone` or is cleanly killed —
  a randomised test that kills scans at random points and checks the driver
  never blocks.
- **Main-thread responsiveness.** With the PTY soak: trigger a Replace All over
  500 files and assert the editor still repaints (frames continue) during it.
- **fd/thread flatness** across a long soak — the check that catches the
  regression this plan prevents.

## 4. Risks

- `killThread` during `BS.hGetContents` leaves a half-read handle; it is closed
  by the `withBinaryFile` bracket in `readTextFile`, so this is safe — but
  every worker's file IO must be inside such a bracket. Audit the three walkers
  for this before enabling kills.
- Killing during `atomically (writeTQueue …)` is safe (STM transactions are
  atomic and abort cleanly).
- Do not kill the input reader or the main thread; `JobKind` deliberately has
  no constructor for them.
