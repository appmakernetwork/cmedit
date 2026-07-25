# 0004 — Single-flight linting: stop external-process pile-up in long sessions

**Theme:** long-session resource stability (processes, fds, CPU)
**Status:** ✅ **RESOLVED** — implemented 2026-07-26 (process-group kill tried
and rejected; see below)
**Risk (as shipped):** low — driver-only, no pure-model change

## Resolved

Measured with a deliberately slow fake linter (4 s per run) on `PATH`, typing
in bursts with ~0.9 s pauses for 13 s and sampling the process table:

| | Before | After |
|---|---|---|
| lint passes started | 9 | **5** |
| **peak concurrent linter processes** | **5** | **1** |
| orphaned processes after exit | 0 | 0 |

Three changes:

1. **Single flight with pre-emption.** `Drv` carries the in-flight pass
   `(generation, ThreadId)`; `startLintRun` kills the previous runner under
   `mask_` before forking the new one, and the runner clears its own slot in a
   `finally` (only if the slot still names it — a newer pass may already have
   replaced it). `runToolCapture` catches the async exception and terminates its
   child, so cancellation reaches the external process rather than orphaning it.
2. **A whole-pass timeout** (`lintPassTimeoutUs`, 20 s) on top of the existing
   per-tool one, so a file matching three linters cannot hold a slot for 3 × 10 s.
3. **A rate floor** (`lintMinIntervalUs`, 2 s). The debounce alone had no rate
   limit — typing in 600 ms bursts started a pass per burst. A pass that would
   start too soon is *deferred*, not dropped: the timer re-arms and it runs when
   the floor lifts. `EffLintNow` (save) deliberately bypasses the floor, but
   records the time so the debounced path does not immediately follow it.

**Rejected: killing the child's whole process group.** Several linters ship as
shell wrappers, and terminating the wrapper leaves its `sleep`/`node` grandchild
running (observed once, in the first iteration of this work). The obvious fix —
`create_group = True` plus `interruptProcessGroupOf` — was implemented and
backed out: with `create_group` off the signal can reach the *caller's* process
group, and during testing it killed the harness's own shell. Signalling a
process group from inside an editor that shares a terminal with the user's shell
is not a risk worth one narrow case. The comment in `runToolCapture` records
this so it is not retried blind.

**Observability counters (§4)** belong to `0006` and are tracked there.

The plan below is the original analysis, kept for the record.

---

---

## 1. The problem

`App.startLintRun` forks a runner unconditionally:

```haskell
startLintRun drv req = do
  gen <- atomically $ do modifyTVar' (drvLintGen drv) (+ 1)
                         readTVar (drvLintGen drv)
  void $ forkIO (runLinters (drvSearchQ drv) (drvLintGen drv) gen req)
```

Nothing stops a second, third or tenth runner from starting while the first is
still executing. `runLinters` checks the generation only **between tools**
(`goRuns`), and `runToolCapture` gives each individual tool up to
`lintTimeoutUs` = **10 seconds**. The generation counter correctly makes stale
*results* be dropped — but it does nothing about stale *work*.

The pile-up is easy to hit in normal use:

- debounce is 500 ms of quiet (`lintDebounceUs`);
- a real-world `eslint` on a large flat-config project, `pyright`, or a Yarn
  PnP bootstrap (`node -e` + module resolution) routinely takes 2–8 seconds;
- a user who types a burst, pauses, types a burst, pauses… gets one new runner
  per pause.

With an 8-second tool and a pause every second, up to 8 concurrent linter
processes are running, each handed the whole buffer on stdin, each with three
pipes and three GHC threads (feeder, stderr drainer, waiter). Over a working
day this shows up as: the machine's fans, a stale-diagnostics flicker as
out-of-order results land (mitigated by the gen check, but only after the work
is done), and — on a Yarn PnP project — several `node` processes each resolving
a module graph.

Two smaller issues in the same area:

- **The 10 s timeout is per tool, not per pass.** A file matching three
  linters can occupy a runner for 30 s.
- **`EffLintNow` bypasses the debounce entirely** (save, Save All). Saving all
  20 modified documents fires 20 immediate passes; `savedAll` emits
  `EffLintNow` once, but a "save all + keep typing" pattern still stacks on
  top of whatever the debounce started.

## 2. Design: one lint pass in flight, cancellable

Add to `Drv`:

```haskell
, drvLintRun :: !(IORef (Maybe (Int, ThreadId)))   -- ^ (generation, runner) currently in flight
```

`startLintRun` becomes single-flight with pre-emption:

```haskell
startLintRun drv req = do
  gen <- atomically $ do modifyTVar' (drvLintGen drv) (+ 1); readTVar (drvLintGen drv)
  -- Kill the superseded runner: its result would be dropped anyway, and its
  -- child processes are pure waste. killThread unwinds runToolCapture's
  -- withCreateProcess bracket, which terminates and reaps the child.
  old <- atomicModifyIORef' (drvLintRun drv) (\o -> (o, o))
  forM_ old $ \(_, tid) -> killThread tid
  tid <- forkIO (runLinters … `finally` clearRun drv gen)
  writeIORef (drvLintRun drv) (Just (gen, tid))
```

The key property that makes `killThread` safe here: `runToolCapture` already
runs the child under `withCreateProcess`, so an async exception unwinds
through `cleanupProcess`, which closes the pipes, sends `SIGTERM` and reaps
the child on a helper thread. The two helper threads (stdin feeder, stderr
drainer) already swallow their own exceptions and die when their handles
close. So cancellation needs **no new bracket** — only that the runner is
interruptible, which it is (it blocks in STM/IO the whole time).

Additional hardening, all cheap:

1. **Whole-pass timeout.** Wrap `goRuns` in a single `timeout
   lintPassTimeoutUs` (say 20 s) in addition to the per-tool one, so a
   three-linter file cannot hold a slot for 30 s.
2. **Mask the launch.** `forkIO` + `writeIORef` has a tiny window where a kill
   could target a stale id. Use `mask_` around the fork/record pair.
3. **Skip identical work.** Fingerprint the *content actually sent* (path +
   `edEditSeq` is already the debounce fingerprint; add it to the in-flight
   record) and return early if the in-flight pass is for the same fingerprint —
   e.g. `EffLintNow` firing right after a debounce pass for the same state.
4. **Serialise, don't parallelise, the tools of one pass.** Already the case
   (`goRuns` is sequential); document why, so nobody "optimises" it into a
   `mapConcurrently` and re-creates the pile-up inside a single pass.

## 3. A second look at the debounce

The current debounce re-arms whenever the fingerprint changes and fires 500 ms
after the last change. That is correct, but it has no *rate limit*: a user
typing in 600 ms bursts triggers a pass per burst. Add a floor:

```haskell
-- Never start a pass within lintMinIntervalUs (2 s) of the previous one
-- finishing; the pending request is coalesced and fires when the floor lifts.
```

This is the standard debounce+throttle pair, and it converts "one pass per
typing pause" into "at most one pass every 2 s", which for an editor is
indistinguishable in feel and strictly less work.

## 4. Observability (small, worth it)

Long-session problems are hard to believe without evidence. Add a hidden
diagnostic the maintainer can turn on:

- a config key `debug-stats = off|status|log` that, when set, puts a compact
  counter block on the status line or appends it to
  `~/.cache/cmedit/stats.log`: forks started/killed, passes completed/timed
  out, in-flight count, `getRTSStats` live bytes, and frame time p50/p99.
- It costs nothing when off (one `Bool` check per key batch) and makes every
  other plan in this directory verifiable in a real session rather than a
  benchmark.

This deserves to be its own small piece of work — see
`0006-session-observability.md`.

## 5. Testing

- **Unit (pure).** `lintRequest` gating is already pure; add cases for the new
  "same fingerprint in flight" early return by factoring the decision into a
  pure function `shouldStartLint :: Maybe InFlight -> LintFp -> Bool`.
- **Integration.** A test linter script (`/bin/sh -c 'sleep 5; echo …'`)
  installed via a temporary `PATH` in a test harness: fire five requests
  200 ms apart and assert (a) at most one child process exists at any moment
  (`pgrep -P` count, or count `SIGCHLD`s), and (b) exactly one `SMLint`
  message is posted, carrying the newest generation.
- **Manual soak.** Covered by the soak harness in `0005`: type continuously
  for ten minutes against a slow fake linter and assert the process count and
  RSS are flat.

## 6. Risks

- `killThread` on a thread blocked in a foreign call cannot interrupt until
  the call returns. `waitForProcess` on a `-threaded` RTS is a *safe* foreign
  call and is interruptible, so this is fine — but keep the `-threaded` flag
  requirement documented in the Makefile comment, since a non-threaded build
  would change this behaviour.
- Killing a runner mid-parse loses nothing: results are posted only at the
  end of a pass, and the generation check already discards them.
- Terminating a linter that is mid-write to its own cache (eslint's cache
  file, pyright's) is safe — these tools are designed to be interrupted — but
  it is worth a note in the code so it is a deliberate decision.
