# 0006 — Session observability: make long-running behaviour visible

**Theme:** diagnosing what a benchmark cannot reproduce
**Status:** ✅ **RESOLVED** — implemented 2026-07-26 (TSV log mode and a
Settings row deliberately dropped; see below)
**Risk:** low — off by default

## Resolved

**`--stats-on-exit`** prints a session summary to stderr after the terminal is
restored. Real output from a short session:

```
cmedit: session 0h00m01s, 31 keys, 31 frames (avg 2.71ms, max 7ms)
        live 0 MB, peak 0 MB, allocated 47 MB, 1 major GCs (11ms total)
        0 files opened, 0 background jobs (0 cancelled), 0 lint passes, 0 idle collections
```

That is the artefact to attach to a bug report about a long session, and it
needs no UI. The memory figures come from `GHC.Stats`, which works because
`0007` put `-T` and `-rtsopts` in the shipped RTS options.

**`debug-stats = on`** (config key, off by default) puts live counters on the
status bar — `f 2.9/3ms 0MB j0`: average and worst frame time, live heap,
background jobs started. Verified through a PTY.

**It arms no timer.** The refresh rides the existing 2-second filesystem
freshness poll (`refreshStatsLine`, which returns whether anything changed so
the loop only repaints when it must). With the key off the whole feature is one
`Bool` test every two seconds — the "About box that stops ticking" discipline,
without adding a fourth software timer to the event loop.

**Counters** (`DrvStats`) are `IORef` bumps at a handful of sites: keys applied,
frames rendered plus total/worst frame time, background jobs started and
cancelled, lint passes, idle collections, files opened. Two bumps per frame.

**Guards** (suite 2 208 → 2 214): the status segment appears only when set, and
— the part that could actually break something — the status bar's *click zones*
still resolve to the same zones, shifted by exactly the segment's width, so
clicking Ln/Col or the encoding still hits the right thing with counters on.
Plus the config key parses and defaults off.

## Dropped from the plan

- **The `log` mode** (TSV to `~/.cache/cmedit/stats.log`). The tri-state
  `off|status|log` became a plain `on|off`: for plotting a session, `+RTS -hT`
  already produces a far richer series (and `0007` made it available on the
  shipped binary), so a bespoke log format would be a second-rate duplicate.
- **A Settings-dialog row.** `settingsSpec` is positional and pinned by a test
  that keeps `applySettingRow` in sync; adding a developer-diagnostic row to a
  user-facing dialog is not worth that coupling. The config key is the interface.
- **Per-document/undo-depth fields** in the status segment. Kept to a fixed,
  narrow set so the segment cannot grow to displace the zones beside it.

The plan below is the original analysis, kept for the record.

---

---

## 1. Why

A six-hour session's problems — a slowly growing heap, a frame that got 3×
slower after opening a particular file, a linter that never stops respawning —
cannot be reproduced on demand, so they have to be *observed in place*. Today
cmedit exposes nothing: no counters, no timings, no heap figures. Every claim
in the other plans is checked by a benchmark, which is a different program
under different conditions.

The cost of adding this is small, and the payoff is that "it feels slower after
a while" turns into a number.

## 2. What to expose

A single config key, defaulting to off:

```
debug-stats = off | status | log
```

- `off` — nothing collected, one `Bool` test per key batch.
- `status` — a compact block on the right of the status bar, refreshed once a
  second: `f 1.8/7.2ms  ⌷ 42MB  ↯3  ⧗1`
  (frame p50/p99, live heap, background threads, in-flight external processes).
- `log` — the same record appended as TSV to `~/.cache/cmedit/stats.log` once a
  second, with a header line, so a session can be plotted afterwards.

Fields worth carrying (all cheap):

| Field | Source |
|---|---|
| frame time p50 / p99 (1 s window) | `getMonotonicTime` around `renderNow` |
| bytes emitted per frame | length of the Builder (already computable) |
| live heap, total allocated | `GHC.Stats.getRTSStats` (needs `-T`) |
| major/minor GCs, max GC pause | `getRTSStats` |
| undo depth of the active doc, total across docs | pure query |
| open documents, total buffer lines/chars | pure query (`bufChars` is O(1)) |
| background threads started/alive | counters in `Drv` |
| external processes started/killed/timed out | counters in `Drv` (see `0004`) |
| search/def/quick generations, results held | `edSearch` etc. |

## 3. Design

- **Collection lives in the driver.** A `DrvStats` record of `IORef`s in
  `Drv`; `renderNow` records frame timing; the fork sites bump counters. The
  pure model contributes only via queries it already has.
- **The status-bar view is pure.** `statusRightInfo` already builds the right
  text and its click zones in one place; add an optional stats segment there,
  fed by a `Maybe StatsSnapshot` field on `Editor` that the driver refreshes
  once a second. That keeps rendering pure and testable and avoids threading
  IO into `Render`.
- **`-T` is required for the heap figures.** Rather than always enabling it,
  read `getRTSStatsEnabled` and show `—` for those fields when it is off; then
  document that `cmedit +RTS -T` (or a build with `-with-rtsopts=-T`, see
  `0007`) unlocks them. `-T` itself is nearly free — it is the same counters
  the RTS keeps anyway — so enabling it by default in the shipped binary is a
  defensible choice, decided in `0007`.
- **A one-second cadence**, driven by the existing `registerDelay` timer
  machinery in the event loop (the same pattern as `fsPollDelayUs`), and only
  armed when `debug-stats /= off`. A settled editor with stats off arms no new
  timers — the "About box that stops ticking" discipline already established.

## 4. A second, sharper tool: `--stats-on-exit`

A CLI flag that prints a session summary to stderr after the terminal is
restored:

```
cmedit: session 4h12m, 84 231 keys, 61 044 frames (p50 1.8ms p99 9.4ms),
        live 38MB, peak 61MB, alloc 214GB, 12 major GCs (max pause 41ms),
        18 files opened, 3 141 lint passes (7 timed out), 2 searches
```

This is the single most useful artefact for a bug report about a long session,
it needs no UI, and it is ~30 lines on top of the counters above.

## 4b. The zero-cost diagnosis recipe (document this, it already works)

GHC's cost-centre profiler needs a profiling build, and the profiling
libraries are **not installed** in this environment (`ghc --make -prof` fails
on `text-2.0.2`), so a `make prof` target cannot be assumed to work. Two tools
do work on the ordinary `-O2` binary and should be documented in
`CONTRIBUTING.md`:

- **`cmedit +RTS -hT -i5`** — a heap census by closure type every 5 s, written
  to `cmedit.hp`. This is what identified the undo-history leak: `UndoState`
  and `THUNK_1_1` appearing in identical byte counts, ~65× above the nominal
  cap. It needs only `-rtsopts` (see `0007`).
- **`cmedit +RTS -s`** — the one-line summary (allocation, GC counts, max
  pause, peak RSS) after a real session.

Both require `-rtsopts` at link time, which is the main reason `0007` proposes
adding it. A short "how to diagnose a slow/heavy session" section pointing at
these two flags is worth more than any amount of speculative optimisation.

## 5. Testing

- The counters are `IORef Int` bumps; a unit test can drive `renderNow`'s
  timing helper directly.
- The status segment is pure: assert its rendering for a few snapshots,
  including the "stats disabled" fallback.
- Assert that with `debug-stats = off` no extra timer is armed — a structural
  test on the event loop's timer decision function, which should be factored
  into a pure `wantStatsTick :: Editor -> Bool` for exactly this reason.

## 6. Risks

- Scope creep into a profiler. Keep it to counters that are O(1) to read and a
  fixed-size status segment. Anything requiring a traversal (e.g. total heap
  by document) does not belong here.
- Writing a log file in `~/.cache` must fail silently (a read-only home is not
  an error worth interrupting an edit session for), matching how
  `EffSaveConfig` already degrades to a status-line message.
