# 0007 — RTS and GC settings for an interactive, long-lived process

**Theme:** latency tails and idle cost over a long session
**Status:** ✅ **RESOLVED** — implemented 2026-07-26 (the memory-return half;
the nursery-tuning idea was measured and dropped, and the capabilities question
is deferred — see below)
**Risk (as shipped):** low

## Resolved

**Result: RSS after opening *and closing* a 32 MB CSV went from 2 507 MB, held
for the life of the process, to 33 MB.**

Two changes, both needed — and the diagnosis in §2b below was only half right,
which is worth recording:

1. **An idle collection** (`App.idleGcDelayUs`, 30 s). The event loop carries a
   third software timer alongside the filesystem poll and the lint debounce.
   Every branch that does work re-arms it; when it fires it runs one
   `performMajorGC` and does **not** re-arm, so a session left open overnight
   costs exactly one collection. Without this nothing is ever collected while
   the user reads rather than types, because the mutator stops allocating and
   no GC is ever scheduled.

2. **`--disable-delayed-os-memory-return`** in the shipped RTS options. This is
   the part the plan got wrong: repeated `performMajorGC` calls alone returned
   *nothing* (measured: 8 collections, RSS unchanged at 2 106 MB), and neither
   `-Fd0` nor `-Fd1` helped. The reason is that GHC hands freed memory back with
   `MADV_FREE` by default, which leaves the pages counted in RSS until the
   kernel wants them. With `MADV_DONTNEED` a single collection took the same
   program from **2 065 MB to 16 MB**. So the memory was never *leaked* — but
   "2.5 GB resident" still trips container limits, alarms anyone reading `top`,
   and hides real growth.

`RTSOPTS` in the Makefile now applies to every shipped target (`all`, `small`,
`static`, `windows`): `--disable-delayed-os-memory-return -T -I2`, plus
`-rtsopts` so a user chasing a problem can add `+RTS -s` or `+RTS -hT` without
rebuilding — which is what makes `0006`'s diagnosis recipe usable on the
shipped binary.

Verified with `docs/plans/bench/pty_ratchet.py` (a 32 MB CSV, opened then
closed): 2507 MB → 2507 MB at 20 s → **33 MB** once the collection fires. A
49 MB text file that is still *open* stays at 165 MB, as it should, and typing
immediately after a collection is responsive.

**Deliberately not done:** the nursery/parallel-GC tuning of §2 (measured as
noise on the realistic workload) and the capabilities question of §4, which
needs the soak harness (`0005`) rather than a microbenchmark to answer.

The plan below is the original analysis, kept for the record — including the
`-Fd`/idle-GC reasoning that turned out to be incomplete.

---

---

## 1. Current state

`Makefile`: `ghc --make … -threaded -O2` — no `-with-rtsopts`, no `-rtsopts`.
The shipped binary therefore runs with GHC's defaults:

- nursery `-A4m` per capability;
- **idle GC on** (`-I0.3`): a *major* collection 0.3 s after the process goes
  idle;
- parallel GC on for `n > 1` capabilities — and `runTui` raises capabilities to
  `min 4 (cores-1)` at startup for the search worker pool;
- no RTS options accepted on the command line at all (no `-rtsopts`), so a user
  cannot even experiment.

For a program that is idle most of the time and must never stutter when it is
not, those defaults are not obviously right.

## 2. Measurements — and a correction

A first pass measured the nursery size on a benchmark that was dominated by
the quadratic lexer of `0002`, and showed `-A16m` ~9% ahead. **Re-measured on
the realistic workload** (100 full driver cycles per keystroke, 50×200, a
5 000-line `.js` file with ordinary 120-character lines, with the lexer fix in
place), the nursery size is *noise*:

| RTS options | 100 keystrokes |
|---|---|
| `-A4m` (default) | 205 ms |
| `-A8m` | 178 ms |
| `-A16m` | 187 ms |
| `-A32m` | 201 ms |
| `-A16m -qg` (serial GC) | 190 ms |
| `-A16m -qn2` (2 GC threads) | 200 ms |

The spread is under 15% with no monotone trend — run-to-run variance, not a
signal. **Do not ship a nursery change on the strength of these numbers.**

This is worth stating plainly because the first version of this plan
recommended `-A16m` on the basis of the earlier, contaminated run. The lesson
generalises: measure GC options *after* fixing the algorithmic problems, or the
algorithmic problem is what you measure.

What that leaves is the part of this plan that was never about throughput —
plus one genuine RTS finding that the microbenchmark could not have shown.

### 2b. The real RTS problem: memory is never returned to the OS

Driving the actual binary through a PTY and then leaving it idle:

| File opened | RSS at 1 s | at 3 s | at 25 s |
|---|---|---|---|
| 32 MB CSV | 701 MB | 2 503 MB | **2 504 MB** |
| 49 MB text | 163 MB | 163 MB | 163 MB |

The CSV parse's transient allocation (8.8 GB — see `0016`) inflates the heap to
2.5 GB, and **it stays there for as long as the editor is open**. The mechanism
is straightforward: GHC returns memory to the OS from major collections, and an
idle editor performs no allocation, so after the single idle GC fires there is
never another collection to return anything. The process sits on 2.5 GB
indefinitely.

**And closing the file changes nothing.** Same PTY session, sending Ctrl+W to
close the document and then typing into the empty buffer:

```
after open:      2504 MB
after close:     2504 MB      <-- the grid is now unreachable garbage
after typing:    2511 MB
after 10s idle:  2511 MB
```

The 2.5 GB is unambiguously dead — the document is closed — and the process
holds every page of it for the rest of its life. Generalised: **a long session's
RSS is the high-water mark of everything it has ever opened**, and it only ever
ratchets upward. Open a big CSV in the morning, close it, and the editor is
still 2.5 GB at 5 pm.

That is precisely the "long-running session" failure this plan set exists to
address, and it has two independent fixes:

1. **Fix the allocation** (`0016` for CSV, `0013` for save) so the peak never
   happens. This is the primary fix.
2. **Return memory when idle.** Two options worth measuring:
   - `-Fd<n>` (RTS): controls how aggressively the RTS returns memory after
     successive idle collections. Lowering it makes the return happen sooner.
   - An explicit `performMajorGC` from the event loop after a long idle period
     (say 30 s of no input, on a timer the loop already knows how to arm). This
     is a handful of lines, is fully under the editor's control, and costs
     nothing when the user is active. It also makes the heap census in `0006`
     meaningful.

Option 2 is the one to implement — it is small, testable and does not depend on
RTS-version behaviour. It belongs in the event loop next to `fsPollDelayUs`:

```haskell
-- After a long idle period, run one major collection so the RTS can hand back
-- the pages a big transient (a CSV parse, a workspace search, a closed
-- document) left behind. Without this the process keeps the high-water mark of
-- everything it has ever done: measured 2.5 GB still resident after opening
-- *and closing* a 32 MB CSV. Armed only when idle, so an active session never
-- pays for it.
idleGcDelayUs :: Int
idleGcDelayUs = 30 * 1000000
```

Validation is the `pty_ratchet.py` probe in `docs/plans/bench/`: the "after
close" line must drop back toward the empty-editor baseline (~23 MB).

## 3. Proposal

Bake a *small*, defensible set into the binary and let users override:

```make
RTSOPTS = -rtsopts "-with-rtsopts=-I2 -T"
```

- **No `-A` change.** See §2: unjustified by measurement on the realistic
  workload. Revisit only with a fresh measurement after `0003`, which changes
  the allocation profile substantially.
- **`-I2`** instead of the default `-I0.3` — an editor is idle constantly
  (every pause between keystrokes over 0.3 s). Idle GC exists to return memory
  and keep the heap tidy; at 0.3 s it fires *between words*. Two seconds keeps
  the benefit (a settled editor still compacts) while removing the
  type-pause-type-pause major-GC treadmill. Do **not** use `-I0` (off): a
  long-idle editor should hand memory back.
- **`-T`** — enables the RTS stat counters that `0006` reports. The overhead is
  effectively nil.
- **An idle `performMajorGC`** (see §2b) — not an RTS flag at all, but the
  actual fix for held memory; implement it in the event loop.
- **`-rtsopts`** — so a user chasing a problem can pass `+RTS -s` or `-hT`
  without a rebuild. (Note: `-rtsopts` on a setuid binary is a security
  consideration; cmedit is not setuid, so this is fine. Worth a Makefile
  comment saying so.)

Explicitly **not** proposed:

- `-qg` (disable parallel GC) / `-qn2`: within noise on the realistic
  workload (see §2), so there is nothing to buy.
- `--nonmoving-gc`: designed for large long-lived heaps with strict pause
  requirements; cmedit's live set is tens of MB once `0001` is fixed, and the
  nonmoving collector's throughput cost is not justified. Revisit only if a
  future feature (e.g. a workspace-wide index) pushes the live set past a few
  hundred MB.
- `-c` (compacting GC): same reasoning.

## 4. A related, larger question: capabilities

`runTui` sets `min 4 (cores-1)` capabilities permanently, for the benefit of
the search walker's grep pool. That means every GC is a parallel GC, with its
synchronisation cost, for the 99.9% of the session where there is exactly one
busy thread. Two options worth measuring:

1. Keep 4 capabilities but add `-qn2` (limit *GC* threads to 2) — decouples
   mutator parallelism from collector parallelism.
2. Start at 1 capability and raise to 4 around a search, dropping back when it
   finishes. `setNumCapabilities` is safe to call at runtime; the cost is a
   brief synchronisation, which next to a tree walk is nothing.

Measure both with the soak harness (`0005`) rather than the microbenchmark —
the effect is on pause distribution, not throughput.

## 4b. Honest accounting of what this plan is worth

After the correction, this plan is: **one diagnostic enabler (`-rtsopts`), one
cheap stats enabler (`-T`), and one idle-behaviour tweak (`-I2`) whose effect
has been reasoned about but not measured.** That is a modest, low-risk change
and it should be presented as such — its real value is unlocking `0006`'s
diagnosis recipe on the shipped binary, not making the editor faster.

The capabilities question in §4 remains genuinely open and genuinely
measurable; it is the part of this plan most likely to matter, and it needs the
soak harness (`0005`) rather than a microbenchmark to answer.

## 5. Testing

- Re-run the `0003` harness across the option set and commit the table to
  `docs/plans/baseline.tsv`.
- Add a p99 frame-time assertion to the soak harness with the chosen options,
  so a future flag change that regresses the tail is caught.
- Verify the packaged builds (`make small`, `make static`, `make deb`) carry
  the same options — `-with-rtsopts` must be added to every link line, and it
  is easy to update `all` and forget `static`.

## 6. Risks

- Any future `-A` change is real RSS: the nursery is per capability, so `-A16m`
  with 4 capabilities is 64 MB before a buffer is loaded. That cost needs a
  measured benefit, which §2 says does not currently exist.
- `-I2` is a *reasoned* change, not a measured one. If it is worth arguing
  about, measure it: idle a session with a large file open and count major GCs
  and CPU time over ten minutes with `-I0.3`, `-I2` and `-I0`.
- `-with-rtsopts` interacts with `-rtsopts`: options given on the command line
  override the baked-in ones, which is exactly what is wanted.
