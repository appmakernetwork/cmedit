# 0027 — Adaptive journal write-behind

**Theme:** performance — a feature whose cost is proportional to what it protects
**Status:** ✅ **RESOLVED** — implemented 2026-07-27
**Risk (as shipped):** low — the journal directory, the format, the recovery
decision and the derived-removal rule are all untouched; what changed is *when*
a pass runs and *which thread* runs it, and both new mechanisms fail towards
"the journal is written again in a moment" rather than towards a stale or
resurrected journal

---

## Resolved

Two changes, one pure and one structural, against the same finding: `0011`'s
write-behind rewrites the **whole buffer** of every stale modified document on
a fixed 2 s timer, and neither its cost nor its cadence knew anything about how
big that buffer was.

**The interval is now a function of the bytes a pass would write.**
`EditorDoc.journalDelayUs :: Int -> Int` maps "bytes this pass would write" to
"minimum spacing between passes": `bytes / journalBudgetBps`, floored at the old
2 s and ceilinged at 30 s. `journalPendingBytes` supplies the input — the summed
size of the *stale* documents, from the buffer's own `bufChars` plus one
separator per line, so it is O(open documents) and never serialises anything to
find out what serialising would cost. Every ordinary file (anything up to 4 MB)
lands exactly on the 2 s floor and behaves precisely as it did before.

**The pass runs on a background thread** (`startJob JJournal`), and the fork
happens *before* the serialisation, which is the larger half of the cost:
`journalRequests` hands over unforced `Journal` records and the writer thread is
what forces them.

### The interval, and why it is a rate floor rather than a longer debounce

The obvious implementation — make the debounce itself size-dependent — is
wrong, and wrong in the direction that silently removes the feature. A debounce
is *replaced* by every keystroke; that is what a debounce is. At 2 s that is
harmless, because a 2 s gap in typing is common. At 20 s it means a user typing
steadily into a large file re-arms the timer forever and **nothing is ever
journalled** — the file the feature was most needed for would be the one file it
never protected.

So the long interval is applied the way `lintMinIntervalUs` already applies one:
as a floor under the *rate*, with the debounce left at 2 s. `maybeArmJournal`
now has three rules:

* the first write after a quiet stretch still lands one 2 s debounce after the
  last edit, so a 40 MB buffer is protected as promptly as a 4 KB one;
* a timer that is already pending is **not** pushed out by further typing, so
  continuous typing cannot starve the write;
* a fresh timer is armed no sooner than one interval after the previous pass
  started (`drvJournalLast`, the twin of `drvLintLast`).

The measured consequence is the point: under continuous typing the journal is
now written *more* reliably than before (every 20 s, where a pure debounce wrote
nothing at all), while writing five times less.

### The budget, and the two clamps

`journalBudgetBps` is **2 MB/s**. That is roughly one ordinary save spread over
a second: enough that the journal is never the busiest thing on the disk, small
enough to disappear against anything else the machine is doing, and — since the
journal exists for laptops, SSH sessions and network home directories — small
enough not to matter on a slow one.

Both clamps are promises rather than tuning:

* the **2 s floor** is what a journal *is*. It is the statement about how much a
  crash can take, and no size argument may weaken it for files where the cost
  was never a problem.
* the **30 s ceiling** is the same promise from the other end. Past half a
  minute the feature starts failing at its own job, so the very largest buffers
  give the budget back instead of widening the loss window further. The largest
  file that opens as text at all is `maxOpenBytes` = 100 MB, which lands at the
  ceiling and costs 3.3 MB/s — over budget, deliberately, and still a third of
  what a 40 MB buffer cost before this change.

No config key. The budget is not a preference: a user cannot be expected to
have an opinion about their editor's cache write rate, and `journal = off`
already covers the one opinion they might have.

### Off the event-loop thread, and the two races that come with it

`writeJournals` measured at **102 ms** for a single 40 MB document on tmpfs and
**141 ms** on an ordinary ext4 home directory (below), of which the file write
is only 13–20 ms; the rest is flattening the buffer into one `Text`, encoding it
and copying it into a strict `ByteString`. `0011` §4 said journal writes must
never show up as a latency spike, and at that size, on the event-loop thread,
they do.

Backgrounding a write that another thread is allowed to *delete* is where the
care goes. Both races are closed by one shared **`drvJournalGate :: MVar Bool`**,
held only around a rename and a couple of set operations — never across the
serialisation or the bulk write, so the event loop can never wait on it for more
than microseconds:

1. **The document is saved while its journal is being written.** `sweepJournals`
   runs on the event-loop thread and deletes journals whose document is no longer
   in `journalLiveKeys` — but it cannot delete a file that does not exist yet, so
   a naive fork lets a write land *after* the sweep and leave a journal for a
   saved document, which the next startup would offer to "recover".
   **The mechanism: the writer re-checks `journalLiveKeys` under the gate,
   immediately before the rename**, and discards its temp file instead if the
   answer changed. Sweep and rename exclude each other, so whichever runs second
   sees the other's work — rename-then-sweep leaves the name in `drvJournals`
   for the sweep to delete, and sweep-then-rename finds the document no longer
   live. (This is also why `sweepJournals` may keep its unlocked
   `Set.null`-fast-path: missing a concurrent insert there is precisely the case
   the writer's own check handles.)
2. **The session quits while a journal is being written**, putting a journal back
   after `dropJournalsOnExit` removed it. The gate's `Bool` is that: quitting
   closes it under the same lock, and a writer that finds it closed discards.

A third hazard is not a race but an accounting error, and it is the reason
`journalsWritten` changed shape. It used to take journal keys and settle each
document's `docJournalSeq` to its *current* `docDocSeq`. With an asynchronous
write that is a lie: an edit landing during the write would be marked journalled
and never written. The counter now travels with the request
(`journalRequests :: Editor -> [(FilePath, Int, Journal)]`) and comes back with
the completion (`journalsWritten :: [(FilePath, Int)] -> Editor -> Editor`), so
such a document simply stays stale and the next tick writes it.

Completion travels back over the search queue as `SMJournal`, like every other
background result; only the one-off "cannot write the journal" note repaints.
Passes are single-flight — a `JournalTick` that finds one in flight re-arms
rather than starting a second writer over the same temp files, exactly as
`LintTick` defers on its rate floor.

### Measurements

All through a PTY against the real binary, typing into a generated 40 MB text
file, in a burst-and-pause rhythm (1.2 s of typing at ~10 cps, then a pause) —
the pattern that makes a 2 s debounce fire once per cycle. Journal rewrites are
counted by polling the `.cmj`'s `mtime_ns`, so the traffic is what actually
reached the filesystem.

**Steady-state write traffic, 40 MB buffer:**

| | writes | gap between writes | traffic |
|---|---|---|---|
| before | 10 in 40 s | 3.7 s | **9.99 MB/s** |
| after | 3 in 60 s | 20.0 s | **2.00 MB/s** |

**Steady-state write traffic, 200 KB buffer (the control):** 8 writes in 30 s,
3.7 s apart, 0.05 MB/s — **identical, byte for byte and write for write, before
and after**. That is the floor doing its job.

**One journal write, by buffer size** (single write per process run, so nothing
is shared with a previous pass — `bench/README.md`'s trap 2):

| buffer | flatten + serialise | write + rename | total |
|---|---|---|---|
| 1 MB | 4.5 ms | 0.3 ms | 4.9 ms |
| 4 MB | 8.0 ms | 1.5 ms | 9.5 ms |
| 10 MB | 24.7 ms | 3.2 ms | 27.9 ms |
| 40 MB | 88.8 ms | 13.0 ms | **101.9 ms** |
| 10 MB → ext4 `$HOME` | 31.3 ms | 4.6 ms | 35.9 ms |
| 40 MB → ext4 `$HOME` | 120.8 ms | 20.2 ms | **141.0 ms** |

The serialisation is 85–87 % of it, which is why the fork precedes it. (The
first four rows write to tmpfs, which understates the write; the last two are an
ordinary disk. A network home directory would be worse than both, and is exactly
who this feature is for.)

**Keystroke → first-output latency, 40 MB buffer, continuous typing** (which the
new arming makes the interesting case: the write now fires *during* a burst
rather than in the gap after it, because a pending timer is no longer pushed
out). Two builds, identical but for the fork; both saw 4 journal writes in the
run:

| | median | p95 | p99 | max | samples > 30 ms | > 60 ms |
|---|---|---|---|---|---|---|
| write on the loop thread, run 1 | 0.5 ms | 1.3 ms | 1.6 ms | **96.4 ms** | 4 | 3 |
| write on the loop thread, run 2 | 0.6 ms | 1.3 ms | 1.5 ms | **92.5 ms** | 4 | 3 |
| background thread, run 1 | 0.7 ms | 1.6 ms | 1.9 ms | **31.9 ms** | 1 | 0 |
| background thread, run 2 | 0.6 ms | 1.5 ms | 1.8 ms | **25.5 ms** | 0 | 0 |

Four writes, four stalls, one per write, reproducibly — and the fork removes
them: nothing above 60 ms, and at most one sample above 30 ms in 800
keystrokes. What is left at ~26–32 ms is not the write but the collection that
follows allocating and dropping a 40 MB `ByteString`, which the background
thread still triggers. That is a smaller, rarer problem than the one being
fixed, and it is the RTS's to schedule; it is recorded here rather than chased.

For the old binary the same probe on the old *burst-and-pause* pattern showed
0.6 ms median against a 22.8 ms maximum over 432 keystrokes — a collision is
hard to arrange deliberately there, because every keystroke cancels the pending
debounce, so the write almost always lands in a gap where nobody is waiting for
it. That is the honest shape of the "before" spike: rare, but 100 ms when it
happens, and *guaranteed* to happen once the interval change makes writes fire
mid-burst.

Verification: the full suite passes with 16 new checks (the interval's floor,
ceiling, monotonicity and budget; the 100 KB and 40 MB bands; the pending-bytes
estimate and its config gate; and the counter semantics that keep an edit made
during a write from being lost). `docs/plans/bench/pty_journal.py` passes 4/4
**unchanged** — its files are small, so they stay in the 2 s regime, which is
the property that made the floor worth having.

---

## 1. Why

`0011` §2.2 sized the write-behind honestly for the files it was imagining: "at
most one write every 2 s of active typing, of a few KB to a few hundred KB —
negligible next to what the linter already does". Both halves of that stop being
true at scale, and the reason is that a journal is not a delta — it is the whole
buffer, by an explicit and correct decision (§2.1: "simple, verifiable, and
buffers are small next to the write budget below").

The buffers are not always small. A 40 MB log or CSV is an ordinary thing to
open in this editor — the paged viewer exists for the files past that — and for
one of those the same 2 s tick means ~10 MB/s of writes to `~/.cache`, measured,
for as long as the session lasts. Nothing is wrong; nothing is even slow; there
is simply a background process rewriting 40 MB every few seconds because a
person is typing.

And each of those writes was 100–140 ms of work between two frames, which §4 of
the same plan had already ruled out ("journal writes must not show up as a
latency spike").

## 2. Design

### 2.1 A pure interval function

```haskell
journalDelayUs :: Int -> Int          -- bytes a pass would write -> µs
journalDelayUs bytes
  | bytes <= 0              = journalMinDelayUs
  | bytes >= atCeilingBytes = journalMaxDelayUs      -- before the multiply,
  | otherwise               = max journalMinDelayUs  -- so it cannot overflow
                                  ((bytes * 1000000) `div` journalBudgetBps)
```

with `journalMinDelayUs` = 2 s, `journalMaxDelayUs` = 30 s and
`journalBudgetBps` = 2 MB/s. Monotonic, clamped at both ends, and testable
without a terminal or a filesystem — which is where its coverage lives.

The input is `journalPendingBytes`, the summed size of the documents whose
journals are stale, taken from `bufChars` and the line count. It is an estimate
on purpose: one character counted as one byte is exact for the ASCII that
dominates source and log files and an under-count for anything else, and the
only way to be exact is to serialise the buffers — which is the work being
scheduled. A CSV's estimate comes from the (stale) buffer under its table rather
than the grid, which is the same order of magnitude, and an order of magnitude
is all a timer needs.

### 2.2 Rate floor, not debounce

See "why it is a rate floor" above. In one line: a debounce that a keystroke can
replace cannot be used to *space* anything, because typing is what it defers to.

### 2.3 The write off the event-loop thread

`startJob JJournal`, single-flight, fork before serialisation, completion over
the search queue, and `drvJournalGate` covering the two races and every mutation
of `drvJournals`. See "Off the event-loop thread" above for the full argument;
the mechanism is stated in the code at `writeJournals` and in `CLAUDE.md`'s
sixth journal invariant.

## 3. What did not change

- The journal **format**, the file naming, `classifyJournal`, the startup GC,
  the recovery dialog and everything it installs.
- **Derived removal.** `journalLiveKeys` is still the only authority on which
  journals should exist, and the sweep still runs after every batch. The writer
  consulting the *same function* before renaming is that rule applied one level
  down, not an exception to it.
- **It still cannot endanger the real file.** Everything remains inside
  `~/.cache/cmedit/journal`, temp-file-then-rename, every operation inside a
  `try`, one status note on failure. The new thread writes to exactly the same
  two paths the old code did.
- **The 2 s promise for ordinary files**, which is why the control measurement
  above matters more than the headline one.

## 4. Non-goals

- **A config key for the budget.** The right value is not a matter of taste, and
  a key would be one more thing to get wrong; `journal = off` remains the only
  switch.
- **Incremental journals (a delta format).** That is the other way to make this
  cheap, and it would be a different feature: it trades the property that makes
  recovery trustworthy — a journal is a complete, independently verifiable copy
  of the buffer — for write volume. The interval buys most of the same win
  without touching what recovery has to trust.
- **`fsync`.** The journal is best-effort by design; a crash that beats the page
  cache to disk is the same crash that beats the 2 s debounce.
