# 0024 — Input-stream journal and deterministic session replay

**Theme:** capability — making "the input stream is the whole truth" a shipped
fact instead of an architectural aspiration
**Status:** proposal — read together with `0011`, which it complements and
does **not** replace (§2)
**Estimated effort:** 4–6 days (the driver tap and replay harness are the
bulk; the serialisation is mechanical)
**Risk:** medium (the risk is *discovering* nondeterminism, which is also the
point; nothing here touches the save path)

---

## 1. The claim being cashed in

Every input the pure model ever sees arrives through enumerable funnels: the
key queue (`applyBatch` — keys, resize, focus, terminal replies), `loadQ`
(`LoadOutcome`s), `searchQ` (the `SM*` messages: search/definition/lint/
quick-open results), and the timer firings the loop turns into pure calls
(`tickLoading`/`tickImage`/`tickAbout`). `update` is deterministic and the
driver callbacks (`setLoaded`, `searchFileFound`, `pagerFilled`, …) are pure.
Therefore: **journal those messages plus the initial conditions, and the
journal *is* the session** — replaying it through the same pure code
reconstructs the exact `Editor`, unsaved edits, view state, zipper and all.

No mainstream editor can do this; they all keep mutable state outside the
event stream. Here it is a tap and a serialiser. What it buys, in value
order:

1. **Bug reproduction.** "Attach `~/.cache/cmedit/session.cmj` to the
   report" turns any interactive bug — including the ones that only appear
   after an hour of use — into a deterministic test case. This is the
   headline.
2. **Replay-driven testing.** The soak harness (`0005`) and the PTY tests
   gain a third mode: replay recorded real sessions and assert invariants
   (memory flatness, no crash, final-state goldens) over *actual* usage
   patterns instead of synthetic ones. A crash found by fuzzing arrives
   pre-reduced: binary-search the journal prefix.
3. **Exact session restore** — but see §2 for why this is the *last* item,
   not the first.

## 2. Relationship to `0011` (this section is the design decision)

`0011` journals **content snapshots** for crash recovery. It should ship
first, and this plan must not absorb it, for one hard reason: **a journal is
only replayable by the binary that recorded it.** Any change to `update`'s
behaviour — i.e. nearly every release — invalidates replay of older
journals. Content snapshots have no such coupling; they are the right
*safety* mechanism precisely because they are dumb. So the division is:

- `0011`: crash safety of *content*. Simple, version-proof, user-facing.
- `0024`: record/replay for *diagnosis and testing*, plus exact restore as
  an opportunistic bonus **when the version matches** (journal header
  carries the exact version string; mismatch ⇒ fall back to `0011`'s
  snapshots, which cover the data that matters).

They share nothing but a cache directory, and `0024` becomes dramatically
cheaper to trust once `0023`'s determinism regression test exists — build in
that order.

## 3. What is journaled

An append-only file, one record per line (`show`-escaped like the persisted
find history, so multi-line payloads survive), rotated per session:

- **The prologue:** version string, CLI args, the config *text* as loaded
  (not the parsed struct — parsing is part of what's being replayed),
  initial terminal size, and the RTS-visible environment the model consumes
  (today: nothing else — keeping it that way is §5's audit).
- **Every key batch** as consumed (post-coalescing, pre-`update` — the exact
  list `applyBatch` receives, so replay and live agree on batching).
- **Every `loadQ` and `searchQ` message.** `LoadOutcome` embeds file
  content, which is the honest cost: the journal contains what you opened
  (see §6, privacy; and a `maxJournalBytes` cap — 256 MB — after which
  recording stops with a status note, never blocks the editor).
- **Every timer firing**, as an explicit record — replay must not re-derive
  timing, it must re-play it.
- **Synchronous startup loads** routed through the same `LoadOutcome`
  capture (they already produce one; the tap is at the single point where
  outcomes are applied).

And, for verification rather than replay: a hash of each `[Effect]` list
`update` emits (§4).

## 4. Replay and the divergence check

A `--replay FILE` mode (and a Spec-side harness sharing the core): a stub
driver that performs **no IO**, feeding journaled records through the same
`applyBatch`/callback code, with every `perform` replaced by "compare
against the journal". That comparison is the killer feature: at each step,
the effects the replayed `update` emits are hashed against the recorded
hash. **First mismatch = the exact step where behaviour diverged** — from
nondeterminism (a bug in this plan's premise) or from a code change (which
turns replay into a behavioural-diff tool between versions: replay an old
session against a new binary and see precisely which keystroke now behaves
differently). Output modes: final-state dump for goldens, `--replay-to N`
prefix replay for bisection, and headless full-speed (no terminal needed —
rendering is pure and can be skipped or sampled).

## 5. The nondeterminism audit (a deliverable, not a risk)

The premise holds only if the pure model consumes nothing outside the
stream. The audit enumerates and closes the gaps, and then a guard keeps
them closed:

- `Date`/clock: the model gets times only via messages (`DiskTime`s inside
  outcomes/messages) — verified by grep, then **pinned by
  `make lint-invariants`** (the `0020` machinery): no `getCurrentTime`/
  `Data.Time` acquisition outside `App`/platform.
- Randomness: none today; same pin.
- Iteration order: `containers` maps are deterministic; hash-order issues
  don't exist (no hashmaps in boot libs — the constraint pays a dividend).
- The one subtle spot: anything the *driver* computes from the world and
  hands to callbacks (canonicalised paths, `statEntry` results) — all of it
  already travels inside the journaled messages, but the audit's job is to
  prove that by listing every callback's argument sources.

The `0023` determinism property test then holds the line per-release.

## 6. Recording policy, privacy, bounds

Off by default; three switches: `--record` (this session), config
`journal-input = on` (always, for the soak farm / the adventurous), and a
crash-ring mode — record to a bounded ring (last `journalRingBytes`, 32 MB)
so "it crashed, send the tail" works without unbounded growth. Journals
contain **file contents and every keystroke** (passwords typed into files
included): directory `0700`, a loud line in the manual and `--help`, never
any automatic upload of any kind, and the status bar shows `⏺ journal` while
recording (the `REC` zone pattern from `0023`). A journal-write failure is a
status note, never a block — recording must be unable to hurt the session it
observes.

## 7. Testing

- **Round-trip:** every record type serialises/parses identically (property
  test over generated `Key`s/messages — `KUnknown` payloads, wide chars,
  embedded newlines in `LoadOutcome` text).
- **The identity theorem:** record a scripted PTY session, replay it, assert
  the final `Editor` equals the live one field-for-field (buffer, zipper,
  cursor, view modes) — the whole plan in one test.
- **Divergence detection detects:** replay against a deliberately patched
  `update` and assert the mismatch is reported at the right step.
- **Truncation:** a journal cut at every byte boundary of its last record
  replays its valid prefix and reports the cut cleanly (crash journals are
  *always* truncated).
- **Overhead:** with recording on, p99 frame time and idle allocation
  unchanged (`0005`/`0006` harnesses) — the tap is an append to a buffered
  handle, flushed on the idle timer, `fsync` never.

## 8. Non-goals

- **Not crash recovery of content** — that is `0011`, which ships first.
- **Not cross-version replay.** Version mismatch is detected and refused
  with a clear message; the behavioural-diff use (§4) is opt-in via a flag
  that acknowledges divergence is expected.
- **Not collaborative/OT anything**, and not a macro format — `0023`'s
  macros stay plain `Seq Key`, though they are this journal's vocabulary.
