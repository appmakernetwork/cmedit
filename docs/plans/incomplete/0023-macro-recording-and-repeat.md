# 0023 — Macro recording, playback, and repeat-last-edit

**Theme:** capability — the first feature that is *cheaper here than anywhere
else*, because the input stream is pure data
**Status:** proposal
**Estimated effort:** 2–3 days (macros); +1 day (repeat-last-edit)
**Risk:** low (pure model + one driver loop; no persistence, no format, no
new subsystem — and the worst failure is a macro that does the wrong edits,
which Undo already covers)

---

## 1. Why this architecture makes it near-free

A macro is a `Seq Key`. `Cmedit.Types.Key` is already a closed, serialisable
vocabulary — every keystroke, decoded and normalised, *before* it reaches any
logic — and `update :: Key -> Editor -> (Editor, [Effect])` is deterministic.
So recording is "append the key", playback is "apply the keys", and the
replay-fidelity bugs that plague macro systems in impure editors (hidden
state the replay misses, timing dependence) **cannot occur by construction**:
there is no state outside `Editor` for a replayed key to miss. This is also
deliberately the warm-up for `0024` — recording taps and replay discipline
built here are the journal's first customer.

## 2. Recording (pure model)

`edMacroRec :: Maybe (Seq Key)` and `edMacro :: Seq Key` (the last completed
recording) on `Editor` — **global** state like `edBrowser`/`edSearch`, not
per-document (a macro's point is to run across documents), so nothing is
added to `Document`.

The tap sits in the `update` wrapper (next to the hover-clear and
`refreshRtf`/`refreshPdf` — the established place for cross-cutting
per-key behaviour), appending each key *before* dispatch. Recorded keys are
filtered there, once, by a pure `recordable :: Key -> Bool`:

- **Excluded: environment events** — `KResize`, `KFocus`, `KReply` (they
  are inputs the *terminal* generates; replaying them would be fiction) and
  the EOF sentinel.
- **Excluded: mouse.** Positions are meaningless on replay (different
  scroll, different window). vim made the same call. Status note "mouse not
  recorded" the first time one arrives while recording.
- **Excluded: the record/stop and playback keys themselves**, and any key
  while a macro is *playing* (see §3's re-entrancy guard).

**Bound structurally**, per the `0001` lesson — but a macro must not
silently drop its *head* the way `pushHist` histories do, so the bound is a
hard stop: at `maxMacroKeys` (10 000) recording ends itself with a status
message. A `Seq` of 10 000 `Key`s is trivial memory; the cap exists to stop
"forgot I was recording for an hour" from becoming a session-length leak.

**UI:** recording state must be visible or it will bite — a `REC ●` zone in
the status bar via `statusRightInfo` (click = stop, a `StatusZone` like
INS/OVR), and the same indicator is what the PTY test asserts on.

## 3. Playback (the one design decision in the plan)

Playback must run **effects**, not just pure state — a macro that does
Find/Replace or Ctrl+C is the point — so it cannot be a pure fold inside
`update`. The clean seam already exists: `App.applyBatch` applies a list of
keys through `update`, running each key's effects in order. So:

- `update` on the play key emits `EffPlayMacro (Seq Key)`;
- the driver handler feeds those keys through `applyBatch` (the exact code
  path a burst of typed keys takes — one repaint at the end for free);
- a driver-side depth guard (`drvMacroDepth`) refuses nested playback, and
  the pure side refuses to *record* while playing — together these make
  macro-triggers-macro impossible rather than merely discouraged.

With a repeat count (a small `DKRepeatMacro` number dialog on a shifted
binding), the guard also caps total applied keys (`maxMacroKeys ×` count),
so playback is bounded even when the user asks for 10 000 repeats.

**A stated, honest limit:** a macro step that starts an *async* load (a
large-file `EffOpen`) does not block playback waiting for `loadQ` — the
following keys land before the load does, exactly as if typed quickly. v1
documents this ("macros and background loads don't mix") rather than
building a coroutine scheduler into the driver; if it ever matters, the fix
is a driver-side "drain loadQ between macro keys when `edLoading`" refinement
confined to the `EffPlayMacro` handler.

## 4. Keys and menus

Function keys are the terminal-safe currency (the F4/F6 lesson: nothing
intercepts them): propose **F9** toggle-record / **F10** play, *pending the
conflict audit* — step one of implementation is checking both against
`Input`'s current decode map and common terminal defaults (F10 is a menu key
in some terminals; if it loses the audit, Shift+F9 plays). Regardless of the
final chords, Edit ▸ "Record Macro / Stop Recording" (one `relabelEntry`'d
item) and "Play Macro" are the always-works fallback, pruned via the usual
machinery when no recording exists, and **guarded in `runAction`** for the
read-only views the same way their other disabled actions are — playback is
allowed anywhere (the macro's own keys are swallowed by read-only views
exactly as typed keys are; determinism means that's well-defined), recording
is allowed anywhere.

## 5. Repeat-last-edit (stage 2)

The `.` idea without modal grammar: `edLastBurst :: Seq Key` maintained by
the same tap — reset at every undo-snapshot boundary (EditorEdit already
knows exactly where those are; the boundary *is* the definition of "one
edit" in this editor, which is what makes this a two-line hook rather than a
heuristic), appended with buffer-modifying keys, capped hard like macros.
Edit ▸ "Repeat Last Edit" (propose Ctrl+. via the Kitty path with a menu
fallback, same audit caveat) replays it through the identical
`EffPlayMacro` machinery. Coupling repeat to undo-coalescing means "repeat"
and "what one Ctrl+Z removes" agree by construction — users already have a
mental model of that unit.

## 6. Testing

- **Pure (the determinism theorem, now a regression test):** for generated
  key sequences over fixture documents, record-then-replay from the same
  initial `Editor` yields equal buffer/cursor/selection state. This test is
  the architecture's warranty and will catch any future impurity sneaking
  into `update`.
- **Pure:** `recordable` filtering; the re-entrancy and cap guards; the
  undo-boundary reset for `edLastBurst` (one undo after a repeat removes
  exactly one application).
- **Pure:** replay across a document switch (macro records Alt+digit /
  MASwitchFile) — the zipper is part of `Editor`, so this should just work,
  and the test proves it stays true.
- **Integration (PTY):** record three edits, play with count 5, assert the
  screen; assert the `REC ●` indicator appears/disappears; assert F9 during
  playback does nothing.
- **Soak:** recording on for a long session stays flat (the cap firing is
  the assertion).

## 7. Risks and non-goals

- **No persistence, one macro register, no macro editing** in v1 — each is a
  separate small plan once the core earns its keep (persisted macros want
  the `show`-escaping discipline `edFindHist` already uses).
- **Dialogs replay as keys**, which is powerful (record a whole
  Find/Replace) but means a macro recorded against one dialog layout could
  land differently if defaults change between record and play *within a
  session* — accepted; it is what "replays your keystrokes" means, and
  determinism makes it at least always the *same* surprise.
- The honest async limitation in §3 is documented in the manual, not hidden.
