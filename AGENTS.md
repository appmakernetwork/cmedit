# AGENTS.md

Repository instructions for Codex. Keep this file short and operational. The
code, tests, `README.md`, and completed design notes under `docs/plans/` are the
source of truth for implementation details; do not turn this file into a
feature history.

## Project constraints

CMeDit is a modeless terminal text editor written from first principles in
Haskell. It deliberately has no TUI framework (`brick`, `vty`, etc.): terminal
control, input parsing, rendering, menus, and dialogs are implemented directly
with termios and ANSI/VT sequences. Preserve that design.

- Use only GHC boot libraries already listed in `Makefile`. Do not add a
  dependency that must be fetched from Hackage.
- Keep `src/` portable. POSIX-only code and `unix` imports belong under
  `platform/posix`; Windows support lives under `platform/windows`. Both expose
  `Cmedit.Term` with matching APIs.
- Preserve the pure-core/thin-IO-shell split. State transitions belong in the
  editor modules; external work is represented by `Effect` and interpreted by
  `Cmedit.App`.
- Do not introduce import cycles to save a little plumbing. Put code in the
  lowest editor layer whose imports suffice.
- Treat performance and bounded-memory behaviour as correctness properties.
  Read the performance invariants in `CONTRIBUTING.md` before changing editor
  state, rendering, search, caches, histories, or background work.

## Working method

Before editing:

1. Read the relevant implementation and nearby tests. Search by constructor,
   field, or action name with `rg`; do not rely only on this overview.
2. Check `git status --short`. Preserve unrelated user changes and generated or
   release artifacts.
3. For a non-trivial subsystem, consult the matching completed note in
   `docs/plans/completed/` and `docs/plans/0000-INDEX.md`. These notes explain
   constraints and failed approaches that are intentionally absent here.

While editing:

- Match surrounding Haskell style and use existing helpers before adding new
  abstractions.
- Keep pure logic testable. Add side effects as `Effect` constructors and
  handle them in `Cmedit.App`.
- Add or update focused tests in `test/Spec.hs` with the implementation.
- Do not "clean up" an unusual invariant until you understand the test or plan
  that protects it. Several apparent simplifications have caused large memory
  or latency regressions.
- If a change spans model, rendering, mouse handling, persistence, and menus,
  trace every consumer of the affected state instead of patching only the
  visible path.

## Build and verification

Use `make`, not `cabal`; this checkout is expected to work without a Hackage
index. GHC is 9.0.2.

```sh
make                 # optimized ./cmedit
make test            # full hand-written suite plus invariant lint
make run             # build and launch in a terminal
make windows-check   # typecheck all modules with the Windows platform layer
make soak            # long-session performance/memory regression check
make windows         # native Windows build; Windows only
```

Verification should match the change:

- Pure editor, parser, buffer, renderer, or persistence logic: run
  `make test`.
- Driver, terminal, filesystem, process, or portability work: also run
  `make windows-check`.
- Histories, caches, editor state, CSV, rendering cost, or background jobs:
  also run `make soak` when practical.
- Interactive terminal behaviour: build the optimized binary and exercise it
  through a real PTY; piping stdin is not a valid TUI test.
- Documentation-only changes do not require a rebuild unless commands or
  behaviour were changed.

Report commands run and any checks not run. Do not claim interactive behaviour
was verified from pure unit tests alone.

## Architecture map

The editor layers depend leftward; `App` and `Render` consume the public API
re-exported by `Editor`:

```text
EditorState <- EditorEdit <- EditorDoc <- EditorFind <- Editor

App (effects / IO)  ------ imports ------> Editor
Render (pure screen) ------ imports ------> Editor
```

- `Cmedit.EditorState`: records, `Effect`, layout, and small queries.
- `Cmedit.EditorEdit`: movement, editing primitives, undo, line operations,
  file properties, and settings specifications.
- `Cmedit.EditorDoc`: document lifecycle, zipper, view plumbing, recents,
  navigation, quick-open, and save/quit flows.
- `Cmedit.EditorFind`: in-file and workspace find/replace.
- `Cmedit.Editor`: public hub for update/dispatch, keys, mouse, menus, dialogs,
  and panels. It re-exports the editor API used by `App`, `Render`, and tests.
- `Cmedit.App`: raw-terminal driver, event loop, effect interpreter,
  background jobs, file IO, and persistence orchestration.
- `Cmedit.Render`: pure screen construction and terminal-frame diffing.
- `Cmedit.Input` / `Cmedit.Caps`: input decoding and terminal capability
  negotiation.
- `Cmedit.Menu` / `Cmedit.Dialog`: editor-independent data and helpers. They
  must not import `Cmedit.Editor`.
- `Cmedit.TextBuffer`, `Cmedit.Csv`, `Cmedit.History`: core data structures.
- `Cmedit.Pager`, `Rtf`, `Pdf`, `Image`, `Zip`, `Docx`, `Xlsx`, `Odf`, and
  `Epub`: file classification and specialised views/readers.
- `Cmedit.Search`, `Definition`, `QuickOpen`, and `Lint`: workspace features
  and supervised background work.
- `Cmedit.ConfigFile` and `Cmedit.Journal`: configuration, recents, sessions,
  crash recovery, and snapshots.
- `platform/{posix,windows}/Cmedit/Term.hs`: platform boundary.

Use `README.md` for user-visible behaviour and module descriptions. Use
`test/Spec.hs` as the executable specification for edge cases.

## Cross-cutting invariants

### Pure state and effects

`Cmedit.Editor.update :: Key -> Editor -> (Editor, [Effect])` owns editor
behaviour. Clipboard access, file operations, URL opening, process management,
notifications, and similar IO must leave the pure core as effects. Driver
results return through pure callbacks such as load/save/paste/error handlers.

The active document is stored directly in `Editor`; inactive documents live in
the `edBefore`/`edAfter` zipper. Per-document state must be saved/restored by
the zipper conversions. Global UI state (menus, modal browser, explorer) does
not belong in `Document`.

### Portability

Outside `platform/posix`, do not import `System.Posix` or depend on Unix types.
Use the portable aliases and `Cmedit.Term` operations already present (for
example `DiskTime` and `Term.statEntry`). Any change to shared driver code must
survive `make windows-check`.

### Rendering and terminal protocols

- Screen coordinates count terminal cells; buffer positions count characters.
  Convert with `Cmedit.Width` helpers. Never use `Text.length` as a screen
  column.
- A wide glyph is a head cell plus `contChar` continuation cells. Preserve the
  head cell's style and hyperlink metadata on continuations.
- Frame diffing, REP compression, hardware scrolling, hyperlink state, cursor
  placement, and pixel overlays interact. New cell emission must participate
  in the existing `EmitState` machinery.
- Safe, ignorable terminal hints may be emitted without a capability gate.
  Sequences whose absence would corrupt display (REP, graphics, unsupported
  SGR forms) must be gated on a positive probe or existing whitelist.
- A terminal that replies to no capability queries must retain a correct
  portable fallback stream.
- Geometry is centralised in `computeLayout` and shared helpers. Keep drawing,
  cursor placement, hit testing, selection, and scrollbar dragging in sync.
  Account for the explorer offset, gutters, both scrollbars, wrapping, and
  view-specific regions.

### Input and driver

The input thread parses into a `TQueue`; the main loop drains a batch, applies
all keys/effects, then renders once. Preserve batching and single-flight
background work. `KUnknown []` is the EOF sentinel and must not be produced by
ordinary parse paths.

Raw mode must go through `Cmedit.Term.setRawMode`; `NoBuffering` alone does not
disable IXON, ICRNL, or ISIG. Terminal setup/teardown must remain bracketed so
termios, mouse modes, cursor state, and the alternate screen are restored after
errors and signals.

### Memory and latency

The complete rules and measurements are in `CONTRIBUTING.md`. In particular:

- Bound histories structurally with `Cmedit.History.pushHist`; never simulate a
  bound with lazy list `take`.
- `detach` any `Text` slice retained beyond its source buffer's lifetime.
- Force values stored in `Seq`; use strict updates such as `Seq.adjust'`.
- Keep per-frame work proportional to visible cells, not document or line size.
- When a caller knows what changed, pass that information instead of diffing a
  large collection to rediscover it.
- Bound accumulating state and allow only one active background job of each
  kind. Cancellation/generation checks must prevent stale work, not merely
  discard its result.
- Do not trust `ptrEq` as proof of equality. It is only a fast path before a
  real comparison; use explicit counters/version fields where comparison is
  too expensive.

### Documents and specialised views

File classification order is safety-sensitive: formats identified by magic
bytes and bounded readers must be detected before generic binary refusal or
large-file handling. Preserve explicit work/size/depth limits in parsers for
untrusted input.

Read-only views must be guarded in both UI discovery and execution: prune or
relabel menu entries, and make shortcuts inert in `runAction`/dispatch. Their
mouse handlers must swallow unsupported text-area events so they cannot mutate
an invisible buffer. If a view scrolls, update both scrollbar measurement and
the action/drag path.

Selection models for formatted readers are over laid-out coordinates. Rewraps
invalidate those coordinates, so clear or deliberately remap selections; do
not leave stale indices pointing at different text.

CSV table mode is authoritative while active; `edBuffer` is stale until
`syncCsvToBuffer`. Every save/export/toggle path must use the established
synchronisation flow. Mutations to `csvRows` must keep its width, dirty-state,
and embedded-newline caches exact through the existing `withRows`/`withCell`
discipline. Do not add whole-grid work to a keystroke or driver-batch path.

Archive, office, e-book, PDF, image, RTF, and paged-file views are intentionally
bounded and mostly read-only. Unsupported constructs are skipped or reported,
not guessed at, and a failed rich-reader path should retain the established
safe fallback where one exists.

### Search, filesystem, and persistence

- Long-lived `Text` in clipboard/search/history/results must be detached.
- Workspace walking must retain pruning, result caps, generation checks, and
  single-flight supervision. Never let a stale worker overwrite newer state.
- Explorer refreshes must merge children while preserving expanded subtrees
  and path-anchored selection; replacing cached children wholesale collapses
  user state.
- Saving must remain streaming/strict and must route CSV through its current
  model. Re-opening a canonical path switches to the existing document.
- Journal/session/snapshot ordering and atomic-write patterns are crash-safety
  invariants. Read `docs/plans/completed/0011-crash-safe-journal-and-session-restore.md`,
  `0027-adaptive-journal-write-behind.md`, and
  `0030-per-workspace-sessions.md` before changing them.
- When adding a config key, update parsing/defaults/help, `settingsSpec`, live
  application/reversion, persistence, and tests together.

## Interactive TUI checks

The editor requires a real tty. For automation, use a PTY harness:

1. `os.openpty`, set the window size with `TIOCSWINSZ`, and start `./cmedit`
   with the slave for stdin/stdout.
2. Feed raw key bytes to the master.
3. Reconstruct the screen from cursor addressing and printable UTF-8. If the
   test exercises scrolling or compression, handle DECSTBM plus SU/SD and REP;
   skip OSC/DCS/APC strings correctly.
4. Test both a silent capability-probe path and, when relevant, explicit
   replies enabling the upgraded path.

Existing PTY and benchmark helpers are under `docs/plans/bench/`. Prefer
extending them over inventing a one-off harness.

## Website and documentation

The marketing site and online manual are maintained in the separate repository
`~/work/appmakernetwork` (`appmakernetwork/appmakernetwork.com`). For requests
that change the product page, online manual, whitepaper, or release links:

1. Treat this repository as the source of truth for editor behaviour.
2. Read `~/work/appmakernetwork/AGENTS.md` before editing that repository.
3. Follow its static-site and deployment constraints; do not restyle unrelated
   pages.

The local `website/` directory is not the deployed site unless a task
explicitly says otherwise.

## Completion checklist

Before handing off a code change, confirm:

- The change is in the lowest appropriate module and respects the pure/IO and
  platform boundaries.
- All representations and interaction paths affected by the state change were
  updated (model, zipper, renderer, mouse, menus/shortcuts, persistence).
- Tests cover the regression or new pure behaviour.
- Relevant `make` checks pass, including `windows-check` or `soak` when the
  change warrants them.
- No unrelated files, build artifacts, archives, or user changes were modified.
