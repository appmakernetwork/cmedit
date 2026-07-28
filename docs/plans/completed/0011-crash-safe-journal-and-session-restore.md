# 0011 — Crash-safe edit journal and session restore

**Theme:** capability — the long session's other risk is losing it
**Status:** ✅ **RESOLVED** — implemented 2026-07-27
**Risk (as shipped):** low — the save path is untouched, journals live in their
own directory, a failed journal write is a status-line note, and removal is
*derived* from the model rather than enumerated per save path

---

## Resolved

Shipped as planned in shape: `Cmedit.Journal` is a pure leaf module owning the
file format, the journal naming and the recovery decision (all unit-tested);
every byte of IO is driver-side and modelled on the linter — a 2 s
`JournalTick` debounce armed after any batch that moves the journal
fingerprint, exactly the `maybeArmLint`/`LintTick` pattern §2.2 named. Startup
GC + scan + `classifyJournal` feed a `DKRecover` dialog (Recover / Discard /
Keep for later); recovery installs `addStagedDoc`-shaped modified documents
with the journal's baseline mtime, so the ◆ stale machinery communicates the
changed-on-disk case as §2.3 intended. The `journal = on|off` config key, its
Settings row, the 0700 directory (a `setPrivateMode` added to both platform
`Term.hs` twins) and the README/manual privacy notes all landed.

Differences from the plan as written, all deliberate:

- **The baseline mtime is an exact integer** (picoseconds since the epoch),
  not §2.1's decimal: recovery's central question is `base == cur`, and a
  lossy decimal means the clean case never fires on a sub-second filesystem.
  Relatedly the header is one key per line (unknown-key tolerance needs it),
  the path is show-escaped (POSIX filenames may contain newlines), and the
  body is the buffer's lines joined with LF with *no* trailing separator, so
  a final blank line survives the round trip. EOL/BOM/final-newline are
  metadata, as saveFile models them.
- **Staleness is a per-document counter (`edDocSeq`), not `edEditSeq`.** The
  plan's `edJournalSeq` records the last *journalled* value as designed, but
  against a per-doc counter: `edEditSeq` is global, so a keystroke in one file
  would restale every other modified file's journal on every tick.
- **Removal is derived, never announced.** Instead of teaching each save/close
  path to emit a drop effect, `journalLiveKeys` answers "which documents would
  a crash still lose" and the driver sweeps `drvJournals` (the journals this
  session wrote or adopted) against that after every batch. Ctrl+S, Save As,
  Save All, close, quit-with-discard, Revert, undo-back-to-clean and switching
  `journal = off` all drop the journal without knowing journals exist, and no
  future save path can forget to. Only the recovery dialog's answers are
  effects (`EffDropJournals` / `EffAdoptJournals`); Keep for later emits
  nothing — nothing outside `drvJournals` is ever deleted, which is what makes
  "keep" mean it.
- **"Clean exit" means a confirmed quit (`edQuit`), not every graceful
  teardown.** The `finally` block also runs on SIGTERM/SIGHUP — and SIGHUP is
  an SSH drop, the headline scenario this feature exists for. Those exits
  preserve journals; only a quit flow that already confirmed discarding
  removes them.
- **Untitled journals are named by a stable per-document id**
  (`edDocId`/`edNextDocId`), seeded at startup past every untitled journal
  still on disk, so a fresh untitled buffer can never clobber one the user
  chose to keep.

Verification: `make test` **3 016 passing** (journal format round-trips, the
table-driven recovery decision, selection/fingerprint/drop coverage, recovery
installation); `make windows-check` clean; and a PTY integration harness
([`bench/pty_journal.py`](../bench/pty_journal.py)) covering §4's scenarios —
type → SIGKILL → restart → recover → byte-identical save, journal removal on
in-session save and on clean exit (second startup shows no dialog), and Keep
for later surviving a later clean exit.

The §6 follow-on (full session restore) remains open, as a separate smaller
plan, as proposed.

---

## 1. Why this belongs with the stability work

The rest of this directory is about a six-hour session staying fast. The other
thing a six-hour session needs is to not evaporate: an SSH drop, a terminal
window closed, an OOM kill, a laptop suspend gone wrong. Today cmedit has
`bracket`/`finally` teardown and a SIGTERM/SIGHUP handler that saves *recents
and history* — but unsaved buffer content is gone.

Every editor the user is likely to compare cmedit to (nano's `.save` files,
vim's swap files, VS Code's hot exit) protects this. It is also a natural fit
for the existing architecture: a new `Effect`, a driver handler, and a pure
model addition — no new dependency, no new subsystem.

## 2. Design

### 2.1 What is persisted

A per-document **journal file** under `~/.cache/cmedit/journal/`:

```
<sha-ish of canonical path or "untitled-<n>">.cmj
```

Contents, written atomically (temp file + `renameFile`, the same discipline
`TextBuffer.saveFile` already uses):

```
cmedit-journal 1
path: /home/ben/work/x.py          (absent for an untitled buffer)
mtime: 1721890123.456              (the on-disk baseline at load: edDiskMtime)
eol: lf    bom: none    final-newline: yes
cursor: 412:7
--
<the full buffer text>
```

Full text, not a delta: it is simple, it is verifiable, and buffers are small
next to the write budget below. Compression is unnecessary complexity.

### 2.2 When it is written

Not per keystroke. A **write-behind on the same debounce machinery the linter
uses**: after any batch that changes `edEditSeq`, arm a timer (`JournalTick`,
2 s); on fire, write the journals of every *modified* document whose journal is
stale. Skip when nothing is modified. This is at most one write every 2 s of
active typing, of a few KB to a few hundred KB — negligible next to what the
linter already does, and it reuses the existing timer/fingerprint pattern
(`maybeArmLint` is the model to copy).

Journals are **removed** on: successful save of that document, close without
changes, and clean exit (the `finally` block that already writes recents).

### 2.3 Recovery

At startup, before the welcome status: scan the journal directory.

- **Journal for a file that still exists, with a matching baseline mtime** →
  offer recovery. Use the existing dialog machinery: a `DKRecover` confirm
  listing the affected files ("3 files have unsaved changes from a previous
  session — Recover / Discard / Keep for later").
- **Journal whose file changed on disk since** → still offer, but flag it
  ("x.py changed on disk since these edits") — the user decides, and the
  existing ◆ stale-file machinery already communicates that concept.
- **Journal for an untitled buffer** → recover as an untitled buffer.
- **Recover** loads each journal as a modified document (exactly the shape
  `addStagedDoc` already produces for staged Replace All — reuse it).
- **Keep for later** leaves the journals in place and shows a status hint.

### 2.4 Garbage collection of journals

Journals for paths that no longer exist and are older than 30 days are removed
at startup (bounded work: one `listDirectory` + stats). A cap on total journal
directory size (say 256 MB, oldest-first eviction) prevents an unbounded cache
in `~/.cache`.

## 3. Interaction with existing behaviour

- **The pure model gains**: `edJournalSeq` (last `edEditSeq` journalled, per
  document, so the driver knows what is stale) and an `EffWriteJournal
  [(FilePath, …)]` / `EffDropJournal FilePath` effect pair. The doc twin goes
  in `Document`, per the "add it to both" rule in `CLAUDE.md`.
- **CSV documents** must journal the *table*, not the stale line buffer: reuse
  `syncCsvToBuffer` before writing, exactly as `EffSaveTo` does.
- **Image documents** are read-only: never journalled.
- **The manual** (`cmedit://Manual.md`) is read-only and excluded, like it is
  from recents.
- **Read-only files** with edits: journal them (the user may want to save
  elsewhere), but the recovery dialog must not offer to write back.

## 4. Testing

- **Pure:** journal serialisation round-trips (text with CRLF, BOM, no final
  newline, embedded NULs rejected, very long lines, non-UTF-8 replaced chars).
- **Pure:** the recovery *decision* function — given (journal metadata, current
  disk state), which of the four cases applies — is a pure function with a
  table-driven test.
- **Integration (PTY):** start cmedit, type, `SIGKILL` it, restart, assert the
  recovery dialog appears and recovery restores byte-identical content.
  `SIGKILL` specifically, because `SIGTERM` runs the graceful path.
- **Integration:** assert journals are removed after a save and after a clean
  exit, and that a second startup shows no dialog.
- **Soak:** journal writes must not show up as a latency spike — assert p99
  frame time is unchanged with journalling on (`0005`, `0006`).

## 5. Risks and non-goals

- **Never let journalling endanger the real file.** The journal is a separate
  file in a separate directory; the save path is untouched. A journal write
  failure is a silent status-line note, never an error dialog, never a block.
- **Not a swap file / not multi-instance locking.** Two cmedit instances
  editing the same file will each keep a journal; the recovery dialog may then
  offer the newer one. Detecting concurrent instances is a separate feature
  (and needs a lock file with pid liveness checks) — explicitly out of scope.
- **Privacy:** the journal contains file content in `~/.cache`. Create the
  directory `0700` and document the behaviour in the README and the manual;
  offer `journal = off` in the config for people editing secrets.

## 6. Follow-on: full session restore

Once journals exist, "reopen the files I had open, in order, with cursors" is
a small addition (the recents list already stores cursor positions): a
`session` file recording the open-document list and the workspace folder, and
a `--restore` flag / config key. Worth doing as a separate, smaller plan once
the journal format has settled.
