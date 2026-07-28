# 0025 — Full session restore

**Theme:** capability — the follow-on `0011` §6 promised
**Status:** ✅ **RESOLVED** — implemented 2026-07-27
**Risk (as shipped):** low — the session file records *paths and cursors*, never
content; every restored file is reopened through the ordinary guarded open
paths, so nothing new can be decoded that could not already be opened; and the
feature is off unless asked for, by flag or by config key

## Resolved

Shipped as `0011` §6 proposed, and as small as it predicted, because the two
pieces that would have been the work already existed: the recents list had been
storing `line:col:path` since the config-file plan, and every open route already
funnels through `App.classifyFile`. What was added is one file, one flag, one
config key and a restore pass that runs at the right moment.

**The file.** `~/.config/cmedit/session`, beside `recent` and `history` (not in
`~/.cache` beside the journals — this is a preference about your workspace, not
recoverable scratch):

```
cmedit-session 1
folder: /home/ben/work/cmedit
active: 2
12:4:/home/ben/work/cmedit/src/Cmedit/App.hs
1:1:/home/ben/work/cmedit/README.md
340:18:/home/ben/work/cmedit/src/Cmedit/Editor.hs
```

A version line, an optional `folder:`, an `active:` index and then one document
per line in tab order, in the **recents encoding** — deliberately, so there is
one line format to parse, one to write and one to test in this config
directory rather than two that drift.

**Untitled buffers are excluded.** They have no path to record, and their
*content* is already `0011`'s job: a journal recovers an unnamed buffer with its
text, where a session file could only record that one existed. Recording them
here would either duplicate the journal or produce empty buffers that claim to
be the user's work.

**Persistence is shape-driven, exactly like recents.** The driver rewrites the
file when the session *shape* changes — the workspace folder, the ordered list
of paths, or which document is active — and once more on the way out, from the
`finally` that already writes recents and history, with live cursor positions.
Cursor movement alone never triggers a write. That discipline is what makes the
file survive the case it exists for: a `SIGKILL` leaves a session file that is
correct about *which* files were open (shape changes are rare and were all
written) and at worst stale about where the cursors were within them — and the
cursors are the part `0011`'s journal patches back for anything that was
actually being edited.

**Two triggers, both explicit.** `--restore` on the command line, or
`restore-session = true` in the config on a start with **no file arguments**.
The second condition is not a nicety: `cmedit foo.py` is an unambiguous
instruction to open `foo.py`, and burying it under nine files from yesterday
would be a surprise nobody asked for. Files named alongside `--restore` are
opened *on top of* the restored session and end up active, which is the useful
reading of "restore my session, and also this".

The key defaults to **false**. Session restore changes what the editor does when
you start it with no arguments, which is the one behaviour a terminal editor's
users have the strongest priors about; opt-in is the only defensible default,
and the flag makes it free to try before committing to the key.

**Restore runs before the journal-recovery scan**, and the ordering is the whole
of how the two features compose. Recovery patches documents that are *already
open* — it installs the journal's unsaved content into the matching path.
Restore first, and a crashed session comes back as the files you had, with the
unsaved edits in them, in the order you had them. Recovery first, and the
recovered documents would be shuffled against the restored ones, or duplicated
by the restore that followed.

Restore itself reopens the folder (so the explorer panel comes back) and each
still-existing path through the normal open paths, so a `.csv` returns to the
table view, a `.pdf` to the reading view, an image to the image view, and a file
that has grown past `maxOpenBytes` since lands in the paged viewer instead of
being refused with an error. Files that no longer exist are **skipped, not
recreated as empty buffers**: they are counted, and the status note says
`Restored 4 of 5 files` (plain `Session restored` when nothing was missing).
The `active:` index and every cursor are restored, with indices clamped and
shifted down past the skips.

A missing or corrupt session file under `--restore` is a status note and nothing
more. It is not an error, because there is no state in which the user asked for
their session back and the correct response is to refuse to start.

**Settings.** One row, "Restore session on start", carrying `restore-session`
like every other key — `settingsSpec` for what it means, `applySettingRow` for
what it does, `updateConfigText` to persist it.

Verification: `make test` — session-file round-trips (folder present and
absent, the active index, paths needing show-escaping, an empty session, a
version line from the future, truncated and garbage input, all of which must
parse to "nothing to restore" rather than fail), and the pure skip/clamp
arithmetic that maps a recorded active index through a set of missing files.
`make windows-check` clean. Integration through the PTY harness
([`bench/pty_session.py`](../bench/pty_session.py)): open three files and a
folder, quit, restart with `--restore`, assert the same three files in the same
order with the same active document and cursors; delete one and assert the
`Restored N of M` note; and the composition test — edit without saving,
`SIGKILL`, restart with `--restore`, and assert the recovered content lands in
the *restored* document rather than in a second copy of it.

---

## 1. Why

`0011` closed with its §6 open: once journals exist, "reopen the files I had
open, in order, with cursors" is a small addition. It is small because the
expensive halves are already paid for. The recents list has stored
`line:col:path` from the beginning — restoring a cursor is a thing this editor
already does every time you re-open a file from the File menu — and
`App.classifyFile` is the single gate every open route already goes through, so
"reopen these paths correctly" needs no new decisions about view modes, size
limits or binary refusal.

What is left is a list: which files, in what order, which one was in front, and
which folder they belonged to. That is a small file and a startup pass over it.

The value is the one the rest of this directory is about. `0011` protects a long
session's *content* against a crash; this protects its *arrangement* — against a
crash, and equally against the ordinary reboot, where nothing went wrong and you
still have to find your eleven files again.

## 2. Design

### 2.1 The session file

`~/.config/cmedit/session`, in the config directory with `recent` and `history`.
A journal is recoverable scratch and belongs in `~/.cache`; a session is a
statement about your workspace and belongs with your other preferences — and it
must survive a cache clean, or `--restore` would fail exactly when a user tidied
up.

```
cmedit-session 1
folder: <path>            (absent when no workspace folder is open)
active: <n>
<line>:<col>:<path>       (one per open document, in tab order)
```

The body is the recents encoding, reused rather than re-invented: same writer,
same parser, same show-escaping for paths (POSIX filenames may contain colons
and newlines), same tolerance for a line it cannot make sense of. The version
line makes a future format change cheap and makes an unrecognised version a
no-op rather than a misparse.

### 2.2 When it is written

Once at **startup** (the initial shape, unconditionally), on every **shape
change** — folder, the ordered path list, or the active index — and once on
exit with live cursors, from the existing `finally`.

The startup write is not redundancy: the change-driven persist compares
against a baseline seeded from the startup shape, so without it a run that
opened its files and then only ever typed would write *nothing*, and a
`SIGKILL` would leave the previous session's file for `--restore` to trust.
The PTY test found exactly that hole and now pins the write
(`bench/pty_session.py`, scenario 4).

The alternative, writing on every cursor move, is what a naive implementation
does and it is wrong twice: it turns idle scrolling into a stream of writes to
`~/.config`, and it buys nothing the exit write does not already buy in the
graceful case. What it *would* buy is exact cursors after a `SIGKILL`, and that
is precisely the case `0011` already covers for any file that was being edited —
an unedited file's cursor is the cheapest thing in the session to lose.

The shape-change rule is what keeps the file fresh enough to be worth restoring
after a kill: opens, closes and file switches are the events that make a stale
session file wrong, and they are all rare and all written through.

### 2.3 Triggers

- `--restore` on the command line, always.
- `restore-session = true` in the config, **only** when no `FILE`/`DIR`
  arguments were given.

Arguments beat the config because they are the more specific instruction. With
`--restore` the user has been specific about both, so both happen: the session
restores and the named files open on top of it and take focus.

### 2.4 Restore, and its position in startup

Order: config → flags → **session restore** → journal recovery scan → welcome
status.

Restore reopens the folder first (the explorer panel is part of the arrangement)
and then each path through the ordinary guarded open. Nothing here reimplements
opening; a restored `.xlsx` is the same document a restored-by-hand `.xlsx`
would be.

Skips are counted, never faked. A path that has since been deleted or renamed
produces no document at all — an empty buffer bearing a dead path invites
`Ctrl+S` to recreate a file the user deleted on purpose. The active index and
the per-document cursors are applied after the skips are known, so both are
clamped and adjusted rather than pointing past the end.

Recovery runs *after*, so its patches land in the documents restore just opened.

## 3. Testing

- **Pure:** session-file round-trips over the awkward cases — no folder, no
  documents, one document, a path needing escaping, an `active:` index out of
  range, a version line we do not know, a truncated final line, and outright
  garbage. Every failure mode must parse to "nothing to restore".
- **Pure:** the skip arithmetic — given a recorded order and a set of missing
  paths, the surviving order and the adjusted active index.
- **Integration (PTY):** `bench/pty_session.py` — quit and restore (order,
  active document, cursors, the folder and its explorer panel); a deleted file
  and the `Restored N of M` note; a corrupt session file under `--restore`
  starting normally with a note; and the composition case, `SIGKILL` with
  unsaved edits, restart with `--restore`, recovered content in the restored
  document.

## 4. Risks and non-goals

- **Restore must never write.** It opens files; it creates none, and it
  overwrites none. The failure mode of every path in it is a status note.
- **Not multi-session, and not named sessions.** One file, one arrangement. Named
  workspaces are a genuinely different feature (they need naming UI, a picker
  and a story about what "the current session" then means) and belong in their
  own plan if anyone wants them.
- **Not concurrent-instance arbitration** — the same non-goal `0011` §5 stated,
  for the same reason. Two instances will each write the session file and the
  last one out wins. Doing better needs lock files with pid liveness checks,
  which is a separate feature and a much larger one than this.
- **Opt-in by default.** The key ships `false`. Changing what a terminal editor
  does when you type its name with no arguments is not a default to take from
  people who did not ask for it.
