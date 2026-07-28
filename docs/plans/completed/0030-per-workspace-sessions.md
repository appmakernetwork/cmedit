# 0030 — Per-workspace sessions, recent sessions, and changed-since-session recovery

**Theme:** capability — `0025` restored *a* session; this restores *the right
one*, and tells you what moved while you were away
**Status:** ✅ **RESOLVED** — implemented 2026-07-28
**Risk (as shipped):** low — every failure in the new surface degrades to
"open the file that is on disk", the format change is additive and v1 still
parses, and the content cache lives behind the switch that already governs
caching content

## Resolved

Everything in §§2–5 shipped, in §7's order. The pure layer is `ConfigFile`
(format v2, `splitLeadingFields`, `SessionSummary`, `newestSession`), `Journal`
(`sessionKeyName`/`sessionFileName`, reusing `pathHash`/`sanitizeBase`
verbatim), `EditorState` (`ChangedFile`, `ChangeVerdict`,
`sessionChangeVerdict`, `snapshotableDoc`/`snapshotOf`, `maxSnapshotBytes`) and
`EditorDoc` (`sessionMenuList`/`sessionMenuEntries`/`addSessionEntries`/
`assignSessionMnemonics`, `openSessionChangedDialog`/`installSessionSnapshots`/
`snapshotDoc`/`afterSessionChanged`, `snapshotRequests`, `sessionsListed`); the
IO is `App` (`sessionPathFor`, `persistSession`, `capSessionsDir`,
`listSessions`, `findRestoreSession`, `applySession`, `sessionChangedPass`,
`writeSnapshots`, `snapshotStampOK`, `readSnapshot`, `gcSnapshotDirs`).

Verified: `make` and `make windows-check` clean; `make test` **3251 passed, 0
failed** (3171 before, so 80 new assertions in a delimited "Per-workspace
sessions (plan 0030)" section plus the v2 conversions of 0025's own block);
`pty_journal.py` 4/4 and `pty_session.py` 5/5 — the latter's five scenarios all
open files and never a folder, so they still land in the folderless
`~/.config/cmedit/session` and remain the v1→v2 regression net.

**Seven deviations from the text below, all deliberate.**

1. **§2.7's "write to `<key>.new`, `renameDirectory` over the old" is not
   possible**: POSIX `rename()` cannot replace a non-empty directory. Shipped as
   write-`.new` / remove-old / rename-into-place, which is *not* atomic as a
   pair — and does not need to be, because the `stamp` file is written **last**
   inside `.new` and the restore only trusts a set whose stamp equals the
   session's `closed:`. A crash between the remove and the rename leaves the key
   with no snapshots, which degrades exactly as a crashed session does.
2. **The fallback's status note is conditional on there being a folder to
   name.** §2.3 sketched "Restored 6 files from ~/work/website" for step 2; an
   exact cwd match adds nothing (it is the directory you are standing in) and
   the folderless session has no folder, so both keep 0025's wording verbatim —
   which is also what lets `pty_session.py`'s five scenarios pass unchanged.
3. **The restored active document is found by path, not by index.** §2.5 said
   the menu restore runs "the same code path as startup", and it does — but
   `planRestore`'s `rpActive` indexes the *session's* surviving list, and a menu
   restore is a union, so the two are different numbers. `applySession`
   translates the plan's answer to a path and looks it up with `findOpenIndex`.
   Without this a menu restore left whatever happened to sit at that position
   active (caught in a smoke test, not by the type checker).
4. **`ChangedFile` carries the parsed snapshot, not a promise of one.** §2.6
   left open when the snapshot is read; the driver reads it during
   `sessionChangedPass`, for the changed files only, so the dialog's second
   button is a pure install — the `RecoverItem` pattern, for the same reason.
   The installed baseline is the *snapshot's own* `jMtime`, which is by
   construction the session's recorded one (both come from `docDiskMtime` at the
   moment the session ended), so no extra field was needed.
5. **`gcSnapshotDirs` runs unconditionally**, not behind `journal` as "beside
   `gcJournalDir`" would have implied. Housekeeping that only happens when
   caching is switched on would leave a user who switched it off with exactly
   the cache they switched it off to avoid.
6. **`newestSession` is a named pure function in `ConfigFile`** rather than an
   inline sort in the driver, so §6's "the fallback picks the greatest
   `closed:`" is a unit test rather than a test of `Data.Ord`.
7. **`installSessionSnapshots` guards each patch with `snapshotableDoc`.** The
   restore reopens every path through the ordinary guards, so a file that has
   since grown past `maxOpenBytes` or been replaced by a PDF is now a view with
   no buffer under it; writing a buffer into one would be nonsense. Such a
   document keeps its disk version — the same floor a missing snapshot gets —
   and the status line counts only what was actually installed.

**The pollFs reconciliation, stated because it is the one place three
mechanisms could have collided.** `installSessionSnapshots` sets
`docDiskMtime` to the session's baseline and `docDiskChanged` to `True`.
`noteDiskMtimes` — the 2 s freshness poll — *never writes a baseline*; it only
ever sets `docDiskChanged` to `True` and never back to `False`. So the poll
confirms the ◆ rather than clobbering it, and the case where the file on disk is
somehow *older* than the record (clock skew, a restored backup) still shows ◆
because the installer set it explicitly. No reconciliation code was needed.

**Review found: two defects, both fixed, both pinned.**

1. **`installSessionSnapshots` overwrote a document with unsaved edits.** The
   patch replaces the buffer outright and clears the undo history, and it was
   guarded only by `snapshotableDoc` and a path match. At startup that is
   harmless — every restored document was loaded from disk moments ago — but a
   *menu* restore (§2.5) runs on a live editor, where a recorded path may
   already be open with work in it. One click on *As You Left Them* destroyed
   it silently, with no undo step to get it back, which makes §2.5's "it adds;
   it never closes… it destroys nothing" false in exactly the case that route
   exists for: overwriting a dirty buffer is strictly worse than closing one.
   Fixed by adding `not (docModified d)` to the same guard, so an unsaved
   document gets the floor a buffer-less view already got — it keeps what it
   has. Pinned by two Spec checks (active and backgrounded dirty documents),
   both of which fail without the guard.
2. **`sessionOriginNote` abbreviated `$HOME` with a bare string prefix**, so
   `/home/ben` was a prefix of `/home/benjamin/site` and the fallback's status
   line named `~jamin/site` — a folder nobody has, in the one message whose
   whole job is to say which folder the session came from (§2.3). Fixed with a
   pure `App.abbreviateHome` that requires the remainder to begin at a path
   separator (and tolerates a trailing separator on `$HOME`); six Spec checks
   pin it, since being pure is what makes it testable without depending on
   whoever's `$HOME` the suite runs under.

Reviewed and held, with the evidence: the recents parser was checked against
the pre-0030 `parseRecentLine` as an oracle over 192 013 generated inputs (a
colon at every position of every string up to length four, plus longer
hand-picked shapes) with **zero** divergences, and a 7 400-input version of
that corpus now ships in `Spec` as the `csvParsePrev` idiom, since every
existing user's recents file goes through the shared `splitLeadingFields` on
the next start. The stamp mechanism was attacked by constructing nine torn
snapshot directories against a live editor — stamp missing, stale (the
T1-set/T2-session case), garbage, empty; the directory missing; only
`<key>.new` (the rename never ran); a stale `.new` beside a good set; a
truncated member — and **every one fails closed** (single-button prompt, disk
content, stray `.new` swept at startup) while the intact control still offers.
Cost: a 400-keystroke batch measures 0.77 ms/key against 0.78 ms/key on the
pre-0030 binary (within noise) and the session file is not rewritten while
typing, so `persistSession` still fires only on shape change; `EffListSessions`
costs 2.2 ms per File-menu open with 8 sessions and 8.3 ms with the directory
at `sessionDirMax`, which is off the render path and once per menu open.

Two §8 risks materialised in the mild form predicted: the session key does
switch mid-session on a folder open (working as designed — the next write goes
to the new key and the old file survives as a restorable past session), and a
menu restore does produce a union whose next shape write records A ∪ B under
B's key. Neither needed mitigation. The two-stacked-modals risk did not
materialise in testing, but the suppression mitigation was not implemented and
remains available.

---

## 1. Why

`0025` shipped one session file, `~/.config/cmedit/session`, and it is correct
for exactly one workspace at a time. The moment there are two, it is a shared
mutable global with last-writer-wins semantics:

> Open cmedit in `~/work/api`, work for an hour, quit. Open it in
> `~/work/website`, fix one CSS file, quit. Go back to `~/work/api` and
> `--restore` — and get the website's one CSS file, because the second session
> overwrote the first.

That is not a rare edge: alternating between projects in two terminals is the
ordinary way a terminal editor gets used, and the failure is silent, because the
restore *succeeds* — it restores the wrong thing. The user's only recovery is
the recents list, one file at a time, which is what the session file exists to
replace.

The second half is relevance. `--restore` today means "the last session
anywhere", so its answer does not depend on where you typed it. But a terminal
editor is started *in a directory*, and that directory is by far the strongest
available signal about which arrangement of files the user wants back. Making
`cd ~/work/api && cmedit --restore` mean "the API session" needs no new UI and
no new decision from the user — the information is already in `$PWD`.

The third is the one `0025` could not have: once several sessions exist, the
File menu can offer them, and "reopen the eleven files I had in the other
project" becomes one keystroke rather than a `cd` and a restart.

And the fourth is what makes any of this trustworthy. A session restored on
Monday describes files as they were on Friday; between the two, `git pull`
happened. Today the restore silently opens whatever is there now, and the only
sign that anything moved is the ◆ marker — which cannot appear, because the
mtime baseline is recorded *at load*, so a file that changed before the restore
is, as far as the editor knows, pristine. A session that records what it saw
can say so instead (§2.6), and — because the same clean exit that wrote the
session can also cache the content — can offer the version you actually had
(§2.7).

## 2. Design

### 2.1 Per-workspace session files

```
~/.config/cmedit/sessions/<pathHash(folder)>-<sanitized-basename>.session
```

`Journal.pathHash` and its `sanitizeBase` sibling, reused verbatim: the hash is
the identity (a folder path is long, contains separators and may contain
anything a filename cannot) and the basename is there so a human looking in
`~/.config` can tell which is which — exactly the argument `journalFileName`
already makes, and there is no reason for this directory to make a different
one.

Two consequences of that naming are load-bearing rather than cosmetic:

* **The cwd lookup is O(1) and needs no listing.** Given a canonical `$PWD`,
  the session file's *name* is computable, so `--restore` is one
  `doesFileExist` and one read (§2.3). Only the menu, which genuinely wants all
  of them, lists the directory.
* **A folder that is renamed loses its session**, and that is the honest
  answer: the paths inside it are stale too. It ages out under §5.

`~/.config/cmedit/session` — the singular file — **stays**, unmoved, as the
session for a run with *no workspace folder open*, and as the read fallback for
anything written before this plan (§4). It is not migrated, not deleted and not
duplicated: a folderless session is a real session, it needs a key, and "the
file 0025 already writes" is the obvious key for it.

**The live session persists to the key of its current folder**, re-evaluated
whenever the shape is written. Open a folder mid-session and the next write goes
to the new key; the previous key's file is left exactly as its last write left
it, and becomes a restorable past session that feeds the menu. That is a
feature, not a leak — closing a folder is the most common way a session *ends*
without the process ending.

The invariant that makes all of this reason about cleanly is one line, and it
is `0025`'s: **a session file describes what is actually open, at the moment it
was written.** Nothing derives, merges or reconciles; the file is a photograph.
Everything awkward below (a menu restore that unions two workspaces, §2.5; a
crash leaving a mid-session write, §2.2) is that invariant behaving correctly
rather than a case to special-case.

### 2.2 Format v2

```
cmedit-session 2
folder: /home/ben/work/cmedit
closed: 1753660000000000000000
active: 2
12:4:1753659000000000000000:/home/ben/work/cmedit/src/Cmedit/App.hs
1:1:-:/home/ben/work/cmedit/notes-not-yet-created.md
340:18:1753652000000000000000:/home/ben/work/cmedit/src/Cmedit/Editor.hs
```

Two additions, both integers, both **exact picoseconds since the epoch** via
`Journal.diskTimeToPicos`/`picosToDiskTime`. The exactness is not tidiness: the
question this feature asks is `recorded == current`, and `0011` already
established that a lossy decimal makes that comparison answer "changed" for a
file nobody touched, on any filesystem with sub-second timestamps.

* **A per-file mtime**, third field of the entry line. It costs **no extra
  stats**: every document already carries `edDiskMtime`/`docDiskMtime`, recorded
  by `loadFile`/`saveFile` and refreshed by the driver's existing `pollFs`
  freshness pass. A file with no baseline (named but never created) writes `-`,
  which parses to `Nothing` and means "no change detection for this one".
* **`closed:`**, the whole file's timestamp. It orders the menu (§2.4) without
  stat'ing anything, and — because it is *recorded* rather than read from the
  filesystem — the ordering survives an `rsync` or a restore of `~/.config`,
  which would flatten every mtime into the same instant.

> **Refinement of the pinned wording.** `closed:` is written on *every* write,
> not only at exit, so its exact meaning is "when this file was last written".
> For every session that ended, the exit write is the last one and it reads as
> "closed at"; for a session that was killed, it is the last shape change, which
> is the honest answer and the same thing every other field in the file is
> saying. Writing it only on exit would leave a crashed session undated, and
> dating it would then require the stat the field exists to avoid.

**The entry line breaks `0025`'s "one line format" if it is not handled
carefully**, and it is worth saying why the shape above is the one to pick. The
recents encoding is `line:col:path`, where only the *first two* colons separate
and everything after them is the path (POSIX filenames may contain colons).
Inserting a third fixed field before the path keeps that property; appending it
after the path would destroy it. The parser generalises to "take *k*
colon-separated leading fields, the rest is the path", with the recents and v1
sessions passing `k = 2` and v2 passing `k = 3` — one function, two callers,
which is the spirit of `0025`'s decision rather than merely its letter.

**v1 files still parse.** The version line governs, as it always has: `1` is
read exactly as today (no mtimes ⇒ no file is ever reported changed, so a v1
restore behaves precisely like a `0025` restore), `2` is read as above, and
anything else remains "no session", never a guess. Only v2 is ever written.

### 2.3 `--restore` and `restore-session` become cwd-scoped

Both triggers keep their existing conditions (`--restore` always;
`restore-session = true` only with no file arguments) and change only *which*
session they find:

1. Canonicalise `$PWD`; compute its session filename; if it exists and parses,
   restore it. This is the case that should be true almost every time, and it
   costs one `doesFileExist`.
2. Otherwise, fall back to the **most recently written session of any kind** —
   including the folderless one — chosen by `closed:`. The status line names
   where it came from: `Restored 6 files from ~/work/website`, with the folder
   abbreviated to `~` where it can be.

Step 2 is what preserves the muscle memory `0025` built: a user who has always
typed `cmedit --restore` from their home directory keeps getting their last
session. Step 1 is what makes the common case right. Naming the folder in the
status is the part that makes step 2 non-astonishing — a restore that silently
produced another project's files is the bug this plan opened with.

A cwd that is *inside* a workspace but not its root deliberately does **not**
match: an ancestor walk would guess, and it would guess wrongly on nested
repositories. Step 2 covers it well enough, and the menu (§2.4) covers it
exactly.

### 2.4 The File menu's recent sessions

A short section of `MARestoreSession !Int` entries, built in `entriesFor` the
way `MARecentFile` already is: dynamic entries spliced by a pure
`addSessionEntries`, addressed by index into a list the same function derives,
so the label and the action cannot disagree.

**Where:** immediately above the recent-files section that `addRecentEntries`
splices in, separated from it, both above the `Settings…`/`Exit` tail. Files and
sessions are both "things I had open"; sessions are the coarser unit, so they
read better first.

**What is listed:** at most `sessionMenuMax` = 4, ordered by `closed:`
descending, **excluding the live session's own key** (offering to restore what
is already open is noise), and de-duplicated by folder so a legacy global
session that names a folder now owning its own file does not appear twice — the
newer `closed:` wins.

**Labels:** `cmedit (11 files)`, i.e. the folder's basename and the recorded
file count, elided from the left past `maxW` exactly as `recentMenuEntries`
elides paths. The folderless session shows as `(no folder) (3 files)`. The
basename rather than the path is deliberate: the path is what
disambiguates two sessions, but two *simultaneously listed* sessions with the
same basename is rare enough to answer with the status line after the fact
rather than with 44 columns of menu.

**Where the list comes from.** `edSessions :: [SessionSummary]` on `Editor` —
global state like `edRecent`/`edSearch`, **not** in `Document`. It is filled by
a driver round trip (`EffListSessions` → `sessionsListed`), fired once at
startup and again whenever the File menu opens — the same hook that already
emits `EffStatFile` on a menu open, and for the same reason: another instance
may have exited since. Reading it is one `listDirectory` and at most
`sessionDirMax` (§5) small parses; the summary carries only folder, count and
`closed:`, never the file list, which the restore re-reads from disk at the
moment it acts.

#### Menu geometry: mnemonics and the digit space

`recentMenuEntries` numbers its entries `&1`..`&6`, which is why they need no
letter: the digits are free in the File menu and the numbering is stable.
`mnemonicItemIn` returns the **first** match, so a second numbered list in the
same dropdown would give sessions dead accelerators and give the user two `1`s.
Continuing the sequence (`&7`..`&0`) is worse: `recentMenuPaths` filters out
already-open files, so the recents section's length varies from 0 to 6 and every
session's digit would move as files are opened.

**So sessions do not take digits.** The proposal is a small pure allocator run
as a post-pass over the *finished* File-menu entry list — which is exactly where
it can see what is taken:

```haskell
-- The first character of the folder's basename, if it is a letter that no
-- other entry in this dropdown has claimed; otherwise no mnemonic at all.
assignSessionMnemonics :: [MenuEntry] -> [MenuEntry]
```

The static File menu claims `n o f i s a l v c d t x`, the recents claim
digits, and a session for `~/work/website` takes `w`. A session whose basename
starts with a taken letter, a digit or a non-letter simply renders without an
underline and stays reachable by arrows and mouse — which is a graceful floor,
not a hole, because nothing else in the menu depends on a mnemonic existing.
This is a pure `[MenuEntry] -> [MenuEntry]` function and gets a table test
(§6).

*Fallback if that proves fussy in review:* give session rows no mnemonic at all.
It costs one keyboard route and nothing else; it is recorded here so the
allocator is a considered choice rather than the only one.

### 2.5 Restoring from the menu

`MARestoreSession k` emits `EffRestoreSession <session file path>` — a driver
round trip, not a pure toggle, because restoring is file IO by definition. The
driver runs **the same code path as startup** (`restoreSession`, generalised to
take a session rather than finding one), on the live editor:

* the folder opens, so the explorer panel follows;
* each recorded path opens through `openPath setLoadedNew imageLoadedNew`, so an
  already-open path **switches to the open copy** rather than duplicating it,
  and every view mode, size guard and binary refusal is the ordinary one;
* the recorded active document and cursors are applied through
  `planRestore`/`seedSessionPos`, unchanged;
* the changed-files dialog (§2.6) then runs.

**Journal recovery does not re-run.** It is a startup scan over
`~/.cache/cmedit/journal`, its dialog is answered once per process, and its
whole premise ("a previous run of *this* editor died") is a startup question.
Re-running it mid-session would offer journals the running session itself wrote.

**A menu restore adds; it never closes.** Files already open stay open, and the
result is a union. This is `0025` §4's rule ("restore must never write… it
opens files; it creates none, and it overwrites none") extended one step: it
also destroys nothing, and it therefore needs no unsaved-changes prompt of its
own. See §8 for what the union does to the *next* session write, which is a
consequence worth being explicit about rather than a defect.

### 2.6 The changed-files dialog

After the files are installed, the driver stats the restored paths (it has just
opened them, so this is nearly free and mostly cache-warm) and compares each
against the mtime the session recorded. Files that differ **moved while the
session was closed** — the ◆ machinery cannot show this, because ◆ compares
against a baseline taken at load, and the load just happened.

If at least one moved, one dialog, a new data-only kind in `Cmedit.Dialog`:

```haskell
DKSessionChanged   -- ^ Restore: files moved on disk since the session ended.
```

built with the existing `mkConfirm`, listing at most `maxRecoverListed` (8)
files and counting the rest, in `openRecoverDialog`'s exact shape — including
the ◆ convention beside each name, which is the marker the editor already uses
for "this file changed underneath you":

```
  Files Changed Since This Session

  3 files have changed on disk since this session ended.

    ◆ src/Cmedit/App.hs
    ◆ README.md
    ◆ notes.md — no saved copy from that session

  Open the newest version, or the files as you left them?

         [ Latest on Disk ]   [ As You Left Them ]
```

**The buttons.** `Latest on Disk` (14 chars) and `As You Left Them` (16).
Rejected: *Open Newest* — the files are already open, so a verb lies about what
the button does; *Use Mine* / *Use Theirs* — merge-conflict vocabulary, which
promises a reconciliation that is not on offer; *Keep Disk Version* /
*Restore Session Version* — accurate and too long for a two-button row on 80
columns. The pair that ships is parallel in grammar, honest about what each side
*is* (a place, and a moment), and needs no knowledge of the feature to read.

**There is no Cancel, and Esc is safe.** By the time the dialog appears the
files are open at their newest state; `Latest on Disk` is therefore a no-op, and
Esc/`cancelDialog` maps to it. A dialog whose escape hatch is also its default
answer is one the user can dismiss without learning anything, which is the
correct property for a prompt that appears rarely and unpredictably.

**When there is nothing to offer** — a crashed session (no snapshots, §2.7), a
file over the snapshot cap, or `journal = off` — the second choice would do
nothing. Offering it anyway would be a lie, so the dialog degrades to a single
`OK` button and the informational wording ("3 files have changed on disk since
this session ended"), joining the single-button family that dismisses on a click
off the box. Per-file, a name with no usable snapshot is annotated in the list
(`— no saved copy from that session`) and stays at its disk version under either
choice.

**All-or-nothing.** One answer covers every listed file. Per-file choice needs a
selectable list widget this dialog system does not have, and the case is rare
enough that "answer twice" (choose *As You Left Them*, then Revert the one you
wanted fresh) is a fair price. Recorded as a non-goal, not an oversight.

### 2.7 What makes "As You Left Them" possible: clean-exit snapshots

**The session file still never holds content.** `0025`'s privacy claim is
unchanged and unqualified: `~/.config/cmedit/sessions/*` are paths, cursors,
mtimes and counts.

The content lives where content already lives — `~/.cache`, 0700, behind
`journal` — in the **existing journal format**, which already carries exactly
the fields needed (path, baseline mtime, EOL, BOM, final-newline, read-only,
cursor, text) and already has a parser, a serialiser, a naming function and a
test suite:

```
~/.cache/cmedit/snapshots/<session-key>/
    stamp                       -- the session's `closed:` value, one line
    <pathHash>-<basename>.cmj   -- Journal.journalFileName, verbatim
```

**When:** on a clean exit only, in the same `finally` that already writes the
recents, the session and the history — after `saveSessionFile`, so the `stamp`
it writes is the value that landed in the session file. The directory is
replaced wholesale (write to `<key>.new`, `renameDirectory` over the old, sweep
the leftover), so a snapshot set is always internally consistent and never
half-updated.

**What:** every open plain-text and CSV document (CSV serialised through
`syncCsvToBuffer`, exactly as `EffSaveTo` and the journal write-behind already
do). Read-only views with no buffer — image, pager, PDF, container-derived —
have nothing to snapshot and are skipped, as they are skipped by the journal.
The manual's `cmedit://` pseudo-path is excluded, as everywhere else.

**Cap:** `maxSnapshotBytes` = 4 MB per document. A file over it gets no
snapshot; the dialog says so, per file, and the restore uses disk. The cap is
what keeps the feature's cost proportional to the sessions people actually
have — and, unlike the journal, a snapshot is written for *every* open document
rather than only modified ones, so it needs a tighter bound than the journal's.

**The stamp is what stops a stale snapshot set being offered, and it closes a
real hole.** "Crash sessions have no snapshots" is not automatically true:
consider a clean exit at T1 (snapshots written, stamp = T1), then a second
session under the same key that is `SIGKILL`ed at T2 (the session file has been
rewritten by shape changes, `closed:` = T2, and no snapshots were written). The
snapshot directory is now T1's while the session file is T2's, and every mtime
comparison would be done against T2's record while the offered content came from
T1 — silently restoring *older* content than the session describes. Requiring
`stamp == closed:` makes the T1 set unusable for the T2 session, and makes
"a crashed session has no snapshots" fall out as a consequence rather than as a
rule someone has to remember to enforce.

**How a snapshot installs:** as a **modified, unsaved buffer** — the
`stagedDoc`/`addStagedDoc` shape that staged Replace All and journal recovery
both already produce, with `docSavedBuffer` set to the file's *current* on-disk
content and `docDiskMtime` to the session's recorded baseline, so the document
reads as modified **and** carries ◆ from the first frame. Saving it over the
newer file is then a deliberate Ctrl+S by a user who has been told twice, which
is precisely the recovery semantics `0011` chose and the reason nothing here
ever writes.

### 2.8 Precedence

Pinned, and worth stating as a table because three mechanisms now have opinions
about the same buffer. Per restored path, at startup:

| session mtime | disk now | snapshot | result |
|---|---|---|---|
| absent (v1) | any | any | open from disk; not listed in the dialog |
| == disk | present | any | open from disk; not listed |
| ≠ disk | present | valid, ≤ cap | listed; *Latest on Disk* → disk, *As You Left Them* → snapshot as a modified buffer |
| ≠ disk | present | none/invalid/over cap | listed and annotated; disk under either answer |
| any | **gone** | any | skipped and counted (`Restored 4 of 5 files`) — `0025`'s rule, unchanged |

and then, **last**, the journal recovery dialog patches whatever it patches. A
journal is unsaved content from a session that died; a snapshot is saved content
from one that ended tidily. When both speak for a file, the unsaved edits must
win, which is what "journal last" means. Menu restores stop after row 5 — no
journal step (§2.5).

**Two stacked modal dialogs at startup is acceptable**, and it should be said
out loud rather than discovered: each is independently conditional (files
changed on disk *and* a previous run died unsaved), both are rare, both are
answered by one keystroke, and the alternative — one merged dialog with four
buttons — would fuse two questions that have different answers and different
consequences. They appear in precedence order, so the second question is asked
about the state the first answer produced.

## 3. Config surface

**No new keys**, and two reasons rather than one.

Snapshots are gated on the existing `journal` key because there must be exactly
one privacy story: *content is cached in `~/.cache` only when `journal = on`*.
Two switches would make that sentence conditional, and a user who turned
journalling off to edit secrets would be entitled to be angry about the second
one.

The rest of the plan needs no key because it changes no behaviour anyone opted
into: `restore-session` and `--restore` mean what they meant, only more
accurately; the menu section is a menu section; and the changed-files dialog
appears only when the answer to "did anything move?" is yes.

**One documentation change is mandatory, and it is not optional politeness.**
Today `journal = on` caches the content of *modified* documents and
`dropJournalsOnExit` removes it all on a clean quit — so a tidy exit leaves no
content in `~/.cache` at all. After this plan, a clean exit *starts* leaving
content there, for every open document, saved or not. That is a genuine
expansion of what the key means, and the key's help text, the Settings row hint
and the README/manual privacy notes must all say so ("cache buffer contents
under `~/.cache/cmedit` for crash recovery and session snapshots"). §8 records
the alternative that was not taken.

## 4. Migration and compatibility

* **A pre-0030 `~/.config/cmedit/session`** is a v1 file and parses. It is
  read as the folderless session, and it is a candidate for both the `--restore`
  fallback (step 2 of §2.3) and the menu. Nothing migrates it, copies it or
  deletes it: it is simply an older session that ages out of the menu as newer
  ones are written, and it keeps serving the no-folder case in the meantime.
* **v1 in general**: no mtimes ⇒ no file is ever "changed", ⇒ no dialog. A
  first-run-after-upgrade restore behaves exactly like `0025`'s, which is the
  right outcome for a session recorded by a binary that did not know what to
  record.
* **Downgrade**: an older binary reading a v2 file sees an unknown version and
  reports "no previous session to restore" — `0025`'s designed behaviour for
  exactly this, working as intended.
* **Windows**: the paths are `getXdgDirectory`/`configDir`-derived as they
  already are, `setPrivateMode` is the platform layer's existing no-op there,
  and the wholesale directory replacement needs `renameDirectory`
  (`directory`, boot). `make windows-check` after the driver work.

## 5. Garbage collection

Two directories grow, and neither may grow without bound.

**`~/.cache/cmedit/snapshots/`** joins the startup housekeeping beside
`gcJournalDir`, with the same discipline (one listing, bounded stats, delete
before read):

* a snapshot directory whose **session file no longer exists** is unreachable
  and goes — the twin of `journalIsOrphan`, and cheaper, because the test is a
  filename computation rather than a parse;
* anything older than `journalMaxAgeSecs` (30 days) goes, matching the journal
  so there is one number to remember;
* a directory cap, evicting oldest-first — proposed at **128 MB**, half the
  journal's, because snapshots are written for every open document rather than
  only modified ones and the loss when one is evicted is smaller (the file on
  disk is still there);
* stray `.new` directories from a crash between write and rename go
  unconditionally.

**`~/.config/cmedit/sessions/`** is capped at `sessionDirMax` = 50 files —
`maxRecentEntries`' number, for the same reason — evicting by `closed:`,
oldest first, on write. This one is in `~/.config` rather than `~/.cache`, so
the eviction should be conservative and silent, and it should never touch the
folderless `session` file.

## 6. Testing

**Pure (`test/Spec.hs`)** — everything below is a function with no IO:

* **v2 round-trips**: mtimes present, absent (`-`), mixed; a `closed:` stamp; an
  `active:` index out of range; paths containing colons *and* a v2 third field
  (the case that catches a mis-generalised line parser); a truncated final line;
  garbage; and a version line from the future. Every failure parses to "nothing
  to restore", as `0025` requires.
* **v1 back-compat**: a byte-exact `0025` session file parses to the same
  `Session` it does today, with every mtime `Nothing`.
* **cwd matching**: the session filename for a path is what `pathHash` +
  `sanitizeBase` say it is; two folders differing only past the basename get
  different names; the fallback picks the greatest `closed:` and its "which
  folder" answer is the one the status line will name.
* **Menu derivation**: `addSessionEntries` over a summary list — ordering by
  `closed:`, the cap, exclusion of the live key, de-duplication by folder,
  labels and elision, the folderless label, and `MARestoreSession k` addressing
  the same entry the label names.
* **Mnemonics**: `assignSessionMnemonics` against a File menu carrying every
  static mnemonic plus 0..6 recents — a session basename whose initial is free
  gets it, one whose initial is taken/absent/a digit gets none, and no dropdown
  ever contains two entries with the same mnemonic (assert over the whole
  generated menu, which also guards future static items).
* **The precedence table of §2.8**, table-driven, one row per line of it,
  including the stamp mismatch and the over-cap file — the decision function
  must be pure and must take (recorded mtime, current mtime, snapshot
  availability) and nothing else.
* **Snapshot selection**: which open documents are snapshotted (CSV yes, via
  the synced buffer; image/pager/PDF/container no; `cmedit://` no; over-cap no),
  and that a snapshot round-trips through the *unmodified* `Journal` serialiser.

**Integration (PTY)** — extending `docs/plans/bench/pty_session.py`, whose five
scenarios must keep passing unchanged (they are the v1→v2 regression net):

* **Two workspaces, no clobbering**: session in A, session in B, `--restore`
  from A gives A. This is the plan's headline and should be scenario 6.
* **cwd-scoped restore and the fallback**: restore from a directory with no
  session lands on the most recent one *and says whose it is*.
* **The changed-file dialog, both answers**: open two files under a folder,
  quit cleanly, modify one on disk from the harness, restart with `--restore`;
  assert the dialog lists exactly the modified file, then (a) *Latest on Disk*
  → the buffer holds the new bytes and is unmodified, and (b) in a second run
  *As You Left Them* → the buffer holds the old bytes, shows modified and ◆, and
  **the file on disk is untouched** until an explicit Ctrl+S.
* **The degenerate case**: same setup but `journal = off` — one button, no
  second choice offered.
* **Menu-driven restore**: File ▸ the session row, on a running editor with a
  different folder open; assert the files arrive, an already-open one does not
  duplicate, and no journal dialog appears.
* **Stacking**: unsaved edit → `SIGKILL` → modify a *different* restored file on
  disk → restart with `--restore`; assert the changed-files dialog first, the
  recovery dialog second, and that the recovered content survives the first
  answer either way.
* **The stamp**: clean exit, then a killed session under the same key, then
  restart — assert the stale snapshots are *not* offered.

## 7. Estimated effort

3–4 days, in the order it should be built (each step leaves the tree shippable):

| | |
|---|---|
| v2 format + the generalised line parser + `Spec` round-trips | 0.5 d |
| Per-folder keying, the sessions directory, its cap, live re-keying | 0.5 d |
| cwd-scoped `--restore` + fallback + status wording | 0.5 d |
| `edSessions`, `EffListSessions`, the menu section, mnemonics | 0.5 d |
| `MARestoreSession` + the live-editor restore path | 0.5 d |
| `DKSessionChanged` + the change comparison + the no-snapshot degradation | 0.5 d |
| Snapshots: write, stamp, install, cap, GC, privacy docs | 1 d |
| PTY scenarios | 0.5 d |

## 8. Design risks

* **Two modal dialogs at startup.** Argued in §2.8 and accepted. The
  mitigation, if it grates in practice, is not to merge them but to suppress the
  changed-files dialog when the recovery dialog will also appear *and* the two
  name the same files — the journal already carries the `RecoverChanged` caveat
  for exactly those, so the second dialog would be repeating itself.
* **The privacy surface of `journal` widens** (§3). This is the risk in the
  plan that is about the user rather than the code: today a clean exit leaves no
  cached content, and after this it leaves a copy of everything that was open.
  The pinned decision — one switch, one story — is kept, because two switches
  make the privacy sentence conditional; but it is only defensible *with* the
  documentation change, and if review disagrees the alternative is a
  `session-snapshots = on|off` key defaulting **off**, which costs the feature
  most of its reach and should be a deliberate trade rather than a default.
* **Snapshot disk cost.** Bounded three ways (4 MB per file, 128 MB per
  directory, 30 days), but the *steady* cost is real: a user with eight
  workspaces of a dozen files each keeps tens of megabytes cached
  indefinitely. That is the same bargain `~/.cache` exists for, and the GC is
  what makes it a bargain rather than a leak.
* **The session key switches mid-session** when a folder is opened or closed,
  and combined with the union semantics of a menu restore (§2.5) that means:
  restore session B while A's files are open, and the next shape write records
  A ∪ B under B's key. This is the photograph invariant (§2.1) working — the
  file says what is open — but it will look like pollution to anyone who expects
  "restore" to mean "replace the window", as it does in VS Code. Accepted, with
  the status line naming what was added; the alternative (close A's documents
  first) means an unsaved-changes prompt inside a restore, which is a third
  dialog on a path that already has two.
* **A file that has been deleted since the session, but was snapshotted**, is
  skipped rather than resurrected from the snapshot. That is `0025`'s explicit
  rule ("an empty buffer bearing a dead path invites Ctrl+S to recreate a file
  the user deleted on purpose") and the snapshot does not change the argument —
  but it is now a case where we demonstrably *have* the content and decline to
  offer it, which deserves to be a recorded decision rather than a silent one.
* **`closed:` is a wall-clock time**, so a clock that moves backwards (NTP
  correction, a VM restored from a snapshot) can order the menu wrongly for one
  session. Nothing worse than a wrongly-ordered list follows: the restore itself
  reads the file, and the cwd match does not consult the timestamp at all.

## 9. Non-goals

* **Named sessions, and more than one per folder.** A folder has one
  arrangement; naming needs UI, a picker and an answer to "what is *the* current
  session then", which is `0025`'s non-goal restated and still a separate plan.
* **Concurrent-instance arbitration.** Two instances in the same folder each
  write that folder's session file and the last one out wins — the same stance
  as `0011` §5 and `0025` §4, for the same reason: doing better needs lock files
  with pid liveness checks. Note that this plan *narrows* the blast radius,
  since two instances in different folders no longer collide at all, which is
  the case that actually happens.
* **Snapshot diffing.** No three-way merge, no "show me what changed", no
  per-hunk choice. The dialog offers two whole files because that is what can be
  explained in one line, and a diff view is a feature with its own plan.
* **Per-file answers in the changed-files dialog** (§2.6).
* **Snapshots of read-only views.** A PDF, an image or a workbook has no buffer
  to cache and nothing a user could have "left" differently.
