#!/usr/bin/env python3
"""Integration test for per-workspace sessions (plan 0030).

Drives the real ./cmedit binary over a pseudo-terminal — the technique
CLAUDE.md documents for verifying interactive behaviour — and asserts what
0030 promises once there is more than one workspace:

  1. two workspaces do not clobber each other: a session in folder A and a
     session in folder B are two files under ~/.config/cmedit/sessions, and
     `--restore` from A's directory restores A while B's restores B (the bug
     the plan opened with was that the second exit silently overwrote the
     first);
  2. from a directory with no session of its own, `--restore` falls back to
     the most recently written session *and names the folder it came from*;
  3. the File menu offers past sessions, and picking one restores it onto the
     live editor — a union, so an already-open file is not duplicated, and no
     journal-recovery prompt re-runs;
  4. a file that moved on disk while the session was closed is reported, and
     both answers do what they say: "Latest on Disk" leaves the newest bytes
     in an unmodified buffer, "As You Left Them" installs the clean-exit
     snapshot as a *modified, unsaved* buffer over a file that still holds the
     newer bytes until an explicit Ctrl+S;
  5. a session that was killed offers no snapshots, because the stamp inside
     the snapshot directory no longer equals the session's `closed:` — the
     check that stops T1's content being offered for T2's session;
  6. `journal = off` writes no snapshots at all, ~/.cache/cmedit is never even
     created, and the changed-files prompt degrades to its single-button form.

The VT emulator, the PTY session helper, the throwaway-HOME isolation and the
poll-with-deadline discipline are pty_journal.py's, imported rather than
copied, exactly as pty_session.py imports them; the config writer and the
clean-quit helper are pty_session.py's own. The three tests read the same
screens and the same ~/.config directory and must not be allowed to drift
apart. None of them answers the startup capability queries, so the editor
stays on the portable emission path here too.

Every path handed to the editor is absolute. A relative one would be resolved
against whichever directory the process happened to start in — and since these
scenarios deliberately start the editor in *four* different directories, a
mistyped relative path opens a new empty buffer instead of failing, which is
the quietest way for a scenario to assert nothing at all.

    python3 docs/plans/bench/pty_workspace.py

Exit status is 1 if any scenario failed.
"""

import os
import shutil
import signal
import subprocess
import sys
import tempfile
import time

# Importing a sibling would otherwise drop a __pycache__ into the repo.
sys.dont_write_bytecode = True
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from pty_journal import (           # noqa: E402
    BINARY, ROOT, CTRL_Q, CTRL_S, ENTER, TAB,
    Failure, Session, check,
)
from pty_session import (           # noqa: E402
    ROWS, COLS, quit_clean, write_config,
)

ESC = b"\x1b"
ALT_F = b"\x1bf"                    # open the File menu

# The status-bar strings the plan pins, quoted here once so a wording change
# shows up as one diff rather than six.
RESTORED = "Session restored"
CHANGED_TITLE = "Files Changed Since This Session"
NO_COPY = "no saved copy from that session"
AS_LEFT = ("Restored 1 file as the session left them \u2014 unsaved, "
           "nothing has been written to disk")

OLD = b"old-line-one\nold-line-two\n"
NEW = b"new-line-one\nnew-line-two\n"


# ---------------------------------------------------------------------------
# Starting the editor *somewhere*
#
# 0030 §2.3 made --restore cwd-scoped, so the directory the process starts in
# is an input to almost every scenario here — and pty_journal.Session chdirs
# its child to $HOME, which is the one thing about it these tests cannot use.
# Patching that single call for the duration of the fork keeps the helper
# shared rather than forking a second copy of it: os.chdir is called by the
# child between pty.fork() and execve, and by nobody else in that window.

def start(home, args, cwd=None):
    if cwd is None:
        return Session(home, args, rows=ROWS, cols=COLS)
    real = os.chdir
    os.chdir = lambda _ignored: real(cwd)
    try:
        return Session(home, args, rows=ROWS, cols=COLS)
    finally:
        os.chdir = real


# ---------------------------------------------------------------------------
# The session files
#
# 0030 §2.1: one file per workspace folder, named
# <pathHash(folder)>-<basename>.session, plus 0025's singular
# ~/.config/cmedit/session for a run with no folder open. The name is
# computable from the folder, which is what makes the cwd lookup one stat —
# but this test deliberately never computes it. Listing the directory and
# reading what is inside is what proves the two workspaces got two files;
# recomputing the hash here would only assert that two copies of the same
# formula agree.

def sessions_dir(home):
    return os.path.join(home, ".config", "cmedit", "sessions")


def folderless_session(home):
    return os.path.join(home, ".config", "cmedit", "session")


def session_files(home):
    try:
        return sorted(n for n in os.listdir(sessions_dir(home))
                      if n.endswith(".session"))
    except OSError:
        return []


def read_session(path):
    """Parse one session file into (folder, closed, [(line, col, mtime, path)]).

    pty_session.read_session generalised over *where* the file is, which is the
    whole of 0030 — and, like it, deliberately a second independent parser:
    asserting against the code the editor writes with would assert nothing
    about the format. v2 only, since only v2 is ever written; a v1 file (which
    this test never produces) would fail on the missing mtime field, and that
    is the right way for a downgrade to show up.
    """
    with open(path, "r", encoding="utf-8") as f:
        lines = [l.rstrip("\r\n") for l in f]
    check(bool(lines) and lines[0].strip() == "cmedit-session 2",
          "%s has no v2 version line: %r" % (path, lines[:1]))
    folder, closed, files = None, None, []
    for line in lines[1:]:
        line = line.strip()
        if not line:
            continue
        if line.startswith("folder:"):
            folder = line[len("folder:"):].strip() or None
        elif line.startswith("closed:"):
            closed = int(line[len("closed:"):].strip())
        elif line.startswith("active:"):
            int(line[len("active:"):].strip())
        else:
            # Only the first three colons separate: a path may contain as many
            # as it likes, which is why the mtime went *before* it.
            ln, _, rest = line.partition(":")
            col, _, rest = rest.partition(":")
            mtime, _, path_ = rest.partition(":")
            check(mtime == "-" or mtime.isdigit(),
                  "entry has no mtime field: %r" % (line,))
            files.append((int(ln), int(col), mtime, path_))
    return folder, closed, files


def the_session(home, folder):
    """The one session file recording this folder, read. Fails if there isn't
    exactly one — which is the headline assertion in scenario 1."""
    hits = []
    for n in session_files(home):
        got = read_session(os.path.join(sessions_dir(home), n))
        if got[0] == folder:
            hits.append((n, got))
    check(len(hits) == 1,
          "expected exactly one session for %s, found %r"
          % (folder, [h[0] for h in hits]))
    return hits[0][1]


def wait_session(home, folder, want, timeout=10.0, session=None):
    """Poll until this folder's session file satisfies `want`, pumping the PTY.

    The session is rewritten after the batch that changes its *shape* and once
    more on exit, so this is "has it happened yet" rather than a sleep of a
    guessed length.
    """
    end = time.monotonic() + timeout
    while True:
        got = None
        for n in session_files(home):
            try:
                s = read_session(os.path.join(sessions_dir(home), n))
            except (OSError, Failure, ValueError):
                continue
            if s[0] == folder:
                got = s
        if got is not None and want(got):
            return got
        if time.monotonic() >= end:
            return got
        if session is not None:
            session.pump(0.1)
        else:
            time.sleep(0.1)


# ---------------------------------------------------------------------------
# The snapshot directories (0030 §2.7)

def snapshots_dir(home):
    return os.path.join(home, ".cache", "cmedit", "snapshots")


def snapshot_keys(home):
    try:
        return sorted(os.listdir(snapshots_dir(home)))
    except OSError:
        return []


def snapshot_set(home):
    """The one snapshot directory's (key, [.cmj names], stamp)."""
    keys = snapshot_keys(home)
    check(len(keys) == 1, "expected one snapshot directory, found %r" % (keys,))
    d = os.path.join(snapshots_dir(home), keys[0])
    names = sorted(n for n in os.listdir(d) if n.endswith(".cmj"))
    with open(os.path.join(d, "stamp"), "r", encoding="utf-8") as f:
        stamp = int(f.read().strip())
    return keys[0], names, stamp


# ---------------------------------------------------------------------------
# Fixtures

def new_home(tag):
    # realpath: the editor canonicalises every path it opens, so the session
    # file records the resolved form and a symlinked /tmp would never compare
    # equal. pty_session.new_home's twin, differing only in the prefix (a
    # failed run leaves the directory behind, and it should say which test left
    # it) and in creating no `work` directory — every scenario here makes its
    # own workspace folders.
    return os.path.realpath(tempfile.mkdtemp(prefix="cmedit-workspace-%s-" % tag))


def make_ws(home, name, files):
    """A workspace folder with files in it. Returns (dir, [paths])."""
    d = os.path.join(home, name)
    os.makedirs(d, exist_ok=True)
    paths = []
    for fname, content in files:
        p = os.path.join(d, fname)
        with open(p, "wb") as f:
            f.write(content)
        paths.append(p)
    return d, paths


def seed(home, folder, paths, needle):
    """Open a workspace folder and its files, then quit cleanly.

    A clean exit is what writes the session (0025) *and* the snapshots (0030
    §2.7), so this is the setup every scenario below starts from.
    """
    s = start(home, [folder] + list(paths), cwd=folder)
    try:
        check(s.wait_screen(needle, 12.0),
              "editor never showed %s under %s:\n%s"
              % (needle, folder, s.screen.text()))
        got = wait_session(home, folder,
                           lambda r: [f[3] for f in r[2]] == list(paths), 10.0, s)
        check(got is not None,
              "startup did not persist a session for %s: %r" % (folder, got))
        quit_clean(s, "editor in " + folder)
    finally:
        s.close()


def rewrite(path, content):
    """Replace a file's bytes and make sure the mtime really moved.

    The whole feature turns on `recorded != current`, so a rewrite the
    filesystem gave the same timestamp would silently make the scenario assert
    the opposite of what it says.
    """
    before = os.stat(path).st_mtime_ns
    for _ in range(200):
        with open(path, "wb") as f:
            f.write(content)
        if os.stat(path).st_mtime_ns != before:
            return
        time.sleep(0.01)
    raise Failure("could not move %s's mtime" % path)


def disk_is(path, want):
    def go():
        with open(path, "rb") as f:
            return f.read() == want
    return go


# ---------------------------------------------------------------------------
# Scenarios

def scenario_two_workspaces():
    """1. Two folders, two session files, and each restores its own."""
    home = new_home("two")
    try:
        A, (a,) = make_ws(home, "wsalpha", [("a.txt", b"alpha-one\nalpha-two\n")])
        B, (b,) = make_ws(home, "zsbravo", [("b.txt", b"bravo-one\nbravo-two\n")])

        seed(home, A, [a], "alpha-one")
        seed(home, B, [b], "bravo-one")

        # The headline: B's exit did not overwrite A's session.
        names = session_files(home)
        check(len(names) == 2, "expected two session files, got %r" % (names,))
        check(len(set(names)) == 2, "the two workspaces share a file name: %r" % (names,))
        check(not os.path.exists(folderless_session(home)),
              "a run with a folder open wrote the folderless session file")
        fA, _, filesA = the_session(home, A)
        fB, _, filesB = the_session(home, B)
        check([f[3] for f in filesA] == [a], "A's session is wrong: %r" % (filesA,))
        check([f[3] for f in filesB] == [b], "B's session is wrong: %r" % (filesB,))
        check(all(f[2].isdigit() for f in filesA + filesB),
              "v2 recorded no mtimes: %r" % (filesA + filesB,))

        # ... and each directory restores its own, with no origin note: an
        # exact cwd match is the directory you are standing in.
        s = start(home, ["--restore"], cwd=A)
        try:
            check(s.wait_screen(RESTORED, 12.0),
                  "--restore in A restored nothing:\n" + s.screen.text())
            check(s.screen.has("alpha-one") and not s.screen.has("bravo-one"),
                  "--restore in A produced the other workspace:\n" + s.screen.text())
            check(s.screen.has("wsalpha") and not s.screen.has("zsbravo"),
                  "A's restore opened the wrong folder:\n" + s.screen.text())
            check(not s.screen.has(RESTORED + " from"),
                  "an exact cwd match named a folder anyway:\n" + s.screen.text())
            quit_clean(s, "the restore in A")
        finally:
            s.close()

        s = start(home, ["--restore"], cwd=B)
        try:
            check(s.wait_screen(RESTORED, 12.0),
                  "--restore in B restored nothing:\n" + s.screen.text())
            check(s.screen.has("bravo-one") and not s.screen.has("alpha-one"),
                  "--restore in B produced the other workspace:\n" + s.screen.text())
            check(s.screen.has("zsbravo") and not s.screen.has("wsalpha"),
                  "B's restore opened the wrong folder:\n" + s.screen.text())
            quit_clean(s, "the restore in B")
        finally:
            s.close()

        # Two restores and two more exits later, still two files, still one
        # workspace each: the photograph invariant, not a merge.
        check(session_files(home) == names,
              "restoring changed the set of session files: %r" % (session_files(home),))
        check([f[3] for f in the_session(home, A)[2]] == [a],
              "A's session picked up B's file")
        check([f[3] for f in the_session(home, B)[2]] == [b],
              "B's session picked up A's file")
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_fallback_names_it():
    """2. No session for this directory: the newest one, and whose it was."""
    home = new_home("fallback")
    try:
        A, (a,) = make_ws(home, "wsalpha", [("a.txt", b"alpha-one\n")])
        B, (b,) = make_ws(home, "zsbravo", [("b.txt", b"bravo-one\n")])
        C = os.path.join(home, "elsewhere")
        os.makedirs(C)

        seed(home, A, [a], "alpha-one")
        seed(home, B, [b], "bravo-one")       # written last, so the newest

        s = start(home, ["--restore"], cwd=C)
        try:
            # The note is what makes the fallback non-astonishing, and the
            # folder is abbreviated to ~ because $HOME is the throwaway one.
            check(s.wait_screen(RESTORED + " from ~/zsbravo", 12.0),
                  "the fallback did not name the folder it came from:\n"
                  + s.screen.text())
            check(s.screen.has("bravo-one") and not s.screen.has("alpha-one"),
                  "the fallback did not pick the newest session:\n" + s.screen.text())
            quit_clean(s, "the fallback restore")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_menu_restore():
    """3. File ▸ a past session, onto a live editor, without duplicating."""
    home = new_home("menu")
    try:
        A, (a,) = make_ws(home, "wsalpha", [("a.txt", b"alpha-one\n")])
        B, (b, c) = make_ws(home, "zsbravo",
                            [("b.txt", b"bravo-one\n"), ("c.txt", b"charlie-one\n")])
        E = os.path.join(home, "elsewhere")
        os.makedirs(E)

        seed(home, A, [a], "alpha-one")
        seed(home, B, [b, c], "bravo-one")

        # Start with A's file already open and no folder, from a directory
        # with no session of its own: nothing here is a restore.
        s = start(home, [a], cwd=E)
        try:
            check(s.wait_screen("alpha-one", 12.0),
                  "editor never showed a.txt:\n" + s.screen.text())
            check(not s.screen.has(RESTORED),
                  "a plain start restored a session:\n" + s.screen.text())

            s.send(ALT_F)
            check(s.wait_screen("zsbravo (2 files)", 8.0),
                  "the File menu lists no sessions:\n" + s.screen.text())
            check(s.screen.has("wsalpha (1 file)"),
                  "session labels are wrong (basename, count, plural):\n"
                  + s.screen.text())

            # `w` is the mnemonic assignSessionMnemonics can give wsalpha: the
            # static File menu claims n o f i s a l v c d t x and the recents
            # take digits, so the initial is free. Reaching the row by pressing
            # it is the assertion.
            s.send(b"w")
            check(s.wait_screen(RESTORED + " from ~/wsalpha", 12.0),
                  "the menu restore said nothing:\n" + s.screen.text())
            check(s.screen.has("wsalpha") and s.screen.has("alpha-one"),
                  "the menu restore did not open the folder and its file:\n"
                  + s.screen.text())
            # A union, and a.txt was already open: switched to, not duplicated.
            # The status bar prefixes a document count only when there is more
            # than one open.
            check(not s.screen.has("[1/"),
                  "the already-open file was opened a second time:\n"
                  + s.screen.text())
            # Journal recovery is a startup question, answered once per
            # process; a menu restore must not re-ask it.
            check(not s.screen.has("Unsaved Changes Recovered"),
                  "a menu restore re-ran journal recovery:\n" + s.screen.text())
            check(not s.screen.has(CHANGED_TITLE),
                  "nothing moved on disk, yet the changed-files prompt appeared:\n"
                  + s.screen.text())
            quit_clean(s, "the menu-restored editor")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def changed_setup(tag):
    """A clean exit with snapshots, then the file rewritten underneath it.

    Its own HOME per answer, deliberately: the first run's clean exit records
    the *new* mtime and re-snapshots, so a second answer in the same HOME would
    be answering a question nothing is asking any more.
    """
    home = new_home(tag)
    A, (a,) = make_ws(home, "wsalpha", [("a.txt", OLD)])
    seed(home, A, [a], "old-line-one")

    # A clean exit writes the snapshot set, and its stamp is the value that
    # landed in the session file — which is the whole of the trust check.
    key, cmjs, stamp = snapshot_set(home)
    check(len(cmjs) == 1 and cmjs[0].endswith("-a.txt.cmj"),
          "the clean exit snapshotted %r" % (cmjs,))
    check(stamp == the_session(home, A)[1],
          "the snapshot stamp is not the session's closed: value")

    rewrite(a, NEW)
    return home, A, a


def scenario_changed_latest():
    """4a. Files changed on disk: Latest on Disk leaves the newest bytes."""
    home, A, a = changed_setup("latest")
    try:
        s = start(home, ["--restore"], cwd=A)
        try:
            check(s.wait_screen(CHANGED_TITLE, 12.0),
                  "no changed-files prompt after a restore:\n" + s.screen.text())
            check(s.screen.has("\u25c6 a.txt"),
                  "the prompt does not list the file with a \u25c6:\n" + s.screen.text())
            check(s.screen.has("Latest on Disk") and s.screen.has("As You Left Them"),
                  "a usable snapshot did not produce both answers:\n" + s.screen.text())
            check(not s.screen.has(NO_COPY),
                  "a snapshot exists, yet the prompt says it does not:\n"
                  + s.screen.text())

            # Esc is button 0: the files are already open at their newest
            # state, so the escape hatch is also the default answer.
            s.send(ESC)
            check(s.until(lambda: not s.screen.has(CHANGED_TITLE), 8.0),
                  "Esc did not dismiss the prompt:\n" + s.screen.text())
            check(s.screen.has("new-line-one") and not s.screen.has("old-line-one"),
                  "the buffer does not hold the disk version:\n" + s.screen.text())
            check(not s.screen.has("\u25cf a.txt"),
                  "Latest on Disk left the document modified:\n" + s.screen.text())
            with open(a, "rb") as f:
                check(f.read() == NEW, "the restore wrote to disk")
            # Nothing unsaved: Ctrl+Q goes without a confirmation.
            quit_clean(s, "the Latest-on-Disk editor")
        finally:
            s.close()
        with open(a, "rb") as f:
            check(f.read() == NEW, "the file changed after a Latest-on-Disk session")
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_changed_as_left():
    """4b. As You Left Them: modified, unsaved, and disk untouched until Ctrl+S."""
    home, A, a = changed_setup("asleft")
    try:
        s = start(home, ["--restore"], cwd=A)
        try:
            check(s.wait_screen(CHANGED_TITLE, 12.0),
                  "no changed-files prompt after a restore:\n" + s.screen.text())
            s.send(TAB)                 # Latest on Disk -> As You Left Them
            s.pump(0.3)
            s.send(ENTER)
            check(s.wait_screen(AS_LEFT, 10.0),
                  "As You Left Them did not report:\n" + s.screen.text())
            check(s.screen.has("old-line-one") and not s.screen.has("new-line-one"),
                  "the snapshot was not installed:\n" + s.screen.text())
            # Modified (●, status bar) and changed-on-disk (◆, explorer) from
            # the first frame — the second of which the ordinary staleness
            # machinery could not show, since the load just happened.
            check(s.screen.has("\u25cf a.txt"),
                  "the installed snapshot is not marked modified:\n" + s.screen.text())
            check(s.screen.has("\u25c6"),
                  "the installed snapshot carries no \u25c6:\n" + s.screen.text())
            with open(a, "rb") as f:
                got = f.read()
            check(got == NEW,
                  "installing a snapshot wrote to disk: %r" % (got,))

            # Saving over the newer file is a deliberate Ctrl+S by a user who
            # has been told twice.
            s.send(CTRL_S)
            check(s.until(disk_is(a, OLD), 10.0),
                  "Ctrl+S did not write the session's version:\n" + s.screen.text())
            quit_clean(s, "the As-You-Left-Them editor")
        finally:
            s.close()
        with open(a, "rb") as f:
            check(f.read() == OLD, "the saved file is not the session's version")
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_crashed_no_snapshots():
    """5. A killed session under the same key cannot use T1's snapshots."""
    home = new_home("stamp")
    try:
        A, (a, b) = make_ws(home, "wsalpha",
                            [("a.txt", OLD), ("b.txt", b"bee-one\n")])
        seed(home, A, [a], "old-line-one")
        key, cmjs, stamp1 = snapshot_set(home)
        check(len(cmjs) == 1, "the clean exit snapshotted %r" % (cmjs,))
        check(stamp1 == the_session(home, A)[1],
              "T1's stamp is not T1's closed: value")

        # A second session under the same key, re-stamped by a shape change
        # (a second file) and then killed, so no snapshots are written.
        s = start(home, [A, a, b], cwd=A)
        try:
            check(s.wait_screen("old-line-one", 12.0),
                  "editor never showed a.txt:\n" + s.screen.text())
            got = wait_session(home, A,
                               lambda r: len(r[2]) == 2 and r[1] != stamp1, 10.0, s)
            check(got is not None and got[1] != stamp1,
                  "the shape change did not re-stamp the session: %r" % (got,))
            stamp2 = got[1]
            s.kill(signal.SIGKILL)
            check(s.wait_exit(5.0), "process survived SIGKILL")
        finally:
            s.close()

        key2, cmjs2, stampOnDisk = snapshot_set(home)
        check((key2, cmjs2) == (key, cmjs) and stampOnDisk == stamp1,
              "SIGKILL touched the snapshot set: %r %r" % (cmjs2, stampOnDisk))
        check(stampOnDisk != the_session(home, A)[1],
              "the crashed session's closed: still matches T1's stamp")

        rewrite(a, NEW)

        s = start(home, ["--restore"], cwd=A)
        try:
            # The stamp mismatch is what makes "a crashed session has no
            # snapshots" fall out: the file is still reported as changed, but
            # in the single-button form, because there is nothing to offer.
            check(s.wait_screen(CHANGED_TITLE, 12.0),
                  "no changed-files prompt after the crashed session:\n"
                  + s.screen.text())
            check(s.screen.has(NO_COPY),
                  "stale snapshots were treated as usable:\n" + s.screen.text())
            check(not s.screen.has("As You Left Them"),
                  "T1's content was offered for T2's session:\n" + s.screen.text())
            check(s.screen.has("OK"),
                  "the degraded prompt has no OK button:\n" + s.screen.text())
            s.send(ENTER)
            check(s.until(lambda: not s.screen.has(CHANGED_TITLE), 8.0),
                  "OK did not dismiss the prompt:\n" + s.screen.text())
            check(s.screen.has("new-line-one") and not s.screen.has("old-line-one"),
                  "the content did not come from disk:\n" + s.screen.text())
            check(not s.screen.has("\u25cf a.txt"),
                  "the document is modified with nothing to have modified it:\n"
                  + s.screen.text())
            quit_clean(s, "the post-crash restore")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_journal_off():
    """6. journal = off: no snapshots, no ~/.cache/cmedit, one button."""
    home = new_home("joff")
    try:
        write_config(home, "journal = off\n")
        A, (a,) = make_ws(home, "wsalpha", [("a.txt", OLD)])
        seed(home, A, [a], "old-line-one")

        cache = os.path.join(home, ".cache", "cmedit")
        check(not os.path.exists(cache),
              "journal = off still created %s" % cache)
        check(snapshot_keys(home) == [], "journal = off wrote snapshots")
        # The session itself is unaffected: it holds paths, cursors and mtimes,
        # never content, so it is not what the key gates.
        check([f[3] for f in the_session(home, A)[2]] == [a],
              "journal = off cost the session its file list")

        rewrite(a, NEW)

        s = start(home, ["--restore"], cwd=A)
        try:
            check(s.wait_screen(CHANGED_TITLE, 12.0),
                  "no changed-files prompt with journal = off:\n" + s.screen.text())
            check(s.screen.has(NO_COPY),
                  "the prompt claims a copy that was never cached:\n" + s.screen.text())
            check(not s.screen.has("As You Left Them"),
                  "a second choice was offered with nothing behind it:\n"
                  + s.screen.text())
            s.send(ENTER)
            check(s.until(lambda: not s.screen.has(CHANGED_TITLE), 8.0),
                  "OK did not dismiss the prompt:\n" + s.screen.text())
            check(s.screen.has("new-line-one") and not s.screen.has("old-line-one"),
                  "the content did not come from disk:\n" + s.screen.text())
            quit_clean(s, "the journal-off restore")
        finally:
            s.close()
        check(not os.path.exists(cache),
              "%s appeared over a journal = off session" % cache)
    finally:
        shutil.rmtree(home, ignore_errors=True)


SCENARIOS = [
    ("two workspaces, two sessions", scenario_two_workspaces),
    ("fallback names the folder", scenario_fallback_names_it),
    ("File menu restores a session", scenario_menu_restore),
    ("changed: Latest on Disk", scenario_changed_latest),
    ("changed: As You Left Them", scenario_changed_as_left),
    ("crashed session, stale stamp", scenario_crashed_no_snapshots),
    ("journal = off, no snapshots", scenario_journal_off),
]


def main():
    if not os.path.exists(BINARY):
        print("building %s ..." % BINARY)
        if subprocess.call(["make"], cwd=ROOT) != 0:
            print("make failed")
            return 2
    failed = 0
    t0 = time.monotonic()
    for i, (name, fn) in enumerate(SCENARIOS, 1):
        t = time.monotonic()
        try:
            fn()
            print("scenario %d  %-32s PASS  (%.1f s)"
                  % (i, name, time.monotonic() - t))
        except Exception as e:
            failed += 1
            print("scenario %d  %-32s FAIL  (%.1f s)  %s"
                  % (i, name, time.monotonic() - t, e))
        sys.stdout.flush()
    print("%d passed, %d failed in %.1f s"
          % (len(SCENARIOS) - failed, failed, time.monotonic() - t0))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
