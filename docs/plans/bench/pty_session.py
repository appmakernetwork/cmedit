#!/usr/bin/env python3
"""Integration test for session restore (plan 0025).

Drives the real ./cmedit binary over a pseudo-terminal — the technique
CLAUDE.md documents for verifying interactive behaviour — and asserts what
0025 promises about ~/.config/cmedit/session:

  1. a clean exit writes the shape the editor was in (folder, ordered paths,
     active index, cursors), and `--restore` puts exactly that back;
  2. a file that has since been deleted is skipped and counted
     ("Restored 1 of 2 files"), not recreated as an empty buffer;
  3. `restore-session = true` restores an *argless* start and deliberately
     does not restore one that names a file;
  4. restore composes with 0011's crash journal — restore runs first, so the
     recovered content lands *inside* the restored document rather than in a
     second copy of it;
  5. `--restore` with nothing to restore says so and leaves a usable editor.

The VT emulator, the PTY session helper, the throwaway-HOME isolation and the
poll-with-deadline discipline are pty_journal.py's, imported rather than
copied — the two tests read the same screens and must not drift apart. That
module answers none of the startup capability queries, so the editor stays on
the portable emission path here too.

    python3 docs/plans/bench/pty_session.py

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

# The screen reconstruction and the PTY plumbing are the journal test's. Session
# is one editor process under a PTY with HOME (and every XDG_*) pointed at a
# throwaway directory, so the developer's own config is never read or written.
from pty_journal import (           # noqa: E402
    BINARY, ROOT, CTRL_Q, CTRL_S, ENTER, TAB,
    Failure, Session, check, journals, wait_journals,
)

CTRL_W = b"\x17"                    # close file
DOWN = b"\x1b[B"
ALT_2 = b"\x1b2"                    # switch to the 2nd open file

# Wide enough that the status bar's right-hand zones ("Ln 3, Col 1") are never
# clipped by a long left-hand message.
ROWS, COLS = 40, 140


# ---------------------------------------------------------------------------
# The session file
#
# ~/.config/cmedit/session, beside `recent` and `history`. A version line, an
# optional `folder:`, an `active:` index, then one 1-based `line:col:path` per
# open document in tab order — the recents encoding, reused deliberately.

def session_path(home):
    return os.path.join(home, ".config", "cmedit", "session")


def read_session(home):
    """Parse the session file into (folder, [(line, col, path)], active).

    Deliberately a second, independent parser: asserting against the same code
    the editor writes with would assert nothing about the format.
    """
    with open(session_path(home), "r", encoding="utf-8") as f:
        lines = [l.rstrip("\r\n") for l in f]
    check(bool(lines) and lines[0].strip() == "cmedit-session 1",
          "session file has no version line: %r" % (lines[:1],))
    folder, files, active = None, [], 0
    for line in lines[1:]:
        line = line.strip()
        if not line:
            continue
        if line.startswith("folder:"):
            folder = line[len("folder:"):].strip() or None
        elif line.startswith("active:"):
            active = int(line[len("active:"):].strip())
        else:
            ln, _, rest = line.partition(":")
            col, _, path = rest.partition(":")
            files.append((int(ln), int(col), path))
    return folder, files, active


def wait_session(home, want, timeout=8.0, session=None):
    """Poll until the session file satisfies `want`, pumping the PTY meanwhile.

    The session is rewritten when its *shape* changes (a file opened, closed or
    switched to) after the batch that changed it, and once more on exit — so
    this is "has it happened yet", never a sleep of a guessed length.
    """
    end = time.monotonic() + timeout
    while True:
        try:
            got = read_session(home)
        except (OSError, Failure, ValueError):
            got = None
        if got is not None and want(got):
            return got
        if time.monotonic() >= end:
            return got
        if session is not None:
            session.pump(0.1)
        else:
            time.sleep(0.1)


# ---------------------------------------------------------------------------
# Fixtures

def new_home(tag):
    # realpath: the editor canonicalises every path it opens, so the session
    # file records the resolved form and a symlinked /tmp would never compare
    # equal.
    home = os.path.realpath(tempfile.mkdtemp(prefix="cmedit-session-%s-" % tag))
    os.makedirs(os.path.join(home, "work"))
    return home


def write_file(home, name, content):
    path = os.path.join(home, "work", name)
    with open(path, "wb") as f:
        f.write(content)
    return path


def write_config(home, text):
    d = os.path.join(home, ".config", "cmedit")
    os.makedirs(d, exist_ok=True)
    with open(os.path.join(d, "config"), "w", encoding="utf-8") as f:
        f.write(text)


def start(home, args):
    return Session(home, args, rows=ROWS, cols=COLS)


def quit_clean(s, what="editor"):
    """Ctrl+Q with nothing unsaved: no confirmation, so the process just goes."""
    s.send(CTRL_Q)
    check(s.wait_exit(10.0), "%s did not quit" % what)


# ---------------------------------------------------------------------------
# Scenarios

def scenario_round_trip():
    """1. Quit, --restore, and get the same files, order, active doc and cursor."""
    home = new_home("trip")
    try:
        a = write_file(home, "a.txt", b"alpha-one\nalpha-two\nalpha-three\nalpha-four\n")
        b = write_file(home, "b.txt", b"bravo-one\nbravo-two\nbravo-three\n")

        s = start(home, [a, b])
        try:
            check(s.wait_screen("alpha-one", 10.0),
                  "editor never showed a.txt:\n" + s.screen.text())
            s.send(ALT_2)                       # make b.txt the active document
            check(s.wait_screen("bravo-one", 8.0),
                  "Alt+2 did not switch to b.txt:\n" + s.screen.text())
            s.send(DOWN)
            s.send(DOWN)
            check(s.wait_screen("Ln 3, Col 1", 8.0),
                  "cursor never reached line 3:\n" + s.screen.text())
            quit_clean(s)                       # nothing edited: no confirmation
        finally:
            s.close()

        check(os.path.exists(session_path(home)),
              "a clean exit wrote no session file")
        folder, files, active = read_session(home)
        check(folder is None, "session recorded a folder that was never open: %r" % folder)
        check([f[2] for f in files] == [a, b],
              "session paths are wrong or out of order: %r" % (files,))
        check(active == 1, "session recorded the wrong active index: %r" % active)
        check(files[0][:2] == (1, 1),
              "untouched a.txt did not record a 1:1 cursor: %r" % (files[0],))
        check(files[1][:2] == (3, 1),
              "b.txt's cursor was not recorded: %r" % (files[1],))

        s = start(home, ["--restore"])
        try:
            check(s.wait_screen("Session restored", 12.0),
                  "--restore did not report a full restore:\n" + s.screen.text())
            check(s.screen.has("bravo-one"),
                  "the active document is not b.txt:\n" + s.screen.text())
            check(s.wait_screen("Ln 3, Col 1", 8.0),
                  "b.txt's cursor was not restored:\n" + s.screen.text())
            # Both files came back, in order: quitting rewrites the same shape.
            quit_clean(s)
        finally:
            s.close()
        folder, files, active = read_session(home)
        check([f[2] for f in files] == [a, b] and active == 1,
              "the restored session did not round-trip: %r %r" % (files, active))
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_partial():
    """2. A file that is gone is skipped and counted, not recreated."""
    home = new_home("partial")
    try:
        a = write_file(home, "a.txt", b"alpha-one\nalpha-two\n")
        b = write_file(home, "b.txt", b"bravo-one\n")

        s = start(home, [a, b])
        try:
            check(s.wait_screen("alpha-one", 10.0), "editor never showed a.txt")
            quit_clean(s)
        finally:
            s.close()
        _, files, _ = read_session(home)
        check([f[2] for f in files] == [a, b], "session did not record both: %r" % (files,))

        os.remove(b)

        s = start(home, ["--restore"])
        try:
            check(s.wait_screen("Restored 1 of 2 files", 12.0),
                  "no partial-restore note:\n" + s.screen.text())
            check(s.screen.has("alpha-one"),
                  "a.txt was not restored:\n" + s.screen.text())
            check(not s.screen.has("bravo-one"),
                  "the deleted file's old content came back:\n" + s.screen.text())
            quit_clean(s)
        finally:
            s.close()
        # The missing file is dropped, not carried forward as an empty buffer.
        _, files, _ = read_session(home)
        check([f[2] for f in files] == [a],
              "the deleted file survived into the new session: %r" % (files,))
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_config_key():
    """3. `restore-session = true` restores an argless start, and only that."""
    home = new_home("cfgkey")
    try:
        a = write_file(home, "a.txt", b"alpha-one\nalpha-two\n")
        c = write_file(home, "c.txt", b"charlie-one\n")

        s = start(home, [a])                    # seed a session to restore
        try:
            check(s.wait_screen("alpha-one", 10.0), "editor never showed a.txt")
            quit_clean(s)
        finally:
            s.close()
        _, files, _ = read_session(home)
        check([f[2] for f in files] == [a], "seed session is wrong: %r" % (files,))

        write_config(home, "restore-session = true\n")

        s = start(home, [])                     # no arguments: the key applies
        try:
            check(s.wait_screen("Session restored", 12.0),
                  "the config key did not restore an argless start:\n" + s.screen.text())
            check(s.screen.has("alpha-one"),
                  "a.txt was not restored:\n" + s.screen.text())
            quit_clean(s)
        finally:
            s.close()

        s = start(home, [c])                    # naming a file overrides the key
        try:
            check(s.wait_screen("charlie-one", 10.0),
                  "editor never showed c.txt:\n" + s.screen.text())
            s.pump(2.0)
            check(not s.screen.has("alpha-one"),
                  "the session was restored under a named file:\n" + s.screen.text())
            check(not s.screen.has("Session restored"),
                  "a named file still reported a restore:\n" + s.screen.text())
            quit_clean(s)
        finally:
            s.close()
        # The decisive check: only c.txt was ever open, so only c.txt is recorded.
        _, files, active = read_session(home)
        check([f[2] for f in files] == [c] and active == 0,
              "the session under a named file is not just that file: %r" % (files,))
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_crash_compose():
    """4. Crash, --restore: the journal's content lands in the restored document.

    Covers both persist paths: the *initial* shape must be on disk before any
    shape change (this test found the release where it was not — a run that
    only typed never wrote a session, and a SIGKILL left the previous
    session's file for --restore to trust), and a real shape change (a second
    file, closed with Ctrl+W) must rewrite it before the crash.
    """
    home = new_home("crash")
    try:
        note = write_file(home, "note.txt", b"note-alpha\nnote-beta\n")
        other = write_file(home, "other.txt", b"other-one\n")
        expected = b"HELLO note-alpha\nnote-beta\n"

        s = start(home, [other, note])
        try:
            check(s.wait_screen("other-one", 10.0),
                  "editor never showed other.txt:\n" + s.screen.text())
            # The startup shape itself must be persisted, before any change:
            # this is the write a shape-quiet session relies on at SIGKILL.
            got0 = wait_session(
                home, lambda r: sorted(f[2] for f in r[1]) == sorted([other, note]),
                8.0, s)
            check(got0 is not None,
                  "startup did not persist the initial session shape")
            s.send(CTRL_W)                      # close it: a session shape change
            check(s.wait_screen("note-alpha", 8.0),
                  "Ctrl+W did not leave note.txt showing:\n" + s.screen.text())
            got = wait_session(home, lambda r: [f[2] for f in r[1]] == [note], 8.0, s)
            check(got is not None and [f[2] for f in got[1]] == [note],
                  "the shape change did not persist a session: %r" % (got,))

            s.type("HELLO ")
            names = wait_journals(home, lambda n: len(n) == 1, 8.0, s)
            check(len(names) == 1, "no journal appeared after typing: %r" % (names,))
            with open(note, "rb") as f:
                check(f.read() == b"note-alpha\nnote-beta\n",
                      "the real file was written before any save")
            s.kill(signal.SIGKILL)              # a crash, not a graceful teardown
            check(s.wait_exit(5.0), "process survived SIGKILL")
        finally:
            s.close()

        check(os.path.exists(session_path(home)), "SIGKILL lost the session file")
        _, files, active = read_session(home)
        check([f[2] for f in files] == [note] and active == 0,
              "the surviving session does not record note.txt: %r" % (files,))
        check(len(journals(home)) == 1, "SIGKILL lost the journal")

        s = start(home, ["--restore"])
        try:
            # Restore runs first, so the recovery prompt arrives over a session
            # that is already back.
            check(s.wait_screen("Unsaved Changes Recovered", 12.0),
                  "no recovery dialog after a restore:\n" + s.screen.text())
            check(s.screen.has("note.txt"),
                  "recovery dialog does not name the file:\n" + s.screen.text())
            s.send(ENTER)                       # button 0 = Recover
            check(s.wait_screen("Recovered 1 unsaved file", 8.0),
                  "Recover did not report:\n" + s.screen.text())
            check(s.wait_screen("HELLO note-alpha", 8.0),
                  "the crash-typed text is not in the restored document:\n"
                  + s.screen.text())
            with open(note, "rb") as f:
                check(f.read() == b"note-alpha\nnote-beta\n",
                      "recovery wrote to disk by itself")
            s.send(CTRL_S)

            def saved():
                with open(note, "rb") as f:
                    return f.read() == expected
            check(s.until(saved, 8.0), "Ctrl+S did not write the recovered text")
            check(wait_journals(home, lambda n: n == [], 6.0, s) == [],
                  "journal survived the save: %r" % journals(home))
            quit_clean(s)
        finally:
            s.close()

        with open(note, "rb") as f:
            got = f.read()
        check(got == expected,
              "recovered file is not byte-identical: %r vs %r" % (got, expected))
        # One document, not two: had recovery opened its own copy of note.txt
        # alongside the restored one, the session would now record it twice.
        _, files, _ = read_session(home)
        check([f[2] for f in files] == [note],
              "recovery landed beside the restored document: %r" % (files,))
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_no_session():
    """5. --restore with nothing to restore says so and still gives an editor."""
    home = new_home("empty")
    try:
        s = start(home, ["--restore"])
        try:
            check(s.wait_screen("No previous session to restore", 12.0),
                  "--restore said nothing about the missing session:\n"
                  + s.screen.text())
            check(s.screen.has("untitled"),
                  "no untitled buffer to work in:\n" + s.screen.text())
            s.type("still usable")
            check(s.wait_screen("still usable", 8.0),
                  "the editor did not accept input:\n" + s.screen.text())
            s.send(CTRL_Q)
            check(s.wait_screen("Save changes to untitled?", 8.0),
                  "no quit confirmation:\n" + s.screen.text())
            s.send(TAB)                         # Save -> Don't Save
            s.send(ENTER)
            check(s.wait_exit(10.0), "editor did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


SCENARIOS = [
    ("round trip: files, order, cursor", scenario_round_trip),
    ("partial restore counts the gone", scenario_partial),
    ("config key, and only argless", scenario_config_key),
    ("crash: journal into restored doc", scenario_crash_compose),
    ("no session to restore", scenario_no_session),
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
            print("scenario %d  %-34s PASS  (%.1f s)"
                  % (i, name, time.monotonic() - t))
        except Exception as e:
            failed += 1
            print("scenario %d  %-34s FAIL  (%.1f s)  %s"
                  % (i, name, time.monotonic() - t, e))
        sys.stdout.flush()
    print("%d passed, %d failed in %.1f s"
          % (len(SCENARIOS) - failed, failed, time.monotonic() - t0))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
