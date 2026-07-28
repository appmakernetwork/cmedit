#!/usr/bin/env python3
"""Integration test for the crash-safe edit journal (plan 0011 §4).

Drives the real ./cmedit binary over a pseudo-terminal — the technique
CLAUDE.md documents for verifying interactive behaviour — and asserts the four
things the plan promises about ~/.cache/cmedit/journal:

  1. a SIGKILL leaves a journal, and the next startup offers it back
     byte-identically (SIGKILL specifically: SIGTERM runs the graceful path,
     which deliberately *preserves* journals but is not a crash);
  2. saving a document removes its journal;
  3. a clean exit removes journals, and the next startup is silent;
  4. "Keep for later" leaves them on disk — a kept journal was never adopted,
     so the exiting session must not delete it.

Every scenario runs against a throwaway HOME (and explicit XDG_* pointing
inside it), so the developer's own cache is never read, written or polluted.

Unlike pty_startup.py / pty_rss.py, which only need to know that bytes came
out, this one has to read the screen: the recovery prompt is a dialog. The VT
emulator below is the small one CLAUDE.md describes — cursor positioning,
printable UTF-8, erases, DECSTBM band scrolling and REP, with OSC/DCS/APC
strings skipped. It answers none of the startup capability queries, so the
editor stays on the portable emission path.

Waits are polls with generous deadlines, not fixed sleeps: the write-behind is
a 2 s debounce after the last edit batch, and the journal sweep runs after a
key batch, so both are "wait until it happened" rather than "wait 2 s".

    python3 docs/plans/bench/pty_journal.py

Exit status is the number of failed scenarios.
"""

import fcntl
import os
import pty
import select
import shutil
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import time

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))))
BINARY = os.path.join(ROOT, "cmedit")

ROWS, COLS = 40, 120

# Keys, as the raw bytes a terminal would send.
CTRL_S = b"\x13"
CTRL_Q = b"\x11"
ENTER = b"\r"
TAB = b"\t"


# ---------------------------------------------------------------------------
# A very small VT emulator: enough to reconstruct what is on the screen.

class Screen:
    def __init__(self, rows, cols):
        self.rows, self.cols = rows, cols
        self.grid = [[" "] * cols for _ in range(rows)]
        self.cy = self.cx = 0
        self.top, self.bot = 0, rows - 1
        self.pending = b""
        self.last = " "

    def text(self):
        return "\n".join("".join(r).rstrip() for r in self.grid)

    def has(self, s):
        return s in self.text()

    # -- painting ----------------------------------------------------------

    def _put(self, ch):
        if 0 <= self.cy < self.rows and 0 <= self.cx < self.cols:
            self.grid[self.cy][self.cx] = ch
        self.last = ch
        self.cx += 1
        if self.cx >= self.cols:
            self.cx = self.cols - 1

    def _blank_row(self):
        return [" "] * self.cols

    def _scroll_up(self, n):
        for _ in range(n):
            del self.grid[self.top]
            self.grid.insert(self.bot, self._blank_row())

    def _scroll_down(self, n):
        for _ in range(n):
            del self.grid[self.bot]
            self.grid.insert(self.top, self._blank_row())

    def _index(self):
        if self.cy == self.bot:
            self._scroll_up(1)
        else:
            self.cy = min(self.rows - 1, self.cy + 1)

    # -- parsing -----------------------------------------------------------

    def feed(self, data):
        b = self.pending + data
        i, n = 0, len(b)
        while i < n:
            c = b[i]
            if c == 0x1B:
                j = self._escape(b, i, n)
                if j is None:
                    break                      # incomplete: wait for more
                i = j
            elif c == 0x0D:
                self.cx = 0
                i += 1
            elif c == 0x0A:
                self._index()
                i += 1
            elif c == 0x08:
                self.cx = max(0, self.cx - 1)
                i += 1
            elif c == 0x09:
                self.cx = min(self.cols - 1, (self.cx // 8 + 1) * 8)
                i += 1
            elif c < 0x20 or c == 0x7F:
                i += 1
            else:
                ln = 1
                if c >= 0xF0:
                    ln = 4
                elif c >= 0xE0:
                    ln = 3
                elif c >= 0xC0:
                    ln = 2
                if i + ln > n:
                    break
                try:
                    self._put(b[i:i + ln].decode("utf-8"))
                except UnicodeDecodeError:
                    self._put("?")
                i += ln
        self.pending = b[i:]

    def _escape(self, b, i, n):
        if i + 1 >= n:
            return None
        c = b[i + 1]
        if c == 0x5B:                                   # CSI
            j = i + 2
            while j < n and not (0x40 <= b[j] <= 0x7E):
                j += 1
            if j >= n:
                return None
            self._csi(b[i + 2:j].decode("latin-1"), chr(b[j]))
            return j + 1
        if c in (0x5D, 0x50, 0x5F, 0x5E, 0x58):         # OSC / DCS / APC / PM / SOS
            j = i + 2
            while j < n:
                if b[j] == 0x07:
                    return j + 1
                if b[j] == 0x1B and j + 1 < n and b[j + 1] == 0x5C:
                    return j + 2
                if b[j] == 0x1B and j + 1 >= n:
                    return None
                j += 1
            return None
        return i + 2                                    # two-byte escape

    def _csi(self, params, final):
        private = params[:1] in ("?", ">", "<", "=")
        if private:
            params = params[1:]
        nums = []
        for p in params.split(";"):
            try:
                nums.append(int(p))
            except ValueError:
                nums.append(None)
        def arg(k, d=1):
            v = nums[k] if k < len(nums) else None
            return d if v is None else v

        if private and final not in ("J", "K"):
            return                                      # mode set/reset etc.
        if final in ("H", "f"):
            self.cy = max(0, min(self.rows - 1, arg(0) - 1))
            self.cx = max(0, min(self.cols - 1, arg(1) - 1))
        elif final == "A":
            self.cy = max(0, self.cy - arg(0))
        elif final == "B":
            self.cy = min(self.rows - 1, self.cy + arg(0))
        elif final == "C":
            self.cx = min(self.cols - 1, self.cx + arg(0))
        elif final == "D":
            self.cx = max(0, self.cx - arg(0))
        elif final == "G":
            self.cx = max(0, min(self.cols - 1, arg(0) - 1))
        elif final == "d":
            self.cy = max(0, min(self.rows - 1, arg(0) - 1))
        elif final == "J":
            mode = arg(0, 0)
            if mode == 0:
                for x in range(self.cx, self.cols):
                    self.grid[self.cy][x] = " "
                for y in range(self.cy + 1, self.rows):
                    self.grid[y] = self._blank_row()
            elif mode == 1:
                for y in range(self.cy):
                    self.grid[y] = self._blank_row()
                for x in range(self.cx + 1):
                    self.grid[self.cy][x] = " "
            else:
                self.grid = [self._blank_row() for _ in range(self.rows)]
        elif final == "K":
            mode = arg(0, 0)
            if mode == 0:
                for x in range(self.cx, self.cols):
                    self.grid[self.cy][x] = " "
            elif mode == 1:
                for x in range(self.cx + 1):
                    self.grid[self.cy][x] = " "
            else:
                self.grid[self.cy] = self._blank_row()
        elif final == "X":
            for x in range(self.cx, min(self.cols, self.cx + arg(0))):
                self.grid[self.cy][x] = " "
        elif final == "r":
            self.top = max(0, arg(0) - 1)
            self.bot = min(self.rows - 1, arg(1, self.rows) - 1)
            if self.top >= self.bot:
                self.top, self.bot = 0, self.rows - 1
            self.cy = self.cx = 0
        elif final == "S":
            self._scroll_up(arg(0))
        elif final == "T":
            self._scroll_down(arg(0))
        elif final == "L":
            for _ in range(arg(0)):
                if self.cy <= self.bot:
                    del self.grid[self.bot]
                    self.grid.insert(self.cy, self._blank_row())
        elif final == "M":
            for _ in range(arg(0)):
                if self.cy <= self.bot:
                    del self.grid[self.cy]
                    self.grid.insert(self.bot, self._blank_row())
        elif final == "P":
            for _ in range(arg(0)):
                del self.grid[self.cy][self.cx]
                self.grid[self.cy].append(" ")
        elif final == "@":
            for _ in range(arg(0)):
                self.grid[self.cy].insert(self.cx, " ")
                del self.grid[self.cy][-1]
        elif final == "b":
            ch = self.last
            for _ in range(arg(0)):
                self._put(ch)
        # m / h / l / t / n / c / q and friends: nothing to reconstruct.


# ---------------------------------------------------------------------------
# One editor process under a PTY, with a throwaway HOME.

class Session:
    def __init__(self, home, args=(), rows=ROWS, cols=COLS):
        env = dict(os.environ)
        env["HOME"] = home
        env["XDG_CACHE_HOME"] = os.path.join(home, ".cache")
        env["XDG_CONFIG_HOME"] = os.path.join(home, ".config")
        env["XDG_DATA_HOME"] = os.path.join(home, ".local", "share")
        env["XDG_STATE_HOME"] = os.path.join(home, ".local", "state")
        env["TERM"] = "xterm-256color"
        env.pop("XDG_RUNTIME_DIR", None)
        self.screen = Screen(rows, cols)
        self.reaped = False
        self.status = None
        pid, fd = pty.fork()
        if pid == 0:
            try:
                os.chdir(home)
                os.execve(BINARY, [BINARY] + list(args), env)
            except Exception:
                pass
            os._exit(127)
        self.pid, self.fd = pid, fd
        fcntl.ioctl(fd, termios.TIOCSWINSZ,
                    struct.pack("HHHH", rows, cols, cols * 8, rows * 16))

    # The editor writes a frame per batch; if nobody drains the PTY it blocks,
    # so every wait in this file goes through pump().
    def pump(self, secs=0.0):
        end = time.monotonic() + secs
        while True:
            r, _, _ = select.select([self.fd], [], [], 0.02)
            if r:
                try:
                    data = os.read(self.fd, 65536)
                except OSError:
                    data = b""
                if not data:
                    self.reap()
                    return
                self.screen.feed(data)
                if time.monotonic() < end:
                    continue
            if time.monotonic() >= end:
                return

    def until(self, cond, timeout, what=""):
        end = time.monotonic() + timeout
        while time.monotonic() < end:
            if cond():
                return True
            self.pump(0.05)
        return bool(cond())

    def wait_screen(self, needle, timeout=10.0):
        return self.until(lambda: self.screen.has(needle), timeout)

    def send(self, data):
        os.write(self.fd, data)

    def type(self, text, chunk=8):
        data = text.encode("utf-8")
        for i in range(0, len(data), chunk):
            self.send(data[i:i + chunk])
            self.pump(0.05)

    def reap(self):
        if self.reaped:
            return True
        try:
            pid, st = os.waitpid(self.pid, os.WNOHANG)
        except ChildProcessError:
            self.reaped = True
            return True
        if pid:
            self.reaped, self.status = True, st
            return True
        return False

    def wait_exit(self, timeout=10.0):
        end = time.monotonic() + timeout
        while time.monotonic() < end:
            if self.reap():
                return True
            self.pump(0.05)
        return self.reap()

    def kill(self, sig=signal.SIGKILL):
        try:
            os.kill(self.pid, sig)
        except ProcessLookupError:
            pass

    def close(self):
        if not self.reaped:
            self.kill()
            self.wait_exit(3.0)
        try:
            os.close(self.fd)
        except OSError:
            pass


# ---------------------------------------------------------------------------
# The journal directory

def journal_dir(home):
    return os.path.join(home, ".cache", "cmedit", "journal")


def journals(home):
    try:
        return sorted(n for n in os.listdir(journal_dir(home))
                      if n.endswith(".cmj"))
    except OSError:
        return []


def journal_body(home, name):
    """The buffer text a .cmj carries (everything past the `--` separator)."""
    with open(os.path.join(journal_dir(home), name), "rb") as f:
        raw = f.read()
    head, sep, body = raw.partition(b"\n--\n")
    if not sep:
        raise AssertionError("journal %s has no -- separator" % name)
    return body.decode("utf-8")


def journal_header(home, name, key):
    with open(os.path.join(journal_dir(home), name), "rb") as f:
        head = f.read().partition(b"\n--\n")[0].decode("utf-8", "replace")
    for line in head.split("\n")[1:]:
        k, _, v = line.partition(":")
        if k.strip() == key:
            return v.strip()
    return None


# ---------------------------------------------------------------------------
# Scenarios

class Failure(AssertionError):
    pass


def check(cond, msg):
    if not cond:
        raise Failure(msg)


def new_home():
    home = tempfile.mkdtemp(prefix="cmedit-journal-")
    os.makedirs(os.path.join(home, "work"))
    return home


def write_file(home, name, content):
    path = os.path.join(home, "work", name)
    with open(path, "wb") as f:
        f.write(content)
    return path


def wait_journals(home, want, timeout=8.0, session=None):
    """Poll the journal directory until it matches `want(names)`.

    The write-behind is debounced 2 s after the last edit batch and the sweep
    runs after a key batch, so this is deliberately a poll with a wide margin
    rather than a sleep of a guessed length.
    """
    end = time.monotonic() + timeout
    while True:
        names = journals(home)
        if want(names):
            return names
        if time.monotonic() >= end:
            return names
        if session is not None:
            session.pump(0.1)
        else:
            time.sleep(0.1)


def scenario_crash_recover():
    """1. SIGKILL, restart, Recover, Save — byte-identical content."""
    home = new_home()
    try:
        path = write_file(home, "note.txt", b"alpha\nbeta\n")
        expected = b"HELLO alpha\nbeta\n"

        s = Session(home, [path])
        try:
            check(s.wait_screen("note.txt", 10.0), "editor never showed note.txt")
            s.type("HELLO ")
            names = wait_journals(home, lambda n: len(n) == 1, 8.0, s)
            check(len(names) == 1, "no journal appeared after typing: %r" % (names,))
            check(names[0].endswith("-note.txt.cmj"),
                  "journal is not named for the file: %r" % names[0])
            # The body is the buffer's lines joined with LF, no trailing
            # separator (Cmedit.Journal.bufferJournalText).
            check(journal_body(home, names[0]) == "HELLO alpha\nbeta",
                  "journal body is not the buffer text: %r"
                  % journal_body(home, names[0]))
            check(journal_header(home, names[0], "path") == '"%s"' % path,
                  "journal path header is wrong: %r"
                  % journal_header(home, names[0], "path"))
            kept = names[0]
            with open(path, "rb") as f:
                check(f.read() == b"alpha\nbeta\n",
                      "the real file was written before any save")
            s.kill(signal.SIGKILL)          # a crash, not a graceful teardown
            check(s.wait_exit(5.0), "process survived SIGKILL")
        finally:
            s.close()

        check(journals(home) == [kept], "SIGKILL lost the journal")

        s = Session(home, [path])
        try:
            check(s.wait_screen("Unsaved Changes Recovered", 12.0),
                  "no recovery dialog on restart:\n" + s.screen.text())
            check(s.screen.has("note.txt"),
                  "recovery dialog does not name the file")
            s.send(ENTER)                   # button 0 = Recover
            check(s.wait_screen("Recovered 1 unsaved file", 8.0),
                  "Recover did not report:\n" + s.screen.text())
            with open(path, "rb") as f:
                check(f.read() == b"alpha\nbeta\n",
                      "recovery wrote to disk by itself")
            s.send(CTRL_S)
            def saved():
                with open(path, "rb") as f:
                    return f.read() == expected
            check(s.until(saved, 8.0), "Ctrl+S did not write the recovered text")
            check(wait_journals(home, lambda n: n == [], 6.0, s) == [],
                  "journal survived the save: %r" % journals(home))
            s.send(CTRL_Q)
            check(s.wait_exit(8.0), "editor did not quit")
        finally:
            s.close()

        with open(path, "rb") as f:
            got = f.read()
        check(got == expected,
              "recovered file is not byte-identical: %r vs %r" % (got, expected))
        check(journals(home) == [], "journals left behind: %r" % journals(home))
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_save_drops_journal():
    """2. An in-session save removes that document's journal."""
    home = new_home()
    try:
        path = write_file(home, "two.txt", b"one\n")
        s = Session(home, [path])
        try:
            check(s.wait_screen("two.txt", 10.0), "editor never showed two.txt")
            s.type("XY")
            names = wait_journals(home, lambda n: len(n) == 1, 8.0, s)
            check(len(names) == 1, "no journal appeared after typing: %r" % (names,))
            s.send(CTRL_S)
            left = wait_journals(home, lambda n: n == [], 6.0, s)
            check(left == [], "journal survived the save: %r" % left)
            with open(path, "rb") as f:
                got = f.read()
            check(got == b"XYone\n", "save wrote %r" % got)
            s.send(CTRL_Q)
            check(s.wait_exit(8.0), "editor did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_clean_exit():
    """3. Quitting and discarding removes journals; the next start is silent."""
    home = new_home()
    try:
        s = Session(home, [])
        try:
            check(s.wait_screen("untitled", 10.0),
                  "editor never showed an untitled buffer:\n" + s.screen.text())
            s.type("scratch text")
            names = wait_journals(home, lambda n: len(n) == 1, 8.0, s)
            check(names == ["untitled-1.cmj"],
                  "untitled buffer was not journalled: %r" % (names,))
            s.send(CTRL_Q)
            check(s.wait_screen("Save changes to untitled?", 8.0),
                  "no quit confirmation:\n" + s.screen.text())
            s.send(TAB)                     # Save -> Don't Save
            s.send(ENTER)
            check(s.wait_exit(8.0), "editor did not quit")
        finally:
            s.close()
        left = journals(home)
        check(left == [], "clean exit left journals: %r" % left)

        s = Session(home, [])
        try:
            check(s.wait_screen("untitled", 10.0), "second startup drew nothing")
            s.pump(2.0)
            check(not s.screen.has("Unsaved Changes Recovered"),
                  "recovery dialog on a clean start:\n" + s.screen.text())
            s.send(CTRL_Q)
            check(s.wait_exit(8.0), "editor did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_keep_for_later():
    """4. "Keep for later" leaves the journal on disk past a clean exit."""
    home = new_home()
    try:
        path = write_file(home, "keep.txt", b"k\n")
        s = Session(home, [path])
        try:
            check(s.wait_screen("keep.txt", 10.0), "editor never showed keep.txt")
            s.type("QQ")
            names = wait_journals(home, lambda n: len(n) == 1, 8.0, s)
            check(len(names) == 1, "no journal appeared after typing: %r" % (names,))
            kept = names[0]
            with open(os.path.join(journal_dir(home), kept), "rb") as f:
                before = f.read()
            s.kill(signal.SIGKILL)
            check(s.wait_exit(5.0), "process survived SIGKILL")
        finally:
            s.close()

        s = Session(home, [path])
        try:
            check(s.wait_screen("Unsaved Changes Recovered", 12.0),
                  "no recovery dialog on restart:\n" + s.screen.text())
            s.send(TAB)                     # Recover -> Discard
            s.send(TAB)                     # Discard -> Keep for later
            s.send(ENTER)
            check(s.wait_screen("Kept the unsaved changes", 8.0),
                  "Keep for later did not report:\n" + s.screen.text())
            # Nothing was adopted, so this session owns no journal and must
            # not delete one on the way out.
            s.send(CTRL_Q)
            check(s.wait_exit(8.0), "editor did not quit")
        finally:
            s.close()

        left = journals(home)
        check(left == [kept], "kept journal did not survive: %r" % left)
        with open(os.path.join(journal_dir(home), kept), "rb") as f:
            check(f.read() == before, "kept journal was rewritten")
        with open(path, "rb") as f:
            check(f.read() == b"k\n", "the real file was touched")
    finally:
        shutil.rmtree(home, ignore_errors=True)


SCENARIOS = [
    ("crash, restart, Recover, Save", scenario_crash_recover),
    ("save removes the journal", scenario_save_drops_journal),
    ("clean exit removes journals", scenario_clean_exit),
    ("Keep for later preserves them", scenario_keep_for_later),
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
