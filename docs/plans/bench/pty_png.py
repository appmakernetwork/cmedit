#!/usr/bin/env python3
"""Open pictures in the real editor over a PTY and report, per file and
terminal mode, the time from spawn to the first picture, the peak resident
set (VmHWM), the resident set once idle, and the RTS's allocated and
peak-live figures from --stats-on-exit.

    python3 -I docs/plans/bench/pty_png.py BIN RUNS FILE...

Each file is opened RUNS times in each mode, a fresh process and HOME every
time. Output, one line per file and mode (time is the median over the runs,
VmHWM and peak-live the maximum, idle RSS the median):

    <file> <kitty|dumb> first N ms VmHWM N MB idle RSS N MB alloc N MB peak-live N MB

  kitty  the terminal answers the editor's capability probes as kitty does
         (graphics OK, 8x16 cell pixels, DA1); "first" is the first kitty
         placement's header appearing in the output.
  dumb   no replies at all (the portable floor); "first" is the first
         half-block picture cell on the emulated screen.

Environment:
  MODES=kitty,dumb  which modes to run (default both)
  THP_OFF=1         disable transparent huge pages for the editor processes
                    (PR_SET_THP_DISABLE, inherited across fork and exec). With
                    THP "always" the kernel may back the heap with 2 MB pages
                    depending on machine-wide state, which makes VmHWM jump in
                    2 MB steps from run to run; with it off, VmHWM moves in
                    4 KB pages and runs are reproducible.

Run timings under `flock /tmp/pngbench.lock` when other benchmarks share the
machine.
"""
import os
import re
import shutil
import statistics
import sys
import tempfile
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import pty_journal as pj  # noqa: E402
import pty_svg as ps  # noqa: E402

if os.environ.get("THP_OFF"):
    # PR_SET_THP_DISABLE (41): no transparent huge pages for this process and
    # its children (kept across exec), so VmHWM moves in 4 KB pages only.
    import ctypes
    assert ctypes.CDLL(None, use_errno=True).prctl(41, 1, 0, 0, 0) == 0

ROWS, COLS = 40, 120
KITTY_REPLY = b"\x1b_Gi=31;OK\x1b\\" + b"\x1b[6;16;8t" + b"\x1b[?62;22c"
PLACE = re.compile(rb"\x1b_Ga=T,f=32,i=1,q=2,s=(\d+),v=(\d+)")
STATS = re.compile(rb"live (\d+) MB, peak (\d+) MB, allocated (\d+) MB")


def vm(pid, key):
    with open("/proc/%d/status" % pid) as f:
        for line in f:
            if line.startswith(key + ":"):
                return int(line.split()[1]) // 1024
    return -1


def until(s, cond, timeout):
    """Session.until checks its condition only every 50 ms (it pumps 0.05 s
    between checks), which quantised every first-picture time to the probe
    time plus a multiple of ~50 ms. Check every few milliseconds instead."""
    end = time.monotonic() + timeout
    while time.monotonic() < end:
        if cond():
            return True
        s.pump(0.003)
    return bool(cond())


def wait_quiet(s, secs=0.6, timeout=30):
    """Pump until the editor has written nothing for @secs@."""
    end = time.monotonic() + timeout
    n = len(s.raw)
    q = time.monotonic()
    while time.monotonic() < end:
        s.pump(0.05)
        if len(s.raw) != n:
            n = len(s.raw)
            q = time.monotonic()
        elif time.monotonic() - q >= secs:
            return True
    return False


def one(binary, path, mode):
    pj.BINARY = binary
    home = tempfile.mkdtemp(prefix="opus-png-")
    try:
        s = ps.RawSession(home, ["--stats-on-exit", path], rows=ROWS, cols=COLS)
        t0 = time.monotonic()
        try:
            if mode == "kitty":
                assert until(s, lambda: b"\x1b_Gi=31" in s.raw, 20)
                s.send(KITTY_REPLY)
                ok = until(s, lambda: PLACE.search(s.raw) is not None, 60)
            else:
                ok = until(s, lambda: ps.picture_cells(s.screen) > 0, 60)
            t1 = time.monotonic()
            if not ok:
                return None
            # idle: the RTS collects after 2 s idle (-I2)
            wait_quiet(s, 0.5)
            s.pump(3.5)
            rss, hwm = vm(s.pid, "VmRSS"), vm(s.pid, "VmHWM")
            s.send(b"\x11")
            s.wait_exit(10)
            s.pump(0.3)
            m = STATS.search(bytes(s.raw))
            alloc = int(m.group(3)) if m else -1
            plive = int(m.group(2)) if m else -1
            return ((t1 - t0) * 1000, hwm, rss, alloc, plive)
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def main():
    binary, runs, files = sys.argv[1], int(sys.argv[2]), sys.argv[3:]
    modes = os.environ.get("MODES", "kitty,dumb").split(",")
    for f in files:
        for mode in modes:
            rs = [r for r in (one(binary, f, mode) for _ in range(runs)) if r]
            if not rs:
                print("%-16s %-5s FAILED" % (os.path.basename(f), mode), flush=True)
                continue
            print("%-16s %-5s first %7.0f ms  VmHWM %4d MB  idle RSS %4d MB  alloc %5d MB  peak-live %4d MB"
                  % (os.path.basename(f), mode, statistics.median(r[0] for r in rs),
                     max(r[1] for r in rs), statistics.median(r[2] for r in rs),
                     rs[0][3], max(r[4] for r in rs)), flush=True)


if __name__ == "__main__":
    main()
