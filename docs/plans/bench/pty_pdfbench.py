#!/usr/bin/env python3
"""Measure the PDF page view on real files, end to end, over a PTY.

For each PDF it drives ./cmedit as a fake kitty (or sixel) terminal and
reports, per file and with the file named only as doc1, doc2, ... (the
corpus may be private; names and contents are never printed):

  pages, size       from the status line ("Page 1/N") and the file
  first             open -> first pixel placement
  cold              PgDn straight after the first placement
  warm              PgDn after the idle prefetch has had time to run
  zoom              a Ctrl+wheel notch -> re-placement
  rss               peak RSS (VmHWM) after paging through up to 20 pages
  idle              CPU use over 3 s once everything has settled (must be ~0)
  maxpay            the largest single graphics payload, in bytes

    python3 docs/plans/bench/pty_pdfbench.py [--sixel|--nocaps] [--max N] LIST_OR_PDFS...

LIST_OR_PDFS are PDF paths, or text files with one path per line.
"""

import os
import re
import shutil
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import pty_journal as pj  # noqa: E402
from pty_svg import RawSession  # noqa: E402

ROWS, COLS = 40, 120
PGDN = b"\x1b[6~"
CTRL_Q = b"\x11"
KITTY_PLACE = re.compile(rb"\x1b_Ga=T,")
SIXEL = re.compile(rb"\x1bP[0-9;]*q")
STATUS = re.compile(r"Page (\d+)/(\d+)")


def replies(s, mode):
    s.pump(0.3)
    if mode == "kitty":
        s.send(b"\x1b_Gi=31;OK\x1b\\" + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
    elif mode == "sixel":
        s.send(b"\x1b[6;16;8t" + b"\x1b[?62;4c")


def placed_since(s, mark, mode):
    pat = KITTY_PLACE if mode == "kitty" else SIXEL
    return pat.search(bytes(s.raw[mark:])) is not None


def max_payload(raw):
    best = 0
    i = 0
    while True:
        m = min([x for x in (raw.find(b"\x1b_G", i), raw.find(b"\x1bP", i)) if x >= 0], default=-1)
        if m < 0:
            return best
        e = raw.find(b"\x1b\\", m)
        if e < 0:
            return best
        # a kitty placement is many chunks: measure up to the next non-chunk
        if raw.startswith(b"\x1b_Ga=T", m):
            j = e + 2
            while raw.startswith(b"\x1b_Gm=", j):
                e2 = raw.find(b"\x1b\\", j)
                if e2 < 0:
                    break
                j = e2 + 2
            best = max(best, j - m)
            i = j
        else:
            best = max(best, e + 2 - m)
            i = e + 2


def cpu_ticks(pid):
    with open("/proc/%d/stat" % pid) as f:
        parts = f.read().rsplit(")", 1)[1].split()
    return int(parts[11]) + int(parts[12])


def hwm_mb(pid):
    with open("/proc/%d/status" % pid) as f:
        for line in f:
            if line.startswith("VmHWM"):
                return int(line.split()[1]) / 1024.0
    return 0.0


def timed(s, mark, mode, timeout):
    t0 = time.monotonic()
    ok = s.until(lambda: placed_since(s, mark, mode), timeout)
    return (time.monotonic() - t0) * 1000 if ok else None


def page_no(s):
    m = STATUS.search(s.screen.text())
    return (int(m.group(1)), int(m.group(2))) if m else (None, None)


def bench(path, mode):
    home = tempfile.mkdtemp(prefix="cmedit-pdfb-")
    r = {"size": os.path.getsize(path) / 1e6}
    try:
        s = RawSession(home, [os.path.abspath(path)], rows=ROWS, cols=COLS)
        try:
            t0 = time.monotonic()
            replies(s, mode)
            if mode == "nocaps":
                s.until(lambda: STATUS.search(s.screen.text()) or "Page 1 of" in s.screen.text(), 30)
                r["first"] = (time.monotonic() - t0) * 1000
            else:
                ok = s.until(lambda: placed_since(s, 0, mode), 60)
                r["first"] = (time.monotonic() - t0) * 1000 if ok else None
            _, r["pages"] = page_no(s)
            if mode != "nocaps" and r["pages"] and r["pages"] > 1:
                mark = len(s.raw)
                s.send(PGDN)
                r["cold"] = timed(s, mark, mode, 60)
                s.pump(3.0)                         # let the prefetch run
                mark = len(s.raw)
                s.send(PGDN)
                r["warm"] = timed(s, mark, mode, 60)
                mark = len(s.raw)
                s.send(b"\x1b[<80;%d;%dM" % (COLS // 2, ROWS // 2))
                r["zoom"] = timed(s, mark, mode, 60)
                s.send(b"0")
                s.pump(0.5)
                for _ in range(17):
                    p, n = page_no(s)
                    if p is None or n is None or p >= n:
                        break
                    mark = len(s.raw)
                    s.send(PGDN)
                    s.until(lambda: placed_since(s, mark, mode) or (page_no(s)[0] or 0) > p, 30)
            s.pump(2.0)                             # settle
            c0 = cpu_ticks(s.pid)
            s.pump(3.0)
            c1 = cpu_ticks(s.pid)
            r["idle"] = (c1 - c0) / 3.0             # ticks/s ~= percent of a core
            r["rss"] = hwm_mb(s.pid)
            r["maxpay"] = max_payload(bytes(s.raw))
            s.send(CTRL_Q)
            s.wait_exit(10)
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)
    return r


def fmt(v, unit=""):
    if v is None:
        return "-"
    if isinstance(v, float):
        return "%.0f%s" % (v, unit)
    return "%s%s" % (v, unit)


def main():
    args = sys.argv[1:]
    mode = "kitty"
    limit = None
    files = []
    while args:
        a = args.pop(0)
        if a == "--sixel":
            mode = "sixel"
        elif a == "--nocaps":
            mode = "nocaps"
        elif a == "--max":
            limit = int(args.pop(0))
        elif a.lower().endswith(".pdf"):
            files.append(a)
        else:
            with open(a) as f:
                files += [l.strip() for l in f if l.strip()]
    if limit:
        files = files[:limit]
    print("mode %s, %dx%d terminal" % (mode, COLS, ROWS))
    print("%-6s %5s %7s %7s %6s %6s %6s %7s %5s %9s" %
          ("doc", "pages", "MB", "first", "cold", "warm", "zoom", "rssMB", "idle%", "maxpay"))
    for k, p in enumerate(files, 1):
        try:
            r = bench(p, mode)
        except Exception as e:  # keep going; report the failure anonymously
            print("doc%-3d error %s" % (k, type(e).__name__))
            continue
        print("%-6s %5s %7.2f %7s %6s %6s %6s %7s %5s %9s" %
              ("doc%d" % k, fmt(r.get("pages")), r["size"], fmt(r.get("first")), fmt(r.get("cold")),
               fmt(r.get("warm")), fmt(r.get("zoom")), fmt(r.get("rss")), fmt(r.get("idle")),
               fmt(r.get("maxpay"))))
        sys.stdout.flush()


if __name__ == "__main__":
    main()
