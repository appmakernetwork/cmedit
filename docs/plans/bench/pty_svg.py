#!/usr/bin/env python3
"""PTY verification of the SVG picture view (plan 0034, workstream F).

Drives the real ./cmedit binary over a pseudo-terminal, reusing the VT
emulator and Session from pty_journal.py, and checks:

  a. With no capability replies (the portable floor), an .svg opens as a
     half-block picture, Alt+T shows its source, Alt+T again the picture.
  b. A fake kitty terminal (answers the kitty-graphics probe, DA1 and the
     XTWINOPS cell size) gets an APC placement (a=T with RGBA data); a wheel
     notch over the picture produces a new placement for the new crop, at a
     size no larger than the text area's pixels; quitting deletes placements.
  c. A large SVG (tens of thousands of paths) opens without blocking input:
     the editor keeps answering keys while the picture renders, and the
     picture arrives.

Every scenario runs against a throwaway HOME. Timings printed are wall clock
on this machine, for the plan's log.

    python3 docs/plans/bench/pty_svg.py

Exit status is the number of failed scenarios.
"""

import os
import re
import shutil
import sys
import tempfile
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import pty_journal as pj  # noqa: E402

ROWS, COLS = 40, 120
ESC = b"\x1b"
ALT_T = b"\x1bt"
CTRL_Q = b"\x11"

HALF_BLOCKS = ("▀", "▄")


class RawSession(pj.Session):
    """A Session that also keeps every byte the editor wrote."""

    def __init__(self, *a, **kw):
        self.raw = bytearray()
        super().__init__(*a, **kw)
        orig = self.screen.feed

        self._pend = b""

        # The VT emulator walks string sequences a byte at a time in Python,
        # which for a megabyte of base64 pixels costs the harness a second
        # per placement and makes every latency it reports a measure of
        # itself. Graphics payloads (APC for kitty, DCS for sixel) carry no
        # screen text, so they are cut out here with bytes.find, and only
        # the rest is replayed.
        def feed(data, orig=orig):
            self.raw.extend(data)
            buf = self._pend + data
            out = bytearray()
            i = 0
            while True:
                cands = [x for x in (buf.find(b"\x1b_", i), buf.find(b"\x1bP", i)) if x >= 0]
                if not cands:
                    if buf.endswith(b"\x1b"):
                        out += buf[i:-1]
                        self._pend = b"\x1b"
                    else:
                        out += buf[i:]
                        self._pend = b""
                    break
                m = min(cands)
                out += buf[i:m]
                end = buf.find(b"\x1b\\", m + 2)
                if end < 0:
                    self._pend = buf[m:]
                    break
                i = end + 2
            if out:
                orig(bytes(out))
        self.screen.feed = feed


def picture_cells(screen):
    return sum(row.count(ch) for row in screen.grid for ch in HALF_BLOCKS)


def write(home, name, text):
    p = os.path.join(home, name)
    with open(p, "w", encoding="utf-8") as f:
        f.write(text)
    return p


ICON = """<svg xmlns="http://www.w3.org/2000/svg" width="48" height="48" viewBox="0 0 48 48">
  <rect x="2" y="2" width="44" height="44" rx="6" fill="#3465a4"/>
  <circle cx="24" cy="24" r="14" fill="#fce94f" stroke="#c4a000" stroke-width="2"/>
  <path d="M14 30 L24 12 L34 30 Z" fill="#cc0000"/>
</svg>
"""


def big_svg(n):
    parts = ['<svg xmlns="http://www.w3.org/2000/svg" width="1000" height="1000">']
    for i in range(n):
        x = (i * 37) % 1000
        y = (i * 91) % 1000
        parts.append('<path d="M%d %d c 20 -30 40 30 60 0 s 40 30 60 0" fill="none" '
                     'stroke="#%06x" stroke-width="3"/>' % (x, y, (i * 2654435761) & 0xFFFFFF))
    parts.append("</svg>\n")
    return "\n".join(parts)


def check(cond, msg):
    if not cond:
        raise pj.Failure(msg)


def scenario_portable():
    home = tempfile.mkdtemp(prefix="cmedit-svg-")
    try:
        path = write(home, "icon.svg", ICON)
        s = RawSession(home, [path], rows=ROWS, cols=COLS)
        try:
            t0 = time.monotonic()
            ok = s.until(lambda: picture_cells(s.screen) > 50, 10)
            t1 = time.monotonic()
            check(ok, "no half-block picture appeared")
            print("  picture after %.0f ms (open + parse + cells)" % ((t1 - t0) * 1000))
            check(s.wait_screen("SVG 48×48", 5), "status bar does not name the SVG")
            check(b"\x1b_Ga=T" not in s.raw, "a pixel placement was sent to a silent terminal")
            s.send(ALT_T)
            check(s.wait_screen("<circle", 5), "Alt+T did not show the source")
            check(picture_cells(s.screen) == 0, "picture still on screen in the source view")
            s.send(ALT_T)
            check(s.until(lambda: picture_cells(s.screen) > 50, 5), "Alt+T did not bring the picture back")
            # zoom with the wheel over the centre: the cells change
            before = s.screen.text()
            s.send(b"\x1b[<64;%d;%dM" % (COLS // 2, ROWS // 2))
            check(s.until(lambda: s.screen.text() != before and "Zoom" in s.screen.text(), 5),
                  "wheel zoom did not change the picture")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


PLACE = re.compile(rb"\x1b_Ga=T,f=32,i=1,q=2,s=(\d+),v=(\d+),c=(\d+),r=(\d+)")


def scenario_kitty():
    home = tempfile.mkdtemp(prefix="cmedit-svg-")
    try:
        path = write(home, "icon.svg", ICON)
        s = RawSession(home, [path], rows=ROWS, cols=COLS)
        try:
            # Answer the probes as kitty would: graphics OK, DA1, cell size.
            s.pump(0.3)
            s.send(b"\x1b_Gi=31;OK\x1b\\" + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
            t0 = time.monotonic()
            check(s.until(lambda: PLACE.search(bytes(s.raw)) is not None, 10),
                  "no kitty placement appeared")
            t1 = time.monotonic()
            m = PLACE.search(bytes(s.raw))
            w, h, c, r = (int(g) for g in m.groups())
            print("  first placement %dx%d px in %dx%d cells after %.0f ms"
                  % (w, h, c, r, (t1 - t0) * 1000))
            check(w * h * 4 > 0, "empty placement")
            check(w <= COLS * 8 and h <= ROWS * 16, "placement larger than the text area")
            check(picture_cells(s.screen) == 0, "half-block cells drawn under the placement")
            n0 = len(PLACE.findall(bytes(s.raw)))
            mark = len(s.raw)
            t2 = time.monotonic()
            s.send(b"\x1b[<64;%d;%dM" % (COLS // 2, ROWS // 2))
            check(s.until(lambda: PLACE.search(bytes(s.raw[mark:])) is not None, 10),
                  "wheel zoom did not re-place the picture")
            t3 = time.monotonic()
            n1 = len(PLACE.findall(bytes(s.raw)))
            print("  re-placement after a wheel notch in %.0f ms (%d placements)" % ((t3 - t2) * 1000, n1))
            check(n1 > n0, "no new placement")
            # Several notches: each settles on a new placement (sharp re-render)
            for _ in range(4):
                mark = len(s.raw)
                s.send(b"\x1b[<64;%d;%dM" % (COLS // 2, ROWS // 2))
                check(s.until(lambda: PLACE.search(bytes(s.raw[mark:])) is not None, 10),
                      "a later wheel notch did not re-place")
            # Alt+T to source: the placement is deleted
            mark = len(s.raw)
            s.send(ALT_T)
            check(s.until(lambda: b"\x1b_Ga=d,d=A" in bytes(s.raw[mark:]), 5),
                  "leaving the picture did not delete the placement")
            s.send(ALT_T)
            mark = len(s.raw)
            check(s.until(lambda: PLACE.search(bytes(s.raw[mark:])) is not None, 10),
                  "returning to the picture did not place it again")
            mark = len(s.raw)
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
            check(b"\x1b_Ga=d,d=A" in bytes(s.raw[mark:]), "quitting did not delete placements")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_large():
    home = tempfile.mkdtemp(prefix="cmedit-svg-")
    try:
        text = big_svg(40000)
        path = write(home, "big.svg", text)
        print("  big.svg: %.1f MB, 40000 paths" % (len(text) / 1e6))
        s = RawSession(home, [path], rows=ROWS, cols=COLS)
        try:
            t0 = time.monotonic()
            # While it parses/renders, the UI must keep answering: open the
            # File menu and close it again.
            s.pump(0.2)
            s.send(b"\x1bf")
            ok_menu = s.wait_screen("Save", 3)
            tm = time.monotonic()
            s.send(ESC)
            check(ok_menu, "the UI did not answer while the SVG was rendering")
            print("  menu answered after %.0f ms" % ((tm - t0) * 1000))
            check(s.until(lambda: picture_cells(s.screen) > 50, 60), "the large picture never appeared")
            t1 = time.monotonic()
            print("  large picture after %.0f ms" % ((t1 - t0) * 1000))
            mark = s.screen.text()
            t2 = time.monotonic()
            s.send(b"\x1b[<64;%d;%dM" % (COLS // 2, ROWS // 2))
            check(s.until(lambda: s.screen.text() != mark and "Zoom" in s.screen.text(), 30),
                  "zooming the large picture did not re-render")
            # wait for the screen to settle (the background raster lands)
            last = s.screen.text()
            end = time.monotonic() + 10
            while time.monotonic() < end:
                s.pump(0.3)
                if s.screen.text() == last:
                    break
                last = s.screen.text()
            t3 = time.monotonic()
            print("  large zoom step settled after %.0f ms" % ((t3 - t2) * 1000))
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


TEXT_SVG = """<svg xmlns="http://www.w3.org/2000/svg" width="200" height="60">
  <rect width="200" height="60" fill="white"/>
  <text x="10" y="45" font-family="DejaVu Sans, sans-serif" font-size="40" font-weight="bold">Hello</text>
</svg>
"""


def scenario_fonts():
    home = tempfile.mkdtemp(prefix="cmedit-svg-")
    try:
        path = write(home, "text.svg", TEXT_SVG)
        s = RawSession(home, [path], rows=ROWS, cols=COLS)
        try:
            check(s.until(lambda: picture_cells(s.screen) > 50, 10), "no picture appeared")
            check(s.wait_screen("SVG 200", 10), "the status does not name the SVG")
            s.pump(0.5)
            txt = s.screen.text()
            if os.path.isdir("/usr/share/fonts"):
                check("no font" not in txt, "system fonts were not found for the text")
            else:
                check("no font" in txt, "no fonts installed, but no note either")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_svgz():
    import gzip
    home = tempfile.mkdtemp(prefix="cmedit-svg-")
    try:
        path = os.path.join(home, "icon.svgz")
        with gzip.open(path, "wb") as f:
            f.write(ICON.encode("utf-8"))
        s = RawSession(home, [path], rows=ROWS, cols=COLS)
        try:
            check(s.until(lambda: picture_cells(s.screen) > 50, 10), "no picture for an .svgz")
            s.send(ALT_T)
            check(s.wait_screen("<circle", 5), "Alt+T did not show the decompressed source")
            s.send(b"x")
            s.send(b"\x13")                       # Ctrl+S: refused, read-only
            check(s.wait_screen("read-only", 5), "saving an .svgz was not refused")
            s.send(CTRL_Q)
            s.pump(0.3)
        finally:
            s.close()
        with open(path, "rb") as f:
            check(f.read(2) == b"\x1f\x8b", "the .svgz on disk was overwritten")
    finally:
        shutil.rmtree(home, ignore_errors=True)


def kitty_replies(s):
    s.pump(0.3)
    s.send(b"\x1b_Gi=31;OK\x1b\\" + b"\x1b[6;16;8t" + b"\x1b[?62;22c")


def scenario_kitty_heavy():
    """Review items 1 and 7: a heavy picture on a pixel terminal shows its
    cells while the overlay renders, and zooming it never blocks the loop."""
    home = tempfile.mkdtemp(prefix="cmedit-svg-")
    try:
        path = write(home, "big.svg", big_svg(40000))
        s = RawSession(home, [path], rows=ROWS, cols=COLS)
        try:
            kitty_replies(s)
            check(s.until(lambda: picture_cells(s.screen) > 50, 60),
                  "no cell picture while the overlay was rendering")
            had_cells_first = PLACE.search(bytes(s.raw)) is None
            check(s.until(lambda: PLACE.search(bytes(s.raw)) is not None, 60), "no placement appeared")
            print("  cells shown before the placement: %s" % had_cells_first)
            # A burst of zoom notches, each superseding the previous render,
            # then the File menu: it must answer at once.
            for _ in range(6):
                s.send(b"\x1b[<64;%d;%dM" % (COLS // 2, ROWS // 2))
                s.pump(0.02)
            t0 = time.monotonic()
            s.send(b"\x1bf")
            ok = s.wait_screen("Save", 5)
            t1 = time.monotonic()
            check(ok, "the menu did not open after a zoom burst")
            print("  menu after a 6-notch zoom burst on a heavy picture: %.0f ms" % ((t1 - t0) * 1000))
            check(t1 - t0 < 1.0, "the event loop was blocked by superseded renders")
            s.send(ESC)
            s.pump(0.3)                      # a lone ESC, not Alt+Ctrl+Q
            t2 = time.monotonic()
            s.send(CTRL_Q)
            check(s.wait_exit(10), "did not quit")
            print("  quit with renders in flight: %.0f ms" % ((time.monotonic() - t2) * 1000))
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_resize():
    """Review item 8: after a resize the old placement is removed, and the
    new one has the new geometry."""
    import fcntl, struct, termios, signal
    home = tempfile.mkdtemp(prefix="cmedit-svg-")
    try:
        path = write(home, "icon.svg", ICON)
        s = RawSession(home, [path], rows=ROWS, cols=COLS)
        try:
            kitty_replies(s)
            check(s.until(lambda: PLACE.search(bytes(s.raw)) is not None, 10), "no placement")
            first = PLACE.findall(bytes(s.raw))[-1]
            mark = len(s.raw)
            r2, c2 = 30, 90
            fcntl.ioctl(s.fd, termios.TIOCSWINSZ, struct.pack("HHHH", r2, c2, c2 * 8, r2 * 16))
            os.kill(s.pid, signal.SIGWINCH)
            check(s.until(lambda: PLACE.search(bytes(s.raw[mark:])) is not None, 10),
                  "no placement after the resize")
            tail = bytes(s.raw[mark:])
            second = PLACE.findall(tail)[-1]
            check(second != first, "the placement did not change with the geometry")
            check(tail.find(b"\x1b_Ga=d,d=A") < tail.find(b"\x1b_Ga=T"),
                  "the old placement was not deleted first")
            print("  placement %s cells -> %s cells after resize"
                  % ("x".join(x.decode() for x in first[2:]), "x".join(x.decode() for x in second[2:])))
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def main():
    if not os.path.exists(pj.BINARY):
        print("build ./cmedit first (make)")
        return 1
    failed = 0
    for name, fn in [("portable half-block picture + Alt+T", scenario_portable),
                     ("kitty placement, zoom re-placement, cleanup", scenario_kitty),
                     ("large SVG stays responsive", scenario_large),
                     ("text drawn with system fonts", scenario_fonts),
                     (".svgz opens read-only as a picture", scenario_svgz),
                     ("heavy picture on kitty: cells first, zoom never blocks", scenario_kitty_heavy),
                     ("resize re-places at the new geometry", scenario_resize)]:
        print(name)
        try:
            fn()
            print("  PASS")
        except pj.Failure as e:
            failed += 1
            print("  FAIL: %s" % e)
    print("%d failed" % failed)
    return failed


if __name__ == "__main__":
    sys.exit(main())
