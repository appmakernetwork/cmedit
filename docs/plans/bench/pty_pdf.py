#!/usr/bin/env python3
"""PTY verification of the PDF page view (plan 0034, workstream F2).

Drives the real ./cmedit over a pseudo-terminal, reusing the VT emulator and
Session from pty_journal.py (and RawSession from pty_svg.py), against a small
multi-page PDF written by this script (no external tools). Checks:

  a. With no capability replies, a PDF opens in the reflow view
     (pdf-view = auto); Alt+T shows the page view as a half-block thumbnail
     and says so; PgDn turns the page; Alt+T back lands on the same page.
  b. A fake kitty terminal (kitty-graphics probe, DA1, XTWINOPS cell size)
     switches auto to the page view once the replies arrive and places the
     page as an APC `a=T` image; PgDn places the next page, measured cold
     (straight after the first placement) and warm (after the idle
     prefetch); a Ctrl+wheel notch re-places at a zoomed crop; quitting
     deletes placements.
  c. Alt+T after a manual choice is not overridden by auto.
  d. Hostile files (pdf/hostile/: nested Type 3 glyphs and nested forms that
     share their bodies, a 10 KB file expanding to astronomically many
     drawing commands) leave the editor responsive under fake kitty: Alt+T
     still switches views within a second, and Ctrl+Q quits within one.
  e. Opening a PDF runs no external program: with a fake PATH whose
     `pyright` is `exec corepack ...` and whose `ruff` records being run,
     neither is executed (linter detection is a PATH lookup only).

The assertions are about placements and status text, not page content, so
the file runs against the stub renderer as well as the real one.

    python3 docs/plans/bench/pty_pdf.py

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
from pty_svg import RawSession, picture_cells, PLACE, kitty_replies  # noqa: E402

ROWS, COLS = 40, 120
ALT_T = b"\x1bt"
CTRL_Q = b"\x11"
PGDN = b"\x1b[6~"
PGUP = b"\x1b[5~"


def make_pdf(npages):
    """A minimal valid PDF: one Helvetica line of text per page."""
    objs = []
    objs.append(b"<< /Type /Catalog /Pages 2 0 R >>")
    kids = " ".join("%d 0 R" % (4 + 2 * i) for i in range(npages))
    objs.append(("<< /Type /Pages /Kids [%s] /Count %d >>" % (kids, npages)).encode())
    objs.append(b"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
    for i in range(npages):
        page_no = 4 + 2 * i
        lines = ["BT /F1 24 Tf 72 720 Td (This is page %d of %d) Tj ET" % (i + 1, npages)]
        # enough body lines that the reflow view can scroll page by page
        for k in range(40):
            lines.append("BT /F1 11 Tf 72 %d Td (Body line %d of page %d, some words to read.) Tj ET"
                         % (690 - 15 * k, k + 1, i + 1))
        content = "\n".join(lines).encode()
        objs.append(("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] "
                     "/Resources << /Font << /F1 3 0 R >> >> /Contents %d 0 R >>"
                     % (page_no + 1)).encode())
        objs.append(b"<< /Length %d >>\nstream\n" % len(content) + content + b"\nendstream")
    out = bytearray(b"%PDF-1.4\n")
    offsets = []
    for k, body in enumerate(objs, start=1):
        offsets.append(len(out))
        out += b"%d 0 obj\n" % k + body + b"\nendobj\n"
    xref = len(out)
    out += b"xref\n0 %d\n0000000000 65535 f \n" % (len(objs) + 1)
    for off in offsets:
        out += b"%010d 00000 n \n" % off
    out += b"trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n" % (len(objs) + 1, xref)
    return bytes(out)


def check(cond, msg):
    if not cond:
        raise pj.Failure(msg)


def fixture(home, npages=5):
    p = os.path.join(home, "doc.pdf")
    with open(p, "wb") as f:
        f.write(make_pdf(npages))
    return p


def placements(s, since=0):
    return PLACE.findall(bytes(s.raw[since:]))


def scenario_portable():
    home = tempfile.mkdtemp(prefix="cmedit-pdf-")
    try:
        s = RawSession(home, [fixture(home)], rows=ROWS, cols=COLS)
        try:
            check(s.wait_screen("This is page 1 of 5", 10), "the reflow view did not show the text")
            check(s.wait_screen("Page 1 of 5", 5), "the reflow status is missing")
            s.send(ALT_T)
            check(s.until(lambda: picture_cells(s.screen) > 50, 10), "no page thumbnail in cells")
            check(s.wait_screen("Page 1/5", 5), "the page view status is missing")
            check(s.wait_screen("thumbnail", 5), "the dumb-terminal note is missing")
            check(not placements(s), "a pixel placement was sent to a silent terminal")
            s.send(PGDN)
            check(s.wait_screen("Page 2/5", 5), "PgDn did not turn the page")
            s.send(b"\x1b[F")                    # End
            check(s.wait_screen("Page 5/5", 5), "End did not go to the last page")
            s.send(b"\x1b[H")                    # Home
            check(s.wait_screen("Page 1/5", 5), "Home did not go to the first page")
            s.send(PGDN)
            check(s.wait_screen("Page 2/5", 5), "PgDn did not turn the page")
            s.send(ALT_T)
            check(s.wait_screen("Page 2 of 5", 5), "Alt+T back did not keep the page")
            # A trackpad flick: thirty wheel notches at once turn one page,
            # not thirty (the wheel is held to a turn per 120 ms).
            s.send(ALT_T)
            check(s.wait_screen("Page 2/5", 5), "Alt+T did not return to the page view")
            s.send(b"".join(b"\x1b[<65;%d;%dM" % (COLS // 2, ROWS // 2) for _ in range(30)))
            s.pump(0.5)
            m = re.search(r"Page (\d)/5", s.screen.text())
            check(m and int(m.group(1)) <= 3, "a wheel flick skipped pages: %s" % (m.group(0) if m else "?"))
            print("  30 wheel notches at once: page 2 -> %s" % (m.group(1) if m else "?"))
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_kitty():
    home = tempfile.mkdtemp(prefix="cmedit-pdf-")
    try:
        s = RawSession(home, [fixture(home)], rows=ROWS, cols=COLS)
        try:
            s.wait_screen("Page 1 of 5", 10)       # reflow first: the replies come after
            t0 = time.monotonic()
            kitty_replies(s)
            check(s.until(lambda: bool(placements(s)), 10), "auto did not switch to a page placement")
            t1 = time.monotonic()
            check(s.wait_screen("Page 1/5", 5), "the page view status is missing")
            w, h, c, r = (int(g) for g in placements(s)[-1])
            print("  auto switch to pixels: first page %dx%d px in %dx%d cells after %.0f ms"
                  % (w, h, c, r, (t1 - t0) * 1000))
            check(w <= COLS * 8 and h <= ROWS * 16, "placement larger than the text area")
            # Cold PgDn: straight away (the prefetch may still be running).
            mark = len(s.raw)
            t2 = time.monotonic()
            s.send(PGDN)
            check(s.until(lambda: bool(placements(s, mark)), 10), "PgDn did not place the next page")
            t3 = time.monotonic()
            check(s.wait_screen("Page 2/5", 5), "PgDn did not turn the page")
            print("  PgDn (cold) -> placement in %.0f ms" % ((t3 - t2) * 1000))
            # Warm PgDn: let the idle prefetch finish first.
            s.pump(1.5)
            mark = len(s.raw)
            t4 = time.monotonic()
            s.send(PGDN)
            check(s.until(lambda: bool(placements(s, mark)), 10), "the second PgDn did not place")
            t5 = time.monotonic()
            check(s.wait_screen("Page 3/5", 5), "the second PgDn did not turn the page")
            print("  PgDn (prefetched) -> placement in %.0f ms" % ((t5 - t4) * 1000))
            # Zoom with Ctrl+wheel: a new placement for the crop.
            mark = len(s.raw)
            s.send(b"\x1b[<80;%d;%dM" % (COLS // 2, ROWS // 2))
            check(s.until(lambda: bool(placements(s, mark)), 10), "Ctrl+wheel did not re-place")
            check(s.wait_screen("Page 3/5", 5) and "100%" not in s.screen.text(),
                  "the status does not show the zoom")
            # 0 back to the fit.
            mark = len(s.raw)
            s.send(b"0")
            check(s.wait_screen("100%", 5), "0 did not reset the zoom")
            mark = len(s.raw)
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
            check(b"\x1b_Ga=d,d=A" in bytes(s.raw[mark:]), "quitting did not delete placements")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_manual_sticks():
    """A reflow chosen with Alt+T before the replies arrive is kept."""
    home = tempfile.mkdtemp(prefix="cmedit-pdf-")
    try:
        s = RawSession(home, [fixture(home)], rows=ROWS, cols=COLS)
        try:
            s.wait_screen("Page 1 of 5", 10)
            s.send(ALT_T)                          # to the page view (cells)
            s.wait_screen("Page 1/5", 5)
            s.send(ALT_T)                          # and back: a manual choice of reflow
            check(s.wait_screen("Page 1 of 5", 5), "did not return to reflow")
            kitty_replies(s)
            s.pump(1.5)
            check(not placements(s), "auto overrode a manual choice of reflow")
            check("Page 1 of 5" in s.screen.text(), "the reflow view did not stay")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def make_blank_pdf():
    """One page, nothing on it but the paper."""
    objs = [b"<< /Type /Catalog /Pages 2 0 R >>",
            b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
            b"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents 4 0 R >>",
            b"<< /Length 0 >>\nstream\n\nendstream"]
    out = bytearray(b"%PDF-1.4\n")
    for k, body in enumerate(objs, start=1):
        out += b"%d 0 obj\n" % k + body + b"\nendobj\n"
    out += b"trailer\n<< /Size 5 /Root 1 0 R >>\n%%EOF\n"
    return bytes(out)


def first_payload(raw):
    """The RGBA pixels of the first kitty placement (its base64 chunks)."""
    import base64
    m = re.search(rb"\x1b_Ga=T,f=32,i=1,q=2,s=(\d+),v=(\d+)[^;]*;", raw)
    if not m:
        return None, 0, 0
    w, h = int(m.group(1)), int(m.group(2))
    data = bytearray()
    i = m.end()
    while True:
        e = raw.find(b"\x1b\\", i)
        data += raw[i:e]
        nxt = re.match(rb"\x1b_Gm=\d+;", raw[e + 2:e + 20])
        if not nxt:
            break
        i = e + 2 + nxt.end()
    return base64.b64decode(bytes(data)), w, h


def dark_pixels(px):
    n = 0
    for k in range(0, len(px) - 3, 4):
        if px[k + 3] > 128 and px[k] + px[k + 1] + px[k + 2] < 3 * 100:
            n += 1
    return n


def scenario_fonts():
    """Text in a non-embedded standard-14 font (Helvetica) is drawn with a
    system substitute: the text page has dark pixels, the blank page none."""
    counts = {}
    for name, blob in [("text", make_pdf(1)), ("blank", make_blank_pdf())]:
        home = tempfile.mkdtemp(prefix="cmedit-pdf-")
        try:
            p = os.path.join(home, name + ".pdf")
            with open(p, "wb") as f:
                f.write(blob)
            s = RawSession(home, [p], rows=ROWS, cols=COLS)
            try:
                kitty_replies(s)
                check(s.until(lambda: bool(placements(s)), 15), "no placement for the %s page" % name)
                s.pump(1.0)
                px, w, h = first_payload(bytes(s.raw))
                check(px is not None and len(px) == 4 * w * h, "could not read the %s payload" % name)
                counts[name] = dark_pixels(px)
                s.send(CTRL_Q)
                s.wait_exit(5)
            finally:
                s.close()
        finally:
            shutil.rmtree(home, ignore_errors=True)
    print("  dark pixels: text page %d, blank page %d" % (counts["text"], counts["blank"]))
    check(counts["blank"] == 0, "the blank page is not blank")
    check(counts["text"] > 500, "no text drawn: are system fonts being found?")


HOSTILE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "pdf", "hostile")


def scenario_hostile():
    for name in ["t3bomb.pdf", "formbomb10.pdf"]:
        home = tempfile.mkdtemp(prefix="cmedit-pdf-")
        try:
            p = os.path.join(home, name)
            shutil.copy(os.path.join(HOSTILE, name), p)
            s = RawSession(home, [p], rows=ROWS, cols=COLS)
            try:
                check(s.wait_screen("Page 1 of", 10), "%s: the reflow view did not open" % name)
                kitty_replies(s)
                check(s.wait_screen("Page 1/", 10), "%s: auto did not switch to the page view" % name)
                s.pump(1.0)                        # the page's jobs are running now
                t0 = time.monotonic()
                s.send(ALT_T)
                check(s.wait_screen("Page 1 of", 1.0), "%s: Alt+T did not reach reflow within 1 s" % name)
                t1 = time.monotonic()
                s.send(ALT_T)
                check(s.wait_screen("Page 1/", 1.0), "%s: Alt+T did not return to the page within 1 s" % name)
                s.pump(0.5)
                t2 = time.monotonic()
                s.send(CTRL_Q)
                check(s.wait_exit(1.0), "%s: Ctrl+Q did not quit within 1 s" % name)
                t3 = time.monotonic()
                print("  %s: Alt+T %.0f ms, Ctrl+Q -> exit %.0f ms" % (name, (t1 - t0) * 1000, (t3 - t2) * 1000))
            finally:
                if not s.reap():
                    s.kill()
                s.close()
        finally:
            shutil.rmtree(home, ignore_errors=True)


def scenario_no_exec():
    home = tempfile.mkdtemp(prefix="cmedit-pdf-")
    old_path = os.environ.get("PATH", "")
    try:
        fake = os.path.join(home, "fakebin")
        ran = os.path.join(home, "ran")
        os.makedirs(fake)
        os.makedirs(ran)
        for name, body in [("corepack", ": > '%s/corepack'\n" % ran),
                           ("pyright", 'exec corepack yarn pyright "$@"\n'),
                           ("ruff", ": > '%s/ruff'\necho 'ruff 0.0.0'\n" % ran),
                           ("eslint", ": > '%s/eslint'\n" % ran)]:
            f = os.path.join(fake, name)
            with open(f, "w") as h:
                h.write("#!/bin/sh\n" + body)
            os.chmod(f, 0o755)
        os.environ["PATH"] = fake + os.pathsep + old_path
        s = RawSession(home, [fixture(home)], rows=ROWS, cols=COLS)
        try:
            check(s.wait_screen("Page 1 of 5", 10), "the PDF did not open")
            s.pump(1.5)                            # detection has long finished
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
        check(not os.listdir(ran), "detection ran: %s" % sorted(os.listdir(ran)))
        check(not os.path.exists(os.path.join(home, ".cache", "node")), "something populated ~/.cache/node")
    finally:
        os.environ["PATH"] = old_path
        shutil.rmtree(home, ignore_errors=True)


def main():
    if not os.path.exists(pj.BINARY):
        print("build ./cmedit first (make)")
        return 1
    failed = 0
    for name, fn in [("portable: reflow by default, page thumbnail on Alt+T", scenario_portable),
                     ("kitty: auto page view, PgDn and zoom re-place, cleanup", scenario_kitty),
                     ("a manual mode is not overridden by auto", scenario_manual_sticks),
                     ("non-embedded Helvetica is drawn with a system font", scenario_fonts),
                     ("hostile shared-body PDFs: responsive, and Ctrl+Q quits in 1 s", scenario_hostile),
                     ("opening a PDF executes no external program", scenario_no_exec)]:
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
