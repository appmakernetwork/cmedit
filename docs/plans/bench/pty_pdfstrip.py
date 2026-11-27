#!/usr/bin/env python3
"""PTY verification of the PDF page view's fit-width layout, the select
tool, and drag boxes drawn over pixels (no fall-back to cells mid-drag).

Drives the real ./cmedit over a pseudo-terminal (pty_journal.Session /
pty_svg.RawSession), against the multi-page PDF pty_pdf.py writes. Checks:

  a. kitty: `pdf-fit = width` shows the pages as bands uploaded once
     (`a=t`, ids >= 100000) and placed by source rectangle (`a=p,...,x=,y=`);
     a wheel scroll only moves placements (no band re-sent; bytes reported);
     PgDn scrolls into page 2; no half-block cells under the pixels.
  b. kitty: S, then a drag over the title line: the rubber band and the
     selected line are kitty placements of the highlight swatches, the
     pixels stay up mid-drag, and Ctrl+C reports what it copied (and the
     OSC 52 fallback, when no clipboard helper exists, carries the text).
  c. kitty: W back to fit page frees the bands (`d=I`) and places the page;
     a zoom drag there keeps the pixels up (no cells) and draws its band,
     and the release zooms (a new placement).
  d. sixel: the fit-width view is sent as one sixel composite; scrolling
     sends another.
  e. no replies: the fit-width view is half-block cells; scrolling moves
     them; Ctrl+A then Ctrl+C copies the page.
  f. kitty: an image's zoom drag keeps its pixels up and draws a band.

    python3 docs/plans/bench/pty_pdfstrip.py

Exit status is the number of failed scenarios.
"""

import base64
import os
import re
import shutil
import struct
import sys
import tempfile
import time
import zlib

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import pty_journal as pj  # noqa: E402
from pty_svg import RawSession, picture_cells, kitty_replies  # noqa: E402
from pty_pdf import make_pdf  # noqa: E402

ROWS, COLS = 40, 120
CTRL_Q = b"\x11"
PGDN = b"\x1b[6~"

TRANSMIT = re.compile(rb"\x1b_Ga=t,f=32,(?:t=.,)?q=2,i=(\d+)")
PLACE_AT = re.compile(rb"\x1b_Ga=p,q=2,C=1,i=(\d+),p=\d+,x=(\d+),y=(\d+),w=(\d+),h=(\d+),X=(\d+),Y=(\d+),z=(\d+)")
FREE = re.compile(rb"\x1b_Ga=d,d=I,q=2,i=(\d+)")
SINGLE = re.compile(rb"\x1b_Ga=T,f=32,i=1,q=2")
SIXEL = re.compile(rb"\x1bP[0-9;]*q")


def check(cond, msg):
    if not cond:
        raise pj.Failure(msg)


def setup(cfg, npages=5):
    home = tempfile.mkdtemp(prefix="cmedit-strip-")
    os.makedirs(os.path.join(home, ".config", "cmedit"))
    with open(os.path.join(home, ".config", "cmedit", "config"), "w") as f:
        f.write(cfg)
    p = os.path.join(home, "doc.pdf")
    with open(p, "wb") as f:
        f.write(make_pdf(npages))
    return home, p


def mouse(s, code, col, row, release=False):
    s.send(b"\x1b[<%d;%d;%d%s" % (code, col, row, b"m" if release else b"M"))


def drag(s, c0, r0, c1, r1, steps=4, settle=0.05):
    mouse(s, 0, c0, r0)
    s.pump(settle)
    for i in range(1, steps + 1):
        mouse(s, 32, c0 + (c1 - c0) * i // steps, r0 + (r1 - r0) * i // steps)
        s.pump(settle)


def tile_ids(raw):
    return {int(m.group(1)) for m in TRANSMIT.finditer(raw) if int(m.group(1)) >= 100000}


def scenario_kitty_strip():
    home, p = setup("pdf-fit = width\npdf-view = page\n")
    try:
        s = RawSession(home, [p], rows=ROWS, cols=COLS)
        try:
            t0 = time.monotonic()
            kitty_replies(s)
            check(s.until(lambda: len(PLACE_AT.findall(bytes(s.raw))) >= 3, 15), "no band placements appeared")
            t1 = time.monotonic()
            check(s.wait_screen("Width 100%", 5), "the status does not say fit width")
            check(s.until(lambda: "rendering" not in s.screen.text(), 10), "the first screen never finished")
            ids0 = tile_ids(bytes(s.raw))
            print("  first bands placed after %.0f ms; %d bands uploaded" % ((t1 - t0) * 1000, len(ids0)))
            check(picture_cells(s.screen) == 0, "half-block cells under the pixels")
            s.pump(1.0)                                   # let the prefetch finish
            mark = len(s.raw)
            for _ in range(3):
                mouse(s, 65, COLS // 2, ROWS // 2)        # wheel down
            check(s.until(lambda: PLACE_AT.search(bytes(s.raw[mark:])) is not None, 5), "the wheel moved nothing")
            s.pump(0.4)
            after = bytes(s.raw[mark:])
            resent = tile_ids(after) & ids0
            print("  3 wheel notches: %d bytes on the wire, %d placements, %d bands re-sent"
                  % (len(after), len(PLACE_AT.findall(after)), len(resent)))
            check(not resent, "a scroll re-sent bands already uploaded")
            for _ in range(4):
                s.send(PGDN)
                s.pump(0.15)
            check(s.until(lambda: re.search(r"Page [2-5]/5", s.screen.text()) is not None, 5),
                  "PgDn did not scroll past page 1")
            check(picture_cells(s.screen) == 0, "half-block cells after scrolling")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_kitty_select():
    home, p = setup("pdf-fit = width\npdf-view = page\n")
    try:
        s = RawSession(home, [p], rows=ROWS, cols=COLS)
        try:
            kitty_replies(s)
            check(s.until(lambda: len(PLACE_AT.findall(bytes(s.raw))) >= 3, 15), "no band placements appeared")
            s.until(lambda: "rendering" not in s.screen.text(), 10)
            s.pump(0.5)
            s.send(b"s")
            check(s.wait_screen("Select", 5), "S did not switch to selecting")
            # the title "This is page 1 of 5" is 72 pt down: at 952 px wide
            # (119 cols × 8) that is ~112 px, row 7 of the text area
            mark = len(s.raw)
            drag(s, 5, 6, 80, 10)
            hl = [m for m in PLACE_AT.finditer(bytes(s.raw[mark:])) if 99995 <= int(m.group(1)) <= 99997]
            check(hl, "the rubber band was not drawn as a pixel layer")
            check(picture_cells(s.screen) == 0, "the view fell back to cells mid-drag")
            print("  drag: %d highlight placements, pixels kept up" % len(hl))
            mouse(s, 0, 80, 10, release=True)
            check(s.wait_screen("Selected", 5), "the release did not report a selection")
            m = re.search(r"Selected (\d+) characters", s.screen.text())
            print("  selected %s characters" % (m.group(1) if m else "?"))
            mark = len(s.raw)
            s.send(b"\x03")                              # Ctrl+C
            check(s.wait_screen("Copied", 5), "Ctrl+C did not copy")
            osc = re.search(rb"\x1b\]52;c;([A-Za-z0-9+/=]+)", bytes(s.raw[mark:]))
            if osc:
                txt = base64.b64decode(osc.group(1)).decode("utf-8", "replace")
                print("  clipboard (OSC 52): %r" % txt[:60])
                check("This is page 1" in txt, "the copied text is not the title")
            else:
                print("  clipboard went to a helper (no OSC 52 to inspect)")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_kitty_fitpage_drag():
    home, p = setup("pdf-fit = width\npdf-view = page\n")
    try:
        s = RawSession(home, [p], rows=ROWS, cols=COLS)
        try:
            kitty_replies(s)
            check(s.until(lambda: len(PLACE_AT.findall(bytes(s.raw))) >= 3, 15), "no band placements appeared")
            s.pump(0.5)
            ids = tile_ids(bytes(s.raw))
            mark = len(s.raw)
            s.send(b"w")
            check(s.until(lambda: SINGLE.search(bytes(s.raw[mark:])) is not None, 10), "fit page did not place the page")
            freed = {int(m.group(1)) for m in FREE.finditer(bytes(s.raw[mark:]))}
            check(ids <= freed or b"a=d,d=A" in bytes(s.raw[mark:]), "the bands were not freed on leaving fit width")
            s.pump(0.5)
            mark = len(s.raw)
            drag(s, 40, 8, 80, 25)
            mid = bytes(s.raw[mark:])
            check(picture_cells(s.screen) == 0, "a zoom drag fell back to cells")
            check(any(99995 <= int(m.group(1)) <= 99997 for m in PLACE_AT.finditer(mid)), "the zoom band was not drawn")
            mark = len(s.raw)
            mouse(s, 0, 80, 25, release=True)
            check(s.until(lambda: SINGLE.search(bytes(s.raw[mark:])) is not None, 10), "the release did not zoom")
            check(s.until(lambda: "100%" not in s.screen.text(), 5), "the status does not show the zoom")
            print("  fit page: zoom drag kept pixels, release zoomed")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_sixel():
    home, p = setup("pdf-fit = width\npdf-view = page\n")
    try:
        s = RawSession(home, [p], rows=ROWS, cols=COLS)
        try:
            s.pump(0.3)
            s.send(b"\x1b[6;16;8t" + b"\x1b[?62;4;22c")   # cell size, DA1 with sixel
            check(s.until(lambda: SIXEL.search(bytes(s.raw)) is not None, 15), "no sixel composite")
            s.until(lambda: "rendering" not in s.screen.text(), 10)
            s.pump(0.5)
            mark = len(s.raw)
            mouse(s, 65, COLS // 2, ROWS // 2)
            check(s.until(lambda: SIXEL.search(bytes(s.raw[mark:])) is not None, 5), "scrolling sent no new composite")
            print("  sixel: composite re-sent on scroll (%d bytes)" % len(s.raw[mark:]))
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_cells():
    home, p = setup("pdf-fit = width\npdf-view = page\n")
    try:
        s = RawSession(home, [p], rows=ROWS, cols=COLS)
        try:
            check(s.until(lambda: picture_cells(s.screen) > 200, 15), "no half-block picture")
            check(s.wait_screen("Width 100%", 5), "the status does not say fit width")
            check(not TRANSMIT.search(bytes(s.raw)) and not SIXEL.search(bytes(s.raw)), "pixels sent to a silent terminal")
            s.pump(0.5)
            mark = len(s.raw)
            mouse(s, 65, COLS // 2, ROWS // 2)
            # (the glyphs are half-blocks either way; the colours move)
            check(s.until(lambda: len(s.raw) - mark > 2000, 5), "the wheel did not scroll the cells")
            s.send(b"\x01")                              # Ctrl+A
            check(s.wait_screen("Selected page", 5), "Ctrl+A did not select the page")
            s.send(b"\x03")
            check(s.wait_screen("Copied", 5), "Ctrl+C did not copy")
            print("  cells: scrolled, selected and copied the page")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def png(w, h):
    raw = b"".join(b"\x00" + bytes(v for x in range(w) for v in (x * 255 // w, y * 255 // h, 128))
                   for y in range(h))
    def chunk(t, d):
        return struct.pack(">I", len(d)) + t + d + struct.pack(">I", zlib.crc32(t + d) & 0xffffffff)
    return (b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0))
            + chunk(b"IDAT", zlib.compress(raw)) + chunk(b"IEND", b""))


def scenario_image_drag():
    home = tempfile.mkdtemp(prefix="cmedit-strip-")
    try:
        p = os.path.join(home, "pic.png")
        with open(p, "wb") as f:
            f.write(png(640, 400))
        s = RawSession(home, [p], rows=ROWS, cols=COLS)
        try:
            kitty_replies(s)
            check(s.until(lambda: SINGLE.search(bytes(s.raw)) is not None, 10), "the image was not placed")
            s.pump(0.3)
            mark = len(s.raw)
            drag(s, 30, 8, 70, 20)
            mid = bytes(s.raw[mark:])
            check(picture_cells(s.screen) == 0, "the image fell back to cells mid-drag")
            check(any(99995 <= int(m.group(1)) <= 99997 for m in PLACE_AT.finditer(mid)), "no band over the image")
            check(b"a=d,d=A" not in mid, "the drag deleted the image")
            mark = len(s.raw)
            mouse(s, 0, 70, 20, release=True)
            check(s.until(lambda: SINGLE.search(bytes(s.raw[mark:])) is not None, 10), "the release did not zoom")
            print("  image: zoom drag kept the pixels, band drawn, release zoomed")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_sixel_image_drag():
    home = tempfile.mkdtemp(prefix="cmedit-strip-")
    try:
        p = os.path.join(home, "pic.png")
        with open(p, "wb") as f:
            f.write(png(320, 200))
        s = RawSession(home, [p], rows=ROWS, cols=COLS)
        try:
            s.pump(0.3)
            s.send(b"\x1b[6;16;8t" + b"\x1b[?62;4;22c")
            check(s.until(lambda: SIXEL.search(bytes(s.raw)) is not None, 10), "the image was not sent as sixel")
            s.pump(0.3)
            mark = len(s.raw)
            drag(s, 30, 8, 70, 20)
            check(s.until(lambda: SIXEL.search(bytes(s.raw[mark:])) is not None, 5), "the band was not burned into a sixel")
            check(picture_cells(s.screen) == 0, "a sixel image fell back to cells mid-drag")
            mouse(s, 0, 70, 20, release=True)
            s.pump(0.5)
            print("  sixel image: band re-sent with the picture, no cells")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_sixel_fitpage_select():
    """Sixel, fit page, a text selection: the highlight is burned into the
    page's sixel (and computing it must not ask for the placement it is part
    of: that once recursed forever)."""
    home, p = setup("pdf-view = page\npdf-drag = select\n")
    try:
        s = RawSession(home, [p], rows=ROWS, cols=COLS)
        try:
            s.pump(0.3)
            s.send(b"\x1b[6;16;8t" + b"\x1b[?62;4;22c")
            check(s.until(lambda: SIXEL.search(bytes(s.raw)) is not None, 15), "the page was not sent as sixel")
            s.pump(0.5)
            mark = len(s.raw)
            drag(s, 40, 5, 90, 12)
            mouse(s, 0, 90, 12, release=True)
            check(s.wait_screen("Selected", 5), "no selection reported")
            check(s.until(lambda: SIXEL.search(bytes(s.raw[mark:])) is not None, 5), "the selection was not drawn into the sixel")
            s.send(b"\x03")
            check(s.wait_screen("Copied", 5), "Ctrl+C did not copy")
            print("  sixel fit page: selection burned in, copied")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def last_pointer(raw):
    m = re.findall(rb"\x1b\]22;([a-z-]+)", raw)
    return m[-1].decode() if m else None


def scenario_pointer_and_pan():
    """The pointer says what a drag would do (crosshair to zoom, I-beam to
    select, a hand with Ctrl where the picture can be dragged), and
    Ctrl+drag pans the fit-width column."""
    home, p = setup("pdf-fit = width\npdf-view = page\n")
    try:
        s = RawSession(home, [p], rows=ROWS, cols=COLS)
        try:
            kitty_replies(s)
            check(s.until(lambda: len(PLACE_AT.findall(bytes(s.raw))) >= 3, 15), "no band placements appeared")
            s.pump(0.5)
            mouse(s, 35, 40, 15)                       # hover
            check(s.until(lambda: last_pointer(bytes(s.raw)) == "crosshair", 3), "no crosshair over the page in zoom mode")
            s.send(b"s")
            check(s.until(lambda: last_pointer(bytes(s.raw)) == "text", 3), "S did not turn the pointer into an I-beam")
            mouse(s, 35 + 16, 41, 15)                  # hover with Ctrl
            check(s.until(lambda: last_pointer(bytes(s.raw)) == "grab", 3), "Ctrl did not show the grab hand")
            mark = len(s.raw)
            mouse(s, 0 + 16, 41, 20)                   # Ctrl+press
            check(s.until(lambda: last_pointer(bytes(s.raw)) == "grabbing", 3), "a Ctrl+press did not grab")
            for k in range(1, 6):
                mouse(s, 32 + 16, 41, 20 - k)
                s.pump(0.05)
            mouse(s, 0 + 16, 41, 15, release=True)
            check(s.until(lambda: PLACE_AT.search(bytes(s.raw[mark:])) is not None, 3), "the pan moved nothing")
            check("Selected" not in s.screen.text(), "a pan made a selection")
            mouse(s, 35, 42, 15)
            check(s.until(lambda: last_pointer(bytes(s.raw)) == "text", 3), "the pointer did not return after the pan")
            print("  pointer: crosshair / text / grab / grabbing; Ctrl+drag panned")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def main():
    failed = 0
    for name, fn in [("kitty fit width", scenario_kitty_strip),
                     ("kitty select", scenario_kitty_select),
                     ("kitty fit page drags", scenario_kitty_fitpage_drag),
                     ("sixel fit width", scenario_sixel),
                     ("cells fit width", scenario_cells),
                     ("kitty image drag", scenario_image_drag),
                     ("sixel image drag", scenario_sixel_image_drag),
                     ("sixel fit page select", scenario_sixel_fitpage_select),
                     ("pointer shapes and pan", scenario_pointer_and_pan)]:
        print(name)
        try:
            fn()
            print("  ok")
        except pj.Failure as e:
            failed += 1
            print("  FAIL: %s" % e)
    print("failed %d" % failed)
    sys.exit(failed)


if __name__ == "__main__":
    main()
