#!/usr/bin/env python3
"""PTY verification of reduced-scale JPEG viewing (plan 0035, D2-D4).

Drives the real ./cmedit binary over a pseudo-terminal (the VT emulator and
Session of pty_journal.py, the byte-keeping RawSession of pty_svg.py) and
checks:

  a. open: a 28 MP JPEG (big420.jpg) opens on a fake kitty terminal and its
     first placement arrives quickly; the time from spawn to that placement,
     and the resident memory once idle (VmRSS, VmHWM), are printed. Run the
     same scenario against another binary (CMEDIT_BIN=...) for a before/after.
  b. refinement: wheel notches over the picture until the view needs more
     pixels than the 1/4 decode has; the status bar says "(1/4 scale)" while
     the finer decode runs, then a placement of the full-resolution crop
     arrives, and its pixels equal a full decode of that crop (Pillow's,
     which ours is bit-exact with) area-averaged exactly as the editor does.
     The crop is predicted by replaying the editor's wheel-zoom arithmetic,
     and the prediction is checked against the payload size it implies.
  c. revert mid-job: start a refinement, replace the file on disk with a
     different picture of the same size, and Revert while the job runs; no
     pixel of the old file is ever placed after the revert, and zooming the
     new file refines to *its* pixels.
  d. the dumb-terminal floor (no replies): the half-block picture appears,
     the status bar names the full size, and no placement is sent.
  e. a stream: the photo arrives through a pipe (/dev/fd/N, as
     `cmedit <(cat big.jpg)` gives it), which cannot be read twice, so it
     opens at full size; switching to another tab and back must keep it
     there (parking returns to the opening scale, not the one a file would
     have opened at), and zooming in then places full-resolution pixels with
     no re-read, no "(1/4 scale)" note, no stuck note and no hang.

Needs Pillow and numpy (the reference decode). Scratch goes under $TMPDIR.

    python3 docs/plans/bench/pty_jpegview.py [scenario...]
    CMEDIT_BIN=/path/to/other/cmedit python3 docs/plans/bench/pty_jpegview.py open

Exit status is the number of failed scenarios.
"""

import base64
import os
import re
import shutil
import sys
import tempfile
import threading
import time

import numpy as np
from PIL import Image as PILImage

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import pty_journal as pj  # noqa: E402
import pty_svg as ps  # noqa: E402

if os.environ.get("CMEDIT_BIN"):
    pj.BINARY = os.path.abspath(os.environ["CMEDIT_BIN"])

ROWS, COLS = 40, 120
CELL = (8, 16)                       # the harness's TIOCSWINSZ pixel size per cell
MAX_GFX = 1200 * 1000                # Cmedit.Gfx.maxGfxPixels
PHOTO = os.environ.get("JPEG_PHOTO",
                       os.path.expanduser("~/.cache/cmedit-work/jpeg/corpus-photos/big420.jpg"))
CTRL_Q = b"\x11"
KITTY_REPLY = b"\x1b_Gi=31;OK\x1b\\" + b"\x1b[6;16;8t" + b"\x1b[?62;22c"
PLACE = re.compile(rb"\x1b_Ga=T,f=32,i=1,q=2,s=(\d+),v=(\d+),c=(\d+),r=(\d+),m=(\d)")

check = ps.check


# ---------------------------------------------------------------- the editor's arithmetic

def hround(x):
    """Haskell's round: to nearest, ties to even (Python's round agrees)."""
    return int(round(x))


def view_fit(aspect, cap, cols, rows, cw, ch):
    cols = max(1, cols)
    sub_h = max(1, rows) * 2
    cw = max(1, cw)
    ch = max(1, ch)
    sc_fit = min(cols / cw, sub_h * aspect / ch)
    sc = sc_fit if cap is None else min(cap, sc_fit)
    out_w = max(1, min(cols, hround(cw * sc)))
    out_h = max(1, min(sub_h, hround(ch * sc / aspect)))
    return out_w, out_h, (cols - out_w) // 2, (sub_h - out_h) // 2


def gfx_fit(cell, geom, crop_wh, allow_up):
    pw, ph = cell
    top, left, tw, th = geom
    cw, ch = max(1, crop_wh[0]), max(1, crop_wh[1])
    sc_fit = min(tw * pw / cw, th * ph / ch)
    sc = sc_fit if allow_up else min(sc_fit, 1.0)
    cols = max(1, min(tw, hround(cw * sc / pw)))
    rows = max(1, min(th, hround(ch * sc / ph)))
    disp_w, disp_h = cols * pw, rows * ph
    cap = min(1.0, (MAX_GFX / max(1, disp_w * disp_h)) ** 0.5, cw / disp_w, ch / disp_h)
    return (top + (th - rows) // 2, left + (tw - cols) // 2, cols, rows,
            max(1, hround(disp_w * cap)), max(1, hround(disp_h * cap)))


class View:
    """The image view's zoom state, replayed (EditorDoc.wheelZoom)."""

    def __init__(self, full, tw, th):
        self.full = full
        self.tw, self.th = tw, th
        self.aspect = min(1.6, max(0.7, CELL[1] / (2 * CELL[0])))
        self.crop = None

    def cap(self, crop):
        return None if crop is not None else 1.0 / CELL[0]

    def cur(self):
        return self.crop or (0, 0) + self.full

    def wheel_in(self, row, col):
        a = self.aspect
        cols = max(1, self.tw)
        sub_h = 2 * max(1, self.th)
        iw, ih = self.full
        cx, cy, cw, ch = self.cur()
        out_w, out_h, off_x, off_y = view_fit(a, self.cap(self.crop), cols, sub_h // 2, cw, ch)
        full_w = view_fit(a, self.cap(None), cols, sub_h // 2, iw, ih)[0]
        s_cur = out_w / max(1, cw)
        s_full = full_w / max(1, iw)
        s_max = max(s_full, cols / 4)
        s2 = min(s_max, s_cur * 1.25)

        def clampd(lo, hi, v):
            return max(lo, min(hi, v))
        px = clampd(cx, cx + cw, cx + ((col - off_x) + 0.5) * cw / max(1, out_w))
        py = clampd(cy, cy + ch, cy + (2 * row + 1 - off_y) * ch / max(1, out_h))
        win_w = cols / s2
        win_h = sub_h * a / s2

        def place(p, screen, win, full):
            if win >= full:
                return 0, full
            w = max(1, min(full, hround(win)))
            x0 = hround(p - screen / s2)
            return max(0, min(full - w, x0)), w
        nx, nw = place(px, col + 0.5, win_w, iw)
        ny, nh = place(py, (2 * row + 1) * a, win_h, ih)
        whole = nw == iw and nh == ih
        if s_cur >= s_max:
            return
        if s2 <= s_full * 1.0001:
            self.crop = None
        else:
            self.crop = (nx, ny, nw, nh)
        del whole

    def payload(self, den):
        """(pxW, pxH) of the placement at decode scale 1/den."""
        cx, cy, cw, ch = self.cur()
        allow_up = self.crop is not None
        _, _, _, _, pw, ph = gfx_fit(CELL, (1, 0, self.tw, self.th), (cw, ch), allow_up)
        if den == 1:
            return pw, ph
        dcrop = sample_crop(self.cur(), self.full, den)
        shrink = min(1.0, dcrop[2] / max(1, pw), dcrop[3] / max(1, ph))
        if shrink < 1:
            return (max(1, min(dcrop[2], hround(pw * shrink))),
                    max(1, min(dcrop[3], hround(ph * shrink))))
        return pw, ph

    def need(self):
        cx, cy, cw, ch = self.cur()
        pw, ph = self.payload(1)
        ok = [d for d in (1, 2, 4, 8) if cw * 16 >= pw * d * 15 and ch * 16 >= ph * d * 15]
        return ok[-1] if ok else 1


def sample_crop(crop, full, den):
    """EditorState.pictureSample's rectangle."""
    x, y, w, h = crop
    fw, fh = full
    dw, dh = -(-fw // den), -(-fh // den)

    def edge(v, f, d):
        # by the scale: decoded pixel i covers source [i*den, (i+1)*den)
        return d if v >= f else min(d, (2 * v + den) // (2 * den))
    x0 = max(0, min(dw - 1, edge(x, fw, dw)))
    y0 = max(0, min(dh - 1, edge(y, fh, dh)))
    x1 = max(0, min(dw, edge(x + w, fw, dw)))
    y1 = max(0, min(dh, edge(y + h, fh, dh)))
    return x0, y0, max(1, x1 - x0), max(1, y1 - y0)


def scale_rgba(rgba, crop, out_w, out_h):
    """Cmedit.Image.scaleRGBA (box average, floor) for a crop inside the image."""
    cx, cy, cw, ch = crop
    xs = np.arange(out_w)
    ys = np.arange(out_h)
    x0 = cx + (xs * cw) // out_w
    x1 = np.maximum(x0 + 1, cx + ((xs + 1) * cw) // out_w)
    y0 = cy + (ys * ch) // out_h
    y1 = np.maximum(y0 + 1, cy + ((ys + 1) * ch) // out_h)
    sub = rgba[: y1.max(), : x1.max()].astype(np.int64)
    integ = np.zeros((sub.shape[0] + 1, sub.shape[1] + 1, 4), dtype=np.int64)
    integ[1:, 1:] = sub.cumsum(0).cumsum(1)
    tot = (integ[y1][:, x1] - integ[y0][:, x1] - integ[y1][:, x0] + integ[y0][:, x0])
    n = ((y1 - y0)[:, None] * (x1 - x0)[None, :])[:, :, None]
    return (tot // n).astype(np.uint8)


def full_rgba(path):
    im = PILImage.open(path)
    return np.asarray(im.convert("RGBA"))


# ---------------------------------------------------------------- reading placements

def placements(raw):
    """Every kitty placement in the output: (offset, w, h, cols, rows, payload bytes)."""
    out = []
    for m in PLACE.finditer(raw):
        w, h, c, r, more = (int(g) for g in m.groups())
        try:
            i = raw.index(b";", m.end()) + 1
            j = raw.index(b"\x1b\\", i)
            chunks = [raw[i:j]]
            pos = j + 2
            while more:
                k = raw.index(b"\x1b_Gm=", pos)
                more = int(raw[k + 5:k + 6])
                i = raw.index(b";", k) + 1
                j = raw.index(b"\x1b\\", i)
                chunks.append(raw[i:j])
                pos = j + 2
        except ValueError:
            break                      # the last placement is still arriving
        out.append((m.start(), w, h, c, r, base64.b64decode(b"".join(chunks))))
    return out


def vm(pid, key):
    with open("/proc/%d/status" % pid) as f:
        for line in f:
            if line.startswith(key + ":"):
                return int(line.split()[1]) // 1024
    return -1


def kitty_session(home, args):
    s = ps.RawSession(home, args, rows=ROWS, cols=COLS)
    t0 = time.monotonic()
    # answer the capability burst as soon as it is sent
    check(s.until(lambda: b"\x1b_Gi=31" in s.raw, 20), "the editor never probed for kitty graphics")
    s.send(KITTY_REPLY)
    return s, t0


def mouse_wheel_up(row, col):
    # SGR mouse, 1-based; the text area starts on screen row 1 (below the menu bar)
    return b"\x1b[<64;%d;%dM" % (col + 1, row + 2)


def wait_quiet(s, secs=0.6, timeout=20):
    """Pump until the editor has written nothing for @secs@."""
    end = time.monotonic() + timeout
    n = len(s.raw)
    quiet_since = time.monotonic()
    while time.monotonic() < end:
        s.pump(0.05)
        if len(s.raw) != n:
            n = len(s.raw)
            quiet_since = time.monotonic()
        elif time.monotonic() - quiet_since >= secs:
            return True
    return False


# ---------------------------------------------------------------- scenarios

def new_home():
    return tempfile.mkdtemp(prefix="cmedit-jpegview-")


def scenario_open():
    home = new_home()
    try:
        path = os.path.join(home, "big.jpg")
        shutil.copy(PHOTO, path)
        s, t0 = kitty_session(home, [path])
        try:
            check(s.until(lambda: PLACE.search(bytes(s.raw)) is not None, 30), "no kitty placement appeared")
            t1 = time.monotonic()
            (_, w, h, c, r, _), = placements(bytes(s.raw))[:1]
            print("  first placement %dx%d px in %dx%d cells %.0f ms after spawn" % (w, h, c, r, (t1 - t0) * 1000))
            # Idle: the RTS collects after 2 s idle (-I2) and hands pages back
            # at once (--disable-delayed-os-memory-return).
            wait_quiet(s, 0.5)
            s.pump(3.5)
            rss, hwm = vm(s.pid, "VmRSS"), vm(s.pid, "VmHWM")
            print("  resident once idle: VmRSS %d MB, VmHWM (peak) %d MB" % (rss, hwm))
            check(ps.picture_cells(s.screen) == 0, "half-block cells drawn under the placement")
            if not os.environ.get("CMEDIT_BIN"):
                # (HEAD ec8265b, full-size decode: ~4 s and ~186 MB; an empty
                # editor sits at ~46 MB)
                check((t1 - t0) < 0.25, "first placement took %.0f ms (target 250)" % ((t1 - t0) * 1000))
                check(rss < 110, "VmRSS %d MB: the decode is not reduced" % rss)
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)
    return (t1 - t0) * 1000, rss, hwm


def zoom_until_full(s, view, burst=False):
    """Send wheel notches (replayed in @view@) until the view needs 1/1.
    With @burst@ they go in one write, so the editor (which applies a whole
    batch of input before one frame) asks for the full decode directly;
    otherwise each notch gets its frame, and a 1/2 decode may come first."""
    row, col = view.th // 2, view.tw // 2
    notches = 0
    out = b""
    while view.need() > 1 and notches < 40:
        view.wheel_in(row, col)
        notches += 1
        if burst:
            out += mouse_wheel_up(row, col)
        else:
            s.send(mouse_wheel_up(row, col))
            s.pump(0.03)
    if burst:
        s.send(out)
    return notches


SCALE_NOTE = re.compile(r"\(1/[248] scale\)")


def scale_note(s):
    return SCALE_NOTE.search(s.screen.text()) is not None


def check_full_crop(s, view, ref, after):
    """Wait for the 1/1 placement of the view's crop; compare it to @ref@."""
    want = view.payload(1)
    head = re.compile(rb"\x1b_Ga=T,f=32,i=1,q=2,s=%d,v=%d,c=\d+,r=\d+,m=(\d);" % want)
    found = [None]

    def landed():
        # only new bytes are searched, and nothing is decoded until the end
        raw = bytes(s.raw)
        if found[0] is None:
            m = head.search(raw, after)
            if m is None:
                return False
            found[0] = m
        m = found[0]
        return m.group(1) == b"0" or raw.find(b"\x1b_Gm=0;", m.end()) >= 0
    check(s.until(landed, 30), "no full-resolution placement of %s arrived (want %dx%d, saw %s)"
          % (view.cur(), want[0], want[1],
             [(p[1], p[2]) for p in placements(bytes(s.raw)) if p[0] >= after]))
    # the last one: notch by notch, an earlier crop can have the same payload size
    wait_quiet(s, 0.5)
    p = [q for q in placements(bytes(s.raw)) if q[0] >= after and (q[1], q[2]) == want][-1]
    got = np.frombuffer(p[5], dtype=np.uint8).reshape(want[1], want[0], 4)
    exp = scale_rgba(ref, view.cur(), want[0], want[1])
    diff = int(np.abs(got.astype(int) - exp.astype(int)).max())
    return diff, want


def scenario_refine():
    home = new_home()
    try:
        path = os.path.join(home, "big.jpg")
        shutil.copy(PHOTO, path)
        full = PILImage.open(path).size
        s, _ = kitty_session(home, [path])
        try:
            check(s.until(lambda: PLACE.search(bytes(s.raw)) is not None, 30), "no kitty placement appeared")
            wait_quiet(s)
            (_, w0, h0, c0, r0, _) = placements(bytes(s.raw))[-1]
            tw, th = COLS - 1, r0          # full width less the scroll-bar column; height-bound fit
            view = View(full, tw, th)
            check(view.payload(1) == (w0, h0),
                  "the replayed fit (%s) does not match the first placement (%dx%d)" % (view.payload(1), w0, h0))
            check(s.screen.has("%d×%d" % full), "the status bar does not name the full size")
            ref = full_rgba(path)
            mark = len(s.raw)
            t2 = time.monotonic()
            n = zoom_until_full(s, view, burst=True)
            print("  %d notches (one burst) to a crop of %s that needs the full decode" % (n, view.cur()))
            pending = s.until(lambda: scale_note(s), 3)
            check(pending, "the status bar never said the picture was reduced while refining")
            check(s.screen.has("(1/4 scale)"), "the note does not name the scale showing (1/4)")
            # The note goes in the frame that installs the finer decode (and
            # places it): a tight poll, so the time is the editor's.
            end = time.monotonic() + 10
            while scale_note(s) and time.monotonic() < end:
                s.pump(0.005)
            t3 = time.monotonic()
            check(not scale_note(s), "the scale note stayed after refining")
            diff, want = check_full_crop(s, view, ref, mark)
            print("  full-resolution %dx%d placement %.0f ms after the notches, max diff vs a full decode: %d"
                  % (want[0], want[1], (t3 - t2) * 1000, diff))
            check(diff == 0, "the refined placement differs from a full decode (max %d)" % diff)
            # The coarse placement of the same crop stayed up meanwhile.
            coarse = view.payload(4)
            seen = [(p[1], p[2]) for p in placements(bytes(s.raw)) if p[0] >= mark]
            check(coarse in seen and seen.index(coarse) < seen.index(want),
                  "the coarse placement (%dx%d) did not precede the full one: %s" % (coarse + (seen,)))
            # Zooming in one notch at a time instead: each notch gets a frame,
            # so a 1/2 decode may land first, and a later one finishes the job.
            s.send(b"\x1b")                 # Esc: back to the fit (the decode stays at 1/1)
            wait_quiet(s)
            check(not scale_note(s), "the fit of a refined picture says it is reduced")
            s.pump(3.5)
            print("  resident after refining: VmRSS %d MB, VmHWM %d MB" % (vm(s.pid, "VmRSS"), vm(s.pid, "VmHWM")))
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_revert():
    home = new_home()
    try:
        path = os.path.join(home, "big.jpg")
        shutil.copy(PHOTO, path)
        full = PILImage.open(path).size
        # the replacement: the same size, mirrored, so a stale install is visible
        other = os.path.join(home, "other.jpg")
        PILImage.open(path).transpose(PILImage.FLIP_LEFT_RIGHT).save(other, quality=88, subsampling=2)
        s, _ = kitty_session(home, ["--stats-on-exit", path])
        try:
            check(s.until(lambda: PLACE.search(bytes(s.raw)) is not None, 30), "no kitty placement appeared")
            wait_quiet(s)
            (_, w0, h0, _, r0, _) = placements(bytes(s.raw))[-1]
            view = View(full, COLS - 1, r0)
            check(view.payload(1) == (w0, h0), "the replayed fit does not match the first placement")
            # The file changes on disk first; then, in quick succession so the
            # refinement is still decoding: zoom, focus-in (which polls the
            # file and offers Revert), File, Revert.
            os.replace(other, path)
            st = os.stat(path)
            os.utime(path, ns=(st.st_atime_ns, st.st_mtime_ns + 2_000_000_000))
            zmark = len(s.raw)
            zoom_until_full(s, view, burst=True)
            s.pump(0.01)
            s.send(b"\x1b[I")
            s.pump(0.005)
            s.send(b"\x1bf")
            s.pump(0.005)
            s.send(b"v")
            mark = len(s.raw)
            # The old model's refinement must not have landed before the revert
            # (else this run proves nothing about a late result).
            early = [p for p in placements(bytes(s.raw[:mark])) if p[0] >= zmark and (p[1], p[2]) == view.payload(1)]
            check(not early, "the refinement landed before the revert (rerun)")
            check(s.until(lambda: s.screen.has("Viewing image"), 5), "the Revert did not reload the picture")
            wait_quiet(s, 1.5)
            new = full_rgba(path)
            fit_view = View(full, COLS - 1, r0)
            fit = fit_view.payload(4)
            after = [p for p in placements(bytes(s.raw)) if p[0] >= mark]
            # From the new model's first placement (its fit) on, nothing else
            # is placed: a stale refinement landing would show as a zoomed one.
            sizes = [(p[1], p[2]) for p in after]
            check(fit in sizes, "the new file's fit (%s) was never placed: %s" % (fit, sizes))
            check(view.payload(1) not in sizes[:sizes.index(fit)],
                  "the refinement landed just before the revert (rerun): %s" % sizes)
            after = after[sizes.index(fit):]
            for p in after:
                check((p[1], p[2]) == fit, "a placement of %dx%d (not the new fit %s) after the revert"
                      % (p[1], p[2], fit))
            got = np.frombuffer(after[-1][5], dtype=np.uint8).reshape(fit[1], fit[0], 4).astype(int)
            new_q = scale_rgba(new, (0, 0) + full, fit[0], fit[1]).astype(int)
            old_q = scale_rgba(full_rgba(PHOTO), (0, 0) + full, fit[0], fit[1]).astype(int)
            d_new = float(np.abs(got - new_q).mean())
            d_old = float(np.abs(got - old_q).mean())
            print("  after the revert: %d placement(s); mean diff to the new file %.2f, to the old %.2f"
                  % (len(after), d_new, d_old))
            check(d_new < d_old / 4, "the picture after the revert looks like the old file")
            check(not s.screen.has("changed since"), "the reverted picture was marked stuck")
            check(not scale_note(s), "the reverted picture still says it is reduced")
            # and the new model refines to its own pixels, a notch at a time
            mark2 = len(s.raw)
            zoom_until_full(s, fit_view)
            diff, want = check_full_crop(s, fit_view, new, mark2)
            print("  the new file refines too: %dx%d, max diff vs its full decode %d" % (want[0], want[1], diff))
            check(diff == 0, "the new file's refinement differs from its full decode")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
            tail = bytes(s.raw[-2000:]).decode("utf-8", "replace")
            m = re.search(r"(\d+) background jobs? \((\d+) cancelled", tail)
            check(m is not None, "no --stats-on-exit line")
            # The refinement in flight at the revert was either cancelled by the
            # upkeep (the new model at its fit wants nothing) or finished and
            # posted a result for the old epoch, which was dropped.
            print("  stats: %s background jobs, %s cancelled (%s)"
                  % (m.group(1), m.group(2),
                     "the stale refinement was cancelled" if int(m.group(2)) >= 1
                     else "the stale refinement finished and its result was dropped"))
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_dumb():
    home = new_home()
    try:
        path = os.path.join(home, "big.jpg")
        shutil.copy(PHOTO, path)
        full = PILImage.open(path).size
        s = ps.RawSession(home, [path], rows=ROWS, cols=COLS)
        try:
            t0 = time.monotonic()
            check(s.until(lambda: ps.picture_cells(s.screen) > 500, 30), "no half-block picture appeared")
            t1 = time.monotonic()
            print("  half-block picture %.0f ms after spawn (%d cells)" % ((t1 - t0) * 1000, ps.picture_cells(s.screen)))
            check(s.wait_screen("%d×%d" % full, 5), "the status bar does not name the full size")
            # a few notches: the cell picture changes, no decode is ever needed for cells
            before = s.screen.text()
            for _ in range(3):
                s.send(mouse_wheel_up(18, 59))
                s.pump(0.05)
            check(s.until(lambda: s.screen.text() != before and "Zoom" in s.screen.text(), 5),
                  "wheel zoom did not change the cell picture")
            wait_quiet(s)
            check(b"\x1b_Ga=T" not in s.raw, "a pixel placement was sent to a silent terminal")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_pipe():
    home = new_home()
    rfd, wfd = os.pipe()
    try:
        note = os.path.join(home, "note.txt")
        with open(note, "w") as f:
            f.write("a note beside the picture\n")
        full = PILImage.open(PHOTO).size
        with open(PHOTO, "rb") as f:
            data = f.read()
        os.set_inheritable(rfd, True)     # the only end the editor gets

        # The editor reads the stream to its end before its first frame, so
        # the writer runs from the start.
        def feed():
            try:
                view = memoryview(data)
                while view:
                    view = view[os.write(wfd, view):]
            except OSError:
                pass                          # the editor went away
            finally:
                os.close(wfd)
        threading.Thread(target=feed, daemon=True).start()
        s, _ = kitty_session(home, ["/dev/fd/%d" % rfd, note])
        os.close(rfd)
        rfd = -1
        try:
            s.send(b"\x1b1")                 # Alt+1: the picture
            check(s.until(lambda: PLACE.search(bytes(s.raw)) is not None, 30), "no kitty placement of the piped picture")
            wait_quiet(s)
            (_, w0, h0, _, r0, _) = placements(bytes(s.raw))[-1]
            view = View(full, COLS - 1, r0)
            check(view.payload(1) == (w0, h0), "the replayed fit does not match the first placement")
            check(s.screen.has("%d×%d" % full), "the status bar does not name the full size")
            # away and back
            s.send(b"\x1b2")
            check(s.wait_screen("a note beside the picture", 5), "Alt+2 did not show the note")
            wait_quiet(s)
            mark = len(s.raw)
            s.send(b"\x1b1")
            check(s.until(lambda: any(p[0] >= mark for p in placements(bytes(s.raw))), 10),
                  "no placement after switching back to the picture")
            wait_quiet(s)
            ref = full_rgba(PHOTO)
            mark = len(s.raw)
            zoom_until_full(s, view, burst=True)
            diff, want = check_full_crop(s, view, ref, mark)
            wait_quiet(s, 1.0)
            print("  after a tab switch, zoomed: %dx%d placement, max diff vs a full decode %d" % (want[0], want[1], diff))
            check(diff == 0, "the zoomed placement differs from a full decode (the picture was degraded)")
            check(not scale_note(s), "the piped picture says it is reduced")
            check(not s.screen.has("Showing this picture"), "the piped picture went stuck")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit (a re-read blocked?)")
        finally:
            s.close()
    finally:
        if rfd >= 0:
            os.close(rfd)
        shutil.rmtree(home, ignore_errors=True)


SCENARIOS = [("open", "open a 28 MP JPEG on kitty: time and memory", scenario_open),
             ("refine", "zoom refines to a full-resolution placement", scenario_refine),
             ("revert", "Revert mid-refinement installs nothing stale", scenario_revert),
             ("dumb", "dumb-terminal floor: half-block picture", scenario_dumb),
             ("pipe", "a piped JPEG survives a tab switch at full size", scenario_pipe)]


def main():
    if not os.path.exists(pj.BINARY):
        print("build ./cmedit first (make)")
        return 1
    if not os.path.exists(PHOTO):
        print("missing %s (JPEG_PHOTO=...)" % PHOTO)
        return 1
    want = sys.argv[1:] or [k for k, _, _ in SCENARIOS]
    print("binary: %s" % pj.BINARY)
    failed = 0
    for key, name, fn in SCENARIOS:
        if key not in want:
            continue
        print(name)
        try:
            fn()
            print("  PASS")
        except (pj.Failure, ValueError, IndexError) as e:
            failed += 1
            print("  FAIL: %s" % e)
    print("%d failed" % failed)
    return failed


if __name__ == "__main__":
    sys.exit(main())
