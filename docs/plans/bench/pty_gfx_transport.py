#!/usr/bin/env python3
"""PTY verification of kitty graphics shm (t=s) and zlib (o=z) transport
(plan 0041).

Drives the real ./cmedit binary over a pseudo-terminal and verifies:

  1. Silent terminal: no capability replies -> half-block fallback, no kitty
     placements, zero /dev/shm/cm-* objects created.
  2. Plain kitty (i=31;OK only): i=32 probe shm is created once after i=31;OK
     and swept after the 2 s timeout (or on exit); placements use plain inline
     t=d with exact raw RGBA bytes.
  3. Remote kitty (i=31;OK + i=33;OK + i=32;ENOENT): probe shm unlinked
     immediately on ENOENT; SVG and PDF placements use o=z and decompress via
     Python's zlib.decompress to exact s*v*4 bytes at >10x wire reduction;
     noisy incompressible PNG bails out cleanly to raw t=d.
  4. Local kitty (i=31;OK + i=32;OK + i=33;OK): verifies i=32 probe shm object
     (4 bytes), placements use t=s (<128 B on wire, no o=z), shm object holds
     exact s*v*4 RGBA bytes; also verifies unread t=s shm objects are unlinked
     on quit so zero /dev/shm/cm-* objects remain after exit.
  5. --stats-on-exit reports graphics placements and active transport mode.

    python3 docs/plans/bench/pty_gfx_transport.py

Exit status is the number of failed scenarios.
"""

import base64
import glob
import os
import random
import re
import shutil
import signal
import struct
import sys
import tempfile
import time
import zlib

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import pty_journal as pj  # noqa: E402
from pty_svg import ICON, RawSession, check, picture_cells, write  # noqa: E402

ROWS, COLS = 40, 120
CTRL_Q = b"\x11"
PGDN = b"\x1b[6~"

APC_RE = re.compile(rb"\x1b_G([^;\x1b]*);([^\x1b]*)\x1b\\")


def shm_cm_objects(pid=None):
    if pid is not None:
        return set(glob.glob(f"/dev/shm/cm-{pid}-*"))
    return set(glob.glob("/dev/shm/cm-*"))


def parse_kv(header_bytes):
    kv = {}
    for part in header_bytes.decode("ascii", errors="replace").split(","):
        if "=" in part:
            k, v = part.split("=", 1)
            kv[k] = v
    return kv


def extract_placements(raw_bytes):
    """Parse kitty a=T placements (reassembling m=1..m=0 chunks) from raw bytes.
    Returns list of (kv_dict, payload_bytes, wire_len)."""
    out = []
    matches = list(APC_RE.finditer(raw_bytes))
    i = 0
    while i < len(matches):
        m = matches[i]
        kv = parse_kv(m.group(1))
        if kv.get("a") == "T":
            chunks = [m.group(2)]
            wire = len(m.group(0))
            more = kv.get("m") == "1"
            j = i + 1
            while more and j < len(matches):
                m2 = matches[j]
                kv2 = parse_kv(m2.group(1))
                chunks.append(m2.group(2))
                wire += len(m2.group(0))
                more = kv2.get("m") == "1"
                j += 1
            if not more:
                b64 = b"".join(chunks)
                out.append((kv, base64.b64decode(b64) if b64 else b"", wire))
            i = j
        else:
            i += 1
    return out


def find_probe(raw_bytes, probe_id):
    for m in APC_RE.finditer(raw_bytes):
        kv = parse_kv(m.group(1))
        if kv.get("a") == "q" and kv.get("i") == str(probe_id):
            return kv, base64.b64decode(m.group(2)) if m.group(2) else b""
    return None


def make_noisy_png(path, w=200, h=200):
    """Generate a valid RGB PNG filled with pseudo-random noise so its decoded
    RGBA pixels are genuinely incompressible (exercises the 64 KiB bailout)."""
    rng = random.Random(0x5EED1234)
    raw = bytearray()
    for _ in range(h):
        raw.append(0)  # filter type None
        raw.extend(rng.randbytes(w * 3))

    def chunk(tag, data):
        crc = zlib.crc32(tag + data) & 0xFFFFFFFF
        return struct.pack(">I", len(data)) + tag + data + struct.pack(">I", crc)

    ihdr = struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0)
    png = (
        b"\x89PNG\r\n\x1a\n"
        + chunk(b"IHDR", ihdr)
        + chunk(b"IDAT", zlib.compress(bytes(raw), level=1))
        + chunk(b"IEND", b"")
    )
    with open(path, "wb") as f:
        f.write(png)
    return path


def scenario_silent():
    home = tempfile.mkdtemp(prefix="cmedit-gfx-silent-")
    try:
        path = write(home, "icon.svg", ICON)
        s = RawSession(home, [path], rows=ROWS, cols=COLS)
        try:
            check(s.until(lambda: picture_cells(s.screen) > 50, 10),
                  "half-block picture did not appear on silent terminal")
            check(len(extract_placements(bytes(s.raw))) == 0,
                  "kitty placement emitted to silent terminal")
            check(find_probe(bytes(s.raw), 31) is not None,
                  "i=31 probe was not sent at startup")
            check(find_probe(bytes(s.raw), 33) is not None,
                  "i=33 zlib probe was not sent at startup")
            check(find_probe(bytes(s.raw), 32) is None,
                  "i=32 local shm probe must NOT be sent before i=31;OK")
            check(len(shm_cm_objects(s.pid)) == 0,
                  "shm object created for silent terminal")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_plain_kitty():
    home = tempfile.mkdtemp(prefix="cmedit-gfx-plain-")
    try:
        path = write(home, "icon.svg", ICON)
        s = RawSession(home, [path], rows=ROWS, cols=COLS)
        try:
            s.pump(0.2)
            # Answer i=31;OK only (no i=32 or i=33 reply)
            s.send(b"\x1b_Gi=31;OK\x1b\\" + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
            check(s.until(lambda: find_probe(bytes(s.raw), 32) is not None, 5),
                  "i=32 local probe was not emitted after i=31;OK")
            check(s.until(lambda: len(extract_placements(bytes(s.raw))) >= 1, 10),
                  "no kitty placement appeared")
            pls = extract_placements(bytes(s.raw))
            kv, payload, wire = pls[-1]
            w, h = int(kv["s"]), int(kv["v"])
            check(kv.get("t", "d") == "d", f"expected plain t=d, got t={kv.get('t')}")
            check("o" not in kv, f"unexpected compression o={kv.get('o')} without i=33;OK")
            check(len(payload) == w * h * 4,
                  f"raw payload size {len(payload)} != {w*h*4}")
            # Wait > 2.5s for the 2 s probe expiry sweep on FsTick
            check(s.until(lambda: len(shm_cm_objects(s.pid)) == 0, 5),
                  f"unanswered i=32 probe shm was not swept: {shm_cm_objects(s.pid)}")
            print(f"  plain kitty: {w}x{h} inline raw {wire} B wire; probe swept cleanly")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            pid = s.pid
            s.close()
        check(len(shm_cm_objects(pid)) == 0, f"shm leak after exit: {shm_cm_objects(pid)}")
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_remote_zlib():
    home = tempfile.mkdtemp(prefix="cmedit-gfx-zlib-")
    repo_root = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
    slides = os.path.join(repo_root, "cmedit-slides.pdf")
    try:
        # 3a. SVG over remote kitty (o=z)
        svg_path = write(home, "icon.svg", ICON)
        s = RawSession(home, [svg_path], rows=ROWS, cols=COLS)
        try:
            s.pump(0.2)
            s.send(b"\x1b_Gi=31;OK\x1b\\" + b"\x1b_Gi=33;OK\x1b\\"
                   + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
            check(s.until(lambda: find_probe(bytes(s.raw), 32) is not None, 5),
                  "i=32 local probe was not emitted")
            probe_kv, probe_name = find_probe(bytes(s.raw), 32)
            probe_shm = "/dev/shm" + probe_name.decode("ascii")
            check(os.path.exists(probe_shm), f"probe shm {probe_shm} missing before reply")
            # Simulate remote terminal replying ENOENT without unlinking
            s.send(b"\x1b_Gi=32;ENOENT:no such file\x1b\\")
            check(s.until(lambda: not os.path.exists(probe_shm), 3),
                  f"probe shm {probe_shm} was not unlinked immediately on ENOENT")
            check(s.until(lambda: len(extract_placements(bytes(s.raw))) >= 1, 10),
                  "no SVG placement appeared")
            kv, payload, wire = extract_placements(bytes(s.raw))[-1]
            w, h = int(kv["s"]), int(kv["v"])
            check(kv.get("o") == "z", f"expected o=z on SVG, got kv={kv}")
            check(kv.get("t", "d") == "d", f"expected t=d on remote kitty, got kv={kv}")
            decomp = zlib.decompress(payload)
            check(len(decomp) == w * h * 4,
                  f"decompressed SVG {len(decomp)} != {w*h*4}")
            raw_wire = ((w * h * 4 + 2) // 3) * 4
            ratio = raw_wire / max(1, wire)
            print(f"  remote zlib SVG: {w}x{h}, wire={wire} B vs raw_b64={raw_wire} B ({ratio:.1f}x)")
            check(ratio >= 5.0, f"expected >=5x compression on SVG, got {ratio:.1f}x")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            pid = s.pid
            s.close()
        check(len(shm_cm_objects(pid)) == 0, f"shm leak: {shm_cm_objects(pid)}")

        # 3b. PDF (cmedit-slides.pdf) over remote kitty (o=z)
        if os.path.exists(slides):
            s = RawSession(home, [slides], rows=ROWS, cols=COLS)
            try:
                s.pump(0.2)
                s.send(b"\x1b_Gi=31;OK\x1b\\" + b"\x1b_Gi=33;OK\x1b\\"
                       + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
                check(s.until(lambda: find_probe(bytes(s.raw), 32) is not None, 5),
                      "i=32 probe missing on PDF")
                s.send(b"\x1b_Gi=32;ENOENT:remote\x1b\\")
                check(s.until(lambda: len(extract_placements(bytes(s.raw))) >= 1, 30),
                      "no PDF placement appeared")
                kv, payload, wire = extract_placements(bytes(s.raw))[-1]
                w, h = int(kv["s"]), int(kv["v"])
                check(kv.get("o") == "z", f"expected o=z on PDF page 1, got kv={kv}")
                decomp = zlib.decompress(payload)
                check(len(decomp) == w * h * 4,
                      f"decompressed PDF page 1 {len(decomp)} != {w*h*4}")
                raw_wire = ((w * h * 4 + 2) // 3) * 4
                ratio = raw_wire / max(1, wire)
                print(f"  remote zlib PDF p1: {w}x{h}, wire={wire} B vs raw_b64={raw_wire} B ({ratio:.1f}x)")
                check(ratio >= 10.0, f"expected >=10x compression on PDF slide, got {ratio:.1f}x")
                # Page down once and verify page 2 also decompresses cleanly
                mark = len(s.raw)
                s.send(PGDN)
                check(s.until(lambda: len(extract_placements(bytes(s.raw[mark:]))) >= 1, 30),
                      "no PDF page 2 placement")
                kv2, payload2, wire2 = extract_placements(bytes(s.raw[mark:]))[-1]
                w2, h2 = int(kv2["s"]), int(kv2["v"])
                check(kv2.get("o") == "z", f"expected o=z on PDF page 2, got kv={kv2}")
                check(len(zlib.decompress(payload2)) == w2 * h2 * 4,
                      "PDF page 2 zlib decode length mismatch")
                print(f"  remote zlib PDF p2: {w2}x{h2}, wire={wire2} B")
                s.send(CTRL_Q)
                check(s.wait_exit(5), "did not quit")
            finally:
                pid = s.pid
                s.close()
            check(len(shm_cm_objects(pid)) == 0, f"shm leak: {shm_cm_objects(pid)}")

        # 3c. Noisy incompressible PNG must bail out to raw t=d (no o=z)
        noisy = make_noisy_png(os.path.join(home, "noise.png"), 200, 200)
        s = RawSession(home, [noisy], rows=ROWS, cols=COLS)
        try:
            s.pump(0.2)
            s.send(b"\x1b_Gi=31;OK\x1b\\" + b"\x1b_Gi=33;OK\x1b\\"
                   + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
            check(s.until(lambda: find_probe(bytes(s.raw), 32) is not None, 5),
                  "i=32 probe missing on noisy PNG")
            s.send(b"\x1b_Gi=32;ENOENT:remote\x1b\\")
            check(s.until(lambda: len(extract_placements(bytes(s.raw))) >= 1, 15),
                  "no noisy PNG placement appeared")
            kv, payload, wire = extract_placements(bytes(s.raw))[-1]
            w, h = int(kv["s"]), int(kv["v"])
            check("o" not in kv,
                  f"noisy incompressible image should bail out to raw, got o={kv.get('o')}")
            check(len(payload) == w * h * 4,
                  f"noisy PNG raw payload {len(payload)} != {w*h*4}")
            print(f"  remote zlib noisy PNG: {w}x{h} bailed out cleanly to raw ({wire} B)")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            pid = s.pid
            s.close()
        check(len(shm_cm_objects(pid)) == 0, f"shm leak: {shm_cm_objects(pid)}")
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_local_shm():
    home = tempfile.mkdtemp(prefix="cmedit-gfx-shm-")
    try:
        svg_path = write(home, "icon.svg", ICON)
        s = RawSession(home, [svg_path], rows=ROWS, cols=COLS)
        try:
            s.pump(0.2)
            s.send(b"\x1b_Gi=31;OK\x1b\\" + b"\x1b_Gi=33;OK\x1b\\"
                   + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
            check(s.until(lambda: find_probe(bytes(s.raw), 32) is not None, 5),
                  "i=32 probe was not emitted")
            probe_kv, probe_name = find_probe(bytes(s.raw), 32)
            check(probe_kv.get("t") == "s", f"expected t=s probe on Linux, got {probe_kv}")
            probe_shm = "/dev/shm" + probe_name.decode("ascii")
            check(os.path.exists(probe_shm), f"probe shm {probe_shm} missing")
            with open(probe_shm, "rb") as f:
                pbytes = f.read()
            check(len(pbytes) == 4, f"probe shm size {len(pbytes)} != 4")
            # Act like a real local kitty terminal: unlink the probe shm and reply OK
            os.unlink(probe_shm)
            s.send(b"\x1b_Gi=32;OK\x1b\\")

            # Wait for the SVG placement
            check(s.until(lambda: len(extract_placements(bytes(s.raw))) >= 1, 10),
                  "no t=s placement appeared")
            # A placement may have raced before i=32;OK; zoom once so we definitely
            # observe a post-probe t=s placement too.
            mark = len(s.raw)
            s.send(b"\x1b[<64;%d;%dM" % (COLS // 2, ROWS // 2))
            check(s.until(lambda: len(extract_placements(bytes(s.raw[mark:]))) >= 1, 10),
                  "no zoomed t=s placement appeared")
            kv, payload, wire = extract_placements(bytes(s.raw[mark:]))[-1]
            w, h = int(kv["s"]), int(kv["v"])
            check(kv.get("t") == "s", f"expected t=s placement, got kv={kv}")
            check("o" not in kv, f"t=s must not set o=z, got kv={kv}")
            check(wire < 128, f"t=s placement wire size {wire} >= 128 B")
            shm_name = payload.decode("ascii")
            shm_path = "/dev/shm" + shm_name
            check(os.path.exists(shm_path), f"placement shm {shm_path} does not exist")
            with open(shm_path, "rb") as f:
                pixels = f.read()
            check(len(pixels) == w * h * 4,
                  f"shm pixel size {len(pixels)} != {w*h*4}")
            print(f"  local shm SVG: {w}x{h}, wire={wire} B, shm={shm_name} ({len(pixels)} B)")

            # Intentionally DO NOT unlink shm_path here: verify cmedit unlinks
            # unconsumed shm objects on clean quit!
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            pid = s.pid
            s.close()
        leftovers = shm_cm_objects(pid)
        check(len(leftovers) == 0, f"unread shm objects leaked after exit: {leftovers}")
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_stats_on_exit():
    home = tempfile.mkdtemp(prefix="cmedit-gfx-stats-")
    try:
        svg_path = write(home, "icon.svg", ICON)
        s = RawSession(home, ["--stats-on-exit", svg_path], rows=ROWS, cols=COLS)
        try:
            s.pump(0.2)
            s.send(b"\x1b_Gi=31;OK\x1b\\" + b"\x1b_Gi=33;OK\x1b\\"
                   + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
            check(s.until(lambda: find_probe(bytes(s.raw), 32) is not None, 5),
                  "i=32 probe missing")
            s.send(b"\x1b_Gi=32;ENOENT:remote\x1b\\")
            check(s.until(lambda: len(extract_placements(bytes(s.raw))) >= 1, 10),
                  "no placement appeared")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
            s.pump(0.3)
            raw_txt = bytes(s.raw).decode("utf-8", errors="replace")
            check("graphics placements" in raw_txt and "kitty zlib" in raw_txt,
                  f"--stats-on-exit did not report kitty zlib graphics stats:\n{raw_txt[-500:]}")
            for line in raw_txt.splitlines():
                if "graphics placements" in line:
                    print("  stats:", line.strip())
                    break
        finally:
            s.close()
    finally:
        shutil.rmtree(home, ignore_errors=True)


def make_anim_gif(path, w=64, h=64, n_frames=2):
    """Write a minimal valid animated GIF89a with n_frames alternating solid
    frames (compresses strongly under o=z and tests both a=f and a=t paths)."""
    # 2-color global color table: index 0 = red, index 1 = blue
    hdr = b"GIF89a" + struct.pack("<HHBBB", w, h, 0x80, 0, 0)
    gct = bytes([255, 0, 0, 0, 0, 255])
    # NETSCAPE2.0 loop forever extension
    netscape = b"\x21\xff\x0bNETSCAPE2.0\x03\x01\x00\x00\x00"

    def lzw_solid(color_idx, npix):
        # min_code_size = 2 -> clear=4, eoi=5, codes are 3-bit initially.
        # Emit clear code every 3 pixels so code width never grows past 3 bits!
        bits = 0
        nbits = 0
        out = bytearray()

        def put(code):
            nonlocal bits, nbits
            bits |= code << nbits
            nbits += 3
            while nbits >= 8:
                out.append(bits & 0xFF)
                bits >>= 8
                nbits -= 8

        put(4)  # clear
        since_clear = 0
        for _ in range(npix):
            put(color_idx)
            since_clear += 1
            if since_clear == 3:
                put(4)
                since_clear = 0
        put(5)  # EOI
        if nbits > 0:
            out.append(bits & 0xFF)
        # Pack into GIF sub-blocks (<= 255 bytes each)
        sub = bytearray([2])  # LZW minimum code size
        pos = 0
        while pos < len(out):
            blk = out[pos:pos + 255]
            sub.append(len(blk))
            sub.extend(blk)
            pos += len(blk)
        sub.append(0)  # block terminator
        return bytes(sub)

    def frame(color_idx):
        gce = b"\x21\xf9\x04\x04" + struct.pack("<H", 10) + b"\x00\x00"
        desc = b"\x2c" + struct.pack("<HHHHB", 0, 0, w, h, 0)
        return gce + desc + lzw_solid(color_idx, w * h)

    data = hdr + gct + netscape + b"".join(frame(i & 1) for i in range(n_frames)) + b"\x3b"
    with open(path, "wb") as f:
        f.write(data)
    return path


def scenario_animated_gif():
    home = tempfile.mkdtemp(prefix="cmedit-gfx-gif-")
    try:
        gif_path = make_anim_gif(os.path.join(home, "anim.gif"), 128, 128)

        # 6a. Native kitty animation (a=T + a=f) over t=s shm
        s = RawSession(home, [gif_path], rows=ROWS, cols=COLS)
        try:
            s.pump(0.2)
            # Send XTVERSION kitty so edGfxKittyAnim is enabled (a=f path)
            s.send(b"\x1bP>|kitty(0.35.0)\x1b\\"
                   + b"\x1b_Gi=31;OK\x1b\\" + b"\x1b_Gi=33;OK\x1b\\"
                   + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
            check(s.until(lambda: find_probe(bytes(s.raw), 32) is not None, 5),
                  "i=32 probe missing on GIF (shm)")
            _, pname = find_probe(bytes(s.raw), 32)
            try:
                os.unlink("/dev/shm" + pname.decode("ascii"))
            except OSError:
                pass
            s.send(b"\x1b_Gi=32;OK\x1b\\")
            check(s.until(lambda: b"a=a,i=1,q=2,s=3,v=1" in bytes(s.raw), 10),
                  "kitty native animation (a=a s=3,v=1) was not emitted")
            # Open and close File menu so the animation re-places AFTER i=32;OK is processed.
            s.send(b"\x1bf")
            s.pump(0.2)
            mark = len(s.raw)
            s.send(b"\x1b")
            check(s.until(lambda: b"a=a,i=1,q=2,s=3,v=1" in bytes(s.raw[mark:]), 10),
                  "kitty native animation did not re-place after closing menu")
            raw_b = bytes(s.raw[mark:])
            af_frames = [parse_kv(m.group(1)) for m in APC_RE.finditer(raw_b)
                         if parse_kv(m.group(1)).get("a") == "f"]
            check(len(af_frames) >= 1, "no a=f animation continuation frames emitted")
            check(af_frames[0].get("t") == "s", f"expected a=f over t=s shm, got {af_frames[0]}")
            print(f"  GIF native anim (kitty t=s): {len(af_frames)} a=f frame(s), kv={af_frames[0]}")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            pid = s.pid
            s.close()
        check(len(shm_cm_objects(pid)) == 0, f"shm leak after GIF anim: {shm_cm_objects(pid)}")

        # 6b. Client-driven animation (a=t + a=p) over remote o=z
        s = RawSession(home, [gif_path], rows=ROWS, cols=COLS)
        try:
            s.pump(0.2)
            # No kitty XTVERSION -> client-driven animation (a=t + a=p)
            s.send(b"\x1b_Gi=31;OK\x1b\\" + b"\x1b_Gi=33;OK\x1b\\"
                   + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
            check(s.until(lambda: find_probe(bytes(s.raw), 32) is not None, 5),
                  "i=32 probe missing on GIF (zlib)")
            s.send(b"\x1b_Gi=32;ENOENT:remote\x1b\\")
            check(s.until(lambda: b"a=p,q=2,i=" in bytes(s.raw), 10),
                  "client-driven animation (a=p) was not emitted")
            raw_b = bytes(s.raw)
            at_frames = []
            for m in APC_RE.finditer(raw_b):
                kv = parse_kv(m.group(1))
                if kv.get("a") == "t":
                    at_frames.append((kv, base64.b64decode(m.group(2)) if m.group(2) else b""))
            check(len(at_frames) >= 2, f"expected >=2 a=t frames, got {len(at_frames)}")
            for kv, data in at_frames:
                w, h = int(kv["s"]), int(kv["v"])
                if kv.get("o") == "z":
                    dec = zlib.decompress(data)
                    check(len(dec) == w * h * 4, f"a=t o=z decode mismatch {len(dec)} != {w*h*4}")
            print(f"  GIF client-driven anim (remote o=z): {len(at_frames)} a=t frames verified")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            pid = s.pid
            s.close()
        check(len(shm_cm_objects(pid)) == 0, f"shm leak after client GIF anim: {shm_cm_objects(pid)}")
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_sigterm_cleanup():
    home = tempfile.mkdtemp(prefix="cmedit-gfx-term-")
    try:
        svg_path = write(home, "icon.svg", ICON)
        s = RawSession(home, [svg_path], rows=ROWS, cols=COLS)
        try:
            s.pump(0.2)
            s.send(b"\x1b_Gi=31;OK\x1b\\" + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
            check(s.until(lambda: find_probe(bytes(s.raw), 32) is not None, 5),
                  "i=32 probe missing")
            _, pname = find_probe(bytes(s.raw), 32)
            try:
                os.unlink("/dev/shm" + pname.decode("ascii"))
            except OSError:
                pass
            s.send(b"\x1b_Gi=32;OK\x1b\\")
            # Force a t=s placement via zoom
            mark = len(s.raw)
            s.send(b"\x1b[<64;%d;%dM" % (COLS // 2, ROWS // 2))
            check(s.until(lambda: len(extract_placements(bytes(s.raw[mark:]))) >= 1, 10),
                  "no t=s placement before SIGTERM")
            check(len(shm_cm_objects(s.pid)) >= 1, "expected live unread shm object before SIGTERM")
            os.kill(s.pid, signal.SIGTERM)
            check(s.wait_exit(5), "process did not exit on SIGTERM")
        finally:
            pid = s.pid
            s.close()
        leftovers = shm_cm_objects(pid)
        check(len(leftovers) == 0, f"shm objects leaked after SIGTERM: {leftovers}")
        print("  SIGTERM cleanup: unread shm objects unlinked cleanly")
    finally:
        shutil.rmtree(home, ignore_errors=True)


def scenario_large_animated_gif():
    """Regression test for >256-frame animations on local kitty: must never
    self-evict early shm frames before emission; falls back cleanly to o=z
    (when supported) or raw t=d (when o=z is absent) with all 300 frames intact."""
    home = tempfile.mkdtemp(prefix="cmedit-gfx-gif300-")
    try:
        gif_path = make_anim_gif(os.path.join(home, "anim300.gif"), 16, 16, n_frames=300)

        # 8a. Local kitty WITH o=z: 300 frames fall back cleanly from t=s to o=z
        s = RawSession(home, [gif_path], rows=ROWS, cols=COLS)
        try:
            s.pump(0.2)
            s.send(b"\x1bP>|kitty(0.35.0)\x1b\\"
                   + b"\x1b_Gi=31;OK\x1b\\" + b"\x1b_Gi=33;OK\x1b\\"
                   + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
            check(s.until(lambda: find_probe(bytes(s.raw), 32) is not None, 5),
                  "i=32 probe missing on 300-frame GIF (shm+zlib)")
            _, pname = find_probe(bytes(s.raw), 32)
            try:
                os.unlink("/dev/shm" + pname.decode("ascii"))
            except OSError:
                pass
            s.send(b"\x1b_Gi=32;OK\x1b\\")
            check(s.until(lambda: b"a=a,i=1,q=2,s=3,v=1" in bytes(s.raw), 15),
                  "300-frame GIF native animation did not appear")
            # Re-trigger placement after i=32;OK via File menu open/close
            s.send(b"\x1bf")
            s.pump(0.2)
            mark = len(s.raw)
            s.send(b"\x1b")
            check(s.until(lambda: b"a=a,i=1,q=2,s=3,v=1" in bytes(s.raw[mark:]), 15),
                  "300-frame GIF did not re-place after closing menu")
            raw_b = bytes(s.raw[mark:])
            root_frames = []
            cont_frames = []
            shm_refs = []
            for m in APC_RE.finditer(raw_b):
                kv = parse_kv(m.group(1))
                a = kv.get("a")
                if a == "T":
                    root_frames.append(kv)
                elif a == "f":
                    cont_frames.append(kv)
                if kv.get("t") == "s":
                    shm_refs.append(base64.b64decode(m.group(2)).decode("ascii"))
            check(len(root_frames) == 1, f"expected 1 a=T root frame, got {len(root_frames)}")
            check(len(cont_frames) == 299, f"expected 299 a=f frames, got {len(cont_frames)}")
            check(len(shm_refs) == 0,
                  f"300-frame GIF should avoid t=s ring overflow, got {len(shm_refs)} t=s refs")
            print(f"  300-frame GIF (local kitty + o=z): 1 a=T + {len(cont_frames)} a=f intact, 0 t=s overflow")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            pid = s.pid
            s.close()
        check(len(shm_cm_objects(pid)) == 0, f"shm leak after 300-frame GIF (zlib): {shm_cm_objects(pid)}")

        # 8b. Local kitty WITHOUT o=z: 300 frames fall back cleanly to raw t=d
        s = RawSession(home, [gif_path], rows=ROWS, cols=COLS)
        try:
            s.pump(0.2)
            s.send(b"\x1bP>|kitty(0.35.0)\x1b\\"
                   + b"\x1b_Gi=31;OK\x1b\\" + b"\x1b_Gi=33;EINVAL\x1b\\"
                   + b"\x1b[6;16;8t" + b"\x1b[?62;22c")
            check(s.until(lambda: find_probe(bytes(s.raw), 32) is not None, 5),
                  "i=32 probe missing on 300-frame GIF (shm-only)")
            _, pname = find_probe(bytes(s.raw), 32)
            try:
                os.unlink("/dev/shm" + pname.decode("ascii"))
            except OSError:
                pass
            s.send(b"\x1b_Gi=32;OK\x1b\\")
            check(s.until(lambda: b"a=a,i=1,q=2,s=3,v=1" in bytes(s.raw), 15),
                  "300-frame GIF (no o=z) did not appear")
            s.send(b"\x1bf")
            s.pump(0.2)
            mark = len(s.raw)
            s.send(b"\x1b")
            check(s.until(lambda: b"a=a,i=1,q=2,s=3,v=1" in bytes(s.raw[mark:]), 15),
                  "300-frame GIF (no o=z) did not re-place")
            raw_b = bytes(s.raw[mark:])
            root_frames = [parse_kv(m.group(1)) for m in APC_RE.finditer(raw_b)
                           if parse_kv(m.group(1)).get("a") == "T"]
            cont_frames = [parse_kv(m.group(1)) for m in APC_RE.finditer(raw_b)
                           if parse_kv(m.group(1)).get("a") == "f"]
            check(len(root_frames) == 1, f"expected 1 a=T root frame (no o=z), got {len(root_frames)}")
            check(len(cont_frames) == 299, f"expected 299 a=f frames (no o=z), got {len(cont_frames)}")
            check(all(kv.get("t", "d") == "d" and "o" not in kv for kv in root_frames + cont_frames),
                  "300-frame GIF without o=z must stream raw t=d")
            print(f"  300-frame GIF (local kitty, no o=z): 1 a=T + {len(cont_frames)} a=f raw t=d intact")
            s.send(CTRL_Q)
            check(s.wait_exit(5), "did not quit")
        finally:
            pid = s.pid
            s.close()
        check(len(shm_cm_objects(pid)) == 0, f"shm leak after 300-frame GIF (raw): {shm_cm_objects(pid)}")
    finally:
        shutil.rmtree(home, ignore_errors=True)


def main():
    scenarios = [
        ("silent terminal (no shm, no o=z)", scenario_silent),
        ("plain kitty (i=31 only, probe sweep)", scenario_plain_kitty),
        ("remote kitty (o=z deflate + noisy bailout)", scenario_remote_zlib),
        ("local kitty (t=s shm + exit cleanup)", scenario_local_shm),
        ("stats-on-exit graphics line", scenario_stats_on_exit),
        ("animated GIF (a=f shm + a=t remote o=z)", scenario_animated_gif),
        ("SIGTERM cleanup of unread shm objects", scenario_sigterm_cleanup),
        ("300-frame animated GIF (>256 shm ring fallback)", scenario_large_animated_gif),
    ]
    failed = 0
    for name, fn in scenarios:
        print(f"=== {name} ===")
        try:
            fn()
            print("  PASS")
        except Exception as e:
            print(f"  FAIL: {e}")
            failed += 1
    sys.exit(failed)


if __name__ == "__main__":
    main()
