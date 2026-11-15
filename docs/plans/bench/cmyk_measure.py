"""Measure how pdftoppm renders DeviceCMYK, and check Cmedit.PdfImage against it.

    python3 -I docs/plans/bench/cmyk_measure.py WORKDIR [src/Cmedit/PdfImage.hs]

'cmykCorners' in Cmedit.PdfImage is a measurement, and this is the instrument.
It writes PDFs of solid DeviceCMYK patches on regular lattices (every
component in steps of 1/n, so (n + 1)^4 colours a page, for each n in
CMYK_STEPS, default 28, 33 and 40), renders them with pdftoppm, which runs
without colour management, and reads the pixels back. Three results:

  1. The sixteen corners of the CMYK cube (every component 0 or 1) as the
     8-bit sRGB values pdftoppm paints.
  2. What the renderer does between the corners: the four-way linear
     interpolation of the corner colours. The script fits that model to all
     the patches by least squares and prints the largest residual, which
     stays at the half code value an 8-bit pixel can hide.
  3. The corner colours to better than 8 bits. A single patch is rounded to
     a code value, but the corners are heavily over-determined (48 unknowns
     against several million measured channels), so the fit resolves them to
     about one part in ten thousand of full scale: lattices of different
     pitch agree to that, which is the honest error bar (the formal standard
     error is smaller, but rounding is not independent noise). The table is
     the fit rounded to four decimals, with 0 and 1 where the renderer
     saturates.

With the Haskell source as a second argument, the table in it is compared
with the fit and the script exits 1 if any entry is more than 0.00015 from
the fitted value.

Needs poppler-utils (pdftoppm) and numpy. Nothing but WORKDIR is written.
"""
import os
import re
import subprocess
import sys

import numpy as np

work = sys.argv[1]
os.makedirs(work, exist_ok=True)
LATTICES = [int(v) for v in os.environ.get("CMYK_STEPS", "28,33,40").split(",")]
CELL = 2                           # points = pixels at 72 dpi


def measure(steps):
    """Render the lattice with the given pitch; return (weights, observed rgb, 8-bit corners)."""
    n = steps + 1
    side = n * n                   # patches per row and per column: (c, m) across, (y, k) down
    parts = []
    for ci in range(n):
        for mi in range(n):
            x = (ci * n + mi) * CELL
            for yi in range(n):
                for ki in range(n):
                    yy = (side - 1 - (yi * n + ki)) * CELL
                    parts.append("%.6g %.6g %.6g %.6g k %d %d %d %d re f"
                                 % (ci / steps, mi / steps, yi / steps, ki / steps, x, yy, CELL, CELL))
    content = "\n".join(parts).encode()
    objs = [
        b"<< /Type /Catalog /Pages 2 0 R >>",
        b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
        ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 %d %d] /Contents 4 0 R /Resources << >> >>"
         % (side * CELL, side * CELL)).encode(),
        b"<< /Length %d >>\nstream\n" % len(content) + content + b"\nendstream",
    ]
    pdf = b"%PDF-1.4\n"
    offsets = []
    for num, body in enumerate(objs, 1):
        offsets.append(len(pdf))
        pdf += b"%d 0 obj\n" % num + body + b"\nendobj\n"
    xref = len(pdf)
    pdf += b"xref\n0 %d\n0000000000 65535 f \n" % (len(objs) + 1)
    for off in offsets:
        pdf += b"%010d 00000 n \n" % off
    pdf += b"trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n" % (len(objs) + 1, xref)
    stem = os.path.join(work, "cmyk_patches_%d" % steps)
    open(stem + ".pdf", "wb").write(pdf)
    subprocess.run(["pdftoppm", "-r", "72", "-singlefile", stem + ".pdf", stem], check=True)
    data = open(stem + ".ppm", "rb").read()
    hdr = re.match(rb"P6\s+(\d+)\s+(\d+)\s+255\s", data)
    w, h = int(hdr.group(1)), int(hdr.group(2))
    assert w == side * CELL and h == side * CELL, (w, h)
    img = np.frombuffer(data, dtype=np.uint8, offset=hdr.end()).reshape(h, w, 3)
    # every patch is flat: its pixels agree
    assert (img[0::2, 0::2] == img[1::2, 1::2]).all(), "patches are not flat; is the page pixel-aligned?"
    patch = img[0::CELL, 0::CELL].astype(np.float64)              # [row = (y, k)][col = (c, m)]
    rgb = patch.reshape(n, n, n, n, 3).transpose(2, 3, 0, 1, 4)   # [c][m][y][k]
    corners = [tuple(int(v) for v in rgb[c * steps, m * steps, y * steps, k * steps])
               for c in (0, 1) for m in (0, 1) for y in (0, 1) for k in (0, 1)]
    t = np.arange(n) / steps
    axis = np.stack([1 - t, t], axis=1)                           # [value][corner bit]
    wgt = np.einsum("ca,mb,yd,ke->cmykabde", axis, axis, axis, axis).reshape(n ** 4, 16)
    return wgt, rgb.reshape(n ** 4, 3), corners


pages = [measure(s) for s in LATTICES]
corners8 = pages[0][2]
assert all(p[2] == corners8 for p in pages)
# Least squares over every lattice: rgb = sum over the 16 corners of weight(c, m, y, k) * corner.
wgt = np.concatenate([p[0] for p in pages])
obs = np.concatenate([p[1] for p in pages])
fit, _res, _rank, _sv = np.linalg.lstsq(wgt, obs, rcond=None)
worst = float(np.abs(obs - wgt @ fit).max())
# How far the lattices disagree with each other, corner by corner.
single = [np.linalg.lstsq(p[0], p[1], rcond=None)[0] for p in pages]
spread = float(max(np.abs(a - b).max() for a in single for b in single)) / 255.0

print("%d patches on lattices of pitch 1/%s; corner colours, index c*8 + m*4 + y*2 + k:"
      % (len(obs), ", 1/".join(str(s) for s in LATTICES)))
print("  cmyk   8-bit rendering    fitted, as a fraction of full scale    to four decimals")
table = []
for i in range(16):
    frac = fit[i] / 255.0
    four = [min(1.0, max(0.0, round(float(v), 4))) for v in frac]
    table += four
    print("  %d%d%d%d   %3d %3d %3d      %9.6f %9.6f %9.6f          %.4f  %.4f  %.4f"
          % ((i >> 3) & 1, (i >> 2) & 1, (i >> 1) & 1, i & 1, *corners8[i], *frac, *four))
print("four-way linear interpolation of the fitted corners: largest residual %.3f of 255" % worst)
print("lattices agree on every corner to within %.6f of full scale" % spread)

status = 0
if worst > 0.75:
    print("the renderer is NOT a multilinear blend of its corners")
    status = 1
if len(sys.argv) > 2:
    hs = open(sys.argv[2], encoding="utf-8").read()
    body = hs[hs.index("cmykCorners ="):]
    body = body[:body.index("\n\n")]
    have = [float(v) for v in re.findall(r"\d+\.\d+|\d+", body[body.index("["):])]
    assert len(have) == 48, len(have)
    bad = 0
    for i in range(16):
        for ch in range(3):
            want = fit[i][ch] / 255.0
            if abs(have[i * 3 + ch] - want) > 0.00015:
                print("corner %d channel %d: source %.4f, measured %.6f" % (i, ch, have[i * 3 + ch], want))
                bad += 1
    print("cmykCorners in the source %s the measurement" % ("matches" if not bad else "DIFFERS from"))
    status = status or (1 if bad else 0)
sys.exit(status)
