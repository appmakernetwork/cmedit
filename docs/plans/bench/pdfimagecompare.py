#!/usr/bin/env python3
"""Second half of pdfimagecheck.sh: compare our dumps with pdfimages'.

    pdfimagecompare.py FILE.pdf TMPDIR

Prints one line per image:
    name#num type color enc  STATUS  max=… mean=…  [note]
STATUS is ok (max channel error <= 2), diff, size (dimensions differ),
missing (we produced nothing), unsupported/corrupt (our typed failure), or
noref (pdfimages wrote nothing to compare with).
"""
import os
import sys

import numpy as np
from PIL import Image

Image.MAX_IMAGE_PIXELS = None


def load_ours(path):
    with open(path, "rb") as fh:
        hdr = fh.readline().split()
        w, h = int(hdr[0]), int(hdr[1])
        data = np.frombuffer(fh.read(), dtype=np.uint8)
    return data[: w * h * 4].reshape(h, w, 4)


def main(pdf, tmp):
    name = os.path.basename(pdf)
    rows = []
    for line in open(os.path.join(tmp, "list"), errors="replace"):
        p = line.split()
        if len(p) < 11 or not p[0].isdigit():
            continue
        rows.append(p)
    status = {}
    for line in open(os.path.join(tmp, "ours.log"), errors="replace"):
        p = line.split(None, 4)
        if len(p) >= 3:
            status[(p[0], p[1])] = (p[2], p[4].strip() if len(p) > 4 else "")
    inline_k = {}
    worst = []
    for p in rows:
        page, num, typ, w, h, color, comp, bpc, enc = p[:9]
        num = int(num)
        if p[10] == "[inline]":
            k = inline_k.get(page, 0)
            inline_k[page] = k + 1
            obj = "inline-%s-%d" % (page, k)
        else:
            obj = p[10]
        kind = {"image": "img", "smask": "smask", "mask": "mask", "stencil": "stencil"}.get(typ, typ)
        ours_path = os.path.join(tmp, "ours", "%s.%s" % (obj, kind))
        tag = "%s#%d %s %s %s bpc%s" % (name, num, typ, color, enc, bpc)
        if not os.path.exists(ours_path):
            st = status.get((obj, kind)) or status.get((obj, "img")) or ("missing", "")
            print("%s  %s  %s" % (tag, st[0], st[1][:80]))
            continue
        try:
            ours = load_ours(ours_path)
        except (ValueError, IndexError) as e:
            print("%s  baddump  %s" % (tag, e))
            continue
        png = os.path.join(tmp, "p-%03d.png" % num)
        ref = None
        note = ""
        pi = None
        if ref is None and os.path.exists(png):
            try:
                pi = Image.open(png)
            except Exception:
                pi = None    # poppler wrote something unreadable
        if pi is not None:
            if typ in ("mask", "stencil"):
                a = np.asarray(pi.convert("L"))
                ref = a
            elif pi.mode in ("L", "1", "I;16", "I"):
                ref = np.asarray(pi.convert("L"))
            else:
                ref = np.asarray(pi.convert("RGB"))
        if ref is None:
            print("%s  noref" % tag)
            continue
        if ref.shape[0] != ours.shape[0] or ref.shape[1] != ours.shape[1]:
            print("%s  size  ours=%dx%d ref=%dx%d" % (tag, ours.shape[1], ours.shape[0], ref.shape[1], ref.shape[0]))
            continue
        info = (status.get((obj, kind)) or ("", ""))[1]
        if typ in ("mask", "stencil"):
            cov = ours[..., 0].astype(int)
            # pdfimages writes a stencil's raw bits, 1 as black, ignoring its
            # /Decode; with the default Decode a 0 bit paints.
            bit1 = (ref.astype(int) < 128)
            painted = bit1 if "inv=1" in info else ~bit1
            d = np.abs(cov - painted * 255)
            note = "coverage"
        elif ref.ndim == 2:
            d = np.abs(ours[..., 0].astype(int) - ref.astype(int))
        else:
            d = np.abs(ours[..., :3].astype(int) - ref.astype(int))
        mx, mean = int(d.max()), float(d.mean())
        st = "ok" if mx <= 2 else "diff"
        worst.append(mx)
        print("%s  %s  max=%d mean=%.3f %s" % (tag, st, mx, mean, note))


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
