#!/usr/bin/env python3
"""Synthetic PDFs exercising Cmedit.PdfImage (plan 0034 workstream C).

    python3 -I docs/plans/bench/gen_pdfimages.py OUT_DIR

Writes one small PDF per feature (bit depths, Decode arrays, Indexed with
string and stream palettes, Separation/DeviceN through every function type,
Lab, CalGray/CalRGB, ICCBased-by-alternate, colour-key, stencil and soft
masks with Matte, image masks, inline images with abbreviations, LZW with
EarlyChange 0, CMYK JPEGs with and without the Adobe YCCK transform). Every
image is drawn on its page, so `pdfimages -list` sees it; pdfimagecheck.sh
compares our decode of each against poppler's.

Needs only the standard library, plus Pillow for the JPEG fixtures (skipped
without it).
"""
import io
import math
import os
import struct
import sys
import zlib

W, H = 61, 37          # deliberately odd: row padding matters


def pattern(x, y, c):
    """A smooth-plus-noise test pattern in [0, 1]."""
    v = 0.5 + 0.5 * math.sin(0.21 * x + 0.13 * y + 1.7 * c) * math.cos(0.07 * x * (c + 1) - 0.11 * y)
    return min(1.0, max(0.0, v))


def pack(samples, bpc, w, h, n):
    """Pack integer samples row by row, each row padded to a byte."""
    out = bytearray()
    for y in range(h):
        acc = 0
        nbits = 0
        for x in range(w):
            for c in range(n):
                v = samples[(y * w + x) * n + c]
                if bpc == 16:
                    out += struct.pack(">H", v)
                    continue
                acc = (acc << bpc) | v
                nbits += bpc
                while nbits >= 8:
                    nbits -= 8
                    out.append((acc >> nbits) & 0xFF)
        if nbits:
            out.append((acc << (8 - nbits)) & 0xFF)
            acc = 0
            nbits = 0
    return bytes(out)


def samples_for(n, bpc, w=W, h=H, levels=None):
    m = (1 << bpc) - 1 if levels is None else levels
    return [int(round(pattern(x, y, c) * m)) for y in range(h) for x in range(w) for c in range(n)]


def lzw_encode(data, early=1):
    """PDF LZW with the given /EarlyChange.

    The decoder adds its table entry one code later than the encoder, so the
    width it reads the next code at is decided by the entry *before* the one
    the encoder just added: hence `nxt - 1` below."""
    out = bytearray()
    acc = [0, 0]  # accumulator, bit count

    def emit(code, width):
        acc[0] = (acc[0] << width) | code
        acc[1] += width
        while acc[1] >= 8:
            acc[1] -= 8
            out.append((acc[0] >> acc[1]) & 0xFF)

    def reset():
        return {bytes([i]): i for i in range(256)}, 258, 9

    table, nxt, width = reset()
    emit(256, width)
    w = b""
    for ch in data:
        wc = w + bytes([ch])
        if wc in table:
            w = wc
            continue
        emit(table[w], width)
        table[wc] = nxt
        nxt += 1
        if nxt - 1 + early >= 4095:
            emit(256, width)
            table, nxt, width = reset()
        elif nxt - 1 + early >= (1 << width):
            width += 1
        w = bytes([ch])
    if w:
        emit(table[w], width)
        nxt += 1
        if nxt - 1 + early >= (1 << width) and width < 12:
            width += 1
    emit(257, width)
    if acc[1]:
        out.append((acc[0] << (8 - acc[1])) & 0xFF)
    return bytes(out)


class Pdf:
    def __init__(self):
        self.objs = []

    def add(self, body):
        self.objs.append(body)
        return len(self.objs)

    def stream(self, dict_src, data):
        return self.add(b"<<" + dict_src.encode() + b" /Length %d>>\nstream\n" % len(data) + data + b"\nendstream")

    def write(self, path, images, extra_content=b""):
        """images: list of (name, objnum). One page drawing each in a grid."""
        cells = []
        xo = []
        for i, (name, num) in enumerate(images):
            x = 10 + (i % 4) * 140
            y = 650 - (i // 4) * 110
            cells.append(b"q 120 0 0 90 %d %d cm /%s Do Q\n" % (x, y, name.encode()))
            xo.append(b"/%s %d 0 R" % (name.encode(), num))
        content = b"".join(cells) + extra_content
        cnum = self.stream("", content)
        res = b"<< /XObject << " + b" ".join(xo) + b" >> >>"
        pages = len(self.objs) + 2
        page = self.add(b"<< /Type /Page /Parent %d 0 R /MediaBox [0 0 600 800] /Resources %s /Contents %d 0 R >>"
                        % (pages, res, cnum))
        self.add(b"<< /Type /Pages /Kids [%d 0 R] /Count 1 >>" % page)
        cat = self.add(b"<< /Type /Catalog /Pages %d 0 R >>" % pages)
        buf = bytearray(b"%PDF-1.7\n%\xe2\xe3\xcf\xd3\n")
        offs = []
        for i, body in enumerate(self.objs):
            offs.append(len(buf))
            buf += b"%d 0 obj\n" % (i + 1) + body + b"\nendobj\n"
        xref = len(buf)
        buf += b"xref\n0 %d\n0000000000 65535 f \n" % (len(self.objs) + 1)
        for o in offs:
            buf += b"%010d 00000 n \n" % o
        buf += b"trailer\n<< /Size %d /Root %d 0 R >>\nstartxref\n%d\n%%%%EOF\n" % (len(self.objs) + 1, cat, xref)
        with open(path, "wb") as fh:
            fh.write(buf)


def image(p, cs, bpc, data, extra="", w=W, h=H, flt="/FlateDecode", compress=True):
    body = zlib.compress(data) if compress else data
    f = (" /Filter %s" % flt) if flt else ""
    return p.stream("/Type /XObject /Subtype /Image /Width %d /Height %d /ColorSpace %s /BitsPerComponent %d%s%s"
                    % (w, h, cs, bpc, f, extra), body)


def main(out):
    os.makedirs(out, exist_ok=True)

    # Bit depths and Decode arrays, gray and RGB.
    p = Pdf()
    ims = []
    for bpc in (1, 2, 4, 8, 16):
        ims.append(("G%d" % bpc, image(p, "/DeviceGray", bpc, pack(samples_for(1, bpc), bpc, W, H, 1))))
    ims.append(("GD", image(p, "/DeviceGray", 4, pack(samples_for(1, 4), 4, W, H, 1), " /Decode [1 0]")))
    ims.append(("GD2", image(p, "/DeviceGray", 8, pack(samples_for(1, 8), 8, W, H, 1), " /Decode [0.2 0.7]")))
    for bpc in (4, 16):
        ims.append(("R%d" % bpc, image(p, "/DeviceRGB", bpc, pack(samples_for(3, bpc), bpc, W, H, 3))))
    ims.append(("RD", image(p, "/DeviceRGB", 8, pack(samples_for(3, 8), 8, W, H, 3), " /Decode [1 0 0 1 1 0]")))
    p.write(os.path.join(out, "g_depths.pdf"), ims)

    # CMYK raw at 8 and 2 bits, with an inverting Decode.
    p = Pdf()
    ims = [("K8", image(p, "/DeviceCMYK", 8, pack(samples_for(4, 8), 8, W, H, 4))),
           ("K2", image(p, "/DeviceCMYK", 2, pack(samples_for(4, 2), 2, W, H, 4))),
           ("KD", image(p, "/DeviceCMYK", 8, pack(samples_for(4, 8), 8, W, H, 4), " /Decode [1 0 1 0 1 0 1 0]"))]
    p.write(os.path.join(out, "g_cmyk.pdf"), ims)

    # Indexed: hex-string palette, stream palette, CMYK base, Decode.
    p = Pdf()
    pal = bytes(int(255 * pattern(i, 3, c)) for i in range(16) for c in range(3))
    palk = bytes(int(255 * pattern(i, 5, c)) for i in range(256) for c in range(4))
    pstream = p.stream("", palk)
    idx4 = pack(samples_for(1, 4), 4, W, H, 1)
    idx8 = pack([v % 256 for v in samples_for(1, 8)], 8, W, H, 1)
    ims = [("I4", image(p, "[/Indexed /DeviceRGB 15 <%s>]" % pal.hex(), 4, idx4)),
           ("I8", image(p, "[/Indexed /DeviceCMYK 255 %d 0 R]" % pstream, 8, idx8)),
           ("ID", image(p, "[/Indexed /DeviceRGB 15 <%s>]" % pal.hex(), 4, idx4, " /Decode [15 0]")),
           ("I1", image(p, "[/Indexed /DeviceRGB 1 <ff0000 0000ff>]", 1, pack(samples_for(1, 1), 1, W, H, 1)))]
    p.write(os.path.join(out, "g_indexed.pdf"), ims)

    # Separation / DeviceN through function types 0, 2, 3, 4.
    p = Pdf()
    f2 = p.add(b"<< /FunctionType 2 /Domain [0 1] /C0 [0 0 0 0] /C1 [0.1 0.8 0 0.2] /N 1 >>")
    f2g = p.add(b"<< /FunctionType 2 /Domain [0 1] /C0 [1 1 1] /C1 [0.2 0.3 0.9] /N 2.2 >>")
    ps = b"{ dup 0.5 gt { 1 exch sub 0 exch } { 0.3 mul 1 } ifelse 0.25 }"
    f4 = p.stream("/FunctionType 4 /Domain [0 1] /Range [0 1 0 1 0 1]", ps)
    samp = bytes(int(255 * pattern(i, 0, c)) for i in range(8) for c in range(3))
    f0 = p.stream("/FunctionType 0 /Domain [0 1] /Range [0 1 0 1 0 1] /Size [8] /BitsPerSample 8", samp)
    f3 = p.add(b"<< /FunctionType 3 /Domain [0 1] /Functions [%d 0 R %d 0 R] /Bounds [0.4] /Encode [0 1 1 0] >>"
               % (f2g, f0))
    psn = b"{ 2 copy add 0.5 mul 3 1 roll exch 0 }"
    fn = p.stream("/FunctionType 4 /Domain [0 1 0 1] /Range [0 1 0 1 0 1 0 1]", psn)
    s8 = pack(samples_for(1, 8), 8, W, H, 1)
    ims = [("S2", image(p, "[/Separation /Spot /DeviceCMYK %d 0 R]" % f2, 8, s8)),
           ("S4", image(p, "[/Separation /Spot2 /DeviceRGB %d 0 R]" % f4, 8, s8)),
           ("S0", image(p, "[/Separation /Spot3 /DeviceRGB %d 0 R]" % f0, 8, s8)),
           ("S3", image(p, "[/Separation /Spot4 /DeviceRGB %d 0 R]" % f3, 8, s8)),
           ("SA", image(p, "[/Separation /All /DeviceCMYK %d 0 R]" % f2, 8, s8)),
           ("DN", image(p, "[/DeviceN [/A /B] /DeviceCMYK %d 0 R]" % fn, 8, pack(samples_for(2, 8), 8, W, H, 2)))]
    p.write(os.path.join(out, "g_tint.pdf"), ims)

    # Calibrated spaces and ICCBased by alternate (the "profile" is junk on
    # purpose: neither reader interprets it).
    p = Pdf()
    icc3 = p.stream("/N 3 /Alternate /DeviceRGB", b"not a profile")
    icc1 = p.stream("/N 1", b"not a profile")
    ims = [("LAB", image(p, "[/Lab << /WhitePoint [0.9642 1 0.8249] /Range [-100 100 -100 100] >>]", 8,
                         pack(samples_for(3, 8), 8, W, H, 3))),
           ("LAB65", image(p, "[/Lab << /WhitePoint [0.9505 1 1.089] >>]", 8,
                           pack(samples_for(3, 8), 8, W, H, 3), " /Decode [0 100 -60 60 -80 80]")),
           ("CG", image(p, "[/CalGray << /WhitePoint [0.9505 1 1.089] /Gamma 2.2 >>]", 8,
                        pack(samples_for(1, 8), 8, W, H, 1))),
           ("CG1", image(p, "[/CalGray << /WhitePoint [0.9642 1 0.8249] >>]", 8,
                         pack(samples_for(1, 8), 8, W, H, 1))),
           ("CR1", image(p, "[/CalRGB << /WhitePoint [0.9642 1 0.8249] >>]", 8,
                         pack(samples_for(3, 8), 8, W, H, 3))),
           ("CR", image(p, "[/CalRGB << /WhitePoint [0.9505 1 1.089] /Gamma [2.2 2.2 2.2] "
                           "/Matrix [0.4124 0.2126 0.0193 0.3576 0.7152 0.1192 0.1805 0.0722 0.9505] >>]", 8,
                        pack(samples_for(3, 8), 8, W, H, 3))),
           ("ICC3", image(p, "[/ICCBased %d 0 R]" % icc3, 8, pack(samples_for(3, 8), 8, W, H, 3))),
           ("ICC1", image(p, "[/ICCBased %d 0 R]" % icc1, 8, pack(samples_for(1, 8), 8, W, H, 1)))]
    p.write(os.path.join(out, "g_cal.pdf"), ims)

    # Masks: colour key, explicit stencil (other size), soft mask + Matte,
    # image masks both ways.
    p = Pdf()
    rgb = samples_for(3, 8)
    stencil = p.stream("/Type /XObject /Subtype /Image /Width 122 /Height 74 /ImageMask true "
                       "/BitsPerComponent 1 /Filter /FlateDecode",
                       zlib.compress(pack([1 if (x // 9 + y // 7) % 2 else 0 for y in range(74) for x in range(122)],
                                          1, 122, 74, 1)))
    alpha = [int(round(255 * x / (W - 1))) for y in range(H) for x in range(W)]
    sm = p.stream("/Type /XObject /Subtype /Image /Width %d /Height %d /ColorSpace /DeviceGray "
                  "/BitsPerComponent 8 /Matte [0.5 0.5 0.5] /Filter /FlateDecode" % (W, H),
                  zlib.compress(pack(alpha, 8, W, H, 1)))
    # Premultiply against the matte so un-premultiplying recovers the colour.
    pre = []
    for i, v in enumerate(rgb):
        a = alpha[i // 3] / 255.0
        pre.append(int(round(127.5 + (v - 127.5) * a)))
    ims = [("CK", image(p, "/DeviceRGB", 8, pack(rgb, 8, W, H, 3), " /Mask [0 120 0 255 0 255]")),
           ("XM", image(p, "/DeviceRGB", 8, pack(rgb, 8, W, H, 3), " /Mask %d 0 R" % stencil)),
           ("SM", image(p, "/DeviceRGB", 8, pack(pre, 8, W, H, 3), " /SMask %d 0 R" % sm)),
           ("IM0", p.stream("/Type /XObject /Subtype /Image /Width %d /Height %d /ImageMask true "
                            "/Filter /FlateDecode" % (W, H), zlib.compress(pack(samples_for(1, 1), 1, W, H, 1)))),
           ("IM1", p.stream("/Type /XObject /Subtype /Image /Width %d /Height %d /ImageMask true /Decode [1 0] "
                            "/BitsPerComponent 1 /Filter /FlateDecode" % (W, H),
                            zlib.compress(pack(samples_for(1, 1), 1, W, H, 1))))]
    p.write(os.path.join(out, "g_masks.pdf"), ims)

    # Filters: LZW (both EarlyChange), ASCII85+Flate, AHx, RL, PNG predictor.
    p = Pdf()
    raw = pack(rgb, 8, W, H, 3)
    # PNG predictor rows: Sub filter (type 1) on each row.
    rows = bytearray()
    for i in range(H):
        r = raw[i * W * 3:(i + 1) * W * 3]
        rows += b"\x01" + bytes((r[k] - (r[k - 3] if k >= 3 else 0)) & 0xFF for k in range(len(r)))
    rows = bytes(rows)
    import base64
    ims = [("LZ1", image(p, "/DeviceRGB", 8, lzw_encode(raw, 1), flt="/LZWDecode", compress=False)),
           ("LZ0", image(p, "/DeviceRGB", 8, lzw_encode(raw, 0),
                         " /DecodeParms << /EarlyChange 0 >>", flt="/LZWDecode", compress=False)),
           ("A85", image(p, "/DeviceRGB", 8, base64.a85encode(zlib.compress(raw)) + b"~>",
                         flt="[/ASCII85Decode /FlateDecode]", compress=False)),
           ("AHX", image(p, "/DeviceRGB", 8, raw.hex().encode() + b">", flt="/ASCIIHexDecode", compress=False)),
           ("PNG", image(p, "/DeviceRGB", 8, zlib.compress(rows),
                         " /DecodeParms << /Predictor 15 /Colors 3 /Columns %d >>" % W, compress=False))]
    p.write(os.path.join(out, "g_filters.pdf"), ims)

    # Inline images, abbreviated keys and names, between the XObjects.
    p = Pdf()
    g8 = pack(samples_for(1, 8, 16, 12), 8, 16, 12, 1)
    i4 = pack(samples_for(1, 4, 16, 12), 4, 16, 12, 1)
    inline = (b"q 100 0 0 75 20 400 cm BI /W 16 /H 12 /CS /G /BPC 8 /F /AHx ID\n" + g8.hex().encode()
              + b">\nEI Q\n"
              + b"q 100 0 0 75 140 400 cm BI /W 16 /H 12 /CS [/I /RGB 15 <" + pal.hex().encode()
              + b">] /BPC 4 ID\n" + i4 + b"\nEI Q\n"
              + b"q 100 0 0 75 260 400 cm BI /W 16 /H 12 /IM true /D [1 0] /F /Fl ID\n"
              + zlib.compress(pack(samples_for(1, 1, 16, 12), 1, 16, 12, 1)) + b"\nEI Q\n")
    ims = [("X1", image(p, "/DeviceRGB", 8, raw))]
    p.write(os.path.join(out, "g_inline.pdf"), ims, inline)

    # JPEG fixtures through Pillow: gray, RGB, CMYK (Adobe, transform 0).
    try:
        from PIL import Image
    except ImportError:
        return
    p = Pdf()
    base = Image.new("RGB", (W * 3, H * 3))
    base.putdata([tuple(int(255 * pattern(x, y, c)) for c in range(3)) for y in range(H * 3) for x in range(W * 3)])
    ims = []
    for mode, cs, name in (("L", "/DeviceGray", "JG"), ("RGB", "/DeviceRGB", "JR"), ("CMYK", "/DeviceCMYK", "JK")):
        buf = io.BytesIO()
        base.convert(mode).save(buf, "JPEG", quality=92)
        dec = " /Decode [1 0 1 0 1 0 1 0]" if mode == "CMYK" else ""
        ims.append((name, p.stream("/Type /XObject /Subtype /Image /Width %d /Height %d /ColorSpace %s "
                                   "/BitsPerComponent 8 /Filter /DCTDecode%s" % (W * 3, H * 3, cs, dec),
                                   buf.getvalue())))
    p.write(os.path.join(out, "g_jpeg.pdf"), ims)


def fax_pdf(raw_path, k, columns, rows, out, eol):
    """Wrap raw fax data (e.g. Ghostscript's faxg3/faxg32d/faxg4 devices)
    in a one-image PDF with the matching CCITTFaxDecode parameters."""
    with open(raw_path, "rb") as fh:
        data = fh.read()
    p = Pdf()
    im = p.stream("/Type /XObject /Subtype /Image /Width %d /Height %d /ColorSpace /DeviceGray /BitsPerComponent 1 "
                  "/Filter /CCITTFaxDecode /DecodeParms << /K %d /Columns %d /Rows %d /EndOfLine %s >>"
                  % (columns, rows, k, columns, rows, "true" if eol else "false"), data)
    p.write(out, [("F", im)])


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "--fax":
        # --fax RAW K COLUMNS ROWS OUT.pdf EOL(0|1)
        a = sys.argv[2:]
        fax_pdf(a[0], int(a[1]), int(a[2]), int(a[3]), a[4], a[5] == "1")
    else:
        main(sys.argv[1] if len(sys.argv) > 1 else "pdfimg-corpus")
