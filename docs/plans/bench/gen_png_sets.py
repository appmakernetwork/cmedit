#!/usr/bin/env python3
"""Regenerate the PNG decoder's three synthetic test sets (deterministic).

    python3 -I docs/plans/bench/gen_png_sets.py [OUT_PARENT=/tmp]

  OUT/fable-extra  312 edge files: every colour type and bit depth, widths
                   1..131, random per-row filters (incl. an invalid type),
                   tRNS variants. All too small for the parallel paths.
  OUT/fable-par    33 files of 1 MB+ raw data that reach the parallel
                   unfilter: per layout a 'wave' file (every row reads the
                   one above), a 'seg' file (independent rows mixed in) and
                   a 'few' file (an independent row every 211 rows).
  OUT/fable-big    11 large files for the converters the photo corpus lacks:
                   sub-byte gray, gray+alpha, 16-bit, tRNS colour keys.

The reference list for the edge set is the baseline harness's hashes, one
line per file in glob order (BIN: an imgbench.sh build of the old source):
    for f in /tmp/fable-extra/*.png; do /tmp/ib-base $f | grep -o 'FNV-1a [0-9a-f]*\\|Left.*'; done > /tmp/ib-base.extra.txt
and a candidate is checked by piping the same loop into `diff /tmp/ib-base.extra.txt -`
(likewise for the other two sets). Run candidates at -N1 and -N4: the
parallel paths only exist with more than one capability.
"""
import os, random, struct, sys, zlib

def chunk(t, d):
    return struct.pack('>I', len(d)) + t + d + struct.pack('>I', zlib.crc32(t + d))

def paeth(a, b, c):
    p = a + b - c; pa = abs(p - a); pb = abs(p - b); pc = abs(p - c)
    return a if pa <= pb and pa <= pc else (b if pb <= pc else c)

CH = {0: 1, 2: 3, 3: 1, 4: 2, 6: 4}

def gen_extra(out):
    os.makedirs(out, exist_ok=True)
    random.seed(7)
    n = 0
    for ct, bds in ((0, (1, 2, 4, 8, 16)), (2, (8, 16)), (3, (1, 2, 4, 8)), (4, (8, 16)), (6, (8, 16))):
        for bd in bds:
            for (w, h) in ((1, 1), (1, 5), (2, 3), (3, 7), (5, 4), (7, 9), (8, 2), (9, 11), (17, 13), (33, 20), (64, 9), (131, 17)):
                for trns in (False, True):
                    ch = CH[ct]; bpp = max(1, ch * bd // 8); rb = (w * ch * bd + 7) // 8
                    prev = bytes(rb); raw = b''
                    for y in range(h):
                        row = bytes(random.randrange(256) if random.random() < 0.7 else 0 for _ in range(rb))
                        ft = random.choice([0, 1, 2, 3, 4, 4, 4, 3, 1, 2] + ([7] if random.random() < 0.05 else []))
                        o = bytearray()
                        for i in range(rb):
                            a = row[i - bpp] if i >= bpp else 0; b = prev[i]; c = prev[i - bpp] if i >= bpp else 0
                            pr = {0: 0, 1: a, 2: b, 3: (a + b) // 2, 4: paeth(a, b, c)}.get(ft, 0)
                            o.append((row[i] - pr) & 255)
                        raw += bytes([ft]) + bytes(o); prev = row
                    d = b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('>IIBBBBB', w, h, bd, ct, 0, 0, 0))
                    if ct == 3:
                        d += chunk(b'PLTE', bytes(random.randrange(256) for _ in range(3 * random.choice([2, 16, 200, 256]))))
                    if trns:
                        if ct == 3:
                            d += chunk(b'tRNS', bytes(random.randrange(256) for _ in range(random.choice([1, 5, 100]))))
                        elif ct == 0:
                            d += chunk(b'tRNS', struct.pack('>H', row[0] if bd >= 8 else (row[0] >> (8 - bd))))
                        elif ct == 2:
                            d += chunk(b'tRNS', struct.pack('>HHH', *((row[0], row[1], row[2]) if bd == 8 else (row[0] * 256 + row[1], row[2] * 256 + row[3], row[4] * 256 + row[5]))) if rb >= 6 or bd == 8 else b'\0' * 6)
                        else:
                            continue
                    d += chunk(b'IDAT', zlib.compress(raw)) + chunk(b'IEND', b'')
                    n += 1
                    open(os.path.join(out, 't%03d_ct%d_bd%d_%dx%d_%d.png' % (n, ct, bd, w, h, trns)), 'wb').write(d)
    return n

def enc(rows, prev0, bpp, fts):
    out = []; prev = prev0
    for row, ft in zip(rows, fts):
        rb = len(row)
        if ft == 0 or ft > 4: o = row
        elif ft == 1: o = bytes((row[i] - (row[i - bpp] if i >= bpp else 0)) & 255 for i in range(rb))
        elif ft == 2: o = bytes((row[i] - prev[i]) & 255 for i in range(rb))
        elif ft == 3: o = bytes((row[i] - (((row[i - bpp] if i >= bpp else 0) + prev[i]) >> 1)) & 255 for i in range(rb))
        else:
            o = bytearray(rb)
            for i in range(rb):
                a = row[i - bpp] if i >= bpp else 0; b = prev[i]; c = prev[i - bpp] if i >= bpp else 0
                o[i] = (row[i] - paeth(a, b, c)) & 255
            o = bytes(o)
        out.append(bytes([ft]) + o); prev = row
    return b''.join(out)

def gen_par(out):
    os.makedirs(out, exist_ok=True)
    random.seed(11)
    n = 0
    for ct, bd, w, h in ((0, 8, 1300, 900), (0, 16, 900, 700), (2, 8, 700, 600), (2, 16, 400, 500), (3, 8, 1300, 900), (4, 8, 800, 700), (4, 16, 500, 600), (6, 8, 600, 500), (6, 16, 300, 500), (0, 4, 2600, 900), (2, 8, 523, 701)):
        ch = CH[ct]; bpp = max(1, ch * bd // 8); rb = (w * ch * bd + 7) // 8
        for mode in ('wave', 'seg', 'few'):
            rows = [random.randbytes(rb) if random.random() < 0.5 else bytes((x * 3 + y) & 255 for x in range(rb)) for y in range(h)]
            if mode == 'wave': fts = [random.choice([2, 3, 4, 4]) for _ in range(h)]
            elif mode == 'seg': fts = [random.choice([0, 1, 2, 3, 4, 4, 4, 4, 4, 9]) for _ in range(h)]
            else: fts = [random.choice([2, 3, 4]) if (y % 211) else 1 for y in range(h)]
            d = b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('>IIBBBBB', w, h, bd, ct, 0, 0, 0))
            if ct == 3: d += chunk(b'PLTE', random.randbytes(768))
            d += chunk(b'IDAT', zlib.compress(enc(rows, bytes(rb), bpp, fts), 1)) + chunk(b'IEND', b'')
            n += 1
            open(os.path.join(out, 'p%02d_ct%d_bd%d_%s.png' % (n, ct, bd, mode)), 'wb').write(d)
    return n

def gen_big(out):
    os.makedirs(out, exist_ok=True)
    def mk(name, w, h, ct, bd, trns=None):
        ch = CH[ct]; rb = (w * ch * bd + 7) // 8
        base = bytes((i * 7 + (i >> 5)) & 255 for i in range(rb))
        raw = b''.join(b'\0' + base[y % 97:] + base[:y % 97] for y in range(h))
        d = b'\x89PNG\r\n\x1a\n' + chunk(b'IHDR', struct.pack('>IIBBBBB', w, h, bd, ct, 0, 0, 0))
        if trns: d += chunk(b'tRNS', trns)
        d += chunk(b'IDAT', zlib.compress(raw, 1)) + chunk(b'IEND', b'')
        open(os.path.join(out, name), 'wb').write(d)
    b = bytes((i * 7 + (i >> 5)) & 255 for i in range(12))
    mk('gray4.png', 4096, 4096, 0, 4); mk('gray1.png', 4096, 4096, 0, 1); mk('ga8.png', 3000, 3000, 4, 8)
    mk('rgb16.png', 2500, 2500, 2, 16); mk('rgba16.png', 2500, 2500, 6, 16); mk('ga16.png', 2500, 2500, 4, 16)
    mk('gray16.png', 3000, 3000, 0, 16)
    mk('rgb8t.png', 3000, 3000, 2, 8, struct.pack('>HHH', b[3], b[4], b[5]))
    mk('rgb16t.png', 2500, 2500, 2, 16, b[6:12]); mk('gray16t.png', 3000, 3000, 0, 16, b[4:6])
    mk('gray8t.png', 3000, 3000, 0, 8, struct.pack('>H', b[5]))
    return 11

if __name__ == '__main__':
    parent = sys.argv[1] if len(sys.argv) > 1 else '/tmp'
    print('extra', gen_extra(os.path.join(parent, 'fable-extra')))
    print('par', gen_par(os.path.join(parent, 'fable-par')))
    print('big', gen_big(os.path.join(parent, 'fable-big')))
