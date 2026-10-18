#!/usr/bin/env python3
"""Generate the image-decoder benchmark corpus (fixture generation only; needs Pillow).

Usage: python3 docs/plans/bench/gen_images.py PHOTO.jpg OUTPUT_DIRECTORY

One workload per format variant the decoders in Cmedit.Image handle: the photo
for natural content, a synthetic screenshot for flat UI content, and noise as
the incompressible worst case. The hand-written Haskell suite does not need
Pillow.
"""
import argparse
import random
import struct
import zlib
from pathlib import Path

from PIL import Image, ImageDraw


def screenshot(w=1920, h=1080):
    im = Image.new("RGBA", (w, h), (24, 27, 32, 255))
    d = ImageDraw.Draw(im)
    for y in range(40, h, 24):
        d.rectangle((32, y, 240, y + 16), fill=(42, 47, 58, 255))
        d.text((260, y), "Image benchmark: editor window, repeated text and panels",
               fill=(190, 210, 230, 255))
    for x in range(0, w, 32):
        d.rectangle((x, 0, x + 31, 23), fill=(x % 256, 95, 150, 128 + x % 128))
    return im


ADAM7 = [(0, 0, 8, 8), (4, 0, 8, 8), (0, 4, 4, 8), (2, 0, 4, 4), (0, 2, 2, 4), (1, 0, 2, 2), (0, 1, 1, 2)]


def adam7_rgb_png(img, path, seed=1):
    """An Adam7-interlaced 8-bit RGB PNG, written by hand: Pillow silently
    ignores interlace=1 on save, so it cannot produce one. Each row gets a
    seeded random filter, so all five filters and the pass resets occur."""
    rng = random.Random(seed)
    w, h = img.size
    px = img.load()
    raw = bytearray()
    for x0, y0, dx, dy in ADAM7:
        pw = (w - x0 + dx - 1) // dx if w > x0 else 0
        ph = (h - y0 + dy - 1) // dy if h > y0 else 0
        if not pw or not ph:
            continue
        prev = None
        for ry in range(ph):
            y = y0 + ry * dy
            row = bytes(c for i in range(pw) for c in px[x0 + i * dx, y][:3])
            f = rng.randrange(5)
            out = bytearray(len(row))
            for i, b in enumerate(row):
                a = row[i - 3] if i >= 3 else 0
                up = prev[i] if prev else 0
                c = prev[i - 3] if prev and i >= 3 else 0
                if f == 1:
                    p = a
                elif f == 2:
                    p = up
                elif f == 3:
                    p = (a + up) // 2
                elif f == 4:
                    q = a + up - c
                    pa, pb, pc = abs(q - a), abs(q - up), abs(q - c)
                    p = a if pa <= pb and pa <= pc else (up if pb <= pc else c)
                else:
                    p = 0
                out[i] = (b - p) & 255
            raw += bytes([f]) + out
            prev = row

    def chunk(t, d):
        return struct.pack(">I", len(d)) + t + d + struct.pack(">I", zlib.crc32(t + d) & 0xFFFFFFFF)

    path.write_bytes(b"\x89PNG\r\n\x1a\n"
                     + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 1))
                     + chunk(b"IDAT", zlib.compress(bytes(raw), 9)) + chunk(b"IEND", b""))


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("photo", type=Path)
    p.add_argument("out", type=Path)
    a = p.parse_args()
    o = a.out
    o.mkdir(parents=True, exist_ok=True)

    photo = Image.open(a.photo).convert("RGB")
    screen = screenshot()
    rng = random.Random(42)
    noise = Image.frombytes("RGB", (1024, 768), rng.randbytes(1024 * 768 * 3))

    # PNG
    photo.save(o / "photo.png")
    screen.save(o / "screen.png")
    noise.save(o / "noise.png")
    photo.convert("P", palette=Image.ADAPTIVE).save(o / "photo-pal.png")
    adam7_rgb_png(photo, o / "photo-interlaced.png")
    photo.convert("L").save(o / "photo-gray.png")
    photo.convert("I;16").save(o / "photo-16.png")

    # JPEG
    photo.save(o / "photo-420.jpg", quality=90, subsampling=2)
    photo.save(o / "photo-444.jpg", quality=90, subsampling=0)
    photo.save(o / "photo-prog.jpg", quality=90, progressive=True)
    photo.convert("L").save(o / "photo-gray.jpg", quality=90)
    noise.save(o / "noise.jpg", quality=95, subsampling=0)

    # GIF: a big still plus an animation of full-canvas and sub-rect frames
    photo.convert("P", palette=Image.ADAPTIVE).save(o / "photo.gif")
    frames = []
    small = photo.resize((640, 507))
    for i in range(24):
        f = small.copy()
        ImageDraw.Draw(f).ellipse((i * 20, 100, i * 20 + 120, 220), fill=(255, 60, 60))
        frames.append(f.convert("P", palette=Image.ADAPTIVE))
    frames[0].save(o / "anim.gif", save_all=True, append_images=frames[1:],
                   duration=60, loop=0, optimize=False)

    # WebP
    photo.save(o / "photo-lossy.webp", quality=85)
    photo.save(o / "photo-lossless.webp", lossless=True)
    screen.save(o / "screen-lossless.webp", lossless=True)
    screen.save(o / "screen-lossy-alpha.webp", quality=85)

    # BMP
    photo.save(o / "photo-24.bmp")
    photo.convert("P", palette=Image.ADAPTIVE).save(o / "photo-8.bmp")
    screen.save(o / "screen-32.bmp")

    # Netpbm
    photo.save(o / "photo.ppm")
    photo.convert("L").save(o / "photo.pgm")
    photo.convert("1").save(o / "photo.pbm")
    # Plain (P3) PNM, written by hand so it does not depend on the Pillow version.
    w, h = small.size
    px = small.tobytes()
    with open(o / "small-ascii.ppm", "w") as fh:
        fh.write(f"P3\n{w} {h}\n255\n")
        for y in range(h):
            row = px[y * w * 3:(y + 1) * w * 3]
            fh.write(" ".join(str(b) for b in row))
            fh.write("\n")


if __name__ == "__main__":
    main()
