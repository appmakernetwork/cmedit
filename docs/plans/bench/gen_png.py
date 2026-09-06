#!/usr/bin/env python3
"""Generate PNG decode workloads (fixture generation only; requires Pillow).

Usage: python3 docs/plans/bench/gen_png.py PHOTO.jpg OUTPUT_DIRECTORY
The hand-written Haskell regression suite does not need Pillow.
"""
import argparse
import random
from pathlib import Path

from PIL import Image, ImageDraw


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("photo", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)

    with Image.open(args.photo) as photo:
        photo.convert("RGB").save(args.output / "photo.png")

    screen = Image.new("RGBA", (1920, 1080), (24, 27, 32, 255))
    draw = ImageDraw.Draw(screen)
    for y in range(40, 1080, 24):
        draw.rectangle((32, y, 240, y + 16), fill=(42, 47, 58, 255))
        draw.text((260, y),
                  "PNG loading benchmark: editor window, repeated text and coloured panels",
                  fill=(190, 210, 230, 255))
    for x in range(0, 1920, 32):
        draw.rectangle((x, 0, x + 31, 23), fill=(x % 256, 95, 150, 128 + x % 128))
    screen.save(args.output / "screen.png")

    rng = random.Random(42)
    noise = Image.frombytes("RGB", (1024, 768), rng.randbytes(1024 * 768 * 3))
    noise.save(args.output / "noise.png")


if __name__ == "__main__":
    main()
