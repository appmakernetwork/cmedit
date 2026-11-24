#!/usr/bin/env bash
# Build and run the inflateDyn differential fuzz harness against 4f3c80e.
# Usage: docs/plans/bench/inflatedyn_fuzz.sh [SRC_DIR] [SEED_COUNT]
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../../.." && pwd)"
SRC="${1:-$REPO/src}"
SEEDS="${2:-60}"

WORK="$(mktemp -d /tmp/inflatedyn-fuzz.XXXXXX)"
trap 'rm -rf "$WORK"' EXIT

CORPUS="$WORK/corpus"
mkdir -p "$CORPUS" "$WORK/src/Cmedit" "$WORK/obj"

# Generate diverse valid streams: text, binary, levels 0..9, stored/fixed/dynamic/rle/huff/filtered,
# multi-block, high-expansion (triggers multiple buffer doublings), and small streams for
# exhaustive every-byte truncation and single-bit-flip sweeps.
python3 - "$CORPUS" << 'PY'
import os, random, sys, zlib

out = sys.argv[1]
rng = random.Random(20261124)

def w(name, data):
    with open(os.path.join(out, name), "wb") as f:
        f.write(data)

def zcomp(data, level=6, wbits=15, strategy=zlib.Z_DEFAULT_STRATEGY):
    co = zlib.compressobj(level, zlib.DEFLATED, wbits, 8, strategy)
    return co.compress(data) + co.flush()

# Payloads
text_small = (b"The quick brown fox jumps over the lazy dog. 0123456789\n" * 8)
text_med   = (b"Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
              b"Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n" * 120)
bin_small  = bytes(rng.getrandbits(8) for _ in range(300))
bin_med    = bytes(rng.getrandbits(8) for _ in range(12000))
mix_med    = b"".join([
    b"A" * 500,
    bytes(range(256)) * 10,
    b"XY" * 300,
    b"ABC" * 300,
    b"ABCD" * 300,
    b"ABCDE" * 300,
    bytes(rng.getrandbits(8) for _ in range(2000)),
    b"Z" * 1000,
])
# High-ratio payload (~200 KB expanded from ~600 bytes compressed) to trigger many buffer doublings!
high_ratio = (b"0123456789abcdef" * 128 + b"\x00" * 2048 + b"abcdefgh" * 256) * 32
# Skewed byte distribution to force deep (12-15 bit) Huffman codes (exercises subtables)
skewed = bytearray()
for i in range(1, 256):
    # Fibonacci-ish frequencies capped so stream stays moderate
    skewed.extend([i] * max(1, (i * i) // 16))
rng.shuffle(skewed)
skewed = bytes(skewed)

# 1. Small streams for exhaustive every-byte truncation & bit-flip tests
w("empty.small.z",            zcomp(b"", level=6))
w("tiny_fixed.small.z",       zcomp(b"hello world!", level=6, strategy=zlib.Z_FIXED))
w("tiny_dyn.small.z",         zcomp(text_small, level=6))
w("tiny_stored.small.z",      zcomp(text_small[:200], level=0))
w("tiny_rle.small.z",         zcomp(b"AAAAABBBCCCCCDDDDDEEEEE" * 10, level=6, strategy=zlib.Z_RLE))
w("tiny_huff.small.z",        zcomp(text_small[:250], level=6, strategy=zlib.Z_HUFFMAN_ONLY))
w("tiny_bin.small.z",         zcomp(bin_small, level=6))
w("tiny_match_d1.small.raw",  zcomp(b"X" * 300, level=9, wbits=-15))
w("tiny_match_d3.small.raw",  zcomp(b"XYZ" * 120, level=9, wbits=-15))

# Multi-block small stream (stored + fixed + dynamic via Z_FULL_FLUSH)
co = zlib.compressobj(6, zlib.DEFLATED, -15)
mb = co.compress(b"first block text " * 5) + co.flush(zlib.Z_SYNC_FLUSH)
co2 = zlib.compressobj(6, zlib.DEFLATED, -15, 8, zlib.Z_FIXED)
# Build a real single-stream multi-block with Z_FULL_FLUSH
co_m = zlib.compressobj(6, zlib.DEFLATED, 15)
p1 = co_m.compress(b"block one repeated pattern " * 10) + co_m.flush(zlib.Z_FULL_FLUSH)
p2 = co_m.compress(bytes(rng.getrandbits(8) for _ in range(80))) + co_m.flush(zlib.Z_FULL_FLUSH)
p3 = co_m.compress(b"Z" * 120 + b"end!") + co_m.flush()
w("multiblock.small.z", p1 + p2 + p3)

# 2. All compression levels 0..9 on text and binary
for lvl in range(10):
    w(f"text_l{lvl}.z", zcomp(text_med, level=lvl))
    w(f"bin_l{lvl}.z",  zcomp(bin_med, level=lvl))

# 3. Strategies & special payloads
for name, strat in [("fixed", zlib.Z_FIXED), ("huff", zlib.Z_HUFFMAN_ONLY),
                    ("rle", zlib.Z_RLE), ("filtered", zlib.Z_FILTERED)]:
    w(f"mix_{name}.z", zcomp(mix_med, level=6, strategy=strat))

w("high_ratio_dyn.z",   zcomp(high_ratio, level=9))
w("high_ratio_fixed.z", zcomp(high_ratio, level=6, strategy=zlib.Z_FIXED))
w("skewed_deep.z",      zcomp(skewed, level=9))
w("skewed_raw.raw",     zcomp(skewed, level=6, wbits=-15))
PY

cp "$SRC/Cmedit/Inflate.hs" "$WORK/src/Cmedit/Inflate.hs"
cp "$HERE/InflateRef4f3c80e.hs" "$WORK/src/InflateRef4f3c80e.hs"
cp "$HERE/InflateDynFuzz.hs" "$WORK/src/Main.hs"

timeout 300 ghc -O2 -i"$WORK/src" -outputdir "$WORK/obj" -o "$WORK/fuzz" "$WORK/src/Main.hs"
"$WORK/fuzz" "$CORPUS" "$SEEDS"
