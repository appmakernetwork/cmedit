#!/bin/sh
# Build ImageDecode.hs against a source tree and time each file in fresh
# processes (one file per process, so the peak is per-file).
#   docs/plans/bench/imgbench.sh build SRC_DIR OUT_BIN
#   docs/plans/bench/imgbench.sh run OUT_BIN RUNS FILE...
# `run` prints, per file: median ms, min ms, allocated MB, peak MB, hash.
set -e
case "$1" in
build)
  src=$2; out=$3
  ghc --make docs/plans/bench/ImageDecode.hs -i"$src" -O2 -rtsopts -with-rtsopts=-T \
    -outputdir "$out.d" -o "$out" -package base -package bytestring -package array \
    -package containers -package text -package filepath \
    -XLambdaCase -XOverloadedStrings -XBangPatterns -XTupleSections \
    -XScopedTypeVariables -XMultiWayIf -XRecordWildCards -XNamedFieldPuns -w >/dev/null
  ;;
run)
  bin=$2; runs=$3; shift 3
  for f in "$@"; do
    fr=0; case "$f" in *anim.gif) fr=1;; esac
    i=0; : > /tmp/imgbench.$$
    while [ $i -lt "$runs" ]; do
      FRAMES=$fr "$bin" "$f" >> /tmp/imgbench.$$; i=$((i+1))
    done
    python3 - "$f" /tmp/imgbench.$$ <<'PY'
import re, statistics, sys, os
f, log = sys.argv[1], sys.argv[2]
rows = [re.search(r": ([\d.]+) ms, (\d+) bytes, peak (\d+) MB.*FNV-1a (\w+)", l).groups()
        for l in open(log)]
ms = [float(r[0]) for r in rows]
print(f"{os.path.basename(f):26s} med {statistics.median(ms):9.2f} ms  min {min(ms):9.2f}  "
      f"alloc {int(rows[0][1])/1e6:9.1f} MB  peak {max(int(r[2]) for r in rows):4d} MB  {rows[0][3]}")
PY
  done
  rm -f /tmp/imgbench.$$
  ;;
esac
