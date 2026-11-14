#!/bin/sh
# Oracle comparison for Cmedit.Raster against resvg (plan 0034, workstream A).
#
#   docs/plans/bench/rastercheck.sh [OUT_DIR]
#   docs/plans/bench/rastercheck.sh bench     # the -O2 performance targets
#   docs/plans/bench/rastercheck.sh safe      # bounds-checked core: oracle cases, left-edge sweep, random scenes
#
# Builds RasterCheck.hs (-O2) against src/, which writes NAME.svg and our
# rendering NAME.out.png for every hand-written case; renders each SVG with
# resvg; and compares the two with ImageMagick, both flattened onto white
# (so colour differences under transparency do not count) and on the alpha
# channel alone. Prints one line per case:
#
#   name   RMSE(colour, 0..1)   RMSE(alpha)   AE (pixels differing by >8%)
#
# Anti-aliasing alone puts renderers ~0.005-0.01 apart on edge-heavy cases;
# the script flags anything over 0.02 with '!'. Run from the repository root.
set -e
out=${1:-/tmp/rastercheck}
bin=/tmp/rastercheck-bin
ghc --make docs/plans/bench/RasterCheck.hs -isrc -O2 -outputdir "$bin.d" -o "$bin" \
  -package base -package bytestring -package array -package containers -package directory -package filepath \
  -XBangPatterns -XScopedTypeVariables -XMultiWayIf -XLambdaCase -w >/dev/null
if [ "$out" = bench ]; then exec "$bin" bench; fi
if [ "$out" = safe ]; then
  # the same program over a bounds-checked core (every array index checked)
  ghc --make docs/plans/bench/RasterCheck.hs -isrc -O1 -DRASTER_SAFE -XCPP -outputdir "$bin-safe.d" -o "$bin-safe" \
    -package base -package bytestring -package array -package containers -package directory -package filepath \
    -XBangPatterns -XScopedTypeVariables -XMultiWayIf -XLambdaCase -w >/dev/null
  exec "$bin-safe" safe
fi
mkdir -p "$out"
"$bin" "$out" >/dev/null
printf '%-28s %9s %9s %7s\n' case rmse alpha-rmse ae
for svg in "$out"/*.svg; do
  n=${svg%.svg}
  name=$(basename "$n")
  resvg "$svg" "$n.ref.png" 2>/dev/null || { printf '%-28s resvg failed\n' "$name"; continue; }
  convert "$n.ref.png" -background white -alpha remove -alpha off "$n.refw.png"
  convert "$n.out.png" -background white -alpha remove -alpha off "$n.outw.png"
  convert "$n.ref.png" -alpha extract "$n.refa.png"
  convert "$n.out.png" -alpha extract "$n.outa.png"
  rmse=$(compare -metric RMSE "$n.outw.png" "$n.refw.png" null: 2>&1 | sed 's/.*(\(.*\)).*/\1/')
  armse=$(compare -metric RMSE "$n.outa.png" "$n.refa.png" null: 2>&1 | sed 's/.*(\(.*\)).*/\1/')
  ae=$(compare -metric AE -fuzz 8% "$n.outw.png" "$n.refw.png" null: 2>&1 | sed 's/ .*//')
  flag=$(awk -v a="$rmse" -v b="$armse" 'BEGIN { if (a > 0.02 || b > 0.02) print "!"; else print "" }')
  printf '%-28s %9.5f %9.5f %7s %s\n' "$name" "$rmse" "$armse" "$ae" "$flag"
done
