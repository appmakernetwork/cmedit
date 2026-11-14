#!/bin/sh
# Plan 0034 workstream D oracle: Cmedit.Svg + Cmedit.Raster against resvg.
#
#   docs/plans/bench/svgcheck.sh [list-of-svgs] [outdir]
#
# With no list, renders the hand-written feature corpus in
# docs/plans/bench/svg/. Each SVG is rendered by resvg at --width $SVGW
# (default 256; 0 = the document's own size) and by our pipeline at
# exactly the size resvg chose; both are flattened onto white and compared
# with ImageMagick: RMSE (normalised) and AE with a 15% fuzz (as a fraction
# of the pixels). A file passes when AE <= 1%. rsvg-convert renders a
# second opinion (column rsvg_ae%: ours against librsvg) for the feature
# corpus, every text-bearing file, and everything when RSVG=1.
#
# Output: <outdir>/results.tsv (path, status, rmse, ae%, rsvg-ae%, parse ms,
# render ms, notes) and a summary on stdout. Text-bearing files are tagged
# "text": both renderers use fonts, ours only once the font provider is
# integrated, so they are reported separately.
set -eu
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
LIST=${1:-}
OUT=${2:-/tmp/svgcheck}
SVGW=${SVGW:-256}
mkdir -p "$OUT/png" "$OUT/ours" "$OUT/rsvg"
BIN=${SVGCHECK_BIN:-/tmp/svgcheck-bin}
[ -n "${SVGCHECK_BIN:-}" ] || ( cd "$ROOT" && ghc -O2 -isrc -outputdir /tmp/svgcheck-o \
    -XLambdaCase -XOverloadedStrings -XBangPatterns -XTupleSections \
    -XScopedTypeVariables -XMultiWayIf -XRecordWildCards -XNamedFieldPuns \
    docs/plans/bench/SvgCheck.hs -o "$BIN" >/dev/null )
if [ -z "$LIST" ]; then
  LIST="$OUT/list.txt"; ls "$ROOT"/docs/plans/bench/svg/*.svg > "$LIST"; SVGW=${SVGW_FEATURE:-0}; RSVG=${RSVG:-1}
fi
RSVG=${RSVG:-0}
: > "$OUT/jobs.txt"; : > "$OUT/resvg-fail.txt"
n=0
while read -r f; do
  n=$((n+1)); key=$(printf '%05d' $n)
  if [ "$SVGW" = 0 ]; then a=""; else a="--width $SVGW"; fi
  # resvg's generic families default to Arial/Times New Roman, which a Linux
  # box rarely has (it then draws no text at all); point them at what
  # Cmedit.FontSys picks, so text compares like with like.
  if timeout 20 resvg $a --font-family "DejaVu Serif" --serif-family "DejaVu Serif" \
       --sans-serif-family "DejaVu Sans" --monospace-family "DejaVu Sans Mono" \
       "$f" "$OUT/png/$key.png" 2>/dev/null; then
    wh=$(identify -format '%w %h' "$OUT/png/$key.png")
    echo "$f $wh $OUT/ours/$key.pam $key" >> "$OUT/jobs.txt"
    if [ "$RSVG" = 1 ] || grep -q '<text' "$f" 2>/dev/null; then
      set -- $wh
      timeout 20 rsvg-convert -w "$1" -h "$2" "$f" -o "$OUT/rsvg/$key.png" 2>/dev/null || true
    fi
  else
    echo "$f" >> "$OUT/resvg-fail.txt"
  fi
done < "$LIST"
cut -d' ' -f1-4 "$OUT/jobs.txt" | "$BIN" > "$OUT/ours.txt" 2>&1 || true
printf 'path\tstatus\trmse\tae%%\trsvg_ae%%\tparse_ms\trender_ms\tnotes\n' > "$OUT/results.tsv"
while read -r f w h pam key; do
  line=$(grep -F "OK $f " "$OUT/ours.txt" | head -1 || true)
  tag=""; grep -q '<text' "$f" 2>/dev/null && tag=" text"
  if [ -z "$line" ]; then
    st=$(grep -F " $f " "$OUT/ours.txt" | head -1 | cut -c1-120 || true)
    printf '%s\tERROR%s\t-\t-\t-\t-\t-\t%s\n' "$f" "$tag" "$st" >> "$OUT/results.tsv"; continue
  fi
  pms=$(echo "$line" | awk '{print $3}'); rms=$(echo "$line" | awk '{print $4}')
  notes=$(echo "$line" | cut -d' ' -f5-)
  magick "$OUT/png/$key.png" -background white -alpha remove -alpha off "$OUT/ours/$key.ref.png"
  magick "$pam" -background white -alpha remove -alpha off "$OUT/ours/$key.png"
  rmse=$(compare -metric RMSE "$OUT/ours/$key.ref.png" "$OUT/ours/$key.png" null: 2>&1 | sed 's/.*(\(.*\)).*/\1/')
  ae=$(compare -fuzz 15% -metric AE "$OUT/ours/$key.ref.png" "$OUT/ours/$key.png" null: 2>&1 | awk '{print $1}')
  aep=$(awk -v a="$ae" -v w="$w" -v h="$h" 'BEGIN{printf "%.3f", 100*a/(w*h)}')
  rae="-"
  if [ -f "$OUT/rsvg/$key.png" ]; then
    magick "$OUT/rsvg/$key.png" -background white -alpha remove -alpha off "$OUT/rsvg/$key.flat.png"
    r=$(compare -fuzz 15% -metric AE "$OUT/ours/$key.png" "$OUT/rsvg/$key.flat.png" null: 2>&1 | awk '{print $1}' || true)
    rae=$(awk -v a="$r" -v w="$w" -v h="$h" 'BEGIN{printf "%.3f", 100*a/(w*h)}')
  fi
  st=$(awk -v a="$aep" 'BEGIN{print (a <= 1.0) ? "PASS" : "FAIL"}')
  printf '%s\t%s%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$f" "$st" "$tag" "$rmse" "$aep" "$rae" "$pms" "$rms" "$notes" >> "$OUT/results.tsv"
done < "$OUT/jobs.txt"
echo "files: $n, resvg failed: $(wc -l < "$OUT/resvg-fail.txt")"
awk -F'\t' 'NR>1 { split($2, s, " "); k = s[1] (s[2] ? " (text)" : ""); c[k]++ } END { for (k in c) print k ": " c[k] }' "$OUT/results.tsv" | sort
echo "worst (by AE%):"
awk -F'\t' 'NR>1 && $4 != "-"' "$OUT/results.tsv" | sort -t"$(printf '\t')" -k4 -g -r | head -20 | cut -f1,2,4,5,8
