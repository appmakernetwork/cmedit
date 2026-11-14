#!/bin/sh
# Plan 0034 workstream E oracle: Cmedit.PdfRender + Cmedit.Raster against
# poppler's pdftoppm, page by page.
#
#   docs/plans/bench/pdfrendercheck.sh [list-of-pdfs] [outdir]
#   docs/plans/bench/pdfrendercheck.sh synth [outdir]   # build + run the synthetic corpus
#
# With no list, renders the committed feature corpus in docs/plans/bench/pdf/
# (gen_pdfrender.py). "synth" additionally builds fixtures with the tools on
# this machine (ps2pdf, soffice, rsvg-convert, magick) into <outdir>/synth;
# those are generated, never committed.
#
# Each page (the first $PAGES of each file, default 5) is rendered by
# pdftoppm at $DPI (default 100) and by PdfRenderCheck at the same size
# (and, unless CAIRO=0, by pdftocairo as a second opinion: poppler's cairo
# back end, whose anti-aliasing is exact-area like ours, where pdftoppm's
# Splash snaps glyphs and thin rules to pixels),
# both flattened on white, and compared with ImageMagick: RMSE (normalised)
# and AE with a 15% fuzz as a fraction of the pixels. Fonts come from the
# system through Cmedit.FontProvider, as the editor will get them.
#
# Output: <outdir>/results.tsv (key, path, page, status, rmse, ae%, scene ms,
# raster ms, commands, notes) and a distribution on stdout. Files are only
# read; nothing is copied anywhere except the rendered pages in <outdir>.
set -eu
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
HERE="$ROOT/docs/plans/bench"
LIST=${1:-}
OUT=${2:-/tmp/pdfrendercheck}
DPI=${DPI:-100}
PAGES=${PAGES:-5}
# The rasteriser's memory cap for the bench, in MB. Its own default (three
# canvases) is too small to minify a photograph much larger than the page
# (the horizontal prescale pass alone is device width x source height), so
# such images are dropped; the page view can choose its cap, and 256 MB is
# what the comparison uses unless told otherwise.
RASTER_MB=${RASTER_MB:-256}
BIN=${PDFRENDERCHECK_BIN:-/tmp/pdfrendercheck}
CAIRO=${CAIRO:-1}
mkdir -p "$OUT/ref" "$OUT/ours" "$OUT/cairo"
if [ -z "${PDFRENDERCHECK_BIN:-}" ]; then
  ( cd "$ROOT" && ghc -O2 -isrc -outputdir /tmp/pdfrendercheck-o -threaded \
      -XLambdaCase -XOverloadedStrings -XBangPatterns -XTupleSections \
      -XScopedTypeVariables -XMultiWayIf -XRecordWildCards -XNamedFieldPuns \
      docs/plans/bench/PdfRenderCheck.hs -o "$BIN" >/dev/null )
fi
if [ "$LIST" = synth ]; then
  S="$OUT/synth"; mkdir -p "$S"
  python3 -I "$HERE/gen_pdfrender.py" "$S"
  # PostScript through Ghostscript: embedded Type 1C subsets, shfill, patterns.
  cat > "$S/ps1.ps" <<'EOF'
%!PS
/Times-Roman findfont 24 scalefont setfont 40 700 moveto (Ghostscript Type 1C text) show
/Helvetica-Oblique findfont 14 scalefont setfont 40 670 moveto 2 0 (ashow spacing) ashow
40 640 moveto 6 0 32 (widthshow with spaces) widthshow
/Courier-Bold findfont 18 scalefont setfont 40 610 moveto (Courier-Bold 0123456789) show
gsave 300 400 translate 30 rotate /Symbol findfont 30 scalefont setfont 0 0 moveto (abgdp) show grestore
<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [40 0 560 0] /Function << /FunctionType 2 /Domain [0 1] /C0 [1 0.5 0] /C1 [0 0.3 1] /N 1 >> /Extend [true true] >> shfill
EOF
  sed -i 's/^<< \/ShadingType 2/gsave 40 100 520 150 rectclip << \/ShadingType 2/; s/shfill$/shfill grestore/' "$S/ps1.ps"
  printf '%s\n' '<< /PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 30 30] /XStep 30 /YStep 30' \
    '/PaintProc { pop 1 0 0 setrgbcolor 0 0 15 15 rectfill 0 0 1 setrgbcolor 15 15 15 15 rectfill } >> matrix makepattern setpattern' \
    '40 300 200 150 rectfill' \
    '0 0.5 0 setrgbcolor 4 setlinewidth [10 5] 0 setdash 300 300 moveto 550 450 lineto stroke showpage' >> "$S/ps1.ps"
  ps2pdf "$S/ps1.ps" "$S/gs_text.pdf" 2>/dev/null || true
  # LibreOffice: TrueType subsets, CIDFontType2, symbols, CJK when installed.
  cat > "$S/lo.html" <<'EOF'
<html><body>
<h1 style="font-family:'Liberation Serif'">LibreOffice heading</h1>
<p style="font-family:'Liberation Sans'">Sans paragraph with <b>bold</b>, <i>italic</i> and <u>underline</u>. The quick brown fox jumps over the lazy dog 0123456789.</p>
<p style="font-family:'DejaVu Sans'">Unicode: &#945;&#946;&#947; &#8364; &#8730;&#8734; &#8592;&#8594; &#9733;&#9829;</p>
<p style="font-family:'Noto Sans CJK JP'">&#26085;&#26412;&#35486;&#12398;&#25991;&#31456; &#20013;&#25991; &#54620;&#44544;</p>
<table border="1"><tr><td>cell</td><td style="background:#cce">shaded</td></tr></table>
</body></html>
EOF
  ( cd "$S" && timeout 120 soffice --headless --convert-to pdf lo.html >/dev/null 2>&1 ) || true
  [ -f "$S/lo.pdf" ] && mv "$S/lo.pdf" "$S/lo_text.pdf" || true
  # cairo: gradients, soft masks, patterns, text as Type 3 or TrueType.
  for f in "$HERE"/svg/*.svg; do
    b=$(basename "$f" .svg)
    timeout 30 rsvg-convert -f pdf "$f" -o "$S/cairo_$b.pdf" 2>/dev/null || rm -f "$S/cairo_$b.pdf"
  done
  # ImageMagick: images through Flate and DCT.
  magick -size 320x240 plasma:fractal -seed 7 "$S/plasma.png" 2>/dev/null || true
  magick "$S/plasma.png" -compress zip "$S/im_zip.pdf" 2>/dev/null || true
  magick "$S/plasma.png" -compress jpeg "$S/im_jpeg.pdf" 2>/dev/null || true
  magick "$S/plasma.png" -colorspace CMYK -compress jpeg "$S/im_cmyk.pdf" 2>/dev/null || true
  LIST="$OUT/list.txt"; ls "$S"/*.pdf > "$LIST"
elif [ -z "$LIST" ]; then
  LIST="$OUT/list.txt"; ls "$HERE"/pdf/*.pdf > "$LIST"
fi
: > "$OUT/jobs.txt"; : > "$OUT/keys.txt"
n=0
while IFS= read -r f; do
  n=$((n+1))
  np=$(timeout 30 pdfinfo "$f" 2>/dev/null | awk '/^Pages:/{print $2}')
  [ -n "$np" ] || { printf '%05d\t%s\t0\tpdfinfo-fail\n' "$n" "$f" >> "$OUT/keys.txt"; continue; }
  p=1
  while [ "$p" -le "$np" ] && [ "$p" -le "$PAGES" ]; do
    key=$(printf '%05d-%03d' "$n" "$p")
    if timeout 120 pdftoppm -cropbox -r "$DPI" -f "$p" -l "$p" -singlefile -png "$f" "$OUT/ref/$key" 2>/dev/null; then
      printf '%s\t%s\t%s\t%s\n' "$f" "$p" "$DPI" "$OUT/ours/$key.pam" >> "$OUT/jobs.txt"
      if [ "$CAIRO" = 1 ]; then
        timeout 120 pdftocairo -cropbox -r "$DPI" -f "$p" -l "$p" -singlefile -png "$f" "$OUT/cairo/$key" 2>/dev/null || true
      fi
      printf '%s\t%s\t%s\tok\n' "$key" "$f" "$p" >> "$OUT/keys.txt"
    else
      printf '%s\t%s\t%s\tpdftoppm-fail\n' "$key" "$f" "$p" >> "$OUT/keys.txt"
    fi
    p=$((p+1))
  done
done < "$LIST"
"$BIN" "mb=$RASTER_MB" < "$OUT/jobs.txt" > "$OUT/ours.txt" 2>"$OUT/ours.err" || true
python3 -I "$HERE/pdfrendercompare.py" "$OUT"
