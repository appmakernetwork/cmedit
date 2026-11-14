#!/bin/sh
# Compare Cmedit.PdfImage against poppler's pdfimages, image by image.
#   docs/plans/bench/pdfimagecheck.sh build            # -> /tmp/pdfimagecheck
#   docs/plans/bench/pdfimagecheck.sh run FILE.pdf...  # one summary line per image
#   docs/plans/bench/pdfimagecheck.sh corpus DIR       # gen_pdfimages.py + ImageMagick fixtures
# Needs poppler-utils (pdfimages), python3 with numpy and Pillow.
#
# What is compared: each image's colour after /Decode and its colour space
# (pdfimages -png converts to grey or RGB the same way), each soft mask on its
# own, each stencil's coverage. CMYK (raw, JPEG, and anything ending in
# DeviceCMYK) is compared directly too: the reader uses poppler's own
# profile-less CMYK->RGB, so those rows are held to the same bar. Files are
# only read; nothing is copied anywhere.
set -e
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
case "$1" in
build)
  ghc --make "$here/PdfImageCheck.hs" -i"$root/src" -O2 -outputdir /tmp/pdfimagecheck.d \
    -o /tmp/pdfimagecheck -package base -package bytestring -package array -package containers \
    -package text -package filepath \
    -XLambdaCase -XOverloadedStrings -XBangPatterns -XTupleSections -XScopedTypeVariables \
    -XMultiWayIf -XRecordWildCards -XNamedFieldPuns -w >/dev/null
  ;;
corpus)
  out=$2; mkdir -p "$out"
  python3 -I "$here/gen_pdfimages.py" "$out"
  t=$(mktemp -d)
  magick -size 320x240 plasma:fractal -seed 7 "$t/rgb.png"
  magick "$t/rgb.png" -colorspace Gray "$t/gray.png"
  magick "$t/rgb.png" -colors 16 -type Palette "$t/pal.png"
  magick "$t/rgb.png" -alpha set -channel A -fx "i/w" "$t/rgba.png"
  magick "$t/gray.png" -threshold 50% -type Bilevel "$t/bw.png"
  magick "$t/rgb.png" -compress zip "$out/m_rgb_zip.pdf"
  magick "$t/rgb.png" -compress jpeg "$out/m_rgb_jpg.pdf"
  magick "$t/rgb.png" -compress jpeg -interlace JPEG "$out/m_rgb_prog.pdf"
  magick "$t/gray.png" -compress jpeg "$out/m_gray_jpg.pdf"
  magick "$t/rgb.png" -colorspace CMYK -compress jpeg "$out/m_ycck_jpg.pdf"
  magick "$t/rgb.png" -colorspace CMYK -compress zip "$out/m_cmyk_zip.pdf"
  magick "$t/pal.png" -compress zip "$out/m_pal.pdf"
  magick "$t/rgba.png" -compress zip "$out/m_rgba.pdf"
  magick "$t/bw.png" -compress Group4 "$out/m_g4.pdf"
  magick "$t/rgb.png" -compress lzw "$out/m_lzw.pdf"
  magick "$t/rgb.png" -compress rle "$out/m_rle.pdf"
  # Group 3 (1-D and 2-D) and Group 4 from ghostscript's own fax encoders,
  # wrapped in PDFs with the matching parameters.
  gs -q -dNOPAUSE -dBATCH -dSAFER -r100 -sDEVICE=pbmraw -o "$t/fax.pbm" "$out/m_g4.pdf"
  set -- $(head -c 64 "$t/fax.pbm" | grep -a -v '^#' | sed -n 2p)
  fw=$1; fh=$2
  for spec in "faxg3 0 1" "faxg3 0 0" "faxg32d 1 1" "faxg4 -1 0"; do
    set -- $spec
    gs -q -dNOPAUSE -dBATCH -dSAFER -r100 -dAdjustWidth=0 -sDEVICE=$1 -o "$t/$1.raw" "$out/m_g4.pdf"
    python3 -I "$here/gen_pdfimages.py" --fax "$t/$1.raw" $2 "$fw" "$fh" "$out/gs_$1_eol$3.pdf" $3
  done
  rm -rf "$t"
  ;;
run)
  shift
  for f in "$@"; do
    t=$(mktemp -d)
    pdfimages -list "$f" 2>/dev/null > "$t/list" || true
    pdfimages -png "$f" "$t/p" 2>/dev/null || true
    mkdir -p "$t/ours"
    timeout 900 /tmp/pdfimagecheck "$f" "$t/ours" > "$t/ours.log" 2>&1 || echo "TIMEOUT-OR-CRASH" >> "$t/ours.log"
    python3 -I "$here/pdfimagecompare.py" "$f" "$t" || echo "$(basename "$f")  compare-crashed"
    rm -rf "$t"
  done
  ;;
esac
