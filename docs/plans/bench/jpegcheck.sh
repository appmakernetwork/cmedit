#!/bin/sh
# JPEG conformance against libjpeg-turbo, through Pillow (plan 0035).
#   docs/plans/bench/jpegcheck.sh corpus PHOTO OUTDIR    # jpeg_gen.py: 141 files
#   docs/plans/bench/jpegcheck.sh run BIN [--raw | --scale N] FILE...  # dump + compare
# BIN is an ImageDecode.hs build (imgbench.sh build). `run` dumps every
# file's RGBA (or, with --raw, decodeJPEGSamples' samples) into a temporary
# directory and prints jpeg_cmp.py's per-file line; exit 1 on any difference.
# --scale N (2, 4, 8) decodes at 1/N (decodeJPEGScaled) and compares with
# Pillow's scaled decode (Image.draft).
# jpeg_ref.py FILE [SCALE] is the executable specification the decoder
# follows (ISLOW IDCT, fancy upsampling, the colour tables, libjpeg's
# colour-space rule) checked against Pillow on its own.
set -e
here=$(cd "$(dirname "$0")" && pwd)
case "$1" in
corpus) python3 -I "$here/jpeg_gen.py" "$2" "$3" ;;
run)
  bin=$2; shift 2; raw=; scale=1
  if [ "$1" = --raw ]; then raw=--raw; shift; fi
  if [ "$1" = --scale ]; then scale=$2; shift 2; fi
  t=$(mktemp -d)
  for f in "$@"; do
    if [ -n "$raw" ]; then IMAGE_RAW=1 IMAGE_RGBA_DIR=$t "$bin" "$f" >/dev/null
    else IMAGE_SCALE=$scale IMAGE_RGBA_DIR=$t "$bin" "$f" >/dev/null; fi
  done
  sc=; [ "$scale" != 1 ] && sc="--scale $scale"
  st=0; python3 -I "$here/jpeg_cmp.py" $raw $sc "$t" "$@" || st=$?
  rm -rf "$t"; exit $st ;;
*) echo "usage: $0 corpus PHOTO OUTDIR | run BIN [--raw | --scale N] FILE..." >&2; exit 2 ;;
esac
