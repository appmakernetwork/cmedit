#!/bin/sh
# Font reader oracles (plan 0034 B): parse every system font, check every
# glyph's outline bbox against glyf headers / AFM files, widths against hmtx /
# AFM, and the URW .t1 fonts against their .otf twins.
#   docs/plans/bench/fontcheck.sh [DIR...]     (default /usr/share/fonts)
set -e
cd "$(dirname "$0")/../../.."
out=dist-fontcheck
mkdir -p $out
ghc --make docs/plans/bench/FontCheck.hs -isrc -O2 -rtsopts -outputdir $out -o $out/fontcheck \
  -package base -package bytestring -package array -package containers -package text \
  -package filepath -package directory \
  -XLambdaCase -XOverloadedStrings -XBangPatterns -XTupleSections \
  -XScopedTypeVariables -XMultiWayIf -XRecordWildCards -XNamedFieldPuns -w >/dev/null
exec $out/fontcheck "$@"
