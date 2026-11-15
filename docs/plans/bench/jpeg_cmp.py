"""Compare decoder dumps against Pillow (libjpeg-turbo), per file (plan 0035).
Usage: jpeg_cmp.py [--raw] DUMPDIR FILE.jpg...
DUMPDIR holds <basename>.rgba files from ImageDecode.hs (IMAGE_RGBA_DIR=DUMPDIR;
with IMAGE_RAW=1 they are decodeJPEGSamples output and --raw compares the
component samples: grey to L, three components to RGB, CMYK to Pillow's CMYK
array, which is the complement of the stored samples for an Adobe file).
Without --raw the display RGB is compared with Pillow's convert('RGB').
Prints one line per file; exits 1 if any file differs."""
import sys, os
import numpy as np
from PIL import Image
args = sys.argv[1:]
raw = args and args[0] == '--raw'
if raw: args = args[1:]
dd, files = args[0], args[1:]
bad = 0
for f in files:
    n = os.path.basename(f); p = os.path.join(dd, n + '.rgba')
    if not os.path.exists(p): print('%-28s NO-DUMP' % n); bad += 1; continue
    im = Image.open(f); mode = im.mode
    if raw:
        ref = np.asarray(im).astype(np.int16)
        if ref.ndim == 2: ref = ref[:, :, None]
        k = ref.shape[2]
        h, w = ref.shape[:2]
        o = np.fromfile(p, dtype=np.uint8).reshape(h, w, 4)[:, :, :k].astype(np.int16)
        if mode == 'CMYK' and 'adobe' in im.info: o = 255 - o
    else:
        ref = np.asarray(im.convert('RGB')).astype(np.int16); h, w = ref.shape[:2]
        o = np.fromfile(p, dtype=np.uint8).reshape(h, w, 4)[:, :, :3].astype(np.int16)
    d = np.abs(o - ref)
    lay = ','.join('%dx%d' % (c[1], c[2]) for c in getattr(im, 'layer', []))
    print('%-28s %-4s %-16s max %3d mean %.4f  !=0 %5.1f%%' % (n, mode, lay, d.max(), d.mean(), (d > 0).mean() * 100))
    bad += d.max() > 0
sys.exit(1 if bad else 0)
