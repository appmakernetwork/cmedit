"""Compare decoder dumps against Pillow (libjpeg-turbo), per file (plan 0035).
Usage: jpeg_cmp.py [--raw | --scale N] DUMPDIR FILE.jpg...
DUMPDIR holds <basename>.rgba files from ImageDecode.hs (IMAGE_RGBA_DIR=DUMPDIR;
with IMAGE_RAW=1 they are decodeJPEGSamples output and --raw compares the
component samples: grey to L, three components to RGB, CMYK to Pillow's CMYK
array, which is the complement of the stored samples for an Adobe file).
Without --raw the display RGB is compared with Pillow's convert('RGB').
With --scale N (2, 4 or 8; dumps from IMAGE_SCALE=N) Pillow decodes at 1/N:
Image.draft('RGB', (ceil(w/N), ceil(h/N))), checked to have picked 1/N by the
size it reports and the decoder configuration it sets ("draft"). Draft picks
the scale as min(w // size_w, h // size_h), which for an odd side rounds down
(4753 // 2377 = 1), so next it is asked for (w // N, h // N), which picks 1/N
whenever both sides are at least N ("draft-floor"; the size is still the
rounded-up one). For a smaller side the same configuration (scale 1/N, the
tile and size rounded up) is set directly ("forced").
Prints one line per file; exits 1 if any file differs."""
import sys, os
import numpy as np
from PIL import Image, ImageFile
args = sys.argv[1:]
raw = args and args[0] == '--raw'
if raw: args = args[1:]
scale = 1
if args and args[0] == '--scale': scale = int(args[1]); args = args[2:]
dd, files = args[0], args[1:]

def open_scaled(f, s):
    """Pillow's decode of f at 1/s, and how the scale was set."""
    im = Image.open(f)
    if s == 1: return im, ''
    w, h = im.size; want = (-(-w // s), -(-h // s))
    im.draft('RGB', want)
    if im.size == want and im.decoderconfig == (s, 0): return im, 'draft'
    im = Image.open(f)
    im.draft('RGB', (max(1, w // s), max(1, h // s)))
    if im.size == want and im.decoderconfig == (s, 0): return im, 'draft-floor'
    im = Image.open(f)
    d, e, o, a = im.tile[0]
    e = (e[0], e[1], (e[2] - e[0] + s - 1) // s + e[0], (e[3] - e[1] + s - 1) // s + e[1])
    im.tile = [ImageFile._Tile(d, e, o, a)]
    im._size = want
    im.decoderconfig = (s, 0)
    return im, 'forced'

bad = 0
for f in files:
    n = os.path.basename(f); p = os.path.join(dd, n + '.rgba')
    if not os.path.exists(p): print('%-28s NO-DUMP' % n); bad += 1; continue
    im, how = open_scaled(f, scale); mode = im.mode
    if os.path.getsize(p) != im.size[0] * im.size[1] * 4:
        print('%-28s SIZE: dump is %d bytes, Pillow %dx%d' % (n, os.path.getsize(p), im.size[0], im.size[1]))
        bad += 1; continue
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
    print('%-28s %-4s %-16s max %3d mean %.4f  !=0 %5.1f%% %s' % (n, mode, lay, d.max(), d.mean(), (d > 0).mean() * 100, how))
    bad += d.max() > 0
sys.exit(1 if bad else 0)
