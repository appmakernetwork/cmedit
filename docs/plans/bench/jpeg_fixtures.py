"""Regenerate the small JPEG fixtures test/Spec.hs embeds for plan 0035
(each layout and colour rule the decoder distinguishes), and print each
one's expected fingerprint: FNV-1a 64 over libjpeg-turbo's decode (Pillow's
RGB, alpha 255), the same fold Spec computes over imgPix.
Usage: jpeg_fixtures.py C420.JPG OUTDIR [EXTRA.jpg...]   (C420.JPG: the
bytes of Spec's jpegColour420; needs Pillow and ImageMagick's `magick`).
Writes OUTDIR/<name>.jpg and prints `name size fingerprint` lines plus each
file as a Haskell list literal, then the fingerprints of libjpeg-turbo's
decode at 1/2, 1/4 and 1/8 (decodeJPEGScaled, plan 0035 phase 2) of each
generated file and each EXTRA (Spec's other fixtures, written out as files).
A scaled decode is Pillow's at that scale, set as Image.draft sets it (scale
1/N, the tile and size rounded up); draft itself cannot reach 1/N on a
picture under N*N pixels a side, and these are all small (jpeg_cmp.py
--scale checks draft and this agree wherever draft can)."""
import os, subprocess, sys
from PIL import Image, ImageFile

src, out, extra = sys.argv[1], sys.argv[2], sys.argv[3:]
os.makedirs(out, exist_ok=True)
pic = Image.open(src).convert('RGB')          # 32x24
small = pic.crop((5, 4, 25, 18))               # 20x14: no layout's MCU divides it
png = os.path.join(out, 'small.png'); small.save(png)

def magick(name, *args):
    subprocess.run(['magick', png, *args, '-quality', '50', os.path.join(out, name)], check=True)

small.save(os.path.join(out, 'h2v1.jpg'), quality=50, subsampling=1)     # 4:2:2, fancy 2x1
magick('h1v2.jpg', '-sampling-factor', '1x2,1x1,1x1')                     # 4:4:0, fancy 1x2
magick('box.jpg', '-sampling-factor', '3x2,1x1,1x1')                       # replication
small.save(os.path.join(out, 'rgb.jpg'), quality=50, subsampling=0, keep_rgb=True)   # Adobe 0, ids R G B
magick('cmyk.jpg', '-colorspace', 'CMYK')                                 # YCCK (Adobe 2), C and K 2x2
small.save(os.path.join(out, 'progrst.jpg'), quality=50, subsampling=2,    # progressive, RST every unit
           progressive=True, restart_marker_blocks=1)
small.save(os.path.join(out, 'ycc444.jpg'), quality=50, subsampling=0)    # YCbCr 4:4:4

def fnv(data):
    h = 14695981039346656037
    for b in data:
        h = ((h ^ b) * 1099511628211) & 0xFFFFFFFFFFFFFFFF
    return h

names = ['h2v1', 'h1v2', 'box', 'rgb', 'cmyk', 'progrst', 'ycc444']
for name in names:
    p = os.path.join(out, name + '.jpg')
    im = Image.open(p)
    print(name, im.size, im.mode, getattr(im, 'layer', None), os.path.getsize(p),
          '0x%016x' % fnv(im.convert('RGBA').tobytes()))
for name in names:
    data = open(os.path.join(out, name + '.jpg'), 'rb').read()
    print('--', name)
    for i in range(0, len(data), 16):
        print('  ' + (', ' if i else '[ ') + ','.join(str(b) for b in data[i:i + 16]))
    print('  ]')

def open_scaled(p, s):
    im = Image.open(p)
    w, h = im.size
    d, e, o, a = im.tile[0]
    e = (e[0], e[1], (e[2] - e[0] + s - 1) // s + e[0], (e[3] - e[1] + s - 1) // s + e[1])
    im.tile = [ImageFile._Tile(d, e, o, a)]
    im._size = (-(-w // s), -(-h // s))
    im.decoderconfig = (s, 0)
    return im

print('-- scaled: name, then (size, fingerprint) at 1/2, 1/4, 1/8')
for p in [os.path.join(out, n + '.jpg') for n in names] + extra:
    fps = []
    for s in (2, 4, 8):
        im = open_scaled(p, s)
        px = im.convert('RGBA').tobytes()
        assert len(px) == im.size[0] * im.size[1] * 4
        fps.append('%dx%d 0x%016x' % (im.size[0], im.size[1], fnv(px)))
    print(os.path.basename(p), ' | '.join(fps))
