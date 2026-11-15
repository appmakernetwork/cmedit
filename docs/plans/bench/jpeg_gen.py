"""Generate the JPEG conformance corpus (plan 0035): every sampling layout
libjpeg handles, odd sizes, q5..q100, progressive, restart (and both), RGB
and CMYK: 141 files.
Usage: jpeg_gen.py PHOTO OUTDIR   (PHOTO: any photograph >= 2017x1389;
needs Pillow and ImageMagick's `magick`). Writes OUTDIR/*.jpg and the three
PNG sources into OUTDIR/src/."""
import os, subprocess, random, sys
from PIL import Image
D=sys.argv[2]; S=os.path.join(D,'src'); os.makedirs(S,exist_ok=True)
src=Image.open(sys.argv[1]).convert('RGB')
ph=src.crop((1500,1000,1500+517,1000+389))          # odd size
random.seed(1)
noise=Image.frombytes('RGB',(131,77),bytes(random.randrange(256) for _ in range(131*77*3)))
# saturated edges: stresses chroma upsampling + range limit
edge=Image.new('RGB',(203,149))
px=edge.load()
cols=[(255,0,0),(0,255,0),(0,0,255),(255,255,0),(0,255,255),(255,0,255),(0,0,0),(255,255,255)]
for y in range(149):
    for x in range(203): px[x,y]=cols[((x//7)+(y//5))%8]
ph.save(f'{S}/ph.png'); noise.save(f'{S}/noise.png'); edge.save(f'{S}/edge.png')
srcs={'ph':ph,'noise':noise,'edge':edge}
# Pillow-written
for n,im in srcs.items():
    for ss,nm in [(0,'444'),(1,'422'),(2,'420')]:
        for q in (5,50,90,100):
            im.save(f'{D}/{n}-{nm}-q{q}.jpg',quality=q,subsampling=ss)
    im.save(f'{D}/{n}-420-prog.jpg',quality=85,subsampling=2,progressive=True)
    im.save(f'{D}/{n}-444-prog.jpg',quality=85,subsampling=0,progressive=True)
    im.save(f'{D}/{n}-420-rst.jpg',quality=85,subsampling=2,restart_marker_blocks=5)
    # progressive with restarts whose interval divides a scan's unit count
    # (every unit, 3 units, every MCU row): no restart follows a scan's
    # last unit, and a decoder that searches for one loses the next scan
    im.save(f'{D}/{n}-420-prog-rst1.jpg',quality=85,subsampling=2,progressive=True,restart_marker_blocks=1)
    im.save(f'{D}/{n}-444-prog-rst3.jpg',quality=85,subsampling=0,progressive=True,restart_marker_blocks=3)
    im.save(f'{D}/{n}-420-prog-rstrow.jpg',quality=85,subsampling=2,progressive=True,restart_marker_rows=1)
    im.save(f'{D}/{n}-420-opt.jpg',quality=85,subsampling=2,optimize=True)
    im.convert('L').save(f'{D}/{n}-gray-q85.jpg',quality=85)
    im.convert('L').save(f'{D}/{n}-gray-q100.jpg',quality=100)
    im.convert('CMYK').save(f'{D}/{n}-cmyk-q85.jpg',quality=85)
    im.save(f'{D}/{n}-rgbjfif-q90.jpg',quality=90,subsampling=0,keep_rgb=True)
# sizes 1..17 & non-multiples
for (w,h) in [(1,1),(2,3),(7,9),(8,8),(15,17),(16,16),(17,33),(31,15),(33,1),(1,40)]:
    c=ph.crop((0,0,w,h))
    for ss,nm in [(0,'444'),(1,'422'),(2,'420')]:
        c.save(f'{D}/sz{w}x{h}-{nm}.jpg',quality=90,subsampling=ss)
# ImageMagick: arbitrary sampling factors
for n in srcs:
    for sf in ['1x2','4x1','4x2','1x4','4x4','3x1','3x3','2x4','1x3','3x2','2x3','4x3']:
        subprocess.run(['magick',f'{S}/{n}.png','-sampling-factor',f'{sf},1x1,1x1','-quality','88',f'{D}/{n}-im{sf}.jpg'],check=False)
    # chroma sampled differently per component; luma not the max
    subprocess.run(['magick',f'{S}/{n}.png','-sampling-factor','2x2,2x1,1x2','-quality','88',f'{D}/{n}-im-mixed.jpg'],check=False)
    subprocess.run(['magick',f'{S}/{n}.png','-sampling-factor','1x1,2x2,2x2','-quality','88',f'{D}/{n}-im-lumasub.jpg'],check=False)
    subprocess.run(['magick',f'{S}/{n}.png','-colorspace','CMYK','-sampling-factor','2x2,1x1,1x1,2x2','-quality','88',f'{D}/{n}-im-cmyk420.jpg'],check=False)
    subprocess.run(['magick',f'{S}/{n}.png','-colorspace','CMYK','-quality','88',f'{D}/{n}-im-cmyk.jpg'],check=False)
    subprocess.run(['magick',f'{S}/{n}.png','-interlace','JPEG','-sampling-factor','2x1','-quality','70',f'{D}/{n}-im-prog422.jpg'],check=False)
print(len(os.listdir(D)))
