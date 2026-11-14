#!/usr/bin/env python3
"""Synthetic feature corpus for plan 0034 workstream E (Cmedit.PdfRender).

Writes small hand-built PDFs, one per feature group, into the output
directory (default docs/plans/bench/pdf/). Everything is generated: no
third-party content. pdfrendercheck.sh renders each with pdftoppm and with
our pipeline and compares.

    python3 -I docs/plans/bench/gen_pdfrender.py [outdir]

Further fixtures are produced by pdfrendercheck.sh's `synth` step with the
tools installed on the machine (ps2pdf, soffice, rsvg-convert, convert);
those are not committed.
"""
import os
import sys
import zlib


class Pdf:
    def __init__(self):
        self.objs = {}

    def add(self, num, body):
        self.objs[num] = body if isinstance(body, bytes) else body.encode('latin-1')

    def stream(self, num, dict_, data, compress=True):
        if isinstance(data, str):
            data = data.encode('latin-1')
        if compress:
            data = zlib.compress(data)
            dict_ = dict_ + ' /Filter /FlateDecode'
        self.add(num, ('<< %s /Length %d >>\nstream\n' % (dict_, len(data))).encode('latin-1') + data + b'\nendstream')

    def write(self, path, root=1):
        out = bytearray(b'%PDF-1.7\n%\xe2\xe3\xcf\xd3\n')
        offs = {}
        for n in sorted(self.objs):
            offs[n] = len(out)
            out += b'%d 0 obj\n' % n + self.objs[n] + b'\nendobj\n'
        xref = len(out)
        top = max(self.objs) + 1
        out += b'xref\n0 %d\n0000000000 65535 f \n' % top
        for n in range(1, top):
            out += (b'%010d 00000 n \n' % offs[n]) if n in offs else b'0000000000 65535 f \n'
        out += b'trailer\n<< /Size %d /Root %d 0 R >>\nstartxref\n%d\n%%%%EOF\n' % (top, root, xref)
        with open(path, 'wb') as f:
            f.write(out)


def page_doc(content, resources='', media='[0 0 300 300]', extra_page='', extra_objs=None, catalog_extra=''):
    p = Pdf()
    p.add(1, '<< /Type /Catalog /Pages 2 0 R %s >>' % catalog_extra)
    p.add(2, '<< /Type /Pages /Kids [3 0 R] /Count 1 >>')
    p.add(3, '<< /Type /Page /Parent 2 0 R /MediaBox %s /Resources << %s >> /Contents 4 0 R %s >>'
          % (media, resources, extra_page))
    p.stream(4, '', content)
    for (n, body) in (extra_objs or []):
        if isinstance(body, tuple):
            p.stream(n, body[0], body[1], body[2] if len(body) > 2 else True)
        else:
            p.add(n, body)
    return p


STD_FONTS = ('/Font << /F1 10 0 R /F2 11 0 R /F3 12 0 R /F4 13 0 R >>',
             [(10, '<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>'),
              (11, '<< /Type /Font /Subtype /Type1 /BaseFont /Times-Roman /Encoding /WinAnsiEncoding >>'),
              (12, '<< /Type /Font /Subtype /Type1 /BaseFont /Courier >>'),
              (13, '<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica-Bold >>')])


def gen(out):
    os.makedirs(out, exist_ok=True)

    # 1. Paths, strokes, dashes, caps and joins, fill rules, clipping.
    c = []
    c.append('0.2 0.4 0.8 rg 20 20 100 60 re f')
    c.append('1 0 0 RG 6 w 1 J 1 j 30 120 m 80 180 l 130 120 l S')
    c.append('0 0.6 0 RG 4 w 2 J 0 j [12 6] 0 d 150 20 m 280 90 l S [] 0 d')
    c.append('0.9 0.6 0 rg 160 120 m 200 220 l 240 120 l 150 180 l 250 180 l h f*')
    c.append('q 20 210 80 70 re W n 0.5 0 0.5 rg 60 245 50 0 360 arc_placeholder Q')
    c.append('0 g 0.5 w 20 200 m 60 280 100 200 140 280 c S')
    c.append('0.3 g 200 250 m 260 300 v 290 220 y h B')
    content = '\n'.join(c).replace('60 245 50 0 360 arc_placeholder', circle(60, 245, 50) + ' f')
    page_doc(content).write(os.path.join(out, 'paths.pdf'))

    # 2. Text operators with the standard 14 (not embedded: substituted).
    res, objs = STD_FONTS
    t = ['BT /F1 18 Tf 20 260 Td (Helvetica Tj) Tj ET',
         'BT /F2 18 Tf 20 235 Td [(Times) -500 (TJ kerned) 200 (A)] TJ ET',
         'BT /F3 12 Tf 20 215 Td 2 Tc (Courier Tc 2) Tj 0 Tc ET',
         'BT /F1 12 Tf 20 195 Td 8 Tw (word spacing Tw 8) Tj 0 Tw ET',
         'BT /F1 12 Tf 20 175 Td 150 Tz (horizontal scale 150) Tj 100 Tz ET',
         'BT /F1 12 Tf 20 155 Td (rise ) Tj 5 Ts (up) Tj -4 Ts (down) Tj 0 Ts ET',
         'BT /F4 16 Tf 1 Tr 0.5 w 0 0 1 RG 20 130 Td (Stroked text) Tj 0 Tr ET',
         'BT /F4 16 Tf 2 Tr 1 0 0 rg 0 0 0 RG 0.4 w 20 108 Td (Fill+stroke) Tj 0 Tr ET',
         'BT /F2 14 Tf 0.7071 0.7071 -0.7071 0.7071 200 40 Tm (Rotated) Tj ET',
         'BT /F1 11 Tf 14 TL 160 260 Td (Line one) Tj T* (Line two) Tj (Line three) \' ET',
         'BT /F1 11 Tf 160 215 Td 3 1 (Quote op) " ET',
         'q BT /F4 40 Tf 7 Tr 20 40 Td (CLIP) Tj ET 0 0.7 0.3 rg 0 0 300 300 re f Q',
         'BT /F1 10 Tf 3 Tr 20 20 Td (invisible) Tj ET']
    page_doc('\n'.join(t), res, extra_objs=objs).write(os.path.join(out, 'text.pdf'))

    # 3. Rotation and crop boxes.
    for rot in (90, 180, 270):
        page_doc('0 0 1 rg 20 20 100 50 re f 1 0 0 rg 200 250 60 30 re f\n'
                 'BT /F1 20 Tf 30 150 Td (Rotate %d) Tj ET' % rot, res, media='[0 0 300 200]',
                 extra_page='/Rotate %d /CropBox [10 10 290 190]' % rot, extra_objs=objs) \
            .write(os.path.join(out, 'rotate%d.pdf' % rot))

    # 4. Shadings: axial/radial with Extend variants, function-based, a
    # stitched function, sh with and without BBox, a shading pattern.
    fn = '<< /FunctionType 2 /Domain [0 1] /C0 [1 0 0] /C1 [0 0 1] /N 1 >>'
    stitch = ('<< /FunctionType 3 /Domain [0 1] /Bounds [0.5] /Encode [0 1 0 1] /Functions ['
              '<< /FunctionType 2 /Domain [0 1] /C0 [1 1 0] /C1 [0 1 0] /N 1 >> '
              '<< /FunctionType 2 /Domain [0 1] /C0 [0 1 0] /C1 [0 0 0.5] /N 2 >> ] >>')
    shres = ('/Shading << /S1 20 0 R /S2 21 0 R /S3 22 0 R /S4 23 0 R /S5 24 0 R >> '
             '/Pattern << /P1 25 0 R >>')
    shobjs = [
        (20, '<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [20 0 130 0] /Function %s /Extend [false false] >>' % fn),
        (21, '<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [170 0 280 0] /Function %s /Extend [true true] >>' % stitch),
        (22, '<< /ShadingType 3 /ColorSpace /DeviceRGB /Coords [75 200 5 90 215 50] /Function %s /Extend [true false] >>' % fn),
        (23, '<< /ShadingType 3 /ColorSpace /DeviceRGB /Coords [225 200 0 225 200 50] /Function %s /Extend [false true] /BBox [170 150 280 250] >>' % stitch),
        (24, ('<< /ShadingType 1 /ColorSpace /DeviceRGB /Domain [0 1 0 1] /Matrix [100 0 0 50 20 20] /Function 26 0 R >>')),
        (25, '<< /PatternType 2 /Shading 27 0 R /Matrix [1 0 0 1 0 0] >>'),
        (26, ('/FunctionType 4 /Domain [0 1 0 1] /Range [0 1 0 1 0 1]', '{ 2 copy mul 3 1 roll }', False)),
        (27, '<< /ShadingType 2 /ColorSpace /DeviceGray /Coords [170 20 280 70] /Function << /FunctionType 2 /Domain [0 1] /C0 [0] /C1 [1] /N 1 >> >>'),
    ]
    sc = ['q 20 260 110 30 re W n /S1 sh Q', 'q 160 260 130 30 re W n /S2 sh Q',
          'q 10 140 140 110 re W n /S3 sh Q', '/S4 sh', 'q 0 0 300 130 re W n /S5 sh Q',
          '/Pattern cs /P1 scn 170 20 110 50 re f']
    page_doc('\n'.join(sc), shres, extra_objs=shobjs).write(os.path.join(out, 'shadings.pdf'))

    # 5. Mesh shadings: free-form (4), lattice (5), Coons (6), tensor (7).
    def pack(vals_bits):
        bits = ''.join(format(v, '0%db' % b) for (v, b) in vals_bits)
        bits += '0' * (-len(bits) % 8)
        return bytes(int(bits[i:i + 8], 2) for i in range(0, len(bits), 8))
    def vtx(flag, x, y, rgb, fbits=8):
        r = pack(([(flag, fbits)] if flag is not None else []) + [(x, 16), (y, 16)] + [(int(c * 255), 8) for c in rgb])
        return r
    m4 = b''.join([vtx(0, 1000, 1000, (1, 0, 0)), vtx(0, 30000, 1000, (0, 1, 0)), vtx(0, 15000, 25000, (0, 0, 1)),
                   vtx(1, 32000, 22000, (1, 1, 0))])
    m5 = b''.join([vtx(None, x, y, c) for (x, y, c) in
                   [(35000, 1000, (1, 0, 0)), (50000, 1000, (0, 1, 0)), (65000, 3000, (0, 0, 1)),
                    (35000, 20000, (1, 1, 0)), (50000, 24000, (0, 1, 1)), (65000, 20000, (1, 0, 1))]])
    def patch(flag, pts, cols, ty=6):
        vals = [(flag, 8)]
        for (x, y) in pts:
            vals += [(x, 16), (y, 16)]
        for c in cols:
            vals += [(int(v * 255), 8) for v in c]
        return pack(vals)
    coons_pts = [(1000, 30000), (1000, 40000), (1000, 50000), (1000, 60000), (10000, 64000), (20000, 56000),
                 (30000, 60000), (30000, 50000), (34000, 40000), (30000, 30000), (20000, 34000), (10000, 26000)]
    m6 = patch(0, coons_pts, [(1, 0, 0), (0, 1, 0), (0, 0, 1), (1, 1, 0)])
    tensor_pts = [(x + 34000, y) for (x, y) in coons_pts] + [(45000, 40000), (45000, 50000), (55000, 50000), (55000, 40000)]
    m7 = patch(0, tensor_pts, [(0, 1, 1), (1, 0, 1), (1, 1, 1), (0, 0, 0)])
    dec = '/Decode [0 300 0 300 0 1 0 1 0 1]'
    meshres = '/Shading << /M4 30 0 R /M5 31 0 R /M6 32 0 R /M7 33 0 R >>'
    meshobjs = [
        (30, ('/ShadingType 4 /ColorSpace /DeviceRGB /BitsPerCoordinate 16 /BitsPerComponent 8 /BitsPerFlag 8 ' + dec, m4)),
        (31, ('/ShadingType 5 /ColorSpace /DeviceRGB /BitsPerCoordinate 16 /BitsPerComponent 8 /VerticesPerRow 3 ' + dec, m5)),
        (32, ('/ShadingType 6 /ColorSpace /DeviceRGB /BitsPerCoordinate 16 /BitsPerComponent 8 /BitsPerFlag 8 ' + dec, m6)),
        (33, ('/ShadingType 7 /ColorSpace /DeviceRGB /BitsPerCoordinate 16 /BitsPerComponent 8 /BitsPerFlag 8 ' + dec, m7)),
    ]
    page_doc('/M4 sh /M5 sh /M6 sh /M7 sh', meshres, extra_objs=meshobjs).write(os.path.join(out, 'meshes.pdf'))

    # 6. Tiling patterns: coloured and uncoloured, scaled and rotated.
    tile1 = '1 0 0 rg 0 0 10 10 re f 0 0 1 rg 10 10 10 10 re f'
    tile2 = '0 0 m 20 20 l 20 0 m 0 20 l S'
    patres = ('/Pattern << /T1 40 0 R /T2 41 0 R >> /ColorSpace << /CsP [/Pattern /DeviceRGB] >>')
    patobjs = [
        (40, ('/PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 20 20] /XStep 20 /YStep 20 /Resources << >> /Matrix [1 0 0 1 0 0]', tile1)),
        (41, ('/PatternType 1 /PaintType 2 /TilingType 1 /BBox [0 0 20 20] /XStep 20 /YStep 20 /Resources << >> /Matrix [0.8 0.5 -0.5 0.8 0 0]', tile2)),
    ]
    pc = ['/Pattern cs /T1 scn 20 20 120 120 re f',
          '/CsP cs 0 0.6 0 /T2 scn 160 20 120 120 re f',
          'q 1 0 0 1 0 150 cm 0.5 0 0 0.5 0 0 cm /Pattern cs /T1 scn 40 40 200 200 re f Q',
          '/CsP CS 0.8 0 0.8 /T2 SCN 12 w 180 200 m 280 280 l S']
    page_doc('\n'.join(pc), patres, extra_objs=patobjs).write(os.path.join(out, 'patterns.pdf'))

    # 7. Images: RGB, gray with /Decode, stencil masks (pattern and colour),
    # an SMask, an inline image, interpolation.
    rgb = bytes(v for y in range(8) for x in range(8) for v in ((x * 32) % 256, (y * 32) % 256, 128))
    gray = bytes(((x + y) * 16) % 256 for y in range(16) for x in range(16))
    mask = bytes([0b11110000, 0b00001111] * 8)
    smask = bytes(int(255 * x / 7) for y in range(8) for x in range(8))
    imres = '/XObject << /I1 50 0 R /I2 51 0 R /M1 52 0 R /I3 53 0 R >> /Pattern << /T1 40 0 R >>'
    imobjs = [
        (50, ('/Type /XObject /Subtype /Image /Width 8 /Height 8 /ColorSpace /DeviceRGB /BitsPerComponent 8', rgb)),
        (51, ('/Type /XObject /Subtype /Image /Width 16 /Height 16 /ColorSpace /DeviceGray /BitsPerComponent 8 /Decode [1 0] /Interpolate true', gray)),
        (52, ('/Type /XObject /Subtype /Image /Width 16 /Height 8 /ImageMask true /BitsPerComponent 1', mask)),
        (53, ('/Type /XObject /Subtype /Image /Width 8 /Height 8 /ColorSpace /DeviceRGB /BitsPerComponent 8 /SMask 54 0 R', rgb)),
        (54, ('/Type /XObject /Subtype /Image /Width 8 /Height 8 /ColorSpace /DeviceGray /BitsPerComponent 8', smask)),
        (40, ('/PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 20 20] /XStep 20 /YStep 20 /Resources << >>', tile1)),
    ]
    inline = ('q 80 0 0 40 200 20 cm BI /W 4 /H 2 /CS /RGB /BPC 8 ID\n'.encode('latin-1')
              + bytes([255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 0, 0, 255, 255, 255, 0, 255, 0, 0, 0, 128, 128, 128])
              + b'\nEI Q\n')
    ic = ('q 100 0 0 100 20 180 cm /I1 Do Q\n'
          'q 100 0 0 100 160 180 cm /I2 Do Q\n'
          'q 0.8 0.1 0.1 rg 100 0 0 50 20 100 cm /M1 Do Q\n'
          'q /Pattern cs /T1 scn 100 0 0 50 160 100 cm /M1 Do Q\n'
          'q 1 1 0 rg 20 20 100 60 re f 100 0 0 60 20 20 cm /I3 Do Q\n').encode('latin-1') + inline
    p = page_doc('', imres, extra_objs=imobjs)
    p.stream(4, '', ic)
    p.write(os.path.join(out, 'images.pdf'))

    # 8. Transparency: ca/CA, blend modes, a luminosity soft mask with a
    # gradient, a knockout group, an isolated group, B with alpha.
    tres = ('/ExtGState << /A1 << /ca 0.5 /CA 0.5 >> /BM1 << /BM /Multiply >> /BM2 << /BM [/Bogus /Screen] >> '
            '/SM << /SMask << /S /Luminosity /G 60 0 R >> >> /SN << /SMask /None >> >> '
            '/XObject << /K 61 0 R /G 62 0 R >>')
    grad = '<< /ShadingType 2 /ColorSpace /DeviceGray /Coords [160 0 280 0] /Function << /FunctionType 2 /Domain [0 1] /C0 [1] /C1 [0] /N 1 >> >>'
    tobjs = [
        (60, ('/Type /XObject /Subtype /Form /BBox [0 0 300 300] /Group << /S /Transparency /CS /DeviceGray >> /Resources << /Shading << /S 63 0 R >> >>', '/S sh')),
        (63, grad),
        (61, ('/Type /XObject /Subtype /Form /BBox [0 0 300 300] /Group << /S /Transparency /K true >> /Resources << /ExtGState << /A << /ca 0.5 >> >> >>',
              '/A gs 1 0 0 rg 20 20 60 60 re f 0 0 1 rg 50 50 60 60 re f')),
        (62, ('/Type /XObject /Subtype /Form /BBox [0 0 300 300] /Group << /S /Transparency /I true >> /Resources << /ExtGState << /M << /BM /Multiply >> >> >>',
              '/M gs 0 1 0 rg 160 20 60 60 re f 1 0 1 rg 190 50 60 60 re f')),
    ]
    tc = ['0.9 0.9 0.2 rg 0 150 300 150 re f',
          '/A1 gs 1 0 0 rg 0 0 1 RG 10 w 30 170 80 80 re B',
          '/BM1 gs 0 0.5 1 rg 80 200 80 80 re f',
          '/BM2 gs 1 0 0 rg 150 160 60 60 re f 1 0 0 1 0 0 cm',
          'q /SM gs 0 0 0.6 rg 160 220 120 70 re f Q',
          'q 0.5 g /K Do Q q /G Do Q']
    page_doc('\n'.join(tc), tres, extra_objs=tobjs).write(os.path.join(out, 'transparency.pdf'))

    # 9. Forms (matrix, bbox clip, resource inheritance), optional content
    # and annotations.
    fres = ('/XObject << /Fm 70 0 R /Hid 71 0 R >> /Properties << /OCoff 72 0 R /OCon 73 0 R >> ' + res)
    fobjs = objs + [
        (70, ('/Type /XObject /Subtype /Form /BBox [0 0 50 50] /Matrix [1 0 0 1 0 0]', '0 0 1 rg 0 0 80 80 re f 1 g 10 10 30 30 re f')),
        (71, ('/Type /XObject /Subtype /Form /BBox [0 0 50 50] /OC 72 0 R', '1 0 0 rg 0 0 50 50 re f')),
        (72, '<< /Type /OCG /Name (Off) >>'),
        (73, '<< /Type /OCG /Name (On) >>'),
        (74, '<< /Type /Annot /Subtype /Square /Rect [150 150 290 290] /AP << /N 75 0 R >> >>'),
        (75, ('/Type /XObject /Subtype /Form /BBox [0 0 20 10] /Matrix [0 1 -1 0 0 0]', '0 0.6 0 rg 0 0 20 10 re f 1 g 2 2 6 6 re f')),
        (76, '<< /Type /Annot /Subtype /Square /F 2 /Rect [10 10 50 50] /AP << /N 77 0 R >> >>'),
        (77, ('/Type /XObject /Subtype /Form /BBox [0 0 10 10]', '1 0 0 rg 0 0 10 10 re f')),
        (78, '<< /Type /Annot /Subtype /Widget /Rect [20 230 120 260] /AS /On /AP << /N << /On 79 0 R /Off 77 0 R >> >> >>'),
        (79, ('/Type /XObject /Subtype /Form /BBox [0 0 100 30] /Resources << /Font << /F1 10 0 R >> >>', '0.9 g 0 0 100 30 re f 0 g BT /F1 12 Tf 10 10 Td (Checked) Tj ET')),
        (80, '<< /OCGs [72 0 R 73 0 R] /D << /OFF [72 0 R] >> >>'),
    ]
    fc = ['q 1 0 0 1 20 20 cm /Fm Do Q', 'q 2 0 0 2 60 80 cm /Fm Do Q',
          'q 1 0 0 1 200 20 cm /Hid Do Q',
          '/OC /OCoff BDC 1 0 0 rg 200 80 50 50 re f EMC',
          '/OC /OCon BDC 0 0.5 0 rg 200 80 20 20 re f EMC',
          'BT /F1 10 Tf 20 200 Td (Forms, OC, annotations) Tj ET']
    page_doc('\n'.join(fc), fres, extra_page='/Annots [74 0 R 76 0 R 78 0 R]', extra_objs=fobjs,
             catalog_extra='/OCProperties 80 0 R').write(os.path.join(out, 'forms.pdf'))

    # 10. A Type 3 font: a d0 glyph with its own colour, a d1 (stencil)
    # glyph in the current colour, an image-mask glyph.
    t3res = '/Font << /T 90 0 R >>'
    t3objs = [
        (90, '<< /Type /Font /Subtype /Type3 /FontBBox [0 0 1000 1000] /FontMatrix [0.001 0 0 0.001 0 0] '
             '/CharProcs << /sq 91 0 R /tri 92 0 R /bm 93 0 R >> /Encoding << /Type /Encoding /Differences [65 /sq /tri /bm] >> '
             '/FirstChar 65 /LastChar 67 /Widths [1000 1000 1000] >>'),
        (91, ('', '1000 0 d0 0 0.6 0 rg 100 100 800 800 re f')),
        (92, ('', '1000 0 0 0 1000 1000 d1 1 0 0 rg 100 100 m 900 100 l 500 900 l f')),
        (93, ('', '1000 0 0 0 1000 1000 d1 q 800 0 0 800 100 100 cm BI /W 8 /H 8 /IM true /BPC 1 ID\n'
                  '\x81\x42\x24\x18\x18\x24\x42\x81\nEI Q', False)),
    ]
    page_doc('BT /T 40 Tf 20 200 Td (ABCABC) Tj 0 0 1 rg 20 120 Td 2 Tz 30 Tf (CCBBAA) Tj ET',
             t3res, extra_objs=t3objs).write(os.path.join(out, 'type3.pdf'))

    # 11. JPEG 2000 placeholder (the codestream is junk: only the filter
    # matters) and a corrupt image.
    page_doc('q 120 0 0 80 20 20 cm /J Do Q q 100 0 0 60 160 160 cm /C Do Q',
             '/XObject << /J 95 0 R /C 96 0 R >>',
             extra_objs=[(95, ('/Type /XObject /Subtype /Image /Width 10 /Height 10 /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /JPXDecode', b'\x00\x00\x00\x0cjP  ', False)),
                         (96, ('/Type /XObject /Subtype /Image /Width 10 /Height 10 /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /DCTDecode', b'\xff\xd8junk', False))]) \
        .write(os.path.join(out, 'placeholder.pdf'))


def circle(cx, cy, r):
    k = 0.5523 * r
    return ('%g %g m %g %g %g %g %g %g c %g %g %g %g %g %g c %g %g %g %g %g %g c %g %g %g %g %g %g c h'
            % (cx + r, cy, cx + r, cy + k, cx + k, cy + r, cx, cy + r,
               cx - k, cy + r, cx - r, cy + k, cx - r, cy,
               cx - r, cy - k, cx - k, cy - r, cx, cy - r,
               cx + k, cy - r, cx + r, cy - k, cx + r, cy))


if __name__ == '__main__':
    gen(sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.path.dirname(os.path.abspath(__file__)), 'pdf'))
