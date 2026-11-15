"""Check Cmedit.PdfObj's glyph-name encoding tables against the specification.

    pdftotext -layout pdfreference1.7.pdf ref.txt
    python3 -I docs/plans/bench/pdf_encodings_check.py ref.txt src/Cmedit/PdfObj.hs [glyphlist.txt]

The source of the tables is Appendix D of Adobe's PDF Reference, sixth
edition (PDF 1.7), which Adobe publishes freely and which ISO 32000-1 Annex D
reproduces: D.1 (the Latin character set: StandardEncoding, MacRomanEncoding,
WinAnsiEncoding), D.3 (MacExpertEncoding), D.4 (Symbol) and D.5
(ZapfDingbats). This script reads those tables out of the document's text
and compares every code of every table in the Haskell source with them. It
prints each disagreement and exits 1 if there is one that the appendix's own
footnotes do not account for.

With a third argument, Adobe's glyphlist.txt (the Adobe Glyph List, from
github.com/adobe-type-tools/agl-aglfn, BSD 3-clause), 'standardGlyphUnicode'
is checked against it as well: every name there must carry the list's first
code point.

The footnotes it applies (they are prose in the appendix, not table rows):
  * WinAnsiEncoding: every unused code above octal 40 is the bullet; octal
    240 is a second space and octal 255 a second hyphen.
  * MacRomanEncoding: octal 312 is a second space.
"""
import re
import sys

ref = open(sys.argv[1], encoding="utf-8").read()
hs = open(sys.argv[2], encoding="utf-8").read()


def section(start, end):
    i = ref.rindex(start)
    return ref[i:ref.index(end, i)]


def haskell_table(name):
    m = re.search(name + r" = encodingTable\s*\[(.*?)\]", hs, re.S)
    words = " ".join(re.findall(r'"([^"]*)"', m.group(1))).split()
    assert len(words) == 256, (name, len(words))
    return {i: w for i, w in enumerate(words) if w != "."}


CODE = r"(?:[0-7]{3}|—)"


def latin():
    """D.1: name, then the STD, MAC, WIN and PDF codes."""
    text = section("D.1 Latin Character Set and Encodings", "D.3 Expert Set and MacExpertEncoding")
    std, mac, win = {}, {}, {}
    codes = r"\s+".join(["(" + CODE + ")"] * 4)
    # A footnote digit may follow the name ("Euro 1", "Zcaron 2").
    row = re.compile(r"([A-Za-z][A-Za-z0-9]*)(?:\s+\d)?\s+" + codes + r"(?=\s|$)", re.M)
    for name, s, m, w, _pdf in row.findall(text):
        for table, code in ((std, s), (mac, m), (win, w)):
            if code != "—":
                table[int(code, 8)] = name
    return std, mac, win


def simple(start, end):
    """D.3 to D.5: name, then one code."""
    text = section(start, end)
    out = {}
    for name, code in re.findall(r"([A-Za-z][A-Za-z0-9]*)\s+([0-7]{3})(?=\s|$)", text, re.M):
        out[int(code, 8)] = name
    return out


std, mac, win = latin()
# The appendix's footnotes.
win[0o240] = "space"
win[0o255] = "hyphen"
for c in range(0o41, 256):
    win.setdefault(c, "bullet")
mac[0o312] = "space"

spec = {
    "standardEncodingNames": std,
    "macRomanEncodingNames": mac,
    "winAnsiEncodingNames": win,
    "macExpertEncodingNames": simple("D.3 Expert Set and MacExpertEncoding", "D.4 Symbol Set and Encoding"),
    "symbolEncodingNames": simple("D.4 Symbol Set and Encoding", "D.5 ZapfDingbats Set and Encoding"),
    "zapfDingbatsEncodingNames": simple("D.5 ZapfDingbats Set and Encoding", "APPENDIX E"),
}

bad = 0
for name, want in spec.items():
    have = haskell_table(name)
    diffs = [(c, want.get(c), have.get(c)) for c in range(256) if want.get(c) != have.get(c)]
    print("%-26s %3d codes in the appendix, %3d in the source, %d differ" % (name, len(want), len(have), len(diffs)))
    for c, w, h in diffs:
        print("    code %03o (%3d): appendix %s, source %s" % (c, c, w, h))
    bad += len(diffs)
if len(sys.argv) > 3:
    agl = {}
    for line in open(sys.argv[3], encoding="utf-8"):
        if line.startswith("#") or ";" not in line:
            continue
        name, cps = line.strip().split(";")
        agl[name] = int(cps.split()[0], 16)
    body = hs[hs.index("standardGlyphUnicode =\n"):]
    body = body[:body.index("]\n") + 1]
    pairs = re.findall(r'\("([^"]+)", 0x([0-9a-fA-F]+)\)', body)
    wrong = [(n, v, agl.get(n)) for n, v in pairs if agl.get(n) != int(v, 16)]
    print("%-26s %3d names, %d differ from the Adobe Glyph List" % ("standardGlyphUnicode", len(pairs), len(wrong)))
    for n, v, a in wrong:
        print("    %s: source %s, list %s" % (n, v, "%04x" % a if a is not None else "absent"))
    bad += len(wrong)
sys.exit(1 if bad else 0)
