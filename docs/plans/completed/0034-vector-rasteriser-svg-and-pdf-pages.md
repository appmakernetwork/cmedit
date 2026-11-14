# 0034 — A vector rasteriser: SVG pictures and true PDF pages

**Theme:** a from-scratch 2D vector stack (display list → anti-aliased
rasteriser), with two front-ends: SVG files open as pictures, and the PDF view
gains a **page view** that renders the real page (text in its real fonts,
vector graphics, embedded images, shadings, transparency) as pixels on
kitty-graphics terminals, with the existing sixel and half-block paths as
fallbacks.
**Status:** ✅ **RESOLVED 2026-11-14** (started 2026-11-13)

## Outcome

**Shipped.** SVG files open as pictures (Alt+T to the source; the buffer stays
the document), and the PDF view gains a page view (Alt+T between reflow and
page; `pdf-view = auto|page|reflow`, auto = page when the terminal draws
pixels), both shown as kitty or sixel pixels re-rasterised sharply at every
zoom, with the half-block cell picture as the floor on any terminal.

| Measure | Result |
|---|---|
| `make test` | 4619 passed, 0 failed; `make windows-check` clean; no new warnings |
| PTY suites | `pty_pdf`, `pty_svg` (new), `pty_journal`, `pty_session`, `pty_workspace` all pass |
| SVG vs resvg | 2987 / 2997 non-text system SVGs within 1 % AE (the rest: resvg's own bugs, an unsupported `feMorphology`, AA on tiny images); text files agree with librsvg |
| Raster vs resvg | 82 hand-built cases, none flagged; a bounds-checked (`RASTER_SAFE`) build renders 1344 hostile/edge cases with no out-of-range access |
| PDF pages vs pdftocairo | median 0.05 % AE, 96 % of 366 corpus pages ≤ 2 %, worst 3.6 % |
| PDF pages vs pdftoppm | median 2.5 % (Splash snaps glyphs and rules to whole pixels; pdftocairo and Ghostscript sit 3–8 % from it on the same pages) |
| Robustness | all 2421 corpus pages render under the bounds-checked rasteriser; every truncation/bit-flip fuzz case returns without an exception |
| PDF images vs pdfimages | 4601 / 4620 Flate/raw images exact (the 19: ICC profiles poppler applies); CCITT G3/G4 bit-exact against Ghostscript |
| Fonts | all 482 system faces parse (bar one bitmap-only emoji font); TrueType outline boxes exact, AFM widths exact |
| Page view latency (real PDFs, kitty) | prefetched PgDn ≈ 55 ms, cold ≈ 60 ms, zoom ≈ 60 ms; idle CPU 0 %; a 1299-page PDF opens and pages normally |
| Hostile PDFs (Fable's 22) | every file places or stops with a note within ~1 s; Ctrl+Q always exits within 15 ms; the 21 KB CID-width bomb opens at 14 MB (was 2.3 GB) |

**Process.** Six Opus workstreams (A raster, B fonts, C PDF objects/images,
D SVG, E PDF render, F integration) in isolated worktrees against a contract
(`Geom` + `Scene`) that a Fable review changed before anyone built on it;
five Fable milestone/release reviews, each of whose findings was fixed with a
regression test before the next stage (the notable ones: an out-of-bounds
write in coverage accumulation; XML entity and `<use>` amplification; masked
job forks that made cancellation block the UI; revert/parse races; prefetch
ping-pong; exponential expansion through shared PDF sub-streams; unfuelled
CID width tables; a lint shim that could fetch from the network).

**Known limits** (documented in the manual): JPEG 2000 and JBIG2 images are
labelled placeholders; predefined CJK CMaps are read as UCS-2 or Identity;
no ICC colour management; non-embedded fonts need system fonts (none ⇒ that
text is omitted with a note); SVG `foreignObject`, `feTurbulence`/lighting
filters and animation are not drawn; startup opens of very large files stay
synchronous as before.

## Ground rules (from CLAUDE.md, restated because every agent reads this first)

- **Boot libraries only, offline, GHC 9.0.2, `make` not `cabal`.** No new
  packages. Everything here is written from first principles, like the image
  decoders (`0032`, `0033`), whose GHC performance lessons apply to every hot
  loop below (`docs/plans/completed/0032-image-decoders-round-2.md`).
- **Pure core.** Every module in this plan except the driver glue is pure:
  `ByteString`/data in, data out, `ST` inside. IO (reading system fonts,
  background render jobs, emitting graphics) lives in `Cmedit.App`.
- **Never an exception, always a bound.** Malformed or hostile input yields
  `Left`, a partial result, or a placeholder; never a crash, a hang or
  unbounded memory. Every loop that a file controls is fuel-limited.
- **Graceful floor.** No capability reply ⇒ the portable path. Pixel output
  is gated on the kitty probe / sixel DA1 attribute exactly as the image view
  is; the half-block cell picture is always the fallback.
- **Portable.** Nothing outside `platform/posix` imports `unix`. System font
  discovery uses `directory` and knows the Linux, macOS and Windows font
  directories. Run `make windows-check` after driver changes.
- **Tests:** each workstream owns one spec module (`test/RasterSpec.hs`,
  `test/FontSpec.hs`, `test/PdfImageSpec.hs`, `test/SvgSpec.hs`,
  `test/PdfRenderSpec.hs`), called from `test/Spec.hs` with its `check`. Edit
  only your own spec module. Fixtures are built in code or checked in as
  bytes; the suite never shells out. Oracle comparisons against external
  renderers live in `docs/plans/bench/` scripts, not in `make test`.

## The architecture

```
             Cmedit.Geom      points, Affine, Rect, Seg/Path       (leaf, written)
                  │
             Cmedit.Scene     the display list: Cmd, Paint, ...    (leaf, written)
              ┌───┴────────────┬──────────────────┐
       Cmedit.Raster     Cmedit.Font*        Cmedit.PdfObj ── Cmedit.PdfImage / PdfFunc / Ccitt
       Scene → Image     TT/CFF/Type1        object layer          image XObjects, functions
              │          → glyph Paths       (split from Pdf)      colour spaces → RGBA
              │               │                    │                    │
              │          ┌────┴──────────┬─────────┴────────────────────┘
              │      Cmedit.Svg      Cmedit.PdfRender
              │      SVG → Scene     PDF page → Scene
              └──────────┴───────────────┴──→ Cmedit.App / Editor / Render integration
```

**The contract is `Cmedit.Geom` + `Cmedit.Scene`** (both in `src/`, both
read-before-you-start). A front-end parses once into a `Scene`; the rasteriser
draws it at any resolution and crop. That split is what makes zoom re-render
sharply, keeps front-ends free of pixels, and lets the expensive half (content
stream + fonts) be paid once per page. Changes to the contract go through the
coordinator, never unilaterally: four workstreams build on it at once.

Conventions fixed by the contract (see the module headers): PDF matrix order
`[a b c d e f]`; `composeA outer inner`; scene space is **y-down**, one unit
per CSS px / PDF point, origin top-left; every `Cmd` carries user→scene;
colours are straight-alpha sRGB doubles; clips and groups contain their
children.

## Workstreams

### A. `Cmedit.Raster`: Scene → Image

`renderScene :: RenderOpts -> Scene -> Image` where `RenderOpts` gives the
output size and the scene→device `Affine` (so a crop/zoom is just a different
matrix), a background colour, and a work budget. Requirements:

- Exact-area (or ≥16-level) anti-aliased coverage, **non-zero and even-odd
  correct for self-intersecting and overlapping paths**, sparse in memory
  (cells/scanline spans, never a full-resolution float mask per path).
- Curve flattening in device space with a fixed tolerance (~0.1–0.25 px),
  adaptive (recursive or parabola-approximation), with segment-count caps.
- A full stroker: joins (miter with limit, round, bevel), caps (butt, round,
  square), dashes with phase, degenerate segments, closed vs open subpaths,
  zero-width hairlines (one device pixel), stroking in user space then
  transforming (elliptical pens under non-uniform scale).
- Paints: solid, linear, two-point-conical radial (PDF type 3 semantics:
  larger `t` wins; SVG focal gradients map onto it), all `Spread`s, sampled
  image paints, tiling patterns (render the cell at device resolution once,
  repeat; cap the cell size), Gouraud triangles.
- Images: area-averaged minification (box or mip pre-filter), bilinear or
  nearest magnification, arbitrary affine (rotated/skewed images), stencil
  masks painted with any paint.
- Clips (scoped, nested = intersected; a rectangular axis-aligned clip should
  be a fast path, it is the common case), groups with opacity, all 16 blend
  modes (PDF/CSS formulas), soft masks (luminosity/alpha, backdrop,
  transfer), filters (call `fApply`), non-isolated groups (blend against the
  backdrop), and the `defaultGroup` fast path (draw children directly).
- Compositing in premultiplied linear-in-sRGB-values floating point
  (i.e. no gamma conversion; this is what PDF and SVG specify by default and
  what `pdftoppm`/`resvg` do), output straight RGBA `Image`.
- Bounds: output pixels ≤ `Image.maxPixels`; a work budget counting
  flattened segments + covered spans; group nesting depth cap; layer memory
  cap. Running out of budget returns what was drawn so far (and the caller
  can tell, e.g. a `Bool` or a note).
- Performance targets (at `-O2`): a 1-megapixel page with 10 000 glyph
  fills in ≤ 300 ms; a 64×64 icon in ≤ 2 ms. Measure; record numbers.
- Correctness: hand-computed coverage tests (a rectangle's exact area, a
  circle's area within 0.5 %, even-odd vs non-zero on a pentagram, dash
  lengths, join shapes), plus the oracle script
  `docs/plans/bench/rastercheck.sh` comparing against `resvg` on SVG
  fixtures with an RMSE/AE tolerance (renderers disagree on AA by ~1 %).

### B. `Cmedit.Font` (+ `FontTT`, `FontCFF`, `FontT1`, `FontSys`): glyph outlines

`parseFont :: ByteString -> Either String Font` sniffing TrueType (`glyf`),
OpenType-CFF, TrueType collections (first face, or by name), bare CFF
(PDF `FontFile3 /Type1C`, CID-keyed `CIDFontType0C` with FDArray/FDSelect,
and `/OpenType`), and Type1 (PFA/PFB/PDF `FontFile` with eexec, charstring
decryption, subrs, OtherSubrs flex/hint replacement, `seac`). A `Font` gives:
glyph count; units-per-em / font matrix; `glyphPath :: Font -> Int -> Path`
in glyph space (memo-friendly); advance widths; cmap subtables (TrueType
(3,1), (3,0), (1,0), (3,10) …); glyph-name → GID (post table, CFF charset,
Type1 CharStrings); built-in encoding (Type1/CFF); CID → GID for CID-keyed
CFF; family/style names. Composite TrueType glyphs (all flag variants,
2×2 transforms, point matching ignored with a note), CFF Type2 charstrings
(all operators incl. hintmask byte counting, `flex*`, subr bias, `seac`-like
`endchar`), Type1 `callothersubr`/`pop` for flex and hint replacement.
Malformed fonts: `Left` or an empty glyph, never an exception; charstring
interpreters are fuel-limited (subr recursion depth, op count).

`Cmedit.FontSys` (pure): given directory listings, choose substitute files for
the PDF standard 14 and for CSS generic/common families, by a table of known
filenames (Liberation, URW base35 / Nimbus, DejaVu, Noto, Arial/Times/Courier
New on Windows/macOS) with style matching (bold/italic). The IO that lists the
directories is the driver's.

### C. `Cmedit.PdfObj`, `Cmedit.PdfFunc`, `Cmedit.PdfImage`, `Cmedit.Ccitt`

1. **Split `Cmedit.Pdf`**: move the object layer (lexer, `Obj`/`Dict`,
   object scan, ObjStm expansion, trailer, `resolve`/`dictOf`/`lookD`/…,
   stream decoding and filters, content tokenizer, matrices, `Fuel`, the
   page tree, media box/rotation, contents concatenation, encodings and the
   glyph-name table) into `Cmedit.PdfObj` with an explicit export list.
   `Cmedit.Pdf` keeps text extraction and the reading view and must behave
   **identically** (all existing tests pass unchanged). `PdfDoc` gains what
   page rendering needs (the parsed `PdfFile` and the page dicts), kept lazy
   so the reflow view's cost does not change.
2. **`Cmedit.PdfFunc`**: PDF functions types 0 (sampled, with
   Encode/Decode/Size, multilinear), 2 (exponential), 3 (stitching), 4
   (PostScript calculator, a bounded stack machine), evaluated as
   `[Double] -> [Double]`.
3. **`Cmedit.PdfImage`**: image XObjects and inline images (incl. the
   abbreviated keys and filter names) → straight RGBA `Image` (or a stencil
   coverage image for `/ImageMask`). Colour spaces: DeviceGray/RGB/CMYK,
   CalGray/CalRGB, Lab, ICCBased (by `/N`, `/Alternate`), Indexed (any base,
   string or stream lookup), Separation/DeviceN (alternate space + tint
   transform via `PdfFunc`), `/Decode`, bpc 1/2/4/8/16, colour-key `/Mask`,
   `/SMask` (+ `/Matte`), explicit stencil `/Mask`. Also a shared
   `colorToRGB` for the renderer's fill/stroke colours. Filters: everything
   `Cmedit.Pdf` already decodes, plus **DCTDecode** through `Image.decodeJPEG`
   (extended to 4-component CMYK/YCCK JPEGs with the Adobe APP14 transform
   and inversion) and **CCITTFaxDecode** (G3 1-D, G3 2-D, G4; `K`, `Columns`,
   `Rows`, `BlackIs1`, `EncodedByteAlign`, `EndOfBlock`) in the new leaf
   `Cmedit.Ccitt`. `JPXDecode` and `JBIG2Decode` return a typed "unsupported"
   so the renderer can draw a labelled placeholder and count it in
   `scNotes`. Bounds: `maxPixels`, stream size caps, fuel.

### D. `Cmedit.Svg`: SVG → Scene

Over `Cmedit.Xml`. Structure: `svg` (nested, viewBox +
preserveAspectRatio), `g`, `defs`, `symbol`, `use` (with recursion guard),
`switch` (first child; `requiredFeatures`/`systemLanguage` treated as
satisfied), `a`. Shapes: `path` (full path-data grammar incl. arcs →
cubics, implicit commands, compact number syntax), `rect` (rx/ry),
`circle`, `ellipse`, `line`, `polyline`, `polygon`. Styling: presentation
attributes, `style=""`, a `<style>` CSS subset (type/class/id/universal/
descendant/child selectors, specificity, `!important`), inheritance,
`currentColor`, all named colours, `#rgb`/`#rgba`/`#rrggbb`/`#rrggbbaa`,
`rgb()`/`rgba()`/`hsl()`/`hsla()`, units (px, pt, pc, mm, cm, in, em, ex, %
against the right viewport dimension), `display`/`visibility`, `opacity`,
`fill-opacity`, `stroke-opacity`, all stroke properties, `paint-order`,
`vector-effect: non-scaling-stroke`. Paint servers: linear/radial gradients
(href inheritance, `gradientUnits`, `gradientTransform`, `spreadMethod`,
`fx`/`fy`/`fr`), patterns (units, `patternContentUnits`, `viewBox`,
`patternTransform`, href). `clipPath` (units, clip-rule, nested clip-path),
`mask` (luminance and `mask-type: alpha`, units), markers (start/mid/end,
`orient` incl. `auto-start-reverse`, `markerUnits`, viewBox), `image` (data:
URIs through `Image.decodeImage`; external files through a resolver the
caller supplies), text (`text`/`tspan` with x/y/dx/dy lists, `text-anchor`,
font-family/size/weight/style, `xml:space`, glyphs from `Cmedit.Font` via a
font provider the caller supplies; no font ⇒ text omitted + a note), filters
(`feGaussianBlur`, `feOffset`, `feFlood`, `feComposite`, `feMerge`,
`feColorMatrix`, `feBlend`, `feDropShadow`; anything else ⇒ render the
element unfiltered + a note). `.svgz` (gzip). Animations are ignored (the
static base value). Bounds: element count, `use` depth, path length.

### E. `Cmedit.PdfRender`: PDF page → Scene

The whole graphics model over `PdfObj`: graphics state stack (`q`/`Q`, `cm`,
`w J j M d ri i gs`), path construction and painting (`m l c v y h re`,
`S s f F f* B B* b b* n`, `W W*`), colour (`CS cs SC SCN sc scn G g RG rg K
k`, patterns as paints), `sh` and shading patterns types 1–7 (1 sampled to
`PSampled`; 2/3 to `PLinear`/`PRadial` with Extend → `Spread`; 4–7 meshes
subdivided to `CGouraud`), tiling patterns (coloured and uncoloured),
XObjects (images, forms with `/Matrix` `/BBox` and transparency groups),
inline images, `ExtGState` (CA/ca, BM, SMask, LW/LC/LJ/ML/D, Font, SA), text
(all `T*` operators, render modes 0–7 incl. clipping, word/char spacing,
horizontal scaling, rise, Type 3 glyph procedures, simple fonts with
encodings + `Differences` + the TrueType symbolic/non-symbolic cmap rules,
Type0/CID fonts with CMaps and `CIDToGIDMap`, standard-14 and missing fonts
substituted through a font provider using the PDF widths to stretch the
substitute's advances), optional content (`/OC` off ⇒ skipped), annotations
with appearance streams (`/AP /N`, not hidden/no-view), crop box + `/Rotate`.
JPX/JBIG2 images ⇒ labelled grey placeholder + a note. Bounds: the existing
`Fuel`, plus a command-count cap.

### F. Integration

- **SVG** opens in the **image view** as a *derived* picture of its text
  buffer (the RTF view's one-way model: the buffer stays the document; Alt+T
  switches to editing the source and back re-renders it). Zoom re-rasterises
  the vector scene for the crop instead of scaling pixels.
- **PDF page view**: a mode of the PDF view (`edPdf`), not an image doc.
  Alt+T toggles reflow ↔ page; page turning (`[`/`]`, PgUp/PgDn), Go To page,
  wheel/drag zoom with sharp re-rendering, Home/End; find and selection
  switch to the reflow view. Default view is configurable
  (`pdf-view = reflow|page|auto`, auto = page when the terminal shows pixels).
- **Rendering is off the event-loop thread**: a background job per request
  (page × zoom × size), superseded by newer requests, cached (current ±1 page
  at the current size), cancellation-safe (`JxlPar` rules if parallel).
- Pixels through the existing `gfxOverlay`/`kittyPlace`/`sixelPlace`; the
  half-block picture as fallback (for a page view it is a thumbnail, and says
  so).
- System font discovery on a startup thread; the font provider is handed to
  the render jobs.
- `cmedit file.svg > out.png`? No: conversion stays text-only. (`cmedit
  file.svg` with redirected stdout prints the source, `cmedit file.pdf` the
  text, as today.)
- Docs: CLAUDE.md architecture bullets, README module table, the manual
  (`Cmedit.Manual`), the help card if a key is added, `configKeysHelp`, the
  Settings dialog row for `pdf-view`.

## Validation

- `make test` green, `make windows-check` green, no new warnings.
- `docs/plans/bench/svgcheck.sh`: every SVG under `/usr/share/icons`,
  `/usr/share/pixmaps` and the hand-written feature set renders without error,
  within the time bound, and within tolerance of `resvg` (report the
  distribution; investigate the worst 20).
- `docs/plans/bench/pdfcheck.sh`: every PDF found on the machine renders
  every page without error, within tolerance of `pdftoppm` (same reporting).
- Fuzz: truncation at every offset and bit flips for a set of fixtures
  through `parseFont`, `Svg`, `PdfRender`, `PdfImage`, `Ccitt`: never an
  exception, never over the time bound.
- PTY harness: open an SVG and a PDF with a fake kitty terminal (answer the
  probe) and check the APC placement appears; with no replies, the half-block
  fallback; zoom/page-turn re-render; Alt+T toggles.
- Memory: peak RSS opening a large PDF in page view and paging through it
  stays bounded (cache size × page pixels + the file).

## Log

- 2026-11-13: plan written; `Cmedit.Geom` and `Cmedit.Scene` (the contract)
  and the per-workstream spec modules added; workstreams A–D dispatched in
  parallel, E after C's split.
- 2026-11-14: F1 (SVG integration) landed: an SVG opens as a derived picture
  of its buffer in the image view (`Cmedit.VecView`, `idVec`, Alt+T to the
  source), with parse, cell and overlay rasters on background jobs
  (`JSvgParse`, `JVecCells`, `JVecGfx`) keyed by scene id × crop × size, and
  system fonts through `Cmedit.FontProvider` (`fontFor` is the PDF page
  view's entry point); `svg-view` config key + Settings row. Measured at -O2
  (`docs/plans/bench/SvgViewBench.hs`): system icons parse in 0.1–80 ms,
  rasterise at cell size in 0.1–11 ms and at a 944×608 overlay in 5–60 ms
  (one 290 ms outlier); a synthetic 4 MB / 40 000-path SVG parses in 430 ms,
  rasterises in 770 ms for cells, 2.5 s for a full overlay and 450 ms zoomed
  8×, all off the UI thread. `pty_svg.py`: every scenario passes.
- 2026-11-14: F1 review fixes: jobs forked unmasked (`Cmedit.Jobs`; a
  superseded busy pure job's kill went from 2.6 s to 1 µs), `fontFor` no
  longer catches kills, parse identity carries a picture epoch (Revert race),
  `gfxFitVector` sizes vector payloads to the display (deep zoom on kitty
  1549×774 instead of 256×128; sixel gets the full box), Alt+T/Save As can
  always leave a picture, scene LRU (`keepScenes`), cells painted until the
  placement exists, placements dropped on geometry change, fonts-ready
  redraw, failed cell rasters remembered, a 2M sync raster budget, unwanted
  jobs cancelled, a 2^24 virtual side, symlinked font dirs followed.
- 2026-11-14: F2 (PDF page view) integrated against a stub `Cmedit.PdfRender`
  (E's agreed API): the page view is a mode of the PDF view (`edPdfView`,
  Alt+T, `pdf-view = auto|page|reflow`, auto switching once when pixel caps
  arrive), pages rendered by `JPdfPage` keyed (path, epoch, page) with the
  font pre-pass, the next/previous page and the next page's overlay
  prefetched while idle, page scenes held to ±1 through `keepScenes` and
  dropped while inactive. `pty_pdf.py` (stub renderer, fake kitty): PgDn
  placement 256 ms cold, 51 ms prefetched.
- 2026-11-14: F2 review fixes: prefetch converges with heavy or failing
  neighbours (`pvNoFit`, `pvFailed`, next-page preference), stale placements
  dropped unless a render is pending, `pvPage` kept across Alt+T
  (`pvLeftAt`), fontless pages after fonts arrive redone, the next-page
  overlay key computed without a simulated turn (5.0 ms → 0.06 µs per frame
  on a cost-17 500 neighbour), job markers written on start and cleared on
  any non-kill death (`guardJob`), prefetch pre-empted by current-page work,
  page errors drawn in the page area, wheel page turns rate-limited.
- 2026-11-14: E (`Cmedit.PdfRender` over `PdfRenderFont`/`PdfRenderShade`)
  landed: page → scene with crop/rotate, clips tracked by id, forms, Type 3
  glyphs and tiling cells shared through `CTransform` (memoised per inherited
  state), all shading types, soft masks, groups, images/stencils/inline
  images with JPX/JBIG2 placeholders, simple/TrueType/CID fonts with CMaps and
  stretched substitutes (`pageFontNeeds` pre-pass), optional content,
  annotations (`/AP /N` plus poppler's generated link/square/circle/markup/
  line/ink appearances). API as stubbed, plus `defaultRenderEnv`,
  `renderPagePrint`, `pageGeometry`. Oracle (`pdfrendercheck.sh`, 100 dpi,
  first 3 pages of the 151-file corpus, 366 pages): against pdftoppm median
  AE 2.5% (57% ≤ 3%, 89% ≤ 5%, max 7.4%); against pdftocairo median 0.05%
  (96% ≤ 2%, max 3.2%) — Splash snaps glyphs and thin rules to pixels, which
  is the whole of the pdftoppm gap (pdftocairo and Ghostscript sit 3–8% from
  it on the same pages). All 2421 corpus pages (≤ 200/file) render under a
  `-DRASTER_SAFE` build with no exception. Vector CMYK uses poppler's
  16-corner formula (`paintColor`); a RasterCore `SkMask` overrun found by
  the corpus is clamped.
- 2026-11-14: E, image memory and the release review. Decoded images are
  reduced at decode time (the rasteriser's streaming box filter, now
  exported) to 600 dpi of their placed size, at most 8 MP each and 32 MP per
  page; stencils likewise. A 16-page scan of 24 MP photos: scene image
  memory 96 → 32 MB per page, peak RSS rendering every page 985 → 555 MB
  (the remainder is the full-size JPEG decode itself), 3.1 → 3.4 s per page.
  Review blockers: shared forms, Type 3 procs, tiles and masks are charged
  their expanded weight on every use (t3bomb/formbomb stop in < 0.1 s with
  "drawing truncated: too complex"); `/W`/`/W2` are intervals charged to
  fuel (cidw20: 1.1 GB → 24 MB). Also: O(1) clip-chain fast path and a 256
  depth cap, memo keys that see /S /BC /TR, inherited resources, inline font
  ids and the colour lock, and patch meshes held to 40 000 triangles per
  shading (shade_F 26 s → 0.3 s). Oracle unchanged (pdftoppm median 2.54%,
  pdftocairo median 0.05%); all 2421 corpus pages clean under RASTER_SAFE.
- 2026-11-14: release follow-up: the reflow extractor's `/W` widths use the
  same interval map as the page renderer (`Cmedit.IntervalMap`, a painter:
  last write wins key by key, so it is exactly the old expanded map, and the
  per-CID budget accounting is unchanged). `parsePdf` over the 151-file corpus
  plus the review's hostile files hashes identically before and after;
  `cmedit cidw20.pdf > /dev/null` peaks at 14 MB (was 2286 MB).
