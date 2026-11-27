# 0043 — Graphical DOCX page view (Tier 2.5)

**Theme:** capability — native, pure-Haskell graphical page rendering for `.docx` documents (`DocModel` → paginated `Scene` → `Raster` → `VecView`), bringing `.docx` to parity with the PDF page view (`0034`, plus fit-width continuous strip and box-select) across text, styles, tables, floats, DrawingML shapes, SmartArt, 2D charts, and OMML math equations
**Status:** proposal (co-designed and reviewed with Fable)
**Estimated effort:** 3–5 weeks solo, or ~4–6 days across parallel workstreams with milestone reviews (roughly 9,000–13,000 LOC total; ~0.8–1.0× the scale of `0034`, reusing its entire rasteriser, font, and terminal-pixel stack)
**Risk:** medium–high. Unlike PDF (a fixed-layout drawing script), OOXML is a reflowable semantic format with deep style inheritance, floating/table/footnote interactions at page breaks, and rich hostile-input surfaces (multi-part ZIP amplification, external-relationship NTLM/SSRF traps, layout non-termination). The design mitigates these with integer layout units, strict checkpoint pagination, explicit well-founded progress invariants, and a staged LibreOffice/Word oracle started on day one.

---

## 1. The problem and opportunity

Plan [`0034`](../completed/0034-vector-rasteriser-svg-and-pdf-pages.md) built a complete, from-scratch 2D vector stack: `Cmedit.Geom`, `Cmedit.Scene`, `Cmedit.Raster`, `Cmedit.Font*`, `Cmedit.FontProvider`, `Cmedit.VecView`, and the `PdfView` page viewer (with kitty shm/`o=z` from `0041`, sixel, half-block fallbacks, plus the in-progress continuous fit-width strip in `PdfStrip.hs` and page box-select).

Meanwhile, `.docx` files currently open only through [`Cmedit.Docx`](../../../src/Cmedit/Docx.hs) (403 LOC), which maps `word/document.xml` onto `RtfPar` for the terminal reflow view — deliberately ignoring `styles.xml`, `theme1.xml`, `numbering.xml`, headers/footers, embedded images, tables-as-grids, drawings, charts, and equations.

Extending graphical page viewing to `.docx` reuses ~60% of `0034`'s stack verbatim (rasterisation, font decoding, system font substitution, PNG/JPEG/GIF/WebP/SVG decoding, zoom/pan crops, continuous strip tiling, box-select, terminal pixel transport), while adding the one layer PDF never needed: a **pure, bounded typesetting and pagination engine** plus object renderers for DrawingML, charts, and OMML math.

### Why Tier 2.5

- **Tier 1** (styles, themes, numbering, proportional wrapping, tables, inline images, headers/footers, pagination) covers ~80–85% of plain prose/report `.docx` files.
- **Tier 2** (floats + text wrap, DrawingML shapes/groups/text boxes, footnotes/endnotes, multi-column sections, `.odttf` embedded fonts, fields, VML + WordArt watermarks) brings coverage to ~90%+.
- **Tier 2.5** cherry-picks three high-value, cleanly isolated "object" renderers that do **not** complicate pagination:
  1. **SmartArt** via Word's pre-computed cached `word/diagrams/drawing*.xml` (reuses Tier 2 DrawingML shapes directly).
  2. **DrawingML 2D Charts** (`word/charts/chart*.xml` using cached `c:numCache`/`c:strCache` values).
  3. **OMML Math Equations** (`m:oMath` / `m:oMathPara`) via a recursive TeX-style `MathBox` engine.

Together, Tier 2.5 targets **~92–96% of real-world Western and CJK `.docx` documents** (business reports, specs, academic papers, slide-like handouts), while staying pure Haskell with zero runtime dependencies. Isolating `DocModel` → `DocLayout` → `DocPaint` also gives CMeDit a reusable typesetter that `.odt`, `.rtf`, and `.epub` can target in future plans.

---

## 2. Ground rules

All constraints from `CLAUDE.md`, `CONTRIBUTING.md`, and `0034` apply without exception:

- **Boot libraries only, offline, `make` not `cabal`.** Zero Hackage dependencies; zero shelling out to LibreOffice or any external tool at runtime.
- **Pure core, thin IO shell.** XML parsing, style resolution, shaping, layout, painting, charts, and OMML are 100% pure (`ByteString`/data in, data out). All ZIP I/O, font discovery/loading, image decoding, and background job scheduling live in `Cmedit.App`.
- **Never an exception, always a bound.** Malformed or hostile archives return `Left`, a partial result, or a labelled placeholder with an honest `scNotes` entry — never a crash, hang, or unbounded allocation.
- **Cancellation-safe.** Every layout, chart, and math module compiles with `{-# OPTIONS_GHC -fno-omit-yields #-}` (like `Cmedit.VecView`) so `killThread` via `Cmedit.Jobs` lands within microseconds on document switch, zoom, or `Ctrl+Q`.
- **Graceful floor.** `Cmedit.Docx`'s single-member reflow mapping remains the fast open path and `DocText` / CLI-stdout source; terminals without pixel caps still get the reflow view (and half-block thumbnails in page view).
- **Portable.** Nothing outside `platform/posix` imports `unix`; `make windows-check` must stay green after every driver change.
- **Performance invariants (`CONTRIBUTING.md`).** Structural `Seq` bounds (never lazy `take`), detached `Text` slices (`T.copy`) at parse boundaries so small retained strings never pin multi-MB XML buffers, strict `Seq.adjust'`/`|>` storage, and at most one background job of each kind in flight.

---

## 3. Scope (Tier 2.5)

### 3.1 Included

#### Tier 1 — Core document & pagination
- **Multi-part OPC package loading**: `[Content_Types].xml`, `_rels/.rels`, per-part `*.rels` (`document.xml`, `styles.xml`, `theme1.xml`, `numbering.xml`, `settings.xml`, `fontTable.xml`, `header*.xml`, `footer*.xml`, `footnotes.xml`, `endnotes.xml`, `docProps/core.xml`, `media/*`, `charts/*`, `diagrams/*`).
- **Full style & theme cascade**: `docDefaults` → `basedOn` chains → paragraph/table/character styles → table conditional formatting (`w:tblLook` / `w:cnfStyle`) → direct `w:pPr` / `w:rPr`; theme font slots (`majorHAnsi`/`minorHAnsi`/`majorEA`/`minorEA`) and theme colour scheme (`dk1`..`folHlink`) with `lumMod`/`lumOff`/`tint`/`shade`/`alpha`.
- **Settings & grid**: `word/settings.xml` (`evenAndOddHeaders`, `defaultTabStop`, `compat` flags affecting spacing/wrapping) and `w:docGrid` (`type="lines"` line-pitch snapping common in CJK documents).
- **Typography & font fidelity** (coordinator-owned extensions to `Font*` — critical for matching Word/LibreOffice line heights and widths):
  - **Vertical font metrics**: expose `hhea` line gap, `OS/2` `usWinAscent`/`usWinDescent`/`sTypoAscender`/`sTypoDescender`/`sTypoLineGap`/`fsSelection` (`USE_TYPO_METRICS`), and underline/strikeout position & thickness on `FontCore.Font`.
  - **GPOS PairPos kerning, gated on `w:kern`**: bounded OpenType `GPOS` lookup type 2 (formats 1 and 2) in `FontTT.hs` alongside legacy `kern` format 0, applied in `TextShape` **only when OOXML's `w:kern` run property is set and the run's font size meets the `w:kern` threshold** (matching Word rather than applying kerning unconditionally). Ligature parity (`liga`, which LibreOffice enables by default while Word disables by default) is settled on day one in Workstream H (see §9.1).
  - **Symbol / Wingdings PUA mapping**: add `ClsWingdings` and PUA→Unicode tables (`U+F0B7` → `•`, `U+F0A7` → `▪`, etc.) in `FontSys.hs` so Word bullets render honestly instead of missing glyphs.
  - **Four-slot `rFonts` selection per character** (`ascii`, `hAnsi`, `eastAsia`, `cs`) + per-character fallback chain.
- **Proportional text layout & line breaking**:
  - Mixed-style runs (size, bold/italic/synthetic oblique & embolden fallback, underline styles, single/double strikeout, highlight/run shading, super/subscript, small caps, all caps, letter spacing `w:spacing`, `w:vanish` hidden text, tracked deletions `w:del`/`w:moveFrom` hidden).
  - UAX #14 subset line breaking **plus CJK character-level break opportunities** (with basic kinsoku non-start/non-end rules) and CJK fallback families in `FontSys.hs`.
  - Alignment: left, center, right, justified (`both`/`distribute` with last-line exemption); tab stops (left/center/right/decimal, default tab stops, dot/dash/underline leaders); paragraph indents (left/right/first-line/hanging), space-before/after (with contextual spacing), line spacing (`auto`/`exact`/`atLeast`), paragraph borders & shading.
- **Numbering & lists**: `word/numbering.xml` (`abstractNum` + `num` + `lvlOverride`, `start`, `startOverride`, `numFmt` decimal/lowerLetter/upperLetter/lowerRoman/upperRoman/bullet/none, `lvlText`, per-level hanging indents and tab stops, continuous counters per `abstractNumId` across paragraphs).
- **Tables**: `w:tblGrid`, `w:tblW`, `w:tcW` (dxa/pct/auto), fixed & bounded auto-fit column width solver, cell margins (`w:tblCellMar`/`w:tcMar`), vertical alignment (`top`/`center`/`bottom`), `gridSpan` (colspan), `vMerge` (resolved into explicit row spans in `DocModel`), collapsed border conflict precedence, cell/row/table shading, rotated cell text (`tbRl`/`btLr`), multi-page row splitting, repeating header rows (`w:tblHeader`), and `w:cantSplit`.
- **Inline media**: `wp:inline` raster images (PNG, JPEG, GIF, BMP, WebP via `Cmedit.Image`) and `asvg:svgBlip` (via `Cmedit.Svg` with external resolution disabled).
- **Sections, headers, footers & pagination**: page size, orientation, margins, header/footer distances, default/first/even-odd headers & footers (inheriting from previous section when omitted), explicit page/column/section breaks (`nextPage`/`continuous`/`evenPage`/`oddPage`), `keepNext`, `keepLines`, `widowControl`, and page numbering (`w:pgNumType`).

#### Tier 2 — Rich Office layout & drawings
- **Anchored / floating objects**: `wp:anchor` positioning (relative to page/margin/column/paragraph/character) with `wrapSquare`, `wrapTopAndBottom`, `wrapNone` (`behindDoc` / in front), and `wrapTight` approximated via bounding-box exclusion with a note when polygonal wrap is simplified.
- **DrawingML shapes, groups & text boxes**: `wps:wsp`, `wpg:wgp`, `a:xfrm` (offset, extent, rotation, flipH/flipV), `a:prstGeom` guide evaluator covering the ~50–60 most common presets (rectangles, rounded rects, ellipses, arrows, callouts, flowchart shapes, stars, braces, connectors/lines), `a:custGeom` path commands, solid/linear/radial gradient fills, strokes/dashes/arrowheads (`a:headEnd`/`a:tailEnd`), and `wps:txbx`/`w:txbxContent` rich text boxes (supporting paragraphs & tables inside shapes via a block-layout callback; XML `a:ext` is authoritative and `a:spAutoFit` is ignored).
- **Footnotes & endnotes**: `word/footnotes.xml` and `word/endnotes.xml`, automatic/custom reference marks (`w:numRestart`: `continuous`/`eachSect`/`eachPage`), separator rule, bottom-of-page footnote area budgeting with multi-page continuation queue for long notes, endnotes rendered at document/section end.
- **Multi-column sections**: `w:cols` (equal and custom column widths, column spacing, optional vertical separator rule, column breaks, balanced continuous section breaks approximated cleanly).
- **Embedded `.odttf` fonts**: parse `word/fontTable.xml` (`embedRegular`/`embedBold`/`embedItalic`/`embedBoldItalic`), de-obfuscate the 32-byte XOR header using `w:fontKey` GUID, and parse via `Cmedit.Font.parseFont`.
- **Fields**: `PAGE`, `NUMPAGES`, `SECTIONPAGES`, `TITLE`, `AUTHOR` (from `docProps/core.xml`), and cached field results (`fldSimple` and `fldChar` begin/separate/end) for all other fields — including **`DATE`** (always displays cached result, never reads the system clock, preserving pure layout just as `Cmedit.Formula` omits `TODAY`) and **`HYPERLINK`** (rendered with hyperlink visual styling only; pixel placements are non-clickable).
- **Minimal VML fallback**: `v:shape`, `v:rect`, `v:roundrect`, `v:oval`, `v:line`, `v:group`, `v:textbox`, `v:imagedata` (also captures cached preview rasters for embedded OLE objects), and **`v:textpath`** (WordArt fitted to a rotated box, which is how Word implements text watermarks in headers).

#### Tier 2.5 — Object renderers (non-paginating, `Scene`-native)
- **SmartArt (`word/diagrams/drawing*.xml`)**: load the pre-laid-out DrawingML shape tree (`dsp:drawing` / `dgm:relIds`) and render it through the Tier 2 DrawingML renderer. (If a producer omitted `drawing*.xml`, show a labelled placeholder + `scNotes`.)
- **DrawingML 2D Charts (`word/charts/chart*.xml`)**: standalone `Cmedit.Chart` module reading cached `c:numCache`/`c:strCache` values and theme accent colours:
  - Chart types: Bar/Column (clustered, stacked, 100% stacked), Line (straight/smooth, markers), Area (standard/stacked/100%), Pie & Doughnut, Scatter (XY) & Bubble, Radar, and Combo (overlaid plot-area charts with primary/secondary axes).
  - Chrome: chart title, axis titles, category/value/date axes, auto-scaled major/minor ticks & gridlines, log axes (guarded for values `<= 0`), basic number formatting (general, fixed, percent, currency, thousands), legends (top/bottom/left/right), and data labels.
  - 3D chart types (`bar3DChart`, `pie3DChart`, `line3DChart`, `area3DChart`) fall back cleanly to their 2D projection with one `scNotes` entry.
- **OMML Math Equations (`m:oMath`, `m:oMathPara`)**: standalone `Cmedit.MathLayout` module using recursive TeX-style `MathBox` measurement (`mbW`, `mbAsc`, `mbDesc`, `mbCmds`) with procedural vector paths for tall delimiters, radical signs, fraction bars, and over/underbraces:
  - Constructs: `m:r` (math italic variables vs upright digits/operators, `m:sty`, `m:nor`), `m:f` (`bar`/`skw`/`lin`/`noBar`), `m:rad` (square & nth roots), `m:sSup`/`m:sSub`/`m:sSubSup`/`m:sPre`, `m:nary` (`∑`, `∫`, `∏`, `⋃`, `⋂`, `undOvr` vs `subSup`), `m:d` (auto-stretching delimiters), `m:m` (matrices), `m:eqArr` (aligned equation arrays), `m:acc` (hat/bar/tilde/dot/vec), `m:bar`/`m:borderBox`/`m:box`, `m:func`, `m:limLow`/`m:limUpp`, `m:groupChr` (overbrace/underbrace).
  - Add `ClsMath` in `FontSys.hs` (`Cambria Math` → `STIXTwoMath`, `LatinModernMath`, `DejaVuMathTeXGyre`, `NotoSansMath`, `DejaVuSerif`, `FreeSerif`).

### 3.2 Explicitly out of scope (honest `scNotes` / placeholders)

- **Complex-script shaping & full UAX #9 BiDi** (Arabic contextual joining, Indic/Thai reordering): requires OpenType `GSUB`/`GPOS` script shapers in `FontTT.hs` + BiDi. RTL paragraphs are laid out LTR with one honest status note (`"RTL/complex-script shaping not supported"`).
- **EMF / WMF metafiles**: legacy Win32 GDI / EMF+ streams. Use any accompanying raster fallback (e.g. `mc:Fallback` or sibling PNG) when present; otherwise draw a labelled placeholder box and record a note in `scNotes`.
- **Live SmartArt layout engine (`layout1.xml`)**: only cached `drawing*.xml` is rendered.
- **True 3D rendering** (3D chart perspective, shape 3D bevels/camera/lighting) and rare Surface/Stock charts.
- **Macros, ActiveX controls, live OLE servers**: render cached preview `v:imagedata`/`a:blip` if present, else labelled placeholder.
- **Interactive link clicks inside pixel placements**: `Scene` has no hyperlink metadata and terminal graphics overlays do not participate in `cellLink`.
- **Routing the reflow view (`Cmedit.Docx`) through `DocModel`**: stays a named follow-up (which could later give reflow real heading sizes and resolved list numbers). Keeping `Cmedit.Docx`'s single-member mapping unchanged preserves its millisecond open path, `DocText` extraction, and CLI stdout output.

---

## 4. Architecture, modules, and frozen contracts

```text
                       Cmedit.Xml (parseXmlQualified)        Cmedit.Zip
                             │                                   │
                       Cmedit.XmlNs (Ns enum + local)       Cmedit.Opc (_rels, parts)
                             └───────────────┬───────────────────┘
                                             ▼
                 ┌────────────────── Cmedit.DocModel ★ CONTRACT ──────────────────┐
                 │  (resolved flow IR, DrawingIR, ChartIR, MathIR, integer Twip64) │
                 └───────┬───────────────────┬──────────────────┬─────────────────┘
                         │                   │                  │
        ┌────────────────┼───────────────────┤                  │
        ▼                ▼                   ▼                  ▼
  DocxStyle/Num/    DocxDraw/Preset/    Cmedit.Chart     Cmedit.MathLayout
  DocxParse         DocxVml/SmartArt    ChartIR→(sz,Cmd) MathIR→MathBox
  (parts→DocModel)  (→Drawing)               │                  │
        │                │                   │                  │
        │        ┌───────┴───────────────────┴──────────────────┘
        │        │             Cmedit.Font* (+ vertical metrics, GPOS PairPos, ClsMath/Wingdings)
        │        │                  │
        │        │             Cmedit.TextShape ★ CONTRACT (itemise, font pick, measure, UAX#14+CJK)
        ▼        ▼                  ▼
     Cmedit.DocLayout (+ DocLayoutTable, DocLayoutFloat)  ★ CONTRACT: PageStart / PageLayout
        │  layoutPage :: LayoutEnv -> DocModel -> PageStart -> (PageLayout, Maybe PageStart)
        ▼
     Cmedit.DocPaint   (PageLayout → (Cmedit.Scene, [PageGlyph]))
        │
        ▼
     Cmedit.Scene → Cmedit.Raster → Cmedit.VecView → PageView / PageStrip (Editor/App/Render)
```

### 4.1 Three contracts frozen before fan-out

Following `0034`'s lesson (where `Geom` + `Scene` were reviewed and frozen before parallel workstreams started), **three contracts are written and reviewed by Fable before any workstream fans out**:

1. **`Cmedit.DocModel`** (leaf over `Geom`, `Scene.Color`/`Paint`/`StrokeStyle`; **must derive `Show`** because `Editor` derives `Show`):
   - **Integer layout units (`Twip64 = Int64`, 1/64 twip = 1/1280 pt)**: exact integer equality on `PageStart`, zero `NaN` loop hazards, and native alignment with Word/LibreOffice's twip grid.
     - *EMU conversion*: `1 EMU = 64/635` `Twip64` units (non-terminating in base 2/10). Convert at parse time by **rounding to nearest `Twip64`** (`(emu * 64 + 317) `div` 635` for non-negative values; max error ≤ 1/2560 pt).
     - *Font advance quantisation*: `TextShape` is the **single quantisation boundary** where `Double` font advances and kerning pairs become `Twip64` — quantised **per glyph advance** (`round (advEm * sizePt * 1280)`) and per kern adjustment, so every downstream line break and checkpoint equality is pure deterministic integer arithmetic.
   - Contains resolved sections (`SectProps`, headers/footers, columns, docGrid), blocks (`Par`, `Table` with `vMerge` already resolved into explicit row-span counts, `PageBreak`, `ColBreak`), inlines (`Run`, `Tab`, `Break`, `Field`, `InlineImage`, `InlineDrawing`, `InlineChart`, `InlineMath`, `FootnoteRef`), floating anchors (`FloatObj`), footnotes/endnotes, resolved list-item labels (computed in document order by `DocxNum`/`DocxParse`, since list numbering depends only on document order and not layout), and compact object IRs (`Drawing`, `ChartIR`, `MathIR`).
   - All retained `Text` values are `T.copy`-detached at parse time (never slicing decoded XML buffers).
   - Carries a parser-computed byte estimate (`dmEstBytes :: !Int`) used by `keepScenes` for model parking (since GHC boot libraries cannot measure live Haskell heap size per value).
2. **`LayoutEnv` / `TextShape` face contract** (coordinator-owned extensions to `FontCore`, `FontTT`, `Font`, `FontSys`, `FontProvider`):
   - `FontCore.Font` gains `fontLineGap`, `fontWinAscent`, `fontWinDescent`, `fontTypoAscent`, `fontTypoDescent`, `fontTypoLineGap`, `fontUseTypoMetrics`, `fontUnderlinePos`, `fontUnderlineThick`, `fontStrikePos`, `fontStrikeThick` (all in ems, defaulting safely when tables are absent for CFF/Type 1).
   - `FontTT.hs` adds a bounded `GPOS` PairPos (lookup type 2, formats 1 and 2) reader composed into `fontKerning`, gated by `TextShape` on `w:kern`.
   - `FontSys.hs` adds `ClsMath`, `ClsWingdings`, CJK fallback families, and Symbol/Wingdings PUA→Unicode lookup tables.
   - `FontProvider.hs` exposes an allocator for embedded-font face IDs from its own `fcNextKey` key space; `DocPaint` packs `(faceId, gid)` using **`PdfRenderFont.glyphKey`'s existing scheme** so embedded `.odttf` fonts can never collide with system-font glyph keys in `Scene`.
   - **`LayoutEnv` lives driver-side** (in `Drv`, keyed by `(path, epoch)` and cleared on close/evict/epoch change), never inside `edDocx` — because `Font` is a record of closures and `Editor` derives `Show`. Because `LayoutEnv` pins loaded faces outside `FontProvider`'s global 12-face / 96 MiB LRU, it enforces its own per-document cap (`maxDocFaces = 24`, `maxDocFaceBytes = 128 MiB`; CJK faces are ~20 MiB each), falling back deterministically to the document's default/generic face with a note once capped.
   - `LayoutEnv` also memoises measured `(sectIdx, hfKind) -> (headerH, footerH)` so pagination accounts for tall headers and footers without re-measuring them on every page.
   - `TextShape.hs` exposes pure itemisation, per-character 4-slot `rFonts` + fallback font selection, `w:kern`-gated `Twip64` width/vertical measurement, and UAX #14 + CJK break opportunities.
3. **`PageStart` & `PageLayout`** (`Cmedit.DocLayout`):
   - `PageStart` is a **small fixed record plus shared persistent maps/queues** (strictly evaluated, **zero `Text` slices**, deriving `Eq` and `Show`):
     - `psPage :: !Int` (global 1-based page index), `psSect :: !Int` (section index — gives page `(w, h)` directly from `DocModel` without laying out the page, enabling `PageStrip`), `psSectPage :: !Int` (1-based page number within section, honouring `w:pgNumType`),
     - `psMode :: !PageFlowMode` (`FlowNormal | FlowPendingBlank | FlowPendingBreak !Int | FlowEndnotes !EndnoteCursor` — cleanly represents `evenPage`/`oddPage` parity blank pages, consecutive explicit page breaks, and the end-of-document/section endnote stream),
     - `psBlock :: !Int` (body block ordinal) and `psParOrd :: !Int` (source `<w:p>` ordinal at page top, for `Alt+T` place-keeping),
     - `psResume :: !BlockCursor` (no active column field — every page begins in column 0):
       - `BCStart` (top of block),
       - `BCPar !Int !Int !(IS.IntSet)` (run index, char offset, and set of float anchor indices of the resumed paragraph already placed so a split paragraph never draws its anchored float twice),
       - `BCTable !Int !(IM.IntMap CellCursor)` (row index + per-cell resume cursors, recursive for nested tables up to `maxTableNestDepth`),
     - `psFootnotes :: !FootnoteState` (FIFO queue of pending footnote IDs, optional head continuation cursor `(fnId, blockIdx, BlockCursor)`, plus active footnote & endnote counters respecting `w:numRestart`). *(List numbering is resolved in document order during `DocxParse`/`DocxNum`, so `PageStart` needs no numbering-counter or seen-`numId` state.)*
   - `layoutPage :: LayoutEnv -> DocModel -> PageStart -> (PageLayout, Maybe PageStart)` — a bounded unfold returning `Nothing` once the document ends (or a cap is hit).
   - **Object workstream signatures**:
     - Charts (`Chart`) and OMML (`MathLayout`) take their IR + a pure measure/font callback and return `(Twip64, Twip64, Twip64, [Cmd], [String])` — `(width, height, baseline, commands, notes)`.
     - DrawingML/SmartArt/VML (`DocxDraw`) additionally accepts a **block-layout callback** in `DrawEnv` (breaking the D → C dependency cleanly) so `wps:txbx`/`w:txbxContent` lays out rich paragraphs and tables clipped to the XML `a:ext` box (`a:spAutoFit` ignored).
     - All object renderers embed into `DocPaint` via `CTransform` and never interact with pagination.

### 4.2 Namespace-URI XML parsing (`Cmedit.XmlNs`)

`Cmedit.Xml.parseXml` strips prefixes, which is fatal across OOXML where prefixes collide semantically:
- Elements: `w:p` vs `a:p`, `w:r` vs `a:r` vs `m:r`, `w:t` vs `a:t` vs `m:t`, `w:tbl` vs `a:tbl`, `w:tc` vs `a:tc`.
- Attributes: `r:id` vs `w:id` vs VML `id`; `w:val` vs `m:val` vs `c:val`.

Every new parser uses `Cmedit.Xml.parseXmlQualified` wrapped in a small leaf module **`Cmedit.XmlNs`** that maintains a namespace-declaration stack (`xmlns` / `xmlns:p`) and maps **namespace URIs** (not prefixes!) to a compact `Ns` enum (`NsW`, `NsA`, `NsWp`, `NsPic`, `NsWps`, `NsWpg`, `NsC`, `NsDgm`, `NsDsp`, `NsM`, `NsR`, `NsMc`, `NsV`, `NsO`, `NsAsvg`, `NsRel`, `NsCt`, `NsCp`, `NsDc`, `NsOther`), covering **both** Transitional (`http://schemas.openxmlformats.org/...`) and Strict (`http://purl.oclc.org/ooxml/...`) URI families.

### 4.3 Shared body-part agreement and `<w:p>` ordinal definition

- **Body member agreement**: today `Cmedit.Docx.docxBodyMember` detects the body by archive path name, while `Cmedit.Opc` follows `_rels/.rels` (`officeDocument` relationship). Both paths must call **one shared body-member resolver** (following `_rels/.rels` first with `docxBodyMember`'s path fallback) so a crafted archive can never present one document in Reflow View and a different document in Page View.
- **Canonical `<w:p>` ordinal**: defined once and pinned by a Spec test: 0-based count of body-level `<w:p>` elements in `word/document.xml` in document order, **including** `<w:p>` inside table cells (for table rows collapsed by `Docx.hs`, the row maps to its first cell's `<w:p>` ordinal), and **excluding** `<w:p>` inside `txbxContent`, `mc:Fallback`, headers/footers, and footnotes/endnotes — matching the exact subtrees `Docx.hs` already skips.
- **Text parity check**: a Spec cross-check verifies that `DocModel` body plain text agrees with `rtfPlainText . docxPars` across the corpus after stated normalisation (dropping empty paragraphs, joining table cells with `\t`, excluding footnotes/endnotes/text boxes/dynamic fields).

---

## 5. Pagination, concurrency, and memory architecture

### 5.1 Checkpoint pagination (bounded unfold of `layoutPage`)

DOCX cannot jump straight to page `N` like PDF, but storing full `Scene`s or full laid-out pages for a 500-page document would violate CMeDit's memory bounds. Instead:

- **Pagination keeps only `Seq PageStart` checkpoints** (small fixed record + shared persistent maps/queues, zero `Text` slices).
- **Single code path**: `layoutPage` is used both during background pagination (discarding `PageLayout`, keeping only the next `PageStart`) and when painting page `N` (running `layoutPage` from checkpoint `N`, then feeding `PageLayout` to `DocPaint`). Pagination and painting therefore **cannot drift**.
- **Header & footer heights during pagination**: tall headers/footers push body content down, so `layoutPage` measures header and footer heights during pagination (memoised per `(sectIdx, hfKind)` in `LayoutEnv`, using `"999"` placeholder width for `PAGE`/`NUMPAGES`), reserving the exact vertical frame before laying out body blocks; header/footer `Scene` commands are painted at page-paint time with the known total.
- **No fixed-point iteration for `NUMPAGES`**: in body text, `NUMPAGES` is measured using its cached field result (or `"999"` if uncached) and substituted with the known total at paint time. Pages painted while `not dxDone` (showing `"…"` for `NUMPAGES`) carry `totalKnown :: !Bool` in their scene identity so they are cleanly invalidated once pagination finishes.
- **Self-validation with loop guard**:
  - When a page paint job lays out page `N` from `checkpoints[N]`:
    - If `checkpoints[N+1]` has not arrived yet (`not dxDone` and `N == last`), **skip end-state validation**.
    - If `N` is the final page (`dxDone` and `N == last`), validate that `layoutPage` returns `Nothing`.
    - Otherwise validate that `layoutPage` returns `Just checkpoints[N+1]`.
  - On mismatch (e.g. fonts finished loading and changed metrics), trigger re-pagination under a new epoch — **capped at 1 automatic re-pagination per `(opening, fontSetVersion)`**. If a second mismatch occurs (e.g. a flapping `providerFiles` timeout), paint unvalidated with an honest `scNotes` note instead of looping.
- **Layout never touches media bytes**: `wp:extent` and `a:ext` supply EMU dimensions in XML, so pagination runs over XML + font metrics alone. Media members are decoded only by the page job painting the page that uses them.

### 5.2 Background job lifecycle and disk-change policy

All jobs run through `Cmedit.Jobs` (`startJobIn` / `cancelJobIn`), `guardJob`, and `trySync` — never on the event-loop thread. `App.withArchive` is generalised from its current `Maybe (Either String LoadOutcome)` / fixed-`maxArchiveBytes` signature to accept an explicit byte budget and polymorphic return type.

1. **`JDocLayout`** (one in flight globally, keyed `(path, epoch)`):
   - Records/verifies file mtime before and after reading (matching `JImgDecode`). On disk change (including when resuming from parked checkpoints), refuses with `"changed on disk — Revert to reload"` rather than silently laying out newer bytes while the reflow `RtfDoc` still displays the old file.
   - Reads XML parts via generalised `withArchive`/`readMember` under `maxDocXmlBytes = 64 MiB` aggregate inflated XML (each individual member, including `document.xml`, remains bounded by `Zip.maxMemberBytes = 16 MiB`) and `maxDocParts = 256` distinct parts (hitting `maxDocParts` yields placeholders + `scNotes`, never a failed open).
   - Parses parts → `DocModel` (detaching `Text`, discarding raw XML `ByteString`s immediately).
   - Runs pure `docFontNeeds :: DocModel -> [FontRequest]`, loads system faces via `FontProvider.fontFor` and de-obfuscated `.odttf` faces via `parseFont` (allocating face IDs from `FontProvider`'s `fcNextKey` space), building the driver-side `LayoutEnv` under `maxDocFaces` / `maxDocFaceBytes`.
   - Unfolds `layoutPage`, posting `SMDocPaged want startIndex batch done` immediately after page 1 (so **page 1 is viewable without waiting for the rest of the document**) and then every ~10 pages.
   - The pure reducer validates `(path, epoch)` and `startIndex == Seq.length (dxPages dx)` before strictly appending checkpoints into `Seq PageStart`.
   - Switching tabs cancels `JDocLayout` (`CONTRIBUTING.md` rule 6); returning resumes pagination from the last stored checkpoint after checking mtime.
2. **Page paint & prefetch (`JPdfPage` / `JPdfPrefetch`, renamed `JPageRender` / `JPagePrefetch`)**:
   - `startPageParse` dispatches on the document's page source (`PdfDoc` vs `DocxState`).
   - For DOCX: re-opens the archive, checks mtime before and after reads (refusing on mismatch with `"changed on disk — Revert to reload"`), decodes only the media referenced on page `N` (memoised per member within the job so a header logo decodes once), runs `layoutPage` from `checkpoints[N]`, validates against `checkpoints[N+1]` (per §5.1), paints `(Scene, [PageGlyph])`, measures `SceneSize`, and posts `SMPageParsed`.
   - Each page paint job gets its own fresh per-page fuel and image memory budget (matching `0034`'s per-page isolation).

### 5.3 Image memory & document parking

- **Scaled JPEG decode**: use `Image.decodeJPEGScaled` to decode to the smallest scale covering the placed size at useful zoom (600 dpi cap of placed size, ≤ **8 MP / 32 MiB RGBA per image**, ≤ **32 MP / 128 MiB RGBA per page** — matching `0034`'s per-page image budget, distinct from `VecView.pageKeepBytes`'s 160 MiB neighbour-scene cache). Past the per-page cap, emit a labelled placeholder + `scNotes`.
- **Embedded SVGs**: `asvg:svgBlip` decodes via `Cmedit.Svg.parseSvg` with `seResolve = const Nothing` (never reads external files).
- **Parking inactive documents (`captureDoc` / `restoreDoc`)**:
  - `dropPageScenes` drops all cached `Scene`s, strip tiles, and rasters immediately (as PDF already does).
  - Inactive `DocModel`s (and their driver-side `LayoutEnv` entries) are evicted via `VecView.keepScenes` against `dmEstBytes` (keeping the most recent few active DOCX models in memory, dropping older ones).
  - `Seq PageStart` checkpoints remain parked on `docDocx`. Returning to a parked document checks mtime, re-parses `DocModel` on `JDocLayout`, and validates against existing checkpoints without restarting pagination unless validation fails.

### 5.4 Explicit resource & complexity bounds

| Bound | Initial value | Rationale |
|---|---|---|
| `Zip.maxMemberBytes` | 16 MiB (existing) | Per-member cap (bounds `document.xml` and any single part) |
| `maxDocXmlBytes` | 64 MiB | Aggregate inflated XML across all parts in one `JDocLayout` |
| `maxDocParts` | 256 | Distinct ZIP members read (past cap → placeholders + `scNotes`, not failed open) |
| `maxDocBlocks` | 200,000 | Paragraphs + tables + breaks in `DocModel` |
| `maxDocPages` | 5,000 | Pagination cap (stops cleanly with a status note) |
| `maxLinesPerPage` | 2,000 | Prevents degenerate tiny-font / zero-height loops |
| `maxFloatsPerPage` | 64 | Bounds float exclusion geometry per page |
| `maxFootnotesPerPage` | 64 | Bounds footnote splitting work per page |
| `maxStyleDepth` | 32 | `basedOn` / `w:link` / `numStyleLink` chain cap |
| `maxTableNestDepth` | 8 | Nested table recursion cap |
| `maxShapeNestDepth` | 16 | Group shapes & text-box-in-text-box cap |
| `maxMathDepth` | 32 | Nested OMML expression cap |
| `maxFieldNestDepth` | 16 | Nested `fldChar` state machine cap |
| `maxChartTicks` | 200 | Hard tick count cap (never step-loop on `majorUnit`) |
| `maxChartPoints` | 10,000 | Per-series point cap (iterated from present nodes only) |
| `maxDocFaces` | 24 (≤ 128 MiB) | Driver-side `LayoutEnv` face pin cap (bounds CJK faces) |
| `maxEmbeddedFonts` / `maxEmbeddedFontBytes` | 16 / 32 MiB total | Prevents `.odttf` fonts bypassing `FontProvider` LRU caps |
| `maxPageImagePixels` | 32 MP / 128 MiB RGBA (≤ 8 MP / 32 MiB per image) | Matches `0034` per-page image decode cap |

---

## 6. Editor & driver integration

### 6.1 Generalise `PdfView` → `PageView` (sequenced after in-progress `PdfStrip` work)

The current working tree extends `PdfView` ([`EditorState.hs`](../../../src/Cmedit/EditorState.hs#L430-L482)) with continuous fit-width strip scrolling (`PdfStrip.hs`: `pvFit`, `pvAnchor`, `pvOff`, `pvZoom`, `pvPanX`, `pvTiles`, `pvTileFailed`) and box text selection (`pvDrag`, `pvGlyphs :: IM.IntMap [Pdf.PdfGlyph]`, `pvBox`, `pvSel :: Maybe PdfBoxSel`). All of those are valuable for DOCX too, and only `pvGlyphs` names a PDF type (`Pdf.PdfGlyph`).

1. **Precondition**: land/commit the in-progress `PdfStrip` + `PdfSelectSpec` work first so Commit 0 starts from a clean tree.
2. **Commit 0 (pure mechanical rename + neutral `PageGlyph`)**:
   - Extract a format-neutral `PageGlyph` (page rect + text/char + reading order) from `Pdf.PdfGlyph` into `Cmedit.VecView` (or `Cmedit.PageStrip`).
   - Rename `PdfView` → `PageView`, `edPdfView` → `edPageView`, `docPdfView` → `docPageView`, `PdfViewMode` → `PageViewMode`, `PdfFit`/`PdfFitPage`/`PdfFitWidth` → `PageFit`/`PageFitPage`/`PageFitWidth`, `PdfDrag`/`PdfDragZoom`/`PdfDragSelect` → `PageDrag`/`PageDragZoom`/`PageDragSelect`, `PdfBoxSel` → `PageBoxSel`, `MATogglePdfPage`/`MAPdfFit`/`MAPdfDrag` → `MATogglePageView`/`MAPageFit`/`MAPageDrag` (menu labels `"&Page View"` / `"&Reflow View"`), `togglePdfPage` → `togglePageView`, and `PdfStrip` → `PageStrip`. `cfgPdfView` stays unchanged (alongside the new `cfgDocxView`).
   - `pageViewOn` drops `isJust (edPdf ed)` in favour of the invariant: `edPageView` is `Just` iff the active document has a graphical page source (`edPdf` or `edDocx`).
   - Roughly ~234 occurrences across ten source modules and `PdfViewSpec.hs` update mechanically; `make test`, `PdfSelectSpec`, and `pty_pdf.py` must pass unchanged before any DOCX integration code lands.
3. **DOCX gets both fit-width strip and box-select natively**:
   - **Fit-width strip (`PageStrip`)**: needs only `(w, h)` for each page index — supplied directly by `psSect` in `dxPages` (`Seq PageStart`) looked up in `DocModel`'s section table, with the strip bottom clamped to `Seq.length dxPages` while `not pvPagesKnown`.
   - **Box-select (`pvGlyphs`, `pvSel`)**: `DocPaint` emits `[PageGlyph]` directly from `PageLayout`'s positioned glyph runs alongside the `Scene`.
4. **DOCX-specific state lives in `edDocx :: !(Maybe DocxState)` / `docDocx`** (carried through `captureDoc`/`restoreDoc` and cleared in `blankReadOnly`), kept out of `PageView`:
   - `dxPath`, `dxMtime`, `dxModel :: Maybe DocModel`, `dxPages :: Seq PageStart`, `dxDone :: Bool`, `dxStaleRetries :: Int`, `dxParMap :: IntMap Int` (`RtfPar` index ↔ canonical `<w:p>` ordinal), `dxNotes :: [String]`, `dxError :: Maybe String`. (The non-`Show` `LayoutEnv` lives in `Drv`, keyed by `(path, epoch)`.)
5. **Minimal additions to `PageView`**:
   - `pvPagesKnown :: !Bool` — `True` immediately for PDF; `False` for DOCX while `JDocLayout` is streaming, so the status bar shows `Page 3/37…` (and `scrollBarInfo`/`scrollBarTo` work unchanged against `pvPages`).
   - `pvGoal :: !(Maybe PageGoal)` where `data PageGoal = GoalPar !Int | GoalPage !Int` — handles both jumping from Reflow View to a `<w:p>` ordinal beyond the currently paginated prefix *and* `Ctrl+G` to page 200 / `End` while only 37 pages are known. Advances as `SMDocPaged` batches arrive, and clears immediately on any manual page turn.

### 6.2 `Alt+T`, menu labels, place-keeping, and config

- **Place-keeping across `Alt+T`**:
  - Extend `Docx.hs` (keeping `docxPars` as a thin wrapper) with `docxParsIndexed :: ByteString -> (Seq RtfPar, IntMap Int, Bool)` recording the canonical `<w:p>` ordinal (§4.3) for each emitted `RtfPar`.
  - `PageStart` stores `psParOrd` (the canonical `<w:p>` ordinal at the top of each page).
  - **Page → Reflow**: look up `psParOrd` → nearest `RtfPar` index → `Rtf.rtfParLineRange` → set `rdTop` and record `pvLeftAt`.
  - **Reflow → Page**: map `rdTop` via `rdCache` → `RtfPar` index → `<w:p>` ordinal → binary search `dxPages` (or set `pvGoal = Just (GoalPar ord)` if pagination hasn't reached that ordinal yet). `pvLeftAt` semantics stay identical to PDF.
- **`Alt+T` and View menu**:
  - For a DOCX with `edPageView = Just _`, `Alt+T` toggles **Page View ↔ Reflow View** via `MATogglePageView`. In [`Editor.hs`](../../../src/Cmedit/Editor.hs#L1728-L1729), the `isJust (edPageView ed)` guard runs *before* `containerViewActive`.
  - `View ▸ Archive Contents` (`MAArchiveView`) remains enabled in the View menu with its `"Alt+T"` accelerator text stripped by `relabelEntry` when `edPageView` is present. From the archive listing, `Alt+T` still returns to the document view as today.
  - Why not a 3-way `Alt+T` cycle? Entering Archive Contents is an async driver round-trip that discards document view state; keeping `Alt+T` as an instant, place-preserving Page↔Reflow toggle matches PDF ergonomics.
  - `.epub`, `.odt`, and `.xlsx` behaviour is completely unchanged.
- **Find, selection, export, sessions, CLI**:
  - `Ctrl+F` in DOCX Page View switches to Reflow View and runs `rtfFindWith` (mirroring PDF).
  - Save As / Export, journal suppression, session restore, and CLI stdout (`cmedit file.docx > out.txt`) remain unchanged.
  - `pdfCapsArrived` and `pdfPageUpkeep` generalise by dispatch.
- **Configuration**:
  - Add `docx-view = auto|page|reflow` to `ConfigFile.hs`, `configKeysHelp`, and one row in `settingsSpec` (`EditorEdit.hs`/`Editor.hs`).
  - **Initial default: `reflow`**, flipping to `auto` once the oracle bars in §9 hold across both synthetic and real-world corpora. A slightly off page rendering as the default view would be a regression over the existing clean reflow view.
  - Update `CLAUDE.md`, `README.md`, `Cmedit.Manual`, and `HelpCard.hs`.

---

## 7. Hostile-file and OOXML edge-case checklist

Every item below must have a corresponding unit test or hostile fixture before release:

### Security & container hygiene
- **External relationships (`TargetMode="External"`)**: **never** fetch URLs, **never** open local filesystem paths, and **never** touch UNC paths (`\\server\share` leaks NTLM credentials on Windows). Ignore all external targets for media/parts (keep URL string only if needed for display notes).
- **Package path canonicalisation**: resolve relative relationship targets (`../media/image1.png`), decode `%XX` percent-encoding, reject path traversal escaping package root, and look up strictly in the ZIP central directory.
- **ZIP bomb / multi-part amplification**: enforce per-member `Zip.maxMemberBytes` (16 MiB), aggregate `maxDocXmlBytes` (64 MiB), and `maxDocParts` (256).
- **`mc:AlternateContent`**: select **exactly one** branch (`mc:Choice` if `Requires` namespaces are all supported by our parser, else `mc:Fallback`). Never process both (otherwise every DrawingML+VML shape draws twice).

### Cycle & recursion guards
- Style inheritance (`w:basedOn`, `w:link`, `w:next`), numbering links (`w:numStyleLink`, `w:styleLink`): visited `Set` + `maxStyleDepth`.
- Footnote/endnote references inside footnotes/endnotes or headers/footers: ignore.
- Nested tables (`maxTableNestDepth`), group shapes & text-box-in-text-box (`maxShapeNestDepth`), nested OMML (`maxMathDepth`), nested complex fields `fldChar` (`maxFieldNestDepth`).

### Guaranteed well-founded pagination progress
- **`PageStart` strictly advances in a well-founded order on every emitted page**: consuming a pending parity blank (`FlowPendingBlank`), an explicit break (`FlowPendingBreak`), a line, a table row slice, a footnote slice, or an endnote slice all strictly advance `PageStart`; no-op transitions are structurally impossible (which simultaneously permits legitimate empty pages from `evenPage`/`oddPage` or consecutive breaks while guaranteeing termination).
- Every line consumes **at least one grapheme/character cluster** (even if a single glyph exceeds column width).
- Enforce a minimum vertical accounting floor on line height (`max 1pt` in pagination accounting even if `w:line="1"` exact or `w:sz="1"`).
- Clamp geometry to Word's own specification limits: ≤ 63 table columns, ≤ 45 text columns, page dimensions 0.1–22 inches, margins clamped so printable width/height ≥ 0.5 inches.
- **Oversized atomic constructs**:
  - A `w:cantSplit` table row taller than the printable page splits anyway.
  - A `w:keepNext` chain taller than a page breaks cleanly rather than looping or overflowing.
  - A `w:tblHeader` block taller than 50% of the page height does not repeat on continuation pages.
  - Floating objects allow **at most one re-flow pass per page**, then pin in place.
  - Footnote continuation is strictly monotone (if remaining note text doesn't fit, spill remainder into `psFootnotes` for the next page with no backtracking retries).

### Attribute & numeric bombs
- Never allocate arrays or lists by a declared count attribute (`w:gridSpan="1000000"`, giant `w:tblGrid`, `c:ptCount val="1000000000"`). Iterate only the XML elements actually present, clamped by caps.
- Chart axis ticks are generated by bounded count (`<= maxChartTicks`), never a `while v <= maxV { v += majorUnit }` loop (guards `majorUnit="1e-300"`).
- Guard `chExt = 0` in DrawingML groups, `<= 0` values on logarithmic chart axes, degenerate pie slices (all-zero series), and division-by-zero / NaN in `a:prstGeom` guide formulas (`*/`, `?:`, `at2`, `sqrt`).
- Embedded `.odttf` fonts enforce `maxEmbeddedFonts` / `maxEmbeddedFontBytes` and allocate unique face IDs from `FontProvider`'s `fcNextKey` space (packed via `PdfRenderFont.glyphKey`).

### Commonly misimplemented OOXML semantics
- `w:sectPr` inside a paragraph's `w:pPr` describes the section **ending** at that paragraph (the final `w:sectPr` directly under `w:body` describes the last section).
- Section headers/footers inherit from the previous section when not explicitly overridden for that type (`default`/`first`/`even`).
- Conditional table styling (`w:tblLook` + `w:cnfStyle`: firstRow, lastRow, firstColumn, lastColumn, band1Horz, band2Horz, etc.).
- Hidden content: `w:del`, `w:moveFrom`, `w:vanish`, `w:webHidden` (do not render `w:del`/`w:moveFrom`/`w:vanish`).
- Per-character `w:rFonts` slot selection (`ascii` / `hAnsi` / `eastAsia` / `cs`).
- Rotated table cell text (`w:textDirection` `tbRl` / `btLr`).
- `w:spacing` before/after collapsing between adjacent paragraphs and `w:contextualSpacing` between same-style paragraphs.

---

## 8. Workstreams and milestone plan

Structured after `0034` for parallel execution in isolated worktrees, with each workstream owning its own `test/*Spec.hs` file:

### Pre-fan-out (Coordinator, gated by Fable review #1)
1. Commit in-progress `PdfStrip`/`PdfSelectSpec` work.
2. Freeze `Cmedit.DocModel`, `LayoutEnv` / `TextShape` interface, and `PageStart` / `PageLayout` types.
3. Land coordinator-owned extensions to `Cmedit.FontCore`, `Cmedit.FontTT` (vertical metrics + bounded GPOS PairPos), `Cmedit.FontSys` (`ClsMath`, `ClsWingdings`, PUA tables, CJK fallback), and `Cmedit.FontProvider` (embedded-font face-ID allocator), verified against `FontSpec` and `0034`'s PDF/SVG corpus (zero regression).
4. Land Commit 0: behaviour-free `PdfView` → `PageView` / `PdfStrip` → `PageStrip` / `MATogglePdfPage` → `MATogglePageView` rename + neutral `PageGlyph`, verified with `make test`, `PdfSelectSpec`, and `pty_pdf.py`.

### Parallel workstreams

| WS | Owned modules | Test spec | Responsibility |
|---|---|---|---|
| **A** | `XmlNs`, `Opc`, `DocxStyle`, `DocxNum`, `DocxParse` (plus shared body resolver & `docxParsIndexed` in `Docx.hs`) | `DocxParseSpec.hs` | Multi-part ZIP/OPC resolution, URI namespaces, full style/theme/numbering/settings cascade (resolving list labels in document order), `vMerge` → row-span resolution, fields, `mc:AlternateContent`, `.odttf` extraction → `DocModel` |
| **B** | `TextShape` | `TextShapeSpec.hs` | Per-char `rFonts`/fallback itemisation, single `Twip64` per-glyph advance quantisation, `w:kern`-gated GPOS/kern application, PUA mapping, UAX #14 + CJK line-break opportunities |
| **C** | `DocLayout`, `DocLayoutTable`, `DocLayoutFloat`, `DocPaint` | `DocLayoutSpec.hs` | Bounded unfold `layoutPage`, integer `Twip64` geometry, memoised header/footer measurement, justification/tabs, tables (grid/auto-fit/spans/collapsed borders/splits/rotation), floats, multi-column, footnotes/endnotes, `PageLayout` → `(Scene, [PageGlyph])` |
| **D** | `DocxDraw`, `DocxPreset`, `DocxVml` | `DocxDrawSpec.hs` | DrawingML shapes, `a:prstGeom` guide evaluator (~50–60 presets) + `a:custGeom`, groups, text boxes (via `DrawEnv` block-layout callback), SmartArt cached `drawing*.xml`, VML + `v:textpath` watermarks & OLE previews |
| **E** | `Chart` | `DocxChartSpec.hs` | Standalone 2D DrawingML charts (`chart*.xml` + theme → `(w, h, bl, [Cmd], [String])`) |
| **F** | `MathLayout` | `DocxMathSpec.hs` | Standalone OMML `MathBox` engine (`m:oMath`/`m:oMathPara` → `(w, h, bl, [Cmd], [String])`) |
| **G** | `EditorState`, `EditorDoc`, `Editor`, `App`, `Render`, `Menu`, `ConfigFile`, `Manual` | `DocxViewSpec.hs` + `pty_docx.py` | `edDocx`/`docDocx`, driver-side `LayoutEnv`, generalised `withArchive`, `JDocLayout` streaming checkpoints, `JPageRender`/`JPagePrefetch` dispatch, fit-width strip & box-select wiring, `Alt+T` place-keeping, `docx-view` config, mtime checks, scaled image decode & parking |
| **H** | `docs/plans/bench/docx/` (`gen_docx.py`, `DocLayoutCheck.hs`, `docxcheck.sh`, hostile suite) | Bench / CI script | Started **day one**: pinned-font corpus generator (`compatibilityMode=15`), ligature check, structural + pixel + Word-marker oracle, hostile corpus & fuzzer |

### Milestones & review gates

1. **M0 — Contract freeze & rename**: Contracts + font extensions + `PageView` rename + `gen_docx.py` synthetic corpus running through LibreOffice (and ligature decision settled). **Fable Review #1 (contract review).**
2. **M1 — Tier 1 end-to-end**: Workstreams A + B + C (Tier 1 core) + G wired together; real `.docx` files open in Page View (single-page + fit-width strip + box-select), paginate incrementally, toggle `Alt+T`, pass Tier 1 pinned-font oracle. **Fable Review #2 (layout & memory review).**
3. **M2 — Tier 2 + Tier 2.5 objects**: Floats, columns, footnotes/endnotes, `.odttf` (C) + DrawingML/SmartArt/VML (D) + Charts (E) + OMML (F) integrated into `DocPaint`. **Fable Review #3 (feature & security review).**
4. **M3 — Hardening & release**: Full hostile corpus, truncation/bit-flip fuzzing, `RASTER_SAFE`, `pty_docx.py`, `make soak`, `make windows-check`, docs updated. **Fable Review #4 (release audit).** Flip `docx-view` default from `reflow` → `auto` only after all M3 acceptance bars pass.

---

## 9. Validation, oracle strategy, and acceptance bars

Pixel error alone is noisy under substitute-font line-break drift, so validation measures **structure first and pixels second**:

### 9.1 Staged oracle (`docs/plans/bench/docx/`)

1. **Pinned-font synthetic corpus (`gen_docx.py`)**:
   - Generates raw OOXML fixtures using stdlib `zipfile` (no `python-docx` needed on the host) with `compatibilityMode=15` in `settings.xml`, covering every Tier 1 / 2 / 2.5 feature.
   - Uses only **Liberation** and **DejaVu** font families (confirmed identical between CMeDit and LibreOffice via `pdffonts` — note the host lacks Carlito/Caladea, so Calibri/Cambria documents drift in both renderers and belong in the real-world tier, not the strict pinned tier).
   - **Day-one ligature check**: verify whether LibreOffice applies OpenType `liga` on the pinned Liberation/DejaVu corpus while Word does not; settle explicitly in M0 by either disabling standard ligatures in the LibreOffice test fixtures (`w14:ligatures w14:val="none"` / avoiding `fi`/`fl` pairs in strict break tests) or adding bounded GSUB lookup type 4 support if needed.
   - Always runs `soffice` (`25.2.3`) with a throwaway `-env:UserInstallation=file:///tmp/...` profile, and **never** feeds hostile fixtures to `soffice`.
2. **Primary structural metrics (`DocLayoutCheck.hs` vs `pdftotext -bbox-layout`)**:
   - Page count agreement.
   - Page-break agreement.
   - Line-break agreement via sequence alignment of line-start words (so one early wrap shift doesn't penalise every subsequent line).
   - Word bounding-box displacement (median and p95) on agreeing lines.
   - Line-width error on identical lines (isolates advance & `w:kern`-gated GPOS kerning accuracy).
   - Baseline pitch error (isolates vertical font metrics & line-spacing accuracy).
3. **Secondary pixel metrics (`pdftocairo` vs `Cmedit.Raster`)**:
   - Direct AE comparison (percentage of pixels whose max RGB channel diff > 2 out of 255, matching `0034`'s `pdfrendercheck.sh`) on pages whose line/page breaks agree.
   - 24 dpi ink-density comparison (PIL/numpy) on pages with break drift.
4. **Word ground truth without Microsoft Word**:
   - Real Word-saved `.docx` files embed `w:lastRenderedPageBreak` markers and `<Pages>` in `docProps/app.xml`.
   - Score **both** CMeDit and LibreOffice against Word's own markers; the real-world acceptance bar is staying within a stated margin of LibreOffice's own disagreement with Word.
5. **Per-object oracles**:
   - **Shapes / presets / SmartArt / VML**: one object per blank page compared against LibreOffice via `pdftocairo` + bounding-box checks.
   - **Charts**: geometric & data-invariant assertions in `DocxChartSpec` (bar heights, pie angles, axis bounds, series count, label placement) plus visual sanity checks (LibreOffice chooses different tick rounding and legend padding than Word).
   - **OMML**: box-metric Spec tests in `DocxMathSpec` (baseline alignment, fraction bar centering, script clearance, matrix column alignment, delimiter growth) plus visual inspection (LibreOffice re-typesets OMML through StarMath with different metrics).

### 9.2 Acceptance bars

| Corpus / Test | Measure | Bar |
|---|---|---|
| Pinned-font synthetic corpus | Page count | Exact on ≥ 95% of documents |
| Pinned-font synthetic corpus | Line-break agreement (aligned) | ≥ 98% |
| Pinned-font synthetic corpus | Word displacement (agreeing lines) | p95 ≤ 1.0 pt |
| Pinned-font synthetic corpus | Pixel AE (agreeing pages vs `pdftocairo`, channel diff > 2/255) | Median ≤ 1.5% |
| Real-world Word corpus | Page-break & `<Pages>` agreement with Word markers | Within 5 percentage points of LibreOffice's own score |
| Text parity | `DocModel` body plain text vs `rtfPlainText . docxPars` (normalised: empty pars dropped, table cells `\t`-joined, excluding notes/text boxes/dynamic fields) | Exact match |
| Throughput (`-O2`, typical 20-page prose/table doc) | Time to page 1 ready & pagination rate | Page 1 ≤ 250 ms; steady-state ≥ 50 pages/s |
| Hostile corpus (`docx/hostile/`) | Crashes, hangs, OOM, external I/O | 0 exceptions, ≤ 1 s per file, 0 external opens/spawns |
| Per-part truncation & bit-flip fuzz | All parsers + layout + paint | 0 exceptions, bounded time |
| Bounds-checked raster (`-DRASTER_SAFE`) | Every corpus page | 0 out-of-range accesses |
| `pty_docx.py` (fake kitty + no-reply) | Page 1 open, PgDn, zoom, strip scroll, box-select, `Alt+T`, `Ctrl+Q` mid-pagination, close RSS | `Ctrl+Q` < 1 s mid-pagination; idle CPU 0%; RSS drops on close |
| Suite & portability | `make test`, `make windows-check`, `make soak`, `pty_pdf.py` | All green, 0 new warnings, zero PDF/SVG regressions |

---

## 10. Risks and mitigations

| Risk | Mitigation |
|---|---|
| **Scope sprawl across 8 workstreams** | Freeze the three contracts before fan-out; keep Charts, OMML, and DrawingML strictly non-paginating object renderers; gate each milestone with Fable review. |
| **Font metric / kerning / ligature drift breaks layout parity** | Land vertical font metrics (`hhea`/`OS/2`) and `w:kern`-gated GPOS PairPos in M0; settle `liga` on day one; measure baseline pitch and identical-line widths separately in the oracle. |
| **Float + table + footnote pagination loops** | Integer `Twip64` coordinates, well-founded `PageStart` progress invariant (§7), single re-flow cap per float per page, and `maxLinesPerPage`/`maxDocPages` hard bounds. |
| **Memory retention on large or multi-tab DOCX sessions** | Compact strict `PageStart` checkpoints (zero `Text` slices), `T.copy` detach on parse, lazy per-page media decode (`decodeJPEGScaled`), bounded driver-side `LayoutEnv`, `keepScenes` eviction of parked `DocModel`s, verified by RSS tests in `pty_docx.py` and `make soak`. |
| **Two-parser divergence (`Docx.hs` vs `DocxParse.hs`)** | Shared body-part resolver + canonical `<w:p>` ordinal + Spec plain-text parity check across the corpus; unifying reflow onto `DocModel` tracked as an explicit follow-up once Page View matures. |

---

## 11. Follow-ups (out of scope here, enabled by this plan)

- **Unify DOCX reflow view onto `DocModel`** (gives terminal reflow real resolved heading sizes, numbering strings, and table columns).
- **Graphical ODT / RTF / EPUB page views** reusing `DocModel` → `DocLayout` → `DocPaint` → `PageView`.
- **OpenType `GSUB`/`GPOS` shaping + UAX #9 BiDi** in `Cmedit.Font*` / `Cmedit.TextShape` (unlocks Arabic, Hebrew, and Indic scripts across SVG, DOCX, and ODT).
