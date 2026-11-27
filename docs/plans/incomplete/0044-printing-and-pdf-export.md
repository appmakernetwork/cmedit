# 0044 — Printing, print preview and PDF export (Linux, macOS, Windows)

**Theme:** capability — paper output for every document the editor can show,
printed *the way we render it*: text with highlighting, CSV grids, images,
SVG, PDF pages and (once `0043` lands) DOCX pages, through one pipeline that
also yields a pixel-accurate print preview inside the terminal and a
"Save as PDF" that works anywhere
**Status:** proposal
**Estimated effort:** 15–22 days across five phases (§9). Phase 1 + 3 alone
(text documents → preview → CUPS / Save as PDF) is a useful 8–10-day cut.
**Risk:** medium. Every piece is bounded and pure apart from the spooler
call, but the PDF writer must cover the whole `Scene` contract, and the
Windows backend is hand-rolled `gdi32`/`winspool` FFI that can only be
exercised on Windows.
**Depends on:** nothing to start. The preview reuses the PDF page view; if
`0043` Commit 0 (`PdfView` → `PageView`) has landed, this plan adds a page
source to `PageView`. If it has not, this plan performs that same rename
first (§6.1), so the two plans never diverge.
**Out of scope:** printing over SSH to the *local* machine (pass-through
`CSI 5 i`, terminal file transfer, reverse-forwarded CUPS). That is a
separate follow-up plan, deliberately: everything here is its prerequisite
(it needs the PDF writer and the dialog), and nothing here depends on it.

---

## 1. The problem and opportunity

CMeDit reads PDFs, Office documents, e-books, spreadsheets and every common
picture format, and draws them with a real vector rasteriser. It cannot put
any of them on paper. Neither can any other terminal editor: nano has no
print command, Vim's `:hardcopy` emits monochrome PostScript of a text
buffer, and Emacs' `ps-print` is similar. None of them print a picture, a
PDF page or a spreadsheet, and none show a preview.

The editor already owns the hard parts:

| Needed for printing | Already in the tree |
|---|---|
| A device-independent page description | `Cmedit.Scene` — PDF's imaging model as data (paths, glyph outlines, images, clips, groups, blend modes, soft masks, shadings) |
| PDF pages as page descriptions | `PdfRender.renderPage` → `Scene` |
| SVG as a page description | `Svg.parseSvg` → `Scene` |
| Pixels at any resolution | `Raster.renderScene` (exact-area coverage, bounded work and memory) |
| Real fonts | `FontProvider.fontFor`, `FontSys.substituteFor` (DejaVu/Liberation/Noto/Consolas/Courier New…) |
| Compression for a PDF | `Cmedit.Deflate.zlibCompress` (hand-rolled RFC 1950/1951, `0041`) |
| A page viewer with pixels, paging, zoom | The PDF page view (`PdfView`, `VecView`, kitty/sixel/half-block) |
| DOCX pages | `0043`: `DocPaint` → `(Scene, [PageGlyph])` per page |

What is missing is the part that turns "a document" into "sheets of paper",
a way to write those sheets as PDF, and a way to hand a job to the operating
system.

## 2. Ground rules

1. **One function decides what a sheet looks like, and every output uses
   it.** `sheetScene :: PrintCtx -> Int -> IO Scene` (§4.3) feeds the
   preview, the PDF writer and the Windows raster path. The preview is not a
   simulation of the printout; it *is* the printout's scene, rasterised for
   the terminal. A difference between preview and paper is a bug in a
   backend, never a second layout.
2. **Imposition is scene composition.** Scaling to the paper, rotation,
   margins, n-up, headers and footers are `CTransform`/`CClip`/`CFill` around
   the source page's `Scene`. No backend knows about n-up.
3. **The pure core is pure.** Pagination, imposition, the PDF writer, the page
   range parser, sheet ordering and option mapping are pure modules with Spec
   tests. IO is the driver's: fonts, re-reading a source file, the spooler.
4. **No new dependencies.** CUPS is driven through its command-line tools
   (`process`); Windows through `gdi32`/`winspool` FFI in the platform layer,
   in the style of `platform/windows/Cmedit/Term.hs`. There is no `network`
   package, so speaking IPP directly is not an option, and it is not needed.
5. **`src/` stays portable.** The CUPS driver is portable code (it only runs
   programs) and lives in `src/`; the Windows GDI driver lives in
   `platform/windows`; a new platform module `Cmedit.PrintSys` with identical
   export lists in both trees chooses (§7).
6. **Nothing prints by accident.** Printing costs paper and toner. Only the
   dialog's Print button (or Enter in the preview, which says so on its status
   line) submits a job; Enter on a focused choice row does not confirm, as in
   Settings.
7. **Printing never blocks the editor.** Listing printers, querying
   capabilities, rendering sheets and submitting are background jobs
   (`startJob`, cancellable, `guardJob`-wrapped), with a spinner and an OSC 9
   notification when the terminal is unfocused, like workspace search.
8. **A buffer prints as it is, not as it was saved.** The text buffer is a
   persistent `Seq`, so capturing it at Print time is O(1) and the user can
   keep typing while the job renders. File-backed sources (PDF, DOCX, image)
   are re-read with the `iwMtime` discipline: refused if the file changed
   since it was opened ("changed on disk — Revert, then print").

## 3. The dialog: our own, with a door to the system's

**Decision: a TUI print dialog of our own is the primary interface, with an
optional hand-off to the printer driver's native properties sheet on Windows
and a generated "Advanced" page of CUPS driver options on Linux/macOS.**

Why not hand everything to the system dialog:

- **Linux has no system print dialog a console process can open.** GTK's and
  Qt's dialogs are toolkit code (not boot libraries, not linkable here). The
  `xdg-desktop-portal` Print interface exists, but needs D-Bus and a
  graphical session, passes the document as a file descriptor (awkward
  through the `gdbus` CLI), and is absent on a server or a plain VT. A feature
  that works on one platform's desktop only is not the feature.
- **Half the settings are ours, not the printer's.** Font size, line numbers,
  syntax colours, wrap vs truncate, header/footer, "print selection", PDF
  scaling, CSV column grouping. No system dialog has those rows, so a dialog
  of our own is needed regardless; the only question is whether the printer
  half lives in it too. Splitting one job across two dialogs is worse than
  one dialog.
- **Page ranges must be in *our* page numbers**, the ones the preview shows.
  Our dialog shows "14 pages · 7 sheets" live as options change.
- **A GUI window opened by a console process is unreliable on Windows.** It
  often appears *behind* the terminal (foreground-lock rules), and under
  Windows Terminal `GetConsoleWindow()` is a hidden pseudo-window, a poor
  owner. Keyboard and mouse users of the editor would be thrown into another
  UI with different keys.
- **It is consistent and testable.** It is built on the dialog machinery
  Settings already uses (`Dialog.Choice` rows, section headers, `chNote`
  lines, scrolling, mouse hit-testing shared with the renderer), so it is
  covered by the PTY harness like every other dialog.

Where the system dialog *is* the right tool: **vendor-specific features**
(stapling, trays, finishing, secure-release PINs, toner saving). Those are
the printer driver's, and only the driver can present them. So:

- **Windows:** a **Printer Properties…** button calls
  `DocumentPropertiesW(…, DM_IN_BUFFER | DM_OUT_BUFFER | DM_IN_PROMPT)`, the
  driver's own property sheet, seeded with the `DEVMODE` our rows describe.
  The returned `DEVMODE` is kept for the job and read back into our rows
  (copies, duplex, paper, orientation, colour), so the two never disagree.
  It runs on a bound thread (§7.3) while the editor shows "Printer
  properties open in another window…".
- **Linux/macOS:** an **Advanced…** button opens a second dialog
  (`DKPrintAdvanced`) of choice rows generated from `lpoptions -p NAME -l`:
  every PPD option the queue exposes (`InputSlot`, `StapleLocation`,
  `cupsPrintQuality`, …) with its current default marked. Options our main
  rows already cover (`PageSize`, `Duplex`, `ColorModel`) are filtered out so
  there is one place for each. Choices go to `lp` as `-o Key=Value`.

### 3.1 Layout

Focus order is fixed by `Dialog` (fields → options → choices → buttons), so
the two typed fields come first. Rows are produced by a pure
`printSpec :: PrintTarget -> PrinterCaps -> PrintPrefs -> [RowSpec]`, the
`settingsSpec` pattern: one source of truth for what row *k* means, and the
row set depends on the document kind.

```
┌─ Print — report.py ──────────────────────────────────────────┐
│ Copies          [1   ]                                       │
│ Pages           [1-3, 7    ]       blank = all · 14 pages    │
│ ☑ Collate   ☐ Selection only   ☐ Reverse order               │
│ ── Printer ──                                                │
│ Printer         ‹ HP_LaserJet_M404 (default) ›               │
│                 idle · two-sided · A4, Letter · greyscale    │
│ Two-sided       ‹ Long edge (book) ›                         │
│ Colour          ‹ Greyscale ›                                │
│ ── Layout ──                                                 │
│ Paper           ‹ A4 ›                                       │
│ Orientation     ‹ Portrait ›                                 │
│ Pages per sheet ‹ 1 ›                                        │
│ Margins         ‹ Normal (15 mm) ›                           │
│ ── Text ──                                                   │
│ Font size       ‹ 9 pt ›                                     │
│ Line numbers    ‹ On ›                                       │
│ Highlighting    ‹ Colour ›                                   │
│ Long lines      ‹ Wrap ›                                     │
│ Header          ‹ Name · date · page N of M ›                │
│                                                              │
│   14 pages on 7 sheets                                       │
│ [ Print ] [ Preview ] [ Save as PDF… ] [ Advanced… ] [Cancel]│
└──────────────────────────────────────────────────────────────┘
```

Row sets by `PrintTarget`:

| Target | Kind-specific rows |
|---|---|
| Text, Markdown source, code | Font size, Line numbers, Highlighting (`Colour \| Black & white \| Off`), Long lines (`Wrap \| Cut`), Header |
| CSV / workbook sheet | Font size, Gridlines, Repeat header row, Column overflow (`Across then down \| Shrink to width`) |
| PDF (page view or reflow) | Scale (`Fit to printable area \| Actual size \| Shrink oversized`), Auto-rotate, Source (`As shown \| Original file`, CUPS only; §5.4) |
| Image / SVG | Scale (`Fit \| Actual size \| Fill page`), Auto-rotate |
| RTF / DOCX reflow / EPUB / ODT | Font size, Header (paragraph layout, §4.2) |
| DOCX page view (`0043`) | Scale, Auto-rotate |

Behaviour:

- **The dialog opens instantly.** The printer row starts as "finding
  printers…" and fills in when `JPrinters` returns (CUPS discovery of network
  printers can take seconds). The list is cached for the session and
  refreshed in the background on each open.
- **Choosing a printer queries its capabilities** (`JPrinterCaps`) and
  re-derives the dependent rows: paper sizes, whether "Two-sided" offers
  long/short edge or only **Manual (print odd, flip, print even)**, colour
  vs greyscale. A row whose value the new printer cannot do falls back to
  its default and the `chNote` line says why.
- **Live count.** `dlgMessage` shows "14 pages on 7 sheets" from the pure
  paginator (text pagination is O(lines) and cheap; for PDF it is the page
  count; for DOCX `0043`'s checkpoint count). An invalid page range shows the
  parse error there and disables Print.
- **"Save as PDF…"** is always present, also on a machine with no printers,
  where the dialog says "No printers found — Save as PDF still works". It
  goes through a Save As-style path prompt, including `askOverwrite`.
- **Preferences.** The dialog remembers its last values per session
  (`edPrintPrefs`, global like `edSearch`). Initial values come from config
  keys (§8), which also get a "Printing" section in Settings, following the
  usual "one key, one `settingsSpec` row, one `applySettingRow` case" rule.
- **Entry points:** File ▸ **P&rint…** and File ▸ Print Pre&view; Ctrl+Shift+P
  where the Kitty keyboard protocol delivers it (Ctrl+P stays Quick Open);
  `MAPrint`/`MAPrintPreview` guarded in `runAction` for documents with nothing
  to print (an archive listing is printable text, so nearly everything is).

## 4. The pipeline

```
   Editor document ──▶ PrintSource ──▶ source pages (Scene + optional text layer)
                                  │
                                  ▼
            PrintLayout (pure): page range → sheets, n-up, scale, rotate,
                                margins, header/footer, duplex ordering
                                  │
                                  ▼
                       sheetScene :: PrintCtx -> Int -> IO Scene
                ┌─────────────────┼────────────────────────┐
                ▼                 ▼                        ▼
       PageView preview    Cmedit.PdfWrite           Raster.renderScene, banded
       (kitty/sixel/cells)  (Scene → PDF bytes)      at device DPI (Windows GDI)
                                  │
                      ┌───────────┴───────────┐
                      ▼                       ▼
                 CUPS `lp FILE`         Save as PDF / `--pdf`
```

### 4.1 `PrintSource`: what a document contributes

```haskell
-- Cmedit.Print (pure)
data PrintSource
  = SrcText    !TextJob      -- buffer snapshot, language, tab width, selection
  | SrcGrid    !GridJob      -- CSV / workbook sheet: rows, column widths, header
  | SrcParas   !ParaJob      -- RtfPar paragraphs (RTF, DOCX/EPUB/ODT reflow, Markdown 0039)
  | SrcPdf     !FilePath !Int !Int   -- path, epoch, page count
  | SrcPicture !FilePath !Int        -- image or SVG: one page
  | SrcDocx    !FilePath !Int        -- 0043 page source
```

Pure sources (text, grid, paragraphs) are captured from the `Editor` at
Print time; file-backed ones name the file and its epoch, and their pages are
produced driver-side by the same jobs the page view uses (`renderPage` for
PDF, `DocPaint` for DOCX).

### 4.2 Pagination of text-like sources (`Cmedit.PrintText`)

- **Text** is laid out in points with one monospace face (`fontFor` with the
  family `monospace` → `substituteFor`: DejaVu Sans Mono, Liberation Mono,
  Consolas, Courier New, Menlo). Layout needs only the advance and the line
  height, so `paginateText :: TextMetrics -> TextOpts -> TextJob -> [PageBreak]`
  is pure and cheap. A `PageBreak` is (first buffer line, first segment), so
  page *N* is drawn without laying out pages 1..N−1 again. Column arithmetic
  reuses `Cmedit.Width` (`charWidth`, tab stops): a wide glyph takes two
  advances, exactly like two cells. Wrapped continuation lines get a `↪` in
  the line-number gutter.
- **Glyph coverage:** a code point the monospace face lacks goes to a fallback
  chain (`Noto Sans Mono CJK`, `Noto Sans`, `DejaVu Sans`, via
  `substituteFor`), still at the monospace advance (CJK at two). Anything still
  missing draws a `.notdef` box and is counted in the sheet's `scNotes`
  ("3 characters have no font").
- **Highlighting on paper is not the screen theme.** Dark themes on white
  paper waste toner and read badly, so `printTokens :: Tok -> PrintStyle` is a
  dedicated palette (the light theme's dark hues as exact RGB, bold keywords,
  italic comments). Black & white keeps bold/italic and maps every colour to
  black, comments to 45% grey. Off is plain black.
- **Selection only** prints `selLineSpan` (columns honoured on its first and
  last line). Header/footer: file name (or "Untitled"), the date (passed in
  by the driver: the module stays pure), "Page N of M".
- **Grids** (`SrcGrid`) reuse `csvWidths` (in characters) as column widths in
  advances, cap each at a fraction of the printable width, and split columns
  into groups that fit. "Across then down" prints every column group of a
  row band before the next band, repeating the header row (and the row
  numbers, when shown) on each sheet.
- **Paragraphs** (`SrcParas`): RTF, the DOCX/EPUB/ODT reflow model and
  `0039`'s Markdown view all produce `Seq RtfPar`. They are typeset with a
  proportional face (serif or sans by the run's family, bold/italic/underline,
  alignment, indent, `rpSpace`) by a greedy line breaker measured in font
  advances. This is a deliberately simple typesetter, meant to make those
  documents printable before `0043`. When `0043` lands, DOCX prints from its
  real pages instead (`SrcDocx`). RTF/EPUB/ODT keep this path until the
  `0043` follow-up moves them onto `DocModel`.

Every text-like source paints to a `Scene` (`CGlyphs` runs from the face's
outlines, `CFill` for gridlines and the header rule) **plus a text layer**:
`[PageGlyph]`-shaped (rectangle, characters), used by the PDF writer for
searchable output (§5.3). This is the shape `0043` introduces for box-select;
until it exists, `Cmedit.Print` defines it and `0043` adopts it.

### 4.3 Imposition (`Cmedit.PrintLayout`, pure)

```haskell
data Paper  = Paper { ppName :: !Text, ppW, ppH :: !Double }   -- points
data Sides  = OneSided | LongEdge | ShortEdge | ManualDuplex
data Scale  = FitPrintable | ActualSize | ShrinkOversized | FillPage

sheetsFor  :: PrintSettings -> Int {- source pages -} -> Either String [Sheet]
data Sheet = Sheet { shSlots :: [(Int, Affine)], shSide :: !SheetSide }

impose     :: PrintSettings -> Sheet -> [(Int, Scene)] -> Scene
```

- `parseRanges :: Text -> Either String [Int]` accepts `1-3, 7, 10-`, `-5`,
  spaces and repeats, and reports the column of an error.
- `sheetsFor` applies range → reverse → n-up grouping (1/2/4/6/9/16, the
  grid rotated to fit the page's aspect) → copies/collate (only for backends
  that cannot do copies themselves; CUPS and GDI can) → manual-duplex
  ordering (all fronts, then the backs in reverse, with a pause, §5.5).
- Each slot's `Affine` scales the source page into its cell of the printable
  area (honouring `Scale` and auto-rotate), and `impose` wraps each page
  scene in `CTransform` + `CClip` to its cell. The paper colour is left to
  the printer: a sheet's scene has no background fill, so PDF output does
  not carry a white rectangle per page.
- `sheetScene` is the IO wrapper: fetch the slots' source scenes (cached
  per job; a 2-up job reads each PDF page once), then `impose`.

## 5. Outputs

### 5.1 `Cmedit.PdfWrite`: `Scene` → PDF

The inverse of `PdfRender`, and tractable because `Scene` was modelled on
PDF's imaging model. It **streams**: `beginPdf`, `writeSheet`, `endPdf` each
return a `Builder` chunk plus the writer state (object numbers, xref
offsets, the resource cache), and the driver writes each chunk to the file
as it comes, so memory is one sheet plus the cache, whatever the length of
the job. Objects referenced from pages before they are written (fonts,
shared forms) get numbers reserved up front, which PDF's cross-reference
table allows.

| `Cmd` / `Paint` | PDF |
|---|---|
| `CFill`, `CStroke` | path operators, `f`/`f*`, `S`, width/cap/join/miter/dash; `ssAdjust` → `SA true` in an ExtGState |
| `PSolid` | `rg`/`RG` (or `g`/`G` when grey); alpha → ExtGState `ca`/`CA` |
| `PLinear`, `PRadial` | Shading types 2/3 with stitched type 2 functions from the stops; `SpreadPad` → `/Extend`; repeat/reflect → a stitched function over the expanded domain the device can see |
| `PTiling` | tiling pattern (PaintType 1) whose cell is the sub-scene |
| `CGlyphs` | Type 3 fonts built from the glyph outlines (§5.2) |
| `CImage` | image XObject, `FlateDecode` with the PNG Up predictor; alpha → `/SMask`; `InterpSmooth` → `/Interpolate true` |
| `CStencil` | `/ImageMask true` image with the paint as fill colour |
| `CGouraud`, `CGouraudRamp` | shading type 4 (free-form triangle mesh); ramp via `/Function` |
| `CClip` | `q`, clip paths with `W n`/`W* n`, children, `Q` |
| `CGroup` | transparency group form XObject: `/Group << /S /Transparency /I /K >>`, `/BM`, `ca`; `goMask` → `/SMask` (luminosity or alpha, `/BC`, `/TR` as a sampled type 0 function); a `defaultGroup` is inlined |
| `CTransform` | `q cm … Q`; a sub-scene referenced more than once becomes a form XObject, once |
| `goFilter` (SVG filters) | not expressible: the group is rasterised at the job's DPI into an image XObject (the one fallback, counted in a note) |

Two rules carry the whole writer:

- **Sharing is preserved, by identity.** A scene shares sub-scenes through
  `CTransform` and the nesting multiplies (the hostile Type 3 PDF in
  `docs/plans/bench/pdf/hostile/` is a small heap and an astronomically large
  tree). The writer visits each command list once per heap object with the
  `StableName` technique of `VecView.measureScene`, emits a form XObject for
  any list reached twice, and is fuelled like that walk. Writing the expanded
  tree would turn a 10 KB PDF into gigabytes.
- **Anything inexpressible is rasterised, never dropped.** The fallback is
  local (one group, at the job's DPI, through `Raster.renderScene`) and
  counted, so a filter-heavy SVG prints correctly and the dialog says why the
  file is large.

Output is PDF 1.4 (transparency groups need it), with an info dictionary
(title = document name, producer = "CMeDit x.y"), `/MediaBox` per sheet, and
no encryption. Numbers are written with at most 4 decimal places and trailing
zeros stripped.

### 5.2 Glyphs as Type 3 fonts

`Glyph` carries an outline and a front-end cache key (`Scene`'s contract:
equal keys promise equal paths). Each distinct key becomes one CharProc in a
Type 3 font of up to 256 codes. A run becomes `BT … Tf … Tm … Tj … ET` with
one-byte codes, and further fonts are opened as the codes run out. CharProcs
are written the first time a key is used, and the font dictionaries (with
their `/Widths` and `/FontMatrix`) at the end. A key `< 0` ("never cache")
draws as an inline path. Text is therefore compact (two bytes per glyph plus
positioning) and drawn exactly as the preview draws it, by whichever face
`fontFor` chose, with no TrueType subsetting at all.

### 5.3 A searchable text layer

`Glyph` has no Unicode, and the `Scene` contract is not changed for this (it
is coordinated across four modules). Searchable, copyable output uses the
technique OCR'd PDFs use: an **invisible text layer** (`3 Tr`) laid over the
drawn glyphs from the source's `PageGlyph`s. The font is a generated
glyph-less Type 0/Identity-H font with a `/ToUnicode` CMap and `/W` widths
fitted to each glyph's rectangle (Tesseract's GlyphLessFont, written from
the PDF specification). The text paginator supplies the layer in phase 1,
PDF and DOCX pages from their `PageGlyph`s in phase 5. `pdftotext` on our
output must reproduce the printed text; that is the test.

### 5.4 CUPS (Linux, macOS): `Cmedit.PrintCups`

| Purpose | Command (always with `LC_ALL=C`, through `runToolCapture`'s timeout) |
|---|---|
| Destinations | `lpstat -e` (includes driverless IPP printers CUPS discovered) |
| Default | `lpstat -d` → `system default destination: NAME` (or `$LPDEST`/`$PRINTER`) |
| State | `lpstat -p NAME` → idle / printing / disabled + reason |
| Capabilities | `lpoptions -p NAME -l` → `PageSize`, `Duplex`, `ColorModel` and every other PPD option, `*` marking the default |
| Submit | `lp -d NAME -t TITLE -n COPIES -o collate=true -o sides=two-sided-long-edge -o media=A4 -o print-color-mode=monochrome [-o Key=Value…] FILE` → `request id is NAME-123 (1 file(s))` |

- **The PDF is written to a private temporary file first, then `lp FILE`.**
  Piping to `lp`'s stdin would make cancellation submit a truncated job.
  With a file, cancelling before `lp` runs submits nothing, and once `lp` has
  returned, `cupsd` has its own copy, so the temporary file is deleted.
- Paper sizes map from IPP `media` names (`iso_a4_210x297mm`) and PPD names
  (`A4`, `Letter`) to points; our margins always sit inside the PPD
  `ImageableArea` when it is known.
- `-o fit-to-page` and `number-up` are **never** passed: we already imposed,
  and letting CUPS scale again would break the preview's promise.
- **PDF source, "Original file":** for a PDF we can also send the file itself
  (`-o page-ranges=`, `-o number-up=` then *are* CUPS's), which CUPS's
  poppler/Ghostscript chain prints with full fidelity and a much smaller spool
  file for image-heavy documents (our `Scene` holds decoded pixels, so
  re-encoding a scanned document with Flate inflates it; §10). The default
  stays **As shown**, the preview's promise, and the row's note gives both
  spool sizes when they differ by more than 4×.
- The job ends with "Sent 7 sheets to HP_LaserJet_M404 (job 123)" on the
  status line. A non-zero exit shows `lp`'s stderr. A missing `lp` makes the
  printer row say "CUPS not installed (lp not found)" and leaves Save as PDF.

### 5.5 Manual duplex

For a simplex printer, `ManualDuplex` submits the fronts as one job, then
opens a `DKPrintFlip` dialog ("Turn the printed pages over, put them back in
the tray, then press Continue"; Continue / Cancel), then submits the backs in
reverse order. It is identical on every backend, because it is only sheet
ordering plus a pause.

### 5.6 Windows: GDI raster (`platform/windows/Cmedit/PrintSys.hs`)

Windows has no universal "accepts PDF" print path, so the Windows backend
rasterises each sheet and draws it through GDI:

| Purpose | API |
|---|---|
| Destinations | `EnumPrintersW` (`PRINTER_ENUM_LOCAL \| PRINTER_ENUM_CONNECTIONS`, level 4), `GetDefaultPrinterW` |
| Capabilities | `DeviceCapabilitiesW` (`DC_PAPERS`, `DC_PAPERNAMES`, `DC_PAPERSIZE`, `DC_DUPLEX`, `DC_COLORDEVICE`, `DC_COPIES`, `DC_COLLATE`) |
| Settings | `DocumentPropertiesW` (`DM_OUT_BUFFER` for the default `DEVMODE`; then our fields: `dmPaperSize`, `dmOrientation`, `dmDuplex`, `dmCopies`, `dmCollate`, `dmColor`; `DM_IN_PROMPT` for Printer Properties…) |
| Job | `CreateDCW(L"WINSPOOL", name, NULL, devmode)`, `StartDocW`, per sheet `StartPage` / bands / `EndPage`, `EndDoc`; `AbortDoc` on any exception (`bracket`) |
| Geometry | `GetDeviceCaps`: `LOGPIXELSX/Y`, `PHYSICALWIDTH/HEIGHT`, `PHYSICALOFFSETX/Y`, `HORZRES/VERTRES` |
| Pixels | `StretchDIBits` with a top-down 24-bit `BITMAPINFO`, one horizontal band at a time |

- **Banded rendering bounds memory.** An A4 sheet at 600 dpi is 4960 × 7016
  pixels, 557 MB as the rasteriser's premultiplied `Float` canvas. Each sheet
  is rendered in bands of `printBandRows` (256) by offsetting `roView`, each
  band converted to BGR and sent before the next is drawn. The resolution is
  `min devicedpi (print-dpi)`, default 300, and `StretchDIBits` scales to the
  device. A glyph cache shared across a sheet's bands (§10) keeps text from
  being re-flattened per band.
- The `PHYSICALOFFSET` is subtracted so our margins measure from the paper
  edge, not the printable area's.
- Link with `-lgdi32 -lwinspool` (Makefile `windows` target; `extra-libraries:
  gdi32 winspool` under `if os(windows)` in `cmedit.cabal`). GHC's mingw
  toolchain ships both import libraries.
- **"Microsoft Print to PDF" makes the backend testable without paper**: it
  is a GDI printer present on every Windows 10/11 machine, and its output
  opens in our own PDF view.
- Rejected for now: GDI vector output (paths via `BeginPath`/`PolyBezierTo`,
  no usable transparency, every driver flattens differently) and the XPS
  Print API (COM, unreasonable through hand-rolled FFI). Raster is the one
  path whose output is the preview's pixels.

### 5.7 Save as PDF and the command line

- File ▸ Print ▸ **Save as PDF…** runs `PdfWrite` to the chosen path. This is
  also the honest answer over SSH until the follow-up plan exists.
- `cmedit --pdf FILE > out.pdf` joins the converter mode (`App.convertFiles`,
  `hIsTerminalDevice stdout`): same `classifyFileWith`, same `PrintSource`,
  default settings from the config, `--paper`, `--pages`, `--font-size`,
  `--landscape`. It refuses to write PDF to a terminal ("redirect stdout").
- `cmedit --print FILE [--printer NAME]` prints non-interactively and prints
  the job id to stderr.

## 6. Editor and driver integration

### 6.1 Preview is a page view over a print job

Print Preview opens a **read-only pseudo-document**,
`cmedit://print-preview/<n>` (the manual's trick: a scheme that cannot collide
with a real path, never recorded in recents or the session), whose page
source is the print job (`edPrintPv :: Maybe PrintPreview` / `docPrintPv`,
through `captureDoc`/`restoreDoc` and cleared in `blankReadOnly`). Everything
else is the page view: placement, fit and zoom, `[`/`]`/PgUp/PgDn, Go To page,
the scrollbar, the status zone ("Sheet 3/7 · A4 · 2-sided"), the half-block
fallback on a dumb terminal, prefetch of the next sheet. Its sheet jobs
(`JPrintSheet`, keyed by (job id, sheet)) run `sheetScene`, the same function
the backends call.

- If `0043` Commit 0 has landed, this is a third `PageView` source beside
  `edPdf` and `edDocx`. If not, this plan does that commit first (the
  mechanical `PdfView` → `PageView` rename, verified by `make test`,
  `PdfSelectSpec` and `pty_pdf.py`), exactly as `0043` §6.1 specifies, and
  `0043` starts from it.
- The status line reads "Print preview — Enter: Print · P: Settings · Esc:
  close". **P** (and Esc, then File ▸ Print) returns to the dialog with the
  same settings. Enter prints; it is the one place outside the dialog that
  can, and it says so.
- Changing a setting in the dialog while a preview is open re-keys the
  preview (new job id), so a stale sheet is never shown.

### 6.2 State, effects, jobs

- `Editor`: `edPrintPrefs :: PrintPrefs` (global, session-long),
  `edPrinters :: PrinterList` (`Loading | Listed [PrinterInfo] | Failed`),
  `edPrinterCaps :: Map Text PrinterCaps`, `edPrintJob :: Maybe PrintStatus`
  (the spinner and progress).
- Dialogs: `DKPrint`, `DKPrintAdvanced`, `DKPrintFlip`. The target document
  is captured when the dialog opens (`PrintTarget` held beside the dialog,
  like `edSaveAsTarget`), so switching documents behind it cannot print the
  wrong one.
- Effects: `EffListPrinters`, `EffPrinterCaps name`, `EffPrinterProps name
  devmode` (Windows), `EffPrint PrintRequest`, `EffPrintPdf PrintRequest
  path`, `EffCancelPrint`. Callbacks: `printersListed`, `printerCapsLoaded`,
  `printProgress k n`, `printDone (Either String Text)`, `printFlipNeeded`.
- Jobs (`App.JobKind`): `JPrinters`, `JPrinterCaps`, `JPrintSheet`, `JPrint`.
  `JPrint` renders sheets one at a time and posts `SMPrintProgress` over the
  search queue; cancelling it (Esc on the status zone, or File ▸ Cancel
  Printing while a job runs) kills it before submission or calls `AbortDoc`.
- `PrintRequest` is pure data (source, settings, printer, the `DEVMODE` bytes
  on Windows, the date string), so `Editor` keeps deriving `Show` and the job
  rebuilds fonts and scenes on its own thread.

### 6.3 Fonts and the fontless machine

Text pages need a face. Every Windows and macOS machine has one. A minimal
Linux server may have none (`providerFiles` returns `Just []`). Then:

- the preview says "No fonts installed — preview unavailable" and shows the
  half-block thumbnail of the layout boxes only;
- the PDF writer switches the text layer from invisible to **visible standard
  14 Courier** (no embedding, WinAnsi; characters outside it print as `?`,
  counted in a note), which every PDF consumer and CUPS can draw. The
  monospace advance is Courier's 0.6 em, so the pagination is unchanged.

## 7. Platform layer

### 7.1 `Cmedit.PrintSys` (two implementations, identical exports)

```haskell
listPrinters  :: IO (Either String [PrinterInfo])
printerCaps   :: Text -> IO (Either String PrinterCaps)
printerProps  :: Text -> PrinterDevmode -> IO (Maybe PrinterDevmode)  -- POSIX: pure Nothing
submitSheets  :: SubmitSpec -> Int -> (Int -> IO Scene) -> (Int -> IO ()) -> IO (Either String Text)
```

`platform/posix/Cmedit/PrintSys.hs` delegates to `Cmedit.PrintCups` (in
`src/`, since it is portable). `platform/windows/Cmedit/PrintSys.hs` holds the
GDI code. `submitSheets` takes a sheet producer and a progress callback, so
neither backend knows about documents, and on POSIX it is "`PdfWrite` to a
temporary file, then `lp`". The Makefile and `cmedit.cabal` pick the module
with `-i` / `if os(windows)` exactly as for `Cmedit.Term`, and `make
windows-check` typechecks it.

### 7.2 macOS

macOS builds the POSIX tree and ships CUPS with the same `lp`/`lpstat`/
`lpoptions`, so it needs nothing beyond a manual test pass.

### 7.3 Bound threads on Windows

GDI device contexts and the driver's property sheet are tied to the OS thread
that created them, and GHC's green threads migrate between OS threads. Every
`PrintSys` Windows entry point therefore runs under `runInBoundThread`, and
`JPrint` forks with `forkOS` there, not `forkIO`. A Spec-level check is
impossible; the Windows manual test plan (§9.3) covers it.

## 8. Configuration

All keys follow the `Config`/`applyKey`/`configFields`/`configKeysHelp`/
`settingsSpec`/`applySettingRow` procedure, in a new Settings section
"Printing":

| Key | Values | Default |
|---|---|---|
| `print-printer` | a destination name, or empty | empty (system default) |
| `print-paper` | `auto \| a4 \| letter \| legal \| a3 \| a5` | `auto` (the printer's default) |
| `print-sides` | `one-sided \| long-edge \| short-edge` | `long-edge` when the printer can, else one-sided |
| `print-colour` | `colour \| greyscale` | `colour` |
| `print-font-size` | 6–16 | 9 |
| `print-line-numbers` | `on \| off` | `on` |
| `print-highlight` | `colour \| mono \| off` | `colour` |
| `print-wrap` | `wrap \| cut` | `wrap` |
| `print-header` | `full \| name \| off` | `full` |
| `print-margins` | `narrow \| normal \| wide` (10/15/25 mm) | `normal` |
| `print-dpi` | 150–600 (Windows raster) | 300 |

## 9. Phases, tests and acceptance

### 9.1 Phases

| Phase | Content | Effort |
|---|---|---|
| **P1** | `Cmedit.Print`, `PrintText` (text + grids), `PrintLayout`, `parseRanges`, print palette; the `PageView` source (or `0043` Commit 0); Print Preview for text and CSV | 3–4 days |
| **P2** | `Cmedit.PdfWrite`: every `Cmd`/`Paint`, sharing by identity, Type 3 glyphs, rasterised fallback, invisible text layer, Courier fallback; Save as PDF; `--pdf` | 4–6 days |
| **P3** | `DKPrint`/`DKPrintAdvanced`/`DKPrintFlip`, `PrintCups`, `PrintSys` (POSIX), jobs/effects, manual duplex, config keys + Settings section, `--print` | 2–3 days |
| **P4** | Windows `PrintSys`: enumeration, capabilities, `DEVMODE`, Printer Properties…, banded GDI raster, bound threads | 3–4 days |
| **P5** | PDF, image and SVG sources (scale/rotate, "Original file"), paragraph typesetter for RTF/DOCX-reflow/EPUB/ODT/Markdown, `SrcDocx` once `0043` lands, PDF/DOCX text layers from `PageGlyph` | 3–5 days |

### 9.2 Tests (`make test`, bench scripts)

- **Pagination (Spec):** lines per page for each font size and margin; wrap
  at width with tabs and CJK (two advances, never split); `↪` continuation;
  selection-only bounds; header totals; grid column groups and repeated
  header rows; a 200 000-line buffer paginates in well under 100 ms
  (pinned, since the dialog's live count calls it).
- **Ranges and ordering (Spec):** `parseRanges` accept/reject table with error
  columns; `sheetsFor` for n-up × reverse × collate × manual duplex (fronts
  then reversed backs, odd counts get a blank back).
- **PDF writer (Spec):** round trip, Scene → `PdfWrite` → our `Pdf`/
  `PdfRender` → `Raster`, against `Raster` of the original Scene (max
  per-channel difference ≤ 2 outside antialiased edges), for a generator of
  random scenes covering every `Cmd`, `Paint`, blend mode and mask kind; the
  output always re-parses; the hostile shared-scene fixture writes a file
  under 1 MB in bounded time (fuel); a 200-page text job's writer state stays
  flat (live-heap check, the `0042` style).
- **External oracles (`docs/plans/bench/printcheck.sh`):** `qpdf --check` and
  `pdfinfo` accept every output; `pdftoppm` of our PDF against our own raster
  of the same sheet (the `pdfrendercheck.sh` harness, reversed); `pdftotext`
  of a text job reproduces the source text (text layer), and of a fontless
  job its Courier text.
- **CUPS (Spec):** a fake `lp`/`lpstat`/`lpoptions` on a temporary `PATH` (the
  `detectLinters` fake-`PATH` technique) records argv and the submitted file;
  checks option mapping, `LC_ALL=C`, the request-id parse, the localised-
  output guard, a hanging `lpstat` hitting the timeout, and that cancellation
  before submission runs no `lp` at all.
- **PTY (`docs/plans/bench/pty_print.py`):** open a file, File ▸ Print,
  change rows (the live sheet count changes), Preview under a fake kitty
  terminal (a placement appears, `]` turns sheets) and under no replies
  (half-block), Print with the fake `lp` (argv checked), cancel mid-job,
  manual duplex flip dialog, no-printer machine (Save as PDF still offered),
  and that Enter on a focused choice row never submits.
- **Real printers (manual, recorded in the plan when done):** a CUPS
  `cups-pdf` virtual queue; one real duplex laser; Windows "Microsoft Print to
  PDF" and one real Windows printer, including Printer Properties… round-trip
  and cancel mid-job (`AbortDoc` leaves no stuck spooler entry).

### 9.3 Acceptance bars

1. For every source kind, the preview's pixels and the PDF's `pdftoppm`
   rendering agree within the round-trip tolerance. **The preview is the
   printout.**
2. A 1 000-page text job prints with peak RSS within 40 MB of the idle editor
   (streaming writer, one sheet live).
3. The editor stays interactive (no frame over 50 ms) while any print job
   renders, lists printers or queries capabilities.
4. No path submits a job except the Print button, Enter in the preview, and
   `--print`.
5. `make windows-check` clean; the Windows manual pass is complete.

## 10. Risks and mitigations

| Risk | Mitigation |
|---|---|
| **Spool size for image-heavy PDFs.** `Scene` holds decoded pixels; Flate cannot match the source's JPEG/JBIG2 | "Original file" on CUPS with both sizes shown; follow-up: let `CImage` carry an optional source-stream hint (a coordinated `Scene` contract change) so the writer can re-embed DCT streams untouched |
| PDF constructs with no exact PDF equivalent (SVG filters, some spreads) | Local rasterisation at job DPI, counted in a note; round-trip tests pin the rest |
| Shared sub-scenes exploding the output | Identity-memoised form XObjects plus fuel, pinned by the hostile fixture |
| `lpstat` hangs on network discovery, or output is localised | Background job with timeout; `LC_ALL=C`; destination names from `lpstat -e`, which is not localised |
| Windows drivers rejecting large DIBs, or GDI thread affinity | 256-row bands; `runInBoundThread`/`forkOS`; `AbortDoc` in `bracket` |
| Banded raster re-flattening glyphs per band | A per-sheet glyph cache in `RenderOpts` (a field added through `defaultRenderOpts` + record update, per the rasteriser's rule) |
| No fonts on a Linux server | Standard 14 Courier text path (§6.3), stated in the dialog |
| Wasted paper from an accidental keypress | Rule 6; Enter on a choice never confirms; PTY test pins it |
| Preview drifting from output | One `sheetScene`; acceptance bar 1 |

## 11. Follow-ups (out of scope here)

- **Printing over SSH to the local machine**: VT pass-through printing
  (`CSI 5 i` … `CSI 4 i`, opt-in, gated on a `CSI ? 15 n` → `?10n` printer
  status reply folded into `Caps`), terminal file transfer (iTerm2
  `OSC 1337 File=`, kitty's file-transfer protocol; XTVERSION-whitelisted),
  and a `print-cups-server` key for `ssh -R` reverse-forwarded CUPS (`lp -h`).
  All three reuse this plan's PDF writer and dialog unchanged.
- A thumbnail of the current sheet inside the print dialog (needs a pixel
  placement owned by a dialog, which the overlay rules currently forbid).
- Re-embedding source JPEG streams (§10).
- Printing several open documents as one job; booklet imposition.
