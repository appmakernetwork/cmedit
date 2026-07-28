# 0021 — Office and e-book reading views: `.xlsx`, `.docx`, `.epub`

**Theme:** capability — "it opens everything", cashing in the ZIP + layout investment
**Status:** ✅ **RESOLVED** — implemented 2026-07-26
**Risk (as shipped):** low — every new view is read-only and derived, and the
graceful floor for any failure is the archive listing, which already worked

## Resolved

All three views shipped, together with the two shared prerequisites. What
landed matches the plan's shape; the differences from what was written below
are recorded at the end of this section.

Measured against the real binary through a PTY at 40×110:

| File | Result |
|---|---|
| 20 000-paragraph `.docx` (2.2 MB `document.xml`) | opens in **103 ms**, 51 MB RSS |
| 100 000-cell `.xlsx` (2.9 MB worksheet) | opens in **249 ms**, 55 MB RSS |
| 8-chapter `.epub` | opens in **3 ms**, 29 MB RSS |
| damaged/truncated/empty container of each format | opens as the **listing** with a `⚠` note, never an error |

**Two shared prerequisites, as planned.**

* **`Cmedit.Xml`** — a non-validating pull parser (`XStart`/`XEnd`/`XText`),
  entity and CDATA handling, comments/PIs/DOCTYPE skipped, matching on **local
  names**, BOM-driven UTF-8/UTF-16 decoding. Bounded on nesting depth and
  text-node length; the event list is lazy, so a consumer that stops on a
  budget stops the parser too. A leaf module (`Cmedit.Types` is not even
  needed), so `Zip`, the three mappers and the tests all use it cycle-free.
* **`Cmedit.Zip` member extraction** — `zeOffset` (with the self-extracting
  prefix and the ZIP64 offset promotion applied), `localDataOffset` and
  `memberBytes` (stored + deflate via `Inflate.inflateDyn`), capped at
  `maxMemberBytes`. The local header's name/extra lengths are read rather than
  assumed to match the central directory's, which is what the second seek per
  member buys. Everything extraction cannot do is a typed `Left`.

**The three views.** As predicted, each is a *mapping* onto machinery that
already existed rather than a new view mode:

| Format | Module | Maps onto | Divided into |
|---|---|---|---|
| DOCX | `Cmedit.Docx` | `Cmedit.Rtf`'s `RtfPar` (`mkRtfDocFrom`) | — |
| ODT | `Cmedit.Odf` | the same | — |
| ODS | `Cmedit.Odf` | `Cmedit.Csv`'s grid (`mkCsvGrid`) | sheets |
| EPUB | `Cmedit.Epub` | the same | chapters (`rdSects`, `[`/`]`, Go to Chapter) |
| XLSX | `Cmedit.Xlsx` | `Cmedit.Csv`'s grid (`mkCsvGrid`) | sheets (`edSheets`, `[`/`]`, Go to Sheet) |

`Rtf.RtfOrigin` is what distinguishes a container view from the `.rtf` case:
the file is binary, so there is no buffer under it — nothing to Alt+T to
except the archive listing, nothing `refreshRtf` can re-parse (`rtfStale`
returns `False`), nothing Save can write, `isPlainDoc` is `False`. Installers
go through a shared `blankReadOnly`, which `pdfLoaded` was refactored onto as
well.

Read-only is enforced twice everywhere, as elsewhere in the editor:
`containerDisabledActions` pruned from the menus *and* guarded in `runAction`,
`handleSheetKey` swallowing every grid-editing key, and `syncCsvToBuffer`
refusing a workbook outright as the backstop under all of it.

**Differences from the plan as written.**

* **§4.3's lazy chapter loading was not built.** Chapters are parsed at load
  time under a character budget (`maxEpubChars`), which is bounded, needs no
  new effect round trip, and measured at 3 ms for a real book. Revisit only if
  a book is found that this is slow on.
* **DOCX tables are read, not skipped** (§4.1 proposed skipping them). A row
  becomes one paragraph whose cells are tab stops — the same bargain
  `Cmedit.Rtf` already strikes with `\cell` — because dropping them loses real
  text. Numbered and bulleted lists get a bullet for the same reason.
* **`rpSpace` was added to `RtfPar`.** Not foreseen, and load-bearing: a
  `.docx` and an XHTML chapter carry their paragraph spacing in a style sheet
  these readers do not resolve, so honouring only their *manual* blank
  paragraphs gives a document with gaps in some places and none in others.
  Both mappers drop empty paragraphs and set `rpSpace` on every real one; RTF
  never sets it.
* **The View ▸ Archive Contents toggle is a driver round trip** rather than a
  pure toggle, because neither view is derived from the other (one is the
  central directory, one is a decompressed member). It re-sniffs, so a file
  replaced on disk lands wherever it now belongs.
* **§4.2's `xl/_rels/workbook.xml.rels` fallback was needed in practice** —
  workbooks written by simpler producers ship no relationship table, and the
  conventional `xl/worksheets/sheetN.xml` naming is right for all of them.

**Follow-on, landed with it: selection and copy in the formatted view.**
The plan left the DOCX/EPUB views unable to copy, because the RTF view they
reuse had no selection model. `Cmedit.Pdf`'s was ported to `Cmedit.Rtf`
(`rdCaret`/`rdAnchor`, `rtfSelText`, `rtfPosAtCell`/`rtfCellOfPos`,
word/line ranges, `rtfExtendTo`, `rtfScrollToCaret`), together with
`handleRtfMouse`, the renderer's highlight, the caret in `computeCursor`, and
`MACopy`/`MASelectAll` removed from `rtfDisabledActions`. The same three
judgements carry over: plain arrows scroll and leave the selection alone, a
plain click leaves nothing behind, and the caret is drawn only while an anchor
is set. A re-wrap drops the selection, since its indices address the old
layout. This also gives the plain `.rtf` view selection and copy, which it
never had.

**Second follow-on: formula evaluation where the file supplies none.**
§4.2 said "formulas show their cached value (the `v` element — never
evaluate)", and for a workbook Excel wrote that is exactly right: the cached
value *is* the spreadsheet's own answer. The gap it left is workbooks written
by a *library* — `openpyxl`, `xlsxwriter`, `pandas.to_excel` — which store the
formula and no value at all; those cells showed blank, which is the one answer
that is certainly wrong. `Cmedit.Formula` (a new leaf: lexer,
recursive-descent parser over Excel's precedence, evaluator with Excel's
coercions and ~50 functions) fills in **only** those cells, and the status bar
reports how many it computed and how many it could not.

The plan's instinct is kept intact, and sharpened: `sheetGrid` records only
formulas with no cached value, so there is no path by which a computed number
displaces a supplied one — pinned by a test with a deliberately *stale* cached
value, where the file says 999 and we would say 30, and 999 is what shows.
Coverage is `SUM`/`AVERAGE`/`MIN`/`MAX`/`COUNT`/`COUNTA`/`MEDIAN`/`PRODUCT`/
`SUMPRODUCT`, `IF`/`IFERROR`/`AND`/`OR`/`NOT`, `SUMIF`/`COUNTIF`/`AVERAGEIF`
with comparison and wildcard criteria, the rounding and maths families, the
text family, `VLOOKUP`/`HLOOKUP`/`INDEX`/`MATCH` and the `IS*` predicates,
plus the full operator set, whole-column ranges, cross-sheet references and
chains. `TODAY`/`NOW`/`RAND` are deliberately absent — the module is pure.
An unknown function makes the whole formula unsupported *before* evaluation,
so a cell is never half-computed. Measured: 20 000 formulas across two sheets
add ~200 ms to a 5 000-row workbook's open. Two bounds are worth recording
because the first attempt got both wrong: the cycle guard is a `Set` rather
than the recursion stack (a running-total column is a chain thousands deep,
and a linear membership test down it is quadratic), with `maxDepth` a separate
much larger stack guard reported as `#NUM!` so a long chain is never
mislabelled circular; and `maxSteps` counts cell *reads* rather than formulas,
because the cost lives inside range scans.

**Third follow-on: Save As exports what the view shows.** Chasing "can we save
an `.xlsx` as a CSV" turned up a bug rather than a gap: `MASaveAs` is in
neither `containerDisabledActions` nor `pdfDisabledActions`, so Save As on a
workbook, PDF, DOCX or EPUB wrote the *empty* buffer underneath the view and
then re-pointed the document at the file it had just emptied. `EffExportTo`
replaces that with an export — the showing sheet as CSV, a document as plain
text — which writes a copy and leaves `edPath` alone, because the open
document is still the workbook and a later Ctrl+S must not aim at the CSV.
The dialog says "Export" and the menu entry is relabelled per view; exporting
over the source archive is refused, since Save As does not confirm before
overwriting; and the two views with no text (image, pager) refuse instead of
writing an empty file. `rtfPlainText`/`pdfPlainText` export paragraphs rather
than laid-out lines — the opposite choice from `rtfSelText`, because a
selection is a piece of the screen but a file wrapped to whatever width the
terminal happened to be is a poor artifact.

**Fourth follow-on: OpenDocument (`.odt`, `.ods`).** The third container
format and the cheapest, because it adds no new *target* — a text body is the
formatted view a `.docx` uses, a spreadsheet is the grid an `.xlsx` uses, and
`Cmedit.Zip` + `Cmedit.Xml` were already paid for. `odfKind` picks between
them from the element inside `<office:body>` rather than the extension or the
`mimetype` member, which repackaging tools drop; `.odp`/`.odg` are positioned
shapes and fall back to the listing.

Three things differ from OOXML and are the whole of the work.
**Formatting lives in named styles, not on the run** (`<text:span
text:style-name="T1">`, with `T1` defined in `<office:automatic-styles>`), so a
reader matching on elements alone sees no formatting at all — `odfStyles`
builds that table in the same pass and `resolveFmt`/`resolvePar` walk the
parent chain. **Lengths are CSS lengths** (`0.5in`, `12pt`) where OOXML writes
bare twips. And **an `.ods` pads every row to the sheet width** with one cell
carrying `table:number-columns-repeated="1024"`, plus trailing rows at
`1048576`; expanding those literally would turn every one-cell sheet into a
million-row grid, so a repeated run of empty cells or rows at the end of its
line is dropped — with the exception that a cell holding a **formula and no
value looks empty and must survive the trim**, being exactly the cell
`Cmedit.Formula` is about to fill.

ODF is also the one format that reads *better* than its OOXML cousin: a cell
carries its **displayed** text as well as its raw value, so the number formats
`0021` had to decline to apply are already applied — dates and currencies read
as `15/01/2024` and `$1,234.50`. `odfFormula` translates the rare uncalculated
formula into the evaluator's syntax (`of:=SUM([.A1:.B9])` → `SUM(A1:B9)`,
`[Sheet2.A1]` → `Sheet2!A1`).

**Fifth follow-on: `cmedit FILE > out.txt` converts instead of opening.** Every
reading view already turns an awkward format into text a terminal can show, so
the same work makes the binary a converter — and a converter is what you want
when the terminal is not there. The trigger is `hIsTerminalDevice stdout` and
needs no flag, because the editor draws on stdout: a redirected stdout cannot
mean "open the editor". Content goes to stdout and one line describing it to
stderr, the only arrangement a redirect survives (stderr is set to UTF-8 first,
since its encoding otherwise follows the locale and the arrow in that
description would make writing it an *exception* under `LANG=C`).
`App.convertPath` reuses `classifyFileWith` and matches on the `LoadOutcome`,
so a format is convertible exactly when it is openable — `.pdf`/`.docx`/`.odt`/
`.epub`/`.rtf` to text, `.xlsx`/`.ods` to CSV (`--sheet N`, since a CSV holds
one table), an archive to its listing. Three cases are handled apart from the
rule: an image has no text, a file over `maxOpenBytes` is *already* plain text
so `cat` is the answer, and a missing path — which is a new empty buffer to the
editor and an error here.

**Audit pass (same day).** A sweep over all of the above found five real
oversights, all fixed:

* **Five accumulation sites stored a computed value lazily**, against the
  strictness invariant `CONTRIBUTING.md` records — `Xlsx.pushCell`'s `|> value
  st` (a thunk per cell over the whole parser state), `Xlsx.sharedStrings`'s
  lazy `T.copy` (which /retains/ the slice it exists to release),
  `Xlsx.wbPut`'s `Seq.update`, `Odf.endCell`'s cell text, and
  `Formula.setCell`'s nested `Seq.update` (twenty thousand nested thunks over
  the grid). Measured: a 100 000-cell workbook 55 MB → **41 MB** resident, a
  20 000-formula workbook 98 MB → **88 MB**.
* **`.fodt`/`.fods` were wrongly in `isContainerPath`.** Flat OpenDocument is a
  single XML file, not a ZIP, so it has no archive to toggle to; it now opens as
  its own markup like any other XML.
* **`odfPars` did not force its style table** before the body fold, so the
  event-list prefix the fold had already passed was retained until the first
  styled span asked for a lookup.
* **An EPUB silently dropped chapters it could not read.** Everything else here
  reports its shortfalls; this now counts them and says "1 chapter unreadable"
  in the note. (An *empty* chapter is still not counted — a cover page whose
  whole content is an image is normal.)
* **A sheet name or chapter title in a wide script broke the status bar's
  click zones.** That block is measured in /characters/ by `statusRightInfo`
  and `statusClick` — a 1-char-is-1-cell assumption that holds because
  everything there has always been ASCII, and that these views broke by putting
  user-supplied names in it. The number is kept and the name shown only when
  every character of it is one cell wide; the full name is still in the open
  message, the Window menu and the conversion line.
* Dead code removed: a no-op `foldl'` in `Formula.evalArgs`, an unused
  `workbookStatus`, and three redundant imports.

**Verification:** `make test` 2 472 → **2 864 passing** (392 new, covering the
XML parser's entity/CDATA/namespace/malformed/bounds cases, member extraction
including the longer-local-extra-field and self-extracting-stub cases, golden
`[RtfPar]` and grid assertions per format, and the editor-level read-only,
menu-pruning, unit-navigation and selection behaviour, and the formula
language end to end, the export path, and OpenDocument's style table, CSS
lengths, repeat-count padding and formula translation, and the command-line
conversion's format decisions and refusals). `make windows-check` clean — all
five new modules are pure and portable, and the only IO is the driver's
existing `readAt`/`withBinaryFile`.

---

## 1. The observation

These three formats are ZIP containers full of XML — and the editor already
owns almost every piece needed to *read* them:

| Needed | Already exists |
|---|---|
| Open the container | `Cmedit.Zip` (`zipMagic` sniffing, EOCD/central-directory parsing, ZIP64, CP437 names) |
| Decompress a member | `Cmedit.Inflate.inflateDyn` (DEFLATE with unknown output size — the PDF path) |
| Lay out formatted paragraphs | `Cmedit.Rtf`: `RtfPar`/`RtfRun`/`RtfFmt` + `layoutRtf` (wrap/indent/align, twips, width-keyed cache) |
| Render formatting runs | `Render.drawRtf` → `expandLineCellsFrom` (tabs, wide glyphs, `legibleOn` colour policy) |
| Show a grid | `Cmedit.Csv.CsvView` (variable-height rows, frozen header, cell selection, copy) |
| Pages / read-only discipline | the PDF view's `[`/`]` page model, `computeCursor = Nothing`, pruned+guarded actions |

What is missing is exactly **two shared pieces** — a small XML parser and a
"read one member's bytes" function on `Cmedit.Zip` — plus one thin mapping
module per format. That is the highest ratio of headline value to new code
available anywhere in the feature space.

Today a `.docx`/`.xlsx`/`.epub` opens as the archive *listing*
(`App.zipOutcome`). That stays: it becomes the fallback for damaged files and
the "show me the container" secondary view.

## 2. Shared prerequisite A: `Cmedit.Xml` (a leaf module, ~1 day)

A non-validating pull parser over `ByteString`/`Text`, in the house style of
`Cmedit.Rtf`'s reader: model what we consume, *skip* what we don't.

- Elements, attributes, character data; entity references (`&amp; &lt; &gt;
  &quot; &apos; &#N; &#xN;`); CDATA; comments/PIs/DOCTYPE skipped wholesale.
- **Namespaces by local name.** OOXML files disagree about prefixes
  (`w:p` vs a default namespace); matching on the local part (`p`) is what
  every pragmatic OOXML reader does and avoids a namespace-resolution table.
- Bounded three ways, like the pager's reads: `maxXmlBytes` per member (input
  is already capped by the extraction cap below), a nesting-depth cap, and a
  per-text-node length cap. A malformed file yields a parse error, never
  unbounded work.
- Leaf module (imports `Cmedit.Types` at most), so `Zip`, the format mappers
  and tests can all use it cycle-free.

The temptation to resist: a DOM with full namespace support. The three
consumers below need streams of (element, attrs, text) events and nothing more.

## 3. Shared prerequisite B: member extraction in `Cmedit.Zip` (~½ day)

`Cmedit.Zip` deliberately never decompresses; that stays true for *listings*.
Add `entryBytes`-style support: given an entry's local-header offset (already
parsed into `ZipEntry`), parse the local header (its name/extra lengths differ
from the central directory's — must be read, not assumed), then either copy
(method 0, stored) or `inflateDyn` (method 8). Everything else — encrypted
members, other methods — returns a typed refusal that surfaces as the listing
plus a status note.

Bounded by the entry's declared uncompressed size, capped at a new
`maxMemberBytes` (16 MiB is generous: `word/document.xml` for a 400-page
document is ~2 MB). The driver reads the compressed span with the existing
`App.readAt`, so extraction stays size-independent in the *archive*: a 1 GB
`.docx` full of images costs only the members we actually parse.

## 4. The three views

### 4.1 DOCX — a formatted reading view (highest reuse, do first)

Target the **existing RTF document model**, not a new one: a mapper
`docxToPars :: XmlEvents -> [RtfPar]` reading `word/document.xml` —
`w:p` (paragraph; `w:jc` → `RtfAlign`, `w:ind` → `rpLeft`/`rpFirst`, twips
already being OOXML's unit, which is why `twipsToCols` transfers unchanged),
`w:r`/`w:t` (runs; `w:b`/`w:i`/`w:u`/`w:strike`/`w:color`/`w:sz` →
`RtfFmt`, half-points matching `rfSize`'s convention), `w:br`, `w:tab`.
Tables, pictures, fields, footnotes: skipped, the Rtf/Pdf bargain.

Then split `mkRtfDoc` so a variant takes `[RtfPar]` directly instead of
parsing buffer text, and **everything downstream is free**: `layoutRtf`,
`rdCache`, `drawRtf`, scrolling, the scrollbar (both halves — `scrollBarInfo`
*and* `scrollBarTo` — already know the RTF mode). The one structural
difference from RTF: there is **no buffer underneath** (the file is binary),
so like PDF it arrives as its own `LoadOutcome`, has no Alt+T toggle, and
Save is refused rather than writing the buffer. `refreshRtf`'s staleness
machinery is unnecessary (nothing can move under a binary file) — the doc is
parsed once at load.

### 4.2 XLSX — the grid, read-only

`xl/workbook.xml` names the sheets; `xl/worksheets/sheetN.xml` holds rows of
cells (`r` = A1-style reference — gaps are real and must materialise as empty
cells; `t="s"` indexes `xl/sharedStrings.xml`; `t="inlineStr"`, numbers,
booleans inline). Map a sheet to the existing `CsvView` (`mkCsvView`) for
display, navigation, cell selection and copy — but **read-only**: this is the
RTF bargain applied to the grid. There is no serialiser, `syncCsvToBuffer`
is never called (there is no buffer), editing keys are swallowed and the
editing/sort actions pruned *and* guarded in `runAction`, exactly the
`rtfDisabledActions` pattern. Multi-sheet via the PDF page model: `[`/`]`
cycle sheets, the status bar shows "Sheet 2/5 · name", and `MAGoToLine` is
relabelled to "Go to Sheet…" via `relabelEntry` as PDF does for pages.

Deliberate v1 limits, stated in the status bar rather than guessed at:
formulas show their cached value (the `v` element — never evaluate), and
number formats (dates!) show the raw stored value with a "formats not
applied" note. Honest and useful beats wrong.

Bounds: `maxSheetCells` (say 2 M) truncates with a visible marker;
shared-string table detached (`T.copy`) per the `0014` rule since its
strings are slices of one decoded member.

### 4.3 EPUB — chapters through the same layout pipeline

`META-INF/container.xml` → OPF path → manifest + spine → an ordered list of
XHTML chapter members. A minimal HTML-to-`RtfPar` mapper (`p`, `h1`–`h6` →
bold + the `rfSize` convention, `em/i`, `strong/b`, `u`, `br`, `li` with a
bullet and indent, `blockquote` via `rpLeft`; everything else unwrapped or
skipped). Chapters are PDF's pages: `[`/`]` turn them, Go To reads a chapter
number, and chapters are parsed **lazily on first visit** (an EPUB can have
hundreds; parse-on-turn keeps open instant) with a small LRU of parsed
chapters so re-visits are free but memory is bounded.

## 5. Classification and fallback

`classifyFileWith` already sniffs `zipMagic` before the binary refusal. Add
one step behind it: peek the member *names* (already in the central
directory, zero extra reads) — `word/document.xml` → DOCX, `xl/workbook.xml`
→ XLSX, an OPF named by `container.xml` (or the `mimetype` member) → EPUB —
and only then fall through to the plain listing. **Any parse failure at any
stage degrades to the listing** with a status note, which is the same
graceful-floor shape as the image view's half-block fallback. View ▸
"Archive Contents" toggles the rendered view back to the listing for the
curious.

Loads cross to the background loader at `imageAsyncBytes` like PDF — these
views pay for their content, not their byte count (`looksLikeDecoded`).

## 6. Testing

- **Pure, `Cmedit.Xml`:** a corpus of entity/CDATA/namespace/malformed cases;
  the bounds each provably terminate (`make test` additions).
- **Pure, per format:** golden tests from small hand-built fixtures (a
  `.docx` is easy to construct in the test itself: `Zip` + a hand-written
  `document.xml` — no external tooling needed offline). Assert the mapped
  `[RtfPar]` / grid, not the rendered cells, so layout changes don't churn
  them.
- **Cross-check:** for DOCX, word-count parity against the RTF view of the
  same document saved both ways (the corpus bar that `Cmedit.Pdf` set with
  `pdftotext`).
- **Fallback:** a truncated/damaged file of each format must open as the
  listing, not error.
- **`make windows-check`:** all new modules are pure and portable; only the
  driver's extraction call touches IO, via the existing `readAt`.

## 7. Risks and non-goals

- **Scope creep is the only real risk.** OOXML is an ocean; the defence is
  the one Rtf/Pdf proved: model the ~15 constructs that carry text and
  position, skip the rest *silently and safely*, and state limits in the UI.
- **No editing, ever, in v1** — no serialisers exist, so the fidelity trap
  cannot arise. Revisit only if someone asks, and then as its own plan.
- **Not a spreadsheet engine:** no formula evaluation, no number-format
  language. Cached values only.
- **`0022` synergy:** the read-only-`CsvView` mechanism built for XLSX (§4.2)
  is exactly what the SQLite browser's result grid needs — build it as a
  shared capability, not an XLSX special.
