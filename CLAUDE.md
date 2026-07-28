# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

CMeDit is a terminal text editor written **from first principles in Haskell** — a
cross between Microsoft Edit (`msedit`) and nano. There is deliberately **no TUI
framework** (no `brick`/`vty`): raw-mode terminal control, the input parser, the
renderer, menus and dialogs are all built directly on `termios` and ANSI/VT
escape sequences. Preserve this constraint when adding features.

## Marketing & documentation website

The CMeDit marketing and documentation site lives in a **separate repository at
`~/work/appmakernetwork`** (GitHub `appmakernetwork/appmakernetwork.com`, served
at https://appmakernetwork.com — the domain the in-app Help links point to). Its
`cmedit/` directory holds the product page (`index.html`), the online manual
(`manual.html`) and the whitepaper (`whitepaper.html`). When a request from this
project involves updating the website — announcing a feature, syncing the online
manual with `Cmedit.Manual`, refreshing release links — step sideways into that
repo and work there directly, keeping this project open as the source of truth
for what the editor actually does. Read `~/work/appmakernetwork/CLAUDE.md` first:
it has its own conventions and hard constraints (plain static HTML with no build
step, Cloudflare Pages auto-deploy on push to `main`, pretty-URL linking rules,
pages that must not be restyled as a side effect).

## Build / test / run

```sh
make          # build the optimized ./cmedit binary (ghc --make -O2)
make test     # build and run the test suite (./cmedit-test)
make run      # build and launch
make windows-check  # typecheck the Windows port's configuration (-fno-code, works on Linux)
make windows  # native Windows build (cmedit.exe) — only runs on Windows itself
make clean
```

- **The platform layer is `Cmedit.Term`, in two implementations**:
  `platform/posix/Cmedit/Term.hs` (termios/signals/ioctl + the one-lstat
  `statEntry`) and `platform/windows/Cmedit/Term.hs` (hand-rolled kernel32
  FFI: `SetConsoleMode` VT modes, polling resize, ctrl-handler, UTF-8 code
  pages) — identical export lists; each build picks one with `-i` (Makefile)
  or `if os(windows)` (cabal). **Everything outside `platform/posix` must
  stay portable**: no `unix`-package imports in `src/` — use `DiskTime`
  (TextBuffer's `UTCTime` alias) for mtimes and `Term.statEntry` for walker
  stats. Run `make windows-check` after touching driver-level code; it
  typechecks every module against the Windows platform layer.

- **Use `make`, not `cabal`, here.** This environment is offline with no Hackage
  index, so `cabal build` fails with "Could not read index". The Makefile drives
  `ghc --make` directly. `cabal build` / `cabal run` only work where a Hackage
  index cache exists. GHC is 9.0.2.
- **Dependencies are limited to GHC boot libraries** (base, bytestring, text,
  containers, array, process, stm, directory, filepath, mtl, time; plus unix
  on the POSIX side only). Do not add a dependency unless it ships with GHC —
  there is no way to fetch packages.
- The test suite (`test/Spec.hs`) is a single hand-rolled program (no external
  test framework offline). There is no per-test selector; it prints
  `Passed N, failed M` and exits non-zero on failure. To run a subset, edit
  `test/Spec.hs`.

## Verifying interactive (TUI) behavior

The editor needs a real tty, so it cannot be exercised by piping stdin. Drive it
through a **PTY harness**: `os.openpty` → set winsize via `TIOCSWINSZ` → exec
`./cmedit` with the slave as stdin/stdout → feed keystrokes to the master →
reconstruct the screen by replaying the emitted escape sequences through a small
VT emulator (track cursor from `CSI row;colH`, place printable UTF-8, handle
`CSI 2J` — and, since the renderer scrolls and compresses, DECSTBM `CSI t;b r`
+ `CSI n S`/`T` band scrolling and REP `CSI n b`, while skipping OSC/DCS/APC
strings to BEL/ST). Keys are sent as raw bytes (e.g. arrows `ESC[A`, Ctrl-A
`\x01`, SGR mouse `ESC[<b;x;yM`). The app sends capability queries at startup;
a harness that answers none of them exercises the portable fallback path, and
one that replies (OSC 11 colour, `CSI ?62;4c` for sixel, `DCS >|name ST`,
`CSI ?1;5R` for the REP probe, `APC Gi=31;OK ST` for kitty graphics) exercises
the upgrades. The pure `update`/buffer/parser logic is covered by
`make test`.

## Architecture (the big picture)

The design splits a **pure core** from a **thin IO shell**, which is why most
logic is unit-testable without a terminal. The per-module responsibilities are
in `README.md`; the cross-cutting structure that matters when editing:

- **Pure model + effects.** `Cmedit.Editor.update :: Key -> Editor -> (Editor,
  [Effect])` does all editing/navigation/selection/menu/dialog logic. Anything
  touching the outside world (clipboard, files, quitting, title, bell) is
  returned as an `Effect`; `Cmedit.App.perform` carries it out and may hand
  results back to pure callbacks (`setLoaded`, `onSaved`, `applyPaste`,
  `setError`). Add new side effects as `Effect` constructors, not inline IO.
  The pure model is split into layered modules — `Cmedit.EditorState` (state
  records, `Effect`, layout, small queries) → `Cmedit.EditorEdit` (movement,
  undo, editing primitives, line ops, file properties/status zones) →
  `Cmedit.EditorDoc` (document lifecycle/zipper, CSV/image view plumbing,
  recents, nav history, quick open, save/quit flows) → `Cmedit.EditorFind`
  (in-file + workspace find/replace, live match feedback, input history) →
  `Cmedit.Editor` (the hub: `update`/`dispatchKey`, every key/mouse handler,
  menus/`runAction`, dialog dispatch, browser/explorer/search-view/def-pick
  panels). Each layer imports only the ones before it; the hub **re-exports
  the whole public API**, so `App`/`Render`/tests import `Cmedit.Editor` only.
  Put new code in the lowest layer whose imports suffice; key handlers that
  call `runAction`/`handleEditKey` back must stay in the hub (that is the one
  cycle the layering exists to prevent).

- **Pure rendering + diffing.** `Cmedit.Render.renderEditor :: Editor -> Screen`
  builds a flat grid of styled `Cell`s; `renderFrame :: RenderCaps -> Maybe
  Screen -> Screen -> Builder` diffs it against the previous `Screen` and emits
  escape codes only for the changed cell spans of each row (small unchanged
  gaps are bridged; the SGR state threads across the whole frame). Wide glyphs
  occupy a cell plus a `contChar` ('\0') continuation sentinel so the diff keeps
  columns aligned — keep this invariant if you touch cell emission. Two emitter
  upgrades live here: **hardware scrolling** — each `Screen` carries a
  `ScrollHint` (text-band geometry + `edTop`); `scrollPlan` treats matching
  hints as a *candidate* shift, builds the predicted post-scroll screen
  (shifted rows, blank exposures), emits DECSTBM + SU/SD only when a cell
  count says it beats the plain diff, then diffs against the *predicted*
  screen so any misprediction (overlays, scrollbar thumb, sidebar) is repaired
  by the diff — an optimisation with no correctness surface, so hints may be
  approximate — and **REP run compression** (`cellRun`/`emitRun`, runs of
  identical printable-ASCII cells → `CSI n b`), gated on `rcRep` because a
  terminal that ignored REP would silently drop cells. The driver wraps every
  frame (diff + title + graphics) in synchronized-output marks (mode 2026)
  inside `App.renderNow`; unsupporting terminals ignore them.

- **Terminal capabilities: probe, reply, fold (`Cmedit.Caps`).**
  `App.enterScreen` fires a burst of queries (OSC 11 background, XTWINOPS
  14/16 pixel geometry, XTVERSION, a kitty-graphics query, an empirical REP
  probe — print 2 chars + `REP 2` + DECXCPR, cursor column tells the truth —
  and DA1 last, which everything answers, as a fence). Replies arrive
  interleaved with keys; `Cmedit.Input` decodes them (OSC/DCS/APC string
  sequences, plus `?`-prefixed CSI `R`/`c` finals and CSI `t` XTWINOPS
  replies) into `KReply TermReply` events, which `applyBatch` consumes like
  `KFocus` (the pure model ignores them). `Caps.applyReply` folds them into
  `drvCaps :: TermCaps`; background colour and cell pixel size go to the
  editor instead (`setDetectedDark`, `setCellPx`). **The rule for new
  features:** anything a silent terminal would *ignore* (OSC hints, private
  modes, title stack) may be emitted unconditionally; anything a silent
  terminal would *corrupt on* (REP, SGR colon forms like curly underline,
  pixel graphics) must be gated on a probe reply or the `supportsUndercurl`
  whitelist. No reply ⇒ the portable stream — never regress a dumb terminal.
  The bare `ESC ]`/`ESC P`/`ESC _` introducers still decode as Alt+]/P/_ when
  no payload byte follows within the ESC timeout.

- **Ambient terminal hints (safe-unconditional family).** The driver also
  emits: pointer-shape hints (OSC 22) on hover transitions — the shape comes
  from the pure `pointerShapeFor` (EditorState) and the last-emitted shape is
  tracked in `drvPointer`; desktop notifications (OSC 9) when a workspace
  search / Replace All / background load finishes while the terminal is
  unfocused (`notifyUnfocused`, gated on `drvFocused`); a title-stack
  push/pop (`CSI 22;0t`/`23;0t`) wrapping the session so OSC 0 titles no
  longer clobber the shell's; and the theme-matched cursor colour. All of
  these are ignored byte-for-byte by terminals that lack them, which is
  exactly why they need no capability gate.
  **OSC 8 hyperlinks** are in this family too: `Cell` carries an optional
  link target (`cellLink`; the `Cell c s` pattern synonym keeps the
  two-field constructor working, `CellL` is the real one), the pure targets
  come from `Cmedit.Link` (`urlSpans` for http(s) URLs in visible document
  lines, `filePathUri` for the explorer / search-result headers / status-bar
  name — absolute paths only, so untitled and `cmedit://` never link), and
  the frame diff opens/closes links exactly like it threads SGR
  (`EmitState` in Render; REP runs and screen diffing already respect links
  because they compare whole cells). Every frame ends with the link closed
  so driver-emitted escapes (title, graphics) can never join it. Keep new
  cell emission link-aware: a wide glyph's continuation cells must carry
  the head cell's link. **The editor also opens document URLs itself**
  (Ctrl+Click or right-click on an http(s) URL → `EffOpenUrl` →
  `Clipboard.openUrl`: xdg-open/open/rundll32, fire-and-forget with stdio
  swallowed and a reaper thread) because a mouse-reporting app rarely gets
  its link clicks forwarded to the terminal's own OSC 8 handling; hovering
  a URL sets `edHoverUrl` (a hand pointer via `pointerShapeAt`, and a
  status-bar hint that overlays `edStatus` while hovering — hovering is the
  current action, the message returns on move-off). `urlAtMouse` is the
  shared hit-test (click-position semantics; CSV/image views never match);
  any keystroke clears the hover state in the `handleEditKey` wrapper.

- **Pixel image upgrade (`Cmedit.Gfx`).** When `TermCaps` shows kitty
  graphics (probe reply) or sixel (DA1 attr 4), `App.renderNow` overlays the
  image view with true pixels: `gfxOverlay` keys the placement on
  (path, crop, geometry, cell-px, kind) in `drvGfx` and re-emits only when
  that changes or after a full redraw; `Image.scaleRGBA` area-averages the
  crop to the fitted resolution (`gfxFit`, aspect-true via the real cell
  pixel size, capped at `maxGfxPixels`), then `kittyPlace` (base64 RGBA
  chunks, delete-all first) or `sixelPlace` (hand-rolled encoder: 6×7×6
  palette, transparency via P2=1, RLE) positions it over the text area.
  `gfxFit` centres the placement and, unless a zoom crop is active (or the
  cell-pixel size is unknown), refuses to enlarge past native (1 device
  pixel per source pixel) — this cap tracks `EditorState.imageFitCap`, which
  applies the identical rule to the cell fallback, so overlay and fallback
  agree on the box. The half-block cell picture is the fallback, drawn
  **only when no placement will cover it**: the shared predicate
  `EditorState.imageOverlayActive` (mirroring `wantGfx`) gates both the
  driver's placement *and* the renderer — when it holds, `Render.drawImage`
  paints blank (terminal-background) cells instead of the half-block grid, so
  the blocky fallback and its transparency checkerboard can't bleed through the
  overlay's transparent pixels. It holds when the terminal advertises pixels
  (`edGfxCaps`, mirrored from `TermCaps` in `applyReplyIO`) and the image is
  the unobstructed content; any overlay (menu, dialog, zoom drag) drops it, so
  the cell picture reappears as the true fallback under that UI.
  **Explorer-panel focus does not** drop it (the panel sits left of the
  placement, and viewing an image with the tree focused is intended — opening
  one from the panel keeps focus there). Cursor position is re-asserted after
  graphics emission.

- **No import cycles by design.** `Cmedit.Menu` and `Cmedit.Dialog` are
  Editor-independent *data* (a `MenuAction` / `DialogKind` enum plus pure field
  helpers); `Cmedit.Editor` interprets what those actions/dialogs *mean*;
  `Cmedit.Render` depends on Editor/Menu/Dialog. Don't make Menu/Dialog import
  Editor.

- **The rightmost terminal column is reserved for the vertical scrollbar**,
  conditional on the `scrollbar-vertical` config key (`cfgScrollBarV`, default
  on) — but still content-independent: `computeLayout` folds the key into
  `loVBarW` (1 or 0) once, and `csvViewportFor` and `searchRegion` subtract
  that instead of a hardcoded 1 (conditional-on-*content* reservation would
  make the wrap width oscillate with content height; conditional-on-*config*
  cannot, since the config doesn't change mid-frame). `scrollBarInfo` returns
  `Nothing` outright when the key is off, which disables drawing and the
  click/drag handlers (`scrollBarPress`/`scrollBarTo`) together.
  `scrollBarInfo`/`scrollThumb` (EditorState) are shared by
  `Render.drawVScroll` and the hub's click/drag handlers (`edScrollDrag`
  swallows the mouse mid-drag like the sidebar drag). Under word wrap the bar
  uses buffer lines as a proxy for visual rows, deliberately (O(1) on huge
  files). A sibling horizontal bar (`scrollbar-horizontal` /
  `cfgScrollBarH`, default on) occupies a reserved row directly above the
  status bar, when the active view can actually scroll sideways (CSV, or
  plain text that isn't an image and isn't word-wrapped) and the search view
  isn't showing (`loHBarRow`, `computeLayout`); `hScrollBarInfo`/
  `Render.drawHScroll`/`edHScrollDrag` mirror the vertical set exactly. Its
  track spans from `loTextLeft` (or, in CSV, past the pinned row-number
  gutter) to one column short of the vertical bar's reserved corner when
  `loVBarW` is 1, or the full width when it is 0. CSV dragging is display-cell
  granular (`Csv.csvXOff` carries the sub-column offset; the first visible
  column can be clipped mid-cell), while keyboard navigation stays
  column-aligned. Both bars are Settings-dialog rows (`settingsSpec`,
  "Vertical/Horizontal scroll bar") that apply live like every other row.
- **Bounded histories are structural (`Cmedit.History`).** Undo/redo (text and
  CSV), the Alt+←/→ navigation trail and the find/replace input history are
  `Seq`s pushed through `pushHist`, *never* lists capped with `take`: `take n
  (x : xs)` retains everything it promises to drop (the cap only applies if
  something walks the list that far, and nothing does), which grew undo history
  for the whole session. `Cmedit.History` is a leaf module so `Csv` and
  `EditorState` can both use it. Anything else that accumulates with session
  length needs the same treatment.
- **A `Text` that outlives its buffer must be `detach`ed** (`EditorState.detach`
  = `T.copy`). `Data.Text` values are slices of a shared array, and a buffer's
  lines are slices of the whole decoded file, so one escaped slice pins the
  entire file: ten retained lines of a 49 MB file held 49 MB. The clipboard,
  find/replace terms and their persisted history, completion candidates and
  `Search`/`Definition` snippets all copy; values that were freshly built (an
  `<>`, an `intercalate`) are already detached.
- **Buffer writes are explicitly strict.** `Seq` is spine-strict but
  element-*lazy*, and `Seq.update i x = adjust (const x) i` stores a thunk
  capturing the line it replaced — which every undo snapshot then holds, so the
  versions chain. `TextBuffer` uses `Seq.adjust'` and bangs the lines it stores.
  `-O2` happens to remove the chain; the space behaviour must not depend on it.
- **The event loop collects when idle** (`App.idleGcDelayUs`, 30 s, re-armed by
  every branch that does work, disarmed once it fires). An idle editor never
  allocates, so nothing is ever collected and the process keeps the high-water
  mark of everything it has opened. The shipped RTS options
  (`Makefile` `RTSOPTS`) pair it with `--disable-delayed-os-memory-return`,
  without which freed pages stay in RSS (`MADV_FREE`) — together they took
  "2.5 GB after opening and closing a 32 MB CSV" to 33 MB.
- **Paged view for huge files (`Cmedit.Pager`, `edPager`/`docPager`).** A
  fourth read-only view mode, alongside CSV and image. A file over
  `maxOpenBytes` used to be refused; it now opens in a viewer whose memory is
  independent of the file's size — measured 30–44 MB resident for a 281 MB log
  and for a 120 MB single-line file. Two structures make that work: a **sparse
  index** of byte offsets, one per `pagerStride` (1000) lines, built by one
  streaming pass (`buildPagerIndex`, which also sniffs the BOM and line
  ending — `pgEol` selects the byte the index and reader split on, so CR-only
  files page correctly), and a **window** of decoded lines around the viewport
  (`readPagerWindow`, refilled via `EffPagerFill` → `pagerFilled`, or
  driver-side by `fillPagerNow` for paths that don't pass through the key
  handler: startup, a background load landing, a resize). Every read is bounded
  three ways — a stride of skipped lines, `count` decoded lines, and
  `maxPagerLine` (64 KiB) per line. That last cap is not a nicety: without it a
  file with no separators makes the reader concatenate the whole file (a 120 MB
  single-line file drove the editor to 51 GB resident before it was added).
  The mode is read-only by construction (there is no buffer), so editing keys
  are swallowed, Save/Revert are refused, and the Find/definition menu entries
  are pruned like the image view's — except **Go To Line**, which is the point
  of the view. Syntax highlighting lexes each visible line from `initialState`,
  since the state before it would mean reading from the top of a multi-gigabyte
  file. Switchable with `paged-view = off`.
- **Formatted RTF view (`Cmedit.Rtf`, `edRtf`/`docRtf`).** A fifth view mode,
  wired exactly like CSV's — same per-document field, same **Alt+T**
  (`handleAlt` picks `MAToggleRtf` over `MAToggleCsv` by extension; a file is
  at most one of the two, so the View menu's two mode groups are pruned
  against each other by `dropViewModes`) — but the traffic is **one-way, and
  that is the whole design**. The CSV table *is* the document while showing,
  so it serialises back (`syncCsvToBuffer`); the RTF view must not, because it
  models bold/italic/underline/strike/colour/alignment/indent and a real
  document also carries style sheets, tables, pictures and revision marks that
  it does not. So there is **no serialiser** — the buffer stays the document,
  Save writes the buffer, leaving the view discards the projection, and the
  fidelity problem that makes RTF editing hard simply cannot arise. Editing an
  `.rtf` means editing its markup (Alt+T; `Syntax`'s `RTF` lexer highlights
  it). Consequences of "derived, not owned": the view is read-only (editing
  keys swallowed by `handleRtfKey`, `rtfDisabledActions` pruned from the menus
  *and* guarded in `runAction` so the shortcuts are inert), it has **no
  cursor** (`computeCursor` returns `Nothing` like the image view — the buffer
  cursor is a point in the *markup*, so showing it would blink somewhere
  arbitrary in the rendered text), it re-derives itself when the buffer moves
  under it (`refreshRtf` from the `update` wrapper, so Undo/Redo and staged
  replaces are covered without hunting the mutation sites), and an RTF doc
  stays `isPlainDoc` (unlike CSV/image/pager) since its buffer is live and
  authoritative.
  **It selects, and that is what makes it worth reading in.** `rdCaret`/
  `rdAnchor` are the PDF view's model, ported: a caret and an optional anchor
  over laid-out (line, character) coordinates, with mouse press/drag/release
  (`handleRtfMouse`), double-click-word and triple-click-line, Shift+movement,
  Ctrl+A and Ctrl+C copying `rtfSelText` (detached, like every clipboard
  value). The same three judgements hold as there: **plain arrows scroll the
  window and leave the selection alone** (Esc is what clears it), a **plain
  click leaves nothing behind** (with no selection the view shows no cursor,
  so a stray click must not conjure one), and the caret is drawn *only* while
  an anchor is set — so `computeCursor` still returns `Nothing` for an
  untouched document. A re-wrap drops the selection outright (`rtfRelayout`):
  the indices address the old layout, and pointing at whatever now sits there
  would be a lie. Note that the mouse handler sets `edMouseSelecting`, which
  gates the chrome's mouse guards, so its *release* branch must always clear
  it — a drag that never ends jams the scroll bars. **`rtfStale` compares `edEditSeq`, an `Int` — not the
  buffer.** The obvious version (keep the line `Seq`, check `ptrEq`, the
  `HlCache` idiom) *silently fails here*: under `-O2` `ptrEq` on a lifted
  value reported a mismatch even for an untouched buffer — immediately after
  `mkRtfDoc` stored it — and unlike `Syntax.sameText` there is no cheap `==`
  to fall back to on a whole document, so every keystroke re-parsed the file
  (measured 169 ms per arrow key on a 1.6 MB RTF; 0.18 ms after). `ptrEq` is
  only sound as a fast path in front of a real comparison; where there isn't
  one, use a counter. A Spec *timing* test pins this, since nothing structural
  can see it. Mouse-wise the view owns only the wheel; **every other mouse
  event in the text area is swallowed** (there is no cursor to move, and
  falling through to `handleEditKey` would drag a selection through the
  invisible markup buffer *and* set `edMouseSelecting`, which gates the
  scrollbar guard). Note the vertical scrollbar needs teaching **twice** —
  `scrollBarInfo` (draw/measure) and the hub's `scrollBarTo` (act) are
  separate view splits, and a mode added to only the first gets a bar that
  tracks perfectly and does nothing when dragged.
  Parsing is the ordinary RTF reader trick: group-scoped state, a colour
  table, and `{\*\...}` plus `skipDestinations` skipped wholesale, so every
  construct we do not model is *ignored* rather than mis-rendered. Layout
  (`layoutRtf`) wraps/indents/aligns into `RtfLine`s keyed on width in
  `rdCache` — recomputed on resize, never on scroll — and `Render.drawRtf`
  feeds their formatting runs to the same `expandLineCellsFrom` the text view
  uses as `baseAt`, so tabs and wide-glyph continuations behave identically.
  Two rendering judgements worth keeping: a document colour is honoured only
  when `legibleOn` says it reads against the theme (nearly every RTF specifies
  *black* body text, invisible on a dark theme), and `\fs`-large runs render
  bold since a terminal has one font size. `attrStrike` (SGR 9) was added for
  `\strike` and needs no capability gate — an unrecognised *numeric* SGR
  parameter is ignored, unlike the colon sub-parameter forms or REP.
- **PDF reading view (`Cmedit.Pdf`, `edPdf`/`docPdf`).** A sixth view mode.
  Structurally it is the **image** view, not the RTF one, and the reason is
  worth stating: a PDF is binary, so there is no buffer under it to toggle to
  (no Alt+T), nothing Save could write, and the file is sniffed
  (`Pdf.sniffPdf`, `%PDF-` anywhere in the first KiB — producers are allowed
  junk in front of it) *before* the binary refusal in `classifyFileWith`,
  exactly like `sniffImage` and `zipMagic`. It arrives as its own
  `LoadOutcome` (`OutPdf`), installed by `pdfLoaded`/`pdfLoadedNew`/
  `addPdfDocument`. Like the pager it needs no second installer for the
  already-open case — it is read-only, so switching to the open copy is the
  only sane result. Parsing is the expensive part, so a PDF crosses to the
  background loader at `imageAsyncBytes` rather than `asyncThresholdBytes`
  (`looksLikeDecoded` covers both: these views pay for their *content*, not
  their byte count).
  Three things make a text view of PDF tractable without a rasteriser, and
  they are the load-bearing decisions in the module:
  **(1) Objects are found without the cross-reference table.** `scanObjects`
  sweeps the file for `N G obj` and parses each, resuming *after* the object
  it just read so an `obj` inside a compressed stream's bytes is never
  mistaken for a definition. This is deliberately not "parse `startxref` and
  walk the chain": broken, patched and linearised-then-appended files are
  everywhere, and a scan reads them all — incremental updates fall out for
  free, since a later definition of an object number wins, which is exactly
  what an appended update means. The one thing a scan cannot see is objects
  inside compressed object streams (`/ObjStm`, universal since PDF 1.5), so
  `expandObjStms` inflates those afterwards and folds them in under the same
  later-wins rule. The trailer is found by keyword, by `/Type /XRef`, or
  failing both by any object calling itself a `/Catalog`.
  **(2) The content stream is a small stack machine.** Of its ~70 operators
  only the text ones and the two matrix ones carry position, so `runContent`
  implements those and clears operands for everything else — an unmodelled
  operator is *ignored*, the same bargain `Cmedit.Rtf` strikes with markup.
  Form XObjects recurse (depth-capped); inline images (`BI … ID …binary… EI`)
  are the one construct that cannot be tokenised as objects and are skipped
  whole.
  **(3) Reading order is recovered, heuristically, in one place.**
  `assemblePage` is where a bag of positioned glyph runs becomes a document:
  columns from a vertical band in the x-occupancy (runs wider than 60% of the
  page are excluded from that test and placed by their centre, or a
  full-width title would fill the gutter and hide it), lines from y-clustering
  at ±0.5 em (superscripts and separately-drawn accents belong to their line),
  spaces from the gap between one run's end and the next one's start measured
  in ems, and paragraphs from lines that both sit at the column's ordinary
  leading *and* reach its right margin. Two statistics there are load-bearing
  and were both wrong first: the leading is the **mode** of consecutive
  baseline steps, not the median (a page with several headings has enough wide
  steps to drag a median past the body leading, and then every paragraph on it
  merges into one), and the right margin is a **high percentile** of where
  lines end, not the maximum (a page number sits at the true page edge, and
  against that no body line ever "reaches the margin", so every ragged-right
  paragraph breaks at its first short line). Both signals must agree before
  two lines join, which is what lets each cover the other's blind spot.
  Everything downstream is the RTF view's shape: `layoutPdf` wraps/indents/
  aligns into `PdfLine`s cached against the width in `pdCache`, `Render.drawPdf`
  feeds their formatting runs to the same `expandLineCellsFrom` the text view
  uses, there is **no cursor** (`computeCursor` returns `Nothing`), mouse in
  the text area is swallowed apart from the wheel, `pdfDisabledActions` are
  pruned from the menus *and* guarded in `runAction`, and the vertical
  scrollbar is taught **twice** (`scrollBarInfo` to measure, `scrollBarTo` to
  act). `refreshPdf` rides the `update` wrapper next to `refreshRtf` — not
  because anything can edit a PDF (nothing can) but because the *width* it was
  laid out for has more sources than the `relayout` sites (turning the
  scrollbar off in Settings moves it by a column).
  **It selects, and that is the point of it.** `pdCaret`/`pdAnchor` are the
  text view's cursor-and-anchor model over laid-out (line, character)
  coordinates, minus everything that writes — mouse press/drag/release with
  double-click-word and triple-click-line, Shift+movement, Ctrl+A, and Ctrl+C
  copying `pdfSelText` (detached, like every clipboard value). Three
  decisions there are deliberate: **plain arrows scroll the window and leave
  the selection alone** (in a reader, scrolling to see the rest of what you
  highlighted must not destroy it — Esc is what clears it); a **plain click
  leaves nothing behind**, since with no selection this view shows no cursor
  and a stray click should not conjure one; and the caret is drawn *only*
  while an anchor is set, so `computeCursor` still returns `Nothing` for an
  untouched document. A re-wrap drops the selection outright (`pdfRelayout`)
  — the indices address the old layout, and pointing at whatever now sits
  there would be a lie. **In-file Find** (`pdfFindWith`, `pdfAllMatches` in
  `EditorFind`) searches the same laid-out lines, so a term the original page
  broke across a line is not found, exactly as in the wrapped text view; the
  hit becomes the selection, which is what makes Find and copy compose —
  search for it, then Ctrl+C. Find Next measures from the caret, Find
  Previous from the *start* of the current selection (otherwise the first
  Shift+F3 after a find re-finds the match you are sitting on), and the
  live-highlight and match-count paths are the text view's own
  (`liveMatchSpans` works on any `Text`; `findCountMsg` grew a PDF branch).
  Workspace search still skips PDFs — they are binary on disk, and the
  reflowed text only exists once the file is open.
  What the view adds that no other has is **pages**: `pdCache` carries the
  first line index of each one, `[`/`]` turn them, and `MAGoToLine` is
  reinterpreted — `gotoLine` reads a page number, `openGoTo` opens
  `mkGoToPage`, and `relabelEntry` retitles the menu item, because a laid-out
  row number moves with the window width and would mean nothing to anyone.
  Fonts are where extraction quality actually lives: a glyph code means
  nothing without the font's `/ToUnicode` CMap or its encoding
  (WinAnsi/MacRoman plus `/Differences`, resolved through a Latin glyph-name
  table), and its *width* decides where words break — which is why the
  standard-14 metrics are built in for files that ship no `/Widths` at all,
  and why Type3 fonts get their `/FontMatrix` applied (`fiWScale`): miss that
  and every advance is out by three orders of magnitude, which reads on screen
  as a document with all its spaces missing. Ligatures are spelled out
  (`expandLigatures`) because U+FB01 is what the font honestly reports and
  what no terminal font has. Encrypted files are *reported*, not guessed at;
  every other limit in the module (`maxPdfPages`, `maxPdfChars`,
  `maxStreamBytes`, `maxOpsPerPage`, `maxFormDepth`) bounds work that only a
  malformed or hostile file could make unbounded. Extraction is at parity with
  `pdftotext`'s word count across the test corpus, which is the bar to hold if
  the heuristics are ever retuned.
- **Archive listings (`Cmedit.Zip`).** A `.zip` — and everything built on it
  (`.jar`, `.whl`, `.docx`, `.epub`, `.apk`; sniffed by `zipMagic` in
  `classifyFileWith`, *before* the size check and the binary refusal, exactly
  like `sniffImage`) — opens as a read-only file tree of its contents. The
  deliberate choice here is that **it is not a view mode**: `App.zipOutcome`
  returns an ordinary `OutText` whose buffer is the listing and whose
  `lrReadOnly` is `True`, the trick `Cmedit.Manual` plays with its pseudo-path.
  Nothing downstream — installers, key handlers, renderer, scroll bars, find,
  wrap — learns a new mode, and there is no serialiser back to ZIP (nor could
  there be), so the archive can never be overwritten by its own table of
  contents. The one hole that leaves is `Save As`, which seeds `edPath` and
  does **not** confirm before overwriting (`DKConfirmOverwrite` exists but is
  never constructed), so `saveAsDialogFlow` seeds `<name>.txt` for an
  `isArchivePath`; if you ever wire up the overwrite confirmation, that
  special case can go.
  Reading is **size-independent and decompresses nothing**: the central
  directory is at the end of the file and is proportional to the entry count,
  so `findEocd` works on a 128 KiB tail (via the driver's `readAt`, the one
  seeking read in `App`) and `parseCentral` on the directory block alone — a
  10 GB archive costs the same two reads as a 10 KB one, and `maxOpenBytes`
  never applies. Because no member is ever read, **entry encryption is
  irrelevant**: an encrypted archive lists normally and flagged members just
  say so. The parser handles ZIP64 (the locator/end-record pair supersedes the
  32-bit fields wholesale), the self-extracting-stub offset correction (the
  directory ends where the end record begins, so subtraction finds the real
  start — 32-bit only; ZIP64's offsets are already absolute), and CP437 names
  (general-purpose bit 11 selects UTF-8; guessing wrong mangles accented
  names). `Cmedit.Zip` is a leaf (`Cmedit.Width` only) and `Syntax`'s `Archive`
  lexer colours the *generated* listing, not any file format — which is why
  it is positional and stateless, and why `archiveExtensions` feeding
  `langForPath` also (harmlessly) recolours those extensions in the explorer
  via `Render.fileKind`.
  **Single-member extraction** (`localDataOffset`/`memberBytes`/`maxMemberBytes`)
  was added for the container reading views below and is the one thing here
  that decompresses. It keeps the size-independence — only named members are
  ever read — and costs a second seek per member, because a local header's
  name and extra lengths are *its own* and not the central directory's
  (archivers routinely write a timestamp extra in one and not the other; assume
  they match and you land in the middle of the data). Everything it cannot do
  (encrypted, a method other than stored/deflate, over the cap) is a typed
  `Left`, and every `Left` becomes the listing plus a note.
- **Office and e-book reading views (`Cmedit.Docx`, `Cmedit.Xlsx`,
  `Cmedit.Epub`, over `Cmedit.Xml`).** A `.docx`, `.xlsx` and `.epub` are ZIP
  containers full of XML, and the editor already owned everything needed to
  *read* them — so all three are **mappings, not view modes**. That is the
  whole design, and the thing to preserve:
  * a **DOCX** and an **EPUB** are `Cmedit.Rtf`'s paragraph model
    (`docxPars` / `htmlPars` → `Seq RtfPar` → `mkRtfDocFrom`), so `layoutRtf`,
    `rdCache`, `Render.drawRtf`, `handleRtfKey`, `scrollBarInfo`/`scrollBarTo`
    and the status zone all work unchanged;
  * an **XLSX** is `Cmedit.Csv`'s grid (`sheetGrid` → `Csv.mkCsvGrid`), so the
    table view's navigation, selection, copy, column drags and both scroll
    bars work unchanged.
  What distinguishes them from the `.rtf` and `.csv` cases is
  `Rtf.RtfOrigin` (`rtfDerived`) and `edSheets`: the file is **binary**, so
  there is no buffer under the view — nothing to Alt+T to except the archive
  listing, nothing `refreshRtf` could re-parse (`rtfStale` returns `False` for
  a container origin), nothing Save could write, and `isPlainDoc` is `False`.
  They arrive as their own `LoadOutcome`s (`OutDoc`, `OutBook`), install via
  `containerDocLoaded`/`workbookLoaded` (over the shared `blankReadOnly`, which
  `pdfLoaded` also uses — a field added to `Editor` and set in three of four
  installers is how a view mode survives into a document that is not it), and
  cross to the background loader at `imageAsyncBytes` like a PDF
  (`looksLikeDecoded` now includes `zipMagic`: these pay for their *content*).
  **The graceful floor is the listing.** `App.archiveOutcome` reads the central
  directory, picks the format from the *member names* (`docxBodyMember`,
  `isXlsx`, `isEpub` — zero extra reads, and more honest than the extension),
  and any failure anywhere returns `Left note` which `listingOutcome` turns
  into the listing with a `⚠` line. `View ▸ Archive Contents` / **Alt+T**
  (`MAArchiveView` → `EffContainerView path listing`) swaps between the two
  deliberately as a *driver round trip*, not a pure toggle: neither view is
  derived from the other (one is the directory, one is a decompressed member),
  and the round trip re-sniffs, so a file replaced on disk lands wherever it
  now belongs. `containerViewActive`/`containerListing` decide which direction
  is offered; the second is by extension, since a listing is an ordinary
  read-only text buffer that records nothing about its origin.
  **Formulas are the one place this reader computes anything, and the rule is
  narrow on purpose: the file's own answer always wins.** Excel writes every
  formula's result into the file beside it (`<v>`), so for a workbook a
  spreadsheet program saved, the grid already holds that program's answers and
  nothing recomputes them. A workbook written by a *library* (`openpyxl`,
  `xlsxwriter`, `pandas`) carries the formula and no value, and those cells
  used to show blank — which is the one answer that is certainly wrong. So
  `sheetGrid` records **only** formulas with no cached value, `Xlsx.resolveFormulas`
  runs `Cmedit.Formula.evalWorkbook` over the whole workbook (whole-workbook
  purely so `=Summary!B2` resolves), and the status note reports how many were
  computed and how many could not be. There is no path by which a computed
  number displaces a supplied one, which is what makes the feature safe to
  have at all — a Spec test pins it with a deliberately *stale* cached value.
  `Cmedit.Formula` is a leaf: lexer, recursive-descent parser over Excel's
  precedence, and an evaluator with a `Value` type, Excel's coercions and ~50
  functions. Four things there are load-bearing. **An unknown function makes
  the whole formula unsupported up front** (`parseFormula` walks the tree
  before evaluation), so a cell is never half-computed — and an unsupported
  formula leaves its cell exactly as it was and is *counted*, because showing
  nothing is the honest answer to "I do not understand this" and showing a
  number is not. **A genuine Excel error is an answer, not a failure**, so
  `#DIV/0!` is shown while an unparseable formula is not. **The cycle guard is
  a `Set`, not the recursion stack**: a running-total column is a chain
  thousands deep, and a linear membership test down it per cell read is
  quadratic in the chain — `maxDepth` is a separate, much larger stack guard
  reported as `#NUM!`, precisely so a long chain is never mislabelled circular
  (measured: a 3 000-deep upward chain resolves; before the split it did not).
  **`maxSteps` counts cell *reads*, not formulas**, because the cost lives
  inside range scans — a sheet of `SUM(A:A)` does millions of reads and only
  thousands of evaluations. `TODAY`/`NOW`/`RAND` are deliberately absent: this
  module is pure, and a wrong date is worse than a blank.
  **Save As is an /export/ in every buffer-less view, and that distinction is
  load-bearing.** `EffExportTo` writes a copy of what is on screen — a
  workbook's showing sheet as CSV, a DOCX/EPUB/PDF as plain text
  (`exportSuggestion`) — and, unlike `EffSaveTo`, does *not* set `edPath` or
  mark anything saved: the open document is still the workbook, and a later
  Ctrl+S must not aim at the CSV. The views with no text to export (image,
  pager) refuse (`saveAsRefusal`). This is not a nicety: `MASaveAs` is in
  neither `containerDisabledActions` nor `pdfDisabledActions`, so before it
  existed Save As on a workbook, PDF, DOCX or EPUB wrote the *empty* buffer
  underneath the view and then re-pointed the document at the file it had just
  emptied. Exporting over the source archive is refused outright, since Save As
  does not confirm before overwriting. `rtfPlainText`/`pdfPlainText` export the
  **paragraphs**, not the laid-out lines — the opposite choice from
  `rtfSelText`, deliberately: a selection is a piece of the screen, but a file
  wrapped to whatever width the terminal happened to be is a poor artifact.
  Read-only is enforced **twice**, as everywhere else: `containerDisabledActions`
  is pruned from the menus *and* guarded in `runAction` (so the shortcuts are
  inert), `handleSheetKey` swallows every grid-editing key, and
  `syncCsvToBuffer` refuses a workbook outright as the backstop under all of
  it — without that, any future save path would write one sheet of someone's
  workbook over their workbook as CSV. `MAGoToLine` is reinterpreted a third
  and fourth time (`goToUnitOK`, `mkGoToUnit`, `relabelEntry`): a *chapter*
  for an EPUB, a *sheet* for a workbook, exactly as PDF reinterprets it as a
  page, and for the same reason — a laid-out row number moves with the window
  width and would mean nothing.
  Two smaller judgements worth keeping. `rpSpace` (blank line after a
  paragraph) exists because a `.docx` and an XHTML chapter carry their
  paragraph spacing in a style sheet these readers do not resolve, so honouring
  only their *manual* blank paragraphs gives a document with gaps in some
  places and none in others; both mappers therefore drop empty paragraphs and
  set `rpSpace` on every real one, and RTF (which spaces itself with empty
  paragraphs) never sets it. And a **table row is one paragraph** whose cells
  are tab stops in both mappers — a `<w:p>` inside a `<w:tc>` must not start a
  paragraph, or every cell lands on its own line and the table's shape is gone.
  **`Cmedit.Odf` (`.odt`/`.ods`) is the same shape a third time**, and the
  cheapest of the three because it adds no target: a text body is the formatted
  view, a spreadsheet is the grid, and `odfKind` picks between them from the
  element inside `<office:body>` rather than from the extension or the
  `mimetype` member (repackaging tools drop it). `.odp`/`.odg` are positioned
  shapes, are deliberately not modelled, and fall back to the listing. Three
  things differ from OOXML and are the whole of the work. **Formatting lives in
  named styles, not on the run** — `<text:span text:style-name="T1">` with `T1`
  defined in `<office:automatic-styles>` — so a reader matching on elements
  alone sees *no* formatting at all; `odfStyles` builds that table in the same
  pass (the styles are required to precede the body) and `resolveFmt`/
  `resolvePar` walk the `style:parent-style-name` chain. **Lengths are CSS
  lengths** (`0.5in`, `1.27cm`, `12pt`), hence `lengthToTwips`, where OOXML
  writes bare twips. And **an `.ods` row is padded to the sheet width** with one
  cell carrying `table:number-columns-repeated="1024"`, plus trailing rows at
  `1048576` — expanding those literally turns every one-cell sheet into a
  million-row grid, so a repeated run of *empty* cells or rows at the end of its
  line is dropped rather than materialised. The exception that bites: a cell
  with a **formula and no value looks empty and must survive the trim**, since
  it is exactly the cell `Cmedit.Formula` is about to fill in.
  ODF is also the one format that reads *better* than its OOXML cousin: a cell
  carries its **displayed** text as well as its raw value, so the number formats
  `Cmedit.Xlsx` declines to apply are already applied. `odfFormula` translates
  the rare uncalculated formula (`of:=SUM([.A1:.B9])` → `SUM(A1:B9)`,
  `[Sheet2.A1]` → `Sheet2!A1`) into the syntax `Cmedit.Formula` reads.
  `Cmedit.Xml` is the shared leaf: a non-validating pull parser that matches on
  **local names** (OOXML producers disagree about prefixes) and never fails,
  bounded on nesting depth and text-node length. Its event list is lazy on
  purpose, so a consumer that stops (a cell budget, a character budget) stops
  the parser too. Element-*stack* matching in the mappers is load-bearing, not
  fussiness: `w:jc` inside `w:pPr` is a paragraph's alignment and `w:jc` inside
  `w:tblPr` is a table's, and matching on the element name alone lets one table
  centre every paragraph in the document.
- **Command-line conversion (`App.convertFiles`, `app/Main.hs`).** Every
  reading view turns an awkward format into text, so the same work makes the
  binary a converter: `cmedit paper.pdf > paper.txt`. **The trigger is
  `hIsTerminalDevice stdout`** (in `base`, so `app/Main.hs` stays portable) and
  needs no flag, because the editor draws on stdout — a redirected stdout
  cannot mean "open the editor". `--convert` forces it at a terminal;
  `--sheet N` picks a workbook's sheet, since a CSV holds one table and
  emitting all of them would produce something that is not a CSV.
  **Content to stdout, the description to stderr**, which is the only
  arrangement a redirect survives; stderr is explicitly set to UTF-8 first,
  since its encoding otherwise follows the locale and the arrows in the
  description would make writing one an *exception* under `LANG=C`. `convertPath`
  reuses `classifyFileWith` and matches on the `LoadOutcome`, so a format is
  convertible exactly when it is openable — with two deliberate refusals
  (an image has no text; a file over `maxOpenBytes` is already plain text, so
  `cat` is the answer) and two special cases: a missing path is a *new buffer*
  to the editor and an error here, and an `.rtf` arrives as a buffer of markup,
  which is the one thing nobody redirecting this wants (it is re-parsed through
  `rtfPlainText`).
- **Two coordinate systems.** Buffer positions (`Pos`) count *characters*; the
  screen counts *display cells*. `Cmedit.Width` maps between them
  (`colToDisplay`/`displayToCol`) and supplies a compact `wcwidth` plus tab-stop
  handling. Anything cursor- or layout-related must go through these, never
  assume 1 char == 1 column.

- **Multiple files via a zipper.** The active document lives *directly in the
  `Editor` fields*; inactive files are `Document` snapshots split into
  `edBefore` / `edAfter`. This is why edit logic operates on `Editor` fields
  unchanged. If you add per-document state, add the field to **both** `Editor`
  and `Document`, and update `captureDoc` / `restoreDoc`.

- **Word wrap is a guarded parallel path.** When `edWordWrap` is on, a separate
  visual-line model is used (`lineSegs`, `segIndexOf`, `visualOffset`,
  `moveVisual`, `ensureVisibleWrap` in Editor; `drawTextAreaWrapped` in Render).
  The default path is horizontal-scroll. Keep both paths working when changing
  navigation/scroll/cursor math. `ensureVisibleWrap` finds the new top by
  walking *backward* from the cursor (O(screen height)); never recompute
  `visualOffset` per candidate top — that made long jumps O(distance²)
  (minutes on a big wrapped file).

- **File opening: guards + async load.** Every open route funnels through
  `App.classifyFile`, which refuses files over `maxOpenBytes` and binary files
  (`TextBuffer.looksBinary`: a NUL byte in the first 8 KiB) — so a huge blob can
  never be decoded into millions of junk lines and hang the loop — sniffs images
  by magic bytes, and otherwise decodes text (`loadFromBytes`); a missing path
  becomes a new empty buffer. The *interactive* open (`EffOpen`) loads files
  larger than `asyncThresholdBytes` on a background thread that posts a
  `LoadOutcome` to a second queue (`loadQ`); `beginLoading`/`endLoading` toggle
  `edLoading` and the event loop `orElse`s over keys, load results, and a
  `registerDelay` tick that animates the spinner (`tickLoading`/`spinnerFrames`).
  While `edLoading` is set, `update` swallows input. Startup and Revert stay
  synchronous but still go through the guards.
- **Input loop.** `Cmedit.App` sets raw mode, then a background thread parses
  bytes (`Cmedit.Input.nextKey`) onto a `TQueue`; SIGWINCH pushes a `KResize`
  event. The main loop **coalesces input**: it blocks for one key, drains the
  rest of the queue (`flushTQueue`), applies the whole batch through `update`
  (effects still run per key) and repaints **once** — so held keys / fast typing
  / scrolling never let the frame rate fall behind the input rate. The parser
  disambiguates a lone ESC from a sequence with a short read timeout. On startup it also enables the **Kitty keyboard protocol**
  (`enableKittyKeys`, the "disambiguate" flag) so modified keys that legacy mode
  can't express — notably Ctrl/Shift+Enter (`KModEnter`) — arrive as `CSI … u`
  sequences; `Input.otherKey` decodes those (and the xterm `CSI 27;mods;code ~`
  form) and must map Ctrl/Alt combos back to the same keys their legacy bytes
  would produce, or those shortcuts regress. Startup also enables **terminal
  focus reporting** (`enableFocusEvents`, CSI `?1004h`): `CSI I`/`CSI O` parse
  to `KFocus`, which the driver consumes in `applyBatch` (focus-in triggers an
  immediate `pollFs` freshness pass) and the pure model ignores. `KUnknown []`
  is the EOF sentinel — no parse path may ever return it, or the loop quits.

- **File browser (`Cmedit.Browser`, `FBrowser` focus).** The Open dialog is a
  lazily-loaded directory tree. Directory listings are IO, so the pure model
  requests them via `EffBrowse` / `EffListDir` and the driver replies through
  `startBrowser` / `browserLoaded` — the same effect/callback round-trip used
  for files and clipboard. Nodes are addressed by path for `fillChildren`.
  `edBrowserPick` puts the modal browser in *folder-pick* mode (File ▸ Open
  Folder): Enter on a directory emits `EffExplorerOpen` instead of expanding.
- **File explorer panel (`edExplorer`, `FExplorer` focus).** A persistent
  VS-Code-style sidebar shown whenever a workspace folder is open. It reuses the
  `Browser` tree model (`mkBrowserNoParent`, no `..`) but lives in `edExplorer`
  (separate from the modal `edBrowser`); listings round-trip via
  `EffExplorerOpen`/`EffExplorerList` → `explorerStart`/`explorerLoaded`. The
  panel **shifts the whole document area right**: `computeLayout` reserves
  `sidebarWidth ed` columns and exposes `loContentLeft` (absolute column where
  the gutter+content begins); `loTextLeft = loContentLeft + gutter`. **Every
  text/CSV/image draw and mouse hit-test must offset by `loContentLeft`** (image
  already uses `loTextLeft`; text/CSV add it explicitly) — when no folder is open
  `loContentLeft` is 0, so the offset is a no-op. Width is mouse-draggable on the
  divider (`edSidebarDrag`), the panel collapses to a one-column strip
  (`edExpCollapsed`, via the `«` button / dragging to the far left; Ctrl+B on a
  collapsed strip expands and focuses it, from any focus/mode), and the `✕`
  button closes the folder via a `DKConfirmCloseFolder` dialog. Per-file
  decorations (`fileMarkFor`: open/active/`●` modified/`◆` changed-on-disk)
  are derived from the open documents. File names are also tinted by
  **type** (`Render.fileKind`, by extension): displayable images get a
  magenta `❏` glyph before the name, source we highlight is green, Markdown/
  HTML cyan, JSON/YAML/CSV/… yellow, and known binary blobs are dimmed grey
  so they read as "nothing to open" — but only for *unopened* files; the
  open/active/modified state colours (and the selection highlight) take
  precedence. Panel state is global (like
  `edBrowser`/`edMenu`), *not* per-document — don't add it to `Document`.
  **The tree self-refreshes**: expanding a directory always re-lists it (cached
  children show instantly, the fresh listing merges in), and the driver's
  freshness poll (`App.pollFs`: every 2s while focused, plus immediately on a
  terminal focus-in) stats each *expanded* directory's hi-res mtime
  (`drvDirMtimes` baseline, recorded before every listing) and re-lists only
  the ones that moved — so a `git pull` in another window shows up by itself
  for ~zero idle cost. All listings land via `Browser.mergeChildren` (through
  `Editor.mergeKeepSel`), which preserves loaded/expanded subtrees and
  re-anchors the selection by path — never `fillChildren` directly, or a
  refresh would collapse the user's open subtrees.

- **Config file & recent files (`Cmedit.ConfigFile`).** A leaf module (imports
  nothing from Cmedit, so `Editor` can import it cycle-free) owning `Config` /
  `defaultConfig` (re-exported by `Editor`) plus the pure parsers for
  `~/.config/cmedit/config` (`key = value`; unknown/bad lines come back as
  warnings shown once on the status line) and `~/.config/cmedit/recent`
  (`line:col:path`, most recent first, capped). `main` loads the config file
  *before* applying CLI flags, so flags override it. The recents list lives in
  `edRecent` (global, not per-document): loads/saves `touchRecent`/`recordRecent`
  it, `doClose` records the closing cursor position, `setLoaded` restores it
  (`restoreRecentPos`), and the File menu splices `MARecentFile` entries above
  Exit via `entriesFor` (open files are filtered out — the Window menu covers
  those). The driver persists when the list's *path order* changes (cursor moves
  alone don't trigger writes) and once more — with live positions via
  `recentsForPersist` — on the way out (a `finally`, so SIGTERM exits count).
- **Settings dialog (`DKSettings`, File ▸ Settings…, Ctrl+,).** Built on the
  dialogs' generic **choice rows** (`Dialog.Choice`: label + value list +
  index, optional section header, per-row hint; focus order fields →
  options → choices → buttons; Left/Right/Space/Enter and value clicks cycle
  — Enter deliberately does *not* confirm while a choice is focused; every
  change funnels through the hub hook `choiceChanged`; renderer and mouse
  share the `choiceCols` geometry, where the `‹ value ›` token is compact
  and right-aligned). `EditorEdit.settingsSpec` is the **single source of
  truth** for what row k means (labels, values, hints, Config→index);
  `Editor.applySettingRow` interprets the same indices, updating the config
  AND the live session mirrors (word wrap, line numbers, whitespace, CSV
  freeze) so changes show behind the dialog immediately. `openSettings`
  first reconciles the session toggles *into* `edConfig` (rows always show
  effective values; Save persists what's on screen) and stashes that config
  in `edSettingsStash`; Cancel/Esc re-applies the stash through
  `applySettingRow` — one code path for apply and revert. Save emits
  `EffSaveConfig` → the driver rewrites the config file via the pure
  `ConfigFile.updateConfigText` (values updated in place, every duplicate
  occurrence rewritten since the parser's last-line wins, trailing `#`
  comments and unknown lines preserved, absent keys appended only when
  non-default). Adding a config key = extend `Config`/`applyKey`/
  `configFields`/`configKeysHelp` (ConfigFile), one `settingsSpec` row, and
  one `applySettingRow` case.

- **Dynamic menus.** Menus are mostly static data in `Cmedit.Menu`, but
  `entriesFor`/`pruneEntries` adjust them per context: the Window menu's entries
  come from the open-files list, the View menu's "Table View" is dropped unless
  the active file is a `.csv`/`.tsv`, the Edit menu's "Delete" is dropped
  unless there's a selection, the File menu's "Revert" is dropped unless
  `revertAvailable` (the active file has a path and either has unsaved edits or
  changed on disk), the File menu's "Save All" only shows with >1 file open and
  unsaved changes, and in the read-only image view the Find menu's in-file
  entries (`imageDisabledFind`: Find/Next/Prev/Replace/Go-to-Line) are dropped —
  workspace Find/Replace in Files stay. `runAction` guards those same actions so
  their keyboard shortcuts (Ctrl+F/R/G, F3) are inert on an image too. All menu
  navigation and rendering goes through
  `Editor.entriesFor ed mi` (not `Menu.entriesOf`) so these dynamic menus work —
  keep using `entriesFor` if you touch menu code. The active document lives in
  the `Editor` fields; the zipper (`edBefore`/`edAfter`) holds the rest (see the
  multi-file note above).
- **Stale-file detection & Revert.** Each loaded/saved doc records the on-disk
  mtime (`edDiskMtime`/`docDiskMtime`, an `EpochTime` from `loadFile`/`saveFile`).
  Opening any menu makes `update` emit an `EffStatFile`; the driver stats the
  active file and calls `noteDiskMtime`, which sets `edDiskChanged` when the file
  is newer than the recorded baseline. The driver's **freshness poll**
  (`App.pollFs`, every `fsPollDelayUs` = 2s, plus immediately on a terminal
  focus-in) also stats *every* open document and folds the results in via
  `noteDiskMtimes`, so the ◆ markers and the active file's "changed on disk"
  status notice appear without opening a menu. "Revert" (`MARevert`) reloads in
  place via `EffRevert` → `revertLoaded` (confirming first when there are
  unsaved edits).
  Re-opening a file that is already open switches to it instead of opening a
  second copy: `setLoadedNew`/`imageLoadedNew` consult `findOpenIndex`, and the
  driver canonicalises paths on open so command-line and browser paths compare
  equal.
- **Menu mnemonics.** Both top-level titles and dropdown items mark their
  mnemonic with `&` (e.g. `F&ind` underlines the `i`, leaving `f` for File);
  `parseMnemonic` strips it for display and `menuTitleDisp` gives the on-screen
  width. `menuAccelFor` maps an Alt-letter to a menu via `mnemonicChar`, so use
  the display title (not the raw `menuTitle`) for any bar geometry.

- **CSV table mode (`Cmedit.Csv`, `edCsv`/`docCsv`).** `.csv`/`.tsv` files open
  in a spreadsheet view. `edCsv` is `Just` when the active doc is in table mode;
  the dispatch in `update` (FEdit branch) routes to `handleCsvKey` when so. The
  table is the live model while in CSV mode — the line buffer is *stale* and is
  re-synced by `syncCsvToBuffer` on save (done in `App` before `saveFile`) and
  on toggling back to text. The table carries its own undo. Mode is per-document
  (`docCsv` in the zipper). When adding save paths, they go through `EffSaveTo`,
  which already syncs — don't read `edBuffer` directly for CSV docs.
  **Every route in builds the grid from the buffer's *lines*** — `mkCsvLines` /
  `csvParseLines`, never `mkCsvView (bufferToText …)`, which is now only the
  clipboard's and the tests' entry point. The parser is a cursor into a list of
  lines (the remainder of the current line plus the lines after it) with the
  newline between them implicit; `csvParse` is a `T.split (== '\n')` in front of
  the same engine, so there is one parser and not two that could drift. Two
  things follow. Re-joining the buffer to parse it cost a second whole copy of
  the file, live for as long as the document stayed open (the cells were slices
  of *it*, not of the buffer's array) — 32 MB of live heap and 134 MB of RSS on
  a 32 MB table; parsing the lines makes both models share one array. And a
  line with no quote and no CR takes a fast path that never enters the field
  machinery at all. Cell text is `Text` slices throughout, so `0014`'s
  copy-at-escape-boundaries rule applies (CSV copy/cut detaches). `cellWidth`
  runs on every cell of the grid on load, so it is a strict fold with no
  `T.unpack` and no `T.splitOn` — 2.7 GB of the 3.7 GB it used to cost to open
  a 32 MB table was that one function (`0026`).
  Cells may contain newlines (Shift/Ctrl+Enter): a row's on-screen height is its
  tallest cell capped at `Csv.maxCellLines` (3), so rows have *variable height*.
  `Csv.rowHeight`/`csvRowLayout` (Render) and `Csv.rowAtLineOffset` (mouse) must
  stay in sync; `scrollTop` and `cellDisplay` handle vertical/horizontal
  scrolling within a tall or being-edited cell. Up/Down while editing move
  between a cell's lines (`editLineUp`/`editLineDown`) before crossing cells.
  `edFreezeHeader` (View ▸ Freeze Header Row; **on by default** via
  `cfgFreezeHeader` / config key `freeze-header`, seeded in `newEditor`)
  pins row 0 below the column header:
  `csvRowLayout` lays it out separately and `ensureVisible`/`csvMouse` take a
  freeze-row count so scrolling and clicks skip the pinned row.
  Rectangular cell selection is `csvSelAnchor` (the far corner); `selRect`
  derives the box. Shift+nav/`withSel` grows it, plain nav/`clearSel` collapses
  it. Copy/cut use `copyText` (a mini-CSV for a box, the raw value for one cell);
  `pasteClip` decides fill vs. spread vs. shaped-overwrite from the clipboard
  grid and selection sizes; delete/cut use `clearSelCells`.
  **Column widths are cached** (`csvWidths`, one clamped width per column, and
  `nCols` is its length): `columnWidths` runs per repaint and per cursor move,
  so it must never rescan the grid. Every `csvRows` change flows through
  `withRows` or undo/redo, which carry the cache via `syncWidths`
  (pointer-diff of the persistent rows; per-cell edits are incremental, shape
  changes recompute) — if you ever set `csvRows` anywhere else, sync
  `csvWidths` too or a fuzz test in `Spec.hs` will fail. `Csv.sortByColumn`
  (Alt+S, toggling asc/desc via `sortedAscBy`) follows that discipline:
  snapshot → `withRows`, numeric-aware keys, frozen header pinned, cursor
  re-anchored to its row. **The modified flag is maintained state, not a
  comparison** (`csvDirty`, read per keystroke by `Csv.isModified` via
  `csvMod`), under the same discipline as `csvWidths` and for the same
  reason — comparing the grid against `csvSaved` cost 2.5 ms and 14 MB per
  keystroke on a 223 000-row table. `CsvDirty` is `DirtyShape` **exactly
  when** the two grids' shapes differ (row count, or any row's length) and
  `DirtyCells n` **exactly when** the shapes match and `n` cells differ;
  both directions of that biconditional are load-bearing, since `withCell`
  keeps `DirtyShape` across a cell write without looking at the grid, and
  every producer of `DirtyCells` relies on the converse. Three producers and
  no others: `dirtyCell` (O(log rows) — a cell write knows its old, new and
  saved text, so the count moves by ±1, which is *why* editing a cell back
  to its saved value clears the flag by construction), `syncDirty`
  (`withRows`/undo/redo, pointer-diff like `syncWidths`; a shape change is
  the cheap case, and a shape *restored* pays one recompute because indices
  cannot be trusted across it) and `dirtyFrom` (from scratch, for
  `rebaseHistory`). Set `csvDirty` only where `csvWidths` is set, plus
  `markSaved`/`markUnsaved`/`mkCsvGrid`; the `Spec.hs` fuzz test runs every mutating
  operation in the module and checks the count against an independent
  recomputation after each one. It must stay **exact** at any size — no
  big-table cutoff, no approximation — because the flag drives the title
  bar, the quit confirmation, Save All, `edDocSeq` and the crash journal's
  sweep. Note `ptrEq` did *not* save the old design: measured under `-O2`,
  `ptrEq (csvRows v) (csvSaved v)` returns `False` even for a freshly built
  view, so the unmodified case walked the whole grid too (0028).
  **A table carries its own saved baseline, so a `Document` that is dirty for
  a reason the buffer knows about has to say so twice.** `mkCsvGrid` adopts the
  grid it is handed as `csvSaved`, which is right for a file read off disk and
  wrong for one recovered from a crash journal — there, `docSavedBuffer` is
  deliberately `emptyBuffer` (the recovered text differs from disk by
  definition) and the table needs `Csv.markUnsaved` for the same reason, or
  `csvMod` recomputes the flag off the recovered grid, calls the document
  clean, and the journal sweep deletes the only copy of the work. The same
  guard is why `plainToCsv`'s *no-stash* branch is gated on `bufModified` — a
  staged workspace replace opens a `.csv` with no table and no stash, so Alt+T
  would otherwise parse its dirtiness away. (The stash branch never needed it:
  `rebaseHistory` keeps the old saved point.) `markUnsaved`
  sets `csvSaved` to the *empty* grid, which is an exact baseline rather than a
  sentinel: `mkCsvGrid` guarantees at least one row, so `DirtyShape` is the
  biconditional's own answer and no cell write can restore a zero-row shape.
  **A third cache rides the same discipline: `csvNl`, the sparse map of rows
  that contain embedded newlines and how many each contributes** to the
  serialised CSV (absent = zero). It exists because `cellTextPos` — "which
  line of the serialised file does this cell begin on?" — used to answer by
  re-serialising *every row above the cursor*, and that question is asked by
  the recents, the session file, the crash journal and the nav history. Set it
  in exactly the places `csvWidths` is set (`mkCsvGrid`, `withCell`,
  `withRows`, `undo`, `redo` — and nowhere else; `rebaseHistory` keeps `new`'s
  grid, so `new`'s own map is already right), via `computeNl` (from scratch),
  `syncNl` (the pointer-diff twin of `syncWidths`/`syncDirty`) and, on the
  typing path, a ±delta in `withCell` that looks at nothing outside the row.
  `linesBefore` prefix-sums it in O(log rows + multi-line rows above), and
  **both** mappings read it: `cellTextPos` for the forward direction and
  `textPosCell` for the inverse (a binary search over `i + linesBefore i`,
  which is strictly increasing — it no longer serialises the file down to the
  target line). The `Spec.hs` fuzz test checks the map against an oracle built
  only out of `csvToText`, and both mappings against the from-scratch versions
  they replaced. The vertical *display* geometry family — `rowHeight`,
  `csvRowLayout`, `rowAtLineOffset`, `scrollTop`/`ensureVisible` — is
  unrelated and already O(screen): it measures from `csvTop`, never from row
  0, and must stay that way.
  **Nothing on the per-batch driver path may ask a table document where its
  cursor is.** `EditorDoc.sessionShape` is evaluated by the driver after every
  key batch and deliberately records no positions — but it used to *project*
  them out of a whole `sessionForPersist`, and `RecentEntry`'s fields are
  strict, so building one forced the `docCursorPos` the projection then
  dropped: 390 ms per keystroke typed into the last row of a 223 209-row CSV
  (0029). It now derives folder/paths/active index from `sessionDocs` without
  touching a `Document`'s cursor, and a `Spec.hs` test pins that with a grid
  whose rows are `error` (a `Seq` is spine-strict but element-*lazy*, so
  nothing structural can see this property).
  **In-file find searches cells, not the buffer** (`csvFindWith`,
  `csvSearchOrder` in `EditorFind`): the line buffer is stale in table mode, so
  a character range would have nowhere to go — the unit a grid can show you is
  a *cell*, and the hit becomes the cell cursor. `csvMatchingCells` feeds both
  the dialog's live count and `csvOrdinalMsg`'s "Match 2 of 3 in B4", and
  `liveCellMatch` (the grid's answer to `liveMatchSpans`) lights every matching
  cell in `drawCsvTable`, below the cursor and above the selection. Both
  counting paths are bounded by `liveCountMaxCells`, the grid's
  `liveCountMaxChars`: the count is a full scan whenever matches are sparse and
  it runs on every dialog keystroke — measured at 58 ms per keystroke on a
  100 000-cell sheet, 6 ms once bounded. *Finding* is never bounded, because it
  stops at the first match. Note `liveFindTerm` used to opt out of CSV
  entirely; it no longer does, and anything new that consumes it must cope with
  the table view being live.
  **User column widths**: dragging a column's border on the header row (like
  the explorer divider: press starts `edCsvColDrag`, which swallows the mouse
  until release; double-click re-fits) sets a sparse per-column override
  (`csvUserW`) that `columnWidths`/`scrollLeft` fold over the cached widths —
  overrides shift with `insertColAt`/`deleteCol` and are per-document via
  `docCsv`. `csvBorderColAt`/`csvColStartX` (EditorState, where
  `csvGutterWidthFor` also lives) are the shared border geometry for the hub's
  hit-test/drag and the `col-resize` pointer hint.

- **Image view mode (`Cmedit.Image`, `edImage`/`docImage`).** A third,
  read-only view mode, structured exactly like CSV but sharing none of its
  machinery (it does *not* go through the line buffer at all). `Cmedit.Image` is
  a self-contained, IO-free module that decodes BMP, Netpbm, GIF, PNG, JPEG
  (baseline **and** progressive) and WebP (lossless VP8L **and** lossy VP8,
  incl. the ALPH alpha chunk and an animation's first frame) from raw bytes
  using only boot libraries — GIF LZW, JPEG Huffman/IDCT, VP8L prefix
  codes/transforms, and the VP8 boolean decoder / intra prediction / loop
  filter are all hand-rolled, and DEFLATE lives next door in the leaf module
  `Cmedit.Inflate` (`inflate` for a known output size, which PNG has;
  `inflateDyn`, which grows its buffer, for PDF content streams, which declare
  only their *compressed* length; `Huff`/`buildHuff` are shared with JPEG's
  own canonical tables) — into
  an RGBA `Image`, and `renderImage` area-averages it down to a grid of half-block
  (`▀`, fg=top pixel / bg=bottom pixel, 24-bit colour) or ASCII-ramp `Cell`s.
  Detection is by **magic bytes** in `App.openPath` (not extension): it sniffs
  the file, decodes images via `imageLoaded`/`imageLoadedNew`/`addImageDocument`,
  and surfaces a `setError` for anything it recognises but cannot decode (rather
  than dumping binary as text). The `update` FEdit branch routes to
  `handleImageKey` when `edImage` is `Just` (before the CSV check); that handler
  delegates global shortcuts to `handleEditKey` and swallows editing keys (docs
  are read-only). The scaled grid is cached in `idCache` keyed by
  `(cols,rows,mode,crop,cellPx)`; `refreshImage` (called from
  `resize`/`restoreDoc`/mode toggle/`setCellPx`) re-scales only when that key
  changes, so resizing regenerates the picture but per-keystroke cost is a
  comparison. `viewFit`/`renderImage` take the sub-pixel aspect ratio
  (`cellAspect ed`, derived from `edCellPx` — the winsize ioctl's
  ws_xpixel/ws_ypixel or the XTWINOPS replies; 1.0 = the classic 2:1 cell
  assumption when unknown) so pictures keep true proportions in any font,
  plus a `maxScale` cap (`imageFitCap ed idoc`): when the cell-pixel size is
  known and the whole image is shown, the fit is capped at native resolution
  so a small picture sits centred at 1:1 rather than being blown up to fill —
  a large one still shrinks to fit, and a zoom crop lifts the cap. The mouse
  crop mapping (`cellRectToCrop`) shares the same geometry (cap included).
  `computeLayout` forces gutter 0
  for image docs so the picture uses the full width. `'a'` toggles
  `HalfBlock`↔`Ascii`. Per-document state lives in `docImage` in the zipper.
  **Opening focus:** `explorerActivate` no longer forces `FEdit` on open —
  focus follows the loaded document, so `setLoaded` (text/CSV) hands focus to
  the editor while `imageLoaded`/`imageLoadedNew` keep it in the panel when the
  open originated there (`edFocus == FExplorer`), since a read-only image has no
  keystroke editing to receive it.
  **Zoom:** `idCrop` is the displayed source-pixel rectangle (`Nothing` = whole
  image) and is part of the render-cache key. A left-drag records `idDrag` (a
  cell rectangle, drawn as a reverse-video border overlay each frame, *not*
  cached); on release `cellRectToCrop` maps it to source pixels via the shared
  `Image.viewFit` geometry and sets `idCrop`. A single click or Esc clears
  `idCrop` back to the full image. Drag/click are handled in `handleImageMouse`.
  **Animation:** `Image.decodeFrames` returns every frame of an animated GIF
  as composed full-canvas RGBA images with per-frame delays (disposal
  methods, sub-rectangles, local palettes and transparency handled in
  `composeGifFrames`; delays under 20ms clamp to 100ms; a `maxAnimBytes`
  budget truncates over-long animations and `decodeGIF`/`decodeImage` stay
  the cheap first-frame path). Frames live in `idFrames`/`idFrame` in the
  `ImageDoc` (so the zipper carries them) and the current frame index is part
  of the `idCache` key. **Who steps the frames is capability-dependent**,
  and answering the kitty-graphics probe is NOT enough to be trusted with
  playback: Ghostty/WezTerm/Konsole speak the static protocol but silently
  drop the animation actions, so native animation is gated on the XTVERSION
  whitelist `Caps.supportsKittyAnim` (real kitty only, mirrored into
  `edGfxKittyAnim` by `setGfxCaps kitty anim sixel`). On whitelisted kitty
  the driver uploads the whole animation once with the placement
  (`Gfx.kittyPlaceAnim`: `a=f` frames + root-gap + `a=a s=3,v=1` run-loop,
  total pixels bounded by `maxAnimGfxPixels`) and the *terminal* loops it —
  the editor never ticks. On a static-kitty terminal the driver instead
  pre-uploads every fitted frame as its own image id (`Gfx.kittyClientAnim`,
  same pixel budget) and the editor's tick swaps the visible placement
  (`Gfx.kittySwapFrame`, a few dozen bytes: `gfxOverlay` detects a
  same-key-but-`gkFrame` change and `placeGfx` gets the previous frame to
  swap from). Everywhere else the event loop arms a separate `ImgTick`
  timer from the pure `imageTickUs` (current frame's delay, floored at 50ms
  for half-block cells and kitty placement swaps, and at a
  pixel-area-scaled ≥100ms for sixel, whose placement is re-emitted whole
  per frame) and `tickImage` advances the frame, re-checking `imageTickUs`
  so ownership changes between arm and fire are safe. A still image or a
  backgrounded animation costs nothing (the timer is simply not armed, like
  the settled About box). Zooming (`idCrop`) on any kitty-protocol terminal
  deliberately shows a frozen still of the current frame (re-uploading a
  cropped frame per tick would be a full-cost transmission); the cell/sixel
  paths keep animating the crop. The graceful floor is always the still
  image, and dumb terminals get the animated half-block picture, which
  needs no capability at all.
- **Editing QoL cluster (all pure `Editor` logic).** Line ops (duplicate /
  Alt+↑↓ move — `EKMoveLine` coalesces held moves into one undo — delete /
  Alt+J join) work on `selLineSpan` (a selection ending at col 0 excludes that
  line) and are guarded by `lineOpBlocked` + pruned from the Edit menu in
  CSV/image views. Toggle comment (Ctrl+/, legacy byte `^_`) uses
  `Syntax.langComment` (line prefix or block pair per language); `Input.otherKey`
  maps Kitty Ctrl+punctuation back to `KCtrlChar`. Bracket matching:
  `TextBuffer.matchBracket` (budgeted scan, `maxBracketScan`), rendered via
  `bracketPair` and jumped with Ctrl+]. Find dialogs live-highlight every match
  (`liveMatchSpans` feeding `expandLineCells`' styled overlays) with a capped
  count in `dlgMessage` (`refreshFindCount`, applied in the FDialog dispatch
  wrapper) and "Match k of N" via `matchOrdinalMsg`. Ctrl+Space word completion
  (`edComplete`, popup drawn only when `edFocus == FEdit`): candidates from all
  open buffers, active buffer scanned outward from the cursor, narrowed by
  typing; any unhandled key dismisses-and-passes-through.
- **File properties & interactive status bar.** `edSavedEol`/`edSavedEnc` (and
  doc twins) baseline the EOL/BOM so `metaModified` keeps the file dirty until
  saved — every modified-flag computation composes it (afterEdit, undo/redo,
  csvMod). `statusRightInfo` builds the status bar's right text AND its click
  zones (`StatusZone`) in one place; `statusClick` dispatches (Ln/Col → Go To,
  INS/OVR, BOM, EOL). View-menu labels for EOL/BOM/theme are rewritten per
  document by `relabelEntry`. Save-time cleanups (`applySaveFixups`/`…All`,
  config keys trim-trailing-whitespace / final-newline) run in the driver's
  `EffSaveTo`/`EffSaveAll` handlers as undoable edits; CSV/image docs exempt.
- **Navigation history (`edNavBack`/`edNavFwd`, Alt+←/→).** `pushNavIfFar`
  records the origin before each *user-initiated* jump (go-to-line, find,
  bracket, Ctrl+Home/End, `openMatch`, quick open, recents, explorer/browser
  opens, MASwitchFile — Alt+digits route through the action for this); same-file
  jumps under `navFarLines` don't count. `navBack`/`navFwd` travel via
  `openMatchRaw`, the push-free variant — keep it that way or the stacks
  self-corrupt. Untitled-buffer stops are dropped when unreachable.
- **Explorer file management.** Ins/Ctrl+N (`DKNewPath`; trailing `/` = folder),
  F2 (`DKRename`), Del (`DKConfirmDelete`) in the panel; the target is
  re-derived from the tree selection at confirm time (no payload in Dialog).
  Effects `EffCreatePath`/`EffRenamePath`/`EffDeletePath` run in App, re-list
  the parent immediately (`refreshExplorerDir`), select the result and (files)
  open it; `renamePaths` prefix-rewrites open docs/recents/nav stops under a
  renamed directory. These dialogs (incl. Esc via `cancelDialog`) return focus
  to the panel (`backToExplorer`).
- **Input history & pristine fields.** `edFindHist`/`edReplHist` (persisted in
  `~/.config/cmedit/history`, show-escaped so multi-line terms survive) are
  recalled with Up/Down in the Find/Replace dialog fields (`histRecall`; the
  in-progress draft is stashed). `dlgPristine` makes the first keystroke replace
  a seeded term (cleared after any key by the FDialog wrapper); renames are
  deliberately not pristine.
- **Themes.** `Render.themeFor (resolvedTheme ed)` picks the palette per
  frame; `Theme` carries `thTokens :: Tok -> Style` so the syntax palette
  differs per theme (light swaps washed-out brights for dark hues). Config key
  `theme =
  auto|dark-terminal|light-terminal|cherry-blossom|flashbang|midnight|graphite`
  (default `auto`; the old `dark`/`light` spellings still parse):
  `resolvedTheme` maps `auto`
  through `edDetectedDark` — the driver's OSC 11 background query, re-run on
  every focus-in so a system light/dark switch follows — and falls back to
  dark when the terminal never answers. Paint with `resolvedTheme`, never
  `cfgTheme`, or `auto` breaks. View ▸ Theme… opens a picker dialog
  (`DKTheme`/`mkTheme`, one button per `themeChoices` entry, focus starting
  on the current mode; its eight buttons are why dialog button rows wrap —
  `EditorState.buttonRows` splits them at `buttonRowMaxW`, shared by the
  renderer, mouse hit-test and `dialogGeom`): moving the focus
  **live-previews** the theme —
  `resolvedTheme` consults the open picker's focused button, so Esc/Cancel
  restores simply because nothing was written — and Enter commits via
  `applyTheme` (per-session; the config key persists it). Note the two
  terminal themes keep the terminal's default background, so a preview
  restyles chrome/tokens only, while the forced-background four —
  cherry-blossom (pink), flashbang (pure white), midnight (deep navy) and
  graphite (neutral near-black) — repaint every cell via `thRemap`, so the
  terminal palette never shows through. Graphite is the one whose chrome
  sits *below* the page (`gpBar` darker than `gpBase`, IDE-style), so its
  remap deliberately maps the hardcoded `White` panel grounds just above the
  page instead of well above it.
  `EditorEdit.themeLabel` doubles as the config word and the UI
  label — keep any new theme's label parseable by `applyKey`. The driver
  also matches the cursor colour to the theme (OSC 12, reset on exit) —
  previews included, since it reads `resolvedTheme` per frame.
- **About-box animation (`Cmedit.About`, `edAboutTick`).** The About dialog's
  wordmark animation ("CMD" and "edit" snake in from opposite sides; the big D
  vaults over the e and clobbers the little d to spell "CMeDit") is pure frame
  generation: `aboutFrameCells width frame` returns positioned styled cells
  that `Render.drawDialog` overlays on the `aboutCanvasH` blank lines reserved
  at the top of `aboutText` (which `dialogGeom` widens to `aboutCanvasMinW`).
  The event loop arms its tick timer at `aboutTickUs` (~30 fps) while
  `aboutAnimating` holds (About dialog open and `edAboutTick <
  aboutTotalFrames`); `tickAbout` advances the counter and is a no-op at the
  end, so a settled About box stops ticking and costs nothing. `openAbout`
  resets the counter, replaying the animation on each open. The module only
  emits single-width block glyphs (no `contChar` continuation concerns), and
  it must stay dependency-free (imports `Cmedit.Types` only) since both
  `Editor` and `Render` import it.
- **Keyboard help card & built-in manual (`Cmedit.HelpCard`, `Cmedit.Manual`).**
  F1 opens a `DKHelp` dialog whose body is blank canvas lines overlaid with
  positioned styled cells (`helpFrameCells`) — the About-box mechanism exactly
  (same leaf-module constraints: `Cmedit.Types` only, single-width glyphs; a
  Spec test enforces `charWidth == 1`) — so the two-column card gets ruled
  section headers and bold keys without touching the dialog machinery;
  `dialogGeom` widens the box to `helpCanvasMinW`. Keep the card curated — the
  exhaustive reference belongs in the manual. Its "Manual" button (btn 0 in
  the `DKHelp` confirm) and Help ▸ Manual (`MAManual`) call `openManual`
  (EditorDoc): the Markdown manual opens as an ordinary but **read-only**
  document under the pseudo-path `manualPath` (`cmedit://Manual.md` — the
  scheme prefix can't collide with canonicalised real paths, and `.md` picks
  the Markdown lexer), so navigation/find/wrap/highlighting all just work.
  Re-opening switches to the open copy (`findOpenIndex`); `doClose` and
  `saveAsDialogFlow` special-case `manualPath` (never recorded in recents,
  Save As seeds a plain filename). `Cmedit.Help` is unrelated — it is the CLI
  `--help`/`--version` text.
- **Workspace find/replace (`Cmedit.Search`, `edSearch`/`FSearch`).** A VS-Code
  / Sublime-style "search in files" view that occupies the main content area
  (offset by `loContentLeft`, so it sits right of the explorer). It is drawn
  whenever `edSearchMode` is set (**not** derived from focus) — that flag stays
  set while a menu or dialog overlays the panel, so the menu bar and the
  Replace All confirmation render *over* the results and return to `FSearch`
  afterwards; keyboard interaction is gated on `edFocus == FSearch`.
  `edSearch :: Maybe SearchState` is *global* (like
  `edBrowser`/`edMenu`, **not** per-document) and persists across hide/show so
  opening a result and returning finds the results intact. `Cmedit.Search` is
  pure *data*: the input fields, options (case/word/regex), the grouped result
  tree (`FileResult`/`Match`, one `Match` per matching line), glob matching
  (`pathIncluded`/`dirPruned` — also used by the walker to prune `.git`,
  `node_modules`, build dirs; `matchGlob` is memoised O(pattern × path) —
  plain backtracking is exponential in the stars, and a user-typed
  `*a*a*a*a*b` include would hang the walker), and the header/result **row layout**
  (`headerLines`/`resultRows`/`focusItems`) shared by the renderer
  (`drawSearch`) and mouse hit-testing. The directory walk + file reads are IO,
  so they follow the **same effect/round-trip pattern as the browser**:
  `runSearch` emits `EffStartSearch` with a monotonic **generation** id; the
  driver seeds the open documents' in-memory matches synchronously
  (`searchOpenDocs`, so unsaved edits are searched) then forks a background
  walker (`App.runWalker`) that streams `SMFile`/`SMProgress`/`SMDone` over a
  second queue (`searchQ`) into `searchFileFound`/`searchProgress`/`searchDone`.
  A `TVar Int` gen counter lets a new search **supersede** a running one (the
  walker bails when it changes; stale updates are dropped by the gen check). The
  walker prunes/skips aggressively (dot-dirs, default-exclude dirs, symlinks,
  files over `maxFileBytesToSearch`, binaries — by extension without opening
  them (`Search.binaryExtension`), otherwise by NUL-sniffing the first 8 KiB
  *before* the bulk read) and caps matches
  (`maxMatchesPerFile`/`maxTotalMatches`) so thousands of files stay cheap; the
  walk thread feeds candidates through a bounded `TBQueue` to a pool of grep
  workers (≤ 4; `runTui` raises the RTS capabilities to `min 4 (cores-1)` at
  startup, so the pool is parallel but can't monopolise the machine); the
  spinner ticks only while the panel is visible; results stream in O(1) per file
  (a `Data.Sequence` append + a running `ssTotal`, no re-sort/re-clamp) and the
  event loop **coalesces** the whole search-queue backlog before one repaint, so
  a broad search over a huge tree can't flood the terminal. **Replace is
  staged** (the chosen model): open plain-text buffers are edited in place
  (undoably), and closed files are *opened as unsaved documents* with the change
  applied (`EffStageReplace` → driver reads each, `addStagedDoc`), never written
  to disk until saved; then `stageReplaceDone` expands the explorer to reveal the
  now-dirty files (`App.revealInExplorer` loads/expands ancestor dirs via
  `Browser.nodeAt`/`expandAt`/`selectPath`) and focuses it. **Save All** (File
  menu, `MASaveAll` → `EffSaveAll` → `modifiedDocsToSave`/`savedAll`) writes every
  dirty document at once. A very large replace (over `maxStageReplaceFiles`) falls
  back to the direct on-disk path (`EffReplaceOnDisk` → `TextBuffer.replaceInFile`,
  BOM/line-endings preserved, then re-run). All replace paths share
  `replaceSubst`. `runReplaceAll` confirms via a `DKConfirmReplaceAll` dialog
  above `replaceConfirmThreshold` files; `runReplaceFile` (Ctrl+Enter on a row)
  replaces just one. (Aside: informational dialogs — the single-button
  binary-file warning and About, plus the two-button F1 help card (`DKHelp`) —
  dismiss on a click *off* the box; multi-button confirms stay modal.) Opening a result uses `edPendingJump` so the cursor lands
  on the match even after an async (large-file) load. The search view is opened by
  **F4** (find) / **F6** (reveal replace) — function keys, so no terminal grabs
  them; `Ctrl+Shift+F/H` stay as aliases but reach the editor only via the
  **Kitty protocol** (`Input.otherKey` maps `CSI code;mods u` with ctrl+shift to
  `KCtrlShiftChar`), which terminals like Ghostty intercept. The Find menu items
  are the always-works fallback. Regex is a from-scratch Thompson-NFA/Pike-VM
  engine in `Cmedit.Regex` (compiled once per search via
  `Search.compileMatcher`): matching is linear-time in the line, so
  pathological patterns like `(a+)+b` cannot hang the search and no match is
  dropped to a step budget; keep lexers/matchers cheap since they run per line.
- **Searching inside documents (`Cmedit.DocText`, `ssDocs`/`sqDocs`/`scDocs`).**
  The same panel, opt-in with **Alt+D** or the `[Doc]` chip, also greps PDFs,
  Word/OpenDocument files, workbooks and e-books. Three decisions carry it.
  **(1) The text comes from `classifyFileWith`, not from a lighter path.** The
  walker's document branch (`App.extractDocFile` → `grepDoc`) builds the very
  same `LoadOutcome` opening the file would, then flattens it
  (`extractPdf`/`extractRtf`/`extractBook`). A cheaper bespoke extractor would
  be faster and would eventually disagree with the view — and a search that
  finds a phrase the reader then cannot is worse than one that never looked.
  It costs what it costs (measured ~100 ms for a 171 KB PDF or a 57 KB DOCX,
  ~250–500 ms for a workbook, against ~1 ms for a source file), which is the
  whole reason the option is off by default; `documentExtension` gates
  admission and `maxDocBytesToSearch` (64 MB, deliberately larger than
  `maxFileBytesToSearch`) sizes it, since a 20 MB PDF is an ordinary PDF where
  a 20 MB source file is not. The document extensions are also *in*
  `binaryExtensions`, which is the correct relationship: with the option off
  they must stay excluded, and the walker consults one list or the other.
  **(2) A hit is addressed by a unit, never by a line.** These views lay out
  against the terminal width (`rdCache`/`pdCache`), so a stored row points
  elsewhere in a wider window. Every one of them does have an intrinsic unit
  its view can already navigate to — page, chapter, paragraph, sheet — because
  `MAGoToLine` is reinterpreted as exactly that in each; so `DocUnit` carries
  `(duIndex, duLabel)` and `docMatches` stamps it onto `Match.mUnit` (on the
  *match*, not the `FileResult`: a workbook's unit is a **cell**, which changes
  line by line, and a per-file map would need an entry per extracted line where
  a sheet has millions). Landing is `applyPendingDoc` (`edPendingDoc`, a
  separate field from `edPendingJump` because it addresses a view rather than a
  buffer): relayout, go to the unit, seed the caret at **the unit's own first
  line** — `rtfSectionLine`/`rtfParLineRange`/`pdfPageLine`, *not* `rdTop`,
  which `rtfClamp`/`pdfClamp` pull back on a short document or a last page —
  then run the view's own in-file find there. That last step is why
  `rtfFindWith` exists (an exact mirror of `pdfFindWith`) and why `MAFind` left
  `rtfDisabledActions`: Ctrl+F in a DOCX/EPUB/RTF view is the same machinery.
  **(3) Replace can never reach one.** `Search.replaceablePaths` is the single
  funnel — every replace path goes through it, `runReplaceFile` guards
  separately, and `docResultCount` feeds both the confirmation prompt's skip
  note and `stageReplaceDone`'s, since below `replaceConfirmThreshold` there is
  no dialog to carry it and a silently-untouched PDF reads as a bug. There is
  no serialiser for any of these formats and there is not going to be one — the
  reading views exist precisely because writing them back is the hard part.
  `Cmedit.DocText` is pure and imports only the format readers, so it is unit
  testable and runs on a grep worker; `DocKind` lives in `Cmedit.Search`
  instead so that low-level module needs no dependency on the readers.
- **Go to Definition (`Cmedit.Definition`, `edDefPick`/`FDefPick`).** F12 /
  Ctrl+Click / Find ▸ Go to Definition looks up the identifier at the cursor
  (via `wordRangeAt`) across the workspace and pops a modal, scrollable picker
  (`DefPick`; Enter/click opens a site via the same `openMatch`/`edPendingJump`
  machinery as search results). Detection is pure and ctags-level in
  `Cmedit.Definition`: word-bounded occurrences of the name (the linear
  `Search.lineMatches`) filtered by per-language *context shapes*
  (`defLineCols` — `def`/`class` for Python, `CREATE [OR REPLACE]
  FUNCTION|PROCEDURE [schema.]` for SQL case-insensitively, `function`/
  `const … =`/method shapes for JS/TS, plus Haskell, shell, Ruby, Go, PHP);
  only extensions in `langOf` are scanned (`defExtensionGlobs` is the walk's
  include filter, so a lookup from Python still finds SQL definitions). The IO
  side reuses the search walker: `runScan` in App is the generic pooled tree
  walk; `runWalker` (term search) and `runDefWalker` (definitions) are thin
  wrappers over it. `goToDefinition` seeds the picker synchronously from the
  open documents' buffers (unsaved edits included; those paths become
  `dfSkip`), then `EffFindDefs` forks the scan, which streams
  `SMDefFile`/`SMDefDone` into `defFound`/`defDone` with its own generation
  counter (`drvDefGen`, independent of searches). `defPickGeom` is shared by
  the renderer (`drawDefPick`) and mouse hit-testing; a click off the box
  dismisses (like the single-button dialogs). Making a document the active
  view (`setLoaded`/`restoreDoc`/`doNew`/`imageLoaded`) clears `edDefPick`
  the same way it clears `edSearchMode`.
- **Quick open (`Cmedit.QuickOpen`, `edQuickOpen`/`FQuickOpen`).** Ctrl+P /
  File ▸ Go to File: a modal fuzzy file picker over `guessRoot`. The module is
  pure data + the from-scratch fuzzy matcher (`fuzzyMatch`: case-insensitive
  subsequence, greedy from the path start AND anchored at the basename, scored
  by boundaries/consecutive-runs/basename bonuses). Cost model: a full re-rank
  of `qoFiles` happens only on a *query* edit (`qoEditField` → `qoRescore`);
  streamed discovery batches are scored alone and merged (`qoAddFiles`), so a
  50k-file walk never re-scores the world per batch. The IO side is
  `EffQuickOpen` → `App.runQuickWalker` (same dot/heavy-dir pruning as search,
  no file reads, batches of ~400 paths over `searchQ`, own gen `drvQuickGen`)
  → `quickOpenSeed` (canonical root + recents-first empty-query ordering; the
  active file is excluded) / `quickFilesFound` / `quickDone`. `quickOpenGeom`
  is shared by `Render.drawQuickOpen` and mouse hit-testing; Enter opens via
  `EffOpen` so already-open files switch and the recents cursor-restore
  applies. Making a document active clears `edQuickOpen` like `edDefPick`.
- **External linting (`Cmedit.Lint`, `edDiags`/`docDiags`).** Real-time
  diagnostics from external linters (ruff, flake8, eslint, stylelint, pyright,
  shellcheck), table-driven off `Lint.linters` — adding a tool is one row
  (extensions, argv, stdin mode, install hint, supersede link: flake8 yields
  when ruff runs). `Cmedit.Lint` is a leaf (ConfigFile imports it for the
  `lint` / `lint-<name>` keys, which round-trip `updateConfigText` like any
  other key). The driver detects availability on a startup thread
  (`detectLinters`: `findExecutable`, plus `node_modules/.bin` under the
  workspace root for node tools), re-probing on `EffDetectLinters` (Settings
  opening) and folder open; `SMLintAvail` → `lintersDetected`, which also
  refreshes an open Settings dialog's notes in place. Linting is
  **driver-initiated like `pollFs`**: after any batch that changes the
  fingerprint (path, `edEditSeq` — a counter bumped wherever the modified
  flag is recomputed — lint config, availability) the loop arms a 500 ms
  `registerDelay` debounce (`LintTick`); firing evaluates the pure
  `lintRequest` (gates: `cfgLint`, plain-text doc, real non-`cmedit://` path;
  `lrCwd` = workspace root so each project's own linter configs apply) and
  forks `runLinters`: buffer over stdin, tolerant `path:line:col:` parse
  (`parseLintOutput`), one gen-checked `SMLint` per pass — empty results
  clear old squiggles; `drvLintGen` supersedes exactly like the search
  walkers. `runToolCapture` feeds stdin and drains stderr on forked threads
  (sequential pipe handling deadlocks once a ~64K buffer fills) under a 10 s
  timeout + `terminateProcess`. Saving re-lints immediately including
  save-time-only tools (pyright is `linStdin = False`, default-off):
  `onSaved`/`savedAll` emit `EffLintNow`. Diags are per-document
  (captureDoc/restoreDoc carry them; loads/reverts reset them); positions go
  stale between passes, so **every consumer clamps** before indexing.
  Rendering: `diagSpans` widens each hit over the identifier at its column;
  `expandLineCellsFrom` takes a separate `diagOver` list that ORs
  `attrUndercurl` and sets `styleUl` onto the already-resolved style (fg/bg
  preserved, shows through selection, wide-glyph continuations inherit) —
  `Style` is now a pattern synonym over the 4-field `StyleU` (the
  Cell/`CellL` trick), and `styleUl` emits SGR 58 only under `rcUndercurl`,
  the same gate that picks `4:3` over plain `4`. Line numbers tint via
  `gutterStyleFor` (never the gutter *width* — that would oscillate the wrap
  width); the status bar gains a click-to-jump `SZDiagnostics` count zone,
  and `diagUnderCursor`'s message slots between the hover-URL override and
  `edStatus`; F8 / Find ▸ Next Problem cycles via `jumpNextDiag`
  (`pushNavIfFar` applies). Settings rows 10.. come from the linters table
  (`nEditingSettings` splits the positional `applySettingRow`; a Spec test
  pins the row sync); `Choice.chNote` is an always-visible dimmed second
  line (`DRNote`, `thDialogDim`) carrying "✓ version" / "✗ not installed —
  <install hint>", and the dialog scrolls when it doesn't fit:
  `dialogScroll` derives the top row purely from focus + height and is
  shared by `drawDialog` and mouse hit-testing (▲/▼ markers when clipped).
- **Crash-recovery journal (`Cmedit.Journal`, `edDocSeq`/`edJournalSeq`).**
  Unsaved buffers are mirrored to `~/.cache/cmedit/journal` so an SSH drop, a
  closed window or an OOM kill doesn't take the session's work with it. The
  format, the naming and the four-case recovery decision are a pure leaf
  module; the pure *spine* (`journalableDoc`/`journalOf`/`journalRequests`/
  `journalLiveKeys` in EditorState + EditorDoc) says what should be
  journalled; **every byte of IO is driver-side and modelled on the linter**
  — a `JournalTick` armed by `maybeArmJournal` when
  `journalFingerprint` moves, exactly like `maybeArmLint`/`LintTick`.
  Six invariants carry it.
  **(1) It can never endanger the real file.** Everything happens inside one
  directory of our own, written temp-file-then-`renameFile`; the save path is
  untouched; every operation is wrapped in `try` and a failure is one
  status-line note (`drvJournalWarned` stops it nagging), never a dialog and
  never a block. The directory is created `0700` via `Term.setPrivateMode`
  — a platform-layer function precisely because `src/` must stay portable
  (the Windows twin is a documented no-op).
  **(2) Removal is derived, never announced.** `journalLiveKeys` answers
  "which documents would a crash still lose", and `sweepJournals` deletes
  anything in `drvJournals` (the journals *this session* wrote or adopted)
  that isn't in it — run after every batch, so it is immediate. That is why
  there is no `EffDropJournal` on the save/close paths: Ctrl+S, Save As, Save
  All, close, quit-with-discard, Revert, undo-back-to-clean and toggling
  `journal = off` all drop the journal without knowing journals exist, and no
  future save path can forget to. The only explicit effects are the recovery
  dialog's two answers — `EffDropJournals` (Discard) and `EffAdoptJournals`
  (Recover). **Keep for later emits nothing**, and that is exactly what
  preserves it: nothing outside `drvJournals` is ever deleted.
  **(3) Staleness is per document, and it has to be.** `edEditSeq` is global,
  so a test built on it would rewrite every modified document's journal
  whenever any one of them was touched — hence `edDocSeq` (the active
  document's own counter, bumped beside `edEditSeq` in `afterEdit`/`undo`/
  `redo`, *and* in `csvMod`, which `edEditSeq` never sees, and on the
  inactive-document branch of `replaceInOpenDocs`) against `edJournalSeq`
  (what the file on disk holds). All three fields plus `edDocId` are per
  document, in `Editor` *and* `Document`, through `captureDoc`/`restoreDoc`.
  **(4) A CSV journals its table, not its buffer** (`journalTextOf`, the
  per-document form of `syncCsvToBuffer` — which only sees the active fields
  and so cannot reach an inactive table). Views with no live buffer under
  them — image, pager, PDF, workbook, container-derived DOCX/EPUB — and
  `cmedit://` pseudo-paths never journal.
  **(5) Untitled buffers need a stable id.** Zipper positions shift, so
  `edDocId` comes from the monotonic `edNextDocId` (handed out in `doNew`,
  the one place besides `newEditor` an untitled buffer is born) and names
  `untitled-<id>.cmj`. At startup `seedJournalIds` pushes the counter past
  every untitled journal still on disk, so a fresh buffer cannot be numbered
  over one the user kept.
  **(6) The write-behind is bounded in traffic and off the event-loop
  thread — and it is the *pass*, not the debounce, that is spaced.** A
  journal is a whole buffer, so a fixed 2 s tick means a 40 MB buffer under
  editing writes ~10 MB/s to `~/.cache` for as long as the session lasts
  (measured; plan 0027). The pure `journalDelayUs :: Int -> Int` spaces
  passes at `journalBudgetBps` (2 MB/s) over `journalPendingBytes` — the
  summed size of the *stale* documents, from `bufChars`, never by
  serialising to find out — floored at the old 2 s so every ordinary file
  (up to 4 MB) behaves exactly as before, and ceilinged at 30 s because past
  that the journal fails at its own job. It is applied as a **rate floor,
  not a longer debounce**: a debounce is replaced by every keystroke, so a
  20 s debounce under steady typing would journal *nothing at all*. Hence
  `maybeArmJournal`'s three rules — the first write after a quiet stretch
  still lands one 2 s debounce later, a pending timer is never pushed out,
  and a fresh one is armed no sooner than one interval after
  `drvJournalLast`. The pass itself runs on a background thread
  (`startJob JJournal`, one at a time; a tick that finds one in flight
  re-arms, like `LintTick` deferring on its rate floor), because serialising
  *and* writing 40 MB costs ~100 ms and 0011 §4 said this must never be a
  stall — and the fork happens **before** the serialisation, which is the
  larger half, since `journalRequests` hands over unforced records.
  Backgrounding costs exactly two races, both closed by **`drvJournalGate`
  (an `MVar Bool`)**: it serialises every mutation of `drvJournals` and
  every rename-into-place against `sweepJournals`, and the writer
  **re-checks `journalLiveKeys` under it immediately before renaming** — so
  a document saved mid-write has its temp file discarded instead of its
  journal appearing *after* the sweep that would have removed it (the sweep
  cannot delete a file that does not exist yet, which is why the check has
  to be the writer's). The gate's `Bool` is the second race:
  `dropJournalsOnExit` closes it, so an in-flight write cannot resurrect a
  journal the user just discarded. And the completion (`SMJournal`, over the
  search queue like every other background result) carries **the edit
  counter the record captured**, not the document's counter now
  (`journalsWritten [(key, seq)]`) — otherwise an edit landing during the
  write would be marked journalled and never written.
  Startup (`startupJournals`, before the first frame) is: GC the directory
  (`gcJournalDir` — stray `.tmp` files, the 256 MB oldest-first cap, then
  30-day-old journals whose file is gone; size before content, so it never
  reads what it is deleting), parse what's left, stat each `jPath`,
  `classifyJournal`, and hand the survivors to `openRecoverDialog`
  (`DKRecover`, Recover / Discard / Keep for later). Recovery installs
  `addStagedDoc`-shaped modified documents (`recoveredDoc`) whose disk-mtime
  baseline is the *journal's*, so `RecoverChanged` arrives already carrying
  the ◆; a journal for an already-open path patches that document rather than
  opening a second copy; and `docSavedBuffer` is deliberately `emptyBuffer`,
  not the recovered text, or the first edit-and-undo would declare the
  document clean and delete the only copy of it. **The exit drop is gated on
  `edQuit`** (`dropJournalsOnExit`): the `finally` block also runs on
  SIGTERM/SIGHUP, and deleting journals on a SIGHUP would defeat the feature
  in its headline scenario — an SSH drop.
- **Session restore (`~/.config/cmedit/session`, `--restore` /
  `restore-session`).** The journal brings back unsaved *content*; this brings
  back the *arrangement* — the open folder, the open files in order with their
  cursors, and which one was active. The file is `Cmedit.ConfigFile`'s
  (`Session`, `parseSessionText`/`renderSessionText`, next to the recents,
  reusing their `line:col:path` encoding under a `cmedit-session 1` version
  line; an unknown version means "no session", since a later format's lines
  would arrive looking exactly like malformed ones). Four invariants:
  **persistence is the recents' discipline, plus one startup write** —
  `maybePersistSession` rewrites when `sessionShape` (folder, ordered paths,
  active index) moves and the exit `finally` writes once more with live
  cursors, so opening/closing/switching persists and typing does not; writing
  *during* the session is the whole point, because the SIGKILL that
  `--restore` is meant to compose with never reaches the `finally`. The
  initial shape is written unconditionally at startup (the change-driven
  persist compares against a baseline seeded from it, so a shape-quiet run
  would otherwise write nothing and a SIGKILL would leave the previous
  session's file); `pty_session.py` scenario 4 pins this. **Restore runs before the journal scan**
  (`buildInitialEditor`, ahead of `startupJournals`), so `recoverJournals`'
  already-open-path patching lands on the restored documents instead of
  opening a second copy of each. **Untitled buffers are not recorded** —
  there is no path to reopen, and their content is the journal's job, which
  is what keeps the two features complementary rather than overlapping (the
  `cmedit://` manual is excluded for the same reason the recents exclude it).
  And **every file goes through the ordinary `openPath`/`classifyFileWith`
  guards**, so an image, workbook, PDF or newly-oversized file lands in the
  view it belongs in *today*; missing files are skipped and counted
  ("Restored 4 of 5 files" vs "Session restored"), never an error.
  The pure parts are `ConfigFile.planRestore` (the index arithmetic: counting
  the survivors ahead of the recorded active index re-addresses it against the
  shortened list, and lands on the next survivor when the active file is
  itself gone) and `EditorDoc.sessionForPersist`/`sessionShape`/
  `seedSessionPos` (the last clamps like `restoreRecentPos`, but addresses a
  named document rather than the active one, because a restore opens several
  files before any is looked at). `--restore` always restores and files named
  alongside it open *on top* and end up active; the config key applies only to
  a start with no arguments, and only `--restore` says so when there was
  nothing to restore.
- **Syntax highlighting (`Cmedit.Syntax`).** Per-language lexers return one
  `Tok` per character plus a trailing `HlState`, threaded across lines so
  multi-line constructs (block comments, Python docstrings, Markdown fences,
  HTML comments) stay correct from the top of the file. The renderer maps
  `Tok` → `Style` and lexes only the visible window each frame: line-start
  states come from `HlCache` (`edHlCache`/`docHlCache`), which is
  **self-validating** — it remembers the exact line `Seq` it was computed from
  and locates edits itself by pointer-equality-first comparison, so buffer
  edits never need to invalidate it explicitly (don't add invalidation calls).
  `App.renderNow` persists the refreshed cache via `Render.refreshHighlight`;
  after a single-line edit the old states are re-adopted once the recomputed
  state converges, so full-file coverage survives typing. Only the extensions
  in `langForPath` are lit; lexers still run per visible line, so keep them
  cheap. Lines over `maxHlLine` chars render unstyled (the lexer state passes
  through unchanged), so a megabyte-long minified line can't dominate a frame.

## Gotchas

- **Raw mode must call `setRawMode` (termios).** Relying on GHC's
  `hSetBuffering NoBuffering` alone leaves IXON/ICRNL/ISIG enabled, so Ctrl-S /
  Ctrl-Q (XON/XOFF) get silently eaten and Ctrl-C would kill the app. The app
  intentionally disables ISIG so Ctrl-C is a copy keystroke, not a signal.
- Terminal teardown (alt screen, mouse, cursor, termios restore) runs via
  `bracket`/`bracket_` in `runTui`, and a SIGTERM/SIGHUP handler throws to the
  main thread so cleanup still runs on external kill.
- Clipboard prefers external helpers (`xclip`/`wl-copy`/`xsel`/`pbcopy`) and
  falls back to an OSC 52 escape; copy returns a `CopyOutcome` telling the
  driver whether to emit the OSC 52 sequence.
