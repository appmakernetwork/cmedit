# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

CMeDit is a terminal text editor written **from first principles in Haskell** — a
cross between Microsoft Edit (`msedit`) and nano. There is deliberately **no TUI
framework** (no `brick`/`vty`): raw-mode terminal control, the input parser, the
renderer, menus and dialogs are all built directly on `termios` and ANSI/VT
escape sequences. Preserve this constraint when adding features.

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

- **The rightmost terminal column is reserved for the scrollbar** —
  `computeLayout` (text width), `csvViewportFor` and `searchRegion` all
  subtract 1 for it unconditionally (conditional reservation would make the
  wrap width oscillate with content height). `scrollBarInfo`/`scrollThumb`
  (EditorState) are shared by `Render.drawVScroll` and the hub's
  click/drag handlers (`scrollBarPress`/`scrollBarTo`, `edScrollDrag` swallows
  the mouse mid-drag like the sidebar drag). Under word wrap the bar uses
  buffer lines as a proxy for visual rows, deliberately (O(1) on huge files).
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
  Cells may contain newlines (Shift/Ctrl+Enter): a row's on-screen height is its
  tallest cell capped at `Csv.maxCellLines` (3), so rows have *variable height*.
  `Csv.rowHeight`/`csvRowLayout` (Render) and `Csv.rowAtLineOffset` (mouse) must
  stay in sync; `scrollTop` and `cellDisplay` handle vertical/horizontal
  scrolling within a tall or being-edited cell. Up/Down while editing move
  between a cell's lines (`editLineUp`/`editLineDown`) before crossing cells.
  `edFreezeHeader` (View ▸ Freeze Header Row) pins row 0 below the column header:
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
  re-anchored to its row. The modified flag
  (`Csv.isModified`, run per keystroke by `csvMod`) is exact at any table
  size: `sameGrid` compares against `csvSaved` with per-row/per-cell pointer
  shortcuts, so don't reintroduce a plain `==` on grids (or a big-table
  cutoff) — content-comparing shared rows made large tables freeze per key.

- **Image view mode (`Cmedit.Image`, `edImage`/`docImage`).** A third,
  read-only view mode, structured exactly like CSV but sharing none of its
  machinery (it does *not* go through the line buffer at all). `Cmedit.Image` is
  a self-contained, IO-free module that decodes BMP, Netpbm, GIF, PNG, JPEG
  (baseline **and** progressive) and WebP (lossless VP8L **and** lossy VP8,
  incl. the ALPH alpha chunk and an animation's first frame) from raw bytes
  using only boot libraries — the DEFLATE `inflate`, GIF LZW, JPEG
  Huffman/IDCT, VP8L prefix codes/transforms, and the VP8 boolean decoder /
  intra prediction / loop filter are all hand-rolled — into
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
- **Themes.** `Render.themeFor (resolvedTheme ed)` picks dark/light per
  frame; `Theme` carries `thTokens :: Tok -> Style` so the syntax palette
  differs per theme (light swaps washed-out brights for dark hues). Config key
  `theme = auto|dark|light` (default `auto`): `resolvedTheme` maps `auto`
  through `edDetectedDark` — the driver's OSC 11 background query, re-run on
  every focus-in so a system light/dark switch follows — and falls back to
  dark when the terminal never answers. Paint with `resolvedTheme`, never
  `cfgTheme`, or `auto` breaks. View ▸ Theme… opens a picker dialog
  (`DKTheme`/`mkTheme`, one button per `themeChoices` entry, focus starting
  on the current mode): moving the focus **live-previews** the theme —
  `resolvedTheme` consults the open picker's focused button, so Esc/Cancel
  restores simply because nothing was written — and Enter commits via
  `applyTheme` (per-session; the config key persists it). Note dark and
  light keep the terminal's default background, so a preview restyles
  chrome/tokens only, while cherry-blossom repaints every cell. The driver
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
