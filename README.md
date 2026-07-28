# CMeDit

*A play on **CMD** and "**C Me Edit**".*

A terminal text editor written **from first principles in Haskell**. It is
modeless — you type and the characters appear — and everything else is where you
would expect it: a drop-down menu bar across the top, a row of shortcut hints
along the bottom, real mouse support (click, drag-select, scroll, resize) and
the system clipboard behind Ctrl+C/X/V.

No TUI framework is used. Everything — raw-mode terminal control, the input
parser, the diff renderer, the menus and dialogs — is built directly on VT/ANSI
escape sequences and the POSIX `termios` API.

**Website:** <https://appmakernetwork.com/cmedit/> ·
**Release white paper:** [*CMeDit: A Text Editor That Exists*](https://appmakernetwork.com/cmedit/whitepaper.html)

```
  File   Edit   Find   View   Help
  1 The quick brown fox jumps over the lazy dog
    and then keeps running across the whole field
  2 short line two
  3 third line here

   ● demo.txt   [2/3]                  Ln 2, Col 1   INS  UTF-8  LF
 ^S Save  ^O Open  ^F Find  ^G Go To  ^Z Undo  ^X Cut  ^V Paste  ^Q Quit  F10 Menu
```

## Features

- **Drop-down menu bar** (File / Edit / Find / View / Help) with keyboard
  (F10, Alt+letter, arrows) and mouse navigation, accelerator hints, and
  separators.
- **Text selection** with Shift + any cursor key, Ctrl+A (select all), mouse
  drag, and double-click (word) / triple-click (line), shown with a highlight.
- **Real system clipboard**: Ctrl+C / Ctrl+X / Ctrl+V go through `xclip`,
  `wl-copy`/`wl-paste`, `xsel`, or `pbcopy`/`pbpaste`, with an OSC 52 escape
  fallback that works over SSH.
- **File-tree Open dialog**: Ctrl+O opens an interactive, lazily-loaded
  directory tree — expand/collapse folders, type-ahead jump, `..`/Backspace to
  go above the root, Enter to open, mouse and `.` to toggle hidden files.
- **Fuzzy quick open (Ctrl+P)**: a "Go to File" palette over the open folder
  (or the active file's directory). Type to fuzzy-filter — matches favour
  filenames, word boundaries and consecutive runs, and the matched letters are
  underlined — while a background walk streams the tree in (same pruning as
  the workspace search, so `node_modules` and friends never slow it down).
  With an empty query your recently-used files lead the list. Enter opens
  (restoring the remembered cursor position), Esc or a click off the box
  dismisses. Typing **`>`** turns it into a **command palette**
  (Ctrl+Shift+P opens it that way directly): fuzzy-search every menu command —
  context-pruned, with live labels like "View: Line Endings: LF" — and Enter
  runs it.
- **File explorer panel**: open a folder
  (`cmedit DIR`, **File ▸ Open Folder**, or **Ctrl+B**) to dock a persistent
  tree on the left. Click a file to open it, or navigate with the arrow keys
  and Enter; directories expand/collapse in place. Open files are highlighted,
  unsaved ones carry a `●`, and a `◆` flags a file that changed on disk since
  you opened it. The panel also **manages files**: `Ins` (or Ctrl+N) creates a
  file — end the name with `/` for a folder — in the selected directory (new
  files open ready to type), `F2` renames (open buffers follow the new path,
  even under a renamed folder), and `Del` deletes after a confirmation. **Drag the divider** to resize the panel, the **`«`** button
  (or dragging to the far left) collapses it to a single clickable strip, and
  the **`✕`** button closes the folder (after a confirmation). Ctrl+B toggles
  focus between the panel and the editor. Files show their size when large, and
  ones too big to edit are dimmed.
- **Safe file opening**: binary files are detected and refused with a clear
  message instead of being decoded into junk (which used to hang on large
  blobs), files above a size cap are refused up front, and legitimately large
  text/image files load on a background thread with a **loading spinner** so the
  UI never freezes.
- **Multiple open files ("windows")**: pass several on the command line, open
  more with Ctrl+O, or start new buffers with Ctrl+N (each in its own window).
  Switch with the **Window** menu (Alt+W), Alt+1…9, or Alt+. / Alt+, . Quitting
  with a few unsaved files prompts for each; with a large batch (more than 8) it
  asks once — **Save All**, **Discard All**, or **Cancel**.
- **CSV table mode**: `.csv`/`.tsv` files open in a navigable spreadsheet grid
  (column letters, row numbers, cell editing, multi-line cells, insert/delete
  rows & columns, and undo — with the header row frozen while you scroll, since
  spreadsheets almost always have one; View ▸ Freeze Header Row or
  `freeze-header = off` in the config turns that off). Select a rectangle of cells
  (Shift+navigation or drag) to copy/cut/delete it; copy yields a mini-CSV, and
  paste fills, spreads, or overwrites a matching block. Saving — or toggling
  back to text with Alt+T or the View menu — writes proper RFC-4180 CSV with
  quoting, preserving the file's line ending. **Alt+S sorts by the current
  column** — numeric-aware, case-insensitive for text, empties last; press it
  again to flip descending. The frozen header row stays pinned, the cursor
  follows its row, and one undo restores the previous order.
- **Formatted view for RTF**: `.rtf` files open as the document rather than as
  `{\rtf1\ansi...`. Bold, italic, underline, strike-through and text colour map
  onto the terminal's own attributes; paragraphs wrap to the window with their
  real alignment, indents and hanging bullets; `\'hh` and `\uN` escapes, curly
  quotes and dashes decode properly, and font tables, style sheets, embedded
  pictures and every other `{\*\...}` destination are skipped rather than shown.
  **Alt+T** toggles to the raw markup, which edits and saves like any other text
  file (with the control words syntax-highlighted). You can **select text with
  the mouse** (double-click for a word, triple for a line, Shift+click to
  extend, or Shift+arrows) and **Ctrl+C** copies it, so quoting a paragraph out
  of a document you are reading is one gesture; **Ctrl+A** takes the whole
  thing and **Esc** clears. The formatted view is
  read-only by design: it is a projection of the buffer and is never written
  back, so the parts of a document it does not model cannot be lost on save.
- **PDF reading view**: opening a `.pdf` (detected by magic bytes, so the
  extension does not matter) shows the *document* — the text, reflowed to the
  window, with headings and bold/italic runs picked out — rather than refusing
  it as binary. Columns are detected and read in order, paragraphs are put back
  together and re-wrapped to your terminal's width (hyphens introduced by the
  original line breaks are undone), tables and other positioned lines keep their
  columns, and `[` / `]` turn the page, with **Ctrl+G** jumping straight to a
  page number. It reads compressed streams and object streams, simple and CID
  fonts, `/ToUnicode` maps, the standard-14 fonts' built-in metrics and Type3
  fonts, all from scratch on GHC's boot libraries — no Poppler, no rasteriser.
  You can **select text with the mouse** (double-click for a word, triple for a
  line, or Shift+arrows) and **Ctrl+C** copies it — looking something up and
  pasting the answer elsewhere is most of what a PDF gets opened for. **Ctrl+F**
  searches the document you are reading, with F3 / Shift+F3 stepping through the
  hits and every match highlighted while the dialog is open; the hit becomes the
  selection, so finding something and copying it are one gesture.
  The view is read-only: there is no serialiser back to PDF and there could not
  be. Encrypted files say so rather than showing noise.
- **Paged view for huge files**: a file too large to load as an editable buffer
  (over 100 MB) used to be refused outright. It now opens in a read-only paged
  viewer whose memory does not depend on the file's size — a 281 MB, 4-million
  line log, or a 120 MB file that is one single line, both sit at around 30 MB
  resident. One streaming pass indexes a byte offset every thousand lines, and
  only a window of lines around the viewport is ever decoded, so scrolling,
  Ctrl+Home/End and **Go To Line** (jump straight to line 2 000 000 of a log)
  are all instant. Syntax highlighting still applies per visible line. Turn it
  off with `paged-view = off` to get the old refusal back.
- **Image view mode**: opening a `.bmp`, `.gif`, `.jpg`/`.jpeg`, `.png`,
  `.webp`, or `.ppm`/`.pgm`/`.pbm` (detected by magic bytes) shows a read-only,
  scaled rendering of the picture — useful for glancing at images over SSH where
  a real viewer isn't an option. Drawn with Unicode half-block glyphs in 24-bit
  colour (two pixels per cell); press **`a`** to switch to a monochrome ASCII
  ramp. **Drag a rectangle** to zoom into that region (still aspect-fit); a
  single click or **Esc** snaps back to the whole image. The
  image re-scales to fit on resize, and an undecodable file reports a clear error
  rather than opening as binary. Every decoder (BMP, GIF LZW, JPEG — both
  baseline and progressive — with hand-rolled Huffman+IDCT, PNG with a
  from-scratch `inflate`, WebP — both lossless VP8L and lossy VP8, boolean
  arithmetic decoder, loop filter and all — and Netpbm) is written from first
  principles using only GHC boot libraries.
- **Archive listings**: opening a `.zip` — or anything built on it, a `.jar`,
  `.whl`, `.apk` (detected by magic bytes, not by name) —
  shows its contents as a read-only file tree with sizes, compression savings
  and timestamps, instead of refusing it as binary. Only the archive's table of
  contents is read, never its members, so this costs two short reads however
  large the file is, and encrypted archives list fine — names and sizes are not
  the part that is encrypted, and members that are get flagged. The listing is
  an ordinary read-only buffer, so Find, word wrap, Go To Line and copying all
  work on it as usual; the archive itself can never be written to.
- **Office and e-book reading views**: a `.docx`, `.xlsx`, `.odt`, `.ods` or
  `.epub` is a ZIP full of XML, so instead of stopping at the listing cmedit
  reads it.
  A **Word document** shows as a document — headings, bold/italic/underline,
  colour, alignment, indents, bullets and tables on tab stops, reflowed to your
  window. A **workbook** opens in the spreadsheet grid, with `[` / `]` turning
  the sheets and **Ctrl+G** jumping to one by number; gaps in a sheet are real
  and are shown, shared strings and inline strings are resolved, and formulas
  show the value Excel last calculated. Where a workbook was written by a
  *library* rather than by Excel — `openpyxl`, `xlsxwriter`, `pandas.to_excel`
  — there is no cached value to show, and those cells used to come up blank;
  cmedit now **evaluates the formula itself**: around fifty functions (`SUM`,
  `AVERAGE`, `IF`, `COUNT`/`COUNTIF`, `MIN`/`MAX`, `ROUND`, `SUMIF`,
  `VLOOKUP`, the text and maths families), the full operator set, ranges
  including whole columns, cross-sheet references and chains of formulas. A
  value the file already gives is **never** recomputed, so nothing cmedit works
  out can contradict the program that wrote the file; a formula it cannot parse
  or has no function for is left blank and counted, and the status bar reports
  both totals. Number formats are still not applied, so a date reads as its
  stored serial number, and the status bar says so. **OpenDocument** files —
  `.odt` and `.ods`, what LibreOffice writes — go through the same two views,
  with one difference in their favour: an `.ods` stores each cell's *displayed*
  text as well as its value, so dates and currencies read as
  `15/01/2024` and `$1,234.50` rather than as the numbers underneath them.
  (`.odp` and `.odg` are positioned shapes rather than documents, and fall back
  to the listing.) **Ctrl+Shift+S** exports
  the sheet you are looking at as a CSV file (and, in the document views, the
  document as plain text) — an export, not a Save As: it writes a copy and
  leaves the workbook itself open and untouched. An **e-book** shows its chapters in
  reading order, with `[` / `]` turning chapters and **Ctrl+G** going to one by
  number; the container, package document and spine are followed properly, so
  the chapters are the ones the book actually orders. The document views
  **select and copy** exactly as the RTF and PDF ones do — drag, double-click a
  word, triple-click a line, Shift+arrows, Ctrl+A, then Ctrl+C — and a
  workbook copies a rectangular block of cells the way a CSV file does.
  Everything the readers do
  not model is skipped rather than mis-rendered, all three are **read-only**
  (there is no serialiser back to any of these formats, and could not be), and
  **Alt+T** shows the archive listing underneath — with Alt+T again coming back.
  Any failure at any stage falls back to that listing with a note saying why, so
  a damaged file still opens. Only the members a format needs are ever
  decompressed, so a 1 GB `.docx` full of photographs costs the members its
  text lives in and no more.
- **Six themes plus auto**: `theme = light-terminal` in the config (or
  View ▸ Theme to pick one live, with preview) swaps the syntax palette for one readable on
  light terminal backgrounds; dark-terminal and light-terminal keep your
  terminal's own background, while the other four are 24-bit themes that
  paint their own background on every cell, so they look the same whatever
  the terminal's colours: `cherry-blossom` is light pink (after GymMaster's),
  `flashbang` is a blinding pure white, `midnight` is a deep navy, and
  `graphite` is a neutral near-black with orchid keywords and gold function
  names.
- **Syntax highlighting** for SQL (PostgreSQL), Python, JavaScript/TypeScript
  (`.js/.mjs/.jsx/.ts/.tsx`), CSS/SCSS/LESS, HTML/XML, FreeMarker (`.ftl`),
  Jinja, shell, Markdown, JSON, YAML, TOML, INI/conf and CSV — including
  multi-line constructs (PG dollar-quoted bodies, Python docstrings, JS
  template literals, fenced code, HTML/Jinja comments).
- **Find / Replace / Go to Line** dialogs, with match-case and whole-word
  options, find-next (F3) / find-previous (Shift+F3), and replace-all. While
  the dialog is open every match in view is highlighted and a live count
  ("12 matches") updates as you type; F3 reports "Match 3 of 17" on the
  status bar. Up/Down in the fields recall **previous search/replace terms**
  (kept across sessions in `~/.config/cmedit/history`), and a seeded term is
  replaced by the first character you type — press an arrow first to edit it
  in place instead.
- **Workspace-wide Find in Files**: **F4**
  opens a search panel over the whole open folder; **F6** adds the
  replace field. Results are grouped by file with match counts and snippet lines
  you can select with the keyboard or **click** to jump straight to that spot in
  the file. Toggle match-case, whole-word and **regular-expression** search, and
  narrow the scope with include / exclude globs (e.g. `*.hs`, `!dist/**`).
  Replace across the workspace **stages** its changes: every affected file is
  opened as an unsaved tab (with regex `$1` group substitution), the explorer
  expands to reveal the changed files (marked `●`), and you review and persist
  them with Ctrl+S or **File ▸ Save All** — nothing is written to disk until you
  do (very large replaces fall back to writing straight to disk). The search runs
  on a background thread with a spinner and stays fast on trees of thousands of
  files
  — it prunes `.git`/`node_modules`/build dirs, skips binaries and huge files,
  and can be superseded instantly by the next search.
- **Search inside documents (Alt+D)**: the same panel can look inside **PDFs,
  Word and OpenDocument files, workbooks and e-books**, decoding each one
  through the very reader that would display it — so anything found is
  something you can then go and look at. Off by default, because decoding one
  document costs roughly what grepping a hundred source files costs (measured:
  0.22 s → 1.1 s once forty PDFs join a source tree).
  Hits are addressed by something intrinsic to the document rather than by a
  line number — these views reflow to your window, so a stored row would point
  somewhere else in a wider one: a PDF says `p.7`, an e-book `ch.3`, a Word or
  OpenDocument file a paragraph, a workbook the cell (`B4`). Enter opens the
  document at that unit and highlights the term there. A workbook is searched
  cell by cell, so a phrase spanning two columns is deliberately not a match.
  **Replace never touches a document**: none of these formats can be written
  back, so they are excluded from every replace path and the panel reports how
  many it left alone rather than skipping them silently.
- **Line operations**: duplicate the current line or selected lines (Ctrl+D,
  or Shift+Alt+↑/↓ to copy up/down), move them up/down with Alt+↑/↓ (held moves
  undo as one step), delete the line (Ctrl+Shift+K), and join lines (Alt+J,
  collapsing the seam whitespace to a single space) — all in the Edit menu.
- **Toggle comment (Ctrl+/)**: comments or uncomments the current line or
  selection using the file type's own syntax (`#`, `//`, `--`, …; HTML/CSS
  wrap the span in a block comment), aligned at the block's indentation with
  blank lines skipped.
- **Bracket matching**: the `()[]{}` pair at the cursor is highlighted, and
  Ctrl+] (Find ▸ Go to Bracket) jumps between the two — bounded scanning, so
  an unmatched bracket in a huge file never stalls a repaint.
- **Word completion (Ctrl+Space)**: completes the identifier at the cursor
  from the words of every open buffer, nearest occurrences first — no language
  servers, works offline. Type to narrow, Tab/Enter accepts, a single
  candidate inserts immediately, and the whole thing is one undo step.
- **External-linter integration**: cmedit auto-detects `ruff`, `flake8`,
  `eslint`, `stylelint`, `pyright` and `shellcheck` on your `PATH` (and
  `eslint`/`stylelint` in the workspace's `node_modules/.bin`, or run through
  `.pnp.cjs` in a Yarn Plug'n'Play workspace), then lints the
  active file automatically about half a second after you stop typing —
  buffer content goes to the tool over stdin, so unsaved changes are checked,
  with the workspace root as the working directory so project configs like
  `pyproject.toml` or `eslint.config.mjs` apply. Saving re-lints immediately.
  `pyright` is save-time-only and off by default (it's slow), and `flake8` is
  skipped when `ruff` is installed and enabled. Problems show as colored
  squiggly underlines (curly where supported), tinted line numbers, and an
  "nE mW" count on the status bar (click to jump); resting the cursor on one
  shows its message. **F8** (or Find ▸ Next Problem) cycles through them.
  File ▸ Settings has a **Linting** section with a master switch and a row
  per linter showing install status and, if missing, the install command.
- **Navigation history (Alt+←/→)**: every long-distance jump — Go to Line,
  find, Go to Definition, a search result, a bracket jump, Ctrl+Home/End,
  opening or switching files — records where you were; Alt+Left walks back
  through those locations (across files) and Alt+Right forward again, like a
  browser. Also in the Find menu (Go Back / Go Forward).
- **Line endings and BOM are switchable**: View ▸ Line Endings flips LF ⇄ CRLF
  and View ▸ UTF-8 BOM toggles the byte-order mark (both written on the next
  save, and both keep the file marked modified until then). The **status bar
  is clickable** too: `Ln, Col` opens Go to Line, `INS/OVR` toggles overwrite,
  and the `UTF-8`/`LF` cells switch encoding/line endings directly.
- **Crash recovery**: unsaved changes are journalled every couple of seconds to
  `~/.cache/cmedit/journal` (a directory created mode `0700`; a very large file
  a little less often, and always on a background thread), so an SSH drop, a
  closed terminal or an OOM kill does not take the last hour with it. The next
  time cmedit starts it lists what it found and offers to **Recover**,
  **Discard** or **Keep for later**; recovered files come back as unsaved
  buffers, so nothing is written over your file until you save it. A journal is
  deleted as soon as its document is saved or closed, and on a clean exit. The
  journal holds file *content* in your cache directory — and, so a restored
  session can offer your files back as you left them (below), a clean exit also
  writes a copy of every open document (up to 4 MB each) to
  `~/.cache/cmedit/snapshots`. One switch covers both: `journal = off` in the
  config (or the row turned off in File ▸ Settings) and cmedit caches no file
  content anywhere — no journals while you work, no snapshots when you quit.
  That is the setting to reach for if you edit secrets.
- **Session restore, per workspace folder**: `cmedit --restore` reopens the
  files you had open in *this* folder last time — in the same order, with the
  same cursor positions, the same workspace folder and the same file in front.
  Each folder gets its own session file under `~/.config/cmedit/sessions`, so
  alternating between two projects in two terminals no longer has the second
  one overwrite the first one's list; `--restore` takes the session for the
  directory you start in, or — if that folder has none — the most recently
  written session of any folder, with the status line naming the folder it
  came from (a folderless session has none to name).
  A run with no folder open keeps using `~/.config/cmedit/session`, and older
  session files still restore. Put `restore-session = true` in the config (or
  turn on "Restore session on start" in File ▸ Settings) to have a bare
  `cmedit` do it; naming files on the command line still just opens those, and
  files named alongside `--restore` open on top of the restored session.
  Session files hold paths, cursors and the times the files were last seen —
  never file content. The **File menu also lists your last few sessions**
  ("website (6 files)", up to four): choosing one restores it into the running
  editor, adding its folder and files to what you already have open — an
  already-open file is switched to, not duplicated — and closing nothing.
- **Files that moved while you were away**: if a restored file changed on disk
  since the session ended, a **"Files Changed Since This Session"** dialog
  lists them (◆) and offers **Latest on Disk** (the default; Esc means it too,
  and it is a no-op since the files are already open at that version) or
  **As You Left Them**, which brings back the contents from the session's clean
  exit as *unsaved*, modified buffers marked ◆ — nothing is written over the
  newer file until you save it yourself. One answer covers the whole list.
  Files over 4 MB, and sessions that ended in a crash (or ran with
  `journal = off`), have no saved copy: those are annotated in the list, stay
  at their newest version, and if none of the listed files has a copy the
  dialog just reports what changed with a single button. Files deleted since
  the session are skipped with a note ("Restored 4 of 5 files"). Restore
  composes with crash recovery: restore runs first and the journal is applied
  last, so unsaved changes from a crash come back *in* the restored files and
  always have the final word.
- **Undo / redo** with sensible coalescing of typing runs.
- **A settings page and config file**: File ▸ Settings (`Ctrl+,`) lists every
  option — tab width, indent style, auto-indent, theme, word wrap, line
  numbers, whitespace markers, the save-time cleanups, the CSV header
  freeze, the crash-recovery journal and session restore — as arrow-key value pickers grouped by topic, with a one-line hint
  for the highlighted row. Changes apply **live** behind the dialog; Save
  writes them back to `~/.config/cmedit/config` surgically (your comments and
  unknown lines survive), and Cancel/Esc reverts everything. The same file can
  be edited by hand (`key = value` lines; command-line flags override it, and
  a bad line is reported on the status bar rather than ignored). The File menu lists recently-opened files, and re-opening one —
  same session or the next — puts the cursor back where you left it
  (`~/.config/cmedit/recent`).
- **Word wrap** (Alt+Z), **line numbers** (Alt+L) and **whitespace markers**
  (Alt+W), toggleable from the View menu.
- **Insert / overwrite** mode (Insert key), auto-indent, and tab/space
  indentation with Tab / Shift+Tab to indent and outdent selections.
- **Scrollbars**: a right-edge vertical track and proportional thumb
  whenever the text, CSV table or search results overflow, and a bottom-edge
  horizontal one for text/CSV that overflows sideways — click anywhere on
  either to jump, or drag the thumb. Both can be hidden from Settings
  (File ▸ Settings…).
- **Resizing**: handles SIGWINCH and adapts to any terminal size; horizontal
  scrolling for long lines when word wrap is off — including with the mouse
  (Shift+wheel, or a horizontal wheel/touchpad): it pans long lines, steps
  across CSV columns, and slides the workspace-search result snippets.
- **Unicode**: UTF-8 throughout, with a compact `wcwidth` so wide (CJK/emoji)
  and zero-width (combining) characters line up; tab stops are honoured.
- **Performant**: a persistent `Seq Text` buffer (so undo snapshots share
  structure), and a double-buffered diff renderer that only repaints rows that
  changed and flushes each frame in a single write. Opens multi-megabyte files
  instantly.

- **Terminal-native, with graceful fallback everywhere**: at startup cmedit
  probes the terminal — device attributes, `XTVERSION`, the background colour
  (OSC 11), cell pixel geometry, a behavioural `REP` probe, and a kitty
  graphics query — and upgrades itself feature by feature. On terminals that
  answer: frames are committed atomically via **synchronized output** (no
  tearing, mode 2026), vertical scrolling uses **hardware scroll regions**
  (a one-line scroll costs a few bytes instead of a band repaint — including
  over SSH), repeated cells compress with **REP**, the image view renders at
  **true pixel resolution** via the **kitty graphics protocol or sixel**
  (aspect-corrected using the terminal's real cell size), the bracket-match
  highlight becomes a **curly underline**, `theme = auto` follows the
  terminal's **light/dark background** (re-checked on focus, so a system
  theme switch follows you), the mouse **pointer shape** tracks what it's
  over, the cursor colour matches the theme, the window **title is
  pushed/popped** instead of clobbered, and finishing a long search or load
  while the terminal is unfocused posts a **desktop notification** (OSC 9).
  URLs in your text and the file names in the explorer, search results and
  status bar are **real hyperlinks** (OSC 8) — hover underlines them and
  Ctrl+Click opens the target — in any terminal that supports links, and
  invisible everywhere else.
  A terminal that answers none of the probes simply gets the portable
  escape stream cmedit always emitted — every upgrade is opt-in by evidence.

## Building

Everything depends only on libraries that ship with GHC, so no network or
package index is required.

```sh
make          # builds the optimized ./cmedit binary (ghc --make)
make test     # builds and runs the test suite
make run      # build and launch
```

### Windows

CMeDit has a native Windows port: the same codebase with a Windows
implementation of the platform layer (`platform/windows/Cmedit/Term.hs`,
hand-rolled kernel32 FFI — no extra packages). On Windows, with GHC (via
[ghcup](https://www.haskell.org/ghcup/)) and `make` (via MSYS2):

```sh
make windows        # builds cmedit.exe
```

It needs a console that speaks VT — Windows 10 1809 or later; **Windows
Terminal is recommended** (and is the Windows 11 default). Legacy conhost
gets the portable fallback path like any other minimal terminal. On any
other OS, `make windows-check` typechecks the whole program against the
Windows platform layer without linking, which is how the port is kept
honest from Linux. (WSL and `ssh` from Windows Terminal run the POSIX
build unchanged, and are still great ways to use CMeDit from Windows.)

`cabal build` / `cabal run cmedit` also work in environments whose Hackage index
cache has been built. On a fully offline machine prefer `make`, which drives
`ghc --make` directly and needs no index.

Requirements: GHC 9.0+ and a clipboard helper (`xclip`, `wl-copy`, `xsel` or
`pbcopy`) for full clipboard integration (OSC 52 is used otherwise).

## Usage

```
cmedit [OPTIONS] [FILE|DIR...]

  A DIR argument opens as a workspace folder in the explorer panel;
  `cmedit .` opens the current directory that way.

  -h, --help              Show help and exit.
  -v, --version           Show version and exit.
  -t, --tab-width N       Tab width in columns (default 4).
      --tabs / --spaces   Indent with tabs / spaces (default tabs).
      --line-numbers / --no-line-numbers   (default: hidden)
      --no-auto-indent
      --readonly
      --restore           Restore this folder's session (or the newest one).
```

Run `cmedit --help` for the full key map and the list of config-file keys
(defaults are read from `~/.config/cmedit/config`; flags override them).

## Key bindings

| Action | Keys |
| --- | --- |
| New / Open / Save / Save As / Save All | Ctrl+N / Ctrl+O / Ctrl+S / Ctrl+Shift+S / File ▸ Save All |
| Go to file (fuzzy) | Ctrl+P |
| Command palette | Ctrl+Shift+P, or `>` in Ctrl+P |
| Open folder / Toggle explorer | File ▸ Open Folder / Ctrl+B |
| Close file / Quit | Ctrl+W / Ctrl+Q |
| Undo / Redo | Ctrl+Z / Ctrl+Y |
| Cut / Copy / Paste | Ctrl+X / Ctrl+C / Ctrl+V |
| Duplicate line / copy up/down | Ctrl+D / Shift+Alt+↑/↓ |
| Move line up / down | Alt+↑ / Alt+↓ |
| Delete line / Join lines | Ctrl+Shift+K / Alt+J |
| Toggle comment | Ctrl+/ |
| Word completion | Ctrl+Space |
| Select all | Ctrl+A |
| Find / Find next / prev / Replace | Ctrl+F / F3 / Shift+F3 / Ctrl+R |
| Find in Files / Replace in Files | F4 / F6 |
| Search inside documents (in the search panel) | Alt+D |
| Search toggles (in the panel) | Alt+C case, Alt+W word, Alt+X regex, Alt+R replace-all |
| Go to line (page in a PDF, chapter in an e-book, sheet in a workbook) / Go to bracket | Ctrl+G / Ctrl+] |
| Go back / forward (history) | Alt+← / Alt+→ |
| Switch open files | Alt+. / Alt+, , Alt+1…9, or the Window menu (Alt+W) |
| Word wrap / Line numbers | Alt+Z / Alt+L |
| Table view (CSV) / formatted view (RTF) / archive contents (DOCX, XLSX, EPUB) | Alt+T |
| Previous / next page (PDF), chapter (EPUB), sheet (XLSX) | `[` / `]` |
| Select / copy in a reading view (PDF, RTF, DOCX, EPUB) | drag, double/triple-click, Shift+arrows / Ctrl+C |
| Export a sheet as CSV / a reading view as text | Ctrl+Shift+S |
| Sort CSV column (table view) | Alt+S |
| Find in the table view (searches cells, moves the cell cursor) | Ctrl+F / F3 / Shift+F3 |
| Menu | F10, or Alt+letter, or click |
| Move by word / to document ends | Ctrl+Left/Right / Ctrl+Home/End |
| Extend selection | hold Shift with any movement key |

## Converting from the command line

Every reading view turns an awkward format into text a terminal can show, so
the same work makes cmedit a converter — and a converter is what you want when
the terminal is not there:

```sh
cmedit report.docx > report.txt      # .pdf .odt .epub .rtf too
cmedit book.xlsx   > book.csv        # .ods too; --sheet N picks another sheet
cmedit paper.pdf | grep -i abstract
```

No flag is needed: the editor draws on stdout, so a redirected stdout cannot
mean "open the editor" and can only mean "give me the text". The converted
content goes to **stdout** and one line describing it to **stderr** — which is
the only arrangement that survives a redirect:

```
report.docx: DOCX, 96 paragraphs → 8032 characters
book.xlsx: workbook, sheet 1 “Sales” → 42 rows, 5 columns as CSV  (of 3; --sheet N for the others)
```

`--convert` forces it when stdout *is* a terminal. An image (no text) or a file
too large to load (already plain text) says so and exits non-zero.

## Architecture

The editor core is **pure**: `update :: Key -> Editor -> (Editor, [Effect])`.
Anything touching the outside world (clipboard, files, quitting) is returned as
an `Effect` for the thin IO driver to perform. Rendering is pure too —
`renderEditor :: Editor -> Screen` builds a grid of styled cells, and
`renderFrame` diffs it against the previously displayed frame. This keeps the
logic unit-testable without a terminal.

| Module | Responsibility |
| --- | --- |
| `Cmedit.Types` | Shared types: keys, mouse, colours, styles, cells |
| `Cmedit.Link` | OSC 8 hyperlink targets: URL recognition in text, `file://` URIs, link ids |
| `Cmedit.ConfigFile` | The `~/.config/cmedit` config file + persisted recent-files list |
| `Cmedit.Lint` | External-linter catalogue (ruff/flake8/eslint/stylelint/pyright/shellcheck): invocation + output parsing into diagnostics |
| `Cmedit.Term` | The platform layer (`platform/{posix,windows}`, one module two implementations): raw mode, window size, resize/terminate wiring, walker stat |
| `Cmedit.Ansi` | ANSI/VT escape-sequence builders (incl. queries, sync output, scroll regions) |
| `Cmedit.Caps` | Terminal capability model: probe-reply folding, fingerprints, colour parsing |
| `Cmedit.Gfx` | Kitty-graphics and sixel encoders (base64, quantiser, RLE) for the pixel image view |
| `Cmedit.Input` | Bytes → key events (escape sequences, mouse, paste) |
| `Cmedit.Width` | `wcwidth`, column↔cell mapping, word wrapping |
| `Cmedit.Clipboard` | System clipboard via helpers + OSC 52, base64 |
| `Cmedit.TextBuffer` | `Seq Text` buffer, edits, movement, file I/O |
| `Cmedit.EditorState` | The editor model: state records, effects, layout, small queries |
| `Cmedit.EditorEdit` | Core editing: movement, undo, primitives, line ops, file properties |
| `Cmedit.EditorDoc` | Document lifecycle: zipper, view modes, recents, nav history, quick open |
| `Cmedit.EditorFind` | Find/replace engine, live match feedback, workspace search model |
| `Cmedit.Editor` | The dispatch hub: `update`, key/mouse handlers, menus (re-exports the rest) |
| `Cmedit.Menu` / `Cmedit.Dialog` | Menu and dialog data + pure helpers |
| `Cmedit.Browser` | Lazy file-tree model for the Open dialog |
| `Cmedit.Search` | Workspace find/replace model: matching, globs, result tree |
| `Cmedit.DocText` | Reading-view documents flattened to searchable text, each line addressed by page/chapter/paragraph/cell |
| `Cmedit.Definition` | Go-to-definition: per-language definition shapes + picker dialog model |
| `Cmedit.QuickOpen` | Ctrl+P go-to-file picker: fuzzy matcher + ranked-list model |
| `Cmedit.Regex` | From-scratch linear-time regex engine — Thompson NFA / Pike VM (for regex search) |
| `Cmedit.Syntax` | Per-language lexers (one token per character) |
| `Cmedit.Csv` | CSV parse/serialise + spreadsheet table model |
| `Cmedit.Inflate` | Hand-rolled DEFLATE (RFC 1951), shared by PNG and PDF's compressed streams |
| `Cmedit.Image` | From-scratch BMP/PNM/GIF/PNG/JPEG (baseline+progressive)/WebP (VP8L+VP8) decoders + image→cell scaler |
| `Cmedit.Pdf` | From-scratch PDF reader: object scan, filters, content-stream interpreter, fonts, and page→document reconstruction |
| `Cmedit.Pager` | Read-only paged view of files too large to load: sparse line index + windowed reads |
| `Cmedit.Rtf` | RTF parser + document layout for the read-only formatted view of `.rtf` files |
| `Cmedit.Zip` | ZIP central-directory reader, single-member extraction, and the file-tree listing shown for an archive |
| `Cmedit.Xml` | Small non-validating XML pull parser (entities, CDATA, local-name matching), shared by the three container readers |
| `Cmedit.Docx` | `word/document.xml` → the formatted view's paragraph model |
| `Cmedit.Xlsx` | Workbook, worksheets and shared strings → the read-only spreadsheet grid |
| `Cmedit.Formula` | Spreadsheet formula parser and evaluator, for the cells a workbook left uncalculated |
| `Cmedit.Epub` | Container/package/spine plumbing + a minimal XHTML → paragraph mapper |
| `Cmedit.Odf` | OpenDocument `.odt`/`.ods`: the automatic-style table, then the same two targets |
| `Cmedit.About` | The About box's animated wordmark (pure frame→cells generation) |
| `Cmedit.Render` | Model → cell grid, and the diff to escape codes |
| `Cmedit.App` | IO driver: setup/teardown, reader thread, event loop |

## Tests

`make test` runs a hand-rolled suite (no external framework needed offline)
covering the text buffer, width/column mapping, the input parser, and the
editor update function.

## License

Copyright © 2026 Benjamin Marsh.

CMeDit is free software, licensed under the **GNU General Public License,
version 3** (GPL-3.0-only). You may use, study, share and modify it under those
terms; distributed derivative works must remain under the GPL. It comes with no
warranty. See the [`LICENSE`](LICENSE) file for the full text.

## Contributing

Contributions are welcome — see [`CONTRIBUTING.md`](CONTRIBUTING.md). Merging a
contribution requires agreeing to the [Contributor License Agreement](CLA.md),
which is handled automatically by a bot on your first pull request.
