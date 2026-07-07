# CMeDit

*A play on **CMD** and "**C Me Edit**".*

A terminal text editor written **from first principles in Haskell**. It is a
cross between Microsoft's [Edit](https://github.com/microsoft/edit) (`msedit`)
and GNU **nano**: a modeless editor with a drop-down menu bar, real mouse
support and system-clipboard integration, plus nano-style on-screen shortcut
hints.

No TUI framework is used. Everything — raw-mode terminal control, the input
parser, the diff renderer, the menus and dialogs — is built directly on VT/ANSI
escape sequences and the POSIX `termios` API.

**Website:** <https://arrrg.org/cmedit/> ·
**Release white paper:** [*CMeDit: A Text Editor That Exists*](https://arrrg.org/cmedit/whitepaper.html)

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
  separators — in the spirit of `msedit`.
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
- **File explorer panel** (VS Code / Sublime style): open a folder
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
- **Dark, light and cherry-blossom themes**: `theme = light` in the config (or
  View ▸ Theme to pick one live, with preview) swaps the syntax palette for one readable on
  light terminal backgrounds; `theme = cherry-blossom` is a light pink 24-bit
  theme (after GymMaster's) that paints its own background on every cell, so it
  looks the same whatever the terminal's colours.
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
- **Workspace-wide Find in Files** (VS Code / Sublime style): **F4**
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
- **Undo / redo** with sensible coalescing of typing runs.
- **A config file and remembered recent files**: defaults (tab width,
  tabs/spaces, auto-indent, word wrap, line numbers, whitespace markers, plus
  opt-in save-time cleanups: `trim-trailing-whitespace` and `final-newline`,
  applied as an undoable edit when you save) load
  from `~/.config/cmedit/config` (`key = value` lines; command-line flags
  override it, and a bad line is reported on the status bar rather than
  ignored). The File menu lists recently-opened files, and re-opening one —
  same session or the next — puts the cursor back where you left it
  (`~/.config/cmedit/recent`).
- **Word wrap** (Alt+Z), **line numbers** (Alt+L) and **whitespace markers**
  (Alt+W), toggleable from the View menu.
- **Insert / overwrite** mode (Insert key), auto-indent, and tab/space
  indentation with Tab / Shift+Tab to indent and outdent selections.
- **A scrollbar**: a right-edge track and proportional thumb whenever the
  text, CSV table or search results overflow — click anywhere on it to jump,
  or drag the thumb.
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
| Search toggles (in the panel) | Alt+C case, Alt+W word, Alt+X regex, Alt+R replace-all |
| Go to line / Go to bracket | Ctrl+G / Ctrl+] |
| Go back / forward (history) | Alt+← / Alt+→ |
| Switch open files | Alt+. / Alt+, , Alt+1…9, or the Window menu (Alt+W) |
| Word wrap / Line numbers | Alt+Z / Alt+L |
| Sort CSV column (table view) | Alt+S |
| Menu | F10, or Alt+letter, or click |
| Move by word / to document ends | Ctrl+Left/Right / Ctrl+Home/End |
| Extend selection | hold Shift with any movement key |

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
| `Cmedit.Definition` | Go-to-definition: per-language definition shapes + picker dialog model |
| `Cmedit.QuickOpen` | Ctrl+P go-to-file picker: fuzzy matcher + ranked-list model |
| `Cmedit.Regex` | From-scratch linear-time regex engine — Thompson NFA / Pike VM (for regex search) |
| `Cmedit.Syntax` | Per-language lexers (one token per character) |
| `Cmedit.Csv` | CSV parse/serialise + spreadsheet table model |
| `Cmedit.Image` | From-scratch BMP/PNM/GIF/PNG/JPEG (baseline+progressive)/WebP (VP8L+VP8) decoders + image→cell scaler |
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
