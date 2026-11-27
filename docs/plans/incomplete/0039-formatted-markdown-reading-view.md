# 0039 — Formatted Markdown reading view (`Alt+T` toggle)

**Theme:** capability — rendered Markdown reading on top of the existing
`RtfDoc` layout, selection, search, and section-jump machinery
**Status:** proposal
**Estimated effort:** 3–4 days
**Risk:** low. Pure leaf parser (`Cmedit.Markdown`) plus minimal plumbing in
`EditorDoc`/`Editor`/`ConfigFile` following the exact RTF and SVG patterns;
the buffer stays authoritative and editable, so save/undo/journal behaviour
is untouched.

---

## 1. The observation

CMeDit already has a complete, battle-tested formatted document view in
`Cmedit.Rtf` (`RtfDoc`) that powers four formats today (`.rtf`, `.docx`,
`.odt`, `.epub`). Out of the box, `RtfDoc` already gives us:

- styled runs (`RtfFmt`: bold, italic, underline, strikethrough, colour,
  heading size);
- paragraph layout (`RtfPar`: alignment, left/first-line hanging indents,
  horizontal rules via `rpBreak`, inter-paragraph spacing via `rpSpace`,
  width-keyed relayout cache `rdCache`);
- laid-out text selection (mouse drag, double/triple-click, Shift+arrows,
  `Ctrl+A`, `Ctrl+C`);
- in-document `Ctrl+F` search with `F3`/`Shift+F3` (`rtfFindWith`);
- named section navigation (`rdSects`: `[` / `]` next/prev section, `Ctrl+G`
  Go to Section/Heading via `goToUnitOK`);
- live edit-counter staleness tracking (`rdSeq` / `rtfStale`), so edits made
  in raw mode or via workspace Replace All re-derive the view automatically.

Meanwhile, Markdown (`.md`, `.markdown`) is one of the most frequently opened
formats in a developer terminal editor (READMEs, design plans, changelogs,
docs), and currently opens only as raw syntax-highlighted markup.

## 2. Design

### 2.1 Reuse `RtfDoc` directly — no new view mode

Extend `RtfOrigin` in `Cmedit.Rtf` by one constructor (or tag `RtfFromBuffer`
with its markup dialect):

```haskell
data RtfOrigin
  = RtfFromBuffer            -- ^ Raw RTF markup in edBuffer; Alt+T toggles.
  | RtfFromMarkdown          -- ^ Raw Markdown in edBuffer; Alt+T toggles.
  | RtfFromContainer !Text   -- ^ Binary container (DOCX, EPUB, ODT); no buffer.
  deriving (Eq, Show)
```

Why `RtfFromMarkdown` instead of reusing `RtfFromBuffer` blindly:

- `refreshRtf` knows whether to re-parse with `Rtf.mkRtfDoc` or
  `Markdown.mkMdDoc` when `rtfStale (edEditSeq ed) rd` fires (e.g. after Undo,
  Revert, or workspace Replace All).
- `rtfDerived` returns `True` for both `RtfFromBuffer` and `RtfFromMarkdown`,
  so Save, Save All, Undo/Redo, journal write-behind, and `edModified` behave
  identically to RTF (the text buffer is the document; the formatted view is a
  pure read-only projection).
- Menu labels and status text say *"Markdown View"* / *"Raw Markdown — editable"*
  and *"Go to Heading…"* (instead of *"Go to Chapter…"* when `rdOrigin == RtfFromMarkdown`).

### 2.2 Default open behaviour: `markdown-view = source | preview`

Unlike `.rtf` (where raw `{\rtf1\ansi...` is unreadable noise), Markdown is
authored directly by hand almost as often as it is read. Follow `svg-view`'s
config pattern (`Cmedit.ConfigFile`):

- Add `markdown-view = source | preview` (`cfgMarkdownView :: !Bool` or
  `MdViewMode`), defaulting to **`source`** (or `preview` if preferred — make
  this explicit in Settings so either workflow is one toggle away; defaulting
  to `source` avoids surprising users who run `cmedit README.md` to make a
  quick edit, while `Alt+T` / **View ▸ Formatted View** toggles instantaneously
  and `markdown-view = preview` opts into reader-first opening).
- Even when `markdown-view = preview` is enabled, honour the existing
  `jumpingHere` guard in `EditorDoc.setLoaded`: opening a `.md` file from a
  workspace search hit (`F4`) or Go to Definition (`F12`) lands in raw source
  mode so the cursor and match highlight land on the exact line.

### 2.3 `Cmedit.Markdown` — a pure CommonMark-subset parser

A leaf module importing only `Cmedit.Rtf` (`RtfPar`, `RtfRun`, `RtfFmt`,
`RtfAlign`, `RtfDoc`), `Cmedit.Types` (`Color`), and `Cmedit.Width` (plus
optionally `Cmedit.Syntax` for fenced code blocks — see §2.4):

**Block-level constructs:**

| Markdown construct | Mapping to `RtfPar` |
|---|---|
| ATX (`#`–`######`) & Setext (`===`/`---`) headings | Bold run with `rfSize` (36 for H1, 30 for H2, 26 for H3, 24 for H4–H6) and distinct heading colour; recorded in `rdSects` as `(parIdx, headingText)` so `[`/`]` and `Ctrl+G` jump between headings |
| Paragraphs | `defaultPar { rpSpace = True }`, soft line breaks folded to spaces, trailing two-space / `\` hard breaks kept as `\n` |
| Thematic breaks (`---`, `***`, `___`) | Empty paragraph with `rpBreak = True` (renders as full-width `────` rule) |
| Unordered & ordered lists (`-`, `*`, `+`, `1.`), nested | Hanging indent (`rpLeft` + negative `rpFirst` in twips) with `• ` / `1. ` prefix; task list items (`- [ ]` / `- [x]`) rendered as `☐ ` / `☑ ` |
| Blockquotes (`>`, nested) | Indented with dimmed vertical bar prefix `│ ` (`rpLeft` + muted colour) |
| Fenced (```` ``` ```` / `~~~`) & indented code blocks | Indented block with preserved line breaks (`\n` inside runs), styled with code colour; optional language label line |
| GFM pipe tables (`\| a \| b \|`) | Column widths measured via `lineDisplayWidth`, alignment honoured (`:---`, `:---:`, `---:`), rendered as aligned monospaced rows with a thin rule `───┼───` under the header (matching `Docx`'s table layout discipline; falls back cleanly to raw text on overly wide tables) |

**Inline constructs:**

- Backtick code spans (`` `code` ``, ```` ``a ` b`` ````) — literal, styled
  with code colour, no nested emphasis parsing.
- Emphasis (`*em*`, `_em_`), strong (`**strong**`, `__strong__`), combined
  (`***both***`), strikethrough (`~~del~~`).
- Links `[label](url)` and autolinks `<https://...>` — render `label`
  underlined + link colour; if `url` is an `http(s)://` URL, either append
  `(url)` in dim text or attach it via an optional `rfLink :: !(Maybe Text)`
  on `RtfFmt` so OSC 8 hyperlinks and `Ctrl+Click` work directly on rendered
  Markdown links (small, high-value touch; `drawRtf` already feeds `urlLinksIn`
  to `expandLineCells`).
- Images `![alt](src)` — rendered cleanly as `[Image: alt]` (dimmed).
- Backslash escapes (`\*`, `\[`, etc.) and common HTML entities (`&amp;`,
  `&lt;`, `&gt;`, `&quot;`, `&nbsp;`).
- Inline HTML tags (`<br>`, `<!-- comments -->`, etc.) — `<br>` becomes `\n`,
  comments skipped, other tags passed through or stripped predictably without
  ever running an HTML parser.

**Bounds & safety:**

- Respect `Rtf.maxRtfChars` (or an explicit `maxMdChars` cap, e.g. 4 MiB),
  showing the same truncation notice on the status line if exceeded.
- Linear single-pass block + inline scanner; bound emphasis/bracket nesting
  depth (e.g. 32) so adversarial inputs like `[[[[[[...` or `******...` never
  go quadratic.

### 2.4 Optional Phase 2 — Syntax-highlighted fenced code blocks

Because `Cmedit.Syntax` already has `langForPath` (easily given a
`langForFenceName :: Text -> Maybe Lang` helper mapping `"haskell"`, `"py"`,
`"js"`, `"sql"`, `"bash"`, `"json"`, etc.) and `lexLineRuns`, fenced code
blocks with an info string can map `Tok` → `RtfFmt` colour directly during
`Markdown -> RtfPar` conversion. That keeps `Cmedit.Render.drawRtf` completely
unchanged while making code blocks in READMEs and plans look great.

### 2.5 Wiring across editor layers

Trace every consumer (`AGENTS.md`):

- **`Cmedit.EditorState`**: `isMarkdownPath :: FilePath -> Bool` (`.md`,
  `.markdown`, `.mdown`, `.mkd`), `isMarkdownFile :: Editor -> Bool`.
- **`Cmedit.EditorDoc`**:
  - `enterMarkdown`, `toggleMarkdown` (or generalise `toggleRtf` /
    `MAToggleRtf` → `MAToggleFormatted` so `Alt+T` handles both `.rtf` and
    `.md`).
  - `refreshRtf`: dispatch on `rdOrigin rd` when `rtfStale` is true.
  - `setLoaded`: auto-enter Markdown preview when `cfgMarkdownView` is on and
    `not jumpingHere`.
  - `applyPendingJump`: if jumping into an already-open `.md` or `.rtf` in
    formatted view, drop back to raw source first (matching the SVG fix noted
    earlier).
- **`Cmedit.Editor`**:
  - `relabelEntry`: `"Go to Heading…"` when `rdOrigin == RtfFromMarkdown`;
    `"Formatted View"` / `"Raw Markdown"` on the `View` menu `Alt+T` entry.
  - `handleRtfKey`: `[` / `]` jump between headings (already works whenever
    `rdSects` is non-empty!).
  - `rtfDisabledActions` already allows `Ctrl+F`, `F3`/`Shift+F3`, `Ctrl+C`,
    `Ctrl+A`, `Ctrl+S`, `Ctrl+Z`/`Ctrl+Y` and guards Cut/Paste/Delete/Replace.
- **`Cmedit.ConfigFile` / `EditorEdit.settingsSpec`**:
  - Add `markdown-view = source | preview` to parsing, defaults, config
    updater, Settings dialog, help text, and tests.
- **`Cmedit.App.convertPath`**:
  - Plain `.md` already outputs text; `--convert` keeps existing behaviour
    (or optionally renders clean plain text only if requested later).

## 3. Implementation steps

1. Extend `RtfOrigin` with `RtfFromMarkdown` (and optionally `rfLink :: Maybe Text`
   on `RtfFmt` + OSC 8 wiring in `Render.drawRtf`).
2. Implement `Cmedit.Markdown` (`parseMarkdown`, `mkMdDoc`) covering blocks,
   inlines, tables, headings (`rdSects`), and linear-time adversarial bounds.
3. Wire `Alt+T`, `enterMarkdown`, `refreshRtf`, `setLoaded`, `relabelEntry`,
   and `markdown-view` config option across `ConfigFile`, `EditorState`,
   `EditorEdit`, `EditorDoc`, `Editor`.
4. Add Phase 2 fenced-code-block highlighting via `Cmedit.Syntax.lexLineRuns`.
5. Update `cmedit.cabal`, `README.md`, `Manual.hs`, and add comprehensive
   tests.

## 4. Test plan

- **Pure parser tests (`test/Spec.hs` or `test/MarkdownSpec.hs`)**:
  - Headings (ATX + Setext) populate `rdSects` in order; `rtfGoToSection`
    lands on the right laid-out line before and after a width change.
  - Inline emphasis/strong/code/strikethrough/links/escapes, including tricky
    cases (`foo_bar_baz` must not italicise mid-word underscores; code spans
    suppress emphasis).
  - Nested lists, task lists, blockquotes, horizontal rules, and GFM tables
    (including ragged row counts and wide CJK cells).
  - Pathological inputs: 100k consecutive `*` or `[` characters parse in linear
    time; files over `maxMdChars` truncate cleanly with status note.
- **Editor integration tests (`test/Spec.hs`)**:
  - `Alt+T` toggles `.md` between source and preview, preserves edits and
    `edModified`, re-parses when edited or undone, and `Ctrl+S` always saves
    the original Markdown buffer byte-for-byte.
  - `Ctrl+F`, `F3`, `Ctrl+C`, `[`/`]`, `Ctrl+G` all work in preview; typing /
    Paste / Cut / Delete are blocked with a clear note pointing to `Alt+T`.
  - `markdown-view` config key round-trips through `parseConfigText`,
    `updateConfigText`, and Settings live preview + Cancel revert.
- **Verification**: `make test`, `make windows-check`, and a PTY smoke test
  opening `README.md` and toggling `Alt+T`.

## 5. Risks & mitigations

- **Accidental loss of cursor/scroll position across `Alt+T`**: toggling from
  source line `L` to preview and back is jarring if it resets to top. Mitigate
  by recording each `RtfPar`'s starting source line (an optional `Seq Int`
  sidecar or proportional ratio fallback) so `Alt+T` scrolls approximately to
  the same heading/paragraph in both directions.
- **Wide tables overflowing narrow terminals**: clamp column widths to the
  viewport width or let the row clip cleanly via `drawRtf`'s existing
  `rlPad + d < loTextWidth lo` clip, never misaligning wide glyphs (already
  enforced by `sanitizeGrid`).
- **Mid-word underscores (`snake_case_name`)**: follow CommonMark's
  intraword-emphasis rule for `_` (only open/close on non-alphanumeric
  boundaries) so technical docs and code references aren't mangled.

## 6. Out of scope

- Full CommonMark spec compliance for arbitrary embedded HTML blocks,
  reference-link edge cases, or LaTeX math rendering.
- Editing inside the formatted preview (read-only projection, matching RTF).
- Inline raster rendering of local `![](img.png)` images inside the text flow
  (possible future enhancement on kitty/sixel, separate plan).
