# 0003 — The frame is the whole cost: bound per-line work to the visible window

**Theme:** interactive latency and allocation churn under sustained editing
**Status:** 🟡 **PARTIALLY LANDED** — Stage 1 shipped 2026-07-26. Stage 3 was
implemented, measured, and **reverted**: it regressed large files 3.7×. Stages
2 and 4 untouched.
**Estimated effort remaining:** 1–2 days
**Risk:** medium — touches the renderer's hot path

## Stage 1: shipped ✅

`Link.urlSpansIn` scans only the visible character window (widened outward to
whitespace, so a URL crossing either edge is still seen whole), and
`Render.urlLinksIn` uses it for lines over 2 000 characters — shorter lines are
still scanned entire, which is cheaper than computing a window and keeps the
common case bit-identical.

| Per keystroke, full driver cycle | Before | After |
|---|---|---|
| `.txt`, 3 000-char lines (no lexer) | 7.85 ms, 32 MB | **2.21 ms, 6.4 MB** |
| `.js`, 3 000-char lines | 35 ms | 28.9 ms |
| `.js`, 600-char lines | 9.8 ms | 8.1 ms |
| 120-char lines | unchanged | unchanged |

That is the long-line cliff for plain text essentially gone; what remains at
3 000 characters is the lexer materialising a token per character.

## Stage 3: implemented, measured, reverted ⛔

A per-line token cache was added to `HlCache` (`hcToks`, keyed by line index and
validated by line identity + incoming lexer state, pruned to the visible
window), with `refreshHlWindow` filling it and the renderer reading it.

It worked, and for normal files it worked well:

| Per keystroke | Stage 1 | Stage 1 + 3 |
|---|---|---|
| `.js` 5 000 lines × 120 | 1.86 ms | **1.49 ms** |
| `.js` 5 000 lines × 600 | 8.1 ms | **3.6 ms** |
| `.js` 5 000 lines × 3 000 | 28.9 ms | **6.4 ms** |
| **`.js` 200 000 lines × 120** | **1.86 ms, 6.2 MB/frame** | **6.89 ms, 39.8 MB/frame** |

The last row is why it was reverted: a 3.7× slowdown and 6.4× more allocation on
a large file, reproducible, and **not** explained by the token work itself —
disabling the token filling entirely (leaving only the changed call structure)
kept the regression, as did reverting the renderer to the old call. The extra
~168 bytes per line of the file per frame is the signature of a full `Seq`
traversal (`syncCache`'s `zip (toList old) (toList cur)`) running where it did
not before, but which call site gains it was not identified within the time
budget.

**Whoever picks this up should start there**: instrument `syncCache` to count
its O(n) path, run `bench type` on the 200k-line case, and find why adding a
field to `HlCache` (or returning a new record from `refreshHlWindow`) changes
how often the `ptrEq (hcLines c) cur` fast path hits. The token-cache idea is
sound and the win is large; the interaction with cache identity is the problem.

## Stages 2 and 4: untouched

Run-length token vectors (§2) and a cheaper `Cell` (§4) are unchanged from the
analysis below.

---

---

## 1. What the measurements say

Per-keystroke cost of the *full driver cycle* (`update` → `refreshHighlight` →
`renderEditor` → `renderFrame` diff → Builder serialisation), 50×200 terminal,
5 000-line file, editor state threaded across iterations exactly as
`App.renderNow` does:

| Stage | `.js`, 120-char lines | `.txt`, 120-char lines | `.js`, 3 000-char lines |
|---|---|---|---|
| `update` alone | 0.01 ms, ~0 MB | 0.01 ms | 0.03 ms |
| `+ refreshHighlight` | 0.01 ms, ~0 MB | 0.01 ms | 0.03 ms |
| `+ renderEditor + diff` | **2.27 ms, 6.0 MB** | **0.97 ms, 3.8 MB** | 450 ms, 81 MB |
| same, full redraw (`prev = Nothing`) | 4.73 ms, 13.4 MB | 2.63 ms, 10.5 MB | 449 ms, 88 MB |

Three conclusions, all of them useful:

1. **The pure model is free.** Editing and the highlight-state cache cost
   essentially nothing, even on a 200 000-line file (measured: 2.23 ms/keystroke
   at 200k lines vs 2.21 ms at 5k lines — file size is irrelevant). The
   `HlCache` design works.
2. **Rendering is 100% of the cost.** Everything the user experiences as
   latency happens between `renderEditor` and the Builder.
3. **Per-line work is proportional to the whole line, not the visible window.**
   A `.txt` file (no lexer at all) costs 0.97 ms/frame at 120-char lines and
   **7.9 ms/frame at 3 000-char lines** — an 8× jump for content that is 95%
   off-screen. Something in the per-line path is O(line length).

Allocation is the other half of the story: 6 MB per frame for a 10 000-cell
screen is ~600 bytes per cell. At a comfortable typing speed that is
60–90 MB/s of garbage, all of it dying young. It is survivable — GHC's
generational collector is good at exactly this — but it sets the floor for
frame time and it is the reason GC shows up at all in a text editor.

## 2. Where the O(line length) work is

In `Render.drawTextArea` (`src/Cmedit/Render.hs:840-883`), the cell expansion
is already correctly windowed:

```haskell
(startCol, startDisp) = windowStart tabw left line
cells = expandLineCellsFrom … startCol startDisp (T.drop startCol line)
visible = takeWhile (\(d,_) -> d < left + tw) (dropWhile …)
```

but the *inputs* it is given are not:

| Site | Cost | Why it is whole-line |
|---|---|---|
| `urlLinks line` → `Link.urlSpans` | O(L) per line per frame | Scans the entire line for `http(s)://` even though only ~200 columns show |
| `visibleHighlight` → `highlightMap` → `lexLine` | O(L) per line per frame | The lexer must scan from the line start to be correct, but it also *materialises* `[Tok]` for every character |
| `mkBaseAt` | O(L) alloc per line per frame | `length toks` then `listArray (0, n-1) toks` — a fresh boxed `Array Int Tok` per visible line per frame |
| `liveMatchSpans ed line` | O(L) per line per frame | Only while a Find dialog is open, but then on every line |
| `lineSelInterval sel bl (T.length line)` | O(1) for `Text` | fine |
| `windowStart tabw left line` | O(startCol) | acceptable — bounded by the scroll offset |

So even with no highlighting, each visible line is walked at least once in
full, with `String` intermediates.

## 3. Design

Three independent stages, in increasing order of invasiveness. Stage 1 alone
should recover most of the long-line cliff.

### Stage 1 — window the auxiliary scans (half a day)

- **URLs.** Give `Link.urlSpans` a window: `urlSpansIn :: Int -> Int -> Text ->
  [(Int,Int,Text)]` scanning `[from, to)` extended left to the nearest
  whitespace (so a URL that starts just off-screen still yields its true
  target) and right to the end of the current token. Fall back to "no links on
  this line" past a length guard, mirroring the existing `urlLinks` guard at
  `maxHlLine`. The hover hit-test (`urlAtMouse`) can keep using the unwindowed
  version — it runs once per mouse event, not 50× per frame.
- **Live find matches.** `liveMatchSpans` already takes the line; give it the
  same window. Matches outside the visible columns cannot be drawn.
- **Diagnostics.** `diagOverFor` is already bounded by the number of diags on
  the line; leave it.

### Stage 2 — stop materialising a token per character (1 day)

`lexLine` returns `[Tok]` — one boxed list cell per character — which
`mkBaseAt` immediately converts to a boxed `Array Int Tok`. Both are per line,
per frame.

Replace the pair with a **run-length token vector**:

```haskell
-- Tokens as (startCol, tok) runs, ascending. A typical code line has 10–40
-- runs instead of 120 list cells + a 120-element boxed array.
type TokRuns = [(Int, Tok)]

lexLineRuns :: Lang -> HlState -> Text -> (TokRuns, HlState)
```

`lexWith` already computes `(n, tok, st')` per step — it *has* the runs and
then explodes them with `replicate n' tok ++ rest`. Emitting runs is strictly
less work. `mkBaseAt` becomes a lookup over a small list (or an unboxed
`UArray Int Word8` of run starts if the profile still shows it), and
`expandLineCellsFrom`'s `baseAt` calls walk the runs monotonically because it
already visits characters in order — pass a cursor instead of an index and the
lookup becomes O(1) amortised.

Keep `lexLine` as a thin wrapper over `lexLineRuns` so the existing tests and
the `Spec.hs` token assertions keep working.

### Stage 3 — cache the visible lines' tokens across frames (1 day)

Today `highlightMap` re-lexes every visible line on every frame, even when the
buffer has not changed at all (a cursor move, a status-bar update, a blink,
a mouse hover all repaint). Extend `HlCache` with a small **per-line token
cache**:

```haskell
data HlCache = HlCache
  { …
  , hcToks :: !(Map Int (Text, HlState, TokRuns))  -- line index -> (line, state-in, runs)
  }
```

Validity is the same trick the cache already uses for states: a hit requires
`ptrEq` on the line `Text` *and* equality of the incoming `HlState`. Bound it
to the visible window plus a small margin (say 4× the text height) and prune
on refresh, so it can never grow with the file.

Expected effect: a repaint with an unchanged buffer does **zero** lexing;
typing re-lexes exactly one line (plus any line whose incoming state changed).

### Stage 4 (optional, measure first) — a cheaper `Cell`

`Screen` is `Array Int Cell` with `Cell = CellL !Char !Style !(Maybe Text)`
and `Style = StyleU !Color !Color !Word8 !Color`. A full screen is 10 000
boxed cells plus their styles, freshly allocated per frame, and forced-background
themes (`thRemap`) allocate a *second* full set (`Render.hs:697-701`).

Two options, in order of preference:

- **Intern the styles.** Most cells share a handful of styles. `putCell`
  allocates a `Cell` regardless, but if the `Style` values come from the theme
  record (as they do for chrome) they are already shared; the ones that are
  not are built per character in `expandLineCellsFrom` (`diagTint`, the
  whitespace/selection variants). Hoisting those out of the per-character loop
  is a small, safe change.
- **Pack the cell.** A `Word64` per cell (21 bits char, 8 bits attr, two
  4-bit/8-bit palette indices, plus an escape to a side table for RGB and
  links) in a `UArray Int Word64` removes ~all of the per-cell allocation and
  makes the diff a word comparison. This is a real change to the renderer's
  vocabulary and should only be done if Stages 1–3 leave allocation
  unacceptably high. Note the `Cell`/`CellL` pattern-synonym trick already in
  place shows the codebase is comfortable with this style of representation
  change.

Do not do Stage 4 speculatively — measure after Stage 3.

## 4. Testing

- **Frame identity.** The strongest guard: for a corpus of editor states
  (plain text, selection, wrapped, CSV, image, search view, dialogs, each
  theme), assert `renderEditor` produces a byte-identical `Screen` before and
  after each stage. `Screen` is comparable cell-by-cell; add an `Eq`-style
  helper in the test rather than deriving one on `Screen`.
- **Windowed-scan equivalence.** For Stage 1, assert that the windowed URL
  spans agree with the unwindowed ones on every span that intersects the
  window, across a corpus including URLs that start before the window, end
  after it, and span it entirely.
- **Cache soundness.** For Stage 3, a randomised test: apply a random edit
  script to a buffer, and after each edit assert the cached token runs equal
  a from-scratch lex of the same line with the same incoming state. This is
  the same discipline `Spec.hs` already uses to fuzz `csvWidths`.
- **Budget guards.** Assert a frame over 3 000-char lines renders under a
  loose wall-clock ceiling, as in plan 0002.

## 5. Sequencing

Do `0002` (the quadratic) first — it is three lines and it is the difference
between 450 ms and 40 ms per keystroke on long lines. Then Stage 1 here, which
should take the `.txt` 3 000-char case from 7.9 ms back toward 1 ms. Stages 2
and 3 are steady-state wins that mostly show up as reduced allocation and
lower GC pressure over a long session; measure before and after with the same
harness so the numbers stay comparable.
