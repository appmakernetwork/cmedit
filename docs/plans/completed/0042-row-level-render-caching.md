# 0042 — Row-level render caching in `Cmedit.Render`

**Theme:** interactive keystroke latency and allocation churn during sustained
editing
**Status:** ✅ **DONE** (shipped 2026-11-26)
**Verification:** `make` (0 warnings), `make windows-check` (82/82 modules, 0
warnings), `make test` (**5 096 passing, 0 failed**, including
`test/RenderCacheSpec.hs`), `make soak` (flat live heap 0.4 → 0.4 MB, flat
alloc/op, flat ms/op), `make lint-invariants` clean, real PTY interactive
smoke test (`exit=0`).

---

## Resolved — what shipped and measured results

### 1. Pre-implementation stage profile (§2 gate)

Measured at `-O2` on a 50×200 terminal (10 000 cells, 47 visible text rows of
distinct lines so GHC cannot share across rows, 200 frames):

| Stage (200 frames, 50×200, `-O2`, pre-0042) | `.txt` 120 | `.js` 120 | `.js` 600 | `.js` 3 000 | `.js` 120 (Graphite) |
|---|---:|---:|---:|---:|---:|
| Full `renderEditor` (keystroke loop) | 202 ms / 908 MB | 258 ms / 997 MB | 750 ms / 2 389 MB | 748 ms / 2 027 MB | 374 ms / 1 206 MB |
| `newSurf + freeze` (10 000 cells) | 11 ms / 28 MB | 11 ms / 28 MB | 11 ms / 28 MB | 11 ms / 28 MB | 11 ms / 28 MB |
| Empty buffer (`newSurf` + chrome + remap + sanitize + freeze) | 22 ms / 37 MB | 22 ms / 38 MB | 22 ms / 38 MB | 22 ms / 39 MB | 47 ms / 245 MB |
| `urlSpans` / `urlSpansIn` on 47 visible lines | 34 ms / 222 MB | 35 ms / 222 MB | 175 ms / 1 121 MB | 65 ms / 388 MB | 35 ms / 222 MB |
| `expandLineCells` on 47 visible lines | 57 ms / 622 MB | 57 ms / 622 MB | 96 ms / 1 035 MB | 96 ms / 1 035 MB | 58 ms / 626 MB |
| `hScrollBarInfo` on 47 visible lines | 3 ms / 4 MB | 3 ms / 4 MB | 11 ms / 0 MB | 53 ms / 0 MB | 3 ms / 0 MB |
| `renderFrame` alone (1-char diff) | 33 ms / 24 MB | 33 ms / 28 MB | 36 ms / 36 MB | 35 ms / 40 MB | 38 ms / 28 MB |

The §2 gate (> 40% in `drawTextArea`) passed decisively: **85–95% of frame
time and allocation** lived in `drawTextArea` (`expandLineCells` list
construction, `urlSpans` character scanning on lines without URLs, `thRemap`
allocating 10 000 remapped `Cell`s per frame on 24-bit themes, and
`hScrollBarInfo` rescanning every visible 3 000-char line).

### 2. What shipped

1. **Zero-allocation URL fast-reject (`src/Cmedit/Link.hs`)**: `urlSpans` and
   `urlSpansIn` check `T.isInfixOf "://"` before walking the line — cutting
   47-line URL scanning on non-URL code from 35–175 ms / 222–1 121 MB (per
   200 frames) down to **~0 ms / 0 MB**.
2. **Direct-write cell expansion (`src/Cmedit/Render.hs`)**:
   `expandLineCellsInto` writes cells directly into a mutable `STArray` row
   slice without building an intermediate `[(Int, Cell)]` list, driven by a
   lightweight `BaseStyle` (`BsConst` / `BsRuns` / `BsFn`) so syntax token
   runs avoid per-character closure allocation. Used by plain text, word-wrap,
   pager, RTF, and PDF views; `expandLineCells` / `expandLineCellsFrom` remain
   exported as reference/oracle helpers.
3. **Pure, driver-owned `RenderCache` (`src/Cmedit/Render.hs`,
   `src/Cmedit/App.hs`)**:
   - Covers both **plain-text** (`rcRowsCache :: IntMap CachedRow`) and
     **word-wrapped** (`rcWrapCache :: IntMap CachedWrapRow`) views.
   - `Editor` stays completely pure and untouched; `App` owns
     `drvRenderCache :: IORef RenderCache` and updates it strictly in
     `renderNow`, while `renderEditorCore` drops cached maps automatically on
     non-text views and rebuilds the active map from empty every frame.
   - Global invalidation keys: `rcCols`, `rcRows`, `rcTheme`, `rcTextGeom`,
     `rcTabWidth`, `rcShowWs`, `rcLeft`, `rcWordWrap`, `rcFindKey`, and
     `rcDocId` (`edDocId`, distinguishing untitled buffers).
   - Per-row keys: `crLine` (`sameText` = strict `ptrEq || ==`), `crToks`
     (`sameToks` = strict `ptrEq || ==`), `crCurLine`, `crSel`, `crBrackets`,
     `crDiags` (plus `cwrComplete` and `length cwrRows >= needRows` for
     wrapped lines clipped at the viewport bottom).
   - **Buffer-pinning guard**: when a row hits via `(==)` rather than `ptrEq`
     (e.g. after Revert/reload with identical text), the cached entry
     re-anchors `crLine = line` and `crToks = toks` (likewise for
     `CachedWrapRow`) so the previous buffer's decoded `Text` is never pinned.
   - **Bounded memory**: each frame constructs a fresh `IntMap` containing
     only the currently visible buffer lines (≤ ~100 entries).
   - **Cached horizontal scrollbar width**: each `CachedRow` caches
     `crLineWidth` (using shared `lineDispWidth` / `hScrollBarInfoFor` factored
     into `Cmedit.EditorState`), eliminating the duplicate 47-line width scan
     in `drawHScrollWith`.
   - **Pre-remapped 24-bit theme rows**: on a cache miss under a forced-bg
     theme (`ThemeCherryBlossom`, `ThemeFlashbang`, `ThemeMidnight`,
     `ThemeGraphite`), the row slice is remapped once via `remapRangeInPlace`
     before freezing. Because every built-in theme remap passes exact
     `ColorRGB`/`Color256` pairs through unchanged, is idempotent
     (`f . f == f`), and maps all text-area styles to exact colours, the
     full-screen `remapRangeInPlace` pass skips cached rows in $O(1)$ with
     zero allocation (and preserves `ptrEq` for `renderFrame`).
   - **Strict `ptrEq` + `unsafeFreeze` + guarded `unsafeAt`**: `sameText`,
     `sameToks`, `sameCell` are strict (`!a !b`) and `INLINE`; `renderEditorCore`
     and row builders freeze with `unsafeFreeze`; `renderFrame` uses
     `screenCovers`-guarded `unsafeAt` with safe `(!)` fallback for
     hand-crafted screens.

### 3. Before / After benchmarks (`docs/plans/bench/Bench.hs`, `-O2`, 50×200)

All Plan 0042 cached benchmarks use **distinct lines per row** (`distinctDoc`)
so GHC cannot share cell work across rows.

#### Typing (100 keystrokes, full driver cycle: `update` → `refreshHighlight` → render → `renderFrame`)

| Workload (50×200, `-O2`) | Baseline (pre-0042) | Post-0042 (uncached `renderEditor`) | Post-0042 (`renderEditorCached`) | Speedup / Alloc reduction |
|---|---:|---:|---:|---:|
| `.js` 5 000 × 120 | 173 ms / 519 MB | 49 ms / 88 MB | **30 ms / 48 MB** | **5.8× / 10.8×** |
| `.js` 5 000 × 600 | 394 ms / 1 228 MB | 72 ms / 134 MB | **36 ms / 58 MB** | **10.9× / 21.2×** |
| `.js` 5 000 × 3 000 | 409 ms / 1 038 MB | 131 ms / 199 MB | **69 ms / 135 MB** | **5.9× / 7.7×** |
| `.txt` 5 000 × 120 | 138 ms / 471 MB | 41 ms / 72 MB | **25 ms / 36 MB** | **5.5× / 13.1×** |
| `.txt` 5 000 × 3 000 | 282 ms / 756 MB | 89 ms / 99 MB | **28 ms / 34 MB** | **10.1× / 22.2×** |
| `.js` 200 000 × 120 | 173 ms / 518 MB | 47 ms / 83 MB | **30 ms / 47 MB** | **5.8× / 11.0×** ($O(1)$ in file size) |
| `.js` 5 000 × 600 (`ThemeGraphite`) | — | — | **40 ms / 65 MB** | 24-bit forced-bg theme |

#### 1-line scrolling (100 frames, render + diff + emit)

Baseline and uncached columns are `bench scroll`: the same distinct-line
corpus with the editor threaded across frames, so the `HlCache` carries over
exactly as it does in `App.renderNow` and in the cached run. (An earlier
version of this table took them from `bench frame`, which restarts from one
editor on every frame and so re-lexes the whole viewport; that credited the
row cache with work the `HlCache` already saves and overstated the long-line
cases, 26x for `.js` 5 000 x 3 000 against the 4.1x measured here.)

| Workload (50×200, `-O2`) | Baseline (pre-0042) | Post-0042 (uncached) | Post-0042 (cached) | Speedup / Alloc reduction |
|---|---:|---:|---:|---:|
| `.js` 5 000 × 120 | 225 ms / 601 MB | 103 ms / 174 MB | **71 ms / 135 MB** | **3.2× / 4.5×** |
| `.js` 5 000 × 600 | 461 ms / 1 309 MB | 131 ms / 217 MB | **80 ms / 154 MB** | **5.8× / 8.5×** |
| `.js` 5 000 × 3 000 | 476 ms / 1 137 MB | 196 ms / 293 MB | **115 ms / 226 MB** | **4.1× / 5.0×** |
| `.txt` 5 000 × 120 | 205 ms / 557 MB | 93 ms / 162 MB | **69 ms / 127 MB** | **3.0× / 4.4×** |
| `.txt` 5 000 × 3 000 | 331 ms / 850 MB | 143 ms / 189 MB | **73 ms / 130 MB** | **4.5× / 6.5×** |
| `.js` 200 000 × 120 | 226 ms / 601 MB | 97 ms / 173 MB | **72 ms / 134 MB** | **3.1× / 4.5×** ($O(1)$ in file size) |

#### Word-wrap typing & PageDown (100 iterations, 50×200, `-O2`)

| Workload | Baseline (pre-0042) | Post-0042 (uncached) | Post-0042 (cached) |
|---|---:|---:|---:|
| `wrap=True` 120-char typing | 156 ms / 565 MB | 52 ms / 158 MB | **25 ms / 47 MB** (6.2× / 12.0×) |
| `wrap=True` 600-char typing | 223 ms / 725 MB | 64 ms / 206 MB | **28 ms / 63 MB** (8.0× / 11.5×) |
| `wrap=True` 3 000-char typing | 408 ms / 916 MB | 83 ms / 273 MB | **47 ms / 146 MB** (8.7× / 6.3×) |
| `wrap=True` 3 000-char PageDown | 881 ms / 4 491 MB | 491 ms / 3 856 MB | **510 ms / 3 858 MB** |

#### All-miss worst-case overhead (100 frames, `.js` 5 000 × 600, cached vs uncached)

| All-miss scenario | Uncached (`renderEditor`) | Cached (`renderEditorCached`) | Overhead |
|---|---:|---:|---:|
| Horizontal scroll (`edLeft++` every frame) | 274 ms / 892 MB | 284 ms / 899 MB | +3.6% time / +0.8% alloc |
| PageDown (non-wrap, 100% miss) | 358 ms / 773 MB | 375 ms / 779 MB | +4.7% time / +0.8% alloc |
| PageDown (`ThemeGraphite`, 100% miss) | 407 ms / 840 MB | 420 ms / 851 MB | +3.2% time / +1.3% alloc |
| Live-Find typing (query changes every frame) | 447 ms / 853 MB | 434 ms / 860 MB | −2.9% time / +0.8% alloc |

### 4. Test coverage (`test/RenderCacheSpec.hs`)

- **Theme remap contract tests**: exhaustively verify across all 6 themes
  (`ThemeDark`, `ThemeLight`, `ThemeCherryBlossom`, `ThemeFlashbang`,
  `ThemeMidnight`, `ThemeGraphite`) that every `thRemap` satisfies exact
  pass-through, idempotence (`f . f == f`), and exact output on every
  text-area style.
- **250-case randomized differential oracle**: deterministic SplitMix64 PRNG
  comparing `expandLineCellsDirect` (`expandLineCellsInto`) cell-for-cell
  against the original list-based `expandLineCellsFrom` across tabs, wide CJK
  glyphs, combining marks, control characters, URLs, selection ranges, EOL
  selection, overlays, diagnostics, and arbitrary `startCol`/`startDisp`.
- **Cache-hit pointer-identity proof**: verifies via `ptrEq` on forced cells
  that untouched rows actually reuse cached `Cell` objects across frames in
  plain Dark, plain Graphite, wrap Dark, and wrap Graphite.
- **Full-screen differential oracle**: asserts exact `scrCells`, `scrCursor`,
  and `scrHint` equality between `renderEditorCached` and `renderEditor` at
  every step across: typing, Enter, Backspace, multi-line `/* ... */` comment
  opening/closing, vertical/horizontal scroll, Shift+arrow selection, live
  Find highlights, bracket matching, line-number/whitespace/scrollbar/chrome
  toggles, all 6 themes + theme-picker live preview, linter diagnostics,
  menus/dialogs/explorer (including explorer wider than window), word-wrap
  with wrap+chrome and wrap+scrollbar toggles, CSV/Pager/RTF/PDF frames,
  and document switching.
- **Memory retention tests (`liveMB`)**: verifies closing a large buffer drops
  cached references (~0 MB retained) AND verifies Revert / `T.copy` reload
  with identical text re-anchors `crLine` without pinning the old buffer.
- **`renderFrame` safety**: verifies `screenCovers`-guarded `unsafeAt` falls
  back cleanly on hand-crafted undersized `Screen` arrays.

---

## Original proposal (preserved for context)

### 1. The problem, measured

Plan `0003` resolved the worst per-line bottlenecks (`urlSpansIn`, run-length
tokens in `Cmedit.Syntax`, and the bounded viewport token cache `HlCache`).
After `0003`, the full driver cycle (`update` → `refreshHighlight` →
`renderEditor` → `renderFrame`) for 100 keystrokes on a 50×200 terminal sat
at 138–409 ms and 471–1 228 MB allocated, while `update` + `refreshHighlight`
together took only ~0.01–0.03 ms per keystroke. Virtually 100% of remaining
per-keystroke cost was inside `Cmedit.Render.renderEditor` rebuilding all
visible rows from scratch on every character typed.

### 2. Risks addressed

- **Stale syntax highlight on multi-line constructs**: `crToks` is part of the
  per-row key (`sameToks`), so typing `/*` or `*/` automatically invalidates
  every downstream visible row whose token runs changed.
- **Double `thRemap` on 24-bit themes**: enforced by the verified theme remap
  contract (`themeRemapContractTests`) and pre-remapping cached rows once on
  miss.
- **$O(n)$ regression on 200k-line files**: `rcRowsCache` and `rcWrapCache`
  only ever index visible lines; 200k-line typing (30 ms) and 1-line scrolling
  (69 ms) match 5k-line files identically.
- **Buffer pinning on close or Revert**: rebuilding the active `IntMap` from
  empty each frame (plus non-text reset inside `renderEditorCore` and
  re-anchoring `crLine`/`crToks` on `(==)` hits) ensures no closed or
  reverted buffer survives a frame.
