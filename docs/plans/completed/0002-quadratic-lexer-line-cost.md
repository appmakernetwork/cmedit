# 0002 — Remove the quadratic in the syntax lexer's step driver

**Theme:** interactive latency; "no performance drop-off when lots is happening"
**Status:** ✅ **RESOLVED** — implemented 2026-07-26
**Risk (as shipped):** very low — one function changed, no step function touched

## Resolved

`Cmedit.Syntax.lexWith` now clamps with the bounded `clampLen` instead of
`min n (length cs)`. Verified against the same harness:

| Measurement | Before | After |
|---|---|---|
| `lexLine` on 8 000 chars (JS / Python / SQL / Haskell / YAML) | 42–102 ms | **0–2 ms** |
| Per keystroke, `.js` 3 000-char lines (full driver cycle) | 452 ms | **35 ms** |
| Per keystroke, `.js` 600-char lines | 21.9 ms | **8.8 ms** |
| Per keystroke, `.js` 120-char lines | 2.56 ms | 2.01 ms |

Tests added to `test/Spec.hs` (suite now 1156 passing, was 1138):
- a **ratio guard** per language — lexing 4× the input must take under 10×
  the time (linear ≈ 4×, the old quadratic ≈ 16×);
- one-token-per-character assertions across five languages, which is the
  invariant the clamp exists to protect.

The plan below is the original analysis, kept for the record.

---

---

## 1. The problem

`Cmedit.Syntax.lexWith` is the driver behind every character-stepping lexer
(SQL, Python, HTML, JS, CSS, Shell, JSON, YAML, TOML, INI, FTL, Jinja,
Haskell — everything except Markdown and CSV):

```haskell
lexWith :: Step -> HlState -> Text -> ([Tok], HlState)
lexWith step st0 line = loop st0 (T.unpack line)
  where
    loop st [] = ([], st)
    loop st cs =
      let (n, tok, st') = step st cs
          n' = max 1 (min n (length cs))          -- <-- O(remaining) per step
          (rest, stEnd) = loop st' (drop n' cs)
      in (replicate n' tok ++ rest, stEnd)
```

`length cs` walks the entire remaining line **on every step**. A step usually
consumes one token (often one character), so a line of length *L* costs
`L + (L-1) + (L-2) + … = O(L²)` list traversals.

The clamp is purely defensive — every `Step` computes its count from the very
list it was handed, so `n` can never exceed the remaining length. The safety it
provides is worth keeping; its cost is not.

Why it matters in practice: `maxHlLine` is 20 000, so lines up to 20 000
characters are lexed in full. Long lines are not exotic — minified JS/CSS, a
one-line JSON payload, a wide generated SQL insert, a base64 blob in a config
file, or a Markdown table. Worse, `Render.highlightMap` re-lexes **every
visible line on every frame**, so the cost is paid per keystroke, per scroll
step, per cursor blink-triggered repaint.

## 2. Evidence

Measured against `src/` at `-O2` (times are per call; allocation is RTS
`allocated_bytes`).

**`lexLine` alone** — the quadratic is unmistakable (4× time for 2× length):

| Language / content | 2 000 chars | 8 000 chars |
|---|---|---|
| Python / code | 10 ms | 81 ms |
| Python / whitespace | 6 ms | 102 ms |
| JS / code | 3 ms | 57 ms |
| SQL / code | 3 ms | 51 ms |
| Haskell / code | 3 ms | 42 ms |
| YAML / whitespace | 6 ms | 88 ms |

At the `maxHlLine` limit of 20 000 characters this extrapolates to roughly
0.5–1 s **per line, per frame**.

**Whole frames** (`renderEditor` + `renderFrame` diff + Builder serialisation,
50×200 terminal, 5 000-line file, 100 scrolling frames):

| File | Line length | Before | After the fix |
|---|---|---|---|
| `.js` (lexed) | 120 | 3.7 ms/frame | 3.3 ms/frame |
| `.js` (lexed) | 600 | 34.6 ms/frame | 12.9 ms/frame |
| `.js` (lexed) | 3 000 | **819 ms/frame** | **56 ms/frame** |
| `.txt` (not lexed) | 3 000 | 7.7 ms/frame | 8.5 ms/frame |

These frames scroll (each frame exposes new lines, so the `HlCache` extends),
which is the fairest test of the lexer. The steady-state *typing* case, with
the editor threaded across iterations exactly as `App.renderNow` does it, shows
the same thing: `.js` with 3 000-character lines costs **452 ms per keystroke**
before the fix and **40 ms** after; with 600-character lines, 21.9 ms → 9.8 ms.
The cache is not hiding the problem, because `Render.highlightMap` re-lexes
every *visible* line on every frame regardless of the cache (which stores
line-start states, not tokens — see `0003`).

819 ms per frame is not "slow", it is a hung editor: every keystroke in a
minified file takes the best part of a second, and holding a key queues frames
faster than they can be drawn. The `.txt` row is the control — with no lexer
the same file renders in 8 ms, which is what identifies the lexer as the cost.

## 3. The fix

Replace the unbounded `length` with a bounded one. Total work becomes the sum
of the chunk sizes, i.e. O(L):

```haskell
lexWith :: Step -> HlState -> Text -> ([Tok], HlState)
lexWith step st0 line = loop st0 (T.unpack line)
  where
    loop st [] = ([], st)
    loop st cs =
      let (n, tok, st') = step st cs
          n' = max 1 (clampLen n cs)
          (rest, stEnd) = loop st' (drop n' cs)
      in (replicate n' tok ++ rest, stEnd)

-- @min n (length xs)@, but stopping as soon as @n@ elements have been seen.
-- The clamp guards against a step that over-reports; walking the whole
-- remainder to find that out made every lexer quadratic in the line length.
clampLen :: Int -> [a] -> Int
clampLen n xs0 = go 0 xs0
  where
    go !k _ | k >= n = n
    go !k []         = k
    go !k (_ : r)    = go (k + 1) r
```

This is the exact patch that produced the "after" column above.

## 4. Second-order cleanups (same file, optional but cheap)

The individual steps have their own `length`-of-remainder calls, but each is
a *single* call that ends the token, so they are O(L) overall, not O(L²):

- `pyNormal`: `(length cs, TkComment, StNormal)` for a comment-to-end-of-line —
  fine, but it walks the tail purely to produce a count that `lexWith`
  immediately re-clamps. Returning a sentinel (e.g. `maxBound`) and letting
  `clampLen` do the bounded walk removes one full pass per comment line.
- `pyStrStart` calls `length pre` on a `span` result — bounded by 2 in
  practice, harmless.
- `findSub` is O(L) per call and can be called once per triple-quote/dollar
  string — acceptable.

None of these change the asymptotics; do them only if the profile still shows
them after the main fix.

## 5. Testing

1. **Correctness parity.** The lexer has existing token tests in
   `test/Spec.hs`. Add a property-ish test: for a corpus of ~20 representative
   lines per language (code, comments, strings, tabs, wide glyphs), assert
   `lexLine lang st line` is byte-for-byte identical before and after — most
   simply by pinning expected token runs in the test, which is the existing
   style there.
2. **A complexity guard.** A timing assertion in a hand-rolled suite is
   fragile, but a *ratio* assertion is stable: lex the same content at 2 000
   and 8 000 characters and assert the 8 000 case takes less than 10× the
   2 000 case (a quadratic gives 16×, linear gives 4×). Use
   `GHC.Clock.getMonotonicTime`, run each 3× and take the minimum. Mark it
   clearly as a performance guard so a future reader knows why it is loose.
3. **Long-line rendering smoke test.** Render a frame over a file of 3 000-char
   lines and assert it completes within a generous wall-clock budget
   (e.g. 200 ms), which today would fail at 819 ms.

## 6. Risks

- `clampLen` must keep the `max 1` outside it, or a step returning 0 would
  loop forever. The prototype preserves this.
- `BangPatterns` is already in the Makefile's shared `EXTS`, so `go !k` needs
  no pragma.
- Behaviour is identical for every input where a step reports a count within
  the remaining length — i.e. all of them. For an (impossible today)
  over-reporting step, `clampLen` clamps to the same value `min n (length cs)`
  would have produced.

## 7. Relationship to other plans

This removes the *algorithmic* cost. The remaining per-frame cost of
highlighting — 56 ms/frame at 3 000-char lines, 122 MB allocated per frame —
is a *representation* problem (a `String` per line, a `[Tok]` per character, a
fresh `Array` per line, several full-line passes per frame) and is addressed
separately in `0003-frame-cost-and-per-line-work.md`. Do this plan first: it is three
lines and it is the difference between "unusable" and "slow".
