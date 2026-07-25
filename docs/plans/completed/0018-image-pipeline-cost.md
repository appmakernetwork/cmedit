# 0018 — The image pipeline: decode off the main thread, scale without a list

**Theme:** UI stalls on a small file; per-resize and per-animation-frame cost
**Status:** ✅ **RESOLVED** — implemented 2026-07-26 (§2.4, the decoders
themselves, deliberately left; see below)
**Risk (as shipped):** low

## Resolved

**§2.1 — the scaler no longer builds a list.** `scaleRGBA` writes straight into
the result with `BS.unfoldrN`, and the box sums use unboxed accumulators instead
of a lazy 5-tuple per source pixel.

| 800×600 placement from a 1280×1014 source | Before | After |
|---|---|---|
| per call | 66 ms | **17 ms** |
| allocated per call | 63 MB | **21 MB** |

That cost is paid on every resize, every zoom-crop change, and every sixel
animation frame, so it is the frame rate of an animated image on a sixel
terminal.

**§2.2 — images go async by decode cost, not file size.** `EffOpen` sniffs the
magic number (one 16-byte read) and uses `imageAsyncBytes` (64 KiB) instead of
`asyncThresholdBytes` (2 MiB) for images. Verified through a PTY by opening the
603 KiB test JPEG with Ctrl+P: output arrives in two bursts — 946 bytes at
0.1 s (the editor painting a frame while the decode runs on the background
thread) and 130 655 bytes at 0.81 s (the image). Under the old rule the loop
was frozen for the whole decode with nothing on screen.

**Not taken:**

- **§2.3 (resize-drag debounce)** — with the scaler 4× faster this is no longer
  obviously worth an extra timer; revisit if a resize drag still stutters.
- **§2.4 (the decoders)** — **since done, 2026-07-26.** See below.
- A pointer-based writer (`unsafeCreate` + pokes) would remove the ~11 bytes per
  output byte that `unfoldrN`'s per-element `Maybe` still costs. Not taken:
  `unfoldrN` is safe, and 17 ms already meets the plan's target.

## §2.4 follow-up: the JPEG decoder (2026-07-26)

First, a correction to the framing above: **829 MB was never memory in use.**
It is cumulative allocation churn; peak RSS for that decode is 35 MB and the
result is 5 MB of RGBA. But at GHC's allocation rate the churn *was* most of
the 679 ms, so it was worth attacking as a speed problem.

| | Before | After |
|---|---|---|
| decode (1280×1014 JPEG, 603 KiB) | 679 ms | **~280 ms** |
| allocation churn | 830 MB | 485 MB |
| peak RSS | 35 MB | 35 MB (never the problem) |

**Output is byte-identical** — verified by content hash over the full RGBA
buffer for four images (4:2:0, 4:4:4, grayscale, and the 1280×1014 photo) at
every step of the change.

What shipped:

1. **Separable IDCT** with scratch buffers allocated once and reused, replacing
   a direct 8⁴ double sum per block plus a fresh 64-element list and `UArray`
   per block (~30 000 of each on that photo).
2. **Unboxed cosine table** — measured **20% faster on its own** (285 ms against
   358 ms).
3. **Unboxed bit-reader state** — four `STRef Int`s became one `STUArray`.
4. **Hoisted per-pixel component lookups**, plus a fast path that skips bilinear
   upsampling for a full-resolution component (always true of luma), where it
   provably reduces to a single read.
5. A strict `forLoop` helper for the hot loops.

### What the measurements refuted

Nearly every prediction I made was wrong, and only bisecting found the truth:

- **The bit reader** was supposed to be the dominant cost. Worth 84 MB — GHC
  was already unboxing most of the `STRef` traffic.
- **The per-pixel output loop** was the next suspect. Stubbing it out entirely
  changed allocation by *zero*.
- **`forM_ [a..b]` lists** in the hot loops: 12 MB. Already fused.
- **Unboxing the Huffman tables** (`Huff`) made it **worse** — 504 MB against
  485 MB on the same tree, with equal time — and was reverted. The elements are
  already-evaluated `Int`s, so a boxed lookup returns a pointer to an existing
  box, while an unboxed lookup must build a fresh box to return the value
  through `ST`. Unboxing pays where the read *fuses into arithmetic* (the
  cosine table) and costs where it is returned monadically. That distinction is
  now recorded in the code.
- One of my own harnesses polluted the numbers: summing 5.2 M pixels to "force"
  the image measured the summing. `imgPix` is a `runSTUArray`, so forcing it to
  WHNF *is* the decode. Third instance of that trap in this codebase.

The bisect that actually located the cost: with the IDCT stubbed out, allocation
was still 477 MB, so **~95% of what remains is the entropy decode** — the
per-bit `nextBit`/`getBitsJ`/`decodeHuffJ` path, whose `let`-bound actions GHC
will not inline out of the enclosing `do` block. Getting further means lifting
the bit reader to top-level `INLINE` functions over the state array. That is a
real refactor of the decoder's core and was not attempted here.

### Test coverage, which did not exist

The JPEG decoder had **no tests at all** — a decoder rewrite had nothing to
check itself against. `test/Spec.hs` now embeds two real 32×24 JPEGs produced by
an independent encoder (baseline grayscale and 4:2:0 colour) and pins 15
assertions: dimensions, format, neutral grayscale, saturated colour blocks
across hard edges, a gradient region, the checkerboard that stresses chroma
upsampling, corner pixels (the upsampler's edge clamping), full opacity across
every pixel, and truncated input. Verified to fail on a one-bit change to the
IDCT output.

The plan below is the original analysis, kept for the record.

---

---

## 1. Measurements

`Van_Gogh_-_Starry_Night_-_Google_Art_Project.jpg` — 603 KiB on disk,
1280×1014 — through the real `Cmedit.Image` API:

| Step | Time | Allocated |
|---|---|---|
| `decodeImage` (headers + structure) | 697 ms | 829 MB |
| first `scaleRGBA` to 800×600 (forces the pixel work) | 578 ms | 626 MB |
| steady-state `scaleRGBA` to 800×600 | **66 ms** | 63 MB |

Two problems fall out of this.

### 1.1 A 603 KiB image blocks the UI for over a second

`App.perform`'s `EffOpen` sends a file to the background loader only when it is
larger than `asyncThresholdBytes` (2 MiB). Images are the one file type where
**bytes on disk say nothing about decode cost**: this 603 KiB JPEG expands to
5 MB of RGBA and takes >1 s of pure computation, all of it on the main thread,
with no spinner — the editor is simply frozen. A 1.9 MiB PNG of a 4K screenshot
would be worse.

### 1.2 `scaleRGBA` builds a list of every output byte

```haskell
scaleRGBA img (cropX, cropY, cropW, cropH) outW0 outH0 =
  BS.pack [ chan | sy <- [0 .. outH - 1], sx <- [0 .. outW - 1]
                 , chan <- sample sx sy ]
```

800×600×4 = 1.92 M output bytes, each a boxed `Word8` in a cons cell before
`BS.pack` copies them out: 63 MB of allocation and 66 ms per call.

Where that repeats:

- **every terminal resize** while an image is open (`refreshImage` re-scales
  when the cache key changes — and a resize *drag* changes it continuously);
- **every animation frame on a sixel terminal** (the placement is re-emitted
  whole per frame, `Gfx.sixelPlace`), which is why `imageTickUs` has to floor
  the sixel frame rate by pixel area — the floor is compensating for this
  function;
- **every zoom-crop change**, i.e. every drag-release;
- and once per frame of `kittyClientAnim`'s pre-upload, ×N frames.

### 1.3 The placement encoders themselves are fine

For completeness, since they sit in the same path (800×600 placement of the
same image):

| Encoder | Per call | Allocated | Payload |
|---|---|---|---|
| `kittyPlace` (base64 RGBA) | 6.5 ms | 62 MB | 2 505 KiB |
| `sixelPlace` (6×7×6 palette + RLE) | 2.8 ms | 5.6 MB | 542 KiB |

The hand-rolled sixel encoder is both faster and 4.6× more compact than the
kitty payload — a good result, and no change is wanted. `kittyPlace`'s 62 MB
is the base64 expansion of the RGBA buffer; it only runs on a re-placement (not
per animation frame, which uses `kittySwapFrame`), so it is acceptable, though
a streaming base64 into the `Builder` would remove it if it ever shows up in a
profile. **The 66 ms `scaleRGBA` dominates both**, which is what §2.1 targets.

## 2. Fixes

### 2.1 Scale without the intermediate list (half a day, big win)

`Data.ByteString.unfoldrN` writes directly into the result buffer with no
intermediate list and is plain `bytestring` (a boot library):

```haskell
scaleRGBA img crop outW0 outH0 = fst (BS.unfoldrN total step 0)
  where
    total = outW * outH * 4
    step !i =
      let (px, ch) = i `quotRem` 4
          (sy, sx) = px `quotRem` outW
      in Just (channel sx sy ch, i + 1)
```

…with `channel` reading from a per-pixel box sum. Better still, hoist the box
sum out of the per-channel step so it is computed once per output pixel rather
than four times — today `sample` already does that, so keep the same shape and
change only the output construction, e.g. by writing into an
`ST`-allocated `STUArray Int Word8` and `BS.pack . elems`-free conversion via
`unfoldrN` over the frozen array.

Additionally, make `boxSum`'s accumulator strict and unboxed: it is currently a
lazy 5-tuple of `Int`s threaded through two loops, which allocates a tuple per
source pixel. Bang the components (`goX !x !y !n !r !g !b !a`) and the inner
loop becomes allocation-free.

Expected: 66 ms → single-digit milliseconds, and 63 MB → ~2 MB (the result
itself) per call. That in turn lets `imageTickUs`'s sixel floor be relaxed,
which directly improves animation smoothness on sixel terminals.

### 2.2 Route images through the async loader by cost, not by size

In `EffOpen`, sniff the magic bytes before deciding:

```haskell
-- Images decode into (width × height × 4) bytes of RGBA and cost far more
-- than their file size suggests — a 600 KiB JPEG is over a second of work.
-- Decide async on the *expected* cost, not the byte count.
asyncWanted sz isImage = sz > asyncThresholdBytes || (isImage && sz > imageAsyncBytes)
  where imageAsyncBytes = 64 * 1024
```

The plumbing already exists in full — `beginLoading`/`loadQ`/`endLoading` and
the spinner — so this is a predicate change plus a cheap `BS.take 16` read.
The startup path stays synchronous by design; that is fine, it is before the
screen is up.

### 2.3 Debounce re-scaling during a resize drag (optional)

A resize drag emits a `KResize` per column. `refreshImage` re-scales on each.
With 2.1 done this may no longer matter; if it does, the event loop already has
the timer vocabulary to coalesce: re-scale on the *last* resize after ~80 ms of
quiet, painting the previous (stretched) picture in the meantime.

### 2.4 The decoders themselves (measure first, then decide)

829 MB of allocation to decode a 603 KiB JPEG says the Huffman/IDCT path is
list-based somewhere. This is a bigger job than the scaler and it only affects
open time (once per image), so it should be driven by a profile rather than by
guesswork. Note that cost-centre profiling needs GHC's profiling libraries,
which are **not installed** in this environment (`ghc -prof` fails on `text`);
either install them first or use the tools that work on an ordinary build —
`+RTS -hT` for the heap shape, and bisecting by decoding stage (entropy decode
only, then IDCT, then colour conversion) with the harness. The likely
candidates, from the module's shape, are bit-reader state in a lazy tuple and
per-coefficient list building — both of which have the same fix as 2.1.

## 3. Testing

- **Pixel identity.** The scaler rewrite must be byte-identical to the current
  one: generate a set of synthetic images (1×1, 1×N, N×1, odd sizes, fully
  transparent, an alpha gradient) and assert `scaleRGBA` output equality
  before/after, at several output sizes and with crops.
- **Decoder corpus unchanged.** `Spec.hs` already covers the decoders; keep it
  green — any change here must not touch decode output.
- **Async threshold.** A test that a small image opens without blocking is
  awkward in-process; do it in the PTY soak (`0005`): open a 600 KiB JPEG and
  assert frames continue to be emitted during the decode (the spinner proves
  the loop is alive).
- **Performance guard.** `scaleRGBA` to 800×600 from a 1280×1014 source in
  under 10 ms.

## 4. Risks

- `unfoldrN` with the wrong length silently truncates; assert
  `BS.length result == total` in a test.
- The scaler feeds both the kitty/sixel placement and (via `renderImage`) the
  cell fallback. Pixel-identity tests cover both, but check the sixel encoder's
  palette output is unchanged on a reference image too — it quantises, so a
  one-LSB change in the scaler would be visible in the encoded output diff.
