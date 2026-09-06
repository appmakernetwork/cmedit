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
real refactor of the decoder's core and was not attempted here. The September
follow-up below implements and measures that experiment.

### Test coverage, which did not exist

The JPEG decoder had **no tests at all** — a decoder rewrite had nothing to
check itself against. `test/Spec.hs` now embeds two real 32×24 JPEGs produced by
an independent encoder (baseline grayscale and 4:2:0 colour) and pins 15
assertions: dimensions, format, neutral grayscale, saturated colour blocks
across hard edges, a gradient region, the checkerboard that stresses chroma
upsampling, corner pixels (the upsampler's edge clamping), full opacity across
every pixel, and truncated input. Verified to fail on a one-bit change to the
IDCT output.

### Entropy reader follow-up (2026-09-06)

Lifted `loadByteJ`, `nextBitJ`, `getBitsJ`, `recvJ`, and `decodeHuffJ` into
top-level `INLINE` functions taking the input bytes and the concrete
`STUArray s Int Int` state. All baseline and progressive call sites use them
directly. Bit ordering, stuffing, EOF/marker handling, restart resets, Huffman
tables, and reconstruction arithmetic are unchanged.

The same 1280×1014, 603 KiB photo, with **GHC 9.6.6** (the compiler installed
on this machine, rather than the documented 9.0.2), `-O2`, nine fresh-process
runs of each binary in alternating order:

| | Before this refactor | After |
|---|---:|---:|
| Median decode CPU time | 264.983 ms | 245.538 ms |
| Range | 260.302–290.531 ms | 243.626–248.731 ms |
| Allocated bytes per decode | 509,555,128 | 193,178,488 |

That is **7.3% less decode time and 62.1% less allocation**. The large churn
reduction does not translate into a proportional speedup; this experiment
does not establish where the remaining time is spent.

Complete RGBA buffers compared byte-for-byte against the pre-change decoder
for the photo and five small images: grayscale, 4:2:0, 4:4:4, progressive,
and restart-marker JPEGs. Two entropy truncations (EOF and a final lone FF)
also retained their full-buffer fingerprints. The suite now pins full RGBA
fingerprints for grayscale, 4:2:0, progressive, and restart-marker inputs;
the latter two are embedded fixtures with no runtime encoder dependency.

`make test` passed (3408 checks, zero failures), including invariant lint;
`make` passed. No PTY, Windows check, or soak was run: this change is confined
to pure decoding, with no terminal, platform, editor-state, or job changes.
GHC 9.0.2 was not available for a second compiler check.

The standalone harness is `docs/plans/bench/ImageDecode.hs`. Build using the normal
Makefile's package and extension settings, with an isolated output directory:

```sh
make -f Makefile -f /dev/stdin jpeg-bench <<'EOF'
jpeg-bench:
	$(GHC) --make docs/plans/bench/ImageDecode.hs -isrc -O2 -rtsopts -with-rtsopts=-T -outputdir /tmp/cmedit-jpeg-bench -o /tmp/cmedit-jpeg-bench-run $(PKGS) $(EXTS) $(WARN)
EOF
/tmp/cmedit-jpeg-bench-run Van_Gogh_-_Starry_Night_-_Google_Art_Project.jpg
```

Input IO and output hashing are outside the timed/allocation interval. Forcing
`imgPix` to WHNF forces the decode; do not sum pixels inside that interval.
Set `IMAGE_RGBA_DIR` to an existing directory to dump complete RGBA buffers
after measurement for direct comparison. To compare revisions, build each
source version into a separate output directory and alternate fresh processes.

### PNG follow-up (2026-09-06)

PNG had two substantial costs: shared DEFLATE decoding and scanline-to-RGBA
conversion. Isolating inflation on the original decoder measured roughly
197 ms for noise, 269 ms for the photo, and 108 ms for the screenshot. The
complete decodes also allocated 1–2 GB each.

What changed:

* DEFLATE bit operations are typed top-level `INLINE` functions. Multi-bit
  values consume available byte fragments instead of repeatedly reading and
  updating state for each bit.
* DEFLATE has a **512-entry Huffman prefix table** per tree, with the original
  canonical walk for longer or missing codes. The shared JPEG `Huff`
  representation is unchanged. Oversubscribed trees bypass the new table.
* Stored blocks copy aligned bytes directly; backreferences fetch the sink
  and check/grow capacity once per match. Copies still run forwards for
  overlapping matches, and output caps and truncated-input padding remain.
* PNG selects its filter once per scanline and reads only the neighbours it
  needs. Two alternating rows replace the full-pass reconstruction buffer,
  with a zero predecessor at the start of each Adam7 pass.
* Pixel/sample operations inline into the conversion loop. Eight-bit samples
  bypass the otherwise redundant scaling division.

Inlining alone removed most allocation but barely improved time. The prefix
table, filter/copy loops, and eight-bit conversion fast path supplied the
larger speed gains. No new editor dependency, platform code, or background
job was introduced.

GHC 9.6.6, `-O2`, **nine fresh-process runs per version per image**, alternating
before/after order. Input reading, output hashing and RGBA dumps are excluded.
Times are median decode CPU times; MB below are decimal allocated bytes,
not resident memory:

| PNG workload | Before | After | Speedup | Allocation before → after |
|---|---:|---:|---:|---:|
| 1280×1014 RGB photo (3.23 MB PNG) | 417.772 ms | 162.640 ms | 2.57× | 2008.37 → 61.24 MB |
| 1920×1080 RGBA screenshot (133 KB PNG) | 336.323 ms | 142.027 ms | 2.37× | 1008.94 → 27.35 MB |
| 1024×768 RGB noise (2.36 MB PNG) | 277.051 ms | 79.042 ms | 3.51× | 1371.22 → 33.77 MB |

Ranges (before → after): photo 414.477–443.225 → 162.020–192.443 ms;
screenshot 333.209–347.159 → 141.243–168.095 ms;
noise 276.265–302.318 → 78.361–104.198 ms.

Validation:

* All **33 complete PNG RGBA buffers** match the original decoder, including
  all 15 supported colour/depth combinations both with and without Adam7.
  The 30 small corpus images also match their source samples; supported
  Pillow conversions provide an additional colour/output comparison.
* Six JPEG RGBA buffers remain byte-identical after the shared-module change.
* The permanent suite embeds 16 PNG cases covering mixed filters, pass
  resets, split IDAT chunks, palette alpha, tRNS and every colour/depth pair.
  DEFLATE tests exercise stored/fixed/dynamic coding, 15-bit Huffman codes,
  overlapping matches, geometric growth, output caps, nonzero input offsets,
  reserved block errors and truncated stored-block padding.
* Another **420 comparisons against Python zlib** passed for mixed blocks,
  varied compression strategies, repetitive/random inputs and output limits.
* `make test` passed: **3507 checks, zero failures**, including invariant lint.
  `make` passed. Windows, PTY and soak checks were not run for this pure
  decoder change; these measurements do not include terminal image output.
  GHC 9.0.2 was not installed for a second compiler check.

Reproduce the large workloads with Pillow (11.1.0 was used here; it is only a
fixture-generation tool, not a runtime or test-suite dependency):

```sh
python3 docs/plans/bench/gen_png.py \
  Van_Gogh_-_Starry_Night_-_Google_Art_Project.jpg /tmp/cmedit-png-inputs
make -f Makefile -f /dev/stdin image-bench <<'EOF'
image-bench:
	$(GHC) --make docs/plans/bench/ImageDecode.hs -isrc -O2 -rtsopts -with-rtsopts=-T -outputdir /tmp/cmedit-image-bench -o /tmp/cmedit-image-bench-run $(PKGS) $(EXTS) $(WARN)
EOF
/tmp/cmedit-image-bench-run /tmp/cmedit-png-inputs/*.png
```

The former JPEG benchmark is now named `ImageDecode.hs`; it handles both
formats. Set `IMAGE_RGBA_DIR` to an existing directory for full-buffer dumps.

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
