# 0032 — Every image decoder, second pass: speed, churn and peak memory

**Theme:** decode latency, allocation churn and peak residency for every
format `Cmedit.Image` reads (WebP, JPEG, PNG + `Cmedit.Inflate`, GIF, BMP, PNM)
**Status:** ✅ **RESOLVED 2026-10-19**

## Method

One format at a time, each held to the same three rules, and each closed only
when an independent reviewer, given the code, the measurements and every
rejected experiment, judged that no further *reasonable* gain remained (it
returned "more gains" with a measured prototype several times; every such
prototype was adopted, re-verified and resubmitted until it did not):

1. **Byte-identical output for every valid file.** The harness hashes every
   frame's RGBA; the hash column of two builds must match on the corpus and
   on a set of generated variants per format (bit depths, palettes,
   transparency, interlacing, disposal methods, sub-rectangles, truncations).
   Pillow agrees bit-for-bit with the WebP decoder (it wraps libwebp) and on
   the corpus's 8-bit PNG, GIF, BMP and PNM files and the Adam7 variants,
   which pins those independently of the old code too.
2. **Corrupt input decodes to what it decoded to before.** `ImageFuzz.hs`
   decodes seeded mutants of small inputs through two builds and diffs the
   output. The only differences anywhere are the inputs on which the *old*
   decoder hung or threw, and one deliberate exception class (JPEG
   coefficients, below).
3. **Measured, not assumed.** Fresh processes, alternating builds, medians.
   Several plausible changes measured slower and were reverted; they are
   listed per format, because they are the most useful thing here for anyone
   tempted to retry them.

Harness: `bench/ImageDecode.hs` (`imgbench.sh`), corpus `bench/gen_images.py`,
fuzzer `bench/ImageFuzz.hs`; see `bench/README.md`. GHC 9.6.6, `-O2`.

## Results

Median decode time / bytes allocated / process peak (the peak includes the
input file and a 6 MB floor, so read it as a difference). Photo = the
1280×1014 Starry Night; screen = a 1920×1080 synthetic UI capture.

| File | Before | After |
|---|---|---|
| photo-lossless.webp | 856 ms / 7 909 MB / 35 MB | **80 ms / 35 MB / 15 MB** |
| screen-lossless.webp | 38 ms / 39 MB / 31 MB | **15 ms / 8.3 MB / 14 MB** |
| photo-lossy.webp | 400 ms / 1 671 MB / 15 MB | **109 ms / 25 MB / 15 MB** |
| screen-lossy-alpha.webp | 308 ms / 1 223 MB / 41 MB | **76 ms / 40 MB / 30 MB** |
| photo-420.jpg | 262 ms / 207 MB / 32 MB | **112 ms / 10.5 MB / 19 MB** |
| photo-444.jpg | 295 ms / 270 MB / 50 MB | **156 ms / 16 MB / 26 MB** |
| photo-prog.jpg (progressive) | 302 ms / 278 MB / 32 MB | **146 ms / 15 MB / 19 MB** |
| photo-gray.jpg | 125 ms / 171 MB / 25 MB | **55 ms / 5.2 MB / 12 MB** |
| noise.jpg (q95 4:4:4) | 292 ms / 563 MB / 37 MB | **133 ms / 9.5 MB / 20 MB** |
| photo.png (RGB8) | 171 ms / 61 MB / 24 MB | **61 ms / 28 MB / 19 MB** |
| screen.png (RGBA8) | 148 ms / 27 MB / 23 MB | **26 ms / 8.4 MB / 14 MB** |
| photo-interlaced.png (Adam7) | 161 ms / 101 MB / 24 MB | **54 ms / 36 MB / 20 MB** |
| noise.png | 83 ms / 34 MB / 19 MB | **27 ms / 10 MB / 16 MB** |
| photo-16.png (16-bit gray) | 125 ms / 39 MB / 22 MB | **41 ms / 16 MB / 15 MB** |
| photo-pal.png | 62 ms / 80 MB / 19 MB | **18 ms / 6.3 MB / 15 MB** |
| anim.gif (24 frames, all decoded) | 366 ms / 2 398 MB / 97 MB | **87 ms / 136 MB / 67 MB** |
| photo.gif (interlaced still) | 67 ms / 469 MB / 29 MB | **19 ms / 22 MB / 18 MB** |
| photo-24.bmp | 14 ms / 5.4 MB / 15 MB | **4.6 ms / 5.2 MB / 15 MB** |
| photo-8.bmp | 32 ms / 253 MB / 13 MB | **3.0 ms / 5.2 MB / 13 MB** |
| screen-32.bmp | 24 ms / 8.5 MB / 23 MB | **7.5 ms / 8.3 MB / 23 MB** |
| photo.ppm (P6) | 45 ms / 337 MB / 15 MB | **3.4 ms / 5.2 MB / 15 MB** |
| photo.pgm (P5) | 20 ms / 97 MB / 13 MB | **2.9 ms / 5.2 MB / 13 MB** |
| photo.pbm (P4) | 15 ms / 5.3 MB / 11 MB | **4.3 ms / 5.2 MB / 11 MB** |
| small-ascii.ppm (P3) | 291 ms / 284 MB / 199 MB | **15 ms / ~1.3 MB / 10 MB** |
| P6, maxval 100 (1280×1014) | 68 ms / 398 MB / 15 MB | **4.7 ms / 5.2 MB / 15 MB** |

The animated GIF's peak is mostly the 24 decoded frames themselves, which
`decodeFrames` returns (31 MB of RGBA).

## The GHC lessons, which explain most of the rewrites

Nearly every large win came from one of five patterns, and the same patterns
explain most of the rejected experiments. They are worth knowing before
touching any hot loop in this codebase:

- **An `ST` function that is not inlined boxes an `Int` result.** There is no
  nested CPR through `ST`'s unboxed tuple in GHC 9.6, so a top-level helper
  returning a symbol, a sample or a position allocates an `I#` per call. Fixes
  used: inline it, return through a state slot, or make the loop a local
  function called only in tail position (a join point).
- **A local function that captures many variables is not lambda-lifted** and
  is allocated afresh on every call of its parent (the VP8 token loop cost 244
  bytes per block this way). A join point does not allocate.
- **`where`-bound helpers of an inlined function become closures** at each
  use; top-level `INLINE` helpers taking the array explicitly do not.
- **Two monadic exits duplicate the continuation.** `vpGetBool` inlined at
  every token with an `if` returning in two branches let case-of-case copy each
  call site's continuation into both; one exit with the bit chosen as a pure
  value fixed it.
- **Nested `forLoop`s can box the inner counter**, and a function argument
  that is not known at the call site boxes whatever it returns (`scale`,
  `val`, a palette-reading closure). Flattening, or inlining the caller so
  the argument is known, fixed each instance.

Also: `-fmax-worker-args` raised module-wide regressed BMP; `NOINLINE` on a
function with a worker blocks the wrapper from inlining and boxes its
arguments; and `unsafeFreeze`-then-mutate aliasing, though measurably faster
for PNG's raw reads, was rejected as outside the API contract.

## Per format

### WebP

- **VP8L (lossless):** the 9-bit prefix table and canonical-walk fallback are
  `Cmedit.Inflate`'s (VP8L's codes are DEFLATE's); an unboxed bit-position
  slot with 32-bit peeks replaces an `STRef` plus a `Maybe` per byte; the whole
  image lives in one RGBA-sized buffer viewed as `Word32` (`castSTUArray`), and
  entropy decoding, all four inverse transforms (palette expansion back to
  front) and the final swizzle run in place. ALPH is written into its result
  and defiltered there.
- **VP8 (lossy):** libwebp's boolean decoder (56-bit prefetch, one computed
  renormalisation shift, one exit); scalar IDCT/WHT with a DC-only path;
  intra predictors over a 13-entry edge buffer, dispatched once per block;
  the token loop as a join point; the planes frozen without copying.
- Rejected: decoder state in loop arguments / CPS (no faster; reproduced
  independently), a pure VP8L pixel loop (less allocation, 15% slower),
  10–12-bit VP8L tables (table build dominated), caching B_VE/B_HE.

### JPEG

Output is pinned to the existing float IDCT and Double upsampling (the test
fingerprints), so the speedups are all exact restatements:

- A 64-bit accumulator reader that never loads past a marker (so restart and
  segment search are unchanged) and a 9-bit lookahead built by *running the
  canonical walk* on every prefix (malformed tables decode identically; the
  table stores symbol indices, so a truncated DHT's lazily-read symbols are
  forced only when used).
- The IDCT skips all-zero rows (their row-pass output is exactly +0.0, and
  adding ±0.0 to an accumulator that starts at +0.0 never changes it), gives
  DC-only blocks one value, and is straight-line code over literal cosine
  factors (`show` of the computed values, which read back identically).
- Upsampling tables per component, and subsampled chroma interpolated across
  once per source row into a two-slot row cache.
- `acRefine` without its four `STRef`s per symbol; per-scan component lookup.
- **Coefficients are `Int16`** (were `Int`): 7.8 MB instead of 31 MB on a
  4:4:4 photo. Every coefficient of every valid JPEG fits (8- and 12-bit); a
  corrupt stream whose values overflow decodes to *different* garbage than
  before (13 of 1 500 fuzz mutants), never a crash.
- Rejected: per-term masked column sums, 1/2/4/8-term column kernels (call
  sites boxed), an 11-bit lookahead, tabulated colour products.

### PNG and `Cmedit.Inflate`

- Inflate: counting-sort `buildHuff` and `ST`-built tables (O(n) per dynamic
  block, was O(maxLen·n) through lists), 24-bit window reads, a 10-bit table,
  an out-of-line slow walk, a known-size block loop writing straight into its
  buffer, and long back-references as block copies (chunked so overlapping
  matches copy exactly what the byte loop did).
- **PNG decodes non-interlaced images in place**: one buffer of
  `max outSize rawSize` bytes, the stream inflated into its tail
  (`inflateInto` takes a base offset; a distance reaching before it reads
  zeros, as one before the start of a fresh buffer does), each row unfiltered
  into two scratch rows and converted to its final place at the front, which
  provably never overwrites a row still to be read; the buffer is then shrunk
  in place (`shrinkMutableByteArray#`). That removes a raw-sized allocation
  (8 MB for the screenshot).
- Per-layout row converters (RGBA8 is a block copy), a division-free 16-bit
  scale (`if lo < hi then hi - 1 else hi`, checked on all 65 536 inputs), a
  branch-light Paeth (checked on all 2^24 triples), unfiltering split at bpp.
- **Fixed a pre-existing hang:** a DEFLATE stream truncated at a block
  boundary decoded the zero padding as endless empty stored blocks (PNG, PDF
  content streams and ZIP members alike). The block loop now stops at the end
  of the data, which changes nothing for any input that ever finished.
- Rejected: a frozen alias for raw reads, a generalised direct-to-output
  RGBA8 unfilter (boxed), positions as loop arguments, 11-bit table, a
  two-literal fast path.

### GIF

- LZW writes each string backwards straight into its sink from a dictionary
  of prefix/suffix/first/length; the one way a corrupt stream can break a
  chain (an entry whose prefix is a code not yet defined) is marked at
  creation, and such codes take the original stack walk out of line.
- **Fixed a pre-existing hang and crash:** a dictionary with a cycle made the
  old first-symbol walk spin forever, or the old chain walk index past its
  stack. That exception escapes the pure decoder uncaught: the old editor,
  opening the 42-byte GIF the Spec test now pins, exits with status 1. The
  stream now ends there and the image displays.
- A packed RGBA palette viewed as words like the canvas; opaque full-canvas
  non-interlaced frames decode straight into a fresh canvas; the first frame
  starts on a fresh canvas and disposal 2 is folded into the next copy.
- Rejected: fusing interlaced frames too (a `quotRem` per pixel cost more
  than the index pass), `Word8` indices (no speed gain).

### BMP and PNM

Row loops with unchecked reads inside the validated raster and 8-bit fast
paths; indexed BMP through a prepacked RGBA word palette (built with `palAt`,
so a pixel past a short colour table still reads the bytes that follow it);
P4 expanded eight pixels per input byte with word writes; ASCII PNM samples
parsed straight into the image by a tokenizer that keeps the old quirks
exactly (a stray non-digit is a 0 sample without advancing; a short raster is
still an error) instead of through a list and a boxed array, with the maxval
scaling written in rather than passed as a function, which boxed every
sample; binary samples off the 8-bit/255 path scale through a table over every
value a sample can hold (a corrupt one can exceed maxval), holding exactly the
truncated bytes the division gave. Rejected: packed word stores for 24/32-bit
BMP and P6 (no gain), `newArray_` (slower).

## Known, not changed

JPEG still throws array-index exceptions on some corrupt files (header nibbles
indexing four-entry tables, a scan's `Se` past 63, a truncated header), in the
old code and the new alike (72 of 1 500 fuzz mutants, identical). Nothing
catches an exception escaping a pure decoder on its way to the screen — the
cyclic GIF above showed the editor exits on one — so such a file can take the
editor down. That is a robustness fix outside this plan's scope (the decoder
needs a way to fail rather than a faster way to succeed) and is left as found.
