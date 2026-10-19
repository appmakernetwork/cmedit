# 0033 — JPEG XL: a from-scratch decoder, held to libjxl and tuned to a stop

**Theme:** a new image format (`.jxl`) for the image view, written from first
principles like every other decoder here, then taken through the `0032`
discipline for decode latency, churn and peak memory
**Status:** ✅ **RESOLVED 2026-10-19**

## What was built

Five modules, layered so that header code and hot loops never share a monad:

| Module | Contents |
|---|---|
| `Cmedit.JxlBits` | Bit reader over a zero-padded `ByteArray#` (64-bit unaligned peeks), `U32`/`U64`/`F16`/enum fields, the `J` monad (`ExceptT String (ST s)`) for header code |
| `Cmedit.JxlEntropy` | Prefix codes, ANS with alias tables, LZ77, hybrid integers, clustered contexts, permutations; `SymReader` + `INLINE readWith` as the hot-loop interface |
| `Cmedit.JxlModular` | MA trees, all predictors incl. the weighted predictor, RCT / palette (with delta) / squeeze, specialised loops per tree shape |
| `Cmedit.JxlVarDct` | All 27 `DctSelect` transforms (DCT 2…256, hornuss, DCT4×8, AFV), dequantisation matrices, LLF from LF, gaborish, EPF, HF coefficient decode |
| `Cmedit.Jxl` | Container and codestream, image/frame headers, TOC, LF/pass groups, chroma subsampling / YCbCr, patches, splines, noise, upsampling, LF frames, reference slots, blending, spot colours, XYB → sRGB with libjxl's ordered dither, orientation |

`Image.decodeJXL` adapts the result; `sniffImage` recognises the bare
codestream and the box container; `App.asyncThresholdFor` sends a JPEG XL to
the background loader at *any* size (a 47 KB wallpaper is a 16-megapixel
picture); `.jxl` joined the explorer's image extensions and the workspace
search's binary list. Only the first displayed frame is decoded.

## The correctness bar

**libjxl's own 8-bit output, at most one code value away; lossless exact.**
The corpus is 137 files generated through libjxl itself (lossless efforts
1–9, lossy distances 0.5–4, modular lossy, no-XYB VarDCT, grey, 16-bit,
alpha, animations, layers with every blend mode, patches, noise at several
ISO levels, upsampling 2/4/8, progressive DC / LF frames, JPEG
recompression with subsampled chroma, EPF 0–3, orientations) plus the 31
GNOME wallpapers in `/usr/share/backgrounds/gnome` (independent: written by
whatever produced them, 4096×4096, 47 KB–2 MB). Every file is within 1 of
libjxl; lossless ones are byte-identical. Three behaviours needed
measurement against libjxl to get right, and are recorded in CLAUDE.md:
the ordered 8×8 Bayer dither on 8-bit output (`x & 3`), colour domains for
blending (frames not saved before the colour transform live in the encoded
space), and the noise seed (counts the current frame; reference-only and LF
frames count as neither visible nor invisible; applied after upsampling).
Modular property 8 follows libjxl rather than j40.

Robustness: Spec sweeps every truncation and two bit flips at every offset of
two fixtures (lossless RGBA in the container, lossy bare codestream);
`ImageFuzz.hs` over 45 corpus files × 150 seeded mutants (6 750 decodes) gives
only clean `Left`s, no exceptions, no timeouts.

## Method

The `0032` method, unchanged: an independent reviewer (GPT-6 Astra, high
effort) given the code, the measurements and every rejected experiment,
working in a private copy and returning measured, validated patches, and the
work closed only when it judged no *reasonable* gain to remain. Five review
rounds; round 1 preceded the first complete decoder. Every adopted patch was
re-verified here against the full corpus (identical error statistics, i.e.
identical output), `make test`, `make windows-check` and a PTY open; the
reviewer additionally hash-compared every RGBA byte across 171 files (the
corpus plus two 4096² alpha fixtures) and ran direct bit-exact harnesses for
each restructured transform (406 IDCT cases, 1 890 EPF, 663 noise, 462
squeeze, 2 250 extra-plane cases).

## Results

Decode time (min of 3, GHC 9.6.6 `-O2`), first complete decoder → final.
Peaks are process high-water RSS.

| File | First complete | Final |
|---|---|---|
| ring-l (4096², DCT64-heavy, EPF, noise) | 12 700 ms / 27.1 GB alloc / ~480 MiB | **2 385 ms / 431 MB / 233 MiB** |
| pills-l (4096², DCT16/32, dense HF) | ~12 000 ms / ~490 MiB | **2 428 ms / 759 MB / 251 MiB** |
| amber-d (4096² + squeezed alpha) | 12 900 ms | **2 472 ms / 658 MB / 310 MiB** |
| photo-ll-e1 (1280×1014 lossless) | 105 ms | **100 ms** |
| photo-ll-e3 | 332 ms | **267 ms** |
| photo-ll-e7 | 444 ms | **401 ms** / 59 MB / 20 MB residency |
| photo-ll-e9 | 478 ms | **465 ms** |
| gray-ll | 166 ms | **148 ms** |
| photo-d1 (lossy) | 199 ms | **185 ms** / 130 MB / 17 MB residency |
| photo-d1-e3 (EPF-heavy) | 447 ms | **212 ms** |
| photo-d4 | ~370 ms | **244 ms** |
| screen-d1 (1920×1080) | ~735 ms | **278 ms** |
| screen-ll | 237 ms | **211 ms** |

The 16 MP float floor is three colour planes (192 MiB); the remaining peak
is block/LF data, filter scratch and collection timing.

## What moved the numbers

By round:

- **Own pass (before review round 2).** Noise convolution from 25 mirrored
  reads per pixel per plane to a separable box sum (6.1 s → 0.62 s on
  ring-l); EPF from evaluating every kernel position directly to per-channel
  difference rows and cross sums in rings keyed by *virtual* row, summed in
  the direct formula's order so the output is bit-identical (3.6 s →
  0.85 s); IDCT columns gathered sixteen at a time (a plane's stride is a
  power of two: column-at-a-time walking maps every row to one cache set)
  and all-zero rows skipped (1.6 s → 0.72 s).
- **Round 2.** HF symbols forced before a conditionally-demanding
  continuation and `withSymReader`'s continuation marked `INLINE` (pills HF
  churn 1.39 GB → 158 MB); noise generated a group row at a time with the
  high-pass fused into application (scratch 192 → 12.6 MiB); unrolled
  DCT-16/32 kernels; weighted EPF cross sums. Peak 482 → 296 MiB.
- **Round 3.** Pruned straight-line IDCT kernels for proven zero tails
  (98.7 % of ring-l's 64×64 blocks have ≤ 8 live rows); EPF restricted to
  dilated spans around blocks whose sigma filters (pills EPF 487 → 153 ms);
  the RGBA output written into the first colour plane, and final modular
  channels converted to float in place (peak 296 → 238 MiB); a specialised
  WP path for the uniform property-15 table (e3 −15 %).
- **Round 4.** Found why six wallpapers were 50 % slower: a full-size alpha
  channel reconstructed by 18 squeeze steps, whose vertical inverse walked a
  column at a time (the same 16 KiB-stride cache trap). Row-pair inverse
  squeeze (gmFinish 951 → 201 ms); a guarded five-property mixed-predictor
  tree walk and an interior neighbour fast path (e7 −11 %); EPF ring copies
  as bulk `copyMutableByteArray#` spans and an all-ineligible bypass.
- **Round 5.** Provably constant extra channels (opaque alpha) are
  recognised and never materialised as full planes (gmFinish 201 → 17 ms;
  amber-d peak 438 → 310 MiB).

## GHC lessons new to this plan

The five `0032` patterns all recurred. These were new:

- **A symbol that a continuation may not demand stays boxed.** The
  `cctx >= 458` error exit made the HF symbol conditionally used, so GHC
  passed a deferred `case … of I# …` into the shared continuation: 1.2 GB of
  churn on one file, fixed by two bangs.
- **A big callback handed to an `INLINE` combinator is still a function of
  its argument** unless it is itself marked `INLINE`; until it was, Core
  still dispatched on `SRAns`/`SRGen` per symbol.
- **A loop bound computed inside the loop floats out as a lazy thunk**, and
  a bang placed inside the loop floats several lazy `Bool`s instead; compute
  and force it *before* the loop so it arrives as an `Int#` join-point
  argument (the bounded IDCT gather allocated an extra 64 bytes per block
  until it did).
- **`castWord32ToFloat` is a runtime call** (`stg_word32ToFloatzh`); writing
  the bits through a `Word32` view of the plane and reading it back as
  `Float` is free.
- **Power-of-two strides are a cache trap twice over** (IDCT columns, then
  vertical squeeze): any column-major walk of a plane needs a strip or a
  row-pair formulation.
- **Reorder the work, never the arithmetic.**
  Every restructured filter keeps the direct formula's float operation order,
  which is what lets "bit-identical" be checked rather than argued.

## Rejected experiments

Kept because they are the most useful thing here for anyone tempted to retry
them: a CPS EPF kernel chain (allocated more); generic recursive zero-tail
IDCT pruning (ring 538 → 578 ms: scratch traffic and boxed offsets at worker
boundaries; generated kernels won); a fully unrolled DCT-64 (mixed: ring
faster, pills slower); unrolled LLF forward DCTs (noise-level); gaborish
neighbours carried in registers (slower despite nine reads becoming three);
bulk-copied gaborish rows (+40 ms across large images, cause not isolated);
scalar sparse EPF ring copies (dense ring regressed; bulk copies are the
fix); an interleaved three-field MA tree layout (e7 495 → 587 ms); WP
continuation/scale specialisation on general trees (no e7/e9 gain, Core
21.5 → 35.6 MB); a WP walk generalised to every local property (weaker than
the narrow one); gaborish fused into the first EPF ring load (exact, but
adjacent pairs regressed and it complicates mirror scheduling).

## Stopping point

Round 5's verdict: **no further reasonable gains.** What remains substantial
on a dense 16 MP file (EPF ~640 ms, noise ~600, render ~430, gaborish ~210)
needs banded reconstruction → filter → output scheduling with reference and
halo handling, a pipeline rewrite with uncertain payoff, and the same is true
of going below the three-float-plane memory floor.

## After the stop: two competing strategies, combined (2026-10-20)

"No further reasonable gains" was a verdict about *incremental* work, so the
next step was structural, run as a competition: Claude Fable 5.1 and GPT-6
Astra each proposed a strategy (larger restructurings explicitly in scope),
audited the other's and revised their own, then implemented it in a private
copy. After phase 1 every run was sequential, because two agents
benchmarking at once had contaminated each other's phase-1 numbers. The
judging was independent: both builds were rebuilt from their patches and
timed in alternating fresh processes pinned to one CPU (1 thread) or four
physical cores (4 threads), with nothing else running.

The two plans converged on one single-threaded core, which both shipped:

- **Two-phase XYB render** (Fable): XYB → linear RGB for a 32-pixel chunk
  into scratch, then transfer, dither and bytes, so neither loop carries the
  whole live set, and one provably redundant clamp removed (the input clamp
  already bounds the monotone sRGB table, and the dither is under 0.5).
- **Interleaved EPF rings** (Fable): steps 1 and 2 read D/S rings laid out
  `[row][x][pair][chan]`, so one output's reads are adjacent; the planar
  step-0 rings are allocated only when step 0 runs.
- **Row-streamed noise with exact GF(2) jump-ahead** (Astra): xorshift128+
  is linear over GF(2), so a channel's starting state is its group seed
  advanced by a matrix power, exactly. The field keeps 8 rows instead of a
  group's 260 (−14 MiB on every wallpaper).

Each added its own parallel stage on top:

- **Fable:** `Cmedit.JxlPar.parOrdered`, the one parallel primitive. Workers
  run `job i` and the caller consumes results in index order with bounded
  lookahead, so shared writes happen on one thread, deterministically. It
  is cancellation-safe: forks under `mask`, kill-and-join under
  `uninterruptibleMask` on any exception, so no worker outlives the call
  (Astra's audit had reproduced exactly that defect in Fable's phase-2
  helper). Fable used it for the final render, in row bands.
- **Astra:** parallel restoration (gaborish + EPF) in bounded 128-row bands
  of 512-pixel tiles with 8-pixel halos, and noise in disjoint row bands
  that regenerate their halos from the RNG rather than reading another
  band's output; Astra also tightened `parOrdered`'s cleanup (partially
  spawned pools, credits returned only after consumption, so a tile slot
  cannot be reused while its output is being copied).

Astra's build won clearly at 4 threads (wallpapers −55 %, against
Fable's −20 %) and tied Fable's at one. The adopted build is **Astra's patch
plus Fable's parallel lossless pass groups** (`passGroupsPar`), which Fable
had built and held back because it raised peak memory on lossless files.
That memory was not inherent, as Fable believed: the job thunks were
memoised in an array that lived for the whole pass, so *every* group's
decoded output stayed reachable until the pass ended (+305 MiB on a
4096² lossless RGBA image). Building each job's result afresh inside the job
leaves only the selection lists in the array; the overhead fell to +45 MiB
there and 2–13 MiB on small files.

| File | Before (1 / 4 thr) | After, 1 thread | After, 4 threads | Peak, 4 threads |
|---|---|---|---|---|
| ring-l | 2315 / 2326 ms | −12.0 % | **−54.7 %** | 233 → 237 MiB |
| pills-l | 2453 / 2476 ms | −8.3 % | **−37.8 %** | 257 → 250 MiB |
| amber-d | 2404 / 2428 ms | −11.7 % | **−54.6 %** | 304 → 304 MiB |
| photo-d1 / d1-e3 / screen-d1 | 182 / 213 / 275 ms | −7.5 … −11.5 % | −14 … −19 % | unchanged |
| photo-ll-e9 | 460 / 465 ms | unchanged | **−65 %** | 40 → 53 MiB |
| gray-ll | 144 / 149 ms | unchanged | **−69 %** | 25 → 26 MiB |
| alpha16-ll (4096² lossless RGBA) | 1328 / 1347 ms | unchanged | **−41 %** | 591 → 636 MiB |

At 1 thread every parallel site runs the original sequential code, so a
one- or two-core machine (the editor uses `min 4 (cores − 1)` capabilities)
gets the single-threaded column. Output is byte-identical to the previous
build on all 171 files at 1 thread and in repeated 4-thread runs. A kill
test interrupts real decodes at 199 points per file and checks the live
thread set; across the lossless files and ring-l it caught workers
mid-flight at 408 of them and none survived.

One experiment was backed out, and the reason is worth keeping: giving each
restoration tile slot reusable EPF ring scratch cut allocation by 60 MB per
wallpaper decode but **raised** peak RSS by ~8 MiB, deterministically.
Allocating less delayed a major collection, so dead large objects lived
longer. On this decoder, peak RSS follows the GC schedule and not the
allocation total, so a change aimed at memory has to be judged by measuring
peak RSS directly.
