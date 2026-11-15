# 0035 — JPEG, third pass: bit-exact with libjpeg-turbo, and fast

**Theme:** decode correctness and latency for `Cmedit.Image`'s JPEG decoder
(baseline and progressive Huffman, every sampling layout, gray/YCbCr/RGB/CMYK/YCCK)
**Status:** ✅ **RESOLVED 2026-11-15** (phase 1, and phase 2a: the scaled
decoder, D1, and the viewer's use of it, D2–D4; phase 2b, PDF page images,
is not started)

## Outcome

Every JPEG the decoder opens now decodes to **exactly** what libjpeg-turbo 2.1
produces (its default decode, the one Pillow ships), and does it 3–6× faster
on one core and 2.5–5× faster on four than HEAD `b4b465b`. Three old
correctness bugs are gone with the rounding differences.

| Measure | Result |
|---|---|
| Against Pillow (libjpeg-turbo) | **max 0** on all 142 corpus files (132 generated + 10 photos), display RGB **and** raw samples; max 0 on 112 more edge files (every layout at 1×1 … 17×1, 1101×901 through the parallel band path), decoded at -N4 |
| Was (HEAD) | max diff up to **255** (3x/4x factors, RGB-coded files), 209 on 4:2:2, 125 on 4:2:0; 25–99% of samples differing outside 4:4:4 |
| `make test` | **4644 passed**, 0 failed (4635 before; 9 new fixtures/rules); `make`, `make windows-check`: zero warnings. After the review fixes below: **4665** |
| Fuzz (ImageFuzz + SFuzz, 62 620 decodes × 2 at -N1 and -N4) | S4 output **identical line for line** to S3, corrupt-input pixels included; zero exceptions, zero timeouts; -N1 == -N4 everywhere. S3 against HEAD: identical Left/Right and every Left message |
| Kill test (40 kills × 8 files) / resume test (30 × 4) | no thread outlives its caller, 0.2 MB live after; 0 failed re-forces, 0 wrong hashes |
| `pdfimagecheck.sh` (run at S3; S4's output is identical) | every JPEG row better (`g_jpeg#1` max 57 → 0, `m_rgb_jpg`/`m_rgb_prog` 3 → 0, gray 1 → 0), nothing else changed; `pdfrendercheck.sh` unchanged |

**Wall time, median of 5, `imgbench.sh`** (fresh processes; the 1-core build
is non-threaded, the -N4 build is the editor's runtime):

| file | HEAD 1 core | **now 1 core** | HEAD -N4 | **now -N4** | Pillow RGB | Pillow RGBA | ImageMagick |
|---|---|---|---|---|---|---|---|
| big420 (6000×4753 4:2:0) | 2013 | **399** | 619 | **175** | 100 | 184 | 131 |
| cmyk (3000×2377) | 1507 | **256** | 534 | **114** | 95 | 102 | 94 |
| gray (4000×3169) | 402 | **91** | 131 | **42** | 50 | 54 | 98 |
| hd (1920×1080) | 238 | **45** | 89 | **22** | 22 | 28 | 17 |
| photo422 | 1075 | **202** | 347 | **81** | 63 | 87 | 69 |
| photo444 | 1271 | **239** | 418 | **116** | 71 | 92 | 83 |
| progressive (4000×3169) | 1109 | **368** | 596 | **235** | 136 | 160 | 145 |
| restart | 950 | **198** | 338 | **69** | 54 | 79 | 69 |
| vangogh (1280×1014) | 113 | **24** | 58 | **17** | 17 | 20 | 11 |
| wallpaper (4096² 4:4:4, 11 MB) | 2250 | **444** | 781 | **206** | 163 | 208 | 173 |

Pillow and ImageMagick from `/tmp/jpgcmp/run.py` re-run against these builds
(medians of 7; ImageMagick with process start-up subtracted). Pillow RGBA is the
fair comparison: we always produce RGBA, and its RGB→RGBA conversion is the
cost of producing four bytes per pixel. At four cores the decoder is under
Pillow-RGBA on seven of the ten files (behind on cmyk, photo444 and
progressive); at one core it is 1.2–2.6× it.
Allocation is down too (big420 136.8 → 114.1 MB, which is the output buffer
itself; wallpaper 105.7 → 67.1 MB), and peak residency is unchanged.

**Per stage, one core, minimum of 7** (`tools/Stage.hs` over an instrumented
copy; progressive's entropy is in its scan walk and is total minus the rest):

| file | entropy S3 → now | IDCT S3 → now | upsample+convert S3 → now |
|---|---|---|---|
| big420 | 176 → **75** | 133 → 120 | 186 → **156** |
| wallpaper | 405 → **154** | 167 → 173 | 72 → 67 |
| photo444 | 135 → **61** | 105 → 98 | 53 → **44** |
| cmyk | 150 → **63** | 91 → 85 | 197 → **63** |
| gray | 73 → **30** | 40 → 38 | 17 → 13 |
| restart | 90 → **41** | 56 → 56 | 74 → 70 |
| progressive | ~311 → **~166** | 55 → 55 | 73 → 68 |

## Measured first (Fable)

The plan started from a measurement of HEAD against Pillow 11.1 over a
132-file generated corpus (`jpeg_gen.py`) plus ten real photographs:

| layout | files | max | max excluding border pixels | samples differing |
|---|---|---|---|---|
| gray | 7 | 1 | 1 | 1.5% |
| 4:4:4 | 17 | 3 | 3 | 3.1% |
| 4:2:2 | 16 | **209** | 4 | 25% |
| 4:2:0 | 26 | **125** | 4 | 29% |
| 4:4:0, mixed | 9 | 165 | 3 | 26% |
| CMYK 4:4:4 / subsampled | 4 / 6 | 3 / 118 | 3 / 3 | 2.5% / 18% |
| 3x/4x factors | 24 | 255 | 255 | 50% |
| RGB-coded (Adobe transform 0) | 3 | 255 | 255 | 99% |

Three of those were bugs, not rounding:

1. **The first row and column of every subsampled picture were wrong**
   (vangogh.jpg by 97): the upsampling tables clamped `floor(-0.25)` to 0 but
   kept the fraction 0.75, so pixel 0 took three quarters of its *right*
   neighbour.
2. **The right and bottom edges interpolated into MCU padding**, not the
   component's true width.
3. **A three-component RGB JPEG** (Adobe transform 0, or ids `R`,`G`,`B`
   without JFIF) was converted as YCbCr.

And the bar turned out to be reachable everywhere: `jpeg_ref.py`, libjpeg-turbo's
default decode written as ~150 lines of Python from the algorithm descriptions,
was bit-identical to Pillow on 420 of 420 runs (all 114 baseline corpus files
at full size and 306 scaled runs through `Image.draft`). On the way it
confirmed what Pillow actually does: ISLOW and fancy upsampling by default;
merged upsampling never while fancy is on; fancy for h2v1/h2v2 only when the
downsampled width exceeds 2, for h1v2 always, and off at 1/8; context rows
clamp to the true component height; scaled decodes enlarge chroma in the IDCT
and do not upsample at all. GHC prototypes put an integer ISLOW at 247 ns per
dense block and integer h2v2 plus table colour at ~290 ms for 6000×4753.

## The bar

Bit-exact with libjpeg-turbo 2.1 (x86 SIMD build, as Pillow ships) for every
complete 8-bit Huffman JPEG libjpeg accepts. No "±1 elsewhere". Outside it:
fractional sampling ratios (libjpeg refuses them; we replicate), truncated or
corrupt streams (libjpeg block-smooths a truncated progressive file; we do
not), non-Adobe four-component files (we keep our rule). Range limiting
**saturates** (the SIMD behaviour) rather than wrapping (`& RANGE_MASK` in the
C code); the two differ only on absurd coefficients. Output stays identical
between -N1 and -N4.

**Provenance.** CMeDit is GPLv3 and written from first principles, so nothing
here transliterates libjpeg or libjpeg-turbo source. Matching their output
means making the same *mathematical* choices, which are facts about an
algorithm, and each kernel cites its source: the IDCT is Loeffler, Ligtenberg
and Moschytz (ICASSP 1989), separable, constants `floor(c·2^13 + 0.5)`, two
extra fraction bits after the column pass, descales `(x + 2^(n-1)) >> n`; the
colour equations are JFIF 1.02's in 16-bit fixed point; upsampling is the
triangle filter for centred chroma with alternating rounding offsets; entropy
decoding is T.81 Annex F (DECODE, RECEIVE, EXTEND) and Annex G. `jpeg_ref.py`
is that specification in executable form, checked against Pillow, not derived
from source. `README.md` and the `Cmedit.Image` header carry the IJG
acknowledgement ("This software is based in part on the work of the
Independent JPEG Group").

## What changed, step by step

**S0, harness.** `jpeg_gen.py`, `jpeg_cmp.py` (per-file max/mean/share, exit 1
on any difference; `--raw` compares samples, i.e. Pillow's CMYK array, which is
255 − stored for Adobe files), `jpeg_ref.py`, `jpegcheck.sh corpus|run`;
`ImageDecode.hs` gained `IMAGE_RAW=1`. HEAD's diff table above reproduced
exactly before anything changed.

**S1, IDCT.** The Double IDCT is gone. `idct8` is the LL&M transform in CPS,
inlined; `jIdctCols` dequantises and runs the columns into an `Int` workspace
(descale 11, zero-AC column shortcut), `jIdctRows` runs the rows into the ring
(descale 18 with the +128 folded into the bias, saturate). A block is
classified by reading its 128 bytes as words: rows 1–7 zero means every column
is constant, so one row transform is copied to eight rows; with row 0's AC
zero too the block is one `Word64` fill. All kernels take bare byte arrays, so
workers get unboxed arguments. big420's IDCT stage 934 → 120 ms.

**S2, colour.** `jYccTab`, 4×256 entries of 16-bit fixed point. The
coefficients must be JFIF 1.02's five-digit values (0.34414, 0.71414): T.871's
0.344136 rounds to 22553 at 2^16, not the 22554 libjpeg uses. libjpeg's
three-component rule (`jpegColourMarkers`: JFIF APP0 with the full fixed part,
first Adobe APP14) gives a new `JViewRgb`. The multiplicative CMYK → RGB was
kept: it is provably identical to Pillow's `nk − MULDIV255(c, nk)`, since
`round((255−c)(255−k)/255)` never has a .5 fraction.

**S3, upsampling.** The Double tables, `jSample` and the row caches are gone.
`jUpFor` picks per component, once, by `jpeg_ref.py`'s selector: full | h2v1 |
h2v2 | h1v2 | box, with true downsampled sizes; kernels write a `Word8` row
buffer (libjpeg rounds to 8 bits before colour conversion, so `Word8` is
required, not chosen). **Halos are now only for components that filter
vertically** (`juHalo`: h2v2, h1v2), so 4:2:2 bands no longer reconstruct
chroma halos. The band and streaming structure (`mkConv`, `runRange`,
`CoefSrc`, `jpegStreamable`, `parOrderedResumable`) is unchanged. All three
bugs are gone and every corpus file is max 0.

**S4, entropy (pure speed; output frozen at S3's).** The old reader kept its
state in an unboxed `STUArray` and read and wrote it several times per
coefficient; through store-to-load forwarding that was most of 160 ms on
big420 and 405 ms on the 11 MB wallpaper. `jScanRows` replaces the whole
per-block path for **every** scan kind (baseline, DC/AC first, DC/AC
refinement):

- **The reader state is loop arguments.** Position, a left-aligned 64-bit
  accumulator and its bit count are carried through one nest of join points
  (rows → units → blocks → the coefficient loop) for a whole `decodeRows`
  call and written back to the state array once, with the EOB run and the
  restart count. A scan is described once by a `JScan` (per unit block:
  component, buffer offset as `(row − base·rowScale)·rowMul + off +
  unit·step`, tables), which covers single-component and interleaved scans
  with one loop. The STG shows every loop as `let-no-escape`; allocation per
  decode equals the output buffer.
- **Refill eight bytes at a time.** When none of the next eight bytes is
  0xFF (`jHasFF`, the exact has-zero-byte test on the complement), the word
  is OR-ed in at once and 5–8 bytes are credited; otherwise a byte loop
  handles stuffing and stops at a marker. The bytes beyond those credited
  are the same stream bits a later load puts there, so OR-ing them twice is
  harmless; nothing past a marker is ever loaded, so the bits past it read as
  zeros for free, and the count is set to `jHuge` so the reader stops looking.
- **A 10-bit lookahead that carries the value** (`JFast`): each entry packs
  code length, symbol and, when code and magnitude bits fit, the extended
  coefficient, so most coefficients cost one table read. Longer codes take
  the canonical walk over a 16-bit window, exactly as before.
- **It reproduces the bit-serial reader exactly, corrupt input included.** The
  argument is in the module: the bit stream (data, then zeros) is identical,
  every decode consumes the same bits, and the byte position stays between the
  consumed data and the next marker, which is all the restart and
  segment searches look at. The odd cases keep their old meaning: an AC
  symbol that is an EOB run inside a baseline scan, a DC category over 16 (read
  in 16-bit pieces with the old wrap-around), a code that matches nothing
  (symbol 0, 16 bits). The fuzz lists, which compare output pixels on corrupt
  input, are identical line for line.
- Two reconstruction wins rode along: **`cvtFour`** (cmyk's convert 197 →
  63 ms) drops its three `quot 255` per pixel for `div255`, exact on all 65 536
  byte products (checked), with one loop per flag combination; and **every
  row converter stores a pixel as one `Word32`** (`wrPx`, bytes in memory
  order whatever the host's byte order), 10–15 ms on big420.

Targets were big420 entropy ≤ 120 ms (now 75), wallpaper "as low as you can"
(405 → 154), progressive "substantially down" (~311 → ~166).

**S5, close-out.** Spec gained five fixtures (20×14 crops of `jpegColour420`'s
picture at q50, so no layout's MCU divides them: 4:2:2, 4:4:0, 3x2 replicated,
RGB-coded with Adobe 0 and ids R G B, YCCK with C and K at 2x2) and three
colour-rule cases built by marker surgery (JFIF beside Adobe 0 is YCbCr; ids R
G B with no marker is RGB; ids R G B under JFIF is YCbCr), each with
libjpeg-turbo's fingerprint embedded. `jpeg_fixtures.py` regenerates them and
prints the fingerprints, so Spec itself never needs Pillow.

## Decisions worth keeping

- **The oracle is the bar, not the previous hash.** For JPEG, a change is
  correct when `jpegcheck.sh run` says `max 0` on the corpus, raw and display.
  Hash-equality with the previous build is only the S4-style check that a
  speed change changed nothing.
- **Corrupt input is data.** The entropy path runs ahead of the bands on the
  calling thread, so it must never throw: an exception's ordering would then
  depend on the capability count. Everything odd decodes to something.
- **Rounding before conversion is libjpeg's**, which is why the chroma row
  buffers are `Word8`.
- **One decoder for every scan kind.** Keeping the old bit-serial path for
  refinement would have left progressive at twice the time; its loops reuse
  the first-scan reader and are one bit per step with the refill at the head.

## Rejected experiments

Each measured, with medians and minimums over several alternated runs (the
machine was shared; stage noise was ±10%, so only differences beyond that
were believed).

- **Lookahead width 9 or 11 instead of 10:** within 2% on every file, either
  way. 10 kept.
- **An unaligned 64-bit load for the refill** (`peekByteOff` through
  `unsafeUseAsCString` and a byte swap) instead of eight byte reads: **slower**,
  84 vs 74 ms on big420's entropy, 170 vs 150 on wallpaper. The
  `withForeignPtr`/keepAlive frame per refill costs more than the reads it
  saves. (Copying the input into a padded `ByteArray#` as `JxlBits` does would
  avoid that, at the cost of a file-sized copy; not needed.)
- **A paired 2x2 kernel** making both output rows of a chroma row in one pass,
  reading the middle row once: no faster (177–188 vs 164–180 ms for
  upsample+convert on big420); five loop-carried values cost what the read
  saved. The single-row kernel stays, and `juNeed` keeps its parity rule.
- From S0–S3: the multiplicative CMYK formula versus Pillow's MULDIV255 form
  (identical, so no change); halos for 4:2:2 (not needed, removed).

## Where the time is now

On one core the decoder is 2–3× Pillow-RGBA. The remainder is spread: the
IDCT (120 ms on big420, about 180 ns a block on average, against
libjpeg-turbo's SIMD), the converters, including the first-touch page faults
of a 114 MB output, and the entropy decoder (75 ms there). At
four cores the entropy decoder is the floor (it runs on the calling thread),
which is why progressive, whose scans must all finish before reconstruction,
is the one file still behind Pillow. Restart-interval parallel entropy
decoding was deliberately not planned.

## Review fixes (Fable's review of phase 1)

Two blocking findings, five minor ones, and two things found while fixing
them. Nothing here moved a single pixel of a valid file the corpus already
had; what it moved is listed under the fuzz gate below.

- **Progressive + restart lost scans.** After a scan's last unit `tick` ran
  the restart search whenever the interval divided the unit count, but no
  encoder writes an RST there, so `findRST` skipped the next scan's header
  and landed on that scan's first RST: the scan was gone (max diff 108–255
  against Pillow for `progressive=True` with `restart_marker_blocks=1` or
  `restart_marker_rows=1`). There is no search after the last unit now.
  New: Spec fixture `jpegProgRst` (from `jpeg_fixtures.py`), and three
  `*-prog-rst*` variants per source in `jpeg_gen.py` (corpus 132 → 141).
- **Quantisation tables latch at a component's first scan**, libjpeg's
  `latch_quant_tables`. Found re-examining the Spec pin that rested on the
  restart quirk: Pillow ignores a DQT after the scan *and* between
  progressive scans, while our whole-image path dequantised with the tables
  as they stood at the end of the walk (`edge/dqtafter*.jpg`: max 115/151
  before, 0 now). Both pins now say "ignored", and a third pins the
  between-scans case.
- **`jpegStreamable` with restarts.** It was "no `FF DA`/`FF DB` pair after
  the scan header". With latching, a DQT no longer matters, and the restart
  argument can be made exact: the walk resumes at M (the first non-RST
  marker after the scan) unless a restart search passes M, and then it lands
  past R, the first RST pair at or after M. So the rule is now "no SOS on
  the walk from M, and with a restart interval no `FF DA` pair after R",
  which contains the old rule. Checked by an equivalence fuzz (SFuzz's
  structural mutations, a build that can force the whole-image path):
  60 900 mutants of 66 inputs, with and without DRI; 35 961 streamed, and
  each was decoded streaming and whole-image, display and samples:
  **0 differences**; 3 395 stream only under the new rule, and none
  streamed under the old rule but not the new. Valid restart files streamed
  before and still do.
- **Scan headers follow `get_sos`** (`jpegScanComps`): at most four
  selectors, each naming the first component with its id not yet named.
  255 selectors over four components sharing an id had made 1020 blocks an
  MCU at 6300² (`z_dos_one_scan.jpg`, 657 bytes): `jScanRows` does not
  allocate, so a kill never landed in that scan. Now 1.1 s per decode, and a
  300 ms timeout lands within 0.5 s. A frame whose ids are all 1 now decodes
  as libjpeg does (it was garbage).
- **Scan limits** (`jpegScanWork`, from `jpegFrame`, before any decoding):
  more than `maxJpegScans` = 1000 scans, or more than `jpegVisitsPerBlock` =
  64 block visits per frame block (+ 1M), is a `Left`. libjpeg's default
  script makes ten scans and visits a luma block six times, so 64 is ten
  times the busiest common script; the scan cap alone would still let 999
  scans take minutes. Counting every `FF DA` pair bounds whatever the walk
  can reach, so refusing is instant (`z_dos_scans.jpg`/`z_dos_refine.jpg`,
  2000 scans in 20 KB at 6000²: minutes before, 13 ms now).
- **Background loading.** `App.asyncThresholdFor` sends a JPEG whose frame
  is over 4 MP, or that has more than one scan, to the background loader at
  any size (`Image.jpegCostHint` over the first `imageAsyncBytes`). One
  scan of 4 MP is at most about one decode's work (0.1–0.2 s here even for
  a hostile four-component file), which the inline path can afford; with
  more scans the work limit still allows seconds at that size
  (`z_dos_20scans.jpg`: 336 bytes, 20 scans at 6300², 3.3 s, killable
  between scans).
- Minor: four components with *any* non-zero Adobe transform are YCCK (the
  display path said `== 2`; the samples path already said `/= 0`); APP14
  needs a 14-byte segment to carry a transform and the last one wins;
  `decodeJPEGSamples` leaves ids `R`,`G`,`B` without a marker or override
  untransformed (pdf.js, libjpeg); a sample precision other than 8 is
  refused; stale comments about the deleted bit-serial reader; CLAUDE.md's
  GHC version (9.6.6).
- **A performance trap found on the way:** all of the above made the
  decoder 4–8% slower on one core and ~13% at -N4, entirely in the entropy
  stage. `jpegScanComps`, called twice inside `decodeJpegST` (scan setup,
  inlined `jpegStreamable`), was compiled out of line, and passing `bs` to it
  made GHC's boxity analysis (9.4+) keep the ByteString boxed in
  `$wdecodeJpegST`'s worker, so every byte read in the inlined `jScanRows`
  paid for it. It is `INLINE` now and the worker takes the unboxed fields
  again; timings are back within noise of the build before the fixes. Any
  new out-of-line call from `decodeJpegST` that takes `bs` will do this
  again: check the worker's arguments (`-ddump-stg-final`) after touching it.

Gates: `jpegcheck.sh` max 0 on 293 files (10 photos, the 132-file corpus,
`edge2`, the review's `pr_*`/`b_*` files, the new progressive+restart files,
the fixtures and the colour-marker variants), display and `--raw`, 1-core and
-N4 builds; hashes identical at 1 core, -N1 and -N4 on those plus the
short-APP14 variants (Pillow inverts every CMYK file; those are the
non-Adobe case outside the bar). Fuzz (ImageFuzz + SFuzz, the same 62 620 ×
2 decodes): 0 exceptions, 0 timeouts, -N1 == -N4; outcome changes are only
the precision check (5 mutants Right → Left, 7 Left messages now name the
precision); pixel changes, all attributable: SFuzz 1 591 mutants with a DQT
after a scan (the latch), 204 with restarts and a later scan, 92 with
selector changes, 1 with an Adobe transform of 49 (now YCCK); ImageFuzz 26,
all mutants of the two DQT-after-the-scan files (the latch). Kill and resume tests pass (now also on the DoS files and a
progressive+restart file). `make test` 4665 passed (21 new).

## Phase 2, D1 (done): the scaled decoder

`decodeJPEGScaled den` (den 1, 2, 4, 8) decodes at ceil(W/den) x ceil(H/den),
**bit-exact with libjpeg-turbo's scaled decode**; `decodeJPEGScaled 1` is
`decodeJPEG`, byte for byte. `jpegDims` reads the full-size frame dimensions
from the headers. Nothing outside the reconstruction changed: entropy
decoding, the coefficient buffers, `mkConv`/`runRange`/`CoefSrc`,
`jpegStreamable` and both parallel paths run as before at every scale.

- **Kernels.** `jIdctBlock4`/`jIdctBlock2` and a one-sample loop beside
  `jIdctBlock`, dispatched once per block row on the component's `ds`. Each
  reduced transform is the 8-point LL&M IDCT followed by a box sum over g
  outputs (g = 8/N), folded into one N x 8 matrix per axis (DCT-domain
  downsampling, Merhav and Bhaskaran 1997); every weight is
  `round(2^13 sqrt2 sum cos((2m+1)k pi/16))` over the group, which
  reproduces each constant in `jpeg_ref.py` (checked numerically), and the
  descales are idct8's plus log2 g per pass. Frequency 4 drops out at N = 4,
  2, 4 and 6 at N = 2, everything but the DC at N = 1. The same exact
  zero-column, zero-row and DC-only shortcuts as at full size.
- **Geometry.** `jUpFor` takes the scale: ds starts at 8/den and doubles while
  both expansions to the output are even (so 4:2:0 at 1/2 is luma 4x4 plus
  chroma 8x8 and no upsampling), the triangle filter is off at 1/8, and the
  method is re-derived from the scaled ratio. Everything output-shaped in
  `decodeJpegST` (row buffers, conversion widths, band rows) uses ow/oh.
- **Size limits apply to the output.** `jpegFrameScaled`: the output must pass
  `checkDims` (its errors name the file's size); for den > 1 the source may
  be up to 4 x `maxPixels` and 2 x `maxDim` a side when it decodes streaming,
  but keeps the full-size pixel limit otherwise, because the whole-image path
  holds every coefficient and decodes each scan in one call a kill cannot
  interrupt. **`decodeImage` picks the scale** (`decodeJPEGFit`): full size when
  it fits, else the first of 1/2, 1/4, 1/8 that does, from the header alone,
  so a 54 MP photo opens at 1/2 instead of being refused. `decodeJPEG` stays
  full size, and the PDF path (`decodeJPEGSamples`) is untouched.
- **No bands at 1/8**: a block is one read there, so the entropy decoder is
  the whole cost (big420: 84 ms either way, 18 MB more peak with bands).

| gate | result |
|---|---|
| vs Pillow at 1/2, 1/4, 1/8 (`jpegcheck.sh --scale`; draft, draft at the floored size, or the same decoder config set directly for sides under N) | **max 0** on all 254 files (photos, 132 layouts, edge2) x 3 scales, 1-core and -N4 builds |
| scale 1 | max 0 display and raw; hashes identical to HEAD `ec8265b` on 302 files, display and raw |
| -N1 vs -N4 vs non-threaded | identical at every scale (302 files), and on scaled fuzz output |
| fuzz (SFuzz + ImageFuzz mutators, 2216 mutants at 1/2 and at 1/8, each also at 1/1) | 0 exceptions, 0 timeouts; Left/Right and every Left message identical to scale 1 |
| kill test at 1/2, 1/4, 1/8 (10 files incl. a 54 MP photo, a 156 MP frame, the DoS files) | OK: no thread survives, 0.2 MB live after; worst kill latency unchanged from scale 1 (z_dos_one_scan 0.5 s either way) |
| `make test` | 4723 passed (58 new: 12 fixtures x 3 scales, scale-1 identity, `jpegDims`, the size rules); zero warnings; `windows-check` clean |

Wall time (median of 9, ms; peak MB in parentheses):

| file | 1 core 1/1 | 1/2 | 1/4 | 1/8 | -N4 1/1 | 1/2 | 1/4 | 1/8 | Pillow 1/1, 1/2, 1/4, 1/8 (RGB) |
|---|---|---|---|---|---|---|---|---|---|
| big420 | 344 (125) | 172 (40) | 116 (18) | 90 (11) | 132 (163) | 99 (75) | 87 (52) | 86 (26) | 88, 59, 48, 37 |
| wallpaper | 381 (85) | 245 (35) | 195 (23) | 166 (17) | 169 (121) | 166 (63) | 167 (53) | 171 (32) | 145, 124, 115, 86 |
| progressive | 277 (101) | 198 (63) | 173 (50) | 160 (50) | 203 (117) | 171 (78) | 165 (65) | 162 (66) | 113, 101, 95, 91 |
| photo444 | 201 (61) | 122 (24) | 87 (10) | 64 (10) | 75 (94) | 73 (53) | 71 (47) | 65 (26) | 62, 45, 39, 29 |

At four cores every scaled decode sits on the entropy decoder's floor
(75 ms on big420, 154 on wallpaper, ~160 for progressive's scan walk), so
D1's gain there is memory, and the time win is on one core. At 1/1 the
decoder is within 1% of HEAD except progressive at one core (+2%,
allocation identical, unchanged by removing the block-size dispatch:
layout, not a code path).

## Phase 2, D2–D4 (done): scaled decode for the viewer

A placement never exceeds `maxGfxPixels` (1.2 MP), yet a 28 MP photo was
decoded whole, and then sampled whole on the UI thread for every cell grid
and every placement. Now the image view opens a large JPEG at a reduced scale
and refines it in a background job only when a zoom needs the pixels.

**D2, the model.** `ImageDoc` gained `idFull` (the file's size), `idDen` (the
decode is the file at 1/den), `idEpoch` (from `tickVecClock`; `mkImageDoc`
takes it, so no installer can forget one), `idDenStuck` and `idRefines`.
`imageDims` returns `idFull`, so `idCrop`, `viewFit`, `wheelZoom`,
`cellRectToCrop`, `imageFitCap` and `gfxFit` are untouched and stay in
full-resolution coordinates. `EditorState.pictureSample idoc crop` is the one
translation: each crop edge maps to the nearest decoded pixel boundary *by
the scale* (decoded pixel i covers source `[i·den, (i+1)·den)`; an edge at the
picture's own maps to the decode's, whose last pixel is partial), with at
least one pixel each way. (It first mapped proportionally to the two sizes,
which drifts by up to a pixel towards the far edge; review fix.) Its callers are `renderPicture`, which fits in
full coordinates and samples the decode with `renderImagePlaced` (used by
`refreshImage` and `Render.drawImage`), and `App.placeGfx`. A kitty payload
is never larger than the decoded crop, because the terminal scales it into
the cell box. Sixel draws 1:1, so it keeps the fitted size and is upsampled.
Loaders hand over a `Decoded` (frames, full size, den, mtime), and
`OutImage` carries it. The status bar prints `idFull`.

**D3, the opening scale.** `jpegOpenDen full` is the largest den whose output
still has at least `maxGfxPixels` pixels, and never finer than
`Image.jpegFitScale` (the limits) allows: big420 opens at 1/4 (1500×1189),
wallpaper at 1/2, anything under 4.8 MP whole. It depends on the file alone,
because cell-size and capability replies arrive after the first frame. The
two other paths need less. The half-block picture is two sub-pixels per
cell, about 60 000 pixels on a 300×100 terminal. Sixel is fitted by the same
`gfxFit` under the same cap as kitty. So one rule covers all three. A stream
(a pipe) cannot be re-read, so it decodes at `jpegFitScale` as before, and is
marked `idStream` so that nothing ever asks to re-read it. The opening scale is
*recorded* (`idBaseDen`, from `dcDen`): parking first recomputed it as
`jpegOpenDen`, which sent a piped photo opened at 1/1 to 1/4 on its first tab
switch, after which its refinement re-read a drained pipe (stuck) or a FIFO
(blocked for good). Other formats are untouched.

**D4, the refinement.** `imageNeedDen` is the largest den whose decode still
holds, in the crop, the pixels the view shows, within 1/16 so that a cell
rounding never costs a decode. On a pixel terminal that is the payload
`gfxFit` wants, *whether or not a menu covers it*, because opening a menu must
not cancel a decode. Elsewhere it is the half-block box. `imageRefineWant`
names `ImgWant (path, epoch, den, full, mtime)` when that is finer than `idDen`,
the picture is not stuck, and it is under `maxImageRefines` (2) for this
visit. `App.imageUpkeep` runs beside `vectorUpkeep` at the top of every
`renderNow`. It starts `JImgDecode` unless that want is in flight
(`drvImgDecode`, written once the job has started and cleared only by its
own result), and cancels it when nothing is wanted. The job runs under
`guardJob` and `trySync`. It re-reads the file (the view holds no bytes),
refuses a file whose mtime is not the one the picture opened with (checked
before and after the read; a same-size rewrite would otherwise install the new
file's pixels into the old picture) or whose frame is another size, decodes,
and forces the pixels.
`imageRedecoded` installs only for the same path and epoch, a finer den, the
same full size, and pixels of exactly that scale's size. It replaces
`idImage` *and* `idFrames`, so the coarse decode is released. A failure or a
size mismatch sets `idDenStuck` with a status note and is not retried. A
later, coarser result is ignored. `GfxKey` gained `gkDen`, so the finer
decode re-places, and the coarse placement stays up until then. While the
view shows fewer pixels than it could, the status zone says `(1/4 scale)`,
and it says nothing at the fit.

Three decisions beyond the design:
* **A document going inactive parks its refinement.** `parkDoc`, used at
  every zipper push (`pushActive`, `nextFile`/`prevFile`, `switchToFile`, and
  `rebuildZipper` for journal recovery and session snapshots), box-
  reduces a refined picture back to its opening scale (`Image.reduceImage`,
  a word-at-a-time k×k mean) and resets the count. It is strict in the result,
  so the 114 MB goes at the switch, not whenever something next reads the
  document. The crop is kept, so returning re-asks for the finer decode, and
  the coarse picture shows meanwhile. That is why the cap is per *visit*:
  per-document would leave a photo permanently coarse after its second
  return. Cost: 76 ms once, when leaving a refined 28 MP picture.
  Not done: keeping the refined decode, because ten zoomed photos would be
  over a gigabyte. Also not done: keeping the coarse decode beside the fine
  one, because the brief asked for it to be released.
* **Pictures record their file's mtime** (`dcMtime` → `edDiskMtime`). They
  used to record none, so the freshness poll never marked a changed image
  and Revert was unreachable for one. Now ◆ and the "changed on disk" notice
  appear, and Revert reloads under a new epoch, which is the path this step
  has to be safe against.
* **The samplers were rewritten** (output identical): `scaleRGBA` walks
  each box once for all four channels into an `unsafeCreate` buffer, and
  `renderImagePlaced`'s box sum is strict with unchecked reads. Without
  this the 1/4 decode still left 290 ms between the capability reply and
  the first placement: 100 ms of cell grid and 190 ms of payload, for a
  1.8 MP source. After: 15 ms and 19 ms. At 1/1 the same work went from 2.4 s
  and 1.6 s to 145 ms and 86 ms.

Measured by `docs/plans/bench/pty_jpegview.py` (fake kitty, 120×40 cells of
8×16 px; HEAD `ec8265b` built separately):

| big420, 6000×4753 | HEAD | now |
|---|---|---|
| spawn → first kitty placement | 4099, 4399 ms | 170–173 ms |
| spawn → half-block picture (no replies) | 2456–2718 ms | 165–167 ms |
| VmRSS once idle (an empty editor: 46 MB) | 188 MB | 76–79 MB |

The live heap after opening is about 12 MB, 7.1 MB of it the 1/4 decode
(`-hT`). The rest of the resident set is memory the RTS keeps from the
decode's transient peak. The refinement lands 160 ms after a wheel burst
(1/1, four capabilities). Its placement equals a full Pillow decode of the
crop, area-averaged as `scaleRGBA` does, with a maximum difference of 0. The
crop is predicted by replaying `wheelZoom`, and the prediction is checked
against the payload size it implies. RSS is 197 MB while refined. A Revert
issued while the refinement runs places nothing of the old file: across runs,
the job was either cancelled or its result was dropped by epoch, and the new
file then refines to its own pixels with a difference of 0. `pty_pdf.py` and
`pty_svg.py` pass, and Spec went from 4723 to 4778 tests.

## Files

`src/Cmedit/Image.hs` (the decoder; `reduceImage`, the samplers),
`src/Cmedit/{EditorState,EditorDoc,EditorEdit,Render,App}.hs` (D2–D4),
`docs/plans/bench/pty_jpegview.py`, `test/Spec.hs` (fixtures, fingerprints,
colour rules), `docs/plans/bench/{ImageDecode.hs,jpegcheck.sh,jpeg_gen.py,jpeg_cmp.py,jpeg_ref.py,jpeg_fixtures.py,README.md}`,
`CLAUDE.md` (the Image view bullet), `README.md` (acknowledgement).
