# 0035 — JPEG, third pass: bit-exact with libjpeg-turbo, and fast

**Theme:** decode correctness and latency for `Cmedit.Image`'s JPEG decoder
(baseline and progressive Huffman, every sampling layout, gray/YCbCr/RGB/CMYK/YCCK)
**Status:** ✅ **RESOLVED 2026-11-15** (phase 1; the scaled decode of phase 2 is
designed below and not started)

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

## Phase 2 (designed, not started): scaled decode for the viewer

A placement never exceeds `maxGfxPixels` (1.2 MP), yet a 28 MP photo is
decoded whole. `decodeJPEGScaled den` (1/2, 1/4, 1/8 by reduced IDCTs, which
libjpeg-turbo does without upsampling) would open big420 at 1/4 (1.78 MP),
~8 MB of pixels instead of 114, and refine to full resolution in a background
job only when a zoom crop needs it (`idFull`/`idDen` in `ImageDoc`, one
`pictureSample` helper for the three samplers). `jUpFor` is where it enters:
`ds` is the only constant to vary, and fancy upsampling must turn off at 1/8.
`jpeg_ref.py` already specifies the scaled kernels and is exact against
`Image.draft`.

## Files

`src/Cmedit/Image.hs` (the decoder), `test/Spec.hs` (fixtures, fingerprints,
colour rules), `docs/plans/bench/{ImageDecode.hs,jpegcheck.sh,jpeg_gen.py,jpeg_cmp.py,jpeg_ref.py,jpeg_fixtures.py,README.md}`,
`CLAUDE.md` (the Image view bullet), `README.md` (acknowledgement).
