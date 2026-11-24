# 0036 — PNG, third pass: in place, from slices, in parallel and from a stream

**Theme:** PNG decode latency and peak memory, in the decoder and in the
editor, without changing one output byte
**Status:** ✅ **RESOLVED 2026-11-25** (main `2983c62`, plus `inflateDyn`
in `9dc95ab`)

## Outcome

The rule for every change was **faster *and* no higher peak memory**, with
output byte-identical to the original decoder (`4f3c80e`) at every
capability count and corrupt input decoding to exactly what it decoded to
before.

| Measure | Before (`4f3c80e`) | After | Change |
|---|---:|---:|---:|
| Decode time, 16-file corpus, `-N4` (harness) | 2845 ms | 414 ms | **−85.5%** (6.9×) |
| Peak memory summed over the 16 files (harness) | 900 MB | 786 MB | −12.7% |
| Allocation during decode | 935 MB | 377 MB | −60% |
| Editor, first picture, 7 large PNGs, kitty | 3514 ms | 818 ms | −77% |
| Editor, first picture, same, no replies | 3087 ms | 547 ms | −82% |
| Editor peak (`VmHWM`) summed, kitty | 776 MB | 639 MB | −18% |
| Editor peak summed, no replies | 740 MB | 623 MB | −16% |
| `inflateDyn` (ZIP/PDF/`.svgz`), ai4096 stream | 460 ms, 247 MB | 96 ms, 145 MB | −79%, −41% |

**Every large file got faster and smaller** in the editor, so the rule held
file by file, not only in total:

| PNG | Terminal | First picture | Peak |
|---|---|---|---|
| ai4096 (4096², 21 MB) | kitty | 1015 → 201 ms | 151 → 112 MB |
| big6000 (6000×4000) | kitty | 1170 → 249 ms | 181 → 144 MB |
| gpt2480 | kitty | 468 → 102 ms | 98 → 79 MB |
| pal4096 (palette) | kitty | 337 → 104 ms | 120 → 112 MB |
| screen3840 | kitty | 192 → 51 ms | 73 → 61 MB |
| rgba_tall | kitty | 211 → 59 ms | 85 → 78 MB |
| photo | kitty | 121 → 52 ms | 68 → 53 MB |

**Against other decoders**, same corpus, decode only, medians of 3:

| Decoder | Time | Peak, summed |
|---|---:|---:|
| ours, 4 capabilities | 413 ms | 634 MB |
| ours, 1 capability | 731 ms | 626 MB |
| Pillow 11.1 (zlib) | 1432 ms | 589 MB |
| ImageMagick 7.1 Q16 (libpng; whole process) | ~1325 ms | 902 MB |
| libpng via `pngtopnm` (includes writing PNM) | 2768 ms | 336 MB |

Single-threaded we are about 1.8× ImageMagick and 2× Pillow. Pillow's
lower total is almost all pal4096, which it keeps at one byte a pixel
(34 MB against our 78 MB); that, and RGB storage for opaque pictures, is
plan `0037`.

## Method

Three implementation agents worked in separate worktrees in rounds (Neon:
the DEFLATE core and the pipeline; Fable: unfilter, conversion and
parallelism; Opus: memory along the editor's real open path), and every
candidate was then **re-measured by the coordinator alone, one idea at a
time, serially**, under an exclusive benchmark lock, against the untouched
baseline: agents' numbers were never taken on trust, and two agents'
results never shared a run. Gates for each commit:

1. **Hashes:** the 16-file corpus plus 356 generated files (312 edge cases:
   bit depths, palettes, tRNS, interlacing, filters, odd sizes; 33 that
   exercise each parallel path; 11 large) at `-N1`..`-N4`, with and without
   `-qm`, identical to `4f3c80e`. Generator:
   `docs/plans/bench/gen_png_sets.py`.
2. **Fuzz:** `ImageFuzz.hs` outcome lists over 2 × 300 seeded mutants, plus
   80 mutants each of two large files that reach the pipeline, identical.
3. **Kill tests:** `docs/plans/bench/PngKill.hs` (interrupt real decodes at
   many points, compare the live thread set, check the resumed result) and
   `PngStreamKill.hs` (the same during a streamed open). Several thousand
   kills over the work, 0 bad, including 4 capabilities squeezed onto 1–2
   cores with `taskset`.
4. **Harness time and peak:** `imgbench.sh build-threaded`, `measure.py`
   medians, alternating builds.
5. **Editor time and peak:** `docs/plans/bench/pty_png.py BIN RUNS FILE…`
   (first-picture time polled every 3 ms, and `VmHWM`), on a kitty-replying
   terminal and on one that answers nothing. The harness cannot see nursery
   placement or render allocation, and twice it called a change neutral that
   the editor showed was not.
6. `make test` and `make windows-check`.

## What changed, by round

**Round 1: the DEFLATE core (Neon) and the row pass (Fable).** Measured
alone: Neon's inflate −31.8%, Fable's unfilter/convert −37.4%; together
**2750 → 793 ms (−71%)**, more than the sum. Opus's competing inflate
measured −15.3% and was not taken (`png-r1-opus`).
- Inflate (`13ef5d6`…`063d3b2`): a register-resident 64-bit bit buffer;
  allocation-free dynamic table building into reusable `STUArray`s; one
  combined two-level literal/length and distance table, decoded by a leaf
  `huffFastLoop` that keeps all state in arguments and walks `Addr#`
  cursors; up to four short literals packed into one 64-bit root entry;
  word-at-a-time match copies (distance ≥ 8 by words, 1/2/4 by broadcast,
  3/5/6/7 by doubling); a two-step literal/match dispatch per refill.
- Rows (`36658c9`, `5bac1dd`…`8eaf721`): unfilter in place, no scratch row;
  a branch-free Paeth with neighbours in registers (a multiply spread for
  RGB); 8-bit RGB/RGBA rows straight from filtered bytes to RGBA; palette
  and grey pixels written as one word; sub-byte palette a packed byte at a
  time.

**Round 2: memory.** Peak summed 894 → 802 MB, time unchanged.
- **No IDAT copy** (`061f241`, Opus's idea, rebuilt by Neon on its
  inflate): `pngChunks` returns IDAT payloads as slices of the file and
  `inflateIntoSegs` reads them as segments, crossing a boundary with the
  same bit semantics and zero padding as one contiguous input. About −21 MB
  on each of the two largest files.
- **Streamed open** (`9a3cd77`, `d9a41aa`, `4ebb3cc`, Opus): a regular PNG
  of 8 MiB or more is read off the handle in 128 KiB reads, lazily
  (`unsafeInterleaveIO`) feeding a windowed inflate, and forced in IO while
  the handle is open. The file's bytes never sit in the peak: editor
  ai4096 135 → 119 MB. Any doubt falls back to the whole-file read.
- **Pinned code alignment** (`c2bd27d`): builds differing only in unrelated
  code were up to 12% apart in decode speed, from where the linker placed
  the hot loops (same objects: pal4096 47 ms in one link, 59 in another).
  `-fproc-alignment=64` on `Inflate.hs` and `Image.hs` removes the swing;
  every later comparison is between aligned builds.

**Round 3: parallel unfilter (Fable) and placement (Opus).** −71.4% →
−79.7%.
- `d958dfe`…`3e821f3`: no zeroing outside the inflated region; RGBA Up rows
  two pixels at a time and None rows as one block move; a scalar Paeth
  chain for one-byte pixels; sub-byte grey through a table; grey+alpha and
  grey16 as one word; dedicated converters for the tRNS colour-key layouts.
- `e74654a`, `f838ee1`: rows whose filter needs no row above unfilter in
  parallel runs (`pngParJobs`, through `parOrderedResumable`); when every
  row needs the one above, a wavefront (`pngWaveJob`). A killed parallel
  decode cannot resume, because the buffer is unfiltered in place, so the
  resumed thunk answers `pngParRetry` and `decodePNG` decodes again
  sequentially. Fable also fixed the streamed loader catching *every*
  exception, which made a cancelled load decode the whole file a second
  time for nobody (now `trySync`).
- `d12ec89`, `b871641`, `f022b01`: `parOrdered` workers `forkOn` the
  capabilities after the caller's, background jobs go round-robin on the
  capabilities other than the UI thread's, and the editor runs with `-qm`.
  A thread allocating on an otherwise idle capability leaves that
  capability's nursery resident (4–5 MB on a picture's peak), and automatic
  migration spreads threads over every nursery. Memory-only: harness peak
  −21 MB, editor −3 to −6 MB, time unchanged.

**Round 4: the editor's own path (Opus).** 27–48% faster to first picture,
1–4 MB lower peak, on all ten large-PNG cases, in four separate rounds.
- `b87e64c`, `44067bd`: the cell picture and the overlay are one clipped,
  word-at-a-time strict box mean (allocation during the first render 1161 →
  132 MB on ai4096).
- `dad9ee0`: these two alone *raised* the peak 1–3 MB. With the old render
  churning garbage, memory was constantly recycled; without it, the startup
  font walk's 12 MB of allocation left its capability's nursery resident for
  the session. Font discovery is now claimed by the first parse that needs
  fonts, or by a thread 500 ms in, and forked with `forkOn` on the
  claimant's capability under `mask_`, so nothing can cut the walk short:
  an earlier version ran it inside the caller's 3 s timeout, where a slow
  disk would restart it, and a claimer killed between taking and forking
  could lose it.

**Round 5: waiting threads (Fable) and the pipeline (Neon).**
- `99bf500`: wavefront workers used to spin and `yield` for the row above.
  `yield` only hands over to another Haskell thread *on the same
  capability*, so with four capabilities on one core the waiter kept the
  core while the worker it waited for had none: ai4096 took 42 s on one
  core, 10 s on two. A waiter now spins, yields for an adaptive number of
  rounds, then blocks on an `MVar` the awaited worker fills (`pngAwait`):
  0.30 s on one core, normal speed unchanged.
- `31a21f3`, `3ab10b6`: with 192 KiB or more of compressed data,
  `inflateStep` runs on the **calling** thread in 128 KiB bands and
  releases rows that end at least 32 832 bytes behind it (the DEFLATE
  window plus margin); `pngWavePipeJob` workers unfilter them as a
  wavefront and one worker converts rows in order. Waiting uses an O(nw)
  Dekker flag plus per-waiter `MVar`s. 590 → 414 ms (−30%), every file at
  least as fast. Inflate stays on the caller on purpose: run on a worker,
  its stream and window landed in another capability's nursery, +1–2 MB on
  every editor peak (the first version of the pipeline failed the rule for
  exactly this reason, and was also slower on screen.png, 4.0 → 4.8 ms,
  hence the 192 KiB gate).

**Round 6: the regression this work caused, and its fix (Neon).** The
fast loop ran only on PNG's known-size path; `inflateDyn` (ZIP members and
so DOCX/XLSX/EPUB/ODF, PDF streams, `.svgz`) still used the old byte loop,
and the PNG work had made that loop 23–30% *slower* (460 → 612 ms on the
ai4096 stream; bisected to `13ef5d6`). No PNG check could see it. Fixed by
moving `inflateDyn` onto `inflateStep` (`b5adca7`…`1063fc3`): it pauses 258
bytes short of its buffer's end, copies into a buffer twice the size and
carries on; output is block-copied out instead of `freeze` + `unfoldrN`;
the scratch buffer is not zero-filled, which is sound because the decoder
never reads a byte it has not written (a match reaching before the output
and a stored block past the input both write explicit zeros). Strict
truncation is kept exactly: no symbol is decoded from past the last real
bit, and a packed multi-literal entry that would cross it falls back to one
symbol. Result 460 → 96 ms, peak −41%; PDF Reference conversion 3.37 →
3.04 s, a 478 KB workbook 312 → 282 ms and 47 → 42 MB.
Its bar is the original decoder's exact `Either` on every input:
`docs/plans/bench/inflatedyn_fuzz.sh` (a frozen copy of `4f3c80e`'s
`Inflate` as reference; every-byte truncations and bit flips, `maxOut` at
growth and match boundaries, odd start bytes, garbage) passed about 260 000
checks over two independently generated corpora, and 274 real PDFs, Office
and ODF files, `.svgz` and zips converted byte-identically.

## Decisions worth keeping

- **Judge memory in the editor, not only in the harness.** Nursery
  placement, render churn and the font walk all moved `VmHWM` with no trace
  in the harness. Peak follows the GC schedule, not the allocation total.
- **Compare aligned builds only.** Without `-fproc-alignment=64`, a 5–12%
  difference between two builds may be the linker.
- **Placement and `-qm` only work together.** `-qm` alone left every
  worker on its caller's capability: a build that "looked fast" in an early
  measurement was in fact running sequentially, and that was the only
  reason its memory looked good.
- **Never spin-and-`yield` only.** See round 5; a timed back-off still
  took 3 s and was dropped.
- **Check what else uses a shared module.** `Cmedit.Inflate` has four
  callers and the PNG checks covered one.

## Rejected experiments

Inflate:
- Opus's whole inflate (−15.3% against Neon's −31.8%), and within it: an
  11-bit literal table (no gain; 9-bit literal and 7-bit distance tables
  were worse), looking up the next entry before the refill (about 15%
  slower, worse code generation), a smaller hot-loop live set (3–8% faster
  alone, lost 11 ms on pal4096 in the harness), and memcpy for long
  non-overlapping matches or memset only for runs of 32+ (no effect).
- A conditional lazy refill (`bc >= 20`): ai4096 174 → 191 ms, branch
  mispredictions.
- Two literals packed in an `Int32` (beaten by four in 64 bits); five-step
  and four-step straight-line literal unrolling (helped gpt2480, lost on
  big6000 and ai4096); a separate `litLoop` join point; `copy16`/`fill16`
  unrolling; `litRootBits = 11`; removing `base` from `huffFastLoop`. None
  beat what shipped.

Rows: a two-byte scalar Paeth and word writes for RGB16/RGBA16 were rejected
on 1–7% differences *before* alignment was pinned, so those rejections are
unproven and worth one aligned re-test if 16-bit files ever matter.

Editor and runtime:
- `-A1m` (a smaller nursery on every capability): lower peaks everywhere
  (ai4096 152 → 145 MB, a 32 MB CSV 253 → 235) but editing slowed (average
  frame 1.1 → 1.5 ms, a CSV's worst frame 3 → 29 ms, 80 major GCs instead
  of 4).
- A single-pixel fast path in the box mean: identical output, no gain.
- Freeing the old picture before a Revert decodes the new one (peak on a
  large picture 150 → 236 MB, transiently): declined, since a failed reload
  would then lose the picture on screen, and it needs another program to
  rewrite the file first.

## Known, not changed

- Under heavy contention the pipeline is not faster: with every core busy
  it averaged about 7% slower than round 4 over five files, and with CPU
  hogs pinned to the same cores 2–4× slower (ai4096 0.7 → 1.8 s). Output is
  always correct; the cost is time.
- Interlaced PNGs and files under 1 MB raw stay sequential.
- PDF's own PNG predictor (`PdfObj.applyPredictor`) does not use these
  kernels; it could.
- Every picture is RGBA, so opaque pictures carry a byte per pixel they do
  not need, and palette/grey ones three: plan `0037`.

## Files

- `src/Cmedit/Inflate.hs`: `huffFastLoop`, `huffBlockST`, `inflateStep`/
  `InflateStream` (resumable, sliding input window, swappable output for
  `inflateDyn`), `inflateIntoSegs`.
- `src/Cmedit/Image.hs`: `pngChunks`, `decodePNGParts`, the unfilter and
  conversion kernels, `pngParJobs`, `pngWaveJob`, `pngWavePipeJob`,
  `pngAwait`, `pngParRetry`.
- `src/Cmedit/App.hs`: `streamPng`. `src/Cmedit/JxlPar.hs`: `workerCap`.
  `src/Cmedit/Jobs.hs`: `backgroundCap`. `src/Cmedit/FontProvider.hs`:
  on-demand discovery. `Makefile`: `-qm`.
- `docs/plans/bench/`: `pty_png.py`, `gen_png_sets.py`, `PngKill.hs`,
  `PngStreamKill.hs`, `inflatedyn_fuzz.sh`, `InflateDynFuzz.hs`,
  `InflateRef4f3c80e.hs`.
