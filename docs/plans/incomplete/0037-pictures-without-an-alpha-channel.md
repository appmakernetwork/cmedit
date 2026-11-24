# 0037 — Pictures without an alpha channel

**Theme:** retained memory and peak memory for every open picture, plus
smaller terminal payloads
**Status:** proposal
**Estimated effort:** 4–6 days (phases 1–3); phase 4 +1 day; phase 5 is a
separate plan
**Risk:** medium. The representation change touches every module that
reads pixels. The rule that makes it safe is the one the decoders already
live under: the picture each phase produces must be the RGBA picture it
replaces, byte for byte, once expanded.

---

## 1. The problem, measured

Every decoder in `Cmedit.Image` returns the same `Image`:

```haskell
data Image = Image
  { imgW   :: !Int
  , imgH   :: !Int
  , imgFmt :: !String
  , imgPix :: !(UArray Int Word8)     -- Length imgW*imgH*4, RGBA.
  }
```

So every picture costs four bytes per pixel whether it has transparency
or not, and most pictures do not:

- **JPEG** cannot carry alpha at all, and is the most common photograph
  format. The same holds for 24-bit BMP, Netpbm, and lossy WebP without
  an `ALPH` chunk.
- **PNG** usually has none. On this machine, 31 of 33 PNGs sampled have no
  real transparency: 21 of 22 screenshots and downloads are colour type 2
  (RGB), and of the 16-file decoder corpus only `screen.png` uses alpha.
  `rgba_tall.png` and `photo-rgba.png` carry an alpha channel that is 255
  everywhere.
- Transparency lives mostly in icons, logos and UI assets, which are small.

The decoded image is the largest single item in a picture's peak:

| Picture | RGBA pixels | Editor peak (`pty_png.py`, kitty) | Share of the peak |
|---|---|---|---|
| ai4096 (4096², RGB) | 64 MiB | 112 MB | ~60% |
| big6000 (6000×4000, RGB) | 92 MiB | 144 MB | ~67% |
| pal4096 (4096², palette) | 64 MiB | 112 MB | ~60% |

An inactive document also keeps its picture, at its opening scale
(`parkDoc`), so the cost is retained for as long as the document stays
open, not just at the peak.

The comparison that prompted this, from the 0036 work: Pillow keeps a
palette image at one byte per pixel and decodes pal4096 in a 34 MB process
against our 78 MB. Across the rest of the corpus we were within a few MB
of it.

## 2. What it would buy

Storing an opaque picture as RGB (three bytes per pixel) saves a quarter
of its pixel memory:

- **Peak memory:** ai4096 is −16 MiB, about −14% of its editor peak.
  big6000 is −23 MiB, about −16%. A 12 MP phone JPEG is −11 MiB.
- **Retained memory:** the same saving for every open picture, including
  parked ones.
- **Terminal traffic:** kitty's graphics protocol accepts RGB payloads
  (`f=24`, already used by `Caps`' probe, `kittyGfxProbe`), so a placement
  is a quarter smaller before base64. That is up to 1.2 MP × 1 byte at
  `maxGfxPixels`, about 1.6 MB less on the wire per placement.
- **Decode time, possibly:** fewer bytes written. For PNG colour type 2
  the conversion becomes a block move of each unfiltered row. This is not
  the goal, and section 6 says why it could also go the other way.

Phase 5 (one-byte grey and palette-indexed storage) would save three
quarters for those pictures, but changes the representation much more, so
it is listed only as a follow-on.

## 3. Design

### 3.1 Representation: a channel count, not a new type

Add one field, and keep everything else:

```haskell
data Image = Image
  { imgW   :: !Int
  , imgH   :: !Int
  , imgCh  :: !Int                    -- 3 (RGB, opaque) or 4 (RGBA)
  , imgFmt :: !String
  , imgPix :: !(UArray Int Word8)     -- Length imgW*imgH*imgCh.
  }
```

A sum type over layouts (`PixRGBA | PixRGB | PixGray | PixPal …`) was
considered and rejected for now. Every consumer would have to handle every
layout at once, where a channel count lets consumers be taught one at a
time, and the single extra layout covers most of the saving. The field is
an `Int` rather than a `Bool` so that phase 5 could add `1` for grey
without another representation change.

Three helpers become the only sanctioned ways to read pixels outside hot
loops:

- `pixelRGBA :: Image -> Int -> Int -> (Word8, Word8, Word8, Word8)`
  (alpha 255 when `imgCh == 3`);
- `imageRGBA :: Image -> UArray Int Word8`, expanding to RGBA (tests and
  benches only; never on a display path);
- `stride :: Image -> Int` (`imgW * imgCh`).

### 3.2 Who produces 3 channels

**Opaque by construction**, decided from the format and known before
decoding:

| Decoder | Emits RGB when |
|---|---|
| PNG | colour types 0, 2, 3 (8- and 16-bit) with no `tRNS` chunk |
| JPEG | always (all component layouts, including the scaled decodes) |
| BMP | 24-bit; 32-bit only where the header rules already treat alpha as absent |
| Netpbm | always |
| WebP | lossy without `ALPH`; lossless when the VP8L header's alpha hint is clear *and* the decoded alpha is all 255 |
| JPEG XL | no alpha channel, or the alpha the decoder already proves constant (it skips materialising that plane today) |
| PDF image XObjects | no `/SMask`, `/Mask` or colour-key mask |
| GIF | never: frames compose through transparency, and animations stay 4-channel |

**Opaque by scan** (phase 4): a decode that produced RGBA whose alpha is
255 everywhere is compacted to RGB in place. A forward 4→3 compaction
never overtakes its own input. `shrinkMutableByteArray#`, already imported
in `Image.hs`, returns the tail. The cost is one pass over the pixels,
which must be measured against the decode it follows. This is what catches
`rgba_tall.png`-style files, which are common from screenshot tools.

### 3.3 Who consumes pixels

The full list, found by following `imgPix` and the `* 4` pixel arithmetic:

| Consumer | Module | Change |
|---|---|---|
| Half-block and ASCII cells: `renderImage`, `renderImagePlaced` | `Image` | stride from `imgCh`; specialise the inner loop per channel count |
| Overlay payload: `scaleRGBA` (the box mean) | `Image` | read 3-byte pixels; emit RGB when the source is RGB (rename it when it stops always producing RGBA) |
| Parking a picture: `reduceImage` | `Image` | keep `imgCh` |
| Crop mapping: `pictureSample` | `EditorState` | geometry only; unchanged |
| kitty: `kittyPlace` | `Gfx` | `f=24` when the payload is RGB; `kittyPlaceAnim`/`kittyClientAnim` stay `f=32` (GIF only) |
| sixel: `sixelPlace` | `Gfx` | read RGB; transparency handling only when `imgCh == 4` |
| Vector rasteriser textures: `Tex8` from `prescaleRGBA` | `Raster`, `RasterCore` | expand to RGBA at texture build time (textures are bounded by the raster's own size), or teach the sampler 3 channels if measurement says it matters |
| PDF images: `PdfRender.reduceImage`, `PdfImage` | `PdfRender`, `PdfImage` | produce RGB for unmasked images; carry `imgCh` through the reduction |
| SVG `<image>` | `Svg` | consumes `decodeImage`; via `Tex8`, as above |
| Vector raster output | `RasterCore` (`Image w h "vector" …`) | stays RGBA: a raster has coverage |
| JPEG XL `renderImg'` | `Jxl` | **see §6**: its in-place output relies on 4-byte pixels |
| Benches and tests: `ImageDecode.hs` hash, `ImageFuzz.hs`, `Spec.hs` (17 uses) | bench, test | hash `imageRGBA img`, so every existing hash and outcome list stays valid |

The `Scene` contract carries an `Image` (`CImage`, `PSampled`). The type
does not change shape, but `CLAUDE.md` says changes to the contract are
coordinated, so phase 2 lands the rasteriser's side before any producer
emits RGB into a scene.

### 3.4 The in-place PNG decode

PNG inflates its raw rows into the tail of the output buffer and converts
forwards (0036, round 1; mechanism (2) in `CLAUDE.md`). For RGB output the
raw image (`h × (1 + 3w)` bytes) is *larger* than the output (`3wh`), the
`rawSize > outSize` case the
converter already supports for sub-byte and palette images. So the buffer
is allocated at `rawSize`, converted forwards from offset 0, and shrunk by
`h` bytes at the end. The parallel unfilter and the pipeline only see
offsets, so they are unaffected.

## 4. Phases

Every phase lands as its own measured commits, under the rule 0036
worked to: faster *and* not more peak memory, judged by `measure.py`
(decoder) and `pty_png.py` (editor). The other image views are judged by
`pty_jpegview.py`, `pty_svg.py` and `pty_pdf.py`.

1. **The field, everywhere, still 4.** Add `imgCh = 4` to every
   constructor, route the benches' and tests' hashes through `imageRGBA`,
   and make every consumer use `stride`. No producer changes. Hashes
   identical; timings within noise. Check the hot loops'
   `-ddump-stg-final` for new boxing, as `CLAUDE.md` requires for
   `decodeJpegST`.
2. **Consumers learn 3.** Each consumer in §3.3, with Spec tests that build
   an RGB image and its RGBA expansion and require identical output:
   cells, kitty and sixel payload bytes, raster textures.
3. **Producers emit 3, by construction.** One decoder per commit, in order
   of payoff: JPEG, PNG, BMP/Netpbm, WebP, PDF images, then JPEG XL (§6).
   Acceptance per decoder: the expanded hash is identical, editor peak is
   down, first picture is not slower.
4. **Opaque by scan** (optional). Compaction after an RGBA decode whose
   alpha is all 255, gated on a measured cost (it must not make opening
   such a file slower overall).
5. **Follow-on, separate plan:** one-byte grey, and palette-indexed
   storage (index plane plus a 256-entry table), where pal4096 would drop
   from 64 MiB of pixels to 16 MiB.

## 5. Test plan

- `make test`, with the new RGB/RGBA equivalence tests from phase 2.
- `docs/plans/bench/imgbench.sh` and `measure.py` on the PNG corpus, and
  the JPEG and WebP corpora from 0032/0035: the expanded hash must be
  identical for every file.
- `jpegcheck.sh run BIN FILE...` (bit-exact against libjpeg-turbo) must
  stay at max 0; it compares display RGB, which is unaffected.
- `ImageFuzz.hs` outcome lists unchanged (`Left`/`Right` and hash).
- The kill tests (`PngKill.hs`, `PngStreamKill.hs`) for the PNG producer
  change, since it touches the in-place buffer.
- PTY: `pty_png.py`, `pty_jpegview.py` (refinement, Revert mid-job, a piped
  photo), `pty_svg.py`, `pty_pdf.py`; kitty and no-reply modes, so both
  overlay paths and the half-block floor are exercised.
- A real kitty session to confirm `f=24` placements, since the PTY harness
  only checks the escape stream.

## 6. Risks

- **Hot-loop speed.** The box mean (`scaleRGBA`) and the cell renderer read
  a pixel as one 32-bit word today. Three-byte pixels mean unaligned or
  split loads. The rule is that neither may get slower: specialise the
  loop per channel count at its top, read 64-bit words and shift, and
  measure. If a consumer cannot match its RGBA speed, it expands rows into
  a small scratch buffer instead.
- **JPEG XL's in-place output.** `renderImg'` writes RGBA into the first
  colour plane because a `Float` and an RGBA pixel are both four bytes,
  and `CLAUDE.md` calls that an ownership contract. RGB output there means
  either compacting afterwards (one pass, phase-4 style) or leaving JXL at
  four channels. Decide by measurement; four channels is the safe default.
- **Places that silently assume four.** Anything that computes
  `numElements (imgPix img) < 4 * w * h` as a sanity check (`Raster` has
  one) would reject a valid RGB image. Phase 1's `stride` sweep is meant
  to catch these; a Spec test that runs every consumer on an RGB image is
  the backstop.
- **Animation.** GIF frames, and kitty's animation uploads, stay four
  channels. A document's `idFrames` always come from one decoder, so its
  frames share a channel count; assert that where the frames are built,
  so a future animated format cannot mix them.
- **The bar.** "Byte-identical" becomes "identical once expanded to RGBA".
  The benches and fuzz lists have to switch to that in phase 1, *before*
  any producer changes, or a phase-3 diff would be unreadable.

## 7. Out of scope

- Colour management, premultiplied storage and 16-bit storage: decoders
  keep reducing to 8-bit sRGB as today.
- The vector rasteriser's own canvases, which need coverage.
- Changing what any picture looks like.
