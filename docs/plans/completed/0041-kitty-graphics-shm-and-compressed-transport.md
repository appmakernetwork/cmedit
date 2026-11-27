# 0041 — Kitty graphics: local shared-memory (`t=s`) and compressed (`o=z`) transport

**Theme:** image/SVG/PDF rendering latency, PTY bandwidth, and allocation
churn on kitty-graphics terminals (both local and over SSH)
**Status:** ✅ Resolved (2026-11-26)
**Estimated effort:** 4–5 days (phase 1 `t=s` / `t=t` local transport: 2 days;
phase 2 `o=z` zlib-compressed direct transport: 2–3 days)
**Risk:** medium. Graphics protocol mistakes can either show blank pictures or
leak shared-memory/temp objects on failure or crashes. Every transport upgrade
must be gated by a positive terminal probe (never assumed) and keep the
existing inline `t=d` stream as the automatic fallback.

---

## 1. The problem

Today every kitty-protocol placement in `Cmedit.Gfx` (`kittyPlace`,
`kittyPlaceAnim`, `kittyClientAnim`) uses **direct inline transmission**
(`t=d`), uncompressed:

- At `maxGfxPixels = 1,200,000` (RGBA, 4.8 MB raw), `kittyPlace` base64-encodes
  all 4.8 MB into **6.4 MB** of ASCII split across **1,563 APC escape chunks**
  (`3072` raw bytes → `4096` base64 chars each) and writes all 6.4 MB through
  the PTY pipe on every zoom step, pan, resize, or PDF page turn.
- Even after plan `0037` (`f=24` RGB for opaque images), a full placement is
  still **4.8 MB** of base64 text on the wire.
- **Locally** (kitty, Ghostty, WezTerm on the same host), pushing megabytes of
  base64 through a 64 KiB kernel PTY buffer causes measurable CPU overhead
  (base64 encode + base64 decode in the terminal) and visible frame stutter
  during scroll-wheel zoom or rapid PDF page flipping.
- **Over SSH**, 4.8–6.4 MB per placement takes seconds on a 10–50 Mbps link —
  even though PDF pages, SVG diagrams, and UI screenshots are 80–95% flat
  solid backgrounds that compress by **10–30×** with DEFLATE!

## 2. Design

The kitty graphics protocol specifies two orthogonal upgrades, both already
widely implemented in kitty, Ghostty, and WezTerm:

1. **Local transport (`t=s` POSIX shared memory, or `t=t` auto-deleted temp
   file)** — transmits pixel bytes out-of-band and sends only a ~70-byte APC
   sequence naming the shm object / temp path.
2. **Zlib compression (`o=z`)** — wraps the raw RGB/RGBA pixel buffer in a
   standard RFC 1950 zlib stream (`0x78 0x01` + DEFLATE + Adler-32) before
   base64 chunking over `t=d`.

### 2.1 Phase 1 — Probed local transport (`t=s` on POSIX, `t=t` fallback)

**Capability probing (opt-in by evidence, `AGENTS.md`):**

Never guess "local vs SSH" from environment variables alone (`SSH_CONNECTION`
can be missing inside containers, `tmux`, or `sudo`, and a terminal might run
over a different filesystem namespace). Instead, **probe directly** using
kitty's `a=q` query action, the exact same way `kittyGfxProbe` already works:

1. At startup (only after `TrKittyGfx True` arrives, or alongside the initial
   probe burst on POSIX when not clearly remote):
   - Create a 4-byte POSIX shared-memory object `/cmedit-probe-<pid>` (or a
     4-byte temp file in `$TMPDIR` with `tty-graphics-protocol` in its name,
     which kitty's `t=t` requires as a safety guard against arbitrary file
     deletion).
   - Send `APC G i=32,a=q,t=s,f=32,s=1,v=1;<base64(name)> ST`.
   - If the terminal is on the same host and supports `t=s`, it opens and
     unlinks the shm object and replies `APC G i=32;OK ST` →
     `TrKittyShm True` in `Cmedit.Caps.TermCaps` (`tcKittyShm :: !Bool`).
   - If over SSH, inside an isolated container, or unsupported, the terminal
     either replies `ENOENT` or ignores it → `tcKittyShm` stays `False`, and
     we unlink the probe object after a short timeout (~2 s) on the driver
     side so nothing leaks.

**Platform boundary (`platform/{posix,windows}/Cmedit/Term.hs`):**

Keep `src/` 100% portable (`make windows-check` must stay clean):

- Add a small platform helper in `Cmedit.Term`:
  ```haskell
  -- | Write bytes into a fresh one-shot shared-memory object (POSIX shm_open)
  -- and return its name for kitty t=s transport, or Nothing on platforms/errors
  -- where shm is unavailable.
  writeKittyShm :: BS.ByteString -> IO (Maybe String)
  unlinkKittyShm :: String -> IO ()
  ```
- Under `platform/posix` + `cbits/cmedit_term.c`: ~35 lines of C calling
  `shm_open(name, O_CREAT | O_EXCL | O_RDWR, 0600)`, `ftruncate`, `write`
  loop, and `close` (linking `-lrt` only if needed — on glibc 2.34+ `shm_open`
  is in `libc`; test with `make static` too, or fall back to `t=t` temp files
  in `/dev/shm` / `$TMPDIR` which need zero extra linker flags and work
  identically on Linux, macOS, and Windows!).
  - **Key check before committing to `t=s` vs `t=t`**: verify `make static`
    (static glibc link in `Makefile`) with `shm_open`. Note that `t=t` (a
    temp file named `…tty-graphics-protocol…` in `/dev/shm` on Linux or
    `$TMPDIR` on macOS/Windows) requires **zero C code**, works on **Windows
    and macOS** as well as Linux, uses RAM-backed tmpfs on Linux (`/dev/shm`),
    and the kitty protocol guarantees the terminal unlinks `t=t` files
    immediately after reading! Compare `t=s` vs `t=t` across kitty, Ghostty,
    and WezTerm and pick the one with broader compatibility + zero static-link
    friction.

**Leak prevention:**

- Track the last few unconsumed probe/placement names in `Drv` and unlink any
  still present on bracketed teardown (`finally`), plus never create more than
  one live shm/temp object per placement (previous one is unlinked by the
  terminal on read; track a monotonic counter + PID in the name).

**Emission (`Cmedit.Gfx`):**

- `kittyPlaceRef :: GfxTrans -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> String -> Builder`
  emits:
  ```text
  Ga=d,d=A ... moveTo row col ... Ga=T,f=<24|32>,t=<s|t>,i=1,q=2,s=W,v=H,c=C,r=R;<base64(name)>
  ```
- Composes cleanly with plan `0037` (`f=24` vs `f=32`) and with both static
  images (`App.placeGfxIO`), vector/PDF rasters (`App.vecPlacement`), and
  animation frames (`kittyPlaceAnim` / `kittyClientAnim`).

### 2.2 Phase 2 — Fast RFC 1950 zlib compression (`o=z`) for remote `t=d`

When `tcKittyShm` is `False` (SSH, remote container, or unsupported terminal),
we still send `t=d` inline chunks — but kitty supports `o=z` (RFC 1950 zlib
compressed payload) on every kitty-graphics terminal.

**Write a small, fast pure-Haskell compressor (`Cmedit.Deflate`, leaf module):**

We do **not** need a heavy optimal-parse deflate encoder — speed matters more
than the last 3% of ratio, because this runs on the render path (or on the
`JVecGfx` background worker for SVG/PDF!):

- **Adler-32** checksum (trivial, ~15 lines in `ST`/unboxed loop; ~1 ms for
  4 MB).
- **Fixed-Huffman LZ77** (block type `01`, RFC 1951 §3.2.6) with a single
  4096-entry or 8192-entry hash table (`STUArray s Int Int`, 32 KiB scratch —
  fits in L1 cache!) matching 4-byte (pixel-aligned!) or 3-byte sequences,
  capped to 1 probe per position (greedy, zero chain walks, like LZ4/zlib
  level 1).
- Why pixel-aligned / 4-byte hashing shines here:
  - PDF pages and SVG diagrams are mostly uniform runs of identical RGBA/RGB
    pixels → 258-byte max-length matches at distance 4 (or 3) compress a
    4.8 MB PDF page to **~60–180 KiB** in **< 8 ms**.
  - For photographic JPEGs (noisy, low compressibility), include a fast
    **incompressibility bailout**: after the first 64 KiB, if output bytes ≥
    90% of input bytes, abort compression immediately and send raw `t=d`
    without `o=z` (cost: < 0.3 ms, never makes photos larger or slower).
- **Where compression runs**:
  - For SVG and PDF pages (`JVecGfx` background worker), compress on the
    worker thread right after rasterisation so `renderNow` only emits the
    already-compressed chunks.
  - For bitmaps (`placeGfxIO`), compress only when `BS.length payload >= 16384`
    and `not (tcKittyShm caps)`.

**Verify round-trip against our own `Cmedit.Inflate.inflate`!** Because
CMeDit already has a full RFC 1951 decompressor, every compressed payload can
be round-trip verified in unit tests with zero external tools.

### 2.3 Protocol interactions & `GfxKey`

- `GfxKey` in `Cmedit.App` does not need to change (transport choice is a
  function of `TermCaps` + payload).
- Synchronised output (`beginSync` … `endSync`) stays intact; with `t=s`/`t=t`,
  the entire frame + image placement drops from ~6.4 MB to ~200 bytes.
- Add a config override `gfx-transport = auto | inline | shm` (optional, or
  keep purely automatic via probe + a debug counter in `--stats-on-exit`
  reporting bytes sent for graphics so improvements are measurable).

## 3. Implementation steps

1. **Measure baseline**: extend `docs/plans/bench/pty_png.py` and
   `pty_pdfbench.py` to record graphics wire bytes and placement wall-clock ms
   in kitty mode (local + simulated slow PTY pipe).
2. **Phase 1 (`t=s` / `t=t` local transport)**:
   - Test `t=s` vs `t=t` compatibility on kitty, Ghostty, WezTerm, and under
     `make static`.
   - Add probe in `Cmedit.Gfx` / `Cmedit.Caps` / `Cmedit.Input` (`TrKittyShm`
     for `i=32`) and portable write/cleanup helpers in `Cmedit.Term`.
   - Wire into `App.vecPlacement` and `App.placeGfxIO` with bracketed cleanup.
3. **Phase 2 (`Cmedit.Deflate` + `o=z`)**:
   - Implement RFC 1950 zlib wrapper (Adler-32 + fast 1-probe hash LZ77 with
     fixed Huffman codes + incompressibility bailout).
   - Round-trip fuzz against `Cmedit.Inflate.inflate` / `inflateDyn` (and
     Python `zlib.decompress` in bench).
   - Wire `o=z` into `kittyPlace` / `kittyPlaceAnim` / `kittyClientAnim` when
     using `t=d`, and offload vector/PDF compression to `JVecGfx`.
4. **Re-measure** wall time, allocation, peak RSS, and wire bytes across PNG,
   JPEG, SVG, and PDF benchmarks.

## 4. Test plan

- **Pure unit tests (`test/Spec.hs`)**:
  - `Cmedit.Deflate` round-trip through `Cmedit.Inflate`: empty, 1 byte,
    all-zeros (tests 258-byte RLE matches), checkerboard, synthetic PDF/SVG
    buffer, pseudo-random noise (verifies bailout triggers cleanly), boundary
    lengths around 32 KiB window.
  - `Cmedit.Caps`: `i=31;OK` enables `tcKittyGfx` without enabling
    `tcKittyShm`; `i=32;OK` enables `tcKittyShm`; `i=32;ENOENT` leaves
    `tcKittyShm = False`.
  - `Cmedit.Gfx`: generated APC escape sequences for `t=s`/`t=t` and `t=d,o=z`
    parse cleanly and respect chunk limits (≤ 4096 base64 chars, `m=1`/`m=0`).
- **PTY & real-terminal checks**:
  - Silent-terminal PTY test: no `t=s`/`o=z` emitted, half-block fallback
    unchanged.
  - Simulated kitty PTY (`i=31;OK` only → `t=d` + `o=z`; `i=31;OK` + `i=32;OK`
    → `t=s`/`t=t`, verifies temp/shm file contains exact pixels and cleanup
    leaves zero files behind on exit).
  - Real terminal verification on kitty (and Ghostty/WezTerm if available),
    both locally and over `ssh localhost`.
- **Portability & builds**: `make test`, `make windows-check`, `make static`.

## 5. Risks & mitigations

- **Terminal compatibility with `o=z` or `t=s`**: some non-kitty terminals
  implement basic `t=d` only. Gate `t=s`/`t=t` on the explicit `i=32` probe;
  also probe `o=z` with an 8×8 RGB compressed `a=q` query (`i=33`) rather than
  assuming every `t=d` terminal supports `o=z`! That keeps the *"opt-in by
  evidence"* invariant 100% intact.
- **Orphaned `/dev/shm` or temp files on SIGKILL**: `t=s` / `t=t` objects are
  unlinked by the terminal as soon as it processes the APC sequence (within
  milliseconds), so only an in-flight placement at the exact millisecond of a
  `SIGKILL` could remain; using a PID-scoped name keeps any footprint bounded.
- **Compression CPU overhead on noisy photos**: mitigated by the 64 KiB
  incompressibility bailout and by bypassing `o=z` completely whenever local
  `t=s`/`t=t` is active.
- **Static linking (`make static`)**: if glibc's `shm_open` pulls unwanted
  NSS/pthread/rt issues in static builds, prefer `t=t` in `/dev/shm` (Linux) /
  `$TMPDIR` (macOS/Windows) which uses ordinary `open`/`write`/`close`
  syscalls and works across all platforms.

## 6. Out of scope

- Sixel transport changes (sixel is already RLE-compressed and ~4.6× smaller
  than raw kitty base64).
- Lossy re-encoding (e.g. shipping PNG/JPEG over `f=100` to the terminal).
- Dynamic-Huffman DEFLATE encoding (only worth adding if fixed-Huffman ratios
  measure poorly on real PDF/SVG corpora).

---

## Resolution & measurements (2026-11-26)

Implemented both phases end-to-end on boot libraries only:

- **Phase 1 — Probed local out-of-band transport (`t=s` on POSIX, `t=t` on Windows):**
  - `cbits/cmedit_term.c`: `cmedit_shm_write` (`O_CREAT | O_EXCL | O_RDWR`, mode `0600`, `EINTR`-safe partial-write loop on Linux/POSIX to avoid `SIGBUS` on full `/dev/shm`, `ftruncate` + `mmap` on macOS, unlinks on error) and `cmedit_shm_unlink`. Linked with `-lrt` on Linux for glibc < 2.34 compatibility (verified both dynamic `make` and static `make static`).
  - `platform/posix/Cmedit/Term.hs` & `platform/windows/Cmedit/Term.hs`: matching `KittyLocalKind(..)`, `writeKittyLocal`, `unlinkKittyLocal`. POSIX uses `t=s` shm names `/cm-<pid>-<8hex>-<seq>` (<= 27 chars, within macOS `PSHMNAMLEN=31`; `t=t` is not used on POSIX). Windows uses `t=t` auto-deleted temp files matching `"cmedit-tty-graphics-protocol-.bin"` and rethrows async exceptions.
  - `Cmedit.App`: emits `i=32` local probe once per session **only after `TrKittyGfx True` (`i=31;OK`)**, under `mask_`; unlinks pending probe on any `i=32` reply, on 2 s `FsTick` timeout, or on teardown; tracks placed objects in a batch-tagged 256-entry / 64 MiB ring that only evicts strictly earlier batches (and falls back from `t=s` to `o=z`/raw `t=d` for animations with `> 256` frames so an upload can never self-evict) and unlinks all entries in `leaveScreen \`finally\` cleanupGfxLocal` (including `SIGTERM`/`SIGHUP`).
- **Phase 2 — Probed RFC 1950 zlib direct transport (`o=z`):**
  - `src/Cmedit/Deflate.hs` (pure leaf module): RFC 1950 zlib header + chunked Adler-32 + RFC 1951 fixed-Huffman 1-probe 8192-entry (32 KiB L1) LZ77 encoder (strict RFC 1951 symbol `285` for length 258, tail hash refresh for distance `1..4` flat runs, periodic + windowed 64 KiB incompressibility bailout, right-sized `BI.createAndTrim` output).
  - `Cmedit.Gfx`, `Cmedit.Caps`, `Cmedit.Input`: `kittyZlibProbe` (`i=33`, `8x8` RGB compressed to `< 32` B so a terminal ignoring `o=z` rejects it for short data; emitted at startup before DA1), `kittyLocalProbe` (`i=32`), `KittyPayload(..)` (`KpRaw`, `KpZlib`, `KpLocal`), `kittyPlacePayload`, `kittyPlaceAnimPayload`, `kittyClientAnimPayload` (existing `kittyPlace`/`kittyPlaceAnim`/`kittyClientAnim` kept byte-identical; plain-kitty animations remain lazy/streaming with arithmetic wire-byte stats).
  - `Cmedit.App`: `VecRaster` carries a 3-way `ZlibCache` (`ZlibUntried`/`ZlibSkip`/`ZlibReady`) pre-compressed on `JVecGfx`/`JVecGfxPrefetch` workers so the UI thread never re-compresses incompressible rasters; `--stats-on-exit` prints a 4th line reporting placements, wire bytes, and active transport mode when graphics were emitted.

### Measured results (120×40 PTY, `-O2`)

| Workload / Mode | Before (`t=d` raw) | After (`o=z` remote zlib) | After (`t=s` local shm) |
|---|---|---|---|
| `cmedit-slides.pdf` p1 (952×528) | 2,680,832 B wire | **38,134 B** (**70.3×** smaller) | **73 B** (**36,700×** smaller) |
| `cmedit-slides.pdf` maxpay (10 pages, `pty_pdfbench.py`) | 3,012,430 B | **86,013 B** (**35.0×** smaller) | **73 B** steady-state (`38,134 B` only if p1 races before `i=32;OK`) |
| `icon.svg` (592×592 / 736×592) | 1,869,144 B wire | **37,817 B** (**49.4×** smaller) | **76 B** |
| Noisy 200×192 PNG (incompressible) | 205,289 B raw | **205,289 B** (clean bailout to raw `t=d`) | **~76 B** wire (`t=s`) |
| `Van_Gogh_-_Starry_Night.jpg` (`pty_png.py`, plain kitty) | 53 ms, 42 MB alloc, 9 MB peak-live | 51 ms, **42 MB alloc, 9 MB peak-live** (zero regression) | — |

### Verification

- `make test`: **4,914 passed, 0 failed** (71 new assertions in `test/GfxTransportSpec.hs`: Adler-32 RFC vectors, fixed-Huffman symbol `285` conformance, distances `1..32769` including codes `28`/`29` at `16385`/`32768`, flat-border+noise bailout, `i=31/32/33` multi-key/unknown-ID parsing, APC wire formatting, 4 MiB POSIX shm round-trip).
- `make windows-check`: **82/82 clean**, zero warnings.
- `make static`: statically linked ELF with `-lrt`, verified with all 8 scenarios in `docs/plans/bench/pty_gfx_transport.py` (silent terminal, plain kitty + 2 s probe sweep, remote `o=z` SVG/PDF + Python `zlib.decompress` + noisy bailout, local `t=s` shm + unread exit cleanup, `--stats-on-exit`, animated GIF `a=f` over `t=s` & `a=t` over `o=z`, `SIGTERM` shm cleanup, 300-frame animated GIF `> 256` shm ring fallback).
- Existing PTY suites: `pty_svg.py` (7/7 PASS), `pty_pdf.py` (6/6 PASS), `pty_png.py` PASS, zero `/dev/shm/cm-*` leftovers.
- Real terminal (Ghostty, local, 2026-11-27): answers `i=31;OK`, `i=33;OK` and `i=32;OK`; a PDF page and an SVG were each placed over `t=s` (90 B on the wire, `--stats-on-exit` reports `kitty shm`), with no `/dev/shm/cm-*` left afterwards. Not yet checked: the picture by eye, `o=z` placements in a real terminal (shared memory takes priority locally, so that needs an SSH session), kitty and WezTerm.

## Correction, 2026-11-27: fixed-Huffman blocks crash Ghostty

The first real `o=z` session (Ghostty 1.3.1 over SSH) segfaulted the terminal
as soon as a PDF page was placed. The stream was valid (Python's zlib inflates
it). Ghostty crashes on any fixed-Huffman (BTYPE=01) stream that contains
matches and inflates past a few tens of kilobytes: stock zlib with `Z_FIXED`
over the same pixels, over zeros and over repeated text crashes it the same
way, at 192 KB and at 927 KB but not at 38 KB, while dynamic blocks over the
same data (zlib levels 1 and 6, `Z_HUFFMAN_ONLY`, up to 5.76 MB of zeros) are
fine. The `i=33` probe could not see it: its 192-byte payload is below the
threshold.

`Cmedit.Deflate` therefore no longer emits a fixed block. It keeps one
constant code (the fixed code, adjusted to 286 + 30 symbols so both codes are
complete, with distances 1 and 4 at 4 bits) and declares it in a precomputed
dynamic-block header of about 80 bytes. The encoder loop is unchanged. The
PDF page that crashed went from 18 730 to 18 273 compressed bytes.
`GfxTransportSpec` pins BTYPE=10 and its symbol walker now reads the dynamic
header. Verified in real Ghostty with `/dev/shm` hidden from the terminal
(`bwrap --tmpfs /dev/shm`, which forces the `o=z` path locally): five PDFs
and two SVGs placed as `kitty zlib`, terminal alive. Wherever the text above
says "fixed-Huffman", read this section instead.

Confirmed the same day over real SSH: the `1+dev2` static build on a remote
host, viewed from Ghostty 1.3.1, placed a PDF page without incident. That
closes the "`o=z` in a real terminal" item left open under verification.
WezTerm (stats line: `kitty zlib`, 54.9 KB sent for one placement) and kitty
were then each checked the same way, a PDF over SSH, and both placed it.
An animated GIF over SSH then played in both kitty (native animation
upload) and Ghostty (client-driven frame swaps), and PDF page turns and
zoom work on the remote path. Nothing from the verification list is left
open.
