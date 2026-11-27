# 0038 — Interactive archive browsing and `.tar` / `.gz` / `.tar.gz` support

**Theme:** capability — turning archive listings into navigable read-only
trees, and extending the same pattern to TAR and gzip files
**Status:** proposal
**Estimated effort:** 4–6 days (phase 1 interactive ZIP: 2 days; phase 2
gzip + TAR/tar.gz: 2–3 days; phase 3 polish/nested-member ergonomics: 1 day)
**Risk:** medium. Untrusted archives are classic zip-bomb / path-traversal /
decompression-ratio traps; every extraction must live under explicit byte and
entry budgets, never write to disk, and always degrade cleanly to a listing or
error.

---

## 1. The problem

CMeDit already has most of the machinery needed to inspect compressed files
and archives without external libraries:

- `Cmedit.Zip` already parses ZIP/ZIP64 central directories without reading
  member payloads (`findEocd`, `parseCentral`) and extracts single named
  members on demand under a strict byte budget (`localDataOffset`,
  `memberBytes`, `App.readMember`, `maxMemberBytes`, `maxArchiveBytes`).
- `Cmedit.Inflate` already provides `inflateDyn` (recently moved onto the
  fast resumable `inflateStep` loop in plan `0036`, ~4.8× faster) and
  `InflateStream` for bounded streaming decompression.
- `Cmedit.SvgTree.gunzip` already parses RFC 1952 gzip headers for `.svgz`.

Yet today:

1. Opening a `.zip`, `.jar`, `.whl`, `.apk`, or `.docx` (via `Alt+T`) stops at
   a static read-only text buffer produced by `zipListing`. Looking at one
   `manifest.json`, `pom.xml`, `.csv`, `.png`, `.svg`, or `.pdf` inside the
   archive forces the user to drop back to a shell and `unzip`.
2. `.tar`, `.tar.gz` / `.tgz`, and standalone `.gz` files (especially rotated
   server logs like `app.log.1.gz` over SSH) are refused as binary files.

## 2. Design

### 2.1 Preserve the read-only, no-temp-file bargain

Nothing extracted from an archive is ever written to disk. Every extracted
member is held in memory as a `BS.ByteString` bounded by `maxMemberBytes`
(already enforced by `Cmedit.Zip`), classified via the existing
`App.classifyBytes True` path (read-only, non-rereadable stream), and opened
as a read-only document with a synthetic display path such as:

```text
/path/to/archive.zip::dir/report.csv
```

Because `forceRO = True`:

- Save and Save All are refused (no archive path can ever be clobbered by a
  member's contents; the archive listing itself already obeys this).
- **Save As / Export** (`Ctrl+Shift+S`) works out of the box, offering the
  member's basename as the default destination filename so a user can save out
  a single file cleanly.
- Re-opening the same `(archivePath, memberName)` switches to the already-open
  tab rather than duplicating it.
- Crash recovery journals and clean-exit snapshots skip read-only / synthetic
  archive-member documents (the same rule as PDF and container views).

### 2.2 Phase 1 — Interactive ZIP browsing

Today an archive listing is a plain read-only text buffer with `Lang = Archive`
highlighting. Keep that text-buffer presentation (Find, copy, word wrap,
scrollbars, Go To Line all keep working for free), and attach a minimal
per-document metadata sidecar when the buffer is a listing:

```haskell
-- Cmedit.Zip (or Cmedit.Archive)
data ArchiveKind = ArchZip | ArchTar | ArchTarGz
  deriving (Eq, Show)

data ArchiveIndex = ArchiveIndex
  { aiPath    :: !FilePath             -- ^ Canonical path of the archive on disk.
  , aiKind    :: !ArchiveKind
  , aiLineMap :: !(Map Int ArchiveItem) -- ^ 0-based listing line -> member descriptor.
  } deriving (Eq, Show)

data ArchiveItem = ArchiveItem
  { atName      :: !Text
  , atSize      :: !Integer
  , atPacked    :: !Integer
  , atEncrypted :: !Bool
  , atDir       :: !Bool
  , atOffset    :: !Integer            -- ^ Local header offset (ZIP) or data offset (TAR).
  } deriving (Eq, Show)
```

`zipListing` already formats one line per tree row; extending it to return
`(Text, Map Int ArchiveItem)` (keeping `zipListing` as a thin wrapper so
existing callers/tests don't break) ties every non-directory line directly to
its member metadata without ever parsing formatted text back.

**Interaction (pure model + one new `Effect`):**

- `edArchive :: !(Maybe ArchiveIndex)` (per-document, carried in `Document`
  via `captureDoc`/`restoreDoc`, cleared in `blankReadOnly` and `setLoaded`).
- In `Editor`, when `isJust (edArchive ed)` and the user presses **`Enter`**
  (or double-clicks a member row):
  - Look up `posLine (edCursor ed)` in `aiLineMap`.
  - If the row is a header/summary/directory: status note (or no-op).
  - If `atEncrypted item`: status note `"Cannot open encrypted archive member"`
    without touching IO.
  - If `atSize item > maxMemberBytes`: status note reporting the size and cap.
  - Otherwise emit `EffOpenArchiveMember aiPath aiKind item` (plus
    `beginLoading`), running on the background loader (`JLoad`) so large
    deflated members never stall the UI.
- In `App`:
  - `EffOpenArchiveMember` extracts the single member into memory under the
    existing `maxMemberBytes` budget.
  - Feeds the bytes into `classifyBytes True (archiveMemberPath path name) bs`,
    plus container sniffing if the member itself is a `.docx`/`.xlsx`/`.epub`
    or nested `.zip` (capped at recursion depth 1 to prevent zip-quine bombs).
  - Delivers the resulting `LoadOutcome` through `drvLoadQ` like any other
    async open, so images (PNG/JPEG/WebP/JXL), SVG pictures, PDFs, CSVs, RTFs,
    JSON, and source files inside an archive all open in their full native
    views automatically.
  - Hint bar in archive listings advertises `Enter Open Member` alongside the
    read-only hints.

### 2.3 Phase 2 — Reusable gzip module and `.gz` / `.tar` / `.tar.gz`

**Extract `Cmedit.Gzip` (leaf module, imports only `Cmedit.Inflate`):**

Move and harden the RFC 1952 `gunzip` helper currently private to
`Cmedit.SvgTree`:

- `gzipMagic :: BS.ByteString -> Bool` (`0x1f, 0x8b, 0x08`).
- `gunzipBounded :: Int -> BS.ByteString -> Either String (BS.ByteString, Maybe Text)`
  — validates header flags (`FEXTRA`, `FNAME`, `FCOMMENT`, `FHCRC`), inflates
  via `inflateDyn` with a strict output cap, verifies the trailing ISIZE
  (mod $2^{32}$) and CRC-32 (a small 256-entry table in pure Haskell), and
  supports concatenated gzip members (common in rotated logs and `bgzip`).
- `Cmedit.SvgTree.decodeSvgBytes` switches to `Cmedit.Gzip.gunzipBounded`.

**Add `Cmedit.Tar` (leaf module, imports `Cmedit.Width` at most):**

- `tarMagic :: BS.ByteString -> Bool` — checks `"ustar"` at byte offset 257
  (POSIX.1-1988 ustar / GNU tar) on a 512-byte head read, plus header checksum
  validation so random binary files never false-positive.
- Supports POSIX ustar `prefix` + `name`, GNU long-name (`'L'`) and PAX
  extended headers (`'x'` / `'g'` `path=` and `size=`), regular files
  (`'0'`/`'\0'`), directories (`'5'`), and symlink/hardlink annotations
  (`'2'`/`'1'`, displayed in listing, never followed on disk).
- **Uncompressed `.tar`**: size-independent indexing by seeking across
  512-byte headers (`hSeek` over each member's rounded payload size) with an
  entry cap (`maxTarEntries = 100_000`) — a multi-gigabyte `.tar` indexes
  without reading member payloads, and extracting one member is a single
  bounded `hSeek` + `BS.hGet` at `atOffset`.
- **Compressed `.tar.gz` / `.tgz`**: streamed through `InflateStream`
  (already built in plan `0036`!) in 64–128 KiB chunks to walk TAR headers
  without holding the whole decompressed archive in memory, subject to a
  decompressed-stream scan budget (e.g. 256 MiB for listing, `maxMemberBytes`
  for member extraction). Beyond the budget, list what was indexed with a
  clear truncation note (`⚠`) just like `Rtf`/`Zip`.
- **Standalone `.gz` (e.g. `server.log.1.gz`, `data.csv.gz`, `doc.pdf.gz`)**:
  - Detected in `classifyFileWith` by `gzipMagic` (after ZIP, before binary
    refusal and before `maxOpenBytes` only up to a compressed cap).
  - First peek the decompressed initial 512 bytes via `InflateStream`:
    - If `tarMagic` holds → route to the `.tar.gz` listing view.
    - Otherwise decompress up to `maxGzipBytes` (e.g. `maxOpenBytes` or 32 MiB
      initially) and dispatch through `classifyBytes True` using the inner
      filename (from the gzip `FNAME` header or stripping `.gz`), so
      `access.log.gz`, `table.csv.gz`, `diagram.svg.gz`, and `photo.png.gz`
      all open read-only in their natural views!
  - Note: `.svgz` continues to open as an editable-view-disabled read-only SVG
    picture via the exact same path (retaining its current behaviour).

### 2.4 Classification order in `App.classifyFileWith`

Order is safety-sensitive (`AGENTS.md`):

1. Missing file → new text file.
2. Non-regular stream (pipe/device) → bounded single read → `classifyBytes True`.
3. `Z.zipMagic` (4 bytes) → `archiveOutcome` (DOCX/XLSX/EPUB/ODF or interactive ZIP listing).
4. `Gzip.gzipMagic` (3 bytes) → `gzipOutcome` (peek 512 decompressed bytes → `.tar.gz` interactive listing, or `.svgz`/standalone `.gz` read-only view).
5. `Tar.tarMagic` (512 bytes) → `tarOutcome` (size-independent interactive TAR listing; `maxOpenBytes` does not apply).
6. Existing `sz > maxOpenBytes` → `Pager` or refusal.
7. Existing `readFile` → `classifyBytes`.

### 2.5 CLI `--convert` bonus

`cmedit archive.zip > list.txt` already prints the listing; extending
`convertPath` to support `.tar` and `.tar.gz` listings (and `.gz` text
decompression) keeps CLI conversion consistent with the interactive editor.

## 3. Implementation steps

1. **Extract `Cmedit.Gzip`** from `Cmedit.SvgTree.gunzip`; add CRC-32 check,
   concatenated-member handling, and streaming peek helper using
   `InflateStream`; add unit tests in `test/Spec.hs`.
2. **Interactive ZIP**: extend `Cmedit.Zip` to return `(Text, ArchiveIndex)`
   alongside `zipListing`, wire `edArchive`/`docArchive`, `Enter`/double-click
   dispatch, and `EffOpenArchiveMember` in `App.hs`; add unit + PTY tests.
3. **Add `Cmedit.Tar`**: header parser (ustar + GNU `'L'` + PAX `'x'`),
   checksum check, shared tree formatter (factored out of `Cmedit.Zip` so ZIP
   and TAR listings look identical and reuse `Archive` syntax highlighting).
4. **Wire TAR / `.tar.gz` / standalone `.gz`** into `App.classifyFileWith`,
   `App.EffOpenArchiveMember`, `Syntax.langForPath` (colouring `.tar`/`.tgz`/
   `.gz` in explorer), and `convertPath`.
5. **Update `README.md`, `Manual.hs`, `cmedit.cabal`, and run `make test` +
   `make windows-check`.**

## 4. Test plan

- **Pure unit tests (`test/Spec.hs`)**:
  - `Gzip`: single member, concatenated members, `FNAME`/`FEXTRA`/`FCOMMENT`/
    `FHCRC` flags, bad CRC, truncated stream, decompression bomb hitting cap.
  - `Tar`: ustar regular/dir/symlink entries, GNU long filename (`'L'`), PAX
    extended path/size, bad header checksum rejected, zero-block EOF, truncated
    archive handled cleanly.
  - `Zip` + `Editor`: line-to-member index alignment, `Enter` on file emits
    `EffOpenArchiveMember`, `Enter` on directory/header/encrypted/oversized
    member produces clear status message and no effect, member document is
    always read-only and excluded from `modifiedDocsToSave` and
    `journalLiveKeys`.
  - Nested archive safety: opening a `.zip` inside a `.zip` works at depth 1
    and stops cleanly (or refuses deeper nesting) without infinite recursion.
- **Portability**: `make windows-check` (all file IO stays in `App` using
  portable `System.IO` / `hSeek`).
- **PTY check**: open a generated `.zip` containing a `.json`, `.csv`, and
  `.png`; press `Enter` on each and verify each opens in its native view;
  verify `Ctrl+S` refuses to overwrite the archive and `Ctrl+Shift+S` offers
  to export.

## 5. Risks & mitigations

- **Decompression bombs (`.gz` / `.tar.gz` / ZIP)**: never allocate from an
  untrusted header size; enforce both `maxMemberBytes` and a streaming scan
  cap via `InflateStream` so hostile inputs terminate in bounded time and
  memory.
- **Duplicate synthetic paths / session restore**: synthetic `archive::member`
  paths do not exist on disk. Exclude them from `sessionDocs` and
  `touchRecent` (same discipline as `manualPath`), and guard `pollFs` so
  freshness polling never stats synthetic paths.
- **Slow random access in large `.tar.gz`**: extracting a member near the end
  of a compressed tarball requires streaming from byte 0. Run extraction on
  the background `JLoad` worker with the spinner and cancellation (`Cmedit.Jobs`),
  and cap the scan budget so the user can cancel with `Esc` or tab-switch.
- **Unwanted sniffing of non-tar files**: require both valid ustar magic *and*
  matching octal header checksum (standard unsigned + signed fallback) before
  treating a 512-byte header as TAR.

## 6. Out of scope

- Creating, modifying, or repacking archives (read-only by construction).
- Encrypted member decryption (password prompts, AES/ZipCrypto).
- Non-DEFLATE compression codecs (`.xz`, `.zst`, `.bz2`, `.7z`, `.rar`).
