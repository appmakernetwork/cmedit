# 0013 — Streaming save and a lower-copy load path

**Theme:** memory spikes on the file-IO boundary
**Status:** ✅ **RESOLVED** — implemented 2026-07-26 (§4.2 "split on bytes"
deliberately not taken; see below)
**Risk (as shipped):** low — landed behind a 14-case round-trip matrix written
*before* the change

## Resolved

**Save is now streaming.** `TextBuffer.bufferBuilder` serialises the buffer
straight into a `Builder` (BOM, lines, separators, optional final newline) and
`writeAtomic` takes a `Builder`, writes it to the temp file with a 64 KiB block
buffer and reports the byte count from `hTell` — so the payload is never
materialised whole. `replaceInFile` goes through the same writer. The
temp-file + rename atomicity is unchanged.

**Load skips normalisation when there is nothing to normalise.**
`detectLineEnding` returning `LF` means the scan found no CR at all, so
`normalizeNewlines`' two whole-file copies are skipped
(`splitContentWith False`). `fromText`, which has no such knowledge, still pays.

| Operation (49 MB, 700 000 lines) | Before | After |
|---|---|---|
| `saveFile` | 117 ms, **440 MB** | **35 ms, 67 MB** |
| `loadFromBytes` | 443 ms, 167 MB | **377 ms**, 167 MB |

Save allocation is down 6.6× — 67 MB for a 49 MB file is roughly one copy, the
floor for encoding UTF-8 out of a line-per-`Text` buffer.

**Guards added** (`test/Spec.hs`, suite 2067 → 2151): a 14-case save/load
round-trip matrix over every combination of LF/CRLF/CR, BOM/no BOM, final
newline/none, plus the empty buffer, a single line with no newline, blank
lines, a trailing empty line, wide/emoji/accented text, an embedded lone CR and
a 5 000-character line. Each case asserts the reported byte count equals the
file size, the BOM is present exactly when requested, the reload reproduces
content/encoding/final-newline, and — the important one — **re-saving what was
loaded reproduces byte-identical output**.

**§4.2 (decode lines individually from the byte string) deliberately not
taken.** The measurement argues against it: a loaded 49 MB file has a live set
of 84 MB with lines as slices of one shared array. Per-line decoding would
replace that single array with 700 000 small ones, each with its own header and
rounding — very likely *more* live memory, in exchange for fixing a pinning
case that only bites when most of a large file is deleted. That trade needs the
soak harness (`0005`) to settle, where it is already tracked alongside the
related compaction question from `0014`.

The plan below is the original analysis, kept for the record.

---

---

## 1. The problem

`TextBuffer.saveFile` materialises the entire file three times before a byte
reaches the disk:

```haskell
saveFile path enc le final b = do
  let txt   = bufferToText le final b          -- 1. one Text of the whole file
      body  = TE.encodeUtf8 txt                -- 2. one ByteString of the whole file
      bom   = if enc == Utf8Bom then … else BS.empty
      bytes = bom <> body                      -- 3. another whole-file copy
  …
  writeAtomic tmp path bytes                   --    BS.hPut of the whole thing
```

`bufferToText` is itself `T.intercalate sep (toList ls)` — a fourth
whole-file intermediate inside `Data.Text`'s builder.

The same shape appears on the way in. `loadFromBytes`:

```haskell
txt        = TE.decodeUtf8With TEE.lenientDecode bs'
le         = detectLineEnding txt                    -- two isInfixOf scans
(ls, fin)  = splitContent txt
  where normalizeNewlines = T.replace "\r" "\n" . T.replace "\r\n" "\n"   -- two full copies
```

`normalizeNewlines` copies the whole file twice even for a pure-LF file (the
common case), before `T.splitOn` copies it once more into lines.

`maxOpenBytes` is 100 MB, so these multipliers apply to files up to that size.

## 2. Evidence

A 49 MB, 700 000-line text file, measured with the `src/`-linked harness:

| Operation | Time | Allocated | Notes |
|---|---|---|---|
| `loadFromBytes` + install | 479 ms | 167 MB | 3.4× the file size |
| live heap after load | — | **84 MB** | 49 MB of content + ~35 MB of `Text`/`Seq` overhead (~50 bytes/line) |
| `saveFile` | 117 ms | **440 MB** | **9× the file size**, for a save that writes 49 MB |
| process peak RSS | — | 249 MB | for a 49 MB file |

The save figure is the striking one: nine times the file size in transient
allocation, all of it in one burst, which on a 100 MB file means ~900 MB of
churn and a corresponding GC spike — during the operation the user is most
anxious about.

## 3. Fix: stream the save

`Data.ByteString.Builder` is already used (the renderer's whole output is a
Builder, `hPutBuilder` is already the emitter). Saving is a natural fit:

```haskell
-- | Serialise a buffer straight into a Builder: no whole-file Text, no
-- whole-file ByteString. The Builder's own chunk buffer (32 KiB by default)
-- bounds the memory used regardless of file size.
bufferBuilder :: Encoding -> LineEnding -> Bool -> Buffer -> Builder
bufferBuilder enc le final (Buffer ls _) =
  (if enc == Utf8Bom then byteString bomBytes else mempty)
  <> go (toList ls)
  where
    sep = byteString (TE.encodeUtf8 (lineEndingText le))
    go []       = mempty
    go [l]      = TE.encodeUtfBuilder l <> (if final then sep else mempty)
    go (l : ls) = TE.encodeUtfBuilder l <> sep <> go ls
```

(`Data.Text.Encoding.encodeUtf8Builder` is the exact primitive wanted and is in
`text`, already a dependency.)

`writeAtomic` takes a `Builder` instead of a `ByteString`:

```haskell
writeAtomic tmp path bld = do
  n <- withBinaryFile tmp WriteMode $ \h -> do
         hSetBinaryMode h True
         hSetBuffering h (BlockBuffering (Just 65536))
         hPutBuilder h bld
         hFlush h
         hTell h            -- byte count, which the caller reports
  renameFile tmp path
  pure n
```

The one thing lost is `BS.length bytes` for the "wrote N bytes" status
message; `hTell` before closing gives the same number without materialising
anything. (Or count in the Builder construction — but `hTell` is simpler.)

Expected effect: allocation drops from ~9× the file size to a constant few
hundred KB; time should improve too, since three copies are removed. The
atomic temp-file + rename semantics are unchanged — that guarantee must not be
touched.

`replaceInFile` (`TextBuffer.hs:650`) has the same whole-file shape and gets
the same treatment for free once `writeAtomic` takes a Builder.

## 4. Fix: a cheaper load

Two independent improvements, in order of value:

1. **Skip `normalizeNewlines` when there is nothing to normalise.** The common
   case is a pure-LF file, where the two `T.replace` passes produce two
   identical copies of the whole file. `detectLineEnding` already scans for
   `\r\n` and `\r`; compute it *first* and skip normalisation entirely when the
   answer is `LF`. That is a two-line change that removes ~2× the file size in
   allocation from every load of a Unix file.
2. **Split on bytes, not on decoded text.** Splitting the `ByteString` at
   `\n` boundaries and decoding each line individually is safe (UTF-8 never
   contains a `0x0A` byte inside a multi-byte sequence), avoids the whole-file
   `Text` intermediate entirely, and produces the per-line `Text` values the
   buffer wants anyway. `BS.split 10` + `map (decodeUtf8With lenient)` is the
   whole implementation for the LF case; CRLF becomes a `stripSuffix "\r"` per
   line, and the rare CR-only file can keep the current slow path.

Item 2 also removes the current asymmetry where a line's `Text` is a slice of
one giant `Text` — worth checking, because if `T.splitOn` produces slices that
share the parent array, then **every line of a loaded file retains the whole
file's byte array** until that line is edited. With `text-2.x` (`Data.Text` is
a UTF-8 `ByteArray` + offset + length) `splitOn` *does* produce sharing
slices. That would mean: edit one line of a 100 MB file, and the original 100 MB
array stays alive as long as any unedited line does — i.e. forever. **This is
worth measuring before anything else in this plan**; if confirmed, per-line
decoding is not an optimisation, it is a memory-correctness fix, and it also
explains part of the 84 MB live figure above.

## 5. Testing

- **Round-trip fidelity is the priority.** The existing save/load tests must
  cover: LF/CRLF/CR, BOM/no BOM, final-newline/no final-newline, empty file,
  single line without newline, embedded lone `\r`, invalid UTF-8 (lenient
  replacement must be byte-identical to today), and a file whose last line is
  empty. Assert `load (save b) == b` and, more strictly, that saving an
  unmodified loaded file reproduces the original bytes.
- **Atomicity.** Assert the temp file is removed on failure and that a failed
  write never leaves the target truncated (simulate with a read-only target
  and with a full filesystem if the harness can, otherwise by injecting an
  exception into the writer).
- **Memory.** In the soak/bench harness, assert save allocation is under a
  small multiple of a *constant*, not of the file size — the property this
  plan is really about.
- **The sharing question (§4 item 2).** A direct test: load a file, keep one
  line, drop everything else, `performMajorGC`, and check live bytes. If they
  are proportional to the file rather than the line, sharing is confirmed.

## 6. Risks

- `hPutBuilder` on a handle in `WriteMode` with block buffering writes in
  chunks; a partial write followed by a crash leaves a partial *temp* file,
  which is then never renamed — the atomicity property is preserved. Do not
  "optimise" by writing directly to the target.
- `hTell` on a buffered handle reports the position after flushing; flush
  before reading it (as above).
- The `Encoding`/`LineEnding` round-trip is the highest-risk surface in the
  whole editor: a bug here silently corrupts user files. Land this behind the
  full round-trip test matrix, not before it.
