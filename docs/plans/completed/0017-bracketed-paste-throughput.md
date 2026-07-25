# 0017 — Bracketed paste: bulk reads, a rolling terminator, and a cap

**Theme:** stability under a burst; memory spike with no bound
**Status:** ✅ **RESOLVED** — implemented 2026-07-26
**Risk (as shipped):** low

## Resolved

`ByteSource` gained `srcChunk` (hand over everything buffered at once) and
`srcPushBack`; `readPaste` now scans whole chunks with `BS.breakSubstring`,
carrying the last five bytes of a terminator-free chunk so a terminator
straddling a boundary is still found, and pushing back whatever followed the
terminator in the same read. Payloads are capped at `maxPasteBytes` (32 MiB) —
the rest is still drained so the parser stays in sync — and an over-long paste
arrives as `KPasteTruncated`, which `update` normalises to a plain paste plus a
status note, so every consumer (buffer, dialog field, CSV cell, search field)
keeps working.

| Paste size | Before | After |
|---|---|---|
| 256 KiB | 22 ms, 69 MB | 1 ms, ~0 MB |
| 1 MiB | 121 ms, 282 MB | 2 ms, 4 MB |
| 4 MiB | 592 ms, 1 142 MB | **8 ms, 22 MB** |

74× faster and 52× less allocation, and the payload size is now bounded by us
rather than by whatever the terminal chooses to send. Ordinary keystrokes are
untouched (200 000 keys in 5 ms).

Guards added (suite 1164 → 1183): reassembly at chunk sizes 1–4096 (so the
terminator is split at every offset), "paste then key" at four chunk sizes (the
push-back regression), a partial terminator inside the payload, an unterminated
paste at EOF, lenient UTF-8, the empty paste, and the truncation normalisation.

The plan below is the original analysis, kept for the record.

---

---

## 1. The problem

`Input.readPaste` accumulates the entire paste payload as a `[Word8]`,
checking for the terminator by rebuilding a six-element list per byte:

```haskell
readPaste src = go []
  where
    terminator = reverse [0x1b,0x5b,0x32,0x30,0x31,0x7e]   -- ESC[201~ reversed
    go acc = do
      mb <- srcNext src
      case mb of
        Nothing -> pure (finish acc)
        Just b  -> let acc' = b : acc
                   in if take 6 acc' == terminator          -- allocates per byte
                        then pure (finish (drop 6 acc'))
                        else go acc'
    finish acc = KPaste (decodeUtf8Bytes (reverse acc))
```

Per pasted byte this does: one `srcNext` (an `IORef` read, a `BS.uncons`, an
`IORef` write), one boxed `Word8`, one cons cell, a six-cell `take` list and its
comparison. Then the whole thing is `reverse`d and packed.

There is also **no upper bound**: a `cat bigfile` into a bracketed-paste
terminal, or a mis-click paste of a large clipboard, streams straight into a
Haskell list.

## 2. Evidence

Measured through the real parser with an in-memory `ByteSource` (no terminal),
so the numbers are the parser's own cost:

| Paste size | Time | Allocated |
|---|---|---|
| 64 KiB | 6 ms | 16 MB |
| 256 KiB | 22 ms | 69 MB |
| 1 MiB | 121 ms | 282 MB |
| 4 MiB | 592 ms | 1 142 MB |

That is **279 bytes of allocation per pasted byte** and ~145 ms per MiB,
linear as expected — the problem is the constant, not the complexity. A 16 MiB
paste costs ~4.5 GB of allocation and a live `[Word8]` list of roughly a
gigabyte before a single character reaches the buffer.

For scale, ordinary typing through the same parser is fine: 200 000 plain
keystrokes in 4 ms (20 ns and 175 bytes per key). This is specifically the
paste path.

## 3. Fix

### 3.1 Add a bulk primitive to `ByteSource`

```haskell
data ByteSource = ByteSource
  { srcNext        :: IO (Maybe Word8)
  , srcNextTimeout :: Int -> IO (Maybe Word8)
  , srcChunk       :: IO BS.ByteString   -- ^ Whatever is buffered, or one fresh read; empty on EOF.
  }
```

`mkHandleSource` already keeps a pending `ByteString` in an `IORef`; `srcChunk`
hands the whole pending buffer over (and refills when it is empty) instead of
peeling one byte at a time. Every other parser path keeps using `srcNext`
unchanged.

### 3.2 Scan chunks, not bytes

```haskell
readPaste :: ByteSource -> IO Key
readPaste src = go mempty 0
  where
    end = BS.pack [0x1b,0x5b,0x32,0x30,0x31,0x7e]   -- ESC[201~
    go acc !n = do
      chunk <- srcChunk src
      if BS.null chunk then pure (finish acc False) else
        case BS.breakSubstring end chunk of
          (before, rest) | BS.null rest ->
            -- No terminator here. Keep all but the last 5 bytes (a terminator
            -- may straddle the chunk boundary) — carry those into the next scan.
            …
          (before, rest) -> do
            pushBack src (BS.drop 6 rest)            -- bytes after the paste
            pure (finish (acc <> byteString before) True)
```

`BS.breakSubstring` is a tuned scan; the straddle case is handled by retaining
the last five bytes of a terminator-free chunk and prefixing them to the next
one (the classic streaming-delimiter carry). Accumulate with a
`Data.ByteString.Builder` (already a dependency, already used for output) or a
reversed list of chunks — either way it is O(chunks), not O(bytes).

`pushBack` is needed because a paste terminator can be followed by more input
in the same read; `mkHandleSource`'s `IORef` makes this a one-line addition.

Expected: allocation proportional to the payload (one copy, ~1 byte per byte
plus chunk overhead) instead of 279×, and time dominated by the `decodeUtf8`
rather than the scan.

### 3.3 Cap it

```haskell
-- Largest bracketed-paste payload accepted. Past this the rest of the paste is
-- drained and discarded, and the editor reports the truncation — the
-- alternative is an unbounded allocation driven by whatever the terminal sends.
maxPasteBytes :: Int
maxPasteBytes = 32 * 1024 * 1024
```

On overflow: keep the first `maxPasteBytes`, keep draining until the
terminator (so the parser stays in sync with the byte stream — critical, or
every subsequent key is garbage), and return a `KPaste` plus a flag the editor
turns into a status-line notice. A new `KPasteTruncated` constructor is the
cleanest way to carry that without changing `KPaste`'s meaning; the pure model
then sets the status like any other message.

Note the interaction with `maxOpenBytes` (100 MB): a paste is currently
*less* bounded than a file open, which is the wrong way round.

## 4. Testing

- **Parser unit tests** with an in-memory `ByteSource` (the benchmark already
  builds one in ~6 lines — promote it into the test suite):
  - payload with no terminator → EOF handling unchanged;
  - terminator split across a chunk boundary at each of the 5 offsets;
  - bytes following the terminator are not swallowed (type a key immediately
    after a paste in the same read — this is the regression the `pushBack`
    prevents);
  - a payload containing bytes that look like a partial terminator
    (`ESC [ 2 0 1` without `~`);
  - invalid UTF-8 in the payload decodes leniently, exactly as today;
  - a payload over `maxPasteBytes` truncates *and* leaves the parser in sync.
- **Throughput guard:** a 4 MiB paste must complete in well under 100 ms
  (today: 592 ms).
- **Soak:** paste repeatedly in the PTY harness (`0005`) and assert RSS returns
  to baseline.

## 5. Risks

- Getting the straddle/pushback logic wrong desynchronises the input parser,
  which is a much worse failure than slow paste. The boundary tests above are
  the whole safety argument — write them first.
- `KPaste` is already `Text` (the type was chosen so "giant pastes must not
  materialise as a char list", per the comment in `Cmedit.Types`) — this plan
  finishes that intent on the *input* side, where the char list still exists.
