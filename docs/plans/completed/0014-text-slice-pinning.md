# 0014 — Text slice pinning: when one line keeps a whole file alive

**Theme:** long-session memory; a subtle, measurable retention class
**Status:** ✅ **RESOLVED** (§4.1 shipped; §4.2/§4.3 deliberately deferred —
see below) — implemented 2026-07-26

## Resolved

`EditorState.detach` (a documented `T.copy`) was added and applied at every
boundary where a `Text` outlives the buffer it was sliced from:

| Site | File |
|---|---|
| copy / cut of a selection → `edClipboard` + `EffCopy` | `EditorEdit.hs:410, 422` |
| `findSeed` (selection → Find field → term → history → `SearchReq`) | `EditorFind.hs:87` |
| `pushFindHist` / `pushReplHist` (session-long, persisted to `~/.config/cmedit/history`) | `EditorFind.hs:499, 505` |
| completion candidates (`collectCandidates`) | `EditorEdit.hs:1091` |

The whole-line copy/cut paths append `"\n"`, which already allocates a fresh
array, so they are left alone (copying twice would be waste) — noted in the
code.

**Regression guard** (`test/Spec.hs`, and the reason the test target now builds
with `-with-rtsopts=-T`): copy one 100-character line out of a ~4 MB document,
drop the document, `performMajorGC`, and assert the live heap is under half the
document. Verified to genuinely catch the bug — reverting the `detach` in
`copy` makes it fail with *"live 4 MB after copying one line out of 4 MB"*.
Suite: 1157 passing.

**Deliberately not done, and why:**

- **§4.2 buffer compaction** (deleting 99% of a large file still retains the
  original array). Real, but it is a *different* trade: rewriting surviving
  lines with `T.copy` breaks the pointer-identity fast paths that `HlCache`,
  `Csv.sameGrid` and `bufModified` depend on, turning their O(1) checks into
  O(n) for the next comparison. It needs the soak harness (`0005`) to justify
  a trigger threshold, so it is tracked there rather than guessed at here.
- **§4.3 per-line decoding at load** belongs to `0013` (the load/save path),
  which will settle the copy-vs-slice question with a measurement.

The plan below is the original analysis, kept for the record.

---

---

## 1. The mechanism

With `text-2.x` (GHC 9.6 ships `text-2.0.2`) a `Text` is a `ByteArray#` plus an
offset and a length. `T.take`, `T.drop`, `T.splitOn`, `T.lines` and friends
return **slices that share the parent array** — no copy. That is exactly why
loading is fast, and it is the right default.

The consequence is that a single surviving slice keeps its *entire* parent
array alive. `TextBuffer.splitContent` builds the buffer with
`T.splitOn "\n"`, so **every line of a loaded file is a slice of one array
holding the whole file**.

The codebase already knows about this in two places, with a good comment:

```haskell
-- Keep snippets bounded, and 'T.copy' them: a Text slice shares its source
-- array, so an uncopied snippet would pin the whole decoded file (up to
-- megabytes) in memory for as long as its result row is on screen.
clip l = T.copy (T.take 2000 l)              -- Search.hs:336
```

…and at `Editor.hs:2158` for definition-picker snippets. Those two are the
only `T.copy` calls in `src/`. Every other boundary where a `Text` escapes its
buffer is unprotected.

## 2. Evidence

A 49 MB / 700 000-line file, loaded through `loadFromBytes`, then all but ten
lines dropped, `performMajorGC`, live bytes measured:

| Retained | Live heap |
|---|---|
| whole buffer | 84 MB |
| **10 lines only (680 characters)** | **49 MB** |
| the same 10 lines, `T.copy`'d | ~0 MB |

680 characters of retained text hold 49 MB of heap.

## 3. Where this bites in a real session

### 3.1 The clipboard survives the file

`EditorEdit.hs:407-431`:

```haskell
(ed { edClipboard = txt, edStatus = "Copied" }, [EffCopy txt])
```

`txt` comes from `textInRange`, which for a single-line selection is
`T.take (cc-ca) (T.drop ca line)` — a slice. Copy one line out of a 100 MB
file, close the file: `edClipboard` still pins 100 MB, for the rest of the
session. (A multi-line selection goes through `T.intercalate`, which copies —
so the *small* copy is the dangerous one, which is a nicely counter-intuitive
bug.)

### 3.2 The find/replace terms and their history

`findSeed` (`EditorFind.hs:81-85`) seeds the Find field from the selection via
`textInRange` — same slice. That value flows into `edSearchTerm`, the dialog
field, `edFindHist`/`edReplHist` (kept for the whole session **and written to
`~/.config/cmedit/history` at exit**), and into `SearchReq` for workspace
searches. Any one of those pins the source file forever.

### 3.3 Deleting most of a large file

Even with no escape at all, editing does not release memory: delete 99% of a
100 MB file and the surviving 1% of lines are still slices of the original
array, so the heap stays at ~100 MB until every original line has been
replaced. Undo snapshots make it worse (they legitimately reference the old
lines — but see `0001`; today they are also unbounded).

### 3.4 CSV cells

`Csv` parses cells out of the loaded text, so a table's cells are slices too.
Combined with `csvUndo` grids (`0008`) this compounds, though the array being
pinned is the file the user is looking at, so the practical impact is smaller
than 3.1/3.2.

## 4. Proposal

### 4.1 Copy at the escape boundaries (do this first — it is tiny)

Add a small helper next to the existing convention and use it wherever a
`Text` outlives the buffer it came from:

```haskell
-- | Detach a Text from its source array. A slice pins the whole array it was
-- cut from (see Cmedit.Search's snippet clipping), so anything that outlives
-- the buffer — the clipboard, search/replace terms and their history, dialog
-- field seeds — must be copied.
detach :: Text -> Text
detach = T.copy
```

Sites:

| Site | Why |
|---|---|
| `EditorEdit.hs` copy/cut (4 call sites) → `edClipboard` and `EffCopy` | outlives the document |
| `EditorFind.hs:findSeed` | flows into terms, history, `SearchReq` |
| `pushFindHist` / `pushReplHist` (`EditorFind.hs:492, 497`) | session-long + persisted to `~/.config/cmedit/history` |
| `Dialog` field seeds built from buffer text (rename, go-to-file, quick open) | outlive the dialog |
| `Complete` candidate list | short-lived, but free to copy (≤100 short words) |
| `NavStop`/recents | no text — nothing to do |

The cost is bounded by the size of the copied text, which is by definition
small at these boundaries (or already bounded: search snippets clip to 2 000
characters first — copy *after* clipping, as the existing code correctly
does).

### 4.2 Compaction on demand (second stage)

For 3.3, add a cheap heuristic: track the size of the array a buffer's lines
came from (available at load: `BS.length`), and when `bufChars` falls below,
say, a quarter of it, rewrite the surviving lines with `T.copy` in one pass.
Where to trigger it without adding a per-edit check:

- at save time (the buffer is being traversed anyway), and
- on the existing 2-second `pollFs` tick, gated on a pure predicate so the
  driver only pays a comparison.

One pass of `T.copy` over the lines is O(remaining content), which is by
construction small when the predicate fires.

### 4.3 Consider copying at load (measure, don't assume)

`0013` proposes decoding lines individually from the byte string, which
produces per-line arrays and removes the pinning at the source. That trades
one 49 MB array for 700 000 small ones — more per-line overhead, more GC
objects, but no pinning and a smaller live set after heavy deletion. The
measured live figure was 84 MB for the shared case; the per-line case should
be measured before choosing. **This plan does not depend on that decision** —
§4.1 is worth doing either way, since even per-line arrays are pinned by a
clipboard slice.

## 5. Testing

- **A retention test per boundary.** For each escape site: load a large
  synthetic buffer, perform the action (copy a line, seed a find term), drop
  the document, `performMajorGC`, and assert live bytes are small. This is
  exactly the measurement above and it is mechanical to write once the soak
  harness (`0005`) exists.
- **A regression guard on the convention.** A comment is not a guard; the
  helper name `detach` makes the intent greppable, and a short note in
  `CLAUDE.md`'s gotchas section ("a `Text` that outlives its buffer must be
  `detach`ed") is where a future contributor will actually see it.

## 6. Risks

- `T.copy` on a large value at the wrong boundary would *add* cost. Every site
  above is bounded (a clipboard selection is user-sized; terms are short;
  snippets are clipped first). Do not blanket-copy inside the buffer itself.
- Compaction (§4.2) rewrites line `Text` values, which breaks the pointer
  identity that `HlCache`, `Csv.sameGrid` and `bufModified` use as a fast
  path. Those all fall back to content comparison and stay *correct*, but a
  compaction pass makes the next comparison O(n) instead of O(1) — so
  compaction must be rare and must not run on every save of a normal file.
  Gate it on the ratio test, and document the interaction next to the code.
