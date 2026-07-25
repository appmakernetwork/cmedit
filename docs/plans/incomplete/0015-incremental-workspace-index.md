# 0015 — An incremental workspace index for quick-open and go-to-definition

**Theme:** capability + amortised performance (the long session finally pays off)
**Status:** proposal
**Estimated effort:** 3–4 days
**Risk:** medium (a new cache with invalidation — the classic hard problem;
mitigated by keeping the current walkers as the fallback path)

---

## 1. The observation

Three workspace features each do a **full tree walk, from scratch, every time
they are invoked**:

| Feature | Work per invocation |
|---|---|
| Quick open (Ctrl+P) | `runQuickWalker`: `listDirectory` + `statEntry` for every file under the root, up to `maxQuickFiles` |
| Go to definition (F12) | `runDefWalker`: the same walk **plus reading and lexing every file** whose extension is in `langOf` |
| Workspace find (F4) | `runWalker`: the walk plus reading every non-excluded file |

Quick open is cheap-ish (no file reads). Go-to-definition is not: pressing F12
on a symbol in a 20 000-file repository reads every `.py`/`.js`/`.sql`/`.hs`
file in it, decodes it, and matches every line — and then throws all of that
away. Press F12 again on a different symbol and it does the whole thing again.

In a long session — the scenario this whole plan set is about — a developer
presses F12 dozens of times an hour. The information being recomputed
(what files exist; which lines look like definitions) changes rarely, and the
editor **already knows when it changes**: `App.pollFs` stats every expanded
directory every 2 seconds, and the explorer re-lists on mtime movement.

## 2. Proposal

A per-workspace index, built once per session (lazily, on first use), kept
fresh by the existing freshness poll, and discarded when the folder closes.

```haskell
-- Cmedit.Index (a driver-side module; the pure model only ever sees results)
data WsIndex = WsIndex
  { wsRoot   :: !FilePath
  , wsFiles  :: !(Seq FilePath)                    -- ^ relative paths, for quick open
  , wsDefs   :: !(HashMapish Text [DefSite])       -- ^ identifier -> definition sites
  , wsStamp  :: !(Map FilePath (DiskTime, Int))    -- ^ per-file (mtime, size) at index time
  , wsState  :: !IndexState                        -- ^ Building n%, Ready, Stale [paths]
  }
```

Only `containers` is needed (`Data.Map.Strict`); no new dependency.

### 2.1 Building

Reuse `runScan` — the generic pooled walk that already backs all three
walkers. The index build is one more `ScanSpec` whose matcher extracts *every*
definition-shaped line (`Definition.defLineCols` generalised from "sites of
name X" to "all definition sites"), rather than sites of one identifier. The
walk cost is exactly today's F12 cost, paid **once**.

Build lazily on the first quick-open or F12 (not at startup — startup must stay
instant), streaming progress through the existing `searchQ`/`SMDef*` message
pattern so the picker fills as it builds, exactly as today.

### 2.2 Freshness

`pollFs` already stats expanded explorer directories every 2 s. Extend it (or
add a sibling pass, gated on the index existing) to:

- re-list an indexed directory whose mtime moved → diff against `wsStamp` →
  queue the changed/added files for re-indexing, drop the removed ones;
- re-index a file when the editor itself saves it (the `onSaved` path already
  emits `EffLintNow`; an `EffIndexFile` alongside it is free);
- re-index open-but-unsaved documents from their buffers on demand at query
  time, which is what `goToDefinition` already does for correctness.

Directories that are *not* expanded in the explorer are not polled today. Two
honest options: (a) poll indexed directories regardless of expansion (a few
hundred `stat` calls every 2 s is still nothing next to a keystroke), or
(b) accept staleness for unexpanded subtrees and re-validate a result when the
user opens it. Prefer (a) — it is simpler and measurably cheap.

### 2.3 Querying

- **Quick open** filters `wsFiles` — the fuzzy matcher is already written and
  already re-ranks only on query edits (`qoRescore`). With an index, the
  picker opens with results already present instead of streaming a walk.
- **Go to definition** becomes a map lookup plus the existing open-document
  scan for unsaved edits. Sub-millisecond instead of a tree read.
- **Workspace find** cannot use the index (arbitrary terms), but it *can* use
  `wsFiles` to skip the directory walk and go straight to the grep pool, which
  removes the `listDirectory`/`statEntry` half of its cost.

### 2.4 Bounds (this is a long-session plan — it must not become the leak)

- Cap `wsFiles` at `maxQuickFiles` (already exists) and `wsDefs` at a total
  site count (say 200 000) with a clearly-surfaced "index truncated" state.
- Estimate and cap memory: a definition site is (path index, line, column,
  clipped snippet). Snippets **must** be `T.copy`'d — see `0014`; the existing
  `DefItem` construction already does this, which is the pattern to follow.
- Drop the whole index when the folder closes, and on an explicit
  "Reindex workspace" command (Help/View menu) for when something goes wrong.

## 3. Why this is worth doing beyond speed

It makes two features that are currently *scoped by cost* become cheap enough
to be scoped by usefulness:

- **Workspace symbol search** ("go to symbol in workspace", Ctrl+T) is a
  ten-line addition once `wsDefs` exists — it is the same picker UI as
  `DefPick` with a fuzzy query over the keys.
- **Find references** becomes practical as an index-assisted narrowing (search
  only files that mention the identifier), instead of a full grep.

Both fit the existing picker/`openMatch`/`edPendingJump` machinery.

## 4. Testing

- **Pure:** the index data structure and its diff (`wsStamp` vs a fresh
  listing) — added/changed/removed files produce the right work list.
- **Pure:** definition extraction over a corpus per language, asserting the
  generalised "all definitions" extractor agrees with today's "definitions of
  X" filter for every X in the corpus. This is the test that lets the index
  replace the walker without changing behaviour.
- **Integration:** build an index over a synthetic tree, mutate files on disk,
  and assert the poll picks up each mutation class (edit, create, delete,
  rename, directory rename).
- **Fallback:** with the index disabled (config key `workspace-index = off`),
  every feature must behave exactly as today. Keeping the walkers alive as the
  fallback is what makes this plan safe to land.
- **Soak:** the index must be flat in memory across a long session with many
  file changes — the `wsStamp` map is the thing to watch.

## 5. Risks

- **Invalidation bugs produce wrong answers, silently.** Mitigate by making
  every index-sourced result *verified on open*: `openMatch` already jumps to a
  (path, line, col); if the line no longer matches, fall back to a targeted
  re-scan of that file. Users forgive a slightly stale list; they do not
  forgive being sent to the wrong line.
- **Scope creep toward an LSP.** This is deliberately ctags-level, matching the
  existing `Cmedit.Definition` design. Anything requiring a real parser belongs
  in a different plan (and probably a different process).
- **First-use latency.** The first F12 in a session still pays the full walk.
  Consider starting the build opportunistically when a folder is opened *and*
  the editor has been idle for a few seconds — the idle detection already
  exists in the event loop's timer structure.
