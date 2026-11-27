# 0040 — Built-in diff view (unsaved changes, disk changes, `--diff`, and optional Git gutter)

**Theme:** capability — showing what actually changed before saving,
reverting, restoring snapshots, or approving staged workspace replaces
**Status:** proposal
**Estimated effort:** 4–6 days (phase 1 diff engine + unified view: 2–3 days;
phase 2 `--diff` + disk/session/staged-replace entry points: 1–2 days; phase 3
async Git gutter marks: 1–2 days)
**Risk:** low–medium. The diff engine is pure and bounded; the main subtlety is
bounding worst-case LCS cost on huge/divergent files so computing a diff never
stalls the UI or explodes memory.

---

## 1. The problem

CMeDit already maintains multiple versions of files across four real
workflows, yet has no way to show the user *what changed*:

1. **Unsaved edits (`edBuffer` vs `edSavedBuffer`)**: before saving or
   discarding a buffer (especially after a staged workspace **Replace All**
   opened 15 modified tabs), the user cannot inspect their hunks in-editor.
2. **External disk changes (`◆`, `edDiskChanged`)**: when a file changes on
   disk while open, **File ▸ Revert** is a blind leap of faith.
3. **Clean-exit snapshots (`DKSessionChanged`, plan `0030`)**: the *"Files
   Changed Since This Session"* dialog offers *Latest on Disk* vs *As You Left
   Them*, both of which CMeDit already has in memory, without letting the user
   see the diff first.
4. **Ad-hoc comparison (`cmedit --diff a b`)**: comparing two files over SSH
   currently requires leaving the editor for `diff -u`.

## 2. Design

### 2.1 `Cmedit.Diff` — pure, bounded line-diff engine

A leaf module importing only `Data.Sequence`, `Data.Text`, `Data.Vector`/
`Data.Array`, and `Cmedit.Types`:

```haskell
data DiffOp = OpEqual !Int !Int !Text   -- ^ oldLine, newLine, text
            | OpDelete !Int !Text       -- ^ oldLine, text
            | OpInsert !Int !Text       -- ^ newLine, text
  deriving (Eq, Show)

data DiffHunk = DiffHunk
  { dhOldStart :: !Int   -- ^ 0-based start line in A
  , dhOldCount :: !Int
  , dhNewStart :: !Int   -- ^ 0-based start line in B
  , dhNewCount :: !Int
  , dhOps      :: ![DiffOp]
  } deriving (Eq, Show)

data DiffResult = DiffResult
  { drHunks     :: !(Seq DiffHunk)
  , drAdded     :: !Int
  , drRemoved   :: !Int
  , drTruncated :: !Bool  -- ^ True if edit-distance/line budget was hit (fell back to coarse diff).
  } deriving (Eq, Show)
```

**Algorithm & bounds (performance invariants):**

- **Fast prefix/suffix stripping**: compare lines from the top and bottom
  first (using `ptrEq` as a fast path before `(==)`, since `edBuffer` and
  `edSavedBuffer` share unchanged `Text` chunks via `Seq`!). For typical edits
  in a 50,000-line file, this reduces the problem to a handful of lines in
  microseconds.
- **Hash-accelerated Myers $O((N+M)D)$** (with optional Patience unique-line
  anchoring for cleaner code hunks): map remaining distinct lines to `Int`
  equivalence IDs via a `Map Text Int`, then run Myers in `STUArray s Int Int`
  with an explicit edit-distance ceiling `maxEditDist = 4096` and line-count
  cap `maxDiffLines = 100_000`.
- If the budget is exceeded (two completely unrelated 80k-line files), fall back
  immediately to a single delete-all / insert-all hunk with `drTruncated = True`
  and a clear header note — never stall, never allocate $O(NM)$ memory.
- **Inline word/character refinement (display-time only)**: for adjacent
  single-line or matched `OpDelete` / `OpInsert` pairs visible on screen,
  compute common prefix + common suffix offsets in $O(\text{line length})$ so
  the renderer can emphasise the exact changed span within a modified line.

### 2.2 Presentation: unified read-only diff buffer vs dedicated view

Two clean options exist; start with the **unified read-only view** (matching
how `Archive` and `Manual` avoid duplicating Find/Copy/Wrap/Scrollbar
machinery), with minimal structured metadata for navigation and styling:

- **Option A (recommended for Phase 1): Read-only buffer + `Lang = Diff`
  syntax highlighting + `edDiff` hunk-jump metadata.**
  - Format `DiffResult` into standard unified-diff text (`--- a`, `+++ b`,
    `@@ -old,n +new,m @@`, `-`, `+`, ` ` lines) and open it as a read-only
    document (`Lang = Diff`).
  - Adding `Diff` to `Cmedit.Syntax` also immediately gives syntax
    highlighting to every `.diff` / `.patch` file and `cmedit <(git diff)`
    automatically!
  - Attach a lightweight `DiffMeta` (hunk line indices + target file/line map)
    so:
    - **`]` / `[`** (or `Alt+Down` / `Alt+Up`) jump to next/previous hunk;
    - **`Enter`** on any context or `+` line jumps directly to that line in
      the live buffer (switching tabs if open);
    - **Copy (`Ctrl+C`)**, **Find (`Ctrl+F`)**, **Save As / Export** (saving a
      `.patch` file!), word wrap, and scrollbars all work with zero new render
      plumbing.
- **Option B (optional Phase 2 upgrade): Dedicated side-by-side / tinted-row
  `DiffView` mode** if full-row red/green background tinting and inline
  changed-span highlights prove noticeably better than syntax-coloured unified
  diff. The pure `Cmedit.Diff` model serves both unchanged.

### 2.3 Entry points

1. **Active buffer vs saved baseline** (`File ▸ View Unsaved Changes` or
   Command Palette `> diff`):
   - Pure! Both `edSavedBuffer` and `edBuffer` (or `Csv.csvToText` if in CSV
     mode) are already in `Editor`. Zero IO, instant.
   - Works for **staged Replace All** review too: open diff on any modified
     tab, or add a `"Diff All Modified"` action that concatenates hunks across
     all modified documents in the zipper into one reviewable patch buffer!
2. **Active buffer vs file on disk** (`File ▸ Diff Against Disk`, especially
   when `edDiskChanged` / `◆` is set):
   - Emits `EffDiffDisk path`; driver reads `path` (bounded by `maxOpenBytes`),
     runs `diffBuffers` off-thread on `JLoad` if large, and opens the diff tab.
3. **Session-changed dialog (`DKSessionChanged`)**:
   - Add a **"View Diff"** button alongside *Latest on Disk* and *As You Left
     Them* when at least one `ChangedFile` has `Just snap`, generating a
     combined unified diff of all changed files against their clean-exit
     snapshots before the user commits to either choice.
4. **CLI `cmedit --diff FILE_A FILE_B`** (`app/Main.hs`):
   - Parses `--diff` (taking two file arguments), reads both bounded by
     `maxOpenBytes`, and opens the diff view directly (or prints unified diff
     to stdout when stdout is not a tty, matching `--convert`!).

### 2.4 Phase 3 — Async Git gutter marks (optional follow-on)

Once `Cmedit.Diff` exists, showing subtle live change markers in the line
number gutter (`│` green = added, yellow = modified, `▾` red = deletion below)
is small and fits the existing architecture cleanly:

- A new single-flight background job kind `JGit` in `Cmedit.Jobs` (gated by
  a config flag `git-gutter = on | off`, default `on`, silent no-op if `git`
  is not installed or file is not in a repo).
- When a file opens or is saved (or on workspace open), run
  `git show HEAD:<relpath>` once (bounded to `maxOpenBytes`, strict timeout
  like `Cmedit.Lint`) and store only a compact per-document `docGitHead`
  (dropped for files > 4 MiB or when inactive if memory-sensitive) — or compute
  per-line marks `[GutterMark]` against the active `edBuffer` whenever
  `edDocSeq` settles (debounced like linting, ~300 ms).
- `Cmedit.Render.drawTextArea` paints one extra narrow column in the gutter
  (already accounted for by `loGutter` / `computeLayout`, keeping mouse hit
  testing and horizontal offsets in sync).

## 3. Implementation steps

1. **Add `Lang = Diff`** to `Cmedit.Syntax` (highlighting `---`/`+++`, `@@`,
   `+`, `-` lines) + tests. Immediately improves `.diff`/`.patch` and
   `cmedit <(git diff)`.
2. **Implement `Cmedit.Diff`**: prefix/suffix stripping (`ptrEq` fast path),
   bounded Myers in `ST`, unified-diff formatter + `DiffMeta` line map, and
   hunk summaries.
3. **Wire pure & IO entry points**:
   - `View Unsaved Changes` & `Diff All Modified` (pure, in `EditorDoc`/
     `Editor`/`Menu`).
   - `Diff Against Disk` (`EffDiffDisk` in `App`).
   - `DKSessionChanged` "View Diff" button.
   - `cmedit --diff a b` in `app/Main.hs` and `App.hs`.
   - `Enter` on a diff line jumps to source; `[`/`]` step hunks.
4. **(Phase 3)** Add `JGit` background `HEAD` baseline fetch, `git-gutter`
   config setting, and gutter marker column in `computeLayout` + `Render`.

## 4. Test plan

- **Pure unit tests (`test/Spec.hs`)**:
  - `Cmedit.Diff` property/matrix tests: identical buffers (0 hunks, fast
    `ptrEq` path), empty ↔ non-empty, pure inserts, pure deletes, interleaved
    edits, context-line merging across nearby hunks, reconstructing `B` by
    applying `DiffResult` to `A` (round-trip correctness!).
  - Budget guard: two unrelated 20k-line buffers terminate quickly with
    `drTruncated == True` and flat allocation.
  - `Editor` tests: `View Unsaved Changes` reflects edits (including CSV
    table edits via `csvToText`), `Enter` jumps to the corresponding source
    line, diff tab is read-only and excluded from `journalLiveKeys` and
    `modifiedDocsToSave`.
  - `Syntax` tests: `.diff`/`.patch` lexing for all tokens.
  - `Main` arg parsing: `--diff a b` accepted, `--diff` with ≠ 2 files
    reports a clear error.
- **Portability & soak**: `make test`, `make windows-check`, and verify
  diffing a modified 50k-line file does not spike heap or retain stale buffers
  after closing the diff tab (`liveMB` guard).

## 5. Risks & mitigations

- **Retaining old buffers**: a diff tab must hold *detached* (`T.copy`) text
  or formatted lines, never pin a 40 MB `Buffer` or `BS.ByteString` after the
  source document closes (plan `0014` invariant).
- **Binary / huge files**: refuse binary or `> maxOpenBytes` targets cleanly
  in `EffDiffDisk` and `--diff` with a readable status message.
- **Git subprocess latency on Windows or slow network repos (Phase 3)**: run
  strictly off-thread via single-flight `JGit` with a short timeout (e.g.
  2 s), never block opening or typing, and use `mayFetchTool`-style hygiene
  (check `.git` ancestry cheaply before spawning `git` at all).

## 6. Out of scope

- Interactive 3-way merge conflict resolution or hunk staging (`git add -p`).
- Full Git client UI (log, blame, commit, branch management).
- Binary byte-level diffing.
