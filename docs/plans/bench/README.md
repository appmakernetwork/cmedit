# Benchmark harness for the plans in `docs/plans/`

Every measurement quoted in the plan documents came from `Bench.hs`, compiled
against `src/` with the same flags the real binary uses (plus `-T` so
`GHC.Stats` works):

```sh
ghc --make docs/plans/bench/Bench.hs -isrc -iplatform/posix cbits/cmedit_term.c \
    -o /tmp/bench -threaded -O2 -rtsopts -with-rtsopts=-T -outputdir /tmp/bench-out \
    -package base -package bytestring -package text -package containers -package array \
    -package process -package stm -package directory -package filepath -package mtl \
    -package time -package unix \
    -XLambdaCase -XOverloadedStrings -XBangPatterns -XTupleSections \
    -XScopedTypeVariables -XMultiWayIf -XRecordWildCards -XNamedFieldPuns -w
```

Modes:

| Command | What it measures | Used by |
|---|---|---|
| `bench undo N` | live heap after N snapshot-pushing edits | 0001 |
| `bench lex2` | `lexLine` cost vs line length, per language | 0002 |
| `bench frame` | render + diff + emit, scrolling | 0002, 0003 |
| `bench type` | full driver cycle per keystroke, editor threaded | 0002, 0003 |
| `bench decomp` | where a frame's time/allocation goes | 0003 |
| `bench edit2` | `update` cost vs file size | index ("checked and healthy") |
| `bench wrap` | word-wrap vs horizontal-scroll paths | index ("checked and healthy") |
| `bench io FILE` | load/save time and allocation on a large file | 0013 |
| `bench share FILE` | Text slice pinning: heap retained by 10 lines | 0014 |
| `bench csv FILE` | CSV parse, per-keystroke edit, isModified, undo/redo, widths, cell/text positions, `sessionShape` | 0016, 0026, 0028, 0029 |
| `bench csvlive FILE lines\|text` | live heap + RSS of one CSV open path | 0026 |
| `bench paste` | bracketed-paste parser throughput (in-memory `ByteSource`) | 0017 |
| `bench image FILE` | image decode + `scaleRGBA` cost | 0018 |
| `bench search FILE` | literal vs regex matcher throughput | 0019 |
| `bench explorer` | explorer panel cost vs expanded-entry count | index ("checked and healthy") |
| `bench gfx FILE` | kitty/sixel placement encoder cost and payload size | 0018 |

`gen_csv.py` builds the corpus the CSV modes are measured on — a deterministic
~32 MB table with quoted fields, embedded delimiters, doubled quotes,
multi-line quoted cells and ragged rows, in LF or CRLF. Generated rather than
found, so 0016's and 0026's numbers can be reproduced:

```sh
python3 docs/plans/bench/gen_csv.py /tmp/big.csv 33554432
```

`bench csvlive` exists because the two CSV open paths cannot be compared for
live heap in one process — whichever runs first is still reachable — so it
retains exactly one of them, alongside the buffer, which is the state the
editor is actually in with a table open.

## The PTY probes

`pty_startup.py` and `pty_rss.py` drive the real `./cmedit` binary over a
pseudo-terminal (the technique `CLAUDE.md` documents for verifying interactive
behaviour), so they measure the shipped program rather than a harness:

```sh
python3 docs/plans/bench/pty_startup.py /path/big.txt /path/big.csv
python3 docs/plans/bench/pty_rss.py /path/big.csv     # RSS sampled every 2 s
```

`pty_ratchet.py` opens a file, closes it with Ctrl+W, types, and samples RSS at
each step — the probe that showed 2.5 GB still resident after the document was
closed, and the acceptance test for the idle collection that fixed it. Current
output for a 32 MB CSV: 2507 MB after open, still 2507 MB at 20 s idle, **33 MB**
once the collection fires at ~30 s.

`pty_startup.py` reports time-to-first-output and RSS per opened file;
`pty_rss.py` samples RSS while the editor sits idle — which is how the
"2.5 GB held indefinitely after opening a 32 MB CSV" finding (0016 / 0007 §2b)
was made. Both send Ctrl+Q and clean up.

## The journal integration test

`pty_journal.py` is the PTY half of 0011 §4 — a pass/fail test rather than a
measurement, and the only one here that reads the screen (the recovery prompt
is a dialog), so it carries the small VT emulator `CLAUDE.md` describes:
cursor positioning, printable UTF-8, erases, DECSTBM band scrolling and REP,
with OSC/DCS/APC strings skipped. It answers none of the startup capability
queries, so the editor stays on the portable emission path.

```sh
python3 docs/plans/bench/pty_journal.py     # ~11 s, exit 1 if a scenario fails
```

Four scenarios, each in a throwaway `HOME` (with `XDG_*` pointed inside it) so
the developer's own `~/.cache/cmedit/journal` is never read or written:

| Scenario | Asserts |
|---|---|
| crash, restart, Recover, Save | a `SIGKILL` (not `SIGTERM`, which runs the graceful path) leaves the `.cmj`; the next startup offers it; Recover + Ctrl+S produces byte-identical content |
| save removes the journal | Ctrl+S sweeps that document's journal away |
| clean exit removes journals | quit-and-discard empties the directory, and the next startup shows no dialog |
| Keep for later preserves them | a journal nothing adopted survives the session that declined it |

Every wait is a poll with a wide deadline, not a sleep: the write-behind is
debounced 2 s after the last edit batch and the sweep runs after a key batch,
so the test asks "has it happened yet" up to 8 s rather than guessing a
duration. (2 s is the floor of `journalDelayUs`, which spaces the passes
further apart for buffers large enough for the traffic to matter — see plan
0027. These files are a few bytes, so they sit on the floor, which is exactly
why this test still pins the shipped behaviour.) Two invariants are checked along the way that are easy to lose —
the journal body is the buffer's lines joined with LF and nothing else, and
neither journalling nor recovery ever writes the real file before a save.

## The session-restore integration test

`pty_session.py` is the PTY half of 0025, and it *imports* `pty_journal.py`
rather than copying it — the VT emulator, the `Session` PTY helper, the
throwaway-`HOME` isolation (with every `XDG_*` pointed inside it) and the
poll-with-deadline discipline are all the journal test's. The two read the same
screens and the same `~/.config` directory, so they must not be allowed to
drift apart.

```sh
python3 docs/plans/bench/pty_session.py    # ~5 s, exit 1 if a scenario fails
```

Five scenarios, each in its own throwaway `HOME`, so the developer's own
`~/.config/cmedit/session` is never read or written and the script is safe to
run concurrently with itself:

| Scenario | Asserts |
|---|---|
| round trip: files, order, cursor | a clean exit writes the shape (no `folder:`, both paths in order, `active: 1`, the active file's line) and `--restore` puts back the same files, the same active document and the same cursor — read off the status bar's `Ln 3, Col 1` |
| partial restore counts the gone | a deleted file is skipped and counted (`Restored 1 of 2 files`), not recreated as an empty buffer, and does not survive into the next session |
| config key, and only argless | `restore-session = true` restores a start with no arguments, and `cmedit c.txt` deliberately does not — checked by quitting and finding only `c.txt` recorded |
| crash: journal into restored doc | a `SIGKILL` leaves both a session and a `.cmj`; `--restore` brings the file back *and then* offers the journal, so Recover lands in the restored document rather than a second copy of it (the session afterwards records the path once, not twice) |
| no session to restore | `--restore` with nothing usable says `No previous session to restore` and still hands over a working untitled buffer |

The session file is parsed by a second, independent parser in the test:
asserting with the same code the editor writes with would assert nothing about
the format. Every wait is a poll with a wide deadline — the session is rewritten
after the batch that changes its *shape* and once more on exit, so the test asks
"has it happened yet" rather than guessing a duration.

The crash scenario pins both persist paths, because this test originally found
a hole in one of them: the session used to be persisted only when its shape
*changed* from the startup baseline, so a run that opened files and then only
typed never wrote the file at all, and a `SIGKILL` left the *previous*
session's file for `--restore` to trust. The startup shape is now written
unconditionally before the first frame, and the scenario asserts that (the
session is on disk, correct, before any shape change), then makes a real shape
change (a second file, closed with Ctrl+W) before the `SIGKILL` so the
change-driven persist is exercised too.

## The per-workspace session integration test

`pty_workspace.py` is the PTY half of 0030, and it imports the same machinery
one level further along: the VT emulator, the `Session` helper and the
throwaway-`HOME` isolation from `pty_journal.py`, and the config writer and
clean-quit helper from `pty_session.py`. Where `pty_session.py` is the v1→v2
regression net — its five scenarios open files but never a *folder*, so they
all still land in the folderless `~/.config/cmedit/session` — this one is
about what happens once there is more than one workspace.

```sh
python3 docs/plans/bench/pty_workspace.py   # ~2 s, exit 1 if a scenario fails
```

Seven scenarios, each in its own throwaway `HOME`, so the developer's own
`~/.config/cmedit/sessions` and `~/.cache/cmedit/snapshots` are never read or
written and the script is safe to run concurrently with itself:

| Scenario | Asserts |
|---|---|
| two workspaces, two sessions | a clean exit in folder A and one in folder B leave **two** files under `sessions/` (and no folderless `session` file at all), each recording its own folder and its own paths with v2 mtimes; `--restore` from A's directory gives A and from B's gives B, with no origin note either time; two restores and two more exits later the two files still have not merged |
| fallback names the folder | from a third directory with no session of its own, `--restore` picks the most recently written session and says `Session restored from ~/zsbravo` — the note that makes the fallback non-astonishing |
| File menu restores a session | with A's file already open and no folder, the File menu lists `zsbravo (2 files)` and `wsalpha (1 file)`; pressing `w` (the mnemonic `assignSessionMnemonics` can give it, since the static File menu claims `n o f i s a l v c d t x` and the recents take digits) restores A onto the live editor — the folder opens, the already-open file is switched to rather than duplicated, and neither the journal-recovery nor the changed-files prompt appears |
| changed: Latest on Disk | a clean exit writes one `.cmj` per open document plus a `stamp` equal to the session's `closed:`; after the file is rewritten underneath it, `--restore` raises `Files Changed Since This Session` listing `◆ a.txt` with both answers; Esc (= button 0) leaves the newest bytes in an unmodified buffer that quits without a confirmation |
| changed: As You Left Them | the same setup answered the other way installs the snapshot as a **modified, unsaved** buffer carrying ● and ◆, with the *newer* bytes still on disk — until an explicit Ctrl+S writes the session's version over them |
| crashed session, stale stamp | a clean exit at T1, then a second session under the same key re-stamped by a shape change and `SIGKILL`ed at T2, leaves T1's snapshot set intact and unusable: the prompt still reports the changed file but in its single-button form (`— no saved copy from that session`, no *As You Left Them*), and the content comes from disk |
| journal = off, no snapshots | a clean exit writes no snapshots and never creates `~/.cache/cmedit` at all, the session file is unaffected (it holds paths, cursors and mtimes, never content), and the changed-files prompt degrades to the same single-button form |

Two things about the harness are worth knowing before adding a scenario.
`pty_journal.Session` chdirs its child to `$HOME`, and 0030 §2.3 made
`--restore` **cwd-scoped**, so the starting directory is an input to almost
every scenario here — `start(home, args, cwd=…)` patches that one call for the
duration of the fork rather than forking a second copy of the helper. And
every path handed to the editor is absolute, for the reason `pty_session.py`
records: a relative path resolved against the wrong one of the four
directories these scenarios start in opens a new empty buffer instead of
failing.

The session file is parsed by a third independent parser (`pty_session.py`'s,
generalised over *where* the file is, which is the whole of 0030) and the
session file's *name* is deliberately never recomputed here — listing the
directory is what proves the two workspaces got two files, where recomputing
the hash would only assert that two copies of the same formula agree.
`rewrite()` re-writes a file until its mtime actually moves: the feature turns
on `recorded != current`, so a rewrite the filesystem gave the same timestamp
would silently make the scenario assert the opposite of what it says.

## Diagnosing without a profiling build

The profiling libraries are not installed in this environment (`ghc -prof`
fails on `text`), but two RTS facilities work on an ordinary `-O2` binary and
carried most of this investigation:

- `+RTS -hT -i0.2` — heap census by closure type. `UndoState` and `THUNK_1_1`
  in identical byte counts is what confirmed the undo-history leak (0001).
- `+RTS -s` / `GHC.Stats.getRTSStats` (with `-T`) — allocation, live bytes,
  GC counts and pauses.

## Reading the results: three traps this harness had to work around

1. **Lazy construction leaking into the measurement.** Building the test buffer
   is O(file) and, if not forced first, is charged to the first timed block.
   `forceEd` exists for this; the first version of the `edit` benchmark
   reported a false "editing is O(file size)" result until it was added.
2. **Lazy consumption hiding the work.** `Screen`'s cells are boxed and lazy,
   so forcing a `Screen` to WHNF does no rendering at all. The frame
   benchmarks therefore run the real `renderFrame` diff and serialise the
   Builder — which is what forces the cells — rather than touching the array.
3. **A loop-invariant call, floated out of the repeat loop.** `[ … | i <- [1..100],
   f v … ]` computes `f v` **once**: full laziness hoists it, and the block
   times one call plus 99 comparisons. This is how `0016` recorded
   `Csv.isModified` at 0.04 ms when it cost 2.5 ms and 14 MB — for two plans.
   The fix is to make each iteration's argument depend on `i` in a way that
   costs nothing (`Csv.setCursor 0 (i mod 3) v`, an O(1) record update that
   touches neither grid).

Trap 2 is the one that keeps happening, because "I forced the result" and "I
forced the work" look identical in the source. It has now produced a wrong
answer in three plans: `renderEditor` forced to WHNF renders nothing (`0003`);
a probe that ended on the `Screen`'s *width* field reported 0.6 ms for a
keystroke that cost 390 ms, and sent `0028` looking in the wrong half of the
program; and `Csv.cellTextPos` returns a **tuple**, so `pure $!` on it computes
neither component and reports 0 ms for a 383 ms call (`0029`). If a probe's
answer is a constructor — a tuple, a record, a `Screen`, a `Seq` — forcing it
proves nothing. Force something arithmetic that every component had to be
computed to produce.

All three produce plausible-looking numbers, in opposite directions. Any new
measurement added here should state which side of them it sits on.
