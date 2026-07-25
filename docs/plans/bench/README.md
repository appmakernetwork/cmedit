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
| `bench csv FILE` | CSV parse, per-keystroke edit, isModified, widths | 0016 |
| `bench paste` | bracketed-paste parser throughput (in-memory `ByteSource`) | 0017 |
| `bench image FILE` | image decode + `scaleRGBA` cost | 0018 |
| `bench search FILE` | literal vs regex matcher throughput | 0019 |
| `bench explorer` | explorer panel cost vs expanded-entry count | index ("checked and healthy") |
| `bench gfx FILE` | kitty/sixel placement encoder cost and payload size | 0018 |

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

## Diagnosing without a profiling build

The profiling libraries are not installed in this environment (`ghc -prof`
fails on `text`), but two RTS facilities work on an ordinary `-O2` binary and
carried most of this investigation:

- `+RTS -hT -i0.2` — heap census by closure type. `UndoState` and `THUNK_1_1`
  in identical byte counts is what confirmed the undo-history leak (0001).
- `+RTS -s` / `GHC.Stats.getRTSStats` (with `-T`) — allocation, live bytes,
  GC counts and pauses.

## Reading the results: two traps this harness had to work around

1. **Lazy construction leaking into the measurement.** Building the test buffer
   is O(file) and, if not forced first, is charged to the first timed block.
   `forceEd` exists for this; the first version of the `edit` benchmark
   reported a false "editing is O(file size)" result until it was added.
2. **Lazy consumption hiding the work.** `Screen`'s cells are boxed and lazy,
   so forcing a `Screen` to WHNF does no rendering at all. The frame
   benchmarks therefore run the real `renderFrame` diff and serialise the
   Builder — which is what forces the cells — rather than touching the array.

Both traps produce plausible-looking numbers, in opposite directions. Any new
measurement added here should state which side of them it sits on.
