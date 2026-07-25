# 0019 — Case-insensitive search: stop lowercasing every line

**Theme:** throughput of the default search path
**Status:** ✅ **CLOSED — investigated, implemented, measured, and reverted.**
The proposed optimisation does not pay off. Recorded here so nobody spends the
half-day again. 2026-07-26

## Outcome: both proposed fixes measured *worse*

**1. The first-character pre-filter.** Implemented as described in §3 and
measured over the same 49 MB corpus:

| Case | Before | With pre-filter |
|---|---|---|
| term whose first char is common (every line) | 227 ms | 186 ms† |
| term whose first char appears nowhere | — | **239 ms**, 63 MB (vs 639 MB) |

† and only because that term matched nothing after the first character.

The catch is correctness: to avoid false negatives on non-ASCII the predicate
has to be `c == lower || c == upper || toLower c == lower` — U+0130's simple
lowercase is `i`, so an ASCII-only comparison would silently miss it. That
`toLower` per character costs about what the fold it is avoiding costs. Skipping
*every* line was not faster than folding *every* line.

**2. `T.map toLower` instead of `T.toLower`.** A microbenchmark said 11× faster
and allocation-free (11 ms / 0 MB against 119 ms / 107 MB). In context it was
**2× slower** — 476 ms against 220 ms. The microbenchmark was fusing
`length . map` into a count that never materialised the folded text; `T.toLower`
has a specialised UTF-8 loop while `T.map` re-encodes per character through the
stream machinery. (A textbook instance of the second measurement trap in
`bench/README.md` — measured by this very investigation, and still walked into.)

**Where the time actually goes:** a case-*sensitive* search over the same corpus
is 56 ms, so the floor is `T.lines` plus the per-line scan, not the folding. The
remaining ~170 ms is spread thin; there is no single lever.

## What was kept

- The **equivalence tests** (`test/Spec.hs`): `lineMatches` is checked against a
  straightforward fold-everything reference across a corpus of Cyrillic, CJK,
  emoji, `ß`, Turkish dotted/dotless I, tabs and empty lines, for every
  combination of case-sensitivity and whole-word. They cost nothing and they are
  what would make a *future* attempt safe.
- Comments in `Search.lineMatches` and `Search.normCase` recording both dead
  ends with their numbers, so the next reader does not re-run this.

## Known limitation, unchanged (documented, not fixed)

`T.toLower` is full Unicode lowercasing and can change length — U+0130 becomes
two code points — while match positions are indices into the *folded* line. A
line containing `İ` can therefore report columns shifted by one for matches
after it. Fixing it properly means a length-preserving fold, which is the
`T.map` path that measured 2× slower. Noted rather than traded for.

The plan below is the original analysis, kept for the record.

---

---

## 1. The measurement

A 49 MB corpus (700 000 lines) through `Search.fileMatchesWith`:

| Matcher | Time | Allocated |
|---|---|---|
| literal, **case-insensitive** (the default) | **222 ms** | **646 MB** |
| literal, case-sensitive | 61 ms | 438 MB |
| regex `^line [0-9]+` | 16 ms | 75 MB |
| regex `[a-h]{4} [a-h]{4}` | 35 ms | 175 MB |

Two things stand out. The regex engine is *fast* — the Thompson-NFA/Pike-VM
does 49 MB in 16–35 ms, comfortably beating the literal path, which is a good
result for a from-scratch engine and worth knowing. And the **default** search
mode (`edSearchCase` starts `False`) is the slowest thing in the table, at 3.6×
the case-sensitive cost.

## 2. Why

```haskell
lineMatches cs ww term line =
  [ (i, len) | i <- scanMatches False ww (normCase cs term) (normCase cs line) ]

normCase cs = if cs then id else T.toLower
```

`T.toLower line` allocates a **complete lowercased copy of every line of every
file searched**, whether or not it contains a match — and the overwhelming
majority do not. At 13 bytes of allocation per corpus byte, that copy is the
dominant cost.

(The term is also re-lowered per line, but it is tiny; GHC may or may not float
it out, and pre-lowering it in `compileMatcher` is free to do anyway.)

## 3. Fix: filter before you fold

Keep `scanMatches` exactly as it is — it is a clean `T.breakOn` loop — and give
the case-insensitive path a cheap pre-filter so the copy only happens for lines
that could match:

```haskell
-- Case-insensitive literal search. Lowercasing every line allocates a copy of
-- the whole corpus; instead, reject the ~99% of lines that cannot contain the
-- term by scanning for either case of its first character, and only fold the
-- case of lines that survive.
lineMatches False ww term line
  | not (couldMatch line) = []
  | otherwise             = [ (i, len) | i <- scanMatches False ww loTerm (T.toLower line) ]
  where
    loTerm  = T.toLower term          -- hoisted into compileMatcher in practice
    c0      = T.head loTerm
    c0Upper = toUpper c0
    couldMatch t = isJust (T.find (\c -> c == c0 || c == c0Upper) t)
```

`T.find` is a single scan with no allocation. For a corpus where few lines
contain the term's first letter in either case this eliminates nearly all the
copying; for a term starting with a very common letter it degrades to today's
behaviour plus one cheap scan.

A stronger version, if the profile still shows the copy: compare
case-insensitively **in place** by walking both texts with `T.uncons` and
`toLower` per character at the candidate offsets only. That removes the copy
entirely but duplicates `scanMatches`' whole-word logic, so do it only if
measurement justifies the duplication.

Also worth doing while here:

- **Pre-lower the term in `compileMatcher`.** `MLit` already carries the flags;
  carry the folded term too, so it is computed once per search rather than once
  per line.
- **Use `toCaseFold`, not `toLower`, if the semantics are meant to be Unicode
  case-insensitive.** These differ for e.g. `ß`/`SS` and Turkish dotted `I`.
  The current behaviour is `toLower`; changing it is a *behaviour* decision,
  not a performance one — flag it, pick one deliberately, and note it in the
  manual. (`Search.clip`-style comments in this module suggest the author
  values this kind of note.)

## 4. Testing

- **Equivalence.** Property-style: for a corpus of lines and terms (ASCII,
  accented Latin, Greek, Turkish `I`/`ı`, `ß`, CJK, emoji, empty term, term
  longer than the line), assert the new `lineMatches` returns exactly the same
  list as the old one. Keep the old implementation in the test as the oracle.
- **Whole-word flag** interacts with the pre-filter (a line can contain the
  first character but fail the boundary check) — cover both.
- **Throughput guard**: 49 MB corpus, case-insensitive literal, under 100 ms.

## 5. Note on what this does *not* need to change

The walker's parallel structure (bounded `TBQueue`, ≤4 grep workers), the
per-file caps and the binary sniffing are all sound and are not implicated by
these numbers: the aggregate throughput across four workers is already
~1 GB/s even at today's cost. This plan is about not doing pointless work, not
about a bottleneck the user would currently notice on a typical repository —
which is why it sits below the confirmed defects in priority.
