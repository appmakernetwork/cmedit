# Contributing to CMeDit

Thanks for your interest in improving CMeDit! Contributions of all kinds — bug
reports, fixes, features, docs — are welcome.

## Contributor License Agreement (required)

CMeDit uses a **licensing Contributor License Agreement (CLA)**. Before your
first contribution can be merged, you must agree to the
[Individual CLA](CLA.md).

You keep the copyright to your work. The CLA grants the project owner
(Benjamin Marsh) a broad license to your contribution — including the right to
distribute it under the GPL and, if ever needed, to re-license the project (for
example, to offer a commercial license). This keeps CMeDit's copyright
consistent and under single stewardship.

**How signing works:** when you open your first pull request, the
[CLA-assistant](https://github.com/contributor-assistant/github-action) bot will
comment with a link to the CLA and ask you to confirm. Reply on the PR with:

> I have read the CLA Document and I hereby sign the CLA

Your agreement is recorded once and applies to future contributions. (No paper,
no email needed.)

## Licensing of contributions

CMeDit is licensed under the **GNU General Public License, version 3**
(`GPL-3.0-only`). All contributions are made under that license (in addition to
the rights granted by the CLA above).

## Development

CMeDit is written from first principles with **no TUI framework** and depends
only on libraries that ship with GHC. Please keep it that way:

- **Build with `make`, not `cabal`** — this project targets offline machines
  with no Hackage index. `make` drives `ghc --make` directly.
  - `make` — build the optimized `./cmedit`
  - `make test` — build and run the test suite (`./cmedit-test`)
  - `make run` — build and launch
- **No new dependencies** unless they ship with GHC (base, bytestring, text,
  containers, array, unix, process, stm, directory, filepath, mtl).
- **Add tests** for pure logic in `test/Spec.hs` (a hand-rolled suite — there's
  no external framework offline). Interactive/TUI behaviour is verified with a
  PTY harness; see `CLAUDE.md` for the approach.
- **Match the surrounding style**: keep the pure core (`Cmedit.Editor`,
  rendering, buffer) free of IO, and route side effects through `Effect`
  constructors handled in `Cmedit.App`. See `README.md` and `CLAUDE.md` for the
  architecture.
- Run `make test` and make sure it prints `failed 0` before opening a PR.

## Reporting bugs

Open an issue with the terminal you're using, the steps to reproduce, and what
you expected versus what happened. For rendering glitches, the `$TERM` value and
terminal size help a lot.
