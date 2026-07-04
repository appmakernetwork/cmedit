# Cmedit build. Uses `ghc --make` directly so it works with only the GHC boot
# libraries and no Hackage index (handy on offline machines). A standard
# `cabal build` also works where the Hackage index cache is present.

GHC      ?= ghc
OUTDIR   := dist-build
TESTDIR  := dist-test

PKGS = -package base -package bytestring -package text -package containers \
       -package array -package unix -package process -package stm \
       -package directory -package filepath -package mtl -package time

EXTS = -XLambdaCase -XOverloadedStrings -XBangPatterns -XTupleSections \
       -XScopedTypeVariables -XMultiWayIf -XRecordWildCards -XNamedFieldPuns

WARN = -Wall -Wno-unused-imports -Wno-name-shadowing -Wno-unused-do-bind \
       -Wno-type-defaults

SRC = $(wildcard src/Cmedit/*.hs)

# Size-optimization flags: drop unused sections at link time (-split-sections +
# --gc-sections) and strip symbols (-optl-s). Keeps -O2 so runtime speed is
# unchanged; produces a smaller but still fully self-contained static binary
# (~19MB vs ~27MB). For an even smaller (~3MB) build that needs the matching
# GHC shared libs present at runtime, add -dynamic.
SMALL = -split-sections -optl-Wl,--gc-sections -optl-s

.PHONY: all small static test run clean deb

all: cmedit

cmedit: app/Main.hs $(SRC) cbits/cmedit_term.c
	$(GHC) --make app/Main.hs -isrc cbits/cmedit_term.c -o $@ \
	    -threaded -O2 -outputdir $(OUTDIR) $(PKGS) $(EXTS) $(WARN)

# Smallest self-contained binary. Builds into a separate outputdir so it does
# not clobber the regular `make` objects.
small: app/Main.hs $(SRC) cbits/cmedit_term.c
	$(GHC) --make app/Main.hs -isrc cbits/cmedit_term.c -o cmedit \
	    -threaded -O2 $(SMALL) -outputdir $(OUTDIR)-small $(PKGS) $(EXTS) $(WARN)

# Portable release binary: -optl-static also links the C libraries (libc, gmp,
# ffi, numa) in, so the result has no versioned glibc references and runs on
# distros older than the build machine (a normal build on trixie/glibc 2.41
# picks up GLIBC_2.38 symbols from GHC's libs and won't start on bookworm's
# 2.36). The NSS linker warnings (getpwnam_r etc.) are for functions cmedit
# never calls. This is what `make deb` ships.
static: app/Main.hs $(SRC) cbits/cmedit_term.c
	$(GHC) --make app/Main.hs -isrc cbits/cmedit_term.c -o cmedit \
	    -threaded -O2 -optl-static $(SMALL) -outputdir $(OUTDIR)-static \
	    $(PKGS) $(EXTS) $(WARN)

cmedit-test: test/Spec.hs $(SRC) cbits/cmedit_term.c
	$(GHC) --make test/Spec.hs -isrc cbits/cmedit_term.c -o $@ \
	    -threaded -O0 -outputdir $(TESTDIR) $(PKGS) $(EXTS) $(WARN)

test: cmedit-test
	./cmedit-test

run: cmedit
	./cmedit

# Build a Debian package (.deb) into the project root. See packaging/.
deb:
	packaging/build-deb.sh

clean:
	rm -rf $(OUTDIR) $(OUTDIR)-small $(OUTDIR)-static $(TESTDIR) dist-deb cmedit cmedit-test *.deb
