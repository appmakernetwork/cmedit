# Cmedit build. Uses `ghc --make` directly so it works with only the GHC boot
# libraries and no Hackage index (handy on offline machines). A standard
# `cabal build` also works where the Hackage index cache is present.
#
# The platform layer lives in platform/{posix,windows}/Cmedit/Term.hs — two
# implementations of the same module; each target picks one with -i. Only the
# POSIX side needs the C shim (cbits) and the unix package.

GHC      ?= ghc
OUTDIR   := dist-build
TESTDIR  := dist-test

# Packages shared by every platform; POSIX targets add -package unix.
PKGS = -package base -package bytestring -package text -package containers \
       -package array -package process -package stm \
       -package directory -package filepath -package mtl -package time
PKGS_POSIX = $(PKGS) -package unix

EXTS = -XLambdaCase -XOverloadedStrings -XBangPatterns -XTupleSections \
       -XScopedTypeVariables -XMultiWayIf -XRecordWildCards -XNamedFieldPuns

WARN = -Wall -Wno-unused-imports -Wno-name-shadowing -Wno-unused-do-bind \
       -Wno-type-defaults

SRC = $(wildcard src/Cmedit/*.hs)
PLATFORM_POSIX   = platform/posix/Cmedit/Term.hs
PLATFORM_WINDOWS = platform/windows/Cmedit/Term.hs

# Size-optimization flags: drop unused sections at link time (-split-sections +
# --gc-sections) and strip symbols (-optl-s). Keeps -O2 so runtime speed is
# unchanged; produces a smaller but still fully self-contained static binary
# (~19MB vs ~27MB). For an even smaller (~3MB) build that needs the matching
# GHC shared libs present at runtime, add -dynamic.
SMALL = -split-sections -optl-Wl,--gc-sections -optl-s

.PHONY: all small static test run clean deb windows windows-check

all: cmedit

cmedit: app/Main.hs $(SRC) $(PLATFORM_POSIX) cbits/cmedit_term.c
	$(GHC) --make app/Main.hs -isrc -iplatform/posix cbits/cmedit_term.c -o $@ \
	    -threaded -O2 -outputdir $(OUTDIR) $(PKGS_POSIX) $(EXTS) $(WARN)

# Smallest self-contained binary. Builds into a separate outputdir so it does
# not clobber the regular `make` objects.
small: app/Main.hs $(SRC) $(PLATFORM_POSIX) cbits/cmedit_term.c
	$(GHC) --make app/Main.hs -isrc -iplatform/posix cbits/cmedit_term.c -o cmedit \
	    -threaded -O2 $(SMALL) -outputdir $(OUTDIR)-small $(PKGS_POSIX) $(EXTS) $(WARN)

# Portable release binary: -optl-static also links the C libraries (libc, gmp,
# ffi, numa) in, so the result has no versioned glibc references and runs on
# distros older than the build machine (a normal build on trixie/glibc 2.41
# picks up GLIBC_2.38 symbols from GHC's libs and won't start on bookworm's
# 2.36). The NSS linker warnings (getpwnam_r etc.) are for functions cmedit
# never calls. This is what `make deb` ships.
static: app/Main.hs $(SRC) $(PLATFORM_POSIX) cbits/cmedit_term.c
	$(GHC) --make app/Main.hs -isrc -iplatform/posix cbits/cmedit_term.c -o cmedit \
	    -threaded -O2 -optl-static $(SMALL) -outputdir $(OUTDIR)-static \
	    $(PKGS_POSIX) $(EXTS) $(WARN)

# Native Windows build (cmedit.exe). Run it on Windows itself — GHC via
# ghcup plus make from MSYS2 (GHC's bundled MinGW toolchain does the linking;
# kernel32 is linked by default, so no extra libraries are needed). There is
# no cross-compiling GHC targeting Windows, so this target refuses to run
# elsewhere; use `make windows-check` on POSIX to typecheck the Windows
# configuration.
windows: app/Main.hs $(SRC) $(PLATFORM_WINDOWS)
ifeq ($(OS),Windows_NT)
	$(GHC) --make app/Main.hs -isrc -iplatform/windows -o cmedit.exe \
	    -threaded -O2 -outputdir $(OUTDIR)-windows $(PKGS) $(EXTS) $(WARN)
else
	@echo "make windows builds the native Windows port and must run on Windows"
	@echo "(GHC via ghcup + make from MSYS2). To typecheck the Windows"
	@echo "configuration on this machine instead, run: make windows-check"
	@exit 1
endif

# Typecheck the whole program against the Windows platform layer, anywhere
# (-fno-code: no object code, no linking, so no Windows libraries needed).
# This is what CI runs on Linux to keep the port from rotting.
windows-check: app/Main.hs $(SRC) $(PLATFORM_WINDOWS)
	$(GHC) --make app/Main.hs -isrc -iplatform/windows -fno-code \
	    -outputdir $(OUTDIR)-wincheck $(PKGS) $(EXTS) $(WARN)

cmedit-test: test/Spec.hs $(SRC) $(PLATFORM_POSIX) cbits/cmedit_term.c
	$(GHC) --make test/Spec.hs -isrc -iplatform/posix cbits/cmedit_term.c -o $@ \
	    -threaded -O0 -outputdir $(TESTDIR) $(PKGS_POSIX) $(EXTS) $(WARN)

test: cmedit-test
	./cmedit-test

run: cmedit
	./cmedit

# Build a Debian package (.deb) into the project root. See packaging/.
deb:
	packaging/build-deb.sh

clean:
	rm -rf $(OUTDIR) $(OUTDIR)-small $(OUTDIR)-static $(OUTDIR)-windows \
	    $(OUTDIR)-wincheck $(TESTDIR) dist-deb cmedit cmedit.exe cmedit-test *.deb
