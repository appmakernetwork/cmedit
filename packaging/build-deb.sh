#!/usr/bin/env bash
#
# Build a Debian package (.deb) for cmedit.
#
# Assembles a staging tree, strips the release binary into it, generates the
# DEBIAN/control metadata (architecture, runtime deps and installed size are
# all computed from the actual build), and runs `dpkg-deb` under fakeroot so
# the installed files are owned by root:root.
#
# Usage:  packaging/build-deb.sh
# Output: cmedit_<version>-<revision>_<arch>.deb in the project root.

set -euo pipefail

# --- locate the project root (parent of this script's directory) -------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$ROOT"

# --- package identity --------------------------------------------------------
PKG=cmedit
VERSION="$(sed -n 's/^version: *\([0-9.]*\).*/\1/p' cmedit.cabal | head -1)"
REVISION="${DEB_REVISION:-1}"
ARCH="$(dpkg --print-architecture)"
MAINTAINER="Benjamin Marsh <benmarshnz@gmail.com>"

if [ -z "$VERSION" ]; then
    echo "error: could not read version from cmedit.cabal" >&2
    exit 1
fi

DEBNAME="${PKG}_${VERSION}-${REVISION}_${ARCH}.deb"
STAGE="$ROOT/dist-deb/${PKG}_${VERSION}-${REVISION}_${ARCH}"

echo ">> Building $PKG $VERSION-$REVISION for $ARCH"

# --- build the optimized binary ----------------------------------------------
# Fully static so the package installs on distros with an older glibc than the
# build machine (a dynamic build on trixie needs GLIBC_2.38, which bookworm's
# glibc 2.36 doesn't have).
make -C "$ROOT" static

# --- lay out the staging tree (FHS) ------------------------------------------
rm -rf "$STAGE"
install -d "$STAGE/DEBIAN"
install -d "$STAGE/usr/bin"
install -d "$STAGE/usr/share/doc/$PKG"
install -d "$STAGE/usr/share/man/man1"

# binary, stripped (mode 0755)
install -m 0755 "$ROOT/cmedit" "$STAGE/usr/bin/$PKG"
strip --strip-unneeded "$STAGE/usr/bin/$PKG"

# man page, gzipped -n (no timestamp -> reproducible)
gzip -9 -n -c "$SCRIPT_DIR/cmedit.1" > "$STAGE/usr/share/man/man1/$PKG.1.gz"

# copyright + README
install -m 0644 "$SCRIPT_DIR/copyright" "$STAGE/usr/share/doc/$PKG/copyright"
install -m 0644 "$ROOT/README.md" "$STAGE/usr/share/doc/$PKG/README.md"

# Debian changelog, gzipped
cat > "$STAGE/usr/share/doc/$PKG/changelog.Debian" <<EOF
$PKG ($VERSION-$REVISION) unstable; urgency=low

  * Initial Debian packaging of cmedit.

 -- $MAINTAINER  $(date -R)
EOF
gzip -9 -n "$STAGE/usr/share/doc/$PKG/changelog.Debian"

# --- compute runtime dependencies from the linked libraries ------------------
# Map each NEEDED shared library to the Debian package that ships it, then
# union those into a Depends line. dpkg's database stores the post-usrmerge
# paths, so a raw ldd path (e.g. /lib/...) may not match directly; try the
# path as-is, its canonical target, and a /usr-prefixed form. Never fatal:
# fall back to a known-good default set if resolution comes up empty.
resolve_deps() {
    local bin="$1" lib cand pkg
    for lib in $(ldd "$bin" | awk '/=>/ {print $3}'); do
        [ -e "$lib" ] || continue
        for cand in "$lib" "$(readlink -f "$lib")" "/usr$lib"; do
            pkg="$(dpkg -S "$cand" 2>/dev/null | head -1 | cut -d: -f1)"
            [ -n "$pkg" ] && { echo "$pkg"; break; }
        done
    done
    return 0
}
# A statically linked binary has no runtime library dependencies; the control
# file then carries no Depends field at all (an empty one is invalid). Only
# fall back to the default set when a *dynamic* binary defeats resolution.
if file "$STAGE/usr/bin/$PKG" | grep -q 'statically linked'; then
    DEPS=""
    DEPENDS_LINE=""
else
    DEPS="$(resolve_deps "$STAGE/usr/bin/$PKG" | sort -u | paste -sd, - | sed 's/,/, /g')"
    [ -z "$DEPS" ] && DEPS="libc6, libgmp10, libffi8"
    DEPENDS_LINE="Depends: $DEPS
"
fi
echo ">> Depends: ${DEPS:-(none: static binary)}"

# --- installed size (KiB, rounded up) ----------------------------------------
INSTALLED_KB="$(du -ks "$STAGE/usr" | cut -f1)"

# --- control file ------------------------------------------------------------
cat > "$STAGE/DEBIAN/control" <<EOF
Package: $PKG
Version: $VERSION-$REVISION
Section: editors
Priority: optional
Architecture: $ARCH
${DEPENDS_LINE}Installed-Size: $INSTALLED_KB
Maintainer: $MAINTAINER
Homepage: https://github.com/appmakernetwork/cmedit
Description: from-scratch terminal text editor in Haskell
 CMeDit is a modeless terminal text editor written from first principles in
 Haskell, a cross between Microsoft's Edit (msedit) and GNU nano. It offers a
 drop-down menu bar, real mouse support, system-clipboard integration, syntax
 highlighting and a CSV/TSV table view.
 .
 No TUI framework is used: raw-mode terminal control, the input parser, the
 diff-based renderer and the menus are built directly on VT/ANSI escape
 sequences and the POSIX termios API.
EOF

# md5sums (over the installed files, paths relative to the stage root)
( cd "$STAGE" && find usr -type f -print0 | sort -z \
    | xargs -0 md5sum > DEBIAN/md5sums )

# --- build the .deb (root-owned files via fakeroot) --------------------------
rm -f "$ROOT/$DEBNAME"
fakeroot dpkg-deb --build --root-owner-group "$STAGE" "$ROOT/$DEBNAME" >/dev/null

echo ">> Wrote $DEBNAME"
echo
dpkg-deb --info "$ROOT/$DEBNAME"
echo
dpkg-deb --contents "$ROOT/$DEBNAME"
