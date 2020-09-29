#!/bin/sh
set -e

ROOT="$(dirname "$(readlink -f "$0")")"
BUILDDIR=build

mkdir -p "$BUILDDIR"
cd "$BUILDDIR"


# Compile
# qtchooser -run-tool=qmake -qt=5 -recursive "$ROOT" "QMAKE_RPATHDIR += ../qtmips_machine ../qtmips_osemu ../qtmips_asm"
qmake ..
# make -j8 sub-qtmips_cli sub-qtmips_gui # Note: we are building these to to not build tests
make -j$(nproc)

cd ..
# Link executables to more suitable place
ln -fs "$BUILDDIR/qtmips_cli/qtmips_cli" cli
ln -fs "$BUILDDIR/qtmips_gui/qtmips_gui" gui
