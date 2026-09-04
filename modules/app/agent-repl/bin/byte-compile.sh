#!/usr/bin/env bash
# Byte-compile every non-test elisp source in lisp/ and report warnings.
#
# The module targets Doom Emacs, so the compile runs under
# bin/compile-prelude.el, which supplies the Doom macro shapes that
# `emacs -Q' lacks.  Emitted .elc files are removed: this script is a
# warning gate, not a build step.
set -uo pipefail

MODULE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$MODULE_DIR" || exit 1

mapfile -t SOURCES < <(ls lisp/*.el | grep -v '/test-')

status=0
for f in "${SOURCES[@]}"; do
  out="$(emacs -batch -Q -L lisp -l bin/compile-prelude.el \
               -f batch-byte-compile "$f" 2>&1)"
  if grep -q 'Warning:\|Error' <<<"$out"; then
    printf '=== %s\n%s\n' "$f" "$out"
    status=1
  fi
done

rm -f lisp/*.elc

exit $status
