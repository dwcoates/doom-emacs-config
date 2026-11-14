#!/usr/bin/env bash
# Byte-compile every non-test elisp source in lisp/ and report warnings: the
# `byte-compile` suite's gate. Exits 1 when any file warns or fails, so a new
# warning fails the merge gate instead of landing.
#
# The module targets Doom Emacs, so the compile runs under
# bin/compile-prelude.el, which supplies the Doom macro shapes that
# `emacs -Q' lacks.  The .elc files go to a directory of the run's own and
# are removed with it: this script is a warning gate, not a build step, and
# an .elc in lisp/ would be loaded in place of its source by every Emacs the
# ERT suite starts beside it.
set -uo pipefail

MODULE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$MODULE_DIR" || exit 1

OUT="$(mktemp -d "${TMPDIR:-/tmp}/byte-compile.XXXXXX")" || exit 1
trap 'rm -rf "$OUT"' EXIT

SOURCES=()
for f in lisp/*.el; do
  case "$f" in lisp/test-*) ;; *) SOURCES+=("$f") ;; esac
done

dest="(setq byte-compile-dest-file-function
        (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\") \"$OUT\")))"

status=0
for f in "${SOURCES[@]}"; do
  out="$(emacs -batch -Q -L lisp -l bin/compile-prelude.el --eval "$dest" \
               -f batch-byte-compile "$f" 2>&1)"
  if grep -q 'Warning:\|Error' <<<"$out"; then
    printf '=== %s\n%s\n' "$f" "$out"
    status=1
  fi
done

if [ "$status" -eq 0 ]; then
  printf 'byte-compile.sh: %d files compile with no warning\n' "${#SOURCES[@]}"
fi
exit $status
