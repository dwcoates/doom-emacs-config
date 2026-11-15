#!/usr/bin/env bash
# Hermetic tests for bin/byte-compile.sh, the elisp warning gate: each case
# copies the script and its prelude into a fixture module whose lisp/ holds
# the sources the case names, and runs the real Emacs over them.

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# shellcheck source=lib-harness-report.sh
. "$THIS_DIR/lib-harness-report.sh"

CLEAN=';;; clean.el --- a clean source -*- lexical-binding: t; -*-
;;; Code:
(defun clean-fn ()
  "Return one."
  1)
(provide (quote clean))
;;; clean.el ends here'

# An undeclared function call: the warning the gate exists to catch.
WARNS=';;; warns.el --- a source that warns -*- lexical-binding: t; -*-
;;; Code:
(defun warns-fn ()
  "Call what nothing defines."
  (warns-undefined-function-xyz))
(provide (quote warns))
;;; warns.el ends here'

# fixture NAME=BODY...: a module directory holding the gate and its prelude,
# with lisp/NAME holding BODY for each argument. Prints the module's path.
fixture() {
  local module
  module="$(mktemp -d "$TMP/module.XXXXXX")"
  mkdir -p "$module/bin" "$module/lisp"
  cp "$THIS_DIR/byte-compile.sh" "$THIS_DIR/compile-prelude.el" "$module/bin/"
  local pair
  for pair in "$@"; do
    printf '%s\n' "${pair#*=}" >"$module/lisp/${pair%%=*}"
  done
  printf '%s\n' "$module"
}

# run_gate MODULE: the gate's output and exit code, in OUT and RC, with its
# TMPDIR at MODULE/tmp so a case sees what it leaves behind.
run_gate() {
  mkdir -p "$1/tmp"
  RC=0
  OUT="$(TMPDIR="$1/tmp" "$1/bin/byte-compile.sh" 2>&1)" || RC=$?
}

test_a_clean_tree_passes_and_counts_its_sources() {
  local module
  module="$(fixture "clean.el=$CLEAN")"
  run_gate "$module"
  if [ "$RC" -eq 0 ] && grep -qF 'byte-compile.sh: 1 files compile with no warning' <<<"$OUT"; then
    pass "a clean tree exits 0 and counts the sources it compiled"
  else
    fail "a clean tree exits 0 and counts the sources it compiled" "rc=$RC" "$OUT"
  fi
}

test_a_warning_fails_the_gate_naming_its_file() {
  local module
  module="$(fixture "clean.el=$CLEAN" "warns.el=$WARNS")"
  run_gate "$module"
  if [ "$RC" -eq 1 ] && grep -qF '=== lisp/warns.el' <<<"$OUT" && grep -qF 'warns-undefined-function-xyz' <<<"$OUT" && ! grep -qF '=== lisp/clean.el' <<<"$OUT"; then
    pass "a source that warns fails the gate, which names that file and its warning alone"
  else
    fail "a source that warns fails the gate, which names that file and its warning alone" "rc=$RC" "$OUT"
  fi
}

test_a_test_file_is_not_compiled() {
  local module
  module="$(fixture "clean.el=$CLEAN" "test-warns.el=$WARNS")"
  run_gate "$module"
  if [ "$RC" -eq 0 ] && ! grep -qF 'test-warns.el' <<<"$OUT"; then
    pass "a test-*.el file is not one of the gate's sources"
  else
    fail "a test-*.el file is not one of the gate's sources" "rc=$RC" "$OUT"
  fi
}

test_no_elc_is_left_in_lisp_or_the_temporary_directory() {
  local module left
  module="$(fixture "clean.el=$CLEAN" "warns.el=$WARNS")"
  run_gate "$module"
  left="$(find "$module/lisp" -name '*.elc'; find "$module/tmp" -mindepth 1)"
  if [ -z "$left" ]; then
    pass "the gate leaves no .elc in lisp/ and removes its own directory"
  else
    fail "the gate leaves no .elc in lisp/ and removes its own directory" "$left"
  fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

harness_finish
