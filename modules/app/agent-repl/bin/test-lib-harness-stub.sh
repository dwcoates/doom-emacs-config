#!/usr/bin/env bash
# Hermetic tests for bin/lib-harness-stub.sh, the one recording stub a script
# harness puts on PATH.

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F).

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
MODULE_DIR="$(cd "$THIS_DIR/.." && pwd -P)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# shellcheck source=lib-harness-report.sh
. "$THIS_DIR/lib-harness-report.sh"
# shellcheck source=lib-harness-stub.sh
. "$THIS_DIR/lib-harness-stub.sh"

new_case() {
  CASE="$TMP/case$RANDOM$RANDOM"
  mkdir -p "$CASE/stubs"
  : >"$CASE/transcript"
}

test_a_stub_records_its_argv_then_runs_its_body() {
  local out rc=0
  new_case
  write_stub tool 'echo "body $1"; exit 3'
  out="$(STUB_DIR="$CASE/stubs" "$CASE/stubs/tool" a "b c")" || rc=$?
  if [ "$out" = "body a" ] && [ "$rc" -eq 3 ] && [ "$(cat "$CASE/transcript")" = "tool a b c" ]; then
    pass "a stub records NAME ARGV, then runs its body and answers its exit"
  else fail "stub" "out=$out rc=$rc" "$(cat "$CASE/transcript")"; fi
}

test_ran_finds_a_recorded_line_and_only_that() {
  new_case
  echo "git push --force host:repo" >"$CASE/transcript"
  if ran "push --force" && ! ran "git pull"; then
    pass "ran succeeds on text a recorded line holds and fails on any other"
  else fail "ran"; fi
}

test_no_harness_defines_its_own_stub_helpers() {
  local hits
  hits="$(grep -lE '^(write_stub|ran)\(\)' "$MODULE_DIR"/bin/*.sh "$MODULE_DIR"/scripts/*.sh | grep -v '/lib-harness-stub\.sh$' || true)"
  if [ -z "$hits" ]; then
    pass "no harness defines write_stub or ran beside bin/lib-harness-stub.sh"
  else fail "hand-rolled stub helpers" "$hits"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do "$t"; done
harness_finish
