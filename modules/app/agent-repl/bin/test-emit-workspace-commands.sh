#!/usr/bin/env bash
# Hermetic tests for skills/emit-workspace-commands.sh, the command-file
# writer the repo-local workspace skills pipe their JSON into. Every case
# points AGENT_REPL_STATE_DIR at its own scratch dir, so the live state dir
# is never written.

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them or their helpers.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
EMIT="$THIS_DIR/../skills/emit-workspace-commands.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT
PASS=0
FAIL=0
pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; shift; [ $# -eq 0 ] || printf '%s\n' "$@" | sed 's/^/        /' >&2; FAIL=$((FAIL + 1)); }

test_the_commands_land_in_one_file_under_a_state_dir_with_a_space() {
  local state="$TMP/state dir" files
  printf '[{"type":"open"}]\n' | AGENT_REPL_STATE_DIR="$state" bash "$EMIT"
  files="$(find "$state/output" -type f)"
  if [ "$(wc -l <<<"$files" | tr -d ' ')" -eq 1 ] && [[ "$(basename "$files")" == workspace_commands_*.json ]] \
     && [ "$(cat "$files")" = '[{"type":"open"}]' ]; then
    pass "the commands land in one workspace_commands_<uuid>.json, even under a state dir with a space"
  else
    fail "spaced state dir" "$files"
  fi
}

test_no_temporary_file_is_left_behind() {
  local state="$TMP/clean"
  printf '[]\n' | AGENT_REPL_STATE_DIR="$state" bash "$EMIT"
  if [ -z "$(find "$state/output" -name '.workspace_commands_*')" ]; then pass "no temporary file is left behind"; else fail "a temporary file remained"; fi
}

test_a_missing_uuidgen_fails_hard_and_writes_nothing() {
  local state="$TMP/no-uuid" out rc=0 bin="$TMP/bin-no-uuid"
  mkdir -p "$bin"
  ln -s "$(command -v cat)" "$bin/cat"
  out="$(printf '[]\n' | env -i PATH="$bin" AGENT_REPL_STATE_DIR="$state" /bin/bash "$EMIT" 2>&1)" || rc=$?
  if [ "$rc" -ne 0 ] && grep -qF "required binary 'uuidgen' is not on PATH" <<<"$out" && [ ! -e "$state/output" ]; then
    pass "a missing uuidgen fails hard and writes nothing"
  else
    fail "missing uuidgen" "rc=$rc" "$out"
  fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
