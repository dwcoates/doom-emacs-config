#!/usr/bin/env bash
# Hermetic tests for bin/deploy-hook-probe.sh, the deploy hook that only
# records that it ran, and bin/deploy-hook-probe-fails.sh, its failing sibling.

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
PROBE="$THIS_DIR/deploy-hook-probe.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

PASS=0
FAIL=0
pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; shift; [ $# -eq 0 ] || printf '%s\n' "$@" | sed 's/^/        /' >&2; FAIL=$((FAIL + 1)); }

test_the_probe_records_where_it_ran_and_exits_zero() {
  local out rc=0
  out="$(cd "$TMP" && "$PROBE" 2>&1)" || rc=$?
  if [ "$rc" -eq 0 ] && grep -qF "deploy-hook-probe: ran in $(cd "$TMP" && pwd)" <<<"$out"; then
    pass "the probe records the directory it ran in and exits 0"
  else
    fail "the probe records the directory it ran in and exits 0" "rc=$rc" "$out"
  fi
}

test_the_probe_says_it_finished() {
  local out
  out="$("$PROBE" 2>&1)"
  if [ "$(tail -1 <<<"$out")" = "deploy-hook-probe: done" ]; then
    pass "the probe's last line says it finished"
  else
    fail "the probe's last line says it finished" "$out"
  fi
}

test_the_failing_probe_runs_the_probe_and_exits_one() {
  local out rc=0
  out="$("$THIS_DIR/deploy-hook-probe-fails.sh" 2>&1)" || rc=$?
  if [ "$rc" -eq 1 ] && grep -qF "deploy-hook-probe: done" <<<"$out" && [ "$(tail -1 <<<"$out")" = "deploy-hook-probe-fails: exiting 1, as it always does" ]; then
    pass "the failing probe runs the probe, says it is failing, and exits 1"
  else
    fail "the failing probe runs the probe, says it is failing, and exits 1" "rc=$rc" "$out"
  fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
