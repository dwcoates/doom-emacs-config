#!/usr/bin/env bash
# Tests for bin/lib-harness-report.sh, the one way a script harness reports
# its cases. Each case sources the library in a child shell, so the counters
# and the exit under test are that shell's, not this harness's own.

# shellcheck disable=SC2016,SC2329
# SC2016: every child-shell body is single-quoted on purpose, so it expands in
# the child. SC2329: the tests are found and run by name (declare -F).

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
LIB="$THIS_DIR/lib-harness-report.sh"
REPO="$(cd "$THIS_DIR/../../../.." && pwd -P)"

# shellcheck source=lib-harness-report.sh
. "$LIB"

# child BODY: run BODY in a bash that sourced the library under set -e,
# capturing its stdout, stderr and exit as OUT, ERR and RC.
OUT="" ERR="" RC=0
child() {
  local errfile
  errfile="$(mktemp "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
  set +e
  OUT="$(bash -c 'set -euo pipefail; . "$1"; '"$1" _ "$LIB" 2>"$errfile")"
  RC=$?
  set -e
  ERR="$(cat "$errfile")"
  rm -f "$errfile"
}

test_pass_prints_the_case_on_stdout_and_counts_it() {
  child 'pass "a thing works"; pass "another"; echo "count=$PASS"'
  if [ "$OUT" = $'  PASS: a thing works\n  PASS: another\ncount=2' ] && [ -z "$ERR" ]; then
    pass "pass prints its case on stdout and counts it"
  else
    fail "pass" "out=$OUT" "err=$ERR"
  fi
}

test_fail_prints_the_case_and_its_evidence_on_stderr_and_counts_it() {
  child 'fail "a thing broke" "rc=3" "$(printf "line one\nline two")"; echo "count=$FAIL"'
  local want=$'  FAIL: a thing broke\n        rc=3\n        line one\n        line two'
  if [ "$ERR" = "$want" ] && [ "$OUT" = "count=1" ]; then
    pass "fail prints its case and every evidence line, indented, on stderr"
  else
    fail "fail" "out=$OUT" "err=$ERR"
  fi
}

test_fail_without_evidence_does_not_stop_a_set_e_harness() {
  child 'fail "no evidence"; echo "still running"'
  if [ "$RC" -eq 0 ] && [ "$OUT" = "still running" ]; then
    pass "a fail with no evidence returns 0 under set -e"
  else
    fail "fail under set -e" "rc=$RC" "out=$OUT"
  fi
}

test_finish_exits_zero_with_the_totals_when_nothing_failed() {
  child 'pass a; pass b; harness_finish; echo "after finish"'
  if [ "$RC" -eq 0 ] && grep -qxF 'Passed: 2  Failed: 0' <<<"$OUT" && ! grep -qF 'after finish' <<<"$OUT"; then
    pass "harness_finish exits 0 with the totals when nothing failed"
  else
    fail "finish green" "rc=$RC" "out=$OUT"
  fi
}

test_finish_exits_one_when_any_case_failed() {
  child 'pass a; fail b; harness_finish'
  if [ "$RC" -eq 1 ] && grep -qxF 'Passed: 1  Failed: 1' <<<"$OUT"; then
    pass "harness_finish exits 1 when any case failed"
  else
    fail "finish red" "rc=$RC" "out=$OUT"
  fi
}

test_the_merge_gate_names_a_failure_from_its_line() {
  child 'fail "the gate reads me"'
  # The gate's scriptFailLine (daemon/internal/merge/failingtests.go).
  if grep -qE '^\s*FAIL(:\s+|\s+-\s+)the gate reads me\s*$' <<<"$ERR"; then
    pass "a fail line matches the merge gate's script failure pattern"
  else
    fail "gate pattern" "err=$ERR"
  fi
}

# Every script harness reports through the library: one that defines its own
# pass, fail or counters has drifted from the shared shape, and fails here.
test_no_script_harness_hand_rolls_its_reporting() {
  local drifted
  drifted="$(grep -lE '^(pass|fail)\(\) \{|^(PASS|FAIL|FAILURES)=0$' \
    "$REPO"/modules/app/agent-repl/bin/test-*.sh \
    "$REPO"/modules/app/agent-repl/scripts/test-*.sh \
    "$REPO"/modules/app/agent-repl/skills/*/test-run.sh \
    "$REPO"/.claude/test-*.sh 2>/dev/null || true)"
  if [ -z "$drifted" ]; then
    pass "no script harness defines its own pass, fail or counters"
  else
    fail "script harnesses that hand-roll their reporting" "$drifted"
  fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

harness_finish
