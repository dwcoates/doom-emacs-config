# shellcheck shell=bash
# lib-harness-report.sh: the one way a script harness reports its cases.
#
# Sourced by every script harness, so a harness never hand-rolls its own
# PASS/FAIL lines, counters or exit. The merge gate names a failing case from
# its `  FAIL: what failed` line (daemon/internal/merge/failingtests.go), so
# every harness printing it the same way is what keeps a failure nameable.
#
#   pass WHAT              record a passing case
#   fail WHAT [EVIDENCE...]  record a failing case, each EVIDENCE line indented
#                          beneath it on stderr
#   harness_finish         print the totals and exit: 0 when nothing failed,
#                          1 otherwise

PASS=0
FAIL=0

pass() {
  printf '  PASS: %s\n' "$1"
  PASS=$((PASS + 1))
}

fail() {
  printf '  FAIL: %s\n' "$1" >&2
  shift
  # Every line of every EVIDENCE argument is indented, a multi-line capture too.
  [ $# -eq 0 ] || printf '%s\n' "$@" | sed 's/^/        /' >&2
  FAIL=$((FAIL + 1))
}

harness_finish() {
  printf '\nPassed: %d  Failed: %d\n' "$PASS" "$FAIL"
  exit $((FAIL > 0))
}
