#!/usr/bin/env bash
#
# test-all.sh — run every agent-repl test suite from one command,
# spread across this host's cores.
#
# Usage:
#   bin/test-all.sh
#   bin/test-all.sh --record
#   bin/test-all.sh --coverage
#   bin/test-all.sh --suites webapp,build-frontend-harness
#
# THE WORK IS SCHEDULED, NOT RUN SUITE BY SUITE. testrun (../testrun, a Go
# program this script builds and runs) turns every selected suite into units —
# one Go package, one chunk of ERT files, one chunk of vitest files, one chunk
# of e2e tests, one harness script — each pinned to ONE core, and runs them on
# every core but two, longest chain first. How many chunks a suite is cut into
# is chosen per run by simulating the schedule against this host's measured
# timings (~/.cache/agent-repl/test-history.json). ../AGENTS.md has the whole
# model.
#
# The output contract is unchanged: one "<suite>: starting" line, one
# "<suite>: passed in Ns" / "<suite> failed after Ns with exit code N" /
# "<suite>: DECLINED after ..." line per suite, then the summaries. The merge
# gate (daemon/internal/merge/testgate.go) parses those lines.
#
# The roster is testrun/roster/roster.go — the one list --suites is validated
# against and the merge gate selects from. --suites narrows the run; WITHOUT IT
# EVERY SUITE RUNS. An unknown suite name is a hard error, never a silently
# empty run.
#
# A failing suite is reported loudly the moment it fails, the run continues
# through everything else, and it ends with a summary of every failure plus a
# non-zero exit status. A suite that exits 77 DECLINED (its precondition is
# unmet) and is never called passed.
#
# --record appends one row per passing suite to ../test_time.csv (its own
# units' summed wall time, measure unit-wall-sum) and compares the run with
# recent rows of the same branch and measure. Failed runs never record, and
# --record refuses --coverage.
# --coverage adds Go and vitest instrumentation and reports. Ordinary runs do
# not pay that cost.
#
# The whole run holds this host's suite slot (bin/suite-slot.sh), because it
# fills the machine by design, and runs inside .claude/safe-test-run.sh's
# git-state net, so a unit that touched the checkout's git state is caught.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_ROOT="$(cd "$THIS_DIR/.." && pwd)"
REPO_ROOT="$(cd "$MODULE_ROOT/../../.." && pwd)"
RUN_TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-test-all.XXXXXX")"
trap 'rm -rf "$RUN_TMP"' EXIT

err() {
    printf '[agent-repl-tests] ERROR: %s\n' "$*" >&2
}

command -v go >/dev/null 2>&1 || {
    err "go is not on PATH; the test runner (testrun) is a Go program"
    exit 1
}

TESTRUN="$RUN_TMP/testrun"
if ! (cd "$MODULE_ROOT/testrun" && go build -o "$TESTRUN" .); then
    err "building the test runner in $MODULE_ROOT/testrun failed"
    exit 1
fi

"$THIS_DIR/suite-slot.sh" \
    "$REPO_ROOT/.claude/safe-test-run.sh" -- \
    "$TESTRUN" run --module "$MODULE_ROOT" "$@"
