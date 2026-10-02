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
# The whole suite run holds this host's suite slot (bin/suite-slot.sh),
# because it fills the machine by design, and runs inside
# .claude/safe-test-run.sh's git-state net, so a unit that touched the
# checkout's git state is caught.
#
# --record ITSELF NEVER RUNS INSIDE THAT NET. `testrun run --record` only
# STAGES this run's timing rows (to $RUN_TMP, outside the checkout); this
# script commits them to test_time.csv via `testrun finish-record` only
# after the net has returned clean, so a canonical history run's own
# declared write is never mistaken for the drift the net exists to catch.
# See testrun/internal/cli/run.go's "testrun run only STAGES --record".

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

# --record is staged, not committed, inside the net: see the header comment.
# RECORD_ARGS carries --record-out only when --record is actually among "$@",
# so an ordinary run's testrun invocation is unchanged.
IS_RECORD=0
for arg in "$@"; do
    [ "$arg" = "--record" ] && IS_RECORD=1
done
RECORD_OUT="$RUN_TMP/pending-record.json"
RECORD_ARGS=()
[ "$IS_RECORD" -eq 1 ] && RECORD_ARGS=(--record-out "$RECORD_OUT")

set +e
"$THIS_DIR/suite-slot.sh" \
    "$REPO_ROOT/.claude/safe-test-run.sh" -- \
    "$TESTRUN" run --module "$MODULE_ROOT" "$@" ${RECORD_ARGS[@]+"${RECORD_ARGS[@]}"}
RUN_RC=$?
set -e

# The net has already taken its clean post-run snapshot by the time this
# runs, so committing the staged rows here can never look like drift the
# run itself caused. A failed run (RUN_RC != 0, whether from a suite or
# from real git-state drift the net caught) never reaches this: nothing is
# recorded, exactly as before the net existed.
if [ "$IS_RECORD" -eq 1 ] && [ "$RUN_RC" -eq 0 ]; then
    "$TESTRUN" finish-record --module "$MODULE_ROOT" --record-out "$RECORD_OUT"
    RUN_RC=$?
fi

exit "$RUN_RC"
