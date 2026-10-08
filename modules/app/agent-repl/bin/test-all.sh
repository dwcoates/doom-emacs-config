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
# half the host's cores, longest chain first. How many chunks a suite is cut into
# is chosen per run by simulating the schedule against this host's measured
# timings (~/.cache/agent-repl/test-history.json). ../AGENTS.md has the whole
# model.
#
# The output contract: one "<suite>: starting" line followed by its
# "<suite>: N units planned" line, one "unit <id> [<suite>] ok|FAILED|..." line
# per unit, one "<suite>: passed in Ns" / "<suite> failed after Ns with exit
# code N" / "<suite>: DECLINED after ..." line per suite, then the summaries.
# The merge gate (daemon/internal/merge/testgate.go) parses those lines, and
# counts each suite's unit verdicts against its planned total.
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

# EVERY RUN WRITES ITS OWN FULL LOG, named by this run alone, and says where at
# its start and at its end. A caller reads that file and never redirects the run
# into a path of its own choosing: two agents that both chose
# `<shared scratchpad>/testall.log` read each other's runs (2026-10-06, a
# report that named suites the caller never selected). The directory is
# <AGENT_REPL_TEST_LOG_ROOT>/<UTC timestamp>-<pid>; the newest
# TEST_LOG_KEEP runs are kept and older ones are removed as each run starts.
#
# The script runs itself once more as the logged run; the marker says which of
# the two this is, and the logged run drops it at once so nothing it starts (a
# harness running a copy of this script) inherits it.
if [ -z "${AGENT_REPL_TEST_ALL_LOGGED:-}" ]; then
    TEST_LOG_ROOT="${AGENT_REPL_TEST_LOG_ROOT:-/tmp/agent-repl-test-runs}"
    TEST_LOG_KEEP=100
    RUN_LOG_DIR="$TEST_LOG_ROOT/$(date -u +%Y%m%dT%H%M%SZ)-$$"
    mkdir -p "$RUN_LOG_DIR"
    RUN_LOG="$RUN_LOG_DIR/test-all.log"
    : >"$RUN_LOG"

    # The oldest runs past the newest TEST_LOG_KEEP go; the glob sorts by the
    # timestamp each directory is named by.
    LOG_DIRS=("$TEST_LOG_ROOT"/*/)
    for ((i = 0; i < ${#LOG_DIRS[@]} - TEST_LOG_KEEP; i++)); do
        rm -rf "${LOG_DIRS[$i]}"
    done

    printf "[agent-repl-tests] this run's full log: %s\n" "$RUN_LOG" | tee -a "$RUN_LOG"
    set +e
    AGENT_REPL_TEST_ALL_LOGGED=1 bash "${BASH_SOURCE[0]}" "$@" 2>&1 | tee -a "$RUN_LOG"
    RUN_RC=${PIPESTATUS[0]}
    set -e
    printf "[agent-repl-tests] exit %d; this run's full log: %s\n" "$RUN_RC" "$RUN_LOG" | tee -a "$RUN_LOG"
    exit "$RUN_RC"
fi
unset AGENT_REPL_TEST_ALL_LOGGED

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
