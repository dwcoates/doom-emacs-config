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
#   bin/test-all.sh --selection REQUEST.pb [--events-fd N]
#   AGENT_REPL_TEST_EVENTS_FILE=PATH bin/test-all.sh [--suites a,b]
#   bin/test-all.sh --redundancy REQUEST.pb
#
# The daemon is the one caller (agentrepl.v1.RunTests, RunRedundancy, and the
# merge queue's gate): bin/background.sh refuses a test run it did not start.
# --redundancy runs `testrun redundancy` on a testrun.v1.RedundancyRequest
# from the module root, inside the same git-state net.
#
# THE WORK IS SCHEDULED, NOT RUN SUITE BY SUITE. testrun (../testrun, a Go
# program this script builds and runs) turns every selected suite into units —
# one Go package, one chunk of ERT files, one chunk of vitest files, one chunk
# of e2e tests, one harness script — each pinned to ONE core, and runs them on
# the host's core budget (at most 35% of its cores, 50% on AC power in High
# Power Mode, less while the rest of the host is busy, shared by every run on
# the host),
# longest chain first. How many chunks a suite is cut into
# is chosen per run by simulating the schedule against this host's measured
# timings (~/.cache/agent-repl/test-history.json). ../AGENTS.md has the whole
# model.
#
# The output contract: one "<suite>: starting" line followed by its
# "<suite>: N units planned" line, one "unit <id> [<suite>] ok|FAILED|..." line
# per unit, one "<suite>: passed in Ns" / "<suite> failed after Ns with exit
# code N" line per suite, then the summaries.
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
# non-zero exit status. No suite may skip: one whose precondition is unmet
# (the e2e sandbox, say) fails like any other.
#
# --record appends one row per passing suite to ../test_time.csv (its own
# units' summed wall time, measure unit-wall-sum) and compares the run with
# recent rows of the same branch and measure. Failed runs never record, and
# --record refuses --coverage.
# --coverage adds Go and vitest instrumentation and reports. Ordinary runs do
# not pay that cost.
#
# The whole suite run runs at background priority (bin/background.sh), which
# keeps the owner's live runtime ahead of it, and inside
# .claude/safe-test-run.sh's git-state net, so a ref a unit's own git moved
# is caught. Every run on the host shares one core
# budget, held unit by unit in priority order, that keeps the host's total
# CPU use under its limits (testrun/internal/cores; owner rulings 2026-10-20,
# 2026-10-26, 2026-10-30, 2026-11-13 and 2026-11-17).
#
# --record ITSELF NEVER RUNS INSIDE THAT NET. `testrun run --record` only
# STAGES this run's timing rows (to $RUN_TMP, outside the checkout); this
# script commits them to test_time.csv via `testrun finish-record` only
# after the net has returned clean, so a run that failed or moved a ref
# records nothing.
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
    # The runner is told its log (AGENT_REPL_TEST_RUN_LOG), so a run that falls
    # back from its RAM disk names this file to the workspace that investigates.
    AGENT_REPL_TEST_ALL_LOGGED=1 AGENT_REPL_TEST_RUN_LOG="$RUN_LOG" bash "${BASH_SOURCE[0]}" "$@" 2>&1 | tee -a "$RUN_LOG"
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

# RUNNER_UNAVAILABLE is the exit status of a run whose runner could not be
# built or started: no suite ran, so nothing of the tree was judged. The merge
# gate reads it as a fault of the HOST, never as a test failure
# (daemon/internal/merge/gate.go gateRunnerUnavailable): it holds the queue
# naming the cause, and spends no fixing attempt (2026-11-14 20:47:31: the
# build hit "too many open files in system", and the queue sent the merge to
# fix tests that never ran). 75 is sysexits' EX_TEMPFAIL.
RUNNER_UNAVAILABLE=75

command -v go >/dev/null 2>&1 || {
    err "go is not on PATH; the test runner (testrun) is a Go program"
    exit "$RUNNER_UNAVAILABLE"
}

TESTRUN="$RUN_TMP/testrun"
if ! (cd "$MODULE_ROOT/testrun" && go build -o "$TESTRUN" .); then
    err "building the test runner in $MODULE_ROOT/testrun failed"
    exit "$RUNNER_UNAVAILABLE"
fi

if [ "${1:-}" = "--redundancy" ]; then
    if [ "$#" -ne 2 ]; then
        err "--redundancy takes exactly one request file"
        exit 2
    fi
    set +e
    (cd "$MODULE_ROOT" && "$REPO_ROOT/.claude/safe-test-run.sh" -- \
        "$TESTRUN" redundancy --request "$2" --repo "$REPO_ROOT")
    RUN_RC=$?
    set -e
    exit "$RUN_RC"
fi

# A record is staged, not committed, inside the net: see the header comment.
# Every run is offered the staging path, because the daemon's selection file
# (--selection) can ask for a record as well as --record can; a run that does
# not record never writes it.
RECORD_OUT="$RUN_TMP/pending-record.json"

# AGENT_REPL_TEST_EVENTS_FILE WRITES THE RUN'S EVENTS TO A FILE (the merge
# gate): the gate's run is relayed by the test service as lines alone, so the
# typed testrun.v1.TestRunEvent stream, with every unit's per-test results and
# inputs digest, is read from this file once the run ends. It is the runner's
# --events-fd, opened on a descriptor here. It rides the environment, never
# the argument list, so a gate running a script from before it runs as it
# always did; and it is unset before the runner starts, so a test that runs
# this script itself never writes the gate's file.
RUN_ARGS=("$@")
if [ -n "${AGENT_REPL_TEST_EVENTS_FILE:-}" ]; then
    exec 9>"$AGENT_REPL_TEST_EVENTS_FILE"
    RUN_ARGS+=(--events-fd 9)
fi
unset AGENT_REPL_TEST_EVENTS_FILE

set +e
"$REPO_ROOT/.claude/safe-test-run.sh" -- \
    "$TESTRUN" run --module "$MODULE_ROOT" ${RUN_ARGS[@]+"${RUN_ARGS[@]}"} --record-out "$RECORD_OUT"
RUN_RC=$?
set -e

# The net has already taken its clean post-run snapshot by the time this
# runs, so committing the staged rows here can never look like drift the
# run itself caused. A failed run (RUN_RC != 0, whether from a suite or
# from real git-state drift the net caught) never reaches this: nothing is
# recorded, exactly as before the net existed.
if [ -f "$RECORD_OUT" ] && [ "$RUN_RC" -eq 0 ]; then
    "$TESTRUN" finish-record --module "$MODULE_ROOT" --record-out "$RECORD_OUT"
    RUN_RC=$?
fi

exit "$RUN_RC"
