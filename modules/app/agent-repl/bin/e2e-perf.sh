#!/usr/bin/env bash
# e2e-perf.sh report|baseline|enforce -- the e2e perf phase (e2e/PERF-SPEC.md
# section D4), as three roster suites that run only when named
# (testrun/roster: e2e-perf, e2e-perf-baseline, e2e-perf-enforce). The daemon
# runs them: `agent-repld test run -suite e2e-perf`.
#
#   report    measure, and REPORT the drift from the committed baselines
#             rather than failing on it (PERF-SPEC.md section I5 finding 17).
#   enforce   measure, and FAIL on a >20% regression against the baselines;
#             for a quiet or dedicated host, since on a shared one it fails on
#             load, not on regressions.
#   baseline  REWRITE the committed baselines from this run, deliberately and
#             by name, never from a failing run (PERF-SPEC.md section D3).
#             -count=5, recording the HIGH-WATER p50/p95 across the five: three
#             put one row's p95 at 1.539ms and the next run measured 2.036ms, a
#             false 32% regression (section I5 finding 17).
#
# SERIAL, AND ON A GREEN TREE. `-parallel 1` is stated rather than inherited,
# so nothing can parallelize a latency measurement (a run went 20.7s -> 38.4s
# at -parallel 12 and lost a test to a bound), and the numbers of a tree whose
# functional e2e suite is red mean nothing: run `-suite e2e` green first.
#
# `-v` is not optional: PerfRecorder.Report writes through t.Logf, and a
# passing run printing its numbers is the whole discipline. The perf build tag
# keeps every perf file out of the functional binary; AGENT_REPL_E2E_PERF is
# the second gate, so a stray `-tags perf` sweep skips loudly.
#
# E2E_PERF_TIMEOUT overrides the go test -timeout (default 45m).

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_DIR="$(cd "$THIS_DIR/.." && pwd)"
TIMEOUT="${E2E_PERF_TIMEOUT:-45m}"

die() {
    printf 'e2e-perf: %s\n' "$*" >&2
    exit 2
}

[ "$#" -eq 1 ] || die "usage: e2e-perf.sh report|baseline|enforce"
mode="$1"
case "$mode" in
    report | enforce | baseline) ;;
    *) die "unknown mode '$mode'; usage: e2e-perf.sh report|baseline|enforce" ;;
esac

# The suite bundles the real shim and drives the real webapp.
"$THIS_DIR/ensure-e2e-deps.sh" || die "the e2e suite's npm deps could not be ensured"

cd "$MODULE_DIR/e2e"
case "$mode" in
    report)
        exec env AGENT_REPL_E2E_PERF=1 \
            go test ./... -tags perf -run 'TestPerf' -count=1 -timeout "$TIMEOUT" -parallel 1 -v
        ;;
    enforce)
        exec env AGENT_REPL_E2E_PERF=1 AGENT_REPL_E2E_PERF_BASELINE=enforce \
            go test ./... -tags perf -run 'TestPerf' -count=1 -timeout "$TIMEOUT" -parallel 1 -v
        ;;
    baseline)
        tip="$(git -C "$MODULE_DIR" rev-parse --short HEAD)" || die "the tip the baselines are measured at could not be read"
        exec env AGENT_REPL_E2E_PERF=1 AGENT_REPL_E2E_PERF_BASELINE=write AGENT_REPL_E2E_PERF_TIP="$tip" \
            go test ./... -tags perf -run 'TestPerf' -count=5 -timeout "$TIMEOUT" -parallel 1 -v
        ;;
esac
