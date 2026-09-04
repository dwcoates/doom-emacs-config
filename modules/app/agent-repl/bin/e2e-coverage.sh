#!/usr/bin/env bash
#
# e2e-coverage.sh — run the cross-system e2e suite with coverage collection
# for the systems it SPAWNS, then merge and report.
#
# Usage:
#   bin/e2e-coverage.sh
#
# The suite's subject is four separate processes, so `go test -cover` on the
# e2e package measures nothing useful. Instead AGENT_REPL_E2E_COVERAGE turns
# on instrumented builds (`go build -cover`) whose processes write counters
# into <root>/<binary>, plus NODE_V8_COVERAGE=<root>/shim, which the daemon
# passes to every shim it spawns. This script sets that root, runs the suite
# at the documented `-parallel 8`, merges each Go binary's counters with
# `go tool covdata textfmt`, reports each with `go tool cover -func`, and
# renders the shim's v8 profiles with c8 against the bundle's source map.
#
# THE SUITE'S OWN RESULT IS THE EXIT STATUS. Reporting always runs, even on a
# failing suite, so one run yields both the failures and the numbers; the
# script exits non-zero if the suite failed or if any report could not be
# produced.
#
# Knobs:
#   AGENT_REPL_E2E_COVERAGE_DIR  keep the profiles here instead of a temp dir
#   E2E_COVERAGE_PARALLEL        override `-parallel` (default 8)
#   E2E_COVERAGE_TIMEOUT         override `-timeout` (default 45m)
#   E2E_COVERAGE_RUN             pass a `-run` pattern through
set -uo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$THIS_DIR/.." && pwd)"
E2E_DIR="$ROOT/e2e"
SHIM_DIR="$ROOT/agent-shim/claude/shim"

PARALLEL="${E2E_COVERAGE_PARALLEL:-8}"
TIMEOUT="${E2E_COVERAGE_TIMEOUT:-45m}"

# Each Go binary this suite spawns, paired with the module whose sources it
# was built from. The names are the GOCOVERDIR subdirectories the harness
# creates (harness.CoverageDir).
GO_BINARIES=(claude-repld shim-store shim-claude-sidecar)

log() { printf '[agent-repl-e2e-coverage] %s\n' "$*"; }
die() { printf '[agent-repl-e2e-coverage] ERROR: %s\n' "$*" >&2; exit 1; }

require_command() {
    command -v "$1" >/dev/null 2>&1 ||
        die "required command '$1' is unavailable"
}

require_command go
[ -f "$E2E_DIR/go.mod" ] || die "the e2e module is missing $E2E_DIR/go.mod"

if [ -n "${AGENT_REPL_E2E_COVERAGE_DIR:-}" ]; then
    COVERAGE_ROOT="$AGENT_REPL_E2E_COVERAGE_DIR"
    mkdir -p "$COVERAGE_ROOT" || die "cannot create $COVERAGE_ROOT"
else
    COVERAGE_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-e2e-coverage.XXXXXX")"
fi
log "coverage root: $COVERAGE_ROOT"

TEST_ARGS=(-count=1 -timeout "$TIMEOUT" -parallel "$PARALLEL")
[ -n "${E2E_COVERAGE_RUN:-}" ] && TEST_ARGS+=(-run "$E2E_COVERAGE_RUN")

log "running the e2e suite with coverage collection on"
(
    cd "$E2E_DIR"
    # TMPDIR=/tmp IS REQUIRED on macOS: the suite's unix sockets do not fit
    # the 103-byte path cap beneath the default per-user temp root.
    TMPDIR=/tmp AGENT_REPL_E2E_COVERAGE="$COVERAGE_ROOT" \
        go test "${TEST_ARGS[@]}" ./...
)
SUITE_STATUS=$?
if [ "$SUITE_STATUS" -eq 0 ]; then
    log "suite: PASS"
else
    log "suite: FAIL (exit $SUITE_STATUS) — reporting the coverage it produced anyway"
fi

REPORT_STATUS=0
report_failed() {
    printf '[agent-repl-e2e-coverage] ERROR: %s\n' "$*" >&2
    REPORT_STATUS=1
}

# --- Go: merge each binary's counters, then report per binary -------------
SUMMARY=()
for binary in "${GO_BINARIES[@]}"; do
    dir="$COVERAGE_ROOT/$binary"
    profile="$COVERAGE_ROOT/$binary.coverprofile"
    functions="$COVERAGE_ROOT/$binary.functions.txt"

    if [ ! -d "$dir" ] || ! compgen -G "$dir/covmeta.*" >/dev/null; then
        report_failed "$binary wrote no coverage counters into $dir — an instrumented process must leave through SIGTERM or its own exit, never SIGKILL"
        continue
    fi
    if ! go tool covdata textfmt -i="$dir" -o="$profile"; then
        report_failed "$binary: go tool covdata textfmt failed"
        continue
    fi
    if ! go tool cover -func="$profile" >"$functions"; then
        report_failed "$binary: go tool cover -func failed"
        continue
    fi
    summary="$(tail -n 1 "$functions")"
    case "$summary" in
        *"(statements)"*) ;;
        *) report_failed "$binary coverage summary is malformed: $summary"; continue ;;
    esac
    log "$binary: $summary"
    SUMMARY+=("$binary $(printf '%s' "$summary" | awk '{print $NF}')")
done

# --- Shim: render the v8 profiles against the bundle's source map ---------
SHIM_DIR_COV="$COVERAGE_ROOT/shim"
if [ ! -d "$SHIM_DIR_COV" ] || [ -z "$(ls -A "$SHIM_DIR_COV" 2>/dev/null)" ]; then
    report_failed "the shim wrote no v8 coverage into $SHIM_DIR_COV — NODE_V8_COVERAGE reaches a shim only through the daemon's spawn environment"
elif [ "${E2E_COVERAGE_SKIP_SHIM_REPORT:-0}" = "1" ]; then
    log "shim: raw v8 profiles kept at $SHIM_DIR_COV (rendering skipped on request)"
else
    require_command npx
    log "shim: rendering v8 coverage with c8"
    if (
        cd "$SHIM_DIR"
        npx --yes "c8@${E2E_COVERAGE_C8_VERSION:-10}" report \
            --temp-directory="$SHIM_DIR_COV" \
            --reports-dir="$COVERAGE_ROOT/shim-report" \
            --reporter=text-summary --reporter=json-summary \
            --include='src/**/*.ts' --all --src=src
    ); then
        SUMMARY+=("shim see $COVERAGE_ROOT/shim-report")
    else
        report_failed "shim: c8 report failed (raw v8 profiles remain at $SHIM_DIR_COV)"
    fi
fi

log "---- e2e coverage ----"
for line in "${SUMMARY[@]}"; do
    log "$line"
done
log "profiles under $COVERAGE_ROOT"

if [ "$SUITE_STATUS" -ne 0 ]; then
    exit "$SUITE_STATUS"
fi
exit "$REPORT_STATUS"
