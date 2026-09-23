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
# `go tool cover -func` resolves a profile's packages through the MODULE it
# reports on, so each report runs from that module's own directory.
GO_BINARIES=(claude-repld shim-store shim-claude-sidecar)
go_module_dir() {
    case "$1" in
        claude-repld)         printf '%s' "$ROOT/daemon" ;;
        shim-store)           printf '%s' "$ROOT/agent-shim/shim-store" ;;
        shim-claude-sidecar)  printf '%s' "$ROOT/agent-shim/claude/shim-sidecar" ;;
        *) die "no module directory is declared for '$1'" ;;
    esac
}

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
    cd "$E2E_DIR" || { log "cannot enter the e2e directory $E2E_DIR"; exit 1; }
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
    if ! (cd "$(go_module_dir "$binary")" && go tool cover -func="$profile") >"$functions"; then
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
#
# c8 is run FROM THE COVERAGE ROOT on purpose: its default file filter keeps
# only scripts beneath the working directory, and the bundle the v8 profiles
# name lives under this root (e2e/main_test.go stages it there under
# coverage). The report is then remapped through the bundle's source map, so
# every entry is a real source file -- ours under agent-shim/**/src, plus the
# dependencies esbuild inlined. The summary below keeps OURS.
SHIM_COV_DIR="$COVERAGE_ROOT/shim"
SHIM_REPORT_DIR="$COVERAGE_ROOT/shim-report"
if [ ! -d "$SHIM_COV_DIR" ] || [ -z "$(ls -A "$SHIM_COV_DIR" 2>/dev/null)" ]; then
    report_failed "the shim wrote no v8 coverage into $SHIM_COV_DIR — NODE_V8_COVERAGE reaches a shim only through the daemon's spawn environment, and a SIGKILLed node process writes nothing"
elif [ "${E2E_COVERAGE_SKIP_SHIM_REPORT:-0}" = "1" ]; then
    log "shim: raw v8 profiles kept at $SHIM_COV_DIR (rendering skipped on request)"
else
    require_command node
    # c8 is the shim's own pinned devDependency (package.json), never fetched
    # at run time: a coverage run must be reproducible and work offline.
    C8_BIN="$SHIM_DIR/node_modules/.bin/c8"
    if [ ! -x "$C8_BIN" ]; then
        die "shim: c8 is not installed at $C8_BIN — run npm ci in agent-shim/claude/shim"
    fi
    log "shim: rendering v8 coverage with c8"
    if (
        cd "$COVERAGE_ROOT"
        "$C8_BIN" report \
            --temp-directory="$SHIM_COV_DIR" \
            --reports-dir="$SHIM_REPORT_DIR" \
            --reporter=json-summary --reporter=html
    ) && [ -f "$SHIM_REPORT_DIR/coverage-summary.json" ]; then
        if shim_summary="$(node "$THIS_DIR/e2e-shim-coverage-summary.mjs" "$SHIM_REPORT_DIR")"; then
            log "shim: $shim_summary"
            SUMMARY+=("shim $shim_summary")
        else
            report_failed "shim: the c8 summary carried no agent-shim source (the bundle's source map is the only thing that attributes the bundle back to src/**/*.ts)"
        fi
    else
        report_failed "shim: c8 report failed (raw v8 profiles remain at $SHIM_COV_DIR)"
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
