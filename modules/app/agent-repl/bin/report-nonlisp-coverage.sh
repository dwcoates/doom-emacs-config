#!/usr/bin/env bash
#
# report-nonlisp-coverage.sh — run and report every agent-repl Go and
# TypeScript test suite with coverage instrumentation.
#
# Usage:
#   bin/report-nonlisp-coverage.sh
#   bin/report-nonlisp-coverage.sh daemon webapp
#
# With no arguments every component runs.  Explicit component names restrict
# the run to those components.  This reporter intentionally measures without
# enforcing the repository's 90% statement target; the commit gate owns
# enforcement after every component has been brought above that target.
#
# Generated protobufs are not coverage targets.  The proto component instead
# validates the schema and verifies that committed generated artifacts match it.
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$THIS_DIR/.." && pwd)"
REPORT_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-coverage.XXXXXX")"
trap 'rm -rf "$REPORT_ROOT"' EXIT

ALL_COMPONENTS=(daemon sidecar store wire logging webapp shim proto)
COMPONENTS=()

log() {
    printf '[agent-repl-coverage] %s\n' "$*"
}

die() {
    printf '[agent-repl-coverage] ERROR: %s\n' "$*" >&2
    exit 1
}

require_command() {
    command -v "$1" >/dev/null 2>&1 ||
        die "required command '$1' is unavailable"
}

known_component_p() {
    local wanted="$1" component
    for component in "${ALL_COMPONENTS[@]}"; do
        [ "$component" = "$wanted" ] && return 0
    done
    return 1
}

if [ "$#" -eq 0 ]; then
    COMPONENTS=("${ALL_COMPONENTS[@]}")
else
    for component in "$@"; do
        known_component_p "$component" ||
            die "unknown component '$component'"
        COMPONENTS+=("$component")
    done
fi

report_go() {
    local component="$1" relative_dir="$2"
    local dir="$ROOT/$relative_dir"
    local profile="$REPORT_ROOT/$component.coverprofile"
    local function_report="$REPORT_ROOT/$component.functions.txt"
    local summary

    require_command go
    [ -f "$dir/go.mod" ] ||
        die "$component module is missing $dir/go.mod"

    log "$component: running all Go tests with coverage"
    if ! (
        cd "$dir"
        go test -count=1 -coverpkg=./... -coverprofile="$profile" ./...
    ); then
        die "$component Go coverage suite failed"
    fi

    if ! (
        cd "$dir"
        go tool cover -func="$profile"
    ) >"$function_report"; then
        die "$component Go coverage report failed"
    fi

    summary="$(tail -n 1 "$function_report")"
    case "$summary" in
        *"(statements)"*) ;;
        *) die "$component Go coverage summary is malformed: $summary" ;;
    esac
    cat "$function_report"
    log "$component: $summary"
}

report_typescript() {
    local component="$1" relative_dir="$2"
    local dir="$ROOT/$relative_dir"
    local reports_dir="$REPORT_ROOT/$component"

    require_command npm
    [ -f "$dir/package.json" ] ||
        die "$component package is missing $dir/package.json"

    log "$component: typechecking"
    if ! (cd "$dir" && npm run typecheck); then
        die "$component TypeScript typecheck failed"
    fi

    log "$component: running all Vitest tests with coverage"
    if ! (
        cd "$dir"
        npm run coverage -- \
            --coverage.reportsDirectory="$reports_dir"
    ); then
        die "$component TypeScript coverage suite failed"
    fi
}

report_proto() {
    local dir="$ROOT/proto"

    require_command make
    [ -f "$dir/Makefile" ] ||
        die "proto Makefile is missing at $dir/Makefile"

    log "proto: validating schemas and generated artifacts"
    if ! make -C "$dir" validate; then
        die "proto validation failed"
    fi
}

for component in "${COMPONENTS[@]}"; do
    case "$component" in
        daemon)  report_go daemon daemon ;;
        sidecar) report_go sidecar agent-shim/claude/shim-sidecar ;;
        store)   report_go store agent-shim/shim-store ;;
        wire)    report_go wire agent-shim/wire ;;
        logging) report_go logging agent-shim/logging/go ;;
        webapp)  report_typescript webapp webapp ;;
        shim)    report_typescript shim agent-shim/claude/shim ;;
        proto)   report_proto ;;
    esac
done

log "all requested coverage reports completed"
