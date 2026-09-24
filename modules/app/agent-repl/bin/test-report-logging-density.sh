#!/usr/bin/env bash
# Source-fixture and repository-source tests for report-logging-density.sh.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_SRC="$THIS_DIR/report-logging-density.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-logging-density-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

PASS=0
FAIL=0

pass() {
    printf '  PASS: %s\n' "$1"
    PASS=$((PASS + 1))
}

fail() {
    printf '  FAIL: %s\n' "$1" >&2
    FAIL=$((FAIL + 1))
}

make_tree() {
    local tree="$1" bin="$1/modules/app/agent-repl/bin"
    mkdir -p \
        "$bin" \
        "$tree/modules/app/agent-repl/daemon" \
        "$tree/modules/app/agent-repl/agent-shim/claude/shim-sidecar" \
        "$tree/modules/app/agent-repl/agent-shim/shim-store" \
        "$tree/modules/app/agent-repl/agent-shim/logging/go" \
        "$tree/modules/app/agent-repl/agent-shim/claude/shim/src" \
        "$tree/modules/app/agent-repl/webapp/src" \
        "$tree/modules/app/agent-repl/lisp"
    cp "$SCRIPT_SRC" "$bin/report-logging-density.sh"
    chmod +x "$bin/report-logging-density.sh"

    printf '%s\n' \
        'package daemon' \
        'func run() {' \
        '  logger.Debug("op", "detail", nil)' \
        '  logger.Info("op", "start", nil)' \
        '  logger.Warn("op", "decision", nil)' \
        '  logger.Error("op", "failed", nil)' \
        '}' >"$tree/modules/app/agent-repl/daemon/main.go"
    printf '%s\n' \
        'package daemon' \
        'func TestIgnored() { logger.Error("op", "test", nil) }' \
        >"$tree/modules/app/agent-repl/daemon/main_test.go"
    printf '%s\n' \
        'package sidecar' \
        'func run() {' \
        '  logger.Log("start")' \
        '  logger.With(logging.Context{Level: "warn"}).Log("decision")' \
        '  logger.With(logging.Context{Level: "error"}).Log("failed")' \
        '  logger.LogVerbose("detail")' \
        '}' >"$tree/modules/app/agent-repl/agent-shim/claude/shim-sidecar/main.go"
    printf '%s\n' \
        'package store' \
        'func run() {' \
        '  logger.Log(logging.Fields{}, "start")' \
        '  logger.Log(logging.Fields{Level: "warn"}, "decision")' \
        '  logger.Log(logging.Fields{Level: "error"}, "failed")' \
        '  logger.LogVerbose(logging.Fields{}, "detail")' \
        '}' >"$tree/modules/app/agent-repl/agent-shim/shim-store/main.go"
    printf '%s\n' \
        'package logging' \
        'func Timestamp() string { return "" }' \
        >"$tree/modules/app/agent-repl/agent-shim/logging/go/timestamp.go"
    printf '%s\n' \
        'log.debug({}, "detail");' \
        'log.info({}, "start");' \
        'log.warn({}, "decision");' \
        'log.error({}, "failed");' \
        'log.logVerbose({}, "trace");' \
        >"$tree/modules/app/agent-repl/agent-shim/claude/shim/src/main.ts"
    printf '%s\n' \
        'log.debug("detail", { verbosity: "verbose" });' \
        'log.info("start", options);' \
        'log.warn("decision", options);' \
        'log.error("failed", options);' \
        >"$tree/modules/app/agent-repl/webapp/src/main.ts"
    printf '%s\n' \
        '(agent-repl--log ws "detail")' \
        '(agent-repl--log-verbose ws "trace")' \
        '(agent-repl--info ws "start")' \
        '(agent-repl--warn ws "decision")' \
        '(agent-repl--warn-once ws "fingerprint" "decision")' \
        '(agent-repl--error ws "failed")' \
        '(agent-repl--fatal ws "failed loudly")' \
        >"$tree/modules/app/agent-repl/lisp/main.el"
    printf '%s\n' \
        '(agent-repl--error ws "test")' \
        >"$tree/modules/app/agent-repl/lisp/test-main.el"
}

field() {
    local report="$1" component="$2" column="$3"
    awk -F, -v component="$component" -v column="$column" \
        '$1 == component { print $column }' <<<"$report"
}

unexpected_zero_components() {
    awk -F, '
        NR > 1 && $4 > 0 && $5 == 0 && $13 != "allowed-no-own-calls" {
            print $1
        }
    ' <<<"$1"
}

run_report() {
    local tree="$1"
    shift
    "$tree/modules/app/agent-repl/bin/report-logging-density.sh" "$@"
}

test_default_report_counts_every_canonical_api() {
    local tree="$TMP/default" report expected row component violations

    # Arrange
    make_tree "$tree"
    expected='daemon;go;1;7;4;1;1;1;1;0;571.43;required
sidecar;go;1;7;4;1;1;1;1;1;571.43;required
store;go;1;7;4;1;1;1;1;1;571.43;required
logging;go;1;2;0;0;0;0;0;0;0.00;allowed-no-own-calls
shim;typescript;1;5;5;2;1;1;1;1;1000.00;required
webapp;typescript;1;4;4;1;1;1;1;1;1000.00;required
emacs;elisp;1;7;7;2;1;2;2;1;1000.00;required'

    # Act
    report="$(run_report "$tree")"
    violations="$(unexpected_zero_components "$report")"

    # Assert
    if [ "$(printf '%s\n' "$report" | wc -l | tr -d ' ')" -ne 8 ]; then
        fail "default report emits one header and seven systems"
        return
    fi
    if [ -n "$violations" ]; then
        fail "every system with source lines has canonical calls"
        return
    fi
    while IFS=';' read -r component _; do
        row="$(awk -F, -v component="$component" \
            '$1 == component { print $1 ";" $2 ";" $3 ";" $4 ";" $5 ";" $6 ";" $7 ";" $8 ";" $9 ";" $10 ";" $11 ";" $13 }' \
            <<<"$report")"
        if ! grep -Fqx "$row" <<<"$expected"; then
            fail "default report counts $component canonical calls by level"
            return
        fi
    done <<<"$expected"
    pass "default report counts every canonical API by level"
}

test_report_prints_each_canonical_pattern() {
    local tree="$TMP/patterns" report expected component pattern

    # Arrange
    make_tree "$tree"
    expected='daemon;\.(Debug|Info|Warn|Error)\(
sidecar;\.(Log|LogVerbose)\(
store;\.(Log|LogVerbose)\(
logging;\.(Log|LogVerbose)\(
shim;\.(debug|info|warn|error|logVerbose)\(
webapp;(^|[^.[:alnum:]_])log\.(debug|info|warn|error)\(
emacs;\(agent-repl--(log|log-verbose|info|warn|warn-once|error|fatal)[[:space:]]'

    # Act
    report="$(run_report "$tree")"

    # Assert
    while IFS=';' read -r component pattern; do
        if [ "$(field "$report" "$component" 12)" != "$pattern" ]; then
            fail "report prints the $component canonical pattern"
            return
        fi
    done <<<"$expected"
    pass "report prints every canonical pattern"
}

test_component_selection_isolated_to_requested_system() {
    local tree="$TMP/selection" report

    # Arrange
    make_tree "$tree"

    # Act
    report="$(run_report "$tree" store)"

    # Assert
    if [ "$(printf '%s\n' "$report" | wc -l | tr -d ' ')" -eq 2 ] &&
        [ "$(field "$report" store 5)" = 4 ]; then
        pass "component selection isolates the requested system"
    else
        fail "component selection isolates the requested system"
    fi
}

test_unknown_component_fails() {
    local tree="$TMP/unknown"

    # Arrange
    make_tree "$tree"

    # Act / Assert
    if run_report "$tree" mystery >/dev/null 2>&1; then
        fail "unknown component fails"
    else
        pass "unknown component fails"
    fi
}

test_zero_call_guard_rejects_a_sourced_system() {
    local tree="$TMP/zero-required" report violations

    # Arrange
    make_tree "$tree"
    printf '%s\n' \
        'package daemon' \
        'func run() {}' \
        >"$tree/modules/app/agent-repl/daemon/main.go"

    # Act
    report="$(run_report "$tree")"
    violations="$(unexpected_zero_components "$report")"

    # Assert
    if [ "$violations" = daemon ]; then
        pass "zero-call guard rejects a sourced system"
    else
        fail "zero-call guard rejects a sourced system"
    fi
}

test_zero_call_guard_allows_the_shared_package() {
    local tree="$TMP/zero-allowed" report violations

    # Arrange
    make_tree "$tree"

    # Act
    report="$(run_report "$tree")"
    violations="$(unexpected_zero_components "$report")"

    # Assert
    if [ -z "$violations" ] &&
        [ "$(field "$report" logging 13)" = allowed-no-own-calls ]; then
        pass "zero-call guard allows the shared package explicitly"
    else
        fail "zero-call guard allows the shared package explicitly"
    fi
}

test_binary_marked_source_is_counted_as_authored_text() {
    local tree="$TMP/binary-source" report source

    # Arrange
    make_tree "$tree"
    source="$tree/modules/app/agent-repl/agent-shim/claude/shim/src/main.ts"
    printf 'const nul = "\0";\n' >"$source"
    printf '%s\n' 'log.error({}, "failed");' >>"$source"

    # Act
    report="$(run_report "$tree" shim)"

    # Assert
    if [ "$(field "$report" shim 5)" = 1 ] &&
        [ "$(field "$report" shim 9)" = 1 ]; then
        pass "binary-marked authored source is counted as text"
    else
        fail "binary-marked authored source is counted as text"
    fi
}

test_repository_sources_have_no_unexpected_zero_calls() {
    local report violations

    # Arrange
    [ -x "$SCRIPT_SRC" ] || {
        fail "repository sources have no unexpected zero-call systems"
        return
    }

    # Act
    report="$("$SCRIPT_SRC")"
    violations="$(unexpected_zero_components "$report")"

    # Assert
    if [ -z "$violations" ]; then
        pass "repository sources have no unexpected zero-call systems"
    else
        fail "repository sources have no unexpected zero-call systems: $violations"
    fi
}

test_default_report_counts_every_canonical_api
test_report_prints_each_canonical_pattern
test_component_selection_isolated_to_requested_system
test_unknown_component_fails
test_zero_call_guard_rejects_a_sourced_system
test_zero_call_guard_allows_the_shared_package
test_binary_marked_source_is_counted_as_authored_text
test_repository_sources_have_no_unexpected_zero_calls

if [ "$FAIL" -ne 0 ]; then
    printf 'FAIL: %d logging-density test(s) failed; %d passed\n' "$FAIL" "$PASS" >&2
    exit 1
fi
printf 'PASS: report-logging-density (%d tests)\n' "$PASS"
