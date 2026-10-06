#!/usr/bin/env bash
#
# Hermetic tests for report-nonlisp-coverage.sh.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/lib-grep-in.sh"

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_SRC="$THIS_DIR/report-nonlisp-coverage.sh"
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
    local tree="$1" component
    mkdir -p "$tree/modules/app/agent-repl/bin"
    cp "$SCRIPT_SRC" "$tree/modules/app/agent-repl/bin/"
    chmod +x "$tree/modules/app/agent-repl/bin/report-nonlisp-coverage.sh"

    for component in \
        daemon \
        agent-shim/claude/shim-sidecar \
        agent-shim/shim-store \
        agent-shim/shim-lock \
        agent-shim/logging/go; do
        mkdir -p "$tree/modules/app/agent-repl/$component"
        printf 'module fixture\n' \
            >"$tree/modules/app/agent-repl/$component/go.mod"
    done

    for component in webapp agent-shim/claude/shim; do
        mkdir -p "$tree/modules/app/agent-repl/$component"
        printf '{}\n' \
            >"$tree/modules/app/agent-repl/$component/package.json"
    done

    mkdir -p "$tree/modules/app/agent-repl/e2e"
    printf 'coverage:\n\t@true\n' \
        >"$tree/modules/app/agent-repl/e2e/Makefile"

    mkdir -p "$tree/modules/app/agent-repl/proto"
    printf 'validate:\n\t@true\n' \
        >"$tree/modules/app/agent-repl/proto/Makefile"
}

make_stubs() {
    local stubs="$1"
    mkdir -p "$stubs"

    cat >"$stubs/go" <<'EOF'
#!/usr/bin/env bash
printf '%s|go %s\n' "$PWD" "$*" >>"$STUB_LOG"
if [ "${GO_STUB_FAIL_TEST:-0}" = "1" ] && [ "${1:-}" = "test" ]; then
    exit 1
fi
case "${1:-} ${2:-}" in
    "test -count=1")
        for arg in "$@"; do
            case "$arg" in
                -coverprofile=*)
                    profile="${arg#-coverprofile=}"
                    printf 'mode: set\nfixture/file.go:1.1,1.2 1 1\n' >"$profile"
                    ;;
            esac
        done
        ;;
    "tool cover")
        [ "${GO_STUB_FAIL_REPORT:-0}" = "1" ] && exit 1
        if [ "${GO_STUB_MALFORMED_REPORT:-0}" = "1" ]; then
            printf 'malformed report\n'
            exit 0
        fi
        printf 'fixture/file.go:1:\tfixture\t100.0%%\n'
        printf 'total:\t(statements)\t100.0%%\n'
        ;;
esac
EOF

    cat >"$stubs/npm" <<'EOF'
#!/usr/bin/env bash
printf '%s|npm %s\n' "$PWD" "$*" >>"$STUB_LOG"
case "${NPM_STUB_FAIL_COMMAND:-}" in
    typecheck)
        [ "$*" = "run typecheck" ] && exit 1
        ;;
    coverage)
        case "$*" in
            "run coverage"*) exit 1 ;;
        esac
        ;;
esac
exit 0
EOF

    cat >"$stubs/make" <<'EOF'
#!/usr/bin/env bash
printf '%s|make %s\n' "$PWD" "$*" >>"$STUB_LOG"
[ "${MAKE_STUB_FAIL:-0}" = "1" ] && exit 1
exit 0
EOF

    chmod +x "$stubs/go" "$stubs/npm" "$stubs/make"
}

run_report() {
    local tree="$1"
    shift
    STUB_LOG="$tree/stub.log"
    : >"$STUB_LOG"
    set +e
    PATH="$tree/stubs:/usr/bin:/bin" \
        STUB_LOG="$STUB_LOG" \
        GO_STUB_FAIL_TEST="${GO_STUB_FAIL_TEST:-0}" \
        GO_STUB_FAIL_REPORT="${GO_STUB_FAIL_REPORT:-0}" \
        GO_STUB_MALFORMED_REPORT="${GO_STUB_MALFORMED_REPORT:-0}" \
        NPM_STUB_FAIL_COMMAND="${NPM_STUB_FAIL_COMMAND:-}" \
        MAKE_STUB_FAIL="${MAKE_STUB_FAIL:-0}" \
        "$tree/modules/app/agent-repl/bin/report-nonlisp-coverage.sh" "$@" \
        >"$tree/stdout" 2>"$tree/stderr"
    RUN_RC=$?
    set -e
}

test_default_runs_every_component() {
    local tree="$TMP/default"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    run_report "$tree"

    if [ "$RUN_RC" -eq 0 ] &&
        [ "$(grep -c '|go test ' "$tree/stub.log")" -eq 5 ] &&
        [ "$(grep -c '|go tool cover ' "$tree/stub.log")" -eq 5 ] &&
        [ "$(grep -c '|npm run typecheck' "$tree/stub.log")" -eq 2 ] &&
        [ "$(grep -c '|npm run coverage' "$tree/stub.log")" -eq 2 ] &&
        grep -q '|make -C .*proto validate' "$tree/stub.log"; then
        pass "default run covers every Go, TypeScript, and proto component"
    else
        fail "default run covers every Go, TypeScript, and proto component"
    fi
}

# The contract is validated before the long suites compile against it, so a
# stale stub fails the sweep first.
test_default_validates_proto_before_any_suite() {
    local tree="$TMP/proto-first"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    run_report "$tree"

    if [ "$RUN_RC" -eq 0 ] &&
        grep_in "$(head -n 1 "$tree/stub.log")" -q '|make -C .*proto validate'; then
        pass "default run validates proto before any other component"
    else
        fail "default run validates proto before any other component"
    fi
}

# THE COMPONENT LIST LIVES IN THE SCRIPT ALONE. proto/Makefile once restated
# it and kept naming the deleted `wire` module, which the script refused, so
# `make coverage` there could not run at all.
test_proto_makefile_names_no_components() {
    local makefile="$THIS_DIR/../proto/Makefile" recipe
    recipe="$(sed -n '/^coverage:/,/^$/p' "$makefile" |
        grep 'report-nonlisp-coverage.sh' || true)"

    if [ -n "$recipe" ] &&
        grep_in "$recipe" -Eq 'report-nonlisp-coverage\.sh[[:space:]]*$'; then
        pass "proto coverage defers to the script's own component list"
    else
        fail "proto coverage defers to the script's own component list"
        printf '  recipe: %s\n' "$recipe" >&2
    fi
}

test_selection_runs_only_requested_components() {
    local tree="$TMP/selection"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    run_report "$tree" store webapp

    if [ "$RUN_RC" -eq 0 ] &&
        [ "$(grep -c '|go test ' "$tree/stub.log")" -eq 1 ] &&
        [ "$(grep -c '|npm run typecheck' "$tree/stub.log")" -eq 1 ] &&
        ! grep -q '|make ' "$tree/stub.log"; then
        pass "explicit selection runs only requested components"
    else
        fail "explicit selection runs only requested components"
    fi
}

test_unknown_component_fails_before_running_tools() {
    local tree="$TMP/unknown"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    run_report "$tree" mystery

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "unknown component 'mystery'" "$tree/stderr" &&
        [ ! -s "$tree/stub.log" ]; then
        pass "unknown component fails before running tools"
    else
        fail "unknown component fails before running tools"
    fi
}

test_go_failure_is_loud() {
    local tree="$TMP/go-failure"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    GO_STUB_FAIL_TEST=1 run_report "$tree" daemon

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "daemon Go coverage suite failed" "$tree/stderr"; then
        pass "Go test failure aborts with component context"
    else
        fail "Go test failure aborts with component context"
    fi
}

test_go_report_failure_is_loud() {
    local tree="$TMP/go-report-failure"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    GO_STUB_FAIL_REPORT=1 run_report "$tree" daemon

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "daemon Go coverage report failed" "$tree/stderr"; then
        pass "Go report failure aborts with component context"
    else
        fail "Go report failure aborts with component context"
    fi
}

test_malformed_go_report_is_loud() {
    local tree="$TMP/go-report-malformed"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    GO_STUB_MALFORMED_REPORT=1 run_report "$tree" daemon

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "daemon Go coverage summary is malformed" "$tree/stderr"; then
        pass "malformed Go report aborts with component context"
    else
        fail "malformed Go report aborts with component context"
    fi
}

test_missing_go_tool_is_loud() {
    local tree="$TMP/go-tool-missing"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    rm "$tree/stubs/go"
    run_report "$tree" daemon

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "required command 'go' is unavailable" "$tree/stderr"; then
        pass "missing Go toolchain aborts explicitly"
    else
        fail "missing Go toolchain aborts explicitly"
    fi
}

test_missing_go_module_is_loud() {
    local tree="$TMP/go-module-missing"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    rm "$tree/modules/app/agent-repl/daemon/go.mod"
    run_report "$tree" daemon

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "daemon module is missing" "$tree/stderr"; then
        pass "missing Go module aborts explicitly"
    else
        fail "missing Go module aborts explicitly"
    fi
}

test_typescript_typecheck_failure_is_loud() {
    local tree="$TMP/typescript-failure"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    NPM_STUB_FAIL_COMMAND=typecheck run_report "$tree" shim

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "shim TypeScript typecheck failed" "$tree/stderr"; then
        pass "TypeScript failure aborts with component context"
    else
        fail "TypeScript failure aborts with component context"
    fi
}

test_typescript_coverage_failure_is_loud() {
    local tree="$TMP/typescript-coverage-failure"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    NPM_STUB_FAIL_COMMAND=coverage run_report "$tree" shim

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "shim TypeScript coverage suite failed" "$tree/stderr"; then
        pass "TypeScript coverage failure aborts with component context"
    else
        fail "TypeScript coverage failure aborts with component context"
    fi
}

test_missing_typescript_package_is_loud() {
    local tree="$TMP/typescript-package-missing"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    rm "$tree/modules/app/agent-repl/webapp/package.json"
    run_report "$tree" webapp

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "webapp package is missing" "$tree/stderr"; then
        pass "missing TypeScript package aborts explicitly"
    else
        fail "missing TypeScript package aborts explicitly"
    fi
}

test_typescript_packages_validate_all_declared_dependencies() {
    local webapp_package="$THIS_DIR/../webapp/package.json"
    local shim_package="$THIS_DIR/../agent-shim/claude/shim/package.json"
    local package

    for package in "$webapp_package" "$shim_package"; do
        if ! grep -Eq '"ensure-deps": "(\.\./)+bin/ensure-deps\.sh"' "$package" ||
            ! grep -Eq '"precoverage": "((\.\./)+bin/background\.sh )?npm run ensure-deps"' "$package" ||
            ! grep -Fq '"pretypecheck": "npm run ensure-deps"' "$package"; then
            fail "TypeScript packages validate every declared dependency"
            return
        fi
    done
    pass "TypeScript packages validate every declared dependency"
}

test_e2e_runs_only_when_named() {
    local tree="$TMP/e2e-opt-in"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    run_report "$tree"

    if [ "$RUN_RC" -eq 0 ] &&
        ! grep -q '|make -C .*e2e coverage' "$tree/stub.log"; then
        pass "the e2e suite stays out of the default sweep"
    else
        fail "the e2e suite stays out of the default sweep"
    fi
}

test_e2e_dispatches_to_the_e2e_makefile() {
    local tree="$TMP/e2e-named"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    run_report "$tree" e2e

    if [ "$RUN_RC" -eq 0 ] &&
        grep -q '|make -C .*e2e coverage' "$tree/stub.log" &&
        ! grep -q '|go test ' "$tree/stub.log"; then
        pass "the named e2e component runs the e2e coverage target"
    else
        fail "the named e2e component runs the e2e coverage target"
    fi
}

test_e2e_failure_is_loud() {
    local tree="$TMP/e2e-failure"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    MAKE_STUB_FAIL=1 run_report "$tree" e2e

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "e2e coverage suite failed" "$tree/stderr"; then
        pass "e2e coverage failure aborts explicitly"
    else
        fail "e2e coverage failure aborts explicitly"
    fi
}

test_missing_e2e_makefile_is_loud() {
    local tree="$TMP/e2e-makefile-missing"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    rm "$tree/modules/app/agent-repl/e2e/Makefile"
    run_report "$tree" e2e

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "e2e Makefile is missing" "$tree/stderr"; then
        pass "missing e2e Makefile aborts explicitly"
    else
        fail "missing e2e Makefile aborts explicitly"
    fi
}

test_proto_validation_failure_is_loud() {
    local tree="$TMP/proto-validation-failure"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    MAKE_STUB_FAIL=1 run_report "$tree" proto

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "proto validation failed" "$tree/stderr"; then
        pass "proto validation failure aborts explicitly"
    else
        fail "proto validation failure aborts explicitly"
    fi
}

test_missing_proto_makefile_is_loud() {
    local tree="$TMP/proto-makefile-missing"
    make_tree "$tree"
    make_stubs "$tree/stubs"
    rm "$tree/modules/app/agent-repl/proto/Makefile"
    run_report "$tree" proto

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "proto Makefile is missing" "$tree/stderr"; then
        pass "missing proto Makefile aborts explicitly"
    else
        fail "missing proto Makefile aborts explicitly"
    fi
}

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-coverage-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

test_default_runs_every_component
test_default_validates_proto_before_any_suite
test_proto_makefile_names_no_components
test_selection_runs_only_requested_components
test_unknown_component_fails_before_running_tools
test_go_failure_is_loud
test_go_report_failure_is_loud
test_malformed_go_report_is_loud
test_missing_go_tool_is_loud
test_missing_go_module_is_loud
test_typescript_typecheck_failure_is_loud
test_typescript_coverage_failure_is_loud
test_missing_typescript_package_is_loud
test_typescript_packages_validate_all_declared_dependencies
test_proto_validation_failure_is_loud
test_missing_proto_makefile_is_loud
test_e2e_runs_only_when_named
test_e2e_dispatches_to_the_e2e_makefile
test_e2e_failure_is_loud
test_missing_e2e_makefile_is_loud

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
