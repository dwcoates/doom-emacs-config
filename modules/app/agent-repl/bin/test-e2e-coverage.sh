#!/usr/bin/env bash
#
# Hermetic tests for e2e-coverage.sh and e2e-shim-coverage-summary.mjs.
# Every external tool (go, npx, node's c8 render) is stubbed; no suite runs.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -uo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/lib-grep-in.sh"

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_SRC="$THIS_DIR/e2e-coverage.sh"
SUMMARY_SRC="$THIS_DIR/e2e-shim-coverage-summary.mjs"
PASS=0
FAIL=0

pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; FAIL=$((FAIL + 1)); }

make_tree() {
    local tree="$1" module
    mkdir -p "$tree/bin"
    cp "$SCRIPT_SRC" "$SUMMARY_SRC" "$tree/bin/"
    chmod +x "$tree/bin/e2e-coverage.sh"
    for module in e2e daemon agent-shim/shim-store agent-shim/claude/shim-sidecar agent-shim/claude/shim; do
        mkdir -p "$tree/$module"
        printf 'module fixture\n' >"$tree/$module/go.mod"
    done
    # ensure-e2e-deps.sh has hermetic tests of its own (test-build-frontend.sh);
    # here it is a stub recording that it ran, exiting ENSURE_STUB_STATUS.
    cat >"$tree/bin/ensure-e2e-deps.sh" <<EOF
#!/usr/bin/env bash
echo ran >>"$tree/ensure.log"
exit "\${ENSURE_STUB_STATUS:-0}"
EOF
    chmod +x "$tree/bin/ensure-e2e-deps.sh"
}

# make_stubs writes a `go` that fabricates counter files on `test` and a
# report on `cover`, and a `c8` (the shim's pinned devDependency, at its
# node_modules/.bin path) that writes a json-summary.
make_stubs() {
    local stubs="$1"
    mkdir -p "$stubs"
    cat >"$stubs/go" <<'EOF'
#!/usr/bin/env bash
case "${1:-} ${2:-}" in
    "test -count=1")
        if [ "${GO_STUB_NO_COUNTERS:-0}" != "1" ]; then
            for binary in claude-repld shim-store shim-claude-sidecar; do
                mkdir -p "$AGENT_REPL_E2E_COVERAGE/$binary"
                : >"$AGENT_REPL_E2E_COVERAGE/$binary/covmeta.deadbeef"
            done
            mkdir -p "$AGENT_REPL_E2E_COVERAGE/shim"
            printf '{}' >"$AGENT_REPL_E2E_COVERAGE/shim/coverage-1.json"
        fi
        exit "${GO_STUB_TEST_STATUS:-0}"
        ;;
    "tool covdata")
        for arg in "$@"; do
            case "$arg" in -o=*) printf 'mode: set\n' >"${arg#-o=}" ;; esac
        done
        ;;
    "tool cover")
        if [ "${GO_STUB_MALFORMED_REPORT:-0}" = "1" ]; then
            printf 'malformed\n'
        else
            printf 'total:\t(statements)\t42.0%%\n'
        fi
        ;;
esac
exit 0
EOF
    mkdir -p "$stubs/../agent-shim/claude/shim/node_modules/.bin"
    cat >"$stubs/../agent-shim/claude/shim/node_modules/.bin/c8" <<'EOF'
#!/usr/bin/env bash
reports_dir=""
for arg in "$@"; do
    case "$arg" in --reports-dir=*) reports_dir="${arg#--reports-dir=}" ;; esac
done
[ "${C8_STUB_FAIL:-0}" = "1" ] && exit 1
mkdir -p "$reports_dir"
cat >"$reports_dir/coverage-summary.json" <<'JSON'
{
  "total": {"statements": {"covered": 9, "total": 20}},
  "/private/x/agent-shim/claude/shim/src/main.ts": {"statements": {"covered": 6, "total": 10}},
  "/private/x/agent-shim/claude/shim/node_modules/dep/index.js": {"statements": {"covered": 3, "total": 10}}
}
JSON
exit 0
EOF
    chmod +x "$stubs/go" "$stubs/../agent-shim/claude/shim/node_modules/.bin/c8"
}

run_coverage() {
    local tree="$1"
    shift
    set +e
    PATH="$tree/stubs:$(dirname "$(command -v node)"):/usr/bin:/bin" \
        AGENT_REPL_E2E_COVERAGE_DIR="$tree/cov" \
        GO_STUB_TEST_STATUS="${GO_STUB_TEST_STATUS:-0}" \
        GO_STUB_NO_COUNTERS="${GO_STUB_NO_COUNTERS:-0}" \
        GO_STUB_MALFORMED_REPORT="${GO_STUB_MALFORMED_REPORT:-0}" \
        C8_STUB_FAIL="${C8_STUB_FAIL:-0}" \
        ENSURE_STUB_STATUS="${ENSURE_STUB_STATUS:-0}" \
        "$tree/bin/e2e-coverage.sh" "$@" >"$tree/stdout" 2>"$tree/stderr"
    RUN_RC=$?
    set -e
}

setup() {
    local tree="$1"
    rm -rf "$tree"
    make_tree "$tree"
    make_stubs "$tree/stubs"
}

test_happy_path_reports_every_module() {
    local tree="$TMP/happy"
    setup "$tree"
    run_coverage "$tree"

    if [ "$RUN_RC" -eq 0 ] &&
        grep -q 'claude-repld: total:' "$tree/stdout" &&
        grep -q 'shim-store: total:' "$tree/stdout" &&
        grep -q 'shim-claude-sidecar: total:' "$tree/stdout" &&
        grep -q 'shim: 60.0% (6/10 statements over 1 files)' "$tree/stdout"; then
        pass "a clean run reports all three Go binaries and the shim"
    else
        fail "a clean run reports all three Go binaries and the shim"
    fi
}

test_suite_failure_still_reports_and_fails() {
    local tree="$TMP/suite-failure"
    setup "$tree"
    GO_STUB_TEST_STATUS=1 run_coverage "$tree"

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q 'suite: FAIL' "$tree/stdout" &&
        grep -q 'claude-repld: total:' "$tree/stdout"; then
        pass "a failing suite still reports the coverage it produced"
    else
        fail "a failing suite still reports the coverage it produced"
    fi
}

test_missing_counters_are_loud() {
    local tree="$TMP/no-counters"
    setup "$tree"
    GO_STUB_NO_COUNTERS=1 run_coverage "$tree"

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q 'claude-repld wrote no coverage counters' "$tree/stderr" &&
        grep -q 'the shim wrote no v8 coverage' "$tree/stderr"; then
        pass "a system that wrote no counters is reported, never ignored"
    else
        fail "a system that wrote no counters is reported, never ignored"
    fi
}

test_malformed_go_summary_is_loud() {
    local tree="$TMP/malformed"
    setup "$tree"
    GO_STUB_MALFORMED_REPORT=1 run_coverage "$tree"

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q 'coverage summary is malformed' "$tree/stderr"; then
        pass "a malformed Go summary is reported"
    else
        fail "a malformed Go summary is reported"
    fi
}

test_c8_failure_is_loud() {
    local tree="$TMP/c8-failure"
    setup "$tree"
    C8_STUB_FAIL=1 run_coverage "$tree"

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q 'c8 report failed' "$tree/stderr"; then
        pass "a failing c8 report is reported"
    else
        fail "a failing c8 report is reported"
    fi
}

test_npm_deps_are_ensured_before_the_suite() {
    local tree="$TMP/deps-ensured"
    setup "$tree"
    run_coverage "$tree"

    if [ "$RUN_RC" -eq 0 ] && [ "$(cat "$tree/ensure.log" 2>/dev/null)" = "ran" ]; then
        pass "the e2e suite's npm deps are ensured once before it runs"
    else
        fail "the e2e suite's npm deps are ensured once before it runs"
    fi
}

test_failed_npm_deps_abort_before_the_suite() {
    local tree="$TMP/deps-failed"
    setup "$tree"
    ENSURE_STUB_STATUS=3 run_coverage "$tree"

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "npm deps could not be ensured" "$tree/stderr" &&
        [ ! -d "$tree/cov/claude-repld" ]; then
        pass "npm deps that cannot be ensured abort the run before the suite"
    else
        fail "npm deps that cannot be ensured abort the run before the suite"
    fi
}

test_summary_counts_only_our_sources() {
    local dir="$TMP/summary-ours"
    mkdir -p "$dir"
    cat >"$dir/coverage-summary.json" <<'JSON'
{
  "total": {"statements": {"covered": 30, "total": 100}},
  "/private/x/agent-shim/claude/shim/src/main.ts": {"statements": {"covered": 5, "total": 10}},
  "/private/x/agent-shim/logging/ts/timestamp.ts": {"statements": {"covered": 5, "total": 10}},
  "/private/x/agent-shim/claude/shim/node_modules/dep/index.js": {"statements": {"covered": 20, "total": 80}}
}
JSON
    local got
    got="$(node "$SUMMARY_SRC" "$dir" 2>&1)"
    if [ "$got" = "50.0% (10/20 statements over 2 files)" ]; then
        pass "the shim summary counts our sources and excludes inlined dependencies"
    else
        fail "the shim summary counts our sources and excludes inlined dependencies (got: $got)"
    fi
}

test_summary_without_our_sources_is_loud() {
    local dir="$TMP/summary-none"
    mkdir -p "$dir"
    cat >"$dir/coverage-summary.json" <<'JSON'
{
  "total": {"statements": {"covered": 1, "total": 2}},
  "/private/x/other/thing.js": {"statements": {"covered": 1, "total": 2}}
}
JSON
    set +e
    local out
    out="$(node "$SUMMARY_SRC" "$dir" 2>&1)"
    local rc=$?
    set -e
    if [ "$rc" -ne 0 ] && grep_in "$out" -q "no agent-shim source survived the remap"; then
        pass "a remap that produced no shim source is reported"
    else
        fail "a remap that produced no shim source is reported"
    fi
}

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-e2e-coverage-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

test_happy_path_reports_every_module
test_suite_failure_still_reports_and_fails
test_missing_counters_are_loud
test_malformed_go_summary_is_loud
test_c8_failure_is_loud
test_npm_deps_are_ensured_before_the_suite
test_failed_npm_deps_abort_before_the_suite
test_summary_counts_only_our_sources
test_summary_without_our_sources_is_loud

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
