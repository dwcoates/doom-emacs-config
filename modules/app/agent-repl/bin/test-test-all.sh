#!/usr/bin/env bash
#
# Hermetic tests for test-all.sh, the entry point that builds the test runner
# (../testrun) and runs it under the host suite slot and the git-state net.
#
# Everything test-all.sh used to do itself -- the roster, --suites, --record,
# the summaries, the regression report -- now lives in testrun and is tested
# there (testrun/internal/cli). What remains here is the wrapper's own
# contract, exercised against stubs: a fake `go` that "builds" a recording
# testrun, and recording stand-ins for suite-slot.sh and safe-test-run.sh.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_SRC="$THIS_DIR/test-all.sh"
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

# make_tree lays out a fake repository: the real test-all.sh, recording
# stand-ins for the two wrappers, and a stub `go` whose `build -o OUT` writes a
# testrun that logs its arguments and exits with $STUB_TESTRUN_EXIT.
make_tree() {
    local tree="$1"
    local module="$tree/modules/app/agent-repl"
    mkdir -p "$module/bin" "$module/testrun" "$tree/.claude" "$tree/stubs"
    cp "$SCRIPT_SRC" "$module/bin/test-all.sh"

    cat >"$module/bin/suite-slot.sh" <<'EOF'
#!/usr/bin/env bash
printf 'slot\n' >>"$STUB_LOG"
exec "$@"
EOF
    cat >"$tree/.claude/safe-test-run.sh" <<'EOF'
#!/usr/bin/env bash
printf 'safe %s\n' "$1" >>"$STUB_LOG"
[ "$1" = "--" ] || exit 99
shift
exec "$@"
EOF
    cat >"$tree/stubs/go" <<'EOF'
#!/usr/bin/env bash
printf 'go %s (in %s)\n' "$*" "$PWD" >>"$STUB_LOG"
[ "${STUB_GO_FAIL:-0}" = 1 ] && { echo "stub go: compile error" >&2; exit 1; }
[ "$1" = build ] && [ "$2" = -o ] || exit 98
cat >"$3" <<'INNER'
#!/usr/bin/env bash
printf 'testrun %s\n' "$*" >>"$STUB_LOG"
exit "${STUB_TESTRUN_EXIT:-0}"
INNER
chmod +x "$3"
EOF
    chmod +x "$module/bin/test-all.sh" "$module/bin/suite-slot.sh" \
        "$tree/.claude/safe-test-run.sh" "$tree/stubs/go"
}

run_test_all() {
    local tree="$1" path="$2"
    shift 2
    STUB_LOG="$tree/stub.log"
    : >"$STUB_LOG"
    set +e
    PATH="$path" STUB_LOG="$STUB_LOG" \
        STUB_GO_FAIL="${STUB_GO_FAIL:-0}" STUB_TESTRUN_EXIT="${STUB_TESTRUN_EXIT:-0}" \
        "$tree/modules/app/agent-repl/bin/test-all.sh" "$@" \
        >"$tree/stdout" 2>"$tree/stderr"
    RUN_RC=$?
    set -e
}

stub_path() { printf '%s:/usr/bin:/bin' "$1/stubs"; }

test_runs_testrun_inside_the_slot_and_the_git_net() {
    local tree
    tree="$(mktemp -d)"
    make_tree "$tree"
    run_test_all "$tree" "$(stub_path "$tree")" --suites ert,daemon --record
    local module want
    module="$(cd "$tree/modules/app/agent-repl" && pwd)"
    want="$(printf 'slot\nsafe --\ntestrun run --module %s --suites ert,daemon --record' "$module")"
    if [ "$RUN_RC" -eq 0 ] && [ "$(grep -v '^go ' "$STUB_LOG")" = "$want" ]; then
        pass "testrun runs under the suite slot, then the git net, with every argument"
    else
        fail "wrapper order or arguments (rc=$RUN_RC): $(cat "$STUB_LOG")"
    fi
    rm -rf "$tree"
}

test_builds_the_runner_from_its_own_tree() {
    local tree
    tree="$(mktemp -d)"
    make_tree "$tree"
    run_test_all "$tree" "$(stub_path "$tree")"
    if grep -q "^go build -o .*/testrun \. (in $(cd "$tree/modules/app/agent-repl/testrun" && pwd))$" "$STUB_LOG"; then
        pass "the runner is built from the checkout test-all.sh lives in"
    else
        fail "runner build: $(cat "$STUB_LOG")"
    fi
    rm -rf "$tree"
}

test_the_runners_exit_status_is_the_runs() {
    local tree
    tree="$(mktemp -d)"
    make_tree "$tree"
    STUB_TESTRUN_EXIT=3 run_test_all "$tree" "$(stub_path "$tree")"
    if [ "$RUN_RC" -eq 3 ]; then
        pass "a failing runner fails test-all with its own status"
    else
        fail "runner exit 3 became $RUN_RC"
    fi
    rm -rf "$tree"
}

test_a_failed_build_fails_before_anything_runs() {
    local tree
    tree="$(mktemp -d)"
    make_tree "$tree"
    STUB_GO_FAIL=1 run_test_all "$tree" "$(stub_path "$tree")"
    if [ "$RUN_RC" -eq 1 ] &&
        grep -q 'ERROR: building the test runner in .*/testrun failed' "$tree/stderr" &&
        ! grep -qE '^(slot|safe|testrun)' "$STUB_LOG"; then
        pass "a runner that does not build fails the run before any suite"
    else
        fail "build failure (rc=$RUN_RC): $(cat "$tree/stderr") / $(cat "$STUB_LOG")"
    fi
    rm -rf "$tree"
}

test_no_go_toolchain_fails_loudly() {
    local tree
    tree="$(mktemp -d)"
    make_tree "$tree"
    run_test_all "$tree" "/usr/bin:/bin"
    if [ "$RUN_RC" -eq 1 ] && grep -q 'ERROR: go is not on PATH' "$tree/stderr" && [ ! -s "$STUB_LOG" ]; then
        pass "a host without go fails loudly before anything runs"
    else
        fail "missing go (rc=$RUN_RC): $(cat "$tree/stderr")"
    fi
    rm -rf "$tree"
}

test_runs_testrun_inside_the_slot_and_the_git_net
test_builds_the_runner_from_its_own_tree
test_the_runners_exit_status_is_the_runs
test_a_failed_build_fails_before_anything_runs
test_no_go_toolchain_fails_loudly

echo "-----"
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ]
