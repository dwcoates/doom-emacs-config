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
REAL_SAFE_TEST_RUN="$(cd "$THIS_DIR/../../../.." && pwd)/.claude/safe-test-run.sh"
CSVHeader="run_id,recorded_at_utc,commit,branch,suite,duration_seconds,measure"
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

# ---- Integration: the real git-state net around a --record run -----------
#
# The defect this guards: `testrun run --record` used to append its own row
# to test_time.csv from INSIDE .claude/safe-test-run.sh's net, so the net's
# post-run snapshot saw a tracked file it hadn't seen before the run --
# DRIFT DETECTED, a checkpoint tag preserved, exit 2, forever, for a run that
# did exactly what --record is documented to do.
#
# These cases run the REAL safe-test-run.sh (never stubbed) against a fake
# `git` (same shape as .claude/test-safe-test-run.sh's), plus a stub testrun
# that models the staging/finishing split precisely enough to prove the
# timing: its `run` subcommand writes ONLY to --record-out (a path outside
# the checkout), and can simulate a suite failure or a HEAD moving mid-run;
# its `finish-record` subcommand is the only thing that ever appends to the
# module's test_time.csv, and test-all.sh never calls it from inside "safe
# --". NO REAL GIT RUNS, and nothing here touches the real test_time.csv.

make_real_net_tree() {
    local tree="$1"
    local repo="$tree/repo"
    local module="$repo/modules/app/agent-repl"
    mkdir -p "$module/bin" "$module/testrun" "$module/lisp" "$repo/.claude" "$tree/stubs" "$tree/gitstate"
    cp "$SCRIPT_SRC" "$module/bin/test-all.sh"
    cp "$REAL_SAFE_TEST_RUN" "$repo/.claude/safe-test-run.sh"
    # safe-test-run.sh checks this file exists even in `--` command mode.
    : >"$module/lisp/test-agent-repl.el"
    printf '%s\n' "$CSVHeader" >"$module/test_time.csv"

    cat >"$module/bin/suite-slot.sh" <<'EOF'
#!/usr/bin/env bash
exec "$@"
EOF

    # fake git answers exactly the plumbing commands safe-test-run.sh issues,
    # from state files a case controls (same shape as
    # .claude/test-safe-test-run.sh's fake git).
    cat >"$tree/stubs/git" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
S="$FAKE_GIT_STATE"
case "$*" in
    "rev-parse --show-toplevel") printf '%s\n' "$FAKE_GIT_TOPLEVEL" ;;
    "rev-parse HEAD") cat "$S/head" ;;
    "rev-parse --abbrev-ref HEAD") cat "$S/branch" ;;
    "worktree list --porcelain") printf 'worktree %s\nbranch refs/heads/%s\n' "$FAKE_GIT_TOPLEVEL" "$(cat "$S/branch")" ;;
    "for-each-ref --format=%(refname) %(objectname) refs/heads/ refs/tags/") cat "$S/refs" ;;
    "status --porcelain") cat "$S/status" ;;
    "tag -d "*) grep -v "^refs/tags/$3 " "$S/refs" >"$S/refs.new" || true; mv "$S/refs.new" "$S/refs" ;;
    "tag "*) printf 'refs/tags/%s %s\n' "$2" "$3" >>"$S/refs" ;;
    *) echo "fake git: unexpected command: git $*" >&2; exit 99 ;;
esac
EOF

    # The stub testrun: `run` stages to --record-out only (and can simulate a
    # suite failure or HEAD moving); `finish-record` is the only thing that
    # appends to the real test_time.csv.
    cat >"$tree/stubs/go" <<'EOF'
#!/usr/bin/env bash
[ "$1" = build ] && [ "$2" = -o ] || exit 98
cat >"$3" <<'INNER'
#!/usr/bin/env bash
set -euo pipefail
printf 'testrun %s\n' "$*" >>"$STUB_LOG"
sub="$1"; shift
RECORD_OUT="" MODULE=""
prev=""
for a in "$@"; do
    [ "$prev" = "--record-out" ] && RECORD_OUT="$a"
    [ "$prev" = "--module" ] && MODULE="$a"
    prev="$a"
done
case "$sub" in
    run)
        if [ "${STUB_MOVE_HEAD:-0}" = 1 ]; then
            echo 2222222222222222222222222222222222222222 >"$FAKE_GIT_STATE/head"
        fi
        [ "${STUB_RUN_EXIT:-0}" != 0 ] && exit "$STUB_RUN_EXIT"
        [ -n "$RECORD_OUT" ] && printf '{"RunID":"run1","Branch":"main","Commit":"1111111111111111111111111111111111111111","Rows":[{"RunID":"run1","RecordedAt":"t","Commit":"1111111111111111111111111111111111111111","Branch":"main","Suite":"ert","Seconds":1.000,"Measure":"unit-wall-sum"}]}' >"$RECORD_OUT"
        exit 0
        ;;
    finish-record)
        printf 'run1,t,1111111111111111111111111111111111111111,main,ert,1.000,unit-wall-sum\n' >>"$MODULE/test_time.csv"
        exit 0
        ;;
esac
INNER
chmod +x "$3"
EOF
    chmod +x "$module/bin/test-all.sh" "$module/bin/suite-slot.sh" \
        "$repo/.claude/safe-test-run.sh" "$tree/stubs/go" "$tree/stubs/git"
}

# run_test_all_real_net runs test-all.sh through the REAL safe-test-run.sh
# and the fake git above; GIT state (head/branch/refs/status) and RC/OUT are
# in STATE/RUN_RC/RUN_OUT.
run_test_all_real_net() {
    local tree="$1"
    shift
    STATE="$tree/gitstate"
    echo 1111111111111111111111111111111111111111 >"$STATE/head"
    echo main >"$STATE/branch"
    echo "refs/heads/main 1111111111111111111111111111111111111111" >"$STATE/refs"
    : >"$STATE/status"
    STUB_LOG="$tree/stub.log"
    : >"$STUB_LOG"
    set +e
    RUN_OUT="$(cd "$tree/repo" && PATH="$tree/stubs:/usr/bin:/bin" \
        FAKE_GIT_STATE="$STATE" FAKE_GIT_TOPLEVEL="$tree/repo" STUB_LOG="$STUB_LOG" \
        STUB_RUN_EXIT="${STUB_RUN_EXIT:-0}" STUB_MOVE_HEAD="${STUB_MOVE_HEAD:-0}" \
        bash "$tree/repo/modules/app/agent-repl/bin/test-all.sh" "$@" 2>&1)"
    RUN_RC=$?
    set -e
}

test_integration_clean_record_run_exits_0_with_no_tag() {
    local tree
    tree="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
    make_real_net_tree "$tree"
    run_test_all_real_net "$tree" --record
    if [ "$RUN_RC" -eq 0 ] \
        && [ "$(cat "$tree/repo/modules/app/agent-repl/test_time.csv")" = "$(printf '%s\nrun1,t,1111111111111111111111111111111111111111,main,ert,1.000,unit-wall-sum' "$CSVHeader")" ] \
        && [ -z "$(grep '^refs/tags/agent-repl-test-checkpoint-' "$STATE/refs" || true)" ]; then
        pass "a clean --record run exits 0, records the row, and leaves no checkpoint tag"
    else
        fail "a clean --record run exits 0, records the row, and leaves no checkpoint tag" \
            "rc=$RUN_RC refs=$(cat "$STATE/refs") csv=$(cat "$tree/repo/modules/app/agent-repl/test_time.csv") out=$RUN_OUT"
    fi
    rm -rf "$tree"
}

test_integration_real_drift_during_record_run_is_still_caught() {
    local tree
    tree="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
    make_real_net_tree "$tree"
    STUB_MOVE_HEAD=1 run_test_all_real_net "$tree" --record
    if [ "$RUN_RC" -eq 2 ] \
        && [[ "$RUN_OUT" == *"DRIFT DETECTED"* ]] \
        && [[ "$RUN_OUT" == *"HEAD changed"* ]] \
        && [ -n "$(grep '^refs/tags/agent-repl-test-checkpoint-' "$STATE/refs" || true)" ] \
        && [ "$(cat "$tree/repo/modules/app/agent-repl/test_time.csv")" = "$CSVHeader" ]; then
        pass "a HEAD moved by something other than the record write is still caught, and nothing is recorded"
    else
        fail "real drift during a --record run is still caught" \
            "rc=$RUN_RC refs=$(cat "$STATE/refs") csv=$(cat "$tree/repo/modules/app/agent-repl/test_time.csv") out=$RUN_OUT"
    fi
    rm -rf "$tree"
}

test_integration_failed_suite_records_nothing() {
    local tree
    tree="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
    make_real_net_tree "$tree"
    STUB_RUN_EXIT=1 run_test_all_real_net "$tree" --record
    if [ "$RUN_RC" -eq 1 ] \
        && ! grep -q '^testrun finish-record ' "$STUB_LOG" \
        && [ "$(cat "$tree/repo/modules/app/agent-repl/test_time.csv")" = "$CSVHeader" ] \
        && [ -z "$(grep '^refs/tags/agent-repl-test-checkpoint-' "$STATE/refs" || true)" ]; then
        pass "a failed suite under the real net records nothing and leaves no checkpoint"
    else
        fail "a failed suite under the real net records nothing" \
            "rc=$RUN_RC csv=$(cat "$tree/repo/modules/app/agent-repl/test_time.csv") out=$RUN_OUT"
    fi
    rm -rf "$tree"
}

test_integration_moved_commit_during_record_run_records_nothing() {
    local tree
    tree="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
    make_real_net_tree "$tree"
    # The commit moves mid-run (simulated by the stub testrun moving the fake
    # git HEAD): the net's own pre/post HEAD comparison catches it, exactly
    # as it would catch any other HEAD movement, and finish-record never runs.
    STUB_MOVE_HEAD=1 run_test_all_real_net "$tree" --record
    if [ "$RUN_RC" -eq 2 ] \
        && ! grep -q '^testrun finish-record ' "$STUB_LOG" \
        && [ "$(cat "$tree/repo/modules/app/agent-repl/test_time.csv")" = "$CSVHeader" ]; then
        pass "a commit that moves during a --record run records nothing"
    else
        fail "a commit that moves during a --record run records nothing" \
            "rc=$RUN_RC csv=$(cat "$tree/repo/modules/app/agent-repl/test_time.csv") out=$RUN_OUT"
    fi
    rm -rf "$tree"
}

test_runs_testrun_inside_the_slot_and_the_git_net() {
    local tree
    tree="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
    make_tree "$tree"
    run_test_all "$tree" "$(stub_path "$tree")" --suites ert,daemon
    local module want
    module="$(cd "$tree/modules/app/agent-repl" && pwd)"
    want="$(printf 'slot\nsafe --\ntestrun run --module %s --suites ert,daemon' "$module")"
    if [ "$RUN_RC" -eq 0 ] && [ "$(grep -v '^go ' "$STUB_LOG")" = "$want" ]; then
        pass "testrun runs under the suite slot, then the git net, with every argument"
    else
        fail "wrapper order or arguments (rc=$RUN_RC): $(cat "$STUB_LOG")"
    fi
    rm -rf "$tree"
}

test_record_stages_inside_the_net_then_finishes_outside_it() {
    local tree
    tree="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
    make_tree "$tree"
    run_test_all "$tree" "$(stub_path "$tree")" --suites ert,daemon --record
    local module
    module="$(cd "$tree/modules/app/agent-repl" && pwd)"
    # finish-record is a SEPARATE testrun invocation, outside "safe --": the
    # whole point of the split is that committing this run's own history
    # never happens from inside the git-state net. Both calls share one
    # --record-out path.
    local run_line finish_line record_out
    run_line="$(grep '^testrun run ' "$STUB_LOG")"
    finish_line="$(grep '^testrun finish-record ' "$STUB_LOG")"
    record_out="$(printf '%s' "$run_line" | sed -n 's/.*--record-out \(.*\)$/\1/p')"
    if [ "$RUN_RC" -eq 0 ] \
        && [ "$run_line" = "testrun run --module $module --suites ert,daemon --record --record-out $record_out" ] \
        && [ "$finish_line" = "testrun finish-record --module $module --record-out $record_out" ] \
        && [[ "$(grep -c '^safe --$' "$STUB_LOG")" -eq 1 ]]; then
        pass "a --record run stages inside the net and finishes in a separate call outside it"
    else
        fail "record staging/finishing split (rc=$RUN_RC)" "$(cat "$STUB_LOG")"
    fi
    rm -rf "$tree"
}

test_a_failed_record_run_never_finishes() {
    local tree
    tree="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
    make_tree "$tree"
    STUB_TESTRUN_EXIT=1 run_test_all "$tree" "$(stub_path "$tree")" --record
    if [ "$RUN_RC" -eq 1 ] && ! grep -q '^testrun finish-record ' "$STUB_LOG"; then
        pass "a failed --record run never calls finish-record"
    else
        fail "a failed --record run never calls finish-record (rc=$RUN_RC)" "$(cat "$STUB_LOG")"
    fi
    rm -rf "$tree"
}

test_builds_the_runner_from_its_own_tree() {
    local tree
    tree="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
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
    tree="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
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
    tree="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
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
    tree="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
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
test_record_stages_inside_the_net_then_finishes_outside_it
test_a_failed_record_run_never_finishes
test_builds_the_runner_from_its_own_tree
test_the_runners_exit_status_is_the_runs
test_a_failed_build_fails_before_anything_runs
test_no_go_toolchain_fails_loudly
test_integration_clean_record_run_exits_0_with_no_tag
test_integration_real_drift_during_record_run_is_still_caught
test_integration_failed_suite_records_nothing
test_integration_moved_commit_during_record_run_records_nothing

echo "-----"
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ]
