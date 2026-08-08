#!/usr/bin/env bash
#
# Hermetic tests for test-all.sh.
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_SRC="$THIS_DIR/test-all.sh"
CSV_HEADER="run_id,recorded_at_utc,commit,branch,suite,duration_seconds"
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
    local tree="$1"
    mkdir -p "$tree/modules/app/agent-repl/bin" \
        "$tree/modules/app/agent-repl/scripts" \
        "$tree/.claude" "$tree/.githooks" "$tree/stubs"
    cp "$SCRIPT_SRC" "$tree/modules/app/agent-repl/bin/test-all.sh"
    printf '%s\n' "$CSV_HEADER" >"$tree/modules/app/agent-repl/test_time.csv"

    cat >"$tree/modules/app/agent-repl/bin/test-test-all.sh" <<'EOF'
#!/usr/bin/env bash
printf 'orchestrator-harness\n' >>"$STUB_LOG"
EOF

    cat >"$tree/modules/app/agent-repl/bin/test-report-nonlisp-coverage.sh" <<'EOF'
#!/usr/bin/env bash
printf 'coverage-harness\n' >>"$STUB_LOG"
EOF

    cat >"$tree/modules/app/agent-repl/bin/test-report-logging-density.sh" <<'EOF'
#!/usr/bin/env bash
printf 'logging-density-harness\n' >>"$STUB_LOG"
EOF

    cat >"$tree/modules/app/agent-repl/bin/test-build-frontend.sh" <<'EOF'
#!/usr/bin/env bash
printf 'build-frontend-harness\n' >>"$STUB_LOG"
EOF

    cat >"$tree/modules/app/agent-repl/bin/test-deploy-all.sh" <<'EOF'
#!/usr/bin/env bash
printf 'deploy-harness\n' >>"$STUB_LOG"
EOF

    cat >"$tree/modules/app/agent-repl/bin/test-readiness-report.sh" <<'EOF'
#!/usr/bin/env bash
printf 'readiness-harness\n' >>"$STUB_LOG"
EOF

    cat >"$tree/modules/app/agent-repl/scripts/test-agent-shim-doctor.sh" <<'EOF'
#!/usr/bin/env bash
printf 'doctor-harness\n' >>"$STUB_LOG"
EOF

    cat >"$tree/.githooks/test-pre-commit.sh" <<'EOF'
#!/usr/bin/env bash
printf 'precommit-harness\n' >>"$STUB_LOG"
EOF

    cat >"$tree/modules/app/agent-repl/bin/report-nonlisp-coverage.sh" <<'EOF'
#!/usr/bin/env bash
component="${1:?component required}"
printf '%s\n' "$component" >>"$STUB_LOG"
for spec in ${STUB_FAIL_SUITES:-}; do
    if [ "${spec%%:*}" = "$component" ]; then
        exit "${spec##*:}"
    fi
done
if [ "${STUB_SLOW_SUITE:-}" = "$component" ]; then
    sleep 1.1
fi
EOF

    cat >"$tree/modules/app/agent-repl/bin/report-logging-density.sh" <<'EOF'
#!/usr/bin/env bash
printf 'logging-density\n' >>"$STUB_LOG"
EOF

    cat >"$tree/.claude/safe-test-run.sh" <<'EOF'
#!/usr/bin/env bash
printf 'ert\n' >>"$STUB_LOG"
EOF

    cat >"$tree/stubs/git" <<'EOF'
#!/usr/bin/env bash
if [ "${GIT_STUB_FAIL:-0}" = "1" ]; then
    exit 1
fi
case "$*" in
    *"branch --show-current") printf '%s\n' "${GIT_STUB_BRANCH:-master}" ;;
    *"rev-parse HEAD") printf '%s\n' "${GIT_STUB_COMMIT:-0123456789abcdef0123456789abcdef01234567}" ;;
    *) printf 'unexpected git invocation: %s\n' "$*" >&2; exit 2 ;;
esac
EOF

    chmod +x \
        "$tree/modules/app/agent-repl/bin/test-all.sh" \
        "$tree/modules/app/agent-repl/bin/test-test-all.sh" \
        "$tree/modules/app/agent-repl/bin/test-report-nonlisp-coverage.sh" \
        "$tree/modules/app/agent-repl/bin/test-report-logging-density.sh" \
        "$tree/modules/app/agent-repl/bin/test-build-frontend.sh" \
        "$tree/modules/app/agent-repl/bin/test-deploy-all.sh" \
        "$tree/modules/app/agent-repl/bin/test-readiness-report.sh" \
        "$tree/modules/app/agent-repl/scripts/test-agent-shim-doctor.sh" \
        "$tree/.githooks/test-pre-commit.sh" \
        "$tree/modules/app/agent-repl/bin/report-nonlisp-coverage.sh" \
        "$tree/modules/app/agent-repl/bin/report-logging-density.sh" \
        "$tree/.claude/safe-test-run.sh" \
        "$tree/stubs/git"
}

run_test_all() {
    local tree="$1"
    shift
    STUB_LOG="$tree/stub.log"
    : >"$STUB_LOG"
    set +e
    PATH="$tree/stubs:/usr/bin:/bin" \
        STUB_LOG="$STUB_LOG" \
        STUB_FAIL_SUITES="${STUB_FAIL_SUITES:-}" \
        STUB_SLOW_SUITE="${STUB_SLOW_SUITE:-}" \
        GIT_STUB_FAIL="${GIT_STUB_FAIL:-0}" \
        GIT_STUB_BRANCH="${GIT_STUB_BRANCH:-master}" \
        GIT_STUB_COMMIT="${GIT_STUB_COMMIT:-0123456789abcdef0123456789abcdef01234567}" \
        "$tree/modules/app/agent-repl/bin/test-all.sh" "$@" \
        >"$tree/stdout" 2>"$tree/stderr"
    RUN_RC=$?
    set -e
}

test_default_runs_every_suite_without_recording() {
    local tree="$TMP/default"
    make_tree "$tree"
    run_test_all "$tree"

    if [ "$RUN_RC" -eq 0 ] &&
        [ "$(wc -l <"$tree/stub.log" | tr -d ' ')" -eq 18 ] &&
        [ "$(wc -l <"$tree/modules/app/agent-repl/test_time.csv" | tr -d ' ')" -eq 1 ] &&
        grep -q "timing: proto" "$tree/stdout" &&
        grep -q "timings were not recorded" "$tree/stdout"; then
        pass "default run executes every suite without mutating history"
    else
        fail "default run executes every suite without mutating history"
    fi
}

test_record_appends_every_suite() {
    local tree="$TMP/record"
    make_tree "$tree"
    run_test_all "$tree" --record

    if [ "$RUN_RC" -eq 0 ] &&
        [ "$(wc -l <"$tree/modules/app/agent-repl/test_time.csv" | tr -d ' ')" -eq 19 ] &&
        grep -q ',master,ert,' "$tree/modules/app/agent-repl/test_time.csv" &&
        grep -q ',master,proto,' "$tree/modules/app/agent-repl/test_time.csv" &&
        grep -q ',master,logging,' "$tree/modules/app/agent-repl/test_time.csv" &&
        grep -Eq ',master,daemon,[0-9]+([.][0-9]+)?$' \
            "$tree/modules/app/agent-repl/test_time.csv"; then
        pass "--record atomically appends one timing per suite"
    else
        fail "--record atomically appends one timing per suite"
    fi
}

test_failure_continues_and_summarizes_every_failure() {
    local tree="$TMP/failure-continues"
    make_tree "$tree"
    STUB_FAIL_SUITES="store:7 wire:9" run_test_all "$tree"

    if grep -q '^wire$' "$tree/stub.log" &&
        grep -q '^proto$' "$tree/stub.log" &&
        grep -q '^logging-density$' "$tree/stub.log" &&
        grep -q "store failed after .*with exit code 7" "$tree/stderr" &&
        grep -q "wire failed after .*with exit code 9" "$tree/stderr" &&
        grep -q "failure summary, 2 of 18 suites failed" "$tree/stderr" &&
        grep -q "failed: store exit code 7 after" "$tree/stderr" &&
        grep -q "failed: wire exit code 9 after" "$tree/stderr"; then
        pass "suite failures run every later suite and summarize each failure"
    else
        fail "suite failures run every later suite and summarize each failure"
    fi
}

test_failure_exits_non_zero_and_never_records() {
    local tree="$TMP/failure-exit"
    make_tree "$tree"
    STUB_FAIL_SUITES="store:7" run_test_all "$tree" --record

    if [ "$RUN_RC" -ne 0 ] &&
        [ "$(wc -l <"$tree/modules/app/agent-repl/test_time.csv" | tr -d ' ')" -eq 1 ] &&
        ! grep -q "all agent-repl tests and coverage suites passed" "$tree/stdout"; then
        pass "a failed suite exits non-zero and leaves history unchanged"
    else
        fail "a failed suite exits non-zero and leaves history unchanged"
    fi
}

test_all_pass_output_is_unchanged() {
    local tree="$TMP/all-pass"
    make_tree "$tree"
    run_test_all "$tree"

    if [ "$RUN_RC" -eq 0 ] &&
        [ ! -s "$tree/stderr" ] &&
        grep -q "timing summary, slowest suite first" "$tree/stdout" &&
        [ "$(tail -n 1 "$tree/stdout")" = \
            "[agent-repl-tests] all agent-repl tests and coverage suites passed" ]; then
        pass "the all-pass path keeps its summary output and clean stderr"
    else
        fail "the all-pass path keeps its summary output and clean stderr"
    fi
}

test_unknown_argument_fails_before_running_suites() {
    local tree="$TMP/unknown"
    make_tree "$tree"
    run_test_all "$tree" --mystery

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "unknown argument" "$tree/stderr" &&
        [ ! -s "$tree/stub.log" ]; then
        pass "unknown argument fails before running suites"
    else
        fail "unknown argument fails before running suites"
    fi
}

test_malformed_csv_fails_before_running_suites() {
    local tree="$TMP/malformed"
    make_tree "$tree"
    printf 'wrong,header\n' >"$tree/modules/app/agent-repl/test_time.csv"
    run_test_all "$tree" --record

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "canonical timing header is invalid" "$tree/stderr" &&
        [ ! -s "$tree/stub.log" ]; then
        pass "malformed canonical history fails before running suites"
    else
        fail "malformed canonical history fails before running suites"
    fi
}

test_git_failure_fails_before_running_suites() {
    local tree="$TMP/git-failure"
    make_tree "$tree"
    GIT_STUB_FAIL=1 run_test_all "$tree" --record

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "could not resolve the initial git branch" "$tree/stderr" &&
        [ ! -s "$tree/stub.log" ]; then
        pass "missing git metadata fails before running suites"
    else
        fail "missing git metadata fails before running suites"
    fi
}

test_big_regression_is_surfaced() {
    local tree="$TMP/regression" index
    make_tree "$tree"
    for index in 1 2 3; do
        printf 'old-%s,2026-07-2%sT00:00:00Z,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,master,daemon,0.010\n' \
            "$index" "$index" >>"$tree/modules/app/agent-repl/test_time.csv"
    done
    STUB_SLOW_SUITE=daemon run_test_all "$tree" --record

    if [ "$RUN_RC" -eq 0 ] &&
        grep -q "TIMING REGRESSION: daemon" "$tree/stdout"; then
        pass "large regression is surfaced against recent same-branch entries"
    else
        fail "large regression is surfaced against recent same-branch entries"
    fi
}

# --- --suites narrowing -----------------------------------------------------

test_suites_runs_only_the_named_suites() {
    local tree="$TMP/suites-subset"
    make_tree "$tree"
    run_test_all "$tree" --suites webapp,build-frontend-harness

    if [ "$RUN_RC" -eq 0 ] &&
        [ "$(wc -l <"$tree/stub.log" | tr -d ' ')" -eq 2 ] &&
        grep -q '^webapp$' "$tree/stub.log" &&
        grep -q '^build-frontend-harness$' "$tree/stub.log" &&
        ! grep -q '^daemon$' "$tree/stub.log" &&
        grep -q "daemon: not selected" "$tree/stdout" &&
        grep -q "selected agent-repl suites passed: webapp build-frontend-harness" "$tree/stdout"; then
        pass "--suites runs only the named suites"
    else
        fail "--suites runs only the named suites"
    fi
}

test_no_suites_argument_still_runs_everything() {
    local tree="$TMP/suites-absent"
    make_tree "$tree"
    run_test_all "$tree"

    if [ "$RUN_RC" -eq 0 ] &&
        [ "$(wc -l <"$tree/stub.log" | tr -d ' ')" -eq 18 ] &&
        ! grep -q "not selected" "$tree/stdout"; then
        pass "an absent --suites leaves the run at every suite"
    else
        fail "an absent --suites leaves the run at every suite"
    fi
}

# An unknown name must never be waved through: a caller that misspells a suite
# would otherwise be told the suite it named passed.
test_unknown_suite_fails_before_running_suites() {
    local tree="$TMP/suites-unknown"
    make_tree "$tree"
    run_test_all "$tree" --suites webapp,webbapp

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "unknown suite 'webbapp'" "$tree/stderr" &&
        [ ! -s "$tree/stub.log" ]; then
        pass "an unknown --suites name fails before running suites"
    else
        fail "an unknown --suites name fails before running suites"
    fi
}

test_empty_suites_list_fails_before_running_suites() {
    local tree="$TMP/suites-empty"
    make_tree "$tree"
    run_test_all "$tree" --suites ""

    if [ "$RUN_RC" -ne 0 ] &&
        grep -q "needs at least one suite name" "$tree/stderr" &&
        [ ! -s "$tree/stub.log" ]; then
        pass "an empty --suites list fails before running suites"
    else
        fail "an empty --suites list fails before running suites"
    fi
}

test_suites_records_only_the_suites_it_ran() {
    local tree="$TMP/suites-record"
    make_tree "$tree"
    run_test_all "$tree" --record --suites daemon,proto

    if [ "$RUN_RC" -eq 0 ] &&
        [ "$(wc -l <"$tree/modules/app/agent-repl/test_time.csv" | tr -d ' ')" -eq 3 ] &&
        grep -q ',master,daemon,' "$tree/modules/app/agent-repl/test_time.csv" &&
        grep -q ',master,proto,' "$tree/modules/app/agent-repl/test_time.csv" &&
        ! grep -q ',master,ert,' "$tree/modules/app/agent-repl/test_time.csv"; then
        pass "--record with --suites records only the suites that ran"
    else
        fail "--record with --suites records only the suites that ran"
    fi
}

# The roster and the run block are two lists that can disagree, and a name in
# one but not the other is exactly the drift that makes --suites lie: an
# undeclared name is rejected as unknown, a declared name nothing runs reports a
# green run of nothing.
test_roster_matches_the_run_block() {
    local declared invoked
    declared="$(sed -n '/^ALL_SUITES=(/,/^)/p' "$SCRIPT_SRC" |
        sed -e '1d' -e '$d' -e 's/[[:space:]]//g' | sort)"
    invoked="$(grep -E '^run_timed ' "$SCRIPT_SRC" | awk '{print $2}' |
        sed 's/^"//; s/"$//' | sort -u)"
    # The per-component loop names its suites through a variable, so its list is
    # read from the loop header rather than from the run_timed line.
    invoked="$(printf '%s\n%s\n' "$invoked" \
        "$(sed -n 's/^for component in \(.*\); do$/\1/p' "$SCRIPT_SRC" | tr ' ' '\n')" |
        grep -v '^\$' | grep -v '^$' | sort -u)"

    if [ "$declared" = "$invoked" ]; then
        pass "ALL_SUITES declares exactly the suites the run block invokes"
    else
        fail "ALL_SUITES declares exactly the suites the run block invokes"
        printf '  declared:\n%s\n  invoked:\n%s\n' "$declared" "$invoked" >&2
    fi
}

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-test-all-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

test_default_runs_every_suite_without_recording
test_record_appends_every_suite
test_failure_continues_and_summarizes_every_failure
test_failure_exits_non_zero_and_never_records
test_all_pass_output_is_unchanged
test_unknown_argument_fails_before_running_suites
test_malformed_csv_fails_before_running_suites
test_git_failure_fails_before_running_suites
test_big_regression_is_surfaced
test_suites_runs_only_the_named_suites
test_no_suites_argument_still_runs_everything
test_unknown_suite_fails_before_running_suites
test_empty_suites_list_fails_before_running_suites
test_suites_records_only_the_suites_it_ran
test_roster_matches_the_run_block

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
