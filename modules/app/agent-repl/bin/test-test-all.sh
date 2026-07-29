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
    mkdir -p "$tree/modules/app/agent-repl/bin" "$tree/.claude" "$tree/.githooks" "$tree/stubs"
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

    cat >"$tree/.githooks/test-pre-commit.sh" <<'EOF'
#!/usr/bin/env bash
printf 'precommit-harness\n' >>"$STUB_LOG"
EOF

    cat >"$tree/modules/app/agent-repl/bin/report-nonlisp-coverage.sh" <<'EOF'
#!/usr/bin/env bash
component="${1:?component required}"
printf '%s\n' "$component" >>"$STUB_LOG"
if [ "${STUB_FAIL_SUITE:-}" = "$component" ]; then
    exit 7
fi
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
        STUB_FAIL_SUITE="${STUB_FAIL_SUITE:-}" \
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
        [ "$(wc -l <"$tree/stub.log" | tr -d ' ')" -eq 16 ] &&
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
        [ "$(wc -l <"$tree/modules/app/agent-repl/test_time.csv" | tr -d ' ')" -eq 17 ] &&
        grep -q ',master,ert,' "$tree/modules/app/agent-repl/test_time.csv" &&
        grep -q ',master,proto,' "$tree/modules/app/agent-repl/test_time.csv" &&
        grep -Eq ',master,daemon,[0-9]+([.][0-9]+)?$' \
            "$tree/modules/app/agent-repl/test_time.csv"; then
        pass "--record atomically appends one timing per suite"
    else
        fail "--record atomically appends one timing per suite"
    fi
}

test_failure_stops_and_never_records() {
    local tree="$TMP/failure"
    make_tree "$tree"
    STUB_FAIL_SUITE=store run_test_all "$tree" --record

    if [ "$RUN_RC" -ne 0 ] &&
        [ "$(wc -l <"$tree/modules/app/agent-repl/test_time.csv" | tr -d ' ')" -eq 1 ] &&
        grep -q "store failed" "$tree/stderr" &&
        ! grep -q '^wire$' "$tree/stub.log"; then
        pass "suite failure aborts before later suites and leaves history unchanged"
    else
        fail "suite failure aborts before later suites and leaves history unchanged"
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

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-test-all-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

test_default_runs_every_suite_without_recording
test_record_appends_every_suite
test_failure_stops_and_never_records
test_unknown_argument_fails_before_running_suites
test_malformed_csv_fails_before_running_suites
test_git_failure_fails_before_running_suites
test_big_regression_is_surfaced

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
