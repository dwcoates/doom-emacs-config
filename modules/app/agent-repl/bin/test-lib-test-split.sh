#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# test-lib-test-split.sh — hermetic tests for lib-test-split.sh, the --list /
# --only protocol every splittable shell harness shares.
#
# Each case runs a fixture harness (written here, sourcing the library under
# test) with no fixtures of its own, so the subject is only the protocol: what
# --list prints, what --only accepts and runs, and the TESTRUN-ITEM line each
# group run prints for the test runner.
#
# Run with:   bash bin/test-lib-test-split.sh

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LIB_UNDER_TEST="$THIS_DIR/lib-test-split.sh"

PASS=0
FAIL=0
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -z "${2:-}" ] || echo "       $2"; }

# The fixture harness: three groups, each recording that it ran.
HARNESS="$TMP/harness.sh"
cat >"$HARNESS" <<EOF
#!/usr/bin/env bash
set -euo pipefail
. "$LIB_UNDER_TEST"
test_split_init "\${BASH_SOURCE[0]}" "\$@"
group() { echo "ran \$1"; }
test_split_run alpha group alpha
test_split_run beta group beta
    test_split_run gamma group gamma
EOF
chmod +x "$HARNESS"

# run ARGS... — the fixture's stdout in OUT, stderr in ERR, status in RC.
run() {
    RC=0
    "$HARNESS" "$@" >"$TMP/out" 2>"$TMP/err" || RC=$?
    OUT="$(cat "$TMP/out")"
    ERR="$(cat "$TMP/err")"
}

t_list_prints_every_group_in_order_and_runs_none() {
    run --list
    if [ "$RC" -eq 0 ] && [ "$OUT" = $'alpha\nbeta\ngamma' ]; then
        pass "--list prints every group, indented calls included, and runs none"
    else
        fail "--list prints every group, indented calls included, and runs none" "rc=$RC out=$OUT err=$ERR"
    fi
}

t_no_argument_runs_every_group() {
    run
    if [ "$RC" -eq 0 ] && [ "$(grep -c '^ran ' <<<"$OUT")" -eq 3 ] \
        && [ "$(grep -c '^TESTRUN-ITEM ' <<<"$OUT")" -eq 3 ]; then
        pass "no argument runs every group and times each"
    else
        fail "no argument runs every group and times each" "rc=$RC out=$OUT err=$ERR"
    fi
}

t_only_runs_exactly_the_named_groups() {
    run --only gamma,alpha
    if [ "$RC" -eq 0 ] && [ "$(grep '^ran ' <<<"$OUT")" = $'ran alpha\nran gamma' ]; then
        pass "--only runs exactly the named groups"
    else
        fail "--only runs exactly the named groups" "rc=$RC out=$OUT err=$ERR"
    fi
}

t_item_line_is_the_runners_shape() {
    run --only beta
    if [ "$RC" -eq 0 ] && [[ "$(grep '^TESTRUN-ITEM' <<<"$OUT")" =~ ^TESTRUN-ITEM\ beta\ [0-9]+\.[0-9]{6}$ ]]; then
        pass "a group prints TESTRUN-ITEM <group> <seconds.micros>"
    else
        fail "a group prints TESTRUN-ITEM <group> <seconds.micros>" "rc=$RC out=$OUT err=$ERR"
    fi
}

t_only_refuses_an_unknown_group() {
    run --only alpha,delta
    if [ "$RC" -eq 2 ] && [[ "$ERR" == *"unknown harness item: delta"* ]] && [ -z "$OUT" ]; then
        pass "--only refuses an unknown group before running anything"
    else
        fail "--only refuses an unknown group before running anything" "rc=$RC out=$OUT err=$ERR"
    fi
}

t_an_unknown_group_names_what_the_script_listed() {
    local sum
    sum="$(cksum <"$HARNESS")"
    run --only delta
    if [ "$RC" -eq 2 ] && [[ "$ERR" == *"$HARNESS lists: alpha,beta,gamma; read as cksum $sum"* ]]; then
        pass "an unknown group's refusal names the groups the script listed and the bytes it read"
    else
        fail "an unknown group's refusal names the groups the script listed and the bytes it read" "rc=$RC out=$OUT err=$ERR"
    fi
}

t_only_refuses_an_empty_list() {
    run --only ""
    if [ "$RC" -eq 2 ] && [[ "$ERR" == *"--only needs a nonempty test list"* ]]; then
        pass "--only refuses an empty list"
    else
        fail "--only refuses an empty list" "rc=$RC out=$OUT err=$ERR"
    fi
}

t_only_refuses_a_missing_list() {
    run --only
    if [ "$RC" -eq 2 ] && [[ "$ERR" == *"--only needs one comma-separated test list"* ]]; then
        pass "--only refuses a missing list"
    else
        fail "--only refuses a missing list" "rc=$RC out=$OUT err=$ERR"
    fi
}

t_unknown_argument_is_refused() {
    run --bogus
    if [ "$RC" -eq 2 ] && [[ "$ERR" == *"unknown harness argument: --bogus"* ]]; then
        pass "an unknown argument is refused"
    else
        fail "an unknown argument is refused" "rc=$RC out=$OUT err=$ERR"
    fi
}

t_a_harness_with_no_groups_is_refused() {
    local empty="$TMP/empty.sh" rc=0 err
    printf '#!/usr/bin/env bash\n. "%s"\ntest_split_init "${BASH_SOURCE[0]}" "$@"\n' "$LIB_UNDER_TEST" >"$empty"
    chmod +x "$empty"
    err="$("$empty" --list 2>&1 >/dev/null)" || rc=$?
    if [ "$rc" -eq 2 ] && [[ "$err" == *"no test_split_run items"* ]]; then
        pass "a harness with no groups is refused"
    else
        fail "a harness with no groups is refused" "rc=$rc err=$err"
    fi
}

t_a_failing_group_fails_the_harness() {
    local failing="$TMP/failing.sh" rc=0
    printf '#!/usr/bin/env bash\nset -euo pipefail\n. "%s"\ntest_split_init "${BASH_SOURCE[0]}" "$@"\nboom() { return 1; }\ntest_split_run broken boom\n' "$LIB_UNDER_TEST" >"$failing"
    chmod +x "$failing"
    "$failing" >"$TMP/out" 2>&1 || rc=$?
    if [ "$rc" -ne 0 ] && ! grep -q '^TESTRUN-ITEM' "$TMP/out"; then
        pass "a failing group fails the harness and reports no timing"
    else
        fail "a failing group fails the harness and reports no timing" "rc=$rc out=$(cat "$TMP/out")"
    fi
}

t_a_bash_without_epochrealtime_is_refused() {
    # bash before 5 has no EPOCHREALTIME; unsetting it reproduces that shell.
    local old="$TMP/old-bash.sh" rc=0 err
    printf '#!/usr/bin/env bash\nunset EPOCHREALTIME\n. "%s"\ntest_split_init "${BASH_SOURCE[0]}" "$@"\ntest_split_run alpha true\n' "$LIB_UNDER_TEST" >"$old"
    chmod +x "$old"
    err="$("$old" 2>&1 >/dev/null)" || rc=$?
    if [ "$rc" -eq 2 ] && [[ "$err" == *"needs bash 5 or later (EPOCHREALTIME)"* ]]; then
        pass "a bash without EPOCHREALTIME is refused rather than timing every group as zero"
    else
        fail "a bash without EPOCHREALTIME is refused rather than timing every group as zero" "rc=$rc err=$err"
    fi
}

t_list_prints_every_group_in_order_and_runs_none
t_no_argument_runs_every_group
t_only_runs_exactly_the_named_groups
t_item_line_is_the_runners_shape
t_only_refuses_an_unknown_group
t_an_unknown_group_names_what_the_script_listed
t_only_refuses_an_empty_list
t_only_refuses_a_missing_list
t_unknown_argument_is_refused
t_a_harness_with_no_groups_is_refused
t_a_failing_group_fails_the_harness
t_a_bash_without_epochrealtime_is_refused

echo "-----"
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ]
