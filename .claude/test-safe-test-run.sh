#!/usr/bin/env bash

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

# test-safe-test-run.sh — hermetic tests for safe-test-run.sh's `--` mode, the
# git-state net bin/test-all.sh runs its whole parallel run inside.
#
# NO REAL GIT RUNS. Each case is a bin/fake-git.sh repository with the REAL
# .githooks/reference-transaction installed, so the net's ref journal is the
# hook's own. A case "drifts" the checkout from inside the wrapped command,
# exactly when a real unit would have touched the repository; a FOREIGN
# session (another workspace's git, which neither carries the run's marker nor
# descends from the net) is a process this harness starts before the net, and
# the wrapped command hands it the turn and waits for it through FIFOs.

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/safe-test-run.sh"
FAKE_GIT="$THIS_DIR/../modules/app/agent-repl/bin/fake-git.sh"

# This harness runs inside git hooks, agent sessions and other nets: no
# caller binding may reach a fixture.
# shellcheck source=/dev/null
. "$THIS_DIR/../modules/app/agent-repl/bin/lib-fake-git-fixture.sh"
clear_live_git_bindings
unset AGENT_REPL_SAFE_TEST_RUN

PASS=0
FAIL=0
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -z "${2:-}" ] || echo "       $2"; }

# The only `git` the net, the hook, the cases and the foreign sessions reach.
mkdir -p "$TMP/bin"
"$FAKE_GIT" --install "$TMP/bin/git"
export PATH="$TMP/bin:$PATH"

# new_case — a fixture checkout with one commit and the hook installed; its top
# in REPO, its HEAD in HEAD0, its git directory in G.
new_case() {
    REPO="$(cd "$(mktemp -d "$TMP/case.XXXXXX")" && pwd)"
    G="$REPO/.fakegit"
    git -C "$REPO" init
    mkdir -p "$REPO/modules/app/agent-repl/lisp"
    : >"$REPO/modules/app/agent-repl/lisp/test-agent-repl.el"
    git -C "$REPO" add -A
    git -C "$REPO" commit -m base
    install_checked_in_hook "$REPO" reference-transaction
    HEAD0="$(git -C "$REPO" rev-parse HEAD)"
}

# run_net ARGS... — the net from inside the fixture; output in OUT, status in RC.
run_net() {
    RC=0
    OUT="$(cd "$REPO" && bash "$SCRIPT_UNDER_TEST" "$@" 2>&1)" || RC=$?
}

refs() { cat "$G/refs" 2>/dev/null || true; }
checkpoints() { refs | grep '^refs/tags/agent-repl-test-checkpoint-' || true; }
registrations() { ls -A "$G/agent-repl-safe-test-runs" 2>/dev/null || true; }

# foreign_session CMD... — another session's git, waiting for its turn: the
# wrapped command FOREIGN_TURN writes $TMP/go, and blocks until CMD has run.
foreign_session() {
    rm -f "$TMP/go" "$TMP/done"
    mkfifo "$TMP/go" "$TMP/done"
    (read -r _ <"$TMP/go"; "$@"; echo finished >"$TMP/done") &
    FOREIGN=$!
}
FOREIGN_TURN=(sh -c 'echo go >"$0"; read -r _ <"$1"' "$TMP/go" "$TMP/done")

t_dashdash_without_a_command_is_refused() {
    new_case
    run_net --
    if [ "$RC" -eq 3 ] && [[ "$OUT" == *"-- needs a command to wrap"* ]] && [ -z "$(checkpoints)" ] && [ -z "$(registrations)" ]; then
        pass "-- without a command is refused before any registration or checkpoint"
    else
        fail "-- without a command is refused before any registration or checkpoint" "rc=$RC out=$OUT"
    fi
}

t_clean_wrapped_run_passes_and_removes_its_checkpoint() {
    new_case
    run_net -- true
    if [ "$RC" -eq 0 ] && [[ "$OUT" == *"zero git-state drift"* ]] && [ -z "$(refs)" ]; then
        pass "a clean wrapped run exits 0 and leaves no checkpoint and no canary"
    else
        fail "a clean wrapped run exits 0 and leaves no checkpoint and no canary" "rc=$RC refs=$(refs) out=$OUT"
    fi
}

t_a_run_unregisters_itself() {
    new_case
    run_net -- sh -c 'git update-ref refs/heads/x HEAD'
    if [ "$RC" -eq 2 ] && [ -d "$G/agent-repl-safe-test-runs" ] && [ -z "$(registrations)" ]; then
        pass "a run's registration is gone once it ends, drift or not"
    else
        fail "a run's registration is gone once it ends, drift or not" "rc=$RC left=$(registrations)"
    fi
}

t_the_wrapped_command_carries_the_run_marker() {
    new_case
    run_net -- sh -c 'printf "%s" "$AGENT_REPL_SAFE_TEST_RUN" >"$0"; ls "$1" >"$2"' "$TMP/marker" "$G/agent-repl-safe-test-runs" "$TMP/registered"
    if [ "$RC" -eq 0 ] && [ -n "$(cat "$TMP/marker")" ] && [ "$(cat "$TMP/marker")" = "$(cat "$TMP/registered")" ] \
        && [[ "$(cat "$TMP/marker")" == *-[0-9]* ]]; then
        pass "the wrapped command carries the marker naming its run's registration, which ends in a pid"
    else
        fail "the wrapped command carries the marker naming its run's registration, which ends in a pid" "marker=$(cat "$TMP/marker") registered=$(cat "$TMP/registered")"
    fi
}

t_wrapped_exit_status_passes_through() {
    new_case
    run_net -- sh -c 'exit 5'
    if [ "$RC" -eq 5 ] && [[ "$OUT" == *"[safe-test-run] sh exited with code 5"* ]] \
        && [[ "$OUT" == *"(The command itself failed; see its output above.)"* ]]; then
        pass "a failing wrapped command's status passes through, named by its program"
    else
        fail "a failing wrapped command's status passes through, named by its program" "rc=$RC out=$OUT"
    fi
}

t_wrapped_arguments_arrive_intact() {
    new_case
    run_net -- sh -c 'printf "%s|" "$@" >"$0"' "$TMP/args" "a b" c
    if [ "$RC" -eq 0 ] && [ "$(cat "$TMP/args")" = "a b|c|" ]; then
        pass "the wrapped command's arguments arrive intact"
    else
        fail "the wrapped command's arguments arrive intact" "rc=$RC out=$OUT args=$(cat "$TMP/args" 2>/dev/null)"
    fi
}

t_head_moved_by_the_command_is_drift() {
    new_case
    run_net -- sh -c 'echo y >moved && git add -A && git commit -m moved'
    if [ "$RC" -eq 2 ] && [[ "$OUT" == *"HEAD changed: $HEAD0 -> "* ]] && [ -n "$(checkpoints)" ]; then
        pass "a HEAD the command moved is drift, and the checkpoint stays"
    else
        fail "a HEAD the command moved is drift, and the checkpoint stays" "rc=$RC out=$OUT"
    fi
}

t_status_dirtied_by_the_command_is_drift() {
    new_case
    run_net -- sh -c 'echo dirty >"$0"' "$REPO/modules/app/agent-repl/lisp/test-agent-repl.el"
    if [ "$RC" -eq 2 ] && [[ "$OUT" == *"Working tree status changed"* ]]; then
        pass "a working tree the command dirtied is drift"
    else
        fail "a working tree the command dirtied is drift" "rc=$RC out=$OUT"
    fi
}

t_failure_outranks_drift() {
    new_case
    run_net -- sh -c 'echo dirty >"$0"; exit 7' "$REPO/modules/app/agent-repl/lisp/test-agent-repl.el"
    if [ "$RC" -eq 7 ] && [[ "$OUT" == *"DRIFT DETECTED"* ]]; then
        pass "a failing command that also drifted exits with its own status and reports the drift"
    else
        fail "a failing command that also drifted exits with its own status and reports the drift" "rc=$RC out=$OUT"
    fi
}

t_a_ref_the_run_creates_is_drift() {
    new_case
    run_net -- git update-ref refs/heads/leaked "$HEAD0"
    if [ "$RC" -eq 2 ] && [[ "$OUT" == *"0000000000000000000000000000000000000000 $HEAD0 refs/heads/leaked pid="*" by=marker cwd=$REPO"* ]] \
        && [[ "$OUT" == *"git update-ref -d refs/heads/leaked"* ]] && [ -n "$(checkpoints)" ]; then
        pass "a branch the run's own git creates is drift, with the command that deletes it"
    else
        fail "a branch the run's own git creates is drift, with the command that deletes it" "rc=$RC out=$OUT"
    fi
}

t_a_ref_the_run_deletes_is_drift() {
    new_case
    git -C "$REPO" update-ref refs/heads/kept "$HEAD0"
    run_net -- git update-ref -d refs/heads/kept
    if [ "$RC" -eq 2 ] && [[ "$OUT" == *"$HEAD0 0000000000000000000000000000000000000000 refs/heads/kept"* ]] \
        && [[ "$OUT" == *"git update-ref refs/heads/kept $HEAD0"* ]]; then
        pass "a branch the run's own git deletes is drift, with the command that restores it"
    else
        fail "a branch the run's own git deletes is drift, with the command that restores it" "rc=$RC out=$OUT"
    fi
}

t_a_ref_moved_by_a_scrubbed_environment_is_drift() {
    # A unit that builds its git's environment from scratch drops the marker,
    # but its git still descends from the net.
    new_case
    run_net -- env -u AGENT_REPL_SAFE_TEST_RUN git tag leaked "$HEAD0"
    if [ "$RC" -eq 2 ] && [[ "$OUT" == *"refs/tags/leaked pid="*" by=ancestry "* ]]; then
        pass "a ref moved by the run's git without the marker is still drift, by ancestry"
    else
        fail "a ref moved by the run's git without the marker is still drift, by ancestry" "rc=$RC out=$OUT"
    fi
}

t_a_ref_the_run_sets_to_its_own_value_is_not_drift() {
    new_case
    git -C "$REPO" update-ref refs/heads/kept "$HEAD0"
    run_net -- git update-ref refs/heads/kept "$HEAD0"
    if [ "$RC" -eq 0 ] && [[ "$OUT" == *"zero git-state drift"* ]]; then
        pass "a ref the run's git sets to the value it already held is not drift"
    else
        fail "a ref the run's git sets to the value it already held is not drift" "rc=$RC out=$OUT"
    fi
}

# 2026-10-13, shape 1: a branch no worktree owned before the run was checked
# out by a sibling workspace, which committed on it mid-run.
t_a_branch_another_session_moves_mid_run_is_not_drift() {
    local next
    new_case
    git -C "$REPO" update-ref refs/heads/hidden-pages-pause-animations "$HEAD0"
    echo z >"$REPO/sibling-file" && git -C "$REPO" add -A && git -C "$REPO" commit -m sibling
    next="$(git -C "$REPO" rev-parse HEAD)"
    git -C "$REPO" revert --no-edit HEAD
    foreign_session git -C "$REPO" update-ref refs/heads/hidden-pages-pause-animations "$next"
    run_net -- "${FOREIGN_TURN[@]}"
    wait "$FOREIGN"
    if [ "$RC" -eq 0 ] && [[ "$OUT" == *"zero git-state drift"* ]] \
        && grep -q "^refs/heads/hidden-pages-pause-animations $next\$" "$G/refs"; then
        pass "a branch another session moves mid-run is not drift"
    else
        fail "a branch another session moves mid-run is not drift" "rc=$RC refs=$(refs) out=$OUT"
    fi
}

# 2026-10-13, shape 2: another session created a branch no worktree owns.
t_a_branch_another_session_creates_mid_run_is_not_drift() {
    new_case
    foreign_session git -C "$REPO" update-ref refs/heads/main-watch-latch "$HEAD0"
    run_net -- "${FOREIGN_TURN[@]}"
    wait "$FOREIGN"
    if [ "$RC" -eq 0 ] && [[ "$OUT" == *"zero git-state drift"* ]] \
        && grep -q "^refs/heads/main-watch-latch $HEAD0\$" "$G/refs"; then
        pass "a branch another session creates mid-run is not drift"
    else
        fail "a branch another session creates mid-run is not drift" "rc=$RC refs=$(refs) out=$OUT"
    fi
}

t_another_sessions_move_does_not_hide_the_runs_own() {
    new_case
    foreign_session git -C "$REPO" update-ref refs/heads/main-watch-latch "$HEAD0"
    run_net -- sh -c '"$@"; git update-ref refs/heads/leaked HEAD' sh "${FOREIGN_TURN[@]}"
    wait "$FOREIGN"
    if [ "$RC" -eq 2 ] && [[ "$OUT" == *"refs/heads/leaked"* ]] && [[ "$OUT" != *"main-watch-latch"* ]]; then
        pass "alongside another session's move, the run's own move is drift and only it is reported"
    else
        fail "alongside another session's move, the run's own move is drift and only it is reported" "rc=$RC out=$OUT"
    fi
}

t_a_missing_hook_is_an_environment_error() {
    new_case
    rm "$G/hooks/reference-transaction"
    run_net -- true
    if [ "$RC" -eq 3 ] && [[ "$OUT" == *"the reference-transaction hook did not journal this run's canary ref"* ]] \
        && [[ "$OUT" == *"reference-transaction-hook"* ]] && [ -z "$(refs)" ] && [ -z "$(registrations)" ]; then
        pass "with no hook journaling, the net refuses to run, leaving no canary, checkpoint or registration"
    else
        fail "with no hook journaling, the net refuses to run, leaving no canary, checkpoint or registration" "rc=$RC refs=$(refs) out=$OUT"
    fi
}

t_a_gone_runs_registration_is_pruned() {
    local gone
    new_case
    sh -c 'exit 0' &
    gone=$!
    wait "$gone"
    mkdir -p "$G/agent-repl-safe-test-runs/20261013-000000-$gone"
    run_net -- true
    if [ "$RC" -eq 0 ] && [[ "$OUT" == *"Removed the registration of a run that is gone: "*"20261013-000000-$gone (root pid $gone)"* ]] \
        && [ -z "$(registrations)" ]; then
        pass "the registration a killed run left behind is removed and reported"
    else
        fail "the registration a killed run left behind is removed and reported" "rc=$RC left=$(registrations) out=$OUT"
    fi
}

t_a_live_runs_registration_is_kept() {
    new_case
    mkdir -p "$G/agent-repl-safe-test-runs/20261013-000000-$$"
    run_net -- true
    if [ "$RC" -eq 0 ] && [ "$(registrations)" = "20261013-000000-$$" ]; then
        pass "a concurrent live run's registration is never pruned"
    else
        fail "a concurrent live run's registration is never pruned" "rc=$RC left=$(registrations) out=$OUT"
    fi
}

t_dashdash_without_a_command_is_refused
t_clean_wrapped_run_passes_and_removes_its_checkpoint
t_a_run_unregisters_itself
t_the_wrapped_command_carries_the_run_marker
t_wrapped_exit_status_passes_through
t_wrapped_arguments_arrive_intact
t_head_moved_by_the_command_is_drift
t_status_dirtied_by_the_command_is_drift
t_failure_outranks_drift
t_a_ref_the_run_creates_is_drift
t_a_ref_the_run_deletes_is_drift
t_a_ref_moved_by_a_scrubbed_environment_is_drift
t_a_ref_the_run_sets_to_its_own_value_is_not_drift
t_a_branch_another_session_moves_mid_run_is_not_drift
t_a_branch_another_session_creates_mid_run_is_not_drift
t_another_sessions_move_does_not_hide_the_runs_own
t_a_missing_hook_is_an_environment_error
t_a_gone_runs_registration_is_pruned
t_a_live_runs_registration_is_kept

echo "-----"
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ]
