#!/usr/bin/env bash

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

# test-safe-test-run.sh — hermetic tests for safe-test-run.sh's `--` mode, the
# git-state net bin/test-all.sh runs its whole parallel run inside.
#
# NO REAL GIT RUNS. A stub `git` first on PATH answers exactly the commands the
# net issues, from state files a case controls, and refuses anything else
# loudly. A case "drifts" the checkout by editing those files from inside the
# wrapped command, exactly when a real unit would have touched the repository.

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/safe-test-run.sh"

PASS=0
FAIL=0
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -z "${2:-}" ] || echo "       $2"; }

STUBS="$TMP/stubs"
mkdir -p "$STUBS"
cat >"$STUBS/git" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
S="$FAKE_GIT_STATE"
printf '%s\n' "$*" >>"$S/calls"
case "$*" in
    "rev-parse --show-toplevel") printf '%s\n' "$FAKE_GIT_TOPLEVEL" ;;
    "rev-parse HEAD") cat "$S/head" ;;
    "rev-parse --abbrev-ref HEAD") cat "$S/branch" ;;
    "worktree list --porcelain")
        printf 'worktree %s\nbranch refs/heads/%s\n' "$FAKE_GIT_TOPLEVEL" "$(cat "$S/branch")"
        [ ! -f "$S/siblings" ] || cat "$S/siblings"
        ;;
    "for-each-ref --format=%(refname) %(objectname) refs/heads/ refs/tags/") cat "$S/refs" ;;
    "status --porcelain") cat "$S/status" ;;
    "tag -d "*) grep -v "^refs/tags/$3 " "$S/refs" >"$S/refs.new" || true; mv "$S/refs.new" "$S/refs" ;;
    "tag "*) printf 'refs/tags/%s %s\n' "$2" "$3" >>"$S/refs" ;;
    *) echo "fake git: unexpected command: git $*" >&2; exit 99 ;;
esac
EOF
chmod +x "$STUBS/git"

# new_case — a fixture checkout and fresh git state, in CASE and STATE.
new_case() {
    CASE="$(mktemp -d "$TMP/case.XXXXXX")"
    STATE="$CASE/git"
    mkdir -p "$CASE/repo/modules/app/agent-repl/lisp" "$STATE"
    : >"$CASE/repo/modules/app/agent-repl/lisp/test-agent-repl.el"
    echo 1111111111111111111111111111111111111111 >"$STATE/head"
    echo main >"$STATE/branch"
    echo "refs/heads/main 1111111111111111111111111111111111111111" >"$STATE/refs"
    : >"$STATE/status"
    : >"$STATE/calls"
}

# run_net ARGS... — the net from inside the fixture; output in OUT, status in RC.
run_net() {
    RC=0
    OUT="$(cd "$CASE/repo" && PATH="$STUBS:$PATH" FAKE_GIT_STATE="$STATE" FAKE_GIT_TOPLEVEL="$CASE/repo" \
        bash "$SCRIPT_UNDER_TEST" "$@" 2>&1)" || RC=$?
}

t_dashdash_without_a_command_is_refused() {
    new_case
    run_net --
    if [ "$RC" -eq 3 ] && [[ "$OUT" == *"-- needs a command to wrap"* ]] && ! grep -q '^tag ' "$STATE/calls"; then
        pass "-- without a command is refused before any checkpoint"
    else
        fail "-- without a command is refused before any checkpoint" "rc=$RC out=$OUT"
    fi
}

t_clean_wrapped_run_passes_and_removes_its_checkpoint() {
    new_case
    run_net -- true
    if [ "$RC" -eq 0 ] && [[ "$OUT" == *"zero git-state drift"* ]] \
        && grep -q '^tag agent-repl-test-checkpoint-' "$STATE/calls" \
        && grep -q '^tag -d agent-repl-test-checkpoint-' "$STATE/calls" \
        && [ "$(cat "$STATE/refs")" = "refs/heads/main 1111111111111111111111111111111111111111" ]; then
        pass "a clean wrapped run exits 0 and removes its checkpoint"
    else
        fail "a clean wrapped run exits 0 and removes its checkpoint" "rc=$RC out=$OUT calls=$(cat "$STATE/calls")"
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
    run_net -- sh -c 'printf "%s|" "$@" >"$0"' "$CASE/args" "a b" c
    if [ "$RC" -eq 0 ] && [ "$(cat "$CASE/args")" = "a b|c|" ]; then
        pass "the wrapped command's arguments arrive intact"
    else
        fail "the wrapped command's arguments arrive intact" "rc=$RC out=$OUT args=$(cat "$CASE/args" 2>/dev/null)"
    fi
}

t_head_moved_by_the_command_is_drift() {
    new_case
    run_net -- sh -c 'echo 2222222222222222222222222222222222222222 >"$0"' "$STATE/head"
    if [ "$RC" -eq 2 ] && [[ "$OUT" == *"HEAD changed: 1111111111111111111111111111111111111111 -> 2222222222222222222222222222222222222222"* ]] \
        && grep -q '^refs/tags/agent-repl-test-checkpoint-' "$STATE/refs"; then
        pass "a HEAD the command moved is drift, and the checkpoint stays"
    else
        fail "a HEAD the command moved is drift, and the checkpoint stays" "rc=$RC out=$OUT"
    fi
}

t_status_dirtied_by_the_command_is_drift() {
    new_case
    run_net -- sh -c 'echo " M lisp/core.el" >"$0"' "$STATE/status"
    if [ "$RC" -eq 2 ] && [[ "$OUT" == *"Working tree status changed"* ]]; then
        pass "a working tree the command dirtied is drift"
    else
        fail "a working tree the command dirtied is drift" "rc=$RC out=$OUT"
    fi
}

t_failure_outranks_drift() {
    new_case
    run_net -- sh -c 'echo " M x" >"$0"; exit 7' "$STATE/status"
    if [ "$RC" -eq 7 ] && [[ "$OUT" == *"DRIFT DETECTED"* ]]; then
        pass "a failing command that also drifted exits with its own status and reports the drift"
    else
        fail "a failing command that also drifted exits with its own status and reports the drift" "rc=$RC out=$OUT"
    fi
}

# sibling_rebasing NAME — a sibling worktree whose rebase of refs/heads/NAME is
# in progress, so it stands on a detached HEAD; its porcelain lines go into
# NEXT, which the wrapped command installs as the sibling list mid-run.
sibling_rebasing() {
    local wt="$CASE/$1" gitdir="$CASE/gitdirs/$1"
    mkdir -p "$wt" "$gitdir/rebase-merge"
    printf 'gitdir: %s\n' "$gitdir" >"$wt/.git"
    printf 'refs/heads/%s\n' "$1" >"$gitdir/rebase-merge/head-name"
    printf '\nworktree %s\nHEAD 3333333333333333333333333333333333333333\ndetached\n' "$wt"
}

# sibling_on NAME — a sibling worktree with refs/heads/NAME checked out.
sibling_on() {
    mkdir -p "$CASE/$1"
    printf '\nworktree %s\nHEAD 3333333333333333333333333333333333333333\nbranch refs/heads/%s\n' "$CASE/$1" "$1"
}

t_a_sibling_starting_a_rebase_is_not_drift() {
    new_case
    echo "refs/heads/sib 4444444444444444444444444444444444444444" >>"$STATE/refs"
    sibling_on sib >"$STATE/siblings"
    sibling_rebasing sib >"$CASE/next"
    run_net -- sh -c 'cp "$0" "$1"; sed -i.bak "s/^refs\/heads\/sib .*/refs\/heads\/sib 5555555555555555555555555555555555555555/" "$2"' \
        "$CASE/next" "$STATE/siblings" "$STATE/refs"
    if [ "$RC" -eq 0 ] && [[ "$OUT" == *"zero git-state drift"* ]]; then
        pass "a sibling worktree that starts a rebase mid-run is not drift"
    else
        fail "a sibling worktree that starts a rebase mid-run is not drift" "rc=$RC out=$OUT"
    fi
}

t_a_sibling_finishing_a_rebase_is_not_drift() {
    new_case
    echo "refs/heads/sib 4444444444444444444444444444444444444444" >>"$STATE/refs"
    sibling_rebasing sib >"$STATE/siblings"
    sibling_on sib >"$CASE/next"
    run_net -- sh -c 'cp "$0" "$1"; sed -i.bak "s/^refs\/heads\/sib .*/refs\/heads\/sib 5555555555555555555555555555555555555555/" "$2"' \
        "$CASE/next" "$STATE/siblings" "$STATE/refs"
    if [ "$RC" -eq 0 ] && [[ "$OUT" == *"zero git-state drift"* ]]; then
        pass "a sibling worktree that finishes a rebase mid-run is not drift"
    else
        fail "a sibling worktree that finishes a rebase mid-run is not drift" "rc=$RC out=$OUT"
    fi
}

t_a_detached_sibling_with_no_rebase_owns_nothing() {
    new_case
    echo "refs/heads/loose 4444444444444444444444444444444444444444" >>"$STATE/refs"
    mkdir -p "$CASE/idle"
    printf '\nworktree %s\nHEAD 3333333333333333333333333333333333333333\ndetached\n' "$CASE/idle" >"$STATE/siblings"
    run_net -- sh -c 'sed -i.bak "s/^refs\/heads\/loose .*/refs\/heads\/loose 5555555555555555555555555555555555555555/" "$0"' "$STATE/refs"
    if [ "$RC" -eq 2 ] && [[ "$OUT" == *"refs/heads/loose"* ]]; then
        pass "a detached sibling with no rebase in progress vouches for no branch"
    else
        fail "a detached sibling with no rebase in progress vouches for no branch" "rc=$RC out=$OUT"
    fi
}

t_dashdash_without_a_command_is_refused
t_a_sibling_starting_a_rebase_is_not_drift
t_a_sibling_finishing_a_rebase_is_not_drift
t_a_detached_sibling_with_no_rebase_owns_nothing
t_clean_wrapped_run_passes_and_removes_its_checkpoint
t_wrapped_exit_status_passes_through
t_wrapped_arguments_arrive_intact
t_head_moved_by_the_command_is_drift
t_status_dirtied_by_the_command_is_drift
t_failure_outranks_drift

echo "-----"
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ]
