#!/usr/bin/env bash

# lib-land-gate.sh — the gate a direct landing passes before it merges
# (bin/land-direct.sh), so a landing that skips the merge queue still lands
# only what the full suite passed.
#
# land_gate MAIN BRANCH refuses, with one line on stdout naming why, unless:
#   - BRANCH is checked out in a worktree of MAIN's repository, with nothing
#     uncommitted (the suite tests that worktree, and only commits land);
#   - BRANCH contains MAIN's head, so the tree the suite tests is the tree the
#     no-ff merge lands;
#   - the full suite (`agent-repld test run -every-suite`) passes in that
#     worktree; its stream is kept in a log named on stderr;
#   - neither MAIN's head nor BRANCH moved while the suite ran.
# It answers 0 when the landing may go on, LAND_GATE_REFUSED when it may not,
# and 2 when git itself failed.
#
# Before this gate, the full-suite run before a direct landing was a rule in
# two metaprompts that the script never checked (2026-11-14: 0.436.0 landed a
# broken test), and a rebase that dropped a wsm migration landed untested
# (2026-11-11: migration 122).

# shellcheck source=lib-worktrees.sh
source "$(dirname "${BASH_SOURCE[0]}")/lib-worktrees.sh"

# LAND_GATE_REFUSED is the status of a landing the gate refused.
LAND_GATE_REFUSED=4

# land_gate MAIN BRANCH [GATED] — GATED is the commit a caller's own full
# suite already passed on, rebased onto MAIN's head (the daemon's gate of a
# deploy remediation or escalation): the branch's tip must be exactly that
# commit, and the suite is not run a second time.
land_gate() {
    local main="$1" branch="$2" gated="${3:-}" wt dirty head tip suite_log rc after_head after_tip
    if ! wt="$(branch_worktree "$main" "$branch")"; then
        echo "$branch is checked out in no worktree of $main, so the full suite cannot run on it; check it out and land again"
        return "$LAND_GATE_REFUSED"
    fi
    dirty="$(git -C "$wt" status --porcelain)" || { echo "land-direct: could not read the status of $wt" >&2; return 2; }
    if [ -n "$dirty" ]; then
        echo "$wt has uncommitted changes, which the suite would test and the landing would leave out; commit or remove them and land again"
        return "$LAND_GATE_REFUSED"
    fi
    head="$(git -C "$main" rev-parse HEAD)" || { echo "land-direct: could not read the head of $main" >&2; return 2; }
    tip="$(git -C "$main" rev-parse --verify --quiet "refs/heads/$branch")" || { echo "land-direct: could not resolve $branch" >&2; return 2; }
    if ! git -C "$main" merge-base --is-ancestor "$head" "$tip"; then
        echo "$branch is not rebased onto the main branch's head $head; rebase it, resolve every conflict, and land again"
        return "$LAND_GATE_REFUSED"
    fi
    if [ -n "$gated" ]; then
        if [ "$tip" != "$gated" ]; then
            echo "$branch's tip $tip is not the commit $gated its gate passed on; gate it again and land again"
            return "$LAND_GATE_REFUSED"
        fi
        echo "[land-direct] $branch's tip $tip is the commit its caller's full suite passed on; the suite is not run again" >&2
        return 0
    fi
    suite_log="$(mktemp "${TMPDIR:-/tmp}/land-direct-suite.XXXXXX")" || { echo "land-direct: could not create the suite's log" >&2; return 2; }
    echo "[land-direct] running the full suite on $branch in $wt; its stream is in $suite_log" >&2
    rc=0
    (cd "$wt" && agent-repld test run -workspace "$wt" -every-suite) >"$suite_log" 2>&1 || rc=$?
    if [ "$rc" -ne 0 ]; then
        echo "the full suite failed on $branch (exit $rc; its stream is in $suite_log); fix every failure on the branch and land again"
        return "$LAND_GATE_REFUSED"
    fi
    after_head="$(git -C "$main" rev-parse HEAD)" || { echo "land-direct: could not read the head of $main" >&2; return 2; }
    after_tip="$(git -C "$main" rev-parse --verify --quiet "refs/heads/$branch")" || { echo "land-direct: could not resolve $branch" >&2; return 2; }
    if [ "$after_head" != "$head" ] || [ "$after_tip" != "$tip" ]; then
        echo "the main branch or $branch moved while the full suite ran ($head..$tip became $after_head..$after_tip), so what passed is not what would land; land again"
        return "$LAND_GATE_REFUSED"
    fi
    echo "[land-direct] the full suite passed on $branch at $tip" >&2
    return 0
}
