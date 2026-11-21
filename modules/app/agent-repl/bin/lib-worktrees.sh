#!/usr/bin/env bash

# lib-worktrees.sh — the ONE reading of a repository's worktree list.
#
# `git worktree list --porcelain` names the MAIN worktree first and every
# linked worktree after it, each as a `worktree <path>` line. Every script that
# asks which worktree is the main one, or which are linked, reads that list
# here, so the parsing cannot drift between them, and so does every script
# that asks which worktree holds a branch. test-lib-worktrees.sh fails any
# script that parses the list itself again.
#
# Each function answers non-zero when git does: a caller decides whether that
# is its own error to report.

# worktree_list DIR: every worktree root of DIR's repository, main first.
worktree_list() {
    local listing
    listing="$(git -C "$1" worktree list --porcelain)" || return 1
    sed -n 's/^worktree //p' <<<"$listing"
}

# roots_main ROOTS: the main worktree of a worktree_list answer, read once and
# kept by a caller that asks more than once (requirements.sh's run).
roots_main() {
    printf '%s\n' "${1%%$'\n'*}"
}

# roots_linked ROOTS: every linked worktree of a worktree_list answer, one a
# line; nothing when the main worktree is the only one.
roots_linked() {
    case "$1" in
        *$'\n'*) printf '%s\n' "${1#*$'\n'}" ;;
    esac
}

# main_worktree DIR: the main worktree of DIR's repository.
main_worktree() {
    local roots
    roots="$(worktree_list "$1")" || return 1
    [ -n "$roots" ] || return 1
    roots_main "$roots"
}

# linked_worktrees DIR: every linked worktree of DIR's repository, one a line.
linked_worktrees() {
    local roots
    roots="$(worktree_list "$1")" || return 1
    roots_linked "$roots"
}

# branch_worktree DIR BRANCH: the worktree of DIR's repository that has BRANCH
# checked out. Non-zero, with nothing printed, when none has.
branch_worktree() {
    local listing
    listing="$(git -C "$1" worktree list --porcelain)" || return 1
    awk -v want="branch refs/heads/$2" '
        /^worktree / { root = substr($0, 10) }
        $0 == want { print root; found = 1; exit }
        END { exit !found }' <<<"$listing"
}
