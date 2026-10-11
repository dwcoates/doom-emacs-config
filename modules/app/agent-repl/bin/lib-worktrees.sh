#!/usr/bin/env bash

# lib-worktrees.sh — the ONE reading of a repository's worktree list.
#
# `git worktree list --porcelain` names the MAIN worktree first and every
# linked worktree after it, each as a `worktree <path>` line. Every script that
# asks which worktree is the main one, or which are linked, reads that list
# here, so the parsing cannot drift between them. test-lib-worktrees.sh fails
# any script that parses the list itself again.
#
# Each function answers non-zero when git does: a caller decides whether that
# is its own error to report.

# worktree_list DIR: every worktree root of DIR's repository, main first.
worktree_list() {
    local listing
    listing="$(git -C "$1" worktree list --porcelain)" || return 1
    sed -n 's/^worktree //p' <<<"$listing"
}

# main_worktree DIR: the main worktree of DIR's repository.
main_worktree() {
    local roots
    roots="$(worktree_list "$1")" || return 1
    [ -n "$roots" ] || return 1
    sed -n 1p <<<"$roots"
}

# linked_worktrees DIR: every linked worktree of DIR's repository, one a line.
linked_worktrees() {
    local roots
    roots="$(worktree_list "$1")" || return 1
    sed 1d <<<"$roots"
}
