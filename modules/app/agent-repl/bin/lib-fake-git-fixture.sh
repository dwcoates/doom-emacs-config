#!/usr/bin/env bash

# lib-fake-git-fixture.sh — the fixture setup every fake-git harness shares.
# TEST SUPPORT ONLY.
#
# clear_live_git_bindings unsets what a caller's live git exports to its
# children (a hook's index, a session's work tree), so no fixture command
# can read the caller's repository as its own. Harnesses run from hooks,
# agent sessions and the safe-test-run net, so each calls it before any
# fixture exists.
#
# install_checked_in_hook REPO NAME installs the checked-in .githooks/NAME,
# executable, as the bin/fake-git.sh repository REPO's NAME hook: the hook a
# harness exercises is the one git runs, never a copy it wrote itself.
#
# two_commit_repo PARENT HOOK makes a fake repository under PARENT with the
# checked-in hook HOOK installed and two commits on master, and sets
# FIXTURE_REPO (its directory as $PWD names it), FIXTURE_FIRST and
# FIXTURE_SECOND (the commits; the second is master's tip). The fake must
# already be the `git` on PATH.
#
# test-lib-fake-git-fixture.sh fails any harness that hand-rolls one of these.

LIB_FAKE_GIT_FIXTURE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)"

clear_live_git_bindings() {
    unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_PREFIX GIT_COMMON_DIR
}

install_checked_in_hook() {
    local repo="$1" name="$2" src
    src="$LIB_FAKE_GIT_FIXTURE_ROOT/.githooks/$name"
    if [ ! -f "$src" ]; then
        echo "lib-fake-git-fixture: install_checked_in_hook: no checked-in hook $src for the fixture $repo" >&2
        return 2
    fi
    mkdir -p "$repo/.fakegit/hooks"
    cp "$src" "$repo/.fakegit/hooks/$name"
    chmod +x "$repo/.fakegit/hooks/$name"
}

# shellcheck disable=SC2034
# SC2034: FIXTURE_* are this helper's outputs, read by the harness that calls it.
two_commit_repo() {
    local parent="$1" hook="$2" repo
    repo="$(mktemp -d "$parent/repo.XXXXXX")" || return
    git -C "$repo" init || return
    install_checked_in_hook "$repo" "$hook" || return
    echo one >"$repo/file"
    (cd "$repo" && git add -A && git commit -qm one) || return
    FIXTURE_FIRST="$(git -C "$repo" rev-parse HEAD)" || return
    echo two >"$repo/file"
    (cd "$repo" && git add -A && git commit -qm two) || return
    FIXTURE_SECOND="$(git -C "$repo" rev-parse HEAD)" || return
    FIXTURE_REPO="$(cd "$repo" && pwd)"
}
