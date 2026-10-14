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
# test-lib-fake-git-fixture.sh fails any harness that hand-rolls either.

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
