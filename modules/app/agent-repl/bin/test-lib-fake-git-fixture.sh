#!/usr/bin/env bash
# Hermetic tests for bin/lib-fake-git-fixture.sh, the fixture setup every
# fake-git harness shares (no test runs real git).

# shellcheck disable=SC2329
# SC2329: the tests are found and run by name (declare -F), so shellcheck sees
# no caller for them or their helpers.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
REPO_DIR="$(cd "$THIS_DIR/../../../.." && pwd -P)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# shellcheck source=/dev/null
. "$THIS_DIR/lib-fake-git-fixture.sh"
# shellcheck source=/dev/null
. "$THIS_DIR/lib-grep-in.sh"

PASS=0
FAIL=0
pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; shift; [ $# -eq 0 ] || printf '%s\n' "$@" | sed 's/^/        /' >&2; FAIL=$((FAIL + 1)); }

test_clear_live_git_bindings_unsets_every_binding() {
  local left
  left="$(
    export GIT_DIR=a GIT_WORK_TREE=b GIT_INDEX_FILE=c GIT_PREFIX=d GIT_COMMON_DIR=e
    clear_live_git_bindings
    env | grep -E '^(GIT_DIR|GIT_WORK_TREE|GIT_INDEX_FILE|GIT_PREFIX|GIT_COMMON_DIR)=' || true
  )"
  if [ -z "$left" ]; then pass "clear_live_git_bindings unsets every live git binding"; else fail "bindings left set" "$left"; fi
}

test_install_checked_in_hook_installs_the_checked_in_file() {
  local repo="$TMP/repo-ok"
  mkdir -p "$repo"
  install_checked_in_hook "$repo" pre-commit
  if cmp -s "$REPO_DIR/.githooks/pre-commit" "$repo/.fakegit/hooks/pre-commit" && [ -x "$repo/.fakegit/hooks/pre-commit" ]; then
    pass "install_checked_in_hook installs the checked-in hook, executable, in the fixture's hooks directory"
  else
    fail "install_checked_in_hook installs the checked-in hook, executable, in the fixture's hooks directory" "$(ls -l "$repo/.fakegit/hooks" 2>&1)"
  fi
}

test_install_checked_in_hook_refuses_an_unknown_hook() {
  local repo="$TMP/repo-missing" rc=0 out
  mkdir -p "$repo"
  out="$(install_checked_in_hook "$repo" no-such-hook 2>&1)" || rc=$?
  if [ "$rc" -eq 2 ] && grep_in "$out" -qF "no checked-in hook $REPO_DIR/.githooks/no-such-hook for the fixture $repo" \
    && [ ! -e "$repo/.fakegit" ]; then
    pass "install_checked_in_hook refuses a hook that is not checked in, naming it and the fixture, and creates nothing"
  else
    fail "install_checked_in_hook refuses a hook that is not checked in, naming it and the fixture, and creates nothing" "rc=$rc" "$out"
  fi
}

# two_commit_repo needs the fake as `git`, as every harness that calls it has.
with_fake_git() {
  [ -x "$TMP/bin/git" ] || { mkdir -p "$TMP/bin" && "$THIS_DIR/fake-git.sh" --install "$TMP/bin/git"; }
  PATH="$TMP/bin:$PATH" "$@"
}

test_two_commit_repo_builds_two_commits_with_the_hook() {
  local parent="$TMP/two-ok" head tags
  mkdir -p "$parent"
  with_fake_git two_commit_repo "$parent" pre-push
  head="$(cd "$FIXTURE_REPO" && with_fake_git git rev-parse HEAD)"
  if [ -n "$FIXTURE_FIRST" ] && [ "$FIXTURE_FIRST" != "$FIXTURE_SECOND" ] && [ "$head" = "$FIXTURE_SECOND" ] &&
    cmp -s "$REPO_DIR/.githooks/pre-push" "$FIXTURE_REPO/.fakegit/hooks/pre-push" &&
    [ "$(dirname "$FIXTURE_REPO")" = "$(cd "$parent" && pwd)" ]; then
    pass "two_commit_repo builds two commits under the parent, master at the second, with the hook installed"
  else
    tags="first=$FIXTURE_FIRST second=$FIXTURE_SECOND head=$head repo=$FIXTURE_REPO"
    fail "two_commit_repo builds two commits under the parent, master at the second, with the hook installed" "$tags"
  fi
}

test_two_commit_repo_refuses_an_unknown_hook() {
  local parent="$TMP/two-missing" rc=0 out
  mkdir -p "$parent"
  out="$(with_fake_git two_commit_repo "$parent" no-such-hook 2>&1)" || rc=$?
  if [ "$rc" -ne 0 ] && grep_in "$out" -qF "no checked-in hook $REPO_DIR/.githooks/no-such-hook"; then
    pass "two_commit_repo fails, naming a hook that is not checked in"
  else
    fail "two_commit_repo fails, naming a hook that is not checked in" "rc=$rc" "$out"
  fi
}

# The call sites share the helpers: no harness unsets the live bindings or
# installs a hook into a fake repository itself.
test_no_harness_hand_rolls_the_fixture_setup() {
  local found
  found="$(grep -rlE 'unset GIT_DIR|^[[:space:]]*cp .*/hooks/|git commit -qm two' --include='test-*.sh' \
    "$REPO_DIR/bin" "$REPO_DIR/.githooks" "$REPO_DIR/.claude" "$REPO_DIR/modules/app/agent-repl/bin" "$REPO_DIR/modules/app/agent-repl/scripts" || true)"
  found="$(grep -v '/test-lib-fake-git-fixture\.sh$' <<<"$found" || true)"
  if [ -z "$found" ]; then pass "no harness hand-rolls the fixture setup lib-fake-git-fixture.sh provides"; else fail "these harnesses hand-roll it; source lib-fake-git-fixture.sh instead:" "$found"; fi
}

for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
