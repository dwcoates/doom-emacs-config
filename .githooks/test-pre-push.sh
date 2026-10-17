#!/usr/bin/env bash
# test-pre-push.sh — hermetic tests for .githooks/pre-push.
#
# Each fixture is a throwaway repository with commits, version labels, and the
# checked-in hook installed, and one PUSH, handed to the hook exactly as git
# hands it: the remote's name and URL as arguments, and one
# "<local ref> <local sha> <remote ref> <remote sha>" line per ref the push
# sends on stdin. It asserts whether the hook let the push through (a nonzero
# exit is git sending nothing) and what the hook said about it.
#
# NO REAL GIT (owner rule: no test of any form runs real git). The fixtures
# are bin/fake-git.sh repositories, and the fake is the only `git` on PATH.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/lib-grep-in.sh"
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/lib-fake-git-fixture.sh"
clear_live_git_bindings

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
FAKE_GIT="$THIS_DIR/../modules/app/agent-repl/bin/fake-git.sh"
PASS=0
FAIL=0

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-prepush-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# The only `git` any fixture or the hook can reach.
FAKE_BIN="$TMP/bin"
mkdir -p "$FAKE_BIN"
"$FAKE_GIT" --install "$FAKE_BIN/git"
export PATH="$FAKE_BIN:$PATH"

pass() {
  printf '  PASS: %s\n' "$1"
  PASS=$((PASS + 1))
}

fail() {
  printf '  FAIL: %s\n' "$1" >&2
  FAIL=$((FAIL + 1))
  shift
  if [ "$#" -gt 0 ]; then
    printf '%s\n' "$@" | sed 's/^/        /' >&2
  fi
}

readonly ZERO=0000000000000000000000000000000000000000

# mkrepo — a fake repository with two commits and the hook installed. FIRST and
# SECOND hold the commits' ids; SECOND is master's tip.
mkrepo() {
  two_commit_repo "$TMP" pre-push
  REPO="$FIXTURE_REPO" FIRST="$FIXTURE_FIRST" SECOND="$FIXTURE_SECOND"
}

label() { git -C "$REPO" tag "$1" "$2"; }

# push LINE... — run the hook for one push to origin whose stdin is the LINEs,
# from the worktree's top as git runs it. RUN_RC and RUN_OUT hold its outcome.
push() {
  set +e
  RUN_OUT="$(cd "$REPO" && printf '%s\n' "$@" | .fakegit/hooks/pre-push origin git@example.com:repo.git 2>&1)"
  RUN_RC=$?
  set -e
}

master_line() { printf 'refs/heads/master %s refs/heads/master %s' "$1" "$ZERO"; }
release_line() { printf 'refs/tags/release-%s %s refs/tags/release-%s %s' "$1" "$2" "$1" "$ZERO"; }

expect_refused() {
  local name="$1" why="$2"
  if [ "$RUN_RC" -ne 0 ] &&
    grep_in "$RUN_OUT" -qF "REFUSED push to origin" &&
    grep_in "$RUN_OUT" -qF "$why" &&
    grep_in "$RUN_OUT" -qF "modules/app/agent-repl/bin/push-release.sh"; then
    pass "$name"
  else
    fail "$name" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_master_with_its_release_tag_goes() {
  mkrepo
  label 0.2.0 "$SECOND"
  push "$(master_line "$SECOND")" "$(release_line 0.2.0 "$SECOND")"
  if [ "$RUN_RC" -eq 0 ] && grep_in "$RUN_OUT" -qF "master at 0.2.0 ($SECOND) goes to origin with refs/tags/release-0.2.0"; then
    pass "master pushed with its version's release tag goes, naming both"
  else
    fail "master pushed with its version's release tag goes, naming both" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_tags_that_are_not_version_labels_are_ignored() {
  mkrepo
  label 0.2.0 "$SECOND"
  label agent-repl-test-checkpoint-1 "$SECOND"
  label 0.2 "$SECOND"
  push "$(master_line "$SECOND")" "$(release_line 0.2.0 "$SECOND")"
  if [ "$RUN_RC" -eq 0 ]; then
    pass "tags that are not X.Y.Z do not count as version labels"
  else
    fail "tags that are not X.Y.Z do not count as version labels" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_master_without_a_version_label_is_refused() {
  mkrepo
  push "$(master_line "$SECOND")"
  expect_refused "master whose tip has no version label is refused" "$SECOND carries no version label"
}

test_master_with_two_version_labels_is_refused() {
  mkrepo
  label 0.2.0 "$SECOND"
  label 0.3.0 "$SECOND"
  push "$(master_line "$SECOND")" "$(release_line 0.2.0 "$SECOND")"
  expect_refused "master whose tip has two version labels is refused as a bug" "carries 2 version labels (0.2.0 0.3.0)"
}

test_master_without_its_release_tag_is_refused() {
  mkrepo
  label 0.2.0 "$SECOND"
  push "$(master_line "$SECOND")"
  expect_refused "master pushed without its release tag is refused" "master moves to 0.2.0 ($SECOND) without refs/tags/release-0.2.0"
}

test_release_tag_at_another_commit_is_refused() {
  mkrepo
  label 0.1.0 "$FIRST"
  label 0.2.0 "$SECOND"
  push "$(master_line "$SECOND")" "$(release_line 0.2.0 "$FIRST")"
  expect_refused "master pushed with its release tag at another commit is refused" "without refs/tags/release-0.2.0 at that commit"
}

test_release_tag_naming_the_wrong_version_is_refused() {
  mkrepo
  label 0.2.0 "$SECOND"
  push "$(master_line "$SECOND")" "$(release_line 0.2.0 "$SECOND")" "$(release_line 0.9.0 "$SECOND")"
  expect_refused "a release tag whose version is not its commit's label is refused" "refs/tags/release-0.9.0 names $SECOND, whose version label is 0.2.0"
}

test_release_tag_without_master_is_refused() {
  mkrepo
  label 0.1.0 "$FIRST"
  push "$(release_line 0.1.0 "$FIRST")"
  expect_refused "a release tag pushed without master is refused" "refs/tags/release-0.1.0 names $FIRST, but this push does not move master there"
}

test_other_branches_go_silently() {
  mkrepo
  push "refs/heads/feature $SECOND refs/heads/feature $ZERO"
  if [ "$RUN_RC" -eq 0 ] && [ -z "$RUN_OUT" ]; then
    pass "a push of another branch goes, and the hook says nothing"
  else
    fail "a push of another branch goes, and the hook says nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_deleting_master_goes() {
  mkrepo
  push "(delete) $ZERO refs/heads/master $SECOND"
  if [ "$RUN_RC" -eq 0 ] && [ -z "$RUN_OUT" ]; then
    pass "deleting the remote's master is never refused"
  else
    fail "deleting the remote's master is never refused" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_a_foreign_repository_sharing_the_hooks_dir_is_untouched() {
  mkrepo
  local owner="$REPO" foreign
  foreign="$(mktemp -d "$TMP/foreign.XXXXXX")"
  git -C "$foreign" init
  git -C "$foreign" config core.hooksPath "$owner/.fakegit/hooks"
  set +e
  RUN_OUT="$(cd "$foreign" && master_line "$SECOND" | "$owner/.fakegit/hooks/pre-push" origin url 2>&1)"
  RUN_RC=$?
  set -e
  if [ "$RUN_RC" -eq 0 ] && [ -z "$RUN_OUT" ]; then
    pass "a repository that inherits the hooks directory is never checked"
  else
    fail "a repository that inherits the hooks directory is never checked" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_git_push_runs_the_hook_and_a_refusal_sends_nothing() {
  mkrepo
  label 0.2.0 "$SECOND"
  set +e
  RUN_OUT="$(cd "$REPO" && git push --atomic origin refs/heads/master:refs/heads/master 2>&1)"
  RUN_RC=$?
  set -e
  if [ "$RUN_RC" -ne 0 ] && grep_in "$RUN_OUT" -qF "without refs/tags/release-0.2.0" &&
    [ -z "$(git -C "$REPO" ls-remote origin)" ]; then
    pass "git push runs the hook, and its refusal leaves the remote untouched"
  else
    fail "git push runs the hook, and its refusal leaves the remote untouched" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

echo "test-pre-push.sh"
for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done
printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
