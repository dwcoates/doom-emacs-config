#!/usr/bin/env bash
# test-push-release.sh — hermetic tests for bin/push-release.sh.
#
# Each fixture is a throwaway repository whose master carries commits and
# version labels, with the checked-in pre-push hook installed, so every push
# the script makes passes through the same hook a real push does. The remote is
# the fake's ref store, read back with `git ls-remote origin`.
#
# NO REAL GIT (owner rule: no test of any form runs real git). The fixtures are
# bin/fake-git.sh repositories, and the fake is the only `git` on PATH.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/lib-grep-in.sh"
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/lib-fake-git-fixture.sh"
clear_live_git_bindings

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="$THIS_DIR/push-release.sh"
PASS=0
FAIL=0

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-push-release-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# The only `git` any fixture, the script or the hook can reach.
FAKE_BIN="$TMP/bin"
mkdir -p "$FAKE_BIN"
"$THIS_DIR/fake-git.sh" --install "$FAKE_BIN/git"
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

# mkrepo — a fake repository with two commits on master and the pre-push hook
# installed. FIRST and SECOND hold the commits' ids; SECOND is master's tip.
mkrepo() {
  two_commit_repo "$TMP" pre-push
  REPO="$FIXTURE_REPO" FIRST="$FIXTURE_FIRST" SECOND="$FIXTURE_SECOND"
}

label() { git -C "$REPO" tag "$1" "$2"; }

# release — run the script from the fixture. RUN_RC and RUN_OUT hold its outcome.
release() {
  set +e
  RUN_OUT="$(cd "$REPO" && "$SCRIPT" 2>&1)"
  RUN_RC=$?
  set -e
}

remote() { git -C "$REPO" ls-remote origin; }
local_tag() { git -C "$REPO" rev-parse --verify --quiet "refs/tags/$1" || true; }

# expect_refused NAME WHY — the script failed naming WHY, and origin holds nothing.
expect_refused() {
  local name="$1" why="$2" held
  held="$(remote)"
  if [ "$RUN_RC" -eq 1 ] && grep_in "$RUN_OUT" -qF "[push-release] $why" && [ -z "$held" ]; then
    pass "$name"
  else
    fail "$name" "exit=$RUN_RC" "$RUN_OUT" "origin holds: $held"
  fi
}

test_master_and_its_release_tag_go_together() {
  mkrepo
  label 0.1.0 "$FIRST"
  label 0.2.0 "$SECOND"
  release
  local want
  want="$(printf '%s\trefs/heads/master\n%s\trefs/tags/release-0.2.0' "$SECOND" "$SECOND")"
  if [ "$RUN_RC" -eq 0 ] && [ "$(remote)" = "$want" ] &&
    [ "$(local_tag release-0.2.0)" = "$SECOND" ] &&
    grep_in "$RUN_OUT" -qF "pushed master at 0.2.0 ($SECOND) to origin as release-0.2.0"; then
    pass "master's tip is tagged release-<its version> and both reach origin"
  else
    fail "master's tip is tagged release-<its version> and both reach origin" "exit=$RUN_RC" "$RUN_OUT" "origin holds: $(remote)"
  fi
}

test_an_existing_release_tag_at_the_tip_is_reused() {
  mkrepo
  label 0.2.0 "$SECOND"
  label release-0.2.0 "$SECOND"
  release
  if [ "$RUN_RC" -eq 0 ] && grep_in "$(remote)" -qF "$SECOND	refs/tags/release-0.2.0"; then
    pass "a release tag already at master's tip is pushed as it is"
  else
    fail "a release tag already at master's tip is pushed as it is" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_an_unlabelled_tip_is_refused_and_tags_nothing() {
  mkrepo
  label 0.1.0 "$FIRST"
  release
  expect_refused "master's tip without a version label is refused" "master's tip $SECOND has no single version label"
  if [ -z "$(local_tag release-0.1.0)" ]; then
    pass "a refused tip leaves no release tag behind"
  else
    fail "a refused tip leaves no release tag behind"
  fi
}

test_a_tip_with_two_labels_is_refused() {
  mkrepo
  label 0.2.0 "$SECOND"
  label 0.3.0 "$SECOND"
  release
  expect_refused "master's tip with two version labels is refused" "master's tip $SECOND has no single version label"
}

test_a_release_tag_at_another_commit_is_refused() {
  mkrepo
  label 0.2.0 "$SECOND"
  label release-0.2.0 "$FIRST"
  release
  expect_refused "a release tag already naming another commit is refused" "release-0.2.0 already names $FIRST, not master's tip $SECOND"
}

test_a_repository_without_master_is_refused() {
  mkrepo
  echo feature >"$REPO/.fakegit/branch"
  release
  expect_refused "a repository without a master branch is refused" "this repository has no master branch"
}

test_a_refused_push_reaches_origin_with_neither() {
  mkrepo
  label 0.2.0 "$SECOND"
  printf '#!/usr/bin/env bash\necho "[pre-push] REFUSED by the fixture" >&2\nexit 1\n' >"$REPO/.fakegit/hooks/pre-push"
  release
  expect_refused "a push the remote side refuses is reported, with origin taking neither ref" "pushing master and release-0.2.0 to origin failed; origin took neither"
}

echo "test-push-release.sh"
for t in $(declare -F | awk '{print $3}' | grep '^test_'); do
  "$t"
done
printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
