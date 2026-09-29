#!/usr/bin/env bash
# test-reference-transaction.sh — hermetic tests for .githooks/reference-transaction.
#
# Each fixture is a throwaway repository with the hook installed as its only
# hook. A fixture drives one way master (or another branch) moves, and asserts
# both halves: whether the move landed, and what the hook said about it.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

# This harness is itself run from inside git hooks and agent sessions. Clear
# the caller's live Git bindings, and the hook's own markers, before any
# fixture command runs.
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_PREFIX GIT_COMMON_DIR
unset AGENT_REPL_MERGE_QUEUE AGENT_REPL_OWNER_OVERRIDE

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
HOOK_SRC="$THIS_DIR/reference-transaction"
PASS=0
FAIL=0

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-reftx-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

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

# fixture_git REPO ARGS... — git in the fixture with the hook as its only hook.
fixture_git() {
  local repo="$1"
  shift
  git -C "$repo" -c core.hooksPath="$repo/.git/hooks" -c commit.gpgsign=false "$@"
}

# mkrepo — a repository on master with one commit, and a branch `feature` one
# commit ahead of it. The hook is installed only after the history exists, so
# building the fixture never depends on the code under test.
mkrepo() {
  local repo
  repo="$(mktemp -d "$TMP/repo.XXXXXX")"
  git -C "$repo" init -q
  git -C "$repo" symbolic-ref HEAD refs/heads/master
  git -C "$repo" config user.email "test@example.com"
  git -C "$repo" config user.name "Test"
  printf 'base\n' >"$repo/base.txt"
  fixture_git "$repo" add base.txt
  fixture_git "$repo" commit -q -m base
  fixture_git "$repo" checkout -q -b feature
  printf 'feature\n' >"$repo/feature.txt"
  fixture_git "$repo" add feature.txt
  fixture_git "$repo" commit -q -m feature
  fixture_git "$repo" checkout -q master
  mkdir -p "$repo/.git/hooks"
  cp "$HOOK_SRC" "$repo/.git/hooks/reference-transaction"
  chmod +x "$repo/.git/hooks/reference-transaction"
  printf '%s\n' "$repo"
}

enforce() {
  git -C "$1" config agentrepl.mergeQueueEnforce "$2"
}

tip() {
  git -C "$1" rev-parse "$2"
}

# attempt REPO ENV... -- ARGS... — run one git under extra bindings, capturing
# its exit status and combined output.
attempt() {
  local repo="$1"
  shift
  local bindings=()
  while [ "$1" != "--" ]; do
    bindings+=("$1")
    shift
  done
  shift
  set +e
  RUN_OUT="$(env ${bindings[@]+"${bindings[@]}"} git -C "$repo" -c core.hooksPath="$repo/.git/hooks" -c commit.gpgsign=false "$@" 2>&1)"
  RUN_RC=$?
  set -e
}

commit_on_master() {
  local repo="$1"
  shift
  printf 'change\n' >"$repo/change.txt"
  fixture_git "$repo" add change.txt
  attempt "$repo" "$@" -- commit -q -m change
}

test_enforcement_off_is_a_silent_no_op() {
  local repo before
  repo="$(mkrepo)"
  before="$(tip "$repo" master)"
  commit_on_master "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ "$(tip "$repo" master)" != "$before" ] && [ -z "$RUN_OUT" ]; then
    pass "enforcement off: a master commit lands and the hook says nothing"
  else
    fail "enforcement off: a master commit lands and the hook says nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforcement_false_is_a_silent_no_op() {
  local repo before
  repo="$(mkrepo)"
  enforce "$repo" false
  before="$(tip "$repo" master)"
  commit_on_master "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ "$(tip "$repo" master)" != "$before" ] && [ -z "$RUN_OUT" ]; then
    pass "enforcement set false: a master commit lands and the hook says nothing"
  else
    fail "enforcement set false: a master commit lands and the hook says nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_master_commit_is_refused() {
  local repo before
  repo="$(mkrepo)"
  enforce "$repo" true
  before="$(tip "$repo" master)"
  commit_on_master "$repo"

  if [ "$RUN_RC" -ne 0 ] && [ "$(tip "$repo" master)" = "$before" ] &&
    printf '%s\n' "$RUN_OUT" | grep -q "merge-queue/SKILL.md"; then
    pass "enforcement on: a master commit is refused, naming the skill"
  else
    fail "enforcement on: a master commit is refused, naming the skill" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_refusal_names_each_skill_form() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" true
  commit_on_master "$repo"

  if [ "$RUN_RC" -ne 0 ] &&
    printf '%s\n' "$RUN_OUT" | grep -q "/merge-queue own" &&
    printf '%s\n' "$RUN_OUT" | grep -q "/merge-queue workspace <worktree-dir>" &&
    printf '%s\n' "$RUN_OUT" | grep -q "/merge-queue branch <branch-name>" &&
    printf '%s\n' "$RUN_OUT" | grep -q "never set AGENT_REPL_OWNER_OVERRIDE"; then
    pass "enforcement on: the refusal names every skill form and forbids the override to agents"
  else
    fail "enforcement on: the refusal names every skill form and forbids the override to agents" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_master_fast_forward_is_refused() {
  local repo before
  repo="$(mkrepo)"
  enforce "$repo" true
  before="$(tip "$repo" master)"
  attempt "$repo" -- merge -q --ff-only feature

  if [ "$RUN_RC" -ne 0 ] && [ "$(tip "$repo" master)" = "$before" ] &&
    printf '%s\n' "$RUN_OUT" | grep -q "merge-queue/SKILL.md"; then
    pass "enforcement on: a hand fast-forward of master is refused, naming the skill"
  else
    fail "enforcement on: a hand fast-forward of master is refused, naming the skill" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_master_merge_commit_is_refused() {
  local repo before
  repo="$(mkrepo)"
  enforce "$repo" true
  before="$(tip "$repo" master)"
  attempt "$repo" -- merge -q --no-ff -m merge feature

  if [ "$RUN_RC" -ne 0 ] && [ "$(tip "$repo" master)" = "$before" ]; then
    pass "enforcement on: a hand merge commit onto master is refused"
  else
    fail "enforcement on: a hand merge commit onto master is refused" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_queue_fast_forward_lands() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" true
  attempt "$repo" AGENT_REPL_MERGE_QUEUE=1 -- merge -q --ff-only feature

  if [ "$RUN_RC" -eq 0 ] && [ "$(tip "$repo" master)" = "$(tip "$repo" feature)" ] && [ -z "$RUN_OUT" ]; then
    pass "enforcement on: the queue's marked fast-forward lands silently"
  else
    fail "enforcement on: the queue's marked fast-forward lands silently" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_marker_other_than_one_is_refused() {
  local repo before
  repo="$(mkrepo)"
  enforce "$repo" true
  before="$(tip "$repo" master)"
  attempt "$repo" AGENT_REPL_MERGE_QUEUE=yes -- merge -q --ff-only feature

  if [ "$RUN_RC" -ne 0 ] && [ "$(tip "$repo" master)" = "$before" ]; then
    pass "enforcement on: a marker that is not exactly 1 vouches for nothing"
  else
    fail "enforcement on: a marker that is not exactly 1 vouches for nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_owner_override_lands_and_says_so() {
  local repo before
  repo="$(mkrepo)"
  enforce "$repo" true
  before="$(tip "$repo" master)"
  commit_on_master "$repo" AGENT_REPL_OWNER_OVERRIDE=1

  if [ "$RUN_RC" -eq 0 ] && [ "$(tip "$repo" master)" != "$before" ] &&
    printf '%s\n' "$RUN_OUT" | grep -q "BYPASSING the merge queue"; then
    pass "enforcement on: the owner override lands and says it bypassed the queue"
  else
    fail "enforcement on: the owner override lands and says it bypassed the queue" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_other_branch_is_untouched() {
  local repo before
  repo="$(mkrepo)"
  enforce "$repo" true
  fixture_git "$repo" checkout -q feature
  before="$(tip "$repo" feature)"
  printf 'more\n' >"$repo/more.txt"
  fixture_git "$repo" add more.txt
  attempt "$repo" -- commit -q -m more

  if [ "$RUN_RC" -eq 0 ] && [ "$(tip "$repo" feature)" != "$before" ] && [ -z "$RUN_OUT" ]; then
    pass "enforcement on: a commit on another branch lands and the hook says nothing"
  else
    fail "enforcement on: a commit on another branch lands and the hook says nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_unreadable_switch_is_refused() {
  local repo before
  repo="$(mkrepo)"
  enforce "$repo" sometimes
  before="$(tip "$repo" master)"
  commit_on_master "$repo"

  if [ "$RUN_RC" -ne 0 ] && [ "$(tip "$repo" master)" = "$before" ] &&
    printf '%s\n' "$RUN_OUT" | grep -q "agentrepl.mergeQueueEnforce is set but unreadable"; then
    pass "an unreadable enforcement switch refuses to move master"
  else
    fail "an unreadable enforcement switch refuses to move master" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforcement_off_is_a_silent_no_op
test_enforcement_false_is_a_silent_no_op
test_enforced_master_commit_is_refused
test_enforced_refusal_names_each_skill_form
test_enforced_master_fast_forward_is_refused
test_enforced_master_merge_commit_is_refused
test_enforced_queue_fast_forward_lands
test_enforced_marker_other_than_one_is_refused
test_enforced_owner_override_lands_and_says_so
test_enforced_other_branch_is_untouched
test_unreadable_switch_is_refused

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
