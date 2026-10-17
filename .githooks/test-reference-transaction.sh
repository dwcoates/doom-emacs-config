#!/usr/bin/env bash
# test-reference-transaction.sh — hermetic tests for .githooks/reference-transaction.
#
# Each fixture is a throwaway repository and one REF TRANSACTION, handed to the
# hook exactly as git hands it: the phase as the argument, and one
# "<old> <new> <ref>" line per ref the transaction moves on stdin.  It asserts
# both halves: whether the hook let the transaction through (in the `prepared`
# phase a nonzero exit is git aborting it, so the ref does not move), and what
# the hook said about it.
#
# NO REAL GIT (owner rule: no test of any form runs real git).  The fixtures
# are bin/fake-git.sh repositories, the fake is the only `git` on PATH, and the
# one git call the hook makes -- reading its enforcement switch -- is logged
# and asserted.  Which ref a commit, a fast-forward or a merge moves is git's
# own behavior, so each transaction below is the one git sends for that act.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/lib-grep-in.sh"

# This harness is itself run from inside git hooks and agent sessions. Clear
# the caller's live Git bindings, and the hook's own markers, before any
# fixture command runs.
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/lib-fake-git-fixture.sh"
clear_live_git_bindings
unset AGENT_REPL_MERGE_QUEUE AGENT_REPL_OWNER_OVERRIDE AGENT_REPL_SAFE_TEST_RUN

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
FAKE_GIT="$THIS_DIR/../modules/app/agent-repl/bin/fake-git.sh"
PASS=0
FAIL=0

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-reftx-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# The only `git` any fixture or the hook can reach.
FAKE_BIN="$TMP/bin"
mkdir -p "$FAKE_BIN"
ln -s "$FAKE_GIT" "$FAKE_BIN/git"
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

# Two commit ids for the transactions: master's tip, and the commit it moves to.
readonly BASE=1111111111111111111111111111111111111111
readonly NEXT=2222222222222222222222222222222222222222

# mkrepo — an empty fake repository with the hook installed.
mkrepo() {
  local repo
  repo="$(mktemp -d "$TMP/repo.XXXXXX")"
  git -C "$repo" init
  install_checked_in_hook "$repo" reference-transaction
  # The directory as the hook's $PWD names it ($TMPDIR may end in a slash).
  (cd "$repo" && pwd)
}

enforce() {
  git -C "$1" config agentrepl.mergeQueueEnforce "$2"
}

# The transactions git sends, in the `prepared` phase, for each act.
#   a commit on master:   HEAD (symbolic, reported as its target) and master.
#   a fast-forward:       ORIG_HEAD, then master.
#   a merge commit:       ORIG_HEAD, then master.
#   a commit on feature:  HEAD and refs/heads/feature.
commit_on_master_tx() { printf '%s %s HEAD\n%s %s refs/heads/master\n' "$BASE" "$NEXT" "$BASE" "$NEXT"; }
fast_forward_tx() { printf '%s %s ORIG_HEAD\n%s %s refs/heads/master\n' "$BASE" "$BASE" "$BASE" "$NEXT"; }
merge_commit_tx() { fast_forward_tx; }
commit_on_feature_tx() { printf '%s %s HEAD\n%s %s refs/heads/feature\n' "$BASE" "$NEXT" "$BASE" "$NEXT"; }

# attempt REPO TX PHASE ENV... — run the hook for one transaction (the output
# of the TX function) in PHASE under extra bindings, from the worktree's top as
# git runs it.  RUN_RC and RUN_OUT hold its outcome, GIT_CALLS its git calls.
attempt() {
  local repo="$1" tx="$2" phase="$3"
  shift 3
  local git_log="$repo/git-calls"
  rm -f "$git_log"
  set +e
  RUN_OUT="$(
    cd "$repo" &&
      "$tx" | env FAKE_GIT_LOG="$git_log" "$@" "$repo/.fakegit/hooks/reference-transaction" "$phase" 2>&1
  )"
  RUN_RC=$?
  set -e
  GIT_CALLS="$(cat "$git_log" 2>/dev/null || true)"
}

# The one git call a hook that reached its switch makes.
readonly SWITCH_CALL="git config --type=bool --get agentrepl.mergeQueueEnforce"

commit_on_master() {
  local repo="$1"
  shift
  attempt "$repo" commit_on_master_tx prepared "$@"
}

test_enforcement_off_is_a_silent_no_op() {
  local repo
  repo="$(mkrepo)"
  commit_on_master "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ -z "$RUN_OUT" ] && [ "$GIT_CALLS" = "$SWITCH_CALL" ]; then
    pass "enforcement off: a master commit lands and the hook says nothing"
  else
    fail "enforcement off: a master commit lands and the hook says nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforcement_false_is_a_silent_no_op() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" false
  commit_on_master "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ -z "$RUN_OUT" ] && [ "$GIT_CALLS" = "$SWITCH_CALL" ]; then
    pass "enforcement set false: a master commit lands and the hook says nothing"
  else
    fail "enforcement set false: a master commit lands and the hook says nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_master_commit_is_refused() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" true
  commit_on_master "$repo"

  if [ "$RUN_RC" -ne 0 ] &&
    grep_in "$RUN_OUT" -q "merge-queue/SKILL.md"; then
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
    grep_in "$RUN_OUT" -q "agent-repld workspace merge here" &&
    grep_in "$RUN_OUT" -q "agent-repld workspace async-merge" &&
    grep_in "$RUN_OUT" -q "never set AGENT_REPL_OWNER_OVERRIDE"; then
    pass "enforcement on: the refusal names every skill form and forbids the override to agents"
  else
    fail "enforcement on: the refusal names every skill form and forbids the override to agents" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_master_fast_forward_is_refused() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" true
  attempt "$repo" fast_forward_tx prepared

  if [ "$RUN_RC" -ne 0 ] &&
    grep_in "$RUN_OUT" -q "merge-queue/SKILL.md"; then
    pass "enforcement on: a hand fast-forward of master is refused, naming the skill"
  else
    fail "enforcement on: a hand fast-forward of master is refused, naming the skill" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_master_merge_commit_is_refused() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" true
  attempt "$repo" merge_commit_tx prepared

  if [ "$RUN_RC" -ne 0 ]; then
    pass "enforcement on: a hand merge commit onto master is refused"
  else
    fail "enforcement on: a hand merge commit onto master is refused" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_queue_fast_forward_lands() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" true
  attempt "$repo" fast_forward_tx prepared AGENT_REPL_MERGE_QUEUE=1

  if [ "$RUN_RC" -eq 0 ] && [ -z "$RUN_OUT" ]; then
    pass "enforcement on: the queue's marked fast-forward lands silently"
  else
    fail "enforcement on: the queue's marked fast-forward lands silently" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_marker_other_than_one_is_refused() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" true
  attempt "$repo" fast_forward_tx prepared AGENT_REPL_MERGE_QUEUE=yes

  if [ "$RUN_RC" -ne 0 ]; then
    pass "enforcement on: a marker that is not exactly 1 vouches for nothing"
  else
    fail "enforcement on: a marker that is not exactly 1 vouches for nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_owner_override_lands_and_says_so() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" true
  commit_on_master "$repo" AGENT_REPL_OWNER_OVERRIDE=1

  if [ "$RUN_RC" -eq 0 ] &&
    grep_in "$RUN_OUT" -q "BYPASSING the merge queue"; then
    pass "enforcement on: the owner override lands and says it bypassed the queue"
  else
    fail "enforcement on: the owner override lands and says it bypassed the queue" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_other_branch_is_untouched() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" true
  attempt "$repo" commit_on_feature_tx prepared

  if [ "$RUN_RC" -eq 0 ] && [ -z "$RUN_OUT" ] && [ -z "$GIT_CALLS" ]; then
    pass "enforcement on: a commit on another branch lands and the hook says nothing"
  else
    fail "enforcement on: a commit on another branch lands and the hook says nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_unreadable_switch_is_refused() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" sometimes
  commit_on_master "$repo"

  if [ "$RUN_RC" -ne 0 ] &&
    grep_in "$RUN_OUT" -q "agentrepl.mergeQueueEnforce is set but unreadable"; then
    pass "an unreadable enforcement switch refuses to move master"
  else
    fail "an unreadable enforcement switch refuses to move master" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_enforced_aborted_report_never_refuses() {
  # Only `prepared` can abort a transaction; `aborted` reports one already
  # decided, and the hook reads nothing for it.
  local repo
  repo="$(mkrepo)"
  enforce "$repo" true
  attempt "$repo" commit_on_master_tx aborted

  if [ "$RUN_RC" -eq 0 ] && [ -z "$RUN_OUT" ] && [ -z "$GIT_CALLS" ]; then
    pass "enforcement on: the aborted report passes silently and reads nothing"
  else
    fail "enforcement on: the aborted report passes silently and reads nothing" "exit=$RUN_RC" "$RUN_OUT" "$GIT_CALLS"
  fi
}

test_enforced_committed_report_never_refuses() {
  local repo
  repo="$(mkrepo)"
  enforce "$repo" true
  attempt "$repo" commit_on_master_tx committed

  if [ "$RUN_RC" -eq 0 ] && [ -z "$RUN_OUT" ] && [ "$GIT_CALLS" = "$COMMON_DIR_CALL" ]; then
    pass "enforcement on: the committed report passes silently, asking only where the run registry is"
  else
    fail "enforcement on: the committed report passes silently, asking only where the run registry is" "exit=$RUN_RC" "$RUN_OUT" "$GIT_CALLS"
  fi
}

# ---- the safe-test-run journal (committed phase) ---------------------------

# The one git call the journal makes: where the run registry lives.
readonly COMMON_DIR_CALL="git rev-parse --path-format=absolute --git-common-dir"

# register REPO NAME ROOT_PID — a registered safe-test-run, as the net leaves
# one: a directory named NAME-ROOT_PID. REGISTERED is set to its id.
register() {
  REGISTERED="$2-$3"
  mkdir -p "$1/.fakegit/agent-repl-safe-test-runs/$REGISTERED"
}

journal_of() { cat "$1/.fakegit/agent-repl-safe-test-runs/$2/journal" 2>/dev/null || true; }

# A pid that is never an ancestor: the walk stops before pid 1.
readonly NOT_AN_ANCESTOR=1

test_committed_with_no_run_registered_journals_nothing() {
  local repo
  repo="$(mkrepo)"
  attempt "$repo" commit_on_feature_tx committed

  if [ "$RUN_RC" -eq 0 ] && [ -z "$RUN_OUT" ] && [ ! -e "$repo/.fakegit/agent-repl-safe-test-runs" ]; then
    pass "committed with no run registered writes nothing"
  else
    fail "committed with no run registered writes nothing" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

test_committed_by_a_marked_git_is_journaled() {
  local repo want
  repo="$(mkrepo)"
  register "$repo" run-a "$NOT_AN_ANCESTOR"
  attempt "$repo" commit_on_feature_tx committed AGENT_REPL_SAFE_TEST_RUN="$REGISTERED"
  want="$BASE $NEXT HEAD pid=$$ by=marker cwd=$repo
$BASE $NEXT refs/heads/feature pid=$$ by=marker cwd=$repo"

  if [ "$RUN_RC" -eq 0 ] && [ "$(journal_of "$repo" "$REGISTERED" | sed 's/pid=[0-9]*/pid='"$$"'/')" = "$want" ]; then
    pass "a git carrying a run's marker has every ref it moved journaled, with its pid, cwd and the marker as evidence"
  else
    fail "a git carrying a run's marker has every ref it moved journaled, with its pid, cwd and the marker as evidence" "exit=$RUN_RC" "$RUN_OUT" "$(journal_of "$repo" "$REGISTERED")"
  fi
}

test_committed_by_a_descendant_without_the_marker_is_journaled() {
  # The run's root is this harness: the hook descends from it, as a git a
  # unit spawned with a scrubbed environment still descends from the run.
  local repo
  repo="$(mkrepo)"
  register "$repo" run-a "$$"
  attempt "$repo" commit_on_feature_tx committed

  if [ "$RUN_RC" -eq 0 ] && grep_in "$(journal_of "$repo" "$REGISTERED")" -q "^$BASE $NEXT refs/heads/feature pid=[0-9]* by=ancestry cwd=$repo\$"; then
    pass "a git descending from a run's root is journaled by ancestry, with no marker"
  else
    fail "a git descending from a run's root is journaled by ancestry, with no marker" "exit=$RUN_RC" "$RUN_OUT" "$(journal_of "$repo" "$REGISTERED")"
  fi
}

test_committed_by_a_foreign_git_is_not_journaled() {
  local repo
  repo="$(mkrepo)"
  register "$repo" run-a "$NOT_AN_ANCESTOR"
  attempt "$repo" commit_on_feature_tx committed

  if [ "$RUN_RC" -eq 0 ] && [ -z "$RUN_OUT" ] && [ -z "$(journal_of "$repo" "$REGISTERED")" ]; then
    pass "a git with neither the marker nor the run's root as an ancestor is never journaled"
  else
    fail "a git with neither the marker nor the run's root as an ancestor is never journaled" "exit=$RUN_RC" "$RUN_OUT" "$(journal_of "$repo" "$REGISTERED")"
  fi
}

test_another_runs_marker_journals_only_that_run() {
  local repo
  repo="$(mkrepo)"
  register "$repo" run-a "$NOT_AN_ANCESTOR"
  register "$repo" run-b "$NOT_AN_ANCESTOR"
  attempt "$repo" commit_on_feature_tx committed AGENT_REPL_SAFE_TEST_RUN="$REGISTERED"

  if [ -z "$(journal_of "$repo" run-a-1)" ] && [ -n "$(journal_of "$repo" run-b-1)" ]; then
    pass "a marked git is journaled in its own run only, never in a concurrent one"
  else
    fail "a marked git is journaled in its own run only, never in a concurrent one" "a=$(journal_of "$repo" run-a-1)" "b=$(journal_of "$repo" run-b-1)"
  fi
}

test_prepared_never_journals() {
  local repo
  repo="$(mkrepo)"
  register "$repo" run-a "$$"
  attempt "$repo" commit_on_feature_tx prepared AGENT_REPL_SAFE_TEST_RUN="$REGISTERED"

  if [ "$RUN_RC" -eq 0 ] && [ -z "$(journal_of "$repo" "$REGISTERED")" ]; then
    pass "the prepared phase journals nothing: only a committed move happened"
  else
    fail "the prepared phase journals nothing: only a committed move happened" "exit=$RUN_RC" "$(journal_of "$repo" "$REGISTERED")"
  fi
}

test_a_run_not_ending_in_a_pid_fails_loudly() {
  local repo
  repo="$(mkrepo)"
  mkdir -p "$repo/.fakegit/agent-repl-safe-test-runs/broken"
  attempt "$repo" commit_on_feature_tx committed

  if [ "$RUN_RC" -ne 0 ] && grep_in "$RUN_OUT" -qF "the registered run $repo/.fakegit/agent-repl-safe-test-runs/broken does not end in -<pid>" &&
    grep_in "$RUN_OUT" -q "refs/heads/feature"; then
    pass "a registered run whose name carries no root pid fails the hook loudly, naming the run and the unjudged refs"
  else
    fail "a registered run whose name carries no root pid fails the hook loudly, naming the run and the unjudged refs" "exit=$RUN_RC" "$RUN_OUT"
  fi
}

# ---- through the fake git: a whole transaction --------------------------------

# committed_repo — a fake repository with one commit, so refs have a target.
committed_repo() {
  local repo
  repo="$(mkrepo)"
  echo x >"$repo/file"
  git -C "$repo" add -A
  git -C "$repo" commit -m one
  printf '%s\n' "$repo"
}

test_a_tag_through_git_lands_in_the_journal() {
  local repo head
  repo="$(committed_repo)"
  head="$(git -C "$repo" rev-parse HEAD)"
  register "$repo" run-a "$NOT_AN_ANCESTOR"
  AGENT_REPL_SAFE_TEST_RUN="$REGISTERED" git -C "$repo" tag v1 "$head"

  if grep_in "$(journal_of "$repo" "$REGISTERED")" -q "^0000000000000000000000000000000000000000 $head refs/tags/v1 pid=[0-9]* by=marker " &&
    grep -q "^refs/tags/v1 $head\$" "$repo/.fakegit/refs"; then
    pass "a tag a run's git makes lands, and its committed transaction is journaled"
  else
    fail "a tag a run's git makes lands, and its committed transaction is journaled" "$(journal_of "$repo" "$REGISTERED")" "$(cat "$repo/.fakegit/refs" 2>/dev/null)"
  fi
}

test_a_refused_transaction_through_git_moves_nothing_and_journals_nothing() {
  local repo head rc=0 out
  repo="$(committed_repo)"
  head="$(git -C "$repo" rev-parse HEAD)"
  enforce "$repo" true
  register "$repo" run-a "$NOT_AN_ANCESTOR"
  out="$(AGENT_REPL_SAFE_TEST_RUN="$REGISTERED" git -C "$repo" update-ref refs/heads/master "$head" 2>&1)" || rc=$?

  if [ "$rc" -ne 0 ] && grep_in "$out" -q "REFUSED" && ! grep -q "refs/heads/master" "$repo/.fakegit/refs" 2>/dev/null &&
    [ -z "$(journal_of "$repo" "$REGISTERED")" ]; then
    pass "a transaction the guard refuses moves no ref and journals nothing"
  else
    fail "a transaction the guard refuses moves no ref and journals nothing" "rc=$rc" "$out" "$(journal_of "$repo" "$REGISTERED")"
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
test_enforced_aborted_report_never_refuses
test_enforced_committed_report_never_refuses
test_committed_with_no_run_registered_journals_nothing
test_committed_by_a_marked_git_is_journaled
test_committed_by_a_descendant_without_the_marker_is_journaled
test_committed_by_a_foreign_git_is_not_journaled
test_another_runs_marker_journals_only_that_run
test_prepared_never_journals
test_a_run_not_ending_in_a_pid_fails_loudly
test_a_tag_through_git_lands_in_the_journal
test_a_refused_transaction_through_git_moves_nothing_and_journals_nothing

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
