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
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_PREFIX GIT_COMMON_DIR
unset AGENT_REPL_MERGE_QUEUE AGENT_REPL_OWNER_OVERRIDE

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
HOOK_SRC="$THIS_DIR/reference-transaction"
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
  mkdir -p "$repo/.fakegit/hooks"
  cp "$HOOK_SRC" "$repo/.fakegit/hooks/reference-transaction"
  chmod +x "$repo/.fakegit/hooks/reference-transaction"
  printf '%s\n' "$repo"
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
    grep_in "$RUN_OUT" -q "/merge-queue own" &&
    grep_in "$RUN_OUT" -q "/merge-queue workspace <worktree-dir>" &&
    grep_in "$RUN_OUT" -q "/merge-queue branch <branch-name>" &&
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

test_enforced_report_phases_never_refuse() {
  # Only `prepared` can abort a transaction; `committed` and `aborted` are
  # reports of one already decided, and the hook reads nothing for them.
  local repo phase ok=1
  repo="$(mkrepo)"
  enforce "$repo" true
  for phase in committed aborted; do
    attempt "$repo" commit_on_master_tx "$phase"
    if [ "$RUN_RC" -ne 0 ] || [ -n "$RUN_OUT" ] || [ -n "$GIT_CALLS" ]; then ok=0; fi
  done

  if [ "$ok" -eq 1 ]; then
    pass "enforcement on: the committed and aborted reports pass silently and read nothing"
  else
    fail "enforcement on: the committed and aborted reports pass silently and read nothing" "exit=$RUN_RC" "$RUN_OUT"
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
test_enforced_report_phases_never_refuse

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
