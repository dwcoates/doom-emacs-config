#!/usr/bin/env bash
# safe-test-run.sh — run the claude-repl ERT suite with a git-state safety net.
#
# Captures HEAD + refs/heads/* + working-tree status before invoking the
# suite, drops a timestamped checkpoint tag at HEAD, then re-captures the
# same state after the suite finishes.  On any drift the checkpoint tag
# is LEFT IN PLACE and the user is told exactly how to roll back; on a
# clean run the tag is auto-removed.
#
# Why this exists: the claude-repl runtime guards catch unmocked-WRAPPER
# call paths, but a raw `(shell-command-to-string "git ...")` in
# production code that bypasses the registered wrappers entirely is NOT
# automatically detected — see AGENTS.md "How the rule is enforced".
# This wrapper does not change that primary obligation; it just makes
# the consequence of a missed audit revertible by a single command
# instead of by manual archeology.
#
# Usage:
#   .claude/safe-test-run.sh [SELECTOR]
#
# SELECTOR is an optional ERT selector (a regex matching test names).
# Defaults to running the full suite via test-claude-repl.el.
#
# Exit codes:
#   0   — tests passed AND no git drift
#   1   — tests failed (exit code propagated from ert)
#   2   — tests passed BUT git state drifted (checkpoint left for rollback)
#   3   — environment error (not a git repo, missing files, etc.)
#
# Designed for manual invocation and (eventually) CI; no interactive
# prompts.
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [ -z "$REPO_ROOT" ]; then
  echo "[safe-test-run] FATAL: not inside a git repo" >&2
  exit 3
fi
cd "$REPO_ROOT"

TEST_FILE="modules/app/claude-repl/test-claude-repl.el"
if [ ! -f "$TEST_FILE" ]; then
  echo "[safe-test-run] FATAL: $TEST_FILE not found" >&2
  exit 3
fi

SELECTOR="${1:-}"

# ---- Capture pre-test state ------------------------------------------------

PRE_HEAD="$(git rev-parse HEAD)"
PRE_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
PRE_REFS="$(git for-each-ref --format='%(refname) %(objectname)' refs/heads/ refs/tags/)"
PRE_REFS_HASH="$(printf '%s' "$PRE_REFS" | shasum -a 256 | cut -d' ' -f1)"
PRE_STATUS="$(git status --porcelain)"

CHECKPOINT_TAG="claude-repl-test-checkpoint-$(date +%Y%m%d-%H%M%S)-$$"
git tag "$CHECKPOINT_TAG" "$PRE_HEAD"

echo "[safe-test-run] Checkpoint tag: $CHECKPOINT_TAG @ $PRE_HEAD"
echo "[safe-test-run] Pre-state: branch=$PRE_BRANCH, refs-hash=${PRE_REFS_HASH:0:12}"
echo ""

# ---- Run the suite ---------------------------------------------------------

# When a SELECTOR is provided pass it as the argument to
# `ert-run-tests-batch-and-exit'.  When omitted, ERT runs every
# registered test.
EMACS_INVOKE=(emacs -batch -Q -l ert -l "$TEST_FILE")
if [ -n "$SELECTOR" ]; then
  EMACS_INVOKE+=(--eval "(ert-run-tests-batch-and-exit \"$SELECTOR\")")
else
  EMACS_INVOKE+=(-f ert-run-tests-batch-and-exit)
fi

echo "[safe-test-run] Running: ${EMACS_INVOKE[*]}"
echo ""

set +e
"${EMACS_INVOKE[@]}"
TEST_RC=$?
set -e

echo ""
echo "[safe-test-run] ert exited with code $TEST_RC"

# ---- Capture post-test state ----------------------------------------------

POST_HEAD="$(git rev-parse HEAD)"
POST_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
POST_REFS="$(git for-each-ref --format='%(refname) %(objectname)' refs/heads/ refs/tags/ | grep -v "^refs/tags/$CHECKPOINT_TAG ")"
POST_REFS_HASH="$(printf '%s' "$POST_REFS" | shasum -a 256 | cut -d' ' -f1)"
POST_STATUS="$(git status --porcelain | grep -v "scheduled_tasks.lock" || true)"
PRE_STATUS_FILTERED="$(printf '%s' "$PRE_STATUS" | grep -v "scheduled_tasks.lock" || true)"

# ---- Detect drift ----------------------------------------------------------

DRIFT=0
DRIFT_REPORT=""

if [ "$PRE_HEAD" != "$POST_HEAD" ]; then
  DRIFT=1
  DRIFT_REPORT="${DRIFT_REPORT}  - HEAD changed: $PRE_HEAD -> $POST_HEAD\n"
fi

if [ "$PRE_BRANCH" != "$POST_BRANCH" ]; then
  DRIFT=1
  DRIFT_REPORT="${DRIFT_REPORT}  - Active branch changed: $PRE_BRANCH -> $POST_BRANCH\n"
fi

if [ "$PRE_REFS_HASH" != "$POST_REFS_HASH" ]; then
  DRIFT=1
  ADDED_REFS="$(diff <(printf '%s\n' "$PRE_REFS") <(printf '%s\n' "$POST_REFS") | awk '/^>/{$1=""; print}' | sed 's/^ //')"
  REMOVED_REFS="$(diff <(printf '%s\n' "$PRE_REFS") <(printf '%s\n' "$POST_REFS") | awk '/^</{$1=""; print}' | sed 's/^ //')"
  DRIFT_REPORT="${DRIFT_REPORT}  - refs/heads or refs/tags changed:\n"
  if [ -n "$ADDED_REFS" ]; then
    DRIFT_REPORT="${DRIFT_REPORT}      added:\n"
    while IFS= read -r line; do [ -n "$line" ] && DRIFT_REPORT="${DRIFT_REPORT}        + $line\n"; done <<< "$ADDED_REFS"
  fi
  if [ -n "$REMOVED_REFS" ]; then
    DRIFT_REPORT="${DRIFT_REPORT}      removed:\n"
    while IFS= read -r line; do [ -n "$line" ] && DRIFT_REPORT="${DRIFT_REPORT}        - $line\n"; done <<< "$REMOVED_REFS"
  fi
fi

if [ "$PRE_STATUS_FILTERED" != "$POST_STATUS" ]; then
  DRIFT=1
  DRIFT_REPORT="${DRIFT_REPORT}  - Working tree status changed (see 'git status' for detail).\n"
fi

# ---- Report + cleanup ------------------------------------------------------

echo ""
if [ "$DRIFT" -eq 0 ]; then
  echo "[safe-test-run] OK: zero git-state drift across the test run."
  git tag -d "$CHECKPOINT_TAG" >/dev/null
  echo "[safe-test-run] Checkpoint $CHECKPOINT_TAG auto-removed."
  if [ "$TEST_RC" -ne 0 ]; then
    echo "[safe-test-run] (Tests themselves failed; see ert output above.)"
    exit "$TEST_RC"
  fi
  echo "[safe-test-run] DONE — exit 0"
  exit 0
fi

echo "[safe-test-run] DRIFT DETECTED:"
printf '%b' "$DRIFT_REPORT" >&2
echo ""
echo "[safe-test-run] Checkpoint tag preserved: $CHECKPOINT_TAG @ $PRE_HEAD" >&2
echo "[safe-test-run] To roll back the worktree + HEAD to pre-test state:" >&2
echo "[safe-test-run]     git reset --hard $CHECKPOINT_TAG" >&2
echo "[safe-test-run] To accept the drift and discard the checkpoint:" >&2
echo "[safe-test-run]     git tag -d $CHECKPOINT_TAG" >&2

if [ "$TEST_RC" -ne 0 ]; then
  echo "[safe-test-run] (Tests also failed — see ert output above.)" >&2
  # Surface the more severe condition (test failure) over the warning.
  exit "$TEST_RC"
fi
exit 2
