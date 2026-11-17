#!/usr/bin/env bash
# safe-test-run.sh — run the agent-repl ERT suite with a git-state safety net.
#
# Drops a timestamped checkpoint tag at HEAD before invoking the suite.
# Every ref the run's OWN git processes move, anywhere in the repository,
# HEAD included, is drift: the reference-transaction hook journals them (see
# "Ref drift" below).  On any drift the checkpoint tag is LEFT IN PLACE and
# the user is told exactly how to roll back; on a clean run the tag is
# auto-removed.
#
# Why this exists: the agent-repl runtime guards catch unmocked-WRAPPER
# call paths, but a raw `(shell-command-to-string "git ...")` in
# production code that bypasses the registered wrappers entirely is NOT
# automatically detected — see AGENTS.md "How the rule is enforced".
# This wrapper does not change that primary obligation; it just makes
# the consequence of a missed audit revertible by a single command
# instead of by manual archeology.
#
# NOTHING HERE COMPARES THE CHECKOUT BEFORE AND AFTER THE RUN.  A snapshot
# of HEAD, the branch or `git status` cannot say WHO changed them, and the
# workspace's own agent edits and commits while its tests run: every drift
# such a comparison ever reported was that, or another false alarm, and
# never a test (owner ruling 2026-11-17).  A test that runs real git is
# fixed to use mocked git instead, wherever it is found (the
# agent-repl-tests skill); what a run's own git does to refs is journaled
# and attributed exactly.
#
# Usage:
#   .claude/safe-test-run.sh [SELECTOR]
#   .claude/safe-test-run.sh -- COMMAND [ARGS...]
#
# SELECTOR is an optional ERT selector (a regex matching test names).
# Defaults to running the full suite via test-agent-repl.el.
#
# `--` wraps an arbitrary COMMAND in the same before/after git-state check
# instead of the ERT suite.  bin/test-all.sh runs its whole parallel run
# this way, so every suite's units (not only ERT's) sit inside the net.
#
# Exit codes:
#   0   — tests passed AND no git drift
#   1   — tests failed (exit code propagated from ert)
#   2   — tests passed BUT git state drifted (checkpoint left for rollback)
#   3   — environment error (not a git repo, missing files, the
#         reference-transaction hook not journaling, etc.)
#
# Designed for manual invocation and (eventually) CI; no interactive
# prompts.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || true)"
if [ -z "$REPO_ROOT" ]; then
  echo "[safe-test-run] FATAL: not inside a git repo" >&2
  exit 3
fi
cd "$REPO_ROOT"

TEST_FILE="modules/app/agent-repl/lisp/test-agent-repl.el"
if [ ! -f "$TEST_FILE" ]; then
  echo "[safe-test-run] FATAL: $TEST_FILE not found" >&2
  exit 3
fi

SELECTOR="${1:-}"
WRAPPED=()
if [ "$SELECTOR" = "--" ]; then
  shift
  if [ "$#" -eq 0 ]; then
    echo "[safe-test-run] FATAL: -- needs a command to wrap" >&2
    exit 3
  fi
  WRAPPED=("$@")
  SELECTOR=""
fi

# ---- Ref drift ---------------------------------------------------------------
# The repository's refs are shared with every linked worktree, and other
# sessions legitimately commit, rebase, create and delete branches while a
# suite runs. So ref drift is never inferred from a before/after snapshot of
# the shared refs, which cannot tell who moved a ref: it is the set of refs
# this run's OWN git processes moved, as .githooks/reference-transaction
# journals them in its committed phase. The run registers itself as
# REGISTRY/RUN_ID, one mkdir, where RUN_ID ends in this script's pid, before
# anything else; the hook journals a ref there only for a git that carries
# AGENT_REPL_SAFE_TEST_RUN=RUN_ID (exported to the wrapped command) or descends
# from that pid. Another session's git is neither, so its activity can never
# fail a run, while a ref the run moves is caught whatever its name. A canary
# ref the run moves itself proves the journal is live before the suite starts;
# a hook that is not installed is an environment error, never a blind pass.

GIT_COMMON_DIR="$(git rev-parse --path-format=absolute --git-common-dir)"
REGISTRY="$GIT_COMMON_DIR/agent-repl-safe-test-runs"
RUN_ID="$(date +%Y%m%d-%H%M%S)-$$"
RUN_DIR="$REGISTRY/$RUN_ID"
CANARY_REF="refs/agent-repl-safe-test-run-canary/$RUN_ID"
CHECKPOINT_TAG="agent-repl-test-checkpoint-$RUN_ID"

# prune_gone_runs removes the registration of every run whose script is no
# longer alive (killed before its exit trap ran), so the hook stops scanning
# it. The pid comes from the run's name, so a concurrent run's registration
# or removal can never be caught half done.
prune_gone_runs() {
  local run pid
  [ -d "$REGISTRY" ] || return 0
  for run in "$REGISTRY"/*; do
    [ -d "$run" ] || continue
    pid="${run##*-}"
    if ! kill -0 "$pid" 2>/dev/null; then
      rm -rf "$run"
      echo "[safe-test-run] Removed the registration of a run that is gone: $run (root pid $pid)"
    fi
  done
}

# prove_journal moves the canary ref as the wrapped command's git would, and
# refuses to run the suite unless the hook journaled it.
prove_journal() {
  local head journaled
  head="$(git rev-parse HEAD)"
  AGENT_REPL_SAFE_TEST_RUN="$RUN_ID" git update-ref "$CANARY_REF" "$head"
  journaled="$(awk -v ref="$CANARY_REF" '$3 == ref' "$RUN_DIR/journal")"
  AGENT_REPL_SAFE_TEST_RUN="$RUN_ID" git update-ref -d "$CANARY_REF"
  if [ -z "$journaled" ]; then
    echo "[safe-test-run] FATAL: the reference-transaction hook did not journal this run's canary ref $CANARY_REF," >&2
    echo "[safe-test-run] so no ref the run moves could be attributed to it. Install .githooks/reference-transaction:" >&2
    echo "[safe-test-run] core.hooksPath set to .githooks, or modules/app/agent-repl/bin/requirements.sh (requirement reference-transaction-hook)." >&2
    exit 3
  fi
}

# run_ref_moves prints the journal's real moves, the run's own bookkeeping
# refs (the canary, the checkpoint tag) left out.
run_ref_moves() {
  awk -v canary="$CANARY_REF" -v tag="refs/tags/$CHECKPOINT_TAG" \
    '$3 != canary && $3 != tag && $1 != $2' "$RUN_DIR/journal"
}

prune_gone_runs
mkdir -p "$REGISTRY"
mkdir "$RUN_DIR"
trap 'rm -rf "$RUN_DIR"' EXIT
: >"$RUN_DIR/journal"
prove_journal

# ---- Capture pre-test state ------------------------------------------------

PRE_HEAD="$(git rev-parse HEAD)"

git tag "$CHECKPOINT_TAG" "$PRE_HEAD"

echo "[safe-test-run] Checkpoint tag: $CHECKPOINT_TAG @ $PRE_HEAD"
echo "[safe-test-run] Ref journal: $RUN_DIR/journal"
echo ""

# ---- Run the suite ---------------------------------------------------------

# When a SELECTOR is provided pass it as the argument to
# `ert-run-tests-batch-and-exit'.  When omitted, ERT runs every
# registered test.
EMACS_INVOKE=(emacs -batch -Q -l ert -l "$TEST_FILE")
if [ "${#WRAPPED[@]}" -gt 0 ]; then
  EMACS_INVOKE=("${WRAPPED[@]}")
elif [ -n "$SELECTOR" ]; then
  EMACS_INVOKE+=(--eval "(ert-run-tests-batch-and-exit \"$SELECTOR\")")
else
  EMACS_INVOKE+=(-f ert-run-tests-batch-and-exit)
fi

echo "[safe-test-run] Running: ${EMACS_INVOKE[*]}"
echo ""

set +e
AGENT_REPL_SAFE_TEST_RUN="$RUN_ID" "${EMACS_INVOKE[@]}"
TEST_RC=$?
set -e

echo ""
echo "[safe-test-run] ${EMACS_INVOKE[0]} exited with code $TEST_RC"

# ---- Capture post-test state ----------------------------------------------

RUN_REF_MOVES="$(run_ref_moves)"

# ---- Detect drift ----------------------------------------------------------

DRIFT=0
DRIFT_REPORT=""

if [ -n "$RUN_REF_MOVES" ]; then
  DRIFT=1
  DRIFT_REPORT="${DRIFT_REPORT}  - refs this run's own git moved (<old> <new> <ref>, the git's pid, how it was attributed, its cwd):\n"
  while IFS= read -r line; do DRIFT_REPORT="${DRIFT_REPORT}        $line\n"; done <<< "$RUN_REF_MOVES"
  # Each ref back to the value it held before the run's first move of it.
  RESTORE="$(awk '$3 != "HEAD" && !seen[$3]++ {
      if ($1 ~ /^0+$/) print "git update-ref -d " $3; else print "git update-ref " $3 " " $1 }' <<< "$RUN_REF_MOVES")"
fi

# ---- Report + cleanup ------------------------------------------------------

echo ""
if [ "$DRIFT" -eq 0 ]; then
  echo "[safe-test-run] OK: zero git-state drift across the test run."
  git tag -d "$CHECKPOINT_TAG" >/dev/null
  echo "[safe-test-run] Checkpoint $CHECKPOINT_TAG auto-removed."
  if [ "$TEST_RC" -ne 0 ]; then
    echo "[safe-test-run] (The command itself failed; see its output above.)"
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
if [ -n "${RESTORE:-}" ]; then
  echo "[safe-test-run] To put back each other ref the run moved:" >&2
  while IFS= read -r line; do echo "[safe-test-run]     $line" >&2; done <<< "$RESTORE"
fi
echo "[safe-test-run] To accept the drift and discard the checkpoint:" >&2
echo "[safe-test-run]     git tag -d $CHECKPOINT_TAG" >&2

if [ "$TEST_RC" -ne 0 ]; then
  echo "[safe-test-run] (The command also failed; see its output above.)" >&2
  # Surface the more severe condition (test failure) over the warning.
  exit "$TEST_RC"
fi
exit 2
