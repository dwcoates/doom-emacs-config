#!/usr/bin/env bash
# run.sh — the merge-queue skill's driver.
#
# Every verb asks for a merge FROM the workspace this shell is inside. The
# merge is put in line once the caller's current turn ends, runs in that
# workspace, and reports its outcome into that workspace's session; no verb
# waits for the outcome.
#
# Verbs:
#   --enqueue-own [--keep-open]  Merge this workspace's own branch. The
#                                workspace closes once it lands, unless
#                                --keep-open.
#   --land-workspace <dir>       Merge ANOTHER workspace's branch, named by its
#                                worktree. That workspace closes once it lands.
#   --land-branch <branch>       Merge a branch that is no workspace (a
#                                subagent's).
#   --pr-merged                  This workspace's branch already merged
#                                upstream: update the default branch and close
#                                this workspace.
#   --remove-branch <branch>     After a --land-branch merge LANDED: remove the
#                                branch's worktree (when it has one) and the
#                                branch, refusing a branch master lacks.
#
# Exit codes (every verb):
#   0  requested (end the turn; the outcome reports into this session), or
#      removed (--remove-branch)
#   2  script/usage error, or the merge could not be requested
#   5  refused by the daemon; how to read why is printed
#   6  the workspace has uncommitted work (--enqueue-own)
#   7  --land-workspace named this shell's own workspace
set -uo pipefail

die() {
  printf 'merge-queue: %s\n' "$*" >&2
  exit 2
}

log() {
  printf 'merge-queue: %s\n' "$*"
}

# main_worktree answers the repository's main worktree: the first entry git
# lists.
main_worktree() {
  local line
  line="$(git worktree list --porcelain 2>/dev/null | head -n 1)" || return 1
  [ "${line#worktree }" != "$line" ] || return 1
  printf '%s\n' "${line#worktree }"
}

# daemon_bin answers the claude-repld that serves this host: the override, or
# the one built in the repository's main worktree.
daemon_bin() {
  local bin main
  if [ -n "${AGENT_REPL_DAEMON_BIN:-}" ]; then
    bin="$AGENT_REPL_DAEMON_BIN"
  else
    main="$(main_worktree)" || die "not inside a git repository"
    bin="$main/modules/app/agent-repl/daemon/bin/claude-repld"
  fi
  [ -x "$bin" ] || die "the daemon binary $bin is missing or not executable"
  printf '%s\n' "$bin"
}

# run_verb runs claude-repld merge-queue, passing its output through, and maps
# its exit onto this script's.
run_verb() {
  local bin rc
  bin="$(daemon_bin)" || exit 2
  "$bin" merge-queue "$@"
  rc=$?
  case "$rc" in
    0) exit 0 ;;
    6) exit 5 ;;
    *) exit 2 ;;
  esac
}

case "${1:-}" in
  --enqueue-own)
    keep=()
    case "${2:-}" in
      "") ;;
      --keep-open) keep=(-keep-open) ;;
      *) die "--enqueue-own takes only --keep-open, not $2" ;;
    esac
    top="$(git rev-parse --show-toplevel 2>/dev/null)" || die "not inside a git worktree"
    main="$(main_worktree)" || die "not inside a git repository"
    [ "$top" != "$main" ] || die "$top is the repository's main worktree, not a workspace; use --land-branch for a branch"
    if [ -n "$(git -C "$top" status --porcelain 2>/dev/null)" ]; then
      log "$top has uncommitted work; commit it before enqueueing"
      exit 6
    fi
    run_verb -own "${keep[@]+"${keep[@]}"}"
    ;;
  --land-workspace)
    [ -n "${2:-}" ] || die "--land-workspace needs the workspace's worktree directory"
    top="$(git rev-parse --show-toplevel 2>/dev/null || true)"
    if [ -n "$top" ] && [ "$(cd "$2" 2>/dev/null && pwd -P)" = "$(cd "$top" && pwd -P)" ]; then
      log "$2 is this shell's own workspace; enqueue it with --enqueue-own and end the turn"
      exit 7
    fi
    run_verb -dir "$2"
    ;;
  --land-branch)
    [ -n "${2:-}" ] || die "--land-branch needs a branch name"
    run_verb -branch "$2"
    ;;
  --pr-merged)
    run_verb -pr-merged
    ;;
  --remove-branch)
    [ -n "${2:-}" ] || die "--remove-branch needs a branch name"
    main="$(main_worktree)" || die "not inside a git repository"
    wt="$(git -C "$main" worktree list --porcelain 2>/dev/null |
      awk -v want="branch refs/heads/$2" '/^worktree /{dir=substr($0,10)} $0==want{print dir}')"
    if [ -n "$wt" ]; then
      git -C "$main" worktree remove "$wt" || die "could not remove $2's worktree $wt"
      log "removed $2's worktree $wt"
    fi
    git -C "$main" branch -d "$2" || die "could not delete $2; it may not have landed on master"
    log "deleted the branch $2"
    ;;
  *)
    printf 'usage: run.sh --enqueue-own [--keep-open] | --land-workspace <dir> | --land-branch <branch> | --pr-merged | --remove-branch <branch>\n' >&2
    exit 1
    ;;
esac
