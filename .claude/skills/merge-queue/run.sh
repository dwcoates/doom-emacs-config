#!/usr/bin/env bash
# run.sh — the merge-queue skill's driver.
#
# Verbs:
#   --enqueue-own                Enqueue the merge of the workspace this shell
#                                is inside. Returns once the merge is in the
#                                queue; it runs when the caller's turn ends.
#   --land-workspace <dir>       Enqueue ANOTHER workspace's merge and wait for
#                                its outcome.
#   --land-branch <branch>       Enqueue a branch that is no workspace (a
#                                subagent's) and wait for its outcome.
#
# Exit codes (every verb):
#   0  landed (--land-*), or in the queue (--enqueue-own)
#   2  script/usage error, or the merge could not be enqueued
#   3  parked awaiting guidance; the parked line is printed
#   4  failed; the reason is printed
#   5  refused by the daemon; how to read why is printed
#   6  the workspace has uncommitted work (--enqueue-own)
#   7  a -wait on the caller's own workspace was refused
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

# failure_reason prints the daemon's recorded reason for a failed merge of dir.
failure_reason() {
  local dir="$1" main logs
  main="$(main_worktree)" || die "not inside a git repository"
  logs="$main/modules/app/agent-repl/bin/logs.sh"
  [ -x "$logs" ] || die "the log reader $logs is missing"
  command -v jq >/dev/null 2>&1 || die "jq is required to read the failure's reason"
  log "the daemon's reason:"
  "$logs" --workspace "$dir" --since 6h --json 2>/dev/null |
    jq -r 'select(.operation == "daemon.merge.abort") | .context.summary' | tail -n 1
}

# run_verb runs claude-repld merge-queue, passing its output through, and maps
# its exit onto this script's. A failure's reason is read from the log of the
# worktree the verb named.
run_verb() {
  local bin rc out dir
  bin="$(daemon_bin)" || exit 2
  out="$(mktemp "${TMPDIR:-/tmp}/agent-repl-merge-queue-out.XXXXXX")" || die "could not create a temp file"
  "$bin" merge-queue "$@" | tee "$out"
  rc=${PIPESTATUS[0]}
  dir="$(sed -n 's/^merge-queue: worktree: //p' "$out" | head -n 1)"
  rm -f "$out"
  case "$rc" in
    0) exit 0 ;;
    4) exit 3 ;;
    5)
      [ -n "$dir" ] || die "the merge failed, and the verb named no worktree to read the reason from"
      failure_reason "$dir"
      exit 4
      ;;
    6) exit 5 ;;
    *) exit 2 ;;
  esac
}

case "${1:-}" in
  --enqueue-own)
    top="$(git rev-parse --show-toplevel 2>/dev/null)" || die "not inside a git worktree"
    main="$(main_worktree)" || die "not inside a git repository"
    [ "$top" != "$main" ] || die "$top is the repository's main worktree, not a workspace; use --land-branch for a branch"
    if [ -n "$(git -C "$top" status --porcelain 2>/dev/null)" ]; then
      log "$top has uncommitted work; commit it before enqueueing"
      exit 6
    fi
    run_verb -dir "$top"
    ;;
  --land-workspace)
    [ -n "${2:-}" ] || die "--land-workspace needs the workspace's worktree directory"
    top="$(git rev-parse --show-toplevel 2>/dev/null || true)"
    if [ -n "$top" ] && [ "$(cd "$2" 2>/dev/null && pwd -P)" = "$(cd "$top" && pwd -P)" ]; then
      log "$2 is this shell's own workspace; enqueue it with --enqueue-own and end the turn"
      exit 7
    fi
    run_verb -dir "$2" -wait
    ;;
  --land-branch)
    [ -n "${2:-}" ] || die "--land-branch needs a branch name"
    main="$(main_worktree)" || die "not inside a git repository"
    run_verb -branch "$2" -repo "$main" -wait
    ;;
  *)
    printf 'usage: run.sh --enqueue-own | --land-workspace <dir> | --land-branch <branch>\n' >&2
    exit 1
    ;;
esac
