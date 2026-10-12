#!/usr/bin/env bash
# run.sh — the merge-queue skill's driver.
#
# Every merge verb asks for a merge FROM the workspace this shell is inside.
# The merge is put in line once the caller's current turn ends, runs in that
# workspace, and reports its outcome into that workspace's session; no merge
# verb waits for the outcome.
#
# The queue-control verbs (--dequeue-*, --pause-queue, --resume-queue,
# --hold-rollouts, --release-rollouts) ask,
# from the workspace this shell is inside, for the queue itself to change, and
# wait (bounded) for the answer, which they print: the outcome when applied, the
# refusal's cause when refused.
#
# Verbs:
#   --enqueue-own [--in-place | --keep-open]
#                                Merge this workspace's own branch. By
#                                default it runs detached, in a merge
#                                workspace forked from this one, and this
#                                workspace stays free. --in-place runs it in
#                                this workspace, which closes once it lands;
#                                --keep-open runs it in place and keeps this
#                                workspace open.
#   --announce-one-shot          Announce, from inside the current turn, that
#                                this workspace one-shot merges once the turn
#                                ends: its own branch in place, this workspace
#                                closed once it lands, and this workspace
#                                marked a one-shot now. Uncommitted work is not
#                                refused here: the turn goes on, and the merge
#                                itself refuses a dirty tree when it starts.
#   --land-workspace <dir>       Merge ANOTHER workspace's branch, named by its
#                                worktree. That workspace closes once it lands.
#   --land-branch <branch>       Merge a branch that is no workspace (a
#                                subagent's).
#   --land-commits <commit>...   Merge ONLY the named commits of this
#                                workspace's branch: they are cherry-picked, in
#                                branch order, onto a new branch off the main
#                                worktree's branch (in a worktree of its own
#                                beside this one), which is then requested as
#                                --land-branch requests a branch. On a conflict
#                                or a refusal nothing built is left behind.
#   --pr-merged                  This workspace's branch already merged
#                                upstream: update the default branch and close
#                                this workspace.
#   --remove-branch <branch>     After a --land-branch merge LANDED: remove the
#                                branch's worktree (when it has one) and the
#                                branch, refusing a branch master lacks.
#   --dequeue-own                Take this workspace's own merge off the queue.
#   --dequeue-workspace <dir>    Take ANOTHER workspace's merge off the queue,
#                                named by its worktree.
#   --pause-queue [<repo-dir>]   Pause the queue of the repository whose main
#                                checkout is <repo-dir>, else of every
#                                repository.
#   --resume-queue [<repo-dir>]  Resume a paused queue, scoped as a pause is.
#   --hold-rollouts              Hold the queue's ROLLOUTS, daemon-wide: merges
#                                still land, but a landing in the daemon's own
#                                checkout no longer redeploys, hot-reloads or
#                                bounces the runtime. Durable until released.
#   --release-rollouts           End the hold; the next landing redeploys.
#
# Modifier:
#   --front                      With any merge verb (--enqueue-own,
#                                --announce-one-shot, --land-workspace,
#                                --land-branch, --land-commits, --pr-merged),
#                                anywhere among
#                                its arguments: the merge goes to the FRONT of
#                                its queue once it is put in line, ahead of
#                                every ordinary merge waiting, never ahead of
#                                the one running. Refused on any other verb.
#
# Exit codes (every verb):
#   0  requested (end the turn; the outcome reports into this session),
#      announced (--announce-one-shot; the turn goes on),
#      removed (--remove-branch), or applied (queue-control verbs; the printed
#      outcome line is evicted, not_queued, paused, resumed, held or
#      released, or unread)
#   2  script/usage error, or the merge or control could not be requested
#   5  refused by the daemon; why (or how to read why) is printed
#   6  the workspace has uncommitted work (--enqueue-own)
#   7  --land-workspace named this shell's own workspace
#   9  a named commit did not apply cleanly onto the main worktree's branch
#      (--land-commits); nothing was built or requested
#   8  the daemon did not answer a queue-control request in time; the request
#      stays pending (queue-control verbs)

set -uo pipefail

# shellcheck source=../../../modules/app/agent-repl/bin/lib-worktrees.sh
. "$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../../../modules/app/agent-repl/bin/lib-worktrees.sh"

die() {
  printf 'merge-queue: %s\n' "$*" >&2
  exit 2
}

log() {
  printf 'merge-queue: %s\n' "$*"
}

# repo_main_worktree answers the repository's main worktree: the first entry
# git lists (lib-worktrees.sh).
repo_main_worktree() {
  main_worktree . 2>/dev/null
}

# workspace_top answers the worktree this shell is inside, refusing the
# repository's main worktree: a merge verb acts FROM a workspace.
workspace_top() {
  local top main
  top="$(git rev-parse --show-toplevel 2>/dev/null)" || die "not inside a git worktree"
  main="$(repo_main_worktree)" || die "not inside a git repository"
  [ "$top" != "$main" ] || die "$top is the repository's main worktree, not a workspace; use --land-branch for a branch"
  printf '%s\n' "$top"
}

# daemon_bin answers the agent-repld that serves this host: the override, or
# the one built in the repository's main worktree.
daemon_bin() {
  local bin main
  if [ -n "${AGENT_REPL_DAEMON_BIN:-}" ]; then
    bin="$AGENT_REPL_DAEMON_BIN"
  else
    main="$(repo_main_worktree)" || die "not inside a git repository"
    bin="$main/modules/app/agent-repl/daemon/bin/agent-repld"
  fi
  [ -x "$bin" ] || die "the daemon binary $bin is missing or not executable"
  printf '%s\n' "$bin"
}

# FRONT is the merge-queue verb's -front, set when --front is among the
# arguments; it rides every merge request this script makes.
FRONT=()
ARGS=()
for arg in "$@"; do
  if [ "$arg" = --front ]; then
    FRONT=(-front)
  else
    ARGS+=("$arg")
  fi
done
set -- "${ARGS[@]+"${ARGS[@]}"}"

# no_front refuses --front on a verb that requests no merge.
no_front() {
  [ "${#FRONT[@]}" -eq 0 ] || die "--front goes with a merge verb; $1 requests no merge"
}

# verb_status runs agent-repld merge-queue, passing its output through, and
# answers its exit mapped onto this script's. A merge request carries -front
# when --front was given.
verb_status() {
  local bin rc
  bin="$(daemon_bin)" || return 2
  "$bin" merge-queue "$@" "${FRONT[@]+"${FRONT[@]}"}"
  rc=$?
  case "$rc" in
    0) return 0 ;;
    6) return 5 ;;
    7) return 8 ;;
    *) return 2 ;;
  esac
}

# run_verb runs agent-repld merge-queue and exits with its mapped status.
run_verb() {
  verb_status "$@"
  exit $?
}

# drop_built removes a --land-commits branch and its worktree, so a conflict or
# a refusal leaves nothing behind.
drop_built() {
  git -C "$1" worktree remove --force "$2" || die "could not remove the built worktree $2"
  git -C "$1" branch -D "$3" || die "could not delete the built branch $3"
}

case "${1:-}" in
  --enqueue-own)
    keep=()
    case "${2:-}" in
      "") ;;
      --keep-open) keep=(-keep-open) ;;
      --in-place) keep=(-in-place) ;;
      *) die "--enqueue-own takes only --in-place or --keep-open, not $2" ;;
    esac
    top="$(workspace_top)" || exit 2
    if [ -n "$(git -C "$top" status --porcelain 2>/dev/null)" ]; then
      log "$top has uncommitted work; commit it before enqueueing"
      exit 6
    fi
    run_verb -own "${keep[@]+"${keep[@]}"}"
    ;;
  --announce-one-shot)
    [ -z "${2:-}" ] || die "--announce-one-shot takes no argument, not $2"
    top="$(workspace_top)" || exit 2
    run_verb -own -one-shot
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
  --land-commits)
    shift
    [ "$#" -gt 0 ] || die "--land-commits needs at least one commit"
    top="$(workspace_top)" || exit 2
    main="$(repo_main_worktree)" || die "not inside a git repository"
    target="$(git -C "$main" rev-parse --abbrev-ref HEAD)" || die "could not read the main worktree's branch"
    own="$(git -C "$top" rev-parse --abbrev-ref HEAD)" || die "could not read this workspace's branch"
    wanted=()
    for commit in "$@"; do
      full="$(git -C "$top" rev-parse --verify "$commit^{commit}")" || die "$commit names no commit"
      git -C "$top" merge-base --is-ancestor "$full" HEAD || die "$commit is not on this workspace's branch $own"
      if git -C "$top" merge-base --is-ancestor "$full" "$target"; then
        die "$commit is already on $target"
      fi
      wanted+=("$full")
    done
    ordered=()
    while IFS= read -r full; do
      for w in "${wanted[@]}"; do
        if [ "$w" = "$full" ]; then
          ordered+=("$full")
          break
        fi
      done
    done < <(git -C "$top" rev-list --reverse "$target..HEAD")
    [ "${#ordered[@]}" -eq "${#wanted[@]}" ] || die "could not order the named commits along $own"
    last="${ordered[${#ordered[@]}-1]}"
    branch="$own-partial-${last:0:9}"
    dir="$(dirname "$top")/${branch//\//-}"
    git -C "$main" worktree add -b "$branch" "$dir" "$target" || die "could not create $branch in $dir"
    if ! git -C "$dir" cherry-pick "${ordered[@]}"; then
      git -C "$dir" cherry-pick --abort || die "could not abort the cherry-pick in $dir"
      drop_built "$main" "$dir" "$branch"
      log "the named commits did not apply cleanly onto $target; nothing was requested"
      exit 9
    fi
    log "built $branch in $dir from ${#ordered[@]} commit(s) of $own"
    verb_status -branch "$branch"
    rc=$?
    [ "$rc" -eq 0 ] || drop_built "$main" "$dir" "$branch"
    exit "$rc"
    ;;
  --pr-merged)
    run_verb -pr-merged
    ;;
  --remove-branch)
    no_front "$1"
    [ -n "${2:-}" ] || die "--remove-branch needs a branch name"
    main="$(repo_main_worktree)" || die "not inside a git repository"
    wt="$(git -C "$main" worktree list --porcelain 2>/dev/null |
      awk -v want="branch refs/heads/$2" '/^worktree /{dir=substr($0,10)} $0==want{print dir}')"
    if [ -n "$wt" ]; then
      git -C "$main" worktree remove "$wt" || die "could not remove $2's worktree $wt"
      log "removed $2's worktree $wt"
    fi
    git -C "$main" branch -d "$2" || die "could not delete $2; it may not have landed on master"
    log "deleted the branch $2"
    ;;
  --dequeue-own)
    no_front "$1"
    [ -z "${2:-}" ] || die "--dequeue-own takes no argument, not $2"
    run_verb -evict
    ;;
  --dequeue-workspace)
    no_front "$1"
    [ -n "${2:-}" ] || die "--dequeue-workspace needs the workspace's worktree directory"
    run_verb -evict-dir "$2"
    ;;
  --pause-queue | --resume-queue)
    no_front "$1"
    control=-pause
    [ "$1" = --pause-queue ] || control=-resume
    if [ -n "${2:-}" ]; then
      run_verb "$control" -repository-dir "$2"
    fi
    run_verb "$control"
    ;;
  --hold-rollouts | --release-rollouts)
    no_front "$1"
    [ -z "${2:-}" ] || die "$1 takes no argument, not $2: the hold is daemon-wide"
    control=-hold-rollouts
    [ "$1" = --hold-rollouts ] || control=-release-rollouts
    run_verb "$control"
    ;;
  *)
    printf 'usage: run.sh --enqueue-own [--in-place | --keep-open] | --announce-one-shot | --land-workspace <dir> | --land-branch <branch> | --land-commits <commit>... | --pr-merged | --remove-branch <branch> | --dequeue-own | --dequeue-workspace <dir> | --pause-queue [<repo-dir>] | --resume-queue [<repo-dir>] | --hold-rollouts | --release-rollouts  (any merge verb also takes --front)\n' >&2
    exit 1
    ;;
esac
