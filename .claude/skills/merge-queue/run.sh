#!/usr/bin/env bash
# run.sh — the merge-queue skill's driver.
#
# Every merge verb asks for a merge FROM the workspace this shell is inside.
# The merge is put in line once the caller's current turn ends and reports its
# outcome into that workspace's session; no merge verb waits for the outcome.
# A merge is a ONE-SHOT (SYNC: this workspace's own branch, run in this
# workspace, which closes once it lands) or ASYNC (a named branch, run in a
# hidden merge workspace forked from this one, which stays free).
#
# The queue-control verbs (--dequeue-*, --move-*, --pause-queue,
# --resume-queue, --hold-rollouts, --release-rollouts, --disable-racing,
# --enable-racing, --subscribe, --unsubscribe) ask,
# from the workspace this shell is inside, for the queue itself to change, and
# wait (bounded) for the answer, which they print: the outcome when applied, the
# refusal's cause when refused.
#
# Verbs:
#   --one-shot [--pr-merged]     Announce, from inside the current turn, that
#                                this workspace one-shot merges once the turn
#                                ends: its own branch in place, this workspace
#                                closed once it lands, and this workspace
#                                marked a one-shot now. With --pr-merged its
#                                branch already merged upstream: the default
#                                branch is updated and this workspace closed.
#                                Refused in the repository's main worktree.
#   --async <name> <branch> [--instruction <text>] [--close <dir>]
#           [--cleanup] [--compact-after] [--before-merge <prompt>]
#           [--after-merge <prompt>]
#                                Merge <branch> ASYNC, in a hidden merge
#                                workspace named <name> forked from this
#                                workspace, which stays free. --instruction
#                                has the merge workspace prepare the branch
#                                first; --close names ANOTHER workspace, by
#                                its worktree, to close once it lands;
#                                --cleanup removes the worktree that has the
#                                branch checked out once it lands;
#                                --compact-after compacts this workspace then;
#                                --before-merge and --after-merge are the
#                                prompts the merge workspace runs before the
#                                landing (failing the merge if it fails) and
#                                after it lands, and the only merge actions it
#                                runs.
#   --dequeue-own                Take this workspace's own merge off the queue.
#   --dequeue-workspace <dir>    Take ANOTHER workspace's merge off the queue,
#                                named by its worktree.
#   --move-up-own                Move this workspace's own waiting merge one
#                                place up the queue.
#   --move-up-workspace <dir>    Move ANOTHER workspace's waiting merge one
#                                place up the queue, named by its worktree.
#   --move-down-own              Move this workspace's own waiting merge one
#                                place down the queue.
#   --move-down-workspace <dir>  Move ANOTHER workspace's waiting merge one
#                                place down the queue, named by its worktree.
#   --pause-queue [<repo-dir>]   Pause the queue of the repository whose main
#                                checkout is <repo-dir>, else of every
#                                repository.
#   --resume-queue [<repo-dir>]  Resume a paused queue, scoped as a pause is.
#   --hold-rollouts              Hold the queue's ROLLOUTS, daemon-wide: merges
#                                still land, but a landing in the daemon's own
#                                checkout no longer redeploys, hot-reloads or
#                                bounces the runtime. Durable until released.
#   --release-rollouts           End the hold; the next landing redeploys.
#   --disable-racing             Disable the queue's RACING, daemon-wide: every
#                                queue works its head alone, whatever the
#                                host's power source. Durable until enabled.
#   --enable-racing              Enable racing again; on AC power each queue
#                                works its top two merges.
#   --subscribe <detail> [<repo-dir>]
#                                Subscribe this workspace's agent to the queue
#                                of the repository whose main checkout is
#                                <repo-dir>, else of every repository, at
#                                <detail>: every-change, advances (a merge
#                                leaves the queue) or depletes (the queue runs
#                                out of merges). Subscribing again in the same
#                                scope replaces the detail.
#   --unsubscribe [<repo-dir>]   End this workspace's subscription in that
#                                scope.
#
# Modifiers:
#   --front                      With a merge verb (--one-shot, --async),
#                                anywhere among
#                                its arguments: the merge goes to the FRONT of
#                                its queue once it is put in line, ahead of
#                                every ordinary merge waiting, never ahead of
#                                the one running. Refused on any other verb.
#   --minor <line> | --patch <line>
#                                With a merge verb, anywhere among its
#                                arguments: how a merge onto agent-repl's
#                                master moves the version, and the one-line
#                                changelog entry its version tag carries. The
#                                daemon refuses such a merge without one, and
#                                any other merge with one (exit 5). One of the
#                                two at most; refused on any other verb.
#
# Exit codes (every verb):
#   0  requested (--async; the outcome reports into this session), announced
#      (--one-shot; the turn goes on), or applied (queue-control verbs; the
#      printed outcome line is evicted, moved_up, moved_down, unmoved,
#      not_queued, paused, resumed, held, released, racing_disabled,
#      racing_enabled, subscribed, resubscribed, unsubscribed or
#      not_subscribed, or unread)
#   2  script/usage error, or the merge or control could not be requested
#   5  refused by the daemon; why (or how to read why) is printed (an async
#      merge's name already taken, its branch checked out in this workspace,
#      or a --close workspace that is not open here, is refused here, as is
#      a merge onto agent-repl's master without --minor or --patch, or any
#      other merge with one)
#   8  the daemon did not answer a queue-control request in time; the request
#      stays pending (queue-control verbs)

set -uo pipefail

# shellcheck source=../../../modules/app/agent-repl/bin/lib-worktrees.sh
. "$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../../../modules/app/agent-repl/bin/lib-worktrees.sh"

die() {
  printf 'merge-queue: %s\n' "$*" >&2
  exit 2
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
  [ "$top" != "$main" ] || die "$top is the repository's main worktree, which never one-shots; land a branch with --async"
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
# arguments; VERSION is its -version-part and -changelog-line, set when
# --minor <line> or --patch <line> is. Both ride every merge request this
# script makes.
FRONT=()
VERSION=()
ARGS=()
while [ "$#" -gt 0 ]; do
  case "$1" in
    --front)
      FRONT=(-front)
      shift ;;
    --minor | --patch)
      [ "${#VERSION[@]}" -eq 0 ] || die "--minor and --patch each move the version once; give one"
      [ -n "${2:-}" ] && [ "${2#--}" = "$2" ] || die "$1 needs the one-line changelog entry the version's tag carries"
      VERSION=(-version-part "${1#--}" -changelog-line "$2")
      shift 2 ;;
    *)
      ARGS+=("$1")
      shift ;;
  esac
done
set -- "${ARGS[@]+"${ARGS[@]}"}"

# no_front refuses --front, --minor and --patch on a verb that requests no
# merge.
no_front() {
  [ "${#FRONT[@]}" -eq 0 ] || die "--front goes with a merge verb; $1 requests no merge"
  [ "${#VERSION[@]}" -eq 0 ] || die "--minor and --patch go with a merge verb; $1 requests no merge"
}

# verb_status runs agent-repld merge-queue, passing its output through, and
# answers its exit mapped onto this script's. A merge request carries -front
# when --front was given, and its version increment when --minor or --patch
# was.
verb_status() {
  local bin rc
  bin="$(daemon_bin)" || return 2
  "$bin" merge-queue "$@" "${FRONT[@]+"${FRONT[@]}"}" "${VERSION[@]+"${VERSION[@]}"}"
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

# run_scoped runs agent-repld merge-queue with ARGS, scoped to the
# repository whose main checkout is DIR when DIR is given, else to every
# repository, and exits with its mapped status.
run_scoped() {
  local dir="$1"
  shift
  if [ -n "$dir" ]; then
    run_verb "$@" -repository-dir "$dir"
  fi
  run_verb "$@"
}

case "${1:-}" in
  --one-shot)
    shift
    mode=(-one-shot)
    case "${1:-}" in
      "") ;;
      --pr-merged)
        [ "$#" -eq 1 ] || die "--one-shot --pr-merged takes nothing after it, not ${2}"
        mode+=(-pr-merged) ;;
      *) die "--one-shot takes only --pr-merged, not $1" ;;
    esac
    workspace_top >/dev/null || exit 2
    run_verb "${mode[@]}"
    ;;
  --async)
    [ -n "${2:-}" ] && [ "${2#--}" = "$2" ] || die "--async needs the merge workspace's name"
    [ -n "${3:-}" ] && [ "${3#--}" = "$3" ] || die "--async <name> needs the branch to land"
    mode=(-async -name "$2" -branch "$3")
    shift 3
    while [ "$#" -gt 0 ]; do
      case "$1" in
        --instruction)
          [ -n "${2:-}" ] || die "--instruction needs the text saying how to prepare the branch"
          mode+=(-instruction "$2")
          shift 2 ;;
        --close)
          [ -n "${2:-}" ] || die "--close needs the worktree directory of the workspace to close"
          mode+=(-close-dir "$2")
          shift 2 ;;
        --cleanup)
          mode+=(-cleanup-worktree)
          shift ;;
        --compact-after)
          mode+=(-compact-after)
          shift ;;
        --before-merge)
          [ -n "${2:-}" ] || die "--before-merge needs the prompt the merge workspace runs before the landing"
          mode+=(-before-merge "$2")
          shift 2 ;;
        --after-merge)
          [ -n "${2:-}" ] || die "--after-merge needs the prompt the merge workspace runs after the landing"
          mode+=(-after-merge "$2")
          shift 2 ;;
        *) die "--async <name> <branch> takes only --instruction <text>, --close <dir>, --cleanup, --compact-after, --before-merge <prompt> and --after-merge <prompt>, not $1; quote an instruction or a prompt as one argument" ;;
      esac
    done
    run_verb "${mode[@]}"
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
  --move-up-own | --move-down-own)
    no_front "$1"
    [ -z "${2:-}" ] || die "$1 takes no argument, not $2"
    move=-move-up
    [ "$1" = --move-up-own ] || move=-move-down
    run_verb "$move"
    ;;
  --move-up-workspace | --move-down-workspace)
    no_front "$1"
    [ -n "${2:-}" ] || die "$1 needs the workspace's worktree directory"
    move=-move-up
    [ "$1" = --move-up-workspace ] || move=-move-down
    run_verb "$move" -move-dir "$2"
    ;;
  --pause-queue | --resume-queue)
    no_front "$1"
    control=-pause
    [ "$1" = --pause-queue ] || control=-resume
    run_scoped "${2:-}" "$control"
    ;;
  --subscribe)
    no_front "$1"
    case "${2:-}" in
      every-change) detail=every_change ;;
      advances | depletes) detail="$2" ;;
      "") die "--subscribe needs the detail: every-change, advances or depletes" ;;
      *) die "--subscribe takes every-change, advances or depletes, not $2" ;;
    esac
    [ -z "${4:-}" ] || die "--subscribe <detail> takes at most a repository directory after it, not $4"
    run_scoped "${3:-}" -subscribe "$detail"
    ;;
  --unsubscribe)
    no_front "$1"
    [ -z "${3:-}" ] || die "--unsubscribe takes at most a repository directory, not $3"
    run_scoped "${2:-}" -unsubscribe
    ;;
  --hold-rollouts | --release-rollouts | --disable-racing | --enable-racing)
    no_front "$1"
    [ -z "${2:-}" ] || die "$1 takes no argument, not $2: the switch is daemon-wide"
    run_verb "${1#-}"
    ;;
  *)
    printf 'usage: run.sh --one-shot [--pr-merged] | --async <name> <branch> [--instruction <text>] [--close <dir>] [--cleanup] [--compact-after] [--before-merge <prompt>] [--after-merge <prompt>] | --dequeue-own | --dequeue-workspace <dir> | --move-up-own | --move-up-workspace <dir> | --move-down-own | --move-down-workspace <dir> | --pause-queue [<repo-dir>] | --resume-queue [<repo-dir>] | --hold-rollouts | --release-rollouts | --disable-racing | --enable-racing | --subscribe <every-change|advances|depletes> [<repo-dir>] | --unsubscribe [<repo-dir>]  (any merge verb also takes --front, and --minor <line> or --patch <line>)\n' >&2
    exit 1
    ;;
esac
