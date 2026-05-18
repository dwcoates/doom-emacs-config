#!/usr/bin/env bash
# run.sh — driver for the workspace-merge skill
# Usage:
#   run.sh --close-current-session-sockets <workspace-name>...
#     Close gns-sockets subscriptions bound to $CLAUDE_CODE_SESSION_ID
#     when the current git branch matches one of the provided workspace
#     names. Prints one closed subscription id per line on stdout.
#     No-ops cleanly (and exits 0) when:
#       - the current branch is not in the argument list
#       - there is no gns-sockets state directory
#       - jq or CLAUDE_CODE_SESSION_ID is unavailable
#     Exit codes: 0=success (including no-op), 2=script error.
#   run.sh --emit-commands
#     Read a JSON commands array from stdin and dispatch it atomically
#     via the shared emit script.
#     Exit codes: 0=success, 2=script error (e.g. missing uuidgen).
# Exit codes: 0=success  1=condition (unknown subcommand)  2=script error

set -uo pipefail

die() { echo "ERROR: $*" >&2; exit 2; }
log() { echo "▶ $*"; }

case "${1:-}" in

--close-current-session-sockets)
  shift
  if [ $# -eq 0 ]; then
    die "no workspace names provided"
  fi
  current_branch=$(git -C "$PWD" branch --show-current 2>/dev/null || true)
  if [ -z "$current_branch" ]; then
    exit 0
  fi
  match=0
  for ws in "$@"; do
    if [ "$ws" = "$current_branch" ]; then
      match=1
      break
    fi
  done
  if [ "$match" -eq 0 ]; then
    exit 0
  fi
  state_dir="${GNS_SOCKETS_STATE_DIR:-$HOME/.gns-sockets}"
  if [ ! -d "$state_dir/threads" ]; then
    exit 0
  fi
  if ! command -v jq >/dev/null 2>&1; then
    echo "warning: jq not available; skipping gns-sockets cleanup" >&2
    exit 0
  fi
  session_id="${CLAUDE_CODE_SESSION_ID:-}"
  if [ -z "$session_id" ]; then
    echo "warning: CLAUDE_CODE_SESSION_ID is unset; skipping gns-sockets cleanup" >&2
    exit 0
  fi
  shopt -s nullglob
  for state in "$state_dir/threads"/*/state.json; do
    id=$(jq -r --arg sid "$session_id" \
      'select(any(.subscriptions[]?; .session_id == $sid)) | .id // empty' \
      "$state" 2>/dev/null) || continue
    [ -n "$id" ] || continue
    if gns sockets close "$id" --purge >/dev/null 2>&1; then
      printf '%s\n' "$id"
    else
      echo "warning: failed to close subscription $id" >&2
    fi
  done
  ;;

--emit-commands)
  shift
  exec bash "$(dirname "$0")/../emit-workspace-commands.sh"
  ;;

*)
  cat >&2 <<'USAGE'
Usage:
  run.sh --close-current-session-sockets <workspace-name>...
  run.sh --emit-commands  (reads JSON from stdin)
USAGE
  exit 1
  ;;

esac
