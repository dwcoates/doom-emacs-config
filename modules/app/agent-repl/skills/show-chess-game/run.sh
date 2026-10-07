#!/usr/bin/env bash
# run.sh — the show-chess-game skill's driver: finding a CEE CLI session and
# the game live in it, for agent-repl's show_chess_board tool.
#
# Verbs:
#   --list-sessions        Print every CEE CLI session id, one per line.
#   --live-game <id>       Print the id of the game live in session <id>.
#
# Exit codes:
#   0  printed what the verb answers
#   1  --list-sessions: no session exists
#      --live-game: the session does not exist or holds no game (the reason
#      is printed)
#   2  script/usage error, or the CEE CLI could not be asked

set -uo pipefail

die() {
  printf 'show-chess-game: %s\n' "$*" >&2
  exit 2
}

log() {
  printf 'show-chess-game: %s\n' "$*"
}

command -v gns >/dev/null 2>&1 || die "gns is not on PATH"
command -v jq >/dev/null 2>&1 || die "jq is not on PATH"

case "${1:-}" in
  --list-sessions)
    [ -z "${2:-}" ] || die "--list-sessions takes no argument, not $2"
    out="$(gns cee session list 2>&1)" || die "gns cee session list failed: $out"
    ids="$(printf '%s' "$out" | jq -r '.sessions[]?' 2>/dev/null)" || die "could not read the session list: $out"
    [ -n "$ids" ] || exit 1
    printf '%s\n' "$ids"
    ;;
  --live-game)
    [ -n "${2:-}" ] || die "--live-game needs a session id"
    out="$(gns cee session poll "$2" 2>&1)" || die "gns cee session poll failed: $out"
    printf '%s' "$out" | jq -e . >/dev/null 2>&1 || die "could not read the session poll: $out"
    active="$(printf '%s' "$out" | jq -r '.active')"
    has_game="$(printf '%s' "$out" | jq -r '.has_game')"
    game="$(printf '%s' "$out" | jq -r '.game_id // ""')"
    if [ "$active" != "true" ]; then
      log "CEE session $2 does not exist"
      exit 1
    fi
    if [ "$has_game" != "true" ] || [ -z "$game" ]; then
      log "CEE session $2 holds no game"
      exit 1
    fi
    printf '%s\n' "$game"
    ;;
  *)
    printf 'usage: run.sh --list-sessions | --live-game <session-id>\n' >&2
    exit 1
    ;;
esac
