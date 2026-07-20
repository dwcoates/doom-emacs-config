#!/usr/bin/env bash
# show-chess-game run.sh — writes a chess-game payload file for the
# agent-repl chess-board widget and prints the marker line the response
# must carry verbatim.
#
# Verbs:
#   --write-game <pgn|fen>                Payload on stdin. Probes the
#                                         chess-widget capability, then
#                                         writes the content-hashed payload
#                                         file and prints the marker line.
#   --write-session <session-id> [url]    Live engine-session pointer.
#                                         Probes the chess-widget capability,
#                                         discovers the engine daemon's HTTP
#                                         address when url is omitted, writes
#                                         the pointer file, and prints the
#                                         marker line.
#
# Both marker-emitting verbs FIRST probe whether the agent-repl daemon can
# actually render a board (the chess-widget capability). When it cannot,
# they print actionable remediation to stdout and exit 4 WITHOUT emitting a
# marker, so a dead marker never reaches the response.
#
# Exit codes:
#   0  Success; stdout is the marker line to re-emit verbatim.
#   1  Usage (no verb).
#   2  Environment or input error (not in a git worktree, empty payload,
#      bad kind, unwritable target).
#   3  Engine daemon address not discoverable; re-run with an explicit url.
#   4  Chess-widget capability unavailable; stdout is actionable remediation
#      (NOT a marker) — surface it to the user instead of a board.
set -uo pipefail

# Address of the agent-repl daemon whose /widget-assets/ mount renders the
# board, inherited from the session environment (default local loopback).
DAEMON_ADDR="${AGENT_REPL_DAEMON_ADDR:-127.0.0.1:8787}"

die() {
  echo "show-chess-game: $*" >&2
  exit 2
}

log() {
  echo "show-chess-game: $*" >&2
}

# Directory the widget host serves payloads from, under this worktree.
game_dir() {
  local root dir
  root=$(git rev-parse --show-toplevel 2>/dev/null) || die "not inside a git worktree"
  dir="$root/.claude/emacs/cee-web-widget"
  mkdir -p "$dir" || die "cannot create $dir"
  echo "$dir"
}

# capability_ok: probe the daemon's authoritative capability endpoint and
# return 0 only when it serves the widget assets AND the mount bundle is
# present. Any other outcome (capability off, bad dist, daemon unreachable)
# returns non-zero.
capability_ok() {
  local body
  body=$(curl -s -m 3 "http://$DAEMON_ADDR/capabilities" 2>/dev/null) || return 1
  case "$body" in
    *'"widget_assets":true'*) : ;;
    *) return 1 ;;
  esac
  case "$body" in
    *'"widget_bundle_present":true'*) return 0 ;;
    *) return 1 ;;
  esac
}

# emit_remediation: print actionable steps to stdout when the capability is
# unavailable — which defcustom to set, the cee-web-widget/dist paths found
# on disk, and the exact command to bounce the daemon.
emit_remediation() {
  local found=0 d
  echo "show-chess-game: the chess-widget capability is unavailable, so a board cannot render."
  echo "(probed the agent-repl daemon at http://$DAEMON_ADDR/capabilities)"
  echo
  echo "To fix it:"
  echo "  1. Bounce the daemon so it serves the widget assets and reloads every webview:"
  echo "       In Emacs: M-x agent-repl-frontend-daemon-restart"
  echo "  2. If it still reports the capability off, set the widget-assets dir explicitly to one"
  echo "     of the cee-web-widget/dist paths below, then bounce again:"
  echo "       In Emacs: M-x customize-variable RET agent-repl-frontend-widget-assets-dir"
  for d in "$HOME"/workspace/ChessCom/*/apps/cee-web-widget/dist \
           "$HOME"/workspace/ChessCom/*/*/apps/cee-web-widget/dist; do
    [ -d "$d" ] && { echo "         $d"; found=1; }
  done
  [ "$found" -eq 0 ] && echo "         (no cee-web-widget/dist found under ~/workspace/ChessCom — build the widget first)"
}

# emit <ext> <content>: content-hashed payload file + marker line.
# Identical content dedupes onto the same file.
emit() {
  local ext="$1" content="$2" dir hash file
  dir=$(game_dir) || exit 2
  hash=$(printf '%s\n' "$content" | shasum -a 256 | cut -c1-12)
  file="$dir/chess-game-$hash.$ext"
  if [ ! -f "$file" ]; then
    printf '%s\n' "$content" > "$file" || die "write failed: $file"
  fi
  echo "---> agent-repl-chess-game-file: $file <---"
}

case "${1:-}" in
  --write-game)
    kind="${2:-}"
    if [ "$kind" != "pgn" ] && [ "$kind" != "fen" ]; then
      die "kind must be pgn or fen (got: ${kind:-<empty>})"
    fi
    content=$(cat)
    [ -n "$content" ] || die "empty payload on stdin"
    if ! capability_ok; then
      emit_remediation
      exit 4
    fi
    emit "$kind" "$content"
    ;;
  --write-session)
    sid="${2:-}"
    [ -n "$sid" ] || die "session id required"
    if ! capability_ok; then
      emit_remediation
      exit 4
    fi
    url="${3:-}"
    if [ -z "$url" ]; then
      for f in "${TMPDIR:-/tmp}/cee-cli-daemon-$(id -u).http" "/tmp/cee-cli-daemon-$(id -u).http"; do
        if [ -r "$f" ]; then
          url=$(cat "$f")
          break
        fi
      done
    fi
    if [ -z "$url" ]; then
      echo "show-chess-game: engine daemon address not found; pass the backend url explicitly" >&2
      exit 3
    fi
    emit "session" "$url
$sid"
    ;;
  *)
    echo "usage: run.sh --write-game <pgn|fen> | --write-session <session-id> [url]" >&2
    exit 1
    ;;
esac
