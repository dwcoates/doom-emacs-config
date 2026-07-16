#!/usr/bin/env bash
# show-chess-game run.sh — writes a chess-game payload file for the
# agent-repl chess-board widget and prints the marker line the response
# must carry verbatim.
#
# Verbs:
#   --write-game <pgn|fen>                Payload on stdin. Writes the
#                                         content-hashed payload file and
#                                         prints the marker line.
#   --write-session <session-id> [url]    Live engine-session pointer.
#                                         Discovers the engine daemon's
#                                         HTTP address when url is omitted,
#                                         writes the pointer file, and
#                                         prints the marker line.
#
# Exit codes:
#   0  Success; stdout is the marker line to re-emit verbatim.
#   1  Usage (no verb).
#   2  Environment or input error (not in a git worktree, empty payload,
#      bad kind, unwritable target).
#   3  Engine daemon address not discoverable; re-run with an explicit url.
set -uo pipefail

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
    emit "$kind" "$content"
    ;;
  --write-session)
    sid="${2:-}"
    [ -n "$sid" ] || die "session id required"
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
