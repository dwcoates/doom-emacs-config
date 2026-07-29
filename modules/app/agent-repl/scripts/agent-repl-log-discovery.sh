#!/usr/bin/env bash
# agent-repl-log-discovery.sh -- resolve and query agent-repl JSONL logs.
#
# This is deliberately read-only.  Workspace runtime logs are canonical
# symlinks under <workspace>/.claude/emacs; global logs are only the few
# runtime streams which are conceptually workspace-free.

set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  agent-repl-log-discovery.sh --workspace DIR [--runtime RUNTIME] [--session ID] [--pid PID] [--tail LINES]
  agent-repl-log-discovery.sh --global [--runtime RUNTIME] [--session ID] [--pid PID] [--tail LINES]

Without --session or --pid, print the canonical log location(s).  With either
filter, print matching JSONL records.  Session matching checks both
agent_repl_session_id and claude_session_id.  --tail limits records considered
before JSON filtering, so it is suitable for active, large logs.

Workspace runtimes: emacs, daemon, shim, webapp, sidecar
Global runtimes:    emacs, daemon, store, sidecar
EOF
}

fail() {
  printf 'agent-repl-log-discovery: %s\n' "$*" >&2
  exit 2
}

require_value() {
  [ "$#" -ge 2 ] || fail "$1 requires a value"
}

WORKSPACE=""
SCOPE=""
RUNTIME=""
SESSION=""
PID=""
TAIL_LINES=""

while [ "$#" -gt 0 ]; do
  case "$1" in
    --workspace)
      require_value "$@"
      [ -z "$SCOPE" ] || fail 'choose exactly one of --workspace and --global'
      SCOPE=workspace
      WORKSPACE="$2"
      shift 2
      ;;
    --global)
      [ -z "$SCOPE" ] || fail 'choose exactly one of --workspace and --global'
      SCOPE=global
      shift
      ;;
    --runtime)
      require_value "$@"
      [ -z "$RUNTIME" ] || fail '--runtime may be specified once'
      RUNTIME="$2"
      shift 2
      ;;
    --session)
      require_value "$@"
      [ -z "$SESSION" ] || fail '--session may be specified once'
      SESSION="$2"
      shift 2
      ;;
    --pid)
      require_value "$@"
      [ -z "$PID" ] || fail '--pid may be specified once'
      PID="$2"
      shift 2
      ;;
    --tail)
      require_value "$@"
      [ -z "$TAIL_LINES" ] || fail '--tail may be specified once'
      TAIL_LINES="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *) fail "unknown argument: $1" ;;
  esac
done

[ -n "$SCOPE" ] || fail 'choose --workspace DIR or --global'
[ -z "$PID" ] || [[ "$PID" =~ ^[1-9][0-9]*$ ]] || fail '--pid must be a positive integer'
[ -z "$TAIL_LINES" ] || [[ "$TAIL_LINES" =~ ^[1-9][0-9]*$ ]] || fail '--tail must be a positive integer'

case "$SCOPE" in
  workspace)
    [ -d "$WORKSPACE" ] || fail "workspace is not a directory: $WORKSPACE"
    WORKSPACE="$(cd "$WORKSPACE" && pwd -P)"
    allowed_runtimes=(emacs daemon shim webapp sidecar)
    ;;
  global)
    allowed_runtimes=(emacs daemon store sidecar)
    ;;
esac

if [ -n "$RUNTIME" ]; then
  valid=0
  for allowed in "${allowed_runtimes[@]}"; do
    [ "$RUNTIME" = "$allowed" ] && valid=1
  done
  [ "$valid" -eq 1 ] || fail "runtime $RUNTIME is not available in $SCOPE scope"
  runtimes=("$RUNTIME")
else
  runtimes=("${allowed_runtimes[@]}")
fi

state_root="${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}"
cache_root="${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl"

runtime_path() {
  local runtime="$1"
  if [ "$SCOPE" = workspace ]; then
    printf '%s/.claude/emacs/%s.log' "$WORKSPACE" "$runtime"
    return 0
  fi
  case "$runtime" in
    emacs) printf '%s/doom-agent-repl.log' "$state_root" ;;
    daemon) printf '%s/claude-repld.log' "$state_root" ;;
    store) printf '%s/log/shim-store.log' "$cache_root" ;;
    sidecar) printf '%s/log/shim-claude-sidecar.log' "$cache_root" ;;
  esac
}

resolve_workspace_target() {
  local canonical="$1"
  python3 - "$canonical" <<'PY'
import os
import sys

path = sys.argv[1]
if not os.path.islink(path):
    raise SystemExit(1)
print(os.path.realpath(path))
PY
}

paths=()
location_fd=1
if [ -n "$SESSION" ] || [ -n "$PID" ]; then
  # Query stdout is intentionally pure JSONL so it composes with jq and other
  # record consumers.  Resolution evidence remains visible on stderr.
  location_fd=2
fi
for runtime in "${runtimes[@]}"; do
  canonical="$(runtime_path "$runtime")"
  if [ "$SCOPE" = workspace ]; then
    if [ -L "$canonical" ]; then
      target="$(resolve_workspace_target "$canonical")" || fail "cannot resolve canonical symlink: $canonical"
      printf 'workspace runtime=%s canonical=%s target=%s\n' "$runtime" "$canonical" "$target" >&"$location_fd"
      paths+=("$canonical")
    else
      printf 'workspace runtime=%s canonical=%s status=absent\n' "$runtime" "$canonical" >&"$location_fd"
    fi
  elif [ -f "$canonical" ]; then
    printf 'global runtime=%s path=%s\n' "$runtime" "$canonical" >&"$location_fd"
    paths+=("$canonical")
  else
    printf 'global runtime=%s path=%s status=absent\n' "$runtime" "$canonical" >&"$location_fd"
  fi
done

if [ -z "$SESSION" ] && [ -z "$PID" ]; then
  exit 0
fi

[ "${#paths[@]}" -gt 0 ] || fail 'none of the selected canonical log files exists'

if [ -n "$TAIL_LINES" ]; then
  input_for_query() {
    local path
    for path in "${paths[@]}"; do
      tail -n "$TAIL_LINES" "$path"
    done
  }
else
  input_for_query() {
    local path
    for path in "${paths[@]}"; do
      cat "$path"
    done
  }
fi

input_for_query | python3 -c '
import json
import sys

session, pid = sys.argv[1:]
for number, raw in enumerate(sys.stdin, 1):
    if not raw.strip():
        continue
    try:
        record = json.loads(raw)
    except json.JSONDecodeError as exc:
        raise SystemExit(f"agent-repl-log-discovery: invalid JSONL record at selected input line {number}: {exc}")
    if not isinstance(record, dict):
        raise SystemExit(f"agent-repl-log-discovery: selected input line {number} is not a JSON object")
    session_match = bool(session) and session in (
        record.get("agent_repl_session_id"), record.get("claude_session_id"),
    )
    pid_match = bool(pid) and record.get("pid") == int(pid)
    if session_match or pid_match:
        sys.stdout.write(raw)
' "$SESSION" "$PID"
