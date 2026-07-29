#!/usr/bin/env bash
# Focused tests for agent-repl-log-discovery.sh.  All state is temporary.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd -P)"
DISCOVER="$SCRIPT_DIR/agent-repl-log-discovery.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

fail() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

workspace="$TMP/workspace"
targets="$TMP/targets"
state="$TMP/state"
cache="$TMP/cache"
emacs_global="$TMP/emacs-global.log"
mkdir -p "$workspace/.claude/emacs" "$targets" "$state" "$cache/agent-repl/log"
workspace_real="$(cd "$workspace" && pwd -P)"
targets_real="$(cd "$targets" && pwd -P)"

cat >"$targets/daemon.log" <<'EOF'
{"timestamp":"2026-07-28T12:00:00Z","runtime":"daemon","pid":101,"level":"info","verbosity":"normal","operation":"daemon.test","message":"first","context":{},"workspace_dir":"/tmp/workspace","workspace_id":"ws","agent_repl_session_id":"agent-1"}
{"timestamp":"2026-07-28T12:00:01Z","runtime":"daemon","pid":202,"level":"error","verbosity":"normal","operation":"daemon.test","message":"second","context":{},"workspace_dir":"/tmp/workspace","workspace_id":"ws","claude_session_id":"claude-2"}
EOF
ln -s "$targets/daemon.log" "$workspace/.claude/emacs/daemon.log"
printf '%s\n' '{"timestamp":"2026-07-28T12:00:02Z","runtime":"store","pid":303,"level":"info","verbosity":"normal","operation":"store.test","message":"global","context":{}}' >"$cache/agent-repl/log/shim-store.log"
printf '%s\n' '{"timestamp":"2026-07-28T12:00:03Z","runtime":"emacs","pid":404,"level":"info","verbosity":"normal","operation":"emacs.test","message":"global-emacs","context":{}}' >"$emacs_global"

out="$(HOME="$TMP/home" AGENT_REPL_STATE_DIR="$state" XDG_CACHE_HOME="$cache" "$DISCOVER" --workspace "$workspace" --runtime daemon)"
case "$out" in
  *"runtime=daemon"*"canonical=$workspace_real/.claude/emacs/daemon.log"*"target=$targets_real/daemon.log"*) ;;
  *) fail "workspace resolution output: $out" ;;
esac

out="$(HOME="$TMP/home" AGENT_REPL_STATE_DIR="$state" XDG_CACHE_HOME="$cache" "$DISCOVER" --workspace "$workspace" --runtime daemon --session claude-2)"
case "$out" in
  *'"message":"second"'*) ;;
  *) fail "claude session query output: $out" ;;
esac
case "$out" in
  *'"message":"first"'*) fail "session query included wrong record: $out" ;;
esac
case "$out" in
  *'workspace runtime='*) fail "session query was not pure JSONL: $out" ;;
esac

out="$(HOME="$TMP/home" AGENT_REPL_STATE_DIR="$state" XDG_CACHE_HOME="$cache" "$DISCOVER" --workspace "$workspace" --runtime daemon --pid 101)"
case "$out" in
  *'"message":"first"'*) ;;
  *) fail "pid query output: $out" ;;
esac

out="$(HOME="$TMP/home" AGENT_REPL_STATE_DIR="$state" XDG_CACHE_HOME="$cache" "$DISCOVER" --global --runtime store --pid 303)"
case "$out" in
  *'"message":"global"'*) ;;
  *) fail "global query output: $out" ;;
esac

out="$(HOME="$TMP/home" AGENT_REPL_STATE_DIR="$state" AGENT_REPL_EMACS_GLOBAL_LOG="$emacs_global" XDG_CACHE_HOME="$cache" "$DISCOVER" --global --runtime emacs --pid 404)"
case "$out" in
  *'"message":"global-emacs"'*) ;;
  *) fail "global Emacs query output: $out" ;;
esac

if HOME="$TMP/home" AGENT_REPL_STATE_DIR="$state" XDG_CACHE_HOME="$cache" "$DISCOVER" --workspace "$workspace" --runtime shim --pid 1 >/dev/null 2>&1; then
  fail 'querying an absent selected log succeeded'
fi

printf 'not-json\n' >>"$targets/daemon.log"
if HOME="$TMP/home" AGENT_REPL_STATE_DIR="$state" XDG_CACHE_HOME="$cache" "$DISCOVER" --workspace "$workspace" --runtime daemon --pid 101 >/dev/null 2>&1; then
  fail 'malformed JSONL was accepted'
fi

printf 'PASS: agent-repl-log-discovery\n'
