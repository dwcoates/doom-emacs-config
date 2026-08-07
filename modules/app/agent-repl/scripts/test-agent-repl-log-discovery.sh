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

# --- performance verbs ------------------------------------------------------
# A dedicated workspace so the record-mode fixtures above stay untouched.
perf="$TMP/perf-workspace"
mkdir -p "$perf/.claude/emacs"
tab=$'\t'

cat >"$targets/perf-daemon.log" <<'EOF'
{"timestamp":"2026-07-28T12:00:00.000000Z","runtime":"daemon","pid":700,"level":"info","verbosity":"normal","operation":"daemon.establish-workspace.start","message":"begin establish","context":{},"workspace_dir":"/tmp/perf","workspace_id":"ws-perf","agent_repl_session_id":"agent-perf","request_id":"r1"}
{"timestamp":"2026-07-28T12:00:00.250000Z","runtime":"daemon","pid":700,"level":"info","verbosity":"normal","operation":"daemon.establish-workspace.complete","message":"end establish","context":{},"workspace_dir":"/tmp/perf","workspace_id":"ws-perf","agent_repl_session_id":"agent-perf","request_id":"r1"}
{"timestamp":"2026-07-28T12:00:01.000000Z","runtime":"daemon","pid":700,"level":"info","verbosity":"normal","operation":"daemon.establish-workspace.start","message":"begin stranded","context":{},"workspace_dir":"/tmp/perf","workspace_id":"ws-perf","agent_repl_session_id":"agent-perf","request_id":"r2"}
{"timestamp":"2026-07-28T12:00:02.000000Z","runtime":"daemon","pid":700,"level":"info","verbosity":"normal","operation":"daemon.spawn.start","message":"begin spawn","context":{},"workspace_dir":"/tmp/perf","workspace_id":"ws-perf","claude_session_id":"claude-perf"}
{"timestamp":"2026-07-28T12:00:24.000000Z","runtime":"daemon","pid":700,"level":"info","verbosity":"normal","operation":"daemon.spawn.complete","message":"end spawn","context":{},"workspace_dir":"/tmp/perf","workspace_id":"ws-perf","claude_session_id":"claude-perf"}
EOF
# Same request_id and family as the stranded start, but a different runtime, so
# it may only pair when the shim runtime is actually selected.
cat >"$targets/perf-shim.log" <<'EOF'
{"timestamp":"2026-07-28T12:00:03.000000Z","runtime":"shim","pid":700,"level":"info","verbosity":"normal","operation":"daemon.establish-workspace.complete","message":"foreign end","context":{},"workspace_dir":"/tmp/perf","workspace_id":"ws-perf","agent_repl_session_id":"agent-perf","request_id":"r2"}
EOF
ln -s "$targets/perf-daemon.log" "$perf/.claude/emacs/daemon.log"
ln -s "$targets/perf-shim.log" "$perf/.claude/emacs/shim.log"

perf_run() {
  HOME="$TMP/home" AGENT_REPL_STATE_DIR="$state" XDG_CACHE_HOME="$cache" \
    "$DISCOVER" --workspace "$perf" --runtime daemon "$@" 2>/dev/null
}

out="$(perf_run --spans daemon.)"
expected="2026-07-28T12:00:00.000000Z${tab}2026-07-28T12:00:00.250000Z${tab}250.000${tab}daemon.establish-workspace.start${tab}r1${tab}matched"
case "$out" in
  *"$expected"*) ;;
  *) fail "matched span row missing: $out" ;;
esac

expected="2026-07-28T12:00:01.000000Z${tab}-${tab}-${tab}daemon.establish-workspace.start${tab}r2${tab}unmatched-start"
case "$out" in
  *"$expected"*) ;;
  *) fail "unmatched start was not surfaced: $out" ;;
esac

expected="2026-07-28T12:00:02.000000Z${tab}2026-07-28T12:00:24.000000Z${tab}22000.000${tab}daemon.spawn.start${tab}pid:700:daemon.spawn${tab}matched"
case "$out" in
  *"$expected"*) ;;
  *) fail "pid-adjacency span row missing: $out" ;;
esac

out="$(perf_run --latency-by operation)"
expected="daemon.spawn.start${tab}1${tab}22000.000${tab}22000.000${tab}22000.000
daemon.establish-workspace.start${tab}1${tab}250.000${tab}250.000${tab}250.000"
[ "$out" = "$expected" ] || fail "latency aggregation ordering: $out"

# Strictly greater than the threshold: the 1000ms gap at the boundary is not a
# finding, the 22000ms stall is.
out="$(perf_run --gaps 1000)"
[ "$(printf '%s\n' "$out" | wc -l | tr -d ' ')" = 1 ] || fail "gap boundary reported extra rows: $out"
case "$out" in
  "22000.000${tab}2026-07-28T12:00:02.000000Z${tab}2026-07-28T12:00:24.000000Z${tab}daemon.spawn.start${tab}daemon.spawn.complete${tab}begin spawn${tab}end spawn") ;;
  *) fail "stall gap row: $out" ;;
esac

out="$(perf_run --gaps 999)"
[ "$(printf '%s\n' "$out" | wc -l | tr -d ' ')" = 2 ] || fail "gap below boundary row count: $out"

# --runtime composition: selecting only the daemon keeps the shim completion
# out of the stream, so r2 stays unmatched.
out="$(HOME="$TMP/home" AGENT_REPL_STATE_DIR="$state" XDG_CACHE_HOME="$cache" \
  "$DISCOVER" --workspace "$perf" --spans daemon.establish-workspace 2>/dev/null)"
case "$out" in
  *"${tab}r2${tab}matched"*) ;;
  *) fail "runtime composition control did not pair across runtimes: $out" ;;
esac

# --session composition: the claude-session span drops out entirely.
out="$(perf_run --session agent-perf --spans daemon.)"
case "$out" in
  *"daemon.spawn.start"*) fail "session filter leaked another session span: $out" ;;
esac
case "$out" in
  *"${tab}r1${tab}matched"*) ;;
  *) fail "session filter dropped its own span: $out" ;;
esac

printf 'not-json\n' >>"$targets/perf-daemon.log"
if HOME="$TMP/home" AGENT_REPL_STATE_DIR="$state" XDG_CACHE_HOME="$cache" \
  "$DISCOVER" --workspace "$perf" --runtime daemon --spans daemon. >/dev/null 2>&1; then
  fail 'malformed JSONL was accepted by --spans'
fi

printf 'not-json\n' >>"$targets/daemon.log"
if HOME="$TMP/home" AGENT_REPL_STATE_DIR="$state" XDG_CACHE_HOME="$cache" "$DISCOVER" --workspace "$workspace" --runtime daemon --pid 101 >/dev/null 2>&1; then
  fail 'malformed JSONL was accepted'
fi

printf 'PASS: agent-repl-log-discovery\n'
