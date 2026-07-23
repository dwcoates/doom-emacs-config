#!/usr/bin/env bash
# agent-shim-doctor.sh — read-only diagnostics for the agent-shim ecosystem
# (§12 of design-agent-shim-architecture.md; see scripts/AGENTS.md).
#
# Reports connectivity + liveness across the shim ecosystem's UDS sockets,
# launchd services, log files, and the store DB. Every check prints exactly
# one PASS / FAIL / SKIP line with a short detail and, on anything but PASS, a
# remediation hint.
#
#   PASS  the checked invariant holds.
#   FAIL  the invariant is violated (something is down / missing / corrupt).
#   SKIP  the check could not run because an OPTIONAL tool is absent. A SKIP
#         is an honest environment report, never a silent fallback: the check
#         did not pass, it simply could not be evaluated here.
#
# This script is STRICTLY READ-ONLY. It never starts, stops, restarts, loads,
# unloads, or otherwise mutates any service, socket, file, or launchd state.
# It only stats files, opens (and immediately closes) sockets, and issues
# read-only `launchctl print` / `sqlite3 'PRAGMA integrity_check'` queries.
#
# State root: defaults to ${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl. Override
# with AGENT_REPL_STATE_ROOT (used by the unit dry-run to point at a fabricated
# temp dir so the real cache is never touched).
#
# Usage:
#   agent-shim-doctor.sh [--json]
#     --json   emit a machine-readable JSON array instead of text lines.
#
# Exit: 0 when no check FAILed (SKIPs do not fail); 1 when any check FAILed.

set -euo pipefail

# --- Configuration ------------------------------------------------------

STATE_ROOT="${AGENT_REPL_STATE_ROOT:-${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl}"
SOCK_DIR="$STATE_ROOT/sock"
LOG_DIR="$STATE_ROOT/log"
STORE_DB="$STATE_ROOT/store/events.db"

STORE_SOCK="$SOCK_DIR/store.sock"
FRONTEND_SOCK="$SOCK_DIR/daemon-frontend.sock"

STORE_LABEL="com.agentrepl.shim-store"
SIDECAR_LABEL="com.agentrepl.shim-claude-sidecar"

# A log is considered "recently written" if touched within this many seconds.
# Beyond it the log still PASSes (an idle service writes nothing) but the age
# is surfaced so a wedged/never-started service is visible.
LOG_RECENT_SECS=900

JSON=0

# --- Result accumulation ------------------------------------------------

# Parallel arrays: one entry per check.
R_NAME=()
R_STATUS=()
R_DETAIL=()
R_HINT=()
FAIL_COUNT=0

# record NAME STATUS DETAIL [HINT]
record() {
  R_NAME+=("$1")
  R_STATUS+=("$2")
  R_DETAIL+=("$3")
  R_HINT+=("${4:-}")
  [ "$2" = "FAIL" ] && FAIL_COUNT=$((FAIL_COUNT + 1))
  return 0
}

# --- Small helpers ------------------------------------------------------

# now_epoch / file_mtime abstract the platform stat call (BSD stat on macOS).
now_epoch() { date +%s; }

file_mtime() {
  # Prints the file's mtime in epoch seconds, or nothing if stat fails.
  stat -f %m "$1" 2>/dev/null || stat -c %Y "$1" 2>/dev/null || true
}

# json_escape STRING — escape for embedding in a JSON string literal.
json_escape() {
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  s="${s//$'\t'/\\t}"
  s="${s//$'\n'/\\n}"
  s="${s//$'\r'/\\r}"
  printf '%s' "$s"
}

# --- Checks -------------------------------------------------------------

check_store_socket_present() {
  if [ -S "$STORE_SOCK" ]; then
    record "store-socket-present" "PASS" "socket exists at $STORE_SOCK"
  else
    record "store-socket-present" "FAIL" "no socket at $STORE_SOCK" \
      "shim-store not running; check '$STORE_LABEL' via launchctl or (re)run install.sh --with-agent-shim-services"
  fi
}

check_store_socket_connectable() {
  if ! command -v nc >/dev/null 2>&1; then
    record "store-socket-connectable" "SKIP" "nc not installed; cannot probe $STORE_SOCK" \
      "install netcat (nc) to enable the connectability probe"
    return 0
  fi
  if [ ! -S "$STORE_SOCK" ]; then
    record "store-socket-connectable" "FAIL" "no socket to connect to at $STORE_SOCK" \
      "shim-store not running; start '$STORE_LABEL' (install.sh --with-agent-shim-services)"
    return 0
  fi
  if nc -U -w 2 "$STORE_SOCK" </dev/null >/dev/null 2>&1; then
    record "store-socket-connectable" "PASS" "connected to $STORE_SOCK"
  else
    record "store-socket-connectable" "FAIL" "socket file present but connection refused at $STORE_SOCK" \
      "shim-store socket is stale (process gone); check '$STORE_LABEL' liveness via launchctl"
  fi
}

check_frontend_socket_present() {
  if [ -S "$FRONTEND_SOCK" ]; then
    record "daemon-frontend-socket-present" "PASS" "socket exists at $FRONTEND_SOCK"
  else
    record "daemon-frontend-socket-present" "FAIL" "no socket at $FRONTEND_SOCK" \
      "the daemon (claude-repld) is not serving its frontend UDS; check the daemon is running"
  fi
}

check_session_sockets() {
  # Enumeration is informational: 0 sessions is a normal idle state, so this
  # always PASSes and simply reports what it found.
  local socks=()
  local f
  if [ -d "$SOCK_DIR" ]; then
    for f in "$SOCK_DIR"/session-*.sock; do
      [ -S "$f" ] && socks+=("$(basename "$f")")
    done
  fi
  local n="${#socks[@]}"
  if [ "$n" -eq 0 ]; then
    record "session-shim-sockets" "PASS" "0 per-session shim sockets in $SOCK_DIR"
  else
    record "session-shim-sockets" "PASS" "$n per-session shim socket(s): ${socks[*]}"
  fi
}

# check_launchd_service LABEL — read-only liveness via `launchctl print`.
check_launchd_service() {
  local label="$1"
  local name="launchd-${label##*.}"
  if ! command -v launchctl >/dev/null 2>&1; then
    record "$name" "SKIP" "launchctl not available; cannot inspect $label" \
      "run on macOS to inspect launchd services"
    return 0
  fi
  local out
  if ! out="$(launchctl print "gui/$(id -u)/$label" 2>/dev/null)"; then
    record "$name" "FAIL" "$label not loaded in launchd" \
      "install/load it: install.sh --with-agent-shim-services (then launchctl bootstrap)"
    return 0
  fi
  # A loaded-and-running service reports a numeric pid; a loaded-but-dead one
  # reports "state = not running" with no pid.
  local pid
  pid="$(printf '%s\n' "$out" | awk -F'= ' '/^\tpid = /{print $2; exit}')"
  if [ -n "$pid" ]; then
    record "$name" "PASS" "$label loaded and running (pid $pid)"
  else
    record "$name" "FAIL" "$label loaded but not running" \
      "service is crash-looping or throttled; inspect $LOG_DIR/${label##*.}.err.log"
  fi
}

# check_log LABEL — a <service>.log exists and (informationally) its age.
check_log() {
  local svc="$1"
  local logf="$LOG_DIR/$svc.log"
  local name="log-$svc"
  if [ ! -f "$logf" ]; then
    record "$name" "FAIL" "no log file at $logf" \
      "service '$svc' has never written a log; confirm it started"
    return 0
  fi
  local mt now age
  mt="$(file_mtime "$logf")"
  now="$(now_epoch)"
  if [ -n "$mt" ]; then
    age=$((now - mt))
    if [ "$age" -le "$LOG_RECENT_SECS" ]; then
      record "$name" "PASS" "$logf written ${age}s ago"
    else
      record "$name" "PASS" "$logf present but stale (last write ${age}s ago)"
    fi
  else
    record "$name" "PASS" "$logf present (mtime unavailable)"
  fi
}

check_store_db() {
  if [ ! -f "$STORE_DB" ]; then
    record "store-db-present" "FAIL" "no store DB at $STORE_DB" \
      "shim-store has not created its event DB; confirm it started with --db $STORE_DB"
    return 0
  fi
  record "store-db-present" "PASS" "store DB exists at $STORE_DB"

  if ! command -v sqlite3 >/dev/null 2>&1; then
    record "store-db-integrity" "SKIP" "sqlite3 CLI not installed; skipping PRAGMA integrity_check on $STORE_DB" \
      "install the sqlite3 CLI to enable the integrity check"
    return 0
  fi
  local res
  # -readonly guarantees we never mutate the live DB under a running store.
  if ! res="$(sqlite3 -readonly "$STORE_DB" 'PRAGMA integrity_check;' 2>&1)"; then
    record "store-db-integrity" "FAIL" "PRAGMA integrity_check could not run: $res" \
      "the DB may be locked or unreadable; inspect $STORE_DB"
    return 0
  fi
  if [ "$res" = "ok" ]; then
    record "store-db-integrity" "PASS" "PRAGMA integrity_check = ok"
  else
    record "store-db-integrity" "FAIL" "PRAGMA integrity_check reported: $res" \
      "the event DB is corrupt; stop the store and investigate before restarting"
  fi
}

# --- Rendering ----------------------------------------------------------

render_text() {
  local i status line
  echo "agent-shim-doctor — state root: $STATE_ROOT"
  echo
  for i in "${!R_NAME[@]}"; do
    status="${R_STATUS[$i]}"
    printf '[%s] %-28s %s\n' "$status" "${R_NAME[$i]}" "${R_DETAIL[$i]}"
    if [ "$status" != "PASS" ] && [ -n "${R_HINT[$i]}" ]; then
      printf '        hint: %s\n' "${R_HINT[$i]}"
    fi
  done
  echo
  if [ "$FAIL_COUNT" -eq 0 ]; then
    echo "Summary: no failures."
  else
    echo "Summary: $FAIL_COUNT check(s) FAILED."
  fi
  # Suppress unused-var lint on 'line' (reserved for future formatting).
  : "${line:-}"
}

render_json() {
  local i sep=""
  printf '['
  for i in "${!R_NAME[@]}"; do
    printf '%s{"check":"%s","status":"%s","detail":"%s","hint":"%s"}' \
      "$sep" \
      "$(json_escape "${R_NAME[$i]}")" \
      "$(json_escape "${R_STATUS[$i]}")" \
      "$(json_escape "${R_DETAIL[$i]}")" \
      "$(json_escape "${R_HINT[$i]}")"
    sep=","
  done
  printf ']\n'
}

# --- Arg parsing --------------------------------------------------------

while [ $# -gt 0 ]; do
  case "$1" in
    --json) JSON=1 ;;
    -h|--help)
      sed -n '2,30p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    *)
      echo "agent-shim-doctor: unknown argument: $1" >&2
      exit 2
      ;;
  esac
  shift
done

# --- Run ----------------------------------------------------------------

check_store_socket_present
check_store_socket_connectable
check_launchd_service "$STORE_LABEL"
check_launchd_service "$SIDECAR_LABEL"
check_frontend_socket_present
check_session_sockets
check_log "shim-store"
check_log "shim-claude-sidecar"
check_store_db

if [ "$JSON" -eq 1 ]; then
  render_json
else
  render_text
fi

[ "$FAIL_COUNT" -eq 0 ] && exit 0 || exit 1
