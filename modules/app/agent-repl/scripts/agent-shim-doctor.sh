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
#   SKIP  the check could not run because an optional tool is absent or the
#         bounded sweep explicitly declined an unbounded deep scan. A SKIP is
#         an honest environment report, never a pass or a silent alternative.
#
# This script is STRICTLY READ-ONLY. It never starts, stops, restarts, loads,
# unloads, or otherwise mutates any service, socket, file, or launchd state.
# It only stats files, opens (and immediately closes) sockets, and issues
# read-only `launchctl print` / SQLite queries. Full integrity scans of large
# live databases are opt-in because they can run for hours.
#
# State root: defaults to ${XDG_CACHE_HOME:-$HOME/.cache}/agent-repl. Override
# with AGENT_REPL_STATE_ROOT (used by the unit dry-run to point at a fabricated
# temp dir so the real cache is never touched).
#
# Store health uses the installed shim-store one-shot client.  Its JSON response
# is retained verbatim in the doctor result metadata, rather than reimplementing
# the correlated HealthCheck protocol in shell.  Tests may explicitly override
# the binary with AGENT_REPL_DOCTOR_SHIM_STORE_BIN.
#
# Usage:
#   agent-shim-doctor.sh [--json] [--deep-integrity]
#     --json             emit a machine-readable JSON array instead of text lines.
#     --deep-integrity   force PRAGMA integrity_check even when the database is
#                        larger than the automatic-scan threshold.
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
STORE_HEALTH_CLIENT="${AGENT_REPL_DOCTOR_SHIM_STORE_BIN:-$STATE_ROOT/bin/shim-store}"
STORE_HEALTH_TIMEOUT="${AGENT_REPL_DOCTOR_STORE_HEALTH_TIMEOUT:-2s}"

STORE_LABEL="com.agentrepl.shim-store"
SIDECAR_LABEL="com.agentrepl.shim-claude-sidecar"

# A log is considered "recently written" if touched within this many seconds.
# Beyond it the log still PASSes (an idle service writes nothing) but the age
# is surfaced so a wedged/never-started service is visible.
LOG_RECENT_SECS=900

JSON=0
DEEP_INTEGRITY=0
# A 34 GiB production store made the nominal health sweep block indefinitely.
# The routine probe declines an automatic deep scan above this size and reports
# an explicit SKIP. --deep-integrity remains available for a maintenance window.
INTEGRITY_AUTO_MAX_BYTES="${AGENT_REPL_DOCTOR_INTEGRITY_AUTO_MAX_BYTES:-1073741824}"

# --- Result accumulation ------------------------------------------------

# Parallel arrays: one entry per check.
R_NAME=()
R_STATUS=()
R_DETAIL=()
R_HINT=()
R_METADATA=()
R_INSTRUMENTATION=()
FAIL_COUNT=0

# record NAME STATUS DETAIL [HINT [METADATA_JSON [INSTRUMENTATION_JSON]]]
record() {
  R_NAME+=("$1")
  R_STATUS+=("$2")
  R_DETAIL+=("$3")
  R_HINT+=("${4:-}")
  R_METADATA+=("${5:-null}")
  R_INSTRUMENTATION+=("${6:-null}")
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

file_size() {
  # Prints the file size in bytes, or nothing if stat fails.
  stat -f %z "$1" 2>/dev/null || stat -c %s "$1" 2>/dev/null || true
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
  local request_id output exit_code failure_class hint instrumentation
  request_id="doctor-$(date +%s)-$$-$RANDOM"
  instrumentation="{\"client\":\"$(json_escape "$STORE_HEALTH_CLIENT")\",\"socket\":\"$(json_escape "$STORE_SOCK")\",\"request_id\":\"$request_id\",\"timeout\":\"$(json_escape "$STORE_HEALTH_TIMEOUT")\""

  if [ ! -x "$STORE_HEALTH_CLIENT" ]; then
    record "store-socket-connectable" "FAIL" \
      "store health client unavailable at $STORE_HEALTH_CLIENT (request_id=$request_id)" \
      "install shim-store at $STATE_ROOT/bin/shim-store before running doctor" \
      "{\"request_id\":\"$request_id\",\"latency_ms\":0,\"component\":\"shim-store-client\",\"healthy\":false,\"failure_class\":\"client_unavailable\",\"reason\":\"health client is not executable\"}" \
      "${instrumentation}}"
    return 0
  fi

  if output="$("$STORE_HEALTH_CLIENT" -health-check -socket "$STORE_SOCK" -log "$LOG_DIR/shim-store.log" -health-request-id "$request_id" -health-timeout "$STORE_HEALTH_TIMEOUT")"; then
    exit_code=0
  else
    exit_code=$?
  fi
  instrumentation="${instrumentation},\"exit_code\":$exit_code}"

  case "$exit_code" in
    0)
      record "store-socket-connectable" "PASS" \
        "store health check passed (request_id=$request_id; response=$output)" \
        "" "$output" "$instrumentation"
      ;;
    10)
      failure_class="missing_socket"
      hint="shim-store socket is absent; start '$STORE_LABEL' and confirm $STORE_SOCK is created"
      ;;
    11)
      failure_class="connect_failure"
      hint="shim-store could not accept the health connection; inspect '$STORE_LABEL' liveness and its log"
      ;;
    12)
      failure_class="write_failure"
      hint="the store health request could not be written; inspect '$STORE_LABEL' and socket ownership"
      ;;
    13)
      failure_class="timeout"
      hint="shim-store did not answer before $STORE_HEALTH_TIMEOUT; inspect '$STORE_LABEL' responsiveness and logs"
      ;;
    14)
      failure_class="decode_failure"
      hint="shim-store returned an invalid health response; inspect '$STORE_LABEL' protocol logs"
      ;;
    15)
      failure_class="mismatched_request_id"
      hint="shim-store returned a health response for another request; inspect protocol correlation in '$STORE_LABEL'"
      ;;
    16)
      failure_class="unhealthy_response"
      hint="shim-store reported itself unhealthy; inspect its health reason and service log"
      ;;
    17)
      failure_class="client_failure"
      hint="the owned shim-store health client failed before completing the protocol probe; inspect its reason and canonical log"
      ;;
    *)
      failure_class="unexpected_exit"
      hint="shim-store health client exited unexpectedly; inspect the client and '$STORE_LABEL' logs"
      ;;
  esac
  record "store-socket-connectable" "FAIL" \
    "store health check failed with $failure_class (request_id=$request_id; exit_code=$exit_code; response=$output)" \
    "$hint" "$output" "$instrumentation"
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
  local res size
  # Prove SQLite can open and read the live schema before deciding whether the
  # potentially enormous page scan belongs in this bounded health sweep.
  if ! res="$(sqlite3 -readonly "$STORE_DB" 'PRAGMA query_only=ON; SELECT count(*) FROM sqlite_schema;' 2>&1)"; then
    record "store-db-openable" "FAIL" "read-only schema query could not run: $res" \
      "the DB may be locked or unreadable; inspect $STORE_DB"
    return 0
  fi
  record "store-db-openable" "PASS" "read-only schema query succeeded (objects=$res)"

  size="$(file_size "$STORE_DB")"
  if [ "$DEEP_INTEGRITY" -ne 1 ] && [ -n "$size" ] &&
     [ "$size" -gt "$INTEGRITY_AUTO_MAX_BYTES" ]; then
    record "store-db-integrity" "SKIP" \
      "database is ${size} bytes, above the ${INTEGRITY_AUTO_MAX_BYTES}-byte automatic deep-scan limit" \
      "run agent-shim-doctor.sh --deep-integrity during a maintenance window to execute PRAGMA integrity_check"
    return 0
  fi

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
  local i status line metadata instrumentation
  echo "agent-shim-doctor — state root: $STATE_ROOT"
  echo
  for i in "${!R_NAME[@]}"; do
    status="${R_STATUS[$i]}"
    printf '[%s] %-28s %s\n' "$status" "${R_NAME[$i]}" "${R_DETAIL[$i]}"
    if [ "$status" != "PASS" ] && [ -n "${R_HINT[$i]}" ]; then
      printf '        hint: %s\n' "${R_HINT[$i]}"
    fi
    metadata="${R_METADATA[$i]}"
    instrumentation="${R_INSTRUMENTATION[$i]}"
    if [ "$metadata" != "null" ]; then
      printf '        metadata: %s\n' "$metadata"
    fi
    if [ "$instrumentation" != "null" ]; then
      printf '        instrumentation: %s\n' "$instrumentation"
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
    printf '%s{"check":"%s","status":"%s","detail":"%s","hint":"%s","metadata":%s,"instrumentation":%s}' \
      "$sep" \
      "$(json_escape "${R_NAME[$i]}")" \
      "$(json_escape "${R_STATUS[$i]}")" \
      "$(json_escape "${R_DETAIL[$i]}")" \
      "$(json_escape "${R_HINT[$i]}")" \
      "${R_METADATA[$i]}" \
      "${R_INSTRUMENTATION[$i]}"
    sep=","
  done
  printf ']\n'
}

# --- Arg parsing --------------------------------------------------------

while [ $# -gt 0 ]; do
  case "$1" in
    --json) JSON=1 ;;
    --deep-integrity) DEEP_INTEGRITY=1 ;;
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
