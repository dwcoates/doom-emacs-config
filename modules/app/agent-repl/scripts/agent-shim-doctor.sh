#!/usr/bin/env bash
# agent-shim-doctor.sh — read-only diagnostics for the agent-shim ecosystem
# (see scripts/AGENTS.md; the store's contract is docs/overhaul/store.md).
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
# Store health has NO verb: store.v1 deliberately defines none, so there is no
# one-shot client to invoke. The doctor instead probes the store's real Connect
# endpoints directly over its UNIX domain socket with curl -- GetLiveWork and
# GetSidecarCursors, both pure reads that mutate nothing. GetLiveWork is asked
# about a sentinel session that owns nothing, so a healthy store answers it an
# empty success: an UNSCOPED request would be refused, and the store rightly
# records every refusal at WARN, so provoking one would put a warning in the
# store's log on every health check. A healthy store
# answers HTTP 200 with a JSON body whose single top-level key is `success`
# (an empty success serializes as {"success":{}}). A `failure` key, a non-200
# status, a refused connection, a missing socket, a timeout and a malformed
# body are each a distinct failure class with its own hint. Every probe sends
# an X-Agent-Repl-Request-Id header and reports that id plus its latency.
# Tests override the per-probe deadline with
# AGENT_REPL_DOCTOR_STORE_PROBE_TIMEOUT (seconds).
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
# GetLiveWork's probe request: a sentinel main-agent id no session ever owns, so
# the store's scoped read answers an empty success. The id names the doctor, so
# a store record of the read says who asked.
LIVE_WORK_PROBE_REQUEST='{"session":{"value":"agent-shim-doctor-probe"}}'

# Per-probe deadline in whole seconds, handed to curl --max-time.
STORE_PROBE_TIMEOUT="${AGENT_REPL_DOCTOR_STORE_PROBE_TIMEOUT:-2}"

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

# store_probe_metadata REQUEST_ID LATENCY_MS RPC HEALTHY FAILURE_CLASS REASON HTTP_STATUS
# Emits the doctor result metadata for one Connect probe. HTTP_STATUS is a bare
# JSON number, or the literal null when no response ever arrived.
store_probe_metadata() {
  printf '{"request_id":"%s","latency_ms":%s,"component":"shim-store","rpc":"%s","healthy":%s,"failure_class":"%s","reason":"%s","http_status":%s}' \
    "$(json_escape "$1")" "$2" "$(json_escape "$3")" "$4" \
    "$(json_escape "$5")" "$(json_escape "$6")" "$7"
}

# store_probe_body_key FILE — reads a Connect JSON response body and prints the
# response's single top-level key on the first line ("success" or "failure";
# "?multi" when the object carries more than one key), that arm's `detail`
# string on the second line when it has one, the failure arm's `kind` oneof
# field name (snake_case, e.g. "invalid_request") on the third line, and — for
# an invalid_request kind only — the offending `field` name on the fourth
# line. The last two lines are empty for a success arm or any other kind.
#
# Exit 0  the body is a JSON object and the key was printed.
# Exit 3  the body is not a JSON object (malformed or a JSON non-object).
store_probe_body_key() {
  python3 - "$1" <<'PY'
import json, re, sys

try:
    with open(sys.argv[1]) as fh:
        doc = json.load(fh)
except Exception:
    sys.exit(3)
if not isinstance(doc, dict):
    sys.exit(3)
keys = list(doc)
if len(keys) != 1:
    print("?multi")
    print("")
    print("")
    print("")
    sys.exit(0)
key = keys[0]
arm = doc[key]
detail = arm.get("detail", "") if isinstance(arm, dict) else ""
kind = ""
field = ""
if isinstance(arm, dict):
    # The failure arm's oneof `kind` serializes as whichever sub-message key
    # is present besides `detail`; protojson spells it camelCase.
    for k, v in arm.items():
        if k == "detail":
            continue
        kind = re.sub(r"(?<!^)(?=[A-Z])", "_", k).lower()
        if isinstance(v, dict):
            field = v.get("field", "") if isinstance(v.get("field", ""), str) else ""
        break
print(key)
print(detail if isinstance(detail, str) else "")
print(kind)
print(field)
PY
}

# check_store_connect_rpc RPC CHECK_SUFFIX REQUEST_JSON — probe one
# store.v1.ShimStore endpoint over the store's UDS with REQUEST_JSON as the
# Connect JSON request body. STRICTLY READ-ONLY: both probed rpcs are pure
# reads, so the probe can never mutate store state. ONLY the success arm
# passes: every request the doctor sends is one a healthy store answers, so any
# refusal is a finding, never an expected answer.
check_store_connect_rpc() {
  local rpc="$1"
  local name="store-connect-$2"
  local request_json="$3"
  local procedure="store.v1.ShimStore/$rpc"
  local request_id url body err out rc http_status latency_ms
  local failure_class reason hint key detail kind field instr http_status_json

  request_id="doctor-$(date +%s)-$$-$RANDOM"
  url="http://store/$procedure"
  instr="{\"rpc\":\"$(json_escape "$procedure")\",\"socket\":\"$(json_escape "$STORE_SOCK")\",\"request_id\":\"$request_id\",\"timeout_s\":$STORE_PROBE_TIMEOUT"

  # The probe needs curl to speak the UDS and python3 to classify the body.
  # Their absence is an honest SKIP, never a pass and never a silent
  # alternative classification.
  if ! command -v curl >/dev/null 2>&1; then
    record "$name" "SKIP" \
      "curl not installed; cannot probe $procedure over $STORE_SOCK (request_id=$request_id)" \
      "install curl to enable the store Connect probes" \
      "$(store_probe_metadata "$request_id" 0 "$procedure" false "prober_unavailable" "curl is not installed" null)" \
      "$instr}"
    return 0
  fi
  if ! command -v python3 >/dev/null 2>&1; then
    record "$name" "SKIP" \
      "python3 not installed; cannot classify the $procedure response body (request_id=$request_id)" \
      "install python3 to enable the store Connect probes" \
      "$(store_probe_metadata "$request_id" 0 "$procedure" false "prober_unavailable" "python3 is not installed" null)" \
      "$instr}"
    return 0
  fi

  if [ ! -S "$STORE_SOCK" ]; then
    record "$name" "FAIL" \
      "no store socket at $STORE_SOCK; $procedure unreachable (request_id=$request_id)" \
      "shim-store is not serving; check '$STORE_LABEL' via launchctl or (re)run install.sh --with-agent-shim-services" \
      "$(store_probe_metadata "$request_id" 0 "$procedure" false "missing_socket" "the store socket does not exist" null)" \
      "$instr,\"curl_exit\":null}"
    return 0
  fi

  body="$(mktemp "${TMPDIR:-/tmp}/agent-shim-doctor-body.XXXXXX")"
  err="$(mktemp "${TMPDIR:-/tmp}/agent-shim-doctor-err.XXXXXX")"
  set +e
  out="$(curl --silent --show-error --max-time "$STORE_PROBE_TIMEOUT" \
    --unix-socket "$STORE_SOCK" \
    -H 'Content-Type: application/json' \
    -H "X-Agent-Repl-Request-Id: $request_id" \
    -X POST "$url" -d "$request_json" \
    -o "$body" -w '%{http_code} %{time_total}' 2>"$err")"
  rc=$?
  set -e
  http_status="${out%% *}"
  latency_ms="$(awk -v t="${out##* }" 'BEGIN { if (t == "") t = 0; printf "%d", t * 1000 }')"
  # curl reports 000 when no response line ever arrived; that is not a JSON
  # number, so the metadata carries null for it rather than an invalid literal.
  case "$http_status" in
    [1-9][0-9][0-9]) http_status_json="$http_status" ;;
    *) http_status="000"; http_status_json="null" ;;
  esac
  instr="$instr,\"curl_exit\":$rc,\"http_status\":\"$(json_escape "$http_status")\",\"latency_ms\":$latency_ms}"
  reason="$(head -c 300 "$err" | tr '\n\t' '  ')"
  [ -n "$reason" ] || reason="curl exited $rc"

  if [ "$rc" -ne 0 ]; then
    case "$rc" in
      7)
        failure_class="connection_refused"
        hint="the socket exists but nothing is accepting on it; '$STORE_LABEL' is down, wedged, or left a stale socket file — inspect it and $LOG_DIR/shim-store.log"
        ;;
      28)
        failure_class="timeout"
        hint="shim-store did not answer $procedure within ${STORE_PROBE_TIMEOUT}s; inspect '$STORE_LABEL' responsiveness and $LOG_DIR/shim-store.log"
        ;;
      *)
        failure_class="transport_failure"
        hint="curl could not complete the Connect request (exit $rc); inspect $STORE_SOCK ownership and '$STORE_LABEL'"
        ;;
    esac
    record "$name" "FAIL" \
      "$procedure probe failed with $failure_class (request_id=$request_id; curl_exit=$rc; latency_ms=$latency_ms; reason=$reason)" \
      "$hint" \
      "$(store_probe_metadata "$request_id" "$latency_ms" "$procedure" false "$failure_class" "$reason" null)" \
      "$instr"
    rm -f "$body" "$err"
    return 0
  fi

  if [ "$http_status" != "200" ]; then
    reason="$(head -c 300 "$body" | tr '\n\t' '  ')"
    [ -n "$reason" ] || reason="empty body"
    record "$name" "FAIL" \
      "$procedure answered HTTP $http_status (request_id=$request_id; latency_ms=$latency_ms; body=$reason)" \
      "a served Connect endpoint answers 200 even when the store REFUSES the call; HTTP $http_status means the request never reached a handler — inspect '$STORE_LABEL' and $LOG_DIR/shim-store.log" \
      "$(store_probe_metadata "$request_id" "$latency_ms" "$procedure" false "http_status" "$reason" "$http_status_json")" \
      "$instr"
    rm -f "$body" "$err"
    return 0
  fi

  set +e
  out="$(store_probe_body_key "$body")"
  rc=$?
  set -e
  if [ "$rc" -ne 0 ]; then
    reason="$(head -c 300 "$body" | tr '\n\t' '  ')"
    [ -n "$reason" ] || reason="empty body"
    record "$name" "FAIL" \
      "$procedure answered HTTP 200 with a body that is not a JSON object (request_id=$request_id; latency_ms=$latency_ms; body=$reason)" \
      "the store answered something that is not a Connect JSON response; inspect '$STORE_LABEL' and $LOG_DIR/shim-store.log for a protocol fault" \
      "$(store_probe_metadata "$request_id" "$latency_ms" "$procedure" false "malformed_response" "$reason" "$http_status_json")" \
      "$instr"
    rm -f "$body" "$err"
    return 0
  fi
  key="$(printf '%s\n' "$out" | sed -n '1p')"
  detail="$(printf '%s\n' "$out" | sed -n '2p')"
  kind="$(printf '%s\n' "$out" | sed -n '3p')"
  field="$(printf '%s\n' "$out" | sed -n '4p')"
  rm -f "$body" "$err"

  case "$key" in
    success)
      record "$name" "PASS" \
        "$procedure answered success (request_id=$request_id; latency_ms=$latency_ms)" \
        "" \
        "$(store_probe_metadata "$request_id" "$latency_ms" "$procedure" true "" "store answered the success arm" "$http_status_json")" \
        "$instr"
      ;;
    failure)
      [ -n "$detail" ] || detail="the store returned the failure arm with no detail"
      record "$name" "FAIL" \
        "$procedure answered the failure arm (request_id=$request_id; latency_ms=$latency_ms; kind=$kind; field=$field; detail=$detail)" \
        "the store is serving but REFUSED this read; its detail names the cause — inspect $LOG_DIR/shim-store.log for the matching refusal record" \
        "$(store_probe_metadata "$request_id" "$latency_ms" "$procedure" false "failure_arm" "$detail" "$http_status_json")" \
        "$instr"
      ;;
    *)
      record "$name" "FAIL" \
        "$procedure answered HTTP 200 with an unrecognized response shape (request_id=$request_id; latency_ms=$latency_ms; top_level_key=$key)" \
        "every store.v1 response is a oneof of success|failure; a body shaped otherwise means the doctor and the store disagree on the contract — inspect '$STORE_LABEL' and its build" \
        "$(store_probe_metadata "$request_id" "$latency_ms" "$procedure" false "unexpected_body" "top-level key was $key" "$http_status_json")" \
        "$instr"
      ;;
  esac
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
  # READ WHOLE, never through a pipe into an awk that exits at its match: under
  # pipefail the printf writing launchctl's output died of SIGPIPE whenever awk
  # exited first, and the doctor itself exited 141 (2026-10-06).
  pid="$(awk -F'= ' '/^\tpid = / && !found {print $2; found = 1}' <<<"$out")"
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
    printf '[%s] %-34s %s\n' "$status" "${R_NAME[$i]}" "${R_DETAIL[$i]}"
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
      awk 'NR > 1 { if (!/^#/) exit; sub(/^# ?/, ""); print }' "$0"
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
check_store_connect_rpc GetLiveWork get-live-work "$LIVE_WORK_PROBE_REQUEST"
check_store_connect_rpc GetSidecarCursors get-sidecar-cursors '{}'
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
