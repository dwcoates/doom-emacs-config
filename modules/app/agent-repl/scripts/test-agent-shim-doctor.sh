#!/usr/bin/env bash
# Focused harness for the doctor's store-health client integration and bounded
# large-database integrity policy. All fixtures remain below $TMP.

set -euo pipefail

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)"
DOCTOR="$SCRIPT_DIR/agent-shim-doctor.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-doctor-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

STATE="$TMP/state"
BIN="$TMP/bin"
CALLS="$TMP/sqlite-calls"
HEALTH_CALLS="$TMP/health-calls"
mkdir -p "$STATE/store" "$STATE/sock" "$STATE/log" "$BIN"

# A sparse file exercises the size gate without consuming the represented disk.
truncate -s 2048 "$STATE/store/events.db"

cat >"$BIN/sqlite3" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >>"$DOCTOR_SQLITE_CALLS"
case "$*" in
  *sqlite_schema*) printf '7\n' ;;
  *integrity_check*) printf 'ok\n' ;;
  *) exit 91 ;;
esac
EOF
chmod +x "$BIN/sqlite3"

# Protocol-compatible shim-store CLI fixture. It validates the complete
# one-shot argument contract and emits exactly one HealthStatus JSON object.
cat >"$BIN/shim-store" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

request_id=""
socket=""
timeout=""
health_check=0
while [ "$#" -gt 0 ]; do
  case "$1" in
    -health-check) health_check=1; shift ;;
    -socket) socket="$2"; shift 2 ;;
    -health-request-id) request_id="$2"; shift 2 ;;
    -health-timeout) timeout="$2"; shift 2 ;;
    *) exit 2 ;;
  esac
done
[ "$health_check" -eq 1 ] && [ -n "$socket" ] && [ -n "$request_id" ] && [ -n "$timeout" ] || exit 2
printf '%s|%s|%s|%s\n' "$health_check" "$socket" "$request_id" "$timeout" >>"$DOCTOR_STORE_HEALTH_CALLS"

case "${DOCTOR_HEALTH_FIXTURE:?}" in
  healthy) class=""; healthy=true; reason="ready"; exit_code=0 ;;
  missing_socket) class="missing_socket"; healthy=false; reason="socket missing"; exit_code=10 ;;
  connect_failure) class="connect_failure"; healthy=false; reason="dial rejected"; exit_code=11 ;;
  write_failure) class="write_failure"; healthy=false; reason="write rejected"; exit_code=12 ;;
  timeout) class="timeout"; healthy=false; reason="deadline exceeded"; exit_code=13 ;;
  decode_failure) class="decode_failure"; healthy=false; reason="invalid response"; exit_code=14 ;;
  mismatched_request_id) class="mismatched_request_id"; healthy=false; reason="response id differs"; exit_code=15 ;;
  unhealthy_response) class="unhealthy_response"; healthy=false; reason="store draining"; exit_code=16 ;;
  unexpected_exit) class="unexpected_exit"; healthy=false; reason="client invariant failed"; exit_code=17 ;;
  *) exit 2 ;;
esac
printf '{"request_id":"%s","latency_ms":17,"component":"shim-store","healthy":%s,"failure_class":"%s","reason":"%s"}\n' \
  "$request_id" "$healthy" "$class" "$reason"
exit "$exit_code"
EOF
chmod +x "$BIN/shim-store"

run_doctor_json() {
  local fixture="$1"
  set +e
  DOCTOR_HEALTH_FIXTURE="$fixture" \
  DOCTOR_SQLITE_CALLS="$CALLS" \
  DOCTOR_STORE_HEALTH_CALLS="$HEALTH_CALLS" \
  AGENT_REPL_STATE_ROOT="$STATE" \
  AGENT_REPL_DOCTOR_SHIM_STORE_BIN="$BIN/shim-store" \
  AGENT_REPL_DOCTOR_INTEGRITY_AUTO_MAX_BYTES=1024 \
  PATH="$BIN:/usr/bin:/bin" \
  "$DOCTOR" --json
  local rc=$?
  set -e
  [ "$rc" -eq 1 ] || {
    printf 'FAIL: fixture %s exit=%s, want 1 from unrelated missing-service checks\n' "$fixture" "$rc" >&2
    exit 1
  }
}

assert_valid_json() {
  JSON_INPUT="$1" python3 -c 'import json, os; json.loads(os.environ["JSON_INPUT"])' || {
    printf 'FAIL: doctor emitted invalid JSON: %s\n' "$1" >&2
    exit 1
  }
}

OUT="$(run_doctor_json healthy)"
assert_valid_json "$OUT"
printf '%s\n' "$OUT" | grep -q '"check":"store-socket-connectable","status":"PASS"' || {
  printf 'FAIL: healthy store health did not pass: %s\n' "$OUT" >&2
  exit 1
}
printf '%s\n' "$OUT" | grep -Eq '"metadata":\{"request_id":"doctor-[^"]+","latency_ms":17,"component":"shim-store","healthy":true,"failure_class":"","reason":"ready"\}' || {
  printf 'FAIL: healthy response metadata was not retained verbatim: %s\n' "$OUT" >&2
  exit 1
}
grep -Eq '^1\|.*/sock/store\.sock\|doctor-[^|]+\|2s$' "$HEALTH_CALLS" || {
  printf 'FAIL: doctor did not use the complete health client contract\n' >&2
  exit 1
}
OUT_BOUNDED="$OUT"

for fixture in missing_socket connect_failure write_failure timeout decode_failure mismatched_request_id unhealthy_response; do
  OUT="$(run_doctor_json "$fixture")"
  assert_valid_json "$OUT"
  printf '%s\n' "$OUT" | grep -q '"check":"store-socket-connectable","status":"FAIL"' || {
    printf 'FAIL: %s did not fail store health: %s\n' "$fixture" "$OUT" >&2
    exit 1
  }
  printf '%s\n' "$OUT" | grep -q "\"failure_class\":\"$fixture\"" || {
    printf 'FAIL: %s lost its exact failure class: %s\n' "$fixture" "$OUT" >&2
    exit 1
  }
done

OUT="$(run_doctor_json unexpected_exit)"
assert_valid_json "$OUT"
printf '%s\n' "$OUT" | grep -q '"failure_class":"unexpected_exit"' || {
  printf 'FAIL: unexpected helper exit lost its explicit classification: %s\n' "$OUT" >&2
  exit 1
}

set +e
OUT_UNAVAILABLE="$(DOCTOR_SQLITE_CALLS="$CALLS" AGENT_REPL_STATE_ROOT="$STATE" AGENT_REPL_DOCTOR_SHIM_STORE_BIN="$BIN/not-installed" AGENT_REPL_DOCTOR_INTEGRITY_AUTO_MAX_BYTES=1024 PATH="$BIN:/usr/bin:/bin" "$DOCTOR" --json)"
RC=$?
set -e
[ "$RC" -eq 1 ] || {
  printf 'FAIL: unavailable helper exit=%s, want 1\n' "$RC" >&2
  exit 1
}
assert_valid_json "$OUT_UNAVAILABLE"
printf '%s\n' "$OUT_UNAVAILABLE" | grep -q '"failure_class":"client_unavailable"' || {
  printf 'FAIL: unavailable helper lost its explicit classification: %s\n' "$OUT_UNAVAILABLE" >&2
  exit 1
}

OUT_TEXT="$(DOCTOR_HEALTH_FIXTURE=timeout DOCTOR_SQLITE_CALLS="$CALLS" DOCTOR_STORE_HEALTH_CALLS="$HEALTH_CALLS" AGENT_REPL_STATE_ROOT="$STATE" AGENT_REPL_DOCTOR_SHIM_STORE_BIN="$BIN/shim-store" AGENT_REPL_DOCTOR_INTEGRITY_AUTO_MAX_BYTES=1024 PATH="$BIN:/usr/bin:/bin" "$DOCTOR" 2>/dev/null || true)"
printf '%s\n' "$OUT_TEXT" | grep -q 'store health check failed with timeout' || {
  printf 'FAIL: text output did not report the exact failure class: %s\n' "$OUT_TEXT" >&2
  exit 1
}
printf '%s\n' "$OUT_TEXT" | grep -q 'deadline exceeded' || {
  printf 'FAIL: text output did not retain the helper reason: %s\n' "$OUT_TEXT" >&2
  exit 1
}

OUT="$OUT_BOUNDED"
printf '%s\n' "$OUT" | grep -q '"check":"store-db-openable","status":"PASS"' || {
  printf 'FAIL: missing openable PASS: %s\n' "$OUT" >&2
  exit 1
}
printf '%s\n' "$OUT" | grep -q '"check":"store-db-integrity","status":"SKIP"' || {
  printf 'FAIL: missing oversized integrity SKIP: %s\n' "$OUT" >&2
  exit 1
}
if grep -q 'integrity_check' "$CALLS"; then
  printf 'FAIL: bounded run executed the deep integrity scan\n' >&2
  exit 1
fi

set +e
OUT_DEEP="$(DOCTOR_HEALTH_FIXTURE=healthy DOCTOR_SQLITE_CALLS="$CALLS" DOCTOR_STORE_HEALTH_CALLS="$HEALTH_CALLS" AGENT_REPL_STATE_ROOT="$STATE" AGENT_REPL_DOCTOR_SHIM_STORE_BIN="$BIN/shim-store" AGENT_REPL_DOCTOR_INTEGRITY_AUTO_MAX_BYTES=1024 PATH="$BIN:/usr/bin:/bin" "$DOCTOR" --json --deep-integrity)"
RC=$?
set -e
[ "$RC" -eq 1 ] || {
  printf 'FAIL: deep run exit=%s, want 1 from missing services\n' "$RC" >&2
  exit 1
}
printf '%s\n' "$OUT_DEEP" | grep -q '"check":"store-db-integrity","status":"PASS"' || {
  printf 'FAIL: deep integrity did not pass: %s\n' "$OUT_DEEP" >&2
  exit 1
}
grep -q 'integrity_check' "$CALLS" || {
  printf 'FAIL: --deep-integrity did not execute PRAGMA integrity_check\n' >&2
  exit 1
}

printf 'PASS: doctor uses correlated store-health results and bounds integrity scans\n'
