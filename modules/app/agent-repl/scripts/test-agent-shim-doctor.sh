#!/usr/bin/env bash
# Focused harness for the doctor's bounded large-database integrity policy.

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
DOCTOR="$SCRIPT_DIR/agent-shim-doctor.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-doctor-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

STATE="$TMP/state"
BIN="$TMP/bin"
CALLS="$TMP/sqlite-calls"
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

# Other missing-service failures are expected in this focused fixture. Select
# only the database records and the sqlite call trace below.
set +e
OUT="$(
  PATH="$BIN:/usr/bin:/bin" \
  DOCTOR_SQLITE_CALLS="$CALLS" \
  AGENT_REPL_STATE_ROOT="$STATE" \
  AGENT_REPL_DOCTOR_INTEGRITY_AUTO_MAX_BYTES=1024 \
  "$DOCTOR" --json
)"
RC=$?
set -e
[ "$RC" -eq 1 ] || {
  printf 'FAIL: bounded run exit=%s, want 1 from missing services\n' "$RC" >&2
  exit 1
}
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
OUT_DEEP="$(
  PATH="$BIN:/usr/bin:/bin" \
  DOCTOR_SQLITE_CALLS="$CALLS" \
  AGENT_REPL_STATE_ROOT="$STATE" \
  AGENT_REPL_DOCTOR_INTEGRITY_AUTO_MAX_BYTES=1024 \
  "$DOCTOR" --json --deep-integrity
)"
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

printf 'PASS: doctor bounds automatic integrity scans and honors --deep-integrity\n'
