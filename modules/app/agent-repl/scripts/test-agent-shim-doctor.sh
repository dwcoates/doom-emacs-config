#!/usr/bin/env bash
# Focused harness for agent-shim-doctor.sh's store Connect probes and its
# bounded large-database integrity policy. All fixtures remain below $TMP.
#
# The store probe is exercised against fake-store-fixture.py — a scripted
# Connect server on a temporary UNIX socket. Nothing here builds or runs the
# real shim-store binary, so the harness stays deterministic and independent
# of the Go modules being rewritten alongside it.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/../bin/lib-grep-in.sh"

SCRIPT_DIR="$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)"
DOCTOR="$SCRIPT_DIR/agent-shim-doctor.sh"
FAKE_STORE="$SCRIPT_DIR/fake-store-fixture.py"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-doctor-test.XXXXXX")"
FAKE_PID=""

cleanup() {
  if [ -n "$FAKE_PID" ]; then
    kill "$FAKE_PID" 2>/dev/null || true
    wait "$FAKE_PID" 2>/dev/null || true
  fi
  rm -rf "$TMP"
}
trap cleanup EXIT HUP INT TERM

STATE="$TMP/state"
BIN="$TMP/bin"
CALLS="$TMP/sqlite-calls"
STORE_CALLS="$TMP/store-calls"
READY_FIFO="$TMP/ready.fifo"
mkdir -p "$STATE/store" "$STATE/sock" "$STATE/log" "$BIN"

STORE_SOCK="$STATE/sock/store.sock"
# The probe deadline the doctor is given, and the delay the "slow" fixture
# holds the response for. The gap is what the timeout case asserts.
PROBE_TIMEOUT=1
SLOW_SECONDS=5

# curl and python3 must be reachable through the pinned PATH the doctor runs
# under, otherwise its honest SKIP branches would mask the probe assertions.
TOOL_DIRS="$(dirname "$(command -v curl)"):$(dirname "$(command -v python3)")"
DOCTOR_PATH="$BIN:$TOOL_DIRS:/usr/bin:/bin"

# A sparse file exercises the size gate without consuming the represented disk.
truncate -s 2048 "$STATE/store/events.db"

# THE OWNER'S LAUNCHD IS NEVER ASKED. /bin is on the doctor's PATH, so without
# this stub every case read the host's real store and sidecar services: the
# "no loaded launchd services" premise held only on a host without them, and
# the real services' `launchctl print` output is what met the doctor's
# early-exiting pid reader (exit 141, 2026-10-06). The stub answers as
# launchctl does for a service that is not loaded.
#
# DOCTOR_LAUNCHCTL_PRINT names a file the stub prints instead, as launchctl
# prints a loaded service.
cat >"$BIN/launchctl" <<'EOF'
#!/usr/bin/env bash
if [ -n "${DOCTOR_LAUNCHCTL_PRINT:-}" ]; then
  cat "$DOCTOR_LAUNCHCTL_PRINT"
  exit 0
fi
echo "Could not find service in domain for port" >&2
exit 113
EOF
chmod +x "$BIN/launchctl"

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

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  exit 1
}

# start_fake_store MODE — bring up the scripted Connect server and block until
# it is accepting. The FIFO write in the fixture completes only after
# bind+listen, so this read IS the readiness signal; there is no sleep-poll.
start_fake_store() {
  rm -f "$READY_FIFO" "$STORE_SOCK"
  mkfifo "$READY_FIFO"
  python3 "$FAKE_STORE" "$STORE_SOCK" "$1" "$READY_FIFO" "$STORE_CALLS" "$SLOW_SECONDS" \
    >"$TMP/fake-store.out" 2>"$TMP/fake-store.err" &
  FAKE_PID=$!
  read -r -t 20 _ <"$READY_FIFO" || {
    cat "$TMP/fake-store.err" >&2
    fail "fake store ($1) never signalled readiness"
  }
}

stop_fake_store() {
  [ -n "$FAKE_PID" ] || return 0
  kill "$FAKE_PID" 2>/dev/null || true
  wait "$FAKE_PID" 2>/dev/null || true
  FAKE_PID=""
  rm -f "$STORE_SOCK"
}

# A bound-then-closed socket file: the path is a real socket, so the doctor's
# presence check passes, but connect() gets ECONNREFUSED.
make_refusing_socket() {
  rm -f "$STORE_SOCK"
  SOCKET_PATH="$STORE_SOCK" python3 -c '
import os, socket
p = os.environ["SOCKET_PATH"]
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
s.bind(p)
s.close()
'
  [ -S "$STORE_SOCK" ] || fail "refusing-socket fixture did not create a socket file"
}

# run_doctor [extra doctor args...] — always exits 1 here, because the
# fabricated state root has no daemon frontend socket and no loaded launchd
# services. The store assertions are made on the emitted records.
run_doctor() {
  local rc
  set +e
  DOCTOR_OUT="$(DOCTOR_SQLITE_CALLS="$CALLS" \
    AGENT_REPL_STATE_ROOT="$STATE" \
    AGENT_REPL_DOCTOR_STORE_PROBE_TIMEOUT="$PROBE_TIMEOUT" \
    AGENT_REPL_DOCTOR_INTEGRITY_AUTO_MAX_BYTES=1024 \
    PATH="$DOCTOR_PATH" \
    "$DOCTOR" "$@" 2>"$TMP/doctor.err")"
  rc=$?
  set -e
  [ "$rc" -eq 1 ] || fail "doctor exit=$rc, want 1 from the unrelated missing-service checks"
}

assert_valid_json() {
  JSON_INPUT="$1" python3 -c 'import json, os; json.loads(os.environ["JSON_INPUT"])' ||
    fail "doctor emitted invalid JSON: $1"
}

# assert_probe_status STATUS — both store-connect records carry STATUS, and
# there are exactly two of them (one per probed rpc).
assert_probe_status() {
  local want="$1" matches n
  # `|| true` is load-bearing: under `set -o pipefail` a zero-match grep would
  # otherwise abort the harness here, before this helper's own assertion could
  # report which status was actually emitted.
  matches="$(grep_in "$DOCTOR_OUT" -o "\"check\":\"store-connect-[a-z-]*\",\"status\":\"$want\"" || true)"
  n="$(grep_in "$matches" -c . || true)"
  [ "$n" -eq 2 ] ||
    fail "expected 2 store-connect records with status $want, got $n: $DOCTOR_OUT"
}

assert_probe_class() {
  grep_in "$DOCTOR_OUT" -q "\"failure_class\":\"$1\"" ||
    fail "probe lost its exact failure class $1: $DOCTOR_OUT"
}

# ---- healthy: a success arm on both probed endpoints -------------------

start_fake_store healthy
run_doctor --json
stop_fake_store
assert_valid_json "$DOCTOR_OUT"
assert_probe_status PASS
grep_in "$DOCTOR_OUT" -q '"check":"store-connect-get-live-work","status":"PASS"' ||
  fail "GetLiveWork probe did not pass: $DOCTOR_OUT"
grep_in "$DOCTOR_OUT" -q '"check":"store-connect-get-sidecar-cursors","status":"PASS"' ||
  fail "GetSidecarCursors probe did not pass: $DOCTOR_OUT"
if grep_in "$DOCTOR_OUT" -q '"check":"store-connect-[a-z-]*","status":"FAIL"'; then
  fail "a healthy store fell through into a failure record: $DOCTOR_OUT"
fi
grep_in "$DOCTOR_OUT" -Eq '"request_id":"doctor-[^"]+","latency_ms":[0-9]+,"component":"shim-store","rpc":"store\.v1\.ShimStore/GetLiveWork","healthy":true' ||
  fail "healthy probe metadata lost its request id, latency, or rpc: $DOCTOR_OUT"
OUT_BOUNDED="$DOCTOR_OUT"

# The store saw both procedures, each carrying the doctor's correlation header
# and the empty-message request body.
grep -q '^/store\.v1\.ShimStore/GetLiveWork	doctor-' "$STORE_CALLS" ||
  fail "the store never received a correlated GetLiveWork request: $(cat "$STORE_CALLS")"
grep -q '^/store\.v1\.ShimStore/GetSidecarCursors	doctor-' "$STORE_CALLS" ||
  fail "the store never received a correlated GetSidecarCursors request: $(cat "$STORE_CALLS")"
grep -q '^/store\.v1\.ShimStore/GetLiveWork	doctor-[^	]*	{"session":{"value":"agent-shim-doctor-probe"}}$' "$STORE_CALLS" ||
  fail "the GetLiveWork probe was not scoped to the doctor's sentinel session: $(cat "$STORE_CALLS")"
grep -q '^/store\.v1\.ShimStore/GetSidecarCursors	doctor-[^	]*	{}$' "$STORE_CALLS" ||
  fail "the GetSidecarCursors probe did not send the empty request message: $(cat "$STORE_CALLS")"

# ---- failure arm: served, but the store refuses the read ---------------

start_fake_store failure
run_doctor --json
stop_fake_store
assert_valid_json "$DOCTOR_OUT"
assert_probe_status FAIL
assert_probe_class failure_arm
grep_in "$DOCTOR_OUT" -q 'database is locked' ||
  fail "the failure arm's detail was not retained: $DOCTOR_OUT"

# ---- non-200: the request never reached a handler ----------------------

start_fake_store non200
run_doctor --json
stop_fake_store
assert_valid_json "$DOCTOR_OUT"
assert_probe_status FAIL
assert_probe_class http_status
grep_in "$DOCTOR_OUT" -q '"http_status":503' ||
  fail "the non-200 status was not reported: $DOCTOR_OUT"

# ---- malformed body: HTTP 200 that is not a Connect response -----------

start_fake_store malformed
run_doctor --json
stop_fake_store
assert_valid_json "$DOCTOR_OUT"
assert_probe_status FAIL
assert_probe_class malformed_response

# ---- refused connection: the socket exists, nothing accepts ------------

make_refusing_socket
run_doctor --json
rm -f "$STORE_SOCK"
assert_valid_json "$DOCTOR_OUT"
assert_probe_status FAIL
assert_probe_class connection_refused
grep_in "$DOCTOR_OUT" -q '"check":"store-socket-present","status":"PASS"' ||
  fail "the refusing-socket fixture should still satisfy the presence check: $DOCTOR_OUT"

# ---- missing socket: nothing to probe at all ---------------------------

rm -f "$STORE_SOCK"
run_doctor --json
assert_valid_json "$DOCTOR_OUT"
assert_probe_status FAIL
assert_probe_class missing_socket
grep_in "$DOCTOR_OUT" -q '"check":"store-socket-present","status":"FAIL"' ||
  fail "a missing socket must also fail the presence check: $DOCTOR_OUT"

# ---- timeout: the store accepts but never answers in time --------------

start_fake_store slow
run_doctor --json
stop_fake_store
assert_valid_json "$DOCTOR_OUT"
assert_probe_status FAIL
assert_probe_class timeout

# ---- text rendering keeps the class, the hint and the correlation ------

start_fake_store failure
set +e
OUT_TEXT="$(DOCTOR_SQLITE_CALLS="$CALLS" \
  AGENT_REPL_STATE_ROOT="$STATE" \
  AGENT_REPL_DOCTOR_STORE_PROBE_TIMEOUT="$PROBE_TIMEOUT" \
  AGENT_REPL_DOCTOR_INTEGRITY_AUTO_MAX_BYTES=1024 \
  PATH="$DOCTOR_PATH" \
  "$DOCTOR" 2>/dev/null)"
set -e
stop_fake_store
grep_in "$OUT_TEXT" -q 'answered the failure arm' ||
  fail "text output did not report the failure arm: $OUT_TEXT"
grep_in "$OUT_TEXT" -q 'detail=database is locked' ||
  fail "text output did not retain the store's detail: $OUT_TEXT"
grep_in "$OUT_TEXT" -q 'hint: the store is serving but REFUSED this read' ||
  fail "text output did not render the failure-arm hint: $OUT_TEXT"

# ---- bounded integrity policy (unchanged behavior) ---------------------

grep_in "$OUT_BOUNDED" -q '"check":"store-db-openable","status":"PASS"' ||
  fail "missing openable PASS: $OUT_BOUNDED"
grep_in "$OUT_BOUNDED" -q '"check":"store-db-integrity","status":"SKIP"' ||
  fail "missing oversized integrity SKIP: $OUT_BOUNDED"
if grep -q 'integrity_check' "$CALLS"; then
  fail "the bounded run executed the deep integrity scan"
fi

start_fake_store healthy
run_doctor --json --deep-integrity
stop_fake_store
grep_in "$DOCTOR_OUT" -q '"check":"store-db-integrity","status":"PASS"' ||
  fail "deep integrity did not pass: $DOCTOR_OUT"
grep -q 'integrity_check' "$CALLS" ||
  fail "--deep-integrity did not execute PRAGMA integrity_check"

# ---- a loaded service's long `launchctl print` is read whole -------------
# The pid line comes first and 200KB follow it, so a reader that stopped at the
# pid left the writer SIGPIPEd and the doctor exiting 141.

PRINT="$TMP/launchctl-print"
{
  printf 'gui/501/com.agentrepl.shim-store = {\n\tpid = 4242\n'
  for _ in $(seq 1 4000); do printf '\tfiller = 0123456789012345678901234567890123456789\n'; done
  printf '}\n'
} >"$PRINT"
start_fake_store healthy
export DOCTOR_LAUNCHCTL_PRINT="$PRINT"
run_doctor --json
unset DOCTOR_LAUNCHCTL_PRINT
stop_fake_store
grep_in "$DOCTOR_OUT" -q '"check":"launchd-shim-store","status":"PASS","detail":"com.agentrepl.shim-store loaded and running (pid 4242)"' ||
  fail "a loaded service's long launchctl print was not read to its pid: $DOCTOR_OUT"

printf 'PASS: doctor probes the store Connect endpoints and bounds integrity scans\n'
