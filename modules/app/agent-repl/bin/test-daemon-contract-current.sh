#!/usr/bin/env bash
# Hermetic tests for bin/daemon-contract-current.sh: every path decided from the
# readiness report alone. The report is a stub; no daemon, no git is reached
# (each case here answers before the script would ask git anything).
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"
set -euo pipefail
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
SCRIPT="$THIS_DIR/daemon-contract-current.sh"
TMP="$(cd "$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")" && pwd -P)"
trap 'rm -rf "$TMP"' EXIT
PASS=0
FAIL=0
pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; FAIL=$((FAIL + 1)); }

# report BODY [EXIT]: a readiness stub that prints BODY and exits EXIT.
report() {
    printf '#!/usr/bin/env bash\ncat <<'"'"'J'"'"'\n%s\nJ\nexit %s\n' "$1" "${2:-0}" >"$TMP/readiness"
    chmod +x "$TMP/readiness"
}
expect_exit() {
    local name="$1" want="$2" got=0
    AGENT_REPL_RUNTIME_READINESS="$TMP/readiness" "$SCRIPT" >/dev/null 2>&1 || got=$?
    if [ "$got" = "$want" ]; then pass "$name"; else fail "$name (exit $got, want $want)"; fi
}

echo "daemon-contract-current"
report '{}' 1
expect_exit "a failed readiness report is unknown" 2
report 'not json'
expect_exit "an unreadable readiness report is unknown" 2
report '{"systems": [{"name": "daemon", "deployed_sha": "0123456789abcdef0123456789abcdef01234567", "running": null}]}'
expect_exit "no running daemon is unknown" 2
report '{"systems": [{"name": "daemon", "deployed_sha": "0123456789abcdef0123456789abcdef01234567", "running": {"pid": 1, "stale_binary": true}}]}'
expect_exit "a running daemon on a stale binary is unknown" 2
report '{"systems": [{"name": "daemon", "deployed_sha": "unknown", "running": {"pid": 1, "stale_binary": false}}]}'
expect_exit "an unreadable build stamp is unknown" 2
report '{"systems": [{"name": "shim", "deployed_sha": "0123456789abcdef0123456789abcdef01234567"}]}'
expect_exit "a report naming no daemon is unknown" 2

echo "daemon-contract-current: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
