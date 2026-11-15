#!/usr/bin/env bash
# test-wifi-helper.sh: the Wi-Fi helper's decisions (wifi-helper/standing.swift),
# compiled with their tests (wifi-helper/test-standing.swift) and run here,
# each of its cases reported through lib-harness-report.sh.
#
# Run with:   bash bin/test-wifi-helper.sh

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

HELPER_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../wifi-helper" && pwd)"

# shellcheck source=lib-harness-report.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)/lib-harness-report.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

if ! build_out="$(swiftc -parse-as-library -swift-version 5 -module-cache-path "$TMP/cache" -o "$TMP/standing-tests" \
  "$HELPER_DIR/standing.swift" "$HELPER_DIR/test-standing.swift" -framework CoreLocation 2>&1)"; then
  fail "the helper's standing compiles with its tests" "$build_out"
  harness_finish
fi

out="$("$TMP/standing-tests" 2>&1)" && rc=0 || rc=$?
while IFS= read -r line; do
  case "$line" in
    "ok - "*) pass "${line#ok - }" ;;
    "not ok - "*) fail "${line#not ok - }" ;;
    *) fail "the tests printed an unknown line" "$line" ;;
  esac
done <<<"$out"
[ "$rc" -eq $((FAIL > 0)) ] || fail "the tests' exit matches their cases" "exit $rc with $FAIL failing"
harness_finish
