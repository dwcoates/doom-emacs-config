#!/usr/bin/env bash
# test-wifi-helper.sh: the Wi-Fi helper's decisions (wifi-helper/standing.swift)
# and its Location Services seats (wifi-helper/seats.swift), each compiled with
# its tests (wifi-helper/test-<source>.swift) and run here, each of their cases
# reported through lib-harness-report.sh.
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

# run_tests SOURCE FRAMEWORK: SOURCE.swift compiled with test-SOURCE.swift
# against FRAMEWORK, and its cases reported.
run_tests() {
  local source="$1" framework="$2" build_out out rc
  if ! build_out="$(swiftc -parse-as-library -swift-version 5 -module-cache-path "$TMP/cache" -o "$TMP/$source-tests" \
    "$HELPER_DIR/$source.swift" "$HELPER_DIR/test-$source.swift" -framework "$framework" 2>&1)"; then
    fail "the helper's $source compiles with its tests" "$build_out"
    return
  fi
  out="$("$TMP/$source-tests" 2>&1)" && rc=0 || rc=$?
  local failed=0
  while IFS= read -r line; do
    case "$line" in
      "ok - "*) pass "${line#ok - }" ;;
      "not ok - "*) fail "${line#not ok - }"; failed=$((failed + 1)) ;;
      *) fail "the $source tests printed an unknown line" "$line" ;;
    esac
  done <<<"$out"
  [ "$rc" -eq $((failed > 0)) ] || fail "the $source tests' exit matches their cases" "exit $rc with $failed failing"
}

# A BUILD OF UNCHANGED SOURCES REUSES THE INSTALLED APP (build.sh --reuse):
# signing uses the local identity's key, which can prompt for the keychain's
# password, so only a change of sources may sign. The stubs fail any compile
# or signing, so a case that reaches them fails.
reuse_case() {
  local hash="$1"
  rm -rf "$TMP/reuse" "$TMP/out.app" "$TMP/stubs"
  mkdir -p "$TMP/reuse/Contents/Resources" "$TMP/stubs"
  printf '%s\n' "$hash" >"$TMP/reuse/Contents/Resources/source-sha256"
  printf 'kept\n' >"$TMP/reuse/Contents/Resources/marker"
  for tool in swiftc codesign; do
    printf '#!/bin/sh\necho "%s ran" >&2\nexit 1\n' "$tool" >"$TMP/stubs/$tool"
    chmod +x "$TMP/stubs/$tool"
  done
  PATH="$TMP/stubs:$PATH" bash "$HELPER_DIR/build.sh" "$TMP/out.app" --reuse "$TMP/reuse" 2>&1
}

current="$(bash "$HELPER_DIR/build.sh" --source-hash)"
if out="$(reuse_case "$current")" && [ "$(cat "$TMP/out.app/Contents/Resources/marker" 2>/dev/null)" = kept ] \
  && [[ $out == *"nothing was signed"* ]] && [[ $out != *"ran"* ]]; then
  pass "a build of unchanged sources copies the installed app and signs nothing"
else
  fail "a build of unchanged sources copies the installed app and signs nothing" "$out"
fi
if out="$(reuse_case "stale")"; then
  fail "a build of changed sources compiles anew rather than reusing the installed app" "$out"
elif [[ $out == *"swiftc ran"* ]]; then
  pass "a build of changed sources compiles anew rather than reusing the installed app"
else
  fail "a build of changed sources compiles anew rather than reusing the installed app" "$out"
fi

run_tests standing CoreLocation
run_tests seats Foundation
harness_finish
