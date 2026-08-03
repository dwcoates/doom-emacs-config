#!/usr/bin/env bash
# Hermetic tests for report-logging-density.sh.
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_SRC="$THIS_DIR/report-logging-density.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-logging-density-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

tree="$TMP/tree"
bin="$tree/modules/app/agent-repl/bin"
mkdir -p \
    "$bin" \
    "$tree/modules/app/agent-repl/daemon" \
    "$tree/modules/app/agent-repl/agent-shim/claude/shim-sidecar" \
    "$tree/modules/app/agent-repl/agent-shim/shim-store" \
    "$tree/modules/app/agent-repl/agent-shim/wire" \
    "$tree/modules/app/agent-repl/agent-shim/logging/go" \
    "$tree/modules/app/agent-repl/agent-shim/claude/shim/src" \
    "$tree/modules/app/agent-repl/webapp/src"
cp "$SCRIPT_SRC" "$bin/report-logging-density.sh"
chmod +x "$bin/report-logging-density.sh"

printf '%s\n' \
    'package daemon' \
    'func run() {' \
    '  logf("start")' \
    '}' >"$tree/modules/app/agent-repl/daemon/main.go"
printf '%s\n' \
    'package daemon' \
    'func TestIgnored() { logf("test") }' \
    >"$tree/modules/app/agent-repl/daemon/main_test.go"
printf '%s\n' \
    'package sidecar' \
    'func run() { logger.Log("start") }' \
    >"$tree/modules/app/agent-repl/agent-shim/claude/shim-sidecar/main.go"
printf '%s\n' \
    'package store' \
    'func run() { logger.LogVerbose(nil, "start") }' \
    >"$tree/modules/app/agent-repl/agent-shim/shim-store/main.go"
printf '%s\n' \
    'package wire' \
    'func Read() error { return nil }' \
    >"$tree/modules/app/agent-repl/agent-shim/wire/wire.go"
printf '%s\n' \
    'package logging' \
    'func Timestamp() string { return "" }' \
    >"$tree/modules/app/agent-repl/agent-shim/logging/go/timestamp.go"
printf '%s\n' \
    'LOGGER.log({}, "start");' \
    'LOGGER.logVerbose({}, "detail");' \
    >"$tree/modules/app/agent-repl/agent-shim/claude/shim/src/main.ts"
printf '%s\n' \
    'log("info", "start", options);' \
    'logVerbose("info", "detail", options);' \
    >"$tree/modules/app/agent-repl/webapp/src/main.ts"

out="$("$bin/report-logging-density.sh")"
[ "$(printf '%s\n' "$out" | wc -l | tr -d ' ')" -eq 8 ] ||
    fail "default report did not emit one header and seven components"
printf '%s\n' "$out" | grep -q '^daemon,go,1,4,1,250.00$' ||
    fail "daemon count included tests or missed canonical calls"
printf '%s\n' "$out" | grep -q '^wire,go,1,2,0,0.00$' ||
    fail "wire zero-call case was not reported"
printf '%s\n' "$out" | grep -q '^logging,go,1,2,0,0.00$' ||
    fail "shared logging zero-call case was not reported"
printf '%s\n' "$out" | grep -q '^shim,typescript,1,2,2,1000.00$' ||
    fail "shim canonical calls were not counted"
printf '%s\n' "$out" | grep -q '^webapp,typescript,1,2,2,1000.00$' ||
    fail "webapp canonical calls were not counted"

out="$("$bin/report-logging-density.sh" store)"
[ "$(printf '%s\n' "$out" | wc -l | tr -d ' ')" -eq 2 ] &&
    printf '%s\n' "$out" | grep -q '^store,go,1,2,1,500.00$' ||
    fail "component selection did not isolate store"

if "$bin/report-logging-density.sh" mystery >/dev/null 2>&1; then
    fail "unknown component succeeded"
fi

printf 'PASS: report-logging-density\n'
