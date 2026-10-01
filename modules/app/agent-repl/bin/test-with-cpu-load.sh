#!/usr/bin/env bash
# test-with-cpu-load.sh -- tests for bin/with-cpu-load.sh, the one way CPU
# load is generated: its busy loops must never outlive it, however it ends.
#
# Every case runs ONE loop for well under a second, so the suite itself puts
# no lasting load on the host. Each run carries a unique token in its argv, so
# the loops (subshells that share the script's command line) are found by it.
#
# Run with:   bash bin/test-with-cpu-load.sh

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
HELPER="$THIS_DIR/with-cpu-load.sh"

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -n "${2:-}" ] && echo "       $2"; }

# gone TOKEN: wait, bounded at 5s, for every process whose argv carries TOKEN
# to be gone. A loop polls its owner once per pass, so it exits within one
# pass of the owner's death; the bound is far past that.
gone() {
  local token=$1
  for _ in $(seq 1 50); do
    pgrep -f -- "$token" >/dev/null || return 0
    perl -e 'select(undef, undef, undef, 0.1)'
  done
  return 1
}

# 1. The command's exit status is the script's.
token="wcl-status-$$"
set +e
"$HELPER" 1 bash -c 'exit 7' "$token"
status=$?
set -e
if [ "$status" = 7 ]; then pass "the command's exit status is passed through"; else fail "the command's exit status is passed through" "got $status"; fi
gone "$token" && pass "the loops end with an ordinary finish" || fail "the loops end with an ordinary finish" "still running: $(pgrep -fl -- "$token")"

# started TOKEN: wait, bounded at 5s, for the owner, its one loop and the
# command (`bash -c` keeps TOKEN as its $0; the trailing `:` keeps bash from
# exec-ing the sleep in its place) to be running.
started() {
  local token=$1
  for _ in $(seq 1 50); do
    [ "$(pgrep -f -- "$token" | wc -l | tr -d ' ')" -ge 3 ] && return 0
    perl -e 'select(undef, undef, undef, 0.1)'
  done
  return 1
}

# reap TOKEN: stop what a case left of its command -- the `bash -c` that
# carries TOKEN and the sleep under it -- so nothing outlives the suite.
reap() {
  local token=$1 command
  for command in $(pgrep -f -- "bash -c sleep 30; : $token"); do
    pkill -P "$command" 2>/dev/null || true
    kill "$command" 2>/dev/null || true
  done
}

# 2. A SIGKILL of the script runs no trap; the loops still stop on their own.
# The script is this suite's direct child (the suite already runs at
# background priority, so the helper does not re-exec), so $! is the owner.
token="wcl-kill9-$$"
"$HELPER" 1 bash -c 'sleep 30; :' "$token" &
helper=$!
started "$token" || fail "the SIGKILL case started" "running: $(pgrep -fl -- "$token")"
kill -KILL "$helper"
wait "$helper" 2>/dev/null || true
reap "$token"
gone "$token" && pass "the loops stop by themselves when the script is SIGKILLed" || fail "the loops stop by themselves when the script is SIGKILLed" "still running: $(pgrep -fl -- "$token")"

# 3. A TERM is handled at once, not after the foreground command ends.
token="wcl-term-$$"
"$HELPER" 1 bash -c 'sleep 30; :' "$token" &
helper=$!
started "$token" || fail "the TERM case started" "running: $(pgrep -fl -- "$token")"
kill -TERM "$helper"
set +e
wait "$helper"
status=$?
set -e
if [ "$status" = 143 ]; then pass "a TERM stops the run at once with 143"; else fail "a TERM stops the run at once with 143" "got $status"; fi
reap "$token"
gone "$token" && pass "a TERM ends the command and the loops" || fail "a TERM ends the command and the loops" "still running: $(pgrep -fl -- "$token")"

# 4. Bad usage is refused before any loop starts.
set +e
"$HELPER" 0 true >/dev/null 2>&1
zero=$?
"$HELPER" 1 >/dev/null 2>&1
nocmd=$?
set -e
if [ "$zero" = 2 ] && [ "$nocmd" = 2 ]; then pass "a zero loop count or a missing command is refused"; else fail "a zero loop count or a missing command is refused" "got $zero and $nocmd"; fi

echo "with-cpu-load: $PASS passed, $FAIL failed"
[ "$FAIL" = 0 ]
