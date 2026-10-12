#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# test-suite-slot.sh — hermetic tests for suite-slot.sh, the host concurrency
# gate.
#
# Every test points the script at a PRIVATE slot directory under a temp dir
# (AGENT_REPL_SUITE_SLOT_DIR), so the real host gate at /tmp is never claimed,
# reclaimed, or reaped by this harness — a test that took the live slot would
# stall whatever suite is actually running on the box.
#
# The subject is the gate's two halves, which pull against each other:
#   - it must SERIALIZE independent suites, and
#   - it must NOT serialize a suite against itself. A held slot belongs to a
#     process tree, so a wrapped command that wraps its own children used to
#     queue behind the holder that was waiting for them — `suite-slot.sh
#     e2e-repeat.sh` hung for 6000s that way.
#
# Run with:   bash bin/test-suite-slot.sh

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

# This harness models independent callers in private slot directories.  When
# the harness itself is correctly run through the host gate, its inherited
# marker describes that OUTER suite, not any subject invocation below.  Clear
# it once here; the nesting cases create and inherit their own marker through
# the script under test.
unset AGENT_REPL_SUITE_SLOT_HELD

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/suite-slot.sh"

# The merge queue's gate runs the whole suite under
# AGENT_REPL_SUITE_PRIORITY=merge, so every subject below would inherit the
# merge priority and stand ahead of the ordinary tickets the ordering rows
# stage. A subject's priority is fixture state: the rows that are about
# priority set it on their own command line, and every other row is an
# ordinary waiter.
unset AGENT_REPL_SUITE_PRIORITY

# The unified suite is itself expected to hold the host slot. Its exported
# re-entrancy marker is not fixture state: each case below constructs its own
# process tree and private slot directory, including the nested case that
# proves the marker is exported by the script under test.
unset AGENT_REPL_SUITE_SLOT_HELD

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -n "${2:-}" ] && echo "       $2"; }

TMP="$(mktemp -d "${TMPDIR:-/tmp}/ss.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# This harness always gives the subject a private slot directory. An outer
# host gate may legitimately wrap the whole verification sweep, but its
# process-tree marker does not describe the isolated subject processes below.
# Clear it once here; the nested-invocation rows establish their own marker by
# acquiring their private outer gate.
unset AGENT_REPL_SUITE_SLOT_HELD

# --- 1. the plain case: the command runs and its status is this script's ----
d="$TMP/t1"; mkdir -p "$d"
set +e
AGENT_REPL_SUITE_SLOT_DIR="$d/slots" bash "$SCRIPT_UNDER_TEST" \
    bash -c 'echo ran > "$0"' "$d/marker" 2>"$d/err"
RC=$?
set -e
if [ "$RC" -eq 0 ] && [ "$(cat "$d/marker" 2>/dev/null)" = "ran" ]; then
    pass "a gated command runs and reports success"
else
    fail "a gated command runs and reports success" "rc=$RC err: $(cat "$d/err")"
fi

# --- 2. a failing command's status survives the gate ------------------------
# A gate must never turn a red suite green on its way past.
d="$TMP/t2"; mkdir -p "$d"
set +e
AGENT_REPL_SUITE_SLOT_DIR="$d/slots" bash "$SCRIPT_UNDER_TEST" \
    bash -c 'exit 7' 2>"$d/err"
RC=$?
set -e
if [ "$RC" -eq 7 ]; then
    pass "a failing command's exit status passes through the gate"
else
    fail "a failing command's exit status passes through the gate" "rc=$RC"
fi

# --- 3. the slot is released when the command finishes ----------------------
d="$TMP/t3"; mkdir -p "$d"
AGENT_REPL_SUITE_SLOT_DIR="$d/slots" bash "$SCRIPT_UNDER_TEST" true 2>"$d/err"
if [ -z "$(ls -A "$d/slots" 2>/dev/null)" ]; then
    pass "the slot is released when the gated command finishes"
else
    fail "the slot is released when the gated command finishes" "slots: $(ls -A "$d/slots")"
fi

# --- 4. a non-nested second suite still WAITS -------------------------------
# The gate's whole reason to exist. A live holder is staged by hand — a
# directory with the pid of a process that is genuinely alive — so nothing has
# to be raced into place.
d="$TMP/t4"; mkdir -p "$d/slots/slot-1"
sleep 60 &
HOLDER=$!
printf '%s\n' "$HOLDER" > "$d/slots/slot-1/pid"
printf '%s\n' "a suite that is already running" > "$d/slots/slot-1/cmd"
set +e
# THE WAITER RUNS AS ITS OWN PROCESS GROUP (job control on for this one
# launch), so ending it ends the gate's polling `sleep 2` with it. Killing the
# bash alone orphaned that sleep, which kept this harness's stderr file open
# on the run's RAM disk until the run's end and failed its release
# (2026-10-07, `held by: sleep[...]`).
set -m
AGENT_REPL_SUITE_SLOT_DIR="$d/slots" bash "$SCRIPT_UNDER_TEST" \
    bash -c 'echo ran > "$0"' "$d/marker" 2>"$d/err" &
WAITER=$!
set +m
# The waiter announces before its first sleep, so the announcement is the
# signal that it is gating rather than running. Poll for it under a bound
# rather than sleeping a guessed interval.
waited=0
while ! grep -q "WAITING" "$d/err" 2>/dev/null; do
    if [ "$waited" -ge 100 ]; then break; fi
    sleep 0.1
    waited=$((waited + 1))
done
GATED=0
if grep -q "WAITING" "$d/err" 2>/dev/null && [ ! -f "$d/marker" ]; then GATED=1; fi
kill -TERM -- "-$WAITER" 2>/dev/null
wait "$WAITER" 2>/dev/null
WAITER_LEFT=$(pgrep -g "$WAITER" 2>/dev/null | tr '\n' ' ')
kill "$HOLDER" 2>/dev/null
wait "$HOLDER" 2>/dev/null
set -e
if [ "$GATED" -eq 1 ]; then
    pass "a second suite waits while another holds the only slot"
else
    fail "a second suite waits while another holds the only slot" "err: $(cat "$d/err")"
fi
if [ -z "$WAITER_LEFT" ]; then
    pass "a stopped waiter leaves nothing of its group running"
else
    fail "a stopped waiter leaves nothing of its group running" "still running: $WAITER_LEFT"
fi

# --- 5. a NESTED invocation runs through instead of deadlocking -------------
# `suite-slot.sh <cmd>` where <cmd> itself calls suite-slot.sh: the inner one
# is inside the holder's process tree, so waiting would be waiting on itself.
# The bound is generous against a loaded box and still finite: a regression
# here does not fail slowly, it never returns at all.
d="$TMP/t5"; mkdir -p "$d"
set +e
AGENT_REPL_SUITE_SLOT_DIR="$d/slots" bash "$SCRIPT_UNDER_TEST" \
    bash "$SCRIPT_UNDER_TEST" bash -c 'echo nested > "$0"' "$d/marker" 2>"$d/err" &
NESTED=$!
waited=0
while kill -0 "$NESTED" 2>/dev/null; do
    if [ "$waited" -ge 300 ]; then break; fi
    sleep 0.1
    waited=$((waited + 1))
done
if kill -0 "$NESTED" 2>/dev/null; then
    kill "$NESTED" 2>/dev/null
    RC=124
else
    wait "$NESTED"
    RC=$?
fi
set -e
if [ "$RC" -eq 0 ] && [ "$(cat "$d/marker" 2>/dev/null)" = "nested" ]; then
    pass "a nested invocation completes instead of queueing behind its own holder"
else
    fail "a nested invocation completes instead of queueing behind its own holder" \
         "rc=$RC err: $(cat "$d/err")"
fi

# --- 6. the nested run says why it did not gate -----------------------------
# A run-through that is silent about itself reads as a gate that stopped
# working, which is how a real overload gets blamed on the wrong thing.
if grep -q "nested invocation" "$TMP/t5/err"; then
    pass "a nested invocation reports that it ran without acquiring a slot"
else
    fail "a nested invocation reports that it ran without acquiring a slot" \
         "err: $(cat "$TMP/t5/err")"
fi

# --- 7. the nested run takes NO second slot ---------------------------------
# The count is the gate. If nesting claimed a slot of its own, one suite would
# occupy two and the host would run more work than the operator asked for.
d="$TMP/t7"; mkdir -p "$d"
AGENT_REPL_SUITE_SLOT_DIR="$d/slots" bash "$SCRIPT_UNDER_TEST" \
    bash "$SCRIPT_UNDER_TEST" bash -c 'ls -A "$0" > "$1"' "$d/slots" "$d/held" 2>"$d/err"
if [ "$(grep -c . "$d/held" 2>/dev/null || echo 0)" -eq 1 ]; then
    pass "a nested invocation holds no slot of its own"
else
    fail "a nested invocation holds no slot of its own" "slots during the run: $(cat "$d/held")"
fi

# --- ordering: tickets in $SLOT_DIR.queue ------------------------------------
# A waiter's ticket is <priority>-<epoch seconds>-<pid>. These rows stage
# tickets by hand, each owned by a process that is genuinely alive (or
# genuinely dead), so no ordering has to be raced into place.

# stage_ticket DIR PRIORITY SECONDS PID -- a ticket as a waiter would write it.
stage_ticket() {
    mkdir -p "$1/slots.queue"
    : > "$(printf '%s/slots.queue/%d-%012d-%010d' "$1" "$2" "$3" "$4")"
}

# await_file FILE PATTERN -- poll (bounded, 10s) until FILE contains PATTERN.
await_file() {
    local waited=0
    while ! grep -q "$2" "$1" 2>/dev/null; do
        if [ "$waited" -ge 100 ]; then return 1; fi
        sleep 0.1
        waited=$((waited + 1))
    done
}

# --- 8. a free slot is NOT taken past an earlier live waiter ----------------
# The bug this ordering fixes: a freed slot went to whichever poller woke
# first, so a waiter could lose the race indefinitely.
d="$TMP/t8"; mkdir -p "$d"
sleep 60 &
EARLIER=$!
stage_ticket "$d" 1 1 "$EARLIER"
set +e
set -m
AGENT_REPL_SUITE_SLOT_DIR="$d/slots" bash "$SCRIPT_UNDER_TEST" \
    bash -c 'echo ran > "$0"' "$d/marker" 2>"$d/err" &
WAITER=$!
set +m
GATED=0
if await_file "$d/err" "WAITING" && [ ! -f "$d/marker" ]; then GATED=1; fi
kill -TERM -- "-$WAITER" 2>/dev/null
wait "$WAITER" 2>/dev/null
kill "$EARLIER" 2>/dev/null
wait "$EARLIER" 2>/dev/null
set -e
if [ "$GATED" -eq 1 ]; then
    pass "a waiter does not take a free slot ahead of an earlier live waiter"
else
    fail "a waiter does not take a free slot ahead of an earlier live waiter" "err: $(cat "$d/err")"
fi

# --- 9. a dead waiter's ticket does not block the line ----------------------
d="$TMP/t9"; mkdir -p "$d"
sleep 0 &
DEAD=$!
wait "$DEAD"
stage_ticket "$d" 1 1 "$DEAD"
set +e
# shellcheck disable=SC2016  # $0 is the inner bash's, expanded there on purpose
timeout 30 env AGENT_REPL_SUITE_SLOT_DIR="$d/slots" bash "$SCRIPT_UNDER_TEST" \
    bash -c 'echo ran > "$0"' "$d/marker" 2>"$d/err"
RC=$?
set -e
if [ "$RC" -eq 0 ] && [ "$(cat "$d/marker" 2>/dev/null)" = "ran" ]; then
    pass "a dead waiter's ticket is dropped instead of blocking the line"
else
    fail "a dead waiter's ticket is dropped instead of blocking the line" "rc=$RC err: $(cat "$d/err")"
fi

# --- 10. the merge queue's gate goes ahead of an earlier ordinary waiter ----
# Master does not move while the merge queue waits, so its gate is served
# before every ad-hoc run, however long that run has been waiting.
d="$TMP/t10"; mkdir -p "$d"
sleep 60 &
EARLIER=$!
stage_ticket "$d" 1 1 "$EARLIER"
set +e
# shellcheck disable=SC2016  # $0 is the inner bash's, expanded there on purpose
timeout 30 env AGENT_REPL_SUITE_PRIORITY=merge AGENT_REPL_SUITE_SLOT_DIR="$d/slots" \
    bash "$SCRIPT_UNDER_TEST" bash -c 'echo ran > "$0"' "$d/marker" 2>"$d/err"
RC=$?
kill "$EARLIER" 2>/dev/null
wait "$EARLIER" 2>/dev/null
set -e
if [ "$RC" -eq 0 ] && [ "$(cat "$d/marker" 2>/dev/null)" = "ran" ]; then
    pass "a merge-priority waiter runs ahead of an earlier ordinary waiter"
else
    fail "a merge-priority waiter runs ahead of an earlier ordinary waiter" "rc=$RC err: $(cat "$d/err")"
fi

# --- 11. an unknown priority is refused, not guessed at ---------------------
d="$TMP/t11"; mkdir -p "$d"
set +e
AGENT_REPL_SUITE_PRIORITY=urgent AGENT_REPL_SUITE_SLOT_DIR="$d/slots" \
    bash "$SCRIPT_UNDER_TEST" bash -c 'echo ran > "$0"' "$d/marker" 2>"$d/err"
RC=$?
set -e
if [ "$RC" -eq 2 ] && [ ! -f "$d/marker" ]; then
    pass "an unknown priority is refused without running the command"
else
    fail "an unknown priority is refused without running the command" "rc=$RC err: $(cat "$d/err")"
fi

# --- 12. a waiter that ran leaves no ticket behind --------------------------
d="$TMP/t12"; mkdir -p "$d"
AGENT_REPL_SUITE_SLOT_DIR="$d/slots" bash "$SCRIPT_UNDER_TEST" true 2>"$d/err"
if [ -z "$(ls -A "$d/slots.queue" 2>/dev/null)" ]; then
    pass "a finished run leaves no ticket in the queue"
else
    fail "a finished run leaves no ticket in the queue" "queue: $(ls -A "$d/slots.queue")"
fi

# --- 13/14. a signalled waiter stops: it never runs its command --------------
# A waiter told to stop must not go on to take the next slot. The holder is
# released only AFTER the waiter has been signalled and reaped, so a waiter
# that survived its signal would find the slot free and write the marker.
stop_waiter_case() {
    local sig=$1 want=$2 label=$3 d holder waiter rc
    d="$TMP/stop-$sig"; mkdir -p "$d/slots/slot-1"
    sleep 60 &
    holder=$!
    printf '%s\n' "$holder" > "$d/slots/slot-1/pid"
    printf '%s\n' "a suite that is already running" > "$d/slots/slot-1/cmd"
    set +e
    # Job control on for the launch, as in case 4: without it bash starts a
    # background job with INT ignored, and a signal ignored on entry cannot be
    # trapped -- unlike the terminal's Ctrl-C, which reaches a foreground job.
    set -m
    AGENT_REPL_SUITE_SLOT_DIR="$d/slots" bash "$SCRIPT_UNDER_TEST" \
        bash -c 'echo ran > "$0"' "$d/marker" 2>"$d/err" &
    waiter=$!
    set +m
    await_file "$d/err" "WAITING"
    kill "-$sig" "$waiter" 2>/dev/null
    wait "$waiter" 2>/dev/null
    rc=$?
    kill "$holder" 2>/dev/null
    wait "$holder" 2>/dev/null
    set -e
    if [ "$rc" -eq "$want" ] && [ ! -f "$d/marker" ] && [ -z "$(ls -A "$d/slots.queue" 2>/dev/null)" ]; then
        pass "$label"
    else
        fail "$label" "rc=$rc (want $want) marker=$([ -f "$d/marker" ] && echo present || echo absent) queue=$(ls -A "$d/slots.queue" 2>/dev/null) err: $(cat "$d/err")"
    fi
}
stop_waiter_case TERM 143 "a waiter stopped by TERM exits without running its command"
stop_waiter_case INT 130 "a waiter interrupted by INT exits without running its command"

echo
echo "passed $PASS, failed $FAIL"
[ "$FAIL" -eq 0 ]
