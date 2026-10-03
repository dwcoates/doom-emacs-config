#!/usr/bin/env bash

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

# test-bounce-agent-repl-forcefully.sh -- hermetic tests of
# bounce-agent-repl-forcefully.sh.
#
# NOTHING LIVE IS EVER TOUCHED. Each case copies the script into its own
# temporary checkout, so every process it matches is matched by a path under
# that checkout or under the case's own XDG_CACHE_HOME; the "backends" are
# shell stand-ins started here; a `launchctl` first on PATH only records that
# it was called (the bounce must never call it); the build is a stand-in
# executable.

set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT="$HERE/bounce-agent-repl-forcefully.sh"
FAILURES=0
STARTED=()
WORLDS=()

cleanup() {
    local pid w
    for pid in "${STARTED[@]:-}"; do
        [ -n "$pid" ] && kill -KILL "$pid" 2>/dev/null
    done
    for w in "${WORLDS[@]:-}"; do
        [ -n "$w" ] && rm -rf "$w"
    done
    return 0
}
trap cleanup EXIT

fail() { echo "FAIL: $*"; FAILURES=$((FAILURES + 1)); }
pass() { echo "ok:   $*"; }

# world -- a temporary checkout, cache home and state root, left in W.
W=""
world() {
    local w
    w="$(mktemp -d /tmp/bounce-test.XXXXXX)"
    WORLDS+=("$w")
    mkdir -p "$w/checkout/scripts" "$w/checkout/daemon/bin" "$w/checkout/agent-shim/claude/shim/dist" \
        "$w/cache/agent-repl/bin" "$w/state/sock" "$w/pathbin"
    cp "$SCRIPT" "$w/checkout/scripts/"
    # THE SERVICES ARE THE NEXT DAEMON'S TO RESTART, so any launchctl call at
    # all is a failure: this one only records that it happened.
    printf '#!/bin/sh\necho "$*" >>"%s/launchctl-calls"\n' "$w" >"$w/pathbin/launchctl"
    chmod +x "$w/pathbin/launchctl"
    printf '#!/bin/sh\nexit 0\n' >"$w/builder"
    chmod +x "$w/builder"
    W="$w"
}

# backend PATH TRAPS [ARG...] -- start a stand-in process whose command line
# names PATH (and ARGs), its pid left in PID; TRAPS "obeys" exits on SIGTERM,
# "ignores" outlives it. Its output is closed, so the substitution that reads its pid
# never waits on it.
#
# RUN AS `PATH call ...` it is the daemon binary's `call` verb instead: the
# call is recorded in $AGENT_REPL_TEST_WORLD/daemon-calls and answered by
# $AGENT_REPL_TEST_WORLD/on-call when that exists (a stand-in for the
# daemon's ordered stand-down), and refused with no daemon serving otherwise.
PID=""
backend() {
    local path="$1" mode="$2" trap_line call_line
    shift 2
    mkdir -p "$(dirname "$path")"
    if [ "$mode" = ignores ]; then
        trap_line='trap "" TERM'
    else
        trap_line='trap "exit 0" TERM'
    fi
    call_line='if [ "$1" = call ]; then echo "$*" >>"$AGENT_REPL_TEST_WORLD/daemon-calls"; [ -x "$AGENT_REPL_TEST_WORLD/on-call" ] && exec "$AGENT_REPL_TEST_WORLD/on-call"; echo "no daemon is serving" >&2; exit 1; fi'
    printf '#!/bin/sh\n%s\n%s\nwhile :; do sleep 1; done\n' "$call_line" "$trap_line" >"$path"
    chmod +x "$path"
    # AN ORPHAN, so a stopped stand-in is reaped at once rather than lingering
    # as this shell's zombie, which `kill -0` would still call alive.
    PID="$(bash -c '"$0" "$@" </dev/null >/dev/null 2>&1 & echo $!' "$path" "$@")"
    STARTED+=("$PID")
}

run() { # WORLD -- run the copied script against the world, output to WORLD/out
    local w="$1"
    AGENT_REPL_TEST_WORLD="$w" AGENT_REPL_STATE_DIR="$w/state" PATH="$w/pathbin:$PATH" \
        XDG_CACHE_HOME="$w/cache" AGENT_REPL_BOUNCE_BUILDER="${BUILDER:-$w/builder}" \
        AGENT_REPL_BOUNCE_GRACE=2 \
        bash "$w/checkout/scripts/bounce-agent-repl-forcefully.sh" >"$w/out" 2>&1
}

gone() { ! kill -0 "$1" 2>/dev/null; }

# ---- a whole bounce --------------------------------------------------------

world; w="$W"
backend "$w/checkout/daemon/bin/claude-repld" obeys; daemon="$PID"
backend "$w/checkout/agent-shim/claude/shim/dist/main.js" ignores; shim="$PID"
backend "$w/cache/agent-repl/bin/shim-lock" obeys; lock="$PID"
run "$w"; status=$?
[ "$status" -eq 0 ] && pass "a bounce exits 0" || fail "a bounce exited $status: $(cat "$w/out")"
gone "$daemon" && pass "the daemon is stopped by its SIGTERM" || fail "the daemon survived"
gone "$shim" && pass "a shim that ignores SIGTERM is killed after the grace" || fail "the shim survived"
gone "$lock" && pass "the shim locks are stopped" || fail "a shim lock survived"

# ---- the services are the next daemon's to restart, never the bounce's -------

world; w="$W"
backend "$w/checkout/daemon/bin/claude-repld" obeys; daemon="$PID"
backend "$w/cache/agent-repl/bin/shim-store" ignores; store="$PID"
backend "$w/cache/agent-repl/bin/shim-claude-sidecar" ignores; sidecar="$PID"
run "$w"; status=$?
[ "$status" -eq 0 ] && kill -0 "$store" 2>/dev/null && kill -0 "$sidecar" 2>/dev/null &&
    pass "the store and the sidecar keep running through a bounce" ||
    fail "a service was stopped (exit $status): $(cat "$w/out")"
[ ! -e "$w/launchctl-calls" ] && pass "a bounce never calls launchctl" ||
    fail "launchctl was called: $(cat "$w/launchctl-calls")"

# on_call WORLD BODY -- the daemon's answer to the stand-down: a script that
# runs BODY (a stand-in for the daemon standing its shims down and exiting)
# and accepts the call.
on_call() {
    printf '#!/bin/sh\n%s\nexit 0\n' "$2" >"$1/on-call"
    chmod +x "$1/on-call"
}

# line_of WORLD PATTERN -- the first output line matching PATTERN, or empty.
line_of() { grep -nE -- "$2" "$1/out" | head -1 | cut -d: -f1; }

# ---- the daemon stands itself and its shims down first ----------------------

world; w="$W"
backend "$w/checkout/daemon/bin/claude-repld" ignores; daemon="$PID"
backend "$w/checkout/agent-shim/claude/shim/dist/main.js" ignores; shim="$PID"
backend "$w/cache/agent-repl/bin/shim-lock" ignores; lock="$PID"
on_call "$w" "kill -KILL $shim $lock; kill -KILL $daemon"
run "$w"; status=$?
[ "$status" -eq 0 ] && pass "an ordered stand-down bounces" || fail "an ordered stand-down exited $status: $(cat "$w/out")"
grep -qF "call -state-dir $w/state UpdateShutdownSchedule {\"now\":{\"reason\":{\"operator\":" "$w/daemon-calls" 2>/dev/null &&
    pass "the daemon is asked to stand down now" || fail "the daemon was not asked: $(cat "$w/daemon-calls" 2>/dev/null)"
gone "$daemon" && gone "$shim" && gone "$lock" &&
    pass "the daemon's own stand-down takes its shims with it" || fail "a backend survived the ordered stand-down"
grep -qE "(daemon|shims|shim locks): asking pid.* to shut down" "$w/out" &&
    fail "a backend the daemon stood down was signalled too: $(cat "$w/out")" ||
    pass "nothing the daemon stood down is signalled"

world; w="$W"
backend "$w/checkout/daemon/bin/claude-repld" obeys; daemon="$PID"
backend "$w/checkout/agent-shim/claude/shim/dist/main.js" ignores; shim="$PID"
on_call "$w" "kill -KILL $shim; kill -KILL $daemon"
run "$w"
daemon_done="$(line_of "$w" "^\[bounce\] daemon: stopped")"
shims_done="$(line_of "$w" "^\[bounce\] shims: ")"
[ -n "$daemon_done" ] && [ -n "$shims_done" ] && [ "$daemon_done" -lt "$shims_done" ] &&
    pass "stragglers are looked for only after the daemon has gone" ||
    fail "the stops ran out of order:
$(cat "$w/out")"

# ---- the fresh runtime Emacs starts meanwhile is left alone ----------------

world; w="$W"
backend "$w/checkout/daemon/bin/claude-repld" ignores; daemon="$PID"
backend "$w/checkout/agent-shim/claude/shim/dist/main.js" ignores; shim="$PID"
# The stand-down's answer also starts what Emacs's relaunched daemon would:
# a fresh daemon and a fresh shim on the same paths.
on_call "$w" "kill -KILL $shim $daemon
bash -c '\"\$0\" </dev/null >/dev/null 2>&1 & echo \$! >\"\$1\"' $w/checkout/daemon/bin/claude-repld $w/fresh-daemon
bash -c '\"\$0\" </dev/null >/dev/null 2>&1 & echo \$! >\"\$1\"' $w/checkout/agent-shim/claude/shim/dist/main.js $w/fresh-shim"
run "$w"; status=$?
fresh_daemon="$(cat "$w/fresh-daemon" 2>/dev/null)"; STARTED+=("$fresh_daemon")
fresh_shim="$(cat "$w/fresh-shim" 2>/dev/null)"; STARTED+=("$fresh_shim")
[ "$status" -eq 0 ] && [ -n "$fresh_daemon" ] && kill -0 "$fresh_daemon" 2>/dev/null &&
    pass "a daemon started after the stand-down keeps running" || fail "the fresh daemon was stopped (exit $status): $(cat "$w/out")"
[ -n "$fresh_shim" ] && kill -0 "$fresh_shim" 2>/dev/null &&
    pass "a shim started after the stand-down keeps running" || fail "the fresh shim was stopped: $(cat "$w/out")"

# ---- a stand-down the daemon refuses falls back to its signal --------------

world; w="$W"
backend "$w/checkout/daemon/bin/claude-repld" obeys; daemon="$PID"
run "$w"; status=$?
[ "$status" -eq 0 ] && gone "$daemon" &&
    pass "a daemon that refuses the stand-down is stopped by its SIGTERM" || fail "a refusing daemon survived (exit $status): $(cat "$w/out")"
grep -q "the stand-down was not accepted" "$w/out" &&
    pass "the refused stand-down is recorded" || fail "the refused stand-down left no record: $(cat "$w/out")"

# ---- a stand-down accepted and never carried out is enforced ---------------

world; w="$W"
backend "$w/checkout/daemon/bin/claude-repld" ignores; daemon="$PID"
on_call "$w" ":"
run "$w"; status=$?
[ "$status" -eq 0 ] && gone "$daemon" &&
    pass "a daemon that outlives its accepted stand-down is killed after the grace" ||
    fail "a daemon that never left survived (exit $status): $(cat "$w/out")"

# ---- another checkout's processes are left alone ---------------------------

world; w="$W"
backend "$w/elsewhere/daemon/bin/claude-repld" obeys; other="$PID"
run "$w"
kill -0 "$other" 2>/dev/null && pass "a daemon from another checkout keeps running" || fail "another checkout's daemon was stopped"

# ---- every shim of the state root goes, whichever build it runs ------------

world; w="$W"
# A shim a deploy started from ANOTHER checkout, serving this state root.
backend "$w/deployed/agent-shim/claude/shim/dist/main.js" ignores --listen "$w/state/sock/ws1.sock"; deployed_shim="$PID"
run "$w"; status=$?
[ "$status" -eq 0 ] && gone "$deployed_shim" &&
    pass "a shim serving the state root from another build path is stopped" ||
    fail "a shim from the installed path survived (exit $status): $(cat "$w/out")"

world; w="$W"
backend "$w/elsewhere/agent-shim/claude/shim/dist/main.js" obeys --listen "$w/other-state/sock/ws1.sock"; other_shim="$PID"
run "$w"
kill -0 "$other_shim" 2>/dev/null && pass "a shim of another checkout serving another state root keeps running" ||
    fail "another state root's shim was stopped"

# ---- the daemon the state root advertises goes, whichever build it runs ----

world; w="$W"
backend "$w/deployed/daemon/bin/claude-repld" obeys; advertised="$PID"
printf '127.0.0.1:9\npid=%s\n' "$advertised" >"$w/state/daemon.addr"
run "$w"; status=$?
[ "$status" -eq 0 ] && gone "$advertised" &&
    pass "the daemon daemon.addr names is stopped from any build path" ||
    fail "the advertised daemon survived (exit $status): $(cat "$w/out")"

# ---- a failed build stops nothing ------------------------------------------

world; w="$W"
backend "$w/checkout/daemon/bin/claude-repld" obeys; daemon="$PID"
printf '#!/bin/sh\nexit 3\n' >"$w/failing-builder"; chmod +x "$w/failing-builder"
BUILDER="$w/failing-builder" run "$w"; status=$?
[ "$status" -eq 1 ] && pass "a failed build exits 1" || fail "a failed build exited $status"
kill -0 "$daemon" 2>/dev/null && pass "a failed build leaves the daemon running" || fail "a failed build stopped the daemon"
grep -q "NOTHING WAS STOPPED" "$w/out" && pass "a failed build says nothing was stopped" || fail "output: $(cat "$w/out")"

if [ "$FAILURES" -ne 0 ]; then
    echo "$FAILURES failure(s)"
    exit 1
fi
echo "all passed"
