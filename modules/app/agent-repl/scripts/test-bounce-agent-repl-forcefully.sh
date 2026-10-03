#!/usr/bin/env bash

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

# test-bounce-agent-repl-forcefully.sh -- hermetic tests of
# bounce-agent-repl-forcefully.sh.
#
# NOTHING LIVE IS EVER TOUCHED. Each case copies the script into its own
# temporary checkout, so every process it matches is matched by a path under
# that checkout or under the case's own XDG_CACHE_HOME; the "backends" are
# shell stand-ins started here; launchctl is a stub keeping its state in
# files; the build is a stand-in executable.

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

# world -- a temporary checkout, cache home and launchd, left in W.
W=""
world() {
    local w
    w="$(mktemp -d /tmp/bounce-test.XXXXXX)"
    WORLDS+=("$w")
    mkdir -p "$w/checkout/scripts" "$w/checkout/daemon/bin" "$w/checkout/agent-shim/claude/shim/dist" \
        "$w/cache/agent-repl/bin" "$w/cache/agent-repl/sock" "$w/agents" "$w/launchd" "$w/state/sock"
    cp "$SCRIPT" "$w/checkout/scripts/"
    touch "$w/agents/com.agentrepl.shim-store.plist" "$w/agents/com.agentrepl.shim-claude-sidecar.plist"
    # A launchctl stub: a loaded label is a file holding its pid; every call is
    # recorded. `bootstrap` of the store binds its socket unless told not to.
    cat >"$w/launchctl" <<EOF
#!/usr/bin/env bash
echo "\$*" >>"$w/launchd/calls"
label="\${2##*/}"
case "\$1" in
  print) [ -f "$w/launchd/\$label" ] || exit 113; echo "	pid = \$(cat "$w/launchd/\$label")" ;;
  bootout) [ -f "$w/launchd/stuck-\$label" ] && [ ! -f "$w/launchd/killed-\$label" ] && { touch "$w/launchd/killed-\$label"; exit 0; }; rm -f "$w/launchd/\$label" ;;
  bootstrap)
    label="\$(basename "\$3" .plist)"
    # A launchd that refuses outright, with nothing loaded.
    [ -f "$w/launchd/refuse-\$label" ] && exit 5
    # Another client (the daemon Emacs relaunched) bootstraps it first: launchd
    # then refuses this one with error 5, as the real one does.
    if [ -f "$w/launchd/raced-\$label" ]; then
      echo 999997 >"$w/launchd/\$label"
      [ "\$label" = com.agentrepl.shim-store ] && python3 -c 'import socket,sys; s=socket.socket(socket.AF_UNIX); s.bind(sys.argv[1])' "$w/cache/agent-repl/sock/store.sock" </dev/null >/dev/null 2>&1
      exit 5
    fi
    echo 999999 >"$w/launchd/\$label"
    if [ "\$label" = com.agentrepl.shim-store ] && [ ! -f "$w/launchd/no-socket" ]; then
      python3 -c 'import socket,sys; s=socket.socket(socket.AF_UNIX); s.bind(sys.argv[1])' "$w/cache/agent-repl/sock/store.sock" </dev/null >/dev/null 2>&1
    fi ;;
esac
EOF
    chmod +x "$w/launchctl"
    echo 999998 >"$w/launchd/com.agentrepl.shim-store"
    echo 999998 >"$w/launchd/com.agentrepl.shim-claude-sidecar"
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
    AGENT_REPL_TEST_WORLD="$w" AGENT_REPL_STATE_DIR="$w/state" \
    AGENT_REPL_LAUNCHCTL="$w/launchctl" AGENT_REPL_LAUNCH_AGENTS_DIR="$w/agents" \
        XDG_CACHE_HOME="$w/cache" AGENT_REPL_BOUNCE_BUILDER="${BUILDER:-$w/builder}" \
        AGENT_REPL_BOUNCE_GRACE=2 AGENT_REPL_BOUNCE_SOCK_MAX=2 \
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
calls="$(cat "$w/launchd/calls")"
last_bootout="$(grep -n bootout <<<"$calls" | tail -1 | cut -d: -f1)"
store_up="$(grep -n "bootstrap.*shim-store" <<<"$calls" | cut -d: -f1)"
sidecar_up="$(grep -n "bootstrap.*shim-claude-sidecar" <<<"$calls" | cut -d: -f1)"
[ "$(grep -c bootout <<<"$calls")" -eq 2 ] && [ -n "$store_up" ] && [ -n "$sidecar_up" ] &&
    [ "$last_bootout" -lt "$store_up" ] && [ "$store_up" -lt "$sidecar_up" ] &&
    pass "both services go down before the store, then the sidecar, come up" ||
    fail "launchctl calls were:
$calls"

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
first_bootout="$(line_of "$w" "booting out")"
[ -n "$daemon_done" ] && [ -n "$shims_done" ] && [ -n "$first_bootout" ] &&
    [ "$daemon_done" -lt "$shims_done" ] && [ "$shims_done" -lt "$first_bootout" ] &&
    pass "the services stop only after the daemon and its shims are gone" ||
    fail "the stops ran out of order:
$(cat "$w/out")"
calls="$(cat "$w/launchd/calls")"
sidecar_out="$(grep -n "bootout.*shim-claude-sidecar" <<<"$calls" | head -1 | cut -d: -f1)"
store_out="$(grep -n "bootout.*shim-store" <<<"$calls" | head -1 | cut -d: -f1)"
[ -n "$sidecar_out" ] && [ -n "$store_out" ] && [ "$sidecar_out" -lt "$store_out" ] &&
    pass "the sidecar is booted out before the store" || fail "launchctl calls were:
$calls"

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

# ---- a service that will not leave is killed --------------------------------

world; w="$W"
backend "$w/cache/agent-repl/bin/shim-claude-sidecar" ignores; sidecar="$PID"
echo "$sidecar" >"$w/launchd/com.agentrepl.shim-claude-sidecar"
touch "$w/launchd/stuck-com.agentrepl.shim-claude-sidecar"
run "$w"; status=$?
[ "$status" -eq 0 ] && pass "a stuck service still bounces" || fail "a stuck service exited $status: $(cat "$w/out")"
gone "$sidecar" && pass "a service that outlives its bootout is killed" || fail "the stuck sidecar survived"

# ---- a failed build stops nothing ------------------------------------------

world; w="$W"
backend "$w/checkout/daemon/bin/claude-repld" obeys; daemon="$PID"
printf '#!/bin/sh\nexit 3\n' >"$w/failing-builder"; chmod +x "$w/failing-builder"
BUILDER="$w/failing-builder" run "$w"; status=$?
[ "$status" -eq 1 ] && pass "a failed build exits 1" || fail "a failed build exited $status"
kill -0 "$daemon" 2>/dev/null && pass "a failed build leaves the daemon running" || fail "a failed build stopped the daemon"
grep -q bootout "$w/launchd/calls" 2>/dev/null && fail "a failed build booted a service out" || pass "a failed build boots no service out"
grep -q "NOTHING WAS STOPPED" "$w/out" && pass "a failed build says nothing was stopped" || fail "output: $(cat "$w/out")"

# ---- a missing plist refuses before anything ---------------------------------

world; w="$W"
rm "$w/agents/com.agentrepl.shim-claude-sidecar.plist"
printf '#!/bin/sh\ntouch "%s/built"\n' "$w" >"$w/builder"
run "$w"; status=$?
[ "$status" -eq 1 ] && pass "a missing plist exits 1" || fail "a missing plist exited $status"
[ ! -e "$w/built" ] && pass "a missing plist builds nothing" || fail "a missing plist still built"

# ---- a store whose socket never appears leaves the sidecar down -------------

world; w="$W"
touch "$w/launchd/no-socket"
run "$w"; status=$?
[ "$status" -eq 1 ] && pass "a store with no socket exits 1" || fail "a store with no socket exited $status"
grep -q "bootstrap.*shim-claude-sidecar" "$w/launchd/calls" && fail "the sidecar was started without the store's socket" || pass "the sidecar is not started without the store's socket"

# ---- a service another client already brought back -------------------------

world; w="$W"
touch "$w/launchd/raced-com.agentrepl.shim-store"
run "$w"; status=$?
[ "$status" -eq 0 ] && pass "a store another client already bootstrapped is taken as up" || fail "a raced store bootstrap failed the bounce ($status): $(cat "$w/out")"
grep -q "already loaded" "$w/out" && pass "the raced bootstrap is recorded" || fail "the raced bootstrap left no record: $(cat "$w/out")"

world; w="$W"
touch "$w/launchd/refuse-com.agentrepl.shim-store"
run "$w"; status=$?
[ "$status" -ne 0 ] && pass "a bootstrap that fails with nothing loaded fails the bounce" || fail "a failed bootstrap with nothing loaded passed"
grep -q "could not be bootstrapped" "$w/out" && pass "the refused bootstrap names the service" || fail "the refused bootstrap said: $(cat "$w/out")"

if [ "$FAILURES" -ne 0 ]; then
    echo "$FAILURES failure(s)"
    exit 1
fi
echo "all passed"
