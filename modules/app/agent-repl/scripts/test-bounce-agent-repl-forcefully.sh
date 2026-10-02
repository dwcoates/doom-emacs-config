#!/usr/bin/env bash
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
        "$w/cache/agent-repl/bin" "$w/cache/agent-repl/sock" "$w/agents" "$w/launchd"
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

# backend PATH TRAPS -- start a stand-in process whose command line names
# PATH, its pid left in PID; TRAPS "obeys" exits on SIGTERM, "ignores"
# outlives it. Its output is closed, so the substitution that reads its pid
# never waits on it.
PID=""
backend() {
    local path="$1" mode="$2"
    mkdir -p "$(dirname "$path")"
    if [ "$mode" = ignores ]; then
        printf '#!/bin/sh\ntrap "" TERM\nwhile :; do sleep 1; done\n' >"$path"
    else
        printf '#!/bin/sh\ntrap "exit 0" TERM\nwhile :; do sleep 1; done\n' >"$path"
    fi
    chmod +x "$path"
    # AN ORPHAN, so a stopped stand-in is reaped at once rather than lingering
    # as this shell's zombie, which `kill -0` would still call alive.
    PID="$(bash -c '"$0" </dev/null >/dev/null 2>&1 & echo $!' "$path")"
    STARTED+=("$PID")
}

run() { # WORLD -- run the copied script against the world, output to WORLD/out
    local w="$1"
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

# ---- every stop is asked at once -------------------------------------------

world; w="$W"
backend "$w/checkout/daemon/bin/claude-repld" ignores; daemon="$PID"
backend "$w/checkout/agent-shim/claude/shim/dist/main.js" ignores; shim="$PID"
backend "$w/cache/agent-repl/bin/shim-claude-sidecar" ignores; sidecar="$PID"
echo "$sidecar" >"$w/launchd/com.agentrepl.shim-claude-sidecar"
touch "$w/launchd/stuck-com.agentrepl.shim-claude-sidecar"
run "$w"; status=$?
first_kill="$(grep -n "killing" "$w/out" | head -1 | cut -d: -f1)"
last_ask="$(grep -nE "asking pid|booting out" "$w/out" | tail -1 | cut -d: -f1)"
[ "$status" -eq 0 ] && [ -n "$first_kill" ] && [ -n "$last_ask" ] && [ "$last_ask" -lt "$first_kill" ] &&
    pass "every backend is asked to stop before any is killed" ||
    fail "the stops ran one after another (exit $status):
$(cat "$w/out")"
gone "$daemon" && gone "$shim" && gone "$sidecar" &&
    pass "every backend that ignores its graceful stop is killed" || fail "a backend survived"

# ---- another checkout's processes are left alone ---------------------------

world; w="$W"
backend "$w/elsewhere/daemon/bin/claude-repld" obeys; other="$PID"
run "$w"
kill -0 "$other" 2>/dev/null && pass "a daemon from another checkout keeps running" || fail "another checkout's daemon was stopped"

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

if [ "$FAILURES" -ne 0 ]; then
    echo "$FAILURES failure(s)"
    exit 1
fi
echo "all passed"
