#!/usr/bin/env bash
# bounce-agent-repl-forcefully.sh -- rebuild every agent-repl component, then
# stand the running daemon and its shims down so the next daemon runs the fresh
# build of everything.
#
# WHAT IT DOES, IN ORDER:
#
#   1. BUILDS everything in place in this checkout: the protobufs, then the
#      shim, webapp, daemon, store, sidecar and lock (`bin/build-frontend.sh
#      --force`). The store, sidecar and lock binaries are installed into
#      ~/.cache/agent-repl/bin, where launchd runs the services from. The
#      webapp is built and never "deployed": Emacs serves the checkout's dist,
#      so every webview opened from now on loads the new one. A FAILED BUILD
#      STOPS HERE, with nothing stopped.
#   2. STANDS THE DAEMON DOWN, and its shims with it:
#        a. the daemon is ASKED to stand down now (`claude-repld call
#           UpdateShutdownSchedule {now}`). That is its own ordered stand-down:
#           it announces its ending to every client, stands each of its shims
#           down itself (so it reads each exit as one it ordered, and each shim
#           concludes against a store that is still up), then exits;
#        b. whatever the daemon left -- a daemon that refused or never
#           answered the request, or one that outlived the grace, and any shim
#           or lock helper still running -- gets SIGTERM, then SIGKILL after
#           the grace. Only the processes running when the stand-down was
#           asked are ever signalled: the daemon Emacs relaunches meanwhile,
#           and the shims it starts, are the fresh build and are left alone.
#
# THE SERVICES ARE NOT TOUCHED HERE, AND THE DAEMON IS NOT STARTED HERE. Emacs
# owns starting the daemon: a running Emacs finds its link gone and starts the
# fresh daemon itself; a starting Emacs does the same at boot. That daemon's
# boot finds the store and the sidecar running an older build than the one
# just installed and restarts them, in the recorded safe order, BEFORE it lets
# any shim start (deploy.Restarter.EnsureCurrent and the spawn latch,
# daemon/AGENTS.md). Stopped from here instead, the services went down under
# whatever the freshly relaunched daemon had already started.
#
# Every process it signals is matched by THIS checkout's own paths, by the
# lock helper's cache-bin path, or by the state root it serves (the daemon its
# daemon.addr names, a shim listening under its sock/), so a daemon or shim of
# another checkout serving another state root is left alone.
#
# Usage:
#   scripts/bounce-agent-repl-forcefully.sh
#
# Honored environment (the test's isolation; defaults are the live host):
#   AGENT_REPL_BOUNCE_GRACE        seconds each graceful stop is given (default 20)
#   AGENT_REPL_STATE_DIR           the state root whose daemon is asked to stand
#                                  down (default: the daemon's own, ~/.claude-emacs)
#   AGENT_REPL_BOUNCE_BUILDER      one executable run in place of the build
#   XDG_CACHE_HOME                 locates ~/.cache/agent-repl (default ~/.cache)
#
# Exit status: 0 when the daemon and its shims are stood down onto the fresh
# build; 1 when the build failed (nothing was stopped).

set -euo pipefail

log() { echo "[bounce] $*"; }
die() { echo "[bounce] $*" >&2; exit 1; }

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$THIS_DIR/.." && pwd)"

GRACE="${AGENT_REPL_BOUNCE_GRACE:-20}"
CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
CACHE_BIN="$CACHE_HOME/agent-repl/bin"

DAEMON_BIN="$ROOT/daemon/bin/claude-repld"
SHIM_MAIN="$ROOT/agent-shim/claude/shim/dist/main.js"
# The state root whose daemon is stood down: the daemon's own default, which is
# the one Emacs runs it with.
STATE_ROOT="${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}"
STATE_ROOT="${STATE_ROOT%/}"

# ---- 1. build --------------------------------------------------------------

build() {
    if [ -n "${AGENT_REPL_BOUNCE_BUILDER:-}" ]; then
        "$AGENT_REPL_BOUNCE_BUILDER"
        return
    fi
    make -C "$ROOT/proto" all
    bash "$ROOT/bin/build-frontend.sh" --force shim webapp daemon store sidecar lock
}

log "building every component in $ROOT ..."
if ! build; then
    die "the build failed; NOTHING WAS STOPPED, and every backend keeps running its current build"
fi
log "built"

# ---- 2. stop ---------------------------------------------------------------

# pids_of TEXT -- the pids whose command line contains TEXT, matched as a
# fixed string (a path is not a pattern), never this script's own.
pids_of() {
    # THE TEXT RIDES THE ENVIRONMENT, never awk's own argv, or awk would find
    # it in its own command line and answer itself.
    ps -axo pid=,command= 2>/dev/null |
        PIDS_OF_TEXT="$1" awk -v self="$$" -v sub_="${BASHPID:-}" \
            'index($0, ENVIRON["PIDS_OF_TEXT"]) { pid = $1; if (pid != self && pid != sub_) print pid }'
}

# advertised_daemon -- the pid the state root's daemon.addr names, when that
# process is running.
advertised_daemon() {
    local pid
    pid="$(sed -n 's/^pid=\([0-9][0-9]*\)$/\1/p' "$STATE_ROOT/daemon.addr" 2>/dev/null | head -1)"
    [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null && echo "$pid"
    return 0
}

# alive PID... -- the given pids that are still running.
alive() {
    local pid
    for pid in "$@"; do
        kill -0 "$pid" 2>/dev/null && echo "$pid"
    done
    return 0
}

# await_exit NAME PID... -- wait up to the grace period for every given pid to
# exit, leaving the ones still running in OUTLIVED.
#
# IT LOOKS EVERY TENTH OF A SECOND, NOT EVERY SECOND. The daemon's stand-down
# exits in tens of milliseconds, and Emacs starts a fresh daemon about a second
# after the old one's link goes down; a straggler the old daemon could not
# stand down is signalled the moment the exit is seen, before that fresh
# daemon's boot could adopt it.
OUTLIVED=""
await_exit() {
    local name="$1" tenths=0
    shift
    while OUTLIVED="$(alive "$@")"; [ -n "$OUTLIVED" ]; do
        if [ "$tenths" -ge "$((GRACE * 10))" ]; then
            # shellcheck disable=SC2086
            log "$name: pid(s) $(echo $OUTLIVED) outlived the ${GRACE}s grace"
            return 0
        fi
        sleep 0.1
        tenths=$((tenths + 1))
    done
    return 0
}

# stop_pids NAME PID... -- SIGTERM every given pid still running, wait up to
# the grace period, then SIGKILL whatever is left.
stop_pids() {
    local name="$1" waited=0 pids left
    shift
    pids="$(alive "$@")"
    if [ -z "$pids" ]; then
        log "$name: none running"
        return 0
    fi
    # shellcheck disable=SC2086
    log "$name: asking pid(s) $(echo $pids) to shut down"
    # shellcheck disable=SC2086
    kill -TERM $pids 2>/dev/null || true
    # shellcheck disable=SC2086
    while left="$(alive $pids)"; [ -n "$left" ]; do
        if [ "$waited" -ge "$GRACE" ]; then
            # shellcheck disable=SC2086
            log "$name: pid(s) $(echo $left) outlived the ${GRACE}s grace; killing"
            # shellcheck disable=SC2086
            kill -KILL $left 2>/dev/null || true
            break
        fi
        sleep 1
        waited=$((waited + 1))
    done
    log "$name: stopped"
}

# THE PROCESSES TO STOP ARE NAMED BEFORE ANYTHING IS ASKED. The moment the
# daemon stands down, Emacs relaunches one from the fresh build and it starts
# fresh shims: those match the same paths, and signalling them would kill the
# new runtime under the very client that just brought it up.
#
# A PROCESS IS AGENT-REPL'S BY WHAT IT SERVES, NOT ONLY BY WHERE IT RUNS FROM.
# A daemon or shim a deploy started runs from the checkout the DAEMON was
# deployed from, which need not be this one, so matching this checkout's paths
# alone left such a shim serving its old build through a bounce, and the fresh
# daemon adopted it (2026-10-03T14:11:15). So the daemon is also the pid the
# state root's daemon.addr names, and a shim is also any process listening
# under the state root's sock/ directory, whichever build it runs.
# shellcheck disable=SC2207
daemons=($( { pids_of "$DAEMON_BIN"; advertised_daemon; } | sort -un))
# shellcheck disable=SC2207
shims=($( { pids_of "$SHIM_MAIN"; pids_of "--listen $STATE_ROOT/sock/"; } | sort -un))
# shellcheck disable=SC2207
locks=($(pids_of "$CACHE_BIN/shim-lock"))

# a. THE DAEMON STANDS ITSELF DOWN, AND ITS SHIMS WITH IT. SIGTERM is the
# daemon's orderly exit too, but that one leaves its shims running for a
# successor to adopt; a bounce wants them on the fresh bundle, and a shim
# killed under a daemon that did not order it is a death in that daemon's
# log, while one killed after the store is gone cannot conclude its session.
stand_down_daemon() {
    local answer
    if [ "${#daemons[@]}" -eq 0 ]; then
        log "daemon: none running"
        return 0
    fi
    log "daemon: asking pid(s) ${daemons[*]} to stand down now, its shims with it"
    if answer="$("$DAEMON_BIN" call -state-dir "$STATE_ROOT" UpdateShutdownSchedule \
        '{"now":{"reason":{"operator":{"note":"bounce-agent-repl-forcefully"}}}}' 2>&1)"; then
        log "daemon: the stand-down was accepted; waiting for it to leave"
        await_exit "daemon" "${daemons[@]}"
        # shellcheck disable=SC2086
        [ -z "$OUTLIVED" ] || stop_pids "daemon" $OUTLIVED
    else
        log "daemon: the stand-down was not accepted (${answer//$'\n'/ }); stopping it by signal"
        stop_pids "daemon" "${daemons[@]}"
    fi
    log "daemon: stopped"
}

stand_down_daemon
# b. WHATEVER THE DAEMON LEFT. After an accepted stand-down these are already
# gone and nothing is signalled.
stop_pids "shims" ${shims[@]+"${shims[@]}"}
stop_pids "shim locks" ${locks[@]+"${locks[@]}"}
log "done: the daemon and its shims are stood down; Emacs starts the fresh daemon when it next links, and its boot restarts the store and the sidecar onto the fresh build before any shim starts"
