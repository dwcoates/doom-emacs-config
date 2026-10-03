#!/usr/bin/env bash
# bounce-agent-repl-forcefully.sh -- rebuild every agent-repl component, then
# stand every running backend down in order and bring the services back on
# the fresh build.
#
# WHAT IT DOES, IN ORDER:
#
#   1. BUILDS everything in place in this checkout: the protobufs, then the
#      shim, webapp, daemon, store, sidecar and lock (`bin/build-frontend.sh
#      --force`). The webapp is built and never "deployed": Emacs serves the
#      checkout's dist, so every webview opened from now on loads the new one.
#      A FAILED BUILD STOPS HERE, with nothing killed: a bounce onto a build
#      that does not exist would leave nothing running at all.
#   2. STOPS every backend IN ORDER, each one only once nothing still running
#      depends on it, so every stop is a planned one at its source:
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
#           and the shims it starts, are the fresh build and are left alone;
#        c. only then the sidecar, then the store (the recorded safe order:
#           the sidecar writes into the store's socket): `launchctl bootout`
#           (launchd's SIGTERM; a kept-alive service only stops by leaving
#           the domain), then SIGKILL after the grace.
#   3. STARTS the store, waits for its socket, then the sidecar (the recorded
#      safe order), each from its installed plist.
#
# THE DAEMON IS NOT STARTED HERE: Emacs owns starting it (it spawns the daemon
# detached, with its own state root and flags). A running Emacs finds its link
# gone and starts the fresh daemon itself; a starting Emacs does the same at
# boot. The daemon then starts each shim on the fresh bundle as its workspaces
# are opened.
#
# Every process it signals is matched by THIS checkout's own paths (and the
# services' cache-bin paths), so a daemon or shim running from another
# checkout is left alone.
#
# Usage:
#   scripts/bounce-agent-repl-forcefully.sh
#
# Honored environment (the test's isolation; defaults are the live host):
#   AGENT_REPL_BOUNCE_GRACE        seconds each graceful stop is given (default 20)
#   AGENT_REPL_STATE_DIR           the state root whose daemon is asked to stand
#                                  down (default: the daemon's own, ~/.claude-emacs)
#   AGENT_REPL_BOUNCE_SOCK_MAX     seconds to wait for the store socket (default 180)
#   AGENT_REPL_BOUNCE_BUILDER      one executable run in place of the build
#   AGENT_REPL_LAUNCHCTL           launchctl to drive (default: the one on PATH)
#   AGENT_REPL_LAUNCH_AGENTS_DIR   where the installed plists live
#                                  (default ~/Library/LaunchAgents)
#   XDG_CACHE_HOME                 locates ~/.cache/agent-repl (default ~/.cache)
#
# Exit status: 0 when the services are back up on the fresh build; 1 when the
# build failed (nothing was stopped) or a service could not be brought back.

set -euo pipefail

log() { echo "[bounce] $*"; }
die() { echo "[bounce] $*" >&2; exit 1; }

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$THIS_DIR/.." && pwd)"

GRACE="${AGENT_REPL_BOUNCE_GRACE:-20}"
SOCK_MAX="${AGENT_REPL_BOUNCE_SOCK_MAX:-180}"
LAUNCHCTL="${AGENT_REPL_LAUNCHCTL:-launchctl}"
LAUNCH_AGENTS_DIR="${AGENT_REPL_LAUNCH_AGENTS_DIR:-$HOME/Library/LaunchAgents}"
CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
CACHE_BIN="$CACHE_HOME/agent-repl/bin"
STORE_SOCK="$CACHE_HOME/agent-repl/sock/store.sock"
STORE_LABEL="com.agentrepl.shim-store"
SIDECAR_LABEL="com.agentrepl.shim-claude-sidecar"
uid="$(id -u)"

DAEMON_BIN="$ROOT/daemon/bin/claude-repld"
SHIM_MAIN="$ROOT/agent-shim/claude/shim/dist/main.js"

# ---- 1. build --------------------------------------------------------------

build() {
    if [ -n "${AGENT_REPL_BOUNCE_BUILDER:-}" ]; then
        "$AGENT_REPL_BOUNCE_BUILDER"
        return
    fi
    make -C "$ROOT/proto" all
    bash "$ROOT/bin/build-frontend.sh" --force shim webapp daemon store sidecar lock
}

# THE PLISTS ARE CHECKED BEFORE ANYTHING IS STOPPED: a bootout with no plist to
# bootstrap back from would leave the host with no store and no sidecar.
for label in "$STORE_LABEL" "$SIDECAR_LABEL"; do
    [ -f "$LAUNCH_AGENTS_DIR/$label.plist" ] ||
        die "$LAUNCH_AGENTS_DIR/$label.plist is missing; nothing was built or stopped. Re-run .claude/install.sh --with-agent-shim-services to install it."
done

log "building every component in $ROOT ..."
if ! build; then
    die "the build failed; NOTHING WAS STOPPED, and every backend keeps running its current build"
fi
log "built"

# ---- 2. stop ---------------------------------------------------------------

# pids_of PATTERN -- the pids whose command line contains PATTERN, never this
# script's own.
pids_of() {
    local pid
    for pid in $(pgrep -f -- "$1" 2>/dev/null || true); do
        [ "$pid" = "$$" ] || [ "$pid" = "${BASHPID:-}" ] || echo "$pid"
    done
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
OUTLIVED=""
await_exit() {
    local name="$1" waited=0
    shift
    # shellcheck disable=SC2086
    while OUTLIVED="$(alive "$@")"; [ -n "$OUTLIVED" ]; do
        if [ "$waited" -ge "$GRACE" ]; then
            # shellcheck disable=SC2086
            log "$name: pid(s) $(echo $OUTLIVED) outlived the ${GRACE}s grace"
            return 0
        fi
        sleep 1
        waited=$((waited + 1))
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

# stop_processes NAME PATTERN -- stop_pids over every process matching PATTERN.
stop_processes() {
    # shellcheck disable=SC2046
    stop_pids "$1" $(pids_of "$2")
}

service_known() { "$LAUNCHCTL" print "gui/$uid/$1" >/dev/null 2>&1; }

service_pid() {
    { "$LAUNCHCTL" print "gui/$uid/$1" 2>/dev/null || true; } |
        awk '/^[[:space:]]*pid = /{ gsub(/[^0-9]/, "", $3); print $3; exit }'
}

# stop_service LABEL BINARY -- boot the service out of the user domain, wait
# up to the grace period, then SIGKILL its process and anything still running
# its binary.
stop_service() {
    local label="$1" binary="$2" waited=0 pid
    if service_known "$label"; then
        pid="$(service_pid "$label")"
        log "$label: booting out (pid ${pid:-none})"
        "$LAUNCHCTL" bootout "gui/$uid/$label" >/dev/null 2>&1 || true
        while service_known "$label"; do
            if [ "$waited" -ge "$GRACE" ]; then
                log "$label: still loaded after the ${GRACE}s grace; killing pid ${pid:-none}"
                [ -n "$pid" ] && kill -KILL "$pid" 2>/dev/null || true
                "$LAUNCHCTL" bootout "gui/$uid/$label" >/dev/null 2>&1 || true
                break
            fi
            sleep 1
            waited=$((waited + 1))
        done
    else
        log "$label: not loaded"
    fi
    # A copy launchd no longer owns (started by hand, or orphaned) goes too.
    stop_processes "$label (stray)" "$binary"
}

# THE PROCESSES TO STOP ARE NAMED BEFORE ANYTHING IS ASKED. The moment the
# daemon stands down, Emacs relaunches one from the fresh build and it starts
# fresh shims: those match the same paths, and signalling them would kill the
# new runtime under the very client that just brought it up.
# shellcheck disable=SC2207
daemons=($(pids_of "$DAEMON_BIN"))
# shellcheck disable=SC2207
shims=($(pids_of "$SHIM_MAIN"))
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
    if answer="$("$DAEMON_BIN" call UpdateShutdownSchedule \
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
# c. THE SERVICES LAST, AND THE SIDECAR BEFORE THE STORE.
stop_service "$SIDECAR_LABEL" "$CACHE_BIN/shim-claude-sidecar"
stop_service "$STORE_LABEL" "$CACHE_BIN/shim-store"
log "every backend is stopped"

# ---- 3. start --------------------------------------------------------------

# A SERVICE SOMEONE ELSE ALREADY BROUGHT BACK IS UP, NOT A FAILURE. Emacs
# relaunches a daemon the moment the old one is gone, and that daemon's cold
# start bootstraps the store and the sidecar itself -- from the fresh build,
# since the build ran before anything was stopped. Its bootstrap can land
# between this script's stop and its own, and launchd then refuses ours
# (error 5). A bootstrap that fails while the label is loaded is that race;
# one that fails with nothing loaded is a real failure.
start_service() {
    log "$1: bootstrapping"
    local err
    if err="$("$LAUNCHCTL" bootstrap "gui/$uid" "$LAUNCH_AGENTS_DIR/$1.plist" 2>&1 >/dev/null)"; then
        return 0
    fi
    if service_known "$1"; then
        log "$1: already loaded (pid $(service_pid "$1")): another client brought it back from the fresh build"
        return 0
    fi
    [ -n "$err" ] && printf '%s\n' "$err" >&2
    die "$1 could not be bootstrapped from $LAUNCH_AGENTS_DIR/$1.plist"
}

start_service "$STORE_LABEL"
waited=0
while [ ! -S "$STORE_SOCK" ]; do
    [ "$waited" -lt "$SOCK_MAX" ] ||
        die "$STORE_SOCK did not appear within ${SOCK_MAX}s; the sidecar was NOT started"
    sleep 1
    waited=$((waited + 1))
done
log "store: socket up"
start_service "$SIDECAR_LABEL"

log "done: the store and the sidecar run the fresh build; Emacs starts the fresh daemon (and the daemon its shims) when it next links"
