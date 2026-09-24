#!/usr/bin/env bash
# store-reset.sh -- discard the record store's database and bring the pair back up.
#
# THE STORE IS A CACHE, AND DURING DEVELOPMENT IT NEEDS NO RETENTION (owner
# ruling 2026-09-13). Everything in `events.db` is re-derivable: the sidecar
# re-reads the vendor's transcripts from offset zero once its cursors are gone,
# and the shim re-observes the live stream. So the answer to a store that has
# grown past what its host wants to carry is to throw the file away, not to
# prune it -- exactly the reasoning `internal/db`'s "nuked, never migrated"
# already applies to a superseded schema.
#
# WHY THE SIDECAR IS STOPPED FIRST AND STARTED LAST. The sidecar's reader
# positions live in `cursor`, IN THIS DATABASE. A sidecar left running across
# the unlink keeps writing into a deleted inode and holds cursors for a file
# that no longer exists, so its next successful batch would advance positions
# against a database that never saw the records they claim. Stopping it first
# and starting it after the store is serving is the same recorded safe order
# the daemon's deploy (daemon/internal/deploy) uses (store strictly before
# sidecar, with a wait on the socket in between); a simultaneous bounce once
# cost a silent full re-read.
#
# WHY THE OPT-IN ENVIRONMENT VARIABLE. This deletes every stored record on the
# host, and the sidecar's re-read of the owner's whole corpus afterwards is
# hours of work. A flag alone is too easy to reach by tab-completion or by
# copying a line out of a document, so the intent is stated twice: the script
# must be asked, and the environment must say AGENT_REPL_STORE_RESET=1.

set -euo pipefail

log()  { echo "[store-reset] $*"; }
die()  { echo "[store-reset] $*" >&2; exit 1; }

usage() {
    cat <<'EOF'
Usage:
  AGENT_REPL_STORE_RESET=1 bin/store-reset.sh [--keep-down]

Stops the sidecar and the store, removes events.db and its -wal/-shm
siblings, then starts the store, waits for its socket, and starts the
sidecar.

  --keep-down   remove the database but leave both services stopped

Refuses unless AGENT_REPL_STORE_RESET=1 is set in the environment.

Honored environment:
  AGENT_REPL_STORE_RESET   must be exactly 1, or the script refuses
  XDG_CACHE_HOME           locates the store directory (default ~/.cache)
  AGENT_REPL_LAUNCHCTL     launchctl to drive (default: the one on PATH)
  AGENT_REPL_LAUNCH_AGENTS_DIR  where the installed plists live
                           (default ~/Library/LaunchAgents)
  AGENT_REPL_STORE_SOCK_MAX  seconds to wait for the socket (default 180)
EOF
}

KEEP_DOWN=0
while [ $# -gt 0 ]; do
    case "$1" in
        --keep-down) KEEP_DOWN=1 ;;
        -h|--help)   usage; exit 0 ;;
        *) echo "[store-reset] unknown argument: $1" >&2; usage >&2; exit 2 ;;
    esac
    shift
done

# THE GUARD IS AN EXACT MATCH, not "non-empty". `AGENT_REPL_STORE_RESET=0` and
# `AGENT_REPL_STORE_RESET=no` are somebody saying no in the only two spellings
# they are likely to reach for, and a truthiness test would read both as yes.
if [ "${AGENT_REPL_STORE_RESET:-}" != "1" ]; then
    die "refusing: this deletes every stored record. Re-run with AGENT_REPL_STORE_RESET=1 to confirm."
fi

CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
STORE_DIR="$CACHE_HOME/agent-repl/store"
STORE_DB="$STORE_DIR/events.db"
STORE_SOCK="$CACHE_HOME/agent-repl/sock/store.sock"
STORE_LABEL="com.agentrepl.shim-store"
SIDECAR_LABEL="com.agentrepl.shim-claude-sidecar"
SOCK_MAX="${AGENT_REPL_STORE_SOCK_MAX:-180}"

# WHERE THE PLISTS LIVE. The same directory `.claude/install.sh`
# (install_agent_shim_services) copies each rewritten template into, and the
# same `$LABEL.plist` naming, because a bootstrap needs the file the service
# was bootstrapped from originally. Overridable for the hermetic harness, for
# the same reason AGENT_REPL_LAUNCHCTL is.
LAUNCH_AGENTS_DIR="${AGENT_REPL_LAUNCH_AGENTS_DIR:-$HOME/Library/LaunchAgents}"
service_plist() { printf '%s/%s.plist' "$LAUNCH_AGENTS_DIR" "$1"; }

# Overridable so the hermetic harness can substitute its PATH stub: reaching
# the LIVE launchd from a test run is exactly what must never happen.
LAUNCHCTL="${AGENT_REPL_LAUNCHCTL:-launchctl}"

uid="$(id -u)"

# NOT RUNNING IS AN ANSWER, NOT A FAILURE. `launchctl print` exits non-zero for
# a label it does not know, and under `set -o pipefail` that would abort the
# script at the very question it is asking -- so the pipeline's status is
# discarded and the ABSENCE OF A PID is the whole signal.
service_pid() { # LABEL -> pid, or empty when launchd reports none
    { "$LAUNCHCTL" print "gui/$uid/$1" 2>/dev/null || true; } |
        awk '/^[[:space:]]*pid = /{ gsub(/[^0-9]/, "", $3); print $3; exit }'
}

# STOP MEANS STOPPED, AND A KEPT-ALIVE SERVICE ONLY STOPS BY LEAVING THE
# DOMAIN. Both plists set `KeepAlive`, so launchd relaunches the process the
# instant it exits: a `launchctl kill` -- even SIGKILL -- is answered by a new
# pid within a second, and a poll for "launchd reports no pid" can never
# succeed. That is not a hypothetical; it is what this script did three times
# on 2026-09-13, timing out at 180s per service having stopped nothing. So the
# stop is `bootout`, which removes the service from the user domain: the
# process is signalled AND the relaunch is taken away with it. The poll then
# asks the question bootout can actually answer -- does launchd still KNOW this
# label -- and `bootstrap` puts it back afterwards.
service_known() { # LABEL -> 0 when launchd still has the service loaded
    "$LAUNCHCTL" print "gui/$uid/$1" >/dev/null 2>&1
}

stop_service() { # LABEL
    local label="$1" waited=0 pid
    if ! service_known "$label"; then
        log "$label: already stopped"
        return 0
    fi
    pid="$(service_pid "$label")"
    log "$label: booting out (pid ${pid:-none})..."
    "$LAUNCHCTL" bootout "gui/$uid/$label" >/dev/null 2>&1 || true
    while service_known "$label"; do
        if [ "$waited" -ge "$SOCK_MAX" ]; then
            die "$label did not exit within ${SOCK_MAX}s; nothing was removed and no service was restarted"
        fi
        sleep 1
        waited=$((waited + 1))
    done
    log "$label: stopped"
}

# A BOOTED-OUT SERVICE COMES BACK BY BOOTSTRAPPING ITS PLIST, not by starting a
# label the domain no longer holds. `RunAtLoad` is set in both plists, so
# bootstrap runs the process itself and nothing further is needed.
start_service() { # LABEL
    local plist
    plist="$(service_plist "$1")"
    log "$1: bootstrapping $plist..."
    "$LAUNCHCTL" bootstrap "gui/$uid" "$plist" >/dev/null
}

wait_for_store_sock() {
    local waited=0
    while [ ! -S "$STORE_SOCK" ]; do
        if [ -z "$(service_pid "$STORE_LABEL")" ]; then
            die "the store died before $STORE_SOCK appeared (${waited}s after start). The sidecar was NOT started."
        fi
        if [ "$waited" -ge "$SOCK_MAX" ]; then
            die "$STORE_SOCK did not appear within the ${SOCK_MAX}s upper bound. The sidecar was NOT started."
        fi
        sleep 1
        waited=$((waited + 1))
    done
    log "store: socket up ($STORE_SOCK)"
}

# A DIRECTORY AT THE DATABASE PATH IS REPORTED, NEVER REMOVED -- the same guard
# `db.Open` applies before it unlinks anything, and for the same reason:
# deleting whatever happens to sit at a path is how a tool destroys somebody
# else's data. An ABSENT database is not an error; the reset's postcondition is
# "no database here", and it already holds.
if [ -e "$STORE_DB" ] && [ ! -f "$STORE_DB" ]; then
    die "$STORE_DB exists and is not a regular file; refusing to remove it"
fi

# THE PLISTS ARE CHECKED BEFORE ANYTHING IS STOPPED. A bootout with no plist to
# bootstrap back from would leave the host with no store and no sidecar and no
# way for this script to return them, so a missing file is a refusal taken
# while everything is still running and nothing has been removed.
if [ "$KEEP_DOWN" -eq 0 ]; then
    for label in "$STORE_LABEL" "$SIDECAR_LABEL"; do
        plist="$(service_plist "$label")"
        [ -f "$plist" ] || die "$plist is missing; this reset boots the services out and needs their plists to bring them back. Nothing was stopped and nothing was removed. Re-run .claude/install.sh --with-agent-shim-services to reinstall them."
    done
fi

# The sidecar goes down first (its cursors live in the file), then the store.
stop_service "$SIDECAR_LABEL"
stop_service "$STORE_LABEL"

# THE SIBLINGS GO WITH IT. A stale -wal beside a fresh database is how a
# "recreated" store comes up carrying fragments of the one it replaced.
removed=0
for f in "$STORE_DB" "$STORE_DB-wal" "$STORE_DB-shm"; do
    if [ -f "$f" ]; then
        rm -f "$f" || die "could not remove $f"
        removed=$((removed + 1))
    fi
done
log "removed $removed database file(s) under $STORE_DIR"

if [ "$KEEP_DOWN" -eq 1 ]; then
    log "--keep-down: the store and the sidecar are left stopped"
    exit 0
fi

start_service "$STORE_LABEL"
wait_for_store_sock
start_service "$SIDECAR_LABEL"
log "done: the store is serving an empty database and the sidecar is re-reading from offset zero"
