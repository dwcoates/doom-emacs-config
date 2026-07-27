#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# deploy-all.sh — one-shot build + deploy of the whole agent-repl stack.
#
# Runs the full chain in dependency order:
#
#   1. protobufs        `make -C proto all` (Go + TS regeneration)
#   2. build-frontend   shim bundle, webapp, daemon (build-frontend.sh)
#   3. daemon (forced)  `go build` unconditionally — build-frontend's mtime
#                       staleness check cannot see proto REGENERATION (the
#                       generated .go files live outside daemon/), so after
#                       step 1 the daemon must be rebuilt regardless
#   4. store/sidecar    `go build` into ~/.cache/agent-repl/bin; each service
#                       is kickstarted when the installed binary is not the one
#                       its running process was started on (a stamp written at
#                       kickstart time, NOT "did this build change the file" —
#                       see needs_bounce), store strictly before sidecar with a
#                       wait on store.sock in between — the recorded safe order
#                       (a simultaneous bounce once cost a silent full re-read
#                       via cold cursor recovery)
#   5. daemon bounce    `(agent-repl-frontend-daemon-restart)` via emacsclient;
#                       when no Emacs server is reachable the bounce is
#                       explicitly deferred until Emacs startup. Once a server
#                       is reachable, any restart failure still fails this
#                       script loudly (exit 3)
#   6. elisp reload     with `--elisp <git-range>`: hot-load every non-test
#                       .el under modules/app/agent-repl changed in the range
#                       into the running Emacs (test-*.el is batch-only and is
#                       never loaded interactively)
#
# Usage:  bin/deploy-all.sh [--force] [--no-bounce] [--elisp <git-range>]
#
#   --force        pass --force to build-frontend.sh and kickstart both
#                  services even when they already run the installed binary
#   --no-bounce    build everything, but skip service kickstarts, the daemon
#                  restart, and any elisp reload (pure build mode). Leaves the
#                  deployed stamps untouched, so a later real run still sees
#                  the freshly installed binaries as un-deployed and bounces.
#   --elisp RANGE  after a successful bounce, hot-load changed non-test .el
#                  files from `git diff --name-only RANGE`
#
# Environment:
#   AGENT_REPL_STORE_SOCK_TIMEOUT  seconds to wait for store.sock after the
#                                  store kickstart (default 15)

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(dirname "$THIS_DIR")"              # modules/app/agent-repl
REPO_ROOT="$(cd "$ROOT/../../.." && pwd)"  # the git checkout

CACHE_BIN="$HOME/.cache/agent-repl/bin"
STORE_SOCK="$HOME/.cache/agent-repl/sock/store.sock"
STORE_LABEL="com.agentrepl.shim-store"
SIDECAR_LABEL="com.agentrepl.shim-claude-sidecar"
SOCK_TIMEOUT="${AGENT_REPL_STORE_SOCK_TIMEOUT:-15}"

# Overridable so the hermetic test harness can substitute its PATH stub — the
# default absolute path would silently bypass any stub and reach the LIVE
# Emacs, which is exactly what a test run must never do.
EMACSCLIENT="${AGENT_REPL_EMACSCLIENT:-/Applications/Emacs.app/Contents/MacOS/bin/emacsclient}"
command -v "$EMACSCLIENT" >/dev/null 2>&1 || EMACSCLIENT="emacsclient"

FORCE=0
NO_BOUNCE=0
ELISP_RANGE=""
while [ $# -gt 0 ]; do
    case "$1" in
        --force)     FORCE=1 ;;
        --no-bounce) NO_BOUNCE=1 ;;
        --elisp)     shift; ELISP_RANGE="${1:?--elisp needs a git range}" ;;
        *) echo "[deploy-all] unknown argument: $1" >&2; exit 2 ;;
    esac
    shift
done

log() { echo "[deploy-all] $*"; }

# ---- 1. protobufs ----------------------------------------------------------
log "proto: regenerating (make all)..."
make -C "$ROOT/proto" all
log "proto: done"

# ---- 2. build-frontend (shim bundle, webapp, daemon) -----------------------
if [ "$FORCE" -eq 1 ]; then
    "$THIS_DIR/build-frontend.sh" --force
else
    "$THIS_DIR/build-frontend.sh"
fi

# ---- 3. daemon, forced (staleness cannot see proto regen) ------------------
log "daemon: forced rebuild..."
mkdir -p "$ROOT/daemon/bin"
( cd "$ROOT/daemon" && go build -o "$ROOT/daemon/bin/claude-repld" ./cmd/claude-repld )
log "daemon: done"

# ---- 4. store + sidecar ----------------------------------------------------
# Build each into a staging path, install over the launchd-run copy only when
# the content differs, and remember which ones changed so only those services
# bounce. `cmp` (not mtime) decides: go build always rewrites the output file.
mkdir -p "$CACHE_BIN"

build_service() { # name module-dir — builds and installs when content differs
    local name="$1" dir="$2"
    local installed="$CACHE_BIN/$name" staged="$CACHE_BIN/.$name.staged"
    log "$name: building..."
    ( cd "$dir" && go build -o "$staged" . )
    if [ -f "$installed" ] && cmp -s "$staged" "$installed"; then
        rm -f "$staged"
        log "$name: build unchanged"
        return 0
    fi
    mv -f "$staged" "$installed"
    log "$name: installed (changed)"
}

# Whether the RUNNING service is executing the installed binary, tracked by a
# stamp written at kickstart time rather than inferred from "did this build
# change the file". Those are different questions, and conflating them is a
# real failure mode: a `--no-bounce` run installs a new binary without
# restarting anything, so the next run's build is "unchanged" while the live
# process is still on the old image — and the deploy silently skips the bounce
# the user asked for. The stamp answers the question that actually matters.
fingerprint() { shasum -a 256 "$1" | cut -d' ' -f1; }

needs_bounce() { # name -> 0 (bounce) when the installed binary is not the deployed one
    local name="$1"
    # Separate statement, not a second `local` assignment: a builtin's
    # arguments all expand before any of them are assigned, so `$name` would
    # still be unset here and `set -u` would abort the run.
    local stamp="$CACHE_BIN/.$name.deployed"
    [ -f "$stamp" ] || return 0
    [ "$(fingerprint "$CACHE_BIN/$name")" = "$(cat "$stamp")" ] && return 1
    return 0
}

record_deployed() { fingerprint "$CACHE_BIN/$1" > "$CACHE_BIN/.$1.deployed"; }

build_service shim-store          "$ROOT/agent-shim/shim-store"
build_service shim-claude-sidecar "$ROOT/agent-shim/claude/shim-sidecar"

STORE_STALE=0
SIDECAR_STALE=0
if needs_bounce shim-store; then STORE_STALE=1; fi
if needs_bounce shim-claude-sidecar; then SIDECAR_STALE=1; fi

if [ "$NO_BOUNCE" -eq 1 ]; then
    log "--no-bounce: skipping kickstarts, daemon restart, and elisp reload"
    exit 0
fi

kickstart() { launchctl kickstart -k "gui/$(id -u)/$1"; }

wait_for_store_sock() {
    local waited=0
    while [ ! -S "$STORE_SOCK" ]; do
        if [ "$waited" -ge "$SOCK_TIMEOUT" ]; then
            echo "[deploy-all] store.sock did not appear within ${SOCK_TIMEOUT}s after kickstart" >&2
            exit 1
        fi
        sleep 1
        waited=$((waited + 1))
    done
}

if [ "$STORE_STALE" -eq 1 ] || [ "$FORCE" -eq 1 ]; then
    log "store: kickstarting $STORE_LABEL..."
    # The store's socket is unlinked on shutdown and recreated on boot, so we
    # wait for the NEW instance's socket — the recorded safe order requires
    # the store healthy before the sidecar bounces (cold cursor recovery on
    # the sidecar is a silent full re-read).
    kickstart "$STORE_LABEL"
    wait_for_store_sock
    record_deployed shim-store
    log "store: up ($STORE_SOCK)"
    # A store bounce always bounces the sidecar too, stale or not: the
    # sidecar's link recovery is connection-scoped, and a fresh pair is the
    # recorded known-good state after a store restart.
    SIDECAR_STALE=1
fi

if [ "$SIDECAR_STALE" -eq 1 ] || [ "$FORCE" -eq 1 ]; then
    log "sidecar: kickstarting $SIDECAR_LABEL..."
    kickstart "$SIDECAR_LABEL"
    record_deployed shim-claude-sidecar
    log "sidecar: done"
fi

# ---- 5. daemon bounce ------------------------------------------------------
# No running Emacs means there is no live daemon to bounce and no old elisp to
# hot-reload. The normal agent-repl startup path rebuilds/bounces the backend
# before restoring workspaces, then loads these files from disk. Treat that
# state as an explicit deferred deployment, not as a restart failure.
EMACS_AVAILABLE=0
EMACS_PROBE_OUT=""
if ! EMACS_PROBE_OUT="$("$EMACSCLIENT" --eval t 2>&1)"; then
    case "$EMACS_PROBE_OUT" in
        *"can't find socket"*|*"No socket or alternate editor"*|*"Could not connect to the Emacs daemon"*|*"Connection refused"*)
            log "daemon: Emacs is not running; restart deferred until Emacs starts"
            log "daemon: the rebuilt backend will start automatically at startup"
            if [ -n "$ELISP_RANGE" ]; then
                log "elisp: reload deferred; Emacs will load the changed files at startup"
            fi
            ;;
        *)
            echo "[deploy-all] Emacs server probe failed: $EMACS_PROBE_OUT" >&2
            exit 3
            ;;
    esac
else
    EMACS_AVAILABLE=1
    log "daemon: Emacs server probe succeeded: $EMACS_PROBE_OUT"
    log "daemon: restarting via emacsclient..."
    RESTART_OUT="$("$EMACSCLIENT" --eval '(agent-repl-frontend-daemon-restart)' 2>&1)" || {
        echo "[deploy-all] daemon restart failed: $RESTART_OUT" >&2
        exit 3
    }
    case "$RESTART_OUT" in
        *refusing*)
            # emacsclient exits 0 even when the elisp signals; the refusal text
            # is the only tell. A refused bounce means the deploy is NOT complete.
            echo "[deploy-all] daemon restart refused (turn in flight): $RESTART_OUT" >&2
            exit 3
            ;;
    esac
    log "daemon: restarted"
fi

# ---- 6. elisp hot-reload ---------------------------------------------------
if [ -n "$ELISP_RANGE" ] && [ "$EMACS_AVAILABLE" -eq 1 ]; then
    log "elisp: reloading non-test .el changed in $ELISP_RANGE..."
    while IFS= read -r rel; do
        base="$(basename "$rel")"
        case "$base" in test-*.el) continue ;; esac   # batch-only harness files
        [ -f "$REPO_ROOT/$rel" ] || continue          # deleted in range
        log "elisp: load $base"
        "$EMACSCLIENT" --eval "(load \"$REPO_ROOT/$rel\" nil t)" >/dev/null
    done < <(git -C "$REPO_ROOT" diff --name-only "$ELISP_RANGE" -- 'modules/app/agent-repl/*.el')
    log "elisp: done"
fi

log "deploy complete"
