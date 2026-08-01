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
#                       script loudly (exit 3).
#
#                       The bounce PRESERVES the session shims — they outlive
#                       their daemon and the replacement reattaches — EXCEPT
#                       when step 2 moved the shim bundle, in which case the
#                       restart is asked to stop them (STOP-SHIMS) so no
#                       survivor keeps running the previous build's code. That
#                       is belt-and-braces beside the daemon's own version-
#                       driven stale-shim refresh, not the only guard
#  5b. webview refresh  `(agent-repl-refresh-webviews)` via emacsclient, right
#                       after the bounce: the pages mounted in Emacs outlive
#                       the daemon they loaded against, so each one is
#                       re-navigated to re-attach to the new daemon. Guarded
#                       with `fboundp' — a running Emacs that predates the
#                       command reports a skip instead of failing the deploy
#   6. elisp reload     with `--elisp <git-range>`: hot-load every non-test
#                       .el under modules/app/agent-repl changed in the range
#                       into the running Emacs (test-*.el is batch-only and is
#                       never loaded interactively).
#
#                       When the change set contains core.el, the load list is
#                       EXPANDED to the full module set in config.el's
#                       `agent-repl--load-module' order: core.el cancels every
#                       module timer at load time and only its owner files
#                       re-arm them, so a partial set containing core.el used
#                       to leave the running Emacs with a dead 1Hz heartbeat
#                       and a frozen tab bar
#  6b. timer assertion  `(agent-repl--assert-heartbeat-armed)' via emacsclient:
#                       verifies every required timer key is armed, re-arms
#                       anything stranded, and fails the deploy (exit 3) when
#                       a re-arm did not take
#
# Usage:  bin/deploy-all.sh [--force] [--no-bounce] [--elisp <git-range>]
#
#   --force        pass --force to build-frontend.sh and kickstart both
#                  services even when they already run the installed binary
#   --no-bounce    build everything, but skip service kickstarts, the daemon
#                  restart, and any elisp reload (pure build mode). Leaves the
#                  deployed stamps untouched, so a later real run still sees
#                  the freshly installed binaries as un-deployed and bounces.
#   --no-daemon-bounce
#                  everything through step 4 (services ARE kickstarted), but
#                  skip step 5's emacsclient restart and step 6's elisp reload.
#                  This is the mode EMACS ITSELF uses on its lazy boot path:
#                  step 5 bounces the daemon by calling back into Emacs over
#                  emacsclient, so an Emacs that ran the full script from
#                  inside its own session-open would be re-entering itself
#                  mid-boot. Emacs starts the daemon directly right after this
#                  returns, so the bounce would be redundant even if it were
#                  safe.
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
MOD_REL="${ROOT#"$REPO_ROOT/"}"            # e.g. modules/app/agent-repl

# shellcheck source=lib-deploy-stamp.sh
. "$THIS_DIR/lib-deploy-stamp.sh"

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
NO_DAEMON_BOUNCE=0
ELISP_RANGE=""
while [ $# -gt 0 ]; do
    case "$1" in
        --force)     FORCE=1 ;;
        --no-bounce) NO_BOUNCE=1 ;;
        --no-daemon-bounce) NO_DAEMON_BOUNCE=1 ;;
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
# Whether the SHIM BUNDLE moved decides the daemon bounce's stop-shims mode
# below. Two signals, because neither alone is sufficient:
#
#   - the built-sha stamp, which moves whenever the source revision does; and
#   - the bundle's own content fingerprint, which is the only signal that moves
#     within one revision — the common case of a DIRTY tree, where the stamp
#     reads "<sha>-dirty" both before and after a rebuild that changed the code.
#
# Either differing means a surviving shim would now be running superseded code.
SHIM_BUNDLE="$ROOT/agent-shim/claude/shim/dist/main.js"
SHIM_STAMP="$ROOT/agent-shim/claude/shim/dist/.built-sha"

shim_identity() {
    read_built_sha "$SHIM_STAMP" 2>/dev/null || echo "no-stamp"
    if [ -f "$SHIM_BUNDLE" ]; then binary_fingerprint "$SHIM_BUNDLE"; else echo "no-bundle"; fi
}

SHIM_BEFORE="$(shim_identity)"

if [ "$FORCE" -eq 1 ]; then
    "$THIS_DIR/build-frontend.sh" --force
else
    "$THIS_DIR/build-frontend.sh"
fi

SHIM_AFTER="$(shim_identity)"
SHIM_CHANGED=0
if [ "$SHIM_BEFORE" != "$SHIM_AFTER" ]; then
    SHIM_CHANGED=1
    log "shim: bundle moved since the last deploy — the daemon bounce will STOP surviving shims"
else
    log "shim: bundle unchanged — the daemon bounce will preserve surviving shims"
fi

# ---- 3. daemon, forced (staleness cannot see proto regen) ------------------
log "daemon: forced rebuild..."
mkdir -p "$ROOT/daemon/bin"
( cd "$ROOT/daemon" && go build -o "$ROOT/daemon/bin/claude-repld" ./cmd/claude-repld )
write_built_sha "$ROOT/daemon/bin/.built-sha" "$ROOT"
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
    # The built-sha stamp is written even when the CONTENT is unchanged: the
    # binary is byte-identical, but the revision it was reproduced from has
    # moved on, and that revision is what the readiness report compares
    # against master. It is the deployed-FINGERPRINT stamp that must not be
    # touched here — that one is bounce detection and belongs to kickstart.
    write_built_sha "$CACHE_BIN/.$name.built-sha" "$ROOT"
    if [ -f "$installed" ] && cmp -s "$staged" "$installed"; then
        rm -f "$staged"
        log "$name: build unchanged"
        return 0
    fi
    mv -f "$staged" "$installed"
    log "$name: installed (changed)"
}

# Whether the RUNNING service is executing the installed binary. The rule and
# the reasoning behind it now live in lib-deploy-stamp.sh, shared with
# readiness-report.sh so the report and the deploy can never disagree about
# what "already deployed" means; these are the thin CACHE_BIN-bound wrappers.
needs_bounce() { service_needs_bounce "$CACHE_BIN" "$1"; }

record_deployed() { record_service_deployed "$CACHE_BIN" "$1"; }

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
# Skipped for a caller that owns the daemon's lifecycle itself. Emacs\'s lazy
# boot path is the one that does: it runs this script and then starts the
# daemon directly, so bouncing here would both re-enter the calling Emacs over
# emacsclient and fight the launch about to happen.
if [ "$NO_DAEMON_BOUNCE" -eq 1 ]; then
    log "--no-daemon-bounce: services deployed; the caller owns the daemon restart"
    exit 0
fi

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
            log "webviews: no Emacs running, so no live page to refresh"
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
    if [ "$SHIM_CHANGED" -eq 1 ]; then
        RESTART_FORM='(agent-repl-frontend-daemon-restart t)'
        log "daemon: restarting via emacsclient (stop-shims: the bundle changed)..."
    else
        RESTART_FORM='(agent-repl-frontend-daemon-restart)'
        log "daemon: restarting via emacsclient..."
    fi
    RESTART_OUT="$("$EMACSCLIENT" --eval "$RESTART_FORM" 2>&1)" || {
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

    # ---- 5b. webview refresh -----------------------------------------------
    # The pages mounted in Emacs outlive the daemon they were loaded against,
    # so a bounced daemon leaves every live webview talking to a listener that
    # no longer exists. Re-navigate them here, from the deploy side.
    #
    # `fboundp'-guarded: the running Emacs may predate the command (the elisp
    # hot-load in step 6 is what would define it, and it runs AFTER this), and
    # a not-yet-loaded symbol must not fail the deploy — an old Emacs simply
    # reports the refresh as skipped.
    REFRESH_FORM='(if (fboundp (quote agent-repl-refresh-webviews)) (format "refreshed %d" (agent-repl-refresh-webviews)) "absent")'
    REFRESH_OUT="$("$EMACSCLIENT" --eval "$REFRESH_FORM" 2>&1)" || {
        echo "[deploy-all] webview refresh failed: $REFRESH_OUT" >&2
        exit 3
    }
    case "$REFRESH_OUT" in
        *absent*)   log "webviews: skipped — function absent (Emacs predates agent-repl-refresh-webviews)" ;;
        *refreshed*) log "webviews: ${REFRESH_OUT//\"/}" ;;
        *)
            echo "[deploy-all] webview refresh returned an unrecognized result: $REFRESH_OUT" >&2
            exit 3
            ;;
    esac
fi

# ---- 6. elisp hot-reload ---------------------------------------------------
#
# core.el runs `(agent-repl--cancel-all-timers)' at LOAD time, which clears
# every module timer including the 1Hz heartbeat that repaints the tab bar.
# The timers are re-armed only by their OWNER files (status.el, readiness.el,
# autosave.el, workspace-status-export.el), so hot-loading a change set that
# contains core.el but not all four owners used to leave a live Emacs with no
# heartbeat at all — frozen tabs until somebody noticed.
#
# So a change set containing core.el is expanded to the FULL module set, in
# config.el's canonical `agent-repl--load-module' order. That order is read
# from config.el itself rather than hardcoded here, so a module added to the
# loader is picked up without touching this script.
#
# Belt-and-braces beside core.el's own `agent-repl--assert-heartbeat-armed',
# which self-heals a bare core.el load; the post-load verification below asks
# the running Emacs for that assertion's result and reports it.

# Emit the canonical module set as repo-relative paths, in load order.
canonical_module_files() {
    local m
    sed -n 's/^(agent-repl--load-module "\([^"]*\)").*/\1/p' "$ROOT/config.el" \
    | while IFS= read -r m; do
        [ -f "$ROOT/$m.el" ] || continue
        printf '%s\n' "$MOD_REL/$m.el"
    done
}

if [ -n "$ELISP_RANGE" ] && [ "$EMACS_AVAILABLE" -eq 1 ]; then
    log "elisp: reloading non-test .el changed in $ELISP_RANGE..."

    CHANGED=()
    CORE_IN_SET=0
    while IFS= read -r rel; do
        base="$(basename "$rel")"
        case "$base" in test-*.el) continue ;; esac   # batch-only harness files
        [ -f "$REPO_ROOT/$rel" ] || continue          # deleted in range
        [ "$base" = "core.el" ] && CORE_IN_SET=1
        CHANGED+=("$rel")
    done < <(git -C "$REPO_ROOT" diff --name-only "$ELISP_RANGE" -- "$MOD_REL/*.el")

    LOAD_LIST=()
    if [ "$CORE_IN_SET" -eq 1 ]; then
        while IFS= read -r rel; do
            LOAD_LIST+=("$rel")
        done < <(canonical_module_files)
        # Anything changed that the loader does not name (config.el itself,
        # for instance) still gets loaded, after the canonical set.
        for rel in ${CHANGED[@]+"${CHANGED[@]}"}; do
            found=0
            for c in ${LOAD_LIST[@]+"${LOAD_LIST[@]}"}; do
                [ "$c" = "$rel" ] && { found=1; break; }
            done
            [ "$found" -eq 0 ] && LOAD_LIST+=("$rel")
        done
        log "elisp: core.el in change set — expanding to full module reload (${#LOAD_LIST[@]} files)"
    else
        LOAD_LIST=(${CHANGED[@]+"${CHANGED[@]}"})
    fi

    for rel in ${LOAD_LIST[@]+"${LOAD_LIST[@]}"}; do
        log "elisp: load $(basename "$rel")"
        "$EMACSCLIENT" --eval "(load \"$REPO_ROOT/$rel\" nil t)" >/dev/null
    done
    log "elisp: done"

    # ---- 6b. post-load timer-contract verification -------------------------
    # Ask the reloaded Emacs whether every required timer key is armed. The
    # assertion re-arms anything stranded, so a nonzero `rearmed' is a repaired
    # deploy, while a nonzero `failed' is a broken one and fails the script.
    #
    # `fboundp'-guarded like the webview refresh above: an Emacs that predates
    # the assertion reports a skip rather than failing the deploy.
    ASSERT_FORM='(if (fboundp (quote agent-repl--assert-heartbeat-armed)) (let ((r (agent-repl--assert-heartbeat-armed))) (format "armed=%d rearmed=%d failed=%d unavailable=%d" (length (plist-get r :armed)) (length (plist-get r :rearmed)) (length (plist-get r :failed)) (length (plist-get r :unavailable)))) "absent")'
    ASSERT_OUT="$("$EMACSCLIENT" --eval "$ASSERT_FORM" 2>&1)" || {
        echo "[deploy-all] elisp: heartbeat assertion failed to run: $ASSERT_OUT" >&2
        exit 3
    }
    ASSERT_OUT="${ASSERT_OUT//\"/}"
    case "$ASSERT_OUT" in
        absent)
            log "elisp: heartbeat assertion skipped — function absent (Emacs predates agent-repl--assert-heartbeat-armed)"
            ;;
        armed=*)
            log "elisp: heartbeat assertion: $ASSERT_OUT"
            case "$ASSERT_OUT" in
                *"rearmed=0"*) ;;
                *) log "elisp: heartbeat assertion RE-ARMED stranded timers — see the agent-repl log for which" ;;
            esac
            case "$ASSERT_OUT" in
                *"failed=0"*) ;;
                *)
                    echo "[deploy-all] elisp: a required timer could not be re-armed: $ASSERT_OUT" >&2
                    exit 3
                    ;;
            esac
            ;;
        *)
            echo "[deploy-all] elisp: heartbeat assertion returned an unrecognized result: $ASSERT_OUT" >&2
            exit 3
            ;;
    esac
fi

log "deploy complete"
