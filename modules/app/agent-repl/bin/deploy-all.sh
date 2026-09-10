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
#   3. daemon (forced)  `go build` unconditionally. Step 1 may have rewritten
#                       generated sources, and this build is cheap next to
#                       being wrong about it
#   4. store/sidecar    `go build` into ~/.cache/agent-repl/bin; each service
#                       is kickstarted when the installed binary is not the one
#                       its running process was started on (a stamp written at
#                       kickstart time, NOT "did this build change the file" —
#                       see needs_bounce), store strictly before sidecar with a
#                       wait on store.sock in between — the recorded safe order
#                       (a simultaneous bounce once cost a silent full re-read
#                       via cold cursor recovery)
#  4b. revision gate   readiness-report.sh --require-ready webapp, run once
#                       everything is built and BEFORE any kickstart or daemon
#                       restart: a stale artifact must never be bounced into
#   5. runtime bounce   first loads the runtime control plane from THIS
#                       checkout so its artifact-root constants name the
#                       binaries built above, then calls
#                       `(agent-repl-runtime-restart-await)` via emacsclient.
#                       This ordering is load-bearing for linked worktrees: the
#                       running Emacs may have loaded the module from the main
#                       worktree, and restarting before rebinding those paths
#                       launches the main worktree's stale build.
#
#                       The await form returns only on terminal completion (the
#                       string `runtime-restart-complete`) and SIGNALS on
#                       failure or timeout, so this script can never mistake a
#                       dispatch for a finished deployment.
#
#                       When no Emacs server is reachable the bounce is
#                       explicitly deferred until Emacs startup. Once a server
#                       is reachable, any restart failure still fails this
#                       script loudly (exit 3).
#
#                       Surviving shims are NOT stopped here and no webview is
#                       re-navigated here. The incoming daemon owns both: its
#                       rollout controller bounces a shim whose reported build
#                       identity is stale, and it pushes `reload_webapp` to the
#                       clients that must reload. This script only reports what
#                       moved.
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
#   --force        pass --force to build-frontend.sh. It does NOT force the
#                  store/sidecar kickstarts: a forced rebuild reproduces a
#                  byte-identical binary, and bouncing on that alone dropped
#                  every live shim's store producer connection and standing
#                  subscription for nothing. A retry loop of 15 forced deploys
#                  on 2026-08-08 turned that into a fleet-wide warn storm. The
#                  deployed-fingerprint stamp is the SOLE kickstart authority,
#                  and it already bounces anything genuinely not running the
#                  installed image (including an installed-but-never-started
#                  binary), so nothing a force could legitimately want is lost
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
#   AGENT_REPL_STORE_SOCK_TIMEOUT  seconds the store may go without writing a
#                                  single byte to its log while store.sock is
#                                  still absent (default 15). It is a STALL
#                                  budget, not a deadline: a boot that is
#                                  visibly working resets it.
#   AGENT_REPL_STORE_SOCK_MAX      the upper bound on the whole wait, however
#                                  busy the store looks (default 180)

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(dirname "$THIS_DIR")"              # modules/app/agent-repl
REPO_ROOT="$(cd "$ROOT/../../.." && pwd)"  # the git checkout
MOD_REL="${ROOT#"$REPO_ROOT/"}"            # e.g. modules/app/agent-repl

# shellcheck source=lib-deploy-stamp.sh
. "$THIS_DIR/lib-deploy-stamp.sh"

# Every artifact this script builds itself owes BOTH stamps: the built-sha, and
# the source-tree stamp that is the staleness authority build-frontend.sh and
# readiness-report.sh share. Writing only the first would leave the artifact
# looking un-built to the very gate a few lines below.
stamp_built_tree() { # NAME STAMP-DIR
    local paths
    paths="$(deploy_stamp_system_paths "$1" "$MOD_REL")" || return 0
    # Deliberately unquoted: the table carries a space-separated pathspec list.
    # shellcheck disable=SC2086
    write_source_tree "$2" "$(source_tree_id "$REPO_ROOT" $paths || true)"
}

CACHE_BIN="$HOME/.cache/agent-repl/bin"
STORE_SOCK="$HOME/.cache/agent-repl/sock/store.sock"
# The store's launchd StandardErrorPath (launchd/com.agentrepl.shim-store.plist).
# It is the only view this script has of a boot in progress.
STORE_LOG="$HOME/.cache/agent-repl/log/shim-store.err.log"
STORE_LABEL="com.agentrepl.shim-store"
SIDECAR_LABEL="com.agentrepl.shim-claude-sidecar"
SOCK_STALL="${AGENT_REPL_STORE_SOCK_TIMEOUT:-15}"
SOCK_MAX="${AGENT_REPL_STORE_SOCK_MAX:-180}"
READINESS_REPORT="$THIS_DIR/readiness-report.sh"

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

# The control-plane files step 5 loads into the running Emacs, in load order.
# `load` here is NOT no-error: a name that is not on disk signals, and the
# deploy fails. This list is the one place the names live, so the preload form
# and the pre-flight below can never name different files — for a while they
# did not have to: the form still loaded lisp/frontend-client.el after the
# module was deleted, and every deploy died on it AFTER both services had been
# kickstarted.
PRELOAD_FILES=(lisp/daemon.el lisp/services.el)

# FAIL ON A BROKEN PRELOAD BEFORE ANYTHING MOVES. A preload naming a file this
# checkout does not have cannot be recovered from later in the run: by the time
# step 5 discovers it, the store and the sidecar have already been bounced and
# the deploy exits with the stack half-deployed. So the names are proved first,
# while the only cost of being wrong is an early exit.
verify_preload_files() {
    local missing=() rel
    for rel in "${PRELOAD_FILES[@]}"; do
        [ -f "$ROOT/$rel" ] || missing+=("$rel")
    done
    if [ "${#missing[@]}" -gt 0 ]; then
        echo "[deploy-all] the runtime control-plane preload names ${#missing[@]} file(s) this checkout does not have: ${missing[*]}" >&2
        echo "[deploy-all] refusing to deploy: nothing was built, no service was kickstarted, and the runtime was not bounced" >&2
        exit 3
    fi
}

# A bounce-less run never reaches the preload, so it is not held to it.
if [ "$NO_BOUNCE" -eq 0 ] && [ "$NO_DAEMON_BOUNCE" -eq 0 ]; then
    verify_preload_files
fi

# The build stamp is the only deployment identity for a webview artifact, so
# this is asserted after everything is built and BEFORE any service or the
# daemon is bounced — see step 4b. Nothing is running the new image yet when it
# fails, which is what makes the failure recoverable.
verify_webapp_revision() {
    local report
    if ! report="$("$READINESS_REPORT" --require-ready webapp)"; then
        echo "[deploy-all] webapp revision gate failed; structured readiness report follows:" >&2
        printf '%s\n' "$report" >&2
        echo "[deploy-all] refusing to bounce: no service was kickstarted and the runtime was not restarted" >&2
        exit 3
    fi
    log "webapp: revision gate passed"
}

# ---- 1. protobufs ----------------------------------------------------------
log "proto: regenerating (make all)..."
make -C "$ROOT/proto" all
log "proto: done"

# ---- 2. build-frontend (shim bundle, webapp, daemon) -----------------------
# Whether the SHIM BUNDLE moved is REPORTED, not acted on: it tells the reader
# of this log whether surviving shims are about to be bounced onto a new bundle
# by the incoming daemon's rollout-controller staleness check. This script
# never stops a shim itself. Two signals, because neither alone is sufficient:
#
#   - the built-sha stamp, which moves whenever the source revision does; and
#   - the bundle's own content fingerprint, which is the only signal that moves
#     within one revision — the common case of a DIRTY tree, where the stamp
#     reads "<sha>-dirty" both before and after a rebuild that changed the code.
#
# Either differing means a surviving shim is now running superseded code.
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
    log "shim: bundle moved since the last deploy — surviving shims keep running until the new daemon rolls each one at its own turn boundary"
else
    log "shim: bundle unchanged — surviving shims need no refresh"
fi

# ---- 3. daemon, forced (staleness cannot see proto regen) ------------------
log "daemon: forced rebuild..."
mkdir -p "$ROOT/daemon/bin"
( cd "$ROOT/daemon" && go build -o "$ROOT/daemon/bin/claude-repld" ./cmd/claude-repld )
write_built_sha "$ROOT/daemon/bin/.built-sha" "$ROOT"
stamp_built_tree daemon "$ROOT/daemon/bin/.source-tree"
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
    stamp_built_tree "$name" "$CACHE_BIN/.$name.source-tree"
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

# ---- 4b. the revision gate, BEFORE anything is bounced ---------------------
# Everything that will be deployed has now been built, and nothing has been
# restarted yet. That is the only safe moment to assert it: a stale artifact
# must never be bounced INTO. The gate used to run at the very end, after the
# services were kickstarted and the daemon restarted, so a failing gate reported
# the problem from a stack that was already running the stale build.
verify_webapp_revision

STORE_STALE=0
SIDECAR_STALE=0
if needs_bounce shim-store; then STORE_STALE=1; fi
if needs_bounce shim-claude-sidecar; then SIDECAR_STALE=1; fi

if [ "$NO_BOUNCE" -eq 1 ]; then
    log "--no-bounce: skipping kickstarts, daemon restart, and elisp reload"
    exit 0
fi

kickstart() { launchctl kickstart -k "gui/$(id -u)/$1"; }

# The store's pid as launchd reports it, or empty when the service is not
# running. A flat deadline could not tell "still booting" from "died on boot",
# and answered both the same way; this is the difference.
store_service_pid() {
    launchctl print "gui/$(id -u)/$STORE_LABEL" 2>/dev/null |
        awk '/^[[:space:]]*pid = /{ gsub(/[^0-9]/, "", $3); print $3; exit }'
}

# How much the store has written. A boot that is still emitting records is
# WORKING, not wedged, and every byte resets the stall budget.
store_log_size() {
    if [ -f "$STORE_LOG" ]; then wc -c < "$STORE_LOG" | tr -d ' '; else echo 0; fi
}

store_log_tail() {
    if [ -f "$STORE_LOG" ]; then tail -n 5 "$STORE_LOG"; else echo "(no $STORE_LOG)"; fi
}

# WAIT ON THE SERVICE, NOT ON A STOPWATCH.
#
# This wait was a flat 15s, and on 2026-09-09 that cost a deploy: the incoming
# store found an 11.5 GB events.db at a superseded schema version and began
# emptying it table by table, minutes of work with the socket absent. The wait
# expired, the script exited, and the sidecar and the runtime were left on the
# old build with no signal beyond one timeout line. (The store no longer empties
# anything — it unlinks the file — but a boot can still be slower than any
# constant somebody guessed, and the deploy must not answer that by walking off
# half-done.)
#
# So the wait continues while the service is ALIVE and its log is ADVANCING, and
# it ends on one of three terminal answers, all of them loud, none of them
# continuing to the sidecar:
#   - launchd reports no pid: the store died on boot,
#   - the log has not grown for SOCK_STALL seconds: it is wedged,
#   - SOCK_MAX seconds have passed: it is beyond the stated upper bound.
wait_for_store_sock() {
    local waited=0 progress=0 size last_size announced=0 pid
    # WHAT THIS BOOT WROTE, NOT WHAT THE FILE HOLDS. launchd appends to the same
    # stderr log across every boot, so a nuke recorded weeks ago is still in
    # there; only the bytes written after this kickstart say anything about the
    # boot being waited on.
    local start_size
    start_size="$(store_log_size)"
    last_size="$start_size"
    while [ ! -S "$STORE_SOCK" ]; do
        pid="$(store_service_pid)"
        if [ -z "$pid" ]; then
            echo "[deploy-all] the store died before $STORE_SOCK appeared (${waited}s after kickstart; launchd reports no pid for $STORE_LABEL). The sidecar was NOT kickstarted and the runtime was NOT bounced." >&2
            echo "[deploy-all] the store's last words:" >&2
            store_log_tail >&2
            exit 1
        fi
        size="$(store_log_size)"
        if [ "$size" != "$last_size" ]; then
            last_size="$size"
            progress="$waited"
        fi
        # A nuke is the one slow boot with a known cause, so it is named rather
        # than left to look like a hang.
        if [ "$announced" -eq 0 ] && [ -f "$STORE_LOG" ] &&
           tail -c "+$((start_size + 1))" "$STORE_LOG" | grep -q "nuked, never migrated"; then
            announced=1
            log "store: the on-disk schema was superseded and the database is being replaced (the store is nuked, never migrated) — waiting up to ${SOCK_MAX}s for $STORE_SOCK"
        fi
        # LOOK AGAIN BEFORE CALLING IT A FAILURE. launchd binds the socket
        # whenever the store gets there, which can be while this iteration was
        # reading the pid and the log; declaring a timeout on a socket that
        # already exists would fail a deploy that had in fact succeeded.
        if [ -S "$STORE_SOCK" ]; then
            break
        fi
        if [ $((waited - progress)) -ge "$SOCK_STALL" ]; then
            echo "[deploy-all] $STORE_SOCK did not appear and the store (pid $pid) has written nothing for ${SOCK_STALL}s, so it is wedged rather than working. The sidecar was NOT kickstarted and the runtime was NOT bounced." >&2
            echo "[deploy-all] the store's last words:" >&2
            store_log_tail >&2
            exit 1
        fi
        if [ "$waited" -ge "$SOCK_MAX" ]; then
            echo "[deploy-all] $STORE_SOCK did not appear within the ${SOCK_MAX}s upper bound, though the store (pid $pid) is alive and still writing. The sidecar was NOT kickstarted and the runtime was NOT bounced." >&2
            echo "[deploy-all] the store's last words:" >&2
            store_log_tail >&2
            exit 1
        fi
        sleep 1
        waited=$((waited + 1))
    done
    if [ "$waited" -gt 0 ]; then
        log "store: socket appeared ${waited}s after kickstart"
    fi
}

if [ "$STORE_STALE" -eq 1 ]; then
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
else
    log "store: unchanged, kickstart skipped"
fi

if [ "$SIDECAR_STALE" -eq 1 ]; then
    log "sidecar: kickstarting $SIDECAR_LABEL..."
    kickstart "$SIDECAR_LABEL"
    record_deployed shim-claude-sidecar
    log "sidecar: done"
else
    log "sidecar: unchanged, kickstart skipped"
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

    # The daemon, shim, and webapp paths are derived by the runtime control
    # plane from the checkout that loaded it. A deploy invoked from a linked
    # worktree must therefore load THIS checkout's control plane BEFORE asking
    # the running Emacs to restart the runtime. Loading it after the restart
    # would successfully build one checkout and then silently launch another
    # checkout's artifacts.
    #
    # Encode ROOT rather than interpolating it into an elisp string literal:
    # valid filesystem paths may contain quotes or backslashes. The returned
    # sentinel also reports whether the runtime artifact root moved; a moved
    # root necessarily means surviving shims execute a different bundle even
    # when this checkout's own before/after fingerprint is unchanged, so it
    # folds into the same REPORTED shim-changed signal (this script stops
    # nothing — the incoming daemon bounces each stale shim itself).
    ROOT_B64="$(printf '%s' "$ROOT" | base64 | tr -d '\n')"
    PRELOAD_LOADS=""
    for rel in "${PRELOAD_FILES[@]}"; do
        PRELOAD_LOADS="$PRELOAD_LOADS (load (expand-file-name \"$rel\" root) nil t)"
    done
    PRELOAD_FORM="(let* ((root (file-name-as-directory (decode-coding-string (base64-decode-string \"$ROOT_B64\") 'utf-8))) (before (and (boundp 'agent-repl--frontend-root) agent-repl--frontend-root)))$PRELOAD_LOADS (unless (equal agent-repl--frontend-root root) (error \"agent-repl deploy root mismatch: expected %S got %S\" root agent-repl--frontend-root)) (if (equal before root) \"artifact-root-same\" \"artifact-root-changed\"))"
    PRELOAD_OUT="$("$EMACSCLIENT" --eval "$PRELOAD_FORM" 2>&1)" || {
        echo "[deploy-all] daemon control-plane preload failed: $PRELOAD_OUT" >&2
        exit 3
    }
    case "$PRELOAD_OUT" in
        *artifact-root-same*)
            log "daemon: control plane loaded from $ROOT (artifact root unchanged)"
            ;;
        *artifact-root-changed*)
            SHIM_CHANGED=1
            log "daemon: control plane loaded from $ROOT (artifact root changed — surviving shims will be rolled at their turn boundaries)"
            ;;
        *)
            echo "[deploy-all] daemon control-plane preload returned an unrecognized result: $PRELOAD_OUT" >&2
            exit 3
            ;;
    esac

    # The await form takes no argument: what happens to surviving shims is the
    # incoming daemon's decision, never a flag this script passes.
    RESTART_FORM='(agent-repl-runtime-restart-await)'
    if [ "$SHIM_CHANGED" -eq 1 ]; then
        log "daemon: restarting and awaiting completion via emacsclient (the incoming daemon bounces each stale shim itself)..."
    else
        log "daemon: restarting and awaiting completion via emacsclient..."
    fi
    RESTART_OUT="$("$EMACSCLIENT" --eval "$RESTART_FORM" 2>&1)" || {
        echo "[deploy-all] daemon restart failed: $RESTART_OUT" >&2
        exit 3
    }
    case "$RESTART_OUT" in
        *refusing*)
            # emacsclient exits 0 even when the elisp signals; the refusal text
            # is the only tell. A refused bounce means the deploy is NOT
            # complete, so the refusal is surfaced verbatim rather than
            # interpreted here.
            echo "[deploy-all] daemon restart refused: $RESTART_OUT" >&2
            exit 3
            ;;
        *runtime-restart-complete*)
            ;;
        *)
            echo "[deploy-all] daemon restart returned no terminal completion: $RESTART_OUT" >&2
            exit 3
            ;;
    esac
    log "daemon: restart completed"

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
# config.el stays at the module ROOT (that is where Doom's loader resolves
# it), while every source it names lives in `lisp/'.
canonical_module_files() {
    local m
    sed -n 's/^(agent-repl--load-module "\([^"]*\)").*/\1/p' "$ROOT/config.el" \
    | while IFS= read -r m; do
        [ -f "$ROOT/lisp/$m.el" ] || continue
        printf '%s\n' "$MOD_REL/lisp/$m.el"
    done
}

if [ -n "$ELISP_RANGE" ] && [ "$EMACS_AVAILABLE" -eq 1 ]; then
    log "elisp: reloading non-test .el changed in $ELISP_RANGE..."

    CHANGED=()
    CORE_IN_SET=0
    # Two pathspecs: the sources live in `$MOD_REL/lisp/', while config.el /
    # packages.el / doctor.el stay at `$MOD_REL/' where Doom's module loader
    # resolves them.
    while IFS= read -r rel; do
        base="$(basename "$rel")"
        case "$base" in test-*.el) continue ;; esac   # batch-only harness files
        [ -f "$REPO_ROOT/$rel" ] || continue          # deleted in range
        [ "$base" = "core.el" ] && CORE_IN_SET=1
        CHANGED+=("$rel")
    done < <(git -C "$REPO_ROOT" diff --name-only "$ELISP_RANGE" \
                 -- "$MOD_REL/*.el" "$MOD_REL/lisp/*.el")

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
