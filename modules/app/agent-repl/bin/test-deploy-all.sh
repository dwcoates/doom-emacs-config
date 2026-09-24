#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
# test-deploy-all.sh — hermetic tests for deploy-all.sh sequencing logic.
#
# Builds a throwaway project tree around a copy of deploy-all.sh and stubs
# `make`, `go`, `launchctl`, `git`, `emacsclient`, and build-frontend.sh so no
# real toolchain, launchd, or Emacs is touched. Each stub appends what it was
# asked to do to an invocation log; tests assert WHICH steps ran, in which
# order, under each scenario.
#
# Run with:   bash bin/test-deploy-all.sh

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/deploy-all.sh"

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -n "${2:-}" ] && echo "       $2"; }

# --- build a fake project tree in a temp dir -------------------------------
# Layout mirrors what deploy-all.sh expects relative to its own location:
#   <root>/modules/app/agent-repl/bin/deploy-all.sh (+ stub build-frontend.sh)
#   <root>/modules/app/agent-repl/{proto,daemon,agent-shim/...}
make_tree() {
    local root="$1"
    local mod="$root/modules/app/agent-repl"
    mkdir -p "$mod/bin" "$mod/lisp" "$mod/proto" "$mod/daemon/cmd/claude-repld" \
             "$mod/agent-shim/shim-store" "$mod/agent-shim/claude/shim-sidecar" \
             "$mod/agent-shim/claude/shim/dist"
    cp "$SCRIPT_UNDER_TEST" "$mod/bin/deploy-all.sh"
    cp "$THIS_DIR/lib-deploy-stamp.sh" "$mod/bin/lib-deploy-stamp.sh"
    cat > "$mod/bin/readiness-report.sh" <<'EOF'
#!/usr/bin/env bash
echo "readiness-report $*" >> "$STUB_LOG"
if [ "${READINESS_GATE_FAIL:-0}" = "1" ]; then
    printf '%s\n' '{"gate":{"system":"webapp","ready":false,"deployed_sha":"deployed-revision","source_sha":"source-revision","error":"required system is not ready"}}'
    exit 3
fi
if [ "${READINESS_DAEMON_GATE_FAIL:-0}" = "1" ] && [ "${2:-}" = "daemon" ]; then
    printf '%s\n' '{"systems":[{"name":"daemon","deployed_sha":"new-deployed-revision","source_sha":"new-source-revision","running":{"pid":31984,"stale_binary":true},"ready":false}],"gate":{"system":"daemon","ready":false,"deployed_sha":"new-deployed-revision","source_sha":"new-source-revision","error":"required system is not ready"}}'
    exit 3
fi
printf '%s\n' "{\"gate\":{\"system\":\"${2:-unknown}\",\"ready\":true,\"deployed_sha\":\"source-revision\",\"source_sha\":\"source-revision\"}}"
EOF
    chmod +x "$mod/bin/readiness-report.sh"
    chmod +x "$mod/bin/deploy-all.sh"
    # Exactly the control-plane files deploy-all's PRELOAD_FILES names, and no
    # others: a stub for a module the checkout does not have is how the harness
    # kept passing while every real deploy died loading lisp/frontend-client.el.
    printf ';; stub wire codec\n' > "$mod/lisp/wire-verbs.el"
    printf ';; stub rpc verbs\n' > "$mod/lisp/rpc.el"
    printf ';; stub daemon control plane\n' > "$mod/lisp/daemon.el"
    printf ';; stub runtime coordinator\n' > "$mod/lisp/services.el"

    # BF_STUB_SHIM_CONTENT makes the stub behave like a real shim build: it
    # writes the bundle and its built-sha stamp, which is what deploy-all reads
    # to decide the bounce's stop-shims mode. Unset leaves both absent, so
    # every pre-existing case sees an unchanged (absent) bundle.
    cat > "$mod/bin/build-frontend.sh" <<'EOF'
#!/usr/bin/env bash
echo "build-frontend $*" >> "$STUB_LOG"
if [ -n "${BF_STUB_SHIM_CONTENT:-}" ]; then
    dist="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/agent-shim/claude/shim/dist"
    mkdir -p "$dist"
    printf '%s' "$BF_STUB_SHIM_CONTENT" > "$dist/main.js"
    printf '%s\n' "${BF_STUB_SHIM_SHA:-deadbeefcafe}" > "$dist/.built-sha"
fi
# BF_STUB_WEBAPP_CONTENT does the same for the webapp's entry point, which is
# what deploy-all fingerprints to decide whether open webviews must reload.
if [ -n "${BF_STUB_WEBAPP_CONTENT:-}" ]; then
    wdist="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/webapp/dist"
    mkdir -p "$wdist"
    printf '%s' "$BF_STUB_WEBAPP_CONTENT" > "$wdist/index.html"
    printf '%s\n' "deadbeefcafe" > "$wdist/.built-sha"
fi
EOF
    chmod +x "$mod/bin/build-frontend.sh"

    # .el files for the --elisp scenarios (test-foo.el and deleted.el are
    # deliberately absent from disk so they double as the skipped cases).
    #
    # config.el carries the canonical `agent-repl--load-module' order the
    # core.el expansion reads. `emoji' is listed but has no file on disk, so
    # the expansion is also exercised against a loader entry whose module is
    # missing.
    printf ';; stub\n' > "$mod/lisp/status.el"
    printf ';; stub\n' > "$mod/lisp/core.el"
    printf ';; stub\n' > "$mod/lisp/workspace.el"
    cat > "$mod/config.el" <<'EOF'
;; stub loader
(agent-repl--load-module "core")
(agent-repl--load-module "workspace")
(agent-repl--load-module "status")
(agent-repl--load-module "emoji")
EOF
}

# --- stub toolchain on PATH -------------------------------------------------
make_stubs() {
    local stubs="$1"
    mkdir -p "$stubs"

    cat > "$stubs/make" <<'EOF'
#!/usr/bin/env bash
echo "make $*" >> "$STUB_LOG"
EOF

    # `go build -o <target> .` writes $GO_STUB_CONTENT to the target so the
    # changed/unchanged decision is driven per-test by pre-installed content.
    cat > "$stubs/go" <<'EOF'
#!/usr/bin/env bash
echo "go $* (pwd=$(basename "$PWD"))" >> "$STUB_LOG"
target=""
prev=""
for a in "$@"; do
    [ "$prev" = "-o" ] && target="$a"
    prev="$a"
done
[ -n "$target" ] && printf '%s' "${GO_STUB_CONTENT:-bin-v1}" > "$target"
exit 0
EOF

    # kickstart of the store label creates the store socket (a real unix
    # socket, since deploy-all checks with -S) as the freshly-booted service
    # would, and `print` answers with a pid as launchd does.
    #
    # STORE_STUB_MODE selects which BOOT a case is about. The slow modes count
    # `print` calls rather than sleeping, so the socket's arrival is pinned to
    # the deploy's own polling and no stub outlives the case that started it:
    #   immediate (default) — the socket is there before the first poll
    #   late      — the socket appears on the STORE_STUB_LATE_POLLS'th poll
    #   dead      — no socket, and launchd reports no pid: it died on boot
    #   wedged    — alive, silent, no socket
    #   nuking    — alive, writing a nuke record on every poll, no socket
    #
    # The SIDECAR is modelled by a loaded-flag file, because the deploy stops
    # it with `bootout` (which removes the label from the user domain) and
    # brings it back with `bootstrap` (which puts the label back). `print`
    # exits non-zero for a label the domain does not hold — the only question a
    # bootout can be polled on. SIDECAR_STUB_STUCK=1 is a bootout that does not
    # take, so the poll's own upper bound is exercised.
    cat > "$stubs/launchctl" <<'EOF'
#!/usr/bin/env bash
echo "launchctl $*" >> "$STUB_LOG"
SOCK_DIR="$HOME/.cache/agent-repl/sock"
SOCK="$SOCK_DIR/store.sock"
LOG_DIR="$HOME/.cache/agent-repl/log"
STORE_LOG="$LOG_DIR/shim-store.err.log"
POLLS="$HOME/.store-stub-polls"

bind_socket() {
    mkdir -p "$SOCK_DIR"
    rm -f "$SOCK"
    python3 -c 'import socket, sys; socket.socket(socket.AF_UNIX).bind(sys.argv[1])' "$SOCK"
}

case "$*" in
    kickstart*com.agentrepl.shim-store*)
        mkdir -p "$SOCK_DIR" "$LOG_DIR"
        rm -f "$SOCK"
        printf '0' > "$POLLS"
        case "${STORE_STUB_MODE:-immediate}" in
            immediate) bind_socket ;;
        esac
        ;;
    bootout*com.agentrepl.shim-claude-sidecar*)
        [ "${SIDECAR_STUB_STUCK:-0}" = "1" ] || rm -f "$HOME/.sidecar-loaded"
        ;;
    bootstrap*com.agentrepl.shim-claude-sidecar*)
        : > "$HOME/.sidecar-loaded"
        ;;
    print*com.agentrepl.shim-claude-sidecar*)
        [ -f "$HOME/.sidecar-loaded" ] || exit 1
        printf 'com.agentrepl.shim-claude-sidecar = {\n\tpid = 4343\n\tstate = running\n}\n'
        ;;
    print*com.agentrepl.shim-store*)
        polls=$(( $(cat "$POLLS" 2>/dev/null || echo 0) + 1 ))
        printf '%s' "$polls" > "$POLLS"
        case "${STORE_STUB_MODE:-immediate}" in
            dead)
                printf 'com.agentrepl.shim-store = {\n\tstate = not running\n}\n'
                exit 0
                ;;
            late)
                if [ "$polls" -ge "${STORE_STUB_LATE_POLLS:-2}" ]; then bind_socket; fi
                ;;
            nuking)
                mkdir -p "$LOG_DIR"
                echo "on-disk schema does not match this binary — the store is nuked, never migrated ($polls)" >> "$STORE_LOG"
                ;;
        esac
        printf 'com.agentrepl.shim-store = {\n\tpid = 4242\n\tstate = running\n}\n'
        ;;
esac
EOF

    cat > "$stubs/emacsclient" <<'EOF'
#!/usr/bin/env bash
echo "emacsclient $*" >> "$STUB_LOG"
if [ "${EC_STUB_UNAVAILABLE:-0}" = "1" ]; then
    echo "emacsclient: can't find socket; have you started the server?" >&2
    exit 1
fi
if [ "${EC_STUB_PROBE_ERROR:-0}" = "1" ]; then
    case "$*" in
        *"--eval t"*)
            echo "emacsclient: permission denied" >&2
            exit 1
            ;;
    esac
fi
case "$*" in
    # THE RUNNING EDITOR HAS A PID, because "does the Emacs that will restart
    # the daemon carry the vendor guard" is a question about a process. The ps
    # stub answers it from EC_STUB_GUARDED.
    *"(emacs-pid)"*)
        printf '%s\n' "${EC_STUB_PID:-31337}"
        exit 0
        ;;
    *artifact-root-same*|*artifact-root-changed*)
        echo \"\"${EC_STUB_ARTIFACT_ROOT_RESULT:-artifact-root-same}\"\"
        exit 0
        ;;
    # WHICH CHECKOUT THE EDITOR LAUNCHES FROM, read before anything is loaded.
    # Unset by default (a first-ever load); EC_STUB_RUNNING_ROOT names another.
    *"(and (boundp 'agent-repl--frontend-root) agent-repl--frontend-root)")
        if [ -n "${EC_STUB_RUNNING_ROOT:-}" ]; then
            printf '"%s"\n' "$EC_STUB_RUNNING_ROOT"
        else
            echo nil
        fi
        exit 0
        ;;
    *runtime-rollout-await*)
        if [ "${EC_STUB_ROLLOUT_REFUSED:-0}" = "1" ]; then
            echo "*ERROR*: agent-repl: not rolled out: a rollout is already in flight, waiting on ws-busy"
            exit 1
        fi
        echo \"\"${EC_STUB_ROLLOUT_RESULT:-runtime-rollout-accepted action=handover workspaces=2 busy=0}\"\"
        exit 0
        ;;
    *runtime-restart-await*)
        if [ "${EC_STUB_NOT_RESTARTED:-0}" = "1" ]; then
            echo "*ERROR*: agent-repl: not restarted: no daemon link is available"
            exit 1
        fi
        if [ "${EC_STUB_REFUSE:-0}" = "1" ]; then
            echo "*ERROR*: agent-repl: refusing daemon stop — turn in flight"
            exit 1
        fi
        echo \"\"${EC_STUB_RESTART_RESULT:-runtime-restart-complete}\"\"
        exit 0
        ;;
    *assert-heartbeat-armed*)
        # The post-load timer-contract verification. EC_STUB_ASSERT selects
        # which shape the running Emacs answers with.
        case "${EC_STUB_ASSERT:-ok}" in
            absent) echo '"absent"' ;;
            rearmed) echo '"armed=3 rearmed=1 failed=0 unavailable=0"' ;;
            failed)  echo '"armed=3 rearmed=0 failed=1 unavailable=0"' ;;
            garbage) echo '"who knows"' ;;
            *)       echo '"armed=4 rearmed=0 failed=0 unavailable=0"' ;;
        esac
        exit 0
        ;;
esac
echo t
EOF

    cat > "$stubs/git" <<'EOF'
#!/usr/bin/env bash
echo "git $*" >> "$STUB_LOG"
case "$*" in
    # The index listing the source-tree id is hashed from. Empty (the
    # GIT_STUB_NO_LS_FILES case) stands for a pathspec that resolves to
    # nothing, which must never be stamped as a known revision.
    *"ls-files -s"*)
        if [ -z "${GIT_STUB_NO_LS_FILES:-}" ]; then
            printf '100644 %s 0\tsource.go\n' "${GIT_STUB_BLOB:-1111111111111111111111111111111111111111}"
        fi
        ;;
    *"rev-parse HEAD"*)     echo "${GIT_STUB_SHA:-deadbeefcafe}" ;;
    *"status --porcelain"*) printf '%s' "${GIT_STUB_DIRTY:-}" ;;
    *"diff --name-only"*)
        if [ -n "${GIT_STUB_DIFF_FILES:-}" ]; then
            # Comma-separated, because RUN_ENV is word-split by the runner and
            # a space-bearing value would be parsed as a command.
            printf '%s\n' ${GIT_STUB_DIFF_FILES//,/ }
        else
            printf '%s\n' \
                "modules/app/agent-repl/lisp/status.el" \
                "modules/app/agent-repl/lisp/test-foo.el" \
                "modules/app/agent-repl/lisp/deleted.el"
        fi
        ;;
esac
EOF

    # `ps` is how deploy-all reads the KERNEL's copy of the running Emacs's
    # environment. The default answer is an editor the owner started
    # themselves; EC_STUB_GUARDED=1 makes it one bin/realtest.sh is driving.
    cat > "$stubs/ps" <<'EOF'
#!/usr/bin/env bash
if [ "${EC_STUB_GUARDED:-0}" = "1" ]; then
    printf '/Applications/Emacs.app/Contents/MacOS/Emacs AGENT_REPL_FORBID_VENDOR_CALLS=1\n'
else
    printf '/Applications/Emacs.app/Contents/MacOS/Emacs\n'
fi
EOF

    chmod +x "$stubs"/make "$stubs"/go "$stubs"/launchctl "$stubs"/emacsclient "$stubs"/git "$stubs"/ps
}

# --- per-test runner --------------------------------------------------------
# run_deploy <case-dir> [deploy-all args...]; env overrides go via RUN_ENV.
run_deploy() {
    local dir="$1"; shift
    make_tree "$dir/tree"
    make_stubs "$dir/stubs"
    mkdir -p "$dir/h"
    # The installed launchd plists. The deploy boots the sidecar OUT for a
    # store restart and needs its plist to bootstrap it back, exactly as
    # store-reset.sh does; NO_SIDECAR_PLIST=1 is the case where it is missing.
    mkdir -p "$dir/h/Library/LaunchAgents"
    printf '<plist/>\n' > "$dir/h/Library/LaunchAgents/com.agentrepl.shim-store.plist"
    if [ "${NO_SIDECAR_PLIST:-0}" != "1" ]; then
        printf '<plist/>\n' > "$dir/h/Library/LaunchAgents/com.agentrepl.shim-claude-sidecar.plist"
    fi
    # launchd holds the sidecar label at the start of every case.
    : > "$dir/h/.sidecar-loaded"
    # PRE_RUN runs against the built tree, for a case whose subject is a tree
    # that is WRONG — a control-plane file the checkout does not have.
    if [ -n "${PRE_RUN:-}" ]; then ( cd "$dir/tree" && eval "$PRE_RUN" ); fi
    STUB_LOG="$dir/log"
    : > "$STUB_LOG"
    set +e
    # RUN_ENV is a space-separated list of NAME=VALUE assignments and is
    # deliberately word-split into `env`'s arguments; quoting it would pass the
    # whole list as one assignment.
    # shellcheck disable=SC2086
    env PATH="$dir/stubs:/usr/bin:/bin" HOME="$dir/h" STUB_LOG="$STUB_LOG" \
        AGENT_REPL_STORE_SOCK_TIMEOUT=2 AGENT_REPL_HANDOVER_MAX=2 AGENT_REPL_EMACSCLIENT=emacsclient \
        ${RUN_ENV:-} \
        bash "$dir/tree/modules/app/agent-repl/bin/deploy-all.sh" "$@" \
        > "$dir/stdout" 2> "$dir/stderr"
    RC=$?
    set -e
}

log_has()    { grep -q "$1" "$STUB_LOG"; }
log_line()   { grep -n "$1" "$STUB_LOG" | head -1 | cut -d: -f1; }
log_last()   { grep -n "$1" "$STUB_LOG" | tail -1 | cut -d: -f1; }
# assert the LAST occurrence of $1 still precedes $2 — "every one of these came
# before that one", which a first-match comparison cannot say.
log_all_before() {
    local a b
    a="$(log_last "$1")" && b="$(log_line "$2")" && [ -n "$a" ] && [ -n "$b" ] && [ "$a" -lt "$b" ]
}
log_before() { # assert pattern $1 appears before pattern $2
    local a b
    a="$(log_line "$1")" && b="$(log_line "$2")" && [ -n "$a" ] && [ -n "$b" ] && [ "$a" -lt "$b" ]
}

TMP="$(mktemp -d "${TMPDIR:-/tmp}/da.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# --- 1. full run from scratch: everything builds, everything bounces --------
d="$TMP/t1"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && log_before "make -C .*proto all" "build-frontend" \
   && log_before "build-frontend" "go build -o .*claude-repld" \
   && log_before "go build -o .*claude-repld" "pwd=shim-store" \
   && log_before "kickstart -k gui/.*shim-store" "bootstrap gui/.*shim-claude-sidecar" \
   && log_before "load .*rpc.el" "runtime-rollout-await" \
   && log_before "load .*daemon.el" "runtime-rollout-await" \
   && log_before "load .*services.el" "runtime-rollout-await" \
   && ! log_has "runtime-restart" \
   && ! log_has "frontend-client.el" \
   && log_before "bootstrap gui/.*shim-claude-sidecar" "runtime-rollout-await" \
   && log_has "readiness-report --require-ready webapp" \
   && log_has "readiness-report --require-ready daemon" \
   && log_before "readiness-report --require-ready webapp" "kickstart -k gui/.*shim-store" \
   && log_before "readiness-report --require-ready webapp" "runtime-rollout-await" \
   && log_before "runtime-rollout-await" "readiness-report --require-ready daemon"; then
    pass "fresh tree runs the full chain in dependency order"
else
    fail "fresh tree runs the full chain in dependency order" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 1c. a revision mismatch aborts with the structured report -------------
d="$TMP/t1c"; mkdir -p "$d"
RUN_ENV="READINESS_GATE_FAIL=1" run_deploy "$d"
if [ "$RC" -eq 3 ] \
   && grep -q "webapp revision gate failed" "$d/stderr" \
   && grep -q '"deployed_sha":"deployed-revision"' "$d/stderr" \
   && grep -q '"source_sha":"source-revision"' "$d/stderr"; then
    pass "a webapp revision mismatch aborts deployment with both revisions in structured output"
else
    fail "a webapp revision mismatch aborts deployment with both revisions in structured output" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 1f. deploy-all stamps the source tree of everything it builds itself ---
# It builds the daemon and both services outside build-frontend.sh, and the
# gate two steps later reads exactly these stamps. Writing only the built-sha
# would leave those artifacts looking un-built to the very gate below.
d="$TMP/t1f"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d"
DAEMON_TREE="$d/tree/modules/app/agent-repl/daemon/bin/.source-tree"
STORE_TREE="$d/h/.cache/agent-repl/bin/.shim-store.source-tree"
if [ "$RC" -eq 0 ] && [ -s "$DAEMON_TREE" ] && [ -s "$STORE_TREE" ]; then
    pass "the daemon and the services it builds itself get .source-tree stamps"
else
    fail "the daemon and the services it builds itself get .source-tree stamps" \
         "rc=$RC daemon=$(cat "$DAEMON_TREE" 2>/dev/null) store=$(cat "$STORE_TREE" 2>/dev/null)"
fi

# --- 1g. an undeterminable source set leaves no stamp, never a stale one ----
# A stamp that outlives the artifact it described is worse than none: the next
# build compares against it and skips work it owes.
d="$TMP/t1g"; mkdir -p "$d"
PRE_RUN='mkdir -p modules/app/agent-repl/daemon/bin && printf "an-old-id\n" > modules/app/agent-repl/daemon/bin/.source-tree' \
    RUN_ENV="GIT_STUB_NO_LS_FILES=1" run_deploy "$d"
if [ ! -e "$d/tree/modules/app/agent-repl/daemon/bin/.source-tree" ]; then
    pass "a source set that cannot be determined drops the stamp rather than leaving a guess"
else
    fail "a source set that cannot be determined drops the stamp rather than leaving a guess" \
         "stamp=$(cat "$d/tree/modules/app/agent-repl/daemon/bin/.source-tree")"
fi

# --- 1d. a failing gate bounces NOTHING ------------------------------------
# The gate used to run last, after both services were kickstarted and the daemon
# restarted, so a stale artifact was already live by the time anyone was told
# about it. A stale artifact must never be bounced into.
d="$TMP/t1d"; mkdir -p "$d"
RUN_ENV="READINESS_GATE_FAIL=1" run_deploy "$d"
if [ "$RC" -eq 3 ] \
   && ! log_has "kickstart" \
   && ! log_has "runtime-restart" \
   && grep -q "refusing to bounce" "$d/stderr"; then
    pass "a failing revision gate kickstarts no service and restarts no daemon"
else
    fail "a failing revision gate kickstarts no service and restarts no daemon" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 1e. the gate is asserted even in pure-build mode ----------------------
# --no-bounce still BUILDS every artifact, and an artifact built wrong is worth
# knowing about at the moment it is built rather than at the next real deploy.
d="$TMP/t1e"; mkdir -p "$d"
RUN_ENV="READINESS_GATE_FAIL=1" run_deploy "$d" --no-bounce
if [ "$RC" -eq 3 ] && ! log_has "kickstart"; then
    pass "--no-bounce still asserts the revision gate"
else
    fail "--no-bounce still asserts the revision gate" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 1b. a deploy from another checkout is REFUSED, loading nothing ---------
# A handover spawns the successor from the RUNNING daemon's own binary, so a
# rollout from a checkout the editor does not launch from would bring the other
# checkout's build back up. It is refused BEFORE the control plane is loaded,
# because the load is what rebinds the editor's root.
d="$TMP/r1"; mkdir -p "$d"
RUN_ENV="EC_STUB_RUNNING_ROOT=/somewhere/else/agent-repl/" run_deploy "$d"
if [ "$RC" -eq 3 ] \
   && grep -q "REFUSING to roll out" "$d/stderr" \
   && grep -q "/somewhere/else/agent-repl" "$d/stderr" \
   && ! log_has "load .*daemon.el" \
   && ! log_has "runtime-r[eo]"; then
    pass "a rollout from a checkout the editor does not launch from is refused before anything is loaded"
else
    fail "a rollout from a checkout the editor does not launch from is refused before anything is loaded" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 1bb. --restart moves the runtime to this checkout, root bound first ----
d="$TMP/r2"; mkdir -p "$d"
RUN_ENV="EC_STUB_RUNNING_ROOT=/somewhere/else/agent-repl/ EC_STUB_ARTIFACT_ROOT_RESULT=artifact-root-changed" run_deploy "$d" --restart
if [ "$RC" -eq 0 ] \
   && log_before "load .*daemon.el" "runtime-restart-await)" \
   && ! log_has "runtime-restart-await t" \
   && ! log_has "runtime-rollout-await" \
   && grep -q "artifact root changed" "$d/stdout"; then
    pass "a moved runtime artifact root is bound before the forced restart"
else
    fail "a moved runtime artifact root is bound before the forced restart" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(cat "$STUB_LOG")"
fi

# --- 1c. the default deploy NEVER forces a restart --------------------------
d="$TMP/r3"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] && log_has "runtime-rollout-await" && ! log_has "runtime-restart"; then
    pass "a plain deploy rolls out and never restarts the daemon"
else
    fail "a plain deploy rolls out and never restarts the daemon" "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 1d. the rollout names what was rebuilt ---------------------------------
# A fresh tree has never rolled a daemon out (no stamp) and its stub build
# writes a shim bundle and a webapp entry that were not there before.
d="$TMP/r4"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] && log_has "runtime-rollout-await '( :daemon t"; then
    pass "a daemon binary that was never rolled out is named in the rollout"
else
    fail "a daemon binary that was never rolled out is named in the rollout" "rc=$RC log: $(grep rollout "$STUB_LOG")"
fi

# --- 1e. an accepted handover stamps the binary it rolled out ---------------
d="$TMP/r5"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d"
STAMP="$d/tree/modules/app/agent-repl/daemon/bin/.rolled-out-fingerprint"
if [ "$RC" -eq 0 ] && [ -f "$STAMP" ] \
   && [ "$(cat "$STAMP")" = "$(shasum -a 256 "$d/tree/modules/app/agent-repl/daemon/bin/claude-repld" | cut -d' ' -f1)" ]; then
    pass "an accepted handover stamps the fingerprint of the binary it rolled out"
else
    fail "an accepted handover stamps the fingerprint of the binary it rolled out" "rc=$RC"
fi

# --- 1f. a busy handover skips the gate it cannot pass, and says so ---------
d="$TMP/r6"; mkdir -p "$d"
RUN_ENV="EC_STUB_ROLLOUT_RESULT=runtime-rollout-accepted_action=handover_workspaces=2_busy=1" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && ! log_has "readiness-report --require-ready daemon" \
   && grep -q "WAITING on busy workspaces" "$d/stdout"; then
    pass "a handover waiting on a busy workspace skips the daemon gate and completes the deploy"
else
    fail "a handover waiting on a busy workspace skips the daemon gate and completes the deploy" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(grep readiness "$STUB_LOG")"
fi

# --- 1g. a rollout already in flight fails the deploy loudly ----------------
d="$TMP/r7"; mkdir -p "$d"; RUN_ENV="EC_STUB_ROLLOUT_REFUSED=1" run_deploy "$d"
if [ "$RC" -eq 3 ] \
   && grep -q "not rolled out" "$d/stderr" \
   && grep -q "waiting on ws-busy" "$d/stderr" \
   && [ ! -f "$d/tree/modules/app/agent-repl/daemon/bin/.rolled-out-fingerprint" ]; then
    pass "a refused rollout exits 3 naming the holdout and stamps nothing"
else
    fail "a refused rollout exits 3 naming the holdout and stamps nothing" "rc=$RC stderr: $(cat "$d/stderr")"
fi

# Pre-seed an installed binary AND its deployed stamp, i.e. a service already
# running the binary that sits in the cache — the only state that may skip a
# kickstart.
seed_deployed() { # case-dir name content
    local bin="$2" content="$3" dir="$1/h/.cache/agent-repl/bin"
    mkdir -p "$dir"
    printf '%s' "$content" > "$dir/$bin"
    shasum -a 256 "$dir/$bin" | cut -d' ' -f1 > "$dir/.$bin.deployed"
}

# --- 2. services already running the installed binary: no kickstarts --------
d="$TMP/t2"
seed_deployed "$d" shim-store bin-v1
seed_deployed "$d" shim-claude-sidecar bin-v1
RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] && ! log_has "launchctl" && log_has "runtime-rollout-await"; then
    pass "services already on the installed binary skip both kickstarts"
else
    fail "services already on the installed binary skip both kickstarts" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 2b. one deploy kickstarts each service AT MOST ONCE --------------------
# A store restart tears down every live shim's producer connection, so a second
# bounce inside one deploy doubles that outage for nothing. Counting is the
# invariant: `log_has`/`log_before` are satisfied by a repeated kickstart.
log_count() { grep -c "$1" "$STUB_LOG"; }

d="$TMP/t2b"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && [ "$(log_count "kickstart -k gui/.*shim-store")" -eq 1 ] \
   && [ "$(log_count "bootstrap gui/.*shim-claude-sidecar")" -eq 1 ] \
   && [ "$(log_count "bootout gui/.*shim-claude-sidecar")" -eq 1 ]; then
    pass "a full deploy kickstarts each service exactly once"
else
    fail "a full deploy kickstarts each service exactly once" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 3. store changed: sidecar bounces too, store first ---------------------
d="$TMP/t3"
seed_deployed "$d" shim-store bin-v0
seed_deployed "$d" shim-claude-sidecar bin-v1
RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && log_before "kickstart -k gui/.*shim-store" "bootstrap gui/.*shim-claude-sidecar"; then
    pass "a store change cascades into a sidecar restart, store first"
else
    fail "a store change cascades into a sidecar restart, store first" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 3a. the store restart is taken with the sidecar already stopped -------
# The gap between the store's socket being unlinked and rebound is the whole
# defect: a sidecar writing across it logged dial failures, "cursor not
# advanced", failed cursor recovery and a suspended producer, all caused by the
# deploy. bin/store-reset.sh records the order that has none of that, and this
# is that order — sidecar out, store restarted, socket up, sidecar back.
d="$TMP/t3a"
seed_deployed "$d" shim-store bin-v0
seed_deployed "$d" shim-claude-sidecar bin-v1
RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && log_before "bootout gui/.*shim-claude-sidecar" "kickstart -k gui/.*shim-store" \
   && log_before "kickstart -k gui/.*shim-store" "bootstrap gui/.*shim-claude-sidecar" \
   && ! log_has "kickstart -k gui/.*shim-claude-sidecar"; then
    pass "a store restart boots the sidecar out first and bootstraps it back after"
else
    fail "a store restart boots the sidecar out first and bootstraps it back after" \
         "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 3b. the sidecar is stopped only after the store's socket is proved up --
# Bootstrapping it while store.sock is still absent is the same outage in a
# smaller window: the sidecar's first batch would dial a socket that is not
# there yet.
d="$TMP/t3b"; mkdir -p "$d"
RUN_ENV="STORE_STUB_MODE=late STORE_STUB_LATE_POLLS=3" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && grep -q "store: socket appeared" "$d/stdout" \
   && log_all_before "print gui/.*shim-store" "bootstrap gui/.*shim-claude-sidecar"; then
    pass "the sidecar is bootstrapped only after the store's socket wait has returned"
else
    fail "the sidecar is bootstrapped only after the store's socket wait has returned" \
         "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 3c. a store that never comes up leaves the sidecar stopped, and says so -
d="$TMP/t3c"; mkdir -p "$d"
RUN_ENV="STORE_STUB_MODE=dead" run_deploy "$d"
if [ "$RC" -eq 1 ] \
   && log_has "bootout gui/.*shim-claude-sidecar" \
   && ! log_has "bootstrap gui/.*shim-claude-sidecar" \
   && grep -q "The sidecar was stopped for this restart and has NOT been started again" "$d/stderr"; then
    pass "a store that fails to come up leaves the sidecar stopped and names that in the failure"
else
    fail "a store that fails to come up leaves the sidecar stopped and names that in the failure" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 3d. a missing sidecar plist refuses before anything is stopped --------
# A bootout with no plist to bootstrap back from would leave the host with no
# sidecar and no way for this script to return one.
d="$TMP/t3d"; mkdir -p "$d"
NO_SIDECAR_PLIST=1 RUN_ENV="" run_deploy "$d"
NO_SIDECAR_PLIST=0
if [ "$RC" -eq 1 ] \
   && grep -q "com.agentrepl.shim-claude-sidecar.plist is missing" "$d/stderr" \
   && ! log_has "bootout" \
   && ! log_has "kickstart" \
   && ! log_has "runtime-restart"; then
    pass "a missing sidecar plist refuses the deploy before the sidecar is stopped"
else
    fail "a missing sidecar plist refuses the deploy before the sidecar is stopped" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 3e. a bootout that does not take stops the deploy before the store ----
d="$TMP/t3e"; mkdir -p "$d"
RUN_ENV="SIDECAR_STUB_STUCK=1 AGENT_REPL_STORE_SOCK_MAX=2" run_deploy "$d"
if [ "$RC" -eq 1 ] \
   && grep -q "did not leave the user domain within the 2s upper bound" "$d/stderr" \
   && ! log_has "kickstart -k gui/.*shim-store" \
   && ! log_has "runtime-restart"; then
    pass "a sidecar that will not boot out stops the deploy before the store is kickstarted"
else
    fail "a sidecar that will not boot out stops the deploy before the store is kickstarted" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 4. sidecar changed alone: store untouched ------------------------------
d="$TMP/t4"
seed_deployed "$d" shim-store bin-v1
seed_deployed "$d" shim-claude-sidecar bin-v0
RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && ! log_has "kickstart -k gui/.*shim-store" \
   && ! log_has "bootout" \
   && log_has "kickstart -k gui/.*shim-claude-sidecar"; then
    pass "a sidecar-only change leaves the store un-bounced"
else
    fail "a sidecar-only change leaves the store un-bounced" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 4b. installed-but-never-deployed binary still bounces ------------------
# The regression a live deploy hit: a prior --no-bounce run installed the new
# binaries without restarting anything, so this run's build is "unchanged"
# while the running processes are still on the old image. Deciding on the
# build delta skipped the bounce silently; deciding on the deployed stamp
# cannot.
d="$TMP/t4b"; mkdir -p "$d/h/.cache/agent-repl/bin"
printf 'bin-v1' > "$d/h/.cache/agent-repl/bin/shim-store"
printf 'bin-v1' > "$d/h/.cache/agent-repl/bin/shim-claude-sidecar"
RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && log_has "kickstart -k gui/.*shim-store" \
   && log_has "bootstrap gui/.*shim-claude-sidecar"; then
    pass "an installed but never-kickstarted binary bounces despite an unchanged build"
else
    fail "an installed but never-kickstarted binary bounces despite an unchanged build" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 4c. --no-bounce leaves the stamps alone so the next run bounces --------
d="$TMP/t4c"
seed_deployed "$d" shim-store bin-v0
seed_deployed "$d" shim-claude-sidecar bin-v0
RUN_ENV="" run_deploy "$d" --no-bounce
if [ "$RC" -eq 0 ] \
   && [ "$(cat "$d/h/.cache/agent-repl/bin/.shim-store.deployed")" \
        != "$(shasum -a 256 "$d/h/.cache/agent-repl/bin/shim-store" | cut -d' ' -f1)" ]; then
    pass "--no-bounce installs without stamping, leaving the service marked stale"
else
    fail "--no-bounce installs without stamping, leaving the service marked stale" "rc=$RC"
fi

# --- 5. --no-bounce: builds only --------------------------------------------
d="$TMP/t5"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d" --no-bounce
if [ "$RC" -eq 0 ] && ! log_has "launchctl" && ! log_has "emacsclient"; then
    pass "--no-bounce builds everything and bounces nothing"
else
    fail "--no-bounce builds everything and bounces nothing" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 5b. --no-daemon-bounce: services deploy, the daemon is left alone ------
# Emacs\'s lazy boot path runs this mode: step 5 bounces the daemon by calling
# BACK into Emacs over emacsclient, so the caller that IS Emacs must not reach
# it -- and starts the daemon itself immediately after.
d="$TMP/t5b"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d" --no-daemon-bounce
if [ "$RC" -eq 0 ] && log_has "launchctl" && ! log_has "emacsclient"; then
    pass "--no-daemon-bounce kickstarts the services and never touches Emacs"
else
    fail "--no-daemon-bounce kickstarts the services and never touches Emacs" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 5c. --no-daemon-bounce still records the deployed stamps ---------------
# Unlike --no-bounce, this mode DID kickstart, so the stamps must move or the
# next run would bounce services that are already running the installed build.
d="$TMP/t5c"
seed_deployed "$d" shim-store bin-v0
seed_deployed "$d" shim-claude-sidecar bin-v0
RUN_ENV="" run_deploy "$d" --no-daemon-bounce
if [ "$RC" -eq 0 ] \
   && [ "$(cat "$d/h/.cache/agent-repl/bin/.shim-store.deployed")" \
        = "$(shasum -a 256 "$d/h/.cache/agent-repl/bin/shim-store" | cut -d' ' -f1)" ]; then
    pass "--no-daemon-bounce stamps the services it kickstarted"
else
    fail "--no-daemon-bounce stamps the services it kickstarted" "rc=$RC"
fi

# --- 6. refused daemon restart fails the deploy loudly ----------------------
d="$TMP/t6"; mkdir -p "$d"; RUN_ENV="EC_STUB_REFUSE=1" run_deploy "$d" --restart
if [ "$RC" -eq 3 ] && grep -q "daemon restart" "$d/stderr"; then
    pass "a refused daemon restart exits 3 with the refusal surfaced"
else
    fail "a refused daemon restart exits 3 with the refusal surfaced" "rc=$RC stderr: $(cat "$d/stderr")"
fi

# --- 6b. pending dispatch is not terminal restart completion ---------------
d="$TMP/t6b"; mkdir -p "$d"; RUN_ENV="EC_STUB_RESTART_RESULT=runtime-restart-pending" run_deploy "$d" --restart
if [ "$RC" -eq 3 ] \
   && grep -q "no terminal completion" "$d/stderr" \
   && log_before "readiness-report" "runtime-restart"; then
    pass "the revision gate is settled before the restart that can fail on it"
else
    fail "the revision gate is settled before the restart that can fail on it" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 6bb. a reasoned non-restart is surfaced distinctly --------------------
d="$TMP/t6bb"; mkdir -p "$d"
RUN_ENV="EC_STUB_NOT_RESTARTED=1" run_deploy "$d" --restart
if [ "$RC" -eq 3 ] \
   && grep -q "daemon not restarted" "$d/stderr" \
   && grep -q "no daemon link is available" "$d/stderr"; then
    pass "a reasoned daemon non-restart is surfaced distinctly"
else
    fail "a reasoned daemon non-restart is surfaced distinctly" \
         "rc=$RC stderr: $(cat "$d/stderr")"
fi

# --- 6c. a stale daemon after the bounce fails with process and revisions ---
d="$TMP/t6c"; mkdir -p "$d"
RUN_ENV="READINESS_DAEMON_GATE_FAIL=1" run_deploy "$d"
if [ "$RC" -eq 3 ] \
   && grep -q "the successor is not serving the deployed build" "$d/stderr" \
   && grep -q '"pid":31984' "$d/stderr" \
   && grep -q '"deployed_sha":"new-deployed-revision"' "$d/stderr" \
   && grep -q '"source_sha":"new-source-revision"' "$d/stderr" \
   && ! grep -q "deploy complete" "$d/stdout"; then
    pass "a stale daemon after the bounce fails with its pid and both revision stamps"
else
    fail "a stale daemon after the bounce fails with its pid and both revision stamps" \
         "rc=$RC stdout: $(cat "$d/stdout") stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 7. --elisp loads changed non-test files only ---------------------------
d="$TMP/t7"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 0 ] \
   && log_has 'emacsclient --eval (load .*status.el' \
   && ! log_has 'test-foo.el' \
   && ! log_has 'deleted.el'; then
    pass "--elisp hot-loads changed .el, skipping test-*.el and deleted files"
else
    fail "--elisp hot-loads changed .el, skipping test-*.el and deleted files" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 7b. a plain deploy (no --elisp) hot-loads the full module set by default
# Owner ruling 2026-09-14: deploy live-reloads Emacs. With no range given, the
# deployed checkout is the source of truth, so the whole canonical module set is
# reloaded — the same heartbeat-safe set the core.el path expands to.
d="$TMP/t7b"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && grep -q "full module reload (default" "$d/stdout" \
   && log_has 'emacsclient --eval (load .*core.el' \
   && log_has 'emacsclient --eval (load .*workspace.el' \
   && log_has 'emacsclient --eval (load .*status.el' \
   && log_before 'load .*status.el' 'assert-heartbeat-armed'; then
    pass "a plain deploy hot-loads the full module set into the running Emacs"
else
    fail "a plain deploy hot-loads the full module set into the running Emacs" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(cat "$STUB_LOG")"
fi

# --- 8. --force propagates to build-frontend ---------------------------------
d="$TMP/t8"; mkdir -p "$d"
seed_deployed "$d" shim-store bin-v0
seed_deployed "$d" shim-claude-sidecar bin-v0
RUN_ENV="" run_deploy "$d" --force
if [ "$RC" -eq 0 ] && log_has "build-frontend --force"; then
    pass "--force propagates to build-frontend"
else
    fail "--force propagates to build-frontend" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 8b. --force does NOT kickstart services already on the installed image --
# A forced rebuild reproduces a byte-identical binary, so bouncing on the force
# alone dropped every live shim's store producer connection and standing
# subscription for nothing — a fleet-wide warn storm per deploy retry. The
# deployed-fingerprint stamp is the sole kickstart authority.
d="$TMP/t8b"
seed_deployed "$d" shim-store bin-v1
seed_deployed "$d" shim-claude-sidecar bin-v1
RUN_ENV="" run_deploy "$d" --force
if [ "$RC" -eq 0 ] \
   && ! log_has "kickstart -k gui/.*shim-store" \
   && ! log_has "kickstart -k gui/.*shim-claude-sidecar" \
   && grep -q "store: unchanged, kickstart skipped" "$d/stdout" \
   && grep -q "sidecar: unchanged, kickstart skipped" "$d/stdout"; then
    pass "--force leaves services already running the installed binary un-bounced"
else
    fail "--force leaves services already running the installed binary un-bounced" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(cat "$STUB_LOG")"
fi

# --- 8c. --force still bounces a service whose installed image genuinely moved
d="$TMP/t8c"
seed_deployed "$d" shim-store bin-v0
seed_deployed "$d" shim-claude-sidecar bin-v0
RUN_ENV="" run_deploy "$d" --force
if [ "$RC" -eq 0 ] \
   && log_has "kickstart -k gui/.*shim-store" \
   && log_has "bootstrap gui/.*shim-claude-sidecar"; then
    pass "--force still kickstarts a service that is not running the installed binary"
else
    fail "--force still kickstarts a service that is not running the installed binary" \
         "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 9. no Emacs server defers restart and elisp reload ---------------------
d="$TMP/t9"; mkdir -p "$d"; RUN_ENV="EC_STUB_UNAVAILABLE=1" run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 0 ] \
   && log_has 'emacsclient --eval t' \
   && ! log_has 'runtime-restart' \
   && ! log_has 'emacsclient --eval (load' \
   && grep -q "Emacs is not running; restart deferred until Emacs starts" "$d/stdout" \
   && ! grep -q "can't find socket" "$d/stdout" \
   && grep -q "reload deferred; Emacs will load the changed files at startup" "$d/stdout"; then
    pass "an unavailable Emacs server defers restart and hot-reload until startup"
else
    fail "an unavailable Emacs server defers restart and hot-reload until startup" \
         "rc=$RC stdout: $(cat "$d/stdout") stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 10. a non-connectivity probe error remains fatal -----------------------
d="$TMP/t10"; mkdir -p "$d"; RUN_ENV="EC_STUB_PROBE_ERROR=1" run_deploy "$d"
if [ "$RC" -eq 3 ] \
   && ! log_has 'runtime-restart' \
   && grep -q "Emacs server probe failed: emacsclient: permission denied" "$d/stderr"; then
    pass "a non-connectivity Emacs probe error fails the deploy loudly"
else
    fail "a non-connectivity Emacs probe error fails the deploy loudly" \
         "rc=$RC stdout: $(cat "$d/stdout") stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 11. built-sha stamps are written; deployed stamps stay the bounce signal
# The two stamp families sit in the same directory and answer different
# questions; a build writing the DEPLOYED stamp would silently suppress the
# next bounce, so this asserts both that built-sha appears and that the
# pre-seeded deployed fingerprint is exactly as the kickstart left it.
d="$TMP/t11"; mkdir -p "$d"
seed_deployed "$d" shim-store bin-v1
BEFORE="$(cat "$d/h/.cache/agent-repl/bin/.shim-store.deployed")"
RUN_ENV="" run_deploy "$d"
CACHE="$d/h/.cache/agent-repl/bin"
if [ "$RC" -eq 0 ] \
   && [ "$(cat "$CACHE/.shim-store.built-sha" 2>/dev/null)" = "deadbeefcafe" ] \
   && [ "$(cat "$CACHE/.shim-claude-sidecar.built-sha" 2>/dev/null)" = "deadbeefcafe" ] \
   && [ "$(cat "$d/tree/modules/app/agent-repl/daemon/bin/.built-sha" 2>/dev/null)" = "deadbeefcafe" ] \
   && [ "$(cat "$CACHE/.shim-store.deployed")" = "$BEFORE" ]; then
    pass "built-sha stamps are written without disturbing the deployed-fingerprint stamps"
else
    fail "built-sha stamps are written without disturbing the deployed-fingerprint stamps" \
         "rc=$RC built=$(cat "$CACHE/.shim-store.built-sha" 2>/dev/null) deployed=$(cat "$CACHE/.shim-store.deployed" 2>/dev/null) want-deployed=$BEFORE"
fi

# --- 12. a dirty tree marks the built-sha stamp -----------------------------
d="$TMP/t12"; mkdir -p "$d"
RUN_ENV="GIT_STUB_DIRTY=M__some_file" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && [ "$(cat "$d/h/.cache/agent-repl/bin/.shim-store.built-sha" 2>/dev/null)" = "deadbeefcafe-dirty" ]; then
    pass "a deploy off a dirty tree marks the built-sha stamp -dirty"
else
    fail "a deploy off a dirty tree marks the built-sha stamp -dirty" \
         "rc=$RC stamp=$(cat "$d/h/.cache/agent-repl/bin/.shim-store.built-sha" 2>/dev/null)"
fi

# --- 13. a moved shim bundle is reported, and stops nothing here ------------
# A survivor keeps running the previous bundle's code until the DAEMON relaunches
# it at its own workspace's freeness. The deploy NAMES the move in the rollout
# and stops no shim itself: it restarts nothing and kills nothing.
d="$TMP/t13"; mkdir -p "$d"
RUN_ENV="BF_STUB_SHIM_CONTENT=bundle-v2" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && log_has "runtime-rollout-await '(.* :shim t" \
   && ! log_has "runtime-restart" \
   && grep -q "shim: bundle moved since the last deploy" "$d/stdout"; then
    pass "a changed shim bundle is reported and the deploy stops no shim itself"
else
    fail "a changed shim bundle is reported and the deploy stops no shim itself" \
         "rc=$RC stdout: $(cat "$d/stdout") rollout=$(grep -o 'runtime-r[eo][^)]*' "$STUB_LOG" | tail -1)"
fi

# --- 14. a second deploy of the SAME build rolls nothing out ----------------
# The ordinary case of a deploy with nothing new in it: the daemon binary is the
# one the first deploy's handover stamped, and neither bundle moved. The daemon
# is not asked for anything — a rollout that names nothing is not a rollout.
d="$TMP/t14"; mkdir -p "$d"
RUN_ENV="BF_STUB_SHIM_CONTENT=bundle-v1" run_deploy "$d"
RUN_ENV="BF_STUB_SHIM_CONTENT=bundle-v1" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && ! log_has "runtime-r[eo]" \
   && grep -q "nothing the daemon rolls out was rebuilt" "$d/stdout"; then
    pass "a second deploy of the same build asks the daemon for nothing"
else
    fail "a second deploy of the same build asks the daemon for nothing" \
         "rc=$RC rollout=$(grep -o 'runtime-r[eo][^)]*' "$STUB_LOG" | tail -1)"
fi

# --- 14b. a moved webapp is named, and nothing else is ----------------------
d="$TMP/t14b"; mkdir -p "$d"
RUN_ENV="BF_STUB_WEBAPP_CONTENT=entry-v1" run_deploy "$d"
RUN_ENV="BF_STUB_WEBAPP_CONTENT=entry-v2" run_deploy "$d"
if [ "$RC" -eq 0 ] && log_has "runtime-rollout-await '( :webapp t )"; then
    pass "a webapp that moved on its own is the only thing the rollout names"
else
    fail "a webapp that moved on its own is the only thing the rollout names" \
         "rc=$RC rollout=$(grep -o 'runtime-r[eo][^)]*' "$STUB_LOG" | tail -1)"
fi

# --- 14c. a binary built but never rolled out is still owed a handover ------
# `--no-bounce` installs the new daemon binary and rolls nothing out. The next
# real deploy rebuilds the identical file, and must NOT read "unchanged" as
# "already serving": the stamp, not the build, is the authority.
d="$TMP/t14c"; mkdir -p "$d"
RUN_ENV="" run_deploy "$d" --no-bounce
RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] && log_has "runtime-rollout-await '( :daemon t"; then
    pass "a daemon binary installed by --no-bounce is still handed over by the next deploy"
else
    fail "a daemon binary installed by --no-bounce is still handed over by the next deploy" \
         "rc=$RC rollout=$(grep -o 'runtime-r[eo][^)]*' "$STUB_LOG" | tail -1)"
fi

# --- 15. a bundle changed WITHIN one revision is still DETECTED -------------
# The dirty-tree case: the built-sha stamp reads the same "<sha>-dirty" before
# and after, so only the bundle's own content can report the change. The signal
# is reported rather than acted on, so what this asserts is the detection.
d="$TMP/t15"; mkdir -p "$d"
RUN_ENV="BF_STUB_SHIM_CONTENT=bundle-v1 GIT_STUB_DIRTY=M__x" run_deploy "$d"
RUN_ENV="BF_STUB_SHIM_CONTENT=bundle-v2 GIT_STUB_DIRTY=M__x" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && grep -q "shim: bundle moved since the last deploy" "$d/stdout" \
   && ! log_has "runtime-restart-await t"; then
    pass "a bundle that moved within one revision is detected without stopping the shims"
else
    fail "a bundle that moved within one revision is detected without stopping the shims" \
         "rc=$RC stdout: $(cat "$d/stdout") restart=$(grep -o 'runtime-restart[^)]*' "$STUB_LOG" | tail -1)"
fi

# --- 16. the deploy never re-navigates a webview itself ---------------------
# The webview reload is the daemon's own `reload_webapp` push. A deploy-side
# sweep would be a second, competing path, so the chain must not call one.
d="$TMP/t16"; mkdir -p "$d"
run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && ! log_has "refresh-webviews" \
   && ! log_has "reload-webview"; then
    pass "the deploy leaves the webview reload to the daemon"
else
    fail "the deploy leaves the webview reload to the daemon" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(cat "$STUB_LOG")"
fi

# --- 20. core.el in the change set expands to the full module set ----------
# core.el cancels every module timer at load time and only its owner files
# re-arm them, so loading core.el with a partial set strands the 1Hz heartbeat.
d="$TMP/t20"; mkdir -p "$d"
RUN_ENV="GIT_STUB_DIFF_FILES=modules/app/agent-repl/lisp/core.el" run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 0 ] \
   && grep -q "core.el in change set — expanding to full module reload (3 files)" "$d/stdout" \
   && log_has 'emacsclient --eval (load .*core.el' \
   && log_has 'emacsclient --eval (load .*workspace.el' \
   && log_has 'emacsclient --eval (load .*status.el'; then
    pass "a change set containing core.el expands to the full canonical module set"
else
    fail "a change set containing core.el expands to the full canonical module set" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(cat "$STUB_LOG")"
fi

# --- 21. the expansion follows config.el's load-module order ---------------
# Loading status.el before core.el would let core.el's cancel-all clear the
# heartbeat status.el had just armed, which is the stranding this fixes.
d="$TMP/t21"; mkdir -p "$d"
RUN_ENV="GIT_STUB_DIFF_FILES=modules/app/agent-repl/lisp/core.el" run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 0 ] \
   && log_before 'load .*/core.el' 'load .*/workspace.el' \
   && log_before 'load .*/workspace.el' 'load .*/status.el'; then
    pass "the expanded load list follows config.el's agent-repl--load-module order"
else
    fail "the expanded load list follows config.el's agent-repl--load-module order" \
         "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 22. a change set without core.el stays minimal ------------------------
d="$TMP/t22"; mkdir -p "$d"
RUN_ENV="GIT_STUB_DIFF_FILES=modules/app/agent-repl/lisp/status.el" run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 0 ] \
   && ! grep -q "expanding to full module reload" "$d/stdout" \
   && log_has 'emacsclient --eval (load .*status.el' \
   && ! log_has 'emacsclient --eval (load .*workspace.el'; then
    pass "a change set without core.el loads only the changed files"
else
    fail "a change set without core.el loads only the changed files" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(cat "$STUB_LOG")"
fi

# --- 23. a changed file outside the loader still loads after the expansion --
d="$TMP/t23"; mkdir -p "$d"
RUN_ENV="GIT_STUB_DIFF_FILES=modules/app/agent-repl/lisp/core.el,modules/app/agent-repl/config.el" \
    run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 0 ] \
   && grep -q "expanding to full module reload (4 files)" "$d/stdout" \
   && log_before 'load .*/status.el' 'load .*/config.el'; then
    pass "a changed file the loader does not name is appended after the canonical set"
else
    fail "a changed file the loader does not name is appended after the canonical set" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(cat "$STUB_LOG")"
fi

# --- 24. the post-load timer assertion runs and its result is logged -------
d="$TMP/t24"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 0 ] \
   && log_before 'load .*status.el' 'assert-heartbeat-armed' \
   && grep -q "heartbeat assertion: armed=4 rearmed=0 failed=0 unavailable=0" "$d/stdout"; then
    pass "the post-load heartbeat assertion runs after the loads and logs its result"
else
    fail "the post-load heartbeat assertion runs after the loads and logs its result" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(cat "$STUB_LOG")"
fi

# --- 25. a re-armed stranded timer is reported loudly, deploy still succeeds
d="$TMP/t25"; mkdir -p "$d"; RUN_ENV="EC_STUB_ASSERT=rearmed" run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 0 ] && grep -q "RE-ARMED stranded timers" "$d/stdout"; then
    pass "a stranded timer that the assertion re-armed is reported without failing the deploy"
else
    fail "a stranded timer that the assertion re-armed is reported without failing the deploy" \
         "rc=$RC stdout: $(cat "$d/stdout")"
fi

# --- 26. a timer that could NOT be re-armed fails the deploy ---------------
d="$TMP/t26"; mkdir -p "$d"; RUN_ENV="EC_STUB_ASSERT=failed" run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 3 ] && grep -q "a required timer could not be re-armed" "$d/stderr"; then
    pass "a timer the assertion could not re-arm fails the deploy loudly"
else
    fail "a timer the assertion could not re-arm fails the deploy loudly" \
         "rc=$RC stderr: $(cat "$d/stderr")"
fi

# --- 27. an Emacs predating the assertion reports a skip -------------------
d="$TMP/t27"; mkdir -p "$d"; RUN_ENV="EC_STUB_ASSERT=absent" run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 0 ] && grep -q "heartbeat assertion skipped — function absent" "$d/stdout"; then
    pass "an Emacs lacking the heartbeat assertion skips it without failing the deploy"
else
    fail "an Emacs lacking the heartbeat assertion skips it without failing the deploy" \
         "rc=$RC stdout: $(cat "$d/stdout")"
fi

# --- 28. an unrecognized assertion result is fatal -------------------------
d="$TMP/t28"; mkdir -p "$d"; RUN_ENV="EC_STUB_ASSERT=garbage" run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 3 ] && grep -q "heartbeat assertion returned an unrecognized result" "$d/stderr"; then
    pass "an unrecognized heartbeat-assertion result fails the deploy rather than passing silently"
else
    fail "an unrecognized heartbeat-assertion result fails the deploy rather than passing silently" \
         "rc=$RC stderr: $(cat "$d/stderr")"
fi

# --- 29. the socket arrives late, but inside the bound ---------------------
# A boot slower than one poll is not a failure. The old wait was a flat
# stopwatch and could not tell a slow boot from a dead one; this one waits on
# the service.
d="$TMP/t29"; mkdir -p "$d"
RUN_ENV="STORE_STUB_MODE=late STORE_STUB_LATE_POLLS=3" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && grep -q "store: socket appeared .*s after kickstart" "$d/stdout" \
   && log_has "bootstrap gui/.*shim-claude-sidecar" \
   && log_has "runtime-rollout-await"; then
    pass "a store socket that appears late but within the bound completes the deploy"
else
    fail "a store socket that appears late but within the bound completes the deploy" \
         "rc=$RC stdout: $(cat "$d/stdout") stderr: $(cat "$d/stderr")"
fi

# --- 30. the store dies before the socket appears --------------------------
# The failure that matters is told apart from a slow boot by launchd's pid, and
# it stops the deploy where it stands: a sidecar kickstarted against a store
# that is not there recovers its link cold, which is a silent full re-read.
d="$TMP/t30"; mkdir -p "$d"
RUN_ENV="STORE_STUB_MODE=dead" run_deploy "$d"
if [ "$RC" -eq 1 ] \
   && grep -q "the store died before" "$d/stderr" \
   && grep -q "The sidecar was stopped for this restart and has NOT been started again" "$d/stderr" \
   && ! log_has "bootstrap gui/.*shim-claude-sidecar" \
   && ! log_has "runtime-restart"; then
    pass "a store that dies before its socket appears fails the deploy without restarting the sidecar"
else
    fail "a store that dies before its socket appears fails the deploy without restarting the sidecar" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 31. a store that is alive and silent is wedged ------------------------
d="$TMP/t31"; mkdir -p "$d"
RUN_ENV="STORE_STUB_MODE=wedged" run_deploy "$d"
if [ "$RC" -eq 1 ] \
   && grep -q "has written nothing for 2s" "$d/stderr" \
   && ! log_has "bootstrap gui/.*shim-claude-sidecar"; then
    pass "a store that is alive but writing nothing fails the wait as wedged"
else
    fail "a store that is alive but writing nothing fails the wait as wedged" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 32. a boot that keeps working past the upper bound --------------------
# Progress keeps the stall budget alive indefinitely, so the wait needs a stated
# ceiling of its own — and the nuke, the one slow boot with a known cause, is
# named rather than left looking like a hang.
d="$TMP/t32"; mkdir -p "$d"
RUN_ENV="STORE_STUB_MODE=nuking AGENT_REPL_STORE_SOCK_MAX=3" run_deploy "$d"
if [ "$RC" -eq 1 ] \
   && grep -q "did not appear within the 3s upper bound" "$d/stderr" \
   && grep -q "the database is being replaced" "$d/stdout" \
   && ! log_has "bootstrap gui/.*shim-claude-sidecar" \
   && ! log_has "runtime-restart"; then
    pass "a store still working past the upper bound fails loudly and names the schema nuke"
else
    fail "a store still working past the upper bound fails loudly and names the schema nuke" \
         "rc=$RC stdout: $(cat "$d/stdout") stderr: $(cat "$d/stderr")"
fi

# --- 33. a preload naming an absent file fails before anything moves -------
# The deleted lisp/frontend-client.el stayed in the preload form, and every
# deploy died on it in step 5 — after both services had been kickstarted.
d="$TMP/t33"; mkdir -p "$d"
PRE_RUN='rm -f modules/app/agent-repl/lisp/services.el' RUN_ENV="" run_deploy "$d"
PRE_RUN=""
if [ "$RC" -eq 3 ] \
   && grep -q "preload names 1 file(s) this checkout does not have: lisp/services.el" "$d/stderr" \
   && ! log_has "launchctl" \
   && ! log_has "make -C" \
   && ! log_has "runtime-restart"; then
    pass "a control-plane preload naming an absent file fails before any build or kickstart"
else
    fail "a control-plane preload naming an absent file fails before any build or kickstart" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 34. a guarded Emacs must not be the one that restarts the daemon ------
# bin/realtest.sh drives the editor under AGENT_REPL_FORBID_VENDOR_CALLS, and
# the restart below is made BY the running Emacs, so a deploy through it hands
# the owner a daemon whose shims answer from the FAKE vendor (owner's live
# logs, 2026-09-13 14:19).
d="$TMP/t34"; mkdir -p "$d"; RUN_ENV="EC_STUB_GUARDED=1" run_deploy "$d"
if [ "$RC" -eq 3 ] \
   && ! log_has 'runtime-restart' \
   && ! log_has 'emacsclient --eval (load' \
   && grep -q "REFUSING to restart the daemon: the running Emacs (pid 31337) carries AGENT_REPL_FORBID_VENDOR_CALLS" "$d/stderr" \
   && grep -q "open -gj -a Emacs" "$d/stderr"; then
    pass "a deploy REFUSES to restart the daemon through an Emacs that carries the vendor guard"
else
    fail "a deploy REFUSES to restart the daemon through an Emacs that carries the vendor guard" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 35. the realtest operator's own consent goes ahead --------------------
d="$TMP/t35"; mkdir -p "$d"
RUN_ENV="EC_STUB_GUARDED=1 AGENT_REPL_REALTEST_TAKEOVER=1" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && log_has 'runtime-rollout-await' \
   && grep -q "AGENT_REPL_REALTEST_TAKEOVER=1 — restarting through the guarded Emacs (pid 31337)" "$d/stdout"; then
    pass "AGENT_REPL_REALTEST_TAKEOVER=1 lets a deploy restart through a guarded Emacs, and says so"
else
    fail "AGENT_REPL_REALTEST_TAKEOVER=1 lets a deploy restart through a guarded Emacs, and says so" \
         "rc=$RC stdout: $(cat "$d/stdout") stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

# --- 36. a guard-free Emacs is deployed through without a word -------------
d="$TMP/t36"; mkdir -p "$d"; RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && log_has 'runtime-rollout-await' \
   && ! grep -q "REFUSING to restart the daemon" "$d/stderr"; then
    pass "an Emacs that carries no vendor guard is deployed through as before"
else
    fail "an Emacs that carries no vendor guard is deployed through as before" \
         "rc=$RC stdout: $(cat "$d/stdout") stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
fi

echo
echo "passed $PASS, failed $FAIL"
[ "$FAIL" -eq 0 ]
