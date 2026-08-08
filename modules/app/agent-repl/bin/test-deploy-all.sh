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
    mkdir -p "$mod/bin" "$mod/proto" "$mod/daemon/cmd/claude-repld" \
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
printf '%s\n' '{"gate":{"system":"webapp","ready":true,"deployed_sha":"source-revision","source_sha":"source-revision"}}'
EOF
    chmod +x "$mod/bin/readiness-report.sh"
    chmod +x "$mod/bin/deploy-all.sh"
    printf ';; stub daemon control plane\n' > "$mod/daemon.el"
    printf ';; stub frontend client control plane\n' > "$mod/frontend-client.el"
    printf ';; stub runtime coordinator\n' > "$mod/services.el"

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
EOF
    chmod +x "$mod/bin/build-frontend.sh"

    # .el files for the --elisp scenarios (test-foo.el and deleted.el are
    # deliberately absent from disk so they double as the skipped cases).
    #
    # config.el carries the canonical `agent-repl--load-module' order the
    # core.el expansion reads. `emoji' is listed but has no file on disk, so
    # the expansion is also exercised against a loader entry whose module is
    # missing.
    printf ';; stub\n' > "$mod/status.el"
    printf ';; stub\n' > "$mod/core.el"
    printf ';; stub\n' > "$mod/workspace.el"
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
    # would.
    cat > "$stubs/launchctl" <<'EOF'
#!/usr/bin/env bash
echo "launchctl $*" >> "$STUB_LOG"
case "$*" in
    *com.agentrepl.shim-store*)
        mkdir -p "$HOME/.cache/agent-repl/sock"
        rm -f "$HOME/.cache/agent-repl/sock/store.sock"
        python3 - <<'PY'
import os, socket
p = os.path.expanduser("~/.cache/agent-repl/sock/store.sock")
socket.socket(socket.AF_UNIX).bind(p)
PY
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
# The webview refresh is `fboundp'-guarded inside the eval, so the stub answers
# for BOTH shapes: a current Emacs returns the sweep's count, an older one
# (EC_STUB_REFRESH_ABSENT=1) returns the absent branch's string.
case "$*" in
    *artifact-root-same*|*artifact-root-changed*)
        echo \"\"${EC_STUB_ARTIFACT_ROOT_RESULT:-artifact-root-same}\"\"
        exit 0
        ;;
    *frontend-daemon-restart-await*)
        if [ "${EC_STUB_REFUSE:-0}" = "1" ]; then
            echo "*ERROR*: agent-repl: refusing daemon stop — turn in flight"
            exit 1
        fi
        echo \"\"${EC_STUB_RESTART_RESULT:-runtime-restart-complete}\"\"
        exit 0
        ;;
    *refresh-webviews*)
        if [ "${EC_STUB_REFRESH_ABSENT:-0}" = "1" ]; then
            echo '"absent"'
        else
            echo "\"refreshed ${EC_STUB_REFRESH_COUNT:-3}\""
        fi
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
    *"rev-parse HEAD"*)     echo "${GIT_STUB_SHA:-deadbeefcafe}" ;;
    *"status --porcelain"*) printf '%s' "${GIT_STUB_DIRTY:-}" ;;
    *"diff --name-only"*)
        if [ -n "${GIT_STUB_DIFF_FILES:-}" ]; then
            # Comma-separated, because RUN_ENV is word-split by the runner and
            # a space-bearing value would be parsed as a command.
            printf '%s\n' ${GIT_STUB_DIFF_FILES//,/ }
        else
            printf '%s\n' \
                "modules/app/agent-repl/status.el" \
                "modules/app/agent-repl/test-foo.el" \
                "modules/app/agent-repl/deleted.el"
        fi
        ;;
esac
EOF

    chmod +x "$stubs"/make "$stubs"/go "$stubs"/launchctl "$stubs"/emacsclient "$stubs"/git
}

# --- per-test runner --------------------------------------------------------
# run_deploy <case-dir> [deploy-all args...]; env overrides go via RUN_ENV.
run_deploy() {
    local dir="$1"; shift
    make_tree "$dir/tree"
    make_stubs "$dir/stubs"
    mkdir -p "$dir/h"
    STUB_LOG="$dir/log"
    : > "$STUB_LOG"
    set +e
    env PATH="$dir/stubs:/usr/bin:/bin" HOME="$dir/h" STUB_LOG="$STUB_LOG" \
        AGENT_REPL_STORE_SOCK_TIMEOUT=2 AGENT_REPL_EMACSCLIENT=emacsclient \
        ${RUN_ENV:-} \
        bash "$dir/tree/modules/app/agent-repl/bin/deploy-all.sh" "$@" \
        > "$dir/stdout" 2> "$dir/stderr"
    RC=$?
    set -e
}

log_has()    { grep -q "$1" "$STUB_LOG"; }
log_line()   { grep -n "$1" "$STUB_LOG" | head -1 | cut -d: -f1; }
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
   && log_before "kickstart -k gui/.*shim-store" "kickstart -k gui/.*shim-claude-sidecar" \
   && log_before "load .*daemon.el" "daemon-restart" \
   && log_before "load .*frontend-client.el" "daemon-restart" \
   && log_before "load .*services.el" "daemon-restart" \
   && log_before "kickstart -k gui/.*shim-claude-sidecar" "daemon-restart" \
   && log_has "readiness-report --require-ready webapp"; then
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

# --- 1b. a linked-worktree deploy binds its artifact root before restart ----
# The running Emacs may have loaded daemon.el from another checkout. The
# deploy must move the runtime root before restarting and must stop every shim
# that could otherwise survive on the other checkout's bundle.
d="$TMP/t1b"; mkdir -p "$d"
RUN_ENV="EC_STUB_ARTIFACT_ROOT_RESULT=artifact-root-changed" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && log_before "load .*daemon.el" "daemon-restart-await t" \
   && grep -q "artifact root changed — surviving shims will stop" "$d/stdout"; then
    pass "a moved runtime artifact root is bound before restart and stops surviving shims"
else
    fail "a moved runtime artifact root is bound before restart and stops surviving shims" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(cat "$STUB_LOG")"
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
if [ "$RC" -eq 0 ] && ! log_has "launchctl" && log_has "daemon-restart"; then
    pass "services already on the installed binary skip both kickstarts"
else
    fail "services already on the installed binary skip both kickstarts" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 3. store changed: sidecar bounces too, store first ---------------------
d="$TMP/t3"
seed_deployed "$d" shim-store bin-v0
seed_deployed "$d" shim-claude-sidecar bin-v1
RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && log_before "kickstart -k gui/.*shim-store" "kickstart -k gui/.*shim-claude-sidecar"; then
    pass "a store change cascades into a sidecar kickstart, store first"
else
    fail "a store change cascades into a sidecar kickstart, store first" "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 4. sidecar changed alone: store untouched ------------------------------
d="$TMP/t4"
seed_deployed "$d" shim-store bin-v1
seed_deployed "$d" shim-claude-sidecar bin-v0
RUN_ENV="" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && ! log_has "kickstart -k gui/.*shim-store" \
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
   && log_has "kickstart -k gui/.*shim-claude-sidecar"; then
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
d="$TMP/t6"; mkdir -p "$d"; RUN_ENV="EC_STUB_REFUSE=1" run_deploy "$d"
if [ "$RC" -eq 3 ] && grep -q "daemon restart" "$d/stderr"; then
    pass "a refused daemon restart exits 3 with the refusal surfaced"
else
    fail "a refused daemon restart exits 3 with the refusal surfaced" "rc=$RC stderr: $(cat "$d/stderr")"
fi

# --- 6b. pending dispatch is not terminal restart completion ---------------
d="$TMP/t6b"; mkdir -p "$d"; RUN_ENV="EC_STUB_RESTART_RESULT=runtime-restart-pending" run_deploy "$d"
if [ "$RC" -eq 3 ] \
   && grep -q "no terminal completion" "$d/stderr" \
   && ! log_has "refresh-webviews" \
   && ! log_has "readiness-report"; then
    pass "a non-terminal restart result aborts before refresh and revision readiness"
else
    fail "a non-terminal restart result aborts before refresh and revision readiness" \
         "rc=$RC stderr: $(cat "$d/stderr") log: $(cat "$STUB_LOG")"
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
   && log_has "kickstart -k gui/.*shim-claude-sidecar"; then
    pass "--force still kickstarts a service that is not running the installed binary"
else
    fail "--force still kickstarts a service that is not running the installed binary" \
         "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 9. no Emacs server defers restart and elisp reload ---------------------
d="$TMP/t9"; mkdir -p "$d"; RUN_ENV="EC_STUB_UNAVAILABLE=1" run_deploy "$d" --elisp "abc..def"
if [ "$RC" -eq 0 ] \
   && log_has 'emacsclient --eval t' \
   && ! log_has 'daemon-restart' \
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
   && ! log_has 'daemon-restart' \
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

# --- 13. a moved shim bundle makes the bounce STOP surviving shims ----------
# A survivor from before the deploy would keep running the previous bundle's
# code, so this is the one case that must not preserve.
d="$TMP/t13"; mkdir -p "$d"
RUN_ENV="BF_STUB_SHIM_CONTENT=bundle-v2" run_deploy "$d"
if [ "$RC" -eq 0 ] && log_has "daemon-restart-await t"; then
    pass "a changed shim bundle bounces the daemon in stop-shims mode"
else
    fail "a changed shim bundle bounces the daemon in stop-shims mode" \
         "rc=$RC restart=$(grep -o 'daemon-restart[^)]*' "$STUB_LOG" | tail -1)"
fi

# --- 14. an unchanged shim bundle PRESERVES surviving shims -----------------
# The second deploy rebuilds the identical bundle, which is the ordinary case:
# preserving is what makes the bounce a reattach rather than a rebuild.
d="$TMP/t14"; mkdir -p "$d"
RUN_ENV="BF_STUB_SHIM_CONTENT=bundle-v1" run_deploy "$d"
RUN_ENV="BF_STUB_SHIM_CONTENT=bundle-v1" run_deploy "$d"
if [ "$RC" -eq 0 ] && log_has "daemon-restart-await)" && ! log_has "daemon-restart-await t"; then
    pass "an unchanged shim bundle leaves the daemon bounce preserving shims"
else
    fail "an unchanged shim bundle leaves the daemon bounce preserving shims" \
         "rc=$RC restart=$(grep -o 'daemon-restart[^)]*' "$STUB_LOG" | tail -1)"
fi

# --- 15. a bundle changed WITHIN one revision still stops the shims ---------
# The dirty-tree case: the built-sha stamp reads the same "<sha>-dirty" before
# and after, so only the bundle's own content can report the change.
d="$TMP/t15"; mkdir -p "$d"
RUN_ENV="BF_STUB_SHIM_CONTENT=bundle-v1 GIT_STUB_DIRTY=M__x" run_deploy "$d"
RUN_ENV="BF_STUB_SHIM_CONTENT=bundle-v2 GIT_STUB_DIRTY=M__x" run_deploy "$d"
if [ "$RC" -eq 0 ] && log_has "daemon-restart-await t"; then
    pass "a bundle that moved within one revision still stops the shims"
else
    fail "a bundle that moved within one revision still stops the shims" \
         "rc=$RC restart=$(grep -o 'daemon-restart[^)]*' "$STUB_LOG" | tail -1)"
fi

# --- 16. the webview refresh runs after a successful daemon restart ---------
# A page mounted before the bounce talks to a listener that no longer exists,
# so the refresh must follow the restart, never precede it.
d="$TMP/t16"; mkdir -p "$d"
RUN_ENV="EC_STUB_REFRESH_COUNT=2" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && log_before "daemon-restart" "refresh-webviews" \
   && grep -q "webviews: refreshed 2" "$d/stdout"; then
    pass "the webview refresh follows the daemon restart and reports its count"
else
    fail "the webview refresh follows the daemon restart and reports its count" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(cat "$STUB_LOG")"
fi

# --- 17. an Emacs without the command skips, and the deploy still succeeds --
# The running Emacs may predate `agent-repl-refresh-webviews' (step 6's elisp
# hot-load is what defines it, and it runs after), so an absent symbol is a
# reported skip rather than a deploy failure.
d="$TMP/t17"; mkdir -p "$d"
RUN_ENV="EC_STUB_REFRESH_ABSENT=1" run_deploy "$d"
if [ "$RC" -eq 0 ] && grep -q "webviews: skipped — function absent" "$d/stdout"; then
    pass "an Emacs lacking the refresh command skips it without failing the deploy"
else
    fail "an Emacs lacking the refresh command skips it without failing the deploy" \
         "rc=$RC stdout: $(cat "$d/stdout") stderr: $(cat "$d/stderr")"
fi

# --- 18. no Emacs server means nothing to refresh --------------------------
d="$TMP/t18"; mkdir -p "$d"; RUN_ENV="EC_STUB_UNAVAILABLE=1" run_deploy "$d"
if [ "$RC" -eq 0 ] \
   && ! log_has "refresh-webviews" \
   && grep -q "webviews: no Emacs running, so no live page to refresh" "$d/stdout"; then
    pass "an unavailable Emacs server reports there is no live page to refresh"
else
    fail "an unavailable Emacs server reports there is no live page to refresh" \
         "rc=$RC stdout: $(cat "$d/stdout") log: $(cat "$STUB_LOG")"
fi

# --- 19. a refused daemon restart never reaches the refresh ----------------
# The refresh is only meaningful once the new daemon is up; a refused bounce
# leaves the OLD daemon serving, and reloading pages against it is noise.
d="$TMP/t19"; mkdir -p "$d"; RUN_ENV="EC_STUB_REFUSE=1" run_deploy "$d"
if [ "$RC" -eq 3 ] && ! log_has "refresh-webviews"; then
    pass "a refused daemon restart aborts before the webview refresh"
else
    fail "a refused daemon restart aborts before the webview refresh" \
         "rc=$RC log: $(cat "$STUB_LOG")"
fi

# --- 20. core.el in the change set expands to the full module set ----------
# core.el cancels every module timer at load time and only its owner files
# re-arm them, so loading core.el with a partial set strands the 1Hz heartbeat.
d="$TMP/t20"; mkdir -p "$d"
RUN_ENV="GIT_STUB_DIFF_FILES=modules/app/agent-repl/core.el" run_deploy "$d" --elisp "abc..def"
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
RUN_ENV="GIT_STUB_DIFF_FILES=modules/app/agent-repl/core.el" run_deploy "$d" --elisp "abc..def"
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
RUN_ENV="GIT_STUB_DIFF_FILES=modules/app/agent-repl/status.el" run_deploy "$d" --elisp "abc..def"
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
RUN_ENV="GIT_STUB_DIFF_FILES=modules/app/agent-repl/core.el,modules/app/agent-repl/config.el" \
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

echo
echo "passed $PASS, failed $FAIL"
[ "$FAIL" -eq 0 ]
