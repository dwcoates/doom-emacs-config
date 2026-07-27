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
             "$mod/agent-shim/shim-store" "$mod/agent-shim/claude/shim-sidecar"
    cp "$SCRIPT_UNDER_TEST" "$mod/bin/deploy-all.sh"
    chmod +x "$mod/bin/deploy-all.sh"

    cat > "$mod/bin/build-frontend.sh" <<'EOF'
#!/usr/bin/env bash
echo "build-frontend $*" >> "$STUB_LOG"
EOF
    chmod +x "$mod/bin/build-frontend.sh"

    # An .el file for the --elisp scenario (test-foo.el deliberately absent
    # from disk to double as the deleted-file case if listed).
    printf ';; stub\n' > "$mod/status.el"
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
if [ "${EC_STUB_REFUSE:-0}" = "1" ]; then
    case "$*" in
        *daemon-restart*)
            echo "*ERROR*: agent-repl: refusing daemon stop — turn in flight"
            exit 1
            ;;
    esac
fi
echo t
EOF

    cat > "$stubs/git" <<'EOF'
#!/usr/bin/env bash
echo "git $*" >> "$STUB_LOG"
case "$*" in
    *"diff --name-only"*)
        printf '%s\n' \
            "modules/app/agent-repl/status.el" \
            "modules/app/agent-repl/test-foo.el" \
            "modules/app/agent-repl/deleted.el"
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
   && log_before "kickstart -k gui/.*shim-claude-sidecar" "daemon-restart"; then
    pass "fresh tree runs the full chain in dependency order"
else
    fail "fresh tree runs the full chain in dependency order" "rc=$RC log: $(cat "$STUB_LOG")"
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

# --- 6. refused daemon restart fails the deploy loudly ----------------------
d="$TMP/t6"; mkdir -p "$d"; RUN_ENV="EC_STUB_REFUSE=1" run_deploy "$d"
if [ "$RC" -eq 3 ] && grep -q "daemon restart" "$d/stderr"; then
    pass "a refused daemon restart exits 3 with the refusal surfaced"
else
    fail "a refused daemon restart exits 3 with the refusal surfaced" "rc=$RC stderr: $(cat "$d/stderr")"
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

# --- 8. --force kickstarts both services despite identical binaries ---------
d="$TMP/t8"; mkdir -p "$d/h/.cache/agent-repl/bin"
printf 'bin-v1' > "$d/h/.cache/agent-repl/bin/shim-store"
printf 'bin-v1' > "$d/h/.cache/agent-repl/bin/shim-claude-sidecar"
RUN_ENV="" run_deploy "$d" --force
if [ "$RC" -eq 0 ] \
   && log_has "build-frontend --force" \
   && log_has "kickstart -k gui/.*shim-store" \
   && log_has "kickstart -k gui/.*shim-claude-sidecar"; then
    pass "--force propagates to build-frontend and kickstarts both services"
else
    fail "--force propagates to build-frontend and kickstarts both services" "rc=$RC log: $(cat "$STUB_LOG")"
fi

echo
echo "passed $PASS, failed $FAIL"
[ "$FAIL" -eq 0 ]
