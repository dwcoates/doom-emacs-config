#!/usr/bin/env bash
# Hermetic fixture tests for bin/store-reset.sh.
#
# NO REAL LAUNCHD AND NO REAL STORE. Every service action goes through a
# launchctl stub on AGENT_REPL_LAUNCHCTL that records what it was asked to do
# in a transcript file and keeps its own view of which labels are "running" as
# files in a state directory. XDG_CACHE_HOME points the script at a fixture
# store directory, so the live ~/.cache/agent-repl is never touched.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
RESET="$THIS_DIR/store-reset.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
PASS=0
FAIL=0

pass() { printf '  PASS: %s\n' "$1"; PASS=$((PASS + 1)); }
fail() { printf '  FAIL: %s\n' "$1" >&2; FAIL=$((FAIL + 1)); }

STUB="$TMP/launchctl"
cat >"$STUB" <<'STUB_EOF'
#!/usr/bin/env bash
# launchctl stub. STATE holds one file per label launchd KNOWS, containing that
# service's pid; TRANSCRIPT records every call as "<verb> <label-or-plist>".
#
# IT MODELS KeepAlive, because both real plists set it and that is the whole
# reason this script boots services out rather than killing them: a `kill` here
# is answered by a NEW PID, never by the label going away. Only `bootout`
# removes a label, and only `bootstrap` (of a plist file) brings one back.
#
# STUB_STORE_SOCK, when set, is bound by a bootstrap of the store label unless
# STUB_STORE_DIES_ON_BOOT says the boot fails instead, and STUB_IGNORES_BOOTOUT
# models a service launchd will not let go of.
set -euo pipefail
state="${STUB_STATE:?}"
verb="$1"
last="${!#}"
case "$verb" in
    bootstrap) label="$(basename "$last" .plist)" ;;
    *)         label="${last##*/}" ;;
esac
printf '%s %s\n' "$verb" "$last" >>"${STUB_TRANSCRIPT:?}"
case "$verb" in
    print)
        [ -f "$state/$label" ] || exit 1
        printf '\tpid = %s\n' "$(cat "$state/$label")"
        ;;
    kill)
        # KeepAlive: the process dies and launchd relaunches it at once, so the
        # label is still loaded and its pid has merely changed.
        [ -f "$state/$label" ] || exit 1
        echo $(( $(cat "$state/$label") + 1 )) >"$state/$label"
        ;;
    bootout)
        [ "${STUB_IGNORES_BOOTOUT:-0}" = 1 ] || rm -f "$state/$label"
        ;;
    bootstrap)
        [ -f "$last" ] || exit 1
        echo 4242 >"$state/$label"
        if [ "$label" = com.agentrepl.shim-store ]; then
            if [ "${STUB_STORE_DIES_ON_BOOT:-0}" = 1 ]; then
                rm -f "$state/$label"
            elif [ -n "${STUB_STORE_SOCK:-}" ]; then
                # BOUND FROM ITS OWN DIRECTORY, BY BASENAME. macOS caps
                # sun_path at ~104 bytes and a mktemp fixture root already
                # spends most of that, so an absolute bind fails where a
                # relative one -- which the kernel measures on its own length
                # -- succeeds.
                mkdir -p "$(dirname "$STUB_STORE_SOCK")"
                rm -f "$STUB_STORE_SOCK"
                ( cd "$(dirname "$STUB_STORE_SOCK")" &&
                  python3 -c 'import socket,sys; s=socket.socket(socket.AF_UNIX); s.bind(sys.argv[1])' \
                      "$(basename "$STUB_STORE_SOCK")" )
            fi
        fi
        ;;
esac
STUB_EOF
chmod +x "$STUB"

# arrange builds one case's world: a fixture cache home with a populated store
# directory and both labels running. It echoes the case root.
arrange() { # CASE-NAME
    local root="$TMP/$1"
    mkdir -p "$root/cache/agent-repl/store" "$root/cache/agent-repl/sock" "$root/state" \
             "$root/LaunchAgents"
    # The installed plists a bootstrap needs, named exactly as
    # .claude/install.sh installs them.
    printf 'plist\n' >"$root/LaunchAgents/com.agentrepl.shim-store.plist"
    printf 'plist\n' >"$root/LaunchAgents/com.agentrepl.shim-claude-sidecar.plist"
    printf 'db\n'  >"$root/cache/agent-repl/store/events.db"
    printf 'wal\n' >"$root/cache/agent-repl/store/events.db-wal"
    printf 'shm\n' >"$root/cache/agent-repl/store/events.db-shm"
    echo 111 >"$root/state/com.agentrepl.shim-store"
    echo 222 >"$root/state/com.agentrepl.shim-claude-sidecar"
    : >"$root/transcript"
    echo "$root"
}

# act runs the script against one case root. Extra environment assignments are
# passed as NAME=VALUE arguments before the script's own flags.
act() { # CASE-ROOT [ENV=VAL...] [-- FLAGS...]
    local root="$1"; shift
    local envs=() flags=()
    while [ $# -gt 0 ]; do
        case "$1" in
            --) shift; flags=("$@"); break ;;
            *) envs+=("$1") ;;
        esac
        shift
    done
    env \
        XDG_CACHE_HOME="$root/cache" \
        AGENT_REPL_LAUNCHCTL="$STUB" \
        AGENT_REPL_LAUNCH_AGENTS_DIR="$root/LaunchAgents" \
        STUB_STATE="$root/state" \
        STUB_TRANSCRIPT="$root/transcript" \
        STUB_STORE_SOCK="$root/cache/agent-repl/sock/store.sock" \
        AGENT_REPL_STORE_SOCK_MAX=2 \
        ${envs[@]+"${envs[@]}"} \
        "$RESET" ${flags[@]+"${flags[@]}"} >"$root/out" 2>"$root/err"
}

echo "store-reset.sh: the guard"

# The unset environment refuses and removes nothing.
root="$(arrange guard-unset)"
if act "$root"; then
    fail "an unset AGENT_REPL_STORE_RESET should refuse"
elif ! grep -q "AGENT_REPL_STORE_RESET=1" "$root/err"; then
    fail "the refusal should name the variable that lifts it"
elif [ ! -f "$root/cache/agent-repl/store/events.db" ]; then
    fail "a refused run must remove nothing"
else
    pass "an unset AGENT_REPL_STORE_RESET refuses and removes nothing"
fi

# An explicit no is a no: the guard is an exact match, not a truthiness test.
root="$(arrange guard-zero)"
if act "$root" AGENT_REPL_STORE_RESET=0; then
    fail "AGENT_REPL_STORE_RESET=0 should refuse"
elif [ ! -f "$root/cache/agent-repl/store/events.db" ]; then
    fail "a refused run must remove nothing"
else
    pass "AGENT_REPL_STORE_RESET=0 refuses rather than reading as truthy"
fi

echo "store-reset.sh: the reset"

# The whole happy path: the three files go, both services come back.
root="$(arrange reset-happy)"
if ! act "$root" AGENT_REPL_STORE_RESET=1; then
    fail "the reset should succeed ($(cat "$root/err"))"
elif [ -f "$root/cache/agent-repl/store/events.db" ] ||
     [ -f "$root/cache/agent-repl/store/events.db-wal" ] ||
     [ -f "$root/cache/agent-repl/store/events.db-shm" ]; then
    fail "the database and its -wal/-shm siblings should all be gone"
else
    pass "the database and its -wal/-shm siblings are removed"
fi

# The recorded safe order: the sidecar stops first and starts last, with the
# store's socket in between.
root="$(arrange reset-order)"
act "$root" AGENT_REPL_STORE_RESET=1 || true
order="$(grep -E '^(bootout|bootstrap) ' "$root/transcript" | awk -F/ '{print $NF}' | tr '\n' '|')"
expected='com.agentrepl.shim-claude-sidecar|com.agentrepl.shim-store|com.agentrepl.shim-store.plist|com.agentrepl.shim-claude-sidecar.plist|'
if [ "$order" != "$expected" ]; then
    fail "expected the recorded safe order, got: $order"
else
    pass "the sidecar stops first and starts last, the store in between"
fi

# An absent database is the reset's postcondition already holding.
root="$(arrange reset-absent)"
rm -f "$root/cache/agent-repl/store/events.db" \
      "$root/cache/agent-repl/store/events.db-wal" \
      "$root/cache/agent-repl/store/events.db-shm"
if ! act "$root" AGENT_REPL_STORE_RESET=1; then
    fail "an absent database should not be an error ($(cat "$root/err"))"
elif ! grep -q "removed 0 database file(s)" "$root/out"; then
    fail "an absent database should be reported as nothing removed"
else
    pass "an absent database is not an error"
fi

# --keep-down removes the files and starts nothing.
root="$(arrange reset-keep-down)"
if ! act "$root" AGENT_REPL_STORE_RESET=1 -- --keep-down; then
    fail "--keep-down should succeed ($(cat "$root/err"))"
elif [ -f "$root/cache/agent-repl/store/events.db" ]; then
    fail "--keep-down should still remove the database"
elif grep -q '^bootstrap ' "$root/transcript"; then
    fail "--keep-down must start nothing"
else
    pass "--keep-down removes the database and leaves both services stopped"
fi

echo "store-reset.sh: the refusals that protect data"

# A directory at the database path is reported, never removed.
root="$(arrange reset-directory)"
rm -f "$root/cache/agent-repl/store/events.db"
mkdir -p "$root/cache/agent-repl/store/events.db/somebody-elses-data"
if act "$root" AGENT_REPL_STORE_RESET=1; then
    fail "a directory at the database path should be refused"
elif [ ! -d "$root/cache/agent-repl/store/events.db/somebody-elses-data" ]; then
    fail "a directory at the database path must survive the refusal"
else
    pass "a directory at the database path is reported, never removed"
fi

# A service that will not leave the domain fails loudly, before anything is
# removed.
root="$(arrange reset-wont-stop)"
if act "$root" AGENT_REPL_STORE_RESET=1 STUB_IGNORES_BOOTOUT=1; then
    fail "a service that never exits should fail the reset"
elif [ ! -f "$root/cache/agent-repl/store/events.db" ]; then
    fail "nothing may be removed while a service is still running"
else
    pass "a service that will not exit fails the reset with the database intact"
fi

# A store that dies on boot fails, and the sidecar is never started.
root="$(arrange reset-store-dies)"
if act "$root" AGENT_REPL_STORE_RESET=1 STUB_STORE_DIES_ON_BOOT=1; then
    fail "a store that dies on boot should fail the reset"
elif grep -q '^bootstrap .*com.agentrepl.shim-claude-sidecar' "$root/transcript"; then
    fail "the sidecar must not be started after the store died"
else
    pass "a store that dies on boot fails and the sidecar is never started"
fi

# A KEPT-ALIVE SERVICE IS NEVER KILLED. `launchctl kill` cannot stop a service
# whose plist sets KeepAlive -- launchd relaunches it with a new pid -- so the
# reset must not reach for one at all.
root="$(arrange reset-no-kill)"
act "$root" AGENT_REPL_STORE_RESET=1 || true
if grep -q '^kill ' "$root/transcript"; then
    fail "a kept-alive service must be booted out, never killed"
else
    pass "the reset never kills a kept-alive service"
fi

# The service is brought back from the plist it was installed from.
root="$(arrange reset-bootstrap-plist)"
act "$root" AGENT_REPL_STORE_RESET=1 || true
if ! grep -qx "bootstrap $root/LaunchAgents/com.agentrepl.shim-store.plist" "$root/transcript"; then
    fail "the store should be bootstrapped from its installed plist, got: $(grep '^bootstrap' "$root/transcript")"
else
    pass "each service is bootstrapped from its installed plist"
fi

# A missing plist is refused while everything is still up.
root="$(arrange reset-missing-plist)"
rm -f "$root/LaunchAgents/com.agentrepl.shim-claude-sidecar.plist"
if act "$root" AGENT_REPL_STORE_RESET=1; then
    fail "a missing plist should refuse: the services could not be brought back"
elif grep -q '^bootout ' "$root/transcript"; then
    fail "nothing may be booted out when a plist is missing"
elif [ ! -f "$root/cache/agent-repl/store/events.db" ]; then
    fail "a refused run must remove nothing"
else
    pass "a missing plist refuses before anything is stopped or removed"
fi

# --keep-down starts nothing, so it needs no plist.
root="$(arrange reset-keep-down-no-plist)"
rm -f "$root/LaunchAgents/"*.plist
if ! act "$root" AGENT_REPL_STORE_RESET=1 -- --keep-down; then
    fail "--keep-down should not require the plists ($(cat "$root/err"))"
elif [ -f "$root/cache/agent-repl/store/events.db" ]; then
    fail "--keep-down should still remove the database"
else
    pass "--keep-down needs no plist, because it starts nothing"
fi

printf '\n%d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
