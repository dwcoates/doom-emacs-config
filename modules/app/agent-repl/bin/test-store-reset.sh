#!/usr/bin/env bash
# Hermetic fixture tests for bin/store-reset.sh.
#
# NO REAL LAUNCHD AND NO REAL STORE. Every service action goes through a
# launchctl stub on AGENT_REPL_LAUNCHCTL that records what it was asked to do
# in a transcript file and keeps its own view of which labels are "running" as
# files in a state directory. XDG_CACHE_HOME points the script at a fixture
# store directory, so the live ~/.cache/agent-repl is never touched.

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
# launchctl stub. STATE holds one file per "running" label; TRANSCRIPT records
# every call as "<verb> <label>". STUB_STORE_SOCK, when set, is bound by a
# kickstart of the store label unless STUB_STORE_DIES_ON_BOOT says the boot
# fails instead, and STUB_IGNORES_SIGTERM models a service that will not exit.
#
# The target is the LAST argument in every form this script uses
# (`print gui/<uid>/<label>`, `kill SIGTERM gui/<uid>/<label>`,
# `kickstart gui/<uid>/<label>`), so the stub reads it from there rather than
# from a per-verb position.
set -euo pipefail
state="${STUB_STATE:?}"
verb="$1"
target="${!#}"
label="${target##*/}"
printf '%s %s\n' "$verb" "$label" >>"${STUB_TRANSCRIPT:?}"
case "$verb" in
    print)
        [ -f "$state/$label" ] || exit 1
        printf '\tpid = %s\n' "$(cat "$state/$label")"
        ;;
    kill)
        [ "${STUB_IGNORES_SIGTERM:-0}" = 1 ] || rm -f "$state/$label"
        ;;
    kickstart)
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
    mkdir -p "$root/cache/agent-repl/store" "$root/cache/agent-repl/sock" "$root/state"
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
order="$(grep -E '^(kill|kickstart) ' "$root/transcript" | tr '\n' '|')"
expected='kill com.agentrepl.shim-claude-sidecar|kill com.agentrepl.shim-store|kickstart com.agentrepl.shim-store|kickstart com.agentrepl.shim-claude-sidecar|'
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
elif grep -q '^kickstart ' "$root/transcript"; then
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

# A service that will not exit fails loudly, before anything is removed.
root="$(arrange reset-wont-stop)"
if act "$root" AGENT_REPL_STORE_RESET=1 STUB_IGNORES_SIGTERM=1; then
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
elif grep -q '^kickstart com.agentrepl.shim-claude-sidecar' "$root/transcript"; then
    fail "the sidecar must not be started after the store died"
else
    pass "a store that dies on boot fails and the sidecar is never started"
fi

printf '\n%d passed, %d failed\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
