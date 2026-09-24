#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# test-realtest.sh — hermetic tests for realtest.sh and its backup helper.
#
# Nothing here touches the owner's editor, the owner's state, or a real
# deployment. realtest.sh is exercised as a COPY in a scratch directory beside
# stub siblings — a stub readiness-report.sh, a stub suite-slot.sh, a stub
# emacsclient — so `THIS_DIR` resolves to the scratch directory and every
# external answer the script depends on is the test's to choose. That is the
# only way to assert a refusal: the thing being asserted is that the script does
# NOT run, and running it for real to find out would be the failure.
#
# The backup helper is tested against files in a scratch tree, because the one
# rule it has — never overwrite an existing backup — is a rule about the
# filesystem and a mock of the filesystem would not be testing it.
#
# Run with:   bash bin/test-realtest.sh

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/realtest.sh"
LIB_UNDER_TEST="$THIS_DIR/lib-realtest-backup.sh"

# shellcheck source=lib-realtest-backup.sh
. "$LIB_UNDER_TEST"

readonly EXIT_DECLINED=77
readonly EXIT_INCOMPLETE=78

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -n "${2:-}" ] && echo "       $2"; return 0; }

SCRATCH="$(mktemp -d)"
cleanup() { rm -rf "$SCRATCH"; }
trap cleanup EXIT

# fake_cp_dir MODE — a directory holding a fake `cp` ahead of the real one on
# PATH. "real" logs every invocation (to $STUB_CP_LOG, when set) and then
# performs the real copy. "clone-fail" fails any invocation carrying `-c`
# (simulating a non-APFS volume or a `cp` that does not understand the flag)
# but performs the real copy otherwise. "total-fail" never copies anything.
fake_cp_dir() {
    local mode="$1" dir
    dir="$SCRATCH/fake-cp-$mode/bin"
    mkdir -p "$dir"
    cat > "$dir/cp" <<STUB
#!/usr/bin/env bash
if [ -n "\${STUB_CP_LOG:-}" ]; then
    printf '%s\n' "\$*" >> "\$STUB_CP_LOG"
fi
case "$mode" in
    total-fail)
        exit 1
        ;;
    clone-fail)
        for a in "\$@"; do
            case "\$a" in
                -c) exit 1 ;;
            esac
        done
        ;;
esac
exec /bin/cp "\$@"
STUB
    chmod +x "$dir/cp"
    printf '%s' "$dir"
}

# ---- the backup helper ----------------------------------------------------

test_backup_copies_the_database() {
    local name="the backup helper copies a database and prints where it went"
    local dir="$SCRATCH/backup-plain"
    mkdir -p "$dir"
    printf 'workspaces' > "$dir/wsm.db"

    local out
    if ! out="$(realtest_backup_database "$dir/wsm.db" 20260910-120000 2>&1)"; then
        fail "$name" "the helper failed: $out"
        return
    fi
    if [ ! -f "$dir/wsm.db.realtest-bak-20260910-120000" ]; then
        fail "$name" "no backup was written; the helper said: $out"
        return
    fi
    if [ "$out" != "$dir/wsm.db.realtest-bak-20260910-120000" ]; then
        fail "$name" "the helper printed $out"
        return
    fi
    pass "$name"
}

test_backup_carries_the_wal_siblings() {
    local name="the backup helper carries the -wal and -shm siblings with the database"
    local dir="$SCRATCH/backup-wal"
    mkdir -p "$dir"
    printf 'db' > "$dir/wsm.db"
    printf 'wal' > "$dir/wsm.db-wal"
    printf 'shm' > "$dir/wsm.db-shm"

    if ! realtest_backup_database "$dir/wsm.db" 20260910-120000 >/dev/null 2>&1; then
        fail "$name" "the helper failed"
        return
    fi
    local suffix missing=""
    for suffix in "" "-wal" "-shm"; do
        [ -f "$dir/wsm.db${suffix}.realtest-bak-20260910-120000" ] || missing="$missing $suffix"
    done
    if [ -n "$missing" ]; then
        fail "$name" "these siblings were not backed up:$missing"
        return
    fi
    pass "$name"
}

test_backup_refuses_to_overwrite() {
    local name="the backup helper REFUSES to overwrite an existing backup"
    local dir="$SCRATCH/backup-refuse"
    mkdir -p "$dir"
    printf 'the state as it was before the first run' > "$dir/wsm.db.realtest-bak-20260910-120000"
    printf 'the state the first run left behind' > "$dir/wsm.db"

    local out status=0
    out="$(realtest_backup_database "$dir/wsm.db" 20260910-120000 2>&1)" || status=$?
    if [ "$status" -eq 0 ]; then
        fail "$name" "the helper succeeded; it must refuse"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'refusing to overwrite'; then
        fail "$name" "the refusal does not say what it refused: $out"
        return
    fi
    # THE POINT OF THE RULE: the earlier copy is still the earlier copy.
    if [ "$(cat "$dir/wsm.db.realtest-bak-20260910-120000")" != "the state as it was before the first run" ]; then
        fail "$name" "the existing backup was overwritten anyway"
        return
    fi
    pass "$name"
}

test_backup_absent_file_is_not_a_failure() {
    local name="the backup helper treats an absent -wal as fine, not as a failure"
    local dir="$SCRATCH/backup-absent"
    mkdir -p "$dir"
    printf 'db' > "$dir/wsm.db"

    if ! realtest_backup_database "$dir/wsm.db" 20260910-120000 >/dev/null 2>&1; then
        fail "$name" "a cleanly closed database with no -wal was reported as a failure"
        return
    fi
    if [ -e "$dir/wsm.db-wal.realtest-bak-20260910-120000" ]; then
        fail "$name" "a backup was invented for a file that does not exist"
        return
    fi
    pass "$name"
}

test_backup_refuses_a_missing_stamp() {
    local name="the backup helper refuses a call with no stamp"
    local dir="$SCRATCH/backup-nostamp"
    mkdir -p "$dir"
    printf 'db' > "$dir/wsm.db"

    local status=0
    realtest_backup_file "$dir/wsm.db" "" >/dev/null 2>&1 || status=$?
    if [ "$status" -eq 0 ]; then
        fail "$name" "a stampless backup was accepted, which would collide with every other run"
        return
    fi
    pass "$name"
}

test_backup_uses_a_clone() {
    local name="the backup helper asks cp for a clone (-c) rather than a plain copy"
    local dir="$SCRATCH/backup-clone-flag"
    mkdir -p "$dir"
    printf 'workspaces' > "$dir/wsm.db"
    local cpdir logfile out status=0
    cpdir="$(fake_cp_dir real)"
    logfile="$SCRATCH/cp-args-clone"
    : > "$logfile"

    out="$(PATH="$cpdir:$PATH" STUB_CP_LOG="$logfile" realtest_backup_database "$dir/wsm.db" 20260911-000000 2>&1)" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "the helper failed: $out"
        return
    fi
    if ! grep -q -- '-c' "$logfile"; then
        fail "$name" "cp was never invoked with -c: $(cat "$logfile")"
        return
    fi
    pass "$name"
}

test_clone_failure_falls_back_to_plain_copy() {
    local name="a clone (-c) failure falls back to a plain copy rather than failing the backup"
    local dir="$SCRATCH/backup-clone-fail"
    mkdir -p "$dir"
    printf 'the live database bytes' > "$dir/wsm.db"
    local cpdir out status=0
    cpdir="$(fake_cp_dir clone-fail)"

    out="$(PATH="$cpdir:$PATH" realtest_backup_database "$dir/wsm.db" 20260911-000000 2>&1)" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "the helper failed instead of falling back: $out"
        return
    fi
    if [ ! -f "$dir/wsm.db.realtest-bak-20260911-000000" ]; then
        fail "$name" "no backup was written after the fallback"
        return
    fi
    if [ "$(cat "$dir/wsm.db.realtest-bak-20260911-000000")" != "the live database bytes" ]; then
        fail "$name" "the fallback copy does not hold the source's bytes"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'falling back to a plain copy'; then
        fail "$name" "the fallback was not noted: $out"
        return
    fi
    pass "$name"
}

test_total_copy_failure_fails_the_backup() {
    local name="a total copy failure (clone and fallback both fail) still fails the backup"
    local dir="$SCRATCH/backup-total-fail"
    mkdir -p "$dir"
    printf 'db' > "$dir/wsm.db"
    local cpdir out status=0
    cpdir="$(fake_cp_dir total-fail)"

    out="$(PATH="$cpdir:$PATH" realtest_backup_database "$dir/wsm.db" 20260911-000000 2>&1)" || status=$?
    if [ "$status" -eq 0 ]; then
        fail "$name" "the helper succeeded despite every copy attempt failing"
        return
    fi
    if [ -e "$dir/wsm.db.realtest-bak-20260911-000000" ]; then
        fail "$name" "a backup file exists despite every copy attempt failing"
        return
    fi
    pass "$name"
}

test_prune_keeps_n_most_recent() {
    local name="pruning keeps only the N most recent backup sets"
    local dir="$SCRATCH/prune-keep"
    mkdir -p "$dir"
    printf 'db' > "$dir/wsm.db"
    local s
    for s in 20260911-000001 20260911-000002 20260911-000003 20260911-000004 20260911-000005; do
        realtest_backup_database "$dir/wsm.db" "$s" >/dev/null
    done

    realtest_prune_backups "$dir/wsm.db" 3 >/dev/null

    local kept=0 f
    for f in "$dir/wsm.db.realtest-bak-"*; do
        [ -e "$f" ] && kept=$((kept + 1))
    done
    if [ "$kept" -ne 3 ]; then
        fail "$name" "expected 3 sets kept, found $kept"
        return
    fi
    if [ ! -f "$dir/wsm.db.realtest-bak-20260911-000005" ] ||
       [ ! -f "$dir/wsm.db.realtest-bak-20260911-000004" ] ||
       [ ! -f "$dir/wsm.db.realtest-bak-20260911-000003" ]; then
        fail "$name" "the newest three sets were not the ones kept"
        return
    fi
    if [ -e "$dir/wsm.db.realtest-bak-20260911-000001" ] ||
       [ -e "$dir/wsm.db.realtest-bak-20260911-000002" ]; then
        fail "$name" "an older set survived pruning"
        return
    fi
    pass "$name"
}

test_prune_deletes_wal_and_shm_siblings() {
    local name="pruning an old set deletes its -wal and -shm siblings too"
    local dir="$SCRATCH/prune-siblings"
    mkdir -p "$dir"
    printf 'db' > "$dir/wsm.db"
    printf 'wal' > "$dir/wsm.db-wal"
    realtest_backup_database "$dir/wsm.db" 20260911-000001 >/dev/null
    rm -f "$dir/wsm.db-wal"
    realtest_backup_database "$dir/wsm.db" 20260911-000002 >/dev/null

    realtest_prune_backups "$dir/wsm.db" 1 >/dev/null

    if [ -e "$dir/wsm.db.realtest-bak-20260911-000001" ] || [ -e "$dir/wsm.db-wal.realtest-bak-20260911-000001" ]; then
        fail "$name" "the older set (db or its -wal sibling) survived pruning"
        return
    fi
    if [ ! -f "$dir/wsm.db.realtest-bak-20260911-000002" ]; then
        fail "$name" "the kept set was removed"
        return
    fi
    pass "$name"
}

test_prune_defaults_to_keeping_three() {
    local name="pruning with no keep count given keeps 3 (realtest.sh's default)"
    local dir="$SCRATCH/prune-default"
    mkdir -p "$dir"
    printf 'db' > "$dir/wsm.db"
    local s
    for s in 20260911-000001 20260911-000002 20260911-000003 20260911-000004; do
        realtest_backup_database "$dir/wsm.db" "$s" >/dev/null
    done

    realtest_prune_backups "$dir/wsm.db" "${AGENT_REPL_REALTEST_BACKUP_KEEP:-3}" >/dev/null

    local kept=0 f
    for f in "$dir/wsm.db.realtest-bak-"*; do
        [ -e "$f" ] && kept=$((kept + 1))
    done
    if [ "$kept" -ne 3 ]; then
        fail "$name" "expected 3 sets kept by default, found $kept"
        return
    fi
    pass "$name"
}

test_prune_returns_success_when_nothing_to_prune() {
    local name="pruning with KEEP-or-fewer sets returns success under set -e"
    local dir="$SCRATCH/prune-noop"
    mkdir -p "$dir"
    printf 'db' > "$dir/wsm.db"
    realtest_backup_database "$dir/wsm.db" 20260911-000001 >/dev/null
    # A single set with keep=3 prunes nothing; the loop's final command is the
    # index test (false). Run under `set -e` in a subshell exactly as
    # bin/realtest.sh does, and assert the caller is not aborted.
    if ( set -e; realtest_prune_backups "$dir/wsm.db" 3 >/dev/null; ); then
        pass "$name"
    else
        fail "$name" "realtest_prune_backups returned non-zero on a no-op prune, which aborts realtest.sh under set -e"
    fi
}

# ---- the script's refusals ------------------------------------------------

# scratch_bin CASE — a copy of realtest.sh beside stub siblings, so THIS_DIR
# resolves here and every external answer is the test's.
#
# The stubs answer READY and NOTHING RUNNING by default; each case overwrites
# the one it is about. suite-slot.sh records that it was reached, which is how a
# case distinguishes "declined" from "ran and failed".
scratch_bin() {
    local dir="$SCRATCH/$1/bin"
    mkdir -p "$dir"
    cp "$SCRIPT_UNDER_TEST" "$dir/realtest.sh"
    cp "$LIB_UNDER_TEST" "$dir/lib-realtest-backup.sh"

    cat > "$dir/readiness-report.sh" <<'STUB'
#!/usr/bin/env bash
cat "${STUB_READINESS_JSON:?the case must state a readiness document}"
STUB

    # THE SLOT STUB IS THE RUN LOG. It appends the test name of every `go test`
    # invocation the script makes, which is how a sequencing case sees WHICH
    # realtests ran and in what order rather than only that one did.
    #
    # It also creates the alive flag, because every realtest leaves an editor
    # running and the next cold-start realtest in a sweep has to face one.
    cat > "$dir/suite-slot.sh" <<'STUB'
#!/usr/bin/env bash
name=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        -run) name="$2"; shift ;;
    esac
    shift
done
printf '%s\n' "$name" >> "${STUB_SLOT_MARKER:?the case must state a marker path}"
held_name="${name#^}"
held_name="${held_name%$}"
printf '%s %s\n' "$held_name" "${AGENT_REPL_REALTEST_FOCUS_HELD:-unset}" \
    >> "${STUB_HELD_MARKER:?the case must state a focus-held marker path}"
[ -n "${STUB_ALIVE_FLAG:-}" ] && : > "$STUB_ALIVE_FLAG"
if [ -n "${STUB_SLOT_FAIL:-}" ] && printf '%s' "$name" | grep -q -- "$STUB_SLOT_FAIL"; then
    exit 1
fi
exit 0
STUB

    # THE EDITOR IS A FILE. It is answering exactly while STUB_ALIVE_FLAG
    # exists, so a quit really does end it and a later realtest really does
    # bring one back (the slot stub recreates the flag) — which is the whole of
    # what a sequencing case needs to see.
    cat > "$dir/emacsclient" <<'STUB'
#!/usr/bin/env bash
[ -f "${STUB_ALIVE_FLAG:?the case must state an alive flag path}" ] || exit 1
for arg in "$@"; do
    case "$arg" in
        '(kill-emacs)')
            printf 'killed\n' >> "${STUB_KILL_MARKER:?}"
            rm -f "$STUB_ALIVE_FLAG"
            ;;
        # THE EDITOR HAS A PID, because the handback's question — does the
        # editor this run leaves behind carry the guard — is a question about a
        # process, and STUB_PROCS is where a case answers it. The default pid
        # is in no case's process table, so an editor is guard-free unless the
        # case puts a line in for it.
        '(emacs-pid)')
            printf '%s\n' "${STUB_EMACS_PID:-9101}"
            exit 0
            ;;
    esac
done
printf 'nil\n'
STUB

    # THE LAUNCHER IS A LOG. `open -gj -a Emacs` is how the owner gets a
    # guard-free editor back, and a case asserts that it was reached and with
    # what environment — STUB_OPEN_GUARD records whether the guard survived
    # into the launch, which is the whole point of the `env -u`.
    cat > "$dir/open" <<'STUB'
#!/usr/bin/env bash
printf '%s guard=%s\n' "$*" "${AGENT_REPL_FORBID_VENDOR_CALLS:-unset}" \
    >> "${STUB_OPEN_MARKER:?the case must state an open marker path}"
[ "${STUB_OPEN_FAIL:-}" = "1" ] && exit 1
# The editor the launch brings up is answering, the same way the slot stub's is.
[ -n "${STUB_ALIVE_FLAG:-}" ] && : > "$STUB_ALIVE_FLAG"
exit 0
STUB

    # THE PROCESS TABLE IS THE TEST'S. pgrep and ps answer out of STUB_PROCS,
    # a file of "<pid> <command line>" lines where the command line carries
    # whatever environment the case wants the kernel to be holding — which is
    # the only way to assert a refusal about a process without running one.
    cat > "$dir/pgrep" <<'STUB'
#!/usr/bin/env bash
pattern=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        -f) ;;
        *) pattern="$1" ;;
    esac
    shift
done
[ -f "${STUB_PROCS:-}" ] || exit 1
found=0
while IFS= read -r line; do
    [ -n "$line" ] || continue
    if printf '%s' "${line#* }" | grep -Eq -- "$pattern"; then
        printf '%s\n' "${line%% *}"
        found=1
    fi
done < "$STUB_PROCS"
[ "$found" = 1 ]
STUB

    cat > "$dir/ps" <<'STUB'
#!/usr/bin/env bash
pid=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        -p) pid="$2"; shift ;;
    esac
    shift
done
[ -f "${STUB_PROCS:-}" ] || exit 1
while IFS= read -r line; do
    [ -n "$line" ] || continue
    if [ "${line%% *}" = "$pid" ]; then
        printf '%s\n' "${line#* }"
        exit 0
    fi
done < "$STUB_PROCS"
exit 1
STUB

    # THE SWEEP'S THREE HARNESS CHECKS ARE `go test` INVOCATIONS — the leftover
    # report at the start, the between-sweeps gap scan, the leftover clean and
    # the mark at the end — and they do not go through suite-slot.sh, so this
    # is where a case sees them and decides what they answer.
    #
    # It records "<test name> <leftover mode> <gap scan>" per invocation, which
    # is what lets a case assert not just THAT the check ran but which question
    # it asked; the two leftover calls differ only by their mode.
    cat > "$dir/go" <<'STUB'
#!/usr/bin/env bash
name=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        -run) name="$2"; shift ;;
    esac
    shift
done
# The anchors the script wraps the name in (`^Name$`) are stripped, so a case
# asserts on the test name rather than on the regexp spelling.
name="${name#^}"
name="${name%$}"
printf '%s %s %s %s\n' "$name" "${AGENT_REPL_REALTEST_LEFTOVERS:-none}" \
    "${AGENT_REPL_REALTEST_GAP_SCAN:-0}" "${AGENT_REPL_REALTEST_LEFTOVER_PREFIX:-none}" \
    >> "${STUB_GO_MARKER:?the case must state a go marker path}"
# THE SWEEP'S FOCUS GETS ITS OWN MARKER rather than a fifth field above: the
# cases that assert the leftover clean anchor their grep on the end of that
# line, and widening it would rewrite tests that are about something else.
if [ -n "${AGENT_REPL_REALTEST_FOCUS:-}" ]; then
    printf '%s %s\n' "$AGENT_REPL_REALTEST_FOCUS" "$name" >> "${STUB_FOCUS_MARKER:?the case must state a focus marker path}"
    if [ "${STUB_FOCUS_TAKE_FAIL:-}" = "1" ] && [ "$AGENT_REPL_REALTEST_FOCUS" = "take" ]; then
        printf 'build the key helper to take the sweep FAILED\n'
        exit 1
    fi
fi
# THE ORDERLY DAEMON STOP IS A `go test` TOO, and this is where a case decides
# whether the daemon answers its own door. Three worlds:
#
#   default                    the door is answered: the daemon stands its
#                              sessions down and EXITS ITSELF, so it leaves the
#                              process table without anything signalling it.
#   STUB_ORDERLY_NO_ANSWER=1   nothing answers the door, which is the run's cue
#                              to say so and fall back to SIGTERM.
#   STUB_ORDERLY_IGNORED=1     the door is answered and the daemon stays, which
#                              is a skip and never an escalation.
if [ "${AGENT_REPL_REALTEST_DAEMON_STOP:-}" = "1" ]; then
    printf '%s\n' "$name" >> "${STUB_ORDERLY_MARKER:?the case must state an orderly-stop marker path}"
    if [ "${STUB_ORDERLY_NO_ANSWER:-}" = "1" ]; then
        printf 'the daemon did not accept an orderly stop: dial tcp 127.0.0.1:1: connect: connection refused\n'
        exit 1
    fi
    if [ -f "${STUB_PROCS:-}" ] && [ "${STUB_ORDERLY_IGNORED:-}" != "1" ]; then
        grep -v 'claude-repld' "$STUB_PROCS" > "$STUB_PROCS.next" || true
        mv "$STUB_PROCS.next" "$STUB_PROCS"
    fi
    exit 0
fi
if [ -n "${STUB_LEFTOVERS_FAIL:-}" ] && [ "${AGENT_REPL_REALTEST_LEFTOVERS:-}" = "$STUB_LEFTOVERS_FAIL" ]; then
    printf 'REALTEST LEFTOVER WORKSPACES\n  ws-c22fed997b234b27 rt-8 (closed, MISSING) /leftover/dir\n'
    exit 1
fi
if [ -n "${STUB_GAPSCAN_FAIL:-}" ] && [ "${AGENT_REPL_REALTEST_GAP_SCAN:-}" = "1" ]; then
    printf 'BETWEEN-SWEEPS FINDINGS: 3 finding(s)\n'
    exit 1
fi
exit 0
STUB

    # STUB_CP_MODE lets a case make every copy fail (total-fail) without
    # touching the other stubs; unset or any other value passes through to
    # the real cp untouched.
    cat > "$dir/cp" <<'STUB'
#!/usr/bin/env bash
case "${STUB_CP_MODE:-}" in
    total-fail) exit 1 ;;
esac
exec /bin/cp "$@"
STUB

    # THE KILL IS THE TEST'S TOO. `kill` is a shell builtin, so a PATH stub
    # would never be reached; realtest.sh calls AGENT_REPL_REALTEST_KILL for
    # the same reason it calls AGENT_REPL_REALTEST_EMACSCLIENT. This one logs
    # the signalled pids and removes them from the process table, which is what
    # a daemon exiting looks like from pgrep's side.
    cat > "$dir/kill" <<'STUB'
#!/usr/bin/env bash
while [ "$#" -gt 0 ]; do
    case "$1" in
        -*) ;;
        *)
            printf '%s\n' "$1" >> "${STUB_KILL_LOG:?the case must state a kill log}"
            if [ -f "${STUB_PROCS:-}" ] && [ "${STUB_KILL_IGNORED:-}" != "1" ]; then
                grep -v "^$1 " "$STUB_PROCS" > "$STUB_PROCS.next" || true
                mv "$STUB_PROCS.next" "$STUB_PROCS"
            fi
            ;;
    esac
    shift
done
exit 0
STUB

    chmod +x "$dir"/*.sh "$dir/emacsclient" "$dir/pgrep" "$dir/ps" "$dir/cp" "$dir/kill" "$dir/go" "$dir/open"
    printf '%s' "$dir"
}

ready_json() {
    cat <<'JSON'
{"systems": [{"name": "daemon", "ready": true}, {"name": "shim", "ready": true}]}
JSON
}

stale_json() {
    cat <<'JSON'
{"systems": [{"name": "daemon", "ready": true},
             {"name": "shim", "ready": false,
              "error": "the artifact is built from source revision aaa, but the checkout is at bbb"}]}
JSON
}

# run_script DIR [ENV=VALUE ...] — run the copied script with the stub
# environment, capturing output and status. Every path the script would
# otherwise reach on the real machine is redirected into the scratch tree.
#
# SCRIPT_ARGS is what the script itself is called with (the realtest selectors
# and any go-test flags); the arguments to run_script are environment
# assignments prefixed to it.
run_script() {
    local dir="$1"
    shift
    set +e
    HOME="$SCRATCH/home" \
    PATH="$dir:$PATH" \
    STUB_PROCS="${STUB_PROCS:-$SCRATCH/procs}" \
    STUB_READINESS_JSON="$SCRATCH/readiness.json" \
    STUB_SLOT_MARKER="$SCRATCH/slot-reached" \
    STUB_GO_MARKER="${STUB_GO_MARKER:-$SCRATCH/go-reached}" \
    STUB_ORDERLY_MARKER="${STUB_ORDERLY_MARKER:-$SCRATCH/orderly-reached}" \
    STUB_FOCUS_MARKER="${STUB_FOCUS_MARKER:-$SCRATCH/focus-reached}" \
    STUB_HELD_MARKER="${STUB_HELD_MARKER:-$SCRATCH/focus-held}" \
    STUB_KILL_MARKER="${STUB_KILL_MARKER:-$SCRATCH/killed}" \
    STUB_KILL_LOG="${STUB_KILL_LOG:-$SCRATCH/kill-log}" \
    STUB_ALIVE_FLAG="${STUB_ALIVE_FLAG:-$SCRATCH/alive}" \
    AGENT_REPL_REALTEST_EMACSCLIENT="$dir/emacsclient" \
    AGENT_REPL_REALTEST_KILL="$dir/kill" \
    AGENT_REPL_REALTEST_OPEN="$dir/open" \
    STUB_OPEN_MARKER="${STUB_OPEN_MARKER:-$SCRATCH/open-reached}" \
    AGENT_REPL_REALTEST_OUT="$SCRATCH/out" \
    "$@" bash "$dir/realtest.sh" ${SCRIPT_ARGS[@]+"${SCRIPT_ARGS[@]}"} 2>&1
    local status=$?
    set -e
    return "$status"
}

prepare_home() {
    SCRIPT_ARGS=()
    rm -rf "${SCRATCH:?}/home" "${SCRATCH:?}/out" "${SCRATCH:?}/slot-reached" "${SCRATCH:?}/killed" \
        "${SCRATCH:?}/alive" "${SCRATCH:?}/kill-log" "${SCRATCH:?}/go-reached" \
        "${SCRATCH:?}/focus-reached" "${SCRATCH:?}/focus-held" "${SCRATCH:?}/open-reached" \
        "${SCRATCH:?}/orderly-reached"
    : > "$SCRATCH/procs"
    mkdir -p "$SCRATCH/home/.claude-emacs" "$SCRATCH/home/.cache/agent-repl/store"
    printf 'workspaces' > "$SCRATCH/home/.claude-emacs/wsm.db"
    printf 'events' > "$SCRATCH/home/.cache/agent-repl/store/events.db"
}

# standing_emacs — an editor is answering the socket when the script starts.
standing_emacs() { : > "$SCRATCH/alive"; }

# guarded_daemon_line PID DIR — a daemon of this checkout, carrying the vendor
# guard, in the stub process table.
guarded_daemon_line() {
    printf '%s %s/daemon/bin/claude-repld --state-dir /tmp AGENT_REPL_FORBID_VENDOR_CALLS=1\n' \
        "$1" "$(cd "$2/.." && pwd)"
}

# The refusal hands the owner the one command that clears it: the daemon owns
# deploys, so the remedy is its `deploy` subcommand.
test_declining_names_the_daemon_deploy_remedy() {
    local name="the not-deployed refusal names claude-repld deploy as the remedy"
    local dir out status=0
    dir="$(scratch_bin not-deployed-remedy)"
    prepare_home
    stale_json > "$SCRATCH/readiness.json"

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -qF 'run daemon/bin/claude-repld deploy, then try again'; then
        fail "$name" "the refusal does not name the deploy remedy: $out"
        return
    fi
    pass "$name"
}

test_declines_when_a_system_is_not_deployed() {
    local name="the script DECLINES when a deployed system is not at this checkout"
    local dir out status=0
    dir="$(scratch_bin not-deployed)"
    prepare_home
    stale_json > "$SCRATCH/readiness.json"

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'these systems are not at this checkout'; then
        fail "$name" "the refusal does not name the problem: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'shim'; then
        fail "$name" "the refusal does not name the stale system: $out"
        return
    fi
    if [ -f "$SCRATCH/slot-reached" ]; then
        fail "$name" "the run was reached despite the refusal"
        return
    fi
    pass "$name"
}

test_declines_when_emacs_is_running_without_a_takeover() {
    local name="the script DECLINES when an Emacs is running and no takeover was authorized"
    local dir out status=0
    dir="$(scratch_bin no-takeover)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"

    standing_emacs
    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'AGENT_REPL_REALTEST_TAKEOVER=1'; then
        fail "$name" "the refusal does not say how to authorize the takeover: $out"
        return
    fi
    if [ -f "$SCRATCH/killed" ]; then
        fail "$name" "the owner's editor was quit without authorization"
        return
    fi
    if [ -f "$SCRATCH/slot-reached" ]; then
        fail "$name" "the run was reached despite the refusal"
        return
    fi
    pass "$name"
}

test_backs_up_before_refusing_the_takeover() {
    local name="the script backs up the owner's state BEFORE it refuses a takeover"
    local dir status=0
    dir="$(scratch_bin backup-first)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"

    standing_emacs
    run_script "$dir" >/dev/null || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED"
        return
    fi
    # The copy is what makes the refusal recoverable: the operator who then sets
    # the takeover flag is running against state that already has a copy.
    if ! ls "$SCRATCH/home/.claude-emacs/wsm.db.realtest-bak-"* >/dev/null 2>&1; then
        fail "$name" "no workspace-state backup was taken before the refusal"
        return
    fi
    # THE STORE IS NOT COPIED. It is a cache that needs no retention during
    # development (owner ruling 2026-09-13), and its per-run clones cost
    # gigabytes for a file every byte of which is re-derivable.
    if ls "$SCRATCH/home/.cache/agent-repl/store/events.db.realtest-bak-"* >/dev/null 2>&1; then
        fail "$name" "the store was copied aside; it is a cache and needs no backup"
        return
    fi
    pass "$name"
}

test_runs_when_nothing_stands_in_the_way() {
    local name="the script runs the realtests when everything is deployed and no Emacs is up"
    local dir out status=0
    dir="$(scratch_bin clean)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"

    # ONE REALTEST, because this case is about the preflight and not about a
    # sweep's sequencing: with a single cold-start realtest and no editor
    # standing, no consent is in play at all.
    SCRIPT_ARGS=(1)
    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ ! -f "$SCRATCH/slot-reached" ]; then
        fail "$name" "the run was never reached; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'the vendor is forbidden for this run'; then
        fail "$name" "the run does not state that the vendor is forbidden: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q "which editor the owner is left with is settled at the end"; then
        fail "$name" "the run does not say the editor it leaves behind is settled at the end: $out"
        return
    fi
    pass "$name"
}

test_declines_when_the_backup_copy_totally_fails() {
    local name="the script DECLINES when the state backup cannot be copied at all"
    local dir out status=0
    dir="$(scratch_bin backup-copy-fails)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"

    out="$(STUB_CP_MODE=total-fail run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'could not be backed up'; then
        fail "$name" "the refusal does not say the backup failed: $out"
        return
    fi
    if [ -f "$SCRATCH/slot-reached" ]; then
        fail "$name" "the run was reached despite the backup failure"
        return
    fi
    pass "$name"
}

test_records_the_deployed_revisions() {
    local name="the script records the deployed revisions it measured"
    local dir status=0
    dir="$(scratch_bin stamps)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1)

    run_script "$dir" >/dev/null || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0"
        return
    fi
    if [ ! -f "$SCRATCH/out/readiness.json" ]; then
        fail "$name" "the run directory holds no readiness document, so a finding cannot be tied to a build"
        return
    fi
    pass "$name"
}

test_declines_when_a_listening_shim_lacks_the_guard() {
    local name="the script DECLINES when a listening shim lacks the vendor guard"
    local dir out status=0
    dir="$(scratch_bin unguarded-shim)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    # A shim left behind by an earlier, unguarded daemon. The daemon this run
    # brings up would ADOPT it, so the guard on the daemon never reaches it.
    printf '94292 node /opt/agent-shim/claude/shim/dist/main.js --listen %s/.claude-emacs/sock/0100059cb65649bc.n1.sock PATH=/usr/bin\n' \
        "$SCRATCH/home" > "$SCRATCH/procs"

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q '94292'; then
        fail "$name" "the refusal does not name the pid to stop: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q '0100059cb65649bc.n1.sock'; then
        fail "$name" "the refusal does not name the socket the shim is listening on: $out"
        return
    fi
    if [ -f "$SCRATCH/slot-reached" ]; then
        fail "$name" "the run was reached despite the refusal"
        return
    fi
    pass "$name"
}

# The shim scan looks under the OWNER's state root whatever the caller's shell
# says. An agent session inherits AGENT_REPL_STATE_DIR from the daemon that
# spawned it; honoring it here once pointed the scan at another directory, and
# an unguarded shim under the owner's root went unseen.
test_declines_on_an_owner_shim_whatever_the_callers_state_dir() {
    local name="the unguarded-shim scan ignores the caller's AGENT_REPL_STATE_DIR"
    local dir out status=0
    dir="$(scratch_bin caller-state-dir)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    printf '94292 node /opt/agent-shim/claude/shim/dist/main.js --listen %s/.claude-emacs/sock/0100059cb65649bc.n1.sock PATH=/usr/bin\n' \
        "$SCRATCH/home" > "$SCRATCH/procs"

    out="$(run_script "$dir" env AGENT_REPL_STATE_DIR="$SCRATCH/elsewhere")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q '94292'; then
        fail "$name" "the refusal does not name the owner's unguarded shim: $out"
        return
    fi
    pass "$name"
}

test_runs_when_every_listening_shim_carries_the_guard() {
    local name="the script runs when a listening shim carries the vendor guard"
    local dir out status=0
    dir="$(scratch_bin guarded-shim)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    printf '94292 node /opt/agent-shim/claude/shim/dist/main.js --listen %s/.claude-emacs/sock/aaaa.n1.sock AGENT_REPL_FORBID_VENDOR_CALLS=1\n' \
        "$SCRATCH/home" > "$SCRATCH/procs"
    SCRIPT_ARGS=(1)

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ ! -f "$SCRATCH/slot-reached" ]; then
        fail "$name" "the run was never reached; output: $out"
        return
    fi
    pass "$name"
}

test_ignores_a_shim_listening_under_another_state_directory() {
    local name="the script ignores an unguarded shim listening outside this state directory"
    local dir out status=0
    dir="$(scratch_bin foreign-shim)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    # Another checkout's shim is not a process this run would adopt, and
    # declining on it would send the operator after the wrong thing.
    printf '77001 node /opt/agent-shim/claude/shim/dist/main.js --listen /var/other-state/sock/bbbb.n1.sock PATH=/usr/bin\n' \
        > "$SCRATCH/procs"
    SCRIPT_ARGS=(1)

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ ! -f "$SCRATCH/slot-reached" ]; then
        fail "$name" "the run was never reached; output: $out"
        return
    fi
    pass "$name"
}

test_declines_when_a_shim_lock_lacks_the_guard() {
    local name="the script DECLINES when a running shim-lock lacks the vendor guard"
    local dir out status=0
    dir="$(scratch_bin unguarded-lock)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    # shim-lock inherits the shim's environment, so it tells the same story
    # about the shim that spawned it.
    printf '81003 /Users/someone/.cache/agent-repl/bin/shim-lock --hold /tmp/lock PATH=/usr/bin\n' > "$SCRATCH/procs"

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'shim-lock pid 81003'; then
        fail "$name" "the refusal does not name the shim-lock pid: $out"
        return
    fi
    pass "$name"
}

test_declines_when_the_daemon_lacks_the_guard() {
    local name="the script DECLINES when the running daemon lacks the vendor guard"
    local dir out status=0
    dir="$(scratch_bin unguarded-daemon)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    # The daemon is matched by its path under the module root, which is the
    # copied script's own parent in this arrangement.
    printf '55501 %s/daemon/bin/claude-repld --state-dir /tmp PATH=/usr/bin\n' \
        "$(cd "$dir/.." && pwd)" > "$SCRATCH/procs"

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q '55501'; then
        fail "$name" "the refusal does not name the daemon pid to stop: $out"
        return
    fi
    pass "$name"
}

# unguarded_daemon_line PID DIR — a daemon of this checkout whose kernel
# environment does NOT carry the vendor guard. This is what a sweep finds on
# the machine every time, because the previous sweep's handback deliberately
# left the owner a guard-free one.
unguarded_daemon_line() {
    printf '%s %s/daemon/bin/claude-repld --state-dir /tmp PATH=/usr/bin\n' \
        "$1" "$(cd "$2/.." && pwd)"
}

test_the_unguarded_daemon_refusal_names_the_consent() {
    local name="the refusal over an unguarded daemon names AGENT_REPL_REALTEST_STOP_DAEMON=1 as the remedy"
    local dir out status=0
    dir="$(scratch_bin unguarded-daemon-remedy)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    unguarded_daemon_line 55501 "$dir" > "$SCRATCH/procs"

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'AGENT_REPL_REALTEST_STOP_DAEMON=1'; then
        fail "$name" "the refusal does not name the consent that resolves it: $out"
        return
    fi
    pass "$name"
}

test_the_unguarded_daemon_is_stopped_under_the_consent() {
    local name="an unguarded daemon is STOPPED under AGENT_REPL_REALTEST_STOP_DAEMON=1 and the run continues"
    local dir out status=0
    dir="$(scratch_bin unguarded-daemon-stopped)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    unguarded_daemon_line 55501 "$dir" > "$SCRATCH/procs"
    SCRIPT_ARGS=(1)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 AGENT_REPL_REALTEST_STOP_DAEMON=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if ! grep -q '^TestOrderlyDaemonStop$' "$SCRATCH/orderly-reached" 2>/dev/null; then
        fail "$name" "the daemon was not asked to stop through its own door: $(cat "$SCRATCH/orderly-reached" 2>/dev/null)"
        return
    fi
    if [ -s "$SCRATCH/kill-log" ]; then
        fail "$name" "a daemon that answered its own door was signalled anyway: $(cat "$SCRATCH/kill-log")"
        return
    fi
    if ! printf '%s' "$out" | grep -q "the owner's unguarded daemon pid 55501 was stopped under AGENT_REPL_REALTEST_STOP_DAEMON"; then
        fail "$name" "the run did not state that it stopped the owner's unguarded daemon: $out"
        return
    fi
    if ! grep -q 'TestRealtestStartTheEditor' "$SCRATCH/slot-reached"; then
        fail "$name" "the realtest did not run once the unguarded daemon was stopped"
        return
    fi
    pass "$name"
}

test_the_editor_is_quit_before_the_unguarded_daemon_is_stopped() {
    local name="the standing editor is QUIT BEFORE the unguarded daemon is stopped"
    local dir out status=0
    dir="$(scratch_bin unguarded-daemon-order)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    unguarded_daemon_line 55501 "$dir" > "$SCRATCH/procs"
    standing_emacs
    SCRIPT_ARGS=(1)

    # The two stubs write to the same log, so the ORDER of the two lines is the
    # assertion: an editor left standing while the daemon goes brings an
    # unguarded daemon straight back up.
    out="$(STUB_KILL_MARKER="$SCRATCH/order-log" STUB_KILL_LOG="$SCRATCH/order-log" \
        STUB_ORDERLY_MARKER="$SCRATCH/order-log" \
        AGENT_REPL_REALTEST_TAKEOVER=1 AGENT_REPL_REALTEST_STOP_DAEMON=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ "$(head -n1 "$SCRATCH/order-log")" != "killed" ]; then
        fail "$name" "the editor quit was not first; log: $(cat "$SCRATCH/order-log")"
        return
    fi
    if ! grep -q '^TestOrderlyDaemonStop$' "$SCRATCH/order-log"; then
        fail "$name" "the unguarded daemon was never stopped; log: $(cat "$SCRATCH/order-log")"
        return
    fi
    pass "$name"
}

# ---- the shims the stopped daemon leaves listening -------------------------
#
# A DAEMON'S SHIMS OUTLIVE IT. Stopping the unguarded daemon does not take them
# with it, and the run's own daemon adopts whatever is still listening, so the
# consent that stops the daemon has to stand its sessions down too. Without
# that, a sweep quit the owner's editor, stopped their daemon and then declined
# over the shims, leaving nothing standing (owner complaint, 2026-09-13 15:2x).

# unguarded_shim_line PID SOCKET — a shim of THIS state directory listening
# without the vendor guard.
unguarded_shim_line() {
    printf '%s node /opt/agent-shim/claude/shim/dist/main.js --listen %s/.claude-emacs/sock/%s PATH=/usr/bin\n' \
        "$1" "$SCRATCH/home" "$2"
}

test_the_unguarded_shim_refusal_names_the_consent() {
    local name="the refusal over unguarded shims names AGENT_REPL_REALTEST_STOP_DAEMON=1 as the remedy"
    local dir out status=0
    dir="$(scratch_bin unguarded-shim-remedy)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    unguarded_shim_line 39689 0100059cb65649bc.n1.sock > "$SCRATCH/procs"

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'THE REMEDY IS AGENT_REPL_REALTEST_STOP_DAEMON=1'; then
        fail "$name" "the refusal does not name the consent that resolves it: $out"
        return
    fi
    pass "$name"
}

test_the_unguarded_shims_are_stopped_under_the_consent() {
    local name="unguarded shims are STOPPED under AGENT_REPL_REALTEST_STOP_DAEMON=1 and the run continues"
    local dir out status=0
    dir="$(scratch_bin unguarded-shim-stopped)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    {
        unguarded_shim_line 39689 0100059cb65649bc.n1.sock
        printf '39736 /Users/someone/.cache/agent-repl/bin/shim-lock --hold /tmp/lock PATH=/usr/bin\n'
    } > "$SCRATCH/procs"
    SCRIPT_ARGS=(1)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 AGENT_REPL_REALTEST_STOP_DAEMON=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if ! grep -q '^39689$' "$SCRATCH/kill-log"; then
        fail "$name" "the unguarded shim was never signalled: $(cat "$SCRATCH/kill-log")"
        return
    fi
    if ! grep -q '^39736$' "$SCRATCH/kill-log"; then
        fail "$name" "the unguarded shim-lock was never signalled: $(cat "$SCRATCH/kill-log")"
        return
    fi
    if ! printf '%s' "$out" | grep -q "the owner's unguarded shim pid 39689 listening on .*0100059cb65649bc.n1.sock was stopped under AGENT_REPL_REALTEST_STOP_DAEMON"; then
        fail "$name" "the run did not state the shim it stopped: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q "the owner's unguarded shim-lock pid 39736 was stopped under AGENT_REPL_REALTEST_STOP_DAEMON"; then
        fail "$name" "the run did not state the shim-lock it stopped: $out"
        return
    fi
    if ! grep -q 'TestRealtestStartTheEditor' "$SCRATCH/slot-reached"; then
        fail "$name" "the realtest did not run once the unguarded shims were stopped"
        return
    fi
    pass "$name"
}

test_a_preflight_decline_after_a_quit_still_hands_an_editor_back() {
    local name="a run that quit the owner's editor in the preflight and then DECLINED still launches a guard-free one"
    local dir out status=0
    dir="$(scratch_bin preflight-decline-handback)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    # The consent is given, so the editor is quit and the daemon asked to stop;
    # the daemon then goes nowhere — through either route — which is a decline
    # with the desktop already empty, the exact shape that left the owner with
    # nothing standing.
    unguarded_daemon_line 55501 "$dir" > "$SCRATCH/procs"
    standing_emacs
    SCRIPT_ARGS=(1)

    out="$(STUB_ORDERLY_IGNORED=1 STUB_KILL_IGNORED=1 AGENT_REPL_REALTEST_TAKEOVER=1 AGENT_REPL_REALTEST_STOP_DAEMON=1 \
        AGENT_REPL_REALTEST_DAEMON_STOP_SECONDS=1 run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if [ ! -f "$SCRATCH/open-reached" ]; then
        fail "$name" "the run quit the owner's editor, declined, and launched nothing; output: $out"
        return
    fi
    if ! grep -q -- '-gj -a Emacs guard=unset' "$SCRATCH/open-reached"; then
        fail "$name" "the editor handed back was not a guard-free cold start: $(cat "$SCRATCH/open-reached")"
        return
    fi
    pass "$name"
}

# ---- the sweep: one world per realtest ------------------------------------

test_unknown_selector_declines_before_anything() {
    local name="an unknown realtest selector DECLINES, and before the state is touched"
    local dir out status=0
    dir="$(scratch_bin bad-selector)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    # 99 rather than a number just past the last authored realtest: the
    # world table grows one row per realtest and a fixture spelled "the next
    # one" stops testing an unknown selector the day that realtest lands,
    # which is exactly what 9 did when realtest 9 was authored. The plan
    # holds 24 realtests and will never hold 99.
    SCRIPT_ARGS=(99)

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'TestRealtestStartTheEditor'; then
        fail "$name" "the refusal does not list the realtests that do exist: $out"
        return
    fi
    # A typo must cost nothing: no readiness report, no clone of an 11GB
    # database, no processes looked at.
    if ls "$SCRATCH/home/.claude-emacs/wsm.db.realtest-bak-"* >/dev/null 2>&1; then
        fail "$name" "the state was backed up for a run that could never start"
        return
    fi
    pass "$name"
}

test_run_pattern_matching_nothing_declines() {
    local name="a -run pattern that matches no realtest DECLINES"
    local dir out status=0
    dir="$(scratch_bin bad-run)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(-run TestRealtestThereIsNoSuchThing)

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'matches no realtest'; then
        fail "$name" "the refusal does not say the pattern matched nothing: $out"
        return
    fi
    pass "$name"
}

test_run_pattern_selects_one_realtest() {
    local name="-run <name> runs exactly that realtest"
    local dir out status=0
    dir="$(scratch_bin run-one)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(-run TestRealtestPriorityCloseReopenKill)

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ "$(cat "$SCRATCH/slot-reached")" != '^TestRealtestPriorityCloseReopenKill$' ]; then
        fail "$name" "the invocations were: $(cat "$SCRATCH/slot-reached")"
        return
    fi
    pass "$name"
}

test_a_sweep_runs_one_invocation_per_realtest_in_order() {
    local name="a sweep runs one go test invocation per realtest, in the order asked for"
    local dir out status=0
    dir="$(scratch_bin sweep-order)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(5 1 7)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    local want='^TestRealtestCreateWorkDeleteAWorkspace$
^TestRealtestStartTheEditor$
^TestRealtestForkAWorkspace$'
    if [ "$(cat "$SCRATCH/slot-reached")" != "$want" ]; then
        fail "$name" "the invocations were: $(cat "$SCRATCH/slot-reached")"
        return
    fi
    pass "$name"
}

test_a_sweep_quits_the_editor_between_cold_starts() {
    local name="a sweep quits the editor each realtest leaves behind, so the next cold start gets its world"
    local dir out status=0
    dir="$(scratch_bin sweep-quits)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    # Three cold-start realtests and no editor standing: the first needs no
    # quit, and each of the other two faces the editor its predecessor left.
    SCRIPT_ARGS=(1 5 6)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    local quits=0
    [ -f "$SCRATCH/killed" ] && quits="$(grep -c killed "$SCRATCH/killed")"
    if [ "$quits" -ne 2 ]; then
        fail "$name" "the editor was quit $quits time(s), want 2"
        return
    fi
    pass "$name"
}

test_a_sweep_backs_up_once() {
    local name="a sweep backs up the owner's state ONCE, not once per realtest"
    local dir status=0
    dir="$(scratch_bin sweep-backup)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1 5 6 7)

    AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir" >/dev/null || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0"
        return
    fi
    local copies=0 f
    for f in "$SCRATCH/home/.claude-emacs/wsm.db.realtest-bak-"*; do
        [ -e "$f" ] && copies=$((copies + 1))
    done
    if [ "$copies" -ne 1 ]; then
        fail "$name" "$copies workspace-state backups were taken; the backup captures the state BEFORE the run and must be taken once"
        return
    fi
    pass "$name"
}

test_declines_a_sweep_that_would_quit_a_standing_editor() {
    local name="a sweep DECLINES before anything runs when it would quit a standing editor without consent"
    local dir out status=0
    dir="$(scratch_bin sweep-no-consent)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    standing_emacs
    SCRIPT_ARGS=(1 5)

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'quits Emacs 2 time(s)'; then
        fail "$name" "the refusal does not say how many quits the consent would authorize: $out"
        return
    fi
    if [ -f "$SCRATCH/slot-reached" ]; then
        fail "$name" "a realtest ran despite the refusal"
        return
    fi
    if [ -f "$SCRATCH/killed" ]; then
        fail "$name" "the owner's editor was quit without authorization"
        return
    fi
    pass "$name"
}

test_realtest_3_is_skipped_without_the_daemon_consent() {
    local name="realtest 3 is SKIPPED with a reason when a daemon is running and no stop consent was given"
    local dir out status=0
    dir="$(scratch_bin rt3-no-consent)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_daemon_line 55501 "$dir" > "$SCRATCH/procs"
    SCRIPT_ARGS=(1 3)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_INCOMPLETE" ]; then
        fail "$name" "exit was $status, want $EXIT_INCOMPLETE; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'AGENT_REPL_REALTEST_STOP_DAEMON=1'; then
        fail "$name" "the skip does not say what consent would let it run: $out"
        return
    fi
    if grep -q 'TestRealtestStartWithTheDaemonDown' "$SCRATCH/slot-reached"; then
        fail "$name" "realtest 3 was run into its own refusal instead of being skipped"
        return
    fi
    if ! grep -q 'TestRealtestStartTheEditor' "$SCRATCH/slot-reached"; then
        fail "$name" "the realtest that COULD run did not: $(cat "$SCRATCH/slot-reached")"
        return
    fi
    if [ -f "$SCRATCH/kill-log" ] && [ -s "$SCRATCH/kill-log" ]; then
        fail "$name" "the daemon was signalled without the consent: $(cat "$SCRATCH/kill-log")"
        return
    fi
    if [ -f "$SCRATCH/orderly-reached" ] && [ -s "$SCRATCH/orderly-reached" ]; then
        fail "$name" "the daemon was asked to stop without the consent: $(cat "$SCRATCH/orderly-reached")"
        return
    fi
    pass "$name"
}

test_realtest_3_stops_the_daemon_under_its_own_consent() {
    local name="realtest 3 runs, and the daemon is stopped through its own door, under AGENT_REPL_REALTEST_STOP_DAEMON=1"
    local dir out status=0
    dir="$(scratch_bin rt3-consent)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_daemon_line 55501 "$dir" > "$SCRATCH/procs"
    SCRIPT_ARGS=(3)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 AGENT_REPL_REALTEST_STOP_DAEMON=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if ! grep -q '^TestOrderlyDaemonStop$' "$SCRATCH/orderly-reached" 2>/dev/null; then
        fail "$name" "the daemon was not asked to stop through its own door: $(cat "$SCRATCH/orderly-reached" 2>/dev/null)"
        return
    fi
    if [ -s "$SCRATCH/kill-log" ]; then
        fail "$name" "a daemon that answered its own door was signalled anyway: $(cat "$SCRATCH/kill-log")"
        return
    fi
    if ! grep -q 'TestRealtestStartWithTheDaemonDown' "$SCRATCH/slot-reached"; then
        fail "$name" "realtest 3 did not run once its world was established"
        return
    fi
    pass "$name"
}

test_a_daemon_that_ignores_sigterm_is_not_escalated() {
    local name="a daemon that survives the stop makes realtest 3 a SKIP, never a SIGKILL"
    local dir out status=0
    dir="$(scratch_bin rt3-stubborn)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_daemon_line 55501 "$dir" > "$SCRATCH/procs"
    SCRIPT_ARGS=(3)

    # STUB_ORDERLY_IGNORED: the door is answered and the process stays.
    # STUB_KILL_IGNORED covers the fallback for the same daemon, so neither
    # route can be the one that quietly removed it.
    out="$(STUB_ORDERLY_IGNORED=1 STUB_KILL_IGNORED=1 AGENT_REPL_REALTEST_TAKEOVER=1 AGENT_REPL_REALTEST_STOP_DAEMON=1 \
        AGENT_REPL_REALTEST_DAEMON_STOP_SECONDS=1 run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED (nothing ran); output: $out"
        return
    fi
    # A daemon that ANSWERED its door is never signalled at all, so the kill
    # log may legitimately not exist; what must never appear is an escalation.
    if [ -f "$SCRATCH/kill-log" ] && grep -q -- '-KILL\|-9' "$SCRATCH/kill-log"; then
        fail "$name" "the owner's daemon was escalated to SIGKILL: $(cat "$SCRATCH/kill-log")"
        return
    fi
    if [ -f "$SCRATCH/slot-reached" ]; then
        fail "$name" "realtest 3 ran with a daemon still up"
        return
    fi
    pass "$name"
}

# ---- which door the stop goes through --------------------------------------
#
# A DAEMON STOPPED BY SIGNAL LEAVES ITS SESSIONS BEHIND. SIGTERM cancels the
# daemon's serving context and nothing else, so every shim it held survives it
# and the next daemon reports each one as an unaccounted-for bounce
# (`daemon.rollout.reconcile`, four times in the harvest of 2026-09-13). The
# stop therefore goes through the daemon's own door — the
# `UpdateShutdownSchedule{now}` the editor's own stop sends — and the signal is
# the fallback, taken only when nothing answers and always said out loud.

test_the_stop_goes_through_the_daemons_own_door() {
    local name="a daemon that answers is stopped through its own door, and no signal is sent"
    local dir out status=0
    dir="$(scratch_bin rt3-orderly-door)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_daemon_line 55501 "$dir" > "$SCRATCH/procs"
    SCRIPT_ARGS=(3)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 AGENT_REPL_REALTEST_STOP_DAEMON=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if ! grep -q '^TestOrderlyDaemonStop$' "$SCRATCH/orderly-reached" 2>/dev/null; then
        fail "$name" "the daemon was never asked through its own door: $(cat "$SCRATCH/orderly-reached" 2>/dev/null)"
        return
    fi
    if [ -s "$SCRATCH/kill-log" ]; then
        fail "$name" "the daemon answered its door and was signalled anyway: $(cat "$SCRATCH/kill-log")"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'UpdateShutdownSchedule{now}'; then
        fail "$name" "the run does not name the door it stopped the daemon through: $out"
        return
    fi
    pass "$name"
}

test_the_sigterm_fallback_is_stated_when_the_door_is_unanswered() {
    local name="a daemon that does not answer its door is SIGTERMed, and the run says why"
    local dir out status=0
    dir="$(scratch_bin rt3-orderly-unanswered)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_daemon_line 55501 "$dir" > "$SCRATCH/procs"
    SCRIPT_ARGS=(3)

    out="$(STUB_ORDERLY_NO_ANSWER=1 AGENT_REPL_REALTEST_TAKEOVER=1 AGENT_REPL_REALTEST_STOP_DAEMON=1 \
        run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ "$(cat "$SCRATCH/kill-log")" != "55501" ]; then
        fail "$name" "the unanswered daemon was not signalled: $(cat "$SCRATCH/kill-log" 2>/dev/null)"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'falls back to SIGTERM'; then
        fail "$name" "the run took the fallback without saying so: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'STANDS NO SESSION DOWN'; then
        fail "$name" "the run does not say what the fallback costs: $out"
        return
    fi
    pass "$name"
}

test_realtest_2_is_skipped_when_no_daemon_is_serving() {
    local name="realtest 2 is SKIPPED with a reason when no daemon is serving for it to adopt"
    local dir out status=0
    dir="$(scratch_bin rt2-no-daemon)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(2)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED (nothing ran); output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'measures an ADOPTION'; then
        fail "$name" "the skip does not say why realtest 2 could not run: $out"
        return
    fi
    if [ -f "$SCRATCH/slot-reached" ]; then
        fail "$name" "realtest 2 ran into its own precondition instead of being skipped"
        return
    fi
    pass "$name"
}

test_realtest_2_keeps_the_standing_editor() {
    local name="the runner does NOT quit the editor for realtest 2, whose restart is what it measures"
    local dir out status=0
    dir="$(scratch_bin rt2-keeps-editor)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_daemon_line 55501 "$dir" > "$SCRATCH/procs"
    standing_emacs
    SCRIPT_ARGS=(2)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ -f "$SCRATCH/killed" ]; then
        fail "$name" "the runner quit the editor, which turns realtest 2's restart into a plain cold start"
        return
    fi
    pass "$name"
}

test_realtest_4_keeps_the_standing_editor() {
    local name="the runner does NOT quit the editor for realtest 4, which adopts whatever is standing"
    local dir out status=0
    dir="$(scratch_bin rt4-keeps-editor)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    standing_emacs
    SCRIPT_ARGS=(4)

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ -f "$SCRATCH/killed" ]; then
        fail "$name" "the editor was quit for a realtest that adopts it"
        return
    fi
    pass "$name"
}

test_a_failing_realtest_does_not_stop_the_sweep() {
    local name="a failing realtest does not stop the sweep, and the run's status is the failure"
    local dir out status=0
    dir="$(scratch_bin sweep-failure)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1 5 6)

    out="$(STUB_SLOT_FAIL=TestRealtestCreateWorkDeleteAWorkspace AGENT_REPL_REALTEST_TAKEOVER=1 \
        run_script "$dir")" || status=$?
    if [ "$status" -eq 0 ] || [ "$status" -eq "$EXIT_DECLINED" ] || [ "$status" -eq "$EXIT_INCOMPLETE" ]; then
        fail "$name" "exit was $status, want a realtest failure; output: $out"
        return
    fi
    if ! grep -q 'TestRealtestRegisterAndReopen' "$SCRATCH/slot-reached"; then
        fail "$name" "the realtest after the failure never ran, so one run does not gather every finding"
        return
    fi
    pass "$name"
}

test_a_realtest_asked_for_twice_runs_once() {
    local name="a realtest asked for twice runs once"
    local dir out status=0
    dir="$(scratch_bin sweep-duplicate)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1 1)

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ "$(grep -c . "$SCRATCH/slot-reached")" -ne 1 ]; then
        fail "$name" "the invocations were: $(cat "$SCRATCH/slot-reached")"
        return
    fi
    pass "$name"
}

test_the_world_table_covers_every_realtest() {
    local name="the world table holds a row for every realtest in e2e/realtest"
    local missing="" fn
    while IFS= read -r fn; do
        [ -n "$fn" ] || continue
        if ! grep -q "|${fn}|" "$SCRIPT_UNDER_TEST"; then
            missing="$missing $fn"
        fi
    done < <(grep -rho '^func TestRealtest[A-Za-z0-9_]*' "$THIS_DIR/../e2e/realtest" | awk '{print $2}' | sort -u)
    if [ -n "$missing" ]; then
        fail "$name" "these realtests have no row, so the runner does not know what world they need:$missing"
        return
    fi
    pass "$name"
}

test_every_world_table_row_names_a_real_test() {
    local name="every row in the world table names a realtest that exists"
    local stale="" fn
    while IFS= read -r fn; do
        [ -n "$fn" ] || continue
        if ! grep -rq "^func ${fn}(" "$THIS_DIR/../e2e/realtest"; then
            stale="$stale $fn"
        fi
    done < <(grep -o '|TestRealtest[A-Za-z0-9_]*|' "$SCRIPT_UNDER_TEST" | tr -d '|' | sort -u)
    if [ -n "$stale" ]; then
        fail "$name" "these rows name no test, so a selector would resolve to nothing:$stale"
        return
    fi
    pass "$name"
}

# ---- the sweep's edges: leftovers and the gap between sweeps --------------
#
# The three checks are `go test` invocations that do NOT go through
# suite-slot.sh (they start no editor and hold no machine), so they are seen
# through the `go` stub's own marker rather than the slot's.

test_declines_when_a_previous_sweep_left_registry_rows() {
    local name="the sweep DECLINES when a previous sweep left workspace rows in the registry"
    local dir out status=0
    dir="$(scratch_bin leftovers-standing)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"

    out="$(STUB_LEFTOVERS_FAIL=report run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q -- '--clean-leftovers'; then
        fail "$name" "the refusal does not name the remedy; output: $out"
        return
    fi
    if [ -s "$SCRATCH/slot-reached" ]; then
        fail "$name" "a realtest ran anyway: $(cat "$SCRATCH/slot-reached")"
        return
    fi
    pass "$name"
}

test_a_declined_sweep_does_not_move_the_mark() {
    local name="a sweep that declined does not move the between-sweeps mark"
    local dir status=0
    dir="$(scratch_bin leftovers-decline-mark)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"

    STUB_LEFTOVERS_FAIL=report run_script "$dir" >/dev/null 2>&1 || status=$?
    if grep -q 'TestBetweenSweepsMarkTheSweepEnd' "$SCRATCH/go-reached"; then
        fail "$name" "the mark was written for a run that never read the gap, discarding that window"
        return
    fi
    pass "$name"
}

test_the_sweep_reads_the_gap_before_any_realtest() {
    local name="the sweep reads the gap since the previous sweep before it runs a realtest"
    local dir out status=0
    dir="$(scratch_bin gap-scan-runs)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if ! grep -q '^TestBetweenSweepsGapScan none 1 ' "$SCRATCH/go-reached"; then
        fail "$name" "the gap scan never ran: $(cat "$SCRATCH/go-reached")"
        return
    fi
    pass "$name"
}

test_between_sweep_findings_fail_an_otherwise_green_sweep() {
    local name="findings between the sweeps fail a sweep whose realtests all passed"
    local dir out status=0
    dir="$(scratch_bin gap-scan-findings)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1)

    out="$(STUB_GAPSCAN_FAIL=1 AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if [ "$status" -eq 0 ] || [ "$status" -eq "$EXIT_DECLINED" ] || [ "$status" -eq "$EXIT_INCOMPLETE" ]; then
        fail "$name" "exit was $status, want a failure; output: $out"
        return
    fi
    if ! grep -q 'TestRealtestStartTheEditor' "$SCRATCH/slot-reached"; then
        fail "$name" "the sweep did not run its realtests; a between-sweeps finding reports, it does not block"
        return
    fi
    pass "$name"
}

test_the_sweep_cleans_its_own_rows_even_after_a_failure() {
    local name="the sweep removes the rows it created even when a realtest failed"
    local dir status=0
    dir="$(scratch_bin leftovers-clean-after-failure)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(5)

    STUB_SLOT_FAIL=TestRealtestCreateWorkDeleteAWorkspace AGENT_REPL_REALTEST_TAKEOVER=1 \
        run_script "$dir" >/dev/null 2>&1 || status=$?
    if ! grep -q "^TestCleanRealtestLeftovers clean 0 $SCRATCH/out\$" "$SCRATCH/go-reached"; then
        fail "$name" "no clean was asked for over the run directory: $(cat "$SCRATCH/go-reached")"
        return
    fi
    pass "$name"
}

test_a_row_the_sweep_could_not_remove_fails_the_run() {
    local name="a registry row the sweep could not remove fails an otherwise green run"
    local dir out status=0
    dir="$(scratch_bin leftovers-survive)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1)

    out="$(STUB_LEFTOVERS_FAIL=clean AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if [ "$status" -eq 0 ] || [ "$status" -eq "$EXIT_DECLINED" ] || [ "$status" -eq "$EXIT_INCOMPLETE" ]; then
        fail "$name" "exit was $status, want a failure; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'REALTEST LEFTOVER WORKSPACES'; then
        fail "$name" "the finding is not named in the output: $out"
        return
    fi
    pass "$name"
}

test_the_sweep_marks_where_it_ended() {
    local name="the sweep records where every source stood when it ended"
    local dir status=0
    dir="$(scratch_bin sweep-mark)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1)

    AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir" >/dev/null 2>&1 || status=$?
    if ! grep -q 'TestBetweenSweepsMarkTheSweepEnd' "$SCRATCH/go-reached"; then
        fail "$name" "the mark was never written, so the next sweep has no window start: $(cat "$SCRATCH/go-reached")"
        return
    fi
    pass "$name"
}

# ---- the sweep's focus: stolen once, handed back once ---------------------

test_the_sweep_takes_focus_before_any_realtest() {
    local name="the sweep takes focus ONCE, before its first realtest"
    local dir out status=0
    dir="$(scratch_bin focus-take)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ "$(grep -c '^take ' "$SCRATCH/focus-reached")" != "1" ]; then
        fail "$name" "the take ran $(grep -c '^take ' "$SCRATCH/focus-reached") time(s), want exactly one: $(cat "$SCRATCH/focus-reached")"
        return
    fi
    pass "$name"
}

test_the_sweep_hands_focus_back_at_its_end() {
    local name="the sweep hands focus back ONCE, at its end"
    local dir out status=0
    dir="$(scratch_bin focus-give-back)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1)

    out="$(AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ "$(grep -c '^give-back ' "$SCRATCH/focus-reached")" != "1" ]; then
        fail "$name" "the handback ran $(grep -c '^give-back ' "$SCRATCH/focus-reached") time(s), want exactly one: $(cat "$SCRATCH/focus-reached")"
        return
    fi
    pass "$name"
}

test_focus_goes_back_after_a_failing_realtest() {
    local name="focus goes back even when a realtest FAILED"
    local dir status=0
    dir="$(scratch_bin focus-after-failure)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1)

    STUB_SLOT_FAIL=TestRealtestStartTheEditor AGENT_REPL_REALTEST_TAKEOVER=1 \
        run_script "$dir" >/dev/null 2>&1 || status=$?
    if ! grep -q '^give-back ' "$SCRATCH/focus-reached"; then
        fail "$name" "focus was never handed back, so the owner's desktop stays on Emacs: $(cat "$SCRATCH/focus-reached")"
        return
    fi
    pass "$name"
}

test_focus_goes_back_after_a_sweep_of_one() {
    local name="a -run of a single realtest steals focus at its start and gives it back at its end"
    local dir status=0
    dir="$(scratch_bin focus-single-run)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(-run TestRealtestStartTheEditor)

    AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir" >/dev/null 2>&1 || status=$?
    if ! grep -q '^take ' "$SCRATCH/focus-reached" || ! grep -q '^give-back ' "$SCRATCH/focus-reached"; then
        fail "$name" "a single -run did not do both halves: $(cat "$SCRATCH/focus-reached")"
        return
    fi
    pass "$name"
}

test_the_presses_are_told_the_sweep_holds_focus() {
    local name="the realtests are told the sweep holds focus, so no press hands it back"
    local dir status=0
    dir="$(scratch_bin focus-held-flag)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1)

    AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir" >/dev/null 2>&1 || status=$?
    if ! grep -q '^TestRealtestStartTheEditor 1$' "$SCRATCH/focus-held"; then
        fail "$name" "the realtest did not carry the focus-held flag: $(cat "$SCRATCH/focus-held")"
        return
    fi
    pass "$name"
}

test_a_take_that_failed_owes_no_handback() {
    local name="a sweep that could NOT take focus owes no handback and tells its realtests so"
    local dir out status=0
    dir="$(scratch_bin focus-take-failed)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1)

    out="$(STUB_FOCUS_TAKE_FAIL=1 AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if grep -q '^give-back ' "$SCRATCH/focus-reached"; then
        fail "$name" "focus was handed back although it was never taken: $(cat "$SCRATCH/focus-reached")"
        return
    fi
    if ! grep -q '^TestRealtestStartTheEditor unset$' "$SCRATCH/focus-held"; then
        fail "$name" "the realtest was told the sweep holds focus although the take failed: $(cat "$SCRATCH/focus-held")"
        return
    fi
    if ! printf '%s' "$out" | grep -q 'COULD NOT TAKE FOCUS'; then
        fail "$name" "the failed take is not named in the output: $out"
        return
    fi
    pass "$name"
}

test_a_take_that_failed_does_not_stop_the_sweep() {
    local name="a sweep whose focus take failed still runs its realtests"
    local dir out status=0
    dir="$(scratch_bin focus-take-failed-runs)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(1)

    out="$(STUB_FOCUS_TAKE_FAIL=1 AGENT_REPL_REALTEST_TAKEOVER=1 run_script "$dir")" || status=$?
    if ! grep -q 'TestRealtestStartTheEditor' "$SCRATCH/slot-reached"; then
        fail "$name" "the sweep ran nothing; a desktop that would not cooperate must not cost the run its findings: $out"
        return
    fi
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0: a failed focus take is not a failed sweep; output: $out"
        return
    fi
    pass "$name"
}

test_clean_leftovers_runs_no_realtest() {
    local name="--clean-leftovers clears the realtest root and runs no realtest"
    local dir out status=0
    dir="$(scratch_bin clean-leftovers)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(--clean-leftovers)

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if ! grep -q "^TestCleanRealtestLeftovers clean 0 $SCRATCH/home/.claude-emacs/realtest\$" "$SCRATCH/go-reached"; then
        fail "$name" "the clean did not cover the realtest root: $(cat "$SCRATCH/go-reached")"
        return
    fi
    if [ -s "$SCRATCH/slot-reached" ]; then
        fail "$name" "a realtest ran: $(cat "$SCRATCH/slot-reached")"
        return
    fi
    pass "$name"
}

test_clean_leftovers_reports_rows_it_could_not_remove() {
    local name="--clean-leftovers answers non-zero when a row survives it"
    local dir out status=0
    dir="$(scratch_bin clean-leftovers-survive)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(--clean-leftovers)

    out="$(STUB_LEFTOVERS_FAIL=clean run_script "$dir")" || status=$?
    if [ "$status" -eq 0 ]; then
        fail "$name" "a survived row was reported as a successful clean; output: $out"
        return
    fi
    pass "$name"
}

test_clean_leftovers_refuses_to_also_run_a_realtest() {
    local name="--clean-leftovers with a realtest selector declines rather than guessing"
    local dir out status=0
    dir="$(scratch_bin clean-leftovers-and-selector)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    SCRIPT_ARGS=(--clean-leftovers 1)

    out="$(run_script "$dir")" || status=$?
    if [ "$status" -ne "$EXIT_DECLINED" ]; then
        fail "$name" "exit was $status, want $EXIT_DECLINED; output: $out"
        return
    fi
    pass "$name"
}

# ---- the editor the owner gets back ---------------------------------------
#
# A RUN LEAVES THE OWNER'S STATE AS IT FOUND IT, and the editor used to be the
# exception: every realtest launches Emacs under the vendor guard, the sweep
# left the last one standing, and the owner's day-to-day editor was therefore
# the one whose daemon and shims answered from the FAKE vendor. These cases are
# about which editor is standing when the run is over.

# guarded_emacs_line PID — an Emacs in the stub process table whose kernel
# environment carries the vendor guard.
guarded_emacs_line() {
    printf '%s /Applications/Emacs.app/Contents/MacOS/Emacs AGENT_REPL_FORBID_VENDOR_CALLS=1\n' "$1"
}

# plain_emacs_line PID — an Emacs the owner started themselves.
plain_emacs_line() {
    printf '%s /Applications/Emacs.app/Contents/MacOS/Emacs\n' "$1"
}

test_a_guarded_editor_is_quit_at_the_end() {
    local name="a guarded Emacs left standing at the end of a run is QUIT"
    local dir out status=0
    dir="$(scratch_bin handback-quit)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_emacs_line 7777 > "$SCRATCH/procs"

    SCRIPT_ARGS=(1)
    out="$(STUB_EMACS_PID=7777 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ ! -f "$SCRATCH/killed" ]; then
        fail "$name" "the guarded editor was left running; output: $out"
        return
    fi
    pass "$name"
}

test_a_guarded_editor_is_replaced_by_a_normal_one() {
    local name="a run that quit a guarded Emacs cold-starts a guard-free one in its place"
    local dir out status=0
    dir="$(scratch_bin handback-relaunch)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_emacs_line 7777 > "$SCRATCH/procs"

    SCRIPT_ARGS=(1)
    out="$(STUB_EMACS_PID=7777 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ ! -f "$SCRATCH/open-reached" ]; then
        fail "$name" "no editor was launched; output: $out"
        return
    fi
    if ! grep -q -- '-gj -a Emacs' "$SCRATCH/open-reached"; then
        fail "$name" "the launch was $(cat "$SCRATCH/open-reached")"
        return
    fi
    pass "$name"
}

test_the_replacement_editor_carries_no_guard() {
    local name="the replacement editor is launched with the vendor guard REMOVED from its environment"
    local dir out status=0
    dir="$(scratch_bin handback-unguarded)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_emacs_line 7777 > "$SCRATCH/procs"

    # THE GUARD IS IN THIS RUN'S OWN ENVIRONMENT, which is the case the `env -u`
    # exists for: an operator who exported it, or a sweep re-run from a shell
    # that still holds it, must not hand the owner another guarded editor.
    SCRIPT_ARGS=(1)
    out="$(STUB_EMACS_PID=7777 AGENT_REPL_FORBID_VENDOR_CALLS=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if ! grep -q 'guard=unset' "$SCRATCH/open-reached"; then
        fail "$name" "the launch carried the guard: $(cat "$SCRATCH/open-reached")"
        return
    fi
    pass "$name"
}

test_the_summary_names_the_editor_the_owner_gets_back() {
    local name="the run's summary names the editor that was quit and the one the owner got back"
    local dir out status=0
    dir="$(scratch_bin handback-summary)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_emacs_line 7777 > "$SCRATCH/procs"

    SCRIPT_ARGS=(1)
    out="$(STUB_EMACS_PID=7777 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q "the owner's editor was restored: guarded Emacs pid 7777 quit"; then
        fail "$name" "the summary does not name what was done: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q "a guard-free Emacs launched"; then
        fail "$name" "the summary does not name the editor the owner got back: $out"
        return
    fi
    pass "$name"
}

test_a_guard_free_editor_is_left_alone() {
    local name="a guard-free Emacs left standing at the end is LEFT ALONE"
    local dir out status=0
    dir="$(scratch_bin handback-leave-alone)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    plain_emacs_line 7777 > "$SCRATCH/procs"

    SCRIPT_ARGS=(1)
    out="$(STUB_EMACS_PID=7777 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ -f "$SCRATCH/killed" ]; then
        fail "$name" "an editor that carries no guard was quit anyway; output: $out"
        return
    fi
    if [ -f "$SCRATCH/open-reached" ]; then
        fail "$name" "a second editor was launched beside the owner's: $(cat "$SCRATCH/open-reached")"
        return
    fi
    if ! printf '%s' "$out" | grep -q "the owner keeps it, untouched"; then
        fail "$name" "the run does not say the editor was left alone: $out"
        return
    fi
    pass "$name"
}

test_a_guarded_daemon_is_stopped_under_its_consent() {
    local name="the handback stops a guarded daemon under AGENT_REPL_REALTEST_STOP_DAEMON=1"
    local dir out status=0
    dir="$(scratch_bin handback-daemon-stop)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    { guarded_emacs_line 7777; guarded_daemon_line 4242 "$dir"; } > "$SCRATCH/procs"

    SCRIPT_ARGS=(1)
    out="$(STUB_EMACS_PID=7777 AGENT_REPL_REALTEST_STOP_DAEMON=1 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if ! grep -q '^TestOrderlyDaemonStop$' "$SCRATCH/orderly-reached" 2>/dev/null; then
        fail "$name" "the guarded daemon was not asked to stop through its own door: $(cat "$SCRATCH/orderly-reached" 2>/dev/null); output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q "guarded daemon pid 4242 stopped"; then
        fail "$name" "the summary does not say the daemon was stopped: $out"
        return
    fi
    pass "$name"
}

test_a_guarded_daemon_is_left_without_the_consent() {
    local name="the handback LEAVES a guarded daemon when its consent was not given, and says so"
    local dir out status=0
    dir="$(scratch_bin handback-daemon-left)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    { guarded_emacs_line 7777; guarded_daemon_line 4242 "$dir"; } > "$SCRATCH/procs"

    SCRIPT_ARGS=(1)
    out="$(STUB_EMACS_PID=7777 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ -s "$SCRATCH/orderly-reached" ] || grep -q '^4242$' "$SCRATCH/kill-log" 2>/dev/null; then
        fail "$name" "the daemon was stopped without the consent that covers it; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q "THE DAEMON LEFT RUNNING (pid(s) 4242) CARRIES"; then
        fail "$name" "the run did not say loudly that a fake-vendor daemon is still up: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q "guarded daemon pid 4242 LEFT RUNNING"; then
        fail "$name" "the summary does not carry the daemon that was left: $out"
        return
    fi
    pass "$name"
}

test_a_run_of_one_realtest_hands_the_editor_back_too() {
    local name="a -run of a single realtest hands the owner a guard-free editor back as a sweep does"
    local dir out status=0
    dir="$(scratch_bin handback-single)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_emacs_line 7777 > "$SCRATCH/procs"

    SCRIPT_ARGS=(-run TestRealtestStartTheEditor)
    out="$(STUB_EMACS_PID=7777 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if [ ! -f "$SCRATCH/killed" ] || [ ! -f "$SCRATCH/open-reached" ]; then
        fail "$name" "the single run left the guarded editor standing; output: $out"
        return
    fi
    pass "$name"
}

test_the_editor_handback_survives_a_failing_realtest() {
    local name="the editor is handed back even when the realtest FAILED"
    local dir out status=0
    dir="$(scratch_bin handback-after-failure)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    guarded_emacs_line 7777 > "$SCRATCH/procs"

    SCRIPT_ARGS=(1)
    out="$(STUB_EMACS_PID=7777 STUB_SLOT_FAIL=TestRealtestStartTheEditor run_script "$dir")" || status=$?
    if [ "$status" -eq 0 ]; then
        fail "$name" "a failing realtest reported success; output: $out"
        return
    fi
    if [ ! -f "$SCRATCH/open-reached" ]; then
        fail "$name" "no guard-free editor was launched after the failure; output: $out"
        return
    fi
    pass "$name"
}

test_a_run_states_which_editor_the_owner_gets_back_before_it_starts() {
    local name="the run says BEFORE its first realtest which editor the owner will get back"
    local dir out status=0
    dir="$(scratch_bin handback-preflight)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    plain_emacs_line 7777 > "$SCRATCH/procs"

    SCRIPT_ARGS=(1)
    out="$(STUB_EMACS_PID=7777 run_script "$dir")" || status=$?
    if [ "$status" -ne 0 ]; then
        fail "$name" "exit was $status, want 0; output: $out"
        return
    fi
    if ! printf '%s' "$out" | grep -q "when this run ends the owner gets a GUARD-FREE editor back"; then
        fail "$name" "the preflight does not name the editor the owner gets back: $out"
        return
    fi
    pass "$name"
}

# ---- run ------------------------------------------------------------------

test_backup_copies_the_database
test_backup_carries_the_wal_siblings
test_backup_refuses_to_overwrite
test_backup_absent_file_is_not_a_failure
test_backup_refuses_a_missing_stamp
test_backup_uses_a_clone
test_clone_failure_falls_back_to_plain_copy
test_total_copy_failure_fails_the_backup
test_prune_keeps_n_most_recent
test_prune_deletes_wal_and_shm_siblings
test_prune_defaults_to_keeping_three
test_prune_returns_success_when_nothing_to_prune
test_declines_when_a_system_is_not_deployed
test_declining_names_the_daemon_deploy_remedy
test_declines_when_emacs_is_running_without_a_takeover
test_backs_up_before_refusing_the_takeover
test_declines_when_the_backup_copy_totally_fails
test_runs_when_nothing_stands_in_the_way
test_records_the_deployed_revisions
test_declines_when_the_daemon_lacks_the_guard
test_the_unguarded_daemon_refusal_names_the_consent
test_the_unguarded_daemon_is_stopped_under_the_consent
test_the_editor_is_quit_before_the_unguarded_daemon_is_stopped
test_declines_when_a_listening_shim_lacks_the_guard
test_declines_on_an_owner_shim_whatever_the_callers_state_dir
test_runs_when_every_listening_shim_carries_the_guard
test_ignores_a_shim_listening_under_another_state_directory
test_declines_when_a_shim_lock_lacks_the_guard
test_the_unguarded_shim_refusal_names_the_consent
test_the_unguarded_shims_are_stopped_under_the_consent
test_a_preflight_decline_after_a_quit_still_hands_an_editor_back
test_unknown_selector_declines_before_anything
test_run_pattern_matching_nothing_declines
test_run_pattern_selects_one_realtest
test_a_sweep_runs_one_invocation_per_realtest_in_order
test_a_sweep_quits_the_editor_between_cold_starts
test_a_sweep_backs_up_once
test_declines_a_sweep_that_would_quit_a_standing_editor
test_realtest_3_is_skipped_without_the_daemon_consent
test_realtest_3_stops_the_daemon_under_its_own_consent
test_a_daemon_that_ignores_sigterm_is_not_escalated
test_the_stop_goes_through_the_daemons_own_door
test_the_sigterm_fallback_is_stated_when_the_door_is_unanswered
test_realtest_2_is_skipped_when_no_daemon_is_serving
test_realtest_2_keeps_the_standing_editor
test_realtest_4_keeps_the_standing_editor
test_a_failing_realtest_does_not_stop_the_sweep
test_a_realtest_asked_for_twice_runs_once
test_the_world_table_covers_every_realtest
test_every_world_table_row_names_a_real_test
test_declines_when_a_previous_sweep_left_registry_rows
test_a_declined_sweep_does_not_move_the_mark
test_the_sweep_reads_the_gap_before_any_realtest
test_between_sweep_findings_fail_an_otherwise_green_sweep
test_the_sweep_cleans_its_own_rows_even_after_a_failure
test_a_row_the_sweep_could_not_remove_fails_the_run
test_the_sweep_marks_where_it_ended
test_the_sweep_takes_focus_before_any_realtest
test_the_sweep_hands_focus_back_at_its_end
test_focus_goes_back_after_a_failing_realtest
test_focus_goes_back_after_a_sweep_of_one
test_the_presses_are_told_the_sweep_holds_focus
test_a_take_that_failed_owes_no_handback
test_a_take_that_failed_does_not_stop_the_sweep
test_clean_leftovers_runs_no_realtest
test_clean_leftovers_reports_rows_it_could_not_remove
test_clean_leftovers_refuses_to_also_run_a_realtest
test_a_guarded_editor_is_quit_at_the_end
test_a_guarded_editor_is_replaced_by_a_normal_one
test_the_replacement_editor_carries_no_guard
test_the_summary_names_the_editor_the_owner_gets_back
test_a_guard_free_editor_is_left_alone
test_a_guarded_daemon_is_stopped_under_its_consent
test_a_guarded_daemon_is_left_without_the_consent
test_a_run_of_one_realtest_hands_the_editor_back_too
test_the_editor_handback_survives_a_failing_realtest
test_a_run_states_which_editor_the_owner_gets_back_before_it_starts

echo
echo "$PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
