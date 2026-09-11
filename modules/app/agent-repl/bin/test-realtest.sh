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

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/realtest.sh"
LIB_UNDER_TEST="$THIS_DIR/lib-realtest-backup.sh"

# shellcheck source=lib-realtest-backup.sh
. "$LIB_UNDER_TEST"

readonly EXIT_DECLINED=77

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

    cat > "$dir/suite-slot.sh" <<'STUB'
#!/usr/bin/env bash
printf 'reached\n' > "${STUB_SLOT_MARKER:?the case must state a marker path}"
exit 0
STUB

    cat > "$dir/emacsclient" <<'STUB'
#!/usr/bin/env bash
# Answers only what the case says it should. With STUB_EMACS_ALIVE unset every
# probe fails, which is how "no Emacs is running" is spelled.
[ "${STUB_EMACS_ALIVE:-}" = "1" ] || exit 1
for arg in "$@"; do
    case "$arg" in
        '(kill-emacs)') printf 'killed\n' > "${STUB_KILL_MARKER:?}"; rm -f "${STUB_ALIVE_FLAG:?}" ;;
    esac
done
printf 'nil\n'
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

    chmod +x "$dir"/*.sh "$dir/emacsclient" "$dir/pgrep" "$dir/ps" "$dir/cp"
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

# run_script DIR — run the copied script with the stub environment, capturing
# output and status. Every path the script would otherwise reach on the real
# machine is redirected into the scratch tree.
run_script() {
    local dir="$1"
    shift
    set +e
    HOME="$SCRATCH/home" \
    PATH="$dir:$PATH" \
    STUB_PROCS="${STUB_PROCS:-$SCRATCH/procs}" \
    STUB_READINESS_JSON="$SCRATCH/readiness.json" \
    STUB_SLOT_MARKER="$SCRATCH/slot-reached" \
    STUB_KILL_MARKER="$SCRATCH/killed" \
    AGENT_REPL_REALTEST_EMACSCLIENT="$dir/emacsclient" \
    AGENT_REPL_REALTEST_OUT="$SCRATCH/out" \
    "$@" bash "$dir/realtest.sh" 2>&1
    local status=$?
    set -e
    return "$status"
}

prepare_home() {
    rm -rf "${SCRATCH:?}/home" "${SCRATCH:?}/out" "${SCRATCH:?}/slot-reached" "${SCRATCH:?}/killed"
    : > "$SCRATCH/procs"
    mkdir -p "$SCRATCH/home/.claude-emacs" "$SCRATCH/home/.cache/agent-repl/store"
    printf 'workspaces' > "$SCRATCH/home/.claude-emacs/wsm.db"
    printf 'events' > "$SCRATCH/home/.cache/agent-repl/store/events.db"
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

    out="$(STUB_EMACS_ALIVE=1 STUB_ALIVE_FLAG="$SCRATCH/alive" run_script "$dir")" || status=$?
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

    STUB_EMACS_ALIVE=1 STUB_ALIVE_FLAG="$SCRATCH/alive" run_script "$dir" >/dev/null || status=$?
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
    if ! ls "$SCRATCH/home/.cache/agent-repl/store/events.db.realtest-bak-"* >/dev/null 2>&1; then
        fail "$name" "no store backup was taken before the refusal"
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
    if ! printf '%s' "$out" | grep -q "the owner's editor is left running"; then
        fail "$name" "the run does not state that the editor is left running: $out"
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

test_runs_when_every_listening_shim_carries_the_guard() {
    local name="the script runs when a listening shim carries the vendor guard"
    local dir out status=0
    dir="$(scratch_bin guarded-shim)"
    prepare_home
    ready_json > "$SCRATCH/readiness.json"
    printf '94292 node /opt/agent-shim/claude/shim/dist/main.js --listen %s/.claude-emacs/sock/aaaa.n1.sock AGENT_REPL_FORBID_VENDOR_CALLS=1\n' \
        "$SCRATCH/home" > "$SCRATCH/procs"

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
test_declines_when_a_system_is_not_deployed
test_declines_when_emacs_is_running_without_a_takeover
test_backs_up_before_refusing_the_takeover
test_declines_when_the_backup_copy_totally_fails
test_runs_when_nothing_stands_in_the_way
test_records_the_deployed_revisions
test_declines_when_the_daemon_lacks_the_guard
test_declines_when_a_listening_shim_lacks_the_guard
test_runs_when_every_listening_shim_carries_the_guard
test_ignores_a_shim_listening_under_another_state_directory
test_declines_when_a_shim_lock_lacks_the_guard

echo
echo "$PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
