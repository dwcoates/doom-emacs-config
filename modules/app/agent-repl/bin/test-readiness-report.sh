#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# test-readiness-report.sh — hermetic tests for readiness-report.sh.
#
# Builds a throwaway GIT repository around a copy of readiness-report.sh (the
# script is all git plumbing, so a scratch repo with real commits is the honest
# fixture — the same approach test-build-frontend.sh takes with a scratch tree)
# and stubs `pgrep` and `ps` on PATH so no real process, launchd job, or
# machine state is consulted. Tests assert what the JSON says under each
# deployed-vs-source scenario.
#
# Every scenario also re-asserts that the document PARSES. "Valid JSON always,
# even on partial failure" is the contract Emacs polls against, and a report
# that goes syntactically wrong on an error path is worse than no report at
# all: the poller would fail silently every 15 seconds.
#
# Run with:   bash bin/test-readiness-report.sh

set -euo pipefail

# A pre-commit hook exports its live index to children. This harness owns only
# scratch repositories, so inheriting that binding would let fixture `git add`
# and `git commit` rewrite the caller's real staging index.
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_PREFIX

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/readiness-report.sh"
LIB_UNDER_TEST="$THIS_DIR/lib-deploy-stamp.sh"

command -v python3 >/dev/null 2>&1 || {
    echo "test-readiness-report.sh: python3 is required to validate the JSON" >&2
    exit 2
}

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -n "${2:-}" ] && echo "       $2"; return 0; }

# --- fixture ----------------------------------------------------------------

git_c() {
    # Scratch commits are fixture construction, not authored repository
    # changes. Isolate them from the parent checkout's absolute shared hook.
    git -c user.name=t -c user.email=t@example.com \
        -c core.hooksPath=/dev/null -C "$1" "${@:2}"
}

# make_repo ROOT — a scratch git repo whose top level IS the module root, with
# one commit per system directory so every pathspec has distinct history.
#
# Commits are dated a fixed hour apart so minutes_behind is a deterministic
# number rather than whatever the clock did during the run.
make_repo() {
    local root="$1" n=0 d
    mkdir -p "$root/bin" "$root/daemon/bin" "$root/proto" \
             "$root/agent-shim/wire" "$root/agent-shim/shim-store" \
             "$root/agent-shim/claude/shim/dist" \
             "$root/agent-shim/claude/shim-sidecar" \
             "$root/webapp/dist" "$root/home/.cache/agent-repl/bin"
    cp "$SCRIPT_UNDER_TEST" "$root/bin/readiness-report.sh"
    cp "$LIB_UNDER_TEST" "$root/bin/lib-deploy-stamp.sh"

    git_c "$root" init -q
    for d in proto agent-shim/wire agent-shim/shim-store \
             agent-shim/claude/shim agent-shim/claude/shim-sidecar \
             webapp daemon; do
        echo "rev0" > "$root/$d/file.txt"
        git_c "$root" add -A
        n=$((n + 1))
        GIT_AUTHOR_DATE="@$((1700000000 + n * 3600)) +0000" \
        GIT_COMMITTER_DATE="@$((1700000000 + n * 3600)) +0000" \
            git_c "$root" commit -qm "seed $d"
    done
}

# touch_system ROOT DIR MESSAGE — one more commit under DIR, dated exactly an
# hour after the repo's current HEAD.
#
# The hour is read back from the repo rather than counted in a shell variable:
# the fixtures are built inside a command substitution (a subshell), so a
# counter would silently reset there and leave every timestamp assertion
# dependent on the order the tests happened to run in.
touch_system() {
    local root="$1" dir="$2" msg="$3" ts
    ts=$(( $(git_c "$root" log -1 --format=%ct) + 3600 ))
    echo "$msg" >> "$root/$dir/file.txt"
    git_c "$root" add -A
    GIT_AUTHOR_DATE="@$ts +0000" GIT_COMMITTER_DATE="@$ts +0000" \
        git_c "$root" commit -qm "$msg"
}

head_sha() { git_c "$1" rev-parse HEAD; }

# make_stubs DIR — pgrep/ps stubs driven by env, so a test names exactly one
# "running" process instead of whatever happens to be on the machine.
make_stubs() {
    local bindir="$1"
    mkdir -p "$bindir"
    cat > "$bindir/pgrep" <<'EOF'
#!/usr/bin/env bash
# pgrep -f PATTERN — a hit only for the substring the test declared running.
pattern="${2:-}"
case "$pattern" in
    *"${FAKE_PROC_MATCH:-__nothing_runs__}"*) echo "${FAKE_PROC_PID:-4242}"; exit 0 ;;
esac
exit 1
EOF
    cat > "$bindir/ps" <<'EOF'
#!/usr/bin/env bash
# ps -o etime= -p PID
echo "${FAKE_PROC_ETIME:-10:00}"
EOF
    chmod +x "$bindir/pgrep" "$bindir/ps"
}

run_report() { # ROOT -> stdout on $OUT, exit status in $RC
    local root="$1"
    set +e
    HOME="$root/home" PATH="$root/stubs:$PATH" \
        bash "$root/bin/readiness-report.sh" >"$root/out.json" 2>"$root/err.txt"
    RC=$?
    set -e
    OUT="$root/out.json"
}

# jq_get FILE PYTHON-EXPR — read the report with `d` bound to the parsed
# document. Also the JSON-validity assertion: a malformed document makes every
# query in the suite fail loudly rather than silently comparing empty strings.
jq_get() {
    python3 -c '
import json, sys
d = json.load(open(sys.argv[1]))
sysmap = {s["name"]: s for s in d["systems"]}
print(eval(sys.argv[2]))
' "$1" "$2"
}

new_root() {
    local root; root="$(mktemp -d)"
    make_repo "$root"
    make_stubs "$root/stubs"
    printf '%s' "$root"
}

stamp() { # FILE VALUE
    mkdir -p "$(dirname "$1")"
    printf '%s\n' "$2" > "$1"
}

# --- 1. a missing stamp reports unknown, never a guess ----------------------
t_missing_stamp_is_unknown() {
    local root; root="$(new_root)"
    run_report "$root"
    if [ "$RC" -eq 0 ] \
       && [ "$(jq_get "$OUT" 'sysmap["daemon"]["deployed_sha"]')" = "None" ] \
       && [ "$(jq_get "$OUT" 'sysmap["daemon"]["ready"]')" = "False" ] \
       && [ "$(jq_get "$OUT" '"built-sha" in sysmap["daemon"]["error"]')" = "True" ]; then
        pass "a missing .built-sha stamp reports null and never guesses repo HEAD"
    else
        fail "a missing .built-sha stamp reports null and never guesses repo HEAD" \
             "rc=$RC out: $(cat "$OUT") err: $(cat "$root/err.txt")"
    fi
    rm -rf "$root"
}

# --- 2. a stamp at the system's newest commit is ready ----------------------
t_current_stamp_is_ready() {
    local root; root="$(new_root)"
    stamp "$root/webapp/dist/.built-sha" \
          "$(git_c "$root" log -1 --format=%H -- webapp proto)"
    run_report "$root"
    if [ "$(jq_get "$OUT" 'sysmap["webapp"]["commits_behind"]')" = "0" ] \
       && [ "$(jq_get "$OUT" 'sysmap["webapp"]["minutes_behind"]')" = "0" ] \
       && [ "$(jq_get "$OUT" 'sysmap["webapp"]["ready"]')" = "True" ]; then
        pass "a stamp at the system's newest commit reports zero distance and ready"
    else
        fail "a stamp at the system's newest commit reports zero distance and ready" \
             "out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# --- 3. commits behind are counted with the pathspec applied ----------------
t_commits_behind_counts_only_own_system() {
    local root behind; root="$(new_root)"
    behind="$(git_c "$root" log -1 --format=%H -- webapp proto)"
    stamp "$root/webapp/dist/.built-sha" "$behind"
    touch_system "$root" webapp "webapp change one"
    touch_system "$root" webapp "webapp change two"
    touch_system "$root" daemon "unrelated daemon change"
    run_report "$root"
    if [ "$(jq_get "$OUT" 'sysmap["webapp"]["commits_behind"]')" = "2" ] \
       && [ "$(jq_get "$OUT" 'sysmap["webapp"]["ready"]')" = "False" ]; then
        pass "commits_behind counts only commits touching the system's own pathspec"
    else
        fail "commits_behind counts only commits touching the system's own pathspec" \
             "out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# --- 4. minutes behind is the commit-timestamp delta ------------------------
t_minutes_behind_is_timestamp_delta() {
    local root; root="$(new_root)"
    stamp "$root/webapp/dist/.built-sha" \
          "$(git_c "$root" log -1 --format=%H -- webapp proto)"
    touch_system "$root" webapp "webapp change"
    run_report "$root"
    # The webapp seed is the 6th of 7 seeds and the seeds are an hour apart, so
    # the new commit (an hour past the 7th) sits exactly 2 hours after it.
    if [ "$(jq_get "$OUT" 'sysmap["webapp"]["minutes_behind"]')" = "120" ]; then
        pass "minutes_behind is the commit-timestamp delta between deployed and source"
    else
        fail "minutes_behind is the commit-timestamp delta between deployed and source" \
             "out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# --- 5. a dirty stamp measures against its own sha and says so --------------
t_dirty_stamp_flagged_and_still_measured() {
    local root behind; root="$(new_root)"
    behind="$(git_c "$root" log -1 --format=%H -- webapp proto)"
    stamp "$root/webapp/dist/.built-sha" "$behind-dirty"
    touch_system "$root" webapp "webapp change"
    run_report "$root"
    if [ "$(jq_get "$OUT" 'sysmap["webapp"]["deployed_dirty"]')" = "True" ] \
       && [ "$(jq_get "$OUT" 'sysmap["webapp"]["commits_behind"]')" = "1" ]; then
        pass "a dirty stamp is flagged and still measured against its own sha"
    else
        fail "a dirty stamp is flagged and still measured against its own sha" \
             "out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# --- 6. a stamp naming an unknown revision errors, without killing the run --
t_unknown_deployed_revision_errors_per_system() {
    local root; root="$(new_root)"
    stamp "$root/webapp/dist/.built-sha" "0123456789012345678901234567890123456789"
    run_report "$root"
    if [ "$RC" -eq 0 ] \
       && [ "$(jq_get "$OUT" 'sysmap["webapp"]["commits_behind"]')" = "None" ] \
       && [ "$(jq_get "$OUT" '"not present in this checkout" in sysmap["webapp"]["error"]')" = "True" ] \
       && [ "$(jq_get "$OUT" 'len(d["systems"])')" = "5" ]; then
        pass "a stamp naming an unknown revision errors that system only, exit stays 0"
    else
        fail "a stamp naming an unknown revision errors that system only, exit stays 0" \
             "rc=$RC out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# --- 7. proto is in every Go/TS system's pathspec ---------------------------
t_proto_commit_stales_every_system() {
    local root; root="$(new_root)"
    touch_system "$root" proto "proto regeneration"
    run_report "$root"
    local want; want="$(head_sha "$root")"
    if [ "$(jq_get "$OUT" "len(set(s['source_sha'] for s in d['systems'])) == 1")" = "True" ] \
       && [ "$(jq_get "$OUT" 'sysmap["shim"]["source_sha"]')" = "$want" ]; then
        pass "a proto commit becomes the source revision of every Go/TS system"
    else
        fail "a proto commit becomes the source revision of every Go/TS system" \
             "want=$want out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# --- 8. a running daemon older than its binary is stale ---------------------
t_daemon_binary_newer_than_process_is_stale() {
    local root; root="$(new_root)"
    stamp "$root/daemon/bin/.built-sha" "$(head_sha "$root")"
    echo binary > "$root/daemon/bin/claude-repld"   # written just now
    set +e
    HOME="$root/home" PATH="$root/stubs:$PATH" \
        FAKE_PROC_MATCH="claude-repld" FAKE_PROC_PID=999 FAKE_PROC_ETIME="10:00" \
        bash "$root/bin/readiness-report.sh" >"$root/out.json" 2>"$root/err.txt"
    RC=$?
    set -e
    OUT="$root/out.json"
    if [ "$(jq_get "$OUT" 'sysmap["daemon"]["running"]["pid"]')" = "999" ] \
       && [ "$(jq_get "$OUT" 'sysmap["daemon"]["running"]["stale_binary"]')" = "True" ] \
       && [ "$(jq_get "$OUT" 'sysmap["daemon"]["ready"]')" = "False" ]; then
        pass "a daemon process older than its binary is stale, and not ready"
    else
        fail "a daemon process older than its binary is stale, and not ready" \
             "rc=$RC out: $(cat "$OUT") err: $(cat "$root/err.txt")"
    fi
    rm -rf "$root"
}

# --- 9. a running daemon newer than its binary is not stale -----------------
t_daemon_started_after_binary_is_fresh() {
    local root; root="$(new_root)"
    stamp "$root/daemon/bin/.built-sha" "$(head_sha "$root")"
    echo binary > "$root/daemon/bin/claude-repld"
    touch -t 202001010000 "$root/daemon/bin/claude-repld"
    set +e
    HOME="$root/home" PATH="$root/stubs:$PATH" \
        FAKE_PROC_MATCH="claude-repld" FAKE_PROC_PID=1001 FAKE_PROC_ETIME="00:05" \
        bash "$root/bin/readiness-report.sh" >"$root/out.json" 2>"$root/err.txt"
    RC=$?
    set -e
    OUT="$root/out.json"
    if [ "$(jq_get "$OUT" 'sysmap["daemon"]["running"]["stale_binary"]')" = "False" ] \
       && [ "$(jq_get "$OUT" 'sysmap["daemon"]["ready"]')" = "True" ]; then
        pass "a daemon process started after its binary was written is not stale"
    else
        fail "a daemon process started after its binary was written is not stale" \
             "rc=$RC out: $(cat "$OUT") err: $(cat "$root/err.txt")"
    fi
    rm -rf "$root"
}

# --- 10. a launchd service whose deployed stamp disagrees is stale ----------
t_service_fingerprint_mismatch_is_stale() {
    local root cache; root="$(new_root)"
    cache="$root/home/.cache/agent-repl/bin"
    stamp "$cache/.shim-store.built-sha" "$(head_sha "$root")"
    printf 'installed-v2' > "$cache/shim-store"
    printf 'a-digest-from-the-previous-image' > "$cache/.shim-store.deployed"
    set +e
    HOME="$root/home" PATH="$root/stubs:$PATH" \
        FAKE_PROC_MATCH="shim-store" FAKE_PROC_PID=555 \
        bash "$root/bin/readiness-report.sh" >"$root/out.json" 2>"$root/err.txt"
    RC=$?
    set -e
    OUT="$root/out.json"
    if [ "$(jq_get "$OUT" 'sysmap["shim-store"]["running"]["stale_binary"]')" = "True" ] \
       && [ "$(jq_get "$OUT" 'sysmap["shim-store"]["ready"]')" = "False" ]; then
        pass "a service whose kickstart fingerprint disagrees with the installed binary is stale"
    else
        fail "a service whose kickstart fingerprint disagrees with the installed binary is stale" \
             "rc=$RC out: $(cat "$OUT") err: $(cat "$root/err.txt")"
    fi
    rm -rf "$root"
}

# --- 11. a matching deployed fingerprint is not stale -----------------------
t_service_fingerprint_match_is_fresh() {
    local root cache; root="$(new_root)"
    cache="$root/home/.cache/agent-repl/bin"
    stamp "$cache/.shim-store.built-sha" "$(head_sha "$root")"
    printf 'installed-v2' > "$cache/shim-store"
    shasum -a 256 "$cache/shim-store" | cut -d' ' -f1 > "$cache/.shim-store.deployed"
    set +e
    HOME="$root/home" PATH="$root/stubs:$PATH" \
        FAKE_PROC_MATCH="shim-store" FAKE_PROC_PID=556 \
        bash "$root/bin/readiness-report.sh" >"$root/out.json" 2>"$root/err.txt"
    RC=$?
    set -e
    OUT="$root/out.json"
    if [ "$(jq_get "$OUT" 'sysmap["shim-store"]["running"]["stale_binary"]')" = "False" ] \
       && [ "$(jq_get "$OUT" 'sysmap["shim-store"]["ready"]')" = "True" ]; then
        pass "a service running the installed binary's fingerprint is not stale"
    else
        fail "a service running the installed binary's fingerprint is not stale" \
             "rc=$RC out: $(cat "$OUT") err: $(cat "$root/err.txt")"
    fi
    rm -rf "$root"
}

# --- 12. systems with no long-lived process report running: null ------------
t_processless_systems_report_null_running() {
    local root; root="$(new_root)"
    run_report "$root"
    if [ "$(jq_get "$OUT" 'sysmap["shim"]["running"]')" = "None" ] \
       && [ "$(jq_get "$OUT" 'sysmap["webapp"]["running"]')" = "None" ]; then
        pass "shim and webapp report a null running process rather than a fabricated one"
    else
        fail "shim and webapp report a null running process rather than a fabricated one" \
             "out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# --- 13. elisp is absent from the report ------------------------------------
t_elisp_is_not_reported() {
    local root; root="$(new_root)"
    run_report "$root"
    if [ "$(jq_get "$OUT" '"elisp" in sysmap')" = "False" ]; then
        pass "elisp is deliberately absent from the systems list"
    else
        fail "elisp is deliberately absent from the systems list" "out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# --- 14. outside a git checkout the report refuses rather than inventing ----
t_no_git_checkout_exits_nonzero() {
    local root; root="$(mktemp -d)"
    mkdir -p "$root/bin" "$root/home"
    cp "$SCRIPT_UNDER_TEST" "$root/bin/readiness-report.sh"
    cp "$LIB_UNDER_TEST" "$root/bin/lib-deploy-stamp.sh"
    make_stubs "$root/stubs"
    # A temp dir can sit under an unrelated checkout on some machines; point
    # git at a ceiling so the probe is decided by THIS tree.
    set +e
    HOME="$root/home" PATH="$root/stubs:$PATH" GIT_CEILING_DIRECTORIES="$root" \
        bash "$root/bin/readiness-report.sh" >"$root/out.json" 2>"$root/err.txt"
    RC=$?
    set -e
    if [ "$RC" -eq 1 ] && grep -q "not inside a git checkout" "$root/err.txt"; then
        pass "outside a git checkout the report exits 1 rather than inventing one"
    else
        fail "outside a git checkout the report exits 1 rather than inventing one" \
             "rc=$RC err: $(cat "$root/err.txt")"
    fi
    rm -rf "$root"
}

# --- 15. an unknown argument is rejected ------------------------------------
t_unknown_argument_exits_two() {
    local root; root="$(new_root)"
    set +e
    HOME="$root/home" PATH="$root/stubs:$PATH" \
        bash "$root/bin/readiness-report.sh" --nope >"$root/out.json" 2>"$root/err.txt"
    RC=$?
    set -e
    if [ "$RC" -eq 2 ] && grep -q "unknown argument" "$root/err.txt"; then
        pass "an unknown argument exits 2 with a usage message"
    else
        fail "an unknown argument exits 2 with a usage message" \
             "rc=$RC err: $(cat "$root/err.txt")"
    fi
    rm -rf "$root"
}

t_missing_stamp_is_unknown
t_current_stamp_is_ready
t_commits_behind_counts_only_own_system
t_minutes_behind_is_timestamp_delta
t_dirty_stamp_flagged_and_still_measured
t_unknown_deployed_revision_errors_per_system
t_proto_commit_stales_every_system
t_daemon_binary_newer_than_process_is_stale
t_daemon_started_after_binary_is_fresh
t_service_fingerprint_mismatch_is_stale
t_service_fingerprint_match_is_fresh
t_processless_systems_report_null_running
t_elisp_is_not_reported
t_no_git_checkout_exits_nonzero
t_unknown_argument_exits_two

echo "-----"
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ]
