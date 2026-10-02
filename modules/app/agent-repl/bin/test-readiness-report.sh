#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312,SC2310
# Opt-in (`-o all`) style checks, declined for the same reasons spelled out at
# the top of build-frontend.sh.
#
# test-readiness-report.sh — hermetic tests for readiness-report.sh.
#
# Builds a throwaway repository around a copy of readiness-report.sh (the
# script is all git plumbing, so a scratch repo with commits is the honest
# fixture — the same approach test-build-frontend.sh takes with a scratch tree)
# and stubs `pgrep` and `ps` on PATH so no real process, launchd job, or
# machine state is consulted.
#
# NO REAL GIT RUNS HERE (owner rule). The repository is bin/fake-git.sh's model
# (history, index and working tree as files under <root>/.fakegit), installed as
# the only `git` on PATH for the harness AND the script it runs; the harness
# refuses to start if any other `git` would answer. Tests assert what the JSON says under each
# deployed-vs-source scenario.
#
# Every scenario also re-asserts that the document PARSES. "Valid JSON always,
# even on partial failure" is the contract Emacs polls against, and a report
# that goes syntactically wrong on an error path is worse than no report at
# all: the poller would fail silently every 15 seconds.
#
# Run with:   bash bin/test-readiness-report.sh

set -euo pipefail

# shellcheck source=lib-test-split.sh
. "$(dirname "${BASH_SOURCE[0]}")/lib-test-split.sh"
test_split_init "${BASH_SOURCE[0]}" "$@"

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

# A pre-commit hook exports its live index to children. The fake git ignores
# them, but nothing here should carry a binding to the caller's repository.
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_PREFIX
# The services' build reports live under $AGENT_REPL_LOCK_DIR when it is set.
# A value inherited from the caller would point every fixture at a real run
# directory, so the harness sets it only where a test means to.
unset AGENT_REPL_LOCK_DIR

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/readiness-report.sh"
LIB_UNDER_TEST="$THIS_DIR/lib-deploy-stamp.sh"

# The fake git, first on PATH for the whole run. Every invocation's argv is
# recorded in FAKE_GIT_LOG.
FAKE_GIT_BIN="$(mktemp -d)"
cp "$THIS_DIR/fake-git.sh" "$FAKE_GIT_BIN/git"
chmod +x "$FAKE_GIT_BIN/git"
export PATH="$FAKE_GIT_BIN:$PATH"
export FAKE_GIT_LOG="$FAKE_GIT_BIN/argv.log"
trap 'rm -rf "$FAKE_GIT_BIN"' EXIT
if [ "$(command -v git)" != "$FAKE_GIT_BIN/git" ]; then
    echo "test-readiness-report.sh: the fake git is not the git on PATH; refusing to run real git" >&2
    exit 2
fi

# The fixtures stamp with the SAME functions the report reads with, so a
# hand-rolled id here can never agree with a broken script.
# shellcheck source=lib-deploy-stamp.sh
. "$LIB_UNDER_TEST"

command -v python3 >/dev/null 2>&1 || {
    echo "test-readiness-report.sh: python3 is required to validate the JSON" >&2
    exit 2
}

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -n "${2:-}" ] && echo "       $2"; return 0; }

# --- fixture ----------------------------------------------------------------

# git_c ROOT ARGS... — git (the fake) run in the fixture ROOT.
git_c() {
    git -C "$1" "${@:2}"
}

# make_repo ROOT — a scratch git repo whose top level IS the module root, with
# one commit per system directory so every pathspec has distinct history.
#
# Commits are dated a fixed hour apart so minutes_behind is a deterministic
# number rather than whatever the clock did during the run.
make_repo() {
    local root="$1" n=0 d
    mkdir -p "$root/bin" "$root/daemon/bin" "$root/proto" \
             "$root/agent-shim/shim-store" \
             "$root/agent-shim/claude/shim/dist" \
             "$root/agent-shim/claude/shim-sidecar" \
             "$root/agent-shim/shim-lock" \
             "$root/webapp/dist" "$root/home/.cache/agent-repl/bin"
    cp "$SCRIPT_UNDER_TEST" "$root/bin/readiness-report.sh"
    cp "$LIB_UNDER_TEST" "$root/bin/lib-deploy-stamp.sh"

    # Mirror the real repo, where every build output is ignored. Without this a
    # fixture commit would SWEEP THE STAMPS INTO THE HISTORY, and a later revert
    # would delete the very stamps the test just wrote.
    printf 'bin/\ndist/\nhome/\nstubs/\nout.json\nerr.txt\n' > "$root/.gitignore"
    git_c "$root" init -q
    for d in proto agent-shim/shim-store \
             agent-shim/claude/shim agent-shim/claude/shim-sidecar \
             agent-shim/shim-lock webapp daemon; do
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

run_required_report() { # ROOT SYSTEM -> stdout on $OUT, exit status in $RC
    local root="$1" system="$2"
    set +e
    HOME="$root/home" PATH="$root/stubs:$PATH" \
        bash "$root/bin/readiness-report.sh" --require-ready "$system" >"$root/out.json" 2>"$root/err.txt"
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

# tree_stamp_path ROOT SYSTEM — where the system's `.source-tree` stamp lives in
# a fixture whose top level IS the module root and whose HOME is redirected.
tree_stamp_path() {
    case "$2" in
        daemon)  printf '%s' "$1/daemon/bin/.source-tree" ;;
        shim)    printf '%s' "$1/agent-shim/claude/shim/dist/.source-tree" ;;
        webapp)  printf '%s' "$1/webapp/dist/.source-tree" ;;
        *)       printf '%s' "$1/home/.cache/agent-repl/bin/.$2.source-tree" ;;
    esac
}

# stamp_tree ROOT SYSTEM — record that the system's artifact was built from the
# revision the fixture is standing at RIGHT NOW. This is the stamp the verdict
# turns on, and it is the same file build-frontend.sh writes and reads.
stamp_tree() {
    local paths
    paths="$(deploy_stamp_system_paths "$2" "")"
    # shellcheck disable=SC2086
    write_source_tree "$(tree_stamp_path "$1" "$2")" "$(source_tree_id "$1" $paths)"
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
    stamp_tree "$root" webapp
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
       && [ "$(jq_get "$OUT" 'len(d["systems"])')" = "6" ]; then
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
    # shim-lock is excluded by NAME, not by oversight: its go.mod requires only
    # agentrepl/logging, it speaks no wire at all, and so a proto regeneration
    # genuinely cannot stale it. Listing proto among its paths to make this
    # assertion uniform would report it behind over a change it never reads.
    if [ "$(jq_get "$OUT" "len(set(s['source_sha'] for s in d['systems'] if s['name'] != 'shim-lock')) == 1")" = "True" ] \
       && [ "$(jq_get "$OUT" 'sysmap["shim"]["source_sha"]')" = "$want" ]; then
        pass "a proto commit becomes the source revision of every Go/TS system"
    else
        fail "a proto commit becomes the source revision of every Go/TS system" \
             "want=$want out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# --- 7a. the runner's roster is a daemon build input ------------------------
# The daemon compiles testrun/roster in (the merge gate's suite selection), so
# a roster commit stales the daemon; the rest of testrun/ is no system's input.
t_runner_roster_commit_stales_only_the_daemon() {
    local root before; root="$(new_root)"
    before="$(head_sha "$root")"
    mkdir -p "$root/testrun/roster" "$root/testrun/internal"
    echo "package roster" > "$root/testrun/roster/roster.go"
    git_c "$root" add testrun/roster
    git_c "$root" commit -qm "roster change"
    local roster; roster="$(head_sha "$root")"
    echo "package sched" > "$root/testrun/internal/sched.go"
    git_c "$root" add testrun/internal
    git_c "$root" commit -qm "runner-only change"
    run_report "$root"
    if [ "$(jq_get "$OUT" 'sysmap["daemon"]["source_sha"]')" = "$roster" ] \
       && [ "$(jq_get "$OUT" "any(s['source_sha'] == '$roster' for s in d['systems'] if s['name'] != 'daemon')")" = "False" ] \
       && [ "$(jq_get "$OUT" "any(s['source_sha'] == '$(head_sha "$root")' for s in d['systems'])")" = "False" ]; then
        pass "a roster commit is the daemon's source revision and no other system's"
    else
        fail "a roster commit is the daemon's source revision and no other system's" \
             "before=$before roster=$roster out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# --- 7b. proto review artifacts are not build inputs ------------------------
# A commit touching only proto/figma-idl-draft/ or the sketch must not advance
# any system's source revision: no build reads them, so the staleness check
# never rebuilds for them and a gate counting them can never be satisfied.
t_proto_review_artifact_commit_stales_nothing() {
    local root before; root="$(new_root)"
    before="$(head_sha "$root")"
    mkdir -p "$root/proto/figma-idl-draft"
    echo "draft round" > "$root/proto/figma-idl-draft/feed.proto"
    echo "sketch note" >> "$root/proto/SKETCH-figma-idl.md"
    git_c "$root" add proto/figma-idl-draft proto/SKETCH-figma-idl.md
    git_c "$root" commit -qm "draft review round"
    run_report "$root"
    local draft; draft="$(head_sha "$root")"
    if [ "$draft" != "$before" ] \
       && [ "$(jq_get "$OUT" "any(s['source_sha'] == '$draft' for s in d['systems'])")" = "False" ]; then
        pass "a draft/sketch-only commit advances no system's source revision"
    else
        fail "a draft/sketch-only commit advances no system's source revision" \
             "draft=$draft out: $(cat "$OUT")"
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
    stamp_tree "$root" daemon
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

# write_service_report FILE PID BUILD — a report exactly as buildreport.Write
# lays it down: Go encoding/json's compact two-field object.
write_service_report() {
    mkdir -p "$(dirname "$1")"
    printf '{"pid":%s,"build":"%s"}' "$2" "$3" > "$1"
}

# --- 10. a launchd service reporting another build is stale -----------------
# The service's report (written at boot into the default run dir, since HOME is
# the fixture's) names a live pid running a DIFFERENT binary than the one
# installed: an install that restarted nothing.
t_service_fingerprint_mismatch_is_stale() {
    local root cache; root="$(new_root)"
    cache="$root/home/.cache/agent-repl/bin"
    stamp "$cache/.shim-store.built-sha" "$(head_sha "$root")"
    printf 'installed-v2' > "$cache/shim-store"
    write_service_report "$root/home/.cache/agent-repl/run/shim-store.build.json" \
        "$$" "$(printf 'installed-v1' | shasum -a 256 | cut -d' ' -f1)"
    set +e
    HOME="$root/home" PATH="$root/stubs:$PATH" \
        FAKE_PROC_MATCH="shim-store" FAKE_PROC_PID=555 \
        bash "$root/bin/readiness-report.sh" >"$root/out.json" 2>"$root/err.txt"
    RC=$?
    set -e
    OUT="$root/out.json"
    if [ "$(jq_get "$OUT" 'sysmap["shim-store"]["running"]["stale_binary"]')" = "True" ] \
       && [ "$(jq_get "$OUT" 'sysmap["shim-store"]["ready"]')" = "False" ]; then
        pass "a service whose build report names another binary than the installed one is stale"
    else
        fail "a service whose build report names another binary than the installed one is stale" \
             "rc=$RC out: $(cat "$OUT") err: $(cat "$root/err.txt")"
    fi
    rm -rf "$root"
}

# --- 11. a live service reporting the installed build is not stale ---------
t_service_fingerprint_match_is_fresh() {
    local root cache; root="$(new_root)"
    cache="$root/home/.cache/agent-repl/bin"
    stamp "$cache/.shim-store.built-sha" "$(head_sha "$root")"
    stamp_tree "$root" shim-store
    printf 'installed-v2' > "$cache/shim-store"
    write_service_report "$root/home/.cache/agent-repl/run/shim-store.build.json" \
        "$$" "$(shasum -a 256 "$cache/shim-store" | cut -d' ' -f1)"
    set +e
    HOME="$root/home" PATH="$root/stubs:$PATH" \
        FAKE_PROC_MATCH="shim-store" FAKE_PROC_PID=556 \
        bash "$root/bin/readiness-report.sh" >"$root/out.json" 2>"$root/err.txt"
    RC=$?
    set -e
    OUT="$root/out.json"
    if [ "$(jq_get "$OUT" 'sysmap["shim-store"]["running"]["stale_binary"]')" = "False" ] \
       && [ "$(jq_get "$OUT" 'sysmap["shim-store"]["ready"]')" = "True" ]; then
        pass "a live service whose build report is the installed binary's hash is not stale"
    else
        fail "a live service whose build report is the installed binary's hash is not stale" \
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

# --- 16. the gate passes with source/deployed identities in JSON ------------
t_required_ready_gate_passes_with_revisions() {
    local root sha; root="$(new_root)"
    sha="$(git_c "$root" log -1 --format=%H -- webapp proto)"
    stamp "$root/webapp/dist/.built-sha" "$sha"
    stamp_tree "$root" webapp
    run_required_report "$root" webapp
    if [ "$RC" -eq 0 ] \
       && [ "$(jq_get "$OUT" 'd["gate"]["ready"]')" = "True" ] \
       && [ "$(jq_get "$OUT" 'd["gate"]["deployed_sha"]')" = "$sha" ] \
       && [ "$(jq_get "$OUT" 'd["gate"]["source_sha"]')" = "$sha" ]; then
        pass "a current required system passes with both revisions in the structured gate"
    else
        fail "a current required system passes with both revisions in the structured gate" \
             "rc=$RC out: $(cat "$OUT") err: $(cat "$root/err.txt")"
    fi
    rm -rf "$root"
}

# --- 17. the gate keeps JSON but fails loudly on revision drift -------------
t_required_ready_gate_fails_with_revisions_on_drift() {
    local root deployed source; root="$(new_root)"
    deployed="$(git_c "$root" log -1 --format=%H -- webapp proto)"
    stamp "$root/webapp/dist/.built-sha" "$deployed"
    stamp_tree "$root" webapp
    touch_system "$root" webapp "webapp gate drift"
    source="$(head_sha "$root")"
    run_required_report "$root" webapp
    if [ "$RC" -eq 3 ] \
       && [ "$(jq_get "$OUT" 'd["gate"]["ready"]')" = "False" ] \
       && [ "$(jq_get "$OUT" 'd["gate"]["deployed_sha"]')" = "$deployed" ] \
       && [ "$(jq_get "$OUT" 'd["gate"]["source_sha"]')" = "$source" ] \
       && [ "$(jq_get "$OUT" '"built from source revision" in d["gate"]["error"]')" = "True" ]; then
        pass "a drifting required system exits nonzero with both revisions in valid JSON"
    else
        fail "a drifting required system exits nonzero with both revisions in valid JSON" \
             "rc=$RC out: $(cat "$OUT") err: $(cat "$root/err.txt")"
    fi
    rm -rf "$root"
}

# --- the verdict is the BUILD's staleness answer, off the same stamp --------
#
# On 2026-09-09 these were two separate computations — the build scanning
# hand-listed mtimes, the report counting commits over a pathspec — and they
# disagreed: the gate said the shim was three commits behind while every
# un-forced build said fresh and refused to rebuild it. These pin the four
# shapes of that disagreement shut.

# The defect itself: a commit under a path the old mtime scan never looked at.
# The report must call it not-ready, and the reason must name both revisions so
# the state is actionable from the JSON alone.
t_source_tree_drift_is_not_ready() {
    local root; root="$(new_root)"
    stamp "$root/webapp/dist/.built-sha" "$(head_sha "$root")"
    stamp_tree "$root" webapp
    touch_system "$root" webapp "webapp source moved on"
    run_report "$root"
    if [ "$(jq_get "$OUT" 'sysmap["webapp"]["source_tree_stale"]')" = "True" ] \
       && [ "$(jq_get "$OUT" 'sysmap["webapp"]["ready"]')" = "False" ] \
       && [ "$(jq_get "$OUT" '"built from source revision" in sysmap["webapp"]["error"]')" = "True" ]; then
        pass "a source set that moved past the artifact's stamp is not ready"
    else
        fail "a source set that moved past the artifact's stamp is not ready" \
             "out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# The mirror image, and the one that makes the gate CLEARABLE: a change and its
# own revert leave the system two commits behind with identical content. The
# build (correctly) will not rebuild that, so a gate that failed on the commit
# count would be a gate no rebuild could ever clear — exactly the trap `--force`
# had to be reached for. Distance is reported; readiness is not decided by it.
t_reverted_change_is_ready_though_commits_behind() {
    local root; root="$(new_root)"
    local deployed
    deployed="$(head_sha "$root")"
    touch_system "$root" webapp "webapp change"
    git_c "$root" revert --no-edit HEAD >/dev/null
    # Stamped from the state the artifact was built at, which the revert has
    # restored: same content, two commits later.
    stamp "$root/webapp/dist/.built-sha" "$deployed"
    stamp_tree "$root" webapp
    run_report "$root"
    if [ "$(jq_get "$OUT" 'sysmap["webapp"]["commits_behind"]')" = "2" ] \
       && [ "$(jq_get "$OUT" 'sysmap["webapp"]["source_tree_stale"]')" = "False" ] \
       && [ "$(jq_get "$OUT" 'sysmap["webapp"]["ready"]')" = "True" ]; then
        pass "a change and its revert are reported as distance but do not fail readiness"
    else
        fail "a change and its revert are reported as distance but do not fail readiness" \
             "out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# A dirty tree is REPORTED, never blocking. Refusing while a checkout has
# uncommitted work would refuse every deploy a developer makes from a working
# tree, and no rebuild could clear that either.
t_dirty_tree_is_flagged_but_still_ready() {
    local root; root="$(new_root)"
    stamp "$root/webapp/dist/.built-sha" "$(head_sha "$root")"
    stamp_tree "$root" webapp
    echo "uncommitted" >> "$root/webapp/file.txt"
    run_report "$root"
    if [ "$(jq_get "$OUT" 'sysmap["webapp"]["source_dirty"]')" = "True" ] \
       && [ "$(jq_get "$OUT" 'sysmap["webapp"]["ready"]')" = "True" ]; then
        pass "an uncommitted edit is flagged as dirty without failing readiness"
    else
        fail "an uncommitted edit is flagged as dirty without failing readiness" \
             "out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# A built-sha with no source-tree beside it is an artifact from a build that
# predates the stamp, and nothing can say what it was built from.
t_missing_source_tree_stamp_is_not_ready() {
    local root; root="$(new_root)"
    stamp "$root/webapp/dist/.built-sha" "$(head_sha "$root")"
    run_report "$root"
    if [ "$(jq_get "$OUT" 'sysmap["webapp"]["built_tree"]')" = "None" ] \
       && [ "$(jq_get "$OUT" 'sysmap["webapp"]["ready"]')" = "False" ] \
       && [ "$(jq_get "$OUT" '"source-tree" in sysmap["webapp"]["error"]')" = "True" ]; then
        pass "a built-sha with no .source-tree beside it is not ready"
    else
        fail "a built-sha with no .source-tree beside it is not ready" \
             "out: $(cat "$OUT")"
    fi
    rm -rf "$root"
}

# --- service_needs_bounce, straight off the library ---------------------------
#
# The one authority for "is this launchd service running the installed binary",
# read by this report and restated by nothing: the service's own build report.
# Each edge is one case. The reports go under AGENT_REPL_LOCK_DIR, the variable
# the services themselves honor.

# bounce_fixture — a temp dir holding an installed shim-store binary, with
# AGENT_REPL_LOCK_DIR pointed at its run dir. Sets BOUNCE_DIR, BOUNCE_BIN,
# BOUNCE_REPORT, BOUNCE_BUILD.
bounce_fixture() {
    BOUNCE_DIR="$(mktemp -d)"
    BOUNCE_BIN="$BOUNCE_DIR/bin"
    mkdir -p "$BOUNCE_BIN" "$BOUNCE_DIR/run"
    printf 'the-installed-store' > "$BOUNCE_BIN/shim-store"
    BOUNCE_BUILD="$(shasum -a 256 "$BOUNCE_BIN/shim-store" | cut -d' ' -f1)"
    BOUNCE_REPORT="$BOUNCE_DIR/run/shim-store.build.json"
}

# needs_bounce — run service_needs_bounce on the fixture; echo "bounce" or
# "fresh", stderr to $BOUNCE_DIR/err.
needs_bounce() {
    if AGENT_REPL_LOCK_DIR="$BOUNCE_DIR/run" \
           service_needs_bounce "$BOUNCE_BIN" shim-store 2>"$BOUNCE_DIR/err"; then
        echo bounce
    else
        echo fresh
    fi
}

# dead_pid — the pid of a child that has already exited and been reaped.
dead_pid() {
    local pid
    true & pid=$!
    wait "$pid"
    printf '%s' "$pid"
}

t_bounce_live_matching_report_is_fresh() {
    local got; bounce_fixture
    write_service_report "$BOUNCE_REPORT" "$$" "$BOUNCE_BUILD"
    got="$(needs_bounce)"
    if [ "$got" = fresh ] && [ ! -s "$BOUNCE_DIR/err" ]; then
        pass "bounce: a live pid reporting the installed binary's hash needs no bounce"
    else
        fail "bounce: a live pid reporting the installed binary's hash needs no bounce" \
             "got=$got err: $(cat "$BOUNCE_DIR/err")"
    fi
    rm -rf "$BOUNCE_DIR"
}

t_bounce_missing_binary_is_stale() {
    local got; bounce_fixture
    write_service_report "$BOUNCE_REPORT" "$$" "$BOUNCE_BUILD"
    rm -f "$BOUNCE_BIN/shim-store"
    got="$(needs_bounce)"
    if [ "$got" = bounce ]; then
        pass "bounce: a missing installed binary needs a bounce"
    else
        fail "bounce: a missing installed binary needs a bounce" "got=$got"
    fi
    rm -rf "$BOUNCE_DIR"
}

t_bounce_missing_report_is_stale() {
    local got; bounce_fixture
    got="$(needs_bounce)"
    if [ "$got" = bounce ]; then
        pass "bounce: a service with no build report needs a bounce"
    else
        fail "bounce: a service with no build report needs a bounce" "got=$got"
    fi
    rm -rf "$BOUNCE_DIR"
}

t_bounce_dead_pid_is_stale() {
    local got; bounce_fixture
    write_service_report "$BOUNCE_REPORT" "$(dead_pid)" "$BOUNCE_BUILD"
    got="$(needs_bounce)"
    if [ "$got" = bounce ]; then
        pass "bounce: a report whose pid is not alive needs a bounce"
    else
        fail "bounce: a report whose pid is not alive needs a bounce" "got=$got"
    fi
    rm -rf "$BOUNCE_DIR"
}

t_bounce_mismatched_hash_is_stale() {
    local got; bounce_fixture
    write_service_report "$BOUNCE_REPORT" "$$" \
        "$(printf 'the-previous-store' | shasum -a 256 | cut -d' ' -f1)"
    got="$(needs_bounce)"
    if [ "$got" = bounce ]; then
        pass "bounce: a report naming another build than the installed binary needs a bounce"
    else
        fail "bounce: a report naming another build than the installed binary needs a bounce" \
             "got=$got"
    fi
    rm -rf "$BOUNCE_DIR"
}

# A report that exists and does not parse is a FAULT: stale, and said so on
# stderr naming the file, never silently fresh.
t_bounce_unparseable_report_is_stale_and_warns() {
    local got; bounce_fixture
    printf '{"pid":%s,"build":' "$$" > "$BOUNCE_REPORT"
    got="$(needs_bounce)"
    if [ "$got" = bounce ] && grep -q "WARNING" "$BOUNCE_DIR/err" &&
           grep -qF "$BOUNCE_REPORT" "$BOUNCE_DIR/err"; then
        pass "bounce: an unparseable report needs a bounce and warns naming the file"
    else
        fail "bounce: an unparseable report needs a bounce and warns naming the file" \
             "got=$got err: $(cat "$BOUNCE_DIR/err")"
    fi
    rm -rf "$BOUNCE_DIR"
}

readiness_group_build_distance() {
    t_missing_stamp_is_unknown
    t_current_stamp_is_ready
    t_commits_behind_counts_only_own_system
    t_minutes_behind_is_timestamp_delta
    t_dirty_stamp_flagged_and_still_measured
    t_unknown_deployed_revision_errors_per_system
}

readiness_group_inputs() {
    t_proto_commit_stales_every_system
    t_runner_roster_commit_stales_only_the_daemon
    t_proto_review_artifact_commit_stales_nothing
}

readiness_group_processes() {
    t_daemon_binary_newer_than_process_is_stale
    t_daemon_started_after_binary_is_fresh
    t_service_fingerprint_mismatch_is_stale
    t_service_fingerprint_match_is_fresh
    t_processless_systems_report_null_running
    t_elisp_is_not_reported
}

readiness_group_cli() {
    t_no_git_checkout_exits_nonzero
    t_unknown_argument_exits_two
    t_required_ready_gate_passes_with_revisions
    t_required_ready_gate_fails_with_revisions_on_drift
}

readiness_group_source_tree() {
    t_source_tree_drift_is_not_ready
    t_reverted_change_is_ready_though_commits_behind
    t_dirty_tree_is_flagged_but_still_ready
    t_missing_source_tree_stamp_is_not_ready
}

readiness_group_bounce() {
    t_bounce_live_matching_report_is_fresh
    t_bounce_missing_binary_is_stale
    t_bounce_missing_report_is_stale
    t_bounce_dead_pid_is_stale
    t_bounce_mismatched_hash_is_stale
    t_bounce_unparseable_report_is_stale_and_warns
}

test_split_run build-distance readiness_group_build_distance
test_split_run inputs readiness_group_inputs
test_split_run processes readiness_group_processes
test_split_run cli readiness_group_cli
test_split_run source-tree readiness_group_source_tree
test_split_run bounce readiness_group_bounce

echo "-----"
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ]
