#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312
# Opt-in (`-o all`) style checks, declined for the reasons spelled out at the
# top of agent_repl_workspace_open. This harness is clean at the default bar
# plus `-S style` with no suppressions at all.

# test-agent_repl_workspace_open.sh — hermetic tests for
# agent_repl_workspace_open's repository-root resolution, its two listing
# passthroughs, its selection paths, and the claude command line it builds.
#
# No real daemon and no real claude run: AGENT_REPL_DAEMON_BIN points at a stub
# that RECORDS the subcommand and flags it was given and prints canned
# tab-separated listings, and AGENT_REPL_CLAUDE_BIN points at a stub that
# records the directory it was exec'd in, its argv, and its CLAUDE_CONFIG_DIR.
#
# Run with:   bash bin/test-agent_repl_workspace_open.sh

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

# A pre-commit hook exports its live index to children. The fake git ignores
# them, but nothing here should carry a binding to the caller's repository.
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_PREFIX

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/agent_repl_workspace_open"

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -n "${2:-}" ] && echo "       $2"; }

WORK="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-workspace-test.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

WS_ALPHA="$WORK/ws/alpha"
WS_BETA="$WORK/ws/beta"
mkdir -p "$WS_ALPHA" "$WS_BETA"
CHESSCOM_CONFIG="$WORK/.claude-chesscom"

# --- stubs ------------------------------------------------------------------
# The daemon stub appends each invocation's argv to $RECORD_ARGS and answers the
# two listing subcommands in their documented tab-separated formats. It logs to
# stderr first, so a script reading anything but stdout would pick up the log.
STUB_BIN="$WORK/stubs"
mkdir -p "$STUB_BIN"
cat > "$STUB_BIN/claude-repld" <<STUB
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "\$@" >> "\$RECORD_ARGS"
echo "a log line that must not be read as a row" >&2
case "\$1" in
    list-workspaces)
        printf 'alpha\t$WS_ALPHA\talpha\tcreated\n'
        printf 'beta\t$WS_BETA\tbeta\tbackfilled\n'
        ;;
    list-transcripts)
        printf 'uuid-newest\t$CHESSCOM_CONFIG\t2026-08-06T10:00:00Z\t42\tok\tteach the CLI to open a workspace\n'
        printf 'uuid-older\t$CHESSCOM_CONFIG\t2026-08-05T10:00:00Z\t3\tempty\t-\n'
        ;;
    *) echo "unexpected subcommand \$1" >&2; exit 64 ;;
esac
STUB
cat > "$STUB_BIN/claude" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
pwd -P > "$RECORD_CWD"
printf '%s\n' "$@" >> "$RECORD_CWD"
echo "CLAUDE_CONFIG_DIR=${CLAUDE_CONFIG_DIR:-}" >> "$RECORD_CWD"
STUB
chmod +x "$STUB_BIN/claude-repld" "$STUB_BIN/claude"

export AGENT_REPL_DAEMON_BIN="$STUB_BIN/claude-repld"
export AGENT_REPL_CLAUDE_BIN="$STUB_BIN/claude"

# --- fixture repository -----------------------------------------------------
# A main worktree plus one linked worktree, which is the shape every workspace
# this script is run from has.
#
# NO REAL GIT RUNS HERE (owner rule). The repository is bin/fake-git.sh's model,
# installed as the only `git` on PATH for the harness AND the script it runs;
# the harness refuses to start if any other `git` would answer.
FAKE_GIT_BIN="$WORK/fake-git-bin"
mkdir -p "$FAKE_GIT_BIN"
ln -s "$THIS_DIR/../modules/app/agent-repl/bin/fake-git.sh" "$FAKE_GIT_BIN/git"
export PATH="$FAKE_GIT_BIN:$PATH"
if [ "$(command -v git)" != "$FAKE_GIT_BIN/git" ]; then
    echo "$(basename "$0"): the fake git is not the git on PATH; refusing to run real git" >&2
    exit 2
fi
git_q() { git -C "$@"; }
REPO="$WORK/repo"
mkdir -p "$REPO"
git_q "$REPO" init --quiet
echo seed > "$REPO/seed.txt"
git_q "$REPO" add seed.txt
git_q "$REPO" commit --quiet -m seed
LINKED="$WORK/linked"
git_q "$REPO" worktree add --quiet -b linked-branch "$LINKED" >/dev/null
mkdir -p "$LINKED/deep/nested"

MAIN_PHYSICAL="$(cd "$REPO" && pwd -P)"

# run_from DIR SELECTIONS ARG... — run the script from DIR feeding SELECTIONS
# (newline-separated menu answers) on stdin, capturing the recorded argv.
run_from() {
    local dir="$1" selections="$2"
    shift 2
    : > "$WORK/args"
    : > "$WORK/cwd"
    printf '%s' "$selections" |
        RECORD_ARGS="$WORK/args" RECORD_CWD="$WORK/cwd" \
        bash -c 'cd "$1" && shift && exec "$@"' _ "$dir" "$SCRIPT_UNDER_TEST" "$@"
}

# recorded_flag FLAG — the value the daemon stub was given for FLAG.
recorded_flag() {
    awk -v flag="$1" '$0 == flag { getline; print; exit }' "$WORK/args"
}

# --- tests ------------------------------------------------------------------

# THE CASE THIS SCRIPT EXISTS FOR: run from a workspace, the listing must still
# be the repository's, which means resolving its MAIN worktree.
if run_from "$LINKED" $'1\n1\n' >/dev/null 2>&1 &&
       [ "$(recorded_flag --git-root)" = "$MAIN_PHYSICAL" ]; then
    pass "run from a linked worktree lists the repository's MAIN worktree"
else
    fail "run from a linked worktree lists the repository's MAIN worktree" \
         "got $(recorded_flag --git-root), want $MAIN_PHYSICAL"
fi

if run_from "$LINKED/deep/nested" $'1\n1\n' >/dev/null 2>&1 &&
       [ "$(recorded_flag --git-root)" = "$MAIN_PHYSICAL" ]; then
    pass "run from a subdirectory of a worktree resolves the main worktree"
else
    fail "run from a subdirectory of a worktree resolves the main worktree" \
         "got $(recorded_flag --git-root), want $MAIN_PHYSICAL"
fi

if run_from "$REPO" $'2\n1\n' >/dev/null 2>&1 &&
       [ "$(head -1 "$WORK/cwd")" = "$(cd "$WS_BETA" && pwd -P)" ]; then
    pass "the workspace menu selection picks that workspace's path"
else
    fail "the workspace menu selection picks that workspace's path" "got $(head -1 "$WORK/cwd")"
fi

# A named workspace skips the first menu entirely, so the sole stdin line is
# the conversation choice.
if run_from "$REPO" $'1\n' beta >/dev/null 2>&1 &&
       [ "$(head -1 "$WORK/cwd")" = "$(cd "$WS_BETA" && pwd -P)" ]; then
    pass "the sole argument selects the workspace by name"
else
    fail "the sole argument selects the workspace by name" "got $(head -1 "$WORK/cwd")"
fi

# The selected workspace must be the one whose transcripts are listed.
if run_from "$REPO" $'1\n1\n' beta >/dev/null 2>&1 &&
       [ "$(recorded_flag --workspace)" = "$WS_BETA" ]; then
    pass "list-transcripts is asked for the selected workspace"
else
    fail "list-transcripts is asked for the selected workspace" \
         "got $(recorded_flag --workspace), want $WS_BETA"
fi

if run_from "$REPO" $'1\n1\n' >/dev/null 2>&1 &&
       grep -qx -- "--resume" "$WORK/cwd" && grep -qx uuid-newest "$WORK/cwd"; then
    pass "picking the first conversation resumes its uuid"
else
    fail "picking the first conversation resumes its uuid" "$(cat "$WORK/cwd")"
fi

# The config dir travels with the transcript, or the resumed uuid is invisible
# to the CLI that is handed it.
if run_from "$REPO" $'1\n1\n' >/dev/null 2>&1 &&
       grep -qx "CLAUDE_CONFIG_DIR=$CHESSCOM_CONFIG" "$WORK/cwd"; then
    pass "a resume exports the transcript's own config dir"
else
    fail "a resume exports the transcript's own config dir" "$(cat "$WORK/cwd")"
fi

# The NEW-conversation entry is last, after the two listed transcripts.
if run_from "$REPO" $'1\n3\n' >/dev/null 2>&1 &&
       ! grep -qx -- "--resume" "$WORK/cwd"; then
    pass "picking the new-conversation entry passes no --resume"
else
    fail "picking the new-conversation entry passes no --resume" "$(cat "$WORK/cwd")"
fi

if run_from "$REPO" $'1\n3\n' >/dev/null 2>&1 &&
       grep -qx -- "--permission-mode" "$WORK/cwd" && grep -qx auto "$WORK/cwd"; then
    pass "claude is exec'd with --permission-mode auto"
else
    fail "claude is exec'd with --permission-mode auto" "$(cat "$WORK/cwd")"
fi

if run_from "$REPO" $'1\n1\n' no-such-workspace >/dev/null 2>&1; then
    fail "an unknown workspace name fails loudly" "the script succeeded"
else
    pass "an unknown workspace name fails loudly"
fi

if run_from "$REPO" $'9\n' >/dev/null 2>&1; then
    fail "an out-of-range selection fails loudly" "the script succeeded"
else
    pass "an out-of-range selection fails loudly"
fi

if run_from "$REPO" $'nope\n' >/dev/null 2>&1; then
    fail "a non-numeric selection fails loudly" "the script succeeded"
else
    pass "a non-numeric selection fails loudly"
fi

if run_from "$REPO" '' >/dev/null 2>&1; then
    fail "no selection at all fails loudly" "the script succeeded"
else
    pass "no selection at all fails loudly"
fi

OUTSIDE="$WORK/outside"
mkdir -p "$OUTSIDE"
if run_from "$OUTSIDE" $'1\n1\n' >/dev/null 2>&1; then
    fail "running outside a repository fails loudly" "the script succeeded"
else
    pass "running outside a repository fails loudly"
fi

if run_from "$REPO" $'1\n1\n' one two >/dev/null 2>&1; then
    fail "extra arguments fail loudly" "the script succeeded"
else
    pass "extra arguments fail loudly"
fi

# An empty listing is a hard failure, not a menu with nothing in it.
EMPTY_STUB="$WORK/stubs-empty"
mkdir -p "$EMPTY_STUB"
cat > "$EMPTY_STUB/claude-repld" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" >> "$RECORD_ARGS"
STUB
chmod +x "$EMPTY_STUB/claude-repld"
if AGENT_REPL_DAEMON_BIN="$EMPTY_STUB/claude-repld" run_from "$REPO" $'1\n1\n' >/dev/null 2>&1; then
    fail "a repository with no workspaces fails loudly" "the script succeeded"
else
    pass "a repository with no workspaces fails loudly"
fi

# The state-db knob must reach the listing, or a test database is silently
# ignored in favor of the real one.
if AGENT_REPL_WORKSPACE_STATE_DB="$WORK/state.db" run_from "$REPO" $'1\n1\n' >/dev/null 2>&1 &&
       [ "$(recorded_flag --state-db)" = "$WORK/state.db" ]; then
    pass "AGENT_REPL_WORKSPACE_STATE_DB reaches list-workspaces"
else
    fail "AGENT_REPL_WORKSPACE_STATE_DB reaches list-workspaces" "got $(recorded_flag --state-db)"
fi

if AGENT_REPL_INCLUDE_UNMANAGED=1 run_from "$REPO" $'1\n1\n' >/dev/null 2>&1 &&
       grep -qx -- "--include-unmanaged" "$WORK/args"; then
    pass "AGENT_REPL_INCLUDE_UNMANAGED reaches list-workspaces"
else
    fail "AGENT_REPL_INCLUDE_UNMANAGED reaches list-workspaces" "$(cat "$WORK/args")"
fi

echo
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ]
