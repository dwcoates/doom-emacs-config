#!/usr/bin/env bash

# shellcheck disable=SC2250,SC2292,SC2312
# Opt-in (`-o all`) style checks, declined for the reasons spelled out at the
# top of agent_repl_workspace. This harness is clean at the default bar plus
# `-S style` with no suppressions at all.

# test-agent_repl_workspace.sh — hermetic tests for agent_repl_workspace's
# repository-root resolution and its handoff into the created worktree.
#
# No real daemon and no real claude run: AGENT_REPL_DAEMON_BIN points at a stub
# that RECORDS the flags it was given and creates the worktree directory it
# reports, and AGENT_REPL_CLAUDE_BIN points at a stub that records the directory
# it was exec'd in. Tests then assert WHICH --git-root the script resolved from
# each starting directory.
#
# Run with:   bash bin/test-agent_repl_workspace.sh

set -euo pipefail

# A pre-commit hook exports its live index to children. This harness owns only
# scratch repositories, so inheriting that binding would let fixture `git add`
# and `git commit` rewrite the caller's real staging index.
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_PREFIX

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_UNDER_TEST="$THIS_DIR/agent_repl_workspace"

PASS=0
FAIL=0
pass() { PASS=$((PASS + 1)); echo "ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "FAIL - $1"; [ -n "${2:-}" ] && echo "       $2"; }

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# --- stubs ------------------------------------------------------------------
# The daemon stub writes its argv to $RECORD_ARGS, creates the worktree it
# claims, and prints that path as its last stdout line, exactly as
# `claude-repld create-workspace` does.
STUB_BIN="$WORK/stubs"
mkdir -p "$STUB_BIN"
cat > "$STUB_BIN/claude-repld" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "$RECORD_ARGS"
name=""
while [ $# -gt 0 ]; do
    case "$1" in
        --name) name="$2"; shift 2 ;;
        *) shift ;;
    esac
done
echo "a log line that must not be read as a path" >&2
mkdir -p "$STUB_WORKTREE/$name"
echo "$STUB_WORKTREE/$name"
STUB
cat > "$STUB_BIN/claude" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
pwd -P > "$RECORD_CWD"
printf '%s\n' "$@" >> "$RECORD_CWD"
STUB
chmod +x "$STUB_BIN/claude-repld" "$STUB_BIN/claude"

export AGENT_REPL_DAEMON_BIN="$STUB_BIN/claude-repld"
export AGENT_REPL_CLAUDE_BIN="$STUB_BIN/claude"
export STUB_WORKTREE="$WORK/created"

# --- fixture repository -----------------------------------------------------
# A main worktree plus one linked worktree, which is the shape every workspace
# this script is run from has.
git_q() { git -c user.name=t -c user.email=t@t.invalid -c commit.gpgsign=false -C "$@"; }
REPO="$WORK/repo"
mkdir -p "$REPO"
git_q "$REPO" init --quiet
git_q "$REPO" symbolic-ref HEAD refs/heads/master
echo seed > "$REPO/seed.txt"
git_q "$REPO" add seed.txt
git_q "$REPO" commit --quiet -m seed
LINKED="$WORK/linked"
git_q "$REPO" worktree add --quiet -b linked-branch "$LINKED" >/dev/null
mkdir -p "$LINKED/deep/nested"

MAIN_PHYSICAL="$(cd "$REPO" && pwd -P)"

# run_from NAME DIR — run the script from DIR, capturing the recorded argv.
run_from() {
    local dir="$1" name="$2"
    RECORD_ARGS="$WORK/args" RECORD_CWD="$WORK/cwd" \
        bash -c 'cd "$1" && shift && exec "$@"' _ "$dir" "$SCRIPT_UNDER_TEST" "$name"
}

# recorded_flag FLAG — the value the daemon stub was given for FLAG.
recorded_flag() {
    awk -v flag="$1" '$0 == flag { getline; print; exit }' "$WORK/args"
}

# --- tests ------------------------------------------------------------------

# The main worktree is the trivial case, and it is still the case the daemon's
# --git-root contract is defined in terms of.
if run_from "$REPO" ws-from-main >/dev/null 2>&1 &&
       [ "$(recorded_flag --git-root)" = "$MAIN_PHYSICAL" ]; then
    pass "run from the main worktree passes the main worktree as --git-root"
else
    fail "run from the main worktree passes the main worktree as --git-root" \
         "got $(recorded_flag --git-root), want $MAIN_PHYSICAL"
fi

# THE CASE THIS SCRIPT EXISTS FOR: run from a workspace, the new worktree must
# still be cut from the repository's main worktree, not from the workspace.
if run_from "$LINKED" ws-from-linked >/dev/null 2>&1 &&
       [ "$(recorded_flag --git-root)" = "$MAIN_PHYSICAL" ]; then
    pass "run from a linked worktree resolves the repository's MAIN worktree"
else
    fail "run from a linked worktree resolves the repository's MAIN worktree" \
         "got $(recorded_flag --git-root), want $MAIN_PHYSICAL"
fi

if run_from "$LINKED/deep/nested" ws-from-subdir >/dev/null 2>&1 &&
       [ "$(recorded_flag --git-root)" = "$MAIN_PHYSICAL" ]; then
    pass "run from a subdirectory of a worktree resolves the main worktree"
else
    fail "run from a subdirectory of a worktree resolves the main worktree" \
         "got $(recorded_flag --git-root), want $MAIN_PHYSICAL"
fi

if run_from "$REPO" ws-name-passed >/dev/null 2>&1 &&
       [ "$(recorded_flag --name)" = "ws-name-passed" ]; then
    pass "the sole argument is passed through as --name"
else
    fail "the sole argument is passed through as --name" "got $(recorded_flag --name)"
fi

# The stub logs to stderr before printing the path; reading anything but the
# last stdout line would cd into that log line.
if run_from "$REPO" ws-handoff >/dev/null 2>&1 &&
       [ "$(head -1 "$WORK/cwd")" = "$(cd "$STUB_WORKTREE/ws-handoff" && pwd -P)" ]; then
    pass "claude is exec'd inside the created worktree"
else
    fail "claude is exec'd inside the created worktree" "got $(head -1 "$WORK/cwd")"
fi

if run_from "$REPO" ws-perm-mode >/dev/null 2>&1 &&
       grep -qx -- "--permission-mode" "$WORK/cwd" && grep -qx auto "$WORK/cwd"; then
    pass "claude is exec'd with --permission-mode auto"
else
    fail "claude is exec'd with --permission-mode auto" "$(cat "$WORK/cwd")"
fi

OUTSIDE="$WORK/outside"
mkdir -p "$OUTSIDE"
if run_from "$OUTSIDE" ws-outside >/dev/null 2>&1; then
    fail "running outside a repository fails loudly" "the script succeeded"
else
    pass "running outside a repository fails loudly"
fi

if ( cd "$REPO" && RECORD_ARGS="$WORK/args" RECORD_CWD="$WORK/cwd" "$SCRIPT_UNDER_TEST" ) >/dev/null 2>&1; then
    fail "a missing workspace name fails loudly" "the script succeeded"
else
    pass "a missing workspace name fails loudly"
fi

if ( cd "$REPO" && RECORD_ARGS="$WORK/args" RECORD_CWD="$WORK/cwd" "$SCRIPT_UNDER_TEST" one two ) >/dev/null 2>&1; then
    fail "extra arguments fail loudly" "the script succeeded"
else
    pass "extra arguments fail loudly"
fi

echo
echo "passed: $PASS  failed: $FAIL"
[ "$FAIL" -eq 0 ]
