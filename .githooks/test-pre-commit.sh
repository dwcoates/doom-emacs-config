#!/usr/bin/env bash
# test-pre-commit.sh — hermetic tests for .githooks/pre-commit.
#
# The hook's ONLY gate is the static external-boundary lint.  The unified
# agent-repl test suite is deliberately NOT run here any more: that gate moved
# into the workspace-merge machinery (merge.Driver's per-commit pick loop plus
# merge.SuiteRunner), which tests each cherry-picked commit as it lands on the
# target.  Every fixture below therefore asserts BOTH halves — the lint ran,
# and the suite runner was never invoked.
#
# NO REAL GIT (owner rule: no test of any form runs real git).  Every fixture
# repository is a bin/fake-git.sh repository, `git` on the hook's PATH is that
# fake, and the hook is run the way git runs a pre-commit hook: from the
# worktree's top, out of the repository's hooks directory.  Every git call the
# hook makes is logged ($FAKE_GIT_LOG) and asserted, so a new call fails here
# loudly (the fake exits 2 on anything it does not model) rather than reaching
# a real repository.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail
# shellcheck source=/dev/null
. "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/lib-grep-in.sh"

# This harness may itself be run from a git hook.  Clear the caller's live Git
# bindings so nothing below can read them as its own.
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_PREFIX

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
HOOK_SRC="$THIS_DIR/pre-commit"
FAKE_GIT="$THIS_DIR/../modules/app/agent-repl/bin/fake-git.sh"

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-precommit-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# The only `git` any fixture or the hook can reach.
FAKE_BIN="$TMP/bin"
mkdir -p "$FAKE_BIN"
ln -s "$FAKE_GIT" "$FAKE_BIN/git"
export PATH="$FAKE_BIN:$PATH"
PASS=0
FAIL=0

pass() {
  printf '  PASS: %s\n' "$1"
  PASS=$((PASS + 1))
}

fail() {
  printf '  FAIL: %s\n' "$1" >&2
  FAIL=$((FAIL + 1))
  shift
  if [ "$#" -gt 0 ]; then
    printf '%s\n' "$@" | sed 's/^/        /' >&2
  fi
}

mkrepo() {
  local module="${1:-agent-repl}"
  local repo
  repo="$(mktemp -d "$TMP/repo.XXXXXXXXXX")"
  git -C "$repo" init

  # The lint script IS the hook's gate.  It records that it ran and honors an
  # injected exit code so a fixture can drive the refusal path.
  mkdir -p "$repo/.claude"
  cat >"$repo/.claude/check-external-boundaries.sh" <<'EOF'
#!/usr/bin/env bash
printf 'lint\n' >"${HOOK_TEST_LINT_LOG:?HOOK_TEST_LINT_LOG is required}"
exit "${HOOK_TEST_LINT_EXIT:-0}"
EOF
  chmod +x "$repo/.claude/check-external-boundaries.sh"

  # The unified runner exists in the fixture ON PURPOSE: its absence would
  # make "the suite did not run" trivially true.  Every assertion below is
  # therefore about the hook CHOOSING not to run a runner that is right there.
  mkdir -p "$repo/modules/app/$module/bin"
  touch "$repo/modules/app/$module/test-$module.el"
  cat >"$repo/modules/app/$module/bin/test-all.sh" <<'EOF'
#!/usr/bin/env bash
printf 'unified\n' >"${HOOK_TEST_RUN_LOG:?HOOK_TEST_RUN_LOG is required}"
exit "${HOOK_TEST_RUN_EXIT:-0}"
EOF
  chmod +x "$repo/modules/app/$module/bin/test-all.sh"

  mkdir -p "$repo/.fakegit/hooks"
  cp "$HOOK_SRC" "$repo/.fakegit/hooks/pre-commit"
  chmod +x "$repo/.fakegit/hooks/pre-commit"
  printf '%s\n' "$repo"
}

stage_module_file() {
  local repo="$1"
  local relative="$2"
  local path="$repo/modules/app/agent-repl/$relative"
  mkdir -p "$(dirname "$path")"
  printf 'test content\n' >"$path"
  git -C "$repo" add "$path"
}

# run_hook WORKTREE HOOK [LINT_EXIT [SUITE_EXIT]] — run HOOK as git runs a
# pre-commit hook for a commit in WORKTREE: from the worktree's top.  RUN_RC
# and RUN_OUT hold its outcome and GIT_CALLS the git calls it made.
run_hook() {
  local worktree="$1"
  local hook="$2"
  local lint_exit="${3:-0}"
  local suite_exit="${4:-0}"
  RUN_LOG="$worktree/unified-called"
  LINT_LOG="$worktree/lint-called"
  local git_log="$worktree/git-calls"
  rm -f "$RUN_LOG" "$LINT_LOG" "$git_log"
  set +e
  RUN_OUT="$(
    cd "$worktree" &&
      FAKE_GIT_LOG="$git_log" \
        HOOK_TEST_RUN_LOG="$RUN_LOG" \
        HOOK_TEST_RUN_EXIT="$suite_exit" \
        HOOK_TEST_LINT_LOG="$LINT_LOG" \
        HOOK_TEST_LINT_EXIT="$lint_exit" \
        "$hook" 2>&1
  )"
  RUN_RC=$?
  set -e
  GIT_CALLS="$(cat "$git_log" 2>/dev/null || true)"
}

run_commit() {
  run_hook "$1" "$1/.fakegit/hooks/pre-commit" "${2:-0}"
}

# The git calls a hook that reaches its gate makes, in order: where the
# repository is, which repository owns the hook, whether a cherry-pick is
# being replayed, and what is staged.  Nothing else -- no branch, no history.
GATED_CALLS="git rev-parse --show-toplevel
git rev-parse --git-common-dir
git rev-parse --git-dir
git diff --cached --name-only"

# assert_gated NAME — the commit succeeded, the lint ran, the suite did not,
# and the hook asked git exactly what a gated commit asks.
assert_gated() {
  if [ "$RUN_RC" -eq 0 ] && [ -f "$LINT_LOG" ] && [ ! -f "$RUN_LOG" ] && [ "$GIT_CALLS" = "$GATED_CALLS" ]; then
    pass "$1"
  else
    fail "$1" "exit=$RUN_RC lint_ran=$([ -f "$LINT_LOG" ] && echo yes || echo no) suite_ran=$([ -f "$RUN_LOG" ] && echo yes || echo no)" \
      "git calls:" "$GIT_CALLS" "$RUN_OUT"
  fi
}

test_cherry_pick_skips_the_gate() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "src/dummy.ts"
  touch "$repo/.fakegit/CHERRY_PICK_HEAD"
  run_commit "$repo"

  if [ ! -f "$LINT_LOG" ] && grep_in "$RUN_OUT" -q "Cherry-pick detected" &&
    ! grep_in "$GIT_CALLS" -q "^git diff"; then
    pass "cherry-pick replay skips the gate"
  else
    fail "cherry-pick replay skips the gate" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_direct_master_commit_runs_the_lint() {
  # A commit on master is gated like any other: the hook never asks which
  # branch it is on (assert_gated pins its git calls, and none names HEAD's
  # branch), so no branch can be exempt.
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "internal/dummy.go"
  run_commit "$repo"
  assert_gated "direct master commit runs the boundary lint"
  rm -rf "$repo"
}

test_elisp_change_runs_the_lint() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "dummy.el"
  run_commit "$repo"
  assert_gated "Elisp change runs the boundary lint"
  rm -rf "$repo"
}

test_typescript_change_runs_the_lint() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "webapp/src/dummy.ts"
  run_commit "$repo"
  assert_gated "TypeScript change runs the boundary lint"
  rm -rf "$repo"
}

test_go_change_runs_the_lint() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "daemon/internal/dummy.go"
  run_commit "$repo"
  assert_gated "Go change runs the boundary lint"
  rm -rf "$repo"
}

test_proto_change_runs_the_lint() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "proto/dummy.proto"
  run_commit "$repo"
  assert_gated "proto change runs the boundary lint"
  rm -rf "$repo"
}

test_package_manifest_runs_the_lint() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "webapp/package.json"
  run_commit "$repo"
  assert_gated "TypeScript package manifest runs the boundary lint"
  rm -rf "$repo"
}

test_hook_change_runs_the_lint() {
  local repo
  repo="$(mkrepo)"
  mkdir -p "$repo/.githooks"
  printf '# changed hook\n' >"$repo/.githooks/pre-commit"
  git -C "$repo" add "$repo/.githooks/pre-commit"
  run_commit "$repo"
  assert_gated "hook change runs the boundary lint"
  rm -rf "$repo"
}

test_shell_harness_change_runs_the_lint() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "bin/test-readiness-report.sh"
  run_commit "$repo"
  assert_gated "shell harness change runs the boundary lint"
  rm -rf "$repo"
}

test_unrelated_docs_skip_the_gate() {
  local repo
  repo="$(mkrepo)"
  printf 'docs\n' >"$repo/README.md"
  git -C "$repo" add "$repo/README.md"
  run_commit "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ ! -f "$LINT_LOG" ]; then
    pass "unrelated documentation skips the gate"
  else
    fail "unrelated documentation skips the gate" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_foreign_repo_skips_shared_hook() {
  local owner foreign
  owner="$(mkrepo)"
  foreign="$(mktemp -d "$TMP/foreign.XXXXXXXXXX")"
  git -C "$foreign" init
  mkdir -p "$foreign/modules/app/agent-repl"
  printf 'foreign fixture\n' >"$foreign/modules/app/agent-repl/dummy.ts"
  git -C "$foreign" add modules/app/agent-repl/dummy.ts

  # The OWNER's installed hook, inherited by a commit in the foreign repo (an
  # absolute core.hooksPath does exactly this).
  run_hook "$foreign" "$owner/.fakegit/hooks/pre-commit"

  if [ "$RUN_RC" -eq 0 ] && [ ! -f "$LINT_LOG" ] &&
    ! grep_in "$GIT_CALLS" -q "^git diff"; then
    pass "foreign repository skips an inherited shared hook"
  else
    fail "foreign repository skips an inherited shared hook" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$owner" "$foreign"
}

test_lint_failure_blocks_commit() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "webapp/src/failing.ts"
  run_commit "$repo" 7

  if [ "$RUN_RC" -ne 0 ] &&
    [ -f "$LINT_LOG" ] &&
    grep_in "$RUN_OUT" -q "refusing commit"; then
    pass "boundary lint failure blocks the commit"
  else
    fail "boundary lint failure blocks the commit" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_missing_lint_blocks_commit() {
  local repo
  repo="$(mkrepo)"
  rm "$repo/.claude/check-external-boundaries.sh"
  stage_module_file "$repo" "daemon/missing-lint.go"
  run_commit "$repo"

  if [ "$RUN_RC" -ne 0 ] &&
    grep_in "$RUN_OUT" -q "external-boundary lint is missing"; then
    pass "missing boundary lint blocks the commit"
  else
    fail "missing boundary lint blocks the commit" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_a_failing_suite_no_longer_blocks_commit() {
  # The whole point of the change: the unified suite is not consulted, so a
  # runner that would have exited non-zero cannot refuse a commit any more.
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "daemon/internal/would-have-failed.go"
  run_hook "$repo" "$repo/.fakegit/hooks/pre-commit" 0 7

  if [ "$RUN_RC" -eq 0 ] && [ ! -f "$RUN_LOG" ]; then
    pass "a failing unified suite no longer blocks the commit"
  else
    fail "a failing unified suite no longer blocks the commit" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_cherry_pick_skips_the_gate
test_direct_master_commit_runs_the_lint
test_elisp_change_runs_the_lint
test_typescript_change_runs_the_lint
test_go_change_runs_the_lint
test_proto_change_runs_the_lint
test_package_manifest_runs_the_lint
test_hook_change_runs_the_lint
test_shell_harness_change_runs_the_lint
test_unrelated_docs_skip_the_gate
test_foreign_repo_skips_shared_hook
test_lint_failure_blocks_commit
test_missing_lint_blocks_commit
test_a_failing_suite_no_longer_blocks_commit

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
