#!/usr/bin/env bash
# test-pre-commit.sh — hermetic tests for .githooks/pre-commit.
#
# The hook's ONLY gate is the static external-boundary lint.  The unified
# agent-repl test suite is deliberately NOT run here any more: that gate moved
# into the workspace-merge machinery (merge.Driver's per-commit pick loop plus
# merge.SuiteRunner), which tests each cherry-picked commit as it lands on the
# target.  Every fixture below therefore asserts BOTH halves — the lint ran,
# and the suite runner was never invoked.
set -euo pipefail

# This harness creates scratch repositories and is itself run by pre-commit.
# Clear the caller's live Git bindings before any fixture command can mutate
# the real staging index.
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_PREFIX

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
HOOK_SRC="$THIS_DIR/pre-commit"
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
  repo="$(mktemp -d)"
  git -C "$repo" init -q
  git -C "$repo" config user.email "test@example.com"
  git -C "$repo" config user.name "Test"
  git -C "$repo" symbolic-ref HEAD refs/heads/work

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

  cp "$HOOK_SRC" "$repo/.git/hooks/pre-commit"
  chmod +x "$repo/.git/hooks/pre-commit"
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

run_commit() {
  local repo="$1"
  local lint_exit="${2:-0}"
  RUN_LOG="$repo/unified-called"
  LINT_LOG="$repo/lint-called"
  rm -f "$RUN_LOG" "$LINT_LOG"
  set +e
  RUN_OUT="$(
    HOOK_TEST_RUN_LOG="$RUN_LOG" \
      HOOK_TEST_LINT_LOG="$LINT_LOG" \
      HOOK_TEST_LINT_EXIT="$lint_exit" \
      git -C "$repo" -c core.hooksPath=.git/hooks commit -m "test commit" 2>&1
  )"
  RUN_RC=$?
  set -e
}

# assert_gated NAME — the commit succeeded, the lint ran, the suite did not.
assert_gated() {
  if [ "$RUN_RC" -eq 0 ] && [ -f "$LINT_LOG" ] && [ ! -f "$RUN_LOG" ]; then
    pass "$1"
  else
    fail "$1" "exit=$RUN_RC lint_ran=$([ -f "$LINT_LOG" ] && echo yes || echo no) suite_ran=$([ -f "$RUN_LOG" ] && echo yes || echo no)" "$RUN_OUT"
  fi
}

test_cherry_pick_skips_the_gate() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "src/dummy.ts"
  touch "$repo/.git/CHERRY_PICK_HEAD"
  run_commit "$repo"

  if [ ! -f "$LINT_LOG" ] && printf '%s\n' "$RUN_OUT" | grep -q "Cherry-pick detected"; then
    pass "cherry-pick replay skips the gate"
  else
    fail "cherry-pick replay skips the gate" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_direct_master_commit_runs_the_lint() {
  local repo
  repo="$(mkrepo)"
  git -C "$repo" symbolic-ref HEAD refs/heads/master
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
  foreign="$(mktemp -d)"
  git -C "$foreign" init -q
  git -C "$foreign" config user.email "test@example.com"
  git -C "$foreign" config user.name "Test"
  mkdir -p "$foreign/modules/app/agent-repl"
  printf 'foreign fixture\n' >"$foreign/modules/app/agent-repl/dummy.ts"
  git -C "$foreign" add modules/app/agent-repl/dummy.ts

  LINT_LOG="$foreign/lint-called"
  set +e
  RUN_OUT="$(
    HOOK_TEST_LINT_LOG="$LINT_LOG" \
      git -C "$foreign" -c core.hooksPath="$owner/.git/hooks" commit -m "foreign fixture" 2>&1
  )"
  RUN_RC=$?
  set -e

  if [ "$RUN_RC" -eq 0 ] && [ ! -f "$LINT_LOG" ]; then
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
    printf '%s\n' "$RUN_OUT" | grep -q "refusing commit"; then
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
    printf '%s\n' "$RUN_OUT" | grep -q "external-boundary lint is missing"; then
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
  RUN_LOG="$repo/unified-called"
  LINT_LOG="$repo/lint-called"
  rm -f "$RUN_LOG" "$LINT_LOG"
  set +e
  RUN_OUT="$(
    HOOK_TEST_RUN_LOG="$RUN_LOG" \
      HOOK_TEST_RUN_EXIT=7 \
      HOOK_TEST_LINT_LOG="$LINT_LOG" \
      git -C "$repo" -c core.hooksPath=.git/hooks commit -m "test commit" 2>&1
  )"
  RUN_RC=$?
  set -e

  if [ "$RUN_RC" -eq 0 ] && [ ! -f "$RUN_LOG" ]; then
    pass "a failing unified suite no longer blocks the commit"
  else
    fail "a failing unified suite no longer blocks the commit" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-precommit-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

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
