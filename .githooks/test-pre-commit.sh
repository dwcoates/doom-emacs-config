#!/usr/bin/env bash
# test-pre-commit.sh — hermetic tests for .githooks/pre-commit.
set -euo pipefail

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
  local exit_override="${2:-0}"
  RUN_LOG="$repo/unified-called"
  rm -f "$RUN_LOG"
  set +e
  RUN_OUT="$(
    HOOK_TEST_RUN_LOG="$RUN_LOG" \
      HOOK_TEST_RUN_EXIT="$exit_override" \
      git -C "$repo" -c core.hooksPath=.git/hooks commit -m "test commit" 2>&1
  )"
  RUN_RC=$?
  set -e
}

test_cherry_pick_skips_unified_gate() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "src/dummy.ts"
  touch "$repo/.git/CHERRY_PICK_HEAD"
  run_commit "$repo"

  if [ ! -f "$RUN_LOG" ] && printf '%s\n' "$RUN_OUT" | grep -q "Cherry-pick detected"; then
    pass "cherry-pick replay skips the unified gate"
  else
    fail "cherry-pick replay skips the unified gate" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_direct_master_commit_runs_unified_gate() {
  local repo
  repo="$(mkrepo)"
  git -C "$repo" symbolic-ref HEAD refs/heads/master
  stage_module_file "$repo" "internal/dummy.go"
  run_commit "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ -f "$RUN_LOG" ]; then
    pass "direct master commit runs the unified gate"
  else
    fail "direct master commit runs the unified gate" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_elisp_change_runs_unified_gate() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "dummy.el"
  run_commit "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ -f "$RUN_LOG" ]; then
    pass "Elisp change runs the unified gate"
  else
    fail "Elisp change runs the unified gate" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_typescript_change_runs_unified_gate() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "webapp/src/dummy.ts"
  run_commit "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ -f "$RUN_LOG" ]; then
    pass "TypeScript change runs the unified gate"
  else
    fail "TypeScript change runs the unified gate" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_go_change_runs_unified_gate() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "daemon/internal/dummy.go"
  run_commit "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ -f "$RUN_LOG" ]; then
    pass "Go change runs the unified gate"
  else
    fail "Go change runs the unified gate" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_proto_change_runs_unified_gate() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "proto/dummy.proto"
  run_commit "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ -f "$RUN_LOG" ]; then
    pass "proto change runs the unified gate"
  else
    fail "proto change runs the unified gate" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_package_manifest_runs_unified_gate() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "webapp/package.json"
  run_commit "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ -f "$RUN_LOG" ]; then
    pass "TypeScript package manifest runs the unified gate"
  else
    fail "TypeScript package manifest runs the unified gate" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_hook_change_runs_unified_gate() {
  local repo
  repo="$(mkrepo)"
  mkdir -p "$repo/.githooks"
  printf '# changed hook\n' >"$repo/.githooks/pre-commit"
  git -C "$repo" add "$repo/.githooks/pre-commit"
  run_commit "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ -f "$RUN_LOG" ]; then
    pass "hook change runs the unified gate"
  else
    fail "hook change runs the unified gate" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_unrelated_docs_skip_unified_gate() {
  local repo
  repo="$(mkrepo)"
  printf 'docs\n' >"$repo/README.md"
  git -C "$repo" add "$repo/README.md"
  run_commit "$repo"

  if [ "$RUN_RC" -eq 0 ] && [ ! -f "$RUN_LOG" ]; then
    pass "unrelated documentation skips the unified gate"
  else
    fail "unrelated documentation skips the unified gate" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_unified_failure_blocks_commit() {
  local repo
  repo="$(mkrepo)"
  stage_module_file "$repo" "webapp/src/failing.ts"
  run_commit "$repo" 7

  if [ "$RUN_RC" -ne 0 ] &&
    [ -f "$RUN_LOG" ] &&
    printf '%s\n' "$RUN_OUT" | grep -q "refusing commit"; then
    pass "unified failure blocks the commit"
  else
    fail "unified failure blocks the commit" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

test_missing_unified_runner_blocks_commit() {
  local repo
  repo="$(mkrepo)"
  rm "$repo/modules/app/agent-repl/bin/test-all.sh"
  stage_module_file "$repo" "daemon/missing-runner.go"
  run_commit "$repo"

  if [ "$RUN_RC" -ne 0 ] &&
    printf '%s\n' "$RUN_OUT" | grep -q "unified runner is missing"; then
    pass "missing unified runner blocks the commit"
  else
    fail "missing unified runner blocks the commit" "exit=$RUN_RC" "$RUN_OUT"
  fi
  rm -rf "$repo"
}

TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-precommit-test.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT

test_cherry_pick_skips_unified_gate
test_direct_master_commit_runs_unified_gate
test_elisp_change_runs_unified_gate
test_typescript_change_runs_unified_gate
test_go_change_runs_unified_gate
test_proto_change_runs_unified_gate
test_package_manifest_runs_unified_gate
test_hook_change_runs_unified_gate
test_unrelated_docs_skip_unified_gate
test_unified_failure_blocks_commit
test_missing_unified_runner_blocks_commit

printf 'Passed: %d  Failed: %d\n' "$PASS" "$FAIL"
[ "$FAIL" -eq 0 ]
