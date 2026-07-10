#!/usr/bin/env bash
# test-pre-commit.sh — tests for .githooks/pre-commit.
#
# Sets up isolated git repos and asserts on the hook's exit behavior.
# No host paths or real Emacs invocations are used.
#
# Run with:   bash .githooks/test-pre-commit.sh
set -euo pipefail

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
HOOK_SRC="$THIS_DIR/pre-commit"

PASS=0
FAIL=0
pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); shift; if [ $# -gt 0 ]; then printf '%s\n' "$@" | sed 's/^/        /'; fi; }

# Create a minimal git repo, copy the hook into it, and return its path.
# The hook is wrapped so that:
#   - sync.sh is absent (cache-sync section is skipped cleanly)
#   - emacs is stubbed via PATH to exit 0 (so ERT never really runs)
#   - check-external-boundaries.sh is absent (boundary lint skipped)
mkrepo() {
  local repo module="${1:-agent-repl}"; repo="$(mktemp -d)"
  git -C "$repo" init -q
  git -C "$repo" config user.email "test@example.com"
  git -C "$repo" config user.name "Test"

  # Minimal repo layout expected by the hook (module name parameterizable
  # so the legacy agent-repl fallback can be exercised).
  mkdir -p "$repo/modules/app/$module"
  touch "$repo/modules/app/$module/test-$module.el"

  # Install the hook under test.
  cp "$HOOK_SRC" "$repo/.git/hooks/pre-commit"
  chmod +x "$repo/.git/hooks/pre-commit"

  echo "$repo"
}

# Stage a dummy module .el file in REPO so the test gate is reached.
stage_el_file() {
  local repo="$1" module="${2:-agent-repl}"
  local file="$repo/modules/app/$module/dummy.el"
  echo "(provide 'dummy)" > "$file"
  git -C "$repo" add "$file"
}

# Build a PATH-stub directory that makes `emacs` a no-op (exit 0).
stub_emacs_dir() {
  local dir; dir="$(mktemp -d)"
  cat > "$dir/emacs" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
  chmod +x "$dir/emacs"
  echo "$dir"
}

# ---------------------------------------------------------------------------

test_cherry_pick_skips_ert() {
  local repo; repo="$(mkrepo)"
  stage_el_file "$repo"
  # Simulate a cherry-pick in progress.
  touch "$repo/.git/CHERRY_PICK_HEAD"

  local out exit_code=0
  out="$(git -C "$repo" -c core.hooksPath=.git/hooks commit -m "cherry" 2>&1)" || exit_code=$?

  # The hook exits 0 and prints the skip message; the commit may still
  # fail for unrelated reasons (empty tree), so we check hook output only.
  if echo "$out" | grep -q "Cherry-pick detected"; then
    pass "cherry-pick: hook prints skip message"
  else
    fail "cherry-pick: expected skip message in output" "$out"
  fi

  rm -rf "$repo"
}

test_cherry_pick_does_not_invoke_emacs() {
  local repo; repo="$(mkrepo)"
  stage_el_file "$repo"
  touch "$repo/.git/CHERRY_PICK_HEAD"

  # Use a stub emacs that leaves a sentinel file if called.
  local stub_dir; stub_dir="$(mktemp -d)"
  local sentinel="$stub_dir/emacs-was-called"
  cat > "$stub_dir/emacs" <<EOF
#!/usr/bin/env bash
touch "$sentinel"
exit 0
EOF
  chmod +x "$stub_dir/emacs"

  PATH="$stub_dir:$PATH" git -C "$repo" -c core.hooksPath=.git/hooks commit -m "cherry" >/dev/null 2>&1 || true

  if [ ! -f "$sentinel" ]; then
    pass "cherry-pick: emacs is never invoked"
  else
    fail "cherry-pick: emacs was invoked despite CHERRY_PICK_HEAD"
  fi

  rm -rf "$repo" "$stub_dir"
}

test_normal_commit_reaches_ert_when_el_staged() {
  local repo; repo="$(mkrepo)"
  stage_el_file "$repo"
  # No CHERRY_PICK_HEAD — normal commit path.

  local stub_dir; stub_dir="$(stub_emacs_dir)"
  local sentinel="$stub_dir/emacs-was-called"
  # Replace the stub emacs so it also records that it was called.
  cat > "$stub_dir/emacs" <<EOF
#!/usr/bin/env bash
touch "$sentinel"
exit 0
EOF
  chmod +x "$stub_dir/emacs"

  PATH="$stub_dir:$PATH" git -C "$repo" -c core.hooksPath=.git/hooks commit -m "normal" >/dev/null 2>&1 || true

  if [ -f "$sentinel" ]; then
    pass "normal commit: emacs (ERT) is invoked when .el files are staged"
  else
    fail "normal commit: emacs was NOT invoked for staged .el files"
  fi

  rm -rf "$repo" "$stub_dir"
}

test_normal_commit_no_el_skips_ert() {
  local repo; repo="$(mkrepo)"
  # Stage a non-.el file — hook should exit 0 without calling emacs.
  echo "hello" > "$repo/README.txt"
  git -C "$repo" add "$repo/README.txt"

  local stub_dir; stub_dir="$(mktemp -d)"
  local sentinel="$stub_dir/emacs-was-called"
  cat > "$stub_dir/emacs" <<EOF
#!/usr/bin/env bash
touch "$sentinel"
exit 0
EOF
  chmod +x "$stub_dir/emacs"

  PATH="$stub_dir:$PATH" git -C "$repo" -c core.hooksPath=.git/hooks commit -m "docs" >/dev/null 2>&1 || true

  if [ ! -f "$sentinel" ]; then
    pass "normal commit: emacs not invoked when no .el files staged"
  else
    fail "normal commit: emacs was invoked despite no .el files staged"
  fi

  rm -rf "$repo" "$stub_dir"
}

test_legacy_claude_repl_layout_still_gated() {
  # A sibling worktree that predates the agent-repl -> agent-repl rename
  # shares this hook via core.hooksPath: the hook must fall back to the
  # legacy module name and run ITS aggregator.
  local repo; repo="$(mkrepo agent-repl)"
  stage_el_file "$repo" agent-repl

  local stub_dir; stub_dir="$(mktemp -d)"
  local arglog="$stub_dir/emacs-args"
  cat > "$stub_dir/emacs" <<EOF
#!/usr/bin/env bash
echo "\$@" > "$arglog"
exit 0
EOF
  chmod +x "$stub_dir/emacs"

  PATH="$stub_dir:$PATH" git -C "$repo" -c core.hooksPath=.git/hooks commit -m "legacy" >/dev/null 2>&1 || true

  if [ -f "$arglog" ] && grep -q "modules/app/agent-repl/test-agent-repl.el" "$arglog"; then
    pass "legacy layout: hook gates on agent-repl aggregator"
  else
    fail "legacy layout: expected emacs run on agent-repl aggregator" "$(cat "$arglog" 2>/dev/null || echo 'emacs never invoked')"
  fi

  rm -rf "$repo" "$stub_dir"
}

test_missing_aggregator_refuses_commit() {
  # Gate reached (module .el staged) but NO module has an aggregator test
  # file: the hook must refuse the commit rather than skipping the suite.
  local repo; repo="$(mkrepo)"
  rm "$repo/modules/app/agent-repl/test-agent-repl.el"
  stage_el_file "$repo"

  local stub_dir; stub_dir="$(stub_emacs_dir)"
  local out exit_code=0
  out="$(PATH="$stub_dir:$PATH" git -C "$repo" -c core.hooksPath=.git/hooks commit -m "broken" 2>&1)" || exit_code=$?

  if [ "$exit_code" -ne 0 ] && echo "$out" | grep -q "refusing to commit"; then
    pass "missing aggregator: commit refused"
  else
    fail "missing aggregator: expected refusal" "exit: $exit_code" "$out"
  fi

  rm -rf "$repo" "$stub_dir"
}

# ---------------------------------------------------------------------------

echo "=== test-pre-commit.sh ==="
test_cherry_pick_skips_ert
test_cherry_pick_does_not_invoke_emacs
test_normal_commit_reaches_ert_when_el_staged
test_normal_commit_no_el_skips_ert
test_legacy_claude_repl_layout_still_gated
test_missing_aggregator_refuses_commit

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
