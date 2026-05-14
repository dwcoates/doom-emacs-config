#!/usr/bin/env bash
# test-install.sh — tests for .claude/install.sh.
#
# Each test builds a self-contained synthetic repo in a tmpdir
# (install.sh + manifest + cache + hook scripts), inits it as a git
# repo, and runs install.sh against a fake HOME.  The real host
# ~/.claude/ and the real repo's git config are never touched.
#
# Focuses on the cached-skill install/uninstall logic and the
# pre-commit hook install/uninstall logic — the hook-script + jq
# bits in install.sh are tested by exercise (we just check they
# don't error).
#
# Run with:   bash .claude/test-install.sh
set -euo pipefail

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$THIS_DIR/.." && pwd)"

PASS=0
FAIL=0
pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); shift; if [ $# -gt 0 ]; then printf '%s\n' "$@" | sed 's/^/        /'; fi; }

# Build a synthetic repo containing a fresh copy of install.sh, the
# pre-commit hook, and a minimal cache+manifest with one cached skill
# 'foo' whose impl points at $1 (default: a non-existent path so
# install.sh exercises the cache-fallback branch).
mkfake_repo() {
  local impl_path="${1:-/nonexistent/impl-$$}"
  local root; root="$(mktemp -d)"
  mkdir -p "$root/.claude" \
           "$root/.githooks" \
           "$root/modules/app/claude-repl/skills-cache/foo" \
           "$root/modules/app/claude-repl/skills" \
           "$root/modules/app/claude-repl/hooks"
  cp "$REPO_ROOT/.claude/install.sh" "$root/.claude/install.sh"
  cp "$REPO_ROOT/.githooks/pre-commit" "$root/.githooks/pre-commit"
  chmod +x "$root/.claude/install.sh" "$root/.githooks/pre-commit"
  # Cache content
  echo "cache-content" > "$root/modules/app/claude-repl/skills-cache/foo/SKILL.md"
  cat > "$root/modules/app/claude-repl/skills-cache/manifest.sh" <<EOF
CACHED_SKILLS=("foo|$impl_path")
EOF
  # Minimum sync.sh so any test path that runs it works
  cp "$REPO_ROOT/modules/app/claude-repl/skills-cache/sync.sh" \
     "$root/modules/app/claude-repl/skills-cache/sync.sh"
  chmod +x "$root/modules/app/claude-repl/skills-cache/sync.sh"
  # Dummy hook script so install.sh's hook-copy loop has something to do
  printf '#!/usr/bin/env bash\nexit 0\n' > "$root/modules/app/claude-repl/hooks/dummy.sh"
  chmod +x "$root/modules/app/claude-repl/hooks/dummy.sh"
  # Init git so install.sh can resolve --show-toplevel + --git-path
  (cd "$root" && git init -q && git config user.email t@t && git config user.name t)
  echo "$root"
}

mkfake_home() {
  local home; home="$(mktemp -d)"
  mkdir -p "$home/.claude/skills"
  echo "$home"
}

# Run install.sh in the isolated repo with fake HOME.  stdout+stderr
# captured so tests can grep over it without polluting test output.
run_install() {
  local repo="$1" home="$2" action="${3:-install}"
  HOME="$home" bash "$repo/.claude/install.sh" "$action" >"$repo/.install.log" 2>&1 || true
}

cleanup() { rm -rf "$1" "$2"; }

# --- install: fresh fallback to cache symlink ---
test_install_fresh_falls_back_to_cache() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  local link="$home/.claude/skills/foo"
  local expected="$repo/modules/app/claude-repl/skills-cache/foo"
  local actual; actual="$(readlink "$link" 2>/dev/null || echo MISSING)"
  if [ "$actual" = "$expected" ]; then
    pass "fresh install symlinks cache fallback"
  else
    fail "fresh install cache fallback" "expected: $expected" "actual:   $actual"
  fi
  cleanup "$repo" "$home"
}

# --- install: existing live-impl symlink is preserved ---
test_install_preserves_live_impl_symlink() {
  local live_impl; live_impl="$(mktemp -d)"
  echo "live" > "$live_impl/SKILL.md"
  local repo home; repo="$(mkfake_repo "$live_impl")"; home="$(mkfake_home)"
  ln -s "$live_impl" "$home/.claude/skills/foo"
  run_install "$repo" "$home"
  local actual; actual="$(readlink "$home/.claude/skills/foo")"
  if [ "$actual" = "$live_impl" ] && grep -q "live-impl symlink already in place" "$repo/.install.log"; then
    pass "existing live-impl symlink is preserved"
  else
    fail "live-impl preservation" "actual: $actual" "$(cat "$repo/.install.log")"
  fi
  rm -rf "$live_impl"
  cleanup "$repo" "$home"
}

# --- install: existing cache symlink is preserved ---
test_install_preserves_cache_symlink() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  local cache="$repo/modules/app/claude-repl/skills-cache/foo"
  ln -s "$cache" "$home/.claude/skills/foo"
  run_install "$repo" "$home"
  if grep -q "cache symlink already in place" "$repo/.install.log"; then
    pass "existing cache symlink is preserved"
  else
    fail "cache symlink preservation" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: foreign symlink is left alone ---
test_install_skips_foreign_symlink() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  ln -s /tmp/something-foreign "$home/.claude/skills/foo"
  run_install "$repo" "$home"
  local actual; actual="$(readlink "$home/.claude/skills/foo")"
  if [ "$actual" = "/tmp/something-foreign" ] \
     && grep -q "foreign symlink" "$repo/.install.log"; then
    pass "foreign symlink is left alone with a warning"
  else
    fail "foreign symlink" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: pre-commit hook is installed when absent ---
test_install_installs_pre_commit_hook() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  local dest="$repo/.git/hooks/pre-commit"
  if [ -x "$dest" ] && grep -q "claude-repl-skills-cache-sync" "$dest"; then
    pass "pre-commit hook installed into repo .git/hooks"
  else
    fail "pre-commit install" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: managed pre-commit hook is refreshed on rerun ---
test_install_refreshes_managed_hook() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"  # first install
  # Tamper: append junk so we can detect refresh
  echo "# tampered" >> "$repo/.git/hooks/pre-commit"
  run_install "$repo" "$home"  # rerun
  if ! grep -q "^# tampered" "$repo/.git/hooks/pre-commit" \
     && grep -q "Refreshed managed pre-commit hook" "$repo/.install.log"; then
    pass "managed pre-commit hook refreshed on rerun"
  else
    fail "managed hook refresh" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: foreign pre-commit hook is preserved ---
test_install_preserves_foreign_pre_commit() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  mkdir -p "$repo/.git/hooks"
  printf '#!/bin/sh\necho foreign\n' > "$repo/.git/hooks/pre-commit"
  chmod +x "$repo/.git/hooks/pre-commit"
  run_install "$repo" "$home"
  if grep -q "echo foreign" "$repo/.git/hooks/pre-commit" \
     && grep -q "foreign pre-commit hook" "$repo/.install.log"; then
    pass "foreign pre-commit hook is preserved"
  else
    fail "foreign pre-commit preservation" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- uninstall: removes cache symlink ---
test_uninstall_removes_cache_symlink() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  [ -L "$home/.claude/skills/foo" ] || { fail "uninstall precondition (cache link missing)"; cleanup "$repo" "$home"; return; }
  run_install "$repo" "$home" uninstall
  if [ ! -e "$home/.claude/skills/foo" ] && [ ! -L "$home/.claude/skills/foo" ]; then
    pass "uninstall removes cache symlink"
  else
    fail "uninstall cache symlink"
  fi
  cleanup "$repo" "$home"
}

# --- uninstall: removes only managed pre-commit hook ---
test_uninstall_only_removes_managed_hook() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  run_install "$repo" "$home" uninstall
  if [ ! -e "$repo/.git/hooks/pre-commit" ]; then
    pass "uninstall removes managed pre-commit hook"
  else
    fail "uninstall managed hook"
  fi
  # Now seed a foreign hook and verify uninstall leaves it alone.
  cleanup "$repo" "$home"
  repo="$(mkfake_repo)"; home="$(mkfake_home)"
  mkdir -p "$repo/.git/hooks"
  printf '#!/bin/sh\necho keep\n' > "$repo/.git/hooks/pre-commit"
  chmod +x "$repo/.git/hooks/pre-commit"
  run_install "$repo" "$home" uninstall
  if [ -f "$repo/.git/hooks/pre-commit" ] && grep -q "echo keep" "$repo/.git/hooks/pre-commit"; then
    pass "uninstall leaves foreign pre-commit hook alone"
  else
    fail "uninstall preserves foreign hook"
  fi
  cleanup "$repo" "$home"
}

echo "=== test-install.sh ==="
test_install_fresh_falls_back_to_cache
test_install_preserves_live_impl_symlink
test_install_preserves_cache_symlink
test_install_skips_foreign_symlink
test_install_installs_pre_commit_hook
test_install_refreshes_managed_hook
test_install_preserves_foreign_pre_commit
test_uninstall_removes_cache_symlink
test_uninstall_only_removes_managed_hook

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
