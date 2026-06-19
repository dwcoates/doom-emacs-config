#!/usr/bin/env bash
# test-install.sh — tests for .claude/install.sh.
#
# Each test builds a self-contained synthetic repo in a tmpdir
# (install.sh + manifest + pre-commit hook + local-skill dirs), inits it
# as a git repo, and runs install.sh against a fake HOME.  The real host
# ~/.claude/ and the real repo's git config are never touched.
#
# Focuses on the skill install/uninstall logic (always symlink straight
# to the canonical impl, NO cache fallback, FAIL HARD when an impl is
# absent) and the pre-commit hook install/uninstall logic.
#
# Run with:   bash .claude/test-install.sh
set -euo pipefail

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$THIS_DIR/.." && pwd)"

PASS=0
FAIL=0
pass() { echo "  PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL: $1"; FAIL=$((FAIL + 1)); shift; if [ $# -gt 0 ]; then printf '%s\n' "$@" | sed 's/^/        /'; fi; }

# Local skills install.sh expects to find under modules/app/claude-repl/skills/.
LOCAL_SKILL_NAMES=( debug-logs profile runtime-eval-code workspace-close workspace-profile )

# Build a synthetic repo containing a fresh copy of install.sh, the
# pre-commit hook, a manifest with one cached skill 'foo' whose impl is
# $1, and the local-skill source dirs.  When $1 is omitted, an existing
# impl dir is created under the repo so install succeeds; pass a
# nonexistent path to exercise the FAIL-HARD branch.
mkfake_repo() {
  local root; root="$(mktemp -d)"
  local impl_path="${1:-$root/impl/foo}"
  mkdir -p "$root/.claude" \
           "$root/.githooks" \
           "$root/modules/app/claude-repl/skills-cache" \
           "$root/modules/app/claude-repl/skills" \
           "$root/modules/app/claude-repl/hooks"
  cp "$REPO_ROOT/.claude/install.sh" "$root/.claude/install.sh"
  cp "$REPO_ROOT/.githooks/pre-commit" "$root/.githooks/pre-commit"
  chmod +x "$root/.claude/install.sh" "$root/.githooks/pre-commit"
  # Create the default impl only when the caller did not supply one.
  if [ -z "${1:-}" ]; then
    mkdir -p "$root/impl/foo"
    echo "impl-content" > "$root/impl/foo/SKILL.md"
  fi
  cat > "$root/modules/app/claude-repl/skills-cache/manifest.sh" <<EOF
CACHED_SKILLS=("foo|$impl_path")
EOF
  # Local-skill source dirs (install.sh links straight to these).
  for s in "${LOCAL_SKILL_NAMES[@]}"; do
    mkdir -p "$root/modules/app/claude-repl/skills/$s"
    printf 'name: %s\n' "$s" > "$root/modules/app/claude-repl/skills/$s/SKILL.md"
  done
  # Dummy hook script so install.sh's hook-copy loop has something to do.
  printf '#!/usr/bin/env bash\nexit 0\n' > "$root/modules/app/claude-repl/hooks/dummy.sh"
  chmod +x "$root/modules/app/claude-repl/hooks/dummy.sh"
  # Init git so install.sh can resolve --show-toplevel + --git-path.
  (cd "$root" && git init -q && git config user.email t@t && git config user.name t)
  echo "$root"
}

mkfake_home() {
  local home; home="$(mktemp -d)"
  mkdir -p "$home/.claude/skills"
  echo "$home"
}

# Run install.sh in the isolated repo with fake HOME, capturing the exit
# code into LAST_RC and stdout+stderr into $repo/.install.log.
LAST_RC=0
run_install() {
  local repo="$1" home="$2" action="${3:-install}"
  set +e
  INSTALL_SH_SKIP_SANDBOX_DETECT=1 HOME="$home" bash "$repo/.claude/install.sh" "$action" >"$repo/.install.log" 2>&1
  LAST_RC=$?
  set -e
}

cleanup() { rm -rf "$1" "$2"; }

# --- install: fresh install symlinks straight to the canonical impl ---
test_install_fresh_symlinks_to_impl() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  local actual; actual="$(readlink "$home/.claude/skills/foo" 2>/dev/null || echo MISSING)"
  if [ "$LAST_RC" -eq 0 ] && [ "$actual" = "$repo/impl/foo" ]; then
    pass "fresh install symlinks straight to impl"
  else
    fail "fresh install impl symlink" "rc: $LAST_RC" "expected: $repo/impl/foo" "actual:   $actual"
  fi
  cleanup "$repo" "$home"
}

# --- install: a pre-existing (stale) symlink is repaired to the impl ---
test_install_relinks_existing_symlink_to_impl() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  ln -s /some/stale/sandbox/path "$home/.claude/skills/foo"
  run_install "$repo" "$home"
  local actual; actual="$(readlink "$home/.claude/skills/foo")"
  if [ "$LAST_RC" -eq 0 ] && [ "$actual" = "$repo/impl/foo" ]; then
    pass "stale symlink is repaired to the impl"
  else
    fail "stale symlink repair" "rc: $LAST_RC" "actual: $actual" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: FAIL HARD when the canonical impl is missing (no fallback) ---
test_install_fails_hard_when_impl_missing() {
  local repo home; repo="$(mkfake_repo "/nonexistent/impl-$$")"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  if [ "$LAST_RC" -ne 0 ] \
     && grep -q "ERROR: canonical impl for 'foo' not found" "$repo/.install.log" \
     && [ ! -L "$home/.claude/skills/foo" ]; then
    pass "missing impl fails hard with no symlink created"
  else
    fail "missing-impl fail-hard" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: a real (non-symlink) file at the dest is NOT trampled ---
test_install_preserves_non_symlink_file() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  echo "user file" > "$home/.claude/skills/foo"
  run_install "$repo" "$home"
  if [ -f "$home/.claude/skills/foo" ] && [ ! -L "$home/.claude/skills/foo" ] \
     && grep -q "non-symlink file" "$repo/.install.log"; then
    pass "non-symlink file is left untouched with a warning"
  else
    fail "non-symlink preservation" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: pre-commit hook is installed when absent ---
test_install_installs_pre_commit_hook() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  local dest="$repo/.git/hooks/pre-commit"
  if [ -x "$dest" ] && grep -q "claude-repl-precommit" "$dest"; then
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
  # Tamper: append junk so we can detect refresh.
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

# --- uninstall: removes the impl symlink ---
test_uninstall_removes_impl_symlink() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  [ -L "$home/.claude/skills/foo" ] || { fail "uninstall precondition (impl link missing)" "$(cat "$repo/.install.log")"; cleanup "$repo" "$home"; return; }
  run_install "$repo" "$home" uninstall
  if [ ! -e "$home/.claude/skills/foo" ] && [ ! -L "$home/.claude/skills/foo" ]; then
    pass "uninstall removes impl symlink"
  else
    fail "uninstall impl symlink"
  fi
  cleanup "$repo" "$home"
}

# --- uninstall: removes only the managed pre-commit hook ---
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
test_install_fresh_symlinks_to_impl
test_install_relinks_existing_symlink_to_impl
test_install_fails_hard_when_impl_missing
test_install_preserves_non_symlink_file
test_install_installs_pre_commit_hook
test_install_refreshes_managed_hook
test_install_preserves_foreign_pre_commit
test_uninstall_removes_impl_symlink
test_uninstall_only_removes_managed_hook

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
