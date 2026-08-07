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

# Local skills install.sh expects to find under modules/app/agent-repl/skills/.
# debug-emacs-agent-repl is deliberately absent: it is PROJECT-scoped
# (checked-in <repo>/.claude/skills symlink), not a user-level installed skill.
LOCAL_SKILL_NAMES=( profile runtime-eval-code workspace-close emit-workspace-commands.sh )

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
           "$root/modules/app/agent-repl/skills-cache" \
           "$root/modules/app/agent-repl/skills"
  cp "$REPO_ROOT/.claude/install.sh" "$root/.claude/install.sh"
  cp "$REPO_ROOT/.githooks/pre-commit" "$root/.githooks/pre-commit"
  chmod +x "$root/.claude/install.sh" "$root/.githooks/pre-commit"
  # Create the default impl only when the caller did not supply one.
  if [ -z "${1:-}" ]; then
    mkdir -p "$root/impl/foo"
    echo "impl-content" > "$root/impl/foo/SKILL.md"
  fi
  cat > "$root/modules/app/agent-repl/skills-cache/manifest.sh" <<EOF
CACHED_SKILLS=("foo|$impl_path")
EOF
  # Local-skill source dirs (install.sh links straight to these).
  for s in "${LOCAL_SKILL_NAMES[@]}"; do
    mkdir -p "$root/modules/app/agent-repl/skills/$s"
    printf 'name: %s\n' "$s" > "$root/modules/app/agent-repl/skills/$s/SKILL.md"
  done
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
  HOME="$home" bash "$repo/.claude/install.sh" "$action" >"$repo/.install.log" 2>&1
  LAST_RC=$?
  set -e
}

cleanup() { rm -rf "$1" "$2"; }

# Seed a fake config dir under $1 with a settings.json carrying BOTH this
# module's pre-cutover hook entries and foreign ones, plus unrelated
# top-level keys, and the orphaned hook scripts on disk.  $2 is the config
# dir basename (".claude" or an alternate-account ".claude-<name>").
mkfake_settings() {
  local home="$1" cfg="${2:-.claude}"
  mkdir -p "$home/$cfg/hooks"
  printf '#!/bin/sh\n' > "$home/$cfg/hooks/stop-notify.sh"
  printf '#!/bin/sh\n' > "$home/$cfg/hooks/session-start-notify.sh"
  cat > "$home/$cfg/settings.json" <<'JSON'
{
  "$schema": "https://json.schemastore.org/claude-code-settings.json",
  "permissions": {
    "allow": [
      "Bash(git status:*)"
    ]
  },
  "hooks": {
    "SessionStart": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "~/.claude/hooks/session-start-notify.sh"
          }
        ]
      }
    ],
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "~/.claude/hooks/stop-notify.sh"
          },
          {
            "type": "command",
            "command": "~/.gns/sockets/hook.py"
          }
        ]
      }
    ],
    "PreToolUse": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "~/.gns/sockets/hook.py"
          }
        ]
      }
    ]
  },
  "model": "opus"
}
JSON
}

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

# --- install: the default action reports success in its EXIT CODE ---
# Regression: the services step used to be guarded by a short-circuit
# `[ "$WITH_SERVICES" -eq 1 ] && install_agent_shim_services` that was the
# last command in its case arm, so with the opt-in flag OFF a fully
# successful install exited 1.
test_install_default_action_exits_zero() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  if [ "$LAST_RC" -eq 0 ] && grep -q "^\[install\] Done\." "$repo/.install.log"; then
    pass "default install (no services flag) exits zero"
  else
    fail "default install exit code" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- uninstall: the default action reports success in its EXIT CODE ---
test_uninstall_default_action_exits_zero() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  run_install "$repo" "$home" uninstall
  if [ "$LAST_RC" -eq 0 ] && grep -q "^\[uninstall\] Done\." "$repo/.install.log"; then
    pass "default uninstall (no services flag) exits zero"
  else
    fail "default uninstall exit code" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: a pre-existing (stale) symlink is repaired to the impl ---
test_install_relinks_existing_symlink_to_impl() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  ln -s /some/stale/path "$home/.claude/skills/foo"
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

# --- install: FORBID a skill impl that lives inside a non-main worktree ---
test_install_rejects_nonmain_worktree_impl() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  # A linked worktree requires at least one commit to branch from.
  (cd "$repo" && git add -A && git commit -qm init >/dev/null 2>&1)
  git -C "$repo" worktree add -q --detach "$repo/linked-wt" >/dev/null 2>&1
  # Point the manifest's skill impl INSIDE the linked (non-main) worktree.
  mkdir -p "$repo/linked-wt/impl/bar"
  echo x > "$repo/linked-wt/impl/bar/SKILL.md"
  cat > "$repo/modules/app/agent-repl/skills-cache/manifest.sh" <<EOF
CACHED_SKILLS=("bar|$repo/linked-wt/impl/bar")
EOF
  run_install "$repo" "$home"
  if [ "$LAST_RC" -ne 0 ] \
     && grep -q "non-main worktree" "$repo/.install.log" \
     && [ ! -L "$home/.claude/skills/bar" ]; then
    pass "rejects a skill impl inside a non-main worktree"
  else
    fail "non-main worktree rejection" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: local skills link to the MAIN worktree even when install.sh
#     is invoked from a linked worktree (symlinks must never dangle on prune) ---
test_local_skills_link_to_main_worktree() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  (cd "$repo" && git add -A && git commit -qm init >/dev/null 2>&1)
  git -C "$repo" worktree add -q --detach "$repo/linked-wt" >/dev/null 2>&1
  # Invoke install.sh from the LINKED worktree's own checkout.
  set +e
  HOME="$home" \
    bash "$repo/linked-wt/.claude/install.sh" install >"$repo/.install.log" 2>&1
  LAST_RC=$?
  set -e
  local actual; actual="$(readlink "$home/.claude/skills/profile" 2>/dev/null || echo MISSING)"
  # install.sh links to the PHYSICAL main-worktree path (matching git's
  # canonical `worktree list` output), so canonicalize the tmpdir-based
  # expectation too (mktemp yields /var/... which symlinks to /private/var/...
  # on macOS).
  local expected; expected="$(cd "$repo" && pwd -P)/modules/app/agent-repl/skills/profile"
  if [ "$LAST_RC" -eq 0 ] && [ "$actual" = "$expected" ]; then
    pass "local skills link to main worktree when run from a linked worktree"
  else
    fail "local-skill main-worktree linkage" "rc: $LAST_RC" "expected: $expected" "actual:   $actual" "$(cat "$repo/.install.log")"
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
  if [ -x "$dest" ] && grep -q "agent-repl-precommit" "$dest"; then
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

# --- install: a hook carrying only the LEGACY marker is still ours ---
# Regression: the claude-repl -> agent-repl rename rewrote the legacy
# marker constant to the current spelling, so a pre-rename installed hook
# was misreported as foreign and never refreshed — freezing the ERT gate
# at a copy that greps a module path which no longer exists.
test_install_refreshes_legacy_marked_hook() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  mkdir -p "$repo/.git/hooks"
  # A pre-rename copy: legacy marker only, plus a body we can detect.
  printf '#!/bin/sh\n# CLAUDE_REPL_MANAGED_HOOK: claude-repl-precommit\necho stale\n' \
    > "$repo/.git/hooks/pre-commit"
  chmod +x "$repo/.git/hooks/pre-commit"
  run_install "$repo" "$home"
  if ! grep -q "echo stale" "$repo/.git/hooks/pre-commit" \
     && grep -q "AGENT_REPL_MANAGED_HOOK" "$repo/.git/hooks/pre-commit" \
     && grep -q "Refreshed managed pre-commit hook" "$repo/.install.log"; then
    pass "legacy-marked pre-commit hook is refreshed, not called foreign"
  else
    fail "legacy-marked hook refresh" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- uninstall: a hook carrying only the LEGACY marker is removed ---
test_uninstall_removes_legacy_marked_hook() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  mkdir -p "$repo/.git/hooks"
  printf '#!/bin/sh\n# CLAUDE_REPL_MANAGED_HOOK: claude-repl-precommit\necho stale\n' \
    > "$repo/.git/hooks/pre-commit"
  chmod +x "$repo/.git/hooks/pre-commit"
  run_install "$repo" "$home" uninstall
  if [ ! -e "$repo/.git/hooks/pre-commit" ]; then
    pass "uninstall removes a legacy-marked pre-commit hook"
  else
    fail "legacy-marked hook uninstall" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: core.hooksPath == .githooks is a no-op, not a `cp` failure ---
# Regression: this repo sets core.hooksPath to the MANAGED SOURCE dir, so
# src_hook and dest_hook are the same file.  `cp` refuses ("are identical"),
# and under `set -e` that aborted the whole install with exit 1 — the
# auto-install warning on every Emacs boot.
test_install_hookspath_is_source_dir_exits_zero() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  git -C "$repo" config core.hooksPath "$repo/.githooks"
  run_install "$repo" "$home"
  if [ "$LAST_RC" -eq 0 ] \
     && grep -q "already live via core.hooksPath" "$repo/.install.log" \
     && grep -q "agent-repl-precommit" "$repo/.githooks/pre-commit"; then
    pass "core.hooksPath pointing at .githooks installs as a no-op, exit 0"
  else
    fail "core.hooksPath no-op install" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- uninstall: never deletes the CHECKED-IN hook under core.hooksPath ---
test_uninstall_keeps_checked_in_hook_under_hookspath() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  git -C "$repo" config core.hooksPath "$repo/.githooks"
  run_install "$repo" "$home" uninstall
  if [ "$LAST_RC" -eq 0 ] && [ -f "$repo/.githooks/pre-commit" ]; then
    pass "uninstall leaves the checked-in .githooks/pre-commit in place"
  else
    fail "uninstall checked-in hook preservation" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
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

# --- purge-legacy-hooks: this module's own entries are removed ---
test_purge_removes_our_hook_entries() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"; mkfake_settings "$home"
  run_install "$repo" "$home" purge-legacy-hooks
  local left; left="$(jq -r '[.hooks[]?[]?.hooks[]?.command] | map(select(test("notify.sh$"))) | length' "$home/.claude/settings.json")"
  if [ "$LAST_RC" -eq 0 ] && [ "$left" = "0" ]; then
    pass "purge removes this module's settings.json hook entries"
  else
    fail "purge our entries" "rc: $LAST_RC" "notify.sh entries left: $left" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- purge-legacy-hooks: FOREIGN entries on the same events survive ---
test_purge_preserves_foreign_hook_entries() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"; mkfake_settings "$home"
  run_install "$repo" "$home" purge-legacy-hooks
  local gns; gns="$(jq -r '[.hooks[]?[]?.hooks[]?.command] | map(select(test("hook.py$"))) | length' "$home/.claude/settings.json")"
  if [ "$gns" = "2" ]; then
    pass "purge leaves foreign hook entries alone"
  else
    fail "purge foreign preservation" "expected 2 foreign entries, found: $gns" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- purge-legacy-hooks: an event left empty is dropped, not left as [] ---
test_purge_prunes_emptied_event() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"; mkfake_settings "$home"
  run_install "$repo" "$home" purge-legacy-hooks
  if [ "$(jq -r '.hooks | has("SessionStart")' "$home/.claude/settings.json")" = "false" ] \
     && [ "$(jq -r '.hooks | has("Stop")' "$home/.claude/settings.json")" = "true" ]; then
    pass "purge drops an event emptied by the removal"
  else
    fail "purge empty-event pruning" "$(jq -c '.hooks | keys' "$home/.claude/settings.json")"
  fi
  cleanup "$repo" "$home"
}

# --- purge-legacy-hooks: unrelated settings keys are untouched ---
test_purge_preserves_unrelated_keys() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"; mkfake_settings "$home"
  run_install "$repo" "$home" purge-legacy-hooks
  local model schema allow
  model="$(jq -r '.model' "$home/.claude/settings.json")"
  schema="$(jq -r '."$schema"' "$home/.claude/settings.json")"
  allow="$(jq -r '.permissions.allow[0]' "$home/.claude/settings.json")"
  if [ "$model" = "opus" ] && [ "$schema" != "null" ] && [ "$allow" = "Bash(git status:*)" ]; then
    pass "purge leaves unrelated settings keys untouched"
  else
    fail "purge unrelated keys" "model=$model schema=$schema allow=$allow"
  fi
  cleanup "$repo" "$home"
}

# --- purge-legacy-hooks: a backup is written before any edit ---
test_purge_writes_backup() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"; mkfake_settings "$home"
  run_install "$repo" "$home" purge-legacy-hooks
  local backups; backups="$(find "$home/.claude" -maxdepth 1 -name 'settings.json.pre-purge-*' | wc -l | tr -d ' ')"
  if [ "$backups" = "1" ] && grep -q "stop-notify.sh" "$home"/.claude/settings.json.pre-purge-*; then
    pass "purge backs the settings file up before editing"
  else
    fail "purge backup" "backups found: $backups"
  fi
  cleanup "$repo" "$home"
}

# --- purge-legacy-hooks: the orphaned scripts are deleted too ---
test_purge_removes_orphaned_scripts() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"; mkfake_settings "$home"
  run_install "$repo" "$home" purge-legacy-hooks
  if [ ! -e "$home/.claude/hooks/stop-notify.sh" ] \
     && [ ! -e "$home/.claude/hooks/session-start-notify.sh" ]; then
    pass "purge deletes the orphaned hook scripts"
  else
    fail "purge orphaned scripts" "$(ls "$home/.claude/hooks")"
  fi
  cleanup "$repo" "$home"
}

# --- purge-legacy-hooks: alternate-account config dirs are purged too ---
test_purge_covers_alternate_config_dirs() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  mkfake_settings "$home"
  mkfake_settings "$home" ".claude-work"
  run_install "$repo" "$home" purge-legacy-hooks
  local left; left="$(jq -r '[.hooks[]?[]?.hooks[]?.command] | map(select(test("notify.sh$"))) | length' "$home/.claude-work/settings.json")"
  if [ "$left" = "0" ]; then
    pass "purge covers alternate-account config dirs"
  else
    fail "purge alt config dir" "notify.sh entries left: $left" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- purge-legacy-hooks: idempotent — a second run rewrites nothing ---
test_purge_is_idempotent() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"; mkfake_settings "$home"
  run_install "$repo" "$home" purge-legacy-hooks
  local after_first; after_first="$(cat "$home/.claude/settings.json")"
  run_install "$repo" "$home" purge-legacy-hooks
  local backups; backups="$(find "$home/.claude" -maxdepth 1 -name 'settings.json.pre-purge-*' | wc -l | tr -d ' ')"
  if [ "$LAST_RC" -eq 0 ] \
     && [ "$after_first" = "$(cat "$home/.claude/settings.json")" ] \
     && [ "$backups" = "1" ] \
     && grep -q "no legacy entries" "$repo/.install.log"; then
    pass "purge is idempotent (second run rewrites nothing)"
  else
    fail "purge idempotence" "rc: $LAST_RC" "backups: $backups" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- purge-legacy-hooks: malformed settings.json fails hard, unedited ---
test_purge_refuses_malformed_settings() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  mkdir -p "$home/.claude"
  printf '{not json' > "$home/.claude/settings.json"
  run_install "$repo" "$home" purge-legacy-hooks
  if [ "$LAST_RC" -ne 0 ] \
     && [ "$(cat "$home/.claude/settings.json")" = "{not json" ] \
     && grep -q "not valid JSON" "$repo/.install.log"; then
    pass "purge refuses to edit malformed settings.json"
  else
    fail "purge malformed settings" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- uninstall: the legacy hook purge runs as part of uninstall ---
test_uninstall_purges_legacy_hooks() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"; mkfake_settings "$home"
  run_install "$repo" "$home" uninstall
  local left; left="$(jq -r '[.hooks[]?[]?.hooks[]?.command] | map(select(test("notify.sh$"))) | length' "$home/.claude/settings.json")"
  if [ "$LAST_RC" -eq 0 ] && [ "$left" = "0" ]; then
    pass "uninstall purges the legacy settings.json hook entries"
  else
    fail "uninstall legacy purge" "rc: $LAST_RC" "notify.sh entries left: $left" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

echo "=== test-install.sh ==="
test_install_fresh_symlinks_to_impl
test_install_default_action_exits_zero
test_uninstall_default_action_exits_zero
test_install_relinks_existing_symlink_to_impl
test_install_fails_hard_when_impl_missing
test_install_rejects_nonmain_worktree_impl
test_local_skills_link_to_main_worktree
test_install_preserves_non_symlink_file
test_install_installs_pre_commit_hook
test_install_refreshes_managed_hook
test_install_refreshes_legacy_marked_hook
test_uninstall_removes_legacy_marked_hook
test_install_hookspath_is_source_dir_exits_zero
test_uninstall_keeps_checked_in_hook_under_hookspath
test_install_preserves_foreign_pre_commit
test_uninstall_removes_impl_symlink
test_uninstall_only_removes_managed_hook
test_purge_removes_our_hook_entries
test_purge_preserves_foreign_hook_entries
test_purge_prunes_emptied_event
test_purge_preserves_unrelated_keys
test_purge_writes_backup
test_purge_removes_orphaned_scripts
test_purge_covers_alternate_config_dirs
test_purge_is_idempotent
test_purge_refuses_malformed_settings
test_uninstall_purges_legacy_hooks

echo
echo "Passed: $PASS  Failed: $FAIL"
exit $((FAIL > 0))
