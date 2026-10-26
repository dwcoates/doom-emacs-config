#!/usr/bin/env bash
# test-install.sh — tests for .claude/install.sh.
#
# Each test builds a self-contained synthetic repo in a tmpdir
# (install.sh + manifest + pre-commit hook + local-skill dirs), inits it
# as a git repo, and runs install.sh against a fake HOME.  The real host
# ~/.claude/ and the real repo's git config are never touched.
#
# NO REAL GIT RUNS HERE (owner rule).  The synthetic repo is bin/fake-git.sh's
# model (its repository directory is .fakegit, so the hooks dir install.sh
# resolves is .fakegit/hooks), installed as the only `git` on PATH for the
# harness and install.sh; the harness refuses to start if any other `git`
# would answer.
#
# Focuses on the skill install/uninstall logic (always symlink straight
# to the canonical impl, NO cache fallback, FAIL HARD when an impl is
# absent) and the pre-commit hook install/uninstall logic.
#
# Run with:   bash .claude/test-install.sh

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$THIS_DIR/.." && pwd)"

HARNESS_TMP="$(mktemp -d "${TMPDIR:-/tmp}/agent-repl-install-test.XXXXXX")"
trap 'rm -rf "$HARNESS_TMP"' EXIT
FAKE_GIT_BIN="$HARNESS_TMP/bin"
mkdir -p "$FAKE_GIT_BIN"
ln -s "$REPO_ROOT/modules/app/agent-repl/bin/fake-git.sh" "$FAKE_GIT_BIN/git"
export PATH="$FAKE_GIT_BIN:$PATH"
if [ "$(command -v git)" != "$FAKE_GIT_BIN/git" ]; then
  echo "test-install.sh: the fake git is not the git on PATH; refusing to run real git" >&2
  exit 2
fi

# shellcheck source=../modules/app/agent-repl/bin/lib-harness-report.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)/../modules/app/agent-repl/bin/lib-harness-report.sh"

# Local skills install.sh expects to find under modules/app/agent-repl/skills/.
# agent-repl-debug is deliberately absent: it is PROJECT-scoped (checked-in
# <repo>/.claude/skills symlink), not a user-level installed skill.
LOCAL_SKILL_NAMES=( agent-repl-profile agent-repl-tests agent-repl-runtime agent-repl-merge-queue agent-repl-workspaces )

# Build a synthetic repo containing a fresh copy of install.sh and the
# requirements runner it delegates to, the pre-commit hook, and a requirements
# manifest naming the local skills, the hook, and one external skill 'foo'
# whose source is $1 (reached through FOO_SKILL_ROOT, as a real external skill
# is reached through MULTI_REPO_ROOT). When $1 is omitted an existing source
# dir is created under the repo so install succeeds; pass a nonexistent path
# to exercise the fail-hard branch.
mkfake_repo() {
  local root; root="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
  local impl_path="${1:-$root/impl/foo}"
  local module="$root/modules/app/agent-repl"
  mkdir -p "$root/.claude" "$root/.githooks" "$module/bin" "$module/skills"
  cp "$REPO_ROOT/.claude/install.sh" "$root/.claude/install.sh"
  cp "$REPO_ROOT/modules/app/agent-repl/bin/requirements.sh" "$module/bin/requirements.sh"
  cp "$REPO_ROOT/modules/app/agent-repl/bin/lib-worktrees.sh" "$module/bin/lib-worktrees.sh"
  cp "$REPO_ROOT/.githooks/pre-commit" "$root/.githooks/pre-commit"
  chmod +x "$root/.claude/install.sh" "$module/bin/requirements.sh" "$root/.githooks/pre-commit"
  # Create the default impl only when the caller did not supply one.
  if [ -z "${1:-}" ]; then
    mkdir -p "$root/impl/foo"
    echo "impl-content" > "$root/impl/foo/SKILL.md"
  fi
  write_manifest "$root" "$impl_path"
  # Local-skill source dirs (the manifest links straight to these).
  for s in "${LOCAL_SKILL_NAMES[@]}"; do
    mkdir -p "$module/skills/$s"
    printf 'name: %s\n' "$s" > "$module/skills/$s/SKILL.md"
  done
  # Init git so the runner can resolve --show-toplevel + --git-path.
  git -C "$root" init
  echo "$root"
}

# write_manifest ROOT FOO_SOURCE: the synthetic repo's requirements manifest.
# FOO_SKILL_ROOT is recorded beside it for run_install to export.
write_manifest() {
  local root="$1" impl_path="$2" s
  {
    printf '{"system": "agent-repl", "requirements": ['
    printf '{"id": "skill-foo", "kind": "skill_link", "why": "a test skill", "profile": "run", "name": "foo", "source": {"env": "FOO_SKILL_ROOT", "path": "%s"}}' "$(basename "$impl_path")"
    for s in "${LOCAL_SKILL_NAMES[@]}"; do
      printf ', {"id": "skill-%s", "kind": "skill_link", "why": "a local skill", "profile": "run", "name": "%s", "source": {"repo": "modules/app/agent-repl/skills/%s"}}' "$s" "$s" "$s"
    done
    printf ', {"id": "pre-commit-hook", "kind": "git_hook", "why": "the lint gate", "profile": "dev", "hook": "pre-commit", "source": ".githooks/pre-commit", "markers": ["AGENT_REPL_MANAGED_HOOK: agent-repl-precommit", "CLAUDE_REPL_MANAGED_HOOK: claude-repl-precommit"]}'
    printf ']}\n'
  } > "$root/modules/app/agent-repl/requirements.json"
  dirname "$impl_path" > "$root/.foo-skill-root"
}

mkfake_home() {
  local home; home="$(mktemp -d "${TMPDIR:-/tmp}/tmp.XXXXXXXXXX")"
  mkdir -p "$home/.claude/skills"
  echo "$home"
}

# Run install.sh in the isolated repo with fake HOME, capturing the exit
# code into LAST_RC and stdout+stderr into $repo/.install.log.
LAST_RC=0
run_install() {
  local repo="$1" home="$2" action="${3:-install}"
  set +e
  HOME="$home" FOO_SKILL_ROOT="$(cat "$repo/.foo-skill-root")" MULTI_REPO_CONFIG_DIR="${WORK_CONFIG:-}" \
    bash "$repo/.claude/install.sh" "$action" >"$repo/.install.log" 2>&1
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
test_install_default_action_exits_zero() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  if [ "$LAST_RC" -eq 0 ] && grep -q "install: every required entry is met" "$repo/.install.log"; then
    pass "default install exits zero"
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
  if [ "$LAST_RC" -eq 0 ] && grep -q "uninstall: done" "$repo/.install.log"; then
    pass "default uninstall exits zero"
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
     && grep -q "foo's source /nonexistent/impl-$$ does not exist" "$repo/.install.log" \
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
  git -C "$repo" add -A
  git -C "$repo" commit -qm init >/dev/null
  git -C "$repo" worktree add -q --detach "$repo/linked-wt" >/dev/null
  # Point the manifest's skill impl INSIDE the linked (non-main) worktree.
  mkdir -p "$repo/linked-wt/impl/bar"
  echo x > "$repo/linked-wt/impl/bar/SKILL.md"
  write_manifest "$repo" "$repo/linked-wt/impl/bar"
  sed -i '' 's/"name": "foo"/"name": "bar"/' "$repo/modules/app/agent-repl/requirements.json"
  run_install "$repo" "$home"
  if [ "$LAST_RC" -ne 0 ] \
     && grep -q "inside a linked worktree" "$repo/.install.log" \
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
  git -C "$repo" add -A
  git -C "$repo" commit -qm init >/dev/null
  git -C "$repo" worktree add -q --detach "$repo/linked-wt" >/dev/null
  # Invoke install.sh from the LINKED worktree's own checkout.
  set +e
  HOME="$home" FOO_SKILL_ROOT="$(cat "$repo/.foo-skill-root")" \
    bash "$repo/linked-wt/.claude/install.sh" install >"$repo/.install.log" 2>&1
  LAST_RC=$?
  set -e
  local actual; actual="$(readlink "$home/.claude/skills/agent-repl-profile" 2>/dev/null || echo MISSING)"
  # install.sh links to the PHYSICAL main-worktree path (matching git's
  # canonical `worktree list` output), so canonicalize the tmpdir-based
  # expectation too (mktemp yields /var/... which symlinks to /private/var/...
  # on macOS).
  local expected; expected="$(cd "$repo" && pwd -P)/modules/app/agent-repl/skills/agent-repl-profile"
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
  if [ "$LAST_RC" -ne 0 ] && [ -f "$home/.claude/skills/foo" ] && [ ! -L "$home/.claude/skills/foo" ] \
     && grep -q "is a file of yours" "$repo/.install.log"; then
    pass "non-symlink file is left untouched, and the install fails naming it"
  else
    fail "non-symlink preservation" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# retire_skill REPO NAME: add a retired skill_link entry NAME to the manifest.
retire_skill() {
  local manifest="$1/modules/app/agent-repl/requirements.json" tmp
  tmp="$(mktemp "${TMPDIR:-/tmp}/manifest.XXXXXX")"
  jq --arg n "$2" '.requirements += [{"id": ("skill-" + $n), "kind": "skill_link", "why": "retired", "retired": true, "profile": "run", "name": $n, "source": {"repo": ("modules/app/agent-repl/skills/" + $n)}}]' "$manifest" >"$tmp"
  mv "$tmp" "$manifest"
}

# --- install: a retired skill's link of ours is removed ---
test_install_removes_a_retired_skills_link() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  retire_skill "$repo" old-skill
  ln -s "$(cd "$repo" && pwd -P)/modules/app/agent-repl/skills/old-skill" "$home/.claude/skills/old-skill"
  run_install "$repo" "$home"
  if [ "$LAST_RC" -eq 0 ] && [ ! -L "$home/.claude/skills/old-skill" ] \
     && grep -q "skill-old-skill is outdated" "$repo/.install.log"; then
    pass "a retired skill's dangling link of ours is removed"
  else
    fail "retired skill link removal" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: a retired skill with no link is already met ---
test_install_a_retired_skill_with_no_link_is_met() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  retire_skill "$repo" old-skill
  run_install "$repo" "$home"
  if [ "$LAST_RC" -eq 0 ] && grep -q "old-skill is retired and not linked" "$repo/.install.log"; then
    pass "a retired skill with no link is met"
  else
    fail "retired skill without link" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: a file of the user's at a retired skill's name is left alone ---
test_install_leaves_a_users_file_at_a_retired_skills_name() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  retire_skill "$repo" old-skill
  echo "mine" > "$home/.claude/skills/old-skill"
  run_install "$repo" "$home"
  if [ "$LAST_RC" -eq 0 ] && [ -f "$home/.claude/skills/old-skill" ] && [ ! -L "$home/.claude/skills/old-skill" ]; then
    pass "a file of the user's at a retired skill's name is left alone"
  else
    fail "retired skill vs user file" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- the work account's config root (MULTI_REPO_CONFIG_DIR) ---
# Sessions under MULTI_REPO_ROOT run under the work account's config root, so
# every skill link is kept in its skills directory as well as ~/.claude's.

test_install_links_every_skill_into_the_work_config_root_too() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  WORK_CONFIG="$home/.claude-work" run_install "$repo" "$home"
  local main work; main="$(readlink "$home/.claude/skills/foo" 2>/dev/null || echo MISSING)"
  work="$(readlink "$home/.claude-work/skills/foo" 2>/dev/null || echo MISSING)"
  if [ "$LAST_RC" -eq 0 ] && [ "$main" = "$repo/impl/foo" ] && [ "$work" = "$repo/impl/foo" ]; then
    pass "install links a skill into the work account's config root and ~/.claude"
  else
    fail "work config root link" "rc: $LAST_RC" "main: $main" "work: $work" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

test_install_with_no_work_config_root_links_only_into_claude() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  if [ "$LAST_RC" -eq 0 ] && [ -L "$home/.claude/skills/foo" ] && [ ! -e "$home/.claude-work" ]; then
    pass "with no work config root, install links into ~/.claude alone"
  else
    fail "no work config root" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

test_check_reports_a_skill_missing_from_the_work_config_root() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  WORK_CONFIG="$home/.claude-work" run_install "$repo" "$home" check
  if grep -q "skill-foo: $home/.claude-work/skills/foo is not linked" "$repo/.install.log"; then
    pass "check reports a skill linked into ~/.claude but missing from the work config root"
  else
    fail "work config root check" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

test_install_removes_a_retired_skills_link_from_the_work_config_root() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  retire_skill "$repo" old-skill
  mkdir -p "$home/.claude-work/skills"
  ln -s "$(cd "$repo" && pwd -P)/modules/app/agent-repl/skills/old-skill" "$home/.claude-work/skills/old-skill"
  ln -s /somewhere/else "$home/.claude-work/skills/foreign"
  WORK_CONFIG="$home/.claude-work" run_install "$repo" "$home"
  if [ "$LAST_RC" -eq 0 ] && [ ! -L "$home/.claude-work/skills/old-skill" ] && [ -L "$home/.claude-work/skills/foreign" ]; then
    pass "a retired skill's link of ours comes out of the work config root, and a foreign link stays"
  else
    fail "retired link in work config root" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

test_uninstall_removes_the_work_config_roots_links_too() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  WORK_CONFIG="$home/.claude-work" run_install "$repo" "$home"
  WORK_CONFIG="$home/.claude-work" run_install "$repo" "$home" uninstall
  if [ "$LAST_RC" -eq 0 ] && [ ! -L "$home/.claude/skills/foo" ] && [ ! -L "$home/.claude-work/skills/foo" ]; then
    pass "uninstall removes our links from both config roots"
  else
    fail "uninstall in work config root" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- install: pre-commit hook is installed when absent ---
test_install_installs_pre_commit_hook() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  run_install "$repo" "$home"
  local dest="$repo/.fakegit/hooks/pre-commit"
  if [ -x "$dest" ] && grep -q "agent-repl-precommit" "$dest"; then
    pass "pre-commit hook installed into repo .fakegit/hooks"
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
  echo "# tampered" >> "$repo/.fakegit/hooks/pre-commit"
  run_install "$repo" "$home"  # rerun
  if ! grep -q "^# tampered" "$repo/.fakegit/hooks/pre-commit" \
     && grep -q "pre-commit-hook is outdated" "$repo/.install.log"; then
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
  mkdir -p "$repo/.fakegit/hooks"
  # A pre-rename copy: legacy marker only, plus a body we can detect.
  printf '#!/bin/sh\n# CLAUDE_REPL_MANAGED_HOOK: claude-repl-precommit\necho stale\n' \
    > "$repo/.fakegit/hooks/pre-commit"
  chmod +x "$repo/.fakegit/hooks/pre-commit"
  run_install "$repo" "$home"
  if ! grep -q "echo stale" "$repo/.fakegit/hooks/pre-commit" \
     && grep -q "AGENT_REPL_MANAGED_HOOK" "$repo/.fakegit/hooks/pre-commit" \
     && grep -q "pre-commit-hook is outdated" "$repo/.install.log"; then
    pass "legacy-marked pre-commit hook is refreshed, not called foreign"
  else
    fail "legacy-marked hook refresh" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# --- uninstall: a hook carrying only the LEGACY marker is removed ---
test_uninstall_removes_legacy_marked_hook() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  mkdir -p "$repo/.fakegit/hooks"
  printf '#!/bin/sh\n# CLAUDE_REPL_MANAGED_HOOK: claude-repl-precommit\necho stale\n' \
    > "$repo/.fakegit/hooks/pre-commit"
  chmod +x "$repo/.fakegit/hooks/pre-commit"
  run_install "$repo" "$home" uninstall
  if [ ! -e "$repo/.fakegit/hooks/pre-commit" ]; then
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
     && grep -q "is the checked-in hook (core.hooksPath)" "$repo/.install.log" \
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
  mkdir -p "$repo/.fakegit/hooks"
  printf '#!/bin/sh\necho foreign\n' > "$repo/.fakegit/hooks/pre-commit"
  chmod +x "$repo/.fakegit/hooks/pre-commit"
  run_install "$repo" "$home"
  if [ "$LAST_RC" -ne 0 ] && grep -q "echo foreign" "$repo/.fakegit/hooks/pre-commit" \
     && grep -q "is a hook of yours" "$repo/.install.log"; then
    pass "foreign pre-commit hook is preserved, and the install fails naming it"
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
  if [ ! -e "$repo/.fakegit/hooks/pre-commit" ]; then
    pass "uninstall removes managed pre-commit hook"
  else
    fail "uninstall managed hook"
  fi
  # Now seed a foreign hook and verify uninstall leaves it alone.
  cleanup "$repo" "$home"
  repo="$(mkfake_repo)"; home="$(mkfake_home)"
  mkdir -p "$repo/.fakegit/hooks"
  printf '#!/bin/sh\necho keep\n' > "$repo/.fakegit/hooks/pre-commit"
  chmod +x "$repo/.fakegit/hooks/pre-commit"
  run_install "$repo" "$home" uninstall
  if [ -f "$repo/.fakegit/hooks/pre-commit" ] && grep -q "echo keep" "$repo/.fakegit/hooks/pre-commit"; then
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

# ---------------------------------------------------------------------------
# command_link: a command of the repository linked into the user's bin dir.
# ---------------------------------------------------------------------------

# add_command_link REPO: add the agent-repld command_link entry to the
# manifest, with its source (a stub script) in the repository.
add_command_link() {
  local manifest="$1/modules/app/agent-repl/requirements.json" tmp
  printf '#!/bin/sh\necho agent-repld\n' >"$1/modules/app/agent-repl/bin/agent-repld"
  chmod +x "$1/modules/app/agent-repl/bin/agent-repld"
  tmp="$(mktemp "${TMPDIR:-/tmp}/manifest.XXXXXX")"
  jq '.requirements += [{"id": "agent-repld-on-path", "kind": "command_link", "why": "the agent API by name", "profile": "run", "name": "agent-repld", "source": {"repo": "modules/app/agent-repl/bin/agent-repld"}}]' "$manifest" >"$tmp"
  mv "$tmp" "$manifest"
}

# run_install_path REPO HOME BIN_DIR PATH_PREFIX [ACTION]: run_install with
# AGENT_REPL_BIN_DIR set to BIN_DIR and PATH_PREFIX put ahead of PATH.
run_install_path() {
  local repo="$1" home="$2" bin="$3" prefix="$4" action="${5:-install}"
  set +e
  HOME="$home" FOO_SKILL_ROOT="$(cat "$repo/.foo-skill-root")" AGENT_REPL_BIN_DIR="$bin" PATH="$prefix:$PATH" \
    bash "$repo/.claude/install.sh" "$action" >"$repo/.install.log" 2>&1
  LAST_RC=$?
  set -e
}

# command_source REPO: the physical main-worktree path the link must name.
command_source() { printf '%s/modules/app/agent-repl/bin/agent-repld\n' "$(cd "$1" && pwd -P)"; }

test_install_links_a_command_into_the_bin_dir_on_path() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  add_command_link "$repo"
  run_install_path "$repo" "$home" "$home/.local/bin" "$home/.local/bin"
  local actual; actual="$(readlink "$home/.local/bin/agent-repld" 2>/dev/null || echo MISSING)"
  if [ "$LAST_RC" -eq 0 ] && [ "$actual" = "$(command_source "$repo")" ] \
     && grep -q "ok          agent-repld-on-path: $home/.local/bin/agent-repld -> .*, on PATH" "$repo/.install.log"; then
    pass "a command link is made in the bin dir and found on PATH"
  else
    fail "command link install" "rc: $LAST_RC" "actual: $actual" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

test_install_a_command_link_honors_the_bin_dir_override() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  add_command_link "$repo"
  run_install_path "$repo" "$home" "$home/tools" "$home/tools"
  if [ "$LAST_RC" -eq 0 ] && [ -L "$home/tools/agent-repld" ] && [ ! -e "$home/.local/bin/agent-repld" ]; then
    pass "AGENT_REPL_BIN_DIR picks the dir a command is linked into"
  else
    fail "command link bin dir override" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

test_install_a_command_link_off_path_needs_the_user() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  add_command_link "$repo"
  run_install_path "$repo" "$home" "$home/.local/bin" "$home/elsewhere"
  if [ "$LAST_RC" -ne 0 ] && [ -L "$home/.local/bin/agent-repld" ] \
     && grep -q "needs-user  agent-repld-on-path: .*$home/.local/bin is not on PATH" "$repo/.install.log" \
     && grep -q "add export PATH=\"$home/.local/bin:\$PATH\" to your shell profile" "$repo/.install.log"; then
    pass "a command linked into a dir off PATH needs the user, naming the profile line"
  else
    fail "command link off PATH" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

test_install_a_command_shadowed_on_path_needs_the_user() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  add_command_link "$repo"
  mkdir -p "$home/shadow"
  printf '#!/bin/sh\n' >"$home/shadow/agent-repld"; chmod +x "$home/shadow/agent-repld"
  run_install_path "$repo" "$home" "$home/.local/bin" "$home/shadow:$home/.local/bin"
  if [ "$LAST_RC" -ne 0 ] && grep -q "agent-repld is found at $home/shadow/agent-repld ahead of $home/.local/bin/agent-repld" "$repo/.install.log"; then
    pass "a command shadowed earlier on PATH needs the user, naming the shadow"
  else
    fail "command link shadowed" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

test_install_preserves_a_users_file_at_the_commands_name() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  add_command_link "$repo"
  mkdir -p "$home/.local/bin"; echo "user file" >"$home/.local/bin/agent-repld"
  run_install_path "$repo" "$home" "$home/.local/bin" "$home/.local/bin"
  if [ "$LAST_RC" -ne 0 ] && [ ! -L "$home/.local/bin/agent-repld" ] && grep -q "user file" "$home/.local/bin/agent-repld" \
     && grep -q "$home/.local/bin/agent-repld is a file of yours" "$repo/.install.log"; then
    pass "a file of the user's at the command's name is never overwritten"
  else
    fail "command link user file" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

test_install_relinks_a_stale_command_link() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  add_command_link "$repo"
  mkdir -p "$home/.local/bin"; ln -s /some/stale/agent-repld "$home/.local/bin/agent-repld"
  run_install_path "$repo" "$home" "$home/.local/bin" "$home/.local/bin"
  if [ "$LAST_RC" -eq 0 ] && [ "$(readlink "$home/.local/bin/agent-repld")" = "$(command_source "$repo")" ] \
     && grep -q "agent-repld-on-path is outdated" "$repo/.install.log"; then
    pass "a stale command link is repaired"
  else
    fail "command link repair" "rc: $LAST_RC" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

test_uninstall_removes_the_command_link() {
  local repo home; repo="$(mkfake_repo)"; home="$(mkfake_home)"
  add_command_link "$repo"
  run_install_path "$repo" "$home" "$home/.local/bin" "$home/.local/bin"
  run_install_path "$repo" "$home" "$home/.local/bin" "$home/.local/bin" uninstall
  if [ ! -e "$home/.local/bin/agent-repld" ] && [ ! -L "$home/.local/bin/agent-repld" ] \
     && grep -q "uninstall: removed $home/.local/bin/agent-repld" "$repo/.install.log"; then
    pass "uninstall removes the command link"
  else
    fail "command link uninstall" "$(cat "$repo/.install.log")"
  fi
  cleanup "$repo" "$home"
}

# Every link kind checks its link through the one check_link, so a link kind
# with a hand-rolled check of its own fails here rather than drifting.
test_every_link_kind_checks_through_check_link() {
  local runner="$REPO_ROOT/modules/app/agent-repl/bin/requirements.sh" fn missing=""
  for fn in check_skill_link_in check_command_link; do
    if ! awk -v f="$fn" '$0 ~ "^"f"\\(\\)" {on=1} on && /^}/ {exit} on' "$runner" | grep -q 'check_link "\$name" "\$src" "\$dest"'; then
      missing="$missing $fn"
    fi
  done
  local refusals; refusals="$(grep -c 'is a file of yours, not a link' "$runner")"
  if [ -z "$missing" ] && [ "$refusals" -eq 1 ]; then
    pass "every link kind checks through check_link"
  else
    fail "shared link check" "not through check_link:$missing" "file-of-yours refusals: $refusals, want 1"
  fi
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
test_install_removes_a_retired_skills_link
test_install_a_retired_skill_with_no_link_is_met
test_install_leaves_a_users_file_at_a_retired_skills_name
test_install_links_every_skill_into_the_work_config_root_too
test_install_with_no_work_config_root_links_only_into_claude
test_check_reports_a_skill_missing_from_the_work_config_root
test_install_removes_a_retired_skills_link_from_the_work_config_root
test_uninstall_removes_the_work_config_roots_links_too
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
test_install_links_a_command_into_the_bin_dir_on_path
test_install_a_command_link_honors_the_bin_dir_override
test_install_a_command_link_off_path_needs_the_user
test_install_a_command_shadowed_on_path_needs_the_user
test_install_preserves_a_users_file_at_the_commands_name
test_install_relinks_a_stale_command_link
test_uninstall_removes_the_command_link
test_every_link_kind_checks_through_check_link

harness_finish
