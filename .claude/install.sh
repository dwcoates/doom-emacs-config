#!/usr/bin/env bash
# install.sh — manage claude-repl hooks in ~/.claude/settings.json and
# install the workspace-* / local skills as symlinks under ~/.claude/skills.
#
# Subcommands:
#   install    (default) Copy managed hook scripts and register them in
#              ~/.claude/settings.json.  Idempotent: safe to run again.
#              Managed entries are identified by the exact command path
#              "~/.claude/hooks/<script>"; foreign entries under the same
#              event keys are preserved.
#   uninstall  Remove managed registrations (preserving foreign entries)
#              and delete the managed hook scripts from ~/.claude/hooks/.
#              Drops an event key when its array becomes empty.
#   reinstall  uninstall then install.  Useful after editing a checked-in
#              managed script.
#   help       Show usage.
#
# Skills: each manifest-declared skill is symlinked into ~/.claude/skills
# straight to its CANONICAL impl path.  There is NO cache fallback — if a
# canonical impl is absent, install FAILS HARD (non-zero exit) rather than
# silently linking a stale copy.  A skill is always the live in-tree
# source, so edits go live with no reinstall.
#
# Backs up ~/.claude/settings.json to settings.json.bak.<unix-ts> before
# any mutation.
#
# Usage:
#   bash .claude/install.sh [install|uninstall|reinstall|help]
#
# Requires: jq, bash 4+.
set -euo pipefail

# --- Shared constants + helpers (needed by both the sandbox repair path
#     and the full install path below) ---
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SKILLS_DIR="$HOME/.claude/skills"

# Resolve a $SCRIPT_DIR-relative path to an absolute canonical form
# (no ".." segments).  Symlink targets that pass through such segments
# would be valid but visually noisy and would break equality checks
# against canonical paths in other tooling.
_canonpath() {
  local raw="$1"
  if [ -d "$raw" ]; then
    ( cd "$raw" && pwd )
  else
    echo "$raw"
  fi
}

# --- Main-worktree resolution ---
# Skill symlinks must ALWAYS target the MAIN worktree, never a transient
# linked worktree (whose path dangles the moment the worktree is pruned).
# `git worktree list --porcelain` emits the main worktree as its FIRST
# `worktree` line; every entry after it is a linked (non-main) worktree.
_main_worktree_root() {
  git -C "$SCRIPT_DIR" worktree list --porcelain 2>/dev/null \
    | awk '/^worktree /{print $2; exit}'
}

# Roots of all NON-main worktrees (every entry after the first).
_nonmain_worktree_roots() {
  git -C "$SCRIPT_DIR" worktree list --porcelain 2>/dev/null \
    | awk '/^worktree /{print $2}' | tail -n +2
}

# Resolve the main worktree, falling back to the SCRIPT_DIR-relative repo
# root when git is unavailable (e.g. the dir is not a git checkout).
MAIN_WORKTREE="$(_main_worktree_root || true)"
if [ -z "$MAIN_WORKTREE" ]; then
  MAIN_WORKTREE="$(_canonpath "$SCRIPT_DIR/..")"
fi

# Return 0 when IMPL resolves inside a non-main worktree of this repo.
# Used to FORBID linking a skill to a transient-worktree path: such links
# dangle once the worktree is pruned, so we fail hard instead.
_impl_in_nonmain_worktree() {
  local impl="$1" root
  while IFS= read -r root; do
    [ -n "$root" ] || continue
    case "$impl/" in
      "$root"/*) return 0 ;;
    esac
  done < <(_nonmain_worktree_roots)
  return 1
}

# Skills checked into THIS (doom) repo, under modules/app/claude-repl/skills/.
# Symlinked into $SKILLS_DIR straight to the MAIN worktree's in-tree source —
# NEVER the invoking (possibly linked) worktree, so the link survives a
# worktree prune.
LOCAL_SKILLS_SRC="$MAIN_WORKTREE/modules/app/claude-repl/skills"
LOCAL_SKILLS=(
  "debug-logs"
  "profile"
  "runtime-eval-code"
  "workspace-close"
)

# Manifest declaring each cached workspace-* skill as "name|canonical-impl".
# The manifest is the single source of truth for where each skill lives;
# install always links straight to that impl (no cache copy exists).
SKILLS_MANIFEST="$(_canonpath "$SCRIPT_DIR/../modules/app/claude-repl/skills-cache")/manifest.sh"

# Link cached skill NAME to its canonical IMPL under $SKILLS_DIR.
# NO fallback: a missing IMPL returns 1 so the caller can fail hard.  A
# real (non-symlink) file at the destination is left untouched so a user
# file is never trampled.  An existing symlink (ours or a stale one) is
# repaired to point at IMPL.  TAG prefixes log lines.
link_skill_to_impl() {
  local name="$1" impl="$2" tag="${3:-install}"
  local dest="$SKILLS_DIR/$name"
  # FORBID linking into a non-main worktree: the link would dangle once
  # that transient worktree is pruned.  Fail hard so the bad manifest
  # entry (or a worktree-relative source) surfaces loudly.
  if _impl_in_nonmain_worktree "$impl"; then
    echo "[$tag] ERROR: skill '$name' impl is inside a non-main worktree: $impl" >&2
    echo "[$tag]        symlinks must target the main worktree: $MAIN_WORKTREE" >&2
    return 1
  fi
  if [ ! -e "$impl" ] && [ ! -L "$impl" ]; then
    echo "[$tag] ERROR: canonical impl for '$name' not found at $impl" >&2
    return 1
  fi
  if [ -e "$dest" ] && [ ! -L "$dest" ]; then
    echo "[$tag] WARNING: $name has a non-symlink file at $dest (skipped, not trampling)"
    return 0
  fi
  ln -sfn "$impl" "$dest"
  echo "[$tag] Linked $name -> $impl"
  return 0
}

# --- Sandbox detection ---
# When executing inside the agent sandbox, the host's ~/.claude/ is
# bind-mounted but skill symlinks in ~/.claude/skills/ may point to
# absolute host paths that don't exist in the container.  Repair them by
# relinking to the SAME canonical impl the host uses.  There is NO cache
# fallback: if a canonical impl is absent we FAIL HARD so a broken
# environment surfaces loudly instead of silently serving stale code.
# Hooks/settings setup is still skipped — only skills are repaired.
if { [ -f /.dockerenv ] || [ "${DOOM_SANDBOX:-}" = "1" ]; } \
   && [ "${INSTALL_SH_SKIP_SANDBOX_DETECT:-}" != "1" ]; then
  echo "[install.sh] Detected sandbox environment — running skill symlink repair only."
  mkdir -p "$SKILLS_DIR"
  missing=0

  if [ ! -f "$SKILLS_MANIFEST" ]; then
    echo "[install.sh/sandbox] ERROR: skills manifest missing at $SKILLS_MANIFEST" >&2
    exit 1
  fi
  # shellcheck source=../modules/app/claude-repl/skills-cache/manifest.sh
  source "$SKILLS_MANIFEST"
  for entry in "${CACHED_SKILLS[@]}"; do
    IFS='|' read -r name impl <<< "$entry"
    link_skill_to_impl "$name" "$impl" "install.sh/sandbox" || missing=$((missing + 1))
  done
  for name in "${LOCAL_SKILLS[@]}"; do
    link_skill_to_impl "$name" "$LOCAL_SKILLS_SRC/$name" "install.sh/sandbox" || missing=$((missing + 1))
  done

  if [ "$missing" -gt 0 ]; then
    echo "[install.sh/sandbox] FAILED: $missing canonical impl path(s) missing — no fallback, refusing to leave stale links." >&2
    exit 1
  fi
  echo "[install.sh] Sandbox skill repair complete."
  exit 0
fi

# --- Constants (full install path) ---
SETTINGS="$HOME/.claude/settings.json"
HOOKS_DIR="$HOME/.claude/hooks"
HOOK_SCRIPTS_SRC="$SCRIPT_DIR/../modules/app/claude-repl/hooks"

# Pre-commit hook (ERT + boundary gate) installed into the repo's git
# hooks dir by do_install below.
GITHOOKS_DIR="$(_canonpath "$SCRIPT_DIR/../.githooks")"

# Each entry: EVENT_KEY|SCRIPT_NAME|MATCHER
# MATCHER is optional (only used for Notification hooks).
HOOKS=(
  "Stop|stop-notify.sh|"
  "StopFailure|stop-failure-notify.sh|"
  "SubagentStart|subagent-start-notify.sh|"
  "SubagentStop|subagent-stop-notify.sh|"
  "UserPromptSubmit|prompt-submit-notify.sh|"
  "SessionStart|session-start-notify.sh|"
  "Notification|permission-notify.sh|permission_prompt"
  # PermissionRequest fires at the moment the permission dialog appears,
  # BEFORE the user answers — that's the real-time signal the tab-bar
  # needs to show `:permission' WHILE Claude is waiting on the user.
  # The older Notification hook above is kept as a fallback (and for the
  # 60s-idle nudge that arrives under the same `permission_prompt'
  # notification type).
  "PermissionRequest|permission-request-notify.sh|"
)

# Marker identifying our managed pre-commit hook so install/uninstall can
# refresh or remove it without touching a foreign pre-commit hook.
PRECOMMIT_MARKER="CLAUDE_REPL_MANAGED_HOOK: claude-repl-precommit"

# --- Helpers ---

show_help() {
  cat <<USAGE
Usage: bash $0 [install|uninstall|reinstall|help]

  install    (default) Copy managed hook scripts and register them in
             ~/.claude/settings.json.  Idempotent.
  uninstall  Remove managed registrations (preserving foreign entries)
             and delete the managed hook scripts.
  reinstall  uninstall then install.
  help       Show this message.
USAGE
}

# Back up settings.json if present.  First arg is the log tag.
backup_settings() {
  if [ -f "$SETTINGS" ]; then
    local backup="$SETTINGS.bak.$(date +%s)"
    cp "$SETTINGS" "$backup"
    echo "[$1] Backed up $SETTINGS -> $backup"
  fi
}

# --- Install ---

do_install() {
  mkdir -p "$(dirname "$SETTINGS")"
  if [ -f "$SETTINGS" ]; then
    backup_settings install
  else
    echo '{}' > "$SETTINGS"
  fi
  mkdir -p "$HOOKS_DIR"

  # Copy managed scripts from the checked-in source tree.
  if [ -d "$HOOK_SCRIPTS_SRC" ]; then
    for src in "$HOOK_SCRIPTS_SRC"/*.sh; do
      [ -f "$src" ] || continue
      dest="$HOOKS_DIR/$(basename "$src")"
      cp "$src" "$dest"
      chmod +x "$dest"
      echo "[install] Copied $(basename "$src") -> $dest"
    done
  else
    echo "[install] WARNING: hook scripts source dir not found: $HOOK_SCRIPTS_SRC"
    echo "[install] Hook scripts must already be in $HOOKS_DIR"
  fi

  # Idempotency rule: a managed hook is identified by the exact command
  # path "~/.claude/hooks/<script>".  Look for it inside the existing event
  # array; if found, skip.  Otherwise append, preserving foreign entries.
  for entry in "${HOOKS[@]}"; do
    IFS='|' read -r event script matcher <<< "$entry"
    script_path="~/.claude/hooks/$script"

    already=$(jq -r --arg event "$event" --arg cmd "$script_path" \
      '.hooks[$event] // [] | [.[].hooks[]?.command] | index($cmd)' \
      "$SETTINGS")
    if [ "$already" != "null" ]; then
      echo "[install] Hook already registered: $event -> $script (skipped)"
      continue
    fi

    if [ -n "$matcher" ]; then
      hook_entry=$(jq -n --arg cmd "$script_path" --arg match "$matcher" \
        '{"matcher": $match, "hooks": [{"type": "command", "command": $cmd}]}')
    else
      hook_entry=$(jq -n --arg cmd "$script_path" \
        '{"hooks": [{"type": "command", "command": $cmd}]}')
    fi

    jq --arg event "$event" --argjson entry "$hook_entry" \
      '
      .hooks //= {}
      | .hooks[$event] //= []
      | .hooks[$event] += [$entry]
      ' "$SETTINGS" > "$SETTINGS.tmp" \
      && mv "$SETTINGS.tmp" "$SETTINGS"

    echo "[install] Registered hook: $event -> $script"
  done

  # Cached workspace-* skills.  Each is symlinked straight to its
  # canonical impl from the manifest.  NO fallback: a missing impl is a
  # hard error (fail loudly rather than serve a stale copy).  A real file
  # already at the destination is left untouched.
  mkdir -p "$SKILLS_DIR"
  if [ ! -f "$SKILLS_MANIFEST" ]; then
    echo "[install] ERROR: skills manifest missing at $SKILLS_MANIFEST" >&2
    exit 1
  fi
  # shellcheck source=../modules/app/claude-repl/skills-cache/manifest.sh
  source "$SKILLS_MANIFEST"
  missing=0
  for entry in "${CACHED_SKILLS[@]}"; do
    IFS='|' read -r name impl <<< "$entry"
    link_skill_to_impl "$name" "$impl" "install" || missing=$((missing + 1))
  done

  # Repo-local managed skills (under modules/app/claude-repl/skills/),
  # symlinked straight to the in-tree source so SKILL.md edits go live.
  if [ ! -d "$LOCAL_SKILLS_SRC" ]; then
    echo "[install] ERROR: local skills source dir not found: $LOCAL_SKILLS_SRC" >&2
    exit 1
  fi
  for name in "${LOCAL_SKILLS[@]}"; do
    link_skill_to_impl "$name" "$LOCAL_SKILLS_SRC/$name" "install" || missing=$((missing + 1))
  done

  # Install the pre-commit hook (ERT + external-boundary gate) into the
  # repo's current git hooks dir (wherever core.hooksPath / git config
  # say it is).  Idempotency:
  #   - No pre-commit exists  → copy ours in.
  #   - Pre-commit has our marker → refresh (the user re-ran install).
  #   - Foreign pre-commit exists → warn, skip, don't trample.
  if [ -d "$GITHOOKS_DIR" ] && command -v git >/dev/null 2>&1; then
    repo_top="$(git -C "$GITHOOKS_DIR" rev-parse --show-toplevel 2>/dev/null || true)"
    if [ -n "$repo_top" ]; then
      hooks_path="$(git -C "$repo_top" rev-parse --git-path hooks 2>/dev/null || true)"
      if [ -n "$hooks_path" ]; then
        if [[ "$hooks_path" != /* ]]; then
          hooks_path="$repo_top/$hooks_path"
        fi
        mkdir -p "$hooks_path"
        src_hook="$GITHOOKS_DIR/pre-commit"
        dest_hook="$hooks_path/pre-commit"
        if [ ! -f "$dest_hook" ]; then
          cp "$src_hook" "$dest_hook"
          chmod +x "$dest_hook"
          echo "[install] Installed pre-commit hook -> $dest_hook"
        elif grep -q "$PRECOMMIT_MARKER" "$dest_hook" 2>/dev/null; then
          cp "$src_hook" "$dest_hook"
          chmod +x "$dest_hook"
          echo "[install] Refreshed managed pre-commit hook -> $dest_hook"
        else
          echo "[install] WARNING: foreign pre-commit hook at $dest_hook (skipped)"
          echo "[install] To enable the claude-repl test gate, append the body of $src_hook to it."
        fi
      fi
    fi
  fi

  # Fail hard at the END (after doing all possible work) when any skill
  # impl was missing, so a broken manifest entry surfaces loudly while the
  # valid skills and the hook still get installed.  No fallback.
  if [ "$missing" -gt 0 ]; then
    echo "[install] FAILED: $missing skill impl path(s) missing — no fallback, fix the manifest impl path(s) above." >&2
    exit 1
  fi

  echo "[install] Done. Hooks registered in $SETTINGS"
}

# --- Uninstall ---

do_uninstall() {
  if [ ! -f "$SETTINGS" ]; then
    echo "[uninstall] No settings.json at $SETTINGS; nothing to uninstall."
  else
    backup_settings uninstall

    for entry in "${HOOKS[@]}"; do
      IFS='|' read -r event script matcher <<< "$entry"
      script_path="~/.claude/hooks/$script"

      # Drop any entries whose inner .hooks[].command equals ours.  If the
      # event's array becomes empty, delete the event key entirely.
      jq --arg event "$event" --arg cmd "$script_path" '
        if .hooks[$event] then
          .hooks[$event] |= map(select(
            ([.hooks[]?.command] | index($cmd)) | not
          ))
          | if .hooks[$event] == [] then del(.hooks[$event]) else . end
        else . end
      ' "$SETTINGS" > "$SETTINGS.tmp" \
        && mv "$SETTINGS.tmp" "$SETTINGS"

      echo "[uninstall] Removed registration: $event -> $script"
    done
  fi

  # Delete managed scripts from the install location.
  for entry in "${HOOKS[@]}"; do
    IFS='|' read -r _event script _matcher <<< "$entry"
    if [ -f "$HOOKS_DIR/$script" ]; then
      rm -f "$HOOKS_DIR/$script"
      echo "[uninstall] Removed $HOOKS_DIR/$script"
    fi
  done

  # Remove cached-skill symlinks — only ours (pointing at the canonical
  # impl declared in the manifest).  Foreign files are left alone.
  if [ -f "$SKILLS_MANIFEST" ]; then
    # shellcheck source=../modules/app/claude-repl/skills-cache/manifest.sh
    source "$SKILLS_MANIFEST"
    for entry in "${CACHED_SKILLS[@]}"; do
      IFS='|' read -r name impl <<< "$entry"
      dest="$SKILLS_DIR/$name"
      if [ -L "$dest" ] && [ "$(readlink "$dest")" = "$impl" ]; then
        rm -f "$dest"
        echo "[uninstall] Removed cached-skill link: $dest"
      fi
    done
  fi

  # Remove repo-local skill symlinks (only ours, pointing at LOCAL_SKILLS_SRC).
  for name in "${LOCAL_SKILLS[@]}"; do
    dest="$SKILLS_DIR/$name"
    expected="$LOCAL_SKILLS_SRC/$name"
    if [ -L "$dest" ] && [ "$(readlink "$dest")" = "$expected" ]; then
      rm -f "$dest"
      echo "[uninstall] Removed local skill link: $dest"
    fi
  done

  # Remove the managed pre-commit hook only if it carries our marker.
  if command -v git >/dev/null 2>&1 && [ -d "$GITHOOKS_DIR" ]; then
    repo_top="$(git -C "$GITHOOKS_DIR" rev-parse --show-toplevel 2>/dev/null || true)"
    if [ -n "$repo_top" ]; then
      hooks_path="$(git -C "$repo_top" rev-parse --git-path hooks 2>/dev/null || true)"
      if [ -n "$hooks_path" ]; then
        if [[ "$hooks_path" != /* ]]; then
          hooks_path="$repo_top/$hooks_path"
        fi
        dest_hook="$hooks_path/pre-commit"
        if [ -f "$dest_hook" ] && grep -q "$PRECOMMIT_MARKER" "$dest_hook" 2>/dev/null; then
          rm -f "$dest_hook"
          echo "[uninstall] Removed managed pre-commit hook: $dest_hook"
        fi
      fi
    fi
  fi

  echo "[uninstall] Done."
}

# --- Dispatch ---

ACTION="${1:-install}"
case "$ACTION" in
  install)        do_install ;;
  uninstall)      do_uninstall ;;
  reinstall)      do_uninstall; do_install ;;
  -h|--help|help) show_help; exit 0 ;;
  *)              echo "[install.sh] Unknown action: $ACTION" >&2
                  show_help
                  exit 2
                  ;;
esac
