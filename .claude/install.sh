#!/usr/bin/env bash
# install.sh — manage claude-repl hooks in ~/.claude/settings.json
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
# Backs up ~/.claude/settings.json to settings.json.bak.<unix-ts> before
# any mutation.  No-ops when running inside the agent sandbox.
#
# Usage:
#   bash .claude/install.sh [install|uninstall|reinstall|help]
#
# Requires: jq, bash 4+.
set -euo pipefail

# --- Sandbox detection ---
# When executing inside the agent sandbox, the host's ~/.claude/ is not
# visible and writes hit the container FS, which is useless and surprising.
if [ -f /.dockerenv ] || [ "${DOOM_SANDBOX:-}" = "1" ]; then
  echo "[install.sh] Detected sandbox environment; skipping."
  echo "[install.sh] Run this script from the host to manage hooks."
  exit 0
fi

# --- Constants ---
SETTINGS="$HOME/.claude/settings.json"
HOOKS_DIR="$HOME/.claude/hooks"
SKILLS_DIR="$HOME/.claude/skills"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
HOOK_SCRIPTS_SRC="$SCRIPT_DIR/../modules/app/claude-repl/hooks"

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
)

# Resolve a $SCRIPT_DIR-relative path to an absolute canonical form
# (no ".." segments).  Symlink targets that pass through such
# segments would be valid but visually noisy and would break equality
# checks against canonical paths in other tooling.
_canonpath() {
  local raw="$1"
  if [ -d "$raw" ]; then
    ( cd "$raw" && pwd )
  else
    echo "$raw"
  fi
}

# Skills checked into THIS repo (under modules/app/claude-repl/skills/).
# Installed via symlink — the in-tree dir is the single source of truth
# so edits to checked-in SKILL.md go live without a reinstall.
LOCAL_SKILLS_SRC="$(_canonpath "$SCRIPT_DIR/../modules/app/claude-repl/skills")"
LOCAL_SKILLS=(
  "debug-logs"
  "profile"
  "workspace-close"
  "workspace-profile"
)

# Cached workspace-* skills.  The manifest declares each cached skill's
# name + canonical host impl path; sync.sh mirrors impl content into
# this dir on commit, and install.sh below symlinks each name into
# $SKILLS_DIR — preferring an existing doom-managed symlink to the live
# impl when one is already in place, falling back to the cache copy
# otherwise (so a fresh clone has working workspace-* skills even when
# the live impl trees aren't on the host).
SKILLS_CACHE_DIR="$(_canonpath "$SCRIPT_DIR/../modules/app/claude-repl/skills-cache")"
SKILLS_CACHE_SYNC="$SKILLS_CACHE_DIR/sync.sh"
SKILLS_CACHE_MANIFEST="$SKILLS_CACHE_DIR/manifest.sh"

# Pre-commit hook that keeps the skills cache in sync with the live
# impls.  Installed into the repo's git hooks dir by do_install below.
GITHOOKS_DIR="$(_canonpath "$SCRIPT_DIR/../.githooks")"

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

  # Cached workspace-* skills.  Idempotency rules:
  #   1. If $SKILLS_DIR/<name> is already a symlink pointing at the
  #      live impl declared in the manifest, that's a doom-managed
  #      symlink from a previous install — leave it alone, it gives
  #      the user live-edit semantics they've already opted into.
  #   2. If $SKILLS_DIR/<name> is already a symlink pointing at our
  #      cache copy, it's already correctly installed — skip.
  #   3. If $SKILLS_DIR/<name> is anything else (foreign symlink or
  #      regular file), warn and skip — don't overwrite the user.
  #   4. Otherwise, symlink the cache copy into $SKILLS_DIR.  The
  #      cache is the persisted fallback that ships with the repo.
  mkdir -p "$SKILLS_DIR"
  if [ ! -f "$SKILLS_CACHE_MANIFEST" ]; then
    echo "[install] WARNING: skills-cache manifest missing at $SKILLS_CACHE_MANIFEST"
    echo "[install] Cached skill installation will be skipped."
  else
    # shellcheck source=../modules/app/claude-repl/skills-cache/manifest.sh
    source "$SKILLS_CACHE_MANIFEST"
    for entry in "${CACHED_SKILLS[@]}"; do
      IFS='|' read -r name impl <<< "$entry"
      cache="$SKILLS_CACHE_DIR/$name"
      dest="$SKILLS_DIR/$name"

      if [ -L "$dest" ]; then
        target="$(readlink "$dest")"
        if [ "$target" = "$impl" ]; then
          echo "[install] Cached skill $name: live-impl symlink already in place (skipped)"
          continue
        fi
        if [ "$target" = "$cache" ]; then
          echo "[install] Cached skill $name: cache symlink already in place (skipped)"
          continue
        fi
        echo "[install] WARNING: $name has foreign symlink at $dest -> $target (skipped)"
        continue
      fi
      if [ -e "$dest" ]; then
        echo "[install] WARNING: $name has non-symlink file at $dest (skipped)"
        continue
      fi
      if [ ! -e "$cache" ] && [ ! -L "$cache" ]; then
        echo "[install] WARNING: cache missing for $name at $cache (skipped)"
        continue
      fi
      ln -sfn "$cache" "$dest"
      echo "[install] Linked cached skill: $dest -> $cache"
    done
  fi

  # Repo-local managed skills (under modules/app/claude-repl/skills/).
  # Symlinked into $SKILLS_DIR so edits to checked-in SKILL.md go live
  # immediately; no copy step required.
  if [ ! -d "$LOCAL_SKILLS_SRC" ]; then
    echo "[install] WARNING: local skills source dir not found: $LOCAL_SKILLS_SRC"
    echo "[install] Local skill symlinks will be skipped."
  else
    for name in "${LOCAL_SKILLS[@]}"; do
      src="$LOCAL_SKILLS_SRC/$name"
      dest="$SKILLS_DIR/$name"
      if [ ! -e "$src" ] && [ ! -L "$src" ]; then
        echo "[install] WARNING: local skill source missing: $src (skipped)"
        continue
      fi
      if [ -L "$dest" ] && [ "$(readlink "$dest")" = "$src" ]; then
        echo "[install] Local skill already linked: $name (skipped)"
        continue
      fi
      ln -sfn "$src" "$dest"
      echo "[install] Linked local skill: $dest -> $src"
    done
  fi

  # Install the skills-cache sync pre-commit hook into the repo's
  # current git hooks dir (wherever core.hooksPath / git config say
  # it is).  Idempotency:
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
        marker="CLAUDE_REPL_MANAGED_HOOK: claude-repl-skills-cache-sync"
        if [ ! -f "$dest_hook" ]; then
          cp "$src_hook" "$dest_hook"
          chmod +x "$dest_hook"
          echo "[install] Installed pre-commit hook -> $dest_hook"
        elif grep -q "$marker" "$dest_hook" 2>/dev/null; then
          cp "$src_hook" "$dest_hook"
          chmod +x "$dest_hook"
          echo "[install] Refreshed managed pre-commit hook -> $dest_hook"
        else
          echo "[install] WARNING: foreign pre-commit hook at $dest_hook (skipped)"
          echo "[install] To enable skills-cache sync, append the body of $src_hook to it."
        fi
      fi
    fi
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

  # Remove cached-skill symlinks — only ours (pointing at either the
  # live impl declared in the manifest or our in-repo cache copy).
  # Foreign files under the same name are left alone.
  if [ -f "$SKILLS_CACHE_MANIFEST" ]; then
    # shellcheck source=../modules/app/claude-repl/skills-cache/manifest.sh
    source "$SKILLS_CACHE_MANIFEST"
    for entry in "${CACHED_SKILLS[@]}"; do
      IFS='|' read -r name impl <<< "$entry"
      cache="$SKILLS_CACHE_DIR/$name"
      dest="$SKILLS_DIR/$name"
      if [ -L "$dest" ]; then
        target="$(readlink "$dest")"
        if [ "$target" = "$impl" ] || [ "$target" = "$cache" ]; then
          rm -f "$dest"
          echo "[uninstall] Removed cached-skill link: $dest"
        fi
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
        marker="CLAUDE_REPL_MANAGED_HOOK: claude-repl-skills-cache-sync"
        if [ -f "$dest_hook" ] && grep -q "$marker" "$dest_hook" 2>/dev/null; then
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
