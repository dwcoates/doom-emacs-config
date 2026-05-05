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
# Source tree for managed skills.  Override via CLAUDE_REPL_SKILLS_SRC.
# Symlinks are created at install time, so $HOME expands on the host;
# nothing is committed with a hardcoded user path.
SKILLS_SRC="${CLAUDE_REPL_SKILLS_SRC:-$HOME/workspace/ChessCom/explanation-engine/.claude/skills}"
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

# Host-level skill symlinks managed by this script.  Each name resolves
# to $SKILLS_DIR/<name> -> $SKILLS_SRC/<name>.
SKILLS=(
  "emit-workspace-commands.sh"
  "generate-workspace"
  "workspace-update"
)

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

  # Managed skill symlinks under ~/.claude/skills/.  Idempotent: replace
  # when the target differs, skip when correct, warn when source absent.
  mkdir -p "$SKILLS_DIR"
  if [ ! -d "$SKILLS_SRC" ]; then
    echo "[install] WARNING: skills source dir not found: $SKILLS_SRC"
    echo "[install] Skill symlinks will be skipped.  Set CLAUDE_REPL_SKILLS_SRC to override."
  else
    for name in "${SKILLS[@]}"; do
      src="$SKILLS_SRC/$name"
      dest="$SKILLS_DIR/$name"
      if [ ! -e "$src" ] && [ ! -L "$src" ]; then
        echo "[install] WARNING: skill source missing: $src (skipped)"
        continue
      fi
      if [ -L "$dest" ] && [ "$(readlink "$dest")" = "$src" ]; then
        echo "[install] Skill already linked: $name (skipped)"
        continue
      fi
      ln -sfn "$src" "$dest"
      echo "[install] Linked skill: $dest -> $src"
    done
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

  # Remove managed skill symlinks — only ours (those pointing at SKILLS_SRC).
  # Foreign files under the same name are left alone.
  for name in "${SKILLS[@]}"; do
    dest="$SKILLS_DIR/$name"
    expected="$SKILLS_SRC/$name"
    if [ -L "$dest" ] && [ "$(readlink "$dest")" = "$expected" ]; then
      rm -f "$dest"
      echo "[uninstall] Removed skill link: $dest"
    fi
  done

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
