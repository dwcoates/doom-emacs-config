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
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
HOOK_SCRIPTS_SRC="$SCRIPT_DIR/../modules/app/claude-repl/hooks"

# Each entry: EVENT_KEY|SCRIPT_NAME|MATCHER
# MATCHER is optional (only used for Notification hooks).
HOOKS=(
  "Stop|stop-notify.sh|"
  "UserPromptSubmit|prompt-submit-notify.sh|"
  "SessionStart|session-start-notify.sh|"
  "Notification|permission-notify.sh|permission_prompt"
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
