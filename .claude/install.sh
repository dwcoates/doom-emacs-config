#!/usr/bin/env bash
# install.sh — register claude-repl hooks in ~/.claude/settings.json
#
# Idempotent: safe to run multiple times.  Each hook is only added if its
# event key is not already present in settings.json.
#
# Usage:
#   bash .claude/install.sh
#
# Requires: jq, bash 4+
set -euo pipefail

SETTINGS="$HOME/.claude/settings.json"
HOOKS_DIR="$HOME/.claude/hooks"

# Ensure settings.json exists with at least an empty object.
mkdir -p "$(dirname "$SETTINGS")"
if [ ! -f "$SETTINGS" ]; then
  echo '{}' > "$SETTINGS"
fi

# Ensure hooks directory exists.
mkdir -p "$HOOKS_DIR"

# --- Hook definitions ---
# Each entry: EVENT_KEY  SCRIPT_NAME  [MATCHER]
# MATCHER is optional (only used for Notification hooks).
HOOKS=(
  "Stop|stop-notify.sh|"
  "UserPromptSubmit|prompt-submit-notify.sh|"
  "SessionStart|session-start-notify.sh|"
  "Notification|permission-notify.sh|permission_prompt"
)

# --- Install hook scripts ---
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
HOOK_SCRIPTS_SRC="$SCRIPT_DIR/../modules/app/claude-repl/hooks"

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

# --- Register hooks in settings.json ---
for entry in "${HOOKS[@]}"; do
  IFS='|' read -r event script matcher <<< "$entry"
  script_path="~/.claude/hooks/$script"

  # Check if event key already exists in hooks.
  existing=$(jq -r ".hooks.\"$event\" // empty" "$SETTINGS")
  if [ -n "$existing" ]; then
    echo "[install] Hook already registered: $event -> $script (skipped)"
    continue
  fi

  # Build the hook entry.
  if [ -n "$matcher" ]; then
    # Notification-style hook with matcher.
    hook_json=$(jq -n --arg cmd "$script_path" --arg match "$matcher" \
      '[{"matcher": $match, "hooks": [{"type": "command", "command": $cmd}]}]')
  else
    # Standard hook (no matcher).
    hook_json=$(jq -n --arg cmd "$script_path" \
      '[{"hooks": [{"type": "command", "command": $cmd}]}]')
  fi

  # Merge into settings.json.
  jq --arg event "$event" --argjson hook "$hook_json" \
    '.hooks[$event] = $hook' "$SETTINGS" > "$SETTINGS.tmp" \
    && mv "$SETTINGS.tmp" "$SETTINGS"

  echo "[install] Registered hook: $event -> $script"
done

echo "[install] Done. Hooks registered in $SETTINGS"
