#!/usr/bin/env bash
# install.sh — register claude-repl hooks in ~/.claude/settings.json
#
# Idempotent: safe to run multiple times.  For each managed hook, the entry
# is appended only if the exact command path is not already present under
# that event.  Foreign entries (unrelated hooks from other integrations) are
# preserved.
#
# Usage:
#   bash .claude/install.sh
#
# Requires: jq, bash 4+
set -euo pipefail

# --- Sandbox detection ---
# When we're executing inside the agent sandbox, the host's ~/.claude/ is
# not visible and any writes happen to the container's filesystem, which
# is surprising and useless. No-op here; the user installs from the host.
if [ -f /.dockerenv ] || [ "${DOOM_SANDBOX:-}" = "1" ]; then
  echo "[install] Detected sandbox environment; skipping install."
  echo "[install] Run this script from the host to install hooks."
  exit 0
fi

SETTINGS="$HOME/.claude/settings.json"
HOOKS_DIR="$HOME/.claude/hooks"

# Ensure settings.json exists with at least an empty object.  We back up
# pre-existing content before any mutation so a bad merge can be restored.
mkdir -p "$(dirname "$SETTINGS")"
if [ -f "$SETTINGS" ]; then
  backup="$SETTINGS.bak.$(date +%s)"
  cp "$SETTINGS" "$backup"
  echo "[install] Backed up $SETTINGS -> $backup"
else
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
# Idempotency rule: a managed hook is identified by the exact command path
# "~/.claude/hooks/<script>".  We look inside the existing event array for
# any entry whose .hooks[].command matches; if found, skip.  Otherwise we
# append a new entry, leaving any foreign entries in place.
for entry in "${HOOKS[@]}"; do
  IFS='|' read -r event script matcher <<< "$entry"
  script_path="~/.claude/hooks/$script"

  # Is our command already registered under this event?
  already=$(jq -r --arg event "$event" --arg cmd "$script_path" \
    '.hooks[$event] // [] | [.[].hooks[]?.command] | index($cmd)' \
    "$SETTINGS")
  if [ "$already" != "null" ]; then
    echo "[install] Hook already registered: $event -> $script (skipped)"
    continue
  fi

  # Build the single hook entry to append.
  if [ -n "$matcher" ]; then
    # Notification-style hook with matcher.
    hook_entry=$(jq -n --arg cmd "$script_path" --arg match "$matcher" \
      '{"matcher": $match, "hooks": [{"type": "command", "command": $cmd}]}')
  else
    # Standard hook (no matcher).
    hook_entry=$(jq -n --arg cmd "$script_path" \
      '{"hooks": [{"type": "command", "command": $cmd}]}')
  fi

  # Append into settings.json, preserving existing entries under this event.
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
