#!/usr/bin/env bash
# Delete all Emacs/Doom cached and persisted state for a fresh start.
# Preserves: env (shell environment snapshot), straight/ (package installations)

set -euo pipefail

LOCAL_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/emacs/.local"

if [[ ! -d "$LOCAL_DIR" ]]; then
  echo "Nothing to clean: $LOCAL_DIR does not exist."
  exit 0
fi

dirs_to_remove=(
  "$LOCAL_DIR/cache"   # autosaves, undo history, projectile cache, native-comp
  "$LOCAL_DIR/etc"     # workspaces/persp sessions, bookmarks, projects, scratch, LSP servers
  "$LOCAL_DIR/state"   # projectile state, logs
)

echo "This will delete:"
for d in "${dirs_to_remove[@]}"; do
  if [[ -d "$d" ]]; then
    echo "  $d ($(du -sh "$d" 2>/dev/null | cut -f1))"
  fi
done

read -rp "Proceed? [y/N] " answer
if [[ "${answer,,}" != "y" ]]; then
  echo "Aborted."
  exit 1
fi

for d in "${dirs_to_remove[@]}"; do
  if [[ -d "$d" ]]; then
    rm -rf "$d"
    echo "Removed $d"
  fi
done

echo "Done. Run 'doom sync' and restart Emacs."
