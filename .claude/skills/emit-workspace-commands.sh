#!/usr/bin/env bash
# Reads a JSON workspace commands array from stdin and writes it atomically
# to ~/.claude/output/workspace_commands_<uuid>.json.
set -e

if ! command -v uuidgen &>/dev/null; then
  echo "ERROR: uuidgen is not available. Please rebuild the sandbox image by running .claude/install.sh and try again." >&2
  exit 1
fi

mkdir -p ~/.claude/output
tmp=$(mktemp ~/.claude/output/.workspace_commands_XXXXXX.json)
cat > "$tmp"
mv "$tmp" ~/.claude/output/workspace_commands_$(uuidgen).json
