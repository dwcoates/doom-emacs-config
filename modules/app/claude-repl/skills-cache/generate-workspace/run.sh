#!/usr/bin/env bash
# Reads a JSON workspace commands array from stdin and writes it atomically
# to ~/.claude/output/workspace_commands_<uuid>.json. Validates the payload
# is a JSON array; otherwise passes the data through unchanged.
#
# Collision disambiguation (appending a 3-letter suffix to a `create`
# entry's `name` when it would clash with an existing workspace, on-disk
# worktree, git branch, or start tag) lives in the downstream consumer
# (Emacs), NOT here.  This script does not mutate names.
#
# (Previously a wrapper that exec'd ../emit-workspace-commands.sh; inlined
# here so the skill is self-contained and can be `gns skills publish`'d.)
set -e

if ! command -v uuidgen &>/dev/null; then
  echo "ERROR: uuidgen is not available. Please rebuild the sandbox image by running .claude/install.sh and try again." >&2
  exit 1
fi

if ! command -v python3 &>/dev/null; then
  echo "ERROR: python3 is not available. Please rebuild the sandbox image by running .claude/install.sh and try again." >&2
  exit 1
fi

mkdir -p ~/.claude/output
# BSD mktemp on macOS only substitutes X's at the END of the template, so
# don't put an extension after them — pick up the .json on the final mv.
tmp=$(mktemp ~/.claude/output/.workspace_commands_XXXXXX)

# Pass the python program via -c so python3's stdin stays connected to
# run.sh's stdin (a `python3 - <<'PY'` heredoc would shadow it).
python3 -c "$(cat <<'PY'
import json
import sys

try:
    data = json.load(sys.stdin)
except json.JSONDecodeError as e:
    print(f"ERROR: stdin is not valid JSON: {e}", file=sys.stderr)
    sys.exit(2)

if not isinstance(data, list):
    print("ERROR: workspace commands payload must be a JSON array", file=sys.stderr)
    sys.exit(2)

json.dump(data, sys.stdout, indent=2)
sys.stdout.write("\n")
PY
)" >"$tmp"

mv "$tmp" "$HOME/.claude/output/workspace_commands_$(uuidgen).json"
