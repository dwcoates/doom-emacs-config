#!/usr/bin/env bash
# Reads a JSON workspace commands array from stdin, appends a random
# 3-letter lowercase suffix (e.g. "-abc") to each create entry's "name"
# field, then writes the result atomically to
# ~/.claude/output/workspace_commands_<uuid>.json.
#
# The suffix is added here — not by the caller — so workspace names
# always carry a deterministic disambiguator, even when callers forget
# to include one. This keeps suffix generation off the LLM's hot path.
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
import secrets
import string
import sys

try:
    data = json.load(sys.stdin)
except json.JSONDecodeError as e:
    print(f"ERROR: stdin is not valid JSON: {e}", file=sys.stderr)
    sys.exit(2)

if not isinstance(data, list):
    print("ERROR: workspace commands payload must be a JSON array", file=sys.stderr)
    sys.exit(2)

for entry in data:
    if (
        isinstance(entry, dict)
        and entry.get("type") == "create"
        and isinstance(entry.get("name"), str)
        and entry["name"]
    ):
        suffix = "".join(secrets.choice(string.ascii_lowercase) for _ in range(3))
        entry["name"] = f"{entry['name']}-{suffix}"

json.dump(data, sys.stdout, indent=2)
sys.stdout.write("\n")
PY
)" >"$tmp"

mv "$tmp" "$HOME/.claude/output/workspace_commands_$(uuidgen).json"
