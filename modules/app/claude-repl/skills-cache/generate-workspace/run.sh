#!/usr/bin/env bash
# Reads a JSON workspace commands array from stdin, prepends a user
# initials prefix (e.g. "JB/") and appends a random 3-letter lowercase
# suffix (e.g. "-abc") to each create entry's "name" field, then writes
# the result atomically to ~/.claude/output/workspace_commands_<uuid>.json.
#
# Prefix and suffix are added here — not by the caller — so workspace
# names always carry the right prefix for the current user and a
# deterministic disambiguator. Prefix resolution order:
#   1. CLAUDE_WORKSPACE_PREFIX env var (manual override)
#   2. Initials derived from `gns whoami` email local part (split on
#      '.', first char of each part, uppercased, concatenated)
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

# Resolve the workspace prefix for the current user.
WS_PREFIX="${CLAUDE_WORKSPACE_PREFIX:-}"
if [[ -z "$WS_PREFIX" ]]; then
  if ! command -v gns &>/dev/null; then
    echo "ERROR: gns is not available and CLAUDE_WORKSPACE_PREFIX is unset — cannot determine workspace prefix." >&2
    exit 1
  fi
  whoami_json=$(gns whoami --json 2>/dev/null || true)
  if [[ -z "$whoami_json" ]]; then
    echo "ERROR: 'gns whoami --json' failed — run 'gns auth login' or set CLAUDE_WORKSPACE_PREFIX." >&2
    exit 1
  fi
  WS_PREFIX=$(python3 -c '
import json, sys
data = json.loads(sys.argv[1] or "{}")
email = (data.get("email") or "").strip()
local = email.split("@", 1)[0] if "@" in email else email
initials = "".join(p[0].upper() for p in local.split(".") if p)
print(initials)
' "$whoami_json")
  if [[ -z "$WS_PREFIX" ]]; then
    echo "ERROR: could not derive workspace prefix from 'gns whoami' — set CLAUDE_WORKSPACE_PREFIX." >&2
    exit 1
  fi
fi

mkdir -p ~/.claude/output
# BSD mktemp on macOS only substitutes X's at the END of the template, so
# don't put an extension after them — pick up the .json on the final mv.
tmp=$(mktemp ~/.claude/output/.workspace_commands_XXXXXX)

# Pass the python program via -c so python3's stdin stays connected to
# run.sh's stdin (a `python3 - <<'PY'` heredoc would shadow it).
WS_PREFIX="$WS_PREFIX" python3 -c "$(cat <<'PY'
import json
import os
import secrets
import string
import sys

prefix = os.environ.get("WS_PREFIX", "")
if not prefix:
    print("ERROR: WS_PREFIX env var is empty inside run.sh python step", file=sys.stderr)
    sys.exit(2)

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
        name = entry["name"]
        suffix = "".join(secrets.choice(string.ascii_lowercase) for _ in range(3))
        entry["name"] = f"{prefix}/{name}-{suffix}"

json.dump(data, sys.stdout, indent=2)
sys.stdout.write("\n")
PY
)" >"$tmp"

mv "$tmp" "$HOME/.claude/output/workspace_commands_$(uuidgen).json"
