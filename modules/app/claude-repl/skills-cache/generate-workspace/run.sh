#!/usr/bin/env bash
# Reads a JSON workspace commands array from stdin, decorates each create
# entry's "name" field, then writes the result atomically to
# ~/.claude/output/workspace_commands_<uuid>.json.
#
# When CLAUDE_WORKSPACE_PREFIX is set in the environment, it is prepended
# to each create entry's name as "PREFIX/name-<suffix>". When unset or
# empty, no prefix is added and the name is emitted as "name-<suffix>".
# There is no derivation or fallback — absent env var means no prefix.
set -e

if ! command -v uuidgen &>/dev/null; then
  echo "ERROR: uuidgen is not available. Please rebuild the sandbox image by running .claude/install.sh and try again." >&2
  exit 1
fi

if ! command -v python3 &>/dev/null; then
  echo "ERROR: python3 is not available. Please rebuild the sandbox image by running .claude/install.sh and try again." >&2
  exit 1
fi

WS_PREFIX="${CLAUDE_WORKSPACE_PREFIX:-}"
if [[ -z "$WS_PREFIX" ]]; then
  echo "NOTICE: CLAUDE_WORKSPACE_PREFIX is unset — emitting workspace name(s) with no branch prefix. Export CLAUDE_WORKSPACE_PREFIX=<your-prefix> in the calling environment if you want one prepended (e.g. 'JB/<slug>')." >&2
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
import re
import secrets
import string
import subprocess
import sys

prefix = os.environ.get("WS_PREFIX", "")

try:
    data = json.load(sys.stdin)
except json.JSONDecodeError as e:
    print(f"ERROR: stdin is not valid JSON: {e}", file=sys.stderr)
    sys.exit(2)

if not isinstance(data, list):
    print("ERROR: workspace commands payload must be a JSON array", file=sys.stderr)
    sys.exit(2)


def resolve_default_branch(git_root):
    """Return the default branch for git_root, falling back to 'master'.

    Resolution order:
      1. `gh api repos/<owner>/<repo> --jq .default_branch` against the
         `origin` remote of <git_root>, when `gh` is available and the
         remote points at github.com.
      2. 'master' as the final fallback when any step fails (no `gh`, no
         `origin`, non-github remote, network error, etc.).
    """
    expanded = os.path.expanduser(git_root)
    try:
        remote = subprocess.run(
            ["git", "-C", expanded, "remote", "get-url", "origin"],
            capture_output=True, text=True, timeout=10,
        )
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return "master"
    if remote.returncode != 0:
        return "master"
    remote_url = remote.stdout.strip()
    m = re.search(r"github\.com[:/]+([^/]+)/([^/\s]+?)(?:\.git)?/?$", remote_url)
    if not m:
        return "master"
    owner, repo = m.group(1), m.group(2)
    try:
        gh = subprocess.run(
            ["gh", "api", f"repos/{owner}/{repo}", "--jq", ".default_branch"],
            capture_output=True, text=True, timeout=15,
        )
    except (FileNotFoundError, subprocess.TimeoutExpired):
        return "master"
    if gh.returncode != 0:
        return "master"
    branch = gh.stdout.strip()
    return branch or "master"


for entry in data:
    if (
        isinstance(entry, dict)
        and entry.get("type") == "create"
        and isinstance(entry.get("name"), str)
        and entry["name"]
    ):
        name = entry["name"]
        suffix = "".join(secrets.choice(string.ascii_lowercase) for _ in range(3))
        entry["name"] = f"{prefix}/{name}-{suffix}" if prefix else f"{name}-{suffix}"

        # Auto-resolve `base_commit` for non-fork creates that didn't
        # specify one, so the downstream consumer never falls back to a
        # hardcoded 'master' on repos whose default branch is `main` (or
        # otherwise). Forks deliberately skip this — downstream uses HEAD.
        if (
            "base_commit" not in entry
            and "fork_from" not in entry
            and isinstance(entry.get("git_root"), str)
            and entry["git_root"]
        ):
            entry["base_commit"] = resolve_default_branch(entry["git_root"])

json.dump(data, sys.stdout, indent=2)
sys.stdout.write("\n")
PY
)" >"$tmp"

mv "$tmp" "$HOME/.claude/output/workspace_commands_$(uuidgen).json"
