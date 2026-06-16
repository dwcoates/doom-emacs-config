#!/usr/bin/env bash
# run.sh — workspace-generation dispatch + read-only main-branch resolver.
#
# Verbs:
#   (no args)                    DISPATCH mode (fire-and-forget). Reads a JSON
#                                workspace-commands array from stdin, validates
#                                the source_ws invariant on every create entry,
#                                decorates each create entry's "name" field,
#                                auto-resolves base_commit for non-fork creates
#                                lacking one, then writes the result atomically
#                                to ~/.claude/output/workspace_commands_<uuid>.json.
#                                NEVER re-invoke: each call mints a fresh suffix.
#
#   --resolve-master [git_root]  READ-ONLY resolve mode. Prints the resolved
#                                main/default branch name (e.g. "master" or
#                                "main") for git_root to stdout, then exits.
#                                git_root defaults to the current directory and
#                                a leading "~" is expanded. Creates NO dispatch
#                                file and mutates nothing — safe to call freely.
#                                Resolution order:
#                                  1. local `origin/HEAD` symref (no network).
#                                  2. `git ls-remote --symref origin HEAD`.
#                                Exits 3 if neither yields a branch.
#
# Exit codes:
#   0  success (dispatch written, or branch name printed)
#   1  unknown verb (usage error)
#   2  dispatch input/validation error, or missing tooling
#   3  --resolve-master could not determine the main branch
#
# Source-ws invariant (enforced before any write, DISPATCH mode only):
#   Every `create` entry MUST carry a `source_ws` object of shape
#     {"name": "<non-empty-str>", "path": "<non-empty-str>"}
#   Dispatches missing either component are rejected with a clear
#   stderr diagnostic identifying the offending entry, and nothing is
#   written to ~/.claude/output/. Prompt-only entries (type=="prompt")
#   are exempt — they target existing workspaces and do not need a
#   source workspace.
#
# When CLAUDE_WORKSPACE_PREFIX is set in the environment, it is prepended
# to each create entry's name as "PREFIX/name-<suffix>". When unset or
# empty, no prefix is added and the name is emitted as "name-<suffix>".
# There is no derivation or fallback — absent env var means no prefix.
#
# When CLAUDE_WORKSPACE_ONE_SHOT is set to a non-empty value (the
# skill exports it when invoked with `--one-shot`), every `create`
# entry's "prompt" is augmented with a trailing instruction telling the
# spawned workspace to open and merge its PR when the work is done. If
# the entry already carries a prompt, the instruction is appended after
# a blank-line separator; if it has no prompt, a prompt carrying just
# the instruction is created. Prompt-only entries (type=="prompt") and
# the no-one-shot case are left untouched.
set -euo pipefail

# --- Read-only resolve mode: must run BEFORE any stdin read, mktemp, or write,
#     so a resolve call never touches ~/.claude/output/ and never consumes the
#     dispatch payload. ---
case "${1:-}" in
  --resolve-master)
    git_root="${2:-.}"
    git_root="${git_root/#\~/$HOME}"
    if [[ ! -d "$git_root" ]]; then
      echo "ERROR: --resolve-master: git_root '$git_root' is not a directory" >&2
      exit 3
    fi
    # 1. Local origin/HEAD symref (fast, no network). Strip the "origin/" prefix.
    branch=$(git -C "$git_root" symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null | sed 's#^origin/##') || true
    if [[ -z "$branch" ]]; then
      # 2. Authoritative remote query.
      branch=$(git -C "$git_root" ls-remote --symref origin HEAD 2>/dev/null \
        | sed -n 's#^ref: refs/heads/\([^[:space:]]*\)[[:space:]].*#\1#p' | head -n1) || true
    fi
    if [[ -z "$branch" ]]; then
      echo "ERROR: --resolve-master: could not resolve the main branch for '$git_root'." >&2
      echo "       No local origin/HEAD symref and 'git ls-remote --symref origin HEAD' returned nothing." >&2
      echo "       Set the local symref with 'git -C $git_root remote set-head origin -a', or verify the 'origin' remote exists and is reachable." >&2
      exit 3
    fi
    printf '%s\n' "$branch"
    exit 0
    ;;
  "")
    : # No verb → DISPATCH mode (handled below).
    ;;
  *)
    echo "ERROR: unknown verb '$1' (expected no args for dispatch, or '--resolve-master [git_root]')" >&2
    exit 1
    ;;
esac

if ! command -v uuidgen &>/dev/null; then
  echo "ERROR: uuidgen is not available. Please rebuild the sandbox image by running .claude/install.sh and try again." >&2
  exit 2
fi

if ! command -v python3 &>/dev/null; then
  echo "ERROR: python3 is not available. Please rebuild the sandbox image by running .claude/install.sh and try again." >&2
  exit 2
fi

WS_PREFIX="${CLAUDE_WORKSPACE_PREFIX:-}"
if [[ -z "$WS_PREFIX" ]]; then
  echo "NOTICE: CLAUDE_WORKSPACE_PREFIX is unset — emitting workspace name(s) with no branch prefix. Export CLAUDE_WORKSPACE_PREFIX=<your-prefix> in the calling environment if you want one prepended (e.g. 'JB/<slug>')." >&2
fi

WS_ONE_SHOT="${CLAUDE_WORKSPACE_ONE_SHOT:-}"

mkdir -p ~/.claude/output
# BSD mktemp on macOS only substitutes X's at the END of the template, so
# don't put an extension after them — pick up the .json on the final mv.
tmp=$(mktemp ~/.claude/output/.workspace_commands_XXXXXX)
# Clean up the staging file on any error so failed validations don't
# leave half-written turds in ~/.claude/output/.
trap 'rm -f "$tmp"' EXIT

# Pass the python program via -c so python3's stdin stays connected to
# run.sh's stdin (a `python3 - <<'PY'` heredoc would shadow it).
WS_PREFIX="$WS_PREFIX" WS_ONE_SHOT="$WS_ONE_SHOT" python3 -c "$(cat <<'PY'
import json
import os
import re
import secrets
import string
import subprocess
import sys

prefix = os.environ.get("WS_PREFIX", "")
one_shot = bool(os.environ.get("WS_ONE_SHOT", "").strip())

ONE_SHOT_INSTRUCTION = (
    "When finished with the work, run /create-or-update-pr --self-certified "
    "--rebase --add-to-merge-queue --patch to open the PR and merge it."
)

try:
    data = json.load(sys.stdin)
except json.JSONDecodeError as e:
    print(f"ERROR: stdin is not valid JSON: {e}", file=sys.stderr)
    sys.exit(2)

if not isinstance(data, list):
    print("ERROR: workspace commands payload must be a JSON array", file=sys.stderr)
    sys.exit(2)


# Enforce the source_ws invariant before any mutation or write. Every
# `create` entry must carry source_ws={"name": <non-empty-str>,
# "path": <non-empty-str>}. Prompt-only entries are exempt.
validation_errors = []
for idx, entry in enumerate(data):
    if not isinstance(entry, dict):
        continue
    if entry.get("type") != "create":
        continue
    label = f"entry[{idx}] (create, name={entry.get('name')!r})"
    sw = entry.get("source_ws")
    if not isinstance(sw, dict):
        validation_errors.append(
            f"  {label}: missing or non-object `source_ws` field"
        )
        continue
    name = sw.get("name")
    path = sw.get("path")
    if not isinstance(name, str) or not name.strip():
        validation_errors.append(
            f"  {label}: `source_ws.name` is missing or empty"
        )
    if not isinstance(path, str) or not path.strip():
        validation_errors.append(
            f"  {label}: `source_ws.path` is missing or empty"
        )

if validation_errors:
    print(
        "ERROR: source_ws invariant violated on one or more create entries:",
        file=sys.stderr,
    )
    for line in validation_errors:
        print(line, file=sys.stderr)
    print("", file=sys.stderr)
    print(
        'Every `create` entry MUST carry source_ws={"name": <non-empty-str>, '
        '"path": <non-empty-str>}. Resolve via Step 1 of SKILL.md (explicit '
        '[source-ws:<name> path:<dir>] tag or current-workspace fallback via '
        '`git rev-parse --abbrev-ref HEAD` + `git rev-parse --show-toplevel`).',
        file=sys.stderr,
    )
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

        # When --one-shot was requested, append the open-and-merge
        # instruction to this create entry's prompt. Compose with an
        # existing inline prompt via a blank-line separator, or create a
        # prompt carrying just the instruction when none was supplied.
        if one_shot:
            existing = entry.get("prompt")
            if isinstance(existing, str) and existing.strip():
                entry["prompt"] = existing.rstrip() + "\n\n" + ONE_SHOT_INSTRUCTION
            else:
                entry["prompt"] = ONE_SHOT_INSTRUCTION

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
