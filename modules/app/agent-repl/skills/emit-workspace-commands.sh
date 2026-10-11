#!/usr/bin/env bash
# Reads a JSON workspace commands array from stdin and writes it atomically
# to ${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}/output/workspace_commands_<uuid>.json.
#
# Repo-local impl: explanation-engine folded the original into its
# `/create-or-update-workspace' skill (`run.sh --emit-commands'), which
# drags in a python3 dependency.  The doom agent-repl skill
# (runtime-eval-code) only needs this trivial
# uuidgen-only emitter, so it lives here and is installed as a
# repo-local managed skill rather than pulled from explanation-engine.
set -e

# HARD failure, never a fallback: the uuid is what keeps concurrent
# emitters from colliding on one output filename, so substituting any
# fixed placeholder would silently make two jobs overwrite each other.
if ! command -v uuidgen &>/dev/null; then
  echo "ERROR: required binary 'uuidgen' is not on PATH; install it and retry." >&2
  exit 1
fi

out_dir="${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}/output"
mkdir -p "$out_dir"
tmp="$(mktemp "$out_dir/.workspace_commands_XXXXXX.json")"
cat > "$tmp"
mv "$tmp" "$out_dir/workspace_commands_$(uuidgen).json"
