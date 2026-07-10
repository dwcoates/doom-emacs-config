#!/usr/bin/env bash
# Reads a JSON workspace commands array from stdin and writes it atomically
# to ${CLAUDE_REPL_STATE_DIR:-$HOME/.claude-emacs}/output/workspace_commands_<uuid>.json.
#
# Repo-local impl: explanation-engine folded the original into its
# `/workspace' super-skill (`workspace/run.sh --emit-commands'), which
# requires python3 the doom-sandbox image does not ship.  The doom
# claude-repl skills (runtime-eval-code, workspace-close) only need this
# trivial uuidgen-only emitter, so it lives here and is installed as a
# repo-local managed skill rather than pulled from explanation-engine.
set -e

if ! command -v uuidgen &>/dev/null; then
  echo "ERROR: uuidgen is not available. Please rebuild the sandbox image by running .claude/install.sh and try again." >&2
  exit 1
fi

mkdir -p ${CLAUDE_REPL_STATE_DIR:-$HOME/.claude-emacs}/output
tmp=$(mktemp ${CLAUDE_REPL_STATE_DIR:-$HOME/.claude-emacs}/output/.workspace_commands_XXXXXX.json)
cat > "$tmp"
mv "$tmp" ${CLAUDE_REPL_STATE_DIR:-$HOME/.claude-emacs}/output/workspace_commands_$(uuidgen).json
