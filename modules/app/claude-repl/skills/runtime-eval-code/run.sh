#!/usr/bin/env bash
# runtime-eval-code/run.sh
#
# Verbs:
#   resolve-ws   Print the current workspace name on stdout.
#                Used by SKILL.md Step 0 to obtain the return-address
#                routing key for every later dispatch.
#   dispatch     Read a JSON command array from stdin and hand it to
#                the editor.  No-op on success; non-zero on failure.
#
# Exit codes:
#   0  success
#   1  usage error / unknown verb
#   2  missing prerequisite (rebuild the sandbox image)

set -uo pipefail

die() { echo "ERROR: $*" >&2; exit 2; }

case "${1:-}" in
  resolve-ws)
    # The workspace name is the basename of the current working
    # directory.  This matches the value the editor uses as the
    # return-address routing key for `type: "eval"` commands.
    basename "$PWD"
    ;;
  dispatch)
    if ! command -v uuidgen >/dev/null 2>&1; then
      die "uuidgen is not available. Rebuild the sandbox image by running .claude/install.sh and try again."
    fi
    # Resolve the sibling script via string-only `dirname` rather than a
    # literal `..`: this script's install dir (`~/.claude/skills/<skill>`)
    # is a symlink into a worktree, and a `..` segment is resolved by the
    # kernel THROUGH that symlink, landing in the worktree (no sibling
    # there) and failing with exit 127.  Double `dirname` collapses to the
    # logical `~/.claude/skills` parent without traversing the symlink.
    exec bash "$(dirname "$(dirname "$0")")/emit-workspace-commands.sh"
    ;;
  ""|-h|--help|help)
    cat <<USAGE >&2
Usage: $0 <verb>

Verbs:
  resolve-ws    Print the current workspace name on stdout (exit 0).
  dispatch      Read a JSON command array from stdin, hand it to the editor (exit 0).
USAGE
    exit 1
    ;;
  *)
    echo "ERROR: unknown verb: $1" >&2
    exit 1
    ;;
esac
