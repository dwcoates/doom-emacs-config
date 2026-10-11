#!/bin/bash
#
# install.sh — set this machine up for agent-repl.
#
# THE INSTALLER IS modules/app/agent-repl/bin/requirements.sh, driven by the
# requirement manifests (modules/app/agent-repl/requirements.json and the
# per-system manifests it includes). This script is only the repository's
# well-known entry point to it, so nothing here decides anything.
#
# Usage:
#   bash .claude/install.sh [install|uninstall|reinstall|purge-legacy-hooks|check|help]
#
#   install    (default) install every requirement that is not met: programs,
#              the Claude Code skill links, the git pre-commit hook, and the
#              launchd store and sidecar. Idempotent.
#   uninstall  remove what install put in place (links, the managed hook, the
#              services) and this module's legacy settings.json hook entries.
#              Shared programs are never removed.
#   reinstall  uninstall, then install.
#   check      report every requirement's status; changes nothing.
#   purge-legacy-hooks
#              remove ONLY this module's pre-cutover Claude Code hook entries.

set -euo pipefail

REQUIREMENTS="$(cd "$(dirname "${BASH_SOURCE[0]}")/../modules/app/agent-repl/bin" && pwd -P)/requirements.sh"

case "${1:-install}" in
  install|uninstall|check|purge-legacy-hooks)
    exec "$REQUIREMENTS" "${1:-install}" ;;
  reinstall)
    "$REQUIREMENTS" uninstall
    exec "$REQUIREMENTS" install ;;
  -h|--help|help)
    sed -n '9,21p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//' ;;
  *)
    printf '[install.sh] ERROR: unknown action %s\n' "$1" >&2
    sed -n '9,21p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//' >&2
    exit 2 ;;
esac
