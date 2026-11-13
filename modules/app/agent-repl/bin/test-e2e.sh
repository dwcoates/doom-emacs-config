#!/usr/bin/env bash
#
# test-e2e.sh — the cross-system e2e suite, WITHOUT a container.
#
# This is the `e2e` gate suite: the Go tests in modules/app/agent-repl/e2e
# that dial the daemon's Connect API directly. They run a real agent-repld,
# a real shim, a real shim-store and a real shim-claude-sidecar against the
# FAKE SDK and the SCRIPTED FAKE GIT, so they need no container and no
# network, and they belong in the gate like every other suite.
#
# Every test in this directory runs here: the Emacs e2e layer, which ran in a
# container, was removed on 2026-11-13 (e2e/AGENTS.md).

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
E2E_DIR="$(cd "$THIS_DIR/../e2e" && pwd)"

command -v go >/dev/null 2>&1 || {
    printf '[e2e] ERROR: go is not on PATH; the cross-system suite cannot build its binaries\n' >&2
    exit 1
}

# The suite bundles the real shim and drives the real webapp, so their npm
# deps are provisioned here rather than left to whichever suite ran before.
"$THIS_DIR/ensure-e2e-deps.sh"

printf '[e2e] running the cross-system suite in %s\n' "$E2E_DIR"
cd "$E2E_DIR"
exec go test ./... "$@"
