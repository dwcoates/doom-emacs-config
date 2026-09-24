#!/usr/bin/env bash
#
# test-e2e.sh — the cross-system e2e suite, WITHOUT a container.
#
# This is the `e2e` gate suite: the Go tests in modules/app/agent-repl/e2e
# that dial the daemon's Connect API directly. They run a real claude-repld,
# a real shim, a real shim-store and a real shim-claude-sidecar against the
# FAKE SDK and the SCRIPTED FAKE GIT, so they need no container and no
# network, and they belong in the gate like every other suite.
#
# The sandboxed Emacs client layer is a SEPARATE suite (test-e2e-emacs.sh),
# because it needs a built container image that this machine may not have.
# Its one test skips itself here, loudly, which is why running everything in
# this directory is safe.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
E2E_DIR="$(cd "$THIS_DIR/../e2e" && pwd)"

command -v go >/dev/null 2>&1 || {
    printf '[e2e] ERROR: go is not on PATH; the cross-system suite cannot build its binaries\n' >&2
    exit 1
}

printf '[e2e] running the cross-system suite in %s\n' "$E2E_DIR"
cd "$E2E_DIR"
exec go test ./... "$@"
