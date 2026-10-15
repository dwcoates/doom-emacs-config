#!/usr/bin/env bash
#
# test-emacs-doom.sh — the SANDBOXED Doom tier.
#
# The `emacs-doom` gate suite. It boots a real Doom Emacs on a GUI frame
# against the FAKE daemon (lisp/testsupport/fakedaemon) and asserts what only
# a booted Doom shows: `map!` leader bindings, popup placement, persp-mode's
# workspace recycling. No real daemon, shim, store or sidecar runs. See
# e2e/doomworld_test.go.
#
# The sandbox, its preflight and its decline (exit 77) are
# bin/lib-sandbox-suite.sh's, shared with the Emacs client layer.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

# shellcheck source=lib-sandbox-suite.sh
. "$(dirname "${BASH_SOURCE[0]}")/lib-sandbox-suite.sh"
run_sandbox_suite emacs-doom 'TestDoom' 'the Doom tier' "$@"
