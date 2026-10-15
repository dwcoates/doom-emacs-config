#!/usr/bin/env bash
#
# test-e2e-emacs.sh — the SANDBOXED Emacs client layer.
#
# The `e2e-emacs` gate suite. It drives a real Emacs, which spawns a real
# agent-repld through the module's own launcher, and asserts on Emacs's own
# buffers, windows, tab-bar, modeline and roster. See e2e/EMACS-LAYER-SPEC.md.
#
# The sandbox, its preflight and its decline (exit 77) are
# bin/lib-sandbox-suite.sh's, shared with the Doom tier.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

# shellcheck source=lib-sandbox-suite.sh
. "$(dirname "${BASH_SOURCE[0]}")/lib-sandbox-suite.sh"
run_sandbox_suite e2e-emacs 'TestEmacs' 'the Emacs client layer' "$@"
