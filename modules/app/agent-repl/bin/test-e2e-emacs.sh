#!/usr/bin/env bash
#
# test-e2e-emacs.sh — the SANDBOXED Emacs client layer.
#
# The `e2e-emacs` gate suite. It drives a real Emacs, which spawns a real
# agent-repld through the module's own launcher, and asserts on Emacs's own
# buffers, windows, tab-bar, modeline and roster. See e2e/EMACS-LAYER-SPEC.md.
#
# The sandbox, its preflight and its decline (exit 77) are
# bin/lib-sandbox-suite.sh's.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

# shellcheck source=lib-sandbox-suite.sh
. "$(dirname "${BASH_SOURCE[0]}")/lib-sandbox-suite.sh"
# Which tests run is the runner's, never this script's: its one argument is
# the -test.run pattern, the roster's (testrun/roster, e2e-emacs's GoTests)
# for a whole run, or just the tests a run names. The Doom tier (TestDoom*)
# shares this suite's container; the roster says why.
run_sandbox_suite e2e-emacs 'the Emacs client layer and the Doom tier' "$@"
