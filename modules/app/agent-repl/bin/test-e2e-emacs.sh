#!/usr/bin/env bash
#
# test-e2e-emacs.sh — the SANDBOXED Emacs client layer.
#
# The `e2e-emacs` gate suite. It drives a real Emacs, which spawns a real
# claude-repld through the module's own launcher, and asserts on Emacs's own
# buffers, windows, tab-bar, modeline and roster. See e2e/EMACS-LAYER-SPEC.md.
#
# THE CONTAINER IS A PRECONDITION, NOT A FAILURE. The layer must never run
# unsandboxed — it starts an editor that would otherwise reach the developer's
# own ~/.claude, ~/.emacs.d and ~/.config — so when the sandbox is not usable
# this script DECLINES:
#
#   exit 0   the suite ran and passed
#   exit 77  the suite DECLINED: the sandbox is unavailable, and the
#            preflight's own message says why (bin/test-all.sh reports 77 as
#            a skip, never as a pass and never as a failure)
#   other    the suite ran and failed
#
# 77 is the autotools "skipped" convention, and it is used rather than 0 so
# that a gate run can never show a green suite that did not execute.
set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_ROOT="$(cd "$THIS_DIR/.." && pwd)"
SANDBOX="$MODULE_ROOT/e2e/sandbox/bin/e2e-sandbox.sh"

readonly EXIT_DECLINED=77

if [ ! -x "$SANDBOX" ]; then
    printf '[e2e-emacs] SKIPPED: the sandbox runner is missing or not executable: %s\n' "$SANDBOX" >&2
    exit "$EXIT_DECLINED"
fi

# The preflight's output is the actionable part (start Docker, build the
# image), so it is reproduced VERBATIM rather than paraphrased — the same
# rule the harness itself follows for its Go-side skip.
if ! preflight_out="$("$SANDBOX" preflight 2>&1)"; then
    printf '[e2e-emacs] SKIPPED: the e2e sandbox is not usable, and this suite never runs unsandboxed.\n' >&2
    printf '%s\n' "$preflight_out" >&2
    exit "$EXIT_DECLINED"
fi
printf '%s\n' "$preflight_out"

printf '[e2e-emacs] running the Emacs client layer inside the sandbox\n'
exec "$SANDBOX" run go test ./e2e/ -run 'TestEmacs' -v "$@"
