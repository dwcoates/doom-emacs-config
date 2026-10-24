#!/usr/bin/env bash
# test-validate.sh -- the proto suite: lint the contract's sources, check the
# conversation package's isolation, and prove every committed binding is what
# its source generates (proto/Makefile `validate`).

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/../../bin/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

exec make -C "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)" validate
