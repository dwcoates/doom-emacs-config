#!/usr/bin/env bash
# low-priority.sh -- run ONE command at the host's background priority WITHOUT
# marking it a test run. The demotion is bin/lib-priority.sh's, exactly as
# bin/background.sh's.
#
# Only bin/realtest.sh calls it: a realtest drives the owner's live editor, so
# it is no test run the daemon starts, but it must still yield the processor to
# the live runtime. Because it exports no AGENT_REPL_BACKGROUND_PRIORITY, no
# test runner (vitest, ERT, the Go integration harness) starts under it.
#
# Usage:
#   bin/low-priority.sh <command> [args...]
set -uo pipefail

# shellcheck source=lib-priority.sh
. "$(dirname "${BASH_SOURCE[0]}")/lib-priority.sh"

(( $# > 0 )) || priority_refuse "no command given; usage: low-priority.sh <command> [args...]"

priority_exec unmarked "$@"
