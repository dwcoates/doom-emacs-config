#!/usr/bin/env bash
# background.sh -- run a TEST command, and its whole process tree, at this
# host's background priority, so test load can never starve the live runtime.
#
# WHY THIS EXISTS, and it is a measurement rather than a precaution.
#
# Several agents ran suites at once -- Go integration at `-test.parallel=8`
# booting a daemon per test, and vitest pools of about ten workers each -- and
# the machine reached a load average of 281. The OWNER'S live runtime paid for
# it: the shim fell four minutes behind reading Claude's output, and one store
# write took 163s. This bounds what any test may take from the processes the
# owner is using. How many cores overlapping runs hold together is bounded
# by testrun's host core budget (testrun/internal/cores, owner ruling
# 2026-10-20), which ranks runs by the niceness this sets.
# Owner ruling: tests run at background priority, for sure.
#
# WHAT IT DOES, per platform. There is no silent path: every platform either
# demotes the command or refuses to run it.
#
#   Darwin  `nice -n 19`: the lowest CPU priority. Owner ruling 2026-09-23,
#   Linux   "very low" priority. The run keeps the performance cores and
#           runs at full speed on an idle host, but it always yields the CPU
#           to the live runtime. `taskpolicy -b` was measured and rejected:
#           it held a run to the efficiency cores and throttled its I/O, and
#           the webapp integration suite then passed 56-58 of 1765, its
#           files failing their 1800ms boot-hook bound. On Linux the only
#           place a suite runs is the e2e sandbox container
#           (e2e/sandbox/bin/entrypoint.sh routes every `run` command here).
#           Disk I/O is NOT demoted on either platform.
#   other   REFUSED, exit 78. No suite here has ever run anywhere else, and
#           a run at normal priority is exactly what this exists to prevent.
#
# Children inherit the niceness, so wrapping the ROOT of a run is enough:
# every go test binary, daemon, shim, vitest worker and Emacs a suite starts
# is demoted too. `nice` EXECs the command, so the pid a caller holds (make,
# npm, a Go harness killing a process group) is the command's own.
#
# IT IS IDEMPOTENT. A process that is already at niceness 19 or above -- an
# entry point called from another entry point, `make test` under
# bin/test-all.sh, `npm run test:webapp-layer` under the Go e2e suite -- runs
# its command straight through (on macOS a second `nice -n 19` would push it
# to 20). What decides that is the process's ACTUAL niceness, read with
# getpriority, never a variable it inherited.
#
# THE MARKER. Before it runs the command, this exports
# AGENT_REPL_BACKGROUND_PRIORITY=<mechanism>. The runners that cannot be
# wrapped from outside -- `emacs -batch` loading lisp/test-helpers.el, a vitest
# config, the Go integration harness -- refuse to start without it, so a raw
# `go test`/`npx vitest`/`emacs -batch` fails loudly instead of running at
# normal priority. Only this script sets it.
#
# ONLY THE DAEMON STARTS A TEST RUN (owner ruling, 2026-10-24). A run is
# refused unless AGENT_REPL_TEST_RUN_BY_DAEMON=1, which the daemon sets on
# every test command it starts (daemon/internal/merge.TestCommandFor: the
# merge queue's gates, agentrepl.v1.RunTests, RunRedundancy) and every process
# of the run inherits. A test run by hand is refused here, before anything is
# demoted; the marker is what the runners that cannot be wrapped wait for, so
# they refuse too. Agents run tests with `agent-repld test run`.
#
# THE LIVE RUNTIME IS NEVER ROUTED THROUGH HERE. The daemon, shim, store and
# sidecar, and the bin/build-frontend.sh steps the daemon's deploy runs, stay
# at normal priority. Only tests are demoted.
#
# Usage:
#   bin/background.sh <command> [args...]
#
# The self-wrapping prologue a bash test entry point carries as its first
# executable line (bin/test-background.sh's source scan requires it):
#
#   [[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "<dir>/background.sh" bash "${BASH_SOURCE[0]}" "$@"
#
# The command runs in the CURRENT directory; this wraps it, never relocates it.
#
# The demotion itself is bin/lib-priority.sh's, shared with bin/low-priority.sh
# (the live-editor realtests, which are no test run and carry no marker).
set -uo pipefail

# shellcheck source=lib-priority.sh
. "$(dirname "${BASH_SOURCE[0]}")/lib-priority.sh"

(( $# > 0 )) || priority_refuse "no command given; usage: background.sh <command> [args...]"

if [[ ${AGENT_REPL_TEST_RUN_BY_DAEMON:-} != 1 ]]; then
    priority_log "REFUSING TO RUN: $*"
    priority_log "  no test runs by hand: the daemon starts every test run (agent-repld test run; the merge queue runs the full suite)"
    exit "$PRIORITY_EXIT_REFUSED"
fi

# A TEST'S RUNTIMES PROFILE INTO THE TEST'S OWN TREE. Every daemon, store,
# sidecar and shim arms runtime profiling at boot and writes an armed marker
# under the profiles root (proto/vocab/profile-session.json); left at the
# default, every test-spawned runtime would mark itself in the operator's
# ~/.cache/agent-repl/profiles, where bin/profile-agent-repl.sh would find and
# signal it. An explicit setting is kept.
export AGENT_REPL_PROFILE_ROOT="${AGENT_REPL_PROFILE_ROOT:-${TMPDIR:-/tmp}/agent-repl-test-profiles}"

priority_exec marker "$@"
