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
# write took 163s. bin/suite-slot.sh bounds how many suites run at once; this
# bounds what any one of them may take from the processes the owner is using.
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
set -uo pipefail

log() { printf 'background: %s\n' "$*" >&2; }

# EX_CONFIG: this host cannot run a test at background priority. Distinct from
# a failing suite (1), from suite-slot's usage error (2) and from test-all's
# precondition decline (77), which would read as "skipped" rather than "wrong".
readonly EXIT_REFUSED=78

refuse() {
    log "REFUSING TO RUN: $*"
    log "  tests only ever run at background priority; see bin/background.sh"
    exit "$EXIT_REFUSED"
}

(( $# > 0 )) || refuse "no command given; usage: background.sh <command> [args...]"

os=$(uname -s) || refuse "could not read the platform with uname"

# read_niceness -- this process's niceness. macOS's nice(1) cannot print it
# (it demands a utility), so there it is getpriority(PRIO_PROCESS, self) from
# perl, which ships with macOS; the perl child inherits this process's value.
# Linux's coreutils nice prints it, and the sandbox image may lack perl.
read_niceness() {
    case $os in
        Darwin) perl -e 'print getpriority(0, 0)' ;;
        Linux) nice ;;
    esac
}

case $os in
    Darwin|Linux)
        niceness=$(read_niceness) || refuse "could not read this process's niceness"
        [[ $niceness =~ ^-?[0-9]+$ ]] || refuse "the niceness read back is not a number: '$niceness'"
        export AGENT_REPL_BACKGROUND_PRIORITY=nice-19
        if (( niceness >= 19 )); then
            exec "$@"
        fi
        exec nice -n 19 "$@"
        ;;
    *)
        refuse "no background-priority mechanism is known for platform '$os'"
        ;;
esac
