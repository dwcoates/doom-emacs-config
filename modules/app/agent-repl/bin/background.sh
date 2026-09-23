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
#   Darwin  `taskpolicy -b` (setpriority PRIO_DARWIN_BG): CPU scheduling AND
#           disk I/O are throttled, and on Apple Silicon the work is kept to
#           the efficiency cores. getpriority(PRIO_DARWIN_PROCESS) reads 1.
#   Linux   `nice -n 19`. The only non-macOS place a suite runs is the e2e
#           sandbox container (e2e/sandbox/bin/entrypoint.sh routes every
#           `run` command through here). CPU priority only: a container's
#           disk is its own tmpfs, and the host-side cost is the Docker VM's.
#   other   REFUSED, exit 78. No suite here has ever run anywhere else, and
#           a run at normal priority is exactly what this exists to prevent.
#
# Children inherit the policy, so wrapping the ROOT of a run is enough: every
# go test binary, daemon, shim, vitest worker and Emacs a suite starts is
# background too. `taskpolicy` and `nice` both EXEC the command, so the pid a
# caller holds (make, npm, a Go harness killing a process group) is the
# command's own.
#
# IT IS IDEMPOTENT. A process that is already at background priority -- an
# entry point called from another entry point, `make test` under
# bin/test-all.sh, `npm run test:webapp-layer` under the Go e2e suite -- runs
# its command straight through. What decides that is the process's ACTUAL
# priority, read with getpriority/`nice`, never a variable it inherited.
#
# THE MARKER. Before it runs the command, this exports
# AGENT_REPL_BACKGROUND_PRIORITY=<mechanism>. The runners that cannot be
# wrapped from outside -- `emacs -batch` loading lisp/test-helpers.el, a vitest
# config, the Go integration harness -- refuse to start without it, so a raw
# `go test`/`npx vitest`/`emacs -batch` fails loudly instead of running at
# normal priority. Only this script sets it.
#
# THE LIVE RUNTIME IS NEVER ROUTED THROUGH HERE. The daemon, shim, store and
# sidecar, and the build steps bin/deploy-all.sh and bin/build-frontend.sh
# run, stay at normal priority. Only tests are demoted.
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

case $os in
    Darwin)
        # getpriority(PRIO_DARWIN_PROCESS = 4, self) answers 1 under
        # PRIO_DARWIN_BG and 0 otherwise, and it is inherited, so the perl
        # child reads this process's own state. NOT `ps -o pri`: that is the
        # live scheduling priority, which decays under load (a background
        # process was measured reading 3, and a busy normal one can sink as
        # low). perl ships with macOS; its getpriority is the plain syscall.
        darwin_bg=$(perl -e 'print getpriority(4, 0)') ||
            refuse "could not read this process's background state with perl"
        export AGENT_REPL_BACKGROUND_PRIORITY=darwin-bg
        case $darwin_bg in
            1) exec "$@" ;;
            0) ;;
            *) refuse "getpriority(PRIO_DARWIN_PROCESS) answered '$darwin_bg', which is neither 0 nor 1" ;;
        esac
        # An absolute path, not a PATH lookup: taskpolicy lives in /usr/sbin,
        # which the stub-PATH harnesses here (and many launch contexts) leave
        # out. AGENT_REPL_TASKPOLICY is the test seam, as AGENT_REPL_LAUNCHCTL
        # is for bin/store-reset.sh.
        taskpolicy=${AGENT_REPL_TASKPOLICY:-/usr/sbin/taskpolicy}
        [[ -x $taskpolicy ]] ||
            refuse "$taskpolicy is missing or not executable, so this macOS host cannot demote the run"
        exec "$taskpolicy" -b "$@"
        ;;
    Linux)
        niceness=$(nice) || refuse "could not read this process's niceness with nice"
        [[ $niceness =~ ^-?[0-9]+$ ]] || refuse "nice reported a niceness that is not a number: '$niceness'"
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
