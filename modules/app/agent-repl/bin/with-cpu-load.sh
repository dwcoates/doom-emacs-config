#!/usr/bin/env bash
# with-cpu-load.sh -- run ONE command while N busy loops hold the CPU, and
# guarantee the loops die with this script, however it ends.
#
# usage: bin/with-cpu-load.sh <nloops> <command> [args...]
#
# THE OWNER DOES NOT WANT THE MACHINE UNDER LOAD (2026-09-30). Reproducing a
# flake under CPU load is done only when the owner asks for it, and then only
# through this script: a hand-written `( while :; do :; done ) &` is how load
# outlived its author twice in one evening. zsh does not word-split
# `kill $PIDS`, so the kill killed nothing and a `wait` hung on the loops; and
# a `trap` cleanup never runs when its shell is killed outright.
#
# WHY THE LOOPS CANNOT OUTLIVE IT. Every loop polls this script's pid
# (`kill -0`) on each pass and exits the moment it is gone, whether the script
# exited, was interrupted, or was SIGKILLed. Nothing has to run for them to
# stop, so no ending of this script leaves one spinning. The trap below only
# makes the common ending immediate.
#
# THE COMMAND RUNS IN THE BACKGROUND AND IS WAITED ON, so a signal to this
# script is handled at once (bash defers a trap while a FOREGROUND child runs,
# which kept a 2000-iteration test spinning under a TERM). The signal is passed
# on to the command, and the script exits with the command's status.
#
# It runs at background priority like every test entry point.

[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -euo pipefail

if [[ $# -lt 2 || ! $1 =~ ^[1-9][0-9]*$ ]]; then
  echo "usage: with-cpu-load.sh <nloops> <command> [args...]" >&2
  exit 2
fi
nloops=$1
shift

owner=$$
loops=()
for ((i = 0; i < nloops; i++)); do
  (while kill -0 "$owner" 2>/dev/null; do :; done) &
  loops+=("$!")
done

command_pid=""
stop() {
  if [[ -n $command_pid ]]; then
    # THE COMMAND'S WHOLE GROUP: a test runner's own children (a compiled
    # test binary, a sleep) would otherwise outlive the runner it was told to
    # stop.
    kill -TERM -- "-$command_pid" 2>/dev/null || true
  fi
  kill "${loops[@]}" 2>/dev/null || true
}
trap stop EXIT
trap 'stop; exit 143' TERM
trap 'stop; exit 130' INT

# The command leads a process group of its own (job control), so stop()
# reaches everything it started.
set -m
"$@" &
command_pid=$!
set +m
set +e
wait "$command_pid"
status=$?
set -e
command_pid=""
exit "$status"
