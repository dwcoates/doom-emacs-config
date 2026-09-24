#!/usr/bin/env bash
# suite-slot.sh -- run a test suite while holding one of this HOST's suite
# slots, so concurrent runs cannot overcommit the machine.
#
# WHY THIS EXISTS, and it is a measurement rather than a precaution.
#
# Every suite in this repo is ALREADY internally parallel: vitest defaults to
# one worker per CPU (16 on the machine this was written on, measured at a
# 116 MiB mean and a 333 MiB peak per worker, so 2-5 GiB for ONE `npm test`),
# `go test` defaults to GOMAXPROCS-way parallelism, and the Go e2e suite runs
# `-parallel 8` with each test booting a real daemon/shim/store/sidecar
# quartet. A single run is therefore sized to fill the box on purpose.
#
# What broke: several agents each ran their own suites at once, each assuming
# it owned the machine. Agent count MULTIPLIES against per-suite parallelism,
# and the box reached a load average of 253 with four concurrent vitest runs
# holding 8-21 GiB of node between them. The Emacs layer then failed 37 of 45
# scenarios on Doom boot -- not a product regression, just a machine with
# nothing left to give, which is a whole test run's evidence thrown away.
#
# What does NOT work, and was tried: telling each runner to wait for the load
# average to fall. Load average is a lagging indicator (it is a decaying mean,
# so it keeps rising for a minute after the work stops), and every waiter
# observes the same number and starts at the same moment -- a thundering herd
# that recreates the overload it was meant to prevent.
#
# So the gate is a COUNT of what is actually running, claimed atomically,
# exactly like the sandbox's container gate in e2e/sandbox/bin/e2e-sandbox.sh
# (read that file's slot section; this is deliberately the same mechanism).
# Slots are directories claimed with `mkdir`, the one filesystem operation
# that is atomic and fails if the name exists, so two runs cannot both believe
# they hold the same slot. A slot whose holder died is reclaimed by reading
# its recorded pid and moving the directory aside, so a slot only ever becomes
# claimable by ceasing to exist, never by being emptied under a live holder.
#
# NESTING IS RE-ENTRANT, AND IT HAS TO BE.
#
# A slot is held by a PROCESS TREE, not by one command, so a wrapped command
# that wraps its own children hits the gate it is already behind. That is not
# theoretical: `bin/suite-slot.sh bin/e2e-repeat.sh ...` hung for 6000s of
# "still waiting", because e2e-repeat takes a slot per run and every one of them
# queued behind the outer holder that was waiting for them to finish. A deadlock
# with a polite progress message is still a deadlock.
#
# So an acquired slot is exported as AGENT_REPL_SUITE_SLOT_HELD, and any nested
# invocation that sees it runs its command DIRECTLY, with one line saying so.
# The gate's promise is unchanged: one suite per slot at a time on this host.
# The nested run is not a second suite, it is the same one, already counted.
#
# Usage:
#   bin/suite-slot.sh npm test
#   bin/suite-slot.sh go test ./... -count=1
#   AGENT_REPL_SUITE_SLOTS=1 bin/suite-slot.sh <cmd>   # serialize everything
#
# The command runs in the CURRENT directory; this wraps it, never relocates it.
#
# The command, and this gate, run at background priority: every
# `bin/suite-slot.sh <cmd>` is a test run, so it re-execs once through
# bin/background.sh like every other test entry point.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -uo pipefail

log() { printf 'suite-slot: %s\n' "$*" >&2; }

# A fixed host path, NOT under $TMPDIR: on macOS $TMPDIR is per-process, which
# would give every caller a private set of slots and therefore no gate at all.
SLOT_DIR=${AGENT_REPL_SUITE_SLOT_DIR:-/tmp/agent-repl-suite.slots}

# HOW MANY SUITES MAY RUN AT ONCE.
#
# One, by default, and the default is the whole point. A suite is sized to use
# the machine; two of them do not go twice as fast, they go slower and one of
# them reports a bound as missed that a quiet box meets. Raise it only for
# suites you have measured as small, and then say so at the call site.
SUITE_SLOTS=${AGENT_REPL_SUITE_SLOTS:-1}

SUITE_SLOT=""
# shellcheck disable=SC2329  # invoked indirectly, from the EXIT/INT/TERM trap
release_slot() {
  [[ -n ${SUITE_SLOT:-} ]] || return 0
  rm -rf "$SUITE_SLOT"
  SUITE_SLOT=""
}

reap_dead_slots() {
  local slot pid
  for slot in "$SLOT_DIR"/slot-*; do
    [[ -d $slot ]] || continue
    pid=$(cat "$slot/pid" 2>/dev/null || echo "")
    # A slot mid-claim (created, pid not yet written) is left alone: the
    # claimer writes the pid immediately, and reclaiming it here would race a
    # live run.
    [[ -n $pid ]] || continue
    kill -0 "$pid" 2>/dev/null && continue
    log "reclaiming slot $(basename "$slot") from dead pid $pid"
    mv "$slot" "$slot.dead.$$" 2>/dev/null && rm -rf "$slot.dead.$$"
  done
}

acquire_slot() {
  # ALREADY INSIDE A HELD SLOT. Waiting here would be waiting on this very
  # process tree, which is the deadlock the header records. Run through.
  if [[ -n ${AGENT_REPL_SUITE_SLOT_HELD:-} ]]; then
    log "already holding $AGENT_REPL_SUITE_SLOT_HELD (nested invocation): running without acquiring a second slot"
    return 0
  fi
  if [[ ${AGENT_REPL_SUITE_NO_GATE:-0} == 1 ]]; then
    log "AGENT_REPL_SUITE_NO_GATE=1: running WITHOUT the host gate; concurrent suites will contend for every CPU"
    return 0
  fi
  mkdir -p "$SLOT_DIR"
  local i waited=0 announced=0 holder
  while true; do
    reap_dead_slots
    for (( i = 1; i <= SUITE_SLOTS; i++ )); do
      if mkdir "$SLOT_DIR/slot-$i" 2>/dev/null; then
        SUITE_SLOT="$SLOT_DIR/slot-$i"
        printf '%s\n' "$$" > "$SUITE_SLOT/pid"
        printf '%s\n' "$*" > "$SUITE_SLOT/cmd"
        # The marker every descendant reads. It is exported, not written to the
        # slot: what matters to a nested invocation is whether IT is inside the
        # holder, which is exactly what an inherited environment answers.
        export AGENT_REPL_SUITE_SLOT_HELD="slot-$i"
        trap release_slot EXIT INT TERM
        (( waited > 0 )) && log "slot $i acquired after ${waited}s"
        return 0
      fi
    done
    if (( announced == 0 )); then
      holder=$(cat "$SLOT_DIR"/slot-*/cmd 2>/dev/null | head -1)
      log "WAITING: all $SUITE_SLOTS suite slot(s) are in use${holder:+ (running: $holder)}."
      log "  This is the host concurrency gate, not a hang: $SLOT_DIR holds a directory per running suite."
      log "  A suite already fills this box; running two makes both slower and makes timing bounds lie."
      log "  Set AGENT_REPL_SUITE_NO_GATE=1 to override deliberately."
      announced=1
    elif (( waited % 60 == 0 )); then
      log "still waiting for a suite slot (${waited}s)"
    fi
    sleep 2
    waited=$(( waited + 2 ))
  done
}

(( $# > 0 )) || { log "usage: suite-slot.sh <command> [args...]"; exit 2; }
acquire_slot "$*"
"$@"
# The command's exit status is this script's: a gate must never turn a red
# suite green (or the reverse) on its way past.
exit $?
