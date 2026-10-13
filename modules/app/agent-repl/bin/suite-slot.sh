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
# WAITERS ARE SERVED IN ORDER, AND THE MERGE QUEUE GOES FIRST.
#
# The gate used to let every waiter retry `mkdir` every two seconds, so a freed
# slot went to whichever poller woke first. That is not a queue, and a waiter
# can lose that race indefinitely: on 2026-10-11 the merge queue's test gate
# for one merge sat behind six ad-hoc runs from one agent session, one of them
# waiting 58 minutes, with nothing ordering who went next. Master does not
# move while the merge queue waits, so every merge stalls behind it.
#
# So each waiter takes a TICKET, a file in $SLOT_DIR.queue named
# <priority>-<epoch seconds>-<pid>, and may claim a free slot only when fewer
# live tickets sort before its own than there are free slots. Names sort
# lexically, so priority 0 (AGENT_REPL_SUITE_PRIORITY=merge, which the merge
# queue's gate sets) is served before priority 1 (every other run), and within
# a priority the oldest ticket goes first. A ticket whose pid is dead is reaped
# exactly as a dead holder's slot is, so a killed waiter never blocks the line.
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
# Two, by default (owner ruling, 2026-10-12; it was one). A suite is sized to
# use the machine, so this stays small: it caps the agents' own ad-hoc runs,
# which pile up without it (the load-253 day above). The merge queue's gates
# are not counted here at all; see the exemption below.
SUITE_SLOTS=${AGENT_REPL_SUITE_SLOTS:-2}

# THE MERGE QUEUE'S GATES TAKE NO SLOT (owner ruling, 2026-10-12). The two
# merges at the front of the queue race to land, each running its gate at
# once, and the merge queue's own governor is what admits them; a host slot on
# top serialized the two racers and stood the queue's head behind the merge
# behind it. A gate runs straight through, saying so. Any other priority is a
# caller's mistake, refused rather than guessed at.
# A SIBLING of the slot directory, not inside it, so the slot directory holds
# only slots and a count of its entries stays a count of running suites.
QUEUE_DIR="$SLOT_DIR.queue"
TICKET_PRIORITY=1
case ${AGENT_REPL_SUITE_PRIORITY:-} in
  merge) MERGE_GATE=1 ;;
  "") MERGE_GATE=0 ;;
  *) log "AGENT_REPL_SUITE_PRIORITY=${AGENT_REPL_SUITE_PRIORITY} is not a known priority (the only one is 'merge')"; exit 2 ;;
esac

SUITE_SLOT=""
TICKET=""
# shellcheck disable=SC2329  # invoked indirectly, from the EXIT trap
release_slot() {
  if [[ -n ${TICKET:-} ]]; then
    rm -f "$TICKET"
    TICKET=""
  fi
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

# A ticket names its own pid, so it is never read half-written.
reap_dead_tickets() {
  local ticket pid
  for ticket in "$QUEUE_DIR"/*; do
    [[ -f $ticket ]] || continue
    pid=$(( 10#${ticket##*-} ))
    kill -0 "$pid" 2>/dev/null && continue
    log "dropping the ticket of dead waiter pid $pid"
    rm -f "$ticket"
  done
}

# How many live slots are held, and how many live tickets are ahead of ours.
held_slots() {
  local slot n=0
  for slot in "$SLOT_DIR"/slot-*; do
    [[ -d $slot ]] && n=$(( n + 1 ))
  done
  echo "$n"
}

tickets_ahead() {
  local ticket mine n=0
  mine=$(basename "$TICKET")
  for ticket in "$QUEUE_DIR"/*; do
    [[ -f $ticket ]] || continue
    [[ $(basename "$ticket") < $mine ]] && n=$(( n + 1 ))
  done
  echo "$n"
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
  mkdir -p "$SLOT_DIR" "$QUEUE_DIR"
  TICKET=$(printf '%s/%d-%012d-%010d' "$QUEUE_DIR" "$TICKET_PRIORITY" "$(date +%s)" "$$")
  # A SIGNAL ENDS THE RUN. A trap on INT or TERM replaces bash's default
  # action (terminate) and execution resumes after the handler, so with
  # release_slot trapped on them directly a waiter that was told to stop
  # dropped its ticket and went on waiting -- then took the next free slot and
  # ran the suite nobody wanted any more (the harness's stopped waiter sat out
  # its holder's whole 60s and ran). The signals exit with their conventional
  # status instead, and the release runs once, from EXIT.
  trap release_slot EXIT
  trap 'exit 130' INT
  trap 'exit 143' TERM
  : > "$TICKET"
  local i waited=0 announced=0 holder ahead
  while true; do
    reap_dead_slots
    reap_dead_tickets
    ahead=$(tickets_ahead)
    (( ahead < SUITE_SLOTS - $(held_slots) )) && for (( i = 1; i <= SUITE_SLOTS; i++ )); do
      if mkdir "$SLOT_DIR/slot-$i" 2>/dev/null; then
        rm -f "$TICKET"
        TICKET=""
        SUITE_SLOT="$SLOT_DIR/slot-$i"
        printf '%s\n' "$$" > "$SUITE_SLOT/pid"
        printf '%s\n' "$*" > "$SUITE_SLOT/cmd"
        # The marker every descendant reads. It is exported, not written to the
        # slot: what matters to a nested invocation is whether IT is inside the
        # holder, which is exactly what an inherited environment answers.
        export AGENT_REPL_SUITE_SLOT_HELD="slot-$i"
        (( waited > 0 )) && log "slot $i acquired after ${waited}s"
        return 0
      fi
    done
    if (( announced == 0 )); then
      holder=$(cat "$SLOT_DIR"/slot-*/cmd 2>/dev/null | sed -n 1p)
      log "WAITING: all $SUITE_SLOTS suite slot(s) are in use or promised to earlier waiters ($ahead ahead of this one)${holder:+ (running: $holder)}."
      log "  This is the host concurrency gate, not a hang: $SLOT_DIR holds a directory per running suite."
      log "  A suite already fills this box; running two makes both slower and makes timing bounds lie."
      log "  Set AGENT_REPL_SUITE_NO_GATE=1 to override deliberately."
      announced=1
    elif (( waited % 60 == 0 )); then
      log "still waiting for a suite slot (${waited}s, $ahead ahead of this one)"
    fi
    sleep 2
    waited=$(( waited + 2 ))
  done
}

(( $# > 0 )) || { log "usage: suite-slot.sh <command> [args...]"; exit 2; }
if (( MERGE_GATE )); then
  log "a merge-queue gate takes no host suite slot; the merge queue's governor admits it: $*"
  "$@"
  exit $?
fi
acquire_slot "$*"
"$@"
# The command's exit status is this script's: a gate must never turn a red
# suite green (or the reverse) on its way past.
exit $?
