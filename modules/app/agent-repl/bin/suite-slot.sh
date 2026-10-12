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

# HOW MANY SUITES THAT ARE NOT THE MERGE QUEUE'S MAY RUN AT ONCE.
#
# One, by default, and the default is the whole point. A suite is sized to use
# the machine; two of them do not go twice as fast, they go slower and one of
# them reports a bound as missed that a quiet box meets. Raise it only for
# suites you have measured as small, and then say so at the call site.
#
# The merge queue's gates (AGENT_REPL_SUITE_PRIORITY head or speculative) take
# no slot: the daemon already bounds them to its window, and the host's ONE
# TEST BUDGET below is what every run, theirs or not, shares.
SUITE_SLOTS=${AGENT_REPL_SUITE_SLOTS:-1}

# THE HOST'S ONE TEST BUDGET (owner ruling, 2026-10-11). Concurrent suites
# share it rather than each sizing itself to the machine: a run claims
# PROCESSOR tokens (core-N) and MEMORY tokens (mem-N, one GiB each) with the
# same atomic `mkdir` the slots use, and is handed the processors it got as
# AGENT_REPL_TEST_CPU_BUDGET, which the runner sizes its own parallelism to.
#
#   processors  one token per logical processor. The first half are the
#               HEAD'S: only a run of the merge queue's head
#               (AGENT_REPL_SUITE_PRIORITY=head) may claim them, so nothing
#               else can starve it. A head run wants half the host; every
#               other run wants a quarter, from the second half only, and
#               takes what the runs before it left (at least one).
#   memory      half the host's memory, in GiB, for the runs' RAM disks: a run
#               claims its whole RAM disk (AGENT_REPL_TEST_RAMDISK_MIB, default
#               8 GiB) or nothing and waits, and the head's one disk's worth
#               is reserved to it. Three runs can never exhaust the host.
#
# A run that cannot get its memory, or a single processor, gives back what it
# claimed this round and waits: nothing is held while waiting for the rest.
detect_cpus() {
  if [[ -n ${AGENT_REPL_SUITE_HOST_CPUS:-} ]]; then printf '%s\n' "$AGENT_REPL_SUITE_HOST_CPUS"; return; fi
  sysctl -n hw.logicalcpu 2>/dev/null || getconf _NPROCESSORS_ONLN
}
detect_mem_gib() {
  if [[ -n ${AGENT_REPL_SUITE_HOST_MEM_GIB:-} ]]; then printf '%s\n' "$AGENT_REPL_SUITE_HOST_MEM_GIB"; return; fi
  local bytes
  if bytes=$(sysctl -n hw.memsize 2>/dev/null); then
    printf '%s\n' $(( bytes / 1073741824 ))
  else
    awk '/^MemTotal:/ { print int($2 / 1048576) }' /proc/meminfo
  fi
}

PRIORITY=${AGENT_REPL_SUITE_PRIORITY:-other}
case "$PRIORITY" in
  head|speculative|other) ;;
  *) log "AGENT_REPL_SUITE_PRIORITY=$PRIORITY is not head or speculative"; exit 2 ;;
esac

SUITE_SLOT=""
CLAIMED=()
# shellcheck disable=SC2329  # invoked indirectly, from the EXIT/INT/TERM trap
release_slot() {
  local token
  for token in "${CLAIMED[@]+"${CLAIMED[@]}"}"; do
    rm -rf "$token"
  done
  CLAIMED=()
  [[ -n ${SUITE_SLOT:-} ]] || return 0
  rm -rf "$SUITE_SLOT"
  SUITE_SLOT=""
}

# claim_tokens KIND FROM TO WANT claims up to WANT of the tokens KIND-FROM ..
# KIND-TO, recording each in CLAIMED, and leaves how many it got in CLAIM_GOT.
claim_tokens() {
  local kind=$1 from=$2 to=$3 want=$4 got=0 i
  for (( i = from; i <= to && got < want; i++ )); do
    if mkdir "$SLOT_DIR/$kind-$i" 2>/dev/null; then
      printf '%s\n' "$$" > "$SLOT_DIR/$kind-$i/pid"
      CLAIMED+=("$SLOT_DIR/$kind-$i")
      got=$(( got + 1 ))
    fi
  done
  CLAIM_GOT=$got
}

# claim_budget claims this run's memory and processors, or nothing.
claim_budget() {
  local cpus mem_gib need mem_pool cpu_reserve want got_mem got_cpu
  cpus=$(detect_cpus)
  mem_gib=$(detect_mem_gib)
  need=$(( (${AGENT_REPL_TEST_RAMDISK_MIB:-8192} + 1023) / 1024 ))
  mem_pool=$(( mem_gib / 2 ))
  cpu_reserve=$(( cpus / 2 ))
  if [[ $PRIORITY == head ]]; then
    claim_tokens mem 1 "$mem_pool" "$need"
  else
    claim_tokens mem $(( need + 1 )) "$mem_pool" "$need"
  fi
  got_mem=$CLAIM_GOT
  if (( got_mem < need )); then
    BUDGET_SHORT="memory ($got_mem of $need GiB free for a $PRIORITY run)"
    return 1
  fi
  if [[ $PRIORITY == head ]]; then
    want=${AGENT_REPL_SUITE_CPUS:-$cpu_reserve}
    claim_tokens core 1 "$cpus" "$want"
  else
    want=${AGENT_REPL_SUITE_CPUS:-$(( cpus / 4 > 0 ? cpus / 4 : 1 ))}
    claim_tokens core $(( cpu_reserve + 1 )) "$cpus" "$want"
  fi
  got_cpu=$CLAIM_GOT
  if (( got_cpu == 0 )); then
    BUDGET_SHORT="processors (none free for a $PRIORITY run)"
    return 1
  fi
  export AGENT_REPL_TEST_CPU_BUDGET=$got_cpu
  log "claimed $got_cpu processor(s) and $need GiB of the host's test budget as a $PRIORITY run"
  return 0
}

reap_dead_slots() {
  local slot pid
  for slot in "$SLOT_DIR"/slot-* "$SLOT_DIR"/core-* "$SLOT_DIR"/mem-*; do
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
  local i slots waited=0 announced=0 holder
  while true; do
    reap_dead_slots
    BUDGET_SHORT=""
    # A SIGNAL STILL ENDS THE RUN: the handler gives back what is claimed and
    # exits, as the default action would have.
    trap release_slot EXIT
    trap 'release_slot; exit 130' INT
    trap 'release_slot; exit 143' TERM
    if [[ $PRIORITY != other ]]; then
      # A MERGE QUEUE GATE takes its budget and no slot.
      if claim_budget; then
        export AGENT_REPL_SUITE_SLOT_HELD="budget-$$"
        (( waited > 0 )) && log "the $PRIORITY run's budget was claimed after ${waited}s"
        return 0
      fi
      release_slot
    fi
    slots=$SUITE_SLOTS
    [[ $PRIORITY == other ]] || slots=0
    for (( i = 1; i <= slots; i++ )); do
      if mkdir "$SLOT_DIR/slot-$i" 2>/dev/null; then
        SUITE_SLOT="$SLOT_DIR/slot-$i"
        printf '%s\n' "$$" > "$SUITE_SLOT/pid"
        printf '%s\n' "$*" > "$SUITE_SLOT/cmd"
        if ! claim_budget; then
          release_slot
          break
        fi
        # The marker every descendant reads. It is exported, not written to the
        # slot: what matters to a nested invocation is whether IT is inside the
        # holder, which is exactly what an inherited environment answers.
        export AGENT_REPL_SUITE_SLOT_HELD="slot-$i"
        (( waited > 0 )) && log "slot $i acquired after ${waited}s"
        return 0
      fi
    done
    if [[ -n $BUDGET_SHORT ]] && (( announced == 0 )); then
      log "WAITING: the host's test budget has no $BUDGET_SHORT for this run."
      log "  This is the host's one test budget, not a hang: $SLOT_DIR holds a directory per claimed processor and GiB."
      announced=1
    elif (( announced == 0 )); then
      holder=$(cat "$SLOT_DIR"/slot-*/cmd 2>/dev/null | sed -n 1p)
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
