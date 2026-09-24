#!/usr/bin/env bash
# e2e-repeat.sh -- run the in-container Go e2e package N times, one run per
# artifacts directory, and NEVER delete a run's evidence.
#
# WHY THIS EXISTS.
#
# The handover/replay family of e2e failures is load-sensitive: each member
# fails roughly once in nine runs, so diagnosing one means running the package
# many times and reading the logs of the ONE run that went red. A previous
# investigation lost exactly that: it reused a single artifacts directory and
# cleaned it between runs, so the run that finally failed overwrote (and then
# removed) the only copy of its own daemon, shim, store and sidecar logs.
#
# The rule this script encodes is therefore structural rather than advisory:
#
#   * every run gets its OWN artifacts directory, named for the run index, so
#     no run can overwrite another's logs;
#   * nothing here removes anything, ever -- there is no cleanup path to
#     forget to guard. A red run's directory survives the whole session, and
#     the summary names it;
#   * each run's stdout is teed into its own artifacts directory as
#     `go-test.log`, so the transcript and the structured logs of one run are
#     found together.
#
# Usage:
#   bin/e2e-repeat.sh [--runs N] [--root DIR] [--stop-on-fail]
#                     [--load-max N | --no-load-gate] [-- <go test args>]
#
# Defaults: 5 runs, root $TMPDIR/agent-repl-e2e-artifacts/<utc stamp>, and the
# package's own default arguments (-count=1 -parallel 8 -v .).
#
# Each run is taken through bin/suite-slot.sh, the host concurrency gate, so a
# repeat driver cannot overcommit the box the way concurrent agents did.
#
# EVERY RUN STARTS ON A QUIET BOX, and the driver WAITS for one rather than
# assuming it. suite-slot serializes the suites that go THROUGH it and nothing
# else: another agent's un-gated build, a container stack, an editor indexing
# in the background all load the same machine, and this package's bounds are
# sized for a box that is otherwise idle. A run started under that load reports
# a bound as missed that a quiet box meets, which is a whole run's evidence
# thrown away -- suite-slot.sh's own header records the load-253 incident this
# comes from. So the 1-minute load average is read before each run, the run
# waits for it to fall below --load-max (8 by default, half this 16-core box),
# and the value the run actually started at is recorded in its own directory
# either way.
#
# THE ONE-MINUTE AVERAGE, because it is the one that decays fast enough to say
# what is happening now; the 5- and 15-minute figures stay high for minutes
# after the previous run's own work has stopped, so gating on those would make
# every run after the first wait for nothing.

# Tests run only at background priority: re-exec once through bin/background.sh.
[[ -n ${AGENT_REPL_BACKGROUND_PRIORITY:-} ]] || exec "$(dirname "${BASH_SOURCE[0]}")/background.sh" bash "${BASH_SOURCE[0]}" "$@"

set -uo pipefail

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
module_root=$(cd -- "$here/.." && pwd)

log() { printf 'e2e-repeat: %s\n' "$*" >&2; }
die() { log "$*"; exit 2; }

runs=5
stop_on_fail=0
root=""
load_max=8
load_gate=1
test_args=(-count=1 -parallel 8 -v .)

while (( $# > 0 )); do
  case $1 in
    --runs) runs=${2:-} ; shift 2 || die "--runs needs a value" ;;
    --root) root=${2:-} ; shift 2 || die "--root needs a value" ;;
    --stop-on-fail) stop_on_fail=1 ; shift ;;
    --load-max) load_max=${2:-} ; shift 2 || die "--load-max needs a value" ;;
    --no-load-gate) load_gate=0 ; shift ;;
    --) shift ; test_args=("$@") ; break ;;
    *) die "unknown argument: $1" ;;
  esac
done

[[ $runs =~ ^[0-9]+$ ]] && (( runs > 0 )) || die "--runs must be a positive integer, got '$runs'"
[[ $load_max =~ ^[0-9]+$ ]] && (( load_max > 0 )) || die "--load-max must be a positive integer, got '$load_max'"

# one_minute_load prints this host's 1-minute load average, or nothing when it
# reports none. An unreadable load is SAID rather than assumed quiet.
one_minute_load() {
  uptime 2>/dev/null | sed -n 's/.*load averages*:[[:space:]]*\([0-9.]*\).*/\1/p'
}

# await_quiet_box blocks until the 1-minute load is below load_max and prints
# the value the run actually starts at.
await_quiet_box() {
  local waited=0 load announced=0
  while true; do
    load=$(one_minute_load)
    if [[ -z $load ]]; then
      log "WARNING: this host reports no load average, so the load gate cannot run and the box is NOT known to be quiet"
      printf 'unavailable\n'
      return 0
    fi
    if awk -v l="$load" -v m="$load_max" 'BEGIN { exit !(l < m) }'; then
      (( announced == 1 )) && log "the box is quiet again after ${waited}s (1-minute load $load)"
      printf '%s\n' "$load"
      return 0
    fi
    if (( announced == 0 )); then
      log "WAITING for a quiet box: the 1-minute load is $load and a run may not start at or above $load_max."
      log "  This package's bounds are sized for an otherwise idle machine, so a run started under load"
      log "  reports a bound as missed that a quiet box meets. Pass --no-load-gate to override deliberately."
      announced=1
    elif (( waited % 60 == 0 )); then
      log "still waiting for a quiet box (${waited}s, 1-minute load $load)"
    fi
    sleep 5
    waited=$(( waited + 5 ))
  done
}

if [[ -z $root ]]; then
  root="${TMPDIR:-/tmp}/agent-repl-e2e-artifacts/$(date -u +%Y%m%dT%H%M%SZ)"
fi
mkdir -p "$root" || die "cannot create artifacts root $root"
root=$(cd -- "$root" && pwd)

log "artifacts root: $root"
log "runs: $runs; go test args: ${test_args[*]}"

reds=()
for (( run = 1; run <= runs; run++ )); do
  run_dir="$root/run-$(printf '%03d' "$run")"
  mkdir -p "$run_dir" || die "cannot create $run_dir"
  log "run $run/$runs -> $run_dir"
  if (( load_gate == 1 )); then
    started_load=$(await_quiet_box)
  else
    started_load=$(one_minute_load)
    log "load gate DISABLED; starting at 1-minute load ${started_load:-unavailable}"
  fi
  printf '%s\n' "${started_load:-unavailable}" > "$run_dir/start-load"
  log "run $run starting at 1-minute load ${started_load:-unavailable}"
  start=$(date -u +%s)
  AGENT_REPL_E2E_ARTIFACTS="$run_dir" \
    "$module_root/bin/suite-slot.sh" \
    "$module_root/e2e/sandbox/bin/e2e-sandbox.sh" run --dir e2e \
    go test "${test_args[@]}" 2>&1 | tee "$run_dir/go-test.log"
  status=${PIPESTATUS[0]}
  elapsed=$(( $(date -u +%s) - start ))
  printf '%s\n' "$status" > "$run_dir/exit-status"
  if (( status == 0 )); then
    log "run $run: PASS in ${elapsed}s"
  else
    reds+=("$run_dir")
    log "run $run: FAIL (exit $status) in ${elapsed}s -- evidence kept at $run_dir"
    grep -E '^\s*--- FAIL' "$run_dir/go-test.log" | sed 's/^/e2e-repeat:   /' >&2
    (( stop_on_fail == 1 )) && break
  fi
done

log "----"
if (( ${#reds[@]} == 0 )); then
  log "all runs passed; artifacts under $root"
  exit 0
fi
log "${#reds[@]} red run(s):"
for d in "${reds[@]}"; do log "  $d"; done
exit 1
