#!/usr/bin/env bash
# async-probe-gaps.sh — render the continuity verdict from an async-probe log.
#
# WHY THIS EXISTS: "the shim survived" and "the workspace recovered" are both
# indirect. The direct question is whether the WORK ITSELF ever stopped, and the
# only honest answer is the probe's own heartbeat: a tick every INTERVAL seconds
# with no gap wider than the interval plus a small scheduling tolerance, across
# a window that spans the bounce.
#
# A gap is the ONLY failure signal that cannot be faked by a recovered UI. A
# process that was SIGKILLed and respawned shows a gap; one that was merely
# reparented or reconnected does not.
#
# USAGE: async-probe-gaps.sh <log> [tolerance-seconds] [--expect-stop]
#   log           the heartbeat file written by async-probe.sh
#   tolerance     max acceptable gap, in seconds (default 2.0 for a 1s interval)
#   --expect-stop the tester deliberately SIGTERMed the probe; a stop is then a
#                 pass rather than a failure
#
# A STOP IS A FAILURE UNLESS THE TESTER ASKED FOR IT. This script previously
# reported PASS for a probe that had been terminated by something other than the
# test: a clean SIGTERM leaves no gap and no second `started` line, so both of
# the other checks are satisfied by work that is no longer running. It happened
# for real — an editor restart took the shim down, the shim took its background
# task with it, and the analyzer called it continuity. The probe cannot tell the
# tester's SIGTERM from anyone else's, so the EXPECTATION has to come from the
# caller rather than from the log.
#
# EXIT: 0 when every gap is within tolerance, the probe never respawned, and it
#       is either still running or was stopped with --expect-stop; 1 otherwise.
#       The exit code is the verdict.
set -u

log="${1:?usage: async-probe-gaps.sh <log> [tolerance-seconds] [--expect-stop]}"
tol="2.0"
expect_stop=0
shift
for arg in "$@"; do
  case "$arg" in
    --expect-stop) expect_stop=1 ;;
    *) tol="$arg" ;;
  esac
done

[ -r "$log" ] || { echo "FAIL: probe log unreadable: $log"; exit 1; }

awk -v tol="$tol" -v logfile="$log" -v expect_stop="$expect_stop" '
  # HH:MM:SS.fraction -> seconds since midnight, as a float.
  function secs(ts,   p, hms, frac) {
    split(ts, p, ".")
    split(p[1], hms, ":")
    frac = (length(p) > 1) ? ("0." p[2]) + 0 : 0
    return hms[1]*3600 + hms[2]*60 + hms[3] + frac
  }
  $3 == "tick" {
    t = secs($1)
    # Midnight wrap: a negative delta means the clock rolled, not a gap.
    if (have && t < prev) t += 86400
    if (have) {
      d = t - prev
      if (d > maxgap) { maxgap = d; maxat = $1 }
      if (d > tol) { breaches++; if (breaches <= 10) blines[breaches] = sprintf("    %.3fs gap ending at %s", d, $1) }
      total += d
    }
    prev = t; have = 1; ticks++
    if (ticks == 1) first = $1
    last = $1
  }
  $3 == "stopped" { stopped = 1; stopline = $0 }
  $3 == "started" { starts++ }
  END {
    printf "probe log:   %s\n", logfile
    printf "ticks:       %d  (first %s  last %s)\n", ticks, first, last
    printf "starts:      %d\n", starts
    if (ticks < 2) {
      printf "VERDICT:     FAIL — need at least 2 ticks to measure a gap\n"
      exit 1
    }
    printf "max gap:     %.3fs (ending %s)   tolerance %.3fs\n", maxgap, maxat, tol
    printf "mean gap:    %.3fs\n", total / (ticks - 1)
    if (stopped) printf "STOPPED:     %s\n", stopline
    if (breaches > 0) {
      printf "breaches:    %d\n", breaches
      for (i = 1; i <= breaches && i <= 10; i++) print blines[i]
      printf "VERDICT:     FAIL — the work stopped for longer than tolerance\n"
      exit 1
    }
    if (starts > 1) {
      printf "VERDICT:     FAIL — probe restarted (%d starts); a respawn is not continuity\n", starts
      exit 1
    }
    if (stopped && expect_stop != 1) {
      printf "VERDICT:     FAIL — the probe STOPPED and the test did not ask it to; work that ended is not work that continued\n"
      exit 1
    }
    printf "VERDICT:     PASS — no gap exceeded tolerance; the work never stopped\n"
    exit 0
  }
' "$log"
