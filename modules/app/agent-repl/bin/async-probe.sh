#!/bin/bash
# async-probe.sh — a long-lived async proxy for bounce-continuity testing.
#
# WHY THIS EXISTS: the loop must prove that a backend bounce never interrupts a
# session's ASYNC work, and that a shim carrying live async work has its bounce
# DEFERRED until that work ends. Subagents are the real async shape, but they are
# expensive and non-deterministic; a background shell is the uniform proxy the
# daemon's task machinery treats identically (local_bash folds into the shim's
# live SDK task set exactly as local_agent does).
#
# THE CONTRACT THIS PROCESS HONORS:
#   - it runs INDEFINITELY, so the deferral it provokes is unambiguous;
#   - it exits ONLY on SIGTERM (or SIGINT), so the tester decides when the async
#     work ends and can watch what the shim does immediately afterwards;
#   - it writes a heartbeat line per tick, so "is it actually running" is
#     answered by evidence rather than by a pid that might be a zombie;
#   - it records its own exit, so an unexpected death is distinguishable from
#     the deliberate SIGTERM the test sends.
#
# USAGE: async-probe.sh <label> [heartbeat-file]
#   label          names the probe in its log lines (use the workspace name)
#   heartbeat-file defaults to /tmp/async-probe-<label>.log
set -u

label="${1:?usage: async-probe.sh <label> [heartbeat-file]}"
out="${2:-/tmp/async-probe-${label}.log}"

# The exit record is written by the trap, never by the loop, so a death that did
# NOT come through a signal leaves no "stopped" line and is therefore visible as
# an absence rather than being indistinguishable from a clean stop.
on_term() {
  printf '%s %s stopped signal=%s pid=%s\n' "$(date +%H:%M:%S.%N)" "$label" "${1:-TERM}" "$$" >>"$out"
  exit 0
}
trap 'on_term TERM' TERM
trap 'on_term INT' INT

printf '%s %s started pid=%s\n' "$(date +%H:%M:%S.%N)" "$label" "$$" >>"$out"

# `wait` after a backgrounded sleep rather than a bare `sleep`: a bare sleep is
# not interruptible by a trap until it returns, which would delay the SIGTERM
# response by up to a full tick and blur the very measurement this exists for.
while true; do
  printf '%s %s tick pid=%s\n' "$(date +%H:%M:%S.%N)" "$label" "$$" >>"$out"
  sleep 2 &
  wait $!
done
