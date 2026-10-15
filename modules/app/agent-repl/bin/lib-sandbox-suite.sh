#!/usr/bin/env bash
#
# lib-sandbox-suite.sh — the ONE way a gate suite runs Go tests inside the
# e2e sandbox. Sourced, never run, by bin/test-e2e-emacs.sh, which runs the
# Emacs client layer (TestEmacs*) and the Doom tier (TestDoom*) in one
# container. A further sandboxed suite sources it too rather than carrying a
# copy of the preflight, build and decline that drifts.
#
# THE CONTAINER IS A PRECONDITION, NOT A FAILURE. A sandboxed suite must never
# run unsandboxed -- it starts an editor that would otherwise reach the
# developer's own ~/.claude, ~/.emacs.d and ~/.config -- so when the sandbox is
# not usable the suite DECLINES:
#
#   exit 0   the suite ran and passed
#   exit 77  the suite DECLINED: the sandbox is unavailable, and the
#            preflight's own message says why (bin/test-all.sh reports 77 as
#            a skip, never as a pass and never as a failure)
#   other    the suite ran and failed
#
# 77 is the autotools "skipped" convention, and it is used rather than 0 so
# that a gate run can never show a green suite that did not execute.

# run_sandbox_suite LABEL PATTERN WHAT [GO-TEST-ARGS...] runs the e2e module's
# tests matching PATTERN inside the sandbox, announcing itself as LABEL and
# WHAT; it execs, so it never returns.
run_sandbox_suite() {
  local label=$1 pattern=$2 what=$3
  shift 3
  local THIS_DIR MODULE_ROOT SANDBOX
  THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  MODULE_ROOT="$(cd "$THIS_DIR/.." && pwd)"
  SANDBOX="$MODULE_ROOT/e2e/sandbox/bin/e2e-sandbox.sh"
  local EXIT_DECLINED=77

  if [ ! -x "$SANDBOX" ]; then
      printf '[%s] SKIPPED: the sandbox runner is missing or not executable: %s\n' "$label" "$SANDBOX" >&2
      exit "$EXIT_DECLINED"
  fi

  # The preflight's output is the actionable part (start Docker, build the
  # image), so it is reproduced VERBATIM rather than paraphrased -- the same
  # rule the harness itself follows for its Go-side skip.
  #
  # THIS CHECKOUT'S IMAGE IS BUILT, NEVER DECLINED FOR. Each image is tagged by
  # the inputs it was built from (sandbox/bin/sandbox-image.sh), so a checkout
  # whose inputs are new has no image yet. That is not an unusable sandbox:
  # the image is built here (a no-op once it exists, and exclusive under the
  # build's host-wide lock), and a build that fails FAILS the suite rather
  # than skipping it, so a sandbox change can never turn a sandboxed suite
  # into a quiet skip.
  local PREFLIGHT_IMAGE_NOT_BUILT=12 preflight_status=0 preflight_out
  preflight_out="$("$SANDBOX" preflight 2>&1)" || preflight_status=$?
  if [ "$preflight_status" -eq "$PREFLIGHT_IMAGE_NOT_BUILT" ]; then
      printf '%s\n' "$preflight_out"
      printf "[%s] building this checkout's sandbox image first\n" "$label"
      "$SANDBOX" build
      preflight_status=0
      preflight_out="$("$SANDBOX" preflight 2>&1)" || preflight_status=$?
  fi
  if [ "$preflight_status" -ne 0 ]; then
      printf '[%s] SKIPPED: the e2e sandbox is not usable, and this suite never runs unsandboxed.\n' "$label" >&2
      printf '%s\n' "$preflight_out" >&2
      exit "$EXIT_DECLINED"
  fi
  printf '%s\n' "$preflight_out"

  # `--dir e2e`, AND `./` RATHER THAN `./e2e/`: the cross-system suite is its
  # OWN Go module (e2e/go.mod) and there is no go.mod at the module root.
  # UNDER testrun A SUITE HOLDS AGENT_REPL_UNIT_SLOTS CORE SLOTS, and the
  # container must use exactly that many CPUs: the sandbox caps itself to
  # AGENT_REPL_SANDBOX_CPUS. A direct run keeps the sandbox's own default.
  if [ -n "${AGENT_REPL_UNIT_SLOTS:-}" ]; then
      export AGENT_REPL_SANDBOX_CPUS="$AGENT_REPL_UNIT_SLOTS"
  fi
  printf '[%s] running %s inside the sandbox\n' "$label" "$what"
  exec "$SANDBOX" run --dir e2e go test ./ -run "$pattern" -v "$@"
}
