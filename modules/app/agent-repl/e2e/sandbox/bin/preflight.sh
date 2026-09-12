#!/usr/bin/env bash
# Preflight for the agent-repl e2e sandbox.
#
# Reports, on stdout, exactly what is missing and what to do about it, and
# exits non-zero when the sandbox cannot run. The harness calls this and
# turns a non-zero exit into a LOUD SKIP that quotes this output verbatim —
# never into a silent pass and never into a fallback to an unsandboxed run.
#
# Exit codes:
#   0  ready
#   10 no container runtime (no docker, no podman)
#   11 a runtime binary exists but its daemon is not reachable
#   12 the image is not built
#   13 not enough free disk
#   14 the runtime did not answer within the bound; its engine is wedged
set -euo pipefail

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
IMAGE=${AGENT_REPL_SANDBOX_IMAGE:-agent-repl-e2e-sandbox:latest}
# The image carries Emacs, Doom's package set, two node_modules trees and a
# primed Go module cache; the working copy and build outputs add to that.
MIN_FREE_GB=${AGENT_REPL_SANDBOX_MIN_FREE_GB:-12}
BUILD_HINT="modules/app/agent-repl/e2e/sandbox/bin/e2e-sandbox.sh build"

say() { printf '%s\n' "$*"; }

# EVERY CALL INTO THE RUNTIME IS BOUNDED. A preflight either answers or is
# reported as not answering, within a bound; it never blocks a test process.
# Observed 2026-09-12: Docker Desktop's backend was alive while its socket
# never answered, so `docker info` blocked forever, and the Go e2e package --
# which runs this script once inside a sync.Once -- died on its own 10m
# timeout with every TestEmacs* queued behind the one wedged probe.
#
# The bound is per runtime call. A healthy `docker info` is sub-second on an
# idle box and a few seconds under load (this script's own callers describe
# preflight as "tens of seconds under load", which covers both calls plus
# image inspection); 3x the ~3s loaded case is ~10s.
RUNTIME_CALL_TIMEOUT_SECONDS=${AGENT_REPL_SANDBOX_RUNTIME_TIMEOUT_SECONDS:-10}

# run_bounded <seconds> <argv...>: run argv, discarding its output, and return
# 124 if it has not finished within the bound. Uses `timeout`/`gtimeout` when
# present; otherwise implements the bound in bash, because an unbounded call
# is the defect this exists to prevent.
run_bounded() {
  local seconds=$1; shift
  local timeout_bin=''
  if command -v timeout >/dev/null 2>&1; then
    timeout_bin=timeout
  elif command -v gtimeout >/dev/null 2>&1; then
    timeout_bin=gtimeout
  fi
  if [[ -n $timeout_bin ]]; then
    "$timeout_bin" "$seconds" "$@" >/dev/null 2>&1
    return $?
  fi

  "$@" >/dev/null 2>&1 &
  local child=$!
  local waited=0
  while (( waited < seconds * 10 )); do
    if ! kill -0 "$child" 2>/dev/null; then
      local status=0
      wait "$child" 2>/dev/null || status=$?
      return $status
    fi
    sleep 0.1
    waited=$(( waited + 1 ))
  done
  kill -TERM "$child" 2>/dev/null || true
  wait "$child" 2>/dev/null || true
  return 124
}

# say_wedged <the call that hung> -- the verbatim, actionable message.
say_wedged() {
  say "agent-repl e2e sandbox UNAVAILABLE: '$runtime' did not answer within ${RUNTIME_CALL_TIMEOUT_SECONDS}s."
  say "  '$1' was still running when the bound expired; the engine is likely wedged"
  say "  (Docker Desktop backend alive but the socket unresponsive)."
  # Compared by BASENAME: AGENT_REPL_SANDBOX_RUNTIME may name an absolute
  # path, and the remedy still depends on which engine it is.
  if [[ $(basename -- "$runtime") == docker ]]; then
    say "  Restart Docker Desktop, then re-run."
  else
    say "  Run 'podman machine stop && podman machine start', then re-run."
  fi
}

runtime=${AGENT_REPL_SANDBOX_RUNTIME:-}
if [[ -z $runtime ]]; then
  for candidate in docker podman; do
    if command -v "$candidate" >/dev/null 2>&1; then runtime=$candidate; break; fi
  done
fi

if [[ -z $runtime ]]; then
  say "agent-repl e2e sandbox UNAVAILABLE: no container runtime found."
  say "  Neither 'docker' nor 'podman' is on PATH."
  say "  Install one, then build the image:  $BUILD_HINT"
  say "  Set AGENT_REPL_SANDBOX_RUNTIME to name a runtime explicitly."
  exit 10
fi

info_status=0
run_bounded "$RUNTIME_CALL_TIMEOUT_SECONDS" "$runtime" info || info_status=$?
if (( info_status == 124 )); then
  say_wedged "$runtime info"
  exit 14
fi
if (( info_status != 0 )); then
  say "agent-repl e2e sandbox UNAVAILABLE: '$runtime' is installed but not usable."
  say "  '$runtime info' failed — the daemon or machine is not running."
  if [[ $runtime == docker ]]; then
    say "  Start Docker Desktop (or dockerd), then re-run."
  else
    say "  Run 'podman machine start', then re-run."
  fi
  say "  Then build the image:  $BUILD_HINT"
  exit 11
fi

inspect_status=0
run_bounded "$RUNTIME_CALL_TIMEOUT_SECONDS" "$runtime" image inspect "$IMAGE" || inspect_status=$?
if (( inspect_status == 124 )); then
  say_wedged "$runtime image inspect $IMAGE"
  exit 14
fi
if (( inspect_status != 0 )); then
  say "agent-repl e2e sandbox UNAVAILABLE: image '$IMAGE' is not built."
  say "  Runtime '$runtime' is usable, but the image does not exist locally."
  say "  Build it:  $BUILD_HINT"
  exit 12
fi

# Free space on the runtime's own storage root when it will tell us, else on
# the checkout's filesystem.
free_kb=$(df -Pk "$here" | awk 'NR==2 {print $4}')
free_gb=$(( free_kb / 1024 / 1024 ))
if (( free_gb < MIN_FREE_GB )); then
  say "agent-repl e2e sandbox UNAVAILABLE: insufficient free disk."
  say "  ${free_gb}GiB free on the filesystem holding $here; the sandbox needs >= ${MIN_FREE_GB}GiB."
  say "  Free space (e.g. '$runtime system prune'), or raise AGENT_REPL_SANDBOX_MIN_FREE_GB deliberately."
  exit 13
fi

say "agent-repl e2e sandbox READY: runtime=$runtime image=$IMAGE free=${free_gb}GiB"
exit 0
