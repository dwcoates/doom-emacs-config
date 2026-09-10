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
set -euo pipefail

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
IMAGE=${AGENT_REPL_SANDBOX_IMAGE:-agent-repl-e2e-sandbox:latest}
# The image carries Emacs, Doom's package set, two node_modules trees and a
# primed Go module cache; the working copy and build outputs add to that.
MIN_FREE_GB=${AGENT_REPL_SANDBOX_MIN_FREE_GB:-12}
BUILD_HINT="modules/app/agent-repl/e2e/sandbox/bin/e2e-sandbox.sh build"

say() { printf '%s\n' "$*"; }

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

if ! "$runtime" info >/dev/null 2>&1; then
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

if ! "$runtime" image inspect "$IMAGE" >/dev/null 2>&1; then
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
