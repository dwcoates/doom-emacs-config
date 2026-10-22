#!/usr/bin/env bash
# deploy-hook-probe-fails.sh — a deploy hook that records that it ran, then
# always fails.
#
# Name it as a request's -pre-deploy-script or -post-deploy-script to check a
# deploy's failed-hook path: it runs deploy-hook-probe.sh, then exits 1, so the
# deploy fails at that hook while whatever already landed stays landed.
set -euo pipefail
"$(dirname "${BASH_SOURCE[0]}")/deploy-hook-probe.sh"
echo "deploy-hook-probe-fails: exiting 1, as it always does"
exit 1
