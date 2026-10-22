#!/usr/bin/env bash
# deploy-hook-probe.sh — a deploy hook that only records that it ran.
#
# Name it as a request's -pre-deploy-script or -post-deploy-script (or a stage
# hook in .agent-repl/deploy.yaml) to check a deploy's hook wiring without the
# hook doing anything: it prints where and when it ran, which lands in the
# hook's own log under ~/.claude-emacs/merge-logs/, and exits 0. Its sibling
# deploy-hook-probe-fails.sh runs it and then exits 1, for checking a deploy's
# failed-hook path.
set -euo pipefail
echo "deploy-hook-probe: ran in $(pwd) at $(date -u +%Y-%m-%dT%H:%M:%SZ)"
sleep 1
echo "deploy-hook-probe: done"
