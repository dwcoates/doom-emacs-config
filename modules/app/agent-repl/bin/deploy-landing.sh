#!/usr/bin/env bash
# deploy-landing.sh — agent-repl's deploy hook (.agent-repl/deploy.yaml).
#
# It asks the daemon to roll out what the deploy landed
# (agentrepl.v1.RollOutLanding). The daemon owns the rollout itself: run by a
# deploy, the ask is registered and the rollout fires at the deploy's teardown,
# once its report is owed to it; run by a hold's release, with no deploy named,
# the rollout starts at once.
#
# It runs from the repository's main checkout root, which is where the front
# door below is the deployed build's.
#
# A deploy's ask names its ledger and DIALS THE DAEMON RUNNING THE DEPLOY
# (AGENT_REPL_DEPLOY_DAEMON): mid-handover the address file can name the other
# daemon, which knows no such deploy and refuses it.
set -euo pipefail

FRONT_DOOR="modules/app/agent-repl/skills/agent-repl-workspaces/agent-repld"
args=(runtime roll-out-landing)
if [ -n "${AGENT_REPL_DEPLOY_LEDGER:-}" ]; then
    args+=(-deploy "$AGENT_REPL_DEPLOY_LEDGER")
fi
if [ -n "${AGENT_REPL_DEPLOY_DAEMON:-}" ]; then
    args+=(-daemon "$AGENT_REPL_DEPLOY_DAEMON")
fi
exec "$FRONT_DOOR" "${args[@]}"
