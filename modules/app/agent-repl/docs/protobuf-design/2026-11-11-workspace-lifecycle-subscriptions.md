# Workspace subscriptions tell a workspace's outcomes; the queue subscription narrows

An agent waiting for a specific workspace's merge ("after X lands") had only the
queue-wide `merge-queue subscribe`, which told every merge put in line, every
merge leaving and every deploy, so a busy queue woke it dozens of times for
nothing it waited on. The per-workspace `workspace subscribe` already existed
but could not say that a landing's deploy had finished, that a merge had been
evicted, that a session had parked at its cold gate, or how a workspace ended.

Design mode: autonomous; the owner approved the contract changes in the
conversation that produced this record.

## Landed changes

### 1. `agentrepl.v1.WorkspaceNoticeEvent` gains deployed, evicted, cold_gated and the lifecycle bundle (2026-11-11)

- WHAT: `WORKSPACE_NOTICE_EVENT_DEPLOYED = 7`, `EVICTED = 8`, `COLD_GATED = 9`,
  and the request-only bundle `LIFECYCLE = 10`. `SubscribeWorkspaceRequest.events`
  may be empty, which holds the bundle. The bundle expands to landed, closed,
  deployed, evicted and cold_gated. The notice for `closed` says closed, killed
  or nuked, in its text.
- WHY: owner ruling: lifecycle outcomes are the normal subscription tier;
  turn-by-turn events stay an explicit opt-in because each notice wakes the
  subscriber at the cost of a full read of its context. `needs_input` does not
  cover a cold gate (`agentrepl.v1.WorkspaceStatus.activity.cold_gate` is its
  own arm), so `cold_gated` is a new event.
- ASSUMPTION (recorded): `LIFECYCLE` is an enum arm rather than a client-side
  expansion, so every client gets the same bundle and the daemon owns it.
- CONSEQUENCE: a workspace subscribed to `deployed` is told only for a landing
  whose deploy the daemon runs (agent-repl's own master), because the payment
  of owed landing reports (`mergeworkspace.Service.PayOwed`) is where a deploy
  ends. A landing into a parent workspace has no deploy.
- CONSEQUENCE: a nuke forgets the workspace and its subscriptions right after
  its close edge, so a `nuked` notice depends on the notifier reaching the edge
  first (recorded in `org/todo.md`).

### 2. `agentrepl.v1.SubscribeMergeQueue` tells only depleted, paused and resumed (2026-11-11)

- WHAT: comments only in `endpoint_subscribe_merge_queue.proto`: no message,
  field or arm changed. A subscription is told the queue depleting, being
  paused, and being resumed, in one short line naming the queue. It is no
  longer told a merge put in line, a merge leaving while others wait, or a
  deploy. It still ends on unsubscribe, on its workspace's close, and on its
  own at depletion.
- WHY: the same ruling: following one merge is the per-workspace
  subscription's job, and the queue-wide feed had no use beyond depletion and
  the pause state.
- REMOVED: `merge.QueueChangeDeployed` and `merge.Orchestrator.ReportDeploy`,
  which only the retired deploy notice read.
