# Deploy intake and remediation: the contract

The standing deploy mediator (one hidden workspace that checks every deploy
and fixes what it finds itself) is replaced by an intake per chain of deploys
and a visible remediation workspace per dispatched fix. The owner's plan and
rulings are in `docs/investigations/2026-11-14-deploy-intake-plan.md`; this
record holds the contract decisions that implement it. Designed autonomously.

## Assumptions (autonomous design)

- OBJECTIVE: deploys are checked before the queue moves past them, by a weak
  model that only states problems, with fixes done by a strong model in a
  workspace the user can watch, so chains converge and are measurable.
- BUILT: a per-chain intake (sonnet, low effort, hidden), a remediation
  workspace (opus, medium effort, visible) restored or created per dispatch,
  a special finish for the remediation (gate, direct landing, deploy), and
  a hold whose owner is always recorded.
- FRONTEND: yes, the purple label beside a repository's name in the
  sidebar's merge queue section; figma→idl governs it.
- SYSTEMS: the lifecycle config (repoconfig.v1, read by the daemon), the
  daemon's merge orchestrator and store, the agent CLI verbs
  (agentrepl.v1.UpdateMergeQueue, GetMergeQueueStatus, FinishWorkspace), the
  sidebar roster (frontend.v1) drawn by the webapp and decoded by Emacs.
- NON-ADDITIVE: `deploy_mediator` is retired (the doom repository's
  `.agent-repl/lifecycle.yaml` moves to `deploy_checks`);
  `MergeQueueStatusAdvanceMediating`'s mediator and name fields are retired;
  `FinishWorkspaceSuccess` gains a required outcome arm, so every producer
  sets one.
- Models are the owner's ruling and live in the daemon, not the config.

## Iteration sequence

1. Configuration: what a repository declares.
2. The intake's decision: the verb the intake decides with, and its
   refusals.
3. The queue's status: how `merge-queue status` says where a check stands.
4. The sidebar label: what the roster draws and what a click selects.
5. The remediation's finish: what `workspace finished` answers a
   remediation workspace.

Leaf packages first is not needed: each increment is one surface, and none
embeds another's new messages.

## Landed changes

### 1. `repoconfig.v1.LifecycleConfig.deploy_checks` replaces `deploy_mediator`

- WHAT: tag 11 `deploy_mediator` and `DeployMediator` are retired;
  `deploy_checks` (tag 12) carries `DeployChecks.remediations_before_escalation`.
- WHY: the mediator was one named workspace; the intake and remediation
  workspaces are made per chain by the daemon, so the config names no
  workspace. The owner ruled the bound is 4 (the doom config sets 4); it is
  per repository because a repository with slower deploys may want fewer.
- The models (intake sonnet low, remediation opus medium) are not config:
  they are the owner's ruling for how the system works, not a per-repository
  choice.
- CONSEQUENCES: `.agent-repl/lifecycle.yaml` in the doom repository moves
  to `deploy_checks: {remediations_before_escalation: 4}`; a config still
  naming `deploy_mediator` fails to parse, loudly, as an unknown field
  (`daemon/internal/lifecycleconfig/lifecycleconfig.go:154` validates the
  old field and goes with it). `daemon/internal/merge/mediation.go:155` and
  `daemon/internal/workspace/mediator.go:76` identify the mediator by the
  configured name and are rewritten to identify the chain's intake by its
  recorded role.
