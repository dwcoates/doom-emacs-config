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

### 2. `agentrepl.v1.UpdateMergeQueueMediation` gains `dispatch`

- WHAT: a third decision arm, `dispatch` (tag 4): `repeated DeployProblem
  problems` and a `remediation` oneof of `restore` or `fresh`.
  `DeployProblem` holds a `statement` and verbatim `evidence` records. Two
  refusals: `no_remediation_to_restore` (17) and `remediation_bound_reached`
  (18, carrying the bound). `not_mediator` and `no_mediation_owed` keep
  their tags and now mean "not the chain's intake" and "no check owed".
- WHY: the owner ruled the intake is a weaker model that only states
  problems and never prescribes fixes, so the dispatch carries statements
  and evidence, no cause and no plan. Restore versus fresh is the intake's
  call by the owner's ruling (restore when the problem is in or caused by
  the last fix). The evidence is verbatim so the remediation workspace
  starts from the records, not a paraphrase.
- The message names keep "Mediation": the intake mediates between a deploy
  and the queue, and renaming the verb across the daemon and the CLI buys
  nothing a reader needs.
- `DeployProblem.evidence` is a list of strings on purpose: each is a log
  line, a doctor line or a metrics row quoted for an agent to read, and its
  sources have no common typed shape. It is text for a model, never parsed.
- The bound is enforced by the refusal, not only by the brief, so a
  sonnet-level intake that miscounts cannot exceed it.
- CONSEQUENCES: the CLI verb `agentrepl merge-queue mediation` gains a
  `dispatch` form; `daemon/internal/merge/mediation.go` (`DecideMediation`)
  gains the dispatch path and both refusals.

### 3. `agentrepl.v1.MergeQueueStatusAdvanceMediating` says the stage

- WHAT: `mediator` (1) and `name` (2) are retired. A required `stage` oneof
  of `rolling_out` (optional intake), `assessing` (intake) and `fixing`
  (remediation, intake), plus `remediations_in_a_row`.
- WHY: there is no one mediator any more; whoever reads the status (an
  agent, `merge-queue status`) needs which stage the check is in and which
  workspace is at work, which is also what the sidebar label draws.
- The `mediating` arm's comment now says merges keep their work in flight:
  the owner ruled that only the head waits while the intake assesses, and
  new speculative work stops only once a fix is dispatched
  (`daemon/internal/merge/standing.go:21`, `queueHeld`, is split to do so).
- `remediations_in_a_row` sits on the encompassing message, not an arm,
  because it holds in every stage of a chain.
- CONSEQUENCES: the CLI's status printer and its tests read the stage;
  `daemon/internal/merge` resolves the stage from the chain's store record.

### 4. `frontend.v1.RosterMergeQueueMediation` gains `fixing` and click targets

- WHAT: a third standing, `fixing` (tag 4). Every standing carries a
  `RosterMergeQueueMediationTarget` (the workspace a click selects):
  optional on `waiting` (no intake exists during a chain's first rollout),
  required on `checking` (the intake) and `fixing` (the remediation).
- WHY: the owner asked for the purple label to select the workspace at work,
  and for a third label while a fix is in progress. The labels' words are
  the owner's: `hot reloading`, `assessing reload`, `fixing reload`.
- Layout (figma→idl): the label is one element of the repository group's
  header, between the name and the count. Its arm is its state and its
  target is its one prop, so one message per arm and one shared target
  message for the one element; the target is not shared with any other
  view.
- Selecting the intake shows a hidden workspace: the selection reveals it
  for that visit, as a click on any hidden workspace's link does.
- CONSEQUENCES: `webapp/src/sidebar/merge-queue-view.ts`
  (`drawQueueMediationLabel`) draws `fixing reload` and makes the label a
  click target; `lisp/wire-roster.el` decodes the new arm and the targets;
  `daemon/internal/resolve/sidebar/sections.go` resolves them from
  `sidebar.QueueMediation`, which gains the stage and the workspaces.

### 5. `agentrepl.v1.FinishWorkspaceSuccess` says how the work lands

- WHAT: `FinishWorkspaceSuccess` gains a required `landing` oneof:
  `queued` (every finish today) and `landing_directly` (a deploy
  remediation workspace). The request's `placement` is documented as not
  read for a remediation workspace.
- WHY: the owner ruled a remediation workspace finishes like a one-shot, but
  its finish runs the full gate (resuming it on a failure to fix and finish
  again), lands directly on the main branch and deploys, never through the
  queue. The agent finishing needs to know which path its finish took,
  because a gate failure comes back to it as a resumed turn rather than as a
  queue notice.
- The request keeps one shape for both paths: the finishing agent does not
  choose its path, its recorded role does, so a second request shape would
  let an agent ask for a path it does not have. The cost accepted is a
  `placement` that is read on one path only, stated at the field.
- CONSEQUENCES: every producer of `FinishWorkspaceSuccess` sets `queued`
  (`daemon/internal/server` finish handler and its tests); the CLI prints
  which path; the remediation finish path in `daemon/internal/merge` records
  each step durably (gate, land, deploy, owed check) so a handover resumes
  it.

## Sweep

- `repoconfig.v1.DeployMediator` was removed with its field. No other
  declaration lost its last reference: `UpdateMergeQueueNotMediator` and
  `UpdateMergeQueueNoMediationOwed` keep their arms with the intake's
  meaning.

## Consequences recap

- The doom lifecycle config moves to `deploy_checks`, and a config naming
  `deploy_mediator` fails to parse.
- The intake decides with approve, escalate or dispatch; the bound is
  enforced by a refusal.
- `merge-queue status` reports rolling out, assessing or fixing, with the
  workspaces and the count in a row.
- The sidebar label gains `fixing reload` and selects the workspace at work.
- Every finish answers `queued` or `landing_directly`; a remediation's
  `placement` is not read.
